defmodule Dagger.Mod do
  @moduledoc false

  @doc """
  Invoke a function.
  """
  def invoke(module) when is_atom(module) do
    case Dagger.Global.start_link() do
      {:ok, _} -> invoke(Dagger.Global.dag(), module)
      otherwise -> otherwise
    end
  end

  def invoke(dag, module) do
    fn_call = Dagger.Client.current_function_call(dag)

    with {:ok, parent_name} <- Dagger.FunctionCall.parent_name(fn_call),
         {:ok, fn_name} <- Dagger.FunctionCall.name(fn_call),
         {:ok, parent_json} <- Dagger.FunctionCall.parent(fn_call),
         {:ok, parent} <- Jason.decode(parent_json),
         {:ok, input_args} <- Dagger.FunctionCall.input_args(fn_call),
         {:ok, json} <- invoke(dag, module, parent, parent_name, fn_name, input_args) do
      Dagger.FunctionCall.return_value(fn_call, json)
    else
      {:error, error} ->
        IO.puts(:stderr, format_error(error))
        exit({:shutdown, 2})
    end
  after
    Dagger.Global.close()
  end

  def invoke(dag, module, _parent, "", _fn_name, _input_args) do
    dag
    |> Dagger.Mod.Module.define(module)
    |> Dagger.Encode.encode(Dagger.Module)
  end

  def invoke(dag, module, _parent, _parent_name, fn_name, input_args) do
    fun = fn_name |> Macro.underscore() |> String.to_existing_atom()
    fun_def = module.__object__(:function, fun)
    args = decode_args(dag, input_args, Keyword.fetch!(fun_def, :args))
    return_type = Keyword.fetch!(fun_def, :return)

    case apply(module, fun, args) do
      {:error, _} = error -> error
      {:ok, result} -> Dagger.Encode.encode(result, return_type)
      result -> Dagger.Encode.encode(result, return_type)
    end
  end

  def decode_args(dag, input_args, args_def) do
    args =
      Enum.into(input_args, %{}, fn arg ->
        {:ok, name} = Dagger.FunctionCallArgValue.name(arg)
        {:ok, value} = Dagger.FunctionCallArgValue.value(arg)
        name = String.to_existing_atom(name)
        {:ok, value} = Dagger.Decode.decode(value, get_in(args_def, [name, :type]), dag)
        {name, value}
      end)

    for {name, _} <- args_def do
      Map.get(args, name)
    end
  end

  defp format_error(%{__exception__: true} = exception), do: Exception.message(exception)
  defp format_error(error) when is_binary(error) or is_atom(error), do: error
  defp format_error(error), do: inspect(error)
end
