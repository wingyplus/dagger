defmodule Dagger.Mod do
  @moduledoc false

  @doc """
  Invoke a function.
  """
  def invoke() do
    with {:ok, _} <- Dagger.Global.start_link(),
         dag = Dagger.Global.dag(),
         fn_call = Dagger.Client.current_function_call(dag),
         {:ok, parent_name} <- Dagger.FunctionCall.parent_name(fn_call),
         {:ok, fn_name} <- Dagger.FunctionCall.name(fn_call),
         {:ok, parent_json} <- Dagger.FunctionCall.parent(fn_call),
         {:ok, parent} <- Jason.decode(parent_json),
         {:ok, input_args} <- Dagger.FunctionCall.input_args(fn_call),
         {:ok, json} <- invoke(dag, parent, parent_name, fn_name, input_args),
         :ok <- Dagger.FunctionCall.return_value(fn_call, json) do
      Dagger.Global.close()
    else
      {:error, reason} ->
        IO.puts(inspect(reason))
        System.halt(2)
    end
  end

  def invoke(dag, _parent, "", _fn_name, _input_args) do
    # TODO: Support multiple modules when root module return another module.
    [module] = Dagger.Mod.Registry.all()

    dag
    |> Dagger.Mod.Module.define(module)
    |> encode(Dagger.Module)
  end

  def invoke(dag, _parent, parent_name, fn_name, input_args) do
    case Dagger.Mod.Registry.get(parent_name) do
      nil ->
        {:error,
         "unknown module #{parent_name}, please make sure the module is created and register to supervision tree in the application."}

      module ->
        fun = fn_name |> Macro.underscore() |> String.to_existing_atom()
        fun_def = Dagger.Mod.Module.get_function_definition(module, fun)
        args = decode_args(dag, input_args, Keyword.fetch!(fun_def, :args))
        return_type = Keyword.fetch!(fun_def, :return)

        case apply(module, fun, args) do
          {:error, _} = error -> error
          {:ok, result} -> encode(result, return_type)
          result -> encode(result, return_type)
        end
    end
  end

  def decode_args(dag, input_args, args_def) do
    args =
      Enum.into(input_args, %{}, fn arg ->
        with {:ok, name} <- Dagger.FunctionCallArgValue.name(arg),
             name = String.to_existing_atom(name),
             {:ok, value} <- Dagger.FunctionCallArgValue.value(arg),
             {:ok, value} <- decode(value, get_in(args_def, [name, :type]), dag) do
          {name, value}
        end
      end)

    for {name, _} <- args_def do
      Map.fetch!(args, name)
    end
  end

  def decode(value, type, dag) do
    with {:ok, value} <- Jason.decode(value) do
      cast(value, type, dag)
    end
  end

  defp cast(value, :integer, _) when is_integer(value) do
    {:ok, value}
  end

  defp cast(value, :boolean, _) when is_boolean(value) do
    {:ok, value}
  end

  defp cast(value, :string, _) when is_binary(value) do
    {:ok, value}
  end

  defp cast(values, {:list, type}, dag) when is_list(values) do
    values =
      for value <- values do
        {:ok, value} = cast(value, type, dag)
        value
      end

    {:ok, values}
  end

  defp cast(value, module, dag) when is_binary(value) and is_atom(module) do
    # NOTE: It feels like we really need a protocol for the module to 
    # load the data from id.
    ["Dagger", name] = Module.split(module)
    name = Macro.underscore(name)
    fun = String.to_existing_atom("load_#{name}_from_id")
    {:ok, apply(Dagger.Client, fun, [dag, value])}
  end

  defp cast(value, type, _) do
    {:error, "cannot cast value #{value} to type #{type}"}
  end

  def encode(result, type) do
    with {:ok, value} <- dump(result, type) do
      Jason.encode(value)
    end
  end

  defp dump(value, :integer) when is_integer(value) do
    {:ok, value}
  end

  defp dump(value, :boolean) when is_boolean(value) do
    {:ok, value}
  end

  defp dump(value, :string) when is_binary(value) do
    {:ok, value}
  end

  defp dump(values, {:list, type}) when is_list(values) do
    values =
      for value <- values do
        {:ok, value} = dump(value, type)
        value
      end

    {:ok, values}
  end

  defp dump(%module{} = struct, module) do
    value =
      if function_exported?(module, :id, 1) do
        Dagger.ID.id!(struct)
      else
        struct
      end

    {:ok, value}
  end

  defp dump(value, type) do
    {:error, "cannot dump value #{value} to type #{type}"}
  end

  defmacro __using__(opts) do
    name = opts[:name]

    unless name do
      raise "Module name is required."
    end

    quote bind_quoted: [name: name] do
      use GenServer

      import Dagger.Mod

      @name name

      Module.register_attribute(__MODULE__, :name, persist: true)

      def start_link(_) do
        GenServer.start_link(__MODULE__, [], name: __MODULE__)
      end

      def init([]) do
        Dagger.Mod.Registry.register(__MODULE__)
        {:ok, []}
      end
    end
  end
end
