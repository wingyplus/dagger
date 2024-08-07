defmodule Dagger.Mod.Object do
  @moduledoc """
  A module for declare an object type.
  """

  defmacro defn(call, do: block) do
    {name, args, return} = extract_call(call |> dbg())
    has_self? = is_tuple(args)
    arg_defs = compile_args(args)
    return_def = compile_typespec!(return)

    quote do
      @function {unquote(name),
                 [self: unquote(has_self?), args: unquote(arg_defs), return: unquote(return_def)]}
      unquote(Dagger.Mod.Object.Defn.define(name, args, block))
    end
  end

  defmacro __using__(opts) do
    quote do
      use Dagger.Mod, unquote(opts)

      import Dagger.Mod.Object, only: [defn: 2]
      import Dagger.Global, only: [dag: 0]

      Module.register_attribute(__MODULE__, :function, accumulate: true, persist: true)
    end
  end

  defp extract_call({:"::", _, [call_def, return]}) do
    {name, args} = extract_call_def(call_def)
    {name, args, return}
  end

  defp extract_call_def({name, _, [args]}) do
    {name, args}
  end

  defp extract_call_def({name, _, [self, args]}) do
    {name, {self, args}}
  end

  defp compile_args({_, args}) do
    compile_args(args)
  end

  defp compile_args(args) do
    for {name, spec} <- args do
      {name, [type: compile_typespec!(spec)]}
    end
  end

  # binary()
  defp compile_typespec!({:binary, _, []}), do: :string
  # integer()
  defp compile_typespec!({:integer, _, []}), do: :integer
  # boolean()
  defp compile_typespec!({:boolean, _, []}), do: :boolean

  # String.t() 
  defp compile_typespec!(
         {{:., _,
           [
             {:__aliases__, _, [:String]},
             :t
           ]}, _, []}
       ) do
    :string
  end

  defp compile_typespec!({{:., _, [{:__aliases__, _, module}, :t]}, _, []}) do
    Module.concat(module)
  end

  defp compile_typespec!({:list, _, [type]}) do
    {:list, compile_typespec!(type)}
  end

  defp compile_typespec!([type]) do
    {:list, compile_typespec!(type)}
  end

  defp compile_typespec!(unsupported_type) do
    raise ArgumentError, "type `#{Macro.to_string(unsupported_type)}` is not supported"
  end
end
