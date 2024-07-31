defmodule Dagger.Mod.Object do
  @moduledoc """
  Define a module as an object type.
  """

  alias Dagger.Mod.Object.Defn

  def __on_definition__(env, :def, _name, _args, _guards, _body) do
    case Module.get_attribute(env.module, :function) do
      nil ->
        :ok

      function ->
        functions = Module.get_attribute(env.module, :functions)
        Module.put_attribute(env.module, :functions, [function | functions])
        Module.delete_attribute(env.module, :function)
    end
  end

  def __on_definition__(_env, :defp, _name, _args, _guards, _body) do
    :ok
  end

  @doc """
  Declare a function.

  The function can be declare with syntax:

      defn name(args) :: return_type do
        # body of the function...
      end

  TODO: write me.

  ## Declare arguments

  The function syntax uses keyword list to declare the arguments by
  the key is the argument name, and the type as value. For example:application

      defn hello(name: :string) :: :string

  The function `hello` has 1 argument, the argument `name` type string.

  Currently, the argument type support only:

  - `:string` - the string type.
  - `:integer` - the integer type.
  - `:boolean` - the boolean type.
  - `module` - any module type.

  ## Examples

  Declare a function that accepts string type and return string:

      defmodule AnObject do
        use Dagger.Mod.Object

        defn hello(name: String) :: String do
          dag
          |> Dagger.Client.container()
          |> Dagger.Container.from("alpine")
          |> Dagger.Container.with_exec(["echo", name])
          |> Dagger.Container.stdout()
        end
      end
  """
  defmacro defn(call, do: block) do
    {name, args, return} = extract_call(call)
    args = Enum.map(args, &compile_arg/1)
    return_type = compile_return_type(return)
    fn_def = define_function(name, args, return_type)
    Defn.define(fn_def, block)
  end

  defp define_function(name, args, return_type) do
    {name, [args: args, return: return_type]}
  end

  defp compile_arg({name, type}) when is_atom(type) do
    {name, [type: type]}
  end

  defp compile_return_type(type) when is_atom(type) do
    type
  end

  defp compile_return_type({:__aliases__, _, type}) do
    type
  end

  defp extract_call({:"::", _, [{name, _, fn_args}, return]}) do
    args =
      case fn_args do
        [] -> []
        [args] -> args
      end

    {name, args, return}
  end

  defmacro __using__(_opts) do
    quote do
      import Dagger.Mod.Object

      @on_definition Dagger.Mod.Object
      @functions []

      Module.register_attribute(__MODULE__, :functions, persist: true)
    end
  end
end
