defmodule Dagger.Mod.Object.Defn do
  @moduledoc false

  def define({name, definition} = fn_def, block) do
    vars = definition |> Keyword.fetch!(:args) |> vars_quote()

    quote do
      @function unquote(fn_def)
      def unquote(name)(unquote_splicing(vars)) do
        unquote(block)
      end
    end
  end

  # Declare `@function` annotation.

  defp vars_quote(args) do
    vars = args |> Enum.map(&arg_name/1) |> Enum.map(&Macro.var(&1, nil))
    # mark `dag` variable as a generated to avoid unused variable
    # if the the function doesn't uses this variable.
    [{:dag, [generated: true], nil} | vars]
  end

  defp arg_name({name, _}), do: name
end
