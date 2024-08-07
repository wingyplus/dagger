defmodule Dagger.Mod.Object.Defn do
  @moduledoc false

  def define(name, args, block) do
    args =
      case args do
        {self, args} ->
          [var(self) | Enum.map(args, &var/1)]

        args ->
          Enum.map(args, &var/1)
      end

    quote do
      def unquote(name)(unquote_splicing(args)) do
        unquote(block)
      end
    end
  end

  # {var, type}
  defp var({name, _}) do
    Macro.var(name, nil)
  end

  # var
  defp var({name, _, nil}) do
    Macro.var(name, nil)
  end

  # pattern = var
  defp var({:=, _, [_, v]}) do
    var(v)
  end
end
