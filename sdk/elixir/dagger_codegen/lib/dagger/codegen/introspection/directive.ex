defmodule Dagger.Codegen.Introspection.Directives.SourceMap do
  @moduledoc false

  defstruct [:module, :filename, :line, :column]

  def cast(args) do
    args =
      Enum.map(args, fn %{"name" => name, "value" => value} ->
        {String.to_existing_atom(name), value}
      end)

    struct(__MODULE__, args)
  end
end

defmodule Dagger.Codegen.Introspection.Directives do
  @moduledoc false

  alias Dagger.Codegen.Introspection.Directives.SourceMap

  def source_map(%{directives: directives}) do
    case Enum.find(directives, &(&1["name"] == "sourceMap")) do
      nil -> nil
      %{"args" => args} -> SourceMap.cast(args)
    end
  end
end
