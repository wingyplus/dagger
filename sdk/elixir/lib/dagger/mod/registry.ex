defmodule Dagger.Mod.Registry do
  @moduledoc false

  defstruct modules: %{}, enums: []

  def register(root_module) do
    root_module
    |> collect_all([])
    |> Enum.reduce(%__MODULE__{}, &put_module(&2, &1))
  end

  defp collect_all(module, visited) do
    if module in visited do
      []
    else
      visited = [module | visited]

      transitive =
        Enum.flat_map(module.__dependencies__(), fn dep -> collect_all(dep, visited) end)

      [module | transitive]
      |> Enum.uniq()
    end
  end

  def put_module(%__MODULE__{} = registry, module) when is_atom(module) do
    case module.__kind__() do
      :enum ->
        %__MODULE__{registry | enums: [module | registry.enums]}

      :object ->
        %__MODULE__{registry | modules: Map.put(registry.modules, module.__name__(), module)}

      _ ->
        registry
    end
  end

  def all_modules(%__MODULE__{} = registry) do
    (Map.values(registry.modules) ++ registry.enums) |> Enum.sort()
  end

  def get_module_by_name!(%__MODULE__{} = registry, name) when is_binary(name) do
    case registry.modules[name] do
      nil -> raise "Cannot find `#{name}` in the registry."
      module -> module
    end
  end
end
