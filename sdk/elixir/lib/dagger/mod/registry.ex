defmodule Dagger.Mod.Registry do
  @moduledoc false

  defstruct modules: %{}, enums: []

  def register(root_module) do
    root_module
    |> collect_all([])
    |> Enum.reduce(%__MODULE__{}, &put_module(&2, &1))
  end

  # Collect root_module and all its transitive dependencies.
  # Uses the compile-time __dependencies__/0 when available; falls back to the
  # legacy runtime traversal otherwise.
  defp collect_all(module, visited) do
    if module in visited do
      []
    else
      visited = [module | visited]

      direct_deps =
        if function_exported?(module, :__dependencies__, 0) do
          module.__dependencies__()
        else
          runtime_deps(module)
        end

      transitive =
        Enum.flat_map(direct_deps, fn dep -> collect_all(dep, visited) end)

      [module | transitive]
      |> Enum.uniq()
    end
  end

  # Legacy: derive direct dependencies by inspecting FunctionDef structs.
  defp runtime_deps(module) do
    if function_exported?(module, :__object__, 1) do
      module.__object__(:functions)
      |> Enum.flat_map(fn {_, fun_def} ->
        arg_modules = collect_enums_from_funs(fun_def)
        ret_modules = if object?(fun_def.return), do: [fun_def.return], else: []
        arg_modules ++ ret_modules
      end)
      |> Enum.uniq()
    else
      []
    end
  end

  def put_module(%__MODULE__{} = registry, module) when is_atom(module) do
    if function_exported?(module, :__kind__, 0) do
      case module.__kind__() do
        :enum ->
          %__MODULE__{registry | enums: [module | registry.enums]}

        :object ->
          %__MODULE__{registry | modules: Map.put(registry.modules, module.__name__(), module)}

        _ ->
          registry
      end
    else
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

  defp collect_enums_from_funs(fun_def) do
    fun_def.args
    |> Enum.flat_map(fn {_name, arg_def} ->
      type = Keyword.fetch!(arg_def, :type)
      collect_enums_from_args(type)
    end)
  end

  defp collect_enums_from_args({:optional, type}), do: collect_enums_from_args(type)
  defp collect_enums_from_args({:list, type}), do: collect_enums_from_args(type)

  defp collect_enums_from_args(module) when is_atom(module) do
    if Code.ensure_loaded?(module) and function_exported?(module, :__kind__, 0) and
         module.__kind__() == :enum do
      [module]
    else
      []
    end
  end

  defp collect_enums_from_args(_), do: []

  defp object?(:integer), do: false
  defp object?(:float), do: false
  defp object?(:boolean), do: false
  defp object?(:string), do: false
  defp object?({:list, type}), do: object?(type)
  defp object?({:optional, type}), do: object?(type)

  defp object?(type),
    do: type.__kind__() == :object and function_exported?(type, :__object__, 1)
end
