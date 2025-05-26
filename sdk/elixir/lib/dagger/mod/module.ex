defmodule Dagger.Mod.Module do
  @moduledoc false

  alias Dagger.Mod.Helper
  alias Dagger.Mod.Object
  alias Dagger.Mod.Object.FieldDef
  alias Dagger.Mod.Object.FunctionDef

  @doc """
  Define a Dagger module from the given module.
  """
  @spec define(Dagger.Client.t(), module()) :: Dagger.Module.t()
  def define(%Dagger.Client{} = dag, root_module) when is_atom(root_module) do
    modules = traverse(root_module)

    dag
    |> Dagger.Client.module()
    |> define_objects(dag, modules)
    |> maybe_with_description(Object.get_module_doc(root_module))
  end

  defp maybe_with_description(module, nil), do: module
  defp maybe_with_description(module, doc), do: Dagger.Module.with_description(module, doc)

  defp define_objects(dag_module, dag, modules) do
    modules
    |> Enum.reduce(dag_module, fn module, dag_module ->
      dag_module
      |> Dagger.Module.with_object(define_object(dag, module))
    end)
  end

  defp define_object(dag, module) do
    mod_name = module.__object__(:name)

    dag
    |> Dagger.Client.type_def()
    |> Dagger.TypeDef.with_object(Helper.camelize(mod_name))
    |> then(&define_fields(&1, dag, module))
    |> then(&define_functions(&1, dag, module))
    |> then(&define_constructor(&1, dag, module))
  end

  defp define_constructor(type_def, dag, module) do
    case Enum.find(module.__object__(:functions), &init?/1) do
      nil ->
        type_def

      {name, fun_def} ->
        type_def
        |> Dagger.TypeDef.with_constructor(
          FunctionDef.to_dag_function(fun_def, name, module, dag)
        )
    end
  end

  defp define_fields(type_def, dag, module) do
    module.__object__(:fields)
    |> Enum.reduce(type_def, fn {name, field_def}, type_def ->
      FieldDef.define(field_def, name, type_def, dag)
    end)
  end

  defp define_functions(type_def, dag, module) do
    module.__object__(:functions)
    |> Enum.reject(&init?/1)
    |> Enum.reduce(type_def, fn {name, fun_def}, type_def ->
      FunctionDef.define(fun_def, name, module, type_def, dag)
    end)
  end

  defp init?({:init, _}), do: true
  defp init?({_, _}), do: false

  # Traverse all functions to find objects that it depends on.
  defp traverse(root_module) do
    traverse(root_module, root_module.__object__(:functions), [root_module])
  end

  defp traverse(_root_module, [], modules), do: Enum.uniq(modules)

  defp traverse(root_module, [%FunctionDef{return: type} | funs], modules) do
    case Module.split(type) do
      # User define object
      [^root_module | _] ->
        traverse(root_module, funs, modules ++ [type] ++ traverse(type))

      _ ->
        traverse(root_module, funs, modules)
    end
  end
end
