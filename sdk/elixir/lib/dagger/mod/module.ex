defmodule Dagger.Mod.Module do
  @moduledoc false

  alias Dagger.Mod.Object
  alias Dagger.Mod.Registry

  @doc """
  Define a Dagger module from the given module.
  """
  @spec define(Dagger.Client.t(), module()) :: Dagger.Module.t()
  def define(dag, module) when is_struct(dag, Dagger.Client) and is_atom(module) do
    module
    |> Registry.register()
    |> Registry.all_modules()
    |> Enum.reduce(Dagger.Client.module(dag), &define(dag, &2, &1))
    |> maybe_with_description(Object.get_module_doc(module))
  end

  defp maybe_with_description(module, nil), do: module
  defp maybe_with_description(module, doc), do: Dagger.Module.with_description(module, doc)

  defp define(dag, dag_module, module) do
    case module.__kind__() do
      :object ->
        Dagger.Module.with_object(dag_module, module.__register__(dag))

      :enum ->
        Dagger.Module.with_enum(dag_module, module.__register__(dag))
    end
  end
end
