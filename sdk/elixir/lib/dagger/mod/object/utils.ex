defmodule Dagger.Mod.Object.Utils do
  @moduledoc false

  def fields_to_type(fields) do
    spec = typespec_for_struct(fields)

    quote do
      @type t() :: unquote(spec)
    end
  end

  def fields_to_struct(fields) do
    names = struct_field_names(fields)

    quote do
      defstruct unquote(names)
    end
  end

  def implement_decoder(_fields) do
    quote do
      @impl Dagger.Decoder
      def __decode__(_dag, _value) do
        # TODO: implements me.
        %__MODULE__{}
      end
    end
  end

  def implement_encoder(_fields) do
    # {:%{}, [], Enum.map(fields, fn {k, v} -> {k, v} end)}

    quote do
      defimpl Dagger.Encoder do
        def __encode__(value) do
          # TODO: oh you cannot do something like this! the object key is camel case
          # while struct fields is snake case.
          {:ok, Map.from_struct(value)}
        end
      end
    end
  end

  defp typespec_for_struct(fields) do
    fields =
      for {name, type, _, _} <- fields do
        {name, type}
      end

    {:%, [],
     [
       {:__MODULE__, [], nil},
       {:%{}, [], fields}
     ]}
  end

  defp struct_field_names(fields) do
    for {name, _, _, _} <- fields do
      name
    end
  end
end
