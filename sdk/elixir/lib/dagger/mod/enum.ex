defmodule Dagger.Mod.Enum do
  @moduledoc """
  Declare a module as an enum type.
  """

  defmacro __using__(opts) do
    values = opts[:values]
    name = opts[:name]

    if is_nil(values) do
      raise "The option `:values` need to be set."
    end

    functions = Enum.map(values, &defenum/1)

    atoms =
      Enum.map_join(values, "|", fn v ->
        case v do
          {k, _v} when is_atom(k) -> k
          k when is_atom(k) -> k
        end
        |> Macro.to_string()
      end)

    {:ok, ast_type} = Code.string_to_quoted("@type t() :: #{atoms}")

    # Pre-compute member registration data at compile time.
    # Each entry: {display_value_string, doc_or_nil}
    member_data = Enum.map(values, &extract_member_data/1)
    camelized_name = Dagger.Mod.Helper.camelize(name)

    quote do
      use Dagger.Core.Base, kind: :enum, name: unquote(name)

      unquote(ast_type)

      def __enum__(:name), do: unquote(name)
      def __enum__(:keys), do: unquote(values)

      unquote_splicing(functions)

      @doc false
      def __register__(dag) do
        Enum.reduce(
          unquote(Macro.escape(member_data)),
          dag |> Dagger.Client.type_def() |> Dagger.TypeDef.with_enum(unquote(camelized_name)),
          fn {val, doc}, td ->
            opts = [value: val] ++ if(doc, do: [description: doc], else: [])
            Dagger.TypeDef.with_enum_member(td, val, opts)
          end
        )
      end

      @doc false
      def __dependencies__(), do: []
    end
  end

  # Extract {value_string, doc_or_nil} from an enum value spec.
  # value_string matches what Atom.to_string/1 gives for the key atom,
  # matching the behaviour of the existing define_enum/2 in Dagger.Mod.Module.
  defp extract_member_data(key) when is_atom(key) do
    {Atom.to_string(key), nil}
  end

  defp extract_member_data({key, options}) when is_atom(key) and is_list(options) do
    {Atom.to_string(key), options[:doc]}
  end

  defp extract_member_data({key, value}) when is_atom(key) and is_binary(value) do
    {Atom.to_string(key), nil}
  end

  defp extract_member_data({key, {_value, options}}) when is_atom(key) and is_list(options) do
    {Atom.to_string(key), options[:doc]}
  end

  defp defenum(key) when is_atom(key) do
    value = Atom.to_string(key)
    fname = String.downcase(value) |> String.to_atom()

    quote do
      def __enum__(:value, unquote(key)), do: unquote(value)
      def __enum__(:key, unquote(value)), do: unquote(key)
      def __enum__(:doc, unquote(key)), do: nil

      def unquote(fname)(), do: unquote(key)
      def from_string(unquote(value)), do: unquote(key)
    end
  end

  defp defenum({key, options}) when is_atom(key) and is_list(options) do
    value = Atom.to_string(key)
    doc = options[:doc]
    fname = String.downcase(value) |> String.to_atom()

    quote do
      def __enum__(:value, unquote(key)), do: unquote(value)
      def __enum__(:key, unquote(value)), do: unquote(key)
      def __enum__(:doc, unquote(key)), do: unquote(doc)

      def unquote(fname)(), do: unquote(key)
      def from_string(unquote(value)), do: unquote(key)
    end
  end

  defp defenum({key, value}) when is_atom(key) and is_binary(value) do
    fname = String.downcase(value) |> String.to_atom()

    quote do
      def __enum__(:value, unquote(key)), do: unquote(value)
      def __enum__(:key, unquote(value)), do: unquote(key)
      def __enum__(:doc, unquote(key)), do: nil

      def unquote(fname)(), do: unquote(key)
      def from_string(unquote(value)), do: unquote(key)
    end
  end

  defp defenum({key, {value, options}})
       when is_atom(key) and is_binary(value) and is_list(options) do
    doc = options[:doc]
    fname = String.downcase(value) |> String.to_atom()

    quote do
      def __enum__(:value, unquote(key)), do: unquote(value)
      def __enum__(:key, unquote(value)), do: unquote(key)
      def __enum__(:doc, unquote(key)), do: unquote(doc)

      def unquote(fname)(), do: unquote(key)
      def from_string(unquote(value)), do: unquote(key)
    end
  end

  def get_key_description(module, key) do
    if Code.ensure_loaded?(module) and function_exported?(module, :__enum__, 2) do
      module.__enum__(:doc, key)
    else
      nil
    end
  end
end
