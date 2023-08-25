defmodule Dagger.GraphQL.QueryBuilder do
  @moduledoc """
  Provides a functionallity to contruct GraphQL query.
  """

  def query(), do: [field("query")]

  def select(query, fields) when is_list(fields) do
    [fields | query]
  end

  def select(query, name) do
    [field(name) | query]
  end

  def select(query, name, args) do
    [field(name, args) | query]
  end

  def field(name, args \\ []) when is_binary(name) and is_list(args) do
    {name, args}
  end

  def build(query) do
    query
    |> encode_to_iodata()
    |> IO.iodata_to_binary()
  end

  def encode_to_iodata([node]) do
    [encode_field(node), "{}"]
  end

  def encode_to_iodata([child | parent]) do
    parent
    |> Enum.reduce(encode_field(child), fn parent, acc ->
      [encode_field(parent), ~c"{", acc, ~c"}"]
    end)
  end

  defp encode_field(nil), do: []

  defp encode_field(field) when is_list(field) do
    Enum.join(field, " ")
  end

  defp encode_field({name, args}) do
    [name, encode_args(args)]
  end

  defp encode_args([]), do: []

  defp encode_args(args) do
    [~c"(", Enum.map_intersperse(args, ",", &encode_key_value/1), ~c")"]
  end

  defp encode_key_value({key, value}), do: [to_string(key), ~c":", encode_value(value)]

  defp encode_value(value) when is_binary(value) do
    value =
      value
      |> String.replace("\n", "\\n")
      |> String.replace("\t", "\\t")
      |> String.replace("\"", "\\\"")

    [~c"\"", value, ~c"\""]
  end

  defp encode_value(value) when is_binary(value), do: [~c"\"", value, ~c"\""]

  defp encode_value(value) when is_list(value) do
    [~c"[", Enum.map_intersperse(value, ",", &encode_value/1), ~c"]"]
  end

  defp encode_value(value) when is_map(value) do
    [
      ~c"{",
      Enum.map_intersperse(value, ",", &encode_key_value/1),
      ~c"}"
    ]
  end

  defp encode_value(value), do: to_string(value)

  def path([]), do: []

  def path([{name, _} | parent]) do
    [name | path(parent)]
  end

  def path([fields | parent]) when is_list(fields) do
    [fields | path(parent)]
  end
end
