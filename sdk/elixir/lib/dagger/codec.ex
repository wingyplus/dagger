defmodule Dagger.Decoder do
  @moduledoc false

  @doc """
  Decode `value` into struct of that type.
  """
  @callback __decode__(dag :: Dagger.Client.t(), value :: term()) :: struct()
end

defmodule Dagger.Decode do
  @moduledoc false

  @doc """
  Decode the `value` from JSON into proper type.
  """
  def decode(value, type_or_module, dag) do
    with {:ok, value} <- Jason.decode(value) do
      cast(value, type_or_module, dag)
    end
  end

  defp cast(value, :integer, _) when is_integer(value) do
    {:ok, value}
  end

  defp cast(value, :boolean, _) when is_boolean(value) do
    {:ok, value}
  end

  defp cast(value, :string, _) when is_binary(value) do
    {:ok, value}
  end

  defp cast(values, {:list, type}, dag) when is_list(values) do
    values =
      for value <- values do
        {:ok, value} = cast(value, type, dag)
        value
      end

    {:ok, values}
  end

  defp cast(nil, {:optional, _type}, _dag), do: {:ok, nil}
  defp cast(value, {:optional, type}, dag), do: cast(value, type, dag)

  defp cast(value, module, dag) when is_binary(value) and is_atom(module) do
    {:ok, module.__decode__(dag, value)}
  end

  defp cast(value, type, _) do
    {:error, "cannot cast value #{value} to type #{type}"}
  end
end

defmodule Dagger.EncodeError do
  defexception [:message]
end

defprotocol Dagger.Encoder do
  @moduledoc false

  @doc """
  Encode `value` into another.
  """
  def __encode__(value)
end

defmodule Dagger.Encode do
  @moduledoc false

  @doc """
  Encode a `result` into JSON.
  """
  def encode(result, type) do
    with {:ok, value} <- dump(result, type) do
      Jason.encode(value)
    end
  end

  defp dump(value, :integer) when is_integer(value) do
    {:ok, value}
  end

  defp dump(value, :boolean) when is_boolean(value) do
    {:ok, value}
  end

  defp dump(value, :string) when is_binary(value) do
    {:ok, value}
  end

  defp dump(values, {:list, type}) when is_list(values) do
    values =
      for value <- values do
        {:ok, value} = dump(value, type)
        value
      end

    {:ok, values}
  end

  defp dump(%module{} = struct, module) do
    Dagger.Encoder.__encode__(struct)
  end

  defp dump(value, type) do
    {:error, "cannot dump value #{value} to type #{type}"}
  end
end
