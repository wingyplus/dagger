# This file generated by `mix dagger.gen`. Please DO NOT EDIT.
defmodule Dagger.Host do
  @moduledoc "Information about the host execution environment."
  use Dagger.QueryBuilder
  defstruct [:selection, :client]

  def directory(%__MODULE__{} = host, opts) do
    selection = select(host.selection, "directory")
    selection = arg(selection, to_string(:path), Keyword.fetch!(opts, :path))

    {_opts, selection} =
      [:exclude, :include]
      |> Enum.reduce({opts, selection}, fn arg, {opts, selection} ->
        if not is_nil(opts[arg]) do
          {opts, arg(selection, to_string(arg), opts[arg])}
        else
          {opts, selection}
        end
      end)

    %Dagger.Directory{selection: selection, client: host.client}
  end

  def env_variable(%__MODULE__{} = host, opts) do
    selection = select(host.selection, "envVariable")
    selection = arg(selection, to_string(:name), Keyword.fetch!(opts, :name))
    execute(selection, host.client)
  end

  def unix_socket(%__MODULE__{} = host, opts) do
    selection = select(host.selection, "unixSocket")
    selection = arg(selection, to_string(:path), Keyword.fetch!(opts, :path))
    %Dagger.Socket{selection: selection, client: host.client}
  end

  def workdir(%__MODULE__{} = host, opts) do
    selection = select(host.selection, "workdir")

    {_opts, selection} =
      [:exclude, :include]
      |> Enum.reduce({opts, selection}, fn arg, {opts, selection} ->
        if not is_nil(opts[arg]) do
          {opts, arg(selection, to_string(arg), opts[arg])}
        else
          {opts, selection}
        end
      end)

    %Dagger.Directory{selection: selection, client: host.client}
  end
end
