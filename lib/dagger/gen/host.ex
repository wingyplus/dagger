# This file generated by `mix dagger.gen`. Please DO NOT EDIT.
defmodule Dagger.Host do
  @moduledoc "Information about the host execution environment."
  use Dagger.QueryBuilder
  defstruct [:selection, :client]

  (
    @doc "Accesses a directory on the host.\n\n## Required Arguments\n\n* `path` - Location of the directory to access (e.g., \".\").\n\n## Optional Arguments\n\n* `exclude` - Exclude artifacts that match the given pattern (e.g., [\"node_modules/\", \".git*\"]).\n* `include` - Include only artifacts that match the given pattern (e.g., [\"app/\", \"package.*\"])."
    def directory(%__MODULE__{} = host, path, optional_args \\ []) do
      selection = select(host.selection, "directory")
      selection = arg(selection, "path", path)

      selection =
        if not is_nil(optional_args[:exclude]) do
          arg(selection, "exclude", optional_args[:exclude])
        else
          selection
        end

      selection =
        if not is_nil(optional_args[:include]) do
          arg(selection, "include", optional_args[:include])
        else
          selection
        end

      %Dagger.Directory{selection: selection, client: host.client}
    end
  )

  (
    @doc "Accesses an environment variable on the host.\n\n## Required Arguments\n\n* `name` - Name of the environment variable (e.g., \"PATH\")."
    def env_variable(%__MODULE__{} = host, name) do
      selection = select(host.selection, "envVariable")
      selection = arg(selection, "name", name)
      execute(selection, host.client)
    end
  )

  (
    @doc "Accesses a Unix socket on the host.\n\n## Required Arguments\n\n* `path` - Location of the Unix socket (e.g., \"/var/run/docker.sock\")."
    def unix_socket(%__MODULE__{} = host, path) do
      selection = select(host.selection, "unixSocket")
      selection = arg(selection, "path", path)
      %Dagger.Socket{selection: selection, client: host.client}
    end
  )

  (
    @doc "Retrieves the current working directory on the host.\n\n\n\n## Optional Arguments\n\n* `exclude` - Exclude artifacts that match the given pattern (e.g., [\"node_modules/\", \".git*\"]).\n* `include` - Include only artifacts that match the given pattern (e.g., [\"app/\", \"package.*\"])."
    @deprecated "Use `directory` with path set to '.' instead."
    def workdir(%__MODULE__{} = host, optional_args \\ []) do
      selection = select(host.selection, "workdir")

      selection =
        if not is_nil(optional_args[:exclude]) do
          arg(selection, "exclude", optional_args[:exclude])
        else
          selection
        end

      selection =
        if not is_nil(optional_args[:include]) do
          arg(selection, "include", optional_args[:include])
        else
          selection
        end

      %Dagger.Directory{selection: selection, client: host.client}
    end
  )
end
