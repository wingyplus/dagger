# This file generated by `mix dagger.gen`. Please DO NOT EDIT.
defmodule Dagger.Query do
  @moduledoc ""
  use Dagger.QueryBuilder
  defstruct [:selection, :client]

  (
    @doc "Constructs a cache volume for a given cache key.\n\n## Required Arguments\n\n* `key` - A string identifier to target this cache volume (e.g., \"modules-cache\").\n\n## Optional Arguments"
    def cache_volume(%__MODULE__{} = query, args) do
      selection = select(query.selection, "cacheVolume")
      selection = arg(selection, "key", Keyword.fetch!(args, :key))
      %Dagger.CacheVolume{selection: selection, client: query.client}
    end
  )

  (
    @doc "Loads a container from ID.\n\nNull ID returns an empty container (scratch).\nOptional platform argument initializes new containers to execute and publish as that platform.\nPlatform defaults to that of the builder's host.\n\n## Required Arguments\n\n\n\n## Optional Arguments\n\n* `id` - \n* `platform` -"
    def container(%__MODULE__{} = query, args) do
      selection = select(query.selection, "container")

      (
        selection =
          if not is_nil(args[:id]) do
            arg(selection, "id", args[:id])
          else
            selection
          end

        selection =
          if not is_nil(args[:platform]) do
            arg(selection, "platform", args[:platform])
          else
            selection
          end
      )

      %Dagger.Container{selection: selection, client: query.client}
    end
  )

  (
    @doc "The default platform of the builder.\n\n## Required Arguments\n\n\n\n## Optional Arguments"
    def default_platform(%__MODULE__{} = query) do
      selection = select(query.selection, "defaultPlatform")
      execute(selection, query.client)
    end
  )

  (
    @doc "Load a directory by ID. No argument produces an empty directory.\n\n## Required Arguments\n\n\n\n## Optional Arguments\n\n* `id` -"
    def directory(%__MODULE__{} = query, args) do
      selection = select(query.selection, "directory")

      selection =
        if not is_nil(args[:id]) do
          arg(selection, "id", args[:id])
        else
          selection
        end

      %Dagger.Directory{selection: selection, client: query.client}
    end
  )

  (
    @doc "Loads a file by ID.\n\n## Required Arguments\n\n* `id` - \n\n## Optional Arguments"
    def file(%__MODULE__{} = query, args) do
      selection = select(query.selection, "file")
      selection = arg(selection, "id", Keyword.fetch!(args, :id))
      execute(selection, query.client)
    end
  )

  (
    @doc "Queries a git repository.\n\n## Required Arguments\n\n* `url` - Url of the git repository.\nCan be formatted as https://{host}/{owner}/{repo}, git@{host}/{owner}/{repo}\nSuffix \".git\" is optional.\n\n## Optional Arguments\n\n* `keep_git_dir` - Set to true to keep .git directory.\n* `experimental_service_host` - A service which must be started before the repo is fetched."
    def git(%__MODULE__{} = query, args) do
      selection = select(query.selection, "git")
      selection = arg(selection, "url", Keyword.fetch!(args, :url))

      (
        selection =
          if not is_nil(args[:keep_git_dir]) do
            arg(selection, "keepGitDir", args[:keep_git_dir])
          else
            selection
          end

        selection =
          if not is_nil(args[:experimental_service_host]) do
            arg(selection, "experimentalServiceHost", args[:experimental_service_host])
          else
            selection
          end
      )

      %Dagger.GitRepository{selection: selection, client: query.client}
    end
  )

  (
    @doc "Queries the host environment.\n\n## Required Arguments\n\n\n\n## Optional Arguments"
    def host(%__MODULE__{} = query) do
      selection = select(query.selection, "host")
      %Dagger.Host{selection: selection, client: query.client}
    end
  )

  (
    @doc "Returns a file containing an http remote url content.\n\n## Required Arguments\n\n* `url` - HTTP url to get the content from (e.g., \"https://docs.dagger.io\").\n\n## Optional Arguments\n\n* `experimental_service_host` - A service which must be started before the URL is fetched."
    def http(%__MODULE__{} = query, args) do
      selection = select(query.selection, "http")
      selection = arg(selection, "url", Keyword.fetch!(args, :url))

      selection =
        if not is_nil(args[:experimental_service_host]) do
          arg(selection, "experimentalServiceHost", args[:experimental_service_host])
        else
          selection
        end

      %Dagger.File{selection: selection, client: query.client}
    end
  )

  (
    @doc "Creates a named sub-pipeline.\n\n## Required Arguments\n\n* `name` - Pipeline name.\n\n## Optional Arguments\n\n* `description` - Pipeline description.\n* `labels` - Pipeline labels."
    def pipeline(%__MODULE__{} = query, args) do
      selection = select(query.selection, "pipeline")
      selection = arg(selection, "name", Keyword.fetch!(args, :name))

      (
        selection =
          if not is_nil(args[:description]) do
            arg(selection, "description", args[:description])
          else
            selection
          end

        selection =
          if not is_nil(args[:labels]) do
            arg(selection, "labels", args[:labels])
          else
            selection
          end
      )

      %Dagger.Query{selection: selection, client: query.client}
    end
  )

  (
    @doc "Look up a project by name\n\n## Required Arguments\n\n* `name` - \n\n## Optional Arguments"
    def project(%__MODULE__{} = query, args) do
      selection = select(query.selection, "project")
      selection = arg(selection, "name", Keyword.fetch!(args, :name))
      %Dagger.Project{selection: selection, client: query.client}
    end
  )

  (
    @doc "Loads a secret from its ID.\n\n## Required Arguments\n\n* `id` - \n\n## Optional Arguments"
    def secret(%__MODULE__{} = query, args) do
      selection = select(query.selection, "secret")
      selection = arg(selection, "id", Keyword.fetch!(args, :id))
      %Dagger.Secret{selection: selection, client: query.client}
    end
  )

  (
    @doc "Sets a secret given a user defined name to its plaintext and returns the secret.\n\n## Required Arguments\n\n* `name` - The user defined name for this secret\n* `plaintext` - The plaintext of the secret\n\n## Optional Arguments"
    def set_secret(%__MODULE__{} = query, args) do
      selection = select(query.selection, "setSecret")
      selection = arg(selection, "name", Keyword.fetch!(args, :name))
      selection = arg(selection, "plaintext", Keyword.fetch!(args, :plaintext))
      %Dagger.Secret{selection: selection, client: query.client}
    end
  )

  (
    @doc "Loads a socket by its ID.\n\n## Required Arguments\n\n\n\n## Optional Arguments\n\n* `id` -"
    def socket(%__MODULE__{} = query, args) do
      selection = select(query.selection, "socket")

      selection =
        if not is_nil(args[:id]) do
          arg(selection, "id", args[:id])
        else
          selection
        end

      %Dagger.Socket{selection: selection, client: query.client}
    end
  )
end
