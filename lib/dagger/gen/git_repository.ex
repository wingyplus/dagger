# This file generated by `mix dagger.gen`. Please DO NOT EDIT.
defmodule Dagger.GitRepository do
  @moduledoc "A git repository."
  use Dagger.QueryBuilder
  defstruct [:selection, :client]

  (
    @doc "Returns details on one branch.\n\n## Required Arguments\n\n* `name` - Branch's name (e.g., \"main\")."
    def branch(%__MODULE__{} = git_repository, name) do
      selection = select(git_repository.selection, "branch")
      selection = arg(selection, "name", name)
      %Dagger.GitRef{selection: selection, client: git_repository.client}
    end
  )

  (
    @doc "Lists of branches on the repository."
    def branches(%__MODULE__{} = git_repository) do
      selection = select(git_repository.selection, "branches")
      execute(selection, git_repository.client)
    end
  )

  (
    @doc "Returns details on one commit.\n\n## Required Arguments\n\n* `id` - Identifier of the commit (e.g., \"b6315d8f2810962c601af73f86831f6866ea798b\")."
    def commit(%__MODULE__{} = git_repository, id) do
      selection = select(git_repository.selection, "commit")
      selection = arg(selection, "id", id)
      %Dagger.GitRef{selection: selection, client: git_repository.client}
    end
  )

  (
    @doc "Returns details on one tag.\n\n## Required Arguments\n\n* `name` - Tag's name (e.g., \"v0.3.9\")."
    def tag(%__MODULE__{} = git_repository, name) do
      selection = select(git_repository.selection, "tag")
      selection = arg(selection, "name", name)
      %Dagger.GitRef{selection: selection, client: git_repository.client}
    end
  )

  (
    @doc "Lists of tags on the repository."
    def tags(%__MODULE__{} = git_repository) do
      selection = select(git_repository.selection, "tags")
      execute(selection, git_repository.client)
    end
  )
end
