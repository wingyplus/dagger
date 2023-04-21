# This file generated by `mix dagger.gen`. Please DO NOT EDIT.
defmodule Dagger.GitRef do
  @moduledoc "A git ref (tag, branch or commit)."
  use Dagger.QueryBuilder
  defstruct [:selection, :client]

  (
    @doc "The digest of the current value of this ref."
    def digest(%__MODULE__{} = git_ref) do
      selection = select(git_ref.selection, "digest")
      execute(selection, git_ref.client)
    end
  )

  (
    @doc "The filesystem tree at this ref."
    def tree(%__MODULE__{} = git_ref, opts) do
      selection = select(git_ref.selection, "tree")

      {_opts, selection} =
        [:ssh_known_hosts, :ssh_auth_socket]
        |> Enum.reduce({opts, selection}, fn arg, {opts, selection} ->
          if not is_nil(opts[arg]) do
            {opts, arg(selection, to_string(arg), opts[arg])}
          else
            {opts, selection}
          end
        end)

      %Dagger.Directory{selection: selection, client: git_ref.client}
    end
  )
end