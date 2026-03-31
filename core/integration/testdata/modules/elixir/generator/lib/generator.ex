defmodule Generator do
  @moduledoc false

  use Dagger.Mod.Object, name: "Generator"

  @generate true
  defn generate_file() :: Dagger.Directory.t() do
    dag()
    |> Dagger.Client.directory()
    |> Dagger.Directory.with_new_file("generated.txt", "hello from generator")
  end

  @generate true
  defn generate_other_file() :: Dagger.Directory.t() do
    dag()
    |> Dagger.Client.directory()
    |> Dagger.Directory.with_new_file("other.txt", "hello from other generator")
  end

  defn hello() :: String.t() do
    "hello"
  end
end
