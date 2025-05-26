defmodule B do
  use Dagger.Mod.Object, name: "B"

  defn c() :: String.t() do
    "Hello"
  end
end

defmodule A do
  use Dagger.Mod.Object, name: "A"

  defn b() :: B.t() do
    %B{}
  end

  defn c() :: integer() do
    1
  end
end
