defmodule A.B do
  use Dagger.Mod.Object, name: "B"

  object do
  end

  defn c() :: String.t() do
    "Hello"
  end
end

defmodule A do
  use Dagger.Mod.Object, name: "A"

  defn b() :: B.t() do
    %A.B{}
  end

  defn c() :: integer() do
    1
  end
end
