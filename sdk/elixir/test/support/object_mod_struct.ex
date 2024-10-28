defmodule ObjectModStruct do
  use Dagger.Mod.Object, name: "ObjectModStruct"

  field(:name, String.t())
  field(:container, Dagger.Container.t())
  field(:directory, Dagger.Directory.t(), default_path: ".")
end
