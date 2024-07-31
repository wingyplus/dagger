defmodule Dagger.Mod.ObjectTest do
  use ExUnit.Case, async: true

  defmodule A do
    use Dagger.Mod.Object

    @doc """
    Echo the name.
    """
    defn hello(name: :string) :: :string do
      "Hello, #{name}"
    end

    defn hello_with_lucky_number(name: :string, lucky_number: :integer) :: :string do
      "Hello, #{name}. You have a number #{lucky_number} as a lucky number!"
    end

    defn hello_boolean(ab: :boolean) :: :string do
      "Hello, #{ab}"
    end
  end

  describe "defn" do
    test "define a function" do
      assert function_exported?(A, :hello, 2)

      dag = nil
      assert A.hello(dag, "A") == "Hello, A"

      assert A.__info__(:attributes) |> Keyword.fetch!(:functions) == [
               hello_boolean: [args: [ab: [type: :boolean]], return: :string],
               hello_with_lucky_number: [
                 args: [name: [type: :string], lucky_number: [type: :integer]],
                 return: :string
               ],
               hello: [args: [name: [type: :string]], return: :string]
             ]
    end

    test "define arg as a module"
  end
end
