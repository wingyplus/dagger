defmodule Dagger.Core.QueryBuilderTest do
  use ExUnit.Case, async: true

  alias Dagger.Core.QueryBuilder, as: QB

  defmodule A do
    defstruct [:name, :value]
  end

  test "query" do
    query =
      []
      |> QB.select("core")
      |> QB.select("image", ref: "alpine")
      |> QB.select("file", path: "/etc/alpine-release")
      |> build()
      |> format()

    assert query == """
           query {
             core {
               image(ref: \"alpine\") {
                 file(path: \"\\/etc\\/alpine-release\")
               }
             }
           }
           """
  end

  test "multi fields" do
    query =
      []
      |> QB.select("core")
      |> QB.select_many(["name", "value"])
      |> build()
      |> format()

    assert query == """
           query {
             core {
               name
               value
             }
           }
           """
  end

  test "string arg" do
    query =
      []
      |> QB.select("a", arg: "value")
      |> build()
      |> format()

    assert query == """
           query {
             a(arg: \"value\")
           }
           """

    query =
      []
      |> QB.select("a", arg: :value)
      |> build()
      |> format()

    assert query == """
           query {
             a(arg: \"value\")
           }
           """

    query =
      []
      |> QB.select("a", arg: "\n\t\"")
      |> build()

    assert query == "query{a(arg:\"\\n\\t\\\"\")}"
  end

  test "boolean arg" do
    query =
      []
      |> QB.select("a", arg: true)
      |> build()
      |> format()

    assert query == """
           query {
             a(arg: true)
           }
           """
  end

  test "object arg" do
    expected = """
    query {
      a(arg: { name: \"a\", value: \"b\" })
    }
    """

    query =
      []
      |> QB.select("a", arg: %{"name" => "a", "value" => "b"})
      |> build()
      |> format()

    assert query == expected

    query =
      []
      |> QB.select("a", arg: %{name: "a", value: "b"})
      |> build()
      |> format()

    assert query == expected

    query =
      []
      |> QB.select("a", arg: %A{name: "a", value: "b"})
      |> build()
      |> format()

    assert query == expected
  end

  test "array arg" do
    query =
      []
      |> QB.select("a", arg: [])
      |> build()
      |> format()

    assert query == """
           query {
             a(arg: [])
           }
           """

    query =
      []
      |> QB.select("a", arg: ["value"])
      |> build()
      |> format()

    assert query == """
           query {
             a(arg: ["value"])
           }
           """

    query =
      []
      |> QB.select("a", arg: ["value", "value2"])
      |> build()
      |> format()

    assert query == """
           query {
             a(arg: ["value", "value2"])
           }
           """

    query =
      []
      |> QB.select("a", arg: [%{name: "foo"}])
      |> build()
      |> format()

    assert query == """
           query {
             a(arg: [{ name: "foo" }])
           }
           """
  end

  test "path" do
    path =
      []
      |> QB.select("a")
      |> QB.select("b")
      |> QB.select("c")
      |> QB.path()

    assert path == ["a", "b", "c"]

    path =
      []
      |> QB.select("a")
      |> QB.select("b")
      |> QB.select("c")
      |> QB.select_many(["d", "e"])
      |> QB.path()

    assert path == ["a", "b", "c"]
  end

  defp build(query) do
    query
    |> QB.build()
    |> IO.iodata_to_binary()
  end

  defp format(query) do
    Absinthe.Formatter.format(query)
  end
end

defmodule Dagger.Core.QueryBuilder.SelectionTest do
  use ExUnit.Case, async: true

  alias Dagger.Core.QueryBuilder.Selection

  test "query" do
    root =
      Selection.query()
      |> Selection.select("core")
      |> Selection.select("image")
      |> Selection.arg("ref", "alpine")
      |> Selection.select("file")
      |> Selection.arg("path", "/etc/alpine-release")

    assert Selection.build(root) ==
             "query{core{image(ref:\"alpine\"){file(path:\"/etc/alpine-release\")}}}"
  end

  test "alias" do
    root =
      Selection.query()
      |> Selection.select("core")
      |> Selection.select("image")
      |> Selection.arg("ref", "alpine")
      |> Selection.select_with_alias("foo", "file")
      |> Selection.arg("path", "/etc/alpine-release")

    assert Selection.build(root) ==
             "query{core{image(ref:\"alpine\"){foo:file(path:\"/etc/alpine-release\")}}}"
  end

  test "select multi fields" do
    root =
      Selection.query()
      |> Selection.select("core")
      |> Selection.select("name value")

    assert Selection.build(root) == "query{core{name value}}"
  end

  test "args" do
    root =
      Selection.query()
      |> Selection.select("a")
      |> Selection.arg("arg", "b")
      |> Selection.arg("arg1", "c")

    assert Selection.build(root) == "query{a(arg:\"b\",arg1:\"c\")}"
  end

  test "arg collision" do
    root =
      Selection.query()
      |> Selection.select("a")
      |> Selection.arg("arg", "one")
      |> Selection.select("b")
      |> Selection.arg("arg", "two")

    assert Selection.build(root) == "query{a(arg:\"one\"){b(arg:\"two\")}}"
  end

  test "array args" do
    root =
      Selection.query()
      |> Selection.select("a")
      |> Selection.arg("arg", [])

    assert Selection.build(root) == "query{a(arg:[])}"

    root =
      Selection.query()
      |> Selection.select("a")
      |> Selection.arg("arg", ["value"])

    assert Selection.build(root) == "query{a(arg:[\"value\"])}"

    root =
      Selection.query()
      |> Selection.select("a")
      |> Selection.arg("arg", ["value", "value2"])

    assert Selection.build(root) == "query{a(arg:[\"value\",\"value2\"])}"

    root =
      Selection.query()
      |> Selection.select("a")
      |> Selection.arg("arg", [%{"name" => "foo"}])

    assert Selection.build(root) == "query{a(arg:[{name:\"foo\"}])}"
  end

  test "object args" do
    root =
      Selection.query()
      |> Selection.select("a")
      |> Selection.arg("arg", %{"name" => "a", "value" => "b"})

    assert Selection.build(root) == "query{a(arg:{name:\"a\",value:\"b\"})}"
  end

  test "string arg escape" do
    root =
      Selection.query()
      |> Selection.select("a")
      |> Selection.arg("arg", "\n\t\"")

    assert Selection.build(root) == "query{a(arg:\"\\n\\t\\\"\")}"
  end

  test "boolean arg" do
    root =
      Selection.query()
      |> Selection.select("a")
      |> Selection.arg("arg", true)

    assert Selection.build(root) == "query{a(arg:true)}"
  end
end
