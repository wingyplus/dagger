defmodule Dagger.GraphQL.QueryBuilderTest do
  use ExUnit.Case, async: true

  alias Dagger.GraphQL.QueryBuilder

  test "select/3" do
    query =
      QueryBuilder.query()
      |> QueryBuilder.select("container")

    assert query == [{"container", []}, {"query", []}]

    query =
      QueryBuilder.query()
      |> QueryBuilder.select("envVariable")
      |> QueryBuilder.select(["name", "value"])

    assert query == [["name", "value"], {"envVariable", []}, {"query", []}]
  end

  test "encode/1" do
    query = QueryBuilder.query()
    assert encode(query) == "query{}"

    query =
      QueryBuilder.query()
      |> QueryBuilder.select("container")

    assert encode(query) == "query{container}"

    query =
      QueryBuilder.query()
      |> QueryBuilder.select("envVariable")
      |> QueryBuilder.select(["name", "value"])

    assert encode(query) == "query{envVariable{name value}}"

    query =
      QueryBuilder.query()
      |> QueryBuilder.select("container")
      |> QueryBuilder.select("from", [{"image", "alpine"}])

    assert encode(query) == "query{container{from(image:\"alpine\")}}"

    query =
      QueryBuilder.query()
      |> QueryBuilder.select("container")
      |> QueryBuilder.select("withExec", [{"args", ["echo", "hello"]}])

    assert encode(query) == "query{container{withExec(args:[\"echo\",\"hello\"])}}"

    query =
      QueryBuilder.query()
      |> QueryBuilder.select("container")
      |> QueryBuilder.select("from", [{"image", "alpine"}])
      |> QueryBuilder.select("file", [{"path", "/etc/alpine-release"}])

    assert encode(query) ==
             "query{container{from(image:\"alpine\"){file(path:\"/etc/alpine-release\")}}}"

    query =
      QueryBuilder.query()
      |> QueryBuilder.select("container")
      |> QueryBuilder.select("pipeline", [{"name", "test"}, {"description", "test pipeline"}])

    assert encode(query) ==
             "query{container{pipeline(name:\"test\",description:\"test pipeline\")}}"

    query =
      QueryBuilder.query()
      |> QueryBuilder.select("container")
      |> QueryBuilder.select("build", [
        {"buildArg", %{name: "A", value: "B"}}
      ])

    assert encode(query) ==
             "query{container{build(buildArg:{name:\"A\",value:\"B\"})}}"

    query =
      QueryBuilder.query()
      |> QueryBuilder.select("container")
      |> QueryBuilder.select("pipeline", [
        {"name", "\n\t\""}
      ])

    assert encode(query) ==
             "query{container{pipeline(name:\"\\n\\t\\\"\")}}"
  end

  defp encode(query) do
    query
    |> QueryBuilder.encode()
    |> IO.iodata_to_binary()
  end
end
