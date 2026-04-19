alias Dagger.Core.QueryBuilder, as: QB

# Shallow selection without arguments.
shallow = fn ->
  QB.query()
  |> QB.select("container")
  |> QB.build()
end

# Two-level selection with a single scalar argument.
simple_with_arg = fn ->
  QB.query()
  |> QB.select("container")
  |> QB.select("withExposedPort")
  |> QB.put_arg("protocol", :TCP)
  |> QB.build()
end

# Deep chain roughly mirroring a realistic container pipeline.
deep_chain = fn ->
  QB.query()
  |> QB.select("container")
  |> QB.select("from")
  |> QB.put_arg("address", "alpine:3.20")
  |> QB.select("withEnvVariable")
  |> QB.put_arg("name", "FOO")
  |> QB.put_arg("value", "bar")
  |> QB.select("withWorkdir")
  |> QB.put_arg("path", "/app")
  |> QB.select("withExec")
  |> QB.put_arg("args", ~w(sh -c echo\ hello))
  |> QB.select("withExposedPort")
  |> QB.put_arg("port", 8080)
  |> QB.put_arg("protocol", :TCP)
  |> QB.select("stdout")
  |> QB.build()
end

# Selection carrying a list of nested maps — exercises encode_value for
# collections and structs.
list_of_maps = fn ->
  build_args = [
    %{name: "VERSION", value: "1.0.0"},
    %{name: "COMMIT", value: "abcdef"},
    %{name: "TARGET", value: "linux/amd64"}
  ]

  QB.query()
  |> QB.select("container")
  |> QB.select("build")
  |> QB.put_arg("context", "./src")
  |> QB.put_arg("buildArgs", build_args)
  |> QB.build()
end

# String argument with characters that trigger the escape path.
escape_heavy = fn ->
  value = ~s(line1\nline2\t"quoted"\\backslash)

  QB.query()
  |> QB.select("container")
  |> QB.select("withNewFile")
  |> QB.put_arg("path", "/tmp/data")
  |> QB.put_arg("contents", value)
  |> QB.build()
end

# maybe_put_arg with all optional arguments nil — the common codegen path.
maybe_put_arg_nil = fn ->
  QB.query()
  |> QB.select("container")
  |> QB.select("asService")
  |> QB.maybe_put_arg("args", nil)
  |> QB.maybe_put_arg("useEntrypoint", nil)
  |> QB.maybe_put_arg("experimentalPrivilegedNesting", nil)
  |> QB.maybe_put_arg("insecureRootCapabilities", nil)
  |> QB.maybe_put_arg("expand", nil)
  |> QB.maybe_put_arg("noInit", nil)
  |> QB.build()
end

Benchee.run(
  %{
    "shallow" => shallow,
    "simple_with_arg" => simple_with_arg,
    "deep_chain" => deep_chain,
    "list_of_maps" => list_of_maps,
    "escape_heavy" => escape_heavy,
    "maybe_put_arg_nil" => maybe_put_arg_nil
  },
  time: 5,
  warmup: 2,
  memory_time: 2,
  reduction_time: 2,
  print: [fast_warning: false]
)
