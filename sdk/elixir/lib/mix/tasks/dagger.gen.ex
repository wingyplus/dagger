defmodule Mix.Tasks.Dagger.Gen do
  @moduledoc "Generate Dagger API."

  use Mix.Task

  def run(_) do
    Application.ensure_all_started(:dagger_ex)

    File.rm_rf!("lib/dagger/gen.ex")

    Mix.shell().info("Generating SDK...")

    dir = Path.join(["lib", "dagger", "gen"])

    Mix.shell().info("Writing to #{dir}")

    File.mkdir_p!(dir)

    _ =
      Dagger.Codegen.Generator.generate()
      |> Enum.map(&write_file!(&1, dir))

    Mix.shell().info("Formatting code")

    Mix.Task.run("format")
  end

  defp write_file!({filename, module}, dir) do
    filepath = Path.join([dir, filename])
    File.write!(filepath, prepend_comment(Macro.to_string(module)))
  end

  defp prepend_comment(module) do
    comment = """
    # This file generated by `mix dagger.gen`. Please DO NOT EDIT.
    """

    comment <> module
  end
end
