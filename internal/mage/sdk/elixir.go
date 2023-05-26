package sdk

import (
	"context"
	"fmt"
	"os"

	"dagger.io/dagger"
	"github.com/dagger/dagger/internal/mage/util"
	"github.com/magefile/mage/mg"
)

var _ SDK = Elixir{}

type Elixir mg.Namespace

func (Elixir) Lint(ctx context.Context) error {
	return nil
}

func (Elixir) Test(ctx context.Context) error {
	c, err := dagger.Connect(ctx, dagger.WithLogOutput(os.Stderr))
	if err != nil {
		return err
	}
	defer c.Close()

	c = c.Pipeline("sdk").Pipeline("go").Pipeline("test")

	devEngine, endpoint, err := util.CIDevEngineContainerAndEndpoint(
		ctx,
		c.Pipeline("dev-engine"),
		util.DevEngineOpts{Name: "sdk-elixir-test"},
	)
	if err != nil {
		return err
	}

	cliBinPath := "/.dagger-cli"

	_, err = elixirBase(c).
		WithServiceBinding("dagger-engine", devEngine).
		WithEnvVariable("_EXPERIMENTAL_DAGGER_RUNNER_HOST", endpoint).
		WithMountedFile(cliBinPath, util.DaggerBinary(c)).
		WithEnvVariable("_EXPERIMENTAL_DAGGER_CLI_BIN", cliBinPath).
		WithExec([]string{"mix", "test"}).
		Sync(ctx)
	if err != nil {
		return err
	}
	return nil
}

func (Elixir) Generate(ctx context.Context) error {
	return nil
}
func (Elixir) Publish(ctx context.Context, tag string) error {
	return nil
}

func (Elixir) Bump(ctx context.Context, engineVersion string) error {
	return nil
}

func elixirBase(c *dagger.Client) *dagger.Container {
	const appDir = "sdk/elixir"
	src := c.Directory().WithDirectory("/", util.Repository(c).Directory(appDir))

	mountPath := fmt.Sprintf("/%s", appDir)

	return c.Container().
		From("hexpm/elixir:1.14.4-erlang-25.3-debian-buster-20230227-slim").
		WithWorkdir(mountPath).
		WithDirectory(mountPath, src).
		WithExec([]string{"mix", "local.hex", "--force"}).
		WithExec([]string{"mix", "local.rebar", "--force"}).
		WithExec([]string{"mix", "deps.get"})
}
