package sdk

import (
	"context"

	"github.com/dagger/dagger/ci/mage/util"
	"github.com/magefile/mage/mg"
)

type DotNet mg.Namespace

var _ SDK = DotNet{}

// Lint lints the DotNet SDK
func (DotNet) Lint(ctx context.Context) error {
	return nil
}

// Test tests the DotNet SDK
func (DotNet) Test(ctx context.Context) error {
	return util.DaggerCall(ctx, "sdk", "dotnet", "test")
}

// Generate re-generates the SDK API
func (DotNet) Generate(ctx context.Context) error {
	return nil
}

// Publish publishes the DotNet SDK
func (DotNet) Publish(ctx context.Context, tag string) error {
	return nil
}

// Bump the DotNet SDK's Engine dependency
func (DotNet) Bump(ctx context.Context, engineVersion string) error {
	return nil
}
