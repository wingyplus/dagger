package main

import "context"

const (
	dotnetSDKPath = "sdk/dotnet"
)

type DotNetSDK struct {
	Dagger *Dagger
}

func (t DotNetSDK) Test(ctx context.Context) error {
	installer, err := t.Dagger.installer(ctx, "sdk-dotnet-test")
	if err != nil {
		return err
	}

	_, err = t.dotNetBase().
		With(installer).
		WithExec([]string{"dagger", "run", "dotnet", "test"}).
		Sync(ctx)
	return err
}

func (t DotNetSDK) dotNetBase() *Container {
	src := t.Dagger.Source.Directory(dotnetSDKPath)
	mountPath := "/" + dotnetSDKPath
	return dag.Container().
		From("mcr.microsoft.com/dotnet/sdk:8.0").
		WithWorkdir(mountPath).
		WithMountedDirectory(mountPath, src).
		WithExec([]string{"dotnet", "restore"})
}
