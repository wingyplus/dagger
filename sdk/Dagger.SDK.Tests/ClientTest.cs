using Dagger.SDK;
using Dagger.SDK.GraphQL;

namespace Dagger.SDK.Tests;


public class ClientTest
{
    [Fact]
    public async void TestSimple()
    {
        var client = Dagger.Connect();
        var output = await client
            .Container()
            .From("debian")
            .WithExec(["echo", "hello"])
            .Stdout();

        Assert.Equal("hello\n", output);
    }

    [Fact]
    public async void TestOptionalArguments()
    {
        var client = Dagger.Connect();
        var env = await client
            .Container()
            .From("debian")
            .WithEnvVariable("A", "a")
            .WithEnvVariable("B", "b")
            .WithEnvVariable("C", "$A:$B", expand: true)
            .EnvVariable("C");

        Assert.Equal("a:b", env);
    }

    [Fact]
    public async void TestScalarIdSerialization()
    {
        var dag = Dagger.Connect();
        var cache = dag.CacheVolume("hello");
        var id = await cache.Id();
        Assert.True(id.Value.Length > 0);
    }
}
