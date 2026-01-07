using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Jobs;

namespace GKBenchmarks;

public class BenchmarkConfig : ManualConfig
{
    public BenchmarkConfig()
    {
        Add(Job.Default.WithWarmupCount(2).WithIterationCount(5));
    }
}
