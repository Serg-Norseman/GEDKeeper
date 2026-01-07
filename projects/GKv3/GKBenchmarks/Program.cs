using BenchmarkDotNet.Reports;
using BenchmarkDotNet.Running;

namespace GKBenchmarks;

public class Program
{
    public static void Main(string[] args)
    {
        Summary summary;
        summary = BenchmarkRunner.Run<MediaFormatVal>();
        summary = BenchmarkRunner.Run<BinarySearch>();
    }
}
