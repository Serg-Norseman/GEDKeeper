using BenchmarkDotNet.Attributes;
using BSLib;
using GDModel.Providers.GEDCOM;

namespace GKBenchmarks;

[Config(typeof(BenchmarkConfig))]
public class BinarySearch
{
    [Benchmark(Baseline = true)]
    public int BinarySearch_direct()
    {
        return GEDCOMUtils.BinarySearch(GEDCOMConsts.GEDCOMDateTypes, "FROM");
    }

    [Benchmark]
    public int BinarySearch_delegate()
    {
        return ArrayHelper.BinarySearch<string>(GEDCOMConsts.GEDCOMDateTypes, "FROM", string.CompareOrdinal);
    }
}
