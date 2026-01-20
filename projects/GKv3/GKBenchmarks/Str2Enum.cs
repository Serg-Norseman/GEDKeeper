using BenchmarkDotNet.Attributes;
using GDModel;
using GDModel.Providers.GEDCOM;

namespace GKBenchmarks;

/// <summary>
/// 
/// </summary>
[MemoryDiagnoser]
[SimpleJob]
public class Str2Enum
{
    private readonly string[] _communicationTypes = new string[]
    {
        "call", "email", "fax", "letter", "tape", "visit"
    };

    public static void Assert(bool condition)
    {
        if (!condition)
            throw new ArgumentException("not equal");
    }

    [Benchmark(Baseline = true)]
    public GDMCommunicationType BinarySearch()
    {
        var result = GEDCOMUtils.Str2Enum("visit", _communicationTypes, GDMCommunicationType.ctCall);
        Assert(result == GDMCommunicationType.ctVisit);
        return result;
    }

    [Benchmark]
    public GDMCommunicationType IfThenElse()
    {
        var result = Str2Enum_IfThenElse("visit", GDMCommunicationType.ctCall);
        Assert(result == GDMCommunicationType.ctVisit);
        return result;
    }

    [Benchmark]
    public GDMCommunicationType LoopSearch()
    {
        var result = Str2Enum_LoopSearch("visit", _communicationTypes, GDMCommunicationType.ctCall);
        Assert(result == GDMCommunicationType.ctVisit);
        return result;
    }

    [Benchmark]
    public GDMCommunicationType Dictionary()
    {
        var result = Str2Enum_Dictionary("visit", _communicationTypes, GDMCommunicationType.ctCall);
        Assert(result == GDMCommunicationType.ctVisit);
        return result;
    }


    public static GDMCommunicationType Str2Enum_IfThenElse(string val, GDMCommunicationType defVal, bool normalize = true)
    {
        if (string.IsNullOrEmpty(val)) return defVal;

        if (normalize) {
            val = GEDCOMUtils.FastNormalization(val);
        }

        if (string.CompareOrdinal(val, "call") == 0) {
            return GDMCommunicationType.ctCall;
        } else if (string.CompareOrdinal(val, "email") == 0) {
            return GDMCommunicationType.ctEMail;
        } else if (string.CompareOrdinal(val, "fax") == 0) {
            return GDMCommunicationType.ctFax;
        } else if (string.CompareOrdinal(val, "letter") == 0) {
            return GDMCommunicationType.ctLetter;
        } else if (string.CompareOrdinal(val, "tape") == 0) {
            return GDMCommunicationType.ctTape;
        } else if (string.CompareOrdinal(val, "visit") == 0) {
            return GDMCommunicationType.ctVisit;
        } else {
            return defVal;
        }
    }

    public static T Str2Enum_LoopSearch<T>(string val, string[] values, T defVal, bool normalize = true)
    {
        if (string.IsNullOrEmpty(val)) return defVal;

        if (normalize) {
            val = GEDCOMUtils.FastNormalization(val);
        }

        // Loop search approach
        for (int i = 0; i < values.Length; i++) {
            if (string.CompareOrdinal(values[i], val) == 0) {
                return (T)((IConvertible)i);
            }
        }

        return defVal;
    }

    private static Dictionary<string, int> lookupCache = null;

    public static T Str2Enum_Dictionary<T>(string val, string[] values, T defVal, bool normalize = true)
    {
        if (string.IsNullOrEmpty(val)) return defVal;

        if (normalize) {
            val = GEDCOMUtils.FastNormalization(val);
        }

        // Dictionary lookup approach
        // Build dictionary once (should be cached in real implementation)
        if (lookupCache == null) {
            lookupCache = new Dictionary<string, int>(StringComparer.Ordinal);
            for (int i = 0; i < values.Length; i++) {
                lookupCache[values[i]] = i;
            }
        }

        if (lookupCache.TryGetValue(val, out int index)) {
            return (T)((IConvertible)index);
        }

        return defVal;
    }
}
