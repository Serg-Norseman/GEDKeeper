// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using BenchmarkDotNet.Attributes;
using GDModel.Providers.GEDCOM;

namespace GKBenchmarks;

[Config(typeof(BenchmarkConfig))]
public class StrTrim
{
    private static char[] arr = "  FROM  ".ToCharArray();

    [Benchmark(Baseline = true)]
    public string GDMTrim_unsafe()
    {
        return GEDCOMUtils.Trim("  FROM  ");
    }

    [Benchmark]
    public string GDMTrim_str2array()
    {
        return Trim_str2arr("  FROM  ");
    }

    [Benchmark]
    public string GDMTrim_array()
    {
        return Trim_arr("  FROM  ", arr);
    }

    [Benchmark]
    public string SystemTrim()
    {
        return "  FROM  ".Trim();
    }

    // This implementation is 53% slower.
    private static string Trim_str2arr(string str)
    {
        if (string.IsNullOrEmpty(str)) return string.Empty;

        int strLen = str.Length;
        var strArr = str.ToCharArray();
        {
            int li = 0;
            int ri = strLen - 1;

            while (li < ri && strArr[li] <= ' ') li++;
            while (ri >= li && strArr[ri] <= ' ') ri--;
            int newLen = ri - li + 1;

            string result = (newLen == strLen) ? str : new string(strArr, li, newLen);
            return result;
        }
    }

    private static string Trim_arr(string str, char[] strArr)
    {
        if (string.IsNullOrEmpty(str)) return string.Empty;

        int strLen = str.Length;
        {
            int li = 0;
            int ri = strLen - 1;

            while (li < ri && strArr[li] <= ' ') li++;
            while (ri >= li && strArr[ri] <= ' ') ri--;
            int newLen = ri - li + 1;

            string result = (newLen == strLen) ? str : new string(strArr, li, newLen);
            return result;
        }
    }
}
