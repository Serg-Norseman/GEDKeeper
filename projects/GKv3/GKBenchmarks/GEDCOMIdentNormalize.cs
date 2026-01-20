/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BenchmarkDotNet.Attributes;
using GDModel.Providers.GEDCOM;

namespace GKBenchmarks;

[Config(typeof(BenchmarkConfig))]
public class GEDCOMIdentNormalize
{
    [Benchmark(Baseline = true)]
    public string CurrentNormTest()
    {
        return CurrentNorm("  FROM  ");
    }

    [Benchmark]
    public string FastNormTest()
    {
        return GEDCOMUtils.FastNormalization("  FROM  ");
    }

    public static string CurrentNorm(string str)
    {
        return GEDCOMUtils.InvariantTextInfo.ToLower(str.Trim());
    }
}
