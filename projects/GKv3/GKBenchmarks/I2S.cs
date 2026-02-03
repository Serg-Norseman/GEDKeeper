/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Diagnostics;
using System.Runtime.CompilerServices;
using BenchmarkDotNet.Attributes;
using GKCore.Utilities;

namespace GKBenchmarks;

[Config(typeof(BenchmarkConfig))]
public class I2S
{
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static string I2Ssimp(int number, int totalWidth, bool ignore = false)
    {
        if (ignore) return string.Empty;

        string result;
        if (number > 0) {
            result = (totalWidth == 2) ? number.ToString("D2", null) : number.ToString().PadLeft(4, '_');
        } else {
            result = (totalWidth == 2) ? "__" : "____";
        }
        return result;
    }

    // 1.00
    [Benchmark(Baseline = true)]
    public string I2S_std()
    {
        string result = I2Ssimp(22, 4);
        Debug.Assert(result == "__22");

        result = I2Ssimp(12, 2);
        Debug.Assert(result == "12");

        result = I2Ssimp(1923, 4);
        Debug.Assert(result == "1923");

        return result;
    }

    [Benchmark]
    public string I2S_unsafe_fx()
    {
        string result = SysUtils.I2S(22, 4);
        Debug.Assert(result == "__22");

        result = SysUtils.I2S(12, 2);
        Debug.Assert(result == "12");

        result = SysUtils.I2S(1923, 4);
        Debug.Assert(result == "1923");

        return result;
    }
}
