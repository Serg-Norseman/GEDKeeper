/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

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
        summary = BenchmarkRunner.Run<StrTrim>();
    }
}
