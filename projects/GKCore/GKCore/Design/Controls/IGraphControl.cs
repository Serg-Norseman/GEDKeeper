/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using GKCore.Stats;

namespace GKCore.Design.Controls
{
    public enum ChartStyle
    {
        Bar,
        Point,
        ClusterBar
    }

    /// <summary>
    /// 
    /// </summary>
    public interface IGraphControl : IBaseControl
    {
        void Clear();
        void PrepareArray(string title, string xAxis, string yAxis, ChartStyle style, bool excludeUnknowns, List<StatsItem> vals);
    }
}
