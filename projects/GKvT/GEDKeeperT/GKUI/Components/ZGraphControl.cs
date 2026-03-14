/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using GKCore.Design.Controls;
using GKCore.Stats;
using Terminal.Gui;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ZGraphControl : GraphView, IGraphControl
    {

        public ZGraphControl()
        {
        }

        public void Activate()
        {
            SetFocus();
        }

        public new void Clear()
        {
        }

        public void PrepareArray(string title, string xAxis, string yAxis, ChartStyle style, bool excludeUnknowns, List<StatsItem> vals)
        {
        }
    }
}
