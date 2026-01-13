/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design;

namespace GKCore.Charts
{
    public interface IChartLayout
    {
        void SetModel(ChartModel model);

        void RecalcChart();
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IChart : IScrollableContainer
    {
        IBaseWindow Base { get; set; }

        void CopySnapshot();
        void SaveSnapshot(string fileName);
    }
}
