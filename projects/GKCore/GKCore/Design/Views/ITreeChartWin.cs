/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Charts;

namespace GKCore.Design.Views
{
    public interface ITreeChartWin : IChartWindow
    {
        ITreeChart TreeBox { get; }

        void GenChart(GDMIndividualRecord startPerson, TreeChartKind chartKind);
    }
}
