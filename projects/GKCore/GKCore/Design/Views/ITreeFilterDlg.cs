/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Charts;
using GKCore.Design.Controls;
using GKCore.Lists;

namespace GKCore.Design.Views
{
    public interface ITreeFilterDlg : ICommonDialog
    {
        ChartFilter Filter { get; set; }

        ISheetList PersonsList { get; }
        INumericBox YearNum { get; }
        IComboBox SourceCombo { get; }
    }
}
