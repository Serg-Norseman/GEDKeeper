/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Design.Controls;

namespace GKCore.Design.Views
{
    public interface IRecMergeDlg : ICommonDialog
    {
        IHyperView View1 { get; }
        IHyperView View2 { get; }
        IButton SkipBtn { get; }
        IProgressBar ProgressBar { get; }
        ICheckBox IndistinctMatchingChk { get; }
        INumericBox NameAccuracyNum { get; }
        ICheckBox BirthYearChk { get; }
        INumericBox YearInaccuracyNum { get; }

        void SetRec1(GDMRecord value);
        void SetRec2(GDMRecord value);
    }
}
