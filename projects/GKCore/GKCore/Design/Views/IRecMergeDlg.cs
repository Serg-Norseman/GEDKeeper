/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
