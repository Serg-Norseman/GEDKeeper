/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Design.Controls;
using GKCore.Lists;

namespace GKCore.Design.Views
{
    public interface ITaskEditDlg : ICommonDialog
    {
        GDMTaskRecord TaskRecord { get; set; }

        ISheetList NotesList { get; }
        IComboBox Priority { get; }
        IDateBox StartDate { get; }
        IDateBox StopDate { get; }
        IComboBox GoalType { get; }
        ITextBox Goal { get; }
        IButton GoalSelect { get; }
    }
}
