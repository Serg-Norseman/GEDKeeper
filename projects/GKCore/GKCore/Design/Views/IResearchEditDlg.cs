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
    public interface IResearchEditDlg : ICommonDialog
    {
        GDMResearchRecord ResearchRecord { get; set; }

        ISheetList TasksList { get; }
        ISheetList CommunicationsList { get; }
        ISheetList GroupsList { get; }
        ISheetList NotesList { get; }

        ITextBox Name { get; }
        IComboBox Priority { get; }
        IComboBox Status { get; }
        IDateBox StartDate { get; }
        IDateBox StopDate { get; }
        INumericBox Percent { get; }
    }
}
