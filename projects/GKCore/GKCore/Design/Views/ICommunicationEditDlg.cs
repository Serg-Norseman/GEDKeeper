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
    public interface ICommunicationEditDlg : ICommonDialog
    {
        GDMCommunicationRecord CommunicationRecord { get; set; }

        ITextBox Corresponder { get; }
        IComboBox CorrType { get; }
        IDateBox Date { get; }
        IComboBox Dir { get; }
        ITextBox Name { get; }

        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
    }
}
