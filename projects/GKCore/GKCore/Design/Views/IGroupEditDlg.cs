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
    public interface IGroupEditDlg : ICommonDialog
    {
        GDMGroupRecord GroupRecord { get; set; }

        ITextBox Name { get; }

        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
        ISheetList MembersList { get; }
    }
}
