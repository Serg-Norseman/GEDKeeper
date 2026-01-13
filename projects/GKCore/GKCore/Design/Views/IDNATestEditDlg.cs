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
    public interface IDNATestEditDlg : ICommonDialog
    {
        GDMDNATest DNATest { get; set; }

        ITextBox TestName { get; }
        IDateBox Date { get; }
        IComboBox Agency { get; }

        IComboBox MHaplogroup { get; }
        IComboBox YHaplogroup { get; }

        ISheetList NotesList { get; }
        ISheetList MediaList { get; }

        IComboBox StoreType { get; }
        ITextBox File { get; }
        IButton FileSelectButton { get; }
        IComboBox FileFormat { get; }

        IComboBox Restriction { get; }
    }
}
