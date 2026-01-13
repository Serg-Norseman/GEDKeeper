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
    public interface IMediaFileDlg : ICommonDialog
    {
        IComboBox MediaType { get; }
        IComboBox StoreType { get; }
        ITextBox Name { get; }
        ITextBox File { get; }
        IButton FileSelectButton { get; }
    }


    public interface IMediaFileEditDlg : IMediaFileDlg
    {
        GDMFileReferenceWithTitle FileRef { get; set; }
    }


    public interface IMediaEditDlg : ICommonDialog
    {
        GDMMultimediaRecord MultimediaRecord { get; set; }

        ISheetList FilesList { get; }
        ISheetList NotesList { get; }
        ISheetList SourcesList { get; }
        ISheetList UserRefList { get; }

        IComboBox MediaType { get; }
        IComboBox StoreType { get; }
        ITextBox Name { get; }
        ITextBox File { get; }
        IButton FileSelectButton { get; }
    }
}
