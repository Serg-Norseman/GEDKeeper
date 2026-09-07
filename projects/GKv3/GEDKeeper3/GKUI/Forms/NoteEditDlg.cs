/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class NoteEditDlg : CommonDialog<INoteEdit, NoteEditDlgController>, INoteEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private RichTextArea txtNote;
        private TabControl tabsData;
        private GKSheetList fSourcesList;
        private GKSheetList fUserRefList;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public GDMNoteRecord NoteRecord
        {
            get { return fController.NoteRecord; }
            set { fController.NoteRecord = value; }
        }

        #region View Interface

        ITextBox INoteEdit.Note
        {
            get { return GetControlHandler<ITextBox>(txtNote); }
        }

        ISheetList INoteEdit.SourcesList
        {
            get { return fSourcesList; }
        }

        ISheetList INoteEdit.UserRefList
        {
            get { return fUserRefList; }
        }

        #endregion

        public NoteEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            tabsData.SelectedIndexChanged += tabControl_SelectedIndexChanged;

            fController = new NoteEditDlgController(this);
            fController.Init(baseWin);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                tabsData.SelectedIndexChanged -= tabControl_SelectedIndexChanged;
            }
            base.Dispose(disposing);
        }
    }
}
