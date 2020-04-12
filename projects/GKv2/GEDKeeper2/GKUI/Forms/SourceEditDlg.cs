/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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

using System;
using System.Windows.Forms;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Views;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class SourceEditDlg : EditorDialog, ISourceEditDlg
    {
        private readonly SourceEditDlgController fController;

        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;
        private readonly GKSheetList fRepositoriesList;

        public GDMSourceRecord Model
        {
            get { return fController.Model; }
            set { fController.Model = value; }
        }

        #region View Interface

        ISheetList ISourceEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        ISheetList ISourceEditDlg.MediaList
        {
            get { return fMediaList; }
        }

        ISheetList ISourceEditDlg.RepositoriesList
        {
            get { return fRepositoriesList; }
        }

        ITextBoxHandler ISourceEditDlg.ShortTitle
        {
            get { return GetControlHandler<ITextBoxHandler>(txtShortTitle); }
        }

        ITextBoxHandler ISourceEditDlg.Author
        {
            get { return GetControlHandler<ITextBoxHandler>(txtAuthor); }
        }

        ITextBoxHandler ISourceEditDlg.Title
        {
            get { return GetControlHandler<ITextBoxHandler>(txtTitle); }
        }

        ITextBoxHandler ISourceEditDlg.Publication
        {
            get { return GetControlHandler<ITextBoxHandler>(txtPublication); }
        }

        ITextBoxHandler ISourceEditDlg.Text
        {
            get { return GetControlHandler<ITextBoxHandler>(txtText); }
        }

        #endregion

        public SourceEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fNotesList = new GKSheetList(pageNotes);
            fMediaList = new GKSheetList(pageMultimedia);

            fRepositoriesList = new GKSheetList(pageRepositories);
            fRepositoriesList.SetControlName("fRepositoriesList"); // for purpose of tests
            fRepositoriesList.OnModify += ModifyReposSheet;

            // SetLang()
            Text = LangMan.LS(LSID.LSID_Source);
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            lblShortTitle.Text = LangMan.LS(LSID.LSID_ShortTitle);
            lblAuthor.Text = LangMan.LS(LSID.LSID_Author);
            lblTitle.Text = LangMan.LS(LSID.LSID_Title);
            lblPublication.Text = LangMan.LS(LSID.LSID_Publication);
            pageCommon.Text = LangMan.LS(LSID.LSID_Common);
            pageText.Text = LangMan.LS(LSID.LSID_Text);
            pageRepositories.Text = LangMan.LS(LSID.LSID_RPRepositories);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);

            fController = new SourceEditDlgController(this);
            fController.Init(baseWin);

            fRepositoriesList.ListModel = new SourceRepositoriesSublistModel(baseWin, fController.LocalUndoman);
            fNotesList.ListModel = new NoteLinksListModel(baseWin, fController.LocalUndoman);
            fMediaList.ListModel = new MediaLinksListModel(baseWin, fController.LocalUndoman);
        }

        private void ModifyReposSheet(object sender, ModifyEventArgs eArgs)
        {
            GDMRepositoryCitation cit = eArgs.ItemData as GDMRepositoryCitation;
            if (eArgs.Action == RecordAction.raJump && cit != null) {
                fController.JumpToRecord(cit.Value);
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.OK : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try {
                fController.Cancel();
            } catch (Exception ex) {
                Logger.LogWrite("SourceEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void EditShortTitle_TextChanged(object sender, EventArgs e)
        {
            Text = string.Format("{0} \"{1}\"", LangMan.LS(LSID.LSID_Source), txtShortTitle.Text);
        }
    }
}
