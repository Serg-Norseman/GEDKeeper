/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class SourceEditDlg : EditorDialog, ISourceEditDlg
    {
        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;
        private readonly GKSheetList fRepositoriesList;

        private GEDCOMSourceRecord fSourceRecord;

        public GEDCOMSourceRecord SourceRecord
        {
            get { return fSourceRecord; }
            set { SetSourceRecord(value); }
        }

        private void SetSourceRecord(GEDCOMSourceRecord value)
        {
            fSourceRecord = value;
            
            txtShortTitle.Text = fSourceRecord.FiledByEntry;
            txtAuthor.Text = fSourceRecord.Originator.Text.Trim();
            txtTitle.Text = fSourceRecord.Title.Text.Trim();
            txtPublication.Text = fSourceRecord.Publication.Text.Trim();
            txtText.Text = fSourceRecord.Text.Text.Trim();

            fRepositoriesList.ListModel.DataOwner = fSourceRecord;
            fNotesList.ListModel.DataOwner = fSourceRecord;
            fMediaList.ListModel.DataOwner = fSourceRecord;
            
            ActiveControl = txtShortTitle;
        }

        public SourceEditDlg()
        {
            InitializeComponent();

            btnAccept.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;

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
        }

        private void ModifyReposSheet(object sender, ModifyEventArgs eArgs)
        {
            GEDCOMRepositoryCitation cit = eArgs.ItemData as GEDCOMRepositoryCitation;
            if (eArgs.Action == RecordAction.raJump && cit != null) {
                AcceptChanges();
                fBase.SelectRecordByXRef(cit.Value.XRef);
                Close();
            }
        }

        private void AcceptChanges()
        {
            fSourceRecord.FiledByEntry = txtShortTitle.Text;
            fSourceRecord.Originator.Clear();
            fSourceRecord.SetOriginatorArray(txtAuthor.Lines);
            fSourceRecord.Title.Clear();
            fSourceRecord.SetTitleArray(txtTitle.Lines);
            fSourceRecord.Publication.Clear();
            fSourceRecord.SetPublicationArray(txtPublication.Lines);
            fSourceRecord.Text.Clear();
            fSourceRecord.SetTextArray(txtText.Lines);

            CommitChanges();

            fBase.NotifyRecord(fSourceRecord, RecordAction.raEdit);
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                AcceptChanges();
                DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("SourceEditDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
            }
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try
            {
                RollbackChanges();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("SourceEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void EditShortTitle_TextChanged(object sender, EventArgs e)
        {
            Text = string.Format("{0} \"{1}\"", LangMan.LS(LSID.LSID_Source), txtShortTitle.Text);
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);

            fRepositoriesList.ListModel = new SourceRepositoriesSublistModel(fBase, fLocalUndoman);
            fNotesList.ListModel = new NoteLinksListModel(fBase, fLocalUndoman);
            fMediaList.ListModel = new MediaLinksListModel(fBase, fLocalUndoman);
        }
    }
}
