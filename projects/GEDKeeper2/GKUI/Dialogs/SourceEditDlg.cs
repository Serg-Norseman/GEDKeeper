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
using GKCore.Operations;
using GKCore.Types;
using GKUI.Engine;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class SourceEditDlg : EditorDialog
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

            fNotesList.ListModel.DataOwner = fSourceRecord;
            fMediaList.ListModel.DataOwner = fSourceRecord;

            UpdateReposSheet();
            
            ActiveControl = txtShortTitle;
        }

        public SourceEditDlg(IBaseWindow baseWin) : base(baseWin)
        {
            InitializeComponent();

            btnAccept.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;

            fNotesList = new GKSheetList(pageNotes, new GKNotesListModel(fBase, fLocalUndoman));

            fMediaList = new GKSheetList(pageMultimedia, new GKMediaListModel(fBase, fLocalUndoman));

            fRepositoriesList = CreateReposSheet(pageRepositories);
            fRepositoriesList.SetControlName("fRepositoriesList"); // for purpose of tests

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

        private GKSheetList CreateReposSheet(Control owner)
        {
            GKSheetList sheet = new GKSheetList(owner);

            sheet.AddColumn(LangMan.LS(LSID.LSID_Repository), 300, false);

            sheet.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbDelete, SheetButton.lbJump);
            sheet.OnModify += ModifyReposSheet;

            return sheet;
        }
        
        private void UpdateReposSheet()
        {
            try
            {
                fRepositoriesList.ClearItems();

                foreach (GEDCOMRepositoryCitation repCit in fSourceRecord.RepositoryCitations) {
                    GEDCOMRepositoryRecord rep = repCit.Value as GEDCOMRepositoryRecord;
                    if (rep == null) continue;

                    fRepositoriesList.AddItem(rep.RepositoryName, repCit);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("SourceEditDlg.UpdateReposSheet(): " + ex.Message);
            }
        }
        
        private void ModifyReposSheet(object sender, ModifyEventArgs eArgs)
        {
            bool result = false;

            GEDCOMRepositoryCitation cit = eArgs.ItemData as GEDCOMRepositoryCitation;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    GEDCOMRepositoryRecord rep = fBase.SelectRecord(GEDCOMRecordType.rtRepository, null) as GEDCOMRepositoryRecord;
                    if (rep != null) {
                        //this.fSourceRecord.AddRepository(rep);
                        fLocalUndoman.DoOrdinaryOperation(OperationType.otSourceRepositoryCitationAdd, fSourceRecord, rep);
                        result = true;
                    }
                    break;

                case RecordAction.raDelete:
                    if (cit != null && UIEngine.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachRepositoryQuery)) != false) {
                        //this.fSourceRecord.RepositoryCitations.Delete(cit);
                        fLocalUndoman.DoOrdinaryOperation(OperationType.otSourceRepositoryCitationRemove, fSourceRecord, cit.Value as GEDCOMRepositoryRecord);
                        result = true;
                    }
                    break;

                case RecordAction.raJump:
                    if (cit != null) {
                        AcceptChanges();
                        fBase.SelectRecordByXRef(cit.Value.XRef);
                        Close();
                    }
                    break;
            }

            if (result) UpdateReposSheet();
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

            fBase.ChangeRecord(fSourceRecord);
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
                fBase.Host.LogWrite("SourceEditDlg.btnAccept_Click(): " + ex.Message);
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
                fBase.Host.LogWrite("SourceEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void EditShortTitle_TextChanged(object sender, EventArgs e)
        {
            Text = string.Format("{0} \"{1}\"", LangMan.LS(LSID.LSID_Source), txtShortTitle.Text);
        }
    }
}
