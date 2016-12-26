/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using GKUI.Controls;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class SourceEditDlg : EditorDialog
    {
        private readonly GKNotesSheet fNotesList;
        private readonly GKMediaSheet fMediaList;
        private readonly GKSheetList fRepositoriesList;

        private GEDCOMSourceRecord fSourceRecord;

        public GEDCOMSourceRecord SourceRecord
        {
            get { return this.fSourceRecord; }
            set { this.SetSourceRecord(value); }
        }

        private void SetSourceRecord(GEDCOMSourceRecord value)
        {
            this.fSourceRecord = value;
            
            this.txtShortTitle.Text = this.fSourceRecord.FiledByEntry;
            this.txtAuthor.Text = this.fSourceRecord.Originator.Text.Trim();
            this.txtTitle.Text = this.fSourceRecord.Title.Text.Trim();
            this.txtPublication.Text = this.fSourceRecord.Publication.Text.Trim();
            this.txtText.Text = this.fSourceRecord.Text.Text.Trim();

            this.fNotesList.DataList = this.fSourceRecord.Notes.GetEnumerator();
            this.fMediaList.DataList = this.fSourceRecord.MultimediaLinks.GetEnumerator();
            this.UpdateReposSheet();
            
            this.ActiveControl = this.txtShortTitle;
        }

        public SourceEditDlg(IBaseWindow baseWin) : base(baseWin)
        {
            this.InitializeComponent();

            this.btnAccept.Image = GKResources.iBtnAccept;
            this.btnCancel.Image = GKResources.iBtnCancel;

            this.fNotesList = new GKNotesSheet(this, this.pageNotes, this.fLocalUndoman);
            this.fMediaList = new GKMediaSheet(this, this.pageMultimedia, this.fLocalUndoman);

            this.fRepositoriesList = this.CreateReposSheet(this.pageRepositories);
            this.fRepositoriesList.SetControlName("fRepositoriesList"); // for purpose of tests

            // SetLang()
            this.Text = LangMan.LS(LSID.LSID_Source);
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.lblShortTitle.Text = LangMan.LS(LSID.LSID_ShortTitle);
            this.lblAuthor.Text = LangMan.LS(LSID.LSID_Author);
            this.lblTitle.Text = LangMan.LS(LSID.LSID_Title);
            this.lblPublication.Text = LangMan.LS(LSID.LSID_Publication);
            this.pageCommon.Text = LangMan.LS(LSID.LSID_Common);
            this.pageText.Text = LangMan.LS(LSID.LSID_Text);
            this.pageRepositories.Text = LangMan.LS(LSID.LSID_RPRepositories);
            this.pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            this.pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
        }

        private GKSheetList CreateReposSheet(Control owner)
        {
            GKSheetList sheet = new GKSheetList(owner);
            
            sheet.Columns_BeginUpdate();
            sheet.AddColumn(LangMan.LS(LSID.LSID_Repository), 300, false);
            sheet.Columns_EndUpdate();

            sheet.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbDelete, SheetButton.lbJump);
            sheet.OnModify += this.ModifyReposSheet;

            return sheet;
        }
        
        private void UpdateReposSheet()
        {
            try
            {
                this.fRepositoriesList.ClearItems();

                foreach (GEDCOMRepositoryCitation repCit in this.fSourceRecord.RepositoryCitations) {
                    GEDCOMRepositoryRecord rep = repCit.Value as GEDCOMRepositoryRecord;
                    if (rep == null) continue;

                    this.fRepositoriesList.AddItem(rep.RepositoryName, repCit);
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
                    GEDCOMRepositoryRecord rep = this.fBase.SelectRecord(GEDCOMRecordType.rtRepository, null) as GEDCOMRepositoryRecord;
                    if (rep != null) {
                        //this.fSourceRecord.AddRepository(rep);
                        this.fLocalUndoman.DoOrdinaryOperation(OperationType.otSourceRepositoryCitationAdd, this.fSourceRecord, rep);
                        result = true;
                    }
                    break;

                case RecordAction.raDelete:
                    if (cit != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachRepositoryQuery)) != DialogResult.No) {
                        //this.fSourceRecord.RepositoryCitations.Delete(cit);
                        this.fLocalUndoman.DoOrdinaryOperation(OperationType.otSourceRepositoryCitationRemove, this.fSourceRecord, cit.Value as GEDCOMRepositoryRecord);
                        result = true;
                    }
                    break;

                case RecordAction.raJump:
                    if (cit != null) {
                        this.AcceptChanges();
                        this.fBase.SelectRecordByXRef(cit.Value.XRef);
                        base.Close();
                    }
                    break;
            }

            if (result) this.UpdateReposSheet();
        }

        private void AcceptChanges()
        {
            this.fSourceRecord.FiledByEntry = this.txtShortTitle.Text;
            this.fSourceRecord.Originator.Clear();
            this.fSourceRecord.SetOriginatorArray(this.txtAuthor.Lines);
            this.fSourceRecord.Title.Clear();
            this.fSourceRecord.SetTitleArray(this.txtTitle.Lines);
            this.fSourceRecord.Publication.Clear();
            this.fSourceRecord.SetPublicationArray(this.txtPublication.Lines);
            this.fSourceRecord.Text.Clear();
            this.fSourceRecord.SetTextArray(this.txtText.Lines);

            base.CommitChanges();

            this.fBase.ChangeRecord(this.fSourceRecord);
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                this.AcceptChanges();
                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("SourceEditDlg.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try
            {
                base.RollbackChanges();
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("SourceEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void EditShortTitle_TextChanged(object sender, EventArgs e)
        {
            this.Text = string.Format("{0} \"{1}\"", LangMan.LS(LSID.LSID_Source), this.txtShortTitle.Text);
        }
    }
}
