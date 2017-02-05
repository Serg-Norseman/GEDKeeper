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
using GKCore.Options;
using GKCore.Types;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class MediaEditDlg : EditorDialog
    {
        private bool fIsNew;
        private GEDCOMMultimediaRecord fMediaRec;

        private readonly GKNotesSheet fNotesList;
        private readonly GKSourcesSheet fSourcesList;

        public GEDCOMMultimediaRecord MediaRec
        {
            get { return this.fMediaRec; }
            set { this.SetMediaRec(value); }
        }

        private bool AcceptChanges()
        {
            GEDCOMFileReferenceWithTitle fileRef = this.fMediaRec.FileReferences[0];

            if (this.fIsNew)
            {
                MediaStoreType gst = (MediaStoreType)this.cmbStoreType.SelectedIndex;
                if ((gst == MediaStoreType.mstArchive || gst == MediaStoreType.mstStorage) && !this.fBase.Context.CheckBasePath())
                {
                    return false;
                }

                bool result = this.fBase.Context.MediaSave(fileRef, this.txtFile.Text, gst);

                if (!result) {
                    return false;
                }
            }

            fileRef.MediaType = (GEDCOMMediaType)this.cmbMediaType.SelectedIndex;
            fileRef.Title = this.txtName.Text;

            this.ControlsRefresh();

            base.CommitChanges();
            this.fBase.ChangeRecord(this.fMediaRec);

            return true;
        }

        private void ControlsRefresh()
        {
            GEDCOMFileReferenceWithTitle fileRef = this.fMediaRec.FileReferences[0];

            this.fIsNew = (fileRef.StringValue == "");

            this.txtName.Text = fileRef.Title;
            this.cmbMediaType.SelectedIndex = (int)fileRef.MediaType;
            this.txtFile.Text = fileRef.StringValue;

            if (this.fIsNew) {
                this.StoreTypesRefresh(true, MediaStoreType.mstReference);
            } else {
                string dummy = "";
                MediaStoreType gst = this.fBase.Context.GetStoreType(fileRef, ref dummy);
                this.StoreTypesRefresh((gst == MediaStoreType.mstArchive), gst);
            }

            this.btnFileSelect.Enabled = this.fIsNew;
            this.cmbStoreType.Enabled = this.fIsNew;

            this.fNotesList.DataList = this.fMediaRec.Notes.GetEnumerator();
            this.fSourcesList.DataList = this.fMediaRec.SourceCitations.GetEnumerator();
        }

        private void SetMediaRec(GEDCOMMultimediaRecord value)
        {
            this.fMediaRec = value;
            try
            {
                this.ControlsRefresh();
                this.ActiveControl = this.txtName;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("MediaEditDlg.SetMediaRec(): " + ex.Message);
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                base.DialogResult = this.AcceptChanges() ? DialogResult.OK : DialogResult.None;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("MediaEditDlg.btnAccept_Click(): " + ex.Message);
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
                this.fBase.Host.LogWrite("MediaEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void btnFileSelect_Click(object sender, EventArgs e)
        {
            string fileName = UIHelper.GetOpenFile("", "", LangMan.LS(LSID.LSID_AllFilter), 1, "");
            if (!string.IsNullOrEmpty(fileName))
            {
                if (GlobalOptions.Instance.RemovableMediaWarning && SysUtils.IsRemovableDrive(fileName)) {
                    if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_RemovableMediaWarningMessage)) == DialogResult.No) {
                        return;
                    }
                }

                this.txtFile.Text = fileName;
                bool canArc = GKUtils.FileCanBeArchived(fileName);
                this.StoreTypesRefresh(canArc, MediaStoreType.mstReference);
                this.cmbStoreType.Enabled = true;
            }
        }

        private void btnView_Click(object sender, EventArgs e)
        {
            this.AcceptChanges();
            this.fBase.ShowMedia(this.fMediaRec, true);
        }

        private void edName_TextChanged(object sender, EventArgs e)
        {
            this.Text = string.Format("{0} \"{1}\"", LangMan.LS(LSID.LSID_RPMultimedia), this.txtName.Text);
        }

        private void StoreTypesRefresh(bool allowArc, MediaStoreType select)
        {
            this.cmbStoreType.Items.Clear();
            this.cmbStoreType.Items.Add(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstReference].Name));
            this.cmbStoreType.Items.Add(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstStorage].Name));
            if (allowArc) {
                this.cmbStoreType.Items.Add(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstArchive].Name));
            }
            this.cmbStoreType.SelectedIndex = (int)select;
        }

        public MediaEditDlg(IBaseWindow baseWin) : base(baseWin)
        {
            this.InitializeComponent();

            this.btnAccept.Image = GKResources.iBtnAccept;
            this.btnCancel.Image = GKResources.iBtnCancel;

            for (GEDCOMMediaType mt = GEDCOMMediaType.mtUnknown; mt <= GEDCOMMediaType.mtLast; mt++)
            {
                this.cmbMediaType.Items.Add(LangMan.LS(GKData.MediaTypes[(int)mt]));
            }

            this.fNotesList = new GKNotesSheet(this, this.pageNotes, this.fLocalUndoman);
            this.fSourcesList = new GKSourcesSheet(this, this.pageSources, this.fLocalUndoman);

            // SetLang()
            this.Text = LangMan.LS(LSID.LSID_RPMultimedia);
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.pageCommon.Text = LangMan.LS(LSID.LSID_Common);
            this.pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            this.pageSources.Text = LangMan.LS(LSID.LSID_RPSources);
            this.lblName.Text = LangMan.LS(LSID.LSID_Title);
            this.lblType.Text = LangMan.LS(LSID.LSID_Type);
            this.lblStoreType.Text = LangMan.LS(LSID.LSID_StoreType);
            this.lblFile.Text = LangMan.LS(LSID.LSID_File);
            this.btnView.Text = LangMan.LS(LSID.LSID_View) + @"...";
        }
    }
}
