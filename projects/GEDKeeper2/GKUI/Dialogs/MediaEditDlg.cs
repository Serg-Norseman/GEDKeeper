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
using GKCore.Options;
using GKCore.Types;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class MediaEditDlg : EditorDialog, IMediaEditDlg
    {
        private bool fIsNew;
        private GEDCOMMultimediaRecord fMediaRec;

        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fSourcesList;

        public GEDCOMMultimediaRecord MediaRec
        {
            get { return fMediaRec; }
            set { SetMediaRec(value); }
        }

        private bool AcceptChanges()
        {
            GEDCOMFileReferenceWithTitle fileRef = fMediaRec.FileReferences[0];

            if (fIsNew)
            {
                GKComboItem item = (GKComboItem)cmbStoreType.SelectedItem;
                MediaStoreType gst = (MediaStoreType)item.Tag;

                if ((gst == MediaStoreType.mstArchive || gst == MediaStoreType.mstStorage) && !fBase.Context.CheckBasePath())
                {
                    return false;
                }

                bool result = fBase.Context.MediaSave(fileRef, txtFile.Text, gst);

                if (!result) {
                    return false;
                }
            }

            fileRef.MediaType = (GEDCOMMediaType)cmbMediaType.SelectedIndex;
            fileRef.Title = txtName.Text;

            ControlsRefresh();

            CommitChanges();
            fBase.NotifyRecord(fMediaRec, RecordAction.raEdit);

            return true;
        }

        private void ControlsRefresh()
        {
            GEDCOMFileReferenceWithTitle fileRef = fMediaRec.FileReferences[0];

            fIsNew = (fileRef.StringValue == "");

            txtName.Text = fileRef.Title;
            cmbMediaType.SelectedIndex = (int)fileRef.MediaType;
            txtFile.Text = fileRef.StringValue;

            if (fIsNew) {
                RefreshStoreTypes(GlobalOptions.Instance.AllowMediaStoreReferences, true, MediaStoreType.mstReference);
            } else {
                MediaStore mediaStore = fBase.Context.GetStoreType(fileRef);
                RefreshStoreTypes((mediaStore.StoreType == MediaStoreType.mstReference),
                                  (mediaStore.StoreType == MediaStoreType.mstArchive), mediaStore.StoreType);
            }

            btnFileSelect.Enabled = fIsNew;
            cmbStoreType.Enabled = fIsNew;

            fNotesList.UpdateSheet();
            fSourcesList.UpdateSheet();
        }

        private void SetMediaRec(GEDCOMMultimediaRecord value)
        {
            fMediaRec = value;
            try
            {
                fNotesList.ListModel.DataOwner = fMediaRec;
                fSourcesList.ListModel.DataOwner = fMediaRec;

                ControlsRefresh();
                ActiveControl = txtName;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("MediaEditDlg.SetMediaRec(): " + ex.Message);
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                DialogResult = AcceptChanges() ? DialogResult.OK : DialogResult.None;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("MediaEditDlg.btnAccept_Click(): " + ex.Message);
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
                Logger.LogWrite("MediaEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void btnFileSelect_Click(object sender, EventArgs e)
        {
            string fileName = AppHost.StdDialogs.GetOpenFile("", "", LangMan.LS(LSID.LSID_AllFilter), 1, "");
            if (string.IsNullOrEmpty(fileName)) return;

            if (GlobalOptions.Instance.RemovableMediaWarning && SysUtils.IsRemovableDrive(fileName)) {
                if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_RemovableMediaWarningMessage)) == false) {
                    return;
                }
            }

            txtFile.Text = fileName;
            bool canArc = GKUtils.FileCanBeArchived(fileName);
            RefreshStoreTypes(GlobalOptions.Instance.AllowMediaStoreReferences, canArc, MediaStoreType.mstReference);
            cmbStoreType.Enabled = true;
        }

        private void btnView_Click(object sender, EventArgs e)
        {
            if (fIsNew) {
                AcceptChanges();
            }

            fBase.ShowMedia(fMediaRec, true);
        }

        private void edName_TextChanged(object sender, EventArgs e)
        {
            Text = string.Format("{0} \"{1}\"", LangMan.LS(LSID.LSID_RPMultimedia), txtName.Text);
        }

        private void RefreshStoreTypes(bool allowRef, bool allowArc, MediaStoreType selectType)
        {
            cmbStoreType.Items.Clear();

            if (allowRef) {
                cmbStoreType.Items.Add(
                    new GKComboItem(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstReference].Name),
                                    MediaStoreType.mstReference));
            }

            cmbStoreType.Items.Add(
                new GKComboItem(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstStorage].Name),
                                MediaStoreType.mstStorage));

            if (allowArc) {
                cmbStoreType.Items.Add(
                    new GKComboItem(LangMan.LS(GKData.GKStoreTypes[(int)MediaStoreType.mstArchive].Name),
                                    MediaStoreType.mstArchive));
            }

            UIHelper.SelectComboItem(cmbStoreType, selectType, true);
        }

        public MediaEditDlg()
        {
            InitializeComponent();

            btnAccept.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;

            for (GEDCOMMediaType mt = GEDCOMMediaType.mtUnknown; mt <= GEDCOMMediaType.mtLast; mt++)
            {
                cmbMediaType.Items.Add(LangMan.LS(GKData.MediaTypes[(int)mt]));
            }

            fNotesList = new GKSheetList(pageNotes);
            fSourcesList = new GKSheetList(pageSources);

            // SetLang()
            Text = LangMan.LS(LSID.LSID_RPMultimedia);
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            pageCommon.Text = LangMan.LS(LSID.LSID_Common);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            pageSources.Text = LangMan.LS(LSID.LSID_RPSources);
            lblName.Text = LangMan.LS(LSID.LSID_Title);
            lblType.Text = LangMan.LS(LSID.LSID_Type);
            lblStoreType.Text = LangMan.LS(LSID.LSID_StoreType);
            lblFile.Text = LangMan.LS(LSID.LSID_File);
            btnView.Text = LangMan.LS(LSID.LSID_View) + @"...";
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);

            fNotesList.ListModel = new NoteLinksListModel(fBase, fLocalUndoman);
            fSourcesList.ListModel = new SourceCitationsListModel(fBase, fLocalUndoman);
        }
    }
}
