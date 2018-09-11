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
using Eto.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class MediaEditDlg : EditorDialog, IMediaEditDlg
    {
        private readonly MediaEditDlgController fController;

        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fSourcesList;

        public GEDCOMMultimediaRecord MediaRec
        {
            get { return fController.MediaRec; }
            set { fController.MediaRec = value; }
        }

        #region View Interface

        ISheetList IMediaEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        ISheetList IMediaEditDlg.SourcesList
        {
            get { return fSourcesList; }
        }

        IComboBoxHandler IMediaEditDlg.MediaType
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(cmbMediaType); }
        }

        IComboBoxHandler IMediaEditDlg.StoreType
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(cmbStoreType); }
        }

        ITextBoxHandler IMediaEditDlg.Name
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtName); }
        }

        ITextBoxHandler IMediaEditDlg.File
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtFile); }
        }

        IButtonHandler IMediaEditDlg.FileSelectButton
        {
            get { return fControlsManager.GetControlHandler<IButtonHandler>(btnFileSelect); }
        }

        #endregion

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try {
                fController.Cancel();
                CancelClickHandler(sender, e);
            } catch (Exception ex) {
                Logger.LogWrite("MediaEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void btnFileSelect_Click(object sender, EventArgs e)
        {
            fController.SelectFile();
        }

        private void btnView_Click(object sender, EventArgs e)
        {
            fController.View();
        }

        private void edName_TextChanged(object sender, EventArgs e)
        {
            Title = string.Format("{0} \"{1}\"", LangMan.LS(LSID.LSID_RPMultimedia), txtName.Text);
        }

        public MediaEditDlg()
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fNotesList = new GKSheetList(pageNotes);
            fSourcesList = new GKSheetList(pageSources);

            // SetLang()
            Title = LangMan.LS(LSID.LSID_RPMultimedia);
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

            fController = new MediaEditDlgController(this);
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);
            fController.Init(baseWin);

            fNotesList.ListModel = new NoteLinksListModel(fBase, fController.LocalUndoman);
            fSourcesList.ListModel = new SourceCitationsListModel(fBase, fController.LocalUndoman);
        }
    }
}
