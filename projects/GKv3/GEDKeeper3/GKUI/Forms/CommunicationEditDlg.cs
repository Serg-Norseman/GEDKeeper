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
    public sealed partial class CommunicationEditDlg : EditorDialog, ICommunicationEditDlg
    {
        private readonly CommunicationEditDlgController fController;

        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;

        public GEDCOMCommunicationRecord Communication
        {
            get { return fController.Communication; }
            set { fController.Communication = value; }
        }

        #region View Interface

        ISheetList ICommunicationEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        ISheetList ICommunicationEditDlg.MediaList
        {
            get { return fMediaList; }
        }

        ITextBoxHandler ICommunicationEditDlg.Corresponder
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtCorresponder); }
        }

        IComboBoxHandler ICommunicationEditDlg.CorrType
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(cmbCorrType); }
        }

        ITextBoxHandler ICommunicationEditDlg.Date
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtDate); }
        }

        IComboBoxHandler ICommunicationEditDlg.Dir
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(txtDir); }
        }

        ITextBoxHandler ICommunicationEditDlg.Name
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtName); }
        }

        #endregion

        private void btnAccept_Click(object sender, EventArgs e)
        {
            bool res = fController.Accept();
            if (res) {
                CommitChanges();
            }
            DialogResult = res ? DialogResult.Ok : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try {
                RollbackChanges();
                CancelClickHandler(sender, e);
            } catch (Exception ex) {
                Logger.LogWrite("CommunicationEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void btnPersonAdd_Click(object sender, EventArgs e)
        {
            fController.SetPerson();
        }

        public CommunicationEditDlg()
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
            btnPersonAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");

            for (GKCommunicationType ct = GKCommunicationType.ctCall; ct <= GKCommunicationType.ctLast; ct++) {
                cmbCorrType.Items.Add(LangMan.LS(GKData.CommunicationNames[(int)ct]));
            }

            fNotesList = new GKSheetList(pageNotes);
            fMediaList = new GKSheetList(pageMultimedia);

            // SetLang()
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Title = LangMan.LS(LSID.LSID_WinCommunicationEdit);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
            lblTheme.Text = LangMan.LS(LSID.LSID_Theme);
            lblCorresponder.Text = LangMan.LS(LSID.LSID_Corresponder);
            lblType.Text = LangMan.LS(LSID.LSID_Type);
            lblDate.Text = LangMan.LS(LSID.LSID_Date);

            btnPersonAdd.ToolTip = LangMan.LS(LSID.LSID_PersonAttachTip);

            txtDir.Items.Clear();
            txtDir.Items.AddRange(GKComboItem.Convert(new string[] {
                LangMan.LS(LSID.LSID_CD_1),
                LangMan.LS(LSID.LSID_CD_2)
            }));

            txtName.Focus();
            fController = new CommunicationEditDlgController(this);
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);
            fController.Init(baseWin);

            fNotesList.ListModel = new NoteLinksListModel(fBase, fLocalUndoman);
            fMediaList.ListModel = new MediaLinksListModel(fBase, fLocalUndoman);
        }
    }
}
