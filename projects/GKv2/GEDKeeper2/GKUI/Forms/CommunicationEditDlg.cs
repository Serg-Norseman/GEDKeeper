/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class CommunicationEditDlg : EditorDialog, ICommunicationEditDlg
    {
        private readonly CommunicationEditDlgController fController;

        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;

        public GDMCommunicationRecord Communication
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

        ITextBox ICommunicationEditDlg.Corresponder
        {
            get { return GetControlHandler<ITextBox>(txtCorresponder); }
        }

        IComboBox ICommunicationEditDlg.CorrType
        {
            get { return GetControlHandler<IComboBox>(cmbCorrType); }
        }

        IDateBox ICommunicationEditDlg.Date
        {
            get { return GetControlHandler<IDateBox>(txtDate); }
        }

        IComboBox ICommunicationEditDlg.Dir
        {
            get { return GetControlHandler<IComboBox>(txtDir); }
        }

        ITextBox ICommunicationEditDlg.Name
        {
            get { return GetControlHandler<ITextBox>(txtName); }
        }

        #endregion

        public CommunicationEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
            btnPersonAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");

            fNotesList = new GKSheetList(pageNotes);
            fMediaList = new GKSheetList(pageMultimedia);

            fController = new CommunicationEditDlgController(this);
            fController.Init(baseWin);

            fNotesList.ListModel = new NoteLinksListModel(baseWin, fController.LocalUndoman);
            fMediaList.ListModel = new MediaLinksListModel(baseWin, fController.LocalUndoman);
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.OK : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Cancel() ? DialogResult.Cancel : DialogResult.None;
        }

        protected override void OnFormClosing(FormClosingEventArgs e)
        {
            base.OnFormClosing(e);
            e.Cancel = fController.CheckChangesPersistence();
        }

        private void btnPersonAdd_Click(object sender, EventArgs e)
        {
            fController.SetPerson();
        }
    }
}
