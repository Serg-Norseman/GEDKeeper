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
    public sealed partial class GroupEditDlg : EditorDialog, IGroupEditDlg
    {
        private readonly GroupEditDlgController fController;

        private readonly GKSheetList fMembersList;
        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;

        public GDMGroupRecord Group
        {
            get { return fController.Group; }
            set { fController.Group = value; }
        }

        #region View Interface

        ISheetList IGroupEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        ISheetList IGroupEditDlg.MediaList
        {
            get { return fMediaList; }
        }

        ISheetList IGroupEditDlg.MembersList
        {
            get { return fMembersList; }
        }

        ITextBoxHandler IGroupEditDlg.Name
        {
            get { return GetControlHandler<ITextBoxHandler>(edName); }
        }

        #endregion

        public GroupEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fMembersList = new GKSheetList(pageMembers);
            fMembersList.SetControlName("fMembersList"); // for purpose of tests
            fMembersList.OnModify += ModifyMembersSheet;

            fNotesList = new GKSheetList(pageNotes);
            fMediaList = new GKSheetList(pageMultimedia);

            // SetLang()
            Text = LangMan.LS(LSID.LSID_WinGroupEdit);
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            lblName.Text = LangMan.LS(LSID.LSID_Title);
            pageMembers.Text = LangMan.LS(LSID.LSID_Members);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);

            fController = new GroupEditDlgController(this);
            fController.Init(baseWin);

            fMembersList.ListModel = new GroupMembersSublistModel(baseWin, fController.LocalUndoman);
            fNotesList.ListModel = new NoteLinksListModel(baseWin, fController.LocalUndoman);
            fMediaList.ListModel = new MediaLinksListModel(baseWin, fController.LocalUndoman);
        }

        private void ModifyMembersSheet(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                fController.JumpToRecord(eArgs.ItemData as GDMIndividualRecord);
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
                Logger.LogWrite("GroupEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }
    }
}
