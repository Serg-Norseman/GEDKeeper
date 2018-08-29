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
using GKCore.Types;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class GroupEditDlg : EditorDialog, IGroupEditDlg
    {
        private readonly GroupEditDlgController fController;

        private readonly GKSheetList fMembersList;
        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;

        private GEDCOMGroupRecord fGroup;

        public GEDCOMGroupRecord Group
        {
            get {
                return fGroup;
            }
            set {
                if (fGroup != value) {
                    fGroup = value;

                    edName.Text = (fGroup == null) ? "" : fGroup.GroupName;

                    fMembersList.ListModel.DataOwner = fGroup;
                    fNotesList.ListModel.DataOwner = fGroup;
                    fMediaList.ListModel.DataOwner = fGroup;
                }
            }
        }

        public GroupEditDlg()
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fMembersList = new GKSheetList(pageMembers);
            fMembersList.OnModify += ModifyMembersSheet;

            fNotesList = new GKSheetList(pageNotes);
            fMediaList = new GKSheetList(pageMultimedia);

            // SetLang()
            Title = LangMan.LS(LSID.LSID_WinGroupEdit);
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            lblName.Text = LangMan.LS(LSID.LSID_Title);
            pageMembers.Text = LangMan.LS(LSID.LSID_Members);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);

            fController = new GroupEditDlgController(this);
        }
        
        private void ModifyMembersSheet(object sender, ModifyEventArgs eArgs)
        {
            GEDCOMIndividualRecord member = eArgs.ItemData as GEDCOMIndividualRecord;
            if (eArgs.Action == RecordAction.raJump && member != null) {
                AcceptChanges();
                DialogResult = DialogResult.Ok;
                fBase.SelectRecordByXRef(member.XRef);
                Close();
            }
        }

        private void AcceptChanges()
        {
            CommitChanges();

            fGroup.GroupName = edName.Text;
            fBase.NotifyRecord(fGroup, RecordAction.raEdit);
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try {
                AcceptChanges();
                DialogResult = DialogResult.Ok;
            } catch (Exception ex) {
                Logger.LogWrite("GroupEditDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
            }
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try {
                RollbackChanges();
                CancelClickHandler(sender, e);
            } catch (Exception ex) {
                Logger.LogWrite("GroupEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);
            fController.Init(baseWin);

            fMembersList.ListModel = new GroupMembersSublistModel(fBase, fLocalUndoman);
            fNotesList.ListModel = new NoteLinksListModel(fBase, fLocalUndoman);
            fMediaList.ListModel = new MediaLinksListModel(fBase, fLocalUndoman);
        }
    }
}
