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
using Eto.Drawing;
using Eto.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Operations;
using GKCore.Types;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class GroupEditDlg : EditorDialog, IGroupEditDlg
    {
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
            
            btnAccept.Image = Bitmap.FromResource("Resources.btn_accept.gif");
            btnCancel.Image = Bitmap.FromResource("Resources.btn_cancel.gif");

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
        }
        
        private void ModifyMembersSheet(object sender, ModifyEventArgs eArgs)
        {
            GEDCOMIndividualRecord member = eArgs.ItemData as GEDCOMIndividualRecord;
            if (eArgs.Action == RecordAction.raJump && member != null) {
                AcceptChanges();
                DialogResult = DlgResult.OK;
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
            try
            {
                AcceptChanges();
                DialogResult = DlgResult.OK;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GroupEditDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DlgResult.None;
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
                Logger.LogWrite("GroupEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);

            fMembersList.ListModel = new GroupMembersSublistModel(fBase, fLocalUndoman);
            fNotesList.ListModel = new NoteLinksListModel(fBase, fLocalUndoman);
            fMediaList.ListModel = new MediaLinksListModel(fBase, fLocalUndoman);
        }
    }
}
