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
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class GroupEditDlg : EditorDialog
    {
        private readonly GKSheetList fMembersList;
        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;

        private GEDCOMGroupRecord fGroup;

        public GEDCOMGroupRecord Group
        {
            get { return fGroup; }
            set { SetGroup(value); }
        }

        private void SetGroup(GEDCOMGroupRecord value)
        {
            fGroup = value;
            try
            {
                edName.Text = (fGroup == null) ? "" : fGroup.GroupName;

                fNotesList.ListModel.DataOwner = fGroup;
                fMediaList.ListModel.DataOwner = fGroup;

                UpdateMembersSheet();
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("GroupEditDlg.SetGroup(): " + ex.Message);
            }
        }

        public GroupEditDlg(IBaseWindow baseWin) : base(baseWin)
        {
            InitializeComponent();
            
            btnAccept.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;

            fMembersList = CreateMembersSheet(pageMembers);
            fMembersList.SetControlName("fMembersList"); // for purpose of tests

            fNotesList = new GKSheetList(pageNotes, new GKNotesListModel(fBase, fLocalUndoman));
            fMediaList = new GKSheetList(pageMultimedia, new GKMediaListModel(fBase, fLocalUndoman));

            // SetLang()
            Text = LangMan.LS(LSID.LSID_WinGroupEdit);
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            lblName.Text = LangMan.LS(LSID.LSID_Title);
            pageMembers.Text = LangMan.LS(LSID.LSID_Members);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
        }

        private GKSheetList CreateMembersSheet(Control owner)
        {
            GKSheetList sheet = new GKSheetList(owner);
            
            sheet.Columns_BeginUpdate();
            sheet.AddColumn(LangMan.LS(LSID.LSID_Name), 300, false);
            sheet.Columns_EndUpdate();

            sheet.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbDelete, SheetButton.lbJump);
            sheet.OnModify += ModifyMembersSheet;
            
            return sheet;
        }
        
        private void UpdateMembersSheet()
        {
            try
            {
                fMembersList.ClearItems();
                if (fGroup == null) return;

                foreach (GEDCOMPointer ptrMember in fGroup.Members) {
                    GEDCOMIndividualRecord member = ptrMember.Value as GEDCOMIndividualRecord;
                    if (member == null) continue;

                    fMembersList.AddItem(GKUtils.GetNameString(member, true, false), member);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GroupEditDlg.UpdateMembersSheet(): " + ex.Message);
            }
        }

        private void ModifyMembersSheet(object sender, ModifyEventArgs eArgs)
        {
            bool result = false;

            GEDCOMIndividualRecord member = eArgs.ItemData as GEDCOMIndividualRecord;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    member = fBase.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svNone);
                    result = (member != null);
                    if (result) {
                        //fGroup.AddMember(member);
                        result = fLocalUndoman.DoOrdinaryOperation(OperationType.otGroupMemberAttach, fGroup, member);
                    }
                    break;

                case RecordAction.raDelete:
                    result = (member != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachMemberQuery)) != DialogResult.No);
                    if (result) {
                        //fGroup.RemoveMember(member);
                        result = fLocalUndoman.DoOrdinaryOperation(OperationType.otGroupMemberDetach, fGroup, member);
                    }
                    break;

                case RecordAction.raJump:
                    if (member != null) {
                        AcceptChanges();
                        DialogResult = DialogResult.OK;
                        fBase.SelectRecordByXRef(member.XRef);
                        Close();
                    }
                    break;
            }

            if (result) UpdateMembersSheet();
        }

        private void AcceptChanges()
        {
            CommitChanges();

            fGroup.GroupName = edName.Text;
            fBase.ChangeRecord(fGroup);
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
                fBase.Host.LogWrite("GroupEditDlg.btnAccept_Click(): " + ex.Message);
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
                fBase.Host.LogWrite("GroupEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }
    }
}
