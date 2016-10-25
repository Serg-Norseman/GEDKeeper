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
    public partial class GroupEditDlg : EditorDialog
    {
        private readonly GKSheetList fMembersList;
        private readonly GKNotesSheet fNotesList;
        private readonly GKMediaSheet fMediaList;

        private GEDCOMGroupRecord fGroup;

        public GEDCOMGroupRecord Group
        {
            get { return this.fGroup; }
            set { this.SetGroup(value); }
        }

        private void SetGroup(GEDCOMGroupRecord value)
        {
            this.fGroup = value;
            try
            {
                this.edName.Text = (this.fGroup == null) ? "" : this.fGroup.GroupName;

                if (this.fGroup != null)
                {
                    this.fNotesList.DataList = this.fGroup.Notes.GetEnumerator();
                    this.fMediaList.DataList = this.fGroup.MultimediaLinks.GetEnumerator();
                }

                this.UpdateMembersSheet();
            }
            catch (Exception ex)
            {
                this.Base.Host.LogWrite("GroupEditDlg.SetGroup(): " + ex.Message);
            }
        }
        
        public GroupEditDlg(IBaseWindow baseWin) : base(baseWin)
        {
            this.InitializeComponent();
            
            this.btnAccept.Image = global::GKResources.iBtnAccept;
            this.btnCancel.Image = global::GKResources.iBtnCancel;

            this.fMembersList = CreateMembersSheet(this.pageMembers);
            this.fNotesList = new GKNotesSheet(this, this.pageNotes);
            this.fMediaList = new GKMediaSheet(this, this.pageMultimedia);

            // SetLang()
            this.Text = LangMan.LS(LSID.LSID_WinGroupEdit);
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.lblName.Text = LangMan.LS(LSID.LSID_Title);
            this.pageMembers.Text = LangMan.LS(LSID.LSID_Members);
            this.pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            this.pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
        }

        private GKSheetList CreateMembersSheet(Control owner)
        {
            GKSheetList sheet = new GKSheetList(owner);
            
            sheet.Columns_BeginUpdate();
            sheet.AddColumn(LangMan.LS(LSID.LSID_Name), 300, false);
            sheet.Columns_EndUpdate();

            sheet.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbDelete, SheetButton.lbJump);
            sheet.OnModify += this.ModifyMembersSheet;
            
            return sheet;
        }
        
        private void UpdateMembersSheet()
        {
            try
            {
                this.fMembersList.ClearItems();
                if (this.fGroup == null) return;

                foreach (GEDCOMPointer ptrMember in this.fGroup.Members) {
                    GEDCOMIndividualRecord member = ptrMember.Value as GEDCOMIndividualRecord;
                    if (member == null) continue;

                    this.fMembersList.AddItem(member.GetNameString(true, false), member);
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
                    member = this.Base.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svNone);
                    result = (member != null);
                    if (result) {
                        //this.fGroup.AddMember(member);
                        this.fLocalUndoman.DoOrdinaryOperation(OperationType.otGroupMemberAttach, this.fGroup, member);
                    }
                    break;

                case RecordAction.raDelete:
                    result = (member != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachMemberQuery)) != DialogResult.No);
                    if (result) {
                        //this.fGroup.RemoveMember(member);
                        this.fLocalUndoman.DoOrdinaryOperation(OperationType.otGroupMemberDetach, this.fGroup, member);
                    }
                    break;

                case RecordAction.raJump:
                    if (member != null) {
                        this.AcceptChanges();
                        base.DialogResult = DialogResult.OK;
                        this.Base.SelectRecordByXRef(member.XRef);
                        base.Close();
                    }
                    break;
            }

            if (result) this.UpdateMembersSheet();
        }

        private void AcceptChanges()
        {
            base.CommitChanges();

            this.fGroup.GroupName = this.edName.Text;
            this.Base.ChangeRecord(this.fGroup);
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
                this.Base.Host.LogWrite("GroupEditDlg.btnAccept_Click(): " + ex.Message);
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
                this.Base.Host.LogWrite("GroupEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }
    }
}
