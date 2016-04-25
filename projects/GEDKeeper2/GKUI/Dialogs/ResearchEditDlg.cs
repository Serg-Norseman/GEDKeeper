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
using GKCore.Types;
using GKUI.Controls;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class ResearchEditDlg : Form, IBaseEditor
    {
        private readonly IBaseWindow fBase;

        private readonly GKSheetList fTasksList;
        private readonly GKSheetList fCommunicationsList;
        private readonly GKSheetList fGroupsList;
        private readonly GKNotesSheet fNotesList;

        private GEDCOMResearchRecord fResearch;

        public GEDCOMResearchRecord Research
        {
            get { return this.fResearch; }
            set { this.SetResearch(value); }
        }

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }

        private void AcceptChanges()
        {
            this.fResearch.ResearchName = this.EditName.Text;
            this.fResearch.Priority = (GKResearchPriority)this.EditPriority.SelectedIndex;
            this.fResearch.Status = (GKResearchStatus)this.EditStatus.SelectedIndex;
            this.fResearch.StartDate.ParseString(GEDCOMUtils.StrToGEDCOMDate(this.EditStartDate.Text, true));
            this.fResearch.StopDate.ParseString(GEDCOMUtils.StrToGEDCOMDate(this.EditStopDate.Text, true));
            this.fResearch.Percent = int.Parse(this.EditPercent.Text);
            this.fBase.ChangeRecord(this.fResearch);
        }

        private void SetResearch(GEDCOMResearchRecord value)
        {
            this.fResearch = value;
            try
            {
                if (this.fResearch == null)
                {
                    this.EditName.Text = "";
                    this.EditPriority.SelectedIndex = -1;
                    this.EditStatus.SelectedIndex = -1;
                    this.EditStartDate.Text = "";
                    this.EditStopDate.Text = "";
                    this.EditPercent.Text = @"0";
                }
                else
                {
                    this.EditName.Text = this.fResearch.ResearchName;
                    this.EditPriority.SelectedIndex = (int)this.fResearch.Priority;
                    this.EditStatus.SelectedIndex = (int)this.fResearch.Status;
                    this.EditStartDate.Text = GKUtils.GetDateFmtString(this.fResearch.StartDate, DateFormat.dfDD_MM_YYYY);
                    this.EditStopDate.Text = GKUtils.GetDateFmtString(this.fResearch.StopDate, DateFormat.dfDD_MM_YYYY);
                    this.EditPercent.Text = this.fResearch.Percent.ToString();
                }

                this.UpdateLists();
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TfmResearchEdit.SetResearch(): " + ex.Message);
            }
        }

        private bool ListTasksModify(object sender, ModifyEventArgs eArgs)
        {
            bool res = false;

            GEDCOMTaskRecord task = eArgs.ItemData as GEDCOMTaskRecord;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    task = this.fBase.SelectRecord(GEDCOMRecordType.rtTask, null) as GEDCOMTaskRecord;
                    res = this.fResearch.AddTask(task);
                    break;

                case RecordAction.raEdit:
                    res = (task != null && this.fBase.ModifyTask(ref task));
                    break;

                case RecordAction.raDelete:
                    if (task != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachTaskQuery)) != DialogResult.No)
                    {
                        this.fResearch.RemoveTask(task);
                        res = true;
                    }
                    break;

                case RecordAction.raJump:
                    if (task != null)
                    {
                        this.AcceptChanges();
                        this.fBase.SelectRecordByXRef(task.XRef);
                        base.Close();
                    }
                    break;
            }
            
            return res;
        }

        private bool ListCommunicationsModify(object sender, ModifyEventArgs eArgs)
        {
            bool res = false;

            GEDCOMCommunicationRecord comm = eArgs.ItemData as GEDCOMCommunicationRecord;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    comm = this.fBase.SelectRecord(GEDCOMRecordType.rtCommunication, null) as GEDCOMCommunicationRecord;
                    res = this.fResearch.AddCommunication(comm);
                    break;

                case RecordAction.raEdit:
                    res = (comm != null && this.fBase.ModifyCommunication(ref comm));
                    break;

                case RecordAction.raDelete:
                    if (comm != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachCommunicationQuery)) != DialogResult.No)
                    {
                        this.fResearch.RemoveCommunication(comm);
                        res = true;
                    }
                    break;

                case RecordAction.raJump:
                    if (comm != null)
                    {
                        this.AcceptChanges();
                        this.fBase.SelectRecordByXRef(comm.XRef);
                        base.Close();
                    }
                    break;
            }

            return res;
        }

        private bool ListGroupsModify(object sender, ModifyEventArgs eArgs)
        {
            bool res = false;

            GEDCOMGroupRecord group = eArgs.ItemData as GEDCOMGroupRecord;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    group = this.fBase.SelectRecord(GEDCOMRecordType.rtGroup, null) as GEDCOMGroupRecord;
                    res = this.fResearch.AddGroup(group);
                    break;

                case RecordAction.raDelete:
                    if (group != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachGroupQuery)) != DialogResult.No)
                    {
                        this.fResearch.RemoveGroup(group);
                        res = true;
                    }
                    break;

                case RecordAction.raJump:
                    if (group != null)
                    {
                        this.AcceptChanges();
                        this.fBase.SelectRecordByXRef(group.XRef);
                        base.Close();
                    }
                    break;
            }

            return res;
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
            bool res = false;

            if (sender == this.fTasksList)
            {
                res = this.ListTasksModify(sender, eArgs);
            }
            else if (sender == this.fCommunicationsList)
            {
                res = this.ListCommunicationsModify(sender, eArgs);
            }
            else if (sender == this.fGroupsList)
            {
                res = this.ListGroupsModify(sender, eArgs);
            }

            if (res) this.UpdateLists();
        }

        private void UpdateLists()
        {
            this.fNotesList.DataList = this.fResearch.Notes.GetEnumerator();

            this.fTasksList.BeginUpdate();
            this.fTasksList.ClearItems();
            foreach (GEDCOMPointer taskPtr in this.fResearch.Tasks)
            {
                GEDCOMTaskRecord task = taskPtr.Value as GEDCOMTaskRecord;
                if (task == null) continue;

                GKListItem item = this.fTasksList.AddItem(GKUtils.GetTaskGoalStr(task), task);
                item.AddSubItem(LangMan.LS(GKData.PriorityNames[(int)task.Priority]));
                item.AddSubItem(task.StartDate);
                item.AddSubItem(task.StopDate);
            }
            this.fTasksList.EndUpdate();

            this.fCommunicationsList.BeginUpdate();
            this.fCommunicationsList.ClearItems();
            foreach (GEDCOMPointer commPtr in this.fResearch.Communications)
            {
                GEDCOMCommunicationRecord corr = commPtr.Value as GEDCOMCommunicationRecord;
                if (corr == null) continue;

                GKListItem item = this.fCommunicationsList.AddItem(corr.CommName, corr);
                item.AddSubItem(GKUtils.GetCorresponderStr(this.fBase.Tree, corr, false));
                item.AddSubItem(LangMan.LS(GKData.CommunicationNames[(int)corr.CommunicationType]));
                item.AddSubItem(corr.Date);
            }
            this.fCommunicationsList.EndUpdate();

            this.fGroupsList.BeginUpdate();
            this.fGroupsList.ClearItems();
            foreach (GEDCOMPointer groupPtr in this.fResearch.Groups)
            {
                GEDCOMGroupRecord grp = groupPtr.Value as GEDCOMGroupRecord;
                if (grp == null) continue;

                this.fGroupsList.AddItem(grp.GroupName, grp);
            }
            this.fGroupsList.EndUpdate();
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
                this.fBase.Host.LogWrite("TfmResearchEdit.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        public ResearchEditDlg(IBaseWindow aBase)
        {
            this.InitializeComponent();

            this.fBase = aBase;

            for (GKResearchPriority rp = GKResearchPriority.rpNone; rp <= GKResearchPriority.rpTop; rp++)
            {
                this.EditPriority.Items.Add(LangMan.LS(GKData.PriorityNames[(int)rp]));
            }

            for (GKResearchStatus rs = GKResearchStatus.rsDefined; rs <= GKResearchStatus.rsWithdrawn; rs++)
            {
                this.EditStatus.Items.Add(LangMan.LS(GKData.StatusNames[(int)rs]));
            }

            this.fTasksList = new GKSheetList(this.SheetTasks);
            this.fTasksList.OnModify += this.ListModify;
            this.fTasksList.Buttons = EnumSet<SheetButton>.Create(
                SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete, SheetButton.lbJump
               );
            this.fTasksList.AddColumn(LangMan.LS(LSID.LSID_Goal), 250, false);
            this.fTasksList.AddColumn(LangMan.LS(LSID.LSID_Priority), 90, false);
            this.fTasksList.AddColumn(LangMan.LS(LSID.LSID_StartDate), 90, false);
            this.fTasksList.AddColumn(LangMan.LS(LSID.LSID_StopDate), 90, false);

            this.fCommunicationsList = new GKSheetList(this.SheetCommunications);
            this.fCommunicationsList.OnModify += this.ListModify;
            this.fCommunicationsList.Buttons = EnumSet<SheetButton>.Create(
                SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete, SheetButton.lbJump
               );
            this.fCommunicationsList.AddColumn(LangMan.LS(LSID.LSID_Theme), 150, false);
            this.fCommunicationsList.AddColumn(LangMan.LS(LSID.LSID_Corresponder), 150, false);
            this.fCommunicationsList.AddColumn(LangMan.LS(LSID.LSID_Type), 90, false);
            this.fCommunicationsList.AddColumn(LangMan.LS(LSID.LSID_Date), 90, false);

            this.fGroupsList = new GKSheetList(this.SheetGroups);
            this.fGroupsList.OnModify += this.ListModify;
            this.fGroupsList.Buttons = EnumSet<SheetButton>.Create(
                SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete, SheetButton.lbJump
               );
            this.fGroupsList.AddColumn(LangMan.LS(LSID.LSID_Group), 350, false);

            this.fNotesList = new GKNotesSheet(this, this.SheetNotes);

            this.SetLang();
        }

        public void SetLang()
        {
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Text = LangMan.LS(LSID.LSID_WinResearchEdit);
            this.SheetTasks.Text = LangMan.LS(LSID.LSID_RPTasks);
            this.SheetCommunications.Text = LangMan.LS(LSID.LSID_RPCommunications);
            this.SheetGroups.Text = LangMan.LS(LSID.LSID_RPGroups);
            this.SheetNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            this.Label1.Text = LangMan.LS(LSID.LSID_Title);
            this.Label2.Text = LangMan.LS(LSID.LSID_Priority);
            this.Label3.Text = LangMan.LS(LSID.LSID_Status);
            this.Label6.Text = LangMan.LS(LSID.LSID_Percent);
            this.Label4.Text = LangMan.LS(LSID.LSID_StartDate);
            this.Label5.Text = LangMan.LS(LSID.LSID_StopDate);
        }
    }
}
