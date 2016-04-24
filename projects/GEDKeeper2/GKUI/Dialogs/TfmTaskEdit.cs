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
using System.Drawing;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TfmTaskEdit : Form, IBaseEditor
    {
        private readonly IBaseWindow fBase;
        private readonly GKNotesSheet fNotesList;
        
        private GEDCOMTaskRecord fTask;
        private GEDCOMRecord fTempRec;

        public GEDCOMTaskRecord Task
        {
            get { return this.fTask; }
            set { this.SetTask(value); }
        }

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }

        private void SetTask(GEDCOMTaskRecord value)
        {
            this.fTask = value;
            try
            {
                if (this.fTask == null)
                {
                    this.EditPriority.SelectedIndex = -1;
                    this.EditStartDate.Text = "";
                    this.EditStopDate.Text = "";
                    this.cbGoalType.SelectedIndex = 0;
                    this.EditGoal.Text = "";
                }
                else
                {
                    this.EditPriority.SelectedIndex = (sbyte)this.fTask.Priority;
                    this.EditStartDate.Text = GKUtils.GetDateFmtString(this.fTask.StartDate, DateFormat.dfDD_MM_YYYY);
                    this.EditStopDate.Text = GKUtils.GetDateFmtString(this.fTask.StopDate, DateFormat.dfDD_MM_YYYY);

                    GKGoalType gt;
                    this.fTask.GetTaskGoal(out gt, out this.fTempRec);
                    this.cbGoalType.SelectedIndex = (sbyte)gt;

                    switch (gt) {
                        case GKGoalType.gtIndividual:
                        case GKGoalType.gtFamily:
                        case GKGoalType.gtSource:
                            this.EditGoal.Text = GKUtils.GetGoalStr(gt, this.fTempRec);
                            break;

                        case GKGoalType.gtOther:
                            this.EditGoal.Text = this.fTask.Goal;
                            break;
                    }
                }

                this.cbGoalType_SelectedIndexChanged(null, null);
                
                this.fNotesList.DataList = this.fTask.Notes.GetEnumerator();
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TfmTaskEdit.SetTask(): " + ex.Message);
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                this.fTask.Priority = (GKResearchPriority)this.EditPriority.SelectedIndex;
                this.fTask.StartDate.ParseString(GEDCOMUtils.StrToGEDCOMDate(this.EditStartDate.Text, true));
                this.fTask.StopDate.ParseString(GEDCOMUtils.StrToGEDCOMDate(this.EditStopDate.Text, true));
                GKGoalType gt = (GKGoalType)this.cbGoalType.SelectedIndex;
                switch (gt) {
                    case GKGoalType.gtIndividual:
                    case GKGoalType.gtFamily:
                    case GKGoalType.gtSource:
                        this.fTask.Goal = GEDCOMUtils.EncloseXRef(this.fTempRec.XRef);
                        break;
                    case GKGoalType.gtOther:
                        this.fTask.Goal = this.EditGoal.Text;
                        break;
                }

                this.Base.ChangeRecord(this.fTask);
                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TfmTaskEdit.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        private void btnGoalSelect_Click(object sender, EventArgs e)
        {
            GKGoalType gt = (GKGoalType)this.cbGoalType.SelectedIndex;
            switch (gt) {
                case GKGoalType.gtIndividual:
                    this.fTempRec = this.Base.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svNone);
                    this.EditGoal.Text = GKUtils.GetGoalStr(gt, this.fTempRec);
                    break;

                case GKGoalType.gtFamily:
                    this.fTempRec = this.Base.SelectRecord(GEDCOMRecordType.rtFamily, new object[0]);
                    this.EditGoal.Text = GKUtils.GetGoalStr(gt, this.fTempRec);
                    break;

                case GKGoalType.gtSource:
                    this.fTempRec = this.Base.SelectRecord(GEDCOMRecordType.rtSource, new object[0]);
                    this.EditGoal.Text = GKUtils.GetGoalStr(gt, this.fTempRec);
                    break;

                case GKGoalType.gtOther:
                    break;
            }
        }

        private void cbGoalType_SelectedIndexChanged(object sender, EventArgs e)
        {
            GKGoalType gt = (GKGoalType)this.cbGoalType.SelectedIndex;
            switch (gt) {
                case GKGoalType.gtIndividual:
                    this.btnGoalSelect.Enabled = true;
                    this.EditGoal.BackColor = SystemColors.Control;
                    this.EditGoal.ReadOnly = true;
                    break;
                case GKGoalType.gtFamily:
                    this.btnGoalSelect.Enabled = true;
                    this.EditGoal.BackColor = SystemColors.Control;
                    this.EditGoal.ReadOnly = true;
                    break;
                case GKGoalType.gtSource:
                    this.btnGoalSelect.Enabled = true;
                    this.EditGoal.BackColor = SystemColors.Control;
                    this.EditGoal.ReadOnly = true;
                    break;
                case GKGoalType.gtOther:
                    this.btnGoalSelect.Enabled = false;
                    this.EditGoal.BackColor = SystemColors.Window;
                    this.EditGoal.ReadOnly = false;
                    break;
            }
        }

        public TfmTaskEdit(IBaseWindow aBase)
        {
            this.InitializeComponent();

            this.fBase = aBase;
            this.fTempRec = null;

            for (GKResearchPriority rp = GKResearchPriority.rpNone; rp <= GKResearchPriority.rpTop; rp++)
            {
                this.EditPriority.Items.Add(LangMan.LS(GKData.PriorityNames[(int)rp]));
            }

            for (GKGoalType gt = GKGoalType.gtIndividual; gt <= GKGoalType.gtOther; gt++)
            {
                this.cbGoalType.Items.Add(LangMan.LS(GKData.GoalNames[(int)gt]));
            }

            this.fNotesList = new GKNotesSheet(this, this.SheetNotes);

            // SetLang()
            this.Text = LangMan.LS(LSID.LSID_WinTaskEdit);
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.SheetNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            this.Label1.Text = LangMan.LS(LSID.LSID_Goal);
            this.Label2.Text = LangMan.LS(LSID.LSID_Priority);
            this.Label4.Text = LangMan.LS(LSID.LSID_StartDate);
            this.Label5.Text = LangMan.LS(LSID.LSID_StopDate);
        }
    }
}
