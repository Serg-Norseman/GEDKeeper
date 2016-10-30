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
    public partial class TaskEditDlg : EditorDialog
    {
        private readonly GKNotesSheet fNotesList;
        
        private GEDCOMTaskRecord fTask;
        private GEDCOMRecord fTempRec;

        public GEDCOMTaskRecord Task
        {
            get { return this.fTask; }
            set { this.SetTask(value); }
        }

        private void SetTask(GEDCOMTaskRecord value)
        {
            this.fTask = value;
            try
            {
                if (this.fTask == null)
                {
                    this.txtPriority.SelectedIndex = -1;
                    this.txtStartDate.Text = "";
                    this.txtStopDate.Text = "";
                    this.cmbGoalType.SelectedIndex = 0;
                    this.txtGoal.Text = "";

                    this.fNotesList.DataList = null;
                }
                else
                {
                    this.txtPriority.SelectedIndex = (sbyte)this.fTask.Priority;
                    this.txtStartDate.Text = GKUtils.GetDateFmtString(this.fTask.StartDate, DateFormat.dfDD_MM_YYYY);
                    this.txtStopDate.Text = GKUtils.GetDateFmtString(this.fTask.StopDate, DateFormat.dfDD_MM_YYYY);

                    GKGoalType gt;
                    this.fTask.GetTaskGoal(out gt, out this.fTempRec);
                    this.cmbGoalType.SelectedIndex = (sbyte)gt;

                    switch (gt) {
                        case GKGoalType.gtIndividual:
                        case GKGoalType.gtFamily:
                        case GKGoalType.gtSource:
                            this.txtGoal.Text = GKUtils.GetGoalStr(gt, this.fTempRec);
                            break;

                        case GKGoalType.gtOther:
                            this.txtGoal.Text = this.fTask.Goal;
                            break;
                    }

                    this.fNotesList.DataList = this.fTask.Notes.GetEnumerator();
                }

                this.cbGoalType_SelectedIndexChanged(null, null);
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TaskEditDlg.SetTask(): " + ex.Message);
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                this.fTask.Priority = (GKResearchPriority)this.txtPriority.SelectedIndex;
                this.fTask.StartDate.ParseString(GEDCOMUtils.StrToGEDCOMDate(this.txtStartDate.Text, true));
                this.fTask.StopDate.ParseString(GEDCOMUtils.StrToGEDCOMDate(this.txtStopDate.Text, true));
                GKGoalType gt = (GKGoalType)this.cmbGoalType.SelectedIndex;
                switch (gt) {
                    case GKGoalType.gtIndividual:
                    case GKGoalType.gtFamily:
                    case GKGoalType.gtSource:
                        this.fTask.Goal = GEDCOMUtils.EncloseXRef(this.fTempRec.XRef);
                        break;
                    case GKGoalType.gtOther:
                        this.fTask.Goal = this.txtGoal.Text;
                        break;
                }

                base.CommitChanges();
                this.Base.ChangeRecord(this.fTask);
                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TaskEditDlg.btnAccept_Click(): " + ex.Message);
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
                this.fBase.Host.LogWrite("TaskEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void btnGoalSelect_Click(object sender, EventArgs e)
        {
            GKGoalType gt = (GKGoalType)this.cmbGoalType.SelectedIndex;
            switch (gt) {
                case GKGoalType.gtIndividual:
                    this.fTempRec = this.Base.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svNone);
                    this.txtGoal.Text = GKUtils.GetGoalStr(gt, this.fTempRec);
                    break;

                case GKGoalType.gtFamily:
                    this.fTempRec = this.Base.SelectRecord(GEDCOMRecordType.rtFamily, new object[0]);
                    this.txtGoal.Text = GKUtils.GetGoalStr(gt, this.fTempRec);
                    break;

                case GKGoalType.gtSource:
                    this.fTempRec = this.Base.SelectRecord(GEDCOMRecordType.rtSource, new object[0]);
                    this.txtGoal.Text = GKUtils.GetGoalStr(gt, this.fTempRec);
                    break;

                case GKGoalType.gtOther:
                    break;
            }
        }

        private void cbGoalType_SelectedIndexChanged(object sender, EventArgs e)
        {
            GKGoalType gt = (GKGoalType)this.cmbGoalType.SelectedIndex;
            switch (gt) {
                case GKGoalType.gtIndividual:
                    this.btnGoalSelect.Enabled = true;
                    this.txtGoal.BackColor = SystemColors.Control;
                    this.txtGoal.ReadOnly = true;
                    break;
                case GKGoalType.gtFamily:
                    this.btnGoalSelect.Enabled = true;
                    this.txtGoal.BackColor = SystemColors.Control;
                    this.txtGoal.ReadOnly = true;
                    break;
                case GKGoalType.gtSource:
                    this.btnGoalSelect.Enabled = true;
                    this.txtGoal.BackColor = SystemColors.Control;
                    this.txtGoal.ReadOnly = true;
                    break;
                case GKGoalType.gtOther:
                    this.btnGoalSelect.Enabled = false;
                    this.txtGoal.BackColor = SystemColors.Window;
                    this.txtGoal.ReadOnly = false;
                    break;
            }
        }

        public TaskEditDlg(IBaseWindow baseWin) : base(baseWin)
        {
            this.InitializeComponent();

            this.btnGoalSelect.Image = global::GKResources.iRecNew;
            this.btnAccept.Image = global::GKResources.iBtnAccept;
            this.btnCancel.Image = global::GKResources.iBtnCancel;

            this.fTempRec = null;

            for (GKResearchPriority rp = GKResearchPriority.rpNone; rp <= GKResearchPriority.rpTop; rp++)
            {
                this.txtPriority.Items.Add(LangMan.LS(GKData.PriorityNames[(int)rp]));
            }

            for (GKGoalType gt = GKGoalType.gtIndividual; gt <= GKGoalType.gtOther; gt++)
            {
                this.cmbGoalType.Items.Add(LangMan.LS(GKData.GoalNames[(int)gt]));
            }

            this.fNotesList = new GKNotesSheet(this, this.pageNotes, this.fLocalUndoman);

            // SetLang()
            this.Text = LangMan.LS(LSID.LSID_WinTaskEdit);
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            this.lblGoal.Text = LangMan.LS(LSID.LSID_Goal);
            this.lblPriority.Text = LangMan.LS(LSID.LSID_Priority);
            this.lblStartDate.Text = LangMan.LS(LSID.LSID_StartDate);
            this.lblStopDate.Text = LangMan.LS(LSID.LSID_StopDate);

            this.toolTip1.SetToolTip(this.btnGoalSelect, LangMan.LS(LSID.LSID_GoalSelectTip));
        }
    }
}
