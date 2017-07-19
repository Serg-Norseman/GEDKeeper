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
using System.Drawing;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TaskEditDlg : EditorDialog, ITaskEditDlg
    {
        private readonly GKSheetList fNotesList;
        
        private GEDCOMTaskRecord fTask;
        private GEDCOMRecord fTempRec;

        public GEDCOMTaskRecord Task
        {
            get { return fTask; }
            set { SetTask(value); }
        }

        private void SetTask(GEDCOMTaskRecord value)
        {
            fTask = value;
            try
            {
                if (fTask == null)
                {
                    txtPriority.SelectedIndex = -1;
                    txtStartDate.Text = "";
                    txtStopDate.Text = "";
                    cmbGoalType.SelectedIndex = 0;
                    txtGoal.Text = "";
                }
                else
                {
                    txtPriority.SelectedIndex = (sbyte)fTask.Priority;
                    txtStartDate.Text = fTask.StartDate.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                    txtStopDate.Text = fTask.StopDate.GetDisplayString(DateFormat.dfDD_MM_YYYY);

                    GKGoalType gt;
                    fTask.GetTaskGoal(out gt, out fTempRec);
                    cmbGoalType.SelectedIndex = (sbyte)gt;

                    switch (gt) {
                        case GKGoalType.gtIndividual:
                        case GKGoalType.gtFamily:
                        case GKGoalType.gtSource:
                            txtGoal.Text = GKUtils.GetGoalStr(gt, fTempRec);
                            break;

                        case GKGoalType.gtOther:
                            txtGoal.Text = fTask.Goal;
                            break;
                    }
                }

                fNotesList.ListModel.DataOwner = fTask;

                cmbGoalType_SelectedIndexChanged(null, null);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TaskEditDlg.SetTask(): " + ex.Message);
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                fTask.Priority = (GKResearchPriority)txtPriority.SelectedIndex;
                fTask.StartDate.Assign(GEDCOMDate.CreateByFormattedStr(txtStartDate.Text, true));
                fTask.StopDate.Assign(GEDCOMDate.CreateByFormattedStr(txtStopDate.Text, true));

                GKGoalType gt = (GKGoalType)cmbGoalType.SelectedIndex;
                switch (gt) {
                    case GKGoalType.gtIndividual:
                    case GKGoalType.gtFamily:
                    case GKGoalType.gtSource:
                        fTask.Goal = GEDCOMUtils.EncloseXRef(fTempRec.XRef);
                        break;
                    case GKGoalType.gtOther:
                        fTask.Goal = txtGoal.Text;
                        break;
                }

                CommitChanges();

                Base.NotifyRecord(fTask, RecordAction.raEdit);

                DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TaskEditDlg.btnAccept_Click(): " + ex.Message);
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
                Logger.LogWrite("TaskEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void btnGoalSelect_Click(object sender, EventArgs e)
        {
            GKGoalType gt = (GKGoalType)cmbGoalType.SelectedIndex;
            switch (gt) {
                case GKGoalType.gtIndividual:
                    fTempRec = fBase.Context.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svNone);
                    txtGoal.Text = GKUtils.GetGoalStr(gt, fTempRec);
                    break;

                case GKGoalType.gtFamily:
                    fTempRec = fBase.Context.SelectRecord(GEDCOMRecordType.rtFamily, new object[0]);
                    txtGoal.Text = GKUtils.GetGoalStr(gt, fTempRec);
                    break;

                case GKGoalType.gtSource:
                    fTempRec = fBase.Context.SelectRecord(GEDCOMRecordType.rtSource, new object[0]);
                    txtGoal.Text = GKUtils.GetGoalStr(gt, fTempRec);
                    break;

                case GKGoalType.gtOther:
                    break;
            }
        }

        private void cmbGoalType_SelectedIndexChanged(object sender, EventArgs e)
        {
            GKGoalType gt = (GKGoalType)cmbGoalType.SelectedIndex;
            switch (gt) {
                case GKGoalType.gtIndividual:
                    btnGoalSelect.Enabled = true;
                    txtGoal.BackColor = SystemColors.Control;
                    txtGoal.ReadOnly = true;
                    break;

                case GKGoalType.gtFamily:
                    btnGoalSelect.Enabled = true;
                    txtGoal.BackColor = SystemColors.Control;
                    txtGoal.ReadOnly = true;
                    break;

                case GKGoalType.gtSource:
                    btnGoalSelect.Enabled = true;
                    txtGoal.BackColor = SystemColors.Control;
                    txtGoal.ReadOnly = true;
                    break;

                case GKGoalType.gtOther:
                    btnGoalSelect.Enabled = false;
                    txtGoal.BackColor = SystemColors.Window;
                    txtGoal.ReadOnly = false;
                    break;
            }
        }

        public TaskEditDlg()
        {
            InitializeComponent();

            btnGoalSelect.Image = GKResources.iRecNew;
            btnAccept.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;

            fTempRec = null;

            for (GKResearchPriority rp = GKResearchPriority.rpNone; rp <= GKResearchPriority.rpTop; rp++)
            {
                txtPriority.Items.Add(LangMan.LS(GKData.PriorityNames[(int)rp]));
            }

            for (GKGoalType gt = GKGoalType.gtIndividual; gt <= GKGoalType.gtOther; gt++)
            {
                cmbGoalType.Items.Add(LangMan.LS(GKData.GoalNames[(int)gt]));
            }

            fNotesList = new GKSheetList(pageNotes);

            // SetLang()
            Text = LangMan.LS(LSID.LSID_WinTaskEdit);
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            lblGoal.Text = LangMan.LS(LSID.LSID_Goal);
            lblPriority.Text = LangMan.LS(LSID.LSID_Priority);
            lblStartDate.Text = LangMan.LS(LSID.LSID_StartDate);
            lblStopDate.Text = LangMan.LS(LSID.LSID_StopDate);

            toolTip1.SetToolTip(btnGoalSelect, LangMan.LS(LSID.LSID_GoalSelectTip));
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);

            fNotesList.ListModel = new NoteLinksListModel(fBase, fLocalUndoman);
        }
    }
}
