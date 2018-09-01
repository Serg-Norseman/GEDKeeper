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
using GKCommon.GEDCOM;
using GKCore.Types;
using GKCore.UIContracts;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class TaskEditDlgController : DialogController<ITaskEditDlg>
    {
        private GEDCOMTaskRecord fTask;
        private GEDCOMRecord fTempRec;

        public GEDCOMTaskRecord Task
        {
            get { return fTask; }
            set {
                if (fTask != value) {
                    fTask = value;
                    UpdateView();
                }
            }
        }


        public TaskEditDlgController(ITaskEditDlg view) : base(view)
        {
            fTempRec = null;

            for (GKResearchPriority rp = GKResearchPriority.rpNone; rp <= GKResearchPriority.rpTop; rp++) {
                fView.Priority.Add(LangMan.LS(GKData.PriorityNames[(int)rp]));
            }

            for (GKGoalType gt = GKGoalType.gtIndividual; gt <= GKGoalType.gtOther; gt++) {
                fView.GoalType.Add(LangMan.LS(GKData.GoalNames[(int)gt]));
            }
        }

        public override bool Accept()
        {
            try {
                fTask.Priority = (GKResearchPriority)fView.Priority.SelectedIndex;
                fTask.StartDate.Assign(GEDCOMDate.CreateByFormattedStr(fView.StartDate.Text, true));
                fTask.StopDate.Assign(GEDCOMDate.CreateByFormattedStr(fView.StopDate.Text, true));

                GKGoalType gt = (GKGoalType)fView.GoalType.SelectedIndex;
                switch (gt) {
                    case GKGoalType.gtIndividual:
                    case GKGoalType.gtFamily:
                    case GKGoalType.gtSource:
                        fTask.Goal = GEDCOMUtils.EncloseXRef(fTempRec.XRef);
                        break;
                    case GKGoalType.gtOther:
                        fTask.Goal = fView.Goal.Text;
                        break;
                }

                fLocalUndoman.Commit();

                fBase.NotifyRecord(fTask, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.LogWrite("TaskEditDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
            if (fTask == null) {
                fView.Priority.SelectedIndex = -1;
                fView.StartDate.Text = "";
                fView.StopDate.Text = "";
                fView.GoalType.SelectedIndex = 0;
                fView.Goal.Text = "";
            } else {
                fView.Priority.SelectedIndex = (sbyte)fTask.Priority;
                fView.StartDate.Text = fTask.StartDate.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                fView.StopDate.Text = fTask.StopDate.GetDisplayString(DateFormat.dfDD_MM_YYYY);

                var goal = fTask.GetTaskGoal();
                fTempRec = goal.GoalRec;
                fView.GoalType.SelectedIndex = (sbyte)goal.GoalType;

                switch (goal.GoalType) {
                    case GKGoalType.gtIndividual:
                    case GKGoalType.gtFamily:
                    case GKGoalType.gtSource:
                        fView.Goal.Text = GKUtils.GetGoalStr(goal.GoalType, fTempRec);
                        break;

                    case GKGoalType.gtOther:
                        fView.Goal.Text = fTask.Goal;
                        break;
                }
            }

            fView.NotesList.ListModel.DataOwner = fTask;

            ChangeGoalType();
        }

        public void SelectGoal()
        {
            GKGoalType gt = (GKGoalType)fView.GoalType.SelectedIndex;
            switch (gt) {
                case GKGoalType.gtIndividual:
                    fTempRec = fBase.Context.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svNone);
                    fView.Goal.Text = GKUtils.GetGoalStr(gt, fTempRec);
                    break;

                case GKGoalType.gtFamily:
                    fTempRec = fBase.Context.SelectRecord(GEDCOMRecordType.rtFamily, new object[0]);
                    fView.Goal.Text = GKUtils.GetGoalStr(gt, fTempRec);
                    break;

                case GKGoalType.gtSource:
                    fTempRec = fBase.Context.SelectRecord(GEDCOMRecordType.rtSource, new object[0]);
                    fView.Goal.Text = GKUtils.GetGoalStr(gt, fTempRec);
                    break;

                case GKGoalType.gtOther:
                    break;
            }
        }

        public void ChangeGoalType()
        {
            GKGoalType gt = (GKGoalType)fView.GoalType.SelectedIndex;
            switch (gt) {
                case GKGoalType.gtIndividual:
                    fView.GoalSelect.Enabled = true;
                    fView.Goal.ReadOnly = true;
                    break;

                case GKGoalType.gtFamily:
                    fView.GoalSelect.Enabled = true;
                    fView.Goal.ReadOnly = true;
                    break;

                case GKGoalType.gtSource:
                    fView.GoalSelect.Enabled = true;
                    fView.Goal.ReadOnly = true;
                    break;

                case GKGoalType.gtOther:
                    fView.GoalSelect.Enabled = false;
                    fView.Goal.ReadOnly = false;
                    break;
            }
        }
    }
}
