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
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class TaskEditDlgController : DialogController<ITaskEditDlg>
    {
        private GDMTaskRecord fTask;
        private GDMRecord fTempRec;

        public GDMTaskRecord Task
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

            for (GDMResearchPriority rp = GDMResearchPriority.rpNone; rp <= GDMResearchPriority.rpTop; rp++) {
                fView.Priority.Add(LangMan.LS(GKData.PriorityNames[(int)rp]));
            }

            for (GDMGoalType gt = GDMGoalType.gtIndividual; gt <= GDMGoalType.gtOther; gt++) {
                fView.GoalType.Add(LangMan.LS(GKData.GoalNames[(int)gt]));
            }
        }

        public override bool Accept()
        {
            try {
                fTask.Priority = (GDMResearchPriority)fView.Priority.SelectedIndex;
                fTask.StartDate.Assign(GDMDate.CreateByFormattedStr(fView.StartDate.Text, true));
                fTask.StopDate.Assign(GDMDate.CreateByFormattedStr(fView.StopDate.Text, true));

                GDMGoalType gt = (GDMGoalType)fView.GoalType.SelectedIndex;
                switch (gt) {
                    case GDMGoalType.gtIndividual:
                    case GDMGoalType.gtFamily:
                    case GDMGoalType.gtSource:
                        fTask.Goal = GEDCOMUtils.EncloseXRef(fTempRec.XRef);
                        break;

                    case GDMGoalType.gtOther:
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
                    case GDMGoalType.gtIndividual:
                    case GDMGoalType.gtFamily:
                    case GDMGoalType.gtSource:
                        fView.Goal.Text = GKUtils.GetGoalStr(goal.GoalType, fTempRec);
                        break;

                    case GDMGoalType.gtOther:
                        fView.Goal.Text = fTask.Goal;
                        break;
                }
            }

            fView.NotesList.ListModel.DataOwner = fTask;

            ChangeGoalType();
        }

        public void SelectGoal()
        {
            GDMGoalType gt = (GDMGoalType)fView.GoalType.SelectedIndex;
            switch (gt) {
                case GDMGoalType.gtIndividual:
                    fTempRec = fBase.Context.SelectPerson(null, TargetMode.tmNone, GDMSex.svUnknown);
                    fView.Goal.Text = GKUtils.GetGoalStr(gt, fTempRec);
                    break;

                case GDMGoalType.gtFamily:
                    fTempRec = fBase.Context.SelectRecord(GDMRecordType.rtFamily, new object[0]);
                    fView.Goal.Text = GKUtils.GetGoalStr(gt, fTempRec);
                    break;

                case GDMGoalType.gtSource:
                    fTempRec = fBase.Context.SelectRecord(GDMRecordType.rtSource, new object[0]);
                    fView.Goal.Text = GKUtils.GetGoalStr(gt, fTempRec);
                    break;

                case GDMGoalType.gtOther:
                    break;
            }
        }

        public void ChangeGoalType()
        {
            GDMGoalType gt = (GDMGoalType)fView.GoalType.SelectedIndex;
            switch (gt) {
                case GDMGoalType.gtIndividual:
                case GDMGoalType.gtFamily:
                case GDMGoalType.gtSource:
                    fView.GoalSelect.Enabled = true;
                    fView.Goal.ReadOnly = true;
                    break;

                case GDMGoalType.gtOther:
                    fView.GoalSelect.Enabled = false;
                    fView.Goal.ReadOnly = false;
                    break;
            }
        }
    }
}
