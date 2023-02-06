/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore.Design.MVP.Controls;
using GKCore.Interfaces;
using GKCore.Lists;
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
        private GDMTaskRecord fTaskRecord;
        private GDMRecord fTempRec;

        public GDMTaskRecord TaskRecord
        {
            get { return fTaskRecord; }
            set {
                if (fTaskRecord != value) {
                    fTaskRecord = value;
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

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.NotesList.ListModel = new NoteLinksListModel(baseWin, fLocalUndoman);
        }

        public override bool Accept()
        {
            try {
                GDMGoalType gt = (GDMGoalType)fView.GoalType.SelectedIndex;
                switch (gt) {
                    case GDMGoalType.gtIndividual:
                    case GDMGoalType.gtFamily:
                    case GDMGoalType.gtSource:
                        if (fTempRec == null) {
                            AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_NoGoalRecordSpecified));
                            return false;
                        } else {
                            fTaskRecord.Goal = GEDCOMUtils.EncloseXRef(fTempRec.XRef);
                        }
                        break;

                    case GDMGoalType.gtOther:
                        fTaskRecord.Goal = fView.Goal.Text;
                        break;
                }

                fTaskRecord.Priority = (GDMResearchPriority)fView.Priority.SelectedIndex;
                fTaskRecord.StartDate.Assign(GDMDate.CreateByFormattedStr(fView.StartDate.NormalizeDate, true));
                fTaskRecord.StopDate.Assign(GDMDate.CreateByFormattedStr(fView.StopDate.NormalizeDate, true));

                fLocalUndoman.Commit();

                fBase.NotifyRecord(fTaskRecord, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("TaskEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            if (fTaskRecord == null) {
                fView.Priority.SelectedIndex = -1;
                fView.StartDate.Text = "";
                fView.StopDate.Text = "";
                fView.GoalType.SelectedIndex = 0;
                fView.Goal.Text = "";
            } else {
                fView.Priority.SelectedIndex = (sbyte)fTaskRecord.Priority;
                fView.StartDate.NormalizeDate = fTaskRecord.StartDate.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                fView.StopDate.NormalizeDate = fTaskRecord.StopDate.GetDisplayString(DateFormat.dfDD_MM_YYYY);

                var goal = GKUtils.GetTaskGoal(fBase.Context.Tree, fTaskRecord);
                fTempRec = goal.GoalRec;
                fView.GoalType.SelectedIndex = (sbyte)goal.GoalType;

                switch (goal.GoalType) {
                    case GDMGoalType.gtIndividual:
                    case GDMGoalType.gtFamily:
                    case GDMGoalType.gtSource:
                        fView.Goal.Text = GKUtils.GetGoalStr(fBase.Context.Tree, goal.GoalType, fTempRec);
                        break;

                    case GDMGoalType.gtOther:
                        fView.Goal.Text = fTaskRecord.Goal;
                        break;
                }
            }

            fView.NotesList.ListModel.DataOwner = fTaskRecord;

            ChangeGoalType();
        }

        public void SelectGoal()
        {
            GDMGoalType gt = (GDMGoalType)fView.GoalType.SelectedIndex;
            switch (gt) {
                case GDMGoalType.gtIndividual:
                    fTempRec = fBase.Context.SelectPerson(null, TargetMode.tmNone, GDMSex.svUnknown);
                    fView.Goal.Text = GKUtils.GetGoalStr(fBase.Context.Tree, gt, fTempRec);
                    break;

                case GDMGoalType.gtFamily:
                    fTempRec = fBase.Context.SelectRecord(GDMRecordType.rtFamily, new object[0]);
                    fView.Goal.Text = GKUtils.GetGoalStr(fBase.Context.Tree, gt, fTempRec);
                    break;

                case GDMGoalType.gtSource:
                    fTempRec = fBase.Context.SelectRecord(GDMRecordType.rtSource, new object[0]);
                    fView.Goal.Text = GKUtils.GetGoalStr(fBase.Context.Tree, gt, fTempRec);
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

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LSID_WinTaskEdit);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.LSID_DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.LSID_DlgCancel);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.LSID_RPNotes);
            GetControl<ILabel>("lblGoal").Text = LangMan.LS(LSID.LSID_Goal);
            GetControl<ILabel>("lblPriority").Text = LangMan.LS(LSID.LSID_Priority);
            GetControl<ILabel>("lblStartDate").Text = LangMan.LS(LSID.LSID_StartDate);
            GetControl<ILabel>("lblStopDate").Text = LangMan.LS(LSID.LSID_StopDate);

            SetToolTip("btnGoalSelect", LangMan.LS(LSID.LSID_GoalSelectTip));
        }
    }
}
