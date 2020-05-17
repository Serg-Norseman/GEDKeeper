/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using BSLib;
using GDModel;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;

namespace GKCore.Lists
{
    public enum ResearchColumnType
    {
        ctName,
        ctPriority,
        ctStatus,
        ctStartDate,
        ctStopDate,
        ctPercent,
        ctChangeDate
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class ResearchListMan : ListManager
    {
        private GDMResearchRecord fRec;


        public ResearchListMan(IBaseContext baseContext) :
            base(baseContext, CreateResearchListColumns(), GDMRecordType.rtResearch)
        {
        }

        public static ListColumns CreateResearchListColumns()
        {
            var result = new ListColumns();

            result.AddColumn(LSID.LSID_Title, DataType.dtString, 300, true, true);
            result.AddColumn(LSID.LSID_Priority, DataType.dtString, 90, true);
            result.AddColumn(LSID.LSID_Status, DataType.dtString, 90, true);
            result.AddColumn(LSID.LSID_StartDate, DataType.dtString, 90, true);
            result.AddColumn(LSID.LSID_StopDate, DataType.dtString, 90, true);
            result.AddColumn(LSID.LSID_Percent, DataType.dtInteger, 90, true);
            result.AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);

            result.ResetDefaults();
            return result;
        }

        public override bool CheckFilter()
        {
            bool res = (QuickFilter == "*" || IsMatchesMask(fRec.ResearchName, QuickFilter));

            res = res && CheckCommonFilter() && CheckExternalFilter(fRec);

            return res;
        }

        public override void Fetch(GDMRecord aRec)
        {
            fRec = (aRec as GDMResearchRecord);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((ResearchColumnType)colType)
            {
                case ResearchColumnType.ctName:
                    result = fRec.ResearchName;
                    break;

                case ResearchColumnType.ctPriority:
                    result = LangMan.LS(GKData.PriorityNames[(int)fRec.Priority]);
                    break;

                case ResearchColumnType.ctStatus:
                    result = LangMan.LS(GKData.StatusNames[(int)fRec.Status]);
                    break;

                case ResearchColumnType.ctStartDate:
                    result = GetDateValue(fRec.StartDate, isVisible);
                    break;

                case ResearchColumnType.ctStopDate:
                    result = GetDateValue(fRec.StopDate, isVisible);
                    break;

                case ResearchColumnType.ctPercent:
                    result = fRec.Percent;
                    break;

                case ResearchColumnType.ctChangeDate:
                    result = fRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class ResTasksSublistModel : ListModel
    {
        public ResTasksSublistModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete, RecordAction.raJump);

            fListColumns.AddColumn(LSID.LSID_Goal, 250, false);
            fListColumns.AddColumn(LSID.LSID_Priority, 90, false);
            fListColumns.AddColumn(LSID.LSID_StartDate, 90, false);
            fListColumns.AddColumn(LSID.LSID_StopDate, 90, false);
            fListColumns.ResetDefaults();
        }

        public override void UpdateContents()
        {
            var research = fDataOwner as GDMResearchRecord;
            if (fSheetList == null || research == null) return;

            try
            {
                fSheetList.BeginUpdate();
                fSheetList.ClearItems();

                foreach (GDMPointer taskPtr in research.Tasks)
                {
                    GDMTaskRecord task = taskPtr.Value as GDMTaskRecord;
                    if (task == null) continue;

                    fSheetList.AddItem(task, new object[] { GKUtils.GetTaskGoalStr(task),
                                           LangMan.LS(GKData.PriorityNames[(int)task.Priority]),
                                           new GDMDateItem(task.StartDate),
                                           new GDMDateItem(task.StopDate) });
                }

                fSheetList.EndUpdate();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("ResTasksSublistModel.UpdateContents(): " + ex.Message);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var research = fDataOwner as GDMResearchRecord;
            if (fBaseWin == null || fSheetList == null || research == null) return;

            GDMTaskRecord task = eArgs.ItemData as GDMTaskRecord;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    task = fBaseWin.Context.SelectRecord(GDMRecordType.rtTask, null) as GDMTaskRecord;
                    result = fUndoman.DoOrdinaryOperation(OperationType.otResearchTaskAdd, research, task);
                    break;

                case RecordAction.raEdit:
                    result = (task != null && BaseController.ModifyTask(fBaseWin, ref task));
                    break;

                case RecordAction.raDelete:
                    if (task != null && AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachTaskQuery)))
                    {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otResearchTaskRemove, research, task);
                    }
                    break;
            }

            if (result) {
                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class ResCommunicationsSublistModel : ListModel
    {
        public ResCommunicationsSublistModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete, RecordAction.raJump);

            fListColumns.AddColumn(LSID.LSID_Theme, 150, false);
            fListColumns.AddColumn(LSID.LSID_Corresponder, 150, false);
            fListColumns.AddColumn(LSID.LSID_Type, 90, false);
            fListColumns.AddColumn(LSID.LSID_Date, 90, false);
            fListColumns.ResetDefaults();
        }

        public override void UpdateContents()
        {
            var research = fDataOwner as GDMResearchRecord;
            if (fSheetList == null || research == null) return;

            try
            {
                fSheetList.BeginUpdate();
                fSheetList.ClearItems();

                foreach (GDMPointer commPtr in research.Communications)
                {
                    GDMCommunicationRecord corr = commPtr.Value as GDMCommunicationRecord;
                    if (corr == null) continue;

                    fSheetList.AddItem(corr, new object[] { corr.CommName,
                                           GKUtils.GetCorresponderStr(fBaseWin.Context.Tree, corr, false),
                                           LangMan.LS(GKData.CommunicationNames[(int)corr.CommunicationType]),
                                           new GDMDateItem(corr.Date) });
                }

                fSheetList.EndUpdate();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("ResCommunicationsSublistModel.UpdateContents(): " + ex.Message);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var research = fDataOwner as GDMResearchRecord;
            if (fBaseWin == null || fSheetList == null || research == null) return;

            GDMCommunicationRecord comm = eArgs.ItemData as GDMCommunicationRecord;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    comm = fBaseWin.Context.SelectRecord(GDMRecordType.rtCommunication, null) as GDMCommunicationRecord;
                    result = fUndoman.DoOrdinaryOperation(OperationType.otResearchCommunicationAdd, research, comm);
                    break;

                case RecordAction.raEdit:
                    result = (comm != null && BaseController.ModifyCommunication(fBaseWin, ref comm));
                    break;

                case RecordAction.raDelete:
                    if (comm != null && AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachCommunicationQuery)))
                    {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otResearchCommunicationRemove, research, comm);
                    }
                    break;
            }

            if (result) {
                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class ResGroupsSublistModel : ListModel
    {
        public ResGroupsSublistModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete, RecordAction.raJump);

            fListColumns.AddColumn(LSID.LSID_Group, 350, false);
            fListColumns.ResetDefaults();
        }

        public override void UpdateContents()
        {
            var research = fDataOwner as GDMResearchRecord;
            if (fSheetList == null || research == null) return;

            try
            {
                fSheetList.BeginUpdate();
                fSheetList.ClearItems();

                foreach (GDMPointer groupPtr in research.Groups)
                {
                    GDMGroupRecord grp = groupPtr.Value as GDMGroupRecord;
                    if (grp == null) continue;

                    fSheetList.AddItem(grp, new object[] { grp.GroupName });
                }

                fSheetList.EndUpdate();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("ResGroupsSublistModel.UpdateContents(): " + ex.Message);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var research = fDataOwner as GDMResearchRecord;
            if (fBaseWin == null || fSheetList == null || research == null) return;

            GDMGroupRecord group = eArgs.ItemData as GDMGroupRecord;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    group = fBaseWin.Context.SelectRecord(GDMRecordType.rtGroup, null) as GDMGroupRecord;
                    result = fUndoman.DoOrdinaryOperation(OperationType.otResearchGroupAdd, research, group);
                    break;

                case RecordAction.raDelete:
                    if (group != null && AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachGroupQuery)))
                    {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otResearchGroupRemove, research, group);
                    }
                    break;
            }

            if (result) {
                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
