/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ResearchListModel : RecordsListModel<GDMResearchRecord>
    {
        public enum ColumnType
        {
            ctXRefNum,
            ctName,
            ctPriority,
            ctStatus,
            ctStartDate,
            ctStopDate,
            ctPercent,
            ctChangeDate
        }


        public ResearchListModel(IBaseContext baseContext) :
            base(baseContext, CreateResearchListColumns(), GDMRecordType.rtResearch)
        {
        }

        public static ListColumns<GDMResearchRecord> CreateResearchListColumns()
        {
            var result = new ListColumns<GDMResearchRecord>();

            result.AddColumn(LSID.LSID_NumberSym, DataType.dtInteger, 50, true);
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
            bool res = IsMatchesMask(fFetchedRec.ResearchName, QuickFilter);

            res = res && CheckCommonFilter() && CheckExternalFilter(fFetchedRec);

            return res;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((ColumnType)colType) {
                case ColumnType.ctXRefNum:
                    result = fFetchedRec.GetId();
                    break;

                case ColumnType.ctName:
                    result = fFetchedRec.ResearchName;
                    break;

                case ColumnType.ctPriority:
                    result = LangMan.LS(GKData.PriorityNames[(int)fFetchedRec.Priority]);
                    break;

                case ColumnType.ctStatus:
                    result = LangMan.LS(GKData.StatusNames[(int)fFetchedRec.Status]);
                    break;

                case ColumnType.ctStartDate:
                    result = GetDateValue(fFetchedRec.StartDate, isVisible);
                    break;

                case ColumnType.ctStopDate:
                    result = GetDateValue(fFetchedRec.StopDate, isVisible);
                    break;

                case ColumnType.ctPercent:
                    result = fFetchedRec.Percent;
                    break;

                case ColumnType.ctChangeDate:
                    result = fFetchedRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class ResTasksListModel : SheetModel<GDMPointer>
    {
        private GDMTaskRecord fTaskRec;

        public ResTasksListModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete, RecordAction.raJump);

            fListColumns.AddColumn(LSID.LSID_Goal, 250, false);
            fListColumns.AddColumn(LSID.LSID_Priority, 90, false);
            fListColumns.AddColumn(LSID.LSID_StartDate, 90, false);
            fListColumns.AddColumn(LSID.LSID_StopDate, 90, false);
            fListColumns.ResetDefaults();
        }

        public override void Fetch(GDMPointer aRec)
        {
            base.Fetch(aRec);
            fTaskRec = fBaseContext.Tree.GetPtrValue<GDMTaskRecord>(fFetchedRec);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = GKUtils.GetTaskGoalStr(fBaseContext.Tree, fTaskRec);
                    break;
                case 1:
                    result = LangMan.LS(GKData.PriorityNames[(int)fTaskRec.Priority]);
                    break;
                case 2:
                    result = new GDMDateItem(fTaskRec.StartDate);
                    break;
                case 3:
                    result = new GDMDateItem(fTaskRec.StopDate);
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var research = fDataOwner as GDMResearchRecord;
            if (research == null) return;

            try {
                UpdateStructList(research.Tasks);
            } catch (Exception ex) {
                Logger.WriteError("ResTasksListModel.UpdateContents()", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var research = fDataOwner as GDMResearchRecord;
            if (fBaseWin == null || research == null) return;

            var task = fBaseContext.Tree.GetPtrValue<GDMTaskRecord>(eArgs.ItemData as GDMPointer);

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    task = fBaseWin.Context.SelectRecord(GDMRecordType.rtTask, null) as GDMTaskRecord;
                    if (task != null) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otResearchTaskAdd, research, task);
                    }
                    break;

                case RecordAction.raEdit:
                    result = (task != null && BaseController.ModifyTask(fBaseWin, ref task));
                    break;

                case RecordAction.raDelete:
                    if (task != null && AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachTaskQuery))) {
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
    public sealed class ResCommunicationsListModel : SheetModel<GDMPointer>
    {
        private GDMCommunicationRecord fCommRec;

        public ResCommunicationsListModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete, RecordAction.raJump);

            fListColumns.AddColumn(LSID.LSID_Theme, 150, false);
            fListColumns.AddColumn(LSID.LSID_Corresponder, 150, false);
            fListColumns.AddColumn(LSID.LSID_Type, 90, false);
            fListColumns.AddColumn(LSID.LSID_Date, 90, false);
            fListColumns.ResetDefaults();
        }

        public override void Fetch(GDMPointer aRec)
        {
            base.Fetch(aRec);
            fCommRec = fBaseContext.Tree.GetPtrValue<GDMCommunicationRecord>(fFetchedRec);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fCommRec.CommName;
                    break;
                case 1:
                    result = GKUtils.GetCorresponderStr(fBaseWin.Context.Tree, fCommRec, false);
                    break;
                case 2:
                    result = LangMan.LS(GKData.CommunicationNames[(int)fCommRec.CommunicationType]);
                    break;
                case 3:
                    result = new GDMDateItem(fCommRec.Date);
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var research = fDataOwner as GDMResearchRecord;
            if (research == null) return;

            try {
                UpdateStructList(research.Communications);
            } catch (Exception ex) {
                Logger.WriteError("ResCommunicationsListModel.UpdateContents()", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var research = fDataOwner as GDMResearchRecord;
            if (fBaseWin == null || research == null) return;

            var comm = fBaseContext.Tree.GetPtrValue<GDMCommunicationRecord>(eArgs.ItemData as GDMPointer);

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    comm = fBaseWin.Context.SelectRecord(GDMRecordType.rtCommunication, null) as GDMCommunicationRecord;
                    if (comm != null) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otResearchCommunicationAdd, research, comm);
                    }
                    break;

                case RecordAction.raEdit:
                    result = (comm != null && BaseController.ModifyCommunication(fBaseWin, ref comm));
                    break;

                case RecordAction.raDelete:
                    if (comm != null && AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachCommunicationQuery))) {
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
    public sealed class ResGroupsListModel : SheetModel<GDMPointer>
    {
        private GDMGroupRecord fGroupRec;

        public ResGroupsListModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete, RecordAction.raJump);

            fListColumns.AddColumn(LSID.LSID_Group, 350, false);
            fListColumns.ResetDefaults();
        }

        public override void Fetch(GDMPointer aRec)
        {
            base.Fetch(aRec);
            fGroupRec = fBaseContext.Tree.GetPtrValue<GDMGroupRecord>(fFetchedRec);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fGroupRec.GroupName;
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var research = fDataOwner as GDMResearchRecord;
            if (research == null) return;

            try {
                UpdateStructList(research.Groups);
            } catch (Exception ex) {
                Logger.WriteError("ResGroupsListModel.UpdateContents()", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var research = fDataOwner as GDMResearchRecord;
            if (fBaseWin == null || research == null) return;

            var group = fBaseContext.Tree.GetPtrValue<GDMGroupRecord>(eArgs.ItemData as GDMPointer);

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    group = fBaseWin.Context.SelectRecord(GDMRecordType.rtGroup, null) as GDMGroupRecord;
                    if (group != null) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otResearchGroupAdd, research, group);
                    }
                    break;

                case RecordAction.raDelete:
                    if (group != null && AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachGroupQuery))) {
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
