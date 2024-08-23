/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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

using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Options;
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
            base(baseContext, CreateListColumns(), GDMRecordType.rtResearch)
        {
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.rtResearch);

            result.AddColumn(LSID.NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.Title, DataType.dtString, 300, true, true);
            result.AddColumn(LSID.Priority, DataType.dtString, 90, true);
            result.AddColumn(LSID.Status, DataType.dtString, 90, true);
            result.AddColumn(LSID.StartDate, DataType.dtString, 90, true);
            result.AddColumn(LSID.StopDate, DataType.dtString, 90, true);
            result.AddColumn(LSID.Percent, DataType.dtInteger, 90, true);
            result.AddColumn(LSID.Changed, DataType.dtDateTime, 150, true);

            result.ResetDefaults();
            return result;
        }

        public override bool CheckFilter()
        {
            bool res = CheckQuickFilter(fFetchedRec.ResearchName);

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

        public ResTasksListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete, RecordAction.raJump);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stResearchTasks);

            result.AddColumn(LSID.Goal, 250, false);
            result.AddColumn(LSID.Priority, 90, false);
            result.AddColumn(LSID.StartDate, 90, false);
            result.AddColumn(LSID.StopDate, 90, false);

            result.ResetDefaults();
            return result;
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
            if (research != null)
                UpdateStructList(research.Tasks);
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var research = fDataOwner as GDMResearchRecord;
            if (fBaseWin == null || research == null) return;

            var task = fBaseContext.Tree.GetPtrValue<GDMTaskRecord>(eArgs.ItemData as GDMPointer);

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    task = await fBaseWin.Context.SelectRecord(fOwner, GDMRecordType.rtTask, null) as GDMTaskRecord;
                    if (task != null) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otResearchTaskAdd, research, task);
                    }
                    break;

                case RecordAction.raEdit:
                    if (task != null) {
                        var taskRes = await BaseController.ModifyTask(fOwner, fBaseWin, task);
                        result = taskRes.Result;
                    }
                    break;

                case RecordAction.raDelete:
                    if (task != null && await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachTaskQuery))) {
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

        public ResCommunicationsListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete, RecordAction.raJump);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stResearchCommunications);

            result.AddColumn(LSID.Theme, 150, false);
            result.AddColumn(LSID.Corresponder, 150, false);
            result.AddColumn(LSID.Type, 90, false);
            result.AddColumn(LSID.Date, 90, false);

            result.ResetDefaults();
            return result;
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
            if (research != null)
                UpdateStructList(research.Communications);
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var research = fDataOwner as GDMResearchRecord;
            if (fBaseWin == null || research == null) return;

            var comm = fBaseContext.Tree.GetPtrValue<GDMCommunicationRecord>(eArgs.ItemData as GDMPointer);

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    comm = await fBaseWin.Context.SelectRecord(fOwner, GDMRecordType.rtCommunication, null) as GDMCommunicationRecord;
                    if (comm != null) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otResearchCommunicationAdd, research, comm);
                    }
                    break;

                case RecordAction.raEdit:
                    if (comm != null) {
                        var commRes = await BaseController.ModifyCommunication(fOwner, fBaseWin, comm);
                        result = commRes.Result;
                    }
                    break;

                case RecordAction.raDelete:
                    if (comm != null && await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachCommunicationQuery))) {
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

        public ResGroupsListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete, RecordAction.raJump);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stResearchGroups);

            result.AddColumn(LSID.Group, 350, false);

            result.ResetDefaults();
            return result;
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
            if (research != null)
                UpdateStructList(research.Groups);
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var research = fDataOwner as GDMResearchRecord;
            if (fBaseWin == null || research == null) return;

            var group = fBaseContext.Tree.GetPtrValue<GDMGroupRecord>(eArgs.ItemData as GDMPointer);

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    group = await fBaseWin.Context.SelectRecord(fOwner, GDMRecordType.rtGroup, null) as GDMGroupRecord;
                    if (group != null) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otResearchGroupAdd, research, group);
                    }
                    break;

                case RecordAction.raDelete:
                    if (group != null && await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachGroupQuery))) {
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
