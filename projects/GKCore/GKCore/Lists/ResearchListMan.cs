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
    public sealed class ResearchListMan : ListManager<GDMResearchRecord>
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


        private GDMResearchRecord fRec;


        public ResearchListMan(IBaseContext baseContext) :
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
            bool res = IsMatchesMask(fRec.ResearchName, QuickFilter);

            res = res && CheckCommonFilter() && CheckExternalFilter(fRec);

            return res;
        }

        public override void Fetch(GDMRecord aRec)
        {
            fRec = (GDMResearchRecord)aRec;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((ColumnType)colType) {
                case ColumnType.ctXRefNum:
                    result = fRec.GetId();
                    break;

                case ColumnType.ctName:
                    result = fRec.ResearchName;
                    break;

                case ColumnType.ctPriority:
                    result = LangMan.LS(GKData.PriorityNames[(int)fRec.Priority]);
                    break;

                case ColumnType.ctStatus:
                    result = LangMan.LS(GKData.StatusNames[(int)fRec.Status]);
                    break;

                case ColumnType.ctStartDate:
                    result = GetDateValue(fRec.StartDate, isVisible);
                    break;

                case ColumnType.ctStopDate:
                    result = GetDateValue(fRec.StopDate, isVisible);
                    break;

                case ColumnType.ctPercent:
                    result = fRec.Percent;
                    break;

                case ColumnType.ctChangeDate:
                    result = fRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class ResTasksSublistModel : SheetModel<GDMPointer>
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

            try {
                fSheetList.ListView.BeginUpdate();
                fSheetList.ListView.ClearItems();

                foreach (GDMPointer taskPtr in research.Tasks) {
                    var task = fBaseContext.Tree.GetPtrValue<GDMTaskRecord>(taskPtr);
                    if (task == null) continue;

                    fSheetList.ListView.AddItem(task, new object[] { GKUtils.GetTaskGoalStr(fBaseContext.Tree, task),
                        LangMan.LS(GKData.PriorityNames[(int)task.Priority]),
                        new GDMDateItem(task.StartDate),
                        new GDMDateItem(task.StopDate)
                    });
                }

                fSheetList.ListView.EndUpdate();
            } catch (Exception ex) {
                Logger.WriteError("ResTasksSublistModel.UpdateContents()", ex);
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
    public sealed class ResCommunicationsSublistModel : SheetModel<GDMPointer>
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

            try {
                fSheetList.ListView.BeginUpdate();
                fSheetList.ListView.ClearItems();

                foreach (GDMPointer commPtr in research.Communications) {
                    var corr = fBaseContext.Tree.GetPtrValue<GDMCommunicationRecord>(commPtr);
                    if (corr == null) continue;

                    fSheetList.ListView.AddItem(corr, new object[] { corr.CommName,
                        GKUtils.GetCorresponderStr(fBaseWin.Context.Tree, corr, false),
                        LangMan.LS(GKData.CommunicationNames[(int)corr.CommunicationType]),
                        new GDMDateItem(corr.Date)
                    });
                }

                fSheetList.ListView.EndUpdate();
            } catch (Exception ex) {
                Logger.WriteError("ResCommunicationsSublistModel.UpdateContents()", ex);
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
    public sealed class ResGroupsSublistModel : SheetModel<GDMPointer>
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

            try {
                fSheetList.ListView.BeginUpdate();
                fSheetList.ListView.ClearItems();

                foreach (GDMPointer groupPtr in research.Groups) {
                    var grp = fBaseContext.Tree.GetPtrValue<GDMGroupRecord>(groupPtr);
                    if (grp == null) continue;

                    fSheetList.ListView.AddItem(grp, new object[] { grp.GroupName });
                }

                fSheetList.ListView.EndUpdate();
            } catch (Exception ex) {
                Logger.WriteError("ResGroupsSublistModel.UpdateContents()", ex);
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
