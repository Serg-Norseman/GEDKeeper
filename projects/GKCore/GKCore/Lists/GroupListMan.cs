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
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GroupListMan : ListManager<GDMGroupRecord>
    {
        public enum ColumnType
        {
            ctXRefNum,
            ctName,
            ctChangeDate
        }


        private GDMGroupRecord fRec;


        public GroupListMan(IBaseContext baseContext) :
            base(baseContext, CreateGroupListColumns(), GDMRecordType.rtGroup)
        {
        }

        public static ListColumns<GDMGroupRecord> CreateGroupListColumns()
        {
            var result = new ListColumns<GDMGroupRecord>();

            result.AddColumn(LSID.LSID_NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.LSID_Group, DataType.dtString, 400, true, true);
            result.AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);

            result.ResetDefaults();
            return result;
        }

        public override bool CheckFilter()
        {
            bool res = IsMatchesMask(fRec.GroupName, QuickFilter);

            res = res && CheckCommonFilter() && CheckExternalFilter(fRec);

            return res;
        }

        public override void Fetch(GDMRecord aRec)
        {
            fRec = (GDMGroupRecord)aRec;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((ColumnType)colType) {
                case ColumnType.ctXRefNum:
                    result = fRec.GetId();
                    break;

                case ColumnType.ctName:
                    result = fRec.GroupName;
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
    public sealed class GroupMembersSublistModel : SheetModel<GDMIndividualLink>
    {
        public GroupMembersSublistModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raDelete /*, RecordAction.raJump*/);

            fListColumns.AddColumn(LSID.LSID_Name, 300, false);
            fListColumns.ResetDefaults();
        }

        public override void UpdateContents()
        {
            var grp = fDataOwner as GDMGroupRecord;
            if (fSheetList == null || grp == null) return;

            try {
                fSheetList.ListView.BeginUpdate();
                fSheetList.ListView.ClearItems();

                var tree = fBaseWin.Context.Tree;
                foreach (GDMIndividualLink ptrMember in grp.Members) {
                    GDMIndividualRecord member = tree.GetPtrValue(ptrMember);
                    if (member == null) continue;

                    fSheetList.ListView.AddItem(member, new object[] { GKUtils.GetNameString(member, true, false) });
                }

                fSheetList.ListView.EndUpdate();
            } catch (Exception ex) {
                Logger.WriteError("GroupMembersSublistModel.UpdateContents()", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var grp = fDataOwner as GDMGroupRecord;
            if (fBaseWin == null || fSheetList == null || grp == null) return;

            GDMIndividualRecord member = eArgs.ItemData as GDMIndividualRecord;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    member = fBaseWin.Context.SelectPerson(null, TargetMode.tmNone, GDMSex.svUnknown);
                    result = (member != null);
                    if (result) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otGroupMemberAttach, grp, member);
                    }
                    break;

                case RecordAction.raDelete:
                    result = (member != null && AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachMemberQuery)));
                    if (result) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otGroupMemberDetach, grp, member);
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
