/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Operations;
using GKCore.Options;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GroupListModel : RecordsListModel<GDMGroupRecord>
    {
        public enum ColumnType
        {
            ctXRefNum,
            ctName,
            ctChangeDate
        }


        public GroupListModel(BaseContext baseContext) :
            base(baseContext, CreateListColumns(), GDMRecordType.rtGroup)
        {
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.rtGroup);
            result.AddColumn(LSID.NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.Group, DataType.dtString, 400, true);
            result.AddColumn(LSID.Changed, DataType.dtDateTime, 150, true);
            return result;
        }

        protected override string GetQuickFilterBuffer()
        {
            return fFetchedRec.GroupName;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((ColumnType)colType) {
                case ColumnType.ctXRefNum:
                    result = fFetchedRec.GetId();
                    break;

                case ColumnType.ctName:
                    result = fFetchedRec.GroupName;
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
    public sealed class GroupMembersListModel : SheetModel<GDMIndividualLink>
    {
        private GDMIndividualRecord fMember;

        public GroupMembersListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raDelete, RecordAction.raJump);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stGroupMembers);
            result.AddColumn(LSID.GeneralName, 300, false);
            return result;
        }

        public override void Fetch(GDMIndividualLink aRec)
        {
            base.Fetch(aRec);
            fMember = fBaseContext.Tree.GetPtrValue(fFetchedRec);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = GKUtils.GetNameString(fMember, false);
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var grp = fDataOwner as GDMGroupRecord;
            if (grp != null)
                UpdateStructList(grp.Members);
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var grp = fDataOwner as GDMGroupRecord;
            if (fBaseWin == null || grp == null) return;

            var member = fBaseContext.Tree.GetPtrValue(eArgs.ItemData as GDMIndividualLink);

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    member = await BaseController.SelectPerson(fOwner, fBaseWin, null, TargetMode.tmNone, GDMSex.svUnknown);
                    if (member != null) {
                        if (grp.IndexOfMember(member) >= 0) {
                            AppHost.StdDialogs.ShowAlert(LangMan.LS(LSID.InvalidLink));
                            return;
                        }

                        result = fUndoman.DoOrdinaryOperation(OperationType.otGroupMemberAttach, grp, member);
                    }
                    break;

                case RecordAction.raDelete:
                    if (member != null && await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachMemberQuery))) {
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
