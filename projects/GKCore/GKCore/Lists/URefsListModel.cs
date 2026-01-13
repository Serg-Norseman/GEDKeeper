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
    public sealed class URefsListModel : SheetModel<GDMUserReference>
    {
        public URefsListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stUserRefs);
            result.AddColumn(LSID.Reference, 300, false);
            result.AddColumn(LSID.Type, 200, false);
            return result;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fFetchedRec.StringValue;
                    break;
                case 1:
                    result = fFetchedRec.ReferenceType;
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var iRec = fDataOwner as GDMRecord;
            if (iRec != null)
                UpdateStructList(iRec.UserReferences);
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var iRec = fDataOwner as GDMRecord;
            if (fBaseWin == null || iRec == null) return;

            GDMUserReference userRef = eArgs.ItemData as GDMUserReference;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit: {
                        bool exists = (userRef != null);
                        if (!exists) {
                            userRef = new GDMUserReference();
                        }

                        using (var dlg = AppHost.ResolveDialog<IUserRefEditDlg>(fBaseWin)) {
                            dlg.UserReference = userRef;
                            result = await AppHost.Instance.ShowModalAsync(dlg, fOwner, false);
                        }

                        if (!exists) {
                            if (result) {
                                result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualURefAdd, iRec, userRef);
                            } else {
                                userRef.Dispose();
                            }
                        }
                    }
                    break;

                case RecordAction.raDelete: {
                        string confirmation = !string.IsNullOrEmpty(userRef.StringValue) ? userRef.StringValue : userRef.ReferenceType;
                        if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.RemoveUserRefQuery, confirmation))) {
                            result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualURefRemove, iRec, userRef);
                        }
                        break;
                    }
            }

            if (result) {
                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
