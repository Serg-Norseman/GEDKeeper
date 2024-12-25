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
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Types;

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

            result.ResetDefaults();
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
