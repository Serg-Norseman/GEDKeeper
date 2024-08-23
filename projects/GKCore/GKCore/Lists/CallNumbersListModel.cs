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
    public sealed class CallNumbersListModel : SheetModel<GDMSourceCallNumber>
    {
        public CallNumbersListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stRepoCitCallNumbers);

            result.AddColumn(LSID.CallNumber, 280, false);
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
                    result = LangMan.LS(GKData.MediaTypes[(int)fFetchedRec.MediaType]);
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var repoCit = fDataOwner as GDMRepositoryCitation;
            if (repoCit != null)
                UpdateStructList(repoCit.CallNumbers);
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var repoCit = fDataOwner as GDMRepositoryCitation;
            if (fBaseWin == null || repoCit == null) return;

            var callNum = eArgs.ItemData as GDMSourceCallNumber;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit: {
                        var callNumRes = await BaseController.ModifyCallNumber(fOwner, fBaseWin, fUndoman, repoCit, callNum);
                        callNum = callNumRes.Record;
                        result = callNumRes.Result;
                    }
                    break;

                case RecordAction.raDelete:
                    if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.RemoveCallNumberQuery, callNum.StringValue))) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otCallNumberRemove, repoCit, callNum);
                    }
                    break;
            }

            if (result) {
                if (eArgs.Action == RecordAction.raAdd) {
                    eArgs.ItemData = callNum;
                }

                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
