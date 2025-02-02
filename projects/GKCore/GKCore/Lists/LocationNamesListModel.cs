/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
    public sealed class LocationNamesListModel : SheetModel<GDMLocationName>
    {
        public LocationNamesListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete, RecordAction.raCopy, RecordAction.raPaste);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stLocationNames);

            result.AddColumn(LSID.NumberSym, 25, false);
            result.AddColumn(LSID.GeneralName, 300, false);
            result.AddColumn(LSID.ShortTitle, 80, false);
            result.AddColumn(LSID.Date, 160, false);

            result.ResetDefaults();
            return result;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fStructList.IndexOf(fFetchedRec) + 1;
                    break;
                case 1:
                    result = fFetchedRec.StringValue;
                    break;
                case 2:
                    result = fFetchedRec.Abbreviation;
                    break;
                case 3:
                    result = new GDMDateItem(fFetchedRec.Date.Value);
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var dataOwner = fDataOwner as GDMLocationRecord;
            if (dataOwner != null)
                UpdateStructList(dataOwner.Names);
        }

        private void UpdateButtons()
        {
            var actions = AllowedActions;

            if (fStructList.Count <= 1) {
                actions.Exclude(RecordAction.raDelete);
            } else {
                actions.Include(RecordAction.raDelete);
            }

            AllowedActions = actions;
        }

        public override void OnItemSelected(int itemIndex, object rowData)
        {
            UpdateButtons();
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var dataOwner = fDataOwner as GDMLocationRecord;
            if (fBaseWin == null || dataOwner == null) return;

            var locName = eArgs.ItemData as GDMLocationName;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit: {
                        var locNameRes = await BaseController.ModifyLocationName(fOwner, fBaseWin, fUndoman, dataOwner, locName);
                        locName = locNameRes.Record;
                        result = locNameRes.Result;

                        if (result) {
                            if (!dataOwner.ValidateNames()) {
                                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.PeriodsOverlap));
                            }
                            dataOwner.SortNames();
                        }
                    }
                    break;

                case RecordAction.raDelete:
                    if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.RemoveNameQuery))) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otLocationNameRemove, dataOwner, locName);
                    }
                    break;

                case RecordAction.raCopy:
                    AppHost.Instance.SetClipboardObj<GDMLocationName>(locName);
                    break;

                case RecordAction.raCut:
                    break;

                case RecordAction.raPaste:
                    locName = AppHost.Instance.GetClipboardObj<GDMLocationName>();
                    if (locName != null) {
                        locName = locName.Clone();
                        result = fUndoman.DoOrdinaryOperation(OperationType.otLocationNameAdd, dataOwner, locName);
                    }
                    break;
            }

            if (result) {
                UpdateButtons();

                if (eArgs.Action == RecordAction.raAdd || eArgs.Action == RecordAction.raPaste) {
                    eArgs.ItemData = locName;
                }

                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
