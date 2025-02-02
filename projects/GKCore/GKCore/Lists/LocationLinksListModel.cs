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
    public sealed class LocationLinksListModel : SheetModel<GDMLocationLink>
    {
        private GDMLocationRecord fRelLocation;

        public LocationLinksListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete, RecordAction.raCopy, RecordAction.raPaste);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stLocationLinks);

            result.AddColumn(LSID.NumberSym, 25, false);
            result.AddColumn(LSID.GeneralName, 300, false);
            result.AddColumn(LSID.Date, 160, false);

            result.ResetDefaults();
            return result;
        }

        public override void Fetch(GDMLocationLink aRec)
        {
            base.Fetch(aRec);
            fRelLocation = fBaseContext.Tree.GetPtrValue<GDMLocationRecord>(fFetchedRec);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fStructList.IndexOf(fFetchedRec) + 1;
                    break;
                case 1:
                    result = (fRelLocation == null) ? string.Empty : fRelLocation.GetNameByDate(fFetchedRec.Date.Value, true);
                    break;
                case 2:
                    result = new GDMDateItem(fFetchedRec.Date.Value);
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var dataOwner = fDataOwner as GDMLocationRecord;
            if (dataOwner != null)
                UpdateStructList(dataOwner.TopLevels);
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var dataOwner = fDataOwner as GDMLocationRecord;
            if (fBaseWin == null || dataOwner == null) return;

            var locLink = eArgs.ItemData as GDMLocationLink;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit: {
                        var locLinkRes = await BaseController.ModifyLocationLink(fOwner, fBaseWin, fUndoman, dataOwner, locLink);
                        locLink = locLinkRes.Record;
                        result = locLinkRes.Result;

                        if (result) {
                            if (!dataOwner.ValidateLinks()) {
                                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.PeriodsOverlap));
                            }
                            dataOwner.SortTopLevels();
                        }
                    }
                    break;

                case RecordAction.raDelete:
                    if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.RemoveTopLevelLinkQuery))) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otLocationLinkRemove, dataOwner, locLink);
                    }
                    break;

                case RecordAction.raCopy:
                    AppHost.Instance.SetClipboardObj<GDMLocationLink>(locLink);
                    break;

                case RecordAction.raCut:
                    break;

                case RecordAction.raPaste:
                    locLink = AppHost.Instance.GetClipboardObj<GDMLocationLink>();
                    if (locLink != null) {
                        locLink = locLink.Clone();
                        result = fUndoman.DoOrdinaryOperation(OperationType.otLocationLinkAdd, dataOwner, locLink);
                    }
                    break;
            }

            if (result) {
                if (eArgs.Action == RecordAction.raAdd || eArgs.Action == RecordAction.raPaste) {
                    eArgs.ItemData = locLink;
                }

                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
