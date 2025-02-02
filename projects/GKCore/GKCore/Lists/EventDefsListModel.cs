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
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Lists
{
    public sealed class EventDefsListModel : SheetModel<EventDef>
    {
        public EventDefsListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stEventDefs);

            result.AddColumn(LSID.GeneralName, 300, false);
            result.AddColumn(LSID.Enabled, 100, false);
            result.AddColumn(LSID.Record, 300, false);

            result.ResetDefaults();
            return result;
        }

        private static string GetTargetStr(EventTarget target)
        {
            switch (target) {
                case EventTarget.etIndividual:
                    return LangMan.LS(LSID.Person);

                case EventTarget.etFamily:
                    return LangMan.LS(LSID.Family);

                case EventTarget.etAny:
                    return LangMan.LS(LSID.Person) + ", " + LangMan.LS(LSID.Family);
            }

            return string.Empty;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fFetchedRec.DisplayName;
                    break;
                case 1:
                    result = fFetchedRec.Enabled ? GKData.CHECK_MARK : GKData.CROSS_MARK;
                    break;
                case 2:
                    result = GetTargetStr(fFetchedRec.Target);
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var dataOwner = fDataOwner as EventDefinitions;
            if (dataOwner != null)
                UpdateStructList(dataOwner.List);
        }

        public override void OnItemSelected(int itemIndex, object rowData)
        {
            var eventDef = rowData as EventDef;

            var actions = AllowedActions;

            if (eventDef.Protected) {
                actions.Exclude(RecordAction.raDelete);
            } else {
                actions.Include(RecordAction.raDelete);
            }

            AllowedActions = actions;
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var dataOwner = fDataOwner as EventDefinitions;
            if (dataOwner == null) return;

            var eventDef = eArgs.ItemData as EventDef;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit: {
                        var defRes = await BaseController.ModifyEventDef(fOwner, dataOwner.List, eventDef);
                        eventDef = defRes.Record;
                        result = defRes.Result;
                    }
                    break;

                case RecordAction.raDelete:
                    if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.RemoveEventQuery))) {
                        dataOwner.List.Remove(eventDef);
                        result = true;
                    }
                    break;
            }

            if (result) {
                if (eArgs.Action == RecordAction.raAdd) {
                    eArgs.ItemData = eventDef;
                }
                eArgs.IsChanged = true;
            }
        }
    }
}
