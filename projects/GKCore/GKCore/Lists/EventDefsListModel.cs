/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Threading.Tasks;
using BSLib;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Events;
using GKCore.Locales;
using GKCore.Operations;
using GKCore.Options;

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
