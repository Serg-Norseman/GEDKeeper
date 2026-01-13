/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using GDModel;
using GKCore.Calendar;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;

namespace GKCore.Controllers
{
    public class ChronicleController : DialogController<IChronicleWin>
    {
        public ChronicleController(IChronicleWin view) : base(view)
        {
        }

        public override void UpdateView()
        {
            var lvEvents = GetControl<IListView>("lvEvents");
            SetLVBase(lvEvents, fBase);
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.MIChronicle));
        }

        private static List<EventRecord> CollectData(IBaseWindow baseWin)
        {
            List<EventRecord> result = new List<EventRecord>();
            if (baseWin == null)
                return result;

            var tree = baseWin.Context.Tree;
            for (int i = 0, num = tree.RecordsCount; i < num; i++) {
                var rec = tree[i] as GDMRecordWithEvents;
                if (rec == null || !rec.HasEvents) continue;

                for (int k = 0, eventsCount = rec.Events.Count; k < eventsCount; k++) {
                    GDMCustomEvent evt = rec.Events[k];
                    UDN udn = evt.Date.GetUDN();
                    if (!udn.IsEmpty()) {
                        result.Add(new EventRecord(evt, rec));
                    }
                }
            }

            return result;
        }

        public void SetLVBase(IListView listView, IBaseWindow baseWin)
        {
            if (baseWin != null) {
                var eventsModel = new EventsListModel(baseWin.Context);
                listView.ListMan = eventsModel;
                eventsModel.DataSource = CollectData(baseWin);
                listView.SortColumn = 0;
                listView.SortOrder = GKSortOrder.Ascending;
                listView.UpdateContents();
            } else {
                listView.ListMan.Clear();
                listView.UpdateContents();
            }
        }

        #region Internals

        internal class EventRecord
        {
            public readonly GDMCustomEvent Event;
            public readonly GDMRecordWithEvents Record;

            public EventRecord(GDMCustomEvent evt, GDMRecordWithEvents record)
            {
                Event = evt;
                Record = record;
            }
        }

        internal sealed class EventsListModel : SimpleListModel<EventRecord>
        {
            public EventsListModel(BaseContext baseContext) : base(baseContext, CreateListColumns())
            {
            }

            public static ListColumns CreateListColumns()
            {
                var result = new ListColumns(GKListType.ltNone);
                result.AddColumn(LangMan.LS(LSID.Date), DataType.dtGEDCOMDate, 100, true);
                result.AddColumn(LangMan.LS(LSID.Event), 100, true);
                result.AddColumn(LangMan.LS(LSID.Subject), 160, true);
                result.AddColumn(LangMan.LS(LSID.Place), 200, false);
                result.AddColumn(LangMan.LS(LSID.Cause), 130, false);
                return result;
            }

            protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
            {
                GDMCustomEvent evt = fFetchedRec.Event;

                object result = null;
                switch (colType) {
                    case 0:
                        result = new GDMDateItem(evt.Date.Value);
                        break;
                    case 1:
                        result = GKUtils.GetEventName(evt);
                        break;
                    case 2:
                        result = GKUtils.GetRecordName(fBaseContext.Tree, fFetchedRec.Record, false);
                        break;
                    case 3:
                        result = (!evt.HasPlace) ? string.Empty : evt.Place.StringValue;
                        break;
                    case 4:
                        result = GKUtils.GetEventCause(evt);
                        break;
                }
                return result;
            }
        }

        #endregion
    }
}
