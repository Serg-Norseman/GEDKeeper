/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017-2025 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using System.Reflection;
using GDModel;
using GKCore;
using GKCore.Calendar;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;
using GKCore.Plugins;

[assembly: AssemblyTitle("GKChroniclePlugin")]
[assembly: AssemblyDescription("GEDKeeper Chronicle plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2017-2025 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.1.0.0")]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

namespace GKChroniclePlugin
{
    public enum PLS
    {
        Chronicle = 1,
        Subject
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
        public EventsListModel(BaseContext baseContext, ILangMan langMan) : base(baseContext, CreateListColumns(langMan))
        {
        }

        public static ListColumns CreateListColumns(ILangMan langMan)
        {
            var result = new ListColumns(GKListType.ltNone);
            result.AddColumn(LangMan.LS(LSID.Date), 80, false);
            result.AddColumn(LangMan.LS(LSID.Event), 90, false);
            result.AddColumn(langMan.LS(PLS.Subject), 130, true);
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
                    string strPlace = (!evt.HasPlace) ? string.Empty : evt.Place.StringValue;
                    result = strPlace;
                    break;
                case 4:
                    result = GKUtils.GetEventCause(evt);
                    break;
            }
            return result;
        }
    }

    #endregion

    public sealed class Plugin : WidgetPlugin
    {
        private string fDisplayName = "GKChroniclePlugin";
        private ILangMan fLangMan;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Common; } }

        private ChronicleWidget fForm;

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                CloseForm();
            }
            base.Dispose(disposing);
        }

        internal void CloseForm()
        {
            if (fForm != null) {
                fForm = null;
            }
        }

        public override void Execute()
        {
            if (!Host.IsWidgetActive(this)) {
                fForm = new ChronicleWidget(this);
                fForm.Show();
            } else {
                fForm.Close();
            }
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.Chronicle);

                if (fForm != null) fForm.SetLocale();
            } catch (Exception ex) {
                Logger.WriteError("GKChroniclePlugin.OnLanguageChange()", ex);
            }
        }

        public override bool Shutdown()
        {
            bool result = true;
            try {
                CloseForm();
            } catch (Exception ex) {
                Logger.WriteError("GKChroniclePlugin.Shutdown()", ex);
                result = false;
            }
            return result;
        }

        public override void BaseChanged(IBaseWindow baseWin)
        {
            if (fForm != null) {
                fForm.BaseChanged(baseWin);
            }
        }

        private static List<EventRecord> CollectData(IBaseWindow baseWin)
        {
            List<EventRecord> result = new List<EventRecord>();
            if (baseWin == null)
                return result;

            var tree = baseWin.Context.Tree;
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                var rec = tree[i] as GDMRecordWithEvents;
                if (rec == null || !rec.HasEvents) continue;

                int eventsCount = rec.Events.Count;
                for (int k = 0; k < eventsCount; k++) {
                    GDMCustomEvent evt = rec.Events[k];
                    UDN udn = evt.Date.GetUDN();
                    if (!udn.IsEmpty()) {
                        result.Add(new EventRecord(evt, rec));
                    }
                }
            }

            return result;
        }

        internal void SetLVBase(IListView listView, IBaseWindow baseWin)
        {
            if (baseWin != null) {
                var eventsModel = new EventsListModel(baseWin.Context, fLangMan);
                listView.ListMan = eventsModel;
                eventsModel.DataSource = Plugin.CollectData(baseWin);
                listView.SortColumn = 0;
                listView.SortOrder = GKSortOrder.Ascending;
                listView.UpdateContents();
            } else {
                listView.ListMan.Clear();
                listView.UpdateContents();
            }
        }

        public override void BaseClosed(IBaseWindow baseWin)
        {
            if (fForm != null) {
                fForm.BaseChanged(null);
            }
        }
    }
}
