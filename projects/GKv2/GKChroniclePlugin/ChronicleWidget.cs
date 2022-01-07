/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;
using BSLib.Calendar;
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;

namespace GKChroniclePlugin
{
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

    /// <summary>
    /// 
    /// </summary>
    public partial class ChronicleWidget : Form, ILocalizable
    {
        private readonly Plugin fPlugin;
        private IBaseWindow fBase;
        private List<EventRecord> fEvents;

        public ChronicleWidget(Plugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;
            fEvents = new List<EventRecord>();
            lvEvents.Clear();

            SetLocale();
        }

        private void CalcWidget_Load(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetShow(fPlugin);
            BaseChanged(fPlugin.Host.GetCurrentFile());
        }

        private void CalcWidget_Closed(object sender, EventArgs e)
        {
            BaseChanged(null);
            fPlugin.Host.WidgetClose(fPlugin);
        }

        public void BaseChanged(IBaseWindow baseWin)
        {
            if (fBase != baseWin) {
                fBase = baseWin;

                CollectData();
                UpdateControls();
            }
        }

        private void CollectData()
        {
            fEvents.Clear();
            if (fBase == null) return;

            int num = fBase.Context.Tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                var rec = fBase.Context.Tree[i] as GDMRecordWithEvents;
                if (rec == null || !rec.HasEvents) continue;

                int eventsCount = rec.Events.Count;
                for (int k = 0; k < eventsCount; k++) {
                    GDMCustomEvent evt = rec.Events[k];
                    UDN udn = evt.Date.GetUDN();
                    if (!udn.IsEmpty()) {
                        fEvents.Add(new EventRecord(evt, rec));
                    }
                }
            }
        }

        private void UpdateControls()
        {
            try {
                lvEvents.BeginUpdate();

                lvEvents.Clear();
                lvEvents.AddColumn(fPlugin.LangMan.LS(CLS.LSID_Date), 80, false);
                lvEvents.AddColumn(fPlugin.LangMan.LS(CLS.LSID_Event), 90, false);
                lvEvents.AddColumn(fPlugin.LangMan.LS(CLS.LSID_Description), 130, false);
                lvEvents.AddColumn(fPlugin.LangMan.LS(CLS.LSID_Place), 200, false);
                lvEvents.AddColumn(fPlugin.LangMan.LS(CLS.LSID_Cause), 130, false);

                for (int i = 0; i < fEvents.Count; i++) {
                    EventRecord eventRec = fEvents[i];
                    GDMCustomEvent evt = eventRec.Event;
                    string strPlace = (!evt.HasPlace) ? string.Empty : evt.Place.StringValue;

                    lvEvents.AddItem(eventRec, new object[] {
                        new GDMDateItem(evt.Date.Value),
                        GKUtils.GetEventName(evt),
                        GKUtils.GetRecordName(fBase.Context.Tree, eventRec.Record, false),
                        strPlace,
                        GKUtils.GetEventCause(evt)
                    });
                }

                lvEvents.ResizeColumn(0);
            } finally {
                lvEvents.EndUpdate();
            }
        }

        #region ILocalizable support

        public void SetLocale()
        {
            Text = fPlugin.LangMan.LS(CLS.LSID_Title);
        }

        #endregion
    }
}
