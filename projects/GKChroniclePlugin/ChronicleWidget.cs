/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCommon;
using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;

namespace GKChroniclePlugin
{
    internal class EventRecord
    {
        public readonly GEDCOMCustomEvent Event;
        public readonly GEDCOMRecordWithEvents Record;

        public EventRecord(GEDCOMCustomEvent evt, GEDCOMRecordWithEvents record)
        {
            Event = evt;
            Record = record;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public partial class ChronicleWidget : Form, ILocalization
    {
        private readonly Plugin fPlugin;
        private IBaseWindow fBase;
        private List<EventRecord> fEvents;

        public ChronicleWidget(Plugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;

            //Screen scr = Screen.PrimaryScreen;
            //Location = new Point(scr.WorkingArea.Width - Width - 10, scr.WorkingArea.Height - Height - 10);

            fEvents = new List<EventRecord>();
            lvEvents.Clear();

            SetLang();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
            }
            base.Dispose(disposing);
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
            if (fBase != baseWin && fBase != null)
            {
            }

            fBase = baseWin;
            fEvents.Clear();

            if (fBase != null)
            {
                CollectData();
            }

            UpdateControls();
        }

        private void CollectData()
        {
            int num = fBase.Tree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecordWithEvents rec = fBase.Tree[i] as GEDCOMRecordWithEvents;
                if (rec == null) continue;

                int eventsCount = rec.Events.Count;
                for (int k = 0; k < eventsCount; k++)
                {
                    GEDCOMCustomEvent evt = rec.Events[k];
                    UDN udn = evt.Date.GetUDN();
                    if (!udn.IsEmpty()) {
                        fEvents.Add(new EventRecord(evt, rec));
                    }
                }
            }
        }

        private void UpdateControls()
        {
            try
            {
                lvEvents.BeginUpdate();

                lvEvents.Clear();
                lvEvents.AddColumn(fPlugin.LangMan.LS(CLS.LSID_Date), 80, false);
                lvEvents.AddColumn(fPlugin.LangMan.LS(CLS.LSID_Event), 90, false);
                lvEvents.AddColumn(fPlugin.LangMan.LS(CLS.LSID_Description), 130, false);
                lvEvents.AddColumn(fPlugin.LangMan.LS(CLS.LSID_Place), 200, false);
                lvEvents.AddColumn(fPlugin.LangMan.LS(CLS.LSID_Cause), 130, false);

                for (int i = 0; i < fEvents.Count; i++)
                {
                    EventRecord eventRec = fEvents[i];
                    GEDCOMCustomEvent evt = eventRec.Event;

                    GKListItem item = lvEvents.AddItem(new GEDCOMDateItem(evt.Date.Value), eventRec);
                    item.AddSubItem(GKUtils.GetEventName(evt));
                    item.AddSubItem(GKUtils.GetRecordName(eventRec.Record, false));
                    item.AddSubItem(evt.Place.StringValue);
                    item.AddSubItem(GKUtils.GetEventCause(evt));
                }

                lvEvents.ResizeColumn(0);
            }
            finally
            {
                lvEvents.EndUpdate();
            }
        }

        #region ILocalization support

        public void SetLang()
        {
            Text = fPlugin.LangMan.LS(CLS.LSID_Title);
        }

        #endregion
    }
}
