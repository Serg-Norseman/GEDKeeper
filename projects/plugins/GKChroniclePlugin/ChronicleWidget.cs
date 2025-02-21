/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;

namespace GKChroniclePlugin
{
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

                fEvents = fPlugin.CollectData(fBase);
                UpdateControls();
            }
        }

        private void UpdateControls()
        {
            try {
                lvEvents.BeginUpdate();

                lvEvents.Clear();
                lvEvents.AddColumn(LangMan.LS(LSID.Date), 80, false);
                lvEvents.AddColumn(LangMan.LS(LSID.Event), 90, false);
                lvEvents.AddColumn(fPlugin.LangMan.LS(PLS.Subject), 130, false);
                lvEvents.AddColumn(LangMan.LS(LSID.Place), 200, false);
                lvEvents.AddColumn(LangMan.LS(LSID.Cause), 130, false);

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

        public void SetLocale()
        {
            Text = fPlugin.LangMan.LS(PLS.Chronicle);
        }
    }
}
