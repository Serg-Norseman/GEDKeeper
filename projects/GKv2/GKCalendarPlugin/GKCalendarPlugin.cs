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
using System.Drawing;
using System.IO;
using System.Reflection;
using BSLib.Design.Graphics;
using BSLib.Design.Handlers;
using GKCore;
using GKCore.Interfaces;
using GKCore.Plugins;
using GKUI.Components;

[assembly: AssemblyTitle("GKCalendarPlugin")]
[assembly: AssemblyDescription("GEDKeeper Calendar plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2014 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyCulture("")]

namespace GKCalendarPlugin
{
    public enum PLS
    {
        LSID_MICalendar,
        LSID_Date,
        LSID_Cal_Gregorian,
        LSID_Cal_Julian,
        LSID_Cal_Hebrew,
        LSID_Cal_Islamic,
        LSID_Cal_Persian,
        LSID_Cal_Indian,
        LSID_Cal_Bahai,

        LSID_BahaiMonths,
        LSID_BahaiWeekdays,
        LSID_ClassicMonths,
        LSID_ClassicWeekdays,
        LSID_HebrewMonths,
        LSID_HebrewWeekdays,
        LSID_IndianCivilMonths,
        LSID_IndianCivilWeekdays,
        LSID_IslamicMonths,
        LSID_IslamicWeekdays,
        LSID_PersianMonths,
        LSID_PersianWeekdays,
        LSID_BahaiCycles
    }

    public sealed class Plugin : WidgetPlugin
    {
        private string fDisplayName = "GKCalendarPlugin";
        private ILangMan fLangMan;
        private IImage fIcon;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return fIcon; } }
        public override PluginCategory Category { get { return PluginCategory.Tool; } }

        private CalendarWidget fForm;

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fForm != null) fForm.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void Execute()
        {
            if (!Host.IsWidgetActive(this)) {
                fForm = new CalendarWidget(this);
                fForm.Show();
            } else {
                fForm.Close();
            }
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.LSID_MICalendar);

                if (fForm != null) fForm.SetLang();
            } catch (Exception ex) {
                Logger.WriteError("GKCalendarPlugin.OnLanguageChange()", ex);
            }
        }

        public override bool Startup(IHost host)
        {
            bool result = base.Startup(host);
            try {
                Assembly assembly = typeof(Plugin).Assembly;
                using (Stream stmIcon = assembly.GetManifestResourceStream("Resources.icon_time.gif")) {
                    Image bmp = Image.FromStream(stmIcon);
                    fIcon = new ImageHandler(bmp);
                }
            } catch (Exception ex) {
                Logger.WriteError("GKCalendarPlugin.Startup()", ex);
                result = false;
            }
            return result;
        }
    }
}
