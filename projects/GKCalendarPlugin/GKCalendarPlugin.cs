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
using System.Reflection;
using System.Runtime.InteropServices;

using GKCommon;
using GKCore.Interfaces;

[assembly: AssemblyTitle("GKCalendarPlugin")]
[assembly: AssemblyDescription("GEDKeeper Calendar plugin")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2014 by Sergey V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: CLSCompliant(false)]
[assembly: ComVisible(false)]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyFileVersion("1.0.0.0")]

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

    public sealed class Plugin : BaseObject, IPlugin, IWidget
    {
        private string fDisplayName = "GKCalendarPlugin";
        private IHost fHost;
        private ILangMan fLangMan;

        public string DisplayName { get { return this.fDisplayName; } }
        public IHost Host { get { return fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

        private CalendarWidget frm;

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (frm != null) frm.Dispose();
            }
            base.Dispose(disposing);
        }

        public void Execute()
        {
            if (!this.fHost.IsWidgetActive(this)) {
                frm = new CalendarWidget(this);
                frm.Show();
            } else {
                frm.Close();
            }
        }

        public void OnHostClosing(ref bool cancelClosing) {}
        public void OnHostActivate() {}
        public void OnHostDeactivate() {}

        public void OnLanguageChange()
        {
            try
            {
                this.fLangMan = this.fHost.CreateLangMan(this);
                this.fDisplayName = this.fLangMan.LS(PLS.LSID_MICalendar);

                CalendarData.BahaiMonths = CalendarData.InitNames(this.fLangMan.LS(PLS.LSID_BahaiMonths));
                CalendarData.BahaiWeekdays = CalendarData.InitNames(this.fLangMan.LS(PLS.LSID_BahaiWeekdays));
                CalendarData.ClassicMonths = CalendarData.InitNames(this.fLangMan.LS(PLS.LSID_ClassicMonths));
                CalendarData.ClassicWeekdays = CalendarData.InitNames(this.fLangMan.LS(PLS.LSID_ClassicWeekdays));
                CalendarData.HebrewMonths = CalendarData.InitNames(this.fLangMan.LS(PLS.LSID_HebrewMonths));
                CalendarData.HebrewWeekdays = CalendarData.InitNames(this.fLangMan.LS(PLS.LSID_HebrewWeekdays));
                CalendarData.IndianCivilMonths = CalendarData.InitNames(this.fLangMan.LS(PLS.LSID_IndianCivilMonths));
                CalendarData.IndianCivilWeekdays = CalendarData.InitNames(this.fLangMan.LS(PLS.LSID_IndianCivilWeekdays));
                CalendarData.IslamicMonths = CalendarData.InitNames(this.fLangMan.LS(PLS.LSID_IslamicMonths));
                CalendarData.IslamicWeekdays = CalendarData.InitNames(this.fLangMan.LS(PLS.LSID_IslamicWeekdays));
                CalendarData.PersianMonths = CalendarData.InitNames(this.fLangMan.LS(PLS.LSID_PersianMonths));
                CalendarData.PersianWeekdays = CalendarData.InitNames(this.fLangMan.LS(PLS.LSID_PersianWeekdays));

                if (frm != null) frm.SetLang();
            }
            catch (Exception ex)
            {
                fHost.LogWrite("GKCalendarPlugin.OnLanguageChange(): " + ex.Message);
            }
        }

        public bool Startup(IHost host)
        {
            bool result = true;
            try
            {
                this.fHost = host;
            }
            catch (Exception ex)
            {
                fHost.LogWrite("GKCalendarPlugin.Startup(): " + ex.Message);
                result = false;
            }
            return result;
        }

        public bool Shutdown()
        {
            bool result = true;
            try
            {
            }
            catch (Exception ex)
            {
                fHost.LogWrite("GKCalendarPlugin.Shutdown(): " + ex.Message);
                result = false;
            }
            return result;
        }

        #region IWidget common

        void IWidget.WidgetInit(IHost host) {}
        void IWidget.BaseChanged(IBaseWindow baseWin) {}
        void IWidget.BaseClosed(IBaseWindow baseWin) {}
        void IWidget.BaseRenamed(IBaseWindow baseWin, string oldName, string newName) {}
        void IWidget.WidgetEnable() {}

        #endregion
    }
}
