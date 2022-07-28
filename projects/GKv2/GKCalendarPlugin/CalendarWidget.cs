/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;
using BSLib.Calendar;
using GKCore;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKCalendarPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public partial class CalendarWidget : Form, ILocalizable
    {
        private readonly Plugin fPlugin;

        private string[] fBahaiMonths;
        private string[] fBahaiWeekdays;
        private string[] fClassicMonths;
        private string[] fClassicWeekdays;
        private string[] fHebrewMonths;
        private string[] fHebrewWeekdays;
        private string[] fIndianCivilMonths;
        private string[] fIndianCivilWeekdays;
        private string[] fIslamicMonths;
        private string[] fIslamicWeekdays;
        private string[] fPersianMonths;
        private string[] fPersianWeekdays;

        public CalendarWidget(Plugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;
            qtc.SelectionStart = DateTime.Now;

            SetLocale();
        }

        private void CalendarWidget_Load(object sender, EventArgs e)
        {
            var loc = AppHost.Instance.WidgetLocate(UIHelper.Rt2Rt(this.Bounds), WidgetHorizontalLocation.Right, WidgetVerticalLocation.Top);
            this.Location = new Point(loc.X, loc.Y);

            fPlugin.Host.WidgetShow(fPlugin);
        }

        private void CalendarWidget_Closed(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetClose(fPlugin);
        }

        private void AddCItem(double jd, int year, int month, int day,
                              string[] months, string[] weekdays, string ext, string calendar)
        {
            // jwday (sunday == 0)
            int jwday = CalendarConverter.jwday(jd);
            // all arrays in range [monday..sunday]
            int weekDay = (jwday > 0) ? jwday - 1 : 6;
            string strDate = ext + string.Format("{0} {1} {2}, {3}", day, months[month - 1], year, weekdays[weekDay]);

            ListViewItem item = lvDates.Items.Add(calendar);
            item.SubItems.Add(strDate);
        }

        private void qtc_DateSelected(object sender, DateRangeEventArgs e)
        {
            lvDates.BeginUpdate();
            try {
                lvDates.Items.Clear();

                DateTime gdt = qtc.SelectionStart;
                double jd = CalendarConverter.gregorian_to_jd(gdt.Year, gdt.Month, gdt.Day);

                int year, month, day;

                CalendarConverter.jd_to_gregorian(jd, out year, out month, out day);
                AddCItem(jd, year, month, day, fClassicMonths, fClassicWeekdays, "", fPlugin.LangMan.LS(PLS.LSID_Cal_Gregorian));

                CalendarConverter.jd_to_julian(jd, out year, out month, out day);
                AddCItem(jd, year, month, day, fClassicMonths, fClassicWeekdays, "", fPlugin.LangMan.LS(PLS.LSID_Cal_Julian));

                CalendarConverter.jd_to_hebrew(jd, out year, out month, out day);
                AddCItem(jd, year, month, day, fHebrewMonths, fHebrewWeekdays, "", fPlugin.LangMan.LS(PLS.LSID_Cal_Hebrew));

                CalendarConverter.jd_to_islamic(jd, out year, out month, out day);
                AddCItem(jd, year, month, day, fIslamicMonths, fIslamicWeekdays, "", fPlugin.LangMan.LS(PLS.LSID_Cal_Islamic));

                CalendarConverter.jd_to_persian(jd, out year, out month, out day);
                AddCItem(jd, year, month, day, fPersianMonths, fPersianWeekdays, "", fPlugin.LangMan.LS(PLS.LSID_Cal_Persian));

                CalendarConverter.jd_to_indian_civil(jd, out year, out month, out day);
                AddCItem(jd, year, month, day, fIndianCivilMonths, fIndianCivilWeekdays, "", fPlugin.LangMan.LS(PLS.LSID_Cal_Indian));

                int major, cycle;
                CalendarConverter.jd_to_bahai(jd, out major, out cycle, out year, out month, out day);
                string s = string.Format(fPlugin.LangMan.LS(PLS.LSID_BahaiCycles), major, cycle) + ", ";
                AddCItem(jd, year, month, day, fBahaiMonths, fBahaiWeekdays, s, fPlugin.LangMan.LS(PLS.LSID_Cal_Bahai));
            } finally {
                lvDates.EndUpdate();
                lvDates.AutoResizeColumns(ColumnHeaderAutoResizeStyle.ColumnContent);
            }
        }

        #region ILocalizable support

        public void SetLocale()
        {
            Text = fPlugin.LangMan.LS(PLS.LSID_MICalendar);
            ColumnHeader1.Text = fPlugin.LangMan.LS(PLS.LSID_MICalendar);
            ColumnHeader2.Text = fPlugin.LangMan.LS(PLS.LSID_Date);

            var langMan = fPlugin.LangMan;
            fBahaiMonths = CalendarData.InitNames(langMan.LS(PLS.LSID_BahaiMonths));
            fBahaiWeekdays = CalendarData.InitNames(langMan.LS(PLS.LSID_BahaiWeekdays));
            fClassicMonths = CalendarData.InitNames(langMan.LS(PLS.LSID_ClassicMonths));
            fClassicWeekdays = CalendarData.InitNames(langMan.LS(PLS.LSID_ClassicWeekdays));
            fHebrewMonths = CalendarData.InitNames(langMan.LS(PLS.LSID_HebrewMonths));
            fHebrewWeekdays = CalendarData.InitNames(langMan.LS(PLS.LSID_HebrewWeekdays));
            fIndianCivilMonths = CalendarData.InitNames(langMan.LS(PLS.LSID_IndianCivilMonths));
            fIndianCivilWeekdays = CalendarData.InitNames(langMan.LS(PLS.LSID_IndianCivilWeekdays));
            fIslamicMonths = CalendarData.InitNames(langMan.LS(PLS.LSID_IslamicMonths));
            fIslamicWeekdays = CalendarData.InitNames(langMan.LS(PLS.LSID_IslamicWeekdays));
            fPersianMonths = CalendarData.InitNames(langMan.LS(PLS.LSID_PersianMonths));
            fPersianWeekdays = CalendarData.InitNames(langMan.LS(PLS.LSID_PersianWeekdays));

            qtc_DateSelected(null, null);
        }

        #endregion
    }
}
