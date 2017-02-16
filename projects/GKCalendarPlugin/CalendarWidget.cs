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
using System.Drawing;
using System.Windows.Forms;

using GKCommon;
using GKCore.Interfaces;

namespace GKCalendarPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public partial class CalendarWidget : Form, ILocalization
    {
        private readonly Plugin fPlugin;

        public CalendarWidget(Plugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;

            Location = new Point(Screen.PrimaryScreen.WorkingArea.Width - Width - 10, 50);

            qtc.SelectionStart = DateTime.Now;

            SetLang();
        }

        private void CalendarWidget_Load(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetShow(fPlugin);
        }

        private void CalendarWidget_Closed(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetClose(fPlugin);
        }

        private static string d2s(int day, string month, int year, string weekday)
        {
            return string.Format("{0} {1} {2}, {3}", day, month, year, weekday);
        }

        private void qtc_DateSelected(object sender, DateRangeEventArgs e)
        {
            lvDates.BeginUpdate();
            try
            {
                string s;
                int year, month, day;

                lvDates.Items.Clear();

                DateTime gdt = qtc.SelectionStart;
                double jd = CalendarConverter.gregorian_to_jd(gdt.Year, gdt.Month, gdt.Day);

                CalendarConverter.jd_to_gregorian(jd, out year, out month, out day);
                s = d2s(day, CalendarData.ClassicMonths[month - 1], year, CalendarData.ClassicWeekdays[CalendarConverter.jwday(jd)]);
                AddItem(fPlugin.LangMan.LS(PLS.LSID_Cal_Gregorian), s);

                CalendarConverter.jd_to_julian(jd, out year, out month, out day);
                s = d2s(day, CalendarData.ClassicMonths[month - 1], year, CalendarData.ClassicWeekdays[CalendarConverter.jwday(jd)]);
                AddItem(fPlugin.LangMan.LS(PLS.LSID_Cal_Julian), s);

                CalendarConverter.jd_to_hebrew(jd, out year, out month, out day);
                s = d2s(day, CalendarData.HebrewMonths[month - 1], year, CalendarData.HebrewWeekdays[CalendarConverter.jwday(jd)]);
                AddItem(fPlugin.LangMan.LS(PLS.LSID_Cal_Hebrew), s);

                CalendarConverter.jd_to_islamic(jd, out year, out month, out day);
                s = d2s(day, CalendarData.IslamicMonths[month - 1], year, CalendarData.IslamicWeekdays[CalendarConverter.jwday(jd)]);
                AddItem(fPlugin.LangMan.LS(PLS.LSID_Cal_Islamic), s);

                CalendarConverter.jd_to_persian(jd, out year, out month, out day);
                s = d2s(day, CalendarData.PersianMonths[month - 1], year, CalendarData.PersianWeekdays[CalendarConverter.jwday(jd)]);
                AddItem(fPlugin.LangMan.LS(PLS.LSID_Cal_Persian), s);

                CalendarConverter.jd_to_indian_civil(jd, out year, out month, out day);
                s = d2s(day, CalendarData.IndianCivilMonths[month - 1], year, CalendarData.IndianCivilWeekdays[CalendarConverter.jwday(jd)]);
                AddItem(fPlugin.LangMan.LS(PLS.LSID_Cal_Indian), s);

                int major, cycle;
                CalendarConverter.jd_to_bahai(jd, out major, out cycle, out year, out month, out day);
                s = string.Format(fPlugin.LangMan.LS(PLS.LSID_BahaiCycles), major, cycle) + ", ";
                s = s + d2s(day, CalendarData.BahaiMonths[month - 1], year, CalendarData.BahaiWeekdays[CalendarConverter.jwday(jd)]);
                AddItem(fPlugin.LangMan.LS(PLS.LSID_Cal_Bahai), s);
            }
            finally
            {
                lvDates.EndUpdate();
            }
        }

        private void AddItem(string calendar, string date)
        {
            ListViewItem item = lvDates.Items.Add(calendar);
            item.SubItems.Add(date);
        }

        #region ILocalization support

        public void SetLang()
        {
            Text = fPlugin.LangMan.LS(PLS.LSID_MICalendar);
            ColumnHeader1.Text = fPlugin.LangMan.LS(PLS.LSID_MICalendar);
            ColumnHeader2.Text = fPlugin.LangMan.LS(PLS.LSID_Date);

            qtc_DateSelected(null, null);
        }

        #endregion
    }
}
