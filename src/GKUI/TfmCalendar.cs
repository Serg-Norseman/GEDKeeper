using System;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Threading;
using System.Windows.Forms;

using Ext.Utils;
using GKCore;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI
{
	public partial class TfmCalendar : Form
	{
		public TfmCalendar()
		{
			this.InitializeComponent();
			this.qtc.SelectionStart = DateTime.Now;
			this.qtc_DateSelected(null, null);
			this.Text = LangMan.LSList[32];
			this.ColumnHeader1.Text = LangMan.LSList[32];
			this.ColumnHeader2.Text = LangMan.LSList[139];
		}

		private void qtc_DateSelected(object sender, DateRangeEventArgs e)
		{
			DateTimeFormatInfo DateTimeInfo = Thread.CurrentThread.CurrentCulture.DateTimeFormat;
			CalendarConverter cal = new CalendarConverter();
			this.lvDates.BeginUpdate();
			try
			{
				this.lvDates.Items.Clear();
				DateTime gdt = this.qtc.SelectionStart;
				string s = cal.date_to_str(gdt.Year, gdt.Month, gdt.Day, CalendarConverter.TDateEra.AD) + ", " + DateTimeInfo.DayNames[(int)gdt.DayOfWeek];
				TfmCalendar._qtc_DateSelected_AddItem(this, LangMan.LSList[156], s);
				double jd = cal.gregorian_to_jd(gdt.Year, gdt.Month, gdt.Day);
				int year = 0;
				int month = 0;
				int day = 0;
				cal.jd_to_julian(jd, ref year, ref month, ref day);
				TfmCalendar._qtc_DateSelected_AddItem(this, LangMan.LSList[157], cal.date_to_str(year, month, day, CalendarConverter.TDateEra.AD));

				cal.jd_to_hebrew(jd, ref year, ref month, ref day);
				s = day.ToString() + " ";
				s += CalendarConverter.HebrewMonths[month - 1];
				s = s + " " + year.ToString() + ", " + CalendarConverter.HebrewWeekdays[cal.jwday(jd)];
				TfmCalendar._qtc_DateSelected_AddItem(this, LangMan.LSList[158], s);

				cal.jd_to_islamic(jd, ref year, ref month, ref day);
				s = day.ToString() + " ";
				s += CalendarConverter.IslamicMonths[month - 1];
				s = s + " " + year.ToString() + ", йаум " + CalendarConverter.IslamicWeekdays[cal.jwday(jd)];
				TfmCalendar._qtc_DateSelected_AddItem(this, LangMan.LSList[159], s);

				cal.jd_to_persian(jd, ref year, ref month, ref day);
				s = day.ToString() + " ";
				s += CalendarConverter.PersianMonths[month - 1];
				s = s + " " + year.ToString() + ", " + CalendarConverter.PersianWeekdays[cal.jwday(jd)];
				TfmCalendar._qtc_DateSelected_AddItem(this, LangMan.LSList[160], s);

				cal.jd_to_indian_civil(jd, ref year, ref month, ref day);
				s = day.ToString() + " ";
				s += CalendarConverter.IndianCivilMonths[month - 1];
				s = s + " " + year.ToString() + ", " + CalendarConverter.IndianCivilWeekdays[cal.jwday(jd)];
				TfmCalendar._qtc_DateSelected_AddItem(this, LangMan.LSList[161], s);

				int major = 0;
				int cycle = 0;
				cal.jd_to_bahai(jd, ref major, ref cycle, ref year, ref month, ref day);
				s = "Кулл-и Шай' " + major.ToString() + ", Вахид " + cycle.ToString() + ", ";
				s = s + day.ToString() + " ";
				s += CalendarConverter.BahaiMonths[month - 1];
				s = s + " " + year.ToString() + ", " + CalendarConverter.BahaiWeekdays[cal.jwday(jd)];
				TfmCalendar._qtc_DateSelected_AddItem(this, LangMan.LSList[162], s);
			}
			finally
			{
				this.lvDates.EndUpdate();
				cal.Free();
			}
		}

		private void TfmCalendar_Closed(object sender, EventArgs e)
		{
			GKUI.TfmGEDKeeper.Instance.miCalendar.Checked = false;
			GKUI.TfmGEDKeeper.Instance.fmCalendar = null;
		}

		private static void _qtc_DateSelected_AddItem([In] TfmCalendar Self, string aCalendar, string aDate)
		{
			ListViewItem item = Self.lvDates.Items.Add(aCalendar);
			item.SubItems.Add(aDate);
		}
	}
}
