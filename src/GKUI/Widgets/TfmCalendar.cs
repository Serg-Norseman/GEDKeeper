using System;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Threading;
using System.Windows.Forms;

using GKCore;

namespace GKUI
{
	public partial class TfmCalendar : Form
	{
		public TfmCalendar()
		{
			this.InitializeComponent();
			this.qtc.SelectionStart = DateTime.Now;
			this.qtc_DateSelected(null, null);
			this.Text = GKL.LSList[32];
			this.ColumnHeader1.Text = GKL.LSList[32];
			this.ColumnHeader2.Text = GKL.LSList[139];
		}

		private void qtc_DateSelected(object sender, DateRangeEventArgs e)
		{
			DateTimeFormatInfo DateTimeInfo = Thread.CurrentThread.CurrentCulture.DateTimeFormat;
			TCalendarConv cal = new TCalendarConv();
			this.lvDates.BeginUpdate();
			try
			{
				this.lvDates.Items.Clear();
				DateTime gdt = this.qtc.SelectionStart;
				string s = cal.date_to_str(gdt.Year, gdt.Month, gdt.Day, TCalendarConv.TDateEra.AD) + ", " + DateTimeInfo.DayNames[(int)gdt.DayOfWeek];
				TfmCalendar._qtc_DateSelected_AddItem(this, GKL.LSList[156], s);
				double jd = cal.gregorian_to_jd(gdt.Year, gdt.Month, gdt.Day);
				int year = 0;
				int month = 0;
				int day = 0;
				cal.jd_to_julian(jd, ref year, ref month, ref day);
				TfmCalendar._qtc_DateSelected_AddItem(this, GKL.LSList[157], cal.date_to_str(year, month, day, TCalendarConv.TDateEra.AD));
				cal.jd_to_hebrew(jd, ref year, ref month, ref day);
				s = day.ToString() + " ";
				s += TCalendarConv.HebrewMonths[month - 1];
				s = string.Concat(new string[]
				{
					s, 
					" ", 
					year.ToString(), 
					", ", 
					TCalendarConv.HebrewWeekdays[cal.jwday(jd)]
				});
				TfmCalendar._qtc_DateSelected_AddItem(this, GKL.LSList[158], s);
				cal.jd_to_islamic(jd, ref year, ref month, ref day);
				s = day.ToString() + " ";
				s += TCalendarConv.IslamicMonths[month - 1];
				s = string.Concat(new string[]
				{
					s, 
					" ", 
					year.ToString(), 
					", йаум ", 
					TCalendarConv.IslamicWeekdays[cal.jwday(jd)]
				});
				TfmCalendar._qtc_DateSelected_AddItem(this, GKL.LSList[159], s);
				cal.jd_to_persian(jd, ref year, ref month, ref day);
				s = day.ToString() + " ";
				s += TCalendarConv.PersianMonths[month - 1];
				s = string.Concat(new string[]
				{
					s, 
					" ", 
					year.ToString(), 
					", ", 
					TCalendarConv.PersianWeekdays[cal.jwday(jd)]
				});
				TfmCalendar._qtc_DateSelected_AddItem(this, GKL.LSList[160], s);
				cal.jd_to_indian_civil(jd, ref year, ref month, ref day);
				s = day.ToString() + " ";
				s += TCalendarConv.IndianCivilMonths[month - 1];
				s = string.Concat(new string[]
				{
					s, 
					" ", 
					year.ToString(), 
					", ", 
					TCalendarConv.IndianCivilWeekdays[cal.jwday(jd)]
				});
				TfmCalendar._qtc_DateSelected_AddItem(this, GKL.LSList[161], s);
				int major = 0;
				int cycle = 0;
				cal.jd_to_bahai(jd, ref major, ref cycle, ref year, ref month, ref day);
				s = string.Concat(new string[]
				{
					"Кулл-и Шай' ", 
					major.ToString(), 
					", Вахид ", 
					cycle.ToString(), 
					", "
				});
				s = s + day.ToString() + " ";
				s += TCalendarConv.BahaiMonths[month - 1];
				s = string.Concat(new string[]
				{
					s, 
					" ", 
					year.ToString(), 
					", ", 
					TCalendarConv.BahaiWeekdays[cal.jwday(jd)]
				});
				TfmCalendar._qtc_DateSelected_AddItem(this, GKL.LSList[162], s);
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
