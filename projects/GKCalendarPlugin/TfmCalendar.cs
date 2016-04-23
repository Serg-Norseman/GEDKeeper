using System;
using System.Drawing;
using System.Globalization;
using System.Threading;
using System.Windows.Forms;

using GKCommon;

namespace GKCalendarPlugin
{
	/// <summary>
	/// 
	/// </summary>
	public partial class TfmCalendar : Form
	{
    	private readonly Plugin fPlugin;

		public TfmCalendar(Plugin plugin) : base()
		{
			this.InitializeComponent();

			this.fPlugin = plugin;

			this.Location = new Point(Screen.PrimaryScreen.WorkingArea.Width - this.Width - 10, 50);

			this.Text = this.fPlugin.LangMan.LS(PLS.LSID_MICalendar);
			this.ColumnHeader1.Text = this.fPlugin.LangMan.LS(PLS.LSID_MICalendar);
			this.ColumnHeader2.Text = this.fPlugin.LangMan.LS(PLS.LSID_Date);

			this.qtc.SelectionStart = DateTime.Now;
			this.qtc_DateSelected(null, null);
		}

        private void TfmCalendar_Load(object sender, EventArgs e)
        {
        	this.fPlugin.Host.WidgetShow(this.fPlugin);
        }

		private void TfmCalendar_Closed(object sender, EventArgs e)
		{
			this.fPlugin.Host.WidgetClose(this.fPlugin);
		}

		private void qtc_DateSelected(object sender, DateRangeEventArgs e)
		{
			DateTimeFormatInfo dtInfo = Thread.CurrentThread.CurrentCulture.DateTimeFormat;

			this.lvDates.BeginUpdate();
			try
			{
				this.lvDates.Items.Clear();
				DateTime gdt = this.qtc.SelectionStart;
				string s = CalendarData.date_to_str(gdt.Year, gdt.Month, gdt.Day, CalendarData.DateEra.AD) + ", " + dtInfo.DayNames[(int)gdt.DayOfWeek];
				this.AddItem(this.fPlugin.LangMan.LS(PLS.LSID_Cal_Gregorian), s);
				double jd = CalendarConverter.gregorian_to_jd(gdt.Year, gdt.Month, gdt.Day);

                int year, month, day;
                CalendarConverter.jd_to_julian(jd, out year, out month, out day);
				this.AddItem(this.fPlugin.LangMan.LS(PLS.LSID_Cal_Julian), CalendarData.date_to_str(year, month, day, CalendarData.DateEra.AD));

                CalendarConverter.jd_to_hebrew(jd, out year, out month, out day);
				s = day.ToString() + " ";
				s += CalendarData.HebrewMonths[month - 1];
				s = s + " " + year.ToString() + ", " + CalendarData.HebrewWeekdays[CalendarConverter.jwday(jd)];
				this.AddItem(this.fPlugin.LangMan.LS(PLS.LSID_Cal_Hebrew), s);

                CalendarConverter.jd_to_islamic(jd, out year, out month, out day);
				s = day.ToString() + " ";
				s += CalendarData.IslamicMonths[month - 1];
				s = s + " " + year.ToString() + ", йаум " + CalendarData.IslamicWeekdays[CalendarConverter.jwday(jd)];
				this.AddItem(this.fPlugin.LangMan.LS(PLS.LSID_Cal_Islamic), s);

                CalendarConverter.jd_to_persian(jd, out year, out month, out day);
				s = day.ToString() + " ";
				s += CalendarData.PersianMonths[month - 1];
				s = s + " " + year.ToString() + ", " + CalendarData.PersianWeekdays[CalendarConverter.jwday(jd)];
				this.AddItem(this.fPlugin.LangMan.LS(PLS.LSID_Cal_Persian), s);

                CalendarConverter.jd_to_indian_civil(jd, out year, out month, out day);
				s = day.ToString() + " ";
				s += CalendarData.IndianCivilMonths[month - 1];
				s = s + " " + year.ToString() + ", " + CalendarData.IndianCivilWeekdays[CalendarConverter.jwday(jd)];
				this.AddItem(this.fPlugin.LangMan.LS(PLS.LSID_Cal_Indian), s);

				int major, cycle;
                CalendarConverter.jd_to_bahai(jd, out major, out cycle, out year, out month, out day);
				s = "Кулл-и Шай' " + major.ToString() + ", Вахид " + cycle.ToString() + ", ";
				s = s + day.ToString() + " ";
				s += CalendarData.BahaiMonths[month - 1];
				s = s + " " + year.ToString() + ", " + CalendarData.BahaiWeekdays[CalendarConverter.jwday(jd)];
				this.AddItem(this.fPlugin.LangMan.LS(PLS.LSID_Cal_Bahai), s);
			}
			finally
			{
				this.lvDates.EndUpdate();
			}
		}

		private void AddItem(string calendar, string date)
		{
			ListViewItem item = this.lvDates.Items.Add(calendar);
			item.SubItems.Add(date);
		}
	}
}
