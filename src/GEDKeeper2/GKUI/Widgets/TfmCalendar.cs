using System;
using System.Drawing;
using System.Globalization;
using System.Threading;
using System.Windows.Forms;

using ExtUtils;
using GKCore;
using GKCore.Interfaces;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Widgets
{
	public partial class TfmCalendar : Form, IWidget
	{
    	#region IWidget common

    	private IHost fHost;
    	private MenuItem fMenuItem;

    	IHost IWidget.Host
    	{
    		get { return this.fHost; }
    	}

    	MenuItem IWidget.MenuItem
    	{
    		get { return this.fMenuItem; }
    	}

    	void IWidget.WidgetInit(IHost host, MenuItem menuItem)
    	{
    		this.fHost = host;
    		this.fMenuItem = menuItem;
    	}

        void IWidget.BaseChanged(IBase aBase) {}

    	#endregion

		public TfmCalendar() : base()
		{
			this.InitializeComponent();

			this.Location = new Point(Screen.PrimaryScreen.WorkingArea.Width - this.Width - 10, 50);

			this.Text = LangMan.LS(LSID.LSID_MICalendar);
			this.ColumnHeader1.Text = LangMan.LS(LSID.LSID_MICalendar);
			this.ColumnHeader2.Text = LangMan.LS(LSID.LSID_Date);

			this.qtc.SelectionStart = DateTime.Now;
			this.qtc_DateSelected(null, null);
		}

        private void TfmCalendar_Load(object sender, EventArgs e)
        {
        	this.fHost.WidgetShow(this);
        }

		private void TfmCalendar_Closed(object sender, EventArgs e)
		{
			this.fHost.WidgetClose(this);
		}

		private void qtc_DateSelected(object sender, DateRangeEventArgs e)
		{
			DateTimeFormatInfo DateTimeInfo = Thread.CurrentThread.CurrentCulture.DateTimeFormat;

			this.lvDates.BeginUpdate();
			try
			{
				this.lvDates.Items.Clear();
				DateTime gdt = this.qtc.SelectionStart;
				string s = CalendarConverter.date_to_str(gdt.Year, gdt.Month, gdt.Day, CalendarConverter.TDateEra.AD) + ", " + DateTimeInfo.DayNames[(int)gdt.DayOfWeek];
				this.AddItem(LangMan.LS(LSID.LSID_Cal_Gregorian), s);
				double jd = CalendarConverter.gregorian_to_jd(gdt.Year, gdt.Month, gdt.Day);
				int year = 0;
				int month = 0;
				int day = 0;
				CalendarConverter.jd_to_julian(jd, ref year, ref month, ref day);
				this.AddItem(LangMan.LS(LSID.LSID_Cal_Julian), CalendarConverter.date_to_str(year, month, day, CalendarConverter.TDateEra.AD));

				CalendarConverter.jd_to_hebrew(jd, ref year, ref month, ref day);
				s = day.ToString() + " ";
				s += CalendarConverter.HebrewMonths[month - 1];
				s = s + " " + year.ToString() + ", " + CalendarConverter.HebrewWeekdays[CalendarConverter.jwday(jd)];
				this.AddItem(LangMan.LS(LSID.LSID_Cal_Hebrew), s);

				CalendarConverter.jd_to_islamic(jd, ref year, ref month, ref day);
				s = day.ToString() + " ";
				s += CalendarConverter.IslamicMonths[month - 1];
				s = s + " " + year.ToString() + ", йаум " + CalendarConverter.IslamicWeekdays[CalendarConverter.jwday(jd)];
				this.AddItem(LangMan.LS(LSID.LSID_Cal_Islamic), s);

				CalendarConverter.jd_to_persian(jd, ref year, ref month, ref day);
				s = day.ToString() + " ";
				s += CalendarConverter.PersianMonths[month - 1];
				s = s + " " + year.ToString() + ", " + CalendarConverter.PersianWeekdays[CalendarConverter.jwday(jd)];
				this.AddItem(LangMan.LS(LSID.LSID_Cal_Persian), s);

				CalendarConverter.jd_to_indian_civil(jd, ref year, ref month, ref day);
				s = day.ToString() + " ";
				s += CalendarConverter.IndianCivilMonths[month - 1];
				s = s + " " + year.ToString() + ", " + CalendarConverter.IndianCivilWeekdays[CalendarConverter.jwday(jd)];
				this.AddItem(LangMan.LS(LSID.LSID_Cal_Indian), s);

				int major = 0;
				int cycle = 0;
				CalendarConverter.jd_to_bahai(jd, ref major, ref cycle, ref year, ref month, ref day);
				s = "Кулл-и Шай' " + major.ToString() + ", Вахид " + cycle.ToString() + ", ";
				s = s + day.ToString() + " ";
				s += CalendarConverter.BahaiMonths[month - 1];
				s = s + " " + year.ToString() + ", " + CalendarConverter.BahaiWeekdays[CalendarConverter.jwday(jd)];
				this.AddItem(LangMan.LS(LSID.LSID_Cal_Bahai), s);
			}
			finally
			{
				this.lvDates.EndUpdate();
			}
		}

		private void AddItem(string aCalendar, string aDate)
		{
			ListViewItem item = this.lvDates.Items.Add(aCalendar);
			item.SubItems.Add(aDate);
		}
	}
}
