using GKCore;
using GKSys;
using System;
using System.Drawing;
using System.Globalization;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Threading;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmCalendar : Form
	{
		private ListView lvDates;
		private MonthCalendar qtc;
		private ColumnHeader ColumnHeader1;
		private ColumnHeader ColumnHeader2;

		private void InitializeComponent()
		{
			this.lvDates = new ListView();
			this.ColumnHeader1 = new ColumnHeader();
			this.ColumnHeader2 = new ColumnHeader();
			this.qtc = new MonthCalendar();
			base.SuspendLayout();
			ListView.ColumnHeaderCollection arg_75_0 = this.lvDates.Columns;
			ColumnHeader[] array = null;
			ColumnHeader[] array2 = array;
			ColumnHeader[] array3;
			ColumnHeader[] expr_47 = array3 = new ColumnHeader[2];
			if (array2 != null)
			{
				int num;
				if ((num = array2.Length) > 2)
				{
					num = 2;
				}
				if (num > 0)
				{
					Array.Copy(array2, array3, num);
				}
			}
			array = expr_47;
			array[0] = this.ColumnHeader1;
			array[1] = this.ColumnHeader2;
			arg_75_0.AddRange(array);
			this.lvDates.FullRowSelect = true;
			this.lvDates.Location = new Point(8, 184);
			this.lvDates.Name = "lvDates";
			this.lvDates.Size = new Size(258, 121);
			this.lvDates.TabIndex = 1;
			this.lvDates.View = View.Details;
			this.ColumnHeader1.Text = "Календарь";
			this.ColumnHeader1.Width = 120;
			this.ColumnHeader2.Text = "Дата";
			this.ColumnHeader2.Width = 120;
			this.qtc.FirstDayOfWeek = Day.Monday;
			this.qtc.Location = new Point(8, 8);
			this.qtc.Name = "qtc";
			this.qtc.ShowWeekNumbers = true;
			this.qtc.TabIndex = 0;
			this.qtc.DateSelected += new DateRangeEventHandler(this.qtc_DateSelected);
			this.AutoScaleBaseSize = new Size(5, 14);
			base.ClientSize = new Size(274, 313);
			base.Controls.Add(this.qtc);
			base.Controls.Add(this.lvDates);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedToolWindow;
			base.Name = "TfmCalendar";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.Manual;
			this.Text = "Календарь";
			base.TopMost = true;
			base.Closed += new EventHandler(this.TfmCalendar_Closed);
			base.ResumeLayout(false);
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
			GKL.fmGEDKeeper.miCalendar.Checked = false;
			GKL.fmGEDKeeper.fmCalendar = null;
		}

		public TfmCalendar()
		{
			this.InitializeComponent();
			this.qtc.SelectionStart = DateTime.Now;
			this.qtc_DateSelected(null, null);
			this.Text = GKL.LSList[32];
			this.ColumnHeader1.Text = GKL.LSList[32];
			this.ColumnHeader2.Text = GKL.LSList[139];
		}

		private static void _qtc_DateSelected_AddItem([In] TfmCalendar Self, string aCalendar, string aDate)
		{
			ListViewItem item = Self.lvDates.Items.Add(aCalendar);
			item.SubItems.Add(aDate);
		}
	}
}
