using System;

namespace GKCalendarPlugin
{
	partial class CalendarWidget
	{
		private System.Windows.Forms.ListView lvDates;
		private System.Windows.Forms.MonthCalendar qtc;
		private System.Windows.Forms.ColumnHeader ColumnHeader1;
		private System.Windows.Forms.ColumnHeader ColumnHeader2;

		private void InitializeComponent()
		{
			this.lvDates = new System.Windows.Forms.ListView();
			this.ColumnHeader1 = new System.Windows.Forms.ColumnHeader();
			this.ColumnHeader2 = new System.Windows.Forms.ColumnHeader();
			this.qtc = new System.Windows.Forms.MonthCalendar();
			base.SuspendLayout();
			this.lvDates.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] { this.ColumnHeader1, this.ColumnHeader2 });
			this.lvDates.FullRowSelect = true;
			this.lvDates.Location = new System.Drawing.Point(8, 184);
			this.lvDates.Name = "lvDates";
			this.lvDates.Size = new System.Drawing.Size(258, 121);
			this.lvDates.TabIndex = 1;
			this.lvDates.View = System.Windows.Forms.View.Details;
			this.ColumnHeader1.Text = "Calendar";
			this.ColumnHeader1.Width = 120;
			this.ColumnHeader2.Text = "Date";
			this.ColumnHeader2.Width = 120;
			this.qtc.FirstDayOfWeek = System.Windows.Forms.Day.Monday;
			this.qtc.Location = new System.Drawing.Point(8, 8);
			this.qtc.Name = "qtc";
			this.qtc.ShowWeekNumbers = true;
			this.qtc.TabIndex = 0;
			this.qtc.DateSelected += new System.Windows.Forms.DateRangeEventHandler(this.qtc_DateSelected);
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			base.ClientSize = new System.Drawing.Size(274, 313);
			base.Controls.Add(this.qtc);
			base.Controls.Add(this.lvDates);
			this.Font = new System.Drawing.Font("Tahoma", 8.25f, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, 204);
			base.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
			this.Load += new System.EventHandler(this.TfmCalendar_Load);
			base.Name = "CalendarWidget";
			base.ShowInTaskbar = false;
			base.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
			this.Text = "CalendarWidget";
			base.TopMost = true;
			base.Closed += new System.EventHandler(this.TfmCalendar_Closed);
			base.ResumeLayout(false);
		}
	}
}