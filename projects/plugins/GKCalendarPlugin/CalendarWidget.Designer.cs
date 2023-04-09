namespace GKCalendarPlugin
{
	partial class CalendarWidget
	{
        private System.Windows.Forms.GroupBox grpSourceDate;
        private HistoryDateBox historyDateBox1;
        private System.Windows.Forms.GroupBox grpConvertedDate;
        private HistoryDateBox historyDateBox2;

        private void InitializeComponent()
		{
            this.grpSourceDate = new System.Windows.Forms.GroupBox();
            this.historyDateBox1 = new GKCalendarPlugin.HistoryDateBox();
            this.grpConvertedDate = new System.Windows.Forms.GroupBox();
            this.historyDateBox2 = new GKCalendarPlugin.HistoryDateBox();
            this.grpSourceDate.SuspendLayout();
            this.grpConvertedDate.SuspendLayout();
            this.SuspendLayout();
            // 
            // grpSourceDate
            // 
            this.grpSourceDate.Controls.Add(this.historyDateBox1);
            this.grpSourceDate.Location = new System.Drawing.Point(12, 12);
            this.grpSourceDate.Name = "grpSourceDate";
            this.grpSourceDate.Size = new System.Drawing.Size(451, 117);
            this.grpSourceDate.TabIndex = 4;
            this.grpSourceDate.TabStop = false;
            this.grpSourceDate.Text = "grpSourceDate";
            // 
            // historyDateBox1
            // 
            this.historyDateBox1.AutoSize = true;
            this.historyDateBox1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.historyDateBox1.Date = double.NaN;
            this.historyDateBox1.Font = new System.Drawing.Font("Tahoma", 8.2F);
            this.historyDateBox1.Location = new System.Drawing.Point(6, 20);
            this.historyDateBox1.Name = "historyDateBox1";
            this.historyDateBox1.Padding = new System.Windows.Forms.Padding(2);
            this.historyDateBox1.Size = new System.Drawing.Size(432, 89);
            this.historyDateBox1.TabIndex = 3;
            // 
            // grpConvertedDate
            // 
            this.grpConvertedDate.Controls.Add(this.historyDateBox2);
            this.grpConvertedDate.Location = new System.Drawing.Point(12, 135);
            this.grpConvertedDate.Name = "grpConvertedDate";
            this.grpConvertedDate.Size = new System.Drawing.Size(451, 117);
            this.grpConvertedDate.TabIndex = 6;
            this.grpConvertedDate.TabStop = false;
            this.grpConvertedDate.Text = "grpConvertedDate";
            // 
            // historyDateBox2
            // 
            this.historyDateBox2.AutoSize = true;
            this.historyDateBox2.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.historyDateBox2.Date = double.NaN;
            this.historyDateBox2.Font = new System.Drawing.Font("Tahoma", 8.2F);
            this.historyDateBox2.Location = new System.Drawing.Point(6, 20);
            this.historyDateBox2.Name = "historyDateBox2";
            this.historyDateBox2.Padding = new System.Windows.Forms.Padding(2);
            this.historyDateBox2.Size = new System.Drawing.Size(432, 89);
            this.historyDateBox2.TabIndex = 3;
            // 
            // CalendarWidget
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
            this.ClientSize = new System.Drawing.Size(475, 264);
            this.Controls.Add(this.grpConvertedDate);
            this.Controls.Add(this.grpSourceDate);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
            this.Name = "CalendarWidget";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
            this.Text = "CalendarWidget";
            this.TopMost = true;
            this.Closed += new System.EventHandler(this.CalendarWidget_Closed);
            this.Load += new System.EventHandler(this.CalendarWidget_Load);
            this.grpSourceDate.ResumeLayout(false);
            this.grpSourceDate.PerformLayout();
            this.grpConvertedDate.ResumeLayout(false);
            this.grpConvertedDate.PerformLayout();
            this.ResumeLayout(false);

		}
    }
}
