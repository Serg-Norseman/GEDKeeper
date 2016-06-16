using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace GKTimeLinePlugin
{
	partial class TimeLineWidget
	{
		private System.Windows.Forms.StatusBarPanel StatusBarPanel1;
		private System.Windows.Forms.StatusBarPanel StatusBarPanel2;
		private System.Windows.Forms.TrackBar tbTimeLine;
		private System.Windows.Forms.StatusBar StatusBar1;

		private void InitializeComponent()
		{
            this.tbTimeLine = new System.Windows.Forms.TrackBar();
            this.StatusBar1 = new System.Windows.Forms.StatusBar();
            this.StatusBarPanel1 = new System.Windows.Forms.StatusBarPanel();
            this.StatusBarPanel2 = new System.Windows.Forms.StatusBarPanel();
            ((System.ComponentModel.ISupportInitialize)(this.tbTimeLine)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.StatusBarPanel1)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.StatusBarPanel2)).BeginInit();
            this.SuspendLayout();
            // 
            // tbTimeLine
            // 
            this.tbTimeLine.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tbTimeLine.LargeChange = 1;
            this.tbTimeLine.Location = new System.Drawing.Point(0, 0);
            this.tbTimeLine.Minimum = 5;
            this.tbTimeLine.Name = "tbTimeLine";
            this.tbTimeLine.Size = new System.Drawing.Size(524, 42);
            this.tbTimeLine.TabIndex = 0;
            this.tbTimeLine.Value = 5;
            this.tbTimeLine.ValueChanged += new System.EventHandler(this.tbTimeLine_ValueChanged);
            // 
            // StatusBar1
            // 
            this.StatusBar1.Location = new System.Drawing.Point(0, 42);
            this.StatusBar1.Name = "StatusBar1";
            this.StatusBar1.Panels.AddRange(new System.Windows.Forms.StatusBarPanel[] {
            this.StatusBarPanel1,
            this.StatusBarPanel2});
            this.StatusBar1.ShowPanels = true;
            this.StatusBar1.Size = new System.Drawing.Size(524, 19);
            this.StatusBar1.TabIndex = 1;
            // 
            // StatusBarPanel1
            // 
            this.StatusBarPanel1.Name = "StatusBarPanel1";
            this.StatusBarPanel1.Text = " ";
            this.StatusBarPanel1.Width = 250;
            // 
            // StatusBarPanel2
            // 
            this.StatusBarPanel2.Name = "StatusBarPanel2";
            this.StatusBarPanel2.Text = " ";
            this.StatusBarPanel2.Width = 250;
            // 
            // TimeLineWidget
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
            this.ClientSize = new System.Drawing.Size(524, 61);
            this.Controls.Add(this.tbTimeLine);
            this.Controls.Add(this.StatusBar1);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
            this.Load += new System.EventHandler(this.TimeLineWidget_Load);
            this.Name = "TimeLineWidget";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
            this.Text = "TimeLineWidget";
            this.TopMost = true;
            this.Closed += new System.EventHandler(this.TimeLineWidget_Closed);
            ((System.ComponentModel.ISupportInitialize)(this.tbTimeLine)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.StatusBarPanel1)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.StatusBarPanel2)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

		}
	}
}