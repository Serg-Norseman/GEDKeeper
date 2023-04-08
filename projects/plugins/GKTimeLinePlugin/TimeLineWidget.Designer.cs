namespace GKTimeLinePlugin
{
	partial class TimeLineWidget
	{
		private System.Windows.Forms.ToolStripStatusLabel StatusBarPanel1;
		private System.Windows.Forms.ToolStripStatusLabel StatusBarPanel2;
		private System.Windows.Forms.TrackBar tbTimeLine;
		private System.Windows.Forms.StatusStrip StatusBar1;

		private void InitializeComponent()
		{
            this.tbTimeLine = new System.Windows.Forms.TrackBar();
            this.StatusBar1 = new System.Windows.Forms.StatusStrip();
            this.StatusBarPanel1 = new System.Windows.Forms.ToolStripStatusLabel();
            this.StatusBarPanel2 = new System.Windows.Forms.ToolStripStatusLabel();
            ((System.ComponentModel.ISupportInitialize)(this.tbTimeLine)).BeginInit();
            this.StatusBar1.SuspendLayout();
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
            this.StatusBar1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.StatusBarPanel1,
            this.StatusBarPanel2});
            this.StatusBar1.Size = new System.Drawing.Size(524, 19);
            this.StatusBar1.TabIndex = 1;
            // 
            // StatusBarPanel1
            // 
            this.StatusBarPanel1.BorderSides = ((System.Windows.Forms.ToolStripStatusLabelBorderSides)((((System.Windows.Forms.ToolStripStatusLabelBorderSides.Left | System.Windows.Forms.ToolStripStatusLabelBorderSides.Top)
            | System.Windows.Forms.ToolStripStatusLabelBorderSides.Right)
            | System.Windows.Forms.ToolStripStatusLabelBorderSides.Bottom)));
            this.StatusBarPanel1.BorderStyle = System.Windows.Forms.Border3DStyle.Flat;
            this.StatusBarPanel1.Name = "StatusBarPanel1";
            this.StatusBarPanel1.Text = " ";
            this.StatusBarPanel1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.StatusBarPanel1.Width = 250;
            // 
            // StatusBarPanel2
            // 
            this.StatusBarPanel2.BorderSides = ((System.Windows.Forms.ToolStripStatusLabelBorderSides)((((System.Windows.Forms.ToolStripStatusLabelBorderSides.Left | System.Windows.Forms.ToolStripStatusLabelBorderSides.Top)
            | System.Windows.Forms.ToolStripStatusLabelBorderSides.Right)
            | System.Windows.Forms.ToolStripStatusLabelBorderSides.Bottom)));
            this.StatusBarPanel2.BorderStyle = System.Windows.Forms.Border3DStyle.Flat;
            this.StatusBarPanel2.Name = "StatusBarPanel2";
            this.StatusBarPanel2.Text = " ";
            this.StatusBarPanel2.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
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
            this.StatusBar1.ResumeLayout(false);
            this.StatusBar1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

		}
	}
}
