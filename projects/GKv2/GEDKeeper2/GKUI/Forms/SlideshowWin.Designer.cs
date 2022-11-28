namespace GKUI.Forms
{
	partial class SlideshowWin
	{
		private System.Windows.Forms.ToolStripButton tbNext;
		private System.Windows.Forms.ToolStripButton tbPrev;
		private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
		private System.Windows.Forms.ToolStripButton tbStart;
		private System.Windows.Forms.ToolStrip toolStrip1;

		private void InitializeComponent()
		{
			this.toolStrip1 = new System.Windows.Forms.ToolStrip();
			this.tbStart = new System.Windows.Forms.ToolStripButton();
			this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
			this.tbPrev = new System.Windows.Forms.ToolStripButton();
			this.tbNext = new System.Windows.Forms.ToolStripButton();
			this.toolStrip1.SuspendLayout();
			this.SuspendLayout();
			// 
			// toolStrip1
			// 
			this.toolStrip1.ImageScalingSize = new System.Drawing.Size(20, 20);
			this.toolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
									this.tbStart,
									this.toolStripSeparator1,
									this.tbPrev,
									this.tbNext});
			this.toolStrip1.Location = new System.Drawing.Point(0, 0);
			this.toolStrip1.Name = "toolStrip1";
			this.toolStrip1.Size = new System.Drawing.Size(792, 27);
			this.toolStrip1.TabIndex = 0;
			this.toolStrip1.Text = "toolStrip1";
			// 
			// tbStart
			// 
			this.tbStart.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
			this.tbStart.ImageTransparentColor = System.Drawing.Color.Magenta;
			this.tbStart.Name = "tbStart";
			this.tbStart.Size = new System.Drawing.Size(24, 24);
			this.tbStart.Text = "tbStart";
			this.tbStart.Click += new System.EventHandler(this.tsbStart_Click);
			// 
			// toolStripSeparator1
			// 
			this.toolStripSeparator1.Name = "toolStripSeparator1";
			this.toolStripSeparator1.Size = new System.Drawing.Size(6, 27);
			// 
			// tbPrev
			// 
			this.tbPrev.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
			this.tbPrev.ImageTransparentColor = System.Drawing.Color.Magenta;
			this.tbPrev.Name = "tbPrev";
			this.tbPrev.Size = new System.Drawing.Size(24, 24);
			this.tbPrev.Text = "tbPrev";
			this.tbPrev.Click += new System.EventHandler(this.tsbPrev_Click);
			// 
			// tbNext
			// 
			this.tbNext.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
			this.tbNext.ImageTransparentColor = System.Drawing.Color.Magenta;
			this.tbNext.Name = "tbNext";
			this.tbNext.Size = new System.Drawing.Size(24, 24);
			this.tbNext.Text = "tbNext";
			this.tbNext.Click += new System.EventHandler(this.tsbNext_Click);
			// 
			// SlideshowWin
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.ClientSize = new System.Drawing.Size(792, 573);
			this.Controls.Add(this.toolStrip1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.KeyPreview = true;
			this.Name = "SlideshowWin";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "SlideshowWin";
			this.FormClosed += new System.Windows.Forms.FormClosedEventHandler(this.SlideshowWin_Closed);
			this.Load += new System.EventHandler(this.SlideshowWin_Load);
			this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.SlideshowWin_KeyDown);
			this.toolStrip1.ResumeLayout(false);
			this.toolStrip1.PerformLayout();
			this.ResumeLayout(false);
			this.PerformLayout();
		}
	}
}
