namespace GKUI
{
	partial class SlideshowWin
	{
		private void InitializeComponent()
		{
			this.components = new System.ComponentModel.Container();
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(SlideshowWin));
			this.toolStrip1 = new System.Windows.Forms.ToolStrip();
			this.tsbStart = new System.Windows.Forms.ToolStripButton();
			this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
			this.tsbPrev = new System.Windows.Forms.ToolStripButton();
			this.tsbNext = new System.Windows.Forms.ToolStripButton();
			this.timer1 = new System.Windows.Forms.Timer(this.components);
			this.toolStrip1.SuspendLayout();
			this.SuspendLayout();
			// 
			// toolStrip1
			// 
			this.toolStrip1.ImageScalingSize = new System.Drawing.Size(20, 20);
			this.toolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
									this.tsbStart,
									this.toolStripSeparator1,
									this.tsbPrev,
									this.tsbNext});
			this.toolStrip1.Location = new System.Drawing.Point(0, 0);
			this.toolStrip1.Name = "toolStrip1";
			this.toolStrip1.Size = new System.Drawing.Size(792, 27);
			this.toolStrip1.TabIndex = 0;
			this.toolStrip1.Text = "toolStrip1";
			// 
			// tsbStart
			// 
			this.tsbStart.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
			this.tsbStart.Image = ((System.Drawing.Image)(resources.GetObject("tsbStart.Image")));
			this.tsbStart.ImageTransparentColor = System.Drawing.Color.Magenta;
			this.tsbStart.Name = "tsbStart";
			this.tsbStart.Size = new System.Drawing.Size(24, 24);
			this.tsbStart.Text = "Старт";
			this.tsbStart.Click += new System.EventHandler(this.tsbStart_Click);
			// 
			// toolStripSeparator1
			// 
			this.toolStripSeparator1.Name = "toolStripSeparator1";
			this.toolStripSeparator1.Size = new System.Drawing.Size(6, 27);
			// 
			// tsbPrev
			// 
			this.tsbPrev.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
			this.tsbPrev.Image = ((System.Drawing.Image)(resources.GetObject("tsbPrev.Image")));
			this.tsbPrev.ImageTransparentColor = System.Drawing.Color.Magenta;
			this.tsbPrev.Name = "tsbPrev";
			this.tsbPrev.Size = new System.Drawing.Size(24, 24);
			this.tsbPrev.Text = "toolStripButton2";
			this.tsbPrev.Click += new System.EventHandler(this.tsbPrev_Click);
			// 
			// tsbNext
			// 
			this.tsbNext.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
			this.tsbNext.Image = ((System.Drawing.Image)(resources.GetObject("tsbNext.Image")));
			this.tsbNext.ImageTransparentColor = System.Drawing.Color.Magenta;
			this.tsbNext.Name = "tsbNext";
			this.tsbNext.Size = new System.Drawing.Size(24, 24);
			this.tsbNext.Text = "toolStripButton3";
			this.tsbNext.Click += new System.EventHandler(this.tsbNext_Click);
			// 
			// timer1
			// 
			this.timer1.Interval = 1000;
			this.timer1.Tick += new System.EventHandler(this.Timer1Tick);
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
			this.Text = "Просмотр";
			this.Load += new System.EventHandler(this.TfmSlideshow_Load);
			this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.TfmMediaView_KeyDown);
			this.toolStrip1.ResumeLayout(false);
			this.toolStrip1.PerformLayout();
			this.ResumeLayout(false);
			this.PerformLayout();
		}
		private System.ComponentModel.IContainer components;
		private System.Windows.Forms.Timer timer1;
		private System.Windows.Forms.ToolStripButton tsbNext;
		private System.Windows.Forms.ToolStripButton tsbPrev;
		private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
		private System.Windows.Forms.ToolStripButton tsbStart;
		private System.Windows.Forms.ToolStrip toolStrip1;
	}
}