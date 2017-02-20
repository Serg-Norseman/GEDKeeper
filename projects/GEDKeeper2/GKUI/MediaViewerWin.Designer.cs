namespace GKUI
{
	partial class MediaViewerWin
	{
		private void InitializeComponent()
		{
		    this.SuspendLayout();
		    // 
		    // MediaViewerWin
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.ClientSize = new System.Drawing.Size(822, 462);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.KeyPreview = true;
		    this.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
		    this.Name = "MediaViewerWin";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
		    this.Text = "MediaViewerWin";
		    this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.MediaViewerWinFormClosing);
		    this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.MediaViewerWin_KeyDown);
		    this.ResumeLayout(false);

		}
	}
}
