using System;

namespace GKUI.Dialogs
{
	partial class MediaViewerWin
	{
		private void InitializeComponent()
		{
			this.SuspendLayout();
			// 
			// MediaViewerWin
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.ClientSize = new System.Drawing.Size(1027, 577);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.KeyPreview = true;
			this.Name = "MediaViewerWin";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Просмотр";
			this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.TfmMediaView_KeyDown);
			this.ResumeLayout(false);
		}
	}
}