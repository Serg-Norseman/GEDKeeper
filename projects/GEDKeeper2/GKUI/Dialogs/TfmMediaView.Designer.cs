using System;

namespace GKUI.Dialogs
{
	partial class TfmMediaView
	{
		private void InitializeComponent()
		{
			this.SuspendLayout();
			// 
			// TfmMediaView
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.ClientSize = new System.Drawing.Size(792, 573);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.KeyPreview = true;
			this.Name = "TfmMediaView";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Просмотр";
			this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.TfmMediaView_KeyDown);
			this.ResumeLayout(false);
		}
	}
}