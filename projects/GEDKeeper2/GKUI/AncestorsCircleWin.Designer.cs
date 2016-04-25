using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace GKUI
{
	partial class AncestorsCircleWin
	{

		protected override void Dispose(bool disposing)
		{
            if (disposing)
            {
                this.fAncestorsCircle.Dispose();
            }
			base.Dispose(disposing);
		}

		private void InitializeComponent()
		{
			this.SuspendLayout();
			// 
			// AncestorsCircleWin
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.ClientSize = new System.Drawing.Size(1093, 579);
			this.KeyPreview = true;
			this.Margin = new System.Windows.Forms.Padding(4);
			this.Name = "AncestorsCircleWin";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Circle of Ancestors";
			this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.TfmAncestors_KeyDown);
			this.ResumeLayout(false);
		}
	}
}
