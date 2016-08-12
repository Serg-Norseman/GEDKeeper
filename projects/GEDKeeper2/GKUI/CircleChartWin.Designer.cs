using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace GKUI
{
	partial class CircleChartWin
	{

		protected override void Dispose(bool disposing)
		{
            if (disposing)
            {
                this.fCircleChart.Dispose();
            }
			base.Dispose(disposing);
		}

		private void InitializeComponent()
		{
			this.SuspendLayout();
			// 
			// CircleChartWin
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.ClientSize = new System.Drawing.Size(1093, 579);
			this.KeyPreview = true;
			this.Margin = new System.Windows.Forms.Padding(4);
			this.Name = "CircleChartWin";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "CircleChartWin";
			this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.CircleChartWin_KeyDown);
			this.ResumeLayout(false);
		}
	}
}
