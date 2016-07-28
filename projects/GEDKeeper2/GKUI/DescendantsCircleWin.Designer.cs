using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace GKUI
{
    partial class DescendantsCircleWin
    {

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fDescendantsCircle.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            this.SuspendLayout();
            // 
            // DescendantsCircleWin
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.ClientSize = new System.Drawing.Size(1093, 579);
            this.KeyPreview = true;
            this.Margin = new System.Windows.Forms.Padding(4);
            this.Name = "DescendantsCircleWin";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "DescendantsCircleWin";
            this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.DescendantsCircleWin_KeyDown);
            this.ResumeLayout(false);
        }
    }
}
