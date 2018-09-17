namespace GKSamplePlugin
{
    partial class PluginForm
    {
        private System.ComponentModel.IContainer components = null;

        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            this.tbInfo = new System.Windows.Forms.TextBox();
            this.SuspendLayout();
            // 
            // tbInfo
            // 
            this.tbInfo.Location = new System.Drawing.Point(0, 0);
            this.tbInfo.Multiline = true;
            this.tbInfo.Name = "tbInfo";
            this.tbInfo.ReadOnly = true;
            this.tbInfo.Size = new System.Drawing.Size(154, 93);
            this.tbInfo.TabIndex = 0;
            // 
            // frmP1Main
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoSize = true;
            this.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.ClientSize = new System.Drawing.Size(155, 95);
            this.Controls.Add(this.tbInfo);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "frmP1Main";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "frmP1Main";
            this.Load += new System.EventHandler(this.frmP1Main_Load);
            this.ResumeLayout(false);
            this.PerformLayout();
        }

        private System.Windows.Forms.TextBox tbInfo;
    }
}
