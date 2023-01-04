namespace GKDataQualityPlugin
{
    partial class DataQualityWidget
    {
        private System.ComponentModel.IContainer components = null;

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null) {
                    components.Dispose();
                }
                fPlugin.CloseForm();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.SuspendLayout();
            // 
            // DataQualityWidget
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(350, 248);
            this.Name = "DataQualityWidget";
            this.Text = "DataQualityWidget";
            this.Closed += new System.EventHandler(this.DataQualityWidget_Closed);
            this.Load += new System.EventHandler(this.DataQualityWidget_Load);
            this.ResumeLayout(false);
        }
    }
}
