namespace GKUI.Forms
{
    partial class LanguageSelectDlg
    {
        private System.ComponentModel.IContainer components = null;
        
        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null) {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }
        
        private void InitializeComponent()
        {
            this.btnAccept = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.lstLanguages = new GKUI.Components.GKListView();
            this.SuspendLayout();
            // 
            // btnAccept
            // 
            this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnAccept.Location = new System.Drawing.Point(316, 16);
            this.btnAccept.Margin = new System.Windows.Forms.Padding(16, 0, 0, 0);
            this.btnAccept.Name = "btnAccept";
            this.btnAccept.Size = new System.Drawing.Size(114, 32);
            this.btnAccept.TabIndex = 7;
            this.btnAccept.Text = "OK";
            this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(317, 64);
            this.btnCancel.Margin = new System.Windows.Forms.Padding(0, 16, 0, 0);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(113, 32);
            this.btnCancel.TabIndex = 8;
            this.btnCancel.Text = "Cancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // lstLanguages
            // 
            this.lstLanguages.Dock = System.Windows.Forms.DockStyle.Left;
            this.lstLanguages.Location = new System.Drawing.Point(16, 16);
            this.lstLanguages.Margin = new System.Windows.Forms.Padding(0);
            this.lstLanguages.Name = "lstLanguages";
            this.lstLanguages.Size = new System.Drawing.Size(278, 377);
            this.lstLanguages.TabIndex = 9;
            this.lstLanguages.View = System.Windows.Forms.View.List;
            // 
            // LanguageSelectDlg
            // 
            this.AcceptButton = this.btnAccept;
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(446, 409);
            this.Controls.Add(this.lstLanguages);
            this.Controls.Add(this.btnAccept);
            this.Controls.Add(this.btnCancel);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "LanguageSelectDlg";
            this.Padding = new System.Windows.Forms.Padding(16);
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Select language";
            this.ResumeLayout(false);
        }
        private GKUI.Components.GKListView lstLanguages;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Button btnAccept;
    }
}
