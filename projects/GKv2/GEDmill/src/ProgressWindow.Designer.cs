using System.Windows.Forms;

namespace GEDmill
{
    public partial class ProgressWindow
    {
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Label m_label;
        private System.Windows.Forms.ProgressBar m_progressbar;
        private System.ComponentModel.Container components = null;

        private void InitializeComponent()
        {
            this.m_progressbar = new System.Windows.Forms.ProgressBar();
            this.m_label = new System.Windows.Forms.Label();
            this.btnCancel = new System.Windows.Forms.Button();
            this.SuspendLayout();

            // 
            // progressBar
            // 
            this.m_progressbar.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                | System.Windows.Forms.AnchorStyles.Right)));
            this.m_progressbar.Location = new System.Drawing.Point(10, 55);
            this.m_progressbar.Name = "progressBar";
            this.m_progressbar.Size = new System.Drawing.Size(230, 27);
            this.m_progressbar.TabIndex = 1;

            // 
            // label
            // 
            this.m_label.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                | System.Windows.Forms.AnchorStyles.Left)
                | System.Windows.Forms.AnchorStyles.Right)));
            this.m_label.Location = new System.Drawing.Point(10, 9);
            this.m_label.Name = "label";
            this.m_label.Size = new System.Drawing.Size(326, 37);
            this.m_label.TabIndex = 0;

            // 
            // btnCancel
            // 
            this.btnCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.btnCancel.Enabled = false;
            this.btnCancel.Location = new System.Drawing.Point(250, 55);
            this.btnCancel.Name = "cancelButton";
            this.btnCancel.Size = new System.Drawing.Size(90, 27);
            this.btnCancel.TabIndex = 2;
            this.btnCancel.Text = "Cancel";
            this.btnCancel.Click += new System.EventHandler(this.OnCancelClicked);

            // 
            // ProgressWindow
            // 
            this.AutoScaleMode = AutoScaleMode.None;
            this.AutoScaleBaseSize = new System.Drawing.Size(6, 15);
            this.ClientSize = new System.Drawing.Size(348, 91);
            this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.m_progressbar);
            this.Controls.Add(this.m_label);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.Name = "ProgressWindow";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "ProgressWindow";
            this.ResumeLayout(false);
        }
    }
}
