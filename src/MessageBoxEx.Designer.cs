using System.Drawing;
using System.Windows.Forms;

namespace GEDmill
{
    public partial class MessageBoxEx
    {
        private System.ComponentModel.Container components = null;
        private System.Windows.Forms.TextBox m_textbox;
        private System.Windows.Forms.Button m_buttonOk;
        private System.Windows.Forms.Button m_buttonCancel;
        private System.Windows.Forms.Button m_buttonYes;
        private System.Windows.Forms.Button m_buttonNo;
        private System.Windows.Forms.Button m_buttonRetry;
        private System.Windows.Forms.Label m_label;

        private void InitializeComponent()
        {
            this.m_textbox = new System.Windows.Forms.TextBox();
            this.m_buttonOk = new System.Windows.Forms.Button();
            this.m_buttonCancel = new System.Windows.Forms.Button();
            this.m_buttonYes = new System.Windows.Forms.Button();
            this.m_buttonNo = new System.Windows.Forms.Button();
            this.m_buttonRetry = new System.Windows.Forms.Button();
            this.m_label = new System.Windows.Forms.Label();
            this.SuspendLayout();

            // 
            // textBox
            // 
            this.m_textbox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                | System.Windows.Forms.AnchorStyles.Left)
                | System.Windows.Forms.AnchorStyles.Right)));
            this.m_textbox.BackColor = System.Drawing.SystemColors.Control;
            this.m_textbox.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.m_textbox.CausesValidation = false;
            this.m_textbox.Location = new System.Drawing.Point(154, 0);
            this.m_textbox.Multiline = true;
            this.m_textbox.Name = "textBox";
            this.m_textbox.ReadOnly = true;
            this.m_textbox.Size = new System.Drawing.Size(345, 92);
            this.m_textbox.TabIndex = 5;
            this.m_textbox.Text = "textBox1";
            this.m_textbox.TextChanged += new System.EventHandler(this.textBox_TextChanged);

            // 
            // OKbutton
            // 
            this.m_buttonOk.Anchor = System.Windows.Forms.AnchorStyles.Bottom;
            this.m_buttonOk.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.m_buttonOk.Location = new System.Drawing.Point(150, 240);
            this.m_buttonOk.Name = "OKbutton";
            this.m_buttonOk.Size = new System.Drawing.Size(90, 27);
            this.m_buttonOk.TabIndex = 3;
            this.m_buttonOk.Text = "OK";

            // 
            // cancelButton
            // 
            this.m_buttonCancel.Anchor = System.Windows.Forms.AnchorStyles.Bottom;
            this.m_buttonCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.m_buttonCancel.Location = new System.Drawing.Point(256, 240);
            this.m_buttonCancel.Name = "cancelButton";
            this.m_buttonCancel.Size = new System.Drawing.Size(90, 27);
            this.m_buttonCancel.TabIndex = 4;
            this.m_buttonCancel.Text = "Cancel";

            // 
            // yesButton
            // 
            this.m_buttonYes.Anchor = System.Windows.Forms.AnchorStyles.Bottom;
            this.m_buttonYes.DialogResult = System.Windows.Forms.DialogResult.Yes;
            this.m_buttonYes.Location = new System.Drawing.Point(38, 240);
            this.m_buttonYes.Name = "yesButton";
            this.m_buttonYes.Size = new System.Drawing.Size(90, 27);
            this.m_buttonYes.TabIndex = 1;
            this.m_buttonYes.Text = "Yes";

            // 
            // noButton
            // 
            this.m_buttonNo.Anchor = System.Windows.Forms.AnchorStyles.Bottom;
            this.m_buttonNo.DialogResult = System.Windows.Forms.DialogResult.No;
            this.m_buttonNo.Location = new System.Drawing.Point(365, 240);
            this.m_buttonNo.Name = "noButton";
            this.m_buttonNo.Size = new System.Drawing.Size(90, 27);
            this.m_buttonNo.TabIndex = 2;
            this.m_buttonNo.Text = "No";

            // 
            // retryButton
            // 
            this.m_buttonRetry.Anchor = System.Windows.Forms.AnchorStyles.Bottom;
            this.m_buttonRetry.DialogResult = System.Windows.Forms.DialogResult.Retry;
            this.m_buttonRetry.Location = new System.Drawing.Point(474, 240);
            this.m_buttonRetry.Name = "retryButton";
            this.m_buttonRetry.Size = new System.Drawing.Size(90, 27);
            this.m_buttonRetry.TabIndex = 1;
            this.m_buttonRetry.Text = "Retry";

            // 
            // label
            // 
            this.m_label.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                | System.Windows.Forms.AnchorStyles.Left)
                | System.Windows.Forms.AnchorStyles.Right)));
            this.m_label.BackColor = System.Drawing.SystemColors.Control;
            this.m_label.Location = new System.Drawing.Point(0, 111);
            this.m_label.Name = "label";
            this.m_label.Size = new System.Drawing.Size(499, 111);
            this.m_label.TabIndex = 6;
            this.m_label.Text = "label";
            this.m_label.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;

            // 
            // MessageBocx
            // 
            this.AutoScaleMode = AutoScaleMode.None;
            this.AutoScaleBaseSize = new System.Drawing.Size(6, 15);
            this.ClientSize = new System.Drawing.Size(501, 275);
            this.Controls.Add(this.m_buttonNo);
            this.Controls.Add(this.m_buttonYes);
            this.Controls.Add(this.m_label);
            this.Controls.Add(this.m_buttonCancel);
            this.Controls.Add(this.m_buttonOk);
            this.Controls.Add(this.m_buttonRetry);
            this.Controls.Add(this.m_textbox);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "MessageBocx";
            this.ShowInTaskbar = false;
            this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Hide;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "MessageBox";
            this.Paint += new System.Windows.Forms.PaintEventHandler(this.messageBox_onPaint);
            this.ResumeLayout(false);
        }        
    }
}
