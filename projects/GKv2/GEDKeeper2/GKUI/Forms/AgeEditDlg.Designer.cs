namespace GKUI.Forms
{
    partial class AgeEditDlg
    {
        private System.Windows.Forms.Button btnAccept;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.GroupBox grpBox;
        private System.Windows.Forms.ComboBox cmbRel1;
        private System.Windows.Forms.ComboBox cmbRel2;
        private System.Windows.Forms.Label lblAge1;
        private System.Windows.Forms.MaskedTextBox txtVal1;
        private System.Windows.Forms.Label lblAge2;
        private System.Windows.Forms.MaskedTextBox txtVal2;

        private void InitializeComponent()
        {
            this.btnAccept = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.grpBox = new System.Windows.Forms.GroupBox();
            this.lblAge1 = new System.Windows.Forms.Label();
            this.cmbRel1 = new System.Windows.Forms.ComboBox();
            this.txtVal1 = new System.Windows.Forms.MaskedTextBox();
            this.lblAge2 = new System.Windows.Forms.Label();
            this.cmbRel2 = new System.Windows.Forms.ComboBox();
            this.txtVal2 = new System.Windows.Forms.MaskedTextBox();
            this.grpBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // btnAccept
            // 
            this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnAccept.Location = new System.Drawing.Point(234, 114);
            this.btnAccept.Name = "btnAccept";
            this.btnAccept.Size = new System.Drawing.Size(114, 30);
            this.btnAccept.TabIndex = 5;
            this.btnAccept.Text = "btnAccept";
            this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.AcceptClickHandler);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(358, 114);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(113, 30);
            this.btnCancel.TabIndex = 6;
            this.btnCancel.Text = "btnCancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnCancel.Click += new System.EventHandler(this.CancelClickHandler);
            // 
            // grpBox
            // 
            this.grpBox.Controls.Add(this.lblAge1);
            this.grpBox.Controls.Add(this.lblAge2);
            this.grpBox.Controls.Add(this.cmbRel1);
            this.grpBox.Controls.Add(this.cmbRel2);
            this.grpBox.Controls.Add(this.txtVal1);
            this.grpBox.Controls.Add(this.txtVal2);
            this.grpBox.Location = new System.Drawing.Point(11, 11);
            this.grpBox.Name = "grpBox";
            this.grpBox.Size = new System.Drawing.Size(460, 90);
            this.grpBox.TabIndex = 4;
            this.grpBox.TabStop = false;
            this.grpBox.Text = "";
            // 
            // lblAge1
            // 
            this.lblAge1.AutoSize = true;
            this.lblAge1.Location = new System.Drawing.Point(11, 22);
            this.lblAge1.Name = "lblAge1";
            this.lblAge1.Size = new System.Drawing.Size(66, 17);
            this.lblAge1.TabIndex = 0;
            this.lblAge1.Text = "lblAge1";
            // 
            // lblAge2
            // 
            this.lblAge2.AutoSize = true;
            this.lblAge2.Location = new System.Drawing.Point(11, 52);
            this.lblAge2.Name = "lblAge2";
            this.lblAge2.Size = new System.Drawing.Size(65, 17);
            this.lblAge2.TabIndex = 2;
            this.lblAge2.Text = "lblAge2";
            // 
            // cmbRel1
            // 
            this.cmbRel1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbRel1.Location = new System.Drawing.Point(170, 16);
            this.cmbRel1.Name = "cmbRel1";
            this.cmbRel1.Size = new System.Drawing.Size(50, 25);
            this.cmbRel1.TabIndex = 3;
            // 
            // cmbRel2
            // 
            this.cmbRel2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbRel2.Location = new System.Drawing.Point(170, 46);
            this.cmbRel2.Name = "cmbRel2";
            this.cmbRel2.Size = new System.Drawing.Size(50, 25);
            this.cmbRel2.TabIndex = 3;
            // 
            // txtVal1
            // 
            this.txtVal1.Location = new System.Drawing.Point(240, 16);
            this.txtVal1.Name = "txtVal1";
            this.txtVal1.Size = new System.Drawing.Size(209, 24);
            this.txtVal1.TabIndex = 1;
            // 
            // txtVal2
            // 
            this.txtVal2.Location = new System.Drawing.Point(240, 46);
            this.txtVal2.Name = "txtVal2";
            this.txtVal2.Size = new System.Drawing.Size(209, 24);
            this.txtVal2.TabIndex = 3;
            // 
            // AgeEditDlg
            // 
            this.AcceptButton = this.btnAccept;
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(485, 160);
            this.Controls.Add(this.btnAccept);
            this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.grpBox);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "AgeEditDlg";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "AgeEditDlg";
            this.grpBox.ResumeLayout(false);
            this.grpBox.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();
        }
    }
}
