namespace GKUI.Forms
{
	partial class SexCheckDlg
	{
		private System.Windows.Forms.TextBox txtName;
		private System.Windows.Forms.GroupBox grpSex;
		private System.Windows.Forms.RadioButton rbNone;
		private System.Windows.Forms.RadioButton rbMale;
		private System.Windows.Forms.RadioButton rbFemale;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;

		private void InitializeComponent()
		{
			this.txtName = new System.Windows.Forms.TextBox();
			this.grpSex = new System.Windows.Forms.GroupBox();
			this.rbNone = new System.Windows.Forms.RadioButton();
			this.rbMale = new System.Windows.Forms.RadioButton();
			this.rbFemale = new System.Windows.Forms.RadioButton();
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.grpSex.SuspendLayout();
			this.SuspendLayout();
			// 
			// txtName
			// 
			this.txtName.Location = new System.Drawing.Point(11, 10);
			this.txtName.Name = "txtName";
			this.txtName.ReadOnly = true;
			this.txtName.Size = new System.Drawing.Size(483, 24);
			this.txtName.TabIndex = 0;
			// 
			// grpSex
			// 
			this.grpSex.Controls.Add(this.rbNone);
			this.grpSex.Controls.Add(this.rbMale);
			this.grpSex.Controls.Add(this.rbFemale);
			this.grpSex.Location = new System.Drawing.Point(11, 43);
			this.grpSex.Name = "grpSex";
			this.grpSex.Size = new System.Drawing.Size(483, 59);
			this.grpSex.TabIndex = 1;
			this.grpSex.TabStop = false;
			this.grpSex.Text = "grpSex";
			// 
			// rbNone
			// 
			this.rbNone.AutoSize = true;
			this.rbNone.Location = new System.Drawing.Point(11, 19);
			this.rbNone.Name = "rbNone";
			this.rbNone.Size = new System.Drawing.Size(36, 21);
			this.rbNone.TabIndex = 0;
			this.rbNone.Text = "?";
			// 
			// rbMale
			// 
			this.rbMale.AutoSize = true;
			this.rbMale.Location = new System.Drawing.Point(167, 19);
			this.rbMale.Name = "rbMale";
			this.rbMale.Size = new System.Drawing.Size(87, 21);
			this.rbMale.TabIndex = 1;
			this.rbMale.Text = "rbMale";
			// 
			// rbFemale
			// 
			this.rbFemale.AutoSize = true;
			this.rbFemale.Location = new System.Drawing.Point(323, 19);
			this.rbFemale.Name = "rbFemale";
			this.rbFemale.Size = new System.Drawing.Size(88, 21);
			this.rbFemale.TabIndex = 2;
			this.rbFemale.Text = "rbFemale";
			// 
			// btnAccept
			// 
			this.btnAccept.DialogResult = System.Windows.Forms.DialogResult.OK;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(258, 117);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(113, 30);
			this.btnAccept.TabIndex = 2;
			this.btnAccept.Text = "btnAccept";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.AcceptClickHandler);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(381, 117);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(113, 30);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "btnCancel";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnCancel.Click += new System.EventHandler(this.CancelClickHandler);
            // 
            // SexCheckDlg
            // 
            this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(504, 158);
			this.Controls.Add(this.txtName);
			this.Controls.Add(this.grpSex);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
			this.Name = "SexCheckDlg";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "SexCheckDlg";
			this.TopMost = true;
			this.grpSex.ResumeLayout(false);
			this.grpSex.PerformLayout();
			this.ResumeLayout(false);
			this.PerformLayout();
		}
	}
}
