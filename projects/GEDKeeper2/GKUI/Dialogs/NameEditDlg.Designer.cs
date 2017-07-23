namespace GKUI.Dialogs
{
	partial class NameEditDlg
	{
		private System.Windows.Forms.Label lblName;
		private System.Windows.Forms.TextBox txtName;
		private System.Windows.Forms.Label lblSex;
		private System.Windows.Forms.ComboBox cmbSex;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.GroupBox grpPatronymics;
		private System.Windows.Forms.Label lblFemale;
		private System.Windows.Forms.TextBox txtFPatr;
		private System.Windows.Forms.Label lblMale;
		private System.Windows.Forms.TextBox txtMPatr;

		private void InitializeComponent()
		{
			this.lblName = new System.Windows.Forms.Label();
			this.txtName = new System.Windows.Forms.TextBox();
			this.lblSex = new System.Windows.Forms.Label();
			this.cmbSex = new System.Windows.Forms.ComboBox();
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.grpPatronymics = new System.Windows.Forms.GroupBox();
			this.lblFemale = new System.Windows.Forms.Label();
			this.lblMale = new System.Windows.Forms.Label();
			this.txtFPatr = new System.Windows.Forms.TextBox();
			this.txtMPatr = new System.Windows.Forms.TextBox();
			this.grpPatronymics.SuspendLayout();
			this.SuspendLayout();
			// 
			// lblName
			// 
			this.lblName.AutoSize = true;
			this.lblName.Location = new System.Drawing.Point(12, 13);
			this.lblName.Name = "lblName";
			this.lblName.Size = new System.Drawing.Size(33, 17);
			this.lblName.TabIndex = 0;
			this.lblName.Text = "lblName";
			// 
			// txtName
			// 
			this.txtName.Location = new System.Drawing.Point(101, 10);
			this.txtName.Name = "txtName";
			this.txtName.Size = new System.Drawing.Size(270, 24);
			this.txtName.TabIndex = 1;
			this.txtName.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edName_KeyPress);
			// 
			// lblSex
			// 
			this.lblSex.AutoSize = true;
			this.lblSex.Location = new System.Drawing.Point(12, 52);
			this.lblSex.Name = "lblSex";
			this.lblSex.Size = new System.Drawing.Size(33, 17);
			this.lblSex.TabIndex = 2;
			this.lblSex.Text = "lblSex";
			// 
			// cmbSex
			// 
			this.cmbSex.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cmbSex.Location = new System.Drawing.Point(101, 49);
			this.cmbSex.Name = "cmbSex";
			this.cmbSex.Size = new System.Drawing.Size(169, 25);
			this.cmbSex.TabIndex = 3;
			// 
			// btnAccept
			// 
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(134, 204);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(114, 30);
			this.btnAccept.TabIndex = 5;
			this.btnAccept.Text = "btnAccept";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(258, 204);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(113, 30);
			this.btnCancel.TabIndex = 6;
			this.btnCancel.Text = "btnCancel";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// grpPatronymics
			// 
			this.grpPatronymics.Controls.Add(this.lblFemale);
			this.grpPatronymics.Controls.Add(this.lblMale);
			this.grpPatronymics.Controls.Add(this.txtFPatr);
			this.grpPatronymics.Controls.Add(this.txtMPatr);
			this.grpPatronymics.Location = new System.Drawing.Point(11, 87);
			this.grpPatronymics.Name = "grpPatronymics";
			this.grpPatronymics.Size = new System.Drawing.Size(360, 95);
			this.grpPatronymics.TabIndex = 4;
			this.grpPatronymics.TabStop = false;
			this.grpPatronymics.Text = "grpPatronymics";
			// 
			// lblFemale
			// 
			this.lblFemale.AutoSize = true;
			this.lblFemale.Location = new System.Drawing.Point(11, 22);
			this.lblFemale.Name = "lblFemale";
			this.lblFemale.Size = new System.Drawing.Size(66, 17);
			this.lblFemale.TabIndex = 0;
			this.lblFemale.Text = "lblFemale";
			// 
			// lblMale
			// 
			this.lblMale.AutoSize = true;
			this.lblMale.Location = new System.Drawing.Point(11, 61);
			this.lblMale.Name = "lblMale";
			this.lblMale.Size = new System.Drawing.Size(65, 17);
			this.lblMale.TabIndex = 2;
			this.lblMale.Text = "lblMale";
			// 
			// txtFPatr
			// 
			this.txtFPatr.Location = new System.Drawing.Point(90, 19);
			this.txtFPatr.Name = "txtFPatr";
			this.txtFPatr.Size = new System.Drawing.Size(259, 24);
			this.txtFPatr.TabIndex = 1;
			this.txtFPatr.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edName_KeyPress);
			// 
			// txtMPatr
			// 
			this.txtMPatr.Location = new System.Drawing.Point(90, 58);
			this.txtMPatr.Name = "txtMPatr";
			this.txtMPatr.Size = new System.Drawing.Size(259, 24);
			this.txtMPatr.TabIndex = 3;
			this.txtMPatr.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edName_KeyPress);
			// 
			// NameEditDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(385, 250);
			this.Controls.Add(this.lblName);
			this.Controls.Add(this.txtName);
			this.Controls.Add(this.lblSex);
			this.Controls.Add(this.cmbSex);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.grpPatronymics);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "NameEditDlg";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "NameEditDlg";
			this.grpPatronymics.ResumeLayout(false);
			this.grpPatronymics.PerformLayout();
			this.ResumeLayout(false);
			this.PerformLayout();
		}
	}
}