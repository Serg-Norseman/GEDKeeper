namespace GKUI.Dialogs
{
	partial class PersonalNameEditDlg
	{
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.IContainer components = null;

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		/// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
		protected override void Dispose(bool disposing)
		{
			if (disposing && (components != null))
			{
				components.Dispose();
			}
			base.Dispose(disposing);
		}

		#region Windows Form Designer generated code

		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.lblSurname = new System.Windows.Forms.Label();
			this.btnAccept = new System.Windows.Forms.Button();
			this.lblName = new System.Windows.Forms.Label();
			this.lblPatronymic = new System.Windows.Forms.Label();
			this.btnCancel = new System.Windows.Forms.Button();
			this.lblSurnamePrefix = new System.Windows.Forms.Label();
			this.lblNamePrefix = new System.Windows.Forms.Label();
			this.lblNameSuffix = new System.Windows.Forms.Label();
			this.lblNickname = new System.Windows.Forms.Label();
			this.txtSurname = new System.Windows.Forms.TextBox();
			this.txtName = new System.Windows.Forms.TextBox();
			this.txtPatronymic = new System.Windows.Forms.TextBox();
			this.txtSurnamePrefix = new System.Windows.Forms.TextBox();
			this.txtNamePrefix = new System.Windows.Forms.TextBox();
			this.txtNameSuffix = new System.Windows.Forms.TextBox();
			this.txtNickname = new System.Windows.Forms.TextBox();
			this.lblType = new System.Windows.Forms.Label();
			this.cmbNameType = new System.Windows.Forms.ComboBox();
			this.SuspendLayout();
			// 
			// lblSurname
			// 
			this.lblSurname.AutoSize = true;
			this.lblSurname.Location = new System.Drawing.Point(10, 7);
			this.lblSurname.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.lblSurname.Name = "lblSurname";
			this.lblSurname.Size = new System.Drawing.Size(51, 13);
			this.lblSurname.TabIndex = 14;
			this.lblSurname.Text = "lblSurname";
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(157, 177);
			this.btnAccept.Margin = new System.Windows.Forms.Padding(2);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(91, 24);
			this.btnAccept.TabIndex = 19;
			this.btnAccept.Text = "btnAccept";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// lblName
			// 
			this.lblName.AutoSize = true;
			this.lblName.Location = new System.Drawing.Point(10, 46);
			this.lblName.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.lblName.Name = "lblName";
			this.lblName.Size = new System.Drawing.Size(26, 13);
			this.lblName.TabIndex = 16;
			this.lblName.Text = "lblName";
			// 
			// lblPatronymic
			// 
			this.lblPatronymic.AutoSize = true;
			this.lblPatronymic.Location = new System.Drawing.Point(10, 86);
			this.lblPatronymic.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.lblPatronymic.Name = "lblPatronymic";
			this.lblPatronymic.Size = new System.Drawing.Size(56, 13);
			this.lblPatronymic.TabIndex = 18;
			this.lblPatronymic.Text = "lblPatronymic";
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(252, 177);
			this.btnCancel.Margin = new System.Windows.Forms.Padding(2);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(91, 24);
			this.btnCancel.TabIndex = 22;
			this.btnCancel.Text = "btnCancel";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// lblSurnamePrefix
			// 
			this.lblSurnamePrefix.AutoSize = true;
			this.lblSurnamePrefix.Location = new System.Drawing.Point(207, 7);
			this.lblSurnamePrefix.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.lblSurnamePrefix.Name = "lblSurnamePrefix";
			this.lblSurnamePrefix.Size = new System.Drawing.Size(98, 13);
			this.lblSurnamePrefix.TabIndex = 24;
			this.lblSurnamePrefix.Text = "lblSurnamePrefix";
			// 
			// lblNamePrefix
			// 
			this.lblNamePrefix.AutoSize = true;
			this.lblNamePrefix.Location = new System.Drawing.Point(207, 46);
			this.lblNamePrefix.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.lblNamePrefix.Name = "lblNamePrefix";
			this.lblNamePrefix.Size = new System.Drawing.Size(84, 13);
			this.lblNamePrefix.TabIndex = 26;
			this.lblNamePrefix.Text = "lblNamePrefix";
			// 
			// lblNameSuffix
			// 
			this.lblNameSuffix.AutoSize = true;
			this.lblNameSuffix.Location = new System.Drawing.Point(207, 86);
			this.lblNameSuffix.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.lblNameSuffix.Name = "lblNameSuffix";
			this.lblNameSuffix.Size = new System.Drawing.Size(86, 13);
			this.lblNameSuffix.TabIndex = 28;
			this.lblNameSuffix.Text = "lblNameSuffix";
			// 
			// lblNickname
			// 
			this.lblNickname.AutoSize = true;
			this.lblNickname.Location = new System.Drawing.Point(205, 124);
			this.lblNickname.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.lblNickname.Name = "lblNickname";
			this.lblNickname.Size = new System.Drawing.Size(58, 13);
			this.lblNickname.TabIndex = 21;
			this.lblNickname.Text = "lblNickname";
			// 
			// txtSurname
			// 
			this.txtSurname.Location = new System.Drawing.Point(10, 23);
			this.txtSurname.Margin = new System.Windows.Forms.Padding(2);
			this.txtSurname.Name = "txtSurname";
			this.txtSurname.Size = new System.Drawing.Size(182, 21);
			this.txtSurname.TabIndex = 15;
			// 
			// txtName
			// 
			this.txtName.Location = new System.Drawing.Point(10, 62);
			this.txtName.Margin = new System.Windows.Forms.Padding(2);
			this.txtName.Name = "txtName";
			this.txtName.Size = new System.Drawing.Size(182, 21);
			this.txtName.TabIndex = 17;
			// 
			// txtPatronymic
			// 
			this.txtPatronymic.Location = new System.Drawing.Point(10, 101);
			this.txtPatronymic.Margin = new System.Windows.Forms.Padding(2);
			this.txtPatronymic.Name = "txtPatronymic";
			this.txtPatronymic.Size = new System.Drawing.Size(182, 21);
			this.txtPatronymic.TabIndex = 20;
			// 
			// txtSurnamePrefix
			// 
			this.txtSurnamePrefix.Location = new System.Drawing.Point(207, 23);
			this.txtSurnamePrefix.Margin = new System.Windows.Forms.Padding(2);
			this.txtSurnamePrefix.Name = "txtSurnamePrefix";
			this.txtSurnamePrefix.Size = new System.Drawing.Size(136, 21);
			this.txtSurnamePrefix.TabIndex = 25;
			// 
			// txtNamePrefix
			// 
			this.txtNamePrefix.Location = new System.Drawing.Point(207, 62);
			this.txtNamePrefix.Margin = new System.Windows.Forms.Padding(2);
			this.txtNamePrefix.Name = "txtNamePrefix";
			this.txtNamePrefix.Size = new System.Drawing.Size(136, 21);
			this.txtNamePrefix.TabIndex = 27;
			// 
			// txtNameSuffix
			// 
			this.txtNameSuffix.Location = new System.Drawing.Point(207, 101);
			this.txtNameSuffix.Margin = new System.Windows.Forms.Padding(2);
			this.txtNameSuffix.Name = "txtNameSuffix";
			this.txtNameSuffix.Size = new System.Drawing.Size(136, 21);
			this.txtNameSuffix.TabIndex = 29;
			// 
			// txtNickname
			// 
			this.txtNickname.Location = new System.Drawing.Point(207, 140);
			this.txtNickname.Margin = new System.Windows.Forms.Padding(2);
			this.txtNickname.Name = "txtNickname";
			this.txtNickname.Size = new System.Drawing.Size(136, 21);
			this.txtNickname.TabIndex = 23;
			// 
			// lblType
			// 
			this.lblType.AutoSize = true;
			this.lblType.Location = new System.Drawing.Point(11, 124);
			this.lblType.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.lblType.Name = "lblType";
			this.lblType.Size = new System.Drawing.Size(25, 13);
			this.lblType.TabIndex = 30;
			this.lblType.Text = "lblType";
			// 
			// cmbNameType
			// 
			this.cmbNameType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cmbNameType.Location = new System.Drawing.Point(11, 140);
			this.cmbNameType.Margin = new System.Windows.Forms.Padding(2);
			this.cmbNameType.Name = "cmbNameType";
			this.cmbNameType.Size = new System.Drawing.Size(181, 21);
			this.cmbNameType.TabIndex = 31;
			// 
			// PersonalNameEditDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(355, 213);
			this.Controls.Add(this.lblType);
			this.Controls.Add(this.cmbNameType);
			this.Controls.Add(this.lblSurname);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.lblName);
			this.Controls.Add(this.lblPatronymic);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.lblSurnamePrefix);
			this.Controls.Add(this.lblNamePrefix);
			this.Controls.Add(this.lblNameSuffix);
			this.Controls.Add(this.lblNickname);
			this.Controls.Add(this.txtSurname);
			this.Controls.Add(this.txtName);
			this.Controls.Add(this.txtPatronymic);
			this.Controls.Add(this.txtSurnamePrefix);
			this.Controls.Add(this.txtNamePrefix);
			this.Controls.Add(this.txtNameSuffix);
			this.Controls.Add(this.txtNickname);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.Margin = new System.Windows.Forms.Padding(2);
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "PersonalNameEditDlg";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "PersonalNameEditDlg";
			this.ResumeLayout(false);
			this.PerformLayout();
		}
		private System.Windows.Forms.ComboBox cmbNameType;
		private System.Windows.Forms.Label lblType;
		private System.Windows.Forms.TextBox txtNickname;
		private System.Windows.Forms.TextBox txtNameSuffix;
		private System.Windows.Forms.TextBox txtNamePrefix;
		private System.Windows.Forms.TextBox txtSurnamePrefix;
		private System.Windows.Forms.TextBox txtPatronymic;
		private System.Windows.Forms.TextBox txtName;
		private System.Windows.Forms.TextBox txtSurname;
		private System.Windows.Forms.Label lblNickname;
		private System.Windows.Forms.Label lblNameSuffix;
		private System.Windows.Forms.Label lblNamePrefix;
		private System.Windows.Forms.Label lblSurnamePrefix;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label lblPatronymic;
		private System.Windows.Forms.Label lblName;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Label lblSurname;

		#endregion
	}
}