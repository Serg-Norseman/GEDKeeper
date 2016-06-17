using System;

namespace GKUI.Dialogs
{
	partial class PersonNewDlg
	{
		private System.ComponentModel.IContainer components = null;

		public System.Windows.Forms.TextBox txtSurname;
		public System.Windows.Forms.TextBox txtName;
		public System.Windows.Forms.ComboBox cmbPatronymic;
		public System.Windows.Forms.ComboBox cmbSex;
		private System.Windows.Forms.Label lblSurname;
		private System.Windows.Forms.Label lblName;
		private System.Windows.Forms.Label lblPatronymic;
		private System.Windows.Forms.Label lblSex;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;

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
            this.lblSurname = new System.Windows.Forms.Label();
            this.txtSurname = new System.Windows.Forms.TextBox();
            this.lblName = new System.Windows.Forms.Label();
            this.txtName = new System.Windows.Forms.TextBox();
            this.lblPatronymic = new System.Windows.Forms.Label();
            this.cmbPatronymic = new System.Windows.Forms.ComboBox();
            this.lblSex = new System.Windows.Forms.Label();
            this.cmbSex = new System.Windows.Forms.ComboBox();
            this.btnAccept = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // lblSurname
            // 
            this.lblSurname.AutoSize = true;
            this.lblSurname.Location = new System.Drawing.Point(12, 13);
            this.lblSurname.Name = "lblSurname";
            this.lblSurname.Size = new System.Drawing.Size(65, 17);
            this.lblSurname.TabIndex = 0;
            this.lblSurname.Text = "lblSurname";
            // 
            // txtSurname
            // 
            this.txtSurname.Location = new System.Drawing.Point(101, 10);
            this.txtSurname.Name = "txtSurname";
            this.txtSurname.Size = new System.Drawing.Size(259, 24);
            this.txtSurname.TabIndex = 1;
            this.txtSurname.KeyDown += new System.Windows.Forms.KeyEventHandler(this.edFamily_KeyDown);
            this.txtSurname.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edFamily_KeyPress);
            // 
            // lblName
            // 
            this.lblName.AutoSize = true;
            this.lblName.Location = new System.Drawing.Point(12, 42);
            this.lblName.Name = "lblName";
            this.lblName.Size = new System.Drawing.Size(33, 17);
            this.lblName.TabIndex = 2;
            this.lblName.Text = "lblName";
            // 
            // txtName
            // 
            this.txtName.Location = new System.Drawing.Point(101, 39);
            this.txtName.Name = "txtName";
            this.txtName.Size = new System.Drawing.Size(259, 24);
            this.txtName.TabIndex = 3;
            this.txtName.KeyDown += new System.Windows.Forms.KeyEventHandler(this.edFamily_KeyDown);
            this.txtName.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edFamily_KeyPress);
            // 
            // lblPatronymic
            // 
            this.lblPatronymic.AutoSize = true;
            this.lblPatronymic.Location = new System.Drawing.Point(12, 71);
            this.lblPatronymic.Name = "lblPatronymic";
            this.lblPatronymic.Size = new System.Drawing.Size(71, 17);
            this.lblPatronymic.TabIndex = 4;
            this.lblPatronymic.Text = "lblPatronymic";
            // 
            // cmbPatronymic
            // 
            this.cmbPatronymic.Location = new System.Drawing.Point(101, 68);
            this.cmbPatronymic.Name = "cmbPatronymic";
            this.cmbPatronymic.Size = new System.Drawing.Size(259, 25);
            this.cmbPatronymic.TabIndex = 5;
            this.cmbPatronymic.KeyDown += new System.Windows.Forms.KeyEventHandler(this.edFamily_KeyDown);
            this.cmbPatronymic.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edFamily_KeyPress);
            // 
            // lblSex
            // 
            this.lblSex.AutoSize = true;
            this.lblSex.Location = new System.Drawing.Point(12, 100);
            this.lblSex.Name = "lblSex";
            this.lblSex.Size = new System.Drawing.Size(33, 17);
            this.lblSex.TabIndex = 6;
            this.lblSex.Text = "lblSex";
            // 
            // cmbSex
            // 
            this.cmbSex.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbSex.Location = new System.Drawing.Point(101, 97);
            this.cmbSex.Name = "cmbSex";
            this.cmbSex.Size = new System.Drawing.Size(169, 25);
            this.cmbSex.TabIndex = 7;
            // 
            // btnAccept
            // 
            this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnAccept.Location = new System.Drawing.Point(126, 140);
            this.btnAccept.Name = "btnAccept";
            this.btnAccept.Size = new System.Drawing.Size(114, 30);
            this.btnAccept.TabIndex = 8;
            this.btnAccept.Text = "btnAccept";
            this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(246, 140);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(114, 30);
            this.btnCancel.TabIndex = 9;
            this.btnCancel.Text = "btnCancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // PersonNewDlg
            // 
            this.AcceptButton = this.btnAccept;
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(375, 185);
            this.Controls.Add(this.lblSurname);
            this.Controls.Add(this.txtSurname);
            this.Controls.Add(this.txtName);
            this.Controls.Add(this.lblName);
            this.Controls.Add(this.lblPatronymic);
            this.Controls.Add(this.cmbPatronymic);
            this.Controls.Add(this.lblSex);
            this.Controls.Add(this.cmbSex);
            this.Controls.Add(this.btnAccept);
            this.Controls.Add(this.btnCancel);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "PersonNewDlg";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "PersonNewDlg";
            this.ResumeLayout(false);
            this.PerformLayout();

		}

	}
}