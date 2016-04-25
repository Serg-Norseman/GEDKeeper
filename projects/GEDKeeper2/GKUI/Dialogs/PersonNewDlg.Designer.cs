using System;

namespace GKUI.Dialogs
{
	partial class PersonNewDlg
	{
		private System.ComponentModel.IContainer components = null;

		public System.Windows.Forms.TextBox edFamily;
		public System.Windows.Forms.TextBox edName;
		public System.Windows.Forms.ComboBox edPatronymic;
		public System.Windows.Forms.ComboBox EditSex;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.Label Label3;
		private System.Windows.Forms.Label Label4;
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
            this.Label1 = new System.Windows.Forms.Label();
            this.edFamily = new System.Windows.Forms.TextBox();
            this.Label2 = new System.Windows.Forms.Label();
            this.edName = new System.Windows.Forms.TextBox();
            this.Label3 = new System.Windows.Forms.Label();
            this.edPatronymic = new System.Windows.Forms.ComboBox();
            this.Label4 = new System.Windows.Forms.Label();
            this.EditSex = new System.Windows.Forms.ComboBox();
            this.btnAccept = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // Label1
            // 
            this.Label1.AutoSize = true;
            this.Label1.Location = new System.Drawing.Point(12, 13);
            this.Label1.Name = "Label1";
            this.Label1.Size = new System.Drawing.Size(65, 17);
            this.Label1.TabIndex = 0;
            this.Label1.Text = "Фамилия";
            // 
            // edFamily
            // 
            this.edFamily.Location = new System.Drawing.Point(101, 10);
            this.edFamily.Name = "edFamily";
            this.edFamily.Size = new System.Drawing.Size(259, 24);
            this.edFamily.TabIndex = 1;
            this.edFamily.KeyDown += new System.Windows.Forms.KeyEventHandler(this.edFamily_KeyDown);
            this.edFamily.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edFamily_KeyPress);
            // 
            // Label2
            // 
            this.Label2.AutoSize = true;
            this.Label2.Location = new System.Drawing.Point(12, 42);
            this.Label2.Name = "Label2";
            this.Label2.Size = new System.Drawing.Size(33, 17);
            this.Label2.TabIndex = 2;
            this.Label2.Text = "Имя";
            // 
            // edName
            // 
            this.edName.Location = new System.Drawing.Point(101, 39);
            this.edName.Name = "edName";
            this.edName.Size = new System.Drawing.Size(259, 24);
            this.edName.TabIndex = 3;
            this.edName.KeyDown += new System.Windows.Forms.KeyEventHandler(this.edFamily_KeyDown);
            this.edName.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edFamily_KeyPress);
            // 
            // Label3
            // 
            this.Label3.AutoSize = true;
            this.Label3.Location = new System.Drawing.Point(12, 71);
            this.Label3.Name = "Label3";
            this.Label3.Size = new System.Drawing.Size(71, 17);
            this.Label3.TabIndex = 4;
            this.Label3.Text = "Отчество";
            // 
            // edPatronymic
            // 
            this.edPatronymic.Location = new System.Drawing.Point(101, 68);
            this.edPatronymic.Name = "edPatronymic";
            this.edPatronymic.Size = new System.Drawing.Size(259, 25);
            this.edPatronymic.TabIndex = 5;
            this.edPatronymic.KeyDown += new System.Windows.Forms.KeyEventHandler(this.edFamily_KeyDown);
            this.edPatronymic.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edFamily_KeyPress);
            // 
            // Label4
            // 
            this.Label4.AutoSize = true;
            this.Label4.Location = new System.Drawing.Point(12, 100);
            this.Label4.Name = "Label4";
            this.Label4.Size = new System.Drawing.Size(33, 17);
            this.Label4.TabIndex = 6;
            this.Label4.Text = "Пол";
            // 
            // EditSex
            // 
            this.EditSex.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.EditSex.Location = new System.Drawing.Point(101, 97);
            this.EditSex.Name = "EditSex";
            this.EditSex.Size = new System.Drawing.Size(169, 25);
            this.EditSex.TabIndex = 7;
            // 
            // btnAccept
            // 
            this.btnAccept.Image = global::GKResources.iBtnAccept;
            this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnAccept.Location = new System.Drawing.Point(126, 140);
            this.btnAccept.Name = "btnAccept";
            this.btnAccept.Size = new System.Drawing.Size(114, 30);
            this.btnAccept.TabIndex = 8;
            this.btnAccept.Text = "Принять";
            this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.Image = global::GKResources.iBtnCancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(246, 140);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(114, 30);
            this.btnCancel.TabIndex = 9;
            this.btnCancel.Text = "Отменить";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // TfmPersonNew
            // 
            this.AcceptButton = this.btnAccept;
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(375, 185);
            this.Controls.Add(this.Label1);
            this.Controls.Add(this.edFamily);
            this.Controls.Add(this.edName);
            this.Controls.Add(this.Label2);
            this.Controls.Add(this.Label3);
            this.Controls.Add(this.edPatronymic);
            this.Controls.Add(this.Label4);
            this.Controls.Add(this.EditSex);
            this.Controls.Add(this.btnAccept);
            this.Controls.Add(this.btnCancel);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "TfmPersonNew";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Новая персональная запись";
            this.ResumeLayout(false);
            this.PerformLayout();

		}

	}
}