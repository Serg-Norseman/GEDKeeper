using System;
using System.Drawing;
using System.Windows.Forms;

namespace GKUI.Dialogs
{
	partial class UserRefEditDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label lblReference;
		private System.Windows.Forms.ComboBox EditRef;
		private System.Windows.Forms.Label lblRefType;
		private System.Windows.Forms.ComboBox EditType;

		private void InitializeComponent()
		{
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(UserRefEditDlg));
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.lblReference = new System.Windows.Forms.Label();
			this.EditRef = new System.Windows.Forms.ComboBox();
			this.lblRefType = new System.Windows.Forms.Label();
			this.EditType = new System.Windows.Forms.ComboBox();
			this.SuspendLayout();
			// 
			// btnAccept
			// 
			this.btnAccept.Image = ((System.Drawing.Image)(resources.GetObject("btnAccept.Image")));
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(246, 136);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(114, 30);
			this.btnAccept.TabIndex = 4;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = ((System.Drawing.Image)(resources.GetObject("btnCancel.Image")));
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(370, 136);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(113, 30);
			this.btnCancel.TabIndex = 5;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// lblReference
			// 
			this.lblReference.AutoSize = true;
			this.lblReference.Location = new System.Drawing.Point(11, 10);
			this.lblReference.Name = "lblReference";
			this.lblReference.Size = new System.Drawing.Size(256, 17);
			this.lblReference.TabIndex = 0;
			this.lblReference.Text = "Сноска/ссылка/пометка/комментарий";
			// 
			// EditRef
			// 
			this.EditRef.Location = new System.Drawing.Point(11, 29);
			this.EditRef.Name = "EditRef";
			this.EditRef.Size = new System.Drawing.Size(472, 25);
			this.EditRef.TabIndex = 1;
			// 
			// lblRefType
			// 
			this.lblRefType.AutoSize = true;
			this.lblRefType.Location = new System.Drawing.Point(11, 68);
			this.lblRefType.Name = "lblRefType";
			this.lblRefType.Size = new System.Drawing.Size(32, 17);
			this.lblRefType.TabIndex = 2;
			this.lblRefType.Text = "Тип";
			// 
			// EditType
			// 
			this.EditType.Location = new System.Drawing.Point(11, 87);
			this.EditType.Name = "EditType";
			this.EditType.Size = new System.Drawing.Size(472, 25);
			this.EditType.TabIndex = 3;
			// 
			// UserRefEditDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(495, 179);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.lblReference);
			this.Controls.Add(this.EditRef);
			this.Controls.Add(this.lblRefType);
			this.Controls.Add(this.EditType);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "UserRefEditDlg";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Пользовательская сноска";
			this.ResumeLayout(false);
			this.PerformLayout();
		}
	}
}