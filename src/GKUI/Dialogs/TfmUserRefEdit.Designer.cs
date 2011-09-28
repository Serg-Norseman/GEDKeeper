using System;
using System.Drawing;
using System.Windows.Forms;

namespace GKUI
{
	partial class TfmUserRefEdit
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.ComboBox EditRef;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.ComboBox EditType;

		private void InitializeComponent()
		{
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.Label1 = new System.Windows.Forms.Label();
			this.EditRef = new System.Windows.Forms.ComboBox();
			this.Label2 = new System.Windows.Forms.Label();
			this.EditType = new System.Windows.Forms.ComboBox();
			this.SuspendLayout();
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(176, 112);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(81, 25);
			this.btnAccept.TabIndex = 4;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(264, 112);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(81, 25);
			this.btnCancel.TabIndex = 5;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// Label1
			// 
			this.Label1.Location = new System.Drawing.Point(8, 8);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(205, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Сноска/ссылка/пометка/комментарий";
			// 
			// EditRef
			// 
			this.EditRef.Location = new System.Drawing.Point(8, 24);
			this.EditRef.Name = "EditRef";
			this.EditRef.Size = new System.Drawing.Size(337, 21);
			this.EditRef.TabIndex = 1;
			// 
			// Label2
			// 
			this.Label2.Location = new System.Drawing.Point(8, 56);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(25, 13);
			this.Label2.TabIndex = 2;
			this.Label2.Text = "Тип";
			// 
			// EditType
			// 
			this.EditType.Location = new System.Drawing.Point(8, 72);
			this.EditType.Name = "EditType";
			this.EditType.Size = new System.Drawing.Size(337, 21);
			this.EditType.TabIndex = 3;
			// 
			// TfmUserRefEdit
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(353, 145);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.Label1);
			this.Controls.Add(this.EditRef);
			this.Controls.Add(this.Label2);
			this.Controls.Add(this.EditType);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TfmUserRefEdit";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Пользовательская сноска";
			this.ResumeLayout(false);
		}
	}
}