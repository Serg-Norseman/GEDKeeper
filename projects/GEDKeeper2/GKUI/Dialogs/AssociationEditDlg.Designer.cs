using System;
using System.Drawing;
using System.Windows.Forms;

namespace GKUI.Dialogs
{
	partial class AssociationEditDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.ComboBox EditRelation;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.TextBox EditPerson;
		private System.Windows.Forms.Button btnPersonAdd;

		private void InitializeComponent()
		{
			this.Label1 = new System.Windows.Forms.Label();
			this.Label2 = new System.Windows.Forms.Label();
			this.btnPersonAdd = new System.Windows.Forms.Button();
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.EditRelation = new System.Windows.Forms.ComboBox();
			this.EditPerson = new System.Windows.Forms.TextBox();
			this.SuspendLayout();
			// 
			// Label1
			// 
			this.Label1.AutoSize = true;
			this.Label1.Location = new System.Drawing.Point(11, 10);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(82, 17);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Отношение";
			// 
			// Label2
			// 
			this.Label2.AutoSize = true;
			this.Label2.Location = new System.Drawing.Point(11, 68);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(62, 17);
			this.Label2.TabIndex = 2;
			this.Label2.Text = "Персона";
			// 
			// btnPersonAdd
			// 
			this.btnPersonAdd.AccessibleDescription = "Выбрать персональную запись";
			this.btnPersonAdd.AccessibleName = "";
			this.btnPersonAdd.AccessibleRole = System.Windows.Forms.AccessibleRole.ToolTip;
			this.btnPersonAdd.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
			this.btnPersonAdd.Image = global::GKResources.iRecNew;
			this.btnPersonAdd.Location = new System.Drawing.Point(448, 84);
			this.btnPersonAdd.Name = "btnPersonAdd";
			this.btnPersonAdd.Size = new System.Drawing.Size(39, 34);
			this.btnPersonAdd.TabIndex = 4;
			this.btnPersonAdd.Click += new System.EventHandler(this.btnPersonAdd_Click);
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(246, 136);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(114, 30);
			this.btnAccept.TabIndex = 5;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(370, 136);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(113, 30);
			this.btnCancel.TabIndex = 6;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// EditRelation
			// 
			this.EditRelation.Location = new System.Drawing.Point(11, 29);
			this.EditRelation.Name = "EditRelation";
			this.EditRelation.Size = new System.Drawing.Size(472, 25);
			this.EditRelation.Sorted = true;
			this.EditRelation.TabIndex = 1;
			// 
			// EditPerson
			// 
			this.EditPerson.BackColor = System.Drawing.SystemColors.Control;
			this.EditPerson.Location = new System.Drawing.Point(11, 87);
			this.EditPerson.Name = "EditPerson";
			this.EditPerson.ReadOnly = true;
			this.EditPerson.Size = new System.Drawing.Size(429, 24);
			this.EditPerson.TabIndex = 3;
			// 
			// AssociationEditDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(496, 181);
			this.Controls.Add(this.Label1);
			this.Controls.Add(this.Label2);
			this.Controls.Add(this.btnPersonAdd);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.EditRelation);
			this.Controls.Add(this.EditPerson);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "AssociationEditDlg";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Ассоциация";
			this.ResumeLayout(false);
			this.PerformLayout();
		}
	}
}