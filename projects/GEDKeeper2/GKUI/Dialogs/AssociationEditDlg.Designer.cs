using System;
using System.Drawing;
using System.Windows.Forms;

namespace GKUI.Dialogs
{
	partial class AssociationEditDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label lblRelation;
		private System.Windows.Forms.ComboBox cmbRelation;
		private System.Windows.Forms.Label lblPerson;
		private System.Windows.Forms.TextBox txtPerson;
		private System.Windows.Forms.Button btnPersonAdd;

		private void InitializeComponent()
		{
			this.lblRelation = new System.Windows.Forms.Label();
			this.lblPerson = new System.Windows.Forms.Label();
			this.btnPersonAdd = new System.Windows.Forms.Button();
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.cmbRelation = new System.Windows.Forms.ComboBox();
			this.txtPerson = new System.Windows.Forms.TextBox();
			this.SuspendLayout();
			// 
			// lblRelation
			// 
			this.lblRelation.AutoSize = true;
			this.lblRelation.Location = new System.Drawing.Point(11, 10);
			this.lblRelation.Name = "lblRelation";
			this.lblRelation.Size = new System.Drawing.Size(82, 17);
			this.lblRelation.TabIndex = 0;
			this.lblRelation.Text = "lblRelation";
			// 
			// lblPerson
			// 
			this.lblPerson.AutoSize = true;
			this.lblPerson.Location = new System.Drawing.Point(11, 68);
			this.lblPerson.Name = "lblPerson";
			this.lblPerson.Size = new System.Drawing.Size(62, 17);
			this.lblPerson.TabIndex = 2;
			this.lblPerson.Text = "lblPerson";
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
			this.btnAccept.Text = "btnAccept";
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
			this.btnCancel.Text = "btnCancel";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// cmbRelation
			// 
			this.cmbRelation.Location = new System.Drawing.Point(11, 29);
			this.cmbRelation.Name = "cmbRelation";
			this.cmbRelation.Size = new System.Drawing.Size(472, 25);
			this.cmbRelation.Sorted = true;
			this.cmbRelation.TabIndex = 1;
			// 
			// txtPerson
			// 
			this.txtPerson.BackColor = System.Drawing.SystemColors.Control;
			this.txtPerson.Location = new System.Drawing.Point(11, 87);
			this.txtPerson.Name = "txtPerson";
			this.txtPerson.ReadOnly = true;
			this.txtPerson.Size = new System.Drawing.Size(429, 24);
			this.txtPerson.TabIndex = 3;
			// 
			// AssociationEditDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(496, 181);
			this.Controls.Add(this.lblRelation);
			this.Controls.Add(this.lblPerson);
			this.Controls.Add(this.btnPersonAdd);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.cmbRelation);
			this.Controls.Add(this.txtPerson);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "AssociationEditDlg";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "AssociationEditDlg";
			this.ResumeLayout(false);
			this.PerformLayout();
		}
	}
}