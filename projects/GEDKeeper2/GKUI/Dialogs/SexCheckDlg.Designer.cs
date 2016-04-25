using System;
using System.Drawing;
using System.Resources;
using System.Windows.Forms;

namespace GKUI.Dialogs
{
	partial class SexCheckDlg
	{
		private System.Windows.Forms.TextBox edName;
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.RadioButton sbNone;
		private System.Windows.Forms.RadioButton sbMale;
		private System.Windows.Forms.RadioButton sbFemale;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;

		private void InitializeComponent()
		{
			this.edName = new System.Windows.Forms.TextBox();
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.sbNone = new System.Windows.Forms.RadioButton();
			this.sbMale = new System.Windows.Forms.RadioButton();
			this.sbFemale = new System.Windows.Forms.RadioButton();
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.GroupBox1.SuspendLayout();
			this.SuspendLayout();
			// 
			// edName
			// 
			this.edName.Location = new System.Drawing.Point(8, 8);
			this.edName.Name = "edName";
			this.edName.ReadOnly = true;
			this.edName.Size = new System.Drawing.Size(345, 21);
			this.edName.TabIndex = 0;
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.sbNone);
			this.GroupBox1.Controls.Add(this.sbMale);
			this.GroupBox1.Controls.Add(this.sbFemale);
			this.GroupBox1.Location = new System.Drawing.Point(8, 35);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(345, 49);
			this.GroupBox1.TabIndex = 1;
			this.GroupBox1.TabStop = false;
			this.GroupBox1.Text = "Пол";
			// 
			// sbNone
			// 
			this.sbNone.Location = new System.Drawing.Point(8, 16);
			this.sbNone.Name = "sbNone";
			this.sbNone.Size = new System.Drawing.Size(105, 22);
			this.sbNone.TabIndex = 0;
			this.sbNone.Text = "?";
			// 
			// sbMale
			// 
			this.sbMale.Location = new System.Drawing.Point(119, 16);
			this.sbMale.Name = "sbMale";
			this.sbMale.Size = new System.Drawing.Size(105, 22);
			this.sbMale.TabIndex = 1;
			this.sbMale.Text = "Мужской";
			// 
			// sbFemale
			// 
			this.sbFemale.Location = new System.Drawing.Point(231, 16);
			this.sbFemale.Name = "sbFemale";
			this.sbFemale.Size = new System.Drawing.Size(105, 22);
			this.sbFemale.TabIndex = 2;
			this.sbFemale.Text = "Женский";
			// 
			// btnAccept
			// 
			this.btnAccept.DialogResult = System.Windows.Forms.DialogResult.OK;
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(184, 96);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(81, 25);
			this.btnAccept.TabIndex = 2;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(272, 96);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(81, 25);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// TfmSexCheck
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(361, 130);
			this.Controls.Add(this.edName);
			this.Controls.Add(this.GroupBox1);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
			this.Name = "TfmSexCheck";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.TopMost = true;
			this.Text = "Проверка пола";
			this.GroupBox1.ResumeLayout(false);
			this.ResumeLayout(false);
			this.PerformLayout();
		}
	}
}