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
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(SexCheckDlg));
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
			this.edName.Location = new System.Drawing.Point(11, 10);
			this.edName.Name = "edName";
			this.edName.ReadOnly = true;
			this.edName.Size = new System.Drawing.Size(483, 24);
			this.edName.TabIndex = 0;
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.sbNone);
			this.GroupBox1.Controls.Add(this.sbMale);
			this.GroupBox1.Controls.Add(this.sbFemale);
			this.GroupBox1.Location = new System.Drawing.Point(11, 43);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(483, 59);
			this.GroupBox1.TabIndex = 1;
			this.GroupBox1.TabStop = false;
			this.GroupBox1.Text = "Пол";
			// 
			// sbNone
			// 
			this.sbNone.AutoSize = true;
			this.sbNone.Location = new System.Drawing.Point(11, 19);
			this.sbNone.Name = "sbNone";
			this.sbNone.Size = new System.Drawing.Size(36, 21);
			this.sbNone.TabIndex = 0;
			this.sbNone.Text = "?";
			// 
			// sbMale
			// 
			this.sbMale.AutoSize = true;
			this.sbMale.Location = new System.Drawing.Point(167, 19);
			this.sbMale.Name = "sbMale";
			this.sbMale.Size = new System.Drawing.Size(87, 21);
			this.sbMale.TabIndex = 1;
			this.sbMale.Text = "Мужской";
			// 
			// sbFemale
			// 
			this.sbFemale.AutoSize = true;
			this.sbFemale.Location = new System.Drawing.Point(323, 19);
			this.sbFemale.Name = "sbFemale";
			this.sbFemale.Size = new System.Drawing.Size(88, 21);
			this.sbFemale.TabIndex = 2;
			this.sbFemale.Text = "Женский";
			// 
			// btnAccept
			// 
			this.btnAccept.DialogResult = System.Windows.Forms.DialogResult.OK;
			this.btnAccept.Image = ((System.Drawing.Image)(resources.GetObject("btnAccept.Image")));
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(258, 117);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(113, 30);
			this.btnAccept.TabIndex = 2;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = ((System.Drawing.Image)(resources.GetObject("btnCancel.Image")));
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(381, 117);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(113, 30);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// SexCheckDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(504, 158);
			this.Controls.Add(this.edName);
			this.Controls.Add(this.GroupBox1);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
			this.Name = "SexCheckDlg";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Проверка пола";
			this.TopMost = true;
			this.GroupBox1.ResumeLayout(false);
			this.GroupBox1.PerformLayout();
			this.ResumeLayout(false);
			this.PerformLayout();
		}
	}
}