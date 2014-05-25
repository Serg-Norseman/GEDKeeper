using System;

namespace GKUI.Dialogs
{
	partial class TfmNameEdit
	{
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.TextBox edName;
		private System.Windows.Forms.Label Label4;
		private System.Windows.Forms.ComboBox edSex;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.Label Label3;
		private System.Windows.Forms.TextBox edFPatr;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.TextBox edMPatr;

		private void InitializeComponent()
		{
			this.Label2 = new System.Windows.Forms.Label();
			this.edName = new System.Windows.Forms.TextBox();
			this.Label4 = new System.Windows.Forms.Label();
			this.edSex = new System.Windows.Forms.ComboBox();
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.Label3 = new System.Windows.Forms.Label();
			this.Label1 = new System.Windows.Forms.Label();
			this.edFPatr = new System.Windows.Forms.TextBox();
			this.edMPatr = new System.Windows.Forms.TextBox();
			this.GroupBox1.SuspendLayout();
			this.SuspendLayout();
			// 
			// Label2
			// 
			this.Label2.Location = new System.Drawing.Point(8, 16);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(40, 13);
			this.Label2.TabIndex = 0;
			this.Label2.Text = "Имя";
			// 
			// edName
			// 
			this.edName.Location = new System.Drawing.Point(72, 8);
			this.edName.Name = "edName";
			this.edName.Size = new System.Drawing.Size(193, 21);
			this.edName.TabIndex = 1;
			this.edName.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edName_KeyPress);
			// 
			// Label4
			// 
			this.Label4.Location = new System.Drawing.Point(8, 48);
			this.Label4.Name = "Label4";
			this.Label4.Size = new System.Drawing.Size(40, 13);
			this.Label4.TabIndex = 2;
			this.Label4.Text = "Пол";
			// 
			// edSex
			// 
			this.edSex.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.edSex.Location = new System.Drawing.Point(72, 40);
			this.edSex.Name = "edSex";
			this.edSex.Size = new System.Drawing.Size(121, 21);
			this.edSex.TabIndex = 3;
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(96, 168);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(81, 25);
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
			this.btnCancel.Location = new System.Drawing.Point(184, 168);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(81, 25);
			this.btnCancel.TabIndex = 6;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.Label3);
			this.GroupBox1.Controls.Add(this.Label1);
			this.GroupBox1.Controls.Add(this.edFPatr);
			this.GroupBox1.Controls.Add(this.edMPatr);
			this.GroupBox1.Location = new System.Drawing.Point(8, 72);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(257, 78);
			this.GroupBox1.TabIndex = 4;
			this.GroupBox1.TabStop = false;
			this.GroupBox1.Text = "Отчества";
			// 
			// Label3
			// 
			this.Label3.Location = new System.Drawing.Point(8, 24);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(52, 13);
			this.Label3.TabIndex = 0;
			this.Label3.Text = "Женское";
			// 
			// Label1
			// 
			this.Label1.Location = new System.Drawing.Point(8, 56);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(52, 13);
			this.Label1.TabIndex = 2;
			this.Label1.Text = "Мужское";
			// 
			// edFPatr
			// 
			this.edFPatr.Location = new System.Drawing.Point(64, 16);
			this.edFPatr.Name = "edFPatr";
			this.edFPatr.Size = new System.Drawing.Size(185, 21);
			this.edFPatr.TabIndex = 1;
			this.edFPatr.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edName_KeyPress);
			// 
			// edMPatr
			// 
			this.edMPatr.Location = new System.Drawing.Point(64, 48);
			this.edMPatr.Name = "edMPatr";
			this.edMPatr.Size = new System.Drawing.Size(185, 21);
			this.edMPatr.TabIndex = 3;
			this.edMPatr.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edName_KeyPress);
			// 
			// TfmNameEdit
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(273, 201);
			this.Controls.Add(this.Label2);
			this.Controls.Add(this.edName);
			this.Controls.Add(this.Label4);
			this.Controls.Add(this.edSex);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.GroupBox1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TfmNameEdit";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Имя";
			this.GroupBox1.ResumeLayout(false);
			this.GroupBox1.PerformLayout();
			this.ResumeLayout(false);
			this.PerformLayout();
		}
	}
}