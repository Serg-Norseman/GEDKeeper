using System;

namespace GKUI
{
	partial class TfmRepositoryEdit
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.TextBox edName;
		private System.Windows.Forms.TabControl PagesData;
		private System.Windows.Forms.TabPage SheetNotes;
		private System.Windows.Forms.Button btnAddress;

		private void InitializeComponent()
		{
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.Label1 = new System.Windows.Forms.Label();
			this.edName = new System.Windows.Forms.TextBox();
			this.btnAddress = new System.Windows.Forms.Button();
			this.PagesData = new System.Windows.Forms.TabControl();
			this.SheetNotes = new System.Windows.Forms.TabPage();
			this.GroupBox1.SuspendLayout();
			this.PagesData.SuspendLayout();
			this.SuspendLayout();
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(240, 336);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(81, 25);
			this.btnAccept.TabIndex = 3;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(328, 336);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(81, 25);
			this.btnCancel.TabIndex = 4;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.Label1);
			this.GroupBox1.Controls.Add(this.edName);
			this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
			this.GroupBox1.Location = new System.Drawing.Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(417, 41);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			// 
			// Label1
			// 
			this.Label1.Location = new System.Drawing.Point(8, 20);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(60, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Название";
			// 
			// edName
			// 
			this.edName.Location = new System.Drawing.Point(72, 12);
			this.edName.Name = "edName";
			this.edName.Size = new System.Drawing.Size(337, 21);
			this.edName.TabIndex = 1;
			// 
			// btnAddress
			// 
			this.btnAddress.Location = new System.Drawing.Point(8, 336);
			this.btnAddress.Name = "btnAddress";
			this.btnAddress.Size = new System.Drawing.Size(81, 25);
			this.btnAddress.TabIndex = 2;
			this.btnAddress.Text = "Адрес...";
			this.btnAddress.Click += new System.EventHandler(this.btnAddress_Click);
			// 
			// PagesData
			// 
			this.PagesData.Controls.Add(this.SheetNotes);
			this.PagesData.Dock = System.Windows.Forms.DockStyle.Top;
			this.PagesData.Location = new System.Drawing.Point(0, 41);
			this.PagesData.Name = "PagesData";
			this.PagesData.SelectedIndex = 0;
			this.PagesData.Size = new System.Drawing.Size(417, 280);
			this.PagesData.TabIndex = 1;
			// 
			// SheetNotes
			// 
			this.SheetNotes.Location = new System.Drawing.Point(4, 22);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new System.Drawing.Size(409, 254);
			this.SheetNotes.TabIndex = 0;
			this.SheetNotes.Text = "Заметки";
			// 
			// TfmRepositoryEdit
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(417, 369);
			this.Controls.Add(this.PagesData);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.GroupBox1);
			this.Controls.Add(this.btnAddress);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TfmRepositoryEdit";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Архив";
			this.GroupBox1.ResumeLayout(false);
			this.GroupBox1.PerformLayout();
			this.PagesData.ResumeLayout(false);
			this.ResumeLayout(false);
		}
	}
}