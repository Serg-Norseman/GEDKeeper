using System;

namespace GKUI.Dialogs
{
	partial class GroupEditDlg
	{
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.TextBox edName;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.TabControl PagesGroupData;
		private System.Windows.Forms.TabPage SheetNotes;
		private System.Windows.Forms.TabPage SheetMultimedia;
		private System.Windows.Forms.TabPage SheetMembers;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;

		private void InitializeComponent()
		{
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(GroupEditDlg));
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.Label1 = new System.Windows.Forms.Label();
			this.edName = new System.Windows.Forms.TextBox();
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.PagesGroupData = new System.Windows.Forms.TabControl();
			this.SheetMembers = new System.Windows.Forms.TabPage();
			this.SheetNotes = new System.Windows.Forms.TabPage();
			this.SheetMultimedia = new System.Windows.Forms.TabPage();
			this.GroupBox1.SuspendLayout();
			this.PagesGroupData.SuspendLayout();
			this.SuspendLayout();
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.Label1);
			this.GroupBox1.Controls.Add(this.edName);
			this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
			this.GroupBox1.Location = new System.Drawing.Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(673, 60);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			// 
			// Label1
			// 
			this.Label1.AutoSize = true;
			this.Label1.Location = new System.Drawing.Point(12, 22);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(67, 17);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Название";
			// 
			// edName
			// 
			this.edName.Location = new System.Drawing.Point(101, 19);
			this.edName.Name = "edName";
			this.edName.Size = new System.Drawing.Size(561, 24);
			this.edName.TabIndex = 0;
			// 
			// btnAccept
			// 
			this.btnAccept.Image = ((System.Drawing.Image)(resources.GetObject("btnAccept.Image")));
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(426, 515);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(113, 30);
			this.btnAccept.TabIndex = 2;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = ((System.Drawing.Image)(resources.GetObject("btnCancel.Image")));
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(549, 515);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(113, 30);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// PagesGroupData
			// 
			this.PagesGroupData.Controls.Add(this.SheetMembers);
			this.PagesGroupData.Controls.Add(this.SheetNotes);
			this.PagesGroupData.Controls.Add(this.SheetMultimedia);
			this.PagesGroupData.Dock = System.Windows.Forms.DockStyle.Top;
			this.PagesGroupData.Location = new System.Drawing.Point(0, 60);
			this.PagesGroupData.Name = "PagesGroupData";
			this.PagesGroupData.SelectedIndex = 0;
			this.PagesGroupData.Size = new System.Drawing.Size(673, 437);
			this.PagesGroupData.TabIndex = 1;
			// 
			// SheetMembers
			// 
			this.SheetMembers.Location = new System.Drawing.Point(4, 26);
			this.SheetMembers.Name = "SheetMembers";
			this.SheetMembers.Size = new System.Drawing.Size(665, 407);
			this.SheetMembers.TabIndex = 0;
			this.SheetMembers.Text = "Участники";
			// 
			// SheetNotes
			// 
			this.SheetNotes.Location = new System.Drawing.Point(4, 26);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new System.Drawing.Size(665, 407);
			this.SheetNotes.TabIndex = 1;
			this.SheetNotes.Text = "Заметки";
			// 
			// SheetMultimedia
			// 
			this.SheetMultimedia.Location = new System.Drawing.Point(4, 26);
			this.SheetMultimedia.Name = "SheetMultimedia";
			this.SheetMultimedia.Size = new System.Drawing.Size(665, 407);
			this.SheetMultimedia.TabIndex = 2;
			this.SheetMultimedia.Text = "Мультимедиа";
			// 
			// GroupEditDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(673, 560);
			this.Controls.Add(this.PagesGroupData);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.GroupBox1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "GroupEditDlg";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Правка группы";
			this.GroupBox1.ResumeLayout(false);
			this.GroupBox1.PerformLayout();
			this.PagesGroupData.ResumeLayout(false);
			this.ResumeLayout(false);
		}
	}
}