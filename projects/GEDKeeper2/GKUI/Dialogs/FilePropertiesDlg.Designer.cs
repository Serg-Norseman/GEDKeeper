using System;

namespace GKUI.Dialogs
{
	partial class FilePropertiesDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.TabPage SheetAuthor;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.Label Label3;
		private System.Windows.Forms.TextBox EditName;
		private System.Windows.Forms.TextBox EditTel;
		private System.Windows.Forms.TextBox MemoAddress;
		private System.Windows.Forms.TabPage SheetAdvanced;
		public System.Windows.Forms.TabControl PageControl1;

		private void InitializeComponent()
		{
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.PageControl1 = new System.Windows.Forms.TabControl();
			this.SheetAuthor = new System.Windows.Forms.TabPage();
			this.Label1 = new System.Windows.Forms.Label();
			this.Label2 = new System.Windows.Forms.Label();
			this.Label3 = new System.Windows.Forms.Label();
			this.EditName = new System.Windows.Forms.TextBox();
			this.EditTel = new System.Windows.Forms.TextBox();
			this.MemoAddress = new System.Windows.Forms.TextBox();
			this.SheetAdvanced = new System.Windows.Forms.TabPage();
			this.lvRecordStats = new System.Windows.Forms.ListView();
			this.columnHeader1 = new System.Windows.Forms.ColumnHeader();
			this.columnHeader2 = new System.Windows.Forms.ColumnHeader();
			this.PageControl1.SuspendLayout();
			this.SheetAuthor.SuspendLayout();
			this.SheetAdvanced.SuspendLayout();
			this.SuspendLayout();
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(381, 359);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(113, 31);
			this.btnAccept.TabIndex = 1;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(504, 359);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(113, 31);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// PageControl1
			// 
			this.PageControl1.Controls.Add(this.SheetAuthor);
			this.PageControl1.Controls.Add(this.SheetAdvanced);
			this.PageControl1.Location = new System.Drawing.Point(11, 10);
			this.PageControl1.Name = "PageControl1";
			this.PageControl1.SelectedIndex = 0;
			this.PageControl1.Size = new System.Drawing.Size(606, 331);
			this.PageControl1.TabIndex = 0;
			// 
			// SheetAuthor
			// 
			this.SheetAuthor.Controls.Add(this.Label1);
			this.SheetAuthor.Controls.Add(this.Label2);
			this.SheetAuthor.Controls.Add(this.Label3);
			this.SheetAuthor.Controls.Add(this.EditName);
			this.SheetAuthor.Controls.Add(this.EditTel);
			this.SheetAuthor.Controls.Add(this.MemoAddress);
			this.SheetAuthor.Location = new System.Drawing.Point(4, 26);
			this.SheetAuthor.Name = "SheetAuthor";
			this.SheetAuthor.Size = new System.Drawing.Size(598, 301);
			this.SheetAuthor.TabIndex = 0;
			this.SheetAuthor.Text = "Автор";
			// 
			// Label1
			// 
			this.Label1.AutoSize = true;
			this.Label1.Location = new System.Drawing.Point(11, 10);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(33, 17);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Имя";
			// 
			// Label2
			// 
			this.Label2.AutoSize = true;
			this.Label2.Location = new System.Drawing.Point(11, 39);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(46, 17);
			this.Label2.TabIndex = 2;
			this.Label2.Text = "Адрес";
			// 
			// Label3
			// 
			this.Label3.AutoSize = true;
			this.Label3.Location = new System.Drawing.Point(11, 185);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(64, 17);
			this.Label3.TabIndex = 4;
			this.Label3.Text = "Телефон";
			// 
			// EditName
			// 
			this.EditName.Location = new System.Drawing.Point(90, 10);
			this.EditName.Name = "EditName";
			this.EditName.Size = new System.Drawing.Size(494, 24);
			this.EditName.TabIndex = 1;
			// 
			// EditTel
			// 
			this.EditTel.Location = new System.Drawing.Point(90, 185);
			this.EditTel.Name = "EditTel";
			this.EditTel.Size = new System.Drawing.Size(494, 24);
			this.EditTel.TabIndex = 5;
			// 
			// MemoAddress
			// 
			this.MemoAddress.Location = new System.Drawing.Point(90, 39);
			this.MemoAddress.Multiline = true;
			this.MemoAddress.Name = "MemoAddress";
			this.MemoAddress.Size = new System.Drawing.Size(494, 137);
			this.MemoAddress.TabIndex = 3;
			// 
			// SheetAdvanced
			// 
			this.SheetAdvanced.Controls.Add(this.lvRecordStats);
			this.SheetAdvanced.Location = new System.Drawing.Point(4, 26);
			this.SheetAdvanced.Name = "SheetAdvanced";
			this.SheetAdvanced.Size = new System.Drawing.Size(598, 301);
			this.SheetAdvanced.TabIndex = 1;
			this.SheetAdvanced.Text = "Прочие";
			// 
			// lvRecordStats
			// 
			this.lvRecordStats.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
									this.columnHeader1,
									this.columnHeader2});
			this.lvRecordStats.Dock = System.Windows.Forms.DockStyle.Fill;
			this.lvRecordStats.FullRowSelect = true;
			this.lvRecordStats.Location = new System.Drawing.Point(0, 0);
			this.lvRecordStats.MultiSelect = false;
			this.lvRecordStats.Name = "lvRecordStats";
			this.lvRecordStats.Size = new System.Drawing.Size(598, 301);
			this.lvRecordStats.TabIndex = 1;
			this.lvRecordStats.UseCompatibleStateImageBehavior = false;
			this.lvRecordStats.View = System.Windows.Forms.View.Details;
			// 
			// columnHeader1
			// 
			this.columnHeader1.Text = "Записи";
			this.columnHeader1.Width = 300;
			// 
			// columnHeader2
			// 
			this.columnHeader2.Text = "Количество";
			this.columnHeader2.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
			this.columnHeader2.Width = 100;
			// 
			// TfmFileProperties
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(630, 405);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.PageControl1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TfmFileProperties";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Свойства файла";
			this.PageControl1.ResumeLayout(false);
			this.SheetAuthor.ResumeLayout(false);
			this.SheetAuthor.PerformLayout();
			this.SheetAdvanced.ResumeLayout(false);
			this.ResumeLayout(false);
		}
		private System.Windows.Forms.ColumnHeader columnHeader2;
		private System.Windows.Forms.ColumnHeader columnHeader1;
		private System.Windows.Forms.ListView lvRecordStats;
	}
}