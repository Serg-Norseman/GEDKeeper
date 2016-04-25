using System;

namespace GKUI.Dialogs
{
	partial class MediaEditDlg
	{
		private System.ComponentModel.IContainer components = null;
		private System.Windows.Forms.TabControl PagesData;
		private System.Windows.Forms.TabPage SheetNotes;
		private System.Windows.Forms.TabPage SheetSources;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.OpenFileDialog OpenDialog1;
		private System.Windows.Forms.Button btnView;
		private System.Windows.Forms.TabPage SheetCommon;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.TextBox edName;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.ComboBox cbMediaType;
		private System.Windows.Forms.Label Label4;
		private System.Windows.Forms.ComboBox cbStoreType;
		private System.Windows.Forms.Label Label3;
		private System.Windows.Forms.TextBox edFile;
		private System.Windows.Forms.Button btnFileSelect;

		protected override void Dispose(bool disposing)
		{
			if (disposing) {
				if (components != null) {
					components.Dispose();
				}
			}
			base.Dispose(disposing);
		}

		private void InitializeComponent()
		{
			this.PagesData = new System.Windows.Forms.TabControl();
			this.SheetCommon = new System.Windows.Forms.TabPage();
			this.Label1 = new System.Windows.Forms.Label();
			this.Label2 = new System.Windows.Forms.Label();
			this.Label4 = new System.Windows.Forms.Label();
			this.Label3 = new System.Windows.Forms.Label();
			this.edName = new System.Windows.Forms.TextBox();
			this.cbMediaType = new System.Windows.Forms.ComboBox();
			this.cbStoreType = new System.Windows.Forms.ComboBox();
			this.edFile = new System.Windows.Forms.TextBox();
			this.btnFileSelect = new System.Windows.Forms.Button();
			this.SheetNotes = new System.Windows.Forms.TabPage();
			this.SheetSources = new System.Windows.Forms.TabPage();
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.btnView = new System.Windows.Forms.Button();
			this.OpenDialog1 = new System.Windows.Forms.OpenFileDialog();
			this.PagesData.SuspendLayout();
			this.SheetCommon.SuspendLayout();
			this.SuspendLayout();
			// 
			// PagesData
			// 
			this.PagesData.Controls.Add(this.SheetCommon);
			this.PagesData.Controls.Add(this.SheetNotes);
			this.PagesData.Controls.Add(this.SheetSources);
			this.PagesData.Dock = System.Windows.Forms.DockStyle.Top;
			this.PagesData.Location = new System.Drawing.Point(0, 0);
			this.PagesData.Name = "PagesData";
			this.PagesData.SelectedIndex = 0;
			this.PagesData.Size = new System.Drawing.Size(728, 302);
			this.PagesData.TabIndex = 0;
			// 
			// SheetCommon
			// 
			this.SheetCommon.Controls.Add(this.Label1);
			this.SheetCommon.Controls.Add(this.Label2);
			this.SheetCommon.Controls.Add(this.Label4);
			this.SheetCommon.Controls.Add(this.Label3);
			this.SheetCommon.Controls.Add(this.edName);
			this.SheetCommon.Controls.Add(this.cbMediaType);
			this.SheetCommon.Controls.Add(this.cbStoreType);
			this.SheetCommon.Controls.Add(this.edFile);
			this.SheetCommon.Controls.Add(this.btnFileSelect);
			this.SheetCommon.Location = new System.Drawing.Point(4, 26);
			this.SheetCommon.Name = "SheetCommon";
			this.SheetCommon.Size = new System.Drawing.Size(720, 272);
			this.SheetCommon.TabIndex = 0;
			this.SheetCommon.Text = "Общие данные";
			// 
			// Label1
			// 
			this.Label1.AutoSize = true;
			this.Label1.Location = new System.Drawing.Point(11, 10);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(67, 17);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Название";
			// 
			// Label2
			// 
			this.Label2.AutoSize = true;
			this.Label2.Location = new System.Drawing.Point(11, 68);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(32, 17);
			this.Label2.TabIndex = 2;
			this.Label2.Text = "Тип";
			// 
			// Label4
			// 
			this.Label4.AutoSize = true;
			this.Label4.Location = new System.Drawing.Point(269, 68);
			this.Label4.Name = "Label4";
			this.Label4.Size = new System.Drawing.Size(121, 17);
			this.Label4.TabIndex = 4;
			this.Label4.Text = "Способ хранения";
			// 
			// Label3
			// 
			this.Label3.AutoSize = true;
			this.Label3.Location = new System.Drawing.Point(11, 126);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(41, 17);
			this.Label3.TabIndex = 6;
			this.Label3.Text = "Файл";
			// 
			// edName
			// 
			this.edName.Location = new System.Drawing.Point(11, 29);
			this.edName.Name = "edName";
			this.edName.Size = new System.Drawing.Size(696, 24);
			this.edName.TabIndex = 1;
			this.edName.TextChanged += new System.EventHandler(this.edName_TextChanged);
			// 
			// cbMediaType
			// 
			this.cbMediaType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbMediaType.DropDownWidth = 15;
			this.cbMediaType.Location = new System.Drawing.Point(11, 87);
			this.cbMediaType.Name = "cbMediaType";
			this.cbMediaType.Size = new System.Drawing.Size(237, 25);
			this.cbMediaType.TabIndex = 3;
			// 
			// cbStoreType
			// 
			this.cbStoreType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbStoreType.Location = new System.Drawing.Point(269, 87);
			this.cbStoreType.Name = "cbStoreType";
			this.cbStoreType.Size = new System.Drawing.Size(281, 25);
			this.cbStoreType.TabIndex = 5;
			// 
			// edFile
			// 
			this.edFile.Location = new System.Drawing.Point(11, 146);
			this.edFile.Name = "edFile";
			this.edFile.ReadOnly = true;
			this.edFile.Size = new System.Drawing.Size(629, 24);
			this.edFile.TabIndex = 7;
			// 
			// btnFileSelect
			// 
			this.btnFileSelect.Location = new System.Drawing.Point(650, 146);
			this.btnFileSelect.Name = "btnFileSelect";
			this.btnFileSelect.Size = new System.Drawing.Size(60, 25);
			this.btnFileSelect.TabIndex = 8;
			this.btnFileSelect.Text = "...";
			this.btnFileSelect.Click += new System.EventHandler(this.btnFileSelect_Click);
			// 
			// SheetNotes
			// 
			this.SheetNotes.Location = new System.Drawing.Point(4, 26);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new System.Drawing.Size(720, 272);
			this.SheetNotes.TabIndex = 1;
			this.SheetNotes.Text = "Заметки";
			// 
			// SheetSources
			// 
			this.SheetSources.Location = new System.Drawing.Point(4, 26);
			this.SheetSources.Name = "SheetSources";
			this.SheetSources.Size = new System.Drawing.Size(720, 272);
			this.SheetSources.TabIndex = 2;
			this.SheetSources.Text = "Источники";
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(482, 321);
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
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(605, 321);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(113, 30);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// btnView
			// 
			this.btnView.Location = new System.Drawing.Point(11, 321);
			this.btnView.Name = "btnView";
			this.btnView.Size = new System.Drawing.Size(114, 30);
			this.btnView.TabIndex = 1;
			this.btnView.Text = "Просмотр...";
			this.btnView.Click += new System.EventHandler(this.btnView_Click);
			// 
			// OpenDialog1
			// 
			this.OpenDialog1.Filter = "Все файлы (*.*)|*.*";
			// 
			// MediaEditDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(728, 365);
			this.Controls.Add(this.PagesData);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.btnView);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "MediaEditDlg";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Редактирование мультимедиа объекта";
			this.PagesData.ResumeLayout(false);
			this.SheetCommon.ResumeLayout(false);
			this.SheetCommon.PerformLayout();
			this.ResumeLayout(false);
		}
	}
}