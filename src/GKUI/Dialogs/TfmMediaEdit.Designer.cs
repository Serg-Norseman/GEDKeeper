using System;

namespace GKUI
{
	partial class TfmMediaEdit
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
			this.PagesData.Location = new System.Drawing.Point(0, 0);
			this.PagesData.Name = "PagesData";
			this.PagesData.SelectedIndex = 0;
			this.PagesData.Size = new System.Drawing.Size(522, 249);
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
			this.SheetCommon.Location = new System.Drawing.Point(4, 22);
			this.SheetCommon.Name = "SheetCommon";
			this.SheetCommon.Size = new System.Drawing.Size(514, 223);
			this.SheetCommon.TabIndex = 0;
			this.SheetCommon.Text = "Общие данные";
			// 
			// Label1
			// 
			this.Label1.Location = new System.Drawing.Point(8, 8);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(55, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Название";
			// 
			// Label2
			// 
			this.Label2.Location = new System.Drawing.Point(8, 56);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(25, 13);
			this.Label2.TabIndex = 2;
			this.Label2.Text = "Тип";
			// 
			// Label4
			// 
			this.Label4.Location = new System.Drawing.Point(192, 56);
			this.Label4.Name = "Label4";
			this.Label4.Size = new System.Drawing.Size(95, 13);
			this.Label4.TabIndex = 4;
			this.Label4.Text = "Способ хранения";
			// 
			// Label3
			// 
			this.Label3.Location = new System.Drawing.Point(8, 104);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(35, 13);
			this.Label3.TabIndex = 6;
			this.Label3.Text = "Файл";
			// 
			// edName
			// 
			this.edName.Location = new System.Drawing.Point(8, 24);
			this.edName.Name = "edName";
			this.edName.Size = new System.Drawing.Size(497, 21);
			this.edName.TabIndex = 1;
			this.edName.TextChanged += new System.EventHandler(this.edName_TextChanged);
			// 
			// cbMediaType
			// 
			this.cbMediaType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbMediaType.DropDownWidth = 15;
			this.cbMediaType.Location = new System.Drawing.Point(8, 72);
			this.cbMediaType.Name = "cbMediaType";
			this.cbMediaType.Size = new System.Drawing.Size(169, 21);
			this.cbMediaType.TabIndex = 3;
			// 
			// cbStoreType
			// 
			this.cbStoreType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbStoreType.Location = new System.Drawing.Point(192, 72);
			this.cbStoreType.Name = "cbStoreType";
			this.cbStoreType.Size = new System.Drawing.Size(201, 21);
			this.cbStoreType.TabIndex = 5;
			// 
			// edFile
			// 
			this.edFile.Location = new System.Drawing.Point(8, 120);
			this.edFile.Name = "edFile";
			this.edFile.ReadOnly = true;
			this.edFile.Size = new System.Drawing.Size(449, 21);
			this.edFile.TabIndex = 7;
			// 
			// btnFileSelect
			// 
			this.btnFileSelect.Location = new System.Drawing.Point(464, 120);
			this.btnFileSelect.Name = "btnFileSelect";
			this.btnFileSelect.Size = new System.Drawing.Size(43, 21);
			this.btnFileSelect.TabIndex = 8;
			this.btnFileSelect.Text = "...";
			this.btnFileSelect.Click += new System.EventHandler(this.btnFileSelect_Click);
			// 
			// SheetNotes
			// 
			this.SheetNotes.Location = new System.Drawing.Point(4, 22);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new System.Drawing.Size(514, 223);
			this.SheetNotes.TabIndex = 1;
			this.SheetNotes.Text = "Заметки";
			// 
			// SheetSources
			// 
			this.SheetSources.Location = new System.Drawing.Point(4, 22);
			this.SheetSources.Name = "SheetSources";
			this.SheetSources.Size = new System.Drawing.Size(514, 223);
			this.SheetSources.TabIndex = 2;
			this.SheetSources.Text = "Источники";
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(344, 264);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(81, 25);
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
			this.btnCancel.Location = new System.Drawing.Point(432, 264);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(81, 25);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// btnView
			// 
			this.btnView.Location = new System.Drawing.Point(8, 264);
			this.btnView.Name = "btnView";
			this.btnView.Size = new System.Drawing.Size(81, 25);
			this.btnView.TabIndex = 1;
			this.btnView.Text = "Просмотр...";
			this.btnView.Click += new System.EventHandler(this.btnView_Click);
			// 
			// OpenDialog1
			// 
			this.OpenDialog1.Filter = "Все файлы (*.*)|*.*";
			// 
			// TfmMediaEdit
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(522, 298);
			this.Controls.Add(this.PagesData);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.btnView);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TfmMediaEdit";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Редактирование мультимедиа объекта";
			this.PagesData.ResumeLayout(false);
			this.SheetCommon.ResumeLayout(false);
			this.SheetCommon.PerformLayout();
			this.ResumeLayout(false);
		}
	}
}