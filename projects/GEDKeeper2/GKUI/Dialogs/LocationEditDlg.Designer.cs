using System;

namespace GKUI.Dialogs
{
	partial class LocationEditDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.TabControl PagesData;
		private System.Windows.Forms.TabPage SheetNotes;
		private System.Windows.Forms.TabPage SheetMultimedia;
		private System.Windows.Forms.TabPage SheetCommon;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.TextBox EditName;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.TextBox EditLatitude;
		private System.Windows.Forms.Label Label3;
		private System.Windows.Forms.TextBox EditLongitude;
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.ListView ListGeoCoords;
		private System.Windows.Forms.Button btnSearch;
		private System.Windows.Forms.Button btnSelect;
		private System.Windows.Forms.Button btnSelectName;
		private System.Windows.Forms.Button btnShowOnMap;
		private System.Windows.Forms.Panel panMap;
		private System.Windows.Forms.ColumnHeader ColumnHeader1;
		private System.Windows.Forms.ColumnHeader ColumnHeader2;
		private System.Windows.Forms.ColumnHeader ColumnHeader3;

		private void InitializeComponent()
		{
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.PagesData = new System.Windows.Forms.TabControl();
			this.SheetCommon = new System.Windows.Forms.TabPage();
			this.Label1 = new System.Windows.Forms.Label();
			this.Label2 = new System.Windows.Forms.Label();
			this.Label3 = new System.Windows.Forms.Label();
			this.EditName = new System.Windows.Forms.TextBox();
			this.EditLatitude = new System.Windows.Forms.TextBox();
			this.EditLongitude = new System.Windows.Forms.TextBox();
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.ListGeoCoords = new System.Windows.Forms.ListView();
			this.ColumnHeader1 = new System.Windows.Forms.ColumnHeader();
			this.ColumnHeader2 = new System.Windows.Forms.ColumnHeader();
			this.ColumnHeader3 = new System.Windows.Forms.ColumnHeader();
			this.btnSearch = new System.Windows.Forms.Button();
			this.btnSelect = new System.Windows.Forms.Button();
			this.btnSelectName = new System.Windows.Forms.Button();
			this.panMap = new System.Windows.Forms.Panel();
			this.btnShowOnMap = new System.Windows.Forms.Button();
			this.SheetNotes = new System.Windows.Forms.TabPage();
			this.SheetMultimedia = new System.Windows.Forms.TabPage();
			this.PagesData.SuspendLayout();
			this.SheetCommon.SuspendLayout();
			this.GroupBox1.SuspendLayout();
			this.SuspendLayout();
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(538, 534);
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
			this.btnCancel.Location = new System.Drawing.Point(661, 534);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(113, 31);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// PagesData
			// 
			this.PagesData.Controls.Add(this.SheetCommon);
			this.PagesData.Controls.Add(this.SheetNotes);
			this.PagesData.Controls.Add(this.SheetMultimedia);
			this.PagesData.Location = new System.Drawing.Point(0, 0);
			this.PagesData.Name = "PagesData";
			this.PagesData.SelectedIndex = 0;
			this.PagesData.Size = new System.Drawing.Size(785, 516);
			this.PagesData.TabIndex = 0;
			// 
			// SheetCommon
			// 
			this.SheetCommon.Controls.Add(this.Label1);
			this.SheetCommon.Controls.Add(this.Label2);
			this.SheetCommon.Controls.Add(this.Label3);
			this.SheetCommon.Controls.Add(this.EditName);
			this.SheetCommon.Controls.Add(this.EditLatitude);
			this.SheetCommon.Controls.Add(this.EditLongitude);
			this.SheetCommon.Controls.Add(this.GroupBox1);
			this.SheetCommon.Controls.Add(this.btnShowOnMap);
			this.SheetCommon.Location = new System.Drawing.Point(4, 26);
			this.SheetCommon.Name = "SheetCommon";
			this.SheetCommon.Size = new System.Drawing.Size(777, 486);
			this.SheetCommon.TabIndex = 0;
			this.SheetCommon.Text = "Основное";
			// 
			// Label1
			// 
			this.Label1.AutoSize = true;
			this.Label1.Location = new System.Drawing.Point(22, 10);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(67, 17);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Название";
			// 
			// Label2
			// 
			this.Label2.AutoSize = true;
			this.Label2.Location = new System.Drawing.Point(402, 10);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(59, 17);
			this.Label2.TabIndex = 2;
			this.Label2.Text = "Широта";
			// 
			// Label3
			// 
			this.Label3.AutoSize = true;
			this.Label3.Location = new System.Drawing.Point(525, 10);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(63, 17);
			this.Label3.TabIndex = 4;
			this.Label3.Text = "Долгота";
			// 
			// EditName
			// 
			this.EditName.Location = new System.Drawing.Point(22, 29);
			this.EditName.Name = "EditName";
			this.EditName.Size = new System.Drawing.Size(371, 24);
			this.EditName.TabIndex = 1;
			this.EditName.TextChanged += new System.EventHandler(this.EditName_TextChanged);
			this.EditName.KeyDown += new System.Windows.Forms.KeyEventHandler(this.EditName_KeyDown);
			// 
			// EditLatitude
			// 
			this.EditLatitude.Location = new System.Drawing.Point(402, 29);
			this.EditLatitude.Name = "EditLatitude";
			this.EditLatitude.Size = new System.Drawing.Size(113, 24);
			this.EditLatitude.TabIndex = 3;
			// 
			// EditLongitude
			// 
			this.EditLongitude.Location = new System.Drawing.Point(525, 29);
			this.EditLongitude.Name = "EditLongitude";
			this.EditLongitude.Size = new System.Drawing.Size(113, 24);
			this.EditLongitude.TabIndex = 5;
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.ListGeoCoords);
			this.GroupBox1.Controls.Add(this.btnSearch);
			this.GroupBox1.Controls.Add(this.btnSelect);
			this.GroupBox1.Controls.Add(this.btnSelectName);
			this.GroupBox1.Controls.Add(this.panMap);
			this.GroupBox1.Location = new System.Drawing.Point(0, 62);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(774, 420);
			this.GroupBox1.TabIndex = 7;
			this.GroupBox1.TabStop = false;
			this.GroupBox1.Text = "Поиск координат (Google Maps)";
			// 
			// ListGeoCoords
			// 
			this.ListGeoCoords.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
									this.ColumnHeader1,
									this.ColumnHeader2,
									this.ColumnHeader3});
			this.ListGeoCoords.FullRowSelect = true;
			this.ListGeoCoords.Location = new System.Drawing.Point(22, 19);
			this.ListGeoCoords.Name = "ListGeoCoords";
			this.ListGeoCoords.Size = new System.Drawing.Size(563, 109);
			this.ListGeoCoords.TabIndex = 1;
			this.ListGeoCoords.UseCompatibleStateImageBehavior = false;
			this.ListGeoCoords.View = System.Windows.Forms.View.Details;
			this.ListGeoCoords.Click += new System.EventHandler(this.ListGeoCoords_Click);
			// 
			// ColumnHeader1
			// 
			this.ColumnHeader1.Text = "Название";
			this.ColumnHeader1.Width = 200;
			// 
			// ColumnHeader2
			// 
			this.ColumnHeader2.Text = "Широта";
			this.ColumnHeader2.Width = 80;
			// 
			// ColumnHeader3
			// 
			this.ColumnHeader3.Text = "Долгота";
			this.ColumnHeader3.Width = 80;
			// 
			// btnSearch
			// 
			this.btnSearch.Location = new System.Drawing.Point(601, 19);
			this.btnSearch.Name = "btnSearch";
			this.btnSearch.Size = new System.Drawing.Size(147, 31);
			this.btnSearch.TabIndex = 2;
			this.btnSearch.Text = "Поиск";
			this.btnSearch.Click += new System.EventHandler(this.btnSearch_Click);
			// 
			// btnSelect
			// 
			this.btnSelect.Location = new System.Drawing.Point(601, 58);
			this.btnSelect.Name = "btnSelect";
			this.btnSelect.Size = new System.Drawing.Size(147, 31);
			this.btnSelect.TabIndex = 3;
			this.btnSelect.Text = "Выбрать коорд.";
			this.btnSelect.Click += new System.EventHandler(this.btnSelect_Click);
			// 
			// btnSelectName
			// 
			this.btnSelectName.Location = new System.Drawing.Point(601, 97);
			this.btnSelectName.Name = "btnSelectName";
			this.btnSelectName.Size = new System.Drawing.Size(147, 31);
			this.btnSelectName.TabIndex = 4;
			this.btnSelectName.Text = "Выбрать название";
			this.btnSelectName.Click += new System.EventHandler(this.btnSelectName_Click);
			// 
			// panMap
			// 
			this.panMap.Location = new System.Drawing.Point(3, 135);
			this.panMap.Name = "panMap";
			this.panMap.Size = new System.Drawing.Size(768, 283);
			this.panMap.TabIndex = 0;
			// 
			// btnShowOnMap
			// 
			this.btnShowOnMap.AccessibleDescription = "Показать на карте";
			this.btnShowOnMap.Location = new System.Drawing.Point(650, 29);
			this.btnShowOnMap.Name = "btnShowOnMap";
			this.btnShowOnMap.Size = new System.Drawing.Size(98, 26);
			this.btnShowOnMap.TabIndex = 6;
			this.btnShowOnMap.Text = "Показать";
			this.btnShowOnMap.Click += new System.EventHandler(this.btnShowOnMap_Click);
			// 
			// SheetNotes
			// 
			this.SheetNotes.Location = new System.Drawing.Point(4, 26);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new System.Drawing.Size(777, 486);
			this.SheetNotes.TabIndex = 1;
			this.SheetNotes.Text = "Заметки";
			// 
			// SheetMultimedia
			// 
			this.SheetMultimedia.Location = new System.Drawing.Point(4, 26);
			this.SheetMultimedia.Name = "SheetMultimedia";
			this.SheetMultimedia.Size = new System.Drawing.Size(777, 486);
			this.SheetMultimedia.TabIndex = 2;
			this.SheetMultimedia.Text = "Мультимедиа";
			// 
			// LocationEditDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(784, 578);
			this.Controls.Add(this.PagesData);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "LocationEditDlg";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Местоположение";
			this.PagesData.ResumeLayout(false);
			this.SheetCommon.ResumeLayout(false);
			this.SheetCommon.PerformLayout();
			this.GroupBox1.ResumeLayout(false);
			this.ResumeLayout(false);
		}
	}
}