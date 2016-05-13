using System;

namespace GKUI.Dialogs
{
	partial class LocationEditDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.TabControl tabsData;
		private System.Windows.Forms.TabPage pageNotes;
		private System.Windows.Forms.TabPage pageMultimedia;
		private System.Windows.Forms.TabPage pageCommon;
		private System.Windows.Forms.Label lblName;
		private System.Windows.Forms.TextBox txtName;
		private System.Windows.Forms.Label lblLatitude;
		private System.Windows.Forms.TextBox txtLatitude;
		private System.Windows.Forms.Label lblLongitude;
		private System.Windows.Forms.TextBox txtLongitude;
		private System.Windows.Forms.GroupBox grpSearch;
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
			this.tabsData = new System.Windows.Forms.TabControl();
			this.pageCommon = new System.Windows.Forms.TabPage();
			this.lblName = new System.Windows.Forms.Label();
			this.lblLatitude = new System.Windows.Forms.Label();
			this.lblLongitude = new System.Windows.Forms.Label();
			this.txtName = new System.Windows.Forms.TextBox();
			this.txtLatitude = new System.Windows.Forms.TextBox();
			this.txtLongitude = new System.Windows.Forms.TextBox();
			this.grpSearch = new System.Windows.Forms.GroupBox();
			this.ListGeoCoords = new System.Windows.Forms.ListView();
			this.ColumnHeader1 = new System.Windows.Forms.ColumnHeader();
			this.ColumnHeader2 = new System.Windows.Forms.ColumnHeader();
			this.ColumnHeader3 = new System.Windows.Forms.ColumnHeader();
			this.btnSearch = new System.Windows.Forms.Button();
			this.btnSelect = new System.Windows.Forms.Button();
			this.btnSelectName = new System.Windows.Forms.Button();
			this.panMap = new System.Windows.Forms.Panel();
			this.btnShowOnMap = new System.Windows.Forms.Button();
			this.pageNotes = new System.Windows.Forms.TabPage();
			this.pageMultimedia = new System.Windows.Forms.TabPage();
			this.tabsData.SuspendLayout();
			this.pageCommon.SuspendLayout();
			this.grpSearch.SuspendLayout();
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
			this.btnAccept.Text = "btnAccept";
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
			this.btnCancel.Text = "btnCancel";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// tabsData
			// 
			this.tabsData.Controls.Add(this.pageCommon);
			this.tabsData.Controls.Add(this.pageNotes);
			this.tabsData.Controls.Add(this.pageMultimedia);
			this.tabsData.Location = new System.Drawing.Point(0, 0);
			this.tabsData.Name = "tabsData";
			this.tabsData.SelectedIndex = 0;
			this.tabsData.Size = new System.Drawing.Size(785, 516);
			this.tabsData.TabIndex = 0;
			// 
			// pageCommon
			// 
			this.pageCommon.Controls.Add(this.lblName);
			this.pageCommon.Controls.Add(this.lblLatitude);
			this.pageCommon.Controls.Add(this.lblLongitude);
			this.pageCommon.Controls.Add(this.txtName);
			this.pageCommon.Controls.Add(this.txtLatitude);
			this.pageCommon.Controls.Add(this.txtLongitude);
			this.pageCommon.Controls.Add(this.grpSearch);
			this.pageCommon.Controls.Add(this.btnShowOnMap);
			this.pageCommon.Location = new System.Drawing.Point(4, 26);
			this.pageCommon.Name = "pageCommon";
			this.pageCommon.Size = new System.Drawing.Size(777, 486);
			this.pageCommon.TabIndex = 0;
			this.pageCommon.Text = "pageCommon";
			// 
			// lblName
			// 
			this.lblName.AutoSize = true;
			this.lblName.Location = new System.Drawing.Point(22, 10);
			this.lblName.Name = "lblName";
			this.lblName.Size = new System.Drawing.Size(67, 17);
			this.lblName.TabIndex = 0;
			this.lblName.Text = "lblName";
			// 
			// lblLatitude
			// 
			this.lblLatitude.AutoSize = true;
			this.lblLatitude.Location = new System.Drawing.Point(402, 10);
			this.lblLatitude.Name = "lblLatitude";
			this.lblLatitude.Size = new System.Drawing.Size(59, 17);
			this.lblLatitude.TabIndex = 2;
			this.lblLatitude.Text = "lblLatitude";
			// 
			// lblLongitude
			// 
			this.lblLongitude.AutoSize = true;
			this.lblLongitude.Location = new System.Drawing.Point(525, 10);
			this.lblLongitude.Name = "lblLongitude";
			this.lblLongitude.Size = new System.Drawing.Size(63, 17);
			this.lblLongitude.TabIndex = 4;
			this.lblLongitude.Text = "lblLongitude";
			// 
			// txtName
			// 
			this.txtName.Location = new System.Drawing.Point(22, 29);
			this.txtName.Name = "txtName";
			this.txtName.Size = new System.Drawing.Size(371, 24);
			this.txtName.TabIndex = 1;
			this.txtName.TextChanged += new System.EventHandler(this.EditName_TextChanged);
			this.txtName.KeyDown += new System.Windows.Forms.KeyEventHandler(this.EditName_KeyDown);
			// 
			// txtLatitude
			// 
			this.txtLatitude.Location = new System.Drawing.Point(402, 29);
			this.txtLatitude.Name = "txtLatitude";
			this.txtLatitude.Size = new System.Drawing.Size(113, 24);
			this.txtLatitude.TabIndex = 3;
			// 
			// txtLongitude
			// 
			this.txtLongitude.Location = new System.Drawing.Point(525, 29);
			this.txtLongitude.Name = "txtLongitude";
			this.txtLongitude.Size = new System.Drawing.Size(113, 24);
			this.txtLongitude.TabIndex = 5;
			// 
			// grpSearch
			// 
			this.grpSearch.Controls.Add(this.ListGeoCoords);
			this.grpSearch.Controls.Add(this.btnSearch);
			this.grpSearch.Controls.Add(this.btnSelect);
			this.grpSearch.Controls.Add(this.btnSelectName);
			this.grpSearch.Controls.Add(this.panMap);
			this.grpSearch.Location = new System.Drawing.Point(0, 62);
			this.grpSearch.Name = "grpSearch";
			this.grpSearch.Size = new System.Drawing.Size(774, 420);
			this.grpSearch.TabIndex = 7;
			this.grpSearch.TabStop = false;
			this.grpSearch.Text = "grpSearch";
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
			this.ColumnHeader1.Text = "Name";
			this.ColumnHeader1.Width = 200;
			// 
			// ColumnHeader2
			// 
			this.ColumnHeader2.Text = "Latitude";
			this.ColumnHeader2.Width = 80;
			// 
			// ColumnHeader3
			// 
			this.ColumnHeader3.Text = "Longitude";
			this.ColumnHeader3.Width = 80;
			// 
			// btnSearch
			// 
			this.btnSearch.Location = new System.Drawing.Point(601, 19);
			this.btnSearch.Name = "btnSearch";
			this.btnSearch.Size = new System.Drawing.Size(147, 31);
			this.btnSearch.TabIndex = 2;
			this.btnSearch.Text = "btnSearch";
			this.btnSearch.Click += new System.EventHandler(this.btnSearch_Click);
			// 
			// btnSelect
			// 
			this.btnSelect.Location = new System.Drawing.Point(601, 58);
			this.btnSelect.Name = "btnSelect";
			this.btnSelect.Size = new System.Drawing.Size(147, 31);
			this.btnSelect.TabIndex = 3;
			this.btnSelect.Text = "btnSelect";
			this.btnSelect.Click += new System.EventHandler(this.btnSelect_Click);
			// 
			// btnSelectName
			// 
			this.btnSelectName.Location = new System.Drawing.Point(601, 97);
			this.btnSelectName.Name = "btnSelectName";
			this.btnSelectName.Size = new System.Drawing.Size(147, 31);
			this.btnSelectName.TabIndex = 4;
			this.btnSelectName.Text = "btnSelectName";
			this.btnSelectName.Click += new System.EventHandler(this.btnSelectName_Click);
			// 
			// panMap
			// 
			this.panMap.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
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
			this.btnShowOnMap.Text = "btnShowOnMap";
			this.btnShowOnMap.Click += new System.EventHandler(this.btnShowOnMap_Click);
			// 
			// pageNotes
			// 
			this.pageNotes.Location = new System.Drawing.Point(4, 26);
			this.pageNotes.Name = "pageNotes";
			this.pageNotes.Size = new System.Drawing.Size(777, 486);
			this.pageNotes.TabIndex = 1;
			this.pageNotes.Text = "pageNotes";
			// 
			// pageMultimedia
			// 
			this.pageMultimedia.Location = new System.Drawing.Point(4, 26);
			this.pageMultimedia.Name = "pageMultimedia";
			this.pageMultimedia.Size = new System.Drawing.Size(777, 486);
			this.pageMultimedia.TabIndex = 2;
			this.pageMultimedia.Text = "pageMultimedia";
			// 
			// LocationEditDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(784, 578);
			this.Controls.Add(this.tabsData);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "LocationEditDlg";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "LocationEditDlg";
			this.tabsData.ResumeLayout(false);
			this.pageCommon.ResumeLayout(false);
			this.pageCommon.PerformLayout();
			this.grpSearch.ResumeLayout(false);
			this.ResumeLayout(false);
		}
	}
}