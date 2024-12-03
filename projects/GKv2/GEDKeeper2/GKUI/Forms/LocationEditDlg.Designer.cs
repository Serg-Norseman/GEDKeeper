namespace GKUI.Forms
{
    partial class LocationEditDlg
    {
        private System.Windows.Forms.Button btnAccept;
        private System.Windows.Forms.Button btnCancel;
        private GKUI.Components.GKTabControl tabsData;
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
        private GKUI.Components.GKListView ListGeoCoords;
        private System.Windows.Forms.Button btnSearch;
        private System.Windows.Forms.Button btnSelect;
        private System.Windows.Forms.Button btnSelectName;
        private System.Windows.Forms.Button btnSelectCursor;
        private System.Windows.Forms.Button btnShowOnMap;
        private System.Windows.Forms.Panel panMap;
        private System.Windows.Forms.ColumnHeader ColumnHeader1;
        private System.Windows.Forms.ColumnHeader ColumnHeader2;
        private System.Windows.Forms.ColumnHeader ColumnHeader3;
        private System.Windows.Forms.TabPage pageHistory;
        private System.Windows.Forms.TableLayoutPanel tabsHistory;
        private System.Windows.Forms.GroupBox pageHistNames;
        private System.Windows.Forms.GroupBox pageHistLinks;

        private void InitializeComponent()
        {
            this.btnAccept = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.tabsData = new GKUI.Components.GKTabControl();
            this.pageCommon = new System.Windows.Forms.TabPage();
            this.lblName = new System.Windows.Forms.Label();
            this.lblLatitude = new System.Windows.Forms.Label();
            this.lblLongitude = new System.Windows.Forms.Label();
            this.txtName = new System.Windows.Forms.TextBox();
            this.txtLatitude = new System.Windows.Forms.TextBox();
            this.txtLongitude = new System.Windows.Forms.TextBox();
            this.grpSearch = new System.Windows.Forms.GroupBox();
            this.ListGeoCoords = new GKUI.Components.GKListView();
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
            this.pageHistory = new System.Windows.Forms.TabPage();
            this.tabsHistory = new System.Windows.Forms.TableLayoutPanel();
            this.pageHistNames = new System.Windows.Forms.GroupBox();
            this.pageHistLinks = new System.Windows.Forms.GroupBox();
            this.btnSelectCursor = new System.Windows.Forms.Button();
            this.tabsData.SuspendLayout();
            this.tabsHistory.SuspendLayout();
            this.pageCommon.SuspendLayout();
            this.grpSearch.SuspendLayout();
            this.SuspendLayout();
            // 
            // btnAccept
            // 
            this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnAccept.Location = new System.Drawing.Point(598, 574);
            this.btnAccept.Name = "btnAccept";
            this.btnAccept.Size = new System.Drawing.Size(113, 31);
            this.btnAccept.TabIndex = 1;
            this.btnAccept.Text = "btnAccept";
            this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.AcceptClickHandler);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(721, 574);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(113, 31);
            this.btnCancel.TabIndex = 2;
            this.btnCancel.Text = "btnCancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnCancel.Click += new System.EventHandler(this.CancelClickHandler);
            // 
            // tabsData
            // 
            this.tabsData.Controls.Add(this.pageCommon);
            this.tabsData.Controls.Add(this.pageHistory);
            this.tabsData.Controls.Add(this.pageNotes);
            this.tabsData.Controls.Add(this.pageMultimedia);
            this.tabsData.Location = new System.Drawing.Point(0, 0);
            this.tabsData.Name = "tabsData";
            this.tabsData.SelectedIndex = 0;
            this.tabsData.Size = new System.Drawing.Size(844, 556);
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
            this.lblName.Size = new System.Drawing.Size(55, 17);
            this.lblName.TabIndex = 0;
            this.lblName.Text = "lblName";
            // 
            // lblLatitude
            // 
            this.lblLatitude.AutoSize = true;
            this.lblLatitude.Location = new System.Drawing.Point(402, 10);
            this.lblLatitude.Name = "lblLatitude";
            this.lblLatitude.Size = new System.Drawing.Size(69, 17);
            this.lblLatitude.TabIndex = 2;
            this.lblLatitude.Text = "lblLatitude";
            // 
            // lblLongitude
            // 
            this.lblLongitude.AutoSize = true;
            this.lblLongitude.Location = new System.Drawing.Point(525, 10);
            this.lblLongitude.Name = "lblLongitude";
            this.lblLongitude.Size = new System.Drawing.Size(81, 17);
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
            this.grpSearch.Controls.Add(this.btnSelectCursor);
            this.grpSearch.Controls.Add(this.panMap);
            this.grpSearch.Location = new System.Drawing.Point(0, 62);
            this.grpSearch.Name = "grpSearch";
            this.grpSearch.Size = new System.Drawing.Size(834, 460);
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
            this.ListGeoCoords.Size = new System.Drawing.Size(623, 149);
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
            this.btnSearch.Location = new System.Drawing.Point(661, 19);
            this.btnSearch.Name = "btnSearch";
            this.btnSearch.Size = new System.Drawing.Size(157, 31);
            this.btnSearch.TabIndex = 2;
            this.btnSearch.Text = "btnSearch";
            this.btnSearch.Click += new System.EventHandler(this.btnSearch_Click);
            // 
            // btnSelect
            // 
            this.btnSelect.Location = new System.Drawing.Point(661, 58);
            this.btnSelect.Name = "btnSelect";
            this.btnSelect.Size = new System.Drawing.Size(157, 31);
            this.btnSelect.TabIndex = 3;
            this.btnSelect.Text = "btnSelect";
            this.btnSelect.Click += new System.EventHandler(this.btnSelect_Click);
            // 
            // btnSelectName
            // 
            this.btnSelectName.Location = new System.Drawing.Point(661, 97);
            this.btnSelectName.Name = "btnSelectName";
            this.btnSelectName.Size = new System.Drawing.Size(157, 31);
            this.btnSelectName.TabIndex = 4;
            this.btnSelectName.Text = "btnSelectName";
            this.btnSelectName.Click += new System.EventHandler(this.btnSelectName_Click);
            // 
            // btnSelectCursor
            // 
            this.btnSelectCursor.Location = new System.Drawing.Point(661, 136);
            this.btnSelectCursor.Name = "btnSelectCursor";
            this.btnSelectCursor.Size = new System.Drawing.Size(157, 31);
            this.btnSelectCursor.TabIndex = 5;
            this.btnSelectCursor.Text = "btnSelectCursor";
            this.btnSelectCursor.Click += new System.EventHandler(this.btnSelectCursor_Click);
            // 
            // panMap
            // 
            this.panMap.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.panMap.Location = new System.Drawing.Point(3, 175);
            this.panMap.Name = "panMap";
            this.panMap.Size = new System.Drawing.Size(828, 283);
            this.panMap.TabIndex = 0;
            // 
            // btnShowOnMap
            // 
            this.btnShowOnMap.Location = new System.Drawing.Point(650, 29);
            this.btnShowOnMap.Name = "btnShowOnMap";
            this.btnShowOnMap.Size = new System.Drawing.Size(98, 26);
            this.btnShowOnMap.TabIndex = 6;
            this.btnShowOnMap.Text = "btnShowOnMap";
            this.btnShowOnMap.Click += new System.EventHandler(this.btnShowOnMap_Click);
            // 
            // pageHistory
            // 
            this.pageHistory.Controls.Add(this.tabsHistory);
            this.pageHistory.Location = new System.Drawing.Point(4, 26);
            this.pageHistory.Name = "pageHistory";
            this.pageHistory.Size = new System.Drawing.Size(777, 486);
            this.pageHistory.TabIndex = 1;
            this.pageHistory.Text = "pageHistory";
            // 
            // pageNotes
            // 
            this.pageNotes.Location = new System.Drawing.Point(4, 26);
            this.pageNotes.Name = "pageNotes";
            this.pageNotes.Size = new System.Drawing.Size(777, 486);
            this.pageNotes.TabIndex = 2;
            this.pageNotes.Text = "pageNotes";
            // 
            // pageMultimedia
            // 
            this.pageMultimedia.Location = new System.Drawing.Point(4, 26);
            this.pageMultimedia.Name = "pageMultimedia";
            this.pageMultimedia.Size = new System.Drawing.Size(777, 486);
            this.pageMultimedia.TabIndex = 3;
            this.pageMultimedia.Text = "pageMultimedia";
            // 
            // pageHistNames
            //
            this.pageHistNames.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pageHistNames.Location = new System.Drawing.Point(4, 26);
            this.pageHistNames.Name = "pageHistNames";
            this.pageHistNames.Size = new System.Drawing.Size(777, 250);
            this.pageHistNames.TabIndex = 0;
            this.pageHistNames.Text = "pageHistNames";
            // 
            // pageHistLinks
            //
            this.pageHistLinks.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pageHistLinks.Location = new System.Drawing.Point(4, 26);
            this.pageHistLinks.Name = "pageHistLinks";
            this.pageHistLinks.Size = new System.Drawing.Size(777, 250);
            this.pageHistLinks.TabIndex = 1;
            this.pageHistLinks.Text = "pageHistLinks";
            // 
            // tabsHistory
            // 
            this.tabsHistory.Controls.Add(this.pageHistNames, 0, 0);
            this.tabsHistory.Controls.Add(this.pageHistLinks, 0, 1);
            this.tabsHistory.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50));
            this.tabsHistory.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50));
            this.tabsHistory.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabsHistory.Location = new System.Drawing.Point(0, 0);
            this.tabsHistory.Name = "tabsHistory";
            this.tabsHistory.Size = new System.Drawing.Size(785, 516);
            this.tabsHistory.TabIndex = 0;
            // 
            // LocationEditDlg
            // 
            this.AcceptButton = this.btnAccept;
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(844, 618);
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
            this.tabsHistory.ResumeLayout(false);
            this.tabsData.ResumeLayout(false);
            this.pageCommon.ResumeLayout(false);
            this.pageCommon.PerformLayout();
            this.grpSearch.ResumeLayout(false);
            this.ResumeLayout(false);
        }
    }
}
