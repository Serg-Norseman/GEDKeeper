namespace GKUI.Forms
{
    partial class MapsViewerWin
    {
        private System.ComponentModel.IContainer components = null;
        private System.Windows.Forms.StatusStrip StatusBar1;
        private GKUI.Components.GKTabControl PageControl1;
        private System.Windows.Forms.TabPage pagePlaces;
        private System.Windows.Forms.TreeView tvPlaces;
        private System.Windows.Forms.GroupBox grpSelection;
        private System.Windows.Forms.ComboBox cmbPersons;
        private System.Windows.Forms.CheckBox chkResidence;
        private System.Windows.Forms.CheckBox chkDeath;
        private System.Windows.Forms.CheckBox chkBirth;
        private System.Windows.Forms.Button btnSelectPlaces;
        private System.Windows.Forms.RadioButton radTotal;
        private System.Windows.Forms.RadioButton radSelected;
        private System.Windows.Forms.CheckBox chkLinesVisible;
        private System.Windows.Forms.ToolStrip ToolBar1;
        private System.Windows.Forms.ToolStripButton tbLoadPlaces;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private System.Windows.Forms.ToolStripButton tbSaveSnapshot;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
        private System.Windows.Forms.ToolStripDropDownButton tbProviders;
        private System.Windows.Forms.ContextMenuStrip MenuProviders;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator3;
        private System.Windows.Forms.ToolStripButton tbClear;
        private System.Windows.Forms.ToolStripButton tbZoomCenter;
        private System.Windows.Forms.TabPage pageCoordinates;
        private System.Windows.Forms.GroupBox gbCoords;
        private System.Windows.Forms.TextBox txtPlace;
        private System.Windows.Forms.Button btnSearch;
        private System.Windows.Forms.TableLayoutPanel panClient;
        private System.Windows.Forms.TrackBar trkZoom;
        private System.Windows.Forms.Button btnZoomUp;
        private System.Windows.Forms.Button btnZoomDown;
        private System.Windows.Forms.TableLayoutPanel panZoom;

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null)
                    components.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.ToolBar1 = new System.Windows.Forms.ToolStrip();
            this.tbLoadPlaces = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.tbSaveSnapshot = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
            this.tbProviders = new System.Windows.Forms.ToolStripDropDownButton();
            this.MenuProviders = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.toolStripSeparator3 = new System.Windows.Forms.ToolStripSeparator();
            this.tbClear = new System.Windows.Forms.ToolStripButton();
            this.tbZoomCenter = new System.Windows.Forms.ToolStripButton();
            this.StatusBar1 = new System.Windows.Forms.StatusStrip();
            this.PageControl1 = new GKUI.Components.GKTabControl();
            this.pagePlaces = new System.Windows.Forms.TabPage();
            this.tvPlaces = new System.Windows.Forms.TreeView();
            this.grpSelection = new System.Windows.Forms.GroupBox();
            this.cmbPersons = new System.Windows.Forms.ComboBox();
            this.chkResidence = new System.Windows.Forms.CheckBox();
            this.chkDeath = new System.Windows.Forms.CheckBox();
            this.chkBirth = new System.Windows.Forms.CheckBox();
            this.btnSelectPlaces = new System.Windows.Forms.Button();
            this.radTotal = new System.Windows.Forms.RadioButton();
            this.radSelected = new System.Windows.Forms.RadioButton();
            this.chkLinesVisible = new System.Windows.Forms.CheckBox();
            this.pageCoordinates = new System.Windows.Forms.TabPage();
            this.gbCoords = new System.Windows.Forms.GroupBox();
            this.txtPlace = new System.Windows.Forms.TextBox();
            this.btnSearch = new System.Windows.Forms.Button();
            this.panClient = new System.Windows.Forms.TableLayoutPanel();
            this.panZoom = new System.Windows.Forms.TableLayoutPanel();
            this.btnZoomUp = new System.Windows.Forms.Button();
            this.trkZoom = new System.Windows.Forms.TrackBar();
            this.btnZoomDown = new System.Windows.Forms.Button();
            this.ToolBar1.SuspendLayout();
            this.PageControl1.SuspendLayout();
            this.pagePlaces.SuspendLayout();
            this.grpSelection.SuspendLayout();
            this.pageCoordinates.SuspendLayout();
            this.gbCoords.SuspendLayout();
            this.panClient.SuspendLayout();
            this.panZoom.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.trkZoom)).BeginInit();
            this.SuspendLayout();
            // 
            // ToolBar1
            // 
            this.ToolBar1.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.ToolBar1.ImageScalingSize = new System.Drawing.Size(20, 20);
            this.ToolBar1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.tbLoadPlaces,
            this.toolStripSeparator1,
            this.tbSaveSnapshot,
            this.toolStripSeparator2,
            this.tbProviders,
            this.toolStripSeparator3,
            this.tbClear,
            this.tbZoomCenter});
            this.ToolBar1.Location = new System.Drawing.Point(0, 0);
            this.ToolBar1.Name = "ToolBar1";
            this.ToolBar1.Size = new System.Drawing.Size(1101, 27);
            this.ToolBar1.TabIndex = 0;
            // 
            // tbLoadPlaces
            // 
            this.tbLoadPlaces.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbLoadPlaces.Name = "tbLoadPlaces";
            this.tbLoadPlaces.Size = new System.Drawing.Size(101, 24);
            this.tbLoadPlaces.Text = "tbLoadPlaces";
            this.tbLoadPlaces.Click += new System.EventHandler(this.tbLoadPlaces_Click);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(6, 27);
            // 
            // tbSaveSnapshot
            // 
            this.tbSaveSnapshot.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbSaveSnapshot.Name = "tbSaveSnapshot";
            this.tbSaveSnapshot.Size = new System.Drawing.Size(23, 24);
            this.tbSaveSnapshot.Click += new System.EventHandler(this.tbSaveSnapshot_Click);
            // 
            // toolStripSeparator2
            // 
            this.toolStripSeparator2.Name = "toolStripSeparator2";
            this.toolStripSeparator2.Size = new System.Drawing.Size(6, 27);
            // 
            // tbProviders
            // 
            this.tbProviders.DropDown = this.MenuProviders;
            this.tbProviders.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbProviders.Name = "tbProviders";
            this.tbProviders.Size = new System.Drawing.Size(98, 24);
            this.tbProviders.Text = "tbProviders";
            // 
            // MenuProviders
            // 
            this.MenuProviders.ImageScalingSize = new System.Drawing.Size(20, 20);
            this.MenuProviders.Name = "MenuProviders";
            this.MenuProviders.OwnerItem = this.tbProviders;
            this.MenuProviders.Size = new System.Drawing.Size(61, 4);
            // 
            // toolStripSeparator3
            // 
            this.toolStripSeparator3.Name = "toolStripSeparator3";
            this.toolStripSeparator3.Size = new System.Drawing.Size(6, 27);
            // 
            // tbClear
            // 
            this.tbClear.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbClear.Name = "tbClear";
            this.tbClear.Size = new System.Drawing.Size(61, 24);
            this.tbClear.Text = "tbClear";
            this.tbClear.Click += new System.EventHandler(this.tbClear_Click);
            // 
            // tbZoomCenter
            // 
            this.tbZoomCenter.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbZoomCenter.Name = "tbZoomCenter";
            this.tbZoomCenter.Size = new System.Drawing.Size(100, 24);
            this.tbZoomCenter.Text = "Zoom Center";
            this.tbZoomCenter.Click += new System.EventHandler(this.tbZoomCenter_Click);
            // 
            // StatusBar1
            // 
            this.StatusBar1.Location = new System.Drawing.Point(0, 549);
            this.StatusBar1.Name = "StatusBar1";
            this.StatusBar1.Size = new System.Drawing.Size(1101, 23);
            this.StatusBar1.TabIndex = 3;
            // 
            // PageControl1
            // 
            this.PageControl1.Controls.Add(this.pagePlaces);
            this.PageControl1.Controls.Add(this.pageCoordinates);
            this.PageControl1.Dock = System.Windows.Forms.DockStyle.Left;
            this.PageControl1.Location = new System.Drawing.Point(0, 27);
            this.PageControl1.Name = "PageControl1";
            this.PageControl1.SelectedIndex = 0;
            this.PageControl1.Size = new System.Drawing.Size(405, 522);
            this.PageControl1.TabIndex = 1;
            // 
            // pagePlaces
            // 
            this.pagePlaces.BackColor = System.Drawing.SystemColors.Control;
            this.pagePlaces.Controls.Add(this.tvPlaces);
            this.pagePlaces.Controls.Add(this.grpSelection);
            this.pagePlaces.Location = new System.Drawing.Point(4, 26);
            this.pagePlaces.Name = "pagePlaces";
            this.pagePlaces.Size = new System.Drawing.Size(397, 492);
            this.pagePlaces.TabIndex = 0;
            this.pagePlaces.Text = "pagePlaces";
            // 
            // tvPlaces
            // 
            this.tvPlaces.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tvPlaces.Location = new System.Drawing.Point(0, 225);
            this.tvPlaces.Name = "tvPlaces";
            this.tvPlaces.Size = new System.Drawing.Size(397, 267);
            this.tvPlaces.TabIndex = 0;
            this.tvPlaces.DoubleClick += new System.EventHandler(this.TreePlaces_DoubleClick);
            // 
            // grpSelection
            // 
            this.grpSelection.Controls.Add(this.cmbPersons);
            this.grpSelection.Controls.Add(this.chkResidence);
            this.grpSelection.Controls.Add(this.chkDeath);
            this.grpSelection.Controls.Add(this.chkBirth);
            this.grpSelection.Controls.Add(this.btnSelectPlaces);
            this.grpSelection.Controls.Add(this.radTotal);
            this.grpSelection.Controls.Add(this.radSelected);
            this.grpSelection.Controls.Add(this.chkLinesVisible);
            this.grpSelection.Dock = System.Windows.Forms.DockStyle.Top;
            this.grpSelection.Location = new System.Drawing.Point(0, 0);
            this.grpSelection.Name = "grpSelection";
            this.grpSelection.Size = new System.Drawing.Size(397, 225);
            this.grpSelection.TabIndex = 1;
            this.grpSelection.TabStop = false;
            this.grpSelection.Text = "grpSelection";
            // 
            // cmbPersons
            // 
            this.cmbPersons.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbPersons.Location = new System.Drawing.Point(11, 126);
            this.cmbPersons.Name = "cmbPersons";
            this.cmbPersons.Size = new System.Drawing.Size(371, 25);
            this.cmbPersons.TabIndex = 5;
            // 
            // chkResidence
            // 
            this.chkResidence.AutoSize = true;
            this.chkResidence.Location = new System.Drawing.Point(27, 78);
            this.chkResidence.Name = "chkResidence";
            this.chkResidence.Size = new System.Drawing.Size(113, 21);
            this.chkResidence.TabIndex = 3;
            this.chkResidence.Text = "chkResidence";
            // 
            // chkDeath
            // 
            this.chkDeath.AutoSize = true;
            this.chkDeath.Location = new System.Drawing.Point(27, 58);
            this.chkDeath.Name = "chkDeath";
            this.chkDeath.Size = new System.Drawing.Size(89, 21);
            this.chkDeath.TabIndex = 2;
            this.chkDeath.Text = "chkDeath";
            // 
            // chkBirth
            // 
            this.chkBirth.AutoSize = true;
            this.chkBirth.Location = new System.Drawing.Point(27, 39);
            this.chkBirth.Name = "chkBirth";
            this.chkBirth.Size = new System.Drawing.Size(80, 21);
            this.chkBirth.TabIndex = 1;
            this.chkBirth.Text = "chkBirth";
            // 
            // btnSelectPlaces
            // 
            this.btnSelectPlaces.Enabled = false;
            this.btnSelectPlaces.Location = new System.Drawing.Point(277, 185);
            this.btnSelectPlaces.Name = "btnSelectPlaces";
            this.btnSelectPlaces.Size = new System.Drawing.Size(105, 30);
            this.btnSelectPlaces.TabIndex = 6;
            this.btnSelectPlaces.Text = "btnSelectPlaces";
            this.btnSelectPlaces.Click += new System.EventHandler(this.btnSelectPlaces_Click);
            // 
            // radTotal
            // 
            this.radTotal.AutoSize = true;
            this.radTotal.Location = new System.Drawing.Point(11, 19);
            this.radTotal.Name = "radTotal";
            this.radTotal.Size = new System.Drawing.Size(79, 21);
            this.radTotal.TabIndex = 0;
            this.radTotal.Text = "radTotal";
            this.radTotal.Click += new System.EventHandler(this.radTotal_Click);
            // 
            // radSelected
            // 
            this.radSelected.AutoSize = true;
            this.radSelected.Location = new System.Drawing.Point(11, 106);
            this.radSelected.Name = "radSelected";
            this.radSelected.Size = new System.Drawing.Size(100, 21);
            this.radSelected.TabIndex = 4;
            this.radSelected.Text = "radSelected";
            this.radSelected.Click += new System.EventHandler(this.radTotal_Click);
            // 
            // chkLinesVisible
            // 
            this.chkLinesVisible.AutoSize = true;
            this.chkLinesVisible.Checked = true;
            this.chkLinesVisible.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chkLinesVisible.Location = new System.Drawing.Point(11, 155);
            this.chkLinesVisible.Name = "chkLinesVisible";
            this.chkLinesVisible.Size = new System.Drawing.Size(117, 21);
            this.chkLinesVisible.TabIndex = 8;
            this.chkLinesVisible.Text = "chkLinesVisible";
            // 
            // pageCoordinates
            // 
            this.pageCoordinates.BackColor = System.Drawing.SystemColors.Control;
            this.pageCoordinates.Controls.Add(this.gbCoords);
            this.pageCoordinates.Location = new System.Drawing.Point(4, 26);
            this.pageCoordinates.Name = "pageCoordinates";
            this.pageCoordinates.Padding = new System.Windows.Forms.Padding(3);
            this.pageCoordinates.Size = new System.Drawing.Size(397, 492);
            this.pageCoordinates.TabIndex = 1;
            this.pageCoordinates.Text = "pageCoordinates";
            // 
            // gbCoords
            // 
            this.gbCoords.Controls.Add(this.txtPlace);
            this.gbCoords.Controls.Add(this.btnSearch);
            this.gbCoords.Dock = System.Windows.Forms.DockStyle.Top;
            this.gbCoords.Location = new System.Drawing.Point(3, 3);
            this.gbCoords.Margin = new System.Windows.Forms.Padding(4);
            this.gbCoords.Name = "gbCoords";
            this.gbCoords.Padding = new System.Windows.Forms.Padding(4);
            this.gbCoords.Size = new System.Drawing.Size(391, 127);
            this.gbCoords.TabIndex = 28;
            this.gbCoords.TabStop = false;
            // 
            // txtPlace
            // 
            this.txtPlace.Location = new System.Drawing.Point(12, 7);
            this.txtPlace.Margin = new System.Windows.Forms.Padding(4);
            this.txtPlace.Name = "txtPlace";
            this.txtPlace.Size = new System.Drawing.Size(161, 24);
            this.txtPlace.TabIndex = 10;
            this.txtPlace.Text = "Egypt, Cairo";
            // 
            // btnSearch
            // 
            this.btnSearch.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.btnSearch.Location = new System.Drawing.Point(176, 6);
            this.btnSearch.Margin = new System.Windows.Forms.Padding(4);
            this.btnSearch.Name = "btnSearch";
            this.btnSearch.Size = new System.Drawing.Size(96, 30);
            this.btnSearch.TabIndex = 8;
            this.btnSearch.Text = "Search";
            this.btnSearch.UseVisualStyleBackColor = true;
            this.btnSearch.Click += new System.EventHandler(this.btnSearch_Click);
            // 
            // panClient
            // 
            this.panClient.ColumnCount = 2;
            this.panClient.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.panClient.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.panClient.Controls.Add(this.panZoom, 1, 0);
            this.panClient.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panClient.Location = new System.Drawing.Point(405, 27);
            this.panClient.Margin = new System.Windows.Forms.Padding(0, 0, 3, 2);
            this.panClient.Name = "panClient";
            this.panClient.RowCount = 1;
            this.panClient.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.panClient.Size = new System.Drawing.Size(696, 522);
            this.panClient.TabIndex = 0;
            // 
            // panZoom
            // 
            this.panZoom.AutoSize = true;
            this.panZoom.ColumnCount = 1;
            this.panZoom.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.panZoom.Controls.Add(this.btnZoomUp, 0, 0);
            this.panZoom.Controls.Add(this.trkZoom, 0, 1);
            this.panZoom.Controls.Add(this.btnZoomDown, 0, 2);
            this.panZoom.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panZoom.Location = new System.Drawing.Point(635, 3);
            this.panZoom.Name = "panZoom";
            this.panZoom.RowCount = 3;
            this.panZoom.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.panZoom.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.panZoom.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.panZoom.Size = new System.Drawing.Size(58, 516);
            this.panZoom.TabIndex = 31;
            // 
            // btnZoomUp
            // 
            this.btnZoomUp.Dock = System.Windows.Forms.DockStyle.Fill;
            this.btnZoomUp.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnZoomUp.Location = new System.Drawing.Point(0, 0);
            this.btnZoomUp.Margin = new System.Windows.Forms.Padding(0);
            this.btnZoomUp.Name = "btnZoomUp";
            this.btnZoomUp.Size = new System.Drawing.Size(58, 33);
            this.btnZoomUp.TabIndex = 0;
            this.btnZoomUp.Text = "+";
            this.btnZoomUp.UseVisualStyleBackColor = true;
            this.btnZoomUp.Click += new System.EventHandler(this.btnZoomUp_Click);
            // 
            // trkZoom
            // 
            this.trkZoom.BackColor = System.Drawing.SystemColors.Control;
            this.trkZoom.Dock = System.Windows.Forms.DockStyle.Fill;
            this.trkZoom.LargeChange = 1;
            this.trkZoom.Location = new System.Drawing.Point(0, 33);
            this.trkZoom.Margin = new System.Windows.Forms.Padding(0);
            this.trkZoom.Maximum = 1700;
            this.trkZoom.Minimum = 1;
            this.trkZoom.Name = "trkZoom";
            this.trkZoom.Orientation = System.Windows.Forms.Orientation.Vertical;
            this.trkZoom.Size = new System.Drawing.Size(58, 450);
            this.trkZoom.TabIndex = 29;
            this.trkZoom.TickFrequency = 100;
            this.trkZoom.TickStyle = System.Windows.Forms.TickStyle.TopLeft;
            this.trkZoom.Value = 12;
            this.trkZoom.ValueChanged += new System.EventHandler(this.trkZoom_ValueChanged);
            // 
            // btnZoomDown
            // 
            this.btnZoomDown.Dock = System.Windows.Forms.DockStyle.Fill;
            this.btnZoomDown.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnZoomDown.Location = new System.Drawing.Point(0, 483);
            this.btnZoomDown.Margin = new System.Windows.Forms.Padding(0);
            this.btnZoomDown.Name = "btnZoomDown";
            this.btnZoomDown.Size = new System.Drawing.Size(58, 33);
            this.btnZoomDown.TabIndex = 1;
            this.btnZoomDown.Text = "-";
            this.btnZoomDown.UseVisualStyleBackColor = true;
            this.btnZoomDown.Click += new System.EventHandler(this.btnZoomDown_Click);
            // 
            // MapsViewerWin
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.ClientSize = new System.Drawing.Size(1101, 572);
            this.Controls.Add(this.panClient);
            this.Controls.Add(this.PageControl1);
            this.Controls.Add(this.StatusBar1);
            this.Controls.Add(this.ToolBar1);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.Sizable;
            this.KeyPreview = true;
            this.Name = "MapsViewerWin";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "MapsViewerWin";
            this.Title = "MapsViewerWin";
            this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.MapsViewerWin_KeyDown);
            this.ToolBar1.ResumeLayout(false);
            this.ToolBar1.PerformLayout();
            this.PageControl1.ResumeLayout(false);
            this.pagePlaces.ResumeLayout(false);
            this.grpSelection.ResumeLayout(false);
            this.grpSelection.PerformLayout();
            this.pageCoordinates.ResumeLayout(false);
            this.gbCoords.ResumeLayout(false);
            this.gbCoords.PerformLayout();
            this.panClient.ResumeLayout(false);
            this.panClient.PerformLayout();
            this.panZoom.ResumeLayout(false);
            this.panZoom.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.trkZoom)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }
    }
}
