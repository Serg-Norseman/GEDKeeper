namespace GKMap.WinForms.Demo
{
    partial class MainForm
    {
        private GKMap.WinForms.Demo.GMapBrowser MapBrowser;
        private System.Windows.Forms.TableLayoutPanel tableLayoutPanel5;
        private System.Windows.Forms.Button btnAddMarker;
        private System.Windows.Forms.GroupBox groupBox3;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.TextBox textBoxGeo;
        private System.Windows.Forms.Button btnGoto;
        private System.Windows.Forms.Label lblLng;
        private System.Windows.Forms.Label lblLat;
        private System.Windows.Forms.TextBox txtLng;
        private System.Windows.Forms.TextBox txtLat;
        private System.ComponentModel.IContainer components = null;
        private System.Windows.Forms.Button btnSaveView;
        private System.Windows.Forms.Button btnZoomCenter;
        private System.Windows.Forms.Button btnClearAll;
        private System.Windows.Forms.TableLayoutPanel panTools;
        private System.Windows.Forms.TableLayoutPanel panClient;
        private System.Windows.Forms.ComboBox cmbMapType;
        private System.Windows.Forms.Label lblMapType;
        private System.Windows.Forms.TrackBar trkZoom;
        private System.Windows.Forms.Button btnZoomUp;
        private System.Windows.Forms.Button btnZoomDown;
        private System.Windows.Forms.TableLayoutPanel panZoom;

        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null)) {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle1 = new System.Windows.Forms.DataGridViewCellStyle();
            this.MapBrowser = new GKMap.WinForms.Demo.GMapBrowser();
            this.tableLayoutPanel5 = new System.Windows.Forms.TableLayoutPanel();
            this.btnAddMarker = new System.Windows.Forms.Button();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.label6 = new System.Windows.Forms.Label();
            this.textBoxGeo = new System.Windows.Forms.TextBox();
            this.btnGoto = new System.Windows.Forms.Button();
            this.lblLng = new System.Windows.Forms.Label();
            this.lblLat = new System.Windows.Forms.Label();
            this.txtLng = new System.Windows.Forms.TextBox();
            this.txtLat = new System.Windows.Forms.TextBox();
            this.btnSaveView = new System.Windows.Forms.Button();
            this.btnZoomCenter = new System.Windows.Forms.Button();
            this.btnClearAll = new System.Windows.Forms.Button();
            this.lblMapType = new System.Windows.Forms.Label();
            this.cmbMapType = new System.Windows.Forms.ComboBox();
            this.panTools = new System.Windows.Forms.TableLayoutPanel();
            this.btnZoomUp = new System.Windows.Forms.Button();
            this.trkZoom = new System.Windows.Forms.TrackBar();
            this.btnZoomDown = new System.Windows.Forms.Button();
            this.panZoom = new System.Windows.Forms.TableLayoutPanel();
            this.panClient = new System.Windows.Forms.TableLayoutPanel();
            this.tableLayoutPanel5.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.SuspendLayout();

            this.lblMapType.AutoSize = true;
            this.lblMapType.Location = new System.Drawing.Point(176, 27);
            this.lblMapType.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.lblMapType.Size = new System.Drawing.Size(35, 17);
            this.lblMapType.Text = "type";

            this.cmbMapType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbMapType.FormattingEnabled = true;
            this.cmbMapType.Location = new System.Drawing.Point(11, 23);
            this.cmbMapType.Margin = new System.Windows.Forms.Padding(4);
            this.cmbMapType.Size = new System.Drawing.Size(163, 24);
            this.cmbMapType.TabIndex = 9;
            this.cmbMapType.DropDownClosed += cmbMapType_DropDownClosed;

            this.panTools.AutoSize = true;
            this.panTools.ColumnCount = 5;
            this.panTools.Controls.Add(cmbMapType, 1, 0);
            this.panTools.Controls.Add(lblMapType, 0, 0);
            this.panTools.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panTools.Location = new System.Drawing.Point(64, 0);
            this.panTools.Margin = new System.Windows.Forms.Padding(0, 0, 3, 2);
            this.panTools.RowCount = 5;
            this.panTools.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.panTools.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.panTools.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.panTools.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.panTools.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 7F));
            this.panTools.Size = new System.Drawing.Size(235, 687);
            this.panTools.TabIndex = 30;

            this.btnZoomUp.Dock = System.Windows.Forms.DockStyle.Fill;
            this.btnZoomUp.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, 0);
            this.btnZoomUp.Location = new System.Drawing.Point(0, 0);
            this.btnZoomUp.Margin = new System.Windows.Forms.Padding(0);
            this.btnZoomUp.Size = new System.Drawing.Size(58, 33);
            this.btnZoomUp.TabIndex = 0;
            this.btnZoomUp.Text = "+";
            this.btnZoomUp.UseVisualStyleBackColor = true;
            this.btnZoomUp.Click += btnZoomUp_Click;

            this.trkZoom.BackColor = System.Drawing.Color.AliceBlue;
            this.trkZoom.Dock = System.Windows.Forms.DockStyle.Fill;
            this.trkZoom.LargeChange = 1;
            this.trkZoom.Location = new System.Drawing.Point(0, 33);
            this.trkZoom.Margin = new System.Windows.Forms.Padding(0);
            this.trkZoom.Maximum = 1700;
            this.trkZoom.Minimum = 1;
            this.trkZoom.Orientation = System.Windows.Forms.Orientation.Vertical;
            this.trkZoom.Size = new System.Drawing.Size(58, 617);
            this.trkZoom.TabIndex = 29;
            this.trkZoom.TickFrequency = 100;
            this.trkZoom.TickStyle = System.Windows.Forms.TickStyle.TopLeft;
            this.trkZoom.Value = 12;
            this.trkZoom.ValueChanged += trkZoom_ValueChanged;

            this.btnZoomDown.Dock = System.Windows.Forms.DockStyle.Fill;
            this.btnZoomDown.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, 0);
            this.btnZoomDown.Location = new System.Drawing.Point(0, 650);
            this.btnZoomDown.Margin = new System.Windows.Forms.Padding(0);
            this.btnZoomDown.Size = new System.Drawing.Size(58, 33);
            this.btnZoomDown.TabIndex = 1;
            this.btnZoomDown.Text = "-";
            this.btnZoomDown.UseVisualStyleBackColor = true;
            this.btnZoomDown.Click += btnZoomDown_Click;
            // 
            // panZoom
            // 
            this.panZoom.ColumnCount = 1;
            this.panZoom.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.panZoom.Controls.Add(btnZoomUp, 0, 0);
            this.panZoom.Controls.Add(trkZoom, 0, 1);
            this.panZoom.Controls.Add(btnZoomDown, 0, 2);
            this.panZoom.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panZoom.Location = new System.Drawing.Point(3, 3);
            this.panZoom.RowCount = 3;
            this.panZoom.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.panZoom.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.panZoom.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.panZoom.Size = new System.Drawing.Size(58, 683);
            this.panZoom.TabIndex = 31;
            // 
            // panClient
            // 
            this.panClient.ColumnCount = 2;
            this.panClient.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.panClient.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.AutoSize));
            this.panClient.Controls.Add(this.panZoom, 1, 1);
            this.panClient.Controls.Add(this.MapBrowser, 0, 1);
            this.panClient.Controls.Add(this.panTools, 0, 0);
            this.panClient.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panClient.Location = new System.Drawing.Point(0, 0);
            this.panClient.Margin = new System.Windows.Forms.Padding(0, 0, 3, 2);
            this.panClient.RowCount = 2;
            this.panClient.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.AutoSize));
            this.panClient.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.panClient.Size = new System.Drawing.Size(235, 687);
            // 
            // MapBrowser
            // 
            this.MapBrowser.Dock = System.Windows.Forms.DockStyle.Fill;
            this.MapBrowser.Location = new System.Drawing.Point(0, 0);
            this.MapBrowser.Margin = new System.Windows.Forms.Padding(4);
            this.MapBrowser.Name = "MapBrowser";
            this.MapBrowser.Size = new System.Drawing.Size(881, 818);
            this.MapBrowser.TabIndex = 0;
            // 
            // tableLayoutPanel5
            // 
            this.tableLayoutPanel5.ColumnCount = 1;
            this.tableLayoutPanel5.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50F));
            this.tableLayoutPanel5.Controls.Add(this.groupBox3, 0, 0);
            this.tableLayoutPanel5.Controls.Add(this.btnClearAll, 0, 1);
            this.tableLayoutPanel5.Controls.Add(this.btnZoomCenter, 0, 2);
            this.tableLayoutPanel5.Controls.Add(this.btnSaveView, 0, 3);
            this.tableLayoutPanel5.Dock = System.Windows.Forms.DockStyle.Right;
            this.tableLayoutPanel5.Location = new System.Drawing.Point(64, 0);
            this.tableLayoutPanel5.Margin = new System.Windows.Forms.Padding(0, 0, 3, 2);
            this.tableLayoutPanel5.Name = "tableLayoutPanel5";
            this.tableLayoutPanel5.RowCount = 5;
            this.tableLayoutPanel5.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel5.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel5.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel5.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.tableLayoutPanel5.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 7F));
            this.tableLayoutPanel5.Size = new System.Drawing.Size(235, 687);
            this.tableLayoutPanel5.TabIndex = 30;
            // 
            // btnSaveView
            // 
            this.btnSaveView.Location = new System.Drawing.Point(123, 144);
            this.btnSaveView.Margin = new System.Windows.Forms.Padding(3, 2, 3, 2);
            this.btnSaveView.Size = new System.Drawing.Size(92, 30);
            this.btnSaveView.TabIndex = 39;
            this.btnSaveView.Text = "Save View";
            this.btnSaveView.UseVisualStyleBackColor = true;
            this.btnSaveView.Click += btnSaveView_Click;
            // 
            // btnZoomCenter
            // 
            this.btnZoomCenter.Location = new System.Drawing.Point(8, 55);
            this.btnZoomCenter.Margin = new System.Windows.Forms.Padding(4);
            this.btnZoomCenter.Size = new System.Drawing.Size(109, 30);
            this.btnZoomCenter.TabIndex = 15;
            this.btnZoomCenter.Text = "Zoom Center";
            this.btnZoomCenter.UseVisualStyleBackColor = true;
            this.btnZoomCenter.Click += btnZoomCenter_Click;
            // 
            // btnClearAll
            // 
            this.btnClearAll.Location = new System.Drawing.Point(125, 55);
            this.btnClearAll.Margin = new System.Windows.Forms.Padding(4);
            this.btnClearAll.Size = new System.Drawing.Size(84, 30);
            this.btnClearAll.TabIndex = 13;
            this.btnClearAll.Text = "Clear All";
            this.btnClearAll.UseVisualStyleBackColor = true;
            this.btnClearAll.Click += btnClearAll_Click;
            // 
            // btnAddMarker
            // 
            this.btnAddMarker.Location = new System.Drawing.Point(112, 121);
            this.btnAddMarker.Margin = new System.Windows.Forms.Padding(4);
            this.btnAddMarker.Name = "btnAddMarker";
            this.btnAddMarker.Size = new System.Drawing.Size(109, 30);
            this.btnAddMarker.TabIndex = 12;
            this.btnAddMarker.Text = "Add Marker";
            this.btnAddMarker.UseVisualStyleBackColor = true;
            this.btnAddMarker.Click += new System.EventHandler(this.btnAddMarker_Click);
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.btnAddMarker);
            this.groupBox3.Controls.Add(this.label6);
            this.groupBox3.Controls.Add(this.textBoxGeo);
            this.groupBox3.Controls.Add(this.btnGoto);
            this.groupBox3.Controls.Add(this.lblLng);
            this.groupBox3.Controls.Add(this.lblLat);
            this.groupBox3.Controls.Add(this.txtLng);
            this.groupBox3.Controls.Add(this.txtLat);
            this.groupBox3.Dock = System.Windows.Forms.DockStyle.Fill;
            this.groupBox3.Location = new System.Drawing.Point(4, 4);
            this.groupBox3.Margin = new System.Windows.Forms.Padding(4);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Padding = new System.Windows.Forms.Padding(4);
            this.groupBox3.Size = new System.Drawing.Size(227, 158);
            this.groupBox3.TabIndex = 28;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "coordinates";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(176, 91);
            this.label6.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(36, 17);
            this.label6.TabIndex = 11;
            this.label6.Text = "goto";
            // 
            // textBoxGeo
            // 
            this.textBoxGeo.Location = new System.Drawing.Point(12, 87);
            this.textBoxGeo.Margin = new System.Windows.Forms.Padding(4);
            this.textBoxGeo.Name = "textBoxGeo";
            this.textBoxGeo.Size = new System.Drawing.Size(161, 22);
            this.textBoxGeo.TabIndex = 10;
            this.textBoxGeo.Text = "Egypt, Cairo";
            this.textBoxGeo.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.textBoxGeo_KeyPress);
            // 
            // btnGoto
            // 
            this.btnGoto.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.btnGoto.Location = new System.Drawing.Point(12, 121);
            this.btnGoto.Margin = new System.Windows.Forms.Padding(4);
            this.btnGoto.Name = "btnGoto";
            this.btnGoto.Size = new System.Drawing.Size(96, 30);
            this.btnGoto.TabIndex = 8;
            this.btnGoto.Text = "GoTo !";
            this.btnGoto.UseVisualStyleBackColor = true;
            this.btnGoto.Click += new System.EventHandler(this.btnGoto_Click);
            // 
            // lblLng
            // 
            this.lblLng.AutoSize = true;
            this.lblLng.Location = new System.Drawing.Point(176, 59);
            this.lblLng.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.lblLng.Name = "lblLng";
            this.lblLng.Size = new System.Drawing.Size(27, 17);
            this.lblLng.TabIndex = 3;
            this.lblLng.Text = "lng";
            // 
            // lblLat
            // 
            this.lblLat.AutoSize = true;
            this.lblLat.Location = new System.Drawing.Point(176, 27);
            this.lblLat.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
            this.lblLat.Name = "lblLat";
            this.lblLat.Size = new System.Drawing.Size(23, 17);
            this.lblLat.TabIndex = 2;
            this.lblLat.Text = "lat";
            // 
            // txtLng
            // 
            this.txtLng.Location = new System.Drawing.Point(12, 55);
            this.txtLng.Margin = new System.Windows.Forms.Padding(4);
            this.txtLng.Name = "txtLng";
            this.txtLng.Size = new System.Drawing.Size(161, 22);
            this.txtLng.TabIndex = 1;
            this.txtLng.Text = "31.2361907958984";
            // 
            // txtLat
            // 
            this.txtLat.Location = new System.Drawing.Point(12, 23);
            this.txtLat.Margin = new System.Windows.Forms.Padding(4);
            this.txtLat.Name = "txtLat";
            this.txtLat.Size = new System.Drawing.Size(161, 22);
            this.txtLat.TabIndex = 0;
            this.txtLat.Text = "30.0447272077905";
            // 
            // MainForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.Color.AliceBlue;
            this.ClientSize = new System.Drawing.Size(1188, 818);
            this.Controls.Add(this.panClient);
            this.Controls.Add(this.tableLayoutPanel5);
            this.KeyPreview = true;
            this.Margin = new System.Windows.Forms.Padding(4);
            this.MinimumSize = new System.Drawing.Size(733, 121);
            this.Name = "MainForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "GKMap - Maps for GEDKeeper (WinForms)";
            this.WindowState = System.Windows.Forms.FormWindowState.Maximized;
            this.Load += new System.EventHandler(this.MainForm_Load);
            this.tableLayoutPanel5.ResumeLayout(false);
            this.groupBox3.ResumeLayout(false);
            this.groupBox3.PerformLayout();
            this.ResumeLayout(false);
        }
    }
}
