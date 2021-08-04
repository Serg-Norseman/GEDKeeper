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
            this.tableLayoutPanel5.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.SuspendLayout();
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
            this.Controls.Add(this.MapBrowser);
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
