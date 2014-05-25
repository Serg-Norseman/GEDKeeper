namespace GKUI.Controls
{
    partial class GKImageControl
    {
        /// <summary> 
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
        	this.toolStrip = new System.Windows.Forms.ToolStrip();
        	this.btnSizeToFit = new System.Windows.Forms.ToolStripButton();
        	this.btnZoomIn = new System.Windows.Forms.ToolStripButton();
        	this.btnZoomOut = new System.Windows.Forms.ToolStripButton();
        	this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
        	this.zoomLevelsToolStripComboBox = new System.Windows.Forms.ToolStripComboBox();
        	this.toolStripSeparator4 = new System.Windows.Forms.ToolStripSeparator();
        	this.imageBox = new Cyotek.Windows.Forms.ImageBox();
        	this.toolStrip.SuspendLayout();
        	this.SuspendLayout();
        	// 
        	// toolStrip
        	// 
        	this.toolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
        	        	        	this.btnSizeToFit,
        	        	        	this.btnZoomIn,
        	        	        	this.btnZoomOut,
        	        	        	this.toolStripSeparator2,
        	        	        	this.zoomLevelsToolStripComboBox,
        	        	        	this.toolStripSeparator4});
        	this.toolStrip.Location = new System.Drawing.Point(0, 0);
        	this.toolStrip.Name = "toolStrip";
        	this.toolStrip.Size = new System.Drawing.Size(944, 28);
        	this.toolStrip.TabIndex = 2;
        	// 
        	// btnSizeToFit
        	// 
        	this.btnSizeToFit.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
        	this.btnSizeToFit.Image = global::GKResources.iSizeToFit2;
        	this.btnSizeToFit.ImageTransparentColor = System.Drawing.Color.Magenta;
        	this.btnSizeToFit.Name = "btnSizeToFit";
        	this.btnSizeToFit.Size = new System.Drawing.Size(23, 25);
        	this.btnSizeToFit.Text = "Actual Size";
        	this.btnSizeToFit.Click += new System.EventHandler(this.btnSizeToFit_Click);
        	// 
        	// btnZoomIn
        	// 
        	this.btnZoomIn.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
        	this.btnZoomIn.Image = global::GKResources.iZoomIn2;
        	this.btnZoomIn.ImageTransparentColor = System.Drawing.Color.Magenta;
        	this.btnZoomIn.Name = "btnZoomIn";
        	this.btnZoomIn.Size = new System.Drawing.Size(23, 25);
        	this.btnZoomIn.Text = "Zoom In";
        	this.btnZoomIn.Click += new System.EventHandler(this.btnZoomIn_Click);
        	// 
        	// btnZoomOut
        	// 
        	this.btnZoomOut.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
        	this.btnZoomOut.Image = global::GKResources.iZoomOut2;
        	this.btnZoomOut.ImageTransparentColor = System.Drawing.Color.Magenta;
        	this.btnZoomOut.Name = "btnZoomOut";
        	this.btnZoomOut.Size = new System.Drawing.Size(23, 25);
        	this.btnZoomOut.Text = "Zoom Out";
        	this.btnZoomOut.Click += new System.EventHandler(this.btnZoomOut_Click);
        	// 
        	// toolStripSeparator2
        	// 
        	this.toolStripSeparator2.Name = "toolStripSeparator2";
        	this.toolStripSeparator2.Size = new System.Drawing.Size(6, 28);
        	// 
        	// zoomLevelsToolStripComboBox
        	// 
        	this.zoomLevelsToolStripComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
        	this.zoomLevelsToolStripComboBox.Name = "zoomLevelsToolStripComboBox";
        	this.zoomLevelsToolStripComboBox.Size = new System.Drawing.Size(140, 28);
        	this.zoomLevelsToolStripComboBox.SelectedIndexChanged += new System.EventHandler(this.zoomLevelsToolStripComboBox_SelectedIndexChanged);
        	// 
        	// toolStripSeparator4
        	// 
        	this.toolStripSeparator4.Name = "toolStripSeparator4";
        	this.toolStripSeparator4.Size = new System.Drawing.Size(6, 28);
        	// 
        	// imageBox
        	// 
        	this.imageBox.BackColor = System.Drawing.SystemColors.ControlDark;
        	this.imageBox.Dock = System.Windows.Forms.DockStyle.Fill;
        	this.imageBox.GridDisplayMode = Cyotek.Windows.Forms.ImageBoxGridDisplayMode.None;
        	this.imageBox.Location = new System.Drawing.Point(0, 28);
        	this.imageBox.Margin = new System.Windows.Forms.Padding(4);
        	this.imageBox.Name = "imageBox";
        	this.imageBox.Size = new System.Drawing.Size(944, 361);
        	this.imageBox.TabIndex = 0;
        	this.imageBox.ZoomChanged += new System.EventHandler(this.imageBox_ZoomChanged);
        	this.imageBox.ZoomLevelsChanged += new System.EventHandler(this.imageBox_ZoomLevelsChanged);
        	// 
        	// GKImageControl
        	// 
        	this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
        	this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
        	this.Controls.Add(this.imageBox);
        	this.Controls.Add(this.toolStrip);
        	this.Margin = new System.Windows.Forms.Padding(4);
        	this.Name = "GKImageControl";
        	this.Size = new System.Drawing.Size(944, 389);
        	this.toolStrip.ResumeLayout(false);
        	this.toolStrip.PerformLayout();
        	this.ResumeLayout(false);
        	this.PerformLayout();
        }

        #endregion

        private Cyotek.Windows.Forms.ImageBox imageBox;
        private System.Windows.Forms.ToolStrip toolStrip;
        private System.Windows.Forms.ToolStripButton btnSizeToFit;
        private System.Windows.Forms.ToolStripButton btnZoomIn;
        private System.Windows.Forms.ToolStripButton btnZoomOut;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
        private System.Windows.Forms.ToolStripComboBox zoomLevelsToolStripComboBox;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator4;
    }
}
