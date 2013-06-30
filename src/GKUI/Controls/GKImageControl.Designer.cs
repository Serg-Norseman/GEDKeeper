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
        	this.actualSizeToolStripButton = new System.Windows.Forms.ToolStripButton();
        	this.zoomInToolStripButton = new System.Windows.Forms.ToolStripButton();
        	this.zoomOutToolStripButton = new System.Windows.Forms.ToolStripButton();
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
        	        	        	this.actualSizeToolStripButton,
        	        	        	this.zoomInToolStripButton,
        	        	        	this.zoomOutToolStripButton,
        	        	        	this.toolStripSeparator2,
        	        	        	this.zoomLevelsToolStripComboBox,
        	        	        	this.toolStripSeparator4});
        	this.toolStrip.Location = new System.Drawing.Point(0, 0);
        	this.toolStrip.Name = "toolStrip";
        	this.toolStrip.Size = new System.Drawing.Size(708, 25);
        	this.toolStrip.TabIndex = 2;
        	// 
        	// actualSizeToolStripButton
        	// 
        	this.actualSizeToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
        	this.actualSizeToolStripButton.Image = global::GKResources.iSizeToFit;
        	this.actualSizeToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
        	this.actualSizeToolStripButton.Name = "actualSizeToolStripButton";
        	this.actualSizeToolStripButton.Size = new System.Drawing.Size(23, 22);
        	this.actualSizeToolStripButton.Text = "Actual Size";
        	this.actualSizeToolStripButton.Click += new System.EventHandler(this.actualSizeToolStripButton_Click);
        	// 
        	// zoomInToolStripButton
        	// 
        	this.zoomInToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
        	this.zoomInToolStripButton.Image = global::GKResources.iZoomIn;
        	this.zoomInToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
        	this.zoomInToolStripButton.Name = "zoomInToolStripButton";
        	this.zoomInToolStripButton.Size = new System.Drawing.Size(23, 22);
        	this.zoomInToolStripButton.Text = "Zoom In";
        	this.zoomInToolStripButton.Click += new System.EventHandler(this.zoomInToolStripButton_Click);
        	// 
        	// zoomOutToolStripButton
        	// 
        	this.zoomOutToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
        	this.zoomOutToolStripButton.Image = global::GKResources.iZoomOut;
        	this.zoomOutToolStripButton.ImageTransparentColor = System.Drawing.Color.Magenta;
        	this.zoomOutToolStripButton.Name = "zoomOutToolStripButton";
        	this.zoomOutToolStripButton.Size = new System.Drawing.Size(23, 22);
        	this.zoomOutToolStripButton.Text = "Zoom Out";
        	this.zoomOutToolStripButton.Click += new System.EventHandler(this.zoomOutToolStripButton_Click);
        	// 
        	// toolStripSeparator2
        	// 
        	this.toolStripSeparator2.Name = "toolStripSeparator2";
        	this.toolStripSeparator2.Size = new System.Drawing.Size(6, 25);
        	// 
        	// zoomLevelsToolStripComboBox
        	// 
        	this.zoomLevelsToolStripComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
        	this.zoomLevelsToolStripComboBox.Name = "zoomLevelsToolStripComboBox";
        	this.zoomLevelsToolStripComboBox.Size = new System.Drawing.Size(140, 25);
        	this.zoomLevelsToolStripComboBox.SelectedIndexChanged += new System.EventHandler(this.zoomLevelsToolStripComboBox_SelectedIndexChanged);
        	// 
        	// toolStripSeparator4
        	// 
        	this.toolStripSeparator4.Name = "toolStripSeparator4";
        	this.toolStripSeparator4.Size = new System.Drawing.Size(6, 25);
        	// 
        	// imageBox
        	// 
        	this.imageBox.BackColor = System.Drawing.SystemColors.ControlDark;
        	this.imageBox.Dock = System.Windows.Forms.DockStyle.Fill;
        	this.imageBox.GridDisplayMode = Cyotek.Windows.Forms.ImageBoxGridDisplayMode.None;
        	this.imageBox.Location = new System.Drawing.Point(0, 25);
        	this.imageBox.Name = "imageBox";
        	this.imageBox.Size = new System.Drawing.Size(708, 291);
        	this.imageBox.TabIndex = 0;
        	this.imageBox.ZoomChanged += new System.EventHandler(this.imageBox_ZoomChanged);
        	this.imageBox.ZoomLevelsChanged += new System.EventHandler(this.imageBox_ZoomLevelsChanged);
        	// 
        	// ImageControl
        	// 
        	this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
        	this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
        	this.Controls.Add(this.imageBox);
        	this.Controls.Add(this.toolStrip);
        	this.Name = "ImageControl";
        	this.Size = new System.Drawing.Size(708, 316);
        	this.toolStrip.ResumeLayout(false);
        	this.toolStrip.PerformLayout();
        	this.ResumeLayout(false);
        	this.PerformLayout();
        }

        #endregion

        private Cyotek.Windows.Forms.ImageBox imageBox;
        private System.Windows.Forms.ToolStrip toolStrip;
        private System.Windows.Forms.ToolStripButton actualSizeToolStripButton;
        private System.Windows.Forms.ToolStripButton zoomInToolStripButton;
        private System.Windows.Forms.ToolStripButton zoomOutToolStripButton;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
        private System.Windows.Forms.ToolStripComboBox zoomLevelsToolStripComboBox;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator4;
    }
}
