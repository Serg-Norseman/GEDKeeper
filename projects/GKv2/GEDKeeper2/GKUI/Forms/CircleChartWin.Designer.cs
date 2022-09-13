namespace GKUI.Forms
{
    partial class CircleChartWin
    {
        private System.ComponentModel.IContainer components;
        private System.Windows.Forms.ToolStrip ToolBar1;
        private System.Windows.Forms.ToolStripButton tbImageSave;
        private System.Windows.Forms.ToolStripSeparator tbs1;
        private System.Windows.Forms.ToolStripButton tbPrev;
        private System.Windows.Forms.ToolStripButton tbNext;
        private System.Windows.Forms.ToolStripSeparator tbs2;
        private System.Windows.Forms.ToolStripButton tbDocPreview;
        private System.Windows.Forms.ToolStripButton tbDocPrint;
        private System.Windows.Forms.ToolStripSeparator tbs3;
        private System.Windows.Forms.ToolStripButton tbOptions;
        private System.Windows.Forms.ToolStripDropDownButton tbModes;
        private System.Windows.Forms.ContextMenuStrip MenuModes;
        private System.Windows.Forms.ToolStripMenuItem miFanMode;

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                this.fCircleChart.Dispose();
                if (components != null)
                    components.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.ToolBar1 = new System.Windows.Forms.ToolStrip();
            this.tbImageSave = new System.Windows.Forms.ToolStripButton();
            this.tbs1 = new System.Windows.Forms.ToolStripSeparator();
            this.tbPrev = new System.Windows.Forms.ToolStripButton();
            this.tbNext = new System.Windows.Forms.ToolStripButton();
            this.tbs2 = new System.Windows.Forms.ToolStripSeparator();
            this.tbDocPreview = new System.Windows.Forms.ToolStripButton();
            this.tbDocPrint = new System.Windows.Forms.ToolStripButton();
            this.tbs3 = new System.Windows.Forms.ToolStripSeparator();
            this.tbOptions = new System.Windows.Forms.ToolStripButton();
            this.tbModes = new System.Windows.Forms.ToolStripDropDownButton();
            this.MenuModes = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.miFanMode = new System.Windows.Forms.ToolStripMenuItem();
            this.ToolBar1.SuspendLayout();
            this.SuspendLayout();
            // 
            // ToolBar1
            // 
            this.ToolBar1.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.ToolBar1.ImageScalingSize = new System.Drawing.Size(20, 20);
            this.ToolBar1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
                                             this.tbImageSave,
                                             this.tbs1,
                                             this.tbPrev,
                                             this.tbNext,
                                             this.tbs2,
                                             this.tbDocPreview,
                                             this.tbDocPrint,
                                             this.tbs3,
                                             this.tbModes,
                                             this.tbOptions});
            this.ToolBar1.Location = new System.Drawing.Point(0, 0);
            this.ToolBar1.Name = "ToolBar1";
            this.ToolBar1.Size = new System.Drawing.Size(1093, 25);
            this.ToolBar1.TabIndex = 1;
            // 
            // tbImageSave
            // 
            this.tbImageSave.Name = "tbImageSave";
            this.tbImageSave.Size = new System.Drawing.Size(23, 22);
            this.tbImageSave.Click += new System.EventHandler(this.tbImageSave_Click);
            // 
            // tbs1
            // 
            this.tbs1.Name = "tbs1";
            this.tbs1.Size = new System.Drawing.Size(6, 25);
            // 
            // tbPrev
            // 
            this.tbPrev.Enabled = false;
            this.tbPrev.Name = "tbPrev";
            this.tbPrev.Size = new System.Drawing.Size(23, 24);
            this.tbPrev.Click += new System.EventHandler(this.ToolBar1_ButtonClick);
            // 
            // tbNext
            // 
            this.tbNext.Enabled = false;
            this.tbNext.Name = "tbNext";
            this.tbNext.Size = new System.Drawing.Size(23, 24);
            this.tbNext.Click += new System.EventHandler(this.ToolBar1_ButtonClick);
            // 
            // tbs2
            // 
            this.tbs2.Name = "tbs2";
            this.tbs2.Size = new System.Drawing.Size(6, 27);
            // 
            // tbDocPreview
            // 
            this.tbDocPreview.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.tbDocPreview.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbDocPreview.Name = "tbDocPreview";
            this.tbDocPreview.Size = new System.Drawing.Size(23, 22);
            this.tbDocPreview.Text = "toolStripButton1";
            this.tbDocPreview.Click += new System.EventHandler(this.tbDocPreview_Click);
            // 
            // tbDocPrint
            // 
            this.tbDocPrint.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.tbDocPrint.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbDocPrint.Name = "tbDocPrint";
            this.tbDocPrint.Size = new System.Drawing.Size(23, 22);
            this.tbDocPrint.Text = "toolStripButton2";
            this.tbDocPrint.Click += new System.EventHandler(this.tbDocPrint_Click);
            // 
            // tbs3
            // 
            this.tbs3.Name = "tbs3";
            this.tbs3.Size = new System.Drawing.Size(6, 27);
            // 
            // tbOptions
            // 
            this.tbOptions.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.tbOptions.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbOptions.Name = "tbOptions";
            this.tbOptions.Size = new System.Drawing.Size(23, 22);
            this.tbOptions.Text = "tbOptions";
            this.tbOptions.Click += new System.EventHandler(this.tbOptions_Click);
            // 
            // tbModes
            // 
            this.tbModes.DropDown = this.MenuModes;
            this.tbModes.Name = "tbModes";
            this.tbModes.Size = new System.Drawing.Size(13, 22);
            // 
            // MenuModes
            // 
            this.MenuModes.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.miFanMode});
            this.MenuModes.Name = "MenuModes";
            this.MenuModes.OwnerItem = this.tbModes;
            this.MenuModes.Size = new System.Drawing.Size(187, 198);
            // 
            // miFanMode
            //
            this.miFanMode.Name = "miFanMode";
            this.miFanMode.Size = new System.Drawing.Size(186, 22);
            this.miFanMode.Text = "miFanMode";
            this.miFanMode.Click += new System.EventHandler(this.miFanMode_Click);
            // 
            // CircleChartWin
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.ClientSize = new System.Drawing.Size(1093, 579);
            this.Controls.Add(this.ToolBar1);
            this.KeyPreview = true;
            this.Margin = new System.Windows.Forms.Padding(4);
            this.Name = "CircleChartWin";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "CircleChartWin";
            this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.CircleChartWin_KeyDown);
            this.ToolBar1.ResumeLayout(false);
            this.ToolBar1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();
        }
    }
}
