namespace GKLifePlugin.ConwayLife
{
    partial class LifeForm
    {
        private System.ComponentModel.IContainer components = null;
        private System.Windows.Forms.ToolStrip ToolBar1;
        private System.Windows.Forms.StatusStrip barStatusLine;
        private System.Windows.Forms.ToolStripButton tbStep;
        private System.Windows.Forms.Timer tmrNextGeneration;
        private System.Windows.Forms.ToolStripButton tbStart;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private System.Windows.Forms.ToolStripButton btnSetCells;
        private System.Windows.Forms.ToolStripButton tbClear;
        private System.Windows.Forms.ToolStripButton tbRandomise;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
        private System.Windows.Forms.ToolStripButton tbOptions;
        private ConwayLife.LifeViewer cmpLife;
        private System.Windows.Forms.ToolStripStatusLabel stlGeneration;
        private System.Windows.Forms.ToolStripStatusLabel stlLivingCells;

        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null)) {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.ToolBar1 = new System.Windows.Forms.ToolStrip();
            this.tbStep = new System.Windows.Forms.ToolStripButton();
            this.tbStart = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.btnSetCells = new System.Windows.Forms.ToolStripButton();
            this.tbClear = new System.Windows.Forms.ToolStripButton();
            this.tbRandomise = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
            this.tbOptions = new System.Windows.Forms.ToolStripButton();
            this.barStatusLine = new System.Windows.Forms.StatusStrip();
            this.stlGeneration = new System.Windows.Forms.ToolStripStatusLabel();
            this.stlLivingCells = new System.Windows.Forms.ToolStripStatusLabel();
            this.tmrNextGeneration = new System.Windows.Forms.Timer(this.components);
            this.cmpLife = new ConwayLife.LifeViewer();
            this.ToolBar1.SuspendLayout();
            this.barStatusLine.SuspendLayout();
            this.SuspendLayout();
            // 
            // ToolBar1
            // 
            this.ToolBar1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
                                             this.tbStep,
                                             this.tbStart,
                                             this.toolStripSeparator1,
                                             this.btnSetCells,
                                             this.tbClear,
                                             this.tbRandomise,
                                             this.toolStripSeparator2,
                                             this.tbOptions});
            this.ToolBar1.Location = new System.Drawing.Point(0, 0);
            this.ToolBar1.Name = "ToolBar1";
            this.ToolBar1.Size = new System.Drawing.Size(923, 27);
            this.ToolBar1.TabIndex = 0;
            this.ToolBar1.Text = "toolStrip1";
            // 
            // tbStep
            // 
            this.tbStep.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.tbStep.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbStep.Name = "tbStep";
            this.tbStep.Size = new System.Drawing.Size(43, 24);
            this.tbStep.Text = "Step";
            this.tbStep.Click += new System.EventHandler(this.tbStep_Click);
            // 
            // tbStart
            // 
            this.tbStart.CheckOnClick = true;
            this.tbStart.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.tbStart.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbStart.Name = "tbStart";
            this.tbStart.Size = new System.Drawing.Size(44, 24);
            this.tbStart.Text = "Start";
            this.tbStart.Click += new System.EventHandler(this.tbStart_Click);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(6, 27);
            // 
            // btnSetCells
            // 
            this.btnSetCells.CheckOnClick = true;
            this.btnSetCells.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.btnSetCells.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.btnSetCells.Name = "btnSetCells";
            this.btnSetCells.Size = new System.Drawing.Size(65, 24);
            this.btnSetCells.Text = "SetCells";
            this.btnSetCells.Click += new System.EventHandler(this.tbSetCells_Click);
            // 
            // tbClear
            // 
            this.tbClear.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.tbClear.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbClear.Name = "tbClear";
            this.tbClear.Size = new System.Drawing.Size(47, 24);
            this.tbClear.Text = "Clear";
            this.tbClear.Click += new System.EventHandler(this.tbClear_Click);
            // 
            // tbRandomise
            // 
            this.tbRandomise.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.tbRandomise.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbRandomise.Name = "tbRandomise";
            this.tbRandomise.Size = new System.Drawing.Size(87, 24);
            this.tbRandomise.Text = "Randomise";
            this.tbRandomise.Click += new System.EventHandler(this.tbRandomise_Click);
            // 
            // toolStripSeparator2
            // 
            this.toolStripSeparator2.Name = "toolStripSeparator2";
            this.toolStripSeparator2.Size = new System.Drawing.Size(6, 27);
            // 
            // tbOptions
            // 
            this.tbOptions.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
            this.tbOptions.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbOptions.Name = "tbOptions";
            this.tbOptions.Size = new System.Drawing.Size(65, 24);
            this.tbOptions.Text = "Options";
            this.tbOptions.Click += new System.EventHandler(this.tbOptions_Click);
            // 
            // barStatusLine
            // 
            this.barStatusLine.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
                                                  this.stlGeneration,
                                                  this.stlLivingCells});
            this.barStatusLine.Location = new System.Drawing.Point(0, 351);
            this.barStatusLine.Name = "barStatusLine";
            this.barStatusLine.Padding = new System.Windows.Forms.Padding(1, 0, 19, 0);
            this.barStatusLine.Size = new System.Drawing.Size(923, 22);
            this.barStatusLine.TabIndex = 1;
            this.barStatusLine.Text = "statusStrip1";
            // 
            // stlGeneration
            // 
            this.stlGeneration.Name = "stlGeneration";
            this.stlGeneration.Size = new System.Drawing.Size(0, 17);
            // 
            // stlLivingCells
            // 
            this.stlLivingCells.Name = "stlLivingCells";
            this.stlLivingCells.Size = new System.Drawing.Size(0, 17);
            // 
            // tmrNextGeneration
            // 
            this.tmrNextGeneration.Tick += new System.EventHandler(this.tmrNextGeneration_Tick);
            // 
            // cmpLife
            // 
            this.cmpLife.Dock = System.Windows.Forms.DockStyle.Fill;
            this.cmpLife.Location = new System.Drawing.Point(0, 27);
            this.cmpLife.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.cmpLife.Name = "cmpLife";
            this.cmpLife.Size = new System.Drawing.Size(923, 324);
            this.cmpLife.TabIndex = 2;
            // 
            // LifeForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(923, 373);
            this.Controls.Add(this.cmpLife);
            this.Controls.Add(this.barStatusLine);
            this.Controls.Add(this.ToolBar1);
            this.Margin = new System.Windows.Forms.Padding(4, 4, 4, 4);
            this.MinimizeBox = false;
            this.Name = "LifeForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "frmP1Main";
            this.Load += new System.EventHandler(this.PluginForm_Load);
            this.Resize += new System.EventHandler(this.PluginFormResize);
            this.ToolBar1.ResumeLayout(false);
            this.ToolBar1.PerformLayout();
            this.barStatusLine.ResumeLayout(false);
            this.barStatusLine.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();
        }
    }
}
