namespace GKWordsCloudPlugin
{
    partial class WordsCloudWidget
    {
        private GKWordsCloudPlugin.WordsCloud.CloudViewer cloudViewer;
        private System.Windows.Forms.ToolStrip ToolBar1;
        private System.Windows.Forms.ToolStripSeparator TBS1;
        private System.Windows.Forms.ToolStripComboBox cbType;

        private void InitializeComponent()
        {
            this.ToolBar1 = new System.Windows.Forms.ToolStrip();
            this.TBS1 = new System.Windows.Forms.ToolStripSeparator();
            this.cbType = new System.Windows.Forms.ToolStripComboBox();
            this.cloudViewer = new GKWordsCloudPlugin.WordsCloud.CloudViewer();
            this.ToolBar1.SuspendLayout();
            this.SuspendLayout();
            // 
            // ToolBar1
            // 
            this.ToolBar1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
                this.TBS1,
                this.cbType});
            this.ToolBar1.Location = new System.Drawing.Point(10, 10);
            this.ToolBar1.Name = "ToolBar1";
            this.ToolBar1.Size = new System.Drawing.Size(780, 25);
            this.ToolBar1.TabIndex = 2;
            // 
            // TBS1
            // 
            this.TBS1.Name = "TBS1";
            this.TBS1.Size = new System.Drawing.Size(6, 25);
            // 
            // cbType
            // 
            this.cbType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbType.Name = "cbType";
            this.cbType.Size = new System.Drawing.Size(262, 25);
            this.cbType.SelectedIndexChanged += new System.EventHandler(this.cbType_SelectedIndexChanged);
            // 
            // cloudViewer
            // 
            this.cloudViewer.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.cloudViewer.Dock = System.Windows.Forms.DockStyle.Fill;
            this.cloudViewer.Location = new System.Drawing.Point(10, 35);
            this.cloudViewer.MaxFontSize = 40;
            this.cloudViewer.MinFontSize = 4;
            this.cloudViewer.Name = "cloudViewer";
            this.cloudViewer.Size = new System.Drawing.Size(780, 555);
            this.cloudViewer.TabIndex = 3;
            this.cloudViewer.WeightedWords = null;
            // 
            // WordsCloudWidget
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(800, 600);
            this.Controls.Add(this.cloudViewer);
            this.Controls.Add(this.ToolBar1);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
            this.Name = "WordsCloudWidget";
            this.Padding = new System.Windows.Forms.Padding(10);
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "WordsCloudWidget";
            this.TopMost = true;
            this.Closed += new System.EventHandler(this.Form_Closed);
            this.Load += new System.EventHandler(this.Form_Load);
            this.ToolBar1.ResumeLayout(false);
            this.ToolBar1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();
        }
    }
}
