namespace GKUI.Forms
{
    partial class TTFamilyGroupsDlg
    {
        private GKUI.Components.GKTabControl tabsTools;
        private System.Windows.Forms.TabPage pageFamilyGroups;
        private System.Windows.Forms.TreeView tvGroups;
        private GKUI.Components.LogChart gkLogChart1;
        private System.Windows.Forms.Button btnAnalyseGroups;
        private System.ComponentModel.IContainer components;
        private System.Windows.Forms.ToolStripMenuItem miDetails;
        private System.Windows.Forms.ToolStripMenuItem miGoToRecord;
        private System.Windows.Forms.ContextMenuStrip menuGT;
        private System.Windows.Forms.ToolStripSeparator N1;
        private System.Windows.Forms.ToolStripMenuItem miCopyXRef;
        private System.Windows.Forms.TabPage pageDataQuality;
        private System.Windows.Forms.ContextMenuStrip menuDQ;
        private System.Windows.Forms.ToolStripMenuItem miDQRefresh;
        private System.Windows.Forms.ToolStripMenuItem miDQResetFilter;

        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.tabsTools = new GKUI.Components.GKTabControl();
            this.pageFamilyGroups = new System.Windows.Forms.TabPage();
            this.btnAnalyseGroups = new System.Windows.Forms.Button();
            this.gkLogChart1 = new GKUI.Components.LogChart();
            this.tvGroups = new System.Windows.Forms.TreeView();
            this.menuGT = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.miDetails = new System.Windows.Forms.ToolStripMenuItem();
            this.miGoToRecord = new System.Windows.Forms.ToolStripMenuItem();
            this.N1 = new System.Windows.Forms.ToolStripSeparator();
            this.miCopyXRef = new System.Windows.Forms.ToolStripMenuItem();
            this.pageDataQuality = new System.Windows.Forms.TabPage();
            this.menuDQ = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.miDQRefresh = new System.Windows.Forms.ToolStripMenuItem();
            this.miDQResetFilter = new System.Windows.Forms.ToolStripMenuItem();
            this.tabsTools.SuspendLayout();
            this.pageFamilyGroups.SuspendLayout();
            this.pageDataQuality.SuspendLayout();
            this.menuGT.SuspendLayout();
            this.menuDQ.SuspendLayout();
            this.SuspendLayout();
            // 
            // tabsTools
            // 
            this.tabsTools.Controls.Add(this.pageFamilyGroups);
            this.tabsTools.Controls.Add(this.pageDataQuality);
            this.tabsTools.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.tabsTools.Location = new System.Drawing.Point(9, 8);
            this.tabsTools.Margin = new System.Windows.Forms.Padding(2);
            this.tabsTools.Name = "tabsTools";
            this.tabsTools.SelectedIndex = 0;
            this.tabsTools.Size = new System.Drawing.Size(808, 436);
            this.tabsTools.TabIndex = 0;
            // 
            // pageFamilyGroups
            // 
            this.pageFamilyGroups.Controls.Add(this.btnAnalyseGroups);
            this.pageFamilyGroups.Controls.Add(this.gkLogChart1);
            this.pageFamilyGroups.Controls.Add(this.tvGroups);
            this.pageFamilyGroups.Location = new System.Drawing.Point(4, 22);
            this.pageFamilyGroups.Margin = new System.Windows.Forms.Padding(2);
            this.pageFamilyGroups.Name = "pageFamilyGroups";
            this.pageFamilyGroups.Size = new System.Drawing.Size(800, 410);
            this.pageFamilyGroups.TabIndex = 5;
            this.pageFamilyGroups.Text = "pageFamilyGroups";
            // 
            // pageDataQuality
            // 
            this.pageDataQuality.Location = new System.Drawing.Point(4, 22);
            this.pageDataQuality.Margin = new System.Windows.Forms.Padding(2);
            this.pageDataQuality.Name = "pageDataQuality";
            this.pageDataQuality.Size = new System.Drawing.Size(800, 410);
            this.pageDataQuality.TabIndex = 5;
            this.pageDataQuality.Text = "pageDataQuality";
            // 
            // btnAnalyseGroups
            //
            this.btnAnalyseGroups.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.btnAnalyseGroups.Location = new System.Drawing.Point(9, 372);
            this.btnAnalyseGroups.Margin = new System.Windows.Forms.Padding(2);
            this.btnAnalyseGroups.Name = "btnAnalyseGroups";
            this.btnAnalyseGroups.Size = new System.Drawing.Size(100, 24);
            this.btnAnalyseGroups.TabIndex = 7;
            this.btnAnalyseGroups.Text = "btnAnalyseGroups";
            this.btnAnalyseGroups.Click += new System.EventHandler(this.btnAnalyseGroups_Click);
            // 
            // gkLogChart1
            //
            this.gkLogChart1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.gkLogChart1.Location = new System.Drawing.Point(125, 372);
            this.gkLogChart1.Margin = new System.Windows.Forms.Padding(2);
            this.gkLogChart1.Name = "gkLogChart1";
            this.gkLogChart1.Size = new System.Drawing.Size(666, 24);
            this.gkLogChart1.TabIndex = 1;
            this.gkLogChart1.TabStop = true;
            // 
            // tvGroups
            //
            this.tvGroups.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right)));
            this.tvGroups.ContextMenuStrip = this.menuGT;
            this.tvGroups.Location = new System.Drawing.Point(9, 8);
            this.tvGroups.Margin = new System.Windows.Forms.Padding(2);
            this.tvGroups.Name = "tvGroups";
            this.tvGroups.Size = new System.Drawing.Size(782, 350);
            this.tvGroups.TabIndex = 0;
            this.tvGroups.DoubleClick += new System.EventHandler(this.tvGroups_DoubleClick);
            // 
            // menuGT
            // 
            this.menuGT.ImageScalingSize = new System.Drawing.Size(20, 20);
            this.menuGT.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.miDetails,
            this.miGoToRecord,
            this.N1,
            this.miCopyXRef});
            this.menuGT.Name = "menuGT";
            this.menuGT.Size = new System.Drawing.Size(153, 76);
            this.menuGT.Opening += new System.ComponentModel.CancelEventHandler(this.contextMenu_Opening);
            // 
            // menuDQ
            // 
            this.menuDQ.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
                this.miDQRefresh,
                new System.Windows.Forms.ToolStripSeparator(),
                this.miDQResetFilter
            });
            // 
            // miDetails
            // 
            this.miDetails.Name = "miDetails";
            this.miDetails.Size = new System.Drawing.Size(152, 22);
            this.miDetails.Text = "miDetails";
            this.miDetails.Click += new System.EventHandler(this.miDetails_Click);
            // 
            // miGoToRecord
            // 
            this.miGoToRecord.Name = "miGoToRecord";
            this.miGoToRecord.Size = new System.Drawing.Size(152, 22);
            this.miGoToRecord.Text = "miGoToRecord";
            this.miGoToRecord.Click += new System.EventHandler(this.miGoToRecord_Click);
            // 
            // N1
            // 
            this.N1.Name = "N1";
            this.N1.Size = new System.Drawing.Size(149, 6);
            // 
            // miCopyXRef
            // 
            this.miCopyXRef.Name = "miCopyXRef";
            this.miCopyXRef.Size = new System.Drawing.Size(152, 22);
            this.miCopyXRef.Text = "miCopyXRef";
            this.miCopyXRef.Click += new System.EventHandler(this.miCopyXRef_Click);

            // 
            // miDQRefresh
            // 
            this.miDQRefresh.Name = "miDQRefresh";
            this.miDQRefresh.Size = new System.Drawing.Size(152, 22);
            this.miDQRefresh.Text = "miDQRefresh";
            this.miDQRefresh.Click += new System.EventHandler(this.miRefresh_Click);
            // 
            // miDQResetFilter
            // 
            this.miDQResetFilter.Name = "miDQResetFilter";
            this.miDQResetFilter.Size = new System.Drawing.Size(152, 22);
            this.miDQResetFilter.Text = "miDQResetFilter";
            this.miDQResetFilter.Click += new System.EventHandler(this.miResetFilter_Click);

            // 
            // TTFamilyGroupsDlg
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.ClientSize = new System.Drawing.Size(827, 454);
            this.Controls.Add(this.tabsTools);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.Sizable;
            this.KeyPreview = true;
            this.Margin = new System.Windows.Forms.Padding(2);
            this.MinimizeBox = false;
            this.Name = "TTFamilyGroupsDlg";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "TreeToolsWin";
            this.Closed += new System.EventHandler(this.Form_Closed);
            this.tabsTools.ResumeLayout(false);
            this.pageDataQuality.ResumeLayout(false);
            this.pageFamilyGroups.ResumeLayout(false);
            this.menuGT.ResumeLayout(false);
            this.menuDQ.ResumeLayout(false);
            this.ResumeLayout(false);
        }
    }
}
