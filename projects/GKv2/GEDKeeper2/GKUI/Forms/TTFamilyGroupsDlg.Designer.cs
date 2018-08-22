namespace GKUI.Forms
{
	partial class TTFamilyGroupsDlg
	{
		private System.Windows.Forms.TabControl tabsTools;
		private System.Windows.Forms.Button btnClose;
		private System.Windows.Forms.TabPage pageFamilyGroups;
		private System.Windows.Forms.TreeView tvGroups;
		private System.Windows.Forms.TabPage pageFamilyGroupsOptions;
		private GKUI.Components.LogChart gkLogChart1;
		private System.Windows.Forms.Button btnAnalyseGroups;

		private void InitializeComponent()
		{
		    this.tabsTools = new System.Windows.Forms.TabControl();
		    this.pageFamilyGroups = new System.Windows.Forms.TabPage();
		    this.btnAnalyseGroups = new System.Windows.Forms.Button();
		    this.gkLogChart1 = new GKUI.Components.LogChart();
		    this.tvGroups = new System.Windows.Forms.TreeView();
		    this.pageFamilyGroupsOptions = new System.Windows.Forms.TabPage();
		    this.btnClose = new System.Windows.Forms.Button();
		    this.tabsTools.SuspendLayout();
		    this.pageFamilyGroups.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // tabsTools
		    // 
		    this.tabsTools.Controls.Add(this.pageFamilyGroups);
		    this.tabsTools.Controls.Add(this.pageFamilyGroupsOptions);
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
		    // btnAnalyseGroups
		    // 
		    this.btnAnalyseGroups.Location = new System.Drawing.Point(691, 372);
		    this.btnAnalyseGroups.Margin = new System.Windows.Forms.Padding(2);
		    this.btnAnalyseGroups.Name = "btnAnalyseGroups";
		    this.btnAnalyseGroups.Size = new System.Drawing.Size(100, 24);
		    this.btnAnalyseGroups.TabIndex = 7;
		    this.btnAnalyseGroups.Text = "btnAnalyseGroups";
		    this.btnAnalyseGroups.Click += new System.EventHandler(this.btnAnalyseGroups_Click);
		    // 
		    // gkLogChart1
		    // 
		    this.gkLogChart1.Location = new System.Drawing.Point(9, 372);
		    this.gkLogChart1.Margin = new System.Windows.Forms.Padding(2);
		    this.gkLogChart1.Name = "gkLogChart1";
		    this.gkLogChart1.Size = new System.Drawing.Size(666, 24);
		    this.gkLogChart1.TabIndex = 1;
		    this.gkLogChart1.TabStop = true;
		    // 
		    // tvGroups
		    // 
		    this.tvGroups.Location = new System.Drawing.Point(9, 8);
		    this.tvGroups.Margin = new System.Windows.Forms.Padding(2);
		    this.tvGroups.Name = "tvGroups";
		    this.tvGroups.Size = new System.Drawing.Size(782, 350);
		    this.tvGroups.TabIndex = 0;
		    this.tvGroups.DoubleClick += new System.EventHandler(this.tvGroups_DoubleClick);
		    // 
		    // pageFamilyGroupsOptions
		    // 
		    this.pageFamilyGroupsOptions.Location = new System.Drawing.Point(4, 22);
		    this.pageFamilyGroupsOptions.Margin = new System.Windows.Forms.Padding(2);
		    this.pageFamilyGroupsOptions.Name = "pageFamilyGroupsOptions";
		    this.pageFamilyGroupsOptions.Size = new System.Drawing.Size(800, 410);
		    this.pageFamilyGroupsOptions.TabIndex = 6;
		    this.pageFamilyGroupsOptions.Text = "pageFamilyGroupsOptions";
		    // 
		    // btnClose
		    // 
		    this.btnClose.DialogResult = System.Windows.Forms.DialogResult.Cancel;
		    this.btnClose.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnClose.Location = new System.Drawing.Point(726, 466);
		    this.btnClose.Margin = new System.Windows.Forms.Padding(2);
		    this.btnClose.Name = "btnClose";
		    this.btnClose.Size = new System.Drawing.Size(91, 24);
		    this.btnClose.TabIndex = 1;
		    this.btnClose.Text = "btnClose";
		    this.btnClose.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    // 
		    // TTFamilyGroupsDlg
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.CancelButton = this.btnClose;
		    this.ClientSize = new System.Drawing.Size(827, 500);
		    this.Controls.Add(this.tabsTools);
		    this.Controls.Add(this.btnClose);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
		    this.KeyPreview = true;
		    this.Margin = new System.Windows.Forms.Padding(2);
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "TTFamilyGroupsDlg";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "TreeToolsWin";
		    this.tabsTools.ResumeLayout(false);
		    this.pageFamilyGroups.ResumeLayout(false);
		    this.ResumeLayout(false);

		}
	}
}