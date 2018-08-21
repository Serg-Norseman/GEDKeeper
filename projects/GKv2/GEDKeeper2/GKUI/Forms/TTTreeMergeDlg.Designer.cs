namespace GKUI.Forms
{
	partial class TTTreeMergeDlg
	{
		private System.Windows.Forms.TabControl tabsTools;
		private System.Windows.Forms.Button btnClose;
		private System.Windows.Forms.TabPage pageTreeMerge;
		private System.Windows.Forms.TabPage pageTreeMergeOptions;
		private System.Windows.Forms.Label lblMasterBase;
		private System.Windows.Forms.TextBox edMasterBase;
		private System.Windows.Forms.Label lblOtherBase;
		private System.Windows.Forms.TextBox edUpdateBase;
		private System.Windows.Forms.Button btnTreeMerge;
		private GKUI.Components.TextBoxEx mSyncRes;

		private void InitializeComponent()
		{
		    this.tabsTools = new System.Windows.Forms.TabControl();
		    this.pageTreeMerge = new System.Windows.Forms.TabPage();
		    this.lblMasterBase = new System.Windows.Forms.Label();
		    this.lblOtherBase = new System.Windows.Forms.Label();
		    this.edMasterBase = new System.Windows.Forms.TextBox();
		    this.edUpdateBase = new System.Windows.Forms.TextBox();
		    this.btnTreeMerge = new System.Windows.Forms.Button();
		    this.mSyncRes = new GKUI.Components.TextBoxEx();
		    this.pageTreeMergeOptions = new System.Windows.Forms.TabPage();
		    this.btnClose = new System.Windows.Forms.Button();
		    this.tabsTools.SuspendLayout();
		    this.pageTreeMerge.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // tabsTools
		    // 
		    this.tabsTools.Controls.Add(this.pageTreeMerge);
		    this.tabsTools.Controls.Add(this.pageTreeMergeOptions);
		    this.tabsTools.Location = new System.Drawing.Point(11, 10);
		    this.tabsTools.Name = "tabsTools";
		    this.tabsTools.SelectedIndex = 0;
		    this.tabsTools.Size = new System.Drawing.Size(1010, 545);
		    this.tabsTools.TabIndex = 0;
		    // 
		    // pageTreeMerge
		    // 
		    this.pageTreeMerge.Controls.Add(this.lblMasterBase);
		    this.pageTreeMerge.Controls.Add(this.lblOtherBase);
		    this.pageTreeMerge.Controls.Add(this.edMasterBase);
		    this.pageTreeMerge.Controls.Add(this.edUpdateBase);
		    this.pageTreeMerge.Controls.Add(this.btnTreeMerge);
		    this.pageTreeMerge.Controls.Add(this.mSyncRes);
		    this.pageTreeMerge.Location = new System.Drawing.Point(4, 26);
		    this.pageTreeMerge.Name = "pageTreeMerge";
		    this.pageTreeMerge.Size = new System.Drawing.Size(1002, 515);
		    this.pageTreeMerge.TabIndex = 1;
		    this.pageTreeMerge.Text = "pageTreeMerge";
		    // 
		    // lblMasterBase
		    // 
		    this.lblMasterBase.AutoSize = true;
		    this.lblMasterBase.Location = new System.Drawing.Point(11, 10);
		    this.lblMasterBase.Name = "lblMasterBase";
		    this.lblMasterBase.Size = new System.Drawing.Size(88, 17);
		    this.lblMasterBase.TabIndex = 0;
		    this.lblMasterBase.Text = "lblMasterBase";
		    // 
		    // lblOtherBase
		    // 
		    this.lblOtherBase.AutoSize = true;
		    this.lblOtherBase.Location = new System.Drawing.Point(11, 68);
		    this.lblOtherBase.Name = "lblOtherBase";
		    this.lblOtherBase.Size = new System.Drawing.Size(83, 17);
		    this.lblOtherBase.TabIndex = 1;
		    this.lblOtherBase.Text = "lblOtherBase";
		    // 
		    // edMasterBase
		    // 
		    this.edMasterBase.BackColor = System.Drawing.SystemColors.Control;
		    this.edMasterBase.Location = new System.Drawing.Point(11, 29);
		    this.edMasterBase.Name = "edMasterBase";
		    this.edMasterBase.ReadOnly = true;
		    this.edMasterBase.Size = new System.Drawing.Size(853, 24);
		    this.edMasterBase.TabIndex = 0;
		    this.edMasterBase.Text = "edMasterBase";
		    // 
		    // edUpdateBase
		    // 
		    this.edUpdateBase.Location = new System.Drawing.Point(11, 87);
		    this.edUpdateBase.Name = "edUpdateBase";
		    this.edUpdateBase.ReadOnly = true;
		    this.edUpdateBase.Size = new System.Drawing.Size(853, 24);
		    this.edUpdateBase.TabIndex = 1;
		    // 
		    // btnTreeMerge
		    // 
		    this.btnTreeMerge.Location = new System.Drawing.Point(874, 85);
		    this.btnTreeMerge.Name = "btnTreeMerge";
		    this.btnTreeMerge.Size = new System.Drawing.Size(113, 30);
		    this.btnTreeMerge.TabIndex = 2;
		    this.btnTreeMerge.Text = "btnTreeMerge";
		    this.btnTreeMerge.Click += new System.EventHandler(this.btnTreeMerge_Click);
		    // 
		    // mSyncRes
		    // 
		    this.mSyncRes.Location = new System.Drawing.Point(11, 131);
		    this.mSyncRes.Multiline = true;
		    this.mSyncRes.Name = "mSyncRes";
		    this.mSyncRes.ReadOnly = true;
		    this.mSyncRes.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
		    this.mSyncRes.Size = new System.Drawing.Size(976, 371);
		    this.mSyncRes.TabIndex = 4;
		    // 
		    // pageTreeMergeOptions
		    // 
		    this.pageTreeMergeOptions.Location = new System.Drawing.Point(4, 26);
		    this.pageTreeMergeOptions.Name = "pageTreeMergeOptions";
		    this.pageTreeMergeOptions.Size = new System.Drawing.Size(1002, 515);
		    this.pageTreeMergeOptions.TabIndex = 2;
		    this.pageTreeMergeOptions.Text = "pageTreeMergeOptions";
		    // 
		    // btnClose
		    // 
		    this.btnClose.DialogResult = System.Windows.Forms.DialogResult.Cancel;
		    this.btnClose.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnClose.Location = new System.Drawing.Point(907, 583);
		    this.btnClose.Name = "btnClose";
		    this.btnClose.Size = new System.Drawing.Size(114, 30);
		    this.btnClose.TabIndex = 1;
		    this.btnClose.Text = "btnClose";
		    this.btnClose.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    // 
		    // TTTreeMergeDlg
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.CancelButton = this.btnClose;
		    this.ClientSize = new System.Drawing.Size(1034, 625);
		    this.Controls.Add(this.tabsTools);
		    this.Controls.Add(this.btnClose);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
		    this.KeyPreview = true;
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "TTTreeMergeDlg";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "TreeToolsWin";
		    this.tabsTools.ResumeLayout(false);
		    this.pageTreeMerge.ResumeLayout(false);
		    this.pageTreeMerge.PerformLayout();
		    this.ResumeLayout(false);

		}
	}
}