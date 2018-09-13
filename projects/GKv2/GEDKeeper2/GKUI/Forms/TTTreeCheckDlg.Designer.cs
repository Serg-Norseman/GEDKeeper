namespace GKUI.Forms
{
	partial class TTTreeCheckDlg
	{
		private System.Windows.Forms.TabControl tabsTools;
		private System.Windows.Forms.Button btnClose;
		private System.Windows.Forms.TabPage pageTreeCheck;
		private System.Windows.Forms.Button btnBaseRepair;
		private System.Windows.Forms.Panel panProblemsContainer;
		private System.Windows.Forms.Button btnAnalyseBase;

		private void InitializeComponent()
		{
		    this.tabsTools = new System.Windows.Forms.TabControl();
		    this.pageTreeCheck = new System.Windows.Forms.TabPage();
		    this.btnBaseRepair = new System.Windows.Forms.Button();
		    this.panProblemsContainer = new System.Windows.Forms.Panel();
		    this.btnClose = new System.Windows.Forms.Button();
		    this.btnAnalyseBase = new System.Windows.Forms.Button();
		    this.tabsTools.SuspendLayout();
		    this.pageTreeCheck.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // tabsTools
		    // 
		    this.tabsTools.Controls.Add(this.pageTreeCheck);
		    this.tabsTools.Location = new System.Drawing.Point(11, 10);
		    this.tabsTools.Name = "tabsTools";
		    this.tabsTools.SelectedIndex = 0;
		    this.tabsTools.Size = new System.Drawing.Size(1010, 545);
		    this.tabsTools.TabIndex = 0;
		    // 
		    // pageTreeCheck
		    // 
		    this.pageTreeCheck.Controls.Add(this.btnAnalyseBase);
		    this.pageTreeCheck.Controls.Add(this.btnBaseRepair);
		    this.pageTreeCheck.Controls.Add(this.panProblemsContainer);
		    this.pageTreeCheck.Location = new System.Drawing.Point(4, 26);
		    this.pageTreeCheck.Name = "pageTreeCheck";
		    this.pageTreeCheck.Size = new System.Drawing.Size(1002, 515);
		    this.pageTreeCheck.TabIndex = 6;
		    this.pageTreeCheck.Text = "pageTreeCheck";
		    // 
		    // btnBaseRepair
		    // 
		    this.btnBaseRepair.Location = new System.Drawing.Point(784, 464);
		    this.btnBaseRepair.Name = "btnBaseRepair";
		    this.btnBaseRepair.Size = new System.Drawing.Size(203, 30);
		    this.btnBaseRepair.TabIndex = 0;
		    this.btnBaseRepair.Text = "btnBaseRepair";
		    this.btnBaseRepair.Click += new System.EventHandler(this.btnBaseRepair_Click);
		    // 
		    // panProblemsContainer
		    // 
		    this.panProblemsContainer.Location = new System.Drawing.Point(0, 0);
		    this.panProblemsContainer.Name = "panProblemsContainer";
		    this.panProblemsContainer.Size = new System.Drawing.Size(998, 448);
		    this.panProblemsContainer.TabIndex = 1;
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
		    // btnAnalyseBase
		    // 
		    this.btnAnalyseBase.Location = new System.Drawing.Point(17, 464);
		    this.btnAnalyseBase.Name = "btnAnalyseBase";
		    this.btnAnalyseBase.Size = new System.Drawing.Size(203, 30);
		    this.btnAnalyseBase.TabIndex = 0;
		    this.btnAnalyseBase.Text = "btnAnalyseBase";
		    this.btnAnalyseBase.Click += new System.EventHandler(this.btnAnalyseBase_Click);
		    // 
		    // TTTreeCheckDlg
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
		    this.Name = "TTTreeCheckDlg";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "TreeToolsWin";
		    this.tabsTools.ResumeLayout(false);
		    this.pageTreeCheck.ResumeLayout(false);
		    this.ResumeLayout(false);
		}
	}
}
