namespace GKUI.Forms
{
	partial class TTTreeCompareDlg
	{
		private GKUI.Components.GKTabControl tabsTools;
		private System.Windows.Forms.TabPage pageTreeCompare;
		private System.Windows.Forms.TextBox ListCompare;
		private System.Windows.Forms.Button btnClose;
		private System.Windows.Forms.Label lblFile;
		private System.Windows.Forms.TextBox txtCompareFile;
		private System.Windows.Forms.Button btnFileChoose;
		private System.Windows.Forms.RadioButton radAnalysis;
		private System.Windows.Forms.Button btnMatch;
		private System.Windows.Forms.RadioButton radMathExternal;
		private System.Windows.Forms.RadioButton radMatchInternal;
		private System.Windows.Forms.GroupBox grpMatchType;

		private void InitializeComponent()
		{
		    this.tabsTools = new GKUI.Components.GKTabControl();
		    this.pageTreeCompare = new System.Windows.Forms.TabPage();
		    this.btnMatch = new System.Windows.Forms.Button();
		    this.grpMatchType = new System.Windows.Forms.GroupBox();
		    this.radAnalysis = new System.Windows.Forms.RadioButton();
		    this.lblFile = new System.Windows.Forms.Label();
		    this.txtCompareFile = new System.Windows.Forms.TextBox();
		    this.btnFileChoose = new System.Windows.Forms.Button();
		    this.radMatchInternal = new System.Windows.Forms.RadioButton();
		    this.radMathExternal = new System.Windows.Forms.RadioButton();
		    this.ListCompare = new System.Windows.Forms.TextBox();
		    this.btnClose = new System.Windows.Forms.Button();
		    this.tabsTools.SuspendLayout();
		    this.pageTreeCompare.SuspendLayout();
		    this.grpMatchType.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // tabsTools
		    // 
		    this.tabsTools.Controls.Add(this.pageTreeCompare);
		    this.tabsTools.Location = new System.Drawing.Point(11, 10);
		    this.tabsTools.Name = "tabsTools";
		    this.tabsTools.SelectedIndex = 0;
		    this.tabsTools.Size = new System.Drawing.Size(1010, 545);
		    this.tabsTools.TabIndex = 0;
		    // 
		    // pageTreeCompare
		    // 
		    this.pageTreeCompare.Controls.Add(this.btnMatch);
		    this.pageTreeCompare.Controls.Add(this.grpMatchType);
		    this.pageTreeCompare.Controls.Add(this.ListCompare);
		    this.pageTreeCompare.Location = new System.Drawing.Point(4, 26);
		    this.pageTreeCompare.Name = "pageTreeCompare";
		    this.pageTreeCompare.Size = new System.Drawing.Size(1002, 515);
		    this.pageTreeCompare.TabIndex = 0;
		    this.pageTreeCompare.Text = "pageTreeCompare";
		    // 
		    // btnMatch
		    // 
		    this.btnMatch.Location = new System.Drawing.Point(874, 13);
		    this.btnMatch.Name = "btnMatch";
		    this.btnMatch.Size = new System.Drawing.Size(113, 31);
		    this.btnMatch.TabIndex = 7;
		    this.btnMatch.Text = "btnMatch";
		    this.btnMatch.Click += new System.EventHandler(this.btnMatch_Click);
		    // 
		    // grpMatchType
		    // 
		    this.grpMatchType.Controls.Add(this.radAnalysis);
		    this.grpMatchType.Controls.Add(this.lblFile);
		    this.grpMatchType.Controls.Add(this.txtCompareFile);
		    this.grpMatchType.Controls.Add(this.btnFileChoose);
		    this.grpMatchType.Controls.Add(this.radMatchInternal);
		    this.grpMatchType.Controls.Add(this.radMathExternal);
		    this.grpMatchType.Location = new System.Drawing.Point(11, 4);
		    this.grpMatchType.Name = "grpMatchType";
		    this.grpMatchType.Size = new System.Drawing.Size(563, 139);
		    this.grpMatchType.TabIndex = 6;
		    this.grpMatchType.TabStop = false;
		    this.grpMatchType.Text = "grpMatchType";
		    // 
		    // radAnalysis
		    // 
		    this.radAnalysis.AutoSize = true;
		    this.radAnalysis.Location = new System.Drawing.Point(22, 109);
		    this.radAnalysis.Name = "radAnalysis";
		    this.radAnalysis.Size = new System.Drawing.Size(96, 21);
		    this.radAnalysis.TabIndex = 7;
		    this.radAnalysis.Text = "radAnalysis";
		    this.radAnalysis.CheckedChanged += new System.EventHandler(this.rbtnMatch_CheckedChanged);
		    // 
		    // lblFile
		    // 
		    this.lblFile.AutoSize = true;
		    this.lblFile.Enabled = false;
		    this.lblFile.Location = new System.Drawing.Point(22, 80);
		    this.lblFile.Name = "lblFile";
		    this.lblFile.Size = new System.Drawing.Size(38, 17);
		    this.lblFile.TabIndex = 4;
		    this.lblFile.Text = "lblFile";
		    // 
		    // txtCompareFile
		    // 
		    this.txtCompareFile.Enabled = false;
		    this.txtCompareFile.Location = new System.Drawing.Point(80, 77);
		    this.txtCompareFile.Name = "txtCompareFile";
		    this.txtCompareFile.ReadOnly = true;
		    this.txtCompareFile.Size = new System.Drawing.Size(339, 24);
		    this.txtCompareFile.TabIndex = 5;
		    // 
		    // btnFileChoose
		    // 
		    this.btnFileChoose.Enabled = false;
		    this.btnFileChoose.Location = new System.Drawing.Point(425, 73);
		    this.btnFileChoose.Name = "btnFileChoose";
		    this.btnFileChoose.Size = new System.Drawing.Size(113, 30);
		    this.btnFileChoose.TabIndex = 6;
		    this.btnFileChoose.Text = "btnFileChoose";
		    this.btnFileChoose.Click += new System.EventHandler(this.btnFileChoose_Click);
		    // 
		    // radMatchInternal
		    // 
		    this.radMatchInternal.AutoSize = true;
		    this.radMatchInternal.Checked = true;
		    this.radMatchInternal.Location = new System.Drawing.Point(22, 19);
		    this.radMatchInternal.Name = "radMatchInternal";
		    this.radMatchInternal.Size = new System.Drawing.Size(132, 21);
		    this.radMatchInternal.TabIndex = 2;
		    this.radMatchInternal.TabStop = true;
		    this.radMatchInternal.Text = "radMatchInternal";
		    this.radMatchInternal.CheckedChanged += new System.EventHandler(this.rbtnMatch_CheckedChanged);
		    // 
		    // radMathExternal
		    // 
		    this.radMathExternal.AutoSize = true;
		    this.radMathExternal.Location = new System.Drawing.Point(22, 49);
		    this.radMathExternal.Name = "radMathExternal";
		    this.radMathExternal.Size = new System.Drawing.Size(129, 21);
		    this.radMathExternal.TabIndex = 3;
		    this.radMathExternal.Text = "radMathExternal";
		    this.radMathExternal.CheckedChanged += new System.EventHandler(this.rbtnMatch_CheckedChanged);
		    // 
		    // ListCompare
		    // 
		    this.ListCompare.Location = new System.Drawing.Point(11, 151);
		    this.ListCompare.Multiline = true;
		    this.ListCompare.Name = "ListCompare";
		    this.ListCompare.ReadOnly = true;
		    this.ListCompare.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
		    this.ListCompare.Size = new System.Drawing.Size(976, 421);
		    this.ListCompare.TabIndex = 0;
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
		    // TTTreeCompareDlg
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
		    this.Name = "TTTreeCompareDlg";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "TreeToolsWin";
		    this.tabsTools.ResumeLayout(false);
		    this.pageTreeCompare.ResumeLayout(false);
		    this.pageTreeCompare.PerformLayout();
		    this.grpMatchType.ResumeLayout(false);
		    this.grpMatchType.PerformLayout();
		    this.ResumeLayout(false);

		}
	}
}
