namespace GKUI.Forms
{
	partial class TTRecMergeDlg
	{
		private System.Windows.Forms.TabControl PageControl1;
		private System.Windows.Forms.TabPage pageMerge;
		private System.Windows.Forms.Button btnAutoSearch;
		private System.Windows.Forms.Button btnSkip;
		private System.Windows.Forms.ProgressBar ProgressBar1;
		private System.Windows.Forms.TabPage pageMergeOptions;
		private System.Windows.Forms.GroupBox rgMode;
		private System.Windows.Forms.GroupBox grpSearchPersons;
		private System.Windows.Forms.Label lblNameAccuracy;
		private System.Windows.Forms.Label lblYearInaccuracy;
		private System.Windows.Forms.NumericUpDown edNameAccuracy;
		private System.Windows.Forms.NumericUpDown edYearInaccuracy;
		private System.Windows.Forms.CheckBox chkBirthYear;
		private System.Windows.Forms.RadioButton radPersons;
		private System.Windows.Forms.RadioButton radNotes;
		private System.Windows.Forms.RadioButton radFamilies;
		private System.Windows.Forms.RadioButton radSources;
		private System.Windows.Forms.CheckBox chkBookmarkMerged;
		private System.Windows.Forms.GroupBox grpMergeOther;
		private GKUI.Components.GKMergeControl MergeControl;
		private System.Windows.Forms.CheckBox chkIndistinctMatching;

		private void InitializeComponent()
		{
		    this.PageControl1 = new System.Windows.Forms.TabControl();
		    this.pageMerge = new System.Windows.Forms.TabPage();
		    this.MergeControl = new GKUI.Components.GKMergeControl();
		    this.btnAutoSearch = new System.Windows.Forms.Button();
		    this.btnSkip = new System.Windows.Forms.Button();
		    this.ProgressBar1 = new System.Windows.Forms.ProgressBar();
		    this.pageMergeOptions = new System.Windows.Forms.TabPage();
		    this.grpMergeOther = new System.Windows.Forms.GroupBox();
		    this.chkBookmarkMerged = new System.Windows.Forms.CheckBox();
		    this.rgMode = new System.Windows.Forms.GroupBox();
		    this.radSources = new System.Windows.Forms.RadioButton();
		    this.radFamilies = new System.Windows.Forms.RadioButton();
		    this.radNotes = new System.Windows.Forms.RadioButton();
		    this.radPersons = new System.Windows.Forms.RadioButton();
		    this.grpSearchPersons = new System.Windows.Forms.GroupBox();
		    this.lblNameAccuracy = new System.Windows.Forms.Label();
		    this.lblYearInaccuracy = new System.Windows.Forms.Label();
		    this.chkIndistinctMatching = new System.Windows.Forms.CheckBox();
		    this.edNameAccuracy = new System.Windows.Forms.NumericUpDown();
		    this.edYearInaccuracy = new System.Windows.Forms.NumericUpDown();
		    this.chkBirthYear = new System.Windows.Forms.CheckBox();
		    this.PageControl1.SuspendLayout();
		    this.pageMerge.SuspendLayout();
		    this.pageMergeOptions.SuspendLayout();
		    this.grpMergeOther.SuspendLayout();
		    this.rgMode.SuspendLayout();
		    this.grpSearchPersons.SuspendLayout();
		    ((System.ComponentModel.ISupportInitialize)(this.edNameAccuracy)).BeginInit();
		    ((System.ComponentModel.ISupportInitialize)(this.edYearInaccuracy)).BeginInit();
		    this.SuspendLayout();
		    // 
		    // PageControl1
		    // 
		    this.PageControl1.Controls.Add(this.pageMerge);
		    this.PageControl1.Controls.Add(this.pageMergeOptions);
		    this.PageControl1.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.PageControl1.Location = new System.Drawing.Point(0, 0);
		    this.PageControl1.Name = "PageControl1";
		    this.PageControl1.SelectedIndex = 0;
		    this.PageControl1.Size = new System.Drawing.Size(965, 499);
		    this.PageControl1.TabIndex = 0;
		    // 
		    // pageMerge
		    // 
		    this.pageMerge.Controls.Add(this.MergeControl);
		    this.pageMerge.Controls.Add(this.btnAutoSearch);
		    this.pageMerge.Controls.Add(this.btnSkip);
		    this.pageMerge.Controls.Add(this.ProgressBar1);
		    this.pageMerge.Location = new System.Drawing.Point(4, 26);
		    this.pageMerge.Name = "pageMerge";
		    this.pageMerge.Size = new System.Drawing.Size(957, 469);
		    this.pageMerge.TabIndex = 0;
		    this.pageMerge.Text = "pageMerge";
		    // 
		    // MergeControl
		    // 
		    this.MergeControl.AutoSize = true;
		    this.MergeControl.Base = null;
		    this.MergeControl.Bookmark = false;
		    this.MergeControl.Dock = System.Windows.Forms.DockStyle.Top;
		    this.MergeControl.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.MergeControl.Location = new System.Drawing.Point(0, 0);
		    this.MergeControl.MergeMode = GKCommon.GEDCOM.GEDCOMRecordType.rtNone;
		    this.MergeControl.Name = "MergeControl";
		    this.MergeControl.Size = new System.Drawing.Size(957, 399);
		    this.MergeControl.TabIndex = 11;
		    // 
		    // btnAutoSearch
		    // 
		    this.btnAutoSearch.Location = new System.Drawing.Point(17, 420);
		    this.btnAutoSearch.Name = "btnAutoSearch";
		    this.btnAutoSearch.Size = new System.Drawing.Size(105, 31);
		    this.btnAutoSearch.TabIndex = 0;
		    this.btnAutoSearch.Text = "btnAutoSearch";
		    this.btnAutoSearch.Click += new System.EventHandler(this.btnSearch_Click);
		    // 
		    // btnSkip
		    // 
		    this.btnSkip.Location = new System.Drawing.Point(129, 420);
		    this.btnSkip.Name = "btnSkip";
		    this.btnSkip.Size = new System.Drawing.Size(105, 31);
		    this.btnSkip.TabIndex = 9;
		    this.btnSkip.Text = "btnSkip";
		    this.btnSkip.Click += new System.EventHandler(this.btnSkip_Click);
		    // 
		    // ProgressBar1
		    // 
		    this.ProgressBar1.Location = new System.Drawing.Point(242, 420);
		    this.ProgressBar1.Name = "ProgressBar1";
		    this.ProgressBar1.Size = new System.Drawing.Size(700, 31);
		    this.ProgressBar1.Step = 1;
		    this.ProgressBar1.TabIndex = 10;
		    // 
		    // pageMergeOptions
		    // 
		    this.pageMergeOptions.Controls.Add(this.grpMergeOther);
		    this.pageMergeOptions.Controls.Add(this.rgMode);
		    this.pageMergeOptions.Controls.Add(this.grpSearchPersons);
		    this.pageMergeOptions.Location = new System.Drawing.Point(4, 26);
		    this.pageMergeOptions.Name = "pageMergeOptions";
		    this.pageMergeOptions.Size = new System.Drawing.Size(960, 471);
		    this.pageMergeOptions.TabIndex = 1;
		    this.pageMergeOptions.Text = "pageMergeOptions";
		    // 
		    // grpMergeOther
		    // 
		    this.grpMergeOther.Controls.Add(this.chkBookmarkMerged);
		    this.grpMergeOther.Location = new System.Drawing.Point(342, 10);
		    this.grpMergeOther.Name = "grpMergeOther";
		    this.grpMergeOther.Size = new System.Drawing.Size(331, 118);
		    this.grpMergeOther.TabIndex = 2;
		    this.grpMergeOther.TabStop = false;
		    this.grpMergeOther.Text = "grpMergeOther";
		    // 
		    // chkBookmarkMerged
		    // 
		    this.chkBookmarkMerged.Location = new System.Drawing.Point(6, 23);
		    this.chkBookmarkMerged.Name = "chkBookmarkMerged";
		    this.chkBookmarkMerged.Size = new System.Drawing.Size(319, 24);
		    this.chkBookmarkMerged.TabIndex = 0;
		    this.chkBookmarkMerged.Text = "chkBookmarkMerged";
		    this.chkBookmarkMerged.UseVisualStyleBackColor = true;
		    this.chkBookmarkMerged.CheckedChanged += new System.EventHandler(this.chkBookmarkMerged_CheckedChanged);
		    // 
		    // rgMode
		    // 
		    this.rgMode.Controls.Add(this.radSources);
		    this.rgMode.Controls.Add(this.radFamilies);
		    this.rgMode.Controls.Add(this.radNotes);
		    this.rgMode.Controls.Add(this.radPersons);
		    this.rgMode.Location = new System.Drawing.Point(11, 10);
		    this.rgMode.Name = "rgMode";
		    this.rgMode.Size = new System.Drawing.Size(315, 118);
		    this.rgMode.TabIndex = 0;
		    this.rgMode.TabStop = false;
		    this.rgMode.Text = "rgMode";
		    // 
		    // radSources
		    // 
		    this.radSources.Location = new System.Drawing.Point(22, 87);
		    this.radSources.Name = "radSources";
		    this.radSources.Size = new System.Drawing.Size(269, 20);
		    this.radSources.TabIndex = 3;
		    this.radSources.Text = "radSources";
		    this.radSources.Click += new System.EventHandler(this.radMergeMode_Click);
		    // 
		    // radFamilies
		    // 
		    this.radFamilies.Location = new System.Drawing.Point(22, 68);
		    this.radFamilies.Name = "radFamilies";
		    this.radFamilies.Size = new System.Drawing.Size(269, 19);
		    this.radFamilies.TabIndex = 2;
		    this.radFamilies.Text = "radFamilies";
		    this.radFamilies.Click += new System.EventHandler(this.radMergeMode_Click);
		    // 
		    // radNotes
		    // 
		    this.radNotes.Location = new System.Drawing.Point(22, 49);
		    this.radNotes.Name = "radNotes";
		    this.radNotes.Size = new System.Drawing.Size(269, 19);
		    this.radNotes.TabIndex = 1;
		    this.radNotes.Text = "radNotes";
		    this.radNotes.Click += new System.EventHandler(this.radMergeMode_Click);
		    // 
		    // radPersons
		    // 
		    this.radPersons.Checked = true;
		    this.radPersons.Location = new System.Drawing.Point(22, 29);
		    this.radPersons.Name = "radPersons";
		    this.radPersons.Size = new System.Drawing.Size(269, 20);
		    this.radPersons.TabIndex = 0;
		    this.radPersons.TabStop = true;
		    this.radPersons.Text = "radPersons";
		    this.radPersons.Click += new System.EventHandler(this.radMergeMode_Click);
		    // 
		    // grpSearchPersons
		    // 
		    this.grpSearchPersons.Controls.Add(this.lblNameAccuracy);
		    this.grpSearchPersons.Controls.Add(this.lblYearInaccuracy);
		    this.grpSearchPersons.Controls.Add(this.chkIndistinctMatching);
		    this.grpSearchPersons.Controls.Add(this.edNameAccuracy);
		    this.grpSearchPersons.Controls.Add(this.edYearInaccuracy);
		    this.grpSearchPersons.Controls.Add(this.chkBirthYear);
		    this.grpSearchPersons.Location = new System.Drawing.Point(11, 136);
		    this.grpSearchPersons.Name = "grpSearchPersons";
		    this.grpSearchPersons.Size = new System.Drawing.Size(315, 193);
		    this.grpSearchPersons.TabIndex = 1;
		    this.grpSearchPersons.TabStop = false;
		    this.grpSearchPersons.Text = "grpSearchPersons";
		    // 
		    // lblNameAccuracy
		    // 
		    this.lblNameAccuracy.Location = new System.Drawing.Point(22, 49);
		    this.lblNameAccuracy.Name = "lblNameAccuracy";
		    this.lblNameAccuracy.Size = new System.Drawing.Size(152, 15);
		    this.lblNameAccuracy.TabIndex = 0;
		    this.lblNameAccuracy.Text = "lblNameAccuracy";
		    // 
		    // lblYearInaccuracy
		    // 
		    this.lblYearInaccuracy.Location = new System.Drawing.Point(22, 132);
		    this.lblYearInaccuracy.Name = "lblYearInaccuracy";
		    this.lblYearInaccuracy.Size = new System.Drawing.Size(152, 16);
		    this.lblYearInaccuracy.TabIndex = 1;
		    this.lblYearInaccuracy.Text = "lblYearInaccuracy";
		    // 
		    // chkIndistinctMatching
		    // 
		    this.chkIndistinctMatching.Location = new System.Drawing.Point(8, 24);
		    this.chkIndistinctMatching.Name = "chkIndistinctMatching";
		    this.chkIndistinctMatching.Size = new System.Drawing.Size(371, 21);
		    this.chkIndistinctMatching.TabIndex = 1;
		    this.chkIndistinctMatching.Text = "chkIndistinctMatching";
		    // 
		    // edNameAccuracy
		    // 
		    this.edNameAccuracy.Location = new System.Drawing.Point(22, 68);
		    this.edNameAccuracy.Name = "edNameAccuracy";
		    this.edNameAccuracy.Size = new System.Drawing.Size(152, 24);
		    this.edNameAccuracy.TabIndex = 2;
		    this.edNameAccuracy.Value = new decimal(new int[] {
            90,
            0,
            0,
            0});
		    // 
		    // edYearInaccuracy
		    // 
		    this.edYearInaccuracy.Location = new System.Drawing.Point(22, 152);
		    this.edYearInaccuracy.Name = "edYearInaccuracy";
		    this.edYearInaccuracy.Size = new System.Drawing.Size(152, 24);
		    this.edYearInaccuracy.TabIndex = 4;
		    this.edYearInaccuracy.Value = new decimal(new int[] {
            3,
            0,
            0,
            0});
		    // 
		    // chkBirthYear
		    // 
		    this.chkBirthYear.Location = new System.Drawing.Point(8, 109);
		    this.chkBirthYear.Name = "chkBirthYear";
		    this.chkBirthYear.Size = new System.Drawing.Size(371, 21);
		    this.chkBirthYear.TabIndex = 6;
		    this.chkBirthYear.Text = "chkBirthYear";
		    // 
		    // TTRecMergeDlg
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.Caption = "TreeToolsWin";
		    this.ClientSize = new System.Drawing.Size(965, 499);
		    this.Controls.Add(this.PageControl1);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
		    this.KeyPreview = true;
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "TTRecMergeDlg";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "TreeToolsWin";
		    this.PageControl1.ResumeLayout(false);
		    this.pageMerge.ResumeLayout(false);
		    this.pageMerge.PerformLayout();
		    this.pageMergeOptions.ResumeLayout(false);
		    this.grpMergeOther.ResumeLayout(false);
		    this.rgMode.ResumeLayout(false);
		    this.grpSearchPersons.ResumeLayout(false);
		    ((System.ComponentModel.ISupportInitialize)(this.edNameAccuracy)).EndInit();
		    ((System.ComponentModel.ISupportInitialize)(this.edYearInaccuracy)).EndInit();
		    this.ResumeLayout(false);

		}
	}
}
