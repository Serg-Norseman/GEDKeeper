namespace GKUI.Forms
{
    partial class TTRecMergeDlg
    {
        private GKUI.Components.GKTabControl PageControl1;
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
        private System.Windows.Forms.CheckBox chkIndistinctMatching;
        private System.Windows.Forms.Button btnMergeToRight;
        private System.Windows.Forms.Button btnMergeToLeft;
        private System.Windows.Forms.Button btnRec2Select;
        private System.Windows.Forms.Button btnRec1Select;
        private System.Windows.Forms.TextBox Edit2;
        private System.Windows.Forms.TextBox Edit1;
        private System.Windows.Forms.Label Lab2;
        private System.Windows.Forms.Label Lab1;
        private GKUI.Components.HyperView fView1;
        private GKUI.Components.HyperView fView2;
        private System.Windows.Forms.Button btnEditRight;
        private System.Windows.Forms.Button btnEditLeft;

        private void InitializeComponent()
        {
            this.PageControl1 = new GKUI.Components.GKTabControl();
            this.pageMerge = new System.Windows.Forms.TabPage();
            this.Lab1 = new System.Windows.Forms.Label();
            this.Lab2 = new System.Windows.Forms.Label();
            this.Edit1 = new System.Windows.Forms.TextBox();
            this.Edit2 = new System.Windows.Forms.TextBox();
            this.btnRec1Select = new System.Windows.Forms.Button();
            this.btnRec2Select = new System.Windows.Forms.Button();
            this.btnMergeToLeft = new System.Windows.Forms.Button();
            this.btnMergeToRight = new System.Windows.Forms.Button();
            this.fView1 = new GKUI.Components.HyperView();
            this.fView2 = new GKUI.Components.HyperView();
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
            this.btnEditLeft = new System.Windows.Forms.Button();
            this.btnEditRight = new System.Windows.Forms.Button();
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
            this.pageMerge.Controls.Add(this.fView1);
            this.pageMerge.Controls.Add(this.Lab1);
            this.pageMerge.Controls.Add(this.Lab2);
            this.pageMerge.Controls.Add(this.Edit1);
            this.pageMerge.Controls.Add(this.Edit2);
            this.pageMerge.Controls.Add(this.btnRec1Select);
            this.pageMerge.Controls.Add(this.btnRec2Select);
            this.pageMerge.Controls.Add(this.btnMergeToLeft);
            this.pageMerge.Controls.Add(this.btnMergeToRight);
            this.pageMerge.Controls.Add(this.btnEditLeft);
            this.pageMerge.Controls.Add(this.btnEditRight);
            this.pageMerge.Controls.Add(this.fView2);
            this.pageMerge.Controls.Add(this.btnAutoSearch);
            this.pageMerge.Controls.Add(this.btnSkip);
            this.pageMerge.Controls.Add(this.ProgressBar1);
            this.pageMerge.Location = new System.Drawing.Point(4, 26);
            this.pageMerge.Name = "pageMerge";
            this.pageMerge.Size = new System.Drawing.Size(957, 469);
            this.pageMerge.TabIndex = 0;
            this.pageMerge.Text = "pageMerge";
            // 
            // Lab1
            // 
            this.Lab1.AutoSize = true;
            this.Lab1.Location = new System.Drawing.Point(14, 13);
            this.Lab1.Name = "Lab1";
            this.Lab1.Size = new System.Drawing.Size(40, 17);
            this.Lab1.TabIndex = 9;
            this.Lab1.Text = "XXX1";
            // 
            // Lab2
            // 
            this.Lab2.AutoSize = true;
            this.Lab2.Location = new System.Drawing.Point(482, 13);
            this.Lab2.Name = "Lab2";
            this.Lab2.Size = new System.Drawing.Size(40, 17);
            this.Lab2.TabIndex = 11;
            this.Lab2.Text = "XXX2";
            // 
            // Edit1
            // 
            this.Edit1.Location = new System.Drawing.Point(14, 33);
            this.Edit1.Name = "Edit1";
            this.Edit1.ReadOnly = true;
            this.Edit1.Size = new System.Drawing.Size(346, 24);
            this.Edit1.TabIndex = 10;
            // 
            // Edit2
            // 
            this.Edit2.Location = new System.Drawing.Point(482, 33);
            this.Edit2.Name = "Edit2";
            this.Edit2.ReadOnly = true;
            this.Edit2.Size = new System.Drawing.Size(353, 24);
            this.Edit2.TabIndex = 12;
            // 
            // btnRec1Select
            // 
            this.btnRec1Select.Location = new System.Drawing.Point(366, 32);
            this.btnRec1Select.Name = "btnRec1Select";
            this.btnRec1Select.Size = new System.Drawing.Size(101, 26);
            this.btnRec1Select.TabIndex = 13;
            this.btnRec1Select.Text = "btnRec1Select";
            this.btnRec1Select.Click += new System.EventHandler(this.btnRec1Select_Click);
            // 
            // btnRec2Select
            // 
            this.btnRec2Select.Location = new System.Drawing.Point(841, 32);
            this.btnRec2Select.Name = "btnRec2Select";
            this.btnRec2Select.Size = new System.Drawing.Size(101, 26);
            this.btnRec2Select.TabIndex = 14;
            this.btnRec2Select.Text = "btnRec2Select";
            this.btnRec2Select.Click += new System.EventHandler(this.btnRec2Select_Click);
            // 
            // btnMergeToLeft
            // 
            this.btnMergeToLeft.Enabled = false;
            this.btnMergeToLeft.Location = new System.Drawing.Point(386, 371);
            this.btnMergeToLeft.Name = "btnMergeToLeft";
            this.btnMergeToLeft.Size = new System.Drawing.Size(81, 26);
            this.btnMergeToLeft.TabIndex = 15;
            this.btnMergeToLeft.Text = "<<<";
            this.btnMergeToLeft.Click += new System.EventHandler(this.btnMergeToLeft_Click);
            // 
            // btnMergeToRight
            // 
            this.btnMergeToRight.Enabled = false;
            this.btnMergeToRight.Location = new System.Drawing.Point(482, 371);
            this.btnMergeToRight.Name = "btnMergeToRight";
            this.btnMergeToRight.Size = new System.Drawing.Size(81, 26);
            this.btnMergeToRight.TabIndex = 16;
            this.btnMergeToRight.Text = ">>>";
            this.btnMergeToRight.Click += new System.EventHandler(this.btnMergeToRight_Click);
            // 
            // btnEditLeft
            // 
            this.btnEditLeft.Enabled = false;
            this.btnEditLeft.Location = new System.Drawing.Point(14, 371);
            this.btnEditLeft.Name = "btnEditLeft";
            this.btnEditLeft.Size = new System.Drawing.Size(140, 26);
            this.btnEditLeft.TabIndex = 15;
            this.btnEditLeft.Text = "Edit";
            this.btnEditLeft.Click += new System.EventHandler(this.btnEditLeft_Click);
            // 
            // btnEditRight
            // 
            this.btnEditRight.Enabled = false;
            this.btnEditRight.Location = new System.Drawing.Point(802, 371);
            this.btnEditRight.Name = "btnEditRight";
            this.btnEditRight.Size = new System.Drawing.Size(140, 26);
            this.btnEditRight.TabIndex = 16;
            this.btnEditRight.Text = "Edit";
            this.btnEditRight.Click += new System.EventHandler(this.btnEditRight_Click);
            // 
            // fView1
            // 
            this.fView1.AutoScroll = true;
            this.fView1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.fView1.BorderWidth = 0;
            this.fView1.LinkColor = System.Drawing.Color.Blue;
            this.fView1.Location = new System.Drawing.Point(14, 63);
            this.fView1.Name = "fView1";
            this.fView1.Size = new System.Drawing.Size(453, 302);
            this.fView1.TabIndex = 17;
            this.fView1.TabStop = true;
            this.fView1.WordWrap = true;
            // 
            // fView2
            // 
            this.fView2.AutoScroll = true;
            this.fView2.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.fView2.BorderWidth = 0;
            this.fView2.LinkColor = System.Drawing.Color.Blue;
            this.fView2.Location = new System.Drawing.Point(482, 63);
            this.fView2.Name = "fView2";
            this.fView2.Size = new System.Drawing.Size(460, 302);
            this.fView2.TabIndex = 18;
            this.fView2.TabStop = true;
            this.fView2.WordWrap = true;
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
            this.pageMergeOptions.Size = new System.Drawing.Size(957, 469);
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
            this.grpSearchPersons.Padding = new System.Windows.Forms.Padding(10);
            this.grpSearchPersons.Size = new System.Drawing.Size(315, 236);
            this.grpSearchPersons.TabIndex = 1;
            this.grpSearchPersons.TabStop = false;
            this.grpSearchPersons.Text = "grpSearchPersons";
            // 
            // lblNameAccuracy
            // 
            this.lblNameAccuracy.Location = new System.Drawing.Point(20, 56);
            this.lblNameAccuracy.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.lblNameAccuracy.Name = "lblNameAccuracy";
            this.lblNameAccuracy.Size = new System.Drawing.Size(285, 20);
            this.lblNameAccuracy.TabIndex = 0;
            this.lblNameAccuracy.Text = "lblNameAccuracy";
            // 
            // lblYearInaccuracy
            // 
            this.lblYearInaccuracy.Location = new System.Drawing.Point(20, 154);
            this.lblYearInaccuracy.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.lblYearInaccuracy.Name = "lblYearInaccuracy";
            this.lblYearInaccuracy.Size = new System.Drawing.Size(285, 20);
            this.lblYearInaccuracy.TabIndex = 1;
            this.lblYearInaccuracy.Text = "lblYearInaccuracy";
            // 
            // chkIndistinctMatching
            // 
            this.chkIndistinctMatching.Location = new System.Drawing.Point(20, 27);
            this.chkIndistinctMatching.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkIndistinctMatching.Name = "chkIndistinctMatching";
            this.chkIndistinctMatching.Size = new System.Drawing.Size(285, 24);
            this.chkIndistinctMatching.TabIndex = 1;
            this.chkIndistinctMatching.Text = "chkIndistinctMatching";
            // 
            // edNameAccuracy
            // 
            this.edNameAccuracy.Location = new System.Drawing.Point(20, 81);
            this.edNameAccuracy.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
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
            this.edYearInaccuracy.Location = new System.Drawing.Point(20, 179);
            this.edYearInaccuracy.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
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
            this.chkBirthYear.Location = new System.Drawing.Point(20, 129);
            this.chkBirthYear.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkBirthYear.Name = "chkBirthYear";
            this.chkBirthYear.Size = new System.Drawing.Size(285, 20);
            this.chkBirthYear.TabIndex = 6;
            this.chkBirthYear.Text = "chkBirthYear";
            // 
            // TTRecMergeDlg
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
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
