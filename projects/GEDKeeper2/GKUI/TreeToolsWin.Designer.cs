using System;
using GKCommon.GEDCOM;

namespace GKUI
{
	partial class TreeToolsWin
	{
		private System.Windows.Forms.TabControl tabsTools;
		private System.Windows.Forms.TabPage pageTreeCompare;
		private System.Windows.Forms.TextBox ListCompare;
		private System.Windows.Forms.Button btnClose;
		private System.Windows.Forms.Label lblFile;
		private System.Windows.Forms.TextBox txtCompareFile;
		private System.Windows.Forms.Button btnFileChoose;
		private System.Windows.Forms.TabPage pageTreeMerge;
		private System.Windows.Forms.TabPage pageTreeSplit;
		private System.Windows.Forms.Button btnSelectAll;
		private System.Windows.Forms.ListBox ListSelected;
		private System.Windows.Forms.ListBox ListSkipped;
		private System.Windows.Forms.Button btnSelectFamily;
		private System.Windows.Forms.Button btnSelectAncestors;
		private System.Windows.Forms.Button btnSelectDescendants;
		private System.Windows.Forms.Button btnDelete;
		private System.Windows.Forms.Button btnSave;
		private System.Windows.Forms.TabPage pageRecMerge;
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
		private System.Windows.Forms.TabPage pageFamilyGroups;
		private System.Windows.Forms.TreeView TreeView1;
		private System.Windows.Forms.TabPage pageTreeCheck;
		private System.Windows.Forms.Button btnBaseRepair;
		private System.Windows.Forms.Panel Panel1;
		private System.Windows.Forms.Label lblMasterBase;
		private System.Windows.Forms.TextBox edMasterBase;
		private System.Windows.Forms.Label lblOtherBase;
		private System.Windows.Forms.TextBox edUpdateBase;
		private System.Windows.Forms.Button btnTreeMerge;
		private System.Windows.Forms.TextBox mSyncRes;
		private System.Windows.Forms.TabPage pagePatSearch;
		private System.Windows.Forms.Button btnPatSearch;
		private System.Windows.Forms.Panel Panel3;
		private System.Windows.Forms.Label lblMinGenerations;
		private System.Windows.Forms.NumericUpDown edMinGens;
		private System.Windows.Forms.TabPage pagePlaceManage;
		private System.Windows.Forms.Panel Panel4;
		private System.Windows.Forms.Button btnSetPatriarch;
		private System.Windows.Forms.Button btnIntoList;
		private System.Windows.Forms.RadioButton radPersons;
		private System.Windows.Forms.RadioButton radNotes;
		private System.Windows.Forms.RadioButton radFamilies;
		private System.Windows.Forms.RadioButton radSources;
		private System.Windows.Forms.CheckBox chkBookmarkMerged;
		private System.Windows.Forms.GroupBox grpMergeOther;
		private GKCommon.Controls.LogChart gkLogChart1;
		private GKUI.Controls.GKMergeControl MergeCtl;
		private System.Windows.Forms.Button btnPatriarchsDiagram;
		private System.Windows.Forms.CheckBox chkIndistinctMatching;
		private System.Windows.Forms.RadioButton radAnalysis;
		private System.Windows.Forms.Button btnMatch;
		private System.Windows.Forms.RadioButton radMathExternal;
		private System.Windows.Forms.RadioButton radMatchInternal;
		private System.Windows.Forms.GroupBox grpMatchType;
		private System.Windows.Forms.CheckBox chkWithoutDates;

		private void InitializeComponent()
		{
		    System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(TreeToolsWin));
		    this.tabsTools = new System.Windows.Forms.TabControl();
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
		    this.pageTreeMerge = new System.Windows.Forms.TabPage();
		    this.lblMasterBase = new System.Windows.Forms.Label();
		    this.lblOtherBase = new System.Windows.Forms.Label();
		    this.edMasterBase = new System.Windows.Forms.TextBox();
		    this.edUpdateBase = new System.Windows.Forms.TextBox();
		    this.btnTreeMerge = new System.Windows.Forms.Button();
		    this.mSyncRes = new System.Windows.Forms.TextBox();
		    this.pageTreeSplit = new System.Windows.Forms.TabPage();
		    this.btnSelectAll = new System.Windows.Forms.Button();
		    this.ListSelected = new System.Windows.Forms.ListBox();
		    this.ListSkipped = new System.Windows.Forms.ListBox();
		    this.btnSelectFamily = new System.Windows.Forms.Button();
		    this.btnSelectAncestors = new System.Windows.Forms.Button();
		    this.btnSelectDescendants = new System.Windows.Forms.Button();
		    this.btnDelete = new System.Windows.Forms.Button();
		    this.btnSave = new System.Windows.Forms.Button();
		    this.pageRecMerge = new System.Windows.Forms.TabPage();
		    this.PageControl1 = new System.Windows.Forms.TabControl();
		    this.pageMerge = new System.Windows.Forms.TabPage();
		    this.MergeCtl = new GKUI.Controls.GKMergeControl();
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
		    this.pageFamilyGroups = new System.Windows.Forms.TabPage();
		    this.gkLogChart1 = new GKCommon.Controls.LogChart();
		    this.TreeView1 = new System.Windows.Forms.TreeView();
		    this.pageTreeCheck = new System.Windows.Forms.TabPage();
		    this.btnBaseRepair = new System.Windows.Forms.Button();
		    this.Panel1 = new System.Windows.Forms.Panel();
		    this.pagePatSearch = new System.Windows.Forms.TabPage();
		    this.btnPatriarchsDiagram = new System.Windows.Forms.Button();
		    this.chkWithoutDates = new System.Windows.Forms.CheckBox();
		    this.lblMinGenerations = new System.Windows.Forms.Label();
		    this.btnPatSearch = new System.Windows.Forms.Button();
		    this.Panel3 = new System.Windows.Forms.Panel();
		    this.edMinGens = new System.Windows.Forms.NumericUpDown();
		    this.btnSetPatriarch = new System.Windows.Forms.Button();
		    this.pagePlaceManage = new System.Windows.Forms.TabPage();
		    this.Panel4 = new System.Windows.Forms.Panel();
		    this.btnIntoList = new System.Windows.Forms.Button();
		    this.btnClose = new System.Windows.Forms.Button();
		    this.tabsTools.SuspendLayout();
		    this.pageTreeCompare.SuspendLayout();
		    this.grpMatchType.SuspendLayout();
		    this.pageTreeMerge.SuspendLayout();
		    this.pageTreeSplit.SuspendLayout();
		    this.pageRecMerge.SuspendLayout();
		    this.PageControl1.SuspendLayout();
		    this.pageMerge.SuspendLayout();
		    this.pageMergeOptions.SuspendLayout();
		    this.grpMergeOther.SuspendLayout();
		    this.rgMode.SuspendLayout();
		    this.grpSearchPersons.SuspendLayout();
		    ((System.ComponentModel.ISupportInitialize)(this.edNameAccuracy)).BeginInit();
		    ((System.ComponentModel.ISupportInitialize)(this.edYearInaccuracy)).BeginInit();
		    this.pageFamilyGroups.SuspendLayout();
		    this.pageTreeCheck.SuspendLayout();
		    this.pagePatSearch.SuspendLayout();
		    ((System.ComponentModel.ISupportInitialize)(this.edMinGens)).BeginInit();
		    this.pagePlaceManage.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // tabsTools
		    // 
		    this.tabsTools.Controls.Add(this.pageTreeCompare);
		    this.tabsTools.Controls.Add(this.pageTreeMerge);
		    this.tabsTools.Controls.Add(this.pageTreeSplit);
		    this.tabsTools.Controls.Add(this.pageRecMerge);
		    this.tabsTools.Controls.Add(this.pageFamilyGroups);
		    this.tabsTools.Controls.Add(this.pageTreeCheck);
		    this.tabsTools.Controls.Add(this.pagePatSearch);
		    this.tabsTools.Controls.Add(this.pagePlaceManage);
		    this.tabsTools.Location = new System.Drawing.Point(11, 10);
		    this.tabsTools.Name = "tabsTools";
		    this.tabsTools.SelectedIndex = 0;
		    this.tabsTools.Size = new System.Drawing.Size(1010, 545);
		    this.tabsTools.TabIndex = 0;
		    this.tabsTools.SelectedIndexChanged += new System.EventHandler(this.PageControl_SelectedIndexChanged);
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
		    this.radAnalysis.Size = new System.Drawing.Size(220, 21);
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
		    this.lblFile.Size = new System.Drawing.Size(41, 17);
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
		    this.radMatchInternal.Size = new System.Drawing.Size(311, 21);
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
		    this.radMathExternal.Size = new System.Drawing.Size(200, 21);
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
		    this.ListCompare.Size = new System.Drawing.Size(976, 351);
		    this.ListCompare.TabIndex = 0;
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
		    this.lblOtherBase.Size = new System.Drawing.Size(122, 17);
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
		    // pageTreeSplit
		    // 
		    this.pageTreeSplit.Controls.Add(this.btnSelectAll);
		    this.pageTreeSplit.Controls.Add(this.ListSelected);
		    this.pageTreeSplit.Controls.Add(this.ListSkipped);
		    this.pageTreeSplit.Controls.Add(this.btnSelectFamily);
		    this.pageTreeSplit.Controls.Add(this.btnSelectAncestors);
		    this.pageTreeSplit.Controls.Add(this.btnSelectDescendants);
		    this.pageTreeSplit.Controls.Add(this.btnDelete);
		    this.pageTreeSplit.Controls.Add(this.btnSave);
		    this.pageTreeSplit.Location = new System.Drawing.Point(4, 26);
		    this.pageTreeSplit.Name = "pageTreeSplit";
		    this.pageTreeSplit.Size = new System.Drawing.Size(1002, 515);
		    this.pageTreeSplit.TabIndex = 2;
		    this.pageTreeSplit.Text = "pageTreeSplit";
		    // 
		    // btnSelectAll
		    // 
		    this.btnSelectAll.Location = new System.Drawing.Point(11, 427);
		    this.btnSelectAll.Name = "btnSelectAll";
		    this.btnSelectAll.Size = new System.Drawing.Size(168, 31);
		    this.btnSelectAll.TabIndex = 0;
		    this.btnSelectAll.Text = "btnSelectAll";
		    this.btnSelectAll.Click += new System.EventHandler(this.btnSelectAll_Click);
		    // 
		    // ListSelected
		    // 
		    this.ListSelected.ItemHeight = 17;
		    this.ListSelected.Location = new System.Drawing.Point(11, 10);
		    this.ListSelected.Name = "ListSelected";
		    this.ListSelected.Size = new System.Drawing.Size(483, 395);
		    this.ListSelected.TabIndex = 1;
		    // 
		    // ListSkipped
		    // 
		    this.ListSkipped.ItemHeight = 17;
		    this.ListSkipped.Location = new System.Drawing.Point(504, 10);
		    this.ListSkipped.Name = "ListSkipped";
		    this.ListSkipped.Size = new System.Drawing.Size(483, 395);
		    this.ListSkipped.TabIndex = 2;
		    // 
		    // btnSelectFamily
		    // 
		    this.btnSelectFamily.Location = new System.Drawing.Point(190, 427);
		    this.btnSelectFamily.Name = "btnSelectFamily";
		    this.btnSelectFamily.Size = new System.Drawing.Size(168, 31);
		    this.btnSelectFamily.TabIndex = 3;
		    this.btnSelectFamily.Text = "btnSelectFamily";
		    this.btnSelectFamily.Click += new System.EventHandler(this.btnSelectFamily_Click);
		    // 
		    // btnSelectAncestors
		    // 
		    this.btnSelectAncestors.Location = new System.Drawing.Point(11, 466);
		    this.btnSelectAncestors.Name = "btnSelectAncestors";
		    this.btnSelectAncestors.Size = new System.Drawing.Size(168, 31);
		    this.btnSelectAncestors.TabIndex = 4;
		    this.btnSelectAncestors.Text = "btnSelectAncestors";
		    this.btnSelectAncestors.Click += new System.EventHandler(this.btnSelectAncestors_Click);
		    // 
		    // btnSelectDescendants
		    // 
		    this.btnSelectDescendants.Location = new System.Drawing.Point(190, 466);
		    this.btnSelectDescendants.Name = "btnSelectDescendants";
		    this.btnSelectDescendants.Size = new System.Drawing.Size(168, 31);
		    this.btnSelectDescendants.TabIndex = 5;
		    this.btnSelectDescendants.Text = "btnSelectDescendants";
		    this.btnSelectDescendants.Click += new System.EventHandler(this.btnSelectDescendants_Click);
		    // 
		    // btnDelete
		    // 
		    this.btnDelete.Location = new System.Drawing.Point(840, 427);
		    this.btnDelete.Name = "btnDelete";
		    this.btnDelete.Size = new System.Drawing.Size(147, 31);
		    this.btnDelete.TabIndex = 6;
		    this.btnDelete.Text = "btnDelete";
		    this.btnDelete.Click += new System.EventHandler(this.btnDelete_Click);
		    // 
		    // btnSave
		    // 
		    this.btnSave.Location = new System.Drawing.Point(840, 466);
		    this.btnSave.Name = "btnSave";
		    this.btnSave.Size = new System.Drawing.Size(147, 31);
		    this.btnSave.TabIndex = 7;
		    this.btnSave.Text = "btnSave";
		    this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
		    // 
		    // pageRecMerge
		    // 
		    this.pageRecMerge.Controls.Add(this.PageControl1);
		    this.pageRecMerge.Location = new System.Drawing.Point(4, 26);
		    this.pageRecMerge.Name = "pageRecMerge";
		    this.pageRecMerge.Size = new System.Drawing.Size(1002, 515);
		    this.pageRecMerge.TabIndex = 3;
		    this.pageRecMerge.Text = "pageRecMerge";
		    // 
		    // PageControl1
		    // 
		    this.PageControl1.Controls.Add(this.pageMerge);
		    this.PageControl1.Controls.Add(this.pageMergeOptions);
		    this.PageControl1.Location = new System.Drawing.Point(11, 10);
		    this.PageControl1.Name = "PageControl1";
		    this.PageControl1.SelectedIndex = 0;
		    this.PageControl1.Size = new System.Drawing.Size(965, 493);
		    this.PageControl1.TabIndex = 0;
		    // 
		    // pageMerge
		    // 
		    this.pageMerge.Controls.Add(this.MergeCtl);
		    this.pageMerge.Controls.Add(this.btnAutoSearch);
		    this.pageMerge.Controls.Add(this.btnSkip);
		    this.pageMerge.Controls.Add(this.ProgressBar1);
		    this.pageMerge.Location = new System.Drawing.Point(4, 26);
		    this.pageMerge.Name = "pageMerge";
		    this.pageMerge.Size = new System.Drawing.Size(957, 463);
		    this.pageMerge.TabIndex = 0;
		    this.pageMerge.Text = "pageMerge";
		    this.pageMerge.Resize += new System.EventHandler(this.SheetMergeResize);
		    // 
		    // MergeCtl
		    // 
		    this.MergeCtl.Base = null;
		    this.MergeCtl.Bookmark = false;
		    this.MergeCtl.Dock = System.Windows.Forms.DockStyle.Top;
		    this.MergeCtl.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.MergeCtl.Location = new System.Drawing.Point(0, 0);
		    this.MergeCtl.MergeMode = GKCommon.GEDCOM.GEDCOMRecordType.rtNone;
		    this.MergeCtl.Name = "MergeCtl";
		    this.MergeCtl.Size = new System.Drawing.Size(957, 402);
		    this.MergeCtl.TabIndex = 11;
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
		    this.pageMergeOptions.Size = new System.Drawing.Size(957, 463);
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
		    this.radSources.Click += new System.EventHandler(this.RadioButton8_Click);
		    // 
		    // radFamilies
		    // 
		    this.radFamilies.Location = new System.Drawing.Point(22, 68);
		    this.radFamilies.Name = "radFamilies";
		    this.radFamilies.Size = new System.Drawing.Size(269, 19);
		    this.radFamilies.TabIndex = 2;
		    this.radFamilies.Text = "radFamilies";
		    this.radFamilies.Click += new System.EventHandler(this.RadioButton8_Click);
		    // 
		    // radNotes
		    // 
		    this.radNotes.Location = new System.Drawing.Point(22, 49);
		    this.radNotes.Name = "radNotes";
		    this.radNotes.Size = new System.Drawing.Size(269, 19);
		    this.radNotes.TabIndex = 1;
		    this.radNotes.Text = "radNotes";
		    this.radNotes.Click += new System.EventHandler(this.RadioButton8_Click);
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
		    this.radPersons.Click += new System.EventHandler(this.RadioButton8_Click);
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
		    // pageFamilyGroups
		    // 
		    this.pageFamilyGroups.Controls.Add(this.gkLogChart1);
		    this.pageFamilyGroups.Controls.Add(this.TreeView1);
		    this.pageFamilyGroups.Location = new System.Drawing.Point(4, 26);
		    this.pageFamilyGroups.Name = "pageFamilyGroups";
		    this.pageFamilyGroups.Size = new System.Drawing.Size(1002, 515);
		    this.pageFamilyGroups.TabIndex = 5;
		    this.pageFamilyGroups.Text = "pageFamilyGroups";
		    // 
		    // gkLogChart1
		    // 
		    this.gkLogChart1.Location = new System.Drawing.Point(11, 461);
		    this.gkLogChart1.Name = "gkLogChart1";
		    this.gkLogChart1.Size = new System.Drawing.Size(976, 34);
		    this.gkLogChart1.TabIndex = 1;
		    this.gkLogChart1.TabStop = true;
		    // 
		    // TreeView1
		    // 
		    this.TreeView1.Location = new System.Drawing.Point(11, 10);
		    this.TreeView1.Name = "TreeView1";
		    this.TreeView1.Size = new System.Drawing.Size(976, 437);
		    this.TreeView1.TabIndex = 0;
		    this.TreeView1.DoubleClick += new System.EventHandler(this.TreeView1_DoubleClick);
		    // 
		    // pageTreeCheck
		    // 
		    this.pageTreeCheck.Controls.Add(this.btnBaseRepair);
		    this.pageTreeCheck.Controls.Add(this.Panel1);
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
		    // Panel1
		    // 
		    this.Panel1.Location = new System.Drawing.Point(0, 0);
		    this.Panel1.Name = "Panel1";
		    this.Panel1.Size = new System.Drawing.Size(998, 448);
		    this.Panel1.TabIndex = 1;
		    // 
		    // pagePatSearch
		    // 
		    this.pagePatSearch.Controls.Add(this.btnPatriarchsDiagram);
		    this.pagePatSearch.Controls.Add(this.chkWithoutDates);
		    this.pagePatSearch.Controls.Add(this.lblMinGenerations);
		    this.pagePatSearch.Controls.Add(this.btnPatSearch);
		    this.pagePatSearch.Controls.Add(this.Panel3);
		    this.pagePatSearch.Controls.Add(this.edMinGens);
		    this.pagePatSearch.Controls.Add(this.btnSetPatriarch);
		    this.pagePatSearch.Location = new System.Drawing.Point(4, 26);
		    this.pagePatSearch.Name = "pagePatSearch";
		    this.pagePatSearch.Size = new System.Drawing.Size(1002, 515);
		    this.pagePatSearch.TabIndex = 7;
		    this.pagePatSearch.Text = "pagePatSearch";
		    // 
		    // btnPatriarchsDiagram
		    // 
		    this.btnPatriarchsDiagram.Location = new System.Drawing.Point(871, 464);
		    this.btnPatriarchsDiagram.Name = "btnPatriarchsDiagram";
		    this.btnPatriarchsDiagram.Size = new System.Drawing.Size(105, 30);
		    this.btnPatriarchsDiagram.TabIndex = 6;
		    this.btnPatriarchsDiagram.Text = "btnPatriarchsDiagram";
		    this.btnPatriarchsDiagram.Click += new System.EventHandler(this.BtnPatriarchsDiagramClick);
		    // 
		    // chkWithoutDates
		    // 
		    this.chkWithoutDates.AutoSize = true;
		    this.chkWithoutDates.CheckAlign = System.Drawing.ContentAlignment.MiddleRight;
		    this.chkWithoutDates.Location = new System.Drawing.Point(362, 467);
		    this.chkWithoutDates.Name = "chkWithoutDates";
		    this.chkWithoutDates.Size = new System.Drawing.Size(158, 21);
		    this.chkWithoutDates.TabIndex = 5;
		    this.chkWithoutDates.Text = "chkWithoutDates";
		    this.chkWithoutDates.UseVisualStyleBackColor = true;
		    // 
		    // lblMinGenerations
		    // 
		    this.lblMinGenerations.AutoSize = true;
		    this.lblMinGenerations.Location = new System.Drawing.Point(16, 468);
		    this.lblMinGenerations.Name = "lblMinGenerations";
		    this.lblMinGenerations.Size = new System.Drawing.Size(207, 17);
		    this.lblMinGenerations.TabIndex = 0;
		    this.lblMinGenerations.Text = "lblMinGenerations";
		    // 
		    // btnPatSearch
		    // 
		    this.btnPatSearch.Location = new System.Drawing.Point(757, 464);
		    this.btnPatSearch.Name = "btnPatSearch";
		    this.btnPatSearch.Size = new System.Drawing.Size(105, 30);
		    this.btnPatSearch.TabIndex = 0;
		    this.btnPatSearch.Text = "btnPatSearch";
		    this.btnPatSearch.Click += new System.EventHandler(this.btnPatSearch_Click);
		    // 
		    // Panel3
		    // 
		    this.Panel3.Location = new System.Drawing.Point(0, 0);
		    this.Panel3.Name = "Panel3";
		    this.Panel3.Size = new System.Drawing.Size(998, 448);
		    this.Panel3.TabIndex = 1;
		    // 
		    // edMinGens
		    // 
		    this.edMinGens.Location = new System.Drawing.Point(258, 466);
		    this.edMinGens.Name = "edMinGens";
		    this.edMinGens.Size = new System.Drawing.Size(79, 24);
		    this.edMinGens.TabIndex = 2;
		    this.edMinGens.Value = new decimal(new int[] {
		    		    		    2,
		    		    		    0,
		    		    		    0,
		    		    		    0});
		    // 
		    // btnSetPatriarch
		    // 
		    this.btnSetPatriarch.Location = new System.Drawing.Point(577, 464);
		    this.btnSetPatriarch.Name = "btnSetPatriarch";
		    this.btnSetPatriarch.Size = new System.Drawing.Size(172, 30);
		    this.btnSetPatriarch.TabIndex = 4;
		    this.btnSetPatriarch.Text = "btnSetPatriarch";
		    this.btnSetPatriarch.Click += new System.EventHandler(this.btnSetPatriarch_Click);
		    // 
		    // pagePlaceManage
		    // 
		    this.pagePlaceManage.Controls.Add(this.Panel4);
		    this.pagePlaceManage.Controls.Add(this.btnIntoList);
		    this.pagePlaceManage.Location = new System.Drawing.Point(4, 26);
		    this.pagePlaceManage.Name = "pagePlaceManage";
		    this.pagePlaceManage.Size = new System.Drawing.Size(1002, 515);
		    this.pagePlaceManage.TabIndex = 8;
		    this.pagePlaceManage.Text = "pagePlaceManage";
		    // 
		    // Panel4
		    // 
		    this.Panel4.Location = new System.Drawing.Point(0, 0);
		    this.Panel4.Name = "Panel4";
		    this.Panel4.Size = new System.Drawing.Size(998, 448);
		    this.Panel4.TabIndex = 0;
		    // 
		    // btnIntoList
		    // 
		    this.btnIntoList.Location = new System.Drawing.Point(11, 466);
		    this.btnIntoList.Name = "btnIntoList";
		    this.btnIntoList.Size = new System.Drawing.Size(179, 31);
		    this.btnIntoList.TabIndex = 1;
		    this.btnIntoList.Text = "btnIntoList";
		    this.btnIntoList.Click += new System.EventHandler(this.btnIntoList_Click);
		    // 
		    // btnClose
		    // 
		    this.btnClose.DialogResult = System.Windows.Forms.DialogResult.Cancel;
		    this.btnClose.Image = ((System.Drawing.Image)(resources.GetObject("btnClose.Image")));
		    this.btnClose.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnClose.Location = new System.Drawing.Point(907, 583);
		    this.btnClose.Name = "btnClose";
		    this.btnClose.Size = new System.Drawing.Size(114, 30);
		    this.btnClose.TabIndex = 1;
		    this.btnClose.Text = "btnClose";
		    this.btnClose.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    // 
		    // TreeToolsWin
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
		    this.Name = "TreeToolsWin";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "TreeToolsWin";
		    this.tabsTools.ResumeLayout(false);
		    this.pageTreeCompare.ResumeLayout(false);
		    this.pageTreeCompare.PerformLayout();
		    this.grpMatchType.ResumeLayout(false);
		    this.grpMatchType.PerformLayout();
		    this.pageTreeMerge.ResumeLayout(false);
		    this.pageTreeMerge.PerformLayout();
		    this.pageTreeSplit.ResumeLayout(false);
		    this.pageRecMerge.ResumeLayout(false);
		    this.PageControl1.ResumeLayout(false);
		    this.pageMerge.ResumeLayout(false);
		    this.pageMergeOptions.ResumeLayout(false);
		    this.grpMergeOther.ResumeLayout(false);
		    this.rgMode.ResumeLayout(false);
		    this.grpSearchPersons.ResumeLayout(false);
		    ((System.ComponentModel.ISupportInitialize)(this.edNameAccuracy)).EndInit();
		    ((System.ComponentModel.ISupportInitialize)(this.edYearInaccuracy)).EndInit();
		    this.pageFamilyGroups.ResumeLayout(false);
		    this.pageTreeCheck.ResumeLayout(false);
		    this.pagePatSearch.ResumeLayout(false);
		    this.pagePatSearch.PerformLayout();
		    ((System.ComponentModel.ISupportInitialize)(this.edMinGens)).EndInit();
		    this.pagePlaceManage.ResumeLayout(false);
		    this.ResumeLayout(false);
		}
	}
}