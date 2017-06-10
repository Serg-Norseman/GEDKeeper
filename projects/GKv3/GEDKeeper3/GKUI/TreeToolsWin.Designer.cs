using System;
using Eto.Drawing;
using Eto.Forms;
using GKCommon.GEDCOM;

namespace GKUI
{
    partial class TreeToolsWin
    {
        private TabControl tabsTools;
        private TabPage pageTreeCompare;
        private GKUI.Components.TextBoxEx ListCompare;
        private Button btnClose;
        private Label lblFile;
        private TextBox txtCompareFile;
        private Button btnFileChoose;
        private TabPage pageTreeMerge;
        private TabPage pageTreeSplit;
        private Button btnSelectAll;
        private ListBox ListSelected;
        private ListBox ListSkipped;
        private Button btnSelectFamily;
        private Button btnSelectAncestors;
        private Button btnSelectDescendants;
        private Button btnDelete;
        private Button btnSave;
        private TabPage pageRecMerge;
        private TabControl PageControl1;
        private TabPage pageMerge;
        private Button btnAutoSearch;
        private Button btnSkip;
        private ProgressBar ProgressBar1;
        private TabPage pageMergeOptions;
        private GroupBox rgMode;
        private GroupBox grpSearchPersons;
        private Label lblNameAccuracy;
        private Label lblYearInaccuracy;
        private NumericUpDown edNameAccuracy;
        private NumericUpDown edYearInaccuracy;
        private CheckBox chkBirthYear;
        private TabPage pageFamilyGroups;
        private TreeView tvGroups;
        private TabPage pageTreeCheck;
        private Button btnBaseRepair;
        private Panel Panel1;
        private Label lblMasterBase;
        private TextBox edMasterBase;
        private Label lblOtherBase;
        private TextBox edUpdateBase;
        private Button btnTreeMerge;
        private GKUI.Components.TextBoxEx mSyncRes;
        private TabPage pagePatSearch;
        private Button btnPatSearch;
        private Panel Panel3;
        private Label lblMinGenerations;
        private NumericUpDown edMinGens;
        private TabPage pagePlaceManage;
        private Panel Panel4;
        private Button btnSetPatriarch;
        private Button btnIntoList;
        private RadioButton radPersons;
        private RadioButton radNotes;
        private RadioButton radFamilies;
        private RadioButton radSources;
        private CheckBox chkBookmarkMerged;
        private GroupBox grpMergeOther;
        private GKUI.Components.LogChart gkLogChart1;
        private GKUI.Components.GKMergeControl MergeCtl;
        private Button btnPatriarchsDiagram;
        private CheckBox chkIndistinctMatching;
        private RadioButton radAnalysis;
        private Button btnMatch;
        private RadioButton radMathExternal;
        private RadioButton radMatchInternal;
        private GroupBox grpMatchType;
        private CheckBox chkWithoutDates;

        private void InitializeComponent()
        {
            tabsTools = new TabControl();
            pageTreeCompare = new TabPage();
            btnMatch = new Button();
            grpMatchType = new GroupBox();
            radAnalysis = new RadioButton();
            lblFile = new Label();
            txtCompareFile = new TextBox();
            btnFileChoose = new Button();
            radMatchInternal = new RadioButton();
            radMathExternal = new RadioButton();
            ListCompare = new GKUI.Components.TextBoxEx();
            pageTreeMerge = new TabPage();
            lblMasterBase = new Label();
            lblOtherBase = new Label();
            edMasterBase = new TextBox();
            edUpdateBase = new TextBox();
            btnTreeMerge = new Button();
            mSyncRes = new GKUI.Components.TextBoxEx();
            pageTreeSplit = new TabPage();
            btnSelectAll = new Button();
            ListSelected = new ListBox();
            ListSkipped = new ListBox();
            btnSelectFamily = new Button();
            btnSelectAncestors = new Button();
            btnSelectDescendants = new Button();
            btnDelete = new Button();
            btnSave = new Button();
            pageRecMerge = new TabPage();
            PageControl1 = new TabControl();
            pageMerge = new TabPage();
            MergeCtl = new GKUI.Components.GKMergeControl();
            btnAutoSearch = new Button();
            btnSkip = new Button();
            ProgressBar1 = new ProgressBar();
            pageMergeOptions = new TabPage();
            grpMergeOther = new GroupBox();
            chkBookmarkMerged = new CheckBox();
            rgMode = new GroupBox();
            radSources = new RadioButton();
            radFamilies = new RadioButton();
            radNotes = new RadioButton();
            radPersons = new RadioButton();
            grpSearchPersons = new GroupBox();
            lblNameAccuracy = new Label();
            lblYearInaccuracy = new Label();
            chkIndistinctMatching = new CheckBox();
            edNameAccuracy = new NumericUpDown();
            edYearInaccuracy = new NumericUpDown();
            chkBirthYear = new CheckBox();
            pageFamilyGroups = new TabPage();
            gkLogChart1 = new GKUI.Components.LogChart();
            tvGroups = new TreeView();
            pageTreeCheck = new TabPage();
            btnBaseRepair = new Button();
            Panel1 = new Panel();
            pagePatSearch = new TabPage();
            btnPatriarchsDiagram = new Button();
            chkWithoutDates = new CheckBox();
            lblMinGenerations = new Label();
            btnPatSearch = new Button();
            Panel3 = new Panel();
            edMinGens = new NumericUpDown();
            btnSetPatriarch = new Button();
            pagePlaceManage = new TabPage();
            Panel4 = new Panel();
            btnIntoList = new Button();
            btnClose = new Button();
            tabsTools.SuspendLayout();
            pageTreeCompare.SuspendLayout();
            grpMatchType.SuspendLayout();
            pageTreeMerge.SuspendLayout();
            pageTreeSplit.SuspendLayout();
            pageRecMerge.SuspendLayout();
            PageControl1.SuspendLayout();
            pageMerge.SuspendLayout();
            pageMergeOptions.SuspendLayout();
            grpMergeOther.SuspendLayout();
            rgMode.SuspendLayout();
            grpSearchPersons.SuspendLayout();
            pageFamilyGroups.SuspendLayout();
            pageTreeCheck.SuspendLayout();
            pagePatSearch.SuspendLayout();
            pagePlaceManage.SuspendLayout();
            SuspendLayout();

            tabsTools.Controls.Add(pageTreeCompare);
            tabsTools.Controls.Add(pageTreeMerge);
            tabsTools.Controls.Add(pageTreeSplit);
            tabsTools.Controls.Add(pageRecMerge);
            tabsTools.Controls.Add(pageFamilyGroups);
            tabsTools.Controls.Add(pageTreeCheck);
            tabsTools.Controls.Add(pagePatSearch);
            tabsTools.Controls.Add(pagePlaceManage);
            tabsTools.Location = new Point(11, 10);
            tabsTools.SelectedIndex = 0;
            tabsTools.Size = new Size(1010, 545);
            tabsTools.SelectedIndexChanged += tabsTools_SelectedIndexChanged;

            pageTreeCompare.Controls.Add(btnMatch);
            pageTreeCompare.Controls.Add(grpMatchType);
            pageTreeCompare.Controls.Add(ListCompare);
            pageTreeCompare.Location = new Point(4, 26);
            pageTreeCompare.Size = new Size(1002, 515);
            pageTreeCompare.Text = "pageTreeCompare";

            btnMatch.Location = new Point(874, 13);
            btnMatch.Size = new Size(113, 31);
            btnMatch.Text = "btnMatch";
            btnMatch.Click += btnMatch_Click;

            grpMatchType.Controls.Add(radAnalysis);
            grpMatchType.Controls.Add(lblFile);
            grpMatchType.Controls.Add(txtCompareFile);
            grpMatchType.Controls.Add(btnFileChoose);
            grpMatchType.Controls.Add(radMatchInternal);
            grpMatchType.Controls.Add(radMathExternal);
            grpMatchType.Location = new Point(11, 4);
            grpMatchType.Size = new Size(563, 139);
            grpMatchType.Text = "grpMatchType";

            radAnalysis.Location = new Point(22, 109);
            radAnalysis.Size = new Size(220, 21);
            radAnalysis.Text = "radAnalysis";
            radAnalysis.CheckedChanged += rbtnMatch_CheckedChanged;

            lblFile.Enabled = false;
            lblFile.Location = new Point(22, 80);
            lblFile.Size = new Size(41, 17);
            lblFile.Text = "lblFile";

            txtCompareFile.Enabled = false;
            txtCompareFile.Location = new Point(80, 77);
            txtCompareFile.ReadOnly = true;
            txtCompareFile.Size = new Size(339, 24);

            btnFileChoose.Enabled = false;
            btnFileChoose.Location = new Point(425, 73);
            btnFileChoose.Size = new Size(113, 30);
            btnFileChoose.Text = "btnFileChoose";
            btnFileChoose.Click += btnFileChoose_Click;

            radMatchInternal.Checked = true;
            radMatchInternal.Location = new Point(22, 19);
            radMatchInternal.Size = new Size(311, 21);
            radMatchInternal.Text = "radMatchInternal";
            radMatchInternal.CheckedChanged += rbtnMatch_CheckedChanged;

            radMathExternal.Location = new Point(22, 49);
            radMathExternal.Size = new Size(200, 21);
            radMathExternal.Text = "radMathExternal";
            radMathExternal.CheckedChanged += rbtnMatch_CheckedChanged;

            ListCompare.Location = new Point(11, 151);
            ListCompare.ReadOnly = true;
            ListCompare.Size = new Size(976, 351);

            pageTreeMerge.Controls.Add(lblMasterBase);
            pageTreeMerge.Controls.Add(lblOtherBase);
            pageTreeMerge.Controls.Add(edMasterBase);
            pageTreeMerge.Controls.Add(edUpdateBase);
            pageTreeMerge.Controls.Add(btnTreeMerge);
            pageTreeMerge.Controls.Add(mSyncRes);
            pageTreeMerge.Location = new Point(4, 26);
            pageTreeMerge.Size = new Size(1002, 515);
            pageTreeMerge.Text = "pageTreeMerge";

            lblMasterBase.Location = new Point(11, 10);
            lblMasterBase.Size = new Size(88, 17);
            lblMasterBase.Text = "lblMasterBase";

            lblOtherBase.Location = new Point(11, 68);
            lblOtherBase.Size = new Size(122, 17);
            lblOtherBase.Text = "lblOtherBase";

            edMasterBase.BackgroundColor = SystemColors.Control;
            edMasterBase.Location = new Point(11, 29);
            edMasterBase.ReadOnly = true;
            edMasterBase.Size = new Size(853, 24);
            edMasterBase.Text = "edMasterBase";

            edUpdateBase.Location = new Point(11, 87);
            edUpdateBase.ReadOnly = true;
            edUpdateBase.Size = new Size(853, 24);

            btnTreeMerge.Location = new Point(874, 85);
            btnTreeMerge.Size = new Size(113, 30);
            btnTreeMerge.Text = "btnTreeMerge";
            btnTreeMerge.Click += btnTreeMerge_Click;

            mSyncRes.Location = new Point(11, 131);
            mSyncRes.ReadOnly = true;
            mSyncRes.Size = new Size(976, 371);

            pageTreeSplit.Controls.Add(btnSelectAll);
            pageTreeSplit.Controls.Add(ListSelected);
            pageTreeSplit.Controls.Add(ListSkipped);
            pageTreeSplit.Controls.Add(btnSelectFamily);
            pageTreeSplit.Controls.Add(btnSelectAncestors);
            pageTreeSplit.Controls.Add(btnSelectDescendants);
            pageTreeSplit.Controls.Add(btnDelete);
            pageTreeSplit.Controls.Add(btnSave);
            pageTreeSplit.Location = new Point(4, 26);
            pageTreeSplit.Size = new Size(1002, 515);
            pageTreeSplit.Text = "pageTreeSplit";

            btnSelectAll.Location = new Point(11, 427);
            btnSelectAll.Size = new Size(168, 31);
            btnSelectAll.Text = "btnSelectAll";
            btnSelectAll.Click += btnSelectAll_Click;

            ListSelected.ItemHeight = 17;
            ListSelected.Location = new Point(11, 10);
            ListSelected.Size = new Size(483, 395);

            ListSkipped.ItemHeight = 17;
            ListSkipped.Location = new Point(504, 10);
            ListSkipped.Size = new Size(483, 395);

            btnSelectFamily.Location = new Point(190, 427);
            btnSelectFamily.Size = new Size(168, 31);
            btnSelectFamily.Text = "btnSelectFamily";
            btnSelectFamily.Click += btnSelectFamily_Click;

            btnSelectAncestors.Location = new Point(11, 466);
            btnSelectAncestors.Size = new Size(168, 31);
            btnSelectAncestors.Text = "btnSelectAncestors";
            btnSelectAncestors.Click += btnSelectAncestors_Click;

            btnSelectDescendants.Location = new Point(190, 466);
            btnSelectDescendants.Size = new Size(168, 31);
            btnSelectDescendants.Text = "btnSelectDescendants";
            btnSelectDescendants.Click += btnSelectDescendants_Click;

            btnDelete.Location = new Point(840, 427);
            btnDelete.Size = new Size(147, 31);
            btnDelete.Text = "btnDelete";
            btnDelete.Click += btnDelete_Click;

            btnSave.Location = new Point(840, 466);
            btnSave.Size = new Size(147, 31);
            btnSave.Text = "btnSave";
            btnSave.Click += btnSave_Click;

            pageRecMerge.Controls.Add(PageControl1);
            pageRecMerge.Location = new Point(4, 26);
            pageRecMerge.Size = new Size(1002, 515);
            pageRecMerge.Text = "pageRecMerge";

            PageControl1.Controls.Add(pageMerge);
            PageControl1.Controls.Add(pageMergeOptions);
            PageControl1.Location = new Point(11, 10);
            PageControl1.SelectedIndex = 0;
            PageControl1.Size = new Size(965, 493);

            pageMerge.Controls.Add(MergeCtl);
            pageMerge.Controls.Add(btnAutoSearch);
            pageMerge.Controls.Add(btnSkip);
            pageMerge.Controls.Add(ProgressBar1);
            pageMerge.Location = new Point(4, 26);
            pageMerge.Size = new Size(957, 463);
            pageMerge.Text = "pageMerge";
            pageMerge.Resize += SheetMergeResize;

            MergeCtl.Base = null;
            MergeCtl.Bookmark = false;
            MergeCtl.Dock = DockStyle.Top;
            MergeCtl.Font = new Font("Tahoma", 8.25F, FontStyle.None);
            MergeCtl.Location = new Point(0, 0);
            MergeCtl.MergeMode = GKCommon.GEDCOM.GEDCOMRecordType.rtNone;
            MergeCtl.Size = new Size(957, 402);

            btnAutoSearch.Location = new Point(17, 420);
            btnAutoSearch.Size = new Size(105, 31);
            btnAutoSearch.Text = "btnAutoSearch";
            btnAutoSearch.Click += btnSearch_Click;

            btnSkip.Location = new Point(129, 420);
            btnSkip.Size = new Size(105, 31);
            btnSkip.Text = "btnSkip";
            btnSkip.Click += btnSkip_Click;

            ProgressBar1.Location = new Point(242, 420);
            ProgressBar1.Size = new Size(700, 31);
            ProgressBar1.Step = 1;

            pageMergeOptions.Controls.Add(grpMergeOther);
            pageMergeOptions.Controls.Add(rgMode);
            pageMergeOptions.Controls.Add(grpSearchPersons);
            pageMergeOptions.Location = new Point(4, 26);
            pageMergeOptions.Size = new Size(957, 463);
            pageMergeOptions.Text = "pageMergeOptions";

            grpMergeOther.Controls.Add(chkBookmarkMerged);
            grpMergeOther.Location = new Point(342, 10);
            grpMergeOther.Size = new Size(331, 118);
            grpMergeOther.Text = "grpMergeOther";

            chkBookmarkMerged.Location = new Point(6, 23);
            chkBookmarkMerged.Size = new Size(319, 24);
            chkBookmarkMerged.Text = "chkBookmarkMerged";
            chkBookmarkMerged.CheckedChanged += chkBookmarkMerged_CheckedChanged;

            rgMode.Controls.Add(radSources);
            rgMode.Controls.Add(radFamilies);
            rgMode.Controls.Add(radNotes);
            rgMode.Controls.Add(radPersons);
            rgMode.Location = new Point(11, 10);
            rgMode.Size = new Size(315, 118);
            rgMode.Text = "rgMode";

            radSources.Location = new Point(22, 87);
            radSources.Size = new Size(269, 20);
            radSources.Text = "radSources";
            radSources.Click += radMergeMode_Click;

            radFamilies.Location = new Point(22, 68);
            radFamilies.Size = new Size(269, 19);
            radFamilies.Text = "radFamilies";
            radFamilies.Click += radMergeMode_Click;

            radNotes.Location = new Point(22, 49);
            radNotes.Size = new Size(269, 19);
            radNotes.Text = "radNotes";
            radNotes.Click += radMergeMode_Click;

            radPersons.Checked = true;
            radPersons.Location = new Point(22, 29);
            radPersons.Size = new Size(269, 20);
            radPersons.Text = "radPersons";
            radPersons.Click += radMergeMode_Click;
            // 
            // grpSearchPersons
            // 
            grpSearchPersons.Controls.Add(lblNameAccuracy);
            grpSearchPersons.Controls.Add(lblYearInaccuracy);
            grpSearchPersons.Controls.Add(chkIndistinctMatching);
            grpSearchPersons.Controls.Add(edNameAccuracy);
            grpSearchPersons.Controls.Add(edYearInaccuracy);
            grpSearchPersons.Controls.Add(chkBirthYear);
            grpSearchPersons.Location = new Point(11, 136);
            grpSearchPersons.Size = new Size(315, 193);
            grpSearchPersons.Text = "grpSearchPersons";

            lblNameAccuracy.Location = new Point(22, 49);
            lblNameAccuracy.Size = new Size(152, 15);
            lblNameAccuracy.Text = "lblNameAccuracy";

            lblYearInaccuracy.Location = new Point(22, 132);
            lblYearInaccuracy.Size = new Size(152, 16);
            lblYearInaccuracy.Text = "lblYearInaccuracy";

            chkIndistinctMatching.Location = new Point(8, 24);
            chkIndistinctMatching.Size = new Size(371, 21);
            chkIndistinctMatching.Text = "chkIndistinctMatching";

            edNameAccuracy.Location = new Point(22, 68);
            edNameAccuracy.Size = new Size(152, 24);
            edNameAccuracy.Value = 90;

            edYearInaccuracy.Location = new Point(22, 152);
            edYearInaccuracy.Size = new Size(152, 24);
            edYearInaccuracy.Value = 3;

            chkBirthYear.Location = new Point(8, 109);
            chkBirthYear.Size = new Size(371, 21);
            chkBirthYear.Text = "chkBirthYear";

            pageFamilyGroups.Controls.Add(gkLogChart1);
            pageFamilyGroups.Controls.Add(tvGroups);
            pageFamilyGroups.Location = new Point(4, 26);
            pageFamilyGroups.Size = new Size(1002, 515);
            pageFamilyGroups.Text = "pageFamilyGroups";

            gkLogChart1.Location = new Point(11, 461);
            gkLogChart1.Size = new Size(976, 34);

            tvGroups.Location = new Point(11, 10);
            tvGroups.Size = new Size(976, 437);
            tvGroups.DoubleClick += tvGroups_DoubleClick;

            pageTreeCheck.Controls.Add(btnBaseRepair);
            pageTreeCheck.Controls.Add(Panel1);
            pageTreeCheck.Location = new Point(4, 26);
            pageTreeCheck.Size = new Size(1002, 515);
            pageTreeCheck.Text = "pageTreeCheck";

            btnBaseRepair.Location = new Point(784, 464);
            btnBaseRepair.Size = new Size(203, 30);
            btnBaseRepair.Text = "btnBaseRepair";
            btnBaseRepair.Click += btnBaseRepair_Click;

            Panel1.Location = new Point(0, 0);
            Panel1.Size = new Size(998, 448);

            pagePatSearch.Controls.Add(btnPatriarchsDiagram);
            pagePatSearch.Controls.Add(chkWithoutDates);
            pagePatSearch.Controls.Add(lblMinGenerations);
            pagePatSearch.Controls.Add(btnPatSearch);
            pagePatSearch.Controls.Add(Panel3);
            pagePatSearch.Controls.Add(edMinGens);
            pagePatSearch.Controls.Add(btnSetPatriarch);
            pagePatSearch.Location = new Point(4, 26);
            pagePatSearch.Size = new Size(1002, 515);
            pagePatSearch.Text = "pagePatSearch";

            btnPatriarchsDiagram.Location = new Point(871, 464);
            btnPatriarchsDiagram.Size = new Size(105, 30);
            btnPatriarchsDiagram.Text = "btnPatriarchsDiagram";
            btnPatriarchsDiagram.Click += btnPatriarchsDiagram_Click;

            chkWithoutDates.CheckAlign = System.Drawing.ContentAlignment.MiddleRight;
            chkWithoutDates.Location = new Point(362, 467);
            chkWithoutDates.Size = new Size(158, 21);
            chkWithoutDates.Text = "chkWithoutDates";

            lblMinGenerations.Location = new Point(16, 468);
            lblMinGenerations.Size = new Size(207, 17);
            lblMinGenerations.Text = "lblMinGenerations";

            btnPatSearch.Location = new Point(757, 464);
            btnPatSearch.Size = new Size(105, 30);
            btnPatSearch.Text = "btnPatSearch";
            btnPatSearch.Click += btnPatSearch_Click;

            Panel3.Location = new Point(0, 0);
            Panel3.Size = new Size(998, 448);

            edMinGens.Location = new Point(258, 466);
            edMinGens.Size = new Size(79, 24);
            edMinGens.Value = 2;

            btnSetPatriarch.Location = new Point(577, 464);
            btnSetPatriarch.Size = new Size(172, 30);
            btnSetPatriarch.Text = "btnSetPatriarch";
            btnSetPatriarch.Click += btnSetPatriarch_Click;

            pagePlaceManage.Controls.Add(Panel4);
            pagePlaceManage.Controls.Add(btnIntoList);
            pagePlaceManage.Location = new Point(4, 26);
            pagePlaceManage.Size = new Size(1002, 515);
            pagePlaceManage.Text = "pagePlaceManage";

            Panel4.Location = new Point(0, 0);
            Panel4.Size = new Size(998, 448);

            btnIntoList.Location = new Point(11, 466);
            btnIntoList.Size = new Size(179, 31);
            btnIntoList.Text = "btnIntoList";
            btnIntoList.Click += btnIntoList_Click;

            btnClose.ImagePosition = ButtonImagePosition.Left;
            btnClose.Location = new Point(907, 583);
            btnClose.Size = new Size(114, 30);
            btnClose.Text = "btnClose";

            AbortButton = btnClose;
            ClientSize = new Size(1034, 625);
            Controls.Add(tabsTools);
            Controls.Add(btnClose);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "TreeToolsWin";
            tabsTools.ResumeLayout();
            pageTreeCompare.ResumeLayout();
            grpMatchType.ResumeLayout();
            pageTreeMerge.ResumeLayout();
            pageTreeSplit.ResumeLayout();
            pageRecMerge.ResumeLayout();
            PageControl1.ResumeLayout();
            pageMerge.ResumeLayout();
            pageMergeOptions.ResumeLayout();
            grpMergeOther.ResumeLayout();
            rgMode.ResumeLayout();
            grpSearchPersons.ResumeLayout();
            pageFamilyGroups.ResumeLayout();
            pageTreeCheck.ResumeLayout();
            pagePatSearch.ResumeLayout();
            pagePlaceManage.ResumeLayout();
            ResumeLayout();
        }
    }
}
