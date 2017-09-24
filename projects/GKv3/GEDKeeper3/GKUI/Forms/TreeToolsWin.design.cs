using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
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
            SuspendLayout();

            radMatchInternal = new RadioButton();
            radMatchInternal.Checked = true;
            radMatchInternal.Text = "radMatchInternal";
            radMatchInternal.CheckedChanged += rbtnMatch_CheckedChanged;

            radMathExternal = new RadioButton(radMatchInternal);
            radMathExternal.Text = "radMathExternal";
            radMathExternal.CheckedChanged += rbtnMatch_CheckedChanged;

            radAnalysis = new RadioButton(radMatchInternal);
            radAnalysis.Text = "radAnalysis";
            radAnalysis.CheckedChanged += rbtnMatch_CheckedChanged;

            lblFile = new Label();
            lblFile.Enabled = false;
            lblFile.Text = "lblFile";

            txtCompareFile = new TextBox();
            txtCompareFile.Enabled = false;
            txtCompareFile.ReadOnly = true;
            txtCompareFile.Width = 500;

            btnFileChoose = new Button();
            btnFileChoose.Enabled = false;
            btnFileChoose.Size = new Size(130, 26);
            btnFileChoose.Text = "btnFileChoose";
            btnFileChoose.Click += btnFileChoose_Click;

            btnMatch = new Button();
            btnMatch.Size = new Size(130, 26);
            btnMatch.Text = "btnMatch";
            btnMatch.Click += btnMatch_Click;

            grpMatchType = new GroupBox();
            grpMatchType.Text = "grpMatchType";
            grpMatchType.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { radMatchInternal }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { TableLayout.Horizontal(10, radMathExternal, txtCompareFile, null, btnFileChoose) }
                    },
                    new TableRow {
                        Cells = { TableLayout.Horizontal(10, radAnalysis, null, btnMatch) }
                    }
                }
            };

            ListCompare = new GKUI.Components.TextBoxEx();
            ListCompare.ReadOnly = true;

            pageTreeCompare = new TabPage();
            pageTreeCompare.Text = "pageTreeCompare";
            pageTreeCompare.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { grpMatchType }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { ListCompare }
                    }
                }
            };

            //

            lblMasterBase = new Label();
            //lblMasterBase.Size = new Size(88, 17);
            lblMasterBase.Text = "lblMasterBase";

            lblOtherBase = new Label();
            //lblOtherBase.Size = new Size(122, 17);
            lblOtherBase.Text = "lblOtherBase";

            edMasterBase = new TextBox();
            edMasterBase.BackgroundColor = SystemColors.Control;
            edMasterBase.ReadOnly = true;
            //edMasterBase.Size = new Size(853, 24);
            edMasterBase.Text = "edMasterBase";

            edUpdateBase = new TextBox();
            edUpdateBase.ReadOnly = true;
            edUpdateBase.Width = 600;

            btnTreeMerge = new Button();
            btnTreeMerge.Size = new Size(130, 26);
            btnTreeMerge.Text = "btnTreeMerge";
            btnTreeMerge.Click += btnTreeMerge_Click;

            mSyncRes = new GKUI.Components.TextBoxEx();
            mSyncRes.ReadOnly = true;

            pageTreeMerge = new TabPage();
            pageTreeMerge.Text = "pageTreeMerge";
            pageTreeMerge.Content = new DefTableLayout {
                Rows = {
                    new DefTableLayout {
                        Rows = {
                            new TableRow {
                                Cells = { lblMasterBase, edMasterBase }
                            },
                            new TableRow {
                                Cells = { lblOtherBase, TableLayout.Horizontal(10, edUpdateBase, btnTreeMerge) }
                            }
                        }
                    },
                    mSyncRes
                }
            };

            //

            ListSelected = new ListBox();

            ListSkipped = new ListBox();

            btnSelectAll = new Button();
            btnSelectAll.Size = new Size(130, 26);
            btnSelectAll.Text = "btnSelectAll";
            btnSelectAll.Click += btnSelectAll_Click;

            btnSelectFamily = new Button();
            btnSelectFamily.Size = new Size(130, 26);
            btnSelectFamily.Text = "btnSelectFamily";
            btnSelectFamily.Click += btnSelectFamily_Click;

            btnSelectAncestors = new Button();
            btnSelectAncestors.Size = new Size(130, 26);
            btnSelectAncestors.Text = "btnSelectAncestors";
            btnSelectAncestors.Click += btnSelectAncestors_Click;

            btnSelectDescendants = new Button();
            btnSelectDescendants.Size = new Size(130, 26);
            btnSelectDescendants.Text = "btnSelectDescendants";
            btnSelectDescendants.Click += btnSelectDescendants_Click;

            btnDelete = new Button();
            btnDelete.Size = new Size(130, 26);
            btnDelete.Text = "btnDelete";
            btnDelete.Click += btnDelete_Click;

            btnSave = new Button();
            btnSave.Size = new Size(130, 26);
            btnSave.Text = "btnSave";
            btnSave.Click += btnSave_Click;

            pageTreeSplit = new TabPage();
            pageTreeSplit.Text = "pageTreeSplit";
            pageTreeSplit.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { TableLayout.HorizontalScaled(10, ListSelected, ListSkipped) }
                    },
                    UIHelper.MakeDialogFooter(btnSelectAll, btnSelectFamily, btnSelectAncestors, btnSelectDescendants, null, btnSave, btnDelete)
                }
            };

            //

            MergeCtl = new GKUI.Components.GKMergeControl();
            MergeCtl.Base = null;
            MergeCtl.Bookmark = false;
            MergeCtl.MergeMode = GKCommon.GEDCOM.GEDCOMRecordType.rtNone;
            //MergeCtl.Size = new Size(957, 402);

            btnAutoSearch = new Button();
            btnAutoSearch.Size = new Size(130, 26);
            btnAutoSearch.Text = "btnAutoSearch";
            btnAutoSearch.Click += btnSearch_Click;

            btnSkip = new Button();
            btnSkip.Size = new Size(130, 26);
            btnSkip.Text = "btnSkip";
            btnSkip.Click += btnSkip_Click;

            ProgressBar1 = new ProgressBar();
            ProgressBar1.Size = new Size(700, 26);
            //ProgressBar1.Step = 1;

            pageMerge = new TabPage();
            pageMerge.Text = "pageMerge";
            pageMerge.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { MergeCtl }
                    },
                    UIHelper.MakeDialogFooter(btnAutoSearch, btnSkip, null, ProgressBar1)
                }
            };

            //

            radPersons = new RadioButton();
            radPersons.Checked = true;
            radPersons.Text = "radPersons";
            radPersons.Click += radMergeMode_Click;

            radFamilies = new RadioButton(radPersons);
            radFamilies.Text = "radFamilies";
            radFamilies.Click += radMergeMode_Click;

            radNotes = new RadioButton(radPersons);
            radNotes.Text = "radNotes";
            radNotes.Click += radMergeMode_Click;

            radSources = new RadioButton(radPersons);
            radSources.Text = "radSources";
            radSources.Click += radMergeMode_Click;

            rgMode = new GroupBox();
            //rgMode.Size = new Size(315, 118);
            rgMode.Text = "rgMode";
            rgMode.Content = new VDefStackLayout {
                Items = { radPersons, radFamilies, radNotes, radSources }
            };

            lblNameAccuracy = new Label();
            //lblNameAccuracy.Size = new Size(152, 15);
            lblNameAccuracy.Text = "lblNameAccuracy";

            lblYearInaccuracy = new Label();
            //lblYearInaccuracy.Size = new Size(152, 16);
            lblYearInaccuracy.Text = "lblYearInaccuracy";

            chkIndistinctMatching = new CheckBox();
            //chkIndistinctMatching.Size = new Size(371, 21);
            chkIndistinctMatching.Text = "chkIndistinctMatching";

            edNameAccuracy = new NumericUpDown();
            //edNameAccuracy.Size = new Size(152, 24);
            edNameAccuracy.Value = 90;

            edYearInaccuracy = new NumericUpDown();
            //edYearInaccuracy.Size = new Size(152, 24);
            edYearInaccuracy.Value = 3;

            chkBirthYear = new CheckBox();
            //chkBirthYear.Size = new Size(371, 21);
            chkBirthYear.Text = "chkBirthYear";

            grpSearchPersons = new GroupBox();
            //grpSearchPersons.Size = new Size(315, 193);
            grpSearchPersons.Text = "grpSearchPersons";
            grpSearchPersons.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { chkIndistinctMatching, null }
                    },
                    new TableRow {
                        Cells = { lblNameAccuracy, edNameAccuracy }
                    },
                    new TableRow {
                        Cells = { chkBirthYear, null }
                    },
                    new TableRow {
                        Cells = { lblYearInaccuracy, edYearInaccuracy }
                    },
                    null
                }
            };

            chkBookmarkMerged = new CheckBox();
            //chkBookmarkMerged.Size = new Size(319, 24);
            chkBookmarkMerged.Text = "chkBookmarkMerged";
            chkBookmarkMerged.CheckedChanged += chkBookmarkMerged_CheckedChanged;

            grpMergeOther = new GroupBox();
            //grpMergeOther.Size = new Size(331, 118);
            grpMergeOther.Text = "grpMergeOther";
            grpMergeOther.Content = chkBookmarkMerged;

            pageMergeOptions = new TabPage();
            pageMergeOptions.Text = "pageMergeOptions";
            pageMergeOptions.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { rgMode, grpSearchPersons }
                    },
                    new TableRow {
                        Cells = { grpMergeOther, null }
                    },
                    null
                }
            };

            PageControl1 = new TabControl();
            PageControl1.Pages.Add(pageMerge);
            PageControl1.Pages.Add(pageMergeOptions);

            pageRecMerge = new TabPage();
            pageRecMerge.Content = PageControl1;
            pageRecMerge.Text = "pageRecMerge";

            //

            gkLogChart1 = new GKUI.Components.LogChart();
            gkLogChart1.Height = 34;
            gkLogChart1.OnHintRequest += HintRequestEventHandler;

            tvGroups = new TreeView();
            tvGroups.LabelEdit = false;
            tvGroups.MouseDoubleClick += tvGroups_DoubleClick;

            pageFamilyGroups = new TabPage();
            pageFamilyGroups.Text = "pageFamilyGroups";
            pageFamilyGroups.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tvGroups }
                    },
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { gkLogChart1 }
                    }
                }
            };

            //

            btnBaseRepair = new Button();
            btnBaseRepair.Size = new Size(130, 26);
            btnBaseRepair.Text = "btnBaseRepair";
            btnBaseRepair.Click += btnBaseRepair_Click;

            Panel1 = new Panel();

            pageTreeCheck = new TabPage();
            pageTreeCheck.Text = "pageTreeCheck";
            pageTreeCheck.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { Panel1 }
                    },
                    UIHelper.MakeDialogFooter(null, btnBaseRepair)
                }
            };

            //

            btnPatriarchsDiagram = new Button();
            btnPatriarchsDiagram.Size = new Size(130, 26);
            btnPatriarchsDiagram.Text = "btnPatriarchsDiagram";
            btnPatriarchsDiagram.Click += btnPatriarchsDiagram_Click;

            chkWithoutDates = new CheckBox();
            chkWithoutDates.Text = "chkWithoutDates";

            lblMinGenerations = new Label();
            lblMinGenerations.Text = "lblMinGenerations";

            btnPatSearch = new Button();
            btnPatSearch.Size = new Size(130, 26);
            btnPatSearch.Text = "btnPatSearch";
            btnPatSearch.Click += btnPatSearch_Click;

            Panel3 = new Panel();

            edMinGens = new NumericUpDown();
            edMinGens.Value = 2;

            btnSetPatriarch = new Button();
            btnSetPatriarch.Size = new Size(130, 26);
            btnSetPatriarch.Text = "btnSetPatriarch";
            btnSetPatriarch.Click += btnSetPatriarch_Click;

            pagePatSearch = new TabPage();
            pagePatSearch.Text = "pagePatSearch";
            pagePatSearch.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { Panel3 }
                    },
                    UIHelper.MakeDialogFooter(lblMinGenerations, edMinGens, chkWithoutDates, null, btnSetPatriarch, btnPatSearch, btnPatriarchsDiagram)
                }
            };

            //

            Panel4 = new Panel();
            //Panel4.Size = new Size(998, 448);

            btnIntoList = new Button();
            btnIntoList.Size = new Size(160, 26);
            btnIntoList.Text = "btnIntoList";
            btnIntoList.Click += btnIntoList_Click;

            pagePlaceManage = new TabPage();
            pagePlaceManage.Text = "pagePlaceManage";
            pagePlaceManage.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { Panel4 }
                    },
                    UIHelper.MakeDialogFooter(null, btnIntoList)
                }
            };

            //

            tabsTools = new TabControl();
            tabsTools.Pages.Add(pageTreeCompare);
            tabsTools.Pages.Add(pageTreeMerge);
            tabsTools.Pages.Add(pageTreeSplit);
            tabsTools.Pages.Add(pageRecMerge);
            tabsTools.Pages.Add(pageFamilyGroups);
            tabsTools.Pages.Add(pageTreeCheck);
            tabsTools.Pages.Add(pagePatSearch);
            tabsTools.Pages.Add(pagePlaceManage);
            tabsTools.SelectedIndex = 0;
            tabsTools.SelectedIndexChanged += tabsTools_SelectedIndexChanged;

            btnClose = new Button();
            btnClose.ImagePosition = ButtonImagePosition.Left;
            btnClose.Size = new Size(130, 26);
            btnClose.Text = "btnClose";
            btnClose.Click += (sender, e) => { Close(); };

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsTools }
                    },
                    UIHelper.MakeDialogFooter(null, btnClose)
                }
            };

            AbortButton = btnClose;
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "TreeToolsWin";

            UIHelper.SetPredefProperties(this, 1030, 620);
            ResumeLayout();
        }
    }
}
