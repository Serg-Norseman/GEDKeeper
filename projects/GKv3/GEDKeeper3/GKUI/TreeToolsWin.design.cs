using System;
using Eto.Drawing;
using Eto.Forms;
using GKCommon.GEDCOM;
using GKUI.Components;

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

            SuspendLayout();

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

            pageTreeCompare.Text = "pageTreeCompare";
            pageTreeCompare.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { grpMatchType, btnMatch }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { ListCompare }
                    }
                }
            };

            btnMatch.Size = new Size(80, 26);
            btnMatch.Text = "btnMatch";
            btnMatch.Click += btnMatch_Click;

            grpMatchType.Text = "grpMatchType";
            grpMatchType.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { radMatchInternal }
                    },
                    new TableRow {
                        Cells = { radMathExternal }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { lblFile, txtCompareFile, btnFileChoose }
                    },
                    new TableRow {
                        Cells = { radAnalysis }
                    }
                }
            };

            radAnalysis.Size = new Size(220, 21);
            radAnalysis.Text = "radAnalysis";
            radAnalysis.CheckedChanged += rbtnMatch_CheckedChanged;

            lblFile.Enabled = false;
            lblFile.Size = new Size(41, 17);
            lblFile.Text = "lblFile";

            txtCompareFile.Enabled = false;
            txtCompareFile.ReadOnly = true;
            txtCompareFile.Size = new Size(339, 24);

            btnFileChoose.Enabled = false;
            btnFileChoose.Size = new Size(113, 30);
            btnFileChoose.Text = "btnFileChoose";
            btnFileChoose.Click += btnFileChoose_Click;

            radMatchInternal.Checked = true;
            radMatchInternal.Size = new Size(311, 21);
            radMatchInternal.Text = "radMatchInternal";
            radMatchInternal.CheckedChanged += rbtnMatch_CheckedChanged;

            radMathExternal.Size = new Size(200, 21);
            radMathExternal.Text = "radMathExternal";
            radMathExternal.CheckedChanged += rbtnMatch_CheckedChanged;

            ListCompare.ReadOnly = true;
            ListCompare.Size = new Size(976, 351);

            //

            pageTreeMerge.Size = new Size(1002, 515);
            pageTreeMerge.Text = "pageTreeMerge";
            pageTreeMerge.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblMasterBase }
                    },
                    new TableRow {
                        Cells = { edMasterBase }
                    },
                    new TableRow {
                        Cells = { lblOtherBase }
                    },
                    new TableRow {
                        Cells = { edUpdateBase, btnTreeMerge }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { mSyncRes }
                    }
                }
            };

            lblMasterBase.Size = new Size(88, 17);
            lblMasterBase.Text = "lblMasterBase";

            lblOtherBase.Size = new Size(122, 17);
            lblOtherBase.Text = "lblOtherBase";

            edMasterBase.BackgroundColor = SystemColors.Control;
            edMasterBase.ReadOnly = true;
            edMasterBase.Size = new Size(853, 24);
            edMasterBase.Text = "edMasterBase";

            edUpdateBase.ReadOnly = true;
            edUpdateBase.Size = new Size(853, 24);

            btnTreeMerge.Size = new Size(113, 30);
            btnTreeMerge.Text = "btnTreeMerge";
            btnTreeMerge.Click += btnTreeMerge_Click;

            mSyncRes.ReadOnly = true;
            mSyncRes.Size = new Size(976, 371);

            //

            pageTreeSplit.Size = new Size(1002, 515);
            pageTreeSplit.Text = "pageTreeSplit";
            pageTreeSplit.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { ListSelected, ListSkipped }
                    },
                    new TableRow {
                        Cells = { btnSelectAll, btnSelectFamily, btnSelectAncestors, btnSelectDescendants, btnSave, btnDelete }
                    }
                }
            };

            btnSelectAll.Size = new Size(168, 31);
            btnSelectAll.Text = "btnSelectAll";
            btnSelectAll.Click += btnSelectAll_Click;

            //ListSelected.ItemHeight = 17;
            ListSelected.Size = new Size(483, 395);

            //ListSkipped.ItemHeight = 17;
            ListSkipped.Size = new Size(483, 395);

            btnSelectFamily.Size = new Size(168, 31);
            btnSelectFamily.Text = "btnSelectFamily";
            btnSelectFamily.Click += btnSelectFamily_Click;

            btnSelectAncestors.Size = new Size(168, 31);
            btnSelectAncestors.Text = "btnSelectAncestors";
            btnSelectAncestors.Click += btnSelectAncestors_Click;

            btnSelectDescendants.Size = new Size(168, 31);
            btnSelectDescendants.Text = "btnSelectDescendants";
            btnSelectDescendants.Click += btnSelectDescendants_Click;

            btnDelete.Size = new Size(147, 31);
            btnDelete.Text = "btnDelete";
            btnDelete.Click += btnDelete_Click;

            btnSave.Size = new Size(147, 31);
            btnSave.Text = "btnSave";
            btnSave.Click += btnSave_Click;

            //

            pageRecMerge.Content = PageControl1;
            pageRecMerge.Size = new Size(1002, 515);
            pageRecMerge.Text = "pageRecMerge";

            PageControl1.Pages.Add(pageMerge);
            PageControl1.Pages.Add(pageMergeOptions);
            PageControl1.SelectedIndex = 0;
            PageControl1.Size = new Size(965, 493);

            pageMerge.Size = new Size(957, 463);
            pageMerge.Text = "pageMerge";
            //pageMerge.SizeChanged += SheetMergeResize;
            pageMerge.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { MergeCtl }
                    },
                    new TableRow {
                        Cells = { btnAutoSearch, btnSkip, ProgressBar1 }
                    }
                }
            };

            MergeCtl.Base = null;
            MergeCtl.Bookmark = false;
            MergeCtl.MergeMode = GKCommon.GEDCOM.GEDCOMRecordType.rtNone;
            MergeCtl.Size = new Size(957, 402);

            btnAutoSearch.Size = new Size(105, 31);
            btnAutoSearch.Text = "btnAutoSearch";
            btnAutoSearch.Click += btnSearch_Click;

            btnSkip.Size = new Size(105, 31);
            btnSkip.Text = "btnSkip";
            btnSkip.Click += btnSkip_Click;

            ProgressBar1.Size = new Size(700, 31);
            //ProgressBar1.Step = 1;

            //

            pageMergeOptions.Size = new Size(957, 463);
            pageMergeOptions.Text = "pageMergeOptions";
            pageMergeOptions.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { rgMode }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { grpSearchPersons }
                    },
                    new TableRow {
                        Cells = { grpMergeOther }
                    }
                }
            };

            grpMergeOther.Size = new Size(331, 118);
            grpMergeOther.Text = "grpMergeOther";
            grpMergeOther.Content = chkBookmarkMerged;

            chkBookmarkMerged.Size = new Size(319, 24);
            chkBookmarkMerged.Text = "chkBookmarkMerged";
            chkBookmarkMerged.CheckedChanged += chkBookmarkMerged_CheckedChanged;

            rgMode.Size = new Size(315, 118);
            rgMode.Text = "rgMode";
            rgMode.Content = new StackLayout {
                radPersons, radFamilies, radNotes, radSources
            };

            radSources.Size = new Size(269, 20);
            radSources.Text = "radSources";
            radSources.Click += radMergeMode_Click;

            radFamilies.Size = new Size(269, 19);
            radFamilies.Text = "radFamilies";
            radFamilies.Click += radMergeMode_Click;

            radNotes.Size = new Size(269, 19);
            radNotes.Text = "radNotes";
            radNotes.Click += radMergeMode_Click;

            radPersons.Checked = true;
            radPersons.Size = new Size(269, 20);
            radPersons.Text = "radPersons";
            radPersons.Click += radMergeMode_Click;

            grpSearchPersons.Size = new Size(315, 193);
            grpSearchPersons.Text = "grpSearchPersons";
            grpSearchPersons.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { chkIndistinctMatching }
                    },
                    new TableRow {
                        Cells = { lblNameAccuracy, edNameAccuracy }
                    },
                    new TableRow {
                        Cells = { chkBirthYear }
                    },
                    new TableRow {
                        Cells = { lblYearInaccuracy, edYearInaccuracy }
                    }
                }
            };

            lblNameAccuracy.Size = new Size(152, 15);
            lblNameAccuracy.Text = "lblNameAccuracy";

            lblYearInaccuracy.Size = new Size(152, 16);
            lblYearInaccuracy.Text = "lblYearInaccuracy";

            chkIndistinctMatching.Size = new Size(371, 21);
            chkIndistinctMatching.Text = "chkIndistinctMatching";

            edNameAccuracy.Size = new Size(152, 24);
            edNameAccuracy.Value = 90;

            edYearInaccuracy.Size = new Size(152, 24);
            edYearInaccuracy.Value = 3;

            chkBirthYear.Size = new Size(371, 21);
            chkBirthYear.Text = "chkBirthYear";

            //

            pageFamilyGroups.Size = new Size(1002, 515);
            pageFamilyGroups.Text = "pageFamilyGroups";
            pageFamilyGroups.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
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

            gkLogChart1.Size = new Size(976, 34);

            tvGroups.Size = new Size(976, 437);
            tvGroups.MouseDoubleClick += tvGroups_DoubleClick;

            //

            pageTreeCheck.Size = new Size(1002, 515);
            pageTreeCheck.Text = "pageTreeCheck";
            pageTreeCheck.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { Panel1 }
                    },
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { null, btnBaseRepair }
                    }
                }
            };

            btnBaseRepair.Size = new Size(203, 30);
            btnBaseRepair.Text = "btnBaseRepair";
            btnBaseRepair.Click += btnBaseRepair_Click;

            Panel1.Size = new Size(998, 448);

            //

            pagePatSearch.Size = new Size(1002, 515);
            pagePatSearch.Text = "pagePatSearch";
            pagePatSearch.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { Panel3 }
                    },
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { lblMinGenerations, edMinGens, chkWithoutDates, btnSetPatriarch, btnPatSearch, btnPatriarchsDiagram }
                    }
                }
            };

            btnPatriarchsDiagram.Size = new Size(105, 30);
            btnPatriarchsDiagram.Text = "btnPatriarchsDiagram";
            btnPatriarchsDiagram.Click += btnPatriarchsDiagram_Click;

            //chkWithoutDates.CheckAlign = System.Drawing.ContentAlignment.MiddleRight;
            chkWithoutDates.Size = new Size(158, 21);
            chkWithoutDates.Text = "chkWithoutDates";

            lblMinGenerations.Size = new Size(207, 17);
            lblMinGenerations.Text = "lblMinGenerations";

            btnPatSearch.Size = new Size(105, 30);
            btnPatSearch.Text = "btnPatSearch";
            btnPatSearch.Click += btnPatSearch_Click;

            Panel3.Size = new Size(998, 448);

            edMinGens.Size = new Size(79, 24);
            edMinGens.Value = 2;

            btnSetPatriarch.Size = new Size(172, 30);
            btnSetPatriarch.Text = "btnSetPatriarch";
            btnSetPatriarch.Click += btnSetPatriarch_Click;

            //

            pagePlaceManage.Size = new Size(1002, 515);
            pagePlaceManage.Text = "pagePlaceManage";
            pagePlaceManage.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { Panel4 }
                    },
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { btnIntoList, null }
                    }
                }
            };

            Panel4.Size = new Size(998, 448);

            btnIntoList.Size = new Size(179, 31);
            btnIntoList.Text = "btnIntoList";
            btnIntoList.Click += btnIntoList_Click;

            //

            btnClose.ImagePosition = ButtonImagePosition.Left;
            btnClose.Size = new Size(80, 26);
            btnClose.Text = "btnClose";

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsTools }
                    },
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { null, btnClose }
                    }
                }
            };

            AbortButton = btnClose;
            ClientSize = new Size(1034, 625);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "TreeToolsWin";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
