using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class TTRecMergeDlg
    {
        private TabControl tabsTools;
        private Button btnClose;
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
        private RadioButton radPersons;
        private RadioButton radNotes;
        private RadioButton radFamilies;
        private RadioButton radSources;
        private CheckBox chkBookmarkMerged;
        private GroupBox grpMergeOther;
        private GKUI.Components.GKMergeControl MergeCtl;
        private CheckBox chkIndistinctMatching;

        private void InitializeComponent()
        {
            SuspendLayout();

            //

            MergeCtl = new GKUI.Components.GKMergeControl();
            MergeCtl.Base = null;
            MergeCtl.Bookmark = false;
            MergeCtl.MergeMode = GKCommon.GEDCOM.GEDCOMRecordType.rtNone;

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
                    }
                }
            };

            chkBookmarkMerged = new CheckBox();
            //chkBookmarkMerged.Size = new Size(319, 24);
            chkBookmarkMerged.Text = "chkBookmarkMerged";
            chkBookmarkMerged.CheckedChanged += chkBookmarkMerged_CheckedChanged;

            grpMergeOther = new GroupBox();
            grpMergeOther.Text = "grpMergeOther";
            grpMergeOther.Content = chkBookmarkMerged;

            pageMergeOptions = new TabPage();
            pageMergeOptions.Text = "pageMergeOptions";
            pageMergeOptions.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
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

            tabsTools = new TabControl();
            tabsTools.Pages.Add(pageRecMerge);
            tabsTools.SelectedIndex = 0;

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
