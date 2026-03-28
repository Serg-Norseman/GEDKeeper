#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class TTRecMergeDlg
    {
        private TabView PageControl1;
        private TabPage pageMerge;
        private Button btnAutoSearch;
        private Button btnSkip;
        private ProgressBar ProgressBar1;
        private TabPage pageMergeOptions;
        private FrameView rgMode;
        private FrameView grpSearchPersons;
        private Label lblNameAccuracy;
        private Label lblYearInaccuracy;
        private NumericStepper edNameAccuracy;
        private NumericStepper edYearInaccuracy;
        private CheckBox chkBirthYear;
        private RadioButton radPersons;
        private RadioButton radNotes;
        private RadioButton radFamilies;
        private RadioButton radSources;
        private CheckBox chkBookmarkMerged;
        private FrameView grpMergeOther;
        private CheckBox chkIndistinctMatching;
        private Button btnMergeToRight;
        private Button btnMergeToLeft;
        private Button btnRec2Select;
        private Button btnRec1Select;
        private TextField Edit2;
        private TextField Edit1;
        private Label Lab2;
        private Label Lab1;
        private HyperView fView1;
        private HyperView fView2;
        private Button btnEditRight;
        private Button btnEditLeft;

        private void InitializeComponent()
        {
            PageControl1 = new TabView();
            pageMerge = new TabPage();
            Lab1 = new Label();
            Lab2 = new Label();
            Edit1 = new TextField();
            Edit2 = new TextField();
            btnRec1Select = new Button();
            btnRec2Select = new Button();
            btnMergeToLeft = new Button();
            btnMergeToRight = new Button();
            fView1 = new HyperView();
            fView2 = new HyperView();
            btnAutoSearch = new Button();
            btnSkip = new Button();
            ProgressBar1 = new ProgressBar();
            pageMergeOptions = new TabPage();
            grpMergeOther = new FrameView();
            chkBookmarkMerged = new CheckBox();
            rgMode = new FrameView();
            radSources = new RadioButton();
            radFamilies = new RadioButton();
            radNotes = new RadioButton();
            radPersons = new RadioButton();
            grpSearchPersons = new FrameView();
            lblNameAccuracy = new Label();
            lblYearInaccuracy = new Label();
            chkIndistinctMatching = new CheckBox();
            edNameAccuracy = new NumericStepper();
            edYearInaccuracy = new NumericStepper();
            chkBirthYear = new CheckBox();
            btnEditLeft = new Button();
            btnEditRight = new Button();

            Lab1.Location = new Point(1, 1);
            Lab1.TabIndex = 9;

            Lab2.Location = new Point(43, 1);
            Lab2.TabIndex = 11;

            Edit1.Location = new Point(1, 2);
            Edit1.ReadOnly = true;
            Edit1.Size = new Size(50, 1);
            Edit1.TabIndex = 10;

            Edit2.Location = new Point(53, 2);
            Edit2.ReadOnly = true;
            Edit2.Size = new Size(50, 1);
            Edit2.TabIndex = 12;

            btnRec1Select.Location = new Point(40, 1);
            btnRec1Select.Size = new Size(5, 1);
            btnRec1Select.TabIndex = 13;
            btnRec1Select.Clicked += btnRec1Select_Click;

            btnRec2Select.Location = new Point(80, 1);
            btnRec2Select.Size = new Size(5, 1);
            btnRec2Select.TabIndex = 14;
            btnRec2Select.Clicked += btnRec2Select_Click;

            var vview1 = new FrameView();
            vview1.Location = new Point(1, 4);
            vview1.Size = new Size(50, 30);
            fView1.Height = Dim.Fill();
            fView1.Width = Dim.Fill();
            fView1.TabIndex = 17;
            fView1.TabStop = true;
            fView1.WordWrap = true;
            vview1.Add(fView1);

            var vview2 = new FrameView();
            vview2.Location = new Point(53, 4);
            vview2.Size = new Size(50, 30);
            fView2.Height = Dim.Fill();
            fView2.Width = Dim.Fill();
            fView2.TabIndex = 18;
            fView2.TabStop = true;
            fView2.WordWrap = true;
            vview2.Add(fView2);

            btnMergeToLeft.Text = "<<-";
            btnMergeToLeft.Enabled = false;
            btnMergeToLeft.Y = Pos.Bottom(vview1);
            btnMergeToLeft.X = Pos.Right(vview1) - 17;
            btnMergeToLeft.Size = new Size(16, 1);
            btnMergeToLeft.TabIndex = 15;
            btnMergeToLeft.Clicked += btnMergeToLeft_Click;

            btnMergeToRight.Text = "->>";
            btnMergeToRight.Enabled = false;
            btnMergeToRight.Y = Pos.Bottom(vview2);
            btnMergeToRight.X = Pos.Left(vview2);
            btnMergeToRight.Size = new Size(16, 1);
            btnMergeToRight.TabIndex = 16;
            btnMergeToRight.Clicked += btnMergeToRight_Click;

            btnEditLeft.Enabled = false;
            btnEditLeft.Y = Pos.Bottom(vview1);
            btnEditLeft.X = Pos.Left(vview1);
            btnEditLeft.Size = new Size(16, 1);
            btnEditLeft.TabIndex = 15;
            btnEditLeft.Clicked += btnEditLeft_Click;

            btnEditRight.Enabled = false;
            btnEditRight.Y = Pos.Bottom(vview2);
            btnEditRight.X = Pos.Right(vview2) - 17;
            btnEditRight.Size = new Size(16, 1);
            btnEditRight.TabIndex = 16;
            btnEditRight.Clicked += btnEditRight_Click;

            btnAutoSearch.Location = new Point(1, 39);
            btnAutoSearch.Size = new Size(16, 1);
            btnAutoSearch.TabIndex = 0;
            btnAutoSearch.Clicked += btnSearch_Click;

            btnSkip.Location = new Point(20, 39);
            btnSkip.Size = new Size(16, 1);
            btnSkip.TabIndex = 9;
            btnSkip.Clicked += btnSkip_Click;

            ProgressBar1.ProgressBarStyle = ProgressBarStyle.Continuous;
            ProgressBar1.Location = new Point(1, 40);
            ProgressBar1.Size = new Size(102, 1);
            ProgressBar1.TabIndex = 10;
            ProgressBar1.ColorScheme = new ColorScheme() {
                Normal = new Terminal.Gui.Attribute(Color.Black, Color.Gray)
            };

            pageMerge.View.Add(vview1);
            pageMerge.View.Add(Lab1);
            pageMerge.View.Add(Lab2);
            pageMerge.View.Add(Edit1);
            pageMerge.View.Add(Edit2);
            pageMerge.View.Add(btnRec1Select);
            pageMerge.View.Add(btnRec2Select);
            pageMerge.View.Add(btnMergeToLeft);
            pageMerge.View.Add(btnMergeToRight);
            pageMerge.View.Add(btnEditLeft);
            pageMerge.View.Add(btnEditRight);
            pageMerge.View.Add(vview2);
            pageMerge.View.Add(btnAutoSearch);
            pageMerge.View.Add(btnSkip);
            pageMerge.View.Add(ProgressBar1);

            radPersons.Checked = true;
            radPersons.Location = new Point(1, 1);
            radPersons.TabIndex = 0;
            radPersons.TabStop = true;
            radPersons.Clicked += radMergeMode_Click;

            radNotes.Location = new Point(1, 2);
            radNotes.TabIndex = 1;
            radNotes.Clicked += radMergeMode_Click;

            radFamilies.Location = new Point(1, 3);
            radFamilies.TabIndex = 2;
            radFamilies.Clicked += radMergeMode_Click;

            radSources.Location = new Point(1, 4);
            radSources.TabIndex = 3;
            radSources.Clicked += radMergeMode_Click;

            rgMode.Add(radSources);
            rgMode.Add(radFamilies);
            rgMode.Add(radNotes);
            rgMode.Add(radPersons);
            rgMode.Location = new Point(1, 1);
            rgMode.Size = new Size(30, 8);
            rgMode.TabIndex = 0;
            rgMode.TabStop = false;

            chkIndistinctMatching.Location = new Point(1, 1);
            chkIndistinctMatching.TabIndex = 1;

            lblNameAccuracy.Location = new Point(1, 3);
            lblNameAccuracy.TabIndex = 0;

            edNameAccuracy.Location = new Point(26, 3);
            edNameAccuracy.Size = new Size(10, 1);
            edNameAccuracy.TabIndex = 2;
            edNameAccuracy.Value = 90;

            chkBirthYear.Location = new Point(1, 5);
            chkBirthYear.TabIndex = 6;

            lblYearInaccuracy.Location = new Point(1, 7);
            lblYearInaccuracy.TabIndex = 1;

            edYearInaccuracy.Location = new Point(26, 7);
            edYearInaccuracy.Size = new Size(10, 1);
            edYearInaccuracy.TabIndex = 4;
            edYearInaccuracy.Value = 3;

            grpSearchPersons.Add(lblNameAccuracy);
            grpSearchPersons.Add(lblYearInaccuracy);
            grpSearchPersons.Add(chkIndistinctMatching);
            grpSearchPersons.Add(edNameAccuracy);
            grpSearchPersons.Add(edYearInaccuracy);
            grpSearchPersons.Add(chkBirthYear);
            grpSearchPersons.Location = new Point(1, 9);
            grpSearchPersons.Size = new Size(40, 11);
            grpSearchPersons.TabIndex = 1;
            grpSearchPersons.TabStop = false;

            chkBookmarkMerged.Location = new Point(1, 1);
            chkBookmarkMerged.TabIndex = 0;
            chkBookmarkMerged.CheckedChanged += chkBookmarkMerged_CheckedChanged;

            grpMergeOther.Add(chkBookmarkMerged);
            grpMergeOther.Location = new Point(32, 1);
            grpMergeOther.Size = new Size(40, 5);
            grpMergeOther.TabIndex = 2;
            grpMergeOther.TabStop = false;

            pageMergeOptions.View.Add(grpMergeOther);
            pageMergeOptions.View.Add(rgMode);
            pageMergeOptions.View.Add(grpSearchPersons);

            PageControl1.AddTab(pageMerge);
            PageControl1.AddTab(pageMergeOptions);
            PageControl1.Location = new Point(0, 0);
            PageControl1.Size = new Size(106, 46);
            PageControl1.TabIndex = 0;

            Size = new Size(108, 48);
            Add(PageControl1);
        }
    }
}
