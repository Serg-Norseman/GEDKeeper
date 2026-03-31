#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class TreeFilterDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private Label lblRPSources;
        private ComboBox cmbSource;
        private FrameView rgBranchCut;
        private RadioButton rbCutNone;
        private RadioButton rbCutYears;
        private RadioButton rbCutPersons;
        private Label lblYear;
        private NumericStepper edYear;
        private FrameView Panel1;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            lblRPSources = new Label();
            cmbSource = new ComboBox();
            rgBranchCut = new FrameView();
            lblYear = new Label();
            rbCutNone = new RadioButton();
            rbCutYears = new RadioButton();
            rbCutPersons = new RadioButton();
            edYear = new NumericStepper();
            Panel1 = new FrameView();

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 3;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 4;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnAccept);
            AddButton(btnCancel);

            rbCutNone.Checked = true;
            rbCutNone.Location = new Point(1, 1);
            rbCutNone.TabIndex = 0;
            rbCutNone.TabStop = true;
            rbCutNone.Clicked += rbCutNoneClick;
            rbCutNone.Group = "cut";

            rbCutYears.Location = new Point(1, 3);
            rbCutYears.TabIndex = 1;
            rbCutYears.Clicked += rbCutNoneClick;
            rbCutYears.Group = "cut";

            lblYear.Location = new Point(4, 4);
            lblYear.TabIndex = 2;

            edYear.Location = new Point(16, 4);
            edYear.Size = new Size(10, 1);
            edYear.Maximum = 3000;
            edYear.Step = 10;
            edYear.TabIndex = 3;

            rbCutPersons.Location = new Point(1, 6);
            rbCutPersons.TabIndex = 4;
            rbCutPersons.Clicked += rbCutNoneClick;
            rbCutPersons.Group = "cut";

            rgBranchCut.Add(lblYear);
            rgBranchCut.Add(rbCutNone);
            rgBranchCut.Add(rbCutYears);
            rgBranchCut.Add(rbCutPersons);
            rgBranchCut.Add(edYear);
            rgBranchCut.Location = new Point(0, 0);
            rgBranchCut.Size = new Size(53, 10);
            rgBranchCut.TabIndex = 0;
            rgBranchCut.TabStop = false;

            Panel1.Location = new Point(0, 10);
            Panel1.Size = new Size(53, 16);
            Panel1.TabIndex = 5;

            lblRPSources.Location = new Point(1, 27);
            lblRPSources.TabIndex = 1;

            cmbSource.Location = new Point(16, 27);
            cmbSource.Size = new Size(34, 2);
            cmbSource.TabIndex = 2;

            Size = new Size(55, 33);
            Add(lblRPSources);
            Add(cmbSource);
            Add(Panel1);
            Add(rgBranchCut);
            Loaded += TreeFilterDlg_Load;
        }
    }
}
