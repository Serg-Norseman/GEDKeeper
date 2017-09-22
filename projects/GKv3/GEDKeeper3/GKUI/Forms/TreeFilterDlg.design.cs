using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class TreeFilterDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private Label lblRPSources;
        private ComboBox cmbSource;
        private GroupBox rgBranchCut;
        private RadioButton rbCutNone;
        private RadioButton rbCutYears;
        private RadioButton rbCutPersons;
        private Label lblYear;
        private NumericUpDown edYear;
        private Panel Panel1;
        private Panel Panel2;

        private void InitializeComponent()
        {
            SuspendLayout();

            Panel2 = new Panel();

            rbCutNone = new RadioButton();
            rbCutNone.Checked = true;
            rbCutNone.Text = "rbCutNone";
            rbCutNone.CheckedChanged += rbCutX_CheckedChanged;

            rbCutYears = new RadioButton(rbCutNone);
            rbCutYears.Text = "rbCutYears";
            rbCutYears.CheckedChanged += rbCutX_CheckedChanged;

            lblYear = new Label();
            lblYear.Text = "lblYear";

            edYear = new NumericUpDown();
            edYear.Increment = 10;
            edYear.MaxValue = 3000;

            rbCutPersons = new RadioButton(rbCutNone);
            rbCutPersons.Text = "rbCutPersons";
            rbCutPersons.CheckedChanged += rbCutX_CheckedChanged;

            rgBranchCut = new GroupBox();
            rgBranchCut.Text = "rgBranchCut";
            rgBranchCut.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { rbCutNone }
                    },
                    new TableRow {
                        Cells = { rbCutYears }
                    },
                    new TableRow {
                        Cells = { TableLayout.Horizontal(10, lblYear, edYear) }
                    },
                    new TableRow {
                        Cells = { rbCutPersons }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { Panel2 }
                    }
                }
            };

            //

            lblRPSources = new Label();
            //lblRPSources.Size = new Size(79, 17);
            lblRPSources.Text = "lblRPSources";

            cmbSource = new ComboBox();
            cmbSource.ReadOnly = true;
            //cmbSource.Size = new Size(528, 25);

            Panel1 = new Panel();
            Panel1.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { rgBranchCut }
                    },
                    new TableRow {
                        Cells = { TableLayout.Horizontal(10, lblRPSources, cmbSource) }
                    }
                }
            };

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(130, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(130, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { Panel1 }
                    },
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "TreeFilterDlg";
            Load += TreeFilterDlg_Load;

            SetPredefProperties(550, 440);
            ResumeLayout();
        }
    }
}
