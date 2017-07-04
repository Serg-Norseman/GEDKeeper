using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
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
            btnAccept = new Button();
            btnCancel = new Button();
            lblRPSources = new Label();
            cmbSource = new ComboBox();
            rgBranchCut = new GroupBox();
            lblYear = new Label();
            rbCutNone = new RadioButton();
            rbCutYears = new RadioButton();
            rbCutPersons = new RadioButton();
            edYear = new NumericUpDown();
            Panel1 = new Panel();
            Panel2 = new Panel();

            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(114, 31);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(113, 31);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            lblRPSources.Size = new Size(79, 17);
            lblRPSources.Text = "lblRPSources";

            cmbSource.ReadOnly = true;
            cmbSource.Size = new Size(528, 25);

            rgBranchCut.Size = new Size(528, 322);
            rgBranchCut.Text = "rgBranchCut";
            Panel1.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { rbCutNone }
                    },
                    new TableRow {
                        Cells = { rbCutYears }
                    },
                    new TableRow {
                        Cells = { lblYear, edYear }
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

            lblYear.Size = new Size(31, 17);
            lblYear.Text = "lblYear";

            rbCutNone.Checked = true;
            rbCutNone.Size = new Size(52, 21);
            rbCutNone.Text = "rbCutNone";
            rbCutNone.Click += rbCutNoneClick;

            rbCutYears.Size = new Size(128, 21);
            rbCutYears.Text = "rbCutYears";
            rbCutYears.Click += rbCutNoneClick;

            rbCutPersons.Size = new Size(156, 21);
            rbCutPersons.Text = "rbCutPersons";
            rbCutPersons.Click += rbCutNoneClick;

            edYear.Increment = 10;
            edYear.MaxValue = 3000;
            edYear.Size = new Size(169, 24);

            Panel1.Size = new Size(504, 166);
            Panel1.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { rgBranchCut }
                    },
                    new TableRow {
                        Cells = { lblRPSources }
                    },
                    new TableRow {
                        Cells = { cmbSource }
                    }
                }
            };

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { Panel1 }
                    },
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { null, btnAccept, btnCancel }
                    }
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
