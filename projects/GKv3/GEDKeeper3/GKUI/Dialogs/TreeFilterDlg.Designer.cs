using System;
using Eto.Drawing;
using Eto.Forms;

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
            rgBranchCut.SuspendLayout();
            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(302, 398);
            btnAccept.Size = new Size(114, 31);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(426, 398);
            btnCancel.Size = new Size(113, 31);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            lblRPSources.Location = new Point(11, 340);
            lblRPSources.Size = new Size(79, 17);
            lblRPSources.Text = "lblRPSources";

            cmbSource.ReadOnly = true;
            cmbSource.Location = new Point(11, 359);
            cmbSource.Size = new Size(528, 25);

            rgBranchCut.Controls.Add(lblYear);
            rgBranchCut.Controls.Add(rbCutNone);
            rgBranchCut.Controls.Add(rbCutYears);
            rgBranchCut.Controls.Add(rbCutPersons);
            rgBranchCut.Controls.Add(edYear);
            rgBranchCut.Location = new Point(11, 10);
            rgBranchCut.Size = new Size(528, 322);
            rgBranchCut.Text = "rgBranchCut";

            lblYear.Location = new Point(43, 89);
            lblYear.Size = new Size(31, 17);
            lblYear.Text = "lblYear";

            rbCutNone.Checked = true;
            rbCutNone.Location = new Point(22, 29);
            rbCutNone.Size = new Size(52, 21);
            rbCutNone.Text = "rbCutNone";
            rbCutNone.Click += rbCutNoneClick;

            rbCutYears.Location = new Point(22, 58);
            rbCutYears.Size = new Size(128, 21);
            rbCutYears.Text = "rbCutYears";
            rbCutYears.Click += rbCutNoneClick;

            rbCutPersons.Location = new Point(22, 126);
            rbCutPersons.Size = new Size(156, 21);
            rbCutPersons.Text = "rbCutPersons";
            rbCutPersons.Click += rbCutNoneClick;

            edYear.Increment = 10;
            edYear.Location = new Point(90, 87);
            edYear.MaxValue = 3000;
            edYear.Size = new Size(169, 24);

            Panel1.BorderStyle = BorderStyle.Fixed3D;
            Panel1.Location = new Point(22, 155);
            Panel1.Size = new Size(504, 166);

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(553, 442);
            Controls.Add(lblRPSources);
            Controls.Add(cmbSource);
            Controls.Add(Panel1);
            Controls.Add(rgBranchCut);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "TreeFilterDlg";
            Load += TreeFilterDlg_Load;
            rgBranchCut.ResumeLayout();
            ResumeLayout();
        }
    }
}
