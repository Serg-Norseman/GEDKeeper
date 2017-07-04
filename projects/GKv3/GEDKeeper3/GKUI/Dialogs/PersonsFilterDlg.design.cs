using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
{
    partial class PersonsFilterDlg
    {
        private TabPage pageSpecificFilter;
        private ComboBox cmbEventVal;
        private ComboBox cmbSource;
        private ComboBox cmbGroup;
        private ComboBox cmbResidence;
        private MaskedTextBox txtAliveBeforeDate;
        private RadioButton rbSexMale;
        private RadioButton rbSexAll;
        private RadioButton rbSexFemale;
        private GroupBox rgSex;
        private ComboBox txtName;
        private RadioButton rbAll;
        private RadioButton rbOnlyLive;
        private RadioButton rbOnlyDead;
        private RadioButton rbAliveBefore;
        private GroupBox rgLife;
        private Label lblEventsMask;
        private Label lblAliveBefore;
        private Label lblSources;
        private CheckBox chkOnlyPatriarchs;
        private GroupBox GroupBox1;
        private Label lblGroups;
        private Label lblPlaceMask;
        private Label lblNameMask;

        private void InitializeComponent()
        {
            SuspendLayout();

            lblNameMask = new Label();
            //lblNameMask.Size = new Size(85, 17);
            lblNameMask.Text = "lblNameMask";

            lblPlaceMask = new Label();
            //lblPlaceMask.Size = new Size(81, 17);
            lblPlaceMask.Text = "lblPlaceMask";

            lblGroups = new Label();
            //lblGroups.Size = new Size(64, 17);
            lblGroups.Text = "lblGroups";

            chkOnlyPatriarchs = new CheckBox();
            //chkOnlyPatriarchs.Size = new Size(140, 21);
            chkOnlyPatriarchs.Text = "chkOnlyPatriarchs";

            GroupBox1 = new GroupBox();
            GroupBox1.Content = chkOnlyPatriarchs;
            //GroupBox1.Size = new Size(234, 41);

            lblSources = new Label();
            //lblSources.Size = new Size(69, 17);
            lblSources.Text = "lblSources";

            lblAliveBefore = new Label();
            //lblAliveBefore.Size = new Size(86, 17);
            lblAliveBefore.Text = "lblAliveBefore";

            lblEventsMask = new Label();
            //lblEventsMask.Size = new Size(92, 17);
            lblEventsMask.Text = "lblEventsMask";

            txtName = new ComboBox();
            //txtName.Size = new Size(281, 25);
            //txtName.Sorted = true;
            txtName.Text = "*";

            //

            rbAll = new RadioButton();
            //rbAll.Size = new Size(54, 21);
            rbAll.Text = "rbAll";
            rbAll.Click += rgLifeClick;

            rbOnlyLive = new RadioButton(rbAll);
            //rbOnlyLive.Size = new Size(94, 21);
            rbOnlyLive.Text = "rbOnlyLive";
            rbOnlyLive.Click += rgLifeClick;

            rbOnlyDead = new RadioButton(rbAll);
            //rbOnlyDead.Size = new Size(102, 21);
            rbOnlyDead.Text = "rbOnlyDead";
            rbOnlyDead.Click += rgLifeClick;

            rbAliveBefore = new RadioButton(rbAll);
            rbAliveBefore.Size = new Size(108, 21);
            //rbAliveBefore.Text = "rbAliveBefore";
            rbAliveBefore.Click += rgLifeClick;

            rgLife = new GroupBox();
            rgLife.Content = new StackLayout { Items = { rbAll, rbOnlyLive, rbOnlyDead, rbAliveBefore } };
            //rgLife.Size = new Size(193, 159);

            //

            rbSexAll = new RadioButton();
            //rbSexAll.Size = new Size(77, 21);
            rbSexAll.Text = "rbSexAll";

            rbSexMale = new RadioButton(rbSexAll);
            //rbSexMale.Size = new Size(91, 21);
            rbSexMale.Text = "rbSexMale";

            rbSexFemale = new RadioButton(rbSexAll);
            //rbSexFemale.Size = new Size(107, 21);
            rbSexFemale.Text = "rbSexFemale";

            rgSex = new GroupBox();
            rgSex.Content = new StackLayout { Items = { rbSexAll, rbSexMale, rbSexFemale } };
            //rgSex.Padding = new Padding(10);
            //rgSex.Size = new Size(166, 130);

            //

            txtAliveBeforeDate = new MaskedTextBox();
            txtAliveBeforeDate.Provider = new FixedMaskedTextProvider("00/00/0000");
            txtAliveBeforeDate.Enabled = false;
            //txtAliveBeforeDate.Mask = "00/00/0000";
            //txtAliveBeforeDate.Size = new Size(148, 24);
            //txtAliveBeforeDate.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;

            cmbResidence = new ComboBox();
            //cmbResidence.Size = new Size(281, 25);
            //cmbResidence.Sorted = true;
            cmbResidence.Text = "*";

            cmbGroup = new ComboBox();
            cmbGroup.ReadOnly = true;
            //cmbGroup.Size = new Size(281, 25);

            cmbSource = new ComboBox();
            cmbSource.ReadOnly = true;
            //cmbSource.Size = new Size(281, 25);

            cmbEventVal = new ComboBox();
            //cmbEventVal.Size = new Size(281, 25);
            //cmbEventVal.Sorted = true;
            cmbEventVal.Text = "*";

            pageSpecificFilter = new TabPage();
            pageSpecificFilter.Text = "pageSpecificFilter";
            pageSpecificFilter.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { rgSex, rgLife, lblAliveBefore, txtAliveBeforeDate, GroupBox1 }
                    },
                    new TableRow {
                        Cells = { lblNameMask }
                    },
                    new TableRow {
                        Cells = { txtName }
                    },
                    new TableRow {
                        Cells = { lblPlaceMask }
                    },
                    new TableRow {
                        Cells = { cmbResidence }
                    },
                    new TableRow {
                        Cells = { lblEventsMask }
                    },
                    new TableRow {
                        Cells = { cmbEventVal }
                    },
                    new TableRow {
                        Cells = { lblGroups }
                    },
                    new TableRow {
                        Cells = { cmbGroup }
                    },
                    new TableRow {
                        Cells = { lblSources }
                    },
                    new TableRow {
                        Cells = { cmbSource }
                    },
                    null
                }
            };

            tabsFilters.Pages.Add(pageSpecificFilter);

            ResumeLayout();
        }
    }
}
