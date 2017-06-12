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

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            pageSpecificFilter = new TabPage();
            lblNameMask = new Label();
            lblPlaceMask = new Label();
            lblGroups = new Label();
            GroupBox1 = new GroupBox();
            chkOnlyPatriarchs = new CheckBox();
            lblSources = new Label();
            lblAliveBefore = new Label();
            lblEventsMask = new Label();
            rgLife = new GroupBox();
            rbAliveBefore = new RadioButton();
            rbOnlyDead = new RadioButton();
            rbOnlyLive = new RadioButton();
            rbAll = new RadioButton();
            txtName = new ComboBox();
            rgSex = new GroupBox();
            rbSexFemale = new RadioButton();
            rbSexAll = new RadioButton();
            rbSexMale = new RadioButton();
            txtAliveBeforeDate = new MaskedTextBox();
            cmbResidence = new ComboBox();
            cmbGroup = new ComboBox();
            cmbSource = new ComboBox();
            cmbEventVal = new ComboBox();
            tabsFilters.SuspendLayout();
            pageSpecificFilter.SuspendLayout();
            GroupBox1.SuspendLayout();
            rgLife.SuspendLayout();
            rgSex.SuspendLayout();
            SuspendLayout();

            tabsFilters.Pages.Add(pageSpecificFilter);
            tabsFilters.Size = new Size(829, 490);

            pageSpecificFilter.Size = new Size(819, 464);
            pageSpecificFilter.Text = "pageSpecificFilter";
            pageSpecificFilter.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
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

            lblNameMask.Size = new Size(85, 17);
            lblNameMask.Text = "lblNameMask";

            lblPlaceMask.Size = new Size(81, 17);
            lblPlaceMask.Text = "lblPlaceMask";

            lblGroups.Size = new Size(64, 17);
            lblGroups.Text = "lblGroups";

            GroupBox1.Content = chkOnlyPatriarchs;
            GroupBox1.Size = new Size(234, 41);

            chkOnlyPatriarchs.Size = new Size(140, 21);
            chkOnlyPatriarchs.Text = "chkOnlyPatriarchs";

            lblSources.Size = new Size(69, 17);
            lblSources.Text = "lblSources";

            lblAliveBefore.Size = new Size(86, 17);
            lblAliveBefore.Text = "lblAliveBefore";

            lblEventsMask.Size = new Size(92, 17);
            lblEventsMask.Text = "lblEventsMask";

            rgLife.Content = new StackLayout { Items = { rbAll, rbOnlyLive, rbOnlyDead, rbAliveBefore } };
            rgLife.Size = new Size(193, 159);

            rbAliveBefore.Size = new Size(108, 21);
            rbAliveBefore.Text = "rbAliveBefore";
            rbAliveBefore.Click += rgLifeClick;

            rbOnlyDead.Size = new Size(102, 21);
            rbOnlyDead.Text = "rbOnlyDead";
            rbOnlyDead.Click += rgLifeClick;

            rbOnlyLive.Size = new Size(94, 21);
            rbOnlyLive.Text = "rbOnlyLive";
            rbOnlyLive.Click += rgLifeClick;

            rbAll.Size = new Size(54, 21);
            rbAll.Text = "rbAll";
            rbAll.Click += rgLifeClick;

            txtName.Size = new Size(281, 25);
            //txtName.Sorted = true;
            txtName.Text = "*";

            rgSex.Content = new StackLayout { Items = { rbSexAll, rbSexMale, rbSexFemale } };
            rgSex.Padding = new Padding(10);
            rgSex.Size = new Size(166, 130);

            rbSexFemale.Size = new Size(107, 21);
            rbSexFemale.Text = "rbSexFemale";

            rbSexAll.Size = new Size(77, 21);
            rbSexAll.Text = "rbSexAll";

            rbSexMale.Size = new Size(91, 21);
            rbSexMale.Text = "rbSexMale";

            txtAliveBeforeDate.Provider = new FixedMaskedTextProvider("00/00/0000");
            txtAliveBeforeDate.Enabled = false;
            //txtAliveBeforeDate.Mask = "00/00/0000";
            txtAliveBeforeDate.Size = new Size(148, 24);
            //txtAliveBeforeDate.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;

            cmbResidence.Size = new Size(281, 25);
            //cmbResidence.Sorted = true;
            cmbResidence.Text = "*";

            cmbGroup.ReadOnly = true;
            cmbGroup.Size = new Size(281, 25);

            cmbSource.ReadOnly = true;
            cmbSource.Size = new Size(281, 25);

            cmbEventVal.Size = new Size(281, 25);
            //cmbEventVal.Sorted = true;
            cmbEventVal.Text = "*";

            ClientSize = new Size(861, 584);
            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
