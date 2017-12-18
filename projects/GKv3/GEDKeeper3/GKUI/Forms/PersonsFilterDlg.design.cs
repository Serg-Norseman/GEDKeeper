using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
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
        private Label lblGroups;
        private Label lblPlaceMask;
        private Label lblNameMask;

        private void InitializeComponent()
        {
            SuspendLayout();

            //

            rbAll = new RadioButton();
            rbAll.Text = "rbAll";
            rbAll.CheckedChanged += rgLife_CheckedChanged;

            rbOnlyLive = new RadioButton(rbAll);
            rbOnlyLive.Text = "rbOnlyLive";
            rbOnlyLive.CheckedChanged += rgLife_CheckedChanged;

            rbOnlyDead = new RadioButton(rbAll);
            rbOnlyDead.Text = "rbOnlyDead";
            rbOnlyDead.CheckedChanged += rgLife_CheckedChanged;

            rbAliveBefore = new RadioButton(rbAll);
            rbAliveBefore.Text = "rbAliveBefore";
            rbAliveBefore.CheckedChanged += rgLife_CheckedChanged;

            lblAliveBefore = new Label();
            lblAliveBefore.Text = "lblAliveBefore";

            txtAliveBeforeDate = new MaskedTextBox();
            txtAliveBeforeDate.Provider = new FixedMaskedTextProvider("00/00/0000");
            txtAliveBeforeDate.Enabled = false;

            rgLife = new GroupBox();
            rgLife.Content = new VDefStackLayout { Items = {
                    rbAll, rbOnlyLive, rbOnlyDead, rbAliveBefore,
                    new DefStackLayout(Orientation.Horizontal, 10, lblAliveBefore, txtAliveBeforeDate)
                } };

            //

            rbSexAll = new RadioButton();
            rbSexAll.Text = "rbSexAll";

            rbSexMale = new RadioButton(rbSexAll);
            rbSexMale.Text = "rbSexMale";

            rbSexFemale = new RadioButton(rbSexAll);
            rbSexFemale.Text = "rbSexFemale";

            rgSex = new GroupBox();
            rgSex.Content = new VDefStackLayout { Items = { rbSexAll, rbSexMale, rbSexFemale } };

            //

            lblNameMask = new Label();
            lblNameMask.Text = "lblNameMask";

            txtName = new ComboBox();

            lblPlaceMask = new Label();
            lblPlaceMask.Text = "lblPlaceMask";

            cmbResidence = new ComboBox();

            lblGroups = new Label();
            lblGroups.Text = "lblGroups";

            cmbGroup = new ComboBox();
            cmbGroup.ReadOnly = true;

            lblEventsMask = new Label();
            lblEventsMask.Text = "lblEventsMask";

            cmbEventVal = new ComboBox();

            lblSources = new Label();
            lblSources.Text = "lblSources";

            cmbSource = new ComboBox();
            cmbSource.ReadOnly = true;

            chkOnlyPatriarchs = new CheckBox();
            chkOnlyPatriarchs.Text = "chkOnlyPatriarchs";

            var masksPanel = new DefTableLayout() {
                Rows = {
                    new TableRow {
                        Cells = { lblNameMask, txtName }
                    },
                    new TableRow {
                        Cells = { lblPlaceMask, cmbResidence }
                    },
                    new TableRow {
                        Cells = { lblEventsMask, cmbEventVal }
                    },
                    new TableRow {
                        Cells = { lblGroups, cmbGroup }
                    },
                    new TableRow {
                        Cells = { lblSources, cmbSource }
                    },
                    new TableRow {
                        Cells = { chkOnlyPatriarchs, null }
                    },
                }
            };

            pageSpecificFilter = new TabPage();
            pageSpecificFilter.Text = "pageSpecificFilter";
            pageSpecificFilter.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { rgSex, rgLife }
                    },
                    new TableRow {
                        Cells = { masksPanel, null }
                    },
                    null
                }
            };

            tabsFilters.Pages.Add(pageSpecificFilter);

            ResumeLayout();
        }
    }
}
