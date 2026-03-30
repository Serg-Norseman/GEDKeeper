#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;
using Terminal.Gui.TextValidateProviders;

namespace GKUI.Forms
{
    partial class PersonsFilterDlg
    {
        private TabPage pageSpecificFilter;
        private ComboBox cmbEventVal;
        private ComboBox cmbSource;
        private ComboBox cmbGroup;
        private ComboBox cmbResidence;
        private TextValidateField txtAliveBeforeDate;
        private RadioButton rbSexMale;
        private RadioButton rbSexAll;
        private RadioButton rbSexFemale;
        private FrameView rgSex;
        private ComboBox txtName;
        private RadioButton rbAll;
        private RadioButton rbOnlyLive;
        private RadioButton rbOnlyDead;
        private RadioButton rbAliveBefore;
        private FrameView rgLife;
        private Label lblEventsMask;
        private Label lblSources;
        private CheckBox chkOnlyPatriarchs;
        private Label lblGroups;
        private Label lblPlaceMask;
        private Label lblNameMask;

        private void InitializeComponent()
        {
            pageSpecificFilter = new TabPage();
            lblNameMask = new Label();
            lblPlaceMask = new Label();
            lblGroups = new Label();
            chkOnlyPatriarchs = new CheckBox();
            lblSources = new Label();
            lblEventsMask = new Label();
            rgLife = new FrameView();
            rbAliveBefore = new RadioButton();
            rbOnlyDead = new RadioButton();
            rbOnlyLive = new RadioButton();
            rbAll = new RadioButton();
            txtName = new ComboBox();
            rgSex = new FrameView();
            rbSexFemale = new RadioButton();
            rbSexAll = new RadioButton();
            rbSexMale = new RadioButton();
            txtAliveBeforeDate = new TextValidateField();
            cmbResidence = new ComboBox();
            cmbGroup = new ComboBox();
            cmbSource = new ComboBox();
            cmbEventVal = new ComboBox();

            rbSexAll.Location = new Point(1, 1);
            rbSexAll.TabIndex = 0;
            rbSexAll.Group = "sx";

            rbSexMale.Location = new Point(1, 2);
            rbSexMale.TabIndex = 1;
            rbSexMale.Group = "sx";

            rbSexFemale.Location = new Point(1, 3);
            rbSexFemale.TabIndex = 2;
            rbSexFemale.Group = "sx";

            chkOnlyPatriarchs.Location = new Point(1, 5);
            chkOnlyPatriarchs.TabIndex = 29;

            rgSex.Add(rbSexAll);
            rgSex.Add(rbSexMale);
            rgSex.Add(rbSexFemale);
            rgSex.Add(chkOnlyPatriarchs);
            rgSex.Location = new Point(1, 1);
            rgSex.Size = new Size(30, 9);
            rgSex.TabIndex = 16;
            rgSex.TabStop = false;

            rbAll.Location = new Point(1, 1);
            rbAll.TabIndex = 0;
            rbAll.CheckedChanged += rgLife_CheckedChanged;
            rbAll.Group = "lf";

            rbOnlyLive.Location = new Point(1, 2);
            rbOnlyLive.TabIndex = 1;
            rbOnlyLive.CheckedChanged += rgLife_CheckedChanged;
            rbOnlyLive.Group = "lf";

            rbOnlyDead.Location = new Point(1, 3);
            rbOnlyDead.TabIndex = 2;
            rbOnlyDead.CheckedChanged += rgLife_CheckedChanged;
            rbOnlyDead.Group = "lf";

            rbAliveBefore.Location = new Point(1, 4);
            rbAliveBefore.TabIndex = 3;
            rbAliveBefore.CheckedChanged += rgLife_CheckedChanged;
            rbAliveBefore.Group = "lf";

            txtAliveBeforeDate.Enabled = false;
            txtAliveBeforeDate.Location = new Point(5, 5);
            txtAliveBeforeDate.Provider = new NetMaskedTextProvider(@"00\/00\/0000");
            txtAliveBeforeDate.InvalidIndication = false;
            txtAliveBeforeDate.Size = new Size(12, 1);
            txtAliveBeforeDate.TabIndex = 18;

            rgLife.Add(rbAll);
            rgLife.Add(rbOnlyLive);
            rgLife.Add(rbOnlyDead);
            rgLife.Add(rbAliveBefore);
            rgLife.Add(txtAliveBeforeDate);
            rgLife.Location = new Point(32, 1);
            rgLife.Size = new Size(30, 9);
            rgLife.TabIndex = 15;
            rgLife.TabStop = false;

            lblNameMask.Location = new Point(1, 12);
            lblNameMask.TabIndex = 19;

            txtName.Location = new Point(28, 12);
            txtName.Size = new Size(28, 2);
            txtName.TabIndex = 20;
            txtName.KeyUp += cmbFilter_KeyUp;

            lblPlaceMask.Location = new Point(1, 14);
            lblPlaceMask.TabIndex = 21;

            cmbResidence.Location = new Point(28, 14);
            cmbResidence.Size = new Size(28, 2);
            cmbResidence.TabIndex = 22;
            cmbResidence.KeyUp += cmbFilter_KeyUp;

            lblEventsMask.Location = new Point(1, 16);
            lblEventsMask.TabIndex = 23;

            cmbEventVal.Location = new Point(28, 16);
            cmbEventVal.Size = new Size(28, 2);
            cmbEventVal.TabIndex = 24;
            cmbEventVal.KeyUp += cmbFilter_KeyUp;

            lblGroups.Location = new Point(1, 18);
            lblGroups.TabIndex = 25;

            cmbGroup.Location = new Point(28, 18);
            cmbGroup.Size = new Size(28, 2);
            cmbGroup.TabIndex = 26;

            lblSources.Location = new Point(1, 20);
            lblSources.TabIndex = 27;

            cmbSource.Location = new Point(28, 20);
            cmbSource.Size = new Size(28, 2);
            cmbSource.TabIndex = 28;

            pageSpecificFilter.View.Add(lblNameMask);
            pageSpecificFilter.View.Add(txtName);
            pageSpecificFilter.View.Add(lblPlaceMask);
            pageSpecificFilter.View.Add(cmbResidence);
            pageSpecificFilter.View.Add(lblEventsMask);
            pageSpecificFilter.View.Add(cmbEventVal);
            pageSpecificFilter.View.Add(rgLife);
            pageSpecificFilter.View.Add(rgSex);
            pageSpecificFilter.View.Add(lblGroups);
            pageSpecificFilter.View.Add(cmbGroup);
            pageSpecificFilter.View.Add(lblSources);
            pageSpecificFilter.View.Add(cmbSource);
            tabsFilters.AddTab(pageSpecificFilter);
        }
    }
}
