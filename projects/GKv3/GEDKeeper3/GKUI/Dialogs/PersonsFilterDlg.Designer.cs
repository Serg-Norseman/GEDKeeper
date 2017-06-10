using System;
using Eto.Drawing;
using Eto.Forms;

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
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
            }
            base.Dispose(disposing);
        }

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
            // 
            // tabsFilters
            // 
            tabsFilters.Controls.Add(pageSpecificFilter);
            tabsFilters.Size = new Size(829, 490);
            tabsFilters.Controls.SetChildIndex(pageSpecificFilter, 0);
            // 
            // pageSpecificFilter
            // 
            pageSpecificFilter.Controls.Add(lblNameMask);
            pageSpecificFilter.Controls.Add(lblPlaceMask);
            pageSpecificFilter.Controls.Add(lblGroups);
            pageSpecificFilter.Controls.Add(GroupBox1);
            pageSpecificFilter.Controls.Add(lblSources);
            pageSpecificFilter.Controls.Add(lblAliveBefore);
            pageSpecificFilter.Controls.Add(lblEventsMask);
            pageSpecificFilter.Controls.Add(rgLife);
            pageSpecificFilter.Controls.Add(txtName);
            pageSpecificFilter.Controls.Add(rgSex);
            pageSpecificFilter.Controls.Add(txtAliveBeforeDate);
            pageSpecificFilter.Controls.Add(cmbResidence);
            pageSpecificFilter.Controls.Add(cmbGroup);
            pageSpecificFilter.Controls.Add(cmbSource);
            pageSpecificFilter.Controls.Add(cmbEventVal);
            pageSpecificFilter.Location = new Point(4, 26);
            pageSpecificFilter.Padding = new Padding(10);
            pageSpecificFilter.Size = new Size(819, 464);
            pageSpecificFilter.Text = "pageSpecificFilter";

            lblNameMask.Location = new Point(14, 179);
            lblNameMask.Size = new Size(85, 17);
            lblNameMask.Text = "lblNameMask";

            lblPlaceMask.Location = new Point(14, 234);
            lblPlaceMask.Size = new Size(81, 17);
            lblPlaceMask.Text = "lblPlaceMask";

            lblGroups.Location = new Point(14, 344);
            lblGroups.Size = new Size(64, 17);
            lblGroups.Text = "lblGroups";

            GroupBox1.Controls.Add(chkOnlyPatriarchs);
            GroupBox1.Location = new Point(411, 73);
            GroupBox1.Size = new Size(234, 41);

            chkOnlyPatriarchs.Location = new Point(8, 16);
            chkOnlyPatriarchs.Size = new Size(140, 21);
            chkOnlyPatriarchs.Text = "chkOnlyPatriarchs";

            lblSources.Location = new Point(13, 399);
            lblSources.Size = new Size(69, 17);
            lblSources.Text = "lblSources";

            lblAliveBefore.Location = new Point(411, 15);
            lblAliveBefore.Size = new Size(86, 17);
            lblAliveBefore.Text = "lblAliveBefore";

            lblEventsMask.Location = new Point(13, 289);
            lblEventsMask.Size = new Size(92, 17);
            lblEventsMask.Text = "lblEventsMask";

            rgLife.Controls.Add(rbAliveBefore);
            rgLife.Controls.Add(rbOnlyDead);
            rgLife.Controls.Add(rbOnlyLive);
            rgLife.Controls.Add(rbAll);
            rgLife.Location = new Point(197, 15);
            rgLife.Padding = new Padding(10);
            rgLife.Size = new Size(193, 159);

            rbAliveBefore.Location = new Point(10, 120);
            rbAliveBefore.Size = new Size(108, 21);
            rbAliveBefore.Text = "rbAliveBefore";
            rbAliveBefore.Click += rgLifeClick;

            rbOnlyDead.Location = new Point(10, 89);
            rbOnlyDead.Size = new Size(102, 21);
            rbOnlyDead.Text = "rbOnlyDead";
            rbOnlyDead.Click += rgLifeClick;

            rbOnlyLive.Location = new Point(10, 58);
            rbOnlyLive.Size = new Size(94, 21);
            rbOnlyLive.Text = "rbOnlyLive";
            rbOnlyLive.Click += rgLifeClick;

            rbAll.Location = new Point(10, 27);
            rbAll.Size = new Size(54, 21);
            rbAll.Text = "rbAll";
            rbAll.Click += rgLifeClick;

            txtName.Location = new Point(13, 199);
            txtName.Size = new Size(281, 25);
            //txtName.Sorted = true;
            txtName.Text = "*";

            rgSex.Controls.Add(rbSexFemale);
            rgSex.Controls.Add(rbSexAll);
            rgSex.Controls.Add(rbSexMale);
            rgSex.Location = new Point(14, 15);
            rgSex.Padding = new Padding(10);
            rgSex.Size = new Size(166, 130);

            rbSexFemale.Location = new Point(10, 89);
            rbSexFemale.Size = new Size(107, 21);
            rbSexFemale.Text = "rbSexFemale";

            rbSexAll.Location = new Point(10, 27);
            rbSexAll.Size = new Size(77, 21);
            rbSexAll.Text = "rbSexAll";

            rbSexMale.Location = new Point(10, 58);
            rbSexMale.Size = new Size(91, 21);
            rbSexMale.Text = "rbSexMale";

            txtAliveBeforeDate.Enabled = false;
            txtAliveBeforeDate.Location = new Point(411, 35);
            txtAliveBeforeDate.Mask = "00/00/0000";
            txtAliveBeforeDate.Size = new Size(148, 24);
            txtAliveBeforeDate.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;

            cmbResidence.Location = new Point(13, 254);
            cmbResidence.Size = new Size(281, 25);
            //cmbResidence.Sorted = true;
            cmbResidence.Text = "*";

            cmbGroup.ReadOnly = true;
            cmbGroup.Location = new Point(13, 364);
            cmbGroup.Size = new Size(281, 25);

            cmbSource.ReadOnly = true;
            cmbSource.Location = new Point(14, 419);
            cmbSource.Size = new Size(281, 25);

            cmbEventVal.Location = new Point(14, 309);
            cmbEventVal.Size = new Size(281, 25);
            //cmbEventVal.Sorted = true;
            cmbEventVal.Text = "*";

            ClientSize = new Size(861, 584);
            tabsFilters.ResumeLayout();
            pageSpecificFilter.ResumeLayout();
            GroupBox1.ResumeLayout();
            rgLife.ResumeLayout();
            rgSex.ResumeLayout();
            ResumeLayout();
        }
    }
}
