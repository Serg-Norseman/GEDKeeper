namespace GKUI.Forms
{
    partial class PersonsFilterDlg
    {
        private System.ComponentModel.IContainer components = null;
        private System.Windows.Forms.TabPage pageSpecificFilter;
        private System.Windows.Forms.ComboBox cmbEventVal;
        private System.Windows.Forms.ComboBox cmbSource;
        private System.Windows.Forms.ComboBox cmbGroup;
        private System.Windows.Forms.ComboBox cmbResidence;
        private System.Windows.Forms.MaskedTextBox txtAliveBeforeDate;
        private System.Windows.Forms.RadioButton rbSexMale;
        private System.Windows.Forms.RadioButton rbSexAll;
        private System.Windows.Forms.RadioButton rbSexFemale;
        private System.Windows.Forms.GroupBox rgSex;
        private System.Windows.Forms.ComboBox txtName;
        private System.Windows.Forms.RadioButton rbAll;
        private System.Windows.Forms.RadioButton rbOnlyLive;
        private System.Windows.Forms.RadioButton rbOnlyDead;
        private System.Windows.Forms.RadioButton rbAliveBefore;
        private System.Windows.Forms.GroupBox rgLife;
        private System.Windows.Forms.Label lblEventsMask;
        private System.Windows.Forms.Label lblAliveBefore;
        private System.Windows.Forms.Label lblSources;
        private System.Windows.Forms.CheckBox chkOnlyPatriarchs;
        private System.Windows.Forms.GroupBox GroupBox1;
        private System.Windows.Forms.Label lblGroups;
        private System.Windows.Forms.Label lblPlaceMask;
        private System.Windows.Forms.Label lblNameMask;

        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null)) {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            this.pageSpecificFilter = new System.Windows.Forms.TabPage();
            this.lblNameMask = new System.Windows.Forms.Label();
            this.lblPlaceMask = new System.Windows.Forms.Label();
            this.lblGroups = new System.Windows.Forms.Label();
            this.GroupBox1 = new System.Windows.Forms.GroupBox();
            this.chkOnlyPatriarchs = new System.Windows.Forms.CheckBox();
            this.lblSources = new System.Windows.Forms.Label();
            this.lblAliveBefore = new System.Windows.Forms.Label();
            this.lblEventsMask = new System.Windows.Forms.Label();
            this.rgLife = new System.Windows.Forms.GroupBox();
            this.rbAliveBefore = new System.Windows.Forms.RadioButton();
            this.rbOnlyDead = new System.Windows.Forms.RadioButton();
            this.rbOnlyLive = new System.Windows.Forms.RadioButton();
            this.rbAll = new System.Windows.Forms.RadioButton();
            this.txtName = new System.Windows.Forms.ComboBox();
            this.rgSex = new System.Windows.Forms.GroupBox();
            this.rbSexFemale = new System.Windows.Forms.RadioButton();
            this.rbSexAll = new System.Windows.Forms.RadioButton();
            this.rbSexMale = new System.Windows.Forms.RadioButton();
            this.txtAliveBeforeDate = new System.Windows.Forms.MaskedTextBox();
            this.cmbResidence = new System.Windows.Forms.ComboBox();
            this.cmbGroup = new System.Windows.Forms.ComboBox();
            this.cmbSource = new System.Windows.Forms.ComboBox();
            this.cmbEventVal = new System.Windows.Forms.ComboBox();
            this.tabsFilters.SuspendLayout();
            this.pageSpecificFilter.SuspendLayout();
            this.GroupBox1.SuspendLayout();
            this.rgLife.SuspendLayout();
            this.rgSex.SuspendLayout();
            this.SuspendLayout();
            // 
            // tabsFilters
            // 
            this.tabsFilters.Controls.Add(this.pageSpecificFilter);
            this.tabsFilters.Size = new System.Drawing.Size(829, 490);
            this.tabsFilters.Controls.SetChildIndex(this.pageSpecificFilter, 0);
            // 
            // pageSpecificFilter
            // 
            this.pageSpecificFilter.Controls.Add(this.lblNameMask);
            this.pageSpecificFilter.Controls.Add(this.lblPlaceMask);
            this.pageSpecificFilter.Controls.Add(this.lblGroups);
            this.pageSpecificFilter.Controls.Add(this.GroupBox1);
            this.pageSpecificFilter.Controls.Add(this.lblSources);
            this.pageSpecificFilter.Controls.Add(this.lblAliveBefore);
            this.pageSpecificFilter.Controls.Add(this.lblEventsMask);
            this.pageSpecificFilter.Controls.Add(this.rgLife);
            this.pageSpecificFilter.Controls.Add(this.txtName);
            this.pageSpecificFilter.Controls.Add(this.rgSex);
            this.pageSpecificFilter.Controls.Add(this.txtAliveBeforeDate);
            this.pageSpecificFilter.Controls.Add(this.cmbResidence);
            this.pageSpecificFilter.Controls.Add(this.cmbGroup);
            this.pageSpecificFilter.Controls.Add(this.cmbSource);
            this.pageSpecificFilter.Controls.Add(this.cmbEventVal);
            this.pageSpecificFilter.Location = new System.Drawing.Point(4, 26);
            this.pageSpecificFilter.Margin = new System.Windows.Forms.Padding(0);
            this.pageSpecificFilter.Name = "pageSpecificFilter";
            this.pageSpecificFilter.Padding = new System.Windows.Forms.Padding(10);
            this.pageSpecificFilter.Size = new System.Drawing.Size(819, 464);
            this.pageSpecificFilter.TabIndex = 1;
            this.pageSpecificFilter.Text = "pageSpecificFilter";
            // 
            // lblNameMask
            // 
            this.lblNameMask.AutoSize = true;
            this.lblNameMask.Location = new System.Drawing.Point(14, 179);
            this.lblNameMask.Name = "lblNameMask";
            this.lblNameMask.Size = new System.Drawing.Size(85, 17);
            this.lblNameMask.TabIndex = 19;
            this.lblNameMask.Text = "lblNameMask";
            // 
            // lblPlaceMask
            // 
            this.lblPlaceMask.AutoSize = true;
            this.lblPlaceMask.Location = new System.Drawing.Point(14, 234);
            this.lblPlaceMask.Name = "lblPlaceMask";
            this.lblPlaceMask.Size = new System.Drawing.Size(81, 17);
            this.lblPlaceMask.TabIndex = 21;
            this.lblPlaceMask.Text = "lblPlaceMask";
            // 
            // lblGroups
            // 
            this.lblGroups.AutoSize = true;
            this.lblGroups.Location = new System.Drawing.Point(14, 344);
            this.lblGroups.Name = "lblGroups";
            this.lblGroups.Size = new System.Drawing.Size(64, 17);
            this.lblGroups.TabIndex = 25;
            this.lblGroups.Text = "lblGroups";
            // 
            // GroupBox1
            // 
            this.GroupBox1.Controls.Add(this.chkOnlyPatriarchs);
            this.GroupBox1.Location = new System.Drawing.Point(411, 73);
            this.GroupBox1.Name = "GroupBox1";
            this.GroupBox1.Size = new System.Drawing.Size(234, 41);
            this.GroupBox1.TabIndex = 29;
            this.GroupBox1.TabStop = false;
            // 
            // chkOnlyPatriarchs
            // 
            this.chkOnlyPatriarchs.AutoSize = true;
            this.chkOnlyPatriarchs.Location = new System.Drawing.Point(8, 16);
            this.chkOnlyPatriarchs.Name = "chkOnlyPatriarchs";
            this.chkOnlyPatriarchs.Size = new System.Drawing.Size(140, 21);
            this.chkOnlyPatriarchs.TabIndex = 0;
            this.chkOnlyPatriarchs.Text = "chkOnlyPatriarchs";
            // 
            // lblSources
            // 
            this.lblSources.AutoSize = true;
            this.lblSources.Location = new System.Drawing.Point(13, 399);
            this.lblSources.Name = "lblSources";
            this.lblSources.Size = new System.Drawing.Size(69, 17);
            this.lblSources.TabIndex = 27;
            this.lblSources.Text = "lblSources";
            // 
            // lblAliveBefore
            // 
            this.lblAliveBefore.AutoSize = true;
            this.lblAliveBefore.Location = new System.Drawing.Point(411, 15);
            this.lblAliveBefore.Name = "lblAliveBefore";
            this.lblAliveBefore.Size = new System.Drawing.Size(86, 17);
            this.lblAliveBefore.TabIndex = 17;
            this.lblAliveBefore.Text = "lblAliveBefore";
            // 
            // lblEventsMask
            // 
            this.lblEventsMask.AutoSize = true;
            this.lblEventsMask.Location = new System.Drawing.Point(13, 289);
            this.lblEventsMask.Name = "lblEventsMask";
            this.lblEventsMask.Size = new System.Drawing.Size(92, 17);
            this.lblEventsMask.TabIndex = 23;
            this.lblEventsMask.Text = "lblEventsMask";
            // 
            // rgLife
            // 
            this.rgLife.Controls.Add(this.rbAliveBefore);
            this.rgLife.Controls.Add(this.rbOnlyDead);
            this.rgLife.Controls.Add(this.rbOnlyLive);
            this.rgLife.Controls.Add(this.rbAll);
            this.rgLife.Location = new System.Drawing.Point(197, 15);
            this.rgLife.Name = "rgLife";
            this.rgLife.Padding = new System.Windows.Forms.Padding(10);
            this.rgLife.Size = new System.Drawing.Size(193, 159);
            this.rgLife.TabIndex = 15;
            this.rgLife.TabStop = false;
            // 
            // rbAliveBefore
            // 
            this.rbAliveBefore.AutoSize = true;
            this.rbAliveBefore.Location = new System.Drawing.Point(10, 120);
            this.rbAliveBefore.Margin = new System.Windows.Forms.Padding(0);
            this.rbAliveBefore.Name = "rbAliveBefore";
            this.rbAliveBefore.Size = new System.Drawing.Size(108, 21);
            this.rbAliveBefore.TabIndex = 3;
            this.rbAliveBefore.Text = "rbAliveBefore";
            this.rbAliveBefore.CheckedChanged += new System.EventHandler(this.rgLife_CheckedChanged);
            // 
            // rbOnlyDead
            // 
            this.rbOnlyDead.AutoSize = true;
            this.rbOnlyDead.Location = new System.Drawing.Point(10, 89);
            this.rbOnlyDead.Margin = new System.Windows.Forms.Padding(0, 0, 0, 10);
            this.rbOnlyDead.Name = "rbOnlyDead";
            this.rbOnlyDead.Size = new System.Drawing.Size(102, 21);
            this.rbOnlyDead.TabIndex = 2;
            this.rbOnlyDead.Text = "rbOnlyDead";
            this.rbOnlyDead.CheckedChanged += new System.EventHandler(this.rgLife_CheckedChanged);
            // 
            // rbOnlyLive
            // 
            this.rbOnlyLive.AutoSize = true;
            this.rbOnlyLive.Location = new System.Drawing.Point(10, 58);
            this.rbOnlyLive.Margin = new System.Windows.Forms.Padding(0, 0, 0, 10);
            this.rbOnlyLive.Name = "rbOnlyLive";
            this.rbOnlyLive.Size = new System.Drawing.Size(94, 21);
            this.rbOnlyLive.TabIndex = 1;
            this.rbOnlyLive.Text = "rbOnlyLive";
            this.rbOnlyLive.CheckedChanged += new System.EventHandler(this.rgLife_CheckedChanged);
            // 
            // rbAll
            // 
            this.rbAll.AutoSize = true;
            this.rbAll.Location = new System.Drawing.Point(10, 27);
            this.rbAll.Margin = new System.Windows.Forms.Padding(0, 0, 0, 10);
            this.rbAll.Name = "rbAll";
            this.rbAll.Size = new System.Drawing.Size(54, 21);
            this.rbAll.TabIndex = 0;
            this.rbAll.Text = "rbAll";
            this.rbAll.CheckedChanged += new System.EventHandler(this.rgLife_CheckedChanged);
            // 
            // txtName
            // 
            this.txtName.Location = new System.Drawing.Point(13, 199);
            this.txtName.Margin = new System.Windows.Forms.Padding(3, 3, 3, 10);
            this.txtName.Name = "txtName";
            this.txtName.Size = new System.Drawing.Size(281, 25);
            this.txtName.Sorted = true;
            this.txtName.TabIndex = 20;
            this.txtName.Text = "*";
            this.txtName.KeyUp += new System.Windows.Forms.KeyEventHandler(this.cmbFilter_KeyUp);
            // 
            // rgSex
            // 
            this.rgSex.Controls.Add(this.rbSexFemale);
            this.rgSex.Controls.Add(this.rbSexAll);
            this.rgSex.Controls.Add(this.rbSexMale);
            this.rgSex.Location = new System.Drawing.Point(14, 15);
            this.rgSex.Margin = new System.Windows.Forms.Padding(10);
            this.rgSex.Name = "rgSex";
            this.rgSex.Padding = new System.Windows.Forms.Padding(10);
            this.rgSex.Size = new System.Drawing.Size(166, 130);
            this.rgSex.TabIndex = 16;
            this.rgSex.TabStop = false;
            // 
            // rbSexFemale
            // 
            this.rbSexFemale.AutoSize = true;
            this.rbSexFemale.Location = new System.Drawing.Point(10, 89);
            this.rbSexFemale.Margin = new System.Windows.Forms.Padding(0);
            this.rbSexFemale.Name = "rbSexFemale";
            this.rbSexFemale.Size = new System.Drawing.Size(107, 21);
            this.rbSexFemale.TabIndex = 2;
            this.rbSexFemale.Text = "rbSexFemale";
            // 
            // rbSexAll
            // 
            this.rbSexAll.AutoSize = true;
            this.rbSexAll.Location = new System.Drawing.Point(10, 27);
            this.rbSexAll.Margin = new System.Windows.Forms.Padding(0, 0, 0, 10);
            this.rbSexAll.Name = "rbSexAll";
            this.rbSexAll.Size = new System.Drawing.Size(77, 21);
            this.rbSexAll.TabIndex = 0;
            this.rbSexAll.Text = "rbSexAll";
            // 
            // rbSexMale
            // 
            this.rbSexMale.AutoSize = true;
            this.rbSexMale.Location = new System.Drawing.Point(10, 58);
            this.rbSexMale.Margin = new System.Windows.Forms.Padding(0, 0, 0, 10);
            this.rbSexMale.Name = "rbSexMale";
            this.rbSexMale.Size = new System.Drawing.Size(91, 21);
            this.rbSexMale.TabIndex = 1;
            this.rbSexMale.Text = "rbSexMale";
            // 
            // txtAliveBeforeDate
            // 
            this.txtAliveBeforeDate.Enabled = false;
            this.txtAliveBeforeDate.Location = new System.Drawing.Point(411, 35);
            this.txtAliveBeforeDate.Mask = "00/00/0000";
            this.txtAliveBeforeDate.Name = "txtAliveBeforeDate";
            this.txtAliveBeforeDate.Size = new System.Drawing.Size(148, 24);
            this.txtAliveBeforeDate.TabIndex = 18;
            this.txtAliveBeforeDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
            // 
            // cmbResidence
            // 
            this.cmbResidence.Location = new System.Drawing.Point(13, 254);
            this.cmbResidence.Margin = new System.Windows.Forms.Padding(3, 3, 3, 10);
            this.cmbResidence.Name = "cmbResidence";
            this.cmbResidence.Size = new System.Drawing.Size(281, 25);
            this.cmbResidence.Sorted = true;
            this.cmbResidence.TabIndex = 22;
            this.cmbResidence.Text = "*";
            this.cmbResidence.KeyUp += new System.Windows.Forms.KeyEventHandler(this.cmbFilter_KeyUp);
            // 
            // cmbGroup
            // 
            this.cmbGroup.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbGroup.Location = new System.Drawing.Point(13, 364);
            this.cmbGroup.Margin = new System.Windows.Forms.Padding(3, 3, 3, 10);
            this.cmbGroup.Name = "cmbGroup";
            this.cmbGroup.Size = new System.Drawing.Size(281, 25);
            this.cmbGroup.TabIndex = 26;
            // 
            // cmbSource
            // 
            this.cmbSource.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbSource.Location = new System.Drawing.Point(14, 419);
            this.cmbSource.Name = "cmbSource";
            this.cmbSource.Size = new System.Drawing.Size(281, 25);
            this.cmbSource.TabIndex = 28;
            // 
            // cmbEventVal
            // 
            this.cmbEventVal.Location = new System.Drawing.Point(14, 309);
            this.cmbEventVal.Margin = new System.Windows.Forms.Padding(3, 3, 3, 10);
            this.cmbEventVal.Name = "cmbEventVal";
            this.cmbEventVal.Size = new System.Drawing.Size(281, 25);
            this.cmbEventVal.Sorted = true;
            this.cmbEventVal.TabIndex = 24;
            this.cmbEventVal.Text = "*";
            this.cmbEventVal.KeyUp += new System.Windows.Forms.KeyEventHandler(this.cmbFilter_KeyUp);
            // 
            // PersonsFilterDlg
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.ClientSize = new System.Drawing.Size(861, 584);
            this.Name = "PersonsFilterDlg";
            this.tabsFilters.ResumeLayout(false);
            this.pageSpecificFilter.ResumeLayout(false);
            this.pageSpecificFilter.PerformLayout();
            this.GroupBox1.ResumeLayout(false);
            this.GroupBox1.PerformLayout();
            this.rgLife.ResumeLayout(false);
            this.rgLife.PerformLayout();
            this.rgSex.ResumeLayout(false);
            this.rgSex.PerformLayout();
            this.ResumeLayout(false);
        }
    }
}
