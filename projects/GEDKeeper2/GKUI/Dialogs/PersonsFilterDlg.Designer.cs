namespace GKUI.Dialogs
{
    partial class PersonsFilterDlg
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;
        private System.Windows.Forms.TabPage pageSpecificFilter;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
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
            // PageControl1
            // 
            this.tabsFilters.Controls.Add(this.pageSpecificFilter);
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
            this.pageSpecificFilter.Name = "pageSpecificFilter";
            this.pageSpecificFilter.Size = new System.Drawing.Size(785, 457);
            this.pageSpecificFilter.TabIndex = 1;
            this.pageSpecificFilter.Text = "pageSpecificFilter";
            // 
            // lblNameMask
            // 
            this.lblNameMask.AutoSize = true;
            this.lblNameMask.Location = new System.Drawing.Point(14, 160);
            this.lblNameMask.Name = "lblNameMask";
            this.lblNameMask.Size = new System.Drawing.Size(90, 17);
            this.lblNameMask.TabIndex = 19;
            this.lblNameMask.Text = "lblNameMask";
            // 
            // lblPlaceMask
            // 
            this.lblPlaceMask.AutoSize = true;
            this.lblPlaceMask.Location = new System.Drawing.Point(14, 209);
            this.lblPlaceMask.Name = "lblPlaceMask";
            this.lblPlaceMask.Size = new System.Drawing.Size(166, 17);
            this.lblPlaceMask.TabIndex = 21;
            this.lblPlaceMask.Text = "lblPlaceMask";
            // 
            // lblGroups
            // 
            this.lblGroups.AutoSize = true;
            this.lblGroups.Location = new System.Drawing.Point(14, 309);
            this.lblGroups.Name = "lblGroups";
            this.lblGroups.Size = new System.Drawing.Size(57, 17);
            this.lblGroups.TabIndex = 25;
            this.lblGroups.Text = "lblGroups";
            // 
            // GroupBox1
            // 
            this.GroupBox1.Controls.Add(this.chkOnlyPatriarchs);
            this.GroupBox1.Location = new System.Drawing.Point(411, 73);
            this.GroupBox1.Name = "GroupBox1";
            this.GroupBox1.Size = new System.Drawing.Size(226, 41);
            this.GroupBox1.TabIndex = 29;
            this.GroupBox1.TabStop = false;
            // 
            // chkOnlyPatriarchs
            // 
            this.chkOnlyPatriarchs.AutoSize = true;
            this.chkOnlyPatriarchs.Location = new System.Drawing.Point(8, 16);
            this.chkOnlyPatriarchs.Name = "chkOnlyPatriarchs";
            this.chkOnlyPatriarchs.Size = new System.Drawing.Size(160, 21);
            this.chkOnlyPatriarchs.TabIndex = 0;
            this.chkOnlyPatriarchs.Text = "chkOnlyPatriarchs";
            // 
            // lblSources
            // 
            this.lblSources.AutoSize = true;
            this.lblSources.Location = new System.Drawing.Point(14, 359);
            this.lblSources.Name = "lblSources";
            this.lblSources.Size = new System.Drawing.Size(79, 17);
            this.lblSources.TabIndex = 27;
            this.lblSources.Text = "lblSources";
            // 
            // lblAliveBefore
            // 
            this.lblAliveBefore.AutoSize = true;
            this.lblAliveBefore.Location = new System.Drawing.Point(411, 15);
            this.lblAliveBefore.Name = "lblAliveBefore";
            this.lblAliveBefore.Size = new System.Drawing.Size(88, 17);
            this.lblAliveBefore.TabIndex = 17;
            this.lblAliveBefore.Text = "lblAliveBefore";
            // 
            // lblEventsMask
            // 
            this.lblEventsMask.AutoSize = true;
            this.lblEventsMask.Location = new System.Drawing.Point(14, 260);
            this.lblEventsMask.Name = "lblEventsMask";
            this.lblEventsMask.Size = new System.Drawing.Size(97, 17);
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
            this.rgLife.Size = new System.Drawing.Size(193, 130);
            this.rgLife.TabIndex = 15;
            this.rgLife.TabStop = false;
            // 
            // rbAliveBefore
            // 
            this.rbAliveBefore.AutoSize = true;
            this.rbAliveBefore.Location = new System.Drawing.Point(8, 101);
            this.rbAliveBefore.Name = "rbAliveBefore";
            this.rbAliveBefore.Size = new System.Drawing.Size(103, 21);
            this.rbAliveBefore.TabIndex = 3;
            this.rbAliveBefore.Text = "rbAliveBefore";
            this.rbAliveBefore.Click += new System.EventHandler(this.rgLifeClick);
            // 
            // rbOnlyDead
            // 
            this.rbOnlyDead.AutoSize = true;
            this.rbOnlyDead.Location = new System.Drawing.Point(8, 74);
            this.rbOnlyDead.Name = "rbOnlyDead";
            this.rbOnlyDead.Size = new System.Drawing.Size(136, 21);
            this.rbOnlyDead.TabIndex = 2;
            this.rbOnlyDead.Text = "rbOnlyDead";
            this.rbOnlyDead.Click += new System.EventHandler(this.rgLifeClick);
            // 
            // rbOnlyLive
            // 
            this.rbOnlyLive.AutoSize = true;
            this.rbOnlyLive.Location = new System.Drawing.Point(8, 47);
            this.rbOnlyLive.Name = "rbOnlyLive";
            this.rbOnlyLive.Size = new System.Drawing.Size(121, 21);
            this.rbOnlyLive.TabIndex = 1;
            this.rbOnlyLive.Text = "rbOnlyLive";
            this.rbOnlyLive.Click += new System.EventHandler(this.rgLifeClick);
            // 
            // rbAll
            // 
            this.rbAll.AutoSize = true;
            this.rbAll.Location = new System.Drawing.Point(8, 20);
            this.rbAll.Name = "rbAll";
            this.rbAll.Size = new System.Drawing.Size(50, 21);
            this.rbAll.TabIndex = 0;
            this.rbAll.Text = "rbAll";
            this.rbAll.Click += new System.EventHandler(this.rgLifeClick);
            // 
            // txtName
            // 
            this.txtName.Location = new System.Drawing.Point(14, 180);
            this.txtName.Name = "txtName";
            this.txtName.Size = new System.Drawing.Size(281, 25);
            this.txtName.Sorted = true;
            this.txtName.TabIndex = 20;
            this.txtName.Text = "*";
            // 
            // rgSex
            // 
            this.rgSex.Controls.Add(this.rbSexFemale);
            this.rgSex.Controls.Add(this.rbSexAll);
            this.rgSex.Controls.Add(this.rbSexMale);
            this.rgSex.Location = new System.Drawing.Point(14, 15);
            this.rgSex.Name = "rgSex";
            this.rgSex.Size = new System.Drawing.Size(166, 130);
            this.rgSex.TabIndex = 16;
            this.rgSex.TabStop = false;
            // 
            // rbSexFemale
            // 
            this.rbSexFemale.AutoSize = true;
            this.rbSexFemale.Location = new System.Drawing.Point(8, 78);
            this.rbSexFemale.Name = "rbSexFemale";
            this.rbSexFemale.Size = new System.Drawing.Size(141, 21);
            this.rbSexFemale.TabIndex = 2;
            this.rbSexFemale.Text = "rbSexFemale";
            // 
            // rbSexAll
            // 
            this.rbSexAll.AutoSize = true;
            this.rbSexAll.Location = new System.Drawing.Point(8, 24);
            this.rbSexAll.Name = "rbSexAll";
            this.rbSexAll.Size = new System.Drawing.Size(50, 21);
            this.rbSexAll.TabIndex = 0;
            this.rbSexAll.Text = "rbSexAll";
            // 
            // rbSexMale
            // 
            this.rbSexMale.AutoSize = true;
            this.rbSexMale.Location = new System.Drawing.Point(8, 51);
            this.rbSexMale.Name = "rbSexMale";
            this.rbSexMale.Size = new System.Drawing.Size(140, 21);
            this.rbSexMale.TabIndex = 1;
            this.rbSexMale.Text = "rbSexMale";
            // 
            // txtAliveBeforeDate
            // 
            this.txtAliveBeforeDate.Enabled = false;
            this.txtAliveBeforeDate.Location = new System.Drawing.Point(411, 35);
            this.txtAliveBeforeDate.Mask = "00/00/0000";
            this.txtAliveBeforeDate.Name = "txtAliveBeforeDate";
            this.txtAliveBeforeDate.Size = new System.Drawing.Size(137, 24);
            this.txtAliveBeforeDate.TabIndex = 18;
            this.txtAliveBeforeDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
            // 
            // cmbResidence
            // 
            this.cmbResidence.Location = new System.Drawing.Point(14, 229);
            this.cmbResidence.Name = "cmbResidence";
            this.cmbResidence.Size = new System.Drawing.Size(281, 25);
            this.cmbResidence.Sorted = true;
            this.cmbResidence.TabIndex = 22;
            this.cmbResidence.Text = "*";
            // 
            // cmbGroup
            // 
            this.cmbGroup.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbGroup.Location = new System.Drawing.Point(14, 329);
            this.cmbGroup.Name = "cmbGroup";
            this.cmbGroup.Size = new System.Drawing.Size(281, 25);
            this.cmbGroup.TabIndex = 26;
            // 
            // cmbSource
            // 
            this.cmbSource.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbSource.Location = new System.Drawing.Point(14, 379);
            this.cmbSource.Name = "cmbSource";
            this.cmbSource.Size = new System.Drawing.Size(281, 25);
            this.cmbSource.TabIndex = 28;
            // 
            // cmbEventVal
            // 
            this.cmbEventVal.Location = new System.Drawing.Point(14, 280);
            this.cmbEventVal.Name = "cmbEventVal";
            this.cmbEventVal.Size = new System.Drawing.Size(281, 25);
            this.cmbEventVal.Sorted = true;
            this.cmbEventVal.TabIndex = 24;
            this.cmbEventVal.Text = "*";
            // 
            // PersonsFilterDlg
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.AutoSize = true;
            this.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.ClientSize = new System.Drawing.Size(816, 557);
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

        #endregion
    }
}
