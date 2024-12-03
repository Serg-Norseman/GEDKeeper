namespace GKFlowInputPlugin
{
    partial class FlowInputDlg
    {
        private System.Windows.Forms.Button btnParse;
        private System.Windows.Forms.Button btnClose;
        private System.Windows.Forms.TabControl PageControl1;
        private System.Windows.Forms.TabPage tsSimpleInput;
        private System.Windows.Forms.TabPage tsSourceInput;
        private System.Windows.Forms.Label lblFullName;
        private System.Windows.Forms.Label lblNote;
        private System.Windows.Forms.Button btnMale;
        private System.Windows.Forms.TextBox txtFullName;
        private System.Windows.Forms.TextBox txtNote;
        private System.Windows.Forms.Panel Panel1;
        private System.Windows.Forms.Label lblBirthDate;
        private System.Windows.Forms.Label lblBirthPlace;
        private System.Windows.Forms.MaskedTextBox txtBirthDate;
        private System.Windows.Forms.TextBox txtBirthPlace;
        private System.Windows.Forms.CheckBox chkBirth;
        private System.Windows.Forms.Panel Panel2;
        private System.Windows.Forms.Label lblDeathDate;
        private System.Windows.Forms.Label lblDeathPlace;
        private System.Windows.Forms.CheckBox chkDeath;
        private System.Windows.Forms.MaskedTextBox txtDeathDate;
        private System.Windows.Forms.TextBox txtDeathPlace;
        private System.Windows.Forms.Label lblSource;
        private System.Windows.Forms.Label lblPage;
        private System.Windows.Forms.Label lblYear;
        private System.Windows.Forms.Label lblSettlement;
        private System.Windows.Forms.ComboBox cbSource;
        private System.Windows.Forms.TextBox edPage;
        private System.Windows.Forms.TextBox edSourceYear;
        private System.Windows.Forms.TextBox edPlace;
        private System.Windows.Forms.ComboBox cbPersonLink;
        private System.Windows.Forms.GroupBox rgSourceKind;
        private System.Windows.Forms.GroupBox gbMetrics;
        private System.Windows.Forms.Label lblEventDate;
        private System.Windows.Forms.Label lblEventType;
        private System.Windows.Forms.MaskedTextBox edEventDate;
        private System.Windows.Forms.ComboBox cbEventType;
        private System.Windows.Forms.Panel sgData;
        private System.Windows.Forms.DataGridView dataGridView1;
        private System.Windows.Forms.RadioButton rbSK_Rev;
        private System.Windows.Forms.RadioButton rbSK_Met;

        private void InitializeComponent()
        {
            this.btnParse = new System.Windows.Forms.Button();
            this.btnClose = new System.Windows.Forms.Button();
            this.PageControl1 = new System.Windows.Forms.TabControl();
            this.tsSimpleInput = new System.Windows.Forms.TabPage();
            this.lblFullName = new System.Windows.Forms.Label();
            this.lblNote = new System.Windows.Forms.Label();
            this.btnMale = new System.Windows.Forms.Button();
            this.txtFullName = new System.Windows.Forms.TextBox();
            this.txtNote = new System.Windows.Forms.TextBox();
            this.Panel1 = new System.Windows.Forms.Panel();
            this.lblBirthDate = new System.Windows.Forms.Label();
            this.lblBirthPlace = new System.Windows.Forms.Label();
            this.txtBirthDate = new System.Windows.Forms.MaskedTextBox();
            this.txtBirthPlace = new System.Windows.Forms.TextBox();
            this.chkBirth = new System.Windows.Forms.CheckBox();
            this.Panel2 = new System.Windows.Forms.Panel();
            this.lblDeathDate = new System.Windows.Forms.Label();
            this.lblDeathPlace = new System.Windows.Forms.Label();
            this.chkDeath = new System.Windows.Forms.CheckBox();
            this.txtDeathDate = new System.Windows.Forms.MaskedTextBox();
            this.txtDeathPlace = new System.Windows.Forms.TextBox();
            this.tsSourceInput = new System.Windows.Forms.TabPage();
            this.lblSource = new System.Windows.Forms.Label();
            this.lblPage = new System.Windows.Forms.Label();
            this.lblYear = new System.Windows.Forms.Label();
            this.lblSettlement = new System.Windows.Forms.Label();
            this.cbSource = new System.Windows.Forms.ComboBox();
            this.edPage = new System.Windows.Forms.TextBox();
            this.edSourceYear = new System.Windows.Forms.TextBox();
            this.edPlace = new System.Windows.Forms.TextBox();
            this.sgData = new System.Windows.Forms.Panel();
            this.dataGridView1 = new System.Windows.Forms.DataGridView();
            this.cbPersonLink = new System.Windows.Forms.ComboBox();
            this.rgSourceKind = new System.Windows.Forms.GroupBox();
            this.rbSK_Met = new System.Windows.Forms.RadioButton();
            this.rbSK_Rev = new System.Windows.Forms.RadioButton();
            this.gbMetrics = new System.Windows.Forms.GroupBox();
            this.lblEventDate = new System.Windows.Forms.Label();
            this.lblEventType = new System.Windows.Forms.Label();
            this.edEventDate = new System.Windows.Forms.MaskedTextBox();
            this.cbEventType = new System.Windows.Forms.ComboBox();
            this.PageControl1.SuspendLayout();
            this.tsSimpleInput.SuspendLayout();
            this.Panel1.SuspendLayout();
            this.Panel2.SuspendLayout();
            this.tsSourceInput.SuspendLayout();
            this.sgData.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).BeginInit();
            this.rgSourceKind.SuspendLayout();
            this.gbMetrics.SuspendLayout();
            this.SuspendLayout();
            // 
            // btnParse
            // 
            this.btnParse.Location = new System.Drawing.Point(464, 424);
            this.btnParse.Name = "btnParse";
            this.btnParse.Size = new System.Drawing.Size(81, 25);
            this.btnParse.TabIndex = 1;
            this.btnParse.Text = "btnParse";
            this.btnParse.Click += new System.EventHandler(this.btnParse_Click);
            // 
            // btnClose
            // 
            this.btnClose.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnClose.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnClose.Location = new System.Drawing.Point(560, 424);
            this.btnClose.Name = "btnClose";
            this.btnClose.Size = new System.Drawing.Size(81, 25);
            this.btnClose.TabIndex = 2;
            this.btnClose.Text = "btnClose";
            this.btnClose.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // PageControl1
            // 
            this.PageControl1.Controls.Add(this.tsSimpleInput);
            this.PageControl1.Controls.Add(this.tsSourceInput);
            this.PageControl1.Location = new System.Drawing.Point(8, 8);
            this.PageControl1.Name = "PageControl1";
            this.PageControl1.SelectedIndex = 0;
            this.PageControl1.Size = new System.Drawing.Size(633, 401);
            this.PageControl1.TabIndex = 0;
            // 
            // tsSimpleInput
            // 
            this.tsSimpleInput.Controls.Add(this.lblFullName);
            this.tsSimpleInput.Controls.Add(this.lblNote);
            this.tsSimpleInput.Controls.Add(this.btnMale);
            this.tsSimpleInput.Controls.Add(this.txtFullName);
            this.tsSimpleInput.Controls.Add(this.txtNote);
            this.tsSimpleInput.Controls.Add(this.Panel1);
            this.tsSimpleInput.Controls.Add(this.Panel2);
            this.tsSimpleInput.Location = new System.Drawing.Point(4, 22);
            this.tsSimpleInput.Name = "tsSimpleInput";
            this.tsSimpleInput.Size = new System.Drawing.Size(625, 375);
            this.tsSimpleInput.TabIndex = 0;
            this.tsSimpleInput.Text = "tsSimpleInput";
            // 
            // Label1
            // 
            this.lblFullName.Location = new System.Drawing.Point(8, 8);
            this.lblFullName.Name = "lblFullName";
            this.lblFullName.Size = new System.Drawing.Size(150, 13);
            this.lblFullName.TabIndex = 0;
            this.lblFullName.Text = "lblFullName";
            // 
            // lblNote
            // 
            this.lblNote.Location = new System.Drawing.Point(8, 232);
            this.lblNote.Name = "lblNote";
            this.lblNote.Size = new System.Drawing.Size(50, 13);
            this.lblNote.TabIndex = 6;
            this.lblNote.Text = "lblNote";
            // 
            // btnMale
            // 
            this.btnMale.Location = new System.Drawing.Point(424, 24);
            this.btnMale.Name = "btnMale";
            this.btnMale.Size = new System.Drawing.Size(49, 21);
            this.btnMale.TabIndex = 2;
            this.btnMale.Text = "btnMale";
            this.btnMale.Click += new System.EventHandler(this.btnSex_Click);
            // 
            // EditName
            // 
            this.txtFullName.Location = new System.Drawing.Point(8, 24);
            this.txtFullName.Name = "EditName";
            this.txtFullName.Size = new System.Drawing.Size(409, 21);
            this.txtFullName.TabIndex = 1;
            // 
            // MemoNote
            // 
            this.txtNote.Location = new System.Drawing.Point(8, 245);
            this.txtNote.Multiline = true;
            this.txtNote.Name = "MemoNote";
            this.txtNote.Size = new System.Drawing.Size(465, 121);
            this.txtNote.TabIndex = 7;
            // 
            // Panel1
            // 
            this.Panel1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.Panel1.Controls.Add(this.lblBirthDate);
            this.Panel1.Controls.Add(this.lblBirthPlace);
            this.Panel1.Controls.Add(this.txtBirthDate);
            this.Panel1.Controls.Add(this.txtBirthPlace);
            this.Panel1.Controls.Add(this.chkBirth);
            this.Panel1.Location = new System.Drawing.Point(8, 56);
            this.Panel1.Name = "Panel1";
            this.Panel1.Size = new System.Drawing.Size(465, 81);
            this.Panel1.TabIndex = 4;
            // 
            // Label3
            // 
            this.lblBirthDate.Location = new System.Drawing.Point(8, 32);
            this.lblBirthDate.Name = "lblBirthDate";
            this.lblBirthDate.Size = new System.Drawing.Size(90, 13);
            this.lblBirthDate.TabIndex = 1;
            this.lblBirthDate.Text = "lblBirthDate";
            // 
            // lblBirthPlace
            // 
            this.lblBirthPlace.Location = new System.Drawing.Point(112, 32);
            this.lblBirthPlace.Name = "lblBirthPlace";
            this.lblBirthPlace.Size = new System.Drawing.Size(100, 13);
            this.lblBirthPlace.TabIndex = 3;
            this.lblBirthPlace.Text = "lblBirthPlace";
            // 
            // EditBirthDate
            // 
            this.txtBirthDate.Location = new System.Drawing.Point(8, 48);
            this.txtBirthDate.Mask = "00/00/0000";
            this.txtBirthDate.Name = "EditBirthDate";
            this.txtBirthDate.Size = new System.Drawing.Size(97, 21);
            this.txtBirthDate.TabIndex = 2;
            this.txtBirthDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
            this.txtBirthDate.TextChanged += new System.EventHandler(this.txtBirthDate_TextChanged);
            // 
            // EditBirthPlace
            // 
            this.txtBirthPlace.Location = new System.Drawing.Point(112, 48);
            this.txtBirthPlace.Name = "EditBirthPlace";
            this.txtBirthPlace.Size = new System.Drawing.Size(337, 21);
            this.txtBirthPlace.TabIndex = 4;
            this.txtBirthPlace.TextChanged += new System.EventHandler(this.txtBirthDate_TextChanged);
            // 
            // CheckBirth
            // 
            this.chkBirth.Location = new System.Drawing.Point(8, 8);
            this.chkBirth.Name = "CheckBirth";
            this.chkBirth.Size = new System.Drawing.Size(96, 17);
            this.chkBirth.TabIndex = 0;
            this.chkBirth.Text = "CheckBirth";
            // 
            // Panel2
            // 
            this.Panel2.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.Panel2.Controls.Add(this.lblDeathDate);
            this.Panel2.Controls.Add(this.lblDeathPlace);
            this.Panel2.Controls.Add(this.chkDeath);
            this.Panel2.Controls.Add(this.txtDeathDate);
            this.Panel2.Controls.Add(this.txtDeathPlace);
            this.Panel2.Location = new System.Drawing.Point(8, 144);
            this.Panel2.Name = "Panel2";
            this.Panel2.Size = new System.Drawing.Size(465, 81);
            this.Panel2.TabIndex = 5;
            // 
            // Label6
            // 
            this.lblDeathDate.Location = new System.Drawing.Point(8, 32);
            this.lblDeathDate.Name = "lblDeathDate";
            this.lblDeathDate.Size = new System.Drawing.Size(90, 13);
            this.lblDeathDate.TabIndex = 1;
            this.lblDeathDate.Text = "lblDeathDate";
            // 
            // Label7
            // 
            this.lblDeathPlace.Location = new System.Drawing.Point(112, 32);
            this.lblDeathPlace.Name = "lblDeathPlace";
            this.lblDeathPlace.Size = new System.Drawing.Size(100, 13);
            this.lblDeathPlace.TabIndex = 3;
            this.lblDeathPlace.Text = "lblDeathPlace";
            // 
            // CheckDeath
            // 
            this.chkDeath.Location = new System.Drawing.Point(8, 8);
            this.chkDeath.Name = "CheckDeath";
            this.chkDeath.Size = new System.Drawing.Size(95, 17);
            this.chkDeath.TabIndex = 0;
            this.chkDeath.Text = "CheckDeath";
            // 
            // EditDeathDate
            // 
            this.txtDeathDate.Location = new System.Drawing.Point(8, 48);
            this.txtDeathDate.Mask = "00/00/0000";
            this.txtDeathDate.Name = "EditDeathDate";
            this.txtDeathDate.Size = new System.Drawing.Size(97, 21);
            this.txtDeathDate.TabIndex = 2;
            this.txtDeathDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
            this.txtDeathDate.TextChanged += new System.EventHandler(this.txtDeathDate_TextChanged);
            // 
            // EditDeathPlace
            // 
            this.txtDeathPlace.Location = new System.Drawing.Point(112, 48);
            this.txtDeathPlace.Name = "EditDeathPlace";
            this.txtDeathPlace.Size = new System.Drawing.Size(337, 21);
            this.txtDeathPlace.TabIndex = 4;
            this.txtDeathPlace.TextChanged += new System.EventHandler(this.txtDeathDate_TextChanged);
            // 
            // tsSourceInput
            // 
            this.tsSourceInput.Controls.Add(this.lblSource);
            this.tsSourceInput.Controls.Add(this.lblPage);
            this.tsSourceInput.Controls.Add(this.lblYear);
            this.tsSourceInput.Controls.Add(this.lblSettlement);
            this.tsSourceInput.Controls.Add(this.cbSource);
            this.tsSourceInput.Controls.Add(this.edPage);
            this.tsSourceInput.Controls.Add(this.edSourceYear);
            this.tsSourceInput.Controls.Add(this.edPlace);
            this.tsSourceInput.Controls.Add(this.sgData);
            this.tsSourceInput.Controls.Add(this.cbPersonLink);
            this.tsSourceInput.Controls.Add(this.rgSourceKind);
            this.tsSourceInput.Controls.Add(this.gbMetrics);
            this.tsSourceInput.Location = new System.Drawing.Point(4, 22);
            this.tsSourceInput.Name = "tsSourceInput";
            this.tsSourceInput.Size = new System.Drawing.Size(625, 375);
            this.tsSourceInput.TabIndex = 1;
            this.tsSourceInput.Text = "tsSourceInput";
            // 
            // lblSource
            // 
            this.lblSource.Location = new System.Drawing.Point(8, 56);
            this.lblSource.Name = "lblSource";
            this.lblSource.Size = new System.Drawing.Size(55, 13);
            this.lblSource.TabIndex = 0;
            this.lblSource.Text = "lblSource";
            // 
            // lblPage
            // 
            this.lblPage.Location = new System.Drawing.Point(304, 56);
            this.lblPage.Name = "lblPage";
            this.lblPage.Size = new System.Drawing.Size(85, 13);
            this.lblPage.TabIndex = 1;
            this.lblPage.Text = "lblPage";
            // 
            // lblYear
            // 
            this.lblYear.Location = new System.Drawing.Point(520, 56);
            this.lblYear.Name = "lblYear";
            this.lblYear.Size = new System.Drawing.Size(30, 13);
            this.lblYear.TabIndex = 2;
            this.lblYear.Text = "lblYear";
            // 
            // lblSettlement
            // 
            this.lblSettlement.Location = new System.Drawing.Point(8, 88);
            this.lblSettlement.Name = "lblSettlement";
            this.lblSettlement.Size = new System.Drawing.Size(105, 13);
            this.lblSettlement.TabIndex = 3;
            this.lblSettlement.Text = "lblSettlement";
            // 
            // cbSource
            // 
            this.cbSource.Location = new System.Drawing.Point(64, 48);
            this.cbSource.Name = "cbSource";
            this.cbSource.Size = new System.Drawing.Size(225, 21);
            this.cbSource.TabIndex = 0;
            // 
            // edPage
            // 
            this.edPage.Location = new System.Drawing.Point(392, 48);
            this.edPage.Name = "edPage";
            this.edPage.Size = new System.Drawing.Size(112, 21);
            this.edPage.TabIndex = 1;
            // 
            // edSourceYear
            // 
            this.edSourceYear.Location = new System.Drawing.Point(560, 48);
            this.edSourceYear.MaxLength = 4;
            this.edSourceYear.Name = "edSourceYear";
            this.edSourceYear.Size = new System.Drawing.Size(57, 21);
            this.edSourceYear.TabIndex = 2;
            this.edSourceYear.Text = "    ";
            // 
            // edPlace
            // 
            this.edPlace.Location = new System.Drawing.Point(120, 80);
            this.edPlace.Name = "edPlace";
            this.edPlace.Size = new System.Drawing.Size(497, 21);
            this.edPlace.TabIndex = 3;
            // 
            // sgData
            // 
            this.sgData.Controls.Add(this.dataGridView1);
            this.sgData.Location = new System.Drawing.Point(8, 184);
            this.sgData.Name = "sgData";
            this.sgData.Size = new System.Drawing.Size(609, 177);
            this.sgData.TabIndex = 4;
            // 
            // dataGridView1
            //
            this.dataGridView1.AllowUserToResizeRows = false;
            this.dataGridView1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dataGridView1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.dataGridView1.Location = new System.Drawing.Point(0, 0);
            this.dataGridView1.MultiSelect = false;
            this.dataGridView1.Name = "dataGridView1";
            this.dataGridView1.Size = new System.Drawing.Size(609, 177);
            this.dataGridView1.TabIndex = 0;
            //
            // cbPersonLink
            // 
            this.cbPersonLink.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbPersonLink.Location = new System.Drawing.Point(336, 296);
            this.cbPersonLink.Name = "cbPersonLink";
            this.cbPersonLink.Size = new System.Drawing.Size(145, 21);
            this.cbPersonLink.TabIndex = 5;
            this.cbPersonLink.Visible = false;
            // 
            // rgSourceKind
            // 
            this.rgSourceKind.Controls.Add(this.rbSK_Met);
            this.rgSourceKind.Controls.Add(this.rbSK_Rev);
            this.rgSourceKind.Location = new System.Drawing.Point(8, 0);
            this.rgSourceKind.Name = "rgSourceKind";
            this.rgSourceKind.Size = new System.Drawing.Size(609, 38);
            this.rgSourceKind.TabIndex = 6;
            this.rgSourceKind.TabStop = false;
            this.rgSourceKind.Text = "rgSourceKind";
            // 
            // radioButton2
            // 
            this.rbSK_Met.Location = new System.Drawing.Point(296, 14);
            this.rbSK_Met.Name = "rbSK_Met";
            this.rbSK_Met.Size = new System.Drawing.Size(273, 18);
            this.rbSK_Met.TabIndex = 1;
            this.rbSK_Met.Text = "rbSK_Met";
            this.rbSK_Met.UseVisualStyleBackColor = true;
            this.rbSK_Met.CheckedChanged += new System.EventHandler(this.rbX_CheckedChanged);
            // 
            // radioButton1
            // 
            this.rbSK_Rev.Checked = true;
            this.rbSK_Rev.Location = new System.Drawing.Point(8, 14);
            this.rbSK_Rev.Name = "rbSK_Rev";
            this.rbSK_Rev.Size = new System.Drawing.Size(273, 18);
            this.rbSK_Rev.TabIndex = 0;
            this.rbSK_Rev.TabStop = true;
            this.rbSK_Rev.Text = "rbSK_Rev";
            this.rbSK_Rev.UseVisualStyleBackColor = true;
            this.rbSK_Rev.CheckedChanged += new System.EventHandler(this.rbX_CheckedChanged);
            // 
            // gbMetrics
            // 
            this.gbMetrics.Controls.Add(this.lblEventDate);
            this.gbMetrics.Controls.Add(this.lblEventType);
            this.gbMetrics.Controls.Add(this.edEventDate);
            this.gbMetrics.Controls.Add(this.cbEventType);
            this.gbMetrics.Enabled = false;
            this.gbMetrics.Location = new System.Drawing.Point(8, 120);
            this.gbMetrics.Name = "gbMetrics";
            this.gbMetrics.Size = new System.Drawing.Size(609, 50);
            this.gbMetrics.TabIndex = 7;
            this.gbMetrics.TabStop = false;
            this.gbMetrics.Text = "gbMetrics";
            // 
            // Label11
            // 
            this.lblEventDate.Location = new System.Drawing.Point(8, 24);
            this.lblEventDate.Name = "lblEventDate";
            this.lblEventDate.Size = new System.Drawing.Size(80, 13);
            this.lblEventDate.TabIndex = 0;
            this.lblEventDate.Text = "lblEventDate";
            // 
            // Label12
            // 
            this.lblEventType.Location = new System.Drawing.Point(248, 24);
            this.lblEventType.Name = "lblEventType";
            this.lblEventType.Size = new System.Drawing.Size(70, 13);
            this.lblEventType.TabIndex = 1;
            this.lblEventType.Text = "lblEventType";
            // 
            // edEventDate
            // 
            this.edEventDate.Location = new System.Drawing.Point(96, 16);
            this.edEventDate.Mask = "00/00/0000";
            this.edEventDate.Name = "edEventDate";
            this.edEventDate.Size = new System.Drawing.Size(129, 21);
            this.edEventDate.TabIndex = 0;
            this.edEventDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
            // 
            // cbEventType
            // 
            this.cbEventType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbEventType.Location = new System.Drawing.Point(328, 16);
            this.cbEventType.Name = "cbEventType";
            this.cbEventType.Size = new System.Drawing.Size(145, 21);
            this.cbEventType.TabIndex = 1;
            // 
            // FlowInputDlg
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
            this.CancelButton = this.btnClose;
            this.ClientSize = new System.Drawing.Size(649, 457);
            this.Controls.Add(this.btnParse);
            this.Controls.Add(this.btnClose);
            this.Controls.Add(this.PageControl1);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "FlowInputDlg";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "FlowInputDlg";
            this.PageControl1.ResumeLayout(false);
            this.tsSimpleInput.ResumeLayout(false);
            this.tsSimpleInput.PerformLayout();
            this.Panel1.ResumeLayout(false);
            this.Panel1.PerformLayout();
            this.Panel2.ResumeLayout(false);
            this.Panel2.PerformLayout();
            this.tsSourceInput.ResumeLayout(false);
            this.tsSourceInput.PerformLayout();
            this.sgData.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).EndInit();
            this.rgSourceKind.ResumeLayout(false);
            this.gbMetrics.ResumeLayout(false);
            this.gbMetrics.PerformLayout();
            this.ResumeLayout(false);
        }
    }
}
