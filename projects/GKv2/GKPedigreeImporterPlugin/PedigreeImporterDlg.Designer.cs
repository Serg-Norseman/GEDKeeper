namespace GKPedigreeImporterPlugin
{
    partial class PedigreeImporterDlg
    {
        private System.ComponentModel.IContainer components = null;

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
            	if (this.fImporter != null) this.fImporter.Dispose();
                if (components != null) components.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            this.lblFile = new System.Windows.Forms.Label();
            this.edImportFile = new System.Windows.Forms.TextBox();
            this.btnImportFileChoose = new System.Windows.Forms.Button();
            this.tabControl1 = new GKUI.Components.WizardPages();
            this.pageSelect = new System.Windows.Forms.TabPage();
            this.grpConversionParams = new System.Windows.Forms.GroupBox();
            this.chkSurnamesNormalize = new System.Windows.Forms.CheckBox();
            this.grpTextPedigreesParams = new System.Windows.Forms.GroupBox();
            this.cbDateSeparator = new System.Windows.Forms.ComboBox();
            this.lblDateSeparator = new System.Windows.Forms.Label();
            this.cbDatesFormat = new System.Windows.Forms.ComboBox();
            this.lblDateFormat = new System.Windows.Forms.Label();
            this.cbGenerationFormat = new System.Windows.Forms.ComboBox();
            this.lblGenerationFormat = new System.Windows.Forms.Label();
            this.cbNameFormat = new System.Windows.Forms.ComboBox();
            this.lblSurnameFormat = new System.Windows.Forms.Label();
            this.cbPersonSeparator = new System.Windows.Forms.ComboBox();
            this.lblPersonLineSeparator = new System.Windows.Forms.Label();
            this.grpPersonIdFormat = new System.Windows.Forms.GroupBox();
            this.rbNumsDAboville = new System.Windows.Forms.RadioButton();
            this.rbNumsKonovalov = new System.Windows.Forms.RadioButton();
            this.rbNumsUnknown = new System.Windows.Forms.RadioButton();
            this.pageResult = new System.Windows.Forms.TabPage();
            this.lbLog = new System.Windows.Forms.ListBox();
            this.columnHeader4 = new System.Windows.Forms.ColumnHeader();
            this.btnNext = new System.Windows.Forms.Button();
            this.btnBack = new System.Windows.Forms.Button();
            this.btnClose = new System.Windows.Forms.Button();
            this.grpPersonLineSpecials = new System.Windows.Forms.GroupBox();
            this.chkSpecial_1 = new System.Windows.Forms.CheckBox();
            this.tabControl1.SuspendLayout();
            this.pageSelect.SuspendLayout();
            this.grpConversionParams.SuspendLayout();
            this.grpTextPedigreesParams.SuspendLayout();
            this.grpPersonIdFormat.SuspendLayout();
            this.pageResult.SuspendLayout();
            this.grpPersonLineSpecials.SuspendLayout();
            this.SuspendLayout();
            // 
            // lblFile
            // 
            this.lblFile.Location = new System.Drawing.Point(7, 19);
            this.lblFile.Margin = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.lblFile.Name = "lblFile";
            this.lblFile.Size = new System.Drawing.Size(55, 17);
            this.lblFile.TabIndex = 4;
            this.lblFile.Text = "lblFile";
            // 
            // edImportFile
            // 
            this.edImportFile.Location = new System.Drawing.Point(77, 16);
            this.edImportFile.Margin = new System.Windows.Forms.Padding(5, 4, 5, 4);
            this.edImportFile.Name = "edImportFile";
            this.edImportFile.ReadOnly = true;
            this.edImportFile.Size = new System.Drawing.Size(735, 24);
            this.edImportFile.TabIndex = 3;
            // 
            // btnImportFileChoose
            // 
            this.btnImportFileChoose.Location = new System.Drawing.Point(821, 11);
            this.btnImportFileChoose.Margin = new System.Windows.Forms.Padding(5, 4, 5, 4);
            this.btnImportFileChoose.Name = "btnImportFileChoose";
            this.btnImportFileChoose.Size = new System.Drawing.Size(107, 33);
            this.btnImportFileChoose.TabIndex = 5;
            this.btnImportFileChoose.Text = "btnImportFileChoose";
            this.btnImportFileChoose.Click += new System.EventHandler(this.btnImportFileChoose_Click);
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.pageSelect);
            this.tabControl1.Controls.Add(this.pageResult);
            this.tabControl1.Location = new System.Drawing.Point(11, 13);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(943, 514);
            this.tabControl1.TabIndex = 7;
            // 
            // pageSelect
            // 
            this.pageSelect.BackColor = System.Drawing.SystemColors.Control;
            this.pageSelect.Controls.Add(this.grpPersonLineSpecials);
            this.pageSelect.Controls.Add(this.grpConversionParams);
            this.pageSelect.Controls.Add(this.grpTextPedigreesParams);
            this.pageSelect.Controls.Add(this.grpPersonIdFormat);
            this.pageSelect.Controls.Add(this.lblFile);
            this.pageSelect.Controls.Add(this.edImportFile);
            this.pageSelect.Controls.Add(this.btnImportFileChoose);
            this.pageSelect.Location = new System.Drawing.Point(4, 26);
            this.pageSelect.Name = "pageSelect";
            this.pageSelect.Padding = new System.Windows.Forms.Padding(3);
            this.pageSelect.Size = new System.Drawing.Size(935, 484);
            this.pageSelect.TabIndex = 0;
            this.pageSelect.Text = "pageSelect";
            // 
            // grpConversionParams
            // 
            this.grpConversionParams.Controls.Add(this.chkSurnamesNormalize);
            this.grpConversionParams.Location = new System.Drawing.Point(397, 247);
            this.grpConversionParams.Name = "grpConversionParams";
            this.grpConversionParams.Size = new System.Drawing.Size(531, 58);
            this.grpConversionParams.TabIndex = 11;
            this.grpConversionParams.TabStop = false;
            this.grpConversionParams.Text = "grpConversionParams";
            // 
            // chkSurnamesNormalize
            // 
            this.chkSurnamesNormalize.Location = new System.Drawing.Point(6, 23);
            this.chkSurnamesNormalize.Name = "chkSurnamesNormalize";
            this.chkSurnamesNormalize.Size = new System.Drawing.Size(519, 24);
            this.chkSurnamesNormalize.TabIndex = 0;
            this.chkSurnamesNormalize.Text = "chkSurnamesNormalize";
            this.chkSurnamesNormalize.UseVisualStyleBackColor = true;
            // 
            // grpTextPedigreesParams
            // 
            this.grpTextPedigreesParams.Controls.Add(this.cbDateSeparator);
            this.grpTextPedigreesParams.Controls.Add(this.lblDateSeparator);
            this.grpTextPedigreesParams.Controls.Add(this.cbDatesFormat);
            this.grpTextPedigreesParams.Controls.Add(this.lblDateFormat);
            this.grpTextPedigreesParams.Controls.Add(this.cbGenerationFormat);
            this.grpTextPedigreesParams.Controls.Add(this.lblGenerationFormat);
            this.grpTextPedigreesParams.Controls.Add(this.cbNameFormat);
            this.grpTextPedigreesParams.Controls.Add(this.lblSurnameFormat);
            this.grpTextPedigreesParams.Controls.Add(this.cbPersonSeparator);
            this.grpTextPedigreesParams.Controls.Add(this.lblPersonLineSeparator);
            this.grpTextPedigreesParams.Location = new System.Drawing.Point(397, 51);
            this.grpTextPedigreesParams.Name = "grpTextPedigreesParams";
            this.grpTextPedigreesParams.Size = new System.Drawing.Size(531, 190);
            this.grpTextPedigreesParams.TabIndex = 10;
            this.grpTextPedigreesParams.TabStop = false;
            this.grpTextPedigreesParams.Text = "grpTextPedigreesParams";
            // 
            // cbDateSeparator
            // 
            this.cbDateSeparator.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbDateSeparator.Enabled = false;
            this.cbDateSeparator.FormattingEnabled = true;
            this.cbDateSeparator.Items.AddRange(new object[] {
                                    ".",
                                    "/",
                                    "-"});
            this.cbDateSeparator.Location = new System.Drawing.Point(344, 153);
            this.cbDateSeparator.Name = "cbDateSeparator";
            this.cbDateSeparator.Size = new System.Drawing.Size(181, 25);
            this.cbDateSeparator.TabIndex = 7;
            // 
            // lblDateSeparator
            // 
            this.lblDateSeparator.Location = new System.Drawing.Point(6, 156);
            this.lblDateSeparator.Name = "lblDateSeparator";
            this.lblDateSeparator.Size = new System.Drawing.Size(333, 25);
            this.lblDateSeparator.TabIndex = 6;
            this.lblDateSeparator.Text = "lblDateSeparator";
            // 
            // cbDatesFormat
            // 
            this.cbDatesFormat.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbDatesFormat.Enabled = false;
            this.cbDatesFormat.FormattingEnabled = true;
            this.cbDatesFormat.Location = new System.Drawing.Point(344, 122);
            this.cbDatesFormat.Name = "cbDatesFormat";
            this.cbDatesFormat.Size = new System.Drawing.Size(181, 25);
            this.cbDatesFormat.TabIndex = 5;
            // 
            // lblDateFormat
            // 
            this.lblDateFormat.Location = new System.Drawing.Point(6, 125);
            this.lblDateFormat.Name = "lblDateFormat";
            this.lblDateFormat.Size = new System.Drawing.Size(333, 25);
            this.lblDateFormat.TabIndex = 4;
            this.lblDateFormat.Text = "lblDateFormat";
            // 
            // cbGenerationFormat
            // 
            this.cbGenerationFormat.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbGenerationFormat.FormattingEnabled = true;
            this.cbGenerationFormat.Location = new System.Drawing.Point(344, 91);
            this.cbGenerationFormat.Name = "cbGenerationFormat";
            this.cbGenerationFormat.Size = new System.Drawing.Size(181, 25);
            this.cbGenerationFormat.TabIndex = 3;
            // 
            // lblGenerationFormat
            // 
            this.lblGenerationFormat.Location = new System.Drawing.Point(6, 94);
            this.lblGenerationFormat.Name = "lblGenerationFormat";
            this.lblGenerationFormat.Size = new System.Drawing.Size(333, 25);
            this.lblGenerationFormat.TabIndex = 2;
            this.lblGenerationFormat.Text = "lblGenerationFormat";
            // 
            // cbNameFormat
            // 
            this.cbNameFormat.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbNameFormat.FormattingEnabled = true;
            this.cbNameFormat.Location = new System.Drawing.Point(345, 60);
            this.cbNameFormat.Name = "cbNameFormat";
            this.cbNameFormat.Size = new System.Drawing.Size(181, 25);
            this.cbNameFormat.TabIndex = 1;
            // 
            // lblSurnameFormat
            // 
            this.lblSurnameFormat.Location = new System.Drawing.Point(6, 64);
            this.lblSurnameFormat.Name = "lblSurnameFormat";
            this.lblSurnameFormat.Size = new System.Drawing.Size(333, 25);
            this.lblSurnameFormat.TabIndex = 0;
            this.lblSurnameFormat.Text = "lblSurnameFormat";
            // 
            // cbPersonSeparator
            // 
            this.cbPersonSeparator.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbPersonSeparator.Enabled = false;
            this.cbPersonSeparator.FormattingEnabled = true;
            this.cbPersonSeparator.Location = new System.Drawing.Point(345, 28);
            this.cbPersonSeparator.Name = "cbPersonSeparator";
            this.cbPersonSeparator.Size = new System.Drawing.Size(181, 25);
            this.cbPersonSeparator.TabIndex = 1;
            // 
            // lblPersonLineSeparator
            // 
            this.lblPersonLineSeparator.Location = new System.Drawing.Point(6, 32);
            this.lblPersonLineSeparator.Name = "lblPersonLineSeparator";
            this.lblPersonLineSeparator.Size = new System.Drawing.Size(333, 25);
            this.lblPersonLineSeparator.TabIndex = 0;
            this.lblPersonLineSeparator.Text = "lblPersonLineSeparator";
            // 
            // grpPersonIdFormat
            // 
            this.grpPersonIdFormat.Controls.Add(this.rbNumsDAboville);
            this.grpPersonIdFormat.Controls.Add(this.rbNumsKonovalov);
            this.grpPersonIdFormat.Controls.Add(this.rbNumsUnknown);
            this.grpPersonIdFormat.Location = new System.Drawing.Point(6, 51);
            this.grpPersonIdFormat.Name = "grpPersonIdFormat";
            this.grpPersonIdFormat.Size = new System.Drawing.Size(385, 129);
            this.grpPersonIdFormat.TabIndex = 9;
            this.grpPersonIdFormat.TabStop = false;
            this.grpPersonIdFormat.Text = "grpPersonIdFormat";
            // 
            // rbNumsDAboville
            // 
            this.rbNumsDAboville.Enabled = false;
            this.rbNumsDAboville.Location = new System.Drawing.Point(14, 92);
            this.rbNumsDAboville.Name = "rbNumsDAboville";
            this.rbNumsDAboville.Size = new System.Drawing.Size(314, 25);
            this.rbNumsDAboville.TabIndex = 0;
            this.rbNumsDAboville.TabStop = true;
            this.rbNumsDAboville.Text = "rbNumsDAboville";
            this.rbNumsDAboville.UseVisualStyleBackColor = true;
            // 
            // rbNumsKonovalov
            // 
            this.rbNumsKonovalov.Location = new System.Drawing.Point(14, 59);
            this.rbNumsKonovalov.Name = "rbNumsKonovalov";
            this.rbNumsKonovalov.Size = new System.Drawing.Size(314, 25);
            this.rbNumsKonovalov.TabIndex = 0;
            this.rbNumsKonovalov.TabStop = true;
            this.rbNumsKonovalov.Text = "rbNumsKonovalov";
            this.rbNumsKonovalov.UseVisualStyleBackColor = true;
            // 
            // rbNumsUnknown
            // 
            this.rbNumsUnknown.Location = new System.Drawing.Point(14, 27);
            this.rbNumsUnknown.Name = "rbNumsUnknown";
            this.rbNumsUnknown.Size = new System.Drawing.Size(314, 25);
            this.rbNumsUnknown.TabIndex = 0;
            this.rbNumsUnknown.TabStop = true;
            this.rbNumsUnknown.Text = "rbNumsUnknown";
            this.rbNumsUnknown.UseVisualStyleBackColor = true;
            // 
            // pageResult
            // 
            this.pageResult.BackColor = System.Drawing.SystemColors.Control;
            this.pageResult.Controls.Add(this.lbLog);
            this.pageResult.Location = new System.Drawing.Point(4, 25);
            this.pageResult.Name = "pageResult";
            this.pageResult.Padding = new System.Windows.Forms.Padding(3);
            this.pageResult.Size = new System.Drawing.Size(935, 485);
            this.pageResult.TabIndex = 2;
            this.pageResult.Text = "pageResult";
            // 
            // lbLog
            // 
            this.lbLog.ItemHeight = 17;
            this.lbLog.Location = new System.Drawing.Point(8, 8);
            this.lbLog.Margin = new System.Windows.Forms.Padding(5, 4, 5, 4);
            this.lbLog.Name = "lbLog";
            this.lbLog.Size = new System.Drawing.Size(919, 463);
            this.lbLog.TabIndex = 7;
            // 
            // columnHeader4
            // 
            this.columnHeader4.Text = "?";
            this.columnHeader4.Width = 871;
            // 
            // btnNext
            // 
            this.btnNext.Location = new System.Drawing.Point(730, 535);
            this.btnNext.Margin = new System.Windows.Forms.Padding(5, 4, 5, 4);
            this.btnNext.Name = "btnNext";
            this.btnNext.Size = new System.Drawing.Size(107, 33);
            this.btnNext.TabIndex = 8;
            this.btnNext.Text = "btnNext";
            this.btnNext.Click += new System.EventHandler(this.btnNext_Click);
            // 
            // btnBack
            // 
            this.btnBack.Location = new System.Drawing.Point(614, 535);
            this.btnBack.Margin = new System.Windows.Forms.Padding(5, 4, 5, 4);
            this.btnBack.Name = "btnBack";
            this.btnBack.Size = new System.Drawing.Size(107, 33);
            this.btnBack.TabIndex = 9;
            this.btnBack.Text = "btnBack";
            this.btnBack.Click += new System.EventHandler(this.btnBack_Click);
            // 
            // btnClose
            // 
            this.btnClose.Location = new System.Drawing.Point(847, 534);
            this.btnClose.Margin = new System.Windows.Forms.Padding(5, 4, 5, 4);
            this.btnClose.Name = "btnClose";
            this.btnClose.Size = new System.Drawing.Size(107, 33);
            this.btnClose.TabIndex = 10;
            this.btnClose.Text = "btnClose";
            this.btnClose.Click += new System.EventHandler(this.btnClose_Click);
            // 
            // grpPersonLineSpecials
            // 
            this.grpPersonLineSpecials.Controls.Add(this.chkSpecial_1);
            this.grpPersonLineSpecials.Location = new System.Drawing.Point(397, 311);
            this.grpPersonLineSpecials.Name = "grpPersonLineSpecials";
            this.grpPersonLineSpecials.Size = new System.Drawing.Size(531, 58);
            this.grpPersonLineSpecials.TabIndex = 11;
            this.grpPersonLineSpecials.TabStop = false;
            this.grpPersonLineSpecials.Text = "grpPersonLineSpecials";
            // 
            // chkSpecial_1
            // 
            this.chkSpecial_1.Location = new System.Drawing.Point(6, 23);
            this.chkSpecial_1.Name = "chkSpecial_1";
            this.chkSpecial_1.Size = new System.Drawing.Size(519, 24);
            this.chkSpecial_1.TabIndex = 0;
            this.chkSpecial_1.Text = "chkSpecial_1";
            this.chkSpecial_1.UseVisualStyleBackColor = true;
            // 
            // PedigreeImporterDlg
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(7, 17);
            this.ClientSize = new System.Drawing.Size(967, 580);
            this.Controls.Add(this.btnClose);
            this.Controls.Add(this.btnBack);
            this.Controls.Add(this.btnNext);
            this.Controls.Add(this.tabControl1);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Margin = new System.Windows.Forms.Padding(5, 4, 5, 4);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "PedigreeImporterDlg";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "PedigreeImporterDlg";
            this.tabControl1.ResumeLayout(false);
            this.pageSelect.ResumeLayout(false);
            this.pageSelect.PerformLayout();
            this.grpConversionParams.ResumeLayout(false);
            this.grpTextPedigreesParams.ResumeLayout(false);
            this.grpPersonIdFormat.ResumeLayout(false);
            this.pageResult.ResumeLayout(false);
            this.grpPersonLineSpecials.ResumeLayout(false);
            this.ResumeLayout(false);
        }
        private System.Windows.Forms.CheckBox chkSpecial_1;
        private System.Windows.Forms.GroupBox grpPersonLineSpecials;
        private System.Windows.Forms.Label lblDateFormat;
        private System.Windows.Forms.ComboBox cbDatesFormat;
        private System.Windows.Forms.Label lblDateSeparator;
        private System.Windows.Forms.ComboBox cbDateSeparator;
        private System.Windows.Forms.CheckBox chkSurnamesNormalize;
        private System.Windows.Forms.GroupBox grpConversionParams;
        private System.Windows.Forms.Label lblGenerationFormat;
        private System.Windows.Forms.ComboBox cbGenerationFormat;
        private System.Windows.Forms.Button btnClose;
        private System.Windows.Forms.ColumnHeader columnHeader4;
        private System.Windows.Forms.ListBox lbLog;
        private System.Windows.Forms.TabPage pageResult;
        private System.Windows.Forms.Label lblSurnameFormat;
        private System.Windows.Forms.ComboBox cbNameFormat;
        private System.Windows.Forms.Label lblPersonLineSeparator;
        private System.Windows.Forms.ComboBox cbPersonSeparator;
        private System.Windows.Forms.GroupBox grpTextPedigreesParams;
        private System.Windows.Forms.RadioButton rbNumsUnknown;
        private System.Windows.Forms.RadioButton rbNumsKonovalov;
        private System.Windows.Forms.RadioButton rbNumsDAboville;
        private System.Windows.Forms.GroupBox grpPersonIdFormat;
        private System.Windows.Forms.Button btnBack;
        private System.Windows.Forms.Button btnNext;
        private System.Windows.Forms.TabPage pageSelect;
        private GKUI.Components.WizardPages tabControl1;
        private System.Windows.Forms.Button btnImportFileChoose;
        private System.Windows.Forms.TextBox edImportFile;
        private System.Windows.Forms.Label lblFile;
    }
}
