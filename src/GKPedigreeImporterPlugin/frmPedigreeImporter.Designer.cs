namespace GKPedigreeImporterPlugin
{
    partial class frmPedigreeImporter
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
            	if (this.fImporter != null) this.fImporter.Dispose();
                if (components != null) components.Dispose();
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(frmPedigreeImporter));
            this.Label3 = new System.Windows.Forms.Label();
            this.edImportFile = new System.Windows.Forms.TextBox();
            this.btnImportFileChoose = new System.Windows.Forms.Button();
            this.OpenDialog2 = new System.Windows.Forms.OpenFileDialog();
            this.tabControl1 = new BSLib.Controls.WizardPages();
            this.pageSelect = new System.Windows.Forms.TabPage();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.chkSurnamesNormalize = new System.Windows.Forms.CheckBox();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.cbDateSeparator = new System.Windows.Forms.ComboBox();
            this.label6 = new System.Windows.Forms.Label();
            this.cbDatesFormat = new System.Windows.Forms.ComboBox();
            this.label5 = new System.Windows.Forms.Label();
            this.cbGenerationFormat = new System.Windows.Forms.ComboBox();
            this.label4 = new System.Windows.Forms.Label();
            this.cbNameFormat = new System.Windows.Forms.ComboBox();
            this.label2 = new System.Windows.Forms.Label();
            this.cbPersonSeparator = new System.Windows.Forms.ComboBox();
            this.label1 = new System.Windows.Forms.Label();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.rbNumsDAboville = new System.Windows.Forms.RadioButton();
            this.rbNumsKonovalov = new System.Windows.Forms.RadioButton();
            this.rbNumsUnknown = new System.Windows.Forms.RadioButton();
            this.pageResult = new System.Windows.Forms.TabPage();
            this.lbLog = new System.Windows.Forms.ListBox();
            this.columnHeader4 = new System.Windows.Forms.ColumnHeader();
            this.btnNext = new System.Windows.Forms.Button();
            this.btnBack = new System.Windows.Forms.Button();
            this.btnClose = new System.Windows.Forms.Button();
            this.groupBox4 = new System.Windows.Forms.GroupBox();
            this.chkSpecial_1 = new System.Windows.Forms.CheckBox();
            this.tabControl1.SuspendLayout();
            this.pageSelect.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.groupBox1.SuspendLayout();
            this.pageResult.SuspendLayout();
            this.groupBox4.SuspendLayout();
            this.SuspendLayout();
            // 
            // Label3
            // 
            this.Label3.Location = new System.Drawing.Point(7, 19);
            this.Label3.Margin = new System.Windows.Forms.Padding(5, 0, 5, 0);
            this.Label3.Name = "Label3";
            this.Label3.Size = new System.Drawing.Size(55, 17);
            this.Label3.TabIndex = 4;
            this.Label3.Text = "Файл";
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
            this.btnImportFileChoose.Text = "Выбрать...";
            this.btnImportFileChoose.Click += new System.EventHandler(this.btnImportFileChoose_Click);
            // 
            // OpenDialog2
            // 
            this.OpenDialog2.Filter = resources.GetString("OpenDialog2.Filter");
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
            this.pageSelect.Controls.Add(this.groupBox4);
            this.pageSelect.Controls.Add(this.groupBox3);
            this.pageSelect.Controls.Add(this.groupBox2);
            this.pageSelect.Controls.Add(this.groupBox1);
            this.pageSelect.Controls.Add(this.Label3);
            this.pageSelect.Controls.Add(this.edImportFile);
            this.pageSelect.Controls.Add(this.btnImportFileChoose);
            this.pageSelect.Location = new System.Drawing.Point(4, 26);
            this.pageSelect.Name = "pageSelect";
            this.pageSelect.Padding = new System.Windows.Forms.Padding(3);
            this.pageSelect.Size = new System.Drawing.Size(935, 484);
            this.pageSelect.TabIndex = 0;
            this.pageSelect.Text = "pageSelect";
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.chkSurnamesNormalize);
            this.groupBox3.Location = new System.Drawing.Point(397, 247);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(531, 58);
            this.groupBox3.TabIndex = 11;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "Параметры преобразований";
            // 
            // chkSurnamesNormalize
            // 
            this.chkSurnamesNormalize.Location = new System.Drawing.Point(6, 23);
            this.chkSurnamesNormalize.Name = "chkSurnamesNormalize";
            this.chkSurnamesNormalize.Size = new System.Drawing.Size(519, 24);
            this.chkSurnamesNormalize.TabIndex = 0;
            this.chkSurnamesNormalize.Text = "Нормализовать фамилии (ПЕТРОВ -> Петров)";
            this.chkSurnamesNormalize.UseVisualStyleBackColor = true;
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.cbDateSeparator);
            this.groupBox2.Controls.Add(this.label6);
            this.groupBox2.Controls.Add(this.cbDatesFormat);
            this.groupBox2.Controls.Add(this.label5);
            this.groupBox2.Controls.Add(this.cbGenerationFormat);
            this.groupBox2.Controls.Add(this.label4);
            this.groupBox2.Controls.Add(this.cbNameFormat);
            this.groupBox2.Controls.Add(this.label2);
            this.groupBox2.Controls.Add(this.cbPersonSeparator);
            this.groupBox2.Controls.Add(this.label1);
            this.groupBox2.Location = new System.Drawing.Point(397, 51);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(531, 190);
            this.groupBox2.TabIndex = 10;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Параметры текстовых росписей";
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
            // label6
            // 
            this.label6.Location = new System.Drawing.Point(6, 156);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(333, 25);
            this.label6.TabIndex = 6;
            this.label6.Text = "Разделитель в датах";
            // 
            // cbDatesFormat
            // 
            this.cbDatesFormat.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbDatesFormat.Enabled = false;
            this.cbDatesFormat.FormattingEnabled = true;
            this.cbDatesFormat.Items.AddRange(new object[] {
                                    "ДД/ММ/ГГГГ",
                                    "ГГГГ/ММ/ДД"});
            this.cbDatesFormat.Location = new System.Drawing.Point(344, 122);
            this.cbDatesFormat.Name = "cbDatesFormat";
            this.cbDatesFormat.Size = new System.Drawing.Size(181, 25);
            this.cbDatesFormat.TabIndex = 5;
            // 
            // label5
            // 
            this.label5.Location = new System.Drawing.Point(6, 125);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(333, 25);
            this.label5.TabIndex = 4;
            this.label5.Text = "Формат дат";
            // 
            // cbGenerationFormat
            // 
            this.cbGenerationFormat.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbGenerationFormat.FormattingEnabled = true;
            this.cbGenerationFormat.Items.AddRange(new object[] {
                                    "I, II, III, IV...",
                                    "Поколение N"});
            this.cbGenerationFormat.Location = new System.Drawing.Point(344, 91);
            this.cbGenerationFormat.Name = "cbGenerationFormat";
            this.cbGenerationFormat.Size = new System.Drawing.Size(181, 25);
            this.cbGenerationFormat.TabIndex = 3;
            // 
            // label4
            // 
            this.label4.Location = new System.Drawing.Point(6, 94);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(333, 25);
            this.label4.TabIndex = 2;
            this.label4.Text = "Формат строки поколения";
            // 
            // cbNameFormat
            // 
            this.cbNameFormat.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbNameFormat.FormattingEnabled = true;
            this.cbNameFormat.Items.AddRange(new object[] {
                                    "Имя Отчество Фамилия",
                                    "Фамилия Имя Отчество"});
            this.cbNameFormat.Location = new System.Drawing.Point(345, 60);
            this.cbNameFormat.Name = "cbNameFormat";
            this.cbNameFormat.Size = new System.Drawing.Size(181, 25);
            this.cbNameFormat.TabIndex = 1;
            // 
            // label2
            // 
            this.label2.Location = new System.Drawing.Point(6, 64);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(333, 25);
            this.label2.TabIndex = 0;
            this.label2.Text = "Формат ФИО в персональной строке";
            // 
            // cbPersonSeparator
            // 
            this.cbPersonSeparator.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbPersonSeparator.Enabled = false;
            this.cbPersonSeparator.FormattingEnabled = true;
            this.cbPersonSeparator.Items.AddRange(new object[] {
                                    "нет специального",
                                    ";",
                                    ","});
            this.cbPersonSeparator.Location = new System.Drawing.Point(345, 28);
            this.cbPersonSeparator.Name = "cbPersonSeparator";
            this.cbPersonSeparator.Size = new System.Drawing.Size(181, 25);
            this.cbPersonSeparator.TabIndex = 1;
            // 
            // label1
            // 
            this.label1.Location = new System.Drawing.Point(6, 32);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(333, 25);
            this.label1.TabIndex = 0;
            this.label1.Text = "Разделитель данных в персональной строке";
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.rbNumsDAboville);
            this.groupBox1.Controls.Add(this.rbNumsKonovalov);
            this.groupBox1.Controls.Add(this.rbNumsUnknown);
            this.groupBox1.Location = new System.Drawing.Point(6, 51);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(385, 129);
            this.groupBox1.TabIndex = 9;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Формат нумерации персон";
            // 
            // rbNumsDAboville
            // 
            this.rbNumsDAboville.Enabled = false;
            this.rbNumsDAboville.Location = new System.Drawing.Point(14, 92);
            this.rbNumsDAboville.Name = "rbNumsDAboville";
            this.rbNumsDAboville.Size = new System.Drawing.Size(314, 25);
            this.rbNumsDAboville.TabIndex = 0;
            this.rbNumsDAboville.TabStop = true;
            this.rbNumsDAboville.Text = "по Д\'Абовиллю (пока не поддерживается)";
            this.rbNumsDAboville.UseVisualStyleBackColor = true;
            // 
            // rbNumsKonovalov
            // 
            this.rbNumsKonovalov.Location = new System.Drawing.Point(14, 59);
            this.rbNumsKonovalov.Name = "rbNumsKonovalov";
            this.rbNumsKonovalov.Size = new System.Drawing.Size(314, 25);
            this.rbNumsKonovalov.TabIndex = 0;
            this.rbNumsKonovalov.TabStop = true;
            this.rbNumsKonovalov.Text = "по Коновалову";
            this.rbNumsKonovalov.UseVisualStyleBackColor = true;
            // 
            // rbNumsUnknown
            // 
            this.rbNumsUnknown.Location = new System.Drawing.Point(14, 27);
            this.rbNumsUnknown.Name = "rbNumsUnknown";
            this.rbNumsUnknown.Size = new System.Drawing.Size(314, 25);
            this.rbNumsUnknown.TabIndex = 0;
            this.rbNumsUnknown.TabStop = true;
            this.rbNumsUnknown.Text = "неизвестно";
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
            this.btnNext.Text = "Вперед >";
            this.btnNext.Click += new System.EventHandler(this.btnNext_Click);
            // 
            // btnBack
            // 
            this.btnBack.Location = new System.Drawing.Point(614, 535);
            this.btnBack.Margin = new System.Windows.Forms.Padding(5, 4, 5, 4);
            this.btnBack.Name = "btnBack";
            this.btnBack.Size = new System.Drawing.Size(107, 33);
            this.btnBack.TabIndex = 9;
            this.btnBack.Text = "< Назад";
            this.btnBack.Click += new System.EventHandler(this.btnBack_Click);
            // 
            // btnClose
            // 
            this.btnClose.Location = new System.Drawing.Point(847, 534);
            this.btnClose.Margin = new System.Windows.Forms.Padding(5, 4, 5, 4);
            this.btnClose.Name = "btnClose";
            this.btnClose.Size = new System.Drawing.Size(107, 33);
            this.btnClose.TabIndex = 10;
            this.btnClose.Text = "Закрыть";
            this.btnClose.Click += new System.EventHandler(this.btnClose_Click);
            // 
            // groupBox4
            // 
            this.groupBox4.Controls.Add(this.chkSpecial_1);
            this.groupBox4.Location = new System.Drawing.Point(397, 311);
            this.groupBox4.Name = "groupBox4";
            this.groupBox4.Size = new System.Drawing.Size(531, 58);
            this.groupBox4.TabIndex = 11;
            this.groupBox4.TabStop = false;
            this.groupBox4.Text = "Специальные настройки формата персональных строк";
            // 
            // chkSpecial_1
            // 
            this.chkSpecial_1.Location = new System.Drawing.Point(6, 23);
            this.chkSpecial_1.Name = "chkSpecial_1";
            this.chkSpecial_1.Size = new System.Drawing.Size(519, 24);
            this.chkSpecial_1.TabIndex = 0;
            this.chkSpecial_1.Text = "1) \"Номер. Имя (*рождение +смерть)\" - даты в скобках";
            this.chkSpecial_1.UseVisualStyleBackColor = true;
            // 
            // frmPedigreeImporter
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
            this.Name = "frmPedigreeImporter";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "frmP2Main";
            this.tabControl1.ResumeLayout(false);
            this.pageSelect.ResumeLayout(false);
            this.pageSelect.PerformLayout();
            this.groupBox3.ResumeLayout(false);
            this.groupBox2.ResumeLayout(false);
            this.groupBox1.ResumeLayout(false);
            this.pageResult.ResumeLayout(false);
            this.groupBox4.ResumeLayout(false);
            this.ResumeLayout(false);
        }
        private System.Windows.Forms.CheckBox chkSpecial_1;
        private System.Windows.Forms.GroupBox groupBox4;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.ComboBox cbDatesFormat;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.ComboBox cbDateSeparator;
        private System.Windows.Forms.CheckBox chkSurnamesNormalize;
        private System.Windows.Forms.GroupBox groupBox3;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.ComboBox cbGenerationFormat;
        private System.Windows.Forms.Button btnClose;
        private System.Windows.Forms.ColumnHeader columnHeader4;
        private System.Windows.Forms.ListBox lbLog;
        private System.Windows.Forms.TabPage pageResult;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.ComboBox cbNameFormat;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.ComboBox cbPersonSeparator;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.RadioButton rbNumsUnknown;
        private System.Windows.Forms.RadioButton rbNumsKonovalov;
        private System.Windows.Forms.RadioButton rbNumsDAboville;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Button btnBack;
        private System.Windows.Forms.Button btnNext;
        private System.Windows.Forms.TabPage pageSelect;
        private BSLib.Controls.WizardPages tabControl1;
        private System.Windows.Forms.OpenFileDialog OpenDialog2;
        private System.Windows.Forms.Button btnImportFileChoose;
        private System.Windows.Forms.TextBox edImportFile;
        private System.Windows.Forms.Label Label3;

        #endregion

    }
}