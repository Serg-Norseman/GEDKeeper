using System;

namespace GKUI
{
	partial class TfmPersonScan
	{
		private System.Windows.Forms.Button btnParse;
		private System.Windows.Forms.Button btnClose;
		private System.Windows.Forms.TabControl PageControl1;
		private System.Windows.Forms.TabPage tsSimpleInput;
		private System.Windows.Forms.TabPage tsSourceInput;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.Button btnMale;
		private System.Windows.Forms.TextBox EditName;
		private System.Windows.Forms.TextBox MemoNote;
		private System.Windows.Forms.Panel Panel1;
		private System.Windows.Forms.Label Label3;
		private System.Windows.Forms.Label Label5;
		private System.Windows.Forms.MaskedTextBox EditBirthDate;
		private System.Windows.Forms.TextBox EditBirthPlace;
		private System.Windows.Forms.CheckBox CheckBirth;
		private System.Windows.Forms.Panel Panel2;
		private System.Windows.Forms.Label Label6;
		private System.Windows.Forms.Label Label7;
		private System.Windows.Forms.CheckBox CheckDeath;
		private System.Windows.Forms.MaskedTextBox EditDeathDate;
		private System.Windows.Forms.TextBox EditDeathPlace;
		private System.Windows.Forms.Label Label4;
		private System.Windows.Forms.Label Label8;
		private System.Windows.Forms.Label Label9;
		private System.Windows.Forms.Label Label10;
		private System.Windows.Forms.ComboBox cbSource;
		private System.Windows.Forms.TextBox edPage;
		private System.Windows.Forms.TextBox edSourceYear;
		private System.Windows.Forms.TextBox edPlace;
		private System.Windows.Forms.ComboBox cbPersonLink;
		private System.Windows.Forms.GroupBox rgSourceKind;
		private System.Windows.Forms.GroupBox gbMetrics;
		private System.Windows.Forms.Label Label11;
		private System.Windows.Forms.Label Label12;
		private System.Windows.Forms.MaskedTextBox edEventDate;
		private System.Windows.Forms.ComboBox cbEventType;
		private System.Windows.Forms.Panel sgData;

		private void InitializeComponent()
		{
			this.btnParse = new System.Windows.Forms.Button();
			this.btnClose = new System.Windows.Forms.Button();
			this.PageControl1 = new System.Windows.Forms.TabControl();
			this.tsSimpleInput = new System.Windows.Forms.TabPage();
			this.Label1 = new System.Windows.Forms.Label();
			this.Label2 = new System.Windows.Forms.Label();
			this.btnMale = new System.Windows.Forms.Button();
			this.EditName = new System.Windows.Forms.TextBox();
			this.MemoNote = new System.Windows.Forms.TextBox();
			this.Panel1 = new System.Windows.Forms.Panel();
			this.Label3 = new System.Windows.Forms.Label();
			this.Label5 = new System.Windows.Forms.Label();
			this.EditBirthDate = new System.Windows.Forms.MaskedTextBox();
			this.EditBirthPlace = new System.Windows.Forms.TextBox();
			this.CheckBirth = new System.Windows.Forms.CheckBox();
			this.Panel2 = new System.Windows.Forms.Panel();
			this.Label6 = new System.Windows.Forms.Label();
			this.Label7 = new System.Windows.Forms.Label();
			this.CheckDeath = new System.Windows.Forms.CheckBox();
			this.EditDeathDate = new System.Windows.Forms.MaskedTextBox();
			this.EditDeathPlace = new System.Windows.Forms.TextBox();
			this.tsSourceInput = new System.Windows.Forms.TabPage();
			this.Label4 = new System.Windows.Forms.Label();
			this.Label8 = new System.Windows.Forms.Label();
			this.Label9 = new System.Windows.Forms.Label();
			this.Label10 = new System.Windows.Forms.Label();
			this.cbSource = new System.Windows.Forms.ComboBox();
			this.edPage = new System.Windows.Forms.TextBox();
			this.edSourceYear = new System.Windows.Forms.TextBox();
			this.edPlace = new System.Windows.Forms.TextBox();
			this.sgData = new System.Windows.Forms.Panel();
			this.dataGridView1 = new System.Windows.Forms.DataGridView();
			this.cbPersonLink = new System.Windows.Forms.ComboBox();
			this.rgSourceKind = new System.Windows.Forms.GroupBox();
			this.radioButton2 = new System.Windows.Forms.RadioButton();
			this.radioButton1 = new System.Windows.Forms.RadioButton();
			this.gbMetrics = new System.Windows.Forms.GroupBox();
			this.Label11 = new System.Windows.Forms.Label();
			this.Label12 = new System.Windows.Forms.Label();
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
			this.btnParse.Text = "Добавить";
			this.btnParse.Click += new System.EventHandler(this.btnParseClick);
			// 
			// btnClose
			// 
			this.btnClose.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnClose.Image = global::GKResources.iBtnCancel;
			this.btnClose.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnClose.Location = new System.Drawing.Point(560, 424);
			this.btnClose.Name = "btnClose";
			this.btnClose.Size = new System.Drawing.Size(81, 25);
			this.btnClose.TabIndex = 2;
			this.btnClose.Text = "Закрыть";
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
			this.tsSimpleInput.Controls.Add(this.Label1);
			this.tsSimpleInput.Controls.Add(this.Label2);
			this.tsSimpleInput.Controls.Add(this.btnMale);
			this.tsSimpleInput.Controls.Add(this.EditName);
			this.tsSimpleInput.Controls.Add(this.MemoNote);
			this.tsSimpleInput.Controls.Add(this.Panel1);
			this.tsSimpleInput.Controls.Add(this.Panel2);
			this.tsSimpleInput.Location = new System.Drawing.Point(4, 22);
			this.tsSimpleInput.Name = "tsSimpleInput";
			this.tsSimpleInput.Size = new System.Drawing.Size(625, 375);
			this.tsSimpleInput.TabIndex = 0;
			this.tsSimpleInput.Text = "Простой ввод";
			// 
			// Label1
			// 
			this.Label1.Location = new System.Drawing.Point(8, 8);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(150, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Полное имя (формат ФИО)";
			// 
			// Label2
			// 
			this.Label2.Location = new System.Drawing.Point(8, 232);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(50, 13);
			this.Label2.TabIndex = 6;
			this.Label2.Text = "Заметка";
			// 
			// btnMale
			// 
			this.btnMale.Location = new System.Drawing.Point(424, 24);
			this.btnMale.Name = "btnMale";
			this.btnMale.Size = new System.Drawing.Size(49, 21);
			this.btnMale.TabIndex = 2;
			this.btnMale.Text = "М";
			this.btnMale.Click += new System.EventHandler(this.BtnMaleClick);
			// 
			// EditName
			// 
			this.EditName.Location = new System.Drawing.Point(8, 24);
			this.EditName.Name = "EditName";
			this.EditName.Size = new System.Drawing.Size(409, 21);
			this.EditName.TabIndex = 1;
			// 
			// MemoNote
			// 
			this.MemoNote.Location = new System.Drawing.Point(8, 245);
			this.MemoNote.Multiline = true;
			this.MemoNote.Name = "MemoNote";
			this.MemoNote.Size = new System.Drawing.Size(465, 121);
			this.MemoNote.TabIndex = 7;
			// 
			// Panel1
			// 
			this.Panel1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.Panel1.Controls.Add(this.Label3);
			this.Panel1.Controls.Add(this.Label5);
			this.Panel1.Controls.Add(this.EditBirthDate);
			this.Panel1.Controls.Add(this.EditBirthPlace);
			this.Panel1.Controls.Add(this.CheckBirth);
			this.Panel1.Location = new System.Drawing.Point(8, 56);
			this.Panel1.Name = "Panel1";
			this.Panel1.Size = new System.Drawing.Size(465, 81);
			this.Panel1.TabIndex = 4;
			// 
			// Label3
			// 
			this.Label3.Location = new System.Drawing.Point(8, 32);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(90, 13);
			this.Label3.TabIndex = 1;
			this.Label3.Text = "Дата рождения";
			// 
			// Label5
			// 
			this.Label5.Location = new System.Drawing.Point(112, 32);
			this.Label5.Name = "Label5";
			this.Label5.Size = new System.Drawing.Size(100, 13);
			this.Label5.TabIndex = 3;
			this.Label5.Text = "Место рождения";
			// 
			// EditBirthDate
			// 
			this.EditBirthDate.Location = new System.Drawing.Point(8, 48);
			this.EditBirthDate.Mask = "00/00/0000";
			this.EditBirthDate.Name = "EditBirthDate";
			this.EditBirthDate.Size = new System.Drawing.Size(97, 21);
			this.EditBirthDate.TabIndex = 2;
			this.EditBirthDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
			this.EditBirthDate.TextChanged += new System.EventHandler(this.EditBirthDateTextChanged);
			// 
			// EditBirthPlace
			// 
			this.EditBirthPlace.Location = new System.Drawing.Point(112, 48);
			this.EditBirthPlace.Name = "EditBirthPlace";
			this.EditBirthPlace.Size = new System.Drawing.Size(337, 21);
			this.EditBirthPlace.TabIndex = 4;
			this.EditBirthPlace.TextChanged += new System.EventHandler(this.EditBirthDateTextChanged);
			// 
			// CheckBirth
			// 
			this.CheckBirth.Location = new System.Drawing.Point(8, 8);
			this.CheckBirth.Name = "CheckBirth";
			this.CheckBirth.Size = new System.Drawing.Size(96, 17);
			this.CheckBirth.TabIndex = 0;
			this.CheckBirth.Text = "Родился";
			// 
			// Panel2
			// 
			this.Panel2.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.Panel2.Controls.Add(this.Label6);
			this.Panel2.Controls.Add(this.Label7);
			this.Panel2.Controls.Add(this.CheckDeath);
			this.Panel2.Controls.Add(this.EditDeathDate);
			this.Panel2.Controls.Add(this.EditDeathPlace);
			this.Panel2.Location = new System.Drawing.Point(8, 144);
			this.Panel2.Name = "Panel2";
			this.Panel2.Size = new System.Drawing.Size(465, 81);
			this.Panel2.TabIndex = 5;
			// 
			// Label6
			// 
			this.Label6.Location = new System.Drawing.Point(8, 32);
			this.Label6.Name = "Label6";
			this.Label6.Size = new System.Drawing.Size(90, 13);
			this.Label6.TabIndex = 1;
			this.Label6.Text = "Дата смерти";
			// 
			// Label7
			// 
			this.Label7.Location = new System.Drawing.Point(112, 32);
			this.Label7.Name = "Label7";
			this.Label7.Size = new System.Drawing.Size(100, 13);
			this.Label7.TabIndex = 3;
			this.Label7.Text = "Место смерти";
			// 
			// CheckDeath
			// 
			this.CheckDeath.Location = new System.Drawing.Point(8, 8);
			this.CheckDeath.Name = "CheckDeath";
			this.CheckDeath.Size = new System.Drawing.Size(95, 17);
			this.CheckDeath.TabIndex = 0;
			this.CheckDeath.Text = "Умер";
			// 
			// EditDeathDate
			// 
			this.EditDeathDate.Location = new System.Drawing.Point(8, 48);
			this.EditDeathDate.Mask = "00/00/0000";
			this.EditDeathDate.Name = "EditDeathDate";
			this.EditDeathDate.Size = new System.Drawing.Size(97, 21);
			this.EditDeathDate.TabIndex = 2;
			this.EditDeathDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
			this.EditDeathDate.TextChanged += new System.EventHandler(this.EditDeathDateTextChanged);
			// 
			// EditDeathPlace
			// 
			this.EditDeathPlace.Location = new System.Drawing.Point(112, 48);
			this.EditDeathPlace.Name = "EditDeathPlace";
			this.EditDeathPlace.Size = new System.Drawing.Size(337, 21);
			this.EditDeathPlace.TabIndex = 4;
			this.EditDeathPlace.TextChanged += new System.EventHandler(this.EditDeathDateTextChanged);
			// 
			// tsSourceInput
			// 
			this.tsSourceInput.Controls.Add(this.Label4);
			this.tsSourceInput.Controls.Add(this.Label8);
			this.tsSourceInput.Controls.Add(this.Label9);
			this.tsSourceInput.Controls.Add(this.Label10);
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
			this.tsSourceInput.Text = "Источник (метрики/ревизии)";
			// 
			// Label4
			// 
			this.Label4.Location = new System.Drawing.Point(8, 56);
			this.Label4.Name = "Label4";
			this.Label4.Size = new System.Drawing.Size(55, 13);
			this.Label4.TabIndex = 0;
			this.Label4.Text = "Источник";
			// 
			// Label8
			// 
			this.Label8.Location = new System.Drawing.Point(304, 56);
			this.Label8.Name = "Label8";
			this.Label8.Size = new System.Drawing.Size(85, 13);
			this.Label8.TabIndex = 1;
			this.Label8.Text = "Лист/страница";
			// 
			// Label9
			// 
			this.Label9.Location = new System.Drawing.Point(520, 56);
			this.Label9.Name = "Label9";
			this.Label9.Size = new System.Drawing.Size(30, 13);
			this.Label9.TabIndex = 2;
			this.Label9.Text = "Год";
			// 
			// Label10
			// 
			this.Label10.Location = new System.Drawing.Point(8, 88);
			this.Label10.Name = "Label10";
			this.Label10.Size = new System.Drawing.Size(105, 13);
			this.Label10.TabIndex = 3;
			this.Label10.Text = "Населенный пункт";
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
			this.rgSourceKind.Controls.Add(this.radioButton2);
			this.rgSourceKind.Controls.Add(this.radioButton1);
			this.rgSourceKind.Location = new System.Drawing.Point(8, 0);
			this.rgSourceKind.Name = "rgSourceKind";
			this.rgSourceKind.Size = new System.Drawing.Size(609, 38);
			this.rgSourceKind.TabIndex = 6;
			this.rgSourceKind.TabStop = false;
			this.rgSourceKind.Text = "Тип источника";
			// 
			// radioButton2
			// 
			this.radioButton2.Location = new System.Drawing.Point(296, 14);
			this.radioButton2.Name = "radioButton2";
			this.radioButton2.Size = new System.Drawing.Size(273, 18);
			this.radioButton2.TabIndex = 1;
			this.radioButton2.Text = "radioButton2";
			this.radioButton2.UseVisualStyleBackColor = true;
			this.radioButton2.CheckedChanged += new System.EventHandler(this.RadioButton1CheckedChanged);
			// 
			// radioButton1
			// 
			this.radioButton1.Checked = true;
			this.radioButton1.Location = new System.Drawing.Point(8, 14);
			this.radioButton1.Name = "radioButton1";
			this.radioButton1.Size = new System.Drawing.Size(273, 18);
			this.radioButton1.TabIndex = 0;
			this.radioButton1.TabStop = true;
			this.radioButton1.Text = "radioButton1";
			this.radioButton1.UseVisualStyleBackColor = true;
			this.radioButton1.CheckedChanged += new System.EventHandler(this.RadioButton1CheckedChanged);
			// 
			// gbMetrics
			// 
			this.gbMetrics.Controls.Add(this.Label11);
			this.gbMetrics.Controls.Add(this.Label12);
			this.gbMetrics.Controls.Add(this.edEventDate);
			this.gbMetrics.Controls.Add(this.cbEventType);
			this.gbMetrics.Enabled = false;
			this.gbMetrics.Location = new System.Drawing.Point(8, 120);
			this.gbMetrics.Name = "gbMetrics";
			this.gbMetrics.Size = new System.Drawing.Size(609, 50);
			this.gbMetrics.TabIndex = 7;
			this.gbMetrics.TabStop = false;
			this.gbMetrics.Text = "Метрическая книга";
			// 
			// Label11
			// 
			this.Label11.Location = new System.Drawing.Point(8, 24);
			this.Label11.Name = "Label11";
			this.Label11.Size = new System.Drawing.Size(80, 13);
			this.Label11.TabIndex = 0;
			this.Label11.Text = "Дата события";
			// 
			// Label12
			// 
			this.Label12.Location = new System.Drawing.Point(248, 24);
			this.Label12.Name = "Label12";
			this.Label12.Size = new System.Drawing.Size(70, 13);
			this.Label12.TabIndex = 1;
			this.Label12.Text = "Тип события";
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
			// TfmPersonScan
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
			this.Name = "TfmPersonScan";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Добавление персон из источника";
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
		private System.Windows.Forms.DataGridView dataGridView1;
		private System.Windows.Forms.RadioButton radioButton1;
		private System.Windows.Forms.RadioButton radioButton2;
	}
}