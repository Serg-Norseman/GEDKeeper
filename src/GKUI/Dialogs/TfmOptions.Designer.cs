using System;

namespace GKUI
{
	partial class TfmOptions
	{
		private System.Windows.Forms.TabControl PageControl1;
		private System.Windows.Forms.TabPage SheetCommon;
		private System.Windows.Forms.GroupBox rgCode;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.TabPage SheetTree;
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.CheckBox chkFamily;
		private System.Windows.Forms.CheckBox chkName;
		private System.Windows.Forms.CheckBox chkPatronymic;
		private System.Windows.Forms.CheckBox chkDiffLines;
		private System.Windows.Forms.CheckBox chkBirthDate;
		private System.Windows.Forms.CheckBox chkDeathDate;
		private System.Windows.Forms.CheckBox chkKinship;
		private System.Windows.Forms.GroupBox GroupBox2;
		private System.Windows.Forms.Panel PanMaleColor;
		private System.Windows.Forms.Panel PanFemaleColor;
		private System.Windows.Forms.Panel PanUnkSexColor;
		private System.Windows.Forms.Panel PanUnHusbandColor;
		private System.Windows.Forms.Panel PanUnWifeColor;
		private System.Windows.Forms.ColorDialog ColorDialog1;
		private System.Windows.Forms.GroupBox GroupBox4;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.Label Label3;
		private System.Windows.Forms.Label Label4;
		private System.Windows.Forms.CheckBox chkProxy;
		private System.Windows.Forms.TextBox edProxyServer;
		private System.Windows.Forms.TextBox edProxyPort;
		private System.Windows.Forms.TextBox edProxyLogin;
		private System.Windows.Forms.TextBox edProxyPass;
		private System.Windows.Forms.TabPage SheetView;
		private System.Windows.Forms.TabControl PageControl2;
		private System.Windows.Forms.TabPage SheetViewCommon;
		private System.Windows.Forms.TabPage SheetViewPersons;
		private System.Windows.Forms.CheckedListBox ListPersonColumns;
		private System.Windows.Forms.Button btnColumnUp;
		private System.Windows.Forms.Button btnColumnDown;
		private System.Windows.Forms.Button btnDefList;
		private System.Windows.Forms.GroupBox rgFNPFormat;
		private System.Windows.Forms.GroupBox rgDateFormat;
		private System.Windows.Forms.CheckBox chkPlacesWithAddress;
		private System.Windows.Forms.GroupBox GroupBox7;
		private System.Windows.Forms.CheckBox chkShowOnStart;
		private System.Windows.Forms.CheckBox chkHighlightUnparented;
		private System.Windows.Forms.CheckBox chkHighlightUnmarried;
		private System.Windows.Forms.CheckBox chkOnlyYears;
		private System.Windows.Forms.CheckBox chkSignsVisible;
		private System.Windows.Forms.CheckBox chkChildlessExclude;
		private System.Windows.Forms.Label Label5;
		private System.Windows.Forms.Panel PanDefFont;
		private System.Windows.Forms.FontDialog FontDialog1;
		private System.Windows.Forms.TabPage SheetPedigree;
		private System.Windows.Forms.GroupBox GroupBox5;
		private System.Windows.Forms.CheckBox chkAttributes;
		private System.Windows.Forms.CheckBox chkNotes;
		private System.Windows.Forms.CheckBox chkSources;
		private System.Windows.Forms.GroupBox EditPedigreeFormat;
		private System.Windows.Forms.Label Label6;
		private System.Windows.Forms.ComboBox cbLanguages;
		private System.Windows.Forms.CheckBox chkTreeDecorative;
		private System.Windows.Forms.CheckBox chkPortraitsVisible;
		private System.Windows.Forms.RadioButton RButton1;
		private System.Windows.Forms.RadioButton RButton2;
		private System.Windows.Forms.RadioButton RButton5;
		private System.Windows.Forms.RadioButton RButton6;
		private System.Windows.Forms.RadioButton RButton7;
		private System.Windows.Forms.RadioButton RButton8;
		private System.Windows.Forms.RadioButton RButton9;
		private System.Windows.Forms.RadioButton RButton10;
		private System.Windows.Forms.RadioButton RButton11;

		private void InitializeComponent()
		{
			this.PageControl1 = new System.Windows.Forms.TabControl();
			this.SheetCommon = new System.Windows.Forms.TabPage();
			this.Label6 = new System.Windows.Forms.Label();
			this.rgCode = new System.Windows.Forms.GroupBox();
			this.RButton2 = new System.Windows.Forms.RadioButton();
			this.RButton1 = new System.Windows.Forms.RadioButton();
			this.GroupBox4 = new System.Windows.Forms.GroupBox();
			this.Label1 = new System.Windows.Forms.Label();
			this.Label2 = new System.Windows.Forms.Label();
			this.Label3 = new System.Windows.Forms.Label();
			this.Label4 = new System.Windows.Forms.Label();
			this.chkProxy = new System.Windows.Forms.CheckBox();
			this.edProxyServer = new System.Windows.Forms.TextBox();
			this.edProxyPort = new System.Windows.Forms.TextBox();
			this.edProxyLogin = new System.Windows.Forms.TextBox();
			this.edProxyPass = new System.Windows.Forms.TextBox();
			this.GroupBox7 = new System.Windows.Forms.GroupBox();
			this.chkRevisionsBackup = new System.Windows.Forms.CheckBox();
			this.chkShowOnStart = new System.Windows.Forms.CheckBox();
			this.cbLanguages = new System.Windows.Forms.ComboBox();
			this.SheetCharts = new System.Windows.Forms.TabPage();
			this.tabControl1 = new System.Windows.Forms.TabControl();
			this.SheetTree = new System.Windows.Forms.TabPage();
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.chkFamily = new System.Windows.Forms.CheckBox();
			this.chkName = new System.Windows.Forms.CheckBox();
			this.chkPatronymic = new System.Windows.Forms.CheckBox();
			this.chkDiffLines = new System.Windows.Forms.CheckBox();
			this.chkBirthDate = new System.Windows.Forms.CheckBox();
			this.chkDeathDate = new System.Windows.Forms.CheckBox();
			this.chkKinship = new System.Windows.Forms.CheckBox();
			this.chkOnlyYears = new System.Windows.Forms.CheckBox();
			this.chkSignsVisible = new System.Windows.Forms.CheckBox();
			this.chkChildlessExclude = new System.Windows.Forms.CheckBox();
			this.chkTreeDecorative = new System.Windows.Forms.CheckBox();
			this.chkPortraitsVisible = new System.Windows.Forms.CheckBox();
			this.GroupBox2 = new System.Windows.Forms.GroupBox();
			this.Label5 = new System.Windows.Forms.Label();
			this.PanMaleColor = new System.Windows.Forms.Panel();
			this.PanFemaleColor = new System.Windows.Forms.Panel();
			this.PanUnkSexColor = new System.Windows.Forms.Panel();
			this.PanUnHusbandColor = new System.Windows.Forms.Panel();
			this.PanUnWifeColor = new System.Windows.Forms.Panel();
			this.PanDefFont = new System.Windows.Forms.Panel();
			this.SheetAncCircle = new System.Windows.Forms.TabPage();
			this.acbLine = new System.Windows.Forms.Label();
			this.chkShowCircLines = new System.Windows.Forms.CheckBox();
			this.acbBack = new System.Windows.Forms.Label();
			this.acbText = new System.Windows.Forms.Label();
			this.acb7 = new System.Windows.Forms.Label();
			this.acb6 = new System.Windows.Forms.Label();
			this.acb5 = new System.Windows.Forms.Label();
			this.acb4 = new System.Windows.Forms.Label();
			this.acb3 = new System.Windows.Forms.Label();
			this.acb2 = new System.Windows.Forms.Label();
			this.acb1 = new System.Windows.Forms.Label();
			this.acb0 = new System.Windows.Forms.Label();
			this.SheetView = new System.Windows.Forms.TabPage();
			this.PageControl2 = new System.Windows.Forms.TabControl();
			this.SheetViewCommon = new System.Windows.Forms.TabPage();
			this.rgFNPFormat = new System.Windows.Forms.GroupBox();
			this.RButton7 = new System.Windows.Forms.RadioButton();
			this.RButton6 = new System.Windows.Forms.RadioButton();
			this.RButton5 = new System.Windows.Forms.RadioButton();
			this.rgDateFormat = new System.Windows.Forms.GroupBox();
			this.RButton9 = new System.Windows.Forms.RadioButton();
			this.RButton8 = new System.Windows.Forms.RadioButton();
			this.chkPlacesWithAddress = new System.Windows.Forms.CheckBox();
			this.chkHighlightUnparented = new System.Windows.Forms.CheckBox();
			this.chkHighlightUnmarried = new System.Windows.Forms.CheckBox();
			this.SheetViewPersons = new System.Windows.Forms.TabPage();
			this.btnColumnUp = new System.Windows.Forms.Button();
			this.btnColumnDown = new System.Windows.Forms.Button();
			this.ListPersonColumns = new System.Windows.Forms.CheckedListBox();
			this.btnDefList = new System.Windows.Forms.Button();
			this.SheetPedigree = new System.Windows.Forms.TabPage();
			this.GroupBox5 = new System.Windows.Forms.GroupBox();
			this.chkAttributes = new System.Windows.Forms.CheckBox();
			this.chkNotes = new System.Windows.Forms.CheckBox();
			this.chkSources = new System.Windows.Forms.CheckBox();
			this.EditPedigreeFormat = new System.Windows.Forms.GroupBox();
			this.RButton10 = new System.Windows.Forms.RadioButton();
			this.RButton11 = new System.Windows.Forms.RadioButton();
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.ColorDialog1 = new System.Windows.Forms.ColorDialog();
			this.FontDialog1 = new System.Windows.Forms.FontDialog();
			this.PageControl1.SuspendLayout();
			this.SheetCommon.SuspendLayout();
			this.rgCode.SuspendLayout();
			this.GroupBox4.SuspendLayout();
			this.GroupBox7.SuspendLayout();
			this.SheetCharts.SuspendLayout();
			this.tabControl1.SuspendLayout();
			this.SheetTree.SuspendLayout();
			this.GroupBox1.SuspendLayout();
			this.GroupBox2.SuspendLayout();
			this.SheetAncCircle.SuspendLayout();
			this.SheetView.SuspendLayout();
			this.PageControl2.SuspendLayout();
			this.SheetViewCommon.SuspendLayout();
			this.rgFNPFormat.SuspendLayout();
			this.rgDateFormat.SuspendLayout();
			this.SheetViewPersons.SuspendLayout();
			this.SheetPedigree.SuspendLayout();
			this.GroupBox5.SuspendLayout();
			this.EditPedigreeFormat.SuspendLayout();
			this.SuspendLayout();
			// 
			// PageControl1
			// 
			this.PageControl1.Controls.Add(this.SheetCommon);
			this.PageControl1.Controls.Add(this.SheetCharts);
			this.PageControl1.Controls.Add(this.SheetView);
			this.PageControl1.Controls.Add(this.SheetPedigree);
			this.PageControl1.Location = new System.Drawing.Point(0, 0);
			this.PageControl1.Name = "PageControl1";
			this.PageControl1.SelectedIndex = 0;
			this.PageControl1.Size = new System.Drawing.Size(513, 377);
			this.PageControl1.TabIndex = 0;
			// 
			// SheetCommon
			// 
			this.SheetCommon.Controls.Add(this.Label6);
			this.SheetCommon.Controls.Add(this.rgCode);
			this.SheetCommon.Controls.Add(this.GroupBox4);
			this.SheetCommon.Controls.Add(this.GroupBox7);
			this.SheetCommon.Controls.Add(this.cbLanguages);
			this.SheetCommon.Location = new System.Drawing.Point(4, 22);
			this.SheetCommon.Name = "SheetCommon";
			this.SheetCommon.Size = new System.Drawing.Size(505, 351);
			this.SheetCommon.TabIndex = 0;
			this.SheetCommon.Text = "Общие";
			// 
			// Label6
			// 
			this.Label6.Location = new System.Drawing.Point(8, 318);
			this.Label6.Name = "Label6";
			this.Label6.Size = new System.Drawing.Size(35, 13);
			this.Label6.TabIndex = 0;
			this.Label6.Text = "Язык";
			// 
			// rgCode
			// 
			this.rgCode.Controls.Add(this.RButton2);
			this.rgCode.Controls.Add(this.RButton1);
			this.rgCode.Location = new System.Drawing.Point(8, 8);
			this.rgCode.Name = "rgCode";
			this.rgCode.Size = new System.Drawing.Size(231, 49);
			this.rgCode.TabIndex = 0;
			this.rgCode.TabStop = false;
			this.rgCode.Text = "Кодировка сохранения файлов";
			// 
			// RButton2
			// 
			this.RButton2.Location = new System.Drawing.Point(103, 16);
			this.RButton2.Name = "RButton2";
			this.RButton2.Size = new System.Drawing.Size(80, 24);
			this.RButton2.TabIndex = 1;
			this.RButton2.Text = "UTF-8";
			// 
			// RButton1
			// 
			this.RButton1.Location = new System.Drawing.Point(8, 16);
			this.RButton1.Name = "RButton1";
			this.RButton1.Size = new System.Drawing.Size(80, 24);
			this.RButton1.TabIndex = 0;
			this.RButton1.Text = "ASCII";
			// 
			// GroupBox4
			// 
			this.GroupBox4.Controls.Add(this.Label1);
			this.GroupBox4.Controls.Add(this.Label2);
			this.GroupBox4.Controls.Add(this.Label3);
			this.GroupBox4.Controls.Add(this.Label4);
			this.GroupBox4.Controls.Add(this.chkProxy);
			this.GroupBox4.Controls.Add(this.edProxyServer);
			this.GroupBox4.Controls.Add(this.edProxyPort);
			this.GroupBox4.Controls.Add(this.edProxyLogin);
			this.GroupBox4.Controls.Add(this.edProxyPass);
			this.GroupBox4.Location = new System.Drawing.Point(8, 63);
			this.GroupBox4.Name = "GroupBox4";
			this.GroupBox4.Size = new System.Drawing.Size(231, 161);
			this.GroupBox4.TabIndex = 1;
			this.GroupBox4.TabStop = false;
			this.GroupBox4.Text = "Загрузка из Интернета";
			// 
			// Label1
			// 
			this.Label1.Location = new System.Drawing.Point(16, 56);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(50, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Сервер";
			// 
			// Label2
			// 
			this.Label2.Location = new System.Drawing.Point(16, 80);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(50, 13);
			this.Label2.TabIndex = 1;
			this.Label2.Text = "Порт";
			// 
			// Label3
			// 
			this.Label3.Location = new System.Drawing.Point(16, 104);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(50, 13);
			this.Label3.TabIndex = 2;
			this.Label3.Text = "Логин";
			// 
			// Label4
			// 
			this.Label4.Location = new System.Drawing.Point(16, 128);
			this.Label4.Name = "Label4";
			this.Label4.Size = new System.Drawing.Size(50, 13);
			this.Label4.TabIndex = 3;
			this.Label4.Text = "Пароль";
			// 
			// chkProxy
			// 
			this.chkProxy.Location = new System.Drawing.Point(16, 24);
			this.chkProxy.Name = "chkProxy";
			this.chkProxy.Size = new System.Drawing.Size(185, 17);
			this.chkProxy.TabIndex = 0;
			this.chkProxy.Text = "Использовать прокси-сервер";
			// 
			// edProxyServer
			// 
			this.edProxyServer.Location = new System.Drawing.Point(80, 48);
			this.edProxyServer.Name = "edProxyServer";
			this.edProxyServer.Size = new System.Drawing.Size(137, 21);
			this.edProxyServer.TabIndex = 1;
			// 
			// edProxyPort
			// 
			this.edProxyPort.Location = new System.Drawing.Point(80, 72);
			this.edProxyPort.Name = "edProxyPort";
			this.edProxyPort.Size = new System.Drawing.Size(137, 21);
			this.edProxyPort.TabIndex = 2;
			// 
			// edProxyLogin
			// 
			this.edProxyLogin.Location = new System.Drawing.Point(80, 96);
			this.edProxyLogin.Name = "edProxyLogin";
			this.edProxyLogin.Size = new System.Drawing.Size(137, 21);
			this.edProxyLogin.TabIndex = 3;
			// 
			// edProxyPass
			// 
			this.edProxyPass.Location = new System.Drawing.Point(80, 120);
			this.edProxyPass.Name = "edProxyPass";
			this.edProxyPass.PasswordChar = '*';
			this.edProxyPass.Size = new System.Drawing.Size(137, 21);
			this.edProxyPass.TabIndex = 4;
			this.edProxyPass.Text = "edProxyPass";
			// 
			// GroupBox7
			// 
			this.GroupBox7.Controls.Add(this.chkRevisionsBackup);
			this.GroupBox7.Controls.Add(this.chkShowOnStart);
			this.GroupBox7.Location = new System.Drawing.Point(8, 230);
			this.GroupBox7.Name = "GroupBox7";
			this.GroupBox7.Size = new System.Drawing.Size(231, 65);
			this.GroupBox7.TabIndex = 2;
			this.GroupBox7.TabStop = false;
			this.GroupBox7.Text = "Прочее";
			// 
			// chkRevisionsBackup
			// 
			this.chkRevisionsBackup.Location = new System.Drawing.Point(8, 40);
			this.chkRevisionsBackup.Name = "chkRevisionsBackup";
			this.chkRevisionsBackup.Size = new System.Drawing.Size(209, 17);
			this.chkRevisionsBackup.TabIndex = 1;
			this.chkRevisionsBackup.Text = "Сохранять версии файлов";
			// 
			// chkShowOnStart
			// 
			this.chkShowOnStart.Location = new System.Drawing.Point(8, 18);
			this.chkShowOnStart.Name = "chkShowOnStart";
			this.chkShowOnStart.Size = new System.Drawing.Size(209, 17);
			this.chkShowOnStart.TabIndex = 0;
			this.chkShowOnStart.Text = "Показывать при старте подсказки";
			// 
			// cbLanguages
			// 
			this.cbLanguages.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbLanguages.Location = new System.Drawing.Point(75, 310);
			this.cbLanguages.Name = "cbLanguages";
			this.cbLanguages.Size = new System.Drawing.Size(164, 21);
			this.cbLanguages.TabIndex = 4;
			// 
			// SheetCharts
			// 
			this.SheetCharts.Controls.Add(this.tabControl1);
			this.SheetCharts.Location = new System.Drawing.Point(4, 22);
			this.SheetCharts.Name = "SheetCharts";
			this.SheetCharts.Padding = new System.Windows.Forms.Padding(3);
			this.SheetCharts.Size = new System.Drawing.Size(505, 351);
			this.SheetCharts.TabIndex = 4;
			this.SheetCharts.Text = "Диаграммы";
			this.SheetCharts.UseVisualStyleBackColor = true;
			// 
			// tabControl1
			// 
			this.tabControl1.Controls.Add(this.SheetTree);
			this.tabControl1.Controls.Add(this.SheetAncCircle);
			this.tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.tabControl1.Location = new System.Drawing.Point(3, 3);
			this.tabControl1.Name = "tabControl1";
			this.tabControl1.SelectedIndex = 0;
			this.tabControl1.Size = new System.Drawing.Size(499, 345);
			this.tabControl1.TabIndex = 0;
			// 
			// SheetTree
			// 
			this.SheetTree.Controls.Add(this.GroupBox1);
			this.SheetTree.Controls.Add(this.GroupBox2);
			this.SheetTree.Location = new System.Drawing.Point(4, 22);
			this.SheetTree.Name = "SheetTree";
			this.SheetTree.Size = new System.Drawing.Size(491, 319);
			this.SheetTree.TabIndex = 3;
			this.SheetTree.Text = "Родословные древа";
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.chkFamily);
			this.GroupBox1.Controls.Add(this.chkName);
			this.GroupBox1.Controls.Add(this.chkPatronymic);
			this.GroupBox1.Controls.Add(this.chkDiffLines);
			this.GroupBox1.Controls.Add(this.chkBirthDate);
			this.GroupBox1.Controls.Add(this.chkDeathDate);
			this.GroupBox1.Controls.Add(this.chkKinship);
			this.GroupBox1.Controls.Add(this.chkOnlyYears);
			this.GroupBox1.Controls.Add(this.chkSignsVisible);
			this.GroupBox1.Controls.Add(this.chkChildlessExclude);
			this.GroupBox1.Controls.Add(this.chkTreeDecorative);
			this.GroupBox1.Controls.Add(this.chkPortraitsVisible);
			this.GroupBox1.Location = new System.Drawing.Point(8, 8);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(279, 225);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			this.GroupBox1.Text = "Отображение персон в древе";
			// 
			// chkFamily
			// 
			this.chkFamily.Location = new System.Drawing.Point(16, 16);
			this.chkFamily.Name = "chkFamily";
			this.chkFamily.Size = new System.Drawing.Size(249, 17);
			this.chkFamily.TabIndex = 0;
			this.chkFamily.Text = "Фамилия";
			// 
			// chkName
			// 
			this.chkName.Location = new System.Drawing.Point(16, 32);
			this.chkName.Name = "chkName";
			this.chkName.Size = new System.Drawing.Size(249, 17);
			this.chkName.TabIndex = 1;
			this.chkName.Text = "Имя";
			// 
			// chkPatronymic
			// 
			this.chkPatronymic.Location = new System.Drawing.Point(16, 48);
			this.chkPatronymic.Name = "chkPatronymic";
			this.chkPatronymic.Size = new System.Drawing.Size(249, 17);
			this.chkPatronymic.TabIndex = 2;
			this.chkPatronymic.Text = "Отчество";
			// 
			// chkDiffLines
			// 
			this.chkDiffLines.Location = new System.Drawing.Point(16, 64);
			this.chkDiffLines.Name = "chkDiffLines";
			this.chkDiffLines.Size = new System.Drawing.Size(249, 17);
			this.chkDiffLines.TabIndex = 3;
			this.chkDiffLines.Text = "Разные строки (имя и отчество)";
			// 
			// chkBirthDate
			// 
			this.chkBirthDate.Location = new System.Drawing.Point(16, 80);
			this.chkBirthDate.Name = "chkBirthDate";
			this.chkBirthDate.Size = new System.Drawing.Size(249, 17);
			this.chkBirthDate.TabIndex = 4;
			this.chkBirthDate.Text = "Дата рождения";
			// 
			// chkDeathDate
			// 
			this.chkDeathDate.Location = new System.Drawing.Point(16, 96);
			this.chkDeathDate.Name = "chkDeathDate";
			this.chkDeathDate.Size = new System.Drawing.Size(249, 17);
			this.chkDeathDate.TabIndex = 5;
			this.chkDeathDate.Text = "Дата смерти";
			// 
			// chkKinship
			// 
			this.chkKinship.Location = new System.Drawing.Point(16, 128);
			this.chkKinship.Name = "chkKinship";
			this.chkKinship.Size = new System.Drawing.Size(249, 17);
			this.chkKinship.TabIndex = 7;
			this.chkKinship.Text = "Степень родства";
			// 
			// chkOnlyYears
			// 
			this.chkOnlyYears.Location = new System.Drawing.Point(32, 112);
			this.chkOnlyYears.Name = "chkOnlyYears";
			this.chkOnlyYears.Size = new System.Drawing.Size(233, 17);
			this.chkOnlyYears.TabIndex = 6;
			this.chkOnlyYears.Text = "Только годы";
			// 
			// chkSignsVisible
			// 
			this.chkSignsVisible.Location = new System.Drawing.Point(16, 144);
			this.chkSignsVisible.Name = "chkSignsVisible";
			this.chkSignsVisible.Size = new System.Drawing.Size(249, 17);
			this.chkSignsVisible.TabIndex = 8;
			this.chkSignsVisible.Text = "Дополнительные символы";
			// 
			// chkChildlessExclude
			// 
			this.chkChildlessExclude.Location = new System.Drawing.Point(16, 200);
			this.chkChildlessExclude.Name = "chkChildlessExclude";
			this.chkChildlessExclude.Size = new System.Drawing.Size(249, 17);
			this.chkChildlessExclude.TabIndex = 11;
			this.chkChildlessExclude.Text = "Исключить умерших в детстве";
			// 
			// chkTreeDecorative
			// 
			this.chkTreeDecorative.Location = new System.Drawing.Point(16, 160);
			this.chkTreeDecorative.Name = "chkTreeDecorative";
			this.chkTreeDecorative.Size = new System.Drawing.Size(249, 17);
			this.chkTreeDecorative.TabIndex = 9;
			this.chkTreeDecorative.Text = "Декоративное оформление";
			// 
			// chkPortraitsVisible
			// 
			this.chkPortraitsVisible.Location = new System.Drawing.Point(16, 176);
			this.chkPortraitsVisible.Name = "chkPortraitsVisible";
			this.chkPortraitsVisible.Size = new System.Drawing.Size(249, 17);
			this.chkPortraitsVisible.TabIndex = 10;
			this.chkPortraitsVisible.Text = "Отображать портреты";
			// 
			// GroupBox2
			// 
			this.GroupBox2.Controls.Add(this.Label5);
			this.GroupBox2.Controls.Add(this.PanMaleColor);
			this.GroupBox2.Controls.Add(this.PanFemaleColor);
			this.GroupBox2.Controls.Add(this.PanUnkSexColor);
			this.GroupBox2.Controls.Add(this.PanUnHusbandColor);
			this.GroupBox2.Controls.Add(this.PanUnWifeColor);
			this.GroupBox2.Controls.Add(this.PanDefFont);
			this.GroupBox2.Location = new System.Drawing.Point(293, 8);
			this.GroupBox2.Name = "GroupBox2";
			this.GroupBox2.Size = new System.Drawing.Size(185, 193);
			this.GroupBox2.TabIndex = 1;
			this.GroupBox2.TabStop = false;
			this.GroupBox2.Text = "Оформление";
			// 
			// Label5
			// 
			this.Label5.Location = new System.Drawing.Point(16, 144);
			this.Label5.Name = "Label5";
			this.Label5.Size = new System.Drawing.Size(50, 13);
			this.Label5.TabIndex = 0;
			this.Label5.Text = "Шрифт";
			// 
			// PanMaleColor
			// 
			this.PanMaleColor.BackColor = System.Drawing.SystemColors.Control;
			this.PanMaleColor.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.PanMaleColor.Cursor = System.Windows.Forms.Cursors.Hand;
			this.PanMaleColor.Location = new System.Drawing.Point(16, 16);
			this.PanMaleColor.Name = "PanMaleColor";
			this.PanMaleColor.Size = new System.Drawing.Size(73, 25);
			this.PanMaleColor.TabIndex = 0;
			this.PanMaleColor.Text = "Мужчина";
			// 
			// PanFemaleColor
			// 
			this.PanFemaleColor.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.PanFemaleColor.Cursor = System.Windows.Forms.Cursors.Hand;
			this.PanFemaleColor.Location = new System.Drawing.Point(96, 16);
			this.PanFemaleColor.Name = "PanFemaleColor";
			this.PanFemaleColor.Size = new System.Drawing.Size(73, 25);
			this.PanFemaleColor.TabIndex = 1;
			this.PanFemaleColor.Text = "Женщина";
			// 
			// PanUnkSexColor
			// 
			this.PanUnkSexColor.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.PanUnkSexColor.Cursor = System.Windows.Forms.Cursors.Hand;
			this.PanUnkSexColor.Location = new System.Drawing.Point(16, 48);
			this.PanUnkSexColor.Name = "PanUnkSexColor";
			this.PanUnkSexColor.Size = new System.Drawing.Size(153, 25);
			this.PanUnkSexColor.TabIndex = 2;
			this.PanUnkSexColor.Text = "Неизвестный пол";
			// 
			// PanUnHusbandColor
			// 
			this.PanUnHusbandColor.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.PanUnHusbandColor.Cursor = System.Windows.Forms.Cursors.Hand;
			this.PanUnHusbandColor.Location = new System.Drawing.Point(16, 80);
			this.PanUnHusbandColor.Name = "PanUnHusbandColor";
			this.PanUnHusbandColor.Size = new System.Drawing.Size(153, 25);
			this.PanUnHusbandColor.TabIndex = 3;
			this.PanUnHusbandColor.Text = "Разведенный супруг";
			// 
			// PanUnWifeColor
			// 
			this.PanUnWifeColor.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.PanUnWifeColor.Cursor = System.Windows.Forms.Cursors.Hand;
			this.PanUnWifeColor.Location = new System.Drawing.Point(16, 112);
			this.PanUnWifeColor.Name = "PanUnWifeColor";
			this.PanUnWifeColor.Size = new System.Drawing.Size(153, 25);
			this.PanUnWifeColor.TabIndex = 4;
			this.PanUnWifeColor.Text = "Разведенная супруга";
			// 
			// PanDefFont
			// 
			this.PanDefFont.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.PanDefFont.Cursor = System.Windows.Forms.Cursors.Hand;
			this.PanDefFont.Location = new System.Drawing.Point(16, 160);
			this.PanDefFont.Name = "PanDefFont";
			this.PanDefFont.Size = new System.Drawing.Size(153, 25);
			this.PanDefFont.TabIndex = 5;
			// 
			// SheetAncCircle
			// 
			this.SheetAncCircle.Controls.Add(this.acbLine);
			this.SheetAncCircle.Controls.Add(this.chkShowCircLines);
			this.SheetAncCircle.Controls.Add(this.acbBack);
			this.SheetAncCircle.Controls.Add(this.acbText);
			this.SheetAncCircle.Controls.Add(this.acb7);
			this.SheetAncCircle.Controls.Add(this.acb6);
			this.SheetAncCircle.Controls.Add(this.acb5);
			this.SheetAncCircle.Controls.Add(this.acb4);
			this.SheetAncCircle.Controls.Add(this.acb3);
			this.SheetAncCircle.Controls.Add(this.acb2);
			this.SheetAncCircle.Controls.Add(this.acb1);
			this.SheetAncCircle.Controls.Add(this.acb0);
			this.SheetAncCircle.Location = new System.Drawing.Point(4, 22);
			this.SheetAncCircle.Name = "SheetAncCircle";
			this.SheetAncCircle.Padding = new System.Windows.Forms.Padding(3);
			this.SheetAncCircle.Size = new System.Drawing.Size(491, 319);
			this.SheetAncCircle.TabIndex = 4;
			this.SheetAncCircle.Text = "Круг предков";
			this.SheetAncCircle.UseVisualStyleBackColor = true;
			// 
			// acbLine
			// 
			this.acbLine.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acbLine.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acbLine.ForeColor = System.Drawing.Color.White;
			this.acbLine.Location = new System.Drawing.Point(246, 80);
			this.acbLine.Name = "acbLine";
			this.acbLine.Size = new System.Drawing.Size(114, 23);
			this.acbLine.TabIndex = 42;
			this.acbLine.Text = "Line color";
			this.acbLine.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acbLine.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// chkShowCircLines
			// 
			this.chkShowCircLines.AutoSize = true;
			this.chkShowCircLines.Location = new System.Drawing.Point(7, 117);
			this.chkShowCircLines.Name = "chkShowCircLines";
			this.chkShowCircLines.Size = new System.Drawing.Size(113, 17);
			this.chkShowCircLines.TabIndex = 41;
			this.chkShowCircLines.Text = "Show circular lines";
			this.chkShowCircLines.UseVisualStyleBackColor = true;
			// 
			// acbBack
			// 
			this.acbBack.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acbBack.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acbBack.Location = new System.Drawing.Point(126, 80);
			this.acbBack.Name = "acbBack";
			this.acbBack.Size = new System.Drawing.Size(114, 23);
			this.acbBack.TabIndex = 40;
			this.acbBack.Text = "Background color";
			this.acbBack.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acbBack.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acbText
			// 
			this.acbText.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acbText.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acbText.ForeColor = System.Drawing.Color.White;
			this.acbText.Location = new System.Drawing.Point(6, 80);
			this.acbText.Name = "acbText";
			this.acbText.Size = new System.Drawing.Size(114, 23);
			this.acbText.TabIndex = 39;
			this.acbText.Text = "Text color";
			this.acbText.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acbText.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acb7
			// 
			this.acb7.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acb7.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acb7.Location = new System.Drawing.Point(366, 46);
			this.acb7.Name = "acb7";
			this.acb7.Size = new System.Drawing.Size(114, 23);
			this.acb7.TabIndex = 38;
			this.acb7.Text = "Circle 7";
			this.acb7.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acb7.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acb6
			// 
			this.acb6.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acb6.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acb6.Location = new System.Drawing.Point(246, 46);
			this.acb6.Name = "acb6";
			this.acb6.Size = new System.Drawing.Size(114, 23);
			this.acb6.TabIndex = 37;
			this.acb6.Text = "Circle 6";
			this.acb6.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acb6.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acb5
			// 
			this.acb5.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acb5.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acb5.Location = new System.Drawing.Point(126, 46);
			this.acb5.Name = "acb5";
			this.acb5.Size = new System.Drawing.Size(114, 23);
			this.acb5.TabIndex = 36;
			this.acb5.Text = "Circle 5";
			this.acb5.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acb5.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acb4
			// 
			this.acb4.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acb4.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acb4.Location = new System.Drawing.Point(6, 46);
			this.acb4.Name = "acb4";
			this.acb4.Size = new System.Drawing.Size(114, 23);
			this.acb4.TabIndex = 35;
			this.acb4.Text = "Circle 4";
			this.acb4.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acb4.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acb3
			// 
			this.acb3.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acb3.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acb3.Location = new System.Drawing.Point(366, 12);
			this.acb3.Name = "acb3";
			this.acb3.Size = new System.Drawing.Size(114, 23);
			this.acb3.TabIndex = 34;
			this.acb3.Text = "Circle 3";
			this.acb3.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acb3.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acb2
			// 
			this.acb2.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acb2.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acb2.Location = new System.Drawing.Point(246, 12);
			this.acb2.Name = "acb2";
			this.acb2.Size = new System.Drawing.Size(114, 23);
			this.acb2.TabIndex = 33;
			this.acb2.Text = "Circle 2";
			this.acb2.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acb2.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acb1
			// 
			this.acb1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acb1.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acb1.Location = new System.Drawing.Point(126, 12);
			this.acb1.Name = "acb1";
			this.acb1.Size = new System.Drawing.Size(114, 23);
			this.acb1.TabIndex = 32;
			this.acb1.Text = "Circle 1";
			this.acb1.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acb1.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acb0
			// 
			this.acb0.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acb0.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acb0.Location = new System.Drawing.Point(6, 12);
			this.acb0.Name = "acb0";
			this.acb0.Size = new System.Drawing.Size(114, 23);
			this.acb0.TabIndex = 31;
			this.acb0.Text = "Circle 0";
			this.acb0.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acb0.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// SheetView
			// 
			this.SheetView.Controls.Add(this.PageControl2);
			this.SheetView.Location = new System.Drawing.Point(4, 22);
			this.SheetView.Name = "SheetView";
			this.SheetView.Size = new System.Drawing.Size(505, 351);
			this.SheetView.TabIndex = 1;
			this.SheetView.Text = "Интерфейс";
			// 
			// PageControl2
			// 
			this.PageControl2.Controls.Add(this.SheetViewCommon);
			this.PageControl2.Controls.Add(this.SheetViewPersons);
			this.PageControl2.Location = new System.Drawing.Point(0, 0);
			this.PageControl2.Name = "PageControl2";
			this.PageControl2.SelectedIndex = 0;
			this.PageControl2.Size = new System.Drawing.Size(505, 349);
			this.PageControl2.TabIndex = 0;
			// 
			// SheetViewCommon
			// 
			this.SheetViewCommon.Controls.Add(this.rgFNPFormat);
			this.SheetViewCommon.Controls.Add(this.rgDateFormat);
			this.SheetViewCommon.Controls.Add(this.chkPlacesWithAddress);
			this.SheetViewCommon.Controls.Add(this.chkHighlightUnparented);
			this.SheetViewCommon.Controls.Add(this.chkHighlightUnmarried);
			this.SheetViewCommon.Location = new System.Drawing.Point(4, 22);
			this.SheetViewCommon.Name = "SheetViewCommon";
			this.SheetViewCommon.Size = new System.Drawing.Size(497, 323);
			this.SheetViewCommon.TabIndex = 0;
			this.SheetViewCommon.Text = "Все списки";
			// 
			// rgFNPFormat
			// 
			this.rgFNPFormat.Controls.Add(this.RButton7);
			this.rgFNPFormat.Controls.Add(this.RButton6);
			this.rgFNPFormat.Controls.Add(this.RButton5);
			this.rgFNPFormat.Location = new System.Drawing.Point(8, 8);
			this.rgFNPFormat.Name = "rgFNPFormat";
			this.rgFNPFormat.Size = new System.Drawing.Size(185, 97);
			this.rgFNPFormat.TabIndex = 0;
			this.rgFNPFormat.TabStop = false;
			this.rgFNPFormat.Text = "Формат имен в списках";
			// 
			// RButton7
			// 
			this.RButton7.Location = new System.Drawing.Point(8, 64);
			this.RButton7.Name = "RButton7";
			this.RButton7.Size = new System.Drawing.Size(160, 24);
			this.RButton7.TabIndex = 2;
			this.RButton7.Text = "Фамилия; Имя; Отчество";
			// 
			// RButton6
			// 
			this.RButton6.Location = new System.Drawing.Point(8, 40);
			this.RButton6.Name = "RButton6";
			this.RButton6.Size = new System.Drawing.Size(160, 24);
			this.RButton6.TabIndex = 1;
			this.RButton6.Text = "Фамилия; Имя_Отчество";
			// 
			// RButton5
			// 
			this.RButton5.Location = new System.Drawing.Point(8, 17);
			this.RButton5.Name = "RButton5";
			this.RButton5.Size = new System.Drawing.Size(160, 24);
			this.RButton5.TabIndex = 0;
			this.RButton5.Text = "Фамилия_Имя_Отчество";
			// 
			// rgDateFormat
			// 
			this.rgDateFormat.Controls.Add(this.RButton9);
			this.rgDateFormat.Controls.Add(this.RButton8);
			this.rgDateFormat.Location = new System.Drawing.Point(8, 112);
			this.rgDateFormat.Name = "rgDateFormat";
			this.rgDateFormat.Size = new System.Drawing.Size(185, 72);
			this.rgDateFormat.TabIndex = 1;
			this.rgDateFormat.TabStop = false;
			this.rgDateFormat.Text = "Формат даты в списках";
			// 
			// RButton9
			// 
			this.RButton9.Location = new System.Drawing.Point(8, 40);
			this.RButton9.Name = "RButton9";
			this.RButton9.Size = new System.Drawing.Size(104, 24);
			this.RButton9.TabIndex = 1;
			this.RButton9.Text = "YYYY.MM.DD";
			// 
			// RButton8
			// 
			this.RButton8.Location = new System.Drawing.Point(8, 16);
			this.RButton8.Name = "RButton8";
			this.RButton8.Size = new System.Drawing.Size(104, 24);
			this.RButton8.TabIndex = 0;
			this.RButton8.Text = "DD.MM.YYYY";
			// 
			// chkPlacesWithAddress
			// 
			this.chkPlacesWithAddress.Location = new System.Drawing.Point(8, 200);
			this.chkPlacesWithAddress.Name = "chkPlacesWithAddress";
			this.chkPlacesWithAddress.Size = new System.Drawing.Size(185, 17);
			this.chkPlacesWithAddress.TabIndex = 2;
			this.chkPlacesWithAddress.Text = "Включать адрес в строки мест";
			// 
			// chkHighlightUnparented
			// 
			this.chkHighlightUnparented.Location = new System.Drawing.Point(8, 224);
			this.chkHighlightUnparented.Name = "chkHighlightUnparented";
			this.chkHighlightUnparented.Size = new System.Drawing.Size(241, 17);
			this.chkHighlightUnparented.TabIndex = 3;
			this.chkHighlightUnparented.Text = "Подсвечивать персоны без родителей";
			// 
			// chkHighlightUnmarried
			// 
			this.chkHighlightUnmarried.Location = new System.Drawing.Point(8, 248);
			this.chkHighlightUnmarried.Name = "chkHighlightUnmarried";
			this.chkHighlightUnmarried.Size = new System.Drawing.Size(241, 17);
			this.chkHighlightUnmarried.TabIndex = 4;
			this.chkHighlightUnmarried.Text = "Подсвечивать персоны без семьи";
			// 
			// SheetViewPersons
			// 
			this.SheetViewPersons.Controls.Add(this.btnColumnUp);
			this.SheetViewPersons.Controls.Add(this.btnColumnDown);
			this.SheetViewPersons.Controls.Add(this.ListPersonColumns);
			this.SheetViewPersons.Controls.Add(this.btnDefList);
			this.SheetViewPersons.Location = new System.Drawing.Point(4, 22);
			this.SheetViewPersons.Name = "SheetViewPersons";
			this.SheetViewPersons.Size = new System.Drawing.Size(497, 323);
			this.SheetViewPersons.TabIndex = 1;
			this.SheetViewPersons.Text = "Список персон";
			// 
			// btnColumnUp
			// 
			this.btnColumnUp.Image = global::GKResources.iUp;
			this.btnColumnUp.Location = new System.Drawing.Point(352, 8);
			this.btnColumnUp.Name = "btnColumnUp";
			this.btnColumnUp.Size = new System.Drawing.Size(28, 28);
			this.btnColumnUp.TabIndex = 0;
			this.btnColumnUp.Click += new System.EventHandler(this.btnColumnUp_Click);
			// 
			// btnColumnDown
			// 
			this.btnColumnDown.Image = global::GKResources.iDown;
			this.btnColumnDown.Location = new System.Drawing.Point(352, 40);
			this.btnColumnDown.Name = "btnColumnDown";
			this.btnColumnDown.Size = new System.Drawing.Size(28, 28);
			this.btnColumnDown.TabIndex = 1;
			this.btnColumnDown.Click += new System.EventHandler(this.btnColumnDown_Click);
			// 
			// ListPersonColumns
			// 
			this.ListPersonColumns.Location = new System.Drawing.Point(8, 8);
			this.ListPersonColumns.Name = "ListPersonColumns";
			this.ListPersonColumns.Size = new System.Drawing.Size(337, 308);
			this.ListPersonColumns.TabIndex = 0;
			this.ListPersonColumns.ItemCheck += new System.Windows.Forms.ItemCheckEventHandler(this.ListPersonColumns_ItemCheck);
			// 
			// btnDefList
			// 
			this.btnDefList.Location = new System.Drawing.Point(352, 288);
			this.btnDefList.Name = "btnDefList";
			this.btnDefList.Size = new System.Drawing.Size(137, 25);
			this.btnDefList.TabIndex = 1;
			this.btnDefList.Text = "Значения по умолчанию";
			this.btnDefList.Click += new System.EventHandler(this.btnDefList_Click);
			// 
			// SheetPedigree
			// 
			this.SheetPedigree.Controls.Add(this.GroupBox5);
			this.SheetPedigree.Location = new System.Drawing.Point(4, 22);
			this.SheetPedigree.Name = "SheetPedigree";
			this.SheetPedigree.Size = new System.Drawing.Size(505, 351);
			this.SheetPedigree.TabIndex = 3;
			this.SheetPedigree.Text = "Росписи";
			// 
			// GroupBox5
			// 
			this.GroupBox5.Controls.Add(this.chkAttributes);
			this.GroupBox5.Controls.Add(this.chkNotes);
			this.GroupBox5.Controls.Add(this.chkSources);
			this.GroupBox5.Controls.Add(this.EditPedigreeFormat);
			this.GroupBox5.Location = new System.Drawing.Point(8, 8);
			this.GroupBox5.Name = "GroupBox5";
			this.GroupBox5.Size = new System.Drawing.Size(289, 160);
			this.GroupBox5.TabIndex = 0;
			this.GroupBox5.TabStop = false;
			this.GroupBox5.Text = "Генерация росписей";
			// 
			// chkAttributes
			// 
			this.chkAttributes.Location = new System.Drawing.Point(16, 16);
			this.chkAttributes.Name = "chkAttributes";
			this.chkAttributes.Size = new System.Drawing.Size(249, 17);
			this.chkAttributes.TabIndex = 0;
			this.chkAttributes.Text = "Включая атрибуты персон";
			// 
			// chkNotes
			// 
			this.chkNotes.Location = new System.Drawing.Point(16, 32);
			this.chkNotes.Name = "chkNotes";
			this.chkNotes.Size = new System.Drawing.Size(249, 17);
			this.chkNotes.TabIndex = 1;
			this.chkNotes.Text = "Включая заметки";
			// 
			// chkSources
			// 
			this.chkSources.Location = new System.Drawing.Point(16, 48);
			this.chkSources.Name = "chkSources";
			this.chkSources.Size = new System.Drawing.Size(249, 17);
			this.chkSources.TabIndex = 2;
			this.chkSources.Text = "Включая источники";
			// 
			// EditPedigreeFormat
			// 
			this.EditPedigreeFormat.Controls.Add(this.RButton10);
			this.EditPedigreeFormat.Controls.Add(this.RButton11);
			this.EditPedigreeFormat.Location = new System.Drawing.Point(16, 72);
			this.EditPedigreeFormat.Name = "EditPedigreeFormat";
			this.EditPedigreeFormat.Size = new System.Drawing.Size(249, 72);
			this.EditPedigreeFormat.TabIndex = 3;
			this.EditPedigreeFormat.TabStop = false;
			this.EditPedigreeFormat.Text = "Формат";
			// 
			// RButton10
			// 
			this.RButton10.Location = new System.Drawing.Point(16, 16);
			this.RButton10.Name = "RButton10";
			this.RButton10.Size = new System.Drawing.Size(104, 24);
			this.RButton10.TabIndex = 3;
			this.RButton10.Text = "Избыточный";
			// 
			// RButton11
			// 
			this.RButton11.Location = new System.Drawing.Point(16, 40);
			this.RButton11.Name = "RButton11";
			this.RButton11.Size = new System.Drawing.Size(104, 24);
			this.RButton11.TabIndex = 2;
			this.RButton11.Text = "Традиционный";
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(336, 392);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(81, 25);
			this.btnAccept.TabIndex = 1;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(424, 392);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(81, 25);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// TfmOptions
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(513, 425);
			this.Controls.Add(this.PageControl1);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TfmOptions";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Настройки";
			this.PageControl1.ResumeLayout(false);
			this.SheetCommon.ResumeLayout(false);
			this.rgCode.ResumeLayout(false);
			this.GroupBox4.ResumeLayout(false);
			this.GroupBox4.PerformLayout();
			this.GroupBox7.ResumeLayout(false);
			this.SheetCharts.ResumeLayout(false);
			this.tabControl1.ResumeLayout(false);
			this.SheetTree.ResumeLayout(false);
			this.GroupBox1.ResumeLayout(false);
			this.GroupBox2.ResumeLayout(false);
			this.SheetAncCircle.ResumeLayout(false);
			this.SheetAncCircle.PerformLayout();
			this.SheetView.ResumeLayout(false);
			this.PageControl2.ResumeLayout(false);
			this.SheetViewCommon.ResumeLayout(false);
			this.rgFNPFormat.ResumeLayout(false);
			this.rgDateFormat.ResumeLayout(false);
			this.SheetViewPersons.ResumeLayout(false);
			this.SheetPedigree.ResumeLayout(false);
			this.GroupBox5.ResumeLayout(false);
			this.EditPedigreeFormat.ResumeLayout(false);
			this.ResumeLayout(false);
		}
		private System.Windows.Forms.Label acb0;
		private System.Windows.Forms.Label acb1;
		private System.Windows.Forms.Label acb2;
		private System.Windows.Forms.Label acb3;
		private System.Windows.Forms.Label acb4;
		private System.Windows.Forms.Label acb5;
		private System.Windows.Forms.Label acb6;
		private System.Windows.Forms.Label acb7;
		private System.Windows.Forms.Label acbText;
		private System.Windows.Forms.Label acbBack;
		private System.Windows.Forms.CheckBox chkShowCircLines;
		private System.Windows.Forms.Label acbLine;
		private System.Windows.Forms.TabPage SheetAncCircle;
		private System.Windows.Forms.TabControl tabControl1;
		private System.Windows.Forms.TabPage SheetCharts;
		private System.Windows.Forms.CheckBox chkRevisionsBackup;
	}
}