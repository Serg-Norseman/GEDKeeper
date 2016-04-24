using System;

namespace GKUI.Dialogs
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
			this.lblMaleColor = new System.Windows.Forms.Label();
			this.PanFemaleColor = new System.Windows.Forms.Panel();
			this.lblFemaleColor = new System.Windows.Forms.Label();
			this.PanUnkSexColor = new System.Windows.Forms.Panel();
			this.lblUnkSexColor = new System.Windows.Forms.Label();
			this.PanUnHusbandColor = new System.Windows.Forms.Panel();
			this.lblUnHusbandColor = new System.Windows.Forms.Label();
			this.PanUnWifeColor = new System.Windows.Forms.Panel();
			this.lblUnWifeColor = new System.Windows.Forms.Label();
			this.PanDefFont = new System.Windows.Forms.Panel();
			this.lblChartFont = new System.Windows.Forms.Label();
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
			this.chkShowDatesCalendar = new System.Windows.Forms.CheckBox();
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
			this.SheetPlugins = new System.Windows.Forms.TabPage();
			this.lvPlugins = new System.Windows.Forms.ListView();
			this.columnHeader1 = new System.Windows.Forms.ColumnHeader();
			this.columnHeader2 = new System.Windows.Forms.ColumnHeader();
			this.columnHeader3 = new System.Windows.Forms.ColumnHeader();
			this.columnHeader4 = new System.Windows.Forms.ColumnHeader();
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
			this.PanMaleColor.SuspendLayout();
			this.PanFemaleColor.SuspendLayout();
			this.PanUnkSexColor.SuspendLayout();
			this.PanUnHusbandColor.SuspendLayout();
			this.PanUnWifeColor.SuspendLayout();
			this.PanDefFont.SuspendLayout();
			this.SheetView.SuspendLayout();
			this.PageControl2.SuspendLayout();
			this.SheetViewCommon.SuspendLayout();
			this.rgFNPFormat.SuspendLayout();
			this.rgDateFormat.SuspendLayout();
			this.SheetViewPersons.SuspendLayout();
			this.SheetPedigree.SuspendLayout();
			this.GroupBox5.SuspendLayout();
			this.EditPedigreeFormat.SuspendLayout();
			this.SheetPlugins.SuspendLayout();
			this.SuspendLayout();
			// 
			// PageControl1
			// 
			this.PageControl1.Controls.Add(this.SheetCommon);
			this.PageControl1.Controls.Add(this.SheetCharts);
			this.PageControl1.Controls.Add(this.SheetView);
			this.PageControl1.Controls.Add(this.SheetPedigree);
			this.PageControl1.Controls.Add(this.SheetPlugins);
			this.PageControl1.Location = new System.Drawing.Point(0, 0);
			this.PageControl1.Name = "PageControl1";
			this.PageControl1.SelectedIndex = 0;
			this.PageControl1.Size = new System.Drawing.Size(718, 458);
			this.PageControl1.TabIndex = 0;
			// 
			// SheetCommon
			// 
			this.SheetCommon.Controls.Add(this.Label6);
			this.SheetCommon.Controls.Add(this.rgCode);
			this.SheetCommon.Controls.Add(this.GroupBox4);
			this.SheetCommon.Controls.Add(this.GroupBox7);
			this.SheetCommon.Controls.Add(this.cbLanguages);
			this.SheetCommon.Location = new System.Drawing.Point(4, 26);
			this.SheetCommon.Name = "SheetCommon";
			this.SheetCommon.Size = new System.Drawing.Size(710, 428);
			this.SheetCommon.TabIndex = 0;
			this.SheetCommon.Text = "Общие";
			// 
			// Label6
			// 
			this.Label6.AutoSize = true;
			this.Label6.Location = new System.Drawing.Point(11, 386);
			this.Label6.Name = "Label6";
			this.Label6.Size = new System.Drawing.Size(40, 17);
			this.Label6.TabIndex = 0;
			this.Label6.Text = "Язык";
			// 
			// rgCode
			// 
			this.rgCode.Controls.Add(this.RButton2);
			this.rgCode.Controls.Add(this.RButton1);
			this.rgCode.Location = new System.Drawing.Point(11, 10);
			this.rgCode.Name = "rgCode";
			this.rgCode.Size = new System.Drawing.Size(324, 59);
			this.rgCode.TabIndex = 0;
			this.rgCode.TabStop = false;
			this.rgCode.Text = "Кодировка сохранения файлов";
			// 
			// RButton2
			// 
			this.RButton2.AutoSize = true;
			this.RButton2.Location = new System.Drawing.Point(146, 23);
			this.RButton2.Name = "RButton2";
			this.RButton2.Size = new System.Drawing.Size(66, 21);
			this.RButton2.TabIndex = 1;
			this.RButton2.Text = "UTF-8";
			// 
			// RButton1
			// 
			this.RButton1.AutoSize = true;
			this.RButton1.Location = new System.Drawing.Point(11, 23);
			this.RButton1.Name = "RButton1";
			this.RButton1.Size = new System.Drawing.Size(62, 21);
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
			this.GroupBox4.Location = new System.Drawing.Point(11, 77);
			this.GroupBox4.Name = "GroupBox4";
			this.GroupBox4.Size = new System.Drawing.Size(324, 195);
			this.GroupBox4.TabIndex = 1;
			this.GroupBox4.TabStop = false;
			this.GroupBox4.Text = "Загрузка из Интернета";
			// 
			// Label1
			// 
			this.Label1.AutoSize = true;
			this.Label1.Location = new System.Drawing.Point(22, 68);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(54, 17);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Сервер";
			// 
			// Label2
			// 
			this.Label2.AutoSize = true;
			this.Label2.Location = new System.Drawing.Point(22, 97);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(41, 17);
			this.Label2.TabIndex = 1;
			this.Label2.Text = "Порт";
			// 
			// Label3
			// 
			this.Label3.AutoSize = true;
			this.Label3.Location = new System.Drawing.Point(22, 126);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(47, 17);
			this.Label3.TabIndex = 2;
			this.Label3.Text = "Логин";
			// 
			// Label4
			// 
			this.Label4.AutoSize = true;
			this.Label4.Location = new System.Drawing.Point(22, 155);
			this.Label4.Name = "Label4";
			this.Label4.Size = new System.Drawing.Size(55, 17);
			this.Label4.TabIndex = 3;
			this.Label4.Text = "Пароль";
			// 
			// chkProxy
			// 
			this.chkProxy.AutoSize = true;
			this.chkProxy.Location = new System.Drawing.Point(22, 29);
			this.chkProxy.Name = "chkProxy";
			this.chkProxy.Size = new System.Drawing.Size(219, 21);
			this.chkProxy.TabIndex = 0;
			this.chkProxy.Text = "Использовать прокси-сервер";
			// 
			// edProxyServer
			// 
			this.edProxyServer.Location = new System.Drawing.Point(112, 58);
			this.edProxyServer.Name = "edProxyServer";
			this.edProxyServer.Size = new System.Drawing.Size(192, 24);
			this.edProxyServer.TabIndex = 1;
			// 
			// edProxyPort
			// 
			this.edProxyPort.Location = new System.Drawing.Point(112, 87);
			this.edProxyPort.Name = "edProxyPort";
			this.edProxyPort.Size = new System.Drawing.Size(192, 24);
			this.edProxyPort.TabIndex = 2;
			// 
			// edProxyLogin
			// 
			this.edProxyLogin.Location = new System.Drawing.Point(112, 117);
			this.edProxyLogin.Name = "edProxyLogin";
			this.edProxyLogin.Size = new System.Drawing.Size(192, 24);
			this.edProxyLogin.TabIndex = 3;
			// 
			// edProxyPass
			// 
			this.edProxyPass.Location = new System.Drawing.Point(112, 146);
			this.edProxyPass.Name = "edProxyPass";
			this.edProxyPass.PasswordChar = '*';
			this.edProxyPass.Size = new System.Drawing.Size(192, 24);
			this.edProxyPass.TabIndex = 4;
			this.edProxyPass.Text = "edProxyPass";
			// 
			// GroupBox7
			// 
			this.GroupBox7.Controls.Add(this.chkRevisionsBackup);
			this.GroupBox7.Controls.Add(this.chkShowOnStart);
			this.GroupBox7.Location = new System.Drawing.Point(11, 279);
			this.GroupBox7.Name = "GroupBox7";
			this.GroupBox7.Size = new System.Drawing.Size(324, 79);
			this.GroupBox7.TabIndex = 2;
			this.GroupBox7.TabStop = false;
			this.GroupBox7.Text = "Прочее";
			// 
			// chkRevisionsBackup
			// 
			this.chkRevisionsBackup.AutoSize = true;
			this.chkRevisionsBackup.Location = new System.Drawing.Point(11, 49);
			this.chkRevisionsBackup.Name = "chkRevisionsBackup";
			this.chkRevisionsBackup.Size = new System.Drawing.Size(201, 21);
			this.chkRevisionsBackup.TabIndex = 1;
			this.chkRevisionsBackup.Text = "Сохранять версии файлов";
			// 
			// chkShowOnStart
			// 
			this.chkShowOnStart.AutoSize = true;
			this.chkShowOnStart.Location = new System.Drawing.Point(11, 22);
			this.chkShowOnStart.Name = "chkShowOnStart";
			this.chkShowOnStart.Size = new System.Drawing.Size(253, 21);
			this.chkShowOnStart.TabIndex = 0;
			this.chkShowOnStart.Text = "Показывать при старте подсказки";
			// 
			// cbLanguages
			// 
			this.cbLanguages.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbLanguages.Location = new System.Drawing.Point(105, 376);
			this.cbLanguages.Name = "cbLanguages";
			this.cbLanguages.Size = new System.Drawing.Size(230, 25);
			this.cbLanguages.TabIndex = 4;
			// 
			// SheetCharts
			// 
			this.SheetCharts.Controls.Add(this.tabControl1);
			this.SheetCharts.Location = new System.Drawing.Point(4, 26);
			this.SheetCharts.Name = "SheetCharts";
			this.SheetCharts.Padding = new System.Windows.Forms.Padding(3);
			this.SheetCharts.Size = new System.Drawing.Size(710, 428);
			this.SheetCharts.TabIndex = 4;
			this.SheetCharts.Text = "Диаграммы";
			this.SheetCharts.UseVisualStyleBackColor = true;
			// 
			// tabControl1
			// 
			this.tabControl1.Controls.Add(this.SheetTree);
			this.tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.tabControl1.Location = new System.Drawing.Point(3, 3);
			this.tabControl1.Name = "tabControl1";
			this.tabControl1.SelectedIndex = 0;
			this.tabControl1.Size = new System.Drawing.Size(704, 422);
			this.tabControl1.TabIndex = 0;
			// 
			// SheetTree
			// 
			this.SheetTree.Controls.Add(this.GroupBox1);
			this.SheetTree.Controls.Add(this.GroupBox2);
			this.SheetTree.Location = new System.Drawing.Point(4, 26);
			this.SheetTree.Name = "SheetTree";
			this.SheetTree.Size = new System.Drawing.Size(696, 392);
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
			this.GroupBox1.Location = new System.Drawing.Point(11, 10);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(391, 273);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			this.GroupBox1.Text = "Отображение персон в древе";
			// 
			// chkFamily
			// 
			this.chkFamily.Location = new System.Drawing.Point(22, 19);
			this.chkFamily.Name = "chkFamily";
			this.chkFamily.Size = new System.Drawing.Size(349, 21);
			this.chkFamily.TabIndex = 0;
			this.chkFamily.Text = "Фамилия";
			// 
			// chkName
			// 
			this.chkName.Location = new System.Drawing.Point(22, 39);
			this.chkName.Name = "chkName";
			this.chkName.Size = new System.Drawing.Size(349, 21);
			this.chkName.TabIndex = 1;
			this.chkName.Text = "Имя";
			// 
			// chkPatronymic
			// 
			this.chkPatronymic.Location = new System.Drawing.Point(22, 58);
			this.chkPatronymic.Name = "chkPatronymic";
			this.chkPatronymic.Size = new System.Drawing.Size(349, 21);
			this.chkPatronymic.TabIndex = 2;
			this.chkPatronymic.Text = "Отчество";
			// 
			// chkDiffLines
			// 
			this.chkDiffLines.Location = new System.Drawing.Point(22, 78);
			this.chkDiffLines.Name = "chkDiffLines";
			this.chkDiffLines.Size = new System.Drawing.Size(349, 20);
			this.chkDiffLines.TabIndex = 3;
			this.chkDiffLines.Text = "Разные строки (имя и отчество)";
			// 
			// chkBirthDate
			// 
			this.chkBirthDate.Location = new System.Drawing.Point(22, 97);
			this.chkBirthDate.Name = "chkBirthDate";
			this.chkBirthDate.Size = new System.Drawing.Size(349, 21);
			this.chkBirthDate.TabIndex = 4;
			this.chkBirthDate.Text = "Дата рождения";
			// 
			// chkDeathDate
			// 
			this.chkDeathDate.Location = new System.Drawing.Point(22, 117);
			this.chkDeathDate.Name = "chkDeathDate";
			this.chkDeathDate.Size = new System.Drawing.Size(349, 20);
			this.chkDeathDate.TabIndex = 5;
			this.chkDeathDate.Text = "Дата смерти";
			// 
			// chkKinship
			// 
			this.chkKinship.Location = new System.Drawing.Point(22, 155);
			this.chkKinship.Name = "chkKinship";
			this.chkKinship.Size = new System.Drawing.Size(349, 21);
			this.chkKinship.TabIndex = 7;
			this.chkKinship.Text = "Степень родства";
			// 
			// chkOnlyYears
			// 
			this.chkOnlyYears.Location = new System.Drawing.Point(45, 136);
			this.chkOnlyYears.Name = "chkOnlyYears";
			this.chkOnlyYears.Size = new System.Drawing.Size(326, 21);
			this.chkOnlyYears.TabIndex = 6;
			this.chkOnlyYears.Text = "Только годы";
			// 
			// chkSignsVisible
			// 
			this.chkSignsVisible.Location = new System.Drawing.Point(22, 175);
			this.chkSignsVisible.Name = "chkSignsVisible";
			this.chkSignsVisible.Size = new System.Drawing.Size(349, 21);
			this.chkSignsVisible.TabIndex = 8;
			this.chkSignsVisible.Text = "Дополнительные символы";
			// 
			// chkChildlessExclude
			// 
			this.chkChildlessExclude.Location = new System.Drawing.Point(22, 243);
			this.chkChildlessExclude.Name = "chkChildlessExclude";
			this.chkChildlessExclude.Size = new System.Drawing.Size(349, 21);
			this.chkChildlessExclude.TabIndex = 11;
			this.chkChildlessExclude.Text = "Исключить умерших в детстве";
			// 
			// chkTreeDecorative
			// 
			this.chkTreeDecorative.Location = new System.Drawing.Point(22, 194);
			this.chkTreeDecorative.Name = "chkTreeDecorative";
			this.chkTreeDecorative.Size = new System.Drawing.Size(349, 21);
			this.chkTreeDecorative.TabIndex = 9;
			this.chkTreeDecorative.Text = "Декоративное оформление";
			// 
			// chkPortraitsVisible
			// 
			this.chkPortraitsVisible.Location = new System.Drawing.Point(22, 214);
			this.chkPortraitsVisible.Name = "chkPortraitsVisible";
			this.chkPortraitsVisible.Size = new System.Drawing.Size(349, 20);
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
			this.GroupBox2.Location = new System.Drawing.Point(410, 10);
			this.GroupBox2.Name = "GroupBox2";
			this.GroupBox2.Size = new System.Drawing.Size(259, 234);
			this.GroupBox2.TabIndex = 1;
			this.GroupBox2.TabStop = false;
			this.GroupBox2.Text = "Оформление";
			// 
			// Label5
			// 
			this.Label5.Location = new System.Drawing.Point(22, 175);
			this.Label5.Name = "Label5";
			this.Label5.Size = new System.Drawing.Size(70, 16);
			this.Label5.TabIndex = 0;
			this.Label5.Text = "Шрифт";
			// 
			// PanMaleColor
			// 
			this.PanMaleColor.BackColor = System.Drawing.SystemColors.Control;
			this.PanMaleColor.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.PanMaleColor.Controls.Add(this.lblMaleColor);
			this.PanMaleColor.Cursor = System.Windows.Forms.Cursors.Hand;
			this.PanMaleColor.Location = new System.Drawing.Point(22, 19);
			this.PanMaleColor.Name = "PanMaleColor";
			this.PanMaleColor.Size = new System.Drawing.Size(103, 31);
			this.PanMaleColor.TabIndex = 0;
			// 
			// lblMaleColor
			// 
			this.lblMaleColor.Dock = System.Windows.Forms.DockStyle.Fill;
			this.lblMaleColor.Location = new System.Drawing.Point(0, 0);
			this.lblMaleColor.Name = "lblMaleColor";
			this.lblMaleColor.Size = new System.Drawing.Size(99, 27);
			this.lblMaleColor.TabIndex = 1;
			this.lblMaleColor.Text = "label7";
			this.lblMaleColor.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.lblMaleColor.Click += new System.EventHandler(this.PanColor_Click);
			// 
			// PanFemaleColor
			// 
			this.PanFemaleColor.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.PanFemaleColor.Controls.Add(this.lblFemaleColor);
			this.PanFemaleColor.Cursor = System.Windows.Forms.Cursors.Hand;
			this.PanFemaleColor.Location = new System.Drawing.Point(134, 19);
			this.PanFemaleColor.Name = "PanFemaleColor";
			this.PanFemaleColor.Size = new System.Drawing.Size(103, 31);
			this.PanFemaleColor.TabIndex = 1;
			// 
			// lblFemaleColor
			// 
			this.lblFemaleColor.Dock = System.Windows.Forms.DockStyle.Fill;
			this.lblFemaleColor.Location = new System.Drawing.Point(0, 0);
			this.lblFemaleColor.Name = "lblFemaleColor";
			this.lblFemaleColor.Size = new System.Drawing.Size(99, 27);
			this.lblFemaleColor.TabIndex = 1;
			this.lblFemaleColor.Text = "label7";
			this.lblFemaleColor.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.lblFemaleColor.Click += new System.EventHandler(this.PanColor_Click);
			// 
			// PanUnkSexColor
			// 
			this.PanUnkSexColor.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.PanUnkSexColor.Controls.Add(this.lblUnkSexColor);
			this.PanUnkSexColor.Cursor = System.Windows.Forms.Cursors.Hand;
			this.PanUnkSexColor.Location = new System.Drawing.Point(22, 58);
			this.PanUnkSexColor.Name = "PanUnkSexColor";
			this.PanUnkSexColor.Size = new System.Drawing.Size(215, 31);
			this.PanUnkSexColor.TabIndex = 2;
			// 
			// lblUnkSexColor
			// 
			this.lblUnkSexColor.Dock = System.Windows.Forms.DockStyle.Fill;
			this.lblUnkSexColor.Location = new System.Drawing.Point(0, 0);
			this.lblUnkSexColor.Name = "lblUnkSexColor";
			this.lblUnkSexColor.Size = new System.Drawing.Size(211, 27);
			this.lblUnkSexColor.TabIndex = 1;
			this.lblUnkSexColor.Text = "label7";
			this.lblUnkSexColor.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.lblUnkSexColor.Click += new System.EventHandler(this.PanColor_Click);
			// 
			// PanUnHusbandColor
			// 
			this.PanUnHusbandColor.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.PanUnHusbandColor.Controls.Add(this.lblUnHusbandColor);
			this.PanUnHusbandColor.Cursor = System.Windows.Forms.Cursors.Hand;
			this.PanUnHusbandColor.Location = new System.Drawing.Point(22, 97);
			this.PanUnHusbandColor.Name = "PanUnHusbandColor";
			this.PanUnHusbandColor.Size = new System.Drawing.Size(215, 31);
			this.PanUnHusbandColor.TabIndex = 3;
			// 
			// lblUnHusbandColor
			// 
			this.lblUnHusbandColor.Dock = System.Windows.Forms.DockStyle.Fill;
			this.lblUnHusbandColor.Location = new System.Drawing.Point(0, 0);
			this.lblUnHusbandColor.Name = "lblUnHusbandColor";
			this.lblUnHusbandColor.Size = new System.Drawing.Size(211, 27);
			this.lblUnHusbandColor.TabIndex = 1;
			this.lblUnHusbandColor.Text = "label7";
			this.lblUnHusbandColor.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.lblUnHusbandColor.Click += new System.EventHandler(this.PanColor_Click);
			// 
			// PanUnWifeColor
			// 
			this.PanUnWifeColor.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.PanUnWifeColor.Controls.Add(this.lblUnWifeColor);
			this.PanUnWifeColor.Cursor = System.Windows.Forms.Cursors.Hand;
			this.PanUnWifeColor.Location = new System.Drawing.Point(22, 136);
			this.PanUnWifeColor.Name = "PanUnWifeColor";
			this.PanUnWifeColor.Size = new System.Drawing.Size(215, 30);
			this.PanUnWifeColor.TabIndex = 4;
			// 
			// lblUnWifeColor
			// 
			this.lblUnWifeColor.Dock = System.Windows.Forms.DockStyle.Fill;
			this.lblUnWifeColor.Location = new System.Drawing.Point(0, 0);
			this.lblUnWifeColor.Name = "lblUnWifeColor";
			this.lblUnWifeColor.Size = new System.Drawing.Size(211, 26);
			this.lblUnWifeColor.TabIndex = 1;
			this.lblUnWifeColor.Text = "label7";
			this.lblUnWifeColor.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.lblUnWifeColor.Click += new System.EventHandler(this.PanColor_Click);
			// 
			// PanDefFont
			// 
			this.PanDefFont.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.PanDefFont.Controls.Add(this.lblChartFont);
			this.PanDefFont.Cursor = System.Windows.Forms.Cursors.Hand;
			this.PanDefFont.Location = new System.Drawing.Point(22, 194);
			this.PanDefFont.Name = "PanDefFont";
			this.PanDefFont.Size = new System.Drawing.Size(215, 31);
			this.PanDefFont.TabIndex = 5;
			this.PanDefFont.Click += new System.EventHandler(this.PanDefFont_Click);
			// 
			// lblChartFont
			// 
			this.lblChartFont.Dock = System.Windows.Forms.DockStyle.Fill;
			this.lblChartFont.Location = new System.Drawing.Point(0, 0);
			this.lblChartFont.Name = "lblChartFont";
			this.lblChartFont.Size = new System.Drawing.Size(211, 27);
			this.lblChartFont.TabIndex = 0;
			this.lblChartFont.Text = "label7";
			this.lblChartFont.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.lblChartFont.Click += new System.EventHandler(this.PanDefFont_Click);
			// 
			// SheetView
			// 
			this.SheetView.Controls.Add(this.PageControl2);
			this.SheetView.Location = new System.Drawing.Point(4, 26);
			this.SheetView.Name = "SheetView";
			this.SheetView.Size = new System.Drawing.Size(710, 428);
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
			this.PageControl2.Size = new System.Drawing.Size(707, 424);
			this.PageControl2.TabIndex = 0;
			// 
			// SheetViewCommon
			// 
			this.SheetViewCommon.Controls.Add(this.rgFNPFormat);
			this.SheetViewCommon.Controls.Add(this.rgDateFormat);
			this.SheetViewCommon.Controls.Add(this.chkPlacesWithAddress);
			this.SheetViewCommon.Controls.Add(this.chkHighlightUnparented);
			this.SheetViewCommon.Controls.Add(this.chkShowDatesCalendar);
			this.SheetViewCommon.Controls.Add(this.chkHighlightUnmarried);
			this.SheetViewCommon.Location = new System.Drawing.Point(4, 26);
			this.SheetViewCommon.Name = "SheetViewCommon";
			this.SheetViewCommon.Size = new System.Drawing.Size(699, 394);
			this.SheetViewCommon.TabIndex = 0;
			this.SheetViewCommon.Text = "Все списки";
			// 
			// rgFNPFormat
			// 
			this.rgFNPFormat.Controls.Add(this.RButton7);
			this.rgFNPFormat.Controls.Add(this.RButton6);
			this.rgFNPFormat.Controls.Add(this.RButton5);
			this.rgFNPFormat.Location = new System.Drawing.Point(11, 10);
			this.rgFNPFormat.Name = "rgFNPFormat";
			this.rgFNPFormat.Size = new System.Drawing.Size(259, 118);
			this.rgFNPFormat.TabIndex = 0;
			this.rgFNPFormat.TabStop = false;
			this.rgFNPFormat.Text = "Формат имен в списках";
			// 
			// RButton7
			// 
			this.RButton7.Location = new System.Drawing.Point(11, 78);
			this.RButton7.Name = "RButton7";
			this.RButton7.Size = new System.Drawing.Size(224, 29);
			this.RButton7.TabIndex = 2;
			this.RButton7.Text = "Фамилия; Имя; Отчество";
			// 
			// RButton6
			// 
			this.RButton6.Location = new System.Drawing.Point(11, 49);
			this.RButton6.Name = "RButton6";
			this.RButton6.Size = new System.Drawing.Size(224, 29);
			this.RButton6.TabIndex = 1;
			this.RButton6.Text = "Фамилия; Имя_Отчество";
			// 
			// RButton5
			// 
			this.RButton5.Location = new System.Drawing.Point(11, 21);
			this.RButton5.Name = "RButton5";
			this.RButton5.Size = new System.Drawing.Size(224, 29);
			this.RButton5.TabIndex = 0;
			this.RButton5.Text = "Фамилия_Имя_Отчество";
			// 
			// rgDateFormat
			// 
			this.rgDateFormat.Controls.Add(this.RButton9);
			this.rgDateFormat.Controls.Add(this.RButton8);
			this.rgDateFormat.Location = new System.Drawing.Point(11, 136);
			this.rgDateFormat.Name = "rgDateFormat";
			this.rgDateFormat.Size = new System.Drawing.Size(259, 87);
			this.rgDateFormat.TabIndex = 1;
			this.rgDateFormat.TabStop = false;
			this.rgDateFormat.Text = "Формат даты в списках";
			// 
			// RButton9
			// 
			this.RButton9.Location = new System.Drawing.Point(11, 49);
			this.RButton9.Name = "RButton9";
			this.RButton9.Size = new System.Drawing.Size(146, 29);
			this.RButton9.TabIndex = 1;
			this.RButton9.Text = "YYYY.MM.DD";
			// 
			// RButton8
			// 
			this.RButton8.Location = new System.Drawing.Point(11, 19);
			this.RButton8.Name = "RButton8";
			this.RButton8.Size = new System.Drawing.Size(146, 30);
			this.RButton8.TabIndex = 0;
			this.RButton8.Text = "DD.MM.YYYY";
			// 
			// chkPlacesWithAddress
			// 
			this.chkPlacesWithAddress.Location = new System.Drawing.Point(11, 243);
			this.chkPlacesWithAddress.Name = "chkPlacesWithAddress";
			this.chkPlacesWithAddress.Size = new System.Drawing.Size(259, 21);
			this.chkPlacesWithAddress.TabIndex = 2;
			this.chkPlacesWithAddress.Text = "Включать адрес в строки мест";
			// 
			// chkHighlightUnparented
			// 
			this.chkHighlightUnparented.Location = new System.Drawing.Point(11, 272);
			this.chkHighlightUnparented.Name = "chkHighlightUnparented";
			this.chkHighlightUnparented.Size = new System.Drawing.Size(338, 21);
			this.chkHighlightUnparented.TabIndex = 3;
			this.chkHighlightUnparented.Text = "Подсвечивать персоны без родителей";
			// 
			// chkShowDatesCalendar
			// 
			this.chkShowDatesCalendar.Location = new System.Drawing.Point(11, 328);
			this.chkShowDatesCalendar.Name = "chkShowDatesCalendar";
			this.chkShowDatesCalendar.Size = new System.Drawing.Size(338, 21);
			this.chkShowDatesCalendar.TabIndex = 4;
			this.chkShowDatesCalendar.Text = "chkShowDatesCalendar";
			// 
			// chkHighlightUnmarried
			// 
			this.chkHighlightUnmarried.Location = new System.Drawing.Point(11, 301);
			this.chkHighlightUnmarried.Name = "chkHighlightUnmarried";
			this.chkHighlightUnmarried.Size = new System.Drawing.Size(338, 21);
			this.chkHighlightUnmarried.TabIndex = 4;
			this.chkHighlightUnmarried.Text = "Подсвечивать персоны без семьи";
			// 
			// SheetViewPersons
			// 
			this.SheetViewPersons.Controls.Add(this.btnColumnUp);
			this.SheetViewPersons.Controls.Add(this.btnColumnDown);
			this.SheetViewPersons.Controls.Add(this.ListPersonColumns);
			this.SheetViewPersons.Controls.Add(this.btnDefList);
			this.SheetViewPersons.Location = new System.Drawing.Point(4, 26);
			this.SheetViewPersons.Name = "SheetViewPersons";
			this.SheetViewPersons.Size = new System.Drawing.Size(699, 394);
			this.SheetViewPersons.TabIndex = 1;
			this.SheetViewPersons.Text = "Список персон";
			// 
			// btnColumnUp
			// 
			this.btnColumnUp.Image = global::GKResources.iUp;
			this.btnColumnUp.Location = new System.Drawing.Point(493, 10);
			this.btnColumnUp.Name = "btnColumnUp";
			this.btnColumnUp.Size = new System.Drawing.Size(39, 34);
			this.btnColumnUp.TabIndex = 0;
			this.btnColumnUp.Click += new System.EventHandler(this.btnColumnUp_Click);
			// 
			// btnColumnDown
			// 
			this.btnColumnDown.Image = global::GKResources.iDown;
			this.btnColumnDown.Location = new System.Drawing.Point(493, 49);
			this.btnColumnDown.Name = "btnColumnDown";
			this.btnColumnDown.Size = new System.Drawing.Size(39, 34);
			this.btnColumnDown.TabIndex = 1;
			this.btnColumnDown.Click += new System.EventHandler(this.btnColumnDown_Click);
			// 
			// ListPersonColumns
			// 
			this.ListPersonColumns.Location = new System.Drawing.Point(11, 10);
			this.ListPersonColumns.Name = "ListPersonColumns";
			this.ListPersonColumns.Size = new System.Drawing.Size(472, 365);
			this.ListPersonColumns.TabIndex = 0;
			this.ListPersonColumns.ItemCheck += new System.Windows.Forms.ItemCheckEventHandler(this.ListPersonColumns_ItemCheck);
			// 
			// btnDefList
			// 
			this.btnDefList.Location = new System.Drawing.Point(493, 340);
			this.btnDefList.Name = "btnDefList";
			this.btnDefList.Size = new System.Drawing.Size(192, 44);
			this.btnDefList.TabIndex = 1;
			this.btnDefList.Text = "Значения по умолчанию";
			this.btnDefList.Click += new System.EventHandler(this.btnDefList_Click);
			// 
			// SheetPedigree
			// 
			this.SheetPedigree.Controls.Add(this.GroupBox5);
			this.SheetPedigree.Location = new System.Drawing.Point(4, 26);
			this.SheetPedigree.Name = "SheetPedigree";
			this.SheetPedigree.Size = new System.Drawing.Size(710, 428);
			this.SheetPedigree.TabIndex = 3;
			this.SheetPedigree.Text = "Росписи";
			// 
			// GroupBox5
			// 
			this.GroupBox5.Controls.Add(this.chkAttributes);
			this.GroupBox5.Controls.Add(this.chkNotes);
			this.GroupBox5.Controls.Add(this.chkSources);
			this.GroupBox5.Controls.Add(this.EditPedigreeFormat);
			this.GroupBox5.Location = new System.Drawing.Point(11, 10);
			this.GroupBox5.Name = "GroupBox5";
			this.GroupBox5.Size = new System.Drawing.Size(405, 194);
			this.GroupBox5.TabIndex = 0;
			this.GroupBox5.TabStop = false;
			this.GroupBox5.Text = "Генерация росписей";
			// 
			// chkAttributes
			// 
			this.chkAttributes.Location = new System.Drawing.Point(22, 19);
			this.chkAttributes.Name = "chkAttributes";
			this.chkAttributes.Size = new System.Drawing.Size(349, 21);
			this.chkAttributes.TabIndex = 0;
			this.chkAttributes.Text = "Включая атрибуты персон";
			// 
			// chkNotes
			// 
			this.chkNotes.Location = new System.Drawing.Point(22, 39);
			this.chkNotes.Name = "chkNotes";
			this.chkNotes.Size = new System.Drawing.Size(349, 21);
			this.chkNotes.TabIndex = 1;
			this.chkNotes.Text = "Включая заметки";
			// 
			// chkSources
			// 
			this.chkSources.Location = new System.Drawing.Point(22, 58);
			this.chkSources.Name = "chkSources";
			this.chkSources.Size = new System.Drawing.Size(349, 21);
			this.chkSources.TabIndex = 2;
			this.chkSources.Text = "Включая источники";
			// 
			// EditPedigreeFormat
			// 
			this.EditPedigreeFormat.Controls.Add(this.RButton10);
			this.EditPedigreeFormat.Controls.Add(this.RButton11);
			this.EditPedigreeFormat.Location = new System.Drawing.Point(22, 87);
			this.EditPedigreeFormat.Name = "EditPedigreeFormat";
			this.EditPedigreeFormat.Size = new System.Drawing.Size(349, 88);
			this.EditPedigreeFormat.TabIndex = 3;
			this.EditPedigreeFormat.TabStop = false;
			this.EditPedigreeFormat.Text = "Формат";
			// 
			// RButton10
			// 
			this.RButton10.Location = new System.Drawing.Point(22, 19);
			this.RButton10.Name = "RButton10";
			this.RButton10.Size = new System.Drawing.Size(146, 30);
			this.RButton10.TabIndex = 3;
			this.RButton10.Text = "Избыточный";
			// 
			// RButton11
			// 
			this.RButton11.Location = new System.Drawing.Point(22, 49);
			this.RButton11.Name = "RButton11";
			this.RButton11.Size = new System.Drawing.Size(146, 29);
			this.RButton11.TabIndex = 2;
			this.RButton11.Text = "Традиционный";
			// 
			// SheetPlugins
			// 
			this.SheetPlugins.Controls.Add(this.lvPlugins);
			this.SheetPlugins.Location = new System.Drawing.Point(4, 26);
			this.SheetPlugins.Name = "SheetPlugins";
			this.SheetPlugins.Padding = new System.Windows.Forms.Padding(3);
			this.SheetPlugins.Size = new System.Drawing.Size(710, 428);
			this.SheetPlugins.TabIndex = 5;
			this.SheetPlugins.Text = "SheetPlugins";
			this.SheetPlugins.UseVisualStyleBackColor = true;
			// 
			// lvPlugins
			// 
			this.lvPlugins.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
									this.columnHeader1,
									this.columnHeader2,
									this.columnHeader3,
									this.columnHeader4});
			this.lvPlugins.Dock = System.Windows.Forms.DockStyle.Fill;
			this.lvPlugins.FullRowSelect = true;
			this.lvPlugins.Location = new System.Drawing.Point(3, 3);
			this.lvPlugins.MultiSelect = false;
			this.lvPlugins.Name = "lvPlugins";
			this.lvPlugins.Size = new System.Drawing.Size(704, 422);
			this.lvPlugins.TabIndex = 0;
			this.lvPlugins.UseCompatibleStateImageBehavior = false;
			this.lvPlugins.View = System.Windows.Forms.View.Details;
			// 
			// columnHeader1
			// 
			this.columnHeader1.Text = "Title";
			this.columnHeader1.Width = 75;
			// 
			// columnHeader2
			// 
			this.columnHeader2.Text = "Version";
			// 
			// columnHeader3
			// 
			this.columnHeader3.Text = "Copyright";
			this.columnHeader3.Width = 125;
			// 
			// columnHeader4
			// 
			this.columnHeader4.Text = "Description";
			this.columnHeader4.Width = 250;
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(470, 476);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(114, 30);
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
			this.btnCancel.Location = new System.Drawing.Point(594, 476);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(114, 30);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// TfmOptions
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(716, 520);
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
			this.SheetCommon.PerformLayout();
			this.rgCode.ResumeLayout(false);
			this.rgCode.PerformLayout();
			this.GroupBox4.ResumeLayout(false);
			this.GroupBox4.PerformLayout();
			this.GroupBox7.ResumeLayout(false);
			this.GroupBox7.PerformLayout();
			this.SheetCharts.ResumeLayout(false);
			this.tabControl1.ResumeLayout(false);
			this.SheetTree.ResumeLayout(false);
			this.GroupBox1.ResumeLayout(false);
			this.GroupBox2.ResumeLayout(false);
			this.PanMaleColor.ResumeLayout(false);
			this.PanFemaleColor.ResumeLayout(false);
			this.PanUnkSexColor.ResumeLayout(false);
			this.PanUnHusbandColor.ResumeLayout(false);
			this.PanUnWifeColor.ResumeLayout(false);
			this.PanDefFont.ResumeLayout(false);
			this.SheetView.ResumeLayout(false);
			this.PageControl2.ResumeLayout(false);
			this.SheetViewCommon.ResumeLayout(false);
			this.rgFNPFormat.ResumeLayout(false);
			this.rgDateFormat.ResumeLayout(false);
			this.SheetViewPersons.ResumeLayout(false);
			this.SheetPedigree.ResumeLayout(false);
			this.GroupBox5.ResumeLayout(false);
			this.EditPedigreeFormat.ResumeLayout(false);
			this.SheetPlugins.ResumeLayout(false);
			this.ResumeLayout(false);
		}
		private System.Windows.Forms.CheckBox chkShowDatesCalendar;
		private System.Windows.Forms.ColumnHeader columnHeader4;
		private System.Windows.Forms.ColumnHeader columnHeader3;
		private System.Windows.Forms.ColumnHeader columnHeader2;
		private System.Windows.Forms.ColumnHeader columnHeader1;
		private System.Windows.Forms.ListView lvPlugins;
		private System.Windows.Forms.TabPage SheetPlugins;
		private System.Windows.Forms.Label lblChartFont;
		private System.Windows.Forms.TabControl tabControl1;
		private System.Windows.Forms.TabPage SheetCharts;
		private System.Windows.Forms.CheckBox chkRevisionsBackup;
        private System.Windows.Forms.Label lblMaleColor;
        private System.Windows.Forms.Label lblFemaleColor;
        private System.Windows.Forms.Label lblUnkSexColor;
        private System.Windows.Forms.Label lblUnHusbandColor;
        private System.Windows.Forms.Label lblUnWifeColor;
	}
}