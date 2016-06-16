using System;

namespace GKUI.Dialogs
{
    partial class OptionsDlg
    {
        private System.Windows.Forms.TabControl PageControl1;
        private System.Windows.Forms.TabPage pageCommon;
        private System.Windows.Forms.GroupBox grpEncoding;
        private System.Windows.Forms.Button btnAccept;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.TabPage pageTreeChart;
        private System.Windows.Forms.GroupBox grpTreePersons;
        private System.Windows.Forms.CheckBox chkSurname;
        private System.Windows.Forms.CheckBox chkName;
        private System.Windows.Forms.CheckBox chkPatronymic;
        private System.Windows.Forms.CheckBox chkDiffLines;
        private System.Windows.Forms.CheckBox chkBirthDate;
        private System.Windows.Forms.CheckBox chkDeathDate;
        private System.Windows.Forms.CheckBox chkKinship;
        private System.Windows.Forms.GroupBox grpTreeDecor;
        private System.Windows.Forms.Panel panMaleColor;
        private System.Windows.Forms.Panel panFemaleColor;
        private System.Windows.Forms.Panel panUnkSexColor;
        private System.Windows.Forms.Panel panUnHusbandColor;
        private System.Windows.Forms.Panel panUnWifeColor;
        private System.Windows.Forms.ColorDialog ColorDialog1;
        private System.Windows.Forms.GroupBox grpInternet;
        private System.Windows.Forms.Label lblProxyServer;
        private System.Windows.Forms.Label lblProxyPort;
        private System.Windows.Forms.Label lblProxyLogin;
        private System.Windows.Forms.Label lblProxyPassword;
        private System.Windows.Forms.CheckBox chkUseProxy;
        private System.Windows.Forms.TextBox txtProxyServer;
        private System.Windows.Forms.TextBox txtProxyPort;
        private System.Windows.Forms.TextBox txtProxyLogin;
        private System.Windows.Forms.TextBox txtProxyPass;
        private System.Windows.Forms.TabPage pageUIView;
        private System.Windows.Forms.TabControl PageControl2;
        private System.Windows.Forms.TabPage pageViewCommon;
        private System.Windows.Forms.TabPage pageViewPersons;
        private System.Windows.Forms.CheckedListBox lstPersonColumns;
        private System.Windows.Forms.Button btnColumnUp;
        private System.Windows.Forms.Button btnColumnDown;
        private System.Windows.Forms.Button btnDefList;
        private System.Windows.Forms.GroupBox rgFNPFormat;
        private System.Windows.Forms.GroupBox grpDateFormat;
        private System.Windows.Forms.CheckBox chkPlacesWithAddress;
        private System.Windows.Forms.GroupBox grpOther;
        private System.Windows.Forms.CheckBox chkShowOnStart;
        private System.Windows.Forms.CheckBox chkHighlightUnparented;
        private System.Windows.Forms.CheckBox chkHighlightUnmarried;
        private System.Windows.Forms.CheckBox chkOnlyYears;
        private System.Windows.Forms.CheckBox chkSignsVisible;
        private System.Windows.Forms.CheckBox chkChildlessExclude;
        private System.Windows.Forms.Label lblFont;
        private System.Windows.Forms.Panel panDefFont;
        private System.Windows.Forms.FontDialog FontDialog1;
        private System.Windows.Forms.TabPage pagePedigree;
        private System.Windows.Forms.GroupBox grpPedigree;
        private System.Windows.Forms.CheckBox chkAttributes;
        private System.Windows.Forms.CheckBox chkNotes;
        private System.Windows.Forms.CheckBox chkSources;
        private System.Windows.Forms.GroupBox grpPedigreeFormat;
        private System.Windows.Forms.Label lblLanguage;
        private System.Windows.Forms.ComboBox cmbLanguages;
        private System.Windows.Forms.CheckBox chkTreeDecorative;
        private System.Windows.Forms.CheckBox chkPortraitsVisible;
        private System.Windows.Forms.RadioButton radASCII;
        private System.Windows.Forms.RadioButton radUTF;
        private System.Windows.Forms.RadioButton radSNP;
        private System.Windows.Forms.RadioButton radS_NP;
        private System.Windows.Forms.RadioButton radS_N_P;
        private System.Windows.Forms.RadioButton radDMY;
        private System.Windows.Forms.RadioButton radYMD;
        private System.Windows.Forms.RadioButton radExcess;
        private System.Windows.Forms.RadioButton radCompact;
        private System.Windows.Forms.CheckBox chkShowDatesSigns;
        private System.Windows.Forms.CheckBox chkShowDatesCalendar;
        private System.Windows.Forms.ColumnHeader columnHeader4;
        private System.Windows.Forms.ColumnHeader columnHeader3;
        private System.Windows.Forms.ColumnHeader columnHeader2;
        private System.Windows.Forms.ColumnHeader columnHeader1;
        private System.Windows.Forms.ListView lvPlugins;
        private System.Windows.Forms.TabPage pagePlugins;
        private System.Windows.Forms.Label lblChartFont;
        private System.Windows.Forms.TabControl tabsCharts;
        private System.Windows.Forms.TabPage pageCharts;
        private System.Windows.Forms.CheckBox chkRevisionsBackup;
        private System.Windows.Forms.Label lblMaleColor;
        private System.Windows.Forms.Label lblFemaleColor;
        private System.Windows.Forms.Label lblUnkSexColor;
        private System.Windows.Forms.Label lblUnHusbandColor;
        private System.Windows.Forms.Label lblUnWifeColor;
        private System.Windows.Forms.Panel panel1;

        private void InitializeComponent()
        {
            this.PageControl1 = new System.Windows.Forms.TabControl();
            this.pageCommon = new System.Windows.Forms.TabPage();
            this.lblLanguage = new System.Windows.Forms.Label();
            this.grpEncoding = new System.Windows.Forms.GroupBox();
            this.radUTF = new System.Windows.Forms.RadioButton();
            this.radASCII = new System.Windows.Forms.RadioButton();
            this.grpInternet = new System.Windows.Forms.GroupBox();
            this.lblProxyServer = new System.Windows.Forms.Label();
            this.lblProxyPort = new System.Windows.Forms.Label();
            this.lblProxyLogin = new System.Windows.Forms.Label();
            this.lblProxyPassword = new System.Windows.Forms.Label();
            this.chkUseProxy = new System.Windows.Forms.CheckBox();
            this.txtProxyServer = new System.Windows.Forms.TextBox();
            this.txtProxyPort = new System.Windows.Forms.TextBox();
            this.txtProxyLogin = new System.Windows.Forms.TextBox();
            this.txtProxyPass = new System.Windows.Forms.TextBox();
            this.grpOther = new System.Windows.Forms.GroupBox();
            this.chkRevisionsBackup = new System.Windows.Forms.CheckBox();
            this.chkShowOnStart = new System.Windows.Forms.CheckBox();
            this.cmbLanguages = new System.Windows.Forms.ComboBox();
            this.pageCharts = new System.Windows.Forms.TabPage();
            this.tabsCharts = new System.Windows.Forms.TabControl();
            this.pageTreeChart = new System.Windows.Forms.TabPage();
            this.grpTreePersons = new System.Windows.Forms.GroupBox();
            this.chkSurname = new System.Windows.Forms.CheckBox();
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
            this.grpTreeDecor = new System.Windows.Forms.GroupBox();
            this.lblFont = new System.Windows.Forms.Label();
            this.panMaleColor = new System.Windows.Forms.Panel();
            this.lblMaleColor = new System.Windows.Forms.Label();
            this.panFemaleColor = new System.Windows.Forms.Panel();
            this.lblFemaleColor = new System.Windows.Forms.Label();
            this.panUnkSexColor = new System.Windows.Forms.Panel();
            this.lblUnkSexColor = new System.Windows.Forms.Label();
            this.panUnHusbandColor = new System.Windows.Forms.Panel();
            this.lblUnHusbandColor = new System.Windows.Forms.Label();
            this.panUnWifeColor = new System.Windows.Forms.Panel();
            this.lblUnWifeColor = new System.Windows.Forms.Label();
            this.panDefFont = new System.Windows.Forms.Panel();
            this.lblChartFont = new System.Windows.Forms.Label();
            this.pageUIView = new System.Windows.Forms.TabPage();
            this.PageControl2 = new System.Windows.Forms.TabControl();
            this.pageViewCommon = new System.Windows.Forms.TabPage();
            this.rgFNPFormat = new System.Windows.Forms.GroupBox();
            this.radS_N_P = new System.Windows.Forms.RadioButton();
            this.radS_NP = new System.Windows.Forms.RadioButton();
            this.radSNP = new System.Windows.Forms.RadioButton();
            this.grpDateFormat = new System.Windows.Forms.GroupBox();
            this.radYMD = new System.Windows.Forms.RadioButton();
            this.radDMY = new System.Windows.Forms.RadioButton();
            this.chkPlacesWithAddress = new System.Windows.Forms.CheckBox();
            this.chkHighlightUnparented = new System.Windows.Forms.CheckBox();
            this.chkShowDatesSigns = new System.Windows.Forms.CheckBox();
            this.chkShowDatesCalendar = new System.Windows.Forms.CheckBox();
            this.chkHighlightUnmarried = new System.Windows.Forms.CheckBox();
            this.pageViewPersons = new System.Windows.Forms.TabPage();
            this.panel1 = new System.Windows.Forms.Panel();
            this.lstPersonColumns = new System.Windows.Forms.CheckedListBox();
            this.btnColumnUp = new System.Windows.Forms.Button();
            this.btnColumnDown = new System.Windows.Forms.Button();
            this.btnDefList = new System.Windows.Forms.Button();
            this.pagePedigree = new System.Windows.Forms.TabPage();
            this.grpPedigree = new System.Windows.Forms.GroupBox();
            this.chkAttributes = new System.Windows.Forms.CheckBox();
            this.chkNotes = new System.Windows.Forms.CheckBox();
            this.chkSources = new System.Windows.Forms.CheckBox();
            this.grpPedigreeFormat = new System.Windows.Forms.GroupBox();
            this.radExcess = new System.Windows.Forms.RadioButton();
            this.radCompact = new System.Windows.Forms.RadioButton();
            this.pagePlugins = new System.Windows.Forms.TabPage();
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
            this.pageCommon.SuspendLayout();
            this.grpEncoding.SuspendLayout();
            this.grpInternet.SuspendLayout();
            this.grpOther.SuspendLayout();
            this.pageCharts.SuspendLayout();
            this.tabsCharts.SuspendLayout();
            this.pageTreeChart.SuspendLayout();
            this.grpTreePersons.SuspendLayout();
            this.grpTreeDecor.SuspendLayout();
            this.panMaleColor.SuspendLayout();
            this.panFemaleColor.SuspendLayout();
            this.panUnkSexColor.SuspendLayout();
            this.panUnHusbandColor.SuspendLayout();
            this.panUnWifeColor.SuspendLayout();
            this.panDefFont.SuspendLayout();
            this.pageUIView.SuspendLayout();
            this.PageControl2.SuspendLayout();
            this.pageViewCommon.SuspendLayout();
            this.rgFNPFormat.SuspendLayout();
            this.grpDateFormat.SuspendLayout();
            this.pageViewPersons.SuspendLayout();
            this.panel1.SuspendLayout();
            this.pagePedigree.SuspendLayout();
            this.grpPedigree.SuspendLayout();
            this.grpPedigreeFormat.SuspendLayout();
            this.pagePlugins.SuspendLayout();
            this.SuspendLayout();
            // 
            // PageControl1
            // 
            this.PageControl1.Controls.Add(this.pageCommon);
            this.PageControl1.Controls.Add(this.pageCharts);
            this.PageControl1.Controls.Add(this.pageUIView);
            this.PageControl1.Controls.Add(this.pagePedigree);
            this.PageControl1.Controls.Add(this.pagePlugins);
            this.PageControl1.Location = new System.Drawing.Point(0, 0);
            this.PageControl1.Name = "PageControl1";
            this.PageControl1.SelectedIndex = 0;
            this.PageControl1.Size = new System.Drawing.Size(718, 458);
            this.PageControl1.TabIndex = 0;
            // 
            // pageCommon
            // 
            this.pageCommon.Controls.Add(this.lblLanguage);
            this.pageCommon.Controls.Add(this.grpEncoding);
            this.pageCommon.Controls.Add(this.grpInternet);
            this.pageCommon.Controls.Add(this.grpOther);
            this.pageCommon.Controls.Add(this.cmbLanguages);
            this.pageCommon.Location = new System.Drawing.Point(4, 26);
            this.pageCommon.Name = "pageCommon";
            this.pageCommon.Size = new System.Drawing.Size(710, 428);
            this.pageCommon.TabIndex = 0;
            this.pageCommon.Text = "pageCommon";
            // 
            // lblLanguage
            // 
            this.lblLanguage.AutoSize = true;
            this.lblLanguage.Location = new System.Drawing.Point(11, 386);
            this.lblLanguage.Name = "lblLanguage";
            this.lblLanguage.Size = new System.Drawing.Size(80, 17);
            this.lblLanguage.TabIndex = 0;
            this.lblLanguage.Text = "lblLanguage";
            // 
            // grpEncoding
            // 
            this.grpEncoding.Controls.Add(this.radUTF);
            this.grpEncoding.Controls.Add(this.radASCII);
            this.grpEncoding.Location = new System.Drawing.Point(11, 10);
            this.grpEncoding.Name = "grpEncoding";
            this.grpEncoding.Padding = new System.Windows.Forms.Padding(10);
            this.grpEncoding.Size = new System.Drawing.Size(324, 59);
            this.grpEncoding.TabIndex = 0;
            this.grpEncoding.TabStop = false;
            this.grpEncoding.Text = "grpEncoding";
            // 
            // radUTF
            // 
            this.radUTF.AutoSize = true;
            this.radUTF.Location = new System.Drawing.Point(138, 25);
            this.radUTF.Name = "radUTF";
            this.radUTF.Size = new System.Drawing.Size(66, 21);
            this.radUTF.TabIndex = 1;
            this.radUTF.Text = "UTF-8";
            // 
            // radASCII
            // 
            this.radASCII.AutoSize = true;
            this.radASCII.Location = new System.Drawing.Point(13, 25);
            this.radASCII.Name = "radASCII";
            this.radASCII.Size = new System.Drawing.Size(62, 21);
            this.radASCII.TabIndex = 0;
            this.radASCII.Text = "ASCII";
            // 
            // grpInternet
            // 
            this.grpInternet.Controls.Add(this.lblProxyServer);
            this.grpInternet.Controls.Add(this.lblProxyPort);
            this.grpInternet.Controls.Add(this.lblProxyLogin);
            this.grpInternet.Controls.Add(this.lblProxyPassword);
            this.grpInternet.Controls.Add(this.chkUseProxy);
            this.grpInternet.Controls.Add(this.txtProxyServer);
            this.grpInternet.Controls.Add(this.txtProxyPort);
            this.grpInternet.Controls.Add(this.txtProxyLogin);
            this.grpInternet.Controls.Add(this.txtProxyPass);
            this.grpInternet.Location = new System.Drawing.Point(11, 77);
            this.grpInternet.Name = "grpInternet";
            this.grpInternet.Size = new System.Drawing.Size(324, 195);
            this.grpInternet.TabIndex = 1;
            this.grpInternet.TabStop = false;
            this.grpInternet.Text = "grpInternet";
            // 
            // lblProxyServer
            // 
            this.lblProxyServer.AutoSize = true;
            this.lblProxyServer.Location = new System.Drawing.Point(22, 68);
            this.lblProxyServer.Name = "lblProxyServer";
            this.lblProxyServer.Size = new System.Drawing.Size(97, 17);
            this.lblProxyServer.TabIndex = 0;
            this.lblProxyServer.Text = "lblProxyServer";
            // 
            // lblProxyPort
            // 
            this.lblProxyPort.AutoSize = true;
            this.lblProxyPort.Location = new System.Drawing.Point(22, 97);
            this.lblProxyPort.Name = "lblProxyPort";
            this.lblProxyPort.Size = new System.Drawing.Size(83, 17);
            this.lblProxyPort.TabIndex = 1;
            this.lblProxyPort.Text = "lblProxyPort";
            // 
            // lblProxyLogin
            // 
            this.lblProxyLogin.AutoSize = true;
            this.lblProxyLogin.Location = new System.Drawing.Point(22, 126);
            this.lblProxyLogin.Name = "lblProxyLogin";
            this.lblProxyLogin.Size = new System.Drawing.Size(90, 17);
            this.lblProxyLogin.TabIndex = 2;
            this.lblProxyLogin.Text = "lblProxyLogin";
            // 
            // lblProxyPassword
            // 
            this.lblProxyPassword.AutoSize = true;
            this.lblProxyPassword.Location = new System.Drawing.Point(22, 155);
            this.lblProxyPassword.Name = "lblProxyPassword";
            this.lblProxyPassword.Size = new System.Drawing.Size(115, 17);
            this.lblProxyPassword.TabIndex = 3;
            this.lblProxyPassword.Text = "lblProxyPassword";
            // 
            // chkUseProxy
            // 
            this.chkUseProxy.AutoSize = true;
            this.chkUseProxy.Location = new System.Drawing.Point(22, 29);
            this.chkUseProxy.Name = "chkUseProxy";
            this.chkUseProxy.Size = new System.Drawing.Size(111, 21);
            this.chkUseProxy.TabIndex = 0;
            this.chkUseProxy.Text = "chkUseProxy";
            // 
            // txtProxyServer
            // 
            this.txtProxyServer.Location = new System.Drawing.Point(112, 58);
            this.txtProxyServer.Name = "txtProxyServer";
            this.txtProxyServer.Size = new System.Drawing.Size(192, 24);
            this.txtProxyServer.TabIndex = 1;
            // 
            // txtProxyPort
            // 
            this.txtProxyPort.Location = new System.Drawing.Point(112, 87);
            this.txtProxyPort.Name = "txtProxyPort";
            this.txtProxyPort.Size = new System.Drawing.Size(192, 24);
            this.txtProxyPort.TabIndex = 2;
            // 
            // txtProxyLogin
            // 
            this.txtProxyLogin.Location = new System.Drawing.Point(112, 117);
            this.txtProxyLogin.Name = "txtProxyLogin";
            this.txtProxyLogin.Size = new System.Drawing.Size(192, 24);
            this.txtProxyLogin.TabIndex = 3;
            // 
            // txtProxyPass
            // 
            this.txtProxyPass.Location = new System.Drawing.Point(112, 146);
            this.txtProxyPass.Name = "txtProxyPass";
            this.txtProxyPass.PasswordChar = '*';
            this.txtProxyPass.Size = new System.Drawing.Size(192, 24);
            this.txtProxyPass.TabIndex = 4;
            this.txtProxyPass.Text = "txtProxyPass";
            // 
            // grpOther
            // 
            this.grpOther.Controls.Add(this.chkRevisionsBackup);
            this.grpOther.Controls.Add(this.chkShowOnStart);
            this.grpOther.Location = new System.Drawing.Point(11, 279);
            this.grpOther.Name = "grpOther";
            this.grpOther.Size = new System.Drawing.Size(324, 79);
            this.grpOther.TabIndex = 2;
            this.grpOther.TabStop = false;
            this.grpOther.Text = "grpOther";
            // 
            // chkRevisionsBackup
            // 
            this.chkRevisionsBackup.AutoSize = true;
            this.chkRevisionsBackup.Location = new System.Drawing.Point(11, 49);
            this.chkRevisionsBackup.Name = "chkRevisionsBackup";
            this.chkRevisionsBackup.Size = new System.Drawing.Size(153, 21);
            this.chkRevisionsBackup.TabIndex = 1;
            this.chkRevisionsBackup.Text = "chkRevisionsBackup";
            // 
            // chkShowOnStart
            // 
            this.chkShowOnStart.AutoSize = true;
            this.chkShowOnStart.Location = new System.Drawing.Point(11, 22);
            this.chkShowOnStart.Name = "chkShowOnStart";
            this.chkShowOnStart.Size = new System.Drawing.Size(134, 21);
            this.chkShowOnStart.TabIndex = 0;
            this.chkShowOnStart.Text = "chkShowOnStart";
            // 
            // cmbLanguages
            // 
            this.cmbLanguages.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbLanguages.Location = new System.Drawing.Point(105, 383);
            this.cmbLanguages.Name = "cmbLanguages";
            this.cmbLanguages.Size = new System.Drawing.Size(230, 25);
            this.cmbLanguages.TabIndex = 4;
            // 
            // pageCharts
            // 
            this.pageCharts.Controls.Add(this.tabsCharts);
            this.pageCharts.Location = new System.Drawing.Point(4, 26);
            this.pageCharts.Name = "pageCharts";
            this.pageCharts.Padding = new System.Windows.Forms.Padding(3);
            this.pageCharts.Size = new System.Drawing.Size(710, 428);
            this.pageCharts.TabIndex = 4;
            this.pageCharts.Text = "pageCharts";
            this.pageCharts.UseVisualStyleBackColor = true;
            // 
            // tabsCharts
            // 
            this.tabsCharts.Controls.Add(this.pageTreeChart);
            this.tabsCharts.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabsCharts.Location = new System.Drawing.Point(3, 3);
            this.tabsCharts.Name = "tabsCharts";
            this.tabsCharts.SelectedIndex = 0;
            this.tabsCharts.Size = new System.Drawing.Size(704, 422);
            this.tabsCharts.TabIndex = 0;
            // 
            // pageTreeChart
            // 
            this.pageTreeChart.Controls.Add(this.grpTreePersons);
            this.pageTreeChart.Controls.Add(this.grpTreeDecor);
            this.pageTreeChart.Location = new System.Drawing.Point(4, 26);
            this.pageTreeChart.Name = "pageTreeChart";
            this.pageTreeChart.Size = new System.Drawing.Size(696, 392);
            this.pageTreeChart.TabIndex = 3;
            this.pageTreeChart.Text = "pageTreeChart";
            // 
            // grpTreePersons
            // 
            this.grpTreePersons.Controls.Add(this.chkSurname);
            this.grpTreePersons.Controls.Add(this.chkName);
            this.grpTreePersons.Controls.Add(this.chkPatronymic);
            this.grpTreePersons.Controls.Add(this.chkDiffLines);
            this.grpTreePersons.Controls.Add(this.chkBirthDate);
            this.grpTreePersons.Controls.Add(this.chkDeathDate);
            this.grpTreePersons.Controls.Add(this.chkKinship);
            this.grpTreePersons.Controls.Add(this.chkOnlyYears);
            this.grpTreePersons.Controls.Add(this.chkSignsVisible);
            this.grpTreePersons.Controls.Add(this.chkChildlessExclude);
            this.grpTreePersons.Controls.Add(this.chkTreeDecorative);
            this.grpTreePersons.Controls.Add(this.chkPortraitsVisible);
            this.grpTreePersons.Location = new System.Drawing.Point(11, 10);
            this.grpTreePersons.Name = "grpTreePersons";
            this.grpTreePersons.Padding = new System.Windows.Forms.Padding(10);
            this.grpTreePersons.Size = new System.Drawing.Size(391, 357);
            this.grpTreePersons.TabIndex = 0;
            this.grpTreePersons.TabStop = false;
            this.grpTreePersons.Text = "grpTreePersons";
            // 
            // chkSurname
            // 
            this.chkSurname.Location = new System.Drawing.Point(10, 27);
            this.chkSurname.Margin = new System.Windows.Forms.Padding(0, 0, 0, 5);
            this.chkSurname.Name = "chkSurname";
            this.chkSurname.Size = new System.Drawing.Size(349, 21);
            this.chkSurname.TabIndex = 0;
            this.chkSurname.Text = "chkSurname";
            // 
            // chkName
            // 
            this.chkName.Location = new System.Drawing.Point(10, 53);
            this.chkName.Margin = new System.Windows.Forms.Padding(0, 0, 0, 5);
            this.chkName.Name = "chkName";
            this.chkName.Size = new System.Drawing.Size(349, 21);
            this.chkName.TabIndex = 1;
            this.chkName.Text = "chkName";
            // 
            // chkPatronymic
            // 
            this.chkPatronymic.Location = new System.Drawing.Point(10, 79);
            this.chkPatronymic.Margin = new System.Windows.Forms.Padding(0, 0, 0, 5);
            this.chkPatronymic.Name = "chkPatronymic";
            this.chkPatronymic.Size = new System.Drawing.Size(349, 21);
            this.chkPatronymic.TabIndex = 2;
            this.chkPatronymic.Text = "chkPatronymic";
            // 
            // chkDiffLines
            // 
            this.chkDiffLines.Location = new System.Drawing.Point(10, 105);
            this.chkDiffLines.Margin = new System.Windows.Forms.Padding(0, 0, 0, 5);
            this.chkDiffLines.Name = "chkDiffLines";
            this.chkDiffLines.Size = new System.Drawing.Size(349, 20);
            this.chkDiffLines.TabIndex = 3;
            this.chkDiffLines.Text = "chkDiffLines";
            // 
            // chkBirthDate
            // 
            this.chkBirthDate.Location = new System.Drawing.Point(10, 130);
            this.chkBirthDate.Margin = new System.Windows.Forms.Padding(0, 0, 0, 5);
            this.chkBirthDate.Name = "chkBirthDate";
            this.chkBirthDate.Size = new System.Drawing.Size(349, 21);
            this.chkBirthDate.TabIndex = 4;
            this.chkBirthDate.Text = "chkBirthDate";
            // 
            // chkDeathDate
            // 
            this.chkDeathDate.Location = new System.Drawing.Point(10, 156);
            this.chkDeathDate.Margin = new System.Windows.Forms.Padding(0, 0, 0, 5);
            this.chkDeathDate.Name = "chkDeathDate";
            this.chkDeathDate.Size = new System.Drawing.Size(349, 20);
            this.chkDeathDate.TabIndex = 5;
            this.chkDeathDate.Text = "chkDeathDate";
            // 
            // chkKinship
            // 
            this.chkKinship.Location = new System.Drawing.Point(10, 207);
            this.chkKinship.Margin = new System.Windows.Forms.Padding(0, 0, 0, 5);
            this.chkKinship.Name = "chkKinship";
            this.chkKinship.Size = new System.Drawing.Size(349, 21);
            this.chkKinship.TabIndex = 7;
            this.chkKinship.Text = "chkKinship";
            // 
            // chkOnlyYears
            // 
            this.chkOnlyYears.Location = new System.Drawing.Point(30, 181);
            this.chkOnlyYears.Margin = new System.Windows.Forms.Padding(20, 0, 0, 5);
            this.chkOnlyYears.Name = "chkOnlyYears";
            this.chkOnlyYears.Size = new System.Drawing.Size(326, 21);
            this.chkOnlyYears.TabIndex = 6;
            this.chkOnlyYears.Text = "chkOnlyYears";
            // 
            // chkSignsVisible
            // 
            this.chkSignsVisible.Location = new System.Drawing.Point(10, 233);
            this.chkSignsVisible.Margin = new System.Windows.Forms.Padding(0, 0, 0, 5);
            this.chkSignsVisible.Name = "chkSignsVisible";
            this.chkSignsVisible.Size = new System.Drawing.Size(349, 21);
            this.chkSignsVisible.TabIndex = 8;
            this.chkSignsVisible.Text = "chkSignsVisible";
            // 
            // chkChildlessExclude
            // 
            this.chkChildlessExclude.Location = new System.Drawing.Point(10, 320);
            this.chkChildlessExclude.Margin = new System.Windows.Forms.Padding(0, 10, 0, 0);
            this.chkChildlessExclude.Name = "chkChildlessExclude";
            this.chkChildlessExclude.Size = new System.Drawing.Size(349, 21);
            this.chkChildlessExclude.TabIndex = 11;
            this.chkChildlessExclude.Text = "chkChildlessExclude";
            // 
            // chkTreeDecorative
            // 
            this.chkTreeDecorative.Location = new System.Drawing.Point(10, 259);
            this.chkTreeDecorative.Margin = new System.Windows.Forms.Padding(0, 0, 0, 5);
            this.chkTreeDecorative.Name = "chkTreeDecorative";
            this.chkTreeDecorative.Size = new System.Drawing.Size(349, 21);
            this.chkTreeDecorative.TabIndex = 9;
            this.chkTreeDecorative.Text = "chkTreeDecorative";
            // 
            // chkPortraitsVisible
            // 
            this.chkPortraitsVisible.Location = new System.Drawing.Point(10, 285);
            this.chkPortraitsVisible.Margin = new System.Windows.Forms.Padding(0, 0, 0, 5);
            this.chkPortraitsVisible.Name = "chkPortraitsVisible";
            this.chkPortraitsVisible.Size = new System.Drawing.Size(349, 20);
            this.chkPortraitsVisible.TabIndex = 10;
            this.chkPortraitsVisible.Text = "chkPortraitsVisible";
            // 
            // grpTreeDecor
            // 
            this.grpTreeDecor.Controls.Add(this.lblFont);
            this.grpTreeDecor.Controls.Add(this.panMaleColor);
            this.grpTreeDecor.Controls.Add(this.panFemaleColor);
            this.grpTreeDecor.Controls.Add(this.panUnkSexColor);
            this.grpTreeDecor.Controls.Add(this.panUnHusbandColor);
            this.grpTreeDecor.Controls.Add(this.panUnWifeColor);
            this.grpTreeDecor.Controls.Add(this.panDefFont);
            this.grpTreeDecor.Location = new System.Drawing.Point(410, 10);
            this.grpTreeDecor.Name = "grpTreeDecor";
            this.grpTreeDecor.Size = new System.Drawing.Size(259, 234);
            this.grpTreeDecor.TabIndex = 1;
            this.grpTreeDecor.TabStop = false;
            this.grpTreeDecor.Text = "grpTreeDecor";
            // 
            // lblFont
            // 
            this.lblFont.Location = new System.Drawing.Point(22, 175);
            this.lblFont.Name = "lblFont";
            this.lblFont.Size = new System.Drawing.Size(70, 16);
            this.lblFont.TabIndex = 0;
            this.lblFont.Text = "lblFont";
            // 
            // panMaleColor
            // 
            this.panMaleColor.BackColor = System.Drawing.SystemColors.Control;
            this.panMaleColor.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.panMaleColor.Controls.Add(this.lblMaleColor);
            this.panMaleColor.Cursor = System.Windows.Forms.Cursors.Hand;
            this.panMaleColor.Location = new System.Drawing.Point(22, 19);
            this.panMaleColor.Name = "panMaleColor";
            this.panMaleColor.Size = new System.Drawing.Size(103, 31);
            this.panMaleColor.TabIndex = 0;
            // 
            // lblMaleColor
            // 
            this.lblMaleColor.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lblMaleColor.Location = new System.Drawing.Point(0, 0);
            this.lblMaleColor.Name = "lblMaleColor";
            this.lblMaleColor.Size = new System.Drawing.Size(99, 27);
            this.lblMaleColor.TabIndex = 1;
            this.lblMaleColor.Text = "lblMaleColor";
            this.lblMaleColor.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            this.lblMaleColor.Click += new System.EventHandler(this.PanColor_Click);
            // 
            // panFemaleColor
            // 
            this.panFemaleColor.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.panFemaleColor.Controls.Add(this.lblFemaleColor);
            this.panFemaleColor.Cursor = System.Windows.Forms.Cursors.Hand;
            this.panFemaleColor.Location = new System.Drawing.Point(134, 19);
            this.panFemaleColor.Name = "panFemaleColor";
            this.panFemaleColor.Size = new System.Drawing.Size(103, 31);
            this.panFemaleColor.TabIndex = 1;
            // 
            // lblFemaleColor
            // 
            this.lblFemaleColor.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lblFemaleColor.Location = new System.Drawing.Point(0, 0);
            this.lblFemaleColor.Name = "lblFemaleColor";
            this.lblFemaleColor.Size = new System.Drawing.Size(99, 27);
            this.lblFemaleColor.TabIndex = 1;
            this.lblFemaleColor.Text = "lblFemaleColor";
            this.lblFemaleColor.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            this.lblFemaleColor.Click += new System.EventHandler(this.PanColor_Click);
            // 
            // panUnkSexColor
            // 
            this.panUnkSexColor.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.panUnkSexColor.Controls.Add(this.lblUnkSexColor);
            this.panUnkSexColor.Cursor = System.Windows.Forms.Cursors.Hand;
            this.panUnkSexColor.Location = new System.Drawing.Point(22, 58);
            this.panUnkSexColor.Name = "panUnkSexColor";
            this.panUnkSexColor.Size = new System.Drawing.Size(215, 31);
            this.panUnkSexColor.TabIndex = 2;
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
            // panUnHusbandColor
            // 
            this.panUnHusbandColor.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.panUnHusbandColor.Controls.Add(this.lblUnHusbandColor);
            this.panUnHusbandColor.Cursor = System.Windows.Forms.Cursors.Hand;
            this.panUnHusbandColor.Location = new System.Drawing.Point(22, 97);
            this.panUnHusbandColor.Name = "panUnHusbandColor";
            this.panUnHusbandColor.Size = new System.Drawing.Size(215, 31);
            this.panUnHusbandColor.TabIndex = 3;
            // 
            // lblUnHusbandColor
            // 
            this.lblUnHusbandColor.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lblUnHusbandColor.Location = new System.Drawing.Point(0, 0);
            this.lblUnHusbandColor.Name = "lblUnHusbandColor";
            this.lblUnHusbandColor.Size = new System.Drawing.Size(211, 27);
            this.lblUnHusbandColor.TabIndex = 1;
            this.lblUnHusbandColor.Text = "lblUnHusbandColor";
            this.lblUnHusbandColor.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            this.lblUnHusbandColor.Click += new System.EventHandler(this.PanColor_Click);
            // 
            // panUnWifeColor
            // 
            this.panUnWifeColor.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.panUnWifeColor.Controls.Add(this.lblUnWifeColor);
            this.panUnWifeColor.Cursor = System.Windows.Forms.Cursors.Hand;
            this.panUnWifeColor.Location = new System.Drawing.Point(22, 136);
            this.panUnWifeColor.Name = "panUnWifeColor";
            this.panUnWifeColor.Size = new System.Drawing.Size(215, 30);
            this.panUnWifeColor.TabIndex = 4;
            // 
            // lblUnWifeColor
            // 
            this.lblUnWifeColor.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lblUnWifeColor.Location = new System.Drawing.Point(0, 0);
            this.lblUnWifeColor.Name = "lblUnWifeColor";
            this.lblUnWifeColor.Size = new System.Drawing.Size(211, 26);
            this.lblUnWifeColor.TabIndex = 1;
            this.lblUnWifeColor.Text = "lblUnWifeColor";
            this.lblUnWifeColor.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            this.lblUnWifeColor.Click += new System.EventHandler(this.PanColor_Click);
            // 
            // panDefFont
            // 
            this.panDefFont.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.panDefFont.Controls.Add(this.lblChartFont);
            this.panDefFont.Cursor = System.Windows.Forms.Cursors.Hand;
            this.panDefFont.Location = new System.Drawing.Point(22, 194);
            this.panDefFont.Name = "panDefFont";
            this.panDefFont.Size = new System.Drawing.Size(215, 31);
            this.panDefFont.TabIndex = 5;
            this.panDefFont.Click += new System.EventHandler(this.PanDefFont_Click);
            // 
            // lblChartFont
            // 
            this.lblChartFont.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lblChartFont.Location = new System.Drawing.Point(0, 0);
            this.lblChartFont.Name = "lblChartFont";
            this.lblChartFont.Size = new System.Drawing.Size(211, 27);
            this.lblChartFont.TabIndex = 0;
            this.lblChartFont.Text = "lblChartFont";
            this.lblChartFont.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            this.lblChartFont.Click += new System.EventHandler(this.PanDefFont_Click);
            // 
            // pageUIView
            // 
            this.pageUIView.Controls.Add(this.PageControl2);
            this.pageUIView.Location = new System.Drawing.Point(4, 26);
            this.pageUIView.Name = "pageUIView";
            this.pageUIView.Size = new System.Drawing.Size(710, 428);
            this.pageUIView.TabIndex = 1;
            this.pageUIView.Text = "pageUIView";
            // 
            // PageControl2
            // 
            this.PageControl2.Controls.Add(this.pageViewCommon);
            this.PageControl2.Controls.Add(this.pageViewPersons);
            this.PageControl2.Location = new System.Drawing.Point(0, 0);
            this.PageControl2.Name = "PageControl2";
            this.PageControl2.SelectedIndex = 0;
            this.PageControl2.Size = new System.Drawing.Size(707, 424);
            this.PageControl2.TabIndex = 0;
            // 
            // pageViewCommon
            // 
            this.pageViewCommon.Controls.Add(this.rgFNPFormat);
            this.pageViewCommon.Controls.Add(this.grpDateFormat);
            this.pageViewCommon.Controls.Add(this.chkPlacesWithAddress);
            this.pageViewCommon.Controls.Add(this.chkHighlightUnparented);
            this.pageViewCommon.Controls.Add(this.chkShowDatesSigns);
            this.pageViewCommon.Controls.Add(this.chkShowDatesCalendar);
            this.pageViewCommon.Controls.Add(this.chkHighlightUnmarried);
            this.pageViewCommon.Location = new System.Drawing.Point(4, 26);
            this.pageViewCommon.Name = "pageViewCommon";
            this.pageViewCommon.Size = new System.Drawing.Size(699, 394);
            this.pageViewCommon.TabIndex = 0;
            this.pageViewCommon.Text = "pageViewCommon";
            // 
            // rgFNPFormat
            // 
            this.rgFNPFormat.Controls.Add(this.radS_N_P);
            this.rgFNPFormat.Controls.Add(this.radS_NP);
            this.rgFNPFormat.Controls.Add(this.radSNP);
            this.rgFNPFormat.Location = new System.Drawing.Point(11, 10);
            this.rgFNPFormat.Name = "rgFNPFormat";
            this.rgFNPFormat.Size = new System.Drawing.Size(259, 118);
            this.rgFNPFormat.TabIndex = 0;
            this.rgFNPFormat.TabStop = false;
            this.rgFNPFormat.Text = "rgFNPFormat";
            // 
            // radS_N_P
            // 
            this.radS_N_P.Location = new System.Drawing.Point(11, 78);
            this.radS_N_P.Name = "radS_N_P";
            this.radS_N_P.Size = new System.Drawing.Size(224, 29);
            this.radS_N_P.TabIndex = 2;
            this.radS_N_P.Text = "radS_N_P";
            // 
            // radS_NP
            // 
            this.radS_NP.Location = new System.Drawing.Point(11, 49);
            this.radS_NP.Name = "radS_NP";
            this.radS_NP.Size = new System.Drawing.Size(224, 29);
            this.radS_NP.TabIndex = 1;
            this.radS_NP.Text = "radS_NP";
            // 
            // radSNP
            // 
            this.radSNP.Location = new System.Drawing.Point(11, 21);
            this.radSNP.Name = "radSNP";
            this.radSNP.Size = new System.Drawing.Size(224, 29);
            this.radSNP.TabIndex = 0;
            this.radSNP.Text = "radSNP";
            // 
            // grpDateFormat
            // 
            this.grpDateFormat.Controls.Add(this.radYMD);
            this.grpDateFormat.Controls.Add(this.radDMY);
            this.grpDateFormat.Location = new System.Drawing.Point(326, 10);
            this.grpDateFormat.Name = "grpDateFormat";
            this.grpDateFormat.Size = new System.Drawing.Size(259, 87);
            this.grpDateFormat.TabIndex = 1;
            this.grpDateFormat.TabStop = false;
            this.grpDateFormat.Text = "grpDateFormat";
            // 
            // radYMD
            // 
            this.radYMD.Location = new System.Drawing.Point(11, 49);
            this.radYMD.Name = "radYMD";
            this.radYMD.Size = new System.Drawing.Size(146, 29);
            this.radYMD.TabIndex = 1;
            this.radYMD.Text = "YYYY.MM.DD";
            // 
            // radDMY
            // 
            this.radDMY.Location = new System.Drawing.Point(11, 19);
            this.radDMY.Name = "radDMY";
            this.radDMY.Size = new System.Drawing.Size(146, 30);
            this.radDMY.TabIndex = 0;
            this.radDMY.Text = "DD.MM.YYYY";
            // 
            // chkPlacesWithAddress
            // 
            this.chkPlacesWithAddress.Location = new System.Drawing.Point(11, 191);
            this.chkPlacesWithAddress.Name = "chkPlacesWithAddress";
            this.chkPlacesWithAddress.Size = new System.Drawing.Size(259, 21);
            this.chkPlacesWithAddress.TabIndex = 2;
            this.chkPlacesWithAddress.Text = "chkPlacesWithAddress";
            // 
            // chkHighlightUnparented
            // 
            this.chkHighlightUnparented.Location = new System.Drawing.Point(11, 220);
            this.chkHighlightUnparented.Name = "chkHighlightUnparented";
            this.chkHighlightUnparented.Size = new System.Drawing.Size(338, 21);
            this.chkHighlightUnparented.TabIndex = 3;
            this.chkHighlightUnparented.Text = "chkHighlightUnparented";
            // 
            // chkShowDatesSigns
            // 
            this.chkShowDatesSigns.Location = new System.Drawing.Point(326, 134);
            this.chkShowDatesSigns.Name = "chkShowDatesSigns";
            this.chkShowDatesSigns.Size = new System.Drawing.Size(338, 21);
            this.chkShowDatesSigns.TabIndex = 4;
            this.chkShowDatesSigns.Text = "chkShowDatesSigns";
            // 
            // chkShowDatesCalendar
            // 
            this.chkShowDatesCalendar.Location = new System.Drawing.Point(326, 107);
            this.chkShowDatesCalendar.Name = "chkShowDatesCalendar";
            this.chkShowDatesCalendar.Size = new System.Drawing.Size(338, 21);
            this.chkShowDatesCalendar.TabIndex = 4;
            this.chkShowDatesCalendar.Text = "chkShowDatesCalendar";
            // 
            // chkHighlightUnmarried
            // 
            this.chkHighlightUnmarried.Location = new System.Drawing.Point(11, 249);
            this.chkHighlightUnmarried.Name = "chkHighlightUnmarried";
            this.chkHighlightUnmarried.Size = new System.Drawing.Size(338, 21);
            this.chkHighlightUnmarried.TabIndex = 4;
            this.chkHighlightUnmarried.Text = "chkHighlightUnmarried";
            // 
            // pageViewPersons
            // 
            this.pageViewPersons.Controls.Add(this.panel1);
            this.pageViewPersons.Controls.Add(this.btnColumnUp);
            this.pageViewPersons.Controls.Add(this.btnColumnDown);
            this.pageViewPersons.Controls.Add(this.btnDefList);
            this.pageViewPersons.Location = new System.Drawing.Point(4, 26);
            this.pageViewPersons.Name = "pageViewPersons";
            this.pageViewPersons.Size = new System.Drawing.Size(699, 394);
            this.pageViewPersons.TabIndex = 1;
            this.pageViewPersons.Text = "pageViewPersons";
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.lstPersonColumns);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Left;
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Name = "panel1";
            this.panel1.Padding = new System.Windows.Forms.Padding(10);
            this.panel1.Size = new System.Drawing.Size(487, 394);
            this.panel1.TabIndex = 2;
            // 
            // lstPersonColumns
            // 
            this.lstPersonColumns.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lstPersonColumns.Location = new System.Drawing.Point(10, 10);
            this.lstPersonColumns.Name = "lstPersonColumns";
            this.lstPersonColumns.Size = new System.Drawing.Size(467, 374);
            this.lstPersonColumns.TabIndex = 1;
            this.lstPersonColumns.ItemCheck += new System.Windows.Forms.ItemCheckEventHandler(this.ListPersonColumns_ItemCheck);
            // 
            // btnColumnUp
            // 
            this.btnColumnUp.Location = new System.Drawing.Point(497, 10);
            this.btnColumnUp.Margin = new System.Windows.Forms.Padding(10, 10, 10, 0);
            this.btnColumnUp.Name = "btnColumnUp";
            this.btnColumnUp.Size = new System.Drawing.Size(39, 34);
            this.btnColumnUp.TabIndex = 0;
            this.btnColumnUp.Click += new System.EventHandler(this.btnColumnUp_Click);
            // 
            // btnColumnDown
            // 
            this.btnColumnDown.Location = new System.Drawing.Point(497, 54);
            this.btnColumnDown.Margin = new System.Windows.Forms.Padding(10);
            this.btnColumnDown.Name = "btnColumnDown";
            this.btnColumnDown.Size = new System.Drawing.Size(39, 34);
            this.btnColumnDown.TabIndex = 1;
            this.btnColumnDown.Click += new System.EventHandler(this.btnColumnDown_Click);
            // 
            // btnDefList
            // 
            this.btnDefList.Location = new System.Drawing.Point(497, 340);
            this.btnDefList.Margin = new System.Windows.Forms.Padding(10);
            this.btnDefList.Name = "btnDefList";
            this.btnDefList.Size = new System.Drawing.Size(192, 44);
            this.btnDefList.TabIndex = 1;
            this.btnDefList.Text = "btnDefList";
            this.btnDefList.Click += new System.EventHandler(this.btnDefList_Click);
            // 
            // pagePedigree
            // 
            this.pagePedigree.Controls.Add(this.grpPedigree);
            this.pagePedigree.Location = new System.Drawing.Point(4, 26);
            this.pagePedigree.Name = "pagePedigree";
            this.pagePedigree.Size = new System.Drawing.Size(710, 428);
            this.pagePedigree.TabIndex = 3;
            this.pagePedigree.Text = "pagePedigree";
            // 
            // grpPedigree
            // 
            this.grpPedigree.Controls.Add(this.chkAttributes);
            this.grpPedigree.Controls.Add(this.chkNotes);
            this.grpPedigree.Controls.Add(this.chkSources);
            this.grpPedigree.Controls.Add(this.grpPedigreeFormat);
            this.grpPedigree.Location = new System.Drawing.Point(11, 10);
            this.grpPedigree.Name = "grpPedigree";
            this.grpPedigree.Size = new System.Drawing.Size(405, 194);
            this.grpPedigree.TabIndex = 0;
            this.grpPedigree.TabStop = false;
            this.grpPedigree.Text = "grpPedigree";
            // 
            // chkAttributes
            // 
            this.chkAttributes.Location = new System.Drawing.Point(22, 19);
            this.chkAttributes.Name = "chkAttributes";
            this.chkAttributes.Size = new System.Drawing.Size(349, 21);
            this.chkAttributes.TabIndex = 0;
            this.chkAttributes.Text = "chkAttributes";
            // 
            // chkNotes
            // 
            this.chkNotes.Location = new System.Drawing.Point(22, 39);
            this.chkNotes.Name = "chkNotes";
            this.chkNotes.Size = new System.Drawing.Size(349, 21);
            this.chkNotes.TabIndex = 1;
            this.chkNotes.Text = "chkNotes";
            // 
            // chkSources
            // 
            this.chkSources.Location = new System.Drawing.Point(22, 58);
            this.chkSources.Name = "chkSources";
            this.chkSources.Size = new System.Drawing.Size(349, 21);
            this.chkSources.TabIndex = 2;
            this.chkSources.Text = "chkSources";
            // 
            // grpPedigreeFormat
            // 
            this.grpPedigreeFormat.Controls.Add(this.radExcess);
            this.grpPedigreeFormat.Controls.Add(this.radCompact);
            this.grpPedigreeFormat.Location = new System.Drawing.Point(22, 87);
            this.grpPedigreeFormat.Name = "grpPedigreeFormat";
            this.grpPedigreeFormat.Size = new System.Drawing.Size(349, 88);
            this.grpPedigreeFormat.TabIndex = 3;
            this.grpPedigreeFormat.TabStop = false;
            this.grpPedigreeFormat.Text = "grpPedigreeFormat";
            // 
            // radExcess
            // 
            this.radExcess.Location = new System.Drawing.Point(22, 19);
            this.radExcess.Name = "radExcess";
            this.radExcess.Size = new System.Drawing.Size(146, 30);
            this.radExcess.TabIndex = 3;
            this.radExcess.Text = "radExcess";
            // 
            // radCompact
            // 
            this.radCompact.Location = new System.Drawing.Point(22, 49);
            this.radCompact.Name = "radCompact";
            this.radCompact.Size = new System.Drawing.Size(146, 29);
            this.radCompact.TabIndex = 2;
            this.radCompact.Text = "radCompact";
            // 
            // pagePlugins
            // 
            this.pagePlugins.BackColor = System.Drawing.SystemColors.Control;
            this.pagePlugins.Controls.Add(this.lvPlugins);
            this.pagePlugins.Location = new System.Drawing.Point(4, 26);
            this.pagePlugins.Margin = new System.Windows.Forms.Padding(0);
            this.pagePlugins.Name = "pagePlugins";
            this.pagePlugins.Padding = new System.Windows.Forms.Padding(10);
            this.pagePlugins.Size = new System.Drawing.Size(710, 428);
            this.pagePlugins.TabIndex = 5;
            this.pagePlugins.Text = "pagePlugins";
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
            this.lvPlugins.Location = new System.Drawing.Point(10, 10);
            this.lvPlugins.MultiSelect = false;
            this.lvPlugins.Name = "lvPlugins";
            this.lvPlugins.Size = new System.Drawing.Size(690, 408);
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
            this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnAccept.Location = new System.Drawing.Point(470, 476);
            this.btnAccept.Name = "btnAccept";
            this.btnAccept.Size = new System.Drawing.Size(114, 30);
            this.btnAccept.TabIndex = 1;
            this.btnAccept.Text = "btnAccept";
            this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(594, 476);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(114, 30);
            this.btnCancel.TabIndex = 2;
            this.btnCancel.Text = "btnCancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // OptionsDlg
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
            this.Name = "OptionsDlg";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "OptionsDlg";
            this.PageControl1.ResumeLayout(false);
            this.pageCommon.ResumeLayout(false);
            this.pageCommon.PerformLayout();
            this.grpEncoding.ResumeLayout(false);
            this.grpEncoding.PerformLayout();
            this.grpInternet.ResumeLayout(false);
            this.grpInternet.PerformLayout();
            this.grpOther.ResumeLayout(false);
            this.grpOther.PerformLayout();
            this.pageCharts.ResumeLayout(false);
            this.tabsCharts.ResumeLayout(false);
            this.pageTreeChart.ResumeLayout(false);
            this.grpTreePersons.ResumeLayout(false);
            this.grpTreeDecor.ResumeLayout(false);
            this.panMaleColor.ResumeLayout(false);
            this.panFemaleColor.ResumeLayout(false);
            this.panUnkSexColor.ResumeLayout(false);
            this.panUnHusbandColor.ResumeLayout(false);
            this.panUnWifeColor.ResumeLayout(false);
            this.panDefFont.ResumeLayout(false);
            this.pageUIView.ResumeLayout(false);
            this.PageControl2.ResumeLayout(false);
            this.pageViewCommon.ResumeLayout(false);
            this.rgFNPFormat.ResumeLayout(false);
            this.grpDateFormat.ResumeLayout(false);
            this.pageViewPersons.ResumeLayout(false);
            this.panel1.ResumeLayout(false);
            this.pagePedigree.ResumeLayout(false);
            this.grpPedigree.ResumeLayout(false);
            this.grpPedigreeFormat.ResumeLayout(false);
            this.pagePlugins.ResumeLayout(false);
            this.ResumeLayout(false);
        }
    }
}
