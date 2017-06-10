using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
{
    partial class OptionsDlg
    {
        private TabControl PageControl1;
        private TabPage pageCommon;
        private Button btnAccept;
        private Button btnCancel;
        private TabPage pageTreeChart;
        private GroupBox grpTreePersons;
        private CheckBox chkSurname;
        private CheckBox chkName;
        private CheckBox chkPatronymic;
        private CheckBox chkDiffLines;
        private CheckBox chkBirthDate;
        private CheckBox chkDeathDate;
        private CheckBox chkKinship;
        private GroupBox grpTreeDecor;
        private Panel panMaleColor;
        private Panel panFemaleColor;
        private Panel panUnkSexColor;
        private Panel panUnHusbandColor;
        private Panel panUnWifeColor;
        private ColorDialog ColorDialog1;
        private GroupBox grpInternet;
        private Label lblProxyServer;
        private Label lblProxyPort;
        private Label lblProxyLogin;
        private Label lblProxyPassword;
        private CheckBox chkUseProxy;
        private TextBox txtProxyServer;
        private TextBox txtProxyPort;
        private TextBox txtProxyLogin;
        private TextBox txtProxyPass;
        private TabPage pageUIView;
        private TabControl PageControl2;
        private TabPage pageViewCommon;
        private TabPage pageViewPersons;
        //private CheckedListBox lstPersonColumns; // FIXME: GKv3 DevRestriction
        private Button btnColumnUp;
        private Button btnColumnDown;
        private Button btnDefList;
        private GroupBox rgFNPFormat;
        private GroupBox grpDateFormat;
        private CheckBox chkPlacesWithAddress;
        private GroupBox grpOther;
        private CheckBox chkShowOnStart;
        private CheckBox chkHighlightUnparented;
        private CheckBox chkHighlightUnmarried;
        private CheckBox chkOnlyYears;
        private CheckBox chkSignsVisible;
        private CheckBox chkChildlessExclude;
        private Label lblFont;
        private Panel panDefFont;
        private TabPage pagePedigree;
        private GroupBox grpPedigree;
        private CheckBox chkAttributes;
        private CheckBox chkNotes;
        private CheckBox chkSources;
        private GroupBox grpPedigreeFormat;
        private Label lblLanguage;
        private ComboBox cmbLanguages;
        private CheckBox chkTreeDecorative;
        private CheckBox chkPortraitsVisible;
        private RadioButton radSNP;
        private RadioButton radS_NP;
        private RadioButton radS_N_P;
        private RadioButton radDMY;
        private RadioButton radYMD;
        private RadioButton radExcess;
        private RadioButton radCompact;
        private CheckBox chkShowDatesSigns;
        private CheckBox chkShowDatesCalendar;
        //private ColumnHeader columnHeader4;
        //private ColumnHeader columnHeader3;
        //private ColumnHeader columnHeader2;
        //private ColumnHeader columnHeader1;
        private GKListViewStub lvPlugins;
        private TabPage pagePlugins;
        private Label lblChartFont;
        private TabControl tabsCharts;
        private TabPage pageCharts;
        private Label lblMaleColor;
        private Label lblFemaleColor;
        private Label lblUnkSexColor;
        private Label lblUnHusbandColor;
        private Label lblUnWifeColor;
        private Panel panel1;
        private RadioButton radFBNone;
        private RadioButton radFBOnlyPrev;
        private RadioButton radFBEachRevision;
        private GroupBox grpFileBackup;
        private CheckBox chkAutosave;
        private NumericUpDown numASMin;
        private Label lblMinutes;
        private GroupBox groupBox1;
        private CheckBox chkGenerations;
        private GKUI.Components.ACOptionsControl ancOptionsControl1;
        private TabPage pageAncCircle;
        private CheckBox chkExtendWomanSurnames;
        private RadioButton radMaiden_Married;
        private RadioButton radMarried_Maiden;
        private RadioButton radMaiden;
        private RadioButton radMarried;
        private GroupBox grpAdvancedNames;
        private CheckBox chkAllowMediaDirectRefs;
        private TabPage pageMultimedia;
        private CheckBox chkEmbeddedMediaPlayer;
        private CheckBox chkLoadRecentFiles;
        private CheckBox chkRemovableMediaWarning;
        private CheckBox chkDefaultPortraits;
        private ComboBox cmbGeocoder;
        private Label lblGeocoder;

        private void InitializeComponent()
        {
            PageControl1 = new TabControl();
            pageCommon = new TabPage();
            groupBox1 = new GroupBox();
            lblMinutes = new Label();
            numASMin = new NumericUpDown();
            chkAutosave = new CheckBox();
            grpFileBackup = new GroupBox();
            radFBEachRevision = new RadioButton();
            radFBOnlyPrev = new RadioButton();
            radFBNone = new RadioButton();
            lblGeocoder = new Label();
            lblLanguage = new Label();
            grpInternet = new GroupBox();
            lblProxyServer = new Label();
            lblProxyPort = new Label();
            lblProxyLogin = new Label();
            lblProxyPassword = new Label();
            chkUseProxy = new CheckBox();
            txtProxyServer = new TextBox();
            txtProxyPort = new TextBox();
            txtProxyLogin = new TextBox();
            txtProxyPass = new TextBox();
            grpOther = new GroupBox();
            chkLoadRecentFiles = new CheckBox();
            chkShowOnStart = new CheckBox();
            cmbGeocoder = new ComboBox();
            cmbLanguages = new ComboBox();
            pageMultimedia = new TabPage();
            chkAllowMediaDirectRefs = new CheckBox();
            chkEmbeddedMediaPlayer = new CheckBox();
            chkRemovableMediaWarning = new CheckBox();
            pageCharts = new TabPage();
            tabsCharts = new TabControl();
            pageTreeChart = new TabPage();
            grpTreePersons = new GroupBox();
            chkSurname = new CheckBox();
            chkName = new CheckBox();
            chkPatronymic = new CheckBox();
            chkDiffLines = new CheckBox();
            chkBirthDate = new CheckBox();
            chkDeathDate = new CheckBox();
            chkKinship = new CheckBox();
            chkDefaultPortraits = new CheckBox();
            chkOnlyYears = new CheckBox();
            chkSignsVisible = new CheckBox();
            chkChildlessExclude = new CheckBox();
            chkTreeDecorative = new CheckBox();
            chkPortraitsVisible = new CheckBox();
            grpTreeDecor = new GroupBox();
            lblFont = new Label();
            panMaleColor = new Panel();
            lblMaleColor = new Label();
            panFemaleColor = new Panel();
            lblFemaleColor = new Label();
            panUnkSexColor = new Panel();
            lblUnkSexColor = new Label();
            panUnHusbandColor = new Panel();
            lblUnHusbandColor = new Label();
            panUnWifeColor = new Panel();
            lblUnWifeColor = new Label();
            panDefFont = new Panel();
            lblChartFont = new Label();
            pageAncCircle = new TabPage();
            ancOptionsControl1 = new GKUI.Components.ACOptionsControl();
            pageUIView = new TabPage();
            PageControl2 = new TabControl();
            pageViewCommon = new TabPage();
            grpAdvancedNames = new GroupBox();
            radMarried = new RadioButton();
            radMaiden = new RadioButton();
            radMarried_Maiden = new RadioButton();
            radMaiden_Married = new RadioButton();
            chkExtendWomanSurnames = new CheckBox();
            rgFNPFormat = new GroupBox();
            radS_N_P = new RadioButton();
            radS_NP = new RadioButton();
            radSNP = new RadioButton();
            grpDateFormat = new GroupBox();
            radYMD = new RadioButton();
            radDMY = new RadioButton();
            chkPlacesWithAddress = new CheckBox();
            chkHighlightUnparented = new CheckBox();
            chkShowDatesSigns = new CheckBox();
            chkShowDatesCalendar = new CheckBox();
            chkHighlightUnmarried = new CheckBox();
            pageViewPersons = new TabPage();
            panel1 = new Panel();
            lstPersonColumns = new CheckedListBox();
            btnColumnUp = new Button();
            btnColumnDown = new Button();
            btnDefList = new Button();
            pagePedigree = new TabPage();
            grpPedigree = new GroupBox();
            chkAttributes = new CheckBox();
            chkNotes = new CheckBox();
            chkGenerations = new CheckBox();
            chkSources = new CheckBox();
            grpPedigreeFormat = new GroupBox();
            radExcess = new RadioButton();
            radCompact = new RadioButton();
            pagePlugins = new TabPage();
            lvPlugins = new GKListViewStub();
            columnHeader1 = new ColumnHeader();
            columnHeader2 = new ColumnHeader();
            columnHeader3 = new ColumnHeader();
            columnHeader4 = new ColumnHeader();
            btnAccept = new Button();
            btnCancel = new Button();
            ColorDialog1 = new ColorDialog();
            PageControl1.SuspendLayout();
            pageCommon.SuspendLayout();
            groupBox1.SuspendLayout();
            grpFileBackup.SuspendLayout();
            grpInternet.SuspendLayout();
            grpOther.SuspendLayout();
            pageMultimedia.SuspendLayout();
            pageCharts.SuspendLayout();
            tabsCharts.SuspendLayout();
            pageTreeChart.SuspendLayout();
            grpTreePersons.SuspendLayout();
            grpTreeDecor.SuspendLayout();
            panMaleColor.SuspendLayout();
            panFemaleColor.SuspendLayout();
            panUnkSexColor.SuspendLayout();
            panUnHusbandColor.SuspendLayout();
            panUnWifeColor.SuspendLayout();
            panDefFont.SuspendLayout();
            pageAncCircle.SuspendLayout();
            pageUIView.SuspendLayout();
            PageControl2.SuspendLayout();
            pageViewCommon.SuspendLayout();
            grpAdvancedNames.SuspendLayout();
            rgFNPFormat.SuspendLayout();
            grpDateFormat.SuspendLayout();
            pageViewPersons.SuspendLayout();
            panel1.SuspendLayout();
            pagePedigree.SuspendLayout();
            grpPedigree.SuspendLayout();
            grpPedigreeFormat.SuspendLayout();
            pagePlugins.SuspendLayout();
            SuspendLayout();

            PageControl1.Controls.Add(pageCommon);
            PageControl1.Controls.Add(pageMultimedia);
            PageControl1.Controls.Add(pageCharts);
            PageControl1.Controls.Add(pageUIView);
            PageControl1.Controls.Add(pagePedigree);
            PageControl1.Controls.Add(pagePlugins);
            PageControl1.Dock = DockStyle.Top;
            PageControl1.Location = new Point(0, 0);
            PageControl1.SelectedIndex = 0;
            PageControl1.Size = new Size(749, 509);

            pageCommon.Controls.Add(groupBox1);
            pageCommon.Controls.Add(lblGeocoder);
            pageCommon.Controls.Add(lblLanguage);
            pageCommon.Controls.Add(grpInternet);
            pageCommon.Controls.Add(grpOther);
            pageCommon.Controls.Add(cmbGeocoder);
            pageCommon.Controls.Add(cmbLanguages);
            pageCommon.Location = new Point(4, 26);
            pageCommon.Padding = new Padding(10);
            pageCommon.Size = new Size(741, 479);
            pageCommon.Text = "pageCommon";

            groupBox1.Controls.Add(lblMinutes);
            groupBox1.Controls.Add(numASMin);
            groupBox1.Controls.Add(chkAutosave);
            groupBox1.Controls.Add(grpFileBackup);
            groupBox1.Location = new Point(348, 10);
            groupBox1.Padding = new Padding(2);
            groupBox1.Size = new Size(379, 219);

            lblMinutes.Location = new Point(289, 188);
            lblMinutes.Size = new Size(66, 17);
            lblMinutes.Text = "lblMinutes";

            numASMin.Location = new Point(234, 182);
            numASMin.Margin = new Padding(2);
            numASMin.MaxValue = 120;
            numASMin.MinValue = 1;
            numASMin.Size = new Size(49, 24);
            numASMin.Value = 1;

            chkAutosave.Location = new Point(12, 184);
            chkAutosave.Size = new Size(109, 21);
            chkAutosave.Text = "chkAutosave";

            grpFileBackup.Controls.Add(radFBEachRevision);
            grpFileBackup.Controls.Add(radFBOnlyPrev);
            grpFileBackup.Controls.Add(radFBNone);
            grpFileBackup.Location = new Point(12, 30);
            grpFileBackup.Padding = new Padding(2);
            grpFileBackup.Size = new Size(342, 136);
            grpFileBackup.Text = "grpFileBackup";

            radFBEachRevision.Location = new Point(12, 98);
            radFBEachRevision.Size = new Size(294, 24);
            radFBEachRevision.Text = "radFBEachRevision";

            radFBOnlyPrev.Location = new Point(12, 64);
            radFBOnlyPrev.Size = new Size(294, 24);
            radFBOnlyPrev.Text = "radFBOnlyPrev";

            radFBNone.Location = new Point(12, 30);
            radFBNone.Size = new Size(294, 24);
            radFBNone.Text = "radFBNone";

            lblGeocoder.Location = new Point(359, 429);
            lblGeocoder.Size = new Size(79, 17);
            lblGeocoder.Text = "lblGeocoder";

            lblLanguage.Location = new Point(11, 429);
            lblLanguage.Size = new Size(80, 17);
            lblLanguage.Text = "lblLanguage";

            grpInternet.Controls.Add(lblProxyServer);
            grpInternet.Controls.Add(lblProxyPort);
            grpInternet.Controls.Add(lblProxyLogin);
            grpInternet.Controls.Add(lblProxyPassword);
            grpInternet.Controls.Add(chkUseProxy);
            grpInternet.Controls.Add(txtProxyServer);
            grpInternet.Controls.Add(txtProxyPort);
            grpInternet.Controls.Add(txtProxyLogin);
            grpInternet.Controls.Add(txtProxyPass);
            grpInternet.Location = new Point(11, 10);
            grpInternet.Padding = new Padding(2);
            grpInternet.Size = new Size(324, 195);
            grpInternet.Text = "grpInternet";

            lblProxyServer.Location = new Point(12, 61);
            lblProxyServer.Size = new Size(97, 17);
            lblProxyServer.Text = "lblProxyServer";

            lblProxyPort.Location = new Point(12, 90);
            lblProxyPort.Size = new Size(83, 17);
            lblProxyPort.Text = "lblProxyPort";

            lblProxyLogin.Location = new Point(12, 120);
            lblProxyLogin.Size = new Size(90, 17);
            lblProxyLogin.Text = "lblProxyLogin";

            lblProxyPassword.Location = new Point(12, 149);
            lblProxyPassword.Size = new Size(115, 17);
            lblProxyPassword.Text = "lblProxyPassword";

            chkUseProxy.Location = new Point(22, 29);
            chkUseProxy.Size = new Size(111, 21);
            chkUseProxy.Text = "chkUseProxy";

            txtProxyServer.Location = new Point(112, 58);
            txtProxyServer.Size = new Size(192, 24);

            txtProxyPort.Location = new Point(112, 88);
            txtProxyPort.Size = new Size(192, 24);

            txtProxyLogin.Location = new Point(112, 118);
            txtProxyLogin.Size = new Size(192, 24);

            txtProxyPass.Location = new Point(112, 146);
            txtProxyPass.PasswordChar = '*';
            txtProxyPass.Size = new Size(192, 24);
            txtProxyPass.Text = "txtProxyPass";

            grpOther.Controls.Add(chkLoadRecentFiles);
            grpOther.Controls.Add(chkShowOnStart);
            grpOther.Location = new Point(11, 244);
            grpOther.Padding = new Padding(10);
            grpOther.Size = new Size(715, 151);
            grpOther.Text = "grpOther";

            chkLoadRecentFiles.Location = new Point(20, 68);
            chkLoadRecentFiles.Size = new Size(149, 21);
            chkLoadRecentFiles.Text = "chkLoadRecentFiles";

            chkShowOnStart.Location = new Point(20, 37);
            chkShowOnStart.Size = new Size(134, 21);
            chkShowOnStart.Text = "chkShowOnStart";

            cmbGeocoder.ReadOnly = true;
            cmbGeocoder.Items.AddRange(new object[] {
                                           "Google",
                                           "Yandex"});
            cmbGeocoder.Location = new Point(470, 425);
            cmbGeocoder.Size = new Size(230, 25);

            cmbLanguages.ReadOnly = true;
            cmbLanguages.Location = new Point(104, 425);
            cmbLanguages.Size = new Size(230, 25);

            pageMultimedia.BackgroundColor = SystemColors.Control;
            pageMultimedia.Controls.Add(chkAllowMediaDirectRefs);
            pageMultimedia.Controls.Add(chkEmbeddedMediaPlayer);
            pageMultimedia.Controls.Add(chkRemovableMediaWarning);
            pageMultimedia.Location = new Point(4, 26);
            pageMultimedia.Padding = new Padding(10);
            pageMultimedia.Size = new Size(741, 479);
            pageMultimedia.Text = "pageMultimedia";

            chkAllowMediaDirectRefs.Location = new Point(20, 102);
            chkAllowMediaDirectRefs.Size = new Size(178, 21);
            chkAllowMediaDirectRefs.Text = "chkAllowMediaDirectRefs";

            chkEmbeddedMediaPlayer.Location = new Point(20, 61);
            chkEmbeddedMediaPlayer.Size = new Size(189, 21);
            chkEmbeddedMediaPlayer.Text = "chkEmbeddedMediaPlayer";

            chkRemovableMediaWarning.Location = new Point(20, 20);
            chkRemovableMediaWarning.Size = new Size(206, 21);
            chkRemovableMediaWarning.Text = "chkRemovableMediaWarning";

            pageCharts.BackgroundColor = SystemColors.Control;
            pageCharts.Controls.Add(tabsCharts);
            pageCharts.Location = new Point(4, 26);
            pageCharts.Padding = new Padding(10);
            pageCharts.Size = new Size(741, 479);
            pageCharts.Text = "pageCharts";

            tabsCharts.Controls.Add(pageTreeChart);
            tabsCharts.Controls.Add(pageAncCircle);
            tabsCharts.Dock = DockStyle.Fill;
            tabsCharts.Location = new Point(10, 10);
            tabsCharts.SelectedIndex = 0;
            tabsCharts.Size = new Size(721, 459);

            pageTreeChart.BackgroundColor = SystemColors.Control;
            pageTreeChart.Controls.Add(grpTreePersons);
            pageTreeChart.Controls.Add(grpTreeDecor);
            pageTreeChart.Location = new Point(4, 26);
            pageTreeChart.Size = new Size(713, 429);
            pageTreeChart.Text = "pageTreeChart";

            grpTreePersons.Controls.Add(chkSurname);
            grpTreePersons.Controls.Add(chkName);
            grpTreePersons.Controls.Add(chkPatronymic);
            grpTreePersons.Controls.Add(chkDiffLines);
            grpTreePersons.Controls.Add(chkBirthDate);
            grpTreePersons.Controls.Add(chkDeathDate);
            grpTreePersons.Controls.Add(chkKinship);
            grpTreePersons.Controls.Add(chkDefaultPortraits);
            grpTreePersons.Controls.Add(chkOnlyYears);
            grpTreePersons.Controls.Add(chkSignsVisible);
            grpTreePersons.Controls.Add(chkChildlessExclude);
            grpTreePersons.Controls.Add(chkTreeDecorative);
            grpTreePersons.Controls.Add(chkPortraitsVisible);
            grpTreePersons.Location = new Point(11, 10);
            grpTreePersons.Padding = new Padding(10);
            grpTreePersons.Size = new Size(391, 394);
            grpTreePersons.Text = "grpTreePersons";

            chkSurname.Location = new Point(20, 28);
            chkSurname.Size = new Size(349, 21);
            chkSurname.Text = "chkSurname";

            chkName.Location = new Point(20, 52);
            chkName.Size = new Size(349, 21);
            chkName.Text = "chkName";

            chkPatronymic.Location = new Point(20, 79);
            chkPatronymic.Size = new Size(349, 21);
            chkPatronymic.Text = "chkPatronymic";

            chkDiffLines.Location = new Point(20, 105);
            chkDiffLines.Size = new Size(349, 20);
            chkDiffLines.Text = "chkDiffLines";

            chkBirthDate.Location = new Point(20, 130);
            chkBirthDate.Size = new Size(349, 21);
            chkBirthDate.Text = "chkBirthDate";

            chkDeathDate.Location = new Point(20, 156);
            chkDeathDate.Size = new Size(349, 20);
            chkDeathDate.Text = "chkDeathDate";

            chkKinship.Location = new Point(18, 208);
            chkKinship.Size = new Size(349, 21);
            chkKinship.Text = "chkKinship";

            chkDefaultPortraits.Location = new Point(40, 310);
            chkDefaultPortraits.Size = new Size(326, 21);
            chkDefaultPortraits.Text = "chkDefaultPortraits";

            chkOnlyYears.Location = new Point(40, 181);
            chkOnlyYears.Size = new Size(326, 21);
            chkOnlyYears.Text = "chkOnlyYears";

            chkSignsVisible.Location = new Point(18, 232);
            chkSignsVisible.Size = new Size(349, 21);
            chkSignsVisible.Text = "chkSignsVisible";

            chkChildlessExclude.Location = new Point(18, 354);
            chkChildlessExclude.Size = new Size(349, 21);
            chkChildlessExclude.Text = "chkChildlessExclude";

            chkTreeDecorative.Location = new Point(18, 259);
            chkTreeDecorative.Size = new Size(349, 21);
            chkTreeDecorative.Text = "chkTreeDecorative";

            chkPortraitsVisible.Location = new Point(18, 285);
            chkPortraitsVisible.Size = new Size(349, 20);
            chkPortraitsVisible.Text = "chkPortraitsVisible";
            chkPortraitsVisible.CheckedChanged += chkPortraitsVisible_CheckedChanged;

            grpTreeDecor.Controls.Add(lblFont);
            grpTreeDecor.Controls.Add(panMaleColor);
            grpTreeDecor.Controls.Add(panFemaleColor);
            grpTreeDecor.Controls.Add(panUnkSexColor);
            grpTreeDecor.Controls.Add(panUnHusbandColor);
            grpTreeDecor.Controls.Add(panUnWifeColor);
            grpTreeDecor.Controls.Add(panDefFont);
            grpTreeDecor.Location = new Point(410, 10);
            grpTreeDecor.Padding = new Padding(2);
            grpTreeDecor.Size = new Size(245, 254);
            grpTreeDecor.Text = "grpTreeDecor";

            lblFont.Location = new Point(15, 192);
            lblFont.Size = new Size(70, 16);
            lblFont.Text = "lblFont";

            panMaleColor.BackgroundColor = SystemColors.Control;
            panMaleColor.BorderStyle = BorderStyle.Fixed3D;
            panMaleColor.Controls.Add(lblMaleColor);
            panMaleColor.Cursor = Cursors.Pointer;
            panMaleColor.Location = new Point(12, 30);
            panMaleColor.Size = new Size(103, 32);

            lblMaleColor.Dock = DockStyle.Fill;
            lblMaleColor.Location = new Point(0, 0);
            lblMaleColor.Size = new Size(99, 28);
            lblMaleColor.Text = "lblMaleColor";
            lblMaleColor.Click += PanColor_Click;

            panFemaleColor.BorderStyle = BorderStyle.Fixed3D;
            panFemaleColor.Controls.Add(lblFemaleColor);
            panFemaleColor.Cursor = Cursors.Pointer;
            panFemaleColor.Location = new Point(126, 30);
            panFemaleColor.Size = new Size(103, 32);

            lblFemaleColor.Dock = DockStyle.Fill;
            lblFemaleColor.Location = new Point(0, 0);
            lblFemaleColor.Size = new Size(99, 28);
            lblFemaleColor.Text = "lblFemaleColor";
            lblFemaleColor.Click += PanColor_Click;

            panUnkSexColor.BorderStyle = BorderStyle.Fixed3D;
            panUnkSexColor.Controls.Add(lblUnkSexColor);
            panUnkSexColor.Cursor = Cursors.Pointer;
            panUnkSexColor.Location = new Point(12, 71);
            panUnkSexColor.Size = new Size(215, 32);

            lblUnkSexColor.Dock = DockStyle.Fill;
            lblUnkSexColor.Location = new Point(0, 0);
            lblUnkSexColor.Size = new Size(211, 28);
            lblUnkSexColor.Text = "label7";
            lblUnkSexColor.Click += PanColor_Click;

            panUnHusbandColor.BorderStyle = BorderStyle.Fixed3D;
            panUnHusbandColor.Controls.Add(lblUnHusbandColor);
            panUnHusbandColor.Cursor = Cursors.Pointer;
            panUnHusbandColor.Location = new Point(12, 112);
            panUnHusbandColor.Size = new Size(215, 32);

            lblUnHusbandColor.Dock = DockStyle.Fill;
            lblUnHusbandColor.Location = new Point(0, 0);
            lblUnHusbandColor.Size = new Size(211, 28);
            lblUnHusbandColor.Text = "lblUnHusbandColor";
            lblUnHusbandColor.Click += PanColor_Click;

            panUnWifeColor.BorderStyle = BorderStyle.Fixed3D;
            panUnWifeColor.Controls.Add(lblUnWifeColor);
            panUnWifeColor.Cursor = Cursors.Pointer;
            panUnWifeColor.Location = new Point(12, 152);
            panUnWifeColor.Size = new Size(215, 30);

            lblUnWifeColor.Dock = DockStyle.Fill;
            lblUnWifeColor.Location = new Point(0, 0);
            lblUnWifeColor.Size = new Size(211, 26);
            lblUnWifeColor.Text = "lblUnWifeColor";
            lblUnWifeColor.Click += PanColor_Click;

            panDefFont.BorderStyle = BorderStyle.Fixed3D;
            panDefFont.Controls.Add(lblChartFont);
            panDefFont.Cursor = Cursors.Pointer;
            panDefFont.Location = new Point(12, 209);
            panDefFont.Size = new Size(215, 32);
            panDefFont.Click += panDefFont_Click;

            lblChartFont.Dock = DockStyle.Fill;
            lblChartFont.Location = new Point(0, 0);
            lblChartFont.Size = new Size(211, 28);
            lblChartFont.Text = "lblChartFont";
            lblChartFont.Click += panDefFont_Click;

            pageAncCircle.BackgroundColor = SystemColors.Control;
            pageAncCircle.Controls.Add(ancOptionsControl1);
            pageAncCircle.Location = new Point(4, 26);
            pageAncCircle.Size = new Size(713, 429);
            pageAncCircle.Text = "pageAncCircle";

            ancOptionsControl1.Dock = DockStyle.Fill;
            ancOptionsControl1.Font = new Font("Tahoma", 8.25F, FontStyle.None);
            ancOptionsControl1.Location = new Point(0, 0);
            ancOptionsControl1.Options = null;
            ancOptionsControl1.Size = new Size(713, 429);

            pageUIView.Controls.Add(PageControl2);
            pageUIView.Location = new Point(4, 26);
            pageUIView.Padding = new Padding(10);
            pageUIView.Size = new Size(741, 479);
            pageUIView.Text = "pageUIView";

            PageControl2.Controls.Add(pageViewCommon);
            PageControl2.Controls.Add(pageViewPersons);
            PageControl2.Dock = DockStyle.Fill;
            PageControl2.Location = new Point(10, 10);
            PageControl2.SelectedIndex = 0;
            PageControl2.Size = new Size(721, 459);

            pageViewCommon.Controls.Add(grpAdvancedNames);
            pageViewCommon.Controls.Add(rgFNPFormat);
            pageViewCommon.Controls.Add(grpDateFormat);
            pageViewCommon.Controls.Add(chkPlacesWithAddress);
            pageViewCommon.Controls.Add(chkHighlightUnparented);
            pageViewCommon.Controls.Add(chkShowDatesSigns);
            pageViewCommon.Controls.Add(chkShowDatesCalendar);
            pageViewCommon.Controls.Add(chkHighlightUnmarried);
            pageViewCommon.Location = new Point(4, 26);
            pageViewCommon.Padding = new Padding(10);
            pageViewCommon.Size = new Size(713, 429);
            pageViewCommon.Text = "pageViewCommon";

            grpAdvancedNames.Controls.Add(radMarried);
            grpAdvancedNames.Controls.Add(radMaiden);
            grpAdvancedNames.Controls.Add(radMarried_Maiden);
            grpAdvancedNames.Controls.Add(radMaiden_Married);
            grpAdvancedNames.Controls.Add(chkExtendWomanSurnames);
            grpAdvancedNames.Location = new Point(326, 191);
            grpAdvancedNames.Padding = new Padding(2);
            grpAdvancedNames.Size = new Size(367, 198);
            grpAdvancedNames.Text = "AdvancedNames";

            radMarried.Location = new Point(26, 159);
            radMarried.Size = new Size(93, 21);
            radMarried.Text = "radMarried";

            radMaiden.Location = new Point(26, 125);
            radMaiden.Size = new Size(91, 21);
            radMaiden.Text = "radMaiden";

            radMarried_Maiden.Location = new Point(26, 91);
            radMarried_Maiden.Size = new Size(143, 21);
            radMarried_Maiden.Text = "radMarried_Maiden";

            radMaiden_Married.Location = new Point(26, 58);
            radMaiden_Married.Size = new Size(143, 21);
            radMaiden_Married.Text = "radMaiden_Married";

            chkExtendWomanSurnames.Location = new Point(12, 22);
            chkExtendWomanSurnames.Size = new Size(184, 21);
            chkExtendWomanSurnames.Text = "ExtendWomanSurnames";
            chkExtendWomanSurnames.CheckedChanged += chkExtendWomanSurnames_CheckedChanged;

            rgFNPFormat.Controls.Add(radS_N_P);
            rgFNPFormat.Controls.Add(radS_NP);
            rgFNPFormat.Controls.Add(radSNP);
            rgFNPFormat.Location = new Point(11, 10);
            rgFNPFormat.Padding = new Padding(2);
            rgFNPFormat.Size = new Size(259, 118);
            rgFNPFormat.Text = "rgFNPFormat";

            radS_N_P.Location = new Point(11, 78);
            radS_N_P.Size = new Size(224, 29);
            radS_N_P.Text = "radS_N_P";

            radS_NP.Location = new Point(11, 49);
            radS_NP.Size = new Size(224, 29);
            radS_NP.Text = "radS_NP";

            radSNP.Location = new Point(11, 21);
            radSNP.Size = new Size(224, 29);
            radSNP.Text = "radSNP";

            grpDateFormat.Controls.Add(radYMD);
            grpDateFormat.Controls.Add(radDMY);
            grpDateFormat.Location = new Point(326, 10);
            grpDateFormat.Padding = new Padding(2);
            grpDateFormat.Size = new Size(259, 88);
            grpDateFormat.Text = "grpDateFormat";

            radYMD.Location = new Point(11, 49);
            radYMD.Size = new Size(146, 29);
            radYMD.Text = "YYYY.MM.DD";

            radDMY.Location = new Point(11, 19);
            radDMY.Size = new Size(146, 30);
            radDMY.Text = "DD.MM.YYYY";

            chkPlacesWithAddress.Location = new Point(11, 191);
            chkPlacesWithAddress.Size = new Size(259, 21);
            chkPlacesWithAddress.Text = "chkPlacesWithAddress";

            chkHighlightUnparented.Location = new Point(11, 220);
            chkHighlightUnparented.Size = new Size(338, 21);
            chkHighlightUnparented.Text = "chkHighlightUnparented";

            chkShowDatesSigns.Location = new Point(326, 134);
            chkShowDatesSigns.Size = new Size(338, 21);
            chkShowDatesSigns.Text = "chkShowDatesSigns";

            chkShowDatesCalendar.Location = new Point(326, 108);
            chkShowDatesCalendar.Size = new Size(338, 21);
            chkShowDatesCalendar.Text = "chkShowDatesCalendar";

            chkHighlightUnmarried.Location = new Point(11, 249);
            chkHighlightUnmarried.Size = new Size(338, 21);
            chkHighlightUnmarried.Text = "chkHighlightUnmarried";

            pageViewPersons.Controls.Add(panel1);
            pageViewPersons.Controls.Add(btnColumnUp);
            pageViewPersons.Controls.Add(btnColumnDown);
            pageViewPersons.Controls.Add(btnDefList);
            pageViewPersons.Location = new Point(4, 26);
            pageViewPersons.Size = new Size(713, 429);
            pageViewPersons.Text = "pageViewPersons";

            panel1.Controls.Add(lstPersonColumns);
            panel1.Dock = DockStyle.Left;
            panel1.Location = new Point(0, 0);
            panel1.Padding = new Padding(10);
            panel1.Size = new Size(488, 429);

            lstPersonColumns.Dock = DockStyle.Fill;
            lstPersonColumns.Location = new Point(10, 10);
            lstPersonColumns.Margin = new Padding(2);
            lstPersonColumns.Name = "lstPersonColumns";
            lstPersonColumns.Size = new Size(468, 409);
            lstPersonColumns.TabIndex = 1;
            lstPersonColumns.ItemCheck += new ItemCheckEventHandler(ListPersonColumns_ItemCheck);

            btnColumnUp.Location = new Point(498, 10);
            btnColumnUp.Size = new Size(39, 34);
            btnColumnUp.Click += btnColumnUp_Click;

            btnColumnDown.Location = new Point(498, 54);
            btnColumnDown.Size = new Size(39, 34);
            btnColumnDown.Click += btnColumnDown_Click;

            btnDefList.Location = new Point(498, 340);
            btnDefList.Size = new Size(192, 44);
            btnDefList.Text = "btnDefList";
            btnDefList.Click += btnDefList_Click;

            pagePedigree.Controls.Add(grpPedigree);
            pagePedigree.Location = new Point(4, 26);
            pagePedigree.Padding = new Padding(10);
            pagePedigree.Size = new Size(741, 479);
            pagePedigree.Text = "pagePedigree";

            grpPedigree.Controls.Add(chkAttributes);
            grpPedigree.Controls.Add(chkNotes);
            grpPedigree.Controls.Add(chkGenerations);
            grpPedigree.Controls.Add(chkSources);
            grpPedigree.Controls.Add(grpPedigreeFormat);
            grpPedigree.Location = new Point(12, 12);
            grpPedigree.Padding = new Padding(10);
            grpPedigree.Size = new Size(405, 258);
            grpPedigree.Text = "grpPedigree";

            chkAttributes.Location = new Point(20, 28);
            chkAttributes.Size = new Size(349, 21);
            chkAttributes.Text = "chkAttributes";

            chkNotes.Location = new Point(20, 52);
            chkNotes.Size = new Size(349, 21);
            chkNotes.Text = "chkNotes";

            chkGenerations.Location = new Point(20, 105);
            chkGenerations.Size = new Size(349, 21);
            chkGenerations.Text = "chkGenerations";

            chkSources.Location = new Point(20, 79);
            chkSources.Size = new Size(349, 21);
            chkSources.Text = "chkSources";

            grpPedigreeFormat.Controls.Add(radExcess);
            grpPedigreeFormat.Controls.Add(radCompact);
            grpPedigreeFormat.Location = new Point(20, 134);
            grpPedigreeFormat.Padding = new Padding(10);
            grpPedigreeFormat.Size = new Size(349, 101);
            grpPedigreeFormat.Text = "grpPedigreeFormat";

            radExcess.Location = new Point(10, 28);
            radExcess.Size = new Size(146, 30);
            radExcess.Text = "radExcess";

            radCompact.Location = new Point(10, 62);
            radCompact.Size = new Size(146, 29);
            radCompact.Text = "radCompact";

            pagePlugins.BackgroundColor = SystemColors.Control;
            pagePlugins.Controls.Add(lvPlugins);
            pagePlugins.Location = new Point(4, 26);
            pagePlugins.Padding = new Padding(10);
            pagePlugins.Size = new Size(741, 479);
            pagePlugins.Text = "pagePlugins";

            lvPlugins.Columns.AddRange(new ColumnHeader[] {
                                           columnHeader1,
                                           columnHeader2,
                                           columnHeader3,
                                           columnHeader4});
            lvPlugins.Dock = DockStyle.Fill;
            lvPlugins.FullRowSelect = true;
            lvPlugins.Location = new Point(10, 10);
            lvPlugins.MultiSelect = false;
            lvPlugins.Size = new Size(721, 459);
            lvPlugins.UseCompatibleStateImageBehavior = false;
            lvPlugins.View = View.Details;

            columnHeader1.Text = "Title";
            columnHeader1.Width = 75;

            columnHeader2.Text = "Version";

            columnHeader3.Text = "Copyright";
            columnHeader3.Width = 125;

            columnHeader4.Text = "Description";
            columnHeader4.Width = 250;

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(492, 525);
            btnAccept.Size = new Size(114, 30);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(622, 525);
            btnCancel.Size = new Size(114, 30);
            btnCancel.Text = "btnCancel";

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(749, 568);
            Controls.Add(PageControl1);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "OptionsDlg";
            PageControl1.ResumeLayout();
            pageCommon.ResumeLayout();
            groupBox1.ResumeLayout();
            grpFileBackup.ResumeLayout();
            grpInternet.ResumeLayout();
            grpOther.ResumeLayout();
            pageMultimedia.ResumeLayout();
            pageCharts.ResumeLayout();
            tabsCharts.ResumeLayout();
            pageTreeChart.ResumeLayout();
            grpTreePersons.ResumeLayout();
            grpTreeDecor.ResumeLayout();
            panMaleColor.ResumeLayout();
            panFemaleColor.ResumeLayout();
            panUnkSexColor.ResumeLayout();
            panUnHusbandColor.ResumeLayout();
            panUnWifeColor.ResumeLayout();
            panDefFont.ResumeLayout();
            pageAncCircle.ResumeLayout();
            pageUIView.ResumeLayout();
            PageControl2.ResumeLayout();
            pageViewCommon.ResumeLayout();
            grpAdvancedNames.ResumeLayout();
            rgFNPFormat.ResumeLayout();
            grpDateFormat.ResumeLayout();
            pageViewPersons.ResumeLayout();
            panel1.ResumeLayout();
            pagePedigree.ResumeLayout();
            grpPedigree.ResumeLayout();
            grpPedigreeFormat.ResumeLayout();
            pagePlugins.ResumeLayout();
            ResumeLayout();
        }
    }
}
