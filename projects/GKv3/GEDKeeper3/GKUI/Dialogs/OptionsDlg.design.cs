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
        private GKListViewStub lstPersonColumns; // FIXME: GKv3 DevRestriction
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
            lstPersonColumns = new GKListViewStub();
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
            btnAccept = new Button();
            btnCancel = new Button();
            ColorDialog1 = new ColorDialog();

            SuspendLayout();

            PageControl1.Pages.Add(pageCommon);
            PageControl1.Pages.Add(pageMultimedia);
            PageControl1.Pages.Add(pageCharts);
            PageControl1.Pages.Add(pageUIView);
            PageControl1.Pages.Add(pagePedigree);
            PageControl1.Pages.Add(pagePlugins);
            PageControl1.SelectedIndex = 0;
            PageControl1.Size = new Size(749, 509);

            pageCommon.Size = new Size(741, 479);
            pageCommon.Text = "pageCommon";
            pageCommon.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { grpInternet, groupBox1 }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { grpOther }
                    }
                }
            };

            groupBox1.Size = new Size(379, 219);
            groupBox1.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { grpFileBackup }
                    },
                    new TableRow {
                        Cells = { chkAutosave, numASMin, lblMinutes }
                    }
                }
            };

            lblMinutes.Size = new Size(66, 17);
            lblMinutes.Text = "lblMinutes";

            numASMin.MaxValue = 120;
            numASMin.MinValue = 1;
            numASMin.Size = new Size(49, 24);
            numASMin.Value = 1;

            chkAutosave.Size = new Size(109, 21);
            chkAutosave.Text = "chkAutosave";

            grpFileBackup.Size = new Size(342, 136);
            grpFileBackup.Text = "grpFileBackup";
            grpFileBackup.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { radFBNone }
                    },
                    new TableRow {
                        Cells = { radFBOnlyPrev }
                    },
                    new TableRow {
                        Cells = { radFBEachRevision }
                    }
                }
            };

            radFBEachRevision.Size = new Size(294, 24);
            radFBEachRevision.Text = "radFBEachRevision";

            radFBOnlyPrev.Size = new Size(294, 24);
            radFBOnlyPrev.Text = "radFBOnlyPrev";

            radFBNone.Size = new Size(294, 24);
            radFBNone.Text = "radFBNone";

            lblGeocoder.Size = new Size(79, 17);
            lblGeocoder.Text = "lblGeocoder";

            lblLanguage.Size = new Size(80, 17);
            lblLanguage.Text = "lblLanguage";

            grpInternet.Size = new Size(324, 195);
            grpInternet.Text = "grpInternet";
            grpInternet.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { chkUseProxy }
                    },
                    new TableRow {
                        Cells = { lblProxyServer, txtProxyServer }
                    },
                    new TableRow {
                        Cells = { lblProxyPort, txtProxyPort }
                    },
                    new TableRow {
                        Cells = { lblProxyLogin, txtProxyLogin }
                    },
                    new TableRow {
                        Cells = { lblProxyPassword, txtProxyPass }
                    }
                }
            };

            lblProxyServer.Size = new Size(97, 17);
            lblProxyServer.Text = "lblProxyServer";

            lblProxyPort.Size = new Size(83, 17);
            lblProxyPort.Text = "lblProxyPort";

            lblProxyLogin.Size = new Size(90, 17);
            lblProxyLogin.Text = "lblProxyLogin";

            lblProxyPassword.Size = new Size(115, 17);
            lblProxyPassword.Text = "lblProxyPassword";

            chkUseProxy.Size = new Size(111, 21);
            chkUseProxy.Text = "chkUseProxy";

            txtProxyServer.Size = new Size(192, 24);

            txtProxyPort.Size = new Size(192, 24);

            txtProxyLogin.Size = new Size(192, 24);

            //txtProxyPass.PasswordChar = '*';
            txtProxyPass.Size = new Size(192, 24);
            txtProxyPass.Text = "txtProxyPass";

            grpOther.Size = new Size(715, 151);
            grpOther.Text = "grpOther";
            grpOther.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { chkLoadRecentFiles }
                    },
                    new TableRow {
                        Cells = { chkShowOnStart }
                    },
                    new TableRow {
                        Cells = { lblLanguage, cmbLanguages }
                    },
                    new TableRow {
                        Cells = { lblGeocoder, cmbGeocoder }
                    }
                }
            };

            chkLoadRecentFiles.Size = new Size(149, 21);
            chkLoadRecentFiles.Text = "chkLoadRecentFiles";

            chkShowOnStart.Size = new Size(134, 21);
            chkShowOnStart.Text = "chkShowOnStart";

            cmbGeocoder.ReadOnly = true;
            cmbGeocoder.Items.Add("Google");
            cmbGeocoder.Items.Add("Yandex");
            cmbGeocoder.Size = new Size(230, 25);

            cmbLanguages.ReadOnly = true;
            cmbLanguages.Size = new Size(230, 25);

            pageMultimedia.BackgroundColor = SystemColors.Control;
            pageMultimedia.Padding = new Padding(10);
            pageMultimedia.Size = new Size(741, 479);
            pageMultimedia.Text = "pageMultimedia";
            pageMultimedia.Content = new StackLayout {
                Orientation = Orientation.Vertical,
                Items = { chkRemovableMediaWarning, chkEmbeddedMediaPlayer, chkAllowMediaDirectRefs }
            };

            chkAllowMediaDirectRefs.Size = new Size(178, 21);
            chkAllowMediaDirectRefs.Text = "chkAllowMediaDirectRefs";

            chkEmbeddedMediaPlayer.Size = new Size(189, 21);
            chkEmbeddedMediaPlayer.Text = "chkEmbeddedMediaPlayer";

            chkRemovableMediaWarning.Size = new Size(206, 21);
            chkRemovableMediaWarning.Text = "chkRemovableMediaWarning";

            pageCharts.BackgroundColor = SystemColors.Control;
            pageCharts.Content = tabsCharts;
            pageCharts.Padding = new Padding(10);
            pageCharts.Size = new Size(741, 479);
            pageCharts.Text = "pageCharts";

            tabsCharts.Pages.Add(pageTreeChart);
            tabsCharts.Pages.Add(pageAncCircle);
            tabsCharts.SelectedIndex = 0;
            tabsCharts.Size = new Size(721, 459);

            pageTreeChart.BackgroundColor = SystemColors.Control;
            pageTreeChart.Size = new Size(713, 429);
            pageTreeChart.Text = "pageTreeChart";
            pageTreeChart.Content = new StackLayout {
                Orientation = Orientation.Horizontal,
                Items = { grpTreePersons, grpTreeDecor }
            };

            grpTreePersons.Padding = new Padding(10);
            grpTreePersons.Size = new Size(391, 394);
            grpTreePersons.Text = "grpTreePersons";
            grpTreePersons.Content = new StackLayout {
                Orientation = Orientation.Vertical,
                Items = { chkSurname, chkName, chkPatronymic, chkDiffLines, chkBirthDate, chkDeathDate, chkOnlyYears,
                    chkKinship, chkSignsVisible, chkTreeDecorative, chkPortraitsVisible, chkDefaultPortraits,
                    chkChildlessExclude }
            };

            chkSurname.Size = new Size(349, 21);
            chkSurname.Text = "chkSurname";

            chkName.Size = new Size(349, 21);
            chkName.Text = "chkName";

            chkPatronymic.Size = new Size(349, 21);
            chkPatronymic.Text = "chkPatronymic";

            chkDiffLines.Size = new Size(349, 20);
            chkDiffLines.Text = "chkDiffLines";

            chkBirthDate.Size = new Size(349, 21);
            chkBirthDate.Text = "chkBirthDate";

            chkDeathDate.Size = new Size(349, 20);
            chkDeathDate.Text = "chkDeathDate";

            chkKinship.Size = new Size(349, 21);
            chkKinship.Text = "chkKinship";

            chkDefaultPortraits.Size = new Size(326, 21);
            chkDefaultPortraits.Text = "chkDefaultPortraits";

            chkOnlyYears.Size = new Size(326, 21);
            chkOnlyYears.Text = "chkOnlyYears";

            chkSignsVisible.Size = new Size(349, 21);
            chkSignsVisible.Text = "chkSignsVisible";

            chkChildlessExclude.Size = new Size(349, 21);
            chkChildlessExclude.Text = "chkChildlessExclude";

            chkTreeDecorative.Size = new Size(349, 21);
            chkTreeDecorative.Text = "chkTreeDecorative";

            chkPortraitsVisible.Size = new Size(349, 20);
            chkPortraitsVisible.Text = "chkPortraitsVisible";
            chkPortraitsVisible.CheckedChanged += chkPortraitsVisible_CheckedChanged;

            grpTreeDecor.Padding = new Padding(2);
            grpTreeDecor.Size = new Size(245, 254);
            grpTreeDecor.Text = "grpTreeDecor";
            grpTreeDecor.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { panMaleColor, panFemaleColor }
                    },
                    new TableRow {
                        Cells = { panUnkSexColor }
                    },
                    new TableRow {
                        Cells = { panUnHusbandColor,  }
                    },
                    new TableRow {
                        Cells = { panUnWifeColor }
                    },
                    new TableRow {
                        Cells = { lblFont }
                    },
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { panDefFont }
                    }
                }
            };

            lblFont.Size = new Size(70, 16);
            lblFont.Text = "lblFont";

            panMaleColor.BackgroundColor = SystemColors.Control;
            //panMaleColor.BorderStyle = BorderStyle.Fixed3D;
            panMaleColor.Content = lblMaleColor;
            panMaleColor.Cursor = Cursors.Pointer;
            panMaleColor.Size = new Size(103, 32);

            lblMaleColor.Size = new Size(99, 28);
            lblMaleColor.Text = "lblMaleColor";
            //lblMaleColor.Click += PanColor_Click;

            //panFemaleColor.BorderStyle = BorderStyle.Fixed3D;
            panFemaleColor.Content = lblFemaleColor;
            panFemaleColor.Cursor = Cursors.Pointer;
            panFemaleColor.Size = new Size(103, 32);

            lblFemaleColor.Size = new Size(99, 28);
            lblFemaleColor.Text = "lblFemaleColor";
            //lblFemaleColor.Click += PanColor_Click;

            //panUnkSexColor.BorderStyle = BorderStyle.Fixed3D;
            panUnkSexColor.Content = lblUnkSexColor;
            panUnkSexColor.Cursor = Cursors.Pointer;
            panUnkSexColor.Size = new Size(215, 32);

            lblUnkSexColor.Size = new Size(211, 28);
            lblUnkSexColor.Text = "label7";
            //lblUnkSexColor.Click += PanColor_Click;

            //panUnHusbandColor.BorderStyle = BorderStyle.Fixed3D;
            panUnHusbandColor.Content = lblUnHusbandColor;
            panUnHusbandColor.Cursor = Cursors.Pointer;
            //panUnHusbandColor.Location = new Point(12, 112);
            panUnHusbandColor.Size = new Size(215, 32);

            lblUnHusbandColor.Size = new Size(211, 28);
            lblUnHusbandColor.Text = "lblUnHusbandColor";
            //lblUnHusbandColor.Click += PanColor_Click;

            //panUnWifeColor.BorderStyle = BorderStyle.Fixed3D;
            panUnWifeColor.Content = lblUnWifeColor;
            panUnWifeColor.Cursor = Cursors.Pointer;
            panUnWifeColor.Size = new Size(215, 30);

            lblUnWifeColor.Size = new Size(211, 26);
            lblUnWifeColor.Text = "lblUnWifeColor";
            //lblUnWifeColor.Click += PanColor_Click;

            //panDefFont.BorderStyle = BorderStyle.Fixed3D;
            panDefFont.Content = lblChartFont;
            panDefFont.Cursor = Cursors.Pointer;
            panDefFont.Size = new Size(215, 32);
            //panDefFont.Click += panDefFont_Click;

            lblChartFont.Size = new Size(211, 28);
            lblChartFont.Text = "lblChartFont";
            //lblChartFont.Click += panDefFont_Click;

            pageAncCircle.BackgroundColor = SystemColors.Control;
            pageAncCircle.Content = ancOptionsControl1;
            pageAncCircle.Size = new Size(713, 429);
            pageAncCircle.Text = "pageAncCircle";

            //ancOptionsControl1.Font = new Font("Tahoma", 8.25F, FontStyle.None);
            ancOptionsControl1.Options = null;
            ancOptionsControl1.Size = new Size(713, 429);

            pageUIView.Content = PageControl2;
            pageUIView.Padding = new Padding(10);
            pageUIView.Size = new Size(741, 479);
            pageUIView.Text = "pageUIView";

            PageControl2.Pages.Add(pageViewCommon);
            PageControl2.Pages.Add(pageViewPersons);
            PageControl2.SelectedIndex = 0;
            PageControl2.Size = new Size(721, 459);

            pageViewCommon.Padding = new Padding(10);
            pageViewCommon.Size = new Size(713, 429);
            pageViewCommon.Text = "pageViewCommon";
            pageViewCommon.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { rgFNPFormat, grpDateFormat }
                    },
                    new TableRow {
                        Cells = { grpAdvancedNames }
                    },
                    new TableRow {
                        Cells = { chkPlacesWithAddress,  }
                    },
                    new TableRow {
                        Cells = { chkHighlightUnparented }
                    },
                    new TableRow {
                        Cells = { chkHighlightUnmarried }
                    },
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { chkShowDatesSigns, chkShowDatesCalendar }
                    }
                }
            };

            grpAdvancedNames.Padding = new Padding(2);
            grpAdvancedNames.Size = new Size(367, 198);
            grpAdvancedNames.Text = "AdvancedNames";
            grpAdvancedNames.Content = new StackLayout {
                Orientation = Orientation.Vertical,
                Items = { chkExtendWomanSurnames, radMaiden_Married, radMarried_Maiden, radMaiden, radMarried }
            };

            radMarried.Size = new Size(93, 21);
            radMarried.Text = "radMarried";

            radMaiden.Size = new Size(91, 21);
            radMaiden.Text = "radMaiden";

            radMarried_Maiden.Size = new Size(143, 21);
            radMarried_Maiden.Text = "radMarried_Maiden";

            radMaiden_Married.Size = new Size(143, 21);
            radMaiden_Married.Text = "radMaiden_Married";

            chkExtendWomanSurnames.Size = new Size(184, 21);
            chkExtendWomanSurnames.Text = "ExtendWomanSurnames";
            chkExtendWomanSurnames.CheckedChanged += chkExtendWomanSurnames_CheckedChanged;

            rgFNPFormat.Padding = new Padding(2);
            rgFNPFormat.Size = new Size(259, 118);
            rgFNPFormat.Text = "rgFNPFormat";
            rgFNPFormat.Content = new StackLayout {
                Orientation = Orientation.Vertical,
                Items = { radS_N_P, radS_NP, radSNP }
            };

            radS_N_P.Size = new Size(224, 29);
            radS_N_P.Text = "radS_N_P";

            radS_NP.Size = new Size(224, 29);
            radS_NP.Text = "radS_NP";

            radSNP.Size = new Size(224, 29);
            radSNP.Text = "radSNP";

            grpDateFormat.Padding = new Padding(2);
            grpDateFormat.Size = new Size(259, 88);
            grpDateFormat.Text = "grpDateFormat";
            grpDateFormat.Content = new StackLayout {
                Orientation = Orientation.Vertical,
                Items = { radDMY, radYMD }
            };

            radYMD.Size = new Size(146, 29);
            radYMD.Text = "YYYY.MM.DD";

            radDMY.Size = new Size(146, 30);
            radDMY.Text = "DD.MM.YYYY";

            chkPlacesWithAddress.Size = new Size(259, 21);
            chkPlacesWithAddress.Text = "chkPlacesWithAddress";

            chkHighlightUnparented.Size = new Size(338, 21);
            chkHighlightUnparented.Text = "chkHighlightUnparented";

            chkShowDatesSigns.Size = new Size(338, 21);
            chkShowDatesSigns.Text = "chkShowDatesSigns";

            chkShowDatesCalendar.Size = new Size(338, 21);
            chkShowDatesCalendar.Text = "chkShowDatesCalendar";

            chkHighlightUnmarried.Size = new Size(338, 21);
            chkHighlightUnmarried.Text = "chkHighlightUnmarried";

            pageViewPersons.Size = new Size(713, 429);
            pageViewPersons.Text = "pageViewPersons";
            pageViewPersons.Content = new StackLayout {
                Orientation = Orientation.Horizontal,
                Items = { lstPersonColumns, panel1 }
            };

            panel1.Padding = new Padding(10);
            panel1.Size = new Size(488, 429);
            panel1.Content = new StackLayout {
                Orientation = Orientation.Vertical,
                Items = { btnColumnUp, btnColumnDown, null, btnDefList }
            };

            lstPersonColumns.Size = new Size(468, 409);
            //lstPersonColumns.ItemCheck += new ItemCheckEventHandler(ListPersonColumns_ItemCheck);

            btnColumnUp.Size = new Size(39, 34);
            btnColumnUp.Click += btnColumnUp_Click;

            btnColumnDown.Size = new Size(39, 34);
            btnColumnDown.Click += btnColumnDown_Click;

            btnDefList.Size = new Size(192, 44);
            btnDefList.Text = "btnDefList";
            btnDefList.Click += btnDefList_Click;

            pagePedigree.Content = grpPedigree;
            pagePedigree.Padding = new Padding(10);
            pagePedigree.Size = new Size(741, 479);
            pagePedigree.Text = "pagePedigree";

            grpPedigree.Padding = new Padding(10);
            grpPedigree.Size = new Size(405, 258);
            grpPedigree.Text = "grpPedigree";
            grpPedigree.Content = new StackLayout {
                Orientation = Orientation.Vertical,
                Items = { chkAttributes, chkNotes, chkSources, chkGenerations, grpPedigreeFormat }
            };

            chkAttributes.Size = new Size(349, 21);
            chkAttributes.Text = "chkAttributes";

            chkNotes.Size = new Size(349, 21);
            chkNotes.Text = "chkNotes";

            chkGenerations.Size = new Size(349, 21);
            chkGenerations.Text = "chkGenerations";

            chkSources.Size = new Size(349, 21);
            chkSources.Text = "chkSources";

            grpPedigreeFormat.Padding = new Padding(10);
            grpPedigreeFormat.Size = new Size(349, 101);
            grpPedigreeFormat.Text = "grpPedigreeFormat";
            grpPedigreeFormat.Content = new StackLayout {
                Orientation = Orientation.Vertical,
                Items = { radExcess, radCompact }
            };

            radExcess.Size = new Size(146, 30);
            radExcess.Text = "radExcess";

            radCompact.Size = new Size(146, 29);
            radCompact.Text = "radCompact";

            pagePlugins.BackgroundColor = SystemColors.Control;
            pagePlugins.Content = lvPlugins;
            pagePlugins.Padding = new Padding(10);
            pagePlugins.Size = new Size(741, 479);
            pagePlugins.Text = "pagePlugins";

            //lvPlugins.FullRowSelect = true;
            //lvPlugins.MultiSelect = false;
            lvPlugins.Size = new Size(721, 459);
            //lvPlugins.UseCompatibleStateImageBehavior = false;
            //lvPlugins.View = View.Details;

            /*columnHeader1.Text = "Title";
            columnHeader1.Width = 75;

            columnHeader2.Text = "Version";

            columnHeader3.Text = "Copyright";
            columnHeader3.Width = 125;

            columnHeader4.Text = "Description";
            columnHeader4.Width = 250;*/

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(114, 30);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(114, 30);
            btnCancel.Text = "btnCancel";

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { PageControl1 }
                    },
                    new TableRow {
                        Cells = { null, btnAccept, btnCancel }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(749, 568);
            Title = "OptionsDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
