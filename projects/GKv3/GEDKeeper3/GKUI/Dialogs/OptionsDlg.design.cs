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
        private Scrollable panMaleColor;
        private Scrollable panFemaleColor;
        private Scrollable panUnkSexColor;
        private Scrollable panUnHusbandColor;
        private Scrollable panUnWifeColor;
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
        private PasswordBox txtProxyPass;
        private TabPage pageUIView;
        private TabControl PageControl2;
        private TabPage pageViewCommon;
        private TabPage pageViewPersons;
        private GKListView lstPersonColumns;
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
        private Scrollable panDefFont;
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
        private GKListView lvPlugins;
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
            txtProxyPass = new PasswordBox();
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
            panMaleColor = new Scrollable();
            lblMaleColor = new Label();
            panFemaleColor = new Scrollable();
            lblFemaleColor = new Label();
            panUnkSexColor = new Scrollable();
            lblUnkSexColor = new Label();
            panUnHusbandColor = new Scrollable();
            lblUnHusbandColor = new Label();
            panUnWifeColor = new Scrollable();
            lblUnWifeColor = new Label();
            panDefFont = new Scrollable();
            lblChartFont = new Label();
            pageAncCircle = new TabPage();
            ancOptionsControl1 = new GKUI.Components.ACOptionsControl();
            pageUIView = new TabPage();
            PageControl2 = new TabControl();
            pageViewCommon = new TabPage();
            grpAdvancedNames = new GroupBox();
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
            lstPersonColumns = new GKListView();
            btnColumnUp = new Button();
            btnColumnDown = new Button();
            btnDefList = new Button();
            pagePedigree = new TabPage();
            grpPedigree = new GroupBox();
            chkAttributes = new CheckBox();
            chkNotes = new CheckBox();
            chkGenerations = new CheckBox();
            chkSources = new CheckBox();
            ColorDialog1 = new ColorDialog();

            SuspendLayout();

            pageCommon.Text = "pageCommon";
            pageCommon.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { grpInternet, groupBox1 }
                    },
                    new TableRow {
                        Cells = { grpOther }
                    },
                    null
                }
            };

            groupBox1.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { grpFileBackup }
                    },
                    new TableRow {
                        Cells = { new StackLayout {
                                Orientation = Orientation.Horizontal,
                                Padding = 10,
                                Spacing = 10,
                                Items = { chkAutosave, numASMin, lblMinutes }
                            } }
                    }
                }
            };

            lblMinutes.Text = "lblMinutes";

            numASMin.MaxValue = 120;
            numASMin.MinValue = 1;
            numASMin.Width = 60;
            numASMin.Value = 1;

            chkAutosave.Text = "chkAutosave";

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

            radFBEachRevision.Text = "radFBEachRevision";

            radFBOnlyPrev.Text = "radFBOnlyPrev";

            radFBNone.Text = "radFBNone";

            //

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

            lblProxyServer.Text = "lblProxyServer";

            lblProxyPort.Text = "lblProxyPort";

            lblProxyLogin.Text = "lblProxyLogin";

            lblProxyPassword.Text = "lblProxyPassword";

            chkUseProxy.Text = "chkUseProxy";

            //txtProxyServer.Size = new Size(192, 24);

            //txtProxyPort.Size = new Size(192, 24);

            //txtProxyLogin.Size = new Size(192, 24);

            txtProxyPass.PasswordChar = '*';
            txtProxyPass.Text = "txtProxyPass";

            //

            chkLoadRecentFiles.Text = "chkLoadRecentFiles";

            chkShowOnStart.Text = "chkShowOnStart";

            lblGeocoder.Text = "lblGeocoder";

            cmbGeocoder.ReadOnly = true;
            cmbGeocoder.Items.Add("Google");
            cmbGeocoder.Items.Add("Yandex");

            lblLanguage.Text = "lblLanguage";

            cmbLanguages.ReadOnly = true;

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

            //

            chkAllowMediaDirectRefs.Text = "chkAllowMediaDirectRefs";

            chkEmbeddedMediaPlayer.Text = "chkEmbeddedMediaPlayer";

            chkRemovableMediaWarning.Text = "chkRemovableMediaWarning";

            pageMultimedia.Text = "pageMultimedia";
            pageMultimedia.Content = new StackLayout {
                Orientation = Orientation.Vertical,
                Padding = new Padding(10),
                Spacing = 10,
                Items = { chkRemovableMediaWarning, chkEmbeddedMediaPlayer, chkAllowMediaDirectRefs }
            };

            //

            pageCharts.Content = tabsCharts;
            pageCharts.Text = "pageCharts";

            tabsCharts.Pages.Add(pageTreeChart);
            tabsCharts.Pages.Add(pageAncCircle);

            //

            pageTreeChart.Text = "pageTreeChart";
            pageTreeChart.Content = new StackLayout {
                Orientation = Orientation.Horizontal,
                Padding = 10,
                Spacing = 10,
                Items = { grpTreePersons, grpTreeDecor }
            };

            grpTreePersons.Text = "grpTreePersons";
            grpTreePersons.Content = new StackLayout {
                Orientation = Orientation.Vertical,
                Padding = 10,
                Spacing = 10,
                Items = { chkSurname, chkName, chkPatronymic, chkDiffLines, chkBirthDate, chkDeathDate, chkOnlyYears,
                    chkKinship, chkSignsVisible, chkTreeDecorative, chkPortraitsVisible, chkDefaultPortraits,
                    chkChildlessExclude }
            };

            chkSurname.Text = "chkSurname";

            chkName.Text = "chkName";

            chkPatronymic.Text = "chkPatronymic";

            chkDiffLines.Text = "chkDiffLines";

            chkBirthDate.Text = "chkBirthDate";

            chkDeathDate.Text = "chkDeathDate";

            chkKinship.Text = "chkKinship";

            chkDefaultPortraits.Text = "chkDefaultPortraits";

            chkOnlyYears.Text = "chkOnlyYears";

            chkSignsVisible.Text = "chkSignsVisible";

            chkChildlessExclude.Text = "chkChildlessExclude";

            chkTreeDecorative.Text = "chkTreeDecorative";

            chkPortraitsVisible.Text = "chkPortraitsVisible";
            chkPortraitsVisible.CheckedChanged += chkPortraitsVisible_CheckedChanged;

            //

            grpTreeDecor.Text = "grpTreeDecor";
            grpTreeDecor.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { new StackLayout { Orientation = Orientation.Horizontal, Spacing = 10, Items = { panMaleColor, panFemaleColor } } }
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
                        ScaleHeight = false,
                        Cells = { panDefFont }
                    },
                    null
                }
            };

            panMaleColor.Border = BorderType.Bezel;
            panMaleColor.Content = lblMaleColor;
            panMaleColor.Cursor = Cursors.Pointer;

            lblMaleColor.Text = "lblMaleColor";
            lblMaleColor.MouseDown += PanColor_Click;

            panFemaleColor.Border = BorderType.Bezel;
            panFemaleColor.Content = lblFemaleColor;
            panFemaleColor.Cursor = Cursors.Pointer;

            lblFemaleColor.Text = "lblFemaleColor";
            lblFemaleColor.MouseDown += PanColor_Click;

            panUnkSexColor.Border = BorderType.Bezel;
            panUnkSexColor.Content = lblUnkSexColor;
            panUnkSexColor.Cursor = Cursors.Pointer;

            lblUnkSexColor.Text = "label7";
            lblUnkSexColor.MouseDown += PanColor_Click;

            panUnHusbandColor.Border = BorderType.Bezel;
            panUnHusbandColor.Content = lblUnHusbandColor;
            panUnHusbandColor.Cursor = Cursors.Pointer;

            lblUnHusbandColor.Text = "lblUnHusbandColor";
            lblUnHusbandColor.MouseDown += PanColor_Click;

            panUnWifeColor.Border = BorderType.Bezel;
            panUnWifeColor.Content = lblUnWifeColor;
            panUnWifeColor.Cursor = Cursors.Pointer;

            lblUnWifeColor.Text = "lblUnWifeColor";
            lblUnWifeColor.MouseDown += PanColor_Click;

            panDefFont.Border = BorderType.Bezel;
            panDefFont.Content = lblChartFont;
            panDefFont.Cursor = Cursors.Pointer;
            panDefFont.MouseDown += panDefFont_Click;

            lblChartFont.Text = "lblChartFont";
            lblChartFont.MouseDown += panDefFont_Click;

            //

            pageAncCircle.Content = ancOptionsControl1;
            pageAncCircle.Text = "pageAncCircle";

            ancOptionsControl1.Options = null;

            //

            pageUIView.Content = PageControl2;
            pageUIView.Text = "pageUIView";

            PageControl2.Pages.Add(pageViewCommon);
            PageControl2.Pages.Add(pageViewPersons);

            pageViewCommon.Text = "pageViewCommon";
            pageViewCommon.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { rgFNPFormat, grpDateFormat }
                    },
                    new TableRow {
                        Cells = { grpAdvancedNames, new StackLayout{
                                Orientation = Orientation.Vertical,
                                Padding = 10,
                                Spacing = 10,
                                Items = { chkPlacesWithAddress, chkHighlightUnparented, chkHighlightUnmarried }
                            }
                        }
                    },
                    null
                }
            };

            //

            radMarried = new RadioButton();
            radMarried.Text = "radMarried";

            radMaiden = new RadioButton(radMarried);
            radMaiden.Text = "radMaiden";

            radMarried_Maiden = new RadioButton(radMarried);
            radMarried_Maiden.Text = "radMarried_Maiden";

            radMaiden_Married = new RadioButton(radMarried);
            radMaiden_Married.Text = "radMaiden_Married";

            chkExtendWomanSurnames.Text = "ExtendWomanSurnames";
            chkExtendWomanSurnames.CheckedChanged += chkExtendWomanSurnames_CheckedChanged;

            grpAdvancedNames.Text = "AdvancedNames";
            grpAdvancedNames.Content = new StackLayout {
                Orientation = Orientation.Vertical,
                Padding = 10,
                Spacing = 10,
                Items = { chkExtendWomanSurnames, radMaiden_Married, radMarried_Maiden, radMaiden, radMarried }
            };

            //

            rgFNPFormat.Text = "rgFNPFormat";
            rgFNPFormat.Content = new StackLayout {
                Orientation = Orientation.Vertical,
                Padding = 10,
                Spacing = 10,
                Items = { radS_N_P, radS_NP, radSNP }
            };

            radS_N_P.Text = "radS_N_P";

            radS_NP.Text = "radS_NP";

            radSNP.Text = "radSNP";

            //

            grpDateFormat.Text = "grpDateFormat";
            grpDateFormat.Content = new StackLayout {
                Orientation = Orientation.Vertical,
                Padding = 10,
                Spacing = 10,
                Items = { radDMY, radYMD, null, chkShowDatesSigns, chkShowDatesCalendar }
            };

            radYMD.Text = "YYYY.MM.DD";

            radDMY.Text = "DD.MM.YYYY";

            chkShowDatesSigns.Text = "chkShowDatesSigns";

            chkShowDatesCalendar.Text = "chkShowDatesCalendar";

            //

            chkPlacesWithAddress.Text = "chkPlacesWithAddress";

            chkHighlightUnparented.Text = "chkHighlightUnparented";

            chkHighlightUnmarried.Text = "chkHighlightUnmarried";

            //

            pageViewPersons.Text = "pageViewPersons";
            pageViewPersons.Content = new StackLayout {
                Orientation = Orientation.Horizontal,
                Padding = 10,
                Spacing = 10,
                Items = { new StackLayoutItem(lstPersonColumns, true), new StackLayoutItem(panel1, false) }
            };

            panel1.Content = new StackLayout {
                Orientation = Orientation.Vertical,
                Padding = 10,
                Spacing = 10,
                Items = { btnColumnUp, btnColumnDown, null, btnDefList }
            };

            //lstPersonColumns.Size = new Size(468, 409);
            //lstPersonColumns.ItemCheck += new ItemCheckEventHandler(ListPersonColumns_ItemCheck);

            btnColumnUp.Size = new Size(26, 26);
            btnColumnUp.Click += btnColumnUp_Click;

            btnColumnDown.Size = new Size(26, 26);
            btnColumnDown.Click += btnColumnDown_Click;

            btnDefList.Size = new Size(192, 26);
            btnDefList.Text = "btnDefList";
            btnDefList.Click += btnDefList_Click;

            //

            radExcess = new RadioButton();
            radExcess.Text = "radExcess";

            radCompact = new RadioButton();
            radCompact.Text = "radCompact";

            grpPedigreeFormat = new GroupBox();
            grpPedigreeFormat.Text = "grpPedigreeFormat";
            grpPedigreeFormat.Content = new StackLayout {
                Orientation = Orientation.Vertical,
                Padding = new Padding(10),
                Spacing = 10,
                Items = { radExcess, radCompact }
            };

            //

            chkAttributes.Text = "chkAttributes";

            chkNotes.Text = "chkNotes";

            chkGenerations.Text = "chkGenerations";

            chkSources.Text = "chkSources";

            grpPedigree.Text = "grpPedigree";
            grpPedigree.Content = new StackLayout {
                Orientation = Orientation.Vertical,
                Padding = new Padding(10),
                Spacing = 10,
                Items = { chkAttributes, chkNotes, chkSources, chkGenerations, grpPedigreeFormat }
            };

            pagePedigree.Content = grpPedigree;
            pagePedigree.Text = "pagePedigree";

            //

            lvPlugins = new GKListView();
            lvPlugins.FullRowSelect = true;
            lvPlugins.AllowMultipleSelection = false;

            pagePlugins = new TabPage();
            pagePlugins.Content = lvPlugins;
            pagePlugins.Text = "pagePlugins";

            //

            PageControl1 = new TabControl();
            PageControl1.Pages.Add(pageCommon);
            PageControl1.Pages.Add(pageMultimedia);
            PageControl1.Pages.Add(pageCharts);
            PageControl1.Pages.Add(pageUIView);
            PageControl1.Pages.Add(pagePedigree);
            PageControl1.Pages.Add(pagePlugins);

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(130, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(130, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += CancelClickHandler;

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { PageControl1 }
                    },
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
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
