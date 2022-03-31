using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
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
        private CheckBox chkAutoCheckUpdates;
        private TabPage pageMultimedia;
        private CheckBox chkEmbeddedMediaPlayer;
        private CheckBox chkLoadRecentFiles;
        private CheckBox chkRemovableMediaWarning;
        private CheckBox chkDefaultPortraits;
        private CheckBox chkInvertedTree;
        private CheckBox chkMarriagesDates;
        private ComboBox cmbGeocoder;
        private Label lblGeocoder;
        private CheckBox chkShowPlaces;
        private CheckBox chkHideUnknownSpouses;
        private GroupBox grpSpacings;
        private Label lblSpouseDist;
        private Label lblGenDist;
        private Label lblBranchDist;
        private Label lblMargins;
        private NumericUpDown numSpouseDist;
        private NumericUpDown numGenDist;
        private NumericUpDown numBranchDist;
        private NumericUpDown numMargins;
        private CheckBox chkAutoSortChildren;
        private CheckBox chkAutoSortSpouses;
        private CheckBox chkCheckTreeSize;
        private CheckBox chkCharsetDetection;
        private Label lblBackupRevisionsMaxCount;
        private NumericUpDown numBackupRevisionsMaxCount;
        private CheckBox chkAllowMediaStoreRelativeReferences;
        private Label lblMediaStoreDefault;
        private ComboBox cmbMediaStoreDefault;
        private CheckBox chkAllowDeleteMediaFileFromStgArc;
        private CheckBox chkAllowDeleteMediaFileFromRefs;
        private CheckBox chkDeleteMediaFileWithoutConfirm;
        private CheckBox chkFirstCapitalLetterInNames;
        private CheckBox chkDialogClosingWarn;
        private Label lblGeoSearchCountry;
        private ComboBox cmbGeoSearchCountry;
        private CheckBox chkSeparateDAPLines;
        private CheckBox chkDottedLinesOfAdoptedChildren;
        private CheckBox chkBoldNames;
        private CheckBox chkOnlyLocality;
        private CheckBox chkMinimizingWidth;
        private CheckBox chkShowAge;

        private CheckBox chkShortKinshipForm;
        private CheckBox chkSurnameFirstInOrder;

        private CheckBox chkSeparateDepth;
        private Label lblDefaultDepth;
        private NumericUpDown numDefaultDepth;
        private Label lblDefaultDepthAncestors;
        private NumericUpDown numDefaultDepthAncestors;
        private Label lblDefaultDepthDescendants;
        private NumericUpDown numDefaultDepthDescendants;

        private void InitializeComponent()
        {
            SuspendLayout();

            lblMinutes = new Label();
            lblMinutes.Text = "lblMinutes";

            numASMin = new NumericUpDown();
            numASMin.MaxValue = 120;
            numASMin.MinValue = 1;
            numASMin.Width = 60;
            numASMin.Value = 1;

            chkAutosave = new CheckBox();
            chkAutosave.Text = "chkAutosave";

            radFBNone = new RadioButton();
            radFBNone.Text = "radFBNone";

            radFBOnlyPrev = new RadioButton(radFBNone);
            radFBOnlyPrev.Text = "radFBOnlyPrev";

            radFBEachRevision = new RadioButton(radFBNone);
            radFBEachRevision.Text = "radFBEachRevision";

            grpFileBackup = new GroupBox();
            grpFileBackup.Text = "grpFileBackup";
            grpFileBackup.Content = new DefTableLayout {
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

            lblBackupRevisionsMaxCount = new Label();
            lblBackupRevisionsMaxCount.Text = "lblBackupRevisionsMaxCount";

            numBackupRevisionsMaxCount = new NumericUpDown();
            numBackupRevisionsMaxCount.MaxValue = 1000;
            numBackupRevisionsMaxCount.MinValue = 0;
            numBackupRevisionsMaxCount.Width = 80;
            numBackupRevisionsMaxCount.Value = 0;

            groupBox1 = new GroupBox();
            groupBox1.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { grpFileBackup }
                    },
                    new TableRow {
                        Cells = {
                            new DefStackLayout(Orientation.Horizontal) {
                                Items = { chkAutosave, numASMin, lblMinutes }
                            }
                        }
                    },
                    new TableRow {
                        Cells = {
                            new DefStackLayout(Orientation.Horizontal) {
                                Items = { lblBackupRevisionsMaxCount, numBackupRevisionsMaxCount }
                            }
                        }
                    }
                }
            };

            //

            chkUseProxy = new CheckBox();
            chkUseProxy.Text = "chkUseProxy";

            lblProxyServer = new Label();
            lblProxyServer.Text = "lblProxyServer";

            lblProxyPort = new Label();
            lblProxyPort.Text = "lblProxyPort";

            lblProxyLogin = new Label();
            lblProxyLogin.Text = "lblProxyLogin";

            lblProxyPassword = new Label();
            lblProxyPassword.Text = "lblProxyPassword";

            txtProxyServer = new TextBox();
            //txtProxyServer.Size = new Size(192, 24);

            txtProxyPort = new TextBox();
            //txtProxyPort.Size = new Size(192, 24);

            txtProxyLogin = new TextBox();
            //txtProxyLogin.Size = new Size(192, 24);

            txtProxyPass = new PasswordBox();
            txtProxyPass.PasswordChar = '*';
            txtProxyPass.Text = "txtProxyPass";

            grpInternet = new GroupBox();
            grpInternet.Text = "grpInternet";
            grpInternet.Content = new DefTableLayout {
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

            //

            chkLoadRecentFiles = new CheckBox();
            chkLoadRecentFiles.Text = "chkLoadRecentFiles";

            chkShowOnStart = new CheckBox();
            chkShowOnStart.Text = "chkShowOnStart";

            chkAutoCheckUpdates = new CheckBox();
            chkAutoCheckUpdates.Text = "chkAutoCheckUpdates";

            chkCharsetDetection = new CheckBox();
            chkCharsetDetection.Text = "chkCharsetDetection";

            lblGeocoder = new Label();
            lblGeocoder.Text = "lblGeocoder";

            cmbGeocoder = new ComboBox();
            cmbGeocoder.ReadOnly = true;
            cmbGeocoder.Items.Add("Google");
            cmbGeocoder.Items.Add("Yandex");
            cmbGeocoder.Items.Add("OSM");

            lblGeoSearchCountry = new Label();
            lblGeoSearchCountry.Text = "lblGeoSearchCountry";

            cmbGeoSearchCountry = new ComboBox();
            cmbGeoSearchCountry.ReadOnly = true;

            lblLanguage = new Label();
            lblLanguage.Text = "lblLanguage";

            cmbLanguages = new ComboBox();
            cmbLanguages.ReadOnly = true;

            chkDialogClosingWarn = new CheckBox();
            chkDialogClosingWarn.Text = "chkDialogClosingWarn";

            grpOther = new GroupBox();
            grpOther.Text = "grpOther";
            grpOther.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { chkLoadRecentFiles }
                    },
                    new TableRow {
                        Cells = { chkShowOnStart }
                    },
                    new TableRow {
                        Cells = { chkAutoCheckUpdates }
                    },
                    new TableRow {
                        Cells = { chkCharsetDetection }
                    },
                    new TableRow {
                        Cells = { chkDialogClosingWarn }
                    },
                    new TableRow {
                        Cells = { lblLanguage, cmbLanguages }
                    },
                    new TableRow {
                        Cells = { lblGeocoder, cmbGeocoder }
                    },
                    new TableRow {
                        Cells = { lblGeoSearchCountry, cmbGeoSearchCountry }
                    }
                }
            };

            pageCommon = new TabPage();
            pageCommon.Text = "pageCommon";
            pageCommon.Content = new DefTableLayout {
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

            //

            chkAllowMediaDirectRefs = new CheckBox();
            chkAllowMediaDirectRefs.Text = "chkAllowMediaDirectRefs";

            chkEmbeddedMediaPlayer = new CheckBox();
            chkEmbeddedMediaPlayer.Text = "chkEmbeddedMediaPlayer";

            chkRemovableMediaWarning = new CheckBox();
            chkRemovableMediaWarning.Text = "chkRemovableMediaWarning";

            chkAllowMediaStoreRelativeReferences = new CheckBox();
            chkAllowMediaStoreRelativeReferences.Text = "chkAllowMediaStoreRelativeReferences";

            lblMediaStoreDefault = new Label();
            lblMediaStoreDefault.Text = "lblMediaStoreDefault";

            cmbMediaStoreDefault = new ComboBox();

            chkAllowDeleteMediaFileFromStgArc = new CheckBox();
            chkAllowDeleteMediaFileFromStgArc.Text = "chkAllowDeleteMediaFileFromStgArc";

            chkAllowDeleteMediaFileFromRefs = new CheckBox();
            chkAllowDeleteMediaFileFromRefs.Text = "chkAllowDeleteMediaFileFromRefs";

            chkDeleteMediaFileWithoutConfirm = new CheckBox();
            chkDeleteMediaFileWithoutConfirm.Text = "chkDeleteMediaFileWithoutConfirm";

            pageMultimedia = new TabPage();
            pageMultimedia.Text = "pageMultimedia";
            pageMultimedia.Content = new DefStackLayout(Orientation.Vertical) {
                Items = {
                    chkRemovableMediaWarning,
                    chkEmbeddedMediaPlayer,
                    chkAllowMediaDirectRefs,
                    chkAllowMediaStoreRelativeReferences,
                    new DefStackLayout(Orientation.Horizontal) {
                                Items = { lblMediaStoreDefault, cmbMediaStoreDefault }
                            },
                    chkAllowDeleteMediaFileFromStgArc,
                    chkAllowDeleteMediaFileFromRefs,
                    chkDeleteMediaFileWithoutConfirm
                }
            };

            //

            chkSurname = new CheckBox();
            chkSurname.Text = "chkSurname";
            chkSurname.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkName = new CheckBox();
            chkName.Text = "chkName";
            chkName.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkPatronymic = new CheckBox();
            chkPatronymic.Text = "chkPatronymic";
            chkPatronymic.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkDiffLines = new CheckBox();
            chkDiffLines.Text = "chkDiffLines";
            chkDiffLines.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkBirthDate = new CheckBox();
            chkBirthDate.Text = "chkBirthDate";
            chkBirthDate.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkDeathDate = new CheckBox();
            chkDeathDate.Text = "chkDeathDate";
            chkDeathDate.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkKinship = new CheckBox();
            chkKinship.Text = "chkKinship";
            chkKinship.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkDefaultPortraits = new CheckBox();
            chkDefaultPortraits.Text = "chkDefaultPortraits";
            chkDefaultPortraits.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkOnlyYears = new CheckBox();
            chkOnlyYears.Text = "chkOnlyYears";
            chkOnlyYears.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkSignsVisible = new CheckBox();
            chkSignsVisible.Text = "chkSignsVisible";
            chkSignsVisible.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkChildlessExclude = new CheckBox();
            chkChildlessExclude.Text = "chkChildlessExclude";
            chkChildlessExclude.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkTreeDecorative = new CheckBox();
            chkTreeDecorative.Text = "chkTreeDecorative";
            chkTreeDecorative.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkPortraitsVisible = new CheckBox();
            chkPortraitsVisible.Text = "chkPortraitsVisible";
            chkPortraitsVisible.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkInvertedTree = new CheckBox();
            chkInvertedTree.Text = "chkInvertedTree";
            chkInvertedTree.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkMarriagesDates = new CheckBox();
            chkMarriagesDates.Text = "chkMarriagesDates";
            chkMarriagesDates.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkShowPlaces = new CheckBox();
            chkShowPlaces.Text = "chkShowPlaces";
            chkShowPlaces.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkHideUnknownSpouses = new CheckBox();
            chkHideUnknownSpouses.Text = "chkHideUnknownSpouses";
            chkHideUnknownSpouses.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkCheckTreeSize = new CheckBox();
            chkCheckTreeSize.Text = "chkCheckTreeSize";
            chkCheckTreeSize.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkSeparateDAPLines = new CheckBox();
            chkSeparateDAPLines.Text = "chkSeparateDAPLines";
            chkSeparateDAPLines.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkDottedLinesOfAdoptedChildren = new CheckBox();
            chkDottedLinesOfAdoptedChildren.Text = "chkDottedLinesOfAdoptedChildren";
            chkDottedLinesOfAdoptedChildren.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkBoldNames = new CheckBox();
            chkBoldNames.Text = "chkBoldNames";
            chkBoldNames.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkOnlyLocality = new CheckBox();
            chkOnlyLocality.Text = "chkOnlyLocality";
            chkOnlyLocality.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkMinimizingWidth = new CheckBox();
            chkMinimizingWidth.Text = "chkMinimizingWidth";
            chkMinimizingWidth.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkShowAge = new CheckBox();
            chkShowAge.Text = "chkShowAge";
            chkShowAge.CheckedChanged += chkTreeChartOption_CheckedChanged;

            grpTreePersons = new GroupBox();
            grpTreePersons.Text = "grpTreePersons";
            grpTreePersons.Content = new DefStackLayout(Orientation.Vertical) {
                Items = { chkSurname, chkName, chkPatronymic, chkDiffLines, chkBirthDate, chkDeathDate, chkOnlyYears, chkShowAge,
                    chkMarriagesDates, chkKinship, chkSignsVisible, chkTreeDecorative, chkPortraitsVisible, chkDefaultPortraits,
                    chkInvertedTree, chkChildlessExclude, chkShowPlaces, chkSeparateDAPLines, chkOnlyLocality, chkHideUnknownSpouses, chkCheckTreeSize,
                    chkDottedLinesOfAdoptedChildren, chkBoldNames, chkMinimizingWidth
                }
            };

            //

            lblMaleColor = new Label();
            lblMaleColor.Size = new Size(100, 26);
            lblMaleColor.Text = "lblMaleColor";
            lblMaleColor.TextAlignment = TextAlignment.Center;
            lblMaleColor.VerticalAlignment = VerticalAlignment.Center;
            lblMaleColor.MouseDown += PanColor_Click;
            lblMaleColor.Cursor = Cursors.Pointer;

            panMaleColor = new Scrollable();
            panMaleColor.Border = BorderType.Bezel;
            panMaleColor.Content = lblMaleColor;

            lblFemaleColor = new Label();
            lblFemaleColor.Size = new Size(100, 26);
            lblFemaleColor.Text = "lblFemaleColor";
            lblFemaleColor.TextAlignment = TextAlignment.Center;
            lblFemaleColor.VerticalAlignment = VerticalAlignment.Center;
            lblFemaleColor.MouseDown += PanColor_Click;
            lblFemaleColor.Cursor = Cursors.Pointer;

            panFemaleColor = new Scrollable();
            panFemaleColor.Border = BorderType.Bezel;
            panFemaleColor.Content = lblFemaleColor;

            lblUnkSexColor = new Label();
            lblUnkSexColor.Size = new Size(210, 26);
            lblUnkSexColor.Text = "label7";
            lblUnkSexColor.TextAlignment = TextAlignment.Center;
            lblUnkSexColor.VerticalAlignment = VerticalAlignment.Center;
            lblUnkSexColor.MouseDown += PanColor_Click;
            lblUnkSexColor.Cursor = Cursors.Pointer;

            panUnkSexColor = new Scrollable();
            panUnkSexColor.Border = BorderType.Bezel;
            panUnkSexColor.Content = lblUnkSexColor;

            lblUnHusbandColor = new Label();
            lblUnHusbandColor.Size = new Size(210, 26);
            lblUnHusbandColor.Text = "lblUnHusbandColor";
            lblUnHusbandColor.TextAlignment = TextAlignment.Center;
            lblUnHusbandColor.VerticalAlignment = VerticalAlignment.Center;
            lblUnHusbandColor.MouseDown += PanColor_Click;
            lblUnHusbandColor.Cursor = Cursors.Pointer;

            panUnHusbandColor = new Scrollable();
            panUnHusbandColor.Border = BorderType.Bezel;
            panUnHusbandColor.Content = lblUnHusbandColor;

            lblUnWifeColor = new Label();
            lblUnWifeColor.Size = new Size(210, 26);
            lblUnWifeColor.Text = "lblUnWifeColor";
            lblUnWifeColor.TextAlignment = TextAlignment.Center;
            lblUnWifeColor.VerticalAlignment = VerticalAlignment.Center;
            lblUnWifeColor.MouseDown += PanColor_Click;
            lblUnWifeColor.Cursor = Cursors.Pointer;

            panUnWifeColor = new Scrollable();
            panUnWifeColor.Border = BorderType.Bezel;
            panUnWifeColor.Content = lblUnWifeColor;

            lblChartFont = new Label();
            lblChartFont.Size = new Size(210, 26);
            lblChartFont.Text = "lblChartFont";
            lblChartFont.TextAlignment = TextAlignment.Center;
            lblChartFont.VerticalAlignment = VerticalAlignment.Center;
            lblChartFont.MouseDown += panDefFont_Click;
            lblChartFont.Cursor = Cursors.Pointer;

            panDefFont = new Scrollable();
            panDefFont.Border = BorderType.Bezel;
            panDefFont.Content = lblChartFont;

            grpTreeDecor = new GroupBox();
            grpTreeDecor.Text = "grpTreeDecor";
            grpTreeDecor.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { TableLayout.HorizontalScaled(10, panMaleColor, panFemaleColor) }
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

            //

            lblMargins = new Label();
            lblMargins.Text = "lblMargins";

            numMargins = new NumericUpDown();
            numMargins.MaxValue = 120;
            numMargins.MinValue = 1;
            numMargins.Width = 60;
            numMargins.Value = 1;

            lblBranchDist = new Label();
            lblBranchDist.Text = "lblBranchDist";

            numBranchDist = new NumericUpDown();
            numBranchDist.MaxValue = 120;
            numBranchDist.MinValue = 1;
            numBranchDist.Width = 60;
            numBranchDist.Value = 1;

            lblGenDist = new Label();
            lblGenDist.Text = "lblGenDist";

            numGenDist = new NumericUpDown();
            numGenDist.MaxValue = 120;
            numGenDist.MinValue = 1;
            numGenDist.Width = 60;
            numGenDist.Value = 1;

            lblSpouseDist = new Label();
            lblSpouseDist.Text = "lblSpouseDist";

            numSpouseDist = new NumericUpDown();
            numSpouseDist.MaxValue = 120;
            numSpouseDist.MinValue = 1;
            numSpouseDist.Width = 60;
            numSpouseDist.Value = 1;

            grpSpacings = new GroupBox();
            grpSpacings.Text = "grpSpacings";
            grpSpacings.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { lblMargins, numMargins }
                    },
                    new TableRow {
                        Cells = { lblBranchDist, numBranchDist }
                    },
                    new TableRow {
                        Cells = { lblGenDist, numGenDist }
                    },
                    new TableRow {
                        Cells = { lblSpouseDist, numSpouseDist }
                    },
                    null
                }
            };

            //

            chkSeparateDepth = new CheckBox();
            chkSeparateDepth.Text = "chkSeparateDepth";
            chkSeparateDepth.CheckedChanged += chkSeparateDepth_CheckedChanged;

            lblDefaultDepth = new Label();
            lblDefaultDepth.Text = "lblDefaultDepth";

            numDefaultDepth = new NumericUpDown();
            numDefaultDepth.MaxValue = 9;
            numDefaultDepth.MinValue = -1;
            numDefaultDepth.Width = 60;
            numDefaultDepth.Value = -1;

            lblDefaultDepthAncestors = new Label();
            lblDefaultDepthAncestors.Text = "lblDefaultDepthAncestors";

            numDefaultDepthAncestors = new NumericUpDown();
            numDefaultDepthAncestors.MaxValue = 9;
            numDefaultDepthAncestors.MinValue = -1;
            numDefaultDepthAncestors.Width = 60;
            numDefaultDepthAncestors.Value = -1;

            lblDefaultDepthDescendants = new Label();
            lblDefaultDepthDescendants.Text = "lblDefaultDepthDescendants";

            numDefaultDepthDescendants = new NumericUpDown();
            numDefaultDepthDescendants.MaxValue = 9;
            numDefaultDepthDescendants.MinValue = -1;
            numDefaultDepthDescendants.Width = 60;
            numDefaultDepthDescendants.Value = -1;

            var depthLayout= new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { chkSeparateDepth, null }
                    },
                    new TableRow {
                        Cells = { lblDefaultDepth, numDefaultDepth }
                    },
                    new TableRow {
                        Cells = { lblDefaultDepthAncestors, numDefaultDepthAncestors }
                    },
                    new TableRow {
                        Cells = { lblDefaultDepthDescendants, numDefaultDepthDescendants }
                    },
                    null
                }
            };

            pageTreeChart = new TabPage();
            pageTreeChart.Text = "pageTreeChart";
            pageTreeChart.Content = new DefStackLayout(Orientation.Horizontal) {
                Items = {
                    grpTreePersons,
                    new DefStackLayout(Orientation.Vertical) {
                        Items = {
                            grpTreeDecor,
                            grpSpacings,
                            depthLayout
                        }
                    }
                }
            };

            //

            ancOptionsControl1 = new GKUI.Components.ACOptionsControl();
            ancOptionsControl1.Options = null;

            pageAncCircle = new TabPage();
            pageAncCircle.Content = ancOptionsControl1;
            pageAncCircle.Text = "pageAncCircle";

            //

            tabsCharts = new TabControl();
            tabsCharts.Pages.Add(pageTreeChart);
            tabsCharts.Pages.Add(pageAncCircle);

            pageCharts = new TabPage();
            pageCharts.Content = tabsCharts;
            pageCharts.Text = "pageCharts";

            //

            lstPersonColumns = new GKListView();
            //lstPersonColumns.Size = new Size(468, 409);
            //lstPersonColumns.ItemCheck += new ItemCheckEventHandler(ListPersonColumns_ItemCheck);

            btnColumnUp = new Button();
            btnColumnUp.Size = new Size(26, 26);
            btnColumnUp.Click += btnColumnUp_Click;

            btnColumnDown = new Button();
            btnColumnDown.Size = new Size(26, 26);
            btnColumnDown.Click += btnColumnDown_Click;

            btnDefList = new Button();
            btnDefList.Size = new Size(190, 26);
            btnDefList.Text = "btnDefList";
            btnDefList.Click += btnDefList_Click;

            panel1 = new Panel();
            panel1.Width = 210;
            panel1.Content = new DefStackLayout(Orientation.Vertical) {
                Items = { btnColumnUp, btnColumnDown, null, btnDefList }
            };

            pageViewPersons = new TabPage();
            pageViewPersons.Text = "pageViewPersons";
            pageViewPersons.Content = TableLayout.Horizontal(10, new TableCell(lstPersonColumns, true), panel1);

            //

            radS_N_P = new RadioButton();
            radS_N_P.Text = "radS_N_P";
            radS_N_P.CheckedChanged += rgFNPFormat_CheckedChanged;

            radS_NP = new RadioButton(radS_N_P);
            radS_NP.Text = "radS_NP";
            radS_NP.CheckedChanged += rgFNPFormat_CheckedChanged;

            radSNP = new RadioButton(radS_N_P);
            radSNP.Text = "radSNP";
            radSNP.CheckedChanged += rgFNPFormat_CheckedChanged;

            chkSurnameFirstInOrder = new CheckBox();
            chkSurnameFirstInOrder.Text = "chkSurnameFirstInOrder";

            rgFNPFormat = new GroupBox();
            rgFNPFormat.Text = "rgFNPFormat";
            rgFNPFormat.Content = new DefStackLayout(Orientation.Vertical)
            {
                Items = { radS_N_P, radS_NP, radSNP, chkSurnameFirstInOrder }
            };

            //

            radDMY = new RadioButton();
            radDMY.Text = "DD.MM.YYYY";

            radYMD = new RadioButton(radDMY);
            radYMD.Text = "YYYY.MM.DD";

            chkShowDatesSigns = new CheckBox();
            chkShowDatesSigns.Text = "chkShowDatesSigns";

            chkShowDatesCalendar = new CheckBox();
            chkShowDatesCalendar.Text = "chkShowDatesCalendar";

            grpDateFormat = new GroupBox();
            grpDateFormat.Text = "grpDateFormat";
            grpDateFormat.Content = new DefStackLayout(Orientation.Vertical)
            {
                Items = { radDMY, radYMD, null, chkShowDatesSigns, chkShowDatesCalendar }
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

            chkExtendWomanSurnames = new CheckBox();
            chkExtendWomanSurnames.Text = "ExtendWomanSurnames";
            chkExtendWomanSurnames.CheckedChanged += chkExtendWomanSurnames_CheckedChanged;

            grpAdvancedNames = new GroupBox();
            grpAdvancedNames.Text = "AdvancedNames";
            grpAdvancedNames.Content = new DefStackLayout(Orientation.Vertical)
            {
                Items = { chkExtendWomanSurnames, radMaiden_Married, radMarried_Maiden, radMaiden, radMarried }
            };

            //

            chkPlacesWithAddress = new CheckBox();
            chkPlacesWithAddress.Text = "chkPlacesWithAddress";

            chkHighlightUnparented = new CheckBox();
            chkHighlightUnparented.Text = "chkHighlightUnparented";

            chkHighlightUnmarried = new CheckBox();
            chkHighlightUnmarried.Text = "chkHighlightUnmarried";

            chkAutoSortChildren = new CheckBox();
            chkAutoSortChildren.Text = "chkAutoSortChildren";

            chkAutoSortSpouses = new CheckBox();
            chkAutoSortSpouses.Text = "chkAutoSortSpouses";

            chkFirstCapitalLetterInNames = new CheckBox();
            chkFirstCapitalLetterInNames.Text = "chkFirstCapitalLetterInNames";

            chkShortKinshipForm = new CheckBox();
            chkShortKinshipForm.Text = "chkShortKinshipForm";

            //

            pageViewCommon = new TabPage();
            pageViewCommon.Text = "pageViewCommon";
            pageViewCommon.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { rgFNPFormat, grpDateFormat }
                    },
                    new TableRow {
                        Cells = { grpAdvancedNames, new DefStackLayout(Orientation.Vertical) {
                                Items = { chkPlacesWithAddress, chkHighlightUnparented, chkHighlightUnmarried,
                                          chkAutoSortChildren, chkAutoSortSpouses, chkFirstCapitalLetterInNames,
                                          chkShortKinshipForm
                                }
                            }
                        }
                    },
                    null
                }
            };

            //

            PageControl2 = new TabControl();
            PageControl2.Pages.Add(pageViewCommon);
            PageControl2.Pages.Add(pageViewPersons);

            pageUIView = new TabPage();
            pageUIView.Content = PageControl2;
            pageUIView.Text = "pageUIView";

            //

            radExcess = new RadioButton();
            radExcess.Text = "radExcess";

            radCompact = new RadioButton(radExcess);
            radCompact.Text = "radCompact";

            grpPedigreeFormat = new GroupBox();
            grpPedigreeFormat.Text = "grpPedigreeFormat";
            grpPedigreeFormat.Content = new DefStackLayout(Orientation.Vertical)
            {
                Items = { radExcess, radCompact }
            };

            //

            chkAttributes = new CheckBox();
            chkAttributes.Text = "chkAttributes";

            chkNotes = new CheckBox();
            chkNotes.Text = "chkNotes";

            chkGenerations = new CheckBox();
            chkGenerations.Text = "chkGenerations";

            chkSources = new CheckBox();
            chkSources.Text = "chkSources";

            grpPedigree = new GroupBox();
            grpPedigree.Text = "grpPedigree";
            grpPedigree.Content = new DefStackLayout(Orientation.Vertical)
            {
                Items = { chkAttributes, chkNotes, chkSources, chkGenerations, grpPedigreeFormat }
            };

            pagePedigree = new TabPage();
            pagePedigree.Content = grpPedigree;
            pagePedigree.Text = "pagePedigree";

            //

            lvPlugins = new GKListView();

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

            Content = new DefTableLayout {
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
            Title = "OptionsDlg";

            SetPredefProperties(740, 560);
            ResumeLayout();
        }
    }
}
