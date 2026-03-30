#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class OptionsDlg
    {
        private TabView PageControl1;
        private TabPage pageCommon;
        private Button btnAccept;
        private Button btnCancel;
        private TabPage pageTreeChart;
        private FrameView panTreePersons;
        private CheckBox chkSurname;
        private CheckBox chkName;
        private CheckBox chkPatronymic;
        private CheckBox chkDiffLines;
        private CheckBox chkBirthDate;
        private CheckBox chkDeathDate;
        private CheckBox chkKinship;
        private FrameView grpTreeDecor;
        private FrameView panMaleColor;
        private FrameView panFemaleColor;
        private FrameView panUnkSexColor;
        private FrameView panUnHusbandColor;
        private FrameView panUnWifeColor;
        private FrameView grpInternet;
        private Label lblProxyServer;
        private Label lblProxyPort;
        private Label lblProxyLogin;
        private Label lblProxyPassword;
        private CheckBox chkUseProxy;
        private TextField txtProxyServer;
        private TextField txtProxyPort;
        private TextField txtProxyLogin;
        private TextField txtProxyPass;
        private TabPage pageUIView;
        private TabView PageControl2;
        private TabPage pageViewCommon;
        private TabPage pageViewPersons;
        private GKListView lstPersonColumns;
        private Button btnColumnUp;
        private Button btnColumnDown;
        private Button btnResetDefaults;
        private FrameView rgFNPFormat;
        private FrameView grpDateFormat;
        private CheckBox chkPlacesWithAddress;
        private FrameView grpOther;
        private CheckBox chkShowOnStart;
        private CheckBox chkHighlightUnparented;
        private CheckBox chkHighlightUnmarried;
        private CheckBox chkOnlyYears;
        private CheckBox chkSignsVisible;
        private CheckBox chkChildlessExclude;
        private TabPage pagePedigree;
        private FrameView grpPedigree;
        private CheckBox chkAttributes;
        private CheckBox chkNotes;
        private CheckBox chkSources;
        private FrameView grpPedigreeFormat;
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
        private TabView tabsCharts;
        private TabPage pageCharts;
        private Label lblMaleColor;
        private Label lblFemaleColor;
        private Label lblUnkSexColor;
        private Label lblUnHusbandColor;
        private Label lblUnWifeColor;
        private PanelView panel1;
        private RadioButton radFBNone;
        private RadioButton radFBOnlyPrev;
        private RadioButton radFBEachRevision;
        private FrameView grpFileBackup;
        private CheckBox chkAutosave;
        private NumericStepper numASMin;
        private Label lblMinutes;
        private FrameView groupBox1;
        private CheckBox chkGenerations;
        private CheckBox chkExtendWomanSurnames;
        private RadioButton radMaiden_Married;
        private RadioButton radMarried_Maiden;
        private RadioButton radMaiden;
        private RadioButton radMarried;
        private FrameView grpAdvancedNames;
        private CheckBox chkAllowMediaDirectRefs;
        private TabPage pageMultimedia;
        private CheckBox chkEmbeddedMediaPlayer;
        private CheckBox chkLoadRecentFiles;
        private CheckBox chkRemovableMediaWarning;
        private CheckBox chkDefaultPortraits;
        private ComboBox cmbGeocoder;
        private Label lblGeocoder;
        private CheckBox chkInvertedTree;
        private CheckBox chkMarriagesDates;
        private CheckBox chkAutoCheckUpdates;
        private CheckBox chkShowPlaces;
        private CheckBox chkHideUnknownSpouses;
        private FrameView grpSpacings;
        private Label lblMargins;
        private NumericStepper numMargins;
        private Label lblSpouseDist;
        private NumericStepper numSpouseDist;
        private Label lblGenDist;
        private NumericStepper numGenDist;
        private Label lblBranchDist;
        private NumericStepper numBranchDist;
        private Label lblPadding;
        private NumericStepper numPadding;
        private CheckBox chkAutoSortSpouses;
        private CheckBox chkAutoSortChildren;
        private CheckBox chkCheckTreeSize;
        private CheckBox chkCharsetDetection;
        private CheckBox chkAllowDeleteMediaFileFromRefs;
        private CheckBox chkAllowDeleteMediaFileFromStgArc;
        private Label lblMediaStoreDefault;
        private CheckBox chkAllowMediaStoreRelativeReferences;
        private ComboBox cmbMediaStoreDefault;
        private CheckBox chkDeleteMediaFileWithoutConfirm;
        private Label lblBackupRevisionsMaxCount;
        private NumericStepper numBackupRevisionsMaxCount;
        private CheckBox chkFirstCapitalLetterInNames;
        private CheckBox chkDialogClosingWarn;
        private CheckBox chkDottedLinesOfAdoptedChildren;
        private CheckBox chkDottedLinesOfDivorcedSpouses;
        private CheckBox chkSeparateDAPLines;
        private CheckBox chkBoldNames;
        private Label lblGeoSearchCountry;
        private ComboBox cmbGeoSearchCountry;
        private NumericStepper numDefaultDepth;
        private NumericStepper numDefaultDepthAncestors;
        private NumericStepper numDefaultDepthDescendants;
        private Label lblDefaultDepth;
        private Label lblDefaultDepthAncestors;
        private Label lblDefaultDepthDescendants;
        private CheckBox chkSeparateDepth;
        private CheckBox chkShortKinshipForm;
        private CheckBox chkSurnameFirstInOrder;
        private CheckBox chkOnlyLocality;
        private CheckBox chkMinimizingWidth;
        private CheckBox chkShowAge;
        private CheckBox chkSurnameInCapitals;
        private Label lblCertaintyAlgorithm;
        private ComboBox cmbCertaintyAlgorithm;
        private CheckBox chkLocalizedCalendarSignatures;
        private TabPage pageSpecials;
        private CheckBox chkUseInlineImagesInSvg;
        private CheckBox chkUseExtendedNotes;
        private CheckBox chkKeepRichNames;
        private Label lblChartWindowsShowMode;
        private ComboBox cmbChartWindowsShowMode;
        private CheckBox chkExtendedTree;
        private CheckBox chkHighlightInaccessibleFiles;
        private CheckBox chkTreeSurnameFirst;
        private CheckBox chkURNotesVisible;
        private CheckBox chkSAFByAllNames;
        private CheckBox chkShortenDateRanges;
        private CheckBox chkKeepInfoPansOverallSize;
        private CheckBox chkFilesOverwriteWarn;
        private CheckBox chkSameCardsWidth;
        private CheckBox chkExtendedKinships;
        private CheckBox chkUseSurnamesInPSF;
        private CheckBox chkUseBirthDatesInPSF;
        private Label lblDescendNumbering;
        private ComboBox cmbDescendNumbering;
        private Label lblAscendNumbering;
        private ComboBox cmbAscendNumbering;
        private CheckBox chkExtendedLocations;
        private CheckBox chkELAbbreviatedNames;
        private CheckBox chkReversePlacesOrder;
        private CheckBox chkShowNumberOfSubstructures;
        private TabPage pageEventTypes;
        private GKSheetList slEventTypes;
        private CheckBox chkFullNameOnOneLine;
        private Label lblMatchPatternMethod;
        private ComboBox cmbMatchPatternMethod;
        private CheckBox chkSourcePages;
        private TabPage pageNavigation;
        private CheckBox chkPortraits;
        private CheckBox chkSimpleSingleSurnames;
        private CheckBox chkSearchPlacesWithoutCoords;
        private TabPage pageGeo;
        private TabView tabsTreeCharts;
        private TabPage pageTreePersons;
        private TabPage pageTreeDesign;
        private CheckBox chkDateDesignations;
        private CheckBox chkMourningEdges;
        private CheckBox chkUseAdditionalDates;
        private CheckBox chkDisplayFullFileName;
        private CheckBox chkDisableNonStdFeatures;
        private CheckBox chkSourceCitations;
        private CheckBox chkEnableStdValidation;
        private TabView tabsCommon;
        private TabPage pageComOther;
        private TabPage pageComBackup;
        private CheckBox chkExtBackupEnabled;
        private Label lblExtBackupFolder;
        private TextField txtExtBackupFolder;
        private Button btnExtBackupFolderChoose;
        private TabPage pageGEDCOM;
        private TabPage pageNames;

        private void InitializeComponent()
        {
            PageControl1 = new TabView();
            pageCommon = new TabPage();
            lblCertaintyAlgorithm = new Label();
            cmbCertaintyAlgorithm = new ComboBox();
            groupBox1 = new FrameView();
            lblBackupRevisionsMaxCount = new Label();
            lblMinutes = new Label();
            numBackupRevisionsMaxCount = new NumericStepper();
            numASMin = new NumericStepper();
            chkAutosave = new CheckBox();
            grpFileBackup = new FrameView();
            radFBEachRevision = new RadioButton();
            radFBOnlyPrev = new RadioButton();
            radFBNone = new RadioButton();
            lblGeoSearchCountry = new Label();
            lblGeocoder = new Label();
            lblLanguage = new Label();
            grpInternet = new FrameView();
            lblProxyServer = new Label();
            lblProxyPort = new Label();
            lblProxyLogin = new Label();
            lblProxyPassword = new Label();
            chkUseProxy = new CheckBox();
            txtProxyServer = new TextField();
            txtProxyPort = new TextField();
            txtProxyLogin = new TextField();
            txtProxyPass = new TextField();
            grpOther = new FrameView();
            chkDialogClosingWarn = new CheckBox();
            chkCharsetDetection = new CheckBox();
            chkAutoCheckUpdates = new CheckBox();
            chkLoadRecentFiles = new CheckBox();
            chkShowOnStart = new CheckBox();
            cmbGeoSearchCountry = new ComboBox();
            cmbGeocoder = new ComboBox();
            cmbLanguages = new ComboBox();
            pageMultimedia = new TabPage();
            cmbMediaStoreDefault = new ComboBox();
            chkDeleteMediaFileWithoutConfirm = new CheckBox();
            chkAllowDeleteMediaFileFromRefs = new CheckBox();
            chkAllowDeleteMediaFileFromStgArc = new CheckBox();
            lblMediaStoreDefault = new Label();
            chkAllowMediaStoreRelativeReferences = new CheckBox();
            chkAllowMediaDirectRefs = new CheckBox();
            chkEmbeddedMediaPlayer = new CheckBox();
            chkRemovableMediaWarning = new CheckBox();
            pageCharts = new TabPage();
            tabsCharts = new TabView();
            pageTreeChart = new TabPage();
            chkSeparateDepth = new CheckBox();
            numDefaultDepth = new NumericStepper();
            lblDefaultDepth = new Label();
            numDefaultDepthAncestors = new NumericStepper();
            lblDefaultDepthAncestors = new Label();
            numDefaultDepthDescendants = new NumericStepper();
            lblDefaultDepthDescendants = new Label();
            grpSpacings = new FrameView();
            numSpouseDist = new NumericStepper();
            numGenDist = new NumericStepper();
            numBranchDist = new NumericStepper();
            numMargins = new NumericStepper();
            lblSpouseDist = new Label();
            lblGenDist = new Label();
            lblBranchDist = new Label();
            lblMargins = new Label();
            lblPadding = new Label();
            numPadding = new NumericStepper();
            panTreePersons = new FrameView();
            chkSurname = new CheckBox();
            chkName = new CheckBox();
            chkPatronymic = new CheckBox();
            chkDiffLines = new CheckBox();
            chkBirthDate = new CheckBox();
            chkMarriagesDates = new CheckBox();
            chkDeathDate = new CheckBox();
            chkKinship = new CheckBox();
            chkDefaultPortraits = new CheckBox();
            chkShowAge = new CheckBox();
            chkOnlyYears = new CheckBox();
            chkSignsVisible = new CheckBox();
            chkOnlyLocality = new CheckBox();
            chkSeparateDAPLines = new CheckBox();
            chkShowPlaces = new CheckBox();
            chkMinimizingWidth = new CheckBox();
            chkBoldNames = new CheckBox();
            chkDottedLinesOfAdoptedChildren = new CheckBox();
            chkDottedLinesOfDivorcedSpouses = new CheckBox();
            chkCheckTreeSize = new CheckBox();
            chkHideUnknownSpouses = new CheckBox();
            chkInvertedTree = new CheckBox();
            chkChildlessExclude = new CheckBox();
            chkTreeDecorative = new CheckBox();
            chkPortraitsVisible = new CheckBox();
            grpTreeDecor = new FrameView();
            panMaleColor = new FrameView();
            lblMaleColor = new Label();
            panFemaleColor = new FrameView();
            lblFemaleColor = new Label();
            panUnkSexColor = new FrameView();
            lblUnkSexColor = new Label();
            panUnHusbandColor = new FrameView();
            lblUnHusbandColor = new Label();
            panUnWifeColor = new FrameView();
            lblUnWifeColor = new Label();
            pageUIView = new TabPage();
            PageControl2 = new TabView();
            pageViewCommon = new TabPage();
            chkSurnameFirstInOrder = new CheckBox();
            grpAdvancedNames = new FrameView();
            radMarried = new RadioButton();
            radMaiden = new RadioButton();
            radMarried_Maiden = new RadioButton();
            radMaiden_Married = new RadioButton();
            chkExtendWomanSurnames = new CheckBox();
            rgFNPFormat = new FrameView();
            radS_N_P = new RadioButton();
            radS_NP = new RadioButton();
            radSNP = new RadioButton();
            grpDateFormat = new FrameView();
            radYMD = new RadioButton();
            radDMY = new RadioButton();
            chkPlacesWithAddress = new CheckBox();
            chkHighlightUnparented = new CheckBox();
            chkLocalizedCalendarSignatures = new CheckBox();
            chkShowDatesSigns = new CheckBox();
            chkShowDatesCalendar = new CheckBox();
            chkSurnameInCapitals = new CheckBox();
            chkFirstCapitalLetterInNames = new CheckBox();
            chkAutoSortSpouses = new CheckBox();
            chkShortKinshipForm = new CheckBox();
            chkAutoSortChildren = new CheckBox();
            chkHighlightUnmarried = new CheckBox();
            pageViewPersons = new TabPage();
            panel1 = new PanelView();
            lstPersonColumns = new GKListView();
            btnColumnUp = new Button();
            btnColumnDown = new Button();
            btnResetDefaults = new Button();
            pagePedigree = new TabPage();
            grpPedigree = new FrameView();
            chkAttributes = new CheckBox();
            chkNotes = new CheckBox();
            chkGenerations = new CheckBox();
            chkSources = new CheckBox();
            grpPedigreeFormat = new FrameView();
            radExcess = new RadioButton();
            radCompact = new RadioButton();
            pagePlugins = new TabPage();
            lvPlugins = new GKListView();
            btnAccept = new Button();
            btnCancel = new Button();
            pageSpecials = new TabPage();
            chkUseInlineImagesInSvg = new CheckBox();
            chkUseExtendedNotes = new CheckBox();
            chkKeepRichNames = new CheckBox();
            lblChartWindowsShowMode = new Label();
            cmbChartWindowsShowMode = new ComboBox();
            chkExtendedTree = new CheckBox();
            chkHighlightInaccessibleFiles = new CheckBox();
            chkTreeSurnameFirst = new CheckBox();
            chkURNotesVisible = new CheckBox();
            chkSAFByAllNames = new CheckBox();
            chkShortenDateRanges = new CheckBox();
            chkKeepInfoPansOverallSize = new CheckBox();
            chkFilesOverwriteWarn = new CheckBox();
            chkSameCardsWidth = new CheckBox();
            chkExtendedKinships = new CheckBox();
            chkUseSurnamesInPSF = new CheckBox();
            chkUseBirthDatesInPSF = new CheckBox();
            lblAscendNumbering = new Label();
            cmbAscendNumbering = new ComboBox();
            lblDescendNumbering = new Label();
            cmbDescendNumbering = new ComboBox();
            chkExtendedLocations = new CheckBox();
            chkELAbbreviatedNames = new CheckBox();
            chkReversePlacesOrder = new CheckBox();
            chkShowNumberOfSubstructures = new CheckBox();
            pageEventTypes = new TabPage();
            slEventTypes = new GKSheetList();
            chkFullNameOnOneLine = new CheckBox();
            lblMatchPatternMethod = new Label();
            cmbMatchPatternMethod = new ComboBox();
            chkSourcePages = new CheckBox();
            pageNavigation = new TabPage();
            chkPortraits = new CheckBox();
            chkSimpleSingleSurnames = new CheckBox();
            chkSearchPlacesWithoutCoords = new CheckBox();
            pageGeo = new TabPage();
            tabsTreeCharts = new TabView();
            pageTreePersons = new TabPage();
            pageTreeDesign = new TabPage();
            chkDateDesignations = new CheckBox();
            chkMourningEdges = new CheckBox();
            chkUseAdditionalDates = new CheckBox();
            chkDisplayFullFileName = new CheckBox();
            chkDisableNonStdFeatures = new CheckBox();
            chkSourceCitations = new CheckBox();
            chkEnableStdValidation = new CheckBox();
            tabsCommon = new TabView();
            pageComOther = new TabPage();
            pageComBackup = new TabPage();
            chkExtBackupEnabled = new CheckBox();
            lblExtBackupFolder = new Label();
            txtExtBackupFolder = new TextField();
            btnExtBackupFolderChoose = new Button();
            pageGEDCOM = new TabPage();
            pageNames = new TabPage();

            //

            lblLanguage.Location = new Point(1, 32);
            lblLanguage.TabIndex = 0;

            cmbLanguages.Location = new Point(14, 32);
            cmbLanguages.Size = new Size(28, 2);
            cmbLanguages.TabIndex = 4;

            lblCertaintyAlgorithm.Location = new Point(1, 34);
            lblCertaintyAlgorithm.TabIndex = 7;

            cmbCertaintyAlgorithm.Location = new Point(36, 34);
            cmbCertaintyAlgorithm.Size = new Size(28, 2);
            cmbCertaintyAlgorithm.TabIndex = 8;

            //

            pageComOther.View.Add(lblCertaintyAlgorithm);
            pageComOther.View.Add(cmbCertaintyAlgorithm);
            pageComOther.View.Add(lblLanguage);
            pageComOther.View.Add(cmbLanguages);
            pageComOther.View.Add(grpOther);
            pageComOther.View.Add(grpInternet);

            //

            chkUseProxy.Location = new Point(2, 1);
            chkUseProxy.TabIndex = 0;

            lblProxyServer.Location = new Point(1, 3);
            lblProxyServer.TabIndex = 0;

            txtProxyServer.Location = new Point(14, 3);
            txtProxyServer.Size = new Size(24, 1);
            txtProxyServer.TabIndex = 1;

            lblProxyPort.Location = new Point(1, 5);
            lblProxyPort.TabIndex = 1;

            txtProxyPort.Location = new Point(14, 5);
            txtProxyPort.Size = new Size(24, 1);
            txtProxyPort.TabIndex = 2;

            lblProxyLogin.Location = new Point(1, 7);
            lblProxyLogin.TabIndex = 2;

            txtProxyLogin.Location = new Point(14, 7);
            txtProxyLogin.Size = new Size(24, 1);
            txtProxyLogin.TabIndex = 3;

            lblProxyPassword.Location = new Point(1, 9);
            lblProxyPassword.TabIndex = 3;

            txtProxyPass.Location = new Point(14, 9);
            txtProxyPass.Secret = true;
            txtProxyPass.Size = new Size(24, 1);
            txtProxyPass.TabIndex = 4;

            grpInternet.Add(lblProxyServer);
            grpInternet.Add(lblProxyPort);
            grpInternet.Add(lblProxyLogin);
            grpInternet.Add(lblProxyPassword);
            grpInternet.Add(chkUseProxy);
            grpInternet.Add(txtProxyServer);
            grpInternet.Add(txtProxyPort);
            grpInternet.Add(txtProxyLogin);
            grpInternet.Add(txtProxyPass);
            grpInternet.Location = new Point(1, 1);
            grpInternet.Size = new Size(40, 13);
            grpInternet.TabIndex = 1;
            grpInternet.TabStop = false;

            grpOther.Add(chkKeepInfoPansOverallSize);
            grpOther.Add(chkFilesOverwriteWarn);
            grpOther.Add(lblChartWindowsShowMode);
            grpOther.Add(cmbChartWindowsShowMode);
            grpOther.Add(chkDisplayFullFileName);
            grpOther.Add(chkDialogClosingWarn);
            grpOther.Add(chkAutoCheckUpdates);
            grpOther.Add(chkLoadRecentFiles);
            grpOther.Add(chkShowOnStart);
            grpOther.Location = new Point(1, 15);
            grpOther.Size = new Size(80, 14);
            grpOther.TabIndex = 2;
            grpOther.TabStop = false;

            chkLoadRecentFiles.Location = new Point(2, 1);
            chkLoadRecentFiles.TabIndex = 9;

            chkShowOnStart.Location = new Point(2, 2);
            chkShowOnStart.TabIndex = 0;

            chkAutoCheckUpdates.Location = new Point(2, 3);
            chkAutoCheckUpdates.TabIndex = 9;

            chkDialogClosingWarn.Location = new Point(2, 4);
            chkDialogClosingWarn.TabIndex = 9;

            chkDisplayFullFileName.Location = new Point(2, 5);
            chkDisplayFullFileName.TabIndex = 9;

            lblChartWindowsShowMode.Location = new Point(2, 7);
            lblChartWindowsShowMode.TabIndex = 12;

            cmbChartWindowsShowMode.Location = new Point(37, 7);
            cmbChartWindowsShowMode.Size = new Size(27, 2);
            cmbChartWindowsShowMode.TabIndex = 12;

            chkKeepInfoPansOverallSize.Location = new Point(2, 9);
            chkKeepInfoPansOverallSize.TabIndex = 15;

            chkFilesOverwriteWarn.Location = new Point(2, 10);
            chkFilesOverwriteWarn.TabIndex = 16;

            //

            radFBNone.Location = new Point(1, 2);
            radFBNone.TabIndex = 0;
            radFBNone.TabStop = true;

            radFBOnlyPrev.Location = new Point(1, 3);
            radFBOnlyPrev.TabIndex = 1;
            radFBOnlyPrev.TabStop = true;

            radFBEachRevision.Location = new Point(1, 4);
            radFBEachRevision.TabIndex = 2;
            radFBEachRevision.TabStop = true;

            grpFileBackup.Add(radFBNone);
            grpFileBackup.Add(radFBOnlyPrev);
            grpFileBackup.Add(radFBEachRevision);
            grpFileBackup.Location = new Point(1, 1);
            grpFileBackup.Size = new Size(42, 7);
            grpFileBackup.TabIndex = 6;
            grpFileBackup.TabStop = false;

            chkAutosave.Location = new Point(1, 9);
            chkAutosave.TabIndex = 7;

            numASMin.Location = new Point(29, 9);
            numASMin.Maximum = 120;
            numASMin.Minimum = 1;
            numASMin.Size = new Size(6, 1);
            numASMin.TabIndex = 8;
            numASMin.Value = 1;

            lblMinutes.Location = new Point(37, 9);
            lblMinutes.TabIndex = 9;

            lblBackupRevisionsMaxCount.Location = new Point(1, 11);
            lblBackupRevisionsMaxCount.TabIndex = 10;

            numBackupRevisionsMaxCount.Location = new Point(41, 11);
            numBackupRevisionsMaxCount.Maximum = 1000;
            numBackupRevisionsMaxCount.Size = new Size(7, 1);
            numBackupRevisionsMaxCount.TabIndex = 8;
            numBackupRevisionsMaxCount.Value = 1;

            chkExtBackupEnabled.Location = new Point(1, 14);
            chkExtBackupEnabled.TabIndex = 3;
            chkExtBackupEnabled.CheckedChanged += chkExtBackupEnabled_CheckedChanged;

            lblExtBackupFolder.Location = new Point(1, 16);
            lblExtBackupFolder.TabIndex = 7;

            txtExtBackupFolder.Location = new Point(1, 17);
            txtExtBackupFolder.ReadOnly = true;
            txtExtBackupFolder.Size = new Size(43, 1);
            txtExtBackupFolder.TabIndex = 8;

            btnExtBackupFolderChoose.Enabled = false;
            btnExtBackupFolderChoose.Location = new Point(45, 17);
            btnExtBackupFolderChoose.Size = new Size(14, 1);
            btnExtBackupFolderChoose.TabIndex = 9;
            btnExtBackupFolderChoose.Clicked += btnExtBackupFolderChoose_Click;

            groupBox1.Add(lblBackupRevisionsMaxCount);
            groupBox1.Add(lblMinutes);
            groupBox1.Add(numBackupRevisionsMaxCount);
            groupBox1.Add(numASMin);
            groupBox1.Add(chkAutosave);
            groupBox1.Add(grpFileBackup);
            groupBox1.Add(btnExtBackupFolderChoose);
            groupBox1.Add(txtExtBackupFolder);
            groupBox1.Add(lblExtBackupFolder);
            groupBox1.Add(chkExtBackupEnabled);
            groupBox1.Location = new Point(1, 1);
            groupBox1.Size = new Size(62, 21);
            groupBox1.TabIndex = 6;
            groupBox1.TabStop = false;

            pageComBackup.View.Add(groupBox1);

            //

            chkCharsetDetection.TabIndex = 1;
            chkDisableNonStdFeatures.TabIndex = 2;
            chkEnableStdValidation.TabIndex = 3;
            chkUseExtendedNotes.TabIndex = 4;
            chkKeepRichNames.TabIndex = 5;

            pageGEDCOM.Text = "GEDCOM";
            pageGEDCOM.View = new StackLayout(Orientation.Vertical, 1, 1, new View[] {
                chkCharsetDetection, chkDisableNonStdFeatures, chkEnableStdValidation, chkUseExtendedNotes, chkKeepRichNames
            });

            //

            tabsCommon.AddTab(pageComOther);
            tabsCommon.AddTab(pageComBackup);
            tabsCommon.AddTab(pageGEDCOM);
            tabsCommon.Location = new Point(0, 0);
            tabsCommon.Height = Dim.Fill();
            tabsCommon.Width = Dim.Fill();
            tabsCommon.TabIndex = 0;

            pageCommon.View.Add(tabsCommon);

            //

            chkRemovableMediaWarning.TabIndex = 1;
            chkEmbeddedMediaPlayer.TabIndex = 2;
            chkAllowMediaDirectRefs.TabIndex = 3;
            chkAllowMediaStoreRelativeReferences.TabIndex = 4;

            lblMediaStoreDefault.TabIndex = 5;
            cmbMediaStoreDefault.Size = new Size(32, 2);
            cmbMediaStoreDefault.TabIndex = 6;

            chkAllowDeleteMediaFileFromStgArc.TabIndex = 7;
            chkAllowDeleteMediaFileFromRefs.TabIndex = 8;
            chkDeleteMediaFileWithoutConfirm.TabIndex = 9;
            chkHighlightInaccessibleFiles.TabIndex = 10;

            pageMultimedia.View = new StackLayout(Orientation.Vertical, 1, 1, new View[] {
                chkRemovableMediaWarning, chkEmbeddedMediaPlayer, chkAllowMediaDirectRefs, chkAllowMediaStoreRelativeReferences,

                new StackLayout(Orientation.Horizontal, 0, 2, new View[] {lblMediaStoreDefault, cmbMediaStoreDefault }),

                chkAllowDeleteMediaFileFromStgArc, chkAllowDeleteMediaFileFromRefs, chkDeleteMediaFileWithoutConfirm, chkHighlightInaccessibleFiles
            });

            //

            chkSeparateDepth.Location = new Point(41, 16);
            chkSeparateDepth.TabIndex = 12;
            chkSeparateDepth.CheckedChanged += chkSeparateDepth_CheckedChanged;

            lblDefaultDepth.Location = new Point(41, 18);
            lblDefaultDepth.TabIndex = 10;

            numDefaultDepth.Location = new Point(76, 18);
            numDefaultDepth.Maximum = 9;
            numDefaultDepth.Size = new Size(6, 1);
            numDefaultDepth.TabIndex = 11;

            lblDefaultDepthAncestors.Location = new Point(41, 20);
            lblDefaultDepthAncestors.TabIndex = 10;

            numDefaultDepthAncestors.Location = new Point(76, 20);
            numDefaultDepthAncestors.Maximum = 9;
            numDefaultDepthAncestors.Size = new Size(6, 1);
            numDefaultDepthAncestors.TabIndex = 11;

            lblDefaultDepthDescendants.Location = new Point(41, 22);
            lblDefaultDepthDescendants.TabIndex = 10;

            numDefaultDepthDescendants.Location = new Point(76, 22);
            numDefaultDepthDescendants.Maximum = 9;
            numDefaultDepthDescendants.Size = new Size(6, 1);
            numDefaultDepthDescendants.TabIndex = 11;

            //

            lblMargins.Location = new Point(2, 1);
            lblMargins.TabIndex = 0;

            numMargins.Location = new Point(28, 1);
            numMargins.Maximum = 120;
            numMargins.Minimum = 1;
            numMargins.Size = new Size(6, 1);
            numMargins.TabIndex = 9;
            numMargins.Value = 1;

            lblBranchDist.Location = new Point(2, 3);
            lblBranchDist.TabIndex = 0;

            numBranchDist.Location = new Point(28, 3);
            numBranchDist.Maximum = 120;
            numBranchDist.Minimum = 1;
            numBranchDist.Size = new Size(6, 1);
            numBranchDist.TabIndex = 9;
            numBranchDist.Value = 1;

            lblGenDist.Location = new Point(2, 5);
            lblGenDist.TabIndex = 0;

            numGenDist.Location = new Point(28, 5);
            numGenDist.Maximum = 120;
            numGenDist.Minimum = 1;
            numGenDist.Size = new Size(6, 1);
            numGenDist.TabIndex = 9;
            numGenDist.Value = 1;

            lblSpouseDist.Location = new Point(2, 7);
            lblSpouseDist.TabIndex = 0;

            numSpouseDist.Location = new Point(28, 7);
            numSpouseDist.Maximum = 120;
            numSpouseDist.Minimum = 1;
            numSpouseDist.Size = new Size(6, 1);
            numSpouseDist.TabIndex = 9;
            numSpouseDist.Value = 1;

            lblPadding.Location = new Point(2, 9);
            lblPadding.TabIndex = 0;

            numPadding.Location = new Point(28, 9);
            numPadding.Maximum = 20;
            numPadding.Minimum = 1;
            numPadding.Size = new Size(6, 1);
            numPadding.TabIndex = 10;
            numPadding.Value = 1;

            grpSpacings.Add(lblSpouseDist);
            grpSpacings.Add(numSpouseDist);
            grpSpacings.Add(lblGenDist);
            grpSpacings.Add(numGenDist);
            grpSpacings.Add(lblBranchDist);
            grpSpacings.Add(numBranchDist);
            grpSpacings.Add(lblMargins);
            grpSpacings.Add(numMargins);
            grpSpacings.Add(lblPadding);
            grpSpacings.Add(numPadding);
            grpSpacings.Location = new Point(39, 1);
            grpSpacings.Size = new Size(37, 12);
            grpSpacings.TabIndex = 2;
            grpSpacings.TabStop = false;

            //

            lblMaleColor.Location = new Point(0, 0);
            lblMaleColor.Size = new Size(12, 1);
            lblMaleColor.TabIndex = 1;
            lblMaleColor.Clicked += PanColor_Click;

            panMaleColor.Add(lblMaleColor);
            panMaleColor.Location = new Point(1, 1);
            panMaleColor.Size = new Size(12, 3);
            panMaleColor.TabIndex = 0;

            lblFemaleColor.Location = new Point(0, 0);
            lblFemaleColor.Size = new Size(12, 1);
            lblFemaleColor.TabIndex = 1;
            lblFemaleColor.Clicked += PanColor_Click;

            panFemaleColor.Add(lblFemaleColor);
            panFemaleColor.Location = new Point(15, 1);
            panFemaleColor.Size = new Size(12, 3);
            panFemaleColor.TabIndex = 1;

            lblUnkSexColor.Location = new Point(0, 0);
            lblUnkSexColor.Size = new Size(26, 1);
            lblUnkSexColor.TabIndex = 1;
            lblUnkSexColor.Clicked += PanColor_Click;

            panUnkSexColor.Add(lblUnkSexColor);
            panUnkSexColor.Location = new Point(1, 4);
            panUnkSexColor.Size = new Size(26, 3);
            panUnkSexColor.TabIndex = 2;

            lblUnHusbandColor.Location = new Point(0, 0);
            lblUnHusbandColor.Size = new Size(26, 1);
            lblUnHusbandColor.TabIndex = 1;
            lblUnHusbandColor.Clicked += PanColor_Click;

            panUnHusbandColor.Add(lblUnHusbandColor);
            panUnHusbandColor.Location = new Point(1, 7);
            panUnHusbandColor.Size = new Size(26, 3);
            panUnHusbandColor.TabIndex = 3;

            lblUnWifeColor.Location = new Point(0, 0);
            lblUnWifeColor.Size = new Size(26, 1);
            lblUnWifeColor.TabIndex = 1;
            lblUnWifeColor.Clicked += PanColor_Click;

            panUnWifeColor.Add(lblUnWifeColor);
            panUnWifeColor.Location = new Point(1, 10);
            panUnWifeColor.Size = new Size(26, 3);
            panUnWifeColor.TabIndex = 4;

            grpTreeDecor.Add(panMaleColor);
            grpTreeDecor.Add(panFemaleColor);
            grpTreeDecor.Add(panUnkSexColor);
            grpTreeDecor.Add(panUnHusbandColor);
            grpTreeDecor.Add(panUnWifeColor);
            grpTreeDecor.Location = new Point(1, 1);
            grpTreeDecor.Size = new Size(30, 16);
            grpTreeDecor.TabIndex = 1;
            grpTreeDecor.TabStop = false;

            //

            pageTreeDesign.View.Add(grpSpacings);
            pageTreeDesign.View.Add(grpTreeDecor);
            pageTreeDesign.View.Add(chkSeparateDepth);
            pageTreeDesign.View.Add(numDefaultDepth);
            pageTreeDesign.View.Add(lblDefaultDepth);
            pageTreeDesign.View.Add(numDefaultDepthAncestors);
            pageTreeDesign.View.Add(lblDefaultDepthAncestors);
            pageTreeDesign.View.Add(numDefaultDepthDescendants);
            pageTreeDesign.View.Add(lblDefaultDepthDescendants);

            //

            chkSurname.Location = new Point(0, 0);
            chkSurname.TabIndex = 0;
            chkSurname.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkTreeSurnameFirst.Location = new Point(4, 1);
            chkTreeSurnameFirst.TabIndex = 1;
            chkTreeSurnameFirst.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkName.Location = new Point(0, 2);
            chkName.TabIndex = 2;
            chkName.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkPatronymic.Location = new Point(0, 3);
            chkPatronymic.TabIndex = 3;
            chkPatronymic.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkDiffLines.Location = new Point(4, 4);
            chkDiffLines.TabIndex = 4;
            chkDiffLines.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkBirthDate.Location = new Point(0, 5);
            chkBirthDate.TabIndex = 5;
            chkBirthDate.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkDeathDate.Location = new Point(0, 6);
            chkDeathDate.TabIndex = 6;
            chkDeathDate.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkOnlyYears.Location = new Point(4, 7);
            chkOnlyYears.TabIndex = 7;
            chkOnlyYears.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkShowAge.Location = new Point(4, 8);
            chkShowAge.TabIndex = 8;
            chkShowAge.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkMarriagesDates.Location = new Point(0, 9);
            chkMarriagesDates.TabIndex = 9;
            chkMarriagesDates.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkKinship.Location = new Point(0, 10);
            chkKinship.TabIndex = 10;
            chkKinship.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkSignsVisible.Location = new Point(0, 11);
            chkSignsVisible.TabIndex = 11;
            chkSignsVisible.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkTreeDecorative.Location = new Point(0, 12);
            chkTreeDecorative.TabIndex = 12;
            chkTreeDecorative.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkPortraitsVisible.Location = new Point(0, 13);
            chkPortraitsVisible.TabIndex = 13;
            chkPortraitsVisible.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkDefaultPortraits.Location = new Point(4, 14);
            chkDefaultPortraits.TabIndex = 14;
            chkDefaultPortraits.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkInvertedTree.Location = new Point(0, 15);
            chkInvertedTree.TabIndex = 15;
            chkInvertedTree.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkChildlessExclude.Location = new Point(0, 16);
            chkChildlessExclude.TabIndex = 16;
            chkChildlessExclude.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkShowPlaces.Location = new Point(0, 17);
            chkShowPlaces.TabIndex = 17;
            chkShowPlaces.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkSeparateDAPLines.Location = new Point(4, 18);
            chkSeparateDAPLines.TabIndex = 18;
            chkSeparateDAPLines.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkOnlyLocality.Location = new Point(4, 19);
            chkOnlyLocality.TabIndex = 19;
            chkOnlyLocality.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkHideUnknownSpouses.Location = new Point(0, 20);
            chkHideUnknownSpouses.TabIndex = 20;
            chkHideUnknownSpouses.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkCheckTreeSize.Location = new Point(0, 21);
            chkCheckTreeSize.TabIndex = 21;
            chkCheckTreeSize.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkDottedLinesOfAdoptedChildren.Location = new Point(0, 22);
            chkDottedLinesOfAdoptedChildren.TabIndex = 22;
            chkDottedLinesOfAdoptedChildren.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkDottedLinesOfDivorcedSpouses.Location = new Point(0, 23);
            chkDottedLinesOfDivorcedSpouses.TabIndex = 23;
            chkDottedLinesOfDivorcedSpouses.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkBoldNames.Location = new Point(0, 24);
            chkBoldNames.TabIndex = 24;
            chkBoldNames.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkMinimizingWidth.Location = new Point(0, 25);
            chkMinimizingWidth.TabIndex = 25;
            chkMinimizingWidth.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkURNotesVisible.Location = new Point(0, 26);
            chkURNotesVisible.TabIndex = 26;
            chkURNotesVisible.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkSameCardsWidth.Location = new Point(0, 27);
            chkSameCardsWidth.TabIndex = 27;
            chkSameCardsWidth.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkFullNameOnOneLine.Location = new Point(0, 28);
            chkFullNameOnOneLine.TabIndex = 28;
            chkFullNameOnOneLine.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkDateDesignations.Location = new Point(0, 29);
            chkDateDesignations.TabIndex = 29;
            chkDateDesignations.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkMourningEdges.Location = new Point(0, 30);
            chkMourningEdges.TabIndex = 30;
            chkMourningEdges.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkUseAdditionalDates.Location = new Point(0, 31);
            chkUseAdditionalDates.TabIndex = 31;
            chkUseAdditionalDates.CheckedChanged += chkTreeChartOption_CheckedChanged;

            chkUseInlineImagesInSvg.Location = new Point(0, 32);
            chkUseInlineImagesInSvg.TabIndex = 12;

            chkExtendedTree.Location = new Point(0, 33);
            chkExtendedTree.TabIndex = 12;

            panTreePersons.Location = new Point(0, 0);
            panTreePersons.Height = Dim.Fill();
            panTreePersons.Width = Dim.Fill();
            panTreePersons.TabIndex = 0;
            panTreePersons.TabStop = false;

            panTreePersons.Add(chkSurname);
            panTreePersons.Add(chkTreeSurnameFirst);
            panTreePersons.Add(chkName);
            panTreePersons.Add(chkPatronymic);
            panTreePersons.Add(chkDiffLines);
            panTreePersons.Add(chkBirthDate);
            panTreePersons.Add(chkDeathDate);
            panTreePersons.Add(chkOnlyYears);
            panTreePersons.Add(chkShowAge);
            panTreePersons.Add(chkMarriagesDates);
            panTreePersons.Add(chkKinship);
            panTreePersons.Add(chkSignsVisible);
            panTreePersons.Add(chkTreeDecorative);
            panTreePersons.Add(chkPortraitsVisible);
            panTreePersons.Add(chkDefaultPortraits);
            panTreePersons.Add(chkInvertedTree);
            panTreePersons.Add(chkChildlessExclude);
            panTreePersons.Add(chkShowPlaces);
            panTreePersons.Add(chkSeparateDAPLines);
            panTreePersons.Add(chkOnlyLocality);
            panTreePersons.Add(chkHideUnknownSpouses);
            panTreePersons.Add(chkCheckTreeSize);
            panTreePersons.Add(chkDottedLinesOfAdoptedChildren);
            panTreePersons.Add(chkDottedLinesOfDivorcedSpouses);
            panTreePersons.Add(chkBoldNames);
            panTreePersons.Add(chkMinimizingWidth);
            panTreePersons.Add(chkURNotesVisible);
            panTreePersons.Add(chkSameCardsWidth);
            panTreePersons.Add(chkFullNameOnOneLine);
            panTreePersons.Add(chkDateDesignations);
            panTreePersons.Add(chkMourningEdges);
            panTreePersons.Add(chkUseAdditionalDates);
            panTreePersons.Add(chkExtendedTree);
            panTreePersons.Add(chkUseInlineImagesInSvg);

            pageTreePersons.View.Add(panTreePersons);

            //

            tabsTreeCharts.AddTab(pageTreePersons);
            tabsTreeCharts.AddTab(pageTreeDesign);
            tabsTreeCharts.Location = new Point(0, 0);
            tabsTreeCharts.Height = Dim.Fill();
            tabsTreeCharts.Width = Dim.Fill();
            tabsTreeCharts.TabIndex = 0;

            pageTreeChart.View.Add(tabsTreeCharts);

            tabsCharts.AddTab(pageTreeChart);
            tabsCharts.Location = new Point(0, 0);
            tabsCharts.Height = Dim.Fill();
            tabsCharts.Width = Dim.Fill();
            tabsCharts.TabIndex = 0;

            pageCharts.View.Add(tabsCharts);

            //

            radSNP.Location = new Point(1, 0);
            radSNP.TabIndex = 0;
            radSNP.CheckedChanged += rgFNPFormat_CheckedChanged;
            radSNP.Group = "snp";

            radS_NP.Location = new Point(1, 1);
            radS_NP.TabIndex = 1;
            radS_NP.CheckedChanged += rgFNPFormat_CheckedChanged;
            radS_NP.Group = "snp";

            radS_N_P.Location = new Point(1, 2);
            radS_N_P.TabIndex = 2;
            radS_N_P.CheckedChanged += rgFNPFormat_CheckedChanged;
            radS_N_P.Group = "snp";

            rgFNPFormat.Add(radSNP);
            rgFNPFormat.Add(radS_NP);
            rgFNPFormat.Add(radS_N_P);
            rgFNPFormat.Location = new Point(1, 1);
            rgFNPFormat.Size = new Size(42, 5);
            rgFNPFormat.TabIndex = 0;
            rgFNPFormat.TabStop = false;

            chkSurnameFirstInOrder.Location = new Point(1, 7);
            chkSurnameFirstInOrder.TabIndex = 1;

            chkFirstCapitalLetterInNames.Location = new Point(1, 9);
            chkFirstCapitalLetterInNames.TabIndex = 2;

            chkSurnameInCapitals.Location = new Point(1, 11);
            chkSurnameInCapitals.TabIndex = 3;

            chkExtendWomanSurnames.Location = new Point(1, 1);
            chkExtendWomanSurnames.TabIndex = 1;
            chkExtendWomanSurnames.CheckedChanged += chkExtendWomanSurnames_CheckedChanged;

            radMaiden_Married.Location = new Point(3, 3);
            radMaiden_Married.TabIndex = 5;
            radMaiden_Married.TabStop = true;
            radMaiden_Married.Group = "an";

            radMarried_Maiden.Location = new Point(3, 4);
            radMarried_Maiden.TabIndex = 4;
            radMarried_Maiden.TabStop = true;
            radMarried_Maiden.Group = "an";

            radMaiden.Location = new Point(3, 5);
            radMaiden.TabIndex = 3;
            radMaiden.TabStop = true;
            radMaiden.Group = "an";

            radMarried.Location = new Point(3, 6);
            radMarried.TabIndex = 2;
            radMarried.TabStop = true;
            radMarried.Group = "an";

            chkSimpleSingleSurnames.Location = new Point(1, 8);
            chkSimpleSingleSurnames.TabIndex = 6;

            grpAdvancedNames.Add(radMarried);
            grpAdvancedNames.Add(radMaiden);
            grpAdvancedNames.Add(radMarried_Maiden);
            grpAdvancedNames.Add(radMaiden_Married);
            grpAdvancedNames.Add(chkExtendWomanSurnames);
            grpAdvancedNames.Add(chkSimpleSingleSurnames);
            grpAdvancedNames.Location = new Point(1, 13);
            grpAdvancedNames.Size = new Size(64, 12);
            grpAdvancedNames.TabIndex = 4;
            grpAdvancedNames.TabStop = false;

            pageNames.View.Add(rgFNPFormat);
            pageNames.View.Add(chkSurnameFirstInOrder);
            pageNames.View.Add(chkFirstCapitalLetterInNames);
            pageNames.View.Add(chkSurnameInCapitals);
            pageNames.View.Add(grpAdvancedNames);

            //

            radDMY.Location = new Point(2, 0);
            radDMY.TabIndex = 0;
            radDMY.Group = "df";

            radYMD.Location = new Point(2, 1);
            radYMD.TabIndex = 1;
            radYMD.Group = "df";

            grpDateFormat.Add(radYMD);
            grpDateFormat.Add(radDMY);
            grpDateFormat.Size = new Size(42, 4);
            grpDateFormat.TabIndex = 0;
            grpDateFormat.TabStop = false;

            chkShowDatesCalendar.TabIndex = 1;
            chkShowDatesSigns.TabIndex = 2;
            chkLocalizedCalendarSignatures.TabIndex = 3;
            chkShortenDateRanges.TabIndex = 4;
            chkPlacesWithAddress.TabIndex = 5;
            chkHighlightUnparented.TabIndex = 6;
            chkHighlightUnmarried.TabIndex = 7;
            chkAutoSortChildren.TabIndex = 8;
            chkAutoSortSpouses.TabIndex = 9;
            chkShowNumberOfSubstructures.TabIndex = 10;

            pageViewCommon.View = new StackLayout(Orientation.Vertical, 1, 1, new View[] {
                grpDateFormat,
                chkShowDatesCalendar, chkShowDatesSigns, chkLocalizedCalendarSignatures, chkShortenDateRanges,
                chkPlacesWithAddress,
                chkHighlightUnparented, chkHighlightUnmarried,
                chkAutoSortChildren, chkAutoSortSpouses,
                chkShowNumberOfSubstructures,
            });

            //

            lstPersonColumns.Location = new Point(0, 0);
            lstPersonColumns.Height = Dim.Fill();
            lstPersonColumns.Width = Dim.Fill();
            lstPersonColumns.TabIndex = 1;

            panel1.Add(lstPersonColumns);
            panel1.Location = new Point(0, 0);
            panel1.Width = 60;
            panel1.Height = Dim.Fill();
            panel1.TabIndex = 1;

            btnColumnUp.Text = "u";
            btnColumnUp.Location = new Point(62, 1);
            btnColumnUp.Size = new Size(5, 1);
            btnColumnUp.TabIndex = 2;
            btnColumnUp.Clicked += btnColumnUp_Click;

            btnColumnDown.Text = "d";
            btnColumnDown.Location = new Point(62, 3);
            btnColumnDown.Size = new Size(5, 1);
            btnColumnDown.TabIndex = 3;
            btnColumnDown.Clicked += btnColumnDown_Click;

            pageViewPersons.View.Add(panel1);
            pageViewPersons.View.Add(btnColumnUp);
            pageViewPersons.View.Add(btnColumnDown);

            //

            chkUseSurnamesInPSF.Location = new Point(1, 1);
            chkUseSurnamesInPSF.TabIndex = 1;

            chkUseBirthDatesInPSF.Location = new Point(1, 3);
            chkUseBirthDatesInPSF.TabIndex = 2;

            lblMatchPatternMethod.Location = new Point(1, 5);
            lblMatchPatternMethod.TabIndex = 3;

            cmbMatchPatternMethod.Location = new Point(30, 5);
            cmbMatchPatternMethod.Size = new Size(32, 2);
            cmbMatchPatternMethod.TabIndex = 4;

            chkSAFByAllNames.Location = new Point(1, 7);
            chkSAFByAllNames.TabIndex = 5;

            pageNavigation.View.Add(chkUseSurnamesInPSF);
            pageNavigation.View.Add(chkUseBirthDatesInPSF);
            pageNavigation.View.Add(lblMatchPatternMethod);
            pageNavigation.View.Add(cmbMatchPatternMethod);
            pageNavigation.View.Add(chkSAFByAllNames);

            //

            pageGeo.View.Add(lblGeoSearchCountry);
            pageGeo.View.Add(lblGeocoder);
            pageGeo.View.Add(cmbGeoSearchCountry);
            pageGeo.View.Add(cmbGeocoder);
            pageGeo.View.Add(chkExtendedLocations);
            pageGeo.View.Add(chkELAbbreviatedNames);
            pageGeo.View.Add(chkReversePlacesOrder);
            pageGeo.View.Add(chkSearchPlacesWithoutCoords);

            lblGeocoder.Location = new Point(1, 1);
            lblGeocoder.TabIndex = 1;

            cmbGeocoder.SetSource(new object[] { "Google", "Yandex", "OSM" });
            cmbGeocoder.Location = new Point(37, 1);
            cmbGeocoder.Size = new Size(28, 2);
            cmbGeocoder.TabIndex = 2;

            lblGeoSearchCountry.Location = new Point(1, 3);
            lblGeoSearchCountry.TabIndex = 3;

            cmbGeoSearchCountry.SetSource(new object[] { "Google", "Yandex", "OSM" });
            cmbGeoSearchCountry.Location = new Point(37, 3);
            cmbGeoSearchCountry.Size = new Size(17, 2);
            cmbGeoSearchCountry.TabIndex = 4;

            chkExtendedLocations.Location = new Point(1, 5);
            chkExtendedLocations.TabIndex = 5;

            chkELAbbreviatedNames.Location = new Point(5, 7);
            chkELAbbreviatedNames.TabIndex = 6;

            chkReversePlacesOrder.Location = new Point(1, 9);
            chkReversePlacesOrder.TabIndex = 7;

            chkSearchPlacesWithoutCoords.Location = new Point(1, 11);
            chkSearchPlacesWithoutCoords.TabIndex = 8;

            //

            PageControl2.AddTab(pageViewCommon);
            PageControl2.AddTab(pageNames);
            PageControl2.AddTab(pageViewPersons);
            PageControl2.AddTab(pageNavigation);
            PageControl2.AddTab(pageGeo);
            PageControl2.Height = Dim.Fill();
            PageControl2.Width = Dim.Fill();
            PageControl2.TabIndex = 0;
            pageUIView.View.Add(PageControl2);

            //

            chkAttributes.Location = new Point(2, 1);
            chkAttributes.TabIndex = 0;

            chkNotes.Location = new Point(2, 2);
            chkNotes.TabIndex = 1;

            chkSources.Location = new Point(2, 3);
            chkSources.TabIndex = 2;

            chkSourcePages.Location = new Point(5, 4);
            chkSourcePages.TabIndex = 2;

            chkSourceCitations.Location = new Point(5, 5);
            chkSourceCitations.TabIndex = 2;

            chkGenerations.Location = new Point(2, 6);
            chkGenerations.TabIndex = 2;

            chkPortraits.Location = new Point(2, 7);
            chkPortraits.TabIndex = 2;

            radExcess.Location = new Point(1, 0);
            radExcess.TabIndex = 1;
            radExcess.Group = "pf";

            radCompact.Location = new Point(1, 1);
            radCompact.TabIndex = 2;
            radCompact.Group = "pf";

            grpPedigreeFormat.Add(radExcess);
            grpPedigreeFormat.Add(radCompact);
            grpPedigreeFormat.Location = new Point(2, 9);
            grpPedigreeFormat.Size = new Size(40, 4);
            grpPedigreeFormat.TabIndex = 3;
            grpPedigreeFormat.TabStop = false;

            lblAscendNumbering.Location = new Point(2, 14);
            lblAscendNumbering.TabIndex = 5;

            cmbAscendNumbering.Location = new Point(28, 14);
            cmbAscendNumbering.Size = new Size(28, 2);
            cmbAscendNumbering.TabIndex = 6;

            lblDescendNumbering.Location = new Point(2, 16);
            lblDescendNumbering.TabIndex = 7;

            cmbDescendNumbering.Location = new Point(28, 16);
            cmbDescendNumbering.Size = new Size(28, 2);
            cmbDescendNumbering.TabIndex = 8;

            grpPedigree.Add(chkAttributes, chkNotes, chkSources, chkSourcePages, chkSourceCitations, chkGenerations, chkPortraits);
            grpPedigree.Add(grpPedigreeFormat, lblAscendNumbering, cmbAscendNumbering, lblDescendNumbering, cmbDescendNumbering);
            grpPedigree.Location = new Point(1, 1);
            grpPedigree.Size = new Size(60, 20);
            grpPedigree.TabIndex = 0;
            grpPedigree.TabStop = false;

            pagePedigree.View.Add(grpPedigree);

            //

            chkShortKinshipForm.TabIndex = 1;
            chkExtendedKinships.TabIndex = 2;

            pageSpecials.View = new StackLayout(Orientation.Vertical, 1, 1, new View[] {
                chkShortKinshipForm, chkExtendedKinships
            });

            //

            slEventTypes.Height = Dim.Fill();
            slEventTypes.Width = Dim.Fill();
            pageEventTypes.View = slEventTypes;

            //

            lvPlugins.Height = Dim.Fill();
            lvPlugins.Width = Dim.Fill();
            pagePlugins.View = lvPlugins;

            //

            PageControl1.AddTab(pageCommon);
            PageControl1.AddTab(pageMultimedia);
            PageControl1.AddTab(pageCharts);
            PageControl1.AddTab(pageUIView);
            PageControl1.AddTab(pagePedigree);
            PageControl1.AddTab(pageSpecials);
            PageControl1.AddTab(pageEventTypes);
            PageControl1.AddTab(pagePlugins);
            PageControl1.Location = new Point(0, 0);
            PageControl1.Size = new Size(91, 49);
            PageControl1.TabIndex = 0;
            Add(PageControl1);

            btnResetDefaults.Size = new Size(26, 1);
            btnResetDefaults.TabIndex = 1;
            btnResetDefaults.Clicked += btnResetDefaults_Click;

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 1;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 2;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnResetDefaults);
            AddButton(btnAccept);
            AddButton(btnCancel);

            Size = new Size(93, 53);
        }
    }
}
