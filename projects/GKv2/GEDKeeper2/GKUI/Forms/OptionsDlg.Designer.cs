namespace GKUI.Forms
{
    partial class OptionsDlg
    {
        private GKUI.Components.GKTabControl PageControl1;
        private System.Windows.Forms.TabPage pageCommon;
        private System.Windows.Forms.Button btnAccept;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.TabPage pageTreeChart;
        private System.Windows.Forms.Panel panTreePersons;
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
        private GKUI.Components.GKTabControl PageControl2;
        private System.Windows.Forms.TabPage pageViewCommon;
        private System.Windows.Forms.TabPage pageViewPersons;
        private GKUI.Components.GKListView lstPersonColumns;
        private System.Windows.Forms.Button btnColumnUp;
        private System.Windows.Forms.Button btnColumnDown;
        private System.Windows.Forms.Button btnResetDefaults;
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
        private GKUI.Components.GKListView lvPlugins;
        private System.Windows.Forms.TabPage pagePlugins;
        private System.Windows.Forms.Label lblChartFont;
        private GKUI.Components.GKTabControl tabsCharts;
        private System.Windows.Forms.TabPage pageCharts;
        private System.Windows.Forms.Label lblMaleColor;
        private System.Windows.Forms.Label lblFemaleColor;
        private System.Windows.Forms.Label lblUnkSexColor;
        private System.Windows.Forms.Label lblUnHusbandColor;
        private System.Windows.Forms.Label lblUnWifeColor;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.RadioButton radFBNone;
        private System.Windows.Forms.RadioButton radFBOnlyPrev;
        private System.Windows.Forms.RadioButton radFBEachRevision;
        private System.Windows.Forms.GroupBox grpFileBackup;
        private System.Windows.Forms.CheckBox chkAutosave;
        private System.Windows.Forms.NumericUpDown numASMin;
        private System.Windows.Forms.Label lblMinutes;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.CheckBox chkGenerations;
        private GKUI.Components.ACOptionsControl ancOptionsControl1;
        private System.Windows.Forms.TabPage pageAncCircle;
        private System.Windows.Forms.CheckBox chkExtendWomanSurnames;
        private System.Windows.Forms.RadioButton radMaiden_Married;
        private System.Windows.Forms.RadioButton radMarried_Maiden;
        private System.Windows.Forms.RadioButton radMaiden;
        private System.Windows.Forms.RadioButton radMarried;
        private System.Windows.Forms.GroupBox grpAdvancedNames;
        private System.Windows.Forms.CheckBox chkAllowMediaDirectRefs;
        private System.Windows.Forms.TabPage pageMultimedia;
        private System.Windows.Forms.CheckBox chkEmbeddedMediaPlayer;
        private System.Windows.Forms.CheckBox chkLoadRecentFiles;
        private System.Windows.Forms.CheckBox chkRemovableMediaWarning;
        private System.Windows.Forms.CheckBox chkDefaultPortraits;
        private System.Windows.Forms.ComboBox cmbGeocoder;
        private System.Windows.Forms.Label lblGeocoder;
        private System.Windows.Forms.CheckBox chkInvertedTree;
        private System.Windows.Forms.CheckBox chkMarriagesDates;
        private System.Windows.Forms.CheckBox chkAutoCheckUpdates;
        private System.Windows.Forms.CheckBox chkShowPlaces;
        private System.Windows.Forms.CheckBox chkHideUnknownSpouses;
        private System.Windows.Forms.GroupBox grpSpacings;
        private System.Windows.Forms.Label lblMargins;
        private System.Windows.Forms.NumericUpDown numMargins;
        private System.Windows.Forms.Label lblSpouseDist;
        private System.Windows.Forms.NumericUpDown numSpouseDist;
        private System.Windows.Forms.Label lblGenDist;
        private System.Windows.Forms.NumericUpDown numGenDist;
        private System.Windows.Forms.Label lblBranchDist;
        private System.Windows.Forms.NumericUpDown numBranchDist;
        private System.Windows.Forms.Label lblPadding;
        private System.Windows.Forms.NumericUpDown numPadding;
        private System.Windows.Forms.CheckBox chkAutoSortSpouses;
        private System.Windows.Forms.CheckBox chkAutoSortChildren;
        private System.Windows.Forms.CheckBox chkCheckTreeSize;
        private System.Windows.Forms.CheckBox chkCharsetDetection;
        private System.Windows.Forms.CheckBox chkAllowDeleteMediaFileFromRefs;
        private System.Windows.Forms.CheckBox chkAllowDeleteMediaFileFromStgArc;
        private System.Windows.Forms.Label lblMediaStoreDefault;
        private System.Windows.Forms.CheckBox chkAllowMediaStoreRelativeReferences;
        private System.Windows.Forms.ComboBox cmbMediaStoreDefault;
        private System.Windows.Forms.CheckBox chkDeleteMediaFileWithoutConfirm;
        private System.Windows.Forms.Label lblBackupRevisionsMaxCount;
        private System.Windows.Forms.NumericUpDown numBackupRevisionsMaxCount;
        private System.Windows.Forms.CheckBox chkFirstCapitalLetterInNames;
        private System.Windows.Forms.CheckBox chkDialogClosingWarn;
        private System.Windows.Forms.CheckBox chkDottedLinesOfAdoptedChildren;
        private System.Windows.Forms.CheckBox chkDottedLinesOfDivorcedSpouses;
        private System.Windows.Forms.CheckBox chkSeparateDAPLines;
        private System.Windows.Forms.CheckBox chkBoldNames;
        private System.Windows.Forms.Label lblGeoSearchCountry;
        private System.Windows.Forms.ComboBox cmbGeoSearchCountry;
        private System.Windows.Forms.NumericUpDown numDefaultDepth;
        private System.Windows.Forms.NumericUpDown numDefaultDepthAncestors;
        private System.Windows.Forms.NumericUpDown numDefaultDepthDescendants;
        private System.Windows.Forms.Label lblDefaultDepth;
        private System.Windows.Forms.Label lblDefaultDepthAncestors;
        private System.Windows.Forms.Label lblDefaultDepthDescendants;
        private System.Windows.Forms.CheckBox chkSeparateDepth;
        private System.Windows.Forms.CheckBox chkShortKinshipForm;
        private System.Windows.Forms.CheckBox chkSurnameFirstInOrder;
        private System.Windows.Forms.CheckBox chkOnlyLocality;
        private System.Windows.Forms.CheckBox chkMinimizingWidth;
        private System.Windows.Forms.CheckBox chkShowAge;
        private System.Windows.Forms.CheckBox chkSurnameInCapitals;
        private System.Windows.Forms.Label lblCertaintyAlgorithm;
        private System.Windows.Forms.ComboBox cmbCertaintyAlgorithm;
        private System.Windows.Forms.CheckBox chkLocalizedCalendarSignatures;
        private System.Windows.Forms.CheckBox chkUseExtraControls;
        private System.Windows.Forms.TabPage pageSpecials;
        private System.Windows.Forms.CheckBox chkUseInlineImagesInSvg;
        private System.Windows.Forms.CheckBox chkUseExtendedNotes;
        private System.Windows.Forms.CheckBox chkKeepRichNames;
        private System.Windows.Forms.Label lblChartWindowsShowMode;
        private System.Windows.Forms.ComboBox cmbChartWindowsShowMode;
        private System.Windows.Forms.CheckBox chkExtendedTree;
        private System.Windows.Forms.CheckBox chkHighlightInaccessibleFiles;
        private System.Windows.Forms.CheckBox chkTreeSurnameFirst;
        private System.Windows.Forms.CheckBox chkURNotesVisible;
        private System.Windows.Forms.CheckBox chkSAFByAllNames;
        private System.Windows.Forms.CheckBox chkShortenDateRanges;
        private System.Windows.Forms.CheckBox chkKeepInfoPansOverallSize;
        private System.Windows.Forms.CheckBox chkFilesOverwriteWarn;
        private System.Windows.Forms.CheckBox chkSameCardsWidth;
        private System.Windows.Forms.CheckBox chkExtendedKinships;
        private System.Windows.Forms.CheckBox chkUseSurnamesInPSF;
        private System.Windows.Forms.CheckBox chkUseBirthDatesInPSF;
        private System.Windows.Forms.Label lblDescendNumbering;
        private System.Windows.Forms.ComboBox cmbDescendNumbering;
        private System.Windows.Forms.Label lblAscendNumbering;
        private System.Windows.Forms.ComboBox cmbAscendNumbering;
        private System.Windows.Forms.CheckBox chkExtendedLocations;
        private System.Windows.Forms.CheckBox chkELAbbreviatedNames;
        private System.Windows.Forms.CheckBox chkReversePlacesOrder;
        private System.Windows.Forms.CheckBox chkShowNumberOfSubstructures;
        private System.Windows.Forms.TabPage pageEventTypes;
        private GKUI.Components.GKSheetList slEventTypes;
        private System.Windows.Forms.CheckBox chkFullNameOnOneLine;
        private System.Windows.Forms.Label lblMatchPatternMethod;
        private System.Windows.Forms.ComboBox cmbMatchPatternMethod;
        private System.Windows.Forms.CheckBox chkSourcePages;
        private System.Windows.Forms.TabPage pageNavigation;
        private System.Windows.Forms.CheckBox chkPortraits;
        private System.Windows.Forms.CheckBox chkSimpleSingleSurnames;
        private System.Windows.Forms.CheckBox chkSearchPlacesWithoutCoords;
        private System.Windows.Forms.TabPage pageGeo;
        private GKUI.Components.GKTabControl tabsTreeCharts;
        private System.Windows.Forms.TabPage pageTreePersons;
        private System.Windows.Forms.TabPage pageTreeDesign;
        private System.Windows.Forms.Label lblTextEffect;
        private System.Windows.Forms.ComboBox cmbTextEffect;
        private System.Windows.Forms.CheckBox chkDateDesignations;
        private System.Windows.Forms.CheckBox chkMourningEdges;
        private System.Windows.Forms.CheckBox chkUseAdditionalDates;
        private System.Windows.Forms.CheckBox chkDisplayFullFileName;

        private void InitializeComponent()
        {
            this.PageControl1 = new GKUI.Components.GKTabControl();
            this.pageCommon = new System.Windows.Forms.TabPage();
            this.lblCertaintyAlgorithm = new System.Windows.Forms.Label();
            this.cmbCertaintyAlgorithm = new System.Windows.Forms.ComboBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.lblBackupRevisionsMaxCount = new System.Windows.Forms.Label();
            this.lblMinutes = new System.Windows.Forms.Label();
            this.numBackupRevisionsMaxCount = new System.Windows.Forms.NumericUpDown();
            this.numASMin = new System.Windows.Forms.NumericUpDown();
            this.chkAutosave = new System.Windows.Forms.CheckBox();
            this.grpFileBackup = new System.Windows.Forms.GroupBox();
            this.radFBEachRevision = new System.Windows.Forms.RadioButton();
            this.radFBOnlyPrev = new System.Windows.Forms.RadioButton();
            this.radFBNone = new System.Windows.Forms.RadioButton();
            this.lblGeoSearchCountry = new System.Windows.Forms.Label();
            this.lblGeocoder = new System.Windows.Forms.Label();
            this.lblLanguage = new System.Windows.Forms.Label();
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
            this.chkDialogClosingWarn = new System.Windows.Forms.CheckBox();
            this.chkCharsetDetection = new System.Windows.Forms.CheckBox();
            this.chkAutoCheckUpdates = new System.Windows.Forms.CheckBox();
            this.chkLoadRecentFiles = new System.Windows.Forms.CheckBox();
            this.chkShowOnStart = new System.Windows.Forms.CheckBox();
            this.cmbGeoSearchCountry = new System.Windows.Forms.ComboBox();
            this.cmbGeocoder = new System.Windows.Forms.ComboBox();
            this.cmbLanguages = new System.Windows.Forms.ComboBox();
            this.pageMultimedia = new System.Windows.Forms.TabPage();
            this.cmbMediaStoreDefault = new System.Windows.Forms.ComboBox();
            this.chkDeleteMediaFileWithoutConfirm = new System.Windows.Forms.CheckBox();
            this.chkAllowDeleteMediaFileFromRefs = new System.Windows.Forms.CheckBox();
            this.chkAllowDeleteMediaFileFromStgArc = new System.Windows.Forms.CheckBox();
            this.lblMediaStoreDefault = new System.Windows.Forms.Label();
            this.chkAllowMediaStoreRelativeReferences = new System.Windows.Forms.CheckBox();
            this.chkAllowMediaDirectRefs = new System.Windows.Forms.CheckBox();
            this.chkEmbeddedMediaPlayer = new System.Windows.Forms.CheckBox();
            this.chkRemovableMediaWarning = new System.Windows.Forms.CheckBox();
            this.pageCharts = new System.Windows.Forms.TabPage();
            this.tabsCharts = new GKUI.Components.GKTabControl();
            this.pageTreeChart = new System.Windows.Forms.TabPage();
            this.chkSeparateDepth = new System.Windows.Forms.CheckBox();
            this.numDefaultDepth = new System.Windows.Forms.NumericUpDown();
            this.lblDefaultDepth = new System.Windows.Forms.Label();
            this.numDefaultDepthAncestors = new System.Windows.Forms.NumericUpDown();
            this.lblDefaultDepthAncestors = new System.Windows.Forms.Label();
            this.numDefaultDepthDescendants = new System.Windows.Forms.NumericUpDown();
            this.lblDefaultDepthDescendants = new System.Windows.Forms.Label();
            this.grpSpacings = new System.Windows.Forms.GroupBox();
            this.numSpouseDist = new System.Windows.Forms.NumericUpDown();
            this.numGenDist = new System.Windows.Forms.NumericUpDown();
            this.numBranchDist = new System.Windows.Forms.NumericUpDown();
            this.numMargins = new System.Windows.Forms.NumericUpDown();
            this.lblSpouseDist = new System.Windows.Forms.Label();
            this.lblGenDist = new System.Windows.Forms.Label();
            this.lblBranchDist = new System.Windows.Forms.Label();
            this.lblMargins = new System.Windows.Forms.Label();
            this.lblPadding = new System.Windows.Forms.Label();
            this.numPadding = new System.Windows.Forms.NumericUpDown();
            this.panTreePersons = new System.Windows.Forms.Panel();
            this.chkSurname = new System.Windows.Forms.CheckBox();
            this.chkName = new System.Windows.Forms.CheckBox();
            this.chkPatronymic = new System.Windows.Forms.CheckBox();
            this.chkDiffLines = new System.Windows.Forms.CheckBox();
            this.chkBirthDate = new System.Windows.Forms.CheckBox();
            this.chkMarriagesDates = new System.Windows.Forms.CheckBox();
            this.chkDeathDate = new System.Windows.Forms.CheckBox();
            this.chkKinship = new System.Windows.Forms.CheckBox();
            this.chkDefaultPortraits = new System.Windows.Forms.CheckBox();
            this.chkShowAge = new System.Windows.Forms.CheckBox();
            this.chkOnlyYears = new System.Windows.Forms.CheckBox();
            this.chkSignsVisible = new System.Windows.Forms.CheckBox();
            this.chkOnlyLocality = new System.Windows.Forms.CheckBox();
            this.chkSeparateDAPLines = new System.Windows.Forms.CheckBox();
            this.chkShowPlaces = new System.Windows.Forms.CheckBox();
            this.chkMinimizingWidth = new System.Windows.Forms.CheckBox();
            this.chkBoldNames = new System.Windows.Forms.CheckBox();
            this.chkDottedLinesOfAdoptedChildren = new System.Windows.Forms.CheckBox();
            this.chkDottedLinesOfDivorcedSpouses = new System.Windows.Forms.CheckBox();
            this.chkCheckTreeSize = new System.Windows.Forms.CheckBox();
            this.chkHideUnknownSpouses = new System.Windows.Forms.CheckBox();
            this.chkInvertedTree = new System.Windows.Forms.CheckBox();
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
            this.pageAncCircle = new System.Windows.Forms.TabPage();
            this.ancOptionsControl1 = new GKUI.Components.ACOptionsControl();
            this.pageUIView = new System.Windows.Forms.TabPage();
            this.PageControl2 = new GKUI.Components.GKTabControl();
            this.pageViewCommon = new System.Windows.Forms.TabPage();
            this.chkSurnameFirstInOrder = new System.Windows.Forms.CheckBox();
            this.grpAdvancedNames = new System.Windows.Forms.GroupBox();
            this.radMarried = new System.Windows.Forms.RadioButton();
            this.radMaiden = new System.Windows.Forms.RadioButton();
            this.radMarried_Maiden = new System.Windows.Forms.RadioButton();
            this.radMaiden_Married = new System.Windows.Forms.RadioButton();
            this.chkExtendWomanSurnames = new System.Windows.Forms.CheckBox();
            this.rgFNPFormat = new System.Windows.Forms.GroupBox();
            this.radS_N_P = new System.Windows.Forms.RadioButton();
            this.radS_NP = new System.Windows.Forms.RadioButton();
            this.radSNP = new System.Windows.Forms.RadioButton();
            this.grpDateFormat = new System.Windows.Forms.GroupBox();
            this.radYMD = new System.Windows.Forms.RadioButton();
            this.radDMY = new System.Windows.Forms.RadioButton();
            this.chkPlacesWithAddress = new System.Windows.Forms.CheckBox();
            this.chkHighlightUnparented = new System.Windows.Forms.CheckBox();
            this.chkLocalizedCalendarSignatures = new System.Windows.Forms.CheckBox();
            this.chkShowDatesSigns = new System.Windows.Forms.CheckBox();
            this.chkShowDatesCalendar = new System.Windows.Forms.CheckBox();
            this.chkSurnameInCapitals = new System.Windows.Forms.CheckBox();
            this.chkFirstCapitalLetterInNames = new System.Windows.Forms.CheckBox();
            this.chkAutoSortSpouses = new System.Windows.Forms.CheckBox();
            this.chkShortKinshipForm = new System.Windows.Forms.CheckBox();
            this.chkAutoSortChildren = new System.Windows.Forms.CheckBox();
            this.chkHighlightUnmarried = new System.Windows.Forms.CheckBox();
            this.pageViewPersons = new System.Windows.Forms.TabPage();
            this.panel1 = new System.Windows.Forms.Panel();
            this.lstPersonColumns = new GKUI.Components.GKListView();
            this.btnColumnUp = new System.Windows.Forms.Button();
            this.btnColumnDown = new System.Windows.Forms.Button();
            this.btnResetDefaults = new System.Windows.Forms.Button();
            this.pagePedigree = new System.Windows.Forms.TabPage();
            this.grpPedigree = new System.Windows.Forms.GroupBox();
            this.chkAttributes = new System.Windows.Forms.CheckBox();
            this.chkNotes = new System.Windows.Forms.CheckBox();
            this.chkGenerations = new System.Windows.Forms.CheckBox();
            this.chkSources = new System.Windows.Forms.CheckBox();
            this.grpPedigreeFormat = new System.Windows.Forms.GroupBox();
            this.radExcess = new System.Windows.Forms.RadioButton();
            this.radCompact = new System.Windows.Forms.RadioButton();
            this.pagePlugins = new System.Windows.Forms.TabPage();
            this.lvPlugins = new GKUI.Components.GKListView();
            this.columnHeader1 = new System.Windows.Forms.ColumnHeader();
            this.columnHeader2 = new System.Windows.Forms.ColumnHeader();
            this.columnHeader3 = new System.Windows.Forms.ColumnHeader();
            this.columnHeader4 = new System.Windows.Forms.ColumnHeader();
            this.btnAccept = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.chkUseExtraControls = new System.Windows.Forms.CheckBox();
            this.pageSpecials = new System.Windows.Forms.TabPage();
            this.chkUseInlineImagesInSvg = new System.Windows.Forms.CheckBox();
            this.chkUseExtendedNotes = new System.Windows.Forms.CheckBox();
            this.chkKeepRichNames = new System.Windows.Forms.CheckBox();
            this.lblChartWindowsShowMode = new System.Windows.Forms.Label();
            this.cmbChartWindowsShowMode = new System.Windows.Forms.ComboBox();
            this.chkExtendedTree = new System.Windows.Forms.CheckBox();
            this.chkHighlightInaccessibleFiles = new System.Windows.Forms.CheckBox();
            this.chkTreeSurnameFirst = new System.Windows.Forms.CheckBox();
            this.chkURNotesVisible = new System.Windows.Forms.CheckBox();
            this.chkSAFByAllNames = new System.Windows.Forms.CheckBox();
            this.chkShortenDateRanges = new System.Windows.Forms.CheckBox();
            this.chkKeepInfoPansOverallSize = new System.Windows.Forms.CheckBox();
            this.chkFilesOverwriteWarn = new System.Windows.Forms.CheckBox();
            this.chkSameCardsWidth = new System.Windows.Forms.CheckBox();
            this.chkExtendedKinships = new System.Windows.Forms.CheckBox();
            this.chkUseSurnamesInPSF = new System.Windows.Forms.CheckBox();
            this.chkUseBirthDatesInPSF = new System.Windows.Forms.CheckBox();
            this.lblAscendNumbering = new System.Windows.Forms.Label();
            this.cmbAscendNumbering = new System.Windows.Forms.ComboBox();
            this.lblDescendNumbering = new System.Windows.Forms.Label();
            this.cmbDescendNumbering = new System.Windows.Forms.ComboBox();
            this.chkExtendedLocations = new System.Windows.Forms.CheckBox();
            this.chkELAbbreviatedNames = new System.Windows.Forms.CheckBox();
            this.chkReversePlacesOrder = new System.Windows.Forms.CheckBox();
            this.chkShowNumberOfSubstructures = new System.Windows.Forms.CheckBox();
            this.pageEventTypes = new System.Windows.Forms.TabPage();
            this.slEventTypes = new GKUI.Components.GKSheetList();
            this.chkFullNameOnOneLine = new System.Windows.Forms.CheckBox();
            this.lblMatchPatternMethod = new System.Windows.Forms.Label();
            this.cmbMatchPatternMethod = new System.Windows.Forms.ComboBox();
            this.chkSourcePages = new System.Windows.Forms.CheckBox();
            this.pageNavigation = new System.Windows.Forms.TabPage();
            this.chkPortraits = new System.Windows.Forms.CheckBox();
            this.chkSimpleSingleSurnames = new System.Windows.Forms.CheckBox();
            this.chkSearchPlacesWithoutCoords = new System.Windows.Forms.CheckBox();
            this.pageGeo = new System.Windows.Forms.TabPage();
            this.tabsTreeCharts = new Components.GKTabControl();
            this.pageTreePersons = new System.Windows.Forms.TabPage();
            this.pageTreeDesign = new System.Windows.Forms.TabPage();
            this.lblTextEffect = new System.Windows.Forms.Label();
            this.cmbTextEffect = new System.Windows.Forms.ComboBox();
            this.chkDateDesignations = new System.Windows.Forms.CheckBox();
            this.chkMourningEdges = new System.Windows.Forms.CheckBox();
            this.chkUseAdditionalDates = new System.Windows.Forms.CheckBox();
            this.chkDisplayFullFileName = new System.Windows.Forms.CheckBox();
            this.PageControl1.SuspendLayout();
            this.pageCommon.SuspendLayout();
            this.groupBox1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numBackupRevisionsMaxCount)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numASMin)).BeginInit();
            this.grpFileBackup.SuspendLayout();
            this.grpInternet.SuspendLayout();
            this.grpOther.SuspendLayout();
            this.pageMultimedia.SuspendLayout();
            this.pageCharts.SuspendLayout();
            this.tabsCharts.SuspendLayout();
            this.pageTreeChart.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numDefaultDepth)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numDefaultDepthAncestors)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numDefaultDepthDescendants)).BeginInit();
            this.grpSpacings.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numSpouseDist)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numGenDist)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numBranchDist)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.numMargins)).BeginInit();
            this.panTreePersons.SuspendLayout();
            this.grpTreeDecor.SuspendLayout();
            this.panMaleColor.SuspendLayout();
            this.panFemaleColor.SuspendLayout();
            this.panUnkSexColor.SuspendLayout();
            this.panUnHusbandColor.SuspendLayout();
            this.panUnWifeColor.SuspendLayout();
            this.panDefFont.SuspendLayout();
            this.pageAncCircle.SuspendLayout();
            this.pageUIView.SuspendLayout();
            this.PageControl2.SuspendLayout();
            this.pageViewCommon.SuspendLayout();
            this.grpAdvancedNames.SuspendLayout();
            this.rgFNPFormat.SuspendLayout();
            this.grpDateFormat.SuspendLayout();
            this.pageViewPersons.SuspendLayout();
            this.panel1.SuspendLayout();
            this.pagePedigree.SuspendLayout();
            this.grpPedigree.SuspendLayout();
            this.grpPedigreeFormat.SuspendLayout();
            this.pagePlugins.SuspendLayout();
            this.pageSpecials.SuspendLayout();
            this.pageEventTypes.SuspendLayout();
            this.pageNavigation.SuspendLayout();
            this.pageGeo.SuspendLayout();
            this.tabsTreeCharts.SuspendLayout();
            this.pageTreePersons.SuspendLayout();
            this.pageTreeDesign.SuspendLayout();
            this.SuspendLayout();
            // 
            // PageControl1
            // 
            this.PageControl1.Controls.Add(this.pageCommon);
            this.PageControl1.Controls.Add(this.pageMultimedia);
            this.PageControl1.Controls.Add(this.pageCharts);
            this.PageControl1.Controls.Add(this.pageUIView);
            this.PageControl1.Controls.Add(this.pagePedigree);
            this.PageControl1.Controls.Add(this.pageSpecials);
            this.PageControl1.Controls.Add(this.pageEventTypes);
            this.PageControl1.Controls.Add(this.pagePlugins);
            this.PageControl1.Dock = System.Windows.Forms.DockStyle.Top;
            this.PageControl1.Location = new System.Drawing.Point(0, 0);
            this.PageControl1.Margin = new System.Windows.Forms.Padding(2);
            this.PageControl1.Name = "PageControl1";
            this.PageControl1.SelectedIndex = 0;
            this.PageControl1.Size = new System.Drawing.Size(749, 692);
            this.PageControl1.TabIndex = 0;
            // 
            // pageCommon
            // 
            this.pageCommon.Controls.Add(this.lblCertaintyAlgorithm);
            this.pageCommon.Controls.Add(this.cmbCertaintyAlgorithm);
            this.pageCommon.Controls.Add(this.groupBox1);
            this.pageCommon.Controls.Add(this.lblLanguage);
            this.pageCommon.Controls.Add(this.grpInternet);
            this.pageCommon.Controls.Add(this.grpOther);
            this.pageCommon.Controls.Add(this.cmbLanguages);
            this.pageCommon.Location = new System.Drawing.Point(4, 26);
            this.pageCommon.Margin = new System.Windows.Forms.Padding(2);
            this.pageCommon.Name = "pageCommon";
            this.pageCommon.Padding = new System.Windows.Forms.Padding(10);
            this.pageCommon.Size = new System.Drawing.Size(741, 762);
            this.pageCommon.TabIndex = 0;
            this.pageCommon.Text = "pageCommon";
            // 
            // lblLanguage
            // 
            this.lblLanguage.AutoSize = true;
            this.lblLanguage.Location = new System.Drawing.Point(11, 547);
            this.lblLanguage.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblLanguage.Name = "lblLanguage";
            this.lblLanguage.Size = new System.Drawing.Size(80, 17);
            this.lblLanguage.TabIndex = 0;
            this.lblLanguage.Text = "lblLanguage";
            // 
            // cmbLanguages
            // 
            this.cmbLanguages.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbLanguages.Location = new System.Drawing.Point(104, 543);
            this.cmbLanguages.Margin = new System.Windows.Forms.Padding(2);
            this.cmbLanguages.Name = "cmbLanguages";
            this.cmbLanguages.Size = new System.Drawing.Size(230, 25);
            this.cmbLanguages.TabIndex = 4;
            // 
            // lblCertaintyAlgorithm
            // 
            this.lblCertaintyAlgorithm.AutoSize = true;
            this.lblCertaintyAlgorithm.Location = new System.Drawing.Point(11, 586);
            this.lblCertaintyAlgorithm.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblCertaintyAlgorithm.Name = "lblCertaintyAlgorithm";
            this.lblCertaintyAlgorithm.Size = new System.Drawing.Size(125, 17);
            this.lblCertaintyAlgorithm.TabIndex = 7;
            this.lblCertaintyAlgorithm.Text = "Certainty algorithm";
            // 
            // cmbCertaintyAlgorithm
            // 
            this.cmbCertaintyAlgorithm.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbCertaintyAlgorithm.Location = new System.Drawing.Point(300, 582);
            this.cmbCertaintyAlgorithm.Margin = new System.Windows.Forms.Padding(2);
            this.cmbCertaintyAlgorithm.Name = "cmbCertaintyAlgorithm";
            this.cmbCertaintyAlgorithm.Size = new System.Drawing.Size(138, 25);
            this.cmbCertaintyAlgorithm.TabIndex = 8;
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.lblBackupRevisionsMaxCount);
            this.groupBox1.Controls.Add(this.lblMinutes);
            this.groupBox1.Controls.Add(this.numBackupRevisionsMaxCount);
            this.groupBox1.Controls.Add(this.numASMin);
            this.groupBox1.Controls.Add(this.chkAutosave);
            this.groupBox1.Controls.Add(this.grpFileBackup);
            this.groupBox1.Location = new System.Drawing.Point(348, 10);
            this.groupBox1.Margin = new System.Windows.Forms.Padding(10);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Padding = new System.Windows.Forms.Padding(2);
            this.groupBox1.Size = new System.Drawing.Size(379, 260);
            this.groupBox1.TabIndex = 6;
            this.groupBox1.TabStop = false;
            // 
            // lblBackupRevisionsMaxCount
            // 
            this.lblBackupRevisionsMaxCount.Location = new System.Drawing.Point(12, 215);
            this.lblBackupRevisionsMaxCount.Margin = new System.Windows.Forms.Padding(10, 10, 10, 0);
            this.lblBackupRevisionsMaxCount.Name = "lblBackupRevisionsMaxCount";
            this.lblBackupRevisionsMaxCount.Size = new System.Drawing.Size(270, 29);
            this.lblBackupRevisionsMaxCount.TabIndex = 10;
            this.lblBackupRevisionsMaxCount.Text = "BackupRevisionsMaxCount";
            // 
            // lblMinutes
            // 
            this.lblMinutes.AutoSize = true;
            this.lblMinutes.Location = new System.Drawing.Point(289, 188);
            this.lblMinutes.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblMinutes.Name = "lblMinutes";
            this.lblMinutes.Size = new System.Drawing.Size(66, 17);
            this.lblMinutes.TabIndex = 9;
            this.lblMinutes.Text = "lblMinutes";
            // 
            // numBackupRevisionsMaxCount
            // 
            this.numBackupRevisionsMaxCount.Location = new System.Drawing.Point(295, 212);
            this.numBackupRevisionsMaxCount.Margin = new System.Windows.Forms.Padding(2);
            this.numBackupRevisionsMaxCount.Maximum = new decimal(new int[] {
            1000,
            0,
            0,
            0});
            this.numBackupRevisionsMaxCount.Name = "numBackupRevisionsMaxCount";
            this.numBackupRevisionsMaxCount.Size = new System.Drawing.Size(60, 24);
            this.numBackupRevisionsMaxCount.TabIndex = 8;
            this.numBackupRevisionsMaxCount.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            // 
            // numASMin
            // 
            this.numASMin.Location = new System.Drawing.Point(234, 182);
            this.numASMin.Margin = new System.Windows.Forms.Padding(2);
            this.numASMin.Maximum = new decimal(new int[] {
            120,
            0,
            0,
            0});
            this.numASMin.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.numASMin.Name = "numASMin";
            this.numASMin.Size = new System.Drawing.Size(49, 24);
            this.numASMin.TabIndex = 8;
            this.numASMin.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            // 
            // chkAutosave
            // 
            this.chkAutosave.AutoSize = true;
            this.chkAutosave.Location = new System.Drawing.Point(12, 184);
            this.chkAutosave.Margin = new System.Windows.Forms.Padding(10, 10, 10, 0);
            this.chkAutosave.Name = "chkAutosave";
            this.chkAutosave.Size = new System.Drawing.Size(109, 21);
            this.chkAutosave.TabIndex = 7;
            this.chkAutosave.Text = "chkAutosave";
            this.chkAutosave.UseVisualStyleBackColor = true;
            // 
            // grpFileBackup
            // 
            this.grpFileBackup.Controls.Add(this.radFBEachRevision);
            this.grpFileBackup.Controls.Add(this.radFBOnlyPrev);
            this.grpFileBackup.Controls.Add(this.radFBNone);
            this.grpFileBackup.Location = new System.Drawing.Point(12, 30);
            this.grpFileBackup.Margin = new System.Windows.Forms.Padding(10);
            this.grpFileBackup.Name = "grpFileBackup";
            this.grpFileBackup.Padding = new System.Windows.Forms.Padding(2);
            this.grpFileBackup.Size = new System.Drawing.Size(342, 136);
            this.grpFileBackup.TabIndex = 6;
            this.grpFileBackup.TabStop = false;
            this.grpFileBackup.Text = "grpFileBackup";
            // 
            // radFBEachRevision
            // 
            this.radFBEachRevision.Location = new System.Drawing.Point(12, 98);
            this.radFBEachRevision.Margin = new System.Windows.Forms.Padding(10);
            this.radFBEachRevision.Name = "radFBEachRevision";
            this.radFBEachRevision.Size = new System.Drawing.Size(294, 24);
            this.radFBEachRevision.TabIndex = 2;
            this.radFBEachRevision.TabStop = true;
            this.radFBEachRevision.Text = "radFBEachRevision";
            this.radFBEachRevision.UseVisualStyleBackColor = true;
            // 
            // radFBOnlyPrev
            // 
            this.radFBOnlyPrev.Location = new System.Drawing.Point(12, 64);
            this.radFBOnlyPrev.Margin = new System.Windows.Forms.Padding(10, 10, 10, 0);
            this.radFBOnlyPrev.Name = "radFBOnlyPrev";
            this.radFBOnlyPrev.Size = new System.Drawing.Size(294, 24);
            this.radFBOnlyPrev.TabIndex = 1;
            this.radFBOnlyPrev.TabStop = true;
            this.radFBOnlyPrev.Text = "radFBOnlyPrev";
            this.radFBOnlyPrev.UseVisualStyleBackColor = true;
            // 
            // radFBNone
            // 
            this.radFBNone.Location = new System.Drawing.Point(12, 30);
            this.radFBNone.Margin = new System.Windows.Forms.Padding(10, 10, 10, 0);
            this.radFBNone.Name = "radFBNone";
            this.radFBNone.Size = new System.Drawing.Size(294, 24);
            this.radFBNone.TabIndex = 0;
            this.radFBNone.TabStop = true;
            this.radFBNone.Text = "radFBNone";
            this.radFBNone.UseVisualStyleBackColor = true;
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
            this.grpInternet.Location = new System.Drawing.Point(11, 10);
            this.grpInternet.Margin = new System.Windows.Forms.Padding(2);
            this.grpInternet.Name = "grpInternet";
            this.grpInternet.Padding = new System.Windows.Forms.Padding(2);
            this.grpInternet.Size = new System.Drawing.Size(324, 195);
            this.grpInternet.TabIndex = 1;
            this.grpInternet.TabStop = false;
            this.grpInternet.Text = "grpInternet";
            // 
            // lblProxyServer
            // 
            this.lblProxyServer.AutoSize = true;
            this.lblProxyServer.Location = new System.Drawing.Point(12, 61);
            this.lblProxyServer.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblProxyServer.Name = "lblProxyServer";
            this.lblProxyServer.Size = new System.Drawing.Size(97, 17);
            this.lblProxyServer.TabIndex = 0;
            this.lblProxyServer.Text = "lblProxyServer";
            // 
            // lblProxyPort
            // 
            this.lblProxyPort.AutoSize = true;
            this.lblProxyPort.Location = new System.Drawing.Point(12, 90);
            this.lblProxyPort.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblProxyPort.Name = "lblProxyPort";
            this.lblProxyPort.Size = new System.Drawing.Size(83, 17);
            this.lblProxyPort.TabIndex = 1;
            this.lblProxyPort.Text = "lblProxyPort";
            // 
            // lblProxyLogin
            // 
            this.lblProxyLogin.AutoSize = true;
            this.lblProxyLogin.Location = new System.Drawing.Point(12, 120);
            this.lblProxyLogin.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblProxyLogin.Name = "lblProxyLogin";
            this.lblProxyLogin.Size = new System.Drawing.Size(90, 17);
            this.lblProxyLogin.TabIndex = 2;
            this.lblProxyLogin.Text = "lblProxyLogin";
            // 
            // lblProxyPassword
            // 
            this.lblProxyPassword.AutoSize = true;
            this.lblProxyPassword.Location = new System.Drawing.Point(12, 149);
            this.lblProxyPassword.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblProxyPassword.Name = "lblProxyPassword";
            this.lblProxyPassword.Size = new System.Drawing.Size(115, 17);
            this.lblProxyPassword.TabIndex = 3;
            this.lblProxyPassword.Text = "lblProxyPassword";
            // 
            // chkUseProxy
            // 
            this.chkUseProxy.AutoSize = true;
            this.chkUseProxy.Location = new System.Drawing.Point(22, 29);
            this.chkUseProxy.Margin = new System.Windows.Forms.Padding(2);
            this.chkUseProxy.Name = "chkUseProxy";
            this.chkUseProxy.Size = new System.Drawing.Size(111, 21);
            this.chkUseProxy.TabIndex = 0;
            this.chkUseProxy.Text = "chkUseProxy";
            // 
            // txtProxyServer
            // 
            this.txtProxyServer.Location = new System.Drawing.Point(112, 58);
            this.txtProxyServer.Margin = new System.Windows.Forms.Padding(2);
            this.txtProxyServer.Name = "txtProxyServer";
            this.txtProxyServer.Size = new System.Drawing.Size(192, 24);
            this.txtProxyServer.TabIndex = 1;
            // 
            // txtProxyPort
            // 
            this.txtProxyPort.Location = new System.Drawing.Point(112, 88);
            this.txtProxyPort.Margin = new System.Windows.Forms.Padding(2);
            this.txtProxyPort.Name = "txtProxyPort";
            this.txtProxyPort.Size = new System.Drawing.Size(192, 24);
            this.txtProxyPort.TabIndex = 2;
            // 
            // txtProxyLogin
            // 
            this.txtProxyLogin.Location = new System.Drawing.Point(112, 118);
            this.txtProxyLogin.Margin = new System.Windows.Forms.Padding(2);
            this.txtProxyLogin.Name = "txtProxyLogin";
            this.txtProxyLogin.Size = new System.Drawing.Size(192, 24);
            this.txtProxyLogin.TabIndex = 3;
            // 
            // txtProxyPass
            // 
            this.txtProxyPass.Location = new System.Drawing.Point(112, 146);
            this.txtProxyPass.Margin = new System.Windows.Forms.Padding(2);
            this.txtProxyPass.Name = "txtProxyPass";
            this.txtProxyPass.PasswordChar = '*';
            this.txtProxyPass.Size = new System.Drawing.Size(192, 24);
            this.txtProxyPass.TabIndex = 4;
            this.txtProxyPass.Text = "txtProxyPass";
            // 
            // grpOther
            // 
            this.grpOther.Controls.Add(this.chkDisplayFullFileName);
            this.grpOther.Controls.Add(this.chkDialogClosingWarn);
            this.grpOther.Controls.Add(this.chkCharsetDetection);
            this.grpOther.Controls.Add(this.chkAutoCheckUpdates);
            this.grpOther.Controls.Add(this.chkLoadRecentFiles);
            this.grpOther.Controls.Add(this.chkShowOnStart);
            this.grpOther.Location = new System.Drawing.Point(11, 282);
            this.grpOther.Margin = new System.Windows.Forms.Padding(2);
            this.grpOther.Name = "grpOther";
            this.grpOther.Padding = new System.Windows.Forms.Padding(10);
            this.grpOther.Size = new System.Drawing.Size(715, 233);
            this.grpOther.TabIndex = 2;
            this.grpOther.TabStop = false;
            this.grpOther.Text = "grpOther";
            // 
            // chkDisplayFullFileName
            // 
            this.chkDisplayFullFileName.AutoSize = true;
            this.chkDisplayFullFileName.Location = new System.Drawing.Point(20, 192);
            this.chkDisplayFullFileName.Margin = new System.Windows.Forms.Padding(10, 10, 10, 0);
            this.chkDisplayFullFileName.Name = "chkDisplayFullFileName";
            this.chkDisplayFullFileName.Size = new System.Drawing.Size(166, 21);
            this.chkDisplayFullFileName.TabIndex = 9;
            this.chkDisplayFullFileName.Text = "chkDisplayFullFileName";
            // 
            // chkDialogClosingWarn
            // 
            this.chkDialogClosingWarn.AutoSize = true;
            this.chkDialogClosingWarn.Location = new System.Drawing.Point(20, 161);
            this.chkDialogClosingWarn.Margin = new System.Windows.Forms.Padding(10, 10, 10, 0);
            this.chkDialogClosingWarn.Name = "chkDialogClosingWarn";
            this.chkDialogClosingWarn.Size = new System.Drawing.Size(166, 21);
            this.chkDialogClosingWarn.TabIndex = 9;
            this.chkDialogClosingWarn.Text = "chkDialogClosingWarn";
            // 
            // chkCharsetDetection
            // 
            this.chkCharsetDetection.AutoSize = true;
            this.chkCharsetDetection.Location = new System.Drawing.Point(20, 130);
            this.chkCharsetDetection.Margin = new System.Windows.Forms.Padding(10, 10, 10, 0);
            this.chkCharsetDetection.Name = "chkCharsetDetection";
            this.chkCharsetDetection.Size = new System.Drawing.Size(158, 21);
            this.chkCharsetDetection.TabIndex = 9;
            this.chkCharsetDetection.Text = "chkCharsetDetection";
            // 
            // chkAutoCheckUpdates
            // 
            this.chkAutoCheckUpdates.AutoSize = true;
            this.chkAutoCheckUpdates.Location = new System.Drawing.Point(20, 99);
            this.chkAutoCheckUpdates.Margin = new System.Windows.Forms.Padding(10, 10, 10, 0);
            this.chkAutoCheckUpdates.Name = "chkAutoCheckUpdates";
            this.chkAutoCheckUpdates.Size = new System.Drawing.Size(169, 21);
            this.chkAutoCheckUpdates.TabIndex = 9;
            this.chkAutoCheckUpdates.Text = "chkAutoCheckUpdates";
            // 
            // chkLoadRecentFiles
            // 
            this.chkLoadRecentFiles.AutoSize = true;
            this.chkLoadRecentFiles.Location = new System.Drawing.Point(20, 68);
            this.chkLoadRecentFiles.Margin = new System.Windows.Forms.Padding(10, 10, 10, 0);
            this.chkLoadRecentFiles.Name = "chkLoadRecentFiles";
            this.chkLoadRecentFiles.Size = new System.Drawing.Size(149, 21);
            this.chkLoadRecentFiles.TabIndex = 9;
            this.chkLoadRecentFiles.Text = "chkLoadRecentFiles";
            // 
            // chkShowOnStart
            // 
            this.chkShowOnStart.AutoSize = true;
            this.chkShowOnStart.Location = new System.Drawing.Point(20, 38);
            this.chkShowOnStart.Margin = new System.Windows.Forms.Padding(10, 10, 10, 0);
            this.chkShowOnStart.Name = "chkShowOnStart";
            this.chkShowOnStart.Size = new System.Drawing.Size(134, 21);
            this.chkShowOnStart.TabIndex = 0;
            this.chkShowOnStart.Text = "chkShowOnStart";
            // 
            // pageMultimedia
            // 
            this.pageMultimedia.BackColor = System.Drawing.SystemColors.Control;
            this.pageMultimedia.Controls.Add(this.cmbMediaStoreDefault);
            this.pageMultimedia.Controls.Add(this.chkDeleteMediaFileWithoutConfirm);
            this.pageMultimedia.Controls.Add(this.chkAllowDeleteMediaFileFromRefs);
            this.pageMultimedia.Controls.Add(this.chkAllowDeleteMediaFileFromStgArc);
            this.pageMultimedia.Controls.Add(this.lblMediaStoreDefault);
            this.pageMultimedia.Controls.Add(this.chkAllowMediaStoreRelativeReferences);
            this.pageMultimedia.Controls.Add(this.chkAllowMediaDirectRefs);
            this.pageMultimedia.Controls.Add(this.chkEmbeddedMediaPlayer);
            this.pageMultimedia.Controls.Add(this.chkRemovableMediaWarning);
            this.pageMultimedia.Controls.Add(this.chkHighlightInaccessibleFiles);
            this.pageMultimedia.Location = new System.Drawing.Point(4, 26);
            this.pageMultimedia.Margin = new System.Windows.Forms.Padding(2);
            this.pageMultimedia.Name = "pageMultimedia";
            this.pageMultimedia.Padding = new System.Windows.Forms.Padding(10);
            this.pageMultimedia.Size = new System.Drawing.Size(741, 762);
            this.pageMultimedia.TabIndex = 6;
            this.pageMultimedia.Text = "pageMultimedia";
            // 
            // cmbMediaStoreDefault
            // 
            this.cmbMediaStoreDefault.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbMediaStoreDefault.FormattingEnabled = true;
            this.cmbMediaStoreDefault.Location = new System.Drawing.Point(382, 181);
            this.cmbMediaStoreDefault.Margin = new System.Windows.Forms.Padding(2);
            this.cmbMediaStoreDefault.Name = "cmbMediaStoreDefault";
            this.cmbMediaStoreDefault.Size = new System.Drawing.Size(218, 25);
            this.cmbMediaStoreDefault.TabIndex = 12;
            // 
            // chkDeleteMediaFileWithoutConfirm
            // 
            this.chkDeleteMediaFileWithoutConfirm.AutoSize = true;
            this.chkDeleteMediaFileWithoutConfirm.Location = new System.Drawing.Point(20, 308);
            this.chkDeleteMediaFileWithoutConfirm.Margin = new System.Windows.Forms.Padding(10);
            this.chkDeleteMediaFileWithoutConfirm.Name = "chkDeleteMediaFileWithoutConfirm";
            this.chkDeleteMediaFileWithoutConfirm.Size = new System.Drawing.Size(240, 21);
            this.chkDeleteMediaFileWithoutConfirm.TabIndex = 11;
            this.chkDeleteMediaFileWithoutConfirm.Text = "chkDeleteMediaFileWithoutConfirm";
            // 
            // chkAllowDeleteMediaFileFromRefs
            // 
            this.chkAllowDeleteMediaFileFromRefs.AutoSize = true;
            this.chkAllowDeleteMediaFileFromRefs.Location = new System.Drawing.Point(20, 266);
            this.chkAllowDeleteMediaFileFromRefs.Margin = new System.Windows.Forms.Padding(10);
            this.chkAllowDeleteMediaFileFromRefs.Name = "chkAllowDeleteMediaFileFromRefs";
            this.chkAllowDeleteMediaFileFromRefs.Size = new System.Drawing.Size(230, 21);
            this.chkAllowDeleteMediaFileFromRefs.TabIndex = 11;
            this.chkAllowDeleteMediaFileFromRefs.Text = "chkAllowDeleteMediaFileFromRefs";
            // 
            // chkAllowDeleteMediaFileFromStgArc
            // 
            this.chkAllowDeleteMediaFileFromStgArc.AutoSize = true;
            this.chkAllowDeleteMediaFileFromStgArc.Location = new System.Drawing.Point(20, 225);
            this.chkAllowDeleteMediaFileFromStgArc.Margin = new System.Windows.Forms.Padding(10);
            this.chkAllowDeleteMediaFileFromStgArc.Name = "chkAllowDeleteMediaFileFromStgArc";
            this.chkAllowDeleteMediaFileFromStgArc.Size = new System.Drawing.Size(245, 21);
            this.chkAllowDeleteMediaFileFromStgArc.TabIndex = 11;
            this.chkAllowDeleteMediaFileFromStgArc.Text = "chkAllowDeleteMediaFileFromStgArc";
            // 
            // lblMediaStoreDefault
            // 
            this.lblMediaStoreDefault.AutoSize = true;
            this.lblMediaStoreDefault.Location = new System.Drawing.Point(20, 184);
            this.lblMediaStoreDefault.Margin = new System.Windows.Forms.Padding(10);
            this.lblMediaStoreDefault.Name = "lblMediaStoreDefault";
            this.lblMediaStoreDefault.Size = new System.Drawing.Size(130, 17);
            this.lblMediaStoreDefault.TabIndex = 11;
            this.lblMediaStoreDefault.Text = "lblMediaStoreDefault";
            // 
            // chkAllowMediaStoreRelativeReferences
            // 
            this.chkAllowMediaStoreRelativeReferences.AutoSize = true;
            this.chkAllowMediaStoreRelativeReferences.Location = new System.Drawing.Point(20, 142);
            this.chkAllowMediaStoreRelativeReferences.Margin = new System.Windows.Forms.Padding(10);
            this.chkAllowMediaStoreRelativeReferences.Name = "chkAllowMediaStoreRelativeReferences";
            this.chkAllowMediaStoreRelativeReferences.Size = new System.Drawing.Size(263, 21);
            this.chkAllowMediaStoreRelativeReferences.TabIndex = 11;
            this.chkAllowMediaStoreRelativeReferences.Text = "chkAllowMediaStoreRelativeReferences";
            // 
            // chkAllowMediaDirectRefs
            // 
            this.chkAllowMediaDirectRefs.AutoSize = true;
            this.chkAllowMediaDirectRefs.Location = new System.Drawing.Point(20, 102);
            this.chkAllowMediaDirectRefs.Margin = new System.Windows.Forms.Padding(10);
            this.chkAllowMediaDirectRefs.Name = "chkAllowMediaDirectRefs";
            this.chkAllowMediaDirectRefs.Size = new System.Drawing.Size(178, 21);
            this.chkAllowMediaDirectRefs.TabIndex = 11;
            this.chkAllowMediaDirectRefs.Text = "chkAllowMediaDirectRefs";
            // 
            // chkEmbeddedMediaPlayer
            // 
            this.chkEmbeddedMediaPlayer.AutoSize = true;
            this.chkEmbeddedMediaPlayer.Location = new System.Drawing.Point(20, 61);
            this.chkEmbeddedMediaPlayer.Margin = new System.Windows.Forms.Padding(10);
            this.chkEmbeddedMediaPlayer.Name = "chkEmbeddedMediaPlayer";
            this.chkEmbeddedMediaPlayer.Size = new System.Drawing.Size(189, 21);
            this.chkEmbeddedMediaPlayer.TabIndex = 11;
            this.chkEmbeddedMediaPlayer.Text = "chkEmbeddedMediaPlayer";
            // 
            // chkRemovableMediaWarning
            // 
            this.chkRemovableMediaWarning.AutoSize = true;
            this.chkRemovableMediaWarning.Location = new System.Drawing.Point(20, 20);
            this.chkRemovableMediaWarning.Margin = new System.Windows.Forms.Padding(10);
            this.chkRemovableMediaWarning.Name = "chkRemovableMediaWarning";
            this.chkRemovableMediaWarning.Size = new System.Drawing.Size(206, 21);
            this.chkRemovableMediaWarning.TabIndex = 10;
            this.chkRemovableMediaWarning.Text = "chkRemovableMediaWarning";
            // 
            // chkHighlightInaccessibleFiles
            // 
            this.chkHighlightInaccessibleFiles.AutoSize = true;
            this.chkHighlightInaccessibleFiles.Location = new System.Drawing.Point(20, 350);
            this.chkHighlightInaccessibleFiles.Margin = new System.Windows.Forms.Padding(10);
            this.chkHighlightInaccessibleFiles.Name = "chkHighlightInaccessibleFiles";
            this.chkHighlightInaccessibleFiles.Size = new System.Drawing.Size(206, 21);
            this.chkHighlightInaccessibleFiles.TabIndex = 11;
            this.chkHighlightInaccessibleFiles.Text = "chkHighlightInaccessibleFiles";
            // 
            // pageCharts
            // 
            this.pageCharts.BackColor = System.Drawing.SystemColors.Control;
            this.pageCharts.Controls.Add(this.tabsCharts);
            this.pageCharts.Location = new System.Drawing.Point(4, 26);
            this.pageCharts.Margin = new System.Windows.Forms.Padding(2);
            this.pageCharts.Name = "pageCharts";
            this.pageCharts.Padding = new System.Windows.Forms.Padding(10);
            this.pageCharts.Size = new System.Drawing.Size(741, 762);
            this.pageCharts.TabIndex = 4;
            this.pageCharts.Text = "pageCharts";
            // 
            // tabsCharts
            // 
            this.tabsCharts.Controls.Add(this.pageTreeChart);
            this.tabsCharts.Controls.Add(this.pageAncCircle);
            this.tabsCharts.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabsCharts.Location = new System.Drawing.Point(10, 10);
            this.tabsCharts.Margin = new System.Windows.Forms.Padding(2);
            this.tabsCharts.Name = "tabsCharts";
            this.tabsCharts.SelectedIndex = 0;
            this.tabsCharts.Size = new System.Drawing.Size(721, 702);
            this.tabsCharts.TabIndex = 0;
            // 
            // tabsTreeCharts
            // 
            this.tabsTreeCharts.Controls.Add(this.pageTreePersons);
            this.tabsTreeCharts.Controls.Add(this.pageTreeDesign);
            this.tabsTreeCharts.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabsTreeCharts.Location = new System.Drawing.Point(10, 10);
            this.tabsTreeCharts.Margin = new System.Windows.Forms.Padding(2);
            this.tabsTreeCharts.Name = "tabsTreeCharts";
            this.tabsTreeCharts.SelectedIndex = 0;
            this.tabsTreeCharts.Size = new System.Drawing.Size(721, 702);
            this.tabsTreeCharts.TabIndex = 0;
            // 
            // pageTreePersons
            // 
            this.pageTreePersons.BackColor = System.Drawing.SystemColors.Control;
            this.pageTreePersons.Controls.Add(this.panTreePersons);
            this.pageTreePersons.Location = new System.Drawing.Point(4, 26);
            this.pageTreePersons.Margin = new System.Windows.Forms.Padding(2);
            this.pageTreePersons.Name = "pageTreePersons";
            this.pageTreePersons.Padding = new System.Windows.Forms.Padding(10);
            this.pageTreePersons.Size = new System.Drawing.Size(713, 712);
            this.pageTreePersons.TabIndex = 3;
            this.pageTreePersons.Text = "pageTreePersons";
            // 
            // pageTreeDesign
            // 
            this.pageTreeDesign.BackColor = System.Drawing.SystemColors.Control;
            this.pageTreeDesign.Controls.Add(this.chkSeparateDepth);
            this.pageTreeDesign.Controls.Add(this.numDefaultDepth);
            this.pageTreeDesign.Controls.Add(this.lblDefaultDepth);
            this.pageTreeDesign.Controls.Add(this.numDefaultDepthAncestors);
            this.pageTreeDesign.Controls.Add(this.lblDefaultDepthAncestors);
            this.pageTreeDesign.Controls.Add(this.numDefaultDepthDescendants);
            this.pageTreeDesign.Controls.Add(this.lblDefaultDepthDescendants);
            this.pageTreeDesign.Controls.Add(this.grpSpacings);
            this.pageTreeDesign.Controls.Add(this.grpTreeDecor);
            this.pageTreeDesign.Controls.Add(this.chkUseExtraControls);
            this.pageTreeDesign.Controls.Add(this.lblTextEffect);
            this.pageTreeDesign.Controls.Add(this.cmbTextEffect);
            this.pageTreeDesign.Location = new System.Drawing.Point(4, 26);
            this.pageTreeDesign.Margin = new System.Windows.Forms.Padding(2);
            this.pageTreeDesign.Name = "pageTreeDesign";
            this.pageTreeDesign.Padding = new System.Windows.Forms.Padding(10);
            this.pageTreeDesign.Size = new System.Drawing.Size(713, 712);
            this.pageTreeDesign.TabIndex = 3;
            this.pageTreeDesign.Text = "pageTreeDesign";
            // 
            // pageTreeChart
            // 
            this.pageTreeChart.BackColor = System.Drawing.SystemColors.Control;
            this.pageTreeChart.Controls.Add(this.tabsTreeCharts);
            this.pageTreeChart.Location = new System.Drawing.Point(4, 26);
            this.pageTreeChart.Margin = new System.Windows.Forms.Padding(2);
            this.pageTreeChart.Name = "pageTreeChart";
            this.pageTreeChart.Padding = new System.Windows.Forms.Padding(10);
            this.pageTreeChart.Size = new System.Drawing.Size(713, 712);
            this.pageTreeChart.TabIndex = 3;
            this.pageTreeChart.Text = "pageTreeChart";
            // 
            // chkSeparateDepth
            // 
            this.chkSeparateDepth.Location = new System.Drawing.Point(334, 228);
            this.chkSeparateDepth.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkSeparateDepth.Name = "chkSeparateDepth";
            this.chkSeparateDepth.Size = new System.Drawing.Size(254, 22);
            this.chkSeparateDepth.TabIndex = 12;
            this.chkSeparateDepth.Text = "chkSeparateDepth";
            this.chkSeparateDepth.CheckedChanged += new System.EventHandler(this.chkSeparateDepth_CheckedChanged);
            // 
            // numDefaultDepth
            // 
            this.numDefaultDepth.Location = new System.Drawing.Point(610, 259);
            this.numDefaultDepth.Margin = new System.Windows.Forms.Padding(4);
            this.numDefaultDepth.Maximum = new decimal(new int[] {
            9,
            0,
            0,
            0});
            this.numDefaultDepth.Name = "numDefaultDepth";
            this.numDefaultDepth.Size = new System.Drawing.Size(49, 24);
            this.numDefaultDepth.TabIndex = 11;
            // 
            // lblDefaultDepth
            // 
            this.lblDefaultDepth.AutoSize = true;
            this.lblDefaultDepth.Location = new System.Drawing.Point(334, 261);
            this.lblDefaultDepth.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.lblDefaultDepth.Name = "lblDefaultDepth";
            this.lblDefaultDepth.Size = new System.Drawing.Size(101, 17);
            this.lblDefaultDepth.TabIndex = 10;
            this.lblDefaultDepth.Text = "lblDefaultDepth";
            // 
            // numDefaultDepthAncestors
            // 
            this.numDefaultDepthAncestors.Location = new System.Drawing.Point(610, 292);
            this.numDefaultDepthAncestors.Margin = new System.Windows.Forms.Padding(4);
            this.numDefaultDepthAncestors.Maximum = new decimal(new int[] {
            9,
            0,
            0,
            0});
            this.numDefaultDepthAncestors.Name = "numDefaultDepthAncestors";
            this.numDefaultDepthAncestors.Size = new System.Drawing.Size(49, 24);
            this.numDefaultDepthAncestors.TabIndex = 11;
            // 
            // lblDefaultDepthAncestors
            // 
            this.lblDefaultDepthAncestors.AutoSize = true;
            this.lblDefaultDepthAncestors.Location = new System.Drawing.Point(334, 295);
            this.lblDefaultDepthAncestors.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.lblDefaultDepthAncestors.Name = "lblDefaultDepthAncestors";
            this.lblDefaultDepthAncestors.Size = new System.Drawing.Size(161, 17);
            this.lblDefaultDepthAncestors.TabIndex = 10;
            this.lblDefaultDepthAncestors.Text = "lblDefaultDepthAncestors";
            // 
            // numDefaultDepthDescendants
            // 
            this.numDefaultDepthDescendants.Location = new System.Drawing.Point(610, 326);
            this.numDefaultDepthDescendants.Margin = new System.Windows.Forms.Padding(4);
            this.numDefaultDepthDescendants.Maximum = new decimal(new int[] {
            9,
            0,
            0,
            0});
            this.numDefaultDepthDescendants.Name = "numDefaultDepthDescendants";
            this.numDefaultDepthDescendants.Size = new System.Drawing.Size(49, 24);
            this.numDefaultDepthDescendants.TabIndex = 11;
            // 
            // lblDefaultDepthDescendants
            // 
            this.lblDefaultDepthDescendants.AutoSize = true;
            this.lblDefaultDepthDescendants.Location = new System.Drawing.Point(334, 329);
            this.lblDefaultDepthDescendants.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.lblDefaultDepthDescendants.Name = "lblDefaultDepthDescendants";
            this.lblDefaultDepthDescendants.Size = new System.Drawing.Size(180, 17);
            this.lblDefaultDepthDescendants.TabIndex = 10;
            this.lblDefaultDepthDescendants.Text = "lblDefaultDepthDescendants";
            // 
            // chkUseExtraControls
            // 
            this.chkUseExtraControls.Location = new System.Drawing.Point(11, 289);
            this.chkUseExtraControls.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkUseExtraControls.Name = "chkUseExtraControls";
            this.chkUseExtraControls.Size = new System.Drawing.Size(600, 22);
            this.chkUseExtraControls.TabIndex = 12;
            this.chkUseExtraControls.Text = "chkUseExtraControls";
            // 
            // lblTextEffect
            // 
            this.lblTextEffect.AutoSize = true;
            this.lblTextEffect.Location = new System.Drawing.Point(11, 316);
            this.lblTextEffect.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblTextEffect.Name = "lblTextEffect";
            this.lblTextEffect.Size = new System.Drawing.Size(80, 17);
            this.lblTextEffect.TabIndex = 0;
            this.lblTextEffect.Text = "lblTextEffect";
            // 
            // cmbTextEffect
            // 
            this.cmbTextEffect.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbTextEffect.Location = new System.Drawing.Point(11, 336);
            this.cmbTextEffect.Margin = new System.Windows.Forms.Padding(2);
            this.cmbTextEffect.Name = "cmbTextEffect";
            this.cmbTextEffect.Size = new System.Drawing.Size(230, 25);
            this.cmbTextEffect.TabIndex = 4;
            // 
            // grpSpacings
            // 
            this.grpSpacings.Controls.Add(this.lblSpouseDist);
            this.grpSpacings.Controls.Add(this.numSpouseDist);
            this.grpSpacings.Controls.Add(this.lblGenDist);
            this.grpSpacings.Controls.Add(this.numGenDist);
            this.grpSpacings.Controls.Add(this.lblBranchDist);
            this.grpSpacings.Controls.Add(this.numBranchDist);
            this.grpSpacings.Controls.Add(this.lblMargins);
            this.grpSpacings.Controls.Add(this.numMargins);
            this.grpSpacings.Controls.Add(this.lblPadding);
            this.grpSpacings.Controls.Add(this.numPadding);
            this.grpSpacings.Location = new System.Drawing.Point(314, 12);
            this.grpSpacings.Margin = new System.Windows.Forms.Padding(2);
            this.grpSpacings.Name = "grpSpacings";
            this.grpSpacings.Padding = new System.Windows.Forms.Padding(10);
            this.grpSpacings.Size = new System.Drawing.Size(286, 190);
            this.grpSpacings.TabIndex = 2;
            this.grpSpacings.TabStop = false;
            this.grpSpacings.Text = "grpSpacings";
            // 
            // lblMargins
            // 
            this.lblMargins.AutoSize = true;
            this.lblMargins.Location = new System.Drawing.Point(20, 28);
            this.lblMargins.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.lblMargins.Name = "lblMargins";
            this.lblMargins.Size = new System.Drawing.Size(66, 17);
            this.lblMargins.TabIndex = 0;
            this.lblMargins.Text = "lblMargins";
            // 
            // numMargins
            // 
            this.numMargins.Location = new System.Drawing.Point(225, 22);
            this.numMargins.Margin = new System.Windows.Forms.Padding(4);
            this.numMargins.Maximum = new decimal(new int[] {
            120,
            0,
            0,
            0});
            this.numMargins.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.numMargins.Name = "numMargins";
            this.numMargins.Size = new System.Drawing.Size(49, 24);
            this.numMargins.TabIndex = 9;
            this.numMargins.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            // 
            // lblBranchDist
            // 
            this.lblBranchDist.AutoSize = true;
            this.lblBranchDist.Location = new System.Drawing.Point(20, 58);
            this.lblBranchDist.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.lblBranchDist.Name = "lblBranchDist";
            this.lblBranchDist.Size = new System.Drawing.Size(86, 17);
            this.lblBranchDist.TabIndex = 0;
            this.lblBranchDist.Text = "lblBranchDist";
            // 
            // numBranchDist
            // 
            this.numBranchDist.Location = new System.Drawing.Point(225, 55);
            this.numBranchDist.Margin = new System.Windows.Forms.Padding(4);
            this.numBranchDist.Maximum = new decimal(new int[] {
            120,
            0,
            0,
            0});
            this.numBranchDist.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.numBranchDist.Name = "numBranchDist";
            this.numBranchDist.Size = new System.Drawing.Size(49, 24);
            this.numBranchDist.TabIndex = 9;
            this.numBranchDist.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            // 
            // lblGenDist
            // 
            this.lblGenDist.AutoSize = true;
            this.lblGenDist.Location = new System.Drawing.Point(20, 89);
            this.lblGenDist.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.lblGenDist.Name = "lblGenDist";
            this.lblGenDist.Size = new System.Drawing.Size(67, 17);
            this.lblGenDist.TabIndex = 0;
            this.lblGenDist.Text = "lblGenDist";
            // 
            // numGenDist
            // 
            this.numGenDist.Location = new System.Drawing.Point(225, 88);
            this.numGenDist.Margin = new System.Windows.Forms.Padding(4);
            this.numGenDist.Maximum = new decimal(new int[] {
            120,
            0,
            0,
            0});
            this.numGenDist.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.numGenDist.Name = "numGenDist";
            this.numGenDist.Size = new System.Drawing.Size(49, 24);
            this.numGenDist.TabIndex = 9;
            this.numGenDist.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            // 
            // lblSpouseDist
            // 
            this.lblSpouseDist.AutoSize = true;
            this.lblSpouseDist.Location = new System.Drawing.Point(20, 121);
            this.lblSpouseDist.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.lblSpouseDist.Name = "lblSpouseDist";
            this.lblSpouseDist.Size = new System.Drawing.Size(88, 17);
            this.lblSpouseDist.TabIndex = 0;
            this.lblSpouseDist.Text = "lblSpouseDist";
            // 
            // numSpouseDist
            // 
            this.numSpouseDist.Location = new System.Drawing.Point(225, 119);
            this.numSpouseDist.Margin = new System.Windows.Forms.Padding(4);
            this.numSpouseDist.Maximum = new decimal(new int[] { 120, 0, 0, 0});
            this.numSpouseDist.Minimum = new decimal(new int[] { 1, 0, 0, 0});
            this.numSpouseDist.Name = "numSpouseDist";
            this.numSpouseDist.Size = new System.Drawing.Size(49, 24);
            this.numSpouseDist.TabIndex = 9;
            this.numSpouseDist.Value = new decimal(new int[] { 1, 0, 0, 0});
            // 
            // lblPadding
            // 
            this.lblPadding.AutoSize = true;
            this.lblPadding.Location = new System.Drawing.Point(20, 153);
            this.lblPadding.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.lblPadding.Name = "lblPadding";
            this.lblPadding.Size = new System.Drawing.Size(88, 17);
            this.lblPadding.TabIndex = 0;
            this.lblPadding.Text = "lblPadding";
            // 
            // numPadding
            // 
            this.numPadding.Location = new System.Drawing.Point(225, 150);
            this.numPadding.Margin = new System.Windows.Forms.Padding(4);
            this.numPadding.Maximum = new decimal(new int[] { 20, 0, 0, 0 });
            this.numPadding.Minimum = new decimal(new int[] { 1, 0, 0, 0 });
            this.numPadding.Name = "numPadding";
            this.numPadding.Size = new System.Drawing.Size(49, 24);
            this.numPadding.TabIndex = 10;
            this.numPadding.Value = new decimal(new int[] { 1, 0, 0, 0 });
            // 
            // panTreePersons
            // 
            this.panTreePersons.AutoScroll = true;
            this.panTreePersons.Controls.Add(this.chkSurname);
            this.panTreePersons.Controls.Add(this.chkTreeSurnameFirst);
            this.panTreePersons.Controls.Add(this.chkName);
            this.panTreePersons.Controls.Add(this.chkPatronymic);
            this.panTreePersons.Controls.Add(this.chkDiffLines);
            this.panTreePersons.Controls.Add(this.chkBirthDate);
            this.panTreePersons.Controls.Add(this.chkDeathDate);
            this.panTreePersons.Controls.Add(this.chkOnlyYears);
            this.panTreePersons.Controls.Add(this.chkShowAge);
            this.panTreePersons.Controls.Add(this.chkMarriagesDates);
            this.panTreePersons.Controls.Add(this.chkKinship);
            this.panTreePersons.Controls.Add(this.chkSignsVisible);
            this.panTreePersons.Controls.Add(this.chkTreeDecorative);
            this.panTreePersons.Controls.Add(this.chkPortraitsVisible);
            this.panTreePersons.Controls.Add(this.chkDefaultPortraits);
            this.panTreePersons.Controls.Add(this.chkInvertedTree);
            this.panTreePersons.Controls.Add(this.chkChildlessExclude);
            this.panTreePersons.Controls.Add(this.chkShowPlaces);
            this.panTreePersons.Controls.Add(this.chkSeparateDAPLines);
            this.panTreePersons.Controls.Add(this.chkOnlyLocality);
            this.panTreePersons.Controls.Add(this.chkHideUnknownSpouses);
            this.panTreePersons.Controls.Add(this.chkCheckTreeSize);
            this.panTreePersons.Controls.Add(this.chkDottedLinesOfAdoptedChildren);
            this.panTreePersons.Controls.Add(this.chkDottedLinesOfDivorcedSpouses);
            this.panTreePersons.Controls.Add(this.chkBoldNames);
            this.panTreePersons.Controls.Add(this.chkMinimizingWidth);
            this.panTreePersons.Controls.Add(this.chkURNotesVisible);
            this.panTreePersons.Controls.Add(this.chkSameCardsWidth);
            this.panTreePersons.Controls.Add(this.chkFullNameOnOneLine);
            this.panTreePersons.Controls.Add(this.chkDateDesignations);
            this.panTreePersons.Controls.Add(this.chkMourningEdges);
            this.panTreePersons.Controls.Add(this.chkUseAdditionalDates);
            this.panTreePersons.Controls.Add(this.chkExtendedTree);
            this.panTreePersons.Controls.Add(this.chkUseInlineImagesInSvg);
            this.panTreePersons.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panTreePersons.Location = new System.Drawing.Point(0, 0);
            this.panTreePersons.Margin = new System.Windows.Forms.Padding(10);
            this.panTreePersons.Name = "panTreePersons";
            this.panTreePersons.Padding = new System.Windows.Forms.Padding(10);
            this.panTreePersons.Size = new System.Drawing.Size(391, 678);
            this.panTreePersons.TabIndex = 0;
            this.panTreePersons.TabStop = false;
            this.panTreePersons.Text = "panTreePersons";
            // 
            // chkSurname
            //
            this.chkSurname.AutoSize = true;
            this.chkSurname.Location = new System.Drawing.Point(6, 6);
            this.chkSurname.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkSurname.Name = "chkSurname";
            this.chkSurname.Size = new System.Drawing.Size(349, 21);
            this.chkSurname.TabIndex = 0;
            this.chkSurname.Text = "chkSurname";
            this.chkSurname.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkTreeSurnameFirst
            // 
            this.chkTreeSurnameFirst.AutoSize = true;
            this.chkTreeSurnameFirst.Location = new System.Drawing.Point(26, 32);
            this.chkTreeSurnameFirst.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkTreeSurnameFirst.Name = "chkTreeSurnameFirst";
            this.chkTreeSurnameFirst.Size = new System.Drawing.Size(349, 21);
            this.chkTreeSurnameFirst.TabIndex = 1;
            this.chkTreeSurnameFirst.Text = "chkTreeSurnameFirst";
            this.chkTreeSurnameFirst.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkName
            // 
            this.chkName.AutoSize = true;
            this.chkName.Location = new System.Drawing.Point(6, 59);
            this.chkName.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkName.Name = "chkName";
            this.chkName.Size = new System.Drawing.Size(349, 21);
            this.chkName.TabIndex = 2;
            this.chkName.Text = "chkName";
            this.chkName.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkPatronymic
            // 
            this.chkPatronymic.AutoSize = true;
            this.chkPatronymic.Location = new System.Drawing.Point(6, 85);
            this.chkPatronymic.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkPatronymic.Name = "chkPatronymic";
            this.chkPatronymic.Size = new System.Drawing.Size(349, 21);
            this.chkPatronymic.TabIndex = 3;
            this.chkPatronymic.Text = "chkPatronymic";
            this.chkPatronymic.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkDiffLines
            // 
            this.chkDiffLines.AutoSize = true;
            this.chkDiffLines.Location = new System.Drawing.Point(26, 110);
            this.chkDiffLines.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkDiffLines.Name = "chkDiffLines";
            this.chkDiffLines.Size = new System.Drawing.Size(341, 20);
            this.chkDiffLines.TabIndex = 4;
            this.chkDiffLines.Text = "chkDiffLines";
            this.chkDiffLines.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkBirthDate
            // 
            this.chkBirthDate.AutoSize = true;
            this.chkBirthDate.Location = new System.Drawing.Point(6, 136);
            this.chkBirthDate.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkBirthDate.Name = "chkBirthDate";
            this.chkBirthDate.Size = new System.Drawing.Size(349, 21);
            this.chkBirthDate.TabIndex = 5;
            this.chkBirthDate.Text = "chkBirthDate";
            this.chkBirthDate.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkDeathDate
            // 
            this.chkDeathDate.AutoSize = true;
            this.chkDeathDate.Location = new System.Drawing.Point(6, 161);
            this.chkDeathDate.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkDeathDate.Name = "chkDeathDate";
            this.chkDeathDate.Size = new System.Drawing.Size(349, 20);
            this.chkDeathDate.TabIndex = 6;
            this.chkDeathDate.Text = "chkDeathDate";
            this.chkDeathDate.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkOnlyYears
            // 
            this.chkOnlyYears.AutoSize = true;
            this.chkOnlyYears.Location = new System.Drawing.Point(26, 188);
            this.chkOnlyYears.Margin = new System.Windows.Forms.Padding(30, 0, 0, 5);
            this.chkOnlyYears.Name = "chkOnlyYears";
            this.chkOnlyYears.Size = new System.Drawing.Size(326, 21);
            this.chkOnlyYears.TabIndex = 7;
            this.chkOnlyYears.Text = "chkOnlyYears";
            this.chkOnlyYears.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkShowAge
            // 
            this.chkShowAge.AutoSize = true;
            this.chkShowAge.Location = new System.Drawing.Point(26, 214);
            this.chkShowAge.Margin = new System.Windows.Forms.Padding(30, 0, 0, 5);
            this.chkShowAge.Name = "chkShowAge";
            this.chkShowAge.Size = new System.Drawing.Size(326, 21);
            this.chkShowAge.TabIndex = 8;
            this.chkShowAge.Text = "chkShowAge";
            this.chkShowAge.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkMarriagesDates
            // 
            this.chkMarriagesDates.AutoSize = true;
            this.chkMarriagesDates.Location = new System.Drawing.Point(6, 239);
            this.chkMarriagesDates.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkMarriagesDates.Name = "chkMarriagesDates";
            this.chkMarriagesDates.Size = new System.Drawing.Size(349, 20);
            this.chkMarriagesDates.TabIndex = 9;
            this.chkMarriagesDates.Text = "chkMarriagesDates";
            this.chkMarriagesDates.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkKinship
            // 
            this.chkKinship.AutoSize = true;
            this.chkKinship.Location = new System.Drawing.Point(6, 265);
            this.chkKinship.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkKinship.Name = "chkKinship";
            this.chkKinship.Size = new System.Drawing.Size(349, 21);
            this.chkKinship.TabIndex = 10;
            this.chkKinship.Text = "chkKinship";
            this.chkKinship.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkSignsVisible
            // 
            this.chkSignsVisible.AutoSize = true;
            this.chkSignsVisible.Location = new System.Drawing.Point(6, 291);
            this.chkSignsVisible.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkSignsVisible.Name = "chkSignsVisible";
            this.chkSignsVisible.Size = new System.Drawing.Size(349, 21);
            this.chkSignsVisible.TabIndex = 11;
            this.chkSignsVisible.Text = "chkSignsVisible";
            this.chkSignsVisible.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkTreeDecorative
            // 
            this.chkTreeDecorative.AutoSize = true;
            this.chkTreeDecorative.Location = new System.Drawing.Point(6, 318);
            this.chkTreeDecorative.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkTreeDecorative.Name = "chkTreeDecorative";
            this.chkTreeDecorative.Size = new System.Drawing.Size(349, 21);
            this.chkTreeDecorative.TabIndex = 12;
            this.chkTreeDecorative.Text = "chkTreeDecorative";
            this.chkTreeDecorative.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkPortraitsVisible
            // 
            this.chkPortraitsVisible.AutoSize = true;
            this.chkPortraitsVisible.Location = new System.Drawing.Point(6, 342);
            this.chkPortraitsVisible.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkPortraitsVisible.Name = "chkPortraitsVisible";
            this.chkPortraitsVisible.Size = new System.Drawing.Size(349, 20);
            this.chkPortraitsVisible.TabIndex = 13;
            this.chkPortraitsVisible.Text = "chkPortraitsVisible";
            this.chkPortraitsVisible.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkDefaultPortraits
            // 
            this.chkDefaultPortraits.AutoSize = true;
            this.chkDefaultPortraits.Location = new System.Drawing.Point(26, 369);
            this.chkDefaultPortraits.Margin = new System.Windows.Forms.Padding(30, 0, 0, 5);
            this.chkDefaultPortraits.Name = "chkDefaultPortraits";
            this.chkDefaultPortraits.Size = new System.Drawing.Size(326, 21);
            this.chkDefaultPortraits.TabIndex = 14;
            this.chkDefaultPortraits.Text = "chkDefaultPortraits";
            this.chkDefaultPortraits.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkInvertedTree
            // 
            this.chkInvertedTree.AutoSize = true;
            this.chkInvertedTree.Location = new System.Drawing.Point(6, 395);
            this.chkInvertedTree.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkInvertedTree.Name = "chkInvertedTree";
            this.chkInvertedTree.Size = new System.Drawing.Size(349, 21);
            this.chkInvertedTree.TabIndex = 15;
            this.chkInvertedTree.Text = "chkInvertedTree";
            this.chkInvertedTree.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkChildlessExclude
            // 
            this.chkChildlessExclude.AutoSize = true;
            this.chkChildlessExclude.Location = new System.Drawing.Point(6, 421);
            this.chkChildlessExclude.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkChildlessExclude.Name = "chkChildlessExclude";
            this.chkChildlessExclude.Size = new System.Drawing.Size(349, 21);
            this.chkChildlessExclude.TabIndex = 16;
            this.chkChildlessExclude.Text = "chkChildlessExclude";
            this.chkChildlessExclude.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkShowPlaces
            // 
            this.chkShowPlaces.AutoSize = true;
            this.chkShowPlaces.Location = new System.Drawing.Point(6, 448);
            this.chkShowPlaces.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkShowPlaces.Name = "chkShowPlaces";
            this.chkShowPlaces.Size = new System.Drawing.Size(349, 21);
            this.chkShowPlaces.TabIndex = 17;
            this.chkShowPlaces.Text = "chkShowPlaces";
            this.chkShowPlaces.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkSeparateDAPLines
            // 
            this.chkSeparateDAPLines.AutoSize = true;
            this.chkSeparateDAPLines.Location = new System.Drawing.Point(26, 474);
            this.chkSeparateDAPLines.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkSeparateDAPLines.Name = "chkSeparateDAPLines";
            this.chkSeparateDAPLines.Size = new System.Drawing.Size(349, 21);
            this.chkSeparateDAPLines.TabIndex = 18;
            this.chkSeparateDAPLines.Text = "chkSeparateDAPLines";
            this.chkSeparateDAPLines.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkOnlyLocality
            // 
            this.chkOnlyLocality.AutoSize = true;
            this.chkOnlyLocality.Location = new System.Drawing.Point(26, 499);
            this.chkOnlyLocality.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkOnlyLocality.Name = "chkOnlyLocality";
            this.chkOnlyLocality.Size = new System.Drawing.Size(349, 21);
            this.chkOnlyLocality.TabIndex = 19;
            this.chkOnlyLocality.Text = "chkOnlyLocality";
            this.chkOnlyLocality.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkHideUnknownSpouses
            // 
            this.chkHideUnknownSpouses.AutoSize = true;
            this.chkHideUnknownSpouses.Location = new System.Drawing.Point(6, 525);
            this.chkHideUnknownSpouses.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkHideUnknownSpouses.Name = "chkHideUnknownSpouses";
            this.chkHideUnknownSpouses.Size = new System.Drawing.Size(349, 21);
            this.chkHideUnknownSpouses.TabIndex = 20;
            this.chkHideUnknownSpouses.Text = "chkHideUnknownSpouses";
            this.chkHideUnknownSpouses.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkCheckTreeSize
            // 
            this.chkCheckTreeSize.AutoSize = true;
            this.chkCheckTreeSize.Location = new System.Drawing.Point(6, 551);
            this.chkCheckTreeSize.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkCheckTreeSize.Name = "chkCheckTreeSize";
            this.chkCheckTreeSize.Size = new System.Drawing.Size(349, 21);
            this.chkCheckTreeSize.TabIndex = 21;
            this.chkCheckTreeSize.Text = "chkCheckTreeSize";
            this.chkCheckTreeSize.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkDottedLinesOfAdoptedChildren
            // 
            this.chkDottedLinesOfAdoptedChildren.AutoSize = true;
            this.chkDottedLinesOfAdoptedChildren.Location = new System.Drawing.Point(6, 578);
            this.chkDottedLinesOfAdoptedChildren.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkDottedLinesOfAdoptedChildren.Name = "chkDottedLinesOfAdoptedChildren";
            this.chkDottedLinesOfAdoptedChildren.Size = new System.Drawing.Size(349, 21);
            this.chkDottedLinesOfAdoptedChildren.TabIndex = 22;
            this.chkDottedLinesOfAdoptedChildren.Text = "chkDottedLinesOfAdoptedChildren";
            this.chkDottedLinesOfAdoptedChildren.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkDottedLinesOfDivorcedSpouses
            // 
            this.chkDottedLinesOfDivorcedSpouses.AutoSize = true;
            this.chkDottedLinesOfDivorcedSpouses.Location = new System.Drawing.Point(6, 605);
            this.chkDottedLinesOfDivorcedSpouses.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkDottedLinesOfDivorcedSpouses.Name = "chkDottedLinesOfDivorcedSpouses";
            this.chkDottedLinesOfDivorcedSpouses.Size = new System.Drawing.Size(349, 21);
            this.chkDottedLinesOfDivorcedSpouses.TabIndex = 23;
            this.chkDottedLinesOfDivorcedSpouses.Text = "chkDottedLinesOfDivorcedSpouses";
            this.chkDottedLinesOfDivorcedSpouses.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkBoldNames
            // 
            this.chkBoldNames.AutoSize = true;
            this.chkBoldNames.Location = new System.Drawing.Point(6, 632);
            this.chkBoldNames.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkBoldNames.Name = "chkBoldNames";
            this.chkBoldNames.Size = new System.Drawing.Size(349, 21);
            this.chkBoldNames.TabIndex = 24;
            this.chkBoldNames.Text = "chkBoldNames";
            this.chkBoldNames.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkMinimizingWidth
            // 
            this.chkMinimizingWidth.AutoSize = true;
            this.chkMinimizingWidth.Location = new System.Drawing.Point(6, 659);
            this.chkMinimizingWidth.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkMinimizingWidth.Name = "chkMinimizingWidth";
            this.chkMinimizingWidth.Size = new System.Drawing.Size(349, 21);
            this.chkMinimizingWidth.TabIndex = 25;
            this.chkMinimizingWidth.Text = "chkMinimizingWidth";
            this.chkMinimizingWidth.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkURNotesVisible
            // 
            this.chkURNotesVisible.AutoSize = true;
            this.chkURNotesVisible.Location = new System.Drawing.Point(6, 686);
            this.chkURNotesVisible.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkURNotesVisible.Name = "chkURNotesVisible";
            this.chkURNotesVisible.Size = new System.Drawing.Size(349, 21);
            this.chkURNotesVisible.TabIndex = 26;
            this.chkURNotesVisible.Text = "chkURNotesVisible";
            this.chkURNotesVisible.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkSameCardsWidth
            // 
            this.chkSameCardsWidth.AutoSize = true;
            this.chkSameCardsWidth.Location = new System.Drawing.Point(6, 713);
            this.chkSameCardsWidth.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkSameCardsWidth.Name = "chkSameCardsWidth";
            this.chkSameCardsWidth.Size = new System.Drawing.Size(349, 21);
            this.chkSameCardsWidth.TabIndex = 27;
            this.chkSameCardsWidth.Text = "chkSameCardsWidth";
            this.chkSameCardsWidth.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkFullNameOnOneLine
            // 
            this.chkFullNameOnOneLine.AutoSize = true;
            this.chkFullNameOnOneLine.Location = new System.Drawing.Point(6, 740);
            this.chkFullNameOnOneLine.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkFullNameOnOneLine.Name = "chkFullNameOnOneLine";
            this.chkFullNameOnOneLine.Size = new System.Drawing.Size(349, 21);
            this.chkFullNameOnOneLine.TabIndex = 28;
            this.chkFullNameOnOneLine.Text = "chkFullNameOnOneLine";
            this.chkFullNameOnOneLine.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkDateDesignations
            // 
            this.chkDateDesignations.AutoSize = true;
            this.chkDateDesignations.Location = new System.Drawing.Point(6, 767);
            this.chkDateDesignations.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkDateDesignations.Name = "chkDateDesignations";
            this.chkDateDesignations.Size = new System.Drawing.Size(349, 21);
            this.chkDateDesignations.TabIndex = 29;
            this.chkDateDesignations.Text = "chkDateDesignations";
            this.chkDateDesignations.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkMourningEdges
            // 
            this.chkMourningEdges.AutoSize = true;
            this.chkMourningEdges.Location = new System.Drawing.Point(6, 794);
            this.chkMourningEdges.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkMourningEdges.Name = "chkMourningEdges";
            this.chkMourningEdges.Size = new System.Drawing.Size(349, 21);
            this.chkMourningEdges.TabIndex = 30;
            this.chkMourningEdges.Text = "chkMourningEdges";
            this.chkMourningEdges.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkUseAdditionalDates
            // 
            this.chkUseAdditionalDates.AutoSize = true;
            this.chkUseAdditionalDates.Location = new System.Drawing.Point(6, 821);
            this.chkUseAdditionalDates.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkUseAdditionalDates.Name = "chkUseAdditionalDates";
            this.chkUseAdditionalDates.Size = new System.Drawing.Size(349, 21);
            this.chkUseAdditionalDates.TabIndex = 31;
            this.chkUseAdditionalDates.Text = "chkUseAdditionalDates";
            this.chkUseAdditionalDates.CheckedChanged += new System.EventHandler(this.chkTreeChartOption_CheckedChanged);
            // 
            // chkUseInlineImagesInSvg
            // 
            this.chkUseInlineImagesInSvg.Location = new System.Drawing.Point(6, 848);
            this.chkUseInlineImagesInSvg.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkUseInlineImagesInSvg.Name = "chkUseInlineImagesInSvg";
            this.chkUseInlineImagesInSvg.Size = new System.Drawing.Size(600, 22);
            this.chkUseInlineImagesInSvg.TabIndex = 12;
            this.chkUseInlineImagesInSvg.Text = "chkUseInlineImagesInSvg";
            // 
            // chkExtendedTree
            // 
            this.chkExtendedTree.Location = new System.Drawing.Point(6, 875);
            this.chkExtendedTree.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkExtendedTree.Name = "chkExtendedTree";
            this.chkExtendedTree.Size = new System.Drawing.Size(600, 22);
            this.chkExtendedTree.TabIndex = 12;
            this.chkExtendedTree.Text = "chkExtendedTree";
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
            this.grpTreeDecor.Location = new System.Drawing.Point(14, 12);
            this.grpTreeDecor.Margin = new System.Windows.Forms.Padding(2);
            this.grpTreeDecor.Name = "grpTreeDecor";
            this.grpTreeDecor.Padding = new System.Windows.Forms.Padding(10);
            this.grpTreeDecor.Size = new System.Drawing.Size(245, 260);
            this.grpTreeDecor.TabIndex = 1;
            this.grpTreeDecor.TabStop = false;
            this.grpTreeDecor.Text = "";
            // 
            // lblFont
            // 
            this.lblFont.Location = new System.Drawing.Point(12, 188);
            this.lblFont.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.lblFont.Name = "lblFont";
            this.lblFont.Size = new System.Drawing.Size(100, 20);
            this.lblFont.TabIndex = 0;
            this.lblFont.Text = "lblFont";
            // 
            // panMaleColor
            // 
            this.panMaleColor.BackColor = System.Drawing.SystemColors.Control;
            this.panMaleColor.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.panMaleColor.Controls.Add(this.lblMaleColor);
            this.panMaleColor.Cursor = System.Windows.Forms.Cursors.Hand;
            this.panMaleColor.Location = new System.Drawing.Point(12, 30);
            this.panMaleColor.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.panMaleColor.Name = "panMaleColor";
            this.panMaleColor.Size = new System.Drawing.Size(103, 32);
            this.panMaleColor.TabIndex = 0;
            // 
            // lblMaleColor
            // 
            this.lblMaleColor.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lblMaleColor.Location = new System.Drawing.Point(0, 0);
            this.lblMaleColor.Margin = new System.Windows.Forms.Padding(0);
            this.lblMaleColor.Name = "lblMaleColor";
            this.lblMaleColor.Size = new System.Drawing.Size(99, 28);
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
            this.panFemaleColor.Location = new System.Drawing.Point(126, 30);
            this.panFemaleColor.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.panFemaleColor.Name = "panFemaleColor";
            this.panFemaleColor.Size = new System.Drawing.Size(103, 32);
            this.panFemaleColor.TabIndex = 1;
            // 
            // lblFemaleColor
            // 
            this.lblFemaleColor.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lblFemaleColor.Location = new System.Drawing.Point(0, 0);
            this.lblFemaleColor.Margin = new System.Windows.Forms.Padding(5, 10, 10, 10);
            this.lblFemaleColor.Name = "lblFemaleColor";
            this.lblFemaleColor.Size = new System.Drawing.Size(99, 28);
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
            this.panUnkSexColor.Location = new System.Drawing.Point(12, 71);
            this.panUnkSexColor.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.panUnkSexColor.Name = "panUnkSexColor";
            this.panUnkSexColor.Size = new System.Drawing.Size(215, 32);
            this.panUnkSexColor.TabIndex = 2;
            // 
            // lblUnkSexColor
            // 
            this.lblUnkSexColor.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lblUnkSexColor.Location = new System.Drawing.Point(0, 0);
            this.lblUnkSexColor.Margin = new System.Windows.Forms.Padding(0);
            this.lblUnkSexColor.Name = "lblUnkSexColor";
            this.lblUnkSexColor.Size = new System.Drawing.Size(211, 28);
            this.lblUnkSexColor.TabIndex = 1;
            this.lblUnkSexColor.Text = "lblUnkSexColor";
            this.lblUnkSexColor.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            this.lblUnkSexColor.Click += new System.EventHandler(this.PanColor_Click);
            // 
            // panUnHusbandColor
            // 
            this.panUnHusbandColor.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.panUnHusbandColor.Controls.Add(this.lblUnHusbandColor);
            this.panUnHusbandColor.Cursor = System.Windows.Forms.Cursors.Hand;
            this.panUnHusbandColor.Location = new System.Drawing.Point(12, 112);
            this.panUnHusbandColor.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.panUnHusbandColor.Name = "panUnHusbandColor";
            this.panUnHusbandColor.Size = new System.Drawing.Size(215, 32);
            this.panUnHusbandColor.TabIndex = 3;
            // 
            // lblUnHusbandColor
            // 
            this.lblUnHusbandColor.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lblUnHusbandColor.Location = new System.Drawing.Point(0, 0);
            this.lblUnHusbandColor.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblUnHusbandColor.Name = "lblUnHusbandColor";
            this.lblUnHusbandColor.Size = new System.Drawing.Size(211, 28);
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
            this.panUnWifeColor.Location = new System.Drawing.Point(12, 152);
            this.panUnWifeColor.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.panUnWifeColor.Name = "panUnWifeColor";
            this.panUnWifeColor.Size = new System.Drawing.Size(215, 30);
            this.panUnWifeColor.TabIndex = 4;
            // 
            // lblUnWifeColor
            // 
            this.lblUnWifeColor.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lblUnWifeColor.Location = new System.Drawing.Point(0, 0);
            this.lblUnWifeColor.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
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
            this.panDefFont.Location = new System.Drawing.Point(12, 212);
            this.panDefFont.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.panDefFont.Name = "panDefFont";
            this.panDefFont.Size = new System.Drawing.Size(215, 32);
            this.panDefFont.TabIndex = 5;
            this.panDefFont.Click += new System.EventHandler(this.panDefFont_Click);
            // 
            // lblChartFont
            // 
            this.lblChartFont.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lblChartFont.Location = new System.Drawing.Point(0, 0);
            this.lblChartFont.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblChartFont.Name = "lblChartFont";
            this.lblChartFont.Size = new System.Drawing.Size(211, 28);
            this.lblChartFont.TabIndex = 0;
            this.lblChartFont.Text = "lblChartFont";
            this.lblChartFont.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            this.lblChartFont.Click += new System.EventHandler(this.panDefFont_Click);
            // 
            // pageAncCircle
            // 
            this.pageAncCircle.BackColor = System.Drawing.SystemColors.Control;
            this.pageAncCircle.Controls.Add(this.ancOptionsControl1);
            this.pageAncCircle.Location = new System.Drawing.Point(4, 26);
            this.pageAncCircle.Margin = new System.Windows.Forms.Padding(2);
            this.pageAncCircle.Name = "pageAncCircle";
            this.pageAncCircle.Size = new System.Drawing.Size(713, 712);
            this.pageAncCircle.TabIndex = 4;
            this.pageAncCircle.Text = "pageAncCircle";
            // 
            // ancOptionsControl1
            // 
            this.ancOptionsControl1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ancOptionsControl1.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.ancOptionsControl1.Location = new System.Drawing.Point(0, 0);
            this.ancOptionsControl1.Margin = new System.Windows.Forms.Padding(2);
            this.ancOptionsControl1.Name = "ancOptionsControl1";
            this.ancOptionsControl1.Options = null;
            this.ancOptionsControl1.Size = new System.Drawing.Size(713, 712);
            this.ancOptionsControl1.TabIndex = 0;
            // 
            // pageUIView
            // 
            this.pageUIView.Controls.Add(this.PageControl2);
            this.pageUIView.Location = new System.Drawing.Point(4, 26);
            this.pageUIView.Margin = new System.Windows.Forms.Padding(2);
            this.pageUIView.Name = "pageUIView";
            this.pageUIView.Padding = new System.Windows.Forms.Padding(10);
            this.pageUIView.Size = new System.Drawing.Size(741, 762);
            this.pageUIView.TabIndex = 1;
            this.pageUIView.Text = "pageUIView";
            // 
            // PageControl2
            // 
            this.PageControl2.Controls.Add(this.pageViewCommon);
            this.PageControl2.Controls.Add(this.pageViewPersons);
            this.PageControl2.Controls.Add(this.pageNavigation);
            this.PageControl2.Controls.Add(this.pageGeo);
            this.PageControl2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.PageControl2.Location = new System.Drawing.Point(10, 10);
            this.PageControl2.Margin = new System.Windows.Forms.Padding(2);
            this.PageControl2.Name = "PageControl2";
            this.PageControl2.SelectedIndex = 0;
            this.PageControl2.Size = new System.Drawing.Size(721, 742);
            this.PageControl2.TabIndex = 0;
            // 
            // pageViewCommon
            // 
            this.pageViewCommon.Controls.Add(this.chkSurnameFirstInOrder);
            this.pageViewCommon.Controls.Add(this.grpAdvancedNames);
            this.pageViewCommon.Controls.Add(this.rgFNPFormat);
            this.pageViewCommon.Controls.Add(this.grpDateFormat);
            this.pageViewCommon.Controls.Add(this.chkPlacesWithAddress);
            this.pageViewCommon.Controls.Add(this.chkHighlightUnparented);
            this.pageViewCommon.Controls.Add(this.chkLocalizedCalendarSignatures);
            this.pageViewCommon.Controls.Add(this.chkShowDatesSigns);
            this.pageViewCommon.Controls.Add(this.chkShowDatesCalendar);
            this.pageViewCommon.Controls.Add(this.chkSurnameInCapitals);
            this.pageViewCommon.Controls.Add(this.chkFirstCapitalLetterInNames);
            this.pageViewCommon.Controls.Add(this.chkAutoSortSpouses);
            this.pageViewCommon.Controls.Add(this.chkShortKinshipForm);
            this.pageViewCommon.Controls.Add(this.chkAutoSortChildren);
            this.pageViewCommon.Controls.Add(this.chkHighlightUnmarried);
            this.pageViewCommon.Location = new System.Drawing.Point(4, 26);
            this.pageViewCommon.Margin = new System.Windows.Forms.Padding(2);
            this.pageViewCommon.Name = "pageViewCommon";
            this.pageViewCommon.Padding = new System.Windows.Forms.Padding(10);
            this.pageViewCommon.Size = new System.Drawing.Size(713, 712);
            this.pageViewCommon.TabIndex = 0;
            this.pageViewCommon.Text = "pageViewCommon";
            // 
            // chkSurnameFirstInOrder
            // 
            this.chkSurnameFirstInOrder.Location = new System.Drawing.Point(11, 132);
            this.chkSurnameFirstInOrder.Margin = new System.Windows.Forms.Padding(2);
            this.chkSurnameFirstInOrder.Name = "chkSurnameFirstInOrder";
            this.chkSurnameFirstInOrder.Size = new System.Drawing.Size(302, 21);
            this.chkSurnameFirstInOrder.TabIndex = 9;
            this.chkSurnameFirstInOrder.Text = "chkSurnameFirstInOrder";
            // 
            // grpAdvancedNames
            // 
            this.grpAdvancedNames.Controls.Add(this.radMarried);
            this.grpAdvancedNames.Controls.Add(this.radMaiden);
            this.grpAdvancedNames.Controls.Add(this.radMarried_Maiden);
            this.grpAdvancedNames.Controls.Add(this.radMaiden_Married);
            this.grpAdvancedNames.Controls.Add(this.chkExtendWomanSurnames);
            this.grpAdvancedNames.Controls.Add(this.chkSimpleSingleSurnames);
            this.grpAdvancedNames.Location = new System.Drawing.Point(326, 201);
            this.grpAdvancedNames.Margin = new System.Windows.Forms.Padding(10);
            this.grpAdvancedNames.Name = "grpAdvancedNames";
            this.grpAdvancedNames.Padding = new System.Windows.Forms.Padding(2);
            this.grpAdvancedNames.Size = new System.Drawing.Size(368, 230);
            this.grpAdvancedNames.TabIndex = 8;
            this.grpAdvancedNames.TabStop = false;
            this.grpAdvancedNames.Text = "AdvancedNames";
            // 
            // radMarried
            // 
            this.radMarried.AutoSize = true;
            this.radMarried.Location = new System.Drawing.Point(26, 157);
            this.radMarried.Margin = new System.Windows.Forms.Padding(10);
            this.radMarried.Name = "radMarried";
            this.radMarried.Size = new System.Drawing.Size(93, 21);
            this.radMarried.TabIndex = 12;
            this.radMarried.TabStop = true;
            this.radMarried.Text = "radMarried";
            this.radMarried.UseVisualStyleBackColor = true;
            // 
            // radMaiden
            // 
            this.radMaiden.AutoSize = true;
            this.radMaiden.Location = new System.Drawing.Point(26, 124);
            this.radMaiden.Margin = new System.Windows.Forms.Padding(10, 10, 10, 0);
            this.radMaiden.Name = "radMaiden";
            this.radMaiden.Size = new System.Drawing.Size(91, 21);
            this.radMaiden.TabIndex = 11;
            this.radMaiden.TabStop = true;
            this.radMaiden.Text = "radMaiden";
            this.radMaiden.UseVisualStyleBackColor = true;
            // 
            // radMarried_Maiden
            // 
            this.radMarried_Maiden.AutoSize = true;
            this.radMarried_Maiden.Location = new System.Drawing.Point(26, 91);
            this.radMarried_Maiden.Margin = new System.Windows.Forms.Padding(10, 10, 10, 0);
            this.radMarried_Maiden.Name = "radMarried_Maiden";
            this.radMarried_Maiden.Size = new System.Drawing.Size(143, 21);
            this.radMarried_Maiden.TabIndex = 10;
            this.radMarried_Maiden.TabStop = true;
            this.radMarried_Maiden.Text = "radMarried_Maiden";
            this.radMarried_Maiden.UseVisualStyleBackColor = true;
            // 
            // radMaiden_Married
            // 
            this.radMaiden_Married.AutoSize = true;
            this.radMaiden_Married.Location = new System.Drawing.Point(26, 58);
            this.radMaiden_Married.Margin = new System.Windows.Forms.Padding(10, 10, 10, 0);
            this.radMaiden_Married.Name = "radMaiden_Married";
            this.radMaiden_Married.Size = new System.Drawing.Size(143, 21);
            this.radMaiden_Married.TabIndex = 9;
            this.radMaiden_Married.TabStop = true;
            this.radMaiden_Married.Text = "radMaiden_Married";
            this.radMaiden_Married.UseVisualStyleBackColor = true;
            // 
            // chkExtendWomanSurnames
            // 
            this.chkExtendWomanSurnames.AutoSize = true;
            this.chkExtendWomanSurnames.Location = new System.Drawing.Point(12, 22);
            this.chkExtendWomanSurnames.Margin = new System.Windows.Forms.Padding(2);
            this.chkExtendWomanSurnames.Name = "chkExtendWomanSurnames";
            this.chkExtendWomanSurnames.Size = new System.Drawing.Size(184, 21);
            this.chkExtendWomanSurnames.TabIndex = 8;
            this.chkExtendWomanSurnames.Text = "ExtendWomanSurnames";
            this.chkExtendWomanSurnames.UseVisualStyleBackColor = true;
            this.chkExtendWomanSurnames.CheckedChanged += new System.EventHandler(this.chkExtendWomanSurnames_CheckedChanged);
            // 
            // chkSimpleSingleSurnames
            // 
            this.chkSimpleSingleSurnames.AutoSize = true;
            this.chkSimpleSingleSurnames.Location = new System.Drawing.Point(12, 190);
            this.chkSimpleSingleSurnames.Margin = new System.Windows.Forms.Padding(2);
            this.chkSimpleSingleSurnames.Name = "chkSimpleSingleSurnames";
            this.chkSimpleSingleSurnames.Size = new System.Drawing.Size(184, 21);
            this.chkSimpleSingleSurnames.TabIndex = 8;
            this.chkSimpleSingleSurnames.Text = "chkSimpleSingleSurnames";
            this.chkSimpleSingleSurnames.UseVisualStyleBackColor = true;
            // 
            // rgFNPFormat
            // 
            this.rgFNPFormat.Controls.Add(this.radS_N_P);
            this.rgFNPFormat.Controls.Add(this.radS_NP);
            this.rgFNPFormat.Controls.Add(this.radSNP);
            this.rgFNPFormat.Location = new System.Drawing.Point(11, 10);
            this.rgFNPFormat.Margin = new System.Windows.Forms.Padding(2);
            this.rgFNPFormat.Name = "rgFNPFormat";
            this.rgFNPFormat.Padding = new System.Windows.Forms.Padding(2);
            this.rgFNPFormat.Size = new System.Drawing.Size(259, 118);
            this.rgFNPFormat.TabIndex = 0;
            this.rgFNPFormat.TabStop = false;
            this.rgFNPFormat.Text = "rgFNPFormat";
            // 
            // radS_N_P
            // 
            this.radS_N_P.Location = new System.Drawing.Point(11, 78);
            this.radS_N_P.Margin = new System.Windows.Forms.Padding(2);
            this.radS_N_P.Name = "radS_N_P";
            this.radS_N_P.Size = new System.Drawing.Size(224, 29);
            this.radS_N_P.TabIndex = 2;
            this.radS_N_P.Text = "radS_N_P";
            this.radS_N_P.CheckedChanged += new System.EventHandler(this.rgFNPFormat_CheckedChanged);
            // 
            // radS_NP
            // 
            this.radS_NP.Location = new System.Drawing.Point(11, 49);
            this.radS_NP.Margin = new System.Windows.Forms.Padding(2);
            this.radS_NP.Name = "radS_NP";
            this.radS_NP.Size = new System.Drawing.Size(224, 29);
            this.radS_NP.TabIndex = 1;
            this.radS_NP.Text = "radS_NP";
            this.radS_NP.CheckedChanged += new System.EventHandler(this.rgFNPFormat_CheckedChanged);
            // 
            // radSNP
            // 
            this.radSNP.Location = new System.Drawing.Point(11, 21);
            this.radSNP.Margin = new System.Windows.Forms.Padding(2);
            this.radSNP.Name = "radSNP";
            this.radSNP.Size = new System.Drawing.Size(224, 29);
            this.radSNP.TabIndex = 0;
            this.radSNP.Text = "radSNP";
            this.radSNP.CheckedChanged += new System.EventHandler(this.rgFNPFormat_CheckedChanged);
            // 
            // grpDateFormat
            // 
            this.grpDateFormat.Controls.Add(this.radYMD);
            this.grpDateFormat.Controls.Add(this.radDMY);
            this.grpDateFormat.Location = new System.Drawing.Point(326, 10);
            this.grpDateFormat.Margin = new System.Windows.Forms.Padding(2);
            this.grpDateFormat.Name = "grpDateFormat";
            this.grpDateFormat.Padding = new System.Windows.Forms.Padding(2);
            this.grpDateFormat.Size = new System.Drawing.Size(259, 88);
            this.grpDateFormat.TabIndex = 1;
            this.grpDateFormat.TabStop = false;
            this.grpDateFormat.Text = "grpDateFormat";
            // 
            // radYMD
            // 
            this.radYMD.Location = new System.Drawing.Point(11, 49);
            this.radYMD.Margin = new System.Windows.Forms.Padding(2);
            this.radYMD.Name = "radYMD";
            this.radYMD.Size = new System.Drawing.Size(146, 29);
            this.radYMD.TabIndex = 1;
            this.radYMD.Text = "YYYY.MM.DD";
            // 
            // radDMY
            // 
            this.radDMY.Location = new System.Drawing.Point(11, 19);
            this.radDMY.Margin = new System.Windows.Forms.Padding(2);
            this.radDMY.Name = "radDMY";
            this.radDMY.Size = new System.Drawing.Size(146, 30);
            this.radDMY.TabIndex = 0;
            this.radDMY.Text = "DD.MM.YYYY";
            // 
            // chkPlacesWithAddress
            // 
            this.chkPlacesWithAddress.Location = new System.Drawing.Point(11, 191);
            this.chkPlacesWithAddress.Margin = new System.Windows.Forms.Padding(2);
            this.chkPlacesWithAddress.Name = "chkPlacesWithAddress";
            this.chkPlacesWithAddress.Size = new System.Drawing.Size(302, 21);
            this.chkPlacesWithAddress.TabIndex = 2;
            this.chkPlacesWithAddress.Text = "chkPlacesWithAddress";
            // 
            // chkHighlightUnparented
            // 
            this.chkHighlightUnparented.Location = new System.Drawing.Point(11, 220);
            this.chkHighlightUnparented.Margin = new System.Windows.Forms.Padding(2);
            this.chkHighlightUnparented.Name = "chkHighlightUnparented";
            this.chkHighlightUnparented.Size = new System.Drawing.Size(302, 21);
            this.chkHighlightUnparented.TabIndex = 3;
            this.chkHighlightUnparented.Text = "chkHighlightUnparented";
            // 
            // chkLocalizedCalendarSignatures
            // 
            this.chkLocalizedCalendarSignatures.Location = new System.Drawing.Point(326, 159);
            this.chkLocalizedCalendarSignatures.Margin = new System.Windows.Forms.Padding(2);
            this.chkLocalizedCalendarSignatures.Name = "chkLocalizedCalendarSignatures";
            this.chkLocalizedCalendarSignatures.Size = new System.Drawing.Size(338, 21);
            this.chkLocalizedCalendarSignatures.TabIndex = 4;
            this.chkLocalizedCalendarSignatures.Text = "chkLocalizedCalendarSignatures";
            // 
            // chkShowDatesSigns
            // 
            this.chkShowDatesSigns.Location = new System.Drawing.Point(326, 134);
            this.chkShowDatesSigns.Margin = new System.Windows.Forms.Padding(2);
            this.chkShowDatesSigns.Name = "chkShowDatesSigns";
            this.chkShowDatesSigns.Size = new System.Drawing.Size(338, 21);
            this.chkShowDatesSigns.TabIndex = 4;
            this.chkShowDatesSigns.Text = "chkShowDatesSigns";
            // 
            // chkShowDatesCalendar
            // 
            this.chkShowDatesCalendar.Location = new System.Drawing.Point(326, 108);
            this.chkShowDatesCalendar.Margin = new System.Windows.Forms.Padding(2);
            this.chkShowDatesCalendar.Name = "chkShowDatesCalendar";
            this.chkShowDatesCalendar.Size = new System.Drawing.Size(338, 21);
            this.chkShowDatesCalendar.TabIndex = 4;
            this.chkShowDatesCalendar.Text = "chkShowDatesCalendar";
            // 
            // chkHighlightUnmarried
            // 
            this.chkHighlightUnmarried.Location = new System.Drawing.Point(11, 249);
            this.chkHighlightUnmarried.Margin = new System.Windows.Forms.Padding(2);
            this.chkHighlightUnmarried.Name = "chkHighlightUnmarried";
            this.chkHighlightUnmarried.Size = new System.Drawing.Size(302, 21);
            this.chkHighlightUnmarried.TabIndex = 4;
            this.chkHighlightUnmarried.Text = "chkHighlightUnmarried";
            // 
            // chkAutoSortChildren
            // 
            this.chkAutoSortChildren.Location = new System.Drawing.Point(11, 300);
            this.chkAutoSortChildren.Margin = new System.Windows.Forms.Padding(2);
            this.chkAutoSortChildren.Name = "chkAutoSortChildren";
            this.chkAutoSortChildren.Size = new System.Drawing.Size(302, 21);
            this.chkAutoSortChildren.TabIndex = 4;
            this.chkAutoSortChildren.Text = "chkAutoSortChildren";
            // 
            // chkAutoSortSpouses
            // 
            this.chkAutoSortSpouses.Location = new System.Drawing.Point(11, 326);
            this.chkAutoSortSpouses.Margin = new System.Windows.Forms.Padding(2);
            this.chkAutoSortSpouses.Name = "chkAutoSortSpouses";
            this.chkAutoSortSpouses.Size = new System.Drawing.Size(302, 21);
            this.chkAutoSortSpouses.TabIndex = 4;
            this.chkAutoSortSpouses.Text = "chkAutoSortSpouses";
            // 
            // chkFirstCapitalLetterInNames
            // 
            this.chkFirstCapitalLetterInNames.Location = new System.Drawing.Point(11, 352);
            this.chkFirstCapitalLetterInNames.Margin = new System.Windows.Forms.Padding(2);
            this.chkFirstCapitalLetterInNames.Name = "chkFirstCapitalLetterInNames";
            this.chkFirstCapitalLetterInNames.Size = new System.Drawing.Size(302, 21);
            this.chkFirstCapitalLetterInNames.TabIndex = 4;
            this.chkFirstCapitalLetterInNames.Text = "chkFirstCapitalLetterInNames";
            // 
            // chkSurnameInCapitals
            // 
            this.chkSurnameInCapitals.Location = new System.Drawing.Point(11, 378);
            this.chkSurnameInCapitals.Margin = new System.Windows.Forms.Padding(2);
            this.chkSurnameInCapitals.Name = "chkSurnameInCapitals";
            this.chkSurnameInCapitals.Size = new System.Drawing.Size(302, 21);
            this.chkSurnameInCapitals.TabIndex = 4;
            this.chkSurnameInCapitals.Text = "chkSurnameInCapitals";
            // 
            // chkShortKinshipForm
            // 
            this.chkShortKinshipForm.Location = new System.Drawing.Point(11, 422);
            this.chkShortKinshipForm.Margin = new System.Windows.Forms.Padding(2);
            this.chkShortKinshipForm.Name = "chkShortKinshipForm";
            this.chkShortKinshipForm.Size = new System.Drawing.Size(302, 21);
            this.chkShortKinshipForm.TabIndex = 4;
            this.chkShortKinshipForm.Text = "chkShortKinshipForm";
            // 
            // pageViewPersons
            // 
            this.pageViewPersons.Controls.Add(this.panel1);
            this.pageViewPersons.Controls.Add(this.btnColumnUp);
            this.pageViewPersons.Controls.Add(this.btnColumnDown);
            this.pageViewPersons.Location = new System.Drawing.Point(4, 26);
            this.pageViewPersons.Margin = new System.Windows.Forms.Padding(2);
            this.pageViewPersons.Name = "pageViewPersons";
            this.pageViewPersons.Size = new System.Drawing.Size(713, 712);
            this.pageViewPersons.TabIndex = 1;
            this.pageViewPersons.Text = "pageViewPersons";
            // 
            // chkUseSurnamesInPSF
            // 
            this.chkUseSurnamesInPSF.Location = new System.Drawing.Point(11, 8);
            this.chkUseSurnamesInPSF.Margin = new System.Windows.Forms.Padding(2);
            this.chkUseSurnamesInPSF.Name = "chkUseSurnamesInPSF";
            this.chkUseSurnamesInPSF.Size = new System.Drawing.Size(500, 21);
            this.chkUseSurnamesInPSF.TabIndex = 4;
            this.chkUseSurnamesInPSF.Text = "chkUseSurnamesInPSF";
            // 
            // chkUseBirthDatesInPSF
            // 
            this.chkUseBirthDatesInPSF.Location = new System.Drawing.Point(11, 34);
            this.chkUseBirthDatesInPSF.Margin = new System.Windows.Forms.Padding(2);
            this.chkUseBirthDatesInPSF.Name = "chkUseBirthDatesInPSF";
            this.chkUseBirthDatesInPSF.Size = new System.Drawing.Size(500, 21);
            this.chkUseBirthDatesInPSF.TabIndex = 4;
            this.chkUseBirthDatesInPSF.Text = "chkUseBirthDatesInPSF";
            // 
            // lblMatchPatternMethod
            // 
            this.lblMatchPatternMethod.AutoSize = true;
            this.lblMatchPatternMethod.Location = new System.Drawing.Point(11, 68);
            this.lblMatchPatternMethod.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblMatchPatternMethod.Name = "lblMatchPatternMethod";
            this.lblMatchPatternMethod.Size = new System.Drawing.Size(175, 21);
            this.lblMatchPatternMethod.TabIndex = 7;
            this.lblMatchPatternMethod.Text = "lblMatchPatternMethod";
            // 
            // cmbMatchPatternMethod
            // 
            this.cmbMatchPatternMethod.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbMatchPatternMethod.Location = new System.Drawing.Point(233, 66);
            this.cmbMatchPatternMethod.Margin = new System.Windows.Forms.Padding(2);
            this.cmbMatchPatternMethod.Name = "cmbMatchPatternMethod";
            this.cmbMatchPatternMethod.Size = new System.Drawing.Size(230, 25);
            this.cmbMatchPatternMethod.TabIndex = 8;
            // 
            // chkSAFByAllNames
            // 
            this.chkSAFByAllNames.Location = new System.Drawing.Point(11, 102);
            this.chkSAFByAllNames.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkSAFByAllNames.Name = "chkSAFByAllNames";
            this.chkSAFByAllNames.Size = new System.Drawing.Size(600, 22);
            this.chkSAFByAllNames.TabIndex = 9;
            this.chkSAFByAllNames.Text = "chkSAFByAllNames";
            // 
            // pageNavigation
            // 
            this.pageNavigation.Controls.Add(this.chkUseSurnamesInPSF);
            this.pageNavigation.Controls.Add(this.chkUseBirthDatesInPSF);
            this.pageNavigation.Controls.Add(this.lblMatchPatternMethod);
            this.pageNavigation.Controls.Add(this.cmbMatchPatternMethod);
            this.pageNavigation.Controls.Add(this.chkSAFByAllNames);
            this.pageNavigation.Location = new System.Drawing.Point(4, 26);
            this.pageNavigation.Margin = new System.Windows.Forms.Padding(2);
            this.pageNavigation.Name = "pageNavigation";
            this.pageNavigation.Size = new System.Drawing.Size(713, 712);
            this.pageNavigation.TabIndex = 1;
            this.pageNavigation.Text = "pageNavigation";
            // 
            // pageGeo
            // 
            this.pageGeo.Controls.Add(this.lblGeoSearchCountry);
            this.pageGeo.Controls.Add(this.lblGeocoder);
            this.pageGeo.Controls.Add(this.cmbGeoSearchCountry);
            this.pageGeo.Controls.Add(this.cmbGeocoder);
            this.pageGeo.Controls.Add(this.chkExtendedLocations);
            this.pageGeo.Controls.Add(this.chkELAbbreviatedNames);
            this.pageGeo.Controls.Add(this.chkReversePlacesOrder);
            this.pageGeo.Controls.Add(this.chkSearchPlacesWithoutCoords);
            this.pageGeo.Location = new System.Drawing.Point(4, 26);
            this.pageGeo.Margin = new System.Windows.Forms.Padding(2);
            this.pageGeo.Name = "pageGeo";
            this.pageGeo.Size = new System.Drawing.Size(713, 712);
            this.pageGeo.TabIndex = 1;
            this.pageGeo.Text = "pageGeo";
            // 
            // lblGeocoder
            // 
            this.lblGeocoder.AutoSize = true;
            this.lblGeocoder.Location = new System.Drawing.Point(11, 8);
            this.lblGeocoder.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblGeocoder.Name = "lblGeocoder";
            this.lblGeocoder.Size = new System.Drawing.Size(79, 17);
            this.lblGeocoder.TabIndex = 0;
            this.lblGeocoder.Text = "lblGeocoder";
            // 
            // cmbGeocoder
            // 
            this.cmbGeocoder.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbGeocoder.Items.AddRange(new object[] {
            "Google",
            "Yandex",
            "OSM"});
            this.cmbGeocoder.Location = new System.Drawing.Point(300, 8);
            this.cmbGeocoder.Margin = new System.Windows.Forms.Padding(2);
            this.cmbGeocoder.Name = "cmbGeocoder";
            this.cmbGeocoder.Size = new System.Drawing.Size(230, 25);
            this.cmbGeocoder.TabIndex = 4;
            // 
            // lblGeoSearchCountry
            // 
            this.lblGeoSearchCountry.AutoSize = true;
            this.lblGeoSearchCountry.Location = new System.Drawing.Point(11, 34);
            this.lblGeoSearchCountry.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblGeoSearchCountry.Name = "lblGeoSearchCountry";
            this.lblGeoSearchCountry.Size = new System.Drawing.Size(216, 17);
            this.lblGeoSearchCountry.TabIndex = 0;
            this.lblGeoSearchCountry.Text = "Restriction geo search by country";
            // 
            // cmbGeoSearchCountry
            // 
            this.cmbGeoSearchCountry.Items.AddRange(new object[] {
            "Google",
            "Yandex",
            "OSM"});
            this.cmbGeoSearchCountry.Location = new System.Drawing.Point(300, 34);
            this.cmbGeoSearchCountry.Margin = new System.Windows.Forms.Padding(2);
            this.cmbGeoSearchCountry.Name = "cmbGeoSearchCountry";
            this.cmbGeoSearchCountry.Size = new System.Drawing.Size(138, 25);
            this.cmbGeoSearchCountry.TabIndex = 4;
            // 
            // chkExtendedLocations
            // 
            this.chkExtendedLocations.Location = new System.Drawing.Point(11, 70);
            this.chkExtendedLocations.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkExtendedLocations.Name = "chkExtendedLocations";
            this.chkExtendedLocations.Size = new System.Drawing.Size(600, 22);
            this.chkExtendedLocations.TabIndex = 18;
            this.chkExtendedLocations.Text = "chkExtendedLocations";
            // 
            // chkELAbbreviatedNames
            // 
            this.chkELAbbreviatedNames.Location = new System.Drawing.Point(41, 96);
            this.chkELAbbreviatedNames.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkELAbbreviatedNames.Name = "chkELAbbreviatedNames";
            this.chkELAbbreviatedNames.Size = new System.Drawing.Size(600, 22);
            this.chkELAbbreviatedNames.TabIndex = 18;
            this.chkELAbbreviatedNames.Text = "chkELAbbreviatedNames";
            // 
            // chkReversePlacesOrder
            // 
            this.chkReversePlacesOrder.Location = new System.Drawing.Point(11, 122);
            this.chkReversePlacesOrder.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkReversePlacesOrder.Name = "chkReversePlacesOrder";
            this.chkReversePlacesOrder.Size = new System.Drawing.Size(600, 22);
            this.chkReversePlacesOrder.TabIndex = 19;
            this.chkReversePlacesOrder.Text = "chkReversePlacesOrder";
            // 
            // chkSearchPlacesWithoutCoords
            // 
            this.chkSearchPlacesWithoutCoords.Location = new System.Drawing.Point(11, 148);
            this.chkSearchPlacesWithoutCoords.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkSearchPlacesWithoutCoords.Name = "chkSearchPlacesWithoutCoords";
            this.chkSearchPlacesWithoutCoords.Size = new System.Drawing.Size(600, 22);
            this.chkSearchPlacesWithoutCoords.TabIndex = 20;
            this.chkSearchPlacesWithoutCoords.Text = "chkSearchPlacesWithoutCoords";
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.lstPersonColumns);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Left;
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Margin = new System.Windows.Forms.Padding(2);
            this.panel1.Name = "panel1";
            this.panel1.Padding = new System.Windows.Forms.Padding(10);
            this.panel1.Size = new System.Drawing.Size(488, 712);
            this.panel1.TabIndex = 2;
            // 
            // lstPersonColumns
            // 
            this.lstPersonColumns.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lstPersonColumns.FullRowSelect = true;
            this.lstPersonColumns.HideSelection = false;
            this.lstPersonColumns.ListMan = null;
            this.lstPersonColumns.Location = new System.Drawing.Point(10, 10);
            this.lstPersonColumns.Margin = new System.Windows.Forms.Padding(2);
            this.lstPersonColumns.Name = "lstPersonColumns";
            this.lstPersonColumns.OwnerDraw = true;
            this.lstPersonColumns.SelectedIndex = -1;
            this.lstPersonColumns.Size = new System.Drawing.Size(468, 692);
            this.lstPersonColumns.SortColumn = 0;
            this.lstPersonColumns.SortOrder = GKCore.Design.BSDTypes.SortOrder.None;
            this.lstPersonColumns.TabIndex = 1;
            this.lstPersonColumns.UseCompatibleStateImageBehavior = false;
            this.lstPersonColumns.View = System.Windows.Forms.View.Details;
            // 
            // btnColumnUp
            // 
            this.btnColumnUp.Location = new System.Drawing.Point(498, 10);
            this.btnColumnUp.Margin = new System.Windows.Forms.Padding(10, 10, 10, 0);
            this.btnColumnUp.Name = "btnColumnUp";
            this.btnColumnUp.Size = new System.Drawing.Size(39, 34);
            this.btnColumnUp.TabIndex = 0;
            this.btnColumnUp.Click += new System.EventHandler(this.btnColumnUp_Click);
            // 
            // btnColumnDown
            // 
            this.btnColumnDown.Location = new System.Drawing.Point(498, 54);
            this.btnColumnDown.Margin = new System.Windows.Forms.Padding(10);
            this.btnColumnDown.Name = "btnColumnDown";
            this.btnColumnDown.Size = new System.Drawing.Size(39, 34);
            this.btnColumnDown.TabIndex = 1;
            this.btnColumnDown.Click += new System.EventHandler(this.btnColumnDown_Click);
            // 
            // btnResetDefaults
            // 
            this.btnResetDefaults.Location = new System.Drawing.Point(14, 702);
            this.btnResetDefaults.Margin = new System.Windows.Forms.Padding(10);
            this.btnResetDefaults.Name = "btnResetDefaults";
            this.btnResetDefaults.Size = new System.Drawing.Size(212, 30);
            this.btnResetDefaults.TabIndex = 1;
            this.btnResetDefaults.Text = "btnResetDefaults";
            this.btnResetDefaults.Click += new System.EventHandler(this.btnResetDefaults_Click);
            // 
            // pagePedigree
            // 
            this.pagePedigree.Controls.Add(this.grpPedigree);
            this.pagePedigree.Location = new System.Drawing.Point(4, 26);
            this.pagePedigree.Margin = new System.Windows.Forms.Padding(2);
            this.pagePedigree.Name = "pagePedigree";
            this.pagePedigree.Padding = new System.Windows.Forms.Padding(10);
            this.pagePedigree.Size = new System.Drawing.Size(741, 762);
            this.pagePedigree.TabIndex = 3;
            this.pagePedigree.Text = "pagePedigree";
            // 
            // grpPedigree
            // 
            this.grpPedigree.Controls.Add(this.lblDescendNumbering);
            this.grpPedigree.Controls.Add(this.cmbDescendNumbering);
            this.grpPedigree.Controls.Add(this.lblAscendNumbering);
            this.grpPedigree.Controls.Add(this.cmbAscendNumbering);
            this.grpPedigree.Controls.Add(this.chkAttributes);
            this.grpPedigree.Controls.Add(this.chkNotes);
            this.grpPedigree.Controls.Add(this.chkGenerations);
            this.grpPedigree.Controls.Add(this.chkSources);
            this.grpPedigree.Controls.Add(this.chkSourcePages);
            this.grpPedigree.Controls.Add(this.chkPortraits);
            this.grpPedigree.Controls.Add(this.grpPedigreeFormat);
            this.grpPedigree.Location = new System.Drawing.Point(12, 12);
            this.grpPedigree.Margin = new System.Windows.Forms.Padding(2);
            this.grpPedigree.Name = "grpPedigree";
            this.grpPedigree.Padding = new System.Windows.Forms.Padding(10);
            this.grpPedigree.Size = new System.Drawing.Size(480, 384);
            this.grpPedigree.TabIndex = 0;
            this.grpPedigree.TabStop = false;
            this.grpPedigree.Text = "grpPedigree";
            // 
            // chkAttributes
            // 
            this.chkAttributes.Location = new System.Drawing.Point(20, 28);
            this.chkAttributes.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkAttributes.Name = "chkAttributes";
            this.chkAttributes.Size = new System.Drawing.Size(349, 21);
            this.chkAttributes.TabIndex = 0;
            this.chkAttributes.Text = "chkAttributes";
            // 
            // chkNotes
            // 
            this.chkNotes.Location = new System.Drawing.Point(20, 52);
            this.chkNotes.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkNotes.Name = "chkNotes";
            this.chkNotes.Size = new System.Drawing.Size(349, 21);
            this.chkNotes.TabIndex = 1;
            this.chkNotes.Text = "chkNotes";
            // 
            // chkGenerations
            // 
            this.chkGenerations.Location = new System.Drawing.Point(20, 124);
            this.chkGenerations.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkGenerations.Name = "chkGenerations";
            this.chkGenerations.Size = new System.Drawing.Size(349, 21);
            this.chkGenerations.TabIndex = 2;
            this.chkGenerations.Text = "chkGenerations";
            // 
            // chkPortraits
            // 
            this.chkPortraits.Location = new System.Drawing.Point(20, 148);
            this.chkPortraits.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkPortraits.Name = "chkPortraits";
            this.chkPortraits.Size = new System.Drawing.Size(349, 21);
            this.chkPortraits.TabIndex = 2;
            this.chkPortraits.Text = "chkPortraits";
            // 
            // chkSources
            // 
            this.chkSources.Location = new System.Drawing.Point(20, 76);
            this.chkSources.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkSources.Name = "chkSources";
            this.chkSources.Size = new System.Drawing.Size(349, 21);
            this.chkSources.TabIndex = 2;
            this.chkSources.Text = "chkSources";
            // 
            // chkSourcePages
            // 
            this.chkSourcePages.Location = new System.Drawing.Point(40, 100);
            this.chkSourcePages.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkSourcePages.Name = "chkSourcePages";
            this.chkSourcePages.Size = new System.Drawing.Size(349, 21);
            this.chkSourcePages.TabIndex = 2;
            this.chkSourcePages.Text = "chkSourcePages";
            // 
            // grpPedigreeFormat
            // 
            this.grpPedigreeFormat.Controls.Add(this.radExcess);
            this.grpPedigreeFormat.Controls.Add(this.radCompact);
            this.grpPedigreeFormat.Location = new System.Drawing.Point(20, 180);
            this.grpPedigreeFormat.Margin = new System.Windows.Forms.Padding(2);
            this.grpPedigreeFormat.Name = "grpPedigreeFormat";
            this.grpPedigreeFormat.Padding = new System.Windows.Forms.Padding(10);
            this.grpPedigreeFormat.Size = new System.Drawing.Size(349, 101);
            this.grpPedigreeFormat.TabIndex = 3;
            this.grpPedigreeFormat.TabStop = false;
            this.grpPedigreeFormat.Text = "grpPedigreeFormat";
            // 
            // radExcess
            // 
            this.radExcess.Location = new System.Drawing.Point(10, 28);
            this.radExcess.Margin = new System.Windows.Forms.Padding(0, 0, 0, 5);
            this.radExcess.Name = "radExcess";
            this.radExcess.Size = new System.Drawing.Size(146, 30);
            this.radExcess.TabIndex = 3;
            this.radExcess.Text = "radExcess";
            // 
            // radCompact
            // 
            this.radCompact.Location = new System.Drawing.Point(10, 62);
            this.radCompact.Margin = new System.Windows.Forms.Padding(0);
            this.radCompact.Name = "radCompact";
            this.radCompact.Size = new System.Drawing.Size(146, 29);
            this.radCompact.TabIndex = 2;
            this.radCompact.Text = "radCompact";
            // 
            // pageSpecials
            // 
            this.pageSpecials.Controls.Add(this.chkUseExtendedNotes);
            this.pageSpecials.Controls.Add(this.chkKeepRichNames);
            this.pageSpecials.Controls.Add(this.lblChartWindowsShowMode);
            this.pageSpecials.Controls.Add(this.cmbChartWindowsShowMode);
            this.pageSpecials.Controls.Add(this.chkShortenDateRanges);
            this.pageSpecials.Controls.Add(this.chkKeepInfoPansOverallSize);
            this.pageSpecials.Controls.Add(this.chkFilesOverwriteWarn);
            this.pageSpecials.Controls.Add(this.chkExtendedKinships);
            this.pageSpecials.Controls.Add(this.chkShowNumberOfSubstructures);
            this.pageSpecials.BackColor = System.Drawing.SystemColors.Control;
            this.pageSpecials.Location = new System.Drawing.Point(4, 26);
            this.pageSpecials.Margin = new System.Windows.Forms.Padding(0);
            this.pageSpecials.Name = "pageSpecials";
            this.pageSpecials.Padding = new System.Windows.Forms.Padding(10);
            this.pageSpecials.Size = new System.Drawing.Size(741, 762);
            this.pageSpecials.TabIndex = 5;
            this.pageSpecials.Text = "pageSpecials";
            // 
            // chkUseExtendedNotes
            // 
            this.chkUseExtendedNotes.Location = new System.Drawing.Point(20, 51);
            this.chkUseExtendedNotes.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkUseExtendedNotes.Name = "chkUseExtendedNotes";
            this.chkUseExtendedNotes.Size = new System.Drawing.Size(600, 22);
            this.chkUseExtendedNotes.TabIndex = 12;
            this.chkUseExtendedNotes.Text = "chkUseExtendedNotes";
            // 
            // chkKeepRichNames
            // 
            this.chkKeepRichNames.Location = new System.Drawing.Point(20, 82);
            this.chkKeepRichNames.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkKeepRichNames.Name = "chkKeepRichNames";
            this.chkKeepRichNames.Size = new System.Drawing.Size(600, 22);
            this.chkKeepRichNames.TabIndex = 12;
            this.chkKeepRichNames.Text = "chkKeepRichNames";
            // 
            // lblChartWindowsShowMode
            // 
            this.lblChartWindowsShowMode.Location = new System.Drawing.Point(20, 113);
            this.lblChartWindowsShowMode.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.lblChartWindowsShowMode.Name = "lblChartWindowsShowMode";
            this.lblChartWindowsShowMode.Size = new System.Drawing.Size(300, 22);
            this.lblChartWindowsShowMode.TabIndex = 12;
            this.lblChartWindowsShowMode.Text = "lblChartWindowsShowMode";
            // 
            // cmbChartWindowsShowMode
            //
            this.cmbChartWindowsShowMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbChartWindowsShowMode.Location = new System.Drawing.Point(320, 113);
            this.cmbChartWindowsShowMode.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.cmbChartWindowsShowMode.Name = "cmbChartWindowsShowMode";
            this.cmbChartWindowsShowMode.Size = new System.Drawing.Size(220, 22);
            this.cmbChartWindowsShowMode.TabIndex = 12;
            this.cmbChartWindowsShowMode.Text = "cmbChartWindowsShowMode";
            // 
            // chkShortenDateRanges
            // 
            this.chkShortenDateRanges.Location = new System.Drawing.Point(20, 237);
            this.chkShortenDateRanges.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkShortenDateRanges.Name = "chkShortenDateRanges";
            this.chkShortenDateRanges.Size = new System.Drawing.Size(600, 22);
            this.chkShortenDateRanges.TabIndex = 14;
            this.chkShortenDateRanges.Text = "chkShortenDateRanges";
            // 
            // chkKeepInfoPansOverallSize
            // 
            this.chkKeepInfoPansOverallSize.Location = new System.Drawing.Point(20, 268);
            this.chkKeepInfoPansOverallSize.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkKeepInfoPansOverallSize.Name = "chkKeepInfoPansOverallSize";
            this.chkKeepInfoPansOverallSize.Size = new System.Drawing.Size(600, 22);
            this.chkKeepInfoPansOverallSize.TabIndex = 15;
            this.chkKeepInfoPansOverallSize.Text = "chkKeepInfoPansOverallSize";
            // 
            // chkFilesOverwriteWarn
            // 
            this.chkFilesOverwriteWarn.Location = new System.Drawing.Point(20, 299);
            this.chkFilesOverwriteWarn.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkFilesOverwriteWarn.Name = "chkFilesOverwriteWarn";
            this.chkFilesOverwriteWarn.Size = new System.Drawing.Size(600, 22);
            this.chkFilesOverwriteWarn.TabIndex = 16;
            this.chkFilesOverwriteWarn.Text = "chkFilesOverwriteWarn";
            // 
            // chkExtendedKinships
            // 
            this.chkExtendedKinships.Location = new System.Drawing.Point(20, 330);
            this.chkExtendedKinships.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkExtendedKinships.Name = "chkExtendedKinships";
            this.chkExtendedKinships.Size = new System.Drawing.Size(600, 22);
            this.chkExtendedKinships.TabIndex = 17;
            this.chkExtendedKinships.Text = "chkExtendedKinships";
            // 
            // chkShowNumberOfSubstructures
            // 
            this.chkShowNumberOfSubstructures.Location = new System.Drawing.Point(20, 454);
            this.chkShowNumberOfSubstructures.Margin = new System.Windows.Forms.Padding(10, 0, 0, 5);
            this.chkShowNumberOfSubstructures.Name = "chkShowNumberOfSubstructures";
            this.chkShowNumberOfSubstructures.Size = new System.Drawing.Size(600, 22);
            this.chkShowNumberOfSubstructures.TabIndex = 20;
            this.chkShowNumberOfSubstructures.Text = "chkShowNumberOfSubstructures";
            // 
            // pageEventTypes
            // 
            this.pageEventTypes.BackColor = System.Drawing.SystemColors.Control;
            this.pageEventTypes.Controls.Add(this.slEventTypes);
            this.pageEventTypes.Location = new System.Drawing.Point(4, 26);
            this.pageEventTypes.Margin = new System.Windows.Forms.Padding(0);
            this.pageEventTypes.Name = "pageEventTypes";
            this.pageEventTypes.Padding = new System.Windows.Forms.Padding(10);
            this.pageEventTypes.Size = new System.Drawing.Size(741, 762);
            this.pageEventTypes.TabIndex = 6;
            this.pageEventTypes.Text = "pageEventTypes";
            // 
            // slEventTypes
            // 
            this.slEventTypes.Dock = System.Windows.Forms.DockStyle.Fill;
            this.slEventTypes.Location = new System.Drawing.Point(10, 10);
            this.slEventTypes.Margin = new System.Windows.Forms.Padding(2);
            this.slEventTypes.Name = "slEventTypes";
            this.slEventTypes.Size = new System.Drawing.Size(721, 742);
            this.slEventTypes.TabIndex = 0;
            // 
            // pagePlugins
            // 
            this.pagePlugins.BackColor = System.Drawing.SystemColors.Control;
            this.pagePlugins.Controls.Add(this.lvPlugins);
            this.pagePlugins.Location = new System.Drawing.Point(4, 26);
            this.pagePlugins.Margin = new System.Windows.Forms.Padding(0);
            this.pagePlugins.Name = "pagePlugins";
            this.pagePlugins.Padding = new System.Windows.Forms.Padding(10);
            this.pagePlugins.Size = new System.Drawing.Size(741, 762);
            this.pagePlugins.TabIndex = 6;
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
            this.lvPlugins.HideSelection = false;
            this.lvPlugins.ListMan = null;
            this.lvPlugins.Location = new System.Drawing.Point(10, 10);
            this.lvPlugins.Margin = new System.Windows.Forms.Padding(2);
            this.lvPlugins.MultiSelect = false;
            this.lvPlugins.Name = "lvPlugins";
            this.lvPlugins.OwnerDraw = true;
            this.lvPlugins.SelectedIndex = -1;
            this.lvPlugins.Size = new System.Drawing.Size(721, 742);
            this.lvPlugins.SortColumn = 0;
            this.lvPlugins.SortOrder = GKCore.Design.BSDTypes.SortOrder.None;
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
            this.btnAccept.Location = new System.Drawing.Point(491, 702);
            this.btnAccept.Margin = new System.Windows.Forms.Padding(2);
            this.btnAccept.Name = "btnAccept";
            this.btnAccept.Size = new System.Drawing.Size(114, 30);
            this.btnAccept.TabIndex = 1;
            this.btnAccept.Text = "btnAccept";
            this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.AcceptClickHandler);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(621, 702);
            this.btnCancel.Margin = new System.Windows.Forms.Padding(2);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(114, 30);
            this.btnCancel.TabIndex = 2;
            this.btnCancel.Text = "btnCancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // lblAscendNumbering
            // 
            this.lblAscendNumbering.AutoSize = true;
            this.lblAscendNumbering.Location = new System.Drawing.Point(17, 313);
            this.lblAscendNumbering.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblAscendNumbering.Name = "lblAscendNumbering";
            this.lblAscendNumbering.Size = new System.Drawing.Size(164, 21);
            this.lblAscendNumbering.TabIndex = 5;
            this.lblAscendNumbering.Text = "lblAscendNumbering";
            // 
            // cmbAscendNumbering
            // 
            this.cmbAscendNumbering.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbAscendNumbering.Location = new System.Drawing.Point(233, 310);
            this.cmbAscendNumbering.Margin = new System.Windows.Forms.Padding(2);
            this.cmbAscendNumbering.Name = "cmbAscendNumbering";
            this.cmbAscendNumbering.Size = new System.Drawing.Size(230, 25);
            this.cmbAscendNumbering.TabIndex = 6;
            // 
            // lblDescendNumbering
            // 
            this.lblDescendNumbering.AutoSize = true;
            this.lblDescendNumbering.Location = new System.Drawing.Point(17, 342);
            this.lblDescendNumbering.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblDescendNumbering.Name = "lblDescendNumbering";
            this.lblDescendNumbering.Size = new System.Drawing.Size(175, 21);
            this.lblDescendNumbering.TabIndex = 7;
            this.lblDescendNumbering.Text = "lblDescendNumbering";
            // 
            // cmbDescendNumbering
            // 
            this.cmbDescendNumbering.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbDescendNumbering.Location = new System.Drawing.Point(233, 339);
            this.cmbDescendNumbering.Margin = new System.Windows.Forms.Padding(2);
            this.cmbDescendNumbering.Name = "cmbDescendNumbering";
            this.cmbDescendNumbering.Size = new System.Drawing.Size(230, 25);
            this.cmbDescendNumbering.TabIndex = 8;
            // 
            // OptionsDlg
            // 
            this.AcceptButton = this.btnAccept;
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(749, 746);
            this.Controls.Add(this.PageControl1);
            this.Controls.Add(this.btnResetDefaults);
            this.Controls.Add(this.btnAccept);
            this.Controls.Add(this.btnCancel);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Margin = new System.Windows.Forms.Padding(2);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "OptionsDlg";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "OptionsDlg";
            this.Title = "OptionsDlg";
            this.pageGeo.ResumeLayout(false);
            this.pageGeo.PerformLayout();
            this.PageControl1.ResumeLayout(false);
            this.pageCommon.ResumeLayout(false);
            this.pageCommon.PerformLayout();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numBackupRevisionsMaxCount)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numASMin)).EndInit();
            this.grpFileBackup.ResumeLayout(false);
            this.grpInternet.ResumeLayout(false);
            this.grpInternet.PerformLayout();
            this.grpOther.ResumeLayout(false);
            this.grpOther.PerformLayout();
            this.pageMultimedia.ResumeLayout(false);
            this.pageMultimedia.PerformLayout();
            this.pageCharts.ResumeLayout(false);
            this.tabsCharts.ResumeLayout(false);
            this.pageTreeChart.ResumeLayout(false);
            this.pageTreeChart.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numDefaultDepth)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numDefaultDepthAncestors)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numDefaultDepthDescendants)).EndInit();
            this.grpSpacings.ResumeLayout(false);
            this.grpSpacings.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.numSpouseDist)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numGenDist)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numBranchDist)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.numMargins)).EndInit();
            this.pageTreePersons.ResumeLayout(false);
            this.pageTreeDesign.ResumeLayout(false);
            this.tabsTreeCharts.ResumeLayout(false);
            this.panTreePersons.ResumeLayout(false);
            this.grpTreeDecor.ResumeLayout(false);
            this.panMaleColor.ResumeLayout(false);
            this.panFemaleColor.ResumeLayout(false);
            this.panUnkSexColor.ResumeLayout(false);
            this.panUnHusbandColor.ResumeLayout(false);
            this.panUnWifeColor.ResumeLayout(false);
            this.panDefFont.ResumeLayout(false);
            this.pageAncCircle.ResumeLayout(false);
            this.pageUIView.ResumeLayout(false);
            this.PageControl2.ResumeLayout(false);
            this.pageViewCommon.ResumeLayout(false);
            this.grpAdvancedNames.ResumeLayout(false);
            this.grpAdvancedNames.PerformLayout();
            this.rgFNPFormat.ResumeLayout(false);
            this.grpDateFormat.ResumeLayout(false);
            this.pageViewPersons.ResumeLayout(false);
            this.panel1.ResumeLayout(false);
            this.pagePedigree.ResumeLayout(false);
            this.grpPedigree.ResumeLayout(false);
            this.grpPedigreeFormat.ResumeLayout(false);
            this.pageSpecials.ResumeLayout(false);
            this.pageEventTypes.ResumeLayout(false);
            this.pagePlugins.ResumeLayout(false);
            this.pageNavigation.ResumeLayout(false);
            this.ResumeLayout(false);
        }
    }
}
