/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;
using GKCore.Backups;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Export;
using GKCore.Filters;
using GKCore.Lists;
using GKCore.Media;
using GKCore.Names;
using GKCore.Options;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Controllers
{
    [TestFixture]
    public class OptionsDlgControllerTests : ControllerTest
    {
        private ISheetList _fEventTypesList;

        // typed handles for string-kept controls, used in tests
        private ICheckBox _chkExtBackupEnabled;
        private ITextBox _txtExtBackupFolder;

        private ITabPage _pageCommon;
        private ITabPage _pageUIView;
        private ITabPage _pageCharts;
        private ITabPage _pageMultimedia;
        private ITabPage _pageEventTypes;
        private ITabPage _pagePedigree;
        private ITabPage _pageSpecials;
        private ITabPage _pageGeo;
        private ITabPage _pageNavigation;
        private ITabPage _pageTreeChart;
        private ITabPage _pageTreeDesign;
        private ITabPage _pageTreePersons;
        private ITabPage _pageAncCircle;
        private ITabPage _pageViewCommon;
        private ITabPage _pageViewPersons;
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IButton _btnColumnUp;
        private IButton _btnColumnDown;
        private IButton _btnResetDefaults;
        private IGroupBox _grpInternet;
        private IGroupBox _grpFileBackup;
        private IGroupBox _grpOther;
        private IGroupBox _rgFNPFormat;
        private IGroupBox _grpDateFormat;
        private IGroupBox _grpAdvancedNames;
        private IGroupBox _grpPedigree;
        private IGroupBox _grpPedigreeFormat;
        private IGroupBox _grpSpacings;
        private ILabel _lblAscendNumbering;
        private ILabel _lblBackupRevisionsMaxCount;
        private ILabel _lblBranchDist;
        private ILabel _lblCertaintyAlgorithm;
        private ILabel _lblChartFont;
        private ILabel _lblChartWindowsShowMode;
        private ILabel _lblDefaultDepth;
        private ILabel _lblDefaultDepthAncestors;
        private ILabel _lblDefaultDepthDescendants;
        private ILabel _lblDescendNumbering;
        private ILabel _lblFemaleColor;
        private ILabel _lblGenDist;
        private ILabel _lblGeocoder;
        private ILabel _lblGeoSearchCountry;
        private ILabel _lblLanguage;
        private ILabel _lblMaleColor;
        private ILabel _lblMargins;
        private ILabel _lblMatchPatternMethod;
        private ILabel _lblMediaStoreDefault;
        private ILabel _lblMinutes;
        private ILabel _lblPadding;
        private ILabel _lblProxyLogin;
        private ILabel _lblProxyPassword;
        private ILabel _lblProxyPort;
        private ILabel _lblProxyServer;
        private ILabel _lblSpouseDist;
        private ILabel _lblTextEffect;
        private ILabel _lblUnHusbandColor;
        private ILabel _lblUnkSexColor;
        private ILabel _lblUnWifeColor;
        private ITextBox _txtProxyLogin;
        private ITextBox _txtProxyPass;
        private ITextBox _txtProxyPort;
        private ITextBox _txtProxyServer;
        private IComboBox _cmbAscendNumbering;
        private IComboBox _cmbCertaintyAlgorithm;
        private IComboBox _cmbChartWindowsShowMode;
        private IComboBox _cmbDescendNumbering;
        private IComboBox _cmbGeocoder;
        private IComboBox _cmbGeoSearchCountry;
        private IComboBox _cmbLanguages;
        private IComboBox _cmbMatchPatternMethod;
        private IComboBox _cmbMediaStoreDefault;
        private IComboBox _cmbTextEffect;
        private ICheckBox _chkAllowDeleteMediaFileFromRefs;
        private ICheckBox _chkAllowDeleteMediaFileFromStgArc;
        private ICheckBox _chkAllowMediaDirectRefs;
        private ICheckBox _chkAllowMediaStoreRelativeReferences;
        private ICheckBox _chkAttributes;
        private ICheckBox _chkAutoCheckUpdates;
        private ICheckBox _chkAutosave;
        private ICheckBox _chkAutoSortChildren;
        private ICheckBox _chkAutoSortSpouses;
        private ICheckBox _chkBirthDate;
        private ICheckBox _chkBoldNames;
        private ICheckBox _chkCharsetDetection;
        private ICheckBox _chkCheckTreeSize;
        private ICheckBox _chkChildlessExclude;
        private ICheckBox _chkDateDesignations;
        private ICheckBox _chkDeathDate;
        private ICheckBox _chkDefaultPortraits;
        private ICheckBox _chkDeleteMediaFileWithoutConfirm;
        private ICheckBox _chkDialogClosingWarn;
        private ICheckBox _chkDiffLines;
        private ICheckBox _chkDisableNonStdFeatures;
        private ICheckBox _chkDottedLinesOfAdoptedChildren;
        private ICheckBox _chkDottedLinesOfDivorcedSpouses;
        private ICheckBox _chkELAbbreviatedNames;
        private ICheckBox _chkEmbeddedMediaPlayer;
        private ICheckBox _chkEnableStdValidation;
        private ICheckBox _chkExtendedKinships;
        private ICheckBox _chkExtendedLocations;
        private ICheckBox _chkExtendedTree;
        private ICheckBox _chkExtendWomanSurnames;
        private ICheckBox _chkFilesOverwriteWarn;
        private ICheckBox _chkFirstCapitalLetterInNames;
        private ICheckBox _chkFullNameOnOneLine;
        private ICheckBox _chkGenerations;
        private ICheckBox _chkHideUnknownSpouses;
        private ICheckBox _chkHighlightInaccessibleFiles;
        private ICheckBox _chkHighlightUnmarried;
        private ICheckBox _chkHighlightUnparented;
        private ICheckBox _chkInvertedTree;
        private ICheckBox _chkKeepInfoPansOverallSize;
        private ICheckBox _chkKeepRichNames;
        private ICheckBox _chkKinship;
        private ICheckBox _chkLoadRecentFiles;
        private ICheckBox _chkLocalizedCalendarSignatures;
        private ICheckBox _chkMarriagesDates;
        private ICheckBox _chkMinimizingWidth;
        private ICheckBox _chkMourningEdges;
        private ICheckBox _chkName;
        private ICheckBox _chkNotes;
        private ICheckBox _chkOnlyLocality;
        private ICheckBox _chkOnlyYears;
        private ICheckBox _chkPatronymic;
        private ICheckBox _chkPlacesWithAddress;
        private ICheckBox _chkPortraits;
        private ICheckBox _chkPortraitsVisible;
        private ICheckBox _chkRemovableMediaWarning;
        private ICheckBox _chkReversePlacesOrder;
        private ICheckBox _chkSAFByAllNames;
        private ICheckBox _chkSameCardsWidth;
        private ICheckBox _chkSearchPlacesWithoutCoords;
        private ICheckBox _chkSeparateDAPLines;
        private ICheckBox _chkSeparateDepth;
        private ICheckBox _chkShortenDateRanges;
        private ICheckBox _chkShortKinshipForm;
        private ICheckBox _chkShowAge;
        private ICheckBox _chkShowDatesCalendar;
        private ICheckBox _chkShowDatesSigns;
        private ICheckBox _chkShowNumberOfSubstructures;
        private ICheckBox _chkShowOnStart;
        private ICheckBox _chkShowPlaces;
        private ICheckBox _chkSignsVisible;
        private ICheckBox _chkSimpleSingleSurnames;
        private ICheckBox _chkSourceCitations;
        private ICheckBox _chkSourcePages;
        private ICheckBox _chkSources;
        private ICheckBox _chkSurname;
        private ICheckBox _chkSurnameFirstInOrder;
        private ICheckBox _chkSurnameInCapitals;
        private ICheckBox _chkTreeDecorative;
        private ICheckBox _chkTreeSurnameFirst;
        private ICheckBox _chkURNotesVisible;
        private ICheckBox _chkUseAdditionalDates;
        private ICheckBox _chkUseBirthDatesInPSF;
        private ICheckBox _chkUseExtendedNotes;
        private ICheckBox _chkUseExtraControls;
        private ICheckBox _chkUseInlineImagesInSvg;
        private ICheckBox _chkUseProxy;
        private ICheckBox _chkUseSurnamesInPSF;
        private INumericBox _numASMin;
        private INumericBox _numBackupRevisionsMaxCount;
        private INumericBox _numBranchDist;
        private INumericBox _numDefaultDepth;
        private INumericBox _numDefaultDepthAncestors;
        private INumericBox _numDefaultDepthDescendants;
        private INumericBox _numGenDist;
        private INumericBox _numMargins;
        private INumericBox _numPadding;
        private INumericBox _numSpouseDist;
        private IRadioButton _radCompact;
        private IRadioButton _radDMY;
        private IRadioButton _radExcess;
        private IRadioButton _radFBEachRevision;
        private IRadioButton _radFBNone;
        private IRadioButton _radFBOnlyPrev;
        private IRadioButton _radMaiden;
        private IRadioButton _radMaiden_Married;
        private IRadioButton _radMarried;
        private IRadioButton _radMarried_Maiden;
        private IRadioButton _radS_N_P;
        private IRadioButton _radS_NP;
        private IRadioButton _radSNP;
        private IRadioButton _radYMD;
        private IListView _lstPersonColumns;

        [Test]
        public void Test_OptionsDlgController()
        {
            IOptionsDlg view = CreateMockView();
            var controller = new OptionsDlgController(view);
            controller.InitView(fBaseWin);
            controller.UpdateView();
            controller.SetLocale();
            controller.ApplyTheme();
            controller.ChangeTab();

            Assert.IsTrue(controller.Accept());
        }

        [Test]
        public void Test_OtherOptions()
        {
            IOptionsDlg view = CreateMockView();
            var controller = new OptionsDlgController(view);
            controller.InitView(fBaseWin);

            controller.UpdateOtherOptions();
            controller.ChangeOtherOption();

            _chkShowOnStart.Checked = true;
            _chkAutoCheckUpdates.Checked = true;
            _cmbCertaintyAlgorithm.GetSelectedTag<CertaintyAlgorithm>().Returns(CertaintyAlgorithm.Maximum);
            _cmbGeocoder.Text = "OSM";
            _cmbGeoSearchCountry.Text = "Russia";

            controller.AcceptOtherOptions();

            Assert.IsTrue(GlobalOptions.Instance.ShowTips);
            Assert.IsTrue(GlobalOptions.Instance.AutoCheckUpdates);
            Assert.AreEqual(CertaintyAlgorithm.Maximum, GlobalOptions.Instance.CertaintyAlgorithm);
            Assert.AreEqual("OSM", GlobalOptions.Instance.Geocoder);
            Assert.AreEqual("Russia", GlobalOptions.Instance.GeoSearchCountry);
        }

        [Test]
        public void Test_PedigreesOptions()
        {
            IOptionsDlg view = CreateMockView();
            var controller = new OptionsDlgController(view);
            controller.InitView(fBaseWin);

            controller.UpdatePedigreesOptions();
            controller.ResetPedigreesOptions();

            _chkNotes.Checked = true;
            _radExcess.Checked = true;
            _cmbAscendNumbering.GetSelectedTag<PedigreeNumbering>().Returns(PedigreeNumbering.Sosa_Stradonitz);
            _cmbDescendNumbering.GetSelectedTag<PedigreeNumbering>().Returns(PedigreeNumbering.Aboville);

            controller.AcceptPedigreesOptions();

            Assert.IsTrue(GlobalOptions.Instance.PedigreeOptions.IncludeNotes);
            Assert.AreEqual(PedigreeFormat.Excess, GlobalOptions.Instance.PedigreeOptions.Format);
            Assert.AreEqual(PedigreeNumbering.Sosa_Stradonitz, GlobalOptions.Instance.PedigreeOptions.AscendNumbering);
            Assert.AreEqual(PedigreeNumbering.Aboville, GlobalOptions.Instance.PedigreeOptions.DescendNumbering);
        }

        [Test]
        public void Test_WomanSurnameFormat()
        {
            IOptionsDlg view = CreateMockView();
            var controller = new OptionsDlgController(view);
            controller.InitView(fBaseWin);

            controller.UpdateWomanSurnameFormat();
            controller.ChangeExtendWomanSurnames();

            _chkExtendWomanSurnames.Checked = true;
            _chkSimpleSingleSurnames.Checked = true;
            _radMaiden_Married.Checked = true;

            controller.AcceptWomanSurnameFormat();

            Assert.AreEqual(WomanSurnameFormat.wsfMaiden_Married, GlobalOptions.Instance.WomanSurnameFormat);
            Assert.IsTrue(GlobalOptions.Instance.SimpleSingleSurnames);

            _chkExtendWomanSurnames.Checked = false;

            controller.AcceptWomanSurnameFormat();

            Assert.AreEqual(WomanSurnameFormat.wsfNotExtend, GlobalOptions.Instance.WomanSurnameFormat);
        }

        [Test]
        public void Test_InterfaceOptions()
        {
            IOptionsDlg view = CreateMockView();
            var controller = new OptionsDlgController(view);
            controller.InitView(fBaseWin);

            controller.UpdateInterfaceOptions();

            _radSNP.Checked = false;
            _radS_NP.Checked = true;
            Assert.AreEqual(NameFormat.nfF_NP, controller.GetSelectedNameFormat());

            controller.ChangeFNPFormat();

            _chkPlacesWithAddress.Checked = true;
            _cmbMatchPatternMethod.GetSelectedTag<MatchPatternMethod>().Returns(MatchPatternMethod.Fast);

            controller.AcceptInterfaceOptions();

            Assert.AreEqual(NameFormat.nfF_NP, GlobalOptions.Instance.DefNameFormat);
            Assert.IsTrue(GlobalOptions.Instance.PlacesWithAddress);
            Assert.AreEqual(MatchPatternMethod.Fast, GlobalOptions.Instance.MatchPatternMethod);
        }

        [Test]
        public void Test_ColumnsList()
        {
            IOptionsDlg view = CreateMockView();
            var controller = new OptionsDlgController(view);
            controller.InitView(fBaseWin);

            controller.UpdateView();
            controller.UpdateColumnsList();

            _lstPersonColumns.SelectedIndex = 1;
            controller.MoveColumnUp();
            _lstPersonColumns.SelectedIndex = 0;
            controller.MoveColumnDown();
            controller.ResetColumnsList();
            controller.AcceptColumnsList();
        }

        [Test]
        public void Test_MediaOptions()
        {
            IOptionsDlg view = CreateMockView();
            var controller = new OptionsDlgController(view);
            controller.InitView(fBaseWin);

            controller.UpdateMediaOptions();

            _chkRemovableMediaWarning.Checked = true;

            controller.AcceptMediaOptions();

            Assert.IsTrue(GlobalOptions.Instance.RemovableMediaWarning);
            Assert.AreEqual(MediaStoreType.mstReference, GlobalOptions.Instance.MediaStoreDefault);
        }

        [Test]
        public void Test_BackupOptions()
        {
            IOptionsDlg view = CreateMockView();
            var controller = new OptionsDlgController(view);
            controller.InitView(fBaseWin);

            controller.UpdateBackupOptions();
            controller.CheckExtBackup();

            _radFBNone.Checked = false;
            _radFBOnlyPrev.Checked = true;
            _chkExtBackupEnabled.Checked = true;
            _txtExtBackupFolder.Text = "backup";

            controller.AcceptBackupOptions();

            Assert.AreEqual(FileBackup.fbOnlyPrev, GlobalOptions.Instance.Backups.FileBackup);
            Assert.IsTrue(GlobalOptions.Instance.Backups.ExtendedBackup);
            Assert.AreEqual("backup", GlobalOptions.Instance.Backups.ExtendedFolder);

            controller.SelectExtBackupFolder();
        }

        [Test]
        public void Test_TreeChartsOptions()
        {
            IOptionsDlg view = CreateMockView();
            var controller = new OptionsDlgController(view);
            controller.InitView(fBaseWin);

            controller.UpdateTreeChartsOptions();
            controller.ChangeTreeChartOption();
            controller.ChangeSeparateDepth();
            controller.UpdateTreeChartFont();

            _chkSurname.Checked = true;
            _chkExtendedTree.Checked = true;

            controller.AcceptTreeChartsOptions();

            Assert.IsTrue(GlobalOptions.Instance.TreeChartOptions.FamilyVisible);
        }

        [Test]
        public void Test_SpecialsOptions()
        {
            IOptionsDlg view = CreateMockView();
            var controller = new OptionsDlgController(view);
            controller.InitView(fBaseWin);

            controller.UpdateSpecials();

            _chkExtendedLocations.Checked = true;
            _chkReversePlacesOrder.Checked = true;
            _chkExtendedTree.Checked = true;

            controller.AcceptSpecials();

            Assert.IsTrue(GlobalOptions.Instance.ExtendedLocations);
            Assert.IsTrue(GlobalOptions.Instance.ReversePlaceEntitiesOrder);
            Assert.IsTrue(GlobalOptions.Instance.TreeChartOptions.ExtendedTree);
        }

        [Test]
        public void Test_FamilyBookAndPlugins()
        {
            IOptionsDlg view = CreateMockView();
            var controller = new OptionsDlgController(view);
            controller.InitView(fBaseWin);

            controller.UpdateFamilyBookOptions();
            controller.AcceptFamilyBookOptions();

            controller.UpdatePlugins();
            controller.AcceptPlugins();

            controller.ResetCircleChartsOptions();
        }

        private IOptionsDlg CreateMockView()
        {
            var view = Substitute.For<IOptionsDlg>();

            _fEventTypesList = Substitute.For<ISheetList>();
            view.EventTypesList.Returns(_fEventTypesList);

            _pageCommon = SubstCtl<ITabPage>(view, "pageCommon");
            _pageUIView = SubstCtl<ITabPage>(view, "pageUIView");
            _pageCharts = SubstCtl<ITabPage>(view, "pageCharts");
            _pageMultimedia = SubstCtl<ITabPage>(view, "pageMultimedia");
            _pageEventTypes = SubstCtl<ITabPage>(view, "pageEventTypes");
            _pagePedigree = SubstCtl<ITabPage>(view, "pagePedigree");
            _pageSpecials = SubstCtl<ITabPage>(view, "pageSpecials");
            _pageGeo = SubstCtl<ITabPage>(view, "pageGeo");
            _pageNavigation = SubstCtl<ITabPage>(view, "pageNavigation");
            _pageTreeChart = SubstCtl<ITabPage>(view, "pageTreeChart");
            _pageTreeDesign = SubstCtl<ITabPage>(view, "pageTreeDesign");
            _pageTreePersons = SubstCtl<ITabPage>(view, "pageTreePersons");
            _pageAncCircle = SubstCtl<ITabPage>(view, "pageAncCircle");
            _pageViewCommon = SubstCtl<ITabPage>(view, "pageViewCommon");
            _pageViewPersons = SubstCtl<ITabPage>(view, "pageViewPersons");
            _btnAccept = SubstCtl<IButton>(view, "btnAccept");
            _btnCancel = SubstCtl<IButton>(view, "btnCancel");
            _btnColumnUp = SubstCtl<IButton>(view, "btnColumnUp");
            _btnColumnDown = SubstCtl<IButton>(view, "btnColumnDown");
            _btnResetDefaults = SubstCtl<IButton>(view, "btnResetDefaults");
            _grpInternet = SubstCtl<IGroupBox>(view, "grpInternet");
            _grpFileBackup = SubstCtl<IGroupBox>(view, "grpFileBackup");
            _grpOther = SubstCtl<IGroupBox>(view, "grpOther");
            _rgFNPFormat = SubstCtl<IGroupBox>(view, "rgFNPFormat");
            _grpDateFormat = SubstCtl<IGroupBox>(view, "grpDateFormat");
            _grpAdvancedNames = SubstCtl<IGroupBox>(view, "grpAdvancedNames");
            _grpPedigree = SubstCtl<IGroupBox>(view, "grpPedigree");
            _grpPedigreeFormat = SubstCtl<IGroupBox>(view, "grpPedigreeFormat");
            _grpSpacings = SubstCtl<IGroupBox>(view, "grpSpacings");
            _lblAscendNumbering = SubstCtl<ILabel>(view, "lblAscendNumbering");
            _lblBackupRevisionsMaxCount = SubstCtl<ILabel>(view, "lblBackupRevisionsMaxCount");
            _lblBranchDist = SubstCtl<ILabel>(view, "lblBranchDist");
            _lblCertaintyAlgorithm = SubstCtl<ILabel>(view, "lblCertaintyAlgorithm");
            _lblChartFont = SubstCtl<ILabel>(view, "lblChartFont");
            _lblChartWindowsShowMode = SubstCtl<ILabel>(view, "lblChartWindowsShowMode");
            _lblDefaultDepth = SubstCtl<ILabel>(view, "lblDefaultDepth");
            _lblDefaultDepthAncestors = SubstCtl<ILabel>(view, "lblDefaultDepthAncestors");
            _lblDefaultDepthDescendants = SubstCtl<ILabel>(view, "lblDefaultDepthDescendants");
            _lblDescendNumbering = SubstCtl<ILabel>(view, "lblDescendNumbering");
            _lblFemaleColor = SubstCtl<ILabel>(view, "lblFemaleColor");
            _lblGenDist = SubstCtl<ILabel>(view, "lblGenDist");
            _lblGeocoder = SubstCtl<ILabel>(view, "lblGeocoder");
            _lblGeoSearchCountry = SubstCtl<ILabel>(view, "lblGeoSearchCountry");
            _lblLanguage = SubstCtl<ILabel>(view, "lblLanguage");
            _lblMaleColor = SubstCtl<ILabel>(view, "lblMaleColor");
            _lblMargins = SubstCtl<ILabel>(view, "lblMargins");
            _lblMatchPatternMethod = SubstCtl<ILabel>(view, "lblMatchPatternMethod");
            _lblMediaStoreDefault = SubstCtl<ILabel>(view, "lblMediaStoreDefault");
            _lblMinutes = SubstCtl<ILabel>(view, "lblMinutes");
            _lblPadding = SubstCtl<ILabel>(view, "lblPadding");
            _lblProxyLogin = SubstCtl<ILabel>(view, "lblProxyLogin");
            _lblProxyPassword = SubstCtl<ILabel>(view, "lblProxyPassword");
            _lblProxyPort = SubstCtl<ILabel>(view, "lblProxyPort");
            _lblProxyServer = SubstCtl<ILabel>(view, "lblProxyServer");
            _lblSpouseDist = SubstCtl<ILabel>(view, "lblSpouseDist");
            _lblTextEffect = SubstCtl<ILabel>(view, "lblTextEffect");
            _lblUnHusbandColor = SubstCtl<ILabel>(view, "lblUnHusbandColor");
            _lblUnkSexColor = SubstCtl<ILabel>(view, "lblUnkSexColor");
            _lblUnWifeColor = SubstCtl<ILabel>(view, "lblUnWifeColor");
            _txtProxyLogin = SubstCtl<ITextBox>(view, "txtProxyLogin");
            _txtProxyPass = SubstCtl<ITextBox>(view, "txtProxyPass");
            _txtProxyPort = SubstCtl<ITextBox>(view, "txtProxyPort");
            _txtProxyServer = SubstCtl<ITextBox>(view, "txtProxyServer");
            _cmbAscendNumbering = SubstCtl<IComboBox>(view, "cmbAscendNumbering");
            _cmbCertaintyAlgorithm = SubstCtl<IComboBox>(view, "cmbCertaintyAlgorithm");
            _cmbChartWindowsShowMode = SubstCtl<IComboBox>(view, "cmbChartWindowsShowMode");
            _cmbDescendNumbering = SubstCtl<IComboBox>(view, "cmbDescendNumbering");
            _cmbGeocoder = SubstCtl<IComboBox>(view, "cmbGeocoder");
            _cmbGeoSearchCountry = SubstCtl<IComboBox>(view, "cmbGeoSearchCountry");
            _cmbLanguages = SubstCtl<IComboBox>(view, "cmbLanguages");
            _cmbMatchPatternMethod = SubstCtl<IComboBox>(view, "cmbMatchPatternMethod");
            _cmbMediaStoreDefault = SubstCtl<IComboBox>(view, "cmbMediaStoreDefault");
            _cmbTextEffect = SubstCtl<IComboBox>(view, "cmbTextEffect");
            _chkAllowDeleteMediaFileFromRefs = SubstCtl<ICheckBox>(view, "chkAllowDeleteMediaFileFromRefs");
            _chkAllowDeleteMediaFileFromStgArc = SubstCtl<ICheckBox>(view, "chkAllowDeleteMediaFileFromStgArc");
            _chkAllowMediaDirectRefs = SubstCtl<ICheckBox>(view, "chkAllowMediaDirectRefs");
            _chkAllowMediaStoreRelativeReferences = SubstCtl<ICheckBox>(view, "chkAllowMediaStoreRelativeReferences");
            _chkAttributes = SubstCtl<ICheckBox>(view, "chkAttributes");
            _chkAutoCheckUpdates = SubstCtl<ICheckBox>(view, "chkAutoCheckUpdates");
            _chkAutosave = SubstCtl<ICheckBox>(view, "chkAutosave");
            _chkAutoSortChildren = SubstCtl<ICheckBox>(view, "chkAutoSortChildren");
            _chkAutoSortSpouses = SubstCtl<ICheckBox>(view, "chkAutoSortSpouses");
            _chkBirthDate = SubstCtl<ICheckBox>(view, "chkBirthDate");
            _chkBoldNames = SubstCtl<ICheckBox>(view, "chkBoldNames");
            _chkCharsetDetection = SubstCtl<ICheckBox>(view, "chkCharsetDetection");
            _chkCheckTreeSize = SubstCtl<ICheckBox>(view, "chkCheckTreeSize");
            _chkChildlessExclude = SubstCtl<ICheckBox>(view, "chkChildlessExclude");
            _chkDateDesignations = SubstCtl<ICheckBox>(view, "chkDateDesignations");
            _chkDeathDate = SubstCtl<ICheckBox>(view, "chkDeathDate");
            _chkDefaultPortraits = SubstCtl<ICheckBox>(view, "chkDefaultPortraits");
            _chkDeleteMediaFileWithoutConfirm = SubstCtl<ICheckBox>(view, "chkDeleteMediaFileWithoutConfirm");
            _chkDialogClosingWarn = SubstCtl<ICheckBox>(view, "chkDialogClosingWarn");
            _chkDiffLines = SubstCtl<ICheckBox>(view, "chkDiffLines");
            _chkDisableNonStdFeatures = SubstCtl<ICheckBox>(view, "chkDisableNonStdFeatures");
            _chkDottedLinesOfAdoptedChildren = SubstCtl<ICheckBox>(view, "chkDottedLinesOfAdoptedChildren");
            _chkDottedLinesOfDivorcedSpouses = SubstCtl<ICheckBox>(view, "chkDottedLinesOfDivorcedSpouses");
            _chkELAbbreviatedNames = SubstCtl<ICheckBox>(view, "chkELAbbreviatedNames");
            _chkEmbeddedMediaPlayer = SubstCtl<ICheckBox>(view, "chkEmbeddedMediaPlayer");
            _chkEnableStdValidation = SubstCtl<ICheckBox>(view, "chkEnableStdValidation");
            _chkExtendedKinships = SubstCtl<ICheckBox>(view, "chkExtendedKinships");
            _chkExtendedLocations = SubstCtl<ICheckBox>(view, "chkExtendedLocations");
            _chkExtendedTree = SubstCtl<ICheckBox>(view, "chkExtendedTree");
            _chkExtendWomanSurnames = SubstCtl<ICheckBox>(view, "chkExtendWomanSurnames");
            _chkFilesOverwriteWarn = SubstCtl<ICheckBox>(view, "chkFilesOverwriteWarn");
            _chkFirstCapitalLetterInNames = SubstCtl<ICheckBox>(view, "chkFirstCapitalLetterInNames");
            _chkFullNameOnOneLine = SubstCtl<ICheckBox>(view, "chkFullNameOnOneLine");
            _chkGenerations = SubstCtl<ICheckBox>(view, "chkGenerations");
            _chkHideUnknownSpouses = SubstCtl<ICheckBox>(view, "chkHideUnknownSpouses");
            _chkHighlightInaccessibleFiles = SubstCtl<ICheckBox>(view, "chkHighlightInaccessibleFiles");
            _chkHighlightUnmarried = SubstCtl<ICheckBox>(view, "chkHighlightUnmarried");
            _chkHighlightUnparented = SubstCtl<ICheckBox>(view, "chkHighlightUnparented");
            _chkInvertedTree = SubstCtl<ICheckBox>(view, "chkInvertedTree");
            _chkKeepInfoPansOverallSize = SubstCtl<ICheckBox>(view, "chkKeepInfoPansOverallSize");
            _chkKeepRichNames = SubstCtl<ICheckBox>(view, "chkKeepRichNames");
            _chkKinship = SubstCtl<ICheckBox>(view, "chkKinship");
            _chkLoadRecentFiles = SubstCtl<ICheckBox>(view, "chkLoadRecentFiles");
            _chkLocalizedCalendarSignatures = SubstCtl<ICheckBox>(view, "chkLocalizedCalendarSignatures");
            _chkMarriagesDates = SubstCtl<ICheckBox>(view, "chkMarriagesDates");
            _chkMinimizingWidth = SubstCtl<ICheckBox>(view, "chkMinimizingWidth");
            _chkMourningEdges = SubstCtl<ICheckBox>(view, "chkMourningEdges");
            _chkName = SubstCtl<ICheckBox>(view, "chkName");
            _chkNotes = SubstCtl<ICheckBox>(view, "chkNotes");
            _chkOnlyLocality = SubstCtl<ICheckBox>(view, "chkOnlyLocality");
            _chkOnlyYears = SubstCtl<ICheckBox>(view, "chkOnlyYears");
            _chkPatronymic = SubstCtl<ICheckBox>(view, "chkPatronymic");
            _chkPlacesWithAddress = SubstCtl<ICheckBox>(view, "chkPlacesWithAddress");
            _chkPortraits = SubstCtl<ICheckBox>(view, "chkPortraits");
            _chkPortraitsVisible = SubstCtl<ICheckBox>(view, "chkPortraitsVisible");
            _chkRemovableMediaWarning = SubstCtl<ICheckBox>(view, "chkRemovableMediaWarning");
            _chkReversePlacesOrder = SubstCtl<ICheckBox>(view, "chkReversePlacesOrder");
            _chkSAFByAllNames = SubstCtl<ICheckBox>(view, "chkSAFByAllNames");
            _chkSameCardsWidth = SubstCtl<ICheckBox>(view, "chkSameCardsWidth");
            _chkSearchPlacesWithoutCoords = SubstCtl<ICheckBox>(view, "chkSearchPlacesWithoutCoords");
            _chkSeparateDAPLines = SubstCtl<ICheckBox>(view, "chkSeparateDAPLines");
            _chkSeparateDepth = SubstCtl<ICheckBox>(view, "chkSeparateDepth");
            _chkShortenDateRanges = SubstCtl<ICheckBox>(view, "chkShortenDateRanges");
            _chkShortKinshipForm = SubstCtl<ICheckBox>(view, "chkShortKinshipForm");
            _chkShowAge = SubstCtl<ICheckBox>(view, "chkShowAge");
            _chkShowDatesCalendar = SubstCtl<ICheckBox>(view, "chkShowDatesCalendar");
            _chkShowDatesSigns = SubstCtl<ICheckBox>(view, "chkShowDatesSigns");
            _chkShowNumberOfSubstructures = SubstCtl<ICheckBox>(view, "chkShowNumberOfSubstructures");
            _chkShowOnStart = SubstCtl<ICheckBox>(view, "chkShowOnStart");
            _chkShowPlaces = SubstCtl<ICheckBox>(view, "chkShowPlaces");
            _chkSignsVisible = SubstCtl<ICheckBox>(view, "chkSignsVisible");
            _chkSimpleSingleSurnames = SubstCtl<ICheckBox>(view, "chkSimpleSingleSurnames");
            _chkSourceCitations = SubstCtl<ICheckBox>(view, "chkSourceCitations");
            _chkSourcePages = SubstCtl<ICheckBox>(view, "chkSourcePages");
            _chkSources = SubstCtl<ICheckBox>(view, "chkSources");
            _chkSurname = SubstCtl<ICheckBox>(view, "chkSurname");
            _chkSurnameFirstInOrder = SubstCtl<ICheckBox>(view, "chkSurnameFirstInOrder");
            _chkSurnameInCapitals = SubstCtl<ICheckBox>(view, "chkSurnameInCapitals");
            _chkTreeDecorative = SubstCtl<ICheckBox>(view, "chkTreeDecorative");
            _chkTreeSurnameFirst = SubstCtl<ICheckBox>(view, "chkTreeSurnameFirst");
            _chkURNotesVisible = SubstCtl<ICheckBox>(view, "chkURNotesVisible");
            _chkUseAdditionalDates = SubstCtl<ICheckBox>(view, "chkUseAdditionalDates");
            _chkUseBirthDatesInPSF = SubstCtl<ICheckBox>(view, "chkUseBirthDatesInPSF");
            _chkUseExtendedNotes = SubstCtl<ICheckBox>(view, "chkUseExtendedNotes");
            _chkUseExtraControls = SubstCtl<ICheckBox>(view, "chkUseExtraControls");
            _chkUseInlineImagesInSvg = SubstCtl<ICheckBox>(view, "chkUseInlineImagesInSvg");
            _chkUseProxy = SubstCtl<ICheckBox>(view, "chkUseProxy");
            _chkUseSurnamesInPSF = SubstCtl<ICheckBox>(view, "chkUseSurnamesInPSF");
            _numASMin = SubstCtl<INumericBox>(view, "numASMin");
            _numBackupRevisionsMaxCount = SubstCtl<INumericBox>(view, "numBackupRevisionsMaxCount");
            _numBranchDist = SubstCtl<INumericBox>(view, "numBranchDist");
            _numDefaultDepth = SubstCtl<INumericBox>(view, "numDefaultDepth");
            _numDefaultDepthAncestors = SubstCtl<INumericBox>(view, "numDefaultDepthAncestors");
            _numDefaultDepthDescendants = SubstCtl<INumericBox>(view, "numDefaultDepthDescendants");
            _numGenDist = SubstCtl<INumericBox>(view, "numGenDist");
            _numMargins = SubstCtl<INumericBox>(view, "numMargins");
            _numPadding = SubstCtl<INumericBox>(view, "numPadding");
            _numSpouseDist = SubstCtl<INumericBox>(view, "numSpouseDist");
            _radCompact = SubstCtl<IRadioButton>(view, "radCompact");
            _radDMY = SubstCtl<IRadioButton>(view, "radDMY");
            _radExcess = SubstCtl<IRadioButton>(view, "radExcess");
            _radFBEachRevision = SubstCtl<IRadioButton>(view, "radFBEachRevision");
            _radFBNone = SubstCtl<IRadioButton>(view, "radFBNone");
            _radFBOnlyPrev = SubstCtl<IRadioButton>(view, "radFBOnlyPrev");
            _radMaiden = SubstCtl<IRadioButton>(view, "radMaiden");
            _radMaiden_Married = SubstCtl<IRadioButton>(view, "radMaiden_Married");
            _radMarried = SubstCtl<IRadioButton>(view, "radMarried");
            _radMarried_Maiden = SubstCtl<IRadioButton>(view, "radMarried_Maiden");
            _radS_N_P = SubstCtl<IRadioButton>(view, "radS_N_P");
            _radS_NP = SubstCtl<IRadioButton>(view, "radS_NP");
            _radSNP = SubstCtl<IRadioButton>(view, "radSNP");
            _radYMD = SubstCtl<IRadioButton>(view, "radYMD");
            _lstPersonColumns = SubstCtl<IListView>(view, "lstPersonColumns");

            // controls absent in some form variants, accessed by string name
            SubstituteControl<IButton>(view, "btnExtBackupFolderChoose");
            SubstituteControl<ICheckBox>(view, "chkAutoSortEvents");
            SubstituteControl<ICheckBox>(view, "chkConvenientSourcesFormat");
            SubstituteControl<ICheckBox>(view, "chkDisplayFullFileName");
            _chkExtBackupEnabled = SubstCtl<ICheckBox>(view, "chkExtBackupEnabled");
            SubstituteControl<ICheckBox>(view, "chkFBIncludeEvents");
            SubstituteControl<ICheckBox>(view, "chkFBIncludeFamilyEvents");
            SubstituteControl<ICheckBox>(view, "chkFBIncludeNotes");
            SubstituteControl<ICheckBox>(view, "chkFBMergeNotes");
            SubstituteControl<ICheckBox>(view, "chkMultipleSpouseLines");
            SubstituteControl<ICheckBox>(view, "chkOverrideThemesFont");
            SubstituteControl<ICheckBox>(view, "chkShowEventsOnlyAlive");
            SubstituteControl<ICheckBox>(view, "chkSortEvents");
            SubstituteControl<ICheckBox>(view, "chkSourceFilterChecksEvents");
            SubstituteControl<ILabel>(view, "lblChartFontTitle");
            SubstituteControl<ILabel>(view, "lblExtBackupFolder");
            SubstituteControl<ILabel>(view, "lblUIFont");
            SubstituteControl<ILabel>(view, "lblUIFontTitle");
            SubstituteControl<IListView>(view, "lvPlugins");
            SubstituteControl<ITabPage>(view, "pageComBackup");
            SubstituteControl<ITabPage>(view, "pageComOther");
            SubstituteControl<ITabPage>(view, "pageFamilyBook");
            SubstituteControl<ITabPage>(view, "pageNames");
            SubstituteControl<ITabPage>(view, "pagePlugins");
            SubstituteControl<ITabPage>(view, "pageReports");
            _txtExtBackupFolder = SubstCtl<ITextBox>(view, "txtExtBackupFolder");

            return view;
        }
    }
}
