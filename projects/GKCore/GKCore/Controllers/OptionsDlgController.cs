/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Threading.Tasks;
using GDModel;
using GKCore.Backups;
using GKCore.Charts;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Export;
using GKCore.Filters;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Media;
using GKCore.Names;
using GKCore.Options;
using GKCore.Plugins;
using GKUI.Themes;

namespace GKCore.Controllers
{
    public class OptionsDlgController : DialogController<IOptionsDlg>
    {
        // Note: fields are not created for controls absent in the GKv2 or GKvX forms,
        // they are accessed by string name:
        //   only GKv3 (Feature.DesktopV3): chkAutoSortEvents, chkConvenientSourcesFormat, chkFBIncludeEvents,
        //   chkFBIncludeFamilyEvents, chkFBIncludeNotes, chkFBMergeNotes, chkMultipleSpouseLines,
        //   chkShowEventsOnlyAlive, chkSortEvents, chkSourceFilterChecksEvents, pageFamilyBook, pageReports
        //   absent in GKv3 (groups are dissolved into the left category list): pageCommon, pageUIView,
        //   pageCharts, pageTreeChart
        //   absent in GKvX: btnExtBackupFolderChoose, chkDisplayFullFileName, chkExtBackupEnabled,
        //   chkOverrideThemesFont, lblChartFontTitle, lblExtBackupFolder, lblUIFont, lblUIFontTitle,
        //   lvPlugins, pageComBackup, pageComOther, pageNames, pagePlugins, txtExtBackupFolder
        #region Design components
#pragma warning disable CS0649, IDE0044

        private ITabPage _pageMultimedia;
        private ITabPage _pageEventTypes;
        private ITabPage _pagePedigree;
        private ITabPage _pageSpecials;
        private ITabPage _pageGeo;
        private ITabPage _pageNavigation;
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

#pragma warning restore CS0649, IDE0044
        #endregion

        private readonly GlobalOptions fOptions;
        private readonly ListColumns fTempColumns;
        private bool fIsTerm;


        public GlobalOptions Options
        {
            get { return fOptions; }
        }


        public OptionsDlgController(IOptionsDlg view) : base(view)
        {
            fIsTerm = !AppHost.Instance.HasFeatureSupport(Feature.Graphics);
            fOptions = GlobalOptions.Instance;
            fTempColumns = IndividualListModel.CreateListColumns();
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            FillGeoSearchCountries();

            fView.EventTypesList.ListModel = new EventDefsListModel(fView, null, null);

            var columnsModel = new ColumnsListModel();
            _lstPersonColumns.ListMan = columnsModel;
            columnsModel.DataSource = fTempColumns.OrderedColumns;

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                var listView = GetControl<IListView>("lvPlugins");
                var pluginsModel = new PluginsListModel();
                pluginsModel.DataSource = AppHost.Plugins.List;
                listView.ListMan = pluginsModel;
            }

            _cmbLanguages.ReadOnly = true;
            _cmbCertaintyAlgorithm.ReadOnly = true;
            _cmbMediaStoreDefault.ReadOnly = true;

            if (!fIsTerm) {
                _cmbChartWindowsShowMode.ReadOnly = true;
                _cmbTextEffect.ReadOnly = true;
            }

            _cmbMatchPatternMethod.ReadOnly = true;
            _cmbAscendNumbering.ReadOnly = true;
            _cmbDescendNumbering.ReadOnly = true;
            _cmbGeocoder.ReadOnly = true;
        }

        public async void ChangeTab()
        {
            await AcceptLanguage();
            SetLocale();
            UpdateEventTypes();
        }

        private void FillGeoSearchCountries()
        {
            var combo = _cmbGeoSearchCountry;

            combo.Clear();
            foreach (var ci in GKUtils.GetCountries()) {
                combo.Add(ci);
            }
        }

        public void UpdateColumnsList()
        {
            _lstPersonColumns.SortColumn = 1;
            _lstPersonColumns.SortOrder = GKSortOrder.Ascending;
            _lstPersonColumns.UpdateContents();
        }

        public void ResetColumnsList()
        {
            fTempColumns.ResetDefaults();
            UpdateColumnsList();
        }

        public void MoveColumnUp()
        {
            if (fTempColumns.MoveColumn(_lstPersonColumns.SelectedIndex, true)) {
                UpdateColumnsList();
            }
        }

        public void MoveColumnDown()
        {
            if (fTempColumns.MoveColumn(_lstPersonColumns.SelectedIndex, false)) {
                UpdateColumnsList();
            }
        }

        public void AcceptColumnsList()
        {
            fTempColumns.CopyTo(fOptions.ListOptions[GDMRecordType.rtIndividual].Columns);
        }

        public void UpdateProxyOptions()
        {
            if (fIsTerm) return;

            var isMobile = AppHost.Instance.HasFeatureSupport(Feature.Mobile);
            _grpInternet.Visible = !isMobile;

            var hasInternetProxy = AppHost.Instance.HasFeatureSupport(Feature.InternetProxy);
            _grpInternet.Enabled = hasInternetProxy;

            _chkUseProxy.Checked = fOptions.Proxy.UseProxy && hasInternetProxy;
            _txtProxyServer.Text = fOptions.Proxy.Server;
            _txtProxyPort.Text = fOptions.Proxy.Port;
            _txtProxyLogin.Text = fOptions.Proxy.Login;
            _txtProxyPass.Text = fOptions.Proxy.Password;
        }

        public void AcceptProxyOptions()
        {
            if (fIsTerm) return;

            fOptions.Proxy.UseProxy = _chkUseProxy.Checked;
            fOptions.Proxy.Server = _txtProxyServer.Text;
            fOptions.Proxy.Port = _txtProxyPort.Text;
            fOptions.Proxy.Login = _txtProxyLogin.Text;
            fOptions.Proxy.Password = _txtProxyPass.Text;
        }

        public void ResetCommonOptions()
        {
            fOptions.ResetDefaults_Common();
            UpdateProxyOptions();
            UpdateBackupOptions();
            UpdateOtherOptions();
        }

        public void UpdateOtherOptions()
        {
            _chkShowOnStart.Checked = fOptions.ShowTips;
            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                GetControl<ICheckBox>("chkShowEventsOnlyAlive").Checked = fOptions.ShowTipsOnlyAlive;
            }

            var hasRecentFilesLoad = AppHost.Instance.HasFeatureSupport(Feature.RecentFilesLoad);
            _chkLoadRecentFiles.Checked = fOptions.LoadRecentFiles && hasRecentFilesLoad;
            _chkLoadRecentFiles.Enabled = hasRecentFilesLoad;

            _chkAutoCheckUpdates.Checked = fOptions.AutoCheckUpdates;
            _chkCharsetDetection.Checked = fOptions.CharsetDetection;
            _chkDialogClosingWarn.Checked = fOptions.DialogClosingWarn;

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ICheckBox>("chkDisplayFullFileName").Checked = fOptions.DisplayFullFileName;
            }

            _cmbGeocoder.Text = fOptions.Geocoder;
            _cmbGeoSearchCountry.Text = fOptions.GeoSearchCountry;

            var combo = _cmbLanguages;
            combo.Clear();
            combo.AddItem(LangMan.LS_DEF_NAME, LangMan.LS_DEF_CODE);
            foreach (LangRecord lngRec in GlobalOptions.Instance.Languages) {
                if (lngRec.Code != LangMan.LS_DEF_CODE) {
                    combo.AddItem(lngRec.Name, lngRec.Code);
                }
            }
            combo.SetSelectedTag(fOptions.InterfaceLang, true);

            _cmbCertaintyAlgorithm.SetSelectedTag(fOptions.CertaintyAlgorithm);
        }

        public async Task AcceptLanguage()
        {
            var item = _cmbLanguages.SelectedItem as ComboItem<int>;
            if (item != null) {
                await AppHost.Instance.LoadLanguage(item.Tag, false);
            }
        }

        public void ChangeOtherOption()
        {
            bool tips = _chkShowOnStart.Checked;
            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                GetControl<ICheckBox>("chkShowEventsOnlyAlive").Enabled = tips;
            }
        }

        public async void AcceptOtherOptions()
        {
            fOptions.ShowTips = _chkShowOnStart.Checked;
            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                fOptions.ShowTipsOnlyAlive = GetControl<ICheckBox>("chkShowEventsOnlyAlive").Checked;
            }

            fOptions.LoadRecentFiles = _chkLoadRecentFiles.Checked;
            fOptions.AutoCheckUpdates = _chkAutoCheckUpdates.Checked;
            fOptions.CharsetDetection = _chkCharsetDetection.Checked;
            fOptions.DialogClosingWarn = _chkDialogClosingWarn.Checked;

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                fOptions.DisplayFullFileName = GetControl<ICheckBox>("chkDisplayFullFileName").Checked;
            }

            fOptions.Geocoder = _cmbGeocoder.Text;
            fOptions.GeoSearchCountry = _cmbGeoSearchCountry.Text;

            await AcceptLanguage();

            fOptions.CertaintyAlgorithm = _cmbCertaintyAlgorithm.GetSelectedTag<CertaintyAlgorithm>();
        }

        public void ResetPedigreesOptions()
        {
            fOptions.PedigreeOptions.ResetDefaults();
            UpdatePedigreesOptions();
        }

        private void PrepareNumbCombo(IComboBox combo, PedigreeType pedigreeType, PedigreeNumbering curValue)
        {
            combo.Clear();
            for (PedigreeNumbering pn = PedigreeNumbering.Aboville; pn <= PedigreeNumbering.Sosa_Stradonitz; pn++) {
                var pnStr = PedigreeData.Numberings[(int)pn];
                if (pnStr.Type == pedigreeType) {
                    combo.AddItem(LangMan.LS(pnStr.Name), pn);
                }
            }
            combo.SetSelectedTag(curValue);
        }

        public void UpdatePedigreesOptions()
        {
            _chkAttributes.Checked = fOptions.PedigreeOptions.IncludeAttributes;
            _chkNotes.Checked = fOptions.PedigreeOptions.IncludeNotes;
            _chkSources.Checked = fOptions.PedigreeOptions.IncludeSources;
            _chkSourcePages.Checked = fOptions.PedigreeOptions.IncludeSourcePages;
            _chkSourceCitations.Checked = fOptions.PedigreeOptions.IncludeSourceCitations;
            _chkGenerations.Checked = fOptions.PedigreeOptions.IncludeGenerations;
            _chkPortraits.Checked = fOptions.PedigreeOptions.IncludePortraits;

            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                GetControl<ICheckBox>("chkSortEvents").Checked = fOptions.PedigreeOptions.SortEvents;
            }

            switch (fOptions.PedigreeOptions.Format) {
                case PedigreeFormat.Excess:
                    _radExcess.Checked = true;
                    break;
                case PedigreeFormat.Compact:
                    _radCompact.Checked = true;
                    break;
            }

            PrepareNumbCombo(_cmbAscendNumbering, PedigreeType.Ascend, fOptions.PedigreeOptions.AscendNumbering);
            PrepareNumbCombo(_cmbDescendNumbering, PedigreeType.Descend, fOptions.PedigreeOptions.DescendNumbering);
        }

        public void AcceptPedigreesOptions()
        {
            fOptions.PedigreeOptions.IncludeAttributes = _chkAttributes.Checked;
            fOptions.PedigreeOptions.IncludeNotes = _chkNotes.Checked;
            fOptions.PedigreeOptions.IncludeSources = _chkSources.Checked;
            fOptions.PedigreeOptions.IncludeSourcePages = _chkSourcePages.Checked;
            fOptions.PedigreeOptions.IncludeSourceCitations = _chkSourceCitations.Checked;
            fOptions.PedigreeOptions.IncludeGenerations = _chkGenerations.Checked;
            fOptions.PedigreeOptions.IncludePortraits = _chkPortraits.Checked;

            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                fOptions.PedigreeOptions.SortEvents = GetControl<ICheckBox>("chkSortEvents").Checked;
            }

            if (_radExcess.Checked) {
                fOptions.PedigreeOptions.Format = PedigreeFormat.Excess;
            } else if (_radCompact.Checked) {
                fOptions.PedigreeOptions.Format = PedigreeFormat.Compact;
            }

            fOptions.PedigreeOptions.AscendNumbering = _cmbAscendNumbering.GetSelectedTag<PedigreeNumbering>();
            fOptions.PedigreeOptions.DescendNumbering = _cmbDescendNumbering.GetSelectedTag<PedigreeNumbering>();
        }

        public void ResetFamilyBookOptions()
        {
            fOptions.FamilyBookOptions.ResetDefaults();
            UpdateFamilyBookOptions();
        }

        public void UpdateFamilyBookOptions()
        {
            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                GetControl<ICheckBox>("chkFBIncludeEvents").Checked = fOptions.FamilyBookOptions.IncludeEvents;
                GetControl<ICheckBox>("chkFBIncludeFamilyEvents").Checked = fOptions.FamilyBookOptions.IncludeFamilyEvents;
                GetControl<ICheckBox>("chkFBIncludeNotes").Checked = fOptions.FamilyBookOptions.IncludeNotes;
                GetControl<ICheckBox>("chkFBMergeNotes").Checked = fOptions.FamilyBookOptions.MergeNotes;
            }
        }

        public void AcceptFamilyBookOptions()
        {
            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                fOptions.FamilyBookOptions.IncludeEvents = GetControl<ICheckBox>("chkFBIncludeEvents").Checked;
                fOptions.FamilyBookOptions.IncludeFamilyEvents = GetControl<ICheckBox>("chkFBIncludeFamilyEvents").Checked;
                fOptions.FamilyBookOptions.IncludeNotes = GetControl<ICheckBox>("chkFBIncludeNotes").Checked;
                fOptions.FamilyBookOptions.MergeNotes = GetControl<ICheckBox>("chkFBMergeNotes").Checked;
            }
        }

        public void UpdateWomanSurnameFormat()
        {
            WomanSurnameFormat wsFmt = fOptions.WomanSurnameFormat;
            bool isExtend = wsFmt != WomanSurnameFormat.wsfNotExtend;

            _chkExtendWomanSurnames.Checked = isExtend;
            _radMaiden_Married.Enabled = isExtend;
            _radMarried_Maiden.Enabled = isExtend;
            _radMaiden.Enabled = isExtend;
            _radMarried.Enabled = isExtend;

            switch (wsFmt) {
                case WomanSurnameFormat.wsfMaiden_Married:
                    _radMaiden_Married.Checked = true;
                    break;

                case WomanSurnameFormat.wsfMarried_Maiden:
                    _radMarried_Maiden.Checked = true;
                    break;

                case WomanSurnameFormat.wsfMaiden:
                    _radMaiden.Checked = true;
                    break;

                case WomanSurnameFormat.wsfMarried:
                    _radMarried.Checked = true;
                    break;
            }

            _chkSimpleSingleSurnames.Checked = isExtend && fOptions.SimpleSingleSurnames;
            _chkSimpleSingleSurnames.Enabled = isExtend;
        }

        public void ChangeExtendWomanSurnames()
        {
            if (!_chkExtendWomanSurnames.Checked) {
                fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfNotExtend;
                UpdateWomanSurnameFormat();
            } else {
                _radMaiden_Married.Enabled = true;
                _radMarried_Maiden.Enabled = true;
                _radMaiden.Enabled = true;
                _radMarried.Enabled = true;
                _chkSimpleSingleSurnames.Enabled = true;
            }
        }

        public void AcceptWomanSurnameFormat()
        {
            if (!_chkExtendWomanSurnames.Checked) {
                fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfNotExtend;
            } else {
                if (_radMaiden_Married.Checked) {
                    fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfMaiden_Married;
                } else if (_radMarried_Maiden.Checked) {
                    fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfMarried_Maiden;
                } else if (_radMaiden.Checked) {
                    fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfMaiden;
                } else if (_radMarried.Checked) {
                    fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfMarried;
                }
            }
            fOptions.SimpleSingleSurnames = _chkSimpleSingleSurnames.Checked;
        }

        public void ResetInterfaceOptions()
        {
            fOptions.ResetDefaults_Interface();
            UpdateInterfaceOptions();
            UpdateWomanSurnameFormat();
        }

        public void UpdateInterfaceOptions()
        {
            switch (fOptions.DefNameFormat) {
                case NameFormat.nfFNP:
                    _radSNP.Checked = true;
                    break;
                case NameFormat.nfF_NP:
                    _radS_NP.Checked = true;
                    break;
                case NameFormat.nfF_N_P:
                    _radS_N_P.Checked = true;
                    break;
            }

            switch (fOptions.DefDateFormat) {
                case DateFormat.dfDD_MM_YYYY:
                    _radDMY.Checked = true;
                    break;
                case DateFormat.dfYYYY_MM_DD:
                    _radYMD.Checked = true;
                    break;
            }

            _chkShowDatesCalendar.Checked = fOptions.ShowDatesCalendar;
            _chkShowDatesSigns.Checked = fOptions.ShowDatesSign;
            _chkLocalizedCalendarSignatures.Checked = fOptions.LocalizedCalendarSignatures;

            _chkPlacesWithAddress.Checked = fOptions.PlacesWithAddress;

            var hasGridCellFormat = AppHost.Instance.HasFeatureSupport(Feature.GridCellFormat);
            _chkHighlightUnparented.Checked = fOptions.ListHighlightUnparentedPersons && hasGridCellFormat;
            _chkHighlightUnparented.Enabled = hasGridCellFormat;
            _chkHighlightUnmarried.Checked = fOptions.ListHighlightUnmarriedPersons && hasGridCellFormat;
            _chkHighlightUnmarried.Enabled = hasGridCellFormat;

            _chkAutoSortChildren.Checked = fOptions.AutoSortChildren;
            _chkAutoSortSpouses.Checked = fOptions.AutoSortSpouses;
            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                GetControl<ICheckBox>("chkAutoSortEvents").Checked = fOptions.AutoSortEvents;
            }
            _chkFirstCapitalLetterInNames.Checked = fOptions.FirstCapitalLetterInNames;

            _chkShortKinshipForm.Checked = fOptions.ShortKinshipForm;
            _chkSurnameFirstInOrder.Checked = fOptions.SurnameFirstInOrder;
            _chkSurnameInCapitals.Checked = fOptions.SurnameInCapitals;

            _chkUseSurnamesInPSF.Checked = fOptions.UseSurnamesInPersonSelectionFilter;
            _chkUseBirthDatesInPSF.Checked = fOptions.UseBirthDatesInPersonSelectionFilter;

            var combo = _cmbMatchPatternMethod;
            combo.Clear();
            for (MatchPatternMethod itm = MatchPatternMethod.RegEx; itm <= MatchPatternMethod.Fast; itm++) {
                combo.AddItem(LangMan.LS(GKData.MatchPatterns[(int)itm].Name), itm);
            }
            combo.SetSelectedTag(fOptions.MatchPatternMethod);

            if (!fIsTerm && !AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                UpdateUIFont();
                GetControl<ICheckBox>("chkOverrideThemesFont").Checked = fOptions.OverrideThemesFont;
            }

            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                GetControl<ICheckBox>("chkSourceFilterChecksEvents").Checked = fOptions.SourceFilterChecksEvents;
                GetControl<ICheckBox>("chkConvenientSourcesFormat").Checked = fOptions.ConvenientSourcesFormat;
            }
        }

        public NameFormat GetSelectedNameFormat()
        {
            NameFormat result;
            if (_radSNP.Checked) {
                result = NameFormat.nfFNP;
            } else if (_radS_NP.Checked) {
                result = NameFormat.nfF_NP;
            } else if (_radS_N_P.Checked) {
                result = NameFormat.nfF_N_P;
            } else {
                result = NameFormat.nfFNP;
            }
            return result;
        }

        public void ChangeFNPFormat()
        {
            var defNameFormat = GetSelectedNameFormat();
            _chkSurnameFirstInOrder.Enabled = (defNameFormat == NameFormat.nfFNP);
        }

        public void AcceptInterfaceOptions()
        {
            fOptions.DefNameFormat = GetSelectedNameFormat();

            if (_radDMY.Checked) {
                fOptions.DefDateFormat = DateFormat.dfDD_MM_YYYY;
            } else if (_radYMD.Checked) {
                fOptions.DefDateFormat = DateFormat.dfYYYY_MM_DD;
            }
            fOptions.ShowDatesCalendar = _chkShowDatesCalendar.Checked;
            fOptions.ShowDatesSign = _chkShowDatesSigns.Checked;
            fOptions.LocalizedCalendarSignatures = _chkLocalizedCalendarSignatures.Checked;

            fOptions.PlacesWithAddress = _chkPlacesWithAddress.Checked;
            fOptions.ListHighlightUnparentedPersons = _chkHighlightUnparented.Checked;
            fOptions.ListHighlightUnmarriedPersons = _chkHighlightUnmarried.Checked;

            fOptions.AutoSortChildren = _chkAutoSortChildren.Checked;
            fOptions.AutoSortSpouses = _chkAutoSortSpouses.Checked;
            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                fOptions.AutoSortEvents = GetControl<ICheckBox>("chkAutoSortEvents").Checked;
            }
            fOptions.FirstCapitalLetterInNames = _chkFirstCapitalLetterInNames.Checked;

            fOptions.ShortKinshipForm = _chkShortKinshipForm.Checked;
            fOptions.SurnameFirstInOrder = _chkSurnameFirstInOrder.Checked;
            fOptions.SurnameInCapitals = _chkSurnameInCapitals.Checked;

            fOptions.UseSurnamesInPersonSelectionFilter = _chkUseSurnamesInPSF.Checked;
            fOptions.UseBirthDatesInPersonSelectionFilter = _chkUseBirthDatesInPSF.Checked;

            fOptions.MatchPatternMethod = _cmbMatchPatternMethod.GetSelectedTag<MatchPatternMethod>();

            if (!fIsTerm && !AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                fOptions.OverrideThemesFont = GetControl<ICheckBox>("chkOverrideThemesFont").Checked;
            }

            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                fOptions.SourceFilterChecksEvents = GetControl<ICheckBox>("chkSourceFilterChecksEvents").Checked;
                fOptions.ConvenientSourcesFormat = GetControl<ICheckBox>("chkConvenientSourcesFormat").Checked;
            }
        }

        public void ResetSpecialsOptions()
        {
            fOptions.ResetDefaults_Specials();
            UpdateSpecials();
        }

        public void UpdateSpecials()
        {
            _chkDisableNonStdFeatures.Checked = fOptions.DisableNonStdFeatures;
            _chkEnableStdValidation.Checked = fOptions.EnableStdValidation;

            _chkUseInlineImagesInSvg.Checked = fOptions.TreeChartOptions.UseInlineImagesInSvg;
            _chkExtendedTree.Checked = fOptions.TreeChartOptions.ExtendedTree;
            _chkExtendedKinships.Checked = fOptions.ExtendedKinships;
            _chkSAFByAllNames.Checked = fOptions.SearchAndFilterByAllNames;

            var isMobile = AppHost.Instance.HasFeatureSupport(Feature.Mobile);
            _chkUseExtendedNotes.Checked = fOptions.UseExtendedNotes && !isMobile;
            _chkUseExtendedNotes.Visible = !isMobile;
            _chkKeepRichNames.Checked = fOptions.KeepRichNames && !isMobile;
            _chkKeepRichNames.Visible = !isMobile;

            if (!fIsTerm) {
                var chartWinMode = !isMobile ? fOptions.ChartWindowsShowMode : ChartWindowsShowMode.Default;
                _cmbChartWindowsShowMode.SetSelectedTag(chartWinMode);
                _cmbChartWindowsShowMode.Visible = !isMobile;
                _lblChartWindowsShowMode.Visible = !isMobile;
            }

            _chkKeepInfoPansOverallSize.Checked = fOptions.KeepInfoPansOverallSize && !isMobile;
            _chkKeepInfoPansOverallSize.Visible = !isMobile;

            var hasOverwritePrompt = AppHost.Instance.HasFeatureSupport(Feature.OverwritePrompt);
            _chkFilesOverwriteWarn.Checked = fOptions.FilesOverwriteWarn && hasOverwritePrompt;
            _chkFilesOverwriteWarn.Enabled = hasOverwritePrompt;

            _chkExtendedLocations.Checked = fOptions.ExtendedLocations;
            _chkELAbbreviatedNames.Checked = fOptions.EL_AbbreviatedNames;

            _chkReversePlacesOrder.Checked = fOptions.ReversePlaceEntitiesOrder;
            _chkShowNumberOfSubstructures.Checked = fOptions.ShowNumberOfSubstructures;
            _chkSearchPlacesWithoutCoords.Checked = fOptions.SearchPlacesWithoutCoords;
        }

        public void AcceptSpecials()
        {
            fOptions.DisableNonStdFeatures = _chkDisableNonStdFeatures.Checked;
            fOptions.EnableStdValidation = _chkEnableStdValidation.Checked;

            fOptions.TreeChartOptions.UseInlineImagesInSvg = _chkUseInlineImagesInSvg.Checked;
            fOptions.TreeChartOptions.ExtendedTree = _chkExtendedTree.Checked;
            fOptions.ExtendedKinships = _chkExtendedKinships.Checked;
            fOptions.SearchAndFilterByAllNames = _chkSAFByAllNames.Checked;

            fOptions.UseExtendedNotes = _chkUseExtendedNotes.Checked;
            fOptions.KeepRichNames = _chkKeepRichNames.Checked;

            if (!fIsTerm) {
                fOptions.ChartWindowsShowMode = _cmbChartWindowsShowMode.GetSelectedTag<ChartWindowsShowMode>();
            }

            fOptions.KeepInfoPansOverallSize = _chkKeepInfoPansOverallSize.Checked;

            fOptions.FilesOverwriteWarn = _chkFilesOverwriteWarn.Checked;

            fOptions.ExtendedLocations = _chkExtendedLocations.Checked;
            fOptions.EL_AbbreviatedNames = _chkELAbbreviatedNames.Checked;

            fOptions.ReversePlaceEntitiesOrder = _chkReversePlacesOrder.Checked;
            fOptions.ShowNumberOfSubstructures = _chkShowNumberOfSubstructures.Checked;
            fOptions.SearchPlacesWithoutCoords = _chkSearchPlacesWithoutCoords.Checked;
        }

        private void UpdateEventTypes()
        {
            fView.EventTypesList.ListModel.DataOwner = AppHost.EventDefinitions;
        }

        public void UpdatePlugins()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                var listView = GetControl<IListView>("lvPlugins");
                listView.UpdateContents();
            }
        }

        public void AcceptPlugins()
        {
            // dummy for future
        }

        public void ResetMediaOptions()
        {
            fOptions.ResetDefaults_Media();
            UpdateMediaOptions();
        }

        public void UpdateMediaOptions()
        {
            _chkRemovableMediaWarning.Checked = fOptions.RemovableMediaWarning;

            var hasMediaPlayer = AppHost.Instance.HasFeatureSupport(Feature.MediaPlayer);
            _chkEmbeddedMediaPlayer.Checked = fOptions.EmbeddedMediaPlayer && hasMediaPlayer;
            _chkEmbeddedMediaPlayer.Enabled = hasMediaPlayer;

            _chkAllowMediaDirectRefs.Checked = fOptions.AllowMediaStoreReferences;
            _chkAllowMediaStoreRelativeReferences.Checked = fOptions.AllowMediaStoreRelativeReferences;
            _cmbMediaStoreDefault.SetSelectedTag<MediaStoreType>(fOptions.MediaStoreDefault);
            _chkAllowDeleteMediaFileFromStgArc.Checked = fOptions.AllowDeleteMediaFileFromStgArc;
            _chkAllowDeleteMediaFileFromRefs.Checked = fOptions.AllowDeleteMediaFileFromRefs;
            _chkDeleteMediaFileWithoutConfirm.Checked = fOptions.DeleteMediaFileWithoutConfirm;

            var hasGridCellFormat = AppHost.Instance.HasFeatureSupport(Feature.GridCellFormat);
            _chkHighlightInaccessibleFiles.Checked = fOptions.HighlightInaccessibleFiles && hasGridCellFormat;
            _chkHighlightInaccessibleFiles.Enabled = hasGridCellFormat;
        }

        public void AcceptMediaOptions()
        {
            fOptions.RemovableMediaWarning = _chkRemovableMediaWarning.Checked;
            fOptions.EmbeddedMediaPlayer = _chkEmbeddedMediaPlayer.Checked;
            fOptions.AllowMediaStoreReferences = _chkAllowMediaDirectRefs.Checked;
            fOptions.AllowMediaStoreRelativeReferences = _chkAllowMediaStoreRelativeReferences.Checked;
            fOptions.MediaStoreDefault = _cmbMediaStoreDefault.GetSelectedTag<MediaStoreType>();
            fOptions.AllowDeleteMediaFileFromStgArc = _chkAllowDeleteMediaFileFromStgArc.Checked;
            fOptions.AllowDeleteMediaFileFromRefs = _chkAllowDeleteMediaFileFromRefs.Checked;
            fOptions.DeleteMediaFileWithoutConfirm = _chkDeleteMediaFileWithoutConfirm.Checked;
            fOptions.HighlightInaccessibleFiles = _chkHighlightInaccessibleFiles.Checked;
        }

        public void UpdateBackupOptions()
        {
            var backupOpts = fOptions.Backups;

            switch (backupOpts.FileBackup) {
                case FileBackup.fbNone:
                    _radFBNone.Checked = true;
                    break;
                case FileBackup.fbOnlyPrev:
                    _radFBOnlyPrev.Checked = true;
                    break;
                case FileBackup.fbEachRevision:
                    _radFBEachRevision.Checked = true;
                    break;
            }

            _chkAutosave.Checked = backupOpts.Autosave;
            _numASMin.Value = backupOpts.AutosaveInterval;
            _numBackupRevisionsMaxCount.Value = backupOpts.FileBackupEachRevisionMaxCount;

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ICheckBox>("chkExtBackupEnabled").Checked = backupOpts.ExtendedBackup;
                GetControl<ITextBox>("txtExtBackupFolder").Text = backupOpts.ExtendedFolder;
            }
        }

        public void AcceptBackupOptions()
        {
            var backupOpts = fOptions.Backups;

            if (_radFBNone.Checked) {
                backupOpts.FileBackup = FileBackup.fbNone;
            } else if (_radFBOnlyPrev.Checked) {
                backupOpts.FileBackup = FileBackup.fbOnlyPrev;
            } else if (_radFBEachRevision.Checked) {
                backupOpts.FileBackup = FileBackup.fbEachRevision;
            }

            backupOpts.Autosave = _chkAutosave.Checked;
            backupOpts.AutosaveInterval = (int)_numASMin.Value;
            backupOpts.FileBackupEachRevisionMaxCount = (int)_numBackupRevisionsMaxCount.Value;

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                backupOpts.ExtendedBackup = GetControl<ICheckBox>("chkExtBackupEnabled").Checked;
                backupOpts.ExtendedFolder = GetControl<ITextBox>("txtExtBackupFolder").Text;
            }
        }

        public void CheckExtBackup()
        {
            var enable = GetControl<ICheckBox>("chkExtBackupEnabled").Checked;
            GetControl<ITextBox>("txtExtBackupFolder").Enabled = enable;
            GetControl<IButton>("btnExtBackupFolderChoose").Enabled = enable;
        }

        public async void SelectExtBackupFolder()
        {
            var selectedFolder = await AppHost.StdDialogs.SelectFolder(fOptions.Backups.ExtendedFolder);
            if (!string.IsNullOrEmpty(selectedFolder)) {
                GetControl<ITextBox>("txtExtBackupFolder").Text = selectedFolder;
            }
        }

        public async void SelectLabColor(ILabel lbl)
        {
            if (lbl != null) {
                lbl.BackColor = await AppHost.StdDialogs.SelectColor(lbl.BackColor);
            }
        }

        public void ResetTreeChartsOptions()
        {
            fOptions.TreeChartOptions.ResetDefaults();
            UpdateTreeChartsOptions();
        }

        public void UpdateTreeChartsOptions()
        {
            _chkSurname.Checked = fOptions.TreeChartOptions.FamilyVisible;
            _chkName.Checked = fOptions.TreeChartOptions.NameVisible;
            _chkPatronymic.Checked = fOptions.TreeChartOptions.PatronymicVisible;
            _chkDiffLines.Checked = fOptions.TreeChartOptions.DiffLines;
            _chkBirthDate.Checked = fOptions.TreeChartOptions.BirthDateVisible;
            _chkDeathDate.Checked = fOptions.TreeChartOptions.DeathDateVisible;
            _chkOnlyYears.Checked = fOptions.TreeChartOptions.OnlyYears;
            _chkMarriagesDates.Checked = fOptions.TreeChartOptions.MarriagesDates;
            _chkKinship.Checked = fOptions.TreeChartOptions.Kinship;
            _chkSignsVisible.Checked = fOptions.TreeChartOptions.SignsVisible;
            _chkTreeDecorative.Checked = fOptions.TreeChartOptions.Decorative;
            _chkPortraitsVisible.Checked = fOptions.TreeChartOptions.PortraitsVisible;
            _chkDefaultPortraits.Checked = fOptions.TreeChartOptions.DefaultPortraits;
            _chkInvertedTree.Checked = fOptions.TreeChartOptions.InvertedTree;
            _chkChildlessExclude.Checked = fOptions.TreeChartOptions.ChildlessExclude;
            _chkShowPlaces.Checked = fOptions.TreeChartOptions.ShowPlaces;
            _chkHideUnknownSpouses.Checked = fOptions.TreeChartOptions.HideUnknownSpouses;
            _chkCheckTreeSize.Checked = fOptions.CheckTreeSize;
            _chkDottedLinesOfAdoptedChildren.Checked = fOptions.TreeChartOptions.DottedLinesOfAdoptedChildren;
            _chkDottedLinesOfDivorcedSpouses.Checked = fOptions.TreeChartOptions.DottedLinesOfCommonLawSpouses;
            _chkSeparateDAPLines.Checked = fOptions.TreeChartOptions.SeparateDatesAndPlacesLines;
            _chkOnlyLocality.Checked = fOptions.TreeChartOptions.OnlyLocality;
            _chkBoldNames.Checked = fOptions.TreeChartOptions.BoldNames;
            _chkMinimizingWidth.Checked = fOptions.TreeChartOptions.MinimizingWidth;
            _chkShowAge.Checked = fOptions.TreeChartOptions.AgeVisible;
            _chkTreeSurnameFirst.Checked = fOptions.TreeChartOptions.SurnameFirstInOrder;
            _chkURNotesVisible.Checked = fOptions.TreeChartOptions.URNotesVisible;
            _chkShortenDateRanges.Checked = fOptions.TreeChartOptions.ShortenDates;
            _chkSameCardsWidth.Checked = fOptions.TreeChartOptions.SameCardsWidth;
            _chkFullNameOnOneLine.Checked = fOptions.TreeChartOptions.FullNameOnOneLine;
            _chkDateDesignations.Checked = fOptions.TreeChartOptions.DateDesignations;
            _chkMourningEdges.Checked = fOptions.TreeChartOptions.MourningEdges;
            _chkUseAdditionalDates.Checked = fOptions.TreeChartOptions.UseAdditionalDates;

            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                GetControl<ICheckBox>("chkMultipleSpouseLines").Checked = fOptions.TreeChartOptions.MultipleSpouseLines;
            }

            _lblMaleColor.BackColor = fOptions.TreeChartOptions.MaleColor;
            _lblFemaleColor.BackColor = fOptions.TreeChartOptions.FemaleColor;
            _lblUnkSexColor.BackColor = fOptions.TreeChartOptions.UnkSexColor;
            _lblUnHusbandColor.BackColor = fOptions.TreeChartOptions.UnHusbandColor;
            _lblUnWifeColor.BackColor = fOptions.TreeChartOptions.UnWifeColor;

            _numMargins.Value = fOptions.TreeChartOptions.Margins;
            _numBranchDist.Value = fOptions.TreeChartOptions.BranchDistance;
            _numGenDist.Value = fOptions.TreeChartOptions.LevelDistance;
            _numSpouseDist.Value = fOptions.TreeChartOptions.SpouseDistance;
            _numPadding.Value = fOptions.TreeChartOptions.Padding;

            _chkSeparateDepth.Checked = fOptions.TreeChartOptions.SeparateDepth;

            _numDefaultDepth.Value = fOptions.TreeChartOptions.DepthLimit;
            _numDefaultDepthAncestors.Value = fOptions.TreeChartOptions.DepthLimitAncestors;
            _numDefaultDepthDescendants.Value = fOptions.TreeChartOptions.DepthLimitDescendants;

            if (!fIsTerm) {
                var hasExtraControls = !AppHost.Instance.HasFeatureSupport(Feature.Mobile);
                _chkUseExtraControls.Checked = fOptions.TreeChartOptions.UseExtraControls && hasExtraControls;
                _chkUseExtraControls.Visible = hasExtraControls;

                UpdateTreeChartFont();

                var cmbTextEffect = _cmbTextEffect;
                cmbTextEffect.Clear();
                for (TextEffect itm = TextEffect.First; itm <= TextEffect.Last; itm++) {
                    cmbTextEffect.AddItem(LangMan.LS(GKData.TextEffects[(int)itm]), itm);
                }
                cmbTextEffect.SetSelectedTag(fOptions.TreeChartOptions.TextEffect);

#if NETCOREAPP
                cmbTextEffect.Enabled = false;
#else
                cmbTextEffect.Enabled = !AppHost.Instance.HasFeatureSupport(Feature.Mobile);
#endif
            }
        }

        public void UpdateTreeChartFont()
        {
            if (!fIsTerm) {
                var opts = fOptions.TreeChartOptions;
                _lblChartFont.Text = $"{opts.DefFontName}, {opts.DefFontSize}";
            }
        }

        public void UpdateUIFont()
        {
            var opts = fOptions;
            GetControl<ILabel>("lblUIFont").Text = $"{opts.DefFontName}, {opts.DefFontSize}";
        }

        public void ChangeSeparateDepth()
        {
            bool sepDepth = _chkSeparateDepth.Checked;

            _numDefaultDepth.Enabled = !sepDepth;
            _numDefaultDepthAncestors.Enabled = sepDepth;
            _numDefaultDepthDescendants.Enabled = sepDepth;
        }

        public void ChangeTreeChartOption()
        {
            _chkSeparateDAPLines.Enabled = _chkShowPlaces.Checked;
            _chkOnlyLocality.Enabled = _chkShowPlaces.Checked;

            _chkDefaultPortraits.Enabled = _chkPortraitsVisible.Checked;

            _chkDiffLines.Enabled = _chkName.Checked && _chkPatronymic.Checked;

            _chkOnlyYears.Enabled = _chkBirthDate.Checked && _chkDeathDate.Checked;

            bool fullNameOnOneLine = _chkFullNameOnOneLine.Checked;
            _chkSurname.Enabled = !fullNameOnOneLine;
            _chkName.Enabled = !fullNameOnOneLine;
            _chkPatronymic.Enabled = !fullNameOnOneLine;
            _chkDiffLines.Enabled = !fullNameOnOneLine;
        }

        public void AcceptTreeChartsOptions()
        {
            fOptions.TreeChartOptions.FamilyVisible = _chkSurname.Checked;
            fOptions.TreeChartOptions.NameVisible = _chkName.Checked;
            fOptions.TreeChartOptions.PatronymicVisible = _chkPatronymic.Checked;
            fOptions.TreeChartOptions.DiffLines = _chkDiffLines.Checked;
            fOptions.TreeChartOptions.BirthDateVisible = _chkBirthDate.Checked;
            fOptions.TreeChartOptions.DeathDateVisible = _chkDeathDate.Checked;
            fOptions.TreeChartOptions.OnlyYears = _chkOnlyYears.Checked;
            fOptions.TreeChartOptions.MarriagesDates = _chkMarriagesDates.Checked;
            fOptions.TreeChartOptions.Kinship = _chkKinship.Checked;
            fOptions.TreeChartOptions.SignsVisible = _chkSignsVisible.Checked;
            fOptions.TreeChartOptions.Decorative = _chkTreeDecorative.Checked;
            fOptions.TreeChartOptions.PortraitsVisible = _chkPortraitsVisible.Checked;
            fOptions.TreeChartOptions.DefaultPortraits = _chkDefaultPortraits.Checked;
            fOptions.TreeChartOptions.InvertedTree = _chkInvertedTree.Checked;
            fOptions.TreeChartOptions.ChildlessExclude = _chkChildlessExclude.Checked;
            fOptions.TreeChartOptions.ShowPlaces = _chkShowPlaces.Checked;
            fOptions.TreeChartOptions.HideUnknownSpouses = _chkHideUnknownSpouses.Checked;
            fOptions.CheckTreeSize = _chkCheckTreeSize.Checked;
            fOptions.TreeChartOptions.DottedLinesOfAdoptedChildren = _chkDottedLinesOfAdoptedChildren.Checked;
            fOptions.TreeChartOptions.DottedLinesOfCommonLawSpouses = _chkDottedLinesOfDivorcedSpouses.Checked;
            fOptions.TreeChartOptions.SeparateDatesAndPlacesLines = _chkSeparateDAPLines.Checked;
            fOptions.TreeChartOptions.OnlyLocality = _chkOnlyLocality.Checked;
            fOptions.TreeChartOptions.BoldNames = _chkBoldNames.Checked;
            fOptions.TreeChartOptions.MinimizingWidth = _chkMinimizingWidth.Checked;
            fOptions.TreeChartOptions.AgeVisible = _chkShowAge.Checked;
            fOptions.TreeChartOptions.SurnameFirstInOrder = _chkTreeSurnameFirst.Checked;
            fOptions.TreeChartOptions.URNotesVisible = _chkURNotesVisible.Checked;
            fOptions.TreeChartOptions.ShortenDates = _chkShortenDateRanges.Checked;
            fOptions.TreeChartOptions.SameCardsWidth = _chkSameCardsWidth.Checked;
            fOptions.TreeChartOptions.FullNameOnOneLine = _chkFullNameOnOneLine.Checked;
            fOptions.TreeChartOptions.DateDesignations = _chkDateDesignations.Checked;
            fOptions.TreeChartOptions.MourningEdges = _chkMourningEdges.Checked;
            fOptions.TreeChartOptions.UseAdditionalDates = _chkUseAdditionalDates.Checked;

            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                fOptions.TreeChartOptions.MultipleSpouseLines = GetControl<ICheckBox>("chkMultipleSpouseLines").Checked;
            }

            fOptions.TreeChartOptions.MaleColor = _lblMaleColor.BackColor;
            fOptions.TreeChartOptions.FemaleColor = _lblFemaleColor.BackColor;
            fOptions.TreeChartOptions.UnkSexColor = _lblUnkSexColor.BackColor;
            fOptions.TreeChartOptions.UnHusbandColor = _lblUnHusbandColor.BackColor;
            fOptions.TreeChartOptions.UnWifeColor = _lblUnWifeColor.BackColor;

            fOptions.TreeChartOptions.Margins = (int)_numMargins.Value;
            fOptions.TreeChartOptions.BranchDistance = (int)_numBranchDist.Value;
            fOptions.TreeChartOptions.LevelDistance = (int)_numGenDist.Value;
            fOptions.TreeChartOptions.SpouseDistance = (int)_numSpouseDist.Value;
            fOptions.TreeChartOptions.Padding = (int)_numPadding.Value;

            fOptions.TreeChartOptions.SeparateDepth = _chkSeparateDepth.Checked;
            fOptions.TreeChartOptions.DepthLimit = (int)_numDefaultDepth.Value;
            fOptions.TreeChartOptions.DepthLimitAncestors = (int)_numDefaultDepthAncestors.Value;
            fOptions.TreeChartOptions.DepthLimitDescendants = (int)_numDefaultDepthDescendants.Value;

            if (!fIsTerm) {
                fOptions.TreeChartOptions.UseExtraControls = _chkUseExtraControls.Checked;
                fOptions.TreeChartOptions.TextEffect = _cmbTextEffect.GetSelectedTag<TextEffect>();
            }
        }

        public void ResetCircleChartsOptions()
        {
            fOptions.CircleChartOptions.ResetDefaults();
            fView.UpdateCircleChartsOptions();
        }

        public override void UpdateView()
        {
            // common
            UpdateProxyOptions();
            UpdateBackupOptions();
            UpdateOtherOptions();

            // media
            UpdateMediaOptions();

            // charts
            UpdateTreeChartsOptions();
            fView.UpdateCircleChartsOptions();

            // interface
            UpdateInterfaceOptions();
            UpdateWomanSurnameFormat();

            fOptions.ListOptions[GDMRecordType.rtIndividual].Columns.CopyTo(fTempColumns);
            UpdateColumnsList();

            // reports
            UpdatePedigreesOptions();
            UpdateFamilyBookOptions();

            // specials
            UpdateSpecials();

            // event types
            UpdateEventTypes();

            // plugins
            UpdatePlugins();
        }

        public override bool Accept()
        {
            // common
            AcceptProxyOptions();
            AcceptBackupOptions();
            AcceptOtherOptions();

            // media
            AcceptMediaOptions();

            // charts
            AcceptTreeChartsOptions();
            fView.AcceptCircleChartsOptions();

            // interface
            AcceptInterfaceOptions();
            AcceptWomanSurnameFormat();
            AcceptColumnsList();

            // reports
            AcceptPedigreesOptions();
            AcceptFamilyBookOptions();

            // specials
            AcceptSpecials();

            // plugins
            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                AcceptPlugins();
            }

            return true;
        }

        private T TryGetControl<T>(string controlName) where T : class, IControl
        {
            var ctlObj = fView.GetControl(controlName);
            return (ctlObj == null) ? null : GetControlHandler<T>(ctlObj);
        }

        public override void SetLocale()
        {
            // in order of execution - called before a similar assignment in the constructor
            fIsTerm = !AppHost.Instance.HasFeatureSupport(Feature.Graphics);

            fView.SetTitle(LangMan.LS(LSID.MIOptions));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);

            // Common
            var pageCommon = TryGetControl<ITabPage>("pageCommon");
            if (pageCommon != null) {
                pageCommon.Text = LangMan.LS(LSID.Common);
            }

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ITabPage>("pageComOther").Text = LangMan.LS(LSID.Common);
                GetControl<ITabPage>("pageComBackup").Text = LangMan.LS(LSID.FileBackup);

                GetControl<ICheckBox>("chkExtBackupEnabled").Text = LangMan.LS(LSID.ExtendedBackup);
                GetControl<ILabel>("lblExtBackupFolder").Text = LangMan.LS(LSID.Folder);
                GetControl<IButton>("btnExtBackupFolderChoose").Text = LangMan.LS(LSID.DlgSelect);
            }

            if (!fIsTerm) {
                _grpInternet.Text = LangMan.LS(LSID.Internet);
                _chkUseProxy.Text = LangMan.LS(LSID.ProxyUse);
                _lblProxyServer.Text = LangMan.LS(LSID.ProxyServer);
                _lblProxyPort.Text = LangMan.LS(LSID.ProxyPort);
                _lblProxyLogin.Text = LangMan.LS(LSID.ProxyLogin);
                _lblProxyPassword.Text = LangMan.LS(LSID.Password);
            }

            _grpFileBackup.Text = LangMan.LS(LSID.BackupOnSave);
            _radFBNone.Text = LangMan.LS(LSID.Not);
            _radFBOnlyPrev.Text = LangMan.LS(LSID.BackupOnlyPrev);
            _radFBEachRevision.Text = LangMan.LS(LSID.BackupEachRevision);

            _chkAutosave.Text = LangMan.LS(LSID.Autosave);
            _lblMinutes.Text = LangMan.LS(LSID.Minutes);
            _lblBackupRevisionsMaxCount.Text = LangMan.LS(LSID.BackupRevisionsMaxCount);

            _grpOther.Text = LangMan.LS(LSID.Other);

            _chkShowOnStart.Text = LangMan.LS(LSID.StartupTips);
            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                GetControl<ICheckBox>("chkShowEventsOnlyAlive").Text = LangMan.LS(LSID.ShowEventsOnlyAlive);
            }

            _chkLoadRecentFiles.Text = LangMan.LS(LSID.LoadRecentFiles);
            _chkAutoCheckUpdates.Text = LangMan.LS(LSID.AutoCheckUpdates);
            _chkCharsetDetection.Text = LangMan.LS(LSID.CharsetDetection);
            _chkDialogClosingWarn.Text = LangMan.LS(LSID.WarnForClosingDialog);

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ICheckBox>("chkDisplayFullFileName").Text = LangMan.LS(LSID.DisplayFullFileName);
            }

            _lblLanguage.Text = LangMan.LS(LSID.Language);
            _lblGeocoder.Text = LangMan.LS(LSID.Geocoder);
            _lblGeoSearchCountry.Text = LangMan.LS(LSID.GeoSearchCountryRestriction);

            _lblCertaintyAlgorithm.Text = LangMan.LS(LSID.CertaintyAlgorithm);
            var cmbCA = _cmbCertaintyAlgorithm;
            cmbCA.Clear();
            for (var ca = CertaintyAlgorithm.WeightedAverage; ca <= CertaintyAlgorithm.Maximum; ca++) {
                cmbCA.AddItem(LangMan.LS(GKData.CertaintyAlgorithms[(int)ca].Name), ca);
            }

            // Multimedia
            _pageMultimedia.Text = LangMan.LS(LSID.RPMultimedia);

            _chkRemovableMediaWarning.Text = LangMan.LS(LSID.RemovableMediaWarningOption);
            _chkEmbeddedMediaPlayer.Text = LangMan.LS(LSID.EmbeddedMediaPlayer);

            _chkAllowMediaDirectRefs.Text = LangMan.LS(LSID.AllowMediaDirectReferences);
            _chkAllowMediaStoreRelativeReferences.Text = LangMan.LS(LSID.AllowMediaRelativeReferences);

            _lblMediaStoreDefault.Text = LangMan.LS(LSID.MediaStoreDefault);

            var combo = _cmbMediaStoreDefault;
            combo.Clear();
            for (MediaStoreType mst = MediaStoreType.mstReference; mst <= MediaStoreType.mstURL; mst++) {
                var storeType = GKData.GKStoreTypes[(int)mst];
                combo.AddItem(LangMan.LS(storeType.Name), mst);
            }

            _chkAllowDeleteMediaFileFromStgArc.Text = LangMan.LS(LSID.AllowDeleteMediaFileFromStgArc);
            _chkAllowDeleteMediaFileFromRefs.Text = LangMan.LS(LSID.AllowDeleteMediaFileFromRefs);
            _chkDeleteMediaFileWithoutConfirm.Text = LangMan.LS(LSID.DeleteMediaFileWithoutConfirm);
            _chkHighlightInaccessibleFiles.Text = LangMan.LS(LSID.HighlightInaccessibleFiles);

            // Charts
            var pageCharts = TryGetControl<ITabPage>("pageCharts");
            if (pageCharts != null) {
                pageCharts.Text = LangMan.LS(LSID.Charts);
            }

            var pageTreeChart = TryGetControl<ITabPage>("pageTreeChart");
            if (pageTreeChart != null) {
                pageTreeChart.Text = LangMan.LS(LSID.Trees);
            }

            _pageTreePersons.Text = LangMan.LS(LSID.ViewTree);
            _pageTreeDesign.Text = LangMan.LS(LSID.Decor);

            _chkSurname.Text = LangMan.LS(LSID.Surname);
            _chkName.Text = LangMan.LS(LSID.GivenName);
            _chkPatronymic.Text = LangMan.LS(LSID.Patronymic);
            _chkDiffLines.Text = LangMan.LS(LSID.DiffLines);
            _chkBirthDate.Text = LangMan.LS(LSID.BirthDate);
            _chkDeathDate.Text = LangMan.LS(LSID.DeathDate);
            _chkOnlyYears.Text = LangMan.LS(LSID.OnlyYears);
            _chkMarriagesDates.Text = LangMan.LS(LSID.MarriagesDates);
            _chkKinship.Text = LangMan.LS(LSID.Kinship);
            _chkSignsVisible.Text = LangMan.LS(LSID.SignsVisible);
            _chkTreeDecorative.Text = LangMan.LS(LSID.TreeDecorative);
            _chkPortraitsVisible.Text = LangMan.LS(LSID.PortraitsVisible);
            _chkDefaultPortraits.Text = LangMan.LS(LSID.DefaultPortraits);
            _chkInvertedTree.Text = LangMan.LS(LSID.InvertedTree);
            _chkChildlessExclude.Text = LangMan.LS(LSID.ChildlessExclude);
            _chkShowPlaces.Text = LangMan.LS(LSID.ShowPlaces);
            _chkSeparateDAPLines.Text = LangMan.LS(LSID.SeparateDatesAndPlacesLines);
            _chkOnlyLocality.Text = LangMan.LS(LSID.OnlyLocality);
            _chkHideUnknownSpouses.Text = LangMan.LS(LSID.HideUnknownSpouses);
            _chkCheckTreeSize.Text = LangMan.LS(LSID.CheckTreeSize);
            _chkDottedLinesOfAdoptedChildren.Text = LangMan.LS(LSID.DottedLinesOfAdoptedChildren);
            _chkDottedLinesOfDivorcedSpouses.Text = LangMan.LS(LSID.DottedLinesOfCommonLawSpouses);
            _chkBoldNames.Text = LangMan.LS(LSID.BoldNames);
            _chkMinimizingWidth.Text = LangMan.LS(LSID.MinimizingWidth);
            _chkShowAge.Text = LangMan.LS(LSID.ShowAge);
            _chkTreeSurnameFirst.Text = LangMan.LS(LSID.SurnameFirstInOrder);
            _chkURNotesVisible.Text = LangMan.LS(LSID.ShowTreeNotes);
            _chkShortenDateRanges.Text = LangMan.LS(LSID.ShortenDates);
            _chkSameCardsWidth.Text = LangMan.LS(LSID.SameCardsWidth);
            _chkFullNameOnOneLine.Text = LangMan.LS(LSID.FullNameOnOneLine);
            _chkDateDesignations.Text = LangMan.LS(LSID.DateDesignations);
            _chkMourningEdges.Text = LangMan.LS(LSID.MourningEdges);
            _chkUseAdditionalDates.Text = LangMan.LS(LSID.UseAdditionalDates);

            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                GetControl<ICheckBox>("chkMultipleSpouseLines").Text = LangMan.LS(LSID.MultipleSpouseLines);
            }

            _lblMaleColor.Text = LangMan.LS(LSID.Man);
            _lblFemaleColor.Text = LangMan.LS(LSID.Woman);
            _lblUnkSexColor.Text = LangMan.LS(LSID.UnkSex);
            _lblUnHusbandColor.Text = LangMan.LS(LSID.UnHusband);
            _lblUnWifeColor.Text = LangMan.LS(LSID.UnWife);

            if (!fIsTerm && !AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ILabel>("lblChartFontTitle").Text = LangMan.LS(LSID.Font);
            }

            _grpSpacings.Text = LangMan.LS(LSID.Spacings);
            _lblMargins.Text = LangMan.LS(LSID.Margins);
            _lblBranchDist.Text = LangMan.LS(LSID.BranchDist);
            _lblGenDist.Text = LangMan.LS(LSID.GenDist);
            _lblSpouseDist.Text = LangMan.LS(LSID.SpouseDist);
            _lblPadding.Text = LangMan.LS(LSID.Padding);

            _chkSeparateDepth.Text = LangMan.LS(LSID.SeparateDepth);
            _lblDefaultDepth.Text = LangMan.LS(LSID.DefaultDepth);
            _lblDefaultDepthAncestors.Text = LangMan.LS(LSID.DefaultDepth) + ": " + LangMan.LS(LSID.Ancestors);
            _lblDefaultDepthDescendants.Text = LangMan.LS(LSID.DefaultDepth) + ": " + LangMan.LS(LSID.Descendants);

            if (!fIsTerm) {
                _chkUseExtraControls.Text = LangMan.LS(LSID.UseExtraControls);
                _lblTextEffect.Text = LangMan.LS(LSID.TextEffect);

                _pageAncCircle.Text = LangMan.LS(LSID.AncestorsCircle);
            }

            // UIView
            var pageUIView = TryGetControl<ITabPage>("pageUIView");
            if (pageUIView != null) {
                pageUIView.Text = LangMan.LS(LSID.Interface);
            }

            _pageViewCommon.Text = LangMan.LS(LSID.ListsAll);

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ITabPage>("pageNames").Text = LangMan.LS(LSID.Names);
            }

            _rgFNPFormat.Text = LangMan.LS(LSID.NamesFormat);
            _radSNP.Text = LangMan.LS(LSID.NF1);
            _radS_NP.Text = LangMan.LS(LSID.NF2);
            _radS_N_P.Text = LangMan.LS(LSID.NF3);

            _chkPlacesWithAddress.Text = LangMan.LS(LSID.PlacesWithAddress);
            _chkHighlightUnparented.Text = LangMan.LS(LSID.HighlightUnparented);
            _chkHighlightUnmarried.Text = LangMan.LS(LSID.HighlightUnmarried);

            _chkAutoSortChildren.Text = LangMan.LS(LSID.AutoSortChildren);
            _chkAutoSortSpouses.Text = LangMan.LS(LSID.AutoSortSpouses);
            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                GetControl<ICheckBox>("chkAutoSortEvents").Text = LangMan.LS(LSID.AutoSortEvents);
            }
            _chkFirstCapitalLetterInNames.Text = LangMan.LS(LSID.FirstCapitalLetterInNames);
            _chkShortKinshipForm.Text = LangMan.LS(LSID.ShortKinshipForm);
            _chkSurnameFirstInOrder.Text = LangMan.LS(LSID.SurnameFirstInOrder);
            _chkSurnameInCapitals.Text = LangMan.LS(LSID.SurnameInCapitals);

            if (!fIsTerm && !AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ILabel>("lblUIFontTitle").Text = LangMan.LS(LSID.Font);
                GetControl<ICheckBox>("chkOverrideThemesFont").Text = LangMan.LS(LSID.OverrideThemesFont);
            }

            _chkUseSurnamesInPSF.Text = LangMan.LS(LSID.UseSurnamesInPersonSelectionFilter);
            _chkUseBirthDatesInPSF.Text = LangMan.LS(LSID.UseBirthDatesInPersonSelectionFilter);

            _radDMY.Text = LangMan.LS(LSID.DF_DD_MM_YYYY);
            _radYMD.Text = LangMan.LS(LSID.DF_YYYY_MM_DD);
            _grpDateFormat.Text = LangMan.LS(LSID.DateFormat);
            _chkShowDatesCalendar.Text = LangMan.LS(LSID.ShowDatesCalendar);
            _chkShowDatesSigns.Text = LangMan.LS(LSID.ShowDatesSigns);
            _chkLocalizedCalendarSignatures.Text = LangMan.LS(LSID.LocalizedCalendarSignaturesOptLabel);

            _grpAdvancedNames.Text = LangMan.LS(LSID.AdditionalNames);
            _chkExtendWomanSurnames.Text = LangMan.LS(LSID.ExtendedWomanSurnames);
            _radMaiden_Married.Text = LangMan.LS(LSID.WSF_Maiden_Married);
            _radMarried_Maiden.Text = LangMan.LS(LSID.WSF_Married_Maiden);
            _radMaiden.Text = LangMan.LS(LSID.WSF_Maiden);
            _radMarried.Text = LangMan.LS(LSID.WSF_Married);
            _chkSimpleSingleSurnames.Text = LangMan.LS(LSID.SimpleSingleSurnames);

            _pageViewPersons.Text = LangMan.LS(LSID.ListPersons);
            _btnResetDefaults.Text = LangMan.LS(LSID.DefList);

            _lblMatchPatternMethod.Text = LangMan.LS(LSID.MatchPatternMethod);

            _pageNavigation.Text = LangMan.LS(LSID.Navigation);
            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                GetControl<ICheckBox>("chkSourceFilterChecksEvents").Text = LangMan.LS(LSID.SourceFilteringAlsoChecksEvents);
                GetControl<ICheckBox>("chkConvenientSourcesFormat").Text = LangMan.LS(LSID.ConvenientSourcesFormat);
            }

            _pageGeo.Text = LangMan.LS(LSID.LocationsAndMaps);

            // Reports
            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                GetControl<ITabPage>("pagePedigree").Text = LangMan.LS(LSID.Reports);

                GetControl<ITabPage>("pageFamilyBook").Text = LangMan.LS(LSID.FamilyBook);
                GetControl<ICheckBox>("chkFBIncludeEvents").Text = LangMan.LS(LSID.IncludeEvents);
                GetControl<ICheckBox>("chkFBIncludeFamilyEvents").Text = LangMan.LS(LSID.IncludeFamilyEvents);
                GetControl<ICheckBox>("chkFBIncludeNotes").Text = LangMan.LS(LSID.IncludeNotes);
                GetControl<ICheckBox>("chkFBMergeNotes").Text = LangMan.LS(LSID.MergeNotes);
            }

            // Pedigree
            _pagePedigree.Text = LangMan.LS(LSID.Pedigrees);

            _grpPedigree.Text = LangMan.LS(LSID.PedigreeGen);
            _chkAttributes.Text = LangMan.LS(LSID.IncludeAttributes);
            _chkNotes.Text = LangMan.LS(LSID.IncludeNotes);
            _chkSources.Text = LangMan.LS(LSID.IncludeSources);
            _chkSourcePages.Text = LangMan.LS(LSID.IncludeSourcePages);
            _chkSourceCitations.Text = LangMan.LS(LSID.IncludeSourceCitations);
            _chkGenerations.Text = LangMan.LS(LSID.IncludeGenerations);
            _chkPortraits.Text = LangMan.LS(LSID.IncludePortraits);

            if (AppHost.Instance.HasFeatureSupport(Feature.DesktopV3)) {
                GetControl<ICheckBox>("chkSortEvents").Text = LangMan.LS(LSID.SortEvents);
            }

            _grpPedigreeFormat.Text = LangMan.LS(LSID.PedigreeFormat);
            _radExcess.Text = LangMan.LS(LSID.PF1);
            _radCompact.Text = LangMan.LS(LSID.PF2);

            _lblAscendNumbering.Text = LangMan.LS(LSID.AscendNumbering);
            _lblDescendNumbering.Text = LangMan.LS(LSID.DescendNumbering);

            // Specials
            _pageSpecials.Text = LangMan.LS(LSID.Specials);
            _chkDisableNonStdFeatures.Text = LangMan.LS(LSID.DisableNonStdFeatures);
            _chkEnableStdValidation.Text = LangMan.LS(LSID.GEDCOMValidation);
            _chkUseInlineImagesInSvg.Text = LangMan.LS(LSID.UseInlineImagesInSvg);
            _chkUseExtendedNotes.Text = LangMan.LS(LSID.UseExtendedNotes);
            _chkKeepRichNames.Text = LangMan.LS(LSID.KeepRichNames);

            if (!fIsTerm) {
                _lblChartWindowsShowMode.Text = LangMan.LS(LSID.ChartWindowsShowMode);
                combo = _cmbChartWindowsShowMode;
                combo.Clear();
                for (ChartWindowsShowMode cwsm = ChartWindowsShowMode.Default; cwsm <= ChartWindowsShowMode.RightHalf; cwsm++) {
                    combo.AddItem(LangMan.LS(GKData.ChartWindowsShowModes[(int)cwsm]), cwsm);
                }
            }

            _chkExtendedTree.Text = LangMan.LS(LSID.ExtendedTree);
            _chkSAFByAllNames.Text = LangMan.LS(LSID.SearchAndFilterByAllNames);
            _chkKeepInfoPansOverallSize.Text = LangMan.LS(LSID.KeepInfoPansOverallSize);
            _chkFilesOverwriteWarn.Text = LangMan.LS(LSID.FilesOverwriteWarn);
            _chkExtendedKinships.Text = LangMan.LS(LSID.ExtendedKinships);
            _chkExtendedLocations.Text = LangMan.LS(LSID.ExtendedLocations);
            _chkELAbbreviatedNames.Text = LangMan.LS(LSID.EL_AbbreviatedNames);
            _chkReversePlacesOrder.Text = LangMan.LS(LSID.ReversePlacesOrder);
            _chkShowNumberOfSubstructures.Text = LangMan.LS(LSID.ShowNumberOfSubstructures);
            _chkSearchPlacesWithoutCoords.Text = LangMan.LS(LSID.SearchPlacesWithoutCoords);

            // event types
            _pageEventTypes.Text = LangMan.LS(LSID.EventTypes);

            // Plugins
            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ITabPage>("pagePlugins").Text = LangMan.LS(LSID.Plugins);
            }
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            _btnColumnUp.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_MoveUp, true);
            _btnColumnDown.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_MoveDown, true);
        }



        private sealed class PluginsListModel : SimpleListModel<IPlugin>
        {
            private PluginInfo pInfo;

            public PluginsListModel() :
                base(null, CreateListColumns())
            {
            }

            public static ListColumns CreateListColumns()
            {
                var result = new ListColumns(GKListType.ltNone);
                result.AddColumn("Title", 75);
                result.AddColumn("Version", 100);
                result.AddColumn("Copyright", 125);
                result.AddColumn("Description", 250);
                return result;
            }

            public override void Fetch(IPlugin aRec)
            {
                base.Fetch(aRec);
                pInfo = PluginInfo.GetPluginInfo(fFetchedRec);
            }

            protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
            {
                object result = null;
                switch (colType) {
                    case 0:
                        result = pInfo.Title;
                        break;
                    case 1:
                        result = pInfo.Version;
                        break;
                    case 2:
                        result = pInfo.Copyright;
                        break;
                    case 3:
                        result = pInfo.Description;
                        break;
                }
                return result;
            }
        }
    }
}
