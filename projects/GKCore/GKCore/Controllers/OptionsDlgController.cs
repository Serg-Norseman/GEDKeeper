/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System.Threading.Tasks;
using GDModel;
using GKCore.Charts;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Plugins;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    public class OptionsDlgController : DialogController<IOptionsDlg>
    {
        private readonly GlobalOptions fOptions;
        private readonly IListColumns fTempColumns;


        public GlobalOptions Options
        {
            get { return fOptions; }
        }

        public IListColumns TempColumns
        {
            get { return fTempColumns; }
        }


        public OptionsDlgController(IOptionsDlg view) : base(view)
        {
            fOptions = GlobalOptions.Instance;
            fTempColumns = IndividualListModel.CreateListColumns();

            FillGeoSearchCountries();

            fView.EventTypesList.ListModel = new EventDefsListModel(fView, null, null);
        }

        public void ChangeTab()
        {
            AcceptLanguage();
            SetLocale();
            UpdateView();
        }

        private void FillGeoSearchCountries()
        {
            var combo = GetControl<IComboBox>("cmbGeoSearchCountry");

            combo.Clear();
            foreach (var ci in GKUtils.GetCountries()) {
                combo.Add(ci);
            }
        }

        public void UpdateColumnsList()
        {
            var listView = GetControl<IListView>("lstPersonColumns");

            listView.BeginUpdate();
            try {
                listView.ClearItems();

                int num = fTempColumns.Count;
                for (int i = 0; i < num; i++) {
                    ListColumn column = fTempColumns.OrderedColumns[i];
                    listView.AddItem(null, column.CurActive, column.ColName);
                }
            } finally {
                listView.EndUpdate();
            }
        }

        public void ResetColumnsList()
        {
            fTempColumns.ResetDefaults();
            UpdateColumnsList();
        }

        public void MoveColumnUp()
        {
            var listView = GetControl<IListView>("lstPersonColumns");

            int idx = listView.SelectedIndex;
            if (fTempColumns.MoveColumn(idx, true)) {
                UpdateColumnsList();
                listView.SelectedIndex = idx - 1;
            }
        }

        public void MoveColumnDown()
        {
            var listView = GetControl<IListView>("lstPersonColumns");

            int idx = listView.SelectedIndex;
            if (fTempColumns.MoveColumn(idx, false)) {
                UpdateColumnsList();
                listView.SelectedIndex = idx + 1;
            }
        }

        public void AcceptColumnsList()
        {
            var listView = GetControl<IListView>("lstPersonColumns");
            for (int i = 0; i < fTempColumns.Count; i++) {
                fTempColumns.OrderedColumns[i].CurActive = listView.Items[i].Checked;
            }

            fTempColumns.CopyTo(fOptions.ListOptions[GDMRecordType.rtIndividual].Columns);
        }

        public void UpdateProxyOptions()
        {
            var isMobile = AppHost.Instance.HasFeatureSupport(Feature.Mobile);
            GetControl<IGroupBox>("grpInternet").Visible = !isMobile;

            var hasInternetProxy = AppHost.Instance.HasFeatureSupport(Feature.InternetProxy);
            GetControl<IGroupBox>("grpInternet").Enabled = hasInternetProxy;

            GetControl<ICheckBox>("chkUseProxy").Checked = fOptions.Proxy.UseProxy && hasInternetProxy;
            GetControl<ITextBox>("txtProxyServer").Text = fOptions.Proxy.Server;
            GetControl<ITextBox>("txtProxyPort").Text = fOptions.Proxy.Port;
            GetControl<ITextBox>("txtProxyLogin").Text = fOptions.Proxy.Login;
            GetControl<ITextBox>("txtProxyPass").Text = fOptions.Proxy.Password;
        }

        public void AcceptProxyOptions()
        {
            fOptions.Proxy.UseProxy = GetControl<ICheckBox>("chkUseProxy").Checked;
            fOptions.Proxy.Server = GetControl<ITextBox>("txtProxyServer").Text;
            fOptions.Proxy.Port = GetControl<ITextBox>("txtProxyPort").Text;
            fOptions.Proxy.Login = GetControl<ITextBox>("txtProxyLogin").Text;
            fOptions.Proxy.Password = GetControl<ITextBox>("txtProxyPass").Text;
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
            GetControl<ICheckBox>("chkShowOnStart").Checked = fOptions.ShowTips;

            var hasRecentFilesLoad = AppHost.Instance.HasFeatureSupport(Feature.RecentFilesLoad);
            GetControl<ICheckBox>("chkLoadRecentFiles").Checked = fOptions.LoadRecentFiles && hasRecentFilesLoad;
            GetControl<ICheckBox>("chkLoadRecentFiles").Enabled = hasRecentFilesLoad;

            GetControl<ICheckBox>("chkAutoCheckUpdates").Checked = fOptions.AutoCheckUpdates;
            GetControl<ICheckBox>("chkCharsetDetection").Checked = fOptions.CharsetDetection;
            GetControl<ICheckBox>("chkDialogClosingWarn").Checked = fOptions.DialogClosingWarn;

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ICheckBox>("chkDisplayFullFileName").Checked = fOptions.DisplayFullFileName;
            }

            GetControl<IComboBox>("cmbGeocoder").Text = fOptions.Geocoder;
            GetControl<IComboBox>("cmbGeoSearchCountry").Text = fOptions.GeoSearchCountry;

            var combo = GetControl<IComboBox>("cmbLanguages");
            combo.Clear();
            combo.AddItem(LangMan.LS_DEF_NAME, LangMan.LS_DEF_CODE);
            foreach (LangRecord lngRec in GlobalOptions.Instance.Languages) {
                if (lngRec.Code != LangMan.LS_DEF_CODE) {
                    combo.AddItem(lngRec.Name, lngRec.Code);
                }
            }
            combo.SetSelectedTag(fOptions.InterfaceLang, true);

            GetControl<IComboBox>("cmbCertaintyAlgorithm").SetSelectedTag(fOptions.CertaintyAlgorithm);
        }

        public async Task AcceptLanguage()
        {
            var item = GetControl<IComboBox>("cmbLanguages").SelectedItem as ComboItem<int>;
            if (item != null) {
                await AppHost.Instance.LoadLanguage(item.Tag, false);
            }
        }

        public async void AcceptOtherOptions()
        {
            fOptions.ShowTips = GetControl<ICheckBox>("chkShowOnStart").Checked;
            fOptions.LoadRecentFiles = GetControl<ICheckBox>("chkLoadRecentFiles").Checked;
            fOptions.AutoCheckUpdates = GetControl<ICheckBox>("chkAutoCheckUpdates").Checked;
            fOptions.CharsetDetection = GetControl<ICheckBox>("chkCharsetDetection").Checked;
            fOptions.DialogClosingWarn = GetControl<ICheckBox>("chkDialogClosingWarn").Checked;

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                fOptions.DisplayFullFileName = GetControl<ICheckBox>("chkDisplayFullFileName").Checked;
            }

            fOptions.Geocoder = GetControl<IComboBox>("cmbGeocoder").Text;
            fOptions.GeoSearchCountry = GetControl<IComboBox>("cmbGeoSearchCountry").Text;

            await AcceptLanguage();

            fOptions.CertaintyAlgorithm = GetControl<IComboBox>("cmbCertaintyAlgorithm").GetSelectedTag<CertaintyAlgorithm>();
        }

        public void ResetPedigreesOptions()
        {
            fOptions.PedigreeOptions.ResetDefaults();
            UpdatePedigreesOptions();
        }

        private void PrepareNumbCombo(string comboName, PedigreeType pedigreeType, PedigreeNumbering curValue)
        {
            var combo = GetControl<IComboBox>(comboName);
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
            GetControl<ICheckBox>("chkAttributes").Checked = fOptions.PedigreeOptions.IncludeAttributes;
            GetControl<ICheckBox>("chkNotes").Checked = fOptions.PedigreeOptions.IncludeNotes;
            GetControl<ICheckBox>("chkSources").Checked = fOptions.PedigreeOptions.IncludeSources;
            GetControl<ICheckBox>("chkSourcePages").Checked = fOptions.PedigreeOptions.IncludeSourcePages;
            GetControl<ICheckBox>("chkGenerations").Checked = fOptions.PedigreeOptions.IncludeGenerations;
            GetControl<ICheckBox>("chkPortraits").Checked = fOptions.PedigreeOptions.IncludePortraits;

            switch (fOptions.PedigreeOptions.Format) {
                case PedigreeFormat.Excess:
                    GetControl<IRadioButton>("radExcess").Checked = true;
                    break;
                case PedigreeFormat.Compact:
                    GetControl<IRadioButton>("radCompact").Checked = true;
                    break;
            }

            PrepareNumbCombo("cmbAscendNumbering", PedigreeType.Ascend, fOptions.PedigreeOptions.AscendNumbering);
            PrepareNumbCombo("cmbDescendNumbering", PedigreeType.Descend, fOptions.PedigreeOptions.DescendNumbering);
        }

        public void AcceptPedigreesOptions()
        {
            fOptions.PedigreeOptions.IncludeAttributes = GetControl<ICheckBox>("chkAttributes").Checked;
            fOptions.PedigreeOptions.IncludeNotes = GetControl<ICheckBox>("chkNotes").Checked;
            fOptions.PedigreeOptions.IncludeSources = GetControl<ICheckBox>("chkSources").Checked;
            fOptions.PedigreeOptions.IncludeSourcePages = GetControl<ICheckBox>("chkSourcePages").Checked;
            fOptions.PedigreeOptions.IncludeGenerations = GetControl<ICheckBox>("chkGenerations").Checked;
            fOptions.PedigreeOptions.IncludePortraits = GetControl<ICheckBox>("chkPortraits").Checked;

            if (GetControl<IRadioButton>("radExcess").Checked) {
                fOptions.PedigreeOptions.Format = PedigreeFormat.Excess;
            } else if (GetControl<IRadioButton>("radCompact").Checked) {
                fOptions.PedigreeOptions.Format = PedigreeFormat.Compact;
            }

            fOptions.PedigreeOptions.AscendNumbering = GetControl<IComboBox>("cmbAscendNumbering").GetSelectedTag<PedigreeNumbering>();
            fOptions.PedigreeOptions.DescendNumbering = GetControl<IComboBox>("cmbDescendNumbering").GetSelectedTag<PedigreeNumbering>();
        }

        public void UpdateWomanSurnameFormat()
        {
            WomanSurnameFormat wsFmt = fOptions.WomanSurnameFormat;
            bool isExtend = wsFmt != WomanSurnameFormat.wsfNotExtend;

            GetControl<ICheckBox>("chkExtendWomanSurnames").Checked = isExtend;
            GetControl<IRadioButton>("radMaiden_Married").Enabled = isExtend;
            GetControl<IRadioButton>("radMarried_Maiden").Enabled = isExtend;
            GetControl<IRadioButton>("radMaiden").Enabled = isExtend;
            GetControl<IRadioButton>("radMarried").Enabled = isExtend;

            switch (wsFmt) {
                case WomanSurnameFormat.wsfMaiden_Married:
                    GetControl<IRadioButton>("radMaiden_Married").Checked = true;
                    break;

                case WomanSurnameFormat.wsfMarried_Maiden:
                    GetControl<IRadioButton>("radMarried_Maiden").Checked = true;
                    break;

                case WomanSurnameFormat.wsfMaiden:
                    GetControl<IRadioButton>("radMaiden").Checked = true;
                    break;

                case WomanSurnameFormat.wsfMarried:
                    GetControl<IRadioButton>("radMarried").Checked = true;
                    break;
            }

            GetControl<ICheckBox>("chkSimpleSingleSurnames").Checked = isExtend && fOptions.SimpleSingleSurnames;
            GetControl<ICheckBox>("chkSimpleSingleSurnames").Enabled = isExtend;
        }

        public void ChangeExtendWomanSurnames()
        {
            if (!GetControl<ICheckBox>("chkExtendWomanSurnames").Checked) {
                fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfNotExtend;
                UpdateWomanSurnameFormat();
            } else {
                GetControl<IRadioButton>("radMaiden_Married").Enabled = true;
                GetControl<IRadioButton>("radMarried_Maiden").Enabled = true;
                GetControl<IRadioButton>("radMaiden").Enabled = true;
                GetControl<IRadioButton>("radMarried").Enabled = true;
                GetControl<ICheckBox>("chkSimpleSingleSurnames").Enabled = true;
            }
        }

        public void AcceptWomanSurnameFormat()
        {
            if (!GetControl<ICheckBox>("chkExtendWomanSurnames").Checked) {
                fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfNotExtend;
            } else {
                if (GetControl<IRadioButton>("radMaiden_Married").Checked) {
                    fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfMaiden_Married;
                } else if (GetControl<IRadioButton>("radMarried_Maiden").Checked) {
                    fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfMarried_Maiden;
                } else if (GetControl<IRadioButton>("radMaiden").Checked) {
                    fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfMaiden;
                } else if (GetControl<IRadioButton>("radMarried").Checked) {
                    fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfMarried;
                }
            }
            fOptions.SimpleSingleSurnames = GetControl<ICheckBox>("chkSimpleSingleSurnames").Checked;
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
                    GetControl<IRadioButton>("radSNP").Checked = true;
                    break;
                case NameFormat.nfF_NP:
                    GetControl<IRadioButton>("radS_NP").Checked = true;
                    break;
                case NameFormat.nfF_N_P:
                    GetControl<IRadioButton>("radS_N_P").Checked = true;
                    break;
            }

            switch (fOptions.DefDateFormat) {
                case DateFormat.dfDD_MM_YYYY:
                    GetControl<IRadioButton>("radDMY").Checked = true;
                    break;
                case DateFormat.dfYYYY_MM_DD:
                    GetControl<IRadioButton>("radYMD").Checked = true;
                    break;
            }

            GetControl<ICheckBox>("chkShowDatesCalendar").Checked = fOptions.ShowDatesCalendar;
            GetControl<ICheckBox>("chkShowDatesSigns").Checked = fOptions.ShowDatesSign;
            GetControl<ICheckBox>("chkLocalizedCalendarSignatures").Checked = fOptions.LocalizedCalendarSignatures;

            GetControl<ICheckBox>("chkPlacesWithAddress").Checked = fOptions.PlacesWithAddress;

            var hasGridCellFormat = AppHost.Instance.HasFeatureSupport(Feature.GridCellFormat);
            GetControl<ICheckBox>("chkHighlightUnparented").Checked = fOptions.ListHighlightUnparentedPersons && hasGridCellFormat;
            GetControl<ICheckBox>("chkHighlightUnparented").Enabled = hasGridCellFormat;
            GetControl<ICheckBox>("chkHighlightUnmarried").Checked = fOptions.ListHighlightUnmarriedPersons && hasGridCellFormat;
            GetControl<ICheckBox>("chkHighlightUnmarried").Enabled = hasGridCellFormat;

            GetControl<ICheckBox>("chkAutoSortChildren").Checked = fOptions.AutoSortChildren;
            GetControl<ICheckBox>("chkAutoSortSpouses").Checked = fOptions.AutoSortSpouses;
            GetControl<ICheckBox>("chkFirstCapitalLetterInNames").Checked = fOptions.FirstCapitalLetterInNames;

            GetControl<ICheckBox>("chkShortKinshipForm").Checked = fOptions.ShortKinshipForm;
            GetControl<ICheckBox>("chkSurnameFirstInOrder").Checked = fOptions.SurnameFirstInOrder;
            GetControl<ICheckBox>("chkSurnameInCapitals").Checked = fOptions.SurnameInCapitals;

            GetControl<ICheckBox>("chkUseSurnamesInPSF").Checked = fOptions.UseSurnamesInPersonSelectionFilter;
            GetControl<ICheckBox>("chkUseBirthDatesInPSF").Checked = fOptions.UseBirthDatesInPersonSelectionFilter;

            var combo = GetControl<IComboBox>("cmbMatchPatternMethod");
            combo.Clear();
            for (MatchPatternMethod itm = MatchPatternMethod.RegEx; itm <= MatchPatternMethod.Fast; itm++) {
                combo.AddItem(itm.ToString(), itm);
            }
            combo.SetSelectedTag(fOptions.MatchPatternMethod);
        }

        public NameFormat GetSelectedNameFormat()
        {
            NameFormat result;
            if (GetControl<IRadioButton>("radSNP").Checked) {
                result = NameFormat.nfFNP;
            } else if (GetControl<IRadioButton>("radS_NP").Checked) {
                result = NameFormat.nfF_NP;
            } else if (GetControl<IRadioButton>("radS_N_P").Checked) {
                result = NameFormat.nfF_N_P;
            } else {
                result = NameFormat.nfFNP;
            }
            return result;
        }

        public void ChangeFNPFormat()
        {
            var defNameFormat = GetSelectedNameFormat();
            GetControl<ICheckBox>("chkSurnameFirstInOrder").Enabled = (defNameFormat == NameFormat.nfFNP);
        }

        public void AcceptInterfaceOptions()
        {
            fOptions.DefNameFormat = GetSelectedNameFormat();

            if (GetControl<IRadioButton>("radDMY").Checked) {
                fOptions.DefDateFormat = DateFormat.dfDD_MM_YYYY;
            } else if (GetControl<IRadioButton>("radYMD").Checked) {
                fOptions.DefDateFormat = DateFormat.dfYYYY_MM_DD;
            }
            fOptions.ShowDatesCalendar = GetControl<ICheckBox>("chkShowDatesCalendar").Checked;
            fOptions.ShowDatesSign = GetControl<ICheckBox>("chkShowDatesSigns").Checked;
            fOptions.LocalizedCalendarSignatures = GetControl<ICheckBox>("chkLocalizedCalendarSignatures").Checked;

            fOptions.PlacesWithAddress = GetControl<ICheckBox>("chkPlacesWithAddress").Checked;
            fOptions.ListHighlightUnparentedPersons = GetControl<ICheckBox>("chkHighlightUnparented").Checked;
            fOptions.ListHighlightUnmarriedPersons = GetControl<ICheckBox>("chkHighlightUnmarried").Checked;

            fOptions.AutoSortChildren = GetControl<ICheckBox>("chkAutoSortChildren").Checked;
            fOptions.AutoSortSpouses = GetControl<ICheckBox>("chkAutoSortSpouses").Checked;
            fOptions.FirstCapitalLetterInNames = GetControl<ICheckBox>("chkFirstCapitalLetterInNames").Checked;

            fOptions.ShortKinshipForm = GetControl<ICheckBox>("chkShortKinshipForm").Checked;
            fOptions.SurnameFirstInOrder = GetControl<ICheckBox>("chkSurnameFirstInOrder").Checked;
            fOptions.SurnameInCapitals = GetControl<ICheckBox>("chkSurnameInCapitals").Checked;

            fOptions.UseSurnamesInPersonSelectionFilter = GetControl<ICheckBox>("chkUseSurnamesInPSF").Checked;
            fOptions.UseBirthDatesInPersonSelectionFilter = GetControl<ICheckBox>("chkUseBirthDatesInPSF").Checked;

            fOptions.MatchPatternMethod = GetControl<IComboBox>("cmbMatchPatternMethod").GetSelectedTag<MatchPatternMethod>();
        }

        public void ResetSpecialsOptions()
        {
            fOptions.ResetDefaults_Specials();
            UpdateSpecials();
        }

        public void UpdateSpecials()
        {
            GetControl<ICheckBox>("chkUseInlineImagesInSvg").Checked = fOptions.TreeChartOptions.UseInlineImagesInSvg;
            GetControl<ICheckBox>("chkExtendedTree").Checked = fOptions.TreeChartOptions.ExtendedTree;
            GetControl<ICheckBox>("chkExtendedKinships").Checked = fOptions.ExtendedKinships;
            GetControl<ICheckBox>("chkSAFByAllNames").Checked = fOptions.SearchAndFilterByAllNames;

            var isMobile = AppHost.Instance.HasFeatureSupport(Feature.Mobile);
            GetControl<ICheckBox>("chkUseExtendedNotes").Checked = fOptions.UseExtendedNotes && !isMobile;
            GetControl<ICheckBox>("chkUseExtendedNotes").Visible = !isMobile;
            GetControl<ICheckBox>("chkKeepRichNames").Checked = fOptions.KeepRichNames && !isMobile;
            GetControl<ICheckBox>("chkKeepRichNames").Visible = !isMobile;

            var chartWinMode = !isMobile ? fOptions.ChartWindowsShowMode : ChartWindowsShowMode.Default;
            GetControl<IComboBox>("cmbChartWindowsShowMode").SetSelectedTag(chartWinMode);
            GetControl<IComboBox>("cmbChartWindowsShowMode").Visible = !isMobile;
            GetControl<ILabel>("lblChartWindowsShowMode").Visible = !isMobile;

            GetControl<ICheckBox>("chkKeepInfoPansOverallSize").Checked = fOptions.KeepInfoPansOverallSize && !isMobile;
            GetControl<ICheckBox>("chkKeepInfoPansOverallSize").Visible = !isMobile;

            var hasOverwritePrompt = AppHost.Instance.HasFeatureSupport(Feature.OverwritePrompt);
            GetControl<ICheckBox>("chkFilesOverwriteWarn").Checked = fOptions.FilesOverwriteWarn && hasOverwritePrompt;
            GetControl<ICheckBox>("chkFilesOverwriteWarn").Enabled = hasOverwritePrompt;

            GetControl<ICheckBox>("chkExtendedLocations").Checked = fOptions.ExtendedLocations;
            GetControl<ICheckBox>("chkELAbbreviatedNames").Checked = fOptions.EL_AbbreviatedNames;

            GetControl<ICheckBox>("chkReversePlacesOrder").Checked = fOptions.ReversePlaceEntitiesOrder;
            GetControl<ICheckBox>("chkShowNumberOfSubstructures").Checked = fOptions.ShowNumberOfSubstructures;
            GetControl<ICheckBox>("chkSearchPlacesWithoutCoords").Checked = fOptions.SearchPlacesWithoutCoords;
        }

        public void AcceptSpecials()
        {
            fOptions.TreeChartOptions.UseInlineImagesInSvg = GetControl<ICheckBox>("chkUseInlineImagesInSvg").Checked;
            fOptions.TreeChartOptions.ExtendedTree = GetControl<ICheckBox>("chkExtendedTree").Checked;
            fOptions.ExtendedKinships = GetControl<ICheckBox>("chkExtendedKinships").Checked;
            fOptions.SearchAndFilterByAllNames = GetControl<ICheckBox>("chkSAFByAllNames").Checked;

            fOptions.UseExtendedNotes = GetControl<ICheckBox>("chkUseExtendedNotes").Checked;
            fOptions.KeepRichNames = GetControl<ICheckBox>("chkKeepRichNames").Checked;

            fOptions.ChartWindowsShowMode = GetControl<IComboBox>("cmbChartWindowsShowMode").GetSelectedTag<ChartWindowsShowMode>();

            fOptions.KeepInfoPansOverallSize = GetControl<ICheckBox>("chkKeepInfoPansOverallSize").Checked;

            fOptions.FilesOverwriteWarn = GetControl<ICheckBox>("chkFilesOverwriteWarn").Checked;

            fOptions.ExtendedLocations = GetControl<ICheckBox>("chkExtendedLocations").Checked;
            fOptions.EL_AbbreviatedNames = GetControl<ICheckBox>("chkELAbbreviatedNames").Checked;

            fOptions.ReversePlaceEntitiesOrder = GetControl<ICheckBox>("chkReversePlacesOrder").Checked;
            fOptions.ShowNumberOfSubstructures = GetControl<ICheckBox>("chkShowNumberOfSubstructures").Checked;
            fOptions.SearchPlacesWithoutCoords = GetControl<ICheckBox>("chkSearchPlacesWithoutCoords").Checked;
        }

        private void UpdateEventTypes()
        {
            fView.EventTypesList.ListModel.DataOwner = AppHost.EventDefinitions;
        }

        public void UpdatePlugins()
        {
            var listView = GetControl<IListView>("lvPlugins");

            listView.ClearItems();
            int num = AppHost.Plugins.Count;
            for (int i = 0; i < num; i++) {
                IPlugin plugin = AppHost.Plugins[i];
                PluginInfo pInfo = PluginInfo.GetPluginInfo(plugin);

                listView.AddItem(null, pInfo.Title, pInfo.Version, pInfo.Copyright, pInfo.Description);
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
            GetControl<ICheckBox>("chkRemovableMediaWarning").Checked = fOptions.RemovableMediaWarning;

            var hasMediaPlayer = AppHost.Instance.HasFeatureSupport(Feature.MediaPlayer);
            GetControl<ICheckBox>("chkEmbeddedMediaPlayer").Checked = fOptions.EmbeddedMediaPlayer && hasMediaPlayer;
            GetControl<ICheckBox>("chkEmbeddedMediaPlayer").Enabled = hasMediaPlayer;

            GetControl<ICheckBox>("chkAllowMediaDirectRefs").Checked = fOptions.AllowMediaStoreReferences;
            GetControl<ICheckBox>("chkAllowMediaStoreRelativeReferences").Checked = fOptions.AllowMediaStoreRelativeReferences;
            GetControl<IComboBox>("cmbMediaStoreDefault").SetSelectedTag<MediaStoreType>(fOptions.MediaStoreDefault);
            GetControl<ICheckBox>("chkAllowDeleteMediaFileFromStgArc").Checked = fOptions.AllowDeleteMediaFileFromStgArc;
            GetControl<ICheckBox>("chkAllowDeleteMediaFileFromRefs").Checked = fOptions.AllowDeleteMediaFileFromRefs;
            GetControl<ICheckBox>("chkDeleteMediaFileWithoutConfirm").Checked = fOptions.DeleteMediaFileWithoutConfirm;

            var hasGridCellFormat = AppHost.Instance.HasFeatureSupport(Feature.GridCellFormat);
            GetControl<ICheckBox>("chkHighlightInaccessibleFiles").Checked = fOptions.HighlightInaccessibleFiles && hasGridCellFormat;
            GetControl<ICheckBox>("chkHighlightInaccessibleFiles").Enabled = hasGridCellFormat;
        }

        public void AcceptMediaOptions()
        {
            fOptions.RemovableMediaWarning = GetControl<ICheckBox>("chkRemovableMediaWarning").Checked;
            fOptions.EmbeddedMediaPlayer = GetControl<ICheckBox>("chkEmbeddedMediaPlayer").Checked;
            fOptions.AllowMediaStoreReferences = GetControl<ICheckBox>("chkAllowMediaDirectRefs").Checked;
            fOptions.AllowMediaStoreRelativeReferences = GetControl<ICheckBox>("chkAllowMediaStoreRelativeReferences").Checked;
            fOptions.MediaStoreDefault = GetControl<IComboBox>("cmbMediaStoreDefault").GetSelectedTag<MediaStoreType>();
            fOptions.AllowDeleteMediaFileFromStgArc = GetControl<ICheckBox>("chkAllowDeleteMediaFileFromStgArc").Checked;
            fOptions.AllowDeleteMediaFileFromRefs = GetControl<ICheckBox>("chkAllowDeleteMediaFileFromRefs").Checked;
            fOptions.DeleteMediaFileWithoutConfirm = GetControl<ICheckBox>("chkDeleteMediaFileWithoutConfirm").Checked;
            fOptions.HighlightInaccessibleFiles = GetControl<ICheckBox>("chkHighlightInaccessibleFiles").Checked;
        }

        public void UpdateBackupOptions()
        {
            switch (fOptions.FileBackup) {
                case FileBackup.fbNone:
                    GetControl<IRadioButton>("radFBNone").Checked = true;
                    break;
                case FileBackup.fbOnlyPrev:
                    GetControl<IRadioButton>("radFBOnlyPrev").Checked = true;
                    break;
                case FileBackup.fbEachRevision:
                    GetControl<IRadioButton>("radFBEachRevision").Checked = true;
                    break;
            }

            GetControl<ICheckBox>("chkAutosave").Checked = fOptions.Autosave;
            GetControl<INumericBox>("numASMin").Value = fOptions.AutosaveInterval;
            GetControl<INumericBox>("numBackupRevisionsMaxCount").Value = fOptions.FileBackupEachRevisionMaxCount;
        }

        public void AcceptBackupOptions()
        {
            if (GetControl<IRadioButton>("radFBNone").Checked) {
                fOptions.FileBackup = FileBackup.fbNone;
            } else if (GetControl<IRadioButton>("radFBOnlyPrev").Checked) {
                fOptions.FileBackup = FileBackup.fbOnlyPrev;
            } else if (GetControl<IRadioButton>("radFBEachRevision").Checked) {
                fOptions.FileBackup = FileBackup.fbEachRevision;
            }

            fOptions.Autosave = GetControl<ICheckBox>("chkAutosave").Checked;
            fOptions.AutosaveInterval = (int)GetControl<INumericBox>("numASMin").Value;
            fOptions.FileBackupEachRevisionMaxCount = (int)GetControl<INumericBox>("numBackupRevisionsMaxCount").Value;
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
            GetControl<ICheckBox>("chkSurname").Checked = fOptions.TreeChartOptions.FamilyVisible;
            GetControl<ICheckBox>("chkName").Checked = fOptions.TreeChartOptions.NameVisible;
            GetControl<ICheckBox>("chkPatronymic").Checked = fOptions.TreeChartOptions.PatronymicVisible;
            GetControl<ICheckBox>("chkDiffLines").Checked = fOptions.TreeChartOptions.DiffLines;
            GetControl<ICheckBox>("chkBirthDate").Checked = fOptions.TreeChartOptions.BirthDateVisible;
            GetControl<ICheckBox>("chkDeathDate").Checked = fOptions.TreeChartOptions.DeathDateVisible;
            GetControl<ICheckBox>("chkOnlyYears").Checked = fOptions.TreeChartOptions.OnlyYears;
            GetControl<ICheckBox>("chkMarriagesDates").Checked = fOptions.TreeChartOptions.MarriagesDates;
            GetControl<ICheckBox>("chkKinship").Checked = fOptions.TreeChartOptions.Kinship;
            GetControl<ICheckBox>("chkSignsVisible").Checked = fOptions.TreeChartOptions.SignsVisible;
            GetControl<ICheckBox>("chkTreeDecorative").Checked = fOptions.TreeChartOptions.Decorative;
            GetControl<ICheckBox>("chkPortraitsVisible").Checked = fOptions.TreeChartOptions.PortraitsVisible;
            GetControl<ICheckBox>("chkDefaultPortraits").Checked = fOptions.TreeChartOptions.DefaultPortraits;
            GetControl<ICheckBox>("chkInvertedTree").Checked = fOptions.TreeChartOptions.InvertedTree;
            GetControl<ICheckBox>("chkChildlessExclude").Checked = fOptions.TreeChartOptions.ChildlessExclude;
            GetControl<ICheckBox>("chkShowPlaces").Checked = fOptions.TreeChartOptions.ShowPlaces;
            GetControl<ICheckBox>("chkHideUnknownSpouses").Checked = fOptions.TreeChartOptions.HideUnknownSpouses;
            GetControl<ICheckBox>("chkCheckTreeSize").Checked = fOptions.CheckTreeSize;
            GetControl<ICheckBox>("chkDottedLinesOfAdoptedChildren").Checked = fOptions.TreeChartOptions.DottedLinesOfAdoptedChildren;
            GetControl<ICheckBox>("chkDottedLinesOfDivorcedSpouses").Checked = fOptions.TreeChartOptions.DottedLinesOfCommonLawSpouses;
            GetControl<ICheckBox>("chkSeparateDAPLines").Checked = fOptions.TreeChartOptions.SeparateDatesAndPlacesLines;
            GetControl<ICheckBox>("chkOnlyLocality").Checked = fOptions.TreeChartOptions.OnlyLocality;
            GetControl<ICheckBox>("chkBoldNames").Checked = fOptions.TreeChartOptions.BoldNames;
            GetControl<ICheckBox>("chkMinimizingWidth").Checked = fOptions.TreeChartOptions.MinimizingWidth;
            GetControl<ICheckBox>("chkShowAge").Checked = fOptions.TreeChartOptions.AgeVisible;
            GetControl<ICheckBox>("chkTreeSurnameFirst").Checked = fOptions.TreeChartOptions.SurnameFirstInOrder;
            GetControl<ICheckBox>("chkURNotesVisible").Checked = fOptions.TreeChartOptions.URNotesVisible;
            GetControl<ICheckBox>("chkShortenDateRanges").Checked = fOptions.TreeChartOptions.ShortenDateRanges;
            GetControl<ICheckBox>("chkSameCardsWidth").Checked = fOptions.TreeChartOptions.SameCardsWidth;
            GetControl<ICheckBox>("chkFullNameOnOneLine").Checked = fOptions.TreeChartOptions.FullNameOnOneLine;
            GetControl<ICheckBox>("chkDateDesignations").Checked = fOptions.TreeChartOptions.DateDesignations;
            GetControl<ICheckBox>("chkMourningEdges").Checked = fOptions.TreeChartOptions.MourningEdges;
            GetControl<ICheckBox>("chkUseAdditionalDates").Checked = fOptions.TreeChartOptions.UseAdditionalDates;

            GetControl<ILabel>("lblMaleColor").BackColor = fOptions.TreeChartOptions.MaleColor;
            GetControl<ILabel>("lblFemaleColor").BackColor = fOptions.TreeChartOptions.FemaleColor;
            GetControl<ILabel>("lblUnkSexColor").BackColor = fOptions.TreeChartOptions.UnkSexColor;
            GetControl<ILabel>("lblUnHusbandColor").BackColor = fOptions.TreeChartOptions.UnHusbandColor;
            GetControl<ILabel>("lblUnWifeColor").BackColor = fOptions.TreeChartOptions.UnWifeColor;

            GetControl<INumericBox>("numMargins").Value = fOptions.TreeChartOptions.Margins;
            GetControl<INumericBox>("numBranchDist").Value = fOptions.TreeChartOptions.BranchDistance;
            GetControl<INumericBox>("numGenDist").Value = fOptions.TreeChartOptions.LevelDistance;
            GetControl<INumericBox>("numSpouseDist").Value = fOptions.TreeChartOptions.SpouseDistance;
            GetControl<INumericBox>("numPadding").Value = fOptions.TreeChartOptions.Padding;

            GetControl<ICheckBox>("chkSeparateDepth").Checked = fOptions.TreeChartOptions.SeparateDepth;

            GetControl<INumericBox>("numDefaultDepth").Value = fOptions.TreeChartOptions.DepthLimit;
            GetControl<INumericBox>("numDefaultDepthAncestors").Value = fOptions.TreeChartOptions.DepthLimitAncestors;
            GetControl<INumericBox>("numDefaultDepthDescendants").Value = fOptions.TreeChartOptions.DepthLimitDescendants;

            var hasExtraControls = !AppHost.Instance.HasFeatureSupport(Feature.Mobile);
            GetControl<ICheckBox>("chkUseExtraControls").Checked = fOptions.TreeChartOptions.UseExtraControls && hasExtraControls;
            GetControl<ICheckBox>("chkUseExtraControls").Visible = hasExtraControls;

            UpdateTreeChartFont();

            var cmbTextEffect = GetControl<IComboBox>("cmbTextEffect");
            cmbTextEffect.Clear();
            for (TextEffect itm = TextEffect.First; itm <= TextEffect.Last; itm++) {
                cmbTextEffect.AddItem(LangMan.LS(GKData.TextEffects[(int)itm]), itm);
            }
            cmbTextEffect.SetSelectedTag(fOptions.TreeChartOptions.TextEffect);

#if NETCORE
            cmbTextEffect.Enabled = false;
#else
            cmbTextEffect.Enabled = !AppHost.Instance.HasFeatureSupport(Feature.Mobile);
#endif
        }

        public void UpdateTreeChartFont()
        {
            GetControl<ILabel>("lblChartFont").Text = fOptions.TreeChartOptions.DefFontName + @", " + fOptions.TreeChartOptions.DefFontSize.ToString();
        }

        public void ChangeSeparateDepth()
        {
            bool sepDepth = GetControl<ICheckBox>("chkSeparateDepth").Checked;

            GetControl<INumericBox>("numDefaultDepth").Enabled = !sepDepth;
            GetControl<INumericBox>("numDefaultDepthAncestors").Enabled = sepDepth;
            GetControl<INumericBox>("numDefaultDepthDescendants").Enabled = sepDepth;
        }

        public void ChangeTreeChartOption()
        {
            GetControl<ICheckBox>("chkSeparateDAPLines").Enabled = GetControl<ICheckBox>("chkShowPlaces").Checked;
            GetControl<ICheckBox>("chkOnlyLocality").Enabled = GetControl<ICheckBox>("chkShowPlaces").Checked;

            GetControl<ICheckBox>("chkDefaultPortraits").Enabled = GetControl<ICheckBox>("chkPortraitsVisible").Checked;

            GetControl<ICheckBox>("chkDiffLines").Enabled = GetControl<ICheckBox>("chkName").Checked && GetControl<ICheckBox>("chkPatronymic").Checked;

            GetControl<ICheckBox>("chkOnlyYears").Enabled = GetControl<ICheckBox>("chkBirthDate").Checked && GetControl<ICheckBox>("chkDeathDate").Checked;

            GetControl<ICheckBox>("chkShowAge").Enabled = GetControl<ICheckBox>("chkOnlyYears").Checked && !GetControl<ICheckBox>("chkShowPlaces").Checked;

            bool fullNameOnOneLine = GetControl<ICheckBox>("chkFullNameOnOneLine").Checked;
            GetControl<ICheckBox>("chkSurname").Enabled = !fullNameOnOneLine;
            GetControl<ICheckBox>("chkName").Enabled = !fullNameOnOneLine;
            GetControl<ICheckBox>("chkPatronymic").Enabled = !fullNameOnOneLine;
            GetControl<ICheckBox>("chkDiffLines").Enabled = !fullNameOnOneLine;
        }

        public void AcceptTreeChartsOptions()
        {
            fOptions.TreeChartOptions.FamilyVisible = GetControl<ICheckBox>("chkSurname").Checked;
            fOptions.TreeChartOptions.NameVisible = GetControl<ICheckBox>("chkName").Checked;
            fOptions.TreeChartOptions.PatronymicVisible = GetControl<ICheckBox>("chkPatronymic").Checked;
            fOptions.TreeChartOptions.DiffLines = GetControl<ICheckBox>("chkDiffLines").Checked;
            fOptions.TreeChartOptions.BirthDateVisible = GetControl<ICheckBox>("chkBirthDate").Checked;
            fOptions.TreeChartOptions.DeathDateVisible = GetControl<ICheckBox>("chkDeathDate").Checked;
            fOptions.TreeChartOptions.OnlyYears = GetControl<ICheckBox>("chkOnlyYears").Checked;
            fOptions.TreeChartOptions.MarriagesDates = GetControl<ICheckBox>("chkMarriagesDates").Checked;
            fOptions.TreeChartOptions.Kinship = GetControl<ICheckBox>("chkKinship").Checked;
            fOptions.TreeChartOptions.SignsVisible = GetControl<ICheckBox>("chkSignsVisible").Checked;
            fOptions.TreeChartOptions.Decorative = GetControl<ICheckBox>("chkTreeDecorative").Checked;
            fOptions.TreeChartOptions.PortraitsVisible = GetControl<ICheckBox>("chkPortraitsVisible").Checked;
            fOptions.TreeChartOptions.DefaultPortraits = GetControl<ICheckBox>("chkDefaultPortraits").Checked;
            fOptions.TreeChartOptions.InvertedTree = GetControl<ICheckBox>("chkInvertedTree").Checked;
            fOptions.TreeChartOptions.ChildlessExclude = GetControl<ICheckBox>("chkChildlessExclude").Checked;
            fOptions.TreeChartOptions.ShowPlaces = GetControl<ICheckBox>("chkShowPlaces").Checked;
            fOptions.TreeChartOptions.HideUnknownSpouses = GetControl<ICheckBox>("chkHideUnknownSpouses").Checked;
            fOptions.CheckTreeSize = GetControl<ICheckBox>("chkCheckTreeSize").Checked;
            fOptions.TreeChartOptions.DottedLinesOfAdoptedChildren = GetControl<ICheckBox>("chkDottedLinesOfAdoptedChildren").Checked;
            fOptions.TreeChartOptions.DottedLinesOfCommonLawSpouses = GetControl<ICheckBox>("chkDottedLinesOfDivorcedSpouses").Checked;
            fOptions.TreeChartOptions.SeparateDatesAndPlacesLines = GetControl<ICheckBox>("chkSeparateDAPLines").Checked;
            fOptions.TreeChartOptions.OnlyLocality = GetControl<ICheckBox>("chkOnlyLocality").Checked;
            fOptions.TreeChartOptions.BoldNames = GetControl<ICheckBox>("chkBoldNames").Checked;
            fOptions.TreeChartOptions.MinimizingWidth = GetControl<ICheckBox>("chkMinimizingWidth").Checked;
            fOptions.TreeChartOptions.AgeVisible = GetControl<ICheckBox>("chkShowAge").Checked;
            fOptions.TreeChartOptions.SurnameFirstInOrder = GetControl<ICheckBox>("chkTreeSurnameFirst").Checked;
            fOptions.TreeChartOptions.URNotesVisible = GetControl<ICheckBox>("chkURNotesVisible").Checked;
            fOptions.TreeChartOptions.ShortenDateRanges = GetControl<ICheckBox>("chkShortenDateRanges").Checked;
            fOptions.TreeChartOptions.SameCardsWidth = GetControl<ICheckBox>("chkSameCardsWidth").Checked;
            fOptions.TreeChartOptions.FullNameOnOneLine = GetControl<ICheckBox>("chkFullNameOnOneLine").Checked;
            fOptions.TreeChartOptions.DateDesignations = GetControl<ICheckBox>("chkDateDesignations").Checked;
            fOptions.TreeChartOptions.MourningEdges = GetControl<ICheckBox>("chkMourningEdges").Checked;
            fOptions.TreeChartOptions.UseAdditionalDates = GetControl<ICheckBox>("chkUseAdditionalDates").Checked;

            fOptions.TreeChartOptions.MaleColor = GetControl<ILabel>("lblMaleColor").BackColor;
            fOptions.TreeChartOptions.FemaleColor = GetControl<ILabel>("lblFemaleColor").BackColor;
            fOptions.TreeChartOptions.UnkSexColor = GetControl<ILabel>("lblUnkSexColor").BackColor;
            fOptions.TreeChartOptions.UnHusbandColor = GetControl<ILabel>("lblUnHusbandColor").BackColor;
            fOptions.TreeChartOptions.UnWifeColor = GetControl<ILabel>("lblUnWifeColor").BackColor;

            fOptions.TreeChartOptions.Margins = (int)GetControl<INumericBox>("numMargins").Value;
            fOptions.TreeChartOptions.BranchDistance = (int)GetControl<INumericBox>("numBranchDist").Value;
            fOptions.TreeChartOptions.LevelDistance = (int)GetControl<INumericBox>("numGenDist").Value;
            fOptions.TreeChartOptions.SpouseDistance = (int)GetControl<INumericBox>("numSpouseDist").Value;
            fOptions.TreeChartOptions.Padding = (int)GetControl<INumericBox>("numPadding").Value;

            fOptions.TreeChartOptions.SeparateDepth = GetControl<ICheckBox>("chkSeparateDepth").Checked;
            fOptions.TreeChartOptions.DepthLimit = (int)GetControl<INumericBox>("numDefaultDepth").Value;
            fOptions.TreeChartOptions.DepthLimitAncestors = (int)GetControl<INumericBox>("numDefaultDepthAncestors").Value;
            fOptions.TreeChartOptions.DepthLimitDescendants = (int)GetControl<INumericBox>("numDefaultDepthDescendants").Value;

            fOptions.TreeChartOptions.UseExtraControls = GetControl<ICheckBox>("chkUseExtraControls").Checked;

            fOptions.TreeChartOptions.TextEffect = GetControl<IComboBox>("cmbTextEffect").GetSelectedTag<TextEffect>();
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

            // pedigrees
            UpdatePedigreesOptions();

            // specials
            UpdateSpecials();

            // event types
            UpdateEventTypes();

            // plugins
            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                UpdatePlugins();
            }
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

            // pedigrees
            AcceptPedigreesOptions();

            // specials
            AcceptSpecials();

            // plugins
            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                AcceptPlugins();
            }

            return true;
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.MIOptions);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);

            // Common
            GetControl<ITabPage>("pageCommon").Text = LangMan.LS(LSID.Common);

            GetControl<IGroupBox>("grpInternet").Text = LangMan.LS(LSID.Internet);
            GetControl<ICheckBox>("chkUseProxy").Text = LangMan.LS(LSID.ProxyUse);
            GetControl<ILabel>("lblProxyServer").Text = LangMan.LS(LSID.ProxyServer);
            GetControl<ILabel>("lblProxyPort").Text = LangMan.LS(LSID.ProxyPort);
            GetControl<ILabel>("lblProxyLogin").Text = LangMan.LS(LSID.ProxyLogin);
            GetControl<ILabel>("lblProxyPassword").Text = LangMan.LS(LSID.Password);

            GetControl<IGroupBox>("grpFileBackup").Text = LangMan.LS(LSID.FileBackup);
            GetControl<IRadioButton>("radFBNone").Text = LangMan.LS(LSID.Not);
            GetControl<IRadioButton>("radFBOnlyPrev").Text = LangMan.LS(LSID.BackupOnlyPrev);
            GetControl<IRadioButton>("radFBEachRevision").Text = LangMan.LS(LSID.BackupEachRevision);

            GetControl<ICheckBox>("chkAutosave").Text = LangMan.LS(LSID.Autosave);
            GetControl<ILabel>("lblMinutes").Text = LangMan.LS(LSID.Minutes);
            GetControl<ILabel>("lblBackupRevisionsMaxCount").Text = LangMan.LS(LSID.BackupRevisionsMaxCount);

            GetControl<IGroupBox>("grpOther").Text = LangMan.LS(LSID.Other);
            GetControl<ICheckBox>("chkShowOnStart").Text = LangMan.LS(LSID.StartupTips);
            GetControl<ICheckBox>("chkLoadRecentFiles").Text = LangMan.LS(LSID.LoadRecentFiles);
            GetControl<ICheckBox>("chkAutoCheckUpdates").Text = LangMan.LS(LSID.AutoCheckUpdates);
            GetControl<ICheckBox>("chkCharsetDetection").Text = LangMan.LS(LSID.CharsetDetection);
            GetControl<ICheckBox>("chkDialogClosingWarn").Text = LangMan.LS(LSID.WarnForClosingDialog);

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ICheckBox>("chkDisplayFullFileName").Text = LangMan.LS(LSID.DisplayFullFileName);
            }

            GetControl<ILabel>("lblLanguage").Text = LangMan.LS(LSID.Language);
            GetControl<ILabel>("lblGeocoder").Text = LangMan.LS(LSID.Geocoder);
            GetControl<ILabel>("lblGeoSearchCountry").Text = LangMan.LS(LSID.GeoSearchCountryRestriction);

            GetControl<ILabel>("lblCertaintyAlgorithm").Text = LangMan.LS(LSID.CertaintyAlgorithm);
            var cmbCA = GetControl<IComboBox>("cmbCertaintyAlgorithm");
            cmbCA.Clear();
            for (var ca = CertaintyAlgorithm.WeightedAverage; ca <= CertaintyAlgorithm.Maximum; ca++) {
                cmbCA.AddItem(ca.ToString(), ca);
            }

            // Multimedia
            GetControl<ITabPage>("pageMultimedia").Text = LangMan.LS(LSID.RPMultimedia);

            GetControl<ICheckBox>("chkRemovableMediaWarning").Text = LangMan.LS(LSID.RemovableMediaWarningOption);
            GetControl<ICheckBox>("chkEmbeddedMediaPlayer").Text = LangMan.LS(LSID.EmbeddedMediaPlayer);

            GetControl<ICheckBox>("chkAllowMediaDirectRefs").Text = LangMan.LS(LSID.AllowMediaDirectReferences);
            GetControl<ICheckBox>("chkAllowMediaStoreRelativeReferences").Text = LangMan.LS(LSID.AllowMediaRelativeReferences);

            GetControl<ILabel>("lblMediaStoreDefault").Text = LangMan.LS(LSID.MediaStoreDefault);

            var combo = GetControl<IComboBox>("cmbMediaStoreDefault");
            combo.Clear();
            for (MediaStoreType mst = MediaStoreType.mstReference; mst <= MediaStoreType.mstURL; mst++) {
                combo.AddItem(LangMan.LS(GKData.GKStoreTypes[(int)mst].Name), mst);
            }

            GetControl<ICheckBox>("chkAllowDeleteMediaFileFromStgArc").Text = LangMan.LS(LSID.AllowDeleteMediaFileFromStgArc);
            GetControl<ICheckBox>("chkAllowDeleteMediaFileFromRefs").Text = LangMan.LS(LSID.AllowDeleteMediaFileFromRefs);
            GetControl<ICheckBox>("chkDeleteMediaFileWithoutConfirm").Text = LangMan.LS(LSID.DeleteMediaFileWithoutConfirm);
            GetControl<ICheckBox>("chkHighlightInaccessibleFiles").Text = LangMan.LS(LSID.HighlightInaccessibleFiles);

            // Charts
            GetControl<ITabPage>("pageCharts").Text = LangMan.LS(LSID.Charts);

            GetControl<ITabPage>("pageTreeChart").Text = LangMan.LS(LSID.Trees);

            GetControl<ITabPage>("pageTreePersons").Text = LangMan.LS(LSID.ViewTree);
            GetControl<ITabPage>("pageTreeDesign").Text = LangMan.LS(LSID.Decor);

            GetControl<ICheckBox>("chkSurname").Text = LangMan.LS(LSID.Surname);
            GetControl<ICheckBox>("chkName").Text = LangMan.LS(LSID.GivenName);
            GetControl<ICheckBox>("chkPatronymic").Text = LangMan.LS(LSID.Patronymic);
            GetControl<ICheckBox>("chkDiffLines").Text = LangMan.LS(LSID.DiffLines);
            GetControl<ICheckBox>("chkBirthDate").Text = LangMan.LS(LSID.BirthDate);
            GetControl<ICheckBox>("chkDeathDate").Text = LangMan.LS(LSID.DeathDate);
            GetControl<ICheckBox>("chkOnlyYears").Text = LangMan.LS(LSID.OnlyYears);
            GetControl<ICheckBox>("chkMarriagesDates").Text = LangMan.LS(LSID.MarriagesDates);
            GetControl<ICheckBox>("chkKinship").Text = LangMan.LS(LSID.Kinship);
            GetControl<ICheckBox>("chkSignsVisible").Text = LangMan.LS(LSID.SignsVisible);
            GetControl<ICheckBox>("chkTreeDecorative").Text = LangMan.LS(LSID.TreeDecorative);
            GetControl<ICheckBox>("chkPortraitsVisible").Text = LangMan.LS(LSID.PortraitsVisible);
            GetControl<ICheckBox>("chkDefaultPortraits").Text = LangMan.LS(LSID.DefaultPortraits);
            GetControl<ICheckBox>("chkInvertedTree").Text = LangMan.LS(LSID.InvertedTree);
            GetControl<ICheckBox>("chkChildlessExclude").Text = LangMan.LS(LSID.ChildlessExclude);
            GetControl<ICheckBox>("chkShowPlaces").Text = LangMan.LS(LSID.ShowPlaces);
            GetControl<ICheckBox>("chkSeparateDAPLines").Text = LangMan.LS(LSID.SeparateDatesAndPlacesLines);
            GetControl<ICheckBox>("chkOnlyLocality").Text = LangMan.LS(LSID.OnlyLocality);
            GetControl<ICheckBox>("chkHideUnknownSpouses").Text = LangMan.LS(LSID.HideUnknownSpouses);
            GetControl<ICheckBox>("chkCheckTreeSize").Text = LangMan.LS(LSID.CheckTreeSize);
            GetControl<ICheckBox>("chkDottedLinesOfAdoptedChildren").Text = LangMan.LS(LSID.DottedLinesOfAdoptedChildren);
            GetControl<ICheckBox>("chkDottedLinesOfDivorcedSpouses").Text = LangMan.LS(LSID.DottedLinesOfCommonLawSpouses);
            GetControl<ICheckBox>("chkBoldNames").Text = LangMan.LS(LSID.BoldNames);
            GetControl<ICheckBox>("chkMinimizingWidth").Text = LangMan.LS(LSID.MinimizingWidth);
            GetControl<ICheckBox>("chkShowAge").Text = LangMan.LS(LSID.ShowAge);
            GetControl<ICheckBox>("chkTreeSurnameFirst").Text = LangMan.LS(LSID.SurnameFirstInOrder);
            GetControl<ICheckBox>("chkURNotesVisible").Text = LangMan.LS(LSID.ShowTreeNotes);
            GetControl<ICheckBox>("chkShortenDateRanges").Text = LangMan.LS(LSID.ShortenDateRanges);
            GetControl<ICheckBox>("chkSameCardsWidth").Text = LangMan.LS(LSID.SameCardsWidth);
            GetControl<ICheckBox>("chkFullNameOnOneLine").Text = LangMan.LS(LSID.FullNameOnOneLine);
            GetControl<ICheckBox>("chkDateDesignations").Text = LangMan.LS(LSID.DateDesignations);
            GetControl<ICheckBox>("chkMourningEdges").Text = LangMan.LS(LSID.MourningEdges);
            GetControl<ICheckBox>("chkUseAdditionalDates").Text = LangMan.LS(LSID.UseAdditionalDates);

            GetControl<ILabel>("lblMaleColor").Text = LangMan.LS(LSID.Man);
            GetControl<ILabel>("lblFemaleColor").Text = LangMan.LS(LSID.Woman);
            GetControl<ILabel>("lblUnkSexColor").Text = LangMan.LS(LSID.UnkSex);
            GetControl<ILabel>("lblUnHusbandColor").Text = LangMan.LS(LSID.UnHusband);
            GetControl<ILabel>("lblUnWifeColor").Text = LangMan.LS(LSID.UnWife);
            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ILabel>("lblFont").Text = LangMan.LS(LSID.Font);
            }

            GetControl<IGroupBox>("grpSpacings").Text = LangMan.LS(LSID.Spacings);
            GetControl<ILabel>("lblMargins").Text = LangMan.LS(LSID.Margins);
            GetControl<ILabel>("lblBranchDist").Text = LangMan.LS(LSID.BranchDist);
            GetControl<ILabel>("lblGenDist").Text = LangMan.LS(LSID.GenDist);
            GetControl<ILabel>("lblSpouseDist").Text = LangMan.LS(LSID.SpouseDist);
            GetControl<ILabel>("lblPadding").Text = LangMan.LS(LSID.Padding);

            GetControl<ICheckBox>("chkSeparateDepth").Text = LangMan.LS(LSID.SeparateDepth);
            GetControl<ILabel>("lblDefaultDepth").Text = LangMan.LS(LSID.DefaultDepth);
            GetControl<ILabel>("lblDefaultDepthAncestors").Text = LangMan.LS(LSID.DefaultDepth) + ": " + LangMan.LS(LSID.Ancestors);
            GetControl<ILabel>("lblDefaultDepthDescendants").Text = LangMan.LS(LSID.DefaultDepth) + ": " + LangMan.LS(LSID.Descendants);

            GetControl<ICheckBox>("chkUseExtraControls").Text = LangMan.LS(LSID.UseExtraControls);

            GetControl<ILabel>("lblTextEffect").Text = LangMan.LS(LSID.TextEffect);

            GetControl<ITabPage>("pageAncCircle").Text = LangMan.LS(LSID.AncestorsCircle);

            // UIView
            GetControl<ITabPage>("pageUIView").Text = LangMan.LS(LSID.Interface);

            GetControl<ITabPage>("pageViewCommon").Text = LangMan.LS(LSID.ListsAll);

            GetControl<IGroupBox>("rgFNPFormat").Text = LangMan.LS(LSID.NamesFormat);
            GetControl<IRadioButton>("radSNP").Text = LangMan.LS(LSID.NF1);
            GetControl<IRadioButton>("radS_NP").Text = LangMan.LS(LSID.NF2);
            GetControl<IRadioButton>("radS_N_P").Text = LangMan.LS(LSID.NF3);

            GetControl<ICheckBox>("chkPlacesWithAddress").Text = LangMan.LS(LSID.PlacesWithAddress);
            GetControl<ICheckBox>("chkHighlightUnparented").Text = LangMan.LS(LSID.HighlightUnparented);
            GetControl<ICheckBox>("chkHighlightUnmarried").Text = LangMan.LS(LSID.HighlightUnmarried);

            GetControl<ICheckBox>("chkAutoSortChildren").Text = LangMan.LS(LSID.AutoSortChildren);
            GetControl<ICheckBox>("chkAutoSortSpouses").Text = LangMan.LS(LSID.AutoSortSpouses);
            GetControl<ICheckBox>("chkFirstCapitalLetterInNames").Text = LangMan.LS(LSID.FirstCapitalLetterInNames);
            GetControl<ICheckBox>("chkShortKinshipForm").Text = LangMan.LS(LSID.ShortKinshipForm);
            GetControl<ICheckBox>("chkSurnameFirstInOrder").Text = LangMan.LS(LSID.SurnameFirstInOrder);
            GetControl<ICheckBox>("chkSurnameInCapitals").Text = LangMan.LS(LSID.SurnameInCapitals);

            GetControl<ICheckBox>("chkUseSurnamesInPSF").Text = LangMan.LS(LSID.UseSurnamesInPersonSelectionFilter);
            GetControl<ICheckBox>("chkUseBirthDatesInPSF").Text = LangMan.LS(LSID.UseBirthDatesInPersonSelectionFilter);

            GetControl<IGroupBox>("grpDateFormat").Text = LangMan.LS(LSID.DateFormat);
            GetControl<ICheckBox>("chkShowDatesCalendar").Text = LangMan.LS(LSID.ShowDatesCalendar);
            GetControl<ICheckBox>("chkShowDatesSigns").Text = LangMan.LS(LSID.ShowDatesSigns);
            GetControl<ICheckBox>("chkLocalizedCalendarSignatures").Text = LangMan.LS(LSID.LocalizedCalendarSignaturesOptLabel);

            GetControl<IGroupBox>("grpAdvancedNames").Text = LangMan.LS(LSID.AdditionalNames);
            GetControl<ICheckBox>("chkExtendWomanSurnames").Text = LangMan.LS(LSID.ExtendedWomanSurnames);
            GetControl<IRadioButton>("radMaiden_Married").Text = LangMan.LS(LSID.WSF_Maiden_Married);
            GetControl<IRadioButton>("radMarried_Maiden").Text = LangMan.LS(LSID.WSF_Married_Maiden);
            GetControl<IRadioButton>("radMaiden").Text = LangMan.LS(LSID.WSF_Maiden);
            GetControl<IRadioButton>("radMarried").Text = LangMan.LS(LSID.WSF_Married);
            GetControl<ICheckBox>("chkSimpleSingleSurnames").Text = LangMan.LS(LSID.SimpleSingleSurnames);

            GetControl<ITabPage>("pageViewPersons").Text = LangMan.LS(LSID.ListPersons);
            GetControl<IButton>("btnResetDefaults").Text = LangMan.LS(LSID.DefList);

            GetControl<ILabel>("lblMatchPatternMethod").Text = LangMan.LS(LSID.MatchPatternMethod);

            GetControl<ITabPage>("pageNavigation").Text = LangMan.LS(LSID.Navigation);

            GetControl<ITabPage>("pageGeo").Text = LangMan.LS(LSID.LocationsAndMaps);

            // Pedigree
            GetControl<ITabPage>("pagePedigree").Text = LangMan.LS(LSID.Pedigrees);

            GetControl<IGroupBox>("grpPedigree").Text = LangMan.LS(LSID.PedigreeGen);
            GetControl<ICheckBox>("chkAttributes").Text = LangMan.LS(LSID.IncludeAttributes);
            GetControl<ICheckBox>("chkNotes").Text = LangMan.LS(LSID.IncludeNotes);
            GetControl<ICheckBox>("chkSources").Text = LangMan.LS(LSID.IncludeSources);
            GetControl<ICheckBox>("chkSourcePages").Text = LangMan.LS(LSID.IncludeSourcePages);
            GetControl<ICheckBox>("chkGenerations").Text = LangMan.LS(LSID.IncludeGenerations);
            GetControl<ICheckBox>("chkPortraits").Text = LangMan.LS(LSID.IncludePortraits);

            GetControl<IGroupBox>("grpPedigreeFormat").Text = LangMan.LS(LSID.PedigreeFormat);
            GetControl<IRadioButton>("radExcess").Text = LangMan.LS(LSID.PF1);
            GetControl<IRadioButton>("radCompact").Text = LangMan.LS(LSID.PF2);

            GetControl<ILabel>("lblAscendNumbering").Text = LangMan.LS(LSID.AscendNumbering);
            GetControl<ILabel>("lblDescendNumbering").Text = LangMan.LS(LSID.DescendNumbering);

            // Specials
            GetControl<ITabPage>("pageSpecials").Text = LangMan.LS(LSID.Specials);
            GetControl<ICheckBox>("chkUseInlineImagesInSvg").Text = LangMan.LS(LSID.UseInlineImagesInSvg);
            GetControl<ICheckBox>("chkUseExtendedNotes").Text = LangMan.LS(LSID.UseExtendedNotes);
            GetControl<ICheckBox>("chkKeepRichNames").Text = LangMan.LS(LSID.KeepRichNames);

            GetControl<ILabel>("lblChartWindowsShowMode").Text = LangMan.LS(LSID.ChartWindowsShowMode);
            combo = GetControl<IComboBox>("cmbChartWindowsShowMode");
            combo.Clear();
            for (ChartWindowsShowMode cwsm = ChartWindowsShowMode.Default; cwsm <= ChartWindowsShowMode.RightHalf; cwsm++) {
                combo.AddItem(LangMan.LS(GKData.ChartWindowsShowModes[(int)cwsm]), cwsm);
            }

            GetControl<ICheckBox>("chkExtendedTree").Text = LangMan.LS(LSID.ExtendedTree);
            GetControl<ICheckBox>("chkSAFByAllNames").Text = LangMan.LS(LSID.SearchAndFilterByAllNames);
            GetControl<ICheckBox>("chkKeepInfoPansOverallSize").Text = LangMan.LS(LSID.KeepInfoPansOverallSize);
            GetControl<ICheckBox>("chkFilesOverwriteWarn").Text = LangMan.LS(LSID.FilesOverwriteWarn);
            GetControl<ICheckBox>("chkExtendedKinships").Text = LangMan.LS(LSID.ExtendedKinships);
            GetControl<ICheckBox>("chkExtendedLocations").Text = LangMan.LS(LSID.ExtendedLocations);
            GetControl<ICheckBox>("chkELAbbreviatedNames").Text = LangMan.LS(LSID.EL_AbbreviatedNames);
            GetControl<ICheckBox>("chkReversePlacesOrder").Text = LangMan.LS(LSID.ReversePlacesOrder);
            GetControl<ICheckBox>("chkShowNumberOfSubstructures").Text = LangMan.LS(LSID.ShowNumberOfSubstructures);
            GetControl<ICheckBox>("chkSearchPlacesWithoutCoords").Text = LangMan.LS(LSID.SearchPlacesWithoutCoords);

            // event types
            GetControl<ITabPage>("pageEventTypes").Text = LangMan.LS(LSID.EventTypes);

            // Plugins
            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ITabPage>("pagePlugins").Text = LangMan.LS(LSID.Plugins);
            }
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            GetControl<IButton>("btnColumnUp").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_MoveUp, true);
            GetControl<IButton>("btnColumnDown").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_MoveDown, true);
        }
    }
}
