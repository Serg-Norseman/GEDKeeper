/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

using BSLib.Design;
using BSLib.Design.MVP.Controls;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Options;
using GKCore.Plugins;
using GKCore.Types;

namespace GKCore.Controllers
{
    public class OptionsDlgController : DialogController<IOptionsDlg>
    {
        private GlobalOptions fOptions;
        private readonly ListColumns fTempColumns;


        public GlobalOptions Options
        {
            get { return fOptions; }
        }


        public OptionsDlgController(IOptionsDlg view) : base(view)
        {
            fOptions = GlobalOptions.Instance;
            fTempColumns = IndividualListMan.CreateIndividualListColumns();

            FillGeoSearchCountries();
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

            //lstPersonColumns.ItemCheck -= ListPersonColumns_ItemCheck;
            listView.BeginUpdate();
            try {
                listView.ClearItems();

                int num = fTempColumns.Count;
                for (int i = 0; i < num; i++) {
                    ListColumn column = fTempColumns.OrderedColumns[i];
                    listView.AddItem(null, column.CurActive, LangMan.LS(column.ColName));
                }
            } finally {
                listView.EndUpdate();
            }
            //lstPersonColumns.ItemCheck += ListPersonColumns_ItemCheck;
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
            fTempColumns.CopyTo(fOptions.IndividualListColumns);
        }

        public void UpdateProxyOptions()
        {
            GetControl<ICheckBox>("chkUseProxy").Checked = fOptions.Proxy.UseProxy;
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

        public void UpdateOtherOptions()
        {
            GetControl<ICheckBox>("chkShowOnStart").Checked = fOptions.ShowTips;
            GetControl<ICheckBox>("chkLoadRecentFiles").Checked = fOptions.LoadRecentFiles;
            GetControl<ICheckBox>("chkAutoCheckUpdates").Checked = fOptions.AutoCheckUpdates;
            GetControl<ICheckBox>("chkCharsetDetection").Checked = fOptions.CharsetDetection;
            GetControl<ICheckBox>("chkDialogClosingWarn").Checked = fOptions.DialogClosingWarn;

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
        }

        public void AcceptOtherOptions()
        {
            fOptions.ShowTips = GetControl<ICheckBox>("chkShowOnStart").Checked;
            fOptions.LoadRecentFiles = GetControl<ICheckBox>("chkLoadRecentFiles").Checked;
            fOptions.AutoCheckUpdates = GetControl<ICheckBox>("chkAutoCheckUpdates").Checked;
            fOptions.CharsetDetection = GetControl<ICheckBox>("chkCharsetDetection").Checked;
            fOptions.DialogClosingWarn = GetControl<ICheckBox>("chkDialogClosingWarn").Checked;

            fOptions.Geocoder = GetControl<IComboBox>("cmbGeocoder").Text;
            fOptions.GeoSearchCountry = GetControl<IComboBox>("cmbGeoSearchCountry").Text;

            var item = GetControl<IComboBox>("cmbLanguages").SelectedItem as ComboItem<int>;
            if (item != null) {
                AppHost.Instance.LoadLanguage(item.Tag);
            }
        }

        public void UpdatePedigreesOptions()
        {
            GetControl<ICheckBox>("chkAttributes").Checked = fOptions.PedigreeOptions.IncludeAttributes;
            GetControl<ICheckBox>("chkNotes").Checked = fOptions.PedigreeOptions.IncludeNotes;
            GetControl<ICheckBox>("chkSources").Checked = fOptions.PedigreeOptions.IncludeSources;
            GetControl<ICheckBox>("chkGenerations").Checked = fOptions.PedigreeOptions.IncludeGenerations;

            switch (fOptions.PedigreeOptions.Format) {
                case PedigreeFormat.Excess:
                    GetControl<IRadioButton>("radExcess").Checked = true;
                    break;
                case PedigreeFormat.Compact:
                    GetControl<IRadioButton>("radCompact").Checked = true;
                    break;
            }
        }

        public void AcceptPedigreesOptions()
        {
            fOptions.PedigreeOptions.IncludeAttributes = GetControl<ICheckBox>("chkAttributes").Checked;
            fOptions.PedigreeOptions.IncludeNotes = GetControl<ICheckBox>("chkNotes").Checked;
            fOptions.PedigreeOptions.IncludeSources = GetControl<ICheckBox>("chkSources").Checked;
            fOptions.PedigreeOptions.IncludeGenerations = GetControl<ICheckBox>("chkGenerations").Checked;

            if (GetControl<IRadioButton>("radExcess").Checked) {
                fOptions.PedigreeOptions.Format = PedigreeFormat.Excess;
            } else if (GetControl<IRadioButton>("radCompact").Checked) {
                fOptions.PedigreeOptions.Format = PedigreeFormat.Compact;
            }
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

            GetControl<ICheckBox>("chkPlacesWithAddress").Checked = fOptions.PlacesWithAddress;
            GetControl<ICheckBox>("chkHighlightUnparented").Checked = fOptions.ListHighlightUnparentedPersons;
            GetControl<ICheckBox>("chkHighlightUnmarried").Checked = fOptions.ListHighlightUnmarriedPersons;

            GetControl<ICheckBox>("chkAutoSortChildren").Checked = fOptions.AutoSortChildren;
            GetControl<ICheckBox>("chkAutoSortSpouses").Checked = fOptions.AutoSortSpouses;
            GetControl<ICheckBox>("chkFirstCapitalLetterInNames").Checked = fOptions.FirstCapitalLetterInNames;

            GetControl<ICheckBox>("chkShortKinshipForm").Checked = fOptions.ShortKinshipForm;
            GetControl<ICheckBox>("chkSurnameFirstInOrder").Checked = fOptions.SurnameFirstInOrder;
            GetControl<ICheckBox>("chkSurnameInCapitals").Checked = fOptions.SurnameInCapitals;
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

            fOptions.PlacesWithAddress = GetControl<ICheckBox>("chkPlacesWithAddress").Checked;
            fOptions.ListHighlightUnparentedPersons = GetControl<ICheckBox>("chkHighlightUnparented").Checked;
            fOptions.ListHighlightUnmarriedPersons = GetControl<ICheckBox>("chkHighlightUnmarried").Checked;

            fOptions.AutoSortChildren = GetControl<ICheckBox>("chkAutoSortChildren").Checked;
            fOptions.AutoSortSpouses = GetControl<ICheckBox>("chkAutoSortSpouses").Checked;
            fOptions.FirstCapitalLetterInNames = GetControl<ICheckBox>("chkFirstCapitalLetterInNames").Checked;

            fOptions.ShortKinshipForm = GetControl<ICheckBox>("chkShortKinshipForm").Checked;
            fOptions.SurnameFirstInOrder = GetControl<ICheckBox>("chkSurnameFirstInOrder").Checked;
            fOptions.SurnameInCapitals = GetControl<ICheckBox>("chkSurnameInCapitals").Checked;
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

        public void UpdateMediaOptions()
        {
            GetControl<ICheckBox>("chkRemovableMediaWarning").Checked = fOptions.RemovableMediaWarning;
            GetControl<ICheckBox>("chkEmbeddedMediaPlayer").Checked = fOptions.EmbeddedMediaPlayer;
            GetControl<ICheckBox>("chkAllowMediaDirectRefs").Checked = fOptions.AllowMediaStoreReferences;
            GetControl<ICheckBox>("chkAllowMediaStoreRelativeReferences").Checked = fOptions.AllowMediaStoreRelativeReferences;
            GetControl<IComboBox>("cmbMediaStoreDefault").SetSelectedTag<MediaStoreType>(fOptions.MediaStoreDefault);
            GetControl<ICheckBox>("chkAllowDeleteMediaFileFromStgArc").Checked = fOptions.AllowDeleteMediaFileFromStgArc;
            GetControl<ICheckBox>("chkAllowDeleteMediaFileFromRefs").Checked = fOptions.AllowDeleteMediaFileFromRefs;
            GetControl<ICheckBox>("chkDeleteMediaFileWithoutConfirm").Checked = fOptions.DeleteMediaFileWithoutConfirm;
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
            GetControl<ICheckBox>("chkSeparateDAPLines").Checked = fOptions.TreeChartOptions.SeparateDatesAndPlacesLines;
            GetControl<ICheckBox>("chkOnlyLocality").Checked = fOptions.TreeChartOptions.OnlyLocality;
            GetControl<ICheckBox>("chkBoldNames").Checked = fOptions.TreeChartOptions.BoldNames;
            GetControl<ICheckBox>("chkMinimizingWidth").Checked = fOptions.TreeChartOptions.MinimizingWidth;
            GetControl<ICheckBox>("chkShowAge").Checked = fOptions.TreeChartOptions.AgeVisible;

            GetControl<ILabel>("lblMaleColor").BackColor = fOptions.TreeChartOptions.MaleColor;
            GetControl<ILabel>("lblFemaleColor").BackColor = fOptions.TreeChartOptions.FemaleColor;
            GetControl<ILabel>("lblUnkSexColor").BackColor = fOptions.TreeChartOptions.UnkSexColor;
            GetControl<ILabel>("lblUnHusbandColor").BackColor = fOptions.TreeChartOptions.UnHusbandColor;
            GetControl<ILabel>("lblUnWifeColor").BackColor = fOptions.TreeChartOptions.UnWifeColor;

            GetControl<INumericBox>("numMargins").Value = fOptions.TreeChartOptions.Margins;
            GetControl<INumericBox>("numBranchDist").Value = fOptions.TreeChartOptions.BranchDistance;
            GetControl<INumericBox>("numGenDist").Value = fOptions.TreeChartOptions.LevelDistance;
            GetControl<INumericBox>("numSpouseDist").Value = fOptions.TreeChartOptions.SpouseDistance;

            GetControl<ICheckBox>("chkSeparateDepth").Checked = fOptions.TreeChartOptions.SeparateDepth;

            GetControl<INumericBox>("numDefaultDepth").Value = fOptions.TreeChartOptions.DepthLimit;
            GetControl<INumericBox>("numDefaultDepthAncestors").Value = fOptions.TreeChartOptions.DepthLimitAncestors;
            GetControl<INumericBox>("numDefaultDepthDescendants").Value = fOptions.TreeChartOptions.DepthLimitDescendants;

            UpdateTreeChartFont();
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
            fOptions.TreeChartOptions.SeparateDatesAndPlacesLines = GetControl<ICheckBox>("chkSeparateDAPLines").Checked;
            fOptions.TreeChartOptions.OnlyLocality = GetControl<ICheckBox>("chkOnlyLocality").Checked;
            fOptions.TreeChartOptions.BoldNames = GetControl<ICheckBox>("chkBoldNames").Checked;
            fOptions.TreeChartOptions.MinimizingWidth = GetControl<ICheckBox>("chkMinimizingWidth").Checked;
            fOptions.TreeChartOptions.AgeVisible = GetControl<ICheckBox>("chkShowAge").Checked;

            fOptions.TreeChartOptions.MaleColor = GetControl<ILabel>("lblMaleColor").BackColor;
            fOptions.TreeChartOptions.FemaleColor = GetControl<ILabel>("lblFemaleColor").BackColor;
            fOptions.TreeChartOptions.UnkSexColor = GetControl<ILabel>("lblUnkSexColor").BackColor;
            fOptions.TreeChartOptions.UnHusbandColor = GetControl<ILabel>("lblUnHusbandColor").BackColor;
            fOptions.TreeChartOptions.UnWifeColor = GetControl<ILabel>("lblUnWifeColor").BackColor;

            fOptions.TreeChartOptions.Margins = (int)GetControl<INumericBox>("numMargins").Value;
            fOptions.TreeChartOptions.BranchDistance = (int)GetControl<INumericBox>("numBranchDist").Value;
            fOptions.TreeChartOptions.LevelDistance = (int)GetControl<INumericBox>("numGenDist").Value;
            fOptions.TreeChartOptions.SpouseDistance = (int)GetControl<INumericBox>("numSpouseDist").Value;

            fOptions.TreeChartOptions.SeparateDepth = GetControl<ICheckBox>("chkSeparateDepth").Checked;
            fOptions.TreeChartOptions.DepthLimit = (int)GetControl<INumericBox>("numDefaultDepth").Value;
            fOptions.TreeChartOptions.DepthLimitAncestors = (int)GetControl<INumericBox>("numDefaultDepthAncestors").Value;
            fOptions.TreeChartOptions.DepthLimitDescendants = (int)GetControl<INumericBox>("numDefaultDepthDescendants").Value;
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

            fOptions.IndividualListColumns.CopyTo(fTempColumns);
            UpdateColumnsList();

            // pedigrees
            UpdatePedigreesOptions();

            // plugins
            UpdatePlugins();
        }

        public void AcceptChanges()
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

            // plugins
            AcceptPlugins();
        }
    }
}
