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
        private readonly IListColumns fTempColumns;


        public GlobalOptions Options
        {
            get { return fOptions; }
        }


        public OptionsDlgController(IOptionsDlg view) : base(view)
        {
            fOptions = GlobalOptions.Instance;
            fTempColumns = IndividualListModel.CreateIndividualListColumns();

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

        public void SelectLabColor(ILabel lbl)
        {
            if (lbl != null) {
                lbl.BackColor = AppHost.StdDialogs.SelectColor(lbl.BackColor);
            }
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

            // plugins
            AcceptPlugins();

            return true;
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LSID_MIOptions);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.LSID_DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.LSID_DlgCancel);

            // Common
            GetControl<ITabPage>("pageCommon").Text = LangMan.LS(LSID.LSID_Common);

            GetControl<IGroupBox>("grpInternet").Text = LangMan.LS(LSID.LSID_Internet);
            GetControl<ICheckBox>("chkUseProxy").Text = LangMan.LS(LSID.LSID_ProxyUse);
            GetControl<ILabel>("lblProxyServer").Text = LangMan.LS(LSID.LSID_ProxyServer);
            GetControl<ILabel>("lblProxyPort").Text = LangMan.LS(LSID.LSID_ProxyPort);
            GetControl<ILabel>("lblProxyLogin").Text = LangMan.LS(LSID.LSID_ProxyLogin);
            GetControl<ILabel>("lblProxyPassword").Text = LangMan.LS(LSID.LSID_Password);

            GetControl<IGroupBox>("grpFileBackup").Text = LangMan.LS(LSID.LSID_FileBackup);
            GetControl<IRadioButton>("radFBNone").Text = LangMan.LS(LSID.LSID_Not);
            GetControl<IRadioButton>("radFBOnlyPrev").Text = LangMan.LS(LSID.LSID_BackupOnlyPrev);
            GetControl<IRadioButton>("radFBEachRevision").Text = LangMan.LS(LSID.LSID_BackupEachRevision);

            GetControl<ICheckBox>("chkAutosave").Text = LangMan.LS(LSID.LSID_Autosave);
            GetControl<ILabel>("lblMinutes").Text = LangMan.LS(LSID.LSID_Minutes);
            GetControl<ILabel>("lblBackupRevisionsMaxCount").Text = LangMan.LS(LSID.LSID_BackupRevisionsMaxCount);

            GetControl<IGroupBox>("grpOther").Text = LangMan.LS(LSID.LSID_Other);
            GetControl<ICheckBox>("chkShowOnStart").Text = LangMan.LS(LSID.LSID_StartupTips);
            GetControl<ICheckBox>("chkLoadRecentFiles").Text = LangMan.LS(LSID.LSID_LoadRecentFiles);
            GetControl<ICheckBox>("chkAutoCheckUpdates").Text = LangMan.LS(LSID.LSID_AutoCheckUpdates);
            GetControl<ICheckBox>("chkCharsetDetection").Text = LangMan.LS(LSID.LSID_CharsetDetection);
            GetControl<ICheckBox>("chkDialogClosingWarn").Text = LangMan.LS(LSID.LSID_WarnForClosingDialog);

            GetControl<ILabel>("lblLanguage").Text = LangMan.LS(LSID.LSID_Language);
            GetControl<ILabel>("lblGeocoder").Text = LangMan.LS(LSID.LSID_Geocoder);
            GetControl<ILabel>("lblGeoSearchCountry").Text = LangMan.LS(LSID.LSID_GeoSearchCountryRestriction);

            // Multimedia
            GetControl<ITabPage>("pageMultimedia").Text = LangMan.LS(LSID.LSID_RPMultimedia);

            GetControl<ICheckBox>("chkRemovableMediaWarning").Text = LangMan.LS(LSID.LSID_RemovableMediaWarningOption);
            GetControl<ICheckBox>("chkEmbeddedMediaPlayer").Text = LangMan.LS(LSID.LSID_EmbeddedMediaPlayer);

            GetControl<ICheckBox>("chkAllowMediaDirectRefs").Text = LangMan.LS(LSID.LSID_AllowMediaDirectReferences);
            GetControl<ICheckBox>("chkAllowMediaStoreRelativeReferences").Text = LangMan.LS(LSID.LSID_AllowMediaRelativeReferences);

            GetControl<ILabel>("lblMediaStoreDefault").Text = LangMan.LS(LSID.LSID_MediaStoreDefault);

            var combo = GetControl<IComboBox>("cmbMediaStoreDefault");
            combo.Clear();
            for (MediaStoreType mst = MediaStoreType.mstReference; mst <= MediaStoreType.mstURL; mst++) {
                combo.AddItem(LangMan.LS(GKData.GKStoreTypes[(int)mst].Name), mst);
            }

            GetControl<ICheckBox>("chkAllowDeleteMediaFileFromStgArc").Text = LangMan.LS(LSID.LSID_AllowDeleteMediaFileFromStgArc);
            GetControl<ICheckBox>("chkAllowDeleteMediaFileFromRefs").Text = LangMan.LS(LSID.LSID_AllowDeleteMediaFileFromRefs);
            GetControl<ICheckBox>("chkDeleteMediaFileWithoutConfirm").Text = LangMan.LS(LSID.LSID_DeleteMediaFileWithoutConfirm);

            // Charts
            GetControl<ITabPage>("pageCharts").Text = LangMan.LS(LSID.LSID_Charts);

            GetControl<ITabPage>("pageTreeChart").Text = LangMan.LS(LSID.LSID_Trees);
            GetControl<IGroupBox>("grpTreePersons").Text = LangMan.LS(LSID.LSID_ViewTree);

            GetControl<ICheckBox>("chkSurname").Text = LangMan.LS(LSID.LSID_Surname);
            GetControl<ICheckBox>("chkName").Text = LangMan.LS(LSID.LSID_Name);
            GetControl<ICheckBox>("chkPatronymic").Text = LangMan.LS(LSID.LSID_Patronymic);
            GetControl<ICheckBox>("chkDiffLines").Text = LangMan.LS(LSID.LSID_DiffLines);
            GetControl<ICheckBox>("chkBirthDate").Text = LangMan.LS(LSID.LSID_BirthDate);
            GetControl<ICheckBox>("chkDeathDate").Text = LangMan.LS(LSID.LSID_DeathDate);
            GetControl<ICheckBox>("chkOnlyYears").Text = LangMan.LS(LSID.LSID_OnlyYears);
            GetControl<ICheckBox>("chkMarriagesDates").Text = LangMan.LS(LSID.LSID_MarriagesDates);
            GetControl<ICheckBox>("chkKinship").Text = LangMan.LS(LSID.LSID_Kinship);
            GetControl<ICheckBox>("chkSignsVisible").Text = LangMan.LS(LSID.LSID_SignsVisible);
            GetControl<ICheckBox>("chkTreeDecorative").Text = LangMan.LS(LSID.LSID_TreeDecorative);
            GetControl<ICheckBox>("chkPortraitsVisible").Text = LangMan.LS(LSID.LSID_PortraitsVisible);
            GetControl<ICheckBox>("chkDefaultPortraits").Text = LangMan.LS(LSID.LSID_DefaultPortraits);
            GetControl<ICheckBox>("chkInvertedTree").Text = LangMan.LS(LSID.LSID_InvertedTree);
            GetControl<ICheckBox>("chkChildlessExclude").Text = LangMan.LS(LSID.LSID_ChildlessExclude);
            GetControl<ICheckBox>("chkShowPlaces").Text = LangMan.LS(LSID.LSID_ShowPlaces);
            GetControl<ICheckBox>("chkSeparateDAPLines").Text = LangMan.LS(LSID.LSID_SeparateDatesAndPlacesLines);
            GetControl<ICheckBox>("chkOnlyLocality").Text = LangMan.LS(LSID.LSID_OnlyLocality);
            GetControl<ICheckBox>("chkHideUnknownSpouses").Text = LangMan.LS(LSID.LSID_HideUnknownSpouses);
            GetControl<ICheckBox>("chkCheckTreeSize").Text = LangMan.LS(LSID.LSID_CheckTreeSize);
            GetControl<ICheckBox>("chkDottedLinesOfAdoptedChildren").Text = LangMan.LS(LSID.LSID_DottedLinesOfAdoptedChildren);
            GetControl<ICheckBox>("chkBoldNames").Text = LangMan.LS(LSID.LSID_BoldNames);
            GetControl<ICheckBox>("chkMinimizingWidth").Text = LangMan.LS(LSID.LSID_MinimizingWidth);
            GetControl<ICheckBox>("chkShowAge").Text = LangMan.LS(LSID.LSID_ShowAge);

            GetControl<IGroupBox>("grpTreeDecor").Text = LangMan.LS(LSID.LSID_Decor);
            GetControl<ILabel>("lblMaleColor").Text = LangMan.LS(LSID.LSID_Man);
            GetControl<ILabel>("lblFemaleColor").Text = LangMan.LS(LSID.LSID_Woman);
            GetControl<ILabel>("lblUnkSexColor").Text = LangMan.LS(LSID.LSID_UnkSex);
            GetControl<ILabel>("lblUnHusbandColor").Text = LangMan.LS(LSID.LSID_UnHusband);
            GetControl<ILabel>("lblUnWifeColor").Text = LangMan.LS(LSID.LSID_UnWife);
            //lblFont.Text = LangMan.LS(LSID.LSID_Font);

            GetControl<IGroupBox>("grpSpacings").Text = LangMan.LS(LSID.LSID_Spacings);
            GetControl<ILabel>("lblMargins").Text = LangMan.LS(LSID.LSID_Margins);
            GetControl<ILabel>("lblBranchDist").Text = LangMan.LS(LSID.LSID_BranchDist);
            GetControl<ILabel>("lblGenDist").Text = LangMan.LS(LSID.LSID_GenDist);
            GetControl<ILabel>("lblSpouseDist").Text = LangMan.LS(LSID.LSID_SpouseDist);

            GetControl<ICheckBox>("chkSeparateDepth").Text = LangMan.LS(LSID.LSID_SeparateDepth);
            GetControl<ILabel>("lblDefaultDepth").Text = LangMan.LS(LSID.LSID_DefaultDepth);
            GetControl<ILabel>("lblDefaultDepthAncestors").Text = LangMan.LS(LSID.LSID_DefaultDepth) + ": " + LangMan.LS(LSID.LSID_Ancestors);
            GetControl<ILabel>("lblDefaultDepthDescendants").Text = LangMan.LS(LSID.LSID_DefaultDepth) + ": " + LangMan.LS(LSID.LSID_Descendants);

            GetControl<ITabPage>("pageAncCircle").Text = LangMan.LS(LSID.LSID_AncestorsCircle);

            // UIView
            GetControl<ITabPage>("pageUIView").Text = LangMan.LS(LSID.LSID_Interface);

            GetControl<ITabPage>("pageViewCommon").Text = LangMan.LS(LSID.LSID_ListsAll);

            GetControl<IGroupBox>("rgFNPFormat").Text = LangMan.LS(LSID.LSID_NamesFormat);
            GetControl<IRadioButton>("radSNP").Text = LangMan.LS(LSID.LSID_NF1);
            GetControl<IRadioButton>("radS_NP").Text = LangMan.LS(LSID.LSID_NF2);
            GetControl<IRadioButton>("radS_N_P").Text = LangMan.LS(LSID.LSID_NF3);

            GetControl<ICheckBox>("chkPlacesWithAddress").Text = LangMan.LS(LSID.LSID_PlacesWithAddress);
            GetControl<ICheckBox>("chkHighlightUnparented").Text = LangMan.LS(LSID.LSID_HighlightUnparented);
            GetControl<ICheckBox>("chkHighlightUnmarried").Text = LangMan.LS(LSID.LSID_HighlightUnmarried);

            GetControl<ICheckBox>("chkAutoSortChildren").Text = LangMan.LS(LSID.LSID_AutoSortChildren);
            GetControl<ICheckBox>("chkAutoSortSpouses").Text = LangMan.LS(LSID.LSID_AutoSortSpouses);
            GetControl<ICheckBox>("chkFirstCapitalLetterInNames").Text = LangMan.LS(LSID.LSID_FirstCapitalLetterInNames);
            GetControl<ICheckBox>("chkShortKinshipForm").Text = LangMan.LS(LSID.LSID_ShortKinshipForm);
            GetControl<ICheckBox>("chkSurnameFirstInOrder").Text = LangMan.LS(LSID.LSID_SurnameFirstInOrder);
            GetControl<ICheckBox>("chkSurnameInCapitals").Text = LangMan.LS(LSID.LSID_SurnameInCapitals);

            GetControl<IGroupBox>("grpDateFormat").Text = LangMan.LS(LSID.LSID_DateFormat);
            GetControl<ICheckBox>("chkShowDatesCalendar").Text = LangMan.LS(LSID.LSID_ShowDatesCalendar);
            GetControl<ICheckBox>("chkShowDatesSigns").Text = LangMan.LS(LSID.LSID_ShowDatesSigns);

            GetControl<IGroupBox>("grpAdvancedNames").Text = LangMan.LS(LSID.LSID_AdditionalNames);
            GetControl<ICheckBox>("chkExtendWomanSurnames").Text = LangMan.LS(LSID.LSID_ExtendedWomanSurnames);
            GetControl<IRadioButton>("radMaiden_Married").Text = LangMan.LS(LSID.LSID_WSF_Maiden_Married);
            GetControl<IRadioButton>("radMarried_Maiden").Text = LangMan.LS(LSID.LSID_WSF_Married_Maiden);
            GetControl<IRadioButton>("radMaiden").Text = LangMan.LS(LSID.LSID_WSF_Maiden);
            GetControl<IRadioButton>("radMarried").Text = LangMan.LS(LSID.LSID_WSF_Married);

            GetControl<ITabPage>("pageViewPersons").Text = LangMan.LS(LSID.LSID_ListPersons);
            GetControl<IButton>("btnDefList").Text = LangMan.LS(LSID.LSID_DefList);

            // Pedigree
            GetControl<ITabPage>("pagePedigree").Text = LangMan.LS(LSID.LSID_Pedigrees);

            GetControl<IGroupBox>("grpPedigree").Text = LangMan.LS(LSID.LSID_PedigreeGen);
            GetControl<ICheckBox>("chkAttributes").Text = LangMan.LS(LSID.LSID_IncludeAttributes);
            GetControl<ICheckBox>("chkNotes").Text = LangMan.LS(LSID.LSID_IncludeNotes);
            GetControl<ICheckBox>("chkSources").Text = LangMan.LS(LSID.LSID_IncludeSources);
            GetControl<ICheckBox>("chkGenerations").Text = LangMan.LS(LSID.LSID_IncludeGenerations);

            GetControl<IGroupBox>("grpPedigreeFormat").Text = LangMan.LS(LSID.LSID_PedigreeFormat);
            GetControl<IRadioButton>("radExcess").Text = LangMan.LS(LSID.LSID_PF1);
            GetControl<IRadioButton>("radCompact").Text = LangMan.LS(LSID.LSID_PF2);

            // Plugins
            GetControl<ITabPage>("pagePlugins").Text = LangMan.LS(LSID.LSID_Plugins);
        }
    }
}
