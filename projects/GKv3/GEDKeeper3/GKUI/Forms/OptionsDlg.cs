/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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

using System;
using System.Globalization;
using BSLib.Design.Graphics;
using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Views;
using GKCore.Options;
using GKCore.Plugins;
using GKCore.Types;
using GKUI.Components;
using GKUI.Platform;

namespace GKUI.Forms
{
    public sealed partial class OptionsDlg : CommonDialog, ILocalizable, IOptionsDlg
    {
        private GlobalOptions fOptions;
        private readonly ListColumns fTempColumns;

        public GlobalOptions Options
        {
            get { return fOptions; }
            set { fOptions = value; }
        }

        public OptionsDlg(IHost host)
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
            btnColumnUp.Image = UIHelper.LoadResourceImage("Resources.btn_up.gif");
            btnColumnDown.Image = UIHelper.LoadResourceImage("Resources.btn_down.gif");

            fOptions = GlobalOptions.Instance;
            fTempColumns = IndividualListMan.CreateIndividualListColumns();

            cmbGeoSearchCountry.Items.Clear();
            foreach (var ci in GKUtils.GetCountries()) {
                cmbGeoSearchCountry.Items.Add(ci);
            }

            lstPersonColumns.AddCheckedColumn("x", 75);
            lstPersonColumns.AddColumn("Title", 100);

            lvPlugins.AddColumn("Title", 75);
            lvPlugins.AddColumn("Version", 100);
            lvPlugins.AddColumn("Copyright", 125);
            lvPlugins.AddColumn("Description", 250);

            SetLocale();
            UpdateForm();
        }

        private void UpdateColumnsList()
        {
            lstPersonColumns.ItemCheck -= ListPersonColumns_ItemCheck;
            lstPersonColumns.BeginUpdate();
            try {
                lstPersonColumns.ClearItems();

                int num = fTempColumns.Count;
                for (int i = 0; i < num; i++) {
                    ListColumn column = fTempColumns.OrderedColumns[i];

                    lstPersonColumns.AddItem(null, column.CurActive, LangMan.LS(column.ColName));
                }
            } finally {
                lstPersonColumns.EndUpdate();
            }
            lstPersonColumns.ItemCheck += ListPersonColumns_ItemCheck;
        }

        private void UpdateProxyOptions()
        {
            chkUseProxy.Checked = fOptions.Proxy.UseProxy;
            txtProxyServer.Text = fOptions.Proxy.Server;
            txtProxyPort.Text = fOptions.Proxy.Port;
            txtProxyLogin.Text = fOptions.Proxy.Login;
            txtProxyPass.Text = fOptions.Proxy.Password;
        }

        private void UpdateBackupOptions()
        {
            switch (fOptions.FileBackup) {
                case FileBackup.fbNone:
                    radFBNone.Checked = true;
                    break;
                case FileBackup.fbOnlyPrev:
                    radFBOnlyPrev.Checked = true;
                    break;
                case FileBackup.fbEachRevision:
                    radFBEachRevision.Checked = true;
                    break;
            }
            chkAutosave.Checked = fOptions.Autosave;
            numASMin.Value = fOptions.AutosaveInterval;
            numBackupRevisionsMaxCount.Value = fOptions.FileBackupEachRevisionMaxCount;
        }

        private void UpdateOtherOptions()
        {
            chkShowOnStart.Checked = fOptions.ShowTips;
            chkLoadRecentFiles.Checked = fOptions.LoadRecentFiles;
            chkAutoCheckUpdates.Checked = fOptions.AutoCheckUpdates;
            chkCharsetDetection.Checked = fOptions.CharsetDetection;
            chkDialogClosingWarn.Checked = fOptions.DialogClosingWarn;
        }

        private void UpdateLangs()
        {
            cmbLanguages.Items.Clear();
            cmbLanguages.Items.Add(new GKComboItem<int>(LangMan.LS_DEF_NAME, LangMan.LS_DEF_CODE));
            foreach (LangRecord lngRec in GlobalOptions.Instance.Languages) {
                if (lngRec.Code != LangMan.LS_DEF_CODE) {
                    cmbLanguages.Items.Add(new GKComboItem<int>(lngRec.Name, lngRec.Code));
                }
            }
            UIHelper.SetSelectedTag(cmbLanguages, fOptions.InterfaceLang, true);
        }

        private void UpdateMediaOptions()
        {
            chkRemovableMediaWarning.Checked = fOptions.RemovableMediaWarning;
            chkEmbeddedMediaPlayer.Checked = fOptions.EmbeddedMediaPlayer;
            chkAllowMediaDirectRefs.Checked = fOptions.AllowMediaStoreReferences;
            chkAllowMediaStoreRelativeReferences.Checked = fOptions.AllowMediaStoreRelativeReferences;
            UIHelper.SetSelectedTag<MediaStoreType>(cmbMediaStoreDefault, fOptions.MediaStoreDefault);
            chkAllowDeleteMediaFileFromStgArc.Checked = fOptions.AllowDeleteMediaFileFromStgArc;
            chkAllowDeleteMediaFileFromRefs.Checked = fOptions.AllowDeleteMediaFileFromRefs;
            chkDeleteMediaFileWithoutConfirm.Checked = fOptions.DeleteMediaFileWithoutConfirm;
        }

        private void UpdateTreeChartsOptions()
        {
            chkSurname.Checked = fOptions.TreeChartOptions.FamilyVisible;
            chkName.Checked = fOptions.TreeChartOptions.NameVisible;
            chkPatronymic.Checked = fOptions.TreeChartOptions.PatronymicVisible;
            chkDiffLines.Checked = fOptions.TreeChartOptions.DiffLines;
            chkBirthDate.Checked = fOptions.TreeChartOptions.BirthDateVisible;
            chkDeathDate.Checked = fOptions.TreeChartOptions.DeathDateVisible;
            chkOnlyYears.Checked = fOptions.TreeChartOptions.OnlyYears;
            chkMarriagesDates.Checked = fOptions.TreeChartOptions.MarriagesDates;
            chkKinship.Checked = fOptions.TreeChartOptions.Kinship;
            chkSignsVisible.Checked = fOptions.TreeChartOptions.SignsVisible;
            chkTreeDecorative.Checked = fOptions.TreeChartOptions.Decorative;
            chkPortraitsVisible.Checked = fOptions.TreeChartOptions.PortraitsVisible;
            chkDefaultPortraits.Checked = fOptions.TreeChartOptions.DefaultPortraits;
            chkInvertedTree.Checked = fOptions.TreeChartOptions.InvertedTree;
            chkChildlessExclude.Checked = fOptions.TreeChartOptions.ChildlessExclude;
            chkShowPlaces.Checked = fOptions.TreeChartOptions.ShowPlaces;
            chkHideUnknownSpouses.Checked = fOptions.TreeChartOptions.HideUnknownSpouses;
            chkCheckTreeSize.Checked = fOptions.CheckTreeSize;
            chkDottedLinesOfAdoptedChildren.Checked = fOptions.TreeChartOptions.DottedLinesOfAdoptedChildren;
            chkSeparateDAPLines.Checked = fOptions.TreeChartOptions.SeparateDatesAndPlacesLines;
            chkBoldNames.Checked = fOptions.TreeChartOptions.BoldNames;

            lblMaleColor.BackgroundColor = UIHelper.ConvertColor(fOptions.TreeChartOptions.MaleColor);
            lblFemaleColor.BackgroundColor = UIHelper.ConvertColor(fOptions.TreeChartOptions.FemaleColor);
            lblUnkSexColor.BackgroundColor = UIHelper.ConvertColor(fOptions.TreeChartOptions.UnkSexColor);
            lblUnHusbandColor.BackgroundColor = UIHelper.ConvertColor(fOptions.TreeChartOptions.UnHusbandColor);
            lblUnWifeColor.BackgroundColor = UIHelper.ConvertColor(fOptions.TreeChartOptions.UnWifeColor);

            numMargins.Value = fOptions.TreeChartOptions.Margins;
            numBranchDist.Value = fOptions.TreeChartOptions.BranchDistance;
            numGenDist.Value = fOptions.TreeChartOptions.LevelDistance;
            numSpouseDist.Value = fOptions.TreeChartOptions.SpouseDistance;

            chkSeparateDepth.Checked = fOptions.TreeChartOptions.SeparateDepth;
            chkSeparateDepth_CheckedChanged(null, null);

            numDefaultDepth.Value = fOptions.TreeChartOptions.DepthLimit;
            numDefaultDepthAncestors.Value = fOptions.TreeChartOptions.DepthLimitAncestors;
            numDefaultDepthDescendants.Value = fOptions.TreeChartOptions.DepthLimitDescendants;

            UpdateTreeChartFont();
        }

        private void UpdateTreeChartFont()
        {
            lblChartFont.Text = fOptions.TreeChartOptions.DefFontName + @", " + fOptions.TreeChartOptions.DefFontSize.ToString();
        }

        private void UpdateCircleChartsOptions()
        {
            ancOptionsControl1.Options = fOptions.CircleChartOptions;
            ancOptionsControl1.UpdateControls();
        }

        private void UpdateInterfaceOptions()
        {
            switch (fOptions.DefNameFormat) {
                case NameFormat.nfFNP:
                    radSNP.Checked = true;
                    break;
                case NameFormat.nfF_NP:
                    radS_NP.Checked = true;
                    break;
                case NameFormat.nfF_N_P:
                    radS_N_P.Checked = true;
                    break;
            }

            switch (fOptions.DefDateFormat) {
                case DateFormat.dfDD_MM_YYYY:
                    radDMY.Checked = true;
                    break;
                case DateFormat.dfYYYY_MM_DD:
                    radYMD.Checked = true;
                    break;
            }
            chkShowDatesCalendar.Checked = fOptions.ShowDatesCalendar;
            chkShowDatesSigns.Checked = fOptions.ShowDatesSign;

            chkPlacesWithAddress.Checked = fOptions.PlacesWithAddress;
            chkHighlightUnparented.Checked = fOptions.ListHighlightUnparentedPersons;
            chkHighlightUnmarried.Checked = fOptions.ListHighlightUnmarriedPersons;

            chkAutoSortChildren.Checked = fOptions.AutoSortChildren;
            chkAutoSortSpouses.Checked = fOptions.AutoSortSpouses;
            chkFirstCapitalLetterInNames.Checked = fOptions.FirstCapitalLetterInNames;
        }

        private void UpdateWomanSurnameFormat()
        {
            WomanSurnameFormat wsFmt = fOptions.WomanSurnameFormat;
            bool isExtend = wsFmt != WomanSurnameFormat.wsfNotExtend;

            chkExtendWomanSurnames.Checked = isExtend;
            radMaiden_Married.Enabled = isExtend;
            radMarried_Maiden.Enabled = isExtend;
            radMaiden.Enabled = isExtend;
            radMarried.Enabled = isExtend;

            switch (wsFmt) {
                case WomanSurnameFormat.wsfMaiden_Married:
                    radMaiden_Married.Checked = true;
                    break;

                case WomanSurnameFormat.wsfMarried_Maiden:
                    radMarried_Maiden.Checked = true;
                    break;

                case WomanSurnameFormat.wsfMaiden:
                    radMaiden.Checked = true;
                    break;

                case WomanSurnameFormat.wsfMarried:
                    radMarried.Checked = true;
                    break;
            }
        }

        private void UpdatePedigreesOptions()
        {
            chkAttributes.Checked = fOptions.PedigreeOptions.IncludeAttributes;
            chkNotes.Checked = fOptions.PedigreeOptions.IncludeNotes;
            chkSources.Checked = fOptions.PedigreeOptions.IncludeSources;
            chkGenerations.Checked = fOptions.PedigreeOptions.IncludeGenerations;

            switch (fOptions.PedigreeOptions.Format) {
                case PedigreeFormat.Excess:
                    radExcess.Checked = true;
                    break;
                case PedigreeFormat.Compact:
                    radCompact.Checked = true;
                    break;
            }
        }

        private void UpdatePlugins()
        {
            lvPlugins.ClearItems();

            int num = AppHost.Plugins.Count;
            for (int i = 0; i < num; i++) {
                IPlugin plugin = AppHost.Plugins[i];
                PluginInfo pInfo = PluginInfo.GetPluginInfo(plugin);

                lvPlugins.AddItem(null, pInfo.Title,
                    pInfo.Version,
                    pInfo.Copyright,
                    pInfo.Description);
            }
        }

        private void UpdateForm()
        {
            // common
            UpdateProxyOptions();
            UpdateBackupOptions();
            UpdateOtherOptions();
            UpdateLangs();

            cmbGeocoder.Text = fOptions.Geocoder;
            cmbGeoSearchCountry.Text = fOptions.GeoSearchCountry;

            // media
            UpdateMediaOptions();

            // charts
            UpdateTreeChartsOptions();
            UpdateCircleChartsOptions();

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

        private void chkExtendWomanSurnames_CheckedChanged(object sender, EventArgs e)
        {
            if (!chkExtendWomanSurnames.Checked.GetValueOrDefault()) {
                fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfNotExtend;
                UpdateWomanSurnameFormat();
            } else {
                radMaiden_Married.Enabled = true;
                radMarried_Maiden.Enabled = true;
                radMaiden.Enabled = true;
                radMarried.Enabled = true;
            }
        }

        private void PanColor_Click(object sender, EventArgs e)
        {
            Label pan = (sender as Label);
            if (pan == null) return;

            pan.BackgroundColor = UIHelper.ConvertColor(AppHost.StdDialogs.SelectColor(UIHelper.ConvertColor(pan.BackgroundColor)));
        }

        private void panDefFont_Click(object sender, EventArgs e)
        {
            TreeChartOptions chartOptions = fOptions.TreeChartOptions;

            var sdFont = new Font(chartOptions.DefFontName, chartOptions.DefFontSize);
            IFont font = new FontHandler(sdFont);
            font = AppHost.StdDialogs.SelectFont(font);
            if (font != null) {
                chartOptions.DefFontName = font.Name;
                chartOptions.DefFontSize = (int)(Math.Round(font.Size));
            }

            UpdateTreeChartFont();
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try {
                AcceptChanges();
                DialogResult = DialogResult.Ok;
            } catch (Exception ex) {
                Logger.WriteError("OptionsDlg.btnAccept_Click()", ex);
                DialogResult = DialogResult.None;
            }
        }

        private void AcceptProxyOptions()
        {
            fOptions.Proxy.UseProxy = chkUseProxy.Checked.GetValueOrDefault();
            fOptions.Proxy.Server = txtProxyServer.Text;
            fOptions.Proxy.Port = txtProxyPort.Text;
            fOptions.Proxy.Login = txtProxyLogin.Text;
            fOptions.Proxy.Password = txtProxyPass.Text;
        }

        private void AcceptBackupOptions()
        {
            if (radFBNone.Checked) {
                fOptions.FileBackup = FileBackup.fbNone;
            } else if (radFBOnlyPrev.Checked) {
                fOptions.FileBackup = FileBackup.fbOnlyPrev;
            } else if (radFBEachRevision.Checked) {
                fOptions.FileBackup = FileBackup.fbEachRevision;
            }

            fOptions.Autosave = chkAutosave.Checked.GetValueOrDefault();
            fOptions.AutosaveInterval = (int)numASMin.Value;
            fOptions.FileBackupEachRevisionMaxCount = (int)numBackupRevisionsMaxCount.Value;
        }

        private void AcceptOtherOptions()
        {
            fOptions.ShowTips = chkShowOnStart.Checked.GetValueOrDefault();
            fOptions.LoadRecentFiles = chkLoadRecentFiles.Checked.GetValueOrDefault();
            fOptions.AutoCheckUpdates = chkAutoCheckUpdates.Checked.GetValueOrDefault();
            fOptions.CharsetDetection = chkCharsetDetection.Checked.GetValueOrDefault();
            fOptions.DialogClosingWarn = chkDialogClosingWarn.Checked.GetValueOrDefault();
        }

        private void AcceptLangs()
        {
            var item = cmbLanguages.Items[cmbLanguages.SelectedIndex] as GKComboItem<int>;
            if (item != null) {
                AppHost.Instance.LoadLanguage((int)item.Tag);
            }
        }

        private void AcceptMediaOptions()
        {
            fOptions.RemovableMediaWarning = chkRemovableMediaWarning.Checked.GetValueOrDefault();
            fOptions.EmbeddedMediaPlayer = chkEmbeddedMediaPlayer.Checked.GetValueOrDefault();
            fOptions.AllowMediaStoreReferences = chkAllowMediaDirectRefs.Checked.GetValueOrDefault();
            fOptions.AllowMediaStoreRelativeReferences = chkAllowMediaStoreRelativeReferences.Checked.GetValueOrDefault();
            fOptions.MediaStoreDefault = UIHelper.GetSelectedTag<MediaStoreType>(cmbMediaStoreDefault);
            fOptions.AllowDeleteMediaFileFromStgArc = chkAllowDeleteMediaFileFromStgArc.Checked.GetValueOrDefault();
            fOptions.AllowDeleteMediaFileFromRefs = chkAllowDeleteMediaFileFromRefs.Checked.GetValueOrDefault();
            fOptions.DeleteMediaFileWithoutConfirm = chkDeleteMediaFileWithoutConfirm.Checked.GetValueOrDefault();
        }

        private void AcceptTreeChartsOptions()
        {
            fOptions.TreeChartOptions.FamilyVisible = chkSurname.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.NameVisible = chkName.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.PatronymicVisible = chkPatronymic.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.DiffLines = chkDiffLines.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.BirthDateVisible = chkBirthDate.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.DeathDateVisible = chkDeathDate.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.OnlyYears = chkOnlyYears.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.MarriagesDates = chkMarriagesDates.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.Kinship = chkKinship.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.SignsVisible = chkSignsVisible.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.Decorative = chkTreeDecorative.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.PortraitsVisible = chkPortraitsVisible.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.DefaultPortraits = chkDefaultPortraits.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.InvertedTree = chkInvertedTree.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.ChildlessExclude = chkChildlessExclude.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.ShowPlaces = chkShowPlaces.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.HideUnknownSpouses = chkHideUnknownSpouses.Checked.GetValueOrDefault();
            fOptions.CheckTreeSize = chkCheckTreeSize.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.DottedLinesOfAdoptedChildren = chkDottedLinesOfAdoptedChildren.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.SeparateDatesAndPlacesLines = chkSeparateDAPLines.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.BoldNames = chkBoldNames.Checked.GetValueOrDefault();

            fOptions.TreeChartOptions.MaleColor = UIHelper.ConvertColor(lblMaleColor.BackgroundColor);
            fOptions.TreeChartOptions.FemaleColor = UIHelper.ConvertColor(lblFemaleColor.BackgroundColor);
            fOptions.TreeChartOptions.UnkSexColor = UIHelper.ConvertColor(lblUnkSexColor.BackgroundColor);
            fOptions.TreeChartOptions.UnHusbandColor = UIHelper.ConvertColor(lblUnHusbandColor.BackgroundColor);
            fOptions.TreeChartOptions.UnWifeColor = UIHelper.ConvertColor(lblUnWifeColor.BackgroundColor);

            fOptions.TreeChartOptions.Margins = (int)numMargins.Value;
            fOptions.TreeChartOptions.BranchDistance = (int)numBranchDist.Value;
            fOptions.TreeChartOptions.LevelDistance = (int)numGenDist.Value;
            fOptions.TreeChartOptions.SpouseDistance = (int)numSpouseDist.Value;

            fOptions.TreeChartOptions.SeparateDepth = chkSeparateDepth.Checked.GetValueOrDefault();
            fOptions.TreeChartOptions.DepthLimit = (int)numDefaultDepth.Value;
            fOptions.TreeChartOptions.DepthLimitAncestors = (int)numDefaultDepthAncestors.Value;
            fOptions.TreeChartOptions.DepthLimitDescendants = (int)numDefaultDepthDescendants.Value;
        }

        private void AcceptCircleChartsOptions()
        {
            ancOptionsControl1.AcceptChanges();
        }

        private void AcceptInterfaceOptions()
        {
            if (radSNP.Checked) {
                fOptions.DefNameFormat = NameFormat.nfFNP;
            } else if (radS_NP.Checked) {
                fOptions.DefNameFormat = NameFormat.nfF_NP;
            } else if (radS_N_P.Checked) {
                fOptions.DefNameFormat = NameFormat.nfF_N_P;
            }

            if (radDMY.Checked) {
                fOptions.DefDateFormat = DateFormat.dfDD_MM_YYYY;
            } else if (radYMD.Checked) {
                fOptions.DefDateFormat = DateFormat.dfYYYY_MM_DD;
            }
            fOptions.ShowDatesCalendar = chkShowDatesCalendar.Checked.GetValueOrDefault();
            fOptions.ShowDatesSign = chkShowDatesSigns.Checked.GetValueOrDefault();

            fOptions.PlacesWithAddress = chkPlacesWithAddress.Checked.GetValueOrDefault();
            fOptions.ListHighlightUnparentedPersons = chkHighlightUnparented.Checked.GetValueOrDefault();
            fOptions.ListHighlightUnmarriedPersons = chkHighlightUnmarried.Checked.GetValueOrDefault();

            fOptions.AutoSortChildren = chkAutoSortChildren.Checked.GetValueOrDefault();
            fOptions.AutoSortSpouses = chkAutoSortSpouses.Checked.GetValueOrDefault();
            fOptions.FirstCapitalLetterInNames = chkFirstCapitalLetterInNames.Checked.GetValueOrDefault();
        }

        private void AcceptWomanSurnameFormat()
        {
            if (!chkExtendWomanSurnames.Checked.GetValueOrDefault()) {
                fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfNotExtend;
            } else {
                if (radMaiden_Married.Checked) {
                    fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfMaiden_Married;
                } else if (radMarried_Maiden.Checked) {
                    fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfMarried_Maiden;
                } else if (radMaiden.Checked) {
                    fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfMaiden;
                } else if (radMarried.Checked) {
                    fOptions.WomanSurnameFormat = WomanSurnameFormat.wsfMarried;
                }
            }
        }

        private void AcceptColumnsList()
        {
            fTempColumns.CopyTo(fOptions.IndividualListColumns);
        }

        private void AcceptPedigreesOptions()
        {
            fOptions.PedigreeOptions.IncludeAttributes = chkAttributes.Checked.GetValueOrDefault();
            fOptions.PedigreeOptions.IncludeNotes = chkNotes.Checked.GetValueOrDefault();
            fOptions.PedigreeOptions.IncludeSources = chkSources.Checked.GetValueOrDefault();
            fOptions.PedigreeOptions.IncludeGenerations = chkGenerations.Checked.GetValueOrDefault();

            if (radExcess.Checked) {
                fOptions.PedigreeOptions.Format = PedigreeFormat.Excess;
            } else if (radCompact.Checked) {
                fOptions.PedigreeOptions.Format = PedigreeFormat.Compact;
            }
        }

        private void AcceptPlugins()
        {
            // dummy for future
        }

        private void AcceptChanges()
        {
            // common
            AcceptProxyOptions();
            AcceptBackupOptions();
            AcceptOtherOptions();
            AcceptLangs();

            fOptions.Geocoder = cmbGeocoder.Text;
            fOptions.GeoSearchCountry = cmbGeoSearchCountry.Text;

            // media
            AcceptMediaOptions();

            // charts
            AcceptTreeChartsOptions();
            AcceptCircleChartsOptions();

            // interface
            AcceptInterfaceOptions();
            AcceptWomanSurnameFormat();
            AcceptColumnsList();

            // pedigrees
            AcceptPedigreesOptions();

            // plugins
            AcceptPlugins();
        }

        private void btnColumnUp_Click(object sender, EventArgs e)
        {
            int idx = lstPersonColumns.SelectedIndex;
            if (fTempColumns.MoveColumn(idx, true)) {
                UpdateColumnsList();
                lstPersonColumns.SelectedIndex = idx - 1;
            }
        }

        private void btnColumnDown_Click(object sender, EventArgs e)
        {
            int idx = lstPersonColumns.SelectedIndex;
            if (fTempColumns.MoveColumn(idx, false)) {
                UpdateColumnsList();
                lstPersonColumns.SelectedIndex = idx + 1;
            }
        }

        private void btnDefList_Click(object sender, EventArgs e)
        {
            fTempColumns.ResetDefaults();
            UpdateColumnsList();
        }

        private void ListPersonColumns_ItemCheck(object sender, ItemCheckEventArgs e)
        {
            fTempColumns.OrderedColumns[e.Index].CurActive = e.NewValue;
        }

        public void SetPage(OptionsPage page)
        {
            switch (page) {
                case OptionsPage.opCommon:
                    PageControl1.SelectedPage = pageCommon;
                    break;

                case OptionsPage.opTreeChart:
                    PageControl1.SelectedPage = pageCharts;
                    tabsCharts.SelectedIndex = 0;
                    break;

                case OptionsPage.opCircleChart:
                    PageControl1.SelectedPage = pageCharts;
                    tabsCharts.SelectedIndex = 1;
                    break;

                case OptionsPage.opInterface:
                    PageControl1.SelectedPage = pageUIView;
                    break;

                case OptionsPage.opPedigree:
                    PageControl1.SelectedPage = pagePedigree;
                    break;

                case OptionsPage.opMultimedia:
                    PageControl1.SelectedPage = pageMultimedia;
                    break;
            }
        }

        public void SetLocale()
        {
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Title = LangMan.LS(LSID.LSID_MIOptions);

            // Common
            pageCommon.Text = LangMan.LS(LSID.LSID_Common);

            grpInternet.Text = LangMan.LS(LSID.LSID_Internet);
            chkUseProxy.Text = LangMan.LS(LSID.LSID_ProxyUse);
            lblProxyServer.Text = LangMan.LS(LSID.LSID_ProxyServer);
            lblProxyPort.Text = LangMan.LS(LSID.LSID_ProxyPort);
            lblProxyLogin.Text = LangMan.LS(LSID.LSID_ProxyLogin);
            lblProxyPassword.Text = LangMan.LS(LSID.LSID_Password);

            grpFileBackup.Text = LangMan.LS(LSID.LSID_FileBackup);
            radFBNone.Text = LangMan.LS(LSID.LSID_Not);
            radFBOnlyPrev.Text = LangMan.LS(LSID.LSID_BackupOnlyPrev);
            radFBEachRevision.Text = LangMan.LS(LSID.LSID_BackupEachRevision);

            chkAutosave.Text = LangMan.LS(LSID.LSID_Autosave);
            lblMinutes.Text = LangMan.LS(LSID.LSID_Minutes);
            lblBackupRevisionsMaxCount.Text = LangMan.LS(LSID.LSID_BackupRevisionsMaxCount);

            grpOther.Text = LangMan.LS(LSID.LSID_Other);
            chkShowOnStart.Text = LangMan.LS(LSID.LSID_StartupTips);
            chkLoadRecentFiles.Text = LangMan.LS(LSID.LSID_LoadRecentFiles);
            chkAutoCheckUpdates.Text = LangMan.LS(LSID.LSID_AutoCheckUpdates);
            chkCharsetDetection.Text = LangMan.LS(LSID.LSID_CharsetDetection);
            chkDialogClosingWarn.Text = LangMan.LS(LSID.LSID_WarnForClosingDialog);

            lblLanguage.Text = LangMan.LS(LSID.LSID_Language);
            lblGeocoder.Text = LangMan.LS(LSID.LSID_Geocoder);
            lblGeoSearchCountry.Text = LangMan.LS(LSID.LSID_GeoSearchCountryRestriction);

            // Multimedia
            pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);

            chkRemovableMediaWarning.Text = LangMan.LS(LSID.LSID_RemovableMediaWarningOption);
            chkEmbeddedMediaPlayer.Text = LangMan.LS(LSID.LSID_EmbeddedMediaPlayer);

            chkAllowMediaDirectRefs.Text = LangMan.LS(LSID.LSID_AllowMediaDirectReferences);
            chkAllowMediaStoreRelativeReferences.Text = LangMan.LS(LSID.LSID_AllowMediaRelativeReferences);

            lblMediaStoreDefault.Text = LangMan.LS(LSID.LSID_MediaStoreDefault);
            cmbMediaStoreDefault.Items.Clear();
            for (MediaStoreType mst = MediaStoreType.mstReference; mst <= MediaStoreType.mstURL; mst++) {
                cmbMediaStoreDefault.Items.Add(new GKComboItem<MediaStoreType>(LangMan.LS(GKData.GKStoreTypes[(int)mst].Name), mst));
            }

            chkAllowDeleteMediaFileFromStgArc.Text = LangMan.LS(LSID.LSID_AllowDeleteMediaFileFromStgArc);
            chkAllowDeleteMediaFileFromRefs.Text = LangMan.LS(LSID.LSID_AllowDeleteMediaFileFromRefs);
            chkDeleteMediaFileWithoutConfirm.Text = LangMan.LS(LSID.LSID_DeleteMediaFileWithoutConfirm);

            // Charts
            pageCharts.Text = LangMan.LS(LSID.LSID_Charts);

            pageTreeChart.Text = LangMan.LS(LSID.LSID_Trees);
            grpTreePersons.Text = LangMan.LS(LSID.LSID_ViewTree);

            chkSurname.Text = LangMan.LS(LSID.LSID_Surname);
            chkName.Text = LangMan.LS(LSID.LSID_Name);
            chkPatronymic.Text = LangMan.LS(LSID.LSID_Patronymic);
            chkDiffLines.Text = LangMan.LS(LSID.LSID_DiffLines);
            chkBirthDate.Text = LangMan.LS(LSID.LSID_BirthDate);
            chkDeathDate.Text = LangMan.LS(LSID.LSID_DeathDate);
            chkOnlyYears.Text = LangMan.LS(LSID.LSID_OnlyYears);
            chkMarriagesDates.Text = LangMan.LS(LSID.LSID_MarriagesDates);
            chkKinship.Text = LangMan.LS(LSID.LSID_Kinship);
            chkSignsVisible.Text = LangMan.LS(LSID.LSID_SignsVisible);
            chkTreeDecorative.Text = LangMan.LS(LSID.LSID_TreeDecorative);
            chkPortraitsVisible.Text = LangMan.LS(LSID.LSID_PortraitsVisible);
            chkDefaultPortraits.Text = LangMan.LS(LSID.LSID_DefaultPortraits);
            chkInvertedTree.Text = LangMan.LS(LSID.LSID_InvertedTree);
            chkChildlessExclude.Text = LangMan.LS(LSID.LSID_ChildlessExclude);
            chkShowPlaces.Text = LangMan.LS(LSID.LSID_ShowPlaces);
            chkSeparateDAPLines.Text = LangMan.LS(LSID.LSID_SeparateDatesAndPlacesLines);
            chkHideUnknownSpouses.Text = LangMan.LS(LSID.LSID_HideUnknownSpouses);
            chkCheckTreeSize.Text = LangMan.LS(LSID.LSID_CheckTreeSize);
            chkDottedLinesOfAdoptedChildren.Text = LangMan.LS(LSID.LSID_DottedLinesOfAdoptedChildren);
            chkBoldNames.Text = LangMan.LS(LSID.LSID_BoldNames);

            grpTreeDecor.Text = LangMan.LS(LSID.LSID_Decor);
            lblMaleColor.Text = LangMan.LS(LSID.LSID_Man);
            lblFemaleColor.Text = LangMan.LS(LSID.LSID_Woman);
            lblUnkSexColor.Text = LangMan.LS(LSID.LSID_UnkSex);
            lblUnHusbandColor.Text = LangMan.LS(LSID.LSID_UnHusband);
            lblUnWifeColor.Text = LangMan.LS(LSID.LSID_UnWife);
            //lblFont.Text = LangMan.LS(LSID.LSID_Font);

            grpSpacings.Text = LangMan.LS(LSID.LSID_Spacings);
            lblMargins.Text = LangMan.LS(LSID.LSID_Margins);
            lblBranchDist.Text = LangMan.LS(LSID.LSID_BranchDist);
            lblGenDist.Text = LangMan.LS(LSID.LSID_GenDist);
            lblSpouseDist.Text = LangMan.LS(LSID.LSID_SpouseDist);

            chkSeparateDepth.Text = LangMan.LS(LSID.LSID_SeparateDepth);
            lblDefaultDepth.Text = LangMan.LS(LSID.LSID_DefaultDepth);
            lblDefaultDepthAncestors.Text = LangMan.LS(LSID.LSID_DefaultDepth) + ": " + LangMan.LS(LSID.LSID_Ancestors);
            lblDefaultDepthDescendants.Text = LangMan.LS(LSID.LSID_DefaultDepth) + ": " + LangMan.LS(LSID.LSID_Descendants);

            pageAncCircle.Text = LangMan.LS(LSID.LSID_AncestorsCircle);

            // UIView
            pageUIView.Text = LangMan.LS(LSID.LSID_Interface);

            pageViewCommon.Text = LangMan.LS(LSID.LSID_ListsAll);

            rgFNPFormat.Text = LangMan.LS(LSID.LSID_NamesFormat);
            radSNP.Text = LangMan.LS(LSID.LSID_NF1);
            radS_NP.Text = LangMan.LS(LSID.LSID_NF2);
            radS_N_P.Text = LangMan.LS(LSID.LSID_NF3);

            chkPlacesWithAddress.Text = LangMan.LS(LSID.LSID_PlacesWithAddress);
            chkHighlightUnparented.Text = LangMan.LS(LSID.LSID_HighlightUnparented);
            chkHighlightUnmarried.Text = LangMan.LS(LSID.LSID_HighlightUnmarried);

            chkAutoSortChildren.Text = LangMan.LS(LSID.LSID_AutoSortChildren);
            chkAutoSortSpouses.Text = LangMan.LS(LSID.LSID_AutoSortSpouses);
            chkFirstCapitalLetterInNames.Text = LangMan.LS(LSID.LSID_FirstCapitalLetterInNames);

            grpDateFormat.Text = LangMan.LS(LSID.LSID_DateFormat);
            chkShowDatesCalendar.Text = LangMan.LS(LSID.LSID_ShowDatesCalendar);
            chkShowDatesSigns.Text = LangMan.LS(LSID.LSID_ShowDatesSigns);

            grpAdvancedNames.Text = LangMan.LS(LSID.LSID_AdditionalNames);
            chkExtendWomanSurnames.Text = LangMan.LS(LSID.LSID_ExtendedWomanSurnames);
            radMaiden_Married.Text = LangMan.LS(LSID.LSID_WSF_Maiden_Married);
            radMarried_Maiden.Text = LangMan.LS(LSID.LSID_WSF_Married_Maiden);
            radMaiden.Text = LangMan.LS(LSID.LSID_WSF_Maiden);
            radMarried.Text = LangMan.LS(LSID.LSID_WSF_Married);

            pageViewPersons.Text = LangMan.LS(LSID.LSID_ListPersons);
            btnDefList.Text = LangMan.LS(LSID.LSID_DefList);

            // Pedigree
            pagePedigree.Text = LangMan.LS(LSID.LSID_Pedigrees);

            grpPedigree.Text = LangMan.LS(LSID.LSID_PedigreeGen);
            chkAttributes.Text = LangMan.LS(LSID.LSID_IncludeAttributes);
            chkNotes.Text = LangMan.LS(LSID.LSID_IncludeNotes);
            chkSources.Text = LangMan.LS(LSID.LSID_IncludeSources);
            chkGenerations.Text = LangMan.LS(LSID.LSID_IncludeGenerations);

            grpPedigreeFormat.Text = LangMan.LS(LSID.LSID_PedigreeFormat);
            radExcess.Text = LangMan.LS(LSID.LSID_PF1);
            radCompact.Text = LangMan.LS(LSID.LSID_PF2);

            // Plugins
            pagePlugins.Text = LangMan.LS(LSID.LSID_Plugins);
        }

        private void chkTreeChartOption_CheckedChanged(object sender, EventArgs e)
        {
            chkShowPlaces.Enabled = !chkOnlyYears.Checked.GetValueOrDefault();
            chkSeparateDAPLines.Enabled = chkShowPlaces.Checked.GetValueOrDefault() && !chkOnlyYears.Checked.GetValueOrDefault();

            chkDefaultPortraits.Enabled = chkPortraitsVisible.Checked.GetValueOrDefault();

            chkDiffLines.Enabled = chkName.Checked.GetValueOrDefault() && chkPatronymic.Checked.GetValueOrDefault();

            chkOnlyYears.Enabled = chkBirthDate.Checked.GetValueOrDefault() && chkDeathDate.Checked.GetValueOrDefault();
        }

        private void chkSeparateDepth_CheckedChanged(object sender, EventArgs e)
        {
            numDefaultDepth.Enabled = !chkSeparateDepth.Checked.GetValueOrDefault();
            numDefaultDepthAncestors.Enabled = chkSeparateDepth.Checked.GetValueOrDefault();
            numDefaultDepthDescendants.Enabled = chkSeparateDepth.Checked.GetValueOrDefault();
        }
    }
}
