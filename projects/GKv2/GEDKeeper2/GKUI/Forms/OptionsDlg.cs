/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;
using BSLib.Design;
using BSLib.Design.Graphics;
using BSLib.Design.Handlers;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Views;
using GKCore.Options;
using GKCore.Plugins;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class OptionsDlg : CommonDialog, ILocalization, IOptionsDlg
    {
        private readonly IHost fHost;
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

            fHost = host;
            fOptions = GlobalOptions.Instance;
            fTempColumns = IndividualListMan.CreateIndividualListColumns();

            SetLang();
            UpdateForm();
        }

        private void UpdateColumnsList()
        {
            lstPersonColumns.ItemCheck -= ListPersonColumns_ItemCheck;
            lstPersonColumns.BeginUpdate();
            try {
                lstPersonColumns.Items.Clear();

                int num = fTempColumns.Count;
                for (int i = 0; i < num; i++) {
                    ListColumn column = fTempColumns.OrderedColumns[i];

                    lstPersonColumns.Items.Add(LangMan.LS(column.ColName), column.CurActive);
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
            cmbLanguages.Items.Add(new ComboItem<int>(LangMan.LS_DEF_NAME, LangMan.LS_DEF_CODE));
            foreach (LangRecord lngRec in GlobalOptions.Instance.Languages) {
                if (lngRec.Code != LangMan.LS_DEF_CODE) {
                    cmbLanguages.Items.Add(new ComboItem<int>(lngRec.Name, lngRec.Code));
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
            UIHelper.SetSelectedTag<MediaStoreType>(cmbMediaStoreDefault, (MediaStoreType)fOptions.MediaStoreDefault);
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

            lblMaleColor.BackColor = UIHelper.ConvertColor(fOptions.TreeChartOptions.MaleColor);
            lblFemaleColor.BackColor = UIHelper.ConvertColor(fOptions.TreeChartOptions.FemaleColor);
            lblUnkSexColor.BackColor = UIHelper.ConvertColor(fOptions.TreeChartOptions.UnkSexColor);
            lblUnHusbandColor.BackColor = UIHelper.ConvertColor(fOptions.TreeChartOptions.UnHusbandColor);
            lblUnWifeColor.BackColor = UIHelper.ConvertColor(fOptions.TreeChartOptions.UnWifeColor);

            numMargins.Value = fOptions.TreeChartOptions.Margins;
            numBranchDist.Value = fOptions.TreeChartOptions.BranchDistance;
            numGenDist.Value = fOptions.TreeChartOptions.LevelDistance;
            numSpouseDist.Value = fOptions.TreeChartOptions.SpouseDistance;

            numDefaultDepth.Value = fOptions.TreeChartOptions.DepthLimit;

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
            if (!chkExtendWomanSurnames.Checked) {
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

            pan.BackColor = UIHelper.ConvertColor(AppHost.StdDialogs.SelectColor(UIHelper.ConvertColor(pan.BackColor)));
        }

        private void panDefFont_Click(object sender, EventArgs e)
        {
            TreeChartOptions chartOptions = fOptions.TreeChartOptions;

            var sdFont = new System.Drawing.Font(chartOptions.DefFontName, chartOptions.DefFontSize);
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
                DialogResult = DialogResult.OK;
            } catch (Exception ex) {
                Logger.WriteError("OptionsDlg.btnAccept_Click()", ex);
                DialogResult = DialogResult.None;
            }
        }

        private void AcceptProxyOptions()
        {
            fOptions.Proxy.UseProxy = chkUseProxy.Checked;
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

            fOptions.Autosave = chkAutosave.Checked;
            fOptions.AutosaveInterval = (int)numASMin.Value;
            fOptions.FileBackupEachRevisionMaxCount = (int)numBackupRevisionsMaxCount.Value;
        }

        private void AcceptOtherOptions()
        {
            fOptions.ShowTips = chkShowOnStart.Checked;
            fOptions.LoadRecentFiles = chkLoadRecentFiles.Checked;
            fOptions.AutoCheckUpdates = chkAutoCheckUpdates.Checked;
            fOptions.CharsetDetection = chkCharsetDetection.Checked;
            fOptions.DialogClosingWarn = chkDialogClosingWarn.Checked;
        }

        private void AcceptLangs()
        {
            var item = cmbLanguages.SelectedItem as ComboItem<int>;
            if (item != null) {
                AppHost.Instance.LoadLanguage((int)item.Tag);
            }
        }

        private void AcceptMediaOptions()
        {
            fOptions.RemovableMediaWarning = chkRemovableMediaWarning.Checked;
            fOptions.EmbeddedMediaPlayer = chkEmbeddedMediaPlayer.Checked;
            fOptions.AllowMediaStoreReferences = chkAllowMediaDirectRefs.Checked;
            fOptions.AllowMediaStoreRelativeReferences = chkAllowMediaStoreRelativeReferences.Checked;
            fOptions.MediaStoreDefault = (int)UIHelper.GetSelectedTag<MediaStoreType>(cmbMediaStoreDefault);
            fOptions.AllowDeleteMediaFileFromStgArc = chkAllowDeleteMediaFileFromStgArc.Checked;
            fOptions.AllowDeleteMediaFileFromRefs = chkAllowDeleteMediaFileFromRefs.Checked;
            fOptions.DeleteMediaFileWithoutConfirm = chkDeleteMediaFileWithoutConfirm.Checked;
        }

        private void AcceptTreeChartsOptions()
        {
            fOptions.TreeChartOptions.FamilyVisible = chkSurname.Checked;
            fOptions.TreeChartOptions.NameVisible = chkName.Checked;
            fOptions.TreeChartOptions.PatronymicVisible = chkPatronymic.Checked;
            fOptions.TreeChartOptions.DiffLines = chkDiffLines.Checked;
            fOptions.TreeChartOptions.BirthDateVisible = chkBirthDate.Checked;
            fOptions.TreeChartOptions.DeathDateVisible = chkDeathDate.Checked;
            fOptions.TreeChartOptions.OnlyYears = chkOnlyYears.Checked;
            fOptions.TreeChartOptions.MarriagesDates = chkMarriagesDates.Checked;
            fOptions.TreeChartOptions.Kinship = chkKinship.Checked;
            fOptions.TreeChartOptions.SignsVisible = chkSignsVisible.Checked;
            fOptions.TreeChartOptions.Decorative = chkTreeDecorative.Checked;
            fOptions.TreeChartOptions.PortraitsVisible = chkPortraitsVisible.Checked;
            fOptions.TreeChartOptions.DefaultPortraits = chkDefaultPortraits.Checked;
            fOptions.TreeChartOptions.InvertedTree = chkInvertedTree.Checked;
            fOptions.TreeChartOptions.ChildlessExclude = chkChildlessExclude.Checked;
            fOptions.TreeChartOptions.ShowPlaces = chkShowPlaces.Checked;
            fOptions.TreeChartOptions.HideUnknownSpouses = chkHideUnknownSpouses.Checked;
            fOptions.CheckTreeSize = chkCheckTreeSize.Checked;

            fOptions.TreeChartOptions.MaleColor = UIHelper.ConvertColor(lblMaleColor.BackColor);
            fOptions.TreeChartOptions.FemaleColor = UIHelper.ConvertColor(lblFemaleColor.BackColor);
            fOptions.TreeChartOptions.UnkSexColor = UIHelper.ConvertColor(lblUnkSexColor.BackColor);
            fOptions.TreeChartOptions.UnHusbandColor = UIHelper.ConvertColor(lblUnHusbandColor.BackColor);
            fOptions.TreeChartOptions.UnWifeColor = UIHelper.ConvertColor(lblUnWifeColor.BackColor);

            fOptions.TreeChartOptions.Margins = (int)numMargins.Value;
            fOptions.TreeChartOptions.BranchDistance = (int)numBranchDist.Value;
            fOptions.TreeChartOptions.LevelDistance = (int)numGenDist.Value;
            fOptions.TreeChartOptions.SpouseDistance = (int)numSpouseDist.Value;

            fOptions.TreeChartOptions.DepthLimit = (int)numDefaultDepth.Value;
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
            fOptions.ShowDatesCalendar = chkShowDatesCalendar.Checked;
            fOptions.ShowDatesSign = chkShowDatesSigns.Checked;

            fOptions.PlacesWithAddress = chkPlacesWithAddress.Checked;
            fOptions.ListHighlightUnparentedPersons = chkHighlightUnparented.Checked;
            fOptions.ListHighlightUnmarriedPersons = chkHighlightUnmarried.Checked;

            fOptions.AutoSortChildren = chkAutoSortChildren.Checked;
            fOptions.AutoSortSpouses = chkAutoSortSpouses.Checked;
            fOptions.FirstCapitalLetterInNames = chkFirstCapitalLetterInNames.Checked;
        }

        private void AcceptWomanSurnameFormat()
        {
            if (!chkExtendWomanSurnames.Checked) {
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
            fOptions.PedigreeOptions.IncludeAttributes = chkAttributes.Checked;
            fOptions.PedigreeOptions.IncludeNotes = chkNotes.Checked;
            fOptions.PedigreeOptions.IncludeSources = chkSources.Checked;
            fOptions.PedigreeOptions.IncludeGenerations = chkGenerations.Checked;

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
            fTempColumns.OrderedColumns[e.Index].CurActive = (e.NewValue == CheckState.Checked);
        }

        private void chkPortraitsVisible_CheckedChanged(object sender, EventArgs e)
        {
            chkDefaultPortraits.Enabled = chkPortraitsVisible.Checked;
        }

        public void SetPage(OptionsPage page)
        {
            switch (page) {
                case OptionsPage.opCommon:
                    PageControl1.SelectedTab = pageCommon;
                    break;

                case OptionsPage.opTreeChart:
                    PageControl1.SelectedTab = pageCharts;
                    tabsCharts.SelectTab(0);
                    break;

                case OptionsPage.opCircleChart:
                    PageControl1.SelectedTab = pageCharts;
                    tabsCharts.SelectTab(1);
                    break;

                case OptionsPage.opInterface:
                    PageControl1.SelectedTab = pageUIView;
                    break;

                case OptionsPage.opPedigree:
                    PageControl1.SelectedTab = pagePedigree;
                    break;

                case OptionsPage.opMultimedia:
                    PageControl1.SelectedTab = pageMultimedia;
                    break;
            }
        }

        public void SetLang()
        {
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Text = LangMan.LS(LSID.LSID_MIOptions);
            pageCommon.Text = LangMan.LS(LSID.LSID_Common);
            pageUIView.Text = LangMan.LS(LSID.LSID_Interface);
            pageTreeChart.Text = LangMan.LS(LSID.LSID_Trees);
            pagePedigree.Text = LangMan.LS(LSID.LSID_Pedigrees);

            grpInternet.Text = LangMan.LS(LSID.LSID_Internet);
            chkUseProxy.Text = LangMan.LS(LSID.LSID_ProxyUse);
            lblProxyServer.Text = LangMan.LS(LSID.LSID_ProxyServer);
            lblProxyPort.Text = LangMan.LS(LSID.LSID_ProxyPort);
            lblProxyLogin.Text = LangMan.LS(LSID.LSID_ProxyLogin);
            lblProxyPassword.Text = LangMan.LS(LSID.LSID_Password);

            grpOther.Text = LangMan.LS(LSID.LSID_Other);

            chkShowOnStart.Text = LangMan.LS(LSID.LSID_StartupTips);
            lblLanguage.Text = LangMan.LS(LSID.LSID_Language);
            pageViewCommon.Text = LangMan.LS(LSID.LSID_ListsAll);
            pageViewPersons.Text = LangMan.LS(LSID.LSID_ListPersons);
            rgFNPFormat.Text = LangMan.LS(LSID.LSID_NamesFormat);
            radSNP.Text = LangMan.LS(LSID.LSID_NF1);
            radS_NP.Text = LangMan.LS(LSID.LSID_NF2);
            radS_N_P.Text = LangMan.LS(LSID.LSID_NF3);
            grpDateFormat.Text = LangMan.LS(LSID.LSID_DateFormat);
            chkPlacesWithAddress.Text = LangMan.LS(LSID.LSID_PlacesWithAddress);
            chkHighlightUnparented.Text = LangMan.LS(LSID.LSID_HighlightUnparented);
            chkHighlightUnmarried.Text = LangMan.LS(LSID.LSID_HighlightUnmarried);
            btnDefList.Text = LangMan.LS(LSID.LSID_DefList);
            grpTreePersons.Text = LangMan.LS(LSID.LSID_ViewTree);
            chkSurname.Text = LangMan.LS(LSID.LSID_Surname);
            chkName.Text = LangMan.LS(LSID.LSID_Name);
            chkPatronymic.Text = LangMan.LS(LSID.LSID_Patronymic);
            chkDiffLines.Text = LangMan.LS(LSID.LSID_DiffLines);
            chkBirthDate.Text = LangMan.LS(LSID.LSID_BirthDate);
            chkDeathDate.Text = LangMan.LS(LSID.LSID_DeathDate);
            chkOnlyYears.Text = LangMan.LS(LSID.LSID_OnlyYears);
            chkKinship.Text = LangMan.LS(LSID.LSID_Kinship);
            chkSignsVisible.Text = LangMan.LS(LSID.LSID_SignsVisible);
            chkTreeDecorative.Text = LangMan.LS(LSID.LSID_TreeDecorative);
            chkPortraitsVisible.Text = LangMan.LS(LSID.LSID_PortraitsVisible);
            chkDefaultPortraits.Text = LangMan.LS(LSID.LSID_DefaultPortraits);
            chkChildlessExclude.Text = LangMan.LS(LSID.LSID_ChildlessExclude);
            chkInvertedTree.Text = LangMan.LS(LSID.LSID_InvertedTree);
            chkMarriagesDates.Text = LangMan.LS(LSID.LSID_MarriagesDates);
            grpTreeDecor.Text = LangMan.LS(LSID.LSID_Decor);
            lblMaleColor.Text = LangMan.LS(LSID.LSID_Man);
            lblFemaleColor.Text = LangMan.LS(LSID.LSID_Woman);
            lblUnkSexColor.Text = LangMan.LS(LSID.LSID_UnkSex);
            lblUnHusbandColor.Text = LangMan.LS(LSID.LSID_UnHusband);
            lblUnWifeColor.Text = LangMan.LS(LSID.LSID_UnWife);
            lblFont.Text = LangMan.LS(LSID.LSID_Font);
            grpPedigree.Text = LangMan.LS(LSID.LSID_PedigreeGen);
            chkAttributes.Text = LangMan.LS(LSID.LSID_IncludeAttributes);
            chkNotes.Text = LangMan.LS(LSID.LSID_IncludeNotes);
            chkSources.Text = LangMan.LS(LSID.LSID_IncludeSources);
            grpPedigreeFormat.Text = LangMan.LS(LSID.LSID_PedigreeFormat);
            radExcess.Text = LangMan.LS(LSID.LSID_PF1);
            radCompact.Text = LangMan.LS(LSID.LSID_PF2);
            pageCharts.Text = LangMan.LS(LSID.LSID_Charts);
            pagePlugins.Text = LangMan.LS(LSID.LSID_Plugins);
            chkShowDatesCalendar.Text = LangMan.LS(LSID.LSID_ShowDatesCalendar);
            chkShowDatesSigns.Text = LangMan.LS(LSID.LSID_ShowDatesSigns);
            chkShowPlaces.Text = LangMan.LS(LSID.LSID_ShowPlaces);
            chkHideUnknownSpouses.Text = LangMan.LS(LSID.LSID_HideUnknownSpouses);

            grpFileBackup.Text = LangMan.LS(LSID.LSID_FileBackup);
            radFBNone.Text = LangMan.LS(LSID.LSID_Not);
            radFBOnlyPrev.Text = LangMan.LS(LSID.LSID_BackupOnlyPrev);
            radFBEachRevision.Text = LangMan.LS(LSID.LSID_BackupEachRevision);

            chkAutosave.Text = LangMan.LS(LSID.LSID_Autosave);
            lblMinutes.Text = LangMan.LS(LSID.LSID_Minutes);

            chkGenerations.Text = LangMan.LS(LSID.LSID_IncludeGenerations);
            pageAncCircle.Text = LangMan.LS(LSID.LSID_AncestorsCircle);

            grpAdvancedNames.Text = LangMan.LS(LSID.LSID_AdditionalNames);
            chkExtendWomanSurnames.Text = LangMan.LS(LSID.LSID_ExtendedWomanSurnames);
            radMaiden_Married.Text = LangMan.LS(LSID.LSID_WSF_Maiden_Married);
            radMarried_Maiden.Text = LangMan.LS(LSID.LSID_WSF_Married_Maiden);
            radMaiden.Text = LangMan.LS(LSID.LSID_WSF_Maiden);
            radMarried.Text = LangMan.LS(LSID.LSID_WSF_Married);

            lblGeocoder.Text = LangMan.LS(LSID.LSID_Geocoder);
            chkRemovableMediaWarning.Text = LangMan.LS(LSID.LSID_RemovableMediaWarningOption);
            chkLoadRecentFiles.Text = LangMan.LS(LSID.LSID_LoadRecentFiles);
            chkEmbeddedMediaPlayer.Text = LangMan.LS(LSID.LSID_EmbeddedMediaPlayer);
            pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
            chkAllowMediaDirectRefs.Text = LangMan.LS(LSID.LSID_AllowMediaDirectReferences);
            chkAutoCheckUpdates.Text = LangMan.LS(LSID.LSID_AutoCheckUpdates);

            grpSpacings.Text = LangMan.LS(LSID.LSID_Spacings);
            lblMargins.Text = LangMan.LS(LSID.LSID_Margins);
            lblBranchDist.Text = LangMan.LS(LSID.LSID_BranchDist);
            lblGenDist.Text = LangMan.LS(LSID.LSID_GenDist);
            lblSpouseDist.Text = LangMan.LS(LSID.LSID_SpouseDist);

            chkAutoSortChildren.Text = LangMan.LS(LSID.LSID_AutoSortChildren);
            chkAutoSortSpouses.Text = LangMan.LS(LSID.LSID_AutoSortSpouses);
            chkCheckTreeSize.Text = LangMan.LS(LSID.LSID_CheckTreeSize);
            chkCharsetDetection.Text = LangMan.LS(LSID.LSID_CharsetDetection);

            chkAllowMediaStoreRelativeReferences.Text = LangMan.LS(LSID.LSID_AllowMediaRelativeReferences);
            lblMediaStoreDefault.Text = LangMan.LS(LSID.LSID_MediaStoreDefault);
            chkAllowDeleteMediaFileFromStgArc.Text = LangMan.LS(LSID.LSID_AllowDeleteMediaFileFromStgArc);
            chkAllowDeleteMediaFileFromRefs.Text = LangMan.LS(LSID.LSID_AllowDeleteMediaFileFromRefs);
            chkDeleteMediaFileWithoutConfirm.Text = LangMan.LS(LSID.LSID_DeleteMediaFileWithoutConfirm);

            cmbMediaStoreDefault.Items.Clear();
            for (MediaStoreType mst = MediaStoreType.mstReference; mst <= MediaStoreType.mstRelativeReference; mst++) {
                cmbMediaStoreDefault.Items.Add(new ComboItem<MediaStoreType>(LangMan.LS(GKData.GKStoreTypes[(int)mst].Name), mst));
            }

            lblBackupRevisionsMaxCount.Text = LangMan.LS(LSID.LSID_BackupRevisionsMaxCount);
            chkFirstCapitalLetterInNames.Text = LangMan.LS(LSID.LSID_FirstCapitalLetterInNames);
            lblDefaultDepth.Text = LangMan.LS(LSID.LSID_DefaultDepth);
            chkDialogClosingWarn.Text = LangMan.LS(LSID.LSID_WarnForClosingDialog);
        }
    }
}
