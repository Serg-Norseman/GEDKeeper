/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Plugins;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class OptionsDlg : CommonDialog, ILocalization
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

            lstPersonColumns.AddCheckedColumn("x", 75);
            lstPersonColumns.AddColumn("Title", 100);

            lvPlugins.AddColumn("Title", 75);
            lvPlugins.AddColumn("Version", 100);
            lvPlugins.AddColumn("Copyright", 125);
            lvPlugins.AddColumn("Description", 250);

            SetLang();
            UpdateForm();
        }

        private void UpdateColumnsList()
        {
            lstPersonColumns.ItemCheck -= ListPersonColumns_ItemCheck;
            lstPersonColumns.BeginUpdate();
            try
            {
                lstPersonColumns.ClearItems();

                int num = fTempColumns.Count;
                for (int i = 0; i < num; i++) {
                    ListColumn column = fTempColumns.OrderedColumns[i];

                    lstPersonColumns.AddItem(null, column.CurActive, LangMan.LS(column.ColName));
                }
            }
            finally
            {
                lstPersonColumns.EndUpdate();
            }
            lstPersonColumns.ItemCheck += ListPersonColumns_ItemCheck;
        }

        private void UpdateControls()
        {
            lblChartFont.Text = fOptions.ChartOptions.DefFontName + @", " + fOptions.ChartOptions.DefFontSize.ToString();
        }

        private void UpdateLangs()
        {
            cmbLanguages.Items.Clear();
            cmbLanguages.Items.Add(new GKComboItem(LangMan.LS_DEF_NAME, LangMan.LS_DEF_CODE));
            foreach (LangRecord lngRec in GlobalOptions.Instance.Languages) {
                if (lngRec.Code != LangMan.LS_DEF_CODE) {
                    cmbLanguages.Items.Add(new GKComboItem(lngRec.Name, lngRec.Code));
                }
            }
            UIHelper.SelectComboItem(cmbLanguages, fOptions.InterfaceLang, true);
        }

        private void UpdateForm()
        {
            switch (fOptions.DefNameFormat)
            {
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

            switch (fOptions.DefDateFormat)
            {
                case DateFormat.dfDD_MM_YYYY:
                    radDMY.Checked = true;
                    break;
                case DateFormat.dfYYYY_MM_DD:
                    radYMD.Checked = true;
                    break;
            }

            chkPlacesWithAddress.Checked = fOptions.PlacesWithAddress;
            chkHighlightUnparented.Checked = fOptions.ListHighlightUnparentedPersons;
            chkHighlightUnmarried.Checked = fOptions.ListHighlightUnmarriedPersons;
            chkShowDatesCalendar.Checked = fOptions.ShowDatesCalendar;
            chkShowDatesSigns.Checked = fOptions.ShowDatesSign;
            chkAutosave.Checked = fOptions.Autosave;
            numASMin.Value = fOptions.AutosaveInterval;
            chkRemovableMediaWarning.Checked = fOptions.RemovableMediaWarning;
            chkLoadRecentFiles.Checked = fOptions.LoadRecentFiles;
            chkEmbeddedMediaPlayer.Checked = fOptions.EmbeddedMediaPlayer;
            chkAllowMediaDirectRefs.Checked = fOptions.AllowMediaStoreReferences;
            chkAutoCheckUpdates.Checked = fOptions.AutoCheckUpdates;

            chkSurname.Checked = fOptions.ChartOptions.FamilyVisible;
            chkName.Checked = fOptions.ChartOptions.NameVisible;
            chkPatronymic.Checked = fOptions.ChartOptions.PatronymicVisible;
            chkDiffLines.Checked = fOptions.ChartOptions.DiffLines;
            chkBirthDate.Checked = fOptions.ChartOptions.BirthDateVisible;
            chkDeathDate.Checked = fOptions.ChartOptions.DeathDateVisible;
            chkOnlyYears.Checked = fOptions.ChartOptions.OnlyYears;
            chkKinship.Checked = fOptions.ChartOptions.Kinship;
            chkSignsVisible.Checked = fOptions.ChartOptions.SignsVisible;
            chkChildlessExclude.Checked = fOptions.ChartOptions.ChildlessExclude;
            chkTreeDecorative.Checked = fOptions.ChartOptions.Decorative;
            chkPortraitsVisible.Checked = fOptions.ChartOptions.PortraitsVisible;
            chkDefaultPortraits.Checked = fOptions.ChartOptions.DefaultPortraits;
            chkInvertedTree.Checked = fOptions.ChartOptions.InvertedTree;
            chkMarriagesDates.Checked = fOptions.ChartOptions.MarriagesDates;
            chkShowPlaces.Checked = fOptions.ChartOptions.ShowPlaces;
            chkHideUnknownSpouses.Checked = fOptions.ChartOptions.HideUnknownSpouses;

            numMargins.Value = fOptions.ChartOptions.Margins;
            numBranchDist.Value = fOptions.ChartOptions.BranchDistance;
            numGenDist.Value = fOptions.ChartOptions.LevelDistance;
            numSpouseDist.Value = fOptions.ChartOptions.SpouseDistance;

            lblMaleColor.BackgroundColor = UIHelper.ConvertColor(fOptions.ChartOptions.MaleColor);
            lblFemaleColor.BackgroundColor = UIHelper.ConvertColor(fOptions.ChartOptions.FemaleColor);
            lblUnkSexColor.BackgroundColor = UIHelper.ConvertColor(fOptions.ChartOptions.UnkSexColor);
            lblUnHusbandColor.BackgroundColor = UIHelper.ConvertColor(fOptions.ChartOptions.UnHusbandColor);
            lblUnWifeColor.BackgroundColor = UIHelper.ConvertColor(fOptions.ChartOptions.UnWifeColor);

            chkUseProxy.Checked = fOptions.Proxy.UseProxy;
            txtProxyServer.Text = fOptions.Proxy.Server;
            txtProxyPort.Text = fOptions.Proxy.Port;
            txtProxyLogin.Text = fOptions.Proxy.Login;
            txtProxyPass.Text = fOptions.Proxy.Password;

            chkAttributes.Checked = fOptions.PedigreeOptions.IncludeAttributes;
            chkNotes.Checked = fOptions.PedigreeOptions.IncludeNotes;
            chkSources.Checked = fOptions.PedigreeOptions.IncludeSources;
            chkGenerations.Checked = fOptions.PedigreeOptions.IncludeGenerations;

            switch (fOptions.PedigreeOptions.Format)
            {
                case PedigreeFormat.Excess:
                    radExcess.Checked = true;
                    break;
                case PedigreeFormat.Compact:
                    radCompact.Checked = true;
                    break;
            }

            chkShowOnStart.Checked = fOptions.ShowTips;

            switch (fOptions.FileBackup)
            {
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

            fOptions.IndividualListColumns.CopyTo(fTempColumns);
            UpdateColumnsList();

            UpdateControls();
            UpdateLangs();

            UpdatePlugins();
            UpdateWomanSurnameFormat();

            ancOptionsControl1.Options = fOptions.AncestorsCircleOptions;
            ancOptionsControl1.UpdateControls();

            cmbGeocoder.Text = fOptions.Geocoder;
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

            switch (wsFmt)
            {
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

        private void UpdatePlugins()
        {
            lvPlugins.ClearItems();

            int num = AppHost.Plugins.Count;
            for (int i = 0; i < num; i++)
            {
                IPlugin plugin = AppHost.Plugins[i];
                PluginInfo pInfo = PluginInfo.GetPluginInfo(plugin);

                lvPlugins.AddItem(null, pInfo.Title,
                                  pInfo.Version,
                                  pInfo.Copyright,
                                  pInfo.Description);
            }
        }

        private void PanColor_Click(object sender, EventArgs e)
        {
            Label pan = (sender as Label);
            if (pan == null) return;

            using (var clrDlg = new ColorDialog()) {
                clrDlg.Color = pan.BackgroundColor;
                if (clrDlg.ShowDialog(this) == Eto.Forms.DialogResult.Ok) {
                    pan.BackgroundColor = clrDlg.Color;
                }
            }
        }

        private void panDefFont_Click(object sender, EventArgs e)
        {
            TreeChartOptions chartOptions = fOptions.ChartOptions;

            var sdFont = new Font(chartOptions.DefFontName, chartOptions.DefFontSize);
            IFont font = new FontHandler(sdFont);
            font = AppHost.StdDialogs.SelectFont(font);
            if (font != null)
            {
                chartOptions.DefFontName = font.Name;
                chartOptions.DefFontSize = (int)(Math.Round(font.Size));
            }

            UpdateControls();
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                AcceptChanges();
                DialogResult = DialogResult.Ok;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("OptionsDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
            }
        }

        private void AcceptChanges()
        {
            fTempColumns.CopyTo(fOptions.IndividualListColumns);

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

            fOptions.PlacesWithAddress = chkPlacesWithAddress.Checked.GetValueOrDefault();
            fOptions.ListHighlightUnparentedPersons = chkHighlightUnparented.Checked.GetValueOrDefault();
            fOptions.ListHighlightUnmarriedPersons = chkHighlightUnmarried.Checked.GetValueOrDefault();
            fOptions.ShowDatesCalendar = chkShowDatesCalendar.Checked.GetValueOrDefault();
            fOptions.ShowDatesSign = chkShowDatesSigns.Checked.GetValueOrDefault();
            fOptions.Autosave = chkAutosave.Checked.GetValueOrDefault();
            fOptions.AutosaveInterval = (int)numASMin.Value;
            fOptions.RemovableMediaWarning = chkRemovableMediaWarning.Checked.GetValueOrDefault();
            fOptions.LoadRecentFiles = chkLoadRecentFiles.Checked.GetValueOrDefault();
            fOptions.EmbeddedMediaPlayer = chkEmbeddedMediaPlayer.Checked.GetValueOrDefault();
            fOptions.AllowMediaStoreReferences = chkAllowMediaDirectRefs.Checked.GetValueOrDefault();
            fOptions.AutoCheckUpdates = chkAutoCheckUpdates.Checked.GetValueOrDefault();

            fOptions.ChartOptions.FamilyVisible = chkSurname.Checked.GetValueOrDefault();
            fOptions.ChartOptions.NameVisible = chkName.Checked.GetValueOrDefault();
            fOptions.ChartOptions.PatronymicVisible = chkPatronymic.Checked.GetValueOrDefault();
            fOptions.ChartOptions.DiffLines = chkDiffLines.Checked.GetValueOrDefault();
            fOptions.ChartOptions.BirthDateVisible = chkBirthDate.Checked.GetValueOrDefault();
            fOptions.ChartOptions.DeathDateVisible = chkDeathDate.Checked.GetValueOrDefault();
            fOptions.ChartOptions.OnlyYears = chkOnlyYears.Checked.GetValueOrDefault();
            fOptions.ChartOptions.Kinship = chkKinship.Checked.GetValueOrDefault();
            fOptions.ChartOptions.SignsVisible = chkSignsVisible.Checked.GetValueOrDefault();
            fOptions.ChartOptions.ChildlessExclude = chkChildlessExclude.Checked.GetValueOrDefault();
            fOptions.ChartOptions.Decorative = chkTreeDecorative.Checked.GetValueOrDefault();
            fOptions.ChartOptions.PortraitsVisible = chkPortraitsVisible.Checked.GetValueOrDefault();
            fOptions.ChartOptions.DefaultPortraits = chkDefaultPortraits.Checked.GetValueOrDefault();
            fOptions.ChartOptions.InvertedTree = chkInvertedTree.Checked.GetValueOrDefault();
            fOptions.ChartOptions.MarriagesDates = chkMarriagesDates.Checked.GetValueOrDefault();
            fOptions.ChartOptions.ShowPlaces = chkShowPlaces.Checked.GetValueOrDefault();
            fOptions.ChartOptions.HideUnknownSpouses = chkHideUnknownSpouses.Checked.GetValueOrDefault();

            fOptions.ChartOptions.Margins = (int)numMargins.Value;
            fOptions.ChartOptions.BranchDistance = (int)numBranchDist.Value;
            fOptions.ChartOptions.LevelDistance = (int)numGenDist.Value;
            fOptions.ChartOptions.SpouseDistance = (int)numSpouseDist.Value;

            fOptions.ChartOptions.MaleColor = UIHelper.ConvertColor(lblMaleColor.BackgroundColor);
            fOptions.ChartOptions.FemaleColor = UIHelper.ConvertColor(lblFemaleColor.BackgroundColor);
            fOptions.ChartOptions.UnkSexColor = UIHelper.ConvertColor(lblUnkSexColor.BackgroundColor);
            fOptions.ChartOptions.UnHusbandColor = UIHelper.ConvertColor(lblUnHusbandColor.BackgroundColor);
            fOptions.ChartOptions.UnWifeColor = UIHelper.ConvertColor(lblUnWifeColor.BackgroundColor);

            fOptions.Proxy.UseProxy = chkUseProxy.Checked.GetValueOrDefault();
            fOptions.Proxy.Server = txtProxyServer.Text;
            fOptions.Proxy.Port = txtProxyPort.Text;
            fOptions.Proxy.Login = txtProxyLogin.Text;
            fOptions.Proxy.Password = txtProxyPass.Text;

            fOptions.PedigreeOptions.IncludeAttributes = chkAttributes.Checked.GetValueOrDefault();
            fOptions.PedigreeOptions.IncludeNotes = chkNotes.Checked.GetValueOrDefault();
            fOptions.PedigreeOptions.IncludeSources = chkSources.Checked.GetValueOrDefault();
            fOptions.PedigreeOptions.IncludeGenerations = chkGenerations.Checked.GetValueOrDefault();

            if (radExcess.Checked) {
                fOptions.PedigreeOptions.Format = PedigreeFormat.Excess;
            } else if (radCompact.Checked) {
                fOptions.PedigreeOptions.Format = PedigreeFormat.Compact;
            }

            fOptions.ShowTips = chkShowOnStart.Checked.GetValueOrDefault();

            if (radFBNone.Checked) {
                fOptions.FileBackup = FileBackup.fbNone;
            } else if (radFBOnlyPrev.Checked) {
                fOptions.FileBackup = FileBackup.fbOnlyPrev;
            } else if (radFBEachRevision.Checked) {
                fOptions.FileBackup = FileBackup.fbEachRevision;
            }

            GKComboItem item = cmbLanguages.Items[cmbLanguages.SelectedIndex] as GKComboItem;
            if (item != null) {
                AppHost.Instance.LoadLanguage((int)item.Tag);
            }

            ancOptionsControl1.AcceptChanges();

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

            fOptions.Geocoder = cmbGeocoder.Text;

            DialogResult = DialogResult.Ok;
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

        private void chkPortraitsVisible_CheckedChanged(object sender, EventArgs e)
        {
            chkDefaultPortraits.Enabled = chkPortraitsVisible.Checked.GetValueOrDefault();
        }

        public void SetPage(OptionsPage page)
        {
            switch (page)
            {
                case OptionsPage.opCommon:
                    PageControl1.SelectedPage = pageCommon;
                    break;

                case OptionsPage.opTreeChart:
                    PageControl1.SelectedPage = pageCharts;
                    tabsCharts.SelectedIndex = 0;
                    break;

                case OptionsPage.opAncestorsCircle:
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

        public void SetLang()
        {
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Title = LangMan.LS(LSID.LSID_MIOptions);
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
            //lblFont.Text = LangMan.LS(LSID.LSID_Font);
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
        }
    }
}
