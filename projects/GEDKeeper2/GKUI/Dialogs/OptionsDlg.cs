/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCommon;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Types;
using GKUI.Controls;

namespace GKUI.Dialogs
{
    public enum OptionsPage
    {
        opCommon, opTreeChart, opAncestorsCircle, opInterface, opPedigree, opMultimedia
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed partial class OptionsDlg : Form, ILocalization
    {
        private readonly IHost fHost;
        private GlobalOptions fOptions;
        private readonly IndividualListColumns fTempColumns;

        public GlobalOptions Options
        {
            get { return fOptions; }
            set { fOptions = value; }
        }

        public OptionsDlg(IHost aHost)
        {
            InitializeComponent();

            btnAccept.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;
            btnColumnUp.Image = GKResources.iUp;
            btnColumnDown.Image = GKResources.iDown;

            fHost = aHost;
            fOptions = GlobalOptions.Instance;
            fTempColumns = new IndividualListColumns();

            SetLang();
            UpdateForm();
        }

        private void UpdateColumnsList()
        {
            lstPersonColumns.ItemCheck -= ListPersonColumns_ItemCheck;
            lstPersonColumns.BeginUpdate();
            try
            {
                lstPersonColumns.Items.Clear();

                int num = fTempColumns.Count;
                for (int i = 0; i < num; i++) {
                    ColumnProps colProps = fTempColumns[i];

                    string colName = LangMan.LS(fTempColumns.ColumnStatics[colProps.ColType].ColName);

                    lstPersonColumns.Items.Add(colName, colProps.ColActive);
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
                cmbLanguages.Items.Add(new GKComboItem(lngRec.Name, (int)lngRec.Code));
            }
            GKUtils.SelectComboItem(cmbLanguages, (int)fOptions.InterfaceLang);
        }

        private void UpdateForm()
        {
            /*switch (fOptions.DefCharacterSet)
            {
                case GEDCOMCharacterSet.csASCII:
                    radASCII.Checked = true;
                    break;
                case GEDCOMCharacterSet.csUTF8:
                    radUTF.Checked = true;
                    break;
            }*/

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
            lblMaleColor.BackColor = fOptions.ChartOptions.MaleColor;
            lblFemaleColor.BackColor = fOptions.ChartOptions.FemaleColor;
            lblUnkSexColor.BackColor = fOptions.ChartOptions.UnkSexColor;
            lblUnHusbandColor.BackColor = fOptions.ChartOptions.UnHusbandColor;
            lblUnWifeColor.BackColor = fOptions.ChartOptions.UnWifeColor;

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

        private void UpdatePlugins()
        {
            if (MainWin.Instance == null) return;

            lvPlugins.Items.Clear();

            foreach (IPlugin plugin in MainWin.Instance.Plugins)
            {
                PluginInfo pInfo = MainWin.GetPluginAttributes(plugin);
                
                ListViewItem item = lvPlugins.Items.Add(pInfo.Title);
                item.SubItems.Add(pInfo.Version);
                item.SubItems.Add(pInfo.Copyright);
                item.SubItems.Add(pInfo.Description);
            }
        }

        private void PanColor_Click(object sender, EventArgs e)
        {
            Label pan = (sender as Label);
            if (pan == null) return;

            ColorDialog1.FullOpen = true;
            ColorDialog1.Color = pan.BackColor;
            if (ColorDialog1.ShowDialog() == DialogResult.OK) {
                pan.BackColor = ColorDialog1.Color;
            }
        }

        private void panDefFont_Click(object sender, EventArgs e)
        {
            TreeChartOptions chartOptions = fOptions.ChartOptions;

            var font = new System.Drawing.Font(chartOptions.DefFontName, chartOptions.DefFontSize);
            font = UIHelper.SelectFont(font);
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
                DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                fHost.LogWrite("OptionsDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
            }
        }

        private void AcceptChanges()
        {
            fTempColumns.CopyTo(fOptions.IndividualListColumns);

            /*if (radASCII.Checked) {
                fOptions.DefCharacterSet = GEDCOMCharacterSet.csASCII;
            } else if (radUTF.Checked) {
                fOptions.DefCharacterSet = GEDCOMCharacterSet.csUTF8;
            }*/

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

            fOptions.PlacesWithAddress = chkPlacesWithAddress.Checked;
            fOptions.ListHighlightUnparentedPersons = chkHighlightUnparented.Checked;
            fOptions.ListHighlightUnmarriedPersons = chkHighlightUnmarried.Checked;
            fOptions.ShowDatesCalendar = chkShowDatesCalendar.Checked;
            fOptions.ShowDatesSign = chkShowDatesSigns.Checked;
            fOptions.Autosave = chkAutosave.Checked;
            fOptions.AutosaveInterval = (int)numASMin.Value;
            fOptions.RemovableMediaWarning = chkRemovableMediaWarning.Checked;
            fOptions.LoadRecentFiles = chkLoadRecentFiles.Checked;
            fOptions.EmbeddedMediaPlayer = chkEmbeddedMediaPlayer.Checked;
            fOptions.AllowMediaStoreReferences = chkAllowMediaDirectRefs.Checked;

            fOptions.ChartOptions.FamilyVisible = chkSurname.Checked;
            fOptions.ChartOptions.NameVisible = chkName.Checked;
            fOptions.ChartOptions.PatronymicVisible = chkPatronymic.Checked;
            fOptions.ChartOptions.DiffLines = chkDiffLines.Checked;
            fOptions.ChartOptions.BirthDateVisible = chkBirthDate.Checked;
            fOptions.ChartOptions.DeathDateVisible = chkDeathDate.Checked;
            fOptions.ChartOptions.OnlyYears = chkOnlyYears.Checked;
            fOptions.ChartOptions.Kinship = chkKinship.Checked;
            fOptions.ChartOptions.SignsVisible = chkSignsVisible.Checked;
            fOptions.ChartOptions.ChildlessExclude = chkChildlessExclude.Checked;
            fOptions.ChartOptions.Decorative = chkTreeDecorative.Checked;
            fOptions.ChartOptions.PortraitsVisible = chkPortraitsVisible.Checked;
            fOptions.ChartOptions.DefaultPortraits = chkDefaultPortraits.Checked;
            fOptions.ChartOptions.MaleColor = lblMaleColor.BackColor;
            fOptions.ChartOptions.FemaleColor = lblFemaleColor.BackColor;
            fOptions.ChartOptions.UnkSexColor = lblUnkSexColor.BackColor;
            fOptions.ChartOptions.UnHusbandColor = lblUnHusbandColor.BackColor;
            fOptions.ChartOptions.UnWifeColor = lblUnWifeColor.BackColor;

            fOptions.Proxy.UseProxy = chkUseProxy.Checked;
            fOptions.Proxy.Server = txtProxyServer.Text;
            fOptions.Proxy.Port = txtProxyPort.Text;
            fOptions.Proxy.Login = txtProxyLogin.Text;
            fOptions.Proxy.Password = txtProxyPass.Text;

            fOptions.PedigreeOptions.IncludeAttributes = chkAttributes.Checked;
            fOptions.PedigreeOptions.IncludeNotes = chkNotes.Checked;
            fOptions.PedigreeOptions.IncludeSources = chkSources.Checked;
            fOptions.PedigreeOptions.IncludeGenerations = chkGenerations.Checked;

            if (radExcess.Checked) {
                fOptions.PedigreeOptions.Format = PedigreeFormat.Excess;
            } else if (radCompact.Checked) {
                fOptions.PedigreeOptions.Format = PedigreeFormat.Compact;
            }

            fOptions.ShowTips = chkShowOnStart.Checked;

            if (radFBNone.Checked) {
                fOptions.FileBackup = FileBackup.fbNone;
            } else if (radFBOnlyPrev.Checked) {
                fOptions.FileBackup = FileBackup.fbOnlyPrev;
            } else if (radFBEachRevision.Checked) {
                fOptions.FileBackup = FileBackup.fbEachRevision;
            }

            GKComboItem item = cmbLanguages.Items[cmbLanguages.SelectedIndex] as GKComboItem;
            if (item != null) {
                MainWin.Instance.LoadLanguage((int)item.Tag);
            }

            ancOptionsControl1.AcceptChanges();

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

            fOptions.Geocoder = cmbGeocoder.Text;

            DialogResult = DialogResult.OK;
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
            bool cs = (e.NewValue == CheckState.Checked);
            fTempColumns[e.Index].ColActive = cs;
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

                case OptionsPage.opAncestorsCircle:
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
            /*grpEncoding.Text = LangMan.LS(LSID.LSID_SaveCoding);*/

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
        }
    }
}
