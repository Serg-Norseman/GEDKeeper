/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Drawing;
using System.Windows.Forms;

using GKCommon.GEDCOM;
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
        opCommon, opTreeChart, opAncestorsCircle, opInterface, opPedigree
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
            get { return this.fOptions; }
            set { this.fOptions = value; }
        }

        public OptionsDlg(IHost aHost)
        {
            this.InitializeComponent();

            this.btnAccept.Image = (Image)MainWin.ResourceManager.GetObjectEx("iBtnAccept");
            this.btnCancel.Image = (Image)MainWin.ResourceManager.GetObjectEx("iBtnCancel");
            this.btnColumnUp.Image = (Image)MainWin.ResourceManager.GetObjectEx("iUp");
            this.btnColumnDown.Image = (Image)MainWin.ResourceManager.GetObjectEx("iDown");

            this.fHost = aHost;
            this.fOptions = MainWin.Instance.Options;
            this.fTempColumns = new IndividualListColumns();

            this.SetLang();
            this.UpdateForm();
        }

        private void UpdateColumnsList()
        {
            this.lstPersonColumns.ItemCheck -= this.ListPersonColumns_ItemCheck;
            this.lstPersonColumns.BeginUpdate();
            try
            {
                this.lstPersonColumns.Items.Clear();

                int num = this.fTempColumns.Count;
                for (int i = 0; i < num; i++) {
                    ColumnProps colProps = fTempColumns[i];
                    
                    string colName = LangMan.LS(fTempColumns.ColumnStatics[colProps.ColType].ColName);

                    this.lstPersonColumns.Items.Add(colName, colProps.ColActive);
                }
            }
            finally
            {
                this.lstPersonColumns.EndUpdate();
            }
            this.lstPersonColumns.ItemCheck += this.ListPersonColumns_ItemCheck;
        }

        private void UpdateControls()
        {
            this.lblChartFont.Text = this.fOptions.ChartOptions.DefFontName + @", " + this.fOptions.ChartOptions.DefFontSize.ToString();
        }

        private void UpdateLangs()
        {
            this.cmbLanguages.Items.Clear();
            this.cmbLanguages.Items.Add(new GKComboItem(LangMan.LS_DEF_NAME, LangMan.LS_DEF_CODE));

            int idx = 0;
            int num = this.fOptions.GetLangsCount() - 1;
            for (int i = 0; i <= num; i++)
            {
                LangRecord lngRec = this.fOptions.GetLang(i);
                if (this.fOptions.InterfaceLang == lngRec.Code)
                {
                    idx = i + 1;
                }
                this.cmbLanguages.Items.Add(new GKComboItem(lngRec.Name, (int)lngRec.Code));
            }
            this.cmbLanguages.SelectedIndex = idx;
        }

        private void UpdateForm()
        {
            switch (this.fOptions.DefCharacterSet)
            {
                case GEDCOMCharacterSet.csASCII:
                    this.radASCII.Checked = true;
                    break;
                case GEDCOMCharacterSet.csUTF8:
                    this.radUTF.Checked = true;
                    break;
            }

            switch (this.fOptions.DefNameFormat)
            {
                case NameFormat.nfFNP:
                    this.radSNP.Checked = true;
                    break;
                case NameFormat.nfF_NP:
                    this.radS_NP.Checked = true;
                    break;
                case NameFormat.nfF_N_P:
                    this.radS_N_P.Checked = true;
                    break;
            }

            switch (this.fOptions.DefDateFormat)
            {
                case DateFormat.dfDD_MM_YYYY:
                    this.radDMY.Checked = true;
                    break;
                case DateFormat.dfYYYY_MM_DD:
                    this.radYMD.Checked = true;
                    break;
            }

            this.chkPlacesWithAddress.Checked = this.fOptions.PlacesWithAddress;
            this.chkHighlightUnparented.Checked = this.fOptions.ListHighlightUnparentedPersons;
            this.chkHighlightUnmarried.Checked = this.fOptions.ListHighlightUnmarriedPersons;
            this.chkSurname.Checked = this.fOptions.ChartOptions.FamilyVisible;
            this.chkName.Checked = this.fOptions.ChartOptions.NameVisible;
            this.chkPatronymic.Checked = this.fOptions.ChartOptions.PatronymicVisible;
            this.chkDiffLines.Checked = this.fOptions.ChartOptions.DiffLines;
            this.chkBirthDate.Checked = this.fOptions.ChartOptions.BirthDateVisible;
            this.chkDeathDate.Checked = this.fOptions.ChartOptions.DeathDateVisible;
            this.chkOnlyYears.Checked = this.fOptions.ChartOptions.OnlyYears;
            this.chkKinship.Checked = this.fOptions.ChartOptions.Kinship;
            this.chkSignsVisible.Checked = this.fOptions.ChartOptions.SignsVisible;
            this.chkChildlessExclude.Checked = this.fOptions.ChartOptions.ChildlessExclude;
            this.chkTreeDecorative.Checked = this.fOptions.ChartOptions.Decorative;
            this.chkPortraitsVisible.Checked = this.fOptions.ChartOptions.PortraitsVisible;
            this.lblMaleColor.BackColor = this.fOptions.ChartOptions.MaleColor;
            this.lblFemaleColor.BackColor = this.fOptions.ChartOptions.FemaleColor;
            this.lblUnkSexColor.BackColor = this.fOptions.ChartOptions.UnkSexColor;
            this.lblUnHusbandColor.BackColor = this.fOptions.ChartOptions.UnHusbandColor;
            this.lblUnWifeColor.BackColor = this.fOptions.ChartOptions.UnWifeColor;
            this.chkUseProxy.Checked = this.fOptions.Proxy.UseProxy;
            this.txtProxyServer.Text = this.fOptions.Proxy.Server;
            this.txtProxyPort.Text = this.fOptions.Proxy.Port;
            this.txtProxyLogin.Text = this.fOptions.Proxy.Login;
            this.txtProxyPass.Text = this.fOptions.Proxy.Password;
            this.chkAttributes.Checked = this.fOptions.PedigreeOptions.IncludeAttributes;
            this.chkNotes.Checked = this.fOptions.PedigreeOptions.IncludeNotes;
            this.chkSources.Checked = this.fOptions.PedigreeOptions.IncludeSources;
            this.chkShowDatesCalendar.Checked = this.fOptions.ShowDatesCalendar;
            this.chkShowDatesSigns.Checked = this.fOptions.ShowDatesSign;

            switch (this.fOptions.PedigreeOptions.Format)
            {
                case PedigreeFormat.Excess:
                    this.radExcess.Checked = true;
                    break;
                case PedigreeFormat.Compact:
                    this.radCompact.Checked = true;
                    break;
            }

            this.chkShowOnStart.Checked = this.fOptions.ShowTips;
            this.chkRevisionsBackup.Checked = this.fOptions.RevisionsBackup;

            this.fOptions.IndividualListColumns.CopyTo(fTempColumns);
            this.UpdateColumnsList();

            this.UpdateControls();
            this.UpdateLangs();
            
            this.UpdatePlugins();
        }

        private void UpdatePlugins()
        {
            this.lvPlugins.Items.Clear();
            
            foreach (IPlugin plugin in MainWin.Instance.Plugins)
            {
                PluginInfo pInfo = MainWin.Instance.GetPluginAttributes(plugin);
                
                ListViewItem item = this.lvPlugins.Items.Add(pInfo.Title);
                item.SubItems.Add(pInfo.Version);
                item.SubItems.Add(pInfo.Copyright);
                item.SubItems.Add(pInfo.Description);
            }
        }

        private void PanColor_Click(object sender, EventArgs e)
        {
            Label pan = (sender as Label);
            if (pan == null) return;

            this.ColorDialog1.FullOpen = true;
            this.ColorDialog1.Color = pan.BackColor;
            if (this.ColorDialog1.ShowDialog() == DialogResult.OK) {
                pan.BackColor = this.ColorDialog1.Color;
            }
        }

        private void PanDefFont_Click(object sender, EventArgs e)
        {
            TreeChartOptions chartOptions = this.fOptions.ChartOptions;

            this.FontDialog1.Font = new System.Drawing.Font(chartOptions.DefFontName, chartOptions.DefFontSize);

            if (this.FontDialog1.ShowDialog() == DialogResult.OK)
            {
                chartOptions.DefFontName = this.FontDialog1.Font.Name;
                chartOptions.DefFontSize = (int)(Math.Round(this.FontDialog1.Font.Size));
            }
            this.UpdateControls();
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                this.AcceptChanges();
                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fHost.LogWrite("OptionsDlg.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        private void AcceptChanges()
        {
            this.fTempColumns.CopyTo(this.fOptions.IndividualListColumns);

            if (this.radASCII.Checked)
            {
                this.fOptions.DefCharacterSet = GEDCOMCharacterSet.csASCII;
            }
            else if (this.radUTF.Checked)
            {
                this.fOptions.DefCharacterSet = GEDCOMCharacterSet.csUTF8;
            }

            if (this.radSNP.Checked)
            {
                this.fOptions.DefNameFormat = NameFormat.nfFNP;
            }
            else if (this.radS_NP.Checked)
            {
                this.fOptions.DefNameFormat = NameFormat.nfF_NP;
            }
            else if (this.radS_N_P.Checked)
            {
                this.fOptions.DefNameFormat = NameFormat.nfF_N_P;
            }

            if (this.radDMY.Checked)
            {
                this.fOptions.DefDateFormat = DateFormat.dfDD_MM_YYYY;
            }
            else if (this.radYMD.Checked)
            {
                this.fOptions.DefDateFormat = DateFormat.dfYYYY_MM_DD;
            }

            this.fOptions.PlacesWithAddress = this.chkPlacesWithAddress.Checked;
            this.fOptions.ListHighlightUnparentedPersons = this.chkHighlightUnparented.Checked;
            this.fOptions.ListHighlightUnmarriedPersons = this.chkHighlightUnmarried.Checked;
            this.fOptions.ChartOptions.FamilyVisible = this.chkSurname.Checked;
            this.fOptions.ChartOptions.NameVisible = this.chkName.Checked;
            this.fOptions.ChartOptions.PatronymicVisible = this.chkPatronymic.Checked;
            this.fOptions.ChartOptions.DiffLines = this.chkDiffLines.Checked;
            this.fOptions.ChartOptions.BirthDateVisible = this.chkBirthDate.Checked;
            this.fOptions.ChartOptions.DeathDateVisible = this.chkDeathDate.Checked;
            this.fOptions.ChartOptions.OnlyYears = this.chkOnlyYears.Checked;
            this.fOptions.ChartOptions.Kinship = this.chkKinship.Checked;
            this.fOptions.ChartOptions.SignsVisible = this.chkSignsVisible.Checked;
            this.fOptions.ChartOptions.ChildlessExclude = this.chkChildlessExclude.Checked;
            this.fOptions.ChartOptions.Decorative = this.chkTreeDecorative.Checked;
            this.fOptions.ChartOptions.PortraitsVisible = this.chkPortraitsVisible.Checked;
            this.fOptions.ChartOptions.MaleColor = this.lblMaleColor.BackColor;
            this.fOptions.ChartOptions.FemaleColor = this.lblFemaleColor.BackColor;
            this.fOptions.ChartOptions.UnkSexColor = this.lblUnkSexColor.BackColor;
            this.fOptions.ChartOptions.UnHusbandColor = this.lblUnHusbandColor.BackColor;
            this.fOptions.ChartOptions.UnWifeColor = this.lblUnWifeColor.BackColor;
            this.fOptions.Proxy.UseProxy = this.chkUseProxy.Checked;
            this.fOptions.Proxy.Server = this.txtProxyServer.Text;
            this.fOptions.Proxy.Port = this.txtProxyPort.Text;
            this.fOptions.Proxy.Login = this.txtProxyLogin.Text;
            this.fOptions.Proxy.Password = this.txtProxyPass.Text;
            this.fOptions.PedigreeOptions.IncludeAttributes = this.chkAttributes.Checked;
            this.fOptions.PedigreeOptions.IncludeNotes = this.chkNotes.Checked;
            this.fOptions.PedigreeOptions.IncludeSources = this.chkSources.Checked;
            this.fOptions.ShowDatesCalendar = this.chkShowDatesCalendar.Checked;
            this.fOptions.ShowDatesSign = this.chkShowDatesSigns.Checked;

            if (this.radExcess.Checked)
            {
                this.fOptions.PedigreeOptions.Format = PedigreeFormat.Excess;
            }
            else if (this.radCompact.Checked)
            {
                this.fOptions.PedigreeOptions.Format = PedigreeFormat.Compact;
            }

            this.fOptions.ShowTips = this.chkShowOnStart.Checked;
            this.fOptions.RevisionsBackup = this.chkRevisionsBackup.Checked;

            GKComboItem item = this.cmbLanguages.Items[this.cmbLanguages.SelectedIndex] as GKComboItem;
            if (item != null) {
                MainWin.Instance.LoadLanguage((int)item.Data);
            }

            base.DialogResult = DialogResult.OK;
        }

        private void btnColumnUp_Click(object sender, EventArgs e)
        {
            int idx = this.lstPersonColumns.SelectedIndex;
            if (this.fTempColumns.MoveColumn(idx, true)) {
                this.UpdateColumnsList();
                this.lstPersonColumns.SelectedIndex = idx - 1;
            }
        }

        private void btnColumnDown_Click(object sender, EventArgs e)
        {
            int idx = this.lstPersonColumns.SelectedIndex;
            if (this.fTempColumns.MoveColumn(idx, false)) {
                this.UpdateColumnsList();
                this.lstPersonColumns.SelectedIndex = idx + 1;
            }
        }

        private void btnDefList_Click(object sender, EventArgs e)
        {
            this.fTempColumns.ResetDefaults();
            this.UpdateColumnsList();
        }

        private void ListPersonColumns_ItemCheck(object sender, ItemCheckEventArgs e)
        {
            bool cs = (e.NewValue == CheckState.Checked);
            ColumnProps props = this.fTempColumns[e.Index];
            props.ColActive = cs;
            this.fTempColumns[e.Index] = props;
        }

        public void SetPage(OptionsPage page)
        {
            switch (page) {
                case OptionsPage.opCommon:
                    this.PageControl1.SelectTab(0);
                    break;
                case OptionsPage.opTreeChart:
                    this.PageControl1.SelectTab(1);
                    this.tabsCharts.SelectTab(0);
                    break;
                case OptionsPage.opAncestorsCircle:
                    this.PageControl1.SelectTab(1);
                    this.tabsCharts.SelectTab(1);
                    break;
                case OptionsPage.opInterface:
                    this.PageControl1.SelectTab(2);
                    break;
                case OptionsPage.opPedigree:
                    this.PageControl1.SelectTab(3);
                    break;
            }
        }

        public void SetLang()
        {
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Text = LangMan.LS(LSID.LSID_MIOptions);
            this.pageCommon.Text = LangMan.LS(LSID.LSID_Common);
            this.pageUIView.Text = LangMan.LS(LSID.LSID_Interface);
            this.pageTreeChart.Text = LangMan.LS(LSID.LSID_Trees);
            this.pagePedigree.Text = LangMan.LS(LSID.LSID_Pedigrees);
            this.grpEncoding.Text = LangMan.LS(LSID.LSID_SaveCoding);

            this.grpInternet.Text = LangMan.LS(LSID.LSID_Internet);
            this.chkUseProxy.Text = LangMan.LS(LSID.LSID_ProxyUse);
            this.lblProxyServer.Text = LangMan.LS(LSID.LSID_ProxyServer);
            this.lblProxyPort.Text = LangMan.LS(LSID.LSID_ProxyPort);
            this.lblProxyLogin.Text = LangMan.LS(LSID.LSID_ProxyLogin);
            this.lblProxyPassword.Text = LangMan.LS(LSID.LSID_Password);

            this.grpOther.Text = LangMan.LS(LSID.LSID_Other);

            this.chkShowOnStart.Text = LangMan.LS(LSID.LSID_StartupTips);
            this.lblLanguage.Text = LangMan.LS(LSID.LSID_Language);
            this.pageViewCommon.Text = LangMan.LS(LSID.LSID_ListsAll);
            this.pageViewPersons.Text = LangMan.LS(LSID.LSID_ListPersons);
            this.rgFNPFormat.Text = LangMan.LS(LSID.LSID_NamesFormat);
            this.radSNP.Text = LangMan.LS(LSID.LSID_NF1);
            this.radS_NP.Text = LangMan.LS(LSID.LSID_NF2);
            this.radS_N_P.Text = LangMan.LS(LSID.LSID_NF3);
            this.grpDateFormat.Text = LangMan.LS(LSID.LSID_DateFormat);
            this.chkPlacesWithAddress.Text = LangMan.LS(LSID.LSID_PlacesWithAddress);
            this.chkHighlightUnparented.Text = LangMan.LS(LSID.LSID_HighlightUnparented);
            this.chkHighlightUnmarried.Text = LangMan.LS(LSID.LSID_HighlightUnmarried);
            this.btnDefList.Text = LangMan.LS(LSID.LSID_DefList);
            this.grpTreePersons.Text = LangMan.LS(LSID.LSID_ViewTree);
            this.chkSurname.Text = LangMan.LS(LSID.LSID_Surname);
            this.chkName.Text = LangMan.LS(LSID.LSID_Name);
            this.chkPatronymic.Text = LangMan.LS(LSID.LSID_Patronymic);
            this.chkDiffLines.Text = LangMan.LS(LSID.LSID_DiffLines);
            this.chkBirthDate.Text = LangMan.LS(LSID.LSID_BirthDate);
            this.chkDeathDate.Text = LangMan.LS(LSID.LSID_DeathDate);
            this.chkOnlyYears.Text = LangMan.LS(LSID.LSID_OnlyYears);
            this.chkKinship.Text = LangMan.LS(LSID.LSID_Kinship);
            this.chkSignsVisible.Text = LangMan.LS(LSID.LSID_SignsVisible);
            this.chkTreeDecorative.Text = LangMan.LS(LSID.LSID_TreeDecorative);
            this.chkPortraitsVisible.Text = LangMan.LS(LSID.LSID_PortraitsVisible);
            this.chkChildlessExclude.Text = LangMan.LS(LSID.LSID_ChildlessExclude);
            this.grpTreeDecor.Text = LangMan.LS(LSID.LSID_Decor);
            this.lblMaleColor.Text = LangMan.LS(LSID.LSID_Man);
            this.lblFemaleColor.Text = LangMan.LS(LSID.LSID_Woman);
            this.lblUnkSexColor.Text = LangMan.LS(LSID.LSID_UnkSex);
            this.lblUnHusbandColor.Text = LangMan.LS(LSID.LSID_UnHusband);
            this.lblUnWifeColor.Text = LangMan.LS(LSID.LSID_UnWife);
            this.lblFont.Text = LangMan.LS(LSID.LSID_Font);
            this.grpPedigree.Text = LangMan.LS(LSID.LSID_PedigreeGen);
            this.chkAttributes.Text = LangMan.LS(LSID.LSID_IncludeAttributes);
            this.chkNotes.Text = LangMan.LS(LSID.LSID_IncludeNotes);
            this.chkSources.Text = LangMan.LS(LSID.LSID_IncludeSources);
            this.grpPedigreeFormat.Text = LangMan.LS(LSID.LSID_PedigreeFormat);
            this.radExcess.Text = LangMan.LS(LSID.LSID_PF1);
            this.radCompact.Text = LangMan.LS(LSID.LSID_PF2);
            this.chkRevisionsBackup.Text = LangMan.LS(LSID.LSID_RevisionsBackup);
            this.pageCharts.Text = LangMan.LS(LSID.LSID_Charts);
            this.pagePlugins.Text = LangMan.LS(LSID.LSID_Plugins);
            this.chkShowDatesCalendar.Text = LangMan.LS(LSID.LSID_ShowDatesCalendar);
            this.chkShowDatesSigns.Text = LangMan.LS(LSID.LSID_ShowDatesSigns);
        }
    }
}
