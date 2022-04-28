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

using System;
using System.Windows.Forms;
using BSLib.Design.Graphics;
using BSLib.Design.Handlers;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Options;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class OptionsDlg : CommonDialog, ILocalizable, IOptionsDlg
    {
        private readonly OptionsDlgController fController;


        public OptionsDlg(IHost host)
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
            btnColumnUp.Image = UIHelper.LoadResourceImage("Resources.btn_up.gif");
            btnColumnDown.Image = UIHelper.LoadResourceImage("Resources.btn_down.gif");

            fController = new OptionsDlgController(this);

            lstPersonColumns.AddCheckedColumn("Title", 175);

            numDefaultDepth.Minimum = -1;
            numDefaultDepthAncestors.Minimum = -1;
            numDefaultDepthDescendants.Minimum = -1;

            SetLocale();

            fController.UpdateView();

            chkSeparateDepth_CheckedChanged(null, null);
        }

        void IOptionsDlg.UpdateCircleChartsOptions()
        {
            ancOptionsControl1.Options = fController.Options.CircleChartOptions;
            ancOptionsControl1.UpdateControls();
        }

        void IOptionsDlg.AcceptCircleChartsOptions()
        {
            ancOptionsControl1.AcceptChanges();
        }

        private void chkExtendWomanSurnames_CheckedChanged(object sender, EventArgs e)
        {
            fController.ChangeExtendWomanSurnames();
        }

        private void PanColor_Click(object sender, EventArgs e)
        {
            Label pan = (sender as Label);
            if (pan == null) return;

            pan.BackColor = UIHelper.ConvertColor(AppHost.StdDialogs.SelectColor(UIHelper.ConvertColor(pan.BackColor)));
        }

        private void panDefFont_Click(object sender, EventArgs e)
        {
            TreeChartOptions chartOptions = fController.Options.TreeChartOptions;

            var sdFont = new System.Drawing.Font(chartOptions.DefFontName, chartOptions.DefFontSize);
            IFont font = new FontHandler(sdFont);
            font = AppHost.StdDialogs.SelectFont(font);
            if (font != null) {
                chartOptions.DefFontName = font.Name;
                chartOptions.DefFontSize = (int)(Math.Round(font.Size));
            }

            fController.UpdateTreeChartFont();
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try {
                fController.AcceptChanges();
                DialogResult = DialogResult.OK;
            } catch (Exception ex) {
                Logger.WriteError("OptionsDlg.btnAccept_Click()", ex);
                DialogResult = DialogResult.None;
            }
        }

        private void btnColumnUp_Click(object sender, EventArgs e)
        {
            fController.MoveColumnUp();
        }

        private void btnColumnDown_Click(object sender, EventArgs e)
        {
            fController.MoveColumnDown();
        }

        private void btnDefList_Click(object sender, EventArgs e)
        {
            fController.ResetColumnsList();
        }

        private void ListPersonColumns_ItemCheck(object sender, ItemCheckEventArgs e)
        {
            //fTempColumns.OrderedColumns[e.Index].CurActive = (e.NewValue == CheckState.Checked);
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
            chkOnlyLocality.Text = LangMan.LS(LSID.LSID_OnlyLocality);
            chkHideUnknownSpouses.Text = LangMan.LS(LSID.LSID_HideUnknownSpouses);
            chkCheckTreeSize.Text = LangMan.LS(LSID.LSID_CheckTreeSize);
            chkDottedLinesOfAdoptedChildren.Text = LangMan.LS(LSID.LSID_DottedLinesOfAdoptedChildren);
            chkBoldNames.Text = LangMan.LS(LSID.LSID_BoldNames);
            chkMinimizingWidth.Text = LangMan.LS(LSID.LSID_MinimizingWidth);
            chkShowAge.Text = LangMan.LS(LSID.LSID_ShowAge);

            grpTreeDecor.Text = LangMan.LS(LSID.LSID_Decor);
            lblMaleColor.Text = LangMan.LS(LSID.LSID_Man);
            lblFemaleColor.Text = LangMan.LS(LSID.LSID_Woman);
            lblUnkSexColor.Text = LangMan.LS(LSID.LSID_UnkSex);
            lblUnHusbandColor.Text = LangMan.LS(LSID.LSID_UnHusband);
            lblUnWifeColor.Text = LangMan.LS(LSID.LSID_UnWife);
            lblFont.Text = LangMan.LS(LSID.LSID_Font);

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
            chkShortKinshipForm.Text = LangMan.LS(LSID.LSID_ShortKinshipForm);
            chkSurnameFirstInOrder.Text = LangMan.LS(LSID.LSID_SurnameFirstInOrder);
            chkSurnameInCapitals.Text = LangMan.LS(LSID.LSID_SurnameInCapitals);

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
            fController.ChangeTreeChartOption();
        }

        private void chkSeparateDepth_CheckedChanged(object sender, EventArgs e)
        {
            fController.ChangeSeparateDepth();
        }

        private void rgFNPFormat_CheckedChanged(object sender, EventArgs e)
        {
            fController.ChangeFNPFormat();
        }
    }
}
