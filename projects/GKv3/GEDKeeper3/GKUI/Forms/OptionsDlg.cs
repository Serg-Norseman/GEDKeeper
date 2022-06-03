﻿/*
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
using BSLib.Design.Graphics;
using BSLib.Design.MVP.Controls;
using Eto.Drawing;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Options;
using GKUI.Components;
using GKUI.Platform;

namespace GKUI.Forms
{
    public sealed partial class OptionsDlg : CommonDialog<IOptionsDlg, OptionsDlgController>, ILocalizable, IOptionsDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private TabControl PageControl1;
        private TabPage pageCommon;
        private Button btnAccept;
        private Button btnCancel;
        private TabPage pageTreeChart;
        private GroupBox grpTreePersons;
        private CheckBox chkSurname;
        private CheckBox chkName;
        private CheckBox chkPatronymic;
        private CheckBox chkDiffLines;
        private CheckBox chkBirthDate;
        private CheckBox chkDeathDate;
        private CheckBox chkKinship;
        private GroupBox grpTreeDecor;
        private Scrollable panMaleColor;
        private Scrollable panFemaleColor;
        private Scrollable panUnkSexColor;
        private Scrollable panUnHusbandColor;
        private Scrollable panUnWifeColor;
        private GroupBox grpInternet;
        private Label lblProxyServer;
        private Label lblProxyPort;
        private Label lblProxyLogin;
        private Label lblProxyPassword;
        private CheckBox chkUseProxy;
        private TextBox txtProxyServer;
        private TextBox txtProxyPort;
        private TextBox txtProxyLogin;
        private PasswordBox txtProxyPass;
        private TabPage pageUIView;
        private TabControl PageControl2;
        private TabPage pageViewCommon;
        private TabPage pageViewPersons;
        private GKListView lstPersonColumns;
        private Button btnColumnUp;
        private Button btnColumnDown;
        private Button btnDefList;
        private GroupBox rgFNPFormat;
        private GroupBox grpDateFormat;
        private CheckBox chkPlacesWithAddress;
        private GroupBox grpOther;
        private CheckBox chkShowOnStart;
        private CheckBox chkHighlightUnparented;
        private CheckBox chkHighlightUnmarried;
        private CheckBox chkOnlyYears;
        private CheckBox chkSignsVisible;
        private CheckBox chkChildlessExclude;
        private Scrollable panDefFont;
        private TabPage pagePedigree;
        private GroupBox grpPedigree;
        private CheckBox chkAttributes;
        private CheckBox chkNotes;
        private CheckBox chkSources;
        private GroupBox grpPedigreeFormat;
        private Label lblLanguage;
        private ComboBox cmbLanguages;
        private CheckBox chkTreeDecorative;
        private CheckBox chkPortraitsVisible;
        private RadioButton radSNP;
        private RadioButton radS_NP;
        private RadioButton radS_N_P;
        private RadioButton radDMY;
        private RadioButton radYMD;
        private RadioButton radExcess;
        private RadioButton radCompact;
        private CheckBox chkShowDatesSigns;
        private CheckBox chkShowDatesCalendar;
        private GKListView lvPlugins;
        private TabPage pagePlugins;
        private Label lblChartFont;
        private TabControl tabsCharts;
        private TabPage pageCharts;
        private Label lblMaleColor;
        private Label lblFemaleColor;
        private Label lblUnkSexColor;
        private Label lblUnHusbandColor;
        private Label lblUnWifeColor;
        private Panel panel1;
        private RadioButton radFBNone;
        private RadioButton radFBOnlyPrev;
        private RadioButton radFBEachRevision;
        private GroupBox grpFileBackup;
        private CheckBox chkAutosave;
        private NumericStepper numASMin;
        private Label lblMinutes;
        private GroupBox groupBox1;
        private CheckBox chkGenerations;
        private GKUI.Components.ACOptionsControl ancOptionsControl1;
        private TabPage pageAncCircle;
        private CheckBox chkExtendWomanSurnames;
        private RadioButton radMaiden_Married;
        private RadioButton radMarried_Maiden;
        private RadioButton radMaiden;
        private RadioButton radMarried;
        private GroupBox grpAdvancedNames;
        private CheckBox chkAllowMediaDirectRefs;
        private CheckBox chkAutoCheckUpdates;
        private TabPage pageMultimedia;
        private CheckBox chkEmbeddedMediaPlayer;
        private CheckBox chkLoadRecentFiles;
        private CheckBox chkRemovableMediaWarning;
        private CheckBox chkDefaultPortraits;
        private CheckBox chkInvertedTree;
        private CheckBox chkMarriagesDates;
        private ComboBox cmbGeocoder;
        private Label lblGeocoder;
        private CheckBox chkShowPlaces;
        private CheckBox chkHideUnknownSpouses;
        private GroupBox grpSpacings;
        private Label lblSpouseDist;
        private Label lblGenDist;
        private Label lblBranchDist;
        private Label lblMargins;
        private NumericStepper numSpouseDist;
        private NumericStepper numGenDist;
        private NumericStepper numBranchDist;
        private NumericStepper numMargins;
        private CheckBox chkAutoSortChildren;
        private CheckBox chkAutoSortSpouses;
        private CheckBox chkCheckTreeSize;
        private CheckBox chkCharsetDetection;
        private Label lblBackupRevisionsMaxCount;
        private NumericStepper numBackupRevisionsMaxCount;
        private CheckBox chkAllowMediaStoreRelativeReferences;
        private Label lblMediaStoreDefault;
        private ComboBox cmbMediaStoreDefault;
        private CheckBox chkAllowDeleteMediaFileFromStgArc;
        private CheckBox chkAllowDeleteMediaFileFromRefs;
        private CheckBox chkDeleteMediaFileWithoutConfirm;
        private CheckBox chkFirstCapitalLetterInNames;
        private CheckBox chkDialogClosingWarn;
        private Label lblGeoSearchCountry;
        private ComboBox cmbGeoSearchCountry;
        private CheckBox chkSeparateDAPLines;
        private CheckBox chkDottedLinesOfAdoptedChildren;
        private CheckBox chkBoldNames;
        private CheckBox chkOnlyLocality;
        private CheckBox chkMinimizingWidth;
        private CheckBox chkShowAge;
        private CheckBox chkShortKinshipForm;
        private CheckBox chkSurnameFirstInOrder;
        private CheckBox chkSurnameInCapitals;
        private CheckBox chkSeparateDepth;
        private Label lblDefaultDepth;
        private NumericStepper numDefaultDepth;
        private Label lblDefaultDepthAncestors;
        private NumericStepper numDefaultDepthAncestors;
        private Label lblDefaultDepthDescendants;
        private NumericStepper numDefaultDepthDescendants;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion


        public OptionsDlg(IHost host)
        {
            XamlReader.Load(this);

            fController = new OptionsDlgController(this);

            lstPersonColumns.Sorting = false;
            lstPersonColumns.AddCheckedColumn("x", 75);
            lstPersonColumns.AddColumn("Title", 100);

            lvPlugins.AddColumn("Title", 75);
            lvPlugins.AddColumn("Version", 100);
            lvPlugins.AddColumn("Copyright", 125);
            lvPlugins.AddColumn("Description", 250);

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
            fController.SelectLabColor(GetControlHandler<ILabel>(sender as Label));
        }

        private void panDefFont_Click(object sender, EventArgs e)
        {
            TreeChartOptions chartOptions = fController.Options.TreeChartOptions;

            var sdFont = new Font(chartOptions.DefFontName, chartOptions.DefFontSize);
            IFont font = new FontHandler(sdFont);
            font = AppHost.StdDialogs.SelectFont(font);
            if (font != null) {
                chartOptions.DefFontName = font.Name;
                chartOptions.DefFontSize = (int)(Math.Round(font.Size));
            }

            fController.UpdateTreeChartFont();
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
            //fTempColumns.OrderedColumns[e.Index].CurActive = e.NewValue;
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
            fController.SetLocale();
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
