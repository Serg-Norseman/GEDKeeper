/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKUI.Components;
using GKUI.Platform.Handlers;

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
        private Button btnResetDefaults;
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
        private Label lblCertaintyAlgorithm;
        private ComboBox cmbCertaintyAlgorithm;
        private CheckBox chkTreeSurnameFirst;
        private CheckBox chkURNotesVisible;
        private CheckBox chkSAFByAllNames;
        private CheckBox chkShortenDateRanges;
        private CheckBox chkExtendedKinships;
        private CheckBox chkUseSurnamesInPSF;
        private CheckBox chkUseBirthDatesInPSF;
        private Label lblDescendNumbering;
        private ComboBox cmbDescendNumbering;
        private Label lblAscendNumbering;
        private ComboBox cmbAscendNumbering;
        private CheckBox chkExtendedLocations;
        private CheckBox chkELAbbreviatedNames;
        private CheckBox chkReversePlacesOrder;
        private TabPage pageEventTypes;
        private GKSheetList slEventTypes;
        private CheckBox chkFullNameOnOneLine;
        private Label lblMatchPatternMethod;
        private ComboBox cmbMatchPatternMethod;
        private CheckBox chkSourcePages;
        private TabPage pageNavigation;
        private CheckBox chkPortraits;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion


        #region View Interface

        ISheetList IOptionsDlg.EventTypesList
        {
            get { return slEventTypes; }
        }

        #endregion


        public OptionsDlg(IHost host)
        {
            XamlReader.Load(this);

            PageControl1.SelectedIndexChanged += PageControl_SelectedIndexChanged;

            UIHelper.FixRadioButtons(this, grpFileBackup);
            UIHelper.FixRadioButtons(this, rgFNPFormat);
            UIHelper.FixRadioButtons(this, grpDateFormat);
            UIHelper.FixRadioButtons(this, grpAdvancedNames);
            UIHelper.FixRadioButtons(this, grpPedigreeFormat);

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

        private void PageControl_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (PageControl1.SelectedPage == pageEventTypes) {
                fController.ChangeTab();
            }

            btnResetDefaults.Enabled = PageControl1.SelectedIndex < 6;
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

        private async void panDefFont_Click(object sender, EventArgs e)
        {
            TreeChartOptions chartOptions = fController.Options.TreeChartOptions;

            var sdFont = new Font(chartOptions.DefFontName, chartOptions.DefFontSize);
            IFont font = new FontHandler(sdFont);
            font = await AppHost.StdDialogs.SelectFont(font);
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

        private void btnResetDefaults_Click(object sender, EventArgs e)
        {
            switch (PageControl1.SelectedIndex) {
                case 0:
                    // common
                    fController.ResetCommonOptions();
                    break;

                case 1:
                    // multimedia
                    fController.ResetMediaOptions();
                    break;

                case 2:
                    // charts
                    switch (tabsCharts.SelectedIndex) {
                        case 0:
                            // tree
                            fController.ResetTreeChartsOptions();
                            break;
                        case 1:
                            // circle
                            fController.ResetCircleChartsOptions();
                            break;
                    }
                    break;

                case 3:
                    // interface
                    switch (PageControl2.SelectedIndex) {
                        case 0:
                            // all lists
                            fController.ResetInterfaceOptions();
                            break;
                        case 1:
                            // individuals
                            fController.ResetColumnsList();
                            break;
                    }
                    break;

                case 4:
                    // pedigrees
                    fController.ResetPedigreesOptions();
                    break;

                case 5:
                    // specials
                    fController.ResetSpecialsOptions();
                    break;

                case 6:
                    // plugins
                    break;
            }
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
            // prevent triggering on incomplete initialization
            if (fController != null) {
                fController.ChangeFNPFormat();
            }
        }

        public override bool SkipTheme(IDisposable component)
        {
            if (component == panMaleColor || component == panFemaleColor || component == panUnkSexColor || component == panUnHusbandColor || component == panUnWifeColor) {
                return true;
            }

            // FIXME: temporariry hack for AncCircleOptions control
            if (component is Control) {
                var control = (Control)component;
                string compName = control.ID; // FIXME: change x:Name to ID for ACOptionsControl.xeto
                if (compName == "acbLine" || compName == "acbBack" || compName == "acbText" || compName == "acb7" || compName == "acb6" ||
                    compName == "acb5" || compName == "acb4" || compName == "acb3" || compName == "acb2" || compName == "acb1" ||
                    compName == "acb0") {
                    return true;
                }
            }

            return false;
        }
    }
}
