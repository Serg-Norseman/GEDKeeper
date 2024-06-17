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
using GKCore.Controllers;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKUI.Components;
using Xamarin.Forms;

namespace GKUI.Forms
{
    public sealed partial class OptionsDlg : CommonDialog<IOptionsDlg, OptionsDlgController>, ILocalizable, IOptionsDlg
    {
        #region View Interface

        ISheetList IOptionsDlg.EventTypesList
        {
            get { return slEventTypes; }
        }

        #endregion


        public OptionsDlg(IHost host)
        {
            InitializeComponent();

            UIHelper.UnsetAnimateTransition(PageControl1);
            UIHelper.UnsetAnimateTransition(tabsCharts);
            UIHelper.UnsetAnimateTransition(PageControl2);
            PageControl1.PositionChanged += PageControl1_PositionChanged;

            SetLabelClickEvent(lblMaleColor);
            SetLabelClickEvent(lblFemaleColor);
            SetLabelClickEvent(lblUnkSexColor);
            SetLabelClickEvent(lblUnHusbandColor);
            SetLabelClickEvent(lblUnWifeColor);
            // lblChartFont <- panDefFont_Click

            cmbGeocoder.AddItem("Google");
            cmbGeocoder.AddItem("Yandex");
            cmbGeocoder.AddItem("OSM");

            fController = new OptionsDlgController(this);

            lstPersonColumns.Sorting = false;
            lstPersonColumns.AddCheckedColumn("x", 75);
            lstPersonColumns.AddColumn("Title", 100);

            fController.UpdateView();

            chkSeparateDepth_CheckedChanged(null, null);
        }

        private void SetLabelClickEvent(Label label)
        {
            var tapRecognizer = new TapGestureRecognizer();
            tapRecognizer.Tapped += (s, e) => {
                PanColor_Click(s, e);
            };
            label.GestureRecognizers.Add(tapRecognizer);
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
            //fController.SelectLabColor(GetControlHandler<ILabel>(sender as Label));
        }

        private void panDefFont_Click(object sender, EventArgs e)
        {
            /*TreeChartOptions chartOptions = fController.Options.TreeChartOptions;

            var sdFont = new Font(chartOptions.DefFontName, chartOptions.DefFontSize);
            IFont font = new FontHandler(sdFont);
            font = AppHost.StdDialogs.SelectFont(font);
            if (font != null)
            {
                chartOptions.DefFontName = font.Name;
                chartOptions.DefFontSize = (int)(Math.Round(font.Size));
            }

            fController.UpdateTreeChartFont();*/
        }

        private void btnColumnUp_Click(object sender, EventArgs e)
        {
            fController.MoveColumnUp();
        }

        private void btnColumnDown_Click(object sender, EventArgs e)
        {
            fController.MoveColumnDown();
        }

        private void PageControl1_PositionChanged(object sender, Xam.Plugin.TabView.PositionChangedEventArgs e)
        {
            // FIXME: When switching between tabs multiple times, nested tabs disappear.

            switch (PageControl1.SelectedTabIndex) {
                case 0: // common
                case 1: // multimedia
                    break;

                case 2: // charts
                    //UIHelper.ResetTabViewLayout(tabsCharts); // did not help
                    break;

                case 3: // interface
                    //UIHelper.ResetTabViewLayout(PageControl2); // did not help
                    break;

                case 4: // pedigrees
                case 5: // specials
                case 6: // plugins
                    break;
            }
        }

        private void btnResetDefaults_Click(object sender, EventArgs e)
        {
            switch (PageControl1.SelectedTabIndex) {
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
                    switch (tabsCharts.SelectedTabIndex) {
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
                    switch (PageControl2.SelectedTabIndex) {
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
                    PageControl1.SelectedTabIndex = 0;
                    break;

                case OptionsPage.opTreeChart:
                    PageControl1.SelectedTabIndex = 2;
                    tabsCharts.SelectedTabIndex = 0;
                    break;

                case OptionsPage.opCircleChart:
                    PageControl1.SelectedTabIndex = 2;
                    tabsCharts.SelectedTabIndex = 1;
                    break;

                case OptionsPage.opInterface:
                    PageControl1.SelectedTabIndex = 3;
                    break;

                case OptionsPage.opPedigree:
                    PageControl1.SelectedTabIndex = 4;
                    break;

                case OptionsPage.opMultimedia:
                    PageControl1.SelectedTabIndex = 1;
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
    }
}
