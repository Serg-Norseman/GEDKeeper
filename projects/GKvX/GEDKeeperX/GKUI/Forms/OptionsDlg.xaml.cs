/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore.Options;
using Xamarin.Forms;

namespace GKUI.Forms
{
    public sealed partial class OptionsDlg : CommonDialog<IOptionsDlg, OptionsDlgController>, ILocalizable, IOptionsDlg
    {
        public OptionsDlg(IHost host)
        {
            InitializeComponent();

            SetLabelClickEvent(lblMaleColor);
            SetLabelClickEvent(lblFemaleColor);
            SetLabelClickEvent(lblUnkSexColor);
            SetLabelClickEvent(lblUnHusbandColor);
            SetLabelClickEvent(lblUnWifeColor);
            // lblChartFont <- panDefFont_Click

            /*
            <comcom:GKComboBox x:Name="cmbGeocoder" IsReadOnly="True">
                <ListItem>Google</ListItem>
                <ListItem>Yandex</ListItem>
                <ListItem>OSM</ListItem>
            </comcom:GKComboBox>
             */

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
