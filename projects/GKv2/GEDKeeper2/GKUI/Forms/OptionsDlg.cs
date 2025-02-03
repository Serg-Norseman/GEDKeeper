/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
        #region View Interface

        ISheetList IOptionsDlg.EventTypesList
        {
            get { return slEventTypes; }
        }

        #endregion


        public OptionsDlg(IHost host)
        {
            InitializeComponent();

            PageControl1.SelectedIndexChanged += PageControl_SelectedIndexChanged;

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
            btnColumnUp.Image = UIHelper.LoadResourceImage("Resources.btn_up.gif");
            btnColumnDown.Image = UIHelper.LoadResourceImage("Resources.btn_down.gif");

            fController = new OptionsDlgController(this);

            lstPersonColumns.AddCheckedColumn("Title", 175);

            numDefaultDepth.Minimum = -1;
            numDefaultDepthAncestors.Minimum = -1;
            numDefaultDepthDescendants.Minimum = -1;

            fController.UpdateView();

            chkSeparateDepth_CheckedChanged(null, null);
        }

        private void PageControl_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (PageControl1.SelectedTab == pageEventTypes) {
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

            var sdFont = new System.Drawing.Font(chartOptions.DefFontName, chartOptions.DefFontSize);
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
                    // event types
                    break;

                case 7:
                    // plugins
                    break;
            }
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
                string compName = control.Name;
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
