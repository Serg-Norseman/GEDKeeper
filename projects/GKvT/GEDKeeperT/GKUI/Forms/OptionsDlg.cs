/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;
using Terminal.Gui;

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

            PageControl1.SelectedTabChanged += PageControl_SelectedIndexChanged;

            //lstPersonColumns.CheckBoxes = true;
            numDefaultDepth.Minimum = -1;
            numDefaultDepthAncestors.Minimum = -1;
            numDefaultDepthDescendants.Minimum = -1;

            fController = new OptionsDlgController(this);
            fController.UpdateView();

            chkSeparateDepth_CheckedChanged(null, false);
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
        }

        void IOptionsDlg.AcceptCircleChartsOptions()
        {
        }

        private void chkExtendWomanSurnames_CheckedChanged(object sender, bool e)
        {
            fController.ChangeExtendWomanSurnames();
        }

        private void PanColor_Click(object sender, EventArgs e)
        {
            fController.SelectLabColor(GetControlHandler<ILabel>(sender as Label));
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
                    break;

                case OptionsPage.opCircleChart:
                    PageControl1.SelectedTab = pageCharts;
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

        private void chkTreeChartOption_CheckedChanged(object sender, bool e)
        {
            fController.ChangeTreeChartOption();
        }

        private void chkSeparateDepth_CheckedChanged(object sender, bool e)
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

        private void chkExtBackupEnabled_CheckedChanged(object sender, bool e)
        {
            // prevent triggering on incomplete initialization
            if (fController != null) {
                fController.CheckExtBackup();
            }
        }

        private void btnExtBackupFolderChoose_Click(object sender, EventArgs e)
        {
            fController.SelectExtBackupFolder();
        }
    }
}
