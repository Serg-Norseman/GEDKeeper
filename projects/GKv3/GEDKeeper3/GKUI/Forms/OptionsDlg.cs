/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Threading.Tasks;
using Eto.Drawing;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
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
        private Scrollable panMaleColor;
        private Scrollable panFemaleColor;
        private Scrollable panUnkSexColor;
        private Scrollable panUnHusbandColor;
        private Scrollable panUnWifeColor;
        private TabPage pageUIView;
        private TabControl PageControl2;
        private Button btnResetDefaults;
        private GroupBox rgFNPFormat;
        private GroupBox grpDateFormat;
        private TabPage pagePedigree;
        private GroupBox grpPedigreeFormat;
        private TabControl tabsCharts;
        private TabPage pageCharts;
        private GroupBox grpFileBackup;
        private GKUI.Components.ACOptionsControl ancOptionsControl1;
        private GroupBox grpAdvancedNames;
        private TabPage pageMultimedia;
        private TabPage pageEventTypes;
        private GKSheetList slEventTypes;
        private TabControl PageControlRep;
        private TabPage pageReports;
        private TabPage pageFamilyBook;

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
            fController.UpdateView();

            chkSeparateDepth_CheckedChanged(null, null);
        }

        private void PageControl_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (PageControl1.SelectedPage == pageEventTypes) {
                fController.ChangeTab();
            }

            btnResetDefaults.Enabled = PageControl1.SelectedIndex < 7;
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

        private async void panChartFont_Click(object sender, EventArgs e)
        {
            await ChangeFont(fController.Options.TreeChartOptions);
            fController.UpdateTreeChartFont();
        }

        private async void panUIFont_Click(object sender, EventArgs e)
        {
            await ChangeFont(fController.Options);
            fController.UpdateUIFont();
        }

        private async Task ChangeFont(IFontOptions opts)
        {
            var sdFont = new Font(opts.DefFontName, opts.DefFontSize);
            IFont font = new FontHandler(sdFont);
            font = await AppHost.StdDialogs.SelectFont(font);
            if (font != null) {
                opts.DefFontName = font.Name;
                opts.DefFontSize = (int)(Math.Round(font.Size));
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
                    // reports
                    switch (PageControlRep.SelectedIndex) {
                        case 0:
                            fController.ResetPedigreesOptions();
                            break;
                        case 1:
                            fController.ResetFamilyBookOptions();
                            break;
                    }
                    break;

                case 5:
                case 6:
                    // specials and gedcom
                    fController.ResetSpecialsOptions();
                    break;

                case 7:
                    // event types
                    break;

                case 8:
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
                    PageControl1.SelectedPage = pageReports;
                    PageControlRep.SelectedPage = pagePedigree;
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

        private void chkOtherOption_CheckedChanged(object sender, EventArgs e)
        {
            fController.ChangeOtherOption();
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
            if (component is Control control) {
                string compName = control.ID; // FIXME: change x:Name to ID for ACOptionsControl.xeto
                if (compName == "acbLine" || compName == "acbBack" || compName == "acbText" || compName == "acb7" || compName == "acb6" ||
                    compName == "acb5" || compName == "acb4" || compName == "acb3" || compName == "acb2" || compName == "acb1" ||
                    compName == "acb0") {
                    return true;
                }
            }

            return false;
        }

        private void chkExtBackupEnabled_CheckedChanged(object sender, EventArgs e)
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
