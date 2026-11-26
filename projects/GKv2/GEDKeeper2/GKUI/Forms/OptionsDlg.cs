/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Threading.Tasks;
using System.Windows.Forms;
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
            accCategories.SelectedIndexChanged += AccCategories_SelectedIndexChanged;

            lstPersonColumns.CheckBoxes = true;
            numDefaultDepth.Minimum = -1;
            numDefaultDepthAncestors.Minimum = -1;
            numDefaultDepthDescendants.Minimum = -1;

            fController = new OptionsDlgController(this);
            fController.InitView(null);
            fController.UpdateView();

            BuildAccordion();

            chkSeparateDepth_CheckedChanged(null, null);
        }

        private void BuildAccordion()
        {
            // the header texts of the pages are localized by the controller in SetLocale() before this

            accCategories.AddGroup(LangMan.LS(LSID.Common));
            accCategories.AddItem(pageComOther.Text, pageComOther);
            accCategories.AddItem(pageComBackup.Text, pageComBackup);
            accCategories.AddItem(pageGEDCOM.Text, pageGEDCOM);
            accCategories.AddItem(pageMultimedia.Text, pageMultimedia);

            accCategories.AddGroup(LangMan.LS(LSID.Charts));
            accCategories.AddItem(pageTreePersons.Text, pageTreePersons);
            accCategories.AddItem(pageTreeDesign.Text, pageTreeDesign);
            accCategories.AddItem(pageAncCircle.Text, pageAncCircle);

            accCategories.AddGroup(LangMan.LS(LSID.Interface));
            accCategories.AddItem(pageViewCommon.Text, pageViewCommon);
            accCategories.AddItem(pageNames.Text, pageNames);
            accCategories.AddItem(pageViewPersons.Text, pageViewPersons);
            accCategories.AddItem(pageNavigation.Text, pageNavigation);
            accCategories.AddItem(pageGeo.Text, pageGeo);

            accCategories.AddGroup(LangMan.LS(LSID.Reports));
            accCategories.AddItem(pagePedigree.Text, pagePedigree);

            accCategories.AddGroup(LangMan.LS(LSID.Other));
            accCategories.AddItem(pageSpecials.Text, pageSpecials);
            accCategories.AddItem(pageEventTypes.Text, pageEventTypes);
            accCategories.AddItem(pagePlugins.Text, pagePlugins);

            accCategories.SetItemSelected(0);
            PageControl1.SelectedTab = pageComOther;
        }

        private void AccCategories_SelectedIndexChanged(object sender, EventArgs e)
        {
            var item = accCategories.SelectedItem as AccordionItem;
            if (item != null && !item.IsGroup && PageControl1.SelectedTab != (TabPage)item.Data) {
                PageControl1.SelectedTab = (TabPage)item.Data;
            }
        }

        private void PageControl_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (PageControl1.SelectedTab == pageEventTypes) {
                fController.ChangeTab();
            }

            btnResetDefaults.Enabled = PageControl1.SelectedTab != pageEventTypes && PageControl1.SelectedTab != pagePlugins;
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
            fController.SelectLabColor(fController.GetControlHandler<ILabel>(sender as Label));
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
            var sdFont = new System.Drawing.Font(opts.DefFontName, opts.DefFontSize);
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
            var page = PageControl1.SelectedTab;

            if (page == pageComOther || page == pageComBackup) {
                // common, backup
                fController.ResetCommonOptions();
            } else if (page == pageGEDCOM || page == pageSpecials) {
                // gedcom, specials
                fController.ResetSpecialsOptions();
            } else if (page == pageMultimedia) {
                // multimedia
                fController.ResetMediaOptions();
            } else if (page == pageTreePersons || page == pageTreeDesign) {
                // tree charts
                fController.ResetTreeChartsOptions();
            } else if (page == pageAncCircle) {
                // circle
                fController.ResetCircleChartsOptions();
            } else if (page == pageViewCommon || page == pageNames || page == pageNavigation || page == pageGeo) {
                // interface
                fController.ResetInterfaceOptions();
            } else if (page == pageViewPersons) {
                // individuals
                fController.ResetColumnsList();
            } else if (page == pagePedigree) {
                // pedigrees
                fController.ResetPedigreesOptions();
            }
        }

        public void SetPage(OptionsPage page)
        {
            TabPage target;

            switch (page) {
                case OptionsPage.opCommon:
                    target = pageComOther;
                    break;

                case OptionsPage.opTreeChart:
                    target = pageTreePersons;
                    break;

                case OptionsPage.opCircleChart:
                    target = pageAncCircle;
                    break;

                case OptionsPage.opInterface:
                    target = pageViewCommon;
                    break;

                case OptionsPage.opPedigree:
                    target = pagePedigree;
                    break;

                case OptionsPage.opMultimedia:
                    target = pageMultimedia;
                    break;

                default:
                    target = null;
                    break;
            }

            if (target != null) {
                PageControl1.SelectedTab = target;
                accCategories.SelectItem(target);
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
            if (component is Control control) {
                string compName = control.Name;
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
