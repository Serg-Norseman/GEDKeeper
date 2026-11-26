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

        private GKAccordion accCategories;
        private WizardPages wizPages;
        private WizardPage pageComOther;
        private WizardPage pageComBackup;
        private WizardPage pageGEDCOM;
        private WizardPage pageMultimedia;
        private WizardPage pageTreePersons;
        private WizardPage pageTreeDesign;
        private WizardPage pageAncCircle;
        private WizardPage pageViewCommon;
        private WizardPage pageNames;
        private WizardPage pageViewPersons;
        private WizardPage pageNavigation;
        private WizardPage pageGeo;
        private WizardPage pagePedigree;
        private WizardPage pageFamilyBook;
        private WizardPage pageSpecials;
        private WizardPage pageEventTypes;
        private WizardPage pagePlugins;
        private GKSwatchPanel panMaleColor;
        private GKSwatchPanel panFemaleColor;
        private GKSwatchPanel panUnkSexColor;
        private GKSwatchPanel panUnHusbandColor;
        private GKSwatchPanel panUnWifeColor;
        private Button btnResetDefaults;
        private GroupBox rgFNPFormat;
        private GroupBox grpDateFormat;
        private GroupBox grpPedigreeFormat;
        private GroupBox grpFileBackup;
        private GroupBox grpAdvancedNames;
        private GKUI.Components.ACOptionsControl ancOptionsControl1;
        private GKSheetList slEventTypes;

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

            UIHelper.FixRadioButtons(this, grpFileBackup);
            UIHelper.FixRadioButtons(this, rgFNPFormat);
            UIHelper.FixRadioButtons(this, grpDateFormat);
            UIHelper.FixRadioButtons(this, grpAdvancedNames);
            UIHelper.FixRadioButtons(this, grpPedigreeFormat);

            wizPages.SelectedIndexChanged += PageControl_SelectedIndexChanged;
            accCategories.SelectedItemChanged += AccCategories_SelectedItemChanged;

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
            accCategories.AddItem(pageFamilyBook.Text, pageFamilyBook);

            accCategories.AddGroup(LangMan.LS(LSID.Other));
            accCategories.AddItem(pageSpecials.Text, pageSpecials);
            accCategories.AddItem(pageEventTypes.Text, pageEventTypes);
            accCategories.AddItem(pagePlugins.Text, pagePlugins);

            accCategories.SelectItem(pageComOther);
            wizPages.SelectedPage = pageComOther;
        }

        private void AccCategories_SelectedItemChanged(object sender, EventArgs e)
        {
            var page = accCategories.SelectedItem?.Data as WizardPage;
            if (page != null && wizPages.SelectedPage != page) {
                wizPages.SelectedPage = page;
            }
        }

        private void PageControl_SelectedIndexChanged(object sender, EventArgs e)
        {
            var page = wizPages.SelectedPage;

            if (page == pageEventTypes) {
                fController.ChangeTab();
            }

            btnResetDefaults.Enabled = page != pageEventTypes && page != pagePlugins;
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
            var swatch = (GKSwatchPanel)sender;
            fController.SelectLabColor(fController.GetControlHandler<ILabel>(swatch.Label));
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
            var page = wizPages.SelectedPage;

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
            } else if (page == pageFamilyBook) {
                // family book
                fController.ResetFamilyBookOptions();
            }
        }

        public void SetPage(OptionsPage page)
        {
            WizardPage target;

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
                wizPages.SelectedPage = target;
                accCategories.SelectItem(target);
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
            if (component == accCategories) {
                return true;
            }

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
