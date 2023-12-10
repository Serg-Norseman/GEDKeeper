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
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Components;
using GKUI.Forms;

namespace GKPedigreeImporterPlugin
{
    public sealed partial class PedigreeImporterDlg : CommonForm
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private CheckBox chkSpecial_1;
        private GroupBox grpPersonLineSpecials;
        private Label lblDateFormat;
        private ComboBox cbDatesFormat;
        private Label lblDateSeparator;
        private ComboBox cbDateSeparator;
        private CheckBox chkSurnamesNormalize;
        private GroupBox grpConversionParams;
        private Label lblGenerationFormat;
        private ComboBox cbGenerationFormat;
        private Button btnClose;
        private ListBox lbLog;
        private TabPage pageResult;
        private Label lblSurnameFormat;
        private ComboBox cbNameFormat;
        private Label lblPersonLineSeparator;
        private ComboBox cbPersonSeparator;
        private GroupBox grpTextPedigreesParams;
        private RadioButton rbNumsUnknown;
        private RadioButton rbNumsKonovalov;
        private RadioButton rbNumsDAboville;
        private GroupBox grpPersonIdFormat;
        private Button btnBack;
        private Button btnNext;
        private TabPage pageSelect;
        private TabControl tabControl1;
        private Button btnImportFileChoose;
        private TextBox edImportFile;
        private Label lblFile;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly IPlugin fPlugin;
        private readonly ILangMan fLangMan;
        private readonly IBaseWindow fBase;
        private readonly Importer fImporter;
        private int fCurrentStage;
        private int fAvailableStage;

        public PedigreeImporterDlg(IPlugin plugin, IBaseWindow curBase)
        {
            XamlReader.Load(this);

            UIHelper.FixRadioButtons(this, grpPersonIdFormat);

            fPlugin = plugin;
            fLangMan = fPlugin.LangMan;

            cbDateSeparator.Items.AddRange(new string[] { ".", "/", "-"});
            cbDatesFormat.Items.AddRange(new string[] { "DD/MM/YYYY", "YYYY/MM/DD" });
            cbGenerationFormat.Items.AddRange(new string[] { "I, II, III, IV...", fLangMan.LS(PLS.Generation) + " N"});
            cbNameFormat.Items.AddRange(new string[] { fLangMan.LS(PLS.NPS), fLangMan.LS(PLS.SNP) });
            cbPersonSeparator.Items.AddRange(new string[] { fLangMan.LS(PLS.NoSpecial), ";", "," });

            fBase = curBase;
            fImporter = new Importer(this, fBase, fLangMan, lbLog.Items);
            fCurrentStage = 0;
            fAvailableStage = 0;

            cbPersonSeparator.SelectedIndex = 0;
            cbNameFormat.SelectedIndex = 0;
            cbGenerationFormat.SelectedIndex = 0;
            cbDatesFormat.SelectedIndex = 0;
            cbDateSeparator.SelectedIndex = 0;

            // SetLocale()
            Title = fLangMan.LS(PLS.PedigreeImporter);
            lblFile.Text = fLangMan.LS(PLS.File);
            btnImportFileChoose.Text = fLangMan.LS(PLS.DlgSelect) + @"...";

            grpPersonIdFormat.Text = fLangMan.LS(PLS.PersonIdFormat);
            rbNumsDAboville.Text = fLangMan.LS(PLS.NumsDAboville);
            rbNumsKonovalov.Text = fLangMan.LS(PLS.NumsKonovalov);
            rbNumsUnknown.Text = fLangMan.LS(PLS.NumsUnknown);

            grpTextPedigreesParams.Text = fLangMan.LS(PLS.TextPedigreesParams);
            lblPersonLineSeparator.Text = fLangMan.LS(PLS.PersonLineSeparator);
            lblSurnameFormat.Text = fLangMan.LS(PLS.SurnameFormat);
            lblGenerationFormat.Text = fLangMan.LS(PLS.GenerationFormat);
            lblDateSeparator.Text = fLangMan.LS(PLS.DateSeparator);
            lblDateFormat.Text = fLangMan.LS(PLS.DateFormat);

            grpConversionParams.Text = fLangMan.LS(PLS.ConversionParams);
            chkSurnamesNormalize.Text = fLangMan.LS(PLS.SurnamesNormalize);

            btnNext.Text = fLangMan.LS(PLS.Next);
            btnBack.Text = fLangMan.LS(PLS.Back);
            btnClose.Text = fLangMan.LS(PLS.Close);

            grpPersonLineSpecials.Text = fLangMan.LS(PLS.PersonLineSpecials);
            chkSpecial_1.Text = fLangMan.LS(PLS.Special_1);
        }

        private async void btnImportFileChoose_Click(object sender, EventArgs e)
        {
            string filter;
#if !MONO
            filter = fLangMan.LS(PLS.AllFiltersW);
#else
            filter = fLangMan.LS(PLS.AllFiltersL);
#endif

            string fileName = await AppHost.StdDialogs.GetOpenFile("", "", filter, 1, "");
            if (string.IsNullOrEmpty(fileName)) return;

            edImportFile.Text = fileName;

            try {
                bool res = fImporter.LoadFile(edImportFile.Text);
                if (res) {
                    fAvailableStage++;

                    switch (fImporter.CanNumbersType) {
                        case PersonNumbersType.pnUndefined:
                            rbNumsUnknown.Checked = true;
                            break;
                        case PersonNumbersType.pnKonovalov:
                            rbNumsKonovalov.Checked = true;
                            break;
                        case PersonNumbersType.pnDAboville:
                            rbNumsDAboville.Checked = true;
                            break;
                    }

                }

                UpdateNavigation();
            } catch (Exception ex) {
                AppHost.StdDialogs.ShowError(ex.Message, fLangMan.LS(PLS.PedigreeImporter));
            }
        }

        private void ChangeStage()
        {
            switch (fCurrentStage) {
                case 1: {
                        if (rbNumsUnknown.Checked) {
                            fImporter.NumbersType = PersonNumbersType.pnUndefined;
                        } else if (rbNumsKonovalov.Checked) {
                            fImporter.NumbersType = PersonNumbersType.pnKonovalov;
                        } else if (rbNumsDAboville.Checked) {
                            fImporter.NumbersType = PersonNumbersType.pnDAboville;
                        }

                        if (cbPersonSeparator.SelectedIndex == 0) {
                            fImporter.PersonLineSeparator = (char)0;
                        } else {
                            fImporter.PersonLineSeparator = cbPersonSeparator.Text[0];
                        }

                        fImporter.NameFormat = (NameFormat)cbNameFormat.SelectedIndex;
                        fImporter.GenerationFormat = (GenerationFormat)cbGenerationFormat.SelectedIndex;
                        fImporter.DateFormat = (DateFormat)cbDatesFormat.SelectedIndex;
                        fImporter.PersonLineSeparator = cbDateSeparator.Text[0];

                        fImporter.SurnamesNormalize = chkSurnamesNormalize.Checked.Value;

                        fImporter.SpecialFormat_1 = chkSpecial_1.Checked.Value;

                        fImporter.ImportContent();

                        fBase.RefreshLists(false);
                    }
                    break;
            }
        }

        private void UpdateNavigation()
        {
            btnBack.Enabled = (fCurrentStage > 0);
            btnNext.Enabled = (fCurrentStage < fAvailableStage);
        }

        private void btnBack_Click(object sender, EventArgs e)
        {
            fCurrentStage--;
            tabControl1.SelectedPage = tabControl1.Pages[fCurrentStage];
            UpdateNavigation();
        }

        private void btnNext_Click(object sender, EventArgs e)
        {
            fCurrentStage++;
            tabControl1.SelectedPage = tabControl1.Pages[fCurrentStage];
            UpdateNavigation();
            ChangeStage();
        }

        private void btnClose_Click(object sender, EventArgs e)
        {
            Close();
        }
    }
}
