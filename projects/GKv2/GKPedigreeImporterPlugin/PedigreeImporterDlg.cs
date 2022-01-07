/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using GKCore.Interfaces;
using GKCore.Types;

namespace GKPedigreeImporterPlugin
{
    public sealed partial class PedigreeImporterDlg : Form
    {
        private readonly IPlugin fPlugin;
        private readonly ILangMan fLangMan;
        private readonly IBaseWindow fBase;
        private readonly Importer fImporter;
        private int fCurrentStage;
        private int fAvailableStage;

        public PedigreeImporterDlg(IPlugin plugin, IBaseWindow curBase)
        {
            InitializeComponent();

            fPlugin = plugin;
            fLangMan = fPlugin.LangMan;

            cbDatesFormat.Items.Clear();
            cbDatesFormat.Items.AddRange(new object[] { "DD/MM/YYYY", "YYYY/MM/DD" });

            cbGenerationFormat.Items.Clear();
            cbGenerationFormat.Items.AddRange(new object[] {
                                                  "I, II, III, IV...",
                                                  fLangMan.LS(ILS.LSID_Generation) + " N"});

            cbNameFormat.Items.AddRange(new object[] { fLangMan.LS(ILS.LSID_NPS), fLangMan.LS(ILS.LSID_SNP) });

            cbPersonSeparator.Items.AddRange(new object[] { fLangMan.LS(ILS.LSID_NoSpecial), ";", ","});

            fBase = curBase;
            fImporter = new Importer(fBase, fLangMan, lbLog.Items);
            fCurrentStage = 0;
            fAvailableStage = 0;

            cbPersonSeparator.SelectedIndex = 0;
            cbNameFormat.SelectedIndex = 0;
            cbGenerationFormat.SelectedIndex = 0;
            cbDatesFormat.SelectedIndex = 0;
            cbDateSeparator.SelectedIndex = 0;

            // SetLocale()
            Text = fLangMan.LS(ILS.LSID_PluginTitle);
            lblFile.Text = fLangMan.LS(ILS.LSID_File);
            btnImportFileChoose.Text = fLangMan.LS(ILS.LSID_DlgSelect) + @"...";

            grpPersonIdFormat.Text = fLangMan.LS(ILS.LSID_PersonIdFormat);
            rbNumsDAboville.Text = fLangMan.LS(ILS.LSID_NumsDAboville);
            rbNumsKonovalov.Text = fLangMan.LS(ILS.LSID_NumsKonovalov);
            rbNumsUnknown.Text = fLangMan.LS(ILS.LSID_NumsUnknown);

            grpTextPedigreesParams.Text = fLangMan.LS(ILS.LSID_TextPedigreesParams);
            lblPersonLineSeparator.Text = fLangMan.LS(ILS.LSID_PersonLineSeparator);
            lblSurnameFormat.Text = fLangMan.LS(ILS.LSID_SurnameFormat);
            lblGenerationFormat.Text = fLangMan.LS(ILS.LSID_GenerationFormat);
            lblDateSeparator.Text = fLangMan.LS(ILS.LSID_DateSeparator);
            lblDateFormat.Text = fLangMan.LS(ILS.LSID_DateFormat);

            grpConversionParams.Text = fLangMan.LS(ILS.LSID_ConversionParams);
            chkSurnamesNormalize.Text = fLangMan.LS(ILS.LSID_SurnamesNormalize);

            btnNext.Text = fLangMan.LS(ILS.LSID_Next);
            btnBack.Text = fLangMan.LS(ILS.LSID_Back);
            btnClose.Text = fLangMan.LS(ILS.LSID_Close);

            grpPersonLineSpecials.Text = fLangMan.LS(ILS.LSID_PersonLineSpecials);
            chkSpecial_1.Text = fLangMan.LS(ILS.LSID_Special_1);
        }

        private void btnImportFileChoose_Click(object sender, EventArgs e)
        {
            string filter;
            #if !MONO
            filter = fLangMan.LS(ILS.LSID_AllFiltersW);
            #else
            filter = fLangMan.LS(ILS.LSID_AllFiltersL);
            #endif

            string fileName = AppHost.StdDialogs.GetOpenFile("", "", filter, 1, "");
            if (string.IsNullOrEmpty(fileName)) return;

            edImportFile.Text = fileName;

            try
            {
                bool res = fImporter.LoadRawData(edImportFile.Text);
                if (res) {
                    fAvailableStage++;

                    switch (fImporter.CanNumbersType)
                    {
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
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, fLangMan.LS(ILS.LSID_PluginTitle), MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void ChangeStage()
        {
            switch (fCurrentStage)
            {
                case 1:
                    {
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

                        fImporter.SurnamesNormalize = chkSurnamesNormalize.Checked;

                        fImporter.SpecialFormat_1 = chkSpecial_1.Checked;

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
            tabControl1.SelectedTab = tabControl1.TabPages[fCurrentStage];
            UpdateNavigation();
        }

        private void btnNext_Click(object sender, EventArgs e)
        {
            fCurrentStage++;
            tabControl1.SelectedTab = tabControl1.TabPages[fCurrentStage];
            UpdateNavigation();
            ChangeStage();
        }

        private void rbNums_CheckedChanged(object sender, EventArgs e)
        {
            RadioButton rb = sender as RadioButton;
            if (rb == null) return;

            if (rb == rbNumsUnknown && rb.Checked) {
                
            }
        }

        private void btnClose_Click(object sender, EventArgs e)
        {
            Close();
        }
    }
}
