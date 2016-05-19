/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using GKCommon;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKPedigreeImporterPlugin
{
    public partial class PedigreeImporterDlg : Form
    {
        private readonly IPlugin fPlugin;
        private readonly ILangMan fLangMan;
        private readonly IBaseWindow fBase;
        private readonly Importer fImporter;
        private int fCurrentStage;
        private int fAvailableStage;

        // TODO: localize
        #if !GK_LINUX
        private static string filter = "Все поддерживаемые форматы (*.txt, *.csv, *.doc, *.xls)|*.txt;*.csv;*.doc;*.xls|Роспись в txt-формате (*.txt)|*.txt|Роспись в csv-формате (*.csv)|*.csv|Роспись в формате Word (*.doc)|*.doc|Роспись в формате Excel (*.xls)|*.xls";
        #else
        private static string filter = "Все поддерживаемые форматы (*.txt, *.csv)|*.txt;*.csv|Роспись в txt-формате (*.txt)|*.txt|Роспись в csv-формате (*.csv)|*.csv";
        #endif
        
        public PedigreeImporterDlg(IPlugin fPlugin)
        {
            InitializeComponent();

            this.fPlugin = fPlugin;
            this.fLangMan = fPlugin.LangMan;

            this.fBase = fPlugin.Host.GetCurrentFile();
            this.fImporter = new Importer(fBase, this.fLangMan, this.lbLog.Items);
            this.fCurrentStage = 0;
            this.fAvailableStage = 0;

            this.cbPersonSeparator.SelectedIndex = 0;
            this.cbNameFormat.SelectedIndex = 0;
            this.cbGenerationFormat.SelectedIndex = 0;
            this.cbDatesFormat.SelectedIndex = 0;
            this.cbDateSeparator.SelectedIndex = 0;

            this.cbDatesFormat.Items.Clear();
            this.cbDatesFormat.Items.AddRange(new object[] {
                                    "ДД/ММ/ГГГГ",
                                    "ГГГГ/ММ/ДД"});

            this.cbGenerationFormat.Items.Clear();
            this.cbGenerationFormat.Items.AddRange(new object[] {
                                    "I, II, III, IV...",
                                    "Поколение N"});

            this.cbNameFormat.Items.AddRange(new object[] {
                                    "Имя Отчество Фамилия",
                                    "Фамилия Имя Отчество"});

            this.cbPersonSeparator.Items.AddRange(new object[] {
                                    "нет специального",
                                    ";",
                                    ","});

            // SetLang()
            this.Text = fLangMan.LS(ILS.LSID_PluginTitle);
            this.lblFile.Text = fLangMan.LS(ILS.LSID_File);
            this.btnImportFileChoose.Text = fLangMan.LS(ILS.LSID_DlgSelect) + @"...";

            this.grpPersonIdFormat.Text = fLangMan.LS(ILS.LSID_PersonIdFormat);
            this.rbNumsDAboville.Text = fLangMan.LS(ILS.LSID_NumsDAboville);
            this.rbNumsKonovalov.Text = fLangMan.LS(ILS.LSID_NumsKonovalov);
            this.rbNumsUnknown.Text = fLangMan.LS(ILS.LSID_NumsUnknown);

            this.grpTextPedigreesParams.Text = fLangMan.LS(ILS.LSID_TextPedigreesParams);
            this.lblPersonLineSeparator.Text = fLangMan.LS(ILS.LSID_PersonLineSeparator);
            this.lblSurnameFormat.Text = fLangMan.LS(ILS.LSID_SurnameFormat);
            this.lblGenerationFormat.Text = fLangMan.LS(ILS.LSID_GenerationFormat);
            this.lblDateSeparator.Text = fLangMan.LS(ILS.LSID_DateSeparator);
            this.lblDateFormat.Text = fLangMan.LS(ILS.LSID_DateFormat);

            this.grpConversionParams.Text = fLangMan.LS(ILS.LSID_ConversionParams);
            this.chkSurnamesNormalize.Text = fLangMan.LS(ILS.LSID_SurnamesNormalize);

            this.btnNext.Text = fLangMan.LS(ILS.LSID_Next);
            this.btnBack.Text = fLangMan.LS(ILS.LSID_Back);
            this.btnClose.Text = fLangMan.LS(ILS.LSID_Close);

            this.grpPersonLineSpecials.Text = fLangMan.LS(ILS.LSID_PersonLineSpecials);
            this.chkSpecial_1.Text = fLangMan.LS(ILS.LSID_Special_1);
        }

        private void btnImportFileChoose_Click(object sender, EventArgs e)
        {
            string fileName = UIHelper.GetOpenFile("", "", filter, 1, "");
            if (!string.IsNullOrEmpty(fileName))
            {
                this.edImportFile.Text = fileName;

                try
                {
                    bool res = this.fImporter.LoadRawData(this.edImportFile.Text);
                    if (res) {
                        this.fAvailableStage++;

                        switch (this.fImporter.CanNumbersType)
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

                    this.UpdateNavigation();
                }
                catch (Exception ex)
                {
                    MessageBox.Show(ex.Message, fLangMan.LS(ILS.LSID_PluginTitle), MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }
        }

        private void ChangeStage()
        {
            switch (this.fCurrentStage)
            {
                case 1:
                    {
                        if (rbNumsUnknown.Checked) {
                            this.fImporter.NumbersType = PersonNumbersType.pnUndefined;
                        } else if (rbNumsKonovalov.Checked) {
                            this.fImporter.NumbersType = PersonNumbersType.pnKonovalov;
                        } else if (rbNumsDAboville.Checked) {
                            this.fImporter.NumbersType = PersonNumbersType.pnDAboville;
                        }

                        if (cbPersonSeparator.SelectedIndex == 0) {
                            this.fImporter.PersonLineSeparator = (char)0;
                        } else {
                            this.fImporter.PersonLineSeparator = cbPersonSeparator.Text[0];
                        }

                        this.fImporter.NameFormat = (NameFormat)cbNameFormat.SelectedIndex;
                        this.fImporter.GenerationFormat = (GenerationFormat)cbGenerationFormat.SelectedIndex;
                        this.fImporter.DateFormat = (DateFormat)cbDatesFormat.SelectedIndex;
                        this.fImporter.PersonLineSeparator = cbDateSeparator.Text[0];

                        this.fImporter.SurnamesNormalize = chkSurnamesNormalize.Checked;

                        this.fImporter.SpecialFormat_1 = chkSpecial_1.Checked;

                        this.fImporter.ImportContent();

                        this.fBase.RefreshLists(false);
                    }
                    break;
            }
        }

        private void UpdateNavigation()
        {
            btnBack.Enabled = (this.fCurrentStage > 0);
            btnNext.Enabled = (this.fCurrentStage < this.fAvailableStage);
        }

        private void btnBack_Click(object sender, EventArgs e)
        {
            this.fCurrentStage--;
            this.tabControl1.SelectedTab = this.tabControl1.TabPages[this.fCurrentStage];
            this.UpdateNavigation();
        }

        private void btnNext_Click(object sender, EventArgs e)
        {
            this.fCurrentStage++;
            this.tabControl1.SelectedTab = this.tabControl1.TabPages[this.fCurrentStage];
            this.UpdateNavigation();
            this.ChangeStage();
        }

        private void rbNums_CheckedChanged(object sender, EventArgs e)
        {
            RadioButton rb = sender as RadioButton;
            if (rb == this.rbNumsUnknown && rb.Checked) {
                
            }
        }

        private void btnClose_Click(object sender, EventArgs e)
        {
            this.Close();
        }
    }
}
