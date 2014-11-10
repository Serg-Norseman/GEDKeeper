/* Form2.cs
 * 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * History:  
 * 10Dec08 AlexC          Migrated from GEDmill 1.10
 *
 */

using System;
using System.Drawing;
using System.Collections;
using System.Windows.Forms;
using GEDmill.HTMLClasses;
using System.IO;
using System.Threading;
using GEDmill.LLClasses;


namespace GEDmill
{
    // The main from from which the application is operated. Contains the GUI controls and the control handlers.
    // This file contains the GUI event handlers and main application logic. The file Form1.cs contains the GUI building code.
    public partial class MainForm : Form
    {
        ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Event handlers /////////////////////////////////////////////////////////////////////////////////////////////////
        ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // Event handler
        private void backButton_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Back button");

            // Mustn't affect configPanel 
            if (m_bConfigPanelOn)
            {
                return;
            }

            if (m_nCurrentPanel > 1)
            {
                --m_nCurrentPanel;
            }


            EnableCurrentPanel(true);

            ShowCurrentPanel();
        }

        // Event handler
        private void nextButton_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Next button clicked. current panel = " + m_nCurrentPanel.ToString());

            // Mustn't affect configPanel
            if (m_bConfigPanelOn)
            {
                return;
            }

            // Disable edit boxes etc. on current panel to avoid confusing users
            EnableCurrentPanel(false);

            // Disable buttons while progress dialogs etc are shown (they may appear to some
            // users as being part of the progress dialog.)
            m_buttonNext.Enabled = false;
            m_buttonBack.Enabled = false;
            m_buttonCancel.Enabled = false;
            m_buttonSettings.Enabled = false;
            m_buttonHelp.Enabled = false;

            if (ValidateCurrentPanel())
            {
                if (m_nCurrentPanel < 9) // Allow for extra ftp panels
                    ++m_nCurrentPanel;
                else
                {
                    s_config.StoreSettings();


                    if (s_config.m_bOpenWebsiteOnExit)
                    {
                        OpenURL(s_config.FrontPageURL);
                    }
                    Close();
                    return;
                }

                InitialiseCurrentPanel();
                ShowCurrentPanel();
            }
            else
            {
                // Restore next button state (it must have been enabled for this function to have been called)
                m_buttonNext.Enabled = true;
            }
            // Back button and quit button are always enabled
            m_buttonBack.Enabled = true;
            m_buttonCancel.Enabled = true;
            m_buttonSettings.Enabled = true;
            m_buttonHelp.Enabled = true;

            EnableCurrentPanel(true);
        }

        // Event handler
        private void cancelButton_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Quit button clicked.");

            DialogResult dialogResult = DialogResult.Yes;

            if (m_nCurrentPanel != 6) 
            {
                // Cancel button is "Finish" in panel 6
                dialogResult = MessageBocx.Show(m_mainForm, "Are you sure you wish to exit GEDmill?", "Quit GEDmill",
                    MessageBoxButtons.YesNo, MessageBoxIcon.Question, false);
            }

            if (dialogResult == DialogResult.Yes)
            {
                if (m_nCurrentPanel >= 6) 
                {
                    // HTML generated, so save settings for that at least
                    // Store checkbox state in config.(Can't do in ValidateCurrentPanel because Next never gets clicked)
                    s_config.m_bOpenWebsiteOnExit = m_checkboxAllDoneShowSite.Checked;
                }

                if (m_nCurrentPanel == 6) 
                {
                    // Finish button is the only time we want to launch webpages
                    if (s_config.m_bOpenWebsiteOnExit && s_config.m_sFrontPageFilename.Length > 0)
                    {
                        OpenURL(s_config.FrontPageURL);
                    }

                }

                s_config.StoreSettings();
                Application.Exit();
            }
        }

        // Event handler
        private void helpButton_click(object sender, System.EventArgs e)
        {
            string sHelpFile = s_config.m_sApplicationPath + "\\" + m_sHelpFilename;

            if (m_bConfigPanelOn)
            {
                switch (m_tabcontrolConfigPanel.SelectedIndex)
                {
                    case 0:
                        Help.ShowHelp(m_buttonHelp, sHelpFile, HelpNavigator.Topic, "SettingsWebpages.htm");
                        break;
                    case 1:
                        Help.ShowHelp(m_buttonHelp, sHelpFile, HelpNavigator.Topic, "SettingsImages.htm");
                        break;
                    case 2:
                        Help.ShowHelp(m_buttonHelp, sHelpFile, HelpNavigator.Topic, "SettingsGEDCOM.htm");
                        break;
                    case 3:
                        Help.ShowHelp(m_buttonHelp, sHelpFile, HelpNavigator.Topic, "SettingsTrees.htm");
                        break;
                    case 4:
                        Help.ShowHelp(m_buttonHelp, sHelpFile, HelpNavigator.Topic, "SettingsAdvanced.htm");
                        break;
                    default:
                        Help.ShowHelp(m_buttonHelp, sHelpFile, HelpNavigator.Topic, "FrontPage.htm");
                        break;
                }
            }
            else
            {
                switch (m_nCurrentPanel)
                {
                    case 2:
                        Help.ShowHelp(m_buttonHelp, sHelpFile, HelpNavigator.Topic, "SelectingInputFile_1.htm");
                        break;
                    case 3:
                        Help.ShowHelp(m_buttonHelp, sHelpFile, HelpNavigator.Topic, "ExcludingPeople_2.htm");
                        break;
                    case 4:
                        Help.ShowHelp(m_buttonHelp, sHelpFile, HelpNavigator.Topic, "SetTheTitle_3.htm");
                        break;
                    case 5:
                        Help.ShowHelp(m_buttonHelp, sHelpFile, HelpNavigator.Topic, "SelectingOutputFile_4.htm");
                        break;
                    case 6:
                        Help.ShowHelp(m_buttonHelp, sHelpFile, HelpNavigator.Topic, "FinishScreen_5.htm");
                        break;
                    default:
                        Help.ShowHelp(m_buttonHelp, sHelpFile, HelpNavigator.Topic, "FrontPage.htm");
                        break;
                }
            }
        }

        // Event handler
        private void configButton_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Config button clicked. current panel = " + m_nCurrentPanel.ToString());
            
            if (!m_bConfigPanelOn)
            {
                // Switch config panel on
                // Initialise config panel settings
                LoadConfigPanelSettings();
                SwitchConfigPanelOn();
            }
            else 
            {
                // Switch config panel off
                // Save config panel settings
                SaveConfigPanelSettings();
                SwitchConfigPanelOff();
            }
        }

        // Event handler
        private void configCancelButton_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Config reset button clicked. current panel = " + m_nCurrentPanel.ToString());

            // Ensure config panel on
            if (!m_bConfigPanelOn)
            {
                return;
            }

            // Remove panel without saving changes
            SwitchConfigPanelOff();
        }

        // Event handler
        private void buttonPruneRecordsSave_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Panel 3 save button clicked.");

            string sTitle = "Save settings for individuals";

            if (s_config.m_sExcludeFileDir.Length == 0)
            {
                // Use path from input GEDCOM file
                s_config.m_sExcludeFileDir = Path.GetDirectoryName(InputFile); 
            }
            if (SelectFile(ref s_config.m_sExcludeFileDir, ref s_config.m_sExcludeFileName, sTitle, "Selected Records.txt", false, "Text files", ".txt") == false)
            {
                return;
            }

            string sFilename = s_config.m_sExcludeFileDir + "\\" + s_config.m_sExcludeFileName;

            // Write selections in to file
            if (File.Exists(sFilename))
            {
                File.SetAttributes(sFilename, FileAttributes.Normal);
                File.Delete(sFilename);
            }

            FileStream fs = new FileStream(sFilename, FileMode.Create);
            StreamWriter sw = new StreamWriter(fs, System.Text.Encoding.UTF8);

            sw.WriteLine("// GEDmill " + m_sSoftwareVersion);

            foreach (ListViewItem lvi in m_listviewPruneRecordsIndis.Items)
            {
                if (lvi is CListableBool)
                {
                    if (lvi.Checked)
                    {
                        LLClasses.CIndividualRecord ir = (LLClasses.CIndividualRecord)(((CListableBool)lvi).ISRecord);
                        string sName = ir.Name;
                        if (sName.Length > 0)
                        {
                            sName = " # " + sName;
                        }
                        sw.WriteLine(String.Concat("I", ir.m_xref, sName));

                        foreach (CMultimediaFileReference mfr in ir.m_alUniqueFileRefs)
                        {
                            SaveMFR(sw, mfr);
                        }
                    }
                }
            }

            foreach (ListViewItem lvi in m_listviewPruneRecordsSources.Items)
            {
                if (lvi is CListableBool)
                {
                    if (lvi.Checked)
                    {
                        LLClasses.CSourceRecord sr = (LLClasses.CSourceRecord)(((CListableBool)lvi).ISRecord);
                        string sDescriptiveTitle = sr.DescriptiveTitle;
                        if (sDescriptiveTitle.Length > 0)
                        {
                            sDescriptiveTitle = " # " + sDescriptiveTitle;
                        }
                        sw.WriteLine(String.Concat("S", sr.m_xref, sDescriptiveTitle));

                        foreach (CMultimediaFileReference mfr in sr.m_alUniqueFileRefs)
                        {
                            SaveMFR(sw, mfr);
                        }
                    }
                }
            }

            m_bPrunepanelDataChanged = false;
            sw.Close();

            EnablePrunePanelButtons();

        }

        // Event handler
        private void buttonPruneRecordsLoad_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Panel 3 load button clicked.");

            if (s_config.m_sExcludeFileDir.Length == 0)
            {
                s_config.m_sExcludeFileDir = Path.GetDirectoryName(InputFile); // Use path from input GEDCOM file
            }
            if (SelectFile(ref s_config.m_sExcludeFileDir, ref s_config.m_sExcludeFileName, "Load settings for individuals", "", true, "Text files", ".txt") == false)
            {
                return;
            }

            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            string sFilename = s_config.m_sExcludeFileDir + "\\" + s_config.m_sExcludeFileName;

            // Parse the selection file
            EVersion eVersion = EVersion.pre1p8;
            Hashtable htSelected = ParseSelectionFile(sFilename, ref eVersion);
            if (htSelected == null)
            {
                // Invalid file selected
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Invalid file selected");
                return;
            }

            // Select individuals accordingly
            m_nPruneExcluded = 0;
            m_nPruneIncluded = 0;
            int nIndisUsedFromHash = 0;
            int nSourcesUsedFromHash = 0;
            bool bWarnOfDifferences = false;
            m_bDisablePrunepanelCheckEvent = true;

            foreach (GEDmill.LLClasses.CIndividualRecord ir in m_gedcom.m_alIndividualRecords)
            {
                bool bRestricted = true;

                string sKeyToLookFor = ir.m_xref;
                if (eVersion >= EVersion.v1p9)
                {
                    sKeyToLookFor = 'I' + ir.m_xref;
                }
                if (htSelected.ContainsKey(sKeyToLookFor))
                {
                    ++nIndisUsedFromHash;

                    CISRecordChanges uc = (CISRecordChanges)htSelected[sKeyToLookFor];
                    if (uc != null)
                    {
                        bRestricted = !uc.m_bIncludeInWebsite;
                        bool bDifferent = LoadMFRs(ir, uc);
                        if (bDifferent)
                        {
                            bWarnOfDifferences = true;
                        }
                    }
                }
                ir.Restricted = bRestricted;
            }


            foreach (GEDmill.LLClasses.CSourceRecord sr in m_gedcom.m_alSourceRecords)
            {
                // Earlier versions don't restrict sources at all
                bool bRestricted = false; 

                if (eVersion >= EVersion.v1p9)
                {
                    bRestricted = true;

                    string sKeyToLookFor = sr.m_xref;
                    if (eVersion >= EVersion.v1p9)
                    {
                        sKeyToLookFor = 'S' + sr.m_xref;
                    }
                    if (htSelected.ContainsKey(sKeyToLookFor))
                    {
                        ++nSourcesUsedFromHash;

                        CISRecordChanges isrc = (CISRecordChanges)htSelected[sKeyToLookFor];
                        if (isrc != null)
                        {
                            bRestricted = !isrc.m_bIncludeInWebsite;
                            bool bDifferent = LoadMFRs(sr, isrc);
                            if (bDifferent)
                            {
                                bWarnOfDifferences = true;
                            }
                        }
                    }
                }
                sr.Restricted = bRestricted;
            }

            if ((nIndisUsedFromHash + nSourcesUsedFromHash) != htSelected.Count || bWarnOfDifferences)
            {
                DialogResult dialogResult = MessageBocx.Show(this, "The GEDCOM file appears to have changed since this\r\nfile was saved. Changes may not be restored exactly.", "GEDCOM Changes Detected",
                    MessageBoxButtons.OK, MessageBoxIcon.Warning, false);
            }
            if (eVersion < EVersion.v1p9)
            {
                DialogResult dialogResult = MessageBocx.Show(this, "This changes file came from an earlier version of GEDmill.\r\nSaving again is recommended in order to update the file.", "Earlier Version Of GEDmill Detected",
                    MessageBoxButtons.OK, MessageBoxIcon.Warning, false);
            }

            m_bPrunepanelDataChanged = false;
            m_bDisablePrunepanelCheckEvent = false;

            // Rebuild lists
            FillIndividualsList(m_listviewPruneRecordsIndis, false, null, true);
            FillSourcesList(m_listviewPruneRecordsSources, true);

            EnablePrunePanelButtons();

            Cursor.Current = Cursors.Default;
            Cursor.Hide();
        }

        // Event handler
        private void buttonChooseGedcomBrowse_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Panel 2 browse button clicked.");

            OpenFileDialog openFileDialog = new OpenFileDialog();

            if (Directory.Exists(InputFile))
            {
                openFileDialog.InitialDirectory = InputFile;
            }
            else
            {
                string sPath = InputFile;
                int nLastFolder = sPath.LastIndexOf('\\'); // Try parent folder
                if (nLastFolder >= 0)
                {
                    sPath = sPath.Substring(0, nLastFolder);
                }
                if (Directory.Exists(sPath))
                {
                    openFileDialog.InitialDirectory = sPath;
                }
                else
                {
                    openFileDialog.InitialDirectory = Environment.GetFolderPath(System.Environment.SpecialFolder.Personal);
                }
            }

            openFileDialog.FileName = InputFile;
            openFileDialog.Filter = "GEDCOM files (*.ged)|*.ged|All files (*.*)|*.*";
            openFileDialog.FilterIndex = 1;
            openFileDialog.RestoreDirectory = true;

            if (openFileDialog.ShowDialog() == DialogResult.OK)
            {
                InputFile = openFileDialog.FileName;
                m_textboxChooseGedcom.SelectionStart = InputFile.Length;
                m_textboxChooseGedcom.SelectionLength = InputFile.Length;

            }
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Selected file : " + InputFile);
        }

        // Event handler
        private void textboxChooseGedcom_textChanged(object sender, System.EventArgs e)
        {
            EnableNextButton();
        }

        // Event handler
        private void textboxChooseOutput_textChanged(object sender, System.EventArgs e)
        {
            EnableNextButton();
        }

        // Event handler
        private void listboxSelectKey_selectedValueChanged(object sender, System.EventArgs e)
        {
            EnableKeyIndividualsDeleteButton();
        }

        // Event handler
        private void buttonSelectKeyAdd_click(object sender, System.EventArgs e)
        {
            // Use a dialog box to let them choose an individual
            IndividualBrowserDialog individualBrowserDialog = new IndividualBrowserDialog(this, false);

            FillIndividualsList(individualBrowserDialog.m_sortableListView, false, null, false);
            DialogResult dialogResult = individualBrowserDialog.ShowDialog(this);
            if (dialogResult != DialogResult.OK)
            {
                return;
            }

            LLClasses.CIndividualRecord ir = individualBrowserDialog.FirstSelectedIndividual;

            // Ensure they are only added once
            bool bAlreadyAdded = false;
            foreach (string keyXref in s_config.m_alKeyIndividuals)
            {
                if (keyXref == ir.m_xref)
                {
                    bAlreadyAdded = true;
                    break;
                }
            }
            if (!bAlreadyAdded)
            {
                s_config.m_alKeyIndividuals.Add(ir.m_xref);
            }

            FillKeyIndividualsList();
        }

        // Event handler
        private void buttonSelectKeyDelete_click(object sender, System.EventArgs e)
        {
            CNameXrefPair xrefPairName = (CNameXrefPair)m_listboxSelectKey.SelectedItem;
            if (xrefPairName != null)
            {
                string xref = xrefPairName.m_xref;
                if (xref != null)
                {
                    int nIndex;
                    int nKeys = s_config.m_alKeyIndividuals.Count;
                    for (nIndex = 0; nIndex < nKeys; nIndex++)
                    {
                        if (xref == (string)(s_config.m_alKeyIndividuals[nIndex]))
                        {
                            s_config.m_alKeyIndividuals.RemoveAt(nIndex);
                            break;
                        }
                    }
                }
                FillKeyIndividualsList();
            }
        }

        // Event handler
        private void buttonChooseOutputBrowse_click(object sender, System.EventArgs e)
        {
            FolderBrowserDialog folderBrowserDialog1 = new FolderBrowserDialog();
            if (Directory.Exists(m_textboxChooseOutput.Text))
            {
                folderBrowserDialog1.SelectedPath = m_textboxChooseOutput.Text;
            }
            else
            {
                string sPath = m_textboxChooseOutput.Text;
                int nLastFolder = sPath.LastIndexOf('\\'); // Try folder above
                if (nLastFolder >= 0)
                {
                    sPath = sPath.Substring(0, nLastFolder);
                }
                if (Directory.Exists(sPath))
                {
                    folderBrowserDialog1.SelectedPath = sPath;
                }
                else
                {
                    folderBrowserDialog1.SelectedPath = Environment.GetFolderPath(System.Environment.SpecialFolder.Personal) + "\\GEDmill_Output";
                }
            }
            if (folderBrowserDialog1.ShowDialog() == DialogResult.OK)
            {
                m_textboxChooseOutput.Text = folderBrowserDialog1.SelectedPath;
                m_textboxChooseOutput.SelectionStart = m_textboxChooseOutput.Text.Length;
                m_textboxChooseOutput.SelectionLength = m_textboxChooseOutput.Text.Length;
            }
        }

        // Event handler
        private void linklabelAllDone_click(object sender, LinkLabelLinkClickedEventArgs e)
        {
            bool bOldVisitedValue = m_linklabelAllDone.Links[m_linklabelAllDone.Links.IndexOf(e.Link)].Visited;
            try
            {
                m_linklabelAllDone.Links[m_linklabelAllDone.Links.IndexOf(e.Link)].Visited = true;
                string sURL = m_linklabelAllDone.Text;
                System.Diagnostics.Process.Start(sURL);
            }
            catch (Exception e2)
            {
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, String.Format("Caught exception while viewing folder : {0}", e2.ToString()));
                m_linklabelAllDone.Links[m_linklabelAllDone.Links.IndexOf(e.Link)].Visited = bOldVisitedValue;
            }
        }

        // Event handler
        private void configPanel_BackImage_BrowseButton_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "config panel back image browse button clicked.");

            OpenFileDialog openFileDialog = new OpenFileDialog();

            if (Directory.Exists(m_textboxConfigBackImageEdit.Text))
            {
                openFileDialog.InitialDirectory = m_textboxConfigBackImageEdit.Text;
            }
            else
            {
                string sPath = m_textboxConfigBackImageEdit.Text;
                int iLastFolder = sPath.LastIndexOf('\\'); // Try parent folder
                if (iLastFolder >= 0)
                {
                    sPath = sPath.Substring(0, iLastFolder);
                }
                if (Directory.Exists(sPath))
                {
                    openFileDialog.InitialDirectory = sPath;
                }
                else
                {
                    openFileDialog.InitialDirectory = Environment.GetFolderPath(System.Environment.SpecialFolder.Personal);
                }
            }

            openFileDialog.FileName = m_textboxConfigBackImageEdit.Text;
            openFileDialog.Filter = "JPEG (*.jpg;*.jpeg)|*.jpg;*.jpeg|"
                + "Portable Network Graphics (*.png)|*.png|"
                + "Graphics Interchange Format (*.gif)|*.gif|"
                + "Windows Bitmap (*.bmp)|*.bmp|"
                + "All supported picture files|*.jpg;*.jpeg;*.gif;*.bmp;*.png";
            openFileDialog.FilterIndex = 1;
            openFileDialog.RestoreDirectory = true;

            if (openFileDialog.ShowDialog() == DialogResult.OK)
            {
                string sExtn = Path.GetExtension(openFileDialog.FileName);
                sExtn = sExtn.ToLower();
                if (sExtn != ".jpg" && sExtn != ".jpeg" && sExtn != ".png" && sExtn != ".gif" && sExtn != ".bmp")
                {
                    MessageBocx.Show(m_mainForm, "The file you have selected is not a supported picture type.", "Unsupported Format",
                        MessageBoxButtons.OK, MessageBoxIcon.Exclamation, false);
                }
                else
                {
                    m_textboxConfigBackImageEdit.Text = openFileDialog.FileName;
                    m_textboxConfigBackImageEdit.SelectionStart = m_textboxConfigBackImageEdit.Text.Length;
                    m_textboxConfigBackImageEdit.SelectionLength = m_textboxConfigBackImageEdit.Text.Length;
                }

            }
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Selected file : " + m_textboxConfigBackImageEdit.Text);
        }

        // Event handler
        private void configPanel_FrontImage_BrowseButton_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "config panel front image browse button clicked.");

            OpenFileDialog openFileDialog1 = new OpenFileDialog();

            if (Directory.Exists(m_textboxConfigFrontImageEdit.Text))
            {
                openFileDialog1.InitialDirectory = m_textboxConfigFrontImageEdit.Text;
            }
            else
            {
                string sPath = m_textboxConfigFrontImageEdit.Text;
                int nLastFolder = sPath.LastIndexOf('\\'); // Try parent folder
                if (nLastFolder >= 0)
                {
                    sPath = sPath.Substring(0, nLastFolder);
                }
                if (Directory.Exists(sPath))
                {
                    openFileDialog1.InitialDirectory = sPath;
                }
                else
                {
                    openFileDialog1.InitialDirectory = Environment.GetFolderPath(System.Environment.SpecialFolder.Personal);
                }
            }

            openFileDialog1.FileName = m_textboxConfigFrontImageEdit.Text;
            openFileDialog1.Filter = "JPEG (*.jpg;*.jpeg)|*.jpg;*.jpeg|"
                + "Portable Network Graphics (*.png)|*.png|"
                + "Graphics Interchange Format (*.gif)|*.gif|"
                + "Windows Bitmap (*.bmp)|*.bmp|"
                + "All supported picture files|*.jpg;*.jpeg;*.gif;*.bmp;*.png";
            openFileDialog1.FilterIndex = 1;
            openFileDialog1.RestoreDirectory = true;

            if (openFileDialog1.ShowDialog() == DialogResult.OK)
            {
                string sExtn = Path.GetExtension(openFileDialog1.FileName);
                sExtn = sExtn.ToLower();
                if (sExtn != ".jpg" && sExtn != ".jpeg" && sExtn != ".png" && sExtn != ".gif" && sExtn != ".bmp")
                {
                    MessageBocx.Show(m_mainForm, "The file you have selected is not a supported picture type.", "Unsupported Format",
                        MessageBoxButtons.OK, MessageBoxIcon.Exclamation, false);
                }
                else
                {
                    m_textboxConfigFrontImageEdit.Text = openFileDialog1.FileName;
                    m_textboxConfigFrontImageEdit.SelectionStart = m_textboxConfigFrontImageEdit.Text.Length;
                    m_textboxConfigFrontImageEdit.SelectionLength = m_textboxConfigFrontImageEdit.Text.Length;
                }

            }
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Selected file : " + m_textboxConfigFrontImageEdit.Text);

        }

        // Event handler
        private void configPanel_TreeDiagrams_CheckBox_click(object sender, System.EventArgs e)
        {
            EnableMiniTreeButtons();
        }

        // Event handler
        private void configPanel_MiniTreeColourIndiBackground_Button_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourIndiBackground_Button_click.");

            m_colordialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiBackground;
            if (m_colordialogConfigMiniTree.ShowDialog() == DialogResult.OK)
            {
                m_colorConfigMiniTreeIndiBackground = m_colordialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MiniTreeColourIndiHighlight_Button_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourIndiHighlight_Button_click.");

            m_colordialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiHighlight;
            if (m_colordialogConfigMiniTree.ShowDialog() == DialogResult.OK)
            {
                m_colorConfigMiniTreeIndiHighlight = m_colordialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MiniTreeColourIndiBgConcealed_Button_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourIndiBgConcealed_Button_click.");

            m_colordialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiBgConcealed;
            if (m_colordialogConfigMiniTree.ShowDialog() == DialogResult.OK)
            {
                m_colorConfigMiniTreeIndiBgConcealed = m_colordialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MiniTreeColourIndiShade_Button_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourIndiShade_Button_click.");

            m_colordialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiShade;
            if (m_colordialogConfigMiniTree.ShowDialog() == DialogResult.OK)
            {
                m_colorConfigMiniTreeIndiShade = m_colordialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MiniTreeColourIndiText_Button_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourIndiText_Button_click.");

            m_colordialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiText;
            if (m_colordialogConfigMiniTree.ShowDialog() == DialogResult.OK)
            {
                m_colorConfigMiniTreeIndiText = m_colordialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MiniTreeColourIndiLink_Button_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourIndiLink_Button_click.");

            m_colordialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiLink;
            if (m_colordialogConfigMiniTree.ShowDialog() == DialogResult.OK)
            {
                m_colorConfigMiniTreeIndiLink = m_colordialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MiniTreeColourBranch_Button_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourBranch_Button_click.");

            m_colordialogConfigMiniTree.Color = m_colorConfigMiniTreeBranch;
            if (m_colordialogConfigMiniTree.ShowDialog() == DialogResult.OK)
            {
                m_colorConfigMiniTreeBranch = m_colordialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MiniTreeColourIndiBorder_Button_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourIndiBorder_Button_click.");

            m_colordialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiBorder;
            if (m_colordialogConfigMiniTree.ShowDialog() == DialogResult.OK)
            {
                m_colorConfigMiniTreeIndiBorder = m_colordialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MiniTreeColourIndiFgConcealed_Button_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourIndiFgConcealed_Button_click.");

            m_colordialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiFgConcealed;
            if (m_colordialogConfigMiniTree.ShowDialog() == DialogResult.OK)
            {
                m_colorConfigMiniTreeIndiFgConcealed = m_colordialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MultiPageIndex_CheckBox_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "config panel multi page index button clicked.");
            EnableMultiPageIndexConfig();
        }

        // Event handler
        private void configPanel_AllowMultimedia_CheckBox_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "allow multimedia button clicked.");
            EnableMultimediaConfig();
        }

        // Event handler
        private void configPanel_IndiImages_CheckBox_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "config panel multi images button clicked.");
            EnableThumbnailsConfig();
        }

        // Event handler
        private void configPanel_ShowWithheldRecords_CheckBox_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "config panel show withheld records button clicked.");
            EnableWithheldConfig();
        }

        // Event handler
        private void configPanel_WithheldName_Label_click(object sender, System.EventArgs e)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "config panel withheld label clicked.");
            EnableWithheldConfig();
        }

        // Event handler
        private void configPanel_Charset_ComboBox_changed(object sender, System.EventArgs e)
        {
            EnableBOMCheckBox();
        }

        // Event handler
        private void pruneIndividualsContextMenuDetails_Click(Object sender, System.EventArgs e)
        {
            LLClasses.CIndividualRecord ir = null;
            CListableBool lb = null;

            if (m_listviewPruneRecordsIndis.SelectedItems.Count == 1)
            {
                lb = (CListableBool)((ListViewItem)(m_listviewPruneRecordsIndis.SelectedItems[0]));
                ir = (LLClasses.CIndividualRecord)lb.ISRecord;
            }

            ShowIndividualDetailsDialog(this, lb, ir, true, true);
        }

        // Event handler
        private void pruneSourcesContextMenuDetails_Click(Object sender, System.EventArgs e)
        {
            LLClasses.CSourceRecord sr = null;
            ListViewItem lvi = null;

            if (m_listviewPruneRecordsSources.SelectedItems.Count == 1)
            {
                lvi = m_listviewPruneRecordsSources.SelectedItems[0];
                sr = (LLClasses.CSourceRecord)((CListableBool)((ListViewItem)lvi)).ISRecord;
            }

            ShowSourceDetailsDialog(this, ((CListableBool)((ListViewItem)lvi)), sr, true, true);
        }

        // Event handler
        private void pruneIndividualsContextMenuUnconnected_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            m_nPruneExcluded = 0;
            m_nPruneIncluded = 0;

            try
            {
                // Clear visited list
                m_gedcom.BeginPruning();

                // exclude all individuals unless connected in any way to this person through non-excluded people
                foreach (ListViewItem lvi in m_listviewPruneRecordsIndis.SelectedItems)
                {
                    if (lvi is CListableBool)
                    {
                        LLClasses.CIndividualRecord ir = (LLClasses.CIndividualRecord)((CListableBool)((ListViewItem)lvi)).ISRecord;
                        if (ir != null)
                        {
                            // First mark as visited all possible relations of irSubject, not following restricted people
                            // Adds to visited list
                            m_gedcom.PruneMarkConnected(ir);
                        }
                    }
                }
                // Then exclude all unmarked individuals (i.e. not in visited list)
                m_gedcom.PruneUnmarked();

                // Remove visited list
                m_gedcom.EndPruning();
            }
            catch (System.Exception ex)
            {
                ReportPruneError(ex);
            }

            // Rebuild list
            FillIndividualsList(m_listviewPruneRecordsIndis, false, null, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(m_nPruneExcluded, m_nPruneIncluded, "individual");
        }

        // Event handler
        private void pruneIndividualsContextMenuDescendantsExc_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            m_nPruneExcluded = 0;
            m_nPruneIncluded = 0;

            try
            {
                // exclude all individuals descended from this person, including spouses and spouses ancestors. 
                if (m_listviewPruneRecordsIndis.SelectedItems.Count == 1)
                {
                    ListViewItem lvi = m_listviewPruneRecordsIndis.SelectedItems[0];
                    if (lvi is CListableBool)
                    {
                        LLClasses.CIndividualRecord ir = (LLClasses.CIndividualRecord)((CListableBool)((ListViewItem)lvi)).ISRecord;
                        if (ir != null)
                        {
                            m_gedcom.BeginPruning(); // Initialises visisted hash table
                            m_gedcom.PruneDescendants(ir, true);
                            m_gedcom.EndPruning(); // Destroys visited hash table
                        }
                    }
                }
            }
            catch (System.Exception ex)
            {
                ReportPruneError(ex);
            }

            // Rebuild list
            FillIndividualsList(m_listviewPruneRecordsIndis, false, null, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(m_nPruneExcluded, m_nPruneIncluded, "individual");

        }

        // Event handler
        private void pruneIndividualsContextMenuDescendantsInc_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            m_nPruneExcluded = 0;
            m_nPruneIncluded = 0;

            try
            {
                // exclude all individuals descended from this person, including spouses and spouses ancestors. 
                if (m_listviewPruneRecordsIndis.SelectedItems.Count == 1)
                {
                    ListViewItem lvi = m_listviewPruneRecordsIndis.SelectedItems[0];
                    if (lvi is CListableBool)
                    {
                        LLClasses.CIndividualRecord ir = (LLClasses.CIndividualRecord)((CListableBool)((ListViewItem)lvi)).ISRecord;
                        if (ir != null)
                        {
                            m_gedcom.BeginPruning();
                            m_gedcom.PruneDescendants(ir, false);
                            m_gedcom.EndPruning();
                        }
                    }
                }
            }
            catch (System.Exception ex)
            {
                ReportPruneError(ex);
            }

            // Rebuild lists
            FillIndividualsList(m_listviewPruneRecordsIndis, false, null, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(m_nPruneExcluded, m_nPruneIncluded, "individual");
        }

        // Event handler
        private void pruneIndividualsContextMenuAncestorsExc_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            m_nPruneExcluded = 0;
            m_nPruneIncluded = 0;

            try
            {
                // Exclude all individuals descended from this person, including spouses and spouses ancestors. 
                if (m_listviewPruneRecordsIndis.SelectedItems.Count == 1)
                {
                    ListViewItem lvi = m_listviewPruneRecordsIndis.SelectedItems[0];
                    if (lvi is CListableBool)
                    {
                        LLClasses.CIndividualRecord ir = (LLClasses.CIndividualRecord)((CListableBool)((ListViewItem)lvi)).ISRecord;
                        if (ir != null)
                        {
                            m_gedcom.BeginPruning();
                            m_gedcom.PruneAncestors(ir, true);
                            m_gedcom.EndPruning();
                        }
                    }
                }
            }
            catch (System.Exception ex)
            {
                ReportPruneError(ex);
            }

            // Rebuild lists
            FillIndividualsList(m_listviewPruneRecordsIndis, false, null, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(m_nPruneExcluded, m_nPruneIncluded, "individual");
        }

        // Event handler
        private void pruneIndividualsContextMenuAncestorsInc_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            m_nPruneExcluded = 0;
            m_nPruneIncluded = 0;

            try
            {
                // Exclude all individuals descended from this person, including spouses and spouses ancestors. 
                if (m_listviewPruneRecordsIndis.SelectedItems.Count == 1)
                {
                    ListViewItem lvi = m_listviewPruneRecordsIndis.SelectedItems[0];
                    if (lvi is CListableBool)
                    {
                        LLClasses.CIndividualRecord ir = (LLClasses.CIndividualRecord)((CListableBool)((ListViewItem)lvi)).ISRecord;
                        if (ir != null)
                        {
                            m_gedcom.BeginPruning();
                            m_gedcom.PruneAncestors(ir, false);
                            m_gedcom.EndPruning();
                        }
                    }
                }
            }
            catch (System.Exception ex)
            {
                ReportPruneError(ex);
            }

            // Rebuild lists
            FillIndividualsList(m_listviewPruneRecordsIndis, false, null, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(m_nPruneExcluded, m_nPruneIncluded, "individual");
        }

        // Event handler
        private void pruneIndividualsContextMenuInclude_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            m_nPruneExcluded = 0;
            m_nPruneIncluded = 0;

            foreach (ListViewItem lvi in m_listviewPruneRecordsIndis.Items)
            {
                if (lvi is CListableBool)
                {
                    LLClasses.CIndividualRecord ir = (LLClasses.CIndividualRecord)((CListableBool)lvi).ISRecord;

                    if (ir.Restricted)
                    {
                        ir.Restricted = false;
                        ++m_nPruneIncluded;
                    }
                }
            }

            // Rebuild lists
            FillIndividualsList(m_listviewPruneRecordsIndis, false, null, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();


            ShowPruneResult(0, m_nPruneIncluded, "individual");

        }

        // Event handler
        // Removes pictures from the selected source
        private void pruneSourcesContextMenuRemovePics_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            int nHidden = 0;
            foreach (ListViewItem lvi in m_listviewPruneRecordsSources.SelectedItems)
            {
                if (lvi is CListableBool)
                {
                    LLClasses.CSourceRecord sr = (LLClasses.CSourceRecord)((CListableBool)((ListViewItem)lvi)).ISRecord;
                    if (sr != null)
                    {
                        int nHiddenThisTime = sr.SetAllMFRsVisible(false);
                        nHidden += nHiddenThisTime;
                        if (nHiddenThisTime > 0)
                        {
                            SetSourceSubItems((CListableBool)lvi, sr, true); // Updates list
                        }
                    }
                }
            }

            // Rebuild lists
            FillSourcesList(m_listviewPruneRecordsSources, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowHidePicsResult(nHidden);

            if (nHidden > 0)
            {
                m_bPrunepanelDataChanged = true;
            }
            EnablePrunePanelButtons();
        }

        // Event handler
        private void pruneIndividualsContextMenuExclude_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            m_nPruneExcluded = 0;
            m_nPruneIncluded = 0;

            foreach (ListViewItem lvi in m_listviewPruneRecordsIndis.Items)
            {
                if (lvi is CListableBool)
                {
                    LLClasses.CIndividualRecord ir = (LLClasses.CIndividualRecord)((CListableBool)lvi).ISRecord;
                    if (!ir.Restricted)
                    {
                        ir.Restricted = true;
                        m_nPruneExcluded++;
                    }
                }
            }

            // Rebuild lists
            FillIndividualsList(m_listviewPruneRecordsIndis, false, null, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(m_nPruneExcluded, 0, "individual");

        }

        // Event handler
        private void pruneSourcesContextMenuInclude_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            m_nPruneExcluded = 0;
            m_nPruneIncluded = 0;

            foreach (ListViewItem lvi in m_listviewPruneRecordsSources.Items)
            {
                if (lvi is CListableBool)
                {
                    LLClasses.CSourceRecord sr = (LLClasses.CSourceRecord)((CListableBool)lvi).ISRecord;
                    if (sr.Restricted)
                    {
                        m_nPruneIncluded++;
                        sr.Restricted = false;
                    }
                }
            }

            // Rebuild list
            FillSourcesList(m_listviewPruneRecordsSources, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(0, m_nPruneIncluded, "source");

        }

        // Event handler
        private void pruneSourcesContextMenuExclude_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            m_nPruneExcluded = 0;
            m_nPruneIncluded = 0;

            foreach (ListViewItem lvi in m_listviewPruneRecordsSources.Items)
            {
                if (lvi is CListableBool)
                {
                    LLClasses.CSourceRecord sr = (LLClasses.CSourceRecord)((CListableBool)lvi).ISRecord;
                    if (!sr.Restricted)
                    {
                        sr.Restricted = true;
                        m_nPruneExcluded++;
                    }
                }
            }

            // Rebuild list
            FillSourcesList(m_listviewPruneRecordsSources, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(m_nPruneExcluded, 0, "source");

        }

        // Event handler
        // Excludes people who aren't dead, but leave people we're not sure about
        private void pruneIndividualsContextMenuAlive_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            m_nPruneExcluded = 0;
            m_nPruneIncluded = 0;

            CPGDate dateNow = new CPGDate(DateTime.Now);
            CPGDate dateThen = new CPGDate(DateTime.Now);
            dateThen.m_year.m_nYear -= 100;

            try
            {
                m_gedcom.BeginPruning();

                foreach (ListViewItem lvi in m_listviewPruneRecordsIndis.Items)
                {
                    if (lvi is CListableBool)
                    {
                        LLClasses.CIndividualRecord ir = (LLClasses.CIndividualRecord)((CListableBool)lvi).ISRecord;
                        if (ir != null)
                        {
                            CPGDate dateBorn = null;
                            CPGQualifiedDate qualdateBorn = ir.BirthDate;
                            if (qualdateBorn != null)
                            {
                                dateBorn = qualdateBorn.m_date;
                            }

                            CPGDate dateDied = null;
                            CPGQualifiedDate qualdateDied = ir.DeathDate;
                            if (qualdateDied != null)
                            {
                                dateDied = qualdateDied.m_date;
                            }

                            bool bInclude = true;

                            if (ir.Living)
                            {
                                bInclude = false;
                            }
                            else if (IsAlive(dateBorn, dateDied, dateNow, dateThen))
                            {
                                bInclude = false;
                            }

                            if (bInclude == false && !ir.Restricted)
                            {
                                m_nPruneExcluded++;
                                ir.Restricted = true;
                            }
                        }
                    }
                }

                m_gedcom.EndPruning();
            }
            catch (System.Exception ex)
            {
                ReportPruneError(ex);
            }

            // Rebuild list
            FillIndividualsList(m_listviewPruneRecordsIndis, false, null, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(m_nPruneExcluded, m_nPruneIncluded, "individual");

        }

        // Event handler
        private void pruneIndividualsContextMenu_popup(System.Object sender, System.EventArgs e)
        {
            int nSelected = m_listviewPruneRecordsIndis.SelectedItems.Count;
            m_menuitemPruneRecordsIndisUnconnected.Enabled = (nSelected > 0);
            if (nSelected <= 1)
            {
                m_menuitemPruneRecordsIndisUnconnected.Text = "E&xclude individuals unless navigable from this person";
            }
            else
            {
                m_menuitemPruneRecordsIndisUnconnected.Text = String.Format("E&xclude individuals unless navigable from these {0} people", nSelected);
            }

            m_menuitemPruneRecordsIndisDescendantsExc.Enabled = (nSelected == 1);
            m_menuitemPruneRecordsIndisDescendantsInc.Enabled = (nSelected == 1);
            m_menuitemPruneRecordsIndisAncestorsExc.Enabled = (nSelected == 1);
            m_menuitemPruneRecordsIndisAncestorsInc.Enabled = (nSelected == 1);
            m_menuitemPruneRecordsIndisDetails.Enabled = (nSelected == 1);
        }

        // Event handler
        private void pruneSourcesContextMenu_popup(System.Object sender, System.EventArgs e)
        {
            int nSelected = m_listviewPruneRecordsSources.SelectedItems.Count;
            m_menuitemPruneRecordsSourcesDetails.Enabled = (nSelected == 1);
            m_menuitemPruneRecordsSourcesRemovePics.Enabled = (nSelected > 0);
            if (nSelected <= 1)
            {
                m_menuitemPruneRecordsSourcesRemovePics.Text = "&Remove pictures from this source";
            }
            else
            {
                m_menuitemPruneRecordsSourcesRemovePics.Text = String.Format("&Remove pictures from these {0} sources", nSelected);
            }
        }

        // Event handler
        private void listviewPruneRecordsIndis_ItemCheck(object sender, ItemCheckEventArgs e)
        {
            if (!m_bDisablePrunepanelCheckEvent)
            {
                CListableBool lb = (CListableBool)m_listviewPruneRecordsIndis.Items[e.Index];
                // For some reason if user presses control while clicking any row of the list, it causes a check event. We don't want any checking to happen in that case.
                if ((Control.ModifierKeys & Keys.Control) == 0)
                {
                    if (e.NewValue == CheckState.Checked && lb.ISRecord.Restricted
                        || e.NewValue == CheckState.Unchecked && !lb.ISRecord.Restricted)
                    {
                        lb.SetRestricted(e.NewValue == CheckState.Unchecked);
                        m_bPrunepanelDataChanged = true;
                        EnablePrunePanelButtons();
                    }
                }
                else
                {
                    if (lb.ISRecord != null)
                    {
                        e.NewValue = lb.ISRecord.Restricted ? CheckState.Unchecked : CheckState.Checked;
                    }
                }
            }
        }

        // Event handler
        private void listviewPruneRecordsSources_ItemCheck(object sender, ItemCheckEventArgs e)
        {
            if (!m_bDisablePrunepanelCheckEvent)
            {
                CListableBool lb = (CListableBool)m_listviewPruneRecordsSources.Items[e.Index];

                // For some reason if user presses control while clicking any row of the list, it causes a check event. We don't want any checking to happen in that case.
                if ((Control.ModifierKeys & Keys.Control) == 0)
                {
                    if (e.NewValue == CheckState.Checked && lb.ISRecord.Restricted
                     || e.NewValue == CheckState.Unchecked && !lb.ISRecord.Restricted)
                    {
                        lb.SetRestricted(e.NewValue == CheckState.Unchecked);
                        m_bPrunepanelDataChanged = true;
                        EnablePrunePanelButtons();
                    }
                }
                else
                {
                    if (lb.ISRecord != null)
                    {
                        e.NewValue = lb.ISRecord.Restricted ? CheckState.Unchecked : CheckState.Checked;
                    }
                }
            }
        }

        ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Public methods /////////////////////////////////////////////////////////////////////////////////////////////////
        ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // Presents a file selection dialog and returns the selecetd file name and path
        public static bool SelectFile(ref string sFileDir, ref string sFileName, string sTitle, string sDefaultName, bool bLoadNotSave, string sFilterName, ArrayList alFilterExtensions)
        {
            bool bFileSelected = false;

            FileDialog fileDialog;
            if (bLoadNotSave)
            {
                fileDialog = new OpenFileDialog();
            }
            else
            {
                fileDialog = new SaveFileDialog();
            }

            if (sFileDir.Length > 0)
            {
                fileDialog.InitialDirectory = sFileDir;
            }
            else
            {
                fileDialog.InitialDirectory = Environment.GetFolderPath(System.Environment.SpecialFolder.Personal);
            }

            if (sFileName.Length > 0)
            {
                fileDialog.FileName = sFileName;
            }
            else
            {
                fileDialog.FileName = sDefaultName;
            }

            string sFilterString = "";
            int nFilterAllIndex = 1;
            if (alFilterExtensions.Count > 0)
            {
                nFilterAllIndex++;
                string sFilterCode = "";
                bool bFirst = true;
                //"Picture files (*.jpg; *.gif)|*.jpg;*.gif|All files (*.*)|*.*";
                foreach (string sFilterExtn in alFilterExtensions)
                {
                    if (!bFirst)
                    {
                        sFilterCode += ";";
                    }
                    else
                    {
                        bFirst = false;
                    }
                    sFilterCode += "*" + sFilterExtn;
                }
                sFilterString = sFilterName + " (" + sFilterCode + ")|" + sFilterCode + "|";
            }
            sFilterString += "All files (*.*)|*.*";
            fileDialog.Filter = sFilterString;
            fileDialog.FilterIndex = 1;
            string sExtn = Path.GetExtension(fileDialog.FileName);

            // Check whether selected file matches given filter
            bool bValidExtn = true;
            if (fileDialog.FileName.Length > 0)
            {
                bValidExtn = false;
                string sExtnFromDlg = Path.GetExtension(fileDialog.FileName).ToUpper();
                foreach (string sFilterExtn in alFilterExtensions)
                {
                    if (sExtnFromDlg == sFilterExtn.ToUpper())
                    {
                        bValidExtn = true;
                        break;
                    }
                }
            }

            if (!bValidExtn)
            {
                // Use *.* filter if default file isn't a .txt file.
                fileDialog.FilterIndex = nFilterAllIndex;
            }
            fileDialog.RestoreDirectory = true;
            fileDialog.Title = sTitle;

            if (fileDialog.ShowDialog() == DialogResult.OK)
            {
                bFileSelected = true;
                sFileDir = Path.GetDirectoryName(fileDialog.FileName);
                sFileName = Path.GetFileName(fileDialog.FileName);
            }
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Selected file : " + sFileDir + "\\" + sFileName);

            return (bFileSelected);
        }

        // Modifies rectNew to fit within the limits given, keeping its aspect ratio
        public static void ScaleAreaToFit(ref Rectangle rectNew, uint uMaxWidth, uint uMaxHeight)
        {
            if (rectNew.Height > uMaxHeight)
            {
                // Image won't fit horizontally, so scale in both directions til it will
                rectNew.Width = (rectNew.Width * (int)uMaxHeight) / rectNew.Height;
                rectNew.Height = (int)uMaxHeight;
            }

            if (rectNew.Width > uMaxWidth)
            {
                // Image won't fit horizontally, so scale in both directions til it will
                rectNew.Height = (rectNew.Height * (int)uMaxWidth) / rectNew.Width;
                rectNew.Width = (int)uMaxWidth;
            }
        }

        // Accessor
        public string InputFile
        {
            get
            {
                return m_textboxChooseGedcom.Text;
            }
            set
            {
                m_textboxChooseGedcom.Text = value;
            }
        }

        // Returns the name of the alternative picture file to display for non-diaplayable files of the given format
        public static string NonPicFilename(string sFormat, bool bSmall, bool bClickToDownload)
        {
            string sFilename;
            switch (sFormat.ToLower())
            {
                case "wav":
                case "mp3":
                case "mid":
                case "midi":
                case "rmi":
                case "au":
                case "wma":
                    sFilename = bSmall ? "gmaudio_sm.png" : bClickToDownload ? "gmaudio.png" : "gmaudion.png";
                    break;
                case "avi":
                case "mpeg":
                case "mpg":
                case "wmv":
                    sFilename = bSmall ? "gmvideo_sm.png" : bClickToDownload ? "gmvideo.png" : "gmvideon.png";
                    break;
                default:
                    sFilename = bSmall ? "gmdoc_sm.png" : bClickToDownload ? "gmdoc.png" : "gmdocn.png";
                    break;
            }
            return sFilename;

        }

        // Brings up a CRecordDetailsForm for the given individual
        public void ShowIndividualDetailsDialog(Form formParent, CListableBool lbItem, LLClasses.CIndividualRecord ir, bool bCanEditPictures, bool bCheckBoxes)
        {
            CRecordDetailsForm detailsForm = new CRecordDetailsForm(formParent, ir, m_gedcom, bCanEditPictures, false);
            detailsForm.ShowDialog(this);
            if (lbItem != null && ir != null)
            {
                SetIndividualSubItems(lbItem, ir, bCheckBoxes);
            }
            EnablePrunePanelButtons();
        }

        ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Private methods ////////////////////////////////////////////////////////////////////////////////////////////////
        ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        // Shows the settings panel
        private void SwitchConfigPanelOn()
        {
            // Disable edit boxes etc. on previous panel to avoid confusing users
            EnableCurrentPanel(false);

            // Move help button to its new location
            m_buttonHelp.Location = new System.Drawing.Point(8, 288);

            // Flag panel as being on
            m_bConfigPanelOn = true;

            // Enable reset button
            m_buttonSettingsCancel.Visible = true;

            // Disable buttons while config panel shown
            m_buttonNext.Visible = false;
            m_buttonBack.Visible = false;
            m_buttonCancel.Visible = false; // To give the panel a "modal" feeling

            // Make config button an "OK" button
            m_buttonSettings.Text = m_sConfigButtonTextOff;
            m_buttonSettings.Location = new System.Drawing.Point(344, 288);
            m_buttonSettings.Size = new System.Drawing.Size(m_ptDefaultButtonSize);

            // Enable config panel
            EnableCurrentPanel(true);
            ShowCurrentPanel();
        }

        // Hides the settings panel
        private void SwitchConfigPanelOff()
        {
            // Disable edit boxes etc. on config panel
            EnableCurrentPanel(false);

            // Move help button to its usual location
            m_buttonHelp.Location = new System.Drawing.Point(186, 288);

            // Flag panel as being off
            m_bConfigPanelOn = false;

            // Restore buttons states
            // Done by ShowCurrentPanel()

            // Make config button back to a config button
            m_buttonSettings.Text = m_sConfigButtonTextOn;
            m_buttonSettings.Location = new System.Drawing.Point(88, 288);
            m_buttonSettings.Size = new System.Drawing.Size(m_ptConfigButtonSize);

            // Enable generic panel
            EnableCurrentPanel(true);

            // Disable reset button
            m_buttonSettingsCancel.Visible = false;

            // ShowCurrentPanel() also restores visibility of back button.
            ShowCurrentPanel();
            m_buttonNext.Visible = true;
            m_buttonCancel.Visible = true;

            // Back button should always be enabled (tho' sometimes not bVisible!)
            m_buttonBack.Enabled = true;

        }

        // Shows the current panel and associated wizard buttons, selected by m_currentPanel, and hides all the others.
        private void ShowCurrentPanel()
        {
            // Making panel3 bVisible calls check event on list view!
            m_bDisablePrunepanelCheckEvent = true; 

            if (m_bConfigPanelOn)
            {
                m_panelWelcome.Visible = false;
                m_panelChooseGedcom.Visible = false;
                m_panelPruneRecords.Visible = false;
                m_panelSelectKey.Visible = false;
                m_panelChooseOutput.Visible = false;
                m_panelAllDone.Visible = false;
                m_tabcontrolConfigPanel.Visible = true;
            } // End showing config panel
            else
            {
                m_panelWelcome.Visible = (m_nCurrentPanel == 1);
                m_panelChooseGedcom.Visible = (m_nCurrentPanel == 2);
                m_panelPruneRecords.Visible = (m_nCurrentPanel == 3);
                m_panelSelectKey.Visible = (m_nCurrentPanel == 4);
                m_panelChooseOutput.Visible = (m_nCurrentPanel == 5);
                m_panelAllDone.Visible = (m_nCurrentPanel == 6);
                m_tabcontrolConfigPanel.Visible = false;

                if (m_nCurrentPanel <= 1)
                {
                    m_buttonBack.Visible = false;
                }
                else
                {
                    m_buttonBack.Visible = true;
                }

                // Config button disappears once html created
                if (m_nCurrentPanel >= 6) 
                {
                    m_buttonSettings.Visible = false;
                }
                else
                {
                    m_buttonSettings.Visible = true;
                }

                if (m_nCurrentPanel == 6)
                {
                    m_buttonCancel.Text = "&Finish";
                    // Can't go back , because we can't undo the file creations.
                    m_buttonBack.Visible = false; 
                    m_buttonHelp.Location = new System.Drawing.Point(8, 288);
                    m_buttonCancel.Location = new System.Drawing.Point(424, 288);
                    m_buttonNext.Visible = false;
                }
                else if (m_nCurrentPanel == 9)
                {
                    m_buttonHelp.Location = new System.Drawing.Point(8, 288);
                    m_buttonNext.Text = "&Finish";
                    m_buttonCancel.Visible = false;
                    m_buttonHelp.Visible = false;
                    // Can't go back , because we can't undo the file creations.
                    m_buttonBack.Visible = false; 
                }
                else if (m_nCurrentPanel == 2)
                {
                    m_textboxChooseGedcom.Focus();
                    m_textboxChooseGedcom.SelectAll();
                }
                else if (m_nCurrentPanel == 4)
                {
                    m_textboxSelectKey.Focus();
                    m_textboxSelectKey.SelectAll();
                }
                else if (m_nCurrentPanel == 5)
                {
                    m_textboxChooseOutput.Focus();
                    m_textboxChooseOutput.SelectAll();
                }
                else
                {
                    m_buttonNext.Text = "&Next >";
                    m_buttonCancel.Visible = true;
                    m_buttonCancel.Text = "&Quit";
                    m_buttonCancel.Location = new System.Drawing.Point(8, 288);
                    m_buttonHelp.Visible = true;
                }

                if (m_nCurrentPanel == 3)
                {
                    EnablePrunePanelButtons();
                }

                EnableNextButton();
            } // End showing generic panels

            m_bDisablePrunepanelCheckEvent = false;
        }

        // Reads the MFRs from the changes file (MFR here is for the user's modifications to the multimedia attached to a record)
        private bool LoadMFRs(CISRecord isr, CISRecordChanges isrc)
        {
            bool bDifferencesDetected = false;
            for (int n = isr.m_alUniqueFileRefs.Count - 1; n >= 0; --n) // Can't use foreach, because then Remove() would corrupt iterator.
            {
                CMultimediaFileReference mfr = (CMultimediaFileReference)(isr.m_alUniqueFileRefs[n]);
                if (mfr.m_bFromGEDCOM == false)
                {
                    isr.m_alUniqueFileRefs.RemoveAt(n);
                }
                else
                {
                    mfr.m_bVisible = false;
                }
            }
            foreach (CMultimediaFileReference mfr in isrc.m_alMfrs)
            {
                CMultimediaFileReference mfrFound = null;
                foreach (CMultimediaFileReference mfrUnique in isr.m_alUniqueFileRefs)
                {
                    if (mfr.m_bEmbedded && mfrUnique.m_bEmbedded)
                    {
                        if (mfr.m_xrefEmbedded == mfrUnique.m_xrefEmbedded)
                        {
                            mfrFound = mfrUnique;
                            break;
                        }
                    }
                    if (!mfr.m_bEmbedded && !mfrUnique.m_bEmbedded)
                    {
                        if (mfr.m_sMultimediaFileReference == mfrUnique.m_sMultimediaFileReference)
                        {
                            mfrFound = mfrUnique;
                            break;
                        }
                    }
                }
                if (mfrFound == null)
                {
                    //Must be a user added file. Add it to unique mfrs
                    if (File.Exists(mfr.m_sMultimediaFileReference))
                    {
                        isr.m_alUniqueFileRefs.Add(mfr);
                        if (mfr.m_bFromGEDCOM)
                        {
                            // User must have removed this file in the interim
                            bDifferencesDetected = true; 
                        }
                    }
                    else
                    {
                        // File has been deleted or moved in the interim
                        bDifferencesDetected = true; 
                    }
                }
                else
                {
                    if (mfrFound.m_bEmbedded)
                    {
                        string sKeepEmbeddedFilename = mfrFound.m_sMultimediaFileReference;
                        mfrFound.CopyFrom(mfr);
                        mfrFound.m_sMultimediaFileReference = sKeepEmbeddedFilename;
                    }
                    else
                    {
                        mfrFound.CopyFrom(mfr);
                    }
                }
            }
            // Fix duplicates of order values
            foreach (CMultimediaFileReference imfr in isr.m_alUniqueFileRefs)
            {
                foreach (CMultimediaFileReference dimfr in isr.m_alUniqueFileRefs)
                {
                    int nOrderIndex = imfr.m_nOrderIndex;
                    if (imfr != dimfr && nOrderIndex == dimfr.m_nOrderIndex)
                    {
                        foreach (CMultimediaFileReference iimfr in isr.m_alUniqueFileRefs)
                        {
                            if (iimfr.m_nOrderIndex >= nOrderIndex)
                            {
                                iimfr.m_nOrderIndex += 1;
                            }
                        }
                        imfr.m_nOrderIndex = nOrderIndex;
                    }
                }
            }
            return (bDifferencesDetected);
        }

        // Parses the changes file
        private Hashtable ParseSelectionFile(string filename, ref EVersion eVersion)
        {
            Hashtable htSelection = new Hashtable();
            FileStream fileStream = null;
            StreamReader streamReader = null;
            uint uLineNumber = 1;
            string sVersion = "";

            try
            {
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, String.Format("opening changes file {0}", filename));

                fileStream = new FileStream(filename, FileMode.Open, FileAccess.Read);
                streamReader = new StreamReader(fileStream, System.Text.Encoding.ASCII);

                string sLine;
                sLine = streamReader.ReadLine();
                // First gedcomLine should be version number
                if (sLine.Length >= 8 && sLine.Substring(0, 11) == "// GEDmill ")
                {
                    sVersion = sLine.Substring(11);
                }
                if (sVersion.Length < 5 || m_sSoftwareVersion.Length < 5)
                {
                    eVersion = EVersion.pre1p8;
                }
                else
                {
                    switch (sVersion.Substring(0, 6))
                    {
                        case "1.10.0":
                        case "1.10.1":
                        case "1.10.2":
                        case "1.10.3":
                        case "1.10.4":
                        case "1.11.0":
                            eVersion = EVersion.v1p10;
                            break;
                        default:
                            switch (sVersion.Substring(0, 5))
                            {
                                case "1.8.0":
                                case "1.8.1":
                                case "1.8.2":
                                case "1.8.3":
                                case "1.8.4":
                                case "1.8.5":
                                case "1.8.6":
                                case "1.8.7":
                                case "1.8.8":
                                case "1.8.9":
                                    eVersion = EVersion.v1p8;
                                    break;
                                case "1.9.0":
                                case "1.9.1":
                                case "1.9.2":
                                    eVersion = EVersion.v1p9;
                                    break;
                            }
                            break;
                    }
                }
                if (eVersion == EVersion.pre1p8)
                {
                    if (sLine.IndexOf("GEDmill") == -1)
                    {
                        DialogResult dialogResult = MessageBocx.Show(this, "The file you have selected is not a valid GEDmill file.", "Invalid File",
                            MessageBoxButtons.OK, MessageBoxIcon.Exclamation, false);
                        return null;
                    }
                    sVersion = "pre 1.8.0";
                }
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, String.Format("version is {0}:{1}", sVersion, sLine));

                CISRecordChanges isrc = null;
                while ((sLine = streamReader.ReadLine()) != null)
                {
                    string xref = "";
                    string sSecondary = "";
                    // Parse characters in gedcomLine until whitespace or // found
                    int n = 0;
                    bool bSecondary = false;
                    while (n < sLine.Length)
                    {
                        char c = sLine[n];
                        if (char.IsWhiteSpace(c))
                        {
                            if (xref.Length == 0)
                            {
                                // Must be a secondary data gedcomLine
                                bSecondary = true;
                            }
                            else
                            {
                                // Whitespace at end of xref gedcomLine
                                break;
                            }
                        }
                        if ((c == '#')) 
                        {
                            // # is the new comment starter. (Used to be // but that screwed up //machine/file/path/filenames )
                            break;
                        }
                        if (bSecondary)
                        {
                            sSecondary += c;
                        }
                        else
                        {
                            xref += c;
                        }
                        n++;
                    }
                    if (xref.Length > 0)
                    {
                        isrc = new CISRecordChanges(true);
                        htSelection[xref] = isrc;
                    }
                    if (sSecondary.Length > 0)
                    {
                        ParseSecondary(ref isrc, sSecondary);
                    }
                    uLineNumber++;
                }
            }
            catch (Exception e)
            {
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, String.Format("A problem was encountered while reading the file : {0} line {1}", e.ToString(), uLineNumber));
            }
            finally
            {
                if (streamReader != null)
                {
                    streamReader.Close();
                }

                if (fileStream != null)
                {
                    fileStream.Close();
                }
            }
            return htSelection;
        }

        // Helps in parsing the changes file
        private void ParseSecondary(ref CISRecordChanges isrc, string sSecondary)
        {
            string sCmd = "";
            string sArg = "";
            string sNum = "";
            int nState = 0;
            foreach (char c in sSecondary)
            {
                switch (nState)
                {
                    case 0:
                        // Leading whitespace
                        if (!char.IsWhiteSpace(c))
                        {
                            sNum += c;
                            nState = 1;
                        }
                        break;
                    case 1:
                        // Number
                        if (char.IsDigit(c))
                        {
                            sNum += c;
                        }
                        else
                        {
                            if (!char.IsWhiteSpace(c))
                            {
                                sCmd += c;
                                nState = 3;
                            }
                            nState = 2;
                        }
                        break;
                    case 2:
                        // Space after number
                        if (!char.IsWhiteSpace(c))
                        {
                            sCmd += c;
                            nState = 3;
                        }
                        break;
                    case 3:
                        if (!char.IsWhiteSpace(c))
                        {
                            sCmd += c;
                        }
                        else
                        {
                            nState = 4;
                        }
                        break;
                    case 4:
                        // Space after cmd
                        if (!char.IsWhiteSpace(c))
                        {
                            sArg += c;
                            nState = 5;
                        }
                        break;
                    case 5:
                        sArg += c;
                        break;
                }

                if (nState == 99)
                {
                    // End of string parsing
                    break; 
                }
            }

            if (isrc != null)
            {
                switch (sCmd)
                {
                    case "OBJE":
                        {
                            CMultimediaFileReference mfr = new CMultimediaFileReference(m_gedcom);
                            mfr.m_bFromGEDCOM = false;
                            isrc.m_alMfrs.Add(mfr);
                            isrc.m_mfrCurrent = mfr;
                            break;
                        }
                    case "FILE":
                        if (isrc.m_mfrCurrent != null)
                        {
                            sArg = sArg.Replace('/', '\\');
                            isrc.m_mfrCurrent.m_sMultimediaFileReference = sArg;
                        }
                        break;
                    case "FORM":
                        if (isrc.m_mfrCurrent != null)
                        {
                            isrc.m_mfrCurrent.m_sMultimediaFormat = sArg;
                        }
                        break;
                    case "TITL":
                        if (isrc.m_mfrCurrent != null)
                        {
                            isrc.m_mfrCurrent.m_sDescriptiveTitle = sArg;
                        }
                        break;
                    case "_ORDER":
                        if (isrc.m_mfrCurrent != null)
                        {
                            isrc.m_mfrCurrent.m_nOrderIndex = int.Parse(sArg);
                        }
                        break;
                    case "_VISIBLE":
                        if (isrc.m_mfrCurrent != null)
                        {
                            isrc.m_mfrCurrent.m_bVisible = (sArg.ToUpper() == "TRUE") ? true : false;
                        }
                        break;
                    case "_GEDCOM":
                        if (isrc.m_mfrCurrent != null)
                        {
                            isrc.m_mfrCurrent.m_bFromGEDCOM = (sArg.ToUpper() == "TRUE") ? true : false;
                        }
                        break;
                    case "_INLINE":
                        if (isrc.m_mfrCurrent != null)
                        {
                            isrc.m_mfrCurrent.m_bEmbedded = true;
                            isrc.m_mfrCurrent.m_xrefEmbedded = sArg;
                        }
                        break;
                    case "_REGION":
                        if (isrc.m_mfrCurrent != null)
                        {
                            int nFirstComma = sArg.IndexOf(',');
                            if (nFirstComma > 0 && nFirstComma + 1 < sArg.Length)
                            {
                                string sAsid = sArg.Substring(0, nFirstComma);
                                string sArea = sArg.Substring(nFirstComma + 1);
                                CAsidPair asidPair = new CAsidPair(sAsid, sArea);
                                isrc.m_mfrCurrent.m_asidPair = asidPair;
                            }
                        }
                        break;
                }
            }
            return;
        }

        // Saves the MFR to the changes file (MFR here is for the user's modifications to the multimedia attached to a record)
        private void SaveMFR(StreamWriter sw, CMultimediaFileReference mfr)
        {
            sw.WriteLine(" 1 OBJE");
            sw.WriteLine(String.Concat(" 2 FORM ", mfr.m_sMultimediaFormat));
            sw.WriteLine(String.Concat(" 2 TITL ", mfr.m_sDescriptiveTitle));
            string sPicFilename = mfr.m_sMultimediaFileReference.Replace('\\', '/'); ;
            if (!mfr.m_bEmbedded)
            {
                sw.WriteLine(String.Concat(" 2 FILE ", sPicFilename));
            }
            else
            {
                sw.WriteLine(" 2 FILE inline");
            }
            sw.WriteLine(String.Concat(" 2 _ORDER ", mfr.m_nOrderIndex.ToString()));
            sw.WriteLine(String.Concat(" 2 _VISIBLE ", mfr.m_bVisible ? "true" : "false"));
            sw.WriteLine(String.Concat(" 2 _GEDCOM ", mfr.m_bFromGEDCOM ? "true" : "false"));
            if (mfr.m_bEmbedded)
            {
                sw.WriteLine(String.Concat(" 2 _INLINE ", mfr.m_xrefEmbedded));
            }
            if (mfr.m_asidPair != null)
            {
                sw.WriteLine(String.Concat(" 2 _REGION ", mfr.m_asidPair.m_sAsid, ",{", mfr.m_asidPair.m_rectArea.Top, ",", mfr.m_asidPair.m_rectArea.Left, ",", mfr.m_asidPair.m_rectArea.Bottom, ",", mfr.m_asidPair.m_rectArea.Right, "}"));
            }
        }

        // Sets sFileDir and sFileName from user with file selection dialog
        // sFileDir is also used as the initial directory
        // sFileName is used as the initially selected file
        // Returns true if a file was actually selected
        // Only sets sFileDir and sFileName if file was actually selected.
        private static bool SelectFile(ref string sFileDir, ref string sFileName, string sTitle, string sDefaultName, bool bLoadNotSave, string sFilterName, string sFilterExtension)
        {
            ArrayList alFilterExtensions = new ArrayList();
            alFilterExtensions.Add(sFilterExtension);
            return SelectFile(ref sFileDir, ref sFileName, sTitle, sDefaultName, bLoadNotSave, sFilterName, alFilterExtensions);
        }

        // Logic for the next page button to ensure that user has completed the current page
        private void EnableNextButton()
        {
            if (m_nCurrentPanel == 2 && InputFile.Length == 0)
            {
                m_buttonNext.Enabled = false;
            }
            else if (m_nCurrentPanel == 5 && m_textboxChooseOutput.Text.Length == 0)
            {
                m_buttonNext.Enabled = false;
            }
            else
            {
                m_buttonNext.Enabled = true;
            }
        }

        // Logic for the key individuals delete button checks that an individual is selected for deletion
        private void EnableKeyIndividualsDeleteButton()
        {
            if (m_listboxSelectKey.SelectedItems.Count > 0)
            {
                m_buttonSelectKeyDelete.Enabled = true;
            }
            else
            {
                m_buttonSelectKeyDelete.Enabled = false;
            }
        }

        // Logic for the mini tree config buttons
        private void EnableMiniTreeButtons()
        {
            bool bEnabled = m_checkboxConfigTreeDiagrams.Checked;

            m_checkboxConfigTreeDiagramsFakeBg.Enabled = bEnabled;
            m_labelConfigTreeDiagramsFormat.Enabled = bEnabled;
            m_comboboxConfigTreeDiagramsFormat.Enabled = bEnabled;
            m_groupboxMiniTreeColours.Enabled = bEnabled;
            m_buttonConfigMiniTreeColourIndiBackground.Enabled = bEnabled;
            m_buttonConfigMiniTreeColourIndiHighlight.Enabled = bEnabled;
            m_buttonConfigMiniTreeColourIndiBgConcealed.Enabled = bEnabled;
            m_buttonConfigMiniTreeColourIndiShade.Enabled = bEnabled;
            m_buttonConfigMiniTreeColourIndiText.Enabled = bEnabled;
            m_buttonConfigMiniTreeColourIndiLink.Enabled = bEnabled;
            m_buttonConfigMiniTreeColourBranch.Enabled = bEnabled;
            m_buttonConfigMiniTreeColourIndiBorder.Enabled = bEnabled;
            m_buttonConfigMiniTreeColourIndiFgConcealed.Enabled = bEnabled;
            m_checkboxConfigConserveTreeWidth.Enabled = bEnabled;
            m_checkboxConfigKeepSiblingOrder.Enabled = bEnabled;

            if (bEnabled)
            {
                SetMiniTreeColourConfigButtons();
            }
            else
            {
                ClearMiniTreeColourConfigButtons();
            }
        }

        // Fills the controls in each page of the app
        private void InitialiseCurrentPanel()
        {
            switch (m_nCurrentPanel)
            {
                case 2:
                    InputFile = s_config.m_sInputFilename;
                    m_textboxChooseGedcom.SelectionStart = InputFile.Length;
                    m_textboxChooseGedcom.SelectionLength = InputFile.Length;
                    break;
                case 4:
                    m_textboxSelectKey.Text = s_config.m_sTitle;
                    s_config.m_sFirstRecordXref = "";
                    FillKeyIndividualsList();
                    break;
                case 5:
                    m_textboxChooseOutput.Text = s_config.m_sOutputFolder;
                    m_textboxChooseOutput.SelectionStart = InputFile.Length;
                    m_textboxChooseOutput.SelectionLength = InputFile.Length;
                    break;
                case 6:
                    m_checkboxAllDoneShowSite.Visible = File.Exists(s_config.FrontPageURL);
                    m_checkboxAllDoneShowSite.Checked = s_config.m_bOpenWebsiteOnExit;
                    m_linklabelAllDone.Text = s_config.m_sOutputFolder;
                    if (s_config.m_sFrontPageFilename != "")
                    {
                        m_labelAllDoneStartFile.Text = String.Concat("(The front page for the website is the file ", s_config.m_sFrontPageFilename, ".", s_config.m_sHtmlExtension, ")");
                        m_labelAllDoneStartFile.Visible = true;
                    }
                    else
                    {
                        m_labelAllDoneStartFile.Text = "(No front page was generated.)";
                        m_labelAllDoneStartFile.Visible = false;
                    }
                    break;
                default:
                    break;
            }
            EnableNextButton();
        }

        // Handles the processing needed at each stage of the app, as the user moves through each page of the wizard.
        private bool ValidateCurrentPanel()
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "ValidateCurrentPanel()");
            DialogResult result;

            // Loop gives user the option to retry folder creation. Use return to exit.
            while (true) 
            {
                switch (m_nCurrentPanel)
                {
                    case 2:
                        if (File.Exists(InputFile) == false)
                        {
                            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "File not found.");

                            MessageBocx.Show(m_mainForm, "The file you have selected could not be found.", "File Not Found",
                                MessageBoxButtons.OK, MessageBoxIcon.Exclamation, false);

                            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "File not found. Returning. ");
                            return false;
                        }

                        ProgressWindow progressWindow = new ProgressWindow();
                        progressWindow.Text = "Reading GEDCOM file";

                        if (s_config.m_sOutputFolder == "" || s_config.m_sInputFilename != InputFile)
                        {
                            s_config.m_sOutputFolder = Path.GetDirectoryName(InputFile);
                            s_config.m_sOutputFolder += "\\GEDmill_Output";
                        }
                        if (s_config.m_sInputFilename != InputFile)
                        {
                            s_config.m_sInputFilename = InputFile;
                            s_config.m_sFirstRecordXref = "";

                            // User is using a new file, so key individuals won't be the same as they were in config
                            s_config.m_alKeyIndividuals = new ArrayList();
                            s_config.m_sFirstRecordXref = "";

                        }

                        m_gedcom.Filename = InputFile;
                        m_gedcom.DataMayStartWithWhitespace = s_config.m_bDataMayStartWithWhitespace;
                        m_gedcom.DataMayEndWithWhitespace = s_config.m_bDataMayEndWithWhitespace;
                        m_gedcom.ProgressCallback = progressWindow;

                        LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Reading GEDCOM file " + InputFile);

                        ThreadStart threadStart = new ThreadStart(m_gedcom.ParseFile);
                        Thread threadWorker = new Thread(threadStart);

                        LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Starting thread");

                        threadWorker.Start();
                        result = progressWindow.ShowDialog(this);
                        threadWorker.Join();

                        LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Thread finished, result=" + result.ToString());

                        if (result == DialogResult.Abort) // Abort means abnormal failure (ie. not user pressing cancel)
                        {
                            // Abort means there were file IO errors
                            MessageBocx.Show(m_mainForm, String.Format("A problem was encountered while reading the GEDCOM file:\r\n\r\n{0}", LogFile.TheLogFile.ErrorReport()), MainForm.m_sSoftwareName,
                                MessageBoxButtons.OK, MessageBoxIcon.Exclamation, true);
                        }
                        else if (result == DialogResult.Retry) // Something went wrong, let user retry
                        {
                            // Currently the only thing this can be is "file already open"
                            MessageBocx.Show(m_mainForm, "A problem was encountered while reading the GEDCOM file.\r\n\r\nPerhaps the file is already open elsewhere.", MainForm.m_sSoftwareName,
                                MessageBoxButtons.OK, MessageBoxIcon.Exclamation, true);
                        }
                        if (result != DialogResult.OK)
                        {
                            return false; // Go back and let user retry loading file.
                        }
                        m_bPrunepanelDataChanged = false; // A fresh file, no user changes yet.
                        FillIndividualsList(m_listviewPruneRecordsIndis, false, null, true);
                        FillSourcesList(m_listviewPruneRecordsSources, true);

                        return true;

                    case 3:
                        // Go through individuals list and set restricted flag as appropriate
                        bool bSomethingChecked = false;
                        foreach (ListViewItem li in m_listviewPruneRecordsIndis.Items)
                        {
                            bool bChecked = ((ListViewItem)li).Checked;
                            if (bChecked)
                            {
                                bSomethingChecked = true;
                            }
                            // Already done on click event: ((CListableBool)((ListViewItem)li)).SetRestricted( !bChecked );
                            if (!bChecked)
                            {
                                m_gedcom.RestrictAssociatedSources((LLClasses.CIndividualRecord)(((CListableBool)((ListViewItem)li)).ISRecord));
                            }
                        }

                        if (bSomethingChecked == false)
                        {
                            MessageBocx.Show(m_mainForm, "Please select at least one individual.", "No Individuals Selected",
                                MessageBoxButtons.OK, MessageBoxIcon.Exclamation, false);
                            return false;
                        }

                        if (m_bPrunepanelDataChanged)
                        {
                            DialogResult dialogResult = MessageBocx.Show(m_mainForm, "You have made changes which will affect the website but have not saved them.\r\nWould you like to save them now?", "Save changes",
                                MessageBoxButtons.YesNo, MessageBoxIcon.Question, false);
                            if (dialogResult == DialogResult.Yes)
                            {
                                buttonPruneRecordsSave_click(null, null);
                            }
                        }
                        return true;

                    case 4:
                        s_config.m_sTitle = m_textboxSelectKey.Text;
                        return true;

                    case 5:
                        s_config.m_sOutputFolder = m_textboxChooseOutput.Text;
                        string sImageFolder = s_config.m_sImageFolder;
                        string sOutputFolder = s_config.m_sOutputFolder;
                        if (sOutputFolder != "")
                        {
                            sOutputFolder = sOutputFolder + '\\';
                        }
                        string sAbsImageFolder = String.Concat(sOutputFolder, sImageFolder);

                        bool bPreserveFiles = false;
                        if (s_config.m_bPreserveFrontPage || s_config.m_bPreserveStylesheet)
                        {
                            // To generate warning if deleting folder & files.
                            bPreserveFiles = true; 
                        }

                        for (; ; )
                        {

                            result = PrepareOutputDirectory(sOutputFolder, bPreserveFiles);
                            if (result == DialogResult.Cancel)
                            {
                                return false;
                            }
                            if (result == DialogResult.OK)
                            {
                                break;
                            }
                        }
                        for (; ; )
                        {
                            result = PrepareOutputDirectory(sAbsImageFolder, false);
                            if (result == DialogResult.Cancel)
                            {
                                return false;
                            }
                            if (result == DialogResult.OK)
                            {
                                break;
                            }
                        }

                        if (CreateWebsite(sOutputFolder, sAbsImageFolder))
                        {
                            return true;
                        }

                        return false;

                    case 6:
                        return true;

                    default:
                        return true;
                }
            }
        }

        // Populates the list of individuals records for inclusion/exclusion in the website
        // alSelectedIndividuals is a small array indicating xrefs of those individuals to mark selected. (not currently used, 3Jan08)
        private void FillIndividualsList(SortableListView listView, bool bExcludeRestricted, ArrayList alSelectedIndividuals, bool bFirstColumnIsCheckbox)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "FillIndividualsList() : " + m_gedcom.m_alIndividualRecords.Count.ToString());

            m_bDisablePrunepanelCheckEvent = true;

            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            listView.Clear();

            listView.View = View.Details;
            int nameWidth = listView.Width - 70 - 70 - 20;
            if (bFirstColumnIsCheckbox)
            {
                listView.Columns.Add("Include", 30, HorizontalAlignment.Left);
                nameWidth -= 30;
            }
            listView.Columns.Add("Name", nameWidth, HorizontalAlignment.Left);
            listView.Columns.Add("Born", 70, HorizontalAlignment.Left);
            listView.Columns.Add("Died", 70, HorizontalAlignment.Left);
            listView.Columns.Add("Id", 60, HorizontalAlignment.Left);
            listView.Columns.Add("User ref", 78, HorizontalAlignment.Left);
            listView.Columns.Add("Pics", 48, HorizontalAlignment.Left);

            // Build an array first then blit the whole array to the list control. This is faster than adding each item to the list control individually.
            ListViewItem[] temporaryItemsList = new ListViewItem[m_gedcom.m_alIndividualRecords.Count];

            int nItem = 0;
            foreach (GEDmill.LLClasses.CIndividualRecord ir in m_gedcom.m_alIndividualRecords)
            {
                // Only allow fully unrestricted individuals.
                if (bExcludeRestricted && ir.Visibility() != LLClasses.CIndividualRecord.EVisibility.Visible) 
                {
                    continue;
                }

                CListableBool lbItem;
                if (bFirstColumnIsCheckbox)
                {
                    lbItem = new CListableBool(ir, true);
                }
                else
                {
                    string sSurname = "";
                    string sFirstName = "";
                    s_config.CapitaliseName(ir.Name, ref sFirstName, ref sSurname);
                    if (ir.NameSuffix != null && ir.NameSuffix != "")
                    {
                        sFirstName += ", " + ir.NameSuffix;
                    }
                    lbItem = new CListableBool(ir, sSurname, sFirstName, false);
                }

                SetIndividualSubItems(lbItem, ir, bFirstColumnIsCheckbox);

                // alSelectedIndividuals is a small array indicating xrefs of those individuals to mark selected. (not currently used, 3Jan08)
                if (alSelectedIndividuals != null && alSelectedIndividuals.Contains(ir.m_xref))
                {
                    lbItem.Selected = true;
                }

                lbItem.Checked = !(ir.Restricted);
                temporaryItemsList[nItem++] = lbItem;
            }

            listView.Items.AddRange(temporaryItemsList);
            listView.Sort();

            m_tabpagePruneRecordsIndis.Text = "Individuals (" + listView.Items.Count + ")";

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            m_bDisablePrunepanelCheckEvent = false;
        }

        // Attaches sub-items to a list item (before the list item is added to the list)
        private void SetIndividualSubItems(CListableBool lbItem, CIndividualRecord ir, bool bCheckBoxes)
        {
            // Save checkbox state because SubItems.Clear() clears item.Text and item.Checked as well, so replace old value after calling Clear().
            bool bWasChecked = lbItem.Checked; 
            string sItemText = lbItem.Text;
            lbItem.SubItems.Clear();
            lbItem.Text = sItemText;
            lbItem.Checked = bWasChecked;

            // If the list view has check boxes, the item is for the checkbox.
            // Otherwise the item is for the name, and so the sub items won't include the name.
            if (bCheckBoxes)
            {
                string sSurname = "";
                string sFirstName = "";
                s_config.CapitaliseName(ir.Name, ref sFirstName, ref sSurname);
                if (ir.NameSuffix != null && ir.NameSuffix != "")
                {
                    sFirstName += ", " + ir.NameSuffix;
                }
                lbItem.SubItems.Add(new CListableName(ir, sSurname, sFirstName));
            }

            CPGQualifiedDate birthDate = ir.BirthDate;
            if (birthDate != null)
            {
                lbItem.SubItems.Add(new CListableYear(birthDate.m_date));
            }
            else
            {
                lbItem.SubItems.Add(new CListableYear(null));
            }

            CPGQualifiedDate deathDate = ir.DeathDate;
            if (deathDate != null)
            {
                lbItem.SubItems.Add(new CListableYear(deathDate.m_date));
            }
            else
            {
                lbItem.SubItems.Add(new CListableYear(null));
            }

            lbItem.SubItems.Add(new CListableString(ir.m_xref));
            lbItem.SubItems.Add(new CListableString(ir.UserReferenceNumber(0)));

            int nVisiblePics = ir.CountVisibleMFRs();
            int nTotalPics = ir.CountAllMFRs();
            if (nVisiblePics != nTotalPics)
            {
                lbItem.SubItems.Add(new CListableNumber(nVisiblePics, String.Format("{0}/{1}", nVisiblePics, nTotalPics)));
            }
            else
            {
                lbItem.SubItems.Add(new CListableNumber(nTotalPics, String.Format("{0}", nTotalPics)));
            }
        }

        // Populates the list of source records for inclusion/exclusion in the website
        private void FillSourcesList(SortableListView listView, bool bFirstColumnIsCheckbox)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "FillSourcesList() : " + m_gedcom.m_alSourceRecords.Count.ToString());

            m_bDisablePrunepanelCheckEvent = true; // call to item.Checked below invokes event handler.

            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            listView.Clear();

            listView.View = View.Details;
            int nWidthTitle = listView.Width - 140 - 20;
            if (bFirstColumnIsCheckbox)
            {
                listView.Columns.Add("Include", 30, HorizontalAlignment.Left);
                nWidthTitle -= 30;
            }
            listView.Columns.Add("Title", nWidthTitle, HorizontalAlignment.Left);
            listView.Columns.Add("Repository", 100, HorizontalAlignment.Left);
            listView.Columns.Add("Citations", 60, HorizontalAlignment.Left);
            listView.Columns.Add("B", 30, HorizontalAlignment.Left);
            listView.Columns.Add("M", 30, HorizontalAlignment.Left);
            listView.Columns.Add("D", 30, HorizontalAlignment.Left);
            listView.Columns.Add("Id", 60, HorizontalAlignment.Left);
            listView.Columns.Add("Pics", 48, HorizontalAlignment.Left);

            ListViewItem[] temporaryItemsList = new ListViewItem[m_gedcom.m_alSourceRecords.Count];
            int nItem = 0;
            foreach (GEDmill.LLClasses.CSourceRecord sr in m_gedcom.m_alSourceRecords)
            {
                CListableBool item = new CListableBool(sr, bFirstColumnIsCheckbox);
                SetSourceSubItems(item, sr, bFirstColumnIsCheckbox);
                item.Checked = !(sr.Restricted);
                temporaryItemsList[nItem++] = item;
            }


            listView.Items.AddRange(temporaryItemsList);
            listView.Sort();

            m_tabpagePruneRecordsSources.Text = "Sources (" + listView.Items.Count + ")";

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            m_bDisablePrunepanelCheckEvent = false;
        }

        // Attaches sub-items to a list item (before the list item is added to the list)
        private void SetSourceSubItems(CListableBool lbItem, CSourceRecord sr, bool bFirstColumnIsCheckbox)
        {
            // Store checkbox value because SubItems.Clear() clears item.Text and item.Checked as well!
            bool bWasChecked = lbItem.Checked;
            lbItem.SubItems.Clear();
            lbItem.Checked = bWasChecked;

            if (bFirstColumnIsCheckbox)
            {
                // First nColumn (ie. item) is checkbox, so first sub-item is title.
                lbItem.SubItems.Add(new CListableSource(sr));
            }

            string sRepositories = "";
            foreach (CSourceRepositoryCitation src in sr.m_alSourceRepositoryCitations)
            {
                CRepositoryRecord rr = m_gedcom.GetRepositoryRecord(src.m_xrefRepo);
                if (rr != null)
                {
                    if (rr.m_sNameOfRepository != null && rr.m_sNameOfRepository != "")
                    {
                        if (sRepositories != "")
                        {
                            sRepositories += ", ";
                        }
                        sRepositories += rr.m_sNameOfRepository;
                    }
                }
            }
            lbItem.SubItems.Add(new ListViewItem.ListViewSubItem(lbItem, sRepositories));

            int nBirths = 0;
            int nMarriages = 0;
            int nDeaths = 0;
            int nIndis = 0;
            int nFamilies = 0;
            int nNotes = 0;
            int nOther = 0;
            int nCitations = 0;
            if (sr.m_alBackreferences.Count > 0)
            {
                foreach (CBackReference br in sr.m_alBackreferences)
                {
                    switch (br.m_sEventType)
                    {
                        case "BIRT":
                            nBirths++;
                            nCitations++;
                            break;
                        case "MARR":
                            nMarriages++;
                            nCitations++;
                            break;
                        case "DEAT":
                            nCitations++;
                            nDeaths++;
                            break;
                        default:
                            switch (br.m_ertRecordType)
                            {
                                case ERecordType.Individual:
                                    nCitations++;
                                    nIndis++;
                                    break;
                                case ERecordType.Family:
                                    // Strictly this should be plus 2 if husb & wife both known, otherwise 1 or 0.
                                    nCitations++; 
                                    nFamilies++;
                                    break;
                                case ERecordType.Note:
                                    nNotes++;
                                    break;
                                default:
                                    nOther++;
                                    break;
                            }
                            break;
                    }
                }
            }

            lbItem.SubItems.Add(new ListViewItem.ListViewSubItem(lbItem, nCitations.ToString()));
            lbItem.SubItems.Add(new ListViewItem.ListViewSubItem(lbItem, nBirths.ToString()));
            lbItem.SubItems.Add(new ListViewItem.ListViewSubItem(lbItem, nMarriages.ToString()));
            lbItem.SubItems.Add(new ListViewItem.ListViewSubItem(lbItem, nDeaths.ToString()));
            lbItem.SubItems.Add(new CListableString(sr.m_xref));

            int nVisiblePics = sr.CountVisibleMFRs();
            int nTotalPics = sr.CountAllMFRs();

            if (nVisiblePics != nTotalPics)
            {
                lbItem.SubItems.Add(new CListableNumber(nVisiblePics, String.Format("{0}/{1}", nVisiblePics, nTotalPics)));
            }
            else
            {
                lbItem.SubItems.Add(new CListableNumber(nTotalPics, String.Format("{0}", nTotalPics)));
            }
            lbItem.Checked = bWasChecked;

        }

        // Used to display the finished website. Uses whatever app the user has assigned to open HTML files.
        private void OpenURL(string sURL)
        {
            try
            {
                System.Diagnostics.Process.Start(sURL);
            }
            catch (Exception e2)
            {
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, String.Format("Caught exception while opening finished webpages : {0}", e2.ToString()));
            }
        }

        // Spawns the website creation thread, which calls CWebsite.Create to do the work.
        private bool CreateWebsite(string sOutputFolder, string sImageFolder)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Creating website");
            DialogResult dialogResult;

            // If user has specified a background image, check it exists
            if (s_config.m_sBackgroundImage != null && s_config.m_sBackgroundImage.Length != 0 && File.Exists(s_config.m_sBackgroundImage) == false)
            {
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Warning, "Can't find background image " + s_config.m_sBackgroundImage);

                dialogResult = MessageBocx.Show(m_mainForm, String.Format("The file {0} is missing. " +
                    "\r\nPages will be created without any background image.",
                    s_config.m_sBackgroundImage),
                    "Creating website",
                    MessageBoxButtons.OKCancel, MessageBoxIcon.Exclamation, false);
                if (dialogResult == DialogResult.Cancel)
                {
                    LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Message box cancelled (1)");
                    return false;
                }
            }

            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Starting progress window");


            ProgressWindow progressWindow = new ProgressWindow();
            progressWindow.Text = "Creating web pages";

            CWebsite website = new CWebsite(m_gedcom, progressWindow);

            ThreadStart threadStart = new ThreadStart(website.Create);
            Thread threadWorker = new Thread(threadStart);

            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Starting progress thread");

            dialogResult = DialogResult.Abort;
            try
            {
                threadWorker.Start();
                dialogResult = progressWindow.ShowDialog(this);
            }

            catch (CHTMLException e)
            {
                MessageBocx.Show(m_mainForm, String.Concat(e.Message), "Creating website",
                    MessageBoxButtons.OK, MessageBoxIcon.Exclamation, false);
            }
            finally
            {
                threadWorker.Join();
            }

            if (dialogResult == DialogResult.Abort)
            {
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Thread aborted");
                if (progressWindow.m_threaderror.m_sMessage == "")
                {
                    // Abort means there were file IO errors
                    MessageBocx.Show(m_mainForm, String.Format("A problem was encountered while creating the website files:\r\n\r\n{0}", LogFile.TheLogFile.ErrorReport()), MainForm.m_sSoftwareName,
                        MessageBoxButtons.OK, MessageBoxIcon.Exclamation, true);
                }
                else
                {
                    // Abort means there were file IO errors
                    MessageBocx.Show(m_mainForm, progressWindow.m_threaderror.m_sMessage, MainForm.m_sSoftwareName,
                        MessageBoxButtons.OK, MessageBoxIcon.Exclamation, true);
                }
            }

            if (dialogResult != DialogResult.OK)
            {
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Dialog not OK");
                return false;
            }

            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Website create done.");

            return true;
        }

        // Enable the current page of the wizard
        private void EnableCurrentPanel(bool bEnable)
        {
            if (m_bConfigPanelOn)
            {
                m_tabcontrolConfigPanel.Enabled = bEnable;
                // Disable all other panels
                bEnable = false; 
            }
            else
            {
                m_panelWelcome.Enabled = (m_nCurrentPanel == 1 && bEnable);
                m_panelChooseGedcom.Enabled = (m_nCurrentPanel == 2 && bEnable);
                m_panelPruneRecords.Enabled = (m_nCurrentPanel == 3 && bEnable);
                m_panelSelectKey.Enabled = (m_nCurrentPanel == 4 && bEnable);
                m_panelChooseOutput.Enabled = (m_nCurrentPanel == 5 && bEnable);
                m_panelAllDone.Enabled = (m_nCurrentPanel == 6 && bEnable);

                m_tabcontrolConfigPanel.Enabled = false;
            }

            m_picturebox.Enabled = bEnable;
        }

        // Displays useful information about a source record in a dialog box
        private void ShowSourceDetailsDialog(Form formParent, CListableBool lbItem, LLClasses.CSourceRecord sr, bool bCanEditPictures, bool bFirstColumnIsCheckbox)
        {
            CRecordDetailsForm detailsForm = new CRecordDetailsForm(formParent, sr, m_gedcom, bCanEditPictures, true);
            detailsForm.ShowDialog(this);
            if (lbItem != null && sr != null)
            {
                SetSourceSubItems(lbItem, sr, bFirstColumnIsCheckbox);
            }
            EnablePrunePanelButtons();
        }

        // Reports any exception thrown during the prune operation
        private void ReportPruneError(System.Exception e)
        {
            MessageBocx.Show(m_mainForm, String.Format("A problem was encountered while navigating the tree structure:\r\n\r\n{0}", LogFile.TheLogFile.ErrorReport()), MainForm.m_sSoftwareName,
                MessageBoxButtons.OK, MessageBoxIcon.Exclamation, true);

            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, String.Format("Caught navigation exception : {0}", e.ToString()));
        }

        // Helper function. Compares dates to see if we know this person is alive
        private static bool IsAlive(CPGDate dateBorn, CPGDate dateDied, CPGDate dateNow, CPGDate dateThen)
        {
            if (dateDied != null)
            {
                if (dateDied.CompareTo(dateNow) > 0)
                {
                    // Have to have death date after today
                    return true;
                }
            }
            else if (dateBorn != null && dateBorn.CompareTo(dateThen) > 0)
            {
                // Have to be born after dateThen
                return true;
            }
            return false;
        }

        // Displays the statistics of the prune operation
        private void ShowPruneResult(int nExcluded, int nIncluded, string sType)
        {
            string sMsg = "";
            if (nExcluded == 0 && nIncluded == 0)
            {
                sMsg = "No changes made.";
            }
            else
            {
                if (nIncluded != 0)
                {
                    sMsg = String.Format("{0} {1}{2} checked.", nIncluded, sType, nIncluded > 1 ? "s" : "");
                }
                if (nExcluded != 0)
                {
                    if (sMsg != "")
                    {
                        sMsg += "\r\n";
                    }
                    sMsg += String.Format("{0} {1}{2} unchecked.", nExcluded, sType, nExcluded > 1 ? "s" : "");
                }
            }

            MessageBocx.Show(this, sMsg, "Select Records", MessageBoxButtons.OK, MessageBoxIcon.Information, false);
        }

        // Displays the statistics of the remove pictures operation
        private void ShowHidePicsResult(int nHidden)
        {
            string sMsg = "";
            if (nHidden == 0)
            {
                sMsg = "No multimedia files hidden.";
            }
            else
            {
                if (nHidden != 0)
                {
                    sMsg = String.Format("{0} multimedia file{1} hidden.", nHidden, nHidden > 1 ? "s" : "");
                }

            }
            MessageBocx.Show(this, sMsg, "Hide Pictures", MessageBoxButtons.OK, MessageBoxIcon.Information, false);
        }

        // Initialises config panel controls
        private void LoadConfigPanelSettings()
        {
            m_textboxConfigFrontImageEdit.Text = s_config.m_sFrontPageImageFilename;
            m_textboxConfigFrontImageEdit.SelectionStart = m_textboxConfigFrontImageEdit.Text.Length;
            m_textboxConfigFrontImageEdit.SelectionLength = m_textboxConfigFrontImageEdit.Text.Length;
            m_textboxConfigBackImageEdit.Text = s_config.m_sBackgroundImage;
            m_textboxConfigBackImageEdit.SelectionStart = m_textboxConfigBackImageEdit.Text.Length;
            m_textboxConfigBackImageEdit.SelectionLength = m_textboxConfigBackImageEdit.Text.Length;
            m_textboxConfigIndiImageWidth.Text = s_config.m_uMaxImageWidth.ToString();
            m_textboxConfigIndiImageHeight.Text = s_config.m_uMaxImageHeight.ToString();
            m_textboxConfigSourceImageWidth.Text = s_config.m_uMaxSourceImageWidth.ToString();
            m_textboxConfigSourceImageHeight.Text = s_config.m_uMaxSourceImageHeight.ToString();
            m_textboxConfigThumbnailImageWidth.Text = s_config.m_uMaxThumbnailImageWidth.ToString();
            m_textboxConfigThumbnailImageHeight.Text = s_config.m_uMaxThumbnailImageHeight.ToString();
            m_comboboxConfigCharset.Items.Clear();
            m_comboboxConfigCharset.Items.AddRange(new object[] { "iso-8859-1", "UTF-8" });
            m_comboboxConfigCharset.SelectedIndex = (s_config.m_ecHtmlCharset == ECharset.UTF8 ? 1 : 0);
            m_comboboxConfigHtmlExtn.Items.Clear();
            m_comboboxConfigHtmlExtn.Items.AddRange(new object[] { ".htm", ".html" });
            m_comboboxConfigHtmlExtn.SelectedIndex = (s_config.m_sHtmlExtension == "html" ? 1 : 0);
            m_textboxConfigNoName.Text = s_config.m_sUnknownName;
            m_textboxConfigWithheldName.Text = s_config.m_sConcealedName;
            m_radiobuttonConfigWithheldNameLabel.Checked = !s_config.m_bUseWithheldNames;
            m_radiobuttonConfigWithheldNameName.Checked = s_config.m_bUseWithheldNames;
            m_checkboxConfigCapNames.Checked = (s_config.m_nNameCapitalisation == 1);
            m_checkboxConfigCapEvents.Checked = s_config.m_bCapitaliseEventDescriptions;
            m_checkboxConfigHideEmails.Checked = s_config.m_bObfuscateEmails;
            m_checkboxConfigOccupationHeadline.Checked = s_config.m_bOccupationHeadline;
            m_checkboxConfigAllowTrailingSpaces.Checked = s_config.m_bDataMayEndWithWhitespace;
            m_checkboxConfigAllowTrailingSpaces.Enabled = m_nCurrentPanel < 3; // Only enable this setting if we haven't loaded any GEDCOM yet.
            m_checkboxConfigShowWithheldRecords.Checked = s_config.m_bOnlyConceal;
            m_textboxConfigTabSpaces.Text = s_config.m_uTabSpaces.ToString();
            m_textboxConfigCommentary.Text = s_config.m_sCommentaryText;
            m_checkboxConfigCommentaryIsHtml.Checked = s_config.m_bCommentaryIsHtml;
            m_checkboxConfigStats.Checked = s_config.m_bFrontPageStats;
            m_checkboxConfigPreserveFrontPage.Checked = s_config.m_bPreserveFrontPage;
            m_checkboxConfigPreserveStylesheet.Checked = s_config.m_bPreserveStylesheet;
            m_checkboxConfigIncludeHelppage.Checked = s_config.m_bIncludeHelppage;
            m_checkboxConfigCdrom.Checked = s_config.m_bCreateCDROMFiles;
            m_checkboxConfigNonPictures.Checked = s_config.m_bAllowNonPictures;
            m_checkboxConfigIndiImages.Checked = s_config.m_bAllowMultipleImages;
            m_checkboxConfigTreeDiagrams.Checked = s_config.m_bShowMiniTrees;
            m_checkboxConfigTreeDiagramsFakeBg.Checked = s_config.m_bFakeMiniTreeTransparency;
            m_textboxConfigEmail.Text = s_config.m_sUserEmailAddress;
            m_textboxConfigIndexName.Text = s_config.m_sFrontPageFilename;
            m_labelConfigIndexNameExtn.Text = "." + s_config.m_sHtmlExtension;
            m_textboxConfigStylesheetName.Text = s_config.m_sStylesheetFilename;
            if (s_config.m_sMainWebsiteLink.Length == 0)
            {
                m_textboxConfigUserLink.Text = "http://";
            }
            else
            {
                m_textboxConfigUserLink.Text = s_config.m_sMainWebsiteLink;
            }
            m_comboboxConfigTreeDiagramsFormat.Items.Clear();
            m_comboboxConfigTreeDiagramsFormat.Items.AddRange(new object[] { "gif", "png" });
            m_comboboxConfigTreeDiagramsFormat.SelectedIndex = (s_config.m_sMiniTreeImageFormat == "png" ? 1 : 0);
            m_checkboxConfigMultiPageIndex.Checked = s_config.m_bMultiPageIndexes;
            m_checkboxConfigUserRefInIndex.Checked = s_config.m_bIncludeUserRefInIndex;
            m_textboxConfigMultiPageIndexNumber.Text = s_config.m_uIndividualsPerIndexPage.ToString();
            m_checkboxConfigKeepOriginals.Checked = s_config.m_bLinkOriginalPicture;
            m_checkboxConfigRenameOriginals.Checked = s_config.m_bRenameOriginalPicture;
            m_checkboxConfigW3C.Checked = s_config.m_bIncludeValiditySticker;
            m_checkboxConfigUserRecFilename.Checked = s_config.m_bUserRecFilename;
            m_textboxConfigCustomFooter.Text = s_config.m_sCustomFooter;
            m_checkboxConfigFooterIsHtml.Checked = s_config.m_bFooterIsHtml;
            m_checkboxConfigConserveTreeWidth.Checked = s_config.m_bConserveTreeWidth;
            m_checkboxConfigKeepSiblingOrder.Checked = s_config.m_bKeepSiblingOrder;
            m_checkboxConfigAllowMultimedia.Checked = s_config.m_bAllowMultimedia;

            m_colorConfigMiniTreeBranch = ImageClasses.CPaintbox.ConvertColour(s_config.m_sMiniTreeColourBranch);
            m_colorConfigMiniTreeIndiBorder = ImageClasses.CPaintbox.ConvertColour(s_config.m_sMiniTreeColourIndiBorder);
            m_colorConfigMiniTreeIndiBackground = ImageClasses.CPaintbox.ConvertColour(s_config.m_sMiniTreeColourIndiBackground);
            m_colorConfigMiniTreeIndiHighlight = ImageClasses.CPaintbox.ConvertColour(s_config.m_sMiniTreeColourIndiHighlight);
            m_colorConfigMiniTreeIndiBgConcealed = ImageClasses.CPaintbox.ConvertColour(s_config.m_sMiniTreeColourIndiBgConcealed);
            m_colorConfigMiniTreeIndiFgConcealed = ImageClasses.CPaintbox.ConvertColour(s_config.m_sMiniTreeColourIndiFgConcealed);
            m_colorConfigMiniTreeIndiShade = ImageClasses.CPaintbox.ConvertColour(s_config.m_sMiniTreeColourIndiShade);
            m_colorConfigMiniTreeIndiText = ImageClasses.CPaintbox.ConvertColour(s_config.m_sMiniTreeColourIndiText);
            m_colorConfigMiniTreeIndiLink = ImageClasses.CPaintbox.ConvertColour(s_config.m_sMiniTreeColourIndiLink);
            m_colorConfigMiniTreeBackground = ImageClasses.CPaintbox.ConvertColour(s_config.m_sMiniTreeColourBackground);

            m_checkboxConfigUseBom.Checked = s_config.m_bUseBom;
            m_checkboxConfigSupressBackreferences.Checked = !s_config.m_bSupressBackreferences;

            SetMiniTreeColourConfigButtons();

            EnableMultiPageIndexConfig();
            EnableMultimediaConfig();
            EnableWithheldConfig();
            EnableBOMCheckBox();
        }

        // Colours the buttons that set the mini tree colours according to the values they control
        private void SetMiniTreeColourConfigButtons()
        {
            m_buttonConfigMiniTreeColourBranch.BackColor = m_colorConfigMiniTreeIndiBackground;
            m_buttonConfigMiniTreeColourBranch.ForeColor = m_colorConfigMiniTreeBranch;
            m_buttonConfigMiniTreeColourIndiBorder.BackColor = m_colorConfigMiniTreeIndiBackground;
            m_buttonConfigMiniTreeColourIndiBorder.ForeColor = m_colorConfigMiniTreeIndiBorder;
            m_buttonConfigMiniTreeColourIndiBackground.BackColor = m_colorConfigMiniTreeIndiBackground;
            m_buttonConfigMiniTreeColourIndiBackground.ForeColor = m_colorConfigMiniTreeIndiLink;
            m_buttonConfigMiniTreeColourIndiHighlight.BackColor = m_colorConfigMiniTreeIndiHighlight;
            m_buttonConfigMiniTreeColourIndiHighlight.ForeColor = m_colorConfigMiniTreeIndiText;
            m_buttonConfigMiniTreeColourIndiBgConcealed.BackColor = m_colorConfigMiniTreeIndiBgConcealed;
            m_buttonConfigMiniTreeColourIndiBgConcealed.ForeColor = m_colorConfigMiniTreeIndiFgConcealed;
            m_buttonConfigMiniTreeColourIndiFgConcealed.BackColor = m_colorConfigMiniTreeIndiBgConcealed;
            m_buttonConfigMiniTreeColourIndiFgConcealed.ForeColor = m_colorConfigMiniTreeIndiFgConcealed;
            m_buttonConfigMiniTreeColourIndiShade.BackColor = m_colorConfigMiniTreeIndiShade;
            m_buttonConfigMiniTreeColourIndiShade.ForeColor = m_colorConfigMiniTreeIndiLink;
            m_buttonConfigMiniTreeColourIndiText.BackColor = m_colorConfigMiniTreeIndiHighlight;
            m_buttonConfigMiniTreeColourIndiText.ForeColor = m_colorConfigMiniTreeIndiText;
            m_buttonConfigMiniTreeColourIndiLink.BackColor = m_colorConfigMiniTreeIndiBackground;
            m_buttonConfigMiniTreeColourIndiLink.ForeColor = m_colorConfigMiniTreeIndiLink;
        }

        // Used to set all buttons grey when form is disabled
        private void ClearMiniTreeColourConfigButtons()
        {
            m_buttonConfigMiniTreeColourBranch.BackColor = Form.DefaultBackColor;
            m_buttonConfigMiniTreeColourBranch.ForeColor = Form.DefaultForeColor;
            m_buttonConfigMiniTreeColourIndiBorder.BackColor = Form.DefaultBackColor;
            m_buttonConfigMiniTreeColourIndiBorder.ForeColor = Form.DefaultForeColor;
            m_buttonConfigMiniTreeColourIndiBackground.BackColor = Form.DefaultBackColor;
            m_buttonConfigMiniTreeColourIndiBackground.ForeColor = Form.DefaultForeColor;
            m_buttonConfigMiniTreeColourIndiHighlight.BackColor = Form.DefaultBackColor;
            m_buttonConfigMiniTreeColourIndiHighlight.ForeColor = Form.DefaultForeColor;
            m_buttonConfigMiniTreeColourIndiBgConcealed.BackColor = Form.DefaultBackColor;
            m_buttonConfigMiniTreeColourIndiBgConcealed.ForeColor = Form.DefaultForeColor;
            m_buttonConfigMiniTreeColourIndiFgConcealed.BackColor = Form.DefaultBackColor;
            m_buttonConfigMiniTreeColourIndiFgConcealed.ForeColor = Form.DefaultForeColor;
            m_buttonConfigMiniTreeColourIndiShade.BackColor = Form.DefaultBackColor;
            m_buttonConfigMiniTreeColourIndiShade.ForeColor = Form.DefaultForeColor;
            m_buttonConfigMiniTreeColourIndiText.BackColor = Form.DefaultBackColor;
            m_buttonConfigMiniTreeColourIndiText.ForeColor = Form.DefaultForeColor;
            m_buttonConfigMiniTreeColourIndiLink.BackColor = Form.DefaultBackColor;
            m_buttonConfigMiniTreeColourIndiLink.ForeColor = Form.DefaultForeColor;
        }

        // Saves changes made in config panel back to main config.
        // Reads (and sanity checks) values from all controls in the config panel and puts them into the CConfig instance.
        private void SaveConfigPanelSettings()
        {
            s_config.m_sFrontPageImageFilename = m_textboxConfigFrontImageEdit.Text;
            s_config.m_sBackgroundImage = m_textboxConfigBackImageEdit.Text;

            try 
            {
                // Sanity check value
                uint uMaxImageWidth = System.UInt32.Parse(m_textboxConfigIndiImageWidth.Text);
                if (uMaxImageWidth > 0 && uMaxImageWidth <= 300)
                {
                    s_config.m_uMaxImageWidth = uMaxImageWidth;
                }
                else if (s_config.m_uMaxImageWidth != uMaxImageWidth && uMaxImageWidth > 300)
                {
                    DialogResult dialogResult = MessageBocx.Show(m_mainForm,
                        String.Format("Setting the image width to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", uMaxImageWidth),
                        "Change settings",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question, false);
                    if (dialogResult == DialogResult.Yes)
                    {
                        s_config.m_uMaxImageWidth = uMaxImageWidth;
                    }

                }
            }
            catch (System.Exception)
            {
                // Leave value unchanged
            };

            try 
            {
                // Sanity check value
                uint uMaxImageHeight = System.UInt32.Parse(m_textboxConfigIndiImageHeight.Text);
                if (uMaxImageHeight > 0 && uMaxImageHeight <= 800)
                {
                    s_config.m_uMaxImageHeight = uMaxImageHeight;
                }
                else if (s_config.m_uMaxImageHeight != uMaxImageHeight && uMaxImageHeight > 800)
                {
                    DialogResult dialogResult = MessageBocx.Show(m_mainForm,
                        String.Format("Setting the image height to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", uMaxImageHeight),
                        "Change settings",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question, false);
                    if (dialogResult == DialogResult.Yes)
                    {
                        s_config.m_uMaxImageHeight = uMaxImageHeight;
                    }

                }

            }
            catch (System.Exception)
            {
                // Leave value unchanged
            };

            try 
            {
                // Sanity check value
                uint uMaxSourceImageWidth = System.UInt32.Parse(m_textboxConfigSourceImageWidth.Text);
                if (uMaxSourceImageWidth > 0 && uMaxSourceImageWidth <= 800)
                {
                    s_config.m_uMaxSourceImageWidth = uMaxSourceImageWidth;
                }
                else if (s_config.m_uMaxSourceImageWidth != uMaxSourceImageWidth && uMaxSourceImageWidth > 800)
                {
                    DialogResult dialogResult = MessageBocx.Show(m_mainForm,
                        String.Format("Setting the source width to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", uMaxSourceImageWidth),
                        "Change settings",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question, false);
                    if (dialogResult == DialogResult.Yes)
                    {
                        s_config.m_uMaxSourceImageWidth = uMaxSourceImageWidth;
                    }


                }
            }
            catch (System.Exception)
            {
                // Leave value unchanged
            };

            try 
            {
                // Sanity check value
                uint uMaxSourceImageHeight = System.UInt32.Parse(m_textboxConfigSourceImageHeight.Text);
                if (uMaxSourceImageHeight > 0 && uMaxSourceImageHeight <= 800)
                {
                    s_config.m_uMaxSourceImageHeight = uMaxSourceImageHeight;
                }
                else if (s_config.m_uMaxSourceImageHeight != uMaxSourceImageHeight && uMaxSourceImageHeight > 800)
                {
                    DialogResult dialogResult = MessageBocx.Show(m_mainForm,
                        String.Format("Setting the source height to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", uMaxSourceImageHeight),
                        "Change settings",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question, false);
                    if (dialogResult == DialogResult.Yes)
                    {
                        s_config.m_uMaxSourceImageHeight = uMaxSourceImageHeight;
                    }
                }
            }
            catch (System.Exception)
            {
                // Leave value unchanged
            };

            try 
            {
                // Sanity check value
                uint uMaxThumbnailImageWidth = System.UInt32.Parse(m_textboxConfigThumbnailImageWidth.Text);
                if (uMaxThumbnailImageWidth > 0 && uMaxThumbnailImageWidth < 80)
                {
                    s_config.m_uMaxThumbnailImageWidth = uMaxThumbnailImageWidth;
                }
                else if (s_config.m_uMaxThumbnailImageWidth != uMaxThumbnailImageWidth && uMaxThumbnailImageWidth > 80)
                {
                    DialogResult dialogResult = MessageBocx.Show(m_mainForm,
                        String.Format("Setting the thumbnail width to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", uMaxThumbnailImageWidth),
                        "Change settings",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question, false);
                    if (dialogResult == DialogResult.Yes)
                    {
                        s_config.m_uMaxThumbnailImageWidth = uMaxThumbnailImageWidth;
                    }
                }
            }
            catch (System.Exception)
            {
                // Leave value unchanged
            };

            try 
            {
                // Sanity check value
                uint uMaxThumbnailImageHeight = System.UInt32.Parse(m_textboxConfigThumbnailImageHeight.Text);
                if (uMaxThumbnailImageHeight > 0 && uMaxThumbnailImageHeight < 80)
                {
                    s_config.m_uMaxThumbnailImageHeight = uMaxThumbnailImageHeight;
                }
                else if (s_config.m_uMaxThumbnailImageHeight != uMaxThumbnailImageHeight && uMaxThumbnailImageHeight > 80)
                {
                    DialogResult dialogResult = MessageBocx.Show(m_mainForm,
                        String.Format("Setting the thumbnail height to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", uMaxThumbnailImageHeight),
                        "Change settings",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question, false);
                    if (dialogResult == DialogResult.Yes)
                    {
                        s_config.m_uMaxThumbnailImageHeight = uMaxThumbnailImageHeight;
                    }
                }
            }
            catch (System.Exception)
            {
                // Leave value unchanged
            };

            s_config.m_ecHtmlCharset = (m_comboboxConfigCharset.SelectedIndex == 1 ? ECharset.UTF8 : ECharset.ISO8859_1);
            s_config.m_sHtmlExtension = (m_comboboxConfigHtmlExtn.SelectedIndex == 1 ? "html" : "htm");
            s_config.m_bIncludeValiditySticker = m_checkboxConfigW3C.Checked;
            s_config.m_bUserRecFilename = m_checkboxConfigUserRecFilename.Checked;
            if (m_textboxConfigNoName.Text.Length > 0)
            {
                s_config.m_sUnknownName = m_textboxConfigNoName.Text;
                CListableName.s_sUnknownName = s_config.m_sUnknownName;
                CListableBool.s_sUnknownName = s_config.m_sUnknownName;
            }
            if (m_textboxConfigWithheldName.Text.Length > 0)
            {
                s_config.m_sConcealedName = m_textboxConfigWithheldName.Text;
            }
            s_config.m_bUseWithheldNames = m_radiobuttonConfigWithheldNameName.Checked;
            s_config.m_nNameCapitalisation = (m_checkboxConfigCapNames.Checked ? 1 : 0);
            s_config.m_bCapitaliseEventDescriptions = m_checkboxConfigCapEvents.Checked;
            s_config.m_bObfuscateEmails = m_checkboxConfigHideEmails.Checked;
            s_config.m_bOccupationHeadline = m_checkboxConfigOccupationHeadline.Checked;
            s_config.m_bDataMayEndWithWhitespace = m_checkboxConfigAllowTrailingSpaces.Checked;
            s_config.m_bOnlyConceal = m_checkboxConfigShowWithheldRecords.Checked;

            try 
            {
                // Sanity check value
                uint uTabSpaces = System.UInt32.Parse(m_textboxConfigTabSpaces.Text);
                if (uTabSpaces > 0 && uTabSpaces < 64)
                {
                    s_config.m_uTabSpaces = uTabSpaces;
                }
            }
            catch (System.Exception)
            {
                // Leave value unchanged
            };

            s_config.m_sCommentaryText = m_textboxConfigCommentary.Text;
            s_config.m_bCommentaryIsHtml = m_checkboxConfigCommentaryIsHtml.Checked;
            s_config.m_bFrontPageStats = m_checkboxConfigStats.Checked;
            s_config.m_bShowMiniTrees = m_checkboxConfigTreeDiagrams.Checked;
            s_config.m_bFakeMiniTreeTransparency = m_checkboxConfigTreeDiagramsFakeBg.Checked;
            s_config.m_sUserEmailAddress = m_textboxConfigEmail.Text;
            s_config.m_bPreserveFrontPage = m_checkboxConfigPreserveFrontPage.Checked;
            s_config.m_bPreserveStylesheet = m_checkboxConfigPreserveStylesheet.Checked;
            s_config.m_bIncludeHelppage = m_checkboxConfigIncludeHelppage.Checked;
            s_config.m_bCreateCDROMFiles = m_checkboxConfigCdrom.Checked;
            s_config.m_bAllowMultipleImages = m_checkboxConfigIndiImages.Checked;
            s_config.m_bAllowNonPictures = m_checkboxConfigNonPictures.Checked;
            s_config.m_bLinkOriginalPicture = m_checkboxConfigKeepOriginals.Checked;
            s_config.m_bRenameOriginalPicture = m_checkboxConfigRenameOriginals.Checked;

            // Validate and strip trailing .html or .htm in case user has put them on
            string sFrontPageFilename = m_textboxConfigIndexName.Text;
            string sFrontPageFilenameUpper = sFrontPageFilename.ToUpper();
            if (sFrontPageFilenameUpper.LastIndexOf(".HTML") >= 0)
            {
                sFrontPageFilename = sFrontPageFilename.Substring(0, sFrontPageFilename.Length - 5);
            }
            else if (sFrontPageFilenameUpper.LastIndexOf(".HTM") >= 0)
            {
                sFrontPageFilename = sFrontPageFilename.Substring(0, sFrontPageFilename.Length - 4);
            }
            s_config.m_sFrontPageFilename = sFrontPageFilename;

            // Validate and strip trailing .css in case user has put them on
            string sStylesheetFilename = m_textboxConfigStylesheetName.Text;
            if (sStylesheetFilename.Length > 0)
            {
                string sStylesheetFilenameUpper = sStylesheetFilename.ToUpper();
                if (sStylesheetFilename.LastIndexOf(".CSS") >= 0)
                {
                    sStylesheetFilename = sStylesheetFilename.Substring(0, sStylesheetFilename.Length - 4);
                }
                s_config.m_sStylesheetFilename = sStylesheetFilename;
            }

            // Validate and strip leading http:// in case user has it them on
            string sMainWebsiteLink = m_textboxConfigUserLink.Text;
            string sMainWebsiteLinkUpper = sMainWebsiteLink.ToUpper();

            if (sMainWebsiteLink.ToLower() == "http://")
            {
                // User hasn't altered default value
                sMainWebsiteLink = "";
            }

            s_config.m_sMainWebsiteLink = sMainWebsiteLink;
            s_config.m_sMiniTreeImageFormat = (m_comboboxConfigTreeDiagramsFormat.SelectedIndex == 1 ? "png" : "gif");
            s_config.m_bMultiPageIndexes = m_checkboxConfigMultiPageIndex.Checked;
            s_config.m_bIncludeUserRefInIndex = m_checkboxConfigUserRefInIndex.Checked;

            try 
            {
                // Sanity check value
                uint uIndex = System.UInt32.Parse(m_textboxConfigMultiPageIndexNumber.Text);
                if (uIndex > 0)
                {
                    s_config.m_uIndividualsPerIndexPage = uIndex;
                }
            }
            catch (System.Exception)
            {
                // Leave value unchanged
            };

            string sCustomFooter = m_textboxConfigCustomFooter.Text;
            s_config.m_sCustomFooter = sCustomFooter;

            s_config.m_bFooterIsHtml = m_checkboxConfigFooterIsHtml.Checked;
            s_config.m_bConserveTreeWidth = m_checkboxConfigConserveTreeWidth.Checked;
            s_config.m_bKeepSiblingOrder = m_checkboxConfigKeepSiblingOrder.Checked;
            s_config.m_bAllowMultimedia = m_checkboxConfigAllowMultimedia.Checked;


            s_config.m_sMiniTreeColourBranch = ImageClasses.CPaintbox.ConvertColour(m_colorConfigMiniTreeBranch);
            s_config.m_sMiniTreeColourIndiBorder = ImageClasses.CPaintbox.ConvertColour(m_colorConfigMiniTreeIndiBorder);
            s_config.m_sMiniTreeColourIndiBackground = ImageClasses.CPaintbox.ConvertColour(m_colorConfigMiniTreeIndiBackground);
            s_config.m_sMiniTreeColourIndiHighlight = ImageClasses.CPaintbox.ConvertColour(m_colorConfigMiniTreeIndiHighlight);
            s_config.m_sMiniTreeColourIndiBgConcealed = ImageClasses.CPaintbox.ConvertColour(m_colorConfigMiniTreeIndiBgConcealed);
            s_config.m_sMiniTreeColourIndiFgConcealed = ImageClasses.CPaintbox.ConvertColour(m_colorConfigMiniTreeIndiFgConcealed);
            s_config.m_sMiniTreeColourIndiShade = ImageClasses.CPaintbox.ConvertColour(m_colorConfigMiniTreeIndiShade);
            s_config.m_sMiniTreeColourIndiText = ImageClasses.CPaintbox.ConvertColour(m_colorConfigMiniTreeIndiText);
            s_config.m_sMiniTreeColourIndiLink = ImageClasses.CPaintbox.ConvertColour(m_colorConfigMiniTreeIndiLink);
            s_config.m_sMiniTreeColourBackground = ImageClasses.CPaintbox.ConvertColour(m_colorConfigMiniTreeBackground);

            s_config.m_bUseBom = m_checkboxConfigUseBom.Checked;
            s_config.m_bSupressBackreferences = !m_checkboxConfigSupressBackreferences.Checked;

        }

        // Populates the list box of individuals to link from the front page
        private void FillKeyIndividualsList()
        {
            if (s_config.m_alKeyIndividuals == null)
            {
                return;
            }

            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "FillKeyIndividualsList() : " + s_config.m_alKeyIndividuals.Count.ToString());

            string sSurname;
            string sFirstName;
            string sFullName;

            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            m_listboxSelectKey.Items.Clear();

            if (s_config.m_alKeyIndividuals != null)
            {
                foreach (string xref in s_config.m_alKeyIndividuals)
                {
                    LLClasses.CIndividualRecord irKey = m_gedcom.GetIndividualRecord(xref);
                    if (irKey != null && !irKey.Restricted)
                    {
                        sFirstName = "";
                        sSurname = "";
                        sFullName = s_config.CapitaliseName(irKey.Name, ref sFirstName, ref sSurname);
                        if (sFullName == "")
                        {
                            sFullName = s_config.m_sUnknownName;
                        }
                        m_listboxSelectKey.Items.Add(new CNameXrefPair(sFullName, xref));
                    }
                }
            }

            EnableKeyIndividualsDeleteButton();

            Cursor.Current = Cursors.Default;
            Cursor.Hide();
        }

        // Creates output directory, deleting old ones if required, leaving them place if required.
        // Returns OK if website creation can proceed, Cancel if user should be returned to main form, Retry to retry the process without returning to main form.
        // front_page_filename is the name of a file to survive throughout this process (see also s_config.m_preserveFrontPage)
        private DialogResult PrepareOutputDirectory(string sOutputFolder, bool bPreserveFiles)
        {
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, String.Format("PrepareOutputDirectory({0})", sOutputFolder));

            string sMessage = "Could not access or create folder.";
            MessageBoxButtons messageBoxButtons = MessageBoxButtons.RetryCancel;
            DialogResult dialogResult = DialogResult.OK;
            bool bFailed = false;
            string sExceptionMessage = "";

            // First see if folder clashes with a file
            if (File.Exists(sOutputFolder))
            {
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Folder clashes with file : " + sOutputFolder);

                // Earn user that file is being deleted
                dialogResult = MessageBocx.Show(m_mainForm, String.Format("The folder {0} needs to be created. " +
                    "\r\nThis will destroy an existing file with that name.", sOutputFolder),
                    "Creating website",
                    MessageBoxButtons.OKCancel, MessageBoxIcon.Exclamation, false);
                if (dialogResult == DialogResult.Cancel)
                {
                    LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Message box cancelled (2)");
                    return DialogResult.Cancel;
                }

                // Now delete output folder (which is currently a file)
                do
                {
                    bFailed = false;
                    LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Deleting output folder as file : " + sOutputFolder);
                    try
                    {
                        Cursor.Current = Cursors.WaitCursor;
                        Cursor.Show();
                        File.Delete(sOutputFolder);
                    }
                    catch (IOException e)
                    {
                        LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught IO exception : " + e.ToString());
                        sExceptionMessage = e.Message;
                        bFailed = true;
                    }
                    catch (System.UnauthorizedAccessException e)
                    {
                        LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught UnauthorizedAccessException(3) : " + e.ToString());
                        sExceptionMessage = e.Message;
                        bFailed = true;
                    }
                    catch (System.Exception e)
                    {
                        LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught generic exception(3) : " + e.ToString());
                        sExceptionMessage = e.Message;
                        bFailed = true;
                    }

                    if (bFailed)
                    {
                        // Catch failure, e.g. if user has dir/file open elsewhere
                        sMessage = String.Format("The file {0} could not be deleted.\r\n{1}", sOutputFolder, sExceptionMessage);
                        messageBoxButtons = MessageBoxButtons.RetryCancel;
                        dialogResult = MessageBocx.Show(m_mainForm, sMessage, "Creating website", messageBoxButtons, MessageBoxIcon.Exclamation, false);
                    }
                }
                while (bFailed && dialogResult == DialogResult.Retry);
                if (bFailed && dialogResult == DialogResult.Cancel)
                {
                    LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Message box cancelled (6)");
                    return DialogResult.Cancel;
                }
            } // End if output folder exists

            // Next see if folder already exists
            // If output folder exists, offer to delete it
            if (Directory.Exists(sOutputFolder))
            {
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Folder exists(11) : " + sOutputFolder);

                if (CHTMLFile.IsDesktop(sOutputFolder))
                {
                    dialogResult = MessageBocx.Show(m_mainForm, "GEDmill will not allow you to create files directly on the Desktop", "Creating website", MessageBoxButtons.OK, MessageBoxIcon.Stop, false);
                    LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Desktop detected as output folder.");
                    return DialogResult.Cancel;
                }

                // Warn user that file is being deleted
                dialogResult = MessageBocx.Show(m_mainForm, String.Format("The folder {0} already exists.\r\nWould you like to delete any files it contains before creating the website files?", sOutputFolder),
                    "Creating website",
                    MessageBoxButtons.YesNoCancel, MessageBoxIcon.Question, false);
                if (dialogResult == DialogResult.Cancel)
                {
                    LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Message box cancelled (3a)");
                    return DialogResult.Cancel;
                }
                if (dialogResult == DialogResult.Yes)
                {
                    if (bPreserveFiles)
                    {
                        dialogResult = MessageBocx.Show(m_mainForm, String.Format("WARNING: Deleting the folder {0} will not preserve any existing front page and stylesheet files.\r\nDelete folder anyway?", sOutputFolder),
                            "Creating website",
                            MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation, false);
                        if (dialogResult == DialogResult.Cancel)
                        {
                            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Message box cancelled (3b)");
                            return DialogResult.Cancel;
                        }
                    }
                    else
                    {
                        dialogResult = MessageBocx.Show(m_mainForm, String.Format("WARNING: If the folder contains non-GEDmill files they will be deleted also.\r\nDelete folder anyway?", sOutputFolder),
                            "Creating website",
                            MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation, false);
                        if (dialogResult == DialogResult.Cancel)
                        {
                            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Message box cancelled (3c)");
                            return DialogResult.Cancel;
                        }
                    }
                    if (dialogResult == DialogResult.Yes)
                    {
                        LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Message box yes. Deleting output folder.");
                        do
                        {
                            bFailed = false;
                            try
                            {
                                Cursor.Current = Cursors.WaitCursor;
                                Cursor.Show();
                                if (Directory.Exists(sOutputFolder))
                                {
                                    Directory.Delete(sOutputFolder, true);
                                }
                            }
                            catch (IOException e)
                            {
                                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught IOException(2) : " + e.ToString());
                                sExceptionMessage = e.Message;
                                bFailed = true;
                            }
                            catch (System.UnauthorizedAccessException e)
                            {
                                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught UnauthorizedAccessException(2) : " + e.ToString());
                                sExceptionMessage = e.Message;
                                bFailed = true;
                            }
                            catch (System.Exception e)
                            {
                                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught generic exception(2) : " + e.ToString());
                                sExceptionMessage = e.Message;
                                bFailed = true;
                            }
                            if (bFailed)
                            {
                                // Catch failure, e.g. if user has dir/file open elsewhere
                                sMessage = String.Format("The folder {0} could not be deleted.\r\n{1}", sOutputFolder, sExceptionMessage);
                                messageBoxButtons = MessageBoxButtons.RetryCancel;
                                dialogResult = MessageBocx.Show(m_mainForm, sMessage, "Creating website",
                                    messageBoxButtons, MessageBoxIcon.Exclamation, false);
                            }
                        }
                        while (bFailed && dialogResult == DialogResult.Retry);
                        if (bFailed && dialogResult == DialogResult.Cancel)
                        {
                            return DialogResult.Cancel;
                        }
                    } // end "Yes" to not preserve files
                } // end "Yes to delete folder"
            } // end if output folder exists

            // At last, try to create the folder
            bFailed = false;
            DirectoryInfo directoryInfo = null;
            LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Creating output folder.");
            try
            {
                directoryInfo = Directory.CreateDirectory(sOutputFolder);
            }
            // Order of catches is important here, due to hierarchy of exception classes.
            catch (DirectoryNotFoundException e)
            {
                sMessage = "The folder you have selected could not be found.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught DirectoryNotFoundException(5) : " + e.ToString());
                bFailed = true;
            }
            catch (ArgumentNullException e)
            {
                sMessage = "The folder name is missing or illegal.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught ArgumentNullException(5) : " + e.ToString());
                bFailed = true;
            }
            catch (PathTooLongException e)
            {
                sMessage = "The folder name you have selected is too long.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught PathTooLongException(5) : " + e.ToString());
                bFailed = true;
            }
            catch (IOException e)
            {
                sMessage = "The path you have selected is read-only, or the folder is not empty.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.RetryCancel; // Let them correct this outside of app
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught IOException(5) : " + e.ToString());
                bFailed = true;
            }
            catch (UnauthorizedAccessException e)
            {
                sMessage = "You do not have the correct permissions to access the folder.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.RetryCancel; // Let them correct this outside of app
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught UnauthorizedAccessException(5) : " + e.ToString());
                bFailed = true;
            }
            catch (ArgumentException e)
            {
                sMessage = "The folder name you have selected is of an illegal format.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught ArgumentException(5) : " + e.ToString());
                bFailed = true;
            }
            catch (NotSupportedException e)
            {
                sMessage = "The folder name you have selected is of an unsupported format.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught NotSupportedException(5) : " + e.ToString());
                bFailed = true;
            }

            // Handle any failure with a sMessage box
            if (bFailed)
            {
                dialogResult = MessageBocx.Show(m_mainForm, String.Concat(sMessage, "\r\n", sExceptionMessage), "Creating website", messageBoxButtons, MessageBoxIcon.Exclamation, false);

                if (dialogResult == DialogResult.Retry)
                {
                    return DialogResult.Retry;
                }
                // Return cancel even if they clicked OK. This is because the sMessage might be "Catastrophic failure nothing you can do about it." 
                return DialogResult.Cancel;  
            }

            return DialogResult.OK;
        }

        // Logic to enable controls related to the multi-page index option
        private void EnableMultiPageIndexConfig()
        {
            if (m_checkboxConfigMultiPageIndex.Checked)
            {
                m_labelConfigMultiPageIndexNumber.Enabled = true;
                m_textboxConfigMultiPageIndexNumber.Enabled = true;
            }
            else
            {
                m_labelConfigMultiPageIndexNumber.Enabled = false;
                m_textboxConfigMultiPageIndexNumber.Enabled = false;
            }

        }

        // Logic to enable the BOM checkbox only if UTF8 is selected
        private void EnableBOMCheckBox()
        {
            bool bUTF8 = (m_comboboxConfigCharset.SelectedIndex == 1);
            m_checkboxConfigUseBom.Enabled = bUTF8;
        }

        // Logic to enable the controls related to multimedia content
        private void EnableMultimediaConfig()
        {
            if (m_checkboxConfigAllowMultimedia.Checked)
            {
                m_labelConfigIndiImageSize.Enabled = true;
                m_labelConfigIndiImageWidth.Enabled = true;
                m_textboxConfigIndiImageWidth.Enabled = true;
                m_labelConfigIndiImageHeight.Enabled = true;
                m_textboxConfigIndiImageHeight.Enabled = true;
                m_labelConfigSourceImageSize.Enabled = true;
                m_labelConfigSourceImageWidth.Enabled = true;
                m_textboxConfigSourceImageWidth.Enabled = true;
                m_labelConfigSourceImageHeight.Enabled = true;
                m_textboxConfigSourceImageHeight.Enabled = true;
                m_checkboxConfigRenameOriginals.Enabled = true;
                m_checkboxConfigKeepOriginals.Enabled = true;
                m_checkboxConfigNonPictures.Enabled = true;
                m_checkboxConfigIndiImages.Enabled = true;

                EnableThumbnailsConfig();
            }
            else
            {
                m_labelConfigIndiImageSize.Enabled = false;
                m_labelConfigIndiImageWidth.Enabled = false;
                m_textboxConfigIndiImageWidth.Enabled = false;
                m_labelConfigIndiImageHeight.Enabled = false;
                m_textboxConfigIndiImageHeight.Enabled = false;
                m_labelConfigSourceImageSize.Enabled = false;
                m_labelConfigSourceImageWidth.Enabled = false;
                m_textboxConfigSourceImageWidth.Enabled = false;
                m_labelConfigSourceImageHeight.Enabled = false;
                m_textboxConfigSourceImageHeight.Enabled = false;
                m_checkboxConfigRenameOriginals.Enabled = false;
                m_checkboxConfigKeepOriginals.Enabled = false;
                m_checkboxConfigNonPictures.Enabled = false;
                m_checkboxConfigIndiImages.Enabled = false;
                m_labelConfigThumbnailImageSize.Enabled = false;
                m_labelConfigThumbnailImageWidth.Enabled = false;
                m_textboxConfigThumbnailImageWidth.Enabled = false;
                m_labelConfigThumbnailImageHeight.Enabled = false;
                m_textboxConfigThumbnailImageHeight.Enabled = false;
            }
        }

        // Logic to enable the controls related to thumbnails
        private void EnableThumbnailsConfig()
        {
            if (m_checkboxConfigIndiImages.Checked)
            {
                m_labelConfigThumbnailImageSize.Enabled = true;
                m_labelConfigThumbnailImageWidth.Enabled = true;
                m_textboxConfigThumbnailImageWidth.Enabled = true;
                m_labelConfigThumbnailImageHeight.Enabled = true;
                m_textboxConfigThumbnailImageHeight.Enabled = true;
            }
            else
            {
                m_labelConfigThumbnailImageSize.Enabled = false;
                m_labelConfigThumbnailImageWidth.Enabled = false;
                m_textboxConfigThumbnailImageWidth.Enabled = false;
                m_labelConfigThumbnailImageHeight.Enabled = false;
                m_textboxConfigThumbnailImageHeight.Enabled = false;
            }

        }

        // Logic to enable the controls related to withheld records
        private void EnableWithheldConfig()
        {
            if (m_checkboxConfigShowWithheldRecords.Checked)
            {
                m_groupboxConfigWithheldName.Enabled = true;
                m_radiobuttonConfigWithheldNameLabel.Enabled = true;
                m_textboxConfigWithheldName.Enabled = m_radiobuttonConfigWithheldNameLabel.Checked;
                m_radiobuttonConfigWithheldNameName.Enabled = true;
            }
            else
            {
                m_groupboxConfigWithheldName.Enabled = false;
                m_radiobuttonConfigWithheldNameLabel.Enabled = false;
                m_textboxConfigWithheldName.Enabled = false;
                m_radiobuttonConfigWithheldNameName.Enabled = false;
            }

        }

        // Logic to enable the save changes button
        private void EnablePrunePanelButtons()
        {
            m_buttonPruneRecordsSave.Enabled = m_bPrunepanelDataChanged;
        }
    } // End of class MainForm


}
