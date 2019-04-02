/* MainForm.cs
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
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Threading;
using System.Windows.Forms;
using GEDmill.Exceptions;
using GEDmill.HTML;
using GEDmill.ListView;
using GEDmill.MiniTree;
using GEDmill.Model;
using GKCommon.GEDCOM;
using GKCore.Logging;
using GKUI.Components;

namespace GEDmill
{
    /// <summary>
    /// The main from from which the application is operated. Contains the GUI controls and the control handlers.
    /// </summary>
    public partial class MainForm : Form
    {
        private static readonly ILogger fLogger = LogManager.GetLogger(CConfig.LOG_FILE, CConfig.LOG_LEVEL, typeof(MainForm).Name);


        // Stores and parses data from GEDCOM files
        private GEDCOMTree fTree;

        // Specifies which panel of the wizard the user is viewing (i.e. which stage in the app they are at)
        private int fCurrentPanel;

        // Application has an important state whereby it displays the settings panes. 
        // The main GUI navigation buttons behave differently in this mode.
        private bool fConfigPanelOn;

        // The Settings button changes its label when the settings panes are displayed
        // These strings define the labels in each state
        private string m_sConfigButtonTextOn = "&Settings...";
        private string m_sConfigButtonTextOff = "&OK";

        // Scales the size of the main GUI
        private System.Drawing.Point fDefaultButtonSize;

        // Scales the size of the config panels GUI
        private System.Drawing.Point fConfigButtonSize;

        // Check event gets called when program builds the list. Don't want to enable buttons in that case.
        private bool fDisablePrunepanelCheckEvent;

        private ColorDialog fColorDialogConfigMiniTree;

        // When user redefines the mini tree colours, these hold the new colours until they click OK.
        private Color m_colorConfigMiniTreeBranch;
        private Color m_colorConfigMiniTreeIndiBorder;
        private Color m_colorConfigMiniTreeIndiBackground;
        private Color m_colorConfigMiniTreeIndiHighlight;
        private Color m_colorConfigMiniTreeIndiBgConcealed;
        private Color m_colorConfigMiniTreeIndiFgConcealed;
        private Color m_colorConfigMiniTreeIndiShade;
        private Color m_colorConfigMiniTreeIndiText;
        private Color m_colorConfigMiniTreeIndiLink;
        private Color m_colorConfigMiniTreeBackground;

        // Public so GEDCOMTree can change it. Should really refactor so that it's a member of GEDCOMTree.
        public int PruneExcluded;

        // Public so GEDCOMTree can change it. Should really refactor so that it's a member of GEDCOMTree.
        public int PruneIncluded;

        // Indicates user has made changes to data from GEDCOM file
        public bool PrunepanelDataChanged;


        public string InputFile
        {
            get { return m_textboxChooseGedcom.Text; }
            set { m_textboxChooseGedcom.Text = value; }
        }


        // Constructor. Initialise and create GUI.
        public MainForm()
        {
            fLogger.WriteInfo(CConfig.SoftwareName + " started at " + DateTime.Now.ToString());

            // Set some values that scale the size of the GUI
            fDefaultButtonSize = new Point(75, 23);
            fConfigButtonSize = new Point(92, 23);

            // Read back any previously stored settings.
            CConfig.Instance.RecoverSettings();

            // Creates the entire GUI
            InitializeComponent();

            fColorDialogConfigMiniTree = new ColorDialog();
            fColorDialogConfigMiniTree.FullOpen = true;
            fColorDialogConfigMiniTree.SolidColorOnly = true;

            m_labelWelcomeVersion.Text = "version " + CConfig.SoftwareVersion;
            m_helpProvider.HelpNamespace = CConfig.Instance.ApplicationPath + "\\" + CConfig.HelpFilename;

            fTree = new GEDCOMTree();
            fCurrentPanel = 1;
            fConfigPanelOn = false;
            PruneExcluded = 0;
            PruneExcluded = 0;

            PrunepanelDataChanged = false;
            fDisablePrunepanelCheckEvent = false;

            ShowCurrentPanel();
        }

        // Clean up any resources being used.
        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fTree != null) {
                    fTree.Dispose();
                }
                if (components != null) {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        #region Event handlers

        private void backButton_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("Back button");

            // Mustn't affect configPanel 
            if (fConfigPanelOn) {
                return;
            }

            if (fCurrentPanel > 1) {
                --fCurrentPanel;
            }

            EnableCurrentPanel(true);
            ShowCurrentPanel();
        }

        private void nextButton_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("Next button clicked. current panel = " + fCurrentPanel.ToString());

            // Mustn't affect configPanel
            if (fConfigPanelOn) {
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

            if (ValidateCurrentPanel()) {
                if (fCurrentPanel < 9) // Allow for extra ftp panels
                    ++fCurrentPanel;
                else {
                    CConfig.Instance.StoreSettings();

                    if (CConfig.Instance.OpenWebsiteOnExit) {
                        GMHelper.OpenURL(CConfig.Instance.FrontPageURL);
                    }
                    Close();
                    return;
                }

                InitialiseCurrentPanel();
                ShowCurrentPanel();
            } else {
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

        private void cancelButton_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("Quit button clicked.");

            DialogResult dialogResult = DialogResult.Yes;

            if (fCurrentPanel != 6) {
                // Cancel button is "Finish" in panel 6
                dialogResult = MessageBox.Show(this, "Are you sure you wish to exit GEDmill?", "Quit GEDmill",
                    MessageBoxButtons.YesNo, MessageBoxIcon.Question);
            }

            if (dialogResult == DialogResult.Yes) {
                if (fCurrentPanel >= 6) {
                    // HTML generated, so save settings for that at least
                    // Store checkbox state in config.(Can't do in ValidateCurrentPanel because Next never gets clicked)
                    CConfig.Instance.OpenWebsiteOnExit = m_checkboxAllDoneShowSite.Checked;
                }

                if (fCurrentPanel == 6) {
                    // Finish button is the only time we want to launch webpages
                    if (CConfig.Instance.OpenWebsiteOnExit && CConfig.Instance.FrontPageFilename.Length > 0) {
                        GMHelper.OpenURL(CConfig.Instance.FrontPageURL);
                    }
                }

                CConfig.Instance.StoreSettings();
                Application.Exit();
            }
        }

        private void helpButton_click(object sender, System.EventArgs e)
        {
            string sHelpFile = CConfig.Instance.ApplicationPath + "\\" + CConfig.HelpFilename;

            if (fConfigPanelOn) {
                switch (m_tabcontrolConfigPanel.SelectedIndex) {
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
            } else {
                switch (fCurrentPanel) {
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

        private void configButton_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("Config button clicked. current panel = " + fCurrentPanel.ToString());

            if (!fConfigPanelOn) {
                // Switch config panel on
                // Initialise config panel settings
                LoadConfigPanelSettings();
                SwitchConfigPanelOn();
            } else {
                // Switch config panel off
                // Save config panel settings
                SaveConfigPanelSettings();
                SwitchConfigPanelOff();
            }
        }

        private void configCancelButton_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("Config reset button clicked. current panel = " + fCurrentPanel.ToString());

            // Ensure config panel on
            if (!fConfigPanelOn) {
                return;
            }

            // Remove panel without saving changes
            SwitchConfigPanelOff();
        }

        private void buttonPruneRecordsSave_click(object sender, System.EventArgs e)
        {
        }

        private void buttonPruneRecordsLoad_click(object sender, System.EventArgs e)
        {
        }

        private void buttonChooseGedcomBrowse_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("Panel 2 browse button clicked.");

            OpenFileDialog openFileDialog = new OpenFileDialog();

            if (Directory.Exists(InputFile)) {
                openFileDialog.InitialDirectory = InputFile;
            } else {
                string sPath = InputFile;
                int nLastFolder = sPath.LastIndexOf('\\'); // Try parent folder
                if (nLastFolder >= 0) {
                    sPath = sPath.Substring(0, nLastFolder);
                }
                if (Directory.Exists(sPath)) {
                    openFileDialog.InitialDirectory = sPath;
                } else {
                    openFileDialog.InitialDirectory = Environment.GetFolderPath(System.Environment.SpecialFolder.Personal);
                }
            }

            openFileDialog.FileName = InputFile;
            openFileDialog.Filter = "GEDCOM files (*.ged)|*.ged|All files (*.*)|*.*";
            openFileDialog.FilterIndex = 1;
            openFileDialog.RestoreDirectory = true;

            if (openFileDialog.ShowDialog() == DialogResult.OK) {
                InputFile = openFileDialog.FileName;
                m_textboxChooseGedcom.SelectionStart = InputFile.Length;
                m_textboxChooseGedcom.SelectionLength = InputFile.Length;
            }
            fLogger.WriteInfo("Selected file : " + InputFile);
        }

        private void textboxChooseGedcom_textChanged(object sender, System.EventArgs e)
        {
            EnableNextButton();
        }

        private void textboxChooseOutput_textChanged(object sender, System.EventArgs e)
        {
            EnableNextButton();
        }

        private void listboxSelectKey_selectedValueChanged(object sender, System.EventArgs e)
        {
            EnableKeyIndividualsDeleteButton();
        }

        private void buttonSelectKeyAdd_click(object sender, System.EventArgs e)
        {
            /*// Use a dialog box to let them choose an individual
            IndividualBrowserDialog individualBrowserDialog = new IndividualBrowserDialog(this, false);

            FillIndividualsList(individualBrowserDialog.m_sortableListView, false, null, false);
            DialogResult dialogResult = individualBrowserDialog.ShowDialog(this);
            if (dialogResult != DialogResult.OK) {
                return;
            }

            GEDCOMIndividualRecord ir = individualBrowserDialog.FirstSelectedIndividual;

            // Ensure they are only added once
            bool bAlreadyAdded = false;
            foreach (string keyXref in s_config.m_alKeyIndividuals) {
                if (keyXref == ir.XRef) {
                    bAlreadyAdded = true;
                    break;
                }
            }
            if (!bAlreadyAdded) {
                s_config.m_alKeyIndividuals.Add(ir.XRef);
            }

            FillKeyIndividualsList();*/
        }

        private void buttonSelectKeyDelete_click(object sender, System.EventArgs e)
        {
            NameXRefPair xrefPairName = (NameXRefPair)m_listboxSelectKey.SelectedItem;
            if (xrefPairName != null) {
                string xref = xrefPairName.XRef;
                if (xref != null) {
                    int nIndex;
                    int nKeys = CConfig.Instance.KeyIndividuals.Count;
                    for (nIndex = 0; nIndex < nKeys; nIndex++) {
                        if (xref == CConfig.Instance.KeyIndividuals[nIndex]) {
                            CConfig.Instance.KeyIndividuals.RemoveAt(nIndex);
                            break;
                        }
                    }
                }
                FillKeyIndividualsList();
            }
        }

        private void buttonChooseOutputBrowse_click(object sender, System.EventArgs e)
        {
            FolderBrowserDialog folderBrowserDialog1 = new FolderBrowserDialog();
            if (Directory.Exists(m_textboxChooseOutput.Text)) {
                folderBrowserDialog1.SelectedPath = m_textboxChooseOutput.Text;
            } else {
                string sPath = m_textboxChooseOutput.Text;
                int nLastFolder = sPath.LastIndexOf('\\'); // Try folder above
                if (nLastFolder >= 0) {
                    sPath = sPath.Substring(0, nLastFolder);
                }
                if (Directory.Exists(sPath)) {
                    folderBrowserDialog1.SelectedPath = sPath;
                } else {
                    folderBrowserDialog1.SelectedPath = Environment.GetFolderPath(System.Environment.SpecialFolder.Personal) + "\\GEDmill_Output";
                }
            }
            if (folderBrowserDialog1.ShowDialog() == DialogResult.OK) {
                m_textboxChooseOutput.Text = folderBrowserDialog1.SelectedPath;
                m_textboxChooseOutput.SelectionStart = m_textboxChooseOutput.Text.Length;
                m_textboxChooseOutput.SelectionLength = m_textboxChooseOutput.Text.Length;
            }
        }

        private void linklabelAllDone_click(object sender, LinkLabelLinkClickedEventArgs e)
        {
            bool bOldVisitedValue = m_linklabelAllDone.Links[m_linklabelAllDone.Links.IndexOf(e.Link)].Visited;
            try {
                m_linklabelAllDone.Links[m_linklabelAllDone.Links.IndexOf(e.Link)].Visited = true;
                string sURL = m_linklabelAllDone.Text;
                System.Diagnostics.Process.Start(sURL);
            } catch (Exception e2) {
                fLogger.WriteError("Caught exception while viewing folder : {0}", e2);
                m_linklabelAllDone.Links[m_linklabelAllDone.Links.IndexOf(e.Link)].Visited = bOldVisitedValue;
            }
        }

        private void configPanel_BackImage_BrowseButton_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("config panel back image browse button clicked.");

            OpenFileDialog openFileDialog = new OpenFileDialog();

            if (Directory.Exists(m_textboxConfigBackImageEdit.Text)) {
                openFileDialog.InitialDirectory = m_textboxConfigBackImageEdit.Text;
            } else {
                string sPath = m_textboxConfigBackImageEdit.Text;
                int iLastFolder = sPath.LastIndexOf('\\'); // Try parent folder
                if (iLastFolder >= 0) {
                    sPath = sPath.Substring(0, iLastFolder);
                }
                if (Directory.Exists(sPath)) {
                    openFileDialog.InitialDirectory = sPath;
                } else {
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

            if (openFileDialog.ShowDialog() == DialogResult.OK) {
                string sExtn = Path.GetExtension(openFileDialog.FileName);
                sExtn = sExtn.ToLower();
                if (sExtn != ".jpg" && sExtn != ".jpeg" && sExtn != ".png" && sExtn != ".gif" && sExtn != ".bmp") {
                    MessageBox.Show(this, "The file you have selected is not a supported picture type.", "Unsupported Format",
                        MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                } else {
                    m_textboxConfigBackImageEdit.Text = openFileDialog.FileName;
                    m_textboxConfigBackImageEdit.SelectionStart = m_textboxConfigBackImageEdit.Text.Length;
                    m_textboxConfigBackImageEdit.SelectionLength = m_textboxConfigBackImageEdit.Text.Length;
                }
            }
            fLogger.WriteInfo("Selected file : " + m_textboxConfigBackImageEdit.Text);
        }

        private void configPanel_FrontImage_BrowseButton_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("config panel front image browse button clicked.");

            OpenFileDialog openFileDialog1 = new OpenFileDialog();

            if (Directory.Exists(m_textboxConfigFrontImageEdit.Text)) {
                openFileDialog1.InitialDirectory = m_textboxConfigFrontImageEdit.Text;
            } else {
                string sPath = m_textboxConfigFrontImageEdit.Text;
                int nLastFolder = sPath.LastIndexOf('\\'); // Try parent folder
                if (nLastFolder >= 0) {
                    sPath = sPath.Substring(0, nLastFolder);
                }
                if (Directory.Exists(sPath)) {
                    openFileDialog1.InitialDirectory = sPath;
                } else {
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

            if (openFileDialog1.ShowDialog() == DialogResult.OK) {
                string sExtn = Path.GetExtension(openFileDialog1.FileName);
                sExtn = sExtn.ToLower();
                if (sExtn != ".jpg" && sExtn != ".jpeg" && sExtn != ".png" && sExtn != ".gif" && sExtn != ".bmp") {
                    MessageBox.Show(this, "The file you have selected is not a supported picture type.", "Unsupported Format",
                        MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                } else {
                    m_textboxConfigFrontImageEdit.Text = openFileDialog1.FileName;
                    m_textboxConfigFrontImageEdit.SelectionStart = m_textboxConfigFrontImageEdit.Text.Length;
                    m_textboxConfigFrontImageEdit.SelectionLength = m_textboxConfigFrontImageEdit.Text.Length;
                }
            }
            fLogger.WriteInfo("Selected file : " + m_textboxConfigFrontImageEdit.Text);
        }

        private void configPanel_TreeDiagrams_CheckBox_click(object sender, System.EventArgs e)
        {
            EnableMiniTreeButtons();
        }

        private void configPanel_MiniTreeColourIndiBackground_Button_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("configPanel_MiniTreeColourIndiBackground_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiBackground;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeIndiBackground = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        private void configPanel_MiniTreeColourIndiHighlight_Button_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("configPanel_MiniTreeColourIndiHighlight_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiHighlight;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeIndiHighlight = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        private void configPanel_MiniTreeColourIndiBgConcealed_Button_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("configPanel_MiniTreeColourIndiBgConcealed_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiBgConcealed;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeIndiBgConcealed = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        private void configPanel_MiniTreeColourIndiShade_Button_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("configPanel_MiniTreeColourIndiShade_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiShade;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeIndiShade = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        private void configPanel_MiniTreeColourIndiText_Button_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("configPanel_MiniTreeColourIndiText_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiText;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeIndiText = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        private void configPanel_MiniTreeColourIndiLink_Button_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("configPanel_MiniTreeColourIndiLink_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiLink;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeIndiLink = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        private void configPanel_MiniTreeColourBranch_Button_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("configPanel_MiniTreeColourBranch_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeBranch;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeBranch = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        private void configPanel_MiniTreeColourIndiBorder_Button_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("configPanel_MiniTreeColourIndiBorder_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiBorder;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeIndiBorder = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        private void configPanel_MiniTreeColourIndiFgConcealed_Button_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("configPanel_MiniTreeColourIndiFgConcealed_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiFgConcealed;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeIndiFgConcealed = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        private void configPanel_MultiPageIndex_CheckBox_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("config panel multi page index button clicked.");
            EnableMultiPageIndexConfig();
        }

        private void configPanel_AllowMultimedia_CheckBox_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("allow multimedia button clicked.");
            EnableMultimediaConfig();
        }

        private void configPanel_IndiImages_CheckBox_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("config panel multi images button clicked.");
            EnableThumbnailsConfig();
        }

        private void configPanel_ShowWithheldRecords_CheckBox_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("config panel show withheld records button clicked.");
            EnableWithheldConfig();
        }

        private void configPanel_WithheldName_Label_click(object sender, System.EventArgs e)
        {
            fLogger.WriteInfo("config panel withheld label clicked.");
            EnableWithheldConfig();
        }

        private void pruneIndividualsContextMenuDetails_Click(Object sender, System.EventArgs e)
        {
            GEDCOMIndividualRecord ir = null;
            CListableBool lb = null;

            if (lvPruneIndividuals.SelectedItems.Count == 1) {
                lb = (CListableBool)lvPruneIndividuals.SelectedItems[0];
                ir = (GEDCOMIndividualRecord)lb.Record;
            }

            ShowIndividualDetailsDialog(this, lb, ir, true, true);
        }

        private void pruneSourcesContextMenuDetails_Click(Object sender, System.EventArgs e)
        {
            GEDCOMSourceRecord sr = null;
            ListViewItem lvi = null;

            if (lvPruneSources.SelectedItems.Count == 1) {
                lvi = lvPruneSources.SelectedItems[0];
                sr = (GEDCOMSourceRecord)((CListableBool)lvi).Record;
            }

            ShowSourceDetailsDialog(this, ((CListableBool)lvi), sr, true, true);
        }

        private void pruneIndividualsContextMenuUnconnected_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            PruneExcluded = 0;
            PruneIncluded = 0;

            try {
                // Clear visited list
                fTree.BeginPruning();

                // exclude all individuals unless connected in any way to this person through non-excluded people
                foreach (ListViewItem lvi in lvPruneIndividuals.SelectedItems) {
                    if (lvi is CListableBool) {
                        GEDCOMIndividualRecord ir = (GEDCOMIndividualRecord)((CListableBool)lvi).Record;
                        if (ir != null) {
                            // First mark as visited all possible relations of irSubject, not following restricted people
                            // Adds to visited list
                            fTree.PruneMarkConnected(ir);
                        }
                    }
                }
                // Then exclude all unmarked individuals (i.e. not in visited list)
                fTree.PruneUnmarked();

                // Remove visited list
                fTree.EndPruning();
            } catch (Exception ex) {
                ReportPruneError(ex);
            }

            // Rebuild list
            FillIndividualsList(lvPruneIndividuals, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(PruneExcluded, PruneIncluded, "individual");
        }

        private void pruneIndividualsContextMenuDescendantsExc_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            PruneExcluded = 0;
            PruneIncluded = 0;

            try {
                // exclude all individuals descended from this person, including spouses and spouses ancestors. 
                if (lvPruneIndividuals.SelectedItems.Count == 1) {
                    ListViewItem lvi = lvPruneIndividuals.SelectedItems[0];
                    if (lvi is CListableBool) {
                        GEDCOMIndividualRecord ir = (GEDCOMIndividualRecord)((CListableBool)lvi).Record;
                        if (ir != null) {
                            fTree.BeginPruning();
                            fTree.PruneDescendants(ir, true);
                            fTree.EndPruning();
                        }
                    }
                }
            } catch (Exception ex) {
                ReportPruneError(ex);
            }

            // Rebuild list
            FillIndividualsList(lvPruneIndividuals, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(PruneExcluded, PruneIncluded, "individual");
        }

        private void pruneIndividualsContextMenuDescendantsInc_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            PruneExcluded = 0;
            PruneIncluded = 0;

            try {
                // exclude all individuals descended from this person, including spouses and spouses ancestors. 
                if (lvPruneIndividuals.SelectedItems.Count == 1) {
                    ListViewItem lvi = lvPruneIndividuals.SelectedItems[0];
                    if (lvi is CListableBool) {
                        GEDCOMIndividualRecord ir = (GEDCOMIndividualRecord)((CListableBool)lvi).Record;
                        if (ir != null) {
                            fTree.BeginPruning();
                            fTree.PruneDescendants(ir, false);
                            fTree.EndPruning();
                        }
                    }
                }
            } catch (Exception ex) {
                ReportPruneError(ex);
            }

            // Rebuild lists
            FillIndividualsList(lvPruneIndividuals, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(PruneExcluded, PruneIncluded, "individual");
        }

        private void pruneIndividualsContextMenuAncestorsExc_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            PruneExcluded = 0;
            PruneIncluded = 0;

            try {
                // Exclude all individuals descended from this person, including spouses and spouses ancestors. 
                if (lvPruneIndividuals.SelectedItems.Count == 1) {
                    ListViewItem lvi = lvPruneIndividuals.SelectedItems[0];
                    if (lvi is CListableBool) {
                        GEDCOMIndividualRecord ir = (GEDCOMIndividualRecord)((CListableBool)lvi).Record;
                        if (ir != null) {
                            fTree.BeginPruning();
                            fTree.PruneAncestors(ir, true);
                            fTree.EndPruning();
                        }
                    }
                }
            } catch (Exception ex) {
                ReportPruneError(ex);
            }

            // Rebuild lists
            FillIndividualsList(lvPruneIndividuals, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(PruneExcluded, PruneIncluded, "individual");
        }

        private void pruneIndividualsContextMenuAncestorsInc_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            PruneExcluded = 0;
            PruneIncluded = 0;

            try {
                // Exclude all individuals descended from this person, including spouses and spouses ancestors. 
                if (lvPruneIndividuals.SelectedItems.Count == 1) {
                    ListViewItem lvi = lvPruneIndividuals.SelectedItems[0];
                    if (lvi is CListableBool) {
                        GEDCOMIndividualRecord ir = (GEDCOMIndividualRecord)((CListableBool)lvi).Record;
                        if (ir != null) {
                            fTree.BeginPruning();
                            fTree.PruneAncestors(ir, false);
                            fTree.EndPruning();
                        }
                    }
                }
            } catch (Exception ex) {
                ReportPruneError(ex);
            }

            // Rebuild lists
            FillIndividualsList(lvPruneIndividuals, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(PruneExcluded, PruneIncluded, "individual");
        }

        private void pruneIndividualsContextMenuInclude_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            PruneExcluded = 0;
            PruneIncluded = 0;
            foreach (ListViewItem lvi in lvPruneIndividuals.Items) {
                if (lvi is CListableBool) {
                    GEDCOMIndividualRecord ir = (GEDCOMIndividualRecord)((CListableBool)lvi).Record;
                    if (!ir.GetVisibility()) {
                        ++PruneIncluded;
                        ir.SetVisibility(true);
                    }
                }
            }

            // Rebuild lists
            FillIndividualsList(lvPruneIndividuals, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(0, PruneIncluded, "individual");
        }

        // Removes pictures from the selected source
        private void pruneSourcesContextMenuRemovePics_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            int nHidden = 0;
            foreach (ListViewItem lvi in lvPruneSources.SelectedItems) {
                if (lvi is CListableBool) {
                    GEDCOMSourceRecord sr = (GEDCOMSourceRecord)((CListableBool)lvi).Record;
                    if (sr != null) {
                        int nHiddenThisTime = sr.SetAllMFRsVisible(false);
                        nHidden += nHiddenThisTime;
                        if (nHiddenThisTime > 0) {
                            SetSourceSubItems((CListableBool)lvi, sr, true); // Updates list
                        }
                    }
                }
            }

            // Rebuild lists
            FillSourcesList(lvPruneSources, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowHidePicsResult(nHidden);

            if (nHidden > 0) {
                PrunepanelDataChanged = true;
            }
            EnablePrunePanelButtons();
        }

        private void pruneIndividualsContextMenuExclude_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            PruneExcluded = 0;
            PruneIncluded = 0;

            foreach (ListViewItem lvi in lvPruneIndividuals.Items) {
                if (lvi is CListableBool) {
                    GEDCOMIndividualRecord ir = (GEDCOMIndividualRecord)((CListableBool)lvi).Record;
                    if (ir.GetVisibility()) {
                        PruneExcluded++;
                        ir.SetVisibility(false);
                    }
                }
            }

            // Rebuild lists
            FillIndividualsList(lvPruneIndividuals, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(PruneExcluded, 0, "individual");
        }

        private void pruneSourcesContextMenuInclude_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            PruneExcluded = 0;
            PruneIncluded = 0;

            foreach (ListViewItem lvi in lvPruneSources.Items) {
                if (lvi is CListableBool) {
                    GEDCOMSourceRecord sr = (GEDCOMSourceRecord)((CListableBool)lvi).Record;
                    if (!sr.GetVisibility()) {
                        PruneIncluded++;
                        sr.SetVisibility(true);
                    }
                }
            }

            // Rebuild list
            FillSourcesList(lvPruneSources, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(0, PruneIncluded, "source");
        }

        private void pruneSourcesContextMenuExclude_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            PruneExcluded = 0;
            PruneIncluded = 0;

            foreach (ListViewItem lvi in lvPruneSources.Items) {
                if (lvi is CListableBool) {
                    GEDCOMSourceRecord sr = (GEDCOMSourceRecord)((CListableBool)lvi).Record;
                    if (sr.GetVisibility()) {
                        PruneExcluded++;
                        sr.SetVisibility(false);
                    }
                }
            }

            // Rebuild list
            FillSourcesList(lvPruneSources, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(PruneExcluded, 0, "source");
        }

        // Excludes people who aren't dead, but leave people we're not sure about
        private void pruneIndividualsContextMenuAlive_Click(Object sender, System.EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            PruneExcluded = 0;
            PruneIncluded = 0;

            try {
                fTree.BeginPruning();
                foreach (ListViewItem lvi in lvPruneIndividuals.Items) {
                    if (lvi is CListableBool) {
                        GEDCOMIndividualRecord ir = (GEDCOMIndividualRecord)((CListableBool)lvi).Record;
                        if (ir != null && ir.IsLive() && ir.GetVisibility()) {
                            PruneExcluded++;
                            ir.SetVisibility(false);
                        }
                    }
                }
                fTree.EndPruning();
            } catch (Exception ex) {
                ReportPruneError(ex);
            }

            // Rebuild list
            FillIndividualsList(lvPruneIndividuals, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(PruneExcluded, PruneIncluded, "individual");
        }

        private void pruneIndividualsContextMenu_popup(Object sender, EventArgs e)
        {
            int nSelected = lvPruneIndividuals.SelectedItems.Count;
            m_menuitemPruneRecordsIndisUnconnected.Enabled = (nSelected > 0);
            if (nSelected <= 1) {
                m_menuitemPruneRecordsIndisUnconnected.Text = "E&xclude individuals unless navigable from this person";
            } else {
                m_menuitemPruneRecordsIndisUnconnected.Text = string.Format("E&xclude individuals unless navigable from these {0} people", nSelected);
            }

            m_menuitemPruneRecordsIndisDescendantsExc.Enabled = (nSelected == 1);
            m_menuitemPruneRecordsIndisDescendantsInc.Enabled = (nSelected == 1);
            m_menuitemPruneRecordsIndisAncestorsExc.Enabled = (nSelected == 1);
            m_menuitemPruneRecordsIndisAncestorsInc.Enabled = (nSelected == 1);
            m_menuitemPruneRecordsIndisDetails.Enabled = (nSelected == 1);
        }

        private void pruneSourcesContextMenu_popup(Object sender, EventArgs e)
        {
            int nSelected = lvPruneSources.SelectedItems.Count;
            m_menuitemPruneRecordsSourcesDetails.Enabled = (nSelected == 1);
            m_menuitemPruneRecordsSourcesRemovePics.Enabled = (nSelected > 0);
            if (nSelected <= 1) {
                m_menuitemPruneRecordsSourcesRemovePics.Text = "&Remove pictures from this source";
            } else {
                m_menuitemPruneRecordsSourcesRemovePics.Text = string.Format("&Remove pictures from these {0} sources", nSelected);
            }
        }

        private void lvPruneIndividuals_ItemCheck(object sender, ItemCheckEventArgs e)
        {
            if (!fDisablePrunepanelCheckEvent) {
                CListableBool lb = (CListableBool)lvPruneIndividuals.Items[e.Index];
                // For some reason if user presses control while clicking any row of the list, it causes a check event. We don't want any checking to happen in that case.
                if ((Control.ModifierKeys & Keys.Control) == 0) {
                    if ((e.NewValue == CheckState.Checked && !lb.Record.GetVisibility())
                        || (e.NewValue == CheckState.Unchecked && lb.Record.GetVisibility())) {
                        lb.SetRestricted(e.NewValue == CheckState.Unchecked);
                        PrunepanelDataChanged = true;
                        EnablePrunePanelButtons();
                    }
                } else {
                    if (lb.Record != null) {
                        e.NewValue = !lb.Record.GetVisibility() ? CheckState.Unchecked : CheckState.Checked;
                    }
                }
            }
        }

        private void listviewPruneRecordsSources_ItemCheck(object sender, ItemCheckEventArgs e)
        {
            if (!fDisablePrunepanelCheckEvent) {
                CListableBool lb = (CListableBool)lvPruneSources.Items[e.Index];

                // For some reason if user presses control while clicking any row of the list, it causes a check event. We don't want any checking to happen in that case.
                if ((Control.ModifierKeys & Keys.Control) == 0) {
                    if ((e.NewValue == CheckState.Checked && !lb.Record.GetVisibility())
                        || (e.NewValue == CheckState.Unchecked && lb.Record.GetVisibility())) {
                        lb.SetRestricted(e.NewValue == CheckState.Unchecked);
                        PrunepanelDataChanged = true;
                        EnablePrunePanelButtons();
                    }
                } else {
                    if (lb.Record != null) {
                        e.NewValue = !lb.Record.GetVisibility() ? CheckState.Unchecked : CheckState.Checked;
                    }
                }
            }
        }

        #endregion

        // Brings up a CRecordDetailsForm for the given individual
        public void ShowIndividualDetailsDialog(Form formParent, CListableBool lbItem, GEDCOMIndividualRecord ir, bool bCanEditPictures, bool bCheckBoxes)
        {
        }

        // Shows the settings panel
        private void SwitchConfigPanelOn()
        {
            // Disable edit boxes etc. on previous panel to avoid confusing users
            EnableCurrentPanel(false);

            // Move help button to its new location
            m_buttonHelp.Location = new Point(8, 288);

            // Flag panel as being on
            fConfigPanelOn = true;

            // Enable reset button
            m_buttonSettingsCancel.Visible = true;

            // Disable buttons while config panel shown
            m_buttonNext.Visible = false;
            m_buttonBack.Visible = false;
            m_buttonCancel.Visible = false; // To give the panel a "modal" feeling

            // Make config button an "OK" button
            m_buttonSettings.Text = m_sConfigButtonTextOff;
            m_buttonSettings.Location = new Point(344, 288);
            m_buttonSettings.Size = new Size(fDefaultButtonSize);

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
            fConfigPanelOn = false;

            // Restore buttons states
            // Done by ShowCurrentPanel()

            // Make config button back to a config button
            m_buttonSettings.Text = m_sConfigButtonTextOn;
            m_buttonSettings.Location = new Point(88, 288);
            m_buttonSettings.Size = new Size(fConfigButtonSize);

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
            fDisablePrunepanelCheckEvent = true;

            if (fConfigPanelOn) {
                m_panelWelcome.Visible = false;
                m_panelChooseGedcom.Visible = false;
                m_panelPruneRecords.Visible = false;
                m_panelSelectKey.Visible = false;
                m_panelChooseOutput.Visible = false;
                m_panelAllDone.Visible = false;
                m_tabcontrolConfigPanel.Visible = true;
            } // End showing config panel
            else {
                m_panelWelcome.Visible = (fCurrentPanel == 1);
                m_panelChooseGedcom.Visible = (fCurrentPanel == 2);
                m_panelPruneRecords.Visible = (fCurrentPanel == 3);
                m_panelSelectKey.Visible = (fCurrentPanel == 4);
                m_panelChooseOutput.Visible = (fCurrentPanel == 5);
                m_panelAllDone.Visible = (fCurrentPanel == 6);
                m_tabcontrolConfigPanel.Visible = false;

                if (fCurrentPanel <= 1) {
                    m_buttonBack.Visible = false;
                } else {
                    m_buttonBack.Visible = true;
                }

                // Config button disappears once html created
                if (fCurrentPanel >= 6) {
                    m_buttonSettings.Visible = false;
                } else {
                    m_buttonSettings.Visible = true;
                }

                if (fCurrentPanel == 6) {
                    m_buttonCancel.Text = "&Finish";
                    // Can't go back , because we can't undo the file creations.
                    m_buttonBack.Visible = false;
                    m_buttonHelp.Location = new Point(8, 288);
                    m_buttonCancel.Location = new Point(424, 288);
                    m_buttonNext.Visible = false;
                } else if (fCurrentPanel == 9) {
                    m_buttonHelp.Location = new Point(8, 288);
                    m_buttonNext.Text = "&Finish";
                    m_buttonCancel.Visible = false;
                    m_buttonHelp.Visible = false;
                    // Can't go back , because we can't undo the file creations.
                    m_buttonBack.Visible = false;
                } else if (fCurrentPanel == 2) {
                    m_textboxChooseGedcom.Focus();
                    m_textboxChooseGedcom.SelectAll();
                } else if (fCurrentPanel == 4) {
                    m_textboxSelectKey.Focus();
                    m_textboxSelectKey.SelectAll();
                } else if (fCurrentPanel == 5) {
                    m_textboxChooseOutput.Focus();
                    m_textboxChooseOutput.SelectAll();
                } else {
                    m_buttonNext.Text = "&Next >";
                    m_buttonCancel.Visible = true;
                    m_buttonCancel.Text = "&Quit";
                    m_buttonCancel.Location = new Point(8, 288);
                    m_buttonHelp.Visible = true;
                }

                if (fCurrentPanel == 3) {
                    EnablePrunePanelButtons();
                }

                EnableNextButton();
            } // End showing generic panels

            fDisablePrunepanelCheckEvent = false;
        }

        // TODO: GEDCOMFileReference/"_REGION"/CAsidPair

        // Logic for the next page button to ensure that user has completed the current page
        private void EnableNextButton()
        {
            if (fCurrentPanel == 2 && InputFile.Length == 0) {
                m_buttonNext.Enabled = false;
            } else if (fCurrentPanel == 5 && m_textboxChooseOutput.Text.Length == 0) {
                m_buttonNext.Enabled = false;
            } else {
                m_buttonNext.Enabled = true;
            }
        }

        // Logic for the key individuals delete button checks that an individual is selected for deletion
        private void EnableKeyIndividualsDeleteButton()
        {
            if (m_listboxSelectKey.SelectedItems.Count > 0) {
                m_buttonSelectKeyDelete.Enabled = true;
            } else {
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

            if (bEnabled) {
                SetMiniTreeColourConfigButtons();
            } else {
                ClearMiniTreeColourConfigButtons();
            }
        }

        // Fills the controls in each page of the app
        private void InitialiseCurrentPanel()
        {
            switch (fCurrentPanel) {
                case 2:
                    InputFile = CConfig.Instance.InputFilename;
                    m_textboxChooseGedcom.SelectionStart = InputFile.Length;
                    m_textboxChooseGedcom.SelectionLength = InputFile.Length;
                    break;
                case 4:
                    m_textboxSelectKey.Text = CConfig.Instance.SiteTitle;
                    CConfig.Instance.FirstRecordXRef = "";
                    FillKeyIndividualsList();
                    break;
                case 5:
                    m_textboxChooseOutput.Text = CConfig.Instance.OutputFolder;
                    m_textboxChooseOutput.SelectionStart = InputFile.Length;
                    m_textboxChooseOutput.SelectionLength = InputFile.Length;
                    break;
                case 6:
                    m_checkboxAllDoneShowSite.Visible = File.Exists(CConfig.Instance.FrontPageURL);
                    m_checkboxAllDoneShowSite.Checked = CConfig.Instance.OpenWebsiteOnExit;
                    m_linklabelAllDone.Text = CConfig.Instance.OutputFolder;
                    if (CConfig.Instance.FrontPageFilename != "") {
                        m_labelAllDoneStartFile.Text = string.Concat("(The front page for the website is the file ", CConfig.Instance.FrontPageFilename, ".", CConfig.Instance.HtmlExtension, ")");
                        m_labelAllDoneStartFile.Visible = true;
                    } else {
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
            fLogger.WriteInfo("ValidateCurrentPanel()");
            DialogResult result = DialogResult.OK;

            // Loop gives user the option to retry folder creation. Use return to exit.
            while (true) {
                switch (fCurrentPanel) {
                    case 2:
                        if (File.Exists(InputFile) == false) {
                            fLogger.WriteInfo("File not found.");

                            MessageBox.Show(this, "The file you have selected could not be found.", "File Not Found",
                                MessageBoxButtons.OK, MessageBoxIcon.Exclamation);

                            fLogger.WriteInfo("File not found. Returning. ");
                            return false;
                        }

                        ProgressWindow progressWindow = new ProgressWindow();
                        progressWindow.Text = "Reading GEDCOM file";

                        if (CConfig.Instance.OutputFolder == "" || CConfig.Instance.InputFilename != InputFile) {
                            CConfig.Instance.OutputFolder = Path.GetDirectoryName(InputFile);
                            CConfig.Instance.OutputFolder += "\\GEDmill_Output";
                        }
                        if (CConfig.Instance.InputFilename != InputFile) {
                            CConfig.Instance.InputFilename = InputFile;
                            CConfig.Instance.FirstRecordXRef = "";

                            // User is using a new file, so key individuals won't be the same as they were in config
                            CConfig.Instance.KeyIndividuals = new List<string>();
                            CConfig.Instance.FirstRecordXRef = "";
                        }

                        var provider = new GEDCOMProvider(fTree);
                        provider.LoadFromFile(InputFile);
                        //m_gedcom.ProgressCallback = progressWindow;

                        fLogger.WriteInfo("Reading GEDCOM file " + InputFile);

                        //ThreadStart threadStart = new ThreadStart(m_gedcom.ParseFile);
                        //Thread threadWorker = new Thread(threadStart);

                        fLogger.WriteInfo("Starting thread");

                        //threadWorker.Start();
                        //result = progressWindow.ShowDialog(this);
                        //threadWorker.Join();

                        //LogFile.TheLogFile.WriteLine(LogFile.DT_APP.Info("Thread finished, result=" + result.ToString());

                        if (result == DialogResult.Abort) // Abort means abnormal failure (ie. not user pressing cancel)
                        {
                            // Abort means there were file IO errors
                            MessageBox.Show(this, string.Format("A problem was encountered while reading the GEDCOM file"), CConfig.SoftwareName,
                                MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        } else if (result == DialogResult.Retry) // Something went wrong, let user retry
                          {
                            // Currently the only thing this can be is "file already open"
                            MessageBox.Show(this, "A problem was encountered while reading the GEDCOM file.\r\n\r\nPerhaps the file is already open elsewhere.", CConfig.SoftwareName,
                                MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                        }
                        if (result != DialogResult.OK) {
                            return false; // Go back and let user retry loading file.
                        }
                        PrunepanelDataChanged = false; // A fresh file, no user changes yet.
                        FillIndividualsList(lvPruneIndividuals, true);
                        FillSourcesList(lvPruneSources, true);

                        return true;

                    case 3:
                        // Go through individuals list and set restricted flag as appropriate
                        bool somethingChecked = false;
                        foreach (ListViewItem li in lvPruneIndividuals.Items) {
                            bool isChecked = li.Checked;
                            if (isChecked) {
                                somethingChecked = true;
                            }
                            // Already done on click event: ((CListableBool)li).SetRestricted( !bChecked );
                            if (!isChecked) {
                                fTree.RestrictAssociatedSources((GEDCOMIndividualRecord)((CListableBool)li).Record);
                            }
                        }

                        if (somethingChecked == false) {
                            MessageBox.Show(this, "Please select at least one individual.", "No Individuals Selected",
                                MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                            return false;
                        }

                        if (PrunepanelDataChanged) {
                            DialogResult dialogResult = MessageBox.Show(this, "You have made changes which will affect the website but have not saved them.\r\nWould you like to save them now?", "Save changes",
                                MessageBoxButtons.YesNo, MessageBoxIcon.Question);
                            if (dialogResult == DialogResult.Yes) {
                                buttonPruneRecordsSave_click(null, null);
                            }
                        }
                        return true;

                    case 4:
                        CConfig.Instance.SiteTitle = m_textboxSelectKey.Text;
                        return true;

                    case 5:
                        CConfig.Instance.OutputFolder = m_textboxChooseOutput.Text;
                        string imageFolder = CConfig.Instance.ImageFolder;
                        string outputFolder = CConfig.Instance.OutputFolder;
                        if (outputFolder != "") {
                            outputFolder = outputFolder + '\\';
                        }
                        string absImageFolder = string.Concat(outputFolder, imageFolder);

                        bool preserveFiles = false;
                        if (CConfig.Instance.PreserveFrontPage || CConfig.Instance.PreserveStylesheet) {
                            // To generate warning if deleting folder & files.
                            preserveFiles = true;
                        }

                        while (true) {
                            result = PrepareOutputDirectory(outputFolder, preserveFiles);
                            if (result == DialogResult.Cancel) {
                                return false;
                            }
                            if (result == DialogResult.OK) {
                                break;
                            }
                        }

                        while (true) {
                            result = PrepareOutputDirectory(absImageFolder, false);
                            if (result == DialogResult.Cancel) {
                                return false;
                            }
                            if (result == DialogResult.OK) {
                                break;
                            }
                        }

                        if (CreateWebsite(outputFolder, absImageFolder)) {
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
        private void FillIndividualsList(GKListView listView, bool firstColumnIsCheckbox)
        {
            var indiRecs = fTree.GetRecords<GEDCOMIndividualRecord>();
            fLogger.WriteInfo("FillIndividualsList() : " + indiRecs.Count.ToString());

            fDisablePrunepanelCheckEvent = true;

            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            listView.Clear();

            listView.View = View.Details;
            int nameWidth = listView.Width - 70 - 70 - 20;
            if (firstColumnIsCheckbox) {
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
            ListViewItem[] temporaryItemsList = new ListViewItem[indiRecs.Count];

            int nItem = 0;
            foreach (GEDCOMIndividualRecord ir in indiRecs) {
                // Only allow fully unrestricted individuals.
                /*if (excludeRestricted && !ir.GetVisibility(EVisibility.Visible)) {
                    continue;
                }*/

                CListableBool lbItem;
                if (firstColumnIsCheckbox) {
                    lbItem = new CListableBool(ir, true);
                } else {
                    string sSurname = "";
                    string sFirstName = "";
                    CConfig.Instance.CapitaliseName(ir.GetPrimaryFullName(), ref sFirstName, ref sSurname);
                    /*if (ir.NameSuffix != null && ir.NameSuffix != "") {
                        sFirstName += ", " + ir.NameSuffix;
                    }*/
                    lbItem = new CListableBool(ir, sSurname, sFirstName, false);
                }

                SetIndividualSubItems(lbItem, ir, firstColumnIsCheckbox);

                lbItem.Checked = ir.GetVisibility();
                temporaryItemsList[nItem++] = lbItem;
            }

            listView.Items.AddRange(temporaryItemsList);
            listView.Sort();

            m_tabpagePruneRecordsIndis.Text = "Individuals (" + listView.Items.Count + ")";

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            fDisablePrunepanelCheckEvent = false;
        }

        // Attaches sub-items to a list item (before the list item is added to the list)
        private void SetIndividualSubItems(CListableBool lbItem, GEDCOMIndividualRecord ir, bool checkBoxes)
        {
            // Save checkbox state because SubItems.Clear() clears item.Text and item.Checked as well, so replace old value after calling Clear().
            bool bWasChecked = lbItem.Checked;
            string sItemText = lbItem.Text;
            lbItem.SubItems.Clear();
            lbItem.Text = sItemText;
            lbItem.Checked = bWasChecked;

            // If the list view has check boxes, the item is for the checkbox.
            // Otherwise the item is for the name, and so the sub items won't include the name.
            if (checkBoxes) {
                string sSurname = "";
                string sFirstName = "";
                var persName = (ir.PersonalNames.Count > 0) ? ir.PersonalNames[0].StringValue : "";
                CConfig.Instance.CapitaliseName(persName, ref sFirstName, ref sSurname);
                /*if (ir.NameSuffix != null && ir.NameSuffix != "") {
                    sFirstName += ", " + ir.NameSuffix;
                }*/
                lbItem.SubItems.Add(new CListableName(ir, sSurname, sFirstName));
            }

            var lifeDatesX = ir.GetLifeDates();
            var birthDate = (lifeDatesX.BirthEvent == null) ? null : lifeDatesX.BirthEvent.Date;
            var deathDate = (lifeDatesX.DeathEvent == null) ? null : lifeDatesX.DeathEvent.Date;

            lbItem.SubItems.Add(new CListableYear(birthDate));
            lbItem.SubItems.Add(new CListableYear(deathDate));
            lbItem.SubItems.Add(new CListableString(ir.XRef));

            string uref = (ir.UserReferences.Count > 0) ? ir.UserReferences[0].StringValue : "";
            lbItem.SubItems.Add(new CListableString(uref));

            int nVisiblePics = ir.CountVisibleMFRs();
            int nTotalPics = ir.CountAllMFRs();
            if (nVisiblePics != nTotalPics) {
                lbItem.SubItems.Add(new CListableNumber(nVisiblePics, string.Format("{0}/{1}", nVisiblePics, nTotalPics)));
            } else {
                lbItem.SubItems.Add(new CListableNumber(nTotalPics, string.Format("{0}", nTotalPics)));
            }
        }

        // Populates the list of source records for inclusion/exclusion in the website
        private void FillSourcesList(GKListView listView, bool firstColumnIsCheckbox)
        {
            var sources = fTree.GetRecords<GEDCOMSourceRecord>();
            fLogger.WriteInfo("FillSourcesList() : " + sources.Count.ToString());

            fDisablePrunepanelCheckEvent = true; // call to item.Checked below invokes event handler.

            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            listView.Clear();

            listView.View = View.Details;
            int nWidthTitle = listView.Width - 140 - 20;
            if (firstColumnIsCheckbox) {
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

            ListViewItem[] temporaryItemsList = new ListViewItem[sources.Count];
            int nItem = 0;
            foreach (GEDCOMSourceRecord sr in sources) {
                CListableBool item = new CListableBool(sr, firstColumnIsCheckbox);
                SetSourceSubItems(item, sr, firstColumnIsCheckbox);
                item.Checked = sr.GetVisibility();
                temporaryItemsList[nItem++] = item;
            }

            listView.Items.AddRange(temporaryItemsList);
            listView.Sort();

            m_tabpagePruneRecordsSources.Text = "Sources (" + listView.Items.Count + ")";

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            fDisablePrunepanelCheckEvent = false;
        }

        // Attaches sub-items to a list item (before the list item is added to the list)
        private void SetSourceSubItems(CListableBool lbItem, GEDCOMSourceRecord sr, bool firstColumnIsCheckbox)
        {
            // Store checkbox value because SubItems.Clear() clears item.Text and item.Checked as well!
            bool wasChecked = lbItem.Checked;
            lbItem.SubItems.Clear();
            lbItem.Checked = wasChecked;

            if (firstColumnIsCheckbox) {
                // First nColumn (ie. item) is checkbox, so first sub-item is title.
                lbItem.SubItems.Add(new CListableSource(sr));
            }

            string repositories = "";
            foreach (GEDCOMRepositoryCitation src in sr.RepositoryCitations) {
                GEDCOMRepositoryRecord rr = src.Value as GEDCOMRepositoryRecord;
                if (rr != null) {
                    if (!string.IsNullOrEmpty(rr.RepositoryName)) {
                        if (repositories != "") {
                            repositories += ", ";
                        }
                        repositories += rr.RepositoryName;
                    }
                }
            }
            lbItem.SubItems.Add(new CListableString(repositories));

            int nBirths = 0;
            int nMarriages = 0;
            int nDeaths = 0;
            int nIndis = 0;
            int nFamilies = 0;
            int nNotes = 0;
            int nOther = 0;
            int nCitations = 0;

            // TODO
            /*if (sr.m_alBackreferences.Count > 0) {
                foreach (CBackReference br in sr.m_alBackreferences) {
                    switch (br.m_sEventType) {
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
                            switch (br.m_ertRecordType) {
                                case GEDCOMRecordType.rtIndividual:
                                    nCitations++;
                                    nIndis++;
                                    break;
                                case GEDCOMRecordType.rtFamily:
                                    // Strictly this should be plus 2 if husb & wife both known, otherwise 1 or 0.
                                    nCitations++;
                                    nFamilies++;
                                    break;
                                case GEDCOMRecordType.rtNote:
                                    nNotes++;
                                    break;
                                default:
                                    nOther++;
                                    break;
                            }
                            break;
                    }
                }
            }*/

            lbItem.SubItems.Add(new CListableNumber(nCitations));
            lbItem.SubItems.Add(new CListableNumber(nBirths));
            lbItem.SubItems.Add(new CListableNumber(nMarriages));
            lbItem.SubItems.Add(new CListableNumber(nDeaths));
            lbItem.SubItems.Add(new CListableString(sr.XRef));

            int nVisiblePics = sr.CountVisibleMFRs();
            int nTotalPics = sr.CountAllMFRs();

            if (nVisiblePics != nTotalPics) {
                lbItem.SubItems.Add(new CListableNumber(nVisiblePics, string.Format("{0}/{1}", nVisiblePics, nTotalPics)));
            } else {
                lbItem.SubItems.Add(new CListableNumber(nTotalPics, string.Format("{0}", nTotalPics)));
            }
            lbItem.Checked = wasChecked;
        }

        // Spawns the website creation thread, which calls CWebsite.Create to do the work.
        private bool CreateWebsite(string outputFolder, string imagesFolder)
        {
            fLogger.WriteInfo("Creating website");
            DialogResult dialogResult;

            // If user has specified a background image, check it exists
            if (!string.IsNullOrEmpty(CConfig.Instance.BackgroundImage) && File.Exists(CConfig.Instance.BackgroundImage) == false) {
                fLogger.WriteError("Can't find background image " + CConfig.Instance.BackgroundImage);

                dialogResult = MessageBox.Show(this, string.Format("The file {0} is missing. \r\nPages will be created without any background image.", CConfig.Instance.BackgroundImage),
                    "Creating website", MessageBoxButtons.OKCancel, MessageBoxIcon.Exclamation);
                if (dialogResult == DialogResult.Cancel) {
                    fLogger.WriteInfo("Message box cancelled (1)");
                    return false;
                }
            }

            fLogger.WriteInfo("Starting progress window");

            ProgressWindow progressWindow = new ProgressWindow();
            progressWindow.Text = "Creating web pages";

            Website website = new Website(fTree, progressWindow);

            ThreadStart threadStart = new ThreadStart(website.Create);
            Thread threadWorker = new Thread(threadStart);

            fLogger.WriteInfo("Starting progress thread");

            dialogResult = DialogResult.Abort;
            try {
                threadWorker.Start();
                dialogResult = progressWindow.ShowDialog(this);
            } catch (HTMLException e) {
                MessageBox.Show(this, string.Concat(e.Message), "Creating website",
                    MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            } finally {
                threadWorker.Join();
            }

            if (dialogResult == DialogResult.Abort) {
                fLogger.WriteInfo("Thread aborted");
                if (progressWindow.ThreadError.Message == "") {
                    // Abort means there were file IO errors
                    MessageBox.Show(this, string.Format("A problem was encountered while creating the website files"), CConfig.SoftwareName,
                        MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                } else {
                    // Abort means there were file IO errors
                    MessageBox.Show(this, progressWindow.ThreadError.Message, CConfig.SoftwareName,
                        MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                }
            }

            if (dialogResult != DialogResult.OK) {
                fLogger.WriteInfo("Dialog not OK");
                return false;
            }

            fLogger.WriteInfo("Website create done.");

            return true;
        }

        // Enable the current page of the wizard
        private void EnableCurrentPanel(bool enable)
        {
            if (fConfigPanelOn) {
                m_tabcontrolConfigPanel.Enabled = enable;
                // Disable all other panels
                enable = false;
            } else {
                m_panelWelcome.Enabled = (fCurrentPanel == 1 && enable);
                m_panelChooseGedcom.Enabled = (fCurrentPanel == 2 && enable);
                m_panelPruneRecords.Enabled = (fCurrentPanel == 3 && enable);
                m_panelSelectKey.Enabled = (fCurrentPanel == 4 && enable);
                m_panelChooseOutput.Enabled = (fCurrentPanel == 5 && enable);
                m_panelAllDone.Enabled = (fCurrentPanel == 6 && enable);

                m_tabcontrolConfigPanel.Enabled = false;
            }

            m_picturebox.Enabled = enable;
        }

        // Displays useful information about a source record in a dialog box
        private void ShowSourceDetailsDialog(Form formParent, CListableBool lbItem, GEDCOMSourceRecord sr, bool bCanEditPictures, bool bFirstColumnIsCheckbox)
        {
        }

        // Reports any exception thrown during the prune operation
        private void ReportPruneError(Exception e)
        {
            MessageBox.Show(this, string.Format("A problem was encountered while navigating the tree structure:\r\n\r\n{0}", e.StackTrace), CConfig.SoftwareName,
                MessageBoxButtons.OK, MessageBoxIcon.Exclamation);

            fLogger.WriteInfo(string.Format("Caught navigation exception : {0}", e.ToString()));
        }

        // Displays the statistics of the prune operation
        private void ShowPruneResult(int nExcluded, int nIncluded, string sType)
        {
            string sMsg = "";
            if (nExcluded == 0 && nIncluded == 0) {
                sMsg = "No changes made.";
            } else {
                if (nIncluded != 0) {
                    sMsg = string.Format("{0} {1}{2} checked.", nIncluded, sType, nIncluded > 1 ? "s" : "");
                }
                if (nExcluded != 0) {
                    if (sMsg != "") {
                        sMsg += "\r\n";
                    }
                    sMsg += string.Format("{0} {1}{2} unchecked.", nExcluded, sType, nExcluded > 1 ? "s" : "");
                }
            }

            MessageBox.Show(this, sMsg, "Select Records", MessageBoxButtons.OK, MessageBoxIcon.Information);
        }

        // Displays the statistics of the remove pictures operation
        private void ShowHidePicsResult(int nHidden)
        {
            string sMsg = "";
            if (nHidden == 0) {
                sMsg = "No multimedia files hidden.";
            } else {
                if (nHidden != 0) {
                    sMsg = string.Format("{0} multimedia file{1} hidden.", nHidden, nHidden > 1 ? "s" : "");
                }
            }
            MessageBox.Show(this, sMsg, "Hide Pictures", MessageBoxButtons.OK, MessageBoxIcon.Information);
        }

        // Initialises config panel controls
        private void LoadConfigPanelSettings()
        {
            m_textboxConfigFrontImageEdit.Text = CConfig.Instance.FrontPageImageFilename;
            m_textboxConfigFrontImageEdit.SelectionStart = m_textboxConfigFrontImageEdit.Text.Length;
            m_textboxConfigFrontImageEdit.SelectionLength = m_textboxConfigFrontImageEdit.Text.Length;
            m_textboxConfigBackImageEdit.Text = CConfig.Instance.BackgroundImage;
            m_textboxConfigBackImageEdit.SelectionStart = m_textboxConfigBackImageEdit.Text.Length;
            m_textboxConfigBackImageEdit.SelectionLength = m_textboxConfigBackImageEdit.Text.Length;
            m_textboxConfigIndiImageWidth.Text = CConfig.Instance.MaxImageWidth.ToString();
            m_textboxConfigIndiImageHeight.Text = CConfig.Instance.MaxImageHeight.ToString();
            m_textboxConfigSourceImageWidth.Text = CConfig.Instance.MaxSourceImageWidth.ToString();
            m_textboxConfigSourceImageHeight.Text = CConfig.Instance.MaxSourceImageHeight.ToString();
            m_textboxConfigThumbnailImageWidth.Text = CConfig.Instance.MaxThumbnailImageWidth.ToString();
            m_textboxConfigThumbnailImageHeight.Text = CConfig.Instance.MaxThumbnailImageHeight.ToString();
            m_comboboxConfigHtmlExtn.Items.Clear();
            m_comboboxConfigHtmlExtn.Items.AddRange(new object[] { ".htm", ".html" });
            m_comboboxConfigHtmlExtn.SelectedIndex = (CConfig.Instance.HtmlExtension == "html" ? 1 : 0);
            m_textboxConfigNoName.Text = CConfig.Instance.UnknownName;
            m_textboxConfigWithheldName.Text = CConfig.Instance.ConcealedName;
            m_radiobuttonConfigWithheldNameLabel.Checked = !CConfig.Instance.UseWithheldNames;
            m_radiobuttonConfigWithheldNameName.Checked = CConfig.Instance.UseWithheldNames;
            m_checkboxConfigCapNames.Checked = (CConfig.Instance.NameCapitalisation == 1);
            m_checkboxConfigCapEvents.Checked = CConfig.Instance.CapitaliseEventDescriptions;
            m_checkboxConfigHideEmails.Checked = CConfig.Instance.ObfuscateEmails;
            m_checkboxConfigOccupationHeadline.Checked = CConfig.Instance.OccupationHeadline;
            m_checkboxConfigAllowTrailingSpaces.Checked = CConfig.Instance.DataMayEndWithWhitespace;
            m_checkboxConfigAllowTrailingSpaces.Enabled = fCurrentPanel < 3; // Only enable this setting if we haven't loaded any GEDCOM yet.
            m_checkboxConfigShowWithheldRecords.Checked = CConfig.Instance.OnlyConceal;
            m_textboxConfigTabSpaces.Text = CConfig.Instance.TabSpaces.ToString();
            m_textboxConfigCommentary.Text = CConfig.Instance.CommentaryText;
            m_checkboxConfigCommentaryIsHtml.Checked = CConfig.Instance.CommentaryIsHtml;
            m_checkboxConfigStats.Checked = CConfig.Instance.ShowFrontPageStats;
            m_checkboxConfigPreserveFrontPage.Checked = CConfig.Instance.PreserveFrontPage;
            m_checkboxConfigPreserveStylesheet.Checked = CConfig.Instance.PreserveStylesheet;
            m_checkboxConfigIncludeHelppage.Checked = CConfig.Instance.IncludeHelpPage;
            m_checkboxConfigCdrom.Checked = CConfig.Instance.CreateCDROMFiles;
            m_checkboxConfigNonPictures.Checked = CConfig.Instance.AllowNonPictures;
            m_checkboxConfigIndiImages.Checked = CConfig.Instance.AllowMultipleImages;
            m_checkboxConfigTreeDiagrams.Checked = CConfig.Instance.ShowMiniTrees;
            m_checkboxConfigTreeDiagramsFakeBg.Checked = CConfig.Instance.FakeMiniTreeTransparency;
            m_textboxConfigEmail.Text = CConfig.Instance.UserEmailAddress;
            m_textboxConfigIndexName.Text = CConfig.Instance.FrontPageFilename;
            m_labelConfigIndexNameExtn.Text = "." + CConfig.Instance.HtmlExtension;
            m_textboxConfigStylesheetName.Text = CConfig.Instance.StylesheetFilename;
            if (CConfig.Instance.MainWebsiteLink.Length == 0) {
                m_textboxConfigUserLink.Text = "http://";
            } else {
                m_textboxConfigUserLink.Text = CConfig.Instance.MainWebsiteLink;
            }
            m_comboboxConfigTreeDiagramsFormat.Items.Clear();
            m_comboboxConfigTreeDiagramsFormat.Items.AddRange(new object[] { "gif", "png" });
            m_comboboxConfigTreeDiagramsFormat.SelectedIndex = (CConfig.Instance.MiniTreeImageFormat == "png" ? 1 : 0);
            m_checkboxConfigMultiPageIndex.Checked = CConfig.Instance.MultiPageIndexes;
            m_checkboxConfigUserRefInIndex.Checked = CConfig.Instance.IncludeUserRefInIndex;
            m_textboxConfigMultiPageIndexNumber.Text = CConfig.Instance.IndividualsPerIndexPage.ToString();
            m_checkboxConfigKeepOriginals.Checked = CConfig.Instance.LinkOriginalPicture;
            m_checkboxConfigRenameOriginals.Checked = CConfig.Instance.RenameOriginalPicture;
            m_checkboxConfigW3C.Checked = CConfig.Instance.IncludeValiditySticker;
            m_checkboxConfigUserRecFilename.Checked = CConfig.Instance.UserRecFilename;
            m_textboxConfigCustomFooter.Text = CConfig.Instance.CustomFooter;
            m_checkboxConfigFooterIsHtml.Checked = CConfig.Instance.FooterIsHtml;
            m_checkboxConfigConserveTreeWidth.Checked = CConfig.Instance.ConserveTreeWidth;
            m_checkboxConfigKeepSiblingOrder.Checked = CConfig.Instance.KeepSiblingOrder;
            m_checkboxConfigAllowMultimedia.Checked = CConfig.Instance.AllowMultimedia;

            m_colorConfigMiniTreeBranch = Paintbox.ConvertColour(CConfig.Instance.MiniTreeColourBranch);
            m_colorConfigMiniTreeIndiBorder = Paintbox.ConvertColour(CConfig.Instance.MiniTreeColourIndiBorder);
            m_colorConfigMiniTreeIndiBackground = Paintbox.ConvertColour(CConfig.Instance.MiniTreeColourIndiBackground);
            m_colorConfigMiniTreeIndiHighlight = Paintbox.ConvertColour(CConfig.Instance.MiniTreeColourIndiHighlight);
            m_colorConfigMiniTreeIndiBgConcealed = Paintbox.ConvertColour(CConfig.Instance.MiniTreeColourIndiBgConcealed);
            m_colorConfigMiniTreeIndiFgConcealed = Paintbox.ConvertColour(CConfig.Instance.MiniTreeColourIndiFgConcealed);
            m_colorConfigMiniTreeIndiShade = Paintbox.ConvertColour(CConfig.Instance.MiniTreeColourIndiShade);
            m_colorConfigMiniTreeIndiText = Paintbox.ConvertColour(CConfig.Instance.MiniTreeColourIndiText);
            m_colorConfigMiniTreeIndiLink = Paintbox.ConvertColour(CConfig.Instance.MiniTreeColourIndiLink);
            m_colorConfigMiniTreeBackground = Paintbox.ConvertColour(CConfig.Instance.MiniTreeColourBackground);

            m_checkboxConfigSupressBackreferences.Checked = !CConfig.Instance.SupressBackreferences;

            SetMiniTreeColourConfigButtons();

            EnableMultiPageIndexConfig();
            EnableMultimediaConfig();
            EnableWithheldConfig();
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
            CConfig.Instance.FrontPageImageFilename = m_textboxConfigFrontImageEdit.Text;
            CConfig.Instance.BackgroundImage = m_textboxConfigBackImageEdit.Text;

            try {
                // Sanity check value
                uint maxImageWidth = System.UInt32.Parse(m_textboxConfigIndiImageWidth.Text);
                if (maxImageWidth > 0 && maxImageWidth <= 300) {
                    CConfig.Instance.MaxImageWidth = maxImageWidth;
                } else if (CConfig.Instance.MaxImageWidth != maxImageWidth && maxImageWidth > 300) {
                    DialogResult dialogResult = MessageBox.Show(this,
                        string.Format("Setting the image width to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", maxImageWidth),
                        "Change settings", MessageBoxButtons.YesNo, MessageBoxIcon.Question);
                    if (dialogResult == DialogResult.Yes) {
                        CConfig.Instance.MaxImageWidth = maxImageWidth;
                    }
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            try {
                // Sanity check value
                uint maxImageHeight = System.UInt32.Parse(m_textboxConfigIndiImageHeight.Text);
                if (maxImageHeight > 0 && maxImageHeight <= 800) {
                    CConfig.Instance.MaxImageHeight = maxImageHeight;
                } else if (CConfig.Instance.MaxImageHeight != maxImageHeight && maxImageHeight > 800) {
                    DialogResult dialogResult = MessageBox.Show(this,
                        string.Format("Setting the image height to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", maxImageHeight),
                        "Change settings", MessageBoxButtons.YesNo, MessageBoxIcon.Question);
                    if (dialogResult == DialogResult.Yes) {
                        CConfig.Instance.MaxImageHeight = maxImageHeight;
                    }
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            try {
                // Sanity check value
                uint maxSourceImageWidth = System.UInt32.Parse(m_textboxConfigSourceImageWidth.Text);
                if (maxSourceImageWidth > 0 && maxSourceImageWidth <= 800) {
                    CConfig.Instance.MaxSourceImageWidth = maxSourceImageWidth;
                } else if (CConfig.Instance.MaxSourceImageWidth != maxSourceImageWidth && maxSourceImageWidth > 800) {
                    DialogResult dialogResult = MessageBox.Show(this,
                        string.Format("Setting the source width to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", maxSourceImageWidth),
                        "Change settings", MessageBoxButtons.YesNo, MessageBoxIcon.Question);
                    if (dialogResult == DialogResult.Yes) {
                        CConfig.Instance.MaxSourceImageWidth = maxSourceImageWidth;
                    }
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            try {
                // Sanity check value
                uint maxSourceImageHeight = System.UInt32.Parse(m_textboxConfigSourceImageHeight.Text);
                if (maxSourceImageHeight > 0 && maxSourceImageHeight <= 800) {
                    CConfig.Instance.MaxSourceImageHeight = maxSourceImageHeight;
                } else if (CConfig.Instance.MaxSourceImageHeight != maxSourceImageHeight && maxSourceImageHeight > 800) {
                    DialogResult dialogResult = MessageBox.Show(this,
                        string.Format("Setting the source height to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", maxSourceImageHeight),
                        "Change settings", MessageBoxButtons.YesNo, MessageBoxIcon.Question);
                    if (dialogResult == DialogResult.Yes) {
                        CConfig.Instance.MaxSourceImageHeight = maxSourceImageHeight;
                    }
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            try {
                // Sanity check value
                uint maxThumbnailImageWidth = System.UInt32.Parse(m_textboxConfigThumbnailImageWidth.Text);
                if (maxThumbnailImageWidth > 0 && maxThumbnailImageWidth < 80) {
                    CConfig.Instance.MaxThumbnailImageWidth = maxThumbnailImageWidth;
                } else if (CConfig.Instance.MaxThumbnailImageWidth != maxThumbnailImageWidth && maxThumbnailImageWidth > 80) {
                    DialogResult dialogResult = MessageBox.Show(this,
                        string.Format("Setting the thumbnail width to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", maxThumbnailImageWidth),
                        "Change settings", MessageBoxButtons.YesNo, MessageBoxIcon.Question);
                    if (dialogResult == DialogResult.Yes) {
                        CConfig.Instance.MaxThumbnailImageWidth = maxThumbnailImageWidth;
                    }
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            try {
                // Sanity check value
                uint maxThumbnailImageHeight = System.UInt32.Parse(m_textboxConfigThumbnailImageHeight.Text);
                if (maxThumbnailImageHeight > 0 && maxThumbnailImageHeight < 80) {
                    CConfig.Instance.MaxThumbnailImageHeight = maxThumbnailImageHeight;
                } else if (CConfig.Instance.MaxThumbnailImageHeight != maxThumbnailImageHeight && maxThumbnailImageHeight > 80) {
                    DialogResult dialogResult = MessageBox.Show(this,
                        string.Format("Setting the thumbnail height to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", maxThumbnailImageHeight),
                        "Change settings", MessageBoxButtons.YesNo, MessageBoxIcon.Question);
                    if (dialogResult == DialogResult.Yes) {
                        CConfig.Instance.MaxThumbnailImageHeight = maxThumbnailImageHeight;
                    }
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            CConfig.Instance.HtmlExtension = (m_comboboxConfigHtmlExtn.SelectedIndex == 1 ? "html" : "htm");
            CConfig.Instance.IncludeValiditySticker = m_checkboxConfigW3C.Checked;
            CConfig.Instance.UserRecFilename = m_checkboxConfigUserRecFilename.Checked;
            if (m_textboxConfigNoName.Text.Length > 0) {
                CConfig.Instance.UnknownName = m_textboxConfigNoName.Text;
            }
            if (m_textboxConfigWithheldName.Text.Length > 0) {
                CConfig.Instance.ConcealedName = m_textboxConfigWithheldName.Text;
            }
            CConfig.Instance.UseWithheldNames = m_radiobuttonConfigWithheldNameName.Checked;
            CConfig.Instance.NameCapitalisation = (m_checkboxConfigCapNames.Checked ? 1 : 0);
            CConfig.Instance.CapitaliseEventDescriptions = m_checkboxConfigCapEvents.Checked;
            CConfig.Instance.ObfuscateEmails = m_checkboxConfigHideEmails.Checked;
            CConfig.Instance.OccupationHeadline = m_checkboxConfigOccupationHeadline.Checked;
            CConfig.Instance.DataMayEndWithWhitespace = m_checkboxConfigAllowTrailingSpaces.Checked;
            CConfig.Instance.OnlyConceal = m_checkboxConfigShowWithheldRecords.Checked;

            try {
                // Sanity check value
                uint tabSpaces = System.UInt32.Parse(m_textboxConfigTabSpaces.Text);
                if (tabSpaces > 0 && tabSpaces < 64) {
                    CConfig.Instance.TabSpaces = tabSpaces;
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            CConfig.Instance.CommentaryText = m_textboxConfigCommentary.Text;
            CConfig.Instance.CommentaryIsHtml = m_checkboxConfigCommentaryIsHtml.Checked;
            CConfig.Instance.ShowFrontPageStats = m_checkboxConfigStats.Checked;
            CConfig.Instance.ShowMiniTrees = m_checkboxConfigTreeDiagrams.Checked;
            CConfig.Instance.FakeMiniTreeTransparency = m_checkboxConfigTreeDiagramsFakeBg.Checked;
            CConfig.Instance.UserEmailAddress = m_textboxConfigEmail.Text;
            CConfig.Instance.PreserveFrontPage = m_checkboxConfigPreserveFrontPage.Checked;
            CConfig.Instance.PreserveStylesheet = m_checkboxConfigPreserveStylesheet.Checked;
            CConfig.Instance.IncludeHelpPage = m_checkboxConfigIncludeHelppage.Checked;
            CConfig.Instance.CreateCDROMFiles = m_checkboxConfigCdrom.Checked;
            CConfig.Instance.AllowMultipleImages = m_checkboxConfigIndiImages.Checked;
            CConfig.Instance.AllowNonPictures = m_checkboxConfigNonPictures.Checked;
            CConfig.Instance.LinkOriginalPicture = m_checkboxConfigKeepOriginals.Checked;
            CConfig.Instance.RenameOriginalPicture = m_checkboxConfigRenameOriginals.Checked;

            // Validate and strip trailing .html or .htm in case user has put them on
            string sFrontPageFilename = m_textboxConfigIndexName.Text;
            string sFrontPageFilenameUpper = sFrontPageFilename.ToUpper();
            if (sFrontPageFilenameUpper.LastIndexOf(".HTML") >= 0) {
                sFrontPageFilename = sFrontPageFilename.Substring(0, sFrontPageFilename.Length - 5);
            } else if (sFrontPageFilenameUpper.LastIndexOf(".HTM") >= 0) {
                sFrontPageFilename = sFrontPageFilename.Substring(0, sFrontPageFilename.Length - 4);
            }
            CConfig.Instance.FrontPageFilename = sFrontPageFilename;

            // Validate and strip trailing .css in case user has put them on
            string sStylesheetFilename = m_textboxConfigStylesheetName.Text;
            if (sStylesheetFilename.Length > 0) {
                string sStylesheetFilenameUpper = sStylesheetFilename.ToUpper();
                if (sStylesheetFilename.LastIndexOf(".CSS") >= 0) {
                    sStylesheetFilename = sStylesheetFilename.Substring(0, sStylesheetFilename.Length - 4);
                }
                CConfig.Instance.StylesheetFilename = sStylesheetFilename;
            }

            // Validate and strip leading http:// in case user has it them on
            string sMainWebsiteLink = m_textboxConfigUserLink.Text;
            string sMainWebsiteLinkUpper = sMainWebsiteLink.ToUpper();

            if (sMainWebsiteLink.ToLower() == "http://") {
                // User hasn't altered default value
                sMainWebsiteLink = "";
            }

            CConfig.Instance.MainWebsiteLink = sMainWebsiteLink;
            CConfig.Instance.MiniTreeImageFormat = (m_comboboxConfigTreeDiagramsFormat.SelectedIndex == 1 ? "png" : "gif");
            CConfig.Instance.MultiPageIndexes = m_checkboxConfigMultiPageIndex.Checked;
            CConfig.Instance.IncludeUserRefInIndex = m_checkboxConfigUserRefInIndex.Checked;

            try {
                // Sanity check value
                uint uIndex = System.UInt32.Parse(m_textboxConfigMultiPageIndexNumber.Text);
                if (uIndex > 0) {
                    CConfig.Instance.IndividualsPerIndexPage = uIndex;
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            string sCustomFooter = m_textboxConfigCustomFooter.Text;
            CConfig.Instance.CustomFooter = sCustomFooter;

            CConfig.Instance.FooterIsHtml = m_checkboxConfigFooterIsHtml.Checked;
            CConfig.Instance.ConserveTreeWidth = m_checkboxConfigConserveTreeWidth.Checked;
            CConfig.Instance.KeepSiblingOrder = m_checkboxConfigKeepSiblingOrder.Checked;
            CConfig.Instance.AllowMultimedia = m_checkboxConfigAllowMultimedia.Checked;

            CConfig.Instance.MiniTreeColourBranch = Paintbox.ConvertColour(m_colorConfigMiniTreeBranch);
            CConfig.Instance.MiniTreeColourIndiBorder = Paintbox.ConvertColour(m_colorConfigMiniTreeIndiBorder);
            CConfig.Instance.MiniTreeColourIndiBackground = Paintbox.ConvertColour(m_colorConfigMiniTreeIndiBackground);
            CConfig.Instance.MiniTreeColourIndiHighlight = Paintbox.ConvertColour(m_colorConfigMiniTreeIndiHighlight);
            CConfig.Instance.MiniTreeColourIndiBgConcealed = Paintbox.ConvertColour(m_colorConfigMiniTreeIndiBgConcealed);
            CConfig.Instance.MiniTreeColourIndiFgConcealed = Paintbox.ConvertColour(m_colorConfigMiniTreeIndiFgConcealed);
            CConfig.Instance.MiniTreeColourIndiShade = Paintbox.ConvertColour(m_colorConfigMiniTreeIndiShade);
            CConfig.Instance.MiniTreeColourIndiText = Paintbox.ConvertColour(m_colorConfigMiniTreeIndiText);
            CConfig.Instance.MiniTreeColourIndiLink = Paintbox.ConvertColour(m_colorConfigMiniTreeIndiLink);
            CConfig.Instance.MiniTreeColourBackground = Paintbox.ConvertColour(m_colorConfigMiniTreeBackground);

            CConfig.Instance.SupressBackreferences = !m_checkboxConfigSupressBackreferences.Checked;
        }

        // Populates the list box of individuals to link from the front page
        private void FillKeyIndividualsList()
        {
            if (CConfig.Instance.KeyIndividuals == null) {
                return;
            }

            fLogger.WriteInfo("FillKeyIndividualsList() : " + CConfig.Instance.KeyIndividuals.Count.ToString());

            string sSurname;
            string sFirstName;
            string sFullName;

            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            m_listboxSelectKey.Items.Clear();

            if (CConfig.Instance.KeyIndividuals != null) {
                foreach (string xref in CConfig.Instance.KeyIndividuals) {
                    GEDCOMIndividualRecord irKey = fTree.XRefIndex_Find(xref) as GEDCOMIndividualRecord;
                    if (irKey != null && irKey.GetVisibility()) {
                        sFirstName = "";
                        sSurname = "";
                        sFullName = CConfig.Instance.CapitaliseName(irKey.GetPrimaryFullName(), ref sFirstName, ref sSurname);
                        if (sFullName == "") {
                            sFullName = CConfig.Instance.UnknownName;
                        }
                        m_listboxSelectKey.Items.Add(new NameXRefPair(sFullName, xref));
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
            fLogger.WriteInfo(string.Format("PrepareOutputDirectory({0})", sOutputFolder));

            string sMessage = "Could not access or create folder.";
            MessageBoxButtons messageBoxButtons = MessageBoxButtons.RetryCancel;
            DialogResult dialogResult = DialogResult.OK;
            bool bFailed = false;
            string sExceptionMessage = "";

            // First see if folder clashes with a file
            if (File.Exists(sOutputFolder)) {
                fLogger.WriteInfo("Folder clashes with file : " + sOutputFolder);

                // Earn user that file is being deleted
                dialogResult = MessageBox.Show(this, string.Format("The folder {0} needs to be created. " +
                    "\r\nThis will destroy an existing file with that name.", sOutputFolder),
                    "Creating website", MessageBoxButtons.OKCancel, MessageBoxIcon.Exclamation);
                if (dialogResult == DialogResult.Cancel) {
                    fLogger.WriteInfo("Message box cancelled (2)");
                    return DialogResult.Cancel;
                }

                // Now delete output folder (which is currently a file)
                do {
                    bFailed = false;
                    fLogger.WriteInfo("Deleting output folder as file : " + sOutputFolder);
                    try {
                        Cursor.Current = Cursors.WaitCursor;
                        Cursor.Show();
                        File.Delete(sOutputFolder);
                    } catch (IOException e) {
                        fLogger.WriteError("Caught IO exception : ", e);
                        sExceptionMessage = e.Message;
                        bFailed = true;
                    } catch (System.UnauthorizedAccessException e) {
                        fLogger.WriteError("Caught UnauthorizedAccessException(3) : ", e);
                        sExceptionMessage = e.Message;
                        bFailed = true;
                    } catch (Exception e) {
                        fLogger.WriteError("Caught generic exception(3) : ", e);
                        sExceptionMessage = e.Message;
                        bFailed = true;
                    }

                    if (bFailed) {
                        // Catch failure, e.g. if user has dir/file open elsewhere
                        sMessage = string.Format("The file {0} could not be deleted.\r\n{1}", sOutputFolder, sExceptionMessage);
                        messageBoxButtons = MessageBoxButtons.RetryCancel;
                        dialogResult = MessageBox.Show(this, sMessage, "Creating website", messageBoxButtons, MessageBoxIcon.Exclamation);
                    }
                }
                while (bFailed && dialogResult == DialogResult.Retry);
                if (bFailed && dialogResult == DialogResult.Cancel) {
                    fLogger.WriteInfo("Message box cancelled (6)");
                    return DialogResult.Cancel;
                }
            } // End if output folder exists

            // Next see if folder already exists
            // If output folder exists, offer to delete it
            if (Directory.Exists(sOutputFolder)) {
                fLogger.WriteInfo("Folder exists(11) : " + sOutputFolder);

                if (GMHelper.IsDesktop(sOutputFolder)) {
                    dialogResult = MessageBox.Show(this, "GEDmill will not allow you to create files directly on the Desktop",
                                                   "Creating website", MessageBoxButtons.OK, MessageBoxIcon.Stop);
                    fLogger.WriteInfo("Desktop detected as output folder.");
                    return DialogResult.Cancel;
                }

                // Warn user that file is being deleted
                dialogResult = MessageBox.Show(this, string.Format("The folder {0} already exists.\r\nWould you like to delete any files it contains before creating the website files?", sOutputFolder),
                    "Creating website",
                    MessageBoxButtons.YesNoCancel, MessageBoxIcon.Question);
                if (dialogResult == DialogResult.Cancel) {
                    fLogger.WriteInfo("Message box cancelled (3a)");
                    return DialogResult.Cancel;
                }
                if (dialogResult == DialogResult.Yes) {
                    if (bPreserveFiles) {
                        dialogResult = MessageBox.Show(this, string.Format("WARNING: Deleting the folder {0} will not preserve any existing front page and stylesheet files.\r\nDelete folder anyway?", sOutputFolder),
                            "Creating website",
                            MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation);
                        if (dialogResult == DialogResult.Cancel) {
                            fLogger.WriteInfo("Message box cancelled (3b)");
                            return DialogResult.Cancel;
                        }
                    } else {
                        dialogResult = MessageBox.Show(this, "WARNING: If the folder contains non-GEDmill files they will be deleted also.\r\nDelete folder anyway?",
                            "Creating website", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation);
                        if (dialogResult == DialogResult.Cancel) {
                            fLogger.WriteInfo("Message box cancelled (3c)");
                            return DialogResult.Cancel;
                        }
                    }
                    if (dialogResult == DialogResult.Yes) {
                        fLogger.WriteInfo("Message box yes. Deleting output folder.");
                        do {
                            bFailed = false;
                            try {
                                Cursor.Current = Cursors.WaitCursor;
                                Cursor.Show();
                                if (Directory.Exists(sOutputFolder)) {
                                    Directory.Delete(sOutputFolder, true);
                                }
                            } catch (IOException e) {
                                fLogger.WriteError("Caught IOException(2) : ", e);
                                sExceptionMessage = e.Message;
                                bFailed = true;
                            } catch (System.UnauthorizedAccessException e) {
                                fLogger.WriteError("Caught UnauthorizedAccessException(2) : ", e);
                                sExceptionMessage = e.Message;
                                bFailed = true;
                            } catch (Exception e) {
                                fLogger.WriteError("Caught generic exception(2) : ", e);
                                sExceptionMessage = e.Message;
                                bFailed = true;
                            }
                            if (bFailed) {
                                // Catch failure, e.g. if user has dir/file open elsewhere
                                sMessage = string.Format("The folder {0} could not be deleted.\r\n{1}", sOutputFolder, sExceptionMessage);
                                messageBoxButtons = MessageBoxButtons.RetryCancel;
                                dialogResult = MessageBox.Show(this, sMessage, "Creating website",
                                    messageBoxButtons, MessageBoxIcon.Exclamation);
                            }
                        }
                        while (bFailed && dialogResult == DialogResult.Retry);
                        if (bFailed && dialogResult == DialogResult.Cancel) {
                            return DialogResult.Cancel;
                        }
                    } // end "Yes" to not preserve files
                } // end "Yes to delete folder"
            } // end if output folder exists

            // At last, try to create the folder
            bFailed = false;
            DirectoryInfo directoryInfo = null;
            fLogger.WriteInfo("Creating output folder.");
            try {
                directoryInfo = Directory.CreateDirectory(sOutputFolder);
            }
            // Order of catches is important here, due to hierarchy of exception classes.
            catch (DirectoryNotFoundException e) {
                sMessage = "The folder you have selected could not be found.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                fLogger.WriteError("Caught DirectoryNotFoundException(5) : ", e);
                bFailed = true;
            } catch (ArgumentNullException e) {
                sMessage = "The folder name is missing or illegal.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                fLogger.WriteError("Caught ArgumentNullException(5) : ", e);
                bFailed = true;
            } catch (PathTooLongException e) {
                sMessage = "The folder name you have selected is too long.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                fLogger.WriteError("Caught PathTooLongException(5) : ", e);
                bFailed = true;
            } catch (IOException e) {
                sMessage = "The path you have selected is read-only, or the folder is not empty.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.RetryCancel; // Let them correct this outside of app
                fLogger.WriteError("Caught IOException(5) : ", e);
                bFailed = true;
            } catch (UnauthorizedAccessException e) {
                sMessage = "You do not have the correct permissions to access the folder.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.RetryCancel; // Let them correct this outside of app
                fLogger.WriteError("Caught UnauthorizedAccessException(5) : ", e);
                bFailed = true;
            } catch (ArgumentException e) {
                sMessage = "The folder name you have selected is of an illegal format.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                fLogger.WriteError("Caught ArgumentException(5) : ", e);
                bFailed = true;
            } catch (NotSupportedException e) {
                sMessage = "The folder name you have selected is of an unsupported format.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                fLogger.WriteError("Caught NotSupportedException(5) : ", e);
                bFailed = true;
            }

            // Handle any failure with a sMessage box
            if (bFailed) {
                dialogResult = MessageBox.Show(this, string.Concat(sMessage, "\r\n", sExceptionMessage), "Creating website", messageBoxButtons, MessageBoxIcon.Exclamation);

                if (dialogResult == DialogResult.Retry) {
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
            if (m_checkboxConfigMultiPageIndex.Checked) {
                m_labelConfigMultiPageIndexNumber.Enabled = true;
                m_textboxConfigMultiPageIndexNumber.Enabled = true;
            } else {
                m_labelConfigMultiPageIndexNumber.Enabled = false;
                m_textboxConfigMultiPageIndexNumber.Enabled = false;
            }
        }

        // Logic to enable the controls related to multimedia content
        private void EnableMultimediaConfig()
        {
            if (m_checkboxConfigAllowMultimedia.Checked) {
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
            } else {
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
            if (m_checkboxConfigIndiImages.Checked) {
                m_labelConfigThumbnailImageSize.Enabled = true;
                m_labelConfigThumbnailImageWidth.Enabled = true;
                m_textboxConfigThumbnailImageWidth.Enabled = true;
                m_labelConfigThumbnailImageHeight.Enabled = true;
                m_textboxConfigThumbnailImageHeight.Enabled = true;
            } else {
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
            if (m_checkboxConfigShowWithheldRecords.Checked) {
                m_groupboxConfigWithheldName.Enabled = true;
                m_radiobuttonConfigWithheldNameLabel.Enabled = true;
                m_textboxConfigWithheldName.Enabled = m_radiobuttonConfigWithheldNameLabel.Checked;
                m_radiobuttonConfigWithheldNameName.Enabled = true;
            } else {
                m_groupboxConfigWithheldName.Enabled = false;
                m_radiobuttonConfigWithheldNameLabel.Enabled = false;
                m_textboxConfigWithheldName.Enabled = false;
                m_radiobuttonConfigWithheldNameName.Enabled = false;
            }
        }

        // Logic to enable the save changes button
        private void EnablePrunePanelButtons()
        {
        }
    }
}
