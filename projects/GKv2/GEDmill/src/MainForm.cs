/* 
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
 */

using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using GDModel;
using GEDmill.HTML;
using GEDmill.ListView;
using GEDmill.Model;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Logging;
using GKCore.Types;
using GKUI.Forms;

namespace GEDmill
{
    public enum PanelKind
    {
        Welcome,
        RestrictRecords,
        WebsiteTitleAndKeys,
        OutputFolder,
        Finish
    }

    /// <summary>
    /// The main from from which the application is operated. Contains the GUI controls and the control handlers.
    /// </summary>
    public partial class MainForm : CommonForm, ILocalizable
    {
        private static readonly GKCore.Logging.ILogger fLogger = LogManager.GetLogger(GMConfig.LOG_FILE, GMConfig.LOG_LEVEL, typeof(MainForm).Name);

        private readonly ILangMan fLangMan;
        private readonly Plugin fPlugin;
        private IBaseWindow fBase;

        // Specifies which panel of the wizard the user is viewing (i.e. which stage in the app they are at)
        private PanelKind fCurrentPanel;

        // Application has an important state whereby it displays the settings panes. 
        // The main GUI navigation buttons behave differently in this mode.
        private bool fConfigPanelVisible;

        // Scales the size of the main GUI
        private Point fDefaultButtonSize;

        // Scales the size of the config panels GUI
        private Point fConfigButtonSize;

        // Check event gets called when program builds the list. Don't want to enable buttons in that case.
        private bool fDisableRestrictsCheckEvent;

        // When user redefines the mini tree colours, these hold the new colours until they click OK.
        private Color fColorConfigMiniTreeBranch;
        private Color fColorConfigMiniTreeIndiBorder;
        private Color fColorConfigMiniTreeIndiBackground;
        private Color fColorConfigMiniTreeIndiHighlight;
        private Color fColorConfigMiniTreeIndiBgConcealed;
        private Color fColorConfigMiniTreeIndiFgConcealed;
        private Color fColorConfigMiniTreeIndiShade;
        private Color fColorConfigMiniTreeIndiText;
        private Color fColorConfigMiniTreeIndiLink;
        private Color fColorConfigMiniTreeBackground;


        // Constructor. Initialise and create GUI.
        public MainForm()
        {
            InitializeComponent();

            fLogger.WriteInfo(GMConfig.SoftwareName + " " + GMConfig.SoftwareVersion + " started");

            // Set some values that scale the size of the GUI
            fDefaultButtonSize = new Point(75, 23);
            fConfigButtonSize = new Point(92, 23);

            helpProvider.SetHelpKeyword(btnHelp, "HelpButtonHelpKeyword");
            helpProvider.SetHelpNavigator(btnHelp, HelpNavigator.TableOfContents);
            helpProvider.SetShowHelp(btnHelp, true);
            helpProvider.HelpNamespace = GMHelper.GetAppPath() + "\\" + GMConfig.HelpFilename;

            fCurrentPanel = PanelKind.Welcome;
            fConfigPanelVisible = false;
            fDisableRestrictsCheckEvent = false;
        }

        public MainForm(Plugin plugin) : this()
        {
            fPlugin = plugin;
            fLangMan = plugin.LangMan;
            SetLocale();
            ShowCurrentPanel();

            GMConfig.Instance.Reset(fLangMan);

            // Read back any previously stored settings.
            GMConfig.Instance.Load();
        }

        // Clean up any resources being used.
        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null) {
                    components.Dispose();
                }
                fPlugin.CloseForm();
            }
            base.Dispose(disposing);
        }

        private void MainForm_Load(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetShow(fPlugin);
            BaseChanged(fPlugin.Host.GetCurrentFile());
        }

        private void MainForm_Closed(object sender, EventArgs e)
        {
            BaseChanged(null);
            fPlugin.Host.WidgetClose(fPlugin);
        }

        public void BaseChanged(IBaseWindow baseWin)
        {
            if (fBase != baseWin) {
                fBase = baseWin;
                //UpdateForm();
            }
        }

        #region ILocalizable support

        public void SetLocale()
        {
            Text = fLangMan.LS(PLS.LSID_Title);
            btnCancel.Text = fLangMan.LS(PLS.LSID_Quit);
            btnBack.Text = fLangMan.LS(PLS.LSID_Back);
            btnNext.Text = fLangMan.LS(PLS.LSID_Next);
            btnHelp.Text = fLangMan.LS(PLS.LSID_Help);
            btnSettings.Text = fLangMan.LS(PLS.LSID_Settings);
            btnSettingsCancel.Text = fLangMan.LS(PLS.LSID_Cancel);

            pageSettingsWebpages.Text = fLangMan.LS(PLS.LSID_Webpages);
            pageSettingsImages.Text = fLangMan.LS(PLS.LSID_Images);
            pageSettingsGedcom.Text = "GEDCOM";
            pageSettingsTreeDiagrams.Text = fLangMan.LS(PLS.LSID_TreeDiagrams);
            pageSettingsAdvanced.Text = fLangMan.LS(PLS.LSID_Advanced);
            pageIndividuals.Text = fLangMan.LS(PLS.LSID_Individuals);
            pageSources.Text = fLangMan.LS(PLS.LSID_Sources);

            lblWelcomeVersion.Text = fLangMan.LS(PLS.LSID_Version) + " " + GMConfig.SoftwareVersion;
            lblWelcomeSubtitle.Text = fLangMan.LS(PLS.LSID_WelcomeSubtitle);
            lblRecordsContinue.Text = fLangMan.LS(PLS.LSID_RecordsContinue);
            miIndiDescendantsExclude.Text = fLangMan.LS(PLS.LSID_IndiDescendantsExclude);
            miIndiAncestorsExclude.Text = fLangMan.LS(PLS.LSID_IndiAncestorsExclude);
            miIndiDescendantsInclude.Text = fLangMan.LS(PLS.LSID_IndiDescendantsInclude);
            miIndiAncestorsInclude.Text = fLangMan.LS(PLS.LSID_IndiAncestorsInclude);
            miUnconnectedExclude.Text = fLangMan.LS(PLS.LSID_UnconnectedExclude);
            miIndividualDetails.Text = fLangMan.LS(PLS.LSID_Details);
            miIndividualsEveryoneInclude.Text = fLangMan.LS(PLS.LSID_IndividualsEveryoneInclude);
            miIndividualsEveryoneExclude.Text = fLangMan.LS(PLS.LSID_IndividualsEveryoneExclude);
            miIndividualsAliveExclude.Text = fLangMan.LS(PLS.LSID_IndividualsAliveExclude);
            miSourceDetails.Text = fLangMan.LS(PLS.LSID_Details);
            miSourceRemovePics.Text = fLangMan.LS(PLS.LSID_SourceRemovePics);
            miSourcesAllInclude.Text = fLangMan.LS(PLS.LSID_SourcesAllInclude);
            miSourcesAllExclude.Text = fLangMan.LS(PLS.LSID_SourcesAllExclude);
            lblPruneRecordsInstructions.Text = fLangMan.LS(PLS.LSID_PruneRecordsInstructions);
            lblPruneRecordsButtons.Text = fLangMan.LS(PLS.LSID_PruneRecordsButtons);
            lblSelectKey.Text = fLangMan.LS(PLS.LSID_SelectKey);
            lblSelectKeyIndividuals.Text = fLangMan.LS(PLS.LSID_SelectKeyIndividuals);
            btnSelectKeyAdd.Text = fLangMan.LS(PLS.LSID_SelectKeyAdd);
            btnSelectKeyDelete.Text = fLangMan.LS(PLS.LSID_SelectKeyDelete);
            lblSelectKeyInstructions.Text = fLangMan.LS(PLS.LSID_SelectKeyInstructions);
            lblConfigCommentary.Text = fLangMan.LS(PLS.LSID_ConfigCommentary);
            chkConfigCommentaryIsHtml.Text = fLangMan.LS(PLS.LSID_IsHtml);
            lblConfigUserLink.Text = fLangMan.LS(PLS.LSID_ConfigUserLink);
            lblConfigCustomFooter.Text = fLangMan.LS(PLS.LSID_ConfigCustomFooter);
            chkConfigFooterIsHtml.Text = fLangMan.LS(PLS.LSID_IsHtml);
            chkConfigStats.Text = fLangMan.LS(PLS.LSID_ConfigStats);
            chkConfigMultiPageIndex.Text = fLangMan.LS(PLS.LSID_ConfigMultiPageIndex);
            chkConfigUserRefInIndex.Text = fLangMan.LS(PLS.LSID_ConfigUserRefInIndex);
            lblConfigMultiPageIndexNumber.Text = fLangMan.LS(PLS.LSID_ConfigMultiPageIndexNumber);
            lblConfigIndexName.Text = fLangMan.LS(PLS.LSID_ConfigIndexName);
            lblConfigEmail.Text = fLangMan.LS(PLS.LSID_ConfigEmail);
            lblConfigBackImageEdit.Text = fLangMan.LS(PLS.LSID_ConfigBackImageEdit);
            btnConfigBackImageBrowse.Text = fLangMan.LS(PLS.LSID_Browse);
            lblConfigFrontImageEdit.Text = fLangMan.LS(PLS.LSID_ConfigFrontImageEdit);
            btnConfigFrontImageBrowse.Text = fLangMan.LS(PLS.LSID_Browse);
            lblConfigIndiImageSize.Text = fLangMan.LS(PLS.LSID_ConfigIndiImageSize);
            lblConfigIndiImageWidth.Text = fLangMan.LS(PLS.LSID_Width);
            lblConfigIndiImageHeight.Text = fLangMan.LS(PLS.LSID_Height);
            lblConfigSourceImageSize.Text = fLangMan.LS(PLS.LSID_ConfigSourceImageSize);
            lblConfigSourceImageWidth.Text = fLangMan.LS(PLS.LSID_Width);
            lblConfigSourceImageHeight.Text = fLangMan.LS(PLS.LSID_Height);
            chkConfigAllowMultimedia.Text = fLangMan.LS(PLS.LSID_ConfigAllowMultimedia);
            chkConfigRenameOriginals.Text = fLangMan.LS(PLS.LSID_ConfigRenameOriginals);
            chkConfigKeepOriginals.Text = fLangMan.LS(PLS.LSID_ConfigKeepOriginals);
            chkConfigNonPictures.Text = fLangMan.LS(PLS.LSID_ConfigNonPictures);
            chkConfigIndiImages.Text = fLangMan.LS(PLS.LSID_ConfigIndiImages);
            lblConfigThumbnailImageSize.Text = fLangMan.LS(PLS.LSID_ConfigThumbnailImageSize);
            lblConfigThumbnailImageWidth.Text = fLangMan.LS(PLS.LSID_Width);
            lblConfigThumbnailImageHeight.Text = fLangMan.LS(PLS.LSID_Height);
            lblConfigTabSpaces.Text = fLangMan.LS(PLS.LSID_ConfigTabSpaces);
            lblConfigNoName.Text = fLangMan.LS(PLS.LSID_ConfigNoName);
            chkConfigShowWithheldRecords.Text = fLangMan.LS(PLS.LSID_ConfigShowWithheldRecords);
            gbConfigWithheldName.Text = fLangMan.LS(PLS.LSID_ConfigWithheldName);
            radConfigWithheldNameLabel.Text = fLangMan.LS(PLS.LSID_ConfigWithheldNameLabel);
            radConfigWithheldNameName.Text = fLangMan.LS(PLS.LSID_ConfigWithheldNameName);
            chkConfigCapSurnames.Text = fLangMan.LS(PLS.LSID_ConfigCapSurnames);
            chkConfigCapEvents.Text = fLangMan.LS(PLS.LSID_ConfigCapEvents);
            chkConfigHideEmails.Text = fLangMan.LS(PLS.LSID_ConfigHideEmails);
            chkConfigOccupationHeadline.Text = fLangMan.LS(PLS.LSID_ConfigOccupationHeadline);
            chkConfigIncludeTreeDiagrams.Text = fLangMan.LS(PLS.LSID_ConfigIncludeTreeDiagrams);
            chkConfigTreeDiagramsFakeBg.Text = fLangMan.LS(PLS.LSID_ConfigTreeDiagramsFakeBg);
            chkConfigConserveTreeWidth.Text = fLangMan.LS(PLS.LSID_ConfigConserveTreeWidth);
            chkConfigKeepSiblingOrder.Text = fLangMan.LS(PLS.LSID_ConfigKeepSiblingOrder);
            gbMiniTreeColours.Text = fLangMan.LS(PLS.LSID_MiniTreeColours);
            btnConfigMiniTreeColourIndiHighlight.Text = fLangMan.LS(PLS.LSID_ConfigMiniTreeColourIndiHighlight);
            btnConfigMiniTreeColourIndiText.Text = fLangMan.LS(PLS.LSID_ConfigMiniTreeColourIndiText);
            btnConfigMiniTreeColourIndiBackground.Text = fLangMan.LS(PLS.LSID_ConfigMiniTreeColourIndiBackground);
            btnConfigMiniTreeColourIndiLink.Text = fLangMan.LS(PLS.LSID_ConfigMiniTreeColourIndiLink);
            btnConfigMiniTreeColourIndiBgConcealed.Text = fLangMan.LS(PLS.LSID_ConfigMiniTreeColourIndiBgConcealed);
            btnConfigMiniTreeColourIndiFgConcealed.Text = fLangMan.LS(PLS.LSID_ConfigMiniTreeColourIndiFgConcealed);
            btnConfigMiniTreeColourIndiShade.Text = fLangMan.LS(PLS.LSID_ConfigMiniTreeColourIndiShade);
            btnConfigMiniTreeColourBranch.Text = fLangMan.LS(PLS.LSID_ConfigMiniTreeColourBranch);
            btnConfigMiniTreeColourIndiBorder.Text = fLangMan.LS(PLS.LSID_ConfigMiniTreeColourIndiBorder);
            chkConfigUserRecFilename.Text = fLangMan.LS(PLS.LSID_ConfigUserRecFilename);
            chkConfigSupressBackreferences.Text = fLangMan.LS(PLS.LSID_ConfigSupressBackreferences);
            lblChooseOutput.Text = fLangMan.LS(PLS.LSID_ChooseOutput);
            btnChooseOutputBrowse.Text = fLangMan.LS(PLS.LSID_Browse);
            lblChooseOutputInstructions.Text = fLangMan.LS(PLS.LSID_ChooseOutputInstructions);
            lblChooseOutputContinue.Text = fLangMan.LS(PLS.LSID_ChooseOutputContinue);
            chkAllDoneShowSite.Text = fLangMan.LS(PLS.LSID_AllDoneShowSite);
            lblAllDone.Text = fLangMan.LS(PLS.LSID_AllDone);
            lblAllDoneThankYou.Text = fLangMan.LS(PLS.LSID_AllDoneThankYou);
            lblAllDoneDirectory.Text = fLangMan.LS(PLS.LSID_AllDoneDirectory);
        }

        #endregion

        #region Event handlers

        private void btnBack_Click(object sender, EventArgs e)
        {
            // Mustn't affect configPanel 
            if (fConfigPanelVisible) {
                return;
            }

            if (fCurrentPanel > PanelKind.Welcome) {
                --fCurrentPanel;
            }

            EnableCurrentPanel(true);
            ShowCurrentPanel();
        }

        private void btnNext_Click(object sender, EventArgs e)
        {
            fLogger.WriteInfo("Next button clicked. Current panel = " + fCurrentPanel.ToString());

            // Mustn't affect configPanel
            if (fConfigPanelVisible) {
                return;
            }

            // Disable edit boxes etc. on current panel to avoid confusing users
            EnableCurrentPanel(false);

            // Disable buttons while progress dialogs etc are shown (they may appear to some
            // users as being part of the progress dialog.)
            btnNext.Enabled = false;
            btnBack.Enabled = false;
            btnCancel.Enabled = false;
            btnSettings.Enabled = false;
            btnHelp.Enabled = false;

            if (ValidateCurrentPanel()) {
                if ((int)fCurrentPanel < 9) // Allow for extra ftp panels
                    ++fCurrentPanel;
                else {
                    GMConfig.Instance.Save();

                    if (GMConfig.Instance.OpenWebsiteOnExit) {
                        GMHelper.OpenURL(GMConfig.Instance.FrontPageURL);
                    }
                    Close();
                    return;
                }

                InitialiseCurrentPanel();
                ShowCurrentPanel();
            } else {
                // Restore next button state (it must have been enabled for this function to have been called)
                btnNext.Enabled = true;
            }

            // Back button and quit button are always enabled
            btnBack.Enabled = true;
            btnCancel.Enabled = true;
            btnSettings.Enabled = true;
            btnHelp.Enabled = true;

            EnableCurrentPanel(true);
        }

        private bool ShowQuestionYN(string msg, string title = GMConfig.SoftwareName)
        {
            return AppHost.StdDialogs.ShowQuestionYN(msg, title);
        }

        private void ShowAlert(string msg, string title = GMConfig.SoftwareName)
        {
            AppHost.StdDialogs.ShowAlert(msg, title);
        }

        private void ShowMessage(string msg, string title = GMConfig.SoftwareName)
        {
            AppHost.StdDialogs.ShowMessage(msg, title);
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            bool dialogResult = true;

            if (fCurrentPanel != PanelKind.Finish) {
                // Cancel button is "Finish" in panel 6
                dialogResult = ShowQuestionYN(fLangMan.LS(PLS.LSID_ExitQuestion));
            }

            if (dialogResult) {
                if (fCurrentPanel >= PanelKind.Finish) {
                    // HTML generated, so save settings for that at least
                    // Store checkbox state in config.(Can't do in ValidateCurrentPanel because Next never gets clicked)
                    GMConfig.Instance.OpenWebsiteOnExit = chkAllDoneShowSite.Checked;
                }

                if (fCurrentPanel == PanelKind.Finish) {
                    // Finish button is the only time we want to launch webpages
                    if (GMConfig.Instance.OpenWebsiteOnExit && GMConfig.Instance.FrontPageFilename.Length > 0) {
                        GMHelper.OpenURL(GMConfig.Instance.FrontPageURL);
                    }
                }

                GMConfig.Instance.Save();
                Close();
            }
        }

        private void btnHelp_Click(object sender, EventArgs e)
        {
            string helpFile = GMHelper.GetAppPath() + "\\" + GMConfig.HelpFilename;

            if (fConfigPanelVisible) {
                switch (tabcontrolConfigPanel.SelectedIndex) {
                    case 0:
                        Help.ShowHelp(btnHelp, helpFile, HelpNavigator.Topic, "SettingsWebpages.htm");
                        break;
                    case 1:
                        Help.ShowHelp(btnHelp, helpFile, HelpNavigator.Topic, "SettingsImages.htm");
                        break;
                    case 2:
                        Help.ShowHelp(btnHelp, helpFile, HelpNavigator.Topic, "SettingsGEDCOM.htm");
                        break;
                    case 3:
                        Help.ShowHelp(btnHelp, helpFile, HelpNavigator.Topic, "SettingsTrees.htm");
                        break;
                    case 4:
                        Help.ShowHelp(btnHelp, helpFile, HelpNavigator.Topic, "SettingsAdvanced.htm");
                        break;
                    default:
                        Help.ShowHelp(btnHelp, helpFile, HelpNavigator.Topic, "FrontPage.htm");
                        break;
                }
            } else {
                switch (fCurrentPanel) {
                    case PanelKind.RestrictRecords:
                        Help.ShowHelp(btnHelp, helpFile, HelpNavigator.Topic, "ExcludingPeople_2.htm");
                        break;
                    case PanelKind.WebsiteTitleAndKeys:
                        Help.ShowHelp(btnHelp, helpFile, HelpNavigator.Topic, "SetTheTitle_3.htm");
                        break;
                    case PanelKind.OutputFolder:
                        Help.ShowHelp(btnHelp, helpFile, HelpNavigator.Topic, "SelectingOutputFile_4.htm");
                        break;
                    case PanelKind.Finish:
                        Help.ShowHelp(btnHelp, helpFile, HelpNavigator.Topic, "FinishScreen_5.htm");
                        break;
                    default:
                        Help.ShowHelp(btnHelp, helpFile, HelpNavigator.Topic, "FrontPage.htm");
                        break;
                }
            }
        }

        private void btnSettings_Click(object sender, EventArgs e)
        {
            if (!fConfigPanelVisible) {
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

        private void btnSettingsCancel_Click(object sender, EventArgs e)
        {
            // Ensure config panel on
            if (!fConfigPanelVisible) return;

            // Remove panel without saving changes
            SwitchConfigPanelOff();
        }

        private void txtChooseOutput_TextChanged(object sender, EventArgs e)
        {
            EnableNextButton();
        }

        private void lstKeyIndividuals_SelectedValueChanged(object sender, EventArgs e)
        {
            EnableKeyIndividualsDeleteButton();
        }

        private void btnSelectKeyAdd_Click(object sender, EventArgs e)
        {
            // Use a dialog box to let them choose an individual
            GDMIndividualRecord indiRec = fBase.Context.SelectPerson(this, null, TargetMode.tmNone, GDMSex.svUnknown);
            if (indiRec == null) return;

            // Ensure they are only added once
            bool alreadyAdded = GMConfig.Instance.KeyIndividuals.Contains(indiRec.XRef);
            if (!alreadyAdded) {
                GMConfig.Instance.KeyIndividuals.Add(indiRec.XRef);
                FillKeyIndividualsList();
            }
        }

        private void btnSelectKeyDelete_Click(object sender, EventArgs e)
        {
            NameXRefPair xrefPairName = lstKeyIndividuals.SelectedItem as NameXRefPair;
            if (xrefPairName != null) {
                string xref = xrefPairName.XRef;
                if (!string.IsNullOrEmpty(xref)) {
                    GMConfig.Instance.KeyIndividuals.Remove(xref);
                }
                FillKeyIndividualsList();
            }
        }

        private void btnChooseOutputBrowse_Click(object sender, EventArgs e)
        {
            FolderBrowserDialog folderBrowserDialog1 = new FolderBrowserDialog();
            if (Directory.Exists(txtChooseOutput.Text)) {
                folderBrowserDialog1.SelectedPath = txtChooseOutput.Text;
            } else {
                string sPath = txtChooseOutput.Text;
                int nLastFolder = sPath.LastIndexOf('\\'); // Try folder above
                if (nLastFolder >= 0) {
                    sPath = sPath.Substring(0, nLastFolder);
                }
                if (Directory.Exists(sPath)) {
                    folderBrowserDialog1.SelectedPath = sPath;
                } else {
                    folderBrowserDialog1.SelectedPath = Environment.GetFolderPath(Environment.SpecialFolder.Personal) + "\\GEDmill_Output";
                }
            }
            if (folderBrowserDialog1.ShowDialog() == DialogResult.OK) {
                txtChooseOutput.Text = folderBrowserDialog1.SelectedPath;
                txtChooseOutput.SelectionStart = txtChooseOutput.Text.Length;
                txtChooseOutput.SelectionLength = txtChooseOutput.Text.Length;
            }
        }

        private void lblAllDone_Click(object sender, LinkLabelLinkClickedEventArgs e)
        {
            bool oldVisitedValue = lblAllDone.Links[lblAllDone.Links.IndexOf(e.Link)].Visited;
            try {
                lblAllDone.Links[lblAllDone.Links.IndexOf(e.Link)].Visited = true;
                string url = lblAllDone.Text;
                System.Diagnostics.Process.Start(url);
            } catch (Exception e2) {
                fLogger.WriteError("Caught exception while viewing folder : {0}", e2);
                lblAllDone.Links[lblAllDone.Links.IndexOf(e.Link)].Visited = oldVisitedValue;
            }
        }

        private void btnConfigBackImageBrowse_Click(object sender, EventArgs e)
        {
            string sPath = GMHelper.GetInitialDirectory(txtConfigBackImageEdit.Text);
            //openFileDialog.FileName = txtConfigBackImageEdit.Text;
            string backFile = AppHost.StdDialogs.GetOpenFile(fLangMan.LS(PLS.LSID_SelectBackImage), sPath, GMHelper.GfxFilter, 1, "");
            if (!string.IsNullOrEmpty(backFile) && IsSupportedFile(backFile)) {
                txtConfigBackImageEdit.Text = backFile;
                txtConfigBackImageEdit.SelectAll();
            }
        }

        private void btnConfigFrontImageBrowse_Click(object sender, EventArgs e)
        {
            string sPath = GMHelper.GetInitialDirectory(txtConfigFrontImageEdit.Text);
            string frontFile = AppHost.StdDialogs.GetOpenFile(fLangMan.LS(PLS.LSID_SelectFrontImage), sPath, GMHelper.GfxFilter, 1, "");
            if (!string.IsNullOrEmpty(frontFile) && IsSupportedFile(frontFile)) {
                txtConfigFrontImageEdit.Text = frontFile;
                txtConfigFrontImageEdit.SelectAll();
            }
        }

        private bool IsSupportedFile(string fileName)
        {
            if (!string.IsNullOrEmpty(fileName)) {
                string exten = Path.GetExtension(fileName).ToLower();
                if (exten != ".jpg" && exten != ".jpeg" && exten != ".png" && exten != ".gif" && exten != ".bmp") {
                    ShowAlert(fLangMan.LS(PLS.LSID_NotSupportedPictureType));
                    return false;
                }
            }
            return true;
        }

        private void chkConfigIncludeTreeDiagrams_Click(object sender, EventArgs e)
        {
            EnableMiniTreeButtons();
        }

        private void btnConfigMiniTreeColourIndiBackground_Click(object sender, EventArgs e)
        {
            fColorConfigMiniTreeIndiBackground = GMHelper.SelectColor(fColorConfigMiniTreeIndiBackground);
            SetMiniTreeColourConfigButtons();
        }

        private void btnConfigMiniTreeColourIndiHighlight_Click(object sender, EventArgs e)
        {
            fColorConfigMiniTreeIndiHighlight = GMHelper.SelectColor(fColorConfigMiniTreeIndiHighlight);
            SetMiniTreeColourConfigButtons();
        }

        private void btnConfigMiniTreeColourIndiBgConcealed_Click(object sender, EventArgs e)
        {
            fColorConfigMiniTreeIndiBgConcealed = GMHelper.SelectColor(fColorConfigMiniTreeIndiBgConcealed);
            SetMiniTreeColourConfigButtons();
        }

        private void btnConfigMiniTreeColourIndiShade_Click(object sender, EventArgs e)
        {
            fColorConfigMiniTreeIndiShade = GMHelper.SelectColor(fColorConfigMiniTreeIndiShade);
            SetMiniTreeColourConfigButtons();
        }

        private void btnConfigMiniTreeColourIndiText_Click(object sender, EventArgs e)
        {
            fColorConfigMiniTreeIndiText = GMHelper.SelectColor(fColorConfigMiniTreeIndiText);
            SetMiniTreeColourConfigButtons();
        }

        private void btnConfigMiniTreeColourIndiLink_Click(object sender, EventArgs e)
        {
            fColorConfigMiniTreeIndiLink = GMHelper.SelectColor(fColorConfigMiniTreeIndiLink);
            SetMiniTreeColourConfigButtons();
        }

        private void btnConfigMiniTreeColourBranch_Click(object sender, EventArgs e)
        {
            fColorConfigMiniTreeBranch = GMHelper.SelectColor(fColorConfigMiniTreeBranch);
            SetMiniTreeColourConfigButtons();
        }

        private void btnConfigMiniTreeColourIndiBorder_Click(object sender, EventArgs e)
        {
            fColorConfigMiniTreeIndiBorder = GMHelper.SelectColor(fColorConfigMiniTreeIndiBorder);
            SetMiniTreeColourConfigButtons();
        }

        private void btnConfigMiniTreeColourIndiFgConcealed_Click(object sender, EventArgs e)
        {
            fColorConfigMiniTreeIndiFgConcealed = GMHelper.SelectColor(fColorConfigMiniTreeIndiFgConcealed);
            SetMiniTreeColourConfigButtons();
        }

        private void chkConfigMultiPageIndex_Click(object sender, EventArgs e)
        {
            EnableMultiPageIndexConfig();
        }

        private void chkConfigAllowMultimedia_Click(object sender, EventArgs e)
        {
            EnableMultimediaConfig();
        }

        private void chkConfigIndiImages_Click(object sender, EventArgs e)
        {
            EnableThumbnailsConfig();
        }

        private void chkConfigShowWithheldRecords_Click(object sender, EventArgs e)
        {
            EnableWithheldConfig();
        }

        private void radConfigWithheldNameLabel_Click(object sender, EventArgs e)
        {
            EnableWithheldConfig();
        }

        private void miIndividualDetails_Click(Object sender, EventArgs e)
        {
            if (lvIndividuals.SelectedItems.Count == 1) {
                var lvi = lvIndividuals.SelectedItems[0] as LVItem;
                var ir = lvi.Record as GDMIndividualRecord;
                BaseController.ViewRecordInfo(this, fBase, ir);
            }
        }

        private void miSourceDetails_Click(Object sender, EventArgs e)
        {
            if (lvSources.SelectedItems.Count == 1) {
                var lvi = lvSources.SelectedItems[0] as LVItem;
                var sr = lvi.Record as GDMSourceRecord;
                BaseController.ViewRecordInfo(this, fBase, sr);
            }
        }

        private void miUnconnectedExclude_Click(Object sender, EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            int recordsExcluded = 0;
            int recordsIncluded = 0;

            try {
                var marks = new List<GDMRecord>();
                // exclude all individuals unless connected in any way to this person through non-excluded people
                foreach (ListViewItem lvi in lvIndividuals.SelectedItems) {
                    if (lvi is LVItem) {
                        var ir = ((LVItem)lvi).Record as GDMIndividualRecord;
                        if (ir != null) {
                            // First mark as visited all possible relations of irSubject, not following restricted people
                            // Adds to visited list
                            GMHelper.MarkConnected(fBase.Context.Tree, ir, marks);
                        }
                    }
                }
                // Then exclude all unmarked individuals (i.e. not in visited list)
                GMHelper.RestrictUnmarked(fBase.Context.Tree, marks, out recordsExcluded);
            } catch (Exception ex) {
                ReportRestrictError(ex);
            }

            // Rebuild list
            FillIndividualsList();

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowRestrictsResult(recordsExcluded, recordsIncluded, fLangMan.LS(PLS.LSID_individuals));
        }

        private void miIndiDescendantsExclude_Click(Object sender, EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            int recordsExcluded = 0;
            int recordsIncluded = 0;

            try {
                // exclude all individuals descended from this person, including spouses and spouses ancestors. 
                if (lvIndividuals.SelectedItems.Count == 1) {
                    var lvi = lvIndividuals.SelectedItems[0] as LVItem;
                    if (lvi != null) {
                        GDMIndividualRecord ir = lvi.Record as GDMIndividualRecord;
                        if (ir != null) {
                            GMHelper.RestrictDescendants(fBase.Context.Tree, ir, false);
                        }
                    }
                }
            } catch (Exception ex) {
                ReportRestrictError(ex);
            }

            // Rebuild list
            FillIndividualsList();

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowRestrictsResult(recordsExcluded, recordsIncluded, fLangMan.LS(PLS.LSID_individuals));
        }

        private void miIndiDescendantsInclude_Click(Object sender, EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            int recordsExcluded = 0;
            int recordsIncluded = 0;

            try {
                // exclude all individuals descended from this person, including spouses and spouses ancestors. 
                if (lvIndividuals.SelectedItems.Count == 1) {
                    var lvi = lvIndividuals.SelectedItems[0] as LVItem;
                    if (lvi != null) {
                        GDMIndividualRecord ir = lvi.Record as GDMIndividualRecord;
                        if (ir != null) {
                            GMHelper.RestrictDescendants(fBase.Context.Tree, ir, true);
                        }
                    }
                }
            } catch (Exception ex) {
                ReportRestrictError(ex);
            }

            // Rebuild lists
            FillIndividualsList();

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowRestrictsResult(recordsExcluded, recordsIncluded, fLangMan.LS(PLS.LSID_individuals));
        }

        private void miIndiAncestorsExclude_Click(Object sender, EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            int recordsExcluded = 0;
            int recordsIncluded = 0;

            try {
                // Exclude all individuals descended from this person, including spouses and spouses ancestors. 
                if (lvIndividuals.SelectedItems.Count == 1) {
                    var lvi = lvIndividuals.SelectedItems[0] as LVItem;
                    if (lvi != null) {
                        var ir = lvi.Record as GDMIndividualRecord;
                        if (ir != null) {
                            GMHelper.RestrictAncestors(fBase.Context.Tree, ir, false);
                        }
                    }
                }
            } catch (Exception ex) {
                ReportRestrictError(ex);
            }

            // Rebuild lists
            FillIndividualsList();

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowRestrictsResult(recordsExcluded, recordsIncluded, fLangMan.LS(PLS.LSID_individuals));
        }

        private void miIndiAncestorsInclude_Click(Object sender, EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            int recordsExcluded = 0;
            int recordsIncluded = 0;

            try {
                // Exclude all individuals descended from this person, including spouses and spouses ancestors. 
                if (lvIndividuals.SelectedItems.Count == 1) {
                    var lvi = lvIndividuals.SelectedItems[0] as LVItem;
                    if (lvi != null) {
                        var ir = lvi.Record as GDMIndividualRecord;
                        if (ir != null) {
                            GMHelper.RestrictAncestors(fBase.Context.Tree, ir, true);
                        }
                    }
                }
            } catch (Exception ex) {
                ReportRestrictError(ex);
            }

            // Rebuild lists
            FillIndividualsList();

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowRestrictsResult(recordsExcluded, recordsIncluded, fLangMan.LS(PLS.LSID_individuals));
        }

        private void miIndividualsEveryoneInclude_Click(Object sender, EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            int recordsIncluded = 0;
            foreach (ListViewItem lvi in lvIndividuals.Items) {
                if (lvi is LVItem) {
                    GDMIndividualRecord ir = (GDMIndividualRecord)((LVItem)lvi).Record;
                    if (!GMHelper.GetVisibility(ir)) {
                        ++recordsIncluded;
                        GMHelper.SetVisibility(ir, true);
                    }
                }
            }

            // Rebuild lists
            FillIndividualsList();

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowRestrictsResult(0, recordsIncluded, fLangMan.LS(PLS.LSID_individuals));
        }

        // Removes pictures from the selected source
        private void miSourceRemovePics_Click(Object sender, EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            int nHidden = 0;
            foreach (ListViewItem lvi in lvSources.SelectedItems) {
                if (lvi is LVItem) {
                    GDMSourceRecord sr = (GDMSourceRecord)((LVItem)lvi).Record;
                    if (sr != null) {
                        int nHiddenThisTime = GMHelper.SetAllMFRsVisible(fBase.Context.Tree, sr, false);
                        nHidden += nHiddenThisTime;
                        if (nHiddenThisTime > 0) {
                            SetSourceSubItems((LVItem)lvi, sr, true); // Updates list
                        }
                    }
                }
            }

            // Rebuild lists
            FillSourcesList();

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowHidePicsResult(nHidden);
            EnablePrunePanelButtons();
        }

        private void miIndividualsEveryoneExclude_Click(Object sender, EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            int recordsExcluded = 0;

            foreach (ListViewItem lvi in lvIndividuals.Items) {
                if (lvi is LVItem) {
                    GDMIndividualRecord ir = (GDMIndividualRecord)((LVItem)lvi).Record;
                    if (GMHelper.GetVisibility(ir)) {
                        recordsExcluded++;
                        GMHelper.SetVisibility(ir, false);
                    }
                }
            }

            // Rebuild lists
            FillIndividualsList();

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowRestrictsResult(recordsExcluded, 0, fLangMan.LS(PLS.LSID_individuals));
        }

        private void miSourcesAllInclude_Click(Object sender, EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            int recordsIncluded = 0;

            foreach (ListViewItem lvi in lvSources.Items) {
                if (lvi is LVItem) {
                    var sr = ((LVItem)lvi).Record as GDMSourceRecord;
                    if (!GMHelper.GetVisibility(sr)) {
                        recordsIncluded++;
                        GMHelper.SetVisibility(sr, true);
                    }
                }
            }

            // Rebuild list
            FillSourcesList();

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowRestrictsResult(0, recordsIncluded, fLangMan.LS(PLS.LSID_sources));
        }

        private void miSourcesAllExclude_Click(Object sender, EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            int recordsExcluded = 0;

            foreach (ListViewItem lvi in lvSources.Items) {
                if (lvi is LVItem) {
                    var sr = ((LVItem)lvi).Record as GDMSourceRecord;
                    if (GMHelper.GetVisibility(sr)) {
                        recordsExcluded++;
                        GMHelper.SetVisibility(sr, false);
                    }
                }
            }

            // Rebuild list
            FillSourcesList();

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowRestrictsResult(recordsExcluded, 0, fLangMan.LS(PLS.LSID_sources));
        }

        // Excludes people who aren't dead, but leave people we're not sure about
        private void miIndividualsAliveExclude_Click(Object sender, EventArgs e)
        {
            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            int recordsExcluded = 0;
            int recordsIncluded = 0;

            try {
                foreach (ListViewItem lvi in lvIndividuals.Items) {
                    if (lvi is LVItem) {
                        var ir = ((LVItem)lvi).Record as GDMIndividualRecord;
                        if (ir != null && ir.IsLive() && GMHelper.GetVisibility(ir)) {
                            recordsExcluded++;
                            GMHelper.SetVisibility(ir, false);
                        }
                    }
                }
            } catch (Exception ex) {
                ReportRestrictError(ex);
            }

            // Rebuild list
            FillIndividualsList();

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowRestrictsResult(recordsExcluded, recordsIncluded, fLangMan.LS(PLS.LSID_individuals));
        }

        private void menuIndividuals_Popup(Object sender, EventArgs e)
        {
            int selectedCount = lvIndividuals.SelectedItems.Count;
            miUnconnectedExclude.Enabled = (selectedCount > 0);
            if (selectedCount <= 1) {
                miUnconnectedExclude.Text = fLangMan.LS(PLS.LSID_ExcludeIndividuals);
            } else {
                miUnconnectedExclude.Text = string.Format(fLangMan.LS(PLS.LSID_ExcludeIndividualsX), selectedCount);
            }

            miIndiDescendantsExclude.Enabled = (selectedCount == 1);
            miIndiDescendantsInclude.Enabled = (selectedCount == 1);
            miIndiAncestorsExclude.Enabled = (selectedCount == 1);
            miIndiAncestorsInclude.Enabled = (selectedCount == 1);
            miIndividualDetails.Enabled = (selectedCount == 1);
        }

        private void menuSources_Popup(Object sender, EventArgs e)
        {
            int selectedCount = lvSources.SelectedItems.Count;
            miSourceDetails.Enabled = (selectedCount == 1);
            miSourceRemovePics.Enabled = (selectedCount > 0);
            if (selectedCount <= 1) {
                miSourceRemovePics.Text = fLangMan.LS(PLS.LSID_RemovePictures);
            } else {
                miSourceRemovePics.Text = string.Format(fLangMan.LS(PLS.LSID_RemovePicturesX), selectedCount);
            }
        }

        private void lvIndividuals_ItemCheck(object sender, ItemCheckEventArgs e)
        {
            if (!fDisableRestrictsCheckEvent) {
                var lb = lvIndividuals.Items[e.Index] as LVItem;
                bool visible = GMHelper.GetVisibility(lb.Record);

                // For some reason if user presses control while clicking any row of the list, it causes a check event. We don't want any checking to happen in that case.
                if ((Control.ModifierKeys & Keys.Control) == 0) {
                    if ((e.NewValue == CheckState.Checked && !visible) || (e.NewValue == CheckState.Unchecked && visible)) {
                        lb.SetVisibility(e.NewValue == CheckState.Checked);
                        EnablePrunePanelButtons();
                    }
                } else {
                    if (lb.Record != null) {
                        e.NewValue = !visible ? CheckState.Unchecked : CheckState.Checked;
                    }
                }
            }
        }

        private void lvSources_ItemCheck(object sender, ItemCheckEventArgs e)
        {
            if (!fDisableRestrictsCheckEvent) {
                var lb = lvSources.Items[e.Index] as LVItem;
                bool visible = GMHelper.GetVisibility(lb.Record);

                // For some reason if user presses control while clicking any row of the list, it causes a check event. We don't want any checking to happen in that case.
                if ((Control.ModifierKeys & Keys.Control) == 0) {
                    if ((e.NewValue == CheckState.Checked && !visible) || (e.NewValue == CheckState.Unchecked && visible)) {
                        lb.SetVisibility(e.NewValue == CheckState.Checked);
                        EnablePrunePanelButtons();
                    }
                } else {
                    if (lb.Record != null) {
                        e.NewValue = !visible ? CheckState.Unchecked : CheckState.Checked;
                    }
                }
            }
        }

        #endregion

        // Shows the settings panel
        private void SwitchConfigPanelOn()
        {
            // Disable edit boxes etc. on previous panel to avoid confusing users
            EnableCurrentPanel(false);

            // Flag panel as being on
            fConfigPanelVisible = true;

            // Disable buttons while config panel shown
            btnHelp.Visible = false;
            btnBack.Visible = false;
            btnNext.Visible = false;
            btnCancel.Visible = false; // To give the panel a "modal" feeling

            // Make config button an "OK" button
            btnSettings.Text = fLangMan.LS(PLS.LSID_Ok);
            btnSettings.Location = new Point(344, 288);
            btnSettings.Size = new Size(fDefaultButtonSize);

            // Enable reset button
            btnSettingsCancel.Visible = true;

            // Enable config panel
            EnableCurrentPanel(true);
            ShowCurrentPanel();
        }

        // Hides the settings panel
        private void SwitchConfigPanelOff()
        {
            // Disable edit boxes etc. on config panel
            EnableCurrentPanel(false);

            // Flag panel as being off
            fConfigPanelVisible = false;

            // Restore buttons states
            btnHelp.Visible = true;
            btnBack.Enabled = true;
            btnNext.Visible = true;
            btnCancel.Visible = true;

            // Make config button back to a config button
            btnSettings.Text = fLangMan.LS(PLS.LSID_Settings);
            btnSettings.Location = new Point(88, 288);
            btnSettings.Size = new Size(fConfigButtonSize);

            // Disable reset button
            btnSettingsCancel.Visible = false;

            // Enable generic panel
            EnableCurrentPanel(true);
            // ShowCurrentPanel() also restores visibility of back button.
            ShowCurrentPanel();
        }

        // Shows the current panel and associated wizard buttons, selected by m_currentPanel, and hides all the others.
        private void ShowCurrentPanel()
        {
            // Making panel3 bVisible calls check event on list view!
            fDisableRestrictsCheckEvent = true;

            if (fConfigPanelVisible) {
                panelWelcome.Visible = false;
                panelRecords.Visible = false;
                panelKeyIndividuals.Visible = false;
                panelChooseOutput.Visible = false;
                panelAllDone.Visible = false;
                tabcontrolConfigPanel.Visible = true;
            } else {
                panelWelcome.Visible = (fCurrentPanel == PanelKind.Welcome);
                panelRecords.Visible = (fCurrentPanel == PanelKind.RestrictRecords);
                panelKeyIndividuals.Visible = (fCurrentPanel == PanelKind.WebsiteTitleAndKeys);
                panelChooseOutput.Visible = (fCurrentPanel == PanelKind.OutputFolder);
                panelAllDone.Visible = (fCurrentPanel == PanelKind.Finish);
                tabcontrolConfigPanel.Visible = false;

                btnBack.Visible = (fCurrentPanel > PanelKind.Welcome);
                // Config button disappears once html created
                btnSettings.Visible = (fCurrentPanel < PanelKind.Finish);

                if (fCurrentPanel == PanelKind.Finish) {
                    btnCancel.Text = fLangMan.LS(PLS.LSID_Finish);
                    // Can't go back , because we can't undo the file creations.
                    btnBack.Visible = false;
                    btnHelp.Location = new Point(8, 288);
                    btnCancel.Location = new Point(424, 288);
                    btnNext.Visible = false;
                } else if (fCurrentPanel > PanelKind.Finish) {
                    btnHelp.Location = new Point(8, 288);
                    btnNext.Text = fLangMan.LS(PLS.LSID_Finish);
                    btnCancel.Visible = false;
                    btnHelp.Visible = false;
                    // Can't go back , because we can't undo the file creations.
                    btnBack.Visible = false;
                } else if (fCurrentPanel == PanelKind.WebsiteTitleAndKeys) {
                    txtSelectKey.Focus();
                    txtSelectKey.SelectAll();
                } else if (fCurrentPanel == PanelKind.OutputFolder) {
                    txtChooseOutput.Focus();
                    txtChooseOutput.SelectAll();
                } else {
                    btnNext.Text = fLangMan.LS(PLS.LSID_Next);
                    btnCancel.Visible = true;
                    btnCancel.Text = fLangMan.LS(PLS.LSID_Quit);
                    btnCancel.Location = new Point(8, 288);
                    btnHelp.Visible = true;
                }

                if (fCurrentPanel == PanelKind.RestrictRecords) {
                    EnablePrunePanelButtons();
                }

                EnableNextButton();
            }

            fDisableRestrictsCheckEvent = false;
        }

        // Logic for the next page button to ensure that user has completed the current page
        private void EnableNextButton()
        {
            if (fCurrentPanel == PanelKind.OutputFolder && txtChooseOutput.Text.Length == 0) {
                btnNext.Enabled = false;
            } else {
                btnNext.Enabled = true;
            }
        }

        // Logic for the key individuals delete button checks that an individual is selected for deletion
        private void EnableKeyIndividualsDeleteButton()
        {
            btnSelectKeyDelete.Enabled = (lstKeyIndividuals.SelectedItems.Count > 0);
        }

        // Logic for the mini tree config buttons
        private void EnableMiniTreeButtons()
        {
            bool includeTreeDiagrams = chkConfigIncludeTreeDiagrams.Checked;

            chkConfigTreeDiagramsFakeBg.Enabled = includeTreeDiagrams;
            gbMiniTreeColours.Enabled = includeTreeDiagrams;
            btnConfigMiniTreeColourIndiBackground.Enabled = includeTreeDiagrams;
            btnConfigMiniTreeColourIndiHighlight.Enabled = includeTreeDiagrams;
            btnConfigMiniTreeColourIndiBgConcealed.Enabled = includeTreeDiagrams;
            btnConfigMiniTreeColourIndiShade.Enabled = includeTreeDiagrams;
            btnConfigMiniTreeColourIndiText.Enabled = includeTreeDiagrams;
            btnConfigMiniTreeColourIndiLink.Enabled = includeTreeDiagrams;
            btnConfigMiniTreeColourBranch.Enabled = includeTreeDiagrams;
            btnConfigMiniTreeColourIndiBorder.Enabled = includeTreeDiagrams;
            btnConfigMiniTreeColourIndiFgConcealed.Enabled = includeTreeDiagrams;
            chkConfigConserveTreeWidth.Enabled = includeTreeDiagrams;
            chkConfigKeepSiblingOrder.Enabled = includeTreeDiagrams;

            if (includeTreeDiagrams) {
                SetMiniTreeColourConfigButtons();
            } else {
                ClearMiniTreeColourConfigButtons();
            }
        }

        // Fills the controls in each page of the app
        private void InitialiseCurrentPanel()
        {
            switch (fCurrentPanel) {
                case PanelKind.WebsiteTitleAndKeys:
                    txtSelectKey.Text = GMConfig.Instance.SiteTitle;
                    GMConfig.Instance.FirstRecordXRef = "";
                    FillKeyIndividualsList();
                    break;

                case PanelKind.OutputFolder:
                    txtChooseOutput.Text = GMConfig.Instance.OutputFolder;
                    break;

                case PanelKind.Finish:
                    chkAllDoneShowSite.Visible = File.Exists(GMConfig.Instance.FrontPageURL);
                    chkAllDoneShowSite.Checked = GMConfig.Instance.OpenWebsiteOnExit;
                    lblAllDone.Text = GMConfig.Instance.OutputFolder;
                    if (GMConfig.Instance.FrontPageFilename != "") {
                        lblAllDoneStartFile.Text = string.Format(fLangMan.LS(PLS.LSID_FrontPageFile), GMConfig.Instance.FrontPageFilename, "html");
                        lblAllDoneStartFile.Visible = true;
                    } else {
                        lblAllDoneStartFile.Text = fLangMan.LS(PLS.LSID_NoFrontPage);
                        lblAllDoneStartFile.Visible = false;
                    }
                    break;
            }

            EnableNextButton();
        }

        // Handles the processing needed at each stage of the app, as the user moves through each page of the wizard.
        private bool ValidateCurrentPanel()
        {
            DialogResult result;

            // Loop gives user the option to retry folder creation. Use return to exit.
            while (true) {
                switch (fCurrentPanel) {
                    case PanelKind.Welcome:
                        string inputFilename = fBase.Context.FileName;
                        fLogger.WriteInfo("Selected file: " + inputFilename);
                        if (GMConfig.Instance.OutputFolder == "") {
                            GMConfig.Instance.OutputFolder = Path.GetDirectoryName(inputFilename);
                            GMConfig.Instance.OutputFolder += "\\GEDmill_Output";
                        }
                        GMConfig.Instance.FirstRecordXRef = "";
                        GMConfig.Instance.KeyIndividuals = new List<string>();
                        GMConfig.Instance.FirstRecordXRef = "";
                        FillIndividualsList();
                        FillSourcesList();
                        return true;

                    case PanelKind.RestrictRecords:
                        // Go through individuals list and set restricted flag as appropriate
                        bool somethingChecked = false;
                        foreach (ListViewItem li in lvIndividuals.Items) {
                            if (li.Checked) {
                                somethingChecked = true;
                            } else {
                                // Already done on click event: ((CListableBool)li).SetRestricted( !bChecked );
                                GMHelper.RestrictAssociatedSources(fBase.Context.Tree, (GDMIndividualRecord)((LVItem)li).Record);
                            }
                        }

                        if (!somethingChecked) {
                            ShowAlert(fLangMan.LS(PLS.LSID_PleaseSelectAtLeastOneIndividual));
                            return false;
                        }

                        /*if (PrunepanelDataChanged) {
                            bool dialogResult = ShowQuestionYN("You have made changes which will affect the website but have not saved them.\r\nWould you like to save them now?");
                            if (dialogResult) {
                                buttonPruneRecordsSave_click(null, null);
                            }
                        }*/
                        return true;

                    case PanelKind.WebsiteTitleAndKeys:
                        GMConfig.Instance.SiteTitle = txtSelectKey.Text;
                        return true;

                    case PanelKind.OutputFolder:
                        GMConfig.Instance.OutputFolder = txtChooseOutput.Text;
                        string outputFolder = GMConfig.Instance.OutputFolder;
                        if (outputFolder != "") {
                            outputFolder = outputFolder + '\\';
                        }
                        string imageFolder = string.Concat(outputFolder, GMConfig.Instance.ImageFolder);

                        // To generate warning if deleting folder & files.
                        bool preserveFiles = false;

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
                            result = PrepareOutputDirectory(imageFolder, false);
                            if (result == DialogResult.Cancel) {
                                return false;
                            }
                            if (result == DialogResult.OK) {
                                break;
                            }
                        }

                        if (CreateWebsite(outputFolder, imageFolder)) {
                            return true;
                        }

                        return false;

                    case PanelKind.Finish:
                        return true;

                    default:
                        return true;
                }
            }
        }

        // Populates the list of individuals records for inclusion/exclusion in the website
        private void FillIndividualsList()
        {
            var indiRecs = fBase.Context.Tree.GetRecords<GDMIndividualRecord>();

            fDisableRestrictsCheckEvent = true;

            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            lvIndividuals.Clear();

            lvIndividuals.View = View.Details;
            int nameWidth = lvIndividuals.Width - 70 - 70 - 20;
            lvIndividuals.Columns.Add(fLangMan.LS(PLS.LSID_Include), 30, HorizontalAlignment.Left);
            nameWidth -= 30;
            lvIndividuals.Columns.Add(fLangMan.LS(PLS.LSID_Name), nameWidth, HorizontalAlignment.Left);
            lvIndividuals.Columns.Add(fLangMan.LS(PLS.LSID_Born), 70, HorizontalAlignment.Left);
            lvIndividuals.Columns.Add(fLangMan.LS(PLS.LSID_Died), 70, HorizontalAlignment.Left);
            lvIndividuals.Columns.Add(fLangMan.LS(PLS.LSID_Id), 60, HorizontalAlignment.Left);
            lvIndividuals.Columns.Add(fLangMan.LS(PLS.LSID_UserRef), 78, HorizontalAlignment.Left);
            lvIndividuals.Columns.Add(fLangMan.LS(PLS.LSID_Pics), 48, HorizontalAlignment.Left);

            // Build an array first then blit the whole array to the list control. This is faster than adding each item to the list control individually.
            var temporaryItemsList = new ListViewItem[indiRecs.Count];
            int nItem = 0;
            foreach (GDMIndividualRecord ir in indiRecs) {
                // Only allow fully unrestricted individuals.
                /*if (excludeRestricted && !ir.GetVisibility(EVisibility.Visible)) {
                    continue;
                }*/

                var lbItem = new LVItem(ir);
                SetIndividualSubItems(lbItem, ir, true);

                lbItem.Checked = GMHelper.GetVisibility(ir);
                temporaryItemsList[nItem++] = lbItem;
            }

            lvIndividuals.Items.AddRange(temporaryItemsList);
            lvIndividuals.Sort();

            pageIndividuals.Text = string.Format("{0} ({1})", fLangMan.LS(PLS.LSID_Individuals), lvIndividuals.Items.Count);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            fDisableRestrictsCheckEvent = false;
        }

        // Attaches sub-items to a list item (before the list item is added to the list)
        private void SetIndividualSubItems(ListViewItem lvItem, GDMIndividualRecord ir, bool checkBoxes)
        {
            // Save checkbox state because SubItems.Clear() clears item.Text and item.Checked as well, so replace old value after calling Clear().
            bool wasChecked = lvItem.Checked;
            lvItem.SubItems.Clear();
            lvItem.Checked = wasChecked;

            // If the list view has check boxes, the item is for the checkbox.
            // Otherwise the item is for the name, and so the sub items won't include the name.
            if (checkBoxes) {
                string surname, firstName;
                GMHelper.CapitaliseName(ir.GetPrimaryPersonalName(), out firstName, out surname);
                lvItem.SubItems.Add(new LVNameItem(surname, firstName));
            }

            var lifeDatesX = ir.GetLifeDates();
            var birthDate = (lifeDatesX.BirthEvent == null) ? 0 : lifeDatesX.BirthEvent.Date.GetChronologicalYear();
            var deathDate = (lifeDatesX.DeathEvent == null) ? 0 : lifeDatesX.DeathEvent.Date.GetChronologicalYear();

            lvItem.SubItems.Add(new LVNumberItem(birthDate, false));
            lvItem.SubItems.Add(new LVNumberItem(deathDate, false));
            lvItem.SubItems.Add(new LVStringItem(ir.XRef));

            string uref = (ir.HasUserReferences) ? ir.UserReferences[0].StringValue : "";
            lvItem.SubItems.Add(new LVStringItem(uref));

            int nVisiblePics, nTotalPics;
            GMHelper.CountMFRs(ir, out nTotalPics, out nVisiblePics);
            if (nVisiblePics != nTotalPics) {
                lvItem.SubItems.Add(new LVNumberItem(nVisiblePics, string.Format("{0}/{1}", nVisiblePics, nTotalPics)));
            } else {
                lvItem.SubItems.Add(new LVNumberItem(nTotalPics, string.Format("{0}", nTotalPics)));
            }
        }

        // Populates the list of source records for inclusion/exclusion in the website
        private void FillSourcesList()
        {
            var sources = fBase.Context.Tree.GetRecords<GDMSourceRecord>();

            fDisableRestrictsCheckEvent = true; // call to item.Checked below invokes event handler.

            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            lvSources.Clear();

            lvSources.View = View.Details;
            int nWidthTitle = lvSources.Width - 140 - 20;
            lvSources.Columns.Add(fLangMan.LS(PLS.LSID_Include), 30, HorizontalAlignment.Left);
            nWidthTitle -= 30;
            lvSources.Columns.Add(fLangMan.LS(PLS.LSID_Title), nWidthTitle, HorizontalAlignment.Left);
            lvSources.Columns.Add(fLangMan.LS(PLS.LSID_Repository), 100, HorizontalAlignment.Left);
            lvSources.Columns.Add(fLangMan.LS(PLS.LSID_Citations), 60, HorizontalAlignment.Left);
            lvSources.Columns.Add("B", 30, HorizontalAlignment.Left);
            lvSources.Columns.Add("M", 30, HorizontalAlignment.Left);
            lvSources.Columns.Add("D", 30, HorizontalAlignment.Left);
            lvSources.Columns.Add(fLangMan.LS(PLS.LSID_Id), 60, HorizontalAlignment.Left);
            lvSources.Columns.Add(fLangMan.LS(PLS.LSID_Pics), 48, HorizontalAlignment.Left);

            var temporaryItemsList = new ListViewItem[sources.Count];
            int nItem = 0;
            foreach (GDMSourceRecord sr in sources) {
                var item = new LVItem(sr);
                SetSourceSubItems(item, sr, true);
                item.Checked = GMHelper.GetVisibility(sr);
                temporaryItemsList[nItem++] = item;
            }

            lvSources.Items.AddRange(temporaryItemsList);
            lvSources.Sort();

            pageSources.Text = string.Format("{0} ({1})", fLangMan.LS(PLS.LSID_Sources), lvSources.Items.Count);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            fDisableRestrictsCheckEvent = false;
        }

        // Attaches sub-items to a list item (before the list item is added to the list)
        private void SetSourceSubItems(ListViewItem lvItem, GDMSourceRecord sr, bool firstColumnIsCheckbox)
        {
            // Store checkbox value because SubItems.Clear() clears item.Text and item.Checked as well!
            bool wasChecked = lvItem.Checked;
            lvItem.SubItems.Clear();
            lvItem.Checked = wasChecked;

            if (firstColumnIsCheckbox) {
                // First nColumn (ie. item) is checkbox, so first sub-item is title.
                lvItem.SubItems.Add(new LVStringItem(sr.ShortTitle));
            }

            string repositories = "";
            foreach (GDMRepositoryCitation repoCit in sr.RepositoryCitations) {
                GDMRepositoryRecord repoRec = fBase.Context.Tree.GetPtrValue<GDMRepositoryRecord>(repoCit);
                if (repoRec != null) {
                    if (!string.IsNullOrEmpty(repoRec.RepositoryName)) {
                        if (repositories != "") {
                            repositories += ", ";
                        }
                        repositories += repoRec.RepositoryName;
                    }
                }
            }
            lvItem.SubItems.Add(new LVStringItem(repositories));

            int nBirths = 0;
            int nMarriages = 0;
            int nDeaths = 0;
            int nIndis = 0;
            int nFamilies = 0;
            int nNotes = 0;
            int nOther = 0;
            int nCitations = 0;

            var backReferences = GMHelper.GetBackReferences(fBase.Context.Tree, sr);
            if (backReferences.Count > 0) {
                foreach (BackReference br in backReferences) {
                    switch (br.EventType) {
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
                            switch (br.RecordType) {
                                case GDMRecordType.rtIndividual:
                                    nCitations++;
                                    nIndis++;
                                    break;
                                case GDMRecordType.rtFamily:
                                    // Strictly this should be plus 2 if husb & wife both known, otherwise 1 or 0.
                                    nCitations++;
                                    nFamilies++;
                                    break;
                                case GDMRecordType.rtNote:
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

            lvItem.SubItems.Add(new LVNumberItem(nCitations));
            lvItem.SubItems.Add(new LVNumberItem(nBirths));
            lvItem.SubItems.Add(new LVNumberItem(nMarriages));
            lvItem.SubItems.Add(new LVNumberItem(nDeaths));
            lvItem.SubItems.Add(new LVStringItem(sr.XRef));

            int nVisiblePics, nTotalPics;
            GMHelper.CountMFRs(sr, out nTotalPics, out nVisiblePics);

            if (nVisiblePics != nTotalPics) {
                lvItem.SubItems.Add(new LVNumberItem(nVisiblePics, string.Format("{0}/{1}", nVisiblePics, nTotalPics)));
            } else {
                lvItem.SubItems.Add(new LVNumberItem(nTotalPics, string.Format("{0}", nTotalPics)));
            }
            lvItem.Checked = wasChecked;
        }

        // Spawns the website creation thread, which calls CWebsite.Create to do the work.
        private bool CreateWebsite(string outputFolder, string imagesFolder)
        {
            fLogger.WriteInfo("Creating website");

            // If user has specified a background image, check it exists
            if (!string.IsNullOrEmpty(GMConfig.Instance.BackgroundImage) && File.Exists(GMConfig.Instance.BackgroundImage) == false) {
                fLogger.WriteError("Can't find background image " + GMConfig.Instance.BackgroundImage);

                bool result = ShowQuestionYN(string.Format(fLangMan.LS(PLS.LSID_BackgroundMissed), GMConfig.Instance.BackgroundImage));
                if (!result) {
                    fLogger.WriteInfo("Message box cancelled (1)");
                    return false;
                }
            }

            fLogger.WriteInfo("Starting progress window");

            var website = new Website(fBase.Context, outputFolder, fLangMan);

            bool res = AppHost.Instance.ExecuteWorkExt(website.Create, fLangMan.LS(PLS.LSID_CreatingWebsite));

            fLogger.WriteInfo("Website create done.");

            return res;
        }

        // Enable the current page of the wizard
        private void EnableCurrentPanel(bool enable)
        {
            if (fConfigPanelVisible) {
                tabcontrolConfigPanel.Enabled = enable;
                // Disable all other panels
                enable = false;
            } else {
                panelWelcome.Enabled = (fCurrentPanel == PanelKind.Welcome && enable);
                panelRecords.Enabled = (fCurrentPanel == PanelKind.RestrictRecords && enable);
                panelKeyIndividuals.Enabled = (fCurrentPanel == PanelKind.WebsiteTitleAndKeys && enable);
                panelChooseOutput.Enabled = (fCurrentPanel == PanelKind.OutputFolder && enable);
                panelAllDone.Enabled = (fCurrentPanel == PanelKind.Finish && enable);

                tabcontrolConfigPanel.Enabled = false;
            }

            pictureBox.Enabled = enable;
        }

        // Reports any exception thrown during the prune operation
        private void ReportRestrictError(Exception e)
        {
            ShowAlert(string.Format(fLangMan.LS(PLS.LSID_TreeError), e.StackTrace));
            fLogger.WriteInfo(string.Format("Caught navigation exception : {0}", e.ToString()));
        }

        // FIXME: i18l
        // Displays the statistics of the prune operation
        private void ShowRestrictsResult(int excluded, int included, string type)
        {
            string msg = "";
            if (excluded == 0 && included == 0) {
                msg = fLangMan.LS(PLS.LSID_NoChangesMade);
            } else {
                if (included != 0) {
                    msg = string.Format("{0} {1}{2} checked.", included, type, included > 1 ? "s" : "");
                }
                if (excluded != 0) {
                    if (msg != "") {
                        msg += "\r\n";
                    }
                    msg += string.Format("{0} {1}{2} unchecked.", excluded, type, excluded > 1 ? "s" : "");
                }
            }
            ShowMessage(msg);
        }

        // FIXME: i18l
        // Displays the statistics of the remove pictures operation
        private void ShowHidePicsResult(int hidden)
        {
            string msg = "";
            if (hidden == 0) {
                msg = fLangMan.LS(PLS.LSID_NoMultimediaFilesHidden);
            } else {
                msg = string.Format("{0} multimedia file{1} hidden.", hidden, hidden > 1 ? "s" : "");
            }
            ShowMessage(msg);
        }

        // Initialises config panel controls
        private void LoadConfigPanelSettings()
        {
            txtConfigFrontImageEdit.Text = GMConfig.Instance.FrontPageImageFilename;
            txtConfigFrontImageEdit.SelectionStart = txtConfigFrontImageEdit.Text.Length;
            txtConfigFrontImageEdit.SelectionLength = txtConfigFrontImageEdit.Text.Length;
            txtConfigBackImageEdit.Text = GMConfig.Instance.BackgroundImage;
            txtConfigBackImageEdit.SelectionStart = txtConfigBackImageEdit.Text.Length;
            txtConfigBackImageEdit.SelectionLength = txtConfigBackImageEdit.Text.Length;
            txtConfigIndiImageWidth.Text = GMConfig.Instance.MaxImageWidth.ToString();
            txtConfigIndiImageHeight.Text = GMConfig.Instance.MaxImageHeight.ToString();
            txtConfigSourceImageWidth.Text = GMConfig.Instance.MaxSourceImageWidth.ToString();
            txtConfigSourceImageHeight.Text = GMConfig.Instance.MaxSourceImageHeight.ToString();
            txtConfigThumbnailImageWidth.Text = GMConfig.Instance.MaxThumbnailImageWidth.ToString();
            txtConfigThumbnailImageHeight.Text = GMConfig.Instance.MaxThumbnailImageHeight.ToString();
            txtConfigNoName.Text = GMConfig.Instance.UnknownName;
            txtConfigWithheldName.Text = GMConfig.Instance.ConcealedName;
            radConfigWithheldNameLabel.Checked = !GMConfig.Instance.UseWithheldNames;
            radConfigWithheldNameName.Checked = GMConfig.Instance.UseWithheldNames;
            chkConfigCapSurnames.Checked = (GMConfig.Instance.NameCapitalisation == 1);
            chkConfigCapEvents.Checked = GMConfig.Instance.CapitaliseEventDescriptions;
            chkConfigHideEmails.Checked = GMConfig.Instance.ObfuscateEmails;
            chkConfigOccupationHeadline.Checked = GMConfig.Instance.OccupationHeadline;
            chkConfigShowWithheldRecords.Checked = GMConfig.Instance.OnlyConceal;
            txtConfigTabSpaces.Text = GMConfig.Instance.TabSpaces.ToString();
            txtConfigCommentary.Text = GMConfig.Instance.CommentaryText;
            chkConfigCommentaryIsHtml.Checked = GMConfig.Instance.CommentaryIsHtml;
            chkConfigStats.Checked = GMConfig.Instance.ShowFrontPageStats;
            chkConfigNonPictures.Checked = GMConfig.Instance.AllowNonPictures;
            chkConfigIndiImages.Checked = GMConfig.Instance.AllowMultipleImages;
            chkConfigIncludeTreeDiagrams.Checked = GMConfig.Instance.ShowMiniTrees;
            chkConfigTreeDiagramsFakeBg.Checked = GMConfig.Instance.FakeMiniTreeTransparency;
            txtConfigEmail.Text = GMConfig.Instance.UserEmailAddress;
            txtConfigIndexName.Text = GMConfig.Instance.FrontPageFilename;
            lblConfigIndexNameExtn.Text = ".html";
            if (GMConfig.Instance.MainWebsiteLink.Length == 0) {
                txtConfigUserLink.Text = "http://";
            } else {
                txtConfigUserLink.Text = GMConfig.Instance.MainWebsiteLink;
            }
            chkConfigMultiPageIndex.Checked = GMConfig.Instance.MultiPageIndexes;
            chkConfigUserRefInIndex.Checked = GMConfig.Instance.IncludeUserRefInIndex;
            txtConfigMultiPageIndexNumber.Text = GMConfig.Instance.IndividualsPerIndexPage.ToString();
            chkConfigKeepOriginals.Checked = GMConfig.Instance.LinkOriginalPicture;
            chkConfigRenameOriginals.Checked = GMConfig.Instance.RenameOriginalPicture;
            chkConfigUserRecFilename.Checked = GMConfig.Instance.UserRecFilename;
            txtConfigCustomFooter.Text = GMConfig.Instance.CustomFooter;
            chkConfigFooterIsHtml.Checked = GMConfig.Instance.FooterIsHtml;
            chkConfigConserveTreeWidth.Checked = GMConfig.Instance.ConserveTreeWidth;
            chkConfigKeepSiblingOrder.Checked = GMConfig.Instance.KeepSiblingOrder;
            chkConfigAllowMultimedia.Checked = GMConfig.Instance.AllowMultimedia;

            fColorConfigMiniTreeBranch = GMConfig.Instance.MiniTreeColourBranch;
            fColorConfigMiniTreeIndiBorder = GMConfig.Instance.MiniTreeColourIndiBorder;
            fColorConfigMiniTreeIndiBackground = GMConfig.Instance.MiniTreeColourIndiBackground;
            fColorConfigMiniTreeIndiHighlight = GMConfig.Instance.MiniTreeColourIndiHighlight;
            fColorConfigMiniTreeIndiBgConcealed = GMConfig.Instance.MiniTreeColourIndiBgConcealed;
            fColorConfigMiniTreeIndiFgConcealed = GMConfig.Instance.MiniTreeColourIndiFgConcealed;
            fColorConfigMiniTreeIndiShade = GMConfig.Instance.MiniTreeColourIndiShade;
            fColorConfigMiniTreeIndiText = GMConfig.Instance.MiniTreeColourIndiText;
            fColorConfigMiniTreeIndiLink = GMConfig.Instance.MiniTreeColourIndiLink;
            fColorConfigMiniTreeBackground = GMConfig.Instance.MiniTreeColourBackground;

            chkConfigSupressBackreferences.Checked = !GMConfig.Instance.SupressBackreferences;

            SetMiniTreeColourConfigButtons();

            EnableMultiPageIndexConfig();
            EnableMultimediaConfig();
            EnableWithheldConfig();
        }

        // Colours the buttons that set the mini tree colours according to the values they control
        private void SetMiniTreeColourConfigButtons()
        {
            btnConfigMiniTreeColourBranch.BackColor = fColorConfigMiniTreeIndiBackground;
            btnConfigMiniTreeColourBranch.ForeColor = fColorConfigMiniTreeBranch;

            btnConfigMiniTreeColourIndiBorder.BackColor = fColorConfigMiniTreeIndiBackground;
            btnConfigMiniTreeColourIndiBorder.ForeColor = fColorConfigMiniTreeIndiBorder;

            btnConfigMiniTreeColourIndiBackground.BackColor = fColorConfigMiniTreeIndiBackground;
            btnConfigMiniTreeColourIndiBackground.ForeColor = fColorConfigMiniTreeIndiLink;

            btnConfigMiniTreeColourIndiHighlight.BackColor = fColorConfigMiniTreeIndiHighlight;
            btnConfigMiniTreeColourIndiHighlight.ForeColor = fColorConfigMiniTreeIndiText;

            btnConfigMiniTreeColourIndiBgConcealed.BackColor = fColorConfigMiniTreeIndiBgConcealed;
            btnConfigMiniTreeColourIndiBgConcealed.ForeColor = fColorConfigMiniTreeIndiFgConcealed;

            btnConfigMiniTreeColourIndiFgConcealed.BackColor = fColorConfigMiniTreeIndiBgConcealed;
            btnConfigMiniTreeColourIndiFgConcealed.ForeColor = fColorConfigMiniTreeIndiFgConcealed;

            btnConfigMiniTreeColourIndiShade.BackColor = fColorConfigMiniTreeIndiShade;
            btnConfigMiniTreeColourIndiShade.ForeColor = fColorConfigMiniTreeIndiLink;

            btnConfigMiniTreeColourIndiText.BackColor = fColorConfigMiniTreeIndiHighlight;
            btnConfigMiniTreeColourIndiText.ForeColor = fColorConfigMiniTreeIndiText;

            btnConfigMiniTreeColourIndiLink.BackColor = fColorConfigMiniTreeIndiBackground;
            btnConfigMiniTreeColourIndiLink.ForeColor = fColorConfigMiniTreeIndiLink;
        }

        // Used to set all buttons grey when form is disabled
        private void ClearMiniTreeColourConfigButtons()
        {
            btnConfigMiniTreeColourBranch.BackColor = Form.DefaultBackColor;
            btnConfigMiniTreeColourBranch.ForeColor = Form.DefaultForeColor;

            btnConfigMiniTreeColourIndiBorder.BackColor = Form.DefaultBackColor;
            btnConfigMiniTreeColourIndiBorder.ForeColor = Form.DefaultForeColor;

            btnConfigMiniTreeColourIndiBackground.BackColor = Form.DefaultBackColor;
            btnConfigMiniTreeColourIndiBackground.ForeColor = Form.DefaultForeColor;

            btnConfigMiniTreeColourIndiHighlight.BackColor = Form.DefaultBackColor;
            btnConfigMiniTreeColourIndiHighlight.ForeColor = Form.DefaultForeColor;

            btnConfigMiniTreeColourIndiBgConcealed.BackColor = Form.DefaultBackColor;
            btnConfigMiniTreeColourIndiBgConcealed.ForeColor = Form.DefaultForeColor;

            btnConfigMiniTreeColourIndiFgConcealed.BackColor = Form.DefaultBackColor;
            btnConfigMiniTreeColourIndiFgConcealed.ForeColor = Form.DefaultForeColor;

            btnConfigMiniTreeColourIndiShade.BackColor = Form.DefaultBackColor;
            btnConfigMiniTreeColourIndiShade.ForeColor = Form.DefaultForeColor;

            btnConfigMiniTreeColourIndiText.BackColor = Form.DefaultBackColor;
            btnConfigMiniTreeColourIndiText.ForeColor = Form.DefaultForeColor;

            btnConfigMiniTreeColourIndiLink.BackColor = Form.DefaultBackColor;
            btnConfigMiniTreeColourIndiLink.ForeColor = Form.DefaultForeColor;
        }

        // Saves changes made in config panel back to main config.
        // Reads (and sanity checks) values from all controls in the config panel and puts them into the CConfig instance.
        private void SaveConfigPanelSettings()
        {
            GMConfig.Instance.FrontPageImageFilename = txtConfigFrontImageEdit.Text;
            GMConfig.Instance.BackgroundImage = txtConfigBackImageEdit.Text;

            try {
                // Sanity check value
                int maxImageWidth = int.Parse(txtConfigIndiImageWidth.Text);
                if (maxImageWidth > 0 && maxImageWidth <= 300) {
                    GMConfig.Instance.MaxImageWidth = maxImageWidth;
                } else if (GMConfig.Instance.MaxImageWidth != maxImageWidth && maxImageWidth > 300) {
                    bool dialogResult = ShowQuestionYN(string.Format(fLangMan.LS(PLS.LSID_ImageSizeProblem), maxImageWidth));
                    if (dialogResult) {
                        GMConfig.Instance.MaxImageWidth = maxImageWidth;
                    }
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            try {
                // Sanity check value
                int maxImageHeight = int.Parse(txtConfigIndiImageHeight.Text);
                if (maxImageHeight > 0 && maxImageHeight <= 800) {
                    GMConfig.Instance.MaxImageHeight = maxImageHeight;
                } else if (GMConfig.Instance.MaxImageHeight != maxImageHeight && maxImageHeight > 800) {
                    bool dialogResult = ShowQuestionYN(string.Format(fLangMan.LS(PLS.LSID_ImageSizeProblem), maxImageHeight));
                    if (dialogResult) {
                        GMConfig.Instance.MaxImageHeight = maxImageHeight;
                    }
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            try {
                // Sanity check value
                int maxSourceImageWidth = int.Parse(txtConfigSourceImageWidth.Text);
                if (maxSourceImageWidth > 0 && maxSourceImageWidth <= 800) {
                    GMConfig.Instance.MaxSourceImageWidth = maxSourceImageWidth;
                } else if (GMConfig.Instance.MaxSourceImageWidth != maxSourceImageWidth && maxSourceImageWidth > 800) {
                    bool dialogResult = ShowQuestionYN(string.Format(fLangMan.LS(PLS.LSID_ImageSizeProblem), maxSourceImageWidth));
                    if (dialogResult) {
                        GMConfig.Instance.MaxSourceImageWidth = maxSourceImageWidth;
                    }
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            try {
                // Sanity check value
                int maxSourceImageHeight = int.Parse(txtConfigSourceImageHeight.Text);
                if (maxSourceImageHeight > 0 && maxSourceImageHeight <= 800) {
                    GMConfig.Instance.MaxSourceImageHeight = maxSourceImageHeight;
                } else if (GMConfig.Instance.MaxSourceImageHeight != maxSourceImageHeight && maxSourceImageHeight > 800) {
                    bool dialogResult = ShowQuestionYN(string.Format(fLangMan.LS(PLS.LSID_ImageSizeProblem), maxSourceImageHeight));
                    if (dialogResult) {
                        GMConfig.Instance.MaxSourceImageHeight = maxSourceImageHeight;
                    }
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            try {
                // Sanity check value
                int maxThumbnailImageWidth = int.Parse(txtConfigThumbnailImageWidth.Text);
                if (maxThumbnailImageWidth > 0 && maxThumbnailImageWidth < 80) {
                    GMConfig.Instance.MaxThumbnailImageWidth = maxThumbnailImageWidth;
                } else if (GMConfig.Instance.MaxThumbnailImageWidth != maxThumbnailImageWidth && maxThumbnailImageWidth > 80) {
                    bool dialogResult = ShowQuestionYN(string.Format(fLangMan.LS(PLS.LSID_ImageSizeProblem), maxThumbnailImageWidth));
                    if (dialogResult) {
                        GMConfig.Instance.MaxThumbnailImageWidth = maxThumbnailImageWidth;
                    }
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            try {
                // Sanity check value
                int maxThumbnailImageHeight = int.Parse(txtConfigThumbnailImageHeight.Text);
                if (maxThumbnailImageHeight > 0 && maxThumbnailImageHeight < 80) {
                    GMConfig.Instance.MaxThumbnailImageHeight = maxThumbnailImageHeight;
                } else if (GMConfig.Instance.MaxThumbnailImageHeight != maxThumbnailImageHeight && maxThumbnailImageHeight > 80) {
                    bool dialogResult = ShowQuestionYN(string.Format(fLangMan.LS(PLS.LSID_ImageSizeProblem), maxThumbnailImageHeight));
                    if (dialogResult) {
                        GMConfig.Instance.MaxThumbnailImageHeight = maxThumbnailImageHeight;
                    }
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            GMConfig.Instance.UserRecFilename = chkConfigUserRecFilename.Checked;
            if (txtConfigNoName.Text.Length > 0) {
                GMConfig.Instance.UnknownName = txtConfigNoName.Text;
            }
            if (txtConfigWithheldName.Text.Length > 0) {
                GMConfig.Instance.ConcealedName = txtConfigWithheldName.Text;
            }
            GMConfig.Instance.UseWithheldNames = radConfigWithheldNameName.Checked;
            GMConfig.Instance.NameCapitalisation = (chkConfigCapSurnames.Checked ? 1 : 0);
            GMConfig.Instance.CapitaliseEventDescriptions = chkConfigCapEvents.Checked;
            GMConfig.Instance.ObfuscateEmails = chkConfigHideEmails.Checked;
            GMConfig.Instance.OccupationHeadline = chkConfigOccupationHeadline.Checked;
            GMConfig.Instance.OnlyConceal = chkConfigShowWithheldRecords.Checked;

            try {
                // Sanity check value
                int tabSpaces = int.Parse(txtConfigTabSpaces.Text);
                if (tabSpaces > 0 && tabSpaces < 64) {
                    GMConfig.Instance.TabSpaces = tabSpaces;
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            GMConfig.Instance.CommentaryText = txtConfigCommentary.Text;
            GMConfig.Instance.CommentaryIsHtml = chkConfigCommentaryIsHtml.Checked;
            GMConfig.Instance.ShowFrontPageStats = chkConfigStats.Checked;
            GMConfig.Instance.ShowMiniTrees = chkConfigIncludeTreeDiagrams.Checked;
            GMConfig.Instance.FakeMiniTreeTransparency = chkConfigTreeDiagramsFakeBg.Checked;
            GMConfig.Instance.UserEmailAddress = txtConfigEmail.Text;
            GMConfig.Instance.AllowMultipleImages = chkConfigIndiImages.Checked;
            GMConfig.Instance.AllowNonPictures = chkConfigNonPictures.Checked;
            GMConfig.Instance.LinkOriginalPicture = chkConfigKeepOriginals.Checked;
            GMConfig.Instance.RenameOriginalPicture = chkConfigRenameOriginals.Checked;

            // Validate and strip trailing .html or .htm in case user has put them on
            string frontPageFilename = txtConfigIndexName.Text;
            string frontPageFilenameUpper = frontPageFilename.ToUpper();
            if (frontPageFilenameUpper.LastIndexOf(".HTML") >= 0) {
                frontPageFilename = frontPageFilename.Substring(0, frontPageFilename.Length - 5);
            } else if (frontPageFilenameUpper.LastIndexOf(".HTM") >= 0) {
                frontPageFilename = frontPageFilename.Substring(0, frontPageFilename.Length - 4);
            }
            GMConfig.Instance.FrontPageFilename = frontPageFilename;

            // Validate and strip leading http:// in case user has it them on
            string mainWebsiteLink = txtConfigUserLink.Text;
            if (mainWebsiteLink.ToLower() == "http://") {
                // User hasn't altered default value
                mainWebsiteLink = "";
            }

            GMConfig.Instance.MainWebsiteLink = mainWebsiteLink;
            GMConfig.Instance.MultiPageIndexes = chkConfigMultiPageIndex.Checked;
            GMConfig.Instance.IncludeUserRefInIndex = chkConfigUserRefInIndex.Checked;

            try {
                // Sanity check value
                int index = int.Parse(txtConfigMultiPageIndexNumber.Text);
                if (index > 0) {
                    GMConfig.Instance.IndividualsPerIndexPage = index;
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            GMConfig.Instance.CustomFooter = txtConfigCustomFooter.Text;

            GMConfig.Instance.FooterIsHtml = chkConfigFooterIsHtml.Checked;
            GMConfig.Instance.ConserveTreeWidth = chkConfigConserveTreeWidth.Checked;
            GMConfig.Instance.KeepSiblingOrder = chkConfigKeepSiblingOrder.Checked;
            GMConfig.Instance.AllowMultimedia = chkConfigAllowMultimedia.Checked;

            GMConfig.Instance.MiniTreeColourBranch = fColorConfigMiniTreeBranch;
            GMConfig.Instance.MiniTreeColourIndiBorder = fColorConfigMiniTreeIndiBorder;
            GMConfig.Instance.MiniTreeColourIndiBackground = fColorConfigMiniTreeIndiBackground;
            GMConfig.Instance.MiniTreeColourIndiHighlight = fColorConfigMiniTreeIndiHighlight;
            GMConfig.Instance.MiniTreeColourIndiBgConcealed = fColorConfigMiniTreeIndiBgConcealed;
            GMConfig.Instance.MiniTreeColourIndiFgConcealed = fColorConfigMiniTreeIndiFgConcealed;
            GMConfig.Instance.MiniTreeColourIndiShade = fColorConfigMiniTreeIndiShade;
            GMConfig.Instance.MiniTreeColourIndiText = fColorConfigMiniTreeIndiText;
            GMConfig.Instance.MiniTreeColourIndiLink = fColorConfigMiniTreeIndiLink;
            GMConfig.Instance.MiniTreeColourBackground = fColorConfigMiniTreeBackground;

            GMConfig.Instance.SupressBackreferences = !chkConfigSupressBackreferences.Checked;
        }

        // Populates the list box of individuals to link from the front page
        private void FillKeyIndividualsList()
        {
            if (GMConfig.Instance.KeyIndividuals == null) return;

            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();
            try {
                lstKeyIndividuals.Items.Clear();
                foreach (string xref in GMConfig.Instance.KeyIndividuals) {
                    GDMIndividualRecord indiRec = fBase.Context.Tree.FindXRef<GDMIndividualRecord>(xref);
                    if (indiRec != null && GMHelper.GetVisibility(indiRec)) {
                        string firstName, surname;
                        string fullName = GMHelper.CapitaliseName(indiRec.GetPrimaryPersonalName(), out firstName, out surname);
                        if (string.IsNullOrEmpty(fullName)) {
                            fullName = GMConfig.Instance.UnknownName;
                        }
                        lstKeyIndividuals.Items.Add(new NameXRefPair(fullName, xref));
                    }
                }
            } finally {
                EnableKeyIndividualsDeleteButton();
                Cursor.Current = Cursors.Default;
                Cursor.Hide();
            }
        }

        // Creates output directory, deleting old ones if required, leaving them place if required.
        // Returns OK if website creation can proceed, Cancel if user should be returned to main form, Retry to retry the process without returning to main form.
        // front_page_filename is the name of a file to survive throughout this process (see also s_config.m_preserveFrontPage)
        private DialogResult PrepareOutputDirectory(string outputFolder, bool preserveFiles)
        {
            fLogger.WriteInfo(string.Format("PrepareOutputDirectory({0})", outputFolder));

            string message = fLangMan.LS(PLS.LSID_FolderAccessError);
            MessageBoxButtons messageBoxButtons = MessageBoxButtons.RetryCancel;
            DialogResult dialogResult;
            bool failed;
            string exceptionMessage = "";

            // First see if folder clashes with a file
            if (File.Exists(outputFolder)) {
                fLogger.WriteInfo("Folder clashes with file : " + outputFolder);

                // Earn user that file is being deleted
                dialogResult = MessageBox.Show(this, string.Format(fLangMan.LS(PLS.LSID_FolderNeedsToBeCreated), outputFolder),
                    fLangMan.LS(PLS.LSID_CreatingWebsite), MessageBoxButtons.OKCancel, MessageBoxIcon.Exclamation);
                if (dialogResult == DialogResult.Cancel) {
                    fLogger.WriteInfo("Message box cancelled (2)");
                    return DialogResult.Cancel;
                }

                // Now delete output folder (which is currently a file)
                do {
                    failed = false;
                    fLogger.WriteInfo("Deleting output folder as file : " + outputFolder);
                    try {
                        Cursor.Current = Cursors.WaitCursor;
                        Cursor.Show();
                        File.Delete(outputFolder);
                    } catch (IOException e) {
                        fLogger.WriteError("Caught IO exception : ", e);
                        exceptionMessage = e.Message;
                        failed = true;
                    } catch (UnauthorizedAccessException e) {
                        fLogger.WriteError("Caught UnauthorizedAccessException(3) : ", e);
                        exceptionMessage = e.Message;
                        failed = true;
                    } catch (Exception e) {
                        fLogger.WriteError("Caught generic exception(3) : ", e);
                        exceptionMessage = e.Message;
                        failed = true;
                    }

                    if (failed) {
                        // Catch failure, e.g. if user has dir/file open elsewhere
                        message = string.Format(fLangMan.LS(PLS.LSID_FileCouldNotBeDeleted), outputFolder, exceptionMessage);
                        messageBoxButtons = MessageBoxButtons.RetryCancel;
                        dialogResult = MessageBox.Show(this, message, fLangMan.LS(PLS.LSID_CreatingWebsite), messageBoxButtons, MessageBoxIcon.Exclamation);
                    }
                }
                while (failed && dialogResult == DialogResult.Retry);

                if (failed && dialogResult == DialogResult.Cancel) {
                    fLogger.WriteInfo("Message box cancelled (6)");
                    return DialogResult.Cancel;
                }
            }

            // Next see if folder already exists
            // If output folder exists, offer to delete it
            if (Directory.Exists(outputFolder)) {
                fLogger.WriteInfo("Folder exists(11) : " + outputFolder);

                if (GMHelper.IsDesktop(outputFolder)) {
                    dialogResult = MessageBox.Show(this, fLangMan.LS(PLS.LSID_WillNotAllowToCreateFilesDirectlyOnDesktop),
                        fLangMan.LS(PLS.LSID_CreatingWebsite), MessageBoxButtons.OK, MessageBoxIcon.Stop);
                    fLogger.WriteInfo("Desktop detected as output folder.");
                    return DialogResult.Cancel;
                }

                // Warn user that file is being deleted
                dialogResult = MessageBox.Show(this, string.Format(fLangMan.LS(PLS.LSID_FolderAlreadyExists), outputFolder),
                    fLangMan.LS(PLS.LSID_CreatingWebsite), MessageBoxButtons.YesNoCancel, MessageBoxIcon.Question);
                if (dialogResult == DialogResult.Cancel) {
                    fLogger.WriteInfo("Message box cancelled (3a)");
                    return DialogResult.Cancel;
                }

                if (dialogResult == DialogResult.Yes) {
                    if (preserveFiles) {
                        dialogResult = MessageBox.Show(this, string.Format(fLangMan.LS(PLS.LSID_DeletingFolderWillNotPreserveAnyExistingFiles), outputFolder),
                            fLangMan.LS(PLS.LSID_CreatingWebsite), MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation);
                        if (dialogResult == DialogResult.Cancel) {
                            fLogger.WriteInfo("Message box cancelled (3b)");
                            return DialogResult.Cancel;
                        }
                    } else {
                        dialogResult = MessageBox.Show(this, fLangMan.LS(PLS.LSID_IfFolderContainsOtherFilesTheyWillBeDeletedAlso),
                            fLangMan.LS(PLS.LSID_CreatingWebsite), MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation);
                        if (dialogResult == DialogResult.Cancel) {
                            fLogger.WriteInfo("Message box cancelled (3c)");
                            return DialogResult.Cancel;
                        }
                    }

                    if (dialogResult == DialogResult.Yes) {
                        fLogger.WriteInfo("Message box yes. Deleting output folder.");
                        do {
                            failed = false;
                            try {
                                Cursor.Current = Cursors.WaitCursor;
                                Cursor.Show();
                                if (Directory.Exists(outputFolder)) {
                                    Directory.Delete(outputFolder, true);
                                }
                            } catch (IOException e) {
                                fLogger.WriteError("Caught IOException(2) : ", e);
                                exceptionMessage = e.Message;
                                failed = true;
                            } catch (UnauthorizedAccessException e) {
                                fLogger.WriteError("Caught UnauthorizedAccessException(2) : ", e);
                                exceptionMessage = e.Message;
                                failed = true;
                            } catch (Exception e) {
                                fLogger.WriteError("Caught generic exception(2) : ", e);
                                exceptionMessage = e.Message;
                                failed = true;
                            }
                            if (failed) {
                                // Catch failure, e.g. if user has dir/file open elsewhere
                                message = string.Format(fLangMan.LS(PLS.LSID_FolderCouldNotBeDeleted), outputFolder, exceptionMessage);
                                messageBoxButtons = MessageBoxButtons.RetryCancel;
                                dialogResult = MessageBox.Show(this, message, fLangMan.LS(PLS.LSID_CreatingWebsite),
                                    messageBoxButtons, MessageBoxIcon.Exclamation);
                            }
                        }
                        while (failed && dialogResult == DialogResult.Retry);

                        if (failed && dialogResult == DialogResult.Cancel) {
                            return DialogResult.Cancel;
                        }
                    }
                }
            }

            // At last, try to create the folder
            failed = false;
            fLogger.WriteInfo("Creating output folder.");
            try {
                Directory.CreateDirectory(outputFolder);
            }
            // Order of catches is important here, due to hierarchy of exception classes.
            catch (DirectoryNotFoundException e) {
                message = fLangMan.LS(PLS.LSID_FolderNotFound);
                exceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                fLogger.WriteError("Caught DirectoryNotFoundException(5) : ", e);
                failed = true;
            } catch (ArgumentNullException e) {
                message = fLangMan.LS(PLS.LSID_FolderNameIsIllegal);
                exceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                fLogger.WriteError("Caught ArgumentNullException(5) : ", e);
                failed = true;
            } catch (PathTooLongException e) {
                message = fLangMan.LS(PLS.LSID_FolderNameIsTooLong);
                exceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                fLogger.WriteError("Caught PathTooLongException(5) : ", e);
                failed = true;
            } catch (IOException e) {
                message = fLangMan.LS(PLS.LSID_PathIsReadonlyOrNotEmpty);
                exceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.RetryCancel; // Let them correct this outside of app
                fLogger.WriteError("Caught IOException(5) : ", e);
                failed = true;
            } catch (UnauthorizedAccessException e) {
                message = fLangMan.LS(PLS.LSID_NotHaveCorrectPermissionsToAccess);
                exceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.RetryCancel; // Let them correct this outside of app
                fLogger.WriteError("Caught UnauthorizedAccessException(5) : ", e);
                failed = true;
            } catch (ArgumentException e) {
                message = fLangMan.LS(PLS.LSID_FolderNameIsIllegalFormat);
                exceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                fLogger.WriteError("Caught ArgumentException(5) : ", e);
                failed = true;
            } catch (NotSupportedException e) {
                message = fLangMan.LS(PLS.LSID_FolderNameIsUnsupportedFormat);
                exceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                fLogger.WriteError("Caught NotSupportedException(5) : ", e);
                failed = true;
            }

            // Handle any failure with a sMessage box
            if (failed) {
                dialogResult = MessageBox.Show(this, string.Concat(message, "\r\n", exceptionMessage), fLangMan.LS(PLS.LSID_CreatingWebsite), messageBoxButtons, MessageBoxIcon.Exclamation);

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
            if (chkConfigMultiPageIndex.Checked) {
                lblConfigMultiPageIndexNumber.Enabled = true;
                txtConfigMultiPageIndexNumber.Enabled = true;
            } else {
                lblConfigMultiPageIndexNumber.Enabled = false;
                txtConfigMultiPageIndexNumber.Enabled = false;
            }
        }

        // Logic to enable the controls related to multimedia content
        private void EnableMultimediaConfig()
        {
            if (chkConfigAllowMultimedia.Checked) {
                lblConfigIndiImageSize.Enabled = true;
                lblConfigIndiImageWidth.Enabled = true;
                txtConfigIndiImageWidth.Enabled = true;
                lblConfigIndiImageHeight.Enabled = true;
                txtConfigIndiImageHeight.Enabled = true;
                lblConfigSourceImageSize.Enabled = true;
                lblConfigSourceImageWidth.Enabled = true;
                txtConfigSourceImageWidth.Enabled = true;
                lblConfigSourceImageHeight.Enabled = true;
                txtConfigSourceImageHeight.Enabled = true;
                chkConfigRenameOriginals.Enabled = true;
                chkConfigKeepOriginals.Enabled = true;
                chkConfigNonPictures.Enabled = true;
                chkConfigIndiImages.Enabled = true;

                EnableThumbnailsConfig();
            } else {
                lblConfigIndiImageSize.Enabled = false;
                lblConfigIndiImageWidth.Enabled = false;
                txtConfigIndiImageWidth.Enabled = false;
                lblConfigIndiImageHeight.Enabled = false;
                txtConfigIndiImageHeight.Enabled = false;
                lblConfigSourceImageSize.Enabled = false;
                lblConfigSourceImageWidth.Enabled = false;
                txtConfigSourceImageWidth.Enabled = false;
                lblConfigSourceImageHeight.Enabled = false;
                txtConfigSourceImageHeight.Enabled = false;
                chkConfigRenameOriginals.Enabled = false;
                chkConfigKeepOriginals.Enabled = false;
                chkConfigNonPictures.Enabled = false;
                chkConfigIndiImages.Enabled = false;
                lblConfigThumbnailImageSize.Enabled = false;
                lblConfigThumbnailImageWidth.Enabled = false;
                txtConfigThumbnailImageWidth.Enabled = false;
                lblConfigThumbnailImageHeight.Enabled = false;
                txtConfigThumbnailImageHeight.Enabled = false;
            }
        }

        // Logic to enable the controls related to thumbnails
        private void EnableThumbnailsConfig()
        {
            if (chkConfigIndiImages.Checked) {
                lblConfigThumbnailImageSize.Enabled = true;
                lblConfigThumbnailImageWidth.Enabled = true;
                txtConfigThumbnailImageWidth.Enabled = true;
                lblConfigThumbnailImageHeight.Enabled = true;
                txtConfigThumbnailImageHeight.Enabled = true;
            } else {
                lblConfigThumbnailImageSize.Enabled = false;
                lblConfigThumbnailImageWidth.Enabled = false;
                txtConfigThumbnailImageWidth.Enabled = false;
                lblConfigThumbnailImageHeight.Enabled = false;
                txtConfigThumbnailImageHeight.Enabled = false;
            }
        }

        // Logic to enable the controls related to withheld records
        private void EnableWithheldConfig()
        {
            if (chkConfigShowWithheldRecords.Checked) {
                gbConfigWithheldName.Enabled = true;
                radConfigWithheldNameLabel.Enabled = true;
                txtConfigWithheldName.Enabled = radConfigWithheldNameLabel.Checked;
                radConfigWithheldNameName.Enabled = true;
            } else {
                gbConfigWithheldName.Enabled = false;
                radConfigWithheldNameLabel.Enabled = false;
                txtConfigWithheldName.Enabled = false;
                radConfigWithheldNameName.Enabled = false;
            }
        }

        // Logic to enable the save changes button
        private void EnablePrunePanelButtons()
        {
        }
    }
}
