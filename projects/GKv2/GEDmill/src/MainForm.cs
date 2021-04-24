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
using System.Threading;
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

namespace GEDmill
{
    /// <summary>
    /// The main from from which the application is operated. Contains the GUI controls and the control handlers.
    /// </summary>
    public partial class MainForm : Form, ILocalization
    {
        private static readonly GKCore.Logging.ILogger fLogger = LogManager.GetLogger(CConfig.LOG_FILE, CConfig.LOG_LEVEL, typeof(MainForm).Name);

        private readonly Plugin fPlugin;
        private IBaseWindow fBase;

        // Specifies which panel of the wizard the user is viewing (i.e. which stage in the app they are at)
        private int fCurrentPanel;

        // Application has an important state whereby it displays the settings panes. 
        // The main GUI navigation buttons behave differently in this mode.
        private bool fConfigPanelOn;

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

            fLogger.WriteInfo(CConfig.SoftwareName + " started at " + DateTime.Now.ToString());

            // Set some values that scale the size of the GUI
            fDefaultButtonSize = new Point(75, 23);
            fConfigButtonSize = new Point(92, 23);

            // Read back any previously stored settings.
            CConfig.Instance.Load();

            helpProvider.SetHelpKeyword(btnHelp, "HelpButtonHelpKeyword");
            helpProvider.SetHelpNavigator(btnHelp, HelpNavigator.TableOfContents);
            helpProvider.SetShowHelp(btnHelp, true);
            helpProvider.HelpNamespace = CConfig.Instance.ApplicationPath + "\\" + CConfig.HelpFilename;

            fCurrentPanel = 1;
            fConfigPanelOn = false;
            fDisableRestrictsCheckEvent = false;
        }

        public MainForm(Plugin plugin) : this()
        {
            fPlugin = plugin;
            SetLang();
            ShowCurrentPanel();
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

        #region ILocalization support

        public void SetLang()
        {
            Text = fPlugin.LangMan.LS(PLS.LSID_Title);
            btnCancel.Text = fPlugin.LangMan.LS(PLS.LSID_Quit);
            lblWelcomeVersion.Text = fPlugin.LangMan.LS(PLS.LSID_Version) + " " + CConfig.SoftwareVersion;
            btnBack.Text = fPlugin.LangMan.LS(PLS.LSID_Back);
            btnHelp.Text = fPlugin.LangMan.LS(PLS.LSID_Help);
            btnSettings.Text = fPlugin.LangMan.LS(PLS.LSID_Settings);
            btnSettingsCancel.Text = fPlugin.LangMan.LS(PLS.LSID_Cancel);
        }

        #endregion

        #region Event handlers

        private void btnBack_Click(object sender, EventArgs e)
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

        private void btnNext_click(object sender, EventArgs e)
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
            btnNext.Enabled = false;
            btnBack.Enabled = false;
            btnCancel.Enabled = false;
            btnSettings.Enabled = false;
            btnHelp.Enabled = false;

            if (ValidateCurrentPanel()) {
                if (fCurrentPanel < 9) // Allow for extra ftp panels
                    ++fCurrentPanel;
                else {
                    CConfig.Instance.Save();

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
                btnNext.Enabled = true;
            }

            // Back button and quit button are always enabled
            btnBack.Enabled = true;
            btnCancel.Enabled = true;
            btnSettings.Enabled = true;
            btnHelp.Enabled = true;

            EnableCurrentPanel(true);
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
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
                    CConfig.Instance.OpenWebsiteOnExit = chkAllDoneShowSite.Checked;
                }

                if (fCurrentPanel == 6) {
                    // Finish button is the only time we want to launch webpages
                    if (CConfig.Instance.OpenWebsiteOnExit && CConfig.Instance.FrontPageFilename.Length > 0) {
                        GMHelper.OpenURL(CConfig.Instance.FrontPageURL);
                    }
                }

                CConfig.Instance.Save();
                Close();
            }
        }

        private void btnHelp_click(object sender, EventArgs e)
        {
            string helpFile = CConfig.Instance.ApplicationPath + "\\" + CConfig.HelpFilename;

            if (fConfigPanelOn) {
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
                    case 2:
                        Help.ShowHelp(btnHelp, helpFile, HelpNavigator.Topic, "SelectingInputFile_1.htm");
                        break;
                    case 3:
                        Help.ShowHelp(btnHelp, helpFile, HelpNavigator.Topic, "ExcludingPeople_2.htm");
                        break;
                    case 4:
                        Help.ShowHelp(btnHelp, helpFile, HelpNavigator.Topic, "SetTheTitle_3.htm");
                        break;
                    case 5:
                        Help.ShowHelp(btnHelp, helpFile, HelpNavigator.Topic, "SelectingOutputFile_4.htm");
                        break;
                    case 6:
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

        private void btnSettingsCancel_Click(object sender, EventArgs e)
        {
            // Ensure config panel on
            if (!fConfigPanelOn) return;

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
            GDMIndividualRecord indiRec = fBase.Context.SelectPerson(null, TargetMode.tmNone, GDMSex.svUnknown);
            if (indiRec == null) return;

            // Ensure they are only added once
            bool alreadyAdded = CConfig.Instance.KeyIndividuals.Contains(indiRec.XRef);
            if (!alreadyAdded) {
                CConfig.Instance.KeyIndividuals.Add(indiRec.XRef);
                FillKeyIndividualsList();
            }
        }

        private void btnSelectKeyDelete_Click(object sender, EventArgs e)
        {
            NameXRefPair xrefPairName = lstKeyIndividuals.SelectedItem as NameXRefPair;
            if (xrefPairName != null) {
                string xref = xrefPairName.XRef;
                if (!string.IsNullOrEmpty(xref)) {
                    CConfig.Instance.KeyIndividuals.Remove(xref);
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
            string backFile = AppHost.StdDialogs.GetOpenFile("Select back image", sPath, GMHelper.GfxFilter, 1, "");
            if (!string.IsNullOrEmpty(backFile) && GMHelper.IsSupportedFile(backFile)) {
                txtConfigBackImageEdit.Text = backFile;
                txtConfigBackImageEdit.SelectAll();
            }
        }

        private void btnConfigFrontImageBrowse_Click(object sender, EventArgs e)
        {
            string sPath = GMHelper.GetInitialDirectory(txtConfigFrontImageEdit.Text);
            string frontFile = AppHost.StdDialogs.GetOpenFile("Select front image", sPath, GMHelper.GfxFilter, 1, "");
            if (!string.IsNullOrEmpty(frontFile) && GMHelper.IsSupportedFile(frontFile)) {
                txtConfigFrontImageEdit.Text = frontFile;
                txtConfigFrontImageEdit.SelectAll();
            }
        }

        private void chkConfigTreeDiagrams_Click(object sender, EventArgs e)
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
            fLogger.WriteInfo("config panel multi page index button clicked.");
            EnableMultiPageIndexConfig();
        }

        private void chkConfigAllowMultimedia_Click(object sender, EventArgs e)
        {
            fLogger.WriteInfo("allow multimedia button clicked.");
            EnableMultimediaConfig();
        }

        private void chkConfigIndiImages_Click(object sender, EventArgs e)
        {
            fLogger.WriteInfo("config panel multi images button clicked.");
            EnableThumbnailsConfig();
        }

        private void chkConfigShowWithheldRecords_Click(object sender, EventArgs e)
        {
            fLogger.WriteInfo("config panel show withheld records button clicked.");
            EnableWithheldConfig();
        }

        private void radConfigWithheldNameLabel_Click(object sender, EventArgs e)
        {
            fLogger.WriteInfo("config panel withheld label clicked.");
            EnableWithheldConfig();
        }

        private void miIndividualDetails_Click(Object sender, EventArgs e)
        {
            if (lvIndividuals.SelectedItems.Count == 1) {
                var lvi = lvIndividuals.SelectedItems[0] as LVItem;
                var ir = lvi.Record as GDMIndividualRecord;
                BaseController.ViewRecordInfo(fBase, ir);
            }
        }

        private void miSourceDetails_Click(Object sender, EventArgs e)
        {
            if (lvSources.SelectedItems.Count == 1) {
                var lvi = lvSources.SelectedItems[0] as LVItem;
                var sr = lvi.Record as GDMSourceRecord;
                BaseController.ViewRecordInfo(fBase, sr);
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

            ShowRestrictsResult(recordsExcluded, recordsIncluded, "individual");
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

            ShowRestrictsResult(recordsExcluded, recordsIncluded, "individual");
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

            ShowRestrictsResult(recordsExcluded, recordsIncluded, "individual");
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

            ShowRestrictsResult(recordsExcluded, recordsIncluded, "individual");
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

            ShowRestrictsResult(recordsExcluded, recordsIncluded, "individual");
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

            ShowRestrictsResult(0, recordsIncluded, "individual");
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

            ShowRestrictsResult(recordsExcluded, 0, "individual");
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

            ShowRestrictsResult(0, recordsIncluded, "source");
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

            ShowRestrictsResult(recordsExcluded, 0, "source");
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

            ShowRestrictsResult(recordsExcluded, recordsIncluded, "individual");
        }

        private void menuIndividuals_Popup(Object sender, EventArgs e)
        {
            int nSelected = lvIndividuals.SelectedItems.Count;
            miUnconnectedExclude.Enabled = (nSelected > 0);
            if (nSelected <= 1) {
                miUnconnectedExclude.Text = "E&xclude individuals unless navigable from this person";
            } else {
                miUnconnectedExclude.Text = string.Format("E&xclude individuals unless navigable from these {0} people", nSelected);
            }

            miIndiDescendantsExclude.Enabled = (nSelected == 1);
            miIndiDescendantsInclude.Enabled = (nSelected == 1);
            miIndiAncestorsExclude.Enabled = (nSelected == 1);
            miIndiAncestorsInclude.Enabled = (nSelected == 1);
            miIndividualDetails.Enabled = (nSelected == 1);
        }

        private void menuSources_Popup(Object sender, EventArgs e)
        {
            int nSelected = lvSources.SelectedItems.Count;
            miSourceDetails.Enabled = (nSelected == 1);
            miSourceRemovePics.Enabled = (nSelected > 0);
            if (nSelected <= 1) {
                miSourceRemovePics.Text = "&Remove pictures from this source";
            } else {
                miSourceRemovePics.Text = string.Format("&Remove pictures from these {0} sources", nSelected);
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

            // Move help button to its new location
            btnHelp.Location = new Point(8, 288);

            // Flag panel as being on
            fConfigPanelOn = true;

            // Enable reset button
            btnSettingsCancel.Visible = true;

            // Disable buttons while config panel shown
            btnNext.Visible = false;
            btnBack.Visible = false;
            btnCancel.Visible = false; // To give the panel a "modal" feeling

            // Make config button an "OK" button
            btnSettings.Text = fPlugin.LangMan.LS(PLS.LSID_Ok);
            btnSettings.Location = new Point(344, 288);
            btnSettings.Size = new Size(fDefaultButtonSize);

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
            btnHelp.Location = new Point(186, 288);

            // Flag panel as being off
            fConfigPanelOn = false;

            // Restore buttons states
            // Done by ShowCurrentPanel()

            // Make config button back to a config button
            btnSettings.Text = fPlugin.LangMan.LS(PLS.LSID_Settings);
            btnSettings.Location = new Point(88, 288);
            btnSettings.Size = new Size(fConfigButtonSize);

            // Enable generic panel
            EnableCurrentPanel(true);

            // Disable reset button
            btnSettingsCancel.Visible = false;

            // ShowCurrentPanel() also restores visibility of back button.
            ShowCurrentPanel();
            btnNext.Visible = true;
            btnCancel.Visible = true;

            // Back button should always be enabled (tho' sometimes not bVisible!)
            btnBack.Enabled = true;
        }

        // Shows the current panel and associated wizard buttons, selected by m_currentPanel, and hides all the others.
        private void ShowCurrentPanel()
        {
            // Making panel3 bVisible calls check event on list view!
            fDisableRestrictsCheckEvent = true;

            if (fConfigPanelOn) {
                panelWelcome.Visible = false;
                panelChooseGedcom.Visible = false;
                panelRecords.Visible = false;
                panelKeyIndividuals.Visible = false;
                panelChooseOutput.Visible = false;
                panelAllDone.Visible = false;
                tabcontrolConfigPanel.Visible = true;
            } else {
                panelWelcome.Visible = (fCurrentPanel == 1);
                panelChooseGedcom.Visible = (fCurrentPanel == 2);
                panelRecords.Visible = (fCurrentPanel == 3);
                panelKeyIndividuals.Visible = (fCurrentPanel == 4);
                panelChooseOutput.Visible = (fCurrentPanel == 5);
                panelAllDone.Visible = (fCurrentPanel == 6);
                tabcontrolConfigPanel.Visible = false;

                if (fCurrentPanel <= 1) {
                    btnBack.Visible = false;
                } else {
                    btnBack.Visible = true;
                }

                // Config button disappears once html created
                if (fCurrentPanel >= 6) {
                    btnSettings.Visible = false;
                } else {
                    btnSettings.Visible = true;
                }

                if (fCurrentPanel == 6) {
                    btnCancel.Text = fPlugin.LangMan.LS(PLS.LSID_Finish);
                    // Can't go back , because we can't undo the file creations.
                    btnBack.Visible = false;
                    btnHelp.Location = new Point(8, 288);
                    btnCancel.Location = new Point(424, 288);
                    btnNext.Visible = false;
                } else if (fCurrentPanel == 9) {
                    btnHelp.Location = new Point(8, 288);
                    btnNext.Text = fPlugin.LangMan.LS(PLS.LSID_Finish);
                    btnCancel.Visible = false;
                    btnHelp.Visible = false;
                    // Can't go back , because we can't undo the file creations.
                    btnBack.Visible = false;
                } else if (fCurrentPanel == 2) {
                } else if (fCurrentPanel == 4) {
                    txtSelectKey.Focus();
                    txtSelectKey.SelectAll();
                } else if (fCurrentPanel == 5) {
                    txtChooseOutput.Focus();
                    txtChooseOutput.SelectAll();
                } else {
                    btnNext.Text = fPlugin.LangMan.LS(PLS.LSID_Next);
                    btnCancel.Visible = true;
                    btnCancel.Text = fPlugin.LangMan.LS(PLS.LSID_Quit);
                    btnCancel.Location = new Point(8, 288);
                    btnHelp.Visible = true;
                }

                if (fCurrentPanel == 3) {
                    EnablePrunePanelButtons();
                }

                EnableNextButton();
            }

            fDisableRestrictsCheckEvent = false;
        }

        // Logic for the next page button to ensure that user has completed the current page
        private void EnableNextButton()
        {
            if (fCurrentPanel == 2) {
                btnNext.Enabled = true;
            } else if (fCurrentPanel == 5 && txtChooseOutput.Text.Length == 0) {
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
            bool bEnabled = chkConfigTreeDiagrams.Checked;

            chkConfigTreeDiagramsFakeBg.Enabled = bEnabled;
            lblConfigTreeDiagramsFormat.Enabled = bEnabled;
            cmbConfigTreeDiagramsFormat.Enabled = bEnabled;
            gbMiniTreeColours.Enabled = bEnabled;
            btnConfigMiniTreeColourIndiBackground.Enabled = bEnabled;
            btnConfigMiniTreeColourIndiHighlight.Enabled = bEnabled;
            btnConfigMiniTreeColourIndiBgConcealed.Enabled = bEnabled;
            btnConfigMiniTreeColourIndiShade.Enabled = bEnabled;
            btnConfigMiniTreeColourIndiText.Enabled = bEnabled;
            btnConfigMiniTreeColourIndiLink.Enabled = bEnabled;
            btnConfigMiniTreeColourBranch.Enabled = bEnabled;
            btnConfigMiniTreeColourIndiBorder.Enabled = bEnabled;
            btnConfigMiniTreeColourIndiFgConcealed.Enabled = bEnabled;
            chkConfigConserveTreeWidth.Enabled = bEnabled;
            chkConfigKeepSiblingOrder.Enabled = bEnabled;

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
                    break;

                case 4:
                    txtSelectKey.Text = CConfig.Instance.SiteTitle;
                    CConfig.Instance.FirstRecordXRef = "";
                    FillKeyIndividualsList();
                    break;

                case 5:
                    txtChooseOutput.Text = CConfig.Instance.OutputFolder;
                    break;

                case 6:
                    chkAllDoneShowSite.Visible = File.Exists(CConfig.Instance.FrontPageURL);
                    chkAllDoneShowSite.Checked = CConfig.Instance.OpenWebsiteOnExit;
                    lblAllDone.Text = CConfig.Instance.OutputFolder;
                    if (CConfig.Instance.FrontPageFilename != "") {
                        lblAllDoneStartFile.Text = string.Concat("(The front page for the website is the file ", CConfig.Instance.FrontPageFilename, ".", CConfig.Instance.HtmlExtension, ")");
                        lblAllDoneStartFile.Visible = true;
                    } else {
                        lblAllDoneStartFile.Text = "(No front page was generated.)";
                        lblAllDoneStartFile.Visible = false;
                    }
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
                    case 1:
                        CConfig.Instance.InputFilename = fBase.Context.FileName;
                        fLogger.WriteInfo("Selected file : " + CConfig.Instance.InputFilename);
                        return true;

                    case 2:
                        if (CConfig.Instance.OutputFolder == "") {
                            CConfig.Instance.OutputFolder = Path.GetDirectoryName(CConfig.Instance.InputFilename);
                            CConfig.Instance.OutputFolder += "\\GEDmill_Output";
                        }
                        CConfig.Instance.FirstRecordXRef = "";
                        CConfig.Instance.KeyIndividuals = new List<string>();
                        CConfig.Instance.FirstRecordXRef = "";
                        FillIndividualsList();
                        FillSourcesList();
                        return true;

                    case 3:
                        // Go through individuals list and set restricted flag as appropriate
                        bool somethingChecked = false;
                        foreach (ListViewItem li in lvIndividuals.Items) {
                            bool isChecked = li.Checked;
                            if (isChecked) {
                                somethingChecked = true;
                            }
                            // Already done on click event: ((CListableBool)li).SetRestricted( !bChecked );
                            if (!isChecked) {
                                GMHelper.RestrictAssociatedSources(fBase.Context.Tree, (GDMIndividualRecord)((LVItem)li).Record);
                            }
                        }

                        if (somethingChecked == false) {
                            MessageBox.Show(this, "Please select at least one individual.", "No Individuals Selected",
                                MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                            return false;
                        }

                        /*if (PrunepanelDataChanged) {
                            DialogResult dialogResult = MessageBox.Show(this, "You have made changes which will affect the website but have not saved them.\r\nWould you like to save them now?", "Save changes",
                                MessageBoxButtons.YesNo, MessageBoxIcon.Question);
                            if (dialogResult == DialogResult.Yes) {
                                buttonPruneRecordsSave_click(null, null);
                            }
                        }*/
                        return true;

                    case 4:
                        CConfig.Instance.SiteTitle = txtSelectKey.Text;
                        return true;

                    case 5:
                        CConfig.Instance.OutputFolder = txtChooseOutput.Text;
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
        private void FillIndividualsList()
        {
            var indiRecs = fBase.Context.Tree.GetRecords<GDMIndividualRecord>();
            fLogger.WriteInfo("FillIndividualsList() : " + indiRecs.Count.ToString());

            fDisableRestrictsCheckEvent = true;

            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            lvIndividuals.Clear();

            lvIndividuals.View = View.Details;
            int nameWidth = lvIndividuals.Width - 70 - 70 - 20;
            lvIndividuals.Columns.Add("Include", 30, HorizontalAlignment.Left);
            nameWidth -= 30;
            lvIndividuals.Columns.Add("Name", nameWidth, HorizontalAlignment.Left);
            lvIndividuals.Columns.Add("Born", 70, HorizontalAlignment.Left);
            lvIndividuals.Columns.Add("Died", 70, HorizontalAlignment.Left);
            lvIndividuals.Columns.Add("Id", 60, HorizontalAlignment.Left);
            lvIndividuals.Columns.Add("User ref", 78, HorizontalAlignment.Left);
            lvIndividuals.Columns.Add("Pics", 48, HorizontalAlignment.Left);

            // Build an array first then blit the whole array to the list control. This is faster than adding each item to the list control individually.
            ListViewItem[] temporaryItemsList = new ListViewItem[indiRecs.Count];

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

            pageIndividuals.Text = "Individuals (" + lvIndividuals.Items.Count + ")";

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
                string surname = "";
                string firstName = "";
                var persName = (ir.PersonalNames.Count > 0) ? ir.PersonalNames[0].StringValue : "";
                GMHelper.CapitaliseName(persName, ref firstName, ref surname);
                /*if (ir.NameSuffix != null && ir.NameSuffix != "") {
                    sFirstName += ", " + ir.NameSuffix;
                }*/
                lvItem.SubItems.Add(new LVNameItem(surname, firstName));
            }

            var lifeDatesX = ir.GetLifeDates();
            var birthDate = (lifeDatesX.BirthEvent == null) ? 0 : lifeDatesX.BirthEvent.Date.GetChronologicalYear();
            var deathDate = (lifeDatesX.DeathEvent == null) ? 0 : lifeDatesX.DeathEvent.Date.GetChronologicalYear();

            lvItem.SubItems.Add(new LVNumberItem(birthDate, false));
            lvItem.SubItems.Add(new LVNumberItem(deathDate, false));
            lvItem.SubItems.Add(new LVStringItem(ir.XRef));

            string uref = (ir.UserReferences.Count > 0) ? ir.UserReferences[0].StringValue : "";
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
            fLogger.WriteInfo("FillSourcesList() : " + sources.Count.ToString());

            fDisableRestrictsCheckEvent = true; // call to item.Checked below invokes event handler.

            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            lvSources.Clear();

            lvSources.View = View.Details;
            int nWidthTitle = lvSources.Width - 140 - 20;
            lvSources.Columns.Add("Include", 30, HorizontalAlignment.Left);
            nWidthTitle -= 30;
            lvSources.Columns.Add("Title", nWidthTitle, HorizontalAlignment.Left);
            lvSources.Columns.Add("Repository", 100, HorizontalAlignment.Left);
            lvSources.Columns.Add("Citations", 60, HorizontalAlignment.Left);
            lvSources.Columns.Add("B", 30, HorizontalAlignment.Left);
            lvSources.Columns.Add("M", 30, HorizontalAlignment.Left);
            lvSources.Columns.Add("D", 30, HorizontalAlignment.Left);
            lvSources.Columns.Add("Id", 60, HorizontalAlignment.Left);
            lvSources.Columns.Add("Pics", 48, HorizontalAlignment.Left);

            ListViewItem[] temporaryItemsList = new ListViewItem[sources.Count];
            int nItem = 0;
            foreach (GDMSourceRecord sr in sources) {
                var item = new LVItem(sr);
                SetSourceSubItems(item, sr, true);
                item.Checked = GMHelper.GetVisibility(sr);
                temporaryItemsList[nItem++] = item;
            }

            lvSources.Items.AddRange(temporaryItemsList);
            lvSources.Sort();

            pageSources.Text = "Sources (" + lvSources.Items.Count + ")";

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

            var progressWindow = new ProgressWindow();
            progressWindow.Text = "Creating web pages";

            Website website = new Website(fBase.Context.Tree, progressWindow);

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
                tabcontrolConfigPanel.Enabled = enable;
                // Disable all other panels
                enable = false;
            } else {
                panelWelcome.Enabled = (fCurrentPanel == 1 && enable);
                panelChooseGedcom.Enabled = (fCurrentPanel == 2 && enable);
                panelRecords.Enabled = (fCurrentPanel == 3 && enable);
                panelKeyIndividuals.Enabled = (fCurrentPanel == 4 && enable);
                panelChooseOutput.Enabled = (fCurrentPanel == 5 && enable);
                panelAllDone.Enabled = (fCurrentPanel == 6 && enable);

                tabcontrolConfigPanel.Enabled = false;
            }

            pictureBox.Enabled = enable;
        }

        // Reports any exception thrown during the prune operation
        private void ReportRestrictError(Exception e)
        {
            MessageBox.Show(this, string.Format("A problem was encountered while navigating the tree structure:\r\n\r\n{0}", e.StackTrace), CConfig.SoftwareName,
                MessageBoxButtons.OK, MessageBoxIcon.Exclamation);

            fLogger.WriteInfo(string.Format("Caught navigation exception : {0}", e.ToString()));
        }

        // Displays the statistics of the prune operation
        private void ShowRestrictsResult(int excluded, int included, string type)
        {
            string msg = "";
            if (excluded == 0 && included == 0) {
                msg = "No changes made.";
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
            MessageBox.Show(this, msg, "Select Records", MessageBoxButtons.OK, MessageBoxIcon.Information);
        }

        // Displays the statistics of the remove pictures operation
        private void ShowHidePicsResult(int hidden)
        {
            string msg = "";
            if (hidden == 0) {
                msg = "No multimedia files hidden.";
            } else {
                if (hidden != 0) {
                    msg = string.Format("{0} multimedia file{1} hidden.", hidden, hidden > 1 ? "s" : "");
                }
            }
            MessageBox.Show(this, msg, "Hide Pictures", MessageBoxButtons.OK, MessageBoxIcon.Information);
        }

        // Initialises config panel controls
        private void LoadConfigPanelSettings()
        {
            txtConfigFrontImageEdit.Text = CConfig.Instance.FrontPageImageFilename;
            txtConfigFrontImageEdit.SelectionStart = txtConfigFrontImageEdit.Text.Length;
            txtConfigFrontImageEdit.SelectionLength = txtConfigFrontImageEdit.Text.Length;
            txtConfigBackImageEdit.Text = CConfig.Instance.BackgroundImage;
            txtConfigBackImageEdit.SelectionStart = txtConfigBackImageEdit.Text.Length;
            txtConfigBackImageEdit.SelectionLength = txtConfigBackImageEdit.Text.Length;
            txtConfigIndiImageWidth.Text = CConfig.Instance.MaxImageWidth.ToString();
            txtConfigIndiImageHeight.Text = CConfig.Instance.MaxImageHeight.ToString();
            txtConfigSourceImageWidth.Text = CConfig.Instance.MaxSourceImageWidth.ToString();
            txtConfigSourceImageHeight.Text = CConfig.Instance.MaxSourceImageHeight.ToString();
            txtConfigThumbnailImageWidth.Text = CConfig.Instance.MaxThumbnailImageWidth.ToString();
            txtConfigThumbnailImageHeight.Text = CConfig.Instance.MaxThumbnailImageHeight.ToString();
            cmbConfigHtmlExtn.Items.Clear();
            cmbConfigHtmlExtn.Items.AddRange(new object[] { ".htm", ".html" });
            cmbConfigHtmlExtn.SelectedIndex = (CConfig.Instance.HtmlExtension == "html" ? 1 : 0);
            txtConfigNoName.Text = CConfig.Instance.UnknownName;
            txtConfigWithheldName.Text = CConfig.Instance.ConcealedName;
            radConfigWithheldNameLabel.Checked = !CConfig.Instance.UseWithheldNames;
            radConfigWithheldNameName.Checked = CConfig.Instance.UseWithheldNames;
            chkConfigCapNames.Checked = (CConfig.Instance.NameCapitalisation == 1);
            chkConfigCapEvents.Checked = CConfig.Instance.CapitaliseEventDescriptions;
            chkConfigHideEmails.Checked = CConfig.Instance.ObfuscateEmails;
            chkConfigOccupationHeadline.Checked = CConfig.Instance.OccupationHeadline;
            chkConfigShowWithheldRecords.Checked = CConfig.Instance.OnlyConceal;
            txtConfigTabSpaces.Text = CConfig.Instance.TabSpaces.ToString();
            txtConfigCommentary.Text = CConfig.Instance.CommentaryText;
            chkConfigCommentaryIsHtml.Checked = CConfig.Instance.CommentaryIsHtml;
            chkConfigStats.Checked = CConfig.Instance.ShowFrontPageStats;
            chkConfigPreserveFrontPage.Checked = CConfig.Instance.PreserveFrontPage;
            chkConfigPreserveStylesheet.Checked = CConfig.Instance.PreserveStylesheet;
            chkConfigIncludeHelppage.Checked = CConfig.Instance.IncludeHelpPage;
            chkConfigCdrom.Checked = CConfig.Instance.CreateCDROMFiles;
            chkConfigNonPictures.Checked = CConfig.Instance.AllowNonPictures;
            chkConfigIndiImages.Checked = CConfig.Instance.AllowMultipleImages;
            chkConfigTreeDiagrams.Checked = CConfig.Instance.ShowMiniTrees;
            chkConfigTreeDiagramsFakeBg.Checked = CConfig.Instance.FakeMiniTreeTransparency;
            txtConfigEmail.Text = CConfig.Instance.UserEmailAddress;
            txtConfigIndexName.Text = CConfig.Instance.FrontPageFilename;
            lblConfigIndexNameExtn.Text = "." + CConfig.Instance.HtmlExtension;
            txtConfigStylesheetName.Text = CConfig.Instance.StylesheetFilename;
            if (CConfig.Instance.MainWebsiteLink.Length == 0) {
                txtConfigUserLink.Text = "http://";
            } else {
                txtConfigUserLink.Text = CConfig.Instance.MainWebsiteLink;
            }
            cmbConfigTreeDiagramsFormat.Items.Clear();
            cmbConfigTreeDiagramsFormat.Items.AddRange(new object[] { "gif", "png" });
            cmbConfigTreeDiagramsFormat.SelectedIndex = (CConfig.Instance.MiniTreeImageFormat == "png" ? 1 : 0);
            chkConfigMultiPageIndex.Checked = CConfig.Instance.MultiPageIndexes;
            chkConfigUserRefInIndex.Checked = CConfig.Instance.IncludeUserRefInIndex;
            txtConfigMultiPageIndexNumber.Text = CConfig.Instance.IndividualsPerIndexPage.ToString();
            chkConfigKeepOriginals.Checked = CConfig.Instance.LinkOriginalPicture;
            chkConfigRenameOriginals.Checked = CConfig.Instance.RenameOriginalPicture;
            chkConfigW3C.Checked = CConfig.Instance.IncludeValiditySticker;
            chkConfigUserRecFilename.Checked = CConfig.Instance.UserRecFilename;
            txtConfigCustomFooter.Text = CConfig.Instance.CustomFooter;
            chkConfigFooterIsHtml.Checked = CConfig.Instance.FooterIsHtml;
            chkConfigConserveTreeWidth.Checked = CConfig.Instance.ConserveTreeWidth;
            chkConfigKeepSiblingOrder.Checked = CConfig.Instance.KeepSiblingOrder;
            chkConfigAllowMultimedia.Checked = CConfig.Instance.AllowMultimedia;

            fColorConfigMiniTreeBranch = CConfig.Instance.MiniTreeColourBranch;
            fColorConfigMiniTreeIndiBorder = CConfig.Instance.MiniTreeColourIndiBorder;
            fColorConfigMiniTreeIndiBackground = CConfig.Instance.MiniTreeColourIndiBackground;
            fColorConfigMiniTreeIndiHighlight = CConfig.Instance.MiniTreeColourIndiHighlight;
            fColorConfigMiniTreeIndiBgConcealed = CConfig.Instance.MiniTreeColourIndiBgConcealed;
            fColorConfigMiniTreeIndiFgConcealed = CConfig.Instance.MiniTreeColourIndiFgConcealed;
            fColorConfigMiniTreeIndiShade = CConfig.Instance.MiniTreeColourIndiShade;
            fColorConfigMiniTreeIndiText = CConfig.Instance.MiniTreeColourIndiText;
            fColorConfigMiniTreeIndiLink = CConfig.Instance.MiniTreeColourIndiLink;
            fColorConfigMiniTreeBackground = CConfig.Instance.MiniTreeColourBackground;

            chkConfigSupressBackreferences.Checked = !CConfig.Instance.SupressBackreferences;

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
            CConfig.Instance.FrontPageImageFilename = txtConfigFrontImageEdit.Text;
            CConfig.Instance.BackgroundImage = txtConfigBackImageEdit.Text;

            try {
                // Sanity check value
                int maxImageWidth = int.Parse(txtConfigIndiImageWidth.Text);
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
                int maxImageHeight = int.Parse(txtConfigIndiImageHeight.Text);
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
                int maxSourceImageWidth = int.Parse(txtConfigSourceImageWidth.Text);
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
                int maxSourceImageHeight = int.Parse(txtConfigSourceImageHeight.Text);
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
                int maxThumbnailImageWidth = int.Parse(txtConfigThumbnailImageWidth.Text);
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
                int maxThumbnailImageHeight = int.Parse(txtConfigThumbnailImageHeight.Text);
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

            CConfig.Instance.HtmlExtension = (cmbConfigHtmlExtn.SelectedIndex == 1 ? "html" : "htm");
            CConfig.Instance.IncludeValiditySticker = chkConfigW3C.Checked;
            CConfig.Instance.UserRecFilename = chkConfigUserRecFilename.Checked;
            if (txtConfigNoName.Text.Length > 0) {
                CConfig.Instance.UnknownName = txtConfigNoName.Text;
            }
            if (txtConfigWithheldName.Text.Length > 0) {
                CConfig.Instance.ConcealedName = txtConfigWithheldName.Text;
            }
            CConfig.Instance.UseWithheldNames = radConfigWithheldNameName.Checked;
            CConfig.Instance.NameCapitalisation = (chkConfigCapNames.Checked ? 1 : 0);
            CConfig.Instance.CapitaliseEventDescriptions = chkConfigCapEvents.Checked;
            CConfig.Instance.ObfuscateEmails = chkConfigHideEmails.Checked;
            CConfig.Instance.OccupationHeadline = chkConfigOccupationHeadline.Checked;
            CConfig.Instance.OnlyConceal = chkConfigShowWithheldRecords.Checked;

            try {
                // Sanity check value
                int tabSpaces = int.Parse(txtConfigTabSpaces.Text);
                if (tabSpaces > 0 && tabSpaces < 64) {
                    CConfig.Instance.TabSpaces = tabSpaces;
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            CConfig.Instance.CommentaryText = txtConfigCommentary.Text;
            CConfig.Instance.CommentaryIsHtml = chkConfigCommentaryIsHtml.Checked;
            CConfig.Instance.ShowFrontPageStats = chkConfigStats.Checked;
            CConfig.Instance.ShowMiniTrees = chkConfigTreeDiagrams.Checked;
            CConfig.Instance.FakeMiniTreeTransparency = chkConfigTreeDiagramsFakeBg.Checked;
            CConfig.Instance.UserEmailAddress = txtConfigEmail.Text;
            CConfig.Instance.PreserveFrontPage = chkConfigPreserveFrontPage.Checked;
            CConfig.Instance.PreserveStylesheet = chkConfigPreserveStylesheet.Checked;
            CConfig.Instance.IncludeHelpPage = chkConfigIncludeHelppage.Checked;
            CConfig.Instance.CreateCDROMFiles = chkConfigCdrom.Checked;
            CConfig.Instance.AllowMultipleImages = chkConfigIndiImages.Checked;
            CConfig.Instance.AllowNonPictures = chkConfigNonPictures.Checked;
            CConfig.Instance.LinkOriginalPicture = chkConfigKeepOriginals.Checked;
            CConfig.Instance.RenameOriginalPicture = chkConfigRenameOriginals.Checked;

            // Validate and strip trailing .html or .htm in case user has put them on
            string frontPageFilename = txtConfigIndexName.Text;
            string frontPageFilenameUpper = frontPageFilename.ToUpper();
            if (frontPageFilenameUpper.LastIndexOf(".HTML") >= 0) {
                frontPageFilename = frontPageFilename.Substring(0, frontPageFilename.Length - 5);
            } else if (frontPageFilenameUpper.LastIndexOf(".HTM") >= 0) {
                frontPageFilename = frontPageFilename.Substring(0, frontPageFilename.Length - 4);
            }
            CConfig.Instance.FrontPageFilename = frontPageFilename;

            // Validate and strip trailing .css in case user has put them on
            string stylesheetFilename = txtConfigStylesheetName.Text;
            if (stylesheetFilename.Length > 0) {
                string stylesheetFilenameUpper = stylesheetFilename.ToUpper();
                if (stylesheetFilename.LastIndexOf(".CSS") >= 0) {
                    stylesheetFilename = stylesheetFilename.Substring(0, stylesheetFilename.Length - 4);
                }
                CConfig.Instance.StylesheetFilename = stylesheetFilename;
            }

            // Validate and strip leading http:// in case user has it them on
            string mainWebsiteLink = txtConfigUserLink.Text;
            if (mainWebsiteLink.ToLower() == "http://") {
                // User hasn't altered default value
                mainWebsiteLink = "";
            }

            CConfig.Instance.MainWebsiteLink = mainWebsiteLink;
            CConfig.Instance.MiniTreeImageFormat = (cmbConfigTreeDiagramsFormat.SelectedIndex == 1 ? "png" : "gif");
            CConfig.Instance.MultiPageIndexes = chkConfigMultiPageIndex.Checked;
            CConfig.Instance.IncludeUserRefInIndex = chkConfigUserRefInIndex.Checked;

            try {
                // Sanity check value
                int index = int.Parse(txtConfigMultiPageIndexNumber.Text);
                if (index > 0) {
                    CConfig.Instance.IndividualsPerIndexPage = index;
                }
            } catch (Exception) {
                // Leave value unchanged
            }

            CConfig.Instance.CustomFooter = txtConfigCustomFooter.Text;

            CConfig.Instance.FooterIsHtml = chkConfigFooterIsHtml.Checked;
            CConfig.Instance.ConserveTreeWidth = chkConfigConserveTreeWidth.Checked;
            CConfig.Instance.KeepSiblingOrder = chkConfigKeepSiblingOrder.Checked;
            CConfig.Instance.AllowMultimedia = chkConfigAllowMultimedia.Checked;

            CConfig.Instance.MiniTreeColourBranch = fColorConfigMiniTreeBranch;
            CConfig.Instance.MiniTreeColourIndiBorder = fColorConfigMiniTreeIndiBorder;
            CConfig.Instance.MiniTreeColourIndiBackground = fColorConfigMiniTreeIndiBackground;
            CConfig.Instance.MiniTreeColourIndiHighlight = fColorConfigMiniTreeIndiHighlight;
            CConfig.Instance.MiniTreeColourIndiBgConcealed = fColorConfigMiniTreeIndiBgConcealed;
            CConfig.Instance.MiniTreeColourIndiFgConcealed = fColorConfigMiniTreeIndiFgConcealed;
            CConfig.Instance.MiniTreeColourIndiShade = fColorConfigMiniTreeIndiShade;
            CConfig.Instance.MiniTreeColourIndiText = fColorConfigMiniTreeIndiText;
            CConfig.Instance.MiniTreeColourIndiLink = fColorConfigMiniTreeIndiLink;
            CConfig.Instance.MiniTreeColourBackground = fColorConfigMiniTreeBackground;

            CConfig.Instance.SupressBackreferences = !chkConfigSupressBackreferences.Checked;
        }

        // Populates the list box of individuals to link from the front page
        private void FillKeyIndividualsList()
        {
            if (CConfig.Instance.KeyIndividuals == null) return;
            fLogger.WriteInfo("FillKeyIndividualsList() : " + CConfig.Instance.KeyIndividuals.Count.ToString());

            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();
            try {
                lstKeyIndividuals.Items.Clear();
                foreach (string xref in CConfig.Instance.KeyIndividuals) {
                    GDMIndividualRecord indiRec = fBase.Context.Tree.FindXRef<GDMIndividualRecord>(xref);
                    if (indiRec != null && GMHelper.GetVisibility(indiRec)) {
                        string firstName = "";
                        string surname = "";
                        string fullName = GMHelper.CapitaliseName(indiRec.GetPrimaryFullName(), ref firstName, ref surname);
                        if (string.IsNullOrEmpty(fullName)) {
                            fullName = CConfig.Instance.UnknownName;
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

            string message = "Could not access or create folder.";
            MessageBoxButtons messageBoxButtons = MessageBoxButtons.RetryCancel;
            DialogResult dialogResult = DialogResult.OK;
            bool failed = false;
            string exceptionMessage = "";

            // First see if folder clashes with a file
            if (File.Exists(outputFolder)) {
                fLogger.WriteInfo("Folder clashes with file : " + outputFolder);

                // Earn user that file is being deleted
                dialogResult = MessageBox.Show(this, string.Format("The folder {0} needs to be created. " +
                    "\r\nThis will destroy an existing file with that name.", outputFolder),
                    "Creating website", MessageBoxButtons.OKCancel, MessageBoxIcon.Exclamation);
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
                        message = string.Format("The file {0} could not be deleted.\r\n{1}", outputFolder, exceptionMessage);
                        messageBoxButtons = MessageBoxButtons.RetryCancel;
                        dialogResult = MessageBox.Show(this, message, "Creating website", messageBoxButtons, MessageBoxIcon.Exclamation);
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
                    dialogResult = MessageBox.Show(this, "GEDmill will not allow you to create files directly on the Desktop",
                                                   "Creating website", MessageBoxButtons.OK, MessageBoxIcon.Stop);
                    fLogger.WriteInfo("Desktop detected as output folder.");
                    return DialogResult.Cancel;
                }

                // Warn user that file is being deleted
                dialogResult = MessageBox.Show(this, string.Format("The folder {0} already exists.\r\nWould you like to delete any files it contains before creating the website files?", outputFolder),
                    "Creating website",
                    MessageBoxButtons.YesNoCancel, MessageBoxIcon.Question);
                if (dialogResult == DialogResult.Cancel) {
                    fLogger.WriteInfo("Message box cancelled (3a)");
                    return DialogResult.Cancel;
                }

                if (dialogResult == DialogResult.Yes) {
                    if (preserveFiles) {
                        dialogResult = MessageBox.Show(this, string.Format("WARNING: Deleting the folder {0} will not preserve any existing front page and stylesheet files.\r\nDelete folder anyway?", outputFolder),
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
                                message = string.Format("The folder {0} could not be deleted.\r\n{1}", outputFolder, exceptionMessage);
                                messageBoxButtons = MessageBoxButtons.RetryCancel;
                                dialogResult = MessageBox.Show(this, message, "Creating website",
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
            DirectoryInfo directoryInfo = null;
            fLogger.WriteInfo("Creating output folder.");
            try {
                directoryInfo = Directory.CreateDirectory(outputFolder);
            }
            // Order of catches is important here, due to hierarchy of exception classes.
            catch (DirectoryNotFoundException e) {
                message = "The folder you have selected could not be found.";
                exceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                fLogger.WriteError("Caught DirectoryNotFoundException(5) : ", e);
                failed = true;
            } catch (ArgumentNullException e) {
                message = "The folder name is missing or illegal.";
                exceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                fLogger.WriteError("Caught ArgumentNullException(5) : ", e);
                failed = true;
            } catch (PathTooLongException e) {
                message = "The folder name you have selected is too long.";
                exceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                fLogger.WriteError("Caught PathTooLongException(5) : ", e);
                failed = true;
            } catch (IOException e) {
                message = "The path you have selected is read-only, or the folder is not empty.";
                exceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.RetryCancel; // Let them correct this outside of app
                fLogger.WriteError("Caught IOException(5) : ", e);
                failed = true;
            } catch (UnauthorizedAccessException e) {
                message = "You do not have the correct permissions to access the folder.";
                exceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.RetryCancel; // Let them correct this outside of app
                fLogger.WriteError("Caught UnauthorizedAccessException(5) : ", e);
                failed = true;
            } catch (ArgumentException e) {
                message = "The folder name you have selected is of an illegal format.";
                exceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                fLogger.WriteError("Caught ArgumentException(5) : ", e);
                failed = true;
            } catch (NotSupportedException e) {
                message = "The folder name you have selected is of an unsupported format.";
                exceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                fLogger.WriteError("Caught NotSupportedException(5) : ", e);
                failed = true;
            }

            // Handle any failure with a sMessage box
            if (failed) {
                dialogResult = MessageBox.Show(this, string.Concat(message, "\r\n", exceptionMessage), "Creating website", messageBoxButtons, MessageBoxIcon.Exclamation);

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
