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
using System.Collections;
using System.Drawing;
using System.IO;
using System.Threading;
using System.Windows.Forms;
using GEDmill.Exceptions;
using GEDmill.HTML;
using GEDmill.ListView;
using GEDmill.MiniTree;
using GKCommon.GEDCOM;

namespace GEDmill
{
    /// <summary>
    /// The main from from which the application is operated. Contains the GUI controls and the control handlers.
    /// </summary>
    public partial class MainForm : Form
    {
        // The current version of this app, for display purposes.
        public static string SoftwareVersion = "1.11.0";

        // The name of the app for display purposes
        public static string SoftwareName = "GEDmill " + SoftwareVersion;

        // Effectively a global reference to the app's GUI object!
        public static MainForm m_mainForm = null;

        // Stores user's configuration parameters (mostly set using Settings panes)
        public static CConfig Config;

        // Filename for the online help (as in "on the same system", as opposed to offline e.g. printed manual)
        public static string HelpFilename = "GEDmill Help.chm";


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


        // Constructor. Initialise and create GUI.
        public MainForm(bool resetConfig)
        {
            // Set some values that scale the size of the GUI
            fDefaultButtonSize = new System.Drawing.Point(75, 23);
            fConfigButtonSize = new System.Drawing.Point(92, 23);

            fColorDialogConfigMiniTree = new ColorDialog();
            fColorDialogConfigMiniTree.FullOpen = true;
            fColorDialogConfigMiniTree.SolidColorOnly = true;

            Config = new CConfig();
            if (!resetConfig) {
                // Read back any previously stored settings.
                Config.RecoverSettings();
            } else {
                // Save default settings without neeeding user to complete app
                Config.StoreSettings();
            }

            // Creates the entire GUI
            InitializeComponent();

            m_helpProvider.HelpNamespace = Config.ApplicationPath + "\\" + HelpFilename;

            fTree = new GEDCOMTree();
            fCurrentPanel = 1;
            fConfigPanelOn = false;
            PruneExcluded = 0;
            PruneExcluded = 0;

            CListableName.UnknownName = Config.UnknownName;
            CListableBool.UnknownName = Config.UnknownName;

            PrunepanelDataChanged = false;
            fDisablePrunepanelCheckEvent = false;

            ShowCurrentPanel();
        }

        // Clean up any resources being used.
        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (components != null) {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        // Builds the GUI for the webpages pane in the config panel
        private void InitialiseSettingsWebpagesPane()
        {
            // 
            // configPanel_Commentary_Label (Webpages)
            // 
            m_labelConfigCommentary.Location = new System.Drawing.Point(9, 0);
            m_labelConfigCommentary.Name = "m_labelConfigCommentary";
            m_labelConfigCommentary.RightToLeft = RightToLeft.No;
            m_labelConfigCommentary.Size = new System.Drawing.Size(200, 24);
            m_labelConfigCommentary.TabIndex = 1;
            m_labelConfigCommentary.Text = "Commentary for &title page:";
            m_labelConfigCommentary.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_Commentary_EditBox (Webpages)
            // 
            m_textboxConfigCommentary.Location = new System.Drawing.Point(9, 26);
            m_textboxConfigCommentary.Name = "m_textboxConfigCommentary";
            m_textboxConfigCommentary.Size = new System.Drawing.Size(240, 70);
            m_textboxConfigCommentary.TabIndex = 2;
            m_textboxConfigCommentary.Text = "";
            m_textboxConfigCommentary.Multiline = true;

            // 
            // configPanel_CommentaryIsHtml_Label (Webpages)
            // 
            m_labelConfigCommentaryIsHtml.Location = new System.Drawing.Point(9, 91);
            m_labelConfigCommentaryIsHtml.Name = "m_labelConfigCommentaryIsHtml";
            m_labelConfigCommentaryIsHtml.RightToLeft = RightToLeft.No;
            m_labelConfigCommentaryIsHtml.Size = new System.Drawing.Size(8, 24);
            m_labelConfigCommentaryIsHtml.TabIndex = 3;
            m_labelConfigCommentaryIsHtml.Text = "(";
            m_labelConfigCommentaryIsHtml.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_CommentaryIsHtml_CheckBox (Webpages)
            // 
            m_checkboxConfigCommentaryIsHtml.Location = new System.Drawing.Point(19, 96);
            m_checkboxConfigCommentaryIsHtml.Name = "m_checkboxConfigCommentaryIsHtml";
            m_checkboxConfigCommentaryIsHtml.Size = new System.Drawing.Size(190, 24);
            m_checkboxConfigCommentaryIsHtml.TabIndex = 4;
            m_checkboxConfigCommentaryIsHtml.Text = "the a&bove text is HTML)";

            // 
            // configPanel_UserLink_Label (Webpages)
            // 
            m_labelConfigUserLink.Location = new System.Drawing.Point(9, 121);
            m_labelConfigUserLink.Name = "m_labelConfigUserLink";
            m_labelConfigUserLink.RightToLeft = RightToLeft.No;
            m_labelConfigUserLink.Size = new System.Drawing.Size(260, 24);
            m_labelConfigUserLink.TabIndex = 5;
            m_labelConfigUserLink.Text = "&Link to your website: (with http:// prefix)";
            m_labelConfigUserLink.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_UserLink_EditBox (Webpages)
            // 
            m_textboxConfigUserLink.Location = new System.Drawing.Point(9, 147);
            m_textboxConfigUserLink.Name = "m_textboxConfigUserLink";
            m_textboxConfigUserLink.Size = new System.Drawing.Size(240, 20);
            m_textboxConfigUserLink.TabIndex = 7;
            m_textboxConfigUserLink.Text = "";
            m_textboxConfigUserLink.Multiline = false;

            // 
            // configPanel_CustomFooter_Label (Webpages)
            // 
            m_labelConfigCustomFooter.Location = new System.Drawing.Point(9, 172);
            m_labelConfigCustomFooter.Name = "m_labelConfigCustomFooter";
            m_labelConfigCustomFooter.RightToLeft = RightToLeft.No;
            m_labelConfigCustomFooter.Size = new System.Drawing.Size(224, 24);
            m_labelConfigCustomFooter.TabIndex = 8;
            m_labelConfigCustomFooter.Text = "Te&xt for page footer:";
            m_labelConfigCustomFooter.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_CustomFooter_EditBox (Webpages)
            //
            m_textboxConfigCustomFooter.Location = new System.Drawing.Point(9, 198);
            m_textboxConfigCustomFooter.Name = "m_textboxConfigCustomFooter";
            m_textboxConfigCustomFooter.Size = new System.Drawing.Size(200, 20);
            m_textboxConfigCustomFooter.Text = "";
            m_textboxConfigCustomFooter.TabIndex = 9;

            // 
            // configPanel_FooterIsHtml_Label (Webpages)
            // 
            m_labelConfigFooterIsHtml.Location = new System.Drawing.Point(9, 213);
            m_labelConfigFooterIsHtml.Name = "m_labelConfigFooterIsHtml";
            m_labelConfigFooterIsHtml.RightToLeft = RightToLeft.No;
            m_labelConfigFooterIsHtml.Size = new System.Drawing.Size(8, 24);
            m_labelConfigFooterIsHtml.TabIndex = 10;
            m_labelConfigFooterIsHtml.Text = "(";
            m_labelConfigFooterIsHtml.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_FooterIsHtml_CheckBox (Webpages)
            // 
            m_checkboxConfigFooterIsHtml.Location = new System.Drawing.Point(19, 218);
            m_checkboxConfigFooterIsHtml.Name = "m_checkboxConfigFooterIsHtml";
            m_checkboxConfigFooterIsHtml.Size = new System.Drawing.Size(190, 24);
            m_checkboxConfigFooterIsHtml.TabIndex = 11;
            m_checkboxConfigFooterIsHtml.Text = "the abo&ve text is HTML)";

            //
            // configPanel_Stats_CheckBox (Webpages)
            // 
            m_checkboxConfigStats.Location = new System.Drawing.Point(266, 7);
            m_checkboxConfigStats.Name = "m_checkboxConfigStats";
            m_checkboxConfigStats.Size = new System.Drawing.Size(200, 20);
            m_checkboxConfigStats.Text = "Include website &statistics";
            m_checkboxConfigStats.TabIndex = 12;

            //
            // configPanel_CDROM_CheckBox (Webpages)
            // 
            m_checkboxConfigCdrom.Location = new System.Drawing.Point(266, 30);
            m_checkboxConfigCdrom.Name = "m_checkboxConfigCdrom";
            m_checkboxConfigCdrom.Size = new System.Drawing.Size(200, 20);
            m_checkboxConfigCdrom.Text = "Create CD-ROM &auto-run files";
            m_checkboxConfigCdrom.TabIndex = 13;

            //
            // configPanel_MultiPageIndex_CheckBox (Webpages)
            // 
            m_checkboxConfigMultiPageIndex.Location = new System.Drawing.Point(266, 53);
            m_checkboxConfigMultiPageIndex.Name = "m_checkboxConfigMultiPageIndex";
            m_checkboxConfigMultiPageIndex.Size = new System.Drawing.Size(220, 20);
            m_checkboxConfigMultiPageIndex.Text = "&Multi-page individuals index";
            m_checkboxConfigMultiPageIndex.TabIndex = 14;
            m_checkboxConfigMultiPageIndex.Click += new System.EventHandler(configPanel_MultiPageIndex_CheckBox_click);

            //
            // configPanel_UserRefInIndex_CheckBox (Webpages)
            //
            m_checkboxConfigUserRefInIndex.Location = new System.Drawing.Point(266, 76);
            m_checkboxConfigUserRefInIndex.Name = "m_checkboxConfigUserRefInIndex";
            m_checkboxConfigUserRefInIndex.Size = new System.Drawing.Size(220, 20);
            m_checkboxConfigUserRefInIndex.Text = "&User Reference numbers in index";
            m_checkboxConfigUserRefInIndex.TabIndex = 15;

            // 
            // configPanel_MultiPageIndexNumber_Label (Webpages)
            // 
            m_labelConfigMultiPageIndexNumber.Location = new System.Drawing.Point(266, 96);
            m_labelConfigMultiPageIndexNumber.Name = "m_labelConfigMultiPageIndexNumber";
            m_labelConfigMultiPageIndexNumber.RightToLeft = RightToLeft.No;
            m_labelConfigMultiPageIndexNumber.Size = new System.Drawing.Size(170, 24);
            m_labelConfigMultiPageIndexNumber.TabIndex = 16;
            m_labelConfigMultiPageIndexNumber.Text = "&Individuals per index page:";
            m_labelConfigMultiPageIndexNumber.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_MultiPageIndexNumber_TextBox (Webpages)
            // 
            m_textboxConfigMultiPageIndexNumber.Location = new System.Drawing.Point(446, 100);
            m_textboxConfigMultiPageIndexNumber.Name = "m_textboxConfigMultiPageIndexNumber";
            m_textboxConfigMultiPageIndexNumber.Size = new System.Drawing.Size(45, 20);
            m_textboxConfigMultiPageIndexNumber.TabIndex = 17;
            m_textboxConfigMultiPageIndexNumber.Text = "";

            // 
            // configPanel_IndexName_Label (Webpages)
            // 
            m_labelConfigIndexName.Location = new System.Drawing.Point(266, 126);
            m_labelConfigIndexName.Name = "m_labelConfigIndexName";
            m_labelConfigIndexName.RightToLeft = RightToLeft.No;
            m_labelConfigIndexName.Size = new System.Drawing.Size(224, 20);
            m_labelConfigIndexName.TabIndex = 18;
            m_labelConfigIndexName.Text = "Name of &front page file:";
            m_labelConfigIndexName.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_IndexName_EditBox (Webpages)
            // 
            m_textboxConfigIndexName.Location = new System.Drawing.Point(266, 148);
            m_textboxConfigIndexName.Name = "m_textboxConfigIndexName";
            m_textboxConfigIndexName.Size = new System.Drawing.Size(175, 20);
            m_textboxConfigIndexName.TabIndex = 19;
            m_textboxConfigIndexName.Text = "";
            m_textboxConfigIndexName.Multiline = false;

            // 
            // configPanel_IndexName_ExtnLabel (Webpages)
            // 
            m_labelConfigIndexNameExtn.Location = new System.Drawing.Point(440, 141);
            m_labelConfigIndexNameExtn.Name = "m_labelConfigIndexNameExtn";
            m_labelConfigIndexNameExtn.RightToLeft = RightToLeft.No;
            m_labelConfigIndexNameExtn.Size = new System.Drawing.Size(60, 24);
            m_labelConfigIndexNameExtn.TabIndex = 20;
            m_labelConfigIndexNameExtn.Text = ""; //Filled programatically
            m_labelConfigIndexNameExtn.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_PreserveFrontPage_CheckBox (Webpages)
            // 
            m_checkboxConfigPreserveFrontPage.Location = new System.Drawing.Point(266, 170);
            m_checkboxConfigPreserveFrontPage.Name = "m_checkboxConfigPreserveFrontPage";
            m_checkboxConfigPreserveFrontPage.Size = new System.Drawing.Size(250, 20);
            m_checkboxConfigPreserveFrontPage.Text = "&Do not generate new front page";
            m_checkboxConfigPreserveFrontPage.TabIndex = 21;

            // 
            // configPanel_Email_Label (Webpages)
            // 
            m_labelConfigEmail.Location = new System.Drawing.Point(266, 190);
            m_labelConfigEmail.Name = "m_labelConfigEmail";
            m_labelConfigEmail.RightToLeft = RightToLeft.No;
            m_labelConfigEmail.Size = new System.Drawing.Size(220, 24);
            m_labelConfigEmail.TabIndex = 22;
            m_labelConfigEmail.Text = "&Email address to put on front page:";
            m_labelConfigEmail.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_Email_EditBox (Webpages)
            // 
            m_textboxConfigEmail.Location = new System.Drawing.Point(266, 216);
            m_textboxConfigEmail.Name = "m_textboxConfigEmail";
            m_textboxConfigEmail.Size = new System.Drawing.Size(220, 20);
            m_textboxConfigEmail.TabIndex = 23;
            m_textboxConfigEmail.Text = "";
            m_textboxConfigEmail.Multiline = false;
        }

        // Builds the GUI for the images pane in the config panel
        private void InitialiseSettingsImagesPane()
        {
            // 
            // configPanel_BackImage_EditLabel (Images)
            // 
            this.m_labelConfigBackImageEdit.Location = new System.Drawing.Point(9, 0);
            this.m_labelConfigBackImageEdit.Name = "m_labelConfigBackImageEdit";
            this.m_labelConfigBackImageEdit.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.m_labelConfigBackImageEdit.Size = new System.Drawing.Size(156, 24);
            this.m_labelConfigBackImageEdit.TabIndex = 1;
            this.m_labelConfigBackImageEdit.Text = "&Background image:";
            this.m_labelConfigBackImageEdit.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_BackImage_EditBox (Images)
            // 
            this.m_textboxConfigBackImageEdit.Location = new System.Drawing.Point(9, 26);
            this.m_textboxConfigBackImageEdit.Name = "m_textboxConfigBackImageEdit";
            this.m_textboxConfigBackImageEdit.Size = new System.Drawing.Size(191, 20);
            this.m_textboxConfigBackImageEdit.TabIndex = 2;
            this.m_textboxConfigBackImageEdit.Text = "";

            // 
            // configPanel_BackImage_BrowseButton (Images)
            // 
            this.m_buttonConfigBackImageBrowse.Location = new System.Drawing.Point(208, 25);
            this.m_buttonConfigBackImageBrowse.Name = "m_buttonConfigBackImageBrowse";
            this.m_buttonConfigBackImageBrowse.TabIndex = 3;
            this.m_buttonConfigBackImageBrowse.Text = "B&rowse...";
            this.m_buttonConfigBackImageBrowse.Click += new System.EventHandler(this.configPanel_BackImage_BrowseButton_click);

            // 
            // configPanel_FrontImage_EditLabel (Images)
            // 
            this.m_labelConfigFrontImageEdit.Location = new System.Drawing.Point(9, 46);
            this.m_labelConfigFrontImageEdit.Name = "m_labelConfigFrontImageEdit";
            this.m_labelConfigFrontImageEdit.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.m_labelConfigFrontImageEdit.Size = new System.Drawing.Size(156, 20);
            this.m_labelConfigFrontImageEdit.TabIndex = 4;
            this.m_labelConfigFrontImageEdit.Text = "&Picture on front page:";
            this.m_labelConfigFrontImageEdit.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_FrontImage_EditBox (Images)
            // 
            this.m_textboxConfigFrontImageEdit.Location = new System.Drawing.Point(9, 68);
            this.m_textboxConfigFrontImageEdit.Name = "m_textboxConfigFrontImageEdit";
            this.m_textboxConfigFrontImageEdit.Size = new System.Drawing.Size(191, 20);
            this.m_textboxConfigFrontImageEdit.TabIndex = 5;
            this.m_textboxConfigFrontImageEdit.Text = "";

            // 
            // configPanel_FrontImage_BrowseButton (Images)
            // 
            this.m_buttonConfigFrontImageBrowse.Location = new System.Drawing.Point(208, 68);
            this.m_buttonConfigFrontImageBrowse.Name = "m_buttonConfigFrontImageBrowse";
            this.m_buttonConfigFrontImageBrowse.TabIndex = 6;
            this.m_buttonConfigFrontImageBrowse.Text = "Br&owse...";
            this.m_buttonConfigFrontImageBrowse.Click += new System.EventHandler(this.configPanel_FrontImage_BrowseButton_click);

            // 
            // configPanel_IndiImageSize_Label (Images)
            // 
            this.m_labelConfigIndiImageSize.Location = new System.Drawing.Point(9, 108);
            this.m_labelConfigIndiImageSize.Name = "m_labelConfigIndiImageSize";
            this.m_labelConfigIndiImageSize.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.m_labelConfigIndiImageSize.Size = new System.Drawing.Size(256, 24);
            this.m_labelConfigIndiImageSize.TabIndex = 7;
            this.m_labelConfigIndiImageSize.Text = "Maximum size of individual images";
            this.m_labelConfigIndiImageSize.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            // 
            // configPanel_IndiImageWidth_Label (Images)
            // 
            this.m_labelConfigIndiImageWidth.Location = new System.Drawing.Point(9, 138);
            this.m_labelConfigIndiImageWidth.Name = "m_labelConfigIndiImageWidth";
            this.m_labelConfigIndiImageWidth.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.m_labelConfigIndiImageWidth.Size = new System.Drawing.Size(50, 24);
            this.m_labelConfigIndiImageWidth.TabIndex = 8;
            this.m_labelConfigIndiImageWidth.Text = "&Width:";
            this.m_labelConfigIndiImageWidth.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_IndiImageWidth_EditBox (Images)
            // 
            this.m_textboxConfigIndiImageWidth.Location = new System.Drawing.Point(61, 138);
            this.m_textboxConfigIndiImageWidth.Name = "m_textboxConfigIndiImageWidth";
            this.m_textboxConfigIndiImageWidth.Size = new System.Drawing.Size(34, 20);
            this.m_textboxConfigIndiImageWidth.TabIndex = 9;
            this.m_textboxConfigIndiImageWidth.Text = "";

            // 
            // configPanel_IndiImageHeight_Label (Images)
            // 
            this.m_labelConfigIndiImageHeight.Location = new System.Drawing.Point(109, 138);
            this.m_labelConfigIndiImageHeight.Name = "m_labelConfigIndiImageHeight";
            this.m_labelConfigIndiImageHeight.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.m_labelConfigIndiImageHeight.Size = new System.Drawing.Size(50, 24);
            this.m_labelConfigIndiImageHeight.TabIndex = 10;
            this.m_labelConfigIndiImageHeight.Text = "&Height:";
            this.m_labelConfigIndiImageHeight.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_IndiImageHeight_EditBox (Images)
            // 
            this.m_textboxConfigIndiImageHeight.Location = new System.Drawing.Point(162, 138);
            this.m_textboxConfigIndiImageHeight.Name = "m_textboxConfigIndiImageHeight";
            this.m_textboxConfigIndiImageHeight.Size = new System.Drawing.Size(34, 20);
            this.m_textboxConfigIndiImageHeight.TabIndex = 11;
            this.m_textboxConfigIndiImageHeight.Text = "";

            // 
            // configPanel_SourceImageSize_Label (Images)
            // 
            this.m_labelConfigSourceImageSize.Location = new System.Drawing.Point(9, 167);
            this.m_labelConfigSourceImageSize.Name = "m_labelConfigSourceImageSize";
            this.m_labelConfigSourceImageSize.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.m_labelConfigSourceImageSize.Size = new System.Drawing.Size(256, 24);
            this.m_labelConfigSourceImageSize.TabIndex = 12;
            this.m_labelConfigSourceImageSize.Text = "Maximum size of source images";
            this.m_labelConfigSourceImageSize.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            // 
            // configPanel_SourceImageWidth_Label (Images)
            // 
            this.m_labelConfigSourceImageWidth.Location = new System.Drawing.Point(9, 193);
            this.m_labelConfigSourceImageWidth.Name = "m_labelConfigSourceImageWidth";
            this.m_labelConfigSourceImageWidth.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.m_labelConfigSourceImageWidth.Size = new System.Drawing.Size(50, 24);
            this.m_labelConfigSourceImageWidth.TabIndex = 13;
            this.m_labelConfigSourceImageWidth.Text = "W&idth:";
            this.m_labelConfigSourceImageWidth.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_SourceImageWidth_EditBox (Images)
            // 
            this.m_textboxConfigSourceImageWidth.Location = new System.Drawing.Point(60, 197);
            this.m_textboxConfigSourceImageWidth.Name = "m_textboxConfigSourceImageWidth";
            this.m_textboxConfigSourceImageWidth.Size = new System.Drawing.Size(34, 20);
            this.m_textboxConfigSourceImageWidth.TabIndex = 14;
            this.m_textboxConfigSourceImageWidth.Text = "";

            // 
            // configPanel_SourceImageHeight_Label (Images)
            // 
            this.m_labelConfigSourceImageHeight.Location = new System.Drawing.Point(109, 193);
            this.m_labelConfigSourceImageHeight.Name = "m_labelConfigSourceImageHeight";
            this.m_labelConfigSourceImageHeight.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.m_labelConfigSourceImageHeight.Size = new System.Drawing.Size(50, 24);
            this.m_labelConfigSourceImageHeight.TabIndex = 15;
            this.m_labelConfigSourceImageHeight.Text = "H&eight:";
            this.m_labelConfigSourceImageHeight.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_SourceImageHeight_EditBox (Images)
            // 
            this.m_textboxConfigSourceImageHeight.Location = new System.Drawing.Point(162, 197);
            this.m_textboxConfigSourceImageHeight.Name = "m_textboxConfigSourceImageHeight";
            this.m_textboxConfigSourceImageHeight.Size = new System.Drawing.Size(34, 20);
            this.m_textboxConfigSourceImageHeight.TabIndex = 16;
            this.m_textboxConfigSourceImageHeight.Text = "";

            //
            // configPanel_AllowMultimedia_CheckBox (Images)
            // 
            this.m_checkboxConfigAllowMultimedia.Location = new System.Drawing.Point(300, 8);
            this.m_checkboxConfigAllowMultimedia.Name = "m_checkboxConfigAllowMultimedia";
            this.m_checkboxConfigAllowMultimedia.Size = new System.Drawing.Size(190, 24);
            this.m_checkboxConfigAllowMultimedia.TabIndex = 5;
            this.m_checkboxConfigAllowMultimedia.Text = "&Allow images etc.";
            this.m_checkboxConfigAllowMultimedia.Click += new System.EventHandler(this.configPanel_AllowMultimedia_CheckBox_click);

            //
            // configPanel_RenameOriginals_CheckBox (Images)
            // 
            this.m_checkboxConfigRenameOriginals.Location = new System.Drawing.Point(300, 38);
            this.m_checkboxConfigRenameOriginals.Name = "m_checkboxConfigRenameOriginals";
            this.m_checkboxConfigRenameOriginals.Size = new System.Drawing.Size(200, 30);
            this.m_checkboxConfigRenameOriginals.Text = "Re&name files";
            this.m_checkboxConfigRenameOriginals.TabIndex = 17;

            //
            // configPanel_KeepOriginals_CheckBox (Images)
            // 
            this.m_checkboxConfigKeepOriginals.Location = new System.Drawing.Point(300, 64);
            this.m_checkboxConfigKeepOriginals.Name = "m_checkboxConfigKeepOriginals";
            this.m_checkboxConfigKeepOriginals.Size = new System.Drawing.Size(200, 40);
            this.m_checkboxConfigKeepOriginals.Text = "In&clude original (full-size) files";
            this.m_checkboxConfigKeepOriginals.TabIndex = 18;

            //
            // configPanel_NonPictures_CheckBox (Images)
            // 
            this.m_checkboxConfigNonPictures.Location = new System.Drawing.Point(266, 120);
            this.m_checkboxConfigNonPictures.Name = "m_checkboxConfigNonPictures";
            this.m_checkboxConfigNonPictures.Size = new System.Drawing.Size(200, 20);
            this.m_checkboxConfigNonPictures.Text = "&Allow files other than pictures";
            this.m_checkboxConfigNonPictures.TabIndex = 19;

            //
            // configPanel_IndiImages_CheckBox (Images)
            // 
            this.m_checkboxConfigIndiImages.Location = new System.Drawing.Point(266, 147);
            this.m_checkboxConfigIndiImages.Name = "m_checkboxConfigIndiImages";
            this.m_checkboxConfigIndiImages.Size = new System.Drawing.Size(200, 20);
            this.m_checkboxConfigIndiImages.Text = "&Multiple individual images";
            this.m_checkboxConfigIndiImages.TabIndex = 20;
            this.m_checkboxConfigIndiImages.Click += new System.EventHandler(this.configPanel_IndiImages_CheckBox_click);

            // 
            // configPanel_ThumbnailImageSize_Label (Images)
            // 
            this.m_labelConfigThumbnailImageSize.Location = new System.Drawing.Point(266, 167);
            this.m_labelConfigThumbnailImageSize.Name = "m_labelConfigThumbnailImageSize";
            this.m_labelConfigThumbnailImageSize.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.m_labelConfigThumbnailImageSize.Size = new System.Drawing.Size(256, 24);
            this.m_labelConfigThumbnailImageSize.TabIndex = 21;
            this.m_labelConfigThumbnailImageSize.Text = "Maximum size of thumbnail images";
            this.m_labelConfigThumbnailImageSize.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            // 
            // configPanel_ThumbnailImageWidth_Label (Images)
            // 
            this.m_labelConfigThumbnailImageWidth.Location = new System.Drawing.Point(266, 193);
            this.m_labelConfigThumbnailImageWidth.Name = "m_labelConfigThumbnailImageWidth";
            this.m_labelConfigThumbnailImageWidth.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.m_labelConfigThumbnailImageWidth.Size = new System.Drawing.Size(50, 24);
            this.m_labelConfigThumbnailImageWidth.TabIndex = 22;
            this.m_labelConfigThumbnailImageWidth.Text = "Wid&th:";
            this.m_labelConfigThumbnailImageWidth.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_ThumbnailImageWidth_EditBox (Images)
            // 
            this.m_textboxConfigThumbnailImageWidth.Location = new System.Drawing.Point(317, 197);
            this.m_textboxConfigThumbnailImageWidth.Name = "m_textboxConfigThumbnailImageWidth";
            this.m_textboxConfigThumbnailImageWidth.Size = new System.Drawing.Size(34, 20);
            this.m_textboxConfigThumbnailImageWidth.TabIndex = 23;
            this.m_textboxConfigThumbnailImageWidth.Text = "";

            // 
            // configPanel_ThumbnailImageHeight_Label (Images)
            // 
            this.m_labelConfigThumbnailImageHeight.Location = new System.Drawing.Point(366, 193);
            this.m_labelConfigThumbnailImageHeight.Name = "m_labelConfigThumbnailImageHeight";
            this.m_labelConfigThumbnailImageHeight.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.m_labelConfigThumbnailImageHeight.Size = new System.Drawing.Size(50, 24);
            this.m_labelConfigThumbnailImageHeight.TabIndex = 24;
            this.m_labelConfigThumbnailImageHeight.Text = "Hei&ght:";
            this.m_labelConfigThumbnailImageHeight.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_ThumbnailImageHeight_EditBox (Images)
            // 
            this.m_textboxConfigThumbnailImageHeight.Location = new System.Drawing.Point(419, 197);
            this.m_textboxConfigThumbnailImageHeight.Name = "m_textboxConfigThumbnailImageHeight";
            this.m_textboxConfigThumbnailImageHeight.Size = new System.Drawing.Size(34, 20);
            this.m_textboxConfigThumbnailImageHeight.TabIndex = 25;
            this.m_textboxConfigThumbnailImageHeight.Text = "";
        }

        // Builds the GUI for the GEDCOM pane in the config panel
        private void InitialiseSettingsGedcomPane()
        {
            // 
            // configPanel_TabSpaces_Label (GEDCOM)
            // 
            m_labelConfigTabSpaces.Location = new System.Drawing.Point(6, 0);
            m_labelConfigTabSpaces.Name = "m_labelConfigTabSpaces";
            m_labelConfigTabSpaces.RightToLeft = RightToLeft.No;
            m_labelConfigTabSpaces.Size = new System.Drawing.Size(188, 24);
            m_labelConfigTabSpaces.TabIndex = 1;
            m_labelConfigTabSpaces.Text = "&Num spaces to replace tabs:";
            m_labelConfigTabSpaces.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_TabSpaces_EditBox (GEDCOM)
            // 
            m_textboxConfigTabSpaces.Location = new System.Drawing.Point(203, 4);
            m_textboxConfigTabSpaces.Name = "m_textboxConfigTabSpaces";
            m_textboxConfigTabSpaces.Size = new System.Drawing.Size(31, 20);
            m_textboxConfigTabSpaces.TabIndex = 2;
            m_textboxConfigTabSpaces.Text = "";

            // 
            // configPanel_NoName_Label (GEDCOM)
            // 
            m_labelConfigNoName.Location = new System.Drawing.Point(6, 24);
            m_labelConfigNoName.Name = "m_labelConfigNoName";
            m_labelConfigNoName.RightToLeft = RightToLeft.No;
            m_labelConfigNoName.Size = new System.Drawing.Size(200, 24);
            m_labelConfigNoName.TabIndex = 3;
            m_labelConfigNoName.Text = "Show &missing names as:";
            m_labelConfigNoName.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_NoName_EditBox (GEDCOM)
            // 
            m_textboxConfigNoName.Location = new System.Drawing.Point(6, 48);
            m_textboxConfigNoName.Name = "m_textboxConfigNoName";
            m_textboxConfigNoName.Size = new System.Drawing.Size(228, 20);
            m_textboxConfigNoName.TabIndex = 4;
            m_textboxConfigNoName.Text = "";

            //
            // configPanel_ShowWithheldRecords_CheckBox (GEDCOM)
            // 
            m_checkboxConfigShowWithheldRecords.Location = new System.Drawing.Point(6, 86);
            m_checkboxConfigShowWithheldRecords.Name = "m_checkboxConfigShowWithheldRecords";
            m_checkboxConfigShowWithheldRecords.Size = new System.Drawing.Size(200, 16);
            m_checkboxConfigShowWithheldRecords.TabIndex = 5;
            m_checkboxConfigShowWithheldRecords.Text = "Include &withheld records";
            m_checkboxConfigShowWithheldRecords.Click += new System.EventHandler(configPanel_ShowWithheldRecords_CheckBox_click);

            // 
            // configPanel_WithheldName_GroupBox (GEDCOM)
            // 
            m_groupboxConfigWithheldName.Location = new System.Drawing.Point(6, 113);
            m_groupboxConfigWithheldName.Name = "m_groupboxConfigWithheldName";
            m_groupboxConfigWithheldName.Size = new System.Drawing.Size(228, 104);
            m_groupboxConfigWithheldName.TabIndex = 6;
            m_groupboxConfigWithheldName.Text = "Label w&ithheld records with:";
            m_groupboxConfigWithheldName.FlatStyle = FlatStyle.System;

            // 
            // configPanel_WithheldName_Label (GEDCOM)
            // 
            m_radiobuttonConfigWithheldNameLabel.Location = new System.Drawing.Point(10, 18);
            m_radiobuttonConfigWithheldNameLabel.Name = "m_radiobuttonConfigWithheldNameLabel";
            m_radiobuttonConfigWithheldNameLabel.RightToLeft = RightToLeft.No;
            m_radiobuttonConfigWithheldNameLabel.Size = new System.Drawing.Size(180, 20);
            m_radiobuttonConfigWithheldNameLabel.TabIndex = 7;
            m_radiobuttonConfigWithheldNameLabel.Text = "this &text:";
            m_radiobuttonConfigWithheldNameLabel.Click += new System.EventHandler(configPanel_WithheldName_Label_click);

            //
            // configPanel_WithheldName_EditBox (GEDCOM)
            // 
            m_textboxConfigWithheldName.Location = new System.Drawing.Point(28, 38);
            m_textboxConfigWithheldName.Name = "m_textboxConfigWithheldName";
            m_textboxConfigWithheldName.Size = new System.Drawing.Size(188, 20);
            m_textboxConfigWithheldName.TabIndex = 8;
            m_textboxConfigWithheldName.Text = "";

            // 
            // configPanel_WithheldName_Name (GEDCOM)
            // 
            m_radiobuttonConfigWithheldNameName.Location = new System.Drawing.Point(10, 72);
            m_radiobuttonConfigWithheldNameName.Name = "m_radiobuttonConfigWithheldNameName";
            m_radiobuttonConfigWithheldNameName.RightToLeft = RightToLeft.No;
            m_radiobuttonConfigWithheldNameName.Size = new System.Drawing.Size(180, 20);
            m_radiobuttonConfigWithheldNameName.TabIndex = 9;
            m_radiobuttonConfigWithheldNameName.Text = "the individual's n&ame";
            m_radiobuttonConfigWithheldNameName.Click += new System.EventHandler(configPanel_WithheldName_Label_click);

            //
            // configPanel_CapNames_CheckBox (GEDCOM)
            // 
            m_checkboxConfigCapNames.Location = new System.Drawing.Point(266, 7);
            m_checkboxConfigCapNames.Name = "m_checkboxConfigCapNames";
            m_checkboxConfigCapNames.Size = new System.Drawing.Size(200, 20);
            m_checkboxConfigCapNames.TabIndex = 10;
            m_checkboxConfigCapNames.Text = "&Put surnames in CAPITALS";

            //
            // configPanel_CapEvents_CheckBox (GEDCOM)
            // 
            m_checkboxConfigCapEvents.Location = new System.Drawing.Point(266, 34);
            m_checkboxConfigCapEvents.Name = "m_checkboxConfigCapEvents";
            m_checkboxConfigCapEvents.Size = new System.Drawing.Size(260, 20);
            m_checkboxConfigCapEvents.TabIndex = 11;
            m_checkboxConfigCapEvents.Text = "&Start events with a capital letter";

            //
            // configPanel_HideEmails_CheckBox (GEDCOM)
            // 
            m_checkboxConfigHideEmails.Location = new System.Drawing.Point(266, 60);
            m_checkboxConfigHideEmails.Name = "m_checkboxConfigHideEmails";
            m_checkboxConfigHideEmails.Size = new System.Drawing.Size(260, 20);
            m_checkboxConfigHideEmails.TabIndex = 12;
            m_checkboxConfigHideEmails.Text = "Don't show &email addresses";

            //
            // configPanel_OccupationHeadline_CheckBox (GEDCOM)
            // 
            m_checkboxConfigOccupationHeadline.Location = new System.Drawing.Point(266, 86);
            m_checkboxConfigOccupationHeadline.Name = "m_checkboxConfigOccupationHeadline";
            m_checkboxConfigOccupationHeadline.Size = new System.Drawing.Size(260, 20);
            m_checkboxConfigOccupationHeadline.TabIndex = 13;
            m_checkboxConfigOccupationHeadline.Text = "Show occupation in pa&ge heading";

            //
            // configPanel_AllowTrailingSpaces_CheckBox (GEDCOM)
            // 
            m_checkboxConfigAllowTrailingSpaces.Location = new System.Drawing.Point(266, 110);
            m_checkboxConfigAllowTrailingSpaces.Name = "m_checkboxConfigAllowTrailingSpaces";
            m_checkboxConfigAllowTrailingSpaces.Size = new System.Drawing.Size(260, 20);
            m_checkboxConfigAllowTrailingSpaces.TabIndex = 14;
            m_checkboxConfigAllowTrailingSpaces.Text = "Preserve t&railing spaces in GEDCOM";
        }

        // Builds the GUI for the Tree Diagrams pane in the config panel
        private void InitialiseSettingsTreeDiagramsPane()
        {
            //
            // configPanel_TreeDiagrams_CheckBox (Tree Diagrams)
            // 
            m_checkboxConfigTreeDiagrams.Location = new System.Drawing.Point(8, 8);
            m_checkboxConfigTreeDiagrams.Name = "m_checkboxConfigTreeDiagrams";
            m_checkboxConfigTreeDiagrams.Size = new System.Drawing.Size(200, 20);
            m_checkboxConfigTreeDiagrams.TabIndex = 2;
            m_checkboxConfigTreeDiagrams.Text = "Include &tree diagrams";
            m_checkboxConfigTreeDiagrams.Click += new System.EventHandler(configPanel_TreeDiagrams_CheckBox_click);

            // 
            // configPanel_TreeDiagramsFormat_Label (Tree Diagrams)
            // 
            m_labelConfigTreeDiagramsFormat.Location = new System.Drawing.Point(22, 25);
            m_labelConfigTreeDiagramsFormat.Name = "m_labelConfigTreeDiagramsFormat";
            m_labelConfigTreeDiagramsFormat.RightToLeft = RightToLeft.No;
            m_labelConfigTreeDiagramsFormat.Size = new System.Drawing.Size(134, 24);
            m_labelConfigTreeDiagramsFormat.TabIndex = 3;
            m_labelConfigTreeDiagramsFormat.Text = "&File format:";
            m_labelConfigTreeDiagramsFormat.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_TreeDiagramsFormat_ComboBox (Tree Diagrams)
            // 
            m_comboboxConfigTreeDiagramsFormat.Location = new System.Drawing.Point(158, 30);
            m_comboboxConfigTreeDiagramsFormat.Name = "m_comboboxConfigTreeDiagramsFormat";
            m_comboboxConfigTreeDiagramsFormat.Size = new System.Drawing.Size(85, 20);
            m_comboboxConfigTreeDiagramsFormat.TabIndex = 4;
            m_comboboxConfigTreeDiagramsFormat.DropDownWidth = 40;
            m_comboboxConfigTreeDiagramsFormat.DropDownStyle = ComboBoxStyle.DropDownList;

            //
            // configPanel_TreeDiagramsFakeBG_CheckBox (Tree Diagrams)
            // 
            m_checkboxConfigTreeDiagramsFakeBg.Location = new System.Drawing.Point(8, 66);
            m_checkboxConfigTreeDiagramsFakeBg.Name = "m_checkboxConfigTreeDiagramsFakeBg";
            m_checkboxConfigTreeDiagramsFakeBg.Size = new System.Drawing.Size(200, 20);
            m_checkboxConfigTreeDiagramsFakeBg.TabIndex = 5;
            m_checkboxConfigTreeDiagramsFakeBg.Text = "&Simulate transparency";

            //
            // configPanel_ConserveTreeWidth_CheckBox (Tree Diagrams)
            // 
            m_checkboxConfigConserveTreeWidth.Location = new System.Drawing.Point(8, 90);
            m_checkboxConfigConserveTreeWidth.Name = "m_checkboxConfigConserveTreeWidth";
            m_checkboxConfigConserveTreeWidth.Size = new System.Drawing.Size(190, 24);
            m_checkboxConfigConserveTreeWidth.TabIndex = 6;
            m_checkboxConfigConserveTreeWidth.Text = "Conserve tree &width";

            //
            // configPanel_KeepSiblingOrder_CheckBox (Tree Diagrams)
            // 
            m_checkboxConfigKeepSiblingOrder.Location = new System.Drawing.Point(8, 114);
            m_checkboxConfigKeepSiblingOrder.Name = "m_checkboxConfigKeepSiblingOrder";
            m_checkboxConfigKeepSiblingOrder.Size = new System.Drawing.Size(230, 24);
            m_checkboxConfigKeepSiblingOrder.TabIndex = 7;
            m_checkboxConfigKeepSiblingOrder.Text = "Keep s&ibling order from GEDCOM";

            //
            // configPanel_MiniTreeColours_GroupBox (Tree Diagrams)
            // 
            m_groupboxMiniTreeColours.Location = new System.Drawing.Point(260, 11);
            m_groupboxMiniTreeColours.Name = "m_groupboxMiniTreeColours";
            m_groupboxMiniTreeColours.Size = new System.Drawing.Size(230, 224);
            m_groupboxMiniTreeColours.TabIndex = 8;
            m_groupboxMiniTreeColours.Text = "Colours";
            m_groupboxMiniTreeColours.FlatStyle = FlatStyle.System;

            //
            // configPanel_MiniTreeColourIndiHighlight_Button (Tree Diagrams)
            // 
            m_buttonConfigMiniTreeColourIndiHighlight.Location = new System.Drawing.Point(12, 24);
            m_buttonConfigMiniTreeColourIndiHighlight.Name = "m_buttonConfigMiniTreeColourIndiHighlight";
            m_buttonConfigMiniTreeColourIndiHighlight.Size = new System.Drawing.Size(98, 24);
            m_buttonConfigMiniTreeColourIndiHighlight.TabIndex = 9;
            m_buttonConfigMiniTreeColourIndiHighlight.Text = "Selected &box";
            m_buttonConfigMiniTreeColourIndiHighlight.Click += new System.EventHandler(configPanel_MiniTreeColourIndiHighlight_Button_click);

            //
            // configPanel_MiniTreeColourIndiText_Button (Tree Diagrams)
            // 
            m_buttonConfigMiniTreeColourIndiText.Location = new System.Drawing.Point(122, 24);
            m_buttonConfigMiniTreeColourIndiText.Name = "m_buttonConfigMiniTreeColourIndiText";
            m_buttonConfigMiniTreeColourIndiText.Size = new System.Drawing.Size(98, 24);
            m_buttonConfigMiniTreeColourIndiText.TabIndex = 10;
            m_buttonConfigMiniTreeColourIndiText.Text = "Selected te&xt";
            m_buttonConfigMiniTreeColourIndiText.Click += new System.EventHandler(configPanel_MiniTreeColourIndiText_Button_click);

            //
            // configPanel_MiniTreeColourIndiBackground_Button (Tree Diagrams)
            // 
            m_buttonConfigMiniTreeColourIndiBackground.Location = new System.Drawing.Point(12, 60);
            m_buttonConfigMiniTreeColourIndiBackground.Name = "m_buttonConfigMiniTreeColourIndiBackground";
            m_buttonConfigMiniTreeColourIndiBackground.Size = new System.Drawing.Size(98, 24);
            m_buttonConfigMiniTreeColourIndiBackground.TabIndex = 11;
            m_buttonConfigMiniTreeColourIndiBackground.Text = "&General box";
            m_buttonConfigMiniTreeColourIndiBackground.BackColor = Color.FromArgb(255, 0, 0);
            m_buttonConfigMiniTreeColourIndiBackground.Click += new System.EventHandler(configPanel_MiniTreeColourIndiBackground_Button_click);

            //
            // configPanel_MiniTreeColourIndiLink_Button (Tree Diagrams)
            // 
            m_buttonConfigMiniTreeColourIndiLink.Location = new System.Drawing.Point(122, 60);
            m_buttonConfigMiniTreeColourIndiLink.Name = "m_buttonConfigMiniTreeColourIndiLink";
            m_buttonConfigMiniTreeColourIndiLink.Size = new System.Drawing.Size(98, 24);
            m_buttonConfigMiniTreeColourIndiLink.TabIndex = 12;
            m_buttonConfigMiniTreeColourIndiLink.Text = "&Link text";
            m_buttonConfigMiniTreeColourIndiLink.Click += new System.EventHandler(configPanel_MiniTreeColourIndiLink_Button_click);

            //
            // configPanel_MiniTreeColourIndiBgConcealed_Button (Tree Diagrams)
            // 
            m_buttonConfigMiniTreeColourIndiBgConcealed.Location = new System.Drawing.Point(12, 96);
            m_buttonConfigMiniTreeColourIndiBgConcealed.Name = "m_buttonConfigMiniTreeColourIndiBgConcealed";
            m_buttonConfigMiniTreeColourIndiBgConcealed.Size = new System.Drawing.Size(98, 24);
            m_buttonConfigMiniTreeColourIndiBgConcealed.TabIndex = 13;
            m_buttonConfigMiniTreeColourIndiBgConcealed.Text = "&Private box";
            m_buttonConfigMiniTreeColourIndiBgConcealed.Click += new System.EventHandler(configPanel_MiniTreeColourIndiBgConcealed_Button_click);

            //
            // configPanel_MiniTreeColourIndiFgConcealed_Button (Tree Diagrams)
            // 
            m_buttonConfigMiniTreeColourIndiFgConcealed.Location = new System.Drawing.Point(122, 96);
            m_buttonConfigMiniTreeColourIndiFgConcealed.Name = "m_buttonConfigMiniTreeColourIndiFgConcealed";
            m_buttonConfigMiniTreeColourIndiFgConcealed.Size = new System.Drawing.Size(98, 24);
            m_buttonConfigMiniTreeColourIndiFgConcealed.TabIndex = 14;
            m_buttonConfigMiniTreeColourIndiFgConcealed.Text = "P&rivate text";
            m_buttonConfigMiniTreeColourIndiFgConcealed.Click += new System.EventHandler(configPanel_MiniTreeColourIndiFgConcealed_Button_click);

            //
            // configPanel_MiniTreeColourIndiShade_Button (Tree Diagrams)
            // 
            m_buttonConfigMiniTreeColourIndiShade.Location = new System.Drawing.Point(12, 132);
            m_buttonConfigMiniTreeColourIndiShade.Name = "m_buttonConfigMiniTreeColourIndiShade";
            m_buttonConfigMiniTreeColourIndiShade.Size = new System.Drawing.Size(98, 24);
            m_buttonConfigMiniTreeColourIndiShade.TabIndex = 15;
            m_buttonConfigMiniTreeColourIndiShade.Text = "Spous&e box";
            m_buttonConfigMiniTreeColourIndiShade.Click += new System.EventHandler(configPanel_MiniTreeColourIndiShade_Button_click);

            //
            // configPanel_MiniTreeColourBranch_Button (Tree Diagrams)
            // 
            m_buttonConfigMiniTreeColourBranch.Location = new System.Drawing.Point(12, 168);
            m_buttonConfigMiniTreeColourBranch.Name = "m_buttonConfigMiniTreeColourBranch";
            m_buttonConfigMiniTreeColourBranch.Size = new System.Drawing.Size(98, 24);
            m_buttonConfigMiniTreeColourBranch.TabIndex = 16;
            m_buttonConfigMiniTreeColourBranch.Text = "Br&anches";
            m_buttonConfigMiniTreeColourBranch.Click += new System.EventHandler(configPanel_MiniTreeColourBranch_Button_click);

            //
            // configPanel_MiniTreeColourIndiBorder_Button (Tree Diagrams)
            // 
            m_buttonConfigMiniTreeColourIndiBorder.Location = new System.Drawing.Point(122, 168);
            m_buttonConfigMiniTreeColourIndiBorder.Name = "m_buttonConfigMiniTreeColourIndiBorder";
            m_buttonConfigMiniTreeColourIndiBorder.Size = new System.Drawing.Size(98, 24);
            m_buttonConfigMiniTreeColourIndiBorder.TabIndex = 17;
            m_buttonConfigMiniTreeColourIndiBorder.Text = "Box bor&ders";
            m_buttonConfigMiniTreeColourIndiBorder.Click += new System.EventHandler(configPanel_MiniTreeColourIndiBorder_Button_click);
        }

        // Builds the GUI for the Advanced pane in the config panel
        private void InitialiseSettingsAdvancedPane()
        {
            // 
            // configPanel_Charset_Label  (Advanced)
            // 
            m_labelConfigCharset.Location = new System.Drawing.Point(9, 0);
            m_labelConfigCharset.Name = "m_labelConfigCharset";
            m_labelConfigCharset.RightToLeft = RightToLeft.No;
            m_labelConfigCharset.Size = new System.Drawing.Size(120, 24);
            m_labelConfigCharset.TabIndex = 1;
            m_labelConfigCharset.Text = "Ch&aracter set:";
            m_labelConfigCharset.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_Charset_ComboBox (Advanced)
            // 
            m_comboboxConfigCharset.Location = new System.Drawing.Point(139, 1);
            m_comboboxConfigCharset.Name = "m_comboboxConfigCharset";
            m_comboboxConfigCharset.Size = new System.Drawing.Size(95, 20);
            m_comboboxConfigCharset.TabIndex = 2;
            m_comboboxConfigCharset.DropDownWidth = 40;
            m_comboboxConfigCharset.DropDownStyle = ComboBoxStyle.DropDownList;
            m_comboboxConfigCharset.SelectedIndexChanged += new System.EventHandler(configPanel_Charset_ComboBox_changed);

            //
            // configPanel_UseBom_CheckBox (Advanced)
            // 
            m_checkboxConfigUseBom.Location = new System.Drawing.Point(11, 26);
            m_checkboxConfigUseBom.Name = "m_checkboxConfigUseBom";
            m_checkboxConfigUseBom.Size = new System.Drawing.Size(200, 20);
            m_checkboxConfigUseBom.Text = "Include &byte order mark (BOM)";
            m_checkboxConfigUseBom.TabIndex = 3;

            // 
            // configPanel_HTMLExtn_Label (Advanced)
            // 
            m_labelConfigHtmlExtn.Location = new System.Drawing.Point(9, 54);
            m_labelConfigHtmlExtn.Name = "m_labelConfigHtmlExtn";
            m_labelConfigHtmlExtn.RightToLeft = RightToLeft.No;
            m_labelConfigHtmlExtn.Size = new System.Drawing.Size(140, 24);
            m_labelConfigHtmlExtn.TabIndex = 4;
            m_labelConfigHtmlExtn.Text = "H&TML file extension:";
            m_labelConfigHtmlExtn.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_HTMLExtn_ComboBox  (Advanced)
            // 
            m_comboboxConfigHtmlExtn.Location = new System.Drawing.Point(149, 55);
            m_comboboxConfigHtmlExtn.Name = "m_comboboxConfigHtmlExtn";
            m_comboboxConfigHtmlExtn.Size = new System.Drawing.Size(85, 20);
            m_comboboxConfigHtmlExtn.TabIndex = 5;
            m_comboboxConfigHtmlExtn.DropDownWidth = 40;
            m_comboboxConfigHtmlExtn.DropDownStyle = ComboBoxStyle.DropDownList;

            //
            // configPanel_W3C_CheckBox (Advanced)
            // 
            m_checkboxConfigW3C.Location = new System.Drawing.Point(11, 91);
            m_checkboxConfigW3C.Name = "m_checkboxConfigW3C";
            m_checkboxConfigW3C.Size = new System.Drawing.Size(200, 20);
            m_checkboxConfigW3C.Text = "Add &W3C validator sticker";
            m_checkboxConfigW3C.TabIndex = 6;

            //
            // configPanel_user_rec_filename_CheckBox (Advanced)
            // 
            m_checkboxConfigUserRecFilename.Location = new System.Drawing.Point(11, 112);
            m_checkboxConfigUserRecFilename.Name = "m_checkboxConfigUserRecFilename";
            m_checkboxConfigUserRecFilename.Size = new System.Drawing.Size(240, 24);
            m_checkboxConfigUserRecFilename.Text = "&Use custom record number for filenames";
            m_checkboxConfigUserRecFilename.TabIndex = 7;

            //
            // configPanel_SupressBackreferences_CheckBox (Advanced)
            // 
            m_checkboxConfigSupressBackreferences.Location = new System.Drawing.Point(11, 136);
            m_checkboxConfigSupressBackreferences.Name = "m_checkboxConfigSupressBackreferences";
            m_checkboxConfigSupressBackreferences.Size = new System.Drawing.Size(250, 20);
            m_checkboxConfigSupressBackreferences.Text = "List c&iting records on source pages";
            m_checkboxConfigSupressBackreferences.TabIndex = 8;

            // 
            // m_labelConfigStylesheetName (Advanced)
            // 
            m_labelConfigStylesheetName.Location = new System.Drawing.Point(266, 0);
            m_labelConfigStylesheetName.Name = "m_labelConfigStylesheetName";
            m_labelConfigStylesheetName.RightToLeft = RightToLeft.No;
            m_labelConfigStylesheetName.Size = new System.Drawing.Size(224, 24);
            m_labelConfigStylesheetName.TabIndex = 9;
            m_labelConfigStylesheetName.Text = "Name of st&ylesheet:";
            m_labelConfigStylesheetName.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_StylesheetName_EditBox (Advanced)
            // 
            m_textboxConfigStylesheetName.Location = new System.Drawing.Point(266, 32);
            m_textboxConfigStylesheetName.Name = "m_textboxConfigStylesheetName";
            m_textboxConfigStylesheetName.Size = new System.Drawing.Size(175, 20);
            m_textboxConfigStylesheetName.TabIndex = 10;
            m_textboxConfigStylesheetName.Text = "";
            m_textboxConfigStylesheetName.Multiline = false;

            // 
            // configPanel_StylesheetName_ExtnLabel (Advanced)
            // 
            m_labelConfigStylesheetNameExtn.Location = new System.Drawing.Point(440, 27);
            m_labelConfigStylesheetNameExtn.Name = "m_labelConfigStylesheetNameExtn";
            m_labelConfigStylesheetNameExtn.RightToLeft = RightToLeft.No;
            m_labelConfigStylesheetNameExtn.Size = new System.Drawing.Size(60, 24);
            m_labelConfigStylesheetNameExtn.TabIndex = 11;
            m_labelConfigStylesheetNameExtn.Text = ".css";
            m_labelConfigStylesheetNameExtn.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            //
            // configPanel_PreserveStylesheet_CheckBox (Advanced)
            // 
            m_checkboxConfigPreserveStylesheet.Location = new System.Drawing.Point(266, 56);
            m_checkboxConfigPreserveStylesheet.Name = "m_checkboxConfigPreserveStylesheet";
            m_checkboxConfigPreserveStylesheet.Size = new System.Drawing.Size(250, 20);
            m_checkboxConfigPreserveStylesheet.Text = "Do &not generate new stylesheet";
            m_checkboxConfigPreserveStylesheet.TabIndex = 12;

            //
            // m_checkboxConfigExcludeHelppage (Advanced)
            // 
            m_checkboxConfigIncludeHelppage.Location = new System.Drawing.Point(266, 91);
            m_checkboxConfigIncludeHelppage.Name = "m_checkboxConfigExcludeHelppage";
            m_checkboxConfigIncludeHelppage.Size = new System.Drawing.Size(250, 20);
            m_checkboxConfigIncludeHelppage.Text = "Include help page";
            m_checkboxConfigIncludeHelppage.TabIndex = 15;
        }

        // The main entry point for the application.
        [STAThread]
        static void Main(string[] args)
        {
            bool resetConfig = false;
            bool nextArgIsFilename = false;
            string logFilename = "";

            // User can hold down Ctrl and Shift while app starts to enable log file creation
            // People will find this easier than altering command gedcomLine options!
            if (Control.ModifierKeys == (Keys.Shift | Keys.Control)) {
                LogFile.Instance.SetLogLevel(LogFile.EDebugLevel.All);
                LogFile.Instance.StartLogFile("C:\\gedmill.txt");
            }

            if (args != null) {
                foreach (string sArg in args) {
                    if (nextArgIsFilename) {
                        logFilename = sArg;
                        nextArgIsFilename = false;
                    } else {
                        switch (sArg) {
                            case "-logfile": {
                                    LogFile.Instance.SetLogLevel(LogFile.EDebugLevel.All);
                                    LogFile.Instance.SetDebugAllowFilter(LogFile.DT_ALL ^ LogFile.DT_GEDCOM); // Everything but gedcom
                                    nextArgIsFilename = true;
                                    break;
                                }
                            case "-debug": {
                                    LogFile.Instance.SetLogLevel(LogFile.EDebugLevel.All);
                                    LogFile.Instance.SetDebugAllowFilter(LogFile.DT_ALL ^ LogFile.DT_GEDCOM); // Everything but gedcom
                                    break;
                                }
                            case "-debug_gedcom": {
                                    LogFile.Instance.SetLogLevel(LogFile.EDebugLevel.All);
                                    LogFile.Instance.SetDebugAllowFilter(LogFile.DT_ALL); // All
                                    break;
                                }
                            case "-reset": {
                                    resetConfig = true;
                                    break;
                                }
                        }
                    }
                }
            }
            if (logFilename != "") {
                LogFile.Instance.StartLogFile(logFilename);
            }
            string startTime = DateTime.Now.ToString();
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, MainForm.SoftwareName + " started at " + startTime);

            m_mainForm = new MainForm(resetConfig);
            Application.Run(m_mainForm);

            LogFile.Instance.StopLogFile();
        }

        #region Event handlers

        private void backButton_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Back button");

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
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Next button clicked. current panel = " + fCurrentPanel.ToString());

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
                    Config.StoreSettings();

                    if (Config.OpenWebsiteOnExit) {
                        OpenURL(Config.FrontPageURL);
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

        // Event handler
        private void cancelButton_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Quit button clicked.");

            DialogResult dialogResult = DialogResult.Yes;

            if (fCurrentPanel != 6) {
                // Cancel button is "Finish" in panel 6
                dialogResult = MessageBoxEx.Show(m_mainForm, "Are you sure you wish to exit GEDmill?", "Quit GEDmill",
                    MessageBoxButtons.YesNo, MessageBoxIcon.Question, false);
            }

            if (dialogResult == DialogResult.Yes) {
                if (fCurrentPanel >= 6) {
                    // HTML generated, so save settings for that at least
                    // Store checkbox state in config.(Can't do in ValidateCurrentPanel because Next never gets clicked)
                    Config.OpenWebsiteOnExit = m_checkboxAllDoneShowSite.Checked;
                }

                if (fCurrentPanel == 6) {
                    // Finish button is the only time we want to launch webpages
                    if (Config.OpenWebsiteOnExit && Config.FrontPageFilename.Length > 0) {
                        OpenURL(Config.FrontPageURL);
                    }

                }

                Config.StoreSettings();
                Application.Exit();
            }
        }

        // Event handler
        private void helpButton_click(object sender, System.EventArgs e)
        {
            string sHelpFile = Config.ApplicationPath + "\\" + HelpFilename;

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

        // Event handler
        private void configButton_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Config button clicked. current panel = " + fCurrentPanel.ToString());

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

        // Event handler
        private void configCancelButton_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Config reset button clicked. current panel = " + fCurrentPanel.ToString());

            // Ensure config panel on
            if (!fConfigPanelOn) {
                return;
            }

            // Remove panel without saving changes
            SwitchConfigPanelOff();
        }

        // Event handler
        private void buttonPruneRecordsSave_click(object sender, System.EventArgs e)
        {
        }

        // Event handler
        private void buttonPruneRecordsLoad_click(object sender, System.EventArgs e)
        {
        }

        // Event handler
        private void buttonChooseGedcomBrowse_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Panel 2 browse button clicked.");

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
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Selected file : " + InputFile);
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

        // Event handler
        private void buttonSelectKeyDelete_click(object sender, System.EventArgs e)
        {
            NameXRefPair xrefPairName = (NameXRefPair)m_listboxSelectKey.SelectedItem;
            if (xrefPairName != null) {
                string xref = xrefPairName.XRef;
                if (xref != null) {
                    int nIndex;
                    int nKeys = Config.KeyIndividuals.Count;
                    for (nIndex = 0; nIndex < nKeys; nIndex++) {
                        if (xref == (string)(Config.KeyIndividuals[nIndex])) {
                            Config.KeyIndividuals.RemoveAt(nIndex);
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

        // Event handler
        private void linklabelAllDone_click(object sender, LinkLabelLinkClickedEventArgs e)
        {
            bool bOldVisitedValue = m_linklabelAllDone.Links[m_linklabelAllDone.Links.IndexOf(e.Link)].Visited;
            try {
                m_linklabelAllDone.Links[m_linklabelAllDone.Links.IndexOf(e.Link)].Visited = true;
                string sURL = m_linklabelAllDone.Text;
                System.Diagnostics.Process.Start(sURL);
            } catch (Exception e2) {
                LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, String.Format("Caught exception while viewing folder : {0}", e2.ToString()));
                m_linklabelAllDone.Links[m_linklabelAllDone.Links.IndexOf(e.Link)].Visited = bOldVisitedValue;
            }
        }

        // Event handler
        private void configPanel_BackImage_BrowseButton_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "config panel back image browse button clicked.");

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
                    MessageBoxEx.Show(m_mainForm, "The file you have selected is not a supported picture type.", "Unsupported Format",
                        MessageBoxButtons.OK, MessageBoxIcon.Exclamation, false);
                } else {
                    m_textboxConfigBackImageEdit.Text = openFileDialog.FileName;
                    m_textboxConfigBackImageEdit.SelectionStart = m_textboxConfigBackImageEdit.Text.Length;
                    m_textboxConfigBackImageEdit.SelectionLength = m_textboxConfigBackImageEdit.Text.Length;
                }

            }
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Selected file : " + m_textboxConfigBackImageEdit.Text);
        }

        // Event handler
        private void configPanel_FrontImage_BrowseButton_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "config panel front image browse button clicked.");

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
                    MessageBoxEx.Show(m_mainForm, "The file you have selected is not a supported picture type.", "Unsupported Format",
                        MessageBoxButtons.OK, MessageBoxIcon.Exclamation, false);
                } else {
                    m_textboxConfigFrontImageEdit.Text = openFileDialog1.FileName;
                    m_textboxConfigFrontImageEdit.SelectionStart = m_textboxConfigFrontImageEdit.Text.Length;
                    m_textboxConfigFrontImageEdit.SelectionLength = m_textboxConfigFrontImageEdit.Text.Length;
                }

            }
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Selected file : " + m_textboxConfigFrontImageEdit.Text);

        }

        // Event handler
        private void configPanel_TreeDiagrams_CheckBox_click(object sender, System.EventArgs e)
        {
            EnableMiniTreeButtons();
        }

        // Event handler
        private void configPanel_MiniTreeColourIndiBackground_Button_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourIndiBackground_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiBackground;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeIndiBackground = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MiniTreeColourIndiHighlight_Button_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourIndiHighlight_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiHighlight;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeIndiHighlight = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MiniTreeColourIndiBgConcealed_Button_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourIndiBgConcealed_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiBgConcealed;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeIndiBgConcealed = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MiniTreeColourIndiShade_Button_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourIndiShade_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiShade;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeIndiShade = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MiniTreeColourIndiText_Button_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourIndiText_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiText;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeIndiText = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MiniTreeColourIndiLink_Button_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourIndiLink_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiLink;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeIndiLink = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MiniTreeColourBranch_Button_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourBranch_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeBranch;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeBranch = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MiniTreeColourIndiBorder_Button_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourIndiBorder_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiBorder;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeIndiBorder = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MiniTreeColourIndiFgConcealed_Button_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "configPanel_MiniTreeColourIndiFgConcealed_Button_click.");

            fColorDialogConfigMiniTree.Color = m_colorConfigMiniTreeIndiFgConcealed;
            if (fColorDialogConfigMiniTree.ShowDialog() == DialogResult.OK) {
                m_colorConfigMiniTreeIndiFgConcealed = fColorDialogConfigMiniTree.Color;
                SetMiniTreeColourConfigButtons();
            }
        }

        // Event handler
        private void configPanel_MultiPageIndex_CheckBox_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "config panel multi page index button clicked.");
            EnableMultiPageIndexConfig();
        }

        // Event handler
        private void configPanel_AllowMultimedia_CheckBox_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "allow multimedia button clicked.");
            EnableMultimediaConfig();
        }

        // Event handler
        private void configPanel_IndiImages_CheckBox_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "config panel multi images button clicked.");
            EnableThumbnailsConfig();
        }

        // Event handler
        private void configPanel_ShowWithheldRecords_CheckBox_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "config panel show withheld records button clicked.");
            EnableWithheldConfig();
        }

        // Event handler
        private void configPanel_WithheldName_Label_click(object sender, System.EventArgs e)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "config panel withheld label clicked.");
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
            GEDCOMIndividualRecord ir = null;
            CListableBool lb = null;

            if (lvPruneIndividuals.SelectedItems.Count == 1) {
                lb = (CListableBool)((ListViewItem)(lvPruneIndividuals.SelectedItems[0]));
                ir = (GEDCOMIndividualRecord)lb.Record;
            }

            ShowIndividualDetailsDialog(this, lb, ir, true, true);
        }

        // Event handler
        private void pruneSourcesContextMenuDetails_Click(Object sender, System.EventArgs e)
        {
            GEDCOMSourceRecord sr = null;
            ListViewItem lvi = null;

            if (lvPruneSources.SelectedItems.Count == 1) {
                lvi = lvPruneSources.SelectedItems[0];
                sr = (GEDCOMSourceRecord)((CListableBool)((ListViewItem)lvi)).Record;
            }

            ShowSourceDetailsDialog(this, ((CListableBool)((ListViewItem)lvi)), sr, true, true);
        }

        // Event handler
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
                        GEDCOMIndividualRecord ir = (GEDCOMIndividualRecord)((CListableBool)((ListViewItem)lvi)).Record;
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
            } catch (System.Exception ex) {
                ReportPruneError(ex);
            }

            // Rebuild list
            FillIndividualsList(lvPruneIndividuals, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(PruneExcluded, PruneIncluded, "individual");
        }

        // Event handler
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
                        GEDCOMIndividualRecord ir = (GEDCOMIndividualRecord)((CListableBool)((ListViewItem)lvi)).Record;
                        if (ir != null) {
                            fTree.BeginPruning(); // Initialises visisted hash table
                            fTree.PruneDescendants(ir, true);
                            fTree.EndPruning(); // Destroys visited hash table
                        }
                    }
                }
            } catch (System.Exception ex) {
                ReportPruneError(ex);
            }

            // Rebuild list
            FillIndividualsList(lvPruneIndividuals, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(PruneExcluded, PruneIncluded, "individual");

        }

        // Event handler
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
                        GEDCOMIndividualRecord ir = (GEDCOMIndividualRecord)((CListableBool)((ListViewItem)lvi)).Record;
                        if (ir != null) {
                            fTree.BeginPruning();
                            fTree.PruneDescendants(ir, false);
                            fTree.EndPruning();
                        }
                    }
                }
            } catch (System.Exception ex) {
                ReportPruneError(ex);
            }

            // Rebuild lists
            FillIndividualsList(lvPruneIndividuals, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(PruneExcluded, PruneIncluded, "individual");
        }

        // Event handler
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
                        GEDCOMIndividualRecord ir = (GEDCOMIndividualRecord)((CListableBool)((ListViewItem)lvi)).Record;
                        if (ir != null) {
                            fTree.BeginPruning();
                            fTree.PruneAncestors(ir, true);
                            fTree.EndPruning();
                        }
                    }
                }
            } catch (System.Exception ex) {
                ReportPruneError(ex);
            }

            // Rebuild lists
            FillIndividualsList(lvPruneIndividuals, true);

            Cursor.Current = Cursors.Default;
            Cursor.Hide();

            ShowPruneResult(PruneExcluded, PruneIncluded, "individual");
        }

        // Event handler
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
                        GEDCOMIndividualRecord ir = (GEDCOMIndividualRecord)((CListableBool)((ListViewItem)lvi)).Record;
                        if (ir != null) {
                            fTree.BeginPruning();
                            fTree.PruneAncestors(ir, false);
                            fTree.EndPruning();
                        }
                    }
                }
            } catch (System.Exception ex) {
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
                        ir.SetVisibility(true);
                        ++PruneIncluded;
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
                        ir.SetVisibility(false);
                        PruneExcluded++;
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
                        sr.SetVisibility(false);
                        PruneExcluded++;
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
                        if (ir != null) {
                            GEDCOMDateValue dateBorn = null, dateDied = null;
                            var lifeDatesX = ir.GetLifeDates();
                            dateBorn = (lifeDatesX.BirthEvent == null) ? null : lifeDatesX.BirthEvent.Date;
                            dateDied = (lifeDatesX.DeathEvent == null) ? null : lifeDatesX.DeathEvent.Date;

                            bool bInclude = true;
                            if (ir.IsLive()) {
                                bInclude = false;
                            }

                            if (bInclude == false && ir.GetVisibility()) {
                                PruneExcluded++;
                                ir.SetVisibility(false);
                            }
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

        private void pruneIndividualsContextMenu_popup(System.Object sender, System.EventArgs e)
        {
            int nSelected = lvPruneIndividuals.SelectedItems.Count;
            m_menuitemPruneRecordsIndisUnconnected.Enabled = (nSelected > 0);
            if (nSelected <= 1) {
                m_menuitemPruneRecordsIndisUnconnected.Text = "E&xclude individuals unless navigable from this person";
            } else {
                m_menuitemPruneRecordsIndisUnconnected.Text = String.Format("E&xclude individuals unless navigable from these {0} people", nSelected);
            }

            m_menuitemPruneRecordsIndisDescendantsExc.Enabled = (nSelected == 1);
            m_menuitemPruneRecordsIndisDescendantsInc.Enabled = (nSelected == 1);
            m_menuitemPruneRecordsIndisAncestorsExc.Enabled = (nSelected == 1);
            m_menuitemPruneRecordsIndisAncestorsInc.Enabled = (nSelected == 1);
            m_menuitemPruneRecordsIndisDetails.Enabled = (nSelected == 1);
        }

        private void pruneSourcesContextMenu_popup(System.Object sender, System.EventArgs e)
        {
            int nSelected = lvPruneSources.SelectedItems.Count;
            m_menuitemPruneRecordsSourcesDetails.Enabled = (nSelected == 1);
            m_menuitemPruneRecordsSourcesRemovePics.Enabled = (nSelected > 0);
            if (nSelected <= 1) {
                m_menuitemPruneRecordsSourcesRemovePics.Text = "&Remove pictures from this source";
            } else {
                m_menuitemPruneRecordsSourcesRemovePics.Text = String.Format("&Remove pictures from these {0} sources", nSelected);
            }
        }

        private void lvPruneIndividuals_ItemCheck(object sender, ItemCheckEventArgs e)
        {
            if (!fDisablePrunepanelCheckEvent) {
                CListableBool lb = (CListableBool)lvPruneIndividuals.Items[e.Index];
                // For some reason if user presses control while clicking any row of the list, it causes a check event. We don't want any checking to happen in that case.
                if ((Control.ModifierKeys & Keys.Control) == 0) {
                    if (e.NewValue == CheckState.Checked && !lb.Record.GetVisibility()
                        || e.NewValue == CheckState.Unchecked && lb.Record.GetVisibility()) {
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

        // Event handler
        private void listviewPruneRecordsSources_ItemCheck(object sender, ItemCheckEventArgs e)
        {
            if (!fDisablePrunepanelCheckEvent) {
                CListableBool lb = (CListableBool)lvPruneSources.Items[e.Index];

                // For some reason if user presses control while clicking any row of the list, it causes a check event. We don't want any checking to happen in that case.
                if ((Control.ModifierKeys & Keys.Control) == 0) {
                    if (e.NewValue == CheckState.Checked && !lb.Record.GetVisibility()
                     || e.NewValue == CheckState.Unchecked && lb.Record.GetVisibility()) {
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

        // Presents a file selection dialog and returns the selecetd file name and path
        public static bool SelectFile(ref string fileDir, ref string fileName, string title, string defaultName,
                                      bool loadNotSave, string filterName, ArrayList filterExtensions)
        {
            bool fileSelected = false;

            FileDialog fileDialog;
            if (loadNotSave) {
                fileDialog = new OpenFileDialog();
            } else {
                fileDialog = new SaveFileDialog();
            }

            if (fileDir.Length > 0) {
                fileDialog.InitialDirectory = fileDir;
            } else {
                fileDialog.InitialDirectory = Environment.GetFolderPath(System.Environment.SpecialFolder.Personal);
            }

            if (fileName.Length > 0) {
                fileDialog.FileName = fileName;
            } else {
                fileDialog.FileName = defaultName;
            }

            string sFilterString = "";
            int nFilterAllIndex = 1;
            if (filterExtensions.Count > 0) {
                nFilterAllIndex++;
                string sFilterCode = "";
                bool bFirst = true;
                //"Picture files (*.jpg; *.gif)|*.jpg;*.gif|All files (*.*)|*.*";
                foreach (string sFilterExtn in filterExtensions) {
                    if (!bFirst) {
                        sFilterCode += ";";
                    } else {
                        bFirst = false;
                    }
                    sFilterCode += "*" + sFilterExtn;
                }
                sFilterString = filterName + " (" + sFilterCode + ")|" + sFilterCode + "|";
            }
            sFilterString += "All files (*.*)|*.*";
            fileDialog.Filter = sFilterString;
            fileDialog.FilterIndex = 1;
            string sExtn = Path.GetExtension(fileDialog.FileName);

            // Check whether selected file matches given filter
            bool bValidExtn = true;
            if (fileDialog.FileName.Length > 0) {
                bValidExtn = false;
                string sExtnFromDlg = Path.GetExtension(fileDialog.FileName).ToUpper();
                foreach (string sFilterExtn in filterExtensions) {
                    if (sExtnFromDlg == sFilterExtn.ToUpper()) {
                        bValidExtn = true;
                        break;
                    }
                }
            }

            if (!bValidExtn) {
                // Use *.* filter if default file isn't a .txt file.
                fileDialog.FilterIndex = nFilterAllIndex;
            }
            fileDialog.RestoreDirectory = true;
            fileDialog.Title = title;

            if (fileDialog.ShowDialog() == DialogResult.OK) {
                fileSelected = true;
                fileDir = Path.GetDirectoryName(fileDialog.FileName);
                fileName = Path.GetFileName(fileDialog.FileName);
            }
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Selected file : " + fileDir + "\\" + fileName);

            return (fileSelected);
        }

        // Modifies rectNew to fit within the limits given, keeping its aspect ratio
        public static void ScaleAreaToFit(ref Rectangle rectNew, uint uMaxWidth, uint uMaxHeight)
        {
            if (rectNew.Height > uMaxHeight) {
                // Image won't fit horizontally, so scale in both directions til it will
                rectNew.Width = (rectNew.Width * (int)uMaxHeight) / rectNew.Height;
                rectNew.Height = (int)uMaxHeight;
            }

            if (rectNew.Width > uMaxWidth) {
                // Image won't fit horizontally, so scale in both directions til it will
                rectNew.Height = (rectNew.Height * (int)uMaxWidth) / rectNew.Width;
                rectNew.Width = (int)uMaxWidth;
            }
        }

        // Accessor
        public string InputFile
        {
            get {
                return m_textboxChooseGedcom.Text;
            }
            set {
                m_textboxChooseGedcom.Text = value;
            }
        }

        // Returns the name of the alternative picture file to display for non-diaplayable files of the given format
        public static string NonPicFilename(string sFormat, bool bSmall, bool bClickToDownload)
        {
            string sFilename;
            switch (sFormat.ToLower()) {
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
        public void ShowIndividualDetailsDialog(Form formParent, CListableBool lbItem, GEDCOMIndividualRecord ir, bool bCanEditPictures, bool bCheckBoxes)
        {
        }

        // Shows the settings panel
        private void SwitchConfigPanelOn()
        {
            // Disable edit boxes etc. on previous panel to avoid confusing users
            EnableCurrentPanel(false);

            // Move help button to its new location
            m_buttonHelp.Location = new System.Drawing.Point(8, 288);

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
            m_buttonSettings.Location = new System.Drawing.Point(344, 288);
            m_buttonSettings.Size = new System.Drawing.Size(fDefaultButtonSize);

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
            m_buttonSettings.Location = new System.Drawing.Point(88, 288);
            m_buttonSettings.Size = new System.Drawing.Size(fConfigButtonSize);

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
                    m_buttonHelp.Location = new System.Drawing.Point(8, 288);
                    m_buttonCancel.Location = new System.Drawing.Point(424, 288);
                    m_buttonNext.Visible = false;
                } else if (fCurrentPanel == 9) {
                    m_buttonHelp.Location = new System.Drawing.Point(8, 288);
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
                    m_buttonCancel.Location = new System.Drawing.Point(8, 288);
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
                    InputFile = Config.InputFilename;
                    m_textboxChooseGedcom.SelectionStart = InputFile.Length;
                    m_textboxChooseGedcom.SelectionLength = InputFile.Length;
                    break;
                case 4:
                    m_textboxSelectKey.Text = Config.SiteTitle;
                    Config.FirstRecordXRef = "";
                    FillKeyIndividualsList();
                    break;
                case 5:
                    m_textboxChooseOutput.Text = Config.OutputFolder;
                    m_textboxChooseOutput.SelectionStart = InputFile.Length;
                    m_textboxChooseOutput.SelectionLength = InputFile.Length;
                    break;
                case 6:
                    m_checkboxAllDoneShowSite.Visible = File.Exists(Config.FrontPageURL);
                    m_checkboxAllDoneShowSite.Checked = Config.OpenWebsiteOnExit;
                    m_linklabelAllDone.Text = Config.OutputFolder;
                    if (Config.FrontPageFilename != "") {
                        m_labelAllDoneStartFile.Text = String.Concat("(The front page for the website is the file ", Config.FrontPageFilename, ".", Config.HtmlExtension, ")");
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
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "ValidateCurrentPanel()");
            DialogResult result = DialogResult.OK;

            // Loop gives user the option to retry folder creation. Use return to exit.
            while (true) {
                switch (fCurrentPanel) {
                    case 2:
                        if (File.Exists(InputFile) == false) {
                            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "File not found.");

                            MessageBoxEx.Show(m_mainForm, "The file you have selected could not be found.", "File Not Found",
                                MessageBoxButtons.OK, MessageBoxIcon.Exclamation, false);

                            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "File not found. Returning. ");
                            return false;
                        }

                        ProgressWindow progressWindow = new ProgressWindow();
                        progressWindow.Text = "Reading GEDCOM file";

                        if (Config.OutputFolder == "" || Config.InputFilename != InputFile) {
                            Config.OutputFolder = Path.GetDirectoryName(InputFile);
                            Config.OutputFolder += "\\GEDmill_Output";
                        }
                        if (Config.InputFilename != InputFile) {
                            Config.InputFilename = InputFile;
                            Config.FirstRecordXRef = "";

                            // User is using a new file, so key individuals won't be the same as they were in config
                            Config.KeyIndividuals = new ArrayList();
                            Config.FirstRecordXRef = "";

                        }

                        var provider = new GEDCOMProvider(fTree);
                        provider.LoadFromFile(InputFile);
                        //m_gedcom.ProgressCallback = progressWindow;

                        LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Reading GEDCOM file " + InputFile);

                        //ThreadStart threadStart = new ThreadStart(m_gedcom.ParseFile);
                        //Thread threadWorker = new Thread(threadStart);

                        LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Starting thread");

                        //threadWorker.Start();
                        //result = progressWindow.ShowDialog(this);
                        //threadWorker.Join();

                        //LogFile.TheLogFile.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Thread finished, result=" + result.ToString());

                        if (result == DialogResult.Abort) // Abort means abnormal failure (ie. not user pressing cancel)
                        {
                            // Abort means there were file IO errors
                            MessageBoxEx.Show(m_mainForm, String.Format("A problem was encountered while reading the GEDCOM file:\r\n\r\n{0}", LogFile.Instance.ErrorReport()), MainForm.SoftwareName,
                                MessageBoxButtons.OK, MessageBoxIcon.Exclamation, true);
                        } else if (result == DialogResult.Retry) // Something went wrong, let user retry
                          {
                            // Currently the only thing this can be is "file already open"
                            MessageBoxEx.Show(m_mainForm, "A problem was encountered while reading the GEDCOM file.\r\n\r\nPerhaps the file is already open elsewhere.", MainForm.SoftwareName,
                                MessageBoxButtons.OK, MessageBoxIcon.Exclamation, true);
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
                            // Already done on click event: ((CListableBool)((ListViewItem)li)).SetRestricted( !bChecked );
                            if (!isChecked) {
                                fTree.RestrictAssociatedSources((GEDCOMIndividualRecord)((CListableBool)li).Record);
                            }
                        }

                        if (somethingChecked == false) {
                            MessageBoxEx.Show(m_mainForm, "Please select at least one individual.", "No Individuals Selected",
                                MessageBoxButtons.OK, MessageBoxIcon.Exclamation, false);
                            return false;
                        }

                        if (PrunepanelDataChanged) {
                            DialogResult dialogResult = MessageBoxEx.Show(m_mainForm, "You have made changes which will affect the website but have not saved them.\r\nWould you like to save them now?", "Save changes",
                                MessageBoxButtons.YesNo, MessageBoxIcon.Question, false);
                            if (dialogResult == DialogResult.Yes) {
                                buttonPruneRecordsSave_click(null, null);
                            }
                        }
                        return true;

                    case 4:
                        Config.SiteTitle = m_textboxSelectKey.Text;
                        return true;

                    case 5:
                        Config.OutputFolder = m_textboxChooseOutput.Text;
                        string imageFolder = Config.ImageFolder;
                        string outputFolder = Config.OutputFolder;
                        if (outputFolder != "") {
                            outputFolder = outputFolder + '\\';
                        }
                        string absImageFolder = String.Concat(outputFolder, imageFolder);

                        bool preserveFiles = false;
                        if (Config.PreserveFrontPage || Config.PreserveStylesheet) {
                            // To generate warning if deleting folder & files.
                            preserveFiles = true;
                        }

                        for (; ; )
                        {
                            result = PrepareOutputDirectory(outputFolder, preserveFiles);
                            if (result == DialogResult.Cancel) {
                                return false;
                            }
                            if (result == DialogResult.OK) {
                                break;
                            }
                        }
                        for (; ; )
                        {
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
        private void FillIndividualsList(SortableListView listView, bool bFirstColumnIsCheckbox)
        {
            var indiRecs = fTree.GetRecords<GEDCOMIndividualRecord>();
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "FillIndividualsList() : " + indiRecs.Count.ToString());

            fDisablePrunepanelCheckEvent = true;

            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            listView.Clear();

            listView.View = View.Details;
            int nameWidth = listView.Width - 70 - 70 - 20;
            if (bFirstColumnIsCheckbox) {
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
                if (bFirstColumnIsCheckbox) {
                    lbItem = new CListableBool(ir, true);
                } else {
                    string sSurname = "";
                    string sFirstName = "";
                    Config.CapitaliseName(ir.Name, ref sFirstName, ref sSurname);
                    /*if (ir.NameSuffix != null && ir.NameSuffix != "") {
                        sFirstName += ", " + ir.NameSuffix;
                    }*/
                    lbItem = new CListableBool(ir, sSurname, sFirstName, false);
                }

                SetIndividualSubItems(lbItem, ir, bFirstColumnIsCheckbox);

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
        private void SetIndividualSubItems(CListableBool lbItem, GEDCOMIndividualRecord ir, bool bCheckBoxes)
        {
            // Save checkbox state because SubItems.Clear() clears item.Text and item.Checked as well, so replace old value after calling Clear().
            bool bWasChecked = lbItem.Checked;
            string sItemText = lbItem.Text;
            lbItem.SubItems.Clear();
            lbItem.Text = sItemText;
            lbItem.Checked = bWasChecked;

            // If the list view has check boxes, the item is for the checkbox.
            // Otherwise the item is for the name, and so the sub items won't include the name.
            if (bCheckBoxes) {
                string sSurname = "";
                string sFirstName = "";
                var persName = (ir.PersonalNames.Count > 0) ? ir.PersonalNames[0].StringValue : "";
                Config.CapitaliseName(persName, ref sFirstName, ref sSurname);
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
                lbItem.SubItems.Add(new CListableNumber(nVisiblePics, String.Format("{0}/{1}", nVisiblePics, nTotalPics)));
            } else {
                lbItem.SubItems.Add(new CListableNumber(nTotalPics, String.Format("{0}", nTotalPics)));
            }
        }

        // Populates the list of source records for inclusion/exclusion in the website
        private void FillSourcesList(SortableListView listView, bool bFirstColumnIsCheckbox)
        {
            var sources = fTree.GetRecords<GEDCOMSourceRecord>();
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "FillSourcesList() : " + sources.Count.ToString());

            fDisablePrunepanelCheckEvent = true; // call to item.Checked below invokes event handler.

            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            listView.Clear();

            listView.View = View.Details;
            int nWidthTitle = listView.Width - 140 - 20;
            if (bFirstColumnIsCheckbox) {
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
                CListableBool item = new CListableBool(sr, bFirstColumnIsCheckbox);
                SetSourceSubItems(item, sr, bFirstColumnIsCheckbox);
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
        private void SetSourceSubItems(CListableBool lbItem, GEDCOMSourceRecord sr, bool bFirstColumnIsCheckbox)
        {
            // Store checkbox value because SubItems.Clear() clears item.Text and item.Checked as well!
            bool bWasChecked = lbItem.Checked;
            lbItem.SubItems.Clear();
            lbItem.Checked = bWasChecked;

            if (bFirstColumnIsCheckbox) {
                // First nColumn (ie. item) is checkbox, so first sub-item is title.
                lbItem.SubItems.Add(new CListableSource(sr));
            }

            string sRepositories = "";
            foreach (GEDCOMRepositoryCitation src in sr.RepositoryCitations) {
                GEDCOMRepositoryRecord rr = src.Value as GEDCOMRepositoryRecord;
                if (rr != null) {
                    if (!string.IsNullOrEmpty(rr.RepositoryName)) {
                        if (sRepositories != "") {
                            sRepositories += ", ";
                        }
                        sRepositories += rr.RepositoryName;
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

            lbItem.SubItems.Add(new ListViewItem.ListViewSubItem(lbItem, nCitations.ToString()));
            lbItem.SubItems.Add(new ListViewItem.ListViewSubItem(lbItem, nBirths.ToString()));
            lbItem.SubItems.Add(new ListViewItem.ListViewSubItem(lbItem, nMarriages.ToString()));
            lbItem.SubItems.Add(new ListViewItem.ListViewSubItem(lbItem, nDeaths.ToString()));
            lbItem.SubItems.Add(new CListableString(sr.XRef));

            int nVisiblePics = sr.CountVisibleMFRs();
            int nTotalPics = sr.CountAllMFRs();

            if (nVisiblePics != nTotalPics) {
                lbItem.SubItems.Add(new CListableNumber(nVisiblePics, String.Format("{0}/{1}", nVisiblePics, nTotalPics)));
            } else {
                lbItem.SubItems.Add(new CListableNumber(nTotalPics, String.Format("{0}", nTotalPics)));
            }
            lbItem.Checked = bWasChecked;

        }

        // Used to display the finished website. Uses whatever app the user has assigned to open HTML files.
        private void OpenURL(string sURL)
        {
            try {
                System.Diagnostics.Process.Start(sURL);
            } catch (Exception e2) {
                LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, String.Format("Caught exception while opening finished webpages : {0}", e2.ToString()));
            }
        }

        // Spawns the website creation thread, which calls CWebsite.Create to do the work.
        private bool CreateWebsite(string outputFolder, string imagesFolder)
        {
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Creating website");
            DialogResult dialogResult;

            // If user has specified a background image, check it exists
            if (Config.BackgroundImage != null && Config.BackgroundImage.Length != 0 && File.Exists(Config.BackgroundImage) == false) {
                LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Warning, "Can't find background image " + Config.BackgroundImage);

                dialogResult = MessageBoxEx.Show(m_mainForm, string.Format("The file {0} is missing. \r\nPages will be created without any background image.", Config.BackgroundImage),
                    "Creating website", MessageBoxButtons.OKCancel, MessageBoxIcon.Exclamation, false);
                if (dialogResult == DialogResult.Cancel) {
                    LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Message box cancelled (1)");
                    return false;
                }
            }

            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Starting progress window");

            ProgressWindow progressWindow = new ProgressWindow();
            progressWindow.Text = "Creating web pages";

            Website website = new Website(fTree, progressWindow);

            ThreadStart threadStart = new ThreadStart(website.Create);
            Thread threadWorker = new Thread(threadStart);

            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Starting progress thread");

            dialogResult = DialogResult.Abort;
            try {
                threadWorker.Start();
                dialogResult = progressWindow.ShowDialog(this);
            } catch (HTMLException e) {
                MessageBoxEx.Show(m_mainForm, String.Concat(e.Message), "Creating website",
                    MessageBoxButtons.OK, MessageBoxIcon.Exclamation, false);
            } finally {
                threadWorker.Join();
            }

            if (dialogResult == DialogResult.Abort) {
                LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Thread aborted");
                if (progressWindow.ThreadError.Message == "") {
                    // Abort means there were file IO errors
                    MessageBoxEx.Show(m_mainForm, String.Format("A problem was encountered while creating the website files:\r\n\r\n{0}", LogFile.Instance.ErrorReport()), MainForm.SoftwareName,
                        MessageBoxButtons.OK, MessageBoxIcon.Exclamation, true);
                } else {
                    // Abort means there were file IO errors
                    MessageBoxEx.Show(m_mainForm, progressWindow.ThreadError.Message, MainForm.SoftwareName,
                        MessageBoxButtons.OK, MessageBoxIcon.Exclamation, true);
                }
            }

            if (dialogResult != DialogResult.OK) {
                LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Dialog not OK");
                return false;
            }

            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Website create done.");

            return true;
        }

        // Enable the current page of the wizard
        private void EnableCurrentPanel(bool bEnable)
        {
            if (fConfigPanelOn) {
                m_tabcontrolConfigPanel.Enabled = bEnable;
                // Disable all other panels
                bEnable = false;
            } else {
                m_panelWelcome.Enabled = (fCurrentPanel == 1 && bEnable);
                m_panelChooseGedcom.Enabled = (fCurrentPanel == 2 && bEnable);
                m_panelPruneRecords.Enabled = (fCurrentPanel == 3 && bEnable);
                m_panelSelectKey.Enabled = (fCurrentPanel == 4 && bEnable);
                m_panelChooseOutput.Enabled = (fCurrentPanel == 5 && bEnable);
                m_panelAllDone.Enabled = (fCurrentPanel == 6 && bEnable);

                m_tabcontrolConfigPanel.Enabled = false;
            }

            m_picturebox.Enabled = bEnable;
        }

        // Displays useful information about a source record in a dialog box
        private void ShowSourceDetailsDialog(Form formParent, CListableBool lbItem, GEDCOMSourceRecord sr, bool bCanEditPictures, bool bFirstColumnIsCheckbox)
        {
        }

        // Reports any exception thrown during the prune operation
        private void ReportPruneError(System.Exception e)
        {
            MessageBoxEx.Show(m_mainForm, String.Format("A problem was encountered while navigating the tree structure:\r\n\r\n{0}", LogFile.Instance.ErrorReport()), MainForm.SoftwareName,
                MessageBoxButtons.OK, MessageBoxIcon.Exclamation, true);

            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, String.Format("Caught navigation exception : {0}", e.ToString()));
        }

        // Helper function. Compares dates to see if we know this person is alive
        private static bool IsAlive(GEDCOMDateValue dateBorn, GEDCOMDateValue dateDied, GEDCOMDateValue dateNow, GEDCOMDateValue dateThen)
        {
            if (dateDied != null) {
                if (dateDied.CompareTo(dateNow) > 0) {
                    // Have to have death date after today
                    return true;
                }
            } else if (dateBorn != null && dateBorn.CompareTo(dateThen) > 0) {
                // Have to be born after dateThen
                return true;
            }
            return false;
        }

        // Displays the statistics of the prune operation
        private void ShowPruneResult(int nExcluded, int nIncluded, string sType)
        {
            string sMsg = "";
            if (nExcluded == 0 && nIncluded == 0) {
                sMsg = "No changes made.";
            } else {
                if (nIncluded != 0) {
                    sMsg = String.Format("{0} {1}{2} checked.", nIncluded, sType, nIncluded > 1 ? "s" : "");
                }
                if (nExcluded != 0) {
                    if (sMsg != "") {
                        sMsg += "\r\n";
                    }
                    sMsg += String.Format("{0} {1}{2} unchecked.", nExcluded, sType, nExcluded > 1 ? "s" : "");
                }
            }

            MessageBoxEx.Show(this, sMsg, "Select Records", MessageBoxButtons.OK, MessageBoxIcon.Information, false);
        }

        // Displays the statistics of the remove pictures operation
        private void ShowHidePicsResult(int nHidden)
        {
            string sMsg = "";
            if (nHidden == 0) {
                sMsg = "No multimedia files hidden.";
            } else {
                if (nHidden != 0) {
                    sMsg = String.Format("{0} multimedia file{1} hidden.", nHidden, nHidden > 1 ? "s" : "");
                }

            }
            MessageBoxEx.Show(this, sMsg, "Hide Pictures", MessageBoxButtons.OK, MessageBoxIcon.Information, false);
        }

        // Initialises config panel controls
        private void LoadConfigPanelSettings()
        {
            m_textboxConfigFrontImageEdit.Text = Config.FrontPageImageFilename;
            m_textboxConfigFrontImageEdit.SelectionStart = m_textboxConfigFrontImageEdit.Text.Length;
            m_textboxConfigFrontImageEdit.SelectionLength = m_textboxConfigFrontImageEdit.Text.Length;
            m_textboxConfigBackImageEdit.Text = Config.BackgroundImage;
            m_textboxConfigBackImageEdit.SelectionStart = m_textboxConfigBackImageEdit.Text.Length;
            m_textboxConfigBackImageEdit.SelectionLength = m_textboxConfigBackImageEdit.Text.Length;
            m_textboxConfigIndiImageWidth.Text = Config.MaxImageWidth.ToString();
            m_textboxConfigIndiImageHeight.Text = Config.MaxImageHeight.ToString();
            m_textboxConfigSourceImageWidth.Text = Config.MaxSourceImageWidth.ToString();
            m_textboxConfigSourceImageHeight.Text = Config.MaxSourceImageHeight.ToString();
            m_textboxConfigThumbnailImageWidth.Text = Config.MaxThumbnailImageWidth.ToString();
            m_textboxConfigThumbnailImageHeight.Text = Config.MaxThumbnailImageHeight.ToString();
            m_comboboxConfigHtmlExtn.Items.Clear();
            m_comboboxConfigHtmlExtn.Items.AddRange(new object[] { ".htm", ".html" });
            m_comboboxConfigHtmlExtn.SelectedIndex = (Config.HtmlExtension == "html" ? 1 : 0);
            m_textboxConfigNoName.Text = Config.UnknownName;
            m_textboxConfigWithheldName.Text = Config.ConcealedName;
            m_radiobuttonConfigWithheldNameLabel.Checked = !Config.UseWithheldNames;
            m_radiobuttonConfigWithheldNameName.Checked = Config.UseWithheldNames;
            m_checkboxConfigCapNames.Checked = (Config.NameCapitalisation == 1);
            m_checkboxConfigCapEvents.Checked = Config.CapitaliseEventDescriptions;
            m_checkboxConfigHideEmails.Checked = Config.ObfuscateEmails;
            m_checkboxConfigOccupationHeadline.Checked = Config.OccupationHeadline;
            m_checkboxConfigAllowTrailingSpaces.Checked = Config.DataMayEndWithWhitespace;
            m_checkboxConfigAllowTrailingSpaces.Enabled = fCurrentPanel < 3; // Only enable this setting if we haven't loaded any GEDCOM yet.
            m_checkboxConfigShowWithheldRecords.Checked = Config.OnlyConceal;
            m_textboxConfigTabSpaces.Text = Config.TabSpaces.ToString();
            m_textboxConfigCommentary.Text = Config.CommentaryText;
            m_checkboxConfigCommentaryIsHtml.Checked = Config.CommentaryIsHtml;
            m_checkboxConfigStats.Checked = Config.ShowFrontPageStats;
            m_checkboxConfigPreserveFrontPage.Checked = Config.PreserveFrontPage;
            m_checkboxConfigPreserveStylesheet.Checked = Config.PreserveStylesheet;
            m_checkboxConfigIncludeHelppage.Checked = Config.IncludeHelpPage;
            m_checkboxConfigCdrom.Checked = Config.CreateCDROMFiles;
            m_checkboxConfigNonPictures.Checked = Config.AllowNonPictures;
            m_checkboxConfigIndiImages.Checked = Config.AllowMultipleImages;
            m_checkboxConfigTreeDiagrams.Checked = Config.ShowMiniTrees;
            m_checkboxConfigTreeDiagramsFakeBg.Checked = Config.FakeMiniTreeTransparency;
            m_textboxConfigEmail.Text = Config.UserEmailAddress;
            m_textboxConfigIndexName.Text = Config.FrontPageFilename;
            m_labelConfigIndexNameExtn.Text = "." + Config.HtmlExtension;
            m_textboxConfigStylesheetName.Text = Config.StylesheetFilename;
            if (Config.MainWebsiteLink.Length == 0) {
                m_textboxConfigUserLink.Text = "http://";
            } else {
                m_textboxConfigUserLink.Text = Config.MainWebsiteLink;
            }
            m_comboboxConfigTreeDiagramsFormat.Items.Clear();
            m_comboboxConfigTreeDiagramsFormat.Items.AddRange(new object[] { "gif", "png" });
            m_comboboxConfigTreeDiagramsFormat.SelectedIndex = (Config.MiniTreeImageFormat == "png" ? 1 : 0);
            m_checkboxConfigMultiPageIndex.Checked = Config.MultiPageIndexes;
            m_checkboxConfigUserRefInIndex.Checked = Config.IncludeUserRefInIndex;
            m_textboxConfigMultiPageIndexNumber.Text = Config.IndividualsPerIndexPage.ToString();
            m_checkboxConfigKeepOriginals.Checked = Config.LinkOriginalPicture;
            m_checkboxConfigRenameOriginals.Checked = Config.RenameOriginalPicture;
            m_checkboxConfigW3C.Checked = Config.IncludeValiditySticker;
            m_checkboxConfigUserRecFilename.Checked = Config.UserRecFilename;
            m_textboxConfigCustomFooter.Text = Config.CustomFooter;
            m_checkboxConfigFooterIsHtml.Checked = Config.FooterIsHtml;
            m_checkboxConfigConserveTreeWidth.Checked = Config.ConserveTreeWidth;
            m_checkboxConfigKeepSiblingOrder.Checked = Config.KeepSiblingOrder;
            m_checkboxConfigAllowMultimedia.Checked = Config.AllowMultimedia;

            m_colorConfigMiniTreeBranch = Paintbox.ConvertColour(Config.MiniTreeColourBranch);
            m_colorConfigMiniTreeIndiBorder = Paintbox.ConvertColour(Config.MiniTreeColourIndiBorder);
            m_colorConfigMiniTreeIndiBackground = Paintbox.ConvertColour(Config.MiniTreeColourIndiBackground);
            m_colorConfigMiniTreeIndiHighlight = Paintbox.ConvertColour(Config.MiniTreeColourIndiHighlight);
            m_colorConfigMiniTreeIndiBgConcealed = Paintbox.ConvertColour(Config.MiniTreeColourIndiBgConcealed);
            m_colorConfigMiniTreeIndiFgConcealed = Paintbox.ConvertColour(Config.MiniTreeColourIndiFgConcealed);
            m_colorConfigMiniTreeIndiShade = Paintbox.ConvertColour(Config.MiniTreeColourIndiShade);
            m_colorConfigMiniTreeIndiText = Paintbox.ConvertColour(Config.MiniTreeColourIndiText);
            m_colorConfigMiniTreeIndiLink = Paintbox.ConvertColour(Config.MiniTreeColourIndiLink);
            m_colorConfigMiniTreeBackground = Paintbox.ConvertColour(Config.MiniTreeColourBackground);

            m_checkboxConfigUseBom.Checked = Config.UseBom;
            m_checkboxConfigSupressBackreferences.Checked = !Config.SupressBackreferences;

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
            Config.FrontPageImageFilename = m_textboxConfigFrontImageEdit.Text;
            Config.BackgroundImage = m_textboxConfigBackImageEdit.Text;

            try {
                // Sanity check value
                uint uMaxImageWidth = System.UInt32.Parse(m_textboxConfigIndiImageWidth.Text);
                if (uMaxImageWidth > 0 && uMaxImageWidth <= 300) {
                    Config.MaxImageWidth = uMaxImageWidth;
                } else if (Config.MaxImageWidth != uMaxImageWidth && uMaxImageWidth > 300) {
                    DialogResult dialogResult = MessageBoxEx.Show(m_mainForm,
                        String.Format("Setting the image width to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", uMaxImageWidth),
                        "Change settings",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question, false);
                    if (dialogResult == DialogResult.Yes) {
                        Config.MaxImageWidth = uMaxImageWidth;
                    }

                }
            } catch (System.Exception) {
                // Leave value unchanged
            };

            try {
                // Sanity check value
                uint uMaxImageHeight = System.UInt32.Parse(m_textboxConfigIndiImageHeight.Text);
                if (uMaxImageHeight > 0 && uMaxImageHeight <= 800) {
                    Config.MaxImageHeight = uMaxImageHeight;
                } else if (Config.MaxImageHeight != uMaxImageHeight && uMaxImageHeight > 800) {
                    DialogResult dialogResult = MessageBoxEx.Show(m_mainForm,
                        String.Format("Setting the image height to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", uMaxImageHeight),
                        "Change settings",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question, false);
                    if (dialogResult == DialogResult.Yes) {
                        Config.MaxImageHeight = uMaxImageHeight;
                    }

                }

            } catch (System.Exception) {
                // Leave value unchanged
            };

            try {
                // Sanity check value
                uint uMaxSourceImageWidth = System.UInt32.Parse(m_textboxConfigSourceImageWidth.Text);
                if (uMaxSourceImageWidth > 0 && uMaxSourceImageWidth <= 800) {
                    Config.MaxSourceImageWidth = uMaxSourceImageWidth;
                } else if (Config.MaxSourceImageWidth != uMaxSourceImageWidth && uMaxSourceImageWidth > 800) {
                    DialogResult dialogResult = MessageBoxEx.Show(m_mainForm,
                        String.Format("Setting the source width to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", uMaxSourceImageWidth),
                        "Change settings",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question, false);
                    if (dialogResult == DialogResult.Yes) {
                        Config.MaxSourceImageWidth = uMaxSourceImageWidth;
                    }


                }
            } catch (System.Exception) {
                // Leave value unchanged
            };

            try {
                // Sanity check value
                uint uMaxSourceImageHeight = System.UInt32.Parse(m_textboxConfigSourceImageHeight.Text);
                if (uMaxSourceImageHeight > 0 && uMaxSourceImageHeight <= 800) {
                    Config.MaxSourceImageHeight = uMaxSourceImageHeight;
                } else if (Config.MaxSourceImageHeight != uMaxSourceImageHeight && uMaxSourceImageHeight > 800) {
                    DialogResult dialogResult = MessageBoxEx.Show(m_mainForm,
                        String.Format("Setting the source height to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", uMaxSourceImageHeight),
                        "Change settings",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question, false);
                    if (dialogResult == DialogResult.Yes) {
                        Config.MaxSourceImageHeight = uMaxSourceImageHeight;
                    }
                }
            } catch (System.Exception) {
                // Leave value unchanged
            };

            try {
                // Sanity check value
                uint uMaxThumbnailImageWidth = System.UInt32.Parse(m_textboxConfigThumbnailImageWidth.Text);
                if (uMaxThumbnailImageWidth > 0 && uMaxThumbnailImageWidth < 80) {
                    Config.MaxThumbnailImageWidth = uMaxThumbnailImageWidth;
                } else if (Config.MaxThumbnailImageWidth != uMaxThumbnailImageWidth && uMaxThumbnailImageWidth > 80) {
                    DialogResult dialogResult = MessageBoxEx.Show(m_mainForm,
                        String.Format("Setting the thumbnail width to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", uMaxThumbnailImageWidth),
                        "Change settings",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question, false);
                    if (dialogResult == DialogResult.Yes) {
                        Config.MaxThumbnailImageWidth = uMaxThumbnailImageWidth;
                    }
                }
            } catch (System.Exception) {
                // Leave value unchanged
            };

            try {
                // Sanity check value
                uint uMaxThumbnailImageHeight = System.UInt32.Parse(m_textboxConfigThumbnailImageHeight.Text);
                if (uMaxThumbnailImageHeight > 0 && uMaxThumbnailImageHeight < 80) {
                    Config.MaxThumbnailImageHeight = uMaxThumbnailImageHeight;
                } else if (Config.MaxThumbnailImageHeight != uMaxThumbnailImageHeight && uMaxThumbnailImageHeight > 80) {
                    DialogResult dialogResult = MessageBoxEx.Show(m_mainForm,
                        String.Format("Setting the thumbnail height to such a large value may cause display problems in some browsers.\r\nReally set value to {0}?", uMaxThumbnailImageHeight),
                        "Change settings",
                        MessageBoxButtons.YesNo, MessageBoxIcon.Question, false);
                    if (dialogResult == DialogResult.Yes) {
                        Config.MaxThumbnailImageHeight = uMaxThumbnailImageHeight;
                    }
                }
            } catch (System.Exception) {
                // Leave value unchanged
            };

            Config.HtmlExtension = (m_comboboxConfigHtmlExtn.SelectedIndex == 1 ? "html" : "htm");
            Config.IncludeValiditySticker = m_checkboxConfigW3C.Checked;
            Config.UserRecFilename = m_checkboxConfigUserRecFilename.Checked;
            if (m_textboxConfigNoName.Text.Length > 0) {
                Config.UnknownName = m_textboxConfigNoName.Text;
                CListableName.UnknownName = Config.UnknownName;
                CListableBool.UnknownName = Config.UnknownName;
            }
            if (m_textboxConfigWithheldName.Text.Length > 0) {
                Config.ConcealedName = m_textboxConfigWithheldName.Text;
            }
            Config.UseWithheldNames = m_radiobuttonConfigWithheldNameName.Checked;
            Config.NameCapitalisation = (m_checkboxConfigCapNames.Checked ? 1 : 0);
            Config.CapitaliseEventDescriptions = m_checkboxConfigCapEvents.Checked;
            Config.ObfuscateEmails = m_checkboxConfigHideEmails.Checked;
            Config.OccupationHeadline = m_checkboxConfigOccupationHeadline.Checked;
            Config.DataMayEndWithWhitespace = m_checkboxConfigAllowTrailingSpaces.Checked;
            Config.OnlyConceal = m_checkboxConfigShowWithheldRecords.Checked;

            try {
                // Sanity check value
                uint uTabSpaces = System.UInt32.Parse(m_textboxConfigTabSpaces.Text);
                if (uTabSpaces > 0 && uTabSpaces < 64) {
                    Config.TabSpaces = uTabSpaces;
                }
            } catch (System.Exception) {
                // Leave value unchanged
            };

            Config.CommentaryText = m_textboxConfigCommentary.Text;
            Config.CommentaryIsHtml = m_checkboxConfigCommentaryIsHtml.Checked;
            Config.ShowFrontPageStats = m_checkboxConfigStats.Checked;
            Config.ShowMiniTrees = m_checkboxConfigTreeDiagrams.Checked;
            Config.FakeMiniTreeTransparency = m_checkboxConfigTreeDiagramsFakeBg.Checked;
            Config.UserEmailAddress = m_textboxConfigEmail.Text;
            Config.PreserveFrontPage = m_checkboxConfigPreserveFrontPage.Checked;
            Config.PreserveStylesheet = m_checkboxConfigPreserveStylesheet.Checked;
            Config.IncludeHelpPage = m_checkboxConfigIncludeHelppage.Checked;
            Config.CreateCDROMFiles = m_checkboxConfigCdrom.Checked;
            Config.AllowMultipleImages = m_checkboxConfigIndiImages.Checked;
            Config.AllowNonPictures = m_checkboxConfigNonPictures.Checked;
            Config.LinkOriginalPicture = m_checkboxConfigKeepOriginals.Checked;
            Config.RenameOriginalPicture = m_checkboxConfigRenameOriginals.Checked;

            // Validate and strip trailing .html or .htm in case user has put them on
            string sFrontPageFilename = m_textboxConfigIndexName.Text;
            string sFrontPageFilenameUpper = sFrontPageFilename.ToUpper();
            if (sFrontPageFilenameUpper.LastIndexOf(".HTML") >= 0) {
                sFrontPageFilename = sFrontPageFilename.Substring(0, sFrontPageFilename.Length - 5);
            } else if (sFrontPageFilenameUpper.LastIndexOf(".HTM") >= 0) {
                sFrontPageFilename = sFrontPageFilename.Substring(0, sFrontPageFilename.Length - 4);
            }
            Config.FrontPageFilename = sFrontPageFilename;

            // Validate and strip trailing .css in case user has put them on
            string sStylesheetFilename = m_textboxConfigStylesheetName.Text;
            if (sStylesheetFilename.Length > 0) {
                string sStylesheetFilenameUpper = sStylesheetFilename.ToUpper();
                if (sStylesheetFilename.LastIndexOf(".CSS") >= 0) {
                    sStylesheetFilename = sStylesheetFilename.Substring(0, sStylesheetFilename.Length - 4);
                }
                Config.StylesheetFilename = sStylesheetFilename;
            }

            // Validate and strip leading http:// in case user has it them on
            string sMainWebsiteLink = m_textboxConfigUserLink.Text;
            string sMainWebsiteLinkUpper = sMainWebsiteLink.ToUpper();

            if (sMainWebsiteLink.ToLower() == "http://") {
                // User hasn't altered default value
                sMainWebsiteLink = "";
            }

            Config.MainWebsiteLink = sMainWebsiteLink;
            Config.MiniTreeImageFormat = (m_comboboxConfigTreeDiagramsFormat.SelectedIndex == 1 ? "png" : "gif");
            Config.MultiPageIndexes = m_checkboxConfigMultiPageIndex.Checked;
            Config.IncludeUserRefInIndex = m_checkboxConfigUserRefInIndex.Checked;

            try {
                // Sanity check value
                uint uIndex = System.UInt32.Parse(m_textboxConfigMultiPageIndexNumber.Text);
                if (uIndex > 0) {
                    Config.IndividualsPerIndexPage = uIndex;
                }
            } catch (System.Exception) {
                // Leave value unchanged
            };

            string sCustomFooter = m_textboxConfigCustomFooter.Text;
            Config.CustomFooter = sCustomFooter;

            Config.FooterIsHtml = m_checkboxConfigFooterIsHtml.Checked;
            Config.ConserveTreeWidth = m_checkboxConfigConserveTreeWidth.Checked;
            Config.KeepSiblingOrder = m_checkboxConfigKeepSiblingOrder.Checked;
            Config.AllowMultimedia = m_checkboxConfigAllowMultimedia.Checked;


            Config.MiniTreeColourBranch = Paintbox.ConvertColour(m_colorConfigMiniTreeBranch);
            Config.MiniTreeColourIndiBorder = Paintbox.ConvertColour(m_colorConfigMiniTreeIndiBorder);
            Config.MiniTreeColourIndiBackground = Paintbox.ConvertColour(m_colorConfigMiniTreeIndiBackground);
            Config.MiniTreeColourIndiHighlight = Paintbox.ConvertColour(m_colorConfigMiniTreeIndiHighlight);
            Config.MiniTreeColourIndiBgConcealed = Paintbox.ConvertColour(m_colorConfigMiniTreeIndiBgConcealed);
            Config.MiniTreeColourIndiFgConcealed = Paintbox.ConvertColour(m_colorConfigMiniTreeIndiFgConcealed);
            Config.MiniTreeColourIndiShade = Paintbox.ConvertColour(m_colorConfigMiniTreeIndiShade);
            Config.MiniTreeColourIndiText = Paintbox.ConvertColour(m_colorConfigMiniTreeIndiText);
            Config.MiniTreeColourIndiLink = Paintbox.ConvertColour(m_colorConfigMiniTreeIndiLink);
            Config.MiniTreeColourBackground = Paintbox.ConvertColour(m_colorConfigMiniTreeBackground);

            Config.UseBom = m_checkboxConfigUseBom.Checked;
            Config.SupressBackreferences = !m_checkboxConfigSupressBackreferences.Checked;

        }

        // Populates the list box of individuals to link from the front page
        private void FillKeyIndividualsList()
        {
            if (Config.KeyIndividuals == null) {
                return;
            }

            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "FillKeyIndividualsList() : " + Config.KeyIndividuals.Count.ToString());

            string sSurname;
            string sFirstName;
            string sFullName;

            Cursor.Current = Cursors.WaitCursor;
            Cursor.Show();

            m_listboxSelectKey.Items.Clear();

            if (Config.KeyIndividuals != null) {
                foreach (string xref in Config.KeyIndividuals) {
                    GEDCOMIndividualRecord irKey = fTree.XRefIndex_Find(xref) as GEDCOMIndividualRecord;
                    if (irKey != null && irKey.GetVisibility()) {
                        sFirstName = "";
                        sSurname = "";
                        sFullName = Config.CapitaliseName(irKey.Name, ref sFirstName, ref sSurname);
                        if (sFullName == "") {
                            sFullName = Config.UnknownName;
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
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, String.Format("PrepareOutputDirectory({0})", sOutputFolder));

            string sMessage = "Could not access or create folder.";
            MessageBoxButtons messageBoxButtons = MessageBoxButtons.RetryCancel;
            DialogResult dialogResult = DialogResult.OK;
            bool bFailed = false;
            string sExceptionMessage = "";

            // First see if folder clashes with a file
            if (File.Exists(sOutputFolder)) {
                LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Folder clashes with file : " + sOutputFolder);

                // Earn user that file is being deleted
                dialogResult = MessageBoxEx.Show(m_mainForm, String.Format("The folder {0} needs to be created. " +
                    "\r\nThis will destroy an existing file with that name.", sOutputFolder),
                    "Creating website",
                    MessageBoxButtons.OKCancel, MessageBoxIcon.Exclamation, false);
                if (dialogResult == DialogResult.Cancel) {
                    LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Message box cancelled (2)");
                    return DialogResult.Cancel;
                }

                // Now delete output folder (which is currently a file)
                do {
                    bFailed = false;
                    LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Deleting output folder as file : " + sOutputFolder);
                    try {
                        Cursor.Current = Cursors.WaitCursor;
                        Cursor.Show();
                        File.Delete(sOutputFolder);
                    } catch (IOException e) {
                        LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught IO exception : " + e.ToString());
                        sExceptionMessage = e.Message;
                        bFailed = true;
                    } catch (System.UnauthorizedAccessException e) {
                        LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught UnauthorizedAccessException(3) : " + e.ToString());
                        sExceptionMessage = e.Message;
                        bFailed = true;
                    } catch (System.Exception e) {
                        LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught generic exception(3) : " + e.ToString());
                        sExceptionMessage = e.Message;
                        bFailed = true;
                    }

                    if (bFailed) {
                        // Catch failure, e.g. if user has dir/file open elsewhere
                        sMessage = String.Format("The file {0} could not be deleted.\r\n{1}", sOutputFolder, sExceptionMessage);
                        messageBoxButtons = MessageBoxButtons.RetryCancel;
                        dialogResult = MessageBoxEx.Show(m_mainForm, sMessage, "Creating website", messageBoxButtons, MessageBoxIcon.Exclamation, false);
                    }
                }
                while (bFailed && dialogResult == DialogResult.Retry);
                if (bFailed && dialogResult == DialogResult.Cancel) {
                    LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Message box cancelled (6)");
                    return DialogResult.Cancel;
                }
            } // End if output folder exists

            // Next see if folder already exists
            // If output folder exists, offer to delete it
            if (Directory.Exists(sOutputFolder)) {
                LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Folder exists(11) : " + sOutputFolder);

                if (HTMLFile.IsDesktop(sOutputFolder)) {
                    dialogResult = MessageBoxEx.Show(m_mainForm, "GEDmill will not allow you to create files directly on the Desktop", "Creating website", MessageBoxButtons.OK, MessageBoxIcon.Stop, false);
                    LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Desktop detected as output folder.");
                    return DialogResult.Cancel;
                }

                // Warn user that file is being deleted
                dialogResult = MessageBoxEx.Show(m_mainForm, String.Format("The folder {0} already exists.\r\nWould you like to delete any files it contains before creating the website files?", sOutputFolder),
                    "Creating website",
                    MessageBoxButtons.YesNoCancel, MessageBoxIcon.Question, false);
                if (dialogResult == DialogResult.Cancel) {
                    LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Message box cancelled (3a)");
                    return DialogResult.Cancel;
                }
                if (dialogResult == DialogResult.Yes) {
                    if (bPreserveFiles) {
                        dialogResult = MessageBoxEx.Show(m_mainForm, String.Format("WARNING: Deleting the folder {0} will not preserve any existing front page and stylesheet files.\r\nDelete folder anyway?", sOutputFolder),
                            "Creating website",
                            MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation, false);
                        if (dialogResult == DialogResult.Cancel) {
                            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Message box cancelled (3b)");
                            return DialogResult.Cancel;
                        }
                    } else {
                        dialogResult = MessageBoxEx.Show(m_mainForm, String.Format("WARNING: If the folder contains non-GEDmill files they will be deleted also.\r\nDelete folder anyway?", sOutputFolder),
                            "Creating website",
                            MessageBoxButtons.YesNoCancel, MessageBoxIcon.Exclamation, false);
                        if (dialogResult == DialogResult.Cancel) {
                            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Message box cancelled (3c)");
                            return DialogResult.Cancel;
                        }
                    }
                    if (dialogResult == DialogResult.Yes) {
                        LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Message box yes. Deleting output folder.");
                        do {
                            bFailed = false;
                            try {
                                Cursor.Current = Cursors.WaitCursor;
                                Cursor.Show();
                                if (Directory.Exists(sOutputFolder)) {
                                    Directory.Delete(sOutputFolder, true);
                                }
                            } catch (IOException e) {
                                LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught IOException(2) : " + e.ToString());
                                sExceptionMessage = e.Message;
                                bFailed = true;
                            } catch (System.UnauthorizedAccessException e) {
                                LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught UnauthorizedAccessException(2) : " + e.ToString());
                                sExceptionMessage = e.Message;
                                bFailed = true;
                            } catch (System.Exception e) {
                                LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught generic exception(2) : " + e.ToString());
                                sExceptionMessage = e.Message;
                                bFailed = true;
                            }
                            if (bFailed) {
                                // Catch failure, e.g. if user has dir/file open elsewhere
                                sMessage = String.Format("The folder {0} could not be deleted.\r\n{1}", sOutputFolder, sExceptionMessage);
                                messageBoxButtons = MessageBoxButtons.RetryCancel;
                                dialogResult = MessageBoxEx.Show(m_mainForm, sMessage, "Creating website",
                                    messageBoxButtons, MessageBoxIcon.Exclamation, false);
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
            LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Note, "Creating output folder.");
            try {
                directoryInfo = Directory.CreateDirectory(sOutputFolder);
            }
            // Order of catches is important here, due to hierarchy of exception classes.
            catch (DirectoryNotFoundException e) {
                sMessage = "The folder you have selected could not be found.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught DirectoryNotFoundException(5) : " + e.ToString());
                bFailed = true;
            } catch (ArgumentNullException e) {
                sMessage = "The folder name is missing or illegal.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught ArgumentNullException(5) : " + e.ToString());
                bFailed = true;
            } catch (PathTooLongException e) {
                sMessage = "The folder name you have selected is too long.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught PathTooLongException(5) : " + e.ToString());
                bFailed = true;
            } catch (IOException e) {
                sMessage = "The path you have selected is read-only, or the folder is not empty.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.RetryCancel; // Let them correct this outside of app
                LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught IOException(5) : " + e.ToString());
                bFailed = true;
            } catch (UnauthorizedAccessException e) {
                sMessage = "You do not have the correct permissions to access the folder.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.RetryCancel; // Let them correct this outside of app
                LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught UnauthorizedAccessException(5) : " + e.ToString());
                bFailed = true;
            } catch (ArgumentException e) {
                sMessage = "The folder name you have selected is of an illegal format.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught ArgumentException(5) : " + e.ToString());
                bFailed = true;
            } catch (NotSupportedException e) {
                sMessage = "The folder name you have selected is of an unsupported format.";
                sExceptionMessage = e.Message;
                messageBoxButtons = MessageBoxButtons.OK; // Ok meaning nothing you can do here, go back to main form.
                LogFile.Instance.WriteLine(LogFile.DT_APP, LogFile.EDebugLevel.Error, "Caught NotSupportedException(5) : " + e.ToString());
                bFailed = true;
            }

            // Handle any failure with a sMessage box
            if (bFailed) {
                dialogResult = MessageBoxEx.Show(m_mainForm, String.Concat(sMessage, "\r\n", sExceptionMessage), "Creating website", messageBoxButtons, MessageBoxIcon.Exclamation, false);

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

        // Logic to enable the BOM checkbox only if UTF8 is selected
        private void EnableBOMCheckBox()
        {
            bool bUTF8 = (m_comboboxConfigCharset.SelectedIndex == 1);
            m_checkboxConfigUseBom.Enabled = bUTF8;
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
