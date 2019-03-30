
using System;
using System.Collections;
using System.Drawing;
using System.IO;
using System.Threading;
using System.Windows.Forms;
using GEDmill.Exceptions;
using GEDmill.HTML;
using GEDmill.ListView;
using GKCommon.GEDCOM;

namespace GEDmill
{
    public partial class MainForm
    {
        private Button m_buttonNext;
        private Button m_buttonBack;
        private Button m_buttonCancel;
        private Button m_buttonSettings;
        private Button m_buttonSettingsCancel;
        private Button m_buttonHelp;
        private Panel m_panelWelcome;
        private Panel m_panelChooseGedcom;
        private Panel m_panelChooseOutput;
        private Panel m_panelPruneRecords;
        private Panel m_panelSelectKey;
        private Panel m_panelAllDone;
        private TabControl m_tabcontrolConfigPanel;
        private Label m_labelConfigFrontImageEdit;
        private TextBox m_textboxConfigFrontImageEdit;
        private Button m_buttonConfigFrontImageBrowse;
        private TextBox m_textboxConfigBackImageEdit;
        private Label m_labelConfigBackImageEdit;
        private Button m_buttonConfigBackImageBrowse;
        private Label m_labelConfigIndiImageSize;
        private Label m_labelConfigIndiImageWidth;
        private TextBox m_textboxConfigIndiImageWidth;
        private Label m_labelConfigIndiImageHeight;
        private TextBox m_textboxConfigIndiImageHeight;
        private Label m_labelConfigSourceImageSize;
        private Label m_labelConfigSourceImageWidth;
        private TextBox m_textboxConfigSourceImageWidth;
        private Label m_labelConfigSourceImageHeight;
        private TextBox m_textboxConfigSourceImageHeight;
        private Label m_labelConfigThumbnailImageSize;
        private Label m_labelConfigThumbnailImageWidth;
        private TextBox m_textboxConfigThumbnailImageWidth;
        private Label m_labelConfigThumbnailImageHeight;
        private TextBox m_textboxConfigThumbnailImageHeight;
        private Label m_labelConfigCharset;
        private ComboBox m_comboboxConfigCharset;
        private Label m_labelConfigHtmlExtn;
        private ComboBox m_comboboxConfigHtmlExtn;
        private CheckBox m_checkboxConfigW3C;
        private CheckBox m_checkboxConfigUserRecFilename;
        private Label m_labelConfigCustomFooter;
        private TextBox m_textboxConfigCustomFooter;
        private Label m_labelConfigFooterIsHtml;
        private CheckBox m_checkboxConfigFooterIsHtml;
        private CheckBox m_checkboxConfigConserveTreeWidth;
        private CheckBox m_checkboxConfigKeepSiblingOrder;
        private GroupBox m_groupboxMiniTreeColours;
        private Button m_buttonConfigMiniTreeColourIndiBackground;
        private Button m_buttonConfigMiniTreeColourIndiHighlight;
        private Button m_buttonConfigMiniTreeColourIndiBgConcealed;
        private Button m_buttonConfigMiniTreeColourIndiShade;
        private Button m_buttonConfigMiniTreeColourIndiText;
        private Button m_buttonConfigMiniTreeColourIndiLink;
        private Button m_buttonConfigMiniTreeColourBranch;
        private Button m_buttonConfigMiniTreeColourIndiBorder;
        private Button m_buttonConfigMiniTreeColourIndiFgConcealed;
        private CheckBox m_checkboxConfigAllowMultimedia;
        private Label m_labelConfigNoName;
        private TextBox m_textboxConfigNoName;
        private GroupBox m_groupboxConfigWithheldName;
        private RadioButton m_radiobuttonConfigWithheldNameLabel;
        private RadioButton m_radiobuttonConfigWithheldNameName;
        private TextBox m_textboxConfigWithheldName;
        private CheckBox m_checkboxConfigCapNames;
        private CheckBox m_checkboxConfigCapEvents;
        private CheckBox m_checkboxConfigHideEmails;
        private CheckBox m_checkboxConfigOccupationHeadline;
        private CheckBox m_checkboxConfigAllowTrailingSpaces;
        private CheckBox m_checkboxConfigShowWithheldRecords;
        private Label m_labelConfigTabSpaces;
        private TextBox m_textboxConfigTabSpaces;
        private Label m_labelConfigCommentaryIsHtml; // Opening bracket
        private CheckBox m_checkboxConfigCommentaryIsHtml;
        private Label m_labelConfigCommentary;
        private TextBox m_textboxConfigCommentary;
        private Label m_labelConfigUserLink;
        private TextBox m_textboxConfigUserLink;
        private Label m_labelConfigEmail;
        private TextBox m_textboxConfigEmail;
        private TextBox m_textboxConfigIndexName;
        private Label m_labelConfigIndexName;
        private Label m_labelConfigIndexNameExtn;
        private CheckBox m_checkboxConfigPreserveFrontPage;
        private TextBox m_textboxConfigStylesheetName;
        private Label m_labelConfigStylesheetName;
        private Label m_labelConfigStylesheetNameExtn;
        private CheckBox m_checkboxConfigPreserveStylesheet;
        private CheckBox m_checkboxConfigIncludeHelppage;
        private CheckBox m_checkboxConfigStats;
        private CheckBox m_checkboxConfigCdrom;
        private CheckBox m_checkboxConfigIndiImages;
        private CheckBox m_checkboxConfigNonPictures;
        private CheckBox m_checkboxConfigKeepOriginals;
        private CheckBox m_checkboxConfigRenameOriginals;
        private CheckBox m_checkboxConfigMultiPageIndex;
        private CheckBox m_checkboxConfigUserRefInIndex;
        private Label m_labelConfigMultiPageIndexNumber;
        private TextBox m_textboxConfigMultiPageIndexNumber;
        private CheckBox m_checkboxConfigTreeDiagrams;
        private CheckBox m_checkboxConfigTreeDiagramsFakeBg;
        private Label m_labelConfigTreeDiagramsFormat;
        private ComboBox m_comboboxConfigTreeDiagramsFormat;
        private CheckBox m_checkboxConfigUseBom;
        private CheckBox m_checkboxConfigSupressBackreferences;
        private Label m_labelWelcomeContinue;
        private Label m_labelWelcomeVersion;
        private Label m_labelWelcomeSubtitle;
        private PictureBox m_picturebox;
        private Label m_labelChooseGedcomInstructions;
        private Button m_buttonChooseGedcomBrowse;
        private Label m_labelChooseGedcom;
        private TextBox m_textboxChooseGedcom;
        private TextBox m_textboxChooseOutput;
        private Label m_labelChooseOutputInstructions;
        private Label m_labelChooseOutput;
        private Label m_labelChooseOutputContinue;
        private Button m_buttonChooseOutputBrowse;
        private Label m_labelPruneRecordsContinue;
        private SortableListView lvPruneIndividuals;
        private SortableListView lvPruneSources;
        private Label m_labelPruneRecordsInstructions;
        private Label m_labelPruneRecordsButtons;
        private Label m_labelSelectKey;
        private TextBox m_textboxSelectKey;
        private Label m_labelSelectKeyIndividuals;
        private ListBox m_listboxSelectKey;
        private Button m_buttonSelectKeyAdd;
        private Button m_buttonSelectKeyDelete;
        private Label m_labelSelectKeyInstructions;
        private Label m_labelAllDoneThankYou;
        private Label m_labelAllDoneDirectory;
        private Label m_labelAllDoneStartFile;
        private CheckBox m_checkboxAllDoneShowSite;
        private LinkLabel m_linklabelAllDone;
        private PictureBox m_pictureBoxWelcome;
        private System.ComponentModel.Container components = null;
        private ContextMenu m_contextmenuPruneRecordsIndis;
        private ContextMenu m_contextmenuPruneRecordsSources;
        private MenuItem m_menuitemPruneRecordsIndisUnconnected;
        private MenuItem m_menuitemPruneRecordsIndisDescendantsExc;
        private MenuItem m_menuitemPruneRecordsIndisDescendantsInc;
        private MenuItem m_menuitemPruneRecordsIndisAncestorsInc;
        private MenuItem m_menuitemPruneRecordsIndisAncestorsExc;
        private MenuItem m_menuitemPruneRecordsIndisDetails;
        private MenuItem m_menuitemPruneRecordsSourcesRemovePics;
        private MenuItem m_menuitemPruneRecordsSourcesDetails;
        private HelpProvider m_helpProvider;
        private TabControl m_tabcontrolPruneRecords;
        private TabPage m_tabpagePruneRecordsIndis;
        private TabPage m_tabpagePruneRecordsSources;

        private void InitializeComponent()
        {
            System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(MainForm));
            m_buttonNext = new Button();
            m_buttonBack = new Button();
            m_buttonCancel = new Button();
            m_buttonHelp = new Button();
            m_buttonSettings = new Button();
            m_buttonSettingsCancel = new Button();
            m_labelConfigFrontImageEdit = new Label();
            m_textboxConfigFrontImageEdit = new TextBox();
            m_buttonConfigFrontImageBrowse = new Button();
            m_textboxConfigBackImageEdit = new TextBox();
            m_labelConfigBackImageEdit = new Label();
            m_buttonConfigBackImageBrowse = new Button();
            m_labelConfigIndiImageSize = new Label();
            m_labelConfigIndiImageWidth = new Label();
            m_textboxConfigIndiImageWidth = new TextBox();
            m_labelConfigIndiImageHeight = new Label();
            m_textboxConfigIndiImageHeight = new TextBox();
            m_labelConfigSourceImageSize = new Label();
            m_labelConfigSourceImageWidth = new Label();
            m_textboxConfigSourceImageWidth = new TextBox();
            m_labelConfigSourceImageHeight = new Label();
            m_textboxConfigSourceImageHeight = new TextBox();
            m_labelConfigThumbnailImageSize = new Label();
            m_labelConfigThumbnailImageWidth = new Label();
            m_textboxConfigThumbnailImageWidth = new TextBox();
            m_labelConfigThumbnailImageHeight = new Label();
            m_textboxConfigThumbnailImageHeight = new TextBox();
            m_labelConfigCharset = new Label();
            m_comboboxConfigCharset = new ComboBox();
            m_labelConfigHtmlExtn = new Label();
            m_comboboxConfigHtmlExtn = new ComboBox();
            m_checkboxConfigW3C = new CheckBox();
            m_checkboxConfigUserRecFilename = new CheckBox();
            m_labelConfigCustomFooter = new Label();
            m_textboxConfigCustomFooter = new TextBox();
            m_labelConfigFooterIsHtml = new Label();
            m_checkboxConfigFooterIsHtml = new CheckBox();
            m_checkboxConfigConserveTreeWidth = new CheckBox();
            m_checkboxConfigKeepSiblingOrder = new CheckBox();
            m_groupboxMiniTreeColours = new GroupBox();
            m_buttonConfigMiniTreeColourIndiBackground = new Button();
            m_buttonConfigMiniTreeColourIndiHighlight = new Button();
            m_buttonConfigMiniTreeColourIndiBgConcealed = new Button();
            m_buttonConfigMiniTreeColourIndiShade = new Button();
            m_buttonConfigMiniTreeColourIndiText = new Button();
            m_buttonConfigMiniTreeColourIndiLink = new Button();
            m_buttonConfigMiniTreeColourBranch = new Button();
            m_buttonConfigMiniTreeColourIndiBorder = new Button();
            m_buttonConfigMiniTreeColourIndiFgConcealed = new Button();
            m_checkboxConfigAllowMultimedia = new CheckBox();
            m_checkboxConfigUseBom = new CheckBox();
            m_checkboxConfigSupressBackreferences = new CheckBox();
            m_labelConfigNoName = new Label();
            m_textboxConfigNoName = new TextBox();
            m_groupboxConfigWithheldName = new GroupBox();
            m_radiobuttonConfigWithheldNameLabel = new RadioButton();
            m_radiobuttonConfigWithheldNameName = new RadioButton();
            m_textboxConfigWithheldName = new TextBox();
            m_checkboxConfigCapNames = new CheckBox();
            m_checkboxConfigCapEvents = new CheckBox();
            m_checkboxConfigHideEmails = new CheckBox();
            m_checkboxConfigOccupationHeadline = new CheckBox();
            m_checkboxConfigAllowTrailingSpaces = new CheckBox();
            m_checkboxConfigShowWithheldRecords = new CheckBox(); ;
            m_labelConfigTabSpaces = new Label();
            m_textboxConfigTabSpaces = new TextBox();
            m_labelConfigCommentary = new Label();
            m_labelConfigCommentaryIsHtml = new Label();
            m_checkboxConfigCommentaryIsHtml = new CheckBox();
            m_textboxConfigCommentary = new TextBox();
            m_labelConfigEmail = new Label();
            m_textboxConfigEmail = new TextBox();
            m_labelConfigUserLink = new Label();
            m_textboxConfigUserLink = new TextBox();
            m_textboxConfigIndexName = new TextBox();
            m_labelConfigIndexName = new Label();
            m_labelConfigIndexNameExtn = new Label();
            m_checkboxConfigPreserveFrontPage = new CheckBox();
            m_textboxConfigStylesheetName = new TextBox();
            m_labelConfigStylesheetName = new Label();
            m_labelConfigStylesheetNameExtn = new Label();
            m_checkboxConfigPreserveStylesheet = new CheckBox();
            m_checkboxConfigIncludeHelppage = new CheckBox();
            m_checkboxConfigStats = new CheckBox();
            m_checkboxConfigTreeDiagrams = new CheckBox();
            m_checkboxConfigTreeDiagramsFakeBg = new CheckBox();
            m_labelConfigTreeDiagramsFormat = new Label();
            m_comboboxConfigTreeDiagramsFormat = new ComboBox();
            m_checkboxConfigMultiPageIndex = new CheckBox();
            m_checkboxConfigUserRefInIndex = new CheckBox();
            m_labelConfigMultiPageIndexNumber = new Label();
            m_textboxConfigMultiPageIndexNumber = new TextBox();
            m_checkboxConfigIndiImages = new CheckBox();
            m_checkboxConfigNonPictures = new CheckBox();
            m_checkboxConfigCdrom = new CheckBox();
            m_checkboxConfigRenameOriginals = new CheckBox();
            m_checkboxConfigKeepOriginals = new CheckBox();
            m_panelWelcome = new Panel();
            m_pictureBoxWelcome = new PictureBox();
            m_labelWelcomeContinue = new Label();
            m_labelWelcomeVersion = new Label();
            m_labelWelcomeSubtitle = new Label();
            m_picturebox = new PictureBox();
            m_panelChooseGedcom = new Panel();
            m_buttonChooseGedcomBrowse = new Button();
            m_labelChooseGedcom = new Label();
            m_labelChooseGedcomInstructions = new Label();
            m_textboxChooseGedcom = new TextBox();
            m_panelChooseOutput = new Panel();
            m_textboxChooseOutput = new TextBox();
            m_labelChooseOutputInstructions = new Label();
            m_labelChooseOutput = new Label();
            m_buttonChooseOutputBrowse = new Button();
            m_labelChooseOutputContinue = new Label();
            m_panelPruneRecords = new Panel();
            m_labelPruneRecordsContinue = new Label();
            lvPruneIndividuals = new SortableListView();
            lvPruneSources = new SortableListView();
            m_labelPruneRecordsInstructions = new Label();
            m_labelPruneRecordsButtons = new Label();
            m_panelSelectKey = new Panel();
            m_tabcontrolConfigPanel = new TabControl();
            m_tabcontrolPruneRecords = new TabControl();
            m_labelSelectKey = new Label();
            m_textboxSelectKey = new TextBox();
            m_labelSelectKeyInstructions = new Label();
            m_labelSelectKeyIndividuals = new Label();
            m_listboxSelectKey = new ListBox();
            m_buttonSelectKeyAdd = new Button();
            m_buttonSelectKeyDelete = new Button();
            m_panelAllDone = new Panel();
            m_checkboxAllDoneShowSite = new CheckBox();
            m_linklabelAllDone = new LinkLabel();
            m_labelAllDoneThankYou = new Label();
            m_labelAllDoneDirectory = new Label();
            m_labelAllDoneStartFile = new Label();
            m_contextmenuPruneRecordsIndis = new ContextMenu();
            m_contextmenuPruneRecordsSources = new ContextMenu();
            m_helpProvider = new HelpProvider();

            m_panelWelcome.SuspendLayout();
            m_panelChooseGedcom.SuspendLayout();
            m_panelChooseOutput.SuspendLayout();
            m_panelPruneRecords.SuspendLayout();
            m_panelSelectKey.SuspendLayout();
            m_tabcontrolConfigPanel.SuspendLayout();
            m_panelAllDone.SuspendLayout();
            SuspendLayout();

            // 
            // nextButton
            // 
            m_buttonNext.Location = new System.Drawing.Point(424, 288);
            m_buttonNext.Name = "m_buttonNext";
            m_buttonNext.TabIndex = 7;
            m_buttonNext.Text = "&Next >";
            m_buttonNext.Click += new System.EventHandler(nextButton_click);

            // 
            // backButton
            // 
            m_buttonBack.Location = new System.Drawing.Point(344, 288);
            m_buttonBack.Name = "m_buttonBack";
            m_buttonBack.TabIndex = 8;
            m_buttonBack.Text = "< &Back";
            m_buttonBack.Click += new System.EventHandler(backButton_click);

            // 
            // cancelButton
            // 
            m_buttonCancel.Location = new System.Drawing.Point(8, 288);
            m_buttonBack.Name = "m_buttonBack";
            m_buttonCancel.TabIndex = 10;
            m_buttonCancel.Text = "&Quit";
            m_buttonCancel.Click += new System.EventHandler(cancelButton_click);

            // 
            // helpButton
            // 
            m_buttonHelp.Location = new System.Drawing.Point(186, 288);
            m_buttonBack.Name = "m_buttonBack";
            m_buttonHelp.TabIndex = 11;
            m_buttonHelp.Text = "&Help";
            m_buttonHelp.Click += new System.EventHandler(helpButton_click);
            m_helpProvider.SetHelpKeyword(m_buttonHelp, "HelpButtonHelpKeyword");
            m_helpProvider.SetHelpNavigator(m_buttonHelp, HelpNavigator.TableOfContents);
            m_helpProvider.SetShowHelp(m_buttonHelp, true);

            // 
            // configButton
            // 
            m_buttonSettings.Location = new System.Drawing.Point(88, 288);
            m_buttonSettings.Name = "m_buttonSettings";
            m_buttonSettings.Size = new System.Drawing.Size(fConfigButtonSize);
            m_buttonSettings.TabIndex = 12;
            m_buttonSettings.Text = m_sConfigButtonTextOn;
            m_buttonSettings.Click += new System.EventHandler(configButton_click);

            // 
            // configCancelButton
            // 
            m_buttonSettingsCancel.Location = new System.Drawing.Point(424, 288);
            m_buttonSettingsCancel.Name = "m_buttonSettingsCancel";
            m_buttonSettingsCancel.TabIndex = 13;
            m_buttonSettingsCancel.Text = "&Cancel";
            m_buttonSettingsCancel.Click += new System.EventHandler(configCancelButton_click);
            m_buttonSettingsCancel.Visible = false; // Only visible when configPanel is on

            // 
            // Welcome Panel 
            // 
            m_panelWelcome.Controls.Add(m_pictureBoxWelcome);
            m_panelWelcome.Controls.Add(m_labelWelcomeContinue);
            m_panelWelcome.Controls.Add(m_labelWelcomeVersion);
            m_panelWelcome.Controls.Add(m_labelWelcomeSubtitle);
            m_panelWelcome.Location = new System.Drawing.Point(216, 0);
            m_panelWelcome.Name = "m_panelWelcome";
            m_panelWelcome.Size = new System.Drawing.Size(280, 272);
            m_panelWelcome.TabIndex = 6;


            // 
            // Welcome Panel PictureBox
            // 
            m_picturebox.Image = ((System.Drawing.Image)(resources.GetObject("panel1PictureBox.Image")));
            m_picturebox.Location = new System.Drawing.Point(8, 8);
            m_picturebox.Name = "m_picturebox";
            m_picturebox.Size = new System.Drawing.Size(200, 264);
            m_picturebox.TabIndex = 8;
            m_picturebox.TabStop = false;

            // 
            // Welcome Panel pictureBox1
            // 
            m_pictureBoxWelcome.Image = ((System.Drawing.Image)(resources.GetObject("title.Image")));
            m_pictureBoxWelcome.Size = new System.Drawing.Size(177, 65);
            m_pictureBoxWelcome.Location = new System.Drawing.Point(56, 50);
            m_pictureBoxWelcome.Name = "m_pictureBoxWelcome";
            m_pictureBoxWelcome.TabIndex = 8;
            m_pictureBoxWelcome.TabStop = false;

            // 
            // Welcome Panel ContinueLabel
            // 
            m_labelWelcomeContinue.Location = new System.Drawing.Point(0, 256);
            m_labelWelcomeContinue.Name = "m_labelWelcomeContinue";
            m_labelWelcomeContinue.Size = new System.Drawing.Size(268, 16);
            m_labelWelcomeContinue.TabIndex = 5;
            m_labelWelcomeContinue.Text = "To continue, click Next.";
            m_labelWelcomeContinue.TextAlign = System.Drawing.ContentAlignment.BottomRight;

            // 
            // Welcome Panel versionLabel
            // 
            m_labelWelcomeVersion.Location = new System.Drawing.Point(88, 116);
            m_labelWelcomeVersion.Name = "m_labelWelcomeVersion";
            m_labelWelcomeVersion.Size = new System.Drawing.Size(112, 16);
            m_labelWelcomeVersion.TabIndex = 4;
            m_labelWelcomeVersion.Text = "version " + MainForm.SoftwareVersion;
            m_labelWelcomeVersion.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;

            // 
            // Welcome Panel SubtitleLabel
            // 
            m_labelWelcomeSubtitle.Font = new System.Drawing.Font("Arial", 14F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
            m_labelWelcomeSubtitle.Location = new System.Drawing.Point(0, 138);
            m_labelWelcomeSubtitle.Name = "m_labelWelcomeSubtitle";
            m_labelWelcomeSubtitle.Size = new System.Drawing.Size(280, 54);
            m_labelWelcomeSubtitle.TabIndex = 3;
            m_labelWelcomeSubtitle.Text = "Family History Website Creator";
            m_labelWelcomeSubtitle.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;

            // 
            // Choose Gedcom 
            // 
            m_panelChooseGedcom.Controls.Add(m_buttonChooseGedcomBrowse);
            m_panelChooseGedcom.Controls.Add(m_labelChooseGedcom);
            m_panelChooseGedcom.Controls.Add(m_labelChooseGedcomInstructions);
            m_panelChooseGedcom.Controls.Add(m_textboxChooseGedcom);
            m_panelChooseGedcom.Location = new System.Drawing.Point(216, 0);
            m_panelChooseGedcom.Name = "m_panelChooseGedcom";
            m_panelChooseGedcom.Size = new System.Drawing.Size(280, 272);
            m_panelChooseGedcom.TabIndex = 6;

            // 
            // Choose Gedcom BrowseButton
            // 
            m_buttonChooseGedcomBrowse.Location = new System.Drawing.Point(200, 120);
            m_buttonChooseGedcomBrowse.Name = "m_buttonChooseGedcomBrowse";
            m_buttonChooseGedcomBrowse.TabIndex = 4;
            m_buttonChooseGedcomBrowse.Text = "B&rowse...";
            m_buttonChooseGedcomBrowse.Click += new System.EventHandler(buttonChooseGedcomBrowse_click);

            // 
            // Choose Gedcom EditLabel
            // 
            m_labelChooseGedcom.Location = new System.Drawing.Point(0, 96);
            m_labelChooseGedcom.Name = "m_labelChooseGedcom";
            m_labelChooseGedcom.RightToLeft = RightToLeft.No;
            m_labelChooseGedcom.Size = new System.Drawing.Size(152, 24);
            m_labelChooseGedcom.TabIndex = 3;
            m_labelChooseGedcom.Text = "&File:";
            m_labelChooseGedcom.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            // 
            // Choose Gedcom InstructionLabel
            // 
            m_labelChooseGedcomInstructions.Location = new System.Drawing.Point(0, 16);
            m_labelChooseGedcomInstructions.Name = "m_labelChooseGedcomInstructions";
            m_labelChooseGedcomInstructions.Size = new System.Drawing.Size(288, 80);
            m_labelChooseGedcomInstructions.TabIndex = 2;
            m_labelChooseGedcomInstructions.Text = "First, please select the GEDCOM file containing your family tree data. Most Family Tree software is capable of exporting its data in GEDCOM format, even though this may not be the format it usually uses.";

            // 
            // Choose Gedcom EditBox
            // 
            m_textboxChooseGedcom.Location = new System.Drawing.Point(0, 120);
            m_textboxChooseGedcom.Name = "m_textboxChooseGedcom";
            m_textboxChooseGedcom.Size = new System.Drawing.Size(192, 20);
            m_textboxChooseGedcom.TabIndex = 1;
            m_textboxChooseGedcom.Text = "";
            m_textboxChooseGedcom.TextChanged += new System.EventHandler(textboxChooseGedcom_textChanged);

            m_panelPruneRecords.Controls.Add(m_labelPruneRecordsContinue);
            m_panelPruneRecords.Controls.Add(m_labelPruneRecordsInstructions);
            m_panelPruneRecords.Controls.Add(m_labelPruneRecordsButtons);
            m_panelPruneRecords.Location = new System.Drawing.Point(0, 0);
            m_panelPruneRecords.Name = "m_panelPruneRecords";
            m_panelPruneRecords.Size = new System.Drawing.Size(496, 272);
            m_panelPruneRecords.TabIndex = 11;
            m_panelPruneRecords.Controls.Add(m_tabcontrolPruneRecords);

            // 
            // m_labelPruneRecordsContinue
            // 
            m_labelPruneRecordsContinue.Location = new System.Drawing.Point(256, 288);
            m_labelPruneRecordsContinue.Name = "m_labelPruneRecordsContinue";
            m_labelPruneRecordsContinue.Size = new System.Drawing.Size(256, 16);
            m_labelPruneRecordsContinue.TabIndex = 5;
            m_labelPruneRecordsContinue.Text = "When you have finished selecting, click Next.";

            //
            // m_pruneIndividualsContextMenu
            //
            m_menuitemPruneRecordsIndisDescendantsExc = new MenuItem("E&xclude all descendants of this person", new System.EventHandler(pruneIndividualsContextMenuDescendantsExc_Click));
            m_menuitemPruneRecordsIndisAncestorsExc = new MenuItem("Exclude all &ancestors of this person", new System.EventHandler(pruneIndividualsContextMenuAncestorsExc_Click));
            m_menuitemPruneRecordsIndisDescendantsInc = new MenuItem("In&clude all descendants of this person", new System.EventHandler(pruneIndividualsContextMenuDescendantsInc_Click));
            m_menuitemPruneRecordsIndisAncestorsInc = new MenuItem("Include all a&ncestors of this person", new System.EventHandler(pruneIndividualsContextMenuAncestorsInc_Click));
            m_menuitemPruneRecordsIndisUnconnected = new MenuItem("E&xclude individuals unless navigable from this person", new System.EventHandler(pruneIndividualsContextMenuUnconnected_Click));
            m_menuitemPruneRecordsIndisDetails = new MenuItem("&Details and pictures...", new System.EventHandler(pruneIndividualsContextMenuDetails_Click));
            m_contextmenuPruneRecordsIndis.MenuItems.Add(m_menuitemPruneRecordsIndisDetails);
            m_contextmenuPruneRecordsIndis.MenuItems.Add(new MenuItem("-"));
            m_contextmenuPruneRecordsIndis.MenuItems.Add(m_menuitemPruneRecordsIndisDescendantsExc);
            m_contextmenuPruneRecordsIndis.MenuItems.Add(m_menuitemPruneRecordsIndisDescendantsInc);
            m_contextmenuPruneRecordsIndis.MenuItems.Add(m_menuitemPruneRecordsIndisAncestorsExc);
            m_contextmenuPruneRecordsIndis.MenuItems.Add(m_menuitemPruneRecordsIndisAncestorsInc);
            m_contextmenuPruneRecordsIndis.MenuItems.Add(new MenuItem("-"));
            m_contextmenuPruneRecordsIndis.MenuItems.Add(new MenuItem("&Include everyone", new System.EventHandler(pruneIndividualsContextMenuInclude_Click)));
            m_contextmenuPruneRecordsIndis.MenuItems.Add(new MenuItem("&Exclude everyone", new System.EventHandler(pruneIndividualsContextMenuExclude_Click)));
            m_contextmenuPruneRecordsIndis.MenuItems.Add(new MenuItem("Exclude everyone still a&live (and those born in last 100 years)", new System.EventHandler(pruneIndividualsContextMenuAlive_Click)));
            m_contextmenuPruneRecordsIndis.MenuItems.Add(new MenuItem("-"));
            m_contextmenuPruneRecordsIndis.MenuItems.Add(m_menuitemPruneRecordsIndisUnconnected);
            m_contextmenuPruneRecordsIndis.Popup += new EventHandler(pruneIndividualsContextMenu_popup);

            //
            // m_pruneSourcesContextMenu
            //
            m_menuitemPruneRecordsSourcesDetails = new MenuItem("&Details and pictures...", new System.EventHandler(pruneSourcesContextMenuDetails_Click));
            m_contextmenuPruneRecordsSources.MenuItems.Add(m_menuitemPruneRecordsSourcesDetails);
            m_contextmenuPruneRecordsSources.MenuItems.Add(new MenuItem("-"));
            m_contextmenuPruneRecordsSources.MenuItems.Add(new MenuItem("&Include all sources", new System.EventHandler(pruneSourcesContextMenuInclude_Click)));
            m_contextmenuPruneRecordsSources.MenuItems.Add(new MenuItem("&Exclude all sources", new System.EventHandler(pruneSourcesContextMenuExclude_Click)));
            m_contextmenuPruneRecordsSources.MenuItems.Add(new MenuItem("-"));
            m_menuitemPruneRecordsSourcesRemovePics = new MenuItem("&Remove pictures from selected sources", new System.EventHandler(pruneSourcesContextMenuRemovePics_Click));
            m_contextmenuPruneRecordsSources.MenuItems.Add(m_menuitemPruneRecordsSourcesRemovePics);
            m_contextmenuPruneRecordsSources.Popup += new EventHandler(pruneSourcesContextMenu_popup);

            // 
            // panel3ListView
            //          
            m_tabcontrolPruneRecords.Location = new System.Drawing.Point(108, 65);
            m_tabcontrolPruneRecords.Name = "m_tabcontrolPruneRecords";
            m_tabcontrolPruneRecords.Size = new System.Drawing.Size(388, 207);
            m_tabcontrolPruneRecords.TabIndex = 4;
            m_tabpagePruneRecordsIndis = new TabPage("Individuals");
            m_tabcontrolPruneRecords.TabPages.Add(m_tabpagePruneRecordsIndis);
            m_tabpagePruneRecordsSources = new TabPage("Sources");
            m_tabcontrolPruneRecords.TabPages.Add(m_tabpagePruneRecordsSources);

            // 
            // panel3ListView
            // 
            lvPruneIndividuals.CheckBoxes = true;
            lvPruneIndividuals.Location = new System.Drawing.Point(0, 0);
            lvPruneIndividuals.Name = "m_listviewPruneRecordsIndis";
            lvPruneIndividuals.Size = new System.Drawing.Size(381, 181);
            lvPruneIndividuals.TabIndex = 4;
            lvPruneIndividuals.View = View.Details;
            lvPruneIndividuals.ColumnClick += new ColumnClickEventHandler(lvPruneIndividuals.ColumnClickHandler);
            lvPruneIndividuals.ItemCheck += new ItemCheckEventHandler(lvPruneIndividuals_ItemCheck);
            lvPruneIndividuals.ContextMenu = m_contextmenuPruneRecordsIndis;
            lvPruneIndividuals.FullRowSelect = true;
            lvPruneIndividuals.GridLines = true;
            lvPruneIndividuals.AllowColumnReorder = true;
            m_tabpagePruneRecordsIndis.Controls.Add(lvPruneIndividuals);

            // 
            // panel3ListView2
            // 
            lvPruneSources.CheckBoxes = true;
            lvPruneSources.Location = new System.Drawing.Point(0, 0);
            lvPruneSources.Name = "m_listviewPruneRecordsSources";
            lvPruneSources.Size = new System.Drawing.Size(381, 181);
            lvPruneSources.TabIndex = 4;
            lvPruneSources.View = View.Details;
            lvPruneSources.ColumnClick += new ColumnClickEventHandler(lvPruneSources.ColumnClickHandler);
            lvPruneSources.ItemCheck += new ItemCheckEventHandler(listviewPruneRecordsSources_ItemCheck);
            lvPruneSources.ContextMenu = m_contextmenuPruneRecordsSources;
            lvPruneSources.FullRowSelect = true;
            lvPruneSources.GridLines = true;
            lvPruneSources.AllowColumnReorder = true;
            m_tabpagePruneRecordsSources.Controls.Add(lvPruneSources);

            // 
            // panel3InstructionLabel
            // 
            m_labelPruneRecordsInstructions.Location = new System.Drawing.Point(8, 16);
            m_labelPruneRecordsInstructions.Name = "m_labelPruneRecordsInstructions";
            m_labelPruneRecordsInstructions.Size = new System.Drawing.Size(488, 45);
            m_labelPruneRecordsInstructions.TabIndex = 3;
            m_labelPruneRecordsInstructions.Text = "Now, you can specify any individuals and sources you don\'t want to appear in the website. " +
                "Clear the box next to their name to prevent them from appearing - those left ticked " +
                "will appear.";

            // 
            // panel3ButtonsLabel
            // 
            m_labelPruneRecordsButtons.Location = new System.Drawing.Point(8, 70);
            m_labelPruneRecordsButtons.Name = "m_labelPruneRecordsButtons";
            m_labelPruneRecordsButtons.Size = new System.Drawing.Size(92, 95);
            m_labelPruneRecordsButtons.TabIndex = 9;
            m_labelPruneRecordsButtons.Text = "Right-click on the list for more options, including adding pictures...";

            // 
            // Key Indidivuals panel
            // 
            m_panelSelectKey.Controls.Add(m_labelSelectKey);
            m_panelSelectKey.Controls.Add(m_textboxSelectKey);
            m_panelSelectKey.Controls.Add(m_labelSelectKeyIndividuals);
            m_panelSelectKey.Controls.Add(m_listboxSelectKey);
            m_panelSelectKey.Controls.Add(m_buttonSelectKeyAdd);
            m_panelSelectKey.Controls.Add(m_buttonSelectKeyDelete);
            m_panelSelectKey.Controls.Add(m_labelSelectKeyInstructions);
            m_panelSelectKey.Location = new System.Drawing.Point(216, 0);
            m_panelSelectKey.Name = "m_panelSelectKey";
            m_panelSelectKey.Size = new System.Drawing.Size(280, 272);
            m_panelSelectKey.TabIndex = 12;

            // 
            // Key Indidivuals EditLabel1
            // 
            m_labelSelectKey.Location = new System.Drawing.Point(0, 120);
            m_labelSelectKey.Name = "panel4EditLabel1";
            m_labelSelectKey.Size = new System.Drawing.Size(184, 24);
            m_labelSelectKey.TabIndex = 2;
            m_labelSelectKey.Text = "&Website Title:";
            m_labelSelectKey.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            // 
            // Key Indidivuals EditBox1
            // 
            m_textboxSelectKey.Location = new System.Drawing.Point(0, 144);
            m_textboxSelectKey.Name = "panel4EditBox1";
            m_textboxSelectKey.Size = new System.Drawing.Size(274, 20);
            m_textboxSelectKey.TabIndex = 1;
            m_textboxSelectKey.Text = "";

            // 
            // Key Indidivuals Label
            // 
            m_labelSelectKeyIndividuals.Location = new System.Drawing.Point(0, 182);
            m_labelSelectKeyIndividuals.Name = "panel4KeyIndividualsLabel";
            m_labelSelectKeyIndividuals.Size = new System.Drawing.Size(184, 24);
            m_labelSelectKeyIndividuals.TabIndex = 3;
            m_labelSelectKeyIndividuals.Text = "&Key Individuals:";
            m_labelSelectKeyIndividuals.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            // 
            // Key Indidivuals ListBox
            // 
            m_listboxSelectKey.Location = new System.Drawing.Point(0, 206);
            m_listboxSelectKey.Name = "panel4KeyIndividualsListBox";
            m_listboxSelectKey.Size = new System.Drawing.Size(192, 68);
            m_listboxSelectKey.TabIndex = 4;
            m_listboxSelectKey.Text = "";
            m_listboxSelectKey.SelectedValueChanged += new System.EventHandler(listboxSelectKey_selectedValueChanged);

            // 
            // Key Indidivuals Add Button
            // 
            m_buttonSelectKeyAdd.Location = new System.Drawing.Point(200, 206);
            m_buttonSelectKeyAdd.Name = "panel4KeyIndividualsAddButton";
            m_buttonSelectKeyAdd.TabIndex = 6;
            m_buttonSelectKeyAdd.Text = "&Add...";
            m_buttonSelectKeyAdd.Click += new System.EventHandler(buttonSelectKeyAdd_click);

            // 
            // Key Indidivuals Delete Button
            // 
            m_buttonSelectKeyDelete.Location = new System.Drawing.Point(200, 236);
            m_buttonSelectKeyDelete.Name = "panel4KeyIndividualsDeleteButton";
            m_buttonSelectKeyDelete.TabIndex = 7;
            m_buttonSelectKeyDelete.Text = "&Remove";
            m_buttonSelectKeyDelete.Click += new System.EventHandler(buttonSelectKeyDelete_click);

            // 
            // Key Indidivuals Instruction Label
            // 
            m_labelSelectKeyInstructions.Location = new System.Drawing.Point(0, 16);
            m_labelSelectKeyInstructions.Name = "panel4InstructionLabel";
            m_labelSelectKeyInstructions.Size = new System.Drawing.Size(288, 96);
            m_labelSelectKeyInstructions.TabIndex = 0;
            m_labelSelectKeyInstructions.Text = "Next, you can choose a title for the front page of your website. " +
                "Leave it blank if you don\'t want a title.";
            m_labelSelectKeyInstructions.Text += "\r\n\r\nYou can also select which people feature as key individuals on the front page.";

            InitialiseSettingsWebpagesPane();

            InitialiseSettingsImagesPane();

            InitialiseSettingsGedcomPane();

            InitialiseSettingsTreeDiagramsPane();

            InitialiseSettingsAdvancedPane();

            // 
            // Choose Output panel
            // 
            m_panelChooseOutput.Controls.Add(m_textboxChooseOutput);
            m_panelChooseOutput.Controls.Add(m_labelChooseOutputInstructions);
            m_panelChooseOutput.Controls.Add(m_labelChooseOutput);
            m_panelChooseOutput.Controls.Add(m_buttonChooseOutputBrowse);
            m_panelChooseOutput.Controls.Add(m_labelChooseOutputContinue);
            m_panelChooseOutput.Location = new System.Drawing.Point(216, 0);
            m_panelChooseOutput.Name = "m_panelChooseOutput";
            m_panelChooseOutput.Size = new System.Drawing.Size(280, 272);
            m_panelChooseOutput.TabIndex = 11;

            // 
            // Choose Output EditBox
            // 
            m_textboxChooseOutput.Location = new System.Drawing.Point(0, 120);
            m_textboxChooseOutput.Name = "m_textboxChooseOutput";
            m_textboxChooseOutput.Size = new System.Drawing.Size(192, 20);
            m_textboxChooseOutput.TabIndex = 4;
            m_textboxChooseOutput.Text = "";
            m_textboxChooseOutput.TextChanged += new System.EventHandler(textboxChooseOutput_textChanged);

            // 
            // m_labelChooseOutput
            // 
            m_labelChooseOutput.Location = new System.Drawing.Point(0, 96);
            m_labelChooseOutput.Name = "m_labelChooseOutput";
            m_labelChooseOutput.RightToLeft = RightToLeft.No;
            m_labelChooseOutput.Size = new System.Drawing.Size(152, 24);
            m_labelChooseOutput.TabIndex = 5;
            m_labelChooseOutput.Text = "&Folder:";
            m_labelChooseOutput.TextAlign = System.Drawing.ContentAlignment.BottomLeft;

            // 
            // m_buttonChooseOutputBrowse
            // 
            m_buttonChooseOutputBrowse.Location = new System.Drawing.Point(200, 120);
            m_buttonChooseOutputBrowse.Name = "m_buttonChooseOutputBrowse";
            m_buttonChooseOutputBrowse.TabIndex = 6;
            m_buttonChooseOutputBrowse.Text = "B&rowse...";
            m_buttonChooseOutputBrowse.Click += new System.EventHandler(buttonChooseOutputBrowse_click);

            // 
            // m_labelChooseOutputInstructions
            // 
            m_labelChooseOutputInstructions.Location = new System.Drawing.Point(0, 16);
            m_labelChooseOutputInstructions.Name = "m_labelChooseOutputInstructions";
            m_labelChooseOutputInstructions.Size = new System.Drawing.Size(280, 80);
            m_labelChooseOutputInstructions.TabIndex = 3;
            m_labelChooseOutputInstructions.Text = "Finally, select the folder where you wish to the website files to be created. If " +
                "the folder doesn\'t exist already it will be created for you.";

            // 
            // m_labelChooseOutputContinue
            // 
            m_labelChooseOutputContinue.Location = new System.Drawing.Point(256, 288);
            m_labelChooseOutputContinue.Name = "m_labelChooseOutputContinue";
            m_labelChooseOutputContinue.Size = new System.Drawing.Size(256, 16);
            m_labelChooseOutputContinue.TabIndex = 15;
            m_labelChooseOutputContinue.Text = "Click Next to create the web pages...";

            // 
            // m_panelAllDone
            // 
            m_panelAllDone.Controls.Add(m_checkboxAllDoneShowSite);
            m_panelAllDone.Controls.Add(m_linklabelAllDone);
            m_panelAllDone.Controls.Add(m_labelAllDoneThankYou);
            m_panelAllDone.Controls.Add(m_labelAllDoneDirectory);
            m_panelAllDone.Controls.Add(m_labelAllDoneStartFile);
            m_panelAllDone.Location = new System.Drawing.Point(216, 0);
            m_panelAllDone.Name = "panel6";
            m_panelAllDone.Size = new System.Drawing.Size(280, 272);
            m_panelAllDone.TabIndex = 12;

            //
            // m_checkboxAllDoneShowSite
            // 
            m_checkboxAllDoneShowSite.Location = new System.Drawing.Point(0, 250);
            m_checkboxAllDoneShowSite.Name = "panel6WebsiteCheckBox";
            m_checkboxAllDoneShowSite.Size = new System.Drawing.Size(288, 24);
            m_checkboxAllDoneShowSite.TabIndex = 7;
            m_checkboxAllDoneShowSite.Text = "&Display web pages after program finishes.";

            // 
            // m_linklabelAllDone
            // 
            m_linklabelAllDone.Location = new System.Drawing.Point(0, 52);
            m_linklabelAllDone.Name = "panel6FolderLink";
            m_linklabelAllDone.Size = new System.Drawing.Size(288, 48);
            m_linklabelAllDone.TabIndex = 7;
            m_linklabelAllDone.TabStop = true;
            m_linklabelAllDone.Text = "<path>";
            m_linklabelAllDone.TextAlign = System.Drawing.ContentAlignment.TopLeft;
            m_linklabelAllDone.LinkClicked += new LinkLabelLinkClickedEventHandler(linklabelAllDone_click);

            // 
            // m_labelAllDoneThankYou
            // 
            m_labelAllDoneThankYou.Location = new System.Drawing.Point(0, 230);
            m_labelAllDoneThankYou.Name = "label1";
            m_labelAllDoneThankYou.Size = new System.Drawing.Size(288, 24);
            m_labelAllDoneThankYou.TabIndex = 3;
            m_labelAllDoneThankYou.Text = "Thank you for using GEDmill.";
            m_labelAllDoneThankYou.TextAlign = System.Drawing.ContentAlignment.TopLeft;

            // 
            // m_labelAllDoneDirectory
            // 
            m_labelAllDoneDirectory.Location = new System.Drawing.Point(0, 16);
            m_labelAllDoneDirectory.Name = "label3";
            m_labelAllDoneDirectory.Size = new System.Drawing.Size(280, 48);
            m_labelAllDoneDirectory.TabIndex = 0;
            m_labelAllDoneDirectory.Text = "The website files have been generated and put in ";
            m_labelAllDoneDirectory.TextAlign = System.Drawing.ContentAlignment.TopLeft;

            // 
            // m_labelAllDoneStartFile
            // 
            m_labelAllDoneStartFile.Location = new System.Drawing.Point(0, 104);
            m_labelAllDoneStartFile.Name = "label3a";
            m_labelAllDoneStartFile.Size = new System.Drawing.Size(288, 48);
            m_labelAllDoneStartFile.TabIndex = 0;
            m_labelAllDoneStartFile.Text = ""; // Filled in programatically to say "(The front page for the website is the file home.html)"
            m_labelAllDoneStartFile.TextAlign = System.Drawing.ContentAlignment.TopLeft;

            TabPage tabPageSettingsWebpages = new TabPage("Webpages");
            tabPageSettingsWebpages.Controls.Add(m_labelConfigCommentary);
            tabPageSettingsWebpages.Controls.Add(m_textboxConfigCommentary);
            tabPageSettingsWebpages.Controls.Add(m_labelConfigCommentaryIsHtml);
            tabPageSettingsWebpages.Controls.Add(m_checkboxConfigCommentaryIsHtml);
            tabPageSettingsWebpages.Controls.Add(m_labelConfigEmail);
            tabPageSettingsWebpages.Controls.Add(m_textboxConfigEmail);
            tabPageSettingsWebpages.Controls.Add(m_labelConfigUserLink);
            tabPageSettingsWebpages.Controls.Add(m_textboxConfigUserLink);
            tabPageSettingsWebpages.Controls.Add(m_textboxConfigIndexName);
            tabPageSettingsWebpages.Controls.Add(m_labelConfigIndexName);
            tabPageSettingsWebpages.Controls.Add(m_labelConfigIndexNameExtn);
            tabPageSettingsWebpages.Controls.Add(m_checkboxConfigPreserveFrontPage);
            tabPageSettingsWebpages.Controls.Add(m_checkboxConfigStats);
            tabPageSettingsWebpages.Controls.Add(m_checkboxConfigMultiPageIndex);
            tabPageSettingsWebpages.Controls.Add(m_checkboxConfigUserRefInIndex);
            tabPageSettingsWebpages.Controls.Add(m_labelConfigMultiPageIndexNumber);
            tabPageSettingsWebpages.Controls.Add(m_textboxConfigMultiPageIndexNumber);
            tabPageSettingsWebpages.Controls.Add(m_checkboxConfigCdrom);
            tabPageSettingsWebpages.Controls.Add(m_labelConfigCustomFooter);
            tabPageSettingsWebpages.Controls.Add(m_textboxConfigCustomFooter);
            tabPageSettingsWebpages.Controls.Add(m_labelConfigFooterIsHtml);
            tabPageSettingsWebpages.Controls.Add(m_checkboxConfigFooterIsHtml);
            m_tabcontrolConfigPanel.TabPages.Add(tabPageSettingsWebpages);

            TabPage tabPageSettingsImages = new TabPage("Images");
            tabPageSettingsImages.Controls.Add(m_labelConfigFrontImageEdit);
            tabPageSettingsImages.Controls.Add(m_textboxConfigFrontImageEdit);
            tabPageSettingsImages.Controls.Add(m_buttonConfigFrontImageBrowse);
            tabPageSettingsImages.Controls.Add(m_textboxConfigBackImageEdit);
            tabPageSettingsImages.Controls.Add(m_labelConfigBackImageEdit);
            tabPageSettingsImages.Controls.Add(m_buttonConfigBackImageBrowse);
            tabPageSettingsImages.Controls.Add(m_labelConfigIndiImageSize);
            tabPageSettingsImages.Controls.Add(m_labelConfigIndiImageWidth);
            tabPageSettingsImages.Controls.Add(m_textboxConfigIndiImageWidth);
            tabPageSettingsImages.Controls.Add(m_labelConfigIndiImageHeight);
            tabPageSettingsImages.Controls.Add(m_textboxConfigIndiImageHeight);
            tabPageSettingsImages.Controls.Add(m_labelConfigSourceImageSize);
            tabPageSettingsImages.Controls.Add(m_labelConfigSourceImageWidth);
            tabPageSettingsImages.Controls.Add(m_textboxConfigSourceImageWidth);
            tabPageSettingsImages.Controls.Add(m_labelConfigSourceImageHeight);
            tabPageSettingsImages.Controls.Add(m_textboxConfigSourceImageHeight);
            tabPageSettingsImages.Controls.Add(m_labelConfigThumbnailImageSize);
            tabPageSettingsImages.Controls.Add(m_labelConfigThumbnailImageWidth);
            tabPageSettingsImages.Controls.Add(m_textboxConfigThumbnailImageWidth);
            tabPageSettingsImages.Controls.Add(m_labelConfigThumbnailImageHeight);
            tabPageSettingsImages.Controls.Add(m_textboxConfigThumbnailImageHeight);
            tabPageSettingsImages.Controls.Add(m_checkboxConfigIndiImages);
            tabPageSettingsImages.Controls.Add(m_checkboxConfigNonPictures);
            tabPageSettingsImages.Controls.Add(m_checkboxConfigRenameOriginals);
            tabPageSettingsImages.Controls.Add(m_checkboxConfigKeepOriginals);
            tabPageSettingsImages.Controls.Add(m_checkboxConfigAllowMultimedia);
            m_tabcontrolConfigPanel.TabPages.Add(tabPageSettingsImages);

            TabPage tabPageSettingsGedcom = new TabPage("GEDCOM");
            tabPageSettingsGedcom.Controls.Add(m_labelConfigNoName);
            tabPageSettingsGedcom.Controls.Add(m_textboxConfigNoName);
            m_groupboxConfigWithheldName.Controls.Add(m_radiobuttonConfigWithheldNameLabel);
            m_groupboxConfigWithheldName.Controls.Add(m_textboxConfigWithheldName);
            m_groupboxConfigWithheldName.Controls.Add(m_radiobuttonConfigWithheldNameName);
            tabPageSettingsGedcom.Controls.Add(m_groupboxConfigWithheldName);
            tabPageSettingsGedcom.Controls.Add(m_checkboxConfigCapNames);
            tabPageSettingsGedcom.Controls.Add(m_checkboxConfigCapEvents);
            tabPageSettingsGedcom.Controls.Add(m_checkboxConfigHideEmails);
            tabPageSettingsGedcom.Controls.Add(m_checkboxConfigOccupationHeadline);
            tabPageSettingsGedcom.Controls.Add(m_checkboxConfigAllowTrailingSpaces);
            tabPageSettingsGedcom.Controls.Add(m_checkboxConfigShowWithheldRecords);
            tabPageSettingsGedcom.Controls.Add(m_labelConfigTabSpaces);
            tabPageSettingsGedcom.Controls.Add(m_textboxConfigTabSpaces);
            m_tabcontrolConfigPanel.TabPages.Add(tabPageSettingsGedcom);

            TabPage tabPageSettingsTreeDiagrams = new TabPage("Tree Diagrams");
            tabPageSettingsTreeDiagrams.Controls.Add(m_checkboxConfigTreeDiagrams);
            tabPageSettingsTreeDiagrams.Controls.Add(m_checkboxConfigTreeDiagramsFakeBg);
            tabPageSettingsTreeDiagrams.Controls.Add(m_labelConfigTreeDiagramsFormat);
            tabPageSettingsTreeDiagrams.Controls.Add(m_comboboxConfigTreeDiagramsFormat);
            tabPageSettingsTreeDiagrams.Controls.Add(m_checkboxConfigConserveTreeWidth);
            tabPageSettingsTreeDiagrams.Controls.Add(m_checkboxConfigKeepSiblingOrder);
            m_groupboxMiniTreeColours.Controls.Add(m_buttonConfigMiniTreeColourIndiBackground);
            m_groupboxMiniTreeColours.Controls.Add(m_buttonConfigMiniTreeColourIndiHighlight);
            m_groupboxMiniTreeColours.Controls.Add(m_buttonConfigMiniTreeColourIndiBgConcealed);
            m_groupboxMiniTreeColours.Controls.Add(m_buttonConfigMiniTreeColourIndiShade);
            m_groupboxMiniTreeColours.Controls.Add(m_buttonConfigMiniTreeColourIndiText);
            m_groupboxMiniTreeColours.Controls.Add(m_buttonConfigMiniTreeColourIndiLink);
            m_groupboxMiniTreeColours.Controls.Add(m_buttonConfigMiniTreeColourBranch);
            m_groupboxMiniTreeColours.Controls.Add(m_buttonConfigMiniTreeColourIndiBorder);
            m_groupboxMiniTreeColours.Controls.Add(m_buttonConfigMiniTreeColourIndiFgConcealed);
            tabPageSettingsTreeDiagrams.Controls.Add(m_groupboxMiniTreeColours);
            m_tabcontrolConfigPanel.TabPages.Add(tabPageSettingsTreeDiagrams);

            TabPage tabPageSettingsAdvanced = new TabPage("Advanced");
            tabPageSettingsAdvanced.Controls.Add(m_labelConfigCharset);
            tabPageSettingsAdvanced.Controls.Add(m_comboboxConfigCharset);
            tabPageSettingsAdvanced.Controls.Add(m_labelConfigHtmlExtn);
            tabPageSettingsAdvanced.Controls.Add(m_comboboxConfigHtmlExtn);
            tabPageSettingsAdvanced.Controls.Add(m_checkboxConfigW3C);
            tabPageSettingsAdvanced.Controls.Add(m_checkboxConfigUserRecFilename);
            tabPageSettingsAdvanced.Controls.Add(m_textboxConfigStylesheetName);
            tabPageSettingsAdvanced.Controls.Add(m_labelConfigStylesheetName);
            tabPageSettingsAdvanced.Controls.Add(m_labelConfigStylesheetNameExtn);
            tabPageSettingsAdvanced.Controls.Add(m_checkboxConfigPreserveStylesheet);
            tabPageSettingsAdvanced.Controls.Add(m_checkboxConfigUseBom);
            tabPageSettingsAdvanced.Controls.Add(m_checkboxConfigSupressBackreferences);
            tabPageSettingsAdvanced.Controls.Add(m_checkboxConfigIncludeHelppage);
            m_tabcontrolConfigPanel.TabPages.Add(tabPageSettingsAdvanced);

            m_tabcontrolConfigPanel.Location = new System.Drawing.Point(0, 0);
            m_tabcontrolConfigPanel.Name = "configTabs";
            m_tabcontrolConfigPanel.Size = new System.Drawing.Size(507, 272);
            m_tabcontrolConfigPanel.TabIndex = 12;

            AutoScaleMode = AutoScaleMode.None;
            AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            ClientSize = new System.Drawing.Size(506, 320);
            Controls.Add(m_panelWelcome);
            Controls.Add(m_panelChooseGedcom);
            Controls.Add(m_panelPruneRecords);
            Controls.Add(m_panelSelectKey);
            Controls.Add(m_tabcontrolConfigPanel);
            Controls.Add(m_panelChooseOutput);
            Controls.Add(m_panelAllDone);
            Controls.Add(m_buttonCancel);
            Controls.Add(m_buttonSettings);
            Controls.Add(m_buttonSettingsCancel);
            Controls.Add(m_buttonHelp);
            Controls.Add(m_buttonBack);
            Controls.Add(m_buttonNext);
            Controls.Add(m_picturebox);
            FormBorderStyle = FormBorderStyle.FixedDialog;
            Icon = ((System.Drawing.Icon)(resources.GetObject("$Icon")));
            MaximumSize = new System.Drawing.Size(512, 355);
            MinimumSize = new System.Drawing.Size(512, 355);
            Name = "MainForm";
            Text = "GEDmill";

            m_panelWelcome.ResumeLayout(false);
            m_panelChooseGedcom.ResumeLayout(false);
            m_panelChooseOutput.ResumeLayout(false);
            m_panelPruneRecords.ResumeLayout(false);
            m_panelSelectKey.ResumeLayout(false);
            m_tabcontrolConfigPanel.ResumeLayout(false);
            m_panelAllDone.ResumeLayout(false);
            ResumeLayout(false);
        }        
    }
}
