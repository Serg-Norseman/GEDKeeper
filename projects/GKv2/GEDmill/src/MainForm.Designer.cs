
using System.Windows.Forms;
using GKUI.Components;

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
        private GKListView lvPruneIndividuals;
        private GKListView lvPruneSources;
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
            lvPruneIndividuals = new GKListView();
            lvPruneSources = new GKListView();
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
            m_labelWelcomeVersion.Text = "version";
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
            m_contextmenuPruneRecordsIndis.Popup += new System.EventHandler(pruneIndividualsContextMenu_popup);

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
            m_contextmenuPruneRecordsSources.Popup += new System.EventHandler(pruneSourcesContextMenu_popup);

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
                "Clear the box next to their name to prevent them from appearing - those left ticked will appear.";

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
            m_buttonConfigMiniTreeColourIndiBackground.BackColor = System.Drawing.Color.FromArgb(255, 0, 0);
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
            m_labelAllDoneStartFile.Text = "";
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
            tabPageSettingsAdvanced.Controls.Add(m_labelConfigHtmlExtn);
            tabPageSettingsAdvanced.Controls.Add(m_comboboxConfigHtmlExtn);
            tabPageSettingsAdvanced.Controls.Add(m_checkboxConfigW3C);
            tabPageSettingsAdvanced.Controls.Add(m_checkboxConfigUserRecFilename);
            tabPageSettingsAdvanced.Controls.Add(m_textboxConfigStylesheetName);
            tabPageSettingsAdvanced.Controls.Add(m_labelConfigStylesheetName);
            tabPageSettingsAdvanced.Controls.Add(m_labelConfigStylesheetNameExtn);
            tabPageSettingsAdvanced.Controls.Add(m_checkboxConfigPreserveStylesheet);
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
