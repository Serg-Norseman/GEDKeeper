namespace GEDmill
{
    public partial class MainForm
    {
        private System.ComponentModel.Container components = null;
        private System.Windows.Forms.Button btnNext;
        private System.Windows.Forms.Button btnBack;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Button btnSettings;
        private System.Windows.Forms.Button btnSettingsCancel;
        private System.Windows.Forms.Button btnHelp;
        private System.Windows.Forms.Panel panelWelcome;
        private System.Windows.Forms.Panel panelChooseOutput;
        private System.Windows.Forms.Panel panelRecords;
        private System.Windows.Forms.Panel panelKeyIndividuals;
        private System.Windows.Forms.Panel panelAllDone;
        private System.Windows.Forms.TabControl tabcontrolConfigPanel;
        private System.Windows.Forms.Label lblConfigFrontImageEdit;
        private System.Windows.Forms.TextBox txtConfigFrontImageEdit;
        private System.Windows.Forms.Button btnConfigFrontImageBrowse;
        private System.Windows.Forms.TextBox txtConfigBackImageEdit;
        private System.Windows.Forms.Label lblConfigBackImageEdit;
        private System.Windows.Forms.Button btnConfigBackImageBrowse;
        private System.Windows.Forms.Label lblConfigIndiImageSize;
        private System.Windows.Forms.Label lblConfigIndiImageWidth;
        private System.Windows.Forms.TextBox txtConfigIndiImageWidth;
        private System.Windows.Forms.Label lblConfigIndiImageHeight;
        private System.Windows.Forms.TextBox txtConfigIndiImageHeight;
        private System.Windows.Forms.Label lblConfigSourceImageSize;
        private System.Windows.Forms.Label lblConfigSourceImageWidth;
        private System.Windows.Forms.TextBox txtConfigSourceImageWidth;
        private System.Windows.Forms.Label lblConfigSourceImageHeight;
        private System.Windows.Forms.TextBox txtConfigSourceImageHeight;
        private System.Windows.Forms.Label lblConfigThumbnailImageSize;
        private System.Windows.Forms.Label lblConfigThumbnailImageWidth;
        private System.Windows.Forms.TextBox txtConfigThumbnailImageWidth;
        private System.Windows.Forms.Label lblConfigThumbnailImageHeight;
        private System.Windows.Forms.TextBox txtConfigThumbnailImageHeight;
        private System.Windows.Forms.CheckBox chkConfigUserRecFilename;
        private System.Windows.Forms.Label lblConfigCustomFooter;
        private System.Windows.Forms.TextBox txtConfigCustomFooter;
        private System.Windows.Forms.Label lblConfigFooterIsHtml;
        private System.Windows.Forms.CheckBox chkConfigFooterIsHtml;
        private System.Windows.Forms.CheckBox chkConfigConserveTreeWidth;
        private System.Windows.Forms.CheckBox chkConfigKeepSiblingOrder;
        private System.Windows.Forms.GroupBox gbMiniTreeColours;
        private System.Windows.Forms.Button btnConfigMiniTreeColourIndiBackground;
        private System.Windows.Forms.Button btnConfigMiniTreeColourIndiHighlight;
        private System.Windows.Forms.Button btnConfigMiniTreeColourIndiBgConcealed;
        private System.Windows.Forms.Button btnConfigMiniTreeColourIndiShade;
        private System.Windows.Forms.Button btnConfigMiniTreeColourIndiText;
        private System.Windows.Forms.Button btnConfigMiniTreeColourIndiLink;
        private System.Windows.Forms.Button btnConfigMiniTreeColourBranch;
        private System.Windows.Forms.Button btnConfigMiniTreeColourIndiBorder;
        private System.Windows.Forms.Button btnConfigMiniTreeColourIndiFgConcealed;
        private System.Windows.Forms.CheckBox chkConfigAllowMultimedia;
        private System.Windows.Forms.Label lblConfigNoName;
        private System.Windows.Forms.TextBox txtConfigNoName;
        private System.Windows.Forms.GroupBox gbConfigWithheldName;
        private System.Windows.Forms.RadioButton radConfigWithheldNameLabel;
        private System.Windows.Forms.RadioButton radConfigWithheldNameName;
        private System.Windows.Forms.TextBox txtConfigWithheldName;
        private System.Windows.Forms.CheckBox chkConfigCapNames;
        private System.Windows.Forms.CheckBox chkConfigCapEvents;
        private System.Windows.Forms.CheckBox chkConfigHideEmails;
        private System.Windows.Forms.CheckBox chkConfigOccupationHeadline;
        private System.Windows.Forms.CheckBox chkConfigShowWithheldRecords;
        private System.Windows.Forms.Label lblConfigTabSpaces;
        private System.Windows.Forms.TextBox txtConfigTabSpaces;
        private System.Windows.Forms.Label lblConfigCommentaryIsHtml;
        private System.Windows.Forms.CheckBox chkConfigCommentaryIsHtml;
        private System.Windows.Forms.Label lblConfigCommentary;
        private System.Windows.Forms.TextBox txtConfigCommentary;
        private System.Windows.Forms.Label lblConfigUserLink;
        private System.Windows.Forms.TextBox txtConfigUserLink;
        private System.Windows.Forms.Label lblConfigEmail;
        private System.Windows.Forms.TextBox txtConfigEmail;
        private System.Windows.Forms.TextBox txtConfigIndexName;
        private System.Windows.Forms.Label lblConfigIndexName;
        private System.Windows.Forms.Label lblConfigIndexNameExtn;
        private System.Windows.Forms.CheckBox chkConfigStats;
        private System.Windows.Forms.CheckBox chkConfigCdrom;
        private System.Windows.Forms.CheckBox chkConfigIndiImages;
        private System.Windows.Forms.CheckBox chkConfigNonPictures;
        private System.Windows.Forms.CheckBox chkConfigKeepOriginals;
        private System.Windows.Forms.CheckBox chkConfigRenameOriginals;
        private System.Windows.Forms.CheckBox chkConfigMultiPageIndex;
        private System.Windows.Forms.CheckBox chkConfigUserRefInIndex;
        private System.Windows.Forms.Label lblConfigMultiPageIndexNumber;
        private System.Windows.Forms.TextBox txtConfigMultiPageIndexNumber;
        private System.Windows.Forms.CheckBox chkConfigIncludeTreeDiagrams;
        private System.Windows.Forms.CheckBox chkConfigTreeDiagramsFakeBg;
        private System.Windows.Forms.Label lblConfigTreeDiagramsFormat;
        private System.Windows.Forms.ComboBox cmbConfigTreeDiagramsFormat;
        private System.Windows.Forms.CheckBox chkConfigSupressBackreferences;
        private System.Windows.Forms.Label lblWelcomeVersion;
        private System.Windows.Forms.Label lblWelcomeSubtitle;
        private System.Windows.Forms.PictureBox pictureBox;
        private System.Windows.Forms.TextBox txtChooseOutput;
        private System.Windows.Forms.Label lblChooseOutputInstructions;
        private System.Windows.Forms.Label lblChooseOutput;
        private System.Windows.Forms.Label lblChooseOutputContinue;
        private System.Windows.Forms.Button btnChooseOutputBrowse;
        private System.Windows.Forms.Label lblRecordsContinue;
        private GKUI.Components.GKListView lvIndividuals;
        private GKUI.Components.GKListView lvSources;
        private System.Windows.Forms.Label lblPruneRecordsInstructions;
        private System.Windows.Forms.Label lblPruneRecordsButtons;
        private System.Windows.Forms.Label lblSelectKey;
        private System.Windows.Forms.TextBox txtSelectKey;
        private System.Windows.Forms.Label lblSelectKeyIndividuals;
        private System.Windows.Forms.ListBox lstKeyIndividuals;
        private System.Windows.Forms.Button btnSelectKeyAdd;
        private System.Windows.Forms.Button btnSelectKeyDelete;
        private System.Windows.Forms.Label lblSelectKeyInstructions;
        private System.Windows.Forms.Label lblAllDoneThankYou;
        private System.Windows.Forms.Label lblAllDoneDirectory;
        private System.Windows.Forms.Label lblAllDoneStartFile;
        private System.Windows.Forms.CheckBox chkAllDoneShowSite;
        private System.Windows.Forms.LinkLabel lblAllDone;
        private System.Windows.Forms.PictureBox pictureBoxWelcome;
        private System.Windows.Forms.ContextMenu menuIndividuals;
        private System.Windows.Forms.ContextMenu menuSources;
        private System.Windows.Forms.MenuItem miUnconnectedExclude;
        private System.Windows.Forms.MenuItem miIndiDescendantsExclude;
        private System.Windows.Forms.MenuItem miIndiDescendantsInclude;
        private System.Windows.Forms.MenuItem miIndiAncestorsInclude;
        private System.Windows.Forms.MenuItem miIndiAncestorsExclude;
        private System.Windows.Forms.MenuItem miIndividualDetails;
        private System.Windows.Forms.MenuItem miSourceRemovePics;
        private System.Windows.Forms.MenuItem miSourceDetails;
        private System.Windows.Forms.HelpProvider helpProvider;
        private System.Windows.Forms.TabControl tabcontrolRestrictRecords;
        private System.Windows.Forms.TabPage pageIndividuals;
        private System.Windows.Forms.TabPage pageSources;
        private System.Windows.Forms.TabPage pageSettingsWebpages;
        private System.Windows.Forms.TabPage pageSettingsImages;
        private System.Windows.Forms.TabPage pageSettingsGedcom;
        private System.Windows.Forms.TabPage pageSettingsTreeDiagrams;
        private System.Windows.Forms.TabPage pageSettingsAdvanced;
        private System.Windows.Forms.MenuItem N1;
        private System.Windows.Forms.MenuItem N2;
        private System.Windows.Forms.MenuItem N3;
        private System.Windows.Forms.MenuItem N4;
        private System.Windows.Forms.MenuItem N5;
        private System.Windows.Forms.MenuItem miIndividualsEveryoneInclude;
        private System.Windows.Forms.MenuItem miIndividualsEveryoneExclude;
        private System.Windows.Forms.MenuItem miIndividualsAliveExclude;
        private System.Windows.Forms.MenuItem miSourcesAllInclude;
        private System.Windows.Forms.MenuItem miSourcesAllExclude;

        private void InitializeComponent()
        {
            System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(MainForm));
            btnNext = new System.Windows.Forms.Button();
            btnBack = new System.Windows.Forms.Button();
            btnCancel = new System.Windows.Forms.Button();
            btnHelp = new System.Windows.Forms.Button();
            btnSettings = new System.Windows.Forms.Button();
            btnSettingsCancel = new System.Windows.Forms.Button();
            lblConfigFrontImageEdit = new System.Windows.Forms.Label();
            txtConfigFrontImageEdit = new System.Windows.Forms.TextBox();
            btnConfigFrontImageBrowse = new System.Windows.Forms.Button();
            txtConfigBackImageEdit = new System.Windows.Forms.TextBox();
            lblConfigBackImageEdit = new System.Windows.Forms.Label();
            btnConfigBackImageBrowse = new System.Windows.Forms.Button();
            lblConfigIndiImageSize = new System.Windows.Forms.Label();
            lblConfigIndiImageWidth = new System.Windows.Forms.Label();
            txtConfigIndiImageWidth = new System.Windows.Forms.TextBox();
            lblConfigIndiImageHeight = new System.Windows.Forms.Label();
            txtConfigIndiImageHeight = new System.Windows.Forms.TextBox();
            lblConfigSourceImageSize = new System.Windows.Forms.Label();
            lblConfigSourceImageWidth = new System.Windows.Forms.Label();
            txtConfigSourceImageWidth = new System.Windows.Forms.TextBox();
            lblConfigSourceImageHeight = new System.Windows.Forms.Label();
            txtConfigSourceImageHeight = new System.Windows.Forms.TextBox();
            lblConfigThumbnailImageSize = new System.Windows.Forms.Label();
            lblConfigThumbnailImageWidth = new System.Windows.Forms.Label();
            txtConfigThumbnailImageWidth = new System.Windows.Forms.TextBox();
            lblConfigThumbnailImageHeight = new System.Windows.Forms.Label();
            txtConfigThumbnailImageHeight = new System.Windows.Forms.TextBox();
            chkConfigUserRecFilename = new System.Windows.Forms.CheckBox();
            lblConfigCustomFooter = new System.Windows.Forms.Label();
            txtConfigCustomFooter = new System.Windows.Forms.TextBox();
            lblConfigFooterIsHtml = new System.Windows.Forms.Label();
            chkConfigFooterIsHtml = new System.Windows.Forms.CheckBox();
            chkConfigConserveTreeWidth = new System.Windows.Forms.CheckBox();
            chkConfigKeepSiblingOrder = new System.Windows.Forms.CheckBox();
            gbMiniTreeColours = new System.Windows.Forms.GroupBox();
            btnConfigMiniTreeColourIndiBackground = new System.Windows.Forms.Button();
            btnConfigMiniTreeColourIndiHighlight = new System.Windows.Forms.Button();
            btnConfigMiniTreeColourIndiBgConcealed = new System.Windows.Forms.Button();
            btnConfigMiniTreeColourIndiShade = new System.Windows.Forms.Button();
            btnConfigMiniTreeColourIndiText = new System.Windows.Forms.Button();
            btnConfigMiniTreeColourIndiLink = new System.Windows.Forms.Button();
            btnConfigMiniTreeColourBranch = new System.Windows.Forms.Button();
            btnConfigMiniTreeColourIndiBorder = new System.Windows.Forms.Button();
            btnConfigMiniTreeColourIndiFgConcealed = new System.Windows.Forms.Button();
            chkConfigAllowMultimedia = new System.Windows.Forms.CheckBox();
            chkConfigSupressBackreferences = new System.Windows.Forms.CheckBox();
            lblConfigNoName = new System.Windows.Forms.Label();
            txtConfigNoName = new System.Windows.Forms.TextBox();
            gbConfigWithheldName = new System.Windows.Forms.GroupBox();
            radConfigWithheldNameLabel = new System.Windows.Forms.RadioButton();
            radConfigWithheldNameName = new System.Windows.Forms.RadioButton();
            txtConfigWithheldName = new System.Windows.Forms.TextBox();
            chkConfigCapNames = new System.Windows.Forms.CheckBox();
            chkConfigCapEvents = new System.Windows.Forms.CheckBox();
            chkConfigHideEmails = new System.Windows.Forms.CheckBox();
            chkConfigOccupationHeadline = new System.Windows.Forms.CheckBox();
            chkConfigShowWithheldRecords = new System.Windows.Forms.CheckBox(); ;
            lblConfigTabSpaces = new System.Windows.Forms.Label();
            txtConfigTabSpaces = new System.Windows.Forms.TextBox();
            lblConfigCommentary = new System.Windows.Forms.Label();
            lblConfigCommentaryIsHtml = new System.Windows.Forms.Label();
            chkConfigCommentaryIsHtml = new System.Windows.Forms.CheckBox();
            txtConfigCommentary = new System.Windows.Forms.TextBox();
            lblConfigEmail = new System.Windows.Forms.Label();
            txtConfigEmail = new System.Windows.Forms.TextBox();
            lblConfigUserLink = new System.Windows.Forms.Label();
            txtConfigUserLink = new System.Windows.Forms.TextBox();
            txtConfigIndexName = new System.Windows.Forms.TextBox();
            lblConfigIndexName = new System.Windows.Forms.Label();
            lblConfigIndexNameExtn = new System.Windows.Forms.Label();
            chkConfigStats = new System.Windows.Forms.CheckBox();
            chkConfigIncludeTreeDiagrams = new System.Windows.Forms.CheckBox();
            chkConfigTreeDiagramsFakeBg = new System.Windows.Forms.CheckBox();
            lblConfigTreeDiagramsFormat = new System.Windows.Forms.Label();
            cmbConfigTreeDiagramsFormat = new System.Windows.Forms.ComboBox();
            chkConfigMultiPageIndex = new System.Windows.Forms.CheckBox();
            chkConfigUserRefInIndex = new System.Windows.Forms.CheckBox();
            lblConfigMultiPageIndexNumber = new System.Windows.Forms.Label();
            txtConfigMultiPageIndexNumber = new System.Windows.Forms.TextBox();
            chkConfigIndiImages = new System.Windows.Forms.CheckBox();
            chkConfigNonPictures = new System.Windows.Forms.CheckBox();
            chkConfigCdrom = new System.Windows.Forms.CheckBox();
            chkConfigRenameOriginals = new System.Windows.Forms.CheckBox();
            chkConfigKeepOriginals = new System.Windows.Forms.CheckBox();
            panelWelcome = new System.Windows.Forms.Panel();
            pictureBoxWelcome = new System.Windows.Forms.PictureBox();
            lblWelcomeVersion = new System.Windows.Forms.Label();
            lblWelcomeSubtitle = new System.Windows.Forms.Label();
            pictureBox = new System.Windows.Forms.PictureBox();
            panelChooseOutput = new System.Windows.Forms.Panel();
            txtChooseOutput = new System.Windows.Forms.TextBox();
            lblChooseOutputInstructions = new System.Windows.Forms.Label();
            lblChooseOutput = new System.Windows.Forms.Label();
            btnChooseOutputBrowse = new System.Windows.Forms.Button();
            lblChooseOutputContinue = new System.Windows.Forms.Label();
            panelRecords = new System.Windows.Forms.Panel();
            lblRecordsContinue = new System.Windows.Forms.Label();
            lvIndividuals = new GKUI.Components.GKListView();
            lvSources = new GKUI.Components.GKListView();
            lblPruneRecordsInstructions = new System.Windows.Forms.Label();
            lblPruneRecordsButtons = new System.Windows.Forms.Label();
            panelKeyIndividuals = new System.Windows.Forms.Panel();
            tabcontrolConfigPanel = new System.Windows.Forms.TabControl();
            tabcontrolRestrictRecords = new System.Windows.Forms.TabControl();
            lblSelectKey = new System.Windows.Forms.Label();
            txtSelectKey = new System.Windows.Forms.TextBox();
            lblSelectKeyInstructions = new System.Windows.Forms.Label();
            lblSelectKeyIndividuals = new System.Windows.Forms.Label();
            lstKeyIndividuals = new System.Windows.Forms.ListBox();
            btnSelectKeyAdd = new System.Windows.Forms.Button();
            btnSelectKeyDelete = new System.Windows.Forms.Button();
            panelAllDone = new System.Windows.Forms.Panel();
            chkAllDoneShowSite = new System.Windows.Forms.CheckBox();
            lblAllDone = new System.Windows.Forms.LinkLabel();
            lblAllDoneThankYou = new System.Windows.Forms.Label();
            lblAllDoneDirectory = new System.Windows.Forms.Label();
            lblAllDoneStartFile = new System.Windows.Forms.Label();
            menuIndividuals = new System.Windows.Forms.ContextMenu();
            menuSources = new System.Windows.Forms.ContextMenu();
            helpProvider = new System.Windows.Forms.HelpProvider();
            pageSettingsWebpages = new System.Windows.Forms.TabPage();
            pageSettingsImages = new System.Windows.Forms.TabPage();
            pageSettingsGedcom = new System.Windows.Forms.TabPage();
            pageSettingsTreeDiagrams = new System.Windows.Forms.TabPage();
            pageSettingsAdvanced = new System.Windows.Forms.TabPage();
            miIndiDescendantsExclude = new System.Windows.Forms.MenuItem();
            miIndiAncestorsExclude = new System.Windows.Forms.MenuItem();
            miIndiDescendantsInclude = new System.Windows.Forms.MenuItem();
            miIndiAncestorsInclude = new System.Windows.Forms.MenuItem();
            miUnconnectedExclude = new System.Windows.Forms.MenuItem();
            miIndividualDetails = new System.Windows.Forms.MenuItem();
            pageIndividuals = new System.Windows.Forms.TabPage();
            pageSources = new System.Windows.Forms.TabPage();
            miSourceDetails = new System.Windows.Forms.MenuItem();
            miSourceRemovePics = new System.Windows.Forms.MenuItem();
            N1 = new System.Windows.Forms.MenuItem();
            N2 = new System.Windows.Forms.MenuItem();
            N3 = new System.Windows.Forms.MenuItem();
            N4 = new System.Windows.Forms.MenuItem();
            N5 = new System.Windows.Forms.MenuItem();
            miIndividualsEveryoneInclude = new System.Windows.Forms.MenuItem();
            miIndividualsEveryoneExclude = new System.Windows.Forms.MenuItem();
            miIndividualsAliveExclude = new System.Windows.Forms.MenuItem();
            miSourcesAllInclude = new System.Windows.Forms.MenuItem();
            miSourcesAllExclude = new System.Windows.Forms.MenuItem();
            panelWelcome.SuspendLayout();
            panelChooseOutput.SuspendLayout();
            panelRecords.SuspendLayout();
            panelKeyIndividuals.SuspendLayout();
            tabcontrolConfigPanel.SuspendLayout();
            panelAllDone.SuspendLayout();
            SuspendLayout();
            // 
            // btnNext
            // 
            btnNext.Location = new System.Drawing.Point(424, 288);
            btnNext.Name = "btnNext";
            btnNext.TabIndex = 7;
            btnNext.Text = "&Next >";
            btnNext.Click += new System.EventHandler(btnNext_Click);
            // 
            // btnBack
            // 
            btnBack.Location = new System.Drawing.Point(344, 288);
            btnBack.Name = "btnBack";
            btnBack.TabIndex = 8;
            btnBack.Text = "< &Back";
            btnBack.Click += new System.EventHandler(btnBack_Click);
            // 
            // btnCancel
            // 
            btnCancel.Location = new System.Drawing.Point(8, 288);
            btnCancel.Name = "btnCancel";
            btnCancel.TabIndex = 10;
            btnCancel.Text = "&Quit";
            btnCancel.Click += new System.EventHandler(btnCancel_Click);
            // 
            // btnHelp
            // 
            btnHelp.Location = new System.Drawing.Point(186, 288);
            btnHelp.Name = "btnHelp";
            btnHelp.TabIndex = 11;
            btnHelp.Text = "&Help";
            btnHelp.Click += new System.EventHandler(btnHelp_Click);
            // 
            // btnSettings
            // 
            btnSettings.Location = new System.Drawing.Point(88, 288);
            btnSettings.Name = "btnSettings";
            btnSettings.Size = new System.Drawing.Size(92, 23);
            btnSettings.TabIndex = 12;
            btnSettings.Text = "btnSettings";
            btnSettings.Click += new System.EventHandler(btnSettings_Click);
            // 
            // btnSettingsCancel
            // 
            btnSettingsCancel.Location = new System.Drawing.Point(424, 288);
            btnSettingsCancel.Name = "btnSettingsCancel";
            btnSettingsCancel.TabIndex = 13;
            btnSettingsCancel.Text = "&Cancel";
            btnSettingsCancel.Click += new System.EventHandler(btnSettingsCancel_Click);
            btnSettingsCancel.Visible = false;
            // 
            // Welcome Panel 
            // 
            panelWelcome.Controls.Add(pictureBoxWelcome);
            panelWelcome.Controls.Add(lblWelcomeVersion);
            panelWelcome.Controls.Add(lblWelcomeSubtitle);
            panelWelcome.Location = new System.Drawing.Point(216, 0);
            panelWelcome.Name = "panelWelcome";
            panelWelcome.Size = new System.Drawing.Size(280, 272);
            panelWelcome.TabIndex = 6;
            // 
            // pictureBox
            // 
            pictureBox.Image = ((System.Drawing.Image)(resources.GetObject("panel1PictureBox.Image")));
            pictureBox.Location = new System.Drawing.Point(8, 8);
            pictureBox.Name = "pictureBox";
            pictureBox.Size = new System.Drawing.Size(200, 264);
            pictureBox.TabIndex = 8;
            pictureBox.TabStop = false;
            // 
            // pictureBoxWelcome
            // 
            pictureBoxWelcome.Image = ((System.Drawing.Image)(resources.GetObject("title.Image")));
            pictureBoxWelcome.Size = new System.Drawing.Size(177, 65);
            pictureBoxWelcome.Location = new System.Drawing.Point(56, 50);
            pictureBoxWelcome.Name = "pictureBoxWelcome";
            pictureBoxWelcome.TabIndex = 8;
            pictureBoxWelcome.TabStop = false;
            // 
            // lblWelcomeVersion
            // 
            lblWelcomeVersion.Location = new System.Drawing.Point(88, 116);
            lblWelcomeVersion.Name = "lblWelcomeVersion";
            lblWelcomeVersion.Size = new System.Drawing.Size(112, 16);
            lblWelcomeVersion.TabIndex = 4;
            lblWelcomeVersion.Text = "version";
            lblWelcomeVersion.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // lblWelcomeSubtitle
            // 
            lblWelcomeSubtitle.Font = new System.Drawing.Font("Arial", 14F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
            lblWelcomeSubtitle.Location = new System.Drawing.Point(0, 138);
            lblWelcomeSubtitle.Name = "lblWelcomeSubtitle";
            lblWelcomeSubtitle.Size = new System.Drawing.Size(280, 54);
            lblWelcomeSubtitle.TabIndex = 3;
            lblWelcomeSubtitle.Text = "Family History Website Creator";
            lblWelcomeSubtitle.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // panelRecords
            // 
            panelRecords.Controls.Add(lblRecordsContinue);
            panelRecords.Controls.Add(lblPruneRecordsInstructions);
            panelRecords.Controls.Add(lblPruneRecordsButtons);
            panelRecords.Controls.Add(tabcontrolRestrictRecords);
            panelRecords.Location = new System.Drawing.Point(0, 0);
            panelRecords.Name = "panelRecords";
            panelRecords.Size = new System.Drawing.Size(496, 272);
            panelRecords.TabIndex = 11;
            // 
            // lblRecordsContinue
            // 
            lblRecordsContinue.Location = new System.Drawing.Point(256, 288);
            lblRecordsContinue.Name = "lblRecordsContinue";
            lblRecordsContinue.Size = new System.Drawing.Size(256, 16);
            lblRecordsContinue.TabIndex = 5;
            lblRecordsContinue.Text = "When you have finished selecting, click Next.";
            //
            // miIndiDescendantsExclude
            //
            miIndiDescendantsExclude.Text = "E&xclude all descendants of this person";
            miIndiDescendantsExclude.Click += new System.EventHandler(miIndiDescendantsExclude_Click);
            //
            // miIndiAncestorsExclude
            //
            miIndiAncestorsExclude.Text = "Exclude all &ancestors of this person";
            miIndiAncestorsExclude.Click += new System.EventHandler(miIndiAncestorsExclude_Click);
            //
            // miIndiDescendantsInclude
            //
            miIndiDescendantsInclude.Text = "In&clude all descendants of this person";
            miIndiDescendantsInclude.Click += new System.EventHandler(miIndiDescendantsInclude_Click);
            //
            // miIndiAncestorsInclude
            //
            miIndiAncestorsInclude.Text = "Include all a&ncestors of this person";
            miIndiAncestorsInclude.Click += new System.EventHandler(miIndiAncestorsInclude_Click);
            //
            // miUnconnectedExclude
            //
            miUnconnectedExclude.Text = "E&xclude individuals unless navigable from this person";
            miUnconnectedExclude.Click += new System.EventHandler(miUnconnectedExclude_Click);
            //
            // miIndividualDetails
            //
            miIndividualDetails.Text = "&Details and pictures...";
            miIndividualDetails.Click += new System.EventHandler(miIndividualDetails_Click);
            //
            // miIndividualsEveryoneInclude
            //
            miIndividualsEveryoneInclude.Text = "&Include everyone";
            miIndividualsEveryoneInclude.Click += new System.EventHandler(miIndividualsEveryoneInclude_Click);
            //
            // miIndividualsEveryoneExclude
            //
            miIndividualsEveryoneExclude.Text = "&Exclude everyone";
            miIndividualsEveryoneExclude.Click += new System.EventHandler(miIndividualsEveryoneExclude_Click);
            //
            // miIndividualsAliveExclude
            //
            miIndividualsAliveExclude.Text = "Exclude everyone still a&live (and those born in last 100 years)";
            miIndividualsAliveExclude.Click += new System.EventHandler(miIndividualsAliveExclude_Click);
            //
            // menuIndividuals
            //
            menuIndividuals.MenuItems.Add(miIndividualDetails);
            menuIndividuals.MenuItems.Add(N3);
            menuIndividuals.MenuItems.Add(miIndiDescendantsExclude);
            menuIndividuals.MenuItems.Add(miIndiDescendantsInclude);
            menuIndividuals.MenuItems.Add(miIndiAncestorsExclude);
            menuIndividuals.MenuItems.Add(miIndiAncestorsInclude);
            menuIndividuals.MenuItems.Add(N4);
            menuIndividuals.MenuItems.Add(miIndividualsEveryoneInclude);
            menuIndividuals.MenuItems.Add(miIndividualsEveryoneExclude);
            menuIndividuals.MenuItems.Add(miIndividualsAliveExclude);
            menuIndividuals.MenuItems.Add(N5);
            menuIndividuals.MenuItems.Add(miUnconnectedExclude);
            menuIndividuals.Popup += new System.EventHandler(menuIndividuals_Popup);
            //
            // miSourceDetails
            //
            miSourceDetails.Text = "&Details and pictures...";
            miSourceDetails.Click += new System.EventHandler(miSourceDetails_Click);
            //
            // miSourceRemovePics
            //
            miSourceRemovePics.Text = "&Remove pictures from selected sources";
            miSourceRemovePics.Click += new System.EventHandler(miSourceRemovePics_Click);
            //
            // N1
            //
            N1.Text = "-";
            //
            // N2
            //
            N2.Text = "-";
            //
            // miSourcesAllInclude
            //
            miSourcesAllInclude.Text = "&Include all sources";
            miSourcesAllInclude.Click += new System.EventHandler(miSourcesAllInclude_Click);
            //
            // miSourcesAllExclude
            //
            miSourcesAllExclude.Text = "&Exclude all sources";
            miSourcesAllExclude.Click += new System.EventHandler(miSourcesAllExclude_Click);
            //
            // menuSources
            //
            menuSources.MenuItems.Add(miSourceDetails);
            menuSources.MenuItems.Add(N1);
            menuSources.MenuItems.Add(miSourcesAllInclude);
            menuSources.MenuItems.Add(miSourcesAllExclude);
            menuSources.MenuItems.Add(N2);
            menuSources.MenuItems.Add(miSourceRemovePics);
            menuSources.Popup += new System.EventHandler(menuSources_Popup);
            // 
            // tabcontrolRestrictRecords
            //          
            tabcontrolRestrictRecords.Location = new System.Drawing.Point(108, 65);
            tabcontrolRestrictRecords.Name = "tabcontrolRestrictRecords";
            tabcontrolRestrictRecords.Size = new System.Drawing.Size(388, 207);
            tabcontrolRestrictRecords.TabIndex = 4;
            tabcontrolRestrictRecords.TabPages.Add(pageIndividuals);
            tabcontrolRestrictRecords.TabPages.Add(pageSources);
            // 
            // lvIndividuals
            // 
            lvIndividuals.CheckBoxes = true;
            lvIndividuals.Location = new System.Drawing.Point(0, 0);
            lvIndividuals.Name = "lvIndividuals";
            lvIndividuals.Size = new System.Drawing.Size(381, 181);
            lvIndividuals.TabIndex = 4;
            lvIndividuals.View = System.Windows.Forms.View.Details;
            lvIndividuals.ItemCheck += new System.Windows.Forms.ItemCheckEventHandler(lvIndividuals_ItemCheck);
            lvIndividuals.ContextMenu = menuIndividuals;
            lvIndividuals.FullRowSelect = true;
            lvIndividuals.GridLines = true;
            lvIndividuals.AllowColumnReorder = true;
            // 
            // pageIndividuals
            // 
            pageIndividuals.Text = "Individuals";
            pageIndividuals.Controls.Add(lvIndividuals);
            // 
            // lvSources
            // 
            lvSources.CheckBoxes = true;
            lvSources.Location = new System.Drawing.Point(0, 0);
            lvSources.Name = "lvSources";
            lvSources.Size = new System.Drawing.Size(381, 181);
            lvSources.TabIndex = 4;
            lvSources.View = System.Windows.Forms.View.Details;
            lvSources.ItemCheck += new System.Windows.Forms.ItemCheckEventHandler(lvSources_ItemCheck);
            lvSources.ContextMenu = menuSources;
            lvSources.FullRowSelect = true;
            lvSources.GridLines = true;
            lvSources.AllowColumnReorder = true;
            // 
            // pageSources
            // 
            pageSources.Text = "Sources";
            pageSources.Controls.Add(lvSources);
            // 
            // lblPruneRecordsInstructions
            // 
            lblPruneRecordsInstructions.Location = new System.Drawing.Point(8, 16);
            lblPruneRecordsInstructions.Name = "lblPruneRecordsInstructions";
            lblPruneRecordsInstructions.Size = new System.Drawing.Size(488, 45);
            lblPruneRecordsInstructions.TabIndex = 3;
            lblPruneRecordsInstructions.Text = "Now, you can specify any individuals and sources you don\'t want to appear in the website. " +
                "Clear the box next to their name to prevent them from appearing - those left ticked will appear.";
            // 
            // lblPruneRecordsButtons
            // 
            lblPruneRecordsButtons.Location = new System.Drawing.Point(8, 70);
            lblPruneRecordsButtons.Name = "lblPruneRecordsButtons";
            lblPruneRecordsButtons.Size = new System.Drawing.Size(92, 95);
            lblPruneRecordsButtons.TabIndex = 9;
            lblPruneRecordsButtons.Text = "Right-click on the list for more options, including adding pictures...";
            // 
            // panelKeyIndividuals
            // 
            panelKeyIndividuals.Controls.Add(lblSelectKey);
            panelKeyIndividuals.Controls.Add(txtSelectKey);
            panelKeyIndividuals.Controls.Add(lblSelectKeyIndividuals);
            panelKeyIndividuals.Controls.Add(lstKeyIndividuals);
            panelKeyIndividuals.Controls.Add(btnSelectKeyAdd);
            panelKeyIndividuals.Controls.Add(btnSelectKeyDelete);
            panelKeyIndividuals.Controls.Add(lblSelectKeyInstructions);
            panelKeyIndividuals.Location = new System.Drawing.Point(216, 0);
            panelKeyIndividuals.Name = "panelKeyIndividuals";
            panelKeyIndividuals.Size = new System.Drawing.Size(280, 272);
            panelKeyIndividuals.TabIndex = 12;
            // 
            // lblSelectKey
            // 
            lblSelectKey.Location = new System.Drawing.Point(0, 120);
            lblSelectKey.Name = "lblSelectKey";
            lblSelectKey.Size = new System.Drawing.Size(184, 24);
            lblSelectKey.TabIndex = 2;
            lblSelectKey.Text = "&Website Title:";
            lblSelectKey.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            // 
            // txtSelectKey
            // 
            txtSelectKey.Location = new System.Drawing.Point(0, 144);
            txtSelectKey.Name = "txtSelectKey";
            txtSelectKey.Size = new System.Drawing.Size(274, 20);
            txtSelectKey.TabIndex = 1;
            txtSelectKey.Text = "";
            // 
            // lblSelectKeyIndividuals
            // 
            lblSelectKeyIndividuals.Location = new System.Drawing.Point(0, 182);
            lblSelectKeyIndividuals.Name = "lblSelectKeyIndividuals";
            lblSelectKeyIndividuals.Size = new System.Drawing.Size(184, 24);
            lblSelectKeyIndividuals.TabIndex = 3;
            lblSelectKeyIndividuals.Text = "&Key Individuals:";
            lblSelectKeyIndividuals.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            // 
            // lstKeyIndividuals
            // 
            lstKeyIndividuals.Location = new System.Drawing.Point(0, 206);
            lstKeyIndividuals.Name = "lstKeyIndividuals";
            lstKeyIndividuals.Size = new System.Drawing.Size(192, 68);
            lstKeyIndividuals.TabIndex = 4;
            lstKeyIndividuals.Text = "";
            lstKeyIndividuals.SelectedValueChanged += new System.EventHandler(lstKeyIndividuals_SelectedValueChanged);
            // 
            // btnSelectKeyAdd
            // 
            btnSelectKeyAdd.Location = new System.Drawing.Point(200, 206);
            btnSelectKeyAdd.Name = "btnSelectKeyAdd";
            btnSelectKeyAdd.TabIndex = 6;
            btnSelectKeyAdd.Text = "&Add...";
            btnSelectKeyAdd.Click += new System.EventHandler(btnSelectKeyAdd_Click);
            // 
            // btnSelectKeyDelete
            // 
            btnSelectKeyDelete.Location = new System.Drawing.Point(200, 236);
            btnSelectKeyDelete.Name = "btnSelectKeyDelete";
            btnSelectKeyDelete.TabIndex = 7;
            btnSelectKeyDelete.Text = "&Remove";
            btnSelectKeyDelete.Click += new System.EventHandler(btnSelectKeyDelete_Click);
            // 
            // lblSelectKeyInstructions
            // 
            lblSelectKeyInstructions.Location = new System.Drawing.Point(0, 16);
            lblSelectKeyInstructions.Name = "lblSelectKeyInstructions";
            lblSelectKeyInstructions.Size = new System.Drawing.Size(288, 96);
            lblSelectKeyInstructions.TabIndex = 0;
            lblSelectKeyInstructions.Text = "Next, you can choose a title for the front page of your website. " +
                "Leave it blank if you don\'t want a title.";
            lblSelectKeyInstructions.Text += "\r\n\r\nYou can also select which people feature as key individuals on the front page.";
            // 
            // lblConfigCommentary
            // 
            lblConfigCommentary.Location = new System.Drawing.Point(9, 0);
            lblConfigCommentary.Name = "lblConfigCommentary";
            lblConfigCommentary.RightToLeft = System.Windows.Forms.RightToLeft.No;
            lblConfigCommentary.Size = new System.Drawing.Size(200, 24);
            lblConfigCommentary.TabIndex = 1;
            lblConfigCommentary.Text = "Commentary for &title page:";
            lblConfigCommentary.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // txtConfigCommentary
            // 
            txtConfigCommentary.Location = new System.Drawing.Point(9, 26);
            txtConfigCommentary.Name = "txtConfigCommentary";
            txtConfigCommentary.Size = new System.Drawing.Size(240, 70);
            txtConfigCommentary.TabIndex = 2;
            txtConfigCommentary.Text = "";
            txtConfigCommentary.Multiline = true;
            // 
            // lblConfigCommentaryIsHtml
            // 
            lblConfigCommentaryIsHtml.Location = new System.Drawing.Point(9, 91);
            lblConfigCommentaryIsHtml.Name = "lblConfigCommentaryIsHtml";
            lblConfigCommentaryIsHtml.RightToLeft = System.Windows.Forms.RightToLeft.No;
            lblConfigCommentaryIsHtml.Size = new System.Drawing.Size(8, 24);
            lblConfigCommentaryIsHtml.TabIndex = 3;
            lblConfigCommentaryIsHtml.Text = "(";
            lblConfigCommentaryIsHtml.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // chkConfigCommentaryIsHtml
            // 
            chkConfigCommentaryIsHtml.Location = new System.Drawing.Point(19, 96);
            chkConfigCommentaryIsHtml.Name = "chkConfigCommentaryIsHtml";
            chkConfigCommentaryIsHtml.Size = new System.Drawing.Size(190, 24);
            chkConfigCommentaryIsHtml.TabIndex = 4;
            chkConfigCommentaryIsHtml.Text = "the a&bove text is HTML)";
            // 
            // lblConfigUserLink
            // 
            lblConfigUserLink.Location = new System.Drawing.Point(9, 121);
            lblConfigUserLink.Name = "lblConfigUserLink";
            lblConfigUserLink.RightToLeft = System.Windows.Forms.RightToLeft.No;
            lblConfigUserLink.Size = new System.Drawing.Size(260, 24);
            lblConfigUserLink.TabIndex = 5;
            lblConfigUserLink.Text = "&Link to your website: (with http:// prefix)";
            lblConfigUserLink.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // txtConfigUserLink
            // 
            txtConfigUserLink.Location = new System.Drawing.Point(9, 147);
            txtConfigUserLink.Name = "txtConfigUserLink";
            txtConfigUserLink.Size = new System.Drawing.Size(240, 20);
            txtConfigUserLink.TabIndex = 7;
            txtConfigUserLink.Text = "";
            txtConfigUserLink.Multiline = false;
            // 
            // lblConfigCustomFooter
            // 
            lblConfigCustomFooter.Location = new System.Drawing.Point(9, 172);
            lblConfigCustomFooter.Name = "lblConfigCustomFooter";
            lblConfigCustomFooter.RightToLeft = System.Windows.Forms.RightToLeft.No;
            lblConfigCustomFooter.Size = new System.Drawing.Size(224, 24);
            lblConfigCustomFooter.TabIndex = 8;
            lblConfigCustomFooter.Text = "Te&xt for page footer:";
            lblConfigCustomFooter.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // txtConfigCustomFooter
            //
            txtConfigCustomFooter.Location = new System.Drawing.Point(9, 198);
            txtConfigCustomFooter.Name = "txtConfigCustomFooter";
            txtConfigCustomFooter.Size = new System.Drawing.Size(200, 20);
            txtConfigCustomFooter.Text = "";
            txtConfigCustomFooter.TabIndex = 9;
            // 
            // lblConfigFooterIsHtml
            // 
            lblConfigFooterIsHtml.Location = new System.Drawing.Point(9, 213);
            lblConfigFooterIsHtml.Name = "lblConfigFooterIsHtml";
            lblConfigFooterIsHtml.RightToLeft = System.Windows.Forms.RightToLeft.No;
            lblConfigFooterIsHtml.Size = new System.Drawing.Size(8, 24);
            lblConfigFooterIsHtml.TabIndex = 10;
            lblConfigFooterIsHtml.Text = "(";
            lblConfigFooterIsHtml.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // chkConfigFooterIsHtml
            // 
            chkConfigFooterIsHtml.Location = new System.Drawing.Point(19, 218);
            chkConfigFooterIsHtml.Name = "chkConfigFooterIsHtml";
            chkConfigFooterIsHtml.Size = new System.Drawing.Size(190, 24);
            chkConfigFooterIsHtml.TabIndex = 11;
            chkConfigFooterIsHtml.Text = "the abo&ve text is HTML)";
            //
            // chkConfigStats
            // 
            chkConfigStats.Location = new System.Drawing.Point(266, 7);
            chkConfigStats.Name = "chkConfigStats";
            chkConfigStats.Size = new System.Drawing.Size(200, 20);
            chkConfigStats.Text = "Include website &statistics";
            chkConfigStats.TabIndex = 12;
            //
            // chkConfigCdrom
            // 
            chkConfigCdrom.Location = new System.Drawing.Point(266, 30);
            chkConfigCdrom.Name = "chkConfigCdrom";
            chkConfigCdrom.Size = new System.Drawing.Size(200, 20);
            chkConfigCdrom.Text = "Create CD-ROM &auto-run files";
            chkConfigCdrom.TabIndex = 13;
            //
            // chkConfigMultiPageIndex
            // 
            chkConfigMultiPageIndex.Location = new System.Drawing.Point(266, 53);
            chkConfigMultiPageIndex.Name = "chkConfigMultiPageIndex";
            chkConfigMultiPageIndex.Size = new System.Drawing.Size(220, 20);
            chkConfigMultiPageIndex.Text = "&Multi-page individuals index";
            chkConfigMultiPageIndex.TabIndex = 14;
            chkConfigMultiPageIndex.Click += new System.EventHandler(chkConfigMultiPageIndex_Click);
            //
            // chkConfigUserRefInIndex
            //
            chkConfigUserRefInIndex.Location = new System.Drawing.Point(266, 76);
            chkConfigUserRefInIndex.Name = "chkConfigUserRefInIndex";
            chkConfigUserRefInIndex.Size = new System.Drawing.Size(220, 20);
            chkConfigUserRefInIndex.Text = "&User Reference numbers in index";
            chkConfigUserRefInIndex.TabIndex = 15;
            // 
            // lblConfigMultiPageIndexNumber
            // 
            lblConfigMultiPageIndexNumber.Location = new System.Drawing.Point(266, 96);
            lblConfigMultiPageIndexNumber.Name = "lblConfigMultiPageIndexNumber";
            lblConfigMultiPageIndexNumber.RightToLeft = System.Windows.Forms.RightToLeft.No;
            lblConfigMultiPageIndexNumber.Size = new System.Drawing.Size(170, 24);
            lblConfigMultiPageIndexNumber.TabIndex = 16;
            lblConfigMultiPageIndexNumber.Text = "&Individuals per index page:";
            lblConfigMultiPageIndexNumber.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // txtConfigMultiPageIndexNumber
            // 
            txtConfigMultiPageIndexNumber.Location = new System.Drawing.Point(446, 100);
            txtConfigMultiPageIndexNumber.Name = "txtConfigMultiPageIndexNumber";
            txtConfigMultiPageIndexNumber.Size = new System.Drawing.Size(45, 20);
            txtConfigMultiPageIndexNumber.TabIndex = 17;
            txtConfigMultiPageIndexNumber.Text = "";
            // 
            // lblConfigIndexName
            // 
            lblConfigIndexName.Location = new System.Drawing.Point(266, 126);
            lblConfigIndexName.Name = "lblConfigIndexName";
            lblConfigIndexName.RightToLeft = System.Windows.Forms.RightToLeft.No;
            lblConfigIndexName.Size = new System.Drawing.Size(224, 20);
            lblConfigIndexName.TabIndex = 18;
            lblConfigIndexName.Text = "Name of &front page file:";
            lblConfigIndexName.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // txtConfigIndexName
            // 
            txtConfigIndexName.Location = new System.Drawing.Point(266, 148);
            txtConfigIndexName.Name = "txtConfigIndexName";
            txtConfigIndexName.Size = new System.Drawing.Size(175, 20);
            txtConfigIndexName.TabIndex = 19;
            txtConfigIndexName.Text = "";
            txtConfigIndexName.Multiline = false;
            // 
            // lblConfigIndexNameExtn
            // 
            lblConfigIndexNameExtn.Location = new System.Drawing.Point(440, 141);
            lblConfigIndexNameExtn.Name = "lblConfigIndexNameExtn";
            lblConfigIndexNameExtn.RightToLeft = System.Windows.Forms.RightToLeft.No;
            lblConfigIndexNameExtn.Size = new System.Drawing.Size(60, 24);
            lblConfigIndexNameExtn.TabIndex = 20;
            lblConfigIndexNameExtn.Text = "";
            lblConfigIndexNameExtn.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            // 
            // lblConfigEmail
            // 
            lblConfigEmail.Location = new System.Drawing.Point(266, 190);
            lblConfigEmail.Name = "lblConfigEmail";
            lblConfigEmail.RightToLeft = System.Windows.Forms.RightToLeft.No;
            lblConfigEmail.Size = new System.Drawing.Size(220, 24);
            lblConfigEmail.TabIndex = 22;
            lblConfigEmail.Text = "&Email address to put on front page:";
            lblConfigEmail.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // txtConfigEmail
            // 
            txtConfigEmail.Location = new System.Drawing.Point(266, 216);
            txtConfigEmail.Name = "txtConfigEmail";
            txtConfigEmail.Size = new System.Drawing.Size(220, 20);
            txtConfigEmail.TabIndex = 23;
            txtConfigEmail.Text = "";
            txtConfigEmail.Multiline = false;
            // 
            // lblConfigBackImageEdit
            // 
            this.lblConfigBackImageEdit.Location = new System.Drawing.Point(9, 0);
            this.lblConfigBackImageEdit.Name = "lblConfigBackImageEdit";
            this.lblConfigBackImageEdit.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.lblConfigBackImageEdit.Size = new System.Drawing.Size(156, 24);
            this.lblConfigBackImageEdit.TabIndex = 1;
            this.lblConfigBackImageEdit.Text = "&Background image:";
            this.lblConfigBackImageEdit.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // txtConfigBackImageEdit
            // 
            this.txtConfigBackImageEdit.Location = new System.Drawing.Point(9, 26);
            this.txtConfigBackImageEdit.Name = "txtConfigBackImageEdit";
            this.txtConfigBackImageEdit.Size = new System.Drawing.Size(191, 20);
            this.txtConfigBackImageEdit.TabIndex = 2;
            this.txtConfigBackImageEdit.Text = "";
            // 
            // btnConfigBackImageBrowse
            // 
            this.btnConfigBackImageBrowse.Location = new System.Drawing.Point(208, 25);
            this.btnConfigBackImageBrowse.Name = "btnConfigBackImageBrowse";
            this.btnConfigBackImageBrowse.TabIndex = 3;
            this.btnConfigBackImageBrowse.Text = "B&rowse...";
            this.btnConfigBackImageBrowse.Click += new System.EventHandler(this.btnConfigBackImageBrowse_Click);
            // 
            // lblConfigFrontImageEdit
            // 
            this.lblConfigFrontImageEdit.Location = new System.Drawing.Point(9, 46);
            this.lblConfigFrontImageEdit.Name = "lblConfigFrontImageEdit";
            this.lblConfigFrontImageEdit.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.lblConfigFrontImageEdit.Size = new System.Drawing.Size(156, 20);
            this.lblConfigFrontImageEdit.TabIndex = 4;
            this.lblConfigFrontImageEdit.Text = "&Picture on front page:";
            this.lblConfigFrontImageEdit.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // txtConfigFrontImageEdit
            // 
            this.txtConfigFrontImageEdit.Location = new System.Drawing.Point(9, 68);
            this.txtConfigFrontImageEdit.Name = "txtConfigFrontImageEdit";
            this.txtConfigFrontImageEdit.Size = new System.Drawing.Size(191, 20);
            this.txtConfigFrontImageEdit.TabIndex = 5;
            this.txtConfigFrontImageEdit.Text = "";
            // 
            // btnConfigFrontImageBrowse
            // 
            this.btnConfigFrontImageBrowse.Location = new System.Drawing.Point(208, 68);
            this.btnConfigFrontImageBrowse.Name = "btnConfigFrontImageBrowse";
            this.btnConfigFrontImageBrowse.TabIndex = 6;
            this.btnConfigFrontImageBrowse.Text = "Br&owse...";
            this.btnConfigFrontImageBrowse.Click += new System.EventHandler(this.btnConfigFrontImageBrowse_Click);
            // 
            // lblConfigIndiImageSize
            // 
            this.lblConfigIndiImageSize.Location = new System.Drawing.Point(9, 108);
            this.lblConfigIndiImageSize.Name = "lblConfigIndiImageSize";
            this.lblConfigIndiImageSize.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.lblConfigIndiImageSize.Size = new System.Drawing.Size(256, 24);
            this.lblConfigIndiImageSize.TabIndex = 7;
            this.lblConfigIndiImageSize.Text = "Maximum size of individual images";
            this.lblConfigIndiImageSize.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            // 
            // lblConfigIndiImageWidth
            // 
            this.lblConfigIndiImageWidth.Location = new System.Drawing.Point(9, 138);
            this.lblConfigIndiImageWidth.Name = "lblConfigIndiImageWidth";
            this.lblConfigIndiImageWidth.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.lblConfigIndiImageWidth.Size = new System.Drawing.Size(50, 24);
            this.lblConfigIndiImageWidth.TabIndex = 8;
            this.lblConfigIndiImageWidth.Text = "&Width:";
            this.lblConfigIndiImageWidth.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // txtConfigIndiImageWidth
            // 
            this.txtConfigIndiImageWidth.Location = new System.Drawing.Point(61, 138);
            this.txtConfigIndiImageWidth.Name = "txtConfigIndiImageWidth";
            this.txtConfigIndiImageWidth.Size = new System.Drawing.Size(34, 20);
            this.txtConfigIndiImageWidth.TabIndex = 9;
            this.txtConfigIndiImageWidth.Text = "";
            // 
            // lblConfigIndiImageHeight
            // 
            this.lblConfigIndiImageHeight.Location = new System.Drawing.Point(109, 138);
            this.lblConfigIndiImageHeight.Name = "lblConfigIndiImageHeight";
            this.lblConfigIndiImageHeight.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.lblConfigIndiImageHeight.Size = new System.Drawing.Size(50, 24);
            this.lblConfigIndiImageHeight.TabIndex = 10;
            this.lblConfigIndiImageHeight.Text = "&Height:";
            this.lblConfigIndiImageHeight.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // txtConfigIndiImageHeight
            // 
            this.txtConfigIndiImageHeight.Location = new System.Drawing.Point(162, 138);
            this.txtConfigIndiImageHeight.Name = "txtConfigIndiImageHeight";
            this.txtConfigIndiImageHeight.Size = new System.Drawing.Size(34, 20);
            this.txtConfigIndiImageHeight.TabIndex = 11;
            this.txtConfigIndiImageHeight.Text = "";
            // 
            // lblConfigSourceImageSize
            // 
            this.lblConfigSourceImageSize.Location = new System.Drawing.Point(9, 167);
            this.lblConfigSourceImageSize.Name = "lblConfigSourceImageSize";
            this.lblConfigSourceImageSize.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.lblConfigSourceImageSize.Size = new System.Drawing.Size(256, 24);
            this.lblConfigSourceImageSize.TabIndex = 12;
            this.lblConfigSourceImageSize.Text = "Maximum size of source images";
            this.lblConfigSourceImageSize.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            // 
            // lblConfigSourceImageWidth
            // 
            this.lblConfigSourceImageWidth.Location = new System.Drawing.Point(9, 193);
            this.lblConfigSourceImageWidth.Name = "lblConfigSourceImageWidth";
            this.lblConfigSourceImageWidth.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.lblConfigSourceImageWidth.Size = new System.Drawing.Size(50, 24);
            this.lblConfigSourceImageWidth.TabIndex = 13;
            this.lblConfigSourceImageWidth.Text = "W&idth:";
            this.lblConfigSourceImageWidth.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // txtConfigSourceImageWidth
            // 
            this.txtConfigSourceImageWidth.Location = new System.Drawing.Point(60, 197);
            this.txtConfigSourceImageWidth.Name = "txtConfigSourceImageWidth";
            this.txtConfigSourceImageWidth.Size = new System.Drawing.Size(34, 20);
            this.txtConfigSourceImageWidth.TabIndex = 14;
            this.txtConfigSourceImageWidth.Text = "";
            // 
            // lblConfigSourceImageHeight
            // 
            this.lblConfigSourceImageHeight.Location = new System.Drawing.Point(109, 193);
            this.lblConfigSourceImageHeight.Name = "lblConfigSourceImageHeight";
            this.lblConfigSourceImageHeight.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.lblConfigSourceImageHeight.Size = new System.Drawing.Size(50, 24);
            this.lblConfigSourceImageHeight.TabIndex = 15;
            this.lblConfigSourceImageHeight.Text = "H&eight:";
            this.lblConfigSourceImageHeight.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // txtConfigSourceImageHeight
            // 
            this.txtConfigSourceImageHeight.Location = new System.Drawing.Point(162, 197);
            this.txtConfigSourceImageHeight.Name = "txtConfigSourceImageHeight";
            this.txtConfigSourceImageHeight.Size = new System.Drawing.Size(34, 20);
            this.txtConfigSourceImageHeight.TabIndex = 16;
            this.txtConfigSourceImageHeight.Text = "";
            //
            // chkConfigAllowMultimedia
            // 
            this.chkConfigAllowMultimedia.Location = new System.Drawing.Point(300, 8);
            this.chkConfigAllowMultimedia.Name = "chkConfigAllowMultimedia";
            this.chkConfigAllowMultimedia.Size = new System.Drawing.Size(190, 24);
            this.chkConfigAllowMultimedia.TabIndex = 5;
            this.chkConfigAllowMultimedia.Text = "&Allow images etc.";
            this.chkConfigAllowMultimedia.Click += new System.EventHandler(this.chkConfigAllowMultimedia_Click);
            //
            // chkConfigRenameOriginals
            // 
            this.chkConfigRenameOriginals.Location = new System.Drawing.Point(300, 38);
            this.chkConfigRenameOriginals.Name = "chkConfigRenameOriginals";
            this.chkConfigRenameOriginals.Size = new System.Drawing.Size(200, 30);
            this.chkConfigRenameOriginals.Text = "Re&name files";
            this.chkConfigRenameOriginals.TabIndex = 17;
            //
            // chkConfigKeepOriginals
            // 
            this.chkConfigKeepOriginals.Location = new System.Drawing.Point(300, 64);
            this.chkConfigKeepOriginals.Name = "chkConfigKeepOriginals";
            this.chkConfigKeepOriginals.Size = new System.Drawing.Size(200, 40);
            this.chkConfigKeepOriginals.Text = "In&clude original (full-size) files";
            this.chkConfigKeepOriginals.TabIndex = 18;
            //
            // chkConfigNonPictures
            // 
            this.chkConfigNonPictures.Location = new System.Drawing.Point(266, 120);
            this.chkConfigNonPictures.Name = "chkConfigNonPictures";
            this.chkConfigNonPictures.Size = new System.Drawing.Size(200, 20);
            this.chkConfigNonPictures.Text = "&Allow files other than pictures";
            this.chkConfigNonPictures.TabIndex = 19;
            //
            // chkConfigIndiImages
            // 
            this.chkConfigIndiImages.Location = new System.Drawing.Point(266, 147);
            this.chkConfigIndiImages.Name = "chkConfigIndiImages";
            this.chkConfigIndiImages.Size = new System.Drawing.Size(200, 20);
            this.chkConfigIndiImages.Text = "&Multiple individual images";
            this.chkConfigIndiImages.TabIndex = 20;
            this.chkConfigIndiImages.Click += new System.EventHandler(this.chkConfigIndiImages_Click);
            // 
            // lblConfigThumbnailImageSize
            // 
            this.lblConfigThumbnailImageSize.Location = new System.Drawing.Point(266, 167);
            this.lblConfigThumbnailImageSize.Name = "lblConfigThumbnailImageSize";
            this.lblConfigThumbnailImageSize.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.lblConfigThumbnailImageSize.Size = new System.Drawing.Size(256, 24);
            this.lblConfigThumbnailImageSize.TabIndex = 21;
            this.lblConfigThumbnailImageSize.Text = "Maximum size of thumbnail images";
            this.lblConfigThumbnailImageSize.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            // 
            // lblConfigThumbnailImageWidth
            // 
            this.lblConfigThumbnailImageWidth.Location = new System.Drawing.Point(266, 193);
            this.lblConfigThumbnailImageWidth.Name = "lblConfigThumbnailImageWidth";
            this.lblConfigThumbnailImageWidth.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.lblConfigThumbnailImageWidth.Size = new System.Drawing.Size(50, 24);
            this.lblConfigThumbnailImageWidth.TabIndex = 22;
            this.lblConfigThumbnailImageWidth.Text = "Wid&th:";
            this.lblConfigThumbnailImageWidth.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // txtConfigThumbnailImageWidth
            // 
            this.txtConfigThumbnailImageWidth.Location = new System.Drawing.Point(317, 197);
            this.txtConfigThumbnailImageWidth.Name = "txtConfigThumbnailImageWidth";
            this.txtConfigThumbnailImageWidth.Size = new System.Drawing.Size(34, 20);
            this.txtConfigThumbnailImageWidth.TabIndex = 23;
            this.txtConfigThumbnailImageWidth.Text = "";
            // 
            // lblConfigThumbnailImageHeight
            // 
            this.lblConfigThumbnailImageHeight.Location = new System.Drawing.Point(366, 193);
            this.lblConfigThumbnailImageHeight.Name = "lblConfigThumbnailImageHeight";
            this.lblConfigThumbnailImageHeight.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.lblConfigThumbnailImageHeight.Size = new System.Drawing.Size(50, 24);
            this.lblConfigThumbnailImageHeight.TabIndex = 24;
            this.lblConfigThumbnailImageHeight.Text = "Hei&ght:";
            this.lblConfigThumbnailImageHeight.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // txtConfigThumbnailImageHeight
            // 
            this.txtConfigThumbnailImageHeight.Location = new System.Drawing.Point(419, 197);
            this.txtConfigThumbnailImageHeight.Name = "txtConfigThumbnailImageHeight";
            this.txtConfigThumbnailImageHeight.Size = new System.Drawing.Size(34, 20);
            this.txtConfigThumbnailImageHeight.TabIndex = 25;
            this.txtConfigThumbnailImageHeight.Text = "";
            // 
            // lblConfigTabSpaces
            // 
            lblConfigTabSpaces.Location = new System.Drawing.Point(6, 0);
            lblConfigTabSpaces.Name = "lblConfigTabSpaces";
            lblConfigTabSpaces.RightToLeft = System.Windows.Forms.RightToLeft.No;
            lblConfigTabSpaces.Size = new System.Drawing.Size(188, 24);
            lblConfigTabSpaces.TabIndex = 1;
            lblConfigTabSpaces.Text = "&Num spaces to replace tabs:";
            lblConfigTabSpaces.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // txtConfigTabSpaces
            // 
            txtConfigTabSpaces.Location = new System.Drawing.Point(203, 4);
            txtConfigTabSpaces.Name = "txtConfigTabSpaces";
            txtConfigTabSpaces.Size = new System.Drawing.Size(31, 20);
            txtConfigTabSpaces.TabIndex = 2;
            txtConfigTabSpaces.Text = "";
            // 
            // lblConfigNoName
            // 
            lblConfigNoName.Location = new System.Drawing.Point(6, 24);
            lblConfigNoName.Name = "lblConfigNoName";
            lblConfigNoName.RightToLeft = System.Windows.Forms.RightToLeft.No;
            lblConfigNoName.Size = new System.Drawing.Size(200, 24);
            lblConfigNoName.TabIndex = 3;
            lblConfigNoName.Text = "Show &missing names as:";
            lblConfigNoName.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // txtConfigNoName
            // 
            txtConfigNoName.Location = new System.Drawing.Point(6, 48);
            txtConfigNoName.Name = "txtConfigNoName";
            txtConfigNoName.Size = new System.Drawing.Size(228, 20);
            txtConfigNoName.TabIndex = 4;
            txtConfigNoName.Text = "";
            //
            // chkConfigShowWithheldRecords
            // 
            chkConfigShowWithheldRecords.Location = new System.Drawing.Point(6, 86);
            chkConfigShowWithheldRecords.Name = "chkConfigShowWithheldRecords";
            chkConfigShowWithheldRecords.Size = new System.Drawing.Size(200, 16);
            chkConfigShowWithheldRecords.TabIndex = 5;
            chkConfigShowWithheldRecords.Text = "Include &withheld records";
            chkConfigShowWithheldRecords.Click += new System.EventHandler(chkConfigShowWithheldRecords_Click);
            // 
            // gbConfigWithheldName
            // 
            gbConfigWithheldName.Location = new System.Drawing.Point(6, 113);
            gbConfigWithheldName.Name = "gbConfigWithheldName";
            gbConfigWithheldName.Size = new System.Drawing.Size(228, 104);
            gbConfigWithheldName.TabIndex = 6;
            gbConfigWithheldName.Text = "Label w&ithheld records with:";
            gbConfigWithheldName.FlatStyle = System.Windows.Forms.FlatStyle.System;
            // 
            // radConfigWithheldNameLabel
            // 
            radConfigWithheldNameLabel.Location = new System.Drawing.Point(10, 18);
            radConfigWithheldNameLabel.Name = "radConfigWithheldNameLabel";
            radConfigWithheldNameLabel.RightToLeft = System.Windows.Forms.RightToLeft.No;
            radConfigWithheldNameLabel.Size = new System.Drawing.Size(180, 20);
            radConfigWithheldNameLabel.TabIndex = 7;
            radConfigWithheldNameLabel.Text = "this &text:";
            radConfigWithheldNameLabel.Click += new System.EventHandler(radConfigWithheldNameLabel_Click);
            //
            // txtConfigWithheldName
            // 
            txtConfigWithheldName.Location = new System.Drawing.Point(28, 38);
            txtConfigWithheldName.Name = "txtConfigWithheldName";
            txtConfigWithheldName.Size = new System.Drawing.Size(188, 20);
            txtConfigWithheldName.TabIndex = 8;
            txtConfigWithheldName.Text = "";
            // 
            // radConfigWithheldNameName
            // 
            radConfigWithheldNameName.Location = new System.Drawing.Point(10, 72);
            radConfigWithheldNameName.Name = "radConfigWithheldNameName";
            radConfigWithheldNameName.RightToLeft = System.Windows.Forms.RightToLeft.No;
            radConfigWithheldNameName.Size = new System.Drawing.Size(180, 20);
            radConfigWithheldNameName.TabIndex = 9;
            radConfigWithheldNameName.Text = "the individual's n&ame";
            radConfigWithheldNameName.Click += new System.EventHandler(radConfigWithheldNameLabel_Click);
            //
            // chkConfigCapNames
            // 
            chkConfigCapNames.Location = new System.Drawing.Point(266, 7);
            chkConfigCapNames.Name = "chkConfigCapNames";
            chkConfigCapNames.Size = new System.Drawing.Size(200, 20);
            chkConfigCapNames.TabIndex = 10;
            chkConfigCapNames.Text = "&Put surnames in CAPITALS";
            //
            // chkConfigCapEvents
            // 
            chkConfigCapEvents.Location = new System.Drawing.Point(266, 34);
            chkConfigCapEvents.Name = "chkConfigCapEvents";
            chkConfigCapEvents.Size = new System.Drawing.Size(260, 20);
            chkConfigCapEvents.TabIndex = 11;
            chkConfigCapEvents.Text = "&Start events with a capital letter";
            //
            // chkConfigHideEmails
            // 
            chkConfigHideEmails.Location = new System.Drawing.Point(266, 60);
            chkConfigHideEmails.Name = "chkConfigHideEmails";
            chkConfigHideEmails.Size = new System.Drawing.Size(260, 20);
            chkConfigHideEmails.TabIndex = 12;
            chkConfigHideEmails.Text = "Don't show &email addresses";
            //
            // chkConfigOccupationHeadline
            // 
            chkConfigOccupationHeadline.Location = new System.Drawing.Point(266, 86);
            chkConfigOccupationHeadline.Name = "chkConfigOccupationHeadline";
            chkConfigOccupationHeadline.Size = new System.Drawing.Size(260, 20);
            chkConfigOccupationHeadline.TabIndex = 13;
            chkConfigOccupationHeadline.Text = "Show occupation in pa&ge heading";
            //
            // chkConfigIncludeTreeDiagrams
            // 
            chkConfigIncludeTreeDiagrams.Location = new System.Drawing.Point(8, 8);
            chkConfigIncludeTreeDiagrams.Name = "chkConfigTreeDiagrams";
            chkConfigIncludeTreeDiagrams.Size = new System.Drawing.Size(200, 20);
            chkConfigIncludeTreeDiagrams.TabIndex = 2;
            chkConfigIncludeTreeDiagrams.Text = "Include &tree diagrams";
            chkConfigIncludeTreeDiagrams.Click += new System.EventHandler(chkConfigIncludeTreeDiagrams_Click);
            // 
            // lblConfigTreeDiagramsFormat
            // 
            lblConfigTreeDiagramsFormat.Location = new System.Drawing.Point(22, 25);
            lblConfigTreeDiagramsFormat.Name = "lblConfigTreeDiagramsFormat";
            lblConfigTreeDiagramsFormat.RightToLeft = System.Windows.Forms.RightToLeft.No;
            lblConfigTreeDiagramsFormat.Size = new System.Drawing.Size(134, 24);
            lblConfigTreeDiagramsFormat.TabIndex = 3;
            lblConfigTreeDiagramsFormat.Text = "&File format:";
            lblConfigTreeDiagramsFormat.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            //
            // cmbConfigTreeDiagramsFormat
            // 
            cmbConfigTreeDiagramsFormat.Location = new System.Drawing.Point(158, 30);
            cmbConfigTreeDiagramsFormat.Name = "cmbConfigTreeDiagramsFormat";
            cmbConfigTreeDiagramsFormat.Size = new System.Drawing.Size(85, 20);
            cmbConfigTreeDiagramsFormat.TabIndex = 4;
            cmbConfigTreeDiagramsFormat.DropDownWidth = 40;
            cmbConfigTreeDiagramsFormat.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            //
            // chkConfigTreeDiagramsFakeBg
            // 
            chkConfigTreeDiagramsFakeBg.Location = new System.Drawing.Point(8, 66);
            chkConfigTreeDiagramsFakeBg.Name = "chkConfigTreeDiagramsFakeBg";
            chkConfigTreeDiagramsFakeBg.Size = new System.Drawing.Size(200, 20);
            chkConfigTreeDiagramsFakeBg.TabIndex = 5;
            chkConfigTreeDiagramsFakeBg.Text = "&Simulate transparency";
            //
            // chkConfigConserveTreeWidth
            // 
            chkConfigConserveTreeWidth.Location = new System.Drawing.Point(8, 90);
            chkConfigConserveTreeWidth.Name = "chkConfigConserveTreeWidth";
            chkConfigConserveTreeWidth.Size = new System.Drawing.Size(190, 24);
            chkConfigConserveTreeWidth.TabIndex = 6;
            chkConfigConserveTreeWidth.Text = "Conserve tree &width";
            //
            // chkConfigKeepSiblingOrder
            // 
            chkConfigKeepSiblingOrder.Location = new System.Drawing.Point(8, 114);
            chkConfigKeepSiblingOrder.Name = "chkConfigKeepSiblingOrder";
            chkConfigKeepSiblingOrder.Size = new System.Drawing.Size(230, 24);
            chkConfigKeepSiblingOrder.TabIndex = 7;
            chkConfigKeepSiblingOrder.Text = "Keep s&ibling order from GEDCOM";
            //
            // gbMiniTreeColours
            // 
            gbMiniTreeColours.Location = new System.Drawing.Point(260, 11);
            gbMiniTreeColours.Name = "gbMiniTreeColours";
            gbMiniTreeColours.Size = new System.Drawing.Size(230, 224);
            gbMiniTreeColours.TabIndex = 8;
            gbMiniTreeColours.Text = "Colours";
            gbMiniTreeColours.FlatStyle = System.Windows.Forms.FlatStyle.System;
            //
            // btnConfigMiniTreeColourIndiHighlight
            // 
            btnConfigMiniTreeColourIndiHighlight.Location = new System.Drawing.Point(12, 24);
            btnConfigMiniTreeColourIndiHighlight.Name = "btnConfigMiniTreeColourIndiHighlight";
            btnConfigMiniTreeColourIndiHighlight.Size = new System.Drawing.Size(98, 24);
            btnConfigMiniTreeColourIndiHighlight.TabIndex = 9;
            btnConfigMiniTreeColourIndiHighlight.Text = "Selected &box";
            btnConfigMiniTreeColourIndiHighlight.Click += new System.EventHandler(btnConfigMiniTreeColourIndiHighlight_Click);
            //
            // btnConfigMiniTreeColourIndiText
            // 
            btnConfigMiniTreeColourIndiText.Location = new System.Drawing.Point(122, 24);
            btnConfigMiniTreeColourIndiText.Name = "btnConfigMiniTreeColourIndiText";
            btnConfigMiniTreeColourIndiText.Size = new System.Drawing.Size(98, 24);
            btnConfigMiniTreeColourIndiText.TabIndex = 10;
            btnConfigMiniTreeColourIndiText.Text = "Selected te&xt";
            btnConfigMiniTreeColourIndiText.Click += new System.EventHandler(btnConfigMiniTreeColourIndiText_Click);
            //
            // btnConfigMiniTreeColourIndiBackground
            // 
            btnConfigMiniTreeColourIndiBackground.Location = new System.Drawing.Point(12, 60);
            btnConfigMiniTreeColourIndiBackground.Name = "btnConfigMiniTreeColourIndiBackground";
            btnConfigMiniTreeColourIndiBackground.Size = new System.Drawing.Size(98, 24);
            btnConfigMiniTreeColourIndiBackground.TabIndex = 11;
            btnConfigMiniTreeColourIndiBackground.Text = "&General box";
            btnConfigMiniTreeColourIndiBackground.BackColor = System.Drawing.Color.FromArgb(255, 0, 0);
            btnConfigMiniTreeColourIndiBackground.Click += new System.EventHandler(btnConfigMiniTreeColourIndiBackground_Click);
            //
            // btnConfigMiniTreeColourIndiLink
            // 
            btnConfigMiniTreeColourIndiLink.Location = new System.Drawing.Point(122, 60);
            btnConfigMiniTreeColourIndiLink.Name = "btnConfigMiniTreeColourIndiLink";
            btnConfigMiniTreeColourIndiLink.Size = new System.Drawing.Size(98, 24);
            btnConfigMiniTreeColourIndiLink.TabIndex = 12;
            btnConfigMiniTreeColourIndiLink.Text = "&Link text";
            btnConfigMiniTreeColourIndiLink.Click += new System.EventHandler(btnConfigMiniTreeColourIndiLink_Click);
            //
            // btnConfigMiniTreeColourIndiBgConcealed
            // 
            btnConfigMiniTreeColourIndiBgConcealed.Location = new System.Drawing.Point(12, 96);
            btnConfigMiniTreeColourIndiBgConcealed.Name = "btnConfigMiniTreeColourIndiBgConcealed";
            btnConfigMiniTreeColourIndiBgConcealed.Size = new System.Drawing.Size(98, 24);
            btnConfigMiniTreeColourIndiBgConcealed.TabIndex = 13;
            btnConfigMiniTreeColourIndiBgConcealed.Text = "&Private box";
            btnConfigMiniTreeColourIndiBgConcealed.Click += new System.EventHandler(btnConfigMiniTreeColourIndiBgConcealed_Click);
            //
            // btnConfigMiniTreeColourIndiFgConcealed
            // 
            btnConfigMiniTreeColourIndiFgConcealed.Location = new System.Drawing.Point(122, 96);
            btnConfigMiniTreeColourIndiFgConcealed.Name = "btnConfigMiniTreeColourIndiFgConcealed";
            btnConfigMiniTreeColourIndiFgConcealed.Size = new System.Drawing.Size(98, 24);
            btnConfigMiniTreeColourIndiFgConcealed.TabIndex = 14;
            btnConfigMiniTreeColourIndiFgConcealed.Text = "P&rivate text";
            btnConfigMiniTreeColourIndiFgConcealed.Click += new System.EventHandler(btnConfigMiniTreeColourIndiFgConcealed_Click);
            //
            // btnConfigMiniTreeColourIndiShade
            // 
            btnConfigMiniTreeColourIndiShade.Location = new System.Drawing.Point(12, 132);
            btnConfigMiniTreeColourIndiShade.Name = "btnConfigMiniTreeColourIndiShade";
            btnConfigMiniTreeColourIndiShade.Size = new System.Drawing.Size(98, 24);
            btnConfigMiniTreeColourIndiShade.TabIndex = 15;
            btnConfigMiniTreeColourIndiShade.Text = "Spous&e box";
            btnConfigMiniTreeColourIndiShade.Click += new System.EventHandler(btnConfigMiniTreeColourIndiShade_Click);
            //
            // btnConfigMiniTreeColourBranch
            // 
            btnConfigMiniTreeColourBranch.Location = new System.Drawing.Point(12, 168);
            btnConfigMiniTreeColourBranch.Name = "btnConfigMiniTreeColourBranch";
            btnConfigMiniTreeColourBranch.Size = new System.Drawing.Size(98, 24);
            btnConfigMiniTreeColourBranch.TabIndex = 16;
            btnConfigMiniTreeColourBranch.Text = "Br&anches";
            btnConfigMiniTreeColourBranch.Click += new System.EventHandler(btnConfigMiniTreeColourBranch_Click);
            //
            // btnConfigMiniTreeColourIndiBorder
            // 
            btnConfigMiniTreeColourIndiBorder.Location = new System.Drawing.Point(122, 168);
            btnConfigMiniTreeColourIndiBorder.Name = "btnConfigMiniTreeColourIndiBorder";
            btnConfigMiniTreeColourIndiBorder.Size = new System.Drawing.Size(98, 24);
            btnConfigMiniTreeColourIndiBorder.TabIndex = 17;
            btnConfigMiniTreeColourIndiBorder.Text = "Box bor&ders";
            btnConfigMiniTreeColourIndiBorder.Click += new System.EventHandler(btnConfigMiniTreeColourIndiBorder_Click);
            //
            // chkConfigUserRecFilename
            // 
            chkConfigUserRecFilename.Location = new System.Drawing.Point(11, 112);
            chkConfigUserRecFilename.Name = "chkConfigUserRecFilename";
            chkConfigUserRecFilename.Size = new System.Drawing.Size(240, 24);
            chkConfigUserRecFilename.Text = "&Use custom record number for filenames";
            chkConfigUserRecFilename.TabIndex = 7;
            //
            // chkConfigSupressBackreferences
            // 
            chkConfigSupressBackreferences.Location = new System.Drawing.Point(11, 136);
            chkConfigSupressBackreferences.Name = "chkConfigSupressBackreferences";
            chkConfigSupressBackreferences.Size = new System.Drawing.Size(250, 20);
            chkConfigSupressBackreferences.Text = "List c&iting records on source pages";
            chkConfigSupressBackreferences.TabIndex = 8;
            // 
            // panelChooseOutput
            // 
            panelChooseOutput.Controls.Add(txtChooseOutput);
            panelChooseOutput.Controls.Add(lblChooseOutputInstructions);
            panelChooseOutput.Controls.Add(lblChooseOutput);
            panelChooseOutput.Controls.Add(btnChooseOutputBrowse);
            panelChooseOutput.Controls.Add(lblChooseOutputContinue);
            panelChooseOutput.Location = new System.Drawing.Point(216, 0);
            panelChooseOutput.Name = "panelChooseOutput";
            panelChooseOutput.Size = new System.Drawing.Size(280, 272);
            panelChooseOutput.TabIndex = 11;
            // 
            // txtChooseOutput
            // 
            txtChooseOutput.Location = new System.Drawing.Point(0, 120);
            txtChooseOutput.Name = "txtChooseOutput";
            txtChooseOutput.Size = new System.Drawing.Size(192, 20);
            txtChooseOutput.TabIndex = 4;
            txtChooseOutput.Text = "";
            txtChooseOutput.TextChanged += new System.EventHandler(txtChooseOutput_TextChanged);
            // 
            // lblChooseOutput
            // 
            lblChooseOutput.Location = new System.Drawing.Point(0, 96);
            lblChooseOutput.Name = "lblChooseOutput";
            lblChooseOutput.RightToLeft = System.Windows.Forms.RightToLeft.No;
            lblChooseOutput.Size = new System.Drawing.Size(152, 24);
            lblChooseOutput.TabIndex = 5;
            lblChooseOutput.Text = "&Folder:";
            lblChooseOutput.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            // 
            // btnChooseOutputBrowse
            // 
            btnChooseOutputBrowse.Location = new System.Drawing.Point(200, 120);
            btnChooseOutputBrowse.Name = "btnChooseOutputBrowse";
            btnChooseOutputBrowse.TabIndex = 6;
            btnChooseOutputBrowse.Text = "B&rowse...";
            btnChooseOutputBrowse.Click += new System.EventHandler(btnChooseOutputBrowse_Click);
            // 
            // lblChooseOutputInstructions
            // 
            lblChooseOutputInstructions.Location = new System.Drawing.Point(0, 16);
            lblChooseOutputInstructions.Name = "lblChooseOutputInstructions";
            lblChooseOutputInstructions.Size = new System.Drawing.Size(280, 80);
            lblChooseOutputInstructions.TabIndex = 3;
            lblChooseOutputInstructions.Text = "Finally, select the folder where you wish to the website files to be created. If " +
                "the folder doesn\'t exist already it will be created for you.";
            // 
            // lblChooseOutputContinue
            // 
            lblChooseOutputContinue.Location = new System.Drawing.Point(256, 288);
            lblChooseOutputContinue.Name = "lblChooseOutputContinue";
            lblChooseOutputContinue.Size = new System.Drawing.Size(256, 16);
            lblChooseOutputContinue.TabIndex = 15;
            lblChooseOutputContinue.Text = "Click Next to create the web pages...";
            // 
            // panelAllDone
            // 
            panelAllDone.Controls.Add(chkAllDoneShowSite);
            panelAllDone.Controls.Add(lblAllDone);
            panelAllDone.Controls.Add(lblAllDoneThankYou);
            panelAllDone.Controls.Add(lblAllDoneDirectory);
            panelAllDone.Controls.Add(lblAllDoneStartFile);
            panelAllDone.Location = new System.Drawing.Point(216, 0);
            panelAllDone.Name = "panelAllDone";
            panelAllDone.Size = new System.Drawing.Size(280, 272);
            panelAllDone.TabIndex = 12;
            //
            // chkAllDoneShowSite
            // 
            chkAllDoneShowSite.Location = new System.Drawing.Point(0, 250);
            chkAllDoneShowSite.Name = "chkAllDoneShowSite";
            chkAllDoneShowSite.Size = new System.Drawing.Size(288, 24);
            chkAllDoneShowSite.TabIndex = 7;
            chkAllDoneShowSite.Text = "&Display web pages after program finishes.";
            // 
            // lblAllDone
            // 
            lblAllDone.Location = new System.Drawing.Point(0, 52);
            lblAllDone.Name = "lblAllDone";
            lblAllDone.Size = new System.Drawing.Size(288, 48);
            lblAllDone.TabIndex = 7;
            lblAllDone.TabStop = true;
            lblAllDone.Text = "<path>";
            lblAllDone.TextAlign = System.Drawing.ContentAlignment.TopLeft;
            lblAllDone.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(lblAllDone_Click);
            // 
            // lblAllDoneThankYou
            // 
            lblAllDoneThankYou.Location = new System.Drawing.Point(0, 230);
            lblAllDoneThankYou.Name = "lblAllDoneThankYou";
            lblAllDoneThankYou.Size = new System.Drawing.Size(288, 24);
            lblAllDoneThankYou.TabIndex = 3;
            lblAllDoneThankYou.Text = "Thank you for using GEDmill.";
            lblAllDoneThankYou.TextAlign = System.Drawing.ContentAlignment.TopLeft;
            // 
            // lblAllDoneDirectory
            // 
            lblAllDoneDirectory.Location = new System.Drawing.Point(0, 16);
            lblAllDoneDirectory.Name = "lblAllDoneDirectory";
            lblAllDoneDirectory.Size = new System.Drawing.Size(280, 48);
            lblAllDoneDirectory.TabIndex = 0;
            lblAllDoneDirectory.Text = "The website files have been generated and put in ";
            lblAllDoneDirectory.TextAlign = System.Drawing.ContentAlignment.TopLeft;
            // 
            // lblAllDoneStartFile
            // 
            lblAllDoneStartFile.Location = new System.Drawing.Point(0, 104);
            lblAllDoneStartFile.Name = "lblAllDoneStartFile";
            lblAllDoneStartFile.Size = new System.Drawing.Size(288, 48);
            lblAllDoneStartFile.TabIndex = 0;
            lblAllDoneStartFile.Text = "";
            lblAllDoneStartFile.TextAlign = System.Drawing.ContentAlignment.TopLeft;
            // 
            // pageSettingsWebpages
            // 
            pageSettingsWebpages.Text = "Webpages";
            pageSettingsWebpages.Controls.Add(lblConfigCommentary);
            pageSettingsWebpages.Controls.Add(txtConfigCommentary);
            pageSettingsWebpages.Controls.Add(lblConfigCommentaryIsHtml);
            pageSettingsWebpages.Controls.Add(chkConfigCommentaryIsHtml);
            pageSettingsWebpages.Controls.Add(lblConfigEmail);
            pageSettingsWebpages.Controls.Add(txtConfigEmail);
            pageSettingsWebpages.Controls.Add(lblConfigUserLink);
            pageSettingsWebpages.Controls.Add(txtConfigUserLink);
            pageSettingsWebpages.Controls.Add(txtConfigIndexName);
            pageSettingsWebpages.Controls.Add(lblConfigIndexName);
            pageSettingsWebpages.Controls.Add(lblConfigIndexNameExtn);
            pageSettingsWebpages.Controls.Add(chkConfigStats);
            pageSettingsWebpages.Controls.Add(chkConfigMultiPageIndex);
            pageSettingsWebpages.Controls.Add(chkConfigUserRefInIndex);
            pageSettingsWebpages.Controls.Add(lblConfigMultiPageIndexNumber);
            pageSettingsWebpages.Controls.Add(txtConfigMultiPageIndexNumber);
            pageSettingsWebpages.Controls.Add(chkConfigCdrom);
            pageSettingsWebpages.Controls.Add(lblConfigCustomFooter);
            pageSettingsWebpages.Controls.Add(txtConfigCustomFooter);
            pageSettingsWebpages.Controls.Add(lblConfigFooterIsHtml);
            pageSettingsWebpages.Controls.Add(chkConfigFooterIsHtml);
            // 
            // pageSettingsImages
            // 
            pageSettingsImages.Text = "Images";
            pageSettingsImages.Controls.Add(lblConfigFrontImageEdit);
            pageSettingsImages.Controls.Add(txtConfigFrontImageEdit);
            pageSettingsImages.Controls.Add(btnConfigFrontImageBrowse);
            pageSettingsImages.Controls.Add(txtConfigBackImageEdit);
            pageSettingsImages.Controls.Add(lblConfigBackImageEdit);
            pageSettingsImages.Controls.Add(btnConfigBackImageBrowse);
            pageSettingsImages.Controls.Add(lblConfigIndiImageSize);
            pageSettingsImages.Controls.Add(lblConfigIndiImageWidth);
            pageSettingsImages.Controls.Add(txtConfigIndiImageWidth);
            pageSettingsImages.Controls.Add(lblConfigIndiImageHeight);
            pageSettingsImages.Controls.Add(txtConfigIndiImageHeight);
            pageSettingsImages.Controls.Add(lblConfigSourceImageSize);
            pageSettingsImages.Controls.Add(lblConfigSourceImageWidth);
            pageSettingsImages.Controls.Add(txtConfigSourceImageWidth);
            pageSettingsImages.Controls.Add(lblConfigSourceImageHeight);
            pageSettingsImages.Controls.Add(txtConfigSourceImageHeight);
            pageSettingsImages.Controls.Add(lblConfigThumbnailImageSize);
            pageSettingsImages.Controls.Add(lblConfigThumbnailImageWidth);
            pageSettingsImages.Controls.Add(txtConfigThumbnailImageWidth);
            pageSettingsImages.Controls.Add(lblConfigThumbnailImageHeight);
            pageSettingsImages.Controls.Add(txtConfigThumbnailImageHeight);
            pageSettingsImages.Controls.Add(chkConfigIndiImages);
            pageSettingsImages.Controls.Add(chkConfigNonPictures);
            pageSettingsImages.Controls.Add(chkConfigRenameOriginals);
            pageSettingsImages.Controls.Add(chkConfigKeepOriginals);
            pageSettingsImages.Controls.Add(chkConfigAllowMultimedia);
            // 
            // gbConfigWithheldName
            // 
            gbConfigWithheldName.Controls.Add(radConfigWithheldNameLabel);
            gbConfigWithheldName.Controls.Add(txtConfigWithheldName);
            gbConfigWithheldName.Controls.Add(radConfigWithheldNameName);
            // 
            // pageSettingsGedcom
            // 
            pageSettingsGedcom.Text = "GEDCOM";
            pageSettingsGedcom.Controls.Add(lblConfigNoName);
            pageSettingsGedcom.Controls.Add(txtConfigNoName);
            pageSettingsGedcom.Controls.Add(gbConfigWithheldName);
            pageSettingsGedcom.Controls.Add(chkConfigCapNames);
            pageSettingsGedcom.Controls.Add(chkConfigCapEvents);
            pageSettingsGedcom.Controls.Add(chkConfigHideEmails);
            pageSettingsGedcom.Controls.Add(chkConfigOccupationHeadline);
            pageSettingsGedcom.Controls.Add(chkConfigShowWithheldRecords);
            pageSettingsGedcom.Controls.Add(lblConfigTabSpaces);
            pageSettingsGedcom.Controls.Add(txtConfigTabSpaces);
            // 
            // gbMiniTreeColours
            // 
            gbMiniTreeColours.Controls.Add(btnConfigMiniTreeColourIndiBackground);
            gbMiniTreeColours.Controls.Add(btnConfigMiniTreeColourIndiHighlight);
            gbMiniTreeColours.Controls.Add(btnConfigMiniTreeColourIndiBgConcealed);
            gbMiniTreeColours.Controls.Add(btnConfigMiniTreeColourIndiShade);
            gbMiniTreeColours.Controls.Add(btnConfigMiniTreeColourIndiText);
            gbMiniTreeColours.Controls.Add(btnConfigMiniTreeColourIndiLink);
            gbMiniTreeColours.Controls.Add(btnConfigMiniTreeColourBranch);
            gbMiniTreeColours.Controls.Add(btnConfigMiniTreeColourIndiBorder);
            gbMiniTreeColours.Controls.Add(btnConfigMiniTreeColourIndiFgConcealed);
            // 
            // pageSettingsTreeDiagrams
            // 
            pageSettingsTreeDiagrams.Text = "Tree Diagrams";
            pageSettingsTreeDiagrams.Controls.Add(chkConfigIncludeTreeDiagrams);
            pageSettingsTreeDiagrams.Controls.Add(chkConfigTreeDiagramsFakeBg);
            pageSettingsTreeDiagrams.Controls.Add(lblConfigTreeDiagramsFormat);
            pageSettingsTreeDiagrams.Controls.Add(cmbConfigTreeDiagramsFormat);
            pageSettingsTreeDiagrams.Controls.Add(chkConfigConserveTreeWidth);
            pageSettingsTreeDiagrams.Controls.Add(chkConfigKeepSiblingOrder);
            pageSettingsTreeDiagrams.Controls.Add(gbMiniTreeColours);
            // 
            // pageSettingsAdvanced
            // 
            pageSettingsAdvanced.Text = "Advanced";
            pageSettingsAdvanced.Controls.Add(chkConfigUserRecFilename);
            pageSettingsAdvanced.Controls.Add(chkConfigSupressBackreferences);
            // 
            // tabcontrolConfigPanel
            // 
            tabcontrolConfigPanel.TabPages.Add(pageSettingsWebpages);
            tabcontrolConfigPanel.TabPages.Add(pageSettingsImages);
            tabcontrolConfigPanel.TabPages.Add(pageSettingsGedcom);
            tabcontrolConfigPanel.TabPages.Add(pageSettingsTreeDiagrams);
            tabcontrolConfigPanel.TabPages.Add(pageSettingsAdvanced);
            tabcontrolConfigPanel.Location = new System.Drawing.Point(0, 0);
            tabcontrolConfigPanel.Name = "tabcontrolConfigPanel";
            tabcontrolConfigPanel.Size = new System.Drawing.Size(507, 272);
            tabcontrolConfigPanel.TabIndex = 12;
            // 
            // MainForm
            // 
            AutoScaleMode = System.Windows.Forms.AutoScaleMode.None;
            AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            ClientSize = new System.Drawing.Size(506, 320);
            Controls.Add(panelWelcome);
            Controls.Add(panelRecords);
            Controls.Add(panelKeyIndividuals);
            Controls.Add(tabcontrolConfigPanel);
            Controls.Add(panelChooseOutput);
            Controls.Add(panelAllDone);
            Controls.Add(btnCancel);
            Controls.Add(btnSettings);
            Controls.Add(btnSettingsCancel);
            Controls.Add(btnHelp);
            Controls.Add(btnBack);
            Controls.Add(btnNext);
            Controls.Add(pictureBox);
            FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            Icon = ((System.Drawing.Icon)(resources.GetObject("$Icon")));
            MaximizeBox = false;
            MaximumSize = new System.Drawing.Size(512, 355);
            MinimizeBox = false;
            MinimumSize = new System.Drawing.Size(512, 355);
            Name = "MainForm";
            StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            Text = "GEDmill";
            this.Closed += new System.EventHandler(this.MainForm_Closed);
            this.Load += new System.EventHandler(this.MainForm_Load);

            panelWelcome.ResumeLayout(false);
            panelChooseOutput.ResumeLayout(false);
            panelRecords.ResumeLayout(false);
            panelKeyIndividuals.ResumeLayout(false);
            tabcontrolConfigPanel.ResumeLayout(false);
            panelAllDone.ResumeLayout(false);
            ResumeLayout(false);
        }        
    }
}
