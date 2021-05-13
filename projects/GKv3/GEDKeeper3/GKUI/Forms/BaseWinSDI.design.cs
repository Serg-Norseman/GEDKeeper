using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class BaseWinSDI
    {
        private TableLayout StatusBar;
        private Label panStatusText;
        private Eto.Forms.ImageView panStatusShieldImage;

        private ToolBar ToolBar1;
        private ButtonToolItem tbFileNew;
        private ButtonToolItem tbFileLoad;
        private ButtonToolItem tbFileSave;
        private ButtonToolItem tbRecordAdd;
        private ButtonToolItem tbRecordEdit;
        private ButtonToolItem tbRecordDelete;
        private ButtonToolItem tbFilter;
        private ButtonToolItem tbTreeAncestors;
        private ButtonToolItem tbTreeDescendants;
        private ButtonToolItem tbPedigree;
        private ButtonToolItem tbStats;
        private ButtonToolItem tbPrev;
        private ButtonToolItem tbNext;
        private ButtonToolItem tbSendMail;

        // Obsolete
        /*private ButtonToolItem tbDocPrint;
        private ButtonToolItem tbDocPreview;*/

        private MenuBar MainMenu1;
        private ButtonMenuItem miFile;
        private ButtonMenuItem miFileNew;
        private ButtonMenuItem miFileLoad;
        private ButtonMenuItem miMRUFiles;
        private ButtonMenuItem miFileSave;
        private ButtonMenuItem miFileSaveAs;
        private ButtonMenuItem miFileClose;
        private ButtonMenuItem miFileProperties;
        private ButtonMenuItem miExportToExcelFile;
        private ButtonMenuItem miExportToFamilyBook;
        private ButtonMenuItem miExportToTreesAlbum;
        private ButtonMenuItem miTreeTools;
        private ButtonMenuItem miExit;
        private ButtonMenuItem miEdit;
        private ButtonMenuItem miRecordAdd;
        private ButtonMenuItem miRecordEdit;
        private ButtonMenuItem miRecordDelete;
        private ButtonMenuItem miSearch;
        private ButtonMenuItem miFilter;
        private ButtonMenuItem miOptions;
        private ButtonMenuItem miPedigree;
        private ButtonMenuItem miTreeAncestors;
        private ButtonMenuItem miTreeDescendants;
        private ButtonMenuItem miPedigree_dAboville;
        private ButtonMenuItem miPedigree_Konovalov;
        private ButtonMenuItem miMap;
        private ButtonMenuItem miStats;
        private ButtonMenuItem miHelp;
        private ButtonMenuItem miContext;
        private ButtonMenuItem miLogSend;
        private ButtonMenuItem miLogView;
        private ButtonMenuItem miAbout;
        private ContextMenu MenuMRU;
        private ContextMenu MenuPedigree;
        private ButtonMenuItem miPedigree_dAboville2;
        private ButtonMenuItem miPedigree_Konovalov2;
        private ButtonMenuItem miOrganizer;
        private ButtonMenuItem miService;
        private ButtonMenuItem miScripts;
        private ButtonMenuItem miExport;
        private ButtonMenuItem miTreeBoth;
        private ButtonMenuItem miAncestorsCircle;
        private ButtonToolItem tbTreeBoth;
        private ButtonMenuItem miReports;
        private ButtonMenuItem miPlugins;
        private ButtonMenuItem miSlideshow;
        private ButtonToolItem tbLoadMRU;
        private ButtonMenuItem miPedigreeAscend;
        private ButtonMenuItem miDescendantsCircle;
        private ButtonMenuItem miRelationshipCalculator;
        private TabControl tabsRecords;
        private ButtonMenuItem miContRecordDuplicate;
        private ButtonMenuItem miContRecordDelete;
        private ButtonMenuItem miContRecordEdit;
        private ButtonMenuItem miContRecordMerge;
        private ContextMenu contextMenu;
        private ButtonMenuItem miContRecordAdd;
        private ButtonMenuItem miTreeCompare;
        private ButtonMenuItem miTreeMerge;
        private ButtonMenuItem miTreeSplit;
        private ButtonMenuItem miRecMerge;
        private ButtonMenuItem miFamilyGroups;
        private ButtonMenuItem miTreeCheck;
        private ButtonMenuItem miPatSearch;
        private ButtonMenuItem miPlacesManager;

        private void InitializeComponent()
        {
            SuspendLayout();

            panStatusText = new Label();

            panStatusShieldImage = new Eto.Forms.ImageView();
            panStatusShieldImage.Size = new Size(18, 18);
            panStatusShieldImage.MouseDoubleClick += StatusBar_MouseDoubleClick;

            var panStatusDummy = new Label();
            panStatusDummy.Width = 20;

            StatusBar = new TableLayout();
            StatusBar.Rows.Add(new TableRow() { Cells = { new TableCell(panStatusText, true), null, panStatusShieldImage, panStatusDummy } });

            //

            MenuMRU = new ContextMenu();

            miPedigree_dAboville2 = new ButtonMenuItem();
            miPedigree_dAboville2.Click += miPedigree_dAbovilleClick;

            miPedigree_Konovalov2 = new ButtonMenuItem();
            miPedigree_Konovalov2.Click += miPedigree_KonovalovClick;

            MenuPedigree = new ContextMenu();
            MenuPedigree.Items.AddRange(new MenuItem[] {
                                            miPedigree_dAboville2,
                                            miPedigree_Konovalov2});

            //

            tbFileNew = new ButtonToolItem();
            tbFileNew.Click += miFileNew_Click;

            tbFileLoad = new ButtonToolItem();
            tbFileLoad.Click += miFileLoad_Click;

            tbLoadMRU = new ButtonToolItem();
            tbLoadMRU.Text = "▼";
            tbLoadMRU.Click  += (sender, e) => {
                if (MenuMRU.Items.Count > 0) {
                    MenuMRU.Show(this);
                }
            };

            /*var cmdFileSave = new Command(ToolBar1_ButtonClick) {
                    MenuText = "Quit",
                    Image = UIHelper.LoadResourceImage("Resources.btn_save.bmp"),
                    Shortcut = Application.Instance.CommonModifier | Keys.Q
            };*/

            tbFileSave = new ButtonToolItem();
            tbFileSave.Click += miFileSave_Click;

            tbRecordAdd = new ButtonToolItem();
            tbRecordAdd.Click += miRecordAdd_Click;

            tbRecordEdit = new ButtonToolItem();
            tbRecordEdit.Click += miRecordEdit_Click;

            tbRecordDelete = new ButtonToolItem();
            tbRecordDelete.Click += miRecordDelete_Click;

            tbFilter = new ButtonToolItem();
            tbFilter.Click += miFilter_Click;

            tbTreeAncestors = new ButtonToolItem();
            tbTreeAncestors.Click += miTreeAncestors_Click;

            tbTreeDescendants = new ButtonToolItem();
            tbTreeDescendants.Click += miTreeDescendants_Click;

            tbTreeBoth = new ButtonToolItem();
            tbTreeBoth.Click += miTreeBoth_Click;

            tbPedigree = new ButtonToolItem();
            tbPedigree.Click  += (sender, e) => MenuPedigree.Show(this);

            tbStats = new ButtonToolItem();
            tbStats.Click += miStats_Click;

            tbPrev = new ButtonToolItem();
            tbPrev.Enabled = false;
            tbPrev.Click += tbPrev_Click;

            tbNext = new ButtonToolItem();
            tbNext.Enabled = false;
            tbNext.Click += tbNext_Click;

            // Obsolete
            /*tbDocPreview = new ButtonToolItem();
            tbDocPreview.Click += tbDocPreview_Click;

            tbDocPrint = new ButtonToolItem();
            tbDocPrint.Click += tbDocPrint_Click;
            */

            tbSendMail = new ButtonToolItem();
            tbSendMail.Click += tbSendMail_Click;

            //

            ToolBar1 = new ToolBar();
            ToolBar1.TextAlign = ToolBarTextAlign.Right;
            ToolBar1.Items.AddRange(new ToolItem[] {
                                        tbFileNew,
                                        tbFileLoad,
                                        tbLoadMRU,
                                        tbFileSave,
                                        new SeparatorToolItem(),
                                        tbRecordAdd,
                                        tbRecordEdit,
                                        tbRecordDelete,
                                        new SeparatorToolItem(),
                                        tbFilter,
                                        new SeparatorToolItem(),
                                        tbTreeAncestors,
                                        tbTreeDescendants,
                                        tbTreeBoth,
                                        new SeparatorToolItem(),
                                        tbPedigree,
                                        new SeparatorToolItem(),
                                        tbStats,
                                        new SeparatorToolItem(),
                                        tbPrev,
                                        tbNext,
                                        new SeparatorToolItem(),
                                        tbSendMail});

            //

            miFileNew = new ButtonMenuItem();
            miFileNew.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.N)));
            miFileNew.Text = "miFileNew";
            miFileNew.Click += miFileNew_Click;
            miFileNew.Image = UIHelper.LoadResourceImage("Resources.btn_create_new.gif");

            miFileLoad = new ButtonMenuItem();
            miFileLoad.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.O)));
            miFileLoad.Text = "miFileLoad";
            miFileLoad.Click += miFileLoad_Click;
            miFileLoad.Image = UIHelper.LoadResourceImage("Resources.btn_load.gif");

            miMRUFiles = new ButtonMenuItem();
            miMRUFiles.Enabled = false;
            miMRUFiles.Text = "miMRUFiles";

            miFileSave = new ButtonMenuItem();
            miFileSave.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.S)));
            miFileSave.Text = "miFileSave";
            miFileSave.Click += miFileSave_Click;
            miFileSave.Image = UIHelper.LoadResourceImage("Resources.btn_save.gif");

            miFileSaveAs = new ButtonMenuItem();
            miFileSaveAs.Text = "miFileSaveAs";
            miFileSaveAs.Click += miFileSaveAs_Click;

            miFileClose = new ButtonMenuItem();
            miFileClose.Text = "miFileClose";
            miFileClose.Click += miFileClose_Click;

            miFileProperties = new ButtonMenuItem();
            miFileProperties.Text = "miFileProperties";
            miFileProperties.Click += miFileProperties_Click;
            miFileProperties.Image = UIHelper.LoadResourceImage("Resources.btn_properties.gif");

            miExportToExcelFile = new ButtonMenuItem();
            miExportToExcelFile.Text = "miExportToExcelFile";
            miExportToExcelFile.Click += miExportToExcelFile_Click;
            miExportToExcelFile.Image = UIHelper.LoadResourceImage("Resources.btn_excel.gif");

            miExport = new ButtonMenuItem();
            miExport.Items.AddRange(new MenuItem[] {
                                        miExportToExcelFile});
            miExport.Text = "miExport";
            miExport.Image = UIHelper.LoadResourceImage("Resources.btn_export.gif");

            miExit = new ButtonMenuItem();
            miExit.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.X)));
            miExit.Text = "miExit";
            miExit.Click += miExit_Click;
            miExit.Image = UIHelper.LoadResourceImage("Resources.btn_exit.gif");

            miFile = new ButtonMenuItem();
            miFile.Text = "File";
            miFile.Items.AddRange(new MenuItem[] {
                                      miFileNew,
                                      miFileLoad,
                                      miMRUFiles,
                                      miFileSave,
                                      miFileSaveAs,
                                      miFileClose,
                                      new SeparatorMenuItem(),
                                      miFileProperties,
                                      new SeparatorMenuItem(),
                                      miExport,
                                      new SeparatorMenuItem(),
                                      miExit});

            miRecordAdd = new ButtonMenuItem();
            miRecordAdd.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.I)));
            miRecordAdd.Text = "miRecordAdd";
            miRecordAdd.Click += miRecordAdd_Click;
            miRecordAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");

            miRecordEdit = new ButtonMenuItem();
            miRecordEdit.Text = "miRecordEdit";
            miRecordEdit.Click += miRecordEdit_Click;
            miRecordEdit.Image = UIHelper.LoadResourceImage("Resources.btn_rec_edit.gif");

            miRecordDelete = new ButtonMenuItem();
            miRecordDelete.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.L)));
            miRecordDelete.Text = "miRecordDelete";
            miRecordDelete.Click += miRecordDelete_Click;
            miRecordDelete.Image = UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif");

            miSearch = new ButtonMenuItem();
            miSearch.Text = "miSearch";
            miSearch.Click += miSearch_Click;
            miSearch.Image = UIHelper.LoadResourceImage("Resources.btn_search.gif");

            miFilter = new ButtonMenuItem();
            miFilter.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.F)));
            miFilter.Text = "miFilter";
            miFilter.Click += miFilter_Click;
            miFilter.Image = UIHelper.LoadResourceImage("Resources.btn_filter.gif");

            miEdit = new ButtonMenuItem();
            miEdit.Text = "Edit";
            miEdit.Items.AddRange(new MenuItem[] {
                                      miRecordAdd,
                                      miRecordEdit,
                                      miRecordDelete,
                                      new SeparatorMenuItem(),
                                      miSearch,
                                      miFilter});

            miTreeAncestors = new ButtonMenuItem();
            miTreeAncestors.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.A)));
            miTreeAncestors.Text = "miTreeAncestors";
            miTreeAncestors.Click += miTreeAncestors_Click;
            miTreeAncestors.Image = UIHelper.LoadResourceImage("Resources.btn_tree_ancestry.gif");

            miTreeDescendants = new ButtonMenuItem();
            miTreeDescendants.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.D)));
            miTreeDescendants.Text = "miTreeDescendants";
            miTreeDescendants.Click += miTreeDescendants_Click;
            miTreeDescendants.Image = UIHelper.LoadResourceImage("Resources.btn_tree_descendants.gif");

            miTreeBoth = new ButtonMenuItem();
            miTreeBoth.Text = "miTreeBoth";
            miTreeBoth.Click += miTreeBoth_Click;
            miTreeBoth.Image = UIHelper.LoadResourceImage("Resources.btn_tree_both.gif");

            miAncestorsCircle = new ButtonMenuItem();
            miAncestorsCircle.Text = "miAncestorsCircle";
            miAncestorsCircle.Click += miAncestorsCircle_Click;

            miDescendantsCircle = new ButtonMenuItem();
            miDescendantsCircle.Text = "miDescendantsCircle";
            miDescendantsCircle.Click += miDescendantsCircle_Click;

            miPedigreeAscend = new ButtonMenuItem();
            miPedigreeAscend.Text = "miPedigreeAscend";
            miPedigreeAscend.Click += miPedigreeAscend_Click;

            miPedigree_dAboville = new ButtonMenuItem();
            miPedigree_dAboville.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.P)));
            miPedigree_dAboville.Text = "miPedigree_dAboville";
            miPedigree_dAboville.Click += miPedigree_dAbovilleClick;

            miPedigree_Konovalov = new ButtonMenuItem();
            miPedigree_Konovalov.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.K)));
            miPedigree_Konovalov.Text = "miPedigree_Konovalov";
            miPedigree_Konovalov.Click += miPedigree_KonovalovClick;

            miExportToFamilyBook = new ButtonMenuItem();
            miExportToFamilyBook.Text = "miExportToFamilyBook";
            miExportToFamilyBook.Click += miExportToFamilyBook_Click;

            miExportToTreesAlbum = new ButtonMenuItem();
            miExportToTreesAlbum.Text = "miExportToTreesAlbum";
            miExportToTreesAlbum.Click += miExportToTreesAlbum_Click;

            miMap = new ButtonMenuItem();
            miMap.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.M)));
            miMap.Text = "miMap";
            miMap.Click += miMap_Click;

            miStats = new ButtonMenuItem();
            miStats.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.T)));
            miStats.Text = "miStats";
            miStats.Click += miStats_Click;
            miStats.Image = UIHelper.LoadResourceImage("Resources.btn_table.gif");

            miRelationshipCalculator = new ButtonMenuItem();
            miRelationshipCalculator.Text = "miRelationshipCalculator";
            miRelationshipCalculator.Click += miRelationshipCalculator_Click;

            miPedigree = new ButtonMenuItem();
            miPedigree.Text = "Pedigree";
            miPedigree.Items.AddRange(new MenuItem[] {
                                          miTreeAncestors,
                                          miTreeDescendants,
                                          miTreeBoth,
                                          miAncestorsCircle,
                                          miDescendantsCircle,
                                          new SeparatorMenuItem(),
                                          miPedigreeAscend,
                                          miPedigree_dAboville,
                                          miPedigree_Konovalov,
                                          miExportToFamilyBook,
                                          miExportToTreesAlbum,
                                          new SeparatorMenuItem(),
                                          miMap,
                                          new SeparatorMenuItem(),
                                          miStats,
                                          new SeparatorMenuItem(),
                                          miRelationshipCalculator});

            miOrganizer = new ButtonMenuItem();
            miOrganizer.Text = "miOrganizer";
            miOrganizer.Click += miOrganizer_Click;
            miOrganizer.Image = UIHelper.LoadResourceImage("Resources.btn_organizer.gif");

            miSlideshow = new ButtonMenuItem();
            miSlideshow.Text = "miSlideshow";
            miSlideshow.Click += miSlideshow_Click;
            miSlideshow.Image = UIHelper.LoadResourceImage("Resources.btn_slideshow.png");

            miScripts = new ButtonMenuItem();
            miScripts.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.F11)));
            miScripts.Text = "miScripts";
            miScripts.Click += miScripts_Click;

            miTreeCompare = new ButtonMenuItem();
            miTreeCompare.Text = "miTreeCompare";
            miTreeCompare.Click += miTTTreeCompare_Click;

            miTreeMerge = new ButtonMenuItem();
            miTreeMerge.Text = "miTreeMerge";
            miTreeMerge.Click += miTTTreeMerge_Click;

            miTreeSplit = new ButtonMenuItem();
            miTreeSplit.Text = "miTreeSplit";
            miTreeSplit.Click += miTTTreeSplit_Click;

            miRecMerge = new ButtonMenuItem();
            miRecMerge.Text = "miRecMerge";
            miRecMerge.Click += miTTRecMerge_Click;

            miFamilyGroups = new ButtonMenuItem();
            miFamilyGroups.Text = "miFamilyGroups";
            miFamilyGroups.Click += miTTFamilyGroups_Click;

            miTreeCheck = new ButtonMenuItem();
            miTreeCheck.Text = "miTreeCheck";
            miTreeCheck.Click += miTTTreeCheck_Click;

            miPatSearch = new ButtonMenuItem();
            miPatSearch.Text = "miPatSearch";
            miPatSearch.Click += miTTPatSearch_Click;

            miPlacesManager = new ButtonMenuItem();
            miPlacesManager.Text = "miPlacesManager";
            miPlacesManager.Click += miTTPlacesManager_Click;

            miTreeTools = new ButtonMenuItem();
            miTreeTools.Text = "miTreeTools";
            miTreeTools.Items.AddRange(new MenuItem[] {
                                          miTreeCompare,
                                          miTreeMerge,
                                          miTreeSplit,
                                          miRecMerge,
                                          miFamilyGroups,
                                          miTreeCheck,
                                          miPatSearch,
                                          miPlacesManager});

            miOptions = new ButtonMenuItem();
            miOptions.Text = "miOptions";
            miOptions.Click += miOptions_Click;
            miOptions.Image = UIHelper.LoadResourceImage("Resources.btn_tools.gif");

            miService = new ButtonMenuItem();
            miService.Text = "Service";
            miService.Items.AddRange(new MenuItem[] {
                                         miOrganizer,
                                         miSlideshow,
                                         new SeparatorMenuItem(),
                                         miScripts,
                                         miTreeTools,
                                         new SeparatorMenuItem(),
                                         miOptions});

            miReports = new ButtonMenuItem();
            miReports.Text = "Reports";

            miPlugins = new ButtonMenuItem();
            miPlugins.Text = "Plugins";

            miContext = new ButtonMenuItem();
            miContext.Shortcut = Keys.F1;
            miContext.Text = "miContext";
            miContext.Click += miContext_Click;
            miContext.Image = UIHelper.LoadResourceImage("Resources.btn_help.gif");

            miLogSend = new ButtonMenuItem();
            miLogSend.Text = "miLogSend";
            miLogSend.Click += miLogSend_Click;

            miLogView = new ButtonMenuItem();
            miLogView.Text = "miLogView";
            miLogView.Click += miLogView_Click;

            miAbout = new ButtonMenuItem();
            miAbout.Text = "miAbout";
            miAbout.Click += miAbout_Click;
            miAbout.Image = UIHelper.LoadResourceImage("Resources.btn_scroll.gif");

            miHelp = new ButtonMenuItem();
            miHelp.Text = "Help";
            miHelp.Items.AddRange(new MenuItem[] {
                                      miContext,
                                      new SeparatorMenuItem(),
                                      miLogSend,
                                      miLogView,
                                      new SeparatorMenuItem(),
                                      miAbout});

            MainMenu1 = new MenuBar();
            MainMenu1.Items.AddRange(new MenuItem[] {
                                         miFile,
                                         miEdit,
                                         miPedigree,
                                         miService,
                                         miReports,
                                         miPlugins,
                                         miHelp});

            //

            miContRecordAdd = new ButtonMenuItem();
            miContRecordAdd.Text = "miContRecordAdd";
            miContRecordAdd.Click += miRecordAdd_Click;

            miContRecordEdit = new ButtonMenuItem();
            miContRecordEdit.Text = "miContRecordEdit";
            miContRecordEdit.Click += miRecordEdit_Click;

            miContRecordDelete = new ButtonMenuItem();
            miContRecordDelete.Text = "miContRecordDelete";
            miContRecordDelete.Click += miRecordDelete_Click;

            miContRecordDuplicate = new ButtonMenuItem();
            miContRecordDuplicate.Text = "miRecordDuplicate";
            miContRecordDuplicate.Click += miRecordDuplicate_Click;

            miContRecordMerge = new ButtonMenuItem();
            miContRecordMerge.Text = "miContRecordMerge";
            miContRecordMerge.Click += miRecordMerge_Click;

            contextMenu = new ContextMenu();
            contextMenu.Items.AddRange(new MenuItem[] {
                                           miContRecordAdd,
                                           miContRecordEdit,
                                           miContRecordDelete,
                                           miContRecordDuplicate,
                                           miContRecordMerge
            });
            contextMenu.Opening += contextMenu_Opening;

            //

            tabsRecords = new TabControl();
            tabsRecords.SelectedIndexChanged += tabsRecords_SelectedIndexChanged;
            tabsRecords.Size = new Size(980, 460);

            Content = new TableLayout() {
                Rows = {
                    new TableRow() {
                        ScaleHeight = true,
                        Cells = { tabsRecords }
                    },
                    StatusBar
                }
            };
            Menu = MainMenu1;
            ToolBar = ToolBar1;

            Title = "BaseWinSDI";
            GotFocus += Form_Activated;
            LostFocus += Form_Deactivate;
            Closing += Form_Closing;
            Closed += Form_Closed;
            Load += Form_Load;
            KeyDown += Form_KeyDown;

            AllowDrop = true;
            DragDrop += Form_DragDrop;
            DragEnter += Form_DragEnter;

            UIHelper.SetPredefProperties(this, 980, 460, true, true);
            ResumeLayout();
        }
    }
}
