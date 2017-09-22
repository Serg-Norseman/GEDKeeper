using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class BaseWinSDI
    {
        private TableLayout StatusBar;
        private Label StatusBarPanel1;
        private Eto.Forms.ImageView StatusBarShieldImage;

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
        private ButtonMenuItem miPlugins;
        private ButtonMenuItem miSlideshow;
        private ButtonToolItem tbLoadMRU;
        private ButtonMenuItem miPedigreeAscend;
        private ButtonMenuItem miDescendantsCircle;
        private ButtonMenuItem miRelationshipCalculator;
        private TabControl tabsRecords;
        private ButtonMenuItem miRecordDuplicate;
        private ButtonMenuItem miContRecordDelete;
        private ButtonMenuItem miContRecordEdit;
        private ContextMenu contextMenu;
        private ButtonMenuItem miContRecordAdd;

        private void InitializeComponent()
        {
            SuspendLayout();

            StatusBarPanel1 = new Label();

            StatusBarShieldImage = new Eto.Forms.ImageView();
            StatusBarShieldImage.Size = new Size(18, 18);
            StatusBarShieldImage.MouseDoubleClick += StatusBar_MouseDoubleClick;

            var StatusBarPanel3 = new Label();
            StatusBarPanel3.Width = 20;

            StatusBar = new TableLayout();
            StatusBar.Rows.Add(new TableRow() { Cells = { new TableCell(StatusBarPanel1, true), null, StatusBarShieldImage, StatusBarPanel3 } });

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
            tbFileNew.Click += ToolBar1_ButtonClick;
            tbFileNew.Image = Bitmap.FromResource("Resources.btn_create_new.gif");

            tbFileLoad = new ButtonToolItem();
            tbFileLoad.Click += ToolBar1_ButtonClick;
            tbFileLoad.Image = Bitmap.FromResource("Resources.btn_load.gif");

            tbLoadMRU = new ButtonToolItem();
            tbLoadMRU.Text = "▼";
            tbLoadMRU.Click  += (sender, e) => {
                if (MenuMRU.Items.Count > 0) {
                    MenuMRU.Show(this);
                }
            };

            /*var cmdFileSave = new Command(ToolBar1_ButtonClick) {
                    MenuText = "Quit",
                    Image = Bitmap.FromResource("Resources.btn_save.bmp"),
                    Shortcut = Application.Instance.CommonModifier | Keys.Q
            };*/

            tbFileSave = new ButtonToolItem();
            tbFileSave.Click += ToolBar1_ButtonClick;
            tbFileSave.Image = Bitmap.FromResource("Resources.btn_save.gif");

            tbRecordAdd = new ButtonToolItem();
            tbRecordAdd.Click += ToolBar1_ButtonClick;
            tbRecordAdd.Image = Bitmap.FromResource("Resources.btn_rec_new.gif");

            tbRecordEdit = new ButtonToolItem();
            tbRecordEdit.Click += ToolBar1_ButtonClick;
            tbRecordEdit.Image = Bitmap.FromResource("Resources.btn_rec_edit.gif");

            tbRecordDelete = new ButtonToolItem();
            tbRecordDelete.Click += ToolBar1_ButtonClick;
            tbRecordDelete.Image = Bitmap.FromResource("Resources.btn_rec_delete.gif");

            tbFilter = new ButtonToolItem();
            tbFilter.Click += ToolBar1_ButtonClick;
            tbFilter.Image = Bitmap.FromResource("Resources.btn_filter.gif");

            tbTreeAncestors = new ButtonToolItem();
            tbTreeAncestors.Click += ToolBar1_ButtonClick;
            tbTreeAncestors.Image = Bitmap.FromResource("Resources.btn_tree_ancestry.gif");

            tbTreeDescendants = new ButtonToolItem();
            tbTreeDescendants.Click += ToolBar1_ButtonClick;
            tbTreeDescendants.Image = Bitmap.FromResource("Resources.btn_tree_descendants.gif");

            tbTreeBoth = new ButtonToolItem();
            tbTreeBoth.Click += ToolBar1_ButtonClick;
            tbTreeBoth.Image = Bitmap.FromResource("Resources.btn_tree_both.gif");

            tbPedigree = new ButtonToolItem();
            tbPedigree.Click  += (sender, e) => MenuPedigree.Show(this);
            tbPedigree.Image = Bitmap.FromResource("Resources.btn_scroll.gif");

            tbStats = new ButtonToolItem();
            tbStats.Click += ToolBar1_ButtonClick;
            tbStats.Image = Bitmap.FromResource("Resources.btn_table.gif");

            tbPrev = new ButtonToolItem();
            tbPrev.Enabled = false;
            tbPrev.Click += tbPrev_Click;
            tbPrev.Image = Bitmap.FromResource("Resources.btn_left.gif");

            tbNext = new ButtonToolItem();
            tbNext.Enabled = false;
            tbNext.Click += tbNext_Click;
            tbNext.Image = Bitmap.FromResource("Resources.btn_right.gif");

            // Obsolete
            /*tbDocPreview = new ButtonToolItem();
            tbDocPreview.Click += tbDocPreview_Click;
            tbDocPreview.Image = Bitmap.FromResource("Resources.btn_preview.gif");

            tbDocPrint = new ButtonToolItem();
            tbDocPrint.Click += tbDocPrint_Click;
            tbDocPrint.Image = Bitmap.FromResource("Resources.btn_print.gif");*/

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
                                        tbNext});

            //

            miFileNew = new ButtonMenuItem();
            miFileNew.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.N)));
            miFileNew.Text = "miFileNew";
            miFileNew.Click += miFileNew_Click;

            miFileLoad = new ButtonMenuItem();
            miFileLoad.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.O)));
            miFileLoad.Text = "miFileLoad";
            miFileLoad.Click += miFileLoad_Click;

            miMRUFiles = new ButtonMenuItem();
            miMRUFiles.Enabled = false;
            miMRUFiles.Text = "miMRUFiles";

            miFileSave = new ButtonMenuItem();
            miFileSave.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.S)));
            miFileSave.Text = "miFileSave";
            miFileSave.Click += miFileSave_Click;

            miFileSaveAs = new ButtonMenuItem();
            miFileSaveAs.Text = "miFileSaveAs";
            miFileSaveAs.Click += miFileSaveAs_Click;

            miFileClose = new ButtonMenuItem();
            miFileClose.Text = "miFileClose";
            miFileClose.Click += miFileClose_Click;

            miFileProperties = new ButtonMenuItem();
            miFileProperties.Text = "miFileProperties";
            miFileProperties.Click += miFileProperties_Click;

            miExportToExcelFile = new ButtonMenuItem();
            miExportToExcelFile.Text = "miExportToExcelFile";
            miExportToExcelFile.Click += miExportToExcelFile_Click;

            miExport = new ButtonMenuItem();
            miExport.Items.AddRange(new MenuItem[] {
                                        miExportToExcelFile});
            miExport.Text = "miExport";

            miExit = new ButtonMenuItem();
            miExit.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.X)));
            miExit.Text = "miExit";
            miExit.Click += miExit_Click;

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

            miRecordEdit = new ButtonMenuItem();
            miRecordEdit.Text = "miRecordEdit";
            miRecordEdit.Click += miRecordEdit_Click;

            miRecordDelete = new ButtonMenuItem();
            miRecordDelete.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.L)));
            miRecordDelete.Text = "miRecordDelete";
            miRecordDelete.Click += miRecordDelete_Click;

            miSearch = new ButtonMenuItem();
            miSearch.Text = "miSearch";
            miSearch.Click += miSearch_Click;

            miFilter = new ButtonMenuItem();
            miFilter.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.F)));
            miFilter.Text = "miFilter";
            miFilter.Click += miFilter_Click;

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

            miTreeDescendants = new ButtonMenuItem();
            miTreeDescendants.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.D)));
            miTreeDescendants.Text = "miTreeDescendants";
            miTreeDescendants.Click += miTreeDescendants_Click;

            miTreeBoth = new ButtonMenuItem();
            miTreeBoth.Text = "miTreeBoth";
            miTreeBoth.Click += miTreeBoth_Click;

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

            miMap = new ButtonMenuItem();
            miMap.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.M)));
            miMap.Text = "miMap";
            miMap.Click += miMap_Click;

            miStats = new ButtonMenuItem();
            miStats.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.T)));
            miStats.Text = "miStats";
            miStats.Click += miStats_Click;

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
                                          new SeparatorMenuItem(),
                                          miMap,
                                          new SeparatorMenuItem(),
                                          miStats,
                                          new SeparatorMenuItem(),
                                          miRelationshipCalculator});

            miOrganizer = new ButtonMenuItem();
            miOrganizer.Text = "miOrganizer";
            miOrganizer.Click += miOrganizer_Click;

            miSlideshow = new ButtonMenuItem();
            miSlideshow.Text = "miSlideshow";
            miSlideshow.Click += miSlideshow_Click;

            miScripts = new ButtonMenuItem();
            miScripts.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.F11)));
            miScripts.Text = "miScripts";
            miScripts.Click += miScripts_Click;

            miTreeTools = new ButtonMenuItem();
            miTreeTools.Text = "miTreeTools";
            miTreeTools.Click += miTreeTools_Click;

            miOptions = new ButtonMenuItem();
            miOptions.Text = "miOptions";
            miOptions.Click += miOptions_Click;

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

            miPlugins = new ButtonMenuItem();
            miPlugins.Text = "Plugins";

            miContext = new ButtonMenuItem();
            miContext.Shortcut = Keys.F1;
            miContext.Text = "miContext";
            miContext.Click += miContext_Click;

            miLogSend = new ButtonMenuItem();
            miLogSend.Text = "miLogSend";
            miLogSend.Click += miLogSend_Click;

            miLogView = new ButtonMenuItem();
            miLogView.Text = "miLogView";
            miLogView.Click += miLogView_Click;

            miAbout = new ButtonMenuItem();
            miAbout.Text = "miAbout";
            miAbout.Click += miAbout_Click;

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

            miRecordDuplicate = new ButtonMenuItem();
            miRecordDuplicate.Text = "miRecordDuplicate";
            miRecordDuplicate.Click += miRecordDuplicate_Click;

            contextMenu = new ContextMenu();
            contextMenu.Items.AddRange(new MenuItem[] {
                                           miContRecordAdd,
                                           miContRecordEdit,
                                           miContRecordDelete,
                                           miRecordDuplicate});
            contextMenu.Opening += contextMenu_Opening;

            //

            tabsRecords = new TabControl();
            tabsRecords.SelectedIndexChanged += tabsRecords_SelectedIndexChanged;

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

            Icon = Icon.FromResource("Resources.icon_gedkeeper.ico");
            Title = "BaseWinSDI";
            GotFocus += Form_Activated;
            LostFocus += Form_Deactivate;
            Closing += Form_Closing;
            Closed += Form_Closed;
            Load += Form_Load;
            //DragDrop += Form_DragDrop;
            //DragEnter += Form_DragEnter;
            KeyDown += Form_KeyDown;

            UIHelper.SetPredefProperties(this, 980, 460, true);
            ResumeLayout();
        }
    }
}
