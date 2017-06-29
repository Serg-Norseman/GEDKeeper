using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI
{
    partial class BaseWinSDI
    {
        //private StatusBar StatusBar; // FIXME: GKv3 DevRestriction
        //private StatusBarPanel StatusBarPanel1; // FIXME: GKv3 DevRestriction
        //private StatusBarPanel StatusBarPanel2; // FIXME: GKv3 DevRestriction
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
        private ButtonToolItem tbPedigree; // FIXME: GKv3 DevRestriction
        private ButtonToolItem tbStats;
        private ButtonToolItem tbPrev;
        private ButtonToolItem tbNext;
        private ButtonToolItem tbDocPrint;
        private ButtonToolItem tbDocPreview;
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
        private ButtonToolItem tbLoadMRU; // FIXME: GKv3 DevRestriction
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
            /*StatusBar = new StatusBar();
            StatusBarPanel1 = new StatusBarPanel();
            StatusBarPanel2 = new StatusBarPanel();*/
            ToolBar1 = new ToolBar();
            tbFileNew = new ButtonToolItem();
            tbFileLoad = new ButtonToolItem();
            tbLoadMRU = new ButtonToolItem();
            MenuMRU = new ContextMenu();
            tbFileSave = new ButtonToolItem();
            tbRecordAdd = new ButtonToolItem();
            tbRecordEdit = new ButtonToolItem();
            tbRecordDelete = new ButtonToolItem();
            tbFilter = new ButtonToolItem();
            tbTreeAncestors = new ButtonToolItem();
            tbTreeDescendants = new ButtonToolItem();
            tbTreeBoth = new ButtonToolItem();
            tbPedigree = new ButtonToolItem();
            MenuPedigree = new ContextMenu();
            miPedigree_dAboville2 = new ButtonMenuItem();
            miPedigree_Konovalov2 = new ButtonMenuItem();
            tbStats = new ButtonToolItem();
            tbPrev = new ButtonToolItem();
            tbNext = new ButtonToolItem();
            tbDocPreview = new ButtonToolItem();
            tbDocPrint = new ButtonToolItem();
            MainMenu1 = new MenuBar();
            miFile = new ButtonMenuItem();
            miFileNew = new ButtonMenuItem();
            miFileLoad = new ButtonMenuItem();
            miMRUFiles = new ButtonMenuItem();
            miFileSave = new ButtonMenuItem();
            miFileSaveAs = new ButtonMenuItem();
            miFileClose = new ButtonMenuItem();
            miFileProperties = new ButtonMenuItem();
            miExport = new ButtonMenuItem();
            miExportToExcelFile = new ButtonMenuItem();
            miExit = new ButtonMenuItem();
            miEdit = new ButtonMenuItem();
            miRecordAdd = new ButtonMenuItem();
            miRecordEdit = new ButtonMenuItem();
            miRecordDelete = new ButtonMenuItem();
            miSearch = new ButtonMenuItem();
            miFilter = new ButtonMenuItem();
            miPedigree = new ButtonMenuItem();
            miTreeAncestors = new ButtonMenuItem();
            miTreeDescendants = new ButtonMenuItem();
            miTreeBoth = new ButtonMenuItem();
            miAncestorsCircle = new ButtonMenuItem();
            miDescendantsCircle = new ButtonMenuItem();
            miPedigreeAscend = new ButtonMenuItem();
            miPedigree_dAboville = new ButtonMenuItem();
            miPedigree_Konovalov = new ButtonMenuItem();
            miExportToFamilyBook = new ButtonMenuItem();
            miMap = new ButtonMenuItem();
            miStats = new ButtonMenuItem();
            miRelationshipCalculator = new ButtonMenuItem();
            miService = new ButtonMenuItem();
            miOrganizer = new ButtonMenuItem();
            miSlideshow = new ButtonMenuItem();
            miScripts = new ButtonMenuItem();
            miTreeTools = new ButtonMenuItem();
            miOptions = new ButtonMenuItem();
            miPlugins = new ButtonMenuItem();
            miHelp = new ButtonMenuItem();
            miContext = new ButtonMenuItem();
            miLogSend = new ButtonMenuItem();
            miLogView = new ButtonMenuItem();
            miAbout = new ButtonMenuItem();
            contextMenu = new ContextMenu();
            miContRecordAdd = new ButtonMenuItem();
            miContRecordEdit = new ButtonMenuItem();
            miContRecordDelete = new ButtonMenuItem();
            miRecordDuplicate = new ButtonMenuItem();

            SuspendLayout();

            /*StatusBar.Location = new Point(0, 438);
            StatusBar.Margin = new Padding(2);
            StatusBar.Name = "StatusBar";
            StatusBar.Panels.AddRange(new StatusBarPanel[] {
                                          StatusBarPanel1,
                                          StatusBarPanel2});
            StatusBar.ShowPanels = true;
            StatusBar.Size = new Size(976, 24);
            StatusBar.TabIndex = 0;
            StatusBar.DrawItem += new StatusBarDrawItemEventHandler(StatusBar_DrawItem);
            StatusBar.PanelClick += new StatusBarPanelClickEventHandler(StatusBar_PanelClick);

            StatusBarPanel1.Name = "StatusBarPanel1";
            StatusBarPanel1.Width = 50;

            StatusBarPanel2.Name = "StatusBarPanel2";
            StatusBarPanel2.Style = StatusBarPanelStyle.OwnerDraw;
            StatusBarPanel2.Width = 24;*/

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
                                        tbDocPreview,
                                        tbDocPrint});

            tbFileNew.Click += ToolBar1_ButtonClick;
            tbFileLoad.Click += ToolBar1_ButtonClick;

            tbLoadMRU.Text = "▼";
            tbLoadMRU.Click  += (sender, e) => {
                if (MenuMRU.Items.Count > 0) {
                    MenuMRU.Show(this);
                }
            };

            tbFileSave.Click += ToolBar1_ButtonClick;

            tbRecordAdd.Click += ToolBar1_ButtonClick;
            tbRecordEdit.Click += ToolBar1_ButtonClick;
            tbRecordDelete.Click += ToolBar1_ButtonClick;

            tbFilter.Click += ToolBar1_ButtonClick;

            tbTreeAncestors.Click += ToolBar1_ButtonClick;
            tbTreeDescendants.Click += ToolBar1_ButtonClick;
            tbTreeBoth.Click += ToolBar1_ButtonClick;

            tbPedigree.Click  += (sender, e) => MenuPedigree.Show(this);

            MenuPedigree.Items.AddRange(new MenuItem[] {
                                            miPedigree_dAboville2,
                                            miPedigree_Konovalov2});

            miPedigree_dAboville2.Click += miPedigree_dAbovilleClick;

            miPedigree_Konovalov2.Click += miPedigree_KonovalovClick;

            tbStats.Click += ToolBar1_ButtonClick;

            tbPrev.Enabled = false;
            tbPrev.Click += ToolBar1_ButtonClick;

            tbNext.Enabled = false;
            tbNext.Click += ToolBar1_ButtonClick;

            tbDocPreview.Click += ToolBar1_ButtonClick;

            tbDocPrint.Click += ToolBar1_ButtonClick;

            MainMenu1.Items.AddRange(new MenuItem[] {
                                         miFile,
                                         miEdit,
                                         miPedigree,
                                         miService,
                                         miPlugins,
                                         miHelp});

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
            miFile.Text = "File";

            miFileNew.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.N)));
            miFileNew.Text = "miFileNew";
            miFileNew.Click += miFileNew_Click;

            miFileLoad.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.O)));
            miFileLoad.Text = "miFileLoad";
            miFileLoad.Click += miFileLoad_Click;

            miMRUFiles.Enabled = false;
            miMRUFiles.Text = "miMRUFiles";

            miFileSave.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.S)));
            miFileSave.Text = "miFileSave";
            miFileSave.Click += miFileSave_Click;

            miFileSaveAs.Text = "miFileSaveAs";
            miFileSaveAs.Click += miFileSaveAs_Click;

            miFileClose.Text = "miFileClose";
            miFileClose.Click += miFileClose_Click;

            miFileProperties.Text = "miFileProperties";
            miFileProperties.Click += miFileProperties_Click;

            miExport.Items.AddRange(new MenuItem[] {
                                        miExportToExcelFile});
            miExport.Text = "miExport";

            miExportToExcelFile.Text = "miExportToExcelFile";
            miExportToExcelFile.Click += miExportToExcelFile_Click;

            miExit.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.X)));
            miExit.Text = "miExit";
            miExit.Click += miExit_Click;

            miEdit.Items.AddRange(new MenuItem[] {
                                      miRecordAdd,
                                      miRecordEdit,
                                      miRecordDelete,
                                      new SeparatorMenuItem(),
                                      miSearch,
                                      miFilter});
            miEdit.Text = "Edit";

            miRecordAdd.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.I)));
            miRecordAdd.Text = "miRecordAdd";
            miRecordAdd.Click += miRecordAdd_Click;

            miRecordEdit.Text = "miRecordEdit";
            miRecordEdit.Click += miRecordEdit_Click;

            miRecordDelete.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.L)));
            miRecordDelete.Text = "miRecordDelete";
            miRecordDelete.Click += miRecordDelete_Click;

            miSearch.Text = "miSearch";
            miSearch.Click += miSearch_Click;

            miFilter.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.F)));
            miFilter.Text = "miFilter";
            miFilter.Click += miFilter_Click;

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
            miPedigree.Text = "Pedigree";

            miTreeAncestors.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.A)));
            miTreeAncestors.Text = "miTreeAncestors";
            miTreeAncestors.Click += miTreeAncestors_Click;

            miTreeDescendants.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.D)));
            miTreeDescendants.Text = "miTreeDescendants";
            miTreeDescendants.Click += miTreeDescendants_Click;

            miTreeBoth.Text = "miTreeBoth";
            miTreeBoth.Click += miTreeBoth_Click;

            miAncestorsCircle.Text = "miAncestorsCircle";
            miAncestorsCircle.Click += miAncestorsCircle_Click;

            miDescendantsCircle.Text = "miDescendantsCircle";
            miDescendantsCircle.Click += miDescendantsCircle_Click;

            miPedigreeAscend.Text = "miPedigreeAscend";
            miPedigreeAscend.Click += miPedigreeAscend_Click;

            miPedigree_dAboville.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.P)));
            miPedigree_dAboville.Text = "miPedigree_dAboville";
            miPedigree_dAboville.Click += miPedigree_dAbovilleClick;

            miPedigree_Konovalov.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.K)));
            miPedigree_Konovalov.Text = "miPedigree_Konovalov";
            miPedigree_Konovalov.Click += miPedigree_KonovalovClick;

            miExportToFamilyBook.Text = "miExportToFamilyBook";
            miExportToFamilyBook.Click += miExportToFamilyBook_Click;

            miMap.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.M)));
            miMap.Text = "miMap";
            miMap.Click += miMap_Click;

            miStats.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.T)));
            miStats.Text = "miStats";
            miStats.Click += miStats_Click;

            miRelationshipCalculator.Text = "miRelationshipCalculator";
            miRelationshipCalculator.Click += miRelationshipCalculator_Click;

            miService.Items.AddRange(new MenuItem[] {
                                         miOrganizer,
                                         miSlideshow,
                                         new SeparatorMenuItem(),
                                         miScripts,
                                         miTreeTools,
                                         new SeparatorMenuItem(),
                                         miOptions});
            miService.Text = "Service";

            miOrganizer.Text = "miOrganizer";
            miOrganizer.Click += miOrganizer_Click;

            miSlideshow.Text = "miSlideshow";
            miSlideshow.Click += miSlideshow_Click;

            miScripts.Shortcut = ((Keys)((Application.Instance.CommonModifier | Keys.F11)));
            miScripts.Text = "miScripts";
            miScripts.Click += miScripts_Click;

            miTreeTools.Text = "miTreeTools";
            miTreeTools.Click += miTreeTools_Click;

            miOptions.Text = "miOptions";
            miOptions.Click += miOptions_Click;

            miPlugins.Text = "Plugins";

            miHelp.Items.AddRange(new MenuItem[] {
                                      miContext,
                                      new SeparatorMenuItem(),
                                      miLogSend,
                                      miLogView,
                                      new SeparatorMenuItem(),
                                      miAbout});
            miHelp.Text = "Help";

            miContext.Shortcut = Keys.F1;
            miContext.Text = "miContext";
            miContext.Click += miContext_Click;

            miLogSend.Text = "miLogSend";
            miLogSend.Click += miLogSend_Click;

            miLogView.Text = "miLogView";
            miLogView.Click += miLogView_Click;

            miAbout.Text = "miAbout";
            miAbout.Click += miAbout_Click;

            contextMenu.Items.AddRange(new MenuItem[] {
                                           miContRecordAdd,
                                           miContRecordEdit,
                                           miContRecordDelete,
                                           miRecordDuplicate});
            contextMenu.Opening += contextMenu_Opening;

            miContRecordAdd.Text = "miContRecordAdd";
            miContRecordAdd.Click += miRecordAdd_Click;

            miContRecordEdit.Text = "miContRecordEdit";
            miContRecordEdit.Click += miRecordEdit_Click;

            miContRecordDelete.Text = "miContRecordDelete";
            miContRecordDelete.Click += miRecordDelete_Click;

            miRecordDuplicate.Text = "miRecordDuplicate";
            miRecordDuplicate.Click += miRecordDuplicate_Click;

            tabsRecords = new TabControl();
            tabsRecords.SelectedIndexChanged += PageRecords_SelectedIndexChanged;

            Content = tabsRecords;
            Menu = MainMenu1;
            ToolBar = ToolBar1;

            ClientSize = new Size(976, 462);
            Title = "BaseWinSDI";
            GotFocus += Form_Activated;
            LostFocus += Form_Deactivate;
            Closing += Form_Closing;
            Closed += Form_Closed;
            Load += Form_Load;
            //VisibleChanged += Form_Show;
            //DragDrop += Form_DragDrop;
            //DragEnter += Form_DragEnter;
            KeyDown += Form_KeyDown;
            SizeChanged += Form_Resize;

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
