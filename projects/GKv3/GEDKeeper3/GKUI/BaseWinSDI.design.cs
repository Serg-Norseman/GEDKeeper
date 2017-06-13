using System;
using System.ComponentModel;
using System.Runtime.Remoting.Contexts;
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
        private SeparatorToolItem TBS1;
        private ButtonToolItem tbRecordAdd;
        private ButtonToolItem tbRecordEdit;
        private ButtonToolItem tbRecordDelete;
        private SeparatorToolItem TBS2;
        private ButtonToolItem tbFilter;
        private ButtonToolItem tbTreeAncestors;
        private ButtonToolItem tbTreeDescendants;
        private SeparatorToolItem TBS4;
        private ButtonToolItem tbPedigree; // FIXME: GKv3 DevRestriction
        private SeparatorToolItem TBS6;
        private ButtonToolItem tbStats;
        private SeparatorToolItem TBS5;
        private ButtonToolItem tbPrev;
        private ButtonToolItem tbNext;
        private SeparatorToolItem TBS8;
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
        private SeparatorMenuItem N1;
        private ButtonMenuItem miFileProperties;
        private SeparatorMenuItem N2;
        private ButtonMenuItem miExportToExcelFile;
        private ButtonMenuItem miExportToFamilyBook;
        private SeparatorMenuItem N3;
        private ButtonMenuItem miTreeTools;
        private ButtonMenuItem miExit;
        private ButtonMenuItem miEdit;
        private ButtonMenuItem miRecordAdd;
        private ButtonMenuItem miRecordEdit;
        private ButtonMenuItem miRecordDelete;
        private SeparatorMenuItem N15;
        private ButtonMenuItem miSearch;
        private SeparatorMenuItem N6;
        private ButtonMenuItem miFilter;
        private SeparatorMenuItem N7;
        private ButtonMenuItem miOptions;
        private ButtonMenuItem miPedigree;
        private ButtonMenuItem miTreeAncestors;
        private ButtonMenuItem miTreeDescendants;
        private SeparatorMenuItem N8;
        private ButtonMenuItem miPedigree_dAboville;
        private ButtonMenuItem miPedigree_Konovalov;
        private SeparatorMenuItem N9;
        private ButtonMenuItem miMap;
        private SeparatorMenuItem N10;
        private ButtonMenuItem miStats;
        private ButtonMenuItem miHelp;
        private ButtonMenuItem miContext;
        private ButtonMenuItem miLogSend;
        private ButtonMenuItem miLogView;
        private SeparatorMenuItem N13;
        private ButtonMenuItem miAbout;
        private ContextMenu MenuMRU;
        private ContextMenu MenuPedigree;
        private ButtonMenuItem miPedigree_dAboville2;
        private ButtonMenuItem miPedigree_Konovalov2;
        private SeparatorToolItem TBS7;
        private ButtonMenuItem miOrganizer;
        private ButtonMenuItem miService;
        private SeparatorMenuItem N12;
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
        private SeparatorMenuItem SeparatorToolItem1;
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
            TBS1 = new SeparatorToolItem();
            tbRecordAdd = new ButtonToolItem();
            tbRecordEdit = new ButtonToolItem();
            tbRecordDelete = new ButtonToolItem();
            TBS2 = new SeparatorToolItem();
            tbFilter = new ButtonToolItem();
            TBS4 = new SeparatorToolItem();
            tbTreeAncestors = new ButtonToolItem();
            tbTreeDescendants = new ButtonToolItem();
            tbTreeBoth = new ButtonToolItem();
            TBS5 = new SeparatorToolItem();
            tbPedigree = new ButtonToolItem();
            MenuPedigree = new ContextMenu();
            miPedigree_dAboville2 = new ButtonMenuItem();
            miPedigree_Konovalov2 = new ButtonMenuItem();
            TBS6 = new SeparatorToolItem();
            tbStats = new ButtonToolItem();
            TBS7 = new SeparatorToolItem();
            tbPrev = new ButtonToolItem();
            tbNext = new ButtonToolItem();
            TBS8 = new SeparatorToolItem();
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
            N1 = new SeparatorMenuItem();
            miFileProperties = new ButtonMenuItem();
            N2 = new SeparatorMenuItem();
            miExport = new ButtonMenuItem();
            miExportToExcelFile = new ButtonMenuItem();
            N3 = new SeparatorMenuItem();
            miExit = new ButtonMenuItem();
            miEdit = new ButtonMenuItem();
            miRecordAdd = new ButtonMenuItem();
            miRecordEdit = new ButtonMenuItem();
            miRecordDelete = new ButtonMenuItem();
            N15 = new SeparatorMenuItem();
            miSearch = new ButtonMenuItem();
            miFilter = new ButtonMenuItem();
            miPedigree = new ButtonMenuItem();
            miTreeAncestors = new ButtonMenuItem();
            miTreeDescendants = new ButtonMenuItem();
            miTreeBoth = new ButtonMenuItem();
            miAncestorsCircle = new ButtonMenuItem();
            miDescendantsCircle = new ButtonMenuItem();
            N6 = new SeparatorMenuItem();
            miPedigreeAscend = new ButtonMenuItem();
            miPedigree_dAboville = new ButtonMenuItem();
            miPedigree_Konovalov = new ButtonMenuItem();
            miExportToFamilyBook = new ButtonMenuItem();
            N7 = new SeparatorMenuItem();
            miMap = new ButtonMenuItem();
            N8 = new SeparatorMenuItem();
            miStats = new ButtonMenuItem();
            SeparatorToolItem1 = new SeparatorMenuItem();
            miRelationshipCalculator = new ButtonMenuItem();
            miService = new ButtonMenuItem();
            miOrganizer = new ButtonMenuItem();
            miSlideshow = new ButtonMenuItem();
            N9 = new SeparatorMenuItem();
            miScripts = new ButtonMenuItem();
            miTreeTools = new ButtonMenuItem();
            N10 = new SeparatorMenuItem();
            miOptions = new ButtonMenuItem();
            miPlugins = new ButtonMenuItem();
            miHelp = new ButtonMenuItem();
            miContext = new ButtonMenuItem();
            N12 = new SeparatorMenuItem();
            miLogSend = new ButtonMenuItem();
            miLogView = new ButtonMenuItem();
            N13 = new SeparatorMenuItem();
            miAbout = new ButtonMenuItem();
            contextMenu = new ContextMenu();
            miContRecordAdd = new ButtonMenuItem();
            miContRecordEdit = new ButtonMenuItem();
            miContRecordDelete = new ButtonMenuItem();
            miRecordDuplicate = new ButtonMenuItem();
            tabsRecords = new TabControl();
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
                                        TBS1,
                                        tbRecordAdd,
                                        tbRecordEdit,
                                        tbRecordDelete,
                                        TBS2,
                                        tbFilter,
                                        TBS4,
                                        tbTreeAncestors,
                                        tbTreeDescendants,
                                        tbTreeBoth,
                                        TBS5,
                                        tbPedigree,
                                        TBS6,
                                        tbStats,
                                        TBS7,
                                        tbPrev,
                                        tbNext,
                                        TBS8,
                                        tbDocPreview,
                                        tbDocPrint});

            tbFileNew.Click += ToolBar1_ButtonClick;
            tbFileLoad.Click += ToolBar1_ButtonClick;

            tbLoadMRU.Text = "v";
            tbLoadMRU.Click  += (sender, e) => MenuMRU.Show(this);

            tbFileSave.Click += ToolBar1_ButtonClick;

            tbRecordAdd.Click += ToolBar1_ButtonClick;
            tbRecordEdit.Click += ToolBar1_ButtonClick;
            tbRecordDelete.Click += ToolBar1_ButtonClick;

            tbFilter.Click += ToolBar1_ButtonClick;

            tbTreeAncestors.Click += ToolBar1_ButtonClick;
            tbTreeDescendants.Click += ToolBar1_ButtonClick;
            tbTreeBoth.Click += ToolBar1_ButtonClick;

            tbLoadMRU.Click  += (sender, e) => MenuPedigree.Show(this);

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
                                      N1,
                                      miFileProperties,
                                      N2,
                                      miExport,
                                      N3,
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
                                      N15,
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
                                          N6,
                                          miPedigreeAscend,
                                          miPedigree_dAboville,
                                          miPedigree_Konovalov,
                                          miExportToFamilyBook,
                                          N7,
                                          miMap,
                                          N8,
                                          miStats,
                                          SeparatorToolItem1,
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
                                         N9,
                                         miScripts,
                                         miTreeTools,
                                         N10,
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
                                      N12,
                                      miLogSend,
                                      miLogView,
                                      N13,
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

            tabsRecords.SelectedIndex = 0;
            tabsRecords.Size = new Size(976, 385);
            tabsRecords.SelectedIndexChanged += PageRecords_SelectedIndexChanged;

            Menu = MainMenu1;
            ToolBar = ToolBar1;

            ClientSize = new Size(976, 462);
            Content = tabsRecords;
            Title = "BaseWinSDI";
            //Activated += Form_Activated;
            //Deactivate += Form_Deactivate;
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
