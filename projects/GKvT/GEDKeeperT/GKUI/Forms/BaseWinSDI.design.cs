using Terminal.Gui.App;
using Terminal.Gui.Drawing;
using Terminal.Gui.Drivers;
using Terminal.Gui.ViewBase;
using Terminal.Gui.Views;

namespace GKUI.Forms
{
    partial class BaseWinSDI
    {
        private void InitializeComponent()
        {
            MainMenu1 = new MenuBar(new MenuBarItem[] {
                miFile = new MenuBarItem("miFile", new MenuItem[] {
                    miFileNew = new MenuItem("miFileNew", "", miFileNew_Click, KeyCode.CtrlMask | KeyCode.N),
                    miFileLoad = new MenuItem("miFileLoad", "", miFileLoad_Click, KeyCode.CtrlMask | KeyCode.O),
                    miMRUFiles = new MenuItem("miMRUFiles", "", null, null),
                    miFileSave = new MenuItem("miFileSave", "", miFileSave_Click, KeyCode.CtrlMask | KeyCode.S),
                    miFileSaveAs = new MenuItem("miFileSaveAs", "", miFileSaveAs_Click),
                    miFileClose = new MenuItem("miFileClose", "", miFileClose_Click),
                    null,
                    miFileProperties = new MenuItem("miFileProperties", "", miFileProperties_Click),
                    null,
                    miExport = new MenuItem("miExport", "", new Menu(new MenuItem[] {
                        miExportTable = new MenuItem("miExportTable", "", miExportTable_Click),
                        miExportToStrictGEDCOM = new MenuItem("miExportToStrictGEDCOM", "", miExportToStrictGEDCOM_Click),
                    })),
                    null,
                    miExit = new MenuItem("miExit", "", miExit_Click, KeyCode.CtrlMask | KeyCode.X)
                }),
                miEdit = new MenuBarItem("miEdit", new MenuItem[] {
                    miRecordAdd = new MenuItem("miRecordAdd", "", miRecordAdd_Click, KeyCode.CtrlMask | KeyCode.I),
                    miRecordEdit = new MenuItem("miRecordEdit", "", miRecordEdit_Click),
                    miRecordDelete = new MenuItem("miRecordDelete", "", miRecordDelete_Click, KeyCode.CtrlMask | KeyCode.L),
                    null,
                    miSearch = new MenuItem("miSearch", "", miSearch_Click),
                    miFindAndReplace = new MenuItem("miFindAndReplace", "", miFindAndReplace_Click),
                    miFilter = new MenuItem("miFilter", "", miFilter_Click, KeyCode.CtrlMask | KeyCode.F)
                }),
                miPedigree = new MenuBarItem("miPedigree", new MenuItem[] {
                    miTreeAncestors = new MenuItem("miTreeAncestors", "", miTreeAncestors_Click, KeyCode.CtrlMask | KeyCode.A),
                    miTreeDescendants = new MenuItem( "miTreeDescendants", "", miTreeDescendants_Click, KeyCode.CtrlMask | KeyCode.D),
                    miTreeBoth = new MenuItem("miTreeBoth", "", miTreeBoth_Click),
                    //miAncestorsCircle = new MenuItem("miAncestorsCircle", "", miAncestorsCircle_Click),
                    //miDescendantsCircle = new MenuItem("miDescendantsCircle", "", miDescendantsCircle_Click),
                    null,
                    miPedigreeAscend = new MenuItem("miPedigreeAscend", "", miPedigreeAscend_Click),
                    miPedigreeDescend = new MenuItem("miPedigreeDescend", "", miPedigreeDescend_Click, KeyCode.CtrlMask | KeyCode.K),
                    miExportToFamilyBook = new MenuItem("miExportToFamilyBook", "", miExportToFamilyBook_Click),
                    miExportToTreesAlbum = new MenuItem("miExportToTreesAlbum", "", miExportToTreesAlbum_Click),
                    //null,
                    //miMap = new MenuItem("miMap", "", miMap_Click, null, null, KeyCode.CtrlMask | KeyCode.M),
                    null,
                    miStats = new MenuItem("miStats", "", miStats_Click, KeyCode.CtrlMask | KeyCode.T),
                    null,
                    miRelationshipCalculator = new MenuItem("miRelationshipCalculator", "", miRelationshipCalculator_Click)
                }),
                miService = new MenuBarItem("miService", new MenuItem[] {
                    miChronicle = new MenuItem("miChronicle", "", miChronicle_Click),
                    miOrganizer = new MenuItem("miOrganizer", "", miOrganizer_Click),
                    //miSlideshow = new MenuItem("miSlideshow", "", miSlideshow_Click),
                    null,
                    miScripts = new MenuItem("miScripts", "", miScripts_Click, KeyCode.F11),
                    miTreeTools = new MenuItem("miTreeTools", "", new Menu(new MenuItem[] {
                        miTreeCompare = new MenuItem("miTreeCompare", "", miTTTreeCompare_Click),
                        miTreeMerge = new MenuItem("miTreeMerge", "", miTTTreeMerge_Click),
                        miTreeSplit = new MenuItem("miTreeSplit", "", miTTTreeSplit_Click),
                        miRecMerge = new MenuItem("miRecMerge", "", miTTRecMerge_Click),
                        miFamilyGroups = new MenuItem("miFamilyGroups", "", miTTFamilyGroups_Click),
                        miTreeCheck = new MenuItem("miTreeCheck", "", miTTTreeCheck_Click),
                        miPatSearch = new MenuItem("miPatSearch", "", miTTPatSearch_Click),
                        miPlacesManager = new MenuItem("miPlacesManager", "", miTTPlacesManager_Click)
                    })),
                    null,
                    miOptions = new MenuItem("miOptions", "", miOptions_Click)
                }),
                miReports = new MenuBarItem("miReports", new MenuItem[] {
                }),
                miPlugins = new MenuBarItem("miPlugins", new MenuItem[] {
                }),
                miHelp = new MenuBarItem("miHelp", new MenuItem[] {
                    miContext = new MenuItem("miContext", "", miContext_Click, KeyCode.F1),
                    null,
                    miLogSend = new MenuItem("miLogSend", "", miLogSend_Click),
                    miLogView = new MenuItem("miLogView", "", miLogView_Click),
                    null,
                    miAbout = new MenuItem("miAbout", "", miAbout_Click)
                })
            });

            tabsRecords = new TabView() { Width = Dim.Fill(), Height = Dim.Fill(1), Y = Pos.Bottom(MainMenu1) };
            tabsRecords.SelectedTabChanged += tabsRecords_SelectedIndexChanged;
            tabsRecords.CanFocus = false;

            lblStatusLine = new Label();
            StatusBar = new StatusBar() { Visible = true };
            StatusBar.Add(lblStatusLine);

            BorderStyle = LineStyle.None;

            Add(MainMenu1, tabsRecords, StatusBar);

            InitializeContextMenu();

            // FIXME
            /*var thisWin = (Window)this;
            thisWin.Activate += Form_Activated;
            thisWin.Deactivate += Form_Deactivate;
            thisWin.Loaded += Form_Load;
            thisWin.Closing += Form_Closing;
            thisWin.Closed += Form_Closed;*/
        }

        private void InitializeContextMenu()
        {
            miContMediaMoveFile2Abs = new MenuItem("miContMediaMoveFile2Abs", "", miContMediaMoveFile_Click);
            miContMediaMoveFile2Rel = new MenuItem("miContMediaMoveFile2Rel", "", miContMediaMoveFile_Click);
            miContMediaMoveFile2Arc = new MenuItem("miContMediaMoveFile2Arc", "", miContMediaMoveFile_Click);
            miContMediaMoveFile = new MenuBarItem("miContMediaMoveFile", new MenuItem[] { miContMediaMoveFile2Abs, miContMediaMoveFile2Rel, miContMediaMoveFile2Arc });

            miContRecordAdd = new MenuItem("miContRecordAdd", "", miRecordAdd_Click);
            miContRecordEdit = new MenuItem("miContRecordEdit", "", miRecordEdit_Click);
            miContRecordDelete = new MenuItem("miContRecordDelete", "", miRecordDelete_Click);
            miContRecordDuplicate = new MenuItem("miContRecordDuplicate", "", miRecordDuplicate_Click);
            miContRecordMerge = new MenuItem("miContRecordMerge", "", miRecordMerge_Click);

            contextMenu = new PopoverMenu(new MenuItem[] {
                miContRecordAdd, miContRecordEdit, miContRecordDelete, miContRecordDuplicate, miContRecordMerge, miContMediaMoveFile
            });
            Application.Popover.Register(contextMenu);

            miCopyContent = new MenuItem("miCopyContent", "", miCopyContent_Click);

            summaryMenu = new PopoverMenu(new MenuItem[] {
                miCopyContent
            });
            Application.Popover.Register(summaryMenu);
        }
    }
}
