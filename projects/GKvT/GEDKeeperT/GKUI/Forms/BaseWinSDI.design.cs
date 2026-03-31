#pragma warning disable IDE1006 // Naming Styles

using System;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class BaseWinSDI
    {
        private StatusBar stbMain;
        private MenuBar MainMenu1;
        private MenuBarItem miFile;
        private MenuItem miFileNew;
        private MenuItem miFileLoad;
        private MenuItem miFileReload;
        private MenuBarItem miMRUFiles;
        private MenuItem miFileSave;
        private MenuItem miFileSaveAs;
        private MenuItem miFileClose;
        private MenuItem miFileProperties;
        private MenuItem miExportTable;
        private MenuItem miExportToFamilyBook;
        private MenuItem miExportToTreesAlbum;
        private MenuItem miTreeTools;
        private MenuItem miExit;
        private MenuBarItem miEdit;
        private MenuItem miRecordAdd;
        private MenuItem miRecordEdit;
        private MenuItem miRecordDelete;
        private MenuItem miSearch;
        private MenuItem miFindAndReplace;
        private MenuItem miFilter;
        private MenuItem miOptions;
        private MenuBarItem miPedigree;
        private MenuItem miTreeAncestors;
        private MenuItem miTreeDescendants;
        private MenuItem miPedigreeDescend;
        private MenuItem miStats;
        private MenuBarItem miHelp;
        private MenuItem miContext;
        private MenuItem miLogSend;
        private MenuItem miLogView;
        private MenuItem miAbout;
        private MenuItem miOrganizer;
        private MenuBarItem miService;
        private MenuItem miScripts;
        private MenuItem miExport;
        private MenuItem miTreeBoth;
        private MenuBarItem miReports;
        private MenuBarItem miPlugins;
        private MenuItem miPedigreeAscend;
        private MenuItem miRelationshipCalculator;
        private TabView tabsRecords;
        private MenuItem miContRecordDuplicate;
        private MenuItem miContRecordDelete;
        private MenuItem miContRecordEdit;
        private MenuItem miContRecordMerge;
        private MenuItem miContMediaMoveFile2Abs;
        private MenuItem miContMediaMoveFile2Rel;
        private MenuItem miContMediaMoveFile2Arc;
        private MenuItem miContMediaMoveFile;
        private ContextMenu contextMenu;
        private MenuItem miContRecordAdd;
        private MenuItem miTreeCompare;
        private MenuItem miTreeMerge;
        private MenuItem miTreeSplit;
        private MenuItem miRecMerge;
        private MenuItem miFamilyGroups;
        private MenuItem miTreeCheck;
        private MenuItem miPatSearch;
        private MenuItem miPlacesManager;
        private ContextMenu summaryMenu;
        private MenuItem miCopyContent;
        private MenuItem miExportToStrictGEDCOM;
        private MenuItem miChronicle;
        private TabPage tab1;
        private TabPage tab2;
        private TabPage tab3;
        private TabPage tab4;
        private TabPage tab5;
        private TabPage tab6;
        private TabPage tab7;
        private TabPage tab8;
        private TabPage tab9;
        private TabPage tab10;
        private TabPage tab11;

        private void InitializeComponent()
        {
            stbMain = new StatusBar() {
                Visible = true,
                Items = new StatusItem[] {
                    new StatusItem(Key.Null, "", null)
                }
            };
            Application.Top.Add(stbMain);

            MainMenu1 = new MenuBar(new MenuBarItem[] {
                miFile = new MenuBarItem("_File", new MenuItem[] {
                    miFileNew = new MenuItem("_New", miFileNew_Click, Key.CtrlMask | Key.N),
                    miFileLoad = new MenuItem("_Open", miFileLoad_Click, Key.CtrlMask | Key.O),
                    miMRUFiles = new MenuBarItem("_Recent", "", null),
                    miFileSave = new MenuItem("_Save", miFileSave_Click, Key.CtrlMask | Key.S),
                    miFileSaveAs = new MenuItem("_SaveAs", miFileSaveAs_Click),
                    miFileClose = new MenuItem("_Close", miFileClose_Click),
                    null,
                    miFileProperties = new MenuItem("_Properties", miFileProperties_Click),
                    null,
                    miExport = new MenuBarItem("_Export", new MenuItem[] {
                        miExportTable = new MenuItem("_ExportTable", miExportTable_Click),
                        miExportToStrictGEDCOM = new MenuItem("Export to strict GEDCOM", miExportToStrictGEDCOM_Click),
                    }),
                    null,
                    miExit = new MenuItem("_Exit", miExit_Click, Key.CtrlMask | Key.X)
                }),
                miEdit = new MenuBarItem("_Edit", new MenuItem[] {
                    miRecordAdd = new MenuItem("miRecordAdd", miRecordAdd_Click, Key.CtrlMask | Key.I),
                    miRecordEdit = new MenuItem("miRecordEdit", miRecordEdit_Click),
                    miRecordDelete = new MenuItem("miRecordDelete", miRecordDelete_Click, Key.CtrlMask | Key.L),
                    null,
                    miSearch = new MenuItem("miSearch", miSearch_Click),
                    miFindAndReplace = new MenuItem("miFindAndReplace", miFindAndReplace_Click),
                    miFilter = new MenuItem("miFilter", miFilter_Click, Key.CtrlMask | Key.F)
                }),
                miPedigree = new MenuBarItem("_Pedigree", new MenuItem[] {
                    miTreeAncestors = new MenuItem("miTreeAncestors", miTreeAncestors_Click, Key.CtrlMask | Key.A),
                    miTreeDescendants = new MenuItem("miTreeDescendants", miTreeDescendants_Click, Key.CtrlMask | Key.D),
                    miTreeBoth = new MenuItem("miTreeBoth", miTreeBoth_Click),
                    null,
                    miPedigreeAscend = new MenuItem("miPedigreeAscend", miPedigreeAscend_Click),
                    miPedigreeDescend = new MenuItem("miPedigreeDescend", miPedigreeDescend_Click, Key.CtrlMask | Key.K),
                    miExportToFamilyBook = new MenuItem("miExportToFamilyBook", miExportToFamilyBook_Click),
                    miExportToTreesAlbum = new MenuItem("miExportToTreesAlbum", miExportToTreesAlbum_Click),
                    null,
                    miStats = new MenuItem("miStats", miStats_Click, Key.CtrlMask | Key.T),
                    null,
                    miRelationshipCalculator = new MenuItem("miRelationshipCalculator", miRelationshipCalculator_Click)
                }),
                miService = new MenuBarItem("_Service", new MenuItem[] {
                    miChronicle = new MenuItem("miChronicle", miChronicle_Click),
                    miOrganizer = new MenuItem("miOrganizer", miOrganizer_Click),
                    null,
                    //miScripts = new MenuItem("miScripts", miScripts_Click, Key.F11),
                    miTreeTools = new MenuBarItem("miTreeTools", new MenuItem[] {
                        miTreeCompare = new MenuItem("miTreeCompare", miTTTreeCompare_Click),
                        miTreeMerge = new MenuItem("miTreeMerge", miTTTreeMerge_Click),
                        miTreeSplit = new MenuItem("miTreeSplit", miTTTreeSplit_Click),
                        miRecMerge = new MenuItem("miRecMerge", miTTRecMerge_Click),
                        miFamilyGroups = new MenuItem("miFamilyGroups", miTTFamilyGroups_Click),
                        miTreeCheck = new MenuItem("miTreeCheck", miTTTreeCheck_Click),
                        miPatSearch = new MenuItem("miPatSearch", miTTPatSearch_Click),
                        miPlacesManager = new MenuItem("miPlacesManager", miTTPlacesManager_Click)
                    }),
                    null,
                    miOptions = new MenuItem("miOptions", miOptions_Click)
                }),
                miReports = new MenuBarItem("_Reports", Array.Empty<MenuItem>()),
                miPlugins = new MenuBarItem("_Plugins", Array.Empty<MenuItem>()),
                miHelp = new MenuBarItem("_Help", new MenuItem[] {
                    miContext = new MenuItem("miContext", miContext_Click, Key.F1),
                    null,
                    miLogSend = new MenuItem("miLogSend", miLogSend_Click),
                    miLogView = new MenuItem("miLogView", miLogView_Click),
                    null,
                    miAbout = new MenuItem("miAbout", miAbout_Click)
                })
            });
            Application.Top.Add(MainMenu1);

            tabsRecords = new TabView() { Width = Dim.Fill(), Height = Dim.Fill(), Y = 0 };
            tabsRecords.SelectedTabChanged += tabsRecords_SelectedIndexChanged;
            Add(tabsRecords);

            InitializeContextMenu();

            // The disabled border leaves padding of 1 on all sides!
            //Border.BorderStyle = BorderStyle.None;
            X = 0;
            Y = 1; // Leave one row for the toplevel menu
            Width = Dim.Fill();
            Height = Dim.Fill();

            var thisWin = (Window)this;
            thisWin.Activate += Form_Activated;
            thisWin.Deactivate += Form_Deactivate;
            thisWin.Loaded += Form_Load;
            thisWin.Closing += Form_Closing;
            thisWin.Closed += Form_Closed;
        }

        private void InitializeContextMenu()
        {
            miContMediaMoveFile2Abs = new MenuItem("miContMediaMoveFile2Abs", miContMediaMoveFile_Click);
            miContMediaMoveFile2Rel = new MenuItem("miContMediaMoveFile2Rel", miContMediaMoveFile_Click);
            miContMediaMoveFile2Arc = new MenuItem("miContMediaMoveFile2Arc", miContMediaMoveFile_Click);
            miContMediaMoveFile = new MenuBarItem("miContMediaMoveFile", new MenuItem[] { miContMediaMoveFile2Abs, miContMediaMoveFile2Rel, miContMediaMoveFile2Arc });

            miContRecordAdd = new MenuItem("miContRecordAdd", miRecordAdd_Click);
            miContRecordEdit = new MenuItem("miContRecordEdit", miRecordEdit_Click);
            miContRecordDelete = new MenuItem("miContRecordDelete", miRecordDelete_Click);
            miContRecordDuplicate = new MenuItem("miContRecordDuplicate", miRecordDuplicate_Click);
            miContRecordMerge = new MenuItem("miContRecordMerge", miRecordMerge_Click);

            contextMenu = new ContextMenu();
            contextMenu.Items.AddRange(new [] {
                miContRecordAdd, miContRecordEdit, miContRecordDelete, miContRecordDuplicate, miContRecordMerge, miContMediaMoveFile
            });
            //contextMenu.MenuBar.MenuOpening += contextMenu_Opening;

            miCopyContent = new MenuItem("miCopyContent", miCopyContent_Click);

            summaryMenu = new ContextMenu();
            summaryMenu.Items.AddRange(new[] { miCopyContent });
        }
    }
}
