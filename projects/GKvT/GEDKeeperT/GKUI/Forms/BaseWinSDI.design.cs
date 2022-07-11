using Terminal.Gui;

namespace GKUI.Forms
{
    partial class BaseWinSDI
    {
        private void InitializeComponent()
        {
            StatusBar = new StatusBar() {
                Visible = true,
                Items = new StatusItem[] {
                    new StatusItem(Key.Q | Key.CtrlMask, "~Ctrl-Q~ Quit", () => {
                        Application.RequestStop();
                    })
                }
            };
            Application.Top.Add(StatusBar);

            MainMenu1 = new MenuBar(new MenuBarItem[] {
                miFile = new MenuBarItem("_File", new MenuItem[] {
                    miFileNew = new MenuItem("_New", "", miFileNew_Click, null, null, Key.CtrlMask | Key.N),
                    miFileLoad = new MenuItem("_Open", "", miFileLoad_Click, null, null, Key.CtrlMask | Key.O),
                    miMRUFiles = new MenuItem("_Recent", "", null),
                    miFileSave = new MenuItem("_Save", "", miFileSave_Click, null, null, Key.CtrlMask | Key.S),
                    miFileSaveAs = new MenuItem("_SaveAs", "", miFileSaveAs_Click),
                    miFileClose = new MenuItem("_Close", "", miFileClose_Click),
                    null,
                    miFileProperties = new MenuItem("_Properties", "", miFileProperties_Click),
                    null,
                    miExport = new MenuBarItem("_Export", new MenuItem[] {
                        miExportToExcelFile = new MenuItem("_ExportToExcelFile", "", miExportToExcelFile_Click),
                    }),
                    null,
                    miExit = new MenuItem("_Exit", "", miExit_Click, null, null, Key.CtrlMask | Key.X)
                }),
                miEdit = new MenuBarItem("_Edit", new MenuItem[] {
                    miRecordAdd = new MenuItem("miRecordAdd", "", miRecordAdd_Click, null, null, Key.CtrlMask | Key.I),
                    miRecordEdit = new MenuItem("miRecordEdit", "", miRecordEdit_Click),
                    miRecordDelete = new MenuItem("miRecordDelete", "", miRecordDelete_Click, null, null, Key.CtrlMask | Key.L),
                    null,
                    miSearch = new MenuItem("miSearch", "", miSearch_Click),
                    miFilter = new MenuItem("miFilter", "", miFilter_Click, null, null, Key.CtrlMask | Key.F)
                }),
                miPedigree = new MenuBarItem("_Pedigree", new MenuItem[] {
                    miTreeAncestors = new MenuItem("miTreeAncestors", "", miTreeAncestors_Click, null, null, Key.CtrlMask | Key.A),
                    miTreeDescendants = new MenuItem("miTreeDescendants", "", miTreeDescendants_Click, null, null, Key.CtrlMask | Key.D),
                    miTreeBoth = new MenuItem("miTreeBoth", "", miTreeBoth_Click),
                    miAncestorsCircle = new MenuItem("miAncestorsCircle", "", miAncestorsCircle_Click),
                    miDescendantsCircle = new MenuItem("miDescendantsCircle", "", miDescendantsCircle_Click),
                    null,
                    miPedigreeAscend = new MenuItem("miPedigreeAscend", "", miPedigreeAscend_Click),
                    miPedigree_dAboville = new MenuItem("miPedigree_dAboville", "", miPedigree_dAbovilleClick, null, null, Key.CtrlMask | Key.P),
                    miPedigree_Konovalov = new MenuItem("miPedigree_Konovalov", "", miPedigree_KonovalovClick, null, null, Key.CtrlMask | Key.K),
                    miExportToFamilyBook = new MenuItem("miExportToFamilyBook", "", miExportToFamilyBook_Click),
                    miExportToTreesAlbum = new MenuItem("miExportToTreesAlbum", "", miExportToTreesAlbum_Click),
                    null,
                    miMap = new MenuItem("miMap", "", miMap_Click, null, null, Key.CtrlMask | Key.M),
                    null,
                    miStats = new MenuItem("miStats", "", miStats_Click, null, null, Key.CtrlMask | Key.T),
                    null,
                    miRelationshipCalculator = new MenuItem("miRelationshipCalculator", "", miRelationshipCalculator_Click)
                }),
                miService = new MenuBarItem("_Service", new MenuItem[] {
                    miOrganizer = new MenuItem("miOrganizer", "", miOrganizer_Click),
                    miSlideshow = new MenuItem("miSlideshow", "", miSlideshow_Click),
                    null,
                    miScripts = new MenuItem("miScripts", "", miScripts_Click, null, null, Key.F11),
                    miTreeTools = new MenuBarItem("miTreeTools", new MenuItem[] {
                        miTreeCompare = new MenuItem("miTreeCompare", "", miTTTreeCompare_Click),
                        miTreeMerge = new MenuItem("miTreeMerge", "", miTTTreeMerge_Click),
                        miTreeSplit = new MenuItem("miTreeSplit", "", miTTTreeSplit_Click),
                        miRecMerge = new MenuItem("miRecMerge", "", miTTRecMerge_Click),
                        miFamilyGroups = new MenuItem("miFamilyGroups", "", miTTFamilyGroups_Click),
                        miTreeCheck = new MenuItem("miTreeCheck", "", miTTTreeCheck_Click),
                        miPatSearch = new MenuItem("miPatSearch", "", miTTPatSearch_Click),
                        miPlacesManager = new MenuItem("miPlacesManager", "", miTTPlacesManager_Click)
                    }),
                    null,
                    miOptions = new MenuItem("miOptions", "", miOptions_Click)
                }),
                miReports = new MenuBarItem("_Reports", new MenuItem[] {
                }),
                miPlugins = new MenuBarItem("_Plugins", new MenuItem[] {
                }),
                miHelp = new MenuBarItem("_Help", new MenuItem[] {
                    miContext = new MenuItem("miContext", "", miContext_Click, null, null, Key.F1),
                    null,
                    miLogSend = new MenuItem("miLogSend", "", miLogSend_Click),
                    miLogView = new MenuItem("miLogView", "", miLogView_Click),
                    null,
                    miAbout = new MenuItem("miAbout", "", miAbout_Click)
                })
            });
            Application.Top.Add(MainMenu1);

            tabsRecords = new TabView() { Width = Dim.Fill(), Height = Dim.Fill(), Y = 0 };
            tabsRecords.SelectedTabChanged += tabsRecords_SelectedIndexChanged;
            Add(tabsRecords);

            /*var login = new Label("Login: ") { X = 3, Y = 2 };
            var password = new Label("Password: ") {
                X = Pos.Left(login),
                Y = Pos.Top(login) + 1
            };
            var loginText = new TextField("") {
                X = Pos.Right(password),
                Y = Pos.Top(login),
                Width = 40
            };
            var passText = new TextField("") {
                Secret = true,
                X = Pos.Left(loginText),
                Y = Pos.Top(password),
                Width = Dim.Width(loginText)
            };
            // Add some controls, 
            this.Add(
                // The ones with my favorite layout system, Computed
                login, password, loginText, passText,

                // The ones laid out like an australopithecus, with Absolute positions:
                new CheckBox(3, 6, "Remember me"),
                new RadioGroup(3, 8, new ustring[] { "_Personal", "_Company" }, 0),
                new Button(3, 14, "Ok"),
                new Button(10, 14, "Cancel"),
                new Label(3, 18, "Press F9 or ESC plus 9 to activate the menubar")
            );*/

            X = 0;
            Y = 1; // Leave one row for the toplevel menu
            // By using Dim.Fill(), it will automatically resize without manual intervention
            Width = Dim.Fill();
            Height = Dim.Fill();

            var thisWin = (Window)this;
            thisWin.Activate += Form_Activated;
            thisWin.Deactivate += Form_Deactivate;
            thisWin.Loaded += Form_Load;
            thisWin.Closing += Form_Closing;
            thisWin.Closed += Form_Closed;
        }
    }
}
