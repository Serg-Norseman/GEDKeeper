using System;
using GKUI.Controls;

namespace GKUI
{
	partial class MainWin
	{
		private System.ComponentModel.IContainer components;
		private System.Windows.Forms.StatusBar StatusBar;
		private System.Windows.Forms.ToolBar ToolBar1;
		private System.Windows.Forms.ToolBarButton tbFileNew;
		private System.Windows.Forms.ToolBarButton tbFileLoad;
		private System.Windows.Forms.ToolBarButton tbFileSave;
		private System.Windows.Forms.ToolBarButton TBS1;
		private System.Windows.Forms.ToolBarButton tbRecordAdd;
		private System.Windows.Forms.ToolBarButton tbRecordEdit;
		private System.Windows.Forms.ToolBarButton tbRecordDelete;
		private System.Windows.Forms.ToolBarButton TBS2;
		private System.Windows.Forms.ToolBarButton tbFilter;
		private System.Windows.Forms.ToolBarButton tbTreeAncestors;
		private System.Windows.Forms.ToolBarButton tbTreeDescendants;
		private System.Windows.Forms.ToolBarButton TBS4;
		private System.Windows.Forms.ToolBarButton tbPedigree;
		private System.Windows.Forms.ToolBarButton TBS6;
		private System.Windows.Forms.ToolBarButton tbStats;
		private System.Windows.Forms.ToolBarButton TBS5;
		private System.Windows.Forms.ToolBarButton tbPrev;
		private System.Windows.Forms.ToolBarButton tbNext;
		private System.Windows.Forms.ToolBarButton TBS8;
		private System.Windows.Forms.ToolBarButton tbDocPrint;
		private System.Windows.Forms.ToolBarButton tbDocPreview;
		private System.Windows.Forms.MainMenu MainMenu1;
		private System.Windows.Forms.MenuItem miFile;
		private System.Windows.Forms.MenuItem miFileNew;
		private System.Windows.Forms.MenuItem miFileLoad;
		private System.Windows.Forms.MenuItem miMRUFiles;
		private System.Windows.Forms.MenuItem miFileSave;
		private System.Windows.Forms.MenuItem miFileClose;
		private System.Windows.Forms.MenuItem N1;
		private System.Windows.Forms.MenuItem miFileProperties;
		private System.Windows.Forms.MenuItem N2;
		private System.Windows.Forms.MenuItem miExportToExcelFile;
		private System.Windows.Forms.MenuItem miExportToFamilyBook;
		private System.Windows.Forms.MenuItem N3;
		private System.Windows.Forms.MenuItem miTreeTools;
		private System.Windows.Forms.MenuItem miExit;
		private System.Windows.Forms.MenuItem miEdit;
		private System.Windows.Forms.MenuItem miRecordAdd;
		private System.Windows.Forms.MenuItem miRecordEdit;
		private System.Windows.Forms.MenuItem miRecordDelete;
		private System.Windows.Forms.MenuItem N15;
		private System.Windows.Forms.MenuItem miSearch;
		private System.Windows.Forms.MenuItem N6;
		private System.Windows.Forms.MenuItem miFilter;
		private System.Windows.Forms.MenuItem N7;
		private System.Windows.Forms.MenuItem miOptions;
		private System.Windows.Forms.MenuItem miPedigree;
		private System.Windows.Forms.MenuItem miTreeAncestors;
		private System.Windows.Forms.MenuItem miTreeDescendants;
		private System.Windows.Forms.MenuItem N8;
		private System.Windows.Forms.MenuItem miPedigree_dAboville;
		private System.Windows.Forms.MenuItem miPedigree_Konovalov;
		private System.Windows.Forms.MenuItem N9;
		private System.Windows.Forms.MenuItem miMap;
		private System.Windows.Forms.MenuItem N10;
		private System.Windows.Forms.MenuItem miStats;
		private System.Windows.Forms.MenuItem miWindow;
		private System.Windows.Forms.MenuItem miWinCascade;
		private System.Windows.Forms.MenuItem miWinHTile;
		private System.Windows.Forms.MenuItem miWinVTile;
		private System.Windows.Forms.MenuItem miWinMinimize;
		private System.Windows.Forms.MenuItem miWinArrange;
		private System.Windows.Forms.MenuItem miHelp;
		private System.Windows.Forms.MenuItem miContext;
		private System.Windows.Forms.MenuItem miLogSend;
		private System.Windows.Forms.MenuItem miLogView;
		private System.Windows.Forms.MenuItem N13;
		private System.Windows.Forms.MenuItem miAbout;
		private System.Windows.Forms.ContextMenu MenuMRU;
		private System.Windows.Forms.ContextMenu MenuPedigree;
		private System.Windows.Forms.MenuItem miPedigree_dAboville2;
		private System.Windows.Forms.MenuItem miPedigree_Konovalov2;
		private System.Windows.Forms.OpenFileDialog OpenDialog1;
		private System.Windows.Forms.SaveFileDialog SaveDialog1;
		private System.Windows.Forms.ToolBarButton TBS7;
		private System.Windows.Forms.ImageList ImageList_Shields;
		private System.Windows.Forms.MenuItem miOrganizer;
		private System.Windows.Forms.MenuItem miService;
		private System.Windows.Forms.MenuItem N12;
		private System.Windows.Forms.MenuItem miScripts;
		private System.Windows.Forms.MenuItem miExport;
		private System.Windows.Forms.MenuItem miTreeBoth;
		private System.Windows.Forms.MenuItem miAncestorsCircle;
		private System.Windows.Forms.ToolBarButton tbTreeBoth;
		private System.Windows.Forms.StatusBarPanel StatusBarPanel1;
		private System.Windows.Forms.StatusBarPanel StatusBarPanel2;
		private System.Windows.Forms.ToolTip ToolTip1;
		public System.Windows.Forms.ImageList ImageList_Buttons;
		private System.Windows.Forms.MenuItem miPlugins;
		private System.Windows.Forms.MenuItem miSlideshow;

		private void InitializeComponent()
		{
			this.components = new System.ComponentModel.Container();
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainWin));
			this.StatusBar = new System.Windows.Forms.StatusBar();
			this.StatusBarPanel1 = new System.Windows.Forms.StatusBarPanel();
			this.StatusBarPanel2 = new System.Windows.Forms.StatusBarPanel();
			this.ImageList_Buttons = new System.Windows.Forms.ImageList(this.components);
			this.ToolBar1 = new System.Windows.Forms.ToolBar();
			this.tbFileNew = new System.Windows.Forms.ToolBarButton();
			this.tbFileLoad = new System.Windows.Forms.ToolBarButton();
			this.MenuMRU = new System.Windows.Forms.ContextMenu();
			this.tbFileSave = new System.Windows.Forms.ToolBarButton();
			this.TBS1 = new System.Windows.Forms.ToolBarButton();
			this.tbRecordAdd = new System.Windows.Forms.ToolBarButton();
			this.tbRecordEdit = new System.Windows.Forms.ToolBarButton();
			this.tbRecordDelete = new System.Windows.Forms.ToolBarButton();
			this.TBS2 = new System.Windows.Forms.ToolBarButton();
			this.tbFilter = new System.Windows.Forms.ToolBarButton();
			this.TBS4 = new System.Windows.Forms.ToolBarButton();
			this.tbTreeAncestors = new System.Windows.Forms.ToolBarButton();
			this.tbTreeDescendants = new System.Windows.Forms.ToolBarButton();
			this.tbTreeBoth = new System.Windows.Forms.ToolBarButton();
			this.TBS5 = new System.Windows.Forms.ToolBarButton();
			this.tbPedigree = new System.Windows.Forms.ToolBarButton();
			this.MenuPedigree = new System.Windows.Forms.ContextMenu();
			this.miPedigree_dAboville2 = new System.Windows.Forms.MenuItem();
			this.miPedigree_Konovalov2 = new System.Windows.Forms.MenuItem();
			this.TBS6 = new System.Windows.Forms.ToolBarButton();
			this.tbStats = new System.Windows.Forms.ToolBarButton();
			this.TBS7 = new System.Windows.Forms.ToolBarButton();
			this.tbPrev = new System.Windows.Forms.ToolBarButton();
			this.tbNext = new System.Windows.Forms.ToolBarButton();
			this.TBS8 = new System.Windows.Forms.ToolBarButton();
			this.tbDocPreview = new System.Windows.Forms.ToolBarButton();
			this.tbDocPrint = new System.Windows.Forms.ToolBarButton();
			this.MainMenu1 = new System.Windows.Forms.MainMenu(this.components);
			this.miFile = new System.Windows.Forms.MenuItem();
			this.miFileNew = new System.Windows.Forms.MenuItem();
			this.miFileLoad = new System.Windows.Forms.MenuItem();
			this.miMRUFiles = new System.Windows.Forms.MenuItem();
			this.miFileSave = new System.Windows.Forms.MenuItem();
			this.miFileClose = new System.Windows.Forms.MenuItem();
			this.N1 = new System.Windows.Forms.MenuItem();
			this.miFileProperties = new System.Windows.Forms.MenuItem();
			this.N2 = new System.Windows.Forms.MenuItem();
			this.miExport = new System.Windows.Forms.MenuItem();
			this.miExportToExcelFile = new System.Windows.Forms.MenuItem();
			this.N3 = new System.Windows.Forms.MenuItem();
			this.miExit = new System.Windows.Forms.MenuItem();
			this.miEdit = new System.Windows.Forms.MenuItem();
			this.miRecordAdd = new System.Windows.Forms.MenuItem();
			this.miRecordEdit = new System.Windows.Forms.MenuItem();
			this.miRecordDelete = new System.Windows.Forms.MenuItem();
			this.N15 = new System.Windows.Forms.MenuItem();
			this.miSearch = new System.Windows.Forms.MenuItem();
			this.miFilter = new System.Windows.Forms.MenuItem();
			this.miPedigree = new System.Windows.Forms.MenuItem();
			this.miTreeAncestors = new System.Windows.Forms.MenuItem();
			this.miTreeDescendants = new System.Windows.Forms.MenuItem();
			this.miTreeBoth = new System.Windows.Forms.MenuItem();
			this.miAncestorsCircle = new System.Windows.Forms.MenuItem();
			this.N6 = new System.Windows.Forms.MenuItem();
			this.miPedigree_dAboville = new System.Windows.Forms.MenuItem();
			this.miPedigree_Konovalov = new System.Windows.Forms.MenuItem();
			this.miExportToFamilyBook = new System.Windows.Forms.MenuItem();
			this.N7 = new System.Windows.Forms.MenuItem();
			this.miMap = new System.Windows.Forms.MenuItem();
			this.N8 = new System.Windows.Forms.MenuItem();
			this.miStats = new System.Windows.Forms.MenuItem();
			this.miService = new System.Windows.Forms.MenuItem();
			this.miOrganizer = new System.Windows.Forms.MenuItem();
			this.miSlideshow = new System.Windows.Forms.MenuItem();
			this.N9 = new System.Windows.Forms.MenuItem();
			this.miScripts = new System.Windows.Forms.MenuItem();
			this.miTreeTools = new System.Windows.Forms.MenuItem();
			this.N10 = new System.Windows.Forms.MenuItem();
			this.miOptions = new System.Windows.Forms.MenuItem();
			this.miPlugins = new System.Windows.Forms.MenuItem();
			this.miWindow = new System.Windows.Forms.MenuItem();
			this.miWinCascade = new System.Windows.Forms.MenuItem();
			this.miWinHTile = new System.Windows.Forms.MenuItem();
			this.miWinVTile = new System.Windows.Forms.MenuItem();
			this.miWinMinimize = new System.Windows.Forms.MenuItem();
			this.miWinArrange = new System.Windows.Forms.MenuItem();
			this.miHelp = new System.Windows.Forms.MenuItem();
			this.miContext = new System.Windows.Forms.MenuItem();
			this.N12 = new System.Windows.Forms.MenuItem();
			this.miLogSend = new System.Windows.Forms.MenuItem();
			this.miLogView = new System.Windows.Forms.MenuItem();
			this.N13 = new System.Windows.Forms.MenuItem();
			this.miAbout = new System.Windows.Forms.MenuItem();
			this.OpenDialog1 = new System.Windows.Forms.OpenFileDialog();
			this.SaveDialog1 = new System.Windows.Forms.SaveFileDialog();
			this.ImageList_Shields = new System.Windows.Forms.ImageList(this.components);
			this.ToolTip1 = new System.Windows.Forms.ToolTip(this.components);
			((System.ComponentModel.ISupportInitialize)(this.StatusBarPanel1)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.StatusBarPanel2)).BeginInit();
			this.SuspendLayout();
			// 
			// StatusBar
			// 
			this.StatusBar.ImeMode = System.Windows.Forms.ImeMode.NoControl;
			this.StatusBar.Location = new System.Drawing.Point(0, 921);
			this.StatusBar.Name = "StatusBar";
			this.StatusBar.Panels.AddRange(new System.Windows.Forms.StatusBarPanel[] {
									this.StatusBarPanel1,
									this.StatusBarPanel2});
			this.StatusBar.ShowPanels = true;
			this.StatusBar.Size = new System.Drawing.Size(896, 24);
			this.StatusBar.TabIndex = 0;
			this.StatusBar.DrawItem += new System.Windows.Forms.StatusBarDrawItemEventHandler(this.StatusBar_DrawItem);
			this.StatusBar.PanelClick += new System.Windows.Forms.StatusBarPanelClickEventHandler(this.StatusBar_PanelClick);
			// 
			// StatusBarPanel1
			// 
			this.StatusBarPanel1.Name = "StatusBarPanel1";
			this.StatusBarPanel1.Width = 50;
			// 
			// StatusBarPanel2
			// 
			this.StatusBarPanel2.Name = "StatusBarPanel2";
			this.StatusBarPanel2.Style = System.Windows.Forms.StatusBarPanelStyle.OwnerDraw;
			this.StatusBarPanel2.Width = 24;
			// 
			// ImageList_Buttons
			// 
			this.ImageList_Buttons.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("ImageList_Buttons.ImageStream")));
			this.ImageList_Buttons.TransparentColor = System.Drawing.Color.Transparent;
			this.ImageList_Buttons.Images.SetKeyName(0, "");
			this.ImageList_Buttons.Images.SetKeyName(1, "");
			this.ImageList_Buttons.Images.SetKeyName(2, "");
			this.ImageList_Buttons.Images.SetKeyName(3, "");
			this.ImageList_Buttons.Images.SetKeyName(4, "");
			this.ImageList_Buttons.Images.SetKeyName(5, "");
			this.ImageList_Buttons.Images.SetKeyName(6, "");
			this.ImageList_Buttons.Images.SetKeyName(7, "");
			this.ImageList_Buttons.Images.SetKeyName(8, "");
			this.ImageList_Buttons.Images.SetKeyName(9, "");
			this.ImageList_Buttons.Images.SetKeyName(10, "");
			this.ImageList_Buttons.Images.SetKeyName(11, "");
			this.ImageList_Buttons.Images.SetKeyName(12, "");
			this.ImageList_Buttons.Images.SetKeyName(13, "");
			this.ImageList_Buttons.Images.SetKeyName(14, "");
			this.ImageList_Buttons.Images.SetKeyName(15, "");
			this.ImageList_Buttons.Images.SetKeyName(16, "");
			this.ImageList_Buttons.Images.SetKeyName(17, "");
			this.ImageList_Buttons.Images.SetKeyName(18, "");
			this.ImageList_Buttons.Images.SetKeyName(19, "");
			this.ImageList_Buttons.Images.SetKeyName(20, "");
			this.ImageList_Buttons.Images.SetKeyName(21, "");
			this.ImageList_Buttons.Images.SetKeyName(22, "");
			this.ImageList_Buttons.Images.SetKeyName(23, "");
			this.ImageList_Buttons.Images.SetKeyName(24, "");
			this.ImageList_Buttons.Images.SetKeyName(25, "");
			this.ImageList_Buttons.Images.SetKeyName(26, "");
			this.ImageList_Buttons.Images.SetKeyName(27, "");
			this.ImageList_Buttons.Images.SetKeyName(28, "");
			this.ImageList_Buttons.Images.SetKeyName(29, "");
			this.ImageList_Buttons.Images.SetKeyName(30, "");
			this.ImageList_Buttons.Images.SetKeyName(31, "");
			this.ImageList_Buttons.Images.SetKeyName(32, "");
			this.ImageList_Buttons.Images.SetKeyName(33, "");
			this.ImageList_Buttons.Images.SetKeyName(34, "");
			this.ImageList_Buttons.Images.SetKeyName(35, "");
			this.ImageList_Buttons.Images.SetKeyName(36, "iFilter");
			this.ImageList_Buttons.Images.SetKeyName(37, "iPreview");
			this.ImageList_Buttons.Images.SetKeyName(38, "iPrint");
			// 
			// ToolBar1
			// 
			this.ToolBar1.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
			this.ToolBar1.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
									this.tbFileNew,
									this.tbFileLoad,
									this.tbFileSave,
									this.TBS1,
									this.tbRecordAdd,
									this.tbRecordEdit,
									this.tbRecordDelete,
									this.TBS2,
									this.tbFilter,
									this.TBS4,
									this.tbTreeAncestors,
									this.tbTreeDescendants,
									this.tbTreeBoth,
									this.TBS5,
									this.tbPedigree,
									this.TBS6,
									this.tbStats,
									this.TBS7,
									this.tbPrev,
									this.tbNext,
									this.TBS8,
									this.tbDocPreview,
									this.tbDocPrint});
			this.ToolBar1.ButtonSize = new System.Drawing.Size(27, 26);
			this.ToolBar1.DropDownArrows = true;
			this.ToolBar1.ImageList = this.ImageList_Buttons;
			this.ToolBar1.ImeMode = System.Windows.Forms.ImeMode.NoControl;
			this.ToolBar1.Location = new System.Drawing.Point(0, 0);
			this.ToolBar1.Name = "ToolBar1";
			this.ToolBar1.ShowToolTips = true;
			this.ToolBar1.Size = new System.Drawing.Size(896, 40);
			this.ToolBar1.TabIndex = 0;
			this.ToolBar1.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.ToolBar1_ButtonClick);
			// 
			// tbFileNew
			// 
			this.tbFileNew.ImageIndex = 0;
			this.tbFileNew.Name = "tbFileNew";
			this.tbFileNew.ToolTipText = "Создать новый файл древа";
			// 
			// tbFileLoad
			// 
			this.tbFileLoad.DropDownMenu = this.MenuMRU;
			this.tbFileLoad.ImageIndex = 1;
			this.tbFileLoad.Name = "tbFileLoad";
			this.tbFileLoad.Style = System.Windows.Forms.ToolBarButtonStyle.DropDownButton;
			this.tbFileLoad.ToolTipText = "Открыть файл древа";
			// 
			// tbFileSave
			// 
			this.tbFileSave.ImageIndex = 2;
			this.tbFileSave.Name = "tbFileSave";
			this.tbFileSave.ToolTipText = "Сохранить файл древа";
			// 
			// TBS1
			// 
			this.TBS1.Name = "TBS1";
			this.TBS1.Style = System.Windows.Forms.ToolBarButtonStyle.Separator;
			// 
			// tbRecordAdd
			// 
			this.tbRecordAdd.ImageIndex = 3;
			this.tbRecordAdd.Name = "tbRecordAdd";
			this.tbRecordAdd.ToolTipText = "Добавить запись (Ctrl+I)";
			// 
			// tbRecordEdit
			// 
			this.tbRecordEdit.ImageIndex = 4;
			this.tbRecordEdit.Name = "tbRecordEdit";
			this.tbRecordEdit.ToolTipText = "Изменить запись (Ctrl+Enter)";
			// 
			// tbRecordDelete
			// 
			this.tbRecordDelete.ImageIndex = 5;
			this.tbRecordDelete.Name = "tbRecordDelete";
			this.tbRecordDelete.ToolTipText = "Удалить запись (Ctrl+L)";
			// 
			// TBS2
			// 
			this.TBS2.Name = "TBS2";
			this.TBS2.Style = System.Windows.Forms.ToolBarButtonStyle.Separator;
			// 
			// tbFilter
			// 
			this.tbFilter.ImageKey = "iFilter";
			this.tbFilter.Name = "tbFilter";
			this.tbFilter.ToolTipText = "Фильтрация списка персон";
			// 
			// TBS4
			// 
			this.TBS4.Name = "TBS4";
			this.TBS4.Style = System.Windows.Forms.ToolBarButtonStyle.Separator;
			// 
			// tbTreeAncestors
			// 
			this.tbTreeAncestors.ImageIndex = 18;
			this.tbTreeAncestors.Name = "tbTreeAncestors";
			this.tbTreeAncestors.ToolTipText = "Сформировать древо предков";
			// 
			// tbTreeDescendants
			// 
			this.tbTreeDescendants.ImageIndex = 19;
			this.tbTreeDescendants.Name = "tbTreeDescendants";
			this.tbTreeDescendants.ToolTipText = "Сформировать древо потомков";
			// 
			// tbTreeBoth
			// 
			this.tbTreeBoth.ImageIndex = 34;
			this.tbTreeBoth.Name = "tbTreeBoth";
			this.tbTreeBoth.ToolTipText = "Сформировать полное древо";
			// 
			// TBS5
			// 
			this.TBS5.Name = "TBS5";
			this.TBS5.Style = System.Windows.Forms.ToolBarButtonStyle.Separator;
			// 
			// tbPedigree
			// 
			this.tbPedigree.DropDownMenu = this.MenuPedigree;
			this.tbPedigree.ImageIndex = 20;
			this.tbPedigree.Name = "tbPedigree";
			this.tbPedigree.Style = System.Windows.Forms.ToolBarButtonStyle.DropDownButton;
			this.tbPedigree.ToolTipText = "Родословная роспись";
			// 
			// MenuPedigree
			// 
			this.MenuPedigree.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
									this.miPedigree_dAboville2,
									this.miPedigree_Konovalov2});
			// 
			// miPedigree_dAboville2
			// 
			this.miPedigree_dAboville2.Index = 0;
			this.miPedigree_dAboville2.Text = "Роспись по д\'Абовиллю";
			this.miPedigree_dAboville2.Click += new System.EventHandler(this.miPedigree_dAbovilleClick);
			// 
			// miPedigree_Konovalov2
			// 
			this.miPedigree_Konovalov2.Index = 1;
			this.miPedigree_Konovalov2.Text = "Роспись по Коновалову";
			this.miPedigree_Konovalov2.Click += new System.EventHandler(this.miPedigree_KonovalovClick);
			// 
			// TBS6
			// 
			this.TBS6.Name = "TBS6";
			this.TBS6.Style = System.Windows.Forms.ToolBarButtonStyle.Separator;
			// 
			// tbStats
			// 
			this.tbStats.ImageIndex = 11;
			this.tbStats.Name = "tbStats";
			this.tbStats.ToolTipText = "Статистический анализ данных";
			// 
			// TBS7
			// 
			this.TBS7.Name = "TBS7";
			this.TBS7.Style = System.Windows.Forms.ToolBarButtonStyle.Separator;
			// 
			// tbPrev
			// 
			this.tbPrev.Enabled = false;
			this.tbPrev.ImageIndex = 22;
			this.tbPrev.Name = "tbPrev";
			this.tbPrev.ToolTipText = "Предыдущая запись";
			// 
			// tbNext
			// 
			this.tbNext.Enabled = false;
			this.tbNext.ImageIndex = 23;
			this.tbNext.Name = "tbNext";
			this.tbNext.ToolTipText = "Следующая запись";
			// 
			// TBS8
			// 
			this.TBS8.Name = "TBS8";
			this.TBS8.Style = System.Windows.Forms.ToolBarButtonStyle.Separator;
			// 
			// tbDocPreview
			// 
			this.tbDocPreview.ImageKey = "iPreview";
			this.tbDocPreview.Name = "tbDocPreview";
			// 
			// tbDocPrint
			// 
			this.tbDocPrint.ImageKey = "iPrint";
			this.tbDocPrint.Name = "tbDocPrint";
			// 
			// MainMenu1
			// 
			this.MainMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
									this.miFile,
									this.miEdit,
									this.miPedigree,
									this.miService,
									this.miPlugins,
									this.miWindow,
									this.miHelp});
			this.MainMenu1.Tag = "";
			// 
			// miFile
			// 
			this.miFile.Index = 0;
			this.miFile.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
									this.miFileNew,
									this.miFileLoad,
									this.miMRUFiles,
									this.miFileSave,
									this.miFileClose,
									this.N1,
									this.miFileProperties,
									this.N2,
									this.miExport,
									this.N3,
									this.miExit});
			this.miFile.Text = "Файл";
			// 
			// miFileNew
			// 
			this.miFileNew.Index = 0;
			this.miFileNew.Shortcut = System.Windows.Forms.Shortcut.CtrlN;
			this.miFileNew.Text = "Новый";
			this.miFileNew.Click += new System.EventHandler(this.miFileNewClick);
			// 
			// miFileLoad
			// 
			this.miFileLoad.Index = 1;
			this.miFileLoad.Shortcut = System.Windows.Forms.Shortcut.CtrlO;
			this.miFileLoad.Text = "Открыть...";
			this.miFileLoad.Click += new System.EventHandler(this.miFileLoadClick);
			// 
			// miMRUFiles
			// 
			this.miMRUFiles.Enabled = false;
			this.miMRUFiles.Index = 2;
			this.miMRUFiles.Text = "Открыть последний";
			// 
			// miFileSave
			// 
			this.miFileSave.Index = 3;
			this.miFileSave.Shortcut = System.Windows.Forms.Shortcut.CtrlS;
			this.miFileSave.Text = "Сохранить...";
			this.miFileSave.Click += new System.EventHandler(this.miFileSaveClick);
			// 
			// miFileClose
			// 
			this.miFileClose.Index = 4;
			this.miFileClose.Text = "Закрыть";
			this.miFileClose.Click += new System.EventHandler(this.miFileCloseClick);
			// 
			// N1
			// 
			this.N1.Index = 5;
			this.N1.Text = "-";
			// 
			// miFileProperties
			// 
			this.miFileProperties.Index = 6;
			this.miFileProperties.Text = "Свойства файла...";
			this.miFileProperties.Click += new System.EventHandler(this.miFilePropertiesClick);
			// 
			// N2
			// 
			this.N2.Index = 7;
			this.N2.Text = "-";
			// 
			// miExport
			// 
			this.miExport.Index = 8;
			this.miExport.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
									this.miExportToExcelFile});
			this.miExport.Text = "Экспорт";
			// 
			// miExportToExcelFile
			// 
			this.miExportToExcelFile.Index = 0;
			this.miExportToExcelFile.Text = "Экспорт в Excel-файл...";
			this.miExportToExcelFile.Click += new System.EventHandler(this.miExportToExcelFileClick);
			// 
			// N3
			// 
			this.N3.Index = 9;
			this.N3.Text = "-";
			// 
			// miExit
			// 
			this.miExit.Index = 10;
			this.miExit.Shortcut = System.Windows.Forms.Shortcut.CtrlX;
			this.miExit.Text = "Выход";
			this.miExit.Click += new System.EventHandler(this.miExit_Click);
			// 
			// miEdit
			// 
			this.miEdit.Index = 1;
			this.miEdit.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
									this.miRecordAdd,
									this.miRecordEdit,
									this.miRecordDelete,
									this.N15,
									this.miSearch,
									this.miFilter});
			this.miEdit.Text = "Правка";
			// 
			// miRecordAdd
			// 
			this.miRecordAdd.Index = 0;
			this.miRecordAdd.Shortcut = System.Windows.Forms.Shortcut.CtrlI;
			this.miRecordAdd.Text = "Добавить запись";
			this.miRecordAdd.Click += new System.EventHandler(this.miRecordAddClick);
			// 
			// miRecordEdit
			// 
			this.miRecordEdit.Index = 1;
			this.miRecordEdit.Text = "Изменить запись";
			this.miRecordEdit.Click += new System.EventHandler(this.miRecordEditClick);
			// 
			// miRecordDelete
			// 
			this.miRecordDelete.Index = 2;
			this.miRecordDelete.Shortcut = System.Windows.Forms.Shortcut.CtrlL;
			this.miRecordDelete.Text = "Удалить запись";
			this.miRecordDelete.Click += new System.EventHandler(this.miRecordDeleteClick);
			// 
			// N15
			// 
			this.N15.Index = 3;
			this.N15.Text = "-";
			// 
			// miSearch
			// 
			this.miSearch.Index = 4;
			this.miSearch.Text = "miSearch";
			this.miSearch.Click += new System.EventHandler(this.miSearchClick);
			// 
			// miFilter
			// 
			this.miFilter.Index = 5;
			this.miFilter.Shortcut = System.Windows.Forms.Shortcut.CtrlF;
			this.miFilter.Text = "Фильтр...";
			this.miFilter.Click += new System.EventHandler(this.miFilterClick);
			// 
			// miPedigree
			// 
			this.miPedigree.Index = 2;
			this.miPedigree.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
									this.miTreeAncestors,
									this.miTreeDescendants,
									this.miTreeBoth,
									this.miAncestorsCircle,
									this.N6,
									this.miPedigree_dAboville,
									this.miPedigree_Konovalov,
									this.miExportToFamilyBook,
									this.N7,
									this.miMap,
									this.N8,
									this.miStats});
			this.miPedigree.Text = "Родословная";
			// 
			// miTreeAncestors
			// 
			this.miTreeAncestors.Index = 0;
			this.miTreeAncestors.Shortcut = System.Windows.Forms.Shortcut.CtrlA;
			this.miTreeAncestors.Text = "Древо предков";
			this.miTreeAncestors.Click += new System.EventHandler(this.miTreeAncestorsClick);
			// 
			// miTreeDescendants
			// 
			this.miTreeDescendants.Index = 1;
			this.miTreeDescendants.Shortcut = System.Windows.Forms.Shortcut.CtrlD;
			this.miTreeDescendants.Text = "Древо потомков";
			this.miTreeDescendants.Click += new System.EventHandler(this.miTreeDescendantsClick);
			// 
			// miTreeBoth
			// 
			this.miTreeBoth.Index = 2;
			this.miTreeBoth.Text = "Древо полное";
			this.miTreeBoth.Click += new System.EventHandler(this.miTreeBothClick);
			// 
			// miAncestorsCircle
			// 
			this.miAncestorsCircle.Index = 3;
			this.miAncestorsCircle.Text = "Круг предков";
			this.miAncestorsCircle.Click += new System.EventHandler(this.miAncestorsCircleClick);
			// 
			// N6
			// 
			this.N6.Index = 4;
			this.N6.Text = "-";
			// 
			// miPedigree_dAboville
			// 
			this.miPedigree_dAboville.Index = 5;
			this.miPedigree_dAboville.Shortcut = System.Windows.Forms.Shortcut.CtrlP;
			this.miPedigree_dAboville.Text = "Роспись по д\'Абовиллю";
			this.miPedigree_dAboville.Click += new System.EventHandler(this.miPedigree_dAbovilleClick);
			// 
			// miPedigree_Konovalov
			// 
			this.miPedigree_Konovalov.Index = 6;
			this.miPedigree_Konovalov.Shortcut = System.Windows.Forms.Shortcut.CtrlK;
			this.miPedigree_Konovalov.Text = "Роспись по Коновалову";
			this.miPedigree_Konovalov.Click += new System.EventHandler(this.miPedigree_KonovalovClick);
			// 
			// miExportToFamilyBook
			// 
			this.miExportToFamilyBook.Index = 7;
			this.miExportToFamilyBook.Text = "miExportToFamilyBook";
			this.miExportToFamilyBook.Click += new System.EventHandler(this.miExportToFamilyBookClick);
			// 
			// N7
			// 
			this.N7.Index = 8;
			this.N7.Text = "-";
			// 
			// miMap
			// 
			this.miMap.Index = 9;
			this.miMap.Shortcut = System.Windows.Forms.Shortcut.CtrlM;
			this.miMap.Text = "Карты...";
			this.miMap.Click += new System.EventHandler(this.miMapClick);
			// 
			// N8
			// 
			this.N8.Index = 10;
			this.N8.Text = "-";
			// 
			// miStats
			// 
			this.miStats.Index = 11;
			this.miStats.Shortcut = System.Windows.Forms.Shortcut.CtrlT;
			this.miStats.Text = "Статистика...";
			this.miStats.Click += new System.EventHandler(this.miStatsClick);
			// 
			// miService
			// 
			this.miService.Index = 3;
			this.miService.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
									this.miOrganizer,
									this.miSlideshow,
									this.N9,
									this.miScripts,
									this.miTreeTools,
									this.N10,
									this.miOptions});
			this.miService.Text = "Сервис";
			// 
			// miOrganizer
			// 
			this.miOrganizer.Index = 0;
			this.miOrganizer.Text = "Органайзер...";
			this.miOrganizer.Click += new System.EventHandler(this.miOrganizerClick);
			// 
			// miSlideshow
			// 
			this.miSlideshow.Index = 1;
			this.miSlideshow.Text = "Slideshow";
			this.miSlideshow.Click += new System.EventHandler(this.miSlideshowClick);
			// 
			// N9
			// 
			this.N9.Index = 2;
			this.N9.Text = "-";
			// 
			// miScripts
			// 
			this.miScripts.Index = 3;
			this.miScripts.Shortcut = System.Windows.Forms.Shortcut.CtrlF11;
			this.miScripts.Text = "Скрипты...";
			this.miScripts.Click += new System.EventHandler(this.miScriptsClick);
			// 
			// miTreeTools
			// 
			this.miTreeTools.Index = 4;
			this.miTreeTools.Text = "Инструменты...";
			this.miTreeTools.Click += new System.EventHandler(this.miTreeToolsClick);
			// 
			// N10
			// 
			this.N10.Index = 5;
			this.N10.Text = "-";
			// 
			// miOptions
			// 
			this.miOptions.Index = 6;
			this.miOptions.Text = "Настройки...";
			this.miOptions.Click += new System.EventHandler(this.miOptionsClick);
			// 
			// miPlugins
			// 
			this.miPlugins.Index = 4;
			this.miPlugins.Text = "Plugins";
			// 
			// miWindow
			// 
			this.miWindow.Index = 5;
			this.miWindow.MdiList = true;
			this.miWindow.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
									this.miWinCascade,
									this.miWinHTile,
									this.miWinVTile,
									this.miWinMinimize,
									this.miWinArrange});
			this.miWindow.Text = "&Окна";
			// 
			// miWinCascade
			// 
			this.miWinCascade.Index = 0;
			this.miWinCascade.Text = "&Каскад";
			this.miWinCascade.Click += new System.EventHandler(this.miWinCascadeClick);
			// 
			// miWinHTile
			// 
			this.miWinHTile.Index = 1;
			this.miWinHTile.Text = "&Горизонтальная мозаика";
			this.miWinHTile.Click += new System.EventHandler(this.miWinHTileClick);
			// 
			// miWinVTile
			// 
			this.miWinVTile.Index = 2;
			this.miWinVTile.Text = "&Вертикальная мозаика";
			this.miWinVTile.Click += new System.EventHandler(this.miWinVTileClick);
			// 
			// miWinMinimize
			// 
			this.miWinMinimize.Index = 3;
			this.miWinMinimize.Text = "&Свернуть все";
			this.miWinMinimize.Click += new System.EventHandler(this.miWinMinimizeClick);
			// 
			// miWinArrange
			// 
			this.miWinArrange.Index = 4;
			this.miWinArrange.Text = "&Разместить все";
			this.miWinArrange.Click += new System.EventHandler(this.miWinArrangeClick);
			// 
			// miHelp
			// 
			this.miHelp.Index = 6;
			this.miHelp.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
									this.miContext,
									this.N12,
									this.miLogSend,
									this.miLogView,
									this.N13,
									this.miAbout});
			this.miHelp.Text = "Справка";
			// 
			// miContext
			// 
			this.miContext.Index = 0;
			this.miContext.Shortcut = System.Windows.Forms.Shortcut.F1;
			this.miContext.Text = "Содержание";
			this.miContext.Click += new System.EventHandler(this.miContextClick);
			// 
			// N12
			// 
			this.N12.Index = 1;
			this.N12.Text = "-";
			// 
			// miLogSend
			// 
			this.miLogSend.Index = 2;
			this.miLogSend.Text = "Отправить журнал ошибок";
			this.miLogSend.Click += new System.EventHandler(this.miLogSendClick);
			// 
			// miLogView
			// 
			this.miLogView.Index = 3;
			this.miLogView.Text = "Просмотреть журнал ошибок";
			this.miLogView.Click += new System.EventHandler(this.miLogViewClick);
			// 
			// N13
			// 
			this.N13.Index = 4;
			this.N13.Text = "-";
			// 
			// miAbout
			// 
			this.miAbout.Index = 5;
			this.miAbout.Text = "О программе...";
			this.miAbout.Click += new System.EventHandler(this.miAboutClick);
			// 
			// OpenDialog1
			// 
			this.OpenDialog1.Filter = "GEDCOM файлы (*.ged)|*.ged|GEDKeeper шифрованные GEDCOM файлы (*.geds)|*.geds|Все" +
			" файлы (*.*)|*.*";
			// 
			// SaveDialog1
			// 
			this.SaveDialog1.DefaultExt = "ged";
			this.SaveDialog1.Filter = "GEDKeeper GEDCOM файлы (*.ged)|*.ged|GEDKeeper шифрованные GEDCOM файлы (*.geds)|" +
			"*.geds";
			this.SaveDialog1.OverwritePrompt = false;
			// 
			// ImageList_Shields
			// 
			this.ImageList_Shields.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("ImageList_Shields.ImageStream")));
			this.ImageList_Shields.TransparentColor = System.Drawing.Color.Transparent;
			this.ImageList_Shields.Images.SetKeyName(0, "shield_max.bmp");
			this.ImageList_Shields.Images.SetKeyName(1, "shield_mid.bmp");
			this.ImageList_Shields.Images.SetKeyName(2, "shield_none.bmp");
			// 
			// TfmGEDKeeper
			// 
			this.AllowDrop = true;
			this.AutoScaleBaseSize = new System.Drawing.Size(7, 17);
			this.ClientSize = new System.Drawing.Size(896, 945);
			this.Controls.Add(this.StatusBar);
			this.Controls.Add(this.ToolBar1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F);
			this.IsMdiContainer = true;
			this.KeyPreview = true;
			this.Location = new System.Drawing.Point(337, 111);
			this.Menu = this.MainMenu1;
			this.Name = "TfmGEDKeeper";
			this.Text = "GEDKeeper2";
			this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.Form_Closing);
			this.FormClosed += new System.Windows.Forms.FormClosedEventHandler(this.Form_Closed);
			this.Load += new System.EventHandler(this.Form_Load);
			this.VisibleChanged += new System.EventHandler(this.Form_Show);
			this.DragDrop += new System.Windows.Forms.DragEventHandler(this.Form_DragDrop);
			this.DragEnter += new System.Windows.Forms.DragEventHandler(this.Form_DragEnter);
			this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Form_KeyDown);
			this.Resize += new System.EventHandler(this.Form_Resize);
			((System.ComponentModel.ISupportInitialize)(this.StatusBarPanel1)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.StatusBarPanel2)).EndInit();
			this.ResumeLayout(false);
			this.PerformLayout();
		}
	}
}