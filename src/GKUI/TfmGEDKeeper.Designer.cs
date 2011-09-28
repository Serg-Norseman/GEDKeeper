using System;
using GKUI.Controls;

namespace GKUI
{
	partial class TfmGEDKeeper
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
		private System.Windows.Forms.ToolBarButton TBS3;
		private System.Windows.Forms.ToolBarButton tbTreeAncestors;
		private System.Windows.Forms.ToolBarButton tbTreeDescendants;
		private System.Windows.Forms.ToolBarButton TBS4;
		private System.Windows.Forms.ToolBarButton tbPedigree;
		private System.Windows.Forms.ToolBarButton TBS6;
		private System.Windows.Forms.ToolBarButton tbStats;
		private System.Windows.Forms.ToolBarButton TBS5;
		private System.Windows.Forms.ToolBarButton tbPrev;
		private System.Windows.Forms.ToolBarButton tbNext;
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
		private System.Windows.Forms.MenuItem miExportToWeb;
		private System.Windows.Forms.MenuItem miExportToExcelFile;
		private System.Windows.Forms.MenuItem N3;
		private System.Windows.Forms.MenuItem miTreeTools;
		private System.Windows.Forms.MenuItem N4;
		private System.Windows.Forms.MenuItem miExit;
		private System.Windows.Forms.MenuItem miEdit;
		private System.Windows.Forms.MenuItem miRecordAdd;
		private System.Windows.Forms.MenuItem miRecordEdit;
		private System.Windows.Forms.MenuItem miRecordDelete;
		private System.Windows.Forms.MenuItem N5;
		private System.Windows.Forms.MenuItem miStreamInput;
		private System.Windows.Forms.MenuItem N6;
		private System.Windows.Forms.MenuItem miFilter;
		private System.Windows.Forms.MenuItem miSearch;
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
		private System.Windows.Forms.MenuItem miGenResources;
		private System.Windows.Forms.MenuItem miKinshipTerms;
		private System.Windows.Forms.MenuItem miContext;
		private System.Windows.Forms.MenuItem N11;
		private System.Windows.Forms.MenuItem miAbout;
		private System.Windows.Forms.ContextMenu MenuMRU;
		private System.Windows.Forms.ContextMenu MenuPedigree;
		private System.Windows.Forms.MenuItem miPedigree_dAboville2;
		private System.Windows.Forms.MenuItem miPedigree_Konovalov2;
		private System.Windows.Forms.OpenFileDialog OpenDialog1;
		private System.Windows.Forms.SaveFileDialog SaveDialog1;
		private System.Windows.Forms.ToolBarButton TBS7;
		private System.Windows.Forms.ToolBarButton tbUndo;
		private System.Windows.Forms.ToolBarButton tbRedo;
		private System.Windows.Forms.MenuItem miFAQ;
		private System.Windows.Forms.ImageList ImageList_Shields;
		private System.Windows.Forms.MenuItem miOrganizer;
		private System.Windows.Forms.MenuItem miService;
		private System.Windows.Forms.MenuItem N12;
		private System.Windows.Forms.MenuItem miUndo;
		private System.Windows.Forms.MenuItem miRedo;
		private System.Windows.Forms.MenuItem miScripts;
		private System.Windows.Forms.MenuItem miExport;
		private System.Windows.Forms.MenuItem miExportToExcelApp;
		private System.Windows.Forms.MenuItem miTreeBoth;
		private System.Windows.Forms.ToolBarButton tbTreeBoth;
		private System.Windows.Forms.StatusBarPanel StatusBarPanel1;
		private System.Windows.Forms.StatusBarPanel StatusBarPanel2;
		private System.Windows.Forms.ToolTip ToolTip1;
		public System.Windows.Forms.ImageList ImageList_Buttons;
		public System.Windows.Forms.MenuItem miCalc;
		public System.Windows.Forms.MenuItem miNamesBook;
		public System.Windows.Forms.MenuItem miCalendar;
		public System.Windows.Forms.MenuItem miTimeLine;

		private void InitializeComponent()
		{
			this.components = new System.ComponentModel.Container();
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(TfmGEDKeeper));
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
			this.tbUndo = new System.Windows.Forms.ToolBarButton();
			this.tbRedo = new System.Windows.Forms.ToolBarButton();
			this.TBS3 = new System.Windows.Forms.ToolBarButton();
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
			this.miExportToWeb = new System.Windows.Forms.MenuItem();
			this.miExportToExcelApp = new System.Windows.Forms.MenuItem();
			this.miExportToExcelFile = new System.Windows.Forms.MenuItem();
			this.N3 = new System.Windows.Forms.MenuItem();
			this.miExit = new System.Windows.Forms.MenuItem();
			this.miEdit = new System.Windows.Forms.MenuItem();
			this.miUndo = new System.Windows.Forms.MenuItem();
			this.miRedo = new System.Windows.Forms.MenuItem();
			this.N4 = new System.Windows.Forms.MenuItem();
			this.miRecordAdd = new System.Windows.Forms.MenuItem();
			this.miRecordEdit = new System.Windows.Forms.MenuItem();
			this.miRecordDelete = new System.Windows.Forms.MenuItem();
			this.N5 = new System.Windows.Forms.MenuItem();
			this.miStreamInput = new System.Windows.Forms.MenuItem();
			this.miPedigree = new System.Windows.Forms.MenuItem();
			this.miTreeAncestors = new System.Windows.Forms.MenuItem();
			this.miTreeDescendants = new System.Windows.Forms.MenuItem();
			this.miTreeBoth = new System.Windows.Forms.MenuItem();
			this.N6 = new System.Windows.Forms.MenuItem();
			this.miPedigree_dAboville = new System.Windows.Forms.MenuItem();
			this.miPedigree_Konovalov = new System.Windows.Forms.MenuItem();
			this.N7 = new System.Windows.Forms.MenuItem();
			this.miMap = new System.Windows.Forms.MenuItem();
			this.N8 = new System.Windows.Forms.MenuItem();
			this.miStats = new System.Windows.Forms.MenuItem();
			this.miService = new System.Windows.Forms.MenuItem();
			this.miCalc = new System.Windows.Forms.MenuItem();
			this.miNamesBook = new System.Windows.Forms.MenuItem();
			this.miCalendar = new System.Windows.Forms.MenuItem();
			this.miTimeLine = new System.Windows.Forms.MenuItem();
			this.miOrganizer = new System.Windows.Forms.MenuItem();
			this.N9 = new System.Windows.Forms.MenuItem();
			this.miScripts = new System.Windows.Forms.MenuItem();
			this.miTreeTools = new System.Windows.Forms.MenuItem();
			this.N10 = new System.Windows.Forms.MenuItem();
			this.miFilter = new System.Windows.Forms.MenuItem();
			this.miSearch = new System.Windows.Forms.MenuItem();
			this.N11 = new System.Windows.Forms.MenuItem();
			this.miOptions = new System.Windows.Forms.MenuItem();
			this.miWindow = new System.Windows.Forms.MenuItem();
			this.miWinCascade = new System.Windows.Forms.MenuItem();
			this.miWinHTile = new System.Windows.Forms.MenuItem();
			this.miWinVTile = new System.Windows.Forms.MenuItem();
			this.miWinMinimize = new System.Windows.Forms.MenuItem();
			this.miWinArrange = new System.Windows.Forms.MenuItem();
			this.miHelp = new System.Windows.Forms.MenuItem();
			this.miGenResources = new System.Windows.Forms.MenuItem();
			this.miKinshipTerms = new System.Windows.Forms.MenuItem();
			this.miFAQ = new System.Windows.Forms.MenuItem();
			this.miContext = new System.Windows.Forms.MenuItem();
			this.N12 = new System.Windows.Forms.MenuItem();
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
			this.StatusBar.Location = new System.Drawing.Point(0, 844);
			this.StatusBar.Name = "StatusBar";
			this.StatusBar.Panels.AddRange(new System.Windows.Forms.StatusBarPanel[] {
									this.StatusBarPanel1,
									this.StatusBarPanel2});
			this.StatusBar.ShowPanels = true;
			this.StatusBar.Size = new System.Drawing.Size(896, 20);
			this.StatusBar.TabIndex = 0;
			this.StatusBar.DrawItem += new System.Windows.Forms.StatusBarDrawItemEventHandler(this.StatusBarDrawItem);
			this.StatusBar.PanelClick += new System.Windows.Forms.StatusBarPanelClickEventHandler(this.StatusBarPanelClick);
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
									this.tbUndo,
									this.tbRedo,
									this.TBS3,
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
									this.tbNext});
			this.ToolBar1.ButtonSize = new System.Drawing.Size(27, 26);
			this.ToolBar1.DropDownArrows = true;
			this.ToolBar1.ImageList = this.ImageList_Buttons;
			this.ToolBar1.ImeMode = System.Windows.Forms.ImeMode.NoControl;
			this.ToolBar1.Location = new System.Drawing.Point(0, 0);
			this.ToolBar1.Name = "ToolBar1";
			this.ToolBar1.ShowToolTips = true;
			this.ToolBar1.Size = new System.Drawing.Size(896, 32);
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
			this.tbRecordAdd.ToolTipText = "Добавить запись";
			// 
			// tbRecordEdit
			// 
			this.tbRecordEdit.ImageIndex = 4;
			this.tbRecordEdit.Name = "tbRecordEdit";
			this.tbRecordEdit.ToolTipText = "Изменить запись";
			// 
			// tbRecordDelete
			// 
			this.tbRecordDelete.ImageIndex = 5;
			this.tbRecordDelete.Name = "tbRecordDelete";
			this.tbRecordDelete.ToolTipText = "Удалить запись";
			// 
			// TBS2
			// 
			this.TBS2.Name = "TBS2";
			this.TBS2.Style = System.Windows.Forms.ToolBarButtonStyle.Separator;
			// 
			// tbUndo
			// 
			this.tbUndo.ImageIndex = 31;
			this.tbUndo.Name = "tbUndo";
			// 
			// tbRedo
			// 
			this.tbRedo.ImageIndex = 32;
			this.tbRedo.Name = "tbRedo";
			// 
			// TBS3
			// 
			this.TBS3.Name = "TBS3";
			this.TBS3.Style = System.Windows.Forms.ToolBarButtonStyle.Separator;
			// 
			// tbFilter
			// 
			this.tbFilter.ImageIndex = 16;
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
			// MainMenu1
			// 
			this.MainMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
									this.miFile,
									this.miEdit,
									this.miPedigree,
									this.miService,
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
									this.miExportToWeb,
									this.miExportToExcelApp,
									this.miExportToExcelFile});
			this.miExport.Text = "Экспорт";
			// 
			// miExportToWeb
			// 
			this.miExportToWeb.Index = 0;
			this.miExportToWeb.Text = "Экспорт в Web...";
			this.miExportToWeb.Click += new System.EventHandler(this.miExportToWebClick);
			// 
			// miExportToExcelApp
			// 
			this.miExportToExcelApp.Index = 1;
			this.miExportToExcelApp.Text = "Экспорт в Excel...";
			this.miExportToExcelApp.Click += new System.EventHandler(this.miExportToExcelAppClick);
			// 
			// miExportToExcelFile
			// 
			this.miExportToExcelFile.Index = 2;
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
			this.miExit.Click += new System.EventHandler(this.miExitClick);
			// 
			// miEdit
			// 
			this.miEdit.Index = 1;
			this.miEdit.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
									this.miUndo,
									this.miRedo,
									this.N4,
									this.miRecordAdd,
									this.miRecordEdit,
									this.miRecordDelete,
									this.N5,
									this.miStreamInput});
			this.miEdit.Text = "Правка";
			// 
			// miUndo
			// 
			this.miUndo.Index = 0;
			this.miUndo.Shortcut = System.Windows.Forms.Shortcut.CtrlZ;
			this.miUndo.Text = "Отменить";
			this.miUndo.Click += new System.EventHandler(this.miUndoClick);
			// 
			// miRedo
			// 
			this.miRedo.Index = 1;
			this.miRedo.Shortcut = System.Windows.Forms.Shortcut.CtrlY;
			this.miRedo.Text = "Вернуть";
			this.miRedo.Click += new System.EventHandler(this.miRedoClick);
			// 
			// N4
			// 
			this.N4.Index = 2;
			this.N4.Text = "-";
			// 
			// miRecordAdd
			// 
			this.miRecordAdd.Index = 3;
			this.miRecordAdd.Shortcut = System.Windows.Forms.Shortcut.CtrlI;
			this.miRecordAdd.Text = "Добавить запись";
			this.miRecordAdd.Click += new System.EventHandler(this.miRecordAddClick);
			// 
			// miRecordEdit
			// 
			this.miRecordEdit.Index = 4;
			this.miRecordEdit.Text = "Изменить запись";
			this.miRecordEdit.Click += new System.EventHandler(this.miRecordEditClick);
			// 
			// miRecordDelete
			// 
			this.miRecordDelete.Index = 5;
			this.miRecordDelete.Shortcut = System.Windows.Forms.Shortcut.CtrlL;
			this.miRecordDelete.Text = "Удалить запись";
			this.miRecordDelete.Click += new System.EventHandler(this.miRecordDeleteClick);
			// 
			// N5
			// 
			this.N5.Index = 6;
			this.N5.Text = "-";
			// 
			// miStreamInput
			// 
			this.miStreamInput.Index = 7;
			this.miStreamInput.Text = "Поточный ввод...";
			this.miStreamInput.Click += new System.EventHandler(this.miStreamInputClick);
			// 
			// miPedigree
			// 
			this.miPedigree.Index = 2;
			this.miPedigree.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
									this.miTreeAncestors,
									this.miTreeDescendants,
									this.miTreeBoth,
									this.N6,
									this.miPedigree_dAboville,
									this.miPedigree_Konovalov,
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
			// N6
			// 
			this.N6.Index = 3;
			this.N6.Text = "-";
			// 
			// miPedigree_dAboville
			// 
			this.miPedigree_dAboville.Index = 4;
			this.miPedigree_dAboville.Shortcut = System.Windows.Forms.Shortcut.CtrlP;
			this.miPedigree_dAboville.Text = "Роспись по д\'Абовиллю";
			this.miPedigree_dAboville.Click += new System.EventHandler(this.miPedigree_dAbovilleClick);
			// 
			// miPedigree_Konovalov
			// 
			this.miPedigree_Konovalov.Index = 5;
			this.miPedigree_Konovalov.Shortcut = System.Windows.Forms.Shortcut.CtrlK;
			this.miPedigree_Konovalov.Text = "Роспись по Коновалову";
			this.miPedigree_Konovalov.Click += new System.EventHandler(this.miPedigree_KonovalovClick);
			// 
			// N7
			// 
			this.N7.Index = 6;
			this.N7.Text = "-";
			// 
			// miMap
			// 
			this.miMap.Index = 7;
			this.miMap.Shortcut = System.Windows.Forms.Shortcut.CtrlM;
			this.miMap.Text = "Карты...";
			this.miMap.Click += new System.EventHandler(this.miMapClick);
			// 
			// N8
			// 
			this.N8.Index = 8;
			this.N8.Text = "-";
			// 
			// miStats
			// 
			this.miStats.Index = 9;
			this.miStats.Shortcut = System.Windows.Forms.Shortcut.CtrlT;
			this.miStats.Text = "Статистика...";
			this.miStats.Click += new System.EventHandler(this.miStatsClick);
			// 
			// miService
			// 
			this.miService.Index = 3;
			this.miService.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
									this.miCalc,
									this.miNamesBook,
									this.miCalendar,
									this.miTimeLine,
									this.miOrganizer,
									this.N9,
									this.miScripts,
									this.miTreeTools,
									this.N10,
									this.miFilter,
									this.miSearch,
									this.N11,
									this.miOptions});
			this.miService.Text = "Сервис";
			// 
			// miCalc
			// 
			this.miCalc.Index = 0;
			this.miCalc.Text = "Калькулятор...";
			this.miCalc.Click += new System.EventHandler(this.miCalcClick);
			// 
			// miNamesBook
			// 
			this.miNamesBook.Index = 1;
			this.miNamesBook.Text = "Справочник имен...";
			this.miNamesBook.Click += new System.EventHandler(this.miNamesBookClick);
			// 
			// miCalendar
			// 
			this.miCalendar.Index = 2;
			this.miCalendar.Text = "Календарь...";
			this.miCalendar.Click += new System.EventHandler(this.miCalendarClick);
			// 
			// miTimeLine
			// 
			this.miTimeLine.Index = 3;
			this.miTimeLine.Text = "Линия времени...";
			this.miTimeLine.Click += new System.EventHandler(this.miTimeLineClick);
			// 
			// miOrganizer
			// 
			this.miOrganizer.Index = 4;
			this.miOrganizer.Text = "Органайзер...";
			this.miOrganizer.Click += new System.EventHandler(this.miOrganizerClick);
			// 
			// N9
			// 
			this.N9.Index = 5;
			this.N9.Text = "-";
			// 
			// miScripts
			// 
			this.miScripts.Index = 6;
			this.miScripts.Shortcut = System.Windows.Forms.Shortcut.CtrlF11;
			this.miScripts.Text = "Скрипты...";
			this.miScripts.Click += new System.EventHandler(this.miScriptsClick);
			// 
			// miTreeTools
			// 
			this.miTreeTools.Index = 7;
			this.miTreeTools.Text = "Инструменты...";
			this.miTreeTools.Click += new System.EventHandler(this.miTreeToolsClick);
			// 
			// N10
			// 
			this.N10.Index = 8;
			this.N10.Text = "-";
			// 
			// miFilter
			// 
			this.miFilter.Index = 9;
			this.miFilter.Shortcut = System.Windows.Forms.Shortcut.CtrlF;
			this.miFilter.Text = "Фильтр...";
			this.miFilter.Click += new System.EventHandler(this.miFilterClick);
			// 
			// miSearch
			// 
			this.miSearch.Index = 10;
			this.miSearch.Text = "Поиск...";
			this.miSearch.Click += new System.EventHandler(this.miSearchClick);
			// 
			// N11
			// 
			this.N11.Index = 11;
			this.N11.Text = "-";
			// 
			// miOptions
			// 
			this.miOptions.Index = 12;
			this.miOptions.Text = "Настройки...";
			this.miOptions.Click += new System.EventHandler(this.miOptionsClick);
			// 
			// miWindow
			// 
			this.miWindow.Index = 4;
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
			this.miHelp.Index = 5;
			this.miHelp.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
									this.miGenResources,
									this.miKinshipTerms,
									this.miFAQ,
									this.miContext,
									this.N12,
									this.miAbout});
			this.miHelp.Text = "Справка";
			// 
			// miGenResources
			// 
			this.miGenResources.Index = 0;
			this.miGenResources.Text = "Ресурсы в Интернете...";
			this.miGenResources.Click += new System.EventHandler(this.miGenResourcesClick);
			// 
			// miKinshipTerms
			// 
			this.miKinshipTerms.Index = 1;
			this.miKinshipTerms.Text = "Терминология родства...";
			this.miKinshipTerms.Click += new System.EventHandler(this.miKinshipTermsClick);
			// 
			// miFAQ
			// 
			this.miFAQ.Index = 2;
			this.miFAQ.Text = "Часто задаваемые вопросы...";
			this.miFAQ.Click += new System.EventHandler(this.miFAQClick);
			// 
			// miContext
			// 
			this.miContext.Index = 3;
			this.miContext.Shortcut = System.Windows.Forms.Shortcut.F1;
			this.miContext.Text = "Содержание";
			this.miContext.Click += new System.EventHandler(this.miContextClick);
			// 
			// N12
			// 
			this.N12.Index = 4;
			this.N12.Text = "-";
			// 
			// miAbout
			// 
			this.miAbout.Index = 5;
			this.miAbout.Text = "О программе...";
			this.miAbout.Click += new System.EventHandler(this.miAboutClick);
			// 
			// OpenDialog1
			// 
			this.OpenDialog1.Filter = "GEDCOM|*.ged|Все файлы (*.*)|*.*\'";
			// 
			// SaveDialog1
			// 
			this.SaveDialog1.DefaultExt = "ged";
			this.SaveDialog1.Filter = "GEDCOM|*.ged";
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
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.ClientSize = new System.Drawing.Size(896, 864);
			this.Controls.Add(this.StatusBar);
			this.Controls.Add(this.ToolBar1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F);
			this.Icon = global::GKResources.GK2_Icon;
			this.IsMdiContainer = true;
			this.KeyPreview = true;
			this.Location = new System.Drawing.Point(337, 111);
			this.Menu = this.MainMenu1;
			this.Name = "TfmGEDKeeper";
			this.Text = "GEDKeeper2";
			this.Closing += new System.ComponentModel.CancelEventHandler(this.TfmGEDKeeper_Closing);
			this.Closed += new System.EventHandler(this.TfmGEDKeeper_Closed);
			this.Load += new System.EventHandler(this.FormCreate);
			this.VisibleChanged += new System.EventHandler(this.FormShow);
			this.DragDrop += new System.Windows.Forms.DragEventHandler(this.TfmGEDKeeper_DragDrop);
			this.DragEnter += new System.Windows.Forms.DragEventHandler(this.TfmGEDKeeper_DragEnter);
			this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.TfmGEDKeeper_KeyDown);
			this.Resize += new System.EventHandler(this.TfmGEDKeeper_Resize);
			((System.ComponentModel.ISupportInitialize)(this.StatusBarPanel1)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.StatusBarPanel2)).EndInit();
			this.ResumeLayout(false);
			this.PerformLayout();
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				if (components != null) {
					components.Dispose();
				}
				TMapBrowser.GeoDone();
			}
			base.Dispose(Disposing);
		}
	}
}