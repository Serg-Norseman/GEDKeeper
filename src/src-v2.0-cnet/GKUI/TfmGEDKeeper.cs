using GedCom551;
using GKCore;
using GKUI.Controls;
using GKSys;
using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Resources;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmGEDKeeper : Form, ILocalization
	{
		private IContainer components;
		private StatusBar StatusBar;
		private ToolBar ToolBar1;
		private ToolBarButton tbFileNew;
		private ToolBarButton tbFileLoad;
		private ToolBarButton tbFileSave;
		private ToolBarButton TBS1;
		private ToolBarButton tbRecordAdd;
		private ToolBarButton tbRecordEdit;
		private ToolBarButton tbRecordDelete;
		private ToolBarButton TBS2;
		private ToolBarButton tbFilter;
		private ToolBarButton TBS3;
		private ToolBarButton tbTreeAncestors;
		private ToolBarButton tbTreeDescendants;
		private ToolBarButton TBS4;
		private ToolBarButton tbPedigree;
		private ToolBarButton TBS6;
		private ToolBarButton tbStats;
		private ToolBarButton TBS5;
		private ToolBarButton tbPrev;
		private ToolBarButton tbNext;
		private MainMenu MainMenu1;
		private MenuItem miFile;
		private MenuItem miFileNew;
		private MenuItem miFileLoad;
		private MenuItem miMRUFiles;
		private MenuItem miFileSave;
		private MenuItem miFileClose;
		private MenuItem N1;
		private MenuItem miFileProperties;
		private MenuItem N2;
		private MenuItem miExportToWeb;
		private MenuItem miExportToExcelFile;
		private MenuItem N3;
		private MenuItem miTreeTools;
		private MenuItem N4;
		private MenuItem miExit;
		private MenuItem miEdit;
		private MenuItem miRecordAdd;
		private MenuItem miRecordEdit;
		private MenuItem miRecordDelete;
		private MenuItem N5;
		private MenuItem miStreamInput;
		private MenuItem N6;
		private MenuItem miFilter;
		private MenuItem N7;
		private MenuItem miOptions;
		private MenuItem miPedigree;
		private MenuItem miTreeAncestors;
		private MenuItem miTreeDescendants;
		private MenuItem N8;
		private MenuItem miPedigree_dAboville;
		private MenuItem miPedigree_Konovalov;
		private MenuItem N9;
		private MenuItem miMap;
		private MenuItem N10;
		private MenuItem miStats;
		private MenuItem miWindow;
		private MenuItem miWinCascade;
		private MenuItem miWinHTile;
		private MenuItem miWinVTile;
		private MenuItem miWinMinimize;
		private MenuItem miWinArrange;
		private MenuItem miHelp;
		private MenuItem miGenResources;
		private MenuItem miKinshipTerms;
		private MenuItem miContext;
		private MenuItem N11;
		private MenuItem miAbout;
		private ContextMenu MenuMRU;
		private ContextMenu MenuPedigree;
		private MenuItem miPedigree_dAboville2;
		private MenuItem miPedigree_Konovalov2;
		private OpenFileDialog OpenDialog1;
		private SaveFileDialog SaveDialog1;
		private ToolBarButton TBS7;
		private ToolBarButton tbUndo;
		private ToolBarButton tbRedo;
		private MenuItem miFAQ;
		private ImageList ImageList_Shields;
		private MenuItem miOrganizer;
		private MenuItem miService;
		private MenuItem N12;
		private MenuItem miUndo;
		private MenuItem miRedo;
		private MenuItem miScripts;
		private MenuItem miExport;
		private MenuItem miExportToExcelApp;
		private MenuItem miTreeBoth;
		private ToolBarButton tbTreeBoth;
		private StatusBarPanel StatusBarPanel1;
		private StatusBarPanel StatusBarPanel2;
		private ToolTip ToolTip1;
		private TNamesTable FNamesTable;
		private TGlobalOptions FOptions;
		public ImageList ImageList_Buttons;
		public MenuItem miCalc;
		public MenuItem miNamesBook;
		public MenuItem miCalendar;
		public MenuItem miTimeLine;
		public TfmTimeLine fmTimeLine;
		public TfmCalendar fmCalendar;
		public TfmNamesBook fmNamesBook;
		public TfmCalcWidget fmCalcWidget;
		[Browsable(false)]
		public TNamesTable NamesTable
		{
			get
			{
				return this.FNamesTable;
			}
		}
		[Browsable(false)]
		public TGlobalOptions Options
		{
			get
			{
				return this.FOptions;
			}
		}
		private void MRUFileClick(object sender, EventArgs e)
		{
			int idx = ((TGKMenuItem)sender).Tag;
			this.CreateBase(this.FOptions.MRUFiles[idx]);
		}
		private void UpdateMRU()
		{
			this.miMRUFiles.Enabled = (this.FOptions.MRUFiles.Count > 0);
			this.miMRUFiles.MenuItems.Clear();
			this.MenuMRU.MenuItems.Clear();
			int arg_52_0 = 0;
			int num = this.FOptions.MRUFiles.Count - 1;
			int i = arg_52_0;
			if (num >= i)
			{
				num++;
				do
				{
					MenuItem mi = new TGKMenuItem(this.FOptions.MRUFiles[i], i);
					mi.Click += new EventHandler(this.MRUFileClick);
					this.miMRUFiles.MenuItems.Add(mi);
					mi = new TGKMenuItem(this.FOptions.MRUFiles[i], i);
					mi.Click += new EventHandler(this.MRUFileClick);
					this.MenuMRU.MenuItems.Add(mi);
					i++;
				}
				while (i != num);
			}
		}
		private TRect CheckFormRect(Form aForm)
		{
			int x = aForm.Left;
			int y = aForm.Top;
			int w = aForm.Width;
			int h = aForm.Height;
			Screen scr = Screen.PrimaryScreen;
			int mw = scr.WorkingArea.Width;
			int mh = scr.WorkingArea.Height;
			if (x < 0)
			{
				x = 0;
			}
			if (y < 0)
			{
				y = 0;
			}
			if (w > mw)
			{
				w = mw;
			}
			if (h > mh)
			{
				h = mh;
			}
			return TRect.Create(x, y, x + w - 1, y + h - 1);
		}
		private void InitializeComponent()
		{
			this.components = new Container();
			ResourceManager resources = new ResourceManager(typeof(TfmGEDKeeper));
			this.StatusBar = new StatusBar();
			this.StatusBarPanel1 = new StatusBarPanel();
			this.StatusBarPanel2 = new StatusBarPanel();
			this.ImageList_Buttons = new ImageList(this.components);
			this.ToolBar1 = new ToolBar();
			this.tbFileNew = new ToolBarButton();
			this.tbFileLoad = new ToolBarButton();
			this.MenuMRU = new ContextMenu();
			this.tbFileSave = new ToolBarButton();
			this.TBS1 = new ToolBarButton();
			this.tbRecordAdd = new ToolBarButton();
			this.tbRecordEdit = new ToolBarButton();
			this.tbRecordDelete = new ToolBarButton();
			this.TBS2 = new ToolBarButton();
			this.tbUndo = new ToolBarButton();
			this.tbRedo = new ToolBarButton();
			this.TBS3 = new ToolBarButton();
			this.tbFilter = new ToolBarButton();
			this.TBS4 = new ToolBarButton();
			this.tbTreeAncestors = new ToolBarButton();
			this.tbTreeDescendants = new ToolBarButton();
			this.tbTreeBoth = new ToolBarButton();
			this.TBS5 = new ToolBarButton();
			this.tbPedigree = new ToolBarButton();
			this.MenuPedigree = new ContextMenu();
			this.miPedigree_dAboville2 = new MenuItem();
			this.miPedigree_Konovalov2 = new MenuItem();
			this.TBS6 = new ToolBarButton();
			this.tbStats = new ToolBarButton();
			this.TBS7 = new ToolBarButton();
			this.tbPrev = new ToolBarButton();
			this.tbNext = new ToolBarButton();
			this.MainMenu1 = new MainMenu();
			this.miFile = new MenuItem();
			this.miFileNew = new MenuItem();
			this.miFileLoad = new MenuItem();
			this.miMRUFiles = new MenuItem();
			this.miFileSave = new MenuItem();
			this.miFileClose = new MenuItem();
			this.N1 = new MenuItem();
			this.miFileProperties = new MenuItem();
			this.N2 = new MenuItem();
			this.miExport = new MenuItem();
			this.miExportToWeb = new MenuItem();
			this.miExportToExcelApp = new MenuItem();
			this.miExportToExcelFile = new MenuItem();
			this.N3 = new MenuItem();
			this.miExit = new MenuItem();
			this.miEdit = new MenuItem();
			this.miUndo = new MenuItem();
			this.miRedo = new MenuItem();
			this.N4 = new MenuItem();
			this.miRecordAdd = new MenuItem();
			this.miRecordEdit = new MenuItem();
			this.miRecordDelete = new MenuItem();
			this.N5 = new MenuItem();
			this.miStreamInput = new MenuItem();
			this.miPedigree = new MenuItem();
			this.miTreeAncestors = new MenuItem();
			this.miTreeDescendants = new MenuItem();
			this.miTreeBoth = new MenuItem();
			this.N6 = new MenuItem();
			this.miPedigree_dAboville = new MenuItem();
			this.miPedigree_Konovalov = new MenuItem();
			this.N7 = new MenuItem();
			this.miMap = new MenuItem();
			this.N8 = new MenuItem();
			this.miStats = new MenuItem();
			this.miService = new MenuItem();
			this.miCalc = new MenuItem();
			this.miNamesBook = new MenuItem();
			this.miCalendar = new MenuItem();
			this.miTimeLine = new MenuItem();
			this.miOrganizer = new MenuItem();
			this.N9 = new MenuItem();
			this.miScripts = new MenuItem();
			this.miTreeTools = new MenuItem();
			this.N10 = new MenuItem();
			this.miFilter = new MenuItem();
			this.N11 = new MenuItem();
			this.miOptions = new MenuItem();
			this.miWindow = new MenuItem();
			this.miWinCascade = new MenuItem();
			this.miWinHTile = new MenuItem();
			this.miWinVTile = new MenuItem();
			this.miWinMinimize = new MenuItem();
			this.miWinArrange = new MenuItem();
			this.miHelp = new MenuItem();
			this.miGenResources = new MenuItem();
			this.miKinshipTerms = new MenuItem();
			this.miFAQ = new MenuItem();
			this.miContext = new MenuItem();
			this.N12 = new MenuItem();
			this.miAbout = new MenuItem();
			this.OpenDialog1 = new OpenFileDialog();
			this.SaveDialog1 = new SaveFileDialog();
			this.ImageList_Shields = new ImageList(this.components);
			this.ToolTip1 = new ToolTip(this.components);
			((ISupportInitialize)this.StatusBarPanel1).BeginInit();
			((ISupportInitialize)this.StatusBarPanel2).BeginInit();
			base.SuspendLayout();
			this.StatusBar.ImeMode = ImeMode.NoControl;
			this.StatusBar.Location = new Point(0, 729);
			this.StatusBar.Name = "StatusBar";
			StatusBar.StatusBarPanelCollection arg_504_0 = this.StatusBar.Panels;
			StatusBarPanel[] array = null;
			StatusBarPanel[] array2 = array;
			StatusBarPanel[] array3;
			StatusBarPanel[] expr_4C9 = array3 = new StatusBarPanel[2];
			if (array2 != null)
			{
				int num;
				if ((num = array2.Length) > 2)
				{
					num = 2;
				}
				if (num > 0)
				{
					Array.Copy(array2, array3, num);
				}
			}
			array = expr_4C9;
			array[0] = this.StatusBarPanel1;
			array[1] = this.StatusBarPanel2;
			arg_504_0.AddRange(array);
			this.StatusBar.ShowPanels = true;
			this.StatusBar.Size = new Size(896, 16);
			this.StatusBar.TabIndex = 0;
			this.StatusBarPanel1.Width = 50;
			this.StatusBarPanel2.Width = 24;
			this.ImageList_Buttons.ColorDepth = ColorDepth.Depth24Bit;
			this.ImageList_Buttons.ImageSize = new Size(20, 20);
			this.ImageList_Buttons.ImageStream = (resources.GetObject("ImageList_Buttons.ImageStream") as ImageListStreamer);
			this.ImageList_Buttons.TransparentColor = Color.Transparent;
			this.ToolBar1.Appearance = ToolBarAppearance.Flat;
			ToolBar.ToolBarButtonCollection arg_6C5_0 = this.ToolBar1.Buttons;
			ToolBarButton[] array4 = null;
			ToolBarButton[] array5 = array4;
			ToolBarButton[] array6;
			ToolBarButton[] expr_5C1 = array6 = new ToolBarButton[23];
			if (array5 != null)
			{
				int num2;
				if ((num2 = array5.Length) > 23)
				{
					num2 = 23;
				}
				if (num2 > 0)
				{
					Array.Copy(array5, array6, num2);
				}
			}
			array4 = expr_5C1;
			array4[0] = this.tbFileNew;
			array4[1] = this.tbFileLoad;
			array4[2] = this.tbFileSave;
			array4[3] = this.TBS1;
			array4[4] = this.tbRecordAdd;
			array4[5] = this.tbRecordEdit;
			array4[6] = this.tbRecordDelete;
			array4[7] = this.TBS2;
			array4[8] = this.tbUndo;
			array4[9] = this.tbRedo;
			array4[10] = this.TBS3;
			array4[11] = this.tbFilter;
			array4[12] = this.TBS4;
			array4[13] = this.tbTreeAncestors;
			array4[14] = this.tbTreeDescendants;
			array4[15] = this.tbTreeBoth;
			array4[16] = this.TBS5;
			array4[17] = this.tbPedigree;
			array4[18] = this.TBS6;
			array4[19] = this.tbStats;
			array4[20] = this.TBS7;
			array4[21] = this.tbPrev;
			array4[22] = this.tbNext;
			arg_6C5_0.AddRange(array4);
			this.ToolBar1.ButtonSize = new Size(27, 26);
			this.ToolBar1.DropDownArrows = true;
			this.ToolBar1.ImageList = this.ImageList_Buttons;
			this.ToolBar1.ImeMode = ImeMode.NoControl;
			this.ToolBar1.Location = new Point(0, 0);
			this.ToolBar1.Name = "ToolBar1";
			this.ToolBar1.ShowToolTips = true;
			this.ToolBar1.Size = new Size(896, 32);
			this.ToolBar1.TabIndex = 0;
			this.ToolBar1.ButtonClick += new ToolBarButtonClickEventHandler(this.ToolBar1_ButtonClick);
			this.tbFileNew.ImageIndex = 0;
			this.tbFileNew.ToolTipText = "Создать новый файл древа";
			this.tbFileLoad.DropDownMenu = this.MenuMRU;
			this.tbFileLoad.ImageIndex = 1;
			this.tbFileLoad.Style = ToolBarButtonStyle.DropDownButton;
			this.tbFileLoad.ToolTipText = "Открыть файл древа";
			this.MenuMRU.RightToLeft = RightToLeft.No;
			this.tbFileSave.ImageIndex = 2;
			this.tbFileSave.ToolTipText = "Сохранить файл древа";
			this.TBS1.Style = ToolBarButtonStyle.Separator;
			this.tbRecordAdd.ImageIndex = 3;
			this.tbRecordAdd.ToolTipText = "Добавить запись";
			this.tbRecordEdit.ImageIndex = 4;
			this.tbRecordEdit.ToolTipText = "Изменить запись";
			this.tbRecordDelete.ImageIndex = 5;
			this.tbRecordDelete.ToolTipText = "Удалить запись";
			this.TBS2.Style = ToolBarButtonStyle.Separator;
			this.tbUndo.ImageIndex = 31;
			this.tbRedo.ImageIndex = 32;
			this.TBS3.Style = ToolBarButtonStyle.Separator;
			this.tbFilter.ImageIndex = 16;
			this.tbFilter.ToolTipText = "Фильтрация списка персон";
			this.TBS4.Style = ToolBarButtonStyle.Separator;
			this.tbTreeAncestors.ImageIndex = 18;
			this.tbTreeAncestors.ToolTipText = "Сформировать древо предков";
			this.tbTreeDescendants.ImageIndex = 19;
			this.tbTreeDescendants.ToolTipText = "Сформировать древо потомков";
			this.tbTreeBoth.ImageIndex = 34;
			this.tbTreeBoth.ToolTipText = "Сформировать полное древо";
			this.TBS5.Style = ToolBarButtonStyle.Separator;
			this.tbPedigree.DropDownMenu = this.MenuPedigree;
			this.tbPedigree.ImageIndex = 20;
			this.tbPedigree.Style = ToolBarButtonStyle.DropDownButton;
			this.tbPedigree.ToolTipText = "Родословная роспись";
			Menu.MenuItemCollection arg_997_0 = this.MenuPedigree.MenuItems;
			MenuItem[] array7 = null;
			MenuItem[] array8 = array7;
			MenuItem[] array9;
			MenuItem[] expr_95C = array9 = new MenuItem[2];
			if (array8 != null)
			{
				int num3;
				if ((num3 = array8.Length) > 2)
				{
					num3 = 2;
				}
				if (num3 > 0)
				{
					Array.Copy(array8, array9, num3);
				}
			}
			array7 = expr_95C;
			array7[0] = this.miPedigree_dAboville2;
			array7[1] = this.miPedigree_Konovalov2;
			arg_997_0.AddRange(array7);
			this.MenuPedigree.RightToLeft = RightToLeft.No;
			this.miPedigree_dAboville2.Index = 0;
			this.miPedigree_dAboville2.Text = "Роспись по д'Абовиллю";
			this.miPedigree_dAboville2.Click += new EventHandler(this.miPedigree_dAbovilleClick);
			this.miPedigree_Konovalov2.Index = 1;
			this.miPedigree_Konovalov2.Text = "Роспись по Коновалову";
			this.miPedigree_Konovalov2.Click += new EventHandler(this.miPedigree_KonovalovClick);
			this.TBS6.Style = ToolBarButtonStyle.Separator;
			this.tbStats.ImageIndex = 11;
			this.tbStats.ToolTipText = "Статистический анализ данных";
			this.TBS7.Style = ToolBarButtonStyle.Separator;
			this.tbPrev.Enabled = false;
			this.tbPrev.ImageIndex = 22;
			this.tbPrev.ToolTipText = "Предыдущая запись";
			this.tbNext.Enabled = false;
			this.tbNext.ImageIndex = 23;
			this.tbNext.ToolTipText = "Следующая запись";
			Menu.MenuItemCollection arg_B10_0 = this.MainMenu1.MenuItems;
			MenuItem[] array10 = null;
			MenuItem[] array11 = array10;
			MenuItem[] array12;
			MenuItem[] expr_AAD = array12 = new MenuItem[6];
			if (array11 != null)
			{
				int num4;
				if ((num4 = array11.Length) > 6)
				{
					num4 = 6;
				}
				if (num4 > 0)
				{
					Array.Copy(array11, array12, num4);
				}
			}
			array10 = expr_AAD;
			array10[0] = this.miFile;
			array10[1] = this.miEdit;
			array10[2] = this.miPedigree;
			array10[3] = this.miService;
			array10[4] = this.miWindow;
			array10[5] = this.miHelp;
			arg_B10_0.AddRange(array10);
			this.miFile.Index = 0;
			Menu.MenuItemCollection arg_BC4_0 = this.miFile.MenuItems;
			MenuItem[] array13 = null;
			MenuItem[] array14 = array13;
			MenuItem[] array15;
			MenuItem[] expr_B38 = array15 = new MenuItem[11];
			if (array14 != null)
			{
				int num5;
				if ((num5 = array14.Length) > 11)
				{
					num5 = 11;
				}
				if (num5 > 0)
				{
					Array.Copy(array14, array15, num5);
				}
			}
			array13 = expr_B38;
			array13[0] = this.miFileNew;
			array13[1] = this.miFileLoad;
			array13[2] = this.miMRUFiles;
			array13[3] = this.miFileSave;
			array13[4] = this.miFileClose;
			array13[5] = this.N1;
			array13[6] = this.miFileProperties;
			array13[7] = this.N2;
			array13[8] = this.miExport;
			array13[9] = this.N3;
			array13[10] = this.miExit;
			arg_BC4_0.AddRange(array13);
			this.miFile.Text = "Файл";
			this.miFileNew.Index = 0;
			this.miFileNew.Shortcut = Shortcut.CtrlN;
			this.miFileNew.Text = "Новый";
			this.miFileNew.Click += new EventHandler(this.miFileNewClick);
			this.miFileLoad.Index = 1;
			this.miFileLoad.Shortcut = Shortcut.CtrlO;
			this.miFileLoad.Text = "Открыть...";
			this.miFileLoad.Click += new EventHandler(this.miFileLoadClick);
			this.miMRUFiles.Enabled = false;
			this.miMRUFiles.Index = 2;
			this.miMRUFiles.Text = "Открыть последний";
			this.miFileSave.Index = 3;
			this.miFileSave.Shortcut = Shortcut.CtrlS;
			this.miFileSave.Text = "Сохранить...";
			this.miFileSave.Click += new EventHandler(this.miFileSaveClick);
			this.miFileClose.Index = 4;
			this.miFileClose.Text = "Закрыть";
			this.miFileClose.Click += new EventHandler(this.miFileCloseClick);
			this.N1.Index = 5;
			this.N1.Text = "-";
			this.miFileProperties.Index = 6;
			this.miFileProperties.Text = "Свойства файла...";
			this.miFileProperties.Click += new EventHandler(this.miFilePropertiesClick);
			this.N2.Index = 7;
			this.N2.Text = "-";
			this.miExport.Index = 8;
			Menu.MenuItemCollection arg_DD1_0 = this.miExport.MenuItems;
			MenuItem[] array16 = null;
			MenuItem[] array17 = array16;
			MenuItem[] array18;
			MenuItem[] expr_D8C = array18 = new MenuItem[3];
			if (array17 != null)
			{
				int num6;
				if ((num6 = array17.Length) > 3)
				{
					num6 = 3;
				}
				if (num6 > 0)
				{
					Array.Copy(array17, array18, num6);
				}
			}
			array16 = expr_D8C;
			array16[0] = this.miExportToWeb;
			array16[1] = this.miExportToExcelApp;
			array16[2] = this.miExportToExcelFile;
			arg_DD1_0.AddRange(array16);
			this.miExport.Text = "Экспорт";
			this.miExportToWeb.Index = 0;
			this.miExportToWeb.Text = "Экспорт в Web...";
			this.miExportToWeb.Click += new EventHandler(this.miExportToWebClick);
			this.miExportToExcelApp.Index = 1;
			this.miExportToExcelApp.Text = "Экспорт в Excel...";
			this.miExportToExcelApp.Click += new EventHandler(this.miExportToExcelAppClick);
			this.miExportToExcelFile.Index = 2;
			this.miExportToExcelFile.Text = "Экспорт в Excel-файл...";
			this.miExportToExcelFile.Click += new EventHandler(this.miExportToExcelFileClick);
			this.N3.Index = 9;
			this.N3.Text = "-";
			this.miExit.Index = 10;
			this.miExit.Shortcut = Shortcut.CtrlX;
			this.miExit.Text = "Выход";
			this.miExit.Click += new EventHandler(this.miExitClick);
			this.miEdit.Index = 1;
			Menu.MenuItemCollection arg_F7B_0 = this.miEdit.MenuItems;
			MenuItem[] array19 = null;
			MenuItem[] array20 = array19;
			MenuItem[] array21;
			MenuItem[] expr_F04 = array21 = new MenuItem[8];
			if (array20 != null)
			{
				int num7;
				if ((num7 = array20.Length) > 8)
				{
					num7 = 8;
				}
				if (num7 > 0)
				{
					Array.Copy(array20, array21, num7);
				}
			}
			array19 = expr_F04;
			array19[0] = this.miUndo;
			array19[1] = this.miRedo;
			array19[2] = this.N4;
			array19[3] = this.miRecordAdd;
			array19[4] = this.miRecordEdit;
			array19[5] = this.miRecordDelete;
			array19[6] = this.N5;
			array19[7] = this.miStreamInput;
			arg_F7B_0.AddRange(array19);
			this.miEdit.Text = "Правка";
			this.miUndo.Index = 0;
			this.miUndo.Shortcut = Shortcut.CtrlZ;
			this.miUndo.Text = "Отменить";
			this.miUndo.Click += new EventHandler(this.miUndoClick);
			this.miRedo.Index = 1;
			this.miRedo.Shortcut = Shortcut.CtrlY;
			this.miRedo.Text = "Вернуть";
			this.miRedo.Click += new EventHandler(this.miRedoClick);
			this.N4.Index = 2;
			this.N4.Text = "-";
			this.miRecordAdd.Index = 3;
			this.miRecordAdd.Shortcut = Shortcut.CtrlI;
			this.miRecordAdd.Text = "Добавить запись";
			this.miRecordAdd.Click += new EventHandler(this.miRecordAddClick);
			this.miRecordEdit.Index = 4;
			this.miRecordEdit.Text = "Изменить запись";
			this.miRecordEdit.Click += new EventHandler(this.miRecordEditClick);
			this.miRecordDelete.Index = 5;
			this.miRecordDelete.Shortcut = Shortcut.CtrlL;
			this.miRecordDelete.Text = "Удалить запись";
			this.miRecordDelete.Click += new EventHandler(this.miRecordDeleteClick);
			this.N5.Index = 6;
			this.N5.Text = "-";
			this.miStreamInput.Index = 7;
			this.miStreamInput.Text = "Поточный ввод...";
			this.miStreamInput.Click += new EventHandler(this.miStreamInputClick);
			this.miPedigree.Index = 2;
			Menu.MenuItemCollection arg_11ED_0 = this.miPedigree.MenuItems;
			MenuItem[] array22 = null;
			MenuItem[] array23 = array22;
			MenuItem[] array24;
			MenuItem[] expr_115F = array24 = new MenuItem[10];
			if (array23 != null)
			{
				int num8;
				if ((num8 = array23.Length) > 10)
				{
					num8 = 10;
				}
				if (num8 > 0)
				{
					Array.Copy(array23, array24, num8);
				}
			}
			array22 = expr_115F;
			array22[0] = this.miTreeAncestors;
			array22[1] = this.miTreeDescendants;
			array22[2] = this.miTreeBoth;
			array22[3] = this.N6;
			array22[4] = this.miPedigree_dAboville;
			array22[5] = this.miPedigree_Konovalov;
			array22[6] = this.N7;
			array22[7] = this.miMap;
			array22[8] = this.N8;
			array22[9] = this.miStats;
			arg_11ED_0.AddRange(array22);
			this.miPedigree.Text = "Родословная";
			this.miTreeAncestors.Index = 0;
			this.miTreeAncestors.Shortcut = Shortcut.CtrlA;
			this.miTreeAncestors.Text = "Древо предков";
			this.miTreeAncestors.Click += new EventHandler(this.miTreeAncestorsClick);
			this.miTreeDescendants.Index = 1;
			this.miTreeDescendants.Shortcut = Shortcut.CtrlD;
			this.miTreeDescendants.Text = "Древо потомков";
			this.miTreeDescendants.Click += new EventHandler(this.miTreeDescendantsClick);
			this.miTreeBoth.Index = 2;
			this.miTreeBoth.Text = "Древо полное";
			this.miTreeBoth.Click += new EventHandler(this.miTreeBothClick);
			this.N6.Index = 3;
			this.N6.Text = "-";
			this.miPedigree_dAboville.Index = 4;
			this.miPedigree_dAboville.Shortcut = Shortcut.CtrlP;
			this.miPedigree_dAboville.Text = "Роспись по д'Абовиллю";
			this.miPedigree_dAboville.Click += new EventHandler(this.miPedigree_dAbovilleClick);
			this.miPedigree_Konovalov.Index = 5;
			this.miPedigree_Konovalov.Shortcut = Shortcut.CtrlK;
			this.miPedigree_Konovalov.Text = "Роспись по Коновалову";
			this.miPedigree_Konovalov.Click += new EventHandler(this.miPedigree_KonovalovClick);
			this.N7.Index = 6;
			this.N7.Text = "-";
			this.miMap.Index = 7;
			this.miMap.Shortcut = Shortcut.CtrlM;
			this.miMap.Text = "Карты...";
			this.miMap.Click += new EventHandler(this.miMapClick);
			this.N8.Index = 8;
			this.N8.Text = "-";
			this.miStats.Index = 9;
			this.miStats.Shortcut = Shortcut.CtrlT;
			this.miStats.Text = "Статистика...";
			this.miStats.Click += new EventHandler(this.miStatsClick);
			this.miService.Index = 3;
			Menu.MenuItemCollection arg_14D5_0 = this.miService.MenuItems;
			MenuItem[] array25 = null;
			MenuItem[] array26 = array25;
			MenuItem[] array27;
			MenuItem[] expr_143F = array27 = new MenuItem[12];
			if (array26 != null)
			{
				int num9;
				if ((num9 = array26.Length) > 12)
				{
					num9 = 12;
				}
				if (num9 > 0)
				{
					Array.Copy(array26, array27, num9);
				}
			}
			array25 = expr_143F;
			array25[0] = this.miCalc;
			array25[1] = this.miNamesBook;
			array25[2] = this.miCalendar;
			array25[3] = this.miTimeLine;
			array25[4] = this.miOrganizer;
			array25[5] = this.N9;
			array25[6] = this.miScripts;
			array25[7] = this.miTreeTools;
			array25[8] = this.N10;
			array25[9] = this.miFilter;
			array25[10] = this.N11;
			array25[11] = this.miOptions;
			arg_14D5_0.AddRange(array25);
			this.miService.Text = "Сервис";
			this.miCalc.Index = 0;
			this.miCalc.Text = "Калькулятор...";
			this.miCalc.Click += new EventHandler(this.miCalcClick);
			this.miNamesBook.Index = 1;
			this.miNamesBook.Text = "Справочник имен...";
			this.miNamesBook.Click += new EventHandler(this.miNamesBookClick);
			this.miCalendar.Index = 2;
			this.miCalendar.Text = "Календарь...";
			this.miCalendar.Click += new EventHandler(this.miCalendarClick);
			this.miTimeLine.Index = 3;
			this.miTimeLine.Text = "Линия времени...";
			this.miTimeLine.Click += new EventHandler(this.miTimeLineClick);
			this.miOrganizer.Index = 4;
			this.miOrganizer.Text = "Органайзер...";
			this.miOrganizer.Click += new EventHandler(this.miOrganizerClick);
			this.N9.Index = 5;
			this.N9.Text = "-";
			this.miScripts.Index = 6;
			this.miScripts.Shortcut = Shortcut.CtrlF11;
			this.miScripts.Text = "Скрипты...";
			this.miScripts.Click += new EventHandler(this.miScriptsClick);
			this.miTreeTools.Index = 7;
			this.miTreeTools.Text = "Инструменты...";
			this.miTreeTools.Click += new EventHandler(this.miTreeToolsClick);
			this.N10.Index = 8;
			this.N10.Text = "-";
			this.miFilter.Index = 9;
			this.miFilter.Shortcut = Shortcut.CtrlF;
			this.miFilter.Text = "Фильтр...";
			this.miFilter.Click += new EventHandler(this.miFilterClick);
			this.N11.Index = 10;
			this.N11.Text = "-";
			this.miOptions.Index = 11;
			this.miOptions.Text = "Настройки...";
			this.miOptions.Click += new EventHandler(this.miOptionsClick);
			this.miWindow.Index = 4;
			Menu.MenuItemCollection arg_17A9_0 = this.miWindow.MenuItems;
			MenuItem[] array28 = null;
			MenuItem[] array29 = array28;
			MenuItem[] array30;
			MenuItem[] expr_1750 = array30 = new MenuItem[5];
			if (array29 != null)
			{
				int num10;
				if ((num10 = array29.Length) > 5)
				{
					num10 = 5;
				}
				if (num10 > 0)
				{
					Array.Copy(array29, array30, num10);
				}
			}
			array28 = expr_1750;
			array28[0] = this.miWinCascade;
			array28[1] = this.miWinHTile;
			array28[2] = this.miWinVTile;
			array28[3] = this.miWinMinimize;
			array28[4] = this.miWinArrange;
			arg_17A9_0.AddRange(array28);
			this.miWindow.Text = "&Окна";
			this.miWinCascade.Index = 0;
			this.miWinCascade.Text = "&Каскад";
			this.miWinCascade.Click += new EventHandler(this.miWinCascadeClick);
			this.miWinHTile.Index = 1;
			this.miWinHTile.Text = "&Горизонтальная мозаика";
			this.miWinHTile.Click += new EventHandler(this.miWinHTileClick);
			this.miWinVTile.Index = 2;
			this.miWinVTile.Text = "&Вертикальная мозаика";
			this.miWinVTile.Click += new EventHandler(this.miWinVTileClick);
			this.miWinMinimize.Index = 3;
			this.miWinMinimize.Text = "&Свернуть все";
			this.miWinMinimize.Click += new EventHandler(this.miWinMinimizeClick);
			this.miWinArrange.Index = 4;
			this.miWinArrange.Text = "&Разместить все";
			this.miWinArrange.Click += new EventHandler(this.miWinArrangeClick);
			this.miHelp.Index = 5;
			Menu.MenuItemCollection arg_1944_0 = this.miHelp.MenuItems;
			MenuItem[] array31 = null;
			MenuItem[] array32 = array31;
			MenuItem[] array33;
			MenuItem[] expr_18E1 = array33 = new MenuItem[6];
			if (array32 != null)
			{
				int num11;
				if ((num11 = array32.Length) > 6)
				{
					num11 = 6;
				}
				if (num11 > 0)
				{
					Array.Copy(array32, array33, num11);
				}
			}
			array31 = expr_18E1;
			array31[0] = this.miGenResources;
			array31[1] = this.miKinshipTerms;
			array31[2] = this.miFAQ;
			array31[3] = this.miContext;
			array31[4] = this.N12;
			array31[5] = this.miAbout;
			arg_1944_0.AddRange(array31);
			this.miHelp.Text = "Справка";
			this.miGenResources.Index = 0;
			this.miGenResources.Text = "Ресурсы в Интернете...";
			this.miGenResources.Click += new EventHandler(this.miGenResourcesClick);
			this.miKinshipTerms.Index = 1;
			this.miKinshipTerms.Text = "Терминология родства...";
			this.miKinshipTerms.Click += new EventHandler(this.miKinshipTermsClick);
			this.miFAQ.Index = 2;
			this.miFAQ.Text = "Часто задаваемые вопросы...";
			this.miFAQ.Click += new EventHandler(this.miFAQClick);
			this.miContext.Index = 3;
			this.miContext.Shortcut = Shortcut.F1;
			this.miContext.Text = "Содержание";
			this.miContext.Click += new EventHandler(this.miContextClick);
			this.N12.Index = 4;
			this.N12.Text = "-";
			this.miAbout.Index = 5;
			this.miAbout.Text = "О программе...";
			this.miAbout.Click += new EventHandler(this.miAboutClick);
			this.OpenDialog1.Filter = "GEDCOM|*.ged|Все файлы (*.*)|*.*'";
			this.SaveDialog1.DefaultExt = "ged";
			this.SaveDialog1.Filter = "GEDCOM|*.ged";
			this.ImageList_Shields.ImageSize = new Size(16, 16);
			this.ImageList_Shields.TransparentColor = Color.Transparent;
			this.AllowDrop = true;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.ClientSize = new Size(896, 745);
			base.Controls.Add(this.StatusBar);
			base.Controls.Add(this.ToolBar1);
			this.Font = new Font("Tahoma", 8.25f);
			base.Icon = (resources.GetObject("$this.Icon") as Icon);
			base.IsMdiContainer = true;
			base.Location = new Point(337, 111);
			base.Menu = this.MainMenu1;
			base.Name = "TfmGEDKeeper";
			this.Text = "GEDKeeper";
			base.Resize += new EventHandler(this.TfmGEDKeeper_Resize);
			base.Closing += new CancelEventHandler(this.TfmGEDKeeper_Closing);
			base.Load += new EventHandler(this.FormCreate);
			base.DragDrop += new DragEventHandler(this.TfmGEDKeeper_DragDrop);
			base.Closed += new EventHandler(this.TfmGEDKeeper_Closed);
			base.VisibleChanged += new EventHandler(this.FormShow);
			base.DragEnter += new DragEventHandler(this.TfmGEDKeeper_DragEnter);
			((ISupportInitialize)this.StatusBarPanel1).EndInit();
			((ISupportInitialize)this.StatusBarPanel2).EndInit();
			base.ResumeLayout(false);
		}
		private void FormCreate(object sender, EventArgs e)
		{
			TGKSys.LogInit(TGKSys.GetAppPath() + "GEDKeeper.log");
			this.FOptions = new TGlobalOptions();
			this.FOptions.LoadFromFile(TGKSys.GetAppPath() + "GEDKeeper2.ini");
			this.FOptions.FindLanguages();
			if (this.FOptions.MWinRect.Left != -1 && this.FOptions.MWinRect.Top != -1 && this.FOptions.MWinRect.Right != -1 && this.FOptions.MWinRect.Bottom != -1)
			{
				base.Left = this.FOptions.MWinRect.Left;
				base.Top = this.FOptions.MWinRect.Top;
				base.Width = this.FOptions.MWinRect.Right;
				base.Height = this.FOptions.MWinRect.Bottom;
			}
			else
			{
				base.Left = (Screen.PrimaryScreen.WorkingArea.Width - 800) / 2;
				base.Top = (Screen.PrimaryScreen.WorkingArea.Height - 600) / 2;
				base.Width = 800;
				base.Height = 600;
			}
			base.WindowState = this.FOptions.MWinState;
			this.FNamesTable = new TNamesTable();
			this.FNamesTable.LoadFromFile(TGKSys.GetAppPath() + "GEDKeeper.nms");
			this.LoadLanguage((int)this.FOptions.InterfaceLang);
			this.UpdateMRU();
			this.UpdateControls(false);
		}
		private void StatusBarDblClick(object sender, EventArgs e)
		{
		}
		private void miExitClick(object sender, EventArgs e)
		{
			base.Close();
		}
		private void miExportToWebClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ExportToWeb();
			}
		}
		private void miExportToExcelFileClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ExportToExcel(false);
			}
		}
		private void miFilePropertiesClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.FileProperties(TfmBase.TFilePropertiesMode.fpmAuthor);
			}
		}
		private void miStreamInputClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.PersonScan();
			}
		}
		private void miScriptsClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ShowScriptDaemon();
			}
		}
		private void miTreeToolsClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.TreeTools();
			}
		}
		private void miOptionsClick(object sender, EventArgs e)
		{
			TfmOptions fmOptions = new TfmOptions();
			try
			{
				if (fmOptions.ShowDialog() == DialogResult.OK)
				{
					int arg_23_0 = 0;
					Form[] mdiChildren = base.MdiChildren;
					int num = ((mdiChildren != null) ? mdiChildren.Length : 0) - 1;
					int i = arg_23_0;
					if (num >= i)
					{
						num++;
						do
						{
							if (base.MdiChildren[i] is TfmBase)
							{
								(base.MdiChildren[i] as TfmBase).ListsRefresh(true);
							}
							i++;
						}
						while (i != num);
					}
				}
			}
			finally
			{
				TObjectHelper.Free(fmOptions);
			}
		}
		private void miFileCloseClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.Close();
			}
		}
		private void miMapClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ShowMap();
			}
		}
		private void miOrganizerClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ShowOrganizer();
			}
		}
		private void miTimeLineClick(object sender, EventArgs e)
		{
			if (!this.miTimeLine.Checked)
			{
				this.fmTimeLine = new TfmTimeLine();
				this.fmTimeLine.Location = new Point(10, Screen.PrimaryScreen.WorkingArea.Height - this.fmTimeLine.Height - 10);
				this.fmTimeLine.Show();
				this.miTimeLine.Checked = true;
			}
			else
			{
				this.miTimeLine.Checked = false;
				object obj = this.fmTimeLine;
				VCLUtils.FreeAndNil(ref obj);
				this.fmTimeLine = (obj as TfmTimeLine);
			}
		}
		private void miCalendarClick(object sender, EventArgs e)
		{
			if (!this.miCalendar.Checked)
			{
				this.fmCalendar = new TfmCalendar();
				this.fmCalendar.Location = new Point(Screen.PrimaryScreen.WorkingArea.Width - this.fmCalendar.Width - 10, 50);
				this.fmCalendar.Show();
				this.miCalendar.Checked = true;
			}
			else
			{
				this.miCalendar.Checked = false;
				object obj = this.fmCalendar;
				VCLUtils.FreeAndNil(ref obj);
				this.fmCalendar = (obj as TfmCalendar);
			}
		}
		private void miNamesBookClick(object sender, EventArgs e)
		{
			if (!this.miNamesBook.Checked)
			{
				Screen scr = Screen.PrimaryScreen;
				this.fmNamesBook = new TfmNamesBook();
				this.fmNamesBook.Location = new Point(scr.WorkingArea.Width - this.fmNamesBook.Width - 10, (scr.WorkingArea.Height - this.fmNamesBook.Height) / 2);
				this.fmNamesBook.Show();
				this.miNamesBook.Checked = true;
			}
			else
			{
				this.miNamesBook.Checked = false;
				object obj = this.fmNamesBook;
				VCLUtils.FreeAndNil(ref obj);
				this.fmNamesBook = (obj as TfmNamesBook);
			}
		}
		private void miCalcClick(object sender, EventArgs e)
		{
			if (!this.miCalc.Checked)
			{
				Screen scr = Screen.PrimaryScreen;
				this.fmCalcWidget = new TfmCalcWidget();
				this.fmCalcWidget.Location = new Point(scr.WorkingArea.Width - this.fmCalcWidget.Width - 10, scr.WorkingArea.Height - this.fmCalcWidget.Height - 10);
				this.fmCalcWidget.Show();
				this.miCalc.Checked = true;
			}
			else
			{
				this.miCalc.Checked = false;
				object obj = this.fmCalcWidget;
				VCLUtils.FreeAndNil(ref obj);
				this.fmCalcWidget = (obj as TfmCalcWidget);
			}
		}
		private void miAboutClick(object sender, EventArgs e)
		{
			TfmAbout.ShowAbout("GEDKeeper", TGKSys.GetFileVersion());
		}
		private void miGenResourcesClick(object sender, EventArgs e)
		{
			this.ShowHelpTopic("::/gkhGenRes.htm");
		}
		private void miKinshipTermsClick(object sender, EventArgs e)
		{
			this.ShowHelpTopic("::/gkhRelations.htm");
		}
		private void miFAQClick(object sender, EventArgs e)
		{
			this.ShowHelpTopic("::/gkhFAQ.htm");
		}
		private void miContextClick(object sender, EventArgs e)
		{
			this.ShowHelpTopic("");
		}
		private void miWinCascadeClick(object sender, EventArgs e)
		{
			base.LayoutMdi(MdiLayout.Cascade);
		}
		private void miWinHTileClick(object sender, EventArgs e)
		{
			base.LayoutMdi(MdiLayout.TileHorizontal);
		}
		private void miWinVTileClick(object sender, EventArgs e)
		{
			base.LayoutMdi(MdiLayout.TileVertical);
		}
		private void miWinMinimizeClick(object sender, EventArgs e)
		{
			Form[] mdiChildren = base.MdiChildren;
			int I = ((mdiChildren != null) ? mdiChildren.Length : 0) - 1;
			if (I >= 0)
			{
				do
				{
					base.MdiChildren[I].WindowState = FormWindowState.Minimized;
					I--;
				}
				while (I != -1);
			}
		}
		private void miWinArrangeClick(object sender, EventArgs e)
		{
			base.LayoutMdi(MdiLayout.ArrangeIcons);
		}
		private void miFilterClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.SetFilter();
			}
		}
		private void tbPrevClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.NavPrev();
			}
		}
		private void tbNextClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.NavNext();
			}
		}
		private void miStatsClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ShowStats();
			}
		}
		private void miFileNewClick(object sender, EventArgs e)
		{
			this.CreateBase("");
		}
		private void miFileLoadClick(object sender, EventArgs e)
		{
			this.OpenDialog1.InitialDirectory = this.FOptions.LastDir;
			if (this.OpenDialog1.ShowDialog() == DialogResult.OK)
			{
				this.CreateBase(this.OpenDialog1.FileName);
			}
		}
		private void miUndoClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.DoUndo();
			}
		}
		private void miRedoClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.DoRedo();
			}
		}
		private void miTreeAncestorsClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ShowTreeAncestors();
			}
		}
		private void miTreeDescendantsClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ShowTreeDescendants();
			}
		}
		private void miPedigree_dAbovilleClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.GenPedigree_dAboville();
			}
		}
		private void miPedigree_KonovalovClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.GenPedigree_Konovalov();
			}
		}
		private void miRecordAddClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.RecordAdd();
			}
		}
		private void miRecordEditClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.RecordEdit(sender, e);
			}
		}
		private void miRecordDeleteClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.RecordDelete();
			}
		}
		private void miExportToExcelAppClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ExportToExcel(true);
			}
		}
		private void miTreeBothClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				cur_base.ShowTreeBoth();
			}
		}
		private void FormShow(object sender, EventArgs e)
		{
			int arg_0F_0 = 0;
			int num = this.FOptions.LastBasesCount - 1;
			int i = arg_0F_0;
			if (num >= i)
			{
				num++;
				do
				{
					TGlobalOptions.TBaseWin lb = this.FOptions.GetLastBase(i);
					if (File.Exists(lb.FileName))
					{
						TfmBase @base = this.CreateBase(lb.FileName);
						@base.Left = lb.WinRect.Left;
						@base.Top = lb.WinRect.Top;
						@base.Width = lb.WinRect.Right;
						@base.Height = lb.WinRect.Bottom;
						@base.WindowState = lb.WinState;
					}
					i++;
				}
				while (i != num);
			}
		}

		private void ToolBar1_ButtonClick(object sender, ToolBarButtonClickEventArgs e)
		{
			if (object.Equals(e.Button, this.tbFileNew))
			{
				this.miFileNewClick(null, null);
			}
			if (object.Equals(e.Button, this.tbFileLoad))
			{
				this.miFileLoadClick(null, null);
			}
			if (object.Equals(e.Button, this.tbFileSave))
			{
				this.miFileSaveClick(null, null);
			}
			if (object.Equals(e.Button, this.tbRecordAdd))
			{
				this.miRecordAddClick(null, null);
			}
			if (object.Equals(e.Button, this.tbRecordEdit))
			{
				this.miRecordEditClick(null, null);
			}
			if (object.Equals(e.Button, this.tbRecordDelete))
			{
				this.miRecordDeleteClick(null, null);
			}
			if (object.Equals(e.Button, this.tbUndo))
			{
				this.miUndoClick(null, null);
			}
			if (object.Equals(e.Button, this.tbRedo))
			{
				this.miRedoClick(null, null);
			}
			if (object.Equals(e.Button, this.tbFilter))
			{
				this.miFilterClick(null, null);
			}
			if (object.Equals(e.Button, this.tbTreeAncestors))
			{
				this.miTreeAncestorsClick(null, null);
			}
			if (object.Equals(e.Button, this.tbTreeDescendants))
			{
				this.miTreeDescendantsClick(null, null);
			}
			if (object.Equals(e.Button, this.tbTreeBoth))
			{
				this.miTreeBothClick(null, null);
			}
			if (object.Equals(e.Button, this.tbStats))
			{
				this.miStatsClick(null, null);
			}
			if (object.Equals(e.Button, this.tbPrev))
			{
				this.tbPrevClick(null, null);
			}
			if (object.Equals(e.Button, this.tbNext))
			{
				this.tbNextClick(null, null);
			}
		}

		private void TfmGEDKeeper_Resize(object sender, EventArgs e)
		{
			this.StatusBar.Panels[0].Width = base.Width - 50;
		}

		private void TfmGEDKeeper_Closed(object sender, EventArgs e)
		{
			this.FOptions.MWinRect = this.CheckFormRect(this);
			this.FOptions.MWinState = base.WindowState;
			VCLUtils.HtmlHelp(IntPtr.Zero, null, 18u, 0u);
			this.FNamesTable.SaveToFile(TGKSys.GetAppPath() + "GEDKeeper.nms");
			this.FNamesTable.Dispose();
			this.FOptions.SaveToFile(TGKSys.GetAppPath() + "GEDKeeper2.ini");
			this.FOptions.Dispose();
		}

		private void TfmGEDKeeper_Closing(object sender, CancelEventArgs e)
		{
			Form[] mdiChildren = base.MdiChildren;
			int i = ((mdiChildren != null) ? mdiChildren.Length : 0) - 1;
			if (i >= 0)
			{
				while (true)
				{
					if (base.MdiChildren[i] is TfmBase)
					{
						TfmBase @base = base.MdiChildren[i] as TfmBase;
						if (!@base.CheckModified())
						{
							break;
						}
					}
					i--;
					if (i == -1)
					{
						goto IL_50;
					}
				}
				e.Cancel = true;
				return;
			}
			IL_50:
			this.FOptions.ClearLastBases();
			Form[] mdiChildren2 = base.MdiChildren;
			i = ((mdiChildren2 != null) ? mdiChildren2.Length : 0) - 1;
			if (i >= 0)
			{
				do
				{
					if (base.MdiChildren[i] is TfmBase)
					{
						TfmBase @base = base.MdiChildren[i] as TfmBase;
						TGlobalOptions.TBaseWin lb = this.FOptions.AddLastBase();
						lb.FileName = @base.FileName;
						lb.WinRect = this.CheckFormRect(@base);
						lb.WinState = @base.WindowState;
						TObjectHelper.Free(@base);
					}
					else
					{
						TObjectHelper.Free(base.MdiChildren[i]);
					}
					i--;
				}
				while (i != -1);
			}
		}

		private void TfmGEDKeeper_DragEnter(object sender, DragEventArgs e)
		{
			if (e.Data.GetDataPresent(DataFormats.FileDrop))
			{
				e.Effect = DragDropEffects.Copy;
			}
			else
			{
				e.Effect = DragDropEffects.None;
			}
		}

		private void TfmGEDKeeper_DragDrop(object sender, DragEventArgs e)
		{
			try
			{
				Array a = e.Data.GetData(DataFormats.FileDrop) as Array;
				if (a != null)
				{
					int arg_23_0 = 0;
					int num = a.Length - 1;
					int i = arg_23_0;
					if (num >= i)
					{
						num++;
						do
						{
							string fn = a.GetValue(i).ToString();
							this.CreateBase(fn);
							i++;
						}
						while (i != num);
					}
				}
			}
			catch (Exception E)
			{
			}
		}

		protected override void WndProc(ref Message m)
		{
			base.WndProc(ref m);
			if (m.Msg == 1135)
			{
				if (this.fmCalcWidget != null && this.fmCalcWidget.Visible)
				{
					VCLUtils.EnableWindow((uint)this.fmCalcWidget.Handle.ToInt32(), (LongBool)(-1));
				}
			}
			else
			{
				int arg_4F_0 = m.Msg;
			}
		}

		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				TMapBrowser.GeoDone();
			}
			base.Dispose(Disposing);
		}

		public TfmGEDKeeper()
		{
			this.InitializeComponent();
			TMapBrowser.GeoInit();
			GKL.fmGEDKeeper = this;
		}

		public TfmBase GetCurrentFile()
		{
			TfmBase Result = ((base.ActiveMdiChild is TfmBase) ? (base.ActiveMdiChild as TfmBase) : null);
			return Result;
		}

		public string GetCurrentFileName()
		{
			TfmBase cb = this.GetCurrentFile();
			string Result;
			if (cb == null)
			{
				Result = "";
			}
			else
			{
				Result = cb.FileName;
			}
			return Result;
		}
		public void AddMRU([In] string aFileName)
		{
			int idx = this.FOptions.MRUFiles.IndexOf(aFileName);
			if (idx < 0)
			{
				this.FOptions.MRUFiles.Insert(0, aFileName);
			}
			else
			{
				if (idx > 0)
				{
					this.FOptions.MRUFiles.Delete(idx);
					this.FOptions.MRUFiles.Insert(0, aFileName);
				}
			}
			this.UpdateMRU();
		}
		public TfmBase CreateBase([In] string aFileName)
		{
			TfmBase Result = new TfmBase();
			Result.MdiParent = this;
			Result.Show();
			if (BDSSystem.WStrCmp(aFileName, "") != 0 && File.Exists(aFileName))
			{
				Result.FileLoad(aFileName);
			}
			else
			{
				Result.FileNew();
			}
			return Result;
		}
		public void UpdateControls(bool ForceDeactivate)
		{
			try
			{
				TfmBase cur_base;
				if (ForceDeactivate)
				{
					cur_base = null;
				}
				else
				{
					cur_base = this.GetCurrentFile();
				}
				TGEDCOMRecord.TGEDCOMRecordType rt;
				bool base_en;
				if (cur_base == null)
				{
					rt = TGEDCOMRecord.TGEDCOMRecordType.rtNone;
					base_en = false;
				}
				else
				{
					rt = (TGEDCOMRecord.TGEDCOMRecordType)(cur_base.PageRecords.SelectedIndex + 1);
					base_en = true;
				}
				this.miFileClose.Enabled = base_en;
				this.miFileSave.Enabled = base_en;
				this.tbFileSave.Enabled = this.miFileSave.Enabled;
				this.miFileProperties.Enabled = base_en;
				this.miExportToWeb.Enabled = base_en;
				this.miExportToExcelFile.Enabled = base_en;
				this.miExportToExcelApp.Enabled = base_en;
				this.miTreeTools.Enabled = base_en;
				this.miStreamInput.Enabled = base_en;
				this.miRecordAdd.Enabled = base_en;
				this.tbRecordAdd.Enabled = this.miRecordAdd.Enabled;
				this.miRecordEdit.Enabled = base_en;
				this.tbRecordEdit.Enabled = this.miRecordEdit.Enabled;
				this.miRecordDelete.Enabled = base_en;
				this.tbRecordDelete.Enabled = this.miRecordDelete.Enabled;
				bool indiv_en = base_en && rt == TGEDCOMRecord.TGEDCOMRecordType.rtIndividual;
				this.miFilter.Enabled = indiv_en;
				this.tbFilter.Enabled = this.miFilter.Enabled;
				this.miStats.Enabled = base_en;
				this.tbStats.Enabled = this.miStats.Enabled;
				this.miTreeAncestors.Enabled = indiv_en;
				this.tbTreeAncestors.Enabled = this.miTreeAncestors.Enabled;
				this.miTreeDescendants.Enabled = indiv_en;
				this.tbTreeDescendants.Enabled = this.miTreeDescendants.Enabled;
				this.miTreeBoth.Enabled = indiv_en;
				this.tbTreeBoth.Enabled = this.miTreeBoth.Enabled;
				this.miPedigree.Enabled = indiv_en;
				this.tbPedigree.Enabled = this.miPedigree.Enabled;
				this.miPedigree_dAboville.Enabled = indiv_en;
				this.miPedigree_Konovalov.Enabled = indiv_en;
				this.miOrganizer.Enabled = base_en;
				this.miScripts.Enabled = base_en;
				this.tbPrev.Enabled = (cur_base != null && cur_base.Backman.CanBackward());
				this.tbNext.Enabled = (cur_base != null && cur_base.Backman.CanForward());
				bool test_funcs = TGenEngine.IsDevComp();
				this.miUndo.Enabled = (test_funcs && cur_base != null && cur_base.Undoman.CanUndo());
				this.tbUndo.Enabled = this.miUndo.Enabled;
				this.miRedo.Enabled = (test_funcs && cur_base != null && cur_base.Undoman.CanRedo());
				this.tbRedo.Enabled = this.miRedo.Enabled;
				if (cur_base != null)
				{
					string st = GKL.LSList[50] + ": " + cur_base.FCounts[(int)rt].Total.ToString();
					if (rt == TGEDCOMRecord.TGEDCOMRecordType.rtIndividual)
					{
						st = string.Concat(new string[]
						{
							st, 
							", ", 
							GKL.LSList[51], 
							": ", 
							cur_base.FCounts[(int)rt].Filtered.ToString()
						});
					}
					this.StatusBar.Panels[0].Text = st;
				}
				this.StatusBar.Invalidate();
			}
			catch (Exception E)
			{
				TGKSys.LogWrite("GKMain.UpdateControls(): " + E.Message);
			}
		}

		public void ShowHelpTopic(string aTopic)
		{
			string fns = TGKSys.GetAppPath() + "GEDKeeper.chm" + aTopic;
			VCLUtils.HtmlHelp(this.Handle, fns, 0u, 0u);
		}

		void ILocalization.SetLang()
		{
			this.miFile.Text = GKL.LSList[0];
			this.miEdit.Text = GKL.LSList[1];
			this.miPedigree.Text = GKL.LSList[2];
			this.miService.Text = GKL.LSList[3];
			this.miWindow.Text = GKL.LSList[4];
			this.miHelp.Text = GKL.LSList[5];
			this.miFileNew.Text = GKL.LSList[6];
			this.miFileLoad.Text = GKL.LSList[7];
			this.miMRUFiles.Text = GKL.LSList[8];
			this.miFileSave.Text = GKL.LSList[9];
			this.miFileClose.Text = GKL.LSList[10];
			this.miFileProperties.Text = GKL.LSList[11];
			this.miExport.Text = GKL.LSList[12];
			this.miExportToWeb.Text = GKL.LSList[13];
			this.miExportToExcelApp.Text = GKL.LSList[14];
			this.miExportToExcelFile.Text = GKL.LSList[15];
			this.miExit.Text = GKL.LSList[16];
			this.miUndo.Text = GKL.LSList[17];
			this.miRedo.Text = GKL.LSList[18];
			this.miRecordAdd.Text = GKL.LSList[19];
			this.miRecordEdit.Text = GKL.LSList[20];
			this.miRecordDelete.Text = GKL.LSList[21];
			this.miStreamInput.Text = GKL.LSList[22] + "...";
			this.miTreeAncestors.Text = GKL.LSList[23];
			this.miTreeDescendants.Text = GKL.LSList[24];
			this.miTreeBoth.Text = GKL.LSList[25];
			this.miPedigree_dAboville.Text = GKL.LSList[26];
			this.miPedigree_Konovalov.Text = GKL.LSList[27];
			this.miMap.Text = GKL.LSList[28] + "...";
			this.miStats.Text = GKL.LSList[29] + "...";
			this.miCalc.Text = GKL.LSList[30] + "...";
			this.miNamesBook.Text = GKL.LSList[31] + "...";
			this.miCalendar.Text = GKL.LSList[32] + "...";
			this.miTimeLine.Text = GKL.LSList[33] + "...";
			this.miOrganizer.Text = GKL.LSList[34] + "...";
			this.miScripts.Text = GKL.LSList[35];
			this.miTreeTools.Text = GKL.LSList[37];
			this.miFilter.Text = GKL.LSList[38] + "...";
			this.miOptions.Text = GKL.LSList[39] + "...";
			this.miWinCascade.Text = GKL.LSList[40];
			this.miWinHTile.Text = GKL.LSList[41];
			this.miWinVTile.Text = GKL.LSList[42];
			this.miWinMinimize.Text = GKL.LSList[43];
			this.miWinArrange.Text = GKL.LSList[44];
			this.miGenResources.Text = GKL.LSList[45];
			this.miKinshipTerms.Text = GKL.LSList[46];
			this.miFAQ.Text = GKL.LSList[47];
			this.miContext.Text = GKL.LSList[48];
			this.miAbout.Text = GKL.LSList[49] + "...";
		}
		public void LoadLanguage(int LangCode)
		{
			Screen scr = Screen.PrimaryScreen;
			int i;
			if (LangCode != 1049)
			{
				bool loaded = false;
				int arg_21_0 = 0;
				int num = this.FOptions.LangsCount - 1;
				i = arg_21_0;
				if (num >= i)
				{
					num++;
					while ((int)this.FOptions.GetLang(i).Code != LangCode)
					{
						i++;
						if (i == num)
						{
							goto IL_66;
						}
					}
					loaded = TLangMan.LoadFromFile(this.FOptions.GetLang(i).FileName);
				}
				IL_66:
				if (!loaded)
				{
					LangCode = 1049;
				}
			}
			if (LangCode == 1049)
			{
				TLangMan.DefInit();
			}
			int arg_9A_0 = 0;
			Form[] mdiChildren = base.MdiChildren;
			int num2 = ((mdiChildren != null) ? mdiChildren.Length : 0) - 1;
			i = arg_9A_0;
			if (num2 >= i)
			{
				num2++;
				do
				{
					object obj;
					bool flag = VCLUtils.Supports(base.MdiChildren[i], typeof(ILocalization).TypeHandle, out obj);
					ILocalization intf = obj as ILocalization;
					if (flag)
					{
						intf.SetLang();
					}
					i++;
				}
				while (i != num2);
			}
			this.FOptions.InterfaceLang = (ushort)LangCode;
		}
		public void miFileSaveClick(object sender, EventArgs e)
		{
			TfmBase cur_base = this.GetCurrentFile();
			if (cur_base != null)
			{
				this.SaveDialog1.FileName = cur_base.FileName;
				if (this.SaveDialog1.ShowDialog() == DialogResult.OK)
				{
					cur_base.FileSave(this.SaveDialog1.FileName);
				}
			}
		}
		public DialogResult ShowModalEx(Form aForm, Form aPopupParent, bool KeepModeless)
		{
			if (KeepModeless)
			{
				VCLUtils.PostMessage((uint)GKL.fmGEDKeeper.Handle.ToInt32(), 1135u, 0, 0);
			}
			return aForm.ShowDialog();
		}
	}
}
