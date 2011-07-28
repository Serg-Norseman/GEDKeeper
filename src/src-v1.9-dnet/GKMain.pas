unit GKMain; {trans:none}

{$I GEDKeeper2.inc}

interface

uses
  System.IO, System.Drawing, System.ComponentModel, System.Windows.Forms,
  System.Resources, System.Text, VCLStub, GKEngine, GKCommon, GKBase, GKLangs,
  GKNamesBook, GKTimeLine, GKExpCalc, GKCalendar;

type
  TfmGEDKeeper = class(System.Windows.Forms.Form, ILocalization)
  strict private
    components: System.ComponentModel.IContainer;
    StatusBar: System.Windows.Forms.StatusBar;
    ToolBar1: System.Windows.Forms.ToolBar;
    tbFileNew: System.Windows.Forms.ToolBarButton;
    tbFileLoad: System.Windows.Forms.ToolBarButton;
    tbFileSave: System.Windows.Forms.ToolBarButton;
    TBS1: System.Windows.Forms.ToolBarButton;
    tbRecordAdd: System.Windows.Forms.ToolBarButton;
    tbRecordEdit: System.Windows.Forms.ToolBarButton;
    tbRecordDelete: System.Windows.Forms.ToolBarButton;
    TBS2: System.Windows.Forms.ToolBarButton;
    tbFilter: System.Windows.Forms.ToolBarButton;
    TBS3: System.Windows.Forms.ToolBarButton;
    tbTreeAncestors: System.Windows.Forms.ToolBarButton;
    tbTreeDescendants: System.Windows.Forms.ToolBarButton;
    TBS4: System.Windows.Forms.ToolBarButton;
    tbPedigree: System.Windows.Forms.ToolBarButton;
    TBS6: System.Windows.Forms.ToolBarButton;
    tbStats: System.Windows.Forms.ToolBarButton;
    TBS5: System.Windows.Forms.ToolBarButton;
    tbPrev: System.Windows.Forms.ToolBarButton;
    tbNext: System.Windows.Forms.ToolBarButton;
    MainMenu1: System.Windows.Forms.MainMenu;
    miFile: System.Windows.Forms.MenuItem;
    miFileNew: System.Windows.Forms.MenuItem;
    miFileLoad: System.Windows.Forms.MenuItem;
    miMRUFiles: System.Windows.Forms.MenuItem;
    miFileSave: System.Windows.Forms.MenuItem;
    miFileClose: System.Windows.Forms.MenuItem;
    N1: System.Windows.Forms.MenuItem;
    miFileProperties: System.Windows.Forms.MenuItem;
    N2: System.Windows.Forms.MenuItem;
    miExportToWeb: System.Windows.Forms.MenuItem;
    miExportToExcelFile: System.Windows.Forms.MenuItem;
    N3: System.Windows.Forms.MenuItem;
    miTreeTools: System.Windows.Forms.MenuItem;
    N4: System.Windows.Forms.MenuItem;
    miExit: System.Windows.Forms.MenuItem;
    miEdit: System.Windows.Forms.MenuItem;
    miRecordAdd: System.Windows.Forms.MenuItem;
    miRecordEdit: System.Windows.Forms.MenuItem;
    miRecordDelete: System.Windows.Forms.MenuItem;
    N5: System.Windows.Forms.MenuItem;
    miStreamInput: System.Windows.Forms.MenuItem;
    N6: System.Windows.Forms.MenuItem;
    miFilter: System.Windows.Forms.MenuItem;
    N7: System.Windows.Forms.MenuItem;
    miOptions: System.Windows.Forms.MenuItem;
    miPedigree: System.Windows.Forms.MenuItem;
    miTreeAncestors: System.Windows.Forms.MenuItem;
    miTreeDescendants: System.Windows.Forms.MenuItem;
    N8: System.Windows.Forms.MenuItem;
    miPedigree_dAboville: System.Windows.Forms.MenuItem;
    miPedigree_Konovalov: System.Windows.Forms.MenuItem;
    N9: System.Windows.Forms.MenuItem;
    miMap: System.Windows.Forms.MenuItem;
    N10: System.Windows.Forms.MenuItem;
    miStats: System.Windows.Forms.MenuItem;
    miWindow: System.Windows.Forms.MenuItem;
    miWinCascade: System.Windows.Forms.MenuItem;
    miWinHTile: System.Windows.Forms.MenuItem;
    miWinVTile: System.Windows.Forms.MenuItem;
    miWinMinimize: System.Windows.Forms.MenuItem;
    miWinArrange: System.Windows.Forms.MenuItem;
    miHelp: System.Windows.Forms.MenuItem;
    miGenResources: System.Windows.Forms.MenuItem;
    miKinshipTerms: System.Windows.Forms.MenuItem;
    miContext: System.Windows.Forms.MenuItem;
    N11: System.Windows.Forms.MenuItem;
    miAbout: System.Windows.Forms.MenuItem;
    MenuMRU: System.Windows.Forms.ContextMenu;
    MenuPedigree: System.Windows.Forms.ContextMenu;
    miPedigree_dAboville2: System.Windows.Forms.MenuItem;
    miPedigree_Konovalov2: System.Windows.Forms.MenuItem;
    OpenDialog1: System.Windows.Forms.OpenFileDialog;
    SaveDialog1: System.Windows.Forms.SaveFileDialog;
    TBS7: System.Windows.Forms.ToolBarButton;
    tbUndo: System.Windows.Forms.ToolBarButton;
    tbRedo: System.Windows.Forms.ToolBarButton;
    miFAQ: System.Windows.Forms.MenuItem;
    ImageList_Shields: System.Windows.Forms.ImageList;
    miOrganizer: System.Windows.Forms.MenuItem;
    miService: System.Windows.Forms.MenuItem;
    N12: System.Windows.Forms.MenuItem;
    miUndo: System.Windows.Forms.MenuItem;
    miRedo: System.Windows.Forms.MenuItem;
    miScripts: System.Windows.Forms.MenuItem;
    miExport: System.Windows.Forms.MenuItem;
    miExportToExcelApp: System.Windows.Forms.MenuItem;
    miTreeBoth: System.Windows.Forms.MenuItem;
    tbTreeBoth: System.Windows.Forms.ToolBarButton;
    StatusBarPanel1: System.Windows.Forms.StatusBarPanel;
    StatusBarPanel2: System.Windows.Forms.StatusBarPanel;
    ToolTip1: System.Windows.Forms.ToolTip;

    FNamesTable: TNamesTable;
    FOptions: TGlobalOptions;

    procedure MRUFileClick(sender: System.Object; e: System.EventArgs);
    procedure UpdateMRU();

    function CheckFormRect(aForm: System.Windows.Forms.Form): TRect;

    procedure InitializeComponent;
    procedure FormCreate(sender: System.Object; e: System.EventArgs);
    //procedure StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure StatusBarDblClick(sender: System.Object; e: System.EventArgs);
    procedure miExitClick(sender: System.Object; e: System.EventArgs);
    procedure miExportToWebClick(sender: System.Object; e: System.EventArgs);
    procedure miExportToExcelFileClick(sender: System.Object; e: System.EventArgs);
    procedure miFilePropertiesClick(sender: System.Object; e: System.EventArgs);
    procedure miStreamInputClick(sender: System.Object; e: System.EventArgs);
    procedure miScriptsClick(sender: System.Object; e: System.EventArgs);
    procedure miTreeToolsClick(sender: System.Object; e: System.EventArgs);
    procedure miOptionsClick(sender: System.Object; e: System.EventArgs);
    procedure miFileCloseClick(sender: System.Object; e: System.EventArgs);
    procedure miMapClick(sender: System.Object; e: System.EventArgs);
    procedure miOrganizerClick(sender: System.Object; e: System.EventArgs);
    procedure miTimeLineClick(sender: System.Object; e: System.EventArgs);
    procedure miCalendarClick(sender: System.Object; e: System.EventArgs);
    procedure miNamesBookClick(sender: System.Object; e: System.EventArgs);
    procedure miCalcClick(sender: System.Object; e: System.EventArgs);
    procedure miAboutClick(sender: System.Object; e: System.EventArgs);
    procedure miGenResourcesClick(sender: System.Object; e: System.EventArgs);
    procedure miKinshipTermsClick(sender: System.Object; e: System.EventArgs);
    procedure miFAQClick(sender: System.Object; e: System.EventArgs);
    procedure miContextClick(sender: System.Object; e: System.EventArgs);
    procedure miWinCascadeClick(sender: System.Object; e: System.EventArgs);
    procedure miWinHTileClick(sender: System.Object; e: System.EventArgs);
    procedure miWinVTileClick(sender: System.Object; e: System.EventArgs);
    procedure miWinMinimizeClick(sender: System.Object; e: System.EventArgs);
    procedure miWinArrangeClick(sender: System.Object; e: System.EventArgs);
    procedure miFilterClick(sender: System.Object; e: System.EventArgs);
    procedure tbPrevClick(sender: System.Object; e: System.EventArgs);
    procedure tbNextClick(sender: System.Object; e: System.EventArgs);
    procedure miStatsClick(sender: System.Object; e: System.EventArgs);
    procedure miFileNewClick(sender: System.Object; e: System.EventArgs);
    procedure miFileLoadClick(sender: System.Object; e: System.EventArgs);
    procedure miUndoClick(sender: System.Object; e: System.EventArgs);
    procedure miRedoClick(sender: System.Object; e: System.EventArgs);
    procedure miTreeAncestorsClick(sender: System.Object; e: System.EventArgs);
    procedure miTreeDescendantsClick(sender: System.Object; e: System.EventArgs);
    procedure miPedigree_dAbovilleClick(sender: System.Object; e: System.EventArgs);
    procedure miPedigree_KonovalovClick(sender: System.Object; e: System.EventArgs);
    procedure miRecordAddClick(sender: System.Object; e: System.EventArgs);
    procedure miRecordEditClick(sender: System.Object; e: System.EventArgs);
    procedure miRecordDeleteClick(sender: System.Object; e: System.EventArgs);
    procedure miExportToExcelAppClick(sender: System.Object; e: System.EventArgs);
    procedure miTreeBothClick(sender: System.Object; e: System.EventArgs);
    procedure FormShow(sender: System.Object; e: System.EventArgs);
    procedure ToolBar1_ButtonClick(sender: System.Object; e: System.Windows.Forms.ToolBarButtonClickEventArgs);
    procedure TfmGEDKeeper_Resize(sender: System.Object; e: System.EventArgs);
    procedure TfmGEDKeeper_Closed(sender: System.Object; e: System.EventArgs);
    procedure TfmGEDKeeper_Closing(sender: System.Object; e: System.ComponentModel.CancelEventArgs);
    procedure TfmGEDKeeper_DragEnter(sender: System.Object; e: System.Windows.Forms.DragEventArgs);
    procedure TfmGEDKeeper_DragDrop(sender: System.Object; e: System.Windows.Forms.DragEventArgs);
  strict protected
    procedure WndProc(var m: System.Windows.Forms.Message); override;
    procedure Dispose(Disposing: Boolean); override;
  public
    ImageList_Buttons: System.Windows.Forms.ImageList;
    miCalc: System.Windows.Forms.MenuItem;
    miNamesBook: System.Windows.Forms.MenuItem;
    miCalendar: System.Windows.Forms.MenuItem;
    miTimeLine: System.Windows.Forms.MenuItem;

    fmTimeLine: TfmTimeLine;
    fmCalendar: TfmCalendar;
    fmNamesBook: TfmNamesBook;
    fmCalcWidget: TfmCalcWidget;

    const
      WM_KEEPMODELESS = WM_USER + 111;

    constructor Create;

    property NamesTable: TNamesTable read FNamesTable;
    property Options: TGlobalOptions read FOptions;

    function GetCurrentFile(): TfmBase;
    function GetCurrentFileName(): string;

    procedure AddMRU(const aFileName: string);
    function CreateBase(const aFileName: string): TfmBase;
    procedure UpdateControls(ForceDeactivate: Boolean = False);

    procedure ShowHelpTopic(aTopic: string = '');

    procedure SetLang();
    procedure LoadLanguage(LangCode: Integer);

    procedure miFileSaveClick(sender: System.Object; e: System.EventArgs);

    function ShowModalEx(aForm: System.Windows.Forms.Form;
      aPopupParent: System.Windows.Forms.Form = nil;
      KeepModeless: Boolean = False): System.Windows.Forms.DialogResult;
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TfmGEDKeeper))]

var
  fmGEDKeeper: TfmGEDKeeper;

//procedure RunInstance();

implementation

uses
  GedCom551, GKUtils, GKCtrls, GKAbout, GKOptions, GKMapBrowser;

procedure TfmGEDKeeper.FormCreate(sender: System.Object; e: System.EventArgs);
begin
  TGKUtils.LogInit(TGKUtils.GetAppPath() + 'GEDKeeper.log');

  FOptions := TGlobalOptions.Create;
  FOptions.LoadFromFile(TGKUtils.GetAppPath() + 'GEDKeeper2.ini');
  FOptions.FindLanguages();

  ///
  if (FOptions.MWinRect.Left <> -1) and (FOptions.MWinRect.Top <> -1)
  and (FOptions.MWinRect.Right <> -1) and (FOptions.MWinRect.Bottom <> -1)
  then begin
    Self.Left := FOptions.MWinRect.Left;
    Self.Top := FOptions.MWinRect.Top;
    Self.Width := FOptions.MWinRect.Right;
    Self.Height := FOptions.MWinRect.Bottom;
  end else begin
    Self.Left := (System.Windows.Forms.Screen.PrimaryScreen.WorkingArea.Width - 800) div 2;
    Self.Top := (System.Windows.Forms.Screen.PrimaryScreen.WorkingArea.Height - 600) div 2;
    Self.Width := 800;
    Self.Height := 600;
  end;
  Self.WindowState := FOptions.MWinState;
  ///

  FNamesTable := TNamesTable.Create;
  FNamesTable.LoadFromFile(TGKUtils.GetAppPath() + 'GEDKeeper.nms');

  LoadLanguage(FOptions.InterfaceLang);

  UpdateMRU();
  UpdateControls();
end;

procedure TfmGEDKeeper.FormShow(sender: System.Object; e: System.EventArgs);
var
  i: Integer;
  lb: TGlobalOptions.TBaseWin;
  base: TfmBase;
begin
  for i := 0 to FOptions.LastBasesCount - 1 do begin
    lb := FOptions.LastBases[i];

    if System.IO.File.Exists(lb.FileName) then begin
      base := CreateBase(lb.FileName);
      base.Left := lb.WinRect.Left;
      base.Top := lb.WinRect.Top;
      base.Width := lb.WinRect.Right;
      base.Height := lb.WinRect.Bottom;
      base.WindowState := lb.WinState;
    end;
  end;
end;

procedure TfmGEDKeeper.TfmGEDKeeper_Closing(sender: System.Object; e: System.ComponentModel.CancelEventArgs);
var
  i: Integer;
  base: TfmBase;
  lb: TGlobalOptions.TBaseWin;
begin
  for i := High(MdiChildren) downto 0 do begin
    if (MdiChildren[i] is TfmBase) then begin
      base := TfmBase(MDIChildren[i]);

      if not(base.CheckModified()) then begin
        e.Cancel := True;
        Exit;
      end;
    end;
  end;

  FOptions.ClearLastBases();

  for i := High(MdiChildren) downto 0 do begin
    if (MdiChildren[i] is TfmBase) then begin
      base := TfmBase(MdiChildren[i]);

      lb := FOptions.AddLastBase();
      lb.FileName := base.FileName;
      lb.WinRect := CheckFormRect(base);
      lb.WinState := base.WindowState;

      base.Free;
    end else MDIChildren[i].Free;
  end;
end;

procedure TfmGEDKeeper.TfmGEDKeeper_Closed(sender: System.Object; e: System.EventArgs);
begin
  FOptions.MWinRect := CheckFormRect(Self);
  FOptions.MWinState := Self.WindowState;

  ///

  HtmlHelp(0, nil, HH_CLOSE_ALL, 0);

  FNamesTable.SaveToFile(TGKUtils.GetAppPath() + 'GEDKeeper.nms');
  FNamesTable.Destroy;

  FOptions.SaveToFile(TGKUtils.GetAppPath() + 'GEDKeeper2.ini');
  FOptions.Destroy;
end;

function TfmGEDKeeper.GetCurrentFile(): TfmBase;
begin
  if (ActiveMdiChild is TfmBase)
  then Result := TfmBase(ActiveMdiChild)
  else Result := nil;
end;

function TfmGEDKeeper.GetCurrentFileName(): string;
var
  cb: TfmBase;
begin
  cb := GetCurrentFile();
  if (cb = nil)
  then Result := ''
  else Result := cb.FileName;
end;

procedure TfmGEDKeeper.InitializeComponent;
type
  TArrayOfSystem_Windows_Forms_StatusBarPanel = array of System.Windows.Forms.StatusBarPanel;
  TArrayOfSystem_Windows_Forms_ToolBarButton = array of System.Windows.Forms.ToolBarButton;
  TArrayOfSystem_Windows_Forms_MenuItem = array of System.Windows.Forms.MenuItem;
var
  resources: System.Resources.ResourceManager;
begin
  Self.components := System.ComponentModel.Container.Create;
  resources := System.Resources.ResourceManager.Create(TypeOf(TfmGEDKeeper));
  Self.StatusBar := System.Windows.Forms.StatusBar.Create;
  Self.StatusBarPanel1 := System.Windows.Forms.StatusBarPanel.Create;
  Self.StatusBarPanel2 := System.Windows.Forms.StatusBarPanel.Create;
  Self.ImageList_Buttons := System.Windows.Forms.ImageList.Create(Self.components);
  Self.ToolBar1 := System.Windows.Forms.ToolBar.Create;
  Self.tbFileNew := System.Windows.Forms.ToolBarButton.Create;
  Self.tbFileLoad := System.Windows.Forms.ToolBarButton.Create;
  Self.MenuMRU := System.Windows.Forms.ContextMenu.Create;
  Self.tbFileSave := System.Windows.Forms.ToolBarButton.Create;
  Self.TBS1 := System.Windows.Forms.ToolBarButton.Create;
  Self.tbRecordAdd := System.Windows.Forms.ToolBarButton.Create;
  Self.tbRecordEdit := System.Windows.Forms.ToolBarButton.Create;
  Self.tbRecordDelete := System.Windows.Forms.ToolBarButton.Create;
  Self.TBS2 := System.Windows.Forms.ToolBarButton.Create;
  Self.tbUndo := System.Windows.Forms.ToolBarButton.Create;
  Self.tbRedo := System.Windows.Forms.ToolBarButton.Create;
  Self.TBS3 := System.Windows.Forms.ToolBarButton.Create;
  Self.tbFilter := System.Windows.Forms.ToolBarButton.Create;
  Self.TBS4 := System.Windows.Forms.ToolBarButton.Create;
  Self.tbTreeAncestors := System.Windows.Forms.ToolBarButton.Create;
  Self.tbTreeDescendants := System.Windows.Forms.ToolBarButton.Create;
  Self.tbTreeBoth := System.Windows.Forms.ToolBarButton.Create;
  Self.TBS5 := System.Windows.Forms.ToolBarButton.Create;
  Self.tbPedigree := System.Windows.Forms.ToolBarButton.Create;
  Self.MenuPedigree := System.Windows.Forms.ContextMenu.Create;
  Self.miPedigree_dAboville2 := System.Windows.Forms.MenuItem.Create;
  Self.miPedigree_Konovalov2 := System.Windows.Forms.MenuItem.Create;
  Self.TBS6 := System.Windows.Forms.ToolBarButton.Create;
  Self.tbStats := System.Windows.Forms.ToolBarButton.Create;
  Self.TBS7 := System.Windows.Forms.ToolBarButton.Create;
  Self.tbPrev := System.Windows.Forms.ToolBarButton.Create;
  Self.tbNext := System.Windows.Forms.ToolBarButton.Create;
  Self.MainMenu1 := System.Windows.Forms.MainMenu.Create;
  Self.miFile := System.Windows.Forms.MenuItem.Create;
  Self.miFileNew := System.Windows.Forms.MenuItem.Create;
  Self.miFileLoad := System.Windows.Forms.MenuItem.Create;
  Self.miMRUFiles := System.Windows.Forms.MenuItem.Create;
  Self.miFileSave := System.Windows.Forms.MenuItem.Create;
  Self.miFileClose := System.Windows.Forms.MenuItem.Create;
  Self.N1 := System.Windows.Forms.MenuItem.Create;
  Self.miFileProperties := System.Windows.Forms.MenuItem.Create;
  Self.N2 := System.Windows.Forms.MenuItem.Create;
  Self.miExport := System.Windows.Forms.MenuItem.Create;
  Self.miExportToWeb := System.Windows.Forms.MenuItem.Create;
  Self.miExportToExcelApp := System.Windows.Forms.MenuItem.Create;
  Self.miExportToExcelFile := System.Windows.Forms.MenuItem.Create;
  Self.N3 := System.Windows.Forms.MenuItem.Create;
  Self.miExit := System.Windows.Forms.MenuItem.Create;
  Self.miEdit := System.Windows.Forms.MenuItem.Create;
  Self.miUndo := System.Windows.Forms.MenuItem.Create;
  Self.miRedo := System.Windows.Forms.MenuItem.Create;
  Self.N4 := System.Windows.Forms.MenuItem.Create;
  Self.miRecordAdd := System.Windows.Forms.MenuItem.Create;
  Self.miRecordEdit := System.Windows.Forms.MenuItem.Create;
  Self.miRecordDelete := System.Windows.Forms.MenuItem.Create;
  Self.N5 := System.Windows.Forms.MenuItem.Create;
  Self.miStreamInput := System.Windows.Forms.MenuItem.Create;
  Self.miPedigree := System.Windows.Forms.MenuItem.Create;
  Self.miTreeAncestors := System.Windows.Forms.MenuItem.Create;
  Self.miTreeDescendants := System.Windows.Forms.MenuItem.Create;
  Self.miTreeBoth := System.Windows.Forms.MenuItem.Create;
  Self.N6 := System.Windows.Forms.MenuItem.Create;
  Self.miPedigree_dAboville := System.Windows.Forms.MenuItem.Create;
  Self.miPedigree_Konovalov := System.Windows.Forms.MenuItem.Create;
  Self.N7 := System.Windows.Forms.MenuItem.Create;
  Self.miMap := System.Windows.Forms.MenuItem.Create;
  Self.N8 := System.Windows.Forms.MenuItem.Create;
  Self.miStats := System.Windows.Forms.MenuItem.Create;
  Self.miService := System.Windows.Forms.MenuItem.Create;
  Self.miCalc := System.Windows.Forms.MenuItem.Create;
  Self.miNamesBook := System.Windows.Forms.MenuItem.Create;
  Self.miCalendar := System.Windows.Forms.MenuItem.Create;
  Self.miTimeLine := System.Windows.Forms.MenuItem.Create;
  Self.miOrganizer := System.Windows.Forms.MenuItem.Create;
  Self.N9 := System.Windows.Forms.MenuItem.Create;
  Self.miScripts := System.Windows.Forms.MenuItem.Create;
  Self.miTreeTools := System.Windows.Forms.MenuItem.Create;
  Self.N10 := System.Windows.Forms.MenuItem.Create;
  Self.miFilter := System.Windows.Forms.MenuItem.Create;
  Self.N11 := System.Windows.Forms.MenuItem.Create;
  Self.miOptions := System.Windows.Forms.MenuItem.Create;
  Self.miWindow := System.Windows.Forms.MenuItem.Create;
  Self.miWinCascade := System.Windows.Forms.MenuItem.Create;
  Self.miWinHTile := System.Windows.Forms.MenuItem.Create;
  Self.miWinVTile := System.Windows.Forms.MenuItem.Create;
  Self.miWinMinimize := System.Windows.Forms.MenuItem.Create;
  Self.miWinArrange := System.Windows.Forms.MenuItem.Create;
  Self.miHelp := System.Windows.Forms.MenuItem.Create;
  Self.miGenResources := System.Windows.Forms.MenuItem.Create;
  Self.miKinshipTerms := System.Windows.Forms.MenuItem.Create;
  Self.miFAQ := System.Windows.Forms.MenuItem.Create;
  Self.miContext := System.Windows.Forms.MenuItem.Create;
  Self.N12 := System.Windows.Forms.MenuItem.Create;
  Self.miAbout := System.Windows.Forms.MenuItem.Create;
  Self.OpenDialog1 := System.Windows.Forms.OpenFileDialog.Create;
  Self.SaveDialog1 := System.Windows.Forms.SaveFileDialog.Create;
  Self.ImageList_Shields := System.Windows.Forms.ImageList.Create(Self.components);
  Self.ToolTip1 := System.Windows.Forms.ToolTip.Create(Self.components);
  (System.ComponentModel.ISupportInitialize(Self.StatusBarPanel1)).BeginInit;
  (System.ComponentModel.ISupportInitialize(Self.StatusBarPanel2)).BeginInit;
  Self.SuspendLayout;
  // 
  // StatusBar
  // 
  Self.StatusBar.ImeMode := System.Windows.Forms.ImeMode.NoControl;
  Self.StatusBar.Location := System.Drawing.Point.Create(0, 729);
  Self.StatusBar.Name := 'StatusBar';
  Self.StatusBar.Panels.AddRange(TArrayOfSystem_Windows_Forms_StatusBarPanel.Create(Self.StatusBarPanel1, 
          Self.StatusBarPanel2));
  Self.StatusBar.ShowPanels := True;
  Self.StatusBar.Size := System.Drawing.Size.Create(896, 16);
  Self.StatusBar.TabIndex := 0;
  // 
  // StatusBarPanel1
  // 
  Self.StatusBarPanel1.Width := 50;
  // 
  // StatusBarPanel2
  // 
  Self.StatusBarPanel2.Width := 24;
  // 
  // ImageList_Buttons
  // 
  Self.ImageList_Buttons.ColorDepth := System.Windows.Forms.ColorDepth.Depth24Bit;
  Self.ImageList_Buttons.ImageSize := System.Drawing.Size.Create(20, 20);
  Self.ImageList_Buttons.ImageStream := (System.Windows.Forms.ImageListStreamer(resources.GetObject('I' +
    'mageList_Buttons.ImageStream')));
  Self.ImageList_Buttons.TransparentColor := System.Drawing.Color.Transparent;
  // 
  // ToolBar1
  // 
  Self.ToolBar1.Appearance := System.Windows.Forms.ToolBarAppearance.Flat;
  Self.ToolBar1.Buttons.AddRange(TArrayOfSystem_Windows_Forms_ToolBarButton.Create(Self.tbFileNew, 
          Self.tbFileLoad, Self.tbFileSave, Self.TBS1, Self.tbRecordAdd, Self.tbRecordEdit, 
          Self.tbRecordDelete, Self.TBS2, Self.tbUndo, Self.tbRedo, Self.TBS3, 
          Self.tbFilter, Self.TBS4, Self.tbTreeAncestors, Self.tbTreeDescendants, 
          Self.tbTreeBoth, Self.TBS5, Self.tbPedigree, Self.TBS6, Self.tbStats, 
          Self.TBS7, Self.tbPrev, Self.tbNext));
  Self.ToolBar1.ButtonSize := System.Drawing.Size.Create(27, 26);
  Self.ToolBar1.DropDownArrows := True;
  Self.ToolBar1.ImageList := Self.ImageList_Buttons;
  Self.ToolBar1.ImeMode := System.Windows.Forms.ImeMode.NoControl;
  Self.ToolBar1.Location := System.Drawing.Point.Create(0, 0);
  Self.ToolBar1.Name := 'ToolBar1';
  Self.ToolBar1.ShowToolTips := True;
  Self.ToolBar1.Size := System.Drawing.Size.Create(896, 32);
  Self.ToolBar1.TabIndex := 0;
  Include(Self.ToolBar1.ButtonClick, Self.ToolBar1_ButtonClick);
  // 
  // tbFileNew
  // 
  Self.tbFileNew.ImageIndex := 0;
  Self.tbFileNew.ToolTipText := 'Создать новый файл древа';
  // 
  // tbFileLoad
  // 
  Self.tbFileLoad.DropDownMenu := Self.MenuMRU;
  Self.tbFileLoad.ImageIndex := 1;
  Self.tbFileLoad.Style := System.Windows.Forms.ToolBarButtonStyle.DropDownButton;
  Self.tbFileLoad.ToolTipText := 'Открыть файл древа';
  // 
  // MenuMRU
  // 
  Self.MenuMRU.RightToLeft := System.Windows.Forms.RightToLeft.No;
  // 
  // tbFileSave
  // 
  Self.tbFileSave.ImageIndex := 2;
  Self.tbFileSave.ToolTipText := 'Сохранить файл древа';
  // 
  // TBS1
  // 
  Self.TBS1.Style := System.Windows.Forms.ToolBarButtonStyle.Separator;
  // 
  // tbRecordAdd
  // 
  Self.tbRecordAdd.ImageIndex := 3;
  Self.tbRecordAdd.ToolTipText := 'Добавить запись';
  // 
  // tbRecordEdit
  // 
  Self.tbRecordEdit.ImageIndex := 4;
  Self.tbRecordEdit.ToolTipText := 'Изменить запись';
  // 
  // tbRecordDelete
  // 
  Self.tbRecordDelete.ImageIndex := 5;
  Self.tbRecordDelete.ToolTipText := 'Удалить запись';
  // 
  // TBS2
  // 
  Self.TBS2.Style := System.Windows.Forms.ToolBarButtonStyle.Separator;
  // 
  // tbUndo
  // 
  Self.tbUndo.ImageIndex := 31;
  // 
  // tbRedo
  // 
  Self.tbRedo.ImageIndex := 32;
  // 
  // TBS3
  // 
  Self.TBS3.Style := System.Windows.Forms.ToolBarButtonStyle.Separator;
  // 
  // tbFilter
  // 
  Self.tbFilter.ImageIndex := 16;
  Self.tbFilter.ToolTipText := 'Фильтрация списка персон';
  // 
  // TBS4
  // 
  Self.TBS4.Style := System.Windows.Forms.ToolBarButtonStyle.Separator;
  // 
  // tbTreeAncestors
  // 
  Self.tbTreeAncestors.ImageIndex := 18;
  Self.tbTreeAncestors.ToolTipText := 'Сформировать древо предков';
  // 
  // tbTreeDescendants
  // 
  Self.tbTreeDescendants.ImageIndex := 19;
  Self.tbTreeDescendants.ToolTipText := 'Сформировать древо потомков';
  // 
  // tbTreeBoth
  // 
  Self.tbTreeBoth.ImageIndex := 34;
  Self.tbTreeBoth.ToolTipText := 'Сформировать полное древо';
  // 
  // TBS5
  // 
  Self.TBS5.Style := System.Windows.Forms.ToolBarButtonStyle.Separator;
  // 
  // tbPedigree
  // 
  Self.tbPedigree.DropDownMenu := Self.MenuPedigree;
  Self.tbPedigree.ImageIndex := 20;
  Self.tbPedigree.Style := System.Windows.Forms.ToolBarButtonStyle.DropDownButton;
  Self.tbPedigree.ToolTipText := 'Родословная роспись';
  // 
  // MenuPedigree
  // 
  Self.MenuPedigree.MenuItems.AddRange(TArrayOfSystem_Windows_Forms_MenuItem.Create(Self.miPedigree_dAboville2, 
          Self.miPedigree_Konovalov2));
  Self.MenuPedigree.RightToLeft := System.Windows.Forms.RightToLeft.No;
  // 
  // miPedigree_dAboville2
  // 
  Self.miPedigree_dAboville2.Index := 0;
  Self.miPedigree_dAboville2.Text := 'Роспись по д''Абовиллю';
  Include(Self.miPedigree_dAboville2.Click, Self.miPedigree_dAbovilleClick);
  // 
  // miPedigree_Konovalov2
  // 
  Self.miPedigree_Konovalov2.Index := 1;
  Self.miPedigree_Konovalov2.Text := 'Роспись по Коновалову';
  Include(Self.miPedigree_Konovalov2.Click, Self.miPedigree_KonovalovClick);
  // 
  // TBS6
  // 
  Self.TBS6.Style := System.Windows.Forms.ToolBarButtonStyle.Separator;
  // 
  // tbStats
  // 
  Self.tbStats.ImageIndex := 11;
  Self.tbStats.ToolTipText := 'Статистический анализ данных';
  // 
  // TBS7
  // 
  Self.TBS7.Style := System.Windows.Forms.ToolBarButtonStyle.Separator;
  // 
  // tbPrev
  // 
  Self.tbPrev.Enabled := False;
  Self.tbPrev.ImageIndex := 22;
  Self.tbPrev.ToolTipText := 'Предыдущая запись';
  // 
  // tbNext
  // 
  Self.tbNext.Enabled := False;
  Self.tbNext.ImageIndex := 23;
  Self.tbNext.ToolTipText := 'Следующая запись';
  // 
  // MainMenu1
  // 
  Self.MainMenu1.MenuItems.AddRange(TArrayOfSystem_Windows_Forms_MenuItem.Create(Self.miFile, 
          Self.miEdit, Self.miPedigree, Self.miService, Self.miWindow, Self.miHelp));
  // 
  // miFile
  // 
  Self.miFile.Index := 0;
  Self.miFile.MenuItems.AddRange(TArrayOfSystem_Windows_Forms_MenuItem.Create(Self.miFileNew, 
          Self.miFileLoad, Self.miMRUFiles, Self.miFileSave, Self.miFileClose, 
          Self.N1, Self.miFileProperties, Self.N2, Self.miExport, Self.N3, Self.miExit));
  Self.miFile.Text := 'Файл';
  // 
  // miFileNew
  // 
  Self.miFileNew.Index := 0;
  Self.miFileNew.Shortcut := System.Windows.Forms.Shortcut.CtrlN;
  Self.miFileNew.Text := 'Новый';
  Include(Self.miFileNew.Click, Self.miFileNewClick);
  // 
  // miFileLoad
  // 
  Self.miFileLoad.Index := 1;
  Self.miFileLoad.Shortcut := System.Windows.Forms.Shortcut.CtrlO;
  Self.miFileLoad.Text := 'Открыть...';
  Include(Self.miFileLoad.Click, Self.miFileLoadClick);
  // 
  // miMRUFiles
  // 
  Self.miMRUFiles.Enabled := False;
  Self.miMRUFiles.Index := 2;
  Self.miMRUFiles.Text := 'Открыть последний';
  // 
  // miFileSave
  // 
  Self.miFileSave.Index := 3;
  Self.miFileSave.Shortcut := System.Windows.Forms.Shortcut.CtrlS;
  Self.miFileSave.Text := 'Сохранить...';
  Include(Self.miFileSave.Click, Self.miFileSaveClick);
  // 
  // miFileClose
  // 
  Self.miFileClose.Index := 4;
  Self.miFileClose.Text := 'Закрыть';
  Include(Self.miFileClose.Click, Self.miFileCloseClick);
  // 
  // N1
  // 
  Self.N1.Index := 5;
  Self.N1.Text := '-';
  // 
  // miFileProperties
  // 
  Self.miFileProperties.Index := 6;
  Self.miFileProperties.Text := 'Свойства файла...';
  Include(Self.miFileProperties.Click, Self.miFilePropertiesClick);
  // 
  // N2
  // 
  Self.N2.Index := 7;
  Self.N2.Text := '-';
  // 
  // miExport
  // 
  Self.miExport.Index := 8;
  Self.miExport.MenuItems.AddRange(TArrayOfSystem_Windows_Forms_MenuItem.Create(Self.miExportToWeb, 
          Self.miExportToExcelApp, Self.miExportToExcelFile));
  Self.miExport.Text := 'Экспорт';
  // 
  // miExportToWeb
  // 
  Self.miExportToWeb.Index := 0;
  Self.miExportToWeb.Text := 'Экспорт в Web...';
  Include(Self.miExportToWeb.Click, Self.miExportToWebClick);
  // 
  // miExportToExcelApp
  // 
  Self.miExportToExcelApp.Index := 1;
  Self.miExportToExcelApp.Text := 'Экспорт в Excel...';
  Include(Self.miExportToExcelApp.Click, Self.miExportToExcelAppClick);
  // 
  // miExportToExcelFile
  // 
  Self.miExportToExcelFile.Index := 2;
  Self.miExportToExcelFile.Text := 'Экспорт в Excel-файл...';
  Include(Self.miExportToExcelFile.Click, Self.miExportToExcelFileClick);
  // 
  // N3
  // 
  Self.N3.Index := 9;
  Self.N3.Text := '-';
  // 
  // miExit
  // 
  Self.miExit.Index := 10;
  Self.miExit.Shortcut := System.Windows.Forms.Shortcut.CtrlX;
  Self.miExit.Text := 'Выход';
  Include(Self.miExit.Click, Self.miExitClick);
  // 
  // miEdit
  // 
  Self.miEdit.Index := 1;
  Self.miEdit.MenuItems.AddRange(TArrayOfSystem_Windows_Forms_MenuItem.Create(Self.miUndo, 
          Self.miRedo, Self.N4, Self.miRecordAdd, Self.miRecordEdit, Self.miRecordDelete, 
          Self.N5, Self.miStreamInput));
  Self.miEdit.Text := 'Правка';
  // 
  // miUndo
  // 
  Self.miUndo.Index := 0;
  Self.miUndo.Shortcut := System.Windows.Forms.Shortcut.CtrlZ;
  Self.miUndo.Text := 'Отменить';
  Include(Self.miUndo.Click, Self.miUndoClick);
  // 
  // miRedo
  // 
  Self.miRedo.Index := 1;
  Self.miRedo.Shortcut := System.Windows.Forms.Shortcut.CtrlY;
  Self.miRedo.Text := 'Вернуть';
  Include(Self.miRedo.Click, Self.miRedoClick);
  // 
  // N4
  // 
  Self.N4.Index := 2;
  Self.N4.Text := '-';
  // 
  // miRecordAdd
  // 
  Self.miRecordAdd.Index := 3;
  Self.miRecordAdd.Shortcut := System.Windows.Forms.Shortcut.CtrlI;
  Self.miRecordAdd.Text := 'Добавить запись';
  Include(Self.miRecordAdd.Click, Self.miRecordAddClick);
  // 
  // miRecordEdit
  // 
  Self.miRecordEdit.Index := 4;
  Self.miRecordEdit.Text := 'Изменить запись';
  Include(Self.miRecordEdit.Click, Self.miRecordEditClick);
  // 
  // miRecordDelete
  // 
  Self.miRecordDelete.Index := 5;
  Self.miRecordDelete.Shortcut := System.Windows.Forms.Shortcut.CtrlL;
  Self.miRecordDelete.Text := 'Удалить запись';
  Include(Self.miRecordDelete.Click, Self.miRecordDeleteClick);
  // 
  // N5
  // 
  Self.N5.Index := 6;
  Self.N5.Text := '-';
  // 
  // miStreamInput
  // 
  Self.miStreamInput.Index := 7;
  Self.miStreamInput.Text := 'Поточный ввод...';
  Include(Self.miStreamInput.Click, Self.miStreamInputClick);
  // 
  // miPedigree
  // 
  Self.miPedigree.Index := 2;
  Self.miPedigree.MenuItems.AddRange(TArrayOfSystem_Windows_Forms_MenuItem.Create(Self.miTreeAncestors, 
          Self.miTreeDescendants, Self.miTreeBoth, Self.N6, Self.miPedigree_dAboville, 
          Self.miPedigree_Konovalov, Self.N7, Self.miMap, Self.N8, Self.miStats));
  Self.miPedigree.Text := 'Родословная';
  // 
  // miTreeAncestors
  // 
  Self.miTreeAncestors.Index := 0;
  Self.miTreeAncestors.Shortcut := System.Windows.Forms.Shortcut.CtrlA;
  Self.miTreeAncestors.Text := 'Древо предков';
  Include(Self.miTreeAncestors.Click, Self.miTreeAncestorsClick);
  // 
  // miTreeDescendants
  // 
  Self.miTreeDescendants.Index := 1;
  Self.miTreeDescendants.Shortcut := System.Windows.Forms.Shortcut.CtrlD;
  Self.miTreeDescendants.Text := 'Древо потомков';
  Include(Self.miTreeDescendants.Click, Self.miTreeDescendantsClick);
  // 
  // miTreeBoth
  // 
  Self.miTreeBoth.Index := 2;
  Self.miTreeBoth.Text := 'Древо полное';
  Include(Self.miTreeBoth.Click, Self.miTreeBothClick);
  // 
  // N6
  // 
  Self.N6.Index := 3;
  Self.N6.Text := '-';
  // 
  // miPedigree_dAboville
  // 
  Self.miPedigree_dAboville.Index := 4;
  Self.miPedigree_dAboville.Shortcut := System.Windows.Forms.Shortcut.CtrlP;
  Self.miPedigree_dAboville.Text := 'Роспись по д''Абовиллю';
  Include(Self.miPedigree_dAboville.Click, Self.miPedigree_dAbovilleClick);
  // 
  // miPedigree_Konovalov
  // 
  Self.miPedigree_Konovalov.Index := 5;
  Self.miPedigree_Konovalov.Shortcut := System.Windows.Forms.Shortcut.CtrlK;
  Self.miPedigree_Konovalov.Text := 'Роспись по Коновалову';
  Include(Self.miPedigree_Konovalov.Click, Self.miPedigree_KonovalovClick);
  // 
  // N7
  // 
  Self.N7.Index := 6;
  Self.N7.Text := '-';
  // 
  // miMap
  // 
  Self.miMap.Index := 7;
  Self.miMap.Shortcut := System.Windows.Forms.Shortcut.CtrlM;
  Self.miMap.Text := 'Карты...';
  Include(Self.miMap.Click, Self.miMapClick);
  // 
  // N8
  // 
  Self.N8.Index := 8;
  Self.N8.Text := '-';
  // 
  // miStats
  // 
  Self.miStats.Index := 9;
  Self.miStats.Shortcut := System.Windows.Forms.Shortcut.CtrlT;
  Self.miStats.Text := 'Статистика...';
  Include(Self.miStats.Click, Self.miStatsClick);
  // 
  // miService
  // 
  Self.miService.Index := 3;
  Self.miService.MenuItems.AddRange(TArrayOfSystem_Windows_Forms_MenuItem.Create(Self.miCalc, 
          Self.miNamesBook, Self.miCalendar, Self.miTimeLine, Self.miOrganizer, 
          Self.N9, Self.miScripts, Self.miTreeTools, Self.N10, Self.miFilter, 
          Self.N11, Self.miOptions));
  Self.miService.Text := 'Сервис';
  // 
  // miCalc
  // 
  Self.miCalc.Index := 0;
  Self.miCalc.Text := 'Калькулятор...';
  Include(Self.miCalc.Click, Self.miCalcClick);
  // 
  // miNamesBook
  // 
  Self.miNamesBook.Index := 1;
  Self.miNamesBook.Text := 'Справочник имен...';
  Include(Self.miNamesBook.Click, Self.miNamesBookClick);
  // 
  // miCalendar
  // 
  Self.miCalendar.Index := 2;
  Self.miCalendar.Text := 'Календарь...';
  Include(Self.miCalendar.Click, Self.miCalendarClick);
  // 
  // miTimeLine
  // 
  Self.miTimeLine.Index := 3;
  Self.miTimeLine.Text := 'Линия времени...';
  Include(Self.miTimeLine.Click, Self.miTimeLineClick);
  // 
  // miOrganizer
  // 
  Self.miOrganizer.Index := 4;
  Self.miOrganizer.Text := 'Органайзер...';
  Include(Self.miOrganizer.Click, Self.miOrganizerClick);
  // 
  // N9
  // 
  Self.N9.Index := 5;
  Self.N9.Text := '-';
  // 
  // miScripts
  // 
  Self.miScripts.Index := 6;
  Self.miScripts.Shortcut := System.Windows.Forms.Shortcut.CtrlF11;
  Self.miScripts.Text := 'Скрипты...';
  Include(Self.miScripts.Click, Self.miScriptsClick);
  // 
  // miTreeTools
  // 
  Self.miTreeTools.Index := 7;
  Self.miTreeTools.Text := 'Инструменты...';
  Include(Self.miTreeTools.Click, Self.miTreeToolsClick);
  // 
  // N10
  // 
  Self.N10.Index := 8;
  Self.N10.Text := '-';
  // 
  // miFilter
  // 
  Self.miFilter.Index := 9;
  Self.miFilter.Shortcut := System.Windows.Forms.Shortcut.CtrlF;
  Self.miFilter.Text := 'Фильтр...';
  Include(Self.miFilter.Click, Self.miFilterClick);
  // 
  // N11
  // 
  Self.N11.Index := 10;
  Self.N11.Text := '-';
  // 
  // miOptions
  // 
  Self.miOptions.Index := 11;
  Self.miOptions.Text := 'Настройки...';
  Include(Self.miOptions.Click, Self.miOptionsClick);
  // 
  // miWindow
  // 
  Self.miWindow.Index := 4;
  Self.miWindow.MenuItems.AddRange(TArrayOfSystem_Windows_Forms_MenuItem.Create(Self.miWinCascade, 
          Self.miWinHTile, Self.miWinVTile, Self.miWinMinimize, Self.miWinArrange));
  Self.miWindow.Text := '&Окна';
  // 
  // miWinCascade
  // 
  Self.miWinCascade.Index := 0;
  Self.miWinCascade.Text := '&Каскад';
  Include(Self.miWinCascade.Click, Self.miWinCascadeClick);
  // 
  // miWinHTile
  // 
  Self.miWinHTile.Index := 1;
  Self.miWinHTile.Text := '&Горизонтальная мозаика';
  Include(Self.miWinHTile.Click, Self.miWinHTileClick);
  // 
  // miWinVTile
  // 
  Self.miWinVTile.Index := 2;
  Self.miWinVTile.Text := '&Вертикальная мозаика';
  Include(Self.miWinVTile.Click, Self.miWinVTileClick);
  // 
  // miWinMinimize
  // 
  Self.miWinMinimize.Index := 3;
  Self.miWinMinimize.Text := '&Свернуть все';
  Include(Self.miWinMinimize.Click, Self.miWinMinimizeClick);
  // 
  // miWinArrange
  // 
  Self.miWinArrange.Index := 4;
  Self.miWinArrange.Text := '&Разместить все';
  Include(Self.miWinArrange.Click, Self.miWinArrangeClick);
  // 
  // miHelp
  // 
  Self.miHelp.Index := 5;
  Self.miHelp.MenuItems.AddRange(TArrayOfSystem_Windows_Forms_MenuItem.Create(Self.miGenResources, 
          Self.miKinshipTerms, Self.miFAQ, Self.miContext, Self.N12, Self.miAbout));
  Self.miHelp.Text := 'Справка';
  // 
  // miGenResources
  // 
  Self.miGenResources.Index := 0;
  Self.miGenResources.Text := 'Ресурсы в Интернете...';
  Include(Self.miGenResources.Click, Self.miGenResourcesClick);
  // 
  // miKinshipTerms
  // 
  Self.miKinshipTerms.Index := 1;
  Self.miKinshipTerms.Text := 'Терминология родства...';
  Include(Self.miKinshipTerms.Click, Self.miKinshipTermsClick);
  // 
  // miFAQ
  // 
  Self.miFAQ.Index := 2;
  Self.miFAQ.Text := 'Часто задаваемые вопросы...';
  Include(Self.miFAQ.Click, Self.miFAQClick);
  // 
  // miContext
  // 
  Self.miContext.Index := 3;
  Self.miContext.Shortcut := System.Windows.Forms.Shortcut.F1;
  Self.miContext.Text := 'Содержание';
  Include(Self.miContext.Click, Self.miContextClick);
  // 
  // N12
  // 
  Self.N12.Index := 4;
  Self.N12.Text := '-';
  // 
  // miAbout
  // 
  Self.miAbout.Index := 5;
  Self.miAbout.Text := 'О программе...';
  Include(Self.miAbout.Click, Self.miAboutClick);
  // 
  // OpenDialog1
  // 
  Self.OpenDialog1.Filter := 'GEDCOM|*.ged|Все файлы (*.*)|*.*''';
  // 
  // SaveDialog1
  // 
  Self.SaveDialog1.DefaultExt := 'ged';
  Self.SaveDialog1.Filter := 'GEDCOM|*.ged';
  // 
  // ImageList_Shields
  // 
  Self.ImageList_Shields.ImageSize := System.Drawing.Size.Create(16, 16);
  Self.ImageList_Shields.TransparentColor := System.Drawing.Color.Transparent;
  // 
  // TfmGEDKeeper
  // 
  Self.AllowDrop := True;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.ClientSize := System.Drawing.Size.Create(896, 745);
  Self.Controls.Add(Self.StatusBar);
  Self.Controls.Add(Self.ToolBar1);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25);
  Self.Icon := (System.Drawing.Icon(resources.GetObject('$this.Icon')));
  Self.IsMdiContainer := True;
  Self.Location := System.Drawing.Point.Create(337, 111);
  Self.Menu := Self.MainMenu1;
  Self.Name := 'TfmGEDKeeper';
  Self.Text := 'GEDKeeper';
  Include(Self.Resize, Self.TfmGEDKeeper_Resize);
  Include(Self.Closing, Self.TfmGEDKeeper_Closing);
  Include(Self.Load, Self.FormCreate);
  Include(Self.DragDrop, Self.TfmGEDKeeper_DragDrop);
  Include(Self.Closed, Self.TfmGEDKeeper_Closed);
  Include(Self.VisibleChanged, Self.FormShow);
  Include(Self.DragEnter, Self.TfmGEDKeeper_DragEnter);
  (System.ComponentModel.ISupportInitialize(Self.StatusBarPanel1)).EndInit;
  (System.ComponentModel.ISupportInitialize(Self.StatusBarPanel2)).EndInit;
  Self.ResumeLayout(False);
end;

constructor TfmGEDKeeper.Create;
begin
  inherited Create;
  InitializeComponent;

  TMapBrowser.GeoInit();
  
{ alert
  ToolTip1.SetToolTip(miFileLoad, 'Открыть файл древа');
  ToolTip1.SetToolTip(miFilter, 'Фильтрация списка персон');
  ToolTip1.SetToolTip(miStats, 'Статистический анализ данных');
  ToolTip1.SetToolTip(miFileNew, 'Создать новый файл древа');
  ToolTip1.SetToolTip(miFileSave, 'Сохранить файл древа');
  ToolTip1.SetToolTip(miRecordAdd, 'Добавить запись');
  ToolTip1.SetToolTip(miRecordEdit, 'Изменить запись');
  ToolTip1.SetToolTip(miRecordDelete, 'Удалить запись');
  ToolTip1.SetToolTip(miTreeAncestors, 'Сформировать древо предков');
  ToolTip1.SetToolTip(miTreeDescendants, 'Сформировать древо потомков');

  WindowMenu = miWindow
  Include(Self.???, Self.FormClose);
  StatusBar.OnDblClick = StatusBarDblClick
  StatusBar.OnDrawPanel = StatusBarDrawPanel
  Self.StatusBar.AutoHint = True
  sbp.Style := psOwnerDraw;
}

  fmGEDKeeper := Self;
end;

procedure TfmGEDKeeper.Dispose(Disposing: Boolean);
begin
  if Disposing then begin
    //
    TMapBrowser.GeoDone();
  end;
  inherited Dispose(Disposing);
end;

procedure TfmGEDKeeper.ToolBar1_ButtonClick(sender: System.Object; e: System.Windows.Forms.ToolBarButtonClickEventArgs);
begin
  if (e.Button = tbFileNew) then Self.miFileNewClick(nil, nil);
  if (e.Button = tbFileLoad) then Self.miFileLoadClick(nil, nil);
  if (e.Button = tbFileSave) then Self.miFileSaveClick(nil, nil);
  if (e.Button = tbRecordAdd) then Self.miRecordAddClick(nil, nil);
  if (e.Button = tbRecordEdit) then Self.miRecordEditClick(nil, nil);
  if (e.Button = tbRecordDelete) then Self.miRecordDeleteClick(nil, nil);
  if (e.Button = tbUndo) then Self.miUndoClick(nil, nil);
  if (e.Button = tbRedo) then Self.miRedoClick(nil, nil);
  if (e.Button = tbFilter) then Self.miFilterClick(nil, nil);
  if (e.Button = tbTreeAncestors) then Self.miTreeAncestorsClick(nil, nil);
  if (e.Button = tbTreeDescendants) then Self.miTreeDescendantsClick(nil, nil);
  if (e.Button = tbTreeBoth) then Self.miTreeBothClick(nil, nil);
  if (e.Button = tbStats) then Self.miStatsClick(nil, nil);
  if (e.Button = tbPrev) then Self.tbPrevClick(nil, nil);
  if (e.Button = tbNext) then Self.tbNextClick(nil, nil);
end;

function TfmGEDKeeper.CreateBase(const aFileName: string): TfmBase;
begin
  Result := TfmBase.Create();
  Result.MdiParent := Self;
  Result.Show();

  if (aFileName <> '') and System.IO.File.Exists(aFileName)
  then Result.FileLoad(aFileName)
  else Result.FileNew();;
end;

procedure TfmGEDKeeper.UpdateControls(ForceDeactivate: Boolean = False);
var
  rt: TGEDCOMRecord.TGEDCOMRecordType;
  cur_base: TfmBase;
  base_en, indiv_en, test_funcs: Boolean;
  st: string;
begin
  try
    if (ForceDeactivate)
    then cur_base := nil
    else cur_base := GetCurrentFile();

    if (cur_base = nil) then begin
      rt := rtNone;
      base_en := False;
    end else begin
      rt := TGEDCOMRecord.TGEDCOMRecordType(cur_base.PageRecords.SelectedIndex + 1);
      base_en := True;
    end;

    miFileClose.Enabled := (base_en);

    miFileSave.Enabled := (base_en);
    tbFileSave.Enabled := miFileSave.Enabled;

    miFileProperties.Enabled := (base_en);
    miExportToWeb.Enabled := (base_en);
    miExportToExcelFile.Enabled := (base_en);
    miExportToExcelApp.Enabled := (base_en);
    miTreeTools.Enabled := (base_en);
    miStreamInput.Enabled := (base_en);

    miRecordAdd.Enabled := (base_en);
    tbRecordAdd.Enabled := miRecordAdd.Enabled;

    miRecordEdit.Enabled := (base_en);
    tbRecordEdit.Enabled := miRecordEdit.Enabled;

    miRecordDelete.Enabled := (base_en);
    tbRecordDelete.Enabled := miRecordDelete.Enabled;

    indiv_en := (base_en) and (rt = rtIndividual);

    miFilter.Enabled := (indiv_en);
    tbFilter.Enabled := miFilter.Enabled;

    miStats.Enabled := (base_en);
    tbStats.Enabled := miStats.Enabled;

    miTreeAncestors.Enabled := (indiv_en);
    tbTreeAncestors.Enabled := miTreeAncestors.Enabled;

    miTreeDescendants.Enabled := (indiv_en);
    tbTreeDescendants.Enabled := miTreeDescendants.Enabled;

    miTreeBoth.Enabled := (indiv_en);
    tbTreeBoth.Enabled := miTreeBoth.Enabled;

    miPedigree.Enabled := (indiv_en);
    tbPedigree.Enabled := miPedigree.Enabled;
    miPedigree_dAboville.Enabled := (indiv_en);
    miPedigree_Konovalov.Enabled := (indiv_en);

    miOrganizer.Enabled := (base_en);
    miScripts.Enabled := (base_en);

    tbPrev.Enabled := (cur_base <> nil) and (cur_base.Backman.CanBackward());
    tbNext.Enabled := (cur_base <> nil) and (cur_base.Backman.CanForward());

    test_funcs := TGenEngine.IsDevComp();

    miUndo.Enabled := (test_funcs) and (cur_base <> nil) and (cur_base.Undoman.CanUndo());
    tbUndo.Enabled := miUndo.Enabled;

    miRedo.Enabled := (test_funcs) and (cur_base <> nil) and (cur_base.Undoman.CanRedo());
    tbRedo.Enabled := miRedo.Enabled;

    if (cur_base <> nil) then begin
      st := LSList[LSID_SBRecords] + ': ' + cur_base.FCounts[rt].Total.ToString();
      if (rt = rtIndividual)
      then st := st + ', ' + LSList[LSID_SBFiltered] + ': ' + cur_base.FCounts[rt].Filtered.ToString();

      StatusBar.Panels[0].Text := st;
    end;

    StatusBar.Invalidate;
  except
    on E: Exception do TGKUtils.LogWrite('GKMain.UpdateControls(): ' + E.Message);
  end;
end;

procedure TfmGEDKeeper.UpdateMRU();
var
  i: Integer;
  mi: System.Windows.Forms.MenuItem;
begin
  miMRUFiles.Enabled := (FOptions.MRUFiles.Count > 0);
  miMRUFiles.MenuItems.Clear();

  MenuMRU.MenuItems.Clear;

  for i := 0 to FOptions.MRUFiles.Count - 1 do begin
    mi := TGKMenuItem.Create(FOptions.MRUFiles[i], i);
    Include(mi.Click, MRUFileClick);
    miMRUFiles.MenuItems.Add(mi);

    mi := TGKMenuItem.Create(FOptions.MRUFiles[i], i);
    Include(mi.Click, MRUFileClick);
    MenuMRU.MenuItems.Add(mi);
  end;
end;

procedure TfmGEDKeeper.AddMRU(const aFileName: string);
var
  idx: Integer;
begin
  idx := FOptions.MRUFiles.IndexOf(aFileName);

  if (idx < 0)
  then FOptions.MRUFiles.Insert(0, aFileName)
  else begin
    if (idx > 0) then begin
      FOptions.MRUFiles.Delete(idx);
      FOptions.MRUFiles.Insert(0, aFileName);
    end;
  end;

  UpdateMRU();
end;

procedure TfmGEDKeeper.MRUFileClick(sender: System.Object; e: System.EventArgs);
var
  idx: Integer;
begin
  idx := (Sender as TGKMenuItem).Tag;
  CreateBase(FOptions.MRUFiles[idx]);
end;

procedure TfmGEDKeeper.TfmGEDKeeper_Resize(sender: System.Object; e: System.EventArgs);
begin
  StatusBar.Panels[0].Width := Width - 50;
end;

//alert
{procedure TfmGEDKeeper.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil)
  then ImageList_Shields.Draw(StatusBar.Canvas, Rect.Left, Rect.Top, Ord(cur_base.ShieldState));
end;}

procedure TfmGEDKeeper.StatusBarDblClick(sender: System.Object; e: System.EventArgs);
var
  mpt: TPoint;
  x, j, pan_index: Integer;
  cur_base: TfmBase;
  ss: TGenEngine.TShieldState;
begin
  //alert
  {if (StatusBar.SimplePanel) or (StatusBar.Panels.Count = 0)
  then Exit;

  mpt := StatusBar.ScreenToClient(Mouse.CursorPos);

  pan_index := -1;
  x := 0;
  for j := 0 to StatusBar.Panels.Count - 1 do begin
    x := x + StatusBar.Panels[j].Width;
    if (mpt.X < x) then begin
      pan_index := j;
      Break;
    end;
  end;

  //clicked "after" the last panel -
  //fake it as if the last one was clicked
  if (pan_index = -1)
  then pan_index := -1 + StatusBar.Panels.Count;

  if (pan_index = 1) then begin
    cur_base := GetCurrentFile();
    if (cur_base = nil) then Exit;

    ss := cur_base.ShieldState;
    if (ss = ssNone) then ss := ssMaximum else ss := TShieldState(Ord(ss) + 1);
    cur_base.ShieldState := ss;

    StatusBar.Repaint;
  end;}
end;

procedure TfmGEDKeeper.WndProc(var m: System.Windows.Forms.Message);
var
  numFiles: Longint;
  buffer: System.Text.StringBuilder;
{$IFNDEF DELPHI_NET}
  ParamStr: string;
  CopyDataStructure: TCopyDataStruct;
  i, len: Integer;
{$ENDIF}
begin
  inherited WndProc(m);

  if (m.Msg = WM_KEEPMODELESS) then begin
    if Assigned(fmCalcWidget) and fmCalcWidget.Visible {Showing}
    then EnableWindow(fmCalcWidget.Handle.ToInt32(), True);
  end
  else
  if (m.Msg = WM_COPYDATA) then begin
    {$IFNDEF DELPHI_NET}
    CopyDataStructure := Msg.CopyDataStruct^;
    ParamStr := '';
    len := CopyDataStructure.cbData;
    for i := 0 to len - 1 do begin
      ParamStr := ParamStr + (PChar(CopyDataStructure.lpData) + i)^;
    end;

    i := 0;
    while not(Length(ParamStr) = 0) do begin
      if IsDelimiter(#13, ParamStr, i) then begin
        CreateBase(Copy(ParamStr, 0, i - 1));
        ParamStr := Copy(ParamStr, i + 1, Length(ParamStr) - i - 1);
      end;
      Inc(i);
    end;
    {$ENDIF}
  end;
end;

procedure TfmGEDKeeper.miExitClick(sender: System.Object; e: System.EventArgs);
begin
  Close;
end;

procedure TfmGEDKeeper.miExportToWebClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ExportToWeb();
end;

procedure TfmGEDKeeper.miExportToExcelFileClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ExportToExcel(False);
end;

procedure TfmGEDKeeper.miFilePropertiesClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.FileProperties();
end;

procedure TfmGEDKeeper.miStreamInputClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.PersonScan();
end;

procedure TfmGEDKeeper.miScriptsClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowScriptDaemon();
end;

procedure TfmGEDKeeper.miTreeToolsClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.TreeTools();
end;

procedure TfmGEDKeeper.miOptionsClick(sender: System.Object; e: System.EventArgs);
var
  fmOptions: TfmOptions;
  i: Integer;
begin
  fmOptions := TfmOptions.Create();
  try
    if (fmOptions.ShowDialog() = System.Windows.Forms.DialogResult.OK) then begin
      for i := 0 to Length(MdiChildren) - 1 do
        if (MDIChildren[i] is TfmBase)
        then TfmBase(MDIChildren[i]).ListsRefresh(True);
    end;
  finally
    fmOptions.Free;
  end;
end;

procedure TfmGEDKeeper.miFileCloseClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.Close();
end;

procedure TfmGEDKeeper.miMapClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowMap();
end;

procedure TfmGEDKeeper.miOrganizerClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowOrganizer();
end;

procedure TfmGEDKeeper.miTimeLineClick(sender: System.Object; e: System.EventArgs);
begin
  if not(miTimeLine.Checked) {and not(Assigned(fmTimeLine)) }then begin
    fmTimeLine := TfmTimeLine.Create();
    fmTimeLine.Location := System.Drawing.Point.Create(
      10, Screen.PrimaryScreen.WorkingArea.Height - fmTimeLine.Height - 10);
    fmTimeLine.Show;

    miTimeLine.Checked := True;
  end else begin
    miTimeLine.Checked := False;

    FreeAndNil(fmTimeLine);
  end;
end;

procedure TfmGEDKeeper.miCalendarClick(sender: System.Object; e: System.EventArgs);
begin
  if not(miCalendar.Checked) {and not(Assigned(fmCalendar)) }then begin
    fmCalendar := TfmCalendar.Create();
    fmCalendar.Location := System.Drawing.Point.Create(
      Screen.PrimaryScreen.WorkingArea.Width - fmCalendar.Width - 10, 50);
    fmCalendar.Show;

    miCalendar.Checked := True;
  end else begin
    miCalendar.Checked := False;

    FreeAndNil(fmCalendar);
  end;
end;

procedure TfmGEDKeeper.miNamesBookClick(sender: System.Object; e: System.EventArgs);
var
  scr: System.Windows.Forms.Screen;
begin
  if not(miNamesBook.Checked) {and not(Assigned(fmNamesBook)) }then begin
    scr := System.Windows.Forms.Screen.PrimaryScreen;

    fmNamesBook := TfmNamesBook.Create();
    fmNamesBook.Location := System.Drawing.Point.Create(
      scr.WorkingArea.Width - fmNamesBook.Width - 10,
      (scr.WorkingArea.Height - fmNamesBook.Height) div 2);
    fmNamesBook.Show;

    miNamesBook.Checked := True;
  end else begin
    miNamesBook.Checked := False;

    FreeAndNil(fmNamesBook);
  end;
end;

procedure TfmGEDKeeper.miCalcClick(sender: System.Object; e: System.EventArgs);
var
  scr: System.Windows.Forms.Screen;
begin
  if not(miCalc.Checked) {and not(Assigned(fmCalcWidget)) }then begin
    scr := System.Windows.Forms.Screen.PrimaryScreen;

    fmCalcWidget := TfmCalcWidget.Create();
    fmCalcWidget.Location := System.Drawing.Point.Create(
      scr.WorkingArea.Width - fmCalcWidget.Width - 10,
      scr.WorkingArea.Height - fmCalcWidget.Height - 10);
    fmCalcWidget.Show;

    miCalc.Checked := True;
  end else begin
    miCalc.Checked := False;

    FreeAndNil(fmCalcWidget);
  end;
end;

procedure TfmGEDKeeper.miAboutClick(sender: System.Object; e: System.EventArgs);
begin
  TfmAbout.ShowAbout(TGenEngine.AppName, TGKUtils.GetFileVersion());
end;

procedure TfmGEDKeeper.miGenResourcesClick(sender: System.Object; e: System.EventArgs);
begin
  ShowHelpTopic('::/gkhGenRes.htm');
end;

procedure TfmGEDKeeper.miKinshipTermsClick(sender: System.Object; e: System.EventArgs);
begin
  ShowHelpTopic('::/gkhRelations.htm');
end;

procedure TfmGEDKeeper.miFAQClick(sender: System.Object; e: System.EventArgs);
begin
  ShowHelpTopic('::/gkhFAQ.htm');
end;

procedure TfmGEDKeeper.ShowHelpTopic(aTopic: string = '');
var
  fns: string;
begin
  fns := TGKUtils.GetAppPath() + 'GEDKeeper.chm' + aTopic;
  HtmlHelp(Handle, fns, HH_DISPLAY_TOPIC, 0);
end;

procedure TfmGEDKeeper.miContextClick(sender: System.Object; e: System.EventArgs);
begin
  ShowHelpTopic();
end;

procedure TfmGEDKeeper.miWinCascadeClick(sender: System.Object; e: System.EventArgs);
begin
  Self.LayoutMDI(MDILayout.Cascade);
end;

procedure TfmGEDKeeper.miWinHTileClick(sender: System.Object; e: System.EventArgs);
begin
  Self.LayoutMDI(MDILayout.TileHorizontal);
end;

procedure TfmGEDKeeper.miWinVTileClick(sender: System.Object; e: System.EventArgs);
begin
  Self.LayoutMDI(MDILayout.TileVertical);
end;

procedure TfmGEDKeeper.miWinMinimizeClick(sender: System.Object; e: System.EventArgs);
var
  I: Integer;
begin
  for I := Length(Self.MdiChildren) - 1 downto 0 do
    Self.MdiChildren[I].WindowState := FormWindowState.Minimized;
end;

procedure TfmGEDKeeper.miWinArrangeClick(sender: System.Object; e: System.EventArgs);
begin
  Self.LayoutMDI(MDILayout.ArrangeIcons);
end;

procedure TfmGEDKeeper.miFilterClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.SetFilter();
end;

procedure TfmGEDKeeper.tbPrevClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.NavPrev();
end;

procedure TfmGEDKeeper.tbNextClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.NavNext();
end;

procedure TfmGEDKeeper.miStatsClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowStats();
end;

procedure TfmGEDKeeper.miFileNewClick(sender: System.Object; e: System.EventArgs);
begin
  CreateBase('');
end;

procedure TfmGEDKeeper.miFileLoadClick(sender: System.Object; e: System.EventArgs);
begin
  OpenDialog1.InitialDirectory := FOptions.LastDir;

  if (OpenDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK)
  then CreateBase(OpenDialog1.FileName);
end;

procedure TfmGEDKeeper.miFileSaveClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base = nil) then Exit;

  SaveDialog1.FileName := cur_base.FileName;
  if (SaveDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK)
  then cur_base.FileSave(SaveDialog1.FileName);
end;

procedure TfmGEDKeeper.miUndoClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.DoUndo();
end;

procedure TfmGEDKeeper.miRedoClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.DoRedo();
end;

procedure TfmGEDKeeper.miTreeAncestorsClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowTreeAncestors();
end;

procedure TfmGEDKeeper.miTreeDescendantsClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowTreeDescendants();
end;

procedure TfmGEDKeeper.miPedigree_dAbovilleClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.GenPedigree_dAboville();
end;

procedure TfmGEDKeeper.miPedigree_KonovalovClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.GenPedigree_Konovalov();
end;

procedure TfmGEDKeeper.miRecordAddClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.RecordAdd();
end;

procedure TfmGEDKeeper.miRecordEditClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.RecordEdit(sender, e);
end;

procedure TfmGEDKeeper.miRecordDeleteClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.RecordDelete();
end;

procedure TfmGEDKeeper.miExportToExcelAppClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ExportToExcel(True);
end;

procedure TfmGEDKeeper.miTreeBothClick(sender: System.Object; e: System.EventArgs);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowTreeBoth();
end;

procedure TfmGEDKeeper.LoadLanguage(LangCode: Integer);
var
  i: Integer;
  loaded: Boolean;
  intf: ILocalization;
  scr: System.Windows.Forms.Screen;
begin
  scr := System.Windows.Forms.Screen.PrimaryScreen;

  if (LangCode <> LSDefCode) then begin
    loaded := False;
    for i := 0 to FOptions.LangsCount - 1 do begin
      if (FOptions.Langs[i].Code = LangCode) then begin
        loaded := TLangMan.LoadFromFile(FOptions.Langs[i].FileName);
        Break;
      end;
    end;

    if not(loaded)
    then LangCode := LSDefCode;
  end;

  if (LangCode = LSDefCode)
  then TLangMan.DefInit();

  for i := 0 to Length(MdiChildren) - 1 do
    if Supports(MdiChildren[i], ILocalization, intf)
    then intf.SetLang();

  FOptions.InterfaceLang := LangCode;
end;

procedure TfmGEDKeeper.SetLang();
//var
  //lMan: TLangMan;
begin
  //miFile.Text :=   lMan[LSID_MIFile];
  miFile.Text := LSList[LSID_MIFile];

  miEdit.Text := LSList[LSID_MIEdit];
  miPedigree.Text := LSList[LSID_MIPedigree];
  miService.Text := LSList[LSID_MIService];
  miWindow.Text := LSList[LSID_MIWindow];
  miHelp.Text := LSList[LSID_MIHelp];

  miFileNew.Text := LSList[LSID_MIFileNew];
  miFileLoad.Text := LSList[LSID_MIFileLoad];
  miMRUFiles.Text := LSList[LSID_MIMRUFiles];
  miFileSave.Text := LSList[LSID_MIFileSave];
  miFileClose.Text := LSList[LSID_MIFileClose];
  miFileProperties.Text := LSList[LSID_MIFileProperties];
  miExport.Text := LSList[LSID_MIExport];
  miExportToWeb.Text := LSList[LSID_MIExportToWeb];
  miExportToExcelApp.Text := LSList[LSID_MIExportToExcelApp];
  miExportToExcelFile.Text := LSList[LSID_MIExportToExcelFile];
  miExit.Text := LSList[LSID_MIExit];

  miUndo.Text := LSList[LSID_MIUndo];
  miRedo.Text := LSList[LSID_MIRedo];
  miRecordAdd.Text := LSList[LSID_MIRecordAdd];
  miRecordEdit.Text := LSList[LSID_MIRecordEdit];
  miRecordDelete.Text := LSList[LSID_MIRecordDelete];
  miStreamInput.Text := LSList[LSID_MIStreamInput] + '...';

  miTreeAncestors.Text := LSList[LSID_MITreeAncestors];
  miTreeDescendants.Text := LSList[LSID_MITreeDescendants];
  miTreeBoth.Text := LSList[LSID_MITreeBoth];
  miPedigree_dAboville.Text := LSList[LSID_MIPedigree_dAboville];
  miPedigree_Konovalov.Text := LSList[LSID_MIPedigree_Konovalov];
  miMap.Text := LSList[LSID_MIMap] + '...';
  miStats.Text := LSList[LSID_MIStats] + '...';

  miCalc.Text := LSList[LSID_MICalc] + '...';
  miNamesBook.Text := LSList[LSID_MINamesBook] + '...';
  miCalendar.Text := LSList[LSID_MICalendar] + '...';
  miTimeLine.Text := LSList[LSID_MITimeLine] + '...';
  miOrganizer.Text := LSList[LSID_MIOrganizer] + '...';
  miScripts.Text := LSList[LSID_MIScripts];
  miTreeTools.Text := LSList[LSID_MITreeTools];
  miFilter.Text := LSList[LSID_MIFilter] + '...';
  miOptions.Text := LSList[LSID_MIOptions] + '...';

  miWinCascade.Text := LSList[LSID_MIWinCascade];
  miWinHTile.Text := LSList[LSID_MIWinHTile];
  miWinVTile.Text := LSList[LSID_MIWinVTile];
  miWinMinimize.Text := LSList[LSID_MIWinMinimize];
  miWinArrange.Text := LSList[LSID_MIWinArrange];

  miGenResources.Text := LSList[LSID_MIGenResources];
  miKinshipTerms.Text := LSList[LSID_MIKinshipTerms];
  miFAQ.Text := LSList[LSID_MIFAQ];
  miContext.Text := LSList[LSID_MIContext];
  miAbout.Text := LSList[LSID_MIAbout] + '...';
end;

function TfmGEDKeeper.CheckFormRect(aForm: System.Windows.Forms.Form): TRect;
var
  x, y, w, h, mw, mh: Integer;
  scr: System.Windows.Forms.Screen;
begin
  x := aForm.Left;
  y := aForm.Top;
  w := aForm.Width;
  h := aForm.Height;

  scr := System.Windows.Forms.Screen.PrimaryScreen;
  mw := scr.WorkingArea.Width;
  mh := scr.WorkingArea.Height;

  if (x < 0) then x := 0;
  if (y < 0) then y := 0;
  if (w > mw) then w := mw;
  if (h > mh) then h := mh;

  Result := TRect.Create(x, y, (x + w) - 1, (y + h) - 1);
end;

function TfmGEDKeeper.ShowModalEx(aForm: System.Windows.Forms.Form;
  aPopupParent: System.Windows.Forms.Form = nil;
  KeepModeless: Boolean = False): System.Windows.Forms.DialogResult;
begin
  if KeepModeless
  then PostMessage(fmGEDKeeper.Handle.ToInt32(), WM_KEEPMODELESS, 0, 0);

  Result := aForm.ShowDialog();
end;

procedure TfmGEDKeeper.TfmGEDKeeper_DragDrop(sender: System.Object; e: System.Windows.Forms.DragEventArgs);
var
  a: System.&Array;
  i: Integer;
  fn: string;
begin
  try
    a := System.&Array(e.Data.GetData(DataFormats.FileDrop));

    if (a <> nil) then begin
      for i := 0 to a.Length - 1 do begin
        fn := a.GetValue(i).ToString();
        CreateBase(fn);
      end;
    end;
  except
  end;
end;

procedure TfmGEDKeeper.TfmGEDKeeper_DragEnter(sender: System.Object; e: System.Windows.Forms.DragEventArgs);
begin
  if (e.Data.GetDataPresent(DataFormats.FileDrop))
  then e.Effect := DragDropEffects.Copy
  else e.Effect := DragDropEffects.None;
end;

end.
