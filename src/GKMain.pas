unit GKMain; {prepare:partial; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Variants, Forms, Controls, Menus, StdCtrls,
  Dialogs, Buttons, Messages, ExtCtrls, ComCtrls, ToolWin, ImgList,
  GKEngine, GKCommon, GKBase, GKLangs, Contnrs;

const
  WM_KEEPMODELESS = WM_USER + 111;

type
  TfmGEDKeeper = class(TForm, ILocalization)
    StatusBar: TStatusBar;
    ImageList_Buttons: TImageList;
    ToolBar1: TToolBar;
    tbFileNew: TToolButton;
    tbFileLoad: TToolButton;
    tbFileSave: TToolButton;
    ToolButton1: TToolButton;
    tbRecordAdd: TToolButton;
    tbRecordEdit: TToolButton;
    tbRecordDelete: TToolButton;
    ToolButton2: TToolButton;
    tbFilter: TToolButton;
    ToolButton3: TToolButton;
    tbTreeAncestors: TToolButton;
    tbTreeDescendants: TToolButton;
    ToolButton4: TToolButton;
    tbPedigree: TToolButton;
    ToolButton6: TToolButton;
    tbStats: TToolButton;
    ToolButton5: TToolButton;
    tbPrev: TToolButton;
    tbNext: TToolButton;
    MainMenu1: TMainMenu;
    miFile: TMenuItem;
    miFileNew: TMenuItem;
    miFileLoad: TMenuItem;
    miMRUFiles: TMenuItem;
    miFileSave: TMenuItem;
    miFileClose: TMenuItem;
    N1: TMenuItem;
    miFileProperties: TMenuItem;
    N2: TMenuItem;
    miExportToWeb: TMenuItem;
    miExportToExcelFile: TMenuItem;
    N3: TMenuItem;
    miTreeTools: TMenuItem;
    N4: TMenuItem;
    miExit: TMenuItem;
    miEdit: TMenuItem;
    miRecordAdd: TMenuItem;
    miRecordEdit: TMenuItem;
    miRecordDelete: TMenuItem;
    N5: TMenuItem;
    miStreamInput: TMenuItem;
    N6: TMenuItem;
    miFilter: TMenuItem;
    N7: TMenuItem;
    miOptions: TMenuItem;
    miPedigree: TMenuItem;
    miTreeAncestors: TMenuItem;
    miTreeDescendants: TMenuItem;
    N8: TMenuItem;
    miPedigree_dAboville: TMenuItem;
    miPedigree_Konovalov: TMenuItem;
    N9: TMenuItem;
    miMap: TMenuItem;
    N10: TMenuItem;
    miStats: TMenuItem;
    miWindow: TMenuItem;
    miWinCascade: TMenuItem;
    miWinHTile: TMenuItem;
    miWinVTile: TMenuItem;
    miWinMinimize: TMenuItem;
    miWinArrange: TMenuItem;
    miHelp: TMenuItem;
    miGenResources: TMenuItem;
    miKinshipTerms: TMenuItem;
    miContext: TMenuItem;
    N11: TMenuItem;
    miAbout: TMenuItem;
    MenuMRU: TPopupMenu;
    MenuPedigree: TPopupMenu;
    miPedigree_dAboville2: TMenuItem;
    miPedigree_Konovalov2: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ToolButton7: TToolButton;
    tbUndo: TToolButton;
    tbRedo: TToolButton;
    miFAQ: TMenuItem;
    ImageList_Shields: TImageList;
    miCalc: TMenuItem;
    miNamesBook: TMenuItem;
    miCalendar: TMenuItem;
    miTimeLine: TMenuItem;
    miOrganizer: TMenuItem;
    miDBImport: TMenuItem;
    miService: TMenuItem;
    N12: TMenuItem;
    miUndo: TMenuItem;
    miRedo: TMenuItem;
    miScripts: TMenuItem;
    miExport: TMenuItem;
    miExportToExcelApp: TMenuItem;
    miTreeBoth: TMenuItem;
    tbTreeBoth: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure StatusBarDblClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miExportToWebClick(Sender: TObject);
    procedure miExportToExcelFileClick(Sender: TObject);
    procedure miFilePropertiesClick(Sender: TObject);
    procedure miStreamInputClick(Sender: TObject);
    procedure miDBImportClick(Sender: TObject);
    procedure miScriptsClick(Sender: TObject);
    procedure miTreeToolsClick(Sender: TObject);
    procedure miOptionsClick(Sender: TObject);
    procedure miFileCloseClick(Sender: TObject);
    procedure miMapClick(Sender: TObject);
    procedure miOrganizerClick(Sender: TObject);
    procedure miTimeLineClick(Sender: TObject);
    procedure miCalendarClick(Sender: TObject);
    procedure miNamesBookClick(Sender: TObject);
    procedure miCalcClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miGenResourcesClick(Sender: TObject);
    procedure miKinshipTermsClick(Sender: TObject);
    procedure miFAQClick(Sender: TObject);
    procedure miContextClick(Sender: TObject);
    procedure miWinCascadeClick(Sender: TObject);
    procedure miWinHTileClick(Sender: TObject);
    procedure miWinVTileClick(Sender: TObject);
    procedure miWinMinimizeClick(Sender: TObject);
    procedure miWinArrangeClick(Sender: TObject);
    procedure miFilterClick(Sender: TObject);
    procedure tbPrevClick(Sender: TObject);
    procedure tbNextClick(Sender: TObject);
    procedure miStatsClick(Sender: TObject);
    procedure miFileNewClick(Sender: TObject);
    procedure miFileLoadClick(Sender: TObject);
    procedure miFileSaveClick(Sender: TObject);
    procedure miUndoClick(Sender: TObject);
    procedure miRedoClick(Sender: TObject);
    procedure miTreeAncestorsClick(Sender: TObject);
    procedure miTreeDescendantsClick(Sender: TObject);
    procedure miPedigree_dAbovilleClick(Sender: TObject);
    procedure miPedigree_KonovalovClick(Sender: TObject);
    procedure miRecordAddClick(Sender: TObject);
    procedure miRecordEditClick(Sender: TObject);
    procedure miRecordDeleteClick(Sender: TObject);
    procedure miExportToExcelAppClick(Sender: TObject);
    procedure miTreeBothClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FNamesTable: TNamesTable;
    FOptions: TGlobalOptions;

    procedure MRUFileClick(Sender: TObject);
    procedure UpdateMRU();

    procedure ExecWidget(WidgetClass: TFormClass; var Widget: TForm);

    procedure FileDrop(var Msg: TWMDROPFILES); message WM_DROPFILES;
    procedure CopyData(var Msg: TWMCopyData); message WM_COPYDATA;
    procedure WMKeepModeless(var Msg: TMessage); message WM_KEEPMODELESS;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMSysCommand(var  Message: TWmSysCommand); message WM_SYSCOMMAND;
  public
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
  end;

var
  fmGEDKeeper: TfmGEDKeeper;

function ShowModalEx(aForm: TCustomForm; aPopupParent: TCustomForm = nil;
  KeepModeless: Boolean = False): Integer;

procedure RunInstance();

implementation

uses
  {$IFDEF DELPHI_NET}System.IO,{$ENDIF}
  {$IFDEF PROFILER}ZProfiler,{$ENDIF}
  uVista, ShellAPI, GedCom551, GKUtils, GKAbout, GKOptions,
  GKNamesBook, GKTimeLine, GKExpCalc, GKCalendar, Types;

{$R *.dfm}

function CheckFormRect(aForm: TForm; aScreen: TScreen): TRect;
var
  x, y, w, h, mw, mh: Integer;
begin
  x := aForm.Left;
  y := aForm.Top;
  w := aForm.Width;
  h := aForm.Height;

  {$IFDEF DELPHI8UP}
  mw := aScreen.PrimaryMonitor.WorkareaRect.Right - aScreen.PrimaryMonitor.WorkareaRect.Left;
  mh := aScreen.PrimaryMonitor.WorkareaRect.Bottom - aScreen.PrimaryMonitor.WorkareaRect.Top;
  {$ELSE}
  mw := aScreen.WorkareaRect.Right - aScreen.WorkareaRect.Left;
  mh := aScreen.WorkareaRect.Bottom - aScreen.WorkareaRect.Top;
  {$ENDIF}

  if (x < 0) then x := 0;
  if (y < 0) then y := 0;
  if (w > mw) then w := mw;
  if (h > mh) then h := mh;

  Result := Rect(x, y, w, h);
end;

function ShowModalEx(aForm: TCustomForm; aPopupParent: TCustomForm = nil;
  KeepModeless: Boolean = False): Integer;
begin
  if KeepModeless
  then PostMessage(Application.MainForm.Handle, WM_KEEPMODELESS, 0, 0);

  {$IFDEF DELPHI8UP}
  {if IsWindowsVista() then begin
    if (aPopupParent = nil)
    then aForm.PopupParent := fmGEDKeeper
    else aForm.PopupParent := aPopupParent;
  end;}
  {$ENDIF}

  Result := aForm.ShowModal;
end;

procedure RunInstance();
var
  i, WParam, LParam: Integer;
  hMainForm: HWND;
  copyDataStruct: TCopyDataStruct;
  ParamString: string;
begin
  // ищем главное окно приложения, вместо Caption - nil,
  // поскольку к заголовку главного окна может добавиться заголовок MDIChild
  // (нужно позаботиться об уникальности имени класса главной формы)

  if IsDevComp()
  then hMainForm := 0
  else hMainForm := FindWindow('TfmGEDKeeper', nil);

  if (hMainForm = 0) then begin
    Application.Initialize;
    Application.Title := 'GEDKeeper';
    Application.CreateForm(TfmGEDKeeper, fmGEDKeeper);
    for i := 1 to ParamCount do fmGEDKeeper.CreateBase(ParamStr(i));
    Application.Run;
  end else begin
    {$IFNDEF DELPHI_NET}
    ParamString := '';
    for i := 1 to ParamCount do begin
      // запихиваем все параметры в одну строку с разделителями ?13
      ParamString := ParamString + ParamStr(i) + #13;
    end;
    // создаем запись типа TCopyDataStruct

    CopyDataStruct.lpData := PChar(ParamString);
    CopyDataStruct.cbData := Length(ParamString);
    CopyDataStruct.dwData := 0;
    WParam := Application.Handle;
    LParam := Integer(@CopyDataStruct);
    // отсылаем сообщение WM_COPYDATA главному окну открытого приложения

    SendMessage(hMainForm, WM_CopyData, WParam, LParam);
    Application.Terminate;
    {$ENDIF}
  end;
end;

{==============================================================================}

procedure TfmGEDKeeper.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  if IsWindowsVista()
  then Params.ExStyle := Params.ExStyle and not WS_EX_TOOLWINDOW or WS_EX_APPWINDOW;
end;

procedure TfmGEDKeeper.WMSysCommand(var Message: TWmSysCommand);
begin
  if IsWindowsVista() then begin
    case (Message.CmdType and $FFF0) of
      SC_MINIMIZE: begin
        ShowWindow(Handle, SW_MINIMIZE);
        Message.Result := 0;
      end;
      SC_RESTORE: begin
        ShowWindow(Handle, SW_RESTORE);
        Message.Result := 0;
      end;
      else inherited;
    end;
  end else inherited;
end;

procedure TfmGEDKeeper.WMActivate(var Message: TWMActivate);
begin
  if (Message.Active = WA_ACTIVE) and not IsWindowEnabled(Handle) then begin
    SetActiveWindow(Application.Handle);
    Message.Result := 0;
  end else inherited;
end;

procedure TfmGEDKeeper.FormCreate(Sender: TObject);
begin
  if IsWindowsVista() then begin
    SetVistaFonts(Self);

    ShowWindow(Application.Handle, SW_HIDE);
    SetWindowLong(Application.Handle, GWL_EXSTYLE,
      GetWindowLong(Application.Handle, GWL_EXSTYLE) and not WS_EX_APPWINDOW or WS_EX_TOOLWINDOW);
    ShowWindow(Application.Handle, SW_SHOW);
  end;

  LongDateFormat := 'DD MMM YYYY';

  LogInit(GetAppPath() + 'GEDKeeper.log');
  {$IFDEF PROFILER}InitProfiler();{$ENDIF}

  FOptions := TGlobalOptions.Create;
  FOptions.LoadFromFile(GetAppPath() + 'GEDKeeper.ini');
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
    Self.Left := (Screen.Width - 800) div 2;
    Self.Top := (Screen.Height - 600) div 2;
    Self.Width := 800;
    Self.Height := 600;
  end;
  Self.WindowState := FOptions.MWinState;
  ///

  FNamesTable := TNamesTable.Create;
  FNamesTable.LoadFromFile(GetAppPath() + 'GEDKeeper.nms');

  LoadLanguage(FOptions.InterfaceLang);

  DragAcceptFiles(Handle, True);

  UpdateMRU();
  UpdateControls();
end;

procedure TfmGEDKeeper.FormShow(Sender: TObject);
var
  i: Integer;
  lb: TBaseWin;
  base: TfmBase;
begin
  for i := 0 to FOptions.LastBasesCount - 1 do begin
    lb := FOptions.LastBases[i];

    if FileExists(lb.FileName) then begin
      base := CreateBase(lb.FileName);
      base.Left := lb.WinRect.Left;
      base.Top := lb.WinRect.Top;
      base.Width := lb.WinRect.Right;
      base.Height := lb.WinRect.Bottom;
      base.WindowState := lb.WinState;
    end;
  end;
end;

procedure TfmGEDKeeper.FormDestroy(Sender: TObject);
begin
  FOptions.MWinRect := CheckFormRect(Self, Screen);
  FOptions.MWinState := Self.WindowState;

  ///

  HtmlHelp(0, nil, HH_CLOSE_ALL, 0);

  FNamesTable.SaveToFile(GetAppPath() + 'GEDKeeper.nms');
  FNamesTable.Destroy;

  FOptions.SaveToFile(GetAppPath() + 'GEDKeeper.ini');
  FOptions.Destroy;

  {$IFDEF PROFILER}DoneProfiler();{$ENDIF}
end;

procedure TfmGEDKeeper.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
  base: TfmBase;
  lb: TBaseWin;
begin
  Action := caFree;

  for i := MDIChildCount - 1 downto 0 do begin
    if (MDIChildren[i] is TfmBase) then begin
      base := TfmBase(MDIChildren[i]);

      if not(base.CheckModified()) then begin
        Action := caNone;
        Exit;
      end;
    end;
  end;

  FOptions.ClearLastBases();

  for i := MDIChildCount - 1 downto 0 do begin
    if (MDIChildren[i] is TfmBase) then begin
      base := TfmBase(MDIChildren[i]);

      lb := FOptions.AddLastBase();
      lb.FileName := base.FileName;
      lb.WinRect := CheckFormRect(base, Screen);
      lb.WinState := base.WindowState;

      base.Free;
    end else MDIChildren[i].Free;
  end;
end;

procedure TfmGEDKeeper.FileDrop(var Msg: TWMDROPFILES);
var
  numFiles: Longint;
  buffer: {$IFNDEF DELPHI_NET}array [0..MAX_PATH] of Char{$ELSE}string{$ENDIF};
begin
  numFiles := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
  if (numFiles > 1) then begin
    ShowMessage('You can drop only one file at a time!');
  end else begin
    {$IFNDEF DELPHI_NET}
    DragQueryFile(Msg.Drop, 0, @buffer, SizeOf(buffer));
    {$ELSE}

    {$ENDIF}
    CreateBase(buffer);
  end;
end;

procedure TfmGEDKeeper.CopyData(var Msg: TWMCopyData);
var
  ParamStr: string;
  CopyDataStructure: TCopyDataStruct;
  i, len: Integer;
begin
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

  inherited;
end;

function TfmGEDKeeper.GetCurrentFile(): TfmBase;
begin
  if (ActiveMDIChild is TfmBase)
  then Result := TfmBase(ActiveMDIChild)
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

function TfmGEDKeeper.CreateBase(const aFileName: string): TfmBase;
begin
  Result := TfmBase.Create(Application);

  if (aFileName <> '') and FileExists(aFileName)
  then Result.FileLoad(aFileName)
  else Result.FileNew();;
end;

procedure TfmGEDKeeper.UpdateControls(ForceDeactivate: Boolean = False);
var
  rt: TGEDCOMRecordType;
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
      rt := TGEDCOMRecordType(cur_base.PageRecords.TabIndex + 1);
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
    miDBImport.Enabled := (base_en);
    miScripts.Enabled := (base_en);

    tbPrev.Enabled := (cur_base <> nil) and (cur_base.Backman.CanBackward());
    tbNext.Enabled := (cur_base <> nil) and (cur_base.Backman.CanForward());

    test_funcs := IsDevComp();

    miUndo.Enabled := (test_funcs) and (cur_base <> nil) and (cur_base.Undoman.CanUndo());
    tbUndo.Enabled := miUndo.Enabled;

    miRedo.Enabled := (test_funcs) and (cur_base <> nil) and (cur_base.Undoman.CanRedo());
    tbRedo.Enabled := miRedo.Enabled;

    if (cur_base <> nil) then begin
      st := LSList[LSID_SBRecords] + ': ' + IntToStr(cur_base.FCounts[rt].Total);
      if (rt = rtIndividual)
      then st := st + ', ' + LSList[LSID_SBFiltered] + ': ' + IntToStr(cur_base.FCounts[rt].Filtered);

      StatusBar.Panels[0].Text := st;
    end;

    StatusBar.Repaint;
  except
    on E: Exception do LogWrite('GKMain.UpdateControls(): ' + E.Message);
  end;
end;

procedure TfmGEDKeeper.UpdateMRU();
var
  i: Integer;
  mi: TMenuItem;
begin
  miMRUFiles.Enabled := (FOptions.MRUFiles.Count > 0);
  miMRUFiles.Clear();

  MenuMRU.Items.Clear;

  for i := 0 to FOptions.MRUFiles.Count - 1 do begin
    mi := TMenuItem.Create(Self);
    mi.Caption := FOptions.MRUFiles[i];
    mi.Tag := i;
    mi.OnClick := MRUFileClick;
    miMRUFiles.Add(mi);

    mi := TMenuItem.Create(Self);
    mi.Caption := FOptions.MRUFiles[i];
    mi.Tag := i;
    mi.OnClick := MRUFileClick;
    MenuMRU.Items.Add(mi);
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

procedure TfmGEDKeeper.MRUFileClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := (Sender as TMenuItem).Tag;
  CreateBase(FOptions.MRUFiles[idx]);
end;

procedure TfmGEDKeeper.FormResize(Sender: TObject);
begin
  StatusBar.Panels[0].Width := Width - 50;
  StatusBar.Repaint;
end;

procedure TfmGEDKeeper.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil)
  then ImageList_Shields.Draw(StatusBar.Canvas, Rect.Left, Rect.Top, Ord(cur_base.ShieldState));
end;

procedure TfmGEDKeeper.StatusBarDblClick(Sender: TObject);
var
  mpt: TPoint;
  x, j, pan_index: Integer;
  cur_base: TfmBase;
  ss: TShieldState;
begin
  if (StatusBar.SimplePanel) or (StatusBar.Panels.Count = 0)
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
  end;
end;

procedure TfmGEDKeeper.WMKeepModeless(var Msg: TMessage);
begin
  if Assigned(fmCalcWidget) and fmCalcWidget.Showing
  then EnableWindow(fmCalcWidget.Handle, True);
end;

procedure TfmGEDKeeper.ExecWidget(WidgetClass: TFormClass; var Widget: TForm);
begin
  (*if (Action.Checked) and not(Assigned(Widget)) then begin
    Widget := WidgetClass.Create(nil);
    Widget.Left := Screen.WorkAreaWidth - Widget.Width - 10; {!!!}
    Widget.Top := Screen.WorkAreaHeight - Widget.Height - 10; {!!!}
    Widget.Show;
  end else begin
    FreeAndNil(Widget);
  end;*)

  // если хранить список открытых виджетов, то можно сделать динамическую
  // установку их один над другим по мере подключения
end;

procedure TfmGEDKeeper.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfmGEDKeeper.miExportToWebClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ExportToWeb();
end;

procedure TfmGEDKeeper.miExportToExcelFileClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ExportToExcel(False);
end;

procedure TfmGEDKeeper.miFilePropertiesClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.FileProperties();
end;

procedure TfmGEDKeeper.miStreamInputClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.PersonScan();
end;

procedure TfmGEDKeeper.miDBImportClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ImportDB();
end;

procedure TfmGEDKeeper.miScriptsClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowScriptDaemon();
end;

procedure TfmGEDKeeper.miTreeToolsClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.TreeTools();
end;

procedure TfmGEDKeeper.miOptionsClick(Sender: TObject);
var
  fmOptions: TfmOptions;
  i: Integer;
begin
  fmOptions := TfmOptions.Create(Application);
  try
    fmOptions.Options := fmGEDKeeper.Options;

    if (ShowModalEx(fmOptions) = mrOk) then begin
      for i := 0 to MDIChildCount - 1 do
        if (MDIChildren[i] is TfmBase)
        then TfmBase(MDIChildren[i]).ListsRefresh(True);
    end;
  finally
    fmOptions.Destroy;
  end;
end;

procedure TfmGEDKeeper.miFileCloseClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.Close();
end;

procedure TfmGEDKeeper.miMapClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowMap();
end;

procedure TfmGEDKeeper.miOrganizerClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowOrganizer();
end;

procedure TfmGEDKeeper.miTimeLineClick(Sender: TObject);
begin
  if (miTimeLine.Checked) and not(Assigned(fmTimeLine)) then begin
    fmTimeLine := TfmTimeLine.Create(nil);
    fmTimeLine.Left := 10;
    fmTimeLine.Top := Screen.WorkAreaHeight - fmTimeLine.Height - 10;
    fmTimeLine.Show;
  end else begin
    FreeAndNil(fmTimeLine);
  end;
end;

procedure TfmGEDKeeper.miCalendarClick(Sender: TObject);
begin
  if (miCalendar.Checked) and not(Assigned(fmCalendar)) then begin
    fmCalendar := TfmCalendar.Create(nil);
    fmCalendar.Left := Screen.WorkAreaWidth - fmCalendar.Width - 10;
    fmCalendar.Top := 50;
    fmCalendar.Show;
  end else begin
    FreeAndNil(fmCalendar);
  end;
end;

procedure TfmGEDKeeper.miNamesBookClick(Sender: TObject);
begin
  if (miNamesBook.Checked) and not(Assigned(fmNamesBook)) then begin
    fmNamesBook := TfmNamesBook.Create(nil);
    fmNamesBook.Left := Screen.WorkAreaWidth - fmNamesBook.Width - 10;
    fmNamesBook.Top := (Screen.WorkAreaHeight - fmNamesBook.Height) div 2;
    fmNamesBook.Show;
  end else begin
    FreeAndNil(fmNamesBook);
  end;
end;

procedure TfmGEDKeeper.miCalcClick(Sender: TObject);
begin
  if (miCalc.Checked) and not(Assigned(fmCalcWidget)) then begin
    fmCalcWidget := TfmCalcWidget.Create(nil);
    fmCalcWidget.Left := Screen.WorkAreaWidth - fmCalcWidget.Width - 10;
    fmCalcWidget.Top := Screen.WorkAreaHeight - fmCalcWidget.Height - 10;
    fmCalcWidget.Show;
  end else begin
    FreeAndNil(fmCalcWidget);
  end;
end;

procedure TfmGEDKeeper.miAboutClick(Sender: TObject);
begin
  ShowAbout(Self, AppName, GetFileVersion());
end;

procedure TfmGEDKeeper.miGenResourcesClick(Sender: TObject);
begin
  ShowHelpTopic('::/gkhGenRes.htm');
end;

procedure TfmGEDKeeper.miKinshipTermsClick(Sender: TObject);
begin
  ShowHelpTopic('::/gkhRelations.htm');
end;

procedure TfmGEDKeeper.miFAQClick(Sender: TObject);
begin
  ShowHelpTopic('::/gkhFAQ.htm');
end;

procedure TfmGEDKeeper.ShowHelpTopic(aTopic: string = '');
begin
  HtmlHelp(Handle, PChar(GetAppPath() + '/GEDKeeper.chm' + aTopic), HH_DISPLAY_TOPIC, 0);
end;

procedure TfmGEDKeeper.miContextClick(Sender: TObject);
begin
  ShowHelpTopic();
end;

procedure TfmGEDKeeper.miWinCascadeClick(Sender: TObject);
begin
  Cascade;
end;

procedure TfmGEDKeeper.miWinHTileClick(Sender: TObject);
begin
  TileMode := tbHorizontal;
  Tile;
end;

procedure TfmGEDKeeper.miWinVTileClick(Sender: TObject);
begin
  TileMode := tbVertical;
  Tile;
end;

procedure TfmGEDKeeper.miWinMinimizeClick(Sender: TObject);
var
  I: Integer;
begin
  for I := MDIChildCount - 1 downto 0 do
    MDIChildren[I].WindowState := wsMinimized;
end;

procedure TfmGEDKeeper.miWinArrangeClick(Sender: TObject);
begin
  ArrangeIcons();
end;

procedure TfmGEDKeeper.miFilterClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.SetFilter();
end;

procedure TfmGEDKeeper.tbPrevClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.NavPrev();
end;

procedure TfmGEDKeeper.tbNextClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.NavNext();
end;

procedure TfmGEDKeeper.miStatsClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowStats();
end;

procedure TfmGEDKeeper.miFileNewClick(Sender: TObject);
begin
  CreateBase('');
end;

procedure TfmGEDKeeper.miFileLoadClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := fmGEDKeeper.Options.LastDir;
  if OpenDialog1.Execute
  then CreateBase(OpenDialog1.FileName);
end;

procedure TfmGEDKeeper.miFileSaveClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base = nil) then Exit;

  SaveDialog1.FileName := cur_base.FileName;
  if SaveDialog1.Execute
  then cur_base.FileSave(SaveDialog1.FileName);
end;

procedure TfmGEDKeeper.miUndoClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.DoUndo();
end;

procedure TfmGEDKeeper.miRedoClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.DoRedo();
end;

procedure TfmGEDKeeper.miTreeAncestorsClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowTreeAncestors();
end;

procedure TfmGEDKeeper.miTreeDescendantsClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowTreeDescendants();
end;

procedure TfmGEDKeeper.miPedigree_dAbovilleClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.GenPedigree_dAboville();
end;

procedure TfmGEDKeeper.miPedigree_KonovalovClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.GenPedigree_Konovalov();
end;

procedure TfmGEDKeeper.miRecordAddClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.RecordAdd();
end;

procedure TfmGEDKeeper.miRecordEditClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.RecordEdit(Sender);
end;

procedure TfmGEDKeeper.miRecordDeleteClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.RecordDelete();
end;

procedure TfmGEDKeeper.miExportToExcelAppClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ExportToExcel(True);
end;

procedure TfmGEDKeeper.miTreeBothClick(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowTreeBoth();
end;

procedure TfmGEDKeeper.LoadLanguage(LangCode: Integer);
var
  st, lfn: string;
  lng_file: TextFile;
  i: Integer;
  intf: ILocalization;
begin
  if (LangCode <> LSDefCode) then begin
    lfn := '';
    for i := 0 to FOptions.LangsCount - 1 do begin
      if (FOptions.Langs[i].Code = LangCode) then begin
        lfn := FOptions.Langs[i].FileName;
        Break;
      end;
    end;

    if (lfn <> '') and (FileExists(lfn)) then begin
      AssignFile(lng_file, lfn);
      try
        Reset(lng_file);
        Readln(lng_file); // skip first line
        i := 0;
        while not Eof(lng_file) do begin
          Readln(lng_file, st);
          Inc(i);
          LSList[i] := Utf8ToAnsi(Trim(st));
        end;
      finally
        CloseFile(lng_file);
      end;
    end else begin
      LangCode := LSDefCode;
    end;
  end;

  if (LangCode = LSDefCode) then begin
    for i := LSID_First to LSID_Last do LSList[i] := LSDefList[i];
  end;

  for i := 0 to Screen.FormCount - 1 do
    if Supports(Screen.Forms[i], ILocalization, intf)
    then intf.SetLang();

  FOptions.InterfaceLang := LangCode;
end;

procedure TfmGEDKeeper.SetLang();
begin
  miFile.Caption := LSList[LSID_MIFile];
  miEdit.Caption := LSList[LSID_MIEdit];
  miPedigree.Caption := LSList[LSID_MIPedigree];
  miService.Caption := LSList[LSID_MIService];
  miWindow.Caption := LSList[LSID_MIWindow];
  miHelp.Caption := LSList[LSID_MIHelp];

  miFileNew.Caption := LSList[LSID_MIFileNew];
  miFileLoad.Caption := LSList[LSID_MIFileLoad];
  miMRUFiles.Caption := LSList[LSID_MIMRUFiles];
  miFileSave.Caption := LSList[LSID_MIFileSave];
  miFileClose.Caption := LSList[LSID_MIFileClose];
  miFileProperties.Caption := LSList[LSID_MIFileProperties];
  miExport.Caption := LSList[LSID_MIExport];
  miExportToWeb.Caption := LSList[LSID_MIExportToWeb];
  miExportToExcelApp.Caption := LSList[LSID_MIExportToExcelApp];
  miExportToExcelFile.Caption := LSList[LSID_MIExportToExcelFile];
  miExit.Caption := LSList[LSID_MIExit];

  miUndo.Caption := LSList[LSID_MIUndo];
  miRedo.Caption := LSList[LSID_MIRedo];
  miRecordAdd.Caption := LSList[LSID_MIRecordAdd];
  miRecordEdit.Caption := LSList[LSID_MIRecordEdit];
  miRecordDelete.Caption := LSList[LSID_MIRecordDelete];
  miStreamInput.Caption := LSList[LSID_MIStreamInput] + '...';

  miTreeAncestors.Caption := LSList[LSID_MITreeAncestors];
  miTreeDescendants.Caption := LSList[LSID_MITreeDescendants];
  miTreeBoth.Caption := LSList[LSID_MITreeBoth];
  miPedigree_dAboville.Caption := LSList[LSID_MIPedigree_dAboville];
  miPedigree_Konovalov.Caption := LSList[LSID_MIPedigree_Konovalov];
  miMap.Caption := LSList[LSID_MIMap] + '...';
  miStats.Caption := LSList[LSID_MIStats] + '...';

  miCalc.Caption := LSList[LSID_MICalc] + '...';
  miNamesBook.Caption := LSList[LSID_MINamesBook] + '...';
  miCalendar.Caption := LSList[LSID_MICalendar] + '...';
  miTimeLine.Caption := LSList[LSID_MITimeLine] + '...';
  miOrganizer.Caption := LSList[LSID_MIOrganizer] + '...';
  miScripts.Caption := LSList[LSID_MIScripts];
  miDBImport.Caption := LSList[LSID_MIDBImport];
  miTreeTools.Caption := LSList[LSID_MITreeTools];
  miFilter.Caption := LSList[LSID_MIFilter] + '...';
  miOptions.Caption := LSList[LSID_MIOptions] + '...';

  miWinCascade.Caption := LSList[LSID_MIWinCascade];
  miWinHTile.Caption := LSList[LSID_MIWinHTile];
  miWinVTile.Caption := LSList[LSID_MIWinVTile];
  miWinMinimize.Caption := LSList[LSID_MIWinMinimize];
  miWinArrange.Caption := LSList[LSID_MIWinArrange];

  miGenResources.Caption := LSList[LSID_MIGenResources];
  miKinshipTerms.Caption := LSList[LSID_MIKinshipTerms];
  miFAQ.Caption := LSList[LSID_MIFAQ];
  miContext.Caption := LSList[LSID_MIContext];
  miAbout.Caption := LSList[LSID_MIAbout] + '...';
end;

end.
