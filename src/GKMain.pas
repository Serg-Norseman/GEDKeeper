unit GKMain;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Forms, Controls, Menus,
  StdCtrls, Dialogs, Buttons, Messages, ExtCtrls, ComCtrls, StdActns, ActnList,
  ToolWin, ImgList, GKEngine, GKCommon, GKBase, GKLangs, GKPluginMan;

const
  WM_KEEPMODELESS = WM_USER + 111;

type
  TfmGEDKeeper = class(TForm, ILocalization)
    StatusBar: TStatusBar;
    actWinCascade: TWindowCascade;
    actWinHTile: TWindowTileHorizontal;
    actWinArrange: TWindowArrange;
    actWinMinimize: TWindowMinimizeAll;
    actWinVTile: TWindowTileVertical;
    ActionList1: TActionList;
    actFileNew: TAction;
    actFileLoad: TAction;
    actFileSave: TAction;
    actExit: TAction;
    actRecordAdd: TAction;
    actRecordEdit: TAction;
    actRecordDelete: TAction;
    actFileProperties: TAction;
    actOptions: TAction;
    actTreeAncestors: TAction;
    actTreeDescendants: TAction;
    actPedigree_dAboville: TAction;
    actPedigree_Konovalov: TAction;
    actFilter: TAction;
    actStats: TAction;
    actExportToWeb: TAction;
    actExportToExcel: TAction;
    actTreeTools: TAction;
    actContextHelp: TAction;
    actAbout: TAction;
    actMap: TAction;
    actGenResources: TAction;
    actKinshipTerms: TAction;
    actPrev: TAction;
    actNext: TAction;
    actFileClose: TAction;
    ImageList1: TImageList;
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
    miDocAuthor: TMenuItem;
    N2: TMenuItem;
    miExportToWeb: TMenuItem;
    miExportToExcel: TMenuItem;
    N3: TMenuItem;
    miTreeTools: TMenuItem;
    N4: TMenuItem;
    miExit: TMenuItem;
    miEdit: TMenuItem;
    miRecordAdd: TMenuItem;
    miRecordEdit: TMenuItem;
    miRecordDelete: TMenuItem;
    N5: TMenuItem;
    miPersonScan: TMenuItem;
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
    actPersonScan: TAction;
    ToolButton7: TToolButton;
    tbUndo: TToolButton;
    tbRedo: TToolButton;
    actUndo: TAction;
    actRedo: TAction;
    miFAQ: TMenuItem;
    actFAQ: TAction;
    ImageList2: TImageList;
    miCalc: TMenuItem;
    actExpCalc: TAction;
    actNamesBook: TAction;
    miNamesBook: TMenuItem;
    miCalendar: TMenuItem;
    actCalendar: TAction;
    actTimeLine: TAction;
    miTimeLine: TMenuItem;
    actStereoView: TAction;
    actOrganizer: TAction;
    miOrganizer: TMenuItem;
    actDBImport: TAction;
    miDBImport: TMenuItem;
    miService: TMenuItem;
    N12: TMenuItem;
    miUndo: TMenuItem;
    miRedo: TMenuItem;
    miSourceParse: TMenuItem;
    actSourceParse: TAction;
    miScripts: TMenuItem;
    actScripts: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileLoadExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actFilePropertiesExecute(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure actContextHelpExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actGenResourcesExecute(Sender: TObject);
    procedure actKinshipTermsExecute(Sender: TObject);
    procedure actExportToWebExecute(Sender: TObject);
    procedure actExportToExcelExecute(Sender: TObject);
    procedure actTreeToolsExecute(Sender: TObject);
    procedure actRecordAddExecute(Sender: TObject);
    procedure actRecordEditExecute(Sender: TObject);
    procedure actRecordDeleteExecute(Sender: TObject);
    procedure actFilterExecute(Sender: TObject);
    procedure actTreeAncestorsExecute(Sender: TObject);
    procedure actTreeDescendantsExecute(Sender: TObject);
    procedure actPedigree_dAbovilleExecute(Sender: TObject);
    procedure actPedigree_KonovalovExecute(Sender: TObject);
    procedure actMapExecute(Sender: TObject);
    procedure actStatsExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actPrevExecute(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure actPersonScanExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actUndoExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actFAQExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure StatusBarDblClick(Sender: TObject);
    procedure actExpCalcExecute(Sender: TObject);
    procedure actNamesBookExecute(Sender: TObject);
    procedure actCalendarExecute(Sender: TObject);
    procedure actTimeLineExecute(Sender: TObject);
    procedure actStereoViewExecute(Sender: TObject);
    procedure actOrganizerExecute(Sender: TObject);
    procedure actDBImportExecute(Sender: TObject);
    procedure actSourceParseExecute(Sender: TObject);
    procedure actScriptsExecute(Sender: TObject);
  private
    FNamesTable: TNamesTable;
    FOptions: TGlobalOptions;
    FPluginMan: TPluginMan;

    procedure MRUFileClick(Sender: TObject);
    procedure UpdateMRU();

    procedure ExecWidget(Action: TAction; WidgetClass: TFormClass; var Widget: TForm);

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

    procedure SetLang(LangID: TLangID);
  end;

var
  fmGEDKeeper: TfmGEDKeeper;

function ShowModalEx(aForm: TCustomForm; aPopupParent: TCustomForm = nil;
  KeepModeless: Boolean = False): Integer;

implementation

uses
  {$IFDEF DELPHI_NET}System.IO,{$ENDIF}
  {$IFDEF PROFILER}ZProfiler,{$ENDIF}
  uVista, ShellAPI, bsComUtils, GKUtils, 
  GKAbout, GKOptions, GKExpCalc, GKNamesBook, GKCalendar, GKTimeLine;

{$R *.dfm}

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

{ TfmGEDKeeper }

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

  FNamesTable := TNamesTable.Create;
  FNamesTable.LoadFromFile(GetAppPath() + 'GEDKeeper.nms');

  FPluginMan := TPluginMan.Create(Self);
  FPluginMan.Directory := ExtractFilePath(Application.ExeName) + '\plugins\';
  FPluginMan.ScanDirectory();
  FPluginMan.LoadAll();

  DragAcceptFiles(Handle, True);

  UpdateMRU();
  UpdateControls();
end;

procedure TfmGEDKeeper.FormDestroy(Sender: TObject);
begin
  FPluginMan.UnloadAll();
  FPluginMan.Destroy;

  FNamesTable.SaveToFile(GetAppPath() + 'GEDKeeper.nms');
  FNamesTable.Destroy;

  FOptions.SaveToFile(GetAppPath() + 'GEDKeeper.ini');
  FOptions.Destroy;

  {$IFDEF PROFILER}DoneProfiler();{$ENDIF}
  LogDone();
end;

procedure TfmGEDKeeper.FileDrop(var Msg: TWMDROPFILES);
var
  numFiles: Longint;
  buffer: array [0..MAX_PATH] of Char;
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
  i: integer;
  len: integer;
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

    actFileClose.Enabled := (base_en);
    actFileSave.Enabled := (base_en);

    actFileProperties.Enabled := (base_en);
    actExportToWeb.Enabled := (base_en);
    actExportToExcel.Enabled := (base_en);
    actTreeTools.Enabled := (base_en);

    actRecordAdd.Enabled := (base_en);
    actRecordEdit.Enabled := (base_en);
    actRecordDelete.Enabled := (base_en);
    actPersonScan.Enabled := (base_en);

    indiv_en := (base_en) and (rt = rtIndividual);

    actFilter.Enabled := (indiv_en);
    miPedigree.Enabled := (indiv_en);
    tbPedigree.Enabled := (indiv_en);
    actTreeAncestors.Enabled := (indiv_en);
    actTreeDescendants.Enabled := (indiv_en);
    actPedigree_dAboville.Enabled := (indiv_en);
    actPedigree_Konovalov.Enabled := (indiv_en);

    actStats.Enabled := (base_en);
    actOrganizer.Enabled := (base_en);
    actDBImport.Enabled := (base_en);
    actScripts.Enabled := (base_en);

    actPrev.Enabled := (cur_base <> nil) and (cur_base.Backman.CanBackward());
    actNext.Enabled := (cur_base <> nil) and (cur_base.Backman.CanForward());

    test_funcs := IsDevComp();

    {$IFDEF FEATURES_INC}
    actUndo.Enabled := (test_funcs) and (cur_base <> nil) and (cur_base.Undoman.CanUndo());
    actRedo.Enabled := (test_funcs) and (cur_base <> nil) and (cur_base.Undoman.CanRedo());
    actSourceParse.Enabled := (test_funcs) and (cur_base <> nil);
    actStereoView.Enabled := (test_funcs) and (cur_base <> nil);
    {$ELSE}
    actUndo.Enabled := False;
    actRedo.Enabled := False;
    actSourceParse.Enabled := False;
    actStereoView.Enabled := False;
    {$ENDIF}

    if (cur_base <> nil) then begin
      st := 'Записей: ' + IntToStr(cur_base.FCounts[rt].Total);
      if (rt = rtIndividual)
      then st := st + ', фильтр: ' + IntToStr(cur_base.FCounts[rt].Filtered);

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

procedure TfmGEDKeeper.actFileNewExecute(Sender: TObject);
begin
  CreateBase('');
end;

procedure TfmGEDKeeper.actFileLoadExecute(Sender: TObject);
begin
  OpenDialog1.InitialDir := fmGEDKeeper.Options.LastDir;
  if OpenDialog1.Execute
  then CreateBase(OpenDialog1.FileName);
end;

procedure TfmGEDKeeper.actFileSaveExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base = nil) then Exit;

  SaveDialog1.FileName := cur_base.FileName;
  if SaveDialog1.Execute
  then cur_base.FileSave(SaveDialog1.FileName);
end;

procedure TfmGEDKeeper.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmGEDKeeper.actFilePropertiesExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.FileProperties();
end;

procedure TfmGEDKeeper.actFileCloseExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.Close();
end;

procedure TfmGEDKeeper.actContextHelpExecute(Sender: TObject);
begin
  LoadExtFile(GetAppPath() + 'help\GEDKeeper.htm');
end;

procedure TfmGEDKeeper.actAboutExecute(Sender: TObject);
begin
  AboutDialog();
end;

procedure TfmGEDKeeper.actGenResourcesExecute(Sender: TObject);
begin
  LoadExtFile(GetAppPath() + 'help\genres.htm');
end;

procedure TfmGEDKeeper.actKinshipTermsExecute(Sender: TObject);
begin
  LoadExtFile(GetAppPath() + 'help\relations.htm');
end;

procedure TfmGEDKeeper.actExportToWebExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ExportToWeb();
end;

procedure TfmGEDKeeper.actExportToExcelExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ExportToExcel();
end;

procedure TfmGEDKeeper.actTreeToolsExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.TreeTools();
end;

procedure TfmGEDKeeper.actRecordAddExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.RecordAdd();
end;

procedure TfmGEDKeeper.actRecordEditExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.RecordEdit(Sender);
end;

procedure TfmGEDKeeper.actRecordDeleteExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.RecordDelete();
end;

procedure TfmGEDKeeper.actFilterExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.SetFilter();
end;

procedure TfmGEDKeeper.actTreeAncestorsExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowTreeAncestors();
end;

procedure TfmGEDKeeper.actTreeDescendantsExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowTreeDescendants();
end;

procedure TfmGEDKeeper.actPedigree_dAbovilleExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.GenPedigree_dAboville();
end;

procedure TfmGEDKeeper.actPedigree_KonovalovExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.GenPedigree_Konovalov();
end;

procedure TfmGEDKeeper.actMapExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowMap();
end;

procedure TfmGEDKeeper.actSourceParseExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.SourceParse();
end;

procedure TfmGEDKeeper.actStatsExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowStats();
end;

procedure TfmGEDKeeper.actStereoViewExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowStereoView();
end;

procedure TfmGEDKeeper.actOptionsExecute(Sender: TObject);
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

procedure TfmGEDKeeper.actPrevExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.NavPrev();
end;

procedure TfmGEDKeeper.actNextExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.NavNext();
end;

procedure TfmGEDKeeper.actPersonScanExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.PersonScan();
end;

procedure TfmGEDKeeper.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
  base: TfmBase;
begin
  Action := caFree;

  for i := MDIChildCount - 1 downto 0 do begin
    if (MDIChildren[i] is TfmBase) then begin
      base := TfmBase(MDIChildren[i]);

      if not(base.CheckModified()) then begin
        Action := caNone;
        Exit;
      end else base.Free;
    end else MDIChildren[i].Free;
  end;
end;

procedure TfmGEDKeeper.SetLang(LangID: TLangID);
begin
  miFile.Caption := GetLangStr(LSID_MIFile);
  miEdit.Caption := GetLangStr(LSID_MIEdit);
  miPedigree.Caption := GetLangStr(LSID_MIPedigree);
  miWindow.Caption := GetLangStr(LSID_MIWindow);
  miHelp.Caption := GetLangStr(LSID_MIHelp);

  miMRUFiles.Caption := GetLangStr(LSID_MIMRUFiles);

  //tbPedigree.Hint :=
end;

procedure TfmGEDKeeper.actUndoExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.DoUndo();
end;

procedure TfmGEDKeeper.actRedoExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.DoRedo();
end;

procedure TfmGEDKeeper.actFAQExecute(Sender: TObject);
begin
  LoadExtFile(GetAppPath() + 'help\faq.htm');
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
  then ImageList2.Draw(StatusBar.Canvas, Rect.Left, Rect.Top, Ord(cur_base.ShieldState));
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

procedure TfmGEDKeeper.ExecWidget(Action: TAction; WidgetClass: TFormClass; var Widget: TForm);
begin
  if (Action.Checked) and not(Assigned(Widget)) then begin
    {FPluginMan.RunPlugin(0);}
    Widget := WidgetClass.Create(nil);
    Widget.Left := Screen.WorkAreaWidth - Widget.Width - 10; {!!!}
    Widget.Top := Screen.WorkAreaHeight - Widget.Height - 10; {!!!}
    Widget.Show;
  end else begin
    FreeAndNil(Widget);
    {FPluginMan.RunPlugin(1);}
  end;

  // если хранить список открытых виджетов, то можно сделать динамическую
  // установку их один над другим по мере подключения
end;

procedure TfmGEDKeeper.actExpCalcExecute(Sender: TObject);
begin
  if (actExpCalc.Checked) and not(Assigned(fmCalcWidget)) then begin
    {FPluginMan.RunPlugin(0);}
    fmCalcWidget := TfmCalcWidget.Create(nil);
    fmCalcWidget.Left := Screen.WorkAreaWidth - fmCalcWidget.Width - 10;
    fmCalcWidget.Top := Screen.WorkAreaHeight - fmCalcWidget.Height - 10;
    fmCalcWidget.Show;
  end else begin
    FreeAndNil(fmCalcWidget);
    {FPluginMan.RunPlugin(1);}
  end;
end;

procedure TfmGEDKeeper.actNamesBookExecute(Sender: TObject);
begin
  if (actNamesBook.Checked) and not(Assigned(fmNamesBook)) then begin
    {FPluginMan.RunPlugin(0);}
    fmNamesBook := TfmNamesBook.Create(nil);
    fmNamesBook.Left := Screen.WorkAreaWidth - fmNamesBook.Width - 10;
    fmNamesBook.Top := (Screen.WorkAreaHeight - fmNamesBook.Height) div 2;
    fmNamesBook.Show;
  end else begin
    FreeAndNil(fmNamesBook);
    {FPluginMan.RunPlugin(1);}
  end;
end;

procedure TfmGEDKeeper.actCalendarExecute(Sender: TObject);
begin
  if (actCalendar.Checked) and not(Assigned(fmCalendar)) then begin
    {FPluginMan.RunPlugin(0);}
    fmCalendar := TfmCalendar.Create(nil);
    fmCalendar.Left := Screen.WorkAreaWidth - fmCalendar.Width - 10;
    fmCalendar.Top := 50;
    fmCalendar.Show;
  end else begin
    FreeAndNil(fmCalendar);
    {FPluginMan.RunPlugin(1);}
  end;
end;

procedure TfmGEDKeeper.actTimeLineExecute(Sender: TObject);
begin
  if (actTimeLine.Checked) and not(Assigned(fmTimeLine)) then begin
    {FPluginMan.RunPlugin(0);}
    fmTimeLine := TfmTimeLine.Create(nil);
    fmTimeLine.Left := 10;
    fmTimeLine.Top := Screen.WorkAreaHeight - fmTimeLine.Height - 10;
    fmTimeLine.Show;
  end else begin
    FreeAndNil(fmTimeLine);
    {FPluginMan.RunPlugin(1);}
  end;
end;

procedure TfmGEDKeeper.actOrganizerExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowOrganizer();
end;

procedure TfmGEDKeeper.actDBImportExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ImportDB();
end;

procedure TfmGEDKeeper.actScriptsExecute(Sender: TObject);
var
  cur_base: TfmBase;
begin
  cur_base := GetCurrentFile();
  if (cur_base <> nil) then cur_base.ShowScriptDaemon();
end;

end.
