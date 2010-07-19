unit GKPluginMan;

interface

uses
  Windows, SysUtils, Classes, Graphics, GKPluginCommon;

type
  TPluginModule = class
    FileName: string;
    ProcCount: Integer;
    Instance: LongInt;
    Address: Integer;
  end;

type
  TPluginProc = class
    ProcName: string;
    Name: string;
    Hint: string;
    Info: string;
    Address: Integer;
    ResID: string;
  end;

type
  TPluginError = (peNoProc, peNoUnloadProc, peNoModule);

  TPluginErrorProc = procedure(Error: TPluginError) of object;

  TPluginLoad = procedure(index: Integer; Bitmap: TBitmap; PluginProc: TPluginProc) of object;

  TPluginUnLoad = procedure(index: Integer) of object;

  TPlugInInit = function(Owner: Integer): PPluginData;

  TPluginExecute = procedure;

  TPluginRegister = function(Index, InfoType: Integer): PChar;

  TProcInfo = function(Index, InfoType: Integer): PChar;

  TPluginInfo = function: PChar;

  TUnloading = procedure;

  TPluginMan = class(TComponent)
  private
    FUnload, FRegister, FInit: String;
    FDir, FExt: string;
    FProcList: TList;
    FFileList: TStringList;
    FCount, FIndex: Integer;
    BM: TBitmap;
    FPluginLoad: TPluginLoad;
    FPluginUnLoad: TPluginUnLoad;
    FPluginError: TPluginErrorProc;
    FFileCount: Integer;
    FModuleList: TStringList;

    procedure ScanDir(Dir, Extension: string);
    function CalculateFirstIndex(Module: string): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadPlugin(Module: string);
    procedure LoadAll();
    procedure RunPlugin(Index: Integer);
    procedure ScanDirectory();
    procedure UnloadAll();
    procedure UnloadModule(Module: string);

    property OnLoadPlugin: TPluginLoad read FPluginLoad write FPluginLoad;
    property OnUnloadPlugin: TPluginUnload read FPLuginUnload write FPluginUnload;
    property OnError: TPluginErrorProc read FPluginError write FPluginError;
    property Count: Integer read FCount;
    property Directory: string read FDir write FDir;
  end;

implementation

{ TPluginMan }

constructor TPluginMan.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInit := 'Initialize';
  FRegister := 'RegisterProcs';
  FUnload := 'Unload';
  FExt := 'dll';
  FDir := '';
  FProcList := TList.Create;
  FFileList := TStringList.Create;
  FModuleList := TStringList.Create;
  FIndex := 0;
  FCount := 0;
  FFileCount := 0;
  BM := TBitmap.Create;
end;

destructor TPluginMan.Destroy;
begin
  FProcList.Free;
  FFileList.Free;
  FModuleList.Free;
  BM.Free;
  inherited Destroy;
end;

procedure TPluginMan.LoadPlugin(Module: string);
var
  PlugMod: TPluginModule;
  PlugProc: TPluginProc;
  PluginData: PPluginData;
  PlugReg: TPluginRegister;
  i: Integer;
begin
  if (FModuleList.IndexOf(Module) > -1) then Exit;
  PlugMod := TPluginModule.Create;
  PlugMod.FileName := Module;
  PlugMod.Address := LoadLibrary (PChar (Module) );
  PluginData := TPluginInit( GetProcAddress( PlugMod.Address, PChar(FInit)))(HInstance);
  PlugMod.ProcCount := PluginData^.Count;
  PlugMod.Instance := PluginData^.Instance;
  if PlugMod.ProcCount < 1 then begin
    if Assigned(FPluginError) then FPluginError(peNoProc);
    PlugMod.Free;
    Exit;
  end;
  FModuleList.AddObject(Module,TObject(PlugMod));
  Inc(FFileCount);

  for i := 0 to PlugMod.ProcCount - 1 do begin
    PlugProc := TPluginProc.Create;
    PlugProc.Address := PlugMod.Address;
    PlugReg := TPluginRegister(GetProcAddress(PlugProc.Address, PChar(FRegister)));
    PlugProc.ProcName := PlugReg(i, itProcName);
    PlugProc.Name := PlugReg(i, itName);
    PlugProc.Hint := PlugReg(i, itHint);
    PlugProc.Info := PlugReg(i, itInfo);
    PlugProc.ResID := PlugReg(i, itResID);
    if PlugProc.ResID <> '' then
      BM.Handle := LoadBitmap(PlugMod.Instance, PChar(PlugProc.ResID));
    FProcList.Add(PlugProc);
    if Assigned(FPluginLoad) then
      FPluginLoad(FIndex, BM, PlugProc);
    Inc(FIndex);
    Inc(FCount);
  end;
end;

procedure TPluginMan.UnloadModule(Module: string);
var
  count, n, index, i: Integer;
  PM: TPluginModule;
  UnloadProc: TUnloading;
begin
  index := FModuleList.IndexOf(Module);
  if (index < 0) then Exit;
  i := CalculateFirstIndex(Module);
  PM := TPluginModule(FModuleList.Objects[index]);
  count := PM.ProcCount;

  for n := i to i + count - 1 do begin
    if (n >= 0) and (n < FProcList.Count) then begin
      Dec(FCount);
      if Assigned(FPluginUnload) then FPluginUnload(n);
    end;
  end;

  try
    UnloadProc := TUnloading(GetProcAddress(PM.Instance, PChar(FUnload)));
    if Assigned(UnloadProc) then UnloadProc();
  except
  end;

  FreeLibrary(PM.Address);
  FModuleList.Delete(index);
end;

procedure TPluginMan.LoadAll();
var
  i: Integer;
begin
  for i := 0 to FFileList.Count - 1 do
    LoadPlugin(FFileList[i]);
end;

procedure TPluginMan.UnloadAll();
var
  i: integer;
begin
  for i := FFileList.Count - 1 downto 0 do
    UnloadModule(FFileList[i]);
end;

procedure TPluginMan.RunPlugin(Index: Integer);
begin
  if (Index < 0) or (Index >= FProcList.Count) then Exit;

  TPluginExecute(GetProcAddress(
    TPluginProc(FProcList[Index]).Address, PChar(TPluginProc(FProcList[Index]).ProcName)));
end;

procedure TPluginMan.ScanDir(Dir, Extension: string);
var
  Found: TSearchRec;
  i, Finished: Integer;
  Dirs: TStrings;
begin
  Dirs := TStringList.Create;
  try
    Finished := FindFirst(Dir + '*.*', 63, Found);
    while (Finished = 0) do begin
      if (Found.Name[1] <> '.') then begin
        if (Found.Attr and faDirectory = faDirectory)
        then Dirs.Add(Dir + Found.Name)
        else
          if Pos(UpperCase(Extension), UpperCase(Found.Name))>0 then
            FFileList.Add(Dir + Found.Name);
      end;
      Finished := FindNext(Found);
    end;
    FindClose(Found);

    for i := 0 to Dirs.Count - 1 do
      ScanDir(Dirs[i], Extension);
  finally
    Dirs.Free;
  end;
end;

procedure TPluginMan.ScanDirectory();
begin
  ScanDir(FDir, '.' + FExt);
end;

function TPluginMan.CalculateFirstIndex(Module: string): Integer;
var
  n, i, count: Integer;
begin
  Result := -1;
  count := 0;
  i := FModuleList.IndexOf(module);
  if i < 0 then Exit;

  for n := 0 to i - 1 do
    count := count + TPluginModule(FModuleList.Objects[i]).ProcCount;

  Result := count;
end;

end.
