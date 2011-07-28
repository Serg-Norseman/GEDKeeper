unit GKCommon; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.IO, System.Drawing, System.ComponentModel, System.Windows.Forms,
  System.Text, VCLStub, GedCom551, GKEngine, GKUtils, GKLangs;

type
  TBackManager = class(TObject)
  private
    FNavBusy: Boolean;
    FStackBackward, FStackForward: TStack;
    FCurrent: TObject;
    procedure SetCurrent(const Value: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    function Back(): TObject;
    function Next(): TObject;
    procedure Clear();

    procedure BeginNav();
    procedure EndNav();

    function CanBackward(): Boolean;
    function CanForward(): Boolean;

    property Busy: Boolean read FNavBusy;
    property Current: TObject read FCurrent write SetCurrent;
  end;

type
  TUndoManager = class;

  TCustomCommand = class
  private
  protected
    FManager: TUndoManager;
  public
    constructor Create(aManager: TUndoManager); virtual;

    function Redo(): Boolean; virtual; abstract;
    procedure Undo(); virtual; abstract;
  end;

  TUndoManager = class(TObject)
  private
    type
      TTransactionEventArg = (taCommit, taCommitUndo, taCommitRedo, taRollback);
      TUndoManType = (autoCommit, manualCommit);
      TTransactionEvent = procedure(Sender: TObject; Arg: TTransactionEventArg) of object;

  private
    FDepth: Integer;
    FOnTransaction: TTransactionEvent;
    FStackUndo: TStack;
    FStackRedo: TStack;
    FTree: TGEDCOMTree;
    FType: TUndoManType;
  protected
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure Transaction(Arg: TTransactionEventArg);
  public
    constructor Create(aTree: TGEDCOMTree; aType: TUndoManType);
    destructor Destroy; override;

    function CmdDo(cmd: TCustomCommand): Boolean;
    procedure CmdUndo();
    procedure CmdRedo();

    function CanUndo(): Boolean;
    function CanRedo(): Boolean;

    procedure Commit();
    procedure Rollback();

    procedure Clear();

    property Depth: Integer read FDepth write FDepth;
    property OnTransaction: TTransactionEvent read FOnTransaction write FOnTransaction;
    property Tree: TGEDCOMTree read FTree;
  end;

type
  TNamesTable = class(TObject)
  public type
    TName = class
    public
      Name: string;
      F_Patronymic: string;
      M_Patronymic: string;
      Sex: TGEDCOMObject.TGEDCOMSex;
    end;

  private
    FNames: TObjectList;

    function Comparable(aName, aPatronymic: string): Boolean;
    function GetName(Index: Integer): TName;
    function GetNameCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ImportNames(aTree: TGEDCOMTree);
    procedure LoadFromFile(const aFileName: string);
    procedure SaveToFile(const aFileName: string);

    property NameCount: Integer read GetNameCount;
    property Names[Index: Integer]: TName read GetName;

    function AddName(aName: string): TName;
    function FindName(aName: string): TName;
    function GetPatronymicByName(aName: string; aSex: TGEDCOMObject.TGEDCOMSex): string;
    function GetNameByPatronymic(aPatronymic: string; aSex: TGEDCOMObject.TGEDCOMSex): string;

    function GetSexByName(aName: string): TGEDCOMObject.TGEDCOMSex;

    procedure SetName(aName, aPatronymic: string; aSex: TGEDCOMObject.TGEDCOMSex);
    procedure SetNameSex(aName: string; aSex: TGEDCOMObject.TGEDCOMSex);
  end;

  TChartOptions = class(TObject)
  private
    FChildlessExclude: Boolean;
    FDecorative: Boolean;
    FFamilyVisible: Boolean;
    FNameVisible: Boolean;
    FPatronymicVisible: Boolean;
    FDiffLines: Boolean;
    FBirthDateVisible: Boolean;
    FDeathDateVisible: Boolean;
    FOnlyYears: Boolean;
    FKinship: Boolean;
    FPortraitsVisible: Boolean;
    FSignsVisible: Boolean;

    FMaleColor: System.Drawing.Color;
    FFemaleColor: System.Drawing.Color;
    FUnkSexColor: System.Drawing.Color;
    FUnHusbandColor: System.Drawing.Color;
    FUnWifeColor: System.Drawing.Color;

    FDefFont_Name: string;
    FDefFont_Size: Integer;
    FDefFont_Color: System.Drawing.Color;
    FDefFont_Style: System.Drawing.FontStyle;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const aIniFile: TIniFile);
    procedure SaveToFile(const aIniFile: TIniFile);

    property ChildlessExclude: Boolean read FChildlessExclude write FChildlessExclude;

    property Decorative: Boolean read FDecorative write FDecorative;
    property FamilyVisible: Boolean read FFamilyVisible write FFamilyVisible;
    property NameVisible: Boolean read FNameVisible write FNameVisible;
    property PatronymicVisible: Boolean read FPatronymicVisible write FPatronymicVisible;
    property DiffLines: Boolean read FDiffLines write FDiffLines;
    property BirthDateVisible: Boolean read FBirthDateVisible write FBirthDateVisible;
    property DeathDateVisible: Boolean read FDeathDateVisible write FDeathDateVisible;
    property OnlyYears: Boolean read FOnlyYears write FOnlyYears;
    property Kinship: Boolean read FKinship write FKinship;
    property PortraitsVisible: Boolean read FPortraitsVisible write FPortraitsVisible;
    property SignsVisible: Boolean read FSignsVisible write FSignsVisible;

    property MaleColor: System.Drawing.Color read FMaleColor write FMaleColor;
    property FemaleColor: System.Drawing.Color read FFemaleColor write FFemaleColor;
    property UnkSexColor: System.Drawing.Color read FUnkSexColor write FUnkSexColor;
    property UnHusbandColor: System.Drawing.Color read FUnHusbandColor write FUnHusbandColor;
    property UnWifeColor: System.Drawing.Color read FUnWifeColor write FUnWifeColor;

    property DefFont_Name: string read FDefFont_Name write FDefFont_Name;
    property DefFont_Size: Integer read FDefFont_Size write FDefFont_Size;
    property DefFont_Color: System.Drawing.Color read FDefFont_Color write FDefFont_Color;
    property DefFont_Style: System.Drawing.FontStyle read FDefFont_Style write FDefFont_Style;
  end;

  TProxy = class(TObject)
  private
    FServer: string;
    FPort: string;
    FLogin: string;
    FPassword: string;
    FUseProxy: Boolean;
  public
    constructor Create;

    procedure LoadFromFile(const aIniFile: TIniFile);
    procedure SaveToFile(const aIniFile: TIniFile);

    property Server: string read FServer write FServer;
    property Port: string read FPort write FPort;
    property Login: string read FLogin write FLogin;
    property Password: string read FPassword write FPassword;
    property UseProxy: Boolean read FUseProxy write FUseProxy;
  end;

  TPedigreeOptions = class(TObject)
  public type
    TPedigreeFormat = (pfExcess, pfCompact);
  private
    FFormat: TPedigreeFormat;
    FIncludeNotes: Boolean;
    FIncludeAttributes: Boolean;
    FIncludeSources: Boolean;
  public
    constructor Create;

    procedure LoadFromFile(const aIniFile: TIniFile);
    procedure SaveToFile(const aIniFile: TIniFile);

    property Format: TPedigreeFormat read FFormat write FFormat;
    property IncludeAttributes: Boolean read FIncludeAttributes write FIncludeAttributes;
    property IncludeNotes: Boolean read FIncludeNotes write FIncludeNotes;
    property IncludeSources: Boolean read FIncludeSources write FIncludeSources;
  end;

  TGlobalOptions = class(TObject)
  public
    type
      TWorkMode = (wmSimple, wmExpert);

      TLangRecord = class(TObject)
      public
        Code: TLangID;
        Name, FileName: string;
      end;

      TBaseWin = class(TObject)
      public
        FileName: string;
        WinRect: TRect;
        WinState: FormWindowState;
      end;

    type
      TPersonColumnType = (
        pctPatriarch, pctName, pctNick, pctSex, pctBirthDate, pctDeathDate,
        pctBirthPlace, pctDeathPlace, pctResidence,
        pctAge, pctLifeExpectancy, pctDaysForBirth, pctGroups,
        pctReligion, pctNationality, pctEducation, pctOccupation, pctCaste,
        pctMili, pctMiliInd, pctMiliDis, pctMiliRank,
        pctChangeDate, pctBookmark
      );

      TPersonColumnProps = record
        colType: TPersonColumnType;
        colSubType: Byte;
        colActive: Boolean;
      end;

      TPersonColumnsList = array [0..Ord(pctBookmark)] of TPersonColumnProps;

      TColumnRec = record
        Name: LSID;
        DefWidth: Integer;
        colOnlyMain: Boolean;
      end;

    const
      PersonColumnsName: array [TPersonColumnType] of TColumnRec = (
        (Name: LSID_Patriarch;          DefWidth:  25; colOnlyMain: False),
        (Name: LSID_FullName;           DefWidth:  25; colOnlyMain: False),
        (Name: LSID_Nickname;           DefWidth:  75; colOnlyMain: False),
        (Name: LSID_Sex;                DefWidth:  45; colOnlyMain: False),
        (Name: LSID_BirthDate;          DefWidth: 100; colOnlyMain: False),
        (Name: LSID_DeathDate;          DefWidth: 100; colOnlyMain: False),
        (Name: LSID_BirthPlace;         DefWidth: 100; colOnlyMain: False),
        (Name: LSID_DeathPlace;         DefWidth: 100; colOnlyMain: False),
        (Name: LSID_Residence;          DefWidth: 100; colOnlyMain: False),

        (Name: LSID_Age;                DefWidth: 100; colOnlyMain: True),
        (Name: LSID_LifeExpectancy;     DefWidth: 100; colOnlyMain: True),
        (Name: LSID_DaysForBirth;       DefWidth: 100; colOnlyMain: True),
        (Name: LSID_RPGroups;           DefWidth: 200; colOnlyMain: True),

        (Name: LSID_Religion;           DefWidth: 200; colOnlyMain: True),
        (Name: LSID_Nationality;        DefWidth: 200; colOnlyMain: True),
        (Name: LSID_Education;          DefWidth: 200; colOnlyMain: True),
        (Name: LSID_Occupation;         DefWidth: 200; colOnlyMain: True),
        (Name: LSID_Caste;              DefWidth: 200; colOnlyMain: True),

        (Name: LSID_Mili;               DefWidth: 200; colOnlyMain: True),
        (Name: LSID_MiliInd;            DefWidth: 200; colOnlyMain: True),
        (Name: LSID_MiliDis;            DefWidth: 200; colOnlyMain: True),
        (Name: LSID_MiliRank;           DefWidth: 200; colOnlyMain: True),

        (Name: LSID_Changed;            DefWidth: 150; colOnlyMain: True),
        (Name: LSID_Bookmark;           DefWidth:  25; colOnlyMain: True)
      );

    const
      DefPersonColumns: TPersonColumnsList = (
        (colType: pctPatriarch; colActive: True),
        (colType: pctName; colActive: True),
        (colType: pctNick; colActive: False),
        (colType: pctSex; colActive: True),
        (colType: pctBirthDate; colActive: True),
        (colType: pctDeathDate; colActive: True),
        (colType: pctBirthPlace; colActive: True),
        (colType: pctDeathPlace; colActive: True),
        (colType: pctResidence; colActive: True),
        (colType: pctAge; colActive: True),
        (colType: pctLifeExpectancy; colActive: True),
        (colType: pctDaysForBirth; colActive: True),
        (colType: pctGroups; colActive: True),

        (colType: pctReligion; colActive: False),
        (colType: pctNationality; colActive: False),
        (colType: pctEducation; colActive: False),
        (colType: pctOccupation; colActive: False),
        (colType: pctCaste; colActive: False),

        (colType: pctMili; colActive: False),
        (colType: pctMiliInd; colActive: False),
        (colType: pctMiliDis; colActive: False),
        (colType: pctMiliRank; colActive: False),

        (colType: pctChangeDate; colActive: True),
        (colType: pctBookmark; colActive: True)
      );

  private
    FChartOptions: TChartOptions;
    FDefCharacterSet: TGEDCOMObject.TGEDCOMCharacterSet;
    FDefDateFormat: TGenEngine.TDateFormat;
    FDefNameFormat: TGenEngine.TNameFormat;
    FEventFilters: TStringList;
    FInterfaceLang: TLangID;
    FLanguages: TObjectList;
    FLastDir: string;
    FMRUFiles: TStringList;
    FNameFilters: TStringList;
    FPedigreeOptions: TPedigreeOptions;
    FPlacesWithAddress: Boolean;
    FProxy: TProxy;
    FRelations: TStringList;
    FResidenceFilters: TStringList;
    FShowTips: Boolean;
    FWorkMode: TWorkMode;

    FListPersonsColumns: TPersonColumnsList;
    FListPersons_HighlightUnmarried: Boolean;
    FListPersons_HighlightUnparented: Boolean;

    FMWinRect: TRect;
    FMWinState: FormWindowState;

    FLastBases: TObjectList;

    function GetKeyLayout(): Word;
    procedure SetKeyLayout(aLayout: Word);

    procedure LngPrepareProc(const FileName: string);
    function GetLangsCount: Integer;
    function GetLang(Index: Integer): TLangRecord;
    function GetLastBase(Index: Integer): TBaseWin;
    function GetLastBasesCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure FindLanguages();
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    property ChartOptions: TChartOptions read FChartOptions;
    property DefCharacterSet: TGEDCOMObject.TGEDCOMCharacterSet read FDefCharacterSet write FDefCharacterSet;
    property DefDateFormat: TGenEngine.TDateFormat read FDefDateFormat write FDefDateFormat;
    property DefNameFormat: TGenEngine.TNameFormat read FDefNameFormat write FDefNameFormat;
    property EventFilters: TStringList read FEventFilters;
    property InterfaceLang: TLangID read FInterfaceLang write FInterfaceLang;
    property Langs[Index: Integer]: TLangRecord read GetLang;
    property LangsCount: Integer read GetLangsCount;
    property LastDir: string read FLastDir write FLastDir;
    property MRUFiles: TStringList read FMRUFiles;
    property MWinRect: TRect read FMWinRect write FMWinRect;
    property MWinState: FormWindowState read FMWinState write FMWinState;
    property NameFilters: TStringList read FNameFilters;
    property PedigreeOptions: TPedigreeOptions read FPedigreeOptions;
    property PlacesWithAddress: Boolean read FPlacesWithAddress write FPlacesWithAddress;
    property Proxy: TProxy read FProxy;
    property Relations: TStringList read FRelations;
    property ResidenceFilters: TStringList read FResidenceFilters;
    property ShowTips: Boolean read FShowTips write FShowTips;
    property WorkMode: TWorkMode read FWorkMode write FWorkMode;

    property ListPersons_HighlightUnmarried: Boolean
      read FListPersons_HighlightUnmarried write FListPersons_HighlightUnmarried;
    property ListPersons_HighlightUnparented: Boolean
      read FListPersons_HighlightUnparented write FListPersons_HighlightUnparented;

    property ListPersonsColumns: TPersonColumnsList
      read FListPersonsColumns write FListPersonsColumns;

    function AddLastBase(): TBaseWin;
    procedure ClearLastBases();

    property LastBases[Index: Integer]: TBaseWin read GetLastBase;
    property LastBasesCount: Integer read GetLastBasesCount; 
  end;

implementation

{ TBackManager }

constructor TBackManager.Create;
begin
  inherited Create;

  FStackBackward := TStack.Create;
  FStackForward := TStack.Create;
  FCurrent := nil;
end;

destructor TBackManager.Destroy;
begin
  FStackBackward.Free;
  FStackForward.Free;

  inherited Destroy;
end;

function TBackManager.Back(): TObject;
begin
  if Assigned(FCurrent) then FStackForward.Push(FCurrent);
  FCurrent := FStackBackward.Pop();
  Result := FCurrent;
end;

function TBackManager.Next(): TObject;
begin
  if Assigned(FCurrent) then FStackBackward.Push(FCurrent);
  FCurrent := FStackForward.Pop();
  Result := FCurrent;
end;

procedure TBackManager.Clear();
begin
  FStackBackward.Clear();
  FStackForward.Clear();
  FCurrent := nil;
end;

function TBackManager.CanBackward(): Boolean;
begin
  Result := (FStackBackward.Count > 0);
end;

function TBackManager.CanForward(): Boolean;
begin
  Result := (FStackForward.Count > 0);
end;

procedure TBackManager.SetCurrent(const Value: TObject);
begin
  if Assigned(FCurrent) then FStackBackward.Push(FCurrent);
  FCurrent := Value;
  FStackForward.Clear();
end;

procedure TBackManager.BeginNav();
begin
  FNavBusy := True;
end;

procedure TBackManager.EndNav();
begin
  FNavBusy := False;
end;

{ TCustomCommand }

constructor TCustomCommand.Create(aManager: TUndoManager);
begin
  inherited Create;
  FManager := aManager;
end;

{ TUndoManager }

constructor TUndoManager.Create(aTree: TGEDCOMTree; aType: TUndoManType);
begin
  inherited Create;

  FDepth := 1000;
  FTree := aTree;

  FType := aType;
  //if (FType = autoCommit) then Application.OnIdle := OnIdle;alert

  FStackUndo := TStack.Create;
  FStackRedo := TStack.Create;
end;

destructor TUndoManager.Destroy;
begin
  FStackUndo.Destroy;
  FStackRedo.Destroy;

  inherited Destroy;
end;

procedure TUndoManager.OnIdle(Sender: TObject; var Done: Boolean);
begin
  Commit();
end;

function TUndoManager.CmdDo(cmd: TCustomCommand): Boolean;
begin
  if not(cmd.Redo()) then begin
    Rollback();
    Result := False;
    Exit;
  end;
  FStackUndo.Push(cmd);
  FStackRedo.Clear();
  Result := True;
end;

procedure TUndoManager.CmdRedo();
var
  cmd: TCustomCommand;
begin
  if (FStackRedo.Count = 0) then Exit;

  //Если транзакция не завершена, завершаем её
  if (FStackUndo.Peek() <> nil) then FStackUndo.Push(nil);

  while (FStackRedo.Peek() <> nil) do begin
    cmd := TCustomCommand(FStackRedo.Pop());
    FStackUndo.Push(cmd);
    if not(cmd.Redo()) then begin
      Rollback();
      Exit;
    end;
  end;
  FStackRedo.Pop();
  FStackUndo.Push(nil);
  Transaction(taCommitRedo);
end;

procedure TUndoManager.CmdUndo();
var
  cmd: TCustomCommand;
begin
  if (FStackUndo.Count < 2) then Exit;

  // Если транзакция завершена, снимаем контр. точку
  if (FStackUndo.Peek() = nil) then FStackUndo.Pop();

  FStackRedo.Push(nil);
  while (FStackUndo.Peek() <> nil) do begin
    cmd := TCustomCommand(FStackUndo.Pop());
    FStackRedo.Push(cmd);
    cmd.Undo();
  end;
  Transaction(taCommitUndo);
end;

function TUndoManager.CanRedo(): Boolean;
begin
  Result := (FStackRedo.Count - 1 > 0);
end;

function TUndoManager.CanUndo(): Boolean;
begin
  Result := (FStackUndo.Count - 1 > 0);
end;

procedure TUndoManager.Commit();
var
  cmd: TCustomCommand;
begin
  cmd := TCustomCommand(FStackUndo.Peek());
  if (cmd <> nil) then begin
    FStackUndo.Push(nil);
    Transaction(taCommit);
  end;
end;

procedure TUndoManager.Rollback();
var
  cmd: TCustomCommand;
begin
  while (FStackUndo.Peek() <> nil) do begin
    cmd := TCustomCommand(FStackUndo.Pop());
    cmd.Undo();
  end;
  Transaction(taRollback);
end;

procedure TUndoManager.Transaction(Arg: TTransactionEventArg);
begin
  if Assigned(FOnTransaction) then FOnTransaction(Self, Arg);
end;

procedure TUndoManager.Clear();
begin
  FStackUndo.Clear();
  FStackUndo.Push(nil); // Initial Check Point
  FStackRedo.Clear();
end;

{==============================================================================}

{ TNamesTable }

constructor TNamesTable.Create;
begin
  inherited Create;
  FNames := TObjectList.Create(True);
end;

destructor TNamesTable.Destroy;
begin
  FNames.Free;
  inherited Destroy;
end;

function TNamesTable.GetName(Index: Integer): TName;
begin
  if (Index >= 0) and (Index < FNames.Count)
  then Result := TName(FNames[Index])
  else Result := nil;
end;

function TNamesTable.GetNameCount(): Integer;
begin
  Result := FNames.Count;
end;

function TNamesTable.FindName(aName: string): TName;
var
  i: Integer;
  n: TName;
begin
  Result := nil;

  for i := 0 to FNames.Count - 1 do begin
    n := TName(FNames[i]);
    if (n.Name = aName) then begin
      Result := n;
      Break;
    end;
  end;
end;

function TNamesTable.GetPatronymicByName(aName: string; aSex: TGEDCOMObject.TGEDCOMSex): string;
var
  n: TName;
begin
  Result := '';

  n := FindName(aName);
  if (n = nil) then Exit;

  case aSex of
    svMale: Result := n.M_Patronymic;
    svFemale: Result := n.F_Patronymic;
  end;
end;

function TNamesTable.GetSexByName(aName: string): TGEDCOMObject.TGEDCOMSex;
var
  n: TName;
begin
  n := FindName(aName);
  if (n = nil)
  then Result := svNone
  else Result := n.Sex;
end;

function TNamesTable.GetNameByPatronymic(aPatronymic: string; aSex: TGEDCOMObject.TGEDCOMSex): string;
var
  i: Integer;
  n: TName;
begin
  Result := '';
  if (aPatronymic = '') then Exit;  

  for i := 0 to FNames.Count - 1 do begin
    n := TName(FNames[i]);
    if (n.F_Patronymic = aPatronymic) or (n.M_Patronymic = aPatronymic) then begin
      Result := n.Name;
      Break;
    end;
  end;
end;

function TNamesTable.AddName(aName: string): TName;
begin
  Result := TName.Create;
  Result.Name := aName;
  FNames.Add(Result);
end;

procedure TNamesTable.SetName(aName, aPatronymic: string; aSex: TGEDCOMObject.TGEDCOMSex);
var
  n: TName;
begin
  if (aName = '') then Exit;

  n := FindName(aName);
  if (n = nil) then n := AddName(aName);

  case aSex of
    svMale: if (n.M_Patronymic = '') then n.M_Patronymic := aPatronymic;
    svFemale: if (n.F_Patronymic = '') then n.F_Patronymic := aPatronymic;
  end;
end;

procedure TNamesTable.SetNameSex(aName: string; aSex: TGEDCOMObject.TGEDCOMSex);
var
  n: TName;
begin
  if (aName = '') then Exit;  

  n := FindName(aName);
  if (n = nil) then begin
    n := TName.Create;
    n.Name := aName;
    FNames.Add(n);
  end;

  if (n.Sex = svNone) and (aSex in [svMale, svFemale])
  then n.Sex := aSex;
end;

function TNamesTable.Comparable(aName, aPatronymic: string): Boolean;
var
  i, len, cmp: Integer;
begin
  cmp := 0;
  len := System.Math.Min(Length(aName), Length(aPatronymic));
  for i := 1 to len do
    if (aName[i] = aPatronymic[i])
    then Inc(cmp)
    else Break;

  Result := (cmp >= Round(len * 3 / 4));
end;

procedure TNamesTable.ImportNames(aTree: TGEDCOMTree);
var
  i: Integer;
  iRec, iFather: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
  dummy, ch_name, ch_pat, fat_nam: string;
begin
  try
    for i := 0 to aTree.RecordsCount - 1 do
      if (aTree.Records[i] is TGEDCOMIndividualRecord) then begin
        iRec := aTree.Records[i] as TGEDCOMIndividualRecord;

        TGenEngine.GetNameParts(iRec, dummy, ch_name, ch_pat);

        SetNameSex(ch_name, iRec.Sex);

        if (iRec.ChildToFamilyLinksCount <> 0) then begin
          family := iRec.ChildToFamilyLinks[0].Family;
          if (family <> nil) then begin
            iFather := TGEDCOMIndividualRecord(family.Husband.Value);
            if (iFather <> nil) then begin
              TGenEngine.GetNameParts(iFather, dummy, fat_nam, dummy);

              if (Length(ch_pat) > 1) and (Length(fat_nam) > 1) and Comparable(fat_nam, ch_pat)
              then SetName(fat_nam, ch_pat, iRec.Sex);
            end;
          end;
        end;
      end;
  except
    on E: Exception do TGKUtils.LogWrite('TNamesTable.ImportNames(): ' + E.Message);
  end;
end;

procedure TNamesTable.LoadFromFile(const aFileName: string);
var
  strd: System.IO.StreamReader;
  st: string;
  nm: TName;
  data: array of string;
begin
  if System.IO.File.Exists(aFileName) then begin
    strd := StreamReader.Create(aFileName, Encoding.GetEncoding(1251));
    try
      while not(strd.Peek = -1) do begin
        st := strd.ReadLine().Trim();
        data := st.Split([';']);

        nm := TName.Create;
        nm.Name := data[0];
        nm.F_Patronymic := data[1];
        nm.M_Patronymic := data[2];
        if (data[3] <> '')
        then nm.Sex := TGenEngine.GetSexBySign(data[3][1]);
        FNames.Add(nm);
      end;
    finally
      strd.Close;
    end;
  end;
end;

procedure TNamesTable.SaveToFile(const aFileName: string);
var
  strd: System.IO.StreamWriter;
  i: Integer;
  nm: TName;
  st: string;
begin
  strd := StreamWriter.Create(aFileName, False, Encoding.GetEncoding(1251));
  try
    for i := 0 to FNames.Count - 1 do begin
      nm := GetName(i);
      st := nm.Name + ';' + nm.F_Patronymic + ';' + nm.M_Patronymic + ';' + TGenEngine.SexData[nm.Sex].Sign;
      strd.WriteLine(st);
    end;
  finally
    strd.Close;
  end;
end;

{ TChartOptions }

constructor TChartOptions.Create;
begin
  inherited Create;

  FChildlessExclude := False;

  FDecorative := True;
  FFamilyVisible := True;
  FNameVisible := True;
  FPatronymicVisible := True;
  FDiffLines := False;
  FBirthDateVisible := False;
  FDeathDateVisible := False;
  FOnlyYears := False;
  FKinship := False;
  FPortraitsVisible := True;
  FSignsVisible := False;

  FMaleColor := System.Drawing.Color.FromArgb($ffC6C6FF);
  FFemaleColor := System.Drawing.Color.FromArgb($ffFFC6C6);
  FUnkSexColor := System.Drawing.Color.FromArgb($ffFFC6FF);
  FUnHusbandColor := System.Drawing.Color.FromArgb($ffD7D7FF);
  FUnWifeColor := System.Drawing.Color.FromArgb($ffFFD7D7);

  FDefFont_Name := 'Verdana';
  FDefFont_Size := 8;
  FDefFont_Color := System.Drawing.Color.Black;
  FDefFont_Style := System.Drawing.FontStyle.Regular;
end;

destructor TChartOptions.Destroy;
begin
  inherited Destroy;
end;

procedure TChartOptions.LoadFromFile(const aIniFile: TIniFile);
begin
  FChildlessExclude := aIniFile.ReadBool('Chart', 'ChildlessExclude', False);
  FDecorative := aIniFile.ReadBool('Chart', 'Decorative', True);
  FFamilyVisible := aIniFile.ReadBool('Chart', 'FamilyVisible', True);
  FNameVisible := aIniFile.ReadBool('Chart', 'NameVisible', True);
  FPatronymicVisible := aIniFile.ReadBool('Chart', 'PatronymicVisible', True);
  FDiffLines := aIniFile.ReadBool('Chart', 'DiffLines', False);
  FBirthDateVisible := aIniFile.ReadBool('Chart', 'BirthDateVisible', False);
  FDeathDateVisible := aIniFile.ReadBool('Chart', 'DeathDateVisible', False);
  FOnlyYears := aIniFile.ReadBool('Chart', 'OnlyYears', False);
  FKinship := aIniFile.ReadBool('Chart', 'Kinship', False);
  FSignsVisible := aIniFile.ReadBool('Chart', 'SignsVisible', False);
  FPortraitsVisible := aIniFile.ReadBool('Chart', 'PortraitsVisible', True);

  FMaleColor := System.Drawing.Color.FromArgb(aIniFile.ReadInteger('Chart', 'MaleColor', $ffC6C6FF));
  FFemaleColor := System.Drawing.Color.FromArgb(aIniFile.ReadInteger('Chart', 'FemaleColor', $ffFFC6C6));
  FUnkSexColor := System.Drawing.Color.FromArgb(aIniFile.ReadInteger('Chart', 'UnkSexColor', $ffFFC6FF));
  FUnHusbandColor := System.Drawing.Color.FromArgb(aIniFile.ReadInteger('Chart', 'UnHusbandColor', $ffD7D7FF));
  FUnWifeColor := System.Drawing.Color.FromArgb(aIniFile.ReadInteger('Chart', 'UnWifeColor', $ffFFD7D7));

  FDefFont_Name := aIniFile.ReadString('Chart', 'FontName', 'Verdana');
  FDefFont_Size := aIniFile.ReadInteger('Chart', 'FontSize', 8);
  FDefFont_Color := System.Drawing.Color.FromArgb(
    aIniFile.ReadInteger('Chart', 'FontColor', System.Drawing.Color.Black.ToArgb));
  FDefFont_Style := FontStyle(Byte(aIniFile.ReadInteger('Chart', 'FontStyle', 0)));
end;

procedure TChartOptions.SaveToFile(const aIniFile: TIniFile);
begin
  aIniFile.WriteBool('Chart', 'ChildlessExclude', FChildlessExclude);
  aIniFile.WriteBool('Chart', 'Decorative', FDecorative);
  aIniFile.WriteBool('Chart', 'FamilyVisible', FFamilyVisible);
  aIniFile.WriteBool('Chart', 'NameVisible', FNameVisible);
  aIniFile.WriteBool('Chart', 'PatronymicVisible', FPatronymicVisible);
  aIniFile.WriteBool('Chart', 'DiffLines', FDiffLines);
  aIniFile.WriteBool('Chart', 'BirthDateVisible', FBirthDateVisible);
  aIniFile.WriteBool('Chart', 'DeathDateVisible', FDeathDateVisible);
  aIniFile.WriteBool('Chart', 'OnlyYears', FOnlyYears);
  aIniFile.WriteBool('Chart', 'Kinship', FKinship);
  aIniFile.WriteBool('Chart', 'SignsVisible', FSignsVisible);
  aIniFile.WriteBool('Chart', 'PortraitsVisible', FPortraitsVisible);

  aIniFile.WriteInteger('Chart', 'MaleColor', FMaleColor.ToArgb());
  aIniFile.WriteInteger('Chart', 'FemaleColor', FFemaleColor.ToArgb());
  aIniFile.WriteInteger('Chart', 'UnkSexColor', FUnkSexColor.ToArgb());
  aIniFile.WriteInteger('Chart', 'UnHusbandColor', FUnHusbandColor.ToArgb());
  aIniFile.WriteInteger('Chart', 'UnWifeColor', FUnWifeColor.ToArgb());

  aIniFile.WriteString('Chart', 'FontName', FDefFont_Name);
  aIniFile.WriteInteger('Chart', 'FontSize', FDefFont_Size);
  aIniFile.WriteInteger('Chart', 'FontColor', FDefFont_Color.ToArgb);
  aIniFile.WriteInteger('Chart', 'FontStyle', Byte(FDefFont_Style));
end;

{ TProxy }

constructor TProxy.Create;
begin
  inherited Create;

  TGKUtils.BuildCRCTable;
end;

procedure TProxy.LoadFromFile(const aIniFile: TIniFile);
begin
  FUseProxy := aIniFile.ReadBool('Proxy', 'UseProxy', False);
  FServer := aIniFile.ReadString('Proxy', 'Server', '');
  FPort := aIniFile.ReadString('Proxy', 'Port', '');
  FLogin := aIniFile.ReadString('Proxy', 'Login', '');
  FPassword := TGKUtils.scDecrypt(aIniFile.ReadString('Proxy', 'Password', ''), TGKUtils.CrcStr(TGenEngine.AppName));
end;

procedure TProxy.SaveToFile(const aIniFile: TIniFile);
begin
  aIniFile.WriteBool('Proxy', 'UseProxy', FUseProxy);
  aIniFile.WriteString('Proxy', 'Server', FServer);
  aIniFile.WriteString('Proxy', 'Port', FPort);
  aIniFile.WriteString('Proxy', 'Login', FLogin);
  aIniFile.WriteString('Proxy', 'Password', TGKUtils.scEncrypt(FPassword, TGKUtils.CrcStr(TGenEngine.AppName)));
end;

{ TPedigreeOptions }

constructor TPedigreeOptions.Create;
begin
  inherited Create;
  FIncludeAttributes := True;
  FIncludeNotes := True;
  FIncludeSources := True;
end;

procedure TPedigreeOptions.LoadFromFile(const aIniFile: TIniFile);
begin
  FIncludeAttributes := aIniFile.ReadBool('Pedigree', 'IncludeAttributes', True);
  FIncludeNotes := aIniFile.ReadBool('Pedigree', 'IncludeNotes', True);
  FIncludeSources := aIniFile.ReadBool('Pedigree', 'IncludeSources', True);

  FFormat := TPedigreeFormat(aIniFile.ReadInteger('Pedigree', 'Format', 0));
end;

procedure TPedigreeOptions.SaveToFile(const aIniFile: TIniFile);
begin
  aIniFile.WriteBool('Pedigree', 'IncludeAttributes', FIncludeAttributes);
  aIniFile.WriteBool('Pedigree', 'IncludeNotes', FIncludeNotes);
  aIniFile.WriteBool('Pedigree', 'IncludeSources', FIncludeSources);

  aIniFile.WriteInteger('Pedigree', 'Format', Ord(FFormat));
end;

{ TGlobalOptions }

constructor TGlobalOptions.Create;
begin
  inherited Create;
  FChartOptions := TChartOptions.Create;
  FEventFilters := TStringList.Create;
  FMRUFiles := TStringList.Create;
  FNameFilters := TStringList.Create;
  FResidenceFilters := TStringList.Create;
  FPedigreeOptions := TPedigreeOptions.Create;
  FProxy := TProxy.Create;
  FRelations := TStringList.Create;

  FListPersonsColumns := DefPersonColumns;

  FLanguages := TObjectList.Create(True);
  FLastBases := TObjectList.Create(True);
end;

destructor TGlobalOptions.Destroy;
begin
  FLastBases.Free;
  FLanguages.Free;

  FRelations.Free;
  FProxy.Free;
  FPedigreeOptions.Free;
  FResidenceFilters.Free;
  FNameFilters.Free;
  FMRUFiles.Free;
  FEventFilters.Free;
  FChartOptions.Destroy;

  inherited Destroy;
end;

function TGlobalOptions.GetKeyLayout(): Word;
begin
  Result := LOWORD(GetKeyboardLayout(0));
end;

procedure TGlobalOptions.SetKeyLayout(aLayout: Word);
begin
  ActivateKeyboardLayout(aLayout, 0);
end;

procedure TGlobalOptions.LoadFromFile(const FileName: string);
var
  ini: TIniFile;
  i, cnt: Integer;
  fn, st: string;
  kl: Word;
  lb: TBaseWin;
begin
  ini := TIniFile.Create(FileName);
  try
    FDefCharacterSet := TGEDCOMObject.TGEDCOMCharacterSet(ini.ReadInteger('Common', 'DefCharacterSet', Ord(csUTF8)));
    FDefNameFormat := TGenEngine.TNameFormat(ini.ReadInteger('Common', 'DefNameFormat', Ord(nfFNP)));
    FDefDateFormat := TGenEngine.TDateFormat(ini.ReadInteger('Common', 'DefDateFormat', Ord(dfDD_MM_YYYY)));
    FLastDir := ini.ReadString('Common', 'LastDir', '');
    FPlacesWithAddress := ini.ReadBool('Common', 'PlacesWithAddress', False);
    FShowTips := ini.ReadBool('Common', 'ShowTips', True);
    FWorkMode := TWorkMode(ini.ReadInteger('Common', 'WorkMode', Ord(wmSimple)));
    FInterfaceLang := ini.ReadInteger('Common', 'InterfaceLang', LSDefCode);

    kl := ini.ReadInteger('Common', 'KeyLayout', GetKeyLayout());
    SetKeyLayout(kl);

    FChartOptions.LoadFromFile(ini);
    FPedigreeOptions.LoadFromFile(ini);
    FProxy.LoadFromFile(ini);

    cnt := ini.ReadInteger('NameFilters', 'Count', 0);
    for i := 0 to cnt - 1 do
      FNameFilters.Add(ini.ReadString('NameFilters', 'Filter_' + i.ToString(), ''));

    cnt := ini.ReadInteger('ResidenceFilters', 'Count', 0);
    for i := 0 to cnt - 1 do
      FResidenceFilters.Add(ini.ReadString('ResidenceFilters', 'Filter_' + i.ToString(), ''));

    cnt := ini.ReadInteger('EventFilters', 'Count', 0);
    for i := 0 to cnt - 1 do
      FEventFilters.Add(ini.ReadString('EventFilters', 'EventVal_' + i.ToString(), ''));

    cnt := ini.ReadInteger('MRUFiles', 'Count', 0);
    for i := 0 to cnt - 1 do begin
      fn := ini.ReadString('MRUFiles', 'File_' + i.ToString(), '');
      if System.IO.File.Exists(fn)
      then FMRUFiles.Add(fn)
      else ini.DeleteKey('MRUFiles', 'File_' + i.ToString());
    end;

    cnt := ini.ReadInteger('Relations', 'Count', 0);
    for i := 0 to cnt - 1 do
      FRelations.Add(ini.ReadString('Relations', 'Relation_' + i.ToString(), ''));

    ///

    for i := 0 to High(FListPersonsColumns) do begin
      FListPersonsColumns[i].colType := TPersonColumnType(ini.ReadInteger('PersonsColumns', 'ColType_' + i.ToString(), Ord(DefPersonColumns[i].colType)));
      FListPersonsColumns[i].colActive := ini.ReadBool('PersonsColumns', 'ColActive_' + i.ToString(), DefPersonColumns[i].colActive);
    end;

    FListPersons_HighlightUnmarried := ini.ReadBool('ListPersons', 'HighlightUnmarried', False);
    FListPersons_HighlightUnparented := ini.ReadBool('ListPersons', 'HighlightUnparented', False);

    FMWinRect.Left := ini.ReadInteger('Common', 'MWinL', -1);
    FMWinRect.Top := ini.ReadInteger('Common', 'MWinT', -1);
    FMWinRect.Right := ini.ReadInteger('Common', 'MWinW', -1);
    FMWinRect.Bottom := ini.ReadInteger('Common', 'MWinH', -1);
    FMWinState := FormWindowState(ini.ReadInteger('Common', 'MWinState', Ord(FormWindowState.Normal)));

    cnt := ini.ReadInteger('LastBases', 'Count', 0);
    for i := 0 to cnt - 1 do begin
      lb := AddLastBase();
      st := 'B' + i.ToString() + '_';

      lb.FileName := ini.ReadString('LastBases', st+'FileName', '');
      lb.WinRect.Left := ini.ReadInteger('LastBases', st+'WinL', 10);
      lb.WinRect.Top := ini.ReadInteger('LastBases', st+'WinT', 10);
      lb.WinRect.Right := ini.ReadInteger('LastBases', st+'WinW', 778);
      lb.WinRect.Bottom := ini.ReadInteger('LastBases', st+'WinH', 312);
      lb.WinState := FormWindowState(ini.ReadInteger('LastBases', st+'WinState', Ord(FormWindowState.Normal)));
    end;
  finally
    ini.Destroy;
  end;
end;

procedure TGlobalOptions.SaveToFile(const FileName: string);
var
  ini: TIniFile;
  i: Integer;
  lb: TBaseWin;
  st: string;
begin
  ini := TIniFile.Create(FileName);
  try
    ini.WriteInteger('Common', 'DefCharacterSet', Ord(FDefCharacterSet));
    ini.WriteInteger('Common', 'DefNameFormat', Ord(FDefNameFormat));
    ini.WriteInteger('Common', 'DefDateFormat', Ord(FDefDateFormat));
    ini.WriteString('Common', 'LastDir', FLastDir);
    ini.WriteBool('Common', 'PlacesWithAddress', FPlacesWithAddress);
    ini.WriteBool('Common', 'ShowTips', FShowTips);
    ini.WriteInteger('Common', 'WorkMode', Ord(FWorkMode));
    ini.WriteInteger('Common', 'InterfaceLang', FInterfaceLang);

    ini.WriteInteger('Common', 'KeyLayout', GetKeyLayout());

    FChartOptions.SaveToFile(ini);
    FPedigreeOptions.SaveToFile(ini);
    FProxy.SaveToFile(ini);

    ini.WriteInteger('NameFilters', 'Count', FNameFilters.Count);
    for i := 0 to FNameFilters.Count - 1 do
      ini.WriteString('NameFilters', 'Filter_' + i.ToString(), FNameFilters[i]);

    ini.WriteInteger('ResidenceFilters', 'Count', FResidenceFilters.Count);
    for i := 0 to FResidenceFilters.Count - 1 do
      ini.WriteString('ResidenceFilters', 'Filter_' + i.ToString(), FResidenceFilters[i]);

    ini.WriteInteger('EventFilters', 'Count', FEventFilters.Count);
    for i := 0 to FEventFilters.Count - 1 do
      ini.WriteString('EventFilters', 'EventVal_' + i.ToString(), FEventFilters[i]);

    ini.WriteInteger('MRUFiles', 'Count', FMRUFiles.Count);
    for i := 0 to FMRUFiles.Count - 1 do
      ini.WriteString('MRUFiles', 'File_' + i.ToString(), FMRUFiles[i]);
    FMRUFiles.Sort();

    ini.WriteInteger('Relations', 'Count', FRelations.Count);
    for i := 0 to FRelations.Count - 1 do
      ini.WriteString('Relations', 'Relation_' + i.ToString(), FRelations[i]);

    ///

    for i := 0 to High(FListPersonsColumns) do begin
      ini.WriteInteger('PersonsColumns', 'ColType_' + i.ToString(), Ord(FListPersonsColumns[i].colType));
      ini.WriteBool('PersonsColumns', 'ColActive_' + i.ToString(), FListPersonsColumns[i].colActive);
    end;

    ini.WriteBool('ListPersons', 'HighlightUnmarried', FListPersons_HighlightUnmarried);
    ini.WriteBool('ListPersons', 'HighlightUnparented', FListPersons_HighlightUnparented);

    ini.WriteInteger('Common', 'MWinL', FMWinRect.Left);
    ini.WriteInteger('Common', 'MWinT', FMWinRect.Top);
    ini.WriteInteger('Common', 'MWinW', FMWinRect.Right);
    ini.WriteInteger('Common', 'MWinH', FMWinRect.Bottom);
    ini.WriteInteger('Common', 'MWinState', Ord(FMWinState));

    ini.WriteInteger('LastBases', 'Count', FLastBases.Count);
    for i := 0 to FLastBases.Count - 1 do begin
      lb := GetLastBase(i);
      st := 'B' + i.ToString() + '_';

      ini.WriteString('LastBases', st+'FileName', lb.FileName);
      ini.WriteInteger('LastBases', st+'WinL', lb.WinRect.Left);
      ini.WriteInteger('LastBases', st+'WinT', lb.WinRect.Top);
      ini.WriteInteger('LastBases', st+'WinW', lb.WinRect.Right);
      ini.WriteInteger('LastBases', st+'WinH', lb.WinRect.Bottom);
      ini.WriteInteger('LastBases', st+'WinState', Ord(lb.WinState));
    end;
  finally
    ini.Destroy;
  end;
end;

function TGlobalOptions.GetLang(Index: Integer): TLangRecord;
begin
  Result := TLangRecord(FLanguages[Index]);
end;

function TGlobalOptions.GetLangsCount(): Integer;
begin
  Result := FLanguages.Count;
end;

function TGlobalOptions.GetLastBase(Index: Integer): TBaseWin;
begin
  Result := TBaseWin(FLastBases[Index]);
end;

function TGlobalOptions.GetLastBasesCount: Integer;
begin
  Result := FLastBases.Count;
end;

function TGlobalOptions.AddLastBase(): TBaseWin;
begin
  Result := TBaseWin.Create;
  FLastBases.Add(Result);
end;

procedure TGlobalOptions.ClearLastBases();
begin
  FLastBases.Clear;
end;

procedure TGlobalOptions.LngPrepareProc(const FileName: string);
var
  st, lng_code, lng_name: string;
  lng_file: System.IO.StreamReader;
  lng_rec: TLangRecord;
begin
  lng_file := System.IO.StreamReader.Create(FileName, Encoding.UTF8);
  try
    st := lng_file.ReadLine();

    if (st[1] = ';') then begin
      try
        StrDelete(st, 1, 1);
        lng_code := TGKUtils.GetToken(st, ',', 1);
        lng_name := TGKUtils.GetToken(st, ',', 2);

        lng_rec := TLangRecord.Create;
        lng_rec.Code := Int32.Parse(lng_code);
        lng_rec.Name := lng_name;
        lng_rec.FileName := FileName;
        FLanguages.Add(lng_rec);
      except end;
    end;
  finally
    lng_file.Close;
  end;
end;

procedure TGlobalOptions.FindLanguages();
var
  path: string;
begin
  path := TGKUtils.GetAppPath() + 'langs\';
  TGKUtils.ScanDir(path, LngPrepareProc, False, faAnyFile, '*.lng');
end;

end.
