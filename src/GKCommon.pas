unit GKCommon;

{$I GEDKeeper.inc}

interface

uses
  Classes, Contnrs, Graphics, IniFiles, GedCom551, GKEngine;

type
  TGKStack = class(TStack)
  protected
    procedure Clear; 
  end;

  TBackManager = class(TObject)
  private
    FNavBusy: Boolean;
    FStackBackward, FStackForward: TGKStack;
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

  TTransactionEventArg = (taCommit, taCommitUndo, taCommitRedo, taRollback);

  TUndoManType = (autoCommit, manualCommit);

  TTransactionEvent = procedure(Sender: TObject; Arg: TTransactionEventArg) of object;

  TUndoManager = class(TObject)
  private
    FDepth: Integer;
    FOnTransaction: TTransactionEvent;
    FStackUndo: TGKStack;
    FStackRedo: TGKStack;
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
  TWorkMode = (wmSimple, wmExpert);

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

const
  PersonColumnsName: array [TPersonColumnType] of record
    Name: string;
    DefWidth: Integer;
    colOnlyMain: Boolean;
  end = (
    (Name: 'Патриарх';                  DefWidth:  25; colOnlyMain: False),
    (Name: 'ФИО';                       DefWidth:  25; colOnlyMain: False),
    (Name: 'Прозвище';                  DefWidth:  75; colOnlyMain: False),
    (Name: 'Пол';                       DefWidth:  45; colOnlyMain: False),
    (Name: 'Дата рождения';             DefWidth: 100; colOnlyMain: False),
    (Name: 'Дата смерти';               DefWidth: 100; colOnlyMain: False),
    (Name: 'Место рождения';            DefWidth: 100; colOnlyMain: False),
    (Name: 'Место смерти';              DefWidth: 100; colOnlyMain: False),
    (Name: 'Местожительство';           DefWidth: 100; colOnlyMain: False),

    (Name: 'Возраст';                   DefWidth: 100; colOnlyMain: True),
    (Name: 'Продолжительность жизни';   DefWidth: 100; colOnlyMain: True),
    (Name: 'Дней до ДР';                DefWidth: 100; colOnlyMain: True),
    (Name: 'Группа';                    DefWidth: 200; colOnlyMain: True),

    (Name: 'Вероисповедание';           DefWidth: 200; colOnlyMain: True),
    (Name: 'Национальность';            DefWidth: 200; colOnlyMain: True),
    (Name: 'Образование';               DefWidth: 200; colOnlyMain: True),
    (Name: 'Профессия';                 DefWidth: 200; colOnlyMain: True),
    (Name: 'Социальное положение';      DefWidth: 200; colOnlyMain: True),

    (Name: 'Военная служба';            DefWidth: 200; colOnlyMain: True),
    (Name: 'Призван в ВС';              DefWidth: 200; colOnlyMain: True),
    (Name: 'Уволен из ВС';              DefWidth: 200; colOnlyMain: True),
    (Name: 'Звание в ВС';               DefWidth: 200; colOnlyMain: True),

    (Name: 'Изменено';                  DefWidth: 150; colOnlyMain: True),
    (Name: 'Закладка';                  DefWidth:  25; colOnlyMain: True)
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

type
  TName = class
  private
  public
    Name: string;
    F_Patronymic: string;
    M_Patronymic: string;
    Sex: TGEDCOMSex;
  end;

  TNamesTable = class(TObject)
  private
    FNames: TObjectList;
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
    function GetPatronymicByName(aName: string; aSex: TGEDCOMSex): string;
    function GetNameByPatronymic(aPatronymic: string; aSex: TGEDCOMSex): string;

    function GetSexByName(aName: string): TGEDCOMSex;

    procedure SetName(aName, aPatronymic: string; aSex: TGEDCOMSex);
    procedure SetNameSex(aName: string; aSex: TGEDCOMSex);
  end;

  TChartOptions = class(TObject)
  private
    FChildlessExclude: Boolean;
    FFamilyVisible: Boolean;
    FNameVisible: Boolean;
    FPatronymicVisible: Boolean;
    FDiffLines: Boolean;
    FBirthDateVisible: Boolean;
    FDeathDateVisible: Boolean;
    FOnlyYears: Boolean;
    FKinship: Boolean;
    FSignsVisible: Boolean;

    FMaleColor: TColor;
    FFemaleColor: TColor;
    FUnkSexColor: TColor;
    FUnHusbandColor: TColor;
    FUnWifeColor: TColor;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const aIniFile: TIniFile);
    procedure SaveToFile(const aIniFile: TIniFile);

    property ChildlessExclude: Boolean read FChildlessExclude write FChildlessExclude;

    property FamilyVisible: Boolean read FFamilyVisible write FFamilyVisible;
    property NameVisible: Boolean read FNameVisible write FNameVisible;
    property PatronymicVisible: Boolean read FPatronymicVisible write FPatronymicVisible;
    property DiffLines: Boolean read FDiffLines write FDiffLines;
    property BirthDateVisible: Boolean read FBirthDateVisible write FBirthDateVisible;
    property DeathDateVisible: Boolean read FDeathDateVisible write FDeathDateVisible;
    property OnlyYears: Boolean read FOnlyYears write FOnlyYears;
    property Kinship: Boolean read FKinship write FKinship;
    property SignsVisible: Boolean read FSignsVisible write FSignsVisible;

    property MaleColor: TColor read FMaleColor write FMaleColor;
    property FemaleColor: TColor read FFemaleColor write FFemaleColor;
    property UnkSexColor: TColor read FUnkSexColor write FUnkSexColor;
    property UnHusbandColor: TColor read FUnHusbandColor write FUnHusbandColor;
    property UnWifeColor: TColor read FUnWifeColor write FUnWifeColor;
  end;

  TProxy = class(TObject)
  private
    FServer: string;
    FPort: string;
    FLogin: string;
    FPassword: string;
    FUseProxy: Boolean;
  public
    procedure LoadFromFile(const aIniFile: TIniFile);
    procedure SaveToFile(const aIniFile: TIniFile);

    property Server: string read FServer write FServer;
    property Port: string read FPort write FPort;
    property Login: string read FLogin write FLogin;
    property Password: string read FPassword write FPassword;
    property UseProxy: Boolean read FUseProxy write FUseProxy;
  end;

  TPedigreeFormat = (pfExcess, pfCompact);

  TPedigreeOptions = class(TObject)
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
  private
    FChartOptions: TChartOptions;
    FDefCharacterSet: TGEDCOMCharacterSet;
    FDefDateFormat: TDateFormat;
    FDefNameFormat: TNameFormat;
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
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    property ChartOptions: TChartOptions read FChartOptions;
    property DefCharacterSet: TGEDCOMCharacterSet read FDefCharacterSet write FDefCharacterSet;
    property DefDateFormat: TDateFormat read FDefDateFormat write FDefDateFormat;
    property DefNameFormat: TNameFormat read FDefNameFormat write FDefNameFormat;
    property LastDir: string read FLastDir write FLastDir;
    property MRUFiles: TStringList read FMRUFiles;
    property NameFilters: TStringList read FNameFilters;
    property PlacesWithAddress: Boolean read FPlacesWithAddress write FPlacesWithAddress;
    property Relations: TStringList read FRelations;
    property ResidenceFilters: TStringList read FResidenceFilters;
    property WorkMode: TWorkMode read FWorkMode write FWorkMode;

    property ListPersons_HighlightUnmarried: Boolean
      read FListPersons_HighlightUnmarried write FListPersons_HighlightUnmarried;
    property ListPersons_HighlightUnparented: Boolean
      read FListPersons_HighlightUnparented write FListPersons_HighlightUnparented;

    property ListPersonsColumns: TPersonColumnsList
      read FListPersonsColumns write FListPersonsColumns;

    property PedigreeOptions: TPedigreeOptions read FPedigreeOptions;
    property Proxy: TProxy read FProxy;

    property ShowTips: Boolean read FShowTips write FShowTips;
  end;

implementation

uses
  {$IFDEF DELPHI_NET}System.IO,{$ENDIF}
  Windows, SysUtils, Math, Forms, bsComUtils, bsMiscUtils, GKUtils;

{ TGKStack }

procedure TGKStack.Clear;
begin
  List.Clear;
end;

{ TBackManager }

constructor TBackManager.Create;
begin
  FStackBackward := TGKStack.Create;
  FStackForward := TGKStack.Create;
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
  FManager := aManager;
end;

{ TUndoManager }

constructor TUndoManager.Create(aTree: TGEDCOMTree; aType: TUndoManType);
begin
  FDepth := 1000;
  FTree := aTree;

  FType := aType;
  if (FType = autoCommit) then Application.OnIdle := OnIdle;

  FStackUndo := TGKStack.Create;
  FStackRedo := TGKStack.Create;
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

function TNamesTable.GetPatronymicByName(aName: string; aSex: TGEDCOMSex): string;
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

function TNamesTable.GetSexByName(aName: string): TGEDCOMSex;
var
  n: TName;
begin
  n := FindName(aName);
  if (n = nil)
  then Result := svNone
  else Result := n.Sex;
end;

function TNamesTable.GetNameByPatronymic(aPatronymic: string; aSex: TGEDCOMSex): string;
var
  i: Integer;
  n: TName;
begin
  Result := '';

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

procedure TNamesTable.SetName(aName, aPatronymic: string; aSex: TGEDCOMSex);
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

procedure TNamesTable.SetNameSex(aName: string; aSex: TGEDCOMSex);
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

procedure TNamesTable.ImportNames(aTree: TGEDCOMTree);

  function Comparable(aName, aPatronymic: string): Boolean;
  var
    i, len, cmp: Integer;
  begin
    cmp := 0;
    len := Min(Length(aName), Length(aPatronymic));
    for i := 1 to len do
      if (aName[i] = aPatronymic[i])
      then Inc(cmp)
      else Break;

    Result := (cmp >= Round(len * 3 / 4));
  end;

var
  i: Integer;
  iRec, iFather: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
  dummy, ch_name, ch_pat, fat_nam: string;
begin
  for i := 0 to aTree.RecordsCount - 1 do
    if (aTree.Records[i] is TGEDCOMIndividualRecord) then begin
      iRec := aTree.Records[i] as TGEDCOMIndividualRecord;

      GetNameParts(iRec, dummy, ch_name, ch_pat);

      SetNameSex(ch_name, iRec.Sex);

      if (iRec.ChildToFamilyLinksCount <> 0) then begin
        family := iRec.ChildToFamilyLinks[0].Family;
        if (family <> nil) then begin
          iFather := TGEDCOMIndividualRecord(family.Husband.Value);
          if (iFather <> nil) then begin
            GetNameParts(iFather, dummy, fat_nam, dummy);

            if (Length(ch_pat) > 1) and (Length(fat_nam) > 1) and Comparable(fat_nam, ch_pat)
            then SetName(fat_nam, ch_pat, iRec.Sex);
          end;
        end;
      end;
    end;
end;

procedure TNamesTable.LoadFromFile(const aFileName: string);
var
  tf: TextFile;
  st, sex: string;
  nm: TName;
begin
  if FileExists(aFileName) then begin
    AssignFile(tf, aFileName); Reset(tf);
    while not Eof(tf) do begin
      Readln(tf, st);

      nm := TName.Create;
      nm.Name := GetToken(st, ';', 1);
      nm.F_Patronymic := GetToken(st, ';', 2);
      nm.M_Patronymic := GetToken(st, ';', 3);

      sex := GetToken(st, ';', 4);
      if (sex <> '') then nm.Sex := GetSexBySign(sex[1]);

      FNames.Add(nm);
    end;
    CloseFile(tf);
  end;
end;

procedure TNamesTable.SaveToFile(const aFileName: string);
var
  tf: TextFile;
  i: Integer;
  nm: TName;
begin
  AssignFile(tf, aFileName); Rewrite(tf);
  for i := 0 to FNames.Count - 1 do begin
    nm := GetName(i);
    Writeln(tf, nm.Name + ';' + nm.F_Patronymic + ';' + nm.M_Patronymic + ';' + SexData[nm.Sex].LatSign);
  end;
  CloseFile(tf);
end;

{ TChartOptions }

constructor TChartOptions.Create;
begin
  inherited Create;

  FChildlessExclude := False;

  FFamilyVisible := True;
  FNameVisible := True;
  FPatronymicVisible := True;
  FDiffLines := False;
  FBirthDateVisible := False;
  FDeathDateVisible := False;
  FOnlyYears := False;
  FKinship := False;
  FSignsVisible := False;

  FMaleColor := $00FFC6C6;
  FFemaleColor := $00C6C6FF;
  FUnkSexColor := $00FFC6FF;
  FUnHusbandColor := $00FFD7D7;
  FUnWifeColor := $00D7D7FF;
end;

destructor TChartOptions.Destroy;
begin
  inherited Destroy;
end;

procedure TChartOptions.LoadFromFile(const aIniFile: TIniFile);
begin
  FChildlessExclude := aIniFile.ReadBool('Chart', 'ChildlessExclude', False);
  FFamilyVisible := aIniFile.ReadBool('Chart', 'FamilyVisible', True);
  FNameVisible := aIniFile.ReadBool('Chart', 'NameVisible', True);
  FPatronymicVisible := aIniFile.ReadBool('Chart', 'PatronymicVisible', True);
  FDiffLines := aIniFile.ReadBool('Chart', 'DiffLines', False);
  FBirthDateVisible := aIniFile.ReadBool('Chart', 'BirthDateVisible', False);
  FDeathDateVisible := aIniFile.ReadBool('Chart', 'DeathDateVisible', False);
  FOnlyYears := aIniFile.ReadBool('Chart', 'OnlyYears', False);
  FKinship := aIniFile.ReadBool('Chart', 'Kinship', False);
  FSignsVisible := aIniFile.ReadBool('Chart', 'SignsVisible', False);

  FMaleColor := aIniFile.ReadInteger('Chart', 'MaleColor', $00FFC6C6);
  FFemaleColor := aIniFile.ReadInteger('Chart', 'FemaleColor', $00C6C6FF);
  FUnkSexColor := aIniFile.ReadInteger('Chart', 'UnkSexColor', $00FFC6FF);
  FUnHusbandColor := aIniFile.ReadInteger('Chart', 'UnHusbandColor', $00FFD7D7);
  FUnWifeColor := aIniFile.ReadInteger('Chart', 'UnWifeColor', $00D7D7FF);
end;

procedure TChartOptions.SaveToFile(const aIniFile: TIniFile);
begin
  aIniFile.WriteBool('Chart', 'ChildlessExclude', FChildlessExclude);
  aIniFile.WriteBool('Chart', 'FamilyVisible', FFamilyVisible);
  aIniFile.WriteBool('Chart', 'NameVisible', FNameVisible);
  aIniFile.WriteBool('Chart', 'PatronymicVisible', FPatronymicVisible);
  aIniFile.WriteBool('Chart', 'DiffLines', FDiffLines);
  aIniFile.WriteBool('Chart', 'BirthDateVisible', FBirthDateVisible);
  aIniFile.WriteBool('Chart', 'DeathDateVisible', FDeathDateVisible);
  aIniFile.WriteBool('Chart', 'OnlyYears', FOnlyYears);
  aIniFile.WriteBool('Chart', 'Kinship', FKinship);
  aIniFile.WriteBool('Chart', 'SignsVisible', FSignsVisible);

  aIniFile.WriteInteger('Chart', 'MaleColor', FMaleColor);
  aIniFile.WriteInteger('Chart', 'FemaleColor', FFemaleColor);
  aIniFile.WriteInteger('Chart', 'UnkSexColor', FUnkSexColor);
  aIniFile.WriteInteger('Chart', 'UnHusbandColor', FUnHusbandColor);
  aIniFile.WriteInteger('Chart', 'UnWifeColor', FUnWifeColor);
end;

{ TProxy }

procedure TProxy.LoadFromFile(const aIniFile: TIniFile);
begin
  FUseProxy := aIniFile.ReadBool('Proxy', 'UseProxy', False);
  FServer := aIniFile.ReadString('Proxy', 'Server', '');
  FPort := aIniFile.ReadString('Proxy', 'Port', '');
  FLogin := aIniFile.ReadString('Proxy', 'Login', '');
  FPassword := scDecrypt(aIniFile.ReadString('Proxy', 'Password', ''), CrcStr(AppName));
end;

procedure TProxy.SaveToFile(const aIniFile: TIniFile);
begin
  aIniFile.WriteBool('Proxy', 'UseProxy', FUseProxy);
  aIniFile.WriteString('Proxy', 'Server', FServer);
  aIniFile.WriteString('Proxy', 'Port', FPort);
  aIniFile.WriteString('Proxy', 'Login', FLogin);
  aIniFile.WriteString('Proxy', 'Password', scEncrypt(FPassword, CrcStr(AppName)));
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

function GetKeyLayout(): Word;
begin
  Result := LOWORD(GetKeyboardLayout(0));
end;

procedure SetKeyLayout(aLayout: Word);
begin
  ActivateKeyboardLayout(aLayout, 0);
end;

constructor TGlobalOptions.Create;
begin
  inherited Create;
  FChartOptions := TChartOptions.Create;
  FMRUFiles := TStringList.Create;
  FNameFilters := TStringList.Create;
  FResidenceFilters := TStringList.Create;
  FPedigreeOptions := TPedigreeOptions.Create;
  FProxy := TProxy.Create;
  FRelations := TStringList.Create;

  FListPersonsColumns := DefPersonColumns;
end;

destructor TGlobalOptions.Destroy;
begin
  FRelations.Free;
  FProxy.Free;
  FPedigreeOptions.Free;
  FResidenceFilters.Free;
  FNameFilters.Free;
  FMRUFiles.Free;
  FChartOptions.Destroy;

  inherited Destroy;
end;

procedure TGlobalOptions.LoadFromFile(const FileName: string);
var
  ini: TIniFile;
  i, cnt: Integer;
  fn: string;
  kl: Word;
begin
  ini := TIniFile.Create(FileName);
  try
    FDefCharacterSet := TGEDCOMCharacterSet(ini.ReadInteger('Common', 'DefCharacterSet', Ord(csUTF8)));
    FDefNameFormat := TNameFormat(ini.ReadInteger('Common', 'DefNameFormat', Ord(nfFNP)));
    FDefDateFormat := TDateFormat(ini.ReadInteger('Common', 'DefDateFormat', Ord(dfDD_MM_YYYY)));
    FLastDir := ini.ReadString('Common', 'LastDir', '');
    FPlacesWithAddress := ini.ReadBool('Common', 'PlacesWithAddress', False);
    FShowTips := ini.ReadBool('Common', 'ShowTips', True);
    FWorkMode := TWorkMode(ini.ReadInteger('Common', 'WorkMode', Ord(wmSimple)));

    kl := ini.ReadInteger('Common', 'KeyLayout', GetKeyLayout());
    SetKeyLayout(kl);

    FChartOptions.LoadFromFile(ini);
    FPedigreeOptions.LoadFromFile(ini);
    FProxy.LoadFromFile(ini);

    cnt := ini.ReadInteger('NameFilters', 'Count', 0);
    for i := 0 to cnt - 1 do
      FNameFilters.Add(ini.ReadString('NameFilters', 'Filter_' + IntToStr(i), ''));

    cnt := ini.ReadInteger('ResidenceFilters', 'Count', 0);
    for i := 0 to cnt - 1 do
      FResidenceFilters.Add(ini.ReadString('ResidenceFilters', 'Filter_' + IntToStr(i), ''));

    cnt := ini.ReadInteger('MRUFiles', 'Count', 0);
    for i := 0 to cnt - 1 do begin
      fn := ini.ReadString('MRUFiles', 'File_' + IntToStr(i), '');
      if FileExists(fn)
      then FMRUFiles.Add(fn)
      else ini.DeleteKey('MRUFiles', 'File_' + IntToStr(i));
    end;

    cnt := ini.ReadInteger('Relations', 'Count', 0);
    for i := 0 to cnt - 1 do
      FRelations.Add(ini.ReadString('Relations', 'Relation_' + IntToStr(i), ''));

    ///

    for i := 0 to High(FListPersonsColumns) do begin
      FListPersonsColumns[i].colType := TPersonColumnType(ini.ReadInteger('PersonsColumns', 'ColType_' + IntToStr(i), Ord(DefPersonColumns[i].colType)));
      FListPersonsColumns[i].colActive := ini.ReadBool('PersonsColumns', 'ColActive_' + IntToStr(i), DefPersonColumns[i].colActive);
    end;

    FListPersons_HighlightUnmarried := ini.ReadBool('ListPersons', 'HighlightUnmarried', False);
    FListPersons_HighlightUnparented := ini.ReadBool('ListPersons', 'HighlightUnparented', False);
  finally
    ini.Destroy;
  end;
end;

procedure TGlobalOptions.SaveToFile(const FileName: string);
var
  ini: TIniFile;
  i: Integer;
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

    ini.WriteInteger('Common', 'KeyLayout', GetKeyLayout());

    FChartOptions.SaveToFile(ini);
    FPedigreeOptions.SaveToFile(ini);
    FProxy.SaveToFile(ini);

    ini.WriteInteger('NameFilters', 'Count', FNameFilters.Count);
    for i := 0 to FNameFilters.Count - 1 do
      ini.WriteString('NameFilters', 'Filter_' + IntToStr(i), FNameFilters[i]);

    ini.WriteInteger('ResidenceFilters', 'Count', FResidenceFilters.Count);
    for i := 0 to FResidenceFilters.Count - 1 do
      ini.WriteString('ResidenceFilters', 'Filter_' + IntToStr(i), FResidenceFilters[i]);

    ini.WriteInteger('MRUFiles', 'Count', FMRUFiles.Count);
    for i := 0 to FMRUFiles.Count - 1 do
      ini.WriteString('MRUFiles', 'File_' + IntToStr(i), FMRUFiles[i]);
    FMRUFiles.Sort();

    ini.WriteInteger('Relations', 'Count', FRelations.Count);
    for i := 0 to FRelations.Count - 1 do
      ini.WriteString('Relations', 'Relation_' + IntToStr(i), FRelations[i]);

    ///

    for i := 0 to High(FListPersonsColumns) do begin
      ini.WriteInteger('PersonsColumns', 'ColType_' + IntToStr(i), Ord(FListPersonsColumns[i].colType));
      ini.WriteBool('PersonsColumns', 'ColActive_' + IntToStr(i), FListPersonsColumns[i].colActive);
    end;

    ini.WriteBool('ListPersons', 'HighlightUnmarried', FListPersons_HighlightUnmarried);
    ini.WriteBool('ListPersons', 'HighlightUnparented', FListPersons_HighlightUnparented);
  finally
    ini.Destroy;
  end;
end;

end.
