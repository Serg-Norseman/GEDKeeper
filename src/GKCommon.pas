unit GKCommon;

{$I GEDKeeper.inc}

interface

uses
  Classes, Contnrs, GedCom551, Graphics, Dialogs, GKCtrls;

resourcestring
  AppName = 'GEDKeeper';

const
  UnkFemale = 'неизвестная';
  UnkMale = 'неизвестный';

type
  TRecAction = (raAdd, raEdit, raDelete);
  TSelectMode = (smPerson, smNote, smMultimedia, smSource, smGroup);
  TTargetMode = (tmNone, tmAncestor, tmDescendant);
  TLifeMode = (lmAll, lmOnlyAlive, lmOnlyDead, lmAliveBefore);

  TName = class
  private
  public
    Name: string;
    F_Patronymic: string;
    M_Patronymic: string;
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

    function FindName(aName: string): TName;
    function GetPatronymicByName(aName: string; aSex: TGEDCOMSex): string;
    function GetNameByPatronymic(aPatronymic: string; aSex: TGEDCOMSex): string;
    procedure SetName(aName, aPatronymic: string; aSex: TGEDCOMSex);
  end;

  TChartOptions = class(TObject)
  private
    FFamilyVisible: Boolean;
    FNameVisible: Boolean;
    FPatronymicVisible: Boolean;
    FDiffLines: Boolean;
    FBirthDateVisible: Boolean;
    FDeathDateVisible: Boolean;
    FKinship: Boolean;

    FMaleColor: TColor;
    FFemaleColor: TColor;
    FUnkSexColor: TColor;
    FUnHusbandColor: TColor;
    FUnWifeColor: TColor;
  public
    constructor Create;
    destructor Destroy; override;

    property FamilyVisible: Boolean read FFamilyVisible write FFamilyVisible;
    property NameVisible: Boolean read FNameVisible write FNameVisible;
    property PatronymicVisible: Boolean read FPatronymicVisible write FPatronymicVisible;
    property DiffLines: Boolean read FDiffLines write FDiffLines;
    property BirthDateVisible: Boolean read FBirthDateVisible write FBirthDateVisible;
    property DeathDateVisible: Boolean read FDeathDateVisible write FDeathDateVisible;
    property Kinship: Boolean read FKinship write FKinship;

    property MaleColor: TColor read FMaleColor write FMaleColor;
    property FemaleColor: TColor read FFemaleColor write FFemaleColor;
    property UnkSexColor: TColor read FUnkSexColor write FUnkSexColor;
    property UnHusbandColor: TColor read FUnHusbandColor write FUnHusbandColor;
    property UnWifeColor: TColor read FUnWifeColor write FUnWifeColor;
  end;

  TNameFormat = (nfFNP, nfF_NP, nfF_N_P);

  TDateFormat = (dfDD_MM_YYYY, dfYYYY_MM_DD);

  TProxy = class(TObject)
  private
    FServer: string;
    FPort: string;
    FLogin: string;
    FPassword: string;
    FUseProxy: Boolean;
  public
    property Server: string read FServer write FServer;
    property Port: string read FPort write FPort;
    property Login: string read FLogin write FLogin;
    property Password: string read FPassword write FPassword;
    property UseProxy: Boolean read FUseProxy write FUseProxy;
  end;

  TPedigreeOptions = class(TObject)
  private
    FIncludeNotes: Boolean;
    FIncludeAttributes: Boolean;
    FIncludeSources: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property IncludeAttributes: Boolean read FIncludeAttributes write FIncludeAttributes;
    property IncludeNotes: Boolean read FIncludeNotes write FIncludeNotes;
    property IncludeSources: Boolean read FIncludeSources write FIncludeSources;
  end;

  TGlobalOptions = class(TObject)
  private
    FChartOptions: TChartOptions;
    FCleanEmptyFamilies: Boolean;
    FPedigreeOptions: TPedigreeOptions;
    FProxy: TProxy;
  public
    constructor Create;
    destructor Destroy; override;

    property ChartOptions: TChartOptions read FChartOptions;
    property CleanEmptyFamilies: Boolean read FCleanEmptyFamilies write FCleanEmptyFamilies;
    property PedigreeOptions: TPedigreeOptions read FPedigreeOptions;
    property Proxy: TProxy read FProxy;
  end;

type
  TDateControlsRange = set of 1..2;

const
  Sex: array [TGEDCOMSex] of string = ('?', 'мужской', 'женский', 'неопределенный');
  SexSigns: array [TGEDCOMSex] of string = ('?', 'М', 'Ж', 'Н');

  MarriageStatusSize = 4;
  MarriageStatus: array [0..MarriageStatusSize-1] of record
    Name: string;
    StatSign: string;
  end = (
    (Name: 'Неизвестно'; StatSign: ''),
    (Name: 'Брак зарегистрирован'; StatSign: 'MARRIED'),
    (Name: 'Брак не зарегистрирован'; StatSign: 'MARRNOTREG'),
    (Name: 'Разведены'; StatSign: 'NOTMARR')
  );

  PersonEventsSize = 15;
  PersonEvents: array [0..PersonEventsSize-1] of record
    Name: string;
    Sign: string;
  end = (
    (Name: 'Новое событие'; Sign: 'EVEN'), {std:check}
    (Name: 'Рождение'; Sign: 'BIRT'), {std:check}
    (Name: 'Усыновление'; Sign: 'ADOP'), {std:check}
    (Name: 'Крещение'; Sign: 'CHR'), {std:check}
    (Name: 'Получение ученой степени'; Sign: 'GRAD'),   {std:check}
    (Name: 'Уход на пенсию'; Sign: 'RETI'), {std:check}
    (Name: 'Натурализация'; Sign: 'NATU'), {std:check}
    (Name: 'Эмиграция'; Sign: 'EMIG'), {std:check}
    (Name: 'Иммиграция'; Sign: 'IMMI'), {std:check}
    (Name: 'Перепись'; Sign: 'CENS'), {std:check}
    (Name: 'Завещание'; Sign: 'WILL'), {std:check}
    (Name: 'Утверждение завещания'; Sign: 'PROB'), {std:check}
    (Name: 'Смерть'; Sign: 'DEAT'), {std:check}
    (Name: 'Похороны'; Sign: 'BURI'), {std:check}
    (Name: 'Кремация'; Sign: 'CREM') {std:check}
  );

  PersonAttributesSize = 14;
  PersonAttributes: array [0..PersonAttributesSize-1] of record
    Name: string;
    Sign: string;
  end = (
    (Name: 'Атрибут'; Sign: 'FACT'),                    {std:check, AT}
    (Name: 'Вероисповедание'; Sign: 'RELI'),            {std:check, AT}
    (Name: 'Национальность'; Sign: 'NATI'),             {std:check, AT}
    (Name: 'Местожительство'; Sign: 'RESI'),            {std:check, AT}
    (Name: 'Физическое описание'; Sign: 'DSCR'),        {std:check, AT}
    (Name: 'Идентификационный номер'; Sign: 'IDNO'),    {std:check, AT}
    (Name: 'Код социального страхования'; Sign: 'SSN'), {std:check, AT}
    (Name: 'Кол-во детей'; Sign: 'NCHI'),               {std:check, AT}
    (Name: 'Кол-во браков'; Sign: 'NMR'),               {std:check, AT}
    (Name: 'Образование'; Sign: 'EDUC'),                {std:check, AT}
    (Name: 'Профессия'; Sign: 'OCCU'),                  {std:check, AT}
    (Name: 'Социальное положение'; Sign: 'CAST'),       {std:check, AT}
    (Name: 'Собственность'; Sign: 'PROP'),              {std:check, AT}
    (Name: 'Титул'; Sign: 'TITL')                       {std:check, AT}
  );

  DateKindsSize = 10;
  DateKinds: array [0..DateKindsSize-1] of record
    Name: string;
    Dates: TDateControlsRange;
  end = (
    { 0} (Name: 'Точно';        Dates: [1]),{+}
    { 1} (Name: 'Ранее';        Dates: [2]),
    { 2} (Name: 'Позднее';      Dates: [1]),
    { 3} (Name: 'Между';        Dates: [1, 2]),
    { 4} (Name: 'Период до';    Dates: [1]),
    { 5} (Name: 'Период после'; Dates: [2]),
    { 6} (Name: 'Период между'; Dates: [1, 2]),
    { 7} (Name: 'Около';        Dates: [1]),{+}
    { 8} (Name: 'По расчету';   Dates: [1]),{+}
    { 9} (Name: 'По оценке';    Dates: [1]) {+}
  );

  FamilyEventsSize = 10;
  FamilyEvents: array [0..FamilyEventsSize-1] of record
    Name: string;
    Sign: string;
  end = (
    (Name: 'Новое событие'; Sign: ''), {std:check}
    (Name: 'Помолвка'; Sign: 'ENGA'),
    (Name: 'Бракосочетание'; Sign: 'MARR'),
    (Name: 'Публичное объявление о бракосочетании'; Sign: 'MARB'),
    (Name: 'Заключение брачного контракта'; Sign: 'MARC'),
    (Name: 'Получение разрешения на брак'; Sign: 'MARL'),
    (Name: 'Заключение брачного соглашения'; Sign: 'MARS'),
    (Name: 'Аннулирование брака'; Sign: 'ANUL'),
    (Name: 'Подача заявления о разводе'; Sign: 'DIVF'),
    (Name: 'Развод'; Sign: 'DIV')
  );

  NamePiecesSize = 6;
  NamePieces: array [0..NamePiecesSize-1] of record
    Name: string;
    Sign: string;
  end = (
    (Name: 'Префикс имени'; Sign: 'NPFX'),
    (Name: 'Псевдоним'; Sign: 'GIVN'),
    (Name: 'Прозвище'; Sign: 'NICK'),
    (Name: 'Префикс фамилии'; Sign: 'SPFX'),
    (Name: 'Фамилия'; Sign: 'SURN'),
    (Name: 'Суффикс имени'; Sign: 'NSFX')
  );
  // Духовное имя ?

function Hole(var A): Integer;

// Конвертировать путь (Linux/Windows)
function ConvertPath(aPath: string): string;

// Начать протоколирование
procedure LogInit(const aFileName: string);
// Записать в протокол
procedure LogWrite(const aMsg: string);
// Завершить протоколирование
procedure LogDone;

// Путь приложения
function GetAppPath(): string;

// Дополнить число нулями
function NumUpdate(val, up: Integer): string;

// Получить число римскими цифрами
function GetRome(N: Integer): string;

// извлечение токена
function GetToken(aString, SepChar: string; TokenNum: Byte): string;
// количество токенов
function GetTokensCount(aString, SepChar: string): Byte;

function GetPersonEventIndex(aSign: string): Integer;
function GetPersonAttributeIndex(aSign: string): Integer;
function GetFamilyEventIndex(aSign: string): Integer;
function GetNamePieceIndex(aSign: string): Integer;

function GetMarriageStatusIndex(aSign: string): Integer;
procedure GetNameParts(iRec: TGEDCOMIndividualRecord; var aFamily, aName, aPatronymic: string);

function GetNameStr(iRec: TGEDCOMIndividualRecord; aByFamily: Boolean = True;
  aPieces: Boolean = False): string;
function GetFamilyStr(aFamily: TGEDCOMFamilyRecord): string;

function GetId(aRecord: TGEDCOMRecord): Integer;

function GEDCOMDateToStr(aDate: TGEDCOMDate; aFormat: TDateFormat = dfDD_MM_YYYY): string;
function StrToGEDCOMDate(aDate: string): string;
function GEDCOMCustomDateToStr(aDate: TGEDCOMCustomDate; aFormat: TDateFormat;
  aSign: Boolean = False): string;
function GEDCOMDateToDate(aDate: TGEDCOMCustomDate): TDateTime;

function GetIndividualEvent(iRec: TGEDCOMIndividualRecord; evName: string): TGEDCOMIndividualEvent;
function GetFamilyEvent(fRec: TGEDCOMFamilyRecord; evName: string): TGEDCOMFamilyEvent;

function GetBirthDate(iRec: TGEDCOMIndividualRecord; aFormat: TDateFormat): string;
function GetDeathDate(iRec: TGEDCOMIndividualRecord; aFormat: TDateFormat): string;
function GetLifeStr(iRec: TGEDCOMIndividualRecord): string;

function GetBirthPlace(iRec: TGEDCOMIndividualRecord): string;
function GetDeathPlace(iRec: TGEDCOMIndividualRecord): string;

function GetAttributeValue(iRec: TGEDCOMIndividualRecord; attrName: string): string;
function GetAttributeStr(iAttr: TGEDCOMIndividualAttribute): string;

function GetMarriageDate(fRec: TGEDCOMFamilyRecord; aFormat: TDateFormat): string;
function GetEventDesc(evDetail: TGEDCOMEventDetail): string;

function IsLive(iRec: TGEDCOMIndividualRecord): Boolean;
function GetLifeExpectancy(iRec: TGEDCOMIndividualRecord): string;
function GetAge(iRec: TGEDCOMIndividualRecord): string;
function GetFirstbornAge(iRec: TGEDCOMIndividualRecord): string;
function GetMarriageAge(iRec: TGEDCOMIndividualRecord): string;
function GetDaysForBirth(iRec: TGEDCOMIndividualRecord): string;

function HyperLink(XRef: string; Text: string): string;

function IndistinctMatching(MaxMatching: Integer; strInputMatching, strInputStandart: String): Integer;
function IndistinctMatching2(A, B: String): Single;
function IndistinctMatching3(StrA, StrB: string): Integer;

function ClearFamily(aFamily: string): string;

function GetSelIndex(aList: TBSListView): Integer;

function PrepareRusFamily(f: string; aFemale: Boolean): string;

function GetChildsCount(aPerson: TGEDCOMIndividualRecord): Integer;
function GetMarriagesCount(aPerson: TGEDCOMIndividualRecord): Integer;
function GetSpousesDiff(fRec: TGEDCOMFamilyRecord): string;

type
  TCommonStats = record
    persons, persons_m, persons_f: Integer;
    lives, lives_m, lives_f: Integer;
    age, age_m, age_f, age_cnt, age_m_cnt, age_f_cnt: Integer;
    life, life_m, life_f, life_cnt, life_m_cnt, life_f_cnt: Integer;
    childs, childs_m, childs_f, childs_cnt, childs_m_cnt, childs_f_cnt: Integer;

    fba, fba_m, fba_f, fba_cnt, fba_m_cnt, fba_f_cnt: Integer;
    marr, marr_m, marr_f, marr_cnt, marr_m_cnt, marr_f_cnt: Integer;
    mage, mage_m, mage_f, mage_cnt, mage_m_cnt, mage_f_cnt: Integer;
  end;

procedure GetCommonStats(aTree: TGEDCOMTree; var aStats: TCommonStats);

procedure MergeFiles(aMainTree: TGEDCOMTree; aFileName: string);

function GetChangeDate(aRec: TGEDCOMRecordWithLists): string;

function CheckGEDCOMFormat(aTree: TGEDCOMTree): Boolean;

procedure LoadExtFile(const aFileName: string);

type
  TGEDCOMTagWithListsEx = class(TGEDCOMTagWithLists)
  public
    property Notes;
    property NotesCount;
    property SourceCitations;
    property SourceCitationsCount;
    property MultimediaLinks;
    property MultimediaLinksCount;
  end;

implementation

uses
  Windows, SysUtils, StrUtils, Math, DateUtils, ShellAPI, Controls, GKProgress;

function Hole(var A): Integer;
asm
end;

// Конвертировать путь (Linux/Windows)
function ConvertPath(aPath: string): string;
var
  i: Integer;
begin
  Result := aPath;
  {$IFDEF OS_LINUX}
  for i := 1 to Length(aPath) do
    if (aPath[i] = '\') then Result[i] := '/';
  {$ELSE}
  for i := 1 to Length(aPath) do
    if (aPath[i] = '/') then Result[i] := '\';
  {$ENDIF}
end;

var
  Log: {$IFNDEF FREEPASCAL}TextFile{$ELSE}Text{$ENDIF};
  LogRunned: Boolean = False;

// Начать протоколирование
procedure LogInit(const aFileName: string);
var
  ex: Boolean;
  fn: string;
begin
  fn := ConvertPath(aFileName);
  ex := FileExists(fn);

  AssignFile(Log, fn);
  if ex
  then Append(Log)
  else Rewrite(Log);

  LogRunned := True;

  Writeln(Log, DateTimeToStr(Now) + ' log begin');
end;

// Записать в протокол
procedure LogWrite(const aMsg: string);
begin
  if LogRunned
  then Writeln(Log, '  -> ' + aMsg);
end;

// Завершить протоколирование
procedure LogDone;
begin
  Writeln(Log, DateTimeToStr(Now) + ' log end');

  LogRunned := False;

  CloseFile(Log);
end;

// Путь приложения
function GetAppPath(): string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

// Дополнить число нулями
function NumUpdate(val, up: Integer): string;
begin
  Result := IntToStr(val);
  while Length(Result) < up do Result := '0' + Result;
end;

// Получить число римскими цифрами
function GetRome(N: Integer): string;
const
  Rn_N: array [1..13] of Integer = (
    1, 4, 5, 9, 10, 40, 50, 90, 100, 400, 500, 900, 1000);
  Rn_S: array [1..13] of string = (
    'I', 'IV', 'V', 'IX', 'X', 'XL', 'L', 'XC', 'C', 'CD', 'D', 'CM', 'M');
var
  S: string;
  T: Byte;
begin
  S := '';
  T := 13;
  while (N > 0) do begin
    while (N < Rn_N[T]) do Dec(T);
    while (N >= Rn_N[T]) do begin
      Dec(N, Rn_N[T]);
      S := S + Rn_S[T];
    end;
  end;
  Result := S;
end;

// извлечение токена
function GetToken(aString, SepChar: string; TokenNum: Byte): string;
var
  tok: string;
  i, tok_num: Integer;
begin
  Result := '';
  if (aString = '') then Exit;

  if (Pos(aString[Length(aString)], SepChar) <= 0)
  then aString := aString + SepChar[1];

  tok_num := 0;
  tok := '';
  for i := 1 to Length(aString) do begin
    if (Pos(aString[i], SepChar) > 0) then begin
      Inc(tok_num);

      if (tok_num = TokenNum) then begin
        Result := tok;
        Break;
      end else tok := '';
    end else begin
      tok := tok + aString[i];
    end;
  end;
end;

// количество токенов
function GetTokensCount(aString, SepChar: string): Byte;
var
  i: Integer;
begin
  Result := 0;
  if (aString = '') then Exit;

  if (Pos(aString[Length(aString)], SepChar) <= 0)
  then aString := aString + SepChar[1];

  for i := 1 to Length(aString) do
    if (Pos(aString[i], SepChar) > 0)
    then Inc(Result);
end;

function GetPersonEventIndex(aSign: string): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to PersonEventsSize - 1 do
    if (PersonEvents[i].Sign = aSign) then begin
      Result := i;
      Break;
    end;
end;

function GetPersonAttributeIndex(aSign: string): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to PersonAttributesSize - 1 do
    if (PersonAttributes[i].Sign = aSign) then begin
      Result := i;
      Break;
    end;
end;

function GetFamilyEventIndex(aSign: string): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to FamilyEventsSize - 1 do
    if (FamilyEvents[i].Sign = aSign) then begin
      Result := i;
      Break;
    end;
end;

function GetNamePieceIndex(aSign: string): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to NamePiecesSize - 1 do
    if (NamePieces[i].Sign = aSign) then begin
      Result := i;
      Break;
    end;
end;

function GetMarriageStatusIndex(aSign: string): Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to MarriageStatusSize - 1 do
    if (MarriageStatus[i].StatSign = aSign) then begin
      Result := i;
      Break;
    end;
end;

procedure GetNameParts(iRec: TGEDCOMIndividualRecord; var aFamily, aName, aPatronymic: string);
var
  np: TGEDCOMPersonalName;
begin
  np := iRec.PersonalNames[0];
  aFamily := np.Surname;
  if (GetTokensCount(np.FirstPart, ' ') > 1) then begin
    aName := GetToken(np.FirstPart, ' ', 1);
    aPatronymic := GetToken(np.FirstPart, ' ', 2);
  end else begin
    aName := np.FirstPart;
    aPatronymic := '';
  end;
end;

function GetNameStr(iRec: TGEDCOMIndividualRecord; aByFamily: Boolean = True;
  aPieces: Boolean = False): string;
(*
    (Name: 'Префикс имени'; Sign: 'NPFX'),
    (Name: 'Псевдоним'; Sign: 'GIVN'),
    (Name: 'Прозвище'; Sign: 'NICK'),
    (Name: 'Префикс фамилии'; Sign: 'SPFX'),
    (Name: 'Фамилия'; Sign: 'SURN'),
    (Name: 'Суффикс имени'; Sign: 'NSFX')
 *)
var
  np: TGEDCOMPersonalName;
  nick: string;
begin
  if (iRec <> nil) then begin
    np := iRec.PersonalNames[0];

    nick := np.TagStringValue('NICK');

    if (aByFamily)
    then Result := np.Surname + ' ' + np.FirstPart
    else Result := np.FirstPart + ' ' + np.Surname;

    if (aPieces) then begin
      if (nick <> '') then Result := Result + ' [' + nick + ']';
    end;
  end else Result := '';
end;

function GetFamilyStr(aFamily: TGEDCOMFamilyRecord): string;
var
  spouse: TGEDCOMIndividualRecord;
begin
  Result := '';

  spouse := TGEDCOMIndividualRecord(aFamily.Husband.Value);
  if (spouse = nil)
  then Result := Result + 'Неизвестный'
  else Result := Result + GetNameStr(spouse);

  Result := Result + ' - ';

  spouse := TGEDCOMIndividualRecord(aFamily.Wife.Value);
  if (spouse = nil)
  then Result := Result + 'Неизвестная'
  else Result := Result + GetNameStr(spouse);
end;

function GetId(aRecord: TGEDCOMRecord): Integer;
var
  xref, sign: string;
begin
  xref := aRecord.XRef;
  sign := GetSignByRecord(aRecord);
  Delete(xref, 1, Length(sign));

  try
    Result := StrToIntDef(xref, 0);
  except
    Result := 0;
  end;
end;

function GEDCOMDateToStr(aDate: TGEDCOMDate; aFormat: TDateFormat = dfDD_MM_YYYY): string;
var
  year: Integer;
  month, day: Word;
begin
  Result := '';
  aDate.GetDate(year, month, day);
  if (year <= 0) and (month <= 0) and (day <= 0) then Exit;

  case aFormat of
    dfDD_MM_YYYY: begin
      if (day > 0)
      then Result := Result + NumUpdate(day, 2) + '.'
      else Result := Result + '__.';

      if (month > 0)
      then Result := Result + NumUpdate(month, 2) + '.'
      else Result := Result + '__.';

      if (year > 0)
      then Result := Result + IntToStr(year)
      else Result := Result + '____';
    end;

    dfYYYY_MM_DD: begin
      if (year > 0)
      then Result := Result + IntToStr(year) + '.'
      else Result := Result + '____.';

      if (month > 0)
      then Result := Result + NumUpdate(month, 2) + '.'
      else Result := Result + '__.';

      if (day > 0)
      then Result := Result + NumUpdate(day, 2)
      else Result := Result + '__';
    end;
  end;
end;

function StrToGEDCOMDate(aDate: string): string;
var
  cnt: Integer;
  pd, pm, py: string;
begin
  Result := '';

  cnt := GetTokensCount(aDate, '.');
  if (cnt < 3) then raise Exception.Create('date failed');

  pd := Trim(GetToken(aDate, '.', 1));
  pm := Trim(GetToken(aDate, '.', 2));
  py := Trim(GetToken(aDate, '.', 3));

  if (pd <> '') then Result := Result + pd + ' ';
  if (pm <> '') then Result := Result + GEDCOMMonthArray[StrToInt(pm)] + ' ';
  if (py <> '') then Result := Result + py;
end;

function GEDCOMCustomDateToStr(aDate: TGEDCOMCustomDate; aFormat: TDateFormat;
  aSign: Boolean = False): string;
var
  dt_range: TGEDCOMDateRange;
  dt_period: TGEDCOMDatePeriod;
begin
  if (aDate is TGEDCOMDateApproximated) then begin
    Result := GEDCOMDateToStr(TGEDCOMDate(aDate), aFormat);

    if (aSign) and (TGEDCOMDateApproximated(aDate).Approximated <> daExact)
    then Result := '~ ' + Result;
  end
  else
  if (aDate is TGEDCOMDateRange) then begin
    dt_range := TGEDCOMDateRange(aDate);

    if (dt_range.After.StringValue = '') and (dt_range.Before.StringValue <> '')
    then begin
      Result := GEDCOMDateToStr(dt_range.Before, aFormat);
      if (aSign) then Result := '< ' + Result;
    end
    else
    if (dt_range.After.StringValue <> '') and (dt_range.Before.StringValue = '')
    then begin
      Result := GEDCOMDateToStr(dt_range.After, aFormat);
      if (aSign) then Result := Result + ' >';
    end
    else
    if (dt_range.After.StringValue <> '') and (dt_range.Before.StringValue <> '')
    then Result := GEDCOMDateToStr(dt_range.After, aFormat) + '-' + GEDCOMDateToStr(dt_range.Before, aFormat);
  end
  else
  if (aDate is TGEDCOMDatePeriod) then begin
    dt_period := TGEDCOMDatePeriod(aDate);

    if (dt_period.DateFrom.StringValue <> '') and (dt_period.DateTo.StringValue = '')
    then begin
      Result := GEDCOMDateToStr(dt_period.DateFrom, aFormat);
      if (aSign) then Result := Result + ' >';
    end
    else
    if (dt_period.DateFrom.StringValue = '') and (dt_period.DateTo.StringValue <> '')
    then begin
      Result := GEDCOMDateToStr(dt_period.DateTo, aFormat);
      if (aSign) then Result := '< ' + Result;
    end
    else
    if (dt_period.DateFrom.StringValue <> '') and (dt_period.DateTo.StringValue <> '')
    then Result := GEDCOMDateToStr(dt_period.DateFrom, aFormat) + '-' + GEDCOMDateToStr(dt_period.DateTo, aFormat);
  end
  else
  if (aDate is TGEDCOMDate) then begin
    Result := GEDCOMDateToStr(TGEDCOMDate(aDate), aFormat);
  end;
end;

function GEDCOMDateToDate(aDate: TGEDCOMCustomDate): TDateTime;
var
  dt: TGEDCOMDate;
  year: Integer;
  month, day: Word;
begin
  try
    Result := 0;

    if (aDate is TGEDCOMDateApproximated) or (aDate is TGEDCOMDate)
    then dt := TGEDCOMDate(aDate)
    else Exit;

    dt.GetDate(year, month, day);

    if (day = 0) then day := 1;

    if (month = 0) then month := 1;

    if (year <= 0)
    then Result := 0
    else Result := EncodeDate(year, month, day);
  except
    Hole(year);
    Hole(month);
    Hole(day);
  end;
end;

function GetIndividualEvent(iRec: TGEDCOMIndividualRecord; evName: string): TGEDCOMIndividualEvent;
var
  i: Integer;
  event: TGEDCOMIndividualEvent;
begin
  Result := nil;
  if (iRec = nil) then Exit;

  for i := 0 to iRec.IndividualEventsCount - 1 do begin
    event := iRec.IndividualEvents[i];

    if (event.Name = evName) then begin
      Result := event;
      Exit;
    end;
  end; // 'BIRT', 'DEAT'
end;

function GetFamilyEvent(fRec: TGEDCOMFamilyRecord; evName: string): TGEDCOMFamilyEvent;
var
  i: Integer;
  event: TGEDCOMFamilyEvent;
begin
  Result := nil;
  if (fRec = nil) then Exit;

  for i := 0 to fRec.FamilyEventCount - 1 do begin
    event := fRec.FamilyEvents[i];

    if (event.Name = evName) then begin
      Result := event;
      Exit;
    end;
  end; 
end;

function GetBirthDate(iRec: TGEDCOMIndividualRecord; aFormat: TDateFormat): string;
var
  event: TGEDCOMIndividualEvent;
begin
  event := GetIndividualEvent(iRec, 'BIRT');

  if (event = nil)
  then Result := ''
  else Result := GEDCOMCustomDateToStr(event.Detail.Date.Value, aFormat);
end;

function GetDeathDate(iRec: TGEDCOMIndividualRecord; aFormat: TDateFormat): string;
var
  event: TGEDCOMIndividualEvent;
begin
  event := GetIndividualEvent(iRec, 'DEAT');

  if (event = nil)
  then Result := ''
  else Result := GEDCOMCustomDateToStr(event.Detail.Date.Value, aFormat);
end;

function GetLifeStr(iRec: TGEDCOMIndividualRecord): string;
var
  ds: string;
  ev: TGEDCOMIndividualEvent;
begin
  Result := ' (';

  ds := GetBirthDate(iRec, dfDD_MM_YYYY);
  if (ds = '') then ds := '?';

  Result := Result + ds;

  ds := GetDeathDate(iRec, dfDD_MM_YYYY);
  if (ds = '') then begin
    ev := GetIndividualEvent(iRec, 'DEAT');
    if (ev <> nil) then ds := '?';
  end;

  if (ds <> '')
  then Result := Result + ' - ' + ds;

  Result := Result + ')';
end;

function GetBirthPlace(iRec: TGEDCOMIndividualRecord): string;
var
  event: TGEDCOMIndividualEvent;
begin
  event := GetIndividualEvent(iRec, 'BIRT');

  if (event = nil)
  then Result := ''
  else Result := event.Detail.Place;
end;

function GetDeathPlace(iRec: TGEDCOMIndividualRecord): string;
var
  event: TGEDCOMIndividualEvent;
begin
  event := GetIndividualEvent(iRec, 'DEAT');

  if (event = nil)
  then Result := ''
  else Result := event.Detail.Place;
end;

function GetAttributeValue(iRec: TGEDCOMIndividualRecord; attrName: string): string;
var
  i: Integer;
  attr: TGEDCOMIndividualAttribute;
begin
  Result := '';

  for i := 0 to iRec.IndividualAttributesCount - 1 do begin
    attr := iRec.IndividualAttributes[i];

    if (attr.Name = attrName) then begin
      Result := attr.StringValue;
      Exit;
    end;
  end;
end;

function GetAttributeStr(iAttr: TGEDCOMIndividualAttribute): string;
var
  idx: Integer;
  st: string;
begin
  idx := GetPersonAttributeIndex(iAttr.Name);
  if (idx = 0) then st := iAttr.Detail.Classification
  else
  if (idx > 0) then st := PersonAttributes[idx].Name
  else st := iAttr.Name;

  Result := st + ': ' + iAttr.StringValue;
end;

function GetMarriageDate(fRec: TGEDCOMFamilyRecord; aFormat: TDateFormat): string;
var
  event: TGEDCOMFamilyEvent;
begin
  event := GetFamilyEvent(fRec, 'MARR');

  if (event = nil)
  then Result := ''
  else Result := GEDCOMCustomDateToStr(event.Detail.Date.Value, aFormat);
end;

function GetEventDesc(evDetail: TGEDCOMEventDetail): string;
var
  dt: string;
begin
  dt := GEDCOMCustomDateToStr(evDetail.Date.Value, dfDD_MM_YYYY, False);

  if (dt = '') and (evDetail.Place = '')
  then Result := '?'
  else begin
    if (dt = '')
    then Result := evDetail.Place
    else
    if (evDetail.Place = '')
    then Result := dt
    else Result := dt + ', ' + evDetail.Place;
  end;
end;

function IsLive(iRec: TGEDCOMIndividualRecord): Boolean;
begin
  Result := (GetIndividualEvent(iRec, 'DEAT') = nil);
end;

function GetAbstractDate(aEventDetail: TGEDCOMEventDetail): Double;
var
  dt: TGEDCOMDate;
  y: Integer;
  m, d: Word;
begin
  Result := 0.0;

  dt := TGEDCOMDate(aEventDetail.Date.Value);
  dt.GetDate(y, m, d);
  if (y > 0) then begin
    Result := y;

    if (m > 0) then begin
      Result := Result + (m / 12);

      if (d > 0) then begin
        Result := Result + (d / DaysInAMonth(y, m) / 12);
      end;
    end;
  end;
end;

function GetLifeExpectancy(iRec: TGEDCOMIndividualRecord): string;
var
  y1, y2: Double;
  i: Integer;
  event: TGEDCOMIndividualEvent;
begin
  Result := '?';
  try
    y1 := -1.0;
    y2 := -1.0;

    // -1 - события нет вовсе
    // 0.0 - нет года
          
    for i := 0 to iRec.IndividualEventsCount - 1 do begin
      event := iRec.IndividualEvents[i];

      if (event.Name = 'BIRT')
      then y1 := GetAbstractDate(event.Detail)
      else
      if (event.Name = 'DEAT')
      then y2 := GetAbstractDate(event.Detail);
    end;

    if (y1 = -1.0) or (y2 = -1.0)
    then Result := ''
    else
    if (y1 = 0.0) or (y2 = 0.0)
    then Result := '?'
    else
      Result := IntToStr(Trunc(y2 - y1));
  except
  end;
end;

function GetAge(iRec: TGEDCOMIndividualRecord): string;
var
  y1, y2: Double;
  i: Integer;
  event: TGEDCOMIndividualEvent;
begin
  Result := '?';
  try
    y1 := -1.0;
    y2 := -1.0;

    for i := 0 to iRec.IndividualEventsCount - 1 do begin
      event := iRec.IndividualEvents[i];

      if (event.Name = 'BIRT')
      then y1 := GetAbstractDate(event.Detail)
      else
      if (event.Name = 'DEAT')
      then y2 := GetAbstractDate(event.Detail);
    end;

    if (y2 <= 1.0)
    then y2 := YearOf(Now()) + MonthOf(Now()) / 12;

    if (y1 = -1.0) or (y2 = -1.0)
    then Result := ''
    else
    if (y1 = 0.0) or (y2 = 0.0)
    then Result := '?'
    else
      Result := IntToStr(Trunc(y2 - y1));
  except
  end;
end;

function GetFirstbornAge(iRec: TGEDCOMIndividualRecord): string;
var
  y1, y2, y2tmp: Double;
  i, k: Integer;
  event: TGEDCOMIndividualEvent;
  family: TGEDCOMFamilyRecord;
  child: TGEDCOMIndividualRecord;
begin
  Result := '?';
  try
    y1 := 0.0;
    y2 := 0.0;

    event := GetIndividualEvent(iRec, 'BIRT');
    if (event = nil)
    then Exit
    else y1 := GetAbstractDate(event.Detail);

    for i := 0 to iRec.SpouseToFamilyLinksCount - 1 do begin
      family := iRec.SpouseToFamilyLinks[i].Family;

      for k := 0 to family.ChildrenCount - 1 do begin
        child := TGEDCOMIndividualRecord(family.Children[k].Value);

        event := GetIndividualEvent(child, 'BIRT');
        if (event <> nil) then begin
          y2tmp := GetAbstractDate(event.Detail);

          if (y2 = 0.0)
          then y2 := y2tmp
          else
          if (y2 > y2tmp)
          then y2 := y2tmp;
        end;
      end;
    end;

    if (y1 > 1.0) and (y2 > 1.0)
    then Result := IntToStr(Trunc(y2 - y1));
  except
  end;
end;

function GetMarriageAge(iRec: TGEDCOMIndividualRecord): string;
var
  y1, y2, y2tmp: Double;
  i: Integer;
  iEvent: TGEDCOMIndividualEvent;
  fEvent: TGEDCOMFamilyEvent;
  family: TGEDCOMFamilyRecord;
begin
  Result := '?';
  try
    y1 := 0.0;
    y2 := 0.0;

    iEvent := GetIndividualEvent(iRec, 'BIRT');
    if (iEvent = nil)
    then Exit
    else y1 := GetAbstractDate(iEvent.Detail);

    for i := 0 to iRec.SpouseToFamilyLinksCount - 1 do begin
      family := iRec.SpouseToFamilyLinks[i].Family;

      fEvent := GetFamilyEvent(family, 'MARR');
      if (fEvent <> nil) then begin
        y2tmp := GetAbstractDate(fEvent.Detail);

        if (y2 = 0.0)
        then y2 := y2tmp
        else
        if (y2 > y2tmp)
        then y2 := y2tmp;
      end;
    end;

    if (y1 > 1.0) and (y2 > 1.0)
    then Result := IntToStr(Trunc(y2 - y1));
  except
  end;
end;

function GetDaysForBirth(iRec: TGEDCOMIndividualRecord): string;
var
  dt: TGEDCOMDate;
  dt1, dt2: Double;
  bd_y: Integer;
  cur_y, cur_m, cur_d, bd_m, bd_d: Word;
  event: TGEDCOMIndividualEvent;
begin
  Result := '';
  try
    event := GetIndividualEvent(iRec, 'DEAT');
    if (event <> nil) then Exit;

    event := GetIndividualEvent(iRec, 'BIRT');
    if (event <> nil) then begin
      dt := TGEDCOMDate(event.Detail.Date.Value);
      dt.GetDate(bd_y, bd_m, bd_d);
      if (bd_m <= 0) or (bd_d <= 0) then Exit;

      DecodeDate(Now(), cur_y, cur_m, cur_d);
      //cur_y := 2009;
      //cur_m := 1;
      //cur_d := 20;

      dt1 := cur_y + (bd_m / 12) + (bd_d / 12 / 31);
      dt2 := cur_y + (cur_m / 12) + (cur_d / 12 / 31);

      if (dt1 < dt2)
      then bd_y := cur_y + 1
      else bd_y := cur_y;

      Result := IntToStr(DaysBetween(EncodeDate(cur_y, cur_m, cur_d), EncodeDate(bd_y, bd_m, bd_d)));
    end;
  except
  end;
end;

function HyperLink(XRef: string; Text: string): string;
begin
  Result := '~^' + XRef + ':' + Text + '~';
end;

type
  TRetCount = packed record
    lngSubRows: Word;
    lngCountLike: Word;
  end;

function Matching(StrA, StrB: String; lngLen: Integer): TRetCount;
var
  TempRet: TRetCount;
  PosStrA, PosStrB: Integer;
  StrTempA, StrTempB: String;
begin
  TempRet.lngSubRows := 0;
  TempRet.lngCountLike := 0;

  for PosStrA := 1 to Length(strA) - lngLen + 1 do begin
    StrTempA := Copy(strA, PosStrA, lngLen);

    PosStrB := 1;
    for PosStrB := 1 to Length(strB) - lngLen + 1 do begin
      StrTempB := Copy(strB, PosStrB, lngLen);
      if AnsiCompareText(StrTempA, StrTempB) = 0 then begin
        Inc(TempRet.lngCountLike);
        Break;
      end;
    end;

    Inc(TempRet.lngSubRows);
  end;

  Matching.lngCountLike := TempRet.lngCountLike;
  Matching.lngSubRows   := TempRet.lngSubRows;
end;

//------------------------------------------------------------------------------
// Функция нечеткого сравнения строк БЕЗ УЧЕТА РЕГИСТРА
//------------------------------------------------------------------------------
// MaxMatching - максимальная длина подстроки (достаточно 3-4)
// strInputMatching - сравниваемая строка
// strInputStandart - строка-образец
// Сравнивание без учета регистра
// if IndistinctMatching(4, "поисковая строка", "оригинальная строка  - эталон") > 40 then ...
function IndistinctMatching(MaxMatching: Integer; strInputMatching, strInputStandart: String): Integer;
var
  gret: TRetCount;
  tret: TRetCount;
  lngCurLen: Integer; //текущая длина подстроки
begin
  //если не передан какой-либо параметр, то выход
  if (MaxMatching = 0) or (Length(strInputMatching) = 0) or (Length(strInputStandart) = 0)
  then begin
    Result := 0;
    Exit;
  end;

  gret.lngCountLike:= 0;
  gret.lngSubRows  := 0;
  // Цикл прохода по длине сравниваемой фразы
  for lngCurLen := 1 to MaxMatching do begin
    // Сравниваем строку A со строкой B
    tret := Matching(strInputMatching, strInputStandart, lngCurLen);
    gret.lngCountLike := gret.lngCountLike + tret.lngCountLike;
    gret.lngSubRows   := gret.lngSubRows + tret.lngSubRows;
    // Сравниваем строку B со строкой A
    tret := Matching(strInputStandart, strInputMatching, lngCurLen);
    gret.lngCountLike := gret.lngCountLike + tret.lngCountLike;
    gret.lngSubRows   := gret.lngSubRows + tret.lngSubRows;
  end;

  if (gret.lngSubRows = 0) then begin
    Result := 0;
    Exit;
  end;

  Result := Trunc((gret.lngCountLike / gret.lngSubRows) * 100);
end;

//------------------------------------------------------------------------------
function IndistinctMatching2(A, B: String): Single;

  function Match(i, j: Byte): Integer;
  label _Loop;
  var
    GlobalSumm, Summ, Max: Integer;
  begin
    GlobalSumm := 0;
    Max := 0;
    _Loop:
       if (A[i] = B[j]) then begin
         Inc(GlobalSumm);
         if (i < Byte(Length(A))) and (j < Byte(Length(B))) then begin
           Inc(i);
           Inc(j);
           goto _Loop;
         end;
       end;
    if (i < Byte(Length(A))) and (j < Byte(Length(B))) then begin
       Summ := Match(i+1, j+1);
       if (Max < Summ) then Max := Summ;
    end;
    if (i < Byte(Length(A))) then begin
       Summ := Match(i+1, j);
       if (Max < Summ) then Max := Summ;
    end;
    if (j < Byte(Length(B))) then begin
      Summ := Match(i, j+1);
      if (Max < Summ) then Max := Summ;
    end;
    Result := GlobalSumm + Max;
  end;

begin
  Result := Match(1, 1) * 2.0 / (Byte(Length(A)) + Byte(Length(B)));
end;

function IndistinctMatching3(StrA, StrB: string): Integer;
var
  i, k, m: Integer;
  mtx: array of array of Char;
begin
  if (Length(StrA) = 0) or (Length(StrB) = 0)
  then begin
    Result := 0;
    Exit;
  end;

  SetLength(mtx, Length(StrA), Length(StrB));

  for i := 1 to Length(StrA) do begin
    for k := 1 to Length(StrB) do begin
      if (StrA[i] = StrB[k])
      then mtx[i - 1, k - 1] := '*'
      else mtx[i - 1, k - 1] := ' ';
    end;
  end;

  m := 0;
  i := 1;
  while (i <= Length(StrA)) do begin
    k := 1;
    while (k <= Length(StrB)) do begin
      if (i <= Length(StrA)) and (k <= Length(StrB)) and (mtx[i - 1, k - 1] = '*') then begin
        while (i <= Length(StrA)) and (k <= Length(StrB)) and (mtx[i - 1, k - 1] = '*') do begin
          Inc(m);
          Inc(i);
          Inc(k);
        end;
      end;

      Inc(k);
    end;

    Inc(i);
  end;

  Result := Round((m / Max(Length(StrA), Length(StrB))) * 100);
end;

function ClearFamily(aFamily: string): string;
var
  p: Integer;
begin
  p := Pos(' (', aFamily);
  if (p > 0)
  then Result := Copy(aFamily, 1, p - 1)
  else Result := aFamily;
end;

function GetSelIndex(aList: TBSListView): Integer;
begin
  if (aList.Selected = nil)
  then Result := -1
  else Result := Integer(aList.Selected.Data);
end;

function PrepareRusFamily(f: string; aFemale: Boolean): string;
var
  p: Integer;
begin
  p := Pos(' (', f);
  if (p > 0) then f := Copy(f, 1, p - 1);

  if (Length(f) > 0) then begin
    if (aFemale) then begin
      if (f[Length(f)] = 'а')
      then f := Copy(f, 1, Length(f) - 1){
      else
      if (Copy(f, Length(f) - 1, 2) = 'ая')
      then f := Copy(f, 1, Length(f) - 2) + 'ий'};
    end;

    if (f[1] = '(') and (f[Length(f)] = ')')
    then f := '?';
  end else f := '?';

  Result := f;
end;

function GetChildsCount(aPerson: TGEDCOMIndividualRecord): Integer;
var
  family: TGEDCOMFamilyRecord;
  i: Integer;
begin
  Result := 0;
  if (aPerson = nil) then Exit;

  if (aPerson.SpouseToFamilyLinksCount > 0) then begin
    for i := 0 to aPerson.SpouseToFamilyLinksCount - 1 do begin
      family := aPerson.SpouseToFamilyLinks[i].Family;
      Result := Result + family.ChildrenCount;
    end;
  end;
end;

function GetMarriagesCount(aPerson: TGEDCOMIndividualRecord): Integer;
begin
  if (aPerson = nil)
  then Result := 0
  else Result := aPerson.SpouseToFamilyLinksCount;
end;

function GetSpousesDiff(fRec: TGEDCOMFamilyRecord): string;
var
  y1, y2: Double;
  event: TGEDCOMIndividualEvent;
  h, w: TGEDCOMIndividualRecord;
begin
  Result := '?';
  try
    y1 := -1.0;
    y2 := -1.0;

    h := TGEDCOMIndividualRecord(fRec.Husband.Value);
    w := TGEDCOMIndividualRecord(fRec.Wife.Value);
    if (h = nil) or (w = nil) then Exit;

    event := GetIndividualEvent(h, 'BIRT');
    if (event <> nil)
    then y1 := GetAbstractDate(event.Detail);

    event := GetIndividualEvent(w, 'BIRT');
    if (event <> nil)
    then y2 := GetAbstractDate(event.Detail);

    if (y1 > 0.0) and (y2 > 0.0)
    then Result := IntToStr(Trunc(Abs(y2 - y1)));
  except
  end;
end;

procedure GetCommonStats(aTree: TGEDCOMTree; var aStats: TCommonStats);
var
  i, ch_cnt, m_cnt: Integer;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  v_age, v_life, v_fba, v_mage: string;
begin
  with aStats do begin
    persons := 0;
    persons_m := 0;
    persons_f := 0;

    lives := 0;
    lives_m := 0;
    lives_f := 0;

    age := 0;
    age_m := 0;
    age_f := 0;
    age_cnt := 0;
    age_m_cnt := 0;
    age_f_cnt := 0;

    life := 0;
    life_m := 0;
    life_f := 0;
    life_cnt := 0;
    life_m_cnt := 0;
    life_f_cnt := 0;

    childs := 0;
    childs_m := 0;
    childs_f := 0;
    childs_cnt := 0;
    childs_m_cnt := 0;
    childs_f_cnt := 0;

    fba := 0;
    fba_m := 0;
    fba_f := 0;
    fba_cnt := 0;
    fba_m_cnt := 0;
    fba_f_cnt := 0;

    marr := 0;
    marr_m := 0;
    marr_f := 0;
    marr_cnt := 0;
    marr_m_cnt := 0;
    marr_f_cnt := 0;

    mage := 0;
    mage_m := 0;
    mage_f := 0;
    mage_cnt := 0;
    mage_m_cnt := 0;
    mage_f_cnt := 0;

    for i := 0 to aTree.Count - 1 do begin
      rec := aTree.Records[i];

      if (rec is TGEDCOMIndividualRecord) then begin
        ind := rec as TGEDCOMIndividualRecord;

        Inc(persons);
        if IsLive(ind) then Inc(lives);

        v_age := GetAge(ind);
        if (v_age <> '') and (v_age <> '?') then begin
          age := age + StrToInt(v_age);
          Inc(age_cnt);
        end;

        v_life := GetLifeExpectancy(ind);
        if (v_life <> '') and (v_life <> '?') then begin
          life := life + StrToInt(v_life);
          Inc(life_cnt);
        end;

        ch_cnt := GetChildsCount(ind);
        if (ch_cnt <> 0) then begin
          childs := childs + ch_cnt;
          Inc(childs_cnt);
        end;

        v_fba := GetFirstbornAge(ind);
        if (v_fba <> '') and (v_fba <> '?') then begin
          fba := fba + StrToInt(v_fba);
          Inc(fba_cnt);
        end;

        m_cnt := GetMarriagesCount(ind);
        if (m_cnt <> 0) then begin
          marr := marr + m_cnt;
          Inc(marr_cnt);
        end;

        v_mage := GetMarriageAge(ind);
        if (v_mage <> '') and (v_mage <> '?') then begin
          mage := mage + StrToInt(v_mage);
          Inc(mage_cnt);
        end;

        case ind.Sex of
          svMale: begin
            Inc(persons_m);
            if IsLive(ind) then Inc(lives_m);

            if (v_age <> '') and (v_age <> '?') then begin
              age_m := age_m + StrToInt(v_age);
              Inc(age_m_cnt);
            end;

            if (v_life <> '') and (v_life <> '?') then begin
              life_m := life_m + StrToInt(v_life);
              Inc(life_m_cnt);
            end;

            if (ch_cnt <> 0) then begin
              childs_m := childs_m + ch_cnt;
              Inc(childs_m_cnt);
            end;

            if (v_fba <> '') and (v_fba <> '?') then begin
              fba_m := fba_m + StrToInt(v_fba);
              Inc(fba_m_cnt);
            end;

            if (m_cnt <> 0) then begin
              marr_m := marr_m + m_cnt;
              Inc(marr_m_cnt);
            end;

            if (v_mage <> '') and (v_mage <> '?') then begin
              mage_m := mage_m + StrToInt(v_mage);
              Inc(mage_m_cnt);
            end;
          end;

          svFemale: begin
            Inc(persons_f);
            if IsLive(ind) then Inc(lives_f);

            if (v_age <> '') and (v_age <> '?') then begin
              age_f := age_f + StrToInt(v_age);
              Inc(age_f_cnt);
            end;

            if (v_life <> '') and (v_life <> '?') then begin
              life_f := life_f + StrToInt(v_life);
              Inc(life_f_cnt);
            end;

            if (ch_cnt <> 0) then begin
              childs_f := childs_f + ch_cnt;
              Inc(childs_f_cnt);
            end;

            if (v_fba <> '') and (v_fba <> '?') then begin
              fba_f := fba_f + StrToInt(v_fba);
              Inc(fba_f_cnt);
            end;

            if (m_cnt <> 0) then begin
              marr_f := marr_f + m_cnt;
              Inc(marr_f_cnt);
            end;

            if (v_mage <> '') and (v_mage <> '?') then begin
              mage_f := mage_f + StrToInt(v_mage);
              Inc(mage_f_cnt);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure MergeFiles(aMainTree: TGEDCOMTree; aFileName: string);
var
  repMap: TXRefReplaceMap;
  i: Integer;
  extTree: TGEDCOMTree;
  rec: TGEDCOMRecord;
  newXRef: string;
begin
  extTree := TGEDCOMTree.Create;
  repMap := TXRefReplaceMap.Create;
  try
    extTree.LoadFromFile(aFileName);
    while (extTree.Count > 0) do begin
      rec := extTree.Extract(0);
      newXRef := aMainTree.XRefIndex_NewXRef(rec);
      repMap.AddXRef(rec, rec.XRef, newXRef);
      rec.XRef := newXRef;
      rec.ResetOwner(aMainTree);
      aMainTree.AddRecord(rec);
    end;

    for i := 0 to repMap.Count - 1 do begin
      rec := repMap.Records[i].Rec;
      rec.ReplaceXRefs(repMap);
    end;
  finally
    repMap.Destroy;
    extTree.Destroy;
  end;
end;

function GetChangeDate(aRec: TGEDCOMRecordWithLists): string;
begin
  try
    if (aRec.ChangeDate.ChangeDateTime <> 0)
    then Result := DateTimeToStr(aRec.ChangeDate.ChangeDateTime)
    else Result := '';
  except
    Result := '';
  end;
end;

procedure CheckRecord(aTree: TGEDCOMTree; aRec: TGEDCOMRecord);

  procedure ReformNote(note: TGEDCOMNotes);
  var
    strData: TStringList;
    noteRec: TGEDCOMNoteRecord;
  begin
    strData := TStringList.Create;
    try
      strData.Text := note.Notes.Text;

      noteRec := TGEDCOMNoteRecord.Create(aTree, aTree);
      noteRec.NewXRef;
      noteRec.Notes := strData;
      aTree.AddRecord(noteRec);

      note.Clear;
      note.Value := noteRec;
    finally
      strData.Destroy;
    end;
  end;

  procedure ReformMultimediaLink(mmLink: TGEDCOMMultimediaLink);
  var
    mmRec: TGEDCOMMultimediaRecord;
    i: Integer;
    fr: TGEDCOMFileReference;
    frt: TGEDCOMFileReferenceWithTitle;
    title: string;
  begin
    try
      title := mmLink.Title;

      mmRec := TGEDCOMMultimediaRecord.Create(aTree, aTree);
      mmRec.NewXRef;
      aTree.AddRecord(mmRec);

      for i := 0 to mmLink.FileReferencesCount - 1 do begin
        fr := mmLink.FileReferences[i];

        frt := TGEDCOMFileReferenceWithTitle.Create(aTree, mmRec);
        if (fr.MultimediaFormat <> mfNone) then frt.MultimediaFormat := fr.MultimediaFormat;
        if (fr.MediaType <> mtNone) then frt.MediaType := fr.MediaType;
        frt.LinkFile(fr.StringValue);

        mmRec.AddFileReference(frt);
      end;

      mmLink.Clear;
      mmLink.Value := mmRec;
      mmLink.Title := title;
    finally
    end;
  end;

  procedure ReformSourceCitation(sourCit: TGEDCOMSourceCitation);
  begin
  end;

  procedure PrepareTag(tag: TGEDCOMTagWithLists);
  var
    i: Integer;
    mmLink: TGEDCOMMultimediaLink;
    note: TGEDCOMNotes;
    sourCit: TGEDCOMSourceCitation;
    exTag: TGEDCOMTagWithListsEx;
  begin
    exTag := TGEDCOMTagWithListsEx(tag);

    for i := 0 to exTag.MultimediaLinksCount - 1 do begin
      mmLink := exTag.MultimediaLinks[i];
      if not(mmLink.IsPointer) then ReformMultimediaLink(mmLink);
    end;

    for i := 0 to exTag.NotesCount - 1 do begin
      note := exTag.Notes[i];
      if not(note.IsPointer) then ReformNote(note);
    end;

    for i := 0 to exTag.SourceCitationsCount - 1 do begin
      sourCit := exTag.SourceCitations[i];
      if not(sourCit.IsPointer) then ReformSourceCitation(sourCit);
    end;
  end;

  procedure PreparePtr(ptr: TGEDCOMPointerWithNotes);
  var
    i: Integer;
    note: TGEDCOMNotes;
  begin
    for i := 0 to ptr.NotesCount - 1 do begin
      note := ptr.Notes[i];
      if not(note.IsPointer) then ReformNote(note);
    end;
  end;

var
  rwl: TGEDCOMRecordWithLists;
  i: Integer;
  mmLink: TGEDCOMMultimediaLink;
  note: TGEDCOMNotes;
  sourCit: TGEDCOMSourceCitation;
  fam: TGEDCOMFamilyRecord;
  ind: TGEDCOMIndividualRecord;
begin
  if (aRec is TGEDCOMRecordWithLists) then begin
    rwl := aRec as TGEDCOMRecordWithLists;

    for i := 0 to rwl.MultimediaLinksCount - 1 do begin
      mmLink := rwl.MultimediaLinks[i];
      if not(mmLink.IsPointer) then ReformMultimediaLink(mmLink);
    end;

    for i := 0 to rwl.NotesCount - 1 do begin
      note := rwl.Notes[i];
      if not(note.IsPointer) then ReformNote(note);
    end;

    for i := 0 to rwl.SourceCitationsCount - 1 do begin
      sourCit := rwl.SourceCitations[i];
      if not(sourCit.IsPointer) then ReformSourceCitation(sourCit);
    end;


    if (rwl is TGEDCOMFamilyRecord) then begin
      fam := rwl as TGEDCOMFamilyRecord;

      fam.SortChilds();

      for i := 0 to fam.FamilyEventCount - 1 do PrepareTag(fam.FamilyEvents[i].Detail);
      for i := 0 to fam.SpouseSealingCount - 1 do PrepareTag(fam.SpouseSealing[i]);
    end
    else
    if (rwl is TGEDCOMIndividualRecord) then begin
      ind := rwl as TGEDCOMIndividualRecord;

      for i := 0 to ind.IndividualAttributesCount - 1 do PrepareTag(ind.IndividualAttributes[i].Detail);
      for i := 0 to ind.IndividualEventsCount - 1 do PrepareTag(ind.IndividualEvents[i].Detail);
      for i := 0 to ind.IndividualOrdinancesCount - 1 do PrepareTag(ind.IndividualOrdinances[i]);

      for i := 0 to ind.ChildToFamilyLinksCount - 1 do PreparePtr(ind.ChildToFamilyLinks[i]);
      for i := 0 to ind.SpouseToFamilyLinksCount - 1 do PreparePtr(ind.SpouseToFamilyLinks[i]);
      for i := 0 to ind.AssociationsCount - 1 do PreparePtr(ind.Associations[i]);
    end
    else
    if (rwl is TGEDCOMMultimediaRecord) then begin
      // dummy
    end
    else
    if (rwl is TGEDCOMNoteRecord) then begin
      // dummy
    end
    else
    if (rwl is TGEDCOMRepositoryRecord) then begin
      // dummy
    end
    else
    if (rwl is TGEDCOMSourceRecord) then begin
      // dummy
    end
    else
    if (rwl is TGEDCOMSubmissionRecord) then begin
      // dummy
    end
    else
    if (rwl is TGEDCOMSubmitterRecord) then begin
      // dummy
    end
    else
    if (rwl is TGEDCOMGroupRecord) then begin
      // dummy
    end;
  end;
end;

function CheckGEDCOMFormat(aTree: TGEDCOMTree): Boolean;

  procedure CorrectIds();
  var
    repMap: TXRefReplaceMap;
    i: Integer;
    rec: TGEDCOMRecord;
    newXRef: string;
  begin
    ProgressInit(aTree.Count, 'Коррекция идентификаторов');

    repMap := TXRefReplaceMap.Create;
    try
      for i := 0 to aTree.Count - 1 do begin
        rec := aTree.Records[i];

        if (GetId(rec) = 0) then begin
          newXRef := aTree.XRefIndex_NewXRef(rec);
          repMap.AddXRef(rec, rec.XRef, newXRef);
          rec.XRef := newXRef;
        end;

        ProgressStep();
      end;

      aTree.Header.ReplaceXRefs(repMap);

      ProgressInit(repMap.Count, 'Коррекция идентификаторов');
      for i := 0 to repMap.Count - 1 do begin
        rec := repMap.Records[i].Rec;
        rec.ReplaceXRefs(repMap);

        ProgressStep();
      end;
    finally
      repMap.Destroy;

      ProgressDone();
    end;
  end;

var
  i: Integer;
  rec: TGEDCOMRecord;
  idCheck: Boolean;
begin
  ProgressInit(aTree.Count, 'Проверка формата');
  try
    idCheck := True;
    i := 0;
    while (i < aTree.Count) do begin
      rec := aTree.Records[i];
      CheckRecord(aTree, rec);

      if (idCheck) and (GetId(rec) = 0)
      then idCheck := False;

      ProgressStep();

      Inc(i);
    end;
  finally
    ProgressDone();
  end;

  if not(idCheck) then begin
    if (MessageDlg('Требуется коррекция идентификаторов записей, продолжить?', mtWarning, [mbYes, mbNo], 0) = mrYes)
    then CorrectIds();
  end;
end;

procedure LoadExtFile(const aFileName: string);
begin
  ShellExecute(0, 'open', PChar(aFileName), nil, nil, SW_SHOW);
end;

{ TNamesTable }

constructor TNamesTable.Create;
begin
  inherited Create;
  FNames := TObjectList.Create(True);
end;

destructor TNamesTable.Destroy;
begin
  FNames.Destroy;
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

procedure TNamesTable.SetName(aName, aPatronymic: string; aSex: TGEDCOMSex);
var
  n: TName;
begin
  n := FindName(aName);
  if (n = nil) then begin
    n := TName.Create;
    n.Name := aName;
    FNames.Add(n);
  end;

  case aSex of
    svMale: if (n.M_Patronymic = '') then n.M_Patronymic := aPatronymic;
    svFemale: if (n.F_Patronymic = '') then n.F_Patronymic := aPatronymic;
  end;
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
  fam, nam, pat, f_fam, f_nam, f_pat: string;
begin
  for i := 0 to aTree.Count - 1 do
    if (aTree.Records[i] is TGEDCOMIndividualRecord) then begin
      iRec := aTree.Records[i] as TGEDCOMIndividualRecord;

      GetNameParts(iRec, fam, nam, pat);

      if (iRec.ChildToFamilyLinksCount <> 0) then begin
        family := iRec.ChildToFamilyLinks[0].Family;
        if (family <> nil) then begin
          iFather := TGEDCOMIndividualRecord(family.Husband.Value);
          if (iFather <> nil) then begin
            GetNameParts(iFather, f_fam, f_nam, f_pat);

            if (Length(pat) > 1) and (Length(f_nam) > 1) and Comparable(f_nam, pat)
            then SetName(f_nam, pat, iRec.Sex);
          end;
        end;
      end;
    end;
end;

procedure TNamesTable.LoadFromFile(const aFileName: string);
var
  tf: TextFile;
  st: string;
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
    Writeln(tf, nm.Name + ';' + nm.F_Patronymic + ';' + nm.M_Patronymic);
  end;
  CloseFile(tf);
end;

{ TChartOptions }

constructor TChartOptions.Create;
begin
  inherited Create;

  FFamilyVisible := True;
  FNameVisible := True;
  FPatronymicVisible := True;
  FDiffLines := False;
  FBirthDateVisible := False;
  FDeathDateVisible := False;
  FKinship := False;

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

{ TPedigreeOptions }

constructor TPedigreeOptions.Create;
begin

end;

destructor TPedigreeOptions.Destroy;
begin

  inherited Destroy;
end;

{ TGlobalOptions }

constructor TGlobalOptions.Create;
begin
  FChartOptions := TChartOptions.Create;
  FPedigreeOptions := TPedigreeOptions.Create;
  FProxy := TProxy.Create;
end;

destructor TGlobalOptions.Destroy;
begin
  FProxy.Destroy;
  FPedigreeOptions.Destroy;
  FChartOptions.Destroy;

  inherited Destroy;
end;

end.
