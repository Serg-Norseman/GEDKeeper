unit GKCommon;

{$I GEDKeeper.inc}

interface

uses
  Classes, Contnrs, GedCom551, Graphics, Dialogs, bsCtrls, IniFiles;

{$IFNDEF DELPHI_NET}
type
  IntPtr = type Integer;
{$ENDIF}

resourcestring
  AppName = 'GEDKeeper';

const
  UnkFemale = 'неизвестная';
  UnkMale = 'неизвестный';

const
  AdvTag = '_ADVANCED';
  ExtTag = '_EXT_NAME';
  PatriarchTag = '_PATRIARCH';

  MLinkPrefix = 'view_';

type
  TWorkMode = (wmSimple, wmExpert);

  TShieldState = (
    // не показываются ни секретные, ни конфиденциальные данные
    ssMaximum,
    // показывается всё кроме секретного
    ssMiddle,
    // показывается всё
    ssNone);

  TGEDCOMRecordType = (
    rtNone, rtIndividual, rtFamily, rtNote, rtMultimedia, rtSource,
    rtRepository, rtGroup, rtResearch, rtTask, rtCommunication, rtLocation,
    rtSubmission, rtSubmitter);

  TSelectMode = (smPerson, smFamily, smNote, smMultimedia, smSource, smRepository,
    smGroup, smTask, smCommunication, smLocation);

  TRecAction = (raAdd, raEdit, raDelete, raJump, raMoveUp, raMoveDown);

  TTargetMode = (tmNone, tmAncestor, tmDescendant, tmChildToFamily);
  TLifeMode = (lmAll, lmOnlyAlive, lmOnlyDead, lmAliveBefore);

  TFamilyTarget = (ftNone, ftSpouse, ftChild);

const
  SelectRecords: array [TSelectMode] of TGEDCOMRecordType = (
    rtIndividual, rtFamily, rtNote, rtMultimedia, rtSource, rtRepository,
    rtGroup, rtTask, rtCommunication, rtLocation
  );

type
  TPersonColumnType = (
    pctPatriarch, pctName, pctSex, pctBirthDate, pctDeathDate,
    pctBirthPlace, pctDeathPlace, pctResidence,
    pctAge, pctLifeExpectancy, pctDaysForBirth, pctGroups,
    pctReligion, pctNationality, pctEducation, pctOccupation, pctCaste,
    pctMili, pctMiliInd, pctMiliDis, pctMiliRank,
    pctChangeDate
  );

  TPersonColumnProps = record
    colType: TPersonColumnType;
    colActive: Boolean;
  end;

  TPersonColumnsList = array [0..Ord(pctChangeDate)] of TPersonColumnProps;

const
  PersonColumnsName: array [TPersonColumnType] of record
    Name: string;
    DefWidth: Integer; 
  end = (
    (Name: 'Патриарх'; DefWidth: 25),
    (Name: 'ФИО'; DefWidth: 25),

    (Name: 'Пол'; DefWidth: 45),
    (Name: 'Дата рождения'; DefWidth: 100),
    (Name: 'Дата смерти'; DefWidth: 100),
    (Name: 'Место рождения'; DefWidth: 100),
    (Name: 'Место смерти'; DefWidth: 100),
    (Name: 'Местожительство'; DefWidth: 100),

    (Name: 'Возраст'; DefWidth: 100),
    (Name: 'Продолжительность жизни'; DefWidth: 100),
    (Name: 'Дней до ДР'; DefWidth: 100),
    (Name: 'Группа'; DefWidth: 200),

    (Name: 'Вероисповедание'; DefWidth: 200),
    (Name: 'Национальность'; DefWidth: 200),
    (Name: 'Образование'; DefWidth: 200),
    (Name: 'Профессия'; DefWidth: 200),
    (Name: 'Социальное положение'; DefWidth: 200),

    (Name: 'Военная служба'; DefWidth: 200),
    (Name: 'Призван в ВС'; DefWidth: 200),
    (Name: 'Уволен из ВС'; DefWidth: 200),
    (Name: 'Звание в ВС'; DefWidth: 200),

    (Name: 'Изменено'; DefWidth: 150)
  );

const
  DefPersonColumns: TPersonColumnsList = (
    (colType: pctPatriarch; colActive: True),
    (colType: pctName; colActive: True),
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

    (colType: pctChangeDate; colActive: True)
  );

type
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

    procedure LoadFromFile(const aIniFile: TIniFile);
    procedure SaveToFile(const aIniFile: TIniFile);

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
    FGEDCOMOptimize: Boolean;
    FLastDir: string;
    FListPersonsColumns: TPersonColumnsList;
    FMRUFiles: TStringList;
    FNameFilters: TStringList;
    FPedigreeOptions: TPedigreeOptions;
    FPlacesWithAddress: Boolean;
    FProxy: TProxy;
    FRelations: TStringList;
    FResidenceFilters: TStringList;
    FShowTips: Boolean;
    FWorkMode: TWorkMode;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    property ChartOptions: TChartOptions read FChartOptions;
    property DefCharacterSet: TGEDCOMCharacterSet read FDefCharacterSet write FDefCharacterSet;
    property DefDateFormat: TDateFormat read FDefDateFormat write FDefDateFormat;
    property DefNameFormat: TNameFormat read FDefNameFormat write FDefNameFormat;
    property GEDCOMOptimize: Boolean read FGEDCOMOptimize write FGEDCOMOptimize;
    property LastDir: string read FLastDir write FLastDir;
    property MRUFiles: TStringList read FMRUFiles;
    property NameFilters: TStringList read FNameFilters;
    property PlacesWithAddress: Boolean read FPlacesWithAddress write FPlacesWithAddress;
    property Relations: TStringList read FRelations;
    property ResidenceFilters: TStringList read FResidenceFilters;
    property WorkMode: TWorkMode read FWorkMode write FWorkMode;

    property ListPersonsColumns: TPersonColumnsList
      read FListPersonsColumns write FListPersonsColumns;

    property PedigreeOptions: TPedigreeOptions read FPedigreeOptions;
    property Proxy: TProxy read FProxy;

    property ShowTips: Boolean read FShowTips write FShowTips;
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

  PersonEventsSize = 36;
  PersonEvents: array [0..PersonEventsSize-1] of record
    Name: string;
    Sign: string;
    Kind: (ekEvent, ekFact);
  end = (
    (Name: 'Новое событие'; Sign: 'EVEN'; Kind: ekEvent),              {std:check, EV}
    (Name: 'Рождение'; Sign: 'BIRT'; Kind: ekEvent),                   {std:check, EV}
    (Name: 'Усыновление'; Sign: 'ADOP'; Kind: ekEvent),                {std:check, EV}
    (Name: 'Крещение'; Sign: 'CHR'; Kind: ekEvent),                    {std:check, EV}
    (Name: 'Получение ученой степени'; Sign: 'GRAD'; Kind: ekEvent),   {std:check, EV}
    (Name: 'Уход на пенсию'; Sign: 'RETI'; Kind: ekEvent),             {std:check, EV}
    (Name: 'Натурализация'; Sign: 'NATU'; Kind: ekEvent),              {std:check, EV}
    (Name: 'Эмиграция'; Sign: 'EMIG'; Kind: ekEvent),                  {std:check, EV}
    (Name: 'Иммиграция'; Sign: 'IMMI'; Kind: ekEvent),                 {std:check, EV}
    (Name: 'Перепись'; Sign: 'CENS'; Kind: ekEvent),                   {std:check, EV}
    (Name: 'Завещание'; Sign: 'WILL'; Kind: ekEvent),                  {std:check, EV}
    (Name: 'Утверждение завещания'; Sign: 'PROB'; Kind: ekEvent),      {std:check, EV}
    (Name: 'Смерть'; Sign: 'DEAT'; Kind: ekEvent),                     {std:check, EV}
    (Name: 'Похороны'; Sign: 'BURI'; Kind: ekEvent),                   {std:check, EV}
    (Name: 'Кремация'; Sign: 'CREM'; Kind: ekEvent),                   {std:check, EV}

    (Name: 'Факт'; Sign: 'FACT'; Kind: ekFact),                        {std:check, AT}
    (Name: 'Вероисповедание'; Sign: 'RELI'; Kind: ekFact),             {std:check, AT}
    (Name: 'Национальность'; Sign: 'NATI'; Kind: ekFact),              {std:check, AT}
    (Name: 'Местожительство'; Sign: 'RESI'; Kind: ekFact),             {std:check, AT}
    (Name: 'Физическое описание'; Sign: 'DSCR'; Kind: ekFact),         {std:check, AT}
    (Name: 'Идентификационный номер'; Sign: 'IDNO'; Kind: ekFact),     {std:check, AT}
    (Name: 'Код социального страхования'; Sign: 'SSN'; Kind: ekFact),  {std:check, AT}
    (Name: 'Кол-во детей'; Sign: 'NCHI'; Kind: ekFact),                {std:check, AT}
    (Name: 'Кол-во браков'; Sign: 'NMR'; Kind: ekFact),                {std:check, AT}
    (Name: 'Образование'; Sign: 'EDUC'; Kind: ekFact),                 {std:check, AT}
    (Name: 'Профессия'; Sign: 'OCCU'; Kind: ekFact),                   {std:check, AT}
    (Name: 'Социальное положение'; Sign: 'CAST'; Kind: ekFact),        {std:check, AT}
    (Name: 'Собственность'; Sign: 'PROP'; Kind: ekFact),               {std:check, AT}
    (Name: 'Титул'; Sign: 'TITL'; Kind: ekFact),                       {std:check, AT}

    (Name: 'Путешествие'; Sign: '_TRAVEL'; Kind: ekFact),              {non-std, AT}
    (Name: 'Хобби'; Sign: '_HOBBY'; Kind: ekFact),                     {non-std, AT}
    (Name: 'Награда'; Sign: '_AWARD'; Kind: ekFact),                   {non-std, AT}

    (Name: 'Военная служба'; Sign: '_MILI'; Kind: ekFact),             {non-std, AT}
    (Name: 'Призван в ВС'; Sign: '_MILI_IND'; Kind: ekFact),           {non-std, AT}
    (Name: 'Уволен из ВС'; Sign: '_MILI_DIS'; Kind: ekFact),           {non-std, AT}
    (Name: 'Звание в ВС'; Sign: '_MILI_RANK'; Kind: ekFact)            {non-std, AT}
  );

{
MILA     Gen                  Military Award
MILD     Gen                  Military Discharge   
MILF     Reunion, Gen         Served in Military  
MILI     Reunion              Military  
MILT     Gen                  Military Services
}

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

type
  TGKStoreType = (gstReference, gstArchive, gstStorage);

const
  GKStoreType: array [TGKStoreType] of record
    Name: string;
    Sign: string;
  end = (
    (Name: 'Ссылка на файл'; Sign: ''), {for compatibility}
    (Name: 'Размещение в архиве'; Sign: 'arc:'),
    (Name: 'Размещение в хранилище'; Sign: 'stg:')
  );

const
  MediaTypes: array [TGEDCOMMediaType] of record
    Name: string;
  end = (
    (Name: '-'),
    (Name: 'Звукозапись'),
    (Name: 'Книга'),
    (Name: 'Карточка'), 
    (Name: 'Электронный'),
    (Name: 'Микрофиша'),
    (Name: 'Фильм'),
    (Name: 'Журнал'),
    (Name: 'Рукопись'),
    (Name: 'Карта'),
    (Name: 'Газета'),
    (Name: 'Фотография'),
    (Name: 'Надгробие'),
    (Name: 'Видео'),
    (Name: '- Другой -')
  );

const
  PriorityNames: array [TResearchPriority] of string = (
    'Не задан', 'Низкий', 'Нормальный', 'Высокий', 'Срочный');

  StatusNames: array [TResearchStatus] of string = (
    'Определено', 'Выполняется', 'Задержано',
    'Осложнения', 'Завершено', 'Отозвано');

  CommunicationNames: array [TCommunicationType] of string = (
    'Звонок', 'Эл.письмо', 'Факс', 'Письмо', 'Кассета', 'Визит');

  CommunicationDirs: array [TCommunicationDir] of string = ('от', 'к');

  GoalNames: array [TGoalType] of string = (
    'персона', 'семья', 'источник', 'иная');

  CertaintyAssessments: array [0..3] of string = (
    'Ненадежное подтверждение или предполагаемые данные',
    'Сомнительная надежность подтверждения',
    'Косвенные доказательства',
    'Прямые и первичные доказательства'
  );

function IsRecordAccess(aRecRestriction: TGEDCOMRestriction; aShieldState: TShieldState): Boolean;

function GetPersonEventIndex(aSign: string): Integer;
function GetFamilyEventIndex(aSign: string): Integer;

function GetMarriageStatusIndex(aSign: string): Integer;

function GetEventName(aEvent: TGEDCOMCustomEvent): string;

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
function GetResidencePlace(iRec: TGEDCOMIndividualRecord; IncludeAddress: Boolean): string;

function GetAttribute(iRec: TGEDCOMIndividualRecord; attrName: string): TGEDCOMIndividualAttribute;
function GetAttributeValue(iRec: TGEDCOMIndividualRecord; attrName: string): string;
function GetAttributeStr(iAttr: TGEDCOMIndividualAttribute): string;

function GetMarriageDate(fRec: TGEDCOMFamilyRecord; aFormat: TDateFormat): string;
function GetEventDesc(evDetail: TGEDCOMEventDetail): string;
function GetEventCause(evDetail: TGEDCOMEventDetail): string;

function IsLive(iRec: TGEDCOMIndividualRecord): Boolean;
procedure GetIndependentDate(aDate: TGEDCOMCustomDate; var AYear: Integer; var AMonth, ADay: Word);
function GetEventsYearsDiff(ev1, ev2: TGEDCOMIndividualEvent; aCurEnd: Boolean = False): string;
function GetLifeExpectancy(iRec: TGEDCOMIndividualRecord): string;
function GetAge(iRec: TGEDCOMIndividualRecord): string;
function GetFirstbornAge(iRec: TGEDCOMIndividualRecord): string;
function GetMarriageAge(iRec: TGEDCOMIndividualRecord): string;
function GetDaysForBirth(iRec: TGEDCOMIndividualRecord): string;

function GetChangeDate(aRec: TGEDCOMRecord): string;

function CreateEventEx(aTree: TGEDCOMTree; iRec: TGEDCOMIndividualRecord;
  evName: string): TGEDCOMCustomEvent;

procedure CreateIEvent(aTree: TGEDCOMTree; iRec: TGEDCOMIndividualRecord;
  evSign, evDate, evPlace: string);
  
function CreatePersonEx(aTree: TGEDCOMTree; aName, aPatronymic, aFamily: string;
  aSex: TGEDCOMSex; aBirthEvent: Boolean = False): TGEDCOMIndividualRecord;

procedure AddSpouseToFamily(aTree: TGEDCOMTree; aFamily: TGEDCOMFamilyRecord;
  aSpouse: TGEDCOMIndividualRecord);
procedure RemoveFamilySpouse(aTree: TGEDCOMTree; aFamily: TGEDCOMFamilyRecord;
  aSpouse: TGEDCOMIndividualRecord);

function CreateNoteEx(aTree: TGEDCOMTree; aText: TStrings;
  aRecord: TGEDCOMRecord = nil): TGEDCOMNoteRecord;

function CreateFamilyEx(aTree: TGEDCOMTree): TGEDCOMFamilyRecord;

function IsMatchesMask(const aName, Mask: string): Boolean;

function HyperLink(XRef: string; Text: string; Num: Integer = 0): string;

function GetCorresponderStr(aTree: TGEDCOMTree; aRec: TGEDCOMCommunicationRecord;
  aLink: Boolean): string;

procedure GetTaskGoal(aTree: TGEDCOMTree; aRec: TGEDCOMTaskRecord;
  var aType: TGoalType; var aGoalRec: TGEDCOMRecord);
function GetTaskGoalStr(aTree: TGEDCOMTree; aRec: TGEDCOMTaskRecord): string;

function IndistinctMatching(MaxMatching: Integer; strInputMatching, strInputStandart: String): Integer;

function ClearFamily(aFamily: string): string;

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

function GetRecordType(rec: TGEDCOMRecord): TGEDCOMRecordType;
function RecordIsType(aRecType: TGEDCOMRecordType; aRec: TGEDCOMRecord): Boolean;

procedure GetCommonStats(aTree: TGEDCOMTree; var aStats: TCommonStats);

function CheckGEDCOMFormat(aTree: TGEDCOMTree): Boolean;

procedure LoadExtFile(const aFileName: string);

// замена данных в потоке с кодировки 1251 на UTF-8
function StreamToUtf8Stream(Stream: TStream): UTF8String;

function ConStrings(aStrings: TStrings): string;

type
  TTextFileEx = class(TObject)
  private
    FStream: TStream;
  public
    constructor Create(aStream: TStream);

    function Eof(): Boolean;
    function ReadLn(): string;
    procedure WriteLn(const s: string); overload;
  end;

const
  LineEnd = #13#10;

implementation

uses
  {$IFDEF DELPHI_NET}
  System.IO,
  {$ENDIF}
  Windows, SysUtils, Math, DateUtils, ShellAPI, Controls, Masks,
  GKProgress, bsComUtils, bsMiscUtils, StorageCrypt;

function IsRecordAccess(aRecRestriction: TGEDCOMRestriction; aShieldState: TShieldState): Boolean;
begin
  Result := False;

  case aShieldState of
    ssMaximum: Result := not(aRecRestriction in [rnConfidential, rnPrivacy]);

    ssMiddle: Result := not(aRecRestriction in [rnPrivacy]);

    ssNone: Result := True;
  end;
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

function GetEventName(aEvent: TGEDCOMCustomEvent): string;
var
  ev: Integer;
begin
  if (aEvent is TGEDCOMIndividualEvent) or (aEvent is TGEDCOMIndividualAttribute) then begin
    ev := GetPersonEventIndex(aEvent.Name);
    if (ev = 0) then Result := aEvent.Detail.Classification
    else
    if (ev > 0) then Result := PersonEvents[ev].Name
    else Result := aEvent.Name;
  end
  else
  if (aEvent is TGEDCOMFamilyEvent) then begin
    ev := GetFamilyEventIndex(aEvent.Name);
    if (ev = 0) then Result := aEvent.Detail.Classification
    else
    if (ev > 0) then Result := FamilyEvents[ev].Name
    else Result := aEvent.Name;
  end
  else Result := '';
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
    Result := -1;
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
  year: Integer;
  month, day: Word;
begin
  try
    Result := 0;

    GetIndependentDate(aDate, year, month, day);

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
  else Result := event.Detail.Place.StringValue;
end;

function GetDeathPlace(iRec: TGEDCOMIndividualRecord): string;
var
  event: TGEDCOMIndividualEvent;
begin
  event := GetIndividualEvent(iRec, 'DEAT');

  if (event = nil)
  then Result := ''
  else Result := event.Detail.Place.StringValue;
end;

function GetResidencePlace(iRec: TGEDCOMIndividualRecord; IncludeAddress: Boolean): string;
var
  attr: TGEDCOMIndividualAttribute;
  resi, addr: string;
begin
  attr := GetAttribute(iRec, 'RESI');
  if (attr = nil)
  then Result := ''
  else begin
    Result := attr.Detail.Place.StringValue;

    if (IncludeAddress) then begin
      resi := attr.StringValue;
      addr := Trim(attr.Detail.Address.Address.Text);
      if (resi <> '') and (addr <> '') then resi := resi + ', ';
      resi := resi + addr;

      if (resi <> '')
      then Result := Result + ' [' + resi + ']';
    end;
  end;
end;

function GetAttribute(iRec: TGEDCOMIndividualRecord; attrName: string): TGEDCOMIndividualAttribute;
var
  i: Integer;
  attr: TGEDCOMIndividualAttribute;
begin
  Result := nil;

  for i := 0 to iRec.IndividualAttributesCount - 1 do begin
    attr := iRec.IndividualAttributes[i];

    if (attr.Name = attrName) then begin
      Result := attr;
      Exit;
    end;
  end;
end;

function GetAttributeValue(iRec: TGEDCOMIndividualRecord; attrName: string): string;
var
  attr: TGEDCOMIndividualAttribute;
begin
  attr := GetAttribute(iRec, attrName);
  if (attr = nil)
  then Result := ''
  else Result := attr.StringValue;
end;

function GetAttributeStr(iAttr: TGEDCOMIndividualAttribute): string;
var
  idx: Integer;
  st: string;
begin
  idx := GetPersonEventIndex(iAttr.Name);
  if (idx = 0) then st := iAttr.Detail.Classification
  else
  if (idx > 0) then st := PersonEvents[idx].Name
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
  dt, place: string;
  location: TGEDCOMLocationRecord;
begin
  dt := GEDCOMCustomDateToStr(evDetail.Date.Value, dfDD_MM_YYYY, False);

  place := evDetail.Place.StringValue;
  location := TGEDCOMLocationRecord(evDetail.Place.Location.Value);
  if (place <> '') and (location <> nil)
  then place := HyperLink(location.XRef, place);

  if (dt = '') and (place = '')
  then Result := '?'
  else begin
    if (dt = '')
    then Result := place
    else
    if (place = '')
    then Result := dt
    else Result := dt + ', ' + place;
  end;
end;

function GetEventCause(evDetail: TGEDCOMEventDetail): string;
begin
  Result := '';

  if (evDetail.Cause <> '')
  then Result := Result + evDetail.Cause;

  if (evDetail.Agency <> '') then begin
    if (Result <> '')
    then Result := Result + ' ';

    Result := Result + '[' + evDetail.Agency + ']';
  end;
end;

function IsLive(iRec: TGEDCOMIndividualRecord): Boolean;
begin
  Result := (GetIndividualEvent(iRec, 'DEAT') = nil);
end;

procedure GetIndependentDate(aDate: TGEDCOMCustomDate; var AYear: Integer; var AMonth, ADay: Word);
var
  dt_range: TGEDCOMDateRange;
  dt_period: TGEDCOMDatePeriod;
  dt: TGEDCOMDate;
begin
  if (aDate is TGEDCOMDateApproximated) then begin
    dt := TGEDCOMDate(aDate);
    dt.GetDate(AYear, AMonth, ADay);
  end
  else
  if (aDate is TGEDCOMDateRange) then begin
    dt_range := TGEDCOMDateRange(aDate);

    if (dt_range.After.StringValue = '') and (dt_range.Before.StringValue <> '')
    then dt_range.Before.GetDate(AYear, AMonth, ADay)
    else
    if (dt_range.After.StringValue <> '') and (dt_range.Before.StringValue = '')
    then dt_range.After.GetDate(AYear, AMonth, ADay)
    else
    if (dt_range.After.StringValue <> '') and (dt_range.Before.StringValue <> '')
    then dt_range.After.GetDate(AYear, AMonth, ADay) // + dt_range.Before.GetDate(AYear, AMonth, ADay)) / 2
  end
  else
  if (aDate is TGEDCOMDatePeriod) then begin
    dt_period := TGEDCOMDatePeriod(aDate);

    if (dt_period.DateFrom.StringValue <> '') and (dt_period.DateTo.StringValue = '')
    then dt_period.DateFrom.GetDate(AYear, AMonth, ADay)
    else
    if (dt_period.DateFrom.StringValue = '') and (dt_period.DateTo.StringValue <> '')
    then dt_period.DateTo.GetDate(AYear, AMonth, ADay)
    else
    if (dt_period.DateFrom.StringValue <> '') and (dt_period.DateTo.StringValue <> '')
    then dt_period.DateFrom.GetDate(AYear, AMonth, ADay)
  end
  else
  if (aDate is TGEDCOMDate) then begin
    TGEDCOMDate(aDate).GetDate(AYear, AMonth, ADay);
  end;
end;

function GetAbstractDate(aEventDetail: TGEDCOMEventDetail): Double;
var
  dt: TGEDCOMDate;
  y: Integer;
  m, d: Word;
begin
  Result := 0.0;

  dt := TGEDCOMDate(aEventDetail.Date.Value);
  if not((dt is TGEDCOMDate)) then Exit;

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

function GetEventsYearsDiff(ev1, ev2: TGEDCOMIndividualEvent; aCurEnd: Boolean = False): string;
var
  y1, y2: Double;
begin
  Result := '?';
  try
    if (ev1 = nil)
    then y1 := -1.0
    else y1 := GetAbstractDate(ev1.Detail);

    if (ev2 = nil)
    then y2 := -1.0
    else y2 := GetAbstractDate(ev2.Detail);

    if (aCurEnd) then begin
      if (y2 <= 1.0)
      then y2 := YearOf(Now()) + MonthOf(Now()) / 12;
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

function GetLifeExpectancy(iRec: TGEDCOMIndividualRecord): string;
var
  i: Integer;
  event, ev1, ev2: TGEDCOMIndividualEvent;
begin
  try
    ev1 := nil;
    ev2 := nil;

    for i := 0 to iRec.IndividualEventsCount - 1 do begin
      event := iRec.IndividualEvents[i];

      if (event.Name = 'BIRT') then ev1 := event
      else
      if (event.Name = 'DEAT') then ev2 := event;
    end;

    Result := GetEventsYearsDiff(ev1, ev2, False);
  except
  end;
end;

function GetAge(iRec: TGEDCOMIndividualRecord): string;
var
  i: Integer;
  event, ev1, ev2: TGEDCOMIndividualEvent;
begin
  try
    ev1 := nil;
    ev2 := nil;

    for i := 0 to iRec.IndividualEventsCount - 1 do begin
      event := iRec.IndividualEvents[i];

      if (event.Name = 'BIRT') then ev1 := event
      else
      if (event.Name = 'DEAT') then ev2 := event;
    end;

    Result := GetEventsYearsDiff(ev1, ev2, True);
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

function GetChangeDate(aRec: TGEDCOMRecord): string;
begin
  try
    if (aRec.ChangeDate.ChangeDateTime <> 0)
    then DateTimeToString(Result, 'yyyy.mm.dd hh:nn:ss', aRec.ChangeDate.ChangeDateTime)
    else Result := '';
  except
    Result := '';
  end;
end;

function CreateEventEx(aTree: TGEDCOMTree; iRec: TGEDCOMIndividualRecord;
  evName: string): TGEDCOMCustomEvent;
begin
  Result := TGEDCOMIndividualEvent.Create(aTree, iRec);
  Result.Name := evName;
  iRec.AddIndividualEvent(TGEDCOMIndividualEvent(Result));
end;

procedure CreateIEvent(aTree: TGEDCOMTree; iRec: TGEDCOMIndividualRecord;
  evSign, evDate, evPlace: string);
var
  event: TGEDCOMIndividualEvent;
begin
  event := TGEDCOMIndividualEvent.Create(aTree, iRec);
  event.Name := evSign;

  event.Detail.Date.ParseString(evDate);
  event.Detail.Place.StringValue := evPlace;

  iRec.AddIndividualEvent(event);
end;

function CreatePersonEx(aTree: TGEDCOMTree; aName, aPatronymic, aFamily: string;
  aSex: TGEDCOMSex; aBirthEvent: Boolean = False): TGEDCOMIndividualRecord;
var
  pn: TGEDCOMPersonalName;
begin
  Result := TGEDCOMIndividualRecord.Create(aTree, aTree);
  Result.InitNew();
  Result.Sex := aSex;

  pn := TGEDCOMPersonalName.Create(aTree, Result);
  pn.StringValue := Trim(aName) + ' ' + Trim(aPatronymic) + ' /' + Trim(aFamily) + '/';
  Result.AddPersonalName(pn);

  Result.ChangeDate.ChangeDateTime := Now();

  aTree.AddRecord(Result);

  if (aBirthEvent)
  then CreateEventEx(aTree, Result, 'BIRT');
end;

procedure AddSpouseToFamily(aTree: TGEDCOMTree; aFamily: TGEDCOMFamilyRecord;
  aSpouse: TGEDCOMIndividualRecord);
var
  spLink: TGEDCOMSpouseToFamilyLink;
begin
  case aSpouse.Sex of
    svNone, svUndetermined: Exit;

    svMale: aFamily.Husband.Value := aSpouse;
    svFemale: aFamily.Wife.Value := aSpouse;
  end;

  spLink := TGEDCOMSpouseToFamilyLink.Create(aTree, aSpouse);
  spLink.Family := aFamily;
  aSpouse.AddSpouseToFamilyLink(spLink);

  {
  }
end;

procedure RemoveFamilySpouse(aTree: TGEDCOMTree; aFamily: TGEDCOMFamilyRecord;
  aSpouse: TGEDCOMIndividualRecord);
begin
  if (aSpouse <> nil) then begin
    aSpouse.DeleteSpouseToFamilyLink(aFamily);

    case aSpouse.Sex of
      svNone, svUndetermined: ;

      svMale: aFamily.Husband.Value := nil;
      svFemale: aFamily.Wife.Value := nil;
    end;
  end;
end;

function CreateNoteEx(aTree: TGEDCOMTree; aText: TStrings;
  aRecord: TGEDCOMRecord = nil): TGEDCOMNoteRecord;
var
  note: TGEDCOMNotes;
begin
  Result := TGEDCOMNoteRecord.Create(aTree, aTree);
  Result.InitNew();
  Result.Notes := aText;
  Result.ChangeDate.ChangeDateTime := Now();
  aTree.AddRecord(Result);

  if (aRecord <> nil) then begin
    note := TGEDCOMNotes.Create(aTree, aRecord);
    note.Value := Result;
    aRecord.AddNotes(note);
  end;
end;

function CreateFamilyEx(aTree: TGEDCOMTree): TGEDCOMFamilyRecord;
begin
  Result := TGEDCOMFamilyRecord.Create(aTree, aTree);
  Result.InitNew();
  Result.ChangeDate.ChangeDateTime := Now();
  aTree.AddRecord(Result);
end;

function IsMatchesMask(const aName, Mask: string): Boolean;
var
  i, tok_count: Integer;
  strx, strmask: string;
begin
  Result := False;

  strx := AnsiLowerCase(aName);
  strmask := AnsiLowerCase(Mask);

  tok_count := GetTokensCount(strmask, '|');
  for i := 1 to tok_count do
    Result := Result or MatchesMask(strx, GetToken(strmask, '|', i));
end;

function HyperLink(XRef: string; Text: string; Num: Integer = 0): string;
begin
  Result := '~^' + XRef + ':' + Text + '~';

  {if (Num <> 0)
  then Result := '№ ' + IntToStr(Num) + ' ' + Result;}
end;

function GetCorresponderStr(aTree: TGEDCOMTree; aRec: TGEDCOMCommunicationRecord;
  aLink: Boolean): string;
var
  dir: TCommunicationDir;
  corresponder: TGEDCOMIndividualRecord;
  nm: string;
begin
  Result := '';

  aRec.GetCorresponder(dir, corresponder);

  if (corresponder <> nil) then begin
    nm := GetNameStr(corresponder);
    if (aLink) then nm := HyperLink(corresponder.XRef, nm);

    Result := '[' + CommunicationDirs[dir] + '] ' + nm;
  end;
end;

procedure GetTaskGoal(aTree: TGEDCOMTree; aRec: TGEDCOMTaskRecord;
  var aType: TGoalType; var aGoalRec: TGEDCOMRecord);
begin
  aGoalRec := aTree.XRefIndex_Find(CleanXRef(aRec.Goal));

  if (aGoalRec is TGEDCOMIndividualRecord)
  then aType := gtIndividual
  else
  if (aGoalRec is TGEDCOMFamilyRecord)
  then aType := gtFamily
  else
  if (aGoalRec is TGEDCOMSourceRecord)
  then aType := gtSource
  else aType := gtOther;
end;

function GetTaskGoalStr(aTree: TGEDCOMTree; aRec: TGEDCOMTaskRecord): string;
var
  gt: TGoalType;
  tempRec: TGEDCOMRecord;
begin
  GetTaskGoal(aTree, aRec, gt, tempRec);

  case gt of
    gtIndividual: Result := GetNameStr((tempRec as TGEDCOMIndividualRecord));
    gtFamily: Result := GetFamilyStr((tempRec as TGEDCOMFamilyRecord));
    gtSource: Result := (tempRec as TGEDCOMSourceRecord).FiledByEntry;
    gtOther: Result := aRec.Goal;
  end;

  if (gt <> gtOther)
  then Result := '[' + GoalNames[gt] + '] ' + Result;
end;

type
  TRetCount = packed record
    lngSubRows: Word;
    lngCountLike: Word;
  end;

function Matching(StrA, StrB: String; lngLen: Integer): TRetCount;
var
  PosStrA, PosStrB: Integer;
  StrTempA, StrTempB: String;
begin
  Result.lngSubRows := 0;
  Result.lngCountLike := 0;

  for PosStrA := 1 to Length(strA) - lngLen + 1 do begin
    StrTempA := Copy(strA, PosStrA, lngLen);

    for PosStrB := 1 to Length(strB) - lngLen + 1 do begin
      StrTempB := Copy(strB, PosStrB, lngLen);
      if AnsiCompareText(StrTempA, StrTempB) = 0 then begin
        Inc(Result.lngCountLike);
        Break;
      end;
    end;

    Inc(Result.lngSubRows);
  end;
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
  Result := 0;

  //если не передан какой-либо параметр, то выход
  if (MaxMatching = 0) or (Length(strInputMatching) = 0) or (Length(strInputStandart) = 0)
  then Exit;

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

  if (gret.lngSubRows <> 0)
  then Result := Trunc((gret.lngCountLike / gret.lngSubRows) * 100);
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

function GetRecordType(rec: TGEDCOMRecord): TGEDCOMRecordType;
begin
  if (rec is TGEDCOMIndividualRecord) then Result := rtIndividual
  else
  if (rec is TGEDCOMFamilyRecord) then Result := rtFamily
  else
  if (rec is TGEDCOMNoteRecord) then Result := rtNote
  else
  if (rec is TGEDCOMMultimediaRecord) then Result := rtMultimedia
  else
  if (rec is TGEDCOMSourceRecord) then Result := rtSource
  else
  if (rec is TGEDCOMRepositoryRecord) then Result := rtRepository
  else
  if (rec is TGEDCOMGroupRecord) then Result := rtGroup
  else
  if (rec is TGEDCOMResearchRecord) then Result := rtResearch
  else
  if (rec is TGEDCOMTaskRecord) then Result := rtTask
  else
  if (rec is TGEDCOMCommunicationRecord) then Result := rtCommunication
  else
  if (rec is TGEDCOMLocationRecord) then Result := rtLocation
  else Result := rtNone;
end;

function RecordIsType(aRecType: TGEDCOMRecordType; aRec: TGEDCOMRecord): Boolean;
begin
  case aRecType of
    rtIndividual: Result := aRec is TGEDCOMIndividualRecord;
    rtFamily: Result := aRec is TGEDCOMFamilyRecord;
    rtNote: Result := aRec is TGEDCOMNoteRecord;
    rtMultimedia: Result := aRec is TGEDCOMMultimediaRecord;
    rtSource: Result := aRec is TGEDCOMSourceRecord;
    rtRepository: Result := aRec is TGEDCOMRepositoryRecord;
    rtGroup: Result := aRec is TGEDCOMGroupRecord;
    rtResearch: Result := aRec is TGEDCOMResearchRecord;
    rtTask: Result := aRec is TGEDCOMTaskRecord;
    rtCommunication: Result := aRec is TGEDCOMCommunicationRecord;
    rtLocation: Result := aRec is TGEDCOMLocationRecord;
    else Result := False;
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

    for i := 0 to aTree.RecordsCount - 1 do begin
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

procedure CheckRecord(aTree: TGEDCOMTree; aRec: TGEDCOMRecord);

  procedure ReformNote(note: TGEDCOMNotes);
  var
    strData: TStringList;
    noteRec: TGEDCOMNoteRecord;
  begin
    strData := TStringList.Create;
    try
      strData.Text := note.Notes.Text;

      noteRec := CreateNoteEx(aTree, strData);

      note.Clear;
      note.Value := noteRec;
    finally
      strData.Free;
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
      mmRec.InitNew();
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

  {fixme}
  procedure ReformSourceCitation(sourCit: TGEDCOMSourceCitation);
  begin
  end;

  procedure PrepareTag(tag: TGEDCOMTagWithLists);
  var
    i: Integer;
    mmLink: TGEDCOMMultimediaLink;
    note: TGEDCOMNotes;
    sourCit: TGEDCOMSourceCitation;
  begin
    for i := 0 to tag.MultimediaLinksCount - 1 do begin
      mmLink := tag.MultimediaLinks[i];
      if not(mmLink.IsPointer) then ReformMultimediaLink(mmLink);
    end;

    for i := 0 to tag.NotesCount - 1 do begin
      note := tag.Notes[i];
      if not(note.IsPointer) then ReformNote(note);
    end;

    for i := 0 to tag.SourceCitationsCount - 1 do begin
      sourCit := tag.SourceCitations[i];
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

  {procedure CheckAttributes(ind: TGEDCOMIndividualRecord);
  var
    i: Integer;
    attr: TGEDCOMIndividualAttribute;
  begin
    for i := 0 to ind.IndividualAttributesCount - 1 do begin
      attr := ind.IndividualAttributes[i];

      if (attr.StringValue = '') and (attr.Detail.Place <> '') then begin
        attr.StringValue := attr.Detail.Place;
        attr.Detail.Place := '';
      end;
    end;
  end;}

  {procedure CheckName(ind: TGEDCOMIndividualRecord);
  var
    fam, nam, pat, np: string;
    tag: TGEDCOMTag;
    pn: TGEDCOMPersonalName;
  begin
    GetNameParts(ind, fam, nam, pat);

    pn := ind.PersonalNames[0];

    if (nam = '')
    then np := pat
    else
    if (pat = '')
    then np := nam
    else np := nam + ' ' + pat;

    tag := pn.FindTag('GIVN');
    if (tag <> nil) and (tag.StringValue = np) then pn.DeleteTag('GIVN');

    tag := pn.FindTag('SURN');
    if (tag <> nil) and (tag.StringValue = fam) then pn.DeleteTag('SURN');
  end;}

  {procedure CheckPlace(ind: TGEDCOMIndividualRecord);
  var
    pn: TGEDCOMPersonalName;
    ev: TGEDCOMIndividualEvent;
  begin
    pn := ind.PersonalNames[0];

    if (pn.Surname <> '') and (pn.Surname[1] in ['Ж']) then begin
      ev := GetIndividualEvent(ind, 'BIRT');
      if (ev.Detail.Place = '')
      then ev.Detail.Place := 'Екатеринбургский уезд, пос. Верхний Тагил';
    end;
  end;}

  procedure CheckPerson(ind: TGEDCOMIndividualRecord);
  var
    k: Integer;
    evt: TGEDCOMCustomEvent;
    loc: TGEDCOMLocationRecord;
  begin
    {CheckAttributes(ind);}
    {CheckName(ind);}
    {CheckPlace(ind);}

    /// Не указаны родители и супруги - нормально

    /// Пустые ссылки на семью родителей - зачищаем
    for k := ind.ChildToFamilyLinksCount - 1 downto 0 do begin
      if (ind.ChildToFamilyLinks[k].Family = nil)
      then ind.DeleteChildToFamilyLink(k);
    end;

    /// Пустые ссылки на брак - зачищаем
    for k := ind.SpouseToFamilyLinksCount - 1 downto 0 do begin
      if (ind.SpouseToFamilyLinks[k].Family = nil)
      then ind.DeleteSpouseToFamilyLink(k);
    end;

    for k := 0 to ind.IndividualEventsCount - 1 do begin
      evt := ind.IndividualEvents[k];

      if (evt.Detail.Place.StringValue <> '') then begin
        loc := TGEDCOMLocationRecord(evt.Detail.Place.Location.Value);

        if (loc <> nil) and (evt.Detail.Place.StringValue <> loc.Name)
        then evt.Detail.Place.StringValue := loc.Name;
      end;
    end;
  end;

  procedure CheckFamily(fam: TGEDCOMFamilyRecord);
  var
    k: Integer;
  begin
    /// Пустые семьи (без супругов, детей и событий) - не трогаем
    /// Пустые указатели супругов не трогаем

    /// Пустые указатели детей - зачищаем
    for k := fam.ChildrenCount - 1 downto 0 do begin
      if (fam.Children[k].Value = nil)
      then fam.DeleteChild(k);
    end;

    /// Сортируем детей по возрасту (для древ и росписей)
    fam.SortChilds();
  end;

  procedure CheckGroup(group: TGEDCOMGroupRecord);
  var
    k: Integer;
    ptr: TGEDCOMPointer;
    irec: TGEDCOMIndividualRecord;
  begin
    /// Проверка на зависшие указатели на членов групп
    for k := group.MembersCount - 1 downto 0 do begin
      ptr := group.Members[k];
      irec := TGEDCOMIndividualRecord(ptr.Value);

      if (irec = nil)
      then group.DeleteMember(k)
      else begin
        if (irec.IndexOfGroup(group) < 0)
        then group.DeleteMember(k);
      end;
    end;
  end;

var
  rwl: TGEDCOMRecord;
  i: Integer;
  mmLink: TGEDCOMMultimediaLink;
  note: TGEDCOMNotes;
  sourCit: TGEDCOMSourceCitation;
  fam: TGEDCOMFamilyRecord;
  ind: TGEDCOMIndividualRecord;
begin
  if (aRec is TGEDCOMRecord) then begin
    rwl := aRec as TGEDCOMRecord;

    /// Общие для всех записей свойства

    if (rwl.UID = '') then rwl.NewUID();

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

    /// Раздельно по подвидам записей

    if (rwl is TGEDCOMFamilyRecord) then begin
      fam := rwl as TGEDCOMFamilyRecord;

      for i := 0 to fam.FamilyEventCount - 1 do PrepareTag(fam.FamilyEvents[i].Detail);

      CheckFamily(fam);
    end
    else
    if (rwl is TGEDCOMIndividualRecord) then begin
      ind := rwl as TGEDCOMIndividualRecord;

      for i := 0 to ind.IndividualAttributesCount - 1 do PrepareTag(ind.IndividualAttributes[i].Detail);
      for i := 0 to ind.IndividualEventsCount - 1 do PrepareTag(ind.IndividualEvents[i].Detail);

      for i := 0 to ind.ChildToFamilyLinksCount - 1 do PreparePtr(ind.ChildToFamilyLinks[i]);
      for i := 0 to ind.SpouseToFamilyLinksCount - 1 do PreparePtr(ind.SpouseToFamilyLinks[i]);
      for i := 0 to ind.AssociationsCount - 1 do PreparePtr(ind.Associations[i]);

      CheckPerson(ind);
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
      CheckGroup(rwl as TGEDCOMGroupRecord);
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
    ProgressInit(aTree.RecordsCount, 'Коррекция идентификаторов');

    repMap := TXRefReplaceMap.Create;
    try
      for i := 0 to aTree.RecordsCount - 1 do begin
        rec := aTree.Records[i];

        if (GetId(rec) < 0) then begin
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
      repMap.Free;

      ProgressDone();
    end;
  end;

var
  i, k: Integer;
  rec: TGEDCOMRecord;
  idCheck: Boolean;
begin
  Result := False;

  for i := 0 to aTree.RecordsCount - 1 do begin
    for k := i + 1 to aTree.RecordsCount - 1 do begin
      if (aTree.Records[i].XRef = aTree.Records[k].XRef)
      then ; // AddDiag(aTree.Records[i].XRef, 'Объект дублирован');
    end;
  end;

  ProgressInit(aTree.RecordsCount, 'Проверка формата');
  try
    idCheck := True;
    i := 0;
    while (i < aTree.RecordsCount) do begin
      rec := aTree.Records[i];
      CheckRecord(aTree, rec);

      if (idCheck) and (GetId(rec) < 0)
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

  Result := True;
end;

procedure LoadExtFile(const aFileName: string);
begin
  {$IFNDEF DELPHI_NET}
  ShellExecute(0, 'open', PChar(aFileName), nil, nil, SW_SHOW);
  {$ELSE}
  ShellExecute(0, 'open', (aFileName), '', '', SW_SHOW);
  {$ENDIF}
end;

// замена данных в потоке с кодировки 1251 на UTF-8
function StreamToUtf8Stream(Stream: TStream): UTF8String;
var
  s: string;
begin
  SetLength(s, Stream.Size);
  Stream.Seek(0, soFromBeginning);

  {$IFNDEF DELPHI_NET}
  Stream.Read(s[1], Stream.Size);
  {$ELSE}
  s := SReadString(Stream);
  {$ENDIF}

  Result := AnsiToUtf8(s);
  Stream.Size := 0;

  {$IFNDEF DELPHI_NET}
  Stream.Write(Result[1], Length(Result));
  {$ELSE}
  SWriteString(Stream, Result);
  {$ENDIF}
end;

function ConStrings(aStrings: TStrings): string;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to aStrings.Count - 1 do begin
    if (Result <> '') then Result := Result + ' ';
    Result := Result + Trim(aStrings[i]);
  end;
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
  dummy, ch_pat, fat_nam: string;
begin
  for i := 0 to aTree.RecordsCount - 1 do
    if (aTree.Records[i] is TGEDCOMIndividualRecord) then begin
      iRec := aTree.Records[i] as TGEDCOMIndividualRecord;

      GetNameParts(iRec, dummy, dummy, ch_pat);

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

procedure TChartOptions.LoadFromFile(const aIniFile: TIniFile);
begin
  FFamilyVisible := aIniFile.ReadBool('Chart', 'FamilyVisible', True);
  FNameVisible := aIniFile.ReadBool('Chart', 'NameVisible', True);
  FPatronymicVisible := aIniFile.ReadBool('Chart', 'PatronymicVisible', True);
  FDiffLines := aIniFile.ReadBool('Chart', 'DiffLines', False);
  FBirthDateVisible := aIniFile.ReadBool('Chart', 'BirthDateVisible', False);
  FDeathDateVisible := aIniFile.ReadBool('Chart', 'DeathDateVisible', False);
  FKinship := aIniFile.ReadBool('Chart', 'Kinship', False);
  FMaleColor := aIniFile.ReadInteger('Chart', 'MaleColor', $00FFC6C6);
  FFemaleColor := aIniFile.ReadInteger('Chart', 'FemaleColor', $00C6C6FF);
  FUnkSexColor := aIniFile.ReadInteger('Chart', 'UnkSexColor', $00FFC6FF);
  FUnHusbandColor := aIniFile.ReadInteger('Chart', 'UnHusbandColor', $00FFD7D7);
  FUnWifeColor := aIniFile.ReadInteger('Chart', 'UnWifeColor', $00D7D7FF);
end;

procedure TChartOptions.SaveToFile(const aIniFile: TIniFile);
begin
  aIniFile.WriteBool('Chart', 'FamilyVisible', FFamilyVisible);
  aIniFile.WriteBool('Chart', 'NameVisible', FNameVisible);
  aIniFile.WriteBool('Chart', 'PatronymicVisible', FPatronymicVisible);
  aIniFile.WriteBool('Chart', 'DiffLines', FDiffLines);
  aIniFile.WriteBool('Chart', 'BirthDateVisible', FBirthDateVisible);
  aIniFile.WriteBool('Chart', 'DeathDateVisible', FDeathDateVisible);
  aIniFile.WriteBool('Chart', 'Kinship', FKinship);
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

    FGEDCOMOptimize := ini.ReadBool('Common', 'GEDCOMOptimize', False);

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

    for i := 0 to High(FListPersonsColumns) do begin
      FListPersonsColumns[i].colType := TPersonColumnType(ini.ReadInteger('PersonsColumns', 'ColType_' + IntToStr(i), Ord(DefPersonColumns[i].colType)));
      FListPersonsColumns[i].colActive := ini.ReadBool('PersonsColumns', 'ColActive_' + IntToStr(i), DefPersonColumns[i].colActive);
    end;
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

    ini.WriteBool('Common', 'GEDCOMOptimize', FGEDCOMOptimize);

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

    for i := 0 to High(FListPersonsColumns) do begin
      ini.WriteInteger('PersonsColumns', 'ColType_' + IntToStr(i), Ord(FListPersonsColumns[i].colType));
      ini.WriteBool('PersonsColumns', 'ColActive_' + IntToStr(i), FListPersonsColumns[i].colActive);
    end;
  finally
    ini.Destroy;
  end;
end;

{==============================================================================}

{ TTextFileEx }

constructor TTextFileEx.Create(aStream: TStream);
begin
  inherited Create;
  FStream := aStream;
end;

function TTextFileEx.Eof(): Boolean;
begin
  Result := (FStream.Position >= FStream.Size);
end;

function TTextFileEx.ReadLn(): string;
var
  c: Char;
begin
  Result := '';

  while not Eof do begin
    FStream.Read(c, SizeOf(c));

    if (c = #13) then begin
      FStream.Read(c, SizeOf(c));

      if (c <> #10)
      then FStream.Seek(SizeOf(c), soFromCurrent);

      Break;
    end;

    if (c = #10)
    then Break;

    Result := Result + c;
  end;
end;

procedure TTextFileEx.WriteLn(const s: string);
var
  i: Integer;
begin
  for i := 1 to Length(s) do
    Write(s[i], SizeOf(s[i]));
  Write(LineEnd, Length(LineEnd));
end;

end.
