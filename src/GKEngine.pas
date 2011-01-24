unit GKEngine;

{$I GEDKeeper.inc}

interface

uses
  Classes, Contnrs, GedCom551;

{$IFNDEF DELPHI_NET}
type
  IntPtr = type Integer;
{$ENDIF}

resourcestring
  AppName = 'GEDKeeper';

const
  UnkFemale = 'неизвестная';
  UnkMale = 'неизвестный';
  MLinkPrefix = 'view_';

const
  AdvTag = '_ADVANCED';
  ExtTag = '_EXT_NAME';
  PatriarchTag = '_PATRIARCH';
  BookmarkTag = '_BOOKMARK';

type
  TGEDCOMRecordType = (
    rtNone,
    rtIndividual, rtFamily, rtNote, rtMultimedia, rtSource,
    rtRepository, rtGroup, rtResearch, rtTask, rtCommunication, rtLocation,
    rtSubmission, rtSubmitter);

type
  TRecAction = (raAdd, raEdit, raDelete, raJump, raMoveUp, raMoveDown);

  TTargetMode = (tmNone, tmAncestor, tmDescendant, tmChildToFamily);
  TLifeMode = (lmAll, lmOnlyAlive, lmOnlyDead, lmAliveBefore, lmTimeLine);

  TFamilyTarget = (ftNone, ftSpouse, ftChild);

  TDateControlsRange = set of 1..2;

  TShieldState = (
    // не показываются ни секретные, ни конфиденциальные данные
    ssMaximum,
    // показывается всё кроме секретного
    ssMiddle,
    // показывается всё
    ssNone);

  TDateFormat = (dfDD_MM_YYYY, dfYYYY_MM_DD, dfYYYY);
  TNameFormat = (nfFNP, nfF_NP, nfF_N_P);

  TPersonEventKind = (ekEvent, ekFact);

const
  SexData: array [TGEDCOMSex] of record
    ViewName: string;
    ViewSign: string;
    LatSign: string;
  end = (
    (ViewName: '?';                 ViewSign: '?'; LatSign: 'N'),
    (ViewName: 'мужской';           ViewSign: 'М'; LatSign: 'M'),
    (ViewName: 'женский';           ViewSign: 'Ж'; LatSign: 'F'),
    (ViewName: 'неопределенный';    ViewSign: 'Н'; LatSign: 'U')
  );

const
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
    Kind: TPersonEventKind;
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
_CENN       BKW6                Census name
_CORR       FO7                 Correspondence entry
_EMAIL      BKW6, FO9, PAF      The email address of the individual
_MARN       BKW6                Married name
_MILT       FTM7                Military Services
_PRIM       FO7                 In the OBJE record to indicate if this is the primary photo for this person.
_RELN       BKW6                Religious name
_SCBK       AQ3, FO7, PAF5      In the OBJE record to indicate if the multimedia object should be in the scrapbook.
_SSHOW      AQ3, PAF5           indicates if image is included in slideshow
_TODO       FO7                 To-do item
HOBB        Gen, Reunion        Hobbies
INFO        FTW5                Information
MILA        Gen                 Military Award
MILD        Gen                 Military Discharge
MILF        Reunion, Gen        Served in Military
MILI        Reunion             Military
MILT        Gen                 Military Services
NAMR        FTW5, Gen, Reunion  Religious Name
RACE        FTW, Gen, Reunion   Race
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

type
  //TUserRefType = (urtCustom, urtPredef);

  TUserRef = (
    urCustom,
    urRI_StGeorgeCross,
    urUSSR_Soldier, urUSSR_FallInBattle, urUSSR_RearVeteran
  );

  TChartPersonSign = urRI_StGeorgeCross..urUSSR_RearVeteran;
  TChartPersonSigns = set of TChartPersonSign;

const
  //UserRefTypes: array [TUserRefType] of string = ('Пользовательский', 'Предопределенный');

  UserRefs: array [TUserRef] of record
    Name: string;
  end = (
    (Name: ''),
    (Name: 'РИ:Георгиевский кавалер'),
    (Name: 'СССР:ВОВ:Участник боевых действий'),
    (Name: 'СССР:ВОВ:Погиб в бою'),
    (Name: 'СССР:ВОВ:Труженик тыла')
  );

function GetSexBySign(const SexSign: Char): TGEDCOMSex;

function IsDevComp(): Boolean;

function GetRecordType(rec: TGEDCOMRecord): TGEDCOMRecordType;
function RecordIsType(aRecType: TGEDCOMRecordType; aRec: TGEDCOMRecord): Boolean;

function IsRecordAccess(aRecRestriction: TGEDCOMRestriction; aShieldState: TShieldState): Boolean;

function GetPersonEventKindBySign(aSign: string): TPersonEventKind;

function GetPersonEventIndex(aSign: string): Integer;
function GetFamilyEventIndex(aSign: string): Integer;

function GetMarriageStatusIndex(aSign: string): Integer;

function GetEventName(aEvent: TGEDCOMCustomEvent): string;

procedure GetNameParts(iRec: TGEDCOMIndividualRecord; var aFamily, aName, aPatronymic: string);

function GetNameStr(iRec: TGEDCOMIndividualRecord; aByFamily: Boolean = True;
  aPieces: Boolean = False): string;
function GetNickStr(iRec: TGEDCOMIndividualRecord): string;
function GetFamilyStr(aFamily: TGEDCOMFamilyRecord): string;

function GetSex(f_name, f_pat: string; aQuery: Boolean = True): TGEDCOMSex;

function GetXRefNum(aRecord: TGEDCOMRecord): string;
function GetId(aRecord: TGEDCOMRecord): Integer;

function GEDCOMDateToStr(aDate: TGEDCOMDate; aFormat: TDateFormat = dfDD_MM_YYYY): string;
function StrToGEDCOMDate(aDate: string; aException: Boolean = True): string;
function GEDCOMCustomDateToStr(aDate: TGEDCOMCustomDate; aFormat: TDateFormat;
  aSign: Boolean = False): string;

function GEDCOMEventToDateStr(aEvent: TGEDCOMCustomEvent; aFormat: TDateFormat;
  aSign: Boolean = False): string;

function GEDCOMDateToDate(aDate: TGEDCOMCustomDate): TDateTime;

function GetIndividualEvent(iRec: TGEDCOMIndividualRecord; evName: string): TGEDCOMCustomEvent;
function GetFamilyEvent(fRec: TGEDCOMFamilyRecord; evName: string): TGEDCOMFamilyEvent;

function GetBirthDate(iRec: TGEDCOMIndividualRecord; aFormat: TDateFormat;
  aCompact: Boolean = False): string;
function GetDeathDate(iRec: TGEDCOMIndividualRecord; aFormat: TDateFormat;
  aCompact: Boolean = False): string;
  
function GetLifeStr(iRec: TGEDCOMIndividualRecord): string;

function GetBirthPlace(iRec: TGEDCOMIndividualRecord): string;
function GetDeathPlace(iRec: TGEDCOMIndividualRecord): string;

function GetResidencePlace(iRec: TGEDCOMIndividualRecord; IncludeAddress: Boolean): string;
function GetPlaceStr(aEvent: TGEDCOMCustomEvent; IncludeAddress: Boolean): string;

function GetAttributeValue(iRec: TGEDCOMIndividualRecord; attrName: string): string;
function GetAttributeStr(iAttr: TGEDCOMIndividualAttribute): string;

function GetMarriageDate(fRec: TGEDCOMFamilyRecord; aFormat: TDateFormat): string;
function GetEventDesc(evDetail: TGEDCOMEventDetail): string;
function GetEventCause(evDetail: TGEDCOMEventDetail): string;

function IsLive(iRec: TGEDCOMIndividualRecord): Boolean;
procedure GetLifeDates(iRec: TGEDCOMIndividualRecord; var aBirthEvent, aDeathEvent: TGEDCOMCustomEvent);
procedure GetIndependentDate(aDate: TGEDCOMCustomDate; var AYear: Integer; var AMonth, ADay: Word);
function GetEventsYearsDiff(ev1, ev2: TGEDCOMCustomEvent; aCurEnd: Boolean = False): string;
function GetLifeExpectancy(iRec: TGEDCOMIndividualRecord): string;
function GetAge(iRec: TGEDCOMIndividualRecord; ToYear: Integer = -1): string;
function GetFirstbornAge(iRec: TGEDCOMIndividualRecord): string;
function GetMarriageAge(iRec: TGEDCOMIndividualRecord): string;
function GetDaysForBirth(iRec: TGEDCOMIndividualRecord): string;

function GetChangeDate(aRec: TGEDCOMRecord): string;

function CreateEventEx(aTree: TGEDCOMTree; aRec: TGEDCOMRecord;
  evSign: string; evDate: string = ''; evPlace: string = ''): TGEDCOMCustomEvent;

function CreatePersonEx(aTree: TGEDCOMTree; aName, aPatronymic, aFamily: string;
  aSex: TGEDCOMSex; aBirthEvent: Boolean = False): TGEDCOMIndividualRecord;

function CreateFamilyEx(aTree: TGEDCOMTree): TGEDCOMFamilyRecord;

procedure SetAddressValue(anAddress: TGEDCOMAddress; aValue: string);

procedure AddSpouseToFamily(aTree: TGEDCOMTree; aFamily: TGEDCOMFamilyRecord;
  aSpouse: TGEDCOMIndividualRecord);
procedure RemoveFamilySpouse(aTree: TGEDCOMTree; aFamily: TGEDCOMFamilyRecord;
  aSpouse: TGEDCOMIndividualRecord);

function FamilyChildAdd(aTree: TGEDCOMTree; aFamily: TGEDCOMFamilyRecord;
  aChild: TGEDCOMIndividualRecord): Boolean;
function FamilyChildRemove(aTree: TGEDCOMTree; aFamily: TGEDCOMFamilyRecord;
  aChild: TGEDCOMIndividualRecord): Boolean;

function CreateNote(aTree: TGEDCOMTree): TGEDCOMNoteRecord;
procedure BindRecordNote(aTree: TGEDCOMTree; aRecord: TGEDCOMRecord;
  aNoteRec: TGEDCOMNoteRecord);
procedure AddNoteText(aNoteRec: TGEDCOMNoteRecord; aText: string);
function CreateNoteEx(aTree: TGEDCOMTree; aText: TStrings;
  aRecord: TGEDCOMRecord = nil): TGEDCOMNoteRecord;

function CreateSource(aTree: TGEDCOMTree): TGEDCOMSourceRecord;
  
procedure BindRecordSource(aTree: TGEDCOMTree; aRecord: TGEDCOMRecord;
  aSrcRec: TGEDCOMSourceRecord; aPage: string; aQuality: Integer);

procedure BindSourceRepository(aTree: TGEDCOMTree; aSourceRecord: TGEDCOMSourceRecord;
  aRepRec: TGEDCOMRepositoryRecord);

procedure GetLocationLinks(aTree: TGEDCOMTree; aLocation: TGEDCOMLocationRecord;
  var aList: TStringList);
  
function IsMatchesMask(const aName, aMask: string): Boolean;

function HyperLink(XRef: string; Text: string; Num: Integer = 0): string;
function GenRecordLink(aTree: TGEDCOMTree; aRecord: TGEDCOMRecord;
  aSigned: Boolean = True): string;

function GetCorresponderStr(aTree: TGEDCOMTree; aRec: TGEDCOMCommunicationRecord;
  aLink: Boolean): string;

procedure GetTaskGoal(aTree: TGEDCOMTree; aRec: TGEDCOMTaskRecord;
  var aType: TGoalType; var aGoalRec: TGEDCOMRecord);
function GetTaskGoalStr(aTree: TGEDCOMTree; aRec: TGEDCOMTaskRecord): string;

function IndistinctMatching(MaxMatching: Integer; strInputMatching, strInputStandart: String): Integer;

function ClearFamily(aFamily: string): string;

function PrepareRusFamily(f: string; aFemale: Boolean): string;

function GetChildsCount(aPerson: TGEDCOMIndividualRecord): Integer;
function GetAncestorsCount(aBuffer: TStringList; aPerson: TGEDCOMIndividualRecord): Integer;
function GetDescendantsCount(aBuffer: TStringList; aPerson: TGEDCOMIndividualRecord): Integer;
function GetDescGenerations(aPerson: TGEDCOMIndividualRecord): Integer;
function GetMarriagesCount(aPerson: TGEDCOMIndividualRecord): Integer;
function GetSpousesDiff(fRec: TGEDCOMFamilyRecord): string;

type
  TPatriarchObj = class(TObject)
  public
    IRec: TGEDCOMIndividualRecord;
    IBirthYear,
    IDescendantsCount,
    IDescGenerations: Integer;
    ILinks: set of Byte;
  end;

procedure GetPatriarchsList(aTree: TGEDCOMTree; aProgress, aLinks: Boolean;
  var aList: TObjectList; aMinGens: Integer = 2);

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

function CheckGEDCOMFormat(aTree: TGEDCOMTree): Boolean;

implementation

uses
  {$IFDEF DELPHI_NET}System.IO,{$ENDIF}
  Windows, SysUtils, DateUtils, Controls, Masks, bsComUtils, bsWinUtils, Dialogs,
  GKProgress;

function GetSexBySign(const SexSign: Char): TGEDCOMSex;
begin
  case SexSign of
    'N': Result := svNone;
    'M': Result := svMale;
    'F': Result := svFemale;
    'U': Result := svUndetermined;
    else Result := svNone;
  end;
end;

function IsDevComp(): Boolean;
begin
  Result := (GetComputerName() = 'VALHALLA') or (GetUserName() = 'Zhdanovskih_SV');
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

function IsRecordAccess(aRecRestriction: TGEDCOMRestriction; aShieldState: TShieldState): Boolean;
begin
  Result := False;

  case aShieldState of
    ssMaximum: Result := not(aRecRestriction in [rnConfidential, rnPrivacy]);

    ssMiddle: Result := not(aRecRestriction in [rnPrivacy]);

    ssNone: Result := True;
  end;
end;

function GetPersonEventKindBySign(aSign: string): TPersonEventKind;
var
  i: Integer;
begin
  Result := ekFact;

  for i := 0 to PersonEventsSize - 1 do
    if (PersonEvents[i].Sign = aSign) then begin
      Result := PersonEvents[i].Kind;
      Break;
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
  if (iRec <> nil) then begin
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

    if (aByFamily)
    then Result := np.Surname + ' ' + np.FirstPart
    else Result := np.FirstPart + ' ' + np.Surname;

    if (aPieces) then begin
      nick := np.Pieces.Nickname;
      if (nick <> '') then Result := Result + ' [' + nick + ']';
    end;
  end else Result := '';
end;

function GetNickStr(iRec: TGEDCOMIndividualRecord): string;
var
  np: TGEDCOMPersonalName;
begin
  if (iRec <> nil) then begin
    np := iRec.PersonalNames[0];
    Result := np.Pieces.Nickname;
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

function GetSex(f_name, f_pat: string; aQuery: Boolean = True): TGEDCOMSex;
begin
  Result := svNone;

  case f_name[Length(f_name)] of
    'а', 'я':
      if (Length(f_pat) > 1) then begin
        if (f_pat[Length(f_pat)] in ['а', 'я'])
        then Result := svFemale
        else
        if (f_pat[Length(f_pat)] in ['в', 'г', 'д', 'й', 'л', 'м', 'н', 'о', 'п', 'р'])
        then Result := svMale;
      end;

    'в', 'г', 'д', 'й', 'л', 'м', 'н', 'о', 'п', 'р':
      Result := svMale;
  end;

  if (aQuery) and (Result = svNone) then begin
    if (MessageDlg('Не определяется пол человека по имени "'+f_name+' '+f_pat+'". Это мужской пол?', mtConfirmation, [mbYes, mbNo], 0) = mrYes)
    then Result := svMale
    else Result := svFemale;
  end;
end;

function GetXRefNum(aRecord: TGEDCOMRecord): string;
var
  xref, sign: string;
begin
  xref := aRecord.XRef;
  sign := GetSignByRecord(aRecord);
  Delete(xref, 1, Length(sign));
  Result := xref;
end;

function GetId(aRecord: TGEDCOMRecord): Integer;
var
  xref, sign: string;
begin
  try
    xref := aRecord.XRef;
    sign := GetSignByRecord(aRecord);
    Delete(xref, 1, Length(sign));

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

    dfYYYY: begin
      if (year > 0)
      then Result := IntToStr(year);
    end;
  end;
end;

function StrToGEDCOMDate(aDate: string; aException: Boolean = True): string;
var
  cnt: Integer;
  pd, pm, py: string;
begin
  Result := '';

  if (Pos('/', aDate) > 0)
  then aDate := StringReplace(aDate, '/', '.', [rfReplaceAll]);

  cnt := GetTokensCount(aDate, '.');
  if (cnt < 3) then begin
    if (aException)
    then raise Exception.Create('date failed')
    else Exit;
  end;

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
  if (aDate = nil)
  then Result := ''
  else begin
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
end;

function GEDCOMEventToDateStr(aEvent: TGEDCOMCustomEvent; aFormat: TDateFormat;
  aSign: Boolean = False): string;
begin
  if (aEvent = nil)
  then Result := ''
  else Result := GEDCOMCustomDateToStr(aEvent.Detail.Date.Value, aFormat, aSign);
end;

function GEDCOMDateToDate(aDate: TGEDCOMCustomDate): TDateTime;
var
  year: Integer;
  month, day: Word;
begin
  try
    GetIndependentDate(aDate, year, month, day);

    if (day = 0) then day := 1;
    if (month = 0) then month := 1;

    if (year <= 0)
    then Result := 0
    else Result := EncodeDate(year, month, day);
  except
    Result := 0;
  end;
end;

function GetIndividualEvent(iRec: TGEDCOMIndividualRecord; evName: string): TGEDCOMCustomEvent;
var
  i: Integer;
  event: TGEDCOMCustomEvent;
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

function CompactDate(const aDate: string): string;
begin
  Result := aDate;
  while (Pos('__.', Result) = 1) do Delete(Result, 1, 3);
end;

function GetBirthDate(iRec: TGEDCOMIndividualRecord; aFormat: TDateFormat;
  aCompact: Boolean = False): string;
var
  event: TGEDCOMCustomEvent;
begin
  event := GetIndividualEvent(iRec, 'BIRT');

  if (event = nil)
  then Result := ''
  else Result := GEDCOMCustomDateToStr(event.Detail.Date.Value, aFormat);

  if (aCompact)
  then Result := CompactDate(Result);
end;

function GetDeathDate(iRec: TGEDCOMIndividualRecord; aFormat: TDateFormat;
  aCompact: Boolean = False): string;
var
  event: TGEDCOMCustomEvent;
begin
  event := GetIndividualEvent(iRec, 'DEAT');

  if (event = nil)
  then Result := ''
  else Result := GEDCOMCustomDateToStr(event.Detail.Date.Value, aFormat);

  if (aCompact)
  then Result := CompactDate(Result);
end;

function GetLifeStr(iRec: TGEDCOMIndividualRecord): string;
var
  ds: string;
  ev: TGEDCOMCustomEvent;
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
  event: TGEDCOMCustomEvent;
begin
  event := GetIndividualEvent(iRec, 'BIRT');

  if (event = nil)
  then Result := ''
  else Result := event.Detail.Place.StringValue;
end;

function GetDeathPlace(iRec: TGEDCOMIndividualRecord): string;
var
  event: TGEDCOMCustomEvent;
begin
  event := GetIndividualEvent(iRec, 'DEAT');

  if (event = nil)
  then Result := ''
  else Result := event.Detail.Place.StringValue;
end;

function GetResidencePlace(iRec: TGEDCOMIndividualRecord; IncludeAddress: Boolean): string;
begin
  Result := GetPlaceStr(GetIndividualEvent(iRec, 'RESI'), IncludeAddress);
end;

function GetPlaceStr(aEvent: TGEDCOMCustomEvent; IncludeAddress: Boolean): string;
var
  resi, addr: string;
begin
  if (aEvent = nil)
  then Result := ''
  else begin
    Result := aEvent.Detail.Place.StringValue;

    if (IncludeAddress) then begin
      resi := aEvent.StringValue;
      addr := Trim(aEvent.Detail.Address.Address.Text);
      if (resi <> '') and (addr <> '') then resi := resi + ', ';
      resi := resi + addr;

      if (resi <> '')
      then Result := Result + ' [' + resi + ']';
    end;
  end;
end;

function GetAttributeValue(iRec: TGEDCOMIndividualRecord; attrName: string): string;
var
  attr: TGEDCOMCustomEvent;
begin
  attr := GetIndividualEvent(iRec, attrName);
  if (attr = nil)
  then Result := ''
  else Result := attr.StringValue;
end;

function GetAttributeStr(iAttr: TGEDCOMIndividualAttribute): string;
var
  idx: Integer;
  st, place: string;
begin
  idx := GetPersonEventIndex(iAttr.Name);
  if (idx = 0) then st := iAttr.Detail.Classification
  else
  if (idx > 0) then st := PersonEvents[idx].Name
  else st := iAttr.Name;

  place := iAttr.Detail.Place.StringValue;
  if (place <> '') then place := ' [' + place + ']';

  Result := st + ': ' + iAttr.StringValue + place;
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

procedure GetLifeDates(iRec: TGEDCOMIndividualRecord; var aBirthEvent, aDeathEvent: TGEDCOMCustomEvent);
var
  i: Integer;
  ev: TGEDCOMCustomEvent;
begin
  aBirthEvent := nil;
  aDeathEvent := nil;

  for i := 0 to iRec.IndividualEventsCount - 1 do begin
    ev := iRec.IndividualEvents[i];

    if (ev.Name = 'BIRT') then aBirthEvent := ev
    else
    if (ev.Name = 'DEAT') then aDeathEvent := ev;
  end;
end;

procedure GetIndependentDate(aDate: TGEDCOMCustomDate; var AYear: Integer; var AMonth, ADay: Word);
var
  dt_range: TGEDCOMDateRange;
  dt_period: TGEDCOMDatePeriod;
begin
  if (aDate is TGEDCOMDateApproximated) then begin
    TGEDCOMDate(aDate).GetDate(AYear, AMonth, ADay);
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
  //dt: TGEDCOMDate;
  y: Integer;
  m, d: Word;
begin
  Result := 0.0;

  GetIndependentDate(aEventDetail.Date.Value, y, m, d);

  //dt := TGEDCOMDate(aEventDetail.Date.Value);
  //if not((dt is TGEDCOMDate)) then Exit;
  //dt.GetDate(y, m, d);

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

function GetEventsYearsDiff(ev1, ev2: TGEDCOMCustomEvent; aCurEnd: Boolean = False): string;
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
  event, ev1, ev2: TGEDCOMCustomEvent;
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

function GetAge(iRec: TGEDCOMIndividualRecord; ToYear: Integer = -1): string;
var
  i: Integer;
  event, ev1, ev2: TGEDCOMCustomEvent;
  dummy: Word;
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

    if (ToYear = -1)
    then Result := GetEventsYearsDiff(ev1, ev2, True)
    else begin
      if (ev1 = nil)
      then Result := ''
      else begin
        GetIndependentDate(ev1.Detail.Date.Value, i, dummy, dummy);
        Result := IntToStr(ToYear - i);
      end;
    end;
  except
  end;
end;

function GetFirstbornAge(iRec: TGEDCOMIndividualRecord): string;
var
  y1, y2, y2tmp: Double;
  i, k: Integer;
  event: TGEDCOMCustomEvent;
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
  iEvent: TGEDCOMCustomEvent;
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
  event: TGEDCOMCustomEvent;
  xr: string;
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
    xr := iRec.XRef;
    Hole(xr);
  end;
end;

function GetChangeDate(aRec: TGEDCOMRecord): string;
begin
  try
    if (aRec.ChangeDate.ChangeDateTime = 0)
    then Result := ''
    else DateTimeToString(Result, 'yyyy.mm.dd hh:nn:ss', aRec.ChangeDate.ChangeDateTime);
  except
    Result := '';
  end;
end;

function CreateEventEx(aTree: TGEDCOMTree; aRec: TGEDCOMRecord;
  evSign: string; evDate: string = ''; evPlace: string = ''): TGEDCOMCustomEvent;
var
  kind: TPersonEventKind;
  ind_rec: TGEDCOMIndividualRecord;
  fam_rec: TGEDCOMFamilyRecord;
begin
  if (aRec is TGEDCOMIndividualRecord) then begin
    ind_rec := TGEDCOMIndividualRecord(aRec);

    kind := GetPersonEventKindBySign(evSign);

    if (kind = ekEvent)
    then Result := TGEDCOMIndividualEvent.Create(aTree, ind_rec)
    else Result := TGEDCOMIndividualAttribute.Create(aTree, ind_rec);

    ind_rec.AddIndividualEvent(Result);
  end
  else
  if (aRec is TGEDCOMFamilyRecord) then begin
    fam_rec := TGEDCOMFamilyRecord(aRec);

    Result := TGEDCOMFamilyEvent.Create(aTree, fam_rec);

    fam_rec.AddFamilyEvent(TGEDCOMFamilyEvent(Result));
  end
  else begin
    Result := nil;
    Exit;
  end;

  Result.Name := evSign;

  if (evDate <> '')
  then Result.Detail.Date.ParseString(evDate);

  if (evPlace <> '')
  then Result.Detail.Place.StringValue := evPlace;
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

function CreateFamilyEx(aTree: TGEDCOMTree): TGEDCOMFamilyRecord;
begin
  Result := TGEDCOMFamilyRecord.Create(aTree, aTree);
  Result.InitNew();
  Result.ChangeDate.ChangeDateTime := Now();
  aTree.AddRecord(Result);
end;

{warning: this is hack wrapper}
procedure SetAddressValue(anAddress: TGEDCOMAddress; aValue: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := aValue;
    anAddress.Address := sl;
  finally
    sl.Free;
  end;
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

function FamilyChildAdd(aTree: TGEDCOMTree; aFamily: TGEDCOMFamilyRecord;
  aChild: TGEDCOMIndividualRecord): Boolean;
var
  chLink: TGEDCOMChildToFamilyLink;
  ptr: TGEDCOMPointer;
begin
  try
    ptr := TGEDCOMPointer.Create(aTree, aFamily);
    ptr.SetNamedValue('CHIL', aChild);
    aFamily.AddChild(ptr);

    chLink := TGEDCOMChildToFamilyLink.Create(aTree, aChild);
    chLink.Family := aFamily;
    aChild.AddChildToFamilyLink(chLink);

    Result := True;
  except
    Result := False;
  end;
end;

function FamilyChildRemove(aTree: TGEDCOMTree; aFamily: TGEDCOMFamilyRecord;
  aChild: TGEDCOMIndividualRecord): Boolean;
begin
  try
    aFamily.DeleteChild(aChild);
    aChild.DeleteChildToFamilyLink(aFamily);

    Result := True;
  except
    Result := False;
  end;
end;

function CreateNote(aTree: TGEDCOMTree): TGEDCOMNoteRecord;
begin
  Result := TGEDCOMNoteRecord.Create(aTree, aTree);
  Result.InitNew();
  Result.ChangeDate.ChangeDateTime := Now();
  aTree.AddRecord(Result);
end;

function CreateNoteEx(aTree: TGEDCOMTree; aText: TStrings;
  aRecord: TGEDCOMRecord = nil): TGEDCOMNoteRecord;
begin
  Result := CreateNote(aTree);

  if (aText <> nil)
  then Result.Notes := aText;

  if (aRecord <> nil)
  then BindRecordNote(aTree, aRecord, Result);
end;

function CreateSource(aTree: TGEDCOMTree): TGEDCOMSourceRecord;
begin
  Result := TGEDCOMSourceRecord.Create(aTree, aTree);
  Result.InitNew();
  Result.ChangeDate.ChangeDateTime := Now();
  aTree.AddRecord(Result);
end;

procedure BindRecordNote(aTree: TGEDCOMTree; aRecord: TGEDCOMRecord;
  aNoteRec: TGEDCOMNoteRecord);
var
  note: TGEDCOMNotes;
begin
  note := TGEDCOMNotes.Create(aTree, aRecord);
  note.Value := aNoteRec;
  aRecord.AddNotes(note);
end;

procedure AddNoteText(aNoteRec: TGEDCOMNoteRecord; aText: string);
var
  strData: TStringList;
begin
  strData := TStringList.Create;
  try
    strData.Text := Trim(aNoteRec.Notes.Text);
    strData.Add(aText);
    aNoteRec.Notes := strData;
  finally
    strData.Free;
  end;
end;

procedure BindRecordSource(aTree: TGEDCOMTree; aRecord: TGEDCOMRecord;
  aSrcRec: TGEDCOMSourceRecord; aPage: string; aQuality: Integer);
var
  cit: TGEDCOMSourceCitation;
begin
  cit := TGEDCOMSourceCitation.Create(aTree, aRecord);
  cit.Value := aSrcRec;
  cit.Page := aPage;
  cit.CertaintyAssessment := aQuality;
  aRecord.AddSourceCitation(cit);
end;

procedure BindSourceRepository(aTree: TGEDCOMTree; aSourceRecord: TGEDCOMSourceRecord;
  aRepRec: TGEDCOMRepositoryRecord);
var
  cit: TGEDCOMRepositoryCitation;
begin
  cit := TGEDCOMRepositoryCitation.Create(aTree, aSourceRecord);
  cit.Value := aRepRec;
  aSourceRecord.AddRepositoryCitation(cit);
end;

procedure GetLocationLinks(aTree: TGEDCOMTree; aLocation: TGEDCOMLocationRecord;
  var aList: TStringList);
var
  i, k: Integer;
  rec: TGEDCOMRecord;
  i_rec: TGEDCOMIndividualRecord;
  f_rec: TGEDCOMFamilyRecord;
  event: TGEDCOMCustomEvent;
begin
  for i := 0 to aTree.RecordsCount - 1 do begin
    rec := aTree.Records[i];

    if (rec is TGEDCOMIndividualRecord) then begin
      i_rec := (rec as TGEDCOMIndividualRecord);

      for k := 0 to i_rec.IndividualEventsCount - 1 do begin
        event := i_rec.IndividualEvents[k];

        if (event.Detail.Place.Location.Value = aLocation)
        then aList.Add(GenRecordLink(aTree, rec, True) + ', ' + AnsiLowerCase(GetEventName(event)));
      end;
    end
    else
    if (rec is TGEDCOMFamilyRecord) then begin
      f_rec := (rec as TGEDCOMFamilyRecord);

      for k := 0 to f_rec.FamilyEventCount - 1 do begin
        event := f_rec.FamilyEvents[k];

        if (event.Detail.Place.Location.Value = aLocation)
        then aList.Add(GenRecordLink(aTree, rec, True) + ', ' + AnsiLowerCase(GetEventName(event)));
      end;
    end;
  end;
end;

function IsMatchesMask(const aName, aMask: string): Boolean;
var
  i, tok_count: Integer;
  strx, strmask: string;
begin
  Result := False;
  if (aName = '') or (aMask = '') then Exit;

  strx := AnsiLowerCase(aName);
  strmask := AnsiLowerCase(aMask);

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

function GenRecordLink(aTree: TGEDCOMTree; aRecord: TGEDCOMRecord;
  aSigned: Boolean = True): string;
var
  st, sign: string;
begin
  sign := '';
  if (aSigned) then begin
    if (aRecord is TGEDCOMIndividualRecord) then sign := ''
    else
    if (aRecord is TGEDCOMFamilyRecord) then sign := 'Семья: '
    else
    if (aRecord is TGEDCOMMultimediaRecord) then sign := 'Медиа-объект: '
    else
    if (aRecord is TGEDCOMGroupRecord) then sign := 'Группа: '
    else
    if (aRecord is TGEDCOMSourceRecord) then sign := 'Источник: '
    else
    if (aRecord is TGEDCOMRepositoryRecord) then sign := 'Архив: '
    else
    if (aRecord is TGEDCOMResearchRecord) then sign := 'Исследование: '
    else
    if (aRecord is TGEDCOMTaskRecord) then sign := 'Задача: '
    else
    if (aRecord is TGEDCOMCommunicationRecord) then sign := 'Корреспонденция: '
    else
    if (aRecord is TGEDCOMLocationRecord) then sign := 'Место: ';
  end;

  if (aRecord is TGEDCOMIndividualRecord)
  then st := GetNameStr(TGEDCOMIndividualRecord(aRecord))
  else
  if (aRecord is TGEDCOMFamilyRecord)
  then st := GetFamilyStr(TGEDCOMFamilyRecord(aRecord))
  else
  if (aRecord is TGEDCOMMultimediaRecord)
  then st := TGEDCOMMultimediaRecord(aRecord).FileReferences[0].Title
  else
  if (aRecord is TGEDCOMGroupRecord)
  then st := TGEDCOMGroupRecord(aRecord).Name
  else
  if (aRecord is TGEDCOMSourceRecord)
  then st := TGEDCOMSourceRecord(aRecord).FiledByEntry
  else
  if (aRecord is TGEDCOMRepositoryRecord)
  then st := TGEDCOMRepositoryRecord(aRecord).RepositoryName
  else
  if (aRecord is TGEDCOMResearchRecord)
  then st := TGEDCOMResearchRecord(aRecord).Name
  else
  if (aRecord is TGEDCOMTaskRecord)
  then st := GetTaskGoalStr(aTree, TGEDCOMTaskRecord(aRecord))
  else
  if (aRecord is TGEDCOMCommunicationRecord)
  then st := TGEDCOMCommunicationRecord(aRecord).Name
  else
  if (aRecord is TGEDCOMLocationRecord)
  then st := TGEDCOMLocationRecord(aRecord).Name
  else st := aRecord.XRef;

  Result := HyperLink(aRecord.XRef, sign + st);
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
      then f := Copy(f, 1, Length(f) - 1)
      else
      if (Copy(f, Length(f) - 2, 3) = 'кая')
      then f := Copy(f, 1, Length(f) - 3) + 'кий'
      else
      if (Copy(f, Length(f) - 2, 3) = 'ная')
      then f := Copy(f, 1, Length(f) - 3) + 'ный'
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

function GetAncestorsCount(aBuffer: TStringList; aPerson: TGEDCOMIndividualRecord): Integer;
var
  family: TGEDCOMFamilyRecord;
  anc: TGEDCOMIndividualRecord;
  xref: string;
  idx: Integer;
begin
  Result := 0;
  if (aPerson = nil) then Exit;

  xref := aPerson.XRef;

  if (aBuffer <> nil)
  then idx := aBuffer.IndexOf(xref)
  else idx := -1;

  if (idx >= 0) then begin
    Result := Integer(aBuffer.Objects[idx]);
  end else begin
    Result := 1;

    if (aPerson.ChildToFamilyLinksCount > 0) then begin
      family := aPerson.ChildToFamilyLinks[0].Family;

      anc := TGEDCOMIndividualRecord(family.Husband.Value);
      Result := Result + GetAncestorsCount(aBuffer, anc);

      anc := TGEDCOMIndividualRecord(family.Wife.Value);
      Result := Result + GetAncestorsCount(aBuffer, anc);
    end;

    if (aBuffer <> nil)
    then aBuffer.AddObject(xref, TObject(Result));
  end;
end;

function GetDescendantsCount(aBuffer: TStringList; aPerson: TGEDCOMIndividualRecord): Integer;
var
  family: TGEDCOMFamilyRecord;
  iChild: TGEDCOMIndividualRecord;
  i, k, idx: Integer;
  xref: string;
begin
  Result := 0;
  if (aPerson = nil) then Exit;

  xref := aPerson.XRef;

  if (aBuffer <> nil)
  then idx := aBuffer.IndexOf(xref)
  else idx := -1;

  if (idx >= 0) then begin
    Result := Integer(aBuffer.Objects[idx]);
  end else begin
    Result := 1;

    for i := 0 to aPerson.SpouseToFamilyLinksCount - 1 do begin
      family := aPerson.SpouseToFamilyLinks[i].Family;

      for k := 0 to family.ChildrenCount - 1 do begin
        iChild := TGEDCOMIndividualRecord(family.Children[k].Value);
        Result := Result + GetDescendantsCount(aBuffer, iChild);
      end;
    end;

    if (aBuffer <> nil)
    then aBuffer.AddObject(xref, TObject(Result));
  end;
end;

function GetDescGenerations(aPerson: TGEDCOMIndividualRecord): Integer;

  function GetDescGens_Recursive(aPerson: TGEDCOMIndividualRecord): Integer;
  var
    family: TGEDCOMFamilyRecord;
    iChild: TGEDCOMIndividualRecord;
    i, k, max, res: Integer;
  begin
    Result := 0;
    if (aPerson = nil) then Exit;

    max := 0;
    for i := 0 to aPerson.SpouseToFamilyLinksCount - 1 do begin
      family := aPerson.SpouseToFamilyLinks[i].Family;

      for k := 0 to family.ChildrenCount - 1 do begin
        iChild := TGEDCOMIndividualRecord(family.Children[k].Value);
        res := GetDescGens_Recursive(iChild);
        if (max < res) then max := res;
      end;
    end;
    Result := 1 + max;
  end;

begin
  Result := GetDescGens_Recursive(aPerson) - 1;
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
  event: TGEDCOMCustomEvent;
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

function PatriarchsCompare(Item1, Item2: Pointer): Integer;
begin
  Result := TPatriarchObj(Item1).IBirthYear - TPatriarchObj(Item2).IBirthYear;
end;

procedure GetPatriarchsList(aTree: TGEDCOMTree; aProgress, aLinks: Boolean;
  var aList: TObjectList; aMinGens: Integer = 2);

  function SearchAnc(descendantRec, searchRec: TGEDCOMIndividualRecord): Boolean;
  var
    family: TGEDCOMFamilyRecord;
    ancestor: TGEDCOMIndividualRecord;
  begin
    Result := False;
    if (descendantRec = nil) then Exit;

    Result := (descendantRec = searchRec);
    if (Result) then Exit;

    if (descendantRec.ChildToFamilyLinksCount > 0) then begin
      family := descendantRec.ChildToFamilyLinks[0].Family;

      ancestor := TGEDCOMIndividualRecord(family.Husband.Value);
      if (ancestor <> nil) then begin
        Result := SearchAnc(ancestor, searchRec);
        if (Result) then Exit;
      end;

      ancestor := TGEDCOMIndividualRecord(family.Wife.Value);
      if (ancestor <> nil) then begin
        Result := SearchAnc(ancestor, searchRec);
        if (Result) then Exit;
      end;
    end;
  end;

  function SearchDesc(ancestorRec, searchRec: TGEDCOMIndividualRecord): Boolean;
  var
    i, k: Integer;
    family: TGEDCOMFamilyRecord;
    child: TGEDCOMIndividualRecord;
    sp: TGEDCOMPointer;
    spouse: TGEDCOMIndividualRecord;
  begin
    Result := False;

    for i := 0 to ancestorRec.SpouseToFamilyLinksCount - 1 do begin
      family := ancestorRec.SpouseToFamilyLinks[i].Family;

      if (ancestorRec.Sex = svMale)
      then sp := family.Wife
      else sp := family.Husband;

      if (sp <> nil) then begin
        spouse := TGEDCOMIndividualRecord(sp.Value);

        Result := SearchAnc(spouse, searchRec);
        if (Result) then Exit;
      end;

      for k := 0 to family.ChildrenCount - 1 do begin
        child := TGEDCOMIndividualRecord(family.Children[k].Value);
        Result := SearchDesc(child, searchRec);
        if (Result) then Exit;
      end;
    end;
  end;

var
  buffer: TStringList;

  function GetIndependentYear(iRec: TGEDCOMIndividualRecord): Integer;
  var
    ev: TGEDCOMCustomEvent;
    year: Integer;
    am, ad: Word;
  begin
    ev := GetIndividualEvent(iRec, 'BIRT');

    if (ev = nil)
    then Result := -1
    else begin
      GetIndependentDate(ev.Detail.Date.Value, year, am, ad);
      Result := year;
    end;
  end;

  function GetBirthYear(iRec: TGEDCOMIndividualRecord): Integer;
  var
    family: TGEDCOMFamilyRecord;
    child: TGEDCOMIndividualRecord;
    i, k, year: Integer;
  begin
    Result := -1;
    if (iRec = nil) then Exit;

    year := GetIndependentYear(iRec);
    if (year > 0) then begin
      Result := year;
      Exit;
    end;

    for i := 0 to iRec.SpouseToFamilyLinksCount - 1 do begin
      family := iRec.SpouseToFamilyLinks[i].Family;

      for k := 0 to family.ChildrenCount - 1 do begin
        child := TGEDCOMIndividualRecord(family.Children[k].Value);
        year := GetBirthYear(child);
        if (year > 0) then begin
          Result := year - 20;
          Exit;
        end;
      end;
    end;
  end;

var
  i, k, bYear, descGens: Integer;
  rec: TGEDCOMRecord;
  i_rec: TGEDCOMIndividualRecord;
  res: Boolean;
  nf, nn, np: string;
  pObj: TPatriarchObj;
  patr1, patr2: TPatriarchObj;
begin
  if (aProgress) then ProgressInit(aTree.RecordsCount, 'Поиск патриархов');

  buffer := TStringList.Create;
  try
    for i := 0 to aTree.RecordsCount - 1 do begin
      rec := aTree.Records[i];

      if (rec is TGEDCOMIndividualRecord) then begin
        i_rec := rec as TGEDCOMIndividualRecord;
        GetNameParts(i_rec, nf, nn, np);
        bYear := GetBirthYear(i_rec);
        descGens := GetDescGenerations(i_rec);

        // 1: нет родителей
        res := (i_rec.ChildToFamilyLinksCount = 0);
        // 2: мужской пол
        res := res and (i_rec.Sex = svMale);
        // 3: известна фамилия и имя
        res := res and ((nf <> '') and (nf <> '?'))
                   and ((nn <> '') and (nn <> '?'));
        // 4: количество поколений потомков
        res := res and (descGens >= aMinGens);
        // 5: год рождения известен или можно вычислить
        res := res and (bYear > 0);

        // Поместить в список
        if (res) then begin
          pObj := TPatriarchObj.Create;
          pObj.IRec := i_rec;
          pObj.IBirthYear := bYear;
          pObj.IDescendantsCount := GetDescendantsCount(buffer, i_rec) - 1;
          pObj.IDescGenerations := descGens;
          pObj.ILinks := [];
          aList.Add(pObj);
        end;
      end;

      if (aProgress) then ProgressStep();
    end;

    aList.Sort(PatriarchsCompare);
  finally
    buffer.Free;
    if (aProgress) then ProgressDone();
  end;

  if (aLinks) then begin
    if (aProgress) then ProgressInit(aList.Count, 'Поиск взаимосвязей');
    try
      for i := 0 to aList.Count - 1 do begin
        patr1 := TPatriarchObj(aList[i]);

        for k := i + 1 to aList.Count - 1 do begin
          patr2 := TPatriarchObj(aList[k]);

          res := SearchDesc(patr1.IRec, patr2.IRec);
          if res then begin
            patr1.ILinks := patr1.ILinks + [k];
            patr2.ILinks := patr2.ILinks + [i];
          end;
        end;

        if (aProgress) then ProgressStep();
      end;
    finally
      if (aProgress) then ProgressDone();
    end;
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

  procedure CheckEventPlace(aEvent: TGEDCOMCustomEvent);
  var
    loc: TGEDCOMLocationRecord;
    place: TGEDCOMPlace;
  begin
    place := aEvent.Detail.Place;

    if (place.Location.XRef <> '') and (place.Location.Value = nil)
    then place.Location.XRef := '';

    if (place.StringValue <> '') then begin
      loc := TGEDCOMLocationRecord(place.Location.Value);

      if (loc <> nil) and (place.StringValue <> loc.Name)
      then place.StringValue := loc.Name;
    end;
  end;

  procedure AddUserRef(iRec: TGEDCOMIndividualRecord; uRef: string);
  var
    ref: TGEDCOMUserReference;
  begin
    ref := TGEDCOMUserReference.Create(aTree, iRec);
    ref.StringValue := uRef;
    iRec.AddUserReference(ref);
  end;

  // совместимость со старыми версиями
  procedure CheckAttrCompatible(iRec: TGEDCOMIndividualRecord; aEvent: TGEDCOMCustomEvent);
  var
    cause: string;
  begin
    if (aEvent.Name = '_MILI') then begin
      cause := AnsiLowerCase(aEvent.Detail.Classification);

      if (Pos('б/д', cause) > 0) then begin
        if (Pos('+', cause) > 0)
        then AddUserRef(iRec, UserRefs[urUSSR_FallInBattle].Name)
        else AddUserRef(iRec, UserRefs[urUSSR_Soldier].Name);

        aEvent.Detail.Classification := '';
      end
      else
      if (Pos('т/т', cause) > 0) then begin
        AddUserRef(iRec, UserRefs[urUSSR_RearVeteran].Name);

        aEvent.Detail.Classification := '';
      end;
    end;
  end;

  // совместимость со старыми версиями
  procedure CheckURefCompatible(iRec: TGEDCOMIndividualRecord; aUserRef: TGEDCOMUserReference);
  begin
    // dummy
  end;

  procedure CheckPerson(ind: TGEDCOMIndividualRecord);
  var
    k: Integer;
    evt: TGEDCOMCustomEvent;
  begin
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
      CheckEventPlace(evt);
      CheckAttrCompatible(ind, evt);
    end;

    for k := 0 to ind.UserReferencesCount - 1 do begin
      CheckURefCompatible(ind, ind.UserReferences[k]);
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

    for k := 0 to fam.FamilyEventCount - 1 do begin
      CheckEventPlace(fam.FamilyEvents[k]);
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

  {for i := 0 to aTree.RecordsCount - 1 do begin
    for k := i + 1 to aTree.RecordsCount - 1 do begin
      if (aTree.Records[i].XRef = aTree.Records[k].XRef)
      then ; // AddDiag(aTree.Records[i].XRef, 'Объект дублирован');
    end;
  end;}

  ProgressInit(aTree.RecordsCount, 'Проверка формата');
  try
    idCheck := True;
    i := 0;
    while (i < aTree.RecordsCount) do begin
      rec := aTree.Records[i];

      // temp patch
      {if (rec is TGEDCOMFamilyRecord)
      and (TGEDCOMFamilyRecord(rec).Husband.Value = nil)
      and (TGEDCOMFamilyRecord(rec).Wife.Value = nil)
      and (TGEDCOMFamilyRecord(rec).ChildrenCount = 0)
      and (TGEDCOMFamilyRecord(rec).FamilyEventCount = 0)
      then begin
        aTree.DeleteRecord(rec);
        Continue;
      end;}

      CheckRecord(aTree, rec);

      if (idCheck) and (GetId(rec) < 0)
      then idCheck := False;

      Inc(i);

      ProgressStep();
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

end.
