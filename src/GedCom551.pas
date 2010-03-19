unit GedCom551;

{$I compiler.inc}

(*
 * 2010-03-02
 *   (-) Removed support (SOUR.DATA).
 * 2010-02-10
 *   (-) Removed support (ALIA).
 * 2010-02-04
 *   (-) Removed support (BAPL, CONL, ENDL, SLGC, SLGS, STAT).
 * 2010-01-26
 *   (-) Removed interfaces support.
 *   (-) TGEDCOMIndividualRecord: removed support of "ANCI" and "DESI".
 * 2009-08-19 (zsv)
 *   (-) TGEDCOMFamilyRecord: removed support of "NCHI" (CountOfChildren).
 * 2009-08-17 (zsv)
 *   (-) Removed: TGEDCOMFamilyEventDetail & TGEDCOMIndividualEventDetail.
 *   (-) TGEDCOMFamilyEventDetail: removed support of "HUSB\AGE" & "WIFE\AGE".
 *   (-) TGEDCOMIndividualEventDetail: removed support of "AGE".
 *)

interface

uses
  Classes, SysUtils;

const
  GEDCOMDelimiter = ' ';
  GEDCOMYearModifierSeparator = '/';
  GEDCOMYearBC = 'B.C.';
  GEDCOMPointerDelimiter = #$40;              // @
  GEDCOMMaxPhoneNumbers = 3;
  GEDCOMMaxEmailAddresses = 3;
  GEDCOMMaxFaxNumbers = 3;
  GEDCOMMaxWebPages = 3;
  GEDCOMMaxLanguages = 3;
  GEDCOMNewLine = #13#10;

type
  TGEDCOMCalendar =
    (dcGregorian, dcJulian, dcHebrew, dcFrench, dcRoman, dcUnknown);

  TGEDCOMApproximated =
    (daExact, daAbout, daCalculated, daEstimated);

  TGEDCOMRange =
    (drAfter, drBefore, drBetween, drAnd);

  TGEDCOMCharacterSet =
    (csASCII, csANSEL, csUNICODE, csUTF8);

  TGEDCOMState =
    (osLoading, osReady);

  TGEDCOMRestriction =
    (rnNone, rnConfidential, rnLocked, rnPrivacy);

  TGEDCOMMultimediaFormat =
    (mfNone, mfBMP, mfGIF, mfJPG, mfOLE, mfPCX, mfTIF, mfWAV,
     mfTXT, mfRTF, mfAVI, mfTGA, mfPNG, mfMPG, mfHTM,
     mfUnknown);

  TGEDCOMMediaType =
    (mtNone, mtAudio, mtBook, mtCard, mtElectronic, mtFiche, mtFilm, mtMagazine,
     mtManuscript, mtMap, mtNewspaper, mtPhoto, mtTombstone, mtVideo, mtUnknown);

  TGEDCOMNameType =
    (ntAka, ntBirth, ntImmigrant, ntMaiden, ntMarried, ntUserDefined);

  TGEDCOMNamePieceType = (
    nptPrefix, nptGiven, nptNickname,
    nptSurnamePrefix, nptSurname, nptSuffix
  );

  TGEDCOMSex =
    (svNone, svMale, svFemale, svUndetermined);

  TGEDCOMOrdinanceProcessFlag =
    (opNone, opYes, opNo);

  TGEDCOMPedigreeLinkageType =
    (plNone, plAdopted, plBirth, plFoster, plSealing);

  TGEDCOMChildLinkageStatus =
    (clNone, clChallenged, clDisproven, clProven);

  TGEDCOMDateFormat =
    (dfGEDCOMStd, dfSystem);

const
  GEDCOMMonthRusArray: array[1..12] of string[3] =
    ('ЯНВ', 'ФЕВ', 'МАР', 'АПР', 'МАЙ', 'ИЮН',
     'ИЮЛ', 'АВГ', 'СЕН', 'ОКТ', 'НОЯ', 'ДЕК');
  GEDCOMMonthSysArray: array[1..12] of string[3] =
    ('01.', '02.', '03.', '04.', '05.', '06.',
     '07.', '08.', '09.', '10.', '11.', '12.');

  GEDCOMDateApproximatedArray: array[TGEDCOMApproximated] of string[3] =
    ('', 'ABT', 'CAL', 'EST');
  GEDCOMMonthArray: array[1..12] of string[3] =
    ('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC');
  GEDCOMMonthFrenchArray: array[1..13] of string[4] =
    ('VEND', 'BRUM', 'FRIM', 'NIVO', 'PLUV', 'VENT',
     'GERM', 'FLOR', 'PRAI', 'MESS', 'THER', 'FRUC', 'COMP');
  GEDCOMMonthHebrewArray: array[1..13] of string[3] =
    ('TSH', 'CSH', 'KSL', 'TVT', 'SHV', 'ADR',
     'ADS', 'NSN', 'IYR', 'SVN', 'TMZ', 'AAV', 'ELL');
  GEDCOMDateEscapeArray: array[TGEDCOMCalendar] of string =
    ('@#DGREGORIAN@', '@#DJULIAN@', '@#DHEBREW@',
     '@#DFRENCH R@', '@#DROMAN@', '@#DUNKNOWN@');
  GEDCOMDateRangeArray: array[TGEDCOMRange] of string[3] =
    ('AFT', 'BEF', 'BET', 'AND');
  GEDCOMDateInterpretedIdent = 'INT';
  GEDCOMDatePeriodFromIdent = 'FROM';
  GEDCOMDatePeriodToIdent = 'TO';

type
  EGEDCOMException = class(Exception);

  TGEDCOMTag = class;
  TGEDCOMDateValue = class;
  TGEDCOMDateExact = class;
  TGEDCOMDatePeriod = class;
  TGEDCOMTime = class;
  TGEDCOMRecord = class;
  TGEDCOMCustomRecord = class;
  TGEDCOMList = class;
  TGEDCOMHeader = class;
  TGEDCOMAddress = class;
  TGEDCOMNotes = class;
  TGEDCOMSourceCitation = class;
  TGEDCOMMultimediaLink = class;
  TGEDCOMRepositoryCitation = class;
  TGEDCOMChildToFamilyLink = class;
  TGEDCOMSpouseToFamilyLink = class;
  TGEDCOMAssociation = class;
  TGEDCOMChangeDate = class;
  TGEDCOMGroupRecord = class;

  TGEDCOMTagClass = class of TGEDCOMTag;

  TGEDCOMObject = class(TObject)
  end;

  TGEDCOMTree = class(TGEDCOMObject)
  private
    FRecords: TGEDCOMList;
    FHeader: TGEDCOMHeader;
    FXRefIndex: TStringList;
    FState: TGEDCOMState;

    function GetCount: Integer;
    function GetRecords(Index: Integer): TGEDCOMRecord;

    procedure XRefIndex_Clear();
    procedure XRefIndex_Add(const XRef: string; ARecord: TGEDCOMRecord);
    procedure XRefIndex_AddRecord(ARecord: TGEDCOMRecord);
    procedure XRefIndex_DeleteRecord(ARecord: TGEDCOMRecord);
  protected
    procedure DeleteRecord(Sender: TGEDCOMRecord);
    procedure SetXRef(Sender: TGEDCOMRecord; const XRef: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function AddRecord(ARecord: TGEDCOMRecord): TGEDCOMRecord;
    procedure Clear(); virtual;
    procedure Delete(Index: Integer);
    function Extract(Index: Integer): TGEDCOMRecord;
    function IndexOfRecord(ARecord: TGEDCOMRecord): Integer;

    procedure LoadFromFile(const aFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);

    procedure SaveHeaderToStream(AStream: TStream);
    procedure SaveFooterToStream(AStream: TStream);

    function XRefIndex_Find(const XRef: string): TGEDCOMRecord;
    function XRefIndex_NewXRef(Sender: TGEDCOMRecord): string;

    property Count: Integer read GetCount;
    property Header: TGEDCOMHeader read FHeader;
    property Records[Index: Integer]: TGEDCOMRecord read GetRecords;
    property State: TGEDCOMState read FState write FState;
  end;

  TXRefRec = record
    Rec: TGEDCOMRecord;
    OldXRef, NewXRef: string;
  end;

  TXRefReplaceMap = class(TObject)
  private
    FList: array of TXRefRec;
    function GetCount: Integer;
    function GetRecord(Index: Integer): TXRefRec;
  public
    procedure AddXRef(rec: TGEDCOMRecord; oldXRef, newXRef: string);
    function FindNewXRef(oldXRef: string): string;

    property Count: Integer read GetCount;
    property Records[Index: Integer]: TXRefRec read GetRecord;
  end;

  TGEDCOMList = class(TObject)
  private
    FList: TList;
    FOwner: TGEDCOMObject;
    function GetCount: Integer;
    function GetItems(Index: Integer): TGEDCOMObject;
  public
    constructor Create(AOwner: TGEDCOMObject);
    destructor Destroy; override;

    function Add(AObject: TGEDCOMObject): TGEDCOMObject;
    procedure Clear();
    procedure Delete(Index: Integer);
    procedure DeleteObject(AObject: TGEDCOMObject);
    procedure Exchange(Index1, Index2: Integer);
    function Extract(Index: Integer): TGEDCOMObject;
    function IndexOfObject(AObject: TGEDCOMObject): Integer;
    procedure SaveToStream(AStream: TStream); virtual;

    procedure ReplaceXRefs(aMap: TXRefReplaceMap);
    procedure ResetOwner(AOwner: TGEDCOMObject); 

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TGEDCOMObject read GetItems; default;
  end;

  TGEDCOMCustomTag = class(TGEDCOMObject)
  private
    FTags: TGEDCOMList;
    FOwner: TGEDCOMObject;
    FParent: TGEDCOMObject;
    FName: string;
    FStringValue: string;
    FLevel: Integer;
    function GetParentRecord(): TGEDCOMCustomRecord;
    function GetTags(Index: Integer): TGEDCOMTag;
    function GetCount: Integer;
  protected
    function FindRecord(const XRef: string): TGEDCOMRecord;
    function GetLevel: Integer;
    function GetStringValue: string; virtual;
    function InsertTag(ATag: TGEDCOMTag): TGEDCOMTag;
    procedure SetLevel(Value: Integer);
    procedure SetStringValue(const S: string); virtual;

    procedure SaveTagToStream(AStream: TStream; ATag: TGEDCOMTag); overload; virtual;
    procedure SaveTagToStream(AStream: TStream; const ATag: string); overload; virtual;
    procedure SaveTagsToStream(AStream: TStream; const ATagSorting: array of string); virtual;
    procedure SaveValueToStream(AStream: TStream); virtual;
    procedure StreamWriteNewLine(AStream: TStream);

    property ParentRecord: TGEDCOMCustomRecord read GetParentRecord;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); reintroduce; virtual;
    constructor CreateTag(AOwner, AParent: TGEDCOMObject; const AName: string = ''; const AValue: string = ''); virtual;
    constructor CreateCopy(Source: TGEDCOMCustomTag;
      AOwner: TGEDCOMObject = nil; AParent: TGEDCOMObject = nil); virtual;
    destructor Destroy; override;

    function  AddTag(const ATag: string; const AValue: string = ''; AClass: TGEDCOMTagClass = nil): TGEDCOMTag; virtual;
    procedure Assign(Source: TGEDCOMCustomTag); virtual;
    procedure Clear(); virtual;
    procedure Delete(Index: Integer);
    procedure DeleteTag(const ATag: string);
    function  FindTag(const ATag: string; StartIndex: Integer = 0): TGEDCOMTag;
    function  IndexOfTag(ATag: TGEDCOMTag): Integer;
    function  IsEmpty(): Boolean; virtual;
    function  ParseString(const AString: string): string; virtual;
    procedure SaveToStream(AStream: TStream); virtual;
    procedure SetTagIntegerValue(const ATag: string; AValue: Integer);
    procedure SetTagStringValue(const ATag, AValue: string);
    procedure SetTagStrings(ATag: TGEDCOMCustomTag; Value: TStrings);
    function  TagClass(const ATag: string; AClass: TGEDCOMTagClass): TGEDCOMTag;
    function  TagIntegerValue(const ATag: string; ADefault: Integer = 0): Integer;
    function  TagStringValue(const ATag: string): string;
    function  TagStrings(ATag: TGEDCOMCustomTag; var AStrings: TStrings): TStrings;

    procedure ReplaceXRefs(aMap: TXRefReplaceMap); virtual;
    procedure ResetOwner(AOwner: TGEDCOMObject); virtual;

    property Count: Integer read GetCount;
    property Level: Integer read GetLevel;
    property Name: string read FName write FName;
    property Owner: TGEDCOMObject read FOwner;
    property Parent: TGEDCOMObject read FParent;
    property StringValue: string read GetStringValue write SetStringValue;
    property Tags[Index: Integer]: TGEDCOMTag read GetTags;
  end;

  TGEDCOMTag = class(TGEDCOMCustomTag)
  public
    property ParentRecord;
  end;

  TGEDCOMLists = set of (stNotes, stSource, stMultimedia);

  TGEDCOMTagWithLists = class(TGEDCOMTag)
  private
    FLists: TGEDCOMLists;
    FNotes: TGEDCOMList;
    FSourceCitations: TGEDCOMList;
    FMultimediaLinks: TGEDCOMList;
    function GetNotes(Index: Integer): TGEDCOMNotes;
    function GetNotesCount: Integer;
    function GetSourceCitations(Index: Integer): TGEDCOMSourceCitation;
    function GetSourceCitationsCount: Integer;
    function GetMultimediaLinks(Index: Integer): TGEDCOMMultimediaLink;
    function GetMultimediaLinksCount: Integer;
  protected
  public
    constructor Create(AOwner, AParent: TGEDCOMObject; ALists: TGEDCOMLists); reintroduce; virtual;
    destructor Destroy; override;

    function AddNotes(ANotes: TGEDCOMNotes): TGEDCOMNotes;
    function AddSourceCitation(ASourceCitation: TGEDCOMSourceCitation): TGEDCOMSourceCitation;
    function AddMultimediaLink(AMultimediaLink: TGEDCOMMultimediaLink): TGEDCOMMultimediaLink;

    procedure DeleteNotes(aIndex: Integer);
    procedure DeleteSourceCitation(aIndex: Integer);
    procedure DeleteMultimediaLink(aIndex: Integer);

    procedure ReplaceXRefs(aMap: TXRefReplaceMap); override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;
    procedure SaveToStream(AStream: TStream); override;

    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    procedure Clear(); override;
    function IsEmpty(): Boolean; override;

    property Notes[Index: Integer]: TGEDCOMNotes read GetNotes;
    property NotesCount: Integer read GetNotesCount;
    property SourceCitations[Index: Integer]: TGEDCOMSourceCitation
      read GetSourceCitations;
    property SourceCitationsCount: Integer read GetSourceCitationsCount;
    property MultimediaLinks[Index: Integer]: TGEDCOMMultimediaLink
      read GetMultimediaLinks;
    property MultimediaLinksCount: Integer read GetMultimediaLinksCount;
  end;

  TGEDCOMCustomRecord = class(TGEDCOMCustomTag)
  private
    FXRef: string;
    procedure SetXRef(const AXRef: string);
  protected
    function AddSubTag(AParent: TGEDCOMCustomTag; const ATag: string;
      const AValue: string = ''; AClass: TGEDCOMTagClass = nil): TGEDCOMTag; virtual;
    procedure SaveValueToStream(AStream: TStream); override;

    property XRef: string read FXRef write SetXRef;
  public
    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
  end;

  TGEDCOMPointer = class(TGEDCOMTag)
  private
    FXRef: string;
    function GetValue: TGEDCOMRecord;
    procedure SetValue(AValue: TGEDCOMRecord);
    function GetXRef: string;
    procedure SetXRef(const Value: string);
  protected
    function GetStringValue: string; override;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;

    procedure Clear(); override;
    function IsEmpty(): Boolean; override;
    function ParseString(const AString: string): string; override;
    procedure ReplaceXRefs(aMap: TXRefReplaceMap); override;

    property Value: TGEDCOMRecord read GetValue write SetValue;
    property XRef: string read GetXRef write SetXRef;
  end;

  TGEDCOMHeader = class(TGEDCOMCustomRecord)
  private
    FNotes: TStrings;
    function GetStringTag(Index: Integer): string;
    procedure SetStringTag(Index: Integer; const Value: string);
    function GetSourceBusinessAddress: TGEDCOMAddress;
    function GetDate: TGEDCOMDateExact;
    function GetTime: TGEDCOMTime;
    function GetSubmittor: TGEDCOMPointer;
    function GetSubmission: TGEDCOMPointer;
    function GetCharacterSet: TGEDCOMCharacterSet;
    procedure SetCharacterSet(const Value: TGEDCOMCharacterSet);
    function GetNotes: TStrings;
    procedure SetNotes(const Value: TStrings);
    function GetTransmissionDateTime: TDateTime;
    procedure SetTransmissionDateTime(const Value: TDateTime);
  protected
    function AddSubTag(AParent: TGEDCOMCustomTag; const ATag: string;
      const AValue: string = ''; AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;
    destructor Destroy; override;

    procedure Clear; override;
    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;

    property CharacterSet: TGEDCOMCharacterSet read GetCharacterSet write SetCharacterSet;
    property CharacterSetVersion: string index 10 read GetStringTag write SetStringTag;
    property Copyright: string index 7 read GetStringTag write SetStringTag;
    property FileName: string index 6 read GetStringTag write SetStringTag;
    property GEDCOMVersion: string index 8 read GetStringTag write SetStringTag;
    property GEDCOMForm: string index 9 read GetStringTag write SetStringTag;
    property Language: string index 11 read GetStringTag write SetStringTag;
    property Notes: TStrings read GetNotes write SetNotes;
    property PlaceHierarchy: string index 12 read GetStringTag write SetStringTag;
    property ReceivingSystemName: string index 5 read GetStringTag write SetStringTag;
    property Source: string index 1 read GetStringTag write SetStringTag;
    property SourceVersion: string index 2 read GetStringTag write SetStringTag;
    property SourceProductName: string index 3 read GetStringTag write SetStringTag;
    property SourceBusinessName: string index 4 read GetStringTag write SetStringTag;
    property SourceBusinessAddress: TGEDCOMAddress read GetSourceBusinessAddress;
    property Submission: TGEDCOMPointer read GetSubmission;
    property Submitter: TGEDCOMPointer read GetSubmittor;
    property TransmissionDate: TGEDCOMDateExact read GetDate;
    property TransmissionTime: TGEDCOMTime read GetTime;
    property TransmissionDateTime: TDateTime read GetTransmissionDateTime write SetTransmissionDateTime;
  end;

  TGEDCOMRecord = class(TGEDCOMCustomRecord)
  private
    FLists: TGEDCOMLists;
    FMultimediaLinks: TGEDCOMList;
    FNotes: TGEDCOMList;
    FSourceCitations: TGEDCOMList;

    function GetChangeDate: TGEDCOMChangeDate;
    function GetMultimediaLinks(Index: Integer): TGEDCOMMultimediaLink;
    function GetMultimediaLinksCount: Integer;
    function GetNotes(Index: Integer): TGEDCOMNotes;
    function GetNotesCount: Integer;
    function GetSourceCitations(Index: Integer): TGEDCOMSourceCitation;
    function GetSourceCitationsCount: Integer;
  protected
  public
    constructor Create(AOwner, AParent: TGEDCOMObject; ALists: TGEDCOMLists); reintroduce; virtual;
    destructor Destroy; override;

    function AddMultimediaLink(AMultimediaLink: TGEDCOMMultimediaLink): TGEDCOMMultimediaLink;
    function AddNotes(ANotes: TGEDCOMNotes): TGEDCOMNotes;
    function AddSourceCitation(ASourceCitation: TGEDCOMSourceCitation): TGEDCOMSourceCitation;

    procedure DeleteMultimediaLink(aIndex: Integer);
    procedure DeleteNotes(aIndex: Integer);
    procedure DeleteSourceCitation(aIndex: Integer);

    procedure MoveTo(aToRecord: TGEDCOMRecord); virtual;
    procedure ReplaceXRefs(aMap: TXRefReplaceMap); override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;
    procedure SaveToStream(AStream: TStream); override;

    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    procedure Clear(); override;
    function IsEmpty(): Boolean; override;
    function NewXRef(): string;

    property ChangeDate: TGEDCOMChangeDate read GetChangeDate;
    property MultimediaLinks[Index: Integer]: TGEDCOMMultimediaLink read GetMultimediaLinks;
    property MultimediaLinksCount: Integer read GetMultimediaLinksCount;
    property Notes[Index: Integer]: TGEDCOMNotes read GetNotes;
    property NotesCount: Integer read GetNotesCount;
    property SourceCitations[Index: Integer]: TGEDCOMSourceCitation read GetSourceCitations;
    property SourceCitationsCount: Integer read GetSourceCitationsCount;
    property XRef;
  end;

  TGEDCOMPlace = class(TGEDCOMTagWithLists)
  private
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); reintroduce; virtual;

    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    procedure SaveToStream(AStream: TStream); override;

    property Notes;
    property NotesCount;
  end;

  { }
  TGEDCOMEventDetail = class(TGEDCOMTagWithLists)
  private
    function GetStringTag(Index: Integer): string;
    procedure SetStringTag(Index: Integer; const Value: string);
    function GetDate: TGEDCOMDateValue;
    function GetAddress: TGEDCOMAddress;
    function GetRestriction: TGEDCOMRestriction;
    procedure SetRestriction(const Value: TGEDCOMRestriction);
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); reintroduce; virtual;

    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    procedure SaveToStream(AStream: TStream); override;

    property Classification: string index 1 read GetStringTag write SetStringTag;
    property Date: TGEDCOMDateValue read GetDate;
    property Place: string index 2 read GetStringTag write SetStringTag;
    property PlaceForm: string index 3 read GetStringTag write SetStringTag;
    property Address: TGEDCOMAddress read GetAddress;
    property Agency: string index 4 read GetStringTag write SetStringTag;
    property ReligiousAffilation: string index 5 read GetStringTag write SetStringTag;
    property Cause: string index 6 read GetStringTag write SetStringTag;
    property Restriction: TGEDCOMRestriction read GetRestriction write SetRestriction;

    property Notes;
    property NotesCount;
    property SourceCitations;
    property SourceCitationsCount;
    property MultimediaLinks;
    property MultimediaLinksCount;
  end;

  { }
  TGEDCOMCustomEvent = class(TGEDCOMTag)
  private
    FDetail: TGEDCOMEventDetail;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;
    destructor Destroy; override;

    procedure Assign(Source: TGEDCOMCustomTag); override;
    procedure ReplaceXRefs(aMap: TXRefReplaceMap); override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;
    procedure SaveToStream(AStream: TStream); override;

    property Detail: TGEDCOMEventDetail read FDetail;
  end;

  { }
  TGEDCOMFamilyEvent = class(TGEDCOMCustomEvent)
  private
  public
    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
  end;

  { }
  TGEDCOMIndividualEvent = class(TGEDCOMCustomEvent)
  private
    function GetFamily: TGEDCOMPointer;
  public
    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;

    // ONLY if this is a BIRT, CHR or ADOP event, the Family property may
    // contain a link to a family record.
    property Family: TGEDCOMPointer read GetFamily;
  end;

  { }
  TGEDCOMIndividualAttribute = class(TGEDCOMCustomEvent)
  private
    FPhysicalDescription: TStrings;
    function GetPhysicalDescription: TStrings;
    procedure SetPhysicalDescription(Value: TStrings);
  public
    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;

    // ONLY if this is a DSCR event, the PhysicalDescription can be used.
    property PhysicalDescription: TStrings
      read GetPhysicalDescription write SetPhysicalDescription;
  end;

  { }
  TGEDCOMFileReference = class(TGEDCOMTag)
  private
    function GetMultimediaFormat: TGEDCOMMultimediaFormat;
    procedure SetMultimediaFormat(const Value: TGEDCOMMultimediaFormat);
    function GetMediaType: TGEDCOMMediaType;
    procedure SetMediaType(const Value: TGEDCOMMediaType);
  protected
    function MediaTypeTagName: string; virtual;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;

    procedure LinkFile(const AFile: string;
      AMediaType: TGEDCOMMediaType = mtUnknown;
      AMultimediaFormat: TGEDCOMMultimediaFormat = mfUnknown);
    class function RecognizeFormat(const AFile: string): TGEDCOMMultimediaFormat;

    property MultimediaFormat: TGEDCOMMultimediaFormat
      read GetMultimediaFormat write SetMultimediaFormat;
    property MediaType: TGEDCOMMediaType read GetMediaType write SetMediaType;
  end;

  TGEDCOMFileReferenceWithTitle = class(TGEDCOMFileReference)
  private
    function GetTitle: string;
    procedure SetTitle(const Value: string);
  protected
    function MediaTypeTagName: string; override;
  public
    property Title: string read GetTitle write SetTitle;
  end;

  TGEDCOMUserReference = class(TGEDCOMTag)
  private
    function GetReferenceType: string;
    procedure SetReferenceType(const Value: string);
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;

    property ReferenceType: string read GetReferenceType write SetReferenceType;
  end;

  { The change date is intended to only record the last change to a record.
    Some systems may want to manage the change process with more detail, but
    it is sufficient for GEDCOM purposes to indicate the last time that a
    record was modified. }
  TGEDCOMChangeDate = class(TGEDCOMTag)
  private
    function GetDate: TGEDCOMDateExact;
    function GetTime: TGEDCOMTime;
    function GetChangeDateTime: TDateTime;
    procedure SetChangeDateTime(const Value: TDateTime);
    function GetNotes: TGEDCOMNotes;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;

    function AddTag(const ATag: string; const AValue: string = ''; AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    function IsEmpty(): Boolean; override;

    property ChangeDate: TGEDCOMDateExact read GetDate;
    property ChangeTime: TGEDCOMTime read GetTime;
    property ChangeDateTime: TDateTime read GetChangeDateTime write SetChangeDateTime;
    property Notes: TGEDCOMNotes read GetNotes;
  end;

  TGEDCOMPersonalNamePieces = class(TGEDCOMTagWithLists)
  private
    function GetStringTag(Index: Integer): string;
    procedure SetStringTag(Index: Integer; const Value: string);
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); reintroduce; virtual;

    procedure SaveToStream(AStream: TStream); override;

    property Prefix: string index 1 read GetStringTag write SetStringTag;
    property Given: string index 2 read GetStringTag write SetStringTag;
    property Nickname: string index 3 read GetStringTag write SetStringTag;
    property SurnamePrefix: string index 4 read GetStringTag write SetStringTag;
    property Surname: string index 5 read GetStringTag write SetStringTag;
    property Suffix: string index 6 read GetStringTag write SetStringTag;

    property Notes;
    property NotesCount;
    property SourceCitations;
    property SourceCitationsCount;
  end;

  TGEDCOMPersonalName = class(TGEDCOMTag)
  private
    FPieces: TGEDCOMPersonalNamePieces;

    function GetFirstPart: string;
    function GetFullName: string;
    function GetLastPart: string;
    function GetSurname: string;
    procedure SetSurname(const Value: string);
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;
    destructor Destroy; override;

    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    procedure Assign(Source: TGEDCOMCustomTag); override;
    procedure Clear(); override;
    function IsEmpty(): Boolean; override;
    procedure ReplaceXRefs(aMap: TXRefReplaceMap); override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure SetNameParts(const FirstPart, Surname, LastPart: string);

    property FullName: string read GetFullName;
    property FirstPart: string read GetFirstPart;
    property Surname: string read GetSurname write SetSurname;
    property LastPart: string read GetLastPart;

    property Pieces: TGEDCOMPersonalNamePieces read FPieces;

    //property NameType: TGEDCOMNameType read Get write Set;
  end;

  TGEDCOMFamilyRecord = class(TGEDCOMRecord)
  private
    FFamilyEvents: TGEDCOMList;
    FChildren: TGEDCOMList;
    FUserReferences: TGEDCOMList;
    function GetStringTag(Index: Integer): string;
    procedure SetStringTag(Index: Integer; const Value: string);
    function GetRestriction: TGEDCOMRestriction;
    procedure SetRestriction(const Value: TGEDCOMRestriction);
    function GetFamilyEvents(Index: Integer): TGEDCOMFamilyEvent;
    function GetFamilyEventCount: Integer;
    function GetChildren(Index: Integer): TGEDCOMPointer;
    function GetChildrenCount: Integer;
    function GetHusband: TGEDCOMPointer;
    function GetWife: TGEDCOMPointer;
    function GetSubmittor: TGEDCOMPointer;
    function GetUserReferences(Index: Integer): TGEDCOMUserReference;
    function GetUserReferencesCount: Integer;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); reintroduce; virtual;
    destructor Destroy; override;

    function AddChild(APointer: TGEDCOMPointer): TGEDCOMPointer;
    function AddFamilyEvent(AFamilyEvent: TGEDCOMFamilyEvent): TGEDCOMFamilyEvent;
    function AddUserReference(AUserReference: TGEDCOMUserReference): TGEDCOMUserReference;

    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    procedure Clear(); override;
    function IsEmpty(): Boolean; override;

    procedure DeleteFamilyEvent(aEvent: TGEDCOMFamilyEvent);
    procedure RemoveChild(XRef: string);

    procedure MoveTo(aToRecord: TGEDCOMRecord); override;
    procedure ReplaceXRefs(aMap: TXRefReplaceMap); override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure SortChilds();

    property AutomatedRecordID: string index 1 read GetStringTag write SetStringTag;
    property Children[Index: Integer]: TGEDCOMPointer read GetChildren;
    property ChildrenCount: Integer read GetChildrenCount;
    property FamilyEvents[Index: Integer]: TGEDCOMFamilyEvent read GetFamilyEvents;
    property FamilyEventCount: Integer read GetFamilyEventCount;
    property Husband: TGEDCOMPointer read GetHusband;
    property Restriction: TGEDCOMRestriction read GetRestriction write SetRestriction;

    property Submitter: TGEDCOMPointer read GetSubmittor;
    property UserReferences[Index: Integer]: TGEDCOMUserReference read GetUserReferences;
    property UserReferencesCount: Integer read GetUserReferencesCount;
    property Wife: TGEDCOMPointer read GetWife;

    property Notes;
    property NotesCount;
    property SourceCitations;
    property SourceCitationsCount;
    property MultimediaLinks;
    property MultimediaLinksCount;
  end;

  TGEDCOMIndividualRecord = class(TGEDCOMRecord)
  private
    FPersonalNames: TGEDCOMList;
    FIndividualEvents: TGEDCOMList;
    FIndividualAttributes: TGEDCOMList;
    FChildToFamilyLinks: TGEDCOMList;
    FSpouseToFamilyLinks: TGEDCOMList;
    FSubmittors: TGEDCOMList;
    FAssociations: TGEDCOMList;
    FUserReferences: TGEDCOMList;
    FGroups: TGEDCOMList;

    function GetRestriction: TGEDCOMRestriction;
    procedure SetRestriction(const Value: TGEDCOMRestriction);
    function GetPersonalNames(Index: Integer): TGEDCOMPersonalName;
    function GetPersonalNamesCount: Integer;
    function GetSex: TGEDCOMSex;
    procedure SetSex(const Value: TGEDCOMSex);
    function GetIndividualEvents(Index: Integer): TGEDCOMIndividualEvent;
    function GetIndividualEventsCount: Integer;
    function GetIndividualAttributes(Index: Integer): TGEDCOMIndividualAttribute;
    function GetIndividualAttributesCount: Integer;
    function GetChildToFamilyLinks(Index: Integer): TGEDCOMChildToFamilyLink;
    function GetChildToFamilyLinksCount: Integer;
    function GetSpouseToFamilyLinks(Index: Integer): TGEDCOMSpouseToFamilyLink;
    function GetSpouseToFamilyLinksCount: Integer;
    function GetSubmittors(Index: Integer): TGEDCOMPointer;
    function GetSubmittorsCount: Integer;
    function GetAssociations(Index: Integer): TGEDCOMAssociation;
    function GetAssociationsCount: Integer;
    function GetStringTag(Index: Integer): string;
    procedure SetStringTag(Index: Integer; const Value: string);
    function GetUserReferences(Index: Integer): TGEDCOMUserReference;
    function GetUserReferencesCount: Integer;
    function GetGroups(Index: Integer): TGEDCOMPointer;
    function GetGroupsCount: Integer;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); reintroduce; virtual;
    destructor Destroy; override;

    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    procedure Clear(); override;
    function IsEmpty(): Boolean; override;

    function AddAssociation(Value: TGEDCOMAssociation): TGEDCOMAssociation;
    function AddChildToFamilyLink(Value: TGEDCOMChildToFamilyLink): TGEDCOMChildToFamilyLink;
    function AddIndividualAttribute(Value: TGEDCOMIndividualAttribute): TGEDCOMIndividualAttribute;
    function AddIndividualEvent(Value: TGEDCOMIndividualEvent): TGEDCOMIndividualEvent;
    function AddPersonalName(Value: TGEDCOMPersonalName): TGEDCOMPersonalName;
    function AddSpouseToFamilyLink(Value: TGEDCOMSpouseToFamilyLink): TGEDCOMSpouseToFamilyLink;
    function AddSubmittor(Value: TGEDCOMPointer): TGEDCOMPointer;
    function AddUserReference(AUserReference: TGEDCOMUserReference): TGEDCOMUserReference;

    function AddGroup(Value: TGEDCOMPointer): TGEDCOMPointer;
    procedure DeleteGroup(aIndex: Integer);
    function IndexOfGroup(aGroup: TGEDCOMGroupRecord): Integer;

    procedure DeleteAssociation(aIndex: Integer);
    procedure DeleteIndividualAttribute(anAttribute: TGEDCOMIndividualAttribute);
    procedure DeleteIndividualEvent(aEvent: TGEDCOMIndividualEvent);
    procedure DeleteSpouseToFamilyLink(Family: TGEDCOMFamilyRecord);
    procedure DeleteChildToFamilyLink(Family: TGEDCOMFamilyRecord);

    procedure ExchangeSpouses(Index1, Index2: Integer);

    procedure MoveTo(aToRecord: TGEDCOMRecord); override;
    procedure ReplaceXRefs(aMap: TXRefReplaceMap); override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;
    procedure SaveToStream(AStream: TStream); override;

    property AncestralFileNumber: string index 2 read GetStringTag write SetStringTag;
    property AutomatedRecordID: string index 3 read GetStringTag write SetStringTag;
    property IndividualAttributes[Index: Integer]: TGEDCOMIndividualAttribute read GetIndividualAttributes;
    property IndividualAttributesCount: Integer read GetIndividualAttributesCount;
    property IndividualEvents[Index: Integer]: TGEDCOMIndividualEvent read GetIndividualEvents;
    property IndividualEventsCount: Integer read GetIndividualEventsCount;
    property PermanentRecordFileNumber: string index 1 read GetStringTag write SetStringTag;
    property PersonalNames[Index: Integer]: TGEDCOMPersonalName read GetPersonalNames;
    property PersonalNamesCount: Integer read GetPersonalNamesCount;
    property Restriction: TGEDCOMRestriction read GetRestriction write SetRestriction;
    property Sex: TGEDCOMSex read GetSex write SetSex;
    property UserReferences[Index: Integer]: TGEDCOMUserReference read GetUserReferences;
    property UserReferencesCount: Integer read GetUserReferencesCount;

    property ChildToFamilyLinks[Index: Integer]: TGEDCOMChildToFamilyLink read GetChildToFamilyLinks;
    property ChildToFamilyLinksCount: Integer read GetChildToFamilyLinksCount;
    property SpouseToFamilyLinks[Index: Integer]: TGEDCOMSpouseToFamilyLink read GetSpouseToFamilyLinks;
    property SpouseToFamilyLinksCount: Integer read GetSpouseToFamilyLinksCount;

    property Submittors[Index: Integer]: TGEDCOMPointer read GetSubmittors;
    property SubmittorsCount: Integer read GetSubmittorsCount;
    property Associations[Index: Integer]: TGEDCOMAssociation read GetAssociations;
    property AssociationsCount: Integer read GetAssociationsCount;

    property Groups[Index: Integer]: TGEDCOMPointer read GetGroups;
    property GroupsCount: Integer read GetGroupsCount;

    property Notes;
    property NotesCount;
    property SourceCitations;
    property SourceCitationsCount;
    property MultimediaLinks;
    property MultimediaLinksCount;
  end;

  { The BLOB context of the multimedia record was removed in version 5.5.1.
    A reference to a multimedia file was added to the record structure. The
    file reference occurs one to many times so that multiple files can be
    grouped together, each pertaining to the same context. For example, if you
    wanted to associate a sound clip and a photo, you would reference each
    multimedia file and indicate the format using the FORM tag subordinate to
    each file reference. }
  TGEDCOMMultimediaRecord = class(TGEDCOMRecord)
  private
    FFileReferences: TGEDCOMList;
    FUserReferences: TGEDCOMList;
    function GetStringTag(Index: Integer): string;
    procedure SetStringTag(Index: Integer; const Value: string);
    function GetFileReferences(Index: Integer): TGEDCOMFileReferenceWithTitle;
    function GetFileReferencesCount: Integer;
    function GetUserReferences(Index: Integer): TGEDCOMUserReference;
    function GetUserReferencesCount: Integer;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); reintroduce; virtual;
    destructor Destroy; override;

    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    function AddFileReference(Value: TGEDCOMFileReferenceWithTitle): TGEDCOMFileReferenceWithTitle;
    function AddUserReference(AUserReference: TGEDCOMUserReference): TGEDCOMUserReference;
    procedure Clear(); override;
    function IsEmpty(): Boolean; override;

    procedure ReplaceXRefs(aMap: TXRefReplaceMap); override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;
    procedure SaveToStream(AStream: TStream); override;

    property AutomatedRecordID: string index 1 read GetStringTag write SetStringTag;
    property FileReferences[Index: Integer]: TGEDCOMFileReferenceWithTitle read GetFileReferences;
    property FileReferencesCount: Integer read GetFileReferencesCount;
    property UserReferences[Index: Integer]: TGEDCOMUserReference read GetUserReferences;
    property UserReferencesCount: Integer read GetUserReferencesCount;

    property Notes;
    property NotesCount;
    property SourceCitations;
    property SourceCitationsCount;
  end;

  { Note records are used to provide notes that can be pointed to by multiple
    tags. }
  TGEDCOMNoteRecord = class(TGEDCOMRecord)
  private
    FNotes: TStrings;
    FUserReferences: TGEDCOMList;
    function GetNotes: TStrings;
    procedure SetNotes(Value: TStrings);
    function GetStringTag(Index: Integer): string;
    function GetUserReferences(Index: Integer): TGEDCOMUserReference;
    function GetUserReferencesCount: Integer;
    procedure SetStringTag(Index: Integer; const Value: string);
  protected
    function AddUserReference(AUserReference: TGEDCOMUserReference): TGEDCOMUserReference;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); reintroduce; virtual;
    destructor Destroy; override;

    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    procedure Clear(); override;
    function IsEmpty(): Boolean; override;

    procedure MoveTo(aToRecord: TGEDCOMRecord); override;
    procedure ReplaceXRefs(aMap: TXRefReplaceMap); override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;

    property AutomatedRecordID: string index 1 read GetStringTag write SetStringTag;
    property Notes: TStrings read GetNotes write SetNotes;
    property UserReferences[Index: Integer]: TGEDCOMUserReference read GetUserReferences;
    property UserReferencesCount: Integer read GetUserReferencesCount;

    property SourceCitations;
    property SourceCitationsCount;
  end;

  { }
  TGEDCOMRepositoryRecord = class(TGEDCOMRecord)
  private
    FUserReferences: TGEDCOMList;
    function GetStringTag(Index: Integer): string;
    procedure SetStringTag(Index: Integer; const Value: string);
    function GetAddress: TGEDCOMAddress;
    function GetUserReferences(Index: Integer): TGEDCOMUserReference;
    function GetUserReferencesCount: Integer;
  protected
    function AddUserReference(AUserReference: TGEDCOMUserReference): TGEDCOMUserReference;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); reintroduce; virtual;
    destructor Destroy; override;

    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    procedure Clear(); override;
    function IsEmpty(): Boolean; override;
    procedure ReplaceXRefs(aMap: TXRefReplaceMap); override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;

    property Address: TGEDCOMAddress read GetAddress;
    property AutomatedRecordID: string index 2 read GetStringTag write SetStringTag;
    property RepositoryName: string index 1 read GetStringTag write SetStringTag;
    property UserReferences[Index: Integer]: TGEDCOMUserReference read GetUserReferences;
    property UserReferencesCount: Integer read GetUserReferencesCount;

    property Notes;
    property NotesCount;
  end;

  { Source records are used to provide a bibliographic description of the
    source cited. }
  TGEDCOMSourceRecord = class(TGEDCOMRecord)
  private
    FTitle: TStrings;
    FOriginator: TStrings;
    FPublication: TStrings;
    FText: TStrings;
    FRepositoryCitations: TGEDCOMList;
    FUserReferences: TGEDCOMList;
    function GetTitle: TStrings;
    procedure SetTitle(Value: TStrings);
    function GetOriginator: TStrings;
    procedure SetOriginator(const Value: TStrings);
    function GetPublication: TStrings;
    function GetText: TStrings;
    procedure SetPublication(const Value: TStrings);
    procedure SetText(const Value: TStrings);
    function GetStringTag(Index: Integer): string;
    procedure SetStringTag(Index: Integer; const Value: string);
    function GetRepositoryCitations(Index: Integer): TGEDCOMRepositoryCitation;
    function GetRepositoryCitationsCount: Integer;
    function GetUserReferences(Index: Integer): TGEDCOMUserReference;
    function GetUserReferencesCount: Integer;
  protected
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); reintroduce; virtual;
    destructor Destroy; override;

    function AddRepositoryCitation(Value: TGEDCOMRepositoryCitation): TGEDCOMRepositoryCitation;
    function AddUserReference(AUserReference: TGEDCOMUserReference): TGEDCOMUserReference;

    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    procedure Clear(); override;
    function IsEmpty(): Boolean; override;

    procedure RemoveRepositoryCitation(Value: TGEDCOMRepositoryCitation);

    procedure MoveTo(aToRecord: TGEDCOMRecord); override;
    procedure ReplaceXRefs(aMap: TXRefReplaceMap); override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;
    procedure SaveToStream(AStream: TStream); override;

    property AutomatedRecordID: string index 2 read GetStringTag write SetStringTag;
    property RepositoryCitations[Index: Integer]: TGEDCOMRepositoryCitation read GetRepositoryCitations;
    property RepositoryCitationsCount: Integer read GetRepositoryCitationsCount;
    property UserReferences[Index: Integer]: TGEDCOMUserReference read GetUserReferences;
    property UserReferencesCount: Integer read GetUserReferencesCount;

    property Originator: TStrings read GetOriginator write SetOriginator;
    property Title: TStrings read GetTitle write SetTitle;
    property FiledByEntry: string index 1 read GetStringTag write SetStringTag;
    property Publication: TStrings read GetPublication write SetPublication;
    property Text: TStrings read GetText write SetText;

    property MultimediaLinks;
    property MultimediaLinksCount;
    property Notes;
    property NotesCount;
  end;

  { The sending system uses a submission record to send instructions and
    information to the receiving system. TempleReady processes submission
    records to determine which temple the cleared records should be directed
    to. The submission record is also used for communication between
    Ancestral File download requests and TempleReady. Each GEDCOM transmission
    file should have only one submission record. Multiple submissions are
    handled by creating separate GEDCOM transmissionfiles. }
  TGEDCOMSubmissionRecord = class(TGEDCOMRecord)
  private
    function GetSubmitter: TGEDCOMPointer;
    function GetStringTag(Index: Integer): string;
    procedure SetStringTag(Index: Integer; const Value: string);
    function GetIntegerTag(Index: Integer): Integer;
    procedure SetIntegerTag(Index: Integer; Value: Integer);
    function GetOrdinanceProcessFlag: TGEDCOMOrdinanceProcessFlag;
    procedure SetOrdinanceProcessFlag(const Value: TGEDCOMOrdinanceProcessFlag);
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); reintroduce; virtual;

    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;

    property AutomatedRecordID: string index 3 read GetStringTag write SetStringTag;
    property FamilyFileName: string index 1 read GetStringTag write SetStringTag;
    property GenerationsOfAncestors: Integer index 1 read GetIntegerTag write SetIntegerTag;
    property GenerationsOfDescendants: Integer index 2 read GetIntegerTag write SetIntegerTag;
    property OrdinanceProcessFlag: TGEDCOMOrdinanceProcessFlag
      read GetOrdinanceProcessFlag write SetOrdinanceProcessFlag;
    property Submitter: TGEDCOMPointer read GetSubmitter;
    property TempleCode: string index 2 read GetStringTag write SetStringTag;

    property Notes;
    property NotesCount;
  end;

  { The submitter record identifies an individual or organization that
    contributed information contained in the GEDCOM transmission. All records
    in the transmission are assumed to be submitted by the SUBMITTER referenced
    in the HEADer, unless a SUBMitter reference inside a specific record points
    at a different SUBMITTER record. }
  TGEDCOMSubmitterRecord = class(TGEDCOMRecord)
  private
    FLanguages: TGEDCOMList;
    function GetStringTag(Index: Integer): string;
    procedure SetStringTag(Index: Integer; const Value: string);
    function GetName: TGEDCOMPersonalName;
    function GetAddress: TGEDCOMAddress;
    function GetLanguages(Index: Integer): string;
    function GetLanguagesCount: Integer;
    procedure SetLanguages(Index: Integer; const Value: string);
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); reintroduce; virtual;
    destructor Destroy; override;

    function AddLanguage(Value: TGEDCOMTag): TGEDCOMTag;
    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    procedure Clear(); override;
    function IsEmpty(): Boolean; override;
    procedure ReplaceXRefs(aMap: TXRefReplaceMap); override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;

    property Address: TGEDCOMAddress read GetAddress;
    property AutomatedRecordID: string index 2 read GetStringTag write SetStringTag;
    property Languages[Index: Integer]: string read GetLanguages write SetLanguages;
    property LanguagesCount: Integer read GetLanguagesCount;
    property Name: TGEDCOMPersonalName read GetName;
    property RegisteredReference: string index 1 read GetStringTag write SetStringTag;

    property MultimediaLinks;
    property MultimediaLinksCount;
    property Notes;
    property NotesCount;
  end;

  TGEDCOMNotes = class(TGEDCOMPointer)
  private
    FNotes: TStrings;
    function GetIsPointer: Boolean;
    function GetNotes: TStrings;
    procedure SetNotes(const Value: TStrings);
  protected
    function GetStringValue: string; override;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;
    destructor Destroy; override;

    procedure Clear(); override;
    function IsEmpty(): Boolean; override;
    function ParseString(const AString: string): string; override;

    property IsPointer: Boolean read GetIsPointer;
    property Notes: TStrings read GetNotes write SetNotes;
  end;

  TGEDCOMSourceCitation = class(TGEDCOMPointer)
  private
    FDescription: TStrings;
    function GetIsPointer: Boolean;
    function GetDescription: TStrings;
    procedure SetDescription(const Value: TStrings);
    function GetPage: string;
    procedure SetPage(const Value: string);
  protected
    function GetStringValue: string; override;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;
    destructor Destroy; override;

    procedure Clear(); override;
    function IsEmpty(): Boolean; override;
    function ParseString(const AString: string): string; override;

    property IsPointer: Boolean read GetIsPointer;
    property Description: TStrings read GetDescription write SetDescription;
    property Page: string read GetPage write SetPage; 
  end;

  TGEDCOMRepositoryCitation = class(TGEDCOMPointer)
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;
  end;

  TGEDCOMMultimediaLink = class(TGEDCOMPointer)
  private
    FFileReferences: TGEDCOMList;
    function GetIsPointer: Boolean;
    function GetStringTag(Index: Integer): string;
    procedure SetStringTag(Index: Integer; const Value: string);
    function GetFileReferences(Index: Integer): TGEDCOMFileReference;
    function GetFileReferencesCount: Integer;
  protected
    function GetStringValue: string; override;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;
    destructor Destroy; override;

    function AddFileReference(AFileReference: TGEDCOMFileReference): TGEDCOMFileReference;
    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    procedure Clear(); override;
    function IsEmpty(): Boolean; override;
    function ParseString(const AString: string): string; override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;
    procedure SaveToStream(AStream: TStream); override;

    property FileReferences[Index: Integer]: TGEDCOMFileReference read GetFileReferences;
    property FileReferencesCount: Integer read GetFileReferencesCount;
    property IsPointer: Boolean read GetIsPointer;
    property Title: string index 1 read GetStringTag write SetStringTag;
  end;

  TGEDCOMPointerWithNotes = class(TGEDCOMPointer)
  private
    FNotes: TGEDCOMList;
    function GetNotes(Index: Integer): TGEDCOMNotes;
    function GetNotesCount: Integer;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;
    destructor Destroy; override;

    function AddNotes(ANotes: TGEDCOMNotes): TGEDCOMNotes;
    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    procedure Clear(); override;
    function IsEmpty(): Boolean; override;
    procedure ReplaceXRefs(aMap: TXRefReplaceMap); override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;

    property Notes[Index: Integer]: TGEDCOMNotes read GetNotes;
    property NotesCount: Integer read GetNotesCount;
  end;

  { }
  TGEDCOMChildToFamilyLink = class(TGEDCOMPointerWithNotes)
  private
    function GetPedigreeLinkageType: TGEDCOMPedigreeLinkageType;
    procedure SetPedigreeLinkageType(const Value: TGEDCOMPedigreeLinkageType);
    function GetChildLinkageStatus: TGEDCOMChildLinkageStatus;
    procedure SetChildLinkageStatus(const Value: TGEDCOMChildLinkageStatus);
    function GetFamily: TGEDCOMFamilyRecord;
    procedure SetFamily(const Value: TGEDCOMFamilyRecord);
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;

    property ChildLinkageStatus: TGEDCOMChildLinkageStatus
      read GetChildLinkageStatus write SetChildLinkageStatus;
    property PedigreeLinkageType: TGEDCOMPedigreeLinkageType
      read GetPedigreeLinkageType write SetPedigreeLinkageType;

    property Family: TGEDCOMFamilyRecord read GetFamily write SetFamily;
  end;

  { }
  TGEDCOMSpouseToFamilyLink = class(TGEDCOMPointerWithNotes)
  private
    function GetFamily: TGEDCOMFamilyRecord;
    procedure SetFamily(const Value: TGEDCOMFamilyRecord);
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;

    property Family: TGEDCOMFamilyRecord read GetFamily write SetFamily;
  end;

  { The association pointer only associates INDIvidual records to INDIvidual
    records. }
  TGEDCOMAssociation = class(TGEDCOMPointerWithNotes)
  private
    FSourceCitations: TGEDCOMList;
    function GetRelation: string;
    procedure SetRelation(const Value: string);
    function GetSourceCitations(Index: Integer): TGEDCOMSourceCitation;
    function GetSourceCitationsCount: Integer;
    function GetIndividual: TGEDCOMIndividualRecord;
    procedure SetIndividual(const Value: TGEDCOMIndividualRecord);
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;
    destructor Destroy; override;

    function AddSourceCitation(ASourceCitation: TGEDCOMSourceCitation): TGEDCOMSourceCitation;
    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    procedure Clear; override;
    function IsEmpty(): Boolean; override;
    procedure ReplaceXRefs(aMap: TXRefReplaceMap); override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;

    property Individual: TGEDCOMIndividualRecord read GetIndividual write SetIndividual;
    property Relation: string read GetRelation write SetRelation;
    property SourceCitations[Index: Integer]: TGEDCOMSourceCitation read GetSourceCitations;
    property SourceCitationsCount: Integer read GetSourceCitationsCount;
  end;

  TGEDCOMCustomDate = class(TGEDCOMTag)
  protected
    function GetDateTime: TDateTime; virtual; abstract;
    procedure SetDateTime(Value: TDateTime); virtual; abstract;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;
    constructor CreateFromString(AOwner, AParent: TGEDCOMObject; const AString: string); reintroduce; virtual;

    property Date: TDateTime read GetDateTime write SetDateTime;
  end;

  TGEDCOMDate = class(TGEDCOMCustomDate)
  private
    FDateCalendar: TGEDCOMCalendar;
    FYear: Integer;
    FYearBC: Boolean;
    FYearModifier: string;
    FMonth: string[4];
    FDay: Word;
    FDateFormat: TGEDCOMDateFormat;
    function GetMonth: string;
    procedure SetMonth(const Value: string);
  protected
    function DayString(NoDelimiter: Boolean = False): string;
    function EscapeString(NoDelimiter: Boolean = False; AllwaysShowEscape: Boolean = False): string;
    function MonthString(NoDelimiter: Boolean = False): string;
    function YearGregString(NoDelimiter: Boolean = True): string;
    function YearString(NoDelimiter: Boolean = True): string;
    function ExtractEscape(const S: string): string;
    function ExtractDay(const S: string): string;
    function ExtractDelimiterEx(const S: string): string;
    function ExtractMonth(const S: string): string;
    function ExtractYear(const S: string): string;
    function GetStringValue: string; override;
    function GetDateTime: TDateTime; override;
    procedure SetDateTime(ADateTime: TDateTime); override;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); reintroduce; overload; override;

    procedure Assign(Source: TGEDCOMCustomTag); override;

    // Exact date (Gregorian date)
    constructor Create(AOwner, AParent: TGEDCOMObject; ADateTime: TDateTime); reintroduce; overload; virtual;

    procedure SetDate(ADay, AMonth, AYear: Word);
    procedure GetDate(var AYear: Integer; var AMonth, ADay: Word);

    // Misc dates
    procedure SetGregorian(const ADay: Word; const AMonth: string;
      AYear: Integer; const AYearModifier: string = ''; BC: Boolean = False);
    procedure SetJulian(const ADay: Word; const AMonth: string;
      AYear: Word; BC: Boolean = False);
    procedure SetHebrew(const ADay: Word; const AMonth: string;
      AYear: Integer; BC: Boolean = False);
    procedure SetFrench(const ADay: Word; const AMonth: string;
      AYear: Word; BC: Boolean = False);
    procedure SetRoman(const ADay: Word; const AMonth: string;
      AYear: Word; BC: Boolean = False);
    procedure SetUnknown(const ADay: Word; const AMonth: string;
      AYear: Word; BC: Boolean = False);

    procedure Clear(); override;
    function IsEmpty(): Boolean; override;
    function ParseString(const AString: string): string; override;

    property Date: TDateTime read GetDateTime write SetDateTime;
    property DateCalendar: TGEDCOMCalendar read FDateCalendar;
    property Year: Integer read FYear write FYear;
    property YearBC: Boolean read FYearBC write FYearBC;
    property YearModifier: string read FYearModifier write FYearModifier;
    property Month: string read GetMonth write SetMonth;
    property Day: Word read FDay write FDay;
  end;

  TGEDCOMDateExact = class(TGEDCOMDate)
  public
    constructor Create(AOwner, AParent: TGEDCOMObject; ADateTime: TDateTime); reintroduce; overload; virtual;
    constructor CreateNow(AOwner, AParent: TGEDCOMObject); virtual;
  end;

  TGEDCOMDatePeriod = class(TGEDCOMCustomDate)
  private
    FDateFrom: TGEDCOMDate;
    FDateTo: TGEDCOMDate;
  protected
    function GetStringValue: string; override;
    function GetDateTime: TDateTime; override;
    procedure SetDateTime(ADateTime: TDateTime); override;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;
    destructor Destroy; override;

    procedure Clear(); override;
    function IsEmpty(): Boolean; override;
    function ParseString(const S: string): string; override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;

    property DateFrom: TGEDCOMDate read FDateFrom;
    property DateTo: TGEDCOMDate read FDateTo;
  end;

  TGEDCOMDateRange = class(TGEDCOMCustomDate)
  private
    FDateAfter: TGEDCOMDate;
    FDateBefore: TGEDCOMDate;
  protected
    function GetStringValue: string; override;
    function GetDateTime: TDateTime; override;
    procedure SetDateTime(ADateTime: TDateTime); override;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;
    destructor Destroy; override;

    constructor CreateAfter(AOwner, AParent: TGEDCOMObject; ADate: TGEDCOMDate);
    constructor CreateBefore(AOwner, AParent: TGEDCOMObject; ADate: TGEDCOMDate);
    constructor CreateBetween(AOwner, AParent: TGEDCOMObject; Date1, Date2: TGEDCOMDate);

    procedure Clear; override;
    function IsEmpty: Boolean; override;
    function ParseString(const S: string): string; override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;

    property After: TGEDCOMDate read FDateAfter;
    property Before: TGEDCOMDate read FDateBefore;
  end;

  TGEDCOMDateApproximated = class(TGEDCOMDate)
  private
    FDateApproximated: TGEDCOMApproximated;
  protected
    function GetStringValue: string; override;
    function ApproximatedString(NoDelimiter: Boolean = False): string;
    function ExtractApproximated(const S: string): string;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;

    function ParseString(const S: string): string; override;

    property Approximated: TGEDCOMApproximated
      read FDateApproximated write FDateApproximated;
  end;

  TGEDCOMDateInterpreted = class(TGEDCOMDate)
  private
    FDatePhrase: string;
    procedure SetDatePhrase(const Value: string);
  protected
    function GetStringValue: string; override;
    function ExtractPhrase(const S: string): string;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;
    constructor CreateFromDate(AOwner, AParent: TGEDCOMObject; ADate: TGEDCOMDate;
      const APhrase: string);

    function ParseString(const S: string): string; override;

    property DatePhrase: string read FDatePhrase write SetDatePhrase;
  end;

  TGEDCOMDatePhrase = class(TGEDCOMCustomDate)
  private
    procedure SetDatePhrase(const Value: string);
  protected
    function GetStringValue: string; override;
    function ExtractPhrase(const S: string): string;
    function GetDateTime: TDateTime; override;
    procedure SetDateTime(ADateTime: TDateTime); override;
  public
    constructor CreateFromText(AOwner, AParent: TGEDCOMObject; const S: string); virtual;

    procedure Clear; override;
    function IsEmpty: Boolean; override;
    function ParseString(const S: string): string; override;

    property DatePhrase: string read GetStringValue write SetStringValue;
  end;

  TGEDCOMDateValue = class(TGEDCOMCustomDate)
  private
    FValue: TGEDCOMCustomDate;
  protected
    function GetStringValue: string; override;
    function GetDateTime: TDateTime; override;
    procedure SetDateTime(ADateTime: TDateTime); override;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;

    procedure Assign(Source: TGEDCOMCustomTag); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    function ParseString(const S: string): string; override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;

    property Value: TGEDCOMCustomDate read FValue;
  end;

  TGEDCOMTime = class(TGEDCOMTag)
  private
    FHour: Word;
    FMinutes: Word;
    FSeconds: Word;
    FFraction: Word;
    function GetValue: TDateTime;
    procedure SetValue(AValue: TDateTime);
  protected
    function GetStringValue: string; override;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;

    procedure Clear; override;
    function IsEmpty: Boolean; override;
    function ParseString(const AString: string): string; override;

    property Fraction: Word read FFraction write FFraction;
    property Hour: Word read FHour write FHour;
    property Minutes: Word read FMinutes write FMinutes;
    property Seconds: Word read FSeconds write FSeconds;
    property Value: TDateTime read GetValue write SetValue;
    property Time: TDateTime read GetValue write SetValue;
  end;

  { The address structure should be formed as it would appear on a mailing
    label using the ADDR and the CONT lines to form the address structure.
    The ADDR and CONT lines are required for any address. The additional
    subordinate address tags such as STAE and CTRY are provided to be used
    by systems that have structured their addresses for indexing and sorting.
    For backward compatibility these lines are not to be used in lieu of the
    required ADDR.and CONT line structure. }
  TGEDCOMAddress = class(TGEDCOMTag)
  private
    FAddress: TStrings;
    FPhoneList: TGEDCOMList;
    FEmailList: TGEDCOMList;
    FFaxList: TGEDCOMList;
    FWWWList: TGEDCOMList;
    function GetAddress: TStrings;
    function GetEmailAddresses(Index: Integer): string;
    function GetEmailAddressesCount: Integer;
    function GetFaxNumbers(Index: Integer): string;
    function GetFaxNumbersCount: Integer;
    function GetPhoneNumbers(Index: Integer): string;
    function GetPhoneNumbersCount: Integer;
    function GetStringTag(Index: Integer): string;
    function GetWebPages(Index: Integer): string;
    function GetWebPagesCount: Integer;
    procedure SetAddress(const Value: TStrings);
    procedure SetEmailAddresses(Index: Integer; const Value: string);
    procedure SetFaxNumbers(Index: Integer; const Value: string);
    procedure SetPhoneNumbers(Index: Integer; const Value: string);
    procedure SetStringTag(Index: Integer; const Value: string);
    procedure SetWebPages(Index: Integer; const Value: string);
  protected
    procedure SaveTagsToStream(AStream: TStream; const ATagSorting: array of string); override;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); override;
    destructor Destroy; override;

    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    procedure Clear(); override;
    function IsEmpty(): Boolean; override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;

    procedure DeletePhoneNumber(Index: Integer);
    procedure DeleteEmail(Index: Integer);
    procedure DeleteWebPage(Index: Integer);

    property Address: TStrings read GetAddress write SetAddress;
    property AddressLine1: string index 1 read GetStringTag write SetStringTag;
    property AddressLine2: string index 2 read GetStringTag write SetStringTag;
    property AddressLine3: string index 3 read GetStringTag write SetStringTag;

    property AddressCity: string index 4 read GetStringTag write SetStringTag;
    property AddressState: string index 5 read GetStringTag write SetStringTag;
    property AddressPostalCode: string index 6 read GetStringTag write SetStringTag;
    property AddressCountry: string index 7 read GetStringTag write SetStringTag;

    property PhoneNumbers[Index: Integer]: string read GetPhoneNumbers write SetPhoneNumbers;
    property PhoneNumbersCount: Integer read GetPhoneNumbersCount;
    property EmailAddresses[Index: Integer]: string read GetEmailAddresses write SetEmailAddresses;
    property EmailAddressesCount: Integer read GetEmailAddressesCount;
    property FaxNumbers[Index: Integer]: string read GetFaxNumbers write SetFaxNumbers;
    property FaxNumbersCount: Integer read GetFaxNumbersCount;
    property WebPages[Index: Integer]: string read GetWebPages write SetWebPages;
    property WebPagesCount: Integer read GetWebPagesCount;
  end;

  { }
  TGEDCOMGroupRecord = class(TGEDCOMRecord)
  private
    FMembers: TGEDCOMList;
    function GetName(): string;
    procedure SetName(const Value: string);
    function GetMember(Index: Integer): TGEDCOMPointer;
    function GetMembersCount: Integer;
  public
    constructor Create(AOwner, AParent: TGEDCOMObject); reintroduce; virtual;
    destructor Destroy; override;

    function AddMember(Value: TGEDCOMPointer): TGEDCOMPointer;
    function AddTag(const ATag: string; const AValue: string = '';
      AClass: TGEDCOMTagClass = nil): TGEDCOMTag; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure ReplaceXRefs(aMap: TXRefReplaceMap); override;
    procedure ResetOwner(AOwner: TGEDCOMObject); override;
    procedure SaveToStream(AStream: TStream); override;

    function IndexOfMember(aMember: TGEDCOMIndividualRecord): Integer;
    procedure RemoveMember(aIndex: Integer);

    property Members[Index: Integer]: TGEDCOMPointer read GetMember;
    property MembersCount: Integer read GetMembersCount;
    property Name: string read GetName write SetName;

    property MultimediaLinks;
    property MultimediaLinksCount;
    property Notes;
    property NotesCount;
  end;

{==============================================================================}

function StrToGEDCOMMonth(const S: string): string;
function StrToGEDCOMMonthFrench(const S: string): string;
function StrToGEDCOMMonthHebrew(const S: string): string;
function IntToGEDCOMMonth(M: Word): string;
function IntToGEDCOMMonthFrench(M: Word): string;
function IntToGEDCOMMonthHebrew(M: Word): string;
function GEDCOMMonthToInt(const S: string): Word;
function GEDCOMMonthFrenchToInt(const S: string): Word;
function GEDCOMMonthHebrewToInt(const S: string): Word;

function IsDigit(C: Char): Boolean;
function IsDigits(const S: string): Boolean;

function ExtractDelimiter(const S: string; Max: Integer = 0): string;
function ExtractDotDelimiter(const S: string; Max: Integer = 0): string;
function ExtractNumber(const S: string; var N: Integer;
  NoException: Boolean = False; ADefault: Integer = 0): string;
function ExtractString(const S: string; var AString: string;
  const ADefault: string = ''): string;
function ExtractXRef(const S: string; var AXRef: string;
  NoException: Boolean = False; const ADefault: string = ''): string;

function CleanXRef(const XRef: string): string;
function EncloseXRef(XRef: string): string;

function CreateGEDCOMTag(AOwner, AParent: TGEDCOMObject; const ATag: string;
  const AValue: string = ''): TGEDCOMTag;

function GetSignByRecord(aRecord: TGEDCOMRecord): string;

{==============================================================================}

implementation

uses
  Windows, bsMiscUtils, GKCommon;

resourcestring
  ATagHasInvalidType = 'The tag %s is of type %s, but type %s was expected';
  EMaxPhoneNumbers = 'The maximum number of phone numbers is %d';
  EMaxEmailAddresses = 'The maximum number of email addresses is %d';
  EMaxFaxNumbers = 'The maximum number of fax numbers is %d';
  EMaxWebPages = 'The maximum number of web page addresses is %d';
  EMaxLanguages = 'The maximum number of languages is %d';

  SNotAValidMonth = 'The string %s is not a valid month identifier';
  SNotAValidFrenchMonth = 'The string %s is not a valid French month identifier';
  SNotAValidHebrewMonth = 'The string %s is not a valid Hebrew month identifier';
  SNotAValidNumber = 'The string %s doesn''t start with a valid number';
  SXRefNotTerminated = 'The string %s contains an unterminated XRef pointer';
  SXRefExpected = 'The string %s is expected to start with an XRef pointer';

procedure StrDelete(var Dest: string; Index, Count: Integer);
begin
  {$IFDEF DELPHI_NET}Borland.Delphi.{$ENDIF}System.Delete(Dest, Index, Count);
end;

{==============================================================================}

function StrToGEDCOMMonth(const S: string): string;
var
  SU: string[3];
  Month: Integer;
begin
  if Length(S) <> 3
  then raise EGEDCOMException.CreateFmt(SNotAValidMonth, [S]);

  SU := UpperCase(S);
  for Month := 1 to 12 do
    if (GEDCOMMonthArray[Month] = SU) then begin
      Result := GEDCOMMonthArray[Month];
      Exit;
    end;

  raise EGEDCOMException.CreateFmt(SNotAValidMonth, [S]);
end;

function StrToGEDCOMMonthFrench(const S: string): string;
var
  SU: string[4];
  Month: Integer;
begin
  if Length(S) <> 4
  then raise EGEDCOMException.CreateFmt(SNotAValidFrenchMonth, [S]);

  SU := UpperCase(S);
  for Month := 1 to 13 do
    if (GEDCOMMonthFrenchArray[Month] = SU) then begin
      Result := GEDCOMMonthFrenchArray[Month];
      Exit;
    end;

  raise EGEDCOMException.CreateFmt(SNotAValidFrenchMonth, [S]);
end;

function StrToGEDCOMMonthHebrew(const S: string): string;
var
  SU: string[3];
  Month: Integer;
begin
  if Length(S) <> 3
  then raise EGEDCOMException.CreateFmt(SNotAValidHebrewMonth, [S]);

  SU := UpperCase(S);
  for Month := 1 to 13 do
    if (GEDCOMMonthHebrewArray[Month] = SU) then begin
      Result := GEDCOMMonthHebrewArray[Month];
      Exit;
    end;

  raise EGEDCOMException.CreateFmt(SNotAValidHebrewMonth, [S]);
end;

function IntToGEDCOMMonth(M: Word): string;
begin
  Result := GEDCOMMonthArray[M];
end;

function IntToGEDCOMMonthFrench(M: Word): string;
begin
  Result := GEDCOMMonthFrenchArray[M];
end;

function IntToGEDCOMMonthHebrew(M: Word): string;
begin
  Result := GEDCOMMonthHebrewArray[M];
end;

function GEDCOMMonthToInt(const S: string): Word;
var
  M: Word;
  SU: string;
begin
  Result := 0;
  SU := UpperCase(S);
  for M := Low(GEDCOMMonthArray) to High(GEDCOMMonthArray) do
    if (GEDCOMMonthArray[M] = SU) then begin
      Result := M;
      Break;
    end;
end;

function GEDCOMMonthFrenchToInt(const S: string): Word;
var
  M: Word;
  SU: string;
begin
  Result := 0;
  SU := UpperCase(S);
  for M := Low(GEDCOMMonthFrenchArray) to High(GEDCOMMonthFrenchArray) do
    if (GEDCOMMonthFrenchArray[M] = SU) then begin
      Result := M;
      Break;
    end;
end;

function GEDCOMMonthHebrewToInt(const S: string): Word;
var
  M: Word;
  SU: string;
begin
  Result := 0;
  SU := UpperCase(S);
  for M := Low(GEDCOMMonthHebrewArray) to High(GEDCOMMonthHebrewArray) do
    if (GEDCOMMonthHebrewArray[M] = SU) then begin
      Result := M;
      Break;
    end;
end;

function IsDigit(C: Char): Boolean;
{ Deze functie geeft True terug als het teken C in de reeks '0' tot '9' zit. }
begin
  Result := C in ['0'..'9'];
end;

function IsDigits(const S: string): Boolean;
{ Deze functie geeft True terug als de string S niet leeg is en alle tekens in
  de string in de reeks '0' tot '9' zitten. }
var
  I: Integer;
begin
  I := 1;
  while (I <= Length(S)) and (S[I] in ['0'..'9']) do
    Inc(I);
  Result := (Length(S) > 0) and (I > Length(S));
end;

function ExtractDelimiter(const S: string; Max: Integer = 0): string;
begin
  Result := S;

  while (Length(Result) > 0) and (Result[1] = GEDCOMDelimiter) do begin
    Delete(Result, 1, 1);

    if (Max > 0) then begin
      Max := Max - 1;
      if (Max = 0) then Break;
    end;
  end;
end;

function ExtractDotDelimiter(const S: string; Max: Integer = 0): string;
begin
  Result := S;

  while (Length(Result) > 0) and (Result[1] = '.') do begin
    Delete(Result, 1, 1);

    if (Max > 0) then begin
      Max := Max - 1;
      if (Max = 0) then Break;
    end;
  end;
end;

function ExtractNumber(const S: string; var N: Integer;
  NoException: Boolean; ADefault: Integer): string;
var
  I: Integer;
begin
  Result := S;
  I := 0;
  while (I < Length(Result)) and IsDigit(Result[I+1]) do
    Inc(I);

  if (I > 0) then begin
    N := StrToInt(Copy(Result, 1, I));
    Delete(Result, 1, I);
  end
  else
  if NoException
  then N := ADefault
  else raise EGEDCOMException.CreateFmt(SNotAValidNumber, [S]);
end;

function ExtractString(const S: string; var AString: string;
  const ADefault: string): string;
var
  I: Integer;
begin
  Result := S;
  I := 0;
  while (I < Length(Result)) and (Result[I+1] <> GEDCOMDelimiter) do
    Inc(I);

  if (I > 0) then begin
    AString := Copy(Result, 1, I);
    Delete(Result, 1, I);
  end else AString := ADefault;
end;

function ExtractXRef(const S: string; var AXRef: string;
  NoException: Boolean; const ADefault: string): string;
var
  P: Integer;
begin
  Result := S;

  if (Length(Result) > 0) and (Result[1] = GEDCOMPointerDelimiter) then begin
    P := Pos(GEDCOMPointerDelimiter, Copy(Result, 2, MaxInt));
    if (P > 0) then begin
      AXRef := Copy(Result, 2, P - 1);
      Delete(Result, 1, P + 1);
    end
    else
    if NoException
    then AXRef := ADefault
    else raise EGEDCOMException.CreateFmt(SXRefNotTerminated, [S]);
  end
  else
  if NoException
  then AXRef := ADefault
  else raise EGEDCOMException.CreateFmt(SXRefExpected, [S]);
end;

function CleanXRef(const XRef: string): string;
begin
  Result := XRef;
  if (XRef = '') then Exit;

  if (Result[1] = GEDCOMPointerDelimiter)
  then StrDelete(Result, 1, 1);

  if (Length(Result) > 0) and (Result[Length(Result)] = GEDCOMPointerDelimiter)
  then StrDelete(Result, Length(Result), 1);
end;

function EncloseXRef(XRef: string): string;
begin
  if (XRef <> '') then begin
    if (Copy(XRef, 1, 1) <> GEDCOMPointerDelimiter)
    then XRef := GEDCOMPointerDelimiter + XRef;

    if (Copy(XRef, Length(XRef), 1) <> GEDCOMPointerDelimiter)
    then XRef := XRef + GEDCOMPointerDelimiter;
  end;

  Result := XRef;
end;

function CreateGEDCOMTag(AOwner, AParent: TGEDCOMObject;
  const ATag, AValue: string): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);
  if (SU = 'DATE')
  then Result := TGEDCOMDateValue.CreateFromString(AOwner, AParent, AValue)
  else
  if (SU = 'TIME')
  then Result := TGEDCOMTime.CreateTag(AOwner, AParent, 'TIME', AValue)
  else
  if (SU = 'ADDR')
  then Result := TGEDCOMAddress.CreateTag(AOwner, AParent, 'ADDR', AValue)
  else Result := TGEDCOMTag.CreateTag(AOwner, AParent, ATag, AValue);
end;

function GetSignByRecord(aRecord: TGEDCOMRecord): string;
begin
  if (aRecord is TGEDCOMFamilyRecord) then Result := 'F' {std:checked}
  else
  if (aRecord is TGEDCOMIndividualRecord) then Result := 'I' {std:checked}
  else
  if (aRecord is TGEDCOMMultimediaRecord) then Result := 'O' {std:checked}
  else
  if (aRecord is TGEDCOMNoteRecord) then Result := 'N' {std:checked}
  else
  if (aRecord is TGEDCOMRepositoryRecord) then Result := 'R' {?}
  else
  if (aRecord is TGEDCOMSourceRecord) then Result := 'S' {std:checked}
  else
  if (aRecord is TGEDCOMSubmitterRecord) then Result := 'SUB' {std:checked}
  else
  if (aRecord is TGEDCOMGroupRecord) then Result := 'G' {nonstd:zsv}
  else
    Result := '';
end;

{==============================================================================}

procedure TXRefReplaceMap.AddXRef(rec: TGEDCOMRecord; oldXRef, newXRef: string);
var
  Len: Integer;
begin
  Len := Length(FList);
  SetLength(FList, Len + 1);
  FList[Len].rec := rec;
  FList[Len].oldXRef := oldXRef;
  FList[Len].newXRef := newXRef;

  //s := s + #13#10 + oldXRef + '=' + newXRef;
end;

function TXRefReplaceMap.FindNewXRef(oldXRef: string): string;
var
  i: Integer;
begin
  Result := oldXRef;

  for i := 0 to Length(FList) - 1 do
    if (CleanXRef(FList[i].oldXRef) = CleanXRef(oldXRef)) then begin
      Result := FList[i].newXRef;
      Break;
    end;
end;

function TXRefReplaceMap.GetCount: Integer;
begin
  Result := Length(FList);
end;

function TXRefReplaceMap.GetRecord(Index: Integer): TXRefRec;
begin
  Result := FList[Index];
end;

{==============================================================================}

{ TGEDCOMTree }

constructor TGEDCOMTree.Create;
begin
  inherited Create;
  FRecords := TGEDCOMList.Create(Self);
  FHeader := TGEDCOMHeader.Create(Self, Self);

  FXRefIndex := TStringList.Create;
  FXRefIndex.Sorted := True;
end;

destructor TGEDCOMTree.Destroy;
begin
  FXRefIndex.Free;

  FHeader.Free;
  FRecords.Free;
  inherited Destroy;
end;

procedure TGEDCOMTree.Clear();
begin
  FRecords.Clear;
  FHeader.Clear;
  XRefIndex_Clear();
end;

procedure TGEDCOMTree.XRefIndex_Add(const XRef: string; ARecord: TGEDCOMRecord);
begin
  if (XRef <> '') and (ARecord <> nil)
  then FXRefIndex.AddObject(XRef, ARecord);
end;

procedure TGEDCOMTree.XRefIndex_AddRecord(ARecord: TGEDCOMRecord);
var
  Index: Integer;
begin
  Index := FXRefIndex.IndexOfObject(ARecord);
  if (Index = -1) and (ARecord.XRef <> '')
  then XRefIndex_Add(ARecord.XRef, ARecord);
end;

procedure TGEDCOMTree.XRefIndex_Clear();
begin
  FXRefIndex.Clear;
end;

procedure TGEDCOMTree.XRefIndex_DeleteRecord(ARecord: TGEDCOMRecord);
var
  Index: Integer;
begin
  Index := FXRefIndex.IndexOfObject(ARecord);
  if (Index >= 0)
  then FXRefIndex.Delete(Index);
end;

function TGEDCOMTree.XRefIndex_Find(const XRef: string): TGEDCOMRecord;
var
  Index: Integer;
begin
  Index := FXRefIndex.IndexOf(XRef);
  if (Index >= 0)
  then Result := TGEDCOMRecord(FXRefIndex.Objects[Index])
  else Result := nil;
end;

function TGEDCOMTree.XRefIndex_NewXRef(Sender: TGEDCOMRecord): string;
var
  I: Integer;
  sign: string;
begin
  sign := GetSignByRecord(Sender);

  I := 1;
  while FXRefIndex.IndexOf(sign + IntToStr(I)) >= 0 do Inc(I);
  Result := sign + IntToStr(I);
end;

procedure TGEDCOMTree.SetXRef(Sender: TGEDCOMRecord; const XRef: string);
var
  Index: Integer;
begin
  Index := FXRefIndex.IndexOfObject(Sender);
  if (Index >= 0) then begin
    FXRefIndex.Sorted := False;
    FXRefIndex[Index] := XRef;
    FXRefIndex.Sorted := True;
  end else XRefIndex_Add(XRef, Sender);
end;

function TGEDCOMTree.AddRecord(ARecord: TGEDCOMRecord): TGEDCOMRecord;
begin
  Result := ARecord;
  FRecords.Add(ARecord);

  if (ARecord.XRef <> '')
  then XRefIndex_AddRecord(ARecord);
end;

procedure TGEDCOMTree.Delete(Index: Integer);
begin
  XRefIndex_DeleteRecord(Records[Index]);
  FRecords.Delete(Index);
end;

procedure TGEDCOMTree.DeleteRecord(Sender: TGEDCOMRecord);
begin
  XRefIndex_DeleteRecord(Sender);
  FRecords.DeleteObject(Sender);
end;

function TGEDCOMTree.Extract(Index: Integer): TGEDCOMRecord;
begin
  XRefIndex_DeleteRecord(GetRecords(Index));
  Result := TGEDCOMRecord(FRecords.Extract(Index));
end;

function TGEDCOMTree.GetRecords(Index: Integer): TGEDCOMRecord;
begin
  Result := TGEDCOMRecord(FRecords[Index]);
end;

procedure TGEDCOMTree.LoadFromFile(const aFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(aFileName, fmOpenRead);
  try
    Clear;
    LoadFromStream(fs);
    FHeader.CharacterSet := csASCII;
  finally
    fs.Destroy;
  end;
end;

procedure TGEDCOMTree.LoadFromStream(AStream: TStream);
var
  SL: TStringList;
  ALevel: Integer;
  AXRef: string;
  ATag: string;
  AValue: string;
  I: Integer;
  Line, S: string;
  CurRecord: TGEDCOMCustomRecord;
  CurTag: TGEDCOMTag;
begin
  FState := osLoading;
  SL := TStringList.Create;
  try
    SL.LoadFromStream(AStream);

    CurRecord := nil;
    CurTag := nil;

    for I := 0 to SL.Count - 1 do begin
      S := SL[I];

      // Delete spaces and tabs from the start of the line. This could be the
      // result of formatting the gedcom file, so every line has been indented
      // according to the level.
      while (Length(S) > 0) and ((S[1] = #32) or (S[1] = #9)) do
        StrDelete(S, 1, 1);

      // Skip empty rows.
      if (S = '') then
        Continue;

      // Parse the row
      Line := S;
      try
        S := ExtractNumber(S, ALevel);
        S := ExtractDelimiter(S);
        S := ExtractXRef(S, AXRef, True, '');
        S := ExtractDelimiter(S);
        S := ExtractString(S, ATag);
        ATag := UpperCase(ATag);
        // Maximaal 1 delimiter parsen, want de eerste spatie in de waarde kan
        // onderdeel zijn van de waarde.
        S := ExtractDelimiter(S, 1);
        AValue := S;
      except
        on E: EGEDCOMException do
          raise EGEDCOMException.Create('Syntax error in line '+IntToStr(I+1)+'.'#13+E.Message);
      else
        raise;
      end;

      // Is this a level 0 line?
      if (ALevel = 0) then begin
        if (ATag = 'HEAD')
        then CurRecord := FHeader
        else
        if (ATag = 'TRLR')
        then Break
        else
        if (ATag = 'FAM')
        then CurRecord := AddRecord(TGEDCOMFamilyRecord.Create(Self, Self))
        else
        if (ATag = 'INDI')
        then CurRecord := AddRecord(TGEDCOMIndividualRecord.Create(Self, Self))
        else
        if (ATag = 'OBJE')
        then CurRecord := AddRecord(TGEDCOMMultimediaRecord.Create(Self, Self))
        else
        if (ATag = 'NOTE')
        then CurRecord := AddRecord(TGEDCOMNoteRecord.Create(Self, Self))
        else
        if (ATag = 'REPO')
        then CurRecord := AddRecord(TGEDCOMRepositoryRecord.Create(Self, Self))
        else
        if (ATag = 'SOUR')
        then CurRecord := AddRecord(TGEDCOMSourceRecord.Create(Self, Self))
        else
        if (ATag = 'SUBN')
        then CurRecord := AddRecord(TGEDCOMSubmissionRecord.Create(Self, Self))
        else
        if (ATag = 'SUBM')
        then CurRecord := AddRecord(TGEDCOMSubmitterRecord.Create(Self, Self))

        // zsv
        else
        if (ATag = '_GROUP')
        then CurRecord := AddRecord(TGEDCOMGroupRecord.Create(Self, Self))
        // zsv

        else CurRecord := nil;

        if (CurRecord <> nil) and (AXRef <> '')
        then CurRecord.XRef := AXRef;
        CurTag := nil;
      end
      else
      if (CurRecord <> nil) then begin
        if (CurTag = nil) or (ALevel = 1)
        then CurTag := CurRecord.AddTag(ATag, AValue)
        else begin
          while (ALevel <= CurTag.Level) do
            CurTag := TGEDCOMTag(CurTag.Parent);
          CurTag := CurTag.AddTag(ATag, AValue);
        end;
      end;
    end;
  finally
    SL.Free;
    FState := osReady;
  end;
end;

function TGEDCOMTree.IndexOfRecord(ARecord: TGEDCOMRecord): Integer;
begin
  Result := FRecords.IndexOfObject(ARecord);
end;

function TGEDCOMTree.GetCount: Integer;
begin
  Result := FRecords.Count;
end;

procedure TGEDCOMTree.SaveToStream(AStream: TStream);
var
  I: Integer;
begin
  SaveHeaderToStream(AStream);

  for I := 0 to FRecords.Count - 1 do
    GetRecords(I).SaveToStream(AStream);

  SaveFooterToStream(AStream);
end;

procedure TGEDCOMTree.SaveHeaderToStream(AStream: TStream);
begin
  FHeader.SaveToStream(AStream);
end;

procedure TGEDCOMTree.SaveFooterToStream(AStream: TStream);
var
  S: string;
begin
  S := '0 TRLR';

  {$IFNDEF DELPHI_NET}
  AStream.Write(S[1], Length(S));
  AStream.Write(GEDCOMNewLine[1], Length(GEDCOMNewLine));
  {$ELSE}
  SWriteString(AStream, S);
  SWriteString(AStream, GEDCOMNewLine);
  {$ENDIF}
end;

{ TGEDCOMList }

constructor TGEDCOMList.Create(AOwner: TGEDCOMObject);
begin
  inherited Create;
  FOwner := AOwner;
  FList := TList.Create;
end;

destructor TGEDCOMList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TGEDCOMList.Add(AObject: TGEDCOMObject): TGEDCOMObject;
begin
  Result := AObject;
  FList.Add(AObject);
end;

procedure TGEDCOMList.Clear();
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    TGEDCOMObject(FList[I]).Free;
  FList.Clear;
end;

procedure TGEDCOMList.Delete(Index: Integer);
begin
  TGEDCOMObject(FList[Index]).Free;
  FList.Delete(Index);
end;

procedure TGEDCOMList.DeleteObject(AObject: TGEDCOMObject);
var
  Index: Integer;
begin
  Index := FList.IndexOf(AObject);
  if Index >= 0 then
    Delete(Index);
end;

function TGEDCOMList.Extract(Index: Integer): TGEDCOMObject;
begin
  Result := GetItems(Index);
  FList.Delete(Index);
end;

function TGEDCOMList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TGEDCOMList.GetItems(Index: Integer): TGEDCOMObject;
begin
  Result := TGEDCOMObject(FList[Index]);
end;

function TGEDCOMList.IndexOfObject(AObject: TGEDCOMObject): Integer;
begin
  Result := FList.IndexOf(AObject);
end;

procedure TGEDCOMList.SaveToStream(AStream: TStream);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if (TGEDCOMObject(FList[I]) is TGEDCOMCustomTag)
    then TGEDCOMCustomTag(FList[I]).SaveToStream(AStream);
end;

procedure TGEDCOMList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

procedure TGEDCOMList.ReplaceXRefs(aMap: TXRefReplaceMap);
var
  k: Integer;
begin
  for k := 0 to FList.Count - 1 do
    if (TObject(FList[k]) is TGEDCOMCustomTag)
    then TGEDCOMCustomTag(FList[k]).ReplaceXRefs(aMap);
end;

procedure TGEDCOMList.ResetOwner(AOwner: TGEDCOMObject);
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TGEDCOMCustomTag(FList[i]).ResetOwner(AOwner);
end;

{ TGEDCOMCustomDate }

constructor TGEDCOMCustomDate.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FName := 'DATE';
end;

constructor TGEDCOMCustomDate.CreateFromString(AOwner, AParent: TGEDCOMObject;
  const AString: string);
var
  temp: string;
begin
  Create(AOwner, AParent);

  temp := AString;
  if (TGEDCOMTree(FOwner).Header.CharacterSet = csUTF8)
  then temp := Utf8ToAnsi(temp);

  ParseString(temp);
end;

{ TGEDCOMDate }

procedure TGEDCOMDate.Assign(Source: TGEDCOMCustomTag);
begin
  if (Source <> nil) and (Source is TGEDCOMDate) then begin
    FDateCalendar := TGEDCOMDate(Source).FDateCalendar;
    FYear := TGEDCOMDate(Source).FYear;
    FYearBC := TGEDCOMDate(Source).FYearBC;
    FYearModifier := TGEDCOMDate(Source).FYearModifier;
    FMonth := TGEDCOMDate(Source).FMonth;
    FDay := TGEDCOMDate(Source).FDay;
  end
  else inherited Assign(Source);
end;

function TGEDCOMDate.GetStringValue: string;
begin
  if (FDateCalendar = dcGregorian)
  then Result := EscapeString + DayString + MonthString + YearGregString
  else Result := EscapeString + DayString + MonthString + YearString;
end;

constructor TGEDCOMDate.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FDateCalendar := dcGregorian;
  FYear := -1;
  FYearBC := False;
  FYearModifier := '';
  FMonth := '';
  FDay := 0;
  Name := 'DATE';

  FDateFormat := dfGEDCOMStd;
  if (TGEDCOMTree(FOwner).Header.Source = 'BROSKEEP')
  then FDateFormat := dfSystem;
end;

constructor TGEDCOMDate.Create(AOwner, AParent: TGEDCOMObject; ADateTime: TDateTime);
var
  Y, M, D: Word;
begin
  Create(AOwner, AParent);
  SetDateTime(ADateTime);
  DecodeDate(ADateTime, Y, M, D);
  SetGregorian(D, GEDCOMMonthArray[M], Y, '', False);
end;

procedure TGEDCOMDate.Clear;
begin
  FDateCalendar := dcGregorian;
  FYear := -1;
  FYearBC := False;
  FYearModifier := '';
  FMonth := '';
  FDay := 0;
end;

function TGEDCOMDate.DayString(NoDelimiter: Boolean): string;
begin
  if (FDay <= 0)
  then Result := ''
  else begin
    Result := IntToStr(FDay);

    if Length(Result) = 1 then Result := '0' + Result;

    if not NoDelimiter
    then Result := Result + GEDCOMDelimiter;
  end;
end;

function TGEDCOMDate.EscapeString(NoDelimiter, AllwaysShowEscape: Boolean): string;
begin
  if AllwaysShowEscape or (FDateCalendar <> dcGregorian) then begin
    Result := GEDCOMDateEscapeArray[FDateCalendar];
    if not NoDelimiter
    then Result := Result + GEDCOMDelimiter;
  end else Result := '';
end;

function TGEDCOMDate.ExtractDay(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  I := 0;
  while (I < Length(Result)) and IsDigit(Result[I+1]) do
    Inc(I);
  if (I >= 1) and (I <= 2) then
  begin
    FDay := StrToInt(Copy(Result,1,I));
    StrDelete(Result, 1, I);
  end;
end;

function TGEDCOMDate.ExtractEscape(const S: string): string;
var
  I: TGEDCOMCalendar;
  P: Integer;
  SU: string;
begin
  Result := S;
  if Copy(Result, 1, 2) = '@#' then
  begin
    P := Pos('@', Copy(Result,3,MaxInt));
    if P > 0 then
    begin
      SU := Copy(Result, 1, P+2);
      for I := Low(TGEDCOMCalendar) to High(TGEDCOMCalendar) do
        if GEDCOMDateEscapeArray[I] = SU then
        begin
          FDateCalendar := I;
          StrDelete(Result, 1, Length(SU));
          Break;
        end;
    end;
  end;
end;

function TGEDCOMDate.ExtractMonth(const S: string): string;
var
  SU: string;
  I: Integer;
begin
  Result := S;
  case FDateCalendar of
    dcFrench: begin
      SU := UpperCase(Copy(Result, 1, 4));
      for I := 1 to 13 do
        if (GEDCOMMonthFrenchArray[I] = SU) then begin
          FMonth := SU;
          StrDelete(Result, 1, 4);
          Break;
        end;
    end;

    dcHebrew: begin
      SU := UpperCase(Copy(Result, 1, 3));
      for I := 1 to 13 do
        if (GEDCOMMonthHebrewArray[I] = SU) then begin
          FMonth := SU;
          StrDelete(Result, 1, 3);
          Break;
        end;
    end;

    else begin
      if not(IsDigit(Result[1])) then begin
        // gedcom standard date (01 JAN 1900)
        SU := AnsiUpperCase(Copy(Result, 1, 3));

        for I := 1 to 12 do
          if (GEDCOMMonthArray[I] = SU) or (AnsiUpperCase(ShortMonthNames[I]) = SU) then begin
            FMonth := GEDCOMMonthArray[I];
            StrDelete(Result, 1, 3);
            Break;
          end;
      end else begin
        // обработка случая, когда дата задана в обычном формате 01.01.1900
        SU := AnsiUpperCase(Copy(Result, 1, 3));

        for I := 1 to 12 do
          if (GEDCOMMonthSysArray[I] = SU) then begin
            FMonth := GEDCOMMonthArray[I];
            StrDelete(Result, 1, 2);
            Break;
          end;
      end;
    end;
  end;
end;

function TGEDCOMDate.ExtractYear(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  I := 0;
  while (I < Length(Result)) and IsDigit(Result[I + 1]) do
    Inc(I);

  if (I > 0) then begin
    FYear := StrToInt(Copy(Result, 1, I));
    StrDelete(Result, 1, I);

    if (Copy(Result, 1, 1) = GEDCOMYearModifierSeparator) and IsDigits(Copy(Result,2,2)) then begin
      FYearModifier := Copy(Result, 2, 2);
      StrDelete(Result, 1, 3);
    end;

    if UpperCase(Copy(Result, 1, Length(GEDCOMYearBC))) = GEDCOMYearBC then begin
      FYearBC := True;
      StrDelete(Result, 1, Length(GEDCOMYearBC));
    end;
  end;
end;

function TGEDCOMDate.GetMonth(): string;
begin
  Result := FMonth;
end;

function TGEDCOMDate.IsEmpty(): Boolean;
begin
  Result := inherited IsEmpty and (FYear <= -1) and (FMonth = '') and (FDay <= 0);
end;

function TGEDCOMDate.MonthString(NoDelimiter: Boolean): string;
begin
  if (FMonth = '')
  then Result := ''
  else begin
    Result := FMonth;
    if not NoDelimiter
    then Result := Result + GEDCOMDelimiter;
  end;
end;

function TGEDCOMDate.ExtractDelimiterEx(const S: string): string;
begin
  if (FDateFormat = dfSystem)
  then Result := ExtractDotDelimiter(S)
  else Result := ExtractDelimiter(S);
end;

function TGEDCOMDate.ParseString(const AString: string): string;
begin
  FDateCalendar := dcGregorian;
  FYear := -1;
  FYearBC := False;
  FYearModifier := '';
  FMonth := '';
  FDay := 0;
  Result := AString;
  if (Result <> '') then begin
    Result := ExtractDelimiter(Result);
    Result := ExtractEscape(Result);
    Result := ExtractDelimiter(Result);
    Result := ExtractDay(Result);
    Result := ExtractDelimiterEx(Result);
    Result := ExtractMonth(Result);
    Result := ExtractDelimiterEx(Result);
    Result := ExtractYear(Result);
  end;
end;

procedure TGEDCOMDate.SetMonth(const Value: string);
begin
  FMonth := Value;
end;

function TGEDCOMDate.YearGregString(NoDelimiter: Boolean): string;
begin
  if FYear = -1 then
    Result := ''
  else
  begin
    Result := IntToStr(FYear);
    if FYearModifier <> '' then
      Result := Result + GEDCOMYearModifierSeparator + FYearModifier;
    if FYearBC then
      Result := Result + GEDCOMYearBC;
    if not NoDelimiter then
      Result := Result + GEDCOMDelimiter;
  end;
end;

function TGEDCOMDate.YearString(NoDelimiter: Boolean): string;
begin
  if FYear = -1 then
    Result := ''
  else
  begin
    Result := IntToStr(FYear);
    if FYearBC then
      Result := Result + GEDCOMYearBC;
    if not NoDelimiter then
      Result := Result + GEDCOMDelimiter;
  end;
end;

function TGEDCOMDate.GetDateTime: TDateTime;
var
  M: Word;
begin
  M := GEDCOMMonthToInt(FMonth);

  if (FYear < 0) or not(M in [1..12]) or not(FDay in [1..31])
  then Result := 0
  else Result := EncodeDate(FYear, M, FDay);
end;

procedure TGEDCOMDate.SetDate(ADay, AMonth, AYear: Word);
begin
  SetGregorian(ADay, IntToGEDCOMMonth(AMonth), AYear);
end;

procedure TGEDCOMDate.SetDateTime(ADateTime: TDateTime);
var
  Y, M, D: Word;
begin
  DecodeDate(ADateTime, Y, M, D);
  SetGregorian(D, GEDCOMMonthArray[M], Y);
end;

procedure TGEDCOMDate.SetFrench(const ADay: Word; const AMonth: string; AYear: Word;
  BC: Boolean);
begin
  FDateCalendar := dcFrench;
  FYear := AYear;
  FYearBC := BC;
  FYearModifier := '';
  FMonth := StrToGEDCOMMonthFrench(AMonth);
  FDay := ADay;
end;

procedure TGEDCOMDate.SetGregorian(const ADay: Word; const AMonth: string;
  AYear: Integer; const AYearModifier: string; BC: Boolean);
begin
  FDateCalendar := dcGregorian;
  FDay := ADay;
  FMonth := StrToGEDCOMMonth(AMonth);
  FYear := AYear;
  FYearModifier := AYearModifier;
  FYearBC := BC;
end;

procedure TGEDCOMDate.SetHebrew(const ADay: Word;
  const AMonth: string; AYear: Integer; BC: Boolean);
begin
  FDateCalendar := dcHebrew;
  FYear := AYear;
  FYearBC := BC;
  FYearModifier := '';
  FMonth := StrToGEDCOMMonth(AMonth);
  FDay := ADay;
end;

procedure TGEDCOMDate.SetJulian(const ADay: Word; const AMonth: string;
  AYear: Word; BC: Boolean);
begin
  FDateCalendar := dcJulian;
  FYear := AYear;
  FYearBC := BC;
  FYearModifier := '';
  FDay := ADay;
  FMonth := StrToGEDCOMMonth(AMonth);
end;

procedure TGEDCOMDate.SetRoman(const ADay: Word; const AMonth: string; AYear: Word;
  BC: Boolean);
begin
  FDateCalendar := dcRoman;
  FYear := AYear;
  FYearBC := BC;
  FYearModifier := '';
  FDay := ADay;
  FMonth := StrToGEDCOMMonth(AMonth);
end;

procedure TGEDCOMDate.SetUnknown(const ADay: Word; const AMonth: string; AYear: Word;
  BC: Boolean);
begin
  FDateCalendar := dcUnknown;
  FYear := AYear;
  FYearBC := BC;
  FYearModifier := '';
  FDay := ADay;
  FMonth := StrToGEDCOMMonth(AMonth);
end;

procedure TGEDCOMDate.GetDate(var AYear: Integer; var AMonth, ADay: Word);
begin
  if FYearBC then
    AYear := -FYear
  else
    AYear := FYear;
  AMonth := GEDCOMMonthToInt(FMonth);
  ADay := FDay;
end;

{ TGEDCOMDateApproximated }

function TGEDCOMDateApproximated.ApproximatedString(NoDelimiter: Boolean): string;
begin
  if (FDateApproximated = daExact)
  then Result := ''
  else begin
    Result := GEDCOMDateApproximatedArray[FDateApproximated];
    if not NoDelimiter
    then Result := Result + GEDCOMDelimiter;
  end;
end;

function TGEDCOMDateApproximated.GetStringValue: string;
begin
  Result := ApproximatedString + inherited GetStringValue;
end;

constructor TGEDCOMDateApproximated.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FDateApproximated := daExact;
end;

function TGEDCOMDateApproximated.ExtractApproximated(const S: string): string;
var
  SU: string;
  I: TGEDCOMApproximated;
begin
  Result := S;
  SU := UpperCase(Copy(Result, 1, 3));
  for I := daAbout to daEstimated do
    if (SU = GEDCOMDateApproximatedArray[I]) then begin
      FDateApproximated := I;
      StrDelete(Result, 1, 3);
      Break;
    end;
end;

function TGEDCOMDateApproximated.ParseString(const S: string): string;
begin
  Result := S;
  Result := ExtractDelimiter(Result);
  Result := ExtractApproximated(Result);
  Result := ExtractDelimiter(Result);
  Result := inherited ParseString(Result);
end;

{ TGEDCOMDateInterpreted }

function TGEDCOMDateInterpreted.GetStringValue: string;
begin
  Result := GEDCOMDateInterpretedIdent + GEDCOMDelimiter +
    inherited GetStringValue + GEDCOMDelimiter + '(' + FDatePhrase + ')';
end;

constructor TGEDCOMDateInterpreted.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FDatePhrase := '';
end;

constructor TGEDCOMDateInterpreted.CreateFromDate(AOwner, AParent: TGEDCOMObject;
  ADate: TGEDCOMDate; const APhrase: string);
begin
  Create(AOwner, AParent);
  Assign(ADate);
  SetDatePhrase(APhrase);
end;

function TGEDCOMDateInterpreted.ExtractPhrase(const S: string): string;
var
  I, C: Integer;
begin
  Result := S;
  if (Length(FDatePhrase) >= 2) and (FDatePhrase[1] = '(') then begin
    StrDelete(Result, 1, 1);

    C := 0;
    for I := 1 to Length(Result) do
      if Result[I] = '(' then
        Inc(C)
      else
      if (Result[I] = ')') or (I = Length(Result)) then begin
        Dec(C);
        if (C <= 0) or (I = Length(Result)) then begin
          if Result[I] = ')' then
            FDatePhrase := Copy(Result,1,I-1)
          else
            FDatePhrase := Copy(Result,1,I);

          StrDelete(Result, 1, I);

          Break;
        end;
      end;
  end;
end;

function TGEDCOMDateInterpreted.ParseString(const S: string): string;
begin
  Result := S;
  Result := ExtractDelimiter(Result);

  if (UpperCase(Copy(Result, 1, 3)) = GEDCOMDateInterpretedIdent)
  then StrDelete(Result, 1, 3);

  Result := ExtractDelimiter(Result);
  Result := inherited ParseString(Result);
  Result := ExtractDelimiter(Result);
  Result := ExtractPhrase(Result);
end;

procedure TGEDCOMDateInterpreted.SetDatePhrase(const Value: string);
begin
  FDatePhrase := Value;
  if Length(FDatePhrase) > 0 then begin
    if FDatePhrase[1] = '(' then
      StrDelete(FDatePhrase, 1, 1);

    if (Length(FDatePhrase) > 0) and (FDatePhrase[Length(FDatePhrase)] = ')') then
      StrDelete(FDatePhrase, Length(FDatePhrase), 1);
  end;
end;

{ TGEDCOMDatePeriod }

function TGEDCOMDatePeriod.GetStringValue: string;
begin
  if not FDateFrom.IsEmpty and not FDateTo.IsEmpty then
    Result := GEDCOMDatePeriodFromIdent + GEDCOMDelimiter +
      FDateFrom.StringValue + GEDCOMDelimiter + GEDCOMDatePeriodToIdent +
      GEDCOMDelimiter + FDateTo.StringValue
  else if not FDateFrom.IsEmpty then
    Result := GEDCOMDatePeriodFromIdent + GEDCOMDelimiter +
      FDateFrom.StringValue
  else if not FDateTo.IsEmpty then
    Result := GEDCOMDatePeriodToIdent + GEDCOMDelimiter +
      FDateTo.StringValue
  else
    Result := '';
end;

procedure TGEDCOMDatePeriod.Clear;
begin
  FDateFrom.Clear;
  FDateTo.Clear;
end;

constructor TGEDCOMDatePeriod.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FDateFrom := TGEDCOMDate.Create(AOwner, Self);
  FDateTo := TGEDCOMDate.Create(AOwner, Self);
end;

destructor TGEDCOMDatePeriod.Destroy;
begin
  FDateFrom.Free;
  FDateTo.Free;
  inherited Destroy;
end;

function TGEDCOMDatePeriod.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and FDateFrom.IsEmpty and FDateTo.IsEmpty;
end;

function TGEDCOMDatePeriod.ParseString(const S: string): string;
begin
  Result := S;

  if UpperCase(Copy(Result, 1, 4)) = GEDCOMDatePeriodFromIdent then begin
    StrDelete(Result, 1, 4);

    Result := ExtractDelimiter(Result);
    Result := FDateFrom.ParseString(Result);
    Result := ExtractDelimiter(Result);
  end;

  if UpperCase(Copy(Result, 1, 2)) = GEDCOMDatePeriodToIdent then begin
    StrDelete(Result, 1, 2);

    Result := ExtractDelimiter(Result);
    Result := FDateTo.ParseString(Result);
  end;
end;

function TGEDCOMDatePeriod.GetDateTime: TDateTime;
begin
  if FDateFrom.IsEmpty then
    Result := FDateTo.GetDateTime
  else if FDateTo.IsEmpty then
    Result := FDateFrom.GetDateTime
  else if FDateFrom.GetDateTime = FDateTo.GetDateTime then
    Result := FDateFrom.GetDateTime
  else
    Result := 0.0;
end;

procedure TGEDCOMDatePeriod.SetDateTime(ADateTime: TDateTime);
begin
  if not FDateFrom.IsEmpty and FDateTo.IsEmpty
  then FDateFrom.SetDateTime(ADateTime)
  else
  if not FDateTo.IsEmpty and FDateFrom.IsEmpty
  then FDateTo.SetDateTime(ADateTime)
  else begin
    FDateFrom.SetDateTime(ADateTime);
    FDateTo.SetDateTime(ADateTime);
  end;
end;

procedure TGEDCOMDatePeriod.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);

  if (FDateFrom <> nil)
  then TGEDCOMCustomTag(FDateFrom).ResetOwner(AOwner);

  if (FDateTo <> nil)
  then TGEDCOMCustomTag(FDateTo).ResetOwner(AOwner);
end;

{ TGEDCOMDateRange }

function TGEDCOMDateRange.GetStringValue: string;
begin
  if not FDateAfter.IsEmpty and not FDateBefore.IsEmpty
  then
    Result := GEDCOMDateRangeArray[drBetween] + GEDCOMDelimiter +
      FDateAfter.StringValue + GEDCOMDelimiter + GEDCOMDateRangeArray[drAnd] +
      GEDCOMDelimiter + FDateBefore.StringValue
  else
  if not FDateAfter.IsEmpty
  then
    Result := GEDCOMDateRangeArray[drAfter] + GEDCOMDelimiter + FDateAfter.StringValue
  else
  if not FDateBefore.IsEmpty
  then
    Result := GEDCOMDateRangeArray[drBefore] + GEDCOMDelimiter + FDateBefore.StringValue
  else
    Result := '';
end;

procedure TGEDCOMDateRange.Clear;
begin
  FDateAfter.Clear;
  FDateBefore.Clear;
end;

constructor TGEDCOMDateRange.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FDateAfter := TGEDCOMDate.Create(AOwner, AParent);
  FDateBefore := TGEDCOMDate.Create(AOwner, AParent);
end;

constructor TGEDCOMDateRange.CreateAfter(AOwner, AParent: TGEDCOMObject;
  ADate: TGEDCOMDate);
begin
  Create(AOwner, AParent);
  FDateAfter.Assign(ADate);
end;

constructor TGEDCOMDateRange.CreateBefore(AOwner, AParent: TGEDCOMObject;
  ADate: TGEDCOMDate);
begin
  Create(AOwner, AParent);
  FDateBefore.Assign(ADate);
end;

constructor TGEDCOMDateRange.CreateBetween(AOwner, AParent: TGEDCOMObject; Date1,
  Date2: TGEDCOMDate);
begin
  Create(AOwner, AParent);
  FDateAfter.Assign(Date1);
  FDateBefore.Assign(Date2);
end;

destructor TGEDCOMDateRange.Destroy;
begin
  FDateAfter.Free;
  FDateBefore.Free;
  inherited Destroy;
end;

function TGEDCOMDateRange.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and FDateAfter.IsEmpty and FDateBefore.IsEmpty;
end;

function TGEDCOMDateRange.ParseString(const S: string): string;
var
  SU: string;
begin
  FDateAfter.Clear;
  FDateBefore.Clear;
  Result := S;
  SU := UpperCase(Copy(Result, 1, 3));
  if (SU = GEDCOMDateRangeArray[drAfter]) then begin
    StrDelete(Result, 1, 3);

    Result := ExtractDelimiter(Result);
    Result := FDateAfter.ParseString(Result);
  end
  else
  if (SU = GEDCOMDateRangeArray[drBefore]) then begin
    StrDelete(Result, 1, 3);

    Result := ExtractDelimiter(Result);
    Result := FDateBefore.ParseString(Result);
  end
  else
  if (SU = GEDCOMDateRangeArray[drBetween]) then begin
    StrDelete(Result, 1, 3);

    Result := ExtractDelimiter(Result);
    Result := FDateAfter.ParseString(Result);
    Result := ExtractDelimiter(Result);
    SU := UpperCase(Copy(Result, 1, 3));
    if (SU = GEDCOMDateRangeArray[drAnd]) then begin
      StrDelete(Result, 1, 3);

      Result := ExtractDelimiter(Result);
      Result := FDateBefore.ParseString(Result);
    end;
  end;
end;

function TGEDCOMDateRange.GetDateTime: TDateTime;
begin
  if FDateAfter.IsEmpty then
    Result := FDateBefore.GetDateTime
  else if FDateBefore.IsEmpty then
    Result := FDateAfter.GetDateTime
  else
    Result := 0.0;
end;

procedure TGEDCOMDateRange.SetDateTime(ADateTime: TDateTime);
begin
  if not FDateAfter.IsEmpty and FDateBefore.IsEmpty then
    FDateAfter.SetDateTime(ADateTime)
  else if not FDateBefore.IsEmpty and FDateAfter.IsEmpty then
    FDateBefore.SetDateTime(ADateTime)
  else
  begin
    FDateAfter.SetDateTime(ADateTime);
    FDateBefore.SetDateTime(ADateTime);
  end;
end;

procedure TGEDCOMDateRange.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);

  if (FDateAfter <> nil)
  then TGEDCOMCustomTag(FDateAfter).ResetOwner(AOwner);

  if (FDateBefore <> nil)
  then TGEDCOMCustomTag(FDateBefore).ResetOwner(AOwner);
end;

{ TGEDCOMDatePhrase }

function TGEDCOMDatePhrase.GetStringValue: string;
begin
  Result := '(' + FStringValue + ')';
end;

procedure TGEDCOMDatePhrase.Clear;
begin
  inherited;
  FStringValue := '';
end;

constructor TGEDCOMDatePhrase.CreateFromText(AOwner, AParent: TGEDCOMObject; const S: string);
begin
  Create(AOwner, AParent);
  SetDatePhrase(S);
end;

function TGEDCOMDatePhrase.ExtractPhrase(const S: string): string;
var
  I, C: Integer;
begin
  Result := S;
  if (Length(FStringValue) >= 2) and (FStringValue[1] = '(') then
  begin
    StrDelete(Result, 1, 1);

    C := 0;
    for I := 1 to Length(Result) do
      if Result[I] = '(' then
        Inc(C)
      else if (Result[I] = ')') or (I = Length(Result)) then
      begin
        Dec(C);
        if (C <= 0) or (I = Length(Result)) then
        begin
          if Result[I] = ')' then
            FStringValue := Copy(Result,1,I-1)
          else
            FStringValue := Copy(Result,1,I);

          StrDelete(Result, 1, I);
          Break;
        end;
      end;
  end;
end;

function TGEDCOMDatePhrase.IsEmpty: Boolean;
begin
  Result := FStringValue = '';
end;

function TGEDCOMDatePhrase.ParseString(const S: string): string;
begin
  Result := S;
  Result := ExtractDelimiter(Result);
  Result := ExtractPhrase(Result);
end;

procedure TGEDCOMDatePhrase.SetDatePhrase(const Value: string);
begin
  FStringValue := Value;
  if Length(FStringValue) > 0 then
  begin
    if FStringValue[1] = '(' then
      StrDelete(FStringValue,1,1);

    if (Length(FStringValue) > 0) and (FStringValue[Length(FStringValue)] = ')') then
      StrDelete(FStringValue,Length(FStringValue),1);
  end;
end;

function TGEDCOMDatePhrase.GetDateTime: TDateTime;
begin
  Result := 0.0;
end;

procedure TGEDCOMDatePhrase.SetDateTime(ADateTime: TDateTime);
var
  D: TGEDCOMDateExact;
begin
  D := TGEDCOMDateExact.Create(nil, nil, ADateTime);
  SetDatePhrase(D.StringValue);
  D.Free;
end;

{ TGEDCOMHeader }

constructor TGEDCOMHeader.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FName := 'HEAD';
  FNotes := nil;
end;

destructor TGEDCOMHeader.Destroy;
begin
  if FNotes <> nil then
    FreeAndNil(FNotes);
  inherited Destroy;
end;

procedure TGEDCOMHeader.Clear;
begin
  inherited Clear;
  if FNotes <> nil then
    FreeAndNil(FNotes);
end;

function TGEDCOMHeader.GetSourceBusinessAddress: TGEDCOMAddress;
var
  SourTag: TGEDCOMTag;
  CorpTag: TGEDCOMTag;
begin
  SourTag := FindTag('SOUR');
  if SourTag = nil then
    SourTag := AddTag('SOUR');
  CorpTag := SourTag.FindTag('CORP');
  if CorpTag = nil then
    CorpTag := SourTag.AddTag('CORP');
  Result := TGEDCOMAddress(CorpTag.TagClass('ADDR', TGEDCOMAddress));
end;

function TGEDCOMHeader.GetStringTag(Index: Integer): string;
begin
  case Index of    
     1: Result := TagStringValue('SOUR');       // Source id
     2: Result := TagStringValue('SOUR\VERS');  // Source version
     3: Result := TagStringValue('SOUR\NAME');  // Source product name
     4: Result := TagStringValue('SOUR\CORP');  // Source business name
     5: Result := TagStringValue('DEST');       // Receiving system name
     6: Result := TagStringValue('FILE');       // File name
     7: Result := TagStringValue('COPR');       // Copyright
     8: Result := TagStringValue('GEDC\VERS');  // GEDCOM version
     9: Result := TagStringValue('GEDC\FORM');  // GEDCOM form
    10: Result := TagStringValue('CHAR\VERS');  // Character set version
    11: Result := TagStringValue('LANG');       // Language ID
    12: Result := TagStringValue('PLAC\FORM');  // Place hierarchy format
  end;
end;

procedure TGEDCOMHeader.SetStringTag(Index: Integer; const Value: string);
begin
  case Index of
     1: SetTagStringValue('SOUR', Value);       // Source id
     2: SetTagStringValue('SOUR\VERS', Value);  // Source version
     3: SetTagStringValue('SOUR\NAME', Value);  // Source product name
     4: SetTagStringValue('SOUR\CORP', Value);  // Source business name
     5: SetTagStringValue('DEST', Value);       // Receiving system name
     6: SetTagStringValue('FILE', Value);       // File name
     7: SetTagStringValue('COPR', Value);       // Copyright
     8: SetTagStringValue('GEDC\VERS', Value);  // GEDCOM version
     9: SetTagStringValue('GEDC\FORM', Value);  // GEDCOM form
    10: SetTagStringValue('CHAR\VERS', Value);  // Character set version
    11: SetTagStringValue('LANG', Value);       // Language ID
    12: SetTagStringValue('PLAC\FORM', Value);  // Place hierarchy format
  end;    
end;

function TGEDCOMHeader.GetDate: TGEDCOMDateExact;
begin
  Result := TGEDCOMDateExact(TagClass('DATE', TGEDCOMDateExact));
end;

function TGEDCOMHeader.GetTime: TGEDCOMTime;
var
  DateTag: TGEDCOMTag;
begin
  DateTag := FindTag('DATE');
  if DateTag = nil then
    DateTag := AddTag('DATE', '');

  Result := TGEDCOMTime(DateTag.TagClass('TIME', TGEDCOMTime));
end;

function TGEDCOMHeader.GetSubmittor: TGEDCOMPointer;
begin
  Result := TGEDCOMPointer(TagClass('SUBM', TGEDCOMPointer));
end;

function TGEDCOMHeader.GetSubmission: TGEDCOMPointer;
begin
  Result := TGEDCOMPointer(TagClass('SUBN', TGEDCOMPointer));
end;

function TGEDCOMHeader.GetCharacterSet: TGEDCOMCharacterSet;
var
  S: string;
begin
  S := UpperCase(TagStringValue('CHAR'));
  if (S = 'ASCII') or (S = 'ANSI') or (S = 'IBMPC')
  then Result := csASCII
  else
  if (S = 'ANSEL')
  then Result := csANSEL
  else
  if (S = 'UNICODE')
  then Result := csUNICODE
  else
  if (S = 'UTF8') or (S = 'UTF-8')
  then Result := csUTF8
  else Result := csANSEL;
end;

procedure TGEDCOMHeader.SetCharacterSet(const Value: TGEDCOMCharacterSet);
var
  S: string;
begin
  case Value of {std values}
    csASCII: S := 'ASCII';
    csANSEL: S := 'ANSEL';
    csUNICODE: S := 'UNICODE';
    csUTF8: S := 'UTF-8';
  end;
  SetTagStringValue('CHAR', S);
end;

function TGEDCOMHeader.GetNotes: TStrings;
begin
  Result := TagStrings(FindTag('NOTE'), FNotes);
end;

procedure TGEDCOMHeader.SetNotes(const Value: TStrings);
begin
  SetTagStrings(TagClass('NOTE', TGEDCOMNotes), Value);
end;

function TGEDCOMHeader.GetTransmissionDateTime: TDateTime;
begin
  Result := TransmissionDate.Date + TransmissionTime.Time;
end;

procedure TGEDCOMHeader.SetTransmissionDateTime(const Value: TDateTime);
begin
  TransmissionDate.Date := Value;
  TransmissionTime.Time := Value;
end;

function TGEDCOMHeader.AddTag(const ATag, AValue: string; AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);

  if (SU = 'DATE')
  then Result := inherited AddTag(ATag, AValue, TGEDCOMDateExact)
  else
  if (SU = 'SUBM')
  then Result := inherited AddTag(ATag, AValue, TGEDCOMPointer)
  else
  if (SU = 'SUBN')
  then Result := inherited AddTag(ATag, AValue, TGEDCOMPointer)
  else Result := inherited AddTag(ATag, AValue, AClass);
end;

function TGEDCOMHeader.AddSubTag(AParent: TGEDCOMCustomTag; const ATag,
  AValue: string; AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);
  if (AParent = FindTag('SOUR\CORP')) and
     ((SU = 'PHON') or (SU = 'EMAIL') or (SU = 'FAX') or (SU = 'WWW')) then
  begin
    if (FindTag('SOUR\CORP\ADDR') = nil)
    then SetTagStringValue('SOUR\CORP\ADDR', '');

    Result := FindTag('SOUR\CORP\ADDR').AddTag(ATag, AValue, AClass);
  end
  else
  if (SU = 'ADDR')
  then Result := inherited AddSubTag(AParent, ATag, AValue, TGEDCOMAddress)
  else Result := inherited AddSubTag(AParent, ATag, AValue, AClass);
end;

{ TGEDCOMPointer }

constructor TGEDCOMPointer.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FXRef := '';
end;

procedure TGEDCOMPointer.Clear;
begin
  inherited Clear;
end;

function TGEDCOMPointer.GetStringValue: string;
begin
  Result := FXRef;
end;

function TGEDCOMPointer.GetValue: TGEDCOMRecord;
begin
  Result := FindRecord(GetXRef());
end;

function TGEDCOMPointer.GetXRef: string;
begin
  Result := CleanXRef(FXRef);
end;

function TGEDCOMPointer.IsEmpty: Boolean;
begin
  Result := FXRef = '';
end;

function TGEDCOMPointer.ParseString(const AString: string): string;
begin
  FXRef := '';
  Result := AString;
  Result := ExtractDelimiter(Result);
  if (Copy(Result,1,1) = GEDCOMPointerDelimiter) and
     (Copy(Result,2,1) <> '#') and
     (Pos(GEDCOMPointerDelimiter, Copy(Result,3,MaxInt)) > 0) then
  begin
    StrDelete(Result,1,1);
    FXRef := GEDCOMPointerDelimiter +
      Copy(Result, 1, Pos(GEDCOMPointerDelimiter, Result));
    StrDelete(Result,1,Pos(GEDCOMPointerDelimiter,Result));
  end;
end;

procedure TGEDCOMPointer.SetValue(AValue: TGEDCOMRecord);
var
  XRef: string;
begin
  FXRef := '';

  if (AValue <> nil) then begin
    XRef := AValue.XRef;

    if (XRef = '')
    then XRef := AValue.NewXRef();

    SetXRef(XRef);
  end;
end;

procedure TGEDCOMPointer.SetXRef(const Value: string);
begin
  FXRef := EncloseXRef(Value);
end;

procedure TGEDCOMPointer.ReplaceXRefs(aMap: TXRefReplaceMap);
begin
  inherited ReplaceXRefs(aMap);

  SetXRef(aMap.FindNewXRef(XRef));
end;

{ TGEDCOMCustomRecord }

function TGEDCOMCustomRecord.AddSubTag(AParent: TGEDCOMCustomTag;
  const ATag, AValue: string; AClass: TGEDCOMTagClass): TGEDCOMTag;
begin
  if (AClass = nil)
  then Result := AParent.InsertTag(CreateGEDCOMTag(Owner, AParent, ATag, AValue))
  else Result := AParent.InsertTag(AClass.CreateTag(Owner, AParent, ATag, AValue))
end;

function TGEDCOMCustomRecord.AddTag(const ATag, AValue: string; AClass: TGEDCOMTagClass): TGEDCOMTag;
begin
  if (AClass = nil)
  then Result := InsertTag(CreateGEDCOMTag(Owner, Self, ATag, AValue))
  else Result := InsertTag(AClass.CreateTag(Owner, Self, ATag, AValue))
end;

procedure TGEDCOMCustomRecord.SaveValueToStream(AStream: TStream);
var
  S: string;
begin
  S := IntToStr(Level);

  if (XRef <> '')
  then S := S + GEDCOMDelimiter + GEDCOMPointerDelimiter + XRef + GEDCOMPointerDelimiter;

  S := S + GEDCOMDelimiter + Name;

  if (StringValue <> '')
  then S := S + GEDCOMDelimiter + StringValue;

  AStream.Write(S[1], Length(S));
  StreamWriteNewLine(AStream);
end;

procedure TGEDCOMCustomRecord.SetXRef(const AXRef: string);
begin
  FXRef := AXRef;

  if (Self is TGEDCOMRecord) and (FOwner <> nil)
  then TGEDCOMTree(FOwner).SetXRef(TGEDCOMRecord(Self), XRef);
end;

{ TGEDCOMDateValue }

constructor TGEDCOMDateValue.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FValue := nil;
end;

function TGEDCOMDateValue.GetStringValue: string;
begin
  if (FValue <> nil)
  then Result := FValue.StringValue
  else Result := '';
end;

procedure TGEDCOMDateValue.Clear;
begin
  if (FValue <> nil) then FValue.Clear;
end;

function TGEDCOMDateValue.IsEmpty: Boolean;
begin
  Result := (FValue = nil) or FValue.IsEmpty;
end;

function TGEDCOMDateValue.ParseString(const S: string): string;
var
  SU: string;
begin
  if (FValue <> nil) then begin
    FValue.Free;
    FValue := nil;
  end;

  SU := UpperCase(Copy(S, 1, 3));
  if (SU = GEDCOMDateApproximatedArray[daAbout]) or
     (SU = GEDCOMDateApproximatedArray[daCalculated]) or
     (SU = GEDCOMDateApproximatedArray[daEstimated])
  then FValue := TGEDCOMDateApproximated.Create(Owner, Self)
  else
  if (SU = GEDCOMDateInterpretedIdent)
  then FValue := TGEDCOMDateInterpreted.Create(Owner, Self)
  else
  if (SU = GEDCOMDateRangeArray[drAfter]) or
     (SU = GEDCOMDateRangeArray[drBefore]) or
     (SU = GEDCOMDateRangeArray[drBetween])
  then FValue := TGEDCOMDateRange.Create(Owner, Self)
  else
  if (UpperCase(Copy(S, 1, 4)) = GEDCOMDatePeriodFromIdent) or
     (UpperCase(Copy(S, 1, 2)) = GEDCOMDatePeriodToIdent)
  then FValue := TGEDCOMDatePeriod.Create(Owner, Self)
  else FValue := TGEDCOMDate.Create(Owner, Self);

  Result := FValue.ParseString(S);
end;

function TGEDCOMDateValue.GetDateTime: TDateTime;
begin
  if (FValue <> nil)
  then Result := FValue.GetDateTime
  else Result := 0.0;
end;

procedure TGEDCOMDateValue.SetDateTime(ADateTime: TDateTime);
begin
  if (FValue <> nil)
  then FValue.SetDateTime(ADateTime)
  else FValue := TGEDCOMDateExact.Create(Owner, Self, ADateTime);
end;

procedure TGEDCOMDateValue.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);

  if (FValue <> nil)
  then TGEDCOMCustomTag(FValue).ResetOwner(AOwner);
end;

procedure TGEDCOMDateValue.Assign(Source: TGEDCOMCustomTag);
begin
  inherited Assign(Source);

  //if (Source is TGEDCOMDateValue)
  //then ParseString((Source as TGEDCOMDateValue).Value.StringValue);
end;

{ TGEDCOMCustomTag }

type
  TTagProps = set of (tpEmptySkip);

const
  TagBaseSize = 21;
  TagBase: array [0..TagBaseSize-1] of record
    Name: string;
    Props: TTagProps;
  end = (
    //(Name: ''; Props: [])
    (Name: 'ADDR'; Props: [tpEmptySkip]),
    (Name: 'AGNC'; Props: [tpEmptySkip]),
    (Name: 'AUTH'; Props: [tpEmptySkip]),
    (Name: 'CAUS'; Props: [tpEmptySkip]),
    (Name: 'DATE'; Props: [tpEmptySkip]),
    (Name: 'PLAC'; Props: [tpEmptySkip]),
    (Name: 'PUBL'; Props: [tpEmptySkip]),
    (Name: 'RESN'; Props: [tpEmptySkip]),
    (Name: 'TIME'; Props: [tpEmptySkip]),
    (Name: 'TYPE'; Props: [tpEmptySkip]),
    (Name: 'CTRY'; Props: [tpEmptySkip]),
    (Name: 'STAE'; Props: [tpEmptySkip]),
    (Name: 'CITY'; Props: [tpEmptySkip]),
    (Name: 'POST'; Props: [tpEmptySkip]),

    (Name: 'TEXT'; Props: [tpEmptySkip]),

    (Name: 'NPFX'; Props: [tpEmptySkip]),
    (Name: 'GIVN'; Props: [tpEmptySkip]),
    (Name: 'NICK'; Props: [tpEmptySkip]),
    (Name: 'SPFX'; Props: [tpEmptySkip]),
    (Name: 'SURN'; Props: [tpEmptySkip]),
    (Name: 'NSFX'; Props: [tpEmptySkip])
  );

function GetTagProps(const aName: string): TTagProps;
var
  i: Integer;
begin
  Result := [];
  for i := 0 to TagBaseSize - 1 do
    if (TagBase[i].Name = aName) then begin
      Result := TagBase[i].Props;
      Break;
    end;
end;

function IsWriteSkip(const aName: string): Boolean;
begin
  Result := {False;//}(tpEmptySkip in GetTagProps(aName));
end;

constructor TGEDCOMCustomTag.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create;

  FOwner := AOwner;
  FParent := AParent;
  FTags := nil;
  FStringValue := '';

  if (AParent <> nil) and (AParent is TGEDCOMCustomTag)
  then FLevel := TGEDCOMCustomTag(AParent).Level + 1
  else FLevel := 0;
end;

constructor TGEDCOMCustomTag.CreateTag(AOwner, AParent: TGEDCOMObject;
  const AName, AValue: string);
begin
  Create(AOwner, AParent);
  Name := AName;
  SetStringValue(AValue);
end;

constructor TGEDCOMCustomTag.CreateCopy(Source: TGEDCOMCustomTag;
  AOwner: TGEDCOMObject = nil; AParent: TGEDCOMObject = nil);
begin
  if (AOwner = nil)
  then AOwner := Source.Owner;

  if (AParent = nil)
  then AParent := Source.Parent;

  Create(AOwner, AParent);

  FName := Source.Name;
  Assign(Source);
end;

destructor TGEDCOMCustomTag.Destroy;
begin
  inherited Destroy;
end;

procedure TGEDCOMCustomTag.Assign(Source: TGEDCOMCustomTag);
var
  i: Integer;
  tag, copy: TGEDCOMTag;
begin
  if (Source = nil) then Exit;

  StringValue := Source.StringValue;

  for i := 0 to Source.Count - 1 do begin
    tag := Source.Tags[i];
    copy := TGEDCOMTagClass(tag.ClassType).CreateCopy(tag, Self.Owner, Self);
    InsertTag(copy);
  end;
end;

function TGEDCOMCustomTag.AddTag(const ATag, AValue: string; AClass: TGEDCOMTagClass): TGEDCOMTag;
begin
  if (ParentRecord <> nil)
  then Result := ParentRecord.AddSubTag(Self, ATag, AValue, AClass)
  else
  if (AClass <> nil)
  then Result := InsertTag(AClass.CreateTag(Owner, Self, ATag, AValue))
  else Result := InsertTag(CreateGEDCOMTag(Owner, Self, ATag, AValue));
end;

procedure TGEDCOMCustomTag.Clear;
begin
  if (FTags <> nil) then FreeAndNil(FTags);
  FStringValue := '';
end;

procedure TGEDCOMCustomTag.Delete(Index: Integer);
begin
  FTags.Delete(Index);
end;

procedure TGEDCOMCustomTag.DeleteTag(const ATag: string);
var
  Tag: TGEDCOMTag;
  Index: Integer;
begin
  if (FTags <> nil) then begin
    Tag := FindTag(ATag);
    Index := FTags.IndexOfObject(Tag);
    while (Tag <> nil) do begin
      FTags.DeleteObject(Tag);
      Tag := FindTag(ATag, Index);
    end;
  end;
end;

function TGEDCOMCustomTag.FindRecord(const XRef: string): TGEDCOMRecord;
begin
  if (FOwner <> nil)
  then Result := TGEDCOMTree(FOwner).XRefIndex_Find(XRef)
  else Result := nil;
end;

function TGEDCOMCustomTag.FindTag(const ATag: string; StartIndex: Integer): TGEDCOMTag;
var
  Index: Integer;
  SU, S1: string;
  O: TGEDCOMCustomTag;
begin
  SU := UpperCase(ATag);

  if (Pos('\', SU) > 0)
  then S1 := Copy(SU, 1, Pos('\', SU) - 1)
  else S1 := SU;

  O := Self;
  repeat
    if (S1 = SU)
    then Index := StartIndex
    else Index := 0;

    while (Index < O.Count) and (TGEDCOMCustomTag(O.Tags[Index]).Name <> S1) do
      Inc(Index);

    if (Index < O.Count) then begin
      Result := TGEDCOMTag(O.Tags[Index]);
      O := Result;
    end else begin
      Result := nil;
      Break;
    end;

    if (Pos('\', SU) > 0) then begin
      SU := Copy(SU, Pos('\', SU) + 1, MaxInt);

      if (Pos('\', SU) > 0)
      then S1 := Copy(SU, 1, Pos('\', SU) - 1)
      else S1 := Su;
    end else SU := '';
  until (SU = '');
end;

function TGEDCOMCustomTag.GetCount: Integer;
begin
  if (FTags = nil)
  then Result := 0
  else Result := FTags.Count;
end;

function TGEDCOMCustomTag.GetLevel: Integer;
begin
  Result := FLevel;
end;

function TGEDCOMCustomTag.GetParentRecord: TGEDCOMCustomRecord;
var
  O: TGEDCOMObject;
begin
  Result := nil;
  O := Parent;
  while (O <> nil) and (O is TGEDCOMCustomTag) do begin
    if (O is TGEDCOMCustomRecord) then begin
      Result := TGEDCOMCustomRecord(O);
      Break;
    end;
    O := TGEDCOMCustomTag(O).Parent;
  end;
end;

function TGEDCOMCustomTag.GetStringValue: string;
begin
  Result := FStringValue;
end;

function TGEDCOMCustomTag.GetTags(Index: Integer): TGEDCOMTag;
begin
  if (FTags <> nil)
  then Result := TGEDCOMTag(FTags[Index])
  else Result := nil;
end;

function TGEDCOMCustomTag.IndexOfTag(ATag: TGEDCOMTag): Integer;
begin
  if (FTags <> nil)
  then Result := FTags.IndexOfObject(ATag)
  else Result := -1;
end;

function TGEDCOMCustomTag.InsertTag(ATag: TGEDCOMTag): TGEDCOMTag;
begin
  Result := ATag;

  if (FTags = nil)
  then FTags := TGEDCOMList.Create(Self);

  FTags.Add(ATag);
end;

function TGEDCOMCustomTag.IsEmpty: Boolean;
begin
  Result := (FStringValue = '') and ((FTags = nil) or (FTags.Count = 0));
end;

function TGEDCOMCustomTag.ParseString(const AString: string): string;
begin
  FStringValue := AString;
end;

procedure TGEDCOMCustomTag.SaveTagsToStream(AStream: TStream;
  const ATagSorting: array of string);
var
  I, Index: Integer;
  SavedTags: TStringList;
begin
  if (Count > 0) then begin
    SavedTags := TStringList.Create;
    try
      SavedTags.Duplicates := dupIgnore;
      SavedTags.Sorted := True;

      for I := 0 to Count - 1 do
        SavedTags.Add(Tags[I].Name);

      if (SavedTags.IndexOf('CONC') >= 0) or (SavedTags.IndexOf('CONT') >= 0) then begin
        for I := 0 to Count - 1 do
          if (Tags[I].Name = 'CONC') or (Tags[I].Name = 'CONT')
          then SaveTagToStream(AStream, Tags[I]);

        if SavedTags.IndexOf('CONC') >= 0
        then SavedTags.Delete(SavedTags.IndexOf('CONC'));

        if SavedTags.IndexOf('CONT') >= 0
        then SavedTags.Delete(SavedTags.IndexOf('CONT'));
      end;
      if Length(ATagSorting) = 0 then
        for I := 0 to Count - 1 do begin
          if (Tags[I].Name <> 'CONT') and (Tags[I].Name <> 'CONC')
          then SaveTagToStream(AStream, Tags[I])
        end
      else begin
        for I := Low(ATagSorting) to High(ATagSorting) do begin
          Index := SavedTags.IndexOf(ATagSorting[I]);
          if (Index >= 0)
          then SavedTags.Delete(Index);
          SaveTagToStream(AStream, ATagSorting[I])
        end;

        for I := 0 to SavedTags.Count - 1 do
          SaveTagToStream(AStream, SavedTags[I]);
      end;
    finally
      SavedTags.Free;
    end;
  end;
end;

procedure TGEDCOMCustomTag.SaveTagToStream(AStream: TStream; ATag: TGEDCOMTag);
begin
  if (ATag <> nil)
  then ATag.SaveToStream(AStream);
end;

procedure TGEDCOMCustomTag.SaveTagToStream(AStream: TStream; const ATag: string);
var
  Tag: TGEDCOMTag;
  Index: Integer;
begin
  Tag := FindTag(ATag);
  while (Tag <> nil) do begin
    Index := IndexOfTag(Tag);
    Tag.SaveToStream(AStream);
    Tag := FindTag(ATag, Index + 1);
  end;
end;

procedure TGEDCOMCustomTag.SaveToStream(AStream: TStream);
begin
  if IsEmpty() and IsWriteSkip(FName)
  then Exit;

  SaveValueToStream(AStream);
  SaveTagsToStream(AStream, []);
end;

procedure TGEDCOMCustomTag.SaveValueToStream(AStream: TStream);
var
  S, Val: string;
begin
  S := IntToStr(Level) + GEDCOMDelimiter + Name;

  Val := StringValue;
  if (TGEDCOMTree(FOwner).Header.CharacterSet = csUTF8)
  then Val := AnsiToUtf8(Val);

  if (Val <> '')
  then S := S + GEDCOMDelimiter + Val;

  AStream.Write(S[1], Length(S));
  StreamWriteNewLine(AStream);
end;

procedure TGEDCOMCustomTag.SetLevel(Value: Integer);
begin
  FLevel := Value;
end;

procedure TGEDCOMCustomTag.SetTagIntegerValue(const ATag: string;
  AValue: Integer);
begin
  SetTagStringValue(ATag, IntToStr(AValue));
end;

function TGEDCOMCustomTag.TagIntegerValue(const ATag: string; ADefault: Integer): Integer;
var
  S: string;
begin
  S := TagStringValue(ATag);

  if (S = '')
  then Result := ADefault
  else Result := StrToIntDef(S, ADefault);
end;

procedure TGEDCOMCustomTag.SetStringValue(const S: string);
var
  temp: string;
begin
  temp := S;

  if (TGEDCOMTree(FOwner).Header.CharacterSet = csUTF8)
  then temp := Utf8ToAnsi(S);

  ParseString(temp);
end;

function TGEDCOMCustomTag.TagStringValue(const ATag: string): string;
var
  Tag: TGEDCOMTag;
begin
  Tag := FindTag(ATag);

  if (Tag <> nil)
  then Result := Tag.StringValue
  else Result := '';
end;

procedure TGEDCOMCustomTag.SetTagStringValue(const ATag, AValue: string);
var
  SU, S1: string;
  Index: Integer;
  O, P: TGEDCOMCustomTag;
begin
  SU := UpperCase(ATag);
  P := FindTag(SU);

  if (P <> nil)
  then P.StringValue := AValue
  else begin
    O := Self;
    while (SU <> '') do begin
      Index := Pos('\', SU);
      if (Index > 0) then begin
        S1 := Copy(SU, 1, Index-1);
        SU := Copy(SU, Index+1, MaxInt);
      end else begin
        S1 := SU;
        SU := '';
      end;

      P := O.FindTag(S1);

      if (P = nil)
      then
        if (SU = '')
        then P := O.AddTag(S1, AValue)
        else P := O.AddTag(S1, '')
      else
        if (SU = '')
        then TGEDCOMTag(P).StringValue := AValue;

      O := P;
    end;
  end;
end;

procedure TGEDCOMCustomTag.StreamWriteNewLine(AStream: TStream);
begin
  AStream.Write(GEDCOMNewLine[1], Length(GEDCOMNewLine));
end;

function TGEDCOMCustomTag.TagClass(const ATag: string; AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  Tag: TGEDCOMTag;
begin
  Tag := FindTag(ATag);

  if (Tag = nil)
  then Result := AddTag(ATag, '', AClass)
  else
    if (Tag is AClass)
    then Result := Tag
    else raise EGEDCOMException.CreateFmt(ATagHasInvalidType, [ATag, Tag.ClassName, AClass.ClassName]);
end;

procedure TGEDCOMCustomTag.SetTagStrings(ATag: TGEDCOMCustomTag; Value: TStrings);
var
  I: Integer;
  S: string;
begin
  if (ATag <> nil) then begin
    ATag.StringValue := '';
    for I := ATag.Count - 1 downto 0 do
      if (ATag.Tags[I].Name = 'CONT') or (ATag.Tags[I].Name = 'CONC')
      then ATag.Delete(I);

    if (Value <> nil) then
      for I := 0 to Value.Count - 1 do begin
        S := Value[I];

        if (I = 0) and not(ATag is TGEDCOMRecord) {zsv-hack}
        then ATag.StringValue := Copy(S, 1, 248)
        else ATag.AddTag('CONT', Copy(S, 1, 248));

        StrDelete(S, 1, 248);

        while Length(S) > 0 do begin
          ATag.AddTag('CONC', Copy(S, 1, 248));
          StrDelete(S, 1, 248);
        end;
      end;
  end;
end;

function TGEDCOMCustomTag.TagStrings(ATag: TGEDCOMCustomTag;
  var AStrings: TStrings): TStrings;
var
  I: Integer;
  Tag: TGEDCOMTag;
begin
  if (AStrings = nil)
  then AStrings := TStringList.Create
  else AStrings.Clear;

  if (ATag <> nil) then begin
    if (ATag.StringValue <> '')
    then AStrings.Add(ATag.StringValue); {zsv: hack}

    for I := 0 to ATag.Count - 1 do begin
      Tag := ATag.Tags[I];

      if (Tag.Name = 'CONC')
      then AStrings[AStrings.Count-1] := AStrings[AStrings.Count-1] + Tag.StringValue
      else
      if (Tag.Name = 'CONT')
      then AStrings.Add(Tag.StringValue);
    end;
  end;

  Result := AStrings;
end;

procedure TGEDCOMCustomTag.ResetOwner(AOwner: TGEDCOMObject);
begin
  FOwner := AOwner;
  if (FTags <> nil) then FTags.ResetOwner(AOwner);
end;

procedure TGEDCOMCustomTag.ReplaceXRefs(aMap: TXRefReplaceMap);
begin
  if (FTags <> nil) then FTags.ReplaceXRefs(aMap);
end;

{ TGEDCOMTagWithLists }

function TGEDCOMTagWithLists.AddMultimediaLink(
  AMultimediaLink: TGEDCOMMultimediaLink): TGEDCOMMultimediaLink;
begin
  Result := AMultimediaLink;

  if (FMultimediaLinks = nil)
  then FMultimediaLinks := TGEDCOMList.Create(Self);

  if (AMultimediaLink <> nil)
  then FMultimediaLinks.Add(AMultimediaLink);
end;

function TGEDCOMTagWithLists.AddNotes(ANotes: TGEDCOMNotes): TGEDCOMNotes;
begin
  Result := ANotes;

  if (FNotes = nil)
  then FNotes := TGEDCOMList.Create(Self);

  if (ANotes <> nil)
  then FNotes.Add(ANotes);
end;

function TGEDCOMTagWithLists.AddSourceCitation(
  ASourceCitation: TGEDCOMSourceCitation): TGEDCOMSourceCitation;
begin
  Result := ASourceCitation;

  if (FSourceCitations = nil)
  then FSourceCitations := TGEDCOMList.Create(Self);

  if (ASourceCitation <> nil)
  then FSourceCitations.Add(ASourceCitation);
end;

procedure TGEDCOMTagWithLists.DeleteNotes(aIndex: Integer);
begin
  if (FNotes <> nil)
  then FNotes.Delete(aIndex);
end;

procedure TGEDCOMTagWithLists.DeleteSourceCitation(aIndex: Integer);
begin
  if (FSourceCitations <> nil)
  then FSourceCitations.Delete(aIndex);
end;

procedure TGEDCOMTagWithLists.DeleteMultimediaLink(aIndex: Integer);
begin
  if (FMultimediaLinks <> nil)
  then FMultimediaLinks.Delete(aIndex);
end;

function TGEDCOMTagWithLists.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);
  if (SU = 'NOTE') and (stNotes in FLists)
  then Result := AddNotes(TGEDCOMNotes.CreateTag(Owner, Self, SU, AValue))
  else
  if (SU = 'SOUR') and (stSource in FLists)
  then Result := AddSourceCitation(TGEDCOMSourceCitation.CreateTag(Owner, Self, SU, AValue))
  else
  if (SU = 'OBJE') and (stMultimedia in FLists)
  then Result := AddMultimediaLink(TGEDCOMMultimediaLink.CreateTag(Owner, Self, SU, AValue))
  else Result := inherited AddTag(ATag, AValue, AClass);
end;

procedure TGEDCOMTagWithLists.Clear;
begin
  inherited Clear;
  if (FNotes <> nil) then FNotes.Clear;
  if (FSourceCitations <> nil) then FSourceCitations.Clear;
  if (FMultimediaLinks <> nil) then FMultimediaLinks.Clear;
end;

constructor TGEDCOMTagWithLists.Create(AOwner, AParent: TGEDCOMObject;
  ALists: TGEDCOMLists);
begin
  inherited Create(AOwner, AParent);
  FLists := ALists;
  FNotes := nil;
  FSourceCitations := nil;
  FMultimediaLinks := nil;
end;

destructor TGEDCOMTagWithLists.Destroy;
begin
  if (FNotes <> nil) then FNotes.Free;
  if (FSourceCitations <> nil) then FSourceCitations.Free;
  if (FMultimediaLinks <> nil) then FMultimediaLinks.Free;
  inherited Destroy;
end;

function TGEDCOMTagWithLists.GetMultimediaLinks(Index: Integer): TGEDCOMMultimediaLink;
begin
  if (FMultimediaLinks = nil)
  then Result := nil
  else Result := TGEDCOMMultimediaLink(FMultimediaLinks[Index]);
end;

function TGEDCOMTagWithLists.GetMultimediaLinksCount: Integer;
begin
  if (FMultimediaLinks = nil)
  then Result := 0
  else Result := FMultimediaLinks.Count;
end;

function TGEDCOMTagWithLists.GetNotes(Index: Integer): TGEDCOMNotes;
begin
  if (FNotes = nil)
  then Result := nil
  else Result := TGEDCOMNotes(FNotes[Index]);
end;

function TGEDCOMTagWithLists.GetNotesCount: Integer;
begin
  if (FNotes = nil)
  then Result := 0
  else Result := FNotes.Count;
end;

function TGEDCOMTagWithLists.GetSourceCitations(
  Index: Integer): TGEDCOMSourceCitation;
begin
  if (FSourceCitations = nil)
  then Result := nil
  else Result := TGEDCOMSourceCitation(FSourceCitations[Index]);
end;

function TGEDCOMTagWithLists.GetSourceCitationsCount: Integer;
begin
  if (FSourceCitations = nil)
  then Result := 0
  else Result := FSourceCitations.Count;
end;

function TGEDCOMTagWithLists.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and (GetNotesCount = 0) and
    (GetSourceCitationsCount = 0) and (GetMultimediaLinksCount = 0);
end;

procedure TGEDCOMTagWithLists.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);

  if (FNotes <> nil) then FNotes.ResetOwner(AOwner);
  if (FSourceCitations <> nil) then FSourceCitations.ResetOwner(AOwner);
  if (FMultimediaLinks <> nil) then FMultimediaLinks.ResetOwner(AOwner);
end;

procedure TGEDCOMTagWithLists.ReplaceXRefs(aMap: TXRefReplaceMap);
begin
  inherited ReplaceXRefs(aMap);

  if (FNotes <> nil) then FNotes.ReplaceXRefs(aMap);
  if (FSourceCitations <> nil) then FSourceCitations.ReplaceXRefs(aMap);
  if (FMultimediaLinks <> nil) then FMultimediaLinks.ReplaceXRefs(aMap);
end;

procedure TGEDCOMTagWithLists.SaveToStream(AStream: TStream);
begin
  inherited SaveToStream(AStream);

  if Assigned(FNotes) then FNotes.SaveToStream(AStream);
  if Assigned(FSourceCitations) then FSourceCitations.SaveToStream(AStream);
  if Assigned(FMultimediaLinks) then FMultimediaLinks.SaveToStream(AStream);
end;

{ TGEDCOMRecord }

constructor TGEDCOMRecord.Create(AOwner, AParent: TGEDCOMObject; ALists: TGEDCOMLists);
begin
  inherited Create(AOwner, AParent);
  FLists := ALists;
  FNotes := nil;
  FSourceCitations := nil;
  FMultimediaLinks := nil;
end;

destructor TGEDCOMRecord.Destroy;
begin
  if (FNotes <> nil) then FNotes.Free;
  if (FSourceCitations <> nil) then FSourceCitations.Free;
  if (FMultimediaLinks <> nil) then FMultimediaLinks.Free;
  inherited Destroy;
end;

function TGEDCOMRecord.NewXRef(): string;
begin
  if (FOwner <> nil) then begin
    FXRef := TGEDCOMTree(FOwner).XRefIndex_NewXRef(Self);
    XRef := FXRef;
  end;
end;

function TGEDCOMRecord.AddMultimediaLink(
  AMultimediaLink: TGEDCOMMultimediaLink): TGEDCOMMultimediaLink;
begin
  Result := AMultimediaLink;

  if (FMultimediaLinks = nil)
  then FMultimediaLinks := TGEDCOMList.Create(Self);

  if (AMultimediaLink <> nil)
  then FMultimediaLinks.Add(AMultimediaLink);
end;

procedure TGEDCOMRecord.DeleteNotes(aIndex: Integer);
begin
  if (FNotes <> nil)
  then FNotes.Delete(aIndex);
end;

procedure TGEDCOMRecord.DeleteSourceCitation(aIndex: Integer);
begin
  if (FSourceCitations <> nil)
  then FSourceCitations.Delete(aIndex);
end;

procedure TGEDCOMRecord.DeleteMultimediaLink(aIndex: Integer);
begin
  if (FMultimediaLinks <> nil)
  then FMultimediaLinks.Delete(aIndex);
end;

function TGEDCOMRecord.AddNotes(ANotes: TGEDCOMNotes): TGEDCOMNotes;
begin
  Result := ANotes;

  if (FNotes = nil)
  then FNotes := TGEDCOMList.Create(Self);

  if (ANotes <> nil)
  then FNotes.Add(ANotes);
end;

function TGEDCOMRecord.AddSourceCitation(
  ASourceCitation: TGEDCOMSourceCitation): TGEDCOMSourceCitation;
begin
  Result := ASourceCitation;

  if (FSourceCitations = nil)
  then FSourceCitations := TGEDCOMList.Create(Self);

  if (ASourceCitation <> nil)
  then FSourceCitations.Add(ASourceCitation);
end;

function TGEDCOMRecord.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);

  if (SU = 'CHAN')
  then Result := inherited AddTag(ATag, AValue, TGEDCOMChangeDate)
  else
  if (SU = 'NOTE') and (stNotes in FLists)
  then Result := AddNotes(TGEDCOMNotes.CreateTag(Owner, Self, SU, AValue))
  else
  if (SU = 'SOUR') and (stSource in FLists)
  then Result := AddSourceCitation(TGEDCOMSourceCitation.CreateTag(Owner, Self, SU, AValue))
  else
  if (SU = 'OBJE') and (stMultimedia in FLists)
  then Result := AddMultimediaLink(TGEDCOMMultimediaLink.CreateTag(Owner, Self, SU, AValue))
  else
    Result := inherited AddTag(ATag, AValue, AClass);
end;

procedure TGEDCOMRecord.Clear;
begin
  inherited Clear;

  if (FNotes <> nil) then FNotes.Clear;
  if (FSourceCitations <> nil) then FSourceCitations.Clear;
  if (FMultimediaLinks <> nil) then FMultimediaLinks.Clear;
end;

function TGEDCOMRecord.GetMultimediaLinks(Index: Integer): TGEDCOMMultimediaLink;
begin
  if (FMultimediaLinks = nil)
  then Result := nil
  else Result := TGEDCOMMultimediaLink(FMultimediaLinks[Index]);
end;

function TGEDCOMRecord.GetMultimediaLinksCount: Integer;
begin
  if (FMultimediaLinks = nil)
  then Result := 0
  else Result := FMultimediaLinks.Count;
end;

function TGEDCOMRecord.GetNotes(Index: Integer): TGEDCOMNotes;
begin
  if (FNotes = nil)
  then Result := nil
  else Result := TGEDCOMNotes(FNotes[Index]);
end;

function TGEDCOMRecord.GetNotesCount: Integer;
begin
  if (FNotes = nil)
  then Result := 0
  else Result := FNotes.Count;
end;

function TGEDCOMRecord.GetSourceCitations(Index: Integer): TGEDCOMSourceCitation;
begin
  if (FSourceCitations = nil)
  then Result := nil
  else Result := TGEDCOMSourceCitation(FSourceCitations[Index]);
end;

function TGEDCOMRecord.GetSourceCitationsCount: Integer;
begin
  if (FSourceCitations = nil)
  then Result := 0
  else Result := FSourceCitations.Count;
end;

function TGEDCOMRecord.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and (GetNotesCount = 0) and
    (GetSourceCitationsCount = 0) and (GetMultimediaLinksCount = 0);
end;

function TGEDCOMRecord.GetChangeDate: TGEDCOMChangeDate;
begin
  Result := TGEDCOMChangeDate(TagClass('CHAN', TGEDCOMChangeDate));
end;

procedure TGEDCOMRecord.SaveToStream(AStream: TStream);
begin
  inherited SaveToStream(AStream);

  if Assigned(FNotes) then FNotes.SaveToStream(AStream);
  if Assigned(FSourceCitations) then FSourceCitations.SaveToStream(AStream);
  if Assigned(FMultimediaLinks) then FMultimediaLinks.SaveToStream(AStream);
end;

procedure TGEDCOMRecord.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);

  if (FNotes <> nil) then FNotes.ResetOwner(AOwner);
  if (FSourceCitations <> nil) then FSourceCitations.ResetOwner(AOwner);
  if (FMultimediaLinks <> nil) then FMultimediaLinks.ResetOwner(AOwner);
end;

procedure TGEDCOMRecord.ReplaceXRefs(aMap: TXRefReplaceMap);
begin
  if (FNotes <> nil) then FNotes.ReplaceXRefs(aMap);
  if (FSourceCitations <> nil) then FSourceCitations.ReplaceXRefs(aMap);
  if (FMultimediaLinks <> nil) then FMultimediaLinks.ReplaceXRefs(aMap);
end;

procedure TGEDCOMRecord.MoveTo(aToRecord: TGEDCOMRecord);
var
  i: Integer;
  tag: TGEDCOMCustomTag;
begin
  if (FTags <> nil) then begin
    for i := FTags.Count - 1 downto 0 do begin
      tag := TGEDCOMCustomTag(FTags.Extract(i));
      if (tag.Name = 'CHAN')
      then tag.Destroy
      else aToRecord.FTags.Add(tag);
    end;
  end;

  //

  if (FNotes <> nil) then begin
    for i := FNotes.Count - 1 downto 0 do begin
      aToRecord.AddNotes(TGEDCOMNotes(FNotes.Extract(i)));
    end;
  end;

  if (FMultimediaLinks <> nil) then begin
    for i := FMultimediaLinks.Count - 1 downto 0 do begin
      aToRecord.AddMultimediaLink(TGEDCOMMultimediaLink(FMultimediaLinks.Extract(i)));
    end;
  end;

  if (FSourceCitations <> nil) then begin
    for i := FSourceCitations.Count - 1 downto 0 do begin
      aToRecord.AddSourceCitation(TGEDCOMSourceCitation(FSourceCitations.Extract(i)));
    end;
  end;
end;

{ TGEDCOMDateExact }

constructor TGEDCOMDateExact.Create(AOwner, AParent: TGEDCOMObject;
  ADateTime: TDateTime);
begin
  Create(AOwner, AParent);
  Date := ADateTime;
end;

constructor TGEDCOMDateExact.CreateNow(AOwner, AParent: TGEDCOMObject);
begin
  Create(AOwner, AParent);
  Date := Now;
end;

{ TGEDCOMTime }

procedure TGEDCOMTime.Clear;
begin
  inherited Clear;
  FHour := 0;
  FMinutes := 0;
  FSeconds := 0;
  FFraction := 0;
end;

constructor TGEDCOMTime.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FName := 'TIME';
end;

function TGEDCOMTime.GetStringValue: string;
begin
  if (FHour = 0) and (FMinutes = 0) and (FSeconds = 0)
  then Result := ''
  else begin
    Result := Format('%.2d:%.2d:%.2d', [FHour, FMinutes, FSeconds]);
    if FFraction > 0 then
      Result := Result + '.' + IntToStr(FFraction);
  end;
end;

function TGEDCOMTime.GetValue: TDateTime;
begin
  Result := EncodeTime(FHour, FMinutes, FSeconds, 100*FFraction);
end;

function TGEDCOMTime.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and (FHour = 0) and (FMinutes = 0) and (FSeconds = 0);
end;

function TGEDCOMTime.ParseString(const AString: string): string;
var
  H, M, S, F: Integer;
begin
  FHour := 0;
  FMinutes := 0;
  FSeconds := 0;
  FFraction := 0;
  Result := AString;

  if (Result <> '') then begin
    Result := ExtractDelimiter(Result);
    Result := ExtractNumber(Result, H);
    FHour := H;

    if Copy(Result,1,1) = ':' then
      StrDelete(Result,1,1);

    Result := ExtractNumber(Result, M);
    FMinutes := M;

    if Copy(Result,1,1) = ':' then begin
      StrDelete(Result,1,1);
      Result := ExtractNumber(Result, S);
      FSeconds := S;

      if Copy(Result,1,1) = '.' then begin
        StrDelete(Result,1,1);
        Result := ExtractNumber(Result, F);
        FFraction := F;
      end;
    end;
  end;
end;

procedure TGEDCOMTime.SetValue(AValue: TDateTime);
var
  MSec: Word;
begin
  DecodeTime(AValue, FHour, FMinutes, FSeconds, MSec);
  FFraction := Trunc(MSec / 100); // error if FFraction=1000
end;

{ TGEDCOMFamilyRecord }

constructor TGEDCOMFamilyRecord.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent, [stNotes, stSource, stMultimedia]);
  FName := 'FAM';
  FFamilyEvents := nil;
  FChildren := nil;
  FUserReferences := nil;
end;

destructor TGEDCOMFamilyRecord.Destroy;
begin
  if (FFamilyEvents <> nil) then FFamilyEvents.Free;
  if (FChildren <> nil) then FChildren.Free;
  if (FUserReferences <> nil) then FUserReferences.Free;
  inherited Destroy;
end;

procedure TGEDCOMFamilyRecord.Clear;
begin
  inherited Clear;
  if (FFamilyEvents <> nil) then FFamilyEvents.Clear;
  if (FChildren <> nil) then FChildren.Clear;
  if (FUserReferences <> nil) then FUserReferences.Clear;
end;

function TGEDCOMFamilyRecord.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);
  if (SU = 'CHIL')
  then Result := AddChild(TGEDCOMPointer.CreateTag(Owner, Self, SU, AValue))
  else
  if (SU = 'ANUL') or (SU = 'CENS') or (SU = 'DIV') or (SU = 'DIVF')
  or (SU = 'ENGA') or (SU = 'MARB') or (SU = 'MARC') or (SU = 'MARR')
  or (SU = 'MARL') or (SU = 'MARS') or (SU = 'RESI') or (SU = 'EVEN')
  then Result := AddFamilyEvent(TGEDCOMFamilyEvent.CreateTag(Owner, Self, SU, AValue))
  else
  if (SU = 'REFN')
  then Result := AddUserReference(TGEDCOMUserReference.CreateTag(Owner, Self, SU, AValue))
  else
  if (SU = 'HUSB') or (SU = 'WIFE')
  then Result := inherited AddTag(ATag, AValue, TGEDCOMPointer)
  else
    Result := inherited AddTag(ATag, AValue, AClass);
end;

function TGEDCOMFamilyRecord.GetHusband: TGEDCOMPointer;
begin
  Result := TGEDCOMPointer(TagClass('HUSB', TGEDCOMPointer));
end;

function TGEDCOMFamilyRecord.GetWife: TGEDCOMPointer;
begin
  Result := TGEDCOMPointer(TagClass('WIFE', TGEDCOMPointer));
end;

function TGEDCOMFamilyRecord.AddChild(APointer: TGEDCOMPointer): TGEDCOMPointer;
begin
  Result := APointer;

  if (FChildren = nil)
  then FChildren := TGEDCOMList.Create(Self);

  if (APointer <> nil)
  then FChildren.Add(APointer);
end;

procedure TGEDCOMFamilyRecord.RemoveChild(XRef: string);
var
  i: Integer;
begin
  if (FChildren <> nil) then begin
    for i := FChildren.Count - 1 downto 0 do
      if (TGEDCOMPointer(FChildren[i]).XRef = XRef) then begin
        FChildren.DeleteObject(FChildren[i]);
        Break;
      end;
  end;
end;

function TGEDCOMFamilyRecord.AddFamilyEvent(
  AFamilyEvent: TGEDCOMFamilyEvent): TGEDCOMFamilyEvent;
begin
  Result := AFamilyEvent;

  if (FFamilyEvents = nil)
  then FFamilyEvents := TGEDCOMList.Create(Self);

  if (AFamilyEvent <> nil)
  then FFamilyEvents.Add(AFamilyEvent);
end;

procedure TGEDCOMFamilyRecord.DeleteFamilyEvent(aEvent: TGEDCOMFamilyEvent);
begin
  if (FFamilyEvents <> nil)
  then FFamilyEvents.DeleteObject(aEvent);
end;

function TGEDCOMFamilyRecord.AddUserReference(
  AUserReference: TGEDCOMUserReference): TGEDCOMUserReference;
begin
  Result := AUserReference;

  if (FUserReferences = nil)
  then FUserReferences := TGEDCOMList.Create(Self);

  if (FUserReferences <> nil)
  then FUserReferences.Add(AUserReference);
end;

function TGEDCOMFamilyRecord.GetChildren(Index: Integer): TGEDCOMPointer;
begin
  if (FChildren = nil)
  then Result := nil
  else Result := TGEDCOMPointer(FChildren[Index]);
end;

function TGEDCOMFamilyRecord.GetChildrenCount: Integer;
begin
  if (FChildren = nil)
  then Result := 0
  else Result := FChildren.Count;
end;

function TGEDCOMFamilyRecord.GetFamilyEventCount: Integer;
begin
  if (FFamilyEvents = nil)
  then Result := 0
  else Result := FFamilyEvents.Count;
end;

function TGEDCOMFamilyRecord.GetFamilyEvents(Index: Integer): TGEDCOMFamilyEvent;
begin
  if (FFamilyEvents = nil)
  then Result := nil
  else Result := TGEDCOMFamilyEvent(FFamilyEvents[Index]);
end;

function TGEDCOMFamilyRecord.GetRestriction: TGEDCOMRestriction;
var
  S: string;
begin
  S := UpperCase(Trim(TagStringValue('RESN')));
  if (S = 'CONFIDENTIAL')
  then Result := rnConfidential
  else
  if (S = 'LOCKED')
  then Result := rnLocked
  else
  if (S = 'PRIVACY')
  then Result := rnPrivacy
  else Result := rnNone;   // default no restriction
end;

function TGEDCOMFamilyRecord.GetStringTag(Index: Integer): string;
begin
  case Index of
    1: Result := TagStringValue('RIN');   // Automated record ID
  end;
end;

function TGEDCOMFamilyRecord.GetSubmittor: TGEDCOMPointer;
begin
  Result := TGEDCOMPointer(TagClass('SUBM', TGEDCOMPointer));
end;

function TGEDCOMFamilyRecord.GetUserReferences(
  Index: Integer): TGEDCOMUserReference;
begin
  if (FUserReferences = nil)
  then Result := nil
  else Result := TGEDCOMUserReference(FUserReferences[Index]);
end;

function TGEDCOMFamilyRecord.GetUserReferencesCount: Integer;
begin
  if (FUserReferences = nil)
  then Result := 0
  else Result := FUserReferences.Count;
end;

function TGEDCOMFamilyRecord.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and (GetFamilyEventCount = 0) and
    (GetChildrenCount = 0) and (GetUserReferencesCount = 0);
end;

procedure TGEDCOMFamilyRecord.SaveToStream(AStream: TStream);
begin
  inherited SaveToStream(AStream);

  if Assigned(FChildren) then FChildren.SaveToStream(AStream);
  if Assigned(FFamilyEvents) then FFamilyEvents.SaveToStream(AStream);
  if Assigned(FUserReferences) then FUserReferences.SaveToStream(AStream);
end;

procedure TGEDCOMFamilyRecord.SetRestriction(const Value: TGEDCOMRestriction);
var
  S: string;
begin
  case Value of
    rnNone: S := '';
    rnConfidential: S := 'confidential';
    rnLocked: S := 'locked';
    rnPrivacy: S := 'privacy';
  end;
  SetTagStringValue('RESN', S);
end;

procedure TGEDCOMFamilyRecord.SetStringTag(Index: Integer; const Value: string);
begin
  case Index of
    1: SetTagStringValue('RIN', Value);   // Automated record ID
  end;
end;

procedure TGEDCOMFamilyRecord.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);

  if (FChildren <> nil) then FChildren.ResetOwner(AOwner);
  if (FFamilyEvents <> nil) then FFamilyEvents.ResetOwner(AOwner);
  if (FUserReferences <> nil) then FUserReferences.ResetOwner(AOwner);
end;

procedure TGEDCOMFamilyRecord.ReplaceXRefs(aMap: TXRefReplaceMap);
begin
  inherited ReplaceXRefs(aMap);

  if (Husband <> nil)
  then Husband.StringValue := EncloseXRef(aMap.FindNewXRef(Husband.StringValue));

  if (Wife <> nil)
  then Wife.StringValue := EncloseXRef(aMap.FindNewXRef(Wife.StringValue));

  if (FChildren <> nil) then FChildren.ReplaceXRefs(aMap);
  if (FFamilyEvents <> nil) then FFamilyEvents.ReplaceXRefs(aMap);
  if (FUserReferences <> nil) then FUserReferences.ReplaceXRefs(aMap);
end;

procedure TGEDCOMFamilyRecord.MoveTo(aToRecord: TGEDCOMRecord);
var
  i: Integer;
  toRec: TGEDCOMFamilyRecord;
begin
  inherited MoveTo(aToRecord);

  toRec := TGEDCOMFamilyRecord(aToRecord);

  if (FFamilyEvents <> nil) then begin
    for i := FFamilyEvents.Count - 1 downto 0 do begin
      toRec.AddFamilyEvent(TGEDCOMFamilyEvent(FFamilyEvents.Extract(i)));
    end;
  end;

  if (FChildren <> nil) then begin
    for i := FChildren.Count - 1 downto 0 do begin
      toRec.AddChild(TGEDCOMPointer(FChildren.Extract(i)));
    end;
  end;

  if (FUserReferences <> nil) then begin
    for i := FUserReferences.Count - 1 downto 0 do begin
      toRec.AddUserReference(TGEDCOMUserReference(FUserReferences.Extract(i)));
    end;
  end;
end;

procedure TGEDCOMFamilyRecord.SortChilds();
var
  i, k: Integer;
  iChild, kChild: TGEDCOMIndividualRecord;
  iEv, kEv: TGEDCOMIndividualEvent;
  iDate, kDate: TDateTime;
begin
  if (FChildren = nil) then Exit;

  for i := 0 to FChildren.Count - 1 do begin
    for k := i + 1 to FChildren.Count - 1 do begin
      iChild := TGEDCOMIndividualRecord(Children[i].Value);
      iEv := GetIndividualEvent(iChild, 'BIRT');
      if (iEv <> nil)
      then iDate := GEDCOMDateToDate(iEv.Detail.Date.Value)
      else iDate := 0;

      kChild := TGEDCOMIndividualRecord(Children[k].Value);
      kEv := GetIndividualEvent(kChild, 'BIRT');
      if (kEv <> nil)
      then kDate := GEDCOMDateToDate(kEv.Detail.Date.Value)
      else kDate := 0;

      if (iDate > kDate)
      then FChildren.Exchange(i, k);
    end;
  end;
end;

{ TGEDCOMIndividualRecord }

constructor TGEDCOMIndividualRecord.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent, [stNotes, stSource, stMultimedia]);

  FName := 'INDI';
  FPersonalNames := nil;
  FIndividualEvents := nil;
  FIndividualAttributes := nil;
  FChildToFamilyLinks := nil;
  FSpouseToFamilyLinks := nil;
  FSubmittors := nil;
  FAssociations := nil;
  FUserReferences := nil;
  FNotes := nil;
  FSourceCitations := nil;
  FMultimediaLinks := nil;
  FGroups := nil;
end;

destructor TGEDCOMIndividualRecord.Destroy;
begin
  if (FPersonalNames <> nil) then FPersonalNames.Free;
  if (FIndividualEvents <> nil) then FIndividualEvents.Free;
  if (FIndividualAttributes <> nil) then FIndividualAttributes.Free;
  if (FChildToFamilyLinks <> nil) then FChildToFamilyLinks.Free;
  if (FSpouseToFamilyLinks <> nil) then FSpouseToFamilyLinks.Free;
  if (FSubmittors <> nil) then FSubmittors.Free;
  if (FAssociations <> nil) then FAssociations.Free;
  if (FUserReferences <> nil) then FUserReferences.Free;
  if (FGroups <> nil) then FGroups.Free;

  inherited Destroy;
end;

procedure TGEDCOMIndividualRecord.Clear();
begin
  inherited Clear();

  if (FPersonalNames <> nil) then FPersonalNames.Clear;
  if (FIndividualEvents <> nil) then FIndividualEvents.Clear;
  if (FIndividualAttributes <> nil) then FIndividualAttributes.Clear;
  if (FChildToFamilyLinks <> nil) then FChildToFamilyLinks.Clear;
  if (FSpouseToFamilyLinks <> nil) then FSpouseToFamilyLinks.Clear;
  if (FSubmittors <> nil) then FSubmittors.Clear;
  if (FAssociations <> nil) then FAssociations.Clear;
  if (FUserReferences <> nil) then FUserReferences.Clear;
  if (FGroups <> nil) then FGroups.Clear;
end;

function TGEDCOMIndividualRecord.AddGroup(Value: TGEDCOMPointer): TGEDCOMPointer;
begin
  Result := Value;

  if (FGroups = nil)
  then FGroups := TGEDCOMList.Create(Self);

  if (Value <> nil)
  then FGroups.Add(Value);
end;

procedure TGEDCOMIndividualRecord.DeleteGroup(aIndex: Integer);
begin
  if (FGroups <> nil)
  then FGroups.Delete(aIndex);
end;

function TGEDCOMIndividualRecord.IndexOfGroup(aGroup: TGEDCOMGroupRecord): Integer;
var
  i: Integer;
begin
  Result := -1;
  if (FGroups = nil) then Exit;

  for i := 0 to FGroups.Count - 1 do
    if (CleanXRef(TGEDCOMPointer(FGroups[i]).XRef) = CleanXRef(aGroup.XRef)) then begin
      Result := i;
      Break;
    end;
end;

function TGEDCOMIndividualRecord.AddAssociation(Value: TGEDCOMAssociation): TGEDCOMAssociation;
begin
  Result := Value;

  if (FAssociations = nil)
  then FAssociations := TGEDCOMList.Create(Self);

  if (Value <> nil)
  then FAssociations.Add(Value);
end;

//

function TGEDCOMIndividualRecord.AddIndividualAttribute(Value: TGEDCOMIndividualAttribute): TGEDCOMIndividualAttribute;
begin
  Result := Value;

  if (FIndividualAttributes = nil)
  then FIndividualAttributes := TGEDCOMList.Create(Self);

  if (Value <> nil) then begin
    Value.SetLevel(Level + 1);
    FIndividualAttributes.Add(Value);
  end;
end;

procedure TGEDCOMIndividualRecord.DeleteIndividualAttribute(anAttribute: TGEDCOMIndividualAttribute);
begin
  if (FIndividualAttributes <> nil)
  then FIndividualAttributes.DeleteObject(anAttribute);
end;

//

function TGEDCOMIndividualRecord.AddIndividualEvent(Value: TGEDCOMIndividualEvent): TGEDCOMIndividualEvent;
begin
  Result := Value;

  if (FIndividualEvents = nil)
  then FIndividualEvents := TGEDCOMList.Create(Self);

  if (Value <> nil) then begin
    Value.SetLevel(Level + 1);
    FIndividualEvents.Add(Value);
  end;
end;

procedure TGEDCOMIndividualRecord.DeleteIndividualEvent(aEvent: TGEDCOMIndividualEvent);
begin
  if (FIndividualEvents <> nil)
  then FIndividualEvents.DeleteObject(aEvent);
end;

//

function TGEDCOMIndividualRecord.AddPersonalName(Value: TGEDCOMPersonalName): TGEDCOMPersonalName;
begin
  Result := Value;

  if (FPersonalNames = nil)
  then FPersonalNames := TGEDCOMList.Create(Self);

  if (Value <> nil) then begin
    Value.SetLevel(Level + 1);
    FPersonalNames.Add(Value);
  end;
end;

//

function TGEDCOMIndividualRecord.AddSpouseToFamilyLink(Value: TGEDCOMSpouseToFamilyLink): TGEDCOMSpouseToFamilyLink;
begin
  Result := Value;

  if (FSpouseToFamilyLinks = nil)
  then FSpouseToFamilyLinks := TGEDCOMList.Create(Self);

  if (Value <> nil)
  then FSpouseToFamilyLinks.Add(Value);
end;

procedure TGEDCOMIndividualRecord.DeleteSpouseToFamilyLink(Family: TGEDCOMFamilyRecord);
var
  i: Integer;
begin
  for i := 0 to FSpouseToFamilyLinks.Count - 1 do
    if (TGEDCOMSpouseToFamilyLink(FSpouseToFamilyLinks[i]).Family = Family) then begin
      FSpouseToFamilyLinks.Delete(i);
      Break;
    end;
end;

//

function TGEDCOMIndividualRecord.AddChildToFamilyLink(Value: TGEDCOMChildToFamilyLink): TGEDCOMChildToFamilyLink;
begin
  Result := Value;

  if (FChildToFamilyLinks = nil)
  then FChildToFamilyLinks := TGEDCOMList.Create(Self);

  if (Value <> nil)
  then FChildToFamilyLinks.Add(Value);
end;

procedure TGEDCOMIndividualRecord.DeleteChildToFamilyLink(Family: TGEDCOMFamilyRecord);
var
  i: Integer;
begin
  for i := 0 to FChildToFamilyLinks.Count - 1 do
    if (TGEDCOMChildToFamilyLink(FChildToFamilyLinks[i]).Family = Family) then begin
      FChildToFamilyLinks.Delete(i);
      Break;
    end;
end;

//

procedure TGEDCOMIndividualRecord.DeleteAssociation(aIndex: Integer);
begin
  if (FAssociations <> nil)
  then FAssociations.Delete(aIndex);
end;

//

function TGEDCOMIndividualRecord.AddSubmittor(Value: TGEDCOMPointer): TGEDCOMPointer;
begin
  Result := Value;

  if (FSubmittors = nil)
  then FSubmittors := TGEDCOMList.Create(Self);

  if (Value <> nil)
  then FSubmittors.Add(Value);
end;

function TGEDCOMIndividualRecord.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
const
  EventsList = '';
var
  SU: string;
begin
  SU := UpperCase(ATag);

  if (SU = 'NAME')
  then Result := AddPersonalName(TGEDCOMPersonalName.CreateTag(Owner, Self, SU, AValue))
  else
  if (SU = 'BIRT') or (SU = 'CHR') or (SU = 'DEAT') or (SU = 'BURI')
  or (SU = 'CREM') or (SU = 'ADOP') or (SU = 'BAPM') or (SU = 'BARM')
  or (SU = 'BASM') or (SU = 'BLES') or (SU = 'CHRA') or (SU = 'CONF')
  or (SU = 'FCOM') or (SU = 'ORDN') or (SU = 'NATU') or (SU = 'EMIG')
  or (SU = 'IMMI') or (SU = 'CENS') or (SU = 'PROB') or (SU = 'WILL')
  or (SU = 'GRAD') or (SU = 'RETI') or (SU = 'EVEN')
  then Result := AddIndividualEvent(TGEDCOMIndividualEvent.CreateTag(Owner, Self, SU, AValue))
  else
  if (SU = 'CAST') or (SU = 'DSCR') or (SU = 'EDUC') or (SU = 'IDNO')
  or (SU = 'NATI') or (SU = 'NCHI') or (SU = 'NMR') or (SU = 'OCCU')
  or (SU = 'PROP') or (SU = 'RELI') or (SU = 'RESI') or (SU = 'SSN')
  or (SU = 'TITL') or (SU = 'FACT')

  or (SU = '_HOBBY') or (SU = '_AWARD')
  or (SU = '_MILI') or (SU = '_MILI_IND') or (SU = '_MILI_DIS') or (SU = '_MILI_RANK')

  then Result := AddIndividualAttribute(TGEDCOMIndividualAttribute.CreateTag(Owner, Self, SU, AValue))
  else
  if (SU = 'SUBM')
  then Result := AddSubmittor(TGEDCOMPointer.CreateTag(Owner, Self, SU, AValue))
  else
  if (SU = 'FAMC')
  then Result := AddChildToFamilyLink(TGEDCOMChildToFamilyLink.CreateTag(Owner, Self, SU, AValue))
  else
  if (SU = 'FAMS')
  then Result := AddSpouseToFamilyLink(TGEDCOMSpouseToFamilyLink.CreateTag(Owner, Self, SU, AValue))
  else
  if (SU = 'ASSO')
  then Result := AddAssociation(TGEDCOMAssociation.CreateTag(Owner, Self, SU, AValue))
  else
  if (SU = 'REFN')
  then Result := AddUserReference(TGEDCOMUserReference.CreateTag(Owner, Self, SU, AValue))
  // zsv
  else
  if (SU = '_GROUP')
  then Result := AddGroup(TGEDCOMPointer.CreateTag(Owner, Self, SU, AValue))
  // zsv
  else Result := inherited AddTag(ATag, AValue, AClass);
end;

function TGEDCOMIndividualRecord.AddUserReference(AUserReference: TGEDCOMUserReference): TGEDCOMUserReference;
begin
  Result := AUserReference;

  if (FUserReferences = nil)
  then FUserReferences := TGEDCOMList.Create(Self);

  if (FUserReferences <> nil)
  then FUserReferences.Add(AUserReference);
end;

function TGEDCOMIndividualRecord.GetGroups(Index: Integer): TGEDCOMPointer;
begin
  if (FGroups = nil)
  then Result := nil
  else Result := TGEDCOMPointer(FGroups[Index]);
end;

function TGEDCOMIndividualRecord.GetGroupsCount: Integer;
begin
  if (FGroups = nil)
  then Result := 0
  else Result := FGroups.Count;
end;

function TGEDCOMIndividualRecord.GetAssociations(Index: Integer): TGEDCOMAssociation;
begin
  if (FAssociations = nil)
  then Result := nil
  else Result := TGEDCOMAssociation(FAssociations[Index]);
end;

function TGEDCOMIndividualRecord.GetAssociationsCount: Integer;
begin
  if (FAssociations = nil)
  then Result := 0
  else Result := FAssociations.Count;
end;

function TGEDCOMIndividualRecord.GetChildToFamilyLinks(Index: Integer): TGEDCOMChildToFamilyLink;
begin
  if (FChildToFamilyLinks = nil)
  then Result := nil
  else Result := TGEDCOMChildToFamilyLink(FChildToFamilyLinks[Index]);
end;

function TGEDCOMIndividualRecord.GetChildToFamilyLinksCount: Integer;
begin
  if (FChildToFamilyLinks = nil)
  then Result := 0
  else Result := FChildToFamilyLinks.Count;
end;

function TGEDCOMIndividualRecord.GetIndividualAttributes(
  Index: Integer): TGEDCOMIndividualAttribute;
begin
  if (FIndividualAttributes = nil)
  then Result := nil
  else Result := TGEDCOMIndividualAttribute(FIndividualAttributes[Index]);
end;

function TGEDCOMIndividualRecord.GetIndividualAttributesCount: Integer;
begin
  if (FIndividualAttributes = nil)
  then Result := 0
  else Result := FIndividualAttributes.Count;
end;

function TGEDCOMIndividualRecord.GetIndividualEvents(
  Index: Integer): TGEDCOMIndividualEvent;
begin
  if (FIndividualEvents = nil)
  then Result := nil
  else Result := TGEDCOMIndividualEvent(FIndividualEvents[Index]);
end;

function TGEDCOMIndividualRecord.GetIndividualEventsCount: Integer;
begin
  if (FIndividualEvents = nil)
  then Result := 0
  else Result := FIndividualEvents.Count;
end;

function TGEDCOMIndividualRecord.GetPersonalNames(Index: Integer): TGEDCOMPersonalName;
begin
  if (FPersonalNames = nil)
  then Result := nil
  else Result := TGEDCOMPersonalName(FPersonalNames[Index]);
end;

function TGEDCOMIndividualRecord.GetPersonalNamesCount: Integer;
begin
  if (FPersonalNames = nil)
  then Result := 0
  else Result := FPersonalNames.Count;
end;

function TGEDCOMIndividualRecord.GetRestriction: TGEDCOMRestriction;
var
  S: string;
begin
  S := UpperCase(Trim(TagStringValue('RESN')));

  if (S = 'CONFIDENTIAL')
  then Result := rnConfidential
  else
  if (S = 'LOCKED')
  then Result := rnLocked
  else
  if (S = 'PRIVACY')
  then Result := rnPrivacy
  else Result := rnNone;   // default no restriction
end;

function TGEDCOMIndividualRecord.GetSex: TGEDCOMSex;
var
  S: string;
begin
  S := UpperCase(Trim(TagStringValue('SEX')));

  if (S = '')
  then Result := svNone
  else
  if (S = 'M')
  then Result := svMale
  else
  if (S = 'F')
  then Result := svFemale
  else
  if (S = 'U')
  then Result := svUndetermined
  else Result := svNone;
end;

function TGEDCOMIndividualRecord.GetSpouseToFamilyLinks(
  Index: Integer): TGEDCOMSpouseToFamilyLink;
begin
  if (FSpouseToFamilyLinks = nil)
  then Result := nil
  else Result := TGEDCOMSpouseToFamilyLink(FSpouseToFamilyLinks[Index]);
end;

function TGEDCOMIndividualRecord.GetSpouseToFamilyLinksCount: Integer;
begin
  if (FSpouseToFamilyLinks = nil)
  then Result := 0
  else Result := FSpouseToFamilyLinks.Count;
end;

function TGEDCOMIndividualRecord.GetStringTag(Index: Integer): string;
begin
  case Index of
    1: Result := TagStringValue('RFN');
    2: Result := TagStringValue('AFN');
    3: Result := TagStringValue('RIN');   // Automated record ID
  end;
end;

function TGEDCOMIndividualRecord.GetSubmittors(Index: Integer): TGEDCOMPointer;
begin
  if (FSubmittors = nil)
  then Result := nil
  else Result := TGEDCOMPointer(FSubmittors[Index]);
end;

function TGEDCOMIndividualRecord.GetSubmittorsCount: Integer;
begin
  if (FSubmittors = nil)
  then Result := 0
  else Result := FSubmittors.Count;
end;

function TGEDCOMIndividualRecord.GetUserReferences(Index: Integer): TGEDCOMUserReference;
begin
  if (FUserReferences = nil)
  then Result := nil
  else Result := TGEDCOMUserReference(FUserReferences[Index]);
end;

function TGEDCOMIndividualRecord.GetUserReferencesCount: Integer;
begin
  if (FUserReferences = nil)
  then Result := 0
  else Result := FUserReferences.Count;
end;

function TGEDCOMIndividualRecord.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and (GetPersonalNamesCount = 0) and
    (GetIndividualEventsCount = 0) and (GetIndividualAttributesCount = 0) and
    (GetChildToFamilyLinksCount = 0) and (GetSpouseToFamilyLinksCount = 0) and
    (GetSubmittorsCount = 0) and (GetAssociationsCount = 0) and
    (GetUserReferencesCount = 0) and (GetGroupsCount = 0);
end;

procedure TGEDCOMIndividualRecord.SetRestriction(const Value: TGEDCOMRestriction);
var
  S: string;
begin
  case Value of
    rnNone: S := '';
    rnConfidential: S := 'confidential';
    rnLocked: S := 'locked';
    rnPrivacy: S := 'privacy';
  end;
  SetTagStringValue('RESN', S);
end;

procedure TGEDCOMIndividualRecord.SetSex(const Value: TGEDCOMSex);
var
  S: string;
begin
  case Value of
    svNone: S := '';
    svMale: S := 'M';
    svFemale: S := 'F';
    svUndetermined: S := 'U';
  end;

  SetTagStringValue('SEX', S);
end;

procedure TGEDCOMIndividualRecord.SetStringTag(Index: Integer;
  const Value: string);
begin
  case Index of
    1: SetTagStringValue('RFN', Value);
    2: SetTagStringValue('AFN', Value);
    3: SetTagStringValue('RIN', Value);   // Automated record ID
  end;
end;

procedure TGEDCOMIndividualRecord.SaveToStream(AStream: TStream);
begin
  inherited SaveToStream(AStream);

  if Assigned(FPersonalNames) then FPersonalNames.SaveToStream(AStream);
  if Assigned(FChildToFamilyLinks) then FChildToFamilyLinks.SaveToStream(AStream);
  if Assigned(FSpouseToFamilyLinks) then FSpouseToFamilyLinks.SaveToStream(AStream);

  if Assigned(FIndividualEvents) then FIndividualEvents.SaveToStream(AStream);
  if Assigned(FIndividualAttributes) then FIndividualAttributes.SaveToStream(AStream);

  if Assigned(FSubmittors) then FSubmittors.SaveToStream(AStream);
  if Assigned(FAssociations) then FAssociations.SaveToStream(AStream);
  if Assigned(FUserReferences) then FUserReferences.SaveToStream(AStream);

  if Assigned(FGroups) then FGroups.SaveToStream(AStream);
end;

procedure TGEDCOMIndividualRecord.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);

  if (FPersonalNames <> nil) then FPersonalNames.ResetOwner(AOwner);
  if (FChildToFamilyLinks <> nil) then FChildToFamilyLinks.ResetOwner(AOwner);
  if (FSpouseToFamilyLinks <> nil) then FSpouseToFamilyLinks.ResetOwner(AOwner);
  if (FIndividualEvents <> nil) then FIndividualEvents.ResetOwner(AOwner);
  if (FIndividualAttributes <> nil) then FIndividualAttributes.ResetOwner(AOwner);
  if (FSubmittors <> nil) then FSubmittors.ResetOwner(AOwner);
  if (FAssociations <> nil) then FAssociations.ResetOwner(AOwner);
  if (FUserReferences <> nil) then FUserReferences.ResetOwner(AOwner);
  if (FGroups <> nil) then FGroups.ResetOwner(AOwner);
end;

procedure TGEDCOMIndividualRecord.ReplaceXRefs(aMap: TXRefReplaceMap);
begin
  inherited ReplaceXRefs(aMap);

  if (FPersonalNames <> nil) then FPersonalNames.ReplaceXRefs(aMap);
  if (FChildToFamilyLinks <> nil) then FChildToFamilyLinks.ReplaceXRefs(aMap);
  if (FSpouseToFamilyLinks <> nil) then FSpouseToFamilyLinks.ReplaceXRefs(aMap);

  if (FIndividualEvents <> nil) then FIndividualEvents.ReplaceXRefs(aMap);
  if (FIndividualAttributes <> nil) then FIndividualAttributes.ReplaceXRefs(aMap);

  if (FSubmittors <> nil) then FSubmittors.ReplaceXRefs(aMap);
  if (FAssociations <> nil) then FAssociations.ReplaceXRefs(aMap);
  if (FUserReferences <> nil) then FUserReferences.ReplaceXRefs(aMap);

  if (FGroups <> nil) then FGroups.ReplaceXRefs(aMap);
end;

procedure TGEDCOMIndividualRecord.MoveTo(aToRecord: TGEDCOMRecord);
var
  stf_link: TGEDCOMSpouseToFamilyLink;
  ctf_link: TGEDCOMChildToFamilyLink;
  family: TGEDCOMFamilyRecord;
  i: Integer;
  toRec: TGEDCOMIndividualRecord;
begin
  inherited MoveTo(aToRecord);

  toRec := TGEDCOMIndividualRecord(aToRecord);

  //

  if (FPersonalNames <> nil) then begin
    for i := FPersonalNames.Count - 1 downto 0 do begin
      toRec.AddPersonalName(TGEDCOMPersonalName(FPersonalNames.Extract(i)));
    end;
  end;

  if (toRec.ChildToFamilyLinksCount = 0) and (Self.ChildToFamilyLinksCount <> 0) then begin
    if (FChildToFamilyLinks <> nil) then begin
      ctf_link := TGEDCOMChildToFamilyLink(FChildToFamilyLinks.Extract(0));
      family := ctf_link.Family;

      for i := 0 to family.ChildrenCount - 1 do
        if (family.Children[i].StringValue = '@' + Self.XRef + '@') then begin
          family.Children[i].StringValue := '@' + aToRecord.XRef + '@';
          Break;
        end;

      toRec.AddChildToFamilyLink(ctf_link);
    end;
  end;

  if (FSpouseToFamilyLinks <> nil) then begin
    for i := FSpouseToFamilyLinks.Count - 1 downto 0 do begin
      stf_link := TGEDCOMSpouseToFamilyLink(FSpouseToFamilyLinks.Extract(i));
      family := stf_link.Family;

      if (family.Husband.StringValue = '@' + Self.XRef + '@')
      then family.Husband.StringValue := '@' + aToRecord.XRef + '@'
      else
      if (family.Wife.StringValue = '@' + Self.XRef + '@')
      then family.Wife.StringValue := '@' + aToRecord.XRef + '@';

      toRec.AddSpouseToFamilyLink(stf_link);
    end;
  end;

  //

  if (FIndividualEvents <> nil) then begin
    for i := FIndividualEvents.Count - 1 downto 0 do begin
      toRec.AddIndividualEvent(TGEDCOMIndividualEvent(FIndividualEvents.Extract(i)));
    end;
  end;

  if (FIndividualAttributes <> nil) then begin
    for i := FIndividualAttributes.Count - 1 downto 0 do begin
      toRec.AddIndividualAttribute(TGEDCOMIndividualAttribute(FIndividualAttributes.Extract(i)));
    end;
  end;

  //

  if (FSubmittors <> nil) then begin
    for i := FSubmittors.Count - 1 downto 0 do begin
      toRec.AddSubmittor(TGEDCOMPointer(FSubmittors.Extract(i)));
    end;
  end;

  if (FAssociations <> nil) then begin
    for i := FAssociations.Count - 1 downto 0 do begin
      toRec.AddAssociation(TGEDCOMAssociation(FAssociations.Extract(i)));
    end;
  end;

  //

  if (FUserReferences <> nil) then begin
    for i := FUserReferences.Count - 1 downto 0 do begin
      toRec.AddUserReference(TGEDCOMUserReference(FUserReferences.Extract(i)));
    end;
  end;

  //

  if (FGroups <> nil) then begin
    for i := FGroups.Count - 1 downto 0 do begin
      toRec.AddGroup(TGEDCOMPointer(FGroups.Extract(i)));
    end;
  end;
end;

procedure TGEDCOMIndividualRecord.ExchangeSpouses(Index1, Index2: Integer);
begin
  if ((Index1 >= 0) and (Index1 < FSpouseToFamilyLinks.Count))
  and ((Index2 >= 0) and (Index2 < FSpouseToFamilyLinks.Count))
  then FSpouseToFamilyLinks.Exchange(Index1, Index2);
end;

{ TGEDCOMSubmissionRecord }

function TGEDCOMSubmissionRecord.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);
  if (SU = 'SUBM')
  then Result := inherited AddTag(ATag, AValue, TGEDCOMPointer)
  else Result := inherited AddTag(ATag, AValue, AClass);
end;

constructor TGEDCOMSubmissionRecord.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent, [stNotes]);
  FName := 'SUBN';
end;

function TGEDCOMSubmissionRecord.GetIntegerTag(Index: Integer): Integer;
begin
  case Index of
    1: Result := TagIntegerValue('ANCE', 0);
    2: Result := TagIntegerValue('DESC', 0);
    else Result := 0;
  end;
end;

function TGEDCOMSubmissionRecord.GetOrdinanceProcessFlag: TGEDCOMOrdinanceProcessFlag;
var
  S: string;
begin
  S := UpperCase(Trim(TagStringValue('ORDI')));
  if (S = 'YES')
  then Result := opYes
  else
  if S = 'NO'
  then Result := opNo
  else Result := opNone;
end;

function TGEDCOMSubmissionRecord.GetStringTag(Index: Integer): string;
begin
  case Index of
    1: Result := TagStringValue('FAMF');
    2: Result := TagStringValue('TEMP');
    3: Result := TagStringValue('RIN');
  end;
end;

function TGEDCOMSubmissionRecord.GetSubmitter: TGEDCOMPointer;
begin
  Result := TGEDCOMPointer(TagClass('SUBM', TGEDCOMPointer));
end;

procedure TGEDCOMSubmissionRecord.SetIntegerTag(Index, Value: Integer);
begin
  case Index of
    1: SetTagIntegerValue('ANCE', Value);
    2: SetTagIntegerValue('DESC', Value);
  end;
end;

procedure TGEDCOMSubmissionRecord.SetOrdinanceProcessFlag(
  const Value: TGEDCOMOrdinanceProcessFlag);
var
  S: string;
begin
  case Value of
    opNone: S := '';
    opYes: S := 'yes';
    opNo: S := 'no';
  else
    S := '';
  end;
  SetTagStringValue('ORDI', S);
end;

procedure TGEDCOMSubmissionRecord.SetStringTag(Index: Integer;
  const Value: string);
begin
  case Index of
    1: SetTagStringValue('FAMF', Value);
    2: SetTagStringValue('TEMP', Value);
    3: SetTagStringValue('RIN', Value);
  end;
end;

{ TGEDCOMSubmitterRecord }

function TGEDCOMSubmitterRecord.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
  AddrTag: TGEDCOMTag;
begin
  SU := UpperCase(ATag);
  if (SU = 'NAME')
  then Result := inherited AddTag(ATag, AValue, TGEDCOMPersonalName)
  else
  if (SU = 'ADDR')
  then Result := inherited AddTag(ATag, AValue, TGEDCOMAddress)
  else
  if (SU = 'PHON') or (SU = 'EMAIL') or (SU = 'FAX') or (SU = 'WWW')
  then begin
    AddrTag := FindTag('ADDR');
    if (AddrTag = nil) then
      AddrTag := AddTag('ADDR');
    Result := AddrTag.AddTag(ATag, AValue, AClass)
  end
  else
  if (SU = 'LANG')
  then Result := TGEDCOMTag(AddLanguage(TGEDCOMTag.CreateTag(Owner, Self, SU, AValue)))
  else Result := inherited AddTag(ATag, AValue, AClass);
end;

function TGEDCOMSubmitterRecord.AddLanguage(Value: TGEDCOMTag): TGEDCOMTag;
begin
  Result := Value;

  if (FLanguages = nil)
  then FLanguages := TGEDCOMList.Create(Self);

  if (FLanguages <> nil)
  then FLanguages.Add(Value);
end;

procedure TGEDCOMSubmitterRecord.Clear;
begin
  inherited Clear;
  if (FLanguages <> nil) then FLanguages.Clear;
end;

constructor TGEDCOMSubmitterRecord.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent, [stNotes, stMultimedia]);
  FName := 'SUBM';
  FLanguages := nil;
end;

destructor TGEDCOMSubmitterRecord.Destroy;
begin
  if (FLanguages <> nil) then FLanguages.Free;
  inherited Destroy;
end;

function TGEDCOMSubmitterRecord.GetAddress: TGEDCOMAddress;
begin
  Result := TGEDCOMAddress(TagClass('ADDR', TGEDCOMAddress));
end;

function TGEDCOMSubmitterRecord.GetLanguages(Index: Integer): string;
begin
  if (FLanguages = nil) or (Index < 0) or (Index >= FLanguages.Count)
  then Result := ''
  else Result := TGEDCOMTag(FLanguages[Index]).StringValue;
end;

function TGEDCOMSubmitterRecord.GetLanguagesCount: Integer;
begin
  if (FLanguages = nil)
  then Result := 0
  else Result := FLanguages.Count;
end;

function TGEDCOMSubmitterRecord.GetName: TGEDCOMPersonalName;
begin
  Result := TGEDCOMPersonalName(TagClass('NAME', TGEDCOMPersonalName));
end;

function TGEDCOMSubmitterRecord.GetStringTag(Index: Integer): string;
begin
  case Index of
    1: Result := TagStringValue('RFN');
    2: Result := TagStringValue('RIN');
  end;
end;

function TGEDCOMSubmitterRecord.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and (GetLanguagesCount = 0);
end;

procedure TGEDCOMSubmitterRecord.ReplaceXRefs(aMap: TXRefReplaceMap);
begin
  inherited ReplaceXRefs(aMap);

  if (FLanguages <> nil) then FLanguages.ReplaceXRefs(aMap);
end;

procedure TGEDCOMSubmitterRecord.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);

  if (FLanguages <> nil) then FLanguages.ResetOwner(AOwner);
end;

procedure TGEDCOMSubmitterRecord.SetLanguages(Index: Integer;
  const Value: string);
begin
  if (Index >= GEDCOMMaxLanguages)
  then raise EGEDCOMException.CreateFmt(EMaxLanguages, [GEDCOMMaxLanguages])
  else
  if (Index >= 0) then begin
    if (FLanguages = nil)
    then FLanguages := TGEDCOMList.Create(Self);

    while (Index >= FLanguages.Count) do
      FLanguages.Add(TGEDCOMTag.CreateTag(Owner, Self, 'LANG', ''));

    TGEDCOMTag(FLanguages[Index]).StringValue := Value;
  end
end;

procedure TGEDCOMSubmitterRecord.SetStringTag(Index: Integer;
  const Value: string);
begin
  case Index of
    1: SetTagStringValue('RFN', Value);
    2: SetTagStringValue('RIN', Value);
  end;
end;

{ TGEDCOMMultimediaRecord }

function TGEDCOMMultimediaRecord.AddFileReference(
  Value: TGEDCOMFileReferenceWithTitle): TGEDCOMFileReferenceWithTitle;
begin
  Result := Value;

  if (FFileReferences = nil)
  then FFileReferences := TGEDCOMList.Create(Self);

  if (Value <> nil)
  then FFileReferences.Add(Value);
end;

function TGEDCOMMultimediaRecord.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);
  if (SU = 'FILE')
  then Result := AddFileReference(TGEDCOMFileReferenceWithTitle.CreateTag(Owner, Self, SU, AValue))
  else
  if (SU = 'REFN')
  then Result := AddUserReference(TGEDCOMUserReference.CreateTag(Owner, Self, SU, AValue))
  else Result := inherited AddTag(ATag, AValue, AClass);
end;

function TGEDCOMMultimediaRecord.AddUserReference(
  AUserReference: TGEDCOMUserReference): TGEDCOMUserReference;
begin
  Result := AUserReference;

  if (FUserReferences = nil)
  then FUserReferences := TGEDCOMList.Create(Self);

  if (FUserReferences <> nil)
  then FUserReferences.Add(AUserReference);
end;

procedure TGEDCOMMultimediaRecord.Clear;
begin
  inherited Clear;
  if (FFileReferences <> nil) then FFileReferences.Clear;
  if (FUserReferences <> nil) then FUserReferences.Clear;
  if (FNotes <> nil) then FNotes.Clear;
  if (FSourceCitations <> nil) then FSourceCitations.Clear;
end;

constructor TGEDCOMMultimediaRecord.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent, [stNotes, stSource]);
  FName := 'OBJE';
  FFileReferences := nil;
  FUserReferences := nil;
end;

destructor TGEDCOMMultimediaRecord.Destroy;
begin
  if (FFileReferences <> nil) then FFileReferences.Free;
  if (FUserReferences <> nil) then FUserReferences.Free;
  inherited Destroy;
end;

function TGEDCOMMultimediaRecord.GetFileReferences(
  Index: Integer): TGEDCOMFileReferenceWithTitle;
begin
  if (FFileReferences = nil)
  then Result := nil
  else Result := TGEDCOMFileReferenceWithTitle(FFileReferences[Index]);
end;

function TGEDCOMMultimediaRecord.GetFileReferencesCount: Integer;
begin
  if (FFileReferences = nil)
  then Result := 0
  else Result := FFileReferences.Count;
end;

function TGEDCOMMultimediaRecord.GetStringTag(Index: Integer): string;
begin
  case Index of
    1: Result := TagStringValue('RIN');   // Automated record ID
  end;
end;

function TGEDCOMMultimediaRecord.GetUserReferences(
  Index: Integer): TGEDCOMUserReference;
begin
  if (FUserReferences = nil)
  then Result := nil
  else Result := TGEDCOMUserReference(FUserReferences[Index]);
end;

function TGEDCOMMultimediaRecord.GetUserReferencesCount: Integer;
begin
  if (FUserReferences = nil)
  then Result := 0
  else Result := FUserReferences.Count;
end;

function TGEDCOMMultimediaRecord.IsEmpty(): Boolean;
begin
  Result := inherited IsEmpty and (GetFileReferencesCount = 0) and
    (GetUserReferencesCount = 0) and (GetNotesCount = 0) and
    (GetSourceCitationsCount = 0);
end;

procedure TGEDCOMMultimediaRecord.ReplaceXRefs(aMap: TXRefReplaceMap);
begin
  inherited ReplaceXRefs(aMap);

  if (FFileReferences <> nil)
  then FFileReferences.ReplaceXRefs(aMap);

  if (FUserReferences <> nil)
  then FUserReferences.ReplaceXRefs(aMap);
end;

procedure TGEDCOMMultimediaRecord.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);

  if (FFileReferences <> nil) then FFileReferences.ResetOwner(AOwner);
  if (FUserReferences <> nil) then FUserReferences.ResetOwner(AOwner);
end;

procedure TGEDCOMMultimediaRecord.SaveToStream(AStream: TStream);
begin
  inherited SaveToStream(AStream);

  if Assigned(FFileReferences) then FFileReferences.SaveToStream(AStream);
  if Assigned(FUserReferences) then FUserReferences.SaveToStream(AStream);
end;

procedure TGEDCOMMultimediaRecord.SetStringTag(Index: Integer;
  const Value: string);
begin
  case Index of
    1: SetTagStringValue('RIN', Value);   // Automated record ID
  end;
end;

{ TGEDCOMNoteRecord }

function TGEDCOMNoteRecord.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);
  if (SU = 'REFN')
  then Result := AddUserReference(TGEDCOMUserReference.CreateTag(Owner, Self, SU, AValue))
  else Result := inherited AddTag(ATag, AValue, AClass);
end;

function TGEDCOMNoteRecord.AddUserReference(
  AUserReference: TGEDCOMUserReference): TGEDCOMUserReference;
begin
  Result := AUserReference;

  if (FUserReferences = nil)
  then FUserReferences := TGEDCOMList.Create(Self);

  if (AUserReference <> nil)
  then FUserReferences.Add(AUserReference);
end;

procedure TGEDCOMNoteRecord.Clear;
begin
  inherited Clear;
  if (FUserReferences <> nil) then FUserReferences.Clear;
end;

constructor TGEDCOMNoteRecord.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent, [stSource]);
  FName := 'NOTE';
  FUserReferences := nil;
  FNotes := nil;
end;

destructor TGEDCOMNoteRecord.Destroy;
begin
  if (FUserReferences <> nil) then FUserReferences.Free;
  if (FNotes <> nil) then FNotes.Free;
  inherited Destroy;
end;

function TGEDCOMNoteRecord.GetNotes: TStrings;
begin
  Result := TagStrings(Self, FNotes);
end;

function TGEDCOMNoteRecord.GetStringTag(Index: Integer): string;
begin
  case Index of
    1: Result := TagStringValue('RIN');
  end;
end;

function TGEDCOMNoteRecord.GetUserReferences(
  Index: Integer): TGEDCOMUserReference;
begin
  if (FUserReferences = nil)
  then Result := nil
  else Result := TGEDCOMUserReference(FUserReferences[Index]);
end;

function TGEDCOMNoteRecord.GetUserReferencesCount: Integer;
begin
  if (FUserReferences = nil)
  then Result := 0
  else Result := FUserReferences.Count;
end;

function TGEDCOMNoteRecord.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and (GetUserReferencesCount = 0);
end;

procedure TGEDCOMNoteRecord.MoveTo(aToRecord: TGEDCOMRecord);
var
  cont: TStringList;
begin
  cont := TStringList.Create;
  try
    cont.Text := TGEDCOMNoteRecord(aToRecord).Notes.Text;
    inherited MoveTo(aToRecord);
    TGEDCOMNoteRecord(aToRecord).Notes := cont;
  finally
    cont.Free;
  end;
end;

procedure TGEDCOMNoteRecord.ReplaceXRefs(aMap: TXRefReplaceMap);
begin
  inherited ReplaceXRefs(aMap);

  if (FUserReferences <> nil)
  then FUserReferences.ReplaceXRefs(aMap);
end;

procedure TGEDCOMNoteRecord.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);

  if (FUserReferences <> nil) then FUserReferences.ResetOwner(AOwner);
end;

procedure TGEDCOMNoteRecord.SetNotes(Value: TStrings);
begin
  SetTagStrings(Self, Value);
end;

procedure TGEDCOMNoteRecord.SetStringTag(Index: Integer; const Value: string);
begin
  case Index of
    1: SetTagStringValue('RIN', Value);
  end;
end;

{ TGEDCOMRepositoryRecord }

function TGEDCOMRepositoryRecord.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);
  if (SU = 'ADDR')
  then Result := inherited AddTag(ATag, AValue, TGEDCOMAddress)
  else
  if (SU = 'PHON') or (SU = 'EMAIL') or (SU = 'FAX') or (SU = 'WWW')
  then Result := TGEDCOMAddress(TagClass('ADDR', TGEDCOMAddress)).AddTag(ATag, AValue, AClass)
  else Result := inherited AddTag(ATag, AValue, AClass);
end;

function TGEDCOMRepositoryRecord.AddUserReference(
  AUserReference: TGEDCOMUserReference): TGEDCOMUserReference;
begin
  Result := AUserReference;

  if (FUserReferences = nil)
  then FUserReferences := TGEDCOMList.Create(Self);

  if (FUserReferences <> nil)
  then FUserReferences.Add(AUserReference);
end;

procedure TGEDCOMRepositoryRecord.Clear;
begin
  inherited Clear;
  if (FUserReferences <> nil) then FUserReferences.Clear;
end;

constructor TGEDCOMRepositoryRecord.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent, [stNotes]);
  FName := 'REPO';
  FUserReferences := nil;
end;

destructor TGEDCOMRepositoryRecord.Destroy;
begin
  if (FUserReferences <> nil) then FUserReferences.Free;
  inherited Destroy;
end;

function TGEDCOMRepositoryRecord.GetAddress: TGEDCOMAddress;
begin
  Result := TGEDCOMAddress(TagClass('ADDR', TGEDCOMAddress));
end;

function TGEDCOMRepositoryRecord.GetStringTag(Index: Integer): string;
begin
  case Index of
    1: Result := TagStringValue('NAME');
    2: Result := TagStringValue('RIN');   // Automated record ID
  end;
end;

function TGEDCOMRepositoryRecord.GetUserReferences(
  Index: Integer): TGEDCOMUserReference;
begin
  if (FUserReferences = nil)
  then Result := nil
  else Result := TGEDCOMUserReference(FUserReferences[Index]);
end;

function TGEDCOMRepositoryRecord.GetUserReferencesCount: Integer;
begin
  if (FUserReferences = nil)
  then Result := 0
  else Result := FUserReferences.Count;
end;

function TGEDCOMRepositoryRecord.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and (GetUserReferencesCount = 0);
end;

procedure TGEDCOMRepositoryRecord.ReplaceXRefs(aMap: TXRefReplaceMap);
begin
  inherited ReplaceXRefs(aMap);

  if (FUserReferences <> nil)
  then FUserReferences.ReplaceXRefs(aMap);
end;

procedure TGEDCOMRepositoryRecord.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);

  if (FUserReferences <> nil) then FUserReferences.ResetOwner(AOwner);
end;

procedure TGEDCOMRepositoryRecord.SetStringTag(Index: Integer;
  const Value: string);
begin
  case Index of
    1: SetTagStringValue('NAME', Value);
    2: SetTagStringValue('RIN', Value);   // Automated record ID
  end;
end;

{ TGEDCOMSourceRecord }

function TGEDCOMSourceRecord.AddRepositoryCitation(
  Value: TGEDCOMRepositoryCitation): TGEDCOMRepositoryCitation;
begin
  Result := Value;

  if (FRepositoryCitations = nil)
  then FRepositoryCitations := TGEDCOMList.Create(Self);

  if (Value <> nil)
  then FRepositoryCitations.Add(Value);
end;

procedure TGEDCOMSourceRecord.RemoveRepositoryCitation(
  Value: TGEDCOMRepositoryCitation);
begin
  if (FRepositoryCitations = nil) then Exit;

  FRepositoryCitations.DeleteObject(Value);
end;

function TGEDCOMSourceRecord.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);
  if (SU = 'REPO')
  then Result := AddRepositoryCitation(TGEDCOMRepositoryCitation.CreateTag(Owner, Self, SU, AValue))
  else
  if (SU = 'REFN')
  then Result := AddUserReference(TGEDCOMUserReference.CreateTag(Owner, Self, SU, AValue))
  else Result := inherited AddTag(ATag, AValue, AClass);
end;

function TGEDCOMSourceRecord.AddUserReference(
  AUserReference: TGEDCOMUserReference): TGEDCOMUserReference;
begin
  Result := AUserReference;

  if (FUserReferences = nil)
  then FUserReferences := TGEDCOMList.Create(Self);

  if (FUserReferences <> nil)
  then FUserReferences.Add(AUserReference);
end;

procedure TGEDCOMSourceRecord.Clear;
begin
  inherited Clear;
  if (FRepositoryCitations <> nil) then FRepositoryCitations.Clear;
  if (FUserReferences <> nil) then FUserReferences.Clear;
end;

constructor TGEDCOMSourceRecord.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent, [stNotes, stMultimedia]);
  FName := 'SOUR';
  FTitle := nil;
  FOriginator := nil;
  FPublication := nil;
  FText := nil;
  FRepositoryCitations := nil;
  FUserReferences := nil;
end;

destructor TGEDCOMSourceRecord.Destroy;
begin
  if (FTitle <> nil) then FTitle.Free;
  if (FOriginator <> nil) then FOriginator.Free;
  if (FPublication <> nil) then FPublication.Free;
  if (FText <> nil) then FText.Free;
  if (FRepositoryCitations <> nil) then FRepositoryCitations.Free;
  if (FUserReferences <> nil) then FUserReferences.Free;
  inherited Destroy;
end;

function TGEDCOMSourceRecord.GetOriginator: TStrings;
begin
  Result := TagStrings(TagClass('AUTH', TGEDCOMTag), FOriginator);
end;

function TGEDCOMSourceRecord.GetPublication: TStrings;
begin
  Result := TagStrings(TagClass('PUBL', TGEDCOMTag), FPublication);
end;

function TGEDCOMSourceRecord.GetRepositoryCitations(Index: Integer): TGEDCOMRepositoryCitation;
begin
  if (FRepositoryCitations = nil)
  then Result := nil
  else Result := TGEDCOMRepositoryCitation(FRepositoryCitations[Index]);
end;

function TGEDCOMSourceRecord.GetRepositoryCitationsCount: Integer;
begin
  if (FRepositoryCitations = nil)
  then Result := 0
  else Result := FRepositoryCitations.Count;
end;

function TGEDCOMSourceRecord.GetStringTag(Index: Integer): string;
begin
  case Index of
    1: Result := TagStringValue('ABBR');
    2: Result := TagStringValue('RIN');   // Automated record ID
  end;
end;

function TGEDCOMSourceRecord.GetText: TStrings;
begin
  Result := TagStrings(TagClass('TEXT', TGEDCOMTag), FText);
end;

function TGEDCOMSourceRecord.GetTitle: TStrings;
begin
  Result := TagStrings(TagClass('TITL', TGEDCOMTag), FTitle);
end;

function TGEDCOMSourceRecord.GetUserReferences(Index: Integer): TGEDCOMUserReference;
begin
  if (FUserReferences = nil)
  then Result := nil
  else Result := TGEDCOMUserReference(FUserReferences[Index]);
end;

function TGEDCOMSourceRecord.GetUserReferencesCount: Integer;
begin
  if (FUserReferences = nil)
  then Result := 0
  else Result := FUserReferences.Count;
end;

function TGEDCOMSourceRecord.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and (GetRepositoryCitationsCount = 0) and
    (GetUserReferencesCount = 0);
end;

procedure TGEDCOMSourceRecord.ReplaceXRefs(aMap: TXRefReplaceMap);
begin
  inherited ReplaceXRefs(aMap);

  if (FRepositoryCitations <> nil)
  then FRepositoryCitations.ReplaceXRefs(aMap);

  if (FUserReferences <> nil)
  then FUserReferences.ReplaceXRefs(aMap);
end;

procedure TGEDCOMSourceRecord.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);

  if (FRepositoryCitations <> nil) then FRepositoryCitations.ResetOwner(AOwner);
  if (FUserReferences <> nil) then FUserReferences.ResetOwner(AOwner);
end;

procedure TGEDCOMSourceRecord.SetOriginator(const Value: TStrings);
begin
  SetTagStrings(TagClass('AUTH', TGEDCOMTag), Value);
end;

procedure TGEDCOMSourceRecord.SetPublication(const Value: TStrings);
begin
  SetTagStrings(TagClass('PUBL', TGEDCOMTag), Value);
end;

procedure TGEDCOMSourceRecord.SetStringTag(Index: Integer;
  const Value: string);
begin
  case Index of
    1: SetTagStringValue('ABBR', Value);
    2: SetTagStringValue('RIN', Value);   // Automated record ID
  end;
end;

procedure TGEDCOMSourceRecord.SetText(const Value: TStrings);
begin
  SetTagStrings(TagClass('TEXT', TGEDCOMTag), Value);
end;

procedure TGEDCOMSourceRecord.SetTitle(Value: TStrings);
begin
  SetTagStrings(TagClass('TITL', TGEDCOMTag), Value);
end;

procedure TGEDCOMSourceRecord.SaveToStream(AStream: TStream);
begin
  inherited SaveToStream(AStream);

  if Assigned(FRepositoryCitations) then FRepositoryCitations.SaveToStream(AStream);
  if Assigned(FUserReferences) then FUserReferences.SaveToStream(AStream);
end;

procedure TGEDCOMSourceRecord.MoveTo(aToRecord: TGEDCOMRecord);
var
  titl, orig, publ, text: TStringList;
  toSource: TGEDCOMSourceRecord;
  i: Integer;
begin
  toSource := TGEDCOMSourceRecord(aToRecord);

  titl := TStringList.Create;
  orig := TStringList.Create;
  publ := TStringList.Create;
  text := TStringList.Create;
  try
    titl.Text := Trim(toSource.Title.Text + #10 + Self.Title.Text);
    orig.Text := Trim(toSource.Originator.Text + #10 + Self.Originator.Text);
    publ.Text := Trim(toSource.Publication.Text + #10 + Self.Publication.Text);
    text.Text := Trim(toSource.Text.Text + #10 + Self.Text.Text);

    DeleteTag('TITL');
    DeleteTag('TEXT');
    DeleteTag('ABBR');
    DeleteTag('PUBL');
    DeleteTag('AUTH');

    inherited MoveTo(aToRecord);

    toSource.Title := titl;
    toSource.Originator := orig;
    toSource.Publication := publ;
    toSource.Text := text;

    //

    if (FRepositoryCitations <> nil) then begin
      for i := FRepositoryCitations.Count - 1 downto 0 do begin
        toSource.AddRepositoryCitation(TGEDCOMRepositoryCitation(FRepositoryCitations.Extract(i)));
      end;
    end;

    if (FUserReferences <> nil) then begin
      for i := FUserReferences.Count - 1 downto 0 do begin
        toSource.AddUserReference(TGEDCOMUserReference(FUserReferences.Extract(i)));
      end;
    end;
  finally
    titl.Free;
    orig.Free;
    publ.Free;
    text.Free;
  end;
end;

{ TGEDCOMUserReference }

constructor TGEDCOMUserReference.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FName := 'REFN';
end;

function TGEDCOMUserReference.GetReferenceType: string;
begin
  Result := TagStringValue('TYPE');
end;

procedure TGEDCOMUserReference.SetReferenceType(const Value: string);
begin
  SetTagStringValue('TYPE', Value);
end;

{ TGEDCOMChangeDate }

constructor TGEDCOMChangeDate.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FName := 'CHAN';
end;

function TGEDCOMChangeDate.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);

  if (SU = 'DATE')
  then Result := inherited AddTag(ATag, AValue, TGEDCOMDateExact)
  else
  if (SU = 'NOTE')
  then Result := inherited AddTag(ATag, AValue, TGEDCOMNotes)
  else Result := inherited AddTag(ATag, AValue, AClass);
end;

function TGEDCOMChangeDate.GetChangeDateTime: TDateTime;
begin
  Result := GetDate.Date + GetTime.Time;
end;

function TGEDCOMChangeDate.GetDate: TGEDCOMDateExact;
begin
  Result := TGEDCOMDateExact(TagClass('DATE', TGEDCOMDateExact));
end;

function TGEDCOMChangeDate.GetNotes: TGEDCOMNotes;
begin
  Result := TGEDCOMNotes(TagClass('NOTE', TGEDCOMNotes));
end;

function TGEDCOMChangeDate.GetTime: TGEDCOMTime;
var
  DateTag: TGEDCOMTag;
begin
  DateTag := FindTag('DATE');

  if (DateTag = nil)
  then DateTag := AddTag('DATE', '');

  Result := TGEDCOMTime(DateTag.TagClass('TIME', TGEDCOMTime));
end;

procedure TGEDCOMChangeDate.SetChangeDateTime(const Value: TDateTime);
begin
  GetDate.Date := Value;
  if Frac(Value) <> 0 then GetTime.Time := Value;
end;

function TGEDCOMChangeDate.IsEmpty(): Boolean;
begin
  Result := inherited IsEmpty();
end;

{ TGEDCOMNotes }

procedure TGEDCOMNotes.Clear;
begin
  inherited Clear;
  if (FNotes <> nil) then FreeAndNil(FNotes);
end;

constructor TGEDCOMNotes.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FName := 'NOTE';
end;

destructor TGEDCOMNotes.Destroy;
begin
  if (FNotes <> nil) then FNotes.Free;
  inherited Destroy;
end;

function TGEDCOMNotes.GetIsPointer: Boolean;
begin
  Result := XRef <> '';
end;

function TGEDCOMNotes.GetNotes: TStrings;
var
  NotesRecord: TGEDCOMRecord;
begin
  if (FNotes = nil)
  then FNotes := TStringList.Create
  else FNotes.Clear;

  if not IsPointer
  then TagStrings(Self, FNotes)
  else begin
    NotesRecord := Value;
    if (NotesRecord <> nil) and (NotesRecord is TGEDCOMNoteRecord)
    then FNotes.Assign(TGEDCOMNoteRecord(NotesRecord).Notes);
  end;
  Result := FNotes;
end;

function TGEDCOMNotes.GetStringValue: string;
begin
  if IsPointer
  then Result := inherited GetStringValue
  else Result := FStringValue;
end;

function TGEDCOMNotes.IsEmpty: Boolean;
begin
  if IsPointer
  then Result := inherited IsEmpty
  else Result := (FStringValue = '') and (Count = 0);
end;

function TGEDCOMNotes.ParseString(const AString: string): string;
begin
  FStringValue := '';
  XRef := '';
  Result := AString;
  Result := ExtractDelimiter(Result);
  Result := inherited ParseString(Result);
  if not IsPointer then begin
    FStringValue := Result;
    Result := '';
  end;
end;

procedure TGEDCOMNotes.SetNotes(const Value: TStrings);
begin
  Clear;
  SetTagStrings(Self, Value);
end;

{ TGEDCOMSourceCitation }

constructor TGEDCOMSourceCitation.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FName := 'SOUR';
end;

destructor TGEDCOMSourceCitation.Destroy;
begin
  if (FDescription <> nil) then FDescription.Free;
  inherited Destroy;
end;

procedure TGEDCOMSourceCitation.Clear;
begin
  inherited Clear;
  if (FDescription <> nil) then FreeAndNil(FDescription);
end;

function TGEDCOMSourceCitation.GetDescription: TStrings;
var
  SourceRecord: TGEDCOMRecord;
begin
  if (FDescription = nil)
  then FDescription := TStringList.Create
  else FDescription.Clear;

  if not IsPointer
  then TagStrings(Self, FDescription)
  else begin
    SourceRecord := Value;
    if (SourceRecord <> nil) and (SourceRecord is TGEDCOMSourceRecord)
    then FDescription.Assign(TGEDCOMSourceRecord(SourceRecord).Title);
  end;
  Result := FDescription;
end;

function TGEDCOMSourceCitation.GetIsPointer: Boolean;
begin
  Result := XRef <> '';
end;

function TGEDCOMSourceCitation.GetStringValue: string;
begin
  if IsPointer
  then Result := inherited GetStringValue
  else Result := FStringValue;
end;

function TGEDCOMSourceCitation.IsEmpty: Boolean;
begin
  if IsPointer
  then Result := inherited IsEmpty
  else Result := (FStringValue = '') and (Count = 0);
end;

function TGEDCOMSourceCitation.ParseString(const AString: string): string;
begin
  FStringValue := '';
  XRef := '';
  Result := AString;
  Result := ExtractDelimiter(Result);
  Result := inherited ParseString(Result);
  if not IsPointer then begin
    FStringValue := Result;
    Result := '';
  end;
end;

procedure TGEDCOMSourceCitation.SetDescription(const Value: TStrings);
begin
  Clear;
  SetTagStrings(Self, Value);
end;

function TGEDCOMSourceCitation.GetPage: string;
begin
  Result := TagStringValue('PAGE');
end;

procedure TGEDCOMSourceCitation.SetPage(const Value: string);
begin
  SetTagStringValue('PAGE', Value);
end;

{ TGEDCOMMultimediaLink }

function TGEDCOMMultimediaLink.AddFileReference(
  AFileReference: TGEDCOMFileReference): TGEDCOMFileReference;
begin
  Result := AFileReference;

  if (FFileReferences = nil)
  then FFileReferences := TGEDCOMList.Create(Self);

  if (AFileReference <> nil)
  then FFileReferences.Add(AFileReference);
end;

function TGEDCOMMultimediaLink.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
begin
  if (UpperCase(ATag) = 'FILE')
  then Result := AddFileReference(TGEDCOMFileReference.CreateTag(Owner, Self, ATag, AValue))
  else Result := inherited AddTag(ATag, AValue, AClass);
end;

procedure TGEDCOMMultimediaLink.Clear;
begin
  inherited Clear;
  if (FFileReferences <> nil) then FFileReferences.Clear;
end;

constructor TGEDCOMMultimediaLink.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FName := 'OBJE';
  FFileReferences := nil;
end;

destructor TGEDCOMMultimediaLink.Destroy;
begin
  if (FFileReferences <> nil) then FFileReferences.Free;
  inherited Destroy;
end;

function TGEDCOMMultimediaLink.GetFileReferences(Index: Integer): TGEDCOMFileReference;
begin
  if (FFileReferences = nil)
  then Result := nil
  else Result := TGEDCOMFileReference(FFileReferences[Index]);
end;

function TGEDCOMMultimediaLink.GetFileReferencesCount: Integer;
begin
  if (FFileReferences = nil)
  then Result := 0
  else Result := FFileReferences.Count;
end;

function TGEDCOMMultimediaLink.GetIsPointer: Boolean;
begin
  Result := XRef <> '';
end;

function TGEDCOMMultimediaLink.GetStringTag(Index: Integer): string;
begin
  case Index of
    1: Result := TagStringValue('TITL');
  end;
end;

function TGEDCOMMultimediaLink.GetStringValue: string;
begin
  if IsPointer
  then Result := inherited GetStringValue
  else Result := FStringValue;
end;

function TGEDCOMMultimediaLink.IsEmpty: Boolean;
begin
  if IsPointer
  then Result := inherited IsEmpty
  else Result := (Count = 0) and ((FFileReferences = nil) or (FFileReferences.Count = 0));
end;

function TGEDCOMMultimediaLink.ParseString(const AString: string): string;
begin
  FStringValue := '';
  Result := inherited ParseString(AString);
end;

procedure TGEDCOMMultimediaLink.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);

  if (FFileReferences <> nil) then FFileReferences.ResetOwner(AOwner);
end;

procedure TGEDCOMMultimediaLink.SaveToStream(AStream: TStream);
begin
  inherited SaveToStream(AStream);

  if (FFileReferences <> nil) then FFileReferences.SaveToStream(AStream);
end;

procedure TGEDCOMMultimediaLink.SetStringTag(Index: Integer;
  const Value: string);
begin
  case Index of
    1: SetTagStringValue('TITL', Value);
  end;
end;

{ TGEDCOMFileReference }

constructor TGEDCOMFileReference.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FName := 'FILE';
end;

function TGEDCOMFileReference.GetMediaType: TGEDCOMMediaType;
var
  S: string;
begin
  S := UpperCase(Trim(TagStringValue(MediaTypeTagName)));

  if (S = '')
  then Result := mtNone
  else
  if (S = 'AUDIO')
  then Result := mtAudio
  else
  if (S = 'BOOK')
  then Result := mtBook
  else
  if (S = 'CARD')
  then Result := mtCard
  else
  if (S = 'ELECTRONIC')
  then Result := mtElectronic
  else
  if (S = 'FICHE')
  then Result := mtFiche
  else
  if (S = 'FILM')
  then Result := mtFilm
  else
  if (S = 'MAGAZINE')
  then Result := mtMagazine
  else
  if (S = 'MANUSCRIPT')
  then Result := mtManuscript
  else
  if (S = 'MAP')
  then Result := mtMap
  else
  if (S = 'NEWSPAPER')
  then Result := mtNewspaper
  else
  if (S = 'PHOTO')
  then Result := mtPhoto
  else
  if (S = 'TOMBSTONE')
  then Result := mtTombstone
  else
  if (S = 'VIDEO')
  then Result := mtVideo
  else Result := mtUnknown;
end;

function TGEDCOMFileReference.GetMultimediaFormat: TGEDCOMMultimediaFormat;
var
  S: string;
begin
  S := UpperCase(Trim(TagStringValue('FORM')));

  if (S = '')
  then Result := mfNone
  else
  if (S = 'BMP')
  then Result := mfBMP
  else
  if (S = 'GIF')
  then Result := mfGIF
  else
  if (S = 'JPG')
  then Result := mfJPG
  else
  if (S = 'OLE')
  then Result := mfOLE
  else
  if (S = 'PCX')
  then Result := mfPCX
  else
  if (S = 'TIF')
  then Result := mfTIF
  else
  if (S = 'WAV')
  then Result := mfWAV
  else
  if (S = 'TXT')
  then Result := mfTXT
  else
  if (S = 'RTF')
  then Result := mfRTF
  else
  if (S = 'AVI')
  then Result := mfAVI
  else
  if (S = 'TGA')
  then Result := mfTGA
  else
  if (S = 'PNG')
  then Result := mfPNG
  else
  if (S = 'MPG')
  then Result := mfMPG
  else
  if (S = 'HTM')
  then Result := mfHTM
  else Result := mfUnknown;
end;

class function TGEDCOMFileReference.RecognizeFormat(const AFile: string): TGEDCOMMultimediaFormat;
var
  E: string;
begin
  E := LowerCase(ExtractFileExt(AFile));

  if (E = '.bmp')
  then Result := mfBMP
  else
  if (E = '.gif')
  then Result := mfGIF
  else
  if (E = '.jpg') or (E = '.jpeg')
  then Result := mfJPG
  else
  if (E = '.ole')
  then Result := mfOLE {TODO: Does this exists? What is an OLE file actually?}
  else
  if (E = '.pcx')
  then Result := mfPCX
  else
  if (E = '.tif') or (E = '.tiff')
  then Result := mfTIF
  else
  if (E = '.wav')
  then Result := mfWAV
  else
  if (E = '.txt')
  then Result := mfTXT
  else
  if (E = '.rtf')
  then Result := mfRTF
  else
  if (E = '.avi')
  then Result := mfAVI
  else
  if (E = '.tga')
  then Result := mfTGA
  else
  if (E = '.png')
  then Result := mfPNG
  else
  if (E = '.mpg') or (E = '.mpeg')
  then Result := mfMPG
  else
  if (E = '.htm') or (E = '.html')
  then Result := mfHTM
  else Result := mfUnknown;
end;

procedure TGEDCOMFileReference.LinkFile(const AFile: string;
  AMediaType: TGEDCOMMediaType; AMultimediaFormat: TGEDCOMMultimediaFormat);
begin
  FStringValue := AFile;
  SetMultimediaFormat(RecognizeFormat(AFile));
  SetMediaType(AMediaType);
end;

procedure TGEDCOMFileReference.SetMediaType(const Value: TGEDCOMMediaType);
var
  S: string;
begin
  case Value of
    mtAudio: S := 'audio';
    mtBook: S := 'book';
    mtCard: S := 'card';
    mtElectronic: S := 'electronic';
    mtFiche: S := 'fiche';
    mtFilm: S := 'film';
    mtMagazine: S := 'magazine';
    mtManuscript: S := 'manuscript';
    mtMap: S := 'map';
    mtNewspaper: S := 'newspaper';
    mtPhoto: S := 'photo';
    mtTombstone: S := 'tombstone';
    mtVideo: S := 'video';
    else S := '';
  end;
  SetTagStringValue(MediaTypeTagName, S);
end;

procedure TGEDCOMFileReference.SetMultimediaFormat(
  const Value: TGEDCOMMultimediaFormat);
begin
  case Value of
    mfBMP: SetTagStringValue('FORM', 'bmp');
    mfGIF: SetTagStringValue('FORM', 'gif');
    mfJPG: SetTagStringValue('FORM', 'jpg');
    mfOLE: SetTagStringValue('FORM', 'ole');
    mfPCX: SetTagStringValue('FORM', 'pcx');
    mfTIF: SetTagStringValue('FORM', 'tif');
    mfWAV: SetTagStringValue('FORM', 'wav');
    mfTXT: SetTagStringValue('FORM', 'txt');
    mfRTF: SetTagStringValue('FORM', 'rtf');
    mfAVI: SetTagStringValue('FORM', 'avi');
    mfTGA: SetTagStringValue('FORM', 'tga');
    mfPNG: SetTagStringValue('FORM', 'png');
    mfMPG: SetTagStringValue('FORM', 'mpg');
    mfHTM: SetTagStringValue('FORM', 'htm');
    else SetTagStringValue('FORM', '');
  end;
end;

function TGEDCOMFileReference.MediaTypeTagName: string;
begin
  Result := 'FORM\MEDI';
end;

{ TGEDCOMFileReferenceWithTitle }

function TGEDCOMFileReferenceWithTitle.GetTitle: string;
begin
  Result := TagStringValue('TITL');
end;

function TGEDCOMFileReferenceWithTitle.MediaTypeTagName: string;
begin
  Result := 'FORM\TYPE';
end;

procedure TGEDCOMFileReferenceWithTitle.SetTitle(const Value: string);
begin
  SetTagStringValue('TITL', Value);
end;

{ TGEDCOMAddress }

function TGEDCOMAddress.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);
  if (SU = 'PHON') then begin
    if FPhoneList = nil then
      FPhoneList := TGEDCOMList.Create(Self);
    Result := TGEDCOMTag(FPhoneList.Add(TGEDCOMTag.CreateTag(Owner, Self, SU, AValue)));
    // The phone numbers have the same level as the ADDR tag.
    Result.SetLevel(Level);
  end
  else
  if (SU = 'EMAIL') then begin
    if FEmailList = nil then
      FEmailList := TGEDCOMList.Create(Self);
    Result := TGEDCOMTag(FEmailList.Add(TGEDCOMTag.CreateTag(Owner, Self, SU, AValue)));
    // The email addresses have the same level as the ADDR tag.
    Result.SetLevel(Level);
  end
  else
  if (SU = 'FAX') then begin
    if FFaxList = nil then
      FFaxList := TGEDCOMList.Create(Self);
    Result := TGEDCOMTag(FFaxList.Add(TGEDCOMTag.CreateTag(Owner, Self, SU, AValue)));
    // The fax numbers have the same level as the ADDR tag.
    Result.SetLevel(Level);
  end
  else
  if (SU = 'WWW') then begin
    if FWWWList = nil then
      FWWWList := TGEDCOMList.Create(Self);
    Result := TGEDCOMTag(FWWWList.Add(TGEDCOMTag.CreateTag(Owner, Self, SU, AValue)));
    // The website names have the same level as the ADDR tag.
    Result.SetLevel(Level);
  end
  else Result := inherited AddTag(ATag, AValue, AClass);
end;

procedure TGEDCOMAddress.Clear();
begin
  inherited Clear();
  if (FPhoneList <> nil) then FPhoneList.Clear;
  if (FEmailList <> nil) then FEmailList.Clear;
  if (FFaxList <> nil) then FFaxList.Clear;
  if (FWWWList <> nil) then FWWWList.Clear;
end;

constructor TGEDCOMAddress.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FName := 'ADDR';
  FAddress := nil;
  FPhoneList := nil;
  FEmailList := nil;
  FFaxList := nil;
  FWWWList := nil;
end;

destructor TGEDCOMAddress.Destroy;
begin
  if (FAddress <> nil) then FAddress.Free;
  if (FPhoneList <> nil) then FPhoneList.Free;
  if (FEmailList <> nil) then FEmailList.Free;
  if (FFaxList <> nil) then FFaxList.Free;
  if (FWWWList <> nil) then FWWWList.Free;
  inherited Destroy;
end;

procedure TGEDCOMAddress.DeleteEmail(Index: Integer);
begin
  if (FEmailList <> nil) and (Index >= 0) and (Index < FEmailList.Count)
  then FEmailList.Delete(Index);
end;

procedure TGEDCOMAddress.DeletePhoneNumber(Index: Integer);
begin
  if (FPhoneList <> nil) and (Index >= 0) and (Index < FPhoneList.Count)
  then FPhoneList.Delete(Index);
end;

procedure TGEDCOMAddress.DeleteWebPage(Index: Integer);
begin
  if (FWWWList <> nil) and (Index >= 0) and (Index < FWWWList.Count)
  then FWWWList.Delete(Index);
end;

function TGEDCOMAddress.GetAddress: TStrings;
begin
  Result := TagStrings(Self, FAddress);
end;

function TGEDCOMAddress.GetEmailAddresses(Index: Integer): string;
begin
  if (FEmailList = nil) or (Index < 0) or (Index >= FEmailList.Count)
  then Result := ''
  else Result := TGEDCOMTag(FEmailList[Index]).StringValue;
end;

function TGEDCOMAddress.GetEmailAddressesCount: Integer;
begin
  if (FEmailList = nil)
  then Result := 0
  else Result := FEmailList.Count;
end;

function TGEDCOMAddress.GetFaxNumbers(Index: Integer): string;
begin
  if (FFaxList = nil) or (Index < 0) or (Index >= FFaxList.Count)
  then Result := ''
  else Result := TGEDCOMTag(FFaxList[Index]).StringValue;
end;

function TGEDCOMAddress.GetFaxNumbersCount: Integer;
begin
  if (FFaxList = nil)
  then Result := 0
  else Result := FFaxList.Count;
end;

function TGEDCOMAddress.GetPhoneNumbers(Index: Integer): string;
begin
  if (FPhoneList = nil) or (Index < 0) or (Index >= FPhoneList.Count)
  then Result := ''
  else Result := TGEDCOMTag(FPhoneList[Index]).StringValue;
end;

function TGEDCOMAddress.GetPhoneNumbersCount: Integer;
begin
  if (FPhoneList = nil)
  then Result := 0
  else Result := FPhoneList.Count;
end;

function TGEDCOMAddress.GetStringTag(Index: Integer): string;
begin
  case Index of
    1: Result := TagStringValue('ADR1');
    2: Result := TagStringValue('ADR2');
    3: Result := TagStringValue('ADR3');
    4: Result := TagStringValue('CITY');
    5: Result := TagStringValue('STAE');
    6: Result := TagStringValue('POST');
    7: Result := TagStringValue('CTRY');
  end;
end;

function TGEDCOMAddress.GetWebPages(Index: Integer): string;
begin
  if (FWWWList = nil) or (Index < 0) or (Index >= FWWWList.Count)
  then Result := ''
  else Result := TGEDCOMTag(FWWWList[Index]).StringValue;
end;

function TGEDCOMAddress.GetWebPagesCount: Integer;
begin
  if (FWWWList = nil)
  then Result := 0
  else Result := FWWWList.Count;
end;

function TGEDCOMAddress.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and (GetPhoneNumbersCount = 0) and
    (GetEmailAddressesCount = 0) and (GetFaxNumbersCount = 0) and
    (GetWebPagesCount = 0);
end;

procedure TGEDCOMAddress.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);

  if (FPhoneList <> nil) then FPhoneList.ResetOwner(AOwner);
  if (FEmailList <> nil) then FEmailList.ResetOwner(AOwner);
  if (FFaxList <> nil) then FFaxList.ResetOwner(AOwner);
  if (FWWWList <> nil) then FWWWList.ResetOwner(AOwner);
end;

procedure TGEDCOMAddress.SaveTagsToStream(AStream: TStream;
  const ATagSorting: array of string);
begin
  inherited SaveTagsToStream(AStream, ATagSorting);

  if (FPhoneList <> nil) then FPhoneList.SaveToStream(AStream);
  if (FEmailList <> nil) then FEmailList.SaveToStream(AStream);
  if (FFaxList <> nil) then FFaxList.SaveToStream(AStream);
  if (FWWWList <> nil) then FWWWList.SaveToStream(AStream);
end;

procedure TGEDCOMAddress.SetAddress(const Value: TStrings);
begin
  SetTagStrings(Self, Value);
end;

procedure TGEDCOMAddress.SetEmailAddresses(Index: Integer;
  const Value: string);
begin
  if (Index >= GEDCOMMaxEmailAddresses)
  then raise EGEDCOMException.CreateFmt(EMaxEmailAddresses, [GEDCOMMaxEmailAddresses])
  else
  if (Index >= 0) then begin
    if (FEmailList = nil)
    then FEmailList := TGEDCOMList.Create(Self);

    while (Index >= FEmailList.Count) do
      FEmailList.Add(TGEDCOMTag.CreateTag(Owner, Self, 'EMAIL', ''));

    TGEDCOMTag(FEmailList[Index]).StringValue := Value;
  end
end;

procedure TGEDCOMAddress.SetFaxNumbers(Index: Integer;
  const Value: string);
begin
  if (Index >= GEDCOMMaxFaxNumbers)
  then raise EGEDCOMException.CreateFmt(EMaxFaxNumbers, [GEDCOMMaxFaxNumbers])
  else
  if (Index >= 0) then begin
    if (FFaxList = nil)
    then FFaxList := TGEDCOMList.Create(Self);

    while (Index >= FFaxList.Count) do
      FFaxList.Add(TGEDCOMTag.CreateTag(Owner, Self, 'FAX', ''));

    TGEDCOMTag(FFaxList[Index]).StringValue := Value;
  end
end;

procedure TGEDCOMAddress.SetPhoneNumbers(Index: Integer;
  const Value: string);
begin
  if (Index >= GEDCOMMaxPhoneNumbers)
  then raise EGEDCOMException.CreateFmt(EMaxPhoneNumbers, [GEDCOMMaxPhoneNumbers])
  else
  if (Index >= 0) then begin
    if (FPhoneList = nil)
    then FPhoneList := TGEDCOMList.Create(Self);

    while (Index >= FPhoneList.Count) do
      FPhoneList.Add(TGEDCOMTag.CreateTag(Owner, Self, 'PHON', ''));

    TGEDCOMTag(FPhoneList[Index]).StringValue := Value;
  end
end;

procedure TGEDCOMAddress.SetStringTag(Index: Integer; const Value: string);
begin
  case Index of
    1: SetTagStringValue('ADR1', Value);
    2: SetTagStringValue('ADR2', Value);
    3: SetTagStringValue('ADR3', Value);
    4: SetTagStringValue('CITY', Value);
    5: SetTagStringValue('STAE', Value);
    6: SetTagStringValue('POST', Value);
    7: SetTagStringValue('CTRY', Value);
  end;
end;

procedure TGEDCOMAddress.SetWebPages(Index: Integer; const Value: string);
begin
  if (Index >= GEDCOMMaxWebPages)
  then raise EGEDCOMException.CreateFmt(EMaxWebPages, [GEDCOMMaxWebPages])
  else
  if (Index >= 0)
  then begin
    if (FWWWList = nil)
    then FWWWList := TGEDCOMList.Create(Self);

    while (Index >= FWWWList.Count) do
      FWWWList.Add(TGEDCOMTag.CreateTag(Owner, Self, 'WWW', ''));

    TGEDCOMTag(FWWWList[Index]).StringValue := Value;
  end
end;

{ TGEDCOMRepositoryCitation }

constructor TGEDCOMRepositoryCitation.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FName := 'REPO';
end;

{ TGEDCOMPersonalNamePieces }

constructor TGEDCOMPersonalNamePieces.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent, [stNotes, stSource]);
end;

procedure TGEDCOMPersonalNamePieces.SaveToStream(AStream: TStream);
begin
  //inherited SaveToStream(AStream);
  //SaveValueToStream(AStream);
  SaveTagsToStream(AStream, []);

  // hack: по причине вложенности объектов TGEDCOMPersonalNamePieces -
  // для них нельзя выполнять SaveValueToStream(), поэтому
  // нельзя вызывать унаследованный SaveToStream().
  if Assigned(FNotes) then FNotes.SaveToStream(AStream);
  if Assigned(FSourceCitations) then FSourceCitations.SaveToStream(AStream);
  //if Assigned(FMultimediaLinks) then FMultimediaLinks.SaveToStream(AStream);
end;

function TGEDCOMPersonalNamePieces.GetStringTag(Index: Integer): string;
begin
  case Index of
    1: Result := TagStringValue('NPFX');
    2: Result := TagStringValue('GIVN');
    3: Result := TagStringValue('NICK');
    4: Result := TagStringValue('SPFX');
    5: Result := TagStringValue('SURN');
    6: Result := TagStringValue('NSFX');
  end;
end;

procedure TGEDCOMPersonalNamePieces.SetStringTag(Index: Integer; const Value: string);
begin
  case Index of
    1: SetTagStringValue('NPFX', Value);
    2: SetTagStringValue('GIVN', Value);
    3: SetTagStringValue('NICK', Value);
    4: SetTagStringValue('SPFX', Value);
    5: SetTagStringValue('SURN', Value);
    6: SetTagStringValue('NSFX', Value);
  end;
end;

{ TGEDCOMPersonalName }

constructor TGEDCOMPersonalName.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FName := 'NAME';
  FPieces := TGEDCOMPersonalNamePieces.Create(AOwner, Self);
  FPieces.SetLevel(Level);
end;

destructor TGEDCOMPersonalName.Destroy;
begin
  FPieces.Destroy;
  inherited Destroy;
end;

procedure TGEDCOMPersonalName.Assign(Source: TGEDCOMCustomTag);
begin
  inherited Assign(Source);

  if (Source is TGEDCOMPersonalName)
  then FPieces.Assign((Source as TGEDCOMPersonalName).Pieces);
end;

function TGEDCOMPersonalName.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);

  if (SU = 'TYPE') or (SU = 'FONE') or (SU = 'ROMN')
  then Result := inherited AddTag(ATag, AValue, AClass)
  else Result := FPieces.AddTag(ATag, AValue, AClass);
end;

procedure TGEDCOMPersonalName.Clear();
begin
  inherited Clear();
  //if (FPiecesList <> nil) then FPiecesList.Clear;
end;

procedure TGEDCOMPersonalName.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);
  FPieces.ResetOwner(AOwner);
end;

procedure TGEDCOMPersonalName.ReplaceXRefs(aMap: TXRefReplaceMap);
begin
  inherited ReplaceXRefs(aMap);
  FPieces.ReplaceXRefs(aMap);
end;

procedure TGEDCOMPersonalName.SaveToStream(AStream: TStream);
begin
  inherited SaveToStream(AStream);
  FPieces.SaveToStream(AStream);
end;

function TGEDCOMPersonalName.IsEmpty(): Boolean;
begin
  Result := inherited IsEmpty() and FPieces.IsEmpty();
end;

function TGEDCOMPersonalName.GetFirstPart: string;
begin
  Result := StringValue;
  if (Pos('/', Result) > 0)
  then Result := TrimRight(Copy(Result, 1, Pos('/', Result)-1));
end;

function TGEDCOMPersonalName.GetFullName: string;
begin
  Result := StringValue;
  while Pos('/', Result) > 0 do
    StrDelete(Result, Pos('/', Result), 1);
end;

function TGEDCOMPersonalName.GetLastPart: string;
begin
  if (Pos('/', StringValue) > 0) then begin
    Result := Copy(StringValue, Pos('/', StringValue) + 1, MaxInt);
    if (Pos('/', Result) > 0)
    then Result := TrimLeft(Copy(Result, Pos('/', Result) + 1, MaxInt))
    else Result := '';
  end else Result := '';
end;

function TGEDCOMPersonalName.GetSurname: string;
begin
  if (Pos('/', StringValue) > 0) then begin
    Result := Copy(StringValue, Pos('/', StringValue) + 1, MaxInt);
    if (Pos('/', Result) > 0)
    then Result := Copy(Result, 1, Pos('/', Result) - 1);
  end else Result := '';
end;

procedure TGEDCOMPersonalName.SetNameParts(const FirstPart, Surname, LastPart: string);
begin
  StringValue := TrimLeft(FirstPart + ' ') + '/' + Surname + '/' + TrimRight(' ' + LastPart);
end;

procedure TGEDCOMPersonalName.SetSurname(const Value: string);
begin
  StringValue := TrimLeft(FirstPart + ' ') + '/' + Value + '/' + TrimRight(' ' + LastPart);
end;

{ TGEDCOMChildToFamilyLink }

constructor TGEDCOMChildToFamilyLink.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FName := 'FAMC';
end;

function TGEDCOMChildToFamilyLink.GetChildLinkageStatus: TGEDCOMChildLinkageStatus;
var
  S: string;
begin
  S := LowerCase(Trim(TagStringValue('STAT')));
  if (S = 'challenged')
  then Result := clChallenged
  else
  if (S = 'disproven')
  then Result := clDisproven
  else
  if (S = 'proven')
  then Result := clProven
  else Result := clNone;
end;

function TGEDCOMChildToFamilyLink.GetFamily: TGEDCOMFamilyRecord;
begin
  Result := TGEDCOMFamilyRecord(GetValue);
end;

function TGEDCOMChildToFamilyLink.GetPedigreeLinkageType: TGEDCOMPedigreeLinkageType;
var
  S: string;
begin
  S := LowerCase(Trim(TagStringValue('PEDI')));
  if (S = 'adopted')
  then Result := plAdopted
  else
  if (S = 'birth')
  then Result := plBirth
  else
  if (S = 'foster')
  then Result := plFoster
  else
  if (S = 'sealing')
  then Result := plSealing
  else Result := plNone;
end;

procedure TGEDCOMChildToFamilyLink.SetChildLinkageStatus(
  const Value: TGEDCOMChildLinkageStatus);
var
  S: string;
begin
  case Value of
    clChallenged: S := 'challenged';
    clDisproven: S := 'disproven';
    clProven: S := 'proven';
    else S := '';
  end;
  SetTagStringValue('STAT', S);
end;

procedure TGEDCOMChildToFamilyLink.SetFamily(const Value: TGEDCOMFamilyRecord);
begin
  SetValue(Value);
end;

procedure TGEDCOMChildToFamilyLink.SetPedigreeLinkageType(
  const Value: TGEDCOMPedigreeLinkageType);
var
  S: string;
begin
  case Value of
    plAdopted: S := 'adopted';
    plBirth: S := 'birth';
    plFoster: S := 'foster';
    plSealing: S := 'sealing';
    else S := '';
  end;
  SetTagStringValue('PEDI', S);
end;

{ TGEDCOMPointerWithNotes }

constructor TGEDCOMPointerWithNotes.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FNotes := nil;
end;

destructor TGEDCOMPointerWithNotes.Destroy;
begin
  if (FNotes <> nil) then FNotes.Free;
  inherited Destroy;
end;

function TGEDCOMPointerWithNotes.AddNotes(ANotes: TGEDCOMNotes): TGEDCOMNotes;
begin
  Result := ANotes;

  if (FNotes = nil)
  then FNotes := TGEDCOMList.Create(Self);

  if (ANotes <> nil)
  then FNotes.Add(ANotes);
end;

function TGEDCOMPointerWithNotes.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);

  if (SU = 'NOTE')
  then Result := AddNotes(TGEDCOMNotes.CreateTag(Owner, Self, SU, AValue))
  else Result := inherited AddTag(ATag, AValue, AClass);
end;

procedure TGEDCOMPointerWithNotes.Clear;
begin
  inherited Clear;
  if (FNotes <> nil) then FNotes.Clear;
end;

function TGEDCOMPointerWithNotes.GetNotes(Index: Integer): TGEDCOMNotes;
begin
  if (FNotes = nil)
  then Result := nil
  else Result := TGEDCOMNotes(FNotes[Index]);
end;

function TGEDCOMPointerWithNotes.GetNotesCount: Integer;
begin
  if (FNotes = nil)
  then Result := 0
  else Result := FNotes.Count;
end;

function TGEDCOMPointerWithNotes.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and (Count = 0) and (GetNotesCount = 0);
end;

procedure TGEDCOMPointerWithNotes.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);

  if (FNotes <> nil) then FNotes.ResetOwner(AOwner);
end;

procedure TGEDCOMPointerWithNotes.ReplaceXRefs(aMap: TXRefReplaceMap);
begin
  inherited ReplaceXRefs(aMap);
  if (FNotes <> nil) then FNotes.ReplaceXRefs(aMap);
end;

{ TGEDCOMSpouseToFamilyLink }

constructor TGEDCOMSpouseToFamilyLink.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FName := 'FAMS';
end;

function TGEDCOMSpouseToFamilyLink.GetFamily: TGEDCOMFamilyRecord;
begin
  Result := TGEDCOMFamilyRecord(GetValue);
end;

procedure TGEDCOMSpouseToFamilyLink.SetFamily(const Value: TGEDCOMFamilyRecord);
begin
  SetValue(Value);
end;

{ TGEDCOMAssociation }

constructor TGEDCOMAssociation.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FName := 'ASSO';
  FSourceCitations := nil;
end;

destructor TGEDCOMAssociation.Destroy;
begin
  if (FSourceCitations <> nil) then FSourceCitations.Free;
  inherited Destroy;
end;

function TGEDCOMAssociation.AddSourceCitation(
  ASourceCitation: TGEDCOMSourceCitation): TGEDCOMSourceCitation;
begin
  Result := ASourceCitation;

  if (FSourceCitations = nil)
  then FSourceCitations := TGEDCOMList.Create(Self);

  if (ASourceCitation <> nil)
  then FSourceCitations.Add(ASourceCitation);
end;

function TGEDCOMAssociation.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);
  if (SU = 'SOUR')
  then Result := AddSourceCitation(TGEDCOMSourceCitation.CreateTag(Owner, Self, SU, AValue))
  else Result := inherited AddTag(ATag, AValue, AClass);
end;

procedure TGEDCOMAssociation.Clear;
begin
  inherited Clear;
  if (FSourceCitations <> nil) then FSourceCitations.Clear;
end;

function TGEDCOMAssociation.GetIndividual: TGEDCOMIndividualRecord;
begin
  Result := TGEDCOMIndividualRecord(GetValue);
end;

function TGEDCOMAssociation.GetRelation: string;
begin
  Result := TagStringValue('RELA');
end;

function TGEDCOMAssociation.GetSourceCitations(Index: Integer): TGEDCOMSourceCitation;
begin
  if (FSourceCitations = nil)
  then Result := nil
  else Result := TGEDCOMSourceCitation(FSourceCitations[Index]);
end;

function TGEDCOMAssociation.GetSourceCitationsCount: Integer;
begin
  if (FSourceCitations = nil)
  then Result := 0
  else Result := FSourceCitations.Count;
end;

function TGEDCOMAssociation.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and (GetSourceCitationsCount = 0);
end;

procedure TGEDCOMAssociation.SetIndividual(const Value: TGEDCOMIndividualRecord);
begin
  SetValue(Value);
end;

procedure TGEDCOMAssociation.SetRelation(const Value: string);
begin
  SetTagStringValue('RELA', Value);
end;

procedure TGEDCOMAssociation.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);

  if (FSourceCitations <> nil) then FSourceCitations.ResetOwner(AOwner);
end;

procedure TGEDCOMAssociation.ReplaceXRefs(aMap: TXRefReplaceMap);
begin
  inherited ReplaceXRefs(aMap);
  if (FSourceCitations <> nil) then FSourceCitations.ReplaceXRefs(aMap);
end;

{ TGEDCOMPlace }

constructor TGEDCOMPlace.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent, [stNotes]);
  FName := 'PLAC';
end;

function TGEDCOMPlace.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);

  if (SU = 'FORM')
  then //Result := inherited AddTag(ATag, AValue, TGEDCOMDateValue)
  else
  if (SU = 'FONE')
  then //Result := inherited AddTag(ATag, AValue, TGEDCOMAddress)
  else
  if (SU = 'ROMN')
  then //Result := inherited AddTag(ATag, AValue, TGEDCOMAddress)
  else
  if (SU = 'MAP')
  then //Result := inherited AddTag(ATag, AValue, TGEDCOMAddress)
  else Result := inherited AddTag(ATag, AValue, AClass);
end;

procedure TGEDCOMPlace.SaveToStream(AStream: TStream);
begin
  inherited SaveToStream(AStream);
end;

{ TGEDCOMEventDetail }

constructor TGEDCOMEventDetail.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent, [stNotes, stSource, stMultimedia]);
  FLevel := TGEDCOMCustomTag(AParent).Level;
end;

function TGEDCOMEventDetail.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);
  if (SU = 'DATE')
  then Result := inherited AddTag(ATag, AValue, TGEDCOMDateValue)
  else
  if (SU = 'ADDR')
  then Result := inherited AddTag(ATag, AValue, TGEDCOMAddress)
  else
  if (SU = 'PHON') or (SU = 'EMAIL') or (SU = 'FAX') or (SU = 'WWW')
  then Result := TGEDCOMAddress(TagClass('ADDR', TGEDCOMAddress)).AddTag(ATag, AValue, AClass)
  else Result := inherited AddTag(ATag, AValue, AClass);
end;

function TGEDCOMEventDetail.GetAddress: TGEDCOMAddress;
begin
  Result := TGEDCOMAddress(TagClass('ADDR', TGEDCOMAddress));
end;

function TGEDCOMEventDetail.GetDate: TGEDCOMDateValue;
begin
  Result := TGEDCOMDateValue(TagClass('DATE', TGEDCOMDateValue));
end;

function TGEDCOMEventDetail.GetRestriction: TGEDCOMRestriction;
var
  S: string;
begin
  S := UpperCase(Trim(TagStringValue('RESN')));
  if (S = 'CONFIDENTIAL')
  then Result := rnConfidential
  else
  if (S = 'LOCKED')
  then Result := rnLocked
  else
  if (S = 'PRIVACY')
  then Result := rnPrivacy
  else Result := rnNone;   // default no restriction
end;

function TGEDCOMEventDetail.GetStringTag(Index: Integer): string;
begin
  case Index of
    1: Result := TagStringValue('TYPE');        // Classification
    2: Result := TagStringValue('PLAC');
    3: Result := TagStringValue('PLAC\FORM');
    4: Result := TagStringValue('AGNC');        // Responsible agency
    5: Result := TagStringValue('RELI');        // Religious affilation
    6: Result := TagStringValue('CAUS');        // Cause of event
  end;
end;

procedure TGEDCOMEventDetail.SetRestriction(const Value: TGEDCOMRestriction);
var
  S: string;
begin
  case Value of
    rnConfidential: S := 'confidential';
    rnLocked: S := 'locked';
    rnPrivacy: S := 'privacy';
    else S := '';
  end;
  SetTagStringValue('RESN', S);
end;

procedure TGEDCOMEventDetail.SetStringTag(Index: Integer; const Value: string);
begin
  case Index of
    1: SetTagStringValue('TYPE', Value);        // Classification
    2: SetTagStringValue('PLAC', Value);
    3: SetTagStringValue('PLAC\FORM', Value);
    4: SetTagStringValue('AGNC', Value);        // Responsible agency
    5: SetTagStringValue('RELI', Value);        // Religious affilation
    6: SetTagStringValue('CAUS', Value);        // Cause of event
  end;
end;

procedure TGEDCOMEventDetail.SaveToStream(AStream: TStream);
begin
  //inherited SaveToStream(AStream);
  //SaveValueToStream(AStream);
  SaveTagsToStream(AStream, []);

  // hack: по причине вложенности объектов TGEDCOMEventDetail -
  // для них нельзя выполнять SaveValueToStream(), поэтому
  // нельзя вызывать унаследованный SaveToStream().
  if Assigned(FNotes) then FNotes.SaveToStream(AStream);
  if Assigned(FSourceCitations) then FSourceCitations.SaveToStream(AStream);
  if Assigned(FMultimediaLinks) then FMultimediaLinks.SaveToStream(AStream);
end;

{ TGEDCOMCustomEvent }

constructor TGEDCOMCustomEvent.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent);
  FDetail := TGEDCOMEventDetail.Create(Owner, Self);
  FDetail.SetLevel(Level);
end;

destructor TGEDCOMCustomEvent.Destroy;
begin
  FDetail.Free;
  inherited Destroy;
end;

procedure TGEDCOMCustomEvent.Assign(Source: TGEDCOMCustomTag);
begin
  inherited Assign(Source);

  if (Source is TGEDCOMCustomEvent)
  then FDetail.Assign((Source as TGEDCOMCustomEvent).Detail);
end;

procedure TGEDCOMCustomEvent.ReplaceXRefs(aMap: TXRefReplaceMap);
begin
  inherited ReplaceXRefs(aMap);
  FDetail.ReplaceXRefs(aMap);
end;

procedure TGEDCOMCustomEvent.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);
  FDetail.ResetOwner(AOwner);
end;

procedure TGEDCOMCustomEvent.SaveToStream(AStream: TStream);
begin
  inherited SaveToStream(AStream);
  FDetail.SaveToStream(AStream);
end;

{ TGEDCOMFamilyEvent }

function TGEDCOMFamilyEvent.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
begin
  Result := FDetail.AddTag(ATag, AValue, AClass);
end;

{ TGEDCOMIndividualEvent }

function TGEDCOMIndividualEvent.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);
  if (SU = 'FAMC')
  then Result := inherited AddTag(ATag, AValue, TGEDCOMPointer)
  else Result := FDetail.AddTag(ATag, AValue, AClass);
end;

function TGEDCOMIndividualEvent.GetFamily: TGEDCOMPointer;
begin
  Result := TGEDCOMPointer(TagClass('FAMC', TGEDCOMPointer));
end;

{ TGEDCOMIndividualAttribute }

function TGEDCOMIndividualAttribute.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);
  if (SU = 'CONC') or (SU = 'CONT')
  then Result := inherited AddTag(ATag, AValue, AClass)
  else Result := FDetail.AddTag(ATag, AValue, AClass);
end;

function TGEDCOMIndividualAttribute.GetPhysicalDescription: TStrings;
begin
  Result := TagStrings(Self, FPhysicalDescription);
end;

procedure TGEDCOMIndividualAttribute.SetPhysicalDescription(Value: TStrings);
begin
  SetTagStrings(Self, Value);
end;

{==============================================================================}

{ TGEDCOMGroupRecord }

constructor TGEDCOMGroupRecord.Create(AOwner, AParent: TGEDCOMObject);
begin
  inherited Create(AOwner, AParent, [stNotes, stMultimedia]);
  FName := '_GROUP';
  FMembers := nil;
end;

destructor TGEDCOMGroupRecord.Destroy;
begin
  if (FMembers <> nil) then FMembers.Free;
  inherited Destroy;
end;

function TGEDCOMGroupRecord.AddTag(const ATag, AValue: string;
  AClass: TGEDCOMTagClass): TGEDCOMTag;
var
  SU: string;
begin
  SU := UpperCase(ATag);

  if (SU = 'NAME')
  then Result := inherited AddTag(ATag, AValue)
  else
  if (SU = '_MEMBER')
  then Result := TGEDCOMTag(AddMember(TGEDCOMPointer.CreateTag(Owner, Self, SU, AValue)))
  else Result := inherited AddTag(ATag, AValue, AClass);
end;

function TGEDCOMGroupRecord.AddMember(Value: TGEDCOMPointer): TGEDCOMPointer;
begin
  Result := Value;

  if (FMembers = nil)
  then FMembers := TGEDCOMList.Create(Self);

  if (FMembers <> nil)
  then FMembers.Add(Value);
end;

procedure TGEDCOMGroupRecord.Clear;
begin
  inherited Clear;
  if (FMembers <> nil) then FMembers.Clear;
end;

function TGEDCOMGroupRecord.GetMember(Index: Integer): TGEDCOMPointer;
begin
  if (FMembers = nil) or (Index < 0) or (Index >= FMembers.Count)
  then Result := nil
  else Result := TGEDCOMPointer(FMembers[Index]);
end;

function TGEDCOMGroupRecord.GetMembersCount: Integer;
begin
  if (FMembers = nil)
  then Result := 0
  else Result := FMembers.Count;
end;

function TGEDCOMGroupRecord.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and (GetMembersCount = 0);
end;

procedure TGEDCOMGroupRecord.ReplaceXRefs(aMap: TXRefReplaceMap);
begin
  inherited ReplaceXRefs(aMap);

  if (FMembers <> nil) then FMembers.ReplaceXRefs(aMap);
end;

procedure TGEDCOMGroupRecord.ResetOwner(AOwner: TGEDCOMObject);
begin
  inherited ResetOwner(AOwner);

  if (FMembers <> nil) then FMembers.ResetOwner(AOwner);
end;

function TGEDCOMGroupRecord.GetName(): string;
begin
  Result := TagStringValue('NAME');
end;

procedure TGEDCOMGroupRecord.SetName(const Value: string);
begin
  SetTagStringValue('NAME', Value);
end;

procedure TGEDCOMGroupRecord.SaveToStream(AStream: TStream);
begin
  inherited SaveToStream(AStream);

  if Assigned(FMembers) then FMembers.SaveToStream(AStream);
end;

procedure TGEDCOMGroupRecord.RemoveMember(aIndex: Integer);
begin
  if (FMembers <> nil)
  then FMembers.Delete(aIndex);
end;

function TGEDCOMGroupRecord.IndexOfMember(aMember: TGEDCOMIndividualRecord): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to FMembers.Count - 1 do
    if (CleanXRef(TGEDCOMPointer(FMembers[i]).XRef) = CleanXRef(aMember.XRef)) then begin
      Result := i;
      Break;
    end;
end;

end.
