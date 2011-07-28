unit GKEngine; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.IO, System.Drawing, System.Windows.Forms, System.Collections,
  VCLStub, GedCom551, GKUtils, GKProgress, GKCtrls, GKLangs;

type
  TGenEngine = class(TObject)
  private
    type
      TRetCount = packed record
        lngSubRows: Word;
        lngCountLike: Word;
      end;

  public
    const
      AppName = 'GEDKeeper';

      MLinkPrefix = 'view_';
      AdvTag = '_ADVANCED';
      ExtTag = '_EXT_NAME';

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
      Restrictions: array [TGEDCOMObject.TGEDCOMRestriction] of string = (
          'нет',
          'конфиденциально',
          'заперто',
          'секретно');

      RecordTypes: array [TGEDCOMRecord.TGEDCOMRecordType] of LSID = (
        LSID_None,
        LSID_Person, LSID_Family, LSID_Note, LSID_RPMultimedia, LSID_Source,
        LSID_Repository, LSID_Group, LSID_Research, LSID_Task, LSID_Communication,
        LSID_Location,
        LSID_None, LSID_None
      );

    type
      TSexRec = record
        NameId: LSID;
        Sign: string;
      end;

    const
      SexData: array [TGEDCOMObject.TGEDCOMSex] of TSexRec = (
        (NameId: LSID_SexN; Sign: 'N'),
        (NameId: LSID_SexM; Sign: 'M'),
        (NameId: LSID_SexF; Sign: 'F'),
        (NameId: LSID_SexU; Sign: 'U')
      );

    const
      MarriageStatusSize = 4;
      MarriageStatus: array [0..MarriageStatusSize-1] of record
        Name: LSID;
        StatSign: string;
      end = (
        (Name: LSID_Unknown; StatSign: ''),
        (Name: LSID_MarrRegistered; StatSign: 'MARRIED'),
        (Name: LSID_MarrNotRegistered; StatSign: 'MARRNOTREG'),
        (Name: LSID_MarrDivorced; StatSign: 'NOTMARR')
      );

      PersonEventsSize = 36;
      PersonEvents: array [0..PersonEventsSize-1] of record
        Name: LSID;
        Sign: string;
        Kind: TPersonEventKind;
      end = (
        (Name: LSID_Event; Sign: 'EVEN'; Kind: ekEvent),              {std:check, EV}
        (Name: LSID_Birth; Sign: 'BIRT'; Kind: ekEvent),              {std:check, EV}
        (Name: LSID_Adoption; Sign: 'ADOP'; Kind: ekEvent),           {std:check, EV}
        (Name: LSID_Christening; Sign: 'CHR'; Kind: ekEvent),         {std:check, EV}
        (Name: LSID_Graduation; Sign: 'GRAD'; Kind: ekEvent),         {std:check, EV}
        (Name: LSID_Retirement; Sign: 'RETI'; Kind: ekEvent),         {std:check, EV}
        (Name: LSID_Naturalization; Sign: 'NATU'; Kind: ekEvent),     {std:check, EV}
        (Name: LSID_Emigration; Sign: 'EMIG'; Kind: ekEvent),         {std:check, EV}
        (Name: LSID_Immigration; Sign: 'IMMI'; Kind: ekEvent),        {std:check, EV}
        (Name: LSID_Census; Sign: 'CENS'; Kind: ekEvent),             {std:check, EV}
        (Name: LSID_LastWill; Sign: 'WILL'; Kind: ekEvent),           {std:check, EV}
        (Name: LSID_ProbateOfWill; Sign: 'PROB'; Kind: ekEvent),      {std:check, EV}
        (Name: LSID_Death; Sign: 'DEAT'; Kind: ekEvent),              {std:check, EV}
        (Name: LSID_Burial; Sign: 'BURI'; Kind: ekEvent),             {std:check, EV}
        (Name: LSID_Cremation; Sign: 'CREM'; Kind: ekEvent),          {std:check, EV}

        (Name: LSID_Fact; Sign: 'FACT'; Kind: ekFact),                {std:check, AT}
        (Name: LSID_Religion; Sign: 'RELI'; Kind: ekFact),            {std:check, AT}
        (Name: LSID_Nationality; Sign: 'NATI'; Kind: ekFact),         {std:check, AT}
        (Name: LSID_Residence; Sign: 'RESI'; Kind: ekFact),           {std:check, AT}
        (Name: LSID_PhysicalDesc; Sign: 'DSCR'; Kind: ekFact),        {std:check, AT}
        (Name: LSID_NationalIDNumber; Sign: 'IDNO'; Kind: ekFact),    {std:check, AT}
        (Name: LSID_SocialSecurityNumber; Sign: 'SSN'; Kind: ekFact), {std:check, AT}
        (Name: LSID_ChildsCount; Sign: 'NCHI'; Kind: ekFact),         {std:check, AT}
        (Name: LSID_MarriagesCount; Sign: 'NMR'; Kind: ekFact),       {std:check, AT}
        (Name: LSID_Education; Sign: 'EDUC'; Kind: ekFact),           {std:check, AT}
        (Name: LSID_Occupation; Sign: 'OCCU'; Kind: ekFact),          {std:check, AT}
        (Name: LSID_Caste; Sign: 'CAST'; Kind: ekFact),               {std:check, AT}
        (Name: LSID_Property; Sign: 'PROP'; Kind: ekFact),            {std:check, AT}
        (Name: LSID_NobilityTitle; Sign: 'TITL'; Kind: ekFact),       {std:check, AT}

        (Name: LSID_Travel; Sign: '_TRAVEL'; Kind: ekFact),           {non-std, AT}
        (Name: LSID_Hobby; Sign: '_HOBBY'; Kind: ekFact),             {non-std, AT}
        (Name: LSID_Award; Sign: '_AWARD'; Kind: ekFact),             {non-std, AT}

        (Name: LSID_Mili; Sign: '_MILI'; Kind: ekFact),               {non-std, AT}
        (Name: LSID_MiliInd; Sign: '_MILI_IND'; Kind: ekFact),        {non-std, AT}
        (Name: LSID_MiliDis; Sign: '_MILI_DIS'; Kind: ekFact),        {non-std, AT}
        (Name: LSID_MiliRank; Sign: '_MILI_RANK'; Kind: ekFact)       {non-std, AT}
      );

      DateKindsSize = 10;
      DateKinds: array [0..DateKindsSize-1] of record
        Name: LSID;
        Dates: TDateControlsRange;
      end = (
        { 0} (Name: LSID_DK_0;  Dates: [1]),{+}
        { 1} (Name: LSID_DK_1;  Dates: [2]),
        { 2} (Name: LSID_DK_2;  Dates: [1]),
        { 3} (Name: LSID_DK_3;  Dates: [1, 2]),
        { 4} (Name: LSID_DK_4;  Dates: [1]),
        { 5} (Name: LSID_DK_5;  Dates: [2]),
        { 6} (Name: LSID_DK_6;  Dates: [1, 2]),
        { 7} (Name: LSID_DK_7;  Dates: [1]),{+}
        { 8} (Name: LSID_DK_8;  Dates: [1]),{+}
        { 9} (Name: LSID_DK_9;  Dates: [1]) {+}
      );

      DateCalendars: array [TGEDCOMDate.TGEDCOMCalendar] of LSID =
        (LSID_Cal_Gregorian, LSID_Cal_Julian, LSID_Cal_Hebrew,
         LSID_Cal_French, LSID_Cal_Roman, LSID_Unknown);

      FamilyEventsSize = 10;
      FamilyEvents: array [0..FamilyEventsSize-1] of record
        Name: LSID;
        Sign: string;
      end = (
        (Name: LSID_Event; Sign: 'EVEN'),
        (Name: LSID_FEvt_1; Sign: 'ENGA'),
        (Name: LSID_FEvt_2; Sign: 'MARR'),
        (Name: LSID_FEvt_3; Sign: 'MARB'),
        (Name: LSID_FEvt_4; Sign: 'MARC'),
        (Name: LSID_FEvt_5; Sign: 'MARL'),
        (Name: LSID_FEvt_6; Sign: 'MARS'),
        (Name: LSID_FEvt_7; Sign: 'ANUL'),
        (Name: LSID_FEvt_8; Sign: 'DIVF'),
        (Name: LSID_FEvt_9; Sign: 'DIV')
      );

      {NamePiecesSize = 6;
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
      );}

    type
      TGKStoreType = (gstReference, gstArchive, gstStorage);

    const
      GKStoreType: array [TGKStoreType] of record
        Name: LSID;
        Sign: string;
      end = (
        (Name: LSID_STRef; Sign: ''), {for compatibility}
        (Name: LSID_STArc; Sign: 'arc:'),
        (Name: LSID_STStg; Sign: 'stg:')
      );

    const
      MediaTypes: array [TGEDCOMFileReference.TGEDCOMMediaType] of LSID = (
        LSID_MT_01, LSID_MT_02, LSID_MT_03, LSID_MT_04,
        LSID_MT_05, LSID_MT_06, LSID_MT_07, LSID_MT_08,
        LSID_MT_09, LSID_MT_10, LSID_MT_11, LSID_MT_12,
        LSID_MT_13, LSID_MT_14, LSID_MT_15
      );

    const
      PriorityNames: array [TResearchPriority] of LSID = (
        LSID_Prt_1, LSID_Prt_2, LSID_Prt_3, LSID_Prt_4, LSID_Prt_5);

      StatusNames: array [TResearchStatus] of LSID = (
        LSID_RStat_1, LSID_RStat_2, LSID_RStat_3,
        LSID_RStat_4, LSID_RStat_5, LSID_RStat_6);

      CommunicationNames: array [TGEDCOMCommunicationRecord.TCommunicationType] of LSID = (
        LSID_Com_1, LSID_Com_2, LSID_Com_3, LSID_Com_4, LSID_Com_5, LSID_Com_6);

      CommunicationDirs: array [TGEDCOMCommunicationRecord.TCommunicationDir] of LSID = (LSID_CD_1, LSID_CD_2);

      GoalNames: array [TGoalType] of LSID = (
        LSID_G_1, LSID_G_2, LSID_G_3, LSID_G_4);

      CertaintyAssessments: array [0..3] of LSID = (
        LSID_Cert_1, LSID_Cert_2, LSID_Cert_3, LSID_Cert_4);

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

    type
      TGEDCOMFormat = (gf_Unknown, gf_Native,
        gf_GENBOX, gf_ALTREE, gf_AGES, gf_PAF);

    const
      GEDCOMFormats: array [TGEDCOMFormat] of record
        Sign: string;
        Name: string;
      end = (
        (Sign: ''; Name: ''),
        (Sign: 'GEDKeeper'; Name: ''),
        (Sign: 'GENBOX'; Name: 'Genbox Family History'),
        (Sign: 'ALTREE'; Name: 'Agelong Tree'), // Древо Жизни 4
        (Sign: 'AGES'; Name: 'Ages!'),
        (Sign: 'PAF'; Name: 'Personal Ancestral File')
      );

    type
      TTreeWalkMode = (twmAll, twmFamily, twmAncestors, twmDescendants, twmNone);

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

      TStatMode = (
        smAncestors, smDescendants, smDescGenerations,
        smFamilies, smNames, smPatronymics,
        smAge, smLifeExpectancy,
        smBirthYears, smBirthTenYears, smDeathYears, smDeathTenYears,
        smChildsCount, smChildsDistribution,
        smBirthPlaces, smDeathPlaces, smResidences, smOccupation,
        smReligious, smNational, smEducation, smCaste,
        smFirstbornAge, smMarriages, smMarriageAge, smSpousesDiff,
        smHobby, smAward, smMili, smMiliInd, smMiliDis, smMiliRank);

      TPatriarchObj = class(TObject)
      public
        IRec: TGEDCOMIndividualRecord;
        IBirthYear,
        IDescendantsCount,
        IDescGenerations: Integer;
        ILinks: set of Byte;
      end;

      TValsItem = class(System.Object)
      private
      public
        Caption: string;
        Value: Integer;

        constructor Create(aCaption: string; aValue: Integer);

        function ToString: string; override;
      end;

    type
      TRelationKind = (
        // runtime
        rkNone, rkParent, rkSpouse, rkChild,

        // base
        rkFather, rkMother,
        rkHusband, rkWife,
        rkSon, rkDaughter,
        rkGrandfather, rkGrandmother,
        rkGrandson, rkGranddaughter,
        rkBrother, rkSister,
        rkSonInLaw, rkDaughterInLaw,
        rkHusbandFather, rkHusbandMother,
        rkWifeFather, rkWifeMother,
        rkUncle, rkAunt,
        rkNephew, rkNiece,
        rkCousinM, rkCousinF,

        // runtime
        rkSame, rkUndefined
      );

    const
      RelationKinds: array [TRelationKind] of LSID = (
        LSID_RK_Unk, LSID_None, LSID_None, LSID_None,

        LSID_RK_Father, LSID_RK_Mother,
        LSID_RK_Husband, LSID_RK_Wife,
        LSID_RK_Son, LSID_RK_Daughter,
        LSID_RK_Grandfather, LSID_RK_Grandmother,
        LSID_RK_Grandson, LSID_RK_Granddaughter,
        LSID_RK_Brother, LSID_RK_Sister,
        LSID_RK_SonInLaw, LSID_RK_DaughterInLaw,
        LSID_RK_HusbandFather, LSID_RK_HusbandMother,
        LSID_RK_WifeFather, LSID_RK_WifeMother,
        LSID_RK_Uncle, LSID_RK_Aunt,
        LSID_RK_Nephew, LSID_RK_Niece,
        LSID_RK_CousinM, LSID_RK_CousinF,

        // Деверь - брат мужа, золовка - сестра мужа, Шурин - брат жены, Свояченица — сестра жены.
        LSID_None, LSID_RK_Unk
      );

      RelationSigns: array [TRelationKind] of string = (
        '?', 'P', 'S', 'C',
        'F', 'M', 'H', 'W', 'Sn', 'Dg', 'Gf', 'Gm', 'Gs', 'Gd', 'Br', 'St',
        '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-',
        '-', '-'
      );

    type
      TKinshipRec = record
        PrevRels: TEnumSet;
        CurrRels: TEnumSet;
        FinRel: TRelationKind;
        Great, Level: Shortint;
      end;

    var
      Kinships: array of TKinshipRec;

      procedure RegisterKinship(aPrevRels: TEnumSet; aCurrRels: TEnumSet;
        aFinRel: TRelationKind; aGreat, aLevel: Shortint);
      procedure InitKinships();
      function FindKinship(prev, cur: TRelationKind; var great, level: Integer): TRelationKind;

    const
      Numerals: array [1..9] of string = (
        '-', 'дво', 'тро', 'четверо', 'пяти', 'шести', 'семи', 'восьми', 'девяти'
      );

      NumKinship: array [TGEDCOMObject.TGEDCOMSex] of string = (
        '-', 'юродный', 'юродная', ''
      );

    type
      TSyncState = (ssUndefined, ssHasMaster, ssNoMaster);

      TSyncRec = class(TObject)
      public
        MasterRecord, UpdateRecord: TGEDCOMRecord;
        State: TSyncState;
        UpdateOldXRef, UpdateNewXRef: string;
      end;


  private
    FFileName: string;
    FTree: TGEDCOMTree;

    function GetIsAdvanced: Boolean;
    procedure SetIsAdvanced(const Value: Boolean);
    function GetExtName: string;
    procedure SetExtName(const Value: string);

    function PatriarchsCompare(Item1, Item2: TObject): Integer;
    class function Matching(StrA, StrB: String; lngLen: Integer): TRetCount;

    class procedure CorrectIds(aTree: TGEDCOMTree);

    class procedure ReformNote(aTree: TGEDCOMTree; note: TGEDCOMNotes);
    class procedure ReformMultimediaLink(aTree: TGEDCOMTree; mmLink: TGEDCOMMultimediaLink);
    class procedure ReformSourceCitation(aTree: TGEDCOMTree; sourCit: TGEDCOMSourceCitation);

    class function GetDescGens_Recursive(aPerson: TGEDCOMIndividualRecord): Integer;

    class function DaysInAMonth(const AYear, AMonth: Word): Word;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddFamilySpouse(aFamily: TGEDCOMFamilyRecord; aSpouse: TGEDCOMIndividualRecord);
    procedure RemoveFamilySpouse(aFamily: TGEDCOMFamilyRecord; aSpouse: TGEDCOMIndividualRecord);

    function AddFamilyChild(aFamily: TGEDCOMFamilyRecord; aChild: TGEDCOMIndividualRecord): Boolean;
    function RemoveFamilyChild(aFamily: TGEDCOMFamilyRecord; aChild: TGEDCOMIndividualRecord): Boolean;

    function AddResearchTask(aResearch: TGEDCOMResearchRecord; aTask: TGEDCOMTaskRecord): Boolean;
    procedure RemoveResearchTask(aResearch: TGEDCOMResearchRecord; aTask: TGEDCOMTaskRecord);

    function AddResearchGroup(aResearch: TGEDCOMResearchRecord; aGroup: TGEDCOMGroupRecord): Boolean;
    procedure RemoveResearchGroup(aResearch: TGEDCOMResearchRecord; aGroup: TGEDCOMGroupRecord);

    function AddResearchComm(aResearch: TGEDCOMResearchRecord; aComm: TGEDCOMCommunicationRecord): Boolean;
    procedure RemoveResearchComm(aResearch: TGEDCOMResearchRecord; aComm: TGEDCOMCommunicationRecord);

    function AddGroupMember(aGroup: TGEDCOMGroupRecord; aMember: TGEDCOMIndividualRecord): Boolean;
    function RemoveGroupMember(aGroup: TGEDCOMGroupRecord; aMember: TGEDCOMIndividualRecord): Boolean;

    function AddAssociation(aRec: TGEDCOMIndividualRecord; aRel: string; aRelPerson: TGEDCOMIndividualRecord): TGEDCOMAssociation;

    procedure CleanFamily(aFamily: TGEDCOMFamilyRecord);
    function GetSubmitter(): TGEDCOMSubmitterRecord;

    function FindSource(aName: string): TGEDCOMSourceRecord;
    procedure GetSourcesList(aSources: TStringList);

    procedure GetCommonStats(var aStats: TCommonStats);
    procedure GetSpecStats(aMode: TStatMode; aVals: System.Collections.Hashtable);

    procedure GetPatriarchsList(aProgress, aLinks: Boolean;
      var aList: TObjectList; aMinGens: Integer = 2);
    function GetPatriarchLinks(lst: TObjectList; pObj: TPatriarchObj): string;

    function CheckPath(): Boolean;
    function GetArcFileName(): string;
    function GetStoreFolder(): string;
    function GetSpecExtName(): string;
    function GetStoreType(aFileRef: string; var aFileName: string): TGKStoreType;
    procedure MediaLoad(aRefName: string; var aStream: TStream); overload;
    procedure MediaLoad(aRefName: string; var aFileName: string); overload;
    procedure MediaSave(aFileName: string; aStoreType: TGKStoreType; var aRefPath: string);

    function SetPrimaryMultimediaRecord(aRec: TGEDCOMIndividualRecord;
      mmRec: TGEDCOMMultimediaRecord): TGEDCOMMultimediaLink;
    function GetPrimaryMultimediaLink(aRec: TGEDCOMIndividualRecord): TGEDCOMMultimediaLink;
    function GetPrimaryBitmap(aRec: TGEDCOMIndividualRecord): System.Drawing.Bitmap;

    procedure ArcFileLoad(target_fn: string; toStream: TStream);
    procedure ArcFileSave(aFileName, sfn: string);

    procedure SortFamilyChilds(aFamily: TGEDCOMFamilyRecord);

    property ExtName: string read GetExtName write SetExtName;
    property FileName: string read FFileName write FFileName;
    property IsAdvanced: Boolean read GetIsAdvanced write SetIsAdvanced;
    property Tree: TGEDCOMTree read FTree write FTree;

    //////////////////////

    class function SexStr(Sex: TGEDCOMObject.TGEDCOMSex): string;

    class function GetSexBySign(const SexSign: Char): TGEDCOMObject.TGEDCOMSex;

    class function IsDevComp(): Boolean;

    class function IsRecordAccess(aRecRestriction: TGEDCOMObject.TGEDCOMRestriction; aShieldState: TShieldState): Boolean;

    class function GetPersonEventKindBySign(aSign: string): TPersonEventKind;

    class function GetPersonEventIndex(aSign: string): Integer;
    class function GetFamilyEventIndex(aSign: string): Integer;

    class function GetMarriageStatusIndex(aSign: string): Integer;

    class function GetEventName(aEvent: TGEDCOMCustomEvent): string;

    class procedure GetNameParts(iRec: TGEDCOMIndividualRecord; var aFamily, aName, aPatronymic: string);

    class function GetNameStr(iRec: TGEDCOMIndividualRecord; aByFamily: Boolean = True;
      aPieces: Boolean = False): string;
    class function GetNickStr(iRec: TGEDCOMIndividualRecord): string;
    class function GetFamilyStr(aFamily: TGEDCOMFamilyRecord): string;

    class function GetSex(f_name, f_pat: string; aQuery: Boolean = True): TGEDCOMObject.TGEDCOMSex;

    class function GetXRefNum(aRecord: TGEDCOMRecord): string;
    class function GetId(aRecord: TGEDCOMRecord): Integer;

    class function GEDCOMDateToStr(aDate: TGEDCOMDate; aFormat: TDateFormat = dfDD_MM_YYYY): string;
    class function StrToGEDCOMDate(aDate: string; aException: Boolean = True): string;
    class function GEDCOMCustomDateToStr(aDate: TGEDCOMCustomDate; aFormat: TDateFormat;
      aSign: Boolean = False): string;

    class function GEDCOMEventToDateStr(aEvent: TGEDCOMCustomEvent; aFormat: TDateFormat;
      aSign: Boolean = False): string;

    class function GEDCOMDateToDate(aDate: TGEDCOMCustomDate): DateTime;

    class function GetIndividualEvent(iRec: TGEDCOMIndividualRecord; evName: string): TGEDCOMCustomEvent;
    class function GetFamilyEvent(fRec: TGEDCOMFamilyRecord; evName: string): TGEDCOMFamilyEvent;

    class function CompactDate(const aDate: string): string;

    class function GetBirthDate(iRec: TGEDCOMIndividualRecord; aFormat: TDateFormat;
      aCompact: Boolean = False): string;
    class function GetDeathDate(iRec: TGEDCOMIndividualRecord; aFormat: TDateFormat;
      aCompact: Boolean = False): string;

    class function GetLifeStr(iRec: TGEDCOMIndividualRecord): string;

    class function GetBirthPlace(iRec: TGEDCOMIndividualRecord): string;
    class function GetDeathPlace(iRec: TGEDCOMIndividualRecord): string;

    class function GetResidencePlace(iRec: TGEDCOMIndividualRecord; IncludeAddress: Boolean): string;
    class function GetPlaceStr(aEvent: TGEDCOMCustomEvent; IncludeAddress: Boolean): string;

    class function GetAttributeValue(iRec: TGEDCOMIndividualRecord; attrName: string): string;
    class function GetAttributeStr(iAttr: TGEDCOMIndividualAttribute): string;

    class function GetMarriageDate(fRec: TGEDCOMFamilyRecord; aFormat: TDateFormat): string;
    class function GetEventDesc(evDetail: TGEDCOMEventDetail): string;
    class function GetEventCause(evDetail: TGEDCOMEventDetail): string;

    class procedure GetLifeDates(iRec: TGEDCOMIndividualRecord; var aBirthEvent, aDeathEvent: TGEDCOMCustomEvent);

    class procedure GetIndependentDate(aDate: TGEDCOMCustomDate; var AYear: Integer; var AMonth, ADay: Word);
    class function GetIndependentYear(iRec: TGEDCOMIndividualRecord; evSign: string): Integer;

    class function GetAbstractDate(aEventDetail: TGEDCOMEventDetail): Double;

    class function GetEventsYearsDiff(ev1, ev2: TGEDCOMCustomEvent; aCurEnd: Boolean = False): string;
    class function GetLifeExpectancy(iRec: TGEDCOMIndividualRecord): string;
    class function GetAge(iRec: TGEDCOMIndividualRecord; ToYear: Integer = -1): string;
    class function GetFirstbornAge(iRec: TGEDCOMIndividualRecord): Integer;
    class function GetMarriageAge(iRec: TGEDCOMIndividualRecord): Integer;
    class function GetDaysForBirth(iRec: TGEDCOMIndividualRecord): string;

    class function CreateEventEx(aTree: TGEDCOMTree; aRec: TGEDCOMRecord;
      evSign: string; evDate: string = ''; evPlace: string = ''): TGEDCOMCustomEvent;

    class function CreatePersonEx(aTree: TGEDCOMTree; aName, aPatronymic, aFamily: string;
      aSex: TGEDCOMObject.TGEDCOMSex; aBirthEvent: Boolean = False): TGEDCOMIndividualRecord;

    class function CreateFamilyEx(aTree: TGEDCOMTree): TGEDCOMFamilyRecord;

    class procedure SetAddressValue(anAddress: TGEDCOMAddress; aValue: string);

    class function CreateNote(aTree: TGEDCOMTree): TGEDCOMNoteRecord;
    class procedure BindRecordNote(aTree: TGEDCOMTree; aRecord: TGEDCOMRecord;
      aNoteRec: TGEDCOMNoteRecord);
    class procedure AddNoteText(aNoteRec: TGEDCOMNoteRecord; aText: string);
    class function CreateNoteEx(aTree: TGEDCOMTree; aText: TStrings;
      aRecord: TGEDCOMRecord = nil): TGEDCOMNoteRecord;

    class function CreateSource(aTree: TGEDCOMTree): TGEDCOMSourceRecord;
  
    class procedure BindRecordSource(aTree: TGEDCOMTree; aRecord: TGEDCOMRecord;
      aSrcRec: TGEDCOMSourceRecord; aPage: string; aQuality: Integer);

    class procedure BindSourceRepository(aTree: TGEDCOMTree; aSourceRecord: TGEDCOMSourceRecord;
      aRepRec: TGEDCOMRepositoryRecord);

    class procedure GetLocationLinks(aTree: TGEDCOMTree; aLocation: TGEDCOMLocationRecord;
      var aList: TStringList);
  
    class function IsMatchesMask(const aName, aMask: string): Boolean;

    class function HyperLink(XRef: string; Text: string; Num: Integer = 0): string;
    class function GenRecordLink(aTree: TGEDCOMTree; aRecord: TGEDCOMRecord;
      aSigned: Boolean = True): string;

    class function GetCorresponderStr(aTree: TGEDCOMTree; aRec: TGEDCOMCommunicationRecord;
      aLink: Boolean): string;

    class procedure GetTaskGoal(aTree: TGEDCOMTree; aRec: TGEDCOMTaskRecord;
      var aType: TGoalType; var aGoalRec: TGEDCOMRecord);
    class function GetTaskGoalStr(aTree: TGEDCOMTree; aRec: TGEDCOMTaskRecord): string;

    class function IndistinctMatching(MaxMatching: Integer; strInputMatching, strInputStandart: String): Integer;

    class function ClearFamily(aFamily: string): string;

    class function PrepareRusFamily(f: string; aFemale: Boolean): string;

    class function GetChildsCount(aPerson: TGEDCOMIndividualRecord): Integer;

    class procedure InitExtCounts(aTree: TGEDCOMTree; aValue: Integer = -1);
    class function GetAncestorsCount(aPerson: TGEDCOMIndividualRecord): Integer;
    class function GetDescendantsCount(aPerson: TGEDCOMIndividualRecord): Integer;

    class function GetDescGenerations(aPerson: TGEDCOMIndividualRecord): Integer;
    class function GetMarriagesCount(aPerson: TGEDCOMIndividualRecord): Integer;
    class function GetSpousesDiff(fRec: TGEDCOMFamilyRecord): Integer;

    class procedure CheckRecord(aTree: TGEDCOMTree; aRec: TGEDCOMRecord; aFormat: TGEDCOMFormat);

    class function GetGEDCOMFormat(aTree: TGEDCOMTree): TGEDCOMFormat;
    class function CheckGEDCOMFormat(aTree: TGEDCOMTree): Boolean;

    class procedure TreeWalk(iRec: TGEDCOMIndividualRecord; aMode: TTreeWalkMode; aList: TList);
    class procedure TreeMerge(aMainTree: TGEDCOMTree; aFileName: string; aLog: TextBox);
    class procedure TreeSync(aMainTree: TGEDCOMTree; aFileName: string; aLog: TextBox);
  end;

implementation

{ TValsItem }

constructor TGenEngine.TValsItem.Create(aCaption: string; aValue: Integer);
begin
  inherited Create;
  Caption := aCaption;
  Value := aValue;
end;

function TGenEngine.TValsItem.ToString: string;
begin
  Result := Caption;
end;

{ TGenEngine }

constructor TGenEngine.Create;
begin
  inherited Create;
  FTree := TGEDCOMTree.Create;

  InitKinships();
end;

destructor TGenEngine.Destroy;
begin
  FTree.Destroy;
  inherited Destroy;
end;

procedure TGenEngine.RegisterKinship(aPrevRels: TEnumSet; aCurrRels: TEnumSet;
  aFinRel: TRelationKind; aGreat, aLevel: Shortint);
var
  len: Integer;
begin
  len := Length(Kinships);
  SetLength(Kinships, len + 1);
  Kinships[len].PrevRels := aPrevRels;
  Kinships[len].CurrRels := aCurrRels;
  Kinships[len].FinRel := aFinRel;
  Kinships[len].Great := aGreat;
  Kinships[len].Level := aLevel;
end;

function TGenEngine.FindKinship(prev, cur: TRelationKind; var great, level: Integer): TRelationKind;
var
  i: Integer;
  rel: TGenEngine.TRelationKind;
begin
  Result := rkUndefined;
  great := 0;
  level := 0;

  for i := 0 to Length(Kinships) - 1 do
    if (Kinships[i].PrevRels.InSet(prev))
    and (Kinships[i].CurrRels.InSet(cur))
    then begin
      rel := Kinships[i].FinRel;
      great := Kinships[i].Great;
      level := Kinships[i].Level;
      if (rel = rkSame) then rel := cur;
      Result := rel;
    end;
end;

procedure TGenEngine.InitKinships();
begin
  SetLength(Kinships, 0);

  RegisterKinship(
    TEnumSet.Create([rkNone]),
    TEnumSet.Create([rkFather, rkMother, rkHusband, rkWife, rkSon, rkDaughter]),
    rkSame, 0, 0);

  RegisterKinship(
    TEnumSet.Create([rkHusband, rkWife]),
    TEnumSet.Create([rkSon, rkDaughter]), rkSame, 0, +1);

  RegisterKinship(
    TEnumSet.Create([rkMother]),
    TEnumSet.Create([rkHusband]), rkFather, 0, 0);
  RegisterKinship(
    TEnumSet.Create([rkFather]),
    TEnumSet.Create([rkWife]), rkMother, 0, 0);

  RegisterKinship(
    TEnumSet.Create([rkGrandfather, rkGrandmother]),
    TEnumSet.Create([rkSon]), rkUncle, 0, +1);
  RegisterKinship(
    TEnumSet.Create([rkGrandfather, rkGrandmother]),
    TEnumSet.Create([rkDaughter]), rkAunt, 0, +1);

    //    (TEnumSet.Create([rkUncle, rkAunt], TEnumSet.Create([rkSon]; rkCousinM, 0, +1),
    //    (TEnumSet.Create([rkUncle, rkAunt], TEnumSet.Create([rkDaughter]; rkCousinF, 0, +1),

  RegisterKinship(
    TEnumSet.Create([rkBrother, rkSister]),
    TEnumSet.Create([rkSon]), rkNephew, 0, +1);
  RegisterKinship(
    TEnumSet.Create([rkBrother, rkSister]),
    TEnumSet.Create([rkDaughter]), rkNiece, 0, +1);

  RegisterKinship(
    TEnumSet.Create([rkSon]),
    TEnumSet.Create([rkWife]), rkDaughterInLaw, 0, 0);
  RegisterKinship(
    TEnumSet.Create([rkDaughter]),
    TEnumSet.Create([rkHusband]), rkSonInLaw, 0, 0);

  RegisterKinship(
    TEnumSet.Create([rkWife]),
    TEnumSet.Create([rkFather]), rkWifeFather, 0, -1);
  RegisterKinship(
    TEnumSet.Create([rkWife]),
    TEnumSet.Create([rkMother]), rkWifeMother, 0, -1);

  RegisterKinship(
    TEnumSet.Create([rkHusband]),
    TEnumSet.Create([rkFather]), rkHusbandFather, 0, -1);
  RegisterKinship(
    TEnumSet.Create([rkHusband]),
    TEnumSet.Create([rkMother]), rkHusbandMother, 0, -1);

  RegisterKinship(
    TEnumSet.Create([rkFather, rkMother]),
    TEnumSet.Create([rkFather]), rkGrandfather, 0, -1);
  RegisterKinship(
    TEnumSet.Create([rkFather, rkMother]),
    TEnumSet.Create([rkMother]), rkGrandmother, 0, -1);

  RegisterKinship(
    TEnumSet.Create([rkFather, rkMother]),
    TEnumSet.Create([rkSon]), rkBrother, 0, +1);
  RegisterKinship(
    TEnumSet.Create([rkFather, rkMother]),
    TEnumSet.Create([rkDaughter]), rkSister, 0, +1);

  RegisterKinship(
    TEnumSet.Create([rkGrandfather, rkGrandmother]),
    TEnumSet.Create([rkFather]), rkGrandfather, +1, -1);
  RegisterKinship(
    TEnumSet.Create([rkGrandfather, rkGrandmother]),
    TEnumSet.Create([rkMother]), rkGrandmother, +1, -1);

  RegisterKinship(
    TEnumSet.Create([rkSon, rkDaughter, rkSonInLaw, rkDaughterInLaw]),
    TEnumSet.Create([rkSon]), rkGrandson, 0, +1);
  RegisterKinship(
    TEnumSet.Create([rkSon, rkDaughter, rkSonInLaw, rkDaughterInLaw]),
    TEnumSet.Create([rkDaughter]), rkGranddaughter, 0, +1);

  RegisterKinship(
    TEnumSet.Create([rkGrandson, rkGranddaughter]),
    TEnumSet.Create([rkSon]), rkGrandson, +1, +1);
  RegisterKinship(
    TEnumSet.Create([rkGrandson, rkGranddaughter]),
    TEnumSet.Create([rkDaughter]), rkGranddaughter, +1, +1);
end;

function TGenEngine.FindSource(aName: string): TGEDCOMSourceRecord;
var
  i: Integer;
  rec: TGEDCOMRecord;
begin
  Result := nil;

  for i := 0 to FTree.RecordsCount - 1 do begin
    rec := FTree.Records[i];

    if (rec is TGEDCOMSourceRecord) and (TGEDCOMSourceRecord(rec).FiledByEntry = aName)
    then begin
      Result := TGEDCOMSourceRecord(rec);
      Break;
    end;
  end;
end;

procedure TGenEngine.GetSourcesList(aSources: TStringList);
var
  i: Integer;
  rec: TGEDCOMRecord;
begin
  if (aSources = nil) then Exit;

  aSources.Clear();
  for i := 0 to FTree.RecordsCount - 1 do begin
    rec := FTree.Records[i];

    if (rec is TGEDCOMSourceRecord)
    then aSources.AddObject(TGEDCOMSourceRecord(rec).FiledByEntry, rec);
  end;
end;

function TGenEngine.GetSubmitter(): TGEDCOMSubmitterRecord;
var
  submitter: TGEDCOMSubmitterRecord;
begin
  submitter := TGEDCOMSubmitterRecord(FTree.Header.Submitter.Value);

  if (submitter = nil) then begin
    submitter := TGEDCOMSubmitterRecord.Create(FTree, FTree);
    submitter.InitNew();
    FTree.AddRecord(submitter);
    FTree.Header.SetTagStringValue('SUBM', '@'+submitter.XRef+'@');
  end;

  Result := submitter;
end;

function TGenEngine.GetIsAdvanced: Boolean;
begin
  Result := (FTree.Header.FindTag(AdvTag) <> nil);
end;

procedure TGenEngine.SetIsAdvanced(const Value: Boolean);
begin
  if (Value) then begin
    if (FTree.Header.FindTag(AdvTag) = nil)
    then FTree.Header.AddTag(AdvTag);
  end else begin
    FTree.Header.DeleteTag(AdvTag);
  end;
end;

function TGenEngine.GetExtName: string;
var
  tag: TGEDCOMTag;
begin
  tag := FTree.Header.FindTag(ExtTag);
  if (tag = nil)
  then Result := ''
  else Result := tag.StringValue;
end;

procedure TGenEngine.SetExtName(const Value: string);
var
  tag: TGEDCOMTag;
begin
  if (Value <> '') then begin
    tag := FTree.Header.FindTag(ExtTag);
    if (tag = nil) then tag := FTree.Header.AddTag(ExtTag);
    tag.StringValue := Value;
  end else begin
    FTree.Header.DeleteTag(ExtTag);
  end;
end;

procedure TGenEngine.AddFamilySpouse(aFamily: TGEDCOMFamilyRecord; aSpouse: TGEDCOMIndividualRecord);
var
  spLink: TGEDCOMSpouseToFamilyLink;
begin
  case aSpouse.Sex of
    svNone, svUndetermined: Exit;

    svMale: aFamily.Husband.Value := aSpouse;
    svFemale: aFamily.Wife.Value := aSpouse;
  end;

  spLink := TGEDCOMSpouseToFamilyLink.Create(FTree, aSpouse);
  spLink.Family := aFamily;
  aSpouse.AddSpouseToFamilyLink(spLink);
end;

procedure TGenEngine.RemoveFamilySpouse(aFamily: TGEDCOMFamilyRecord; aSpouse: TGEDCOMIndividualRecord);
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

function TGenEngine.AddAssociation(aRec: TGEDCOMIndividualRecord; aRel: string;
  aRelPerson: TGEDCOMIndividualRecord): TGEDCOMAssociation;
begin
  Result := TGEDCOMAssociation.Create(FTree, aRec);
  Result.Relation := aRel;
  Result.Individual := aRelPerson;
  aRec.AddAssociation(Result);
end;

function TGenEngine.AddFamilyChild(aFamily: TGEDCOMFamilyRecord; aChild: TGEDCOMIndividualRecord): Boolean;
var
  chLink: TGEDCOMChildToFamilyLink;
  ptr: TGEDCOMPointer;
begin
  try
    ptr := TGEDCOMPointer.Create(FTree, aFamily);
    ptr.SetNamedValue('CHIL', aChild);
    aFamily.AddChild(ptr);

    chLink := TGEDCOMChildToFamilyLink.Create(FTree, aChild);
    chLink.Family := aFamily;
    aChild.AddChildToFamilyLink(chLink);

    Result := True;
  except
    Result := False;
  end;
end;

function TGenEngine.RemoveFamilyChild(aFamily: TGEDCOMFamilyRecord; aChild: TGEDCOMIndividualRecord): Boolean;
begin
  try
    aFamily.DeleteChild(aChild);
    aChild.DeleteChildToFamilyLink(aFamily);

    Result := True;
  except
    Result := False;
  end;
end;

function TGenEngine.AddResearchTask(aResearch: TGEDCOMResearchRecord;
  aTask: TGEDCOMTaskRecord): Boolean;
var
  ptr: TGEDCOMPointer;
begin
  Result := False;
  if (aResearch = nil) or (aTask = nil) then Exit;

  ptr := TGEDCOMPointer.Create(FTree, aResearch);
  ptr.SetNamedValue('_TASK', aTask);
  aResearch.AddTask(ptr);

  Result := True;
end;

procedure TGenEngine.RemoveResearchTask(aResearch: TGEDCOMResearchRecord;
  aTask: TGEDCOMTaskRecord);
begin
  aResearch.DeleteTask(aResearch.IndexOfTask(aTask));
end;

function TGenEngine.AddResearchGroup(aResearch: TGEDCOMResearchRecord;
  aGroup: TGEDCOMGroupRecord): Boolean;
var
  ptr: TGEDCOMPointer;
begin
  Result := False;
  if (aResearch = nil) or (aGroup = nil) then Exit;

  ptr := TGEDCOMPointer.Create(FTree, aResearch);
  ptr.SetNamedValue('_GROUP', aGroup);
  aResearch.AddGroup(ptr);

  Result := True;
end;

procedure TGenEngine.RemoveResearchGroup(aResearch: TGEDCOMResearchRecord;
  aGroup: TGEDCOMGroupRecord);
begin
  aResearch.DeleteGroup(aResearch.IndexOfGroup(aGroup));
end;

function TGenEngine.AddResearchComm(aResearch: TGEDCOMResearchRecord;
  aComm: TGEDCOMCommunicationRecord): Boolean;
var
  ptr: TGEDCOMPointer;
begin
  Result := False;
  if (aResearch = nil) or (aComm = nil) then Exit;

  ptr := TGEDCOMPointer.Create(FTree, aResearch);
  ptr.SetNamedValue('_COMM', aComm);
  aResearch.AddCommunication(ptr);

  Result := True;
end;

procedure TGenEngine.RemoveResearchComm(aResearch: TGEDCOMResearchRecord;
  aComm: TGEDCOMCommunicationRecord);
begin
  aResearch.DeleteCommunication(aResearch.IndexOfCommunication(aComm));
end;

function TGenEngine.AddGroupMember(aGroup: TGEDCOMGroupRecord; aMember: TGEDCOMIndividualRecord): Boolean;
var
  ptr: TGEDCOMPointer;
begin
  try
    ptr := TGEDCOMPointer.Create(FTree, aGroup);
    ptr.SetNamedValue('_MEMBER', aMember);
    aGroup.AddMember(ptr);

    ptr := TGEDCOMPointer.Create(FTree, aMember);
    ptr.SetNamedValue('_GROUP', aGroup);
    aMember.AddGroup(ptr);

    Result := True;
  except
    Result := False;
  end;
end;

function TGenEngine.RemoveGroupMember(aGroup: TGEDCOMGroupRecord; aMember: TGEDCOMIndividualRecord): Boolean;
begin
  try
    aGroup.DeleteMember(aGroup.IndexOfMember(aMember));
    aMember.DeleteGroup(aMember.IndexOfGroup(aGroup));

    Result := True;
  except
    Result := False;
  end;
end;

procedure TGenEngine.CleanFamily(aFamily: TGEDCOMFamilyRecord);
var
  i: Integer;
  child, spouse: TGEDCOMIndividualRecord;
begin
  if (aFamily = nil) then Exit;

  for i := 0 to aFamily.ChildrenCount - 1 do begin
    child := TGEDCOMIndividualRecord(aFamily.Children[i].Value);
    child.DeleteChildToFamilyLink(aFamily);
  end;

  spouse := TGEDCOMIndividualRecord(aFamily.Husband.Value);
  RemoveFamilySpouse(aFamily, spouse);

  spouse := TGEDCOMIndividualRecord(aFamily.Wife.Value);
  RemoveFamilySpouse(aFamily, spouse);
end;

procedure TGenEngine.GetCommonStats(var aStats: TCommonStats);

  // alert
  {procedure CalcVal(val: string; sex: TGEDCOMSex; var s_sum, s_cnt: Integer);
  begin
  end;}

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

    for i := 0 to FTree.RecordsCount - 1 do begin
      rec := FTree.Records[i];

      if (rec is TGEDCOMIndividualRecord) then begin
        ind := rec as TGEDCOMIndividualRecord;

        Inc(persons);
        if ind.IsLive() then Inc(lives);

        v_age := GetAge(ind);
        if (v_age <> '') and (v_age <> '?') then begin
          age := age + TGKUtils.StrToInt(v_age);
          Inc(age_cnt);
        end;

        v_life := GetLifeExpectancy(ind);
        if (v_life <> '') and (v_life <> '?') then begin
          life := life + TGKUtils.StrToInt(v_life);
          Inc(life_cnt);
        end;

        ch_cnt := GetChildsCount(ind);
        if (ch_cnt <> 0) then begin
          childs := childs + ch_cnt;
          Inc(childs_cnt);
        end;

        v_fba := GetFirstbornAge(ind).ToString();
        if (v_fba <> '') and (v_fba <> '?') and (v_fba <> '0') then begin
          fba := fba + TGKUtils.StrToInt(v_fba);
          Inc(fba_cnt);
        end;

        m_cnt := GetMarriagesCount(ind);
        if (m_cnt <> 0) then begin
          marr := marr + m_cnt;
          Inc(marr_cnt);
        end;

        v_mage := GetMarriageAge(ind).ToString();
        if (v_mage <> '') and (v_mage <> '?') and (v_mage <> '0') then begin
          mage := mage + TGKUtils.StrToInt(v_mage);
          Inc(mage_cnt);
        end;

        case ind.Sex of
          svMale: begin
            Inc(persons_m);
            if ind.IsLive() then Inc(lives_m);

            if (v_age <> '') and (v_age <> '?') then begin
              age_m := age_m + TGKUtils.StrToInt(v_age);
              Inc(age_m_cnt);
            end;

            if (v_life <> '') and (v_life <> '?') then begin
              life_m := life_m + TGKUtils.StrToInt(v_life);
              Inc(life_m_cnt);
            end;

            if (ch_cnt <> 0) then begin
              childs_m := childs_m + ch_cnt;
              Inc(childs_m_cnt);
            end;

            if (v_fba <> '') and (v_fba <> '?') and (v_fba <> '0') then begin
              fba_m := fba_m + TGKUtils.StrToInt(v_fba);
              Inc(fba_m_cnt);
            end;

            if (m_cnt <> 0) then begin
              marr_m := marr_m + m_cnt;
              Inc(marr_m_cnt);
            end;

            if (v_mage <> '') and (v_mage <> '?') and (v_mage <> '0') then begin
              mage_m := mage_m + TGKUtils.StrToInt(v_mage);
              Inc(mage_m_cnt);
            end;
          end;

          svFemale: begin
            Inc(persons_f);
            if ind.IsLive() then Inc(lives_f);

            if (v_age <> '') and (v_age <> '?') then begin
              age_f := age_f + TGKUtils.StrToInt(v_age);
              Inc(age_f_cnt);
            end;

            if (v_life <> '') and (v_life <> '?') then begin
              life_f := life_f + TGKUtils.StrToInt(v_life);
              Inc(life_f_cnt);
            end;

            if (ch_cnt <> 0) then begin
              childs_f := childs_f + ch_cnt;
              Inc(childs_f_cnt);
            end;

            if (v_fba <> '') and (v_fba <> '?') and (v_fba <> '0') then begin
              fba_f := fba_f + TGKUtils.StrToInt(v_fba);
              Inc(fba_f_cnt);
            end;

            if (m_cnt <> 0) then begin
              marr_f := marr_f + m_cnt;
              Inc(marr_f_cnt);
            end;

            if (v_mage <> '') and (v_mage <> '?') and (v_mage <> '0') then begin
              mage_f := mage_f + TGKUtils.StrToInt(v_mage);
              Inc(mage_f_cnt);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TGenEngine.GetSpecStats(aMode: TStatMode; aVals: System.Collections.Hashtable);
var
  i, k, year: Integer;
  m, d: Word;
  iRec: TGEDCOMIndividualRecord;
  event: TGEDCOMCustomEvent;
  fRec: TGEDCOMFamilyRecord;
  V, fam, nam, pat, iName: string;
begin
  if (aMode in [smAncestors, smDescendants])
  then InitExtCounts(FTree);

  try
    for i := 0 to FTree.RecordsCount - 1 do begin
      if (FTree.Records[i] is TGEDCOMIndividualRecord) and (aMode <> smSpousesDiff) then begin
        iRec := FTree.Records[i] as TGEDCOMIndividualRecord;
        iName := GetNameStr(iRec);

        case aMode of
          smAncestors: aVals.Add(iName, System.Object(GetAncestorsCount(iRec) - 1));

          smDescendants: aVals.Add(iName, System.Object(GetDescendantsCount(iRec) - 1));

          smDescGenerations: aVals.Add(iName, System.Object(GetDescGenerations(iRec)));

          smChildsCount: aVals.Add(iName, System.Object(GetChildsCount(iRec)));

          smFirstbornAge: aVals.Add(iName, System.Object(GetFirstbornAge(iRec)));

          smMarriages: aVals.Add(iName, System.Object(GetMarriagesCount(iRec)));

          smMarriageAge: aVals.Add(iName, System.Object(GetMarriageAge(iRec)));

          else begin
            case aMode of
              smNames, smFamilies, smPatronymics: begin
                GetNameParts(iRec, fam, nam, pat);

                case aMode of
                  smFamilies: V := PrepareRusFamily(fam, (iRec.Sex = svFemale));
                  smNames: V := nam;
                  smPatronymics: V := pat;
                end;
              end;

              smAge: V := GetAge(iRec);

              smLifeExpectancy: V := GetLifeExpectancy(iRec);

              smResidences: V := GetResidencePlace(iRec, False);

              smOccupation: V := GetAttributeValue(iRec, 'OCCU');

              smReligious: V := GetAttributeValue(iRec, 'RELI');

              smNational: V := GetAttributeValue(iRec, 'NATI');

              smEducation: V := GetAttributeValue(iRec, 'EDUC');

              smCaste: V := GetAttributeValue(iRec, 'CAST');

              smChildsDistribution: V := GetChildsCount(iRec).ToString;

              smBirthYears..smDeathTenYears, smBirthPlaces, smDeathPlaces: begin
                V := '?';
                for k := 0 to iRec.IndividualEventsCount - 1 do begin
                  event := iRec.IndividualEvents[k];
                  GetIndependentDate(event.Detail.Date.Value, year, m, d);

                  if (Abs(year) > 3000)
                  then TGKUtils.ShowMessage(event.Detail.Date.StringValue + '/' + iName);

                  if (event.Name = 'BIRT') then begin
                    if (aMode = smBirthYears)
                    then V := System.Convert.ToString(year)
                    else
                    if (aMode = smBirthTenYears)
                    then V := System.Convert.ToString((year div 10) * 10)
                    else
                    if (aMode = smBirthPlaces)
                    then V := event.Detail.Place.StringValue;
                  end
                  else
                  if (event.Name = 'DEAT') then begin
                    if (aMode = smDeathYears)
                    then V := System.Convert.ToString(year)
                    else
                    if (aMode = smDeathTenYears)
                    then V := System.Convert.ToString((year div 10) * 10)
                    else
                    if (aMode = smDeathPlaces)
                    then V := event.Detail.Place.StringValue;
                  end;
                end;
              end;

              smHobby: V := GetAttributeValue(iRec, '_HOBBY');

              smAward: V := GetAttributeValue(iRec, '_AWARD');

              smMili: V := GetAttributeValue(iRec, '_MILI');

              smMiliInd: V := GetAttributeValue(iRec, '_MILI_IND');

              smMiliDis: V := GetAttributeValue(iRec, '_MILI_DIS');

              smMiliRank: V := GetAttributeValue(iRec, '_MILI_RANK');
            end;

            if (V = '-1') or (V = '') or (V = '0') then V := '?';

            if not aVals.ContainsKey(V)
            then aVals.Add(V, System.Object(1))
            else aVals[V] := System.Object(Int32.Parse(aVals[V].ToString) + 1);
          end;
        end;
      end
      else
      if (FTree.Records[i] is TGEDCOMFamilyRecord) and (aMode = smSpousesDiff) then begin
        fRec := FTree.Records[i] as TGEDCOMFamilyRecord;
        aVals.Add(GetFamilyStr(fRec), System.Object(GetSpousesDiff(fRec)));
      end;
    end;
  finally
  end;
end;

function TGenEngine.PatriarchsCompare(Item1, Item2: TObject): Integer;
begin
  Result := TPatriarchObj(Item1).IBirthYear - TPatriarchObj(Item2).IBirthYear;
end;

function TGenEngine.GetPatriarchLinks(lst: TObjectList; pObj: TPatriarchObj): string;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to lst.Count - 1 do begin
    if (i in pObj.ILinks) then begin
      if (Result <> '') then Result := Result + ', ';
      Result := Result + GetNameStr(TPatriarchObj(lst[i]).IRec);
    end;
  end;
end;

procedure TGenEngine.GetPatriarchsList(aProgress, aLinks: Boolean;
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

  function GetBirthYear(iRec: TGEDCOMIndividualRecord): Integer;
  var
    family: TGEDCOMFamilyRecord;
    child: TGEDCOMIndividualRecord;
    i, k, year: Integer;
  begin
    Result := -1;
    if (iRec = nil) then Exit;

    year := GetIndependentYear(iRec, 'BIRT');
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
  if (aProgress)
  then TfmProgress.ProgressInit(FTree.RecordsCount, LSList[LSID_PatSearch]);

  InitExtCounts(FTree);
  try
    for i := 0 to FTree.RecordsCount - 1 do begin
      rec := FTree.Records[i];

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
          pObj.IDescendantsCount := GetDescendantsCount(i_rec) - 1;
          pObj.IDescGenerations := descGens;
          pObj.ILinks := [];
          aList.Add(pObj);
        end;
      end;

      if (aProgress) then TfmProgress.ProgressStep();
    end;

    aList.Sort(PatriarchsCompare);
  finally
    if (aProgress) then TfmProgress.ProgressDone();
  end;

  if (aLinks) then begin
    if (aProgress)
    then TfmProgress.ProgressInit(aList.Count, LSList[LSID_LinksSearch]);

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

        if (aProgress) then TfmProgress.ProgressStep();
      end;
    finally
      if (aProgress) then TfmProgress.ProgressDone();
    end;
  end;
end;

function TGenEngine.GetSpecExtName(): string;
var
  p: Integer;
  ext: string;
begin
  ext := ExtName;

  if (ext = '') then begin
    Result := System.IO.Path.GetFileName(FFileName);

    p := Pos('.ged', Result);
    if (p > 0) then Result := Copy(Result, 1, p - 1);
  end else Result := ext;
end;

function TGenEngine.GetArcFileName(): string;
begin
  Result := ExtractFilePath(FFileName) + GetSpecExtName() + '.zip';
end;

function TGenEngine.GetStoreFolder(): string;
begin
  Result := ExtractFilePath(FFileName) + GetSpecExtName() + '\';

  if not(System.IO.Directory.Exists(Result))
  then CreateDir(Result);
end;

{fixme!!!}
function TGenEngine.CheckPath(): Boolean;
var
  path: string;
begin
  path := ExtractFilePath(FFileName);
  Result := (path <> '');

  if not(Result)
  then TGKUtils.ShowError('Для типов хранения "архив" и "хранилище" новый файл БД нужно предварительно сохранить');
end;

function TGenEngine.GetStoreType(aFileRef: string; var aFileName: string): TGKStoreType;
begin
  aFileName := aFileRef;

  if (Pos(GKStoreType[gstArchive].Sign, aFileRef) > 0) then begin
    Result := gstArchive;
    StrDelete(aFileName, 1, 4);
  end
  else
  if (Pos(GKStoreType[gstStorage].Sign, aFileRef) > 0) then begin
    Result := gstStorage;
    StrDelete(aFileName, 1, 4);
  end
  else begin
    Result := gstReference;
  end;
end;

procedure TGenEngine.ArcFileLoad(target_fn: string; toStream: TStream);
{$IFNDEF DELPHI_NET}
var
  az: TAbZipKit;
{$ENDIF}
begin
  {$IFNDEF DELPHI_NET}
  AnsiToOem(PChar(target_fn), PChar(target_fn));

  az := TAbZipKit.Create(nil);
  try
    az.OpenArchive(GetArcFileName());
    az.ExtractToStream(target_fn, toStream);
  finally
    az.Destroy;
  end;
  {$ENDIF}
end;

procedure TGenEngine.MediaLoad(aRefName: string; var aStream: TStream);
var
  gst: TGKStoreType;
  target_fn: string;
begin
  gst := GetStoreType(aRefName, target_fn);

  case gst of
    gstReference: begin
      aStream := TFileStream.Create(target_fn, fmOpenRead);
    end;

    gstArchive: begin
      aStream := TMemoryStream.Create();

      if not(System.IO.File.Exists(GetArcFileName()))
      then TGKUtils.ShowError(LSList[LSID_ArcNotFound])
      else begin
        ArcFileLoad(target_fn, aStream);
        aStream.Seek(0, soBeginning);
      end;
    end;

    gstStorage: begin
      target_fn := GetStoreFolder() + target_fn;
      aStream := TFileStream.Create(target_fn, fmOpenRead);
    end;
  end;
end;

procedure TGenEngine.MediaLoad(aRefName: string; var aFileName: string);
var
  gst: TGKStoreType;
  target_fn: string;
  fs: TFileStream;
begin
  gst := GetStoreType(aRefName, target_fn);

  case gst of
    gstReference: begin
      aFileName := target_fn;
    end;

    gstArchive: begin
      aFileName := TGKUtils.GetTempDir() + System.IO.Path.GetFileName(target_fn);

      fs := TFileStream.Create(aFileName, fmCreate);
      try
        if not(System.IO.File.Exists(GetArcFileName()))
        then TGKUtils.ShowError(LSList[LSID_ArcNotFound])
        else begin
          target_fn := StringReplace(target_fn, '\', '/', [rfReplaceAll]);
          ArcFileLoad(target_fn, fs);
        end;
      finally
        fs.Destroy;
      end;
    end;

    gstStorage: begin
      aFileName := GetStoreFolder() + target_fn;
    end;
  end;
end;

procedure TGenEngine.ArcFileSave(aFileName, sfn: string);
{$IFNDEF DELPHI_NET}
var
  az: TAbZipper;
  arc_item: TAbArchiveItem;
  fs: TFileStream;
  idx, fdt: Longint;
{$ENDIF}
begin
  {$IFNDEF DELPHI_NET}
  fs := TFileStream.Create(aFileName, fmOpenRead);
  az := TAbZipper.Create(nil);
  try
    AnsiToOem(PChar(sfn), PChar(sfn));

    az.FileName := GetArcFileName();
    az.CompressionMethodToUse := smDeflated;
    az.AddFromStream(sfn, fs);

    sfn := StringReplace(sfn, '\', '/', [rfReplaceAll]);
    idx := az.FindFile(sfn);
    if (idx >= 0) then begin
      arc_item := az.Items[idx];
      fdt := FileGetDate(fs.Handle);
      arc_item.LastModFileDate := LongRec(fdt).Hi;
      arc_item.LastModFileTime := LongRec(fdt).Lo;
    end;

    az.Save();
  finally
    az.Destroy;
    fs.Destroy;
  end;
  {$ENDIF}
end;

procedure TGenEngine.MediaSave(aFileName: string; aStoreType: TGKStoreType; var aRefPath: string);
var
  target_fn, sfn, spath: string;
  mmFormat: TGEDCOMFileReference.TGEDCOMMultimediaFormat;
begin
  sfn := System.IO.Path.GetFileName(aFileName);

  mmFormat := TGEDCOMFileReference.RecognizeFormat(aFileName);

  case mmFormat of
    mfNone, mfUnknown, mfOLE: spath := 'unknown\';
    mfBMP, mfGIF, mfJPG, mfPCX, mfTIF, mfTGA, mfPNG: spath := 'images\';
    mfWAV: spath := 'audio\';
    mfTXT, mfRTF, mfHTM: spath := 'texts\';
    mfAVI, mfMPG: spath := 'video\';
  end;

  case aStoreType of
    gstReference: begin
      aRefPath := aFileName;
    end;

    gstArchive: begin
      sfn := spath + sfn;
      aRefPath := GKStoreType[aStoreType].Sign + sfn;

      ArcFileSave(aFileName, sfn);
    end;

    gstStorage: begin
      sfn := spath + sfn;
      aRefPath := GKStoreType[aStoreType].Sign + sfn;
      target_fn := GetStoreFolder() + sfn;

      System.IO.File.Move(aFileName, target_fn);
    end;
  end;
end;

function TGenEngine.GetPrimaryMultimediaLink(aRec: TGEDCOMIndividualRecord): TGEDCOMMultimediaLink;
var
  mmLink: TGEDCOMMultimediaLink;
  i: Integer;
begin
  for i := 0 to aRec.MultimediaLinksCount - 1 do begin
    mmLink := aRec.MultimediaLinks[i];

    if (mmLink.IsPrimary) then begin
      Result := mmLink;
      Exit;
    end;
  end;

  Result := nil;
end;

function TGenEngine.SetPrimaryMultimediaRecord(aRec: TGEDCOMIndividualRecord;
  mmRec: TGEDCOMMultimediaRecord): TGEDCOMMultimediaLink;
var
  mmLink: TGEDCOMMultimediaLink;
  i: Integer;
begin
  mmLink := nil;
  for i := 0 to aRec.MultimediaLinksCount - 1 do begin
    if (aRec.MultimediaLinks[i].Value = mmRec) then begin
      mmLink := aRec.MultimediaLinks[i];
      Break;
    end;
  end;

  if (mmLink = nil) then begin
    mmLink := TGEDCOMMultimediaLink.Create(FTree, aRec);
    mmLink.Value := mmRec;
    aRec.AddMultimediaLink(mmLink);
  end;

  mmLink.IsPrimary := True;
  Result := mmLink;
end;

function TGenEngine.GetPrimaryBitmap(aRec: TGEDCOMIndividualRecord): System.Drawing.Bitmap;
var
  mmLink: TGEDCOMMultimediaLink;
  mmRec: TGEDCOMMultimediaRecord;
  target_fn: string;
begin
  Result := nil;
  mmLink := GetPrimaryMultimediaLink(aRec);

  if (mmLink <> nil) then begin
    mmRec := TGEDCOMMultimediaRecord(mmLink.Value);
    MediaLoad(mmRec.FileReferences[0].StringValue, target_fn);

    if System.IO.File.Exists(target_fn)
    then Result := Bitmap.Create(target_fn);
  end;
end;

procedure TGenEngine.SortFamilyChilds(aFamily: TGEDCOMFamilyRecord);
var
  i, k: Integer;
  iChild, kChild: TGEDCOMIndividualRecord;
  iEv, kEv: TGEDCOMCustomEvent;
  iDate, kDate: DateTime;
begin
  for i := 0 to aFamily.ChildrenCount - 1 do begin
    for k := i + 1 to aFamily.ChildrenCount - 1 do begin
      iChild := TGEDCOMIndividualRecord(aFamily.Children[i].Value);
      iEv := GetIndividualEvent(iChild, 'BIRT');
      if (iEv <> nil)
      then iDate := GEDCOMDateToDate(iEv.Detail.Date.Value)
      else iDate := DateTime.Create(0);

      kChild := TGEDCOMIndividualRecord(aFamily.Children[k].Value);
      kEv := GetIndividualEvent(kChild, 'BIRT');
      if (kEv <> nil)
      then kDate := GEDCOMDateToDate(kEv.Detail.Date.Value)
      else kDate := DateTime.Create(0);

      if (iDate > kDate)
      then aFamily.ChildrenExchange(i, k);
    end;
  end;
end;

class function TGenEngine.SexStr(Sex: TGEDCOMObject.TGEDCOMSex): string;
begin
  Result := LSList[SexData[Sex].NameId];
end;

class function TGenEngine.GetSexBySign(const SexSign: Char): TGEDCOMObject.TGEDCOMSex;
begin
  case SexSign of
    'N': Result := svNone;
    'M': Result := svMale;
    'F': Result := svFemale;
    'U': Result := svUndetermined;
    else Result := svNone;
  end;
end;

class function TGenEngine.IsDevComp(): Boolean;
begin
  Result := (Environment.MachineName = 'VALHALLA')
         or (Environment.UserName = 'Zhdanovskih_SV');
end;

class function TGenEngine.IsRecordAccess(aRecRestriction: TGEDCOMObject.TGEDCOMRestriction; aShieldState: TShieldState): Boolean;
begin
  Result := False;

  case aShieldState of
    ssMaximum: Result := not(aRecRestriction in [rnConfidential, rnPrivacy]);

    ssMiddle: Result := not(aRecRestriction in [rnPrivacy]);

    ssNone: Result := True;
  end;
end;

class function TGenEngine.GetPersonEventKindBySign(aSign: string): TPersonEventKind;
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

class function TGenEngine.GetPersonEventIndex(aSign: string): Integer;
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

class function TGenEngine.GetFamilyEventIndex(aSign: string): Integer;
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

class function TGenEngine.GetMarriageStatusIndex(aSign: string): Integer;
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

class function TGenEngine.GetEventName(aEvent: TGEDCOMCustomEvent): string;
var
  ev: Integer;
begin
  if (aEvent is TGEDCOMIndividualEvent) or (aEvent is TGEDCOMIndividualAttribute) then begin
    ev := GetPersonEventIndex(aEvent.Name);
    if (ev = 0) then Result := aEvent.Detail.Classification
    else
    if (ev > 0) then Result := LSList[PersonEvents[ev].Name]
    else Result := aEvent.Name;
  end
  else
  if (aEvent is TGEDCOMFamilyEvent) then begin
    ev := GetFamilyEventIndex(aEvent.Name);
    if (ev = 0) then Result := aEvent.Detail.Classification
    else
    if (ev > 0) then Result := LSList[FamilyEvents[ev].Name]
    else Result := aEvent.Name;
  end
  else Result := '';
end;

class procedure TGenEngine.GetNameParts(iRec: TGEDCOMIndividualRecord; var aFamily, aName, aPatronymic: string);
var
  np: TGEDCOMPersonalName;
begin
  if (iRec <> nil) and (iRec.PersonalNamesCount > 0) then begin
    np := iRec.PersonalNames[0];
    aFamily := np.Surname;
    if (TGKUtils.GetTokensCount(np.FirstPart, ' ') > 1) then begin
      aName := TGKUtils.GetToken(np.FirstPart, ' ', 1);
      aPatronymic := TGKUtils.GetToken(np.FirstPart, ' ', 2);
    end else begin
      aName := np.FirstPart;
      aPatronymic := '';
    end;
  end else begin
    aFamily := '';
    aName := '';
    aPatronymic := '';
  end;
end;

class function TGenEngine.GetNameStr(iRec: TGEDCOMIndividualRecord; aByFamily: Boolean = True;
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
  if (iRec <> nil) and (iRec.PersonalNamesCount > 0) then begin
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

class function TGenEngine.GetNickStr(iRec: TGEDCOMIndividualRecord): string;
var
  np: TGEDCOMPersonalName;
begin
  if (iRec <> nil) then begin
    np := iRec.PersonalNames[0];
    Result := np.Pieces.Nickname;
  end else Result := '';
end;

class function TGenEngine.GetFamilyStr(aFamily: TGEDCOMFamilyRecord): string;
var
  spouse: TGEDCOMIndividualRecord;
begin
  Result := '';

  spouse := TGEDCOMIndividualRecord(aFamily.Husband.Value);
  if (spouse = nil)
  then Result := Result + LSList[LSID_UnkMale]
  else Result := Result + GetNameStr(spouse);

  Result := Result + ' - ';

  spouse := TGEDCOMIndividualRecord(aFamily.Wife.Value);
  if (spouse = nil)
  then Result := Result + LSList[LSID_UnkFemale]
  else Result := Result + GetNameStr(spouse);
end;

{fixme!!!}
class function TGenEngine.GetSex(f_name, f_pat: string; aQuery: Boolean = True): TGEDCOMObject.TGEDCOMSex;
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
    if (TGKUtils.ShowQuestion('Не определяется пол человека по имени "'+f_name+' '+f_pat+'". Это мужской пол?') = System.Windows.Forms.DialogResult.Yes)
    then Result := svMale
    else Result := svFemale;
  end;
end;

class function TGenEngine.GetXRefNum(aRecord: TGEDCOMRecord): string;
var
  xref: string;
begin
  xref := aRecord.XRef;
  while not(xref[1] in ['0'..'9']) do StrDelete(xref, 1, 1);
  Result := xref;
end;

class function TGenEngine.GetId(aRecord: TGEDCOMRecord): Integer;
var
  xref: string;
begin
  try
    xref := GetXRefNum(aRecord);
    Result := StrToIntDef(xref, 0);
  except
    Result := -1;
  end;
end;

class function TGenEngine.GEDCOMDateToStr(aDate: TGEDCOMDate; aFormat: TDateFormat = dfDD_MM_YYYY): string;
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
      then Result := Result + TGKUtils.NumUpdate(day, 2) + '.'
      else Result := Result + '__.';

      if (month > 0)
      then Result := Result + TGKUtils.NumUpdate(month, 2) + '.'
      else Result := Result + '__.';

      if (year > 0)
      then Result := Result + year.ToString()
      else Result := Result + '____';
    end;

    dfYYYY_MM_DD: begin
      if (year > 0)
      then Result := Result + year.ToString() + '.'
      else Result := Result + '____.';

      if (month > 0)
      then Result := Result + TGKUtils.NumUpdate(month, 2) + '.'
      else Result := Result + '__.';

      if (day > 0)
      then Result := Result + TGKUtils.NumUpdate(day, 2)
      else Result := Result + '__';
    end;

    dfYYYY: begin
      if (year > 0)
      then Result := year.ToString();
    end;
  end;
end;

class function TGenEngine.StrToGEDCOMDate(aDate: string; aException: Boolean = True): string;
var
  cnt: Integer;
  pd, pm, py: string;
begin
  Result := '';

  if (Pos('/', aDate) > 0)
  then aDate := StringReplace(aDate, '/', '.', [rfReplaceAll]);

  cnt := TGKUtils.GetTokensCount(aDate, '.');
  if (cnt < 3) then begin
    if (aException)
    then raise Exception.Create('date failed')
    else Exit;
  end;

  pd := TGKUtils.GetToken(aDate, '.', 1).Trim();
  pm := TGKUtils.GetToken(aDate, '.', 2).Trim();
  py := TGKUtils.GetToken(aDate, '.', 3).Trim();

  if (pd <> '') then Result := Result + pd + ' ';
  if (pm <> '') then Result := Result + TGEDCOMDate.GEDCOMMonthArray[TGKUtils.StrToInt(pm)] + ' ';
  if (py <> '') then Result := Result + py;
end;

class function TGenEngine.GEDCOMCustomDateToStr(aDate: TGEDCOMCustomDate; aFormat: TDateFormat;
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

class function TGenEngine.GEDCOMEventToDateStr(aEvent: TGEDCOMCustomEvent; aFormat: TDateFormat;
  aSign: Boolean = False): string;
begin
  if (aEvent = nil)
  then Result := ''
  else Result := GEDCOMCustomDateToStr(aEvent.Detail.Date.Value, aFormat, aSign);
end;

class function TGenEngine.GEDCOMDateToDate(aDate: TGEDCOMCustomDate): DateTime;
var
  year: Integer;
  month, day: Word;
begin
  try
    GetIndependentDate(aDate, year, month, day);

    if (day = 0) then day := 1;
    if (month = 0) then month := 1;

    if (year <= 0)
    then Result := DateTime.Create(0)
    else Result := DateTime.Create(year, month, day);
  except
    on E: Exception do begin
      TGKUtils.LogWrite(System.&String.Format('GEDCOMDateToDate(%d, %d, %d): ', [year, month, day]) + E.Message);
      TGKUtils.LogWrite('Record (' + TGEDCOMRecord(TGEDCOMTag(aDate).ParentRecord).XRef + '): invalid date');
      Result := DateTime.Create(0);
    end;
  end;
end;

class function TGenEngine.GetIndividualEvent(iRec: TGEDCOMIndividualRecord; evName: string): TGEDCOMCustomEvent;
begin
  if (iRec = nil)
  then Result := nil
  else Result := iRec.GetIndividualEvent(evName);
end;

class function TGenEngine.GetFamilyEvent(fRec: TGEDCOMFamilyRecord; evName: string): TGEDCOMFamilyEvent;
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

class function TGenEngine.CompactDate(const aDate: string): string;
begin
  Result := aDate;
  while (Pos('__.', Result) = 1) do StrDelete(Result, 1, 3);
end;

class function TGenEngine.GetBirthDate(iRec: TGEDCOMIndividualRecord; aFormat: TDateFormat;
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

class function TGenEngine.GetDeathDate(iRec: TGEDCOMIndividualRecord; aFormat: TDateFormat;
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

class function TGenEngine.GetLifeStr(iRec: TGEDCOMIndividualRecord): string;
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

class function TGenEngine.GetBirthPlace(iRec: TGEDCOMIndividualRecord): string;
var
  event: TGEDCOMCustomEvent;
begin
  event := GetIndividualEvent(iRec, 'BIRT');

  if (event = nil)
  then Result := ''
  else Result := event.Detail.Place.StringValue;
end;

class function TGenEngine.GetDeathPlace(iRec: TGEDCOMIndividualRecord): string;
var
  event: TGEDCOMCustomEvent;
begin
  event := GetIndividualEvent(iRec, 'DEAT');

  if (event = nil)
  then Result := ''
  else Result := event.Detail.Place.StringValue;
end;

class function TGenEngine.GetResidencePlace(iRec: TGEDCOMIndividualRecord; IncludeAddress: Boolean): string;
begin
  Result := GetPlaceStr(GetIndividualEvent(iRec, 'RESI'), IncludeAddress);
end;

class function TGenEngine.GetPlaceStr(aEvent: TGEDCOMCustomEvent; IncludeAddress: Boolean): string;
var
  resi, addr: string;
begin
  if (aEvent = nil)
  then Result := ''
  else begin
    Result := aEvent.Detail.Place.StringValue;

    if (IncludeAddress) then begin
      resi := aEvent.StringValue;
      addr := aEvent.Detail.Address.Address.Text.Trim();
      if (resi <> '') and (addr <> '') then resi := resi + ', ';
      resi := resi + addr;

      if (resi <> '')
      then Result := Result + ' [' + resi + ']';
    end;
  end;
end;

class function TGenEngine.GetAttributeValue(iRec: TGEDCOMIndividualRecord; attrName: string): string;
var
  attr: TGEDCOMCustomEvent;
begin
  attr := GetIndividualEvent(iRec, attrName);
  if (attr = nil)
  then Result := ''
  else Result := attr.StringValue;
end;

class function TGenEngine.GetAttributeStr(iAttr: TGEDCOMIndividualAttribute): string;
var
  idx: Integer;
  st, place: string;
begin
  idx := GetPersonEventIndex(iAttr.Name);
  if (idx = 0) then st := iAttr.Detail.Classification
  else
  if (idx > 0) then st := LSList[PersonEvents[idx].Name]
  else st := iAttr.Name;

  place := iAttr.Detail.Place.StringValue;
  if (place <> '') then place := ' [' + place + ']';

  Result := st + ': ' + iAttr.StringValue + place;
end;

class function TGenEngine.GetMarriageDate(fRec: TGEDCOMFamilyRecord; aFormat: TDateFormat): string;
var
  event: TGEDCOMFamilyEvent;
begin
  event := GetFamilyEvent(fRec, 'MARR');

  if (event = nil)
  then Result := ''
  else Result := GEDCOMCustomDateToStr(event.Detail.Date.Value, aFormat);
end;

class function TGenEngine.GetEventDesc(evDetail: TGEDCOMEventDetail): string;
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

class function TGenEngine.GetEventCause(evDetail: TGEDCOMEventDetail): string;
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

class procedure TGenEngine.GetLifeDates(iRec: TGEDCOMIndividualRecord; var aBirthEvent, aDeathEvent: TGEDCOMCustomEvent);
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

class procedure TGenEngine.GetIndependentDate(aDate: TGEDCOMCustomDate; var AYear: Integer; var AMonth, ADay: Word);
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

class function TGenEngine.GetIndependentYear(iRec: TGEDCOMIndividualRecord; evSign: string): Integer;
var
  ev: TGEDCOMCustomEvent;
  year: Integer;
  am, ad: Word;
begin
  ev := GetIndividualEvent(iRec, evSign);

  if (ev = nil)
  then Result := -1
  else begin
    GetIndependentDate(ev.Detail.Date.Value, year, am, ad);
    Result := year;
  end;
end;

class function TGenEngine.DaysInAMonth(const AYear, AMonth: Word): Word;
begin
  Result := MonthDays[(AMonth = 2) and DateTime.IsLeapYear(AYear), AMonth];
end;

class function TGenEngine.GetAbstractDate(aEventDetail: TGEDCOMEventDetail): Double;
var
  y: Integer;
  m, d: Word;
begin
  Result := 0.0;

  GetIndependentDate(aEventDetail.Date.Value, y, m, d);

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

class function TGenEngine.GetEventsYearsDiff(ev1, ev2: TGEDCOMCustomEvent; aCurEnd: Boolean = False): string;
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
      then y2 := DateTime.Now.Year + DateTime.Now.Month / 12;
    end;

    if (y1 = -1.0) or (y2 = -1.0)
    then Result := ''
    else
    if (y1 = 0.0) or (y2 = 0.0)
    then Result := '?'
    else
      Result := Trunc(y2 - y1).ToString();
  except
  end;
end;

class function TGenEngine.GetLifeExpectancy(iRec: TGEDCOMIndividualRecord): string;
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

class function TGenEngine.GetAge(iRec: TGEDCOMIndividualRecord; ToYear: Integer = -1): string;
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
    then Result := GetEventsYearsDiff(ev1, ev2, (ev2 = nil){True})
    else begin
      if (ev1 = nil)
      then Result := ''
      else begin
        GetIndependentDate(ev1.Detail.Date.Value, i, dummy, dummy);
        Result := System.Convert.ToString(ToYear - i);
      end;
    end;
  except
  end;
end;

class function TGenEngine.GetFirstbornAge(iRec: TGEDCOMIndividualRecord): Integer;
var
  y1, y2, y2tmp: Double;
  i, k: Integer;
  event: TGEDCOMCustomEvent;
  family: TGEDCOMFamilyRecord;
  child: TGEDCOMIndividualRecord;
begin
  Result := 0;
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
    then Result := Trunc(y2 - y1);
  except
  end;
end;

class function TGenEngine.GetMarriageAge(iRec: TGEDCOMIndividualRecord): Integer;
var
  y1, y2, y2tmp: Double;
  i: Integer;
  iEvent: TGEDCOMCustomEvent;
  fEvent: TGEDCOMFamilyEvent;
  family: TGEDCOMFamilyRecord;
begin
  Result := 0;
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
    then Result := Trunc(y2 - y1);
  except
  end;
end;

class function TGenEngine.GetDaysForBirth(iRec: TGEDCOMIndividualRecord): string;
var
  dt: TGEDCOMDate;
  dt1, dt2: Double;
  bd_y: Integer;
  cur_y, cur_m, cur_d, bd_m, bd_d: Word;
  event: TGEDCOMCustomEvent;
  xr: string;
  dtx: DateTime;
begin
  Result := '';
  try
    event := GetIndividualEvent(iRec, 'DEAT');
    if (event <> nil) then Exit;

    event := GetIndividualEvent(iRec, 'BIRT');
    if (event <> nil) then begin
      dt := TGEDCOMDate(event.Detail.Date.Value);
      if (dt <> nil) then begin
        dt.GetDate(bd_y, bd_m, bd_d);
        if (bd_m <= 0) or (bd_d <= 0) then Exit;

        dtx := DateTime.Now;
        cur_y := dtx.Year;
        cur_m := dtx.Month;
        cur_d := dtx.Day;

        dt1 := cur_y + (bd_m / 12) + (bd_d / 12 / 31);
        dt2 := cur_y + (cur_m / 12) + (cur_d / 12 / 31);

        if (dt1 < dt2)
        then bd_y := cur_y + 1
        else bd_y := cur_y;

        Result := System.Convert.ToString(
          TGKUtils.DaysBetween(
            DateTime.Create(cur_y, cur_m, cur_d),
            DateTime.Create(bd_y, bd_m, bd_d)));
      end else begin
        // Alert: выяснить почему нет даты в событии
      end;
    end;
  except
    xr := iRec.XRef;
    TGKUtils.Hole(xr);
  end;
end;

class function TGenEngine.CreateEventEx(aTree: TGEDCOMTree; aRec: TGEDCOMRecord;
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

class function TGenEngine.CreatePersonEx(aTree: TGEDCOMTree; aName, aPatronymic, aFamily: string;
  aSex: TGEDCOMObject.TGEDCOMSex; aBirthEvent: Boolean = False): TGEDCOMIndividualRecord;
var
  pn: TGEDCOMPersonalName;
begin
  Result := TGEDCOMIndividualRecord.Create(aTree, aTree);
  Result.InitNew();
  Result.Sex := aSex;

  pn := TGEDCOMPersonalName.Create(aTree, Result);
  pn.StringValue := aName.Trim() + ' ' + aPatronymic.Trim() + ' /' + aFamily.Trim() + '/';
  Result.AddPersonalName(pn);

  Result.ChangeDate.ChangeDateTime := DateTime.Now;

  aTree.AddRecord(Result);

  if (aBirthEvent)
  then CreateEventEx(aTree, Result, 'BIRT');
end;

class function TGenEngine.CreateFamilyEx(aTree: TGEDCOMTree): TGEDCOMFamilyRecord;
begin
  Result := TGEDCOMFamilyRecord.Create(aTree, aTree);
  Result.InitNew();
  Result.ChangeDate.ChangeDateTime := DateTime.Now;
  aTree.AddRecord(Result);
end;

{warning: this is hack wrapper}
class procedure TGenEngine.SetAddressValue(anAddress: TGEDCOMAddress; aValue: string);
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

class function TGenEngine.CreateNote(aTree: TGEDCOMTree): TGEDCOMNoteRecord;
begin
  Result := TGEDCOMNoteRecord.Create(aTree, aTree);
  Result.InitNew();
  Result.ChangeDate.ChangeDateTime := DateTime.Now;
  aTree.AddRecord(Result);
end;

class function TGenEngine.CreateNoteEx(aTree: TGEDCOMTree; aText: TStrings;
  aRecord: TGEDCOMRecord = nil): TGEDCOMNoteRecord;
begin
  Result := CreateNote(aTree);

  if (aText <> nil)
  then Result.Notes := aText;

  if (aRecord <> nil)
  then BindRecordNote(aTree, aRecord, Result);
end;

class function TGenEngine.CreateSource(aTree: TGEDCOMTree): TGEDCOMSourceRecord;
begin
  Result := TGEDCOMSourceRecord.Create(aTree, aTree);
  Result.InitNew();
  Result.ChangeDate.ChangeDateTime := DateTime.Now;
  aTree.AddRecord(Result);
end;

class procedure TGenEngine.BindRecordNote(aTree: TGEDCOMTree; aRecord: TGEDCOMRecord;
  aNoteRec: TGEDCOMNoteRecord);
var
  note: TGEDCOMNotes;
begin
  note := TGEDCOMNotes.Create(aTree, aRecord);
  note.Value := aNoteRec;
  aRecord.AddNotes(note);
end;

class procedure TGenEngine.AddNoteText(aNoteRec: TGEDCOMNoteRecord; aText: string);
var
  strData: TStringList;
begin
  strData := TStringList.Create;
  try
    strData.Text := aNoteRec.Notes.Text.Trim();
    strData.Add(aText);
    aNoteRec.Notes := strData;
  finally
    strData.Free;
  end;
end;

class procedure TGenEngine.BindRecordSource(aTree: TGEDCOMTree; aRecord: TGEDCOMRecord;
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

class procedure TGenEngine.BindSourceRepository(aTree: TGEDCOMTree; aSourceRecord: TGEDCOMSourceRecord;
  aRepRec: TGEDCOMRepositoryRecord);
var
  cit: TGEDCOMRepositoryCitation;
begin
  cit := TGEDCOMRepositoryCitation.Create(aTree, aSourceRecord);
  cit.Value := aRepRec;
  aSourceRecord.AddRepositoryCitation(cit);
end;

class procedure TGenEngine.GetLocationLinks(aTree: TGEDCOMTree; aLocation: TGEDCOMLocationRecord;
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
        then aList.Add(GenRecordLink(aTree, rec, True) + ', ' + GetEventName(event).ToLower());
      end;
    end
    else
    if (rec is TGEDCOMFamilyRecord) then begin
      f_rec := (rec as TGEDCOMFamilyRecord);

      for k := 0 to f_rec.FamilyEventCount - 1 do begin
        event := f_rec.FamilyEvents[k];

        if (event.Detail.Place.Location.Value = aLocation)
        then aList.Add(GenRecordLink(aTree, rec, True) + ', ' + GetEventName(event).ToLower());
      end;
    end;
  end;
end;

class function TGenEngine.IsMatchesMask(const aName, aMask: string): Boolean;
var
  i: Integer;
  strx: string;
  sts: array of string;
begin
  Result := False;
  if (aName = '') or (aMask = '') then Exit;

  strx := aName.ToLower();
  sts := aMask.ToLower().Split(['|']);
  for i := 0 to High(sts) do Result := Result or MatchesMask(strx, sts[i]);
end;

class function TGenEngine.HyperLink(XRef: string; Text: string; Num: Integer = 0): string;
begin
  Result := '~^' + XRef;
  if (Text <> '') then Result := Result + ':' + Text;
  Result := Result + '~';

  {if (Num <> 0)
  then Result := '№ ' + IntToStr(Num) + ' ' + Result;}
end;

class function TGenEngine.GenRecordLink(aTree: TGEDCOMTree; aRecord: TGEDCOMRecord;
  aSigned: Boolean = True): string;
var
  st, sign: string;
begin
  if (aRecord = nil) then begin
    Result := '';
    Exit;
  end;

  sign := '';
  if (aSigned) then begin
    case aRecord.RecordType of
      rtIndividual:
        sign := '';

      rtFamily, rtMultimedia, rtGroup, rtSource, rtRepository,
      rtResearch, rtTask, rtCommunication, rtLocation:
        sign := LSList[RecordTypes[aRecord.RecordType]]+': ';
    end;
  end;

  case aRecord.RecordType of
    rtIndividual: st := GetNameStr(TGEDCOMIndividualRecord(aRecord));
    rtFamily: st := GetFamilyStr(TGEDCOMFamilyRecord(aRecord));
    rtMultimedia: st := TGEDCOMMultimediaRecord(aRecord).FileReferences[0].Title;
    rtGroup: st := TGEDCOMGroupRecord(aRecord).Name;
    rtSource: st := TGEDCOMSourceRecord(aRecord).FiledByEntry;
    rtRepository: st := TGEDCOMRepositoryRecord(aRecord).RepositoryName;
    rtResearch: st := TGEDCOMResearchRecord(aRecord).Name;
    rtTask: st := GetTaskGoalStr(aTree, TGEDCOMTaskRecord(aRecord));
    rtCommunication: st := TGEDCOMCommunicationRecord(aRecord).Name;
    rtLocation: st := TGEDCOMLocationRecord(aRecord).Name;
    else st := aRecord.XRef;
  end;

  Result := HyperLink(aRecord.XRef, sign + st);
end;

class function TGenEngine.GetCorresponderStr(aTree: TGEDCOMTree; aRec: TGEDCOMCommunicationRecord;
  aLink: Boolean): string;
var
  dir: TGEDCOMCommunicationRecord.TCommunicationDir;
  corresponder: TGEDCOMIndividualRecord;
  nm: string;
begin
  Result := '';

  aRec.GetCorresponder(dir, corresponder);

  if (corresponder <> nil) then begin
    nm := GetNameStr(corresponder);
    if (aLink) then nm := HyperLink(corresponder.XRef, nm);

    Result := '[' + LSList[CommunicationDirs[dir]] + '] ' + nm;
  end;
end;

class procedure TGenEngine.GetTaskGoal(aTree: TGEDCOMTree; aRec: TGEDCOMTaskRecord;
  var aType: TGoalType; var aGoalRec: TGEDCOMRecord);
begin
  aGoalRec := aTree.XRefIndex_Find(TGEDCOMObject.CleanXRef(aRec.Goal));

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

class function TGenEngine.GetTaskGoalStr(aTree: TGEDCOMTree; aRec: TGEDCOMTaskRecord): string;
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
  then Result := '[' + LSList[GoalNames[gt]] + '] ' + Result;
end;

class function TGenEngine.Matching(StrA, StrB: String; lngLen: Integer): TRetCount;
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
      if System.String.Compare(StrTempA, StrTempB, True) = 0 then begin
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
class function TGenEngine.IndistinctMatching(MaxMatching: Integer; strInputMatching, strInputStandart: String): Integer;
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

class function TGenEngine.ClearFamily(aFamily: string): string;
var
  p: Integer;
begin
  p := Pos(' (', aFamily);
  if (p > 0)
  then Result := Copy(aFamily, 1, p - 1)
  else Result := aFamily;
end;

class function TGenEngine.PrepareRusFamily(f: string; aFemale: Boolean): string;
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

class function TGenEngine.GetChildsCount(aPerson: TGEDCOMIndividualRecord): Integer;
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

class procedure TGenEngine.InitExtCounts(aTree: TGEDCOMTree; aValue: Integer = -1);
var
  i: Integer;
  rec: TGEDCOMRecord;
begin
  for i := 0 to aTree.RecordsCount - 1 do begin
    rec := aTree.Records[i];

    if (rec is TGEDCOMIndividualRecord)
    then rec.ExtData := TObject(aValue);
  end;
end;

class function TGenEngine.GetAncestorsCount(aPerson: TGEDCOMIndividualRecord): Integer;
var
  family: TGEDCOMFamilyRecord;
  anc: TGEDCOMIndividualRecord;
  val: Integer;
begin
  Result := 0;
  if (aPerson = nil) then Exit;

  val := Integer(aPerson.ExtData);

  if (val < 0) then begin
    val := 1;

    if (aPerson.ChildToFamilyLinksCount > 0) then begin
      family := aPerson.ChildToFamilyLinks[0].Family;

      anc := TGEDCOMIndividualRecord(family.Husband.Value);
      val := val + GetAncestorsCount(anc);

      anc := TGEDCOMIndividualRecord(family.Wife.Value);
      val := val + GetAncestorsCount(anc);
    end;

    aPerson.ExtData := TObject(val);
  end;

  Result := val;
end;

class function TGenEngine.GetDescendantsCount(aPerson: TGEDCOMIndividualRecord): Integer;
var
  family: TGEDCOMFamilyRecord;
  iChild: TGEDCOMIndividualRecord;
  i, k, val: Integer;
begin
  Result := 0;
  if (aPerson = nil) then Exit;

  val := Integer(aPerson.ExtData);

  if (val < 0) then begin
    val := 1;

    for i := 0 to aPerson.SpouseToFamilyLinksCount - 1 do begin
      family := aPerson.SpouseToFamilyLinks[i].Family;

      for k := 0 to family.ChildrenCount - 1 do begin
        iChild := TGEDCOMIndividualRecord(family.Children[k].Value);
        val := val + GetDescendantsCount(iChild);
      end;
    end;

    aPerson.ExtData := TObject(val);
  end;

  Result := val;
end;

class function TGenEngine.GetDescGens_Recursive(aPerson: TGEDCOMIndividualRecord): Integer;
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

class function TGenEngine.GetDescGenerations(aPerson: TGEDCOMIndividualRecord): Integer;
begin
  Result := GetDescGens_Recursive(aPerson) - 1;
end;

class function TGenEngine.GetMarriagesCount(aPerson: TGEDCOMIndividualRecord): Integer;
begin
  if (aPerson = nil)
  then Result := 0
  else Result := aPerson.SpouseToFamilyLinksCount;
end;

class function TGenEngine.GetSpousesDiff(fRec: TGEDCOMFamilyRecord): Integer;
var
  y1, y2: Double;
  event: TGEDCOMCustomEvent;
  h, w: TGEDCOMIndividualRecord;
begin
  Result := 0;
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
    then Result := Trunc(Abs(y2 - y1));
  except
  end;
end;

class procedure TGenEngine.ReformNote(aTree: TGEDCOMTree; note: TGEDCOMNotes);
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

class procedure TGenEngine.ReformMultimediaLink(aTree: TGEDCOMTree; mmLink: TGEDCOMMultimediaLink);
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
class procedure TGenEngine.ReformSourceCitation(aTree: TGEDCOMTree; sourCit: TGEDCOMSourceCitation);
begin
end;

class procedure TGenEngine.CheckRecord(aTree: TGEDCOMTree; aRec: TGEDCOMRecord; aFormat: TGEDCOMFormat);

  procedure PrepareTag(tag: TGEDCOMTagWithLists);
  var
    i: Integer;
    mmLink: TGEDCOMMultimediaLink;
    note: TGEDCOMNotes;
    sourCit: TGEDCOMSourceCitation;
  begin
    for i := 0 to tag.MultimediaLinksCount - 1 do begin
      mmLink := tag.MultimediaLinks[i];
      if not(mmLink.IsPointer) then ReformMultimediaLink(aTree, mmLink);
    end;

    for i := 0 to tag.NotesCount - 1 do begin
      note := tag.Notes[i];
      if not(note.IsPointer) then ReformNote(aTree, note);
    end;

    for i := 0 to tag.SourceCitationsCount - 1 do begin
      sourCit := tag.SourceCitations[i];
      if not(sourCit.IsPointer) then ReformSourceCitation(aTree, sourCit);
    end;
  end;

  procedure PreparePtr(ptr: TGEDCOMPointerWithNotes);
  var
    i: Integer;
    note: TGEDCOMNotes;
  begin
    for i := 0 to ptr.NotesCount - 1 do begin
      note := ptr.Notes[i];
      if not(note.IsPointer) then ReformNote(aTree, note);
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
      cause := aEvent.Detail.Classification.ToLower();

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
    /// базовая проверка
    if (aFormat = gf_Native) then begin
      // проверка совместимости со старыми версиями
      for k := 0 to ind.IndividualEventsCount - 1 do begin
        evt := ind.IndividualEvents[k];
        CheckEventPlace(evt);
        CheckAttrCompatible(ind, evt);
      end;

      for k := 0 to ind.UserReferencesCount - 1 do begin
        CheckURefCompatible(ind, ind.UserReferences[k]);
      end;
    end else begin
      for k := 0 to ind.IndividualEventsCount - 1 do PrepareTag(ind.IndividualEvents[k].Detail);
      for k := 0 to ind.ChildToFamilyLinksCount - 1 do PreparePtr(ind.ChildToFamilyLinks[k]);
      for k := 0 to ind.SpouseToFamilyLinksCount - 1 do PreparePtr(ind.SpouseToFamilyLinks[k]);
      for k := 0 to ind.AssociationsCount - 1 do PreparePtr(ind.Associations[k]);
    end;

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
  end;

  procedure CheckFamily(fam: TGEDCOMFamilyRecord);
  var
    k: Integer;
  begin
    /// базовая проверка
    if (aFormat = gf_Native) then begin
      // проверка совместимости со старыми версиями
      for k := 0 to fam.FamilyEventCount - 1 do CheckEventPlace(fam.FamilyEvents[k]);
    end else begin
      // проверка совместимости с чужими данными
      for k := 0 to fam.FamilyEventCount - 1 do PrepareTag(fam.FamilyEvents[k].Detail);
    end;

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
  i: Integer;
  mmLink: TGEDCOMMultimediaLink;
  note: TGEDCOMNotes;
  sourCit: TGEDCOMSourceCitation;
begin
  /// Общие для всех записей свойства
  if (aRec.UID = '') then aRec.NewUID();

  if (aFormat <> gf_Native) then begin
    for i := 0 to aRec.MultimediaLinksCount - 1 do begin
      mmLink := aRec.MultimediaLinks[i];
      if not(mmLink.IsPointer) then ReformMultimediaLink(aTree, mmLink);
    end;

    for i := 0 to aRec.NotesCount - 1 do begin
      note := aRec.Notes[i];
      if not(note.IsPointer) then ReformNote(aTree, note);
    end;

    for i := 0 to aRec.SourceCitationsCount - 1 do begin
      sourCit := aRec.SourceCitations[i];
      if not(sourCit.IsPointer) then ReformSourceCitation(aTree, sourCit);
    end;
  end;

  /// Раздельно по подвидам записей
  case aRec.RecordType of
    rtFamily: CheckFamily(aRec as TGEDCOMFamilyRecord);
    rtIndividual: CheckPerson(aRec as TGEDCOMIndividualRecord);
    rtMultimedia: ;// dummy
    rtNote: ;// dummy
    rtRepository: ;// dummy
    rtSource: ;// dummy
    rtSubmission: ;// dummy
    rtSubmitter: ;// dummy
    rtGroup: CheckGroup(aRec as TGEDCOMGroupRecord);
  end;
end;

class function TGenEngine.GetGEDCOMFormat(aTree: TGEDCOMTree): TGEDCOMFormat;
var
  gf: TGEDCOMFormat;
  sour: string;
begin
  sour := aTree.Header.Source;

  for gf := gf_Native to High(TGEDCOMFormat) do
    if (GEDCOMFormats[gf].Sign = sour) then begin
      Result := gf;
      Exit;
    end;

  Result := gf_Unknown;
end;

class procedure TGenEngine.CorrectIds(aTree: TGEDCOMTree);
var
  repMap: TXRefReplaceMap;
  i: Integer;
  rec: TGEDCOMRecord;
  newXRef: string;
begin
  TfmProgress.ProgressInit(aTree.RecordsCount, LSList[LSID_IDsCorrect]);

  repMap := TXRefReplaceMap.Create;
  try
    for i := 0 to aTree.RecordsCount - 1 do begin
      rec := aTree.Records[i];

      if (GetId(rec) < 0) then begin
        newXRef := aTree.XRefIndex_NewXRef(rec);
        repMap.AddXRef(rec, rec.XRef, newXRef);
        rec.XRef := newXRef;
      end;

      TfmProgress.ProgressStep();
    end;

    aTree.Header.ReplaceXRefs(repMap);

    TfmProgress.ProgressInit(repMap.Count, LSList[LSID_IDsCorrect]);
    for i := 0 to repMap.Count - 1 do begin
      rec := repMap.Records[i].Rec;
      rec.ReplaceXRefs(repMap);

      TfmProgress.ProgressStep();
    end;
  finally
    repMap.Free;

    TfmProgress.ProgressDone();
  end;
end;

class function TGenEngine.CheckGEDCOMFormat(aTree: TGEDCOMTree): Boolean;
var
  i{, k}: Integer;
  rec: TGEDCOMRecord;
  idCheck: Boolean;
  format: TGEDCOMFormat;
begin
  Result := False;
  TfmProgress.ProgressInit(aTree.RecordsCount, LSList[LSID_FormatCheck]);
  try
    format := GetGEDCOMFormat(aTree);

    {for i := 0 to aTree.RecordsCount - 1 do begin
      for k := i + 1 to aTree.RecordsCount - 1 do begin
        if (aTree.Records[i].XRef = aTree.Records[k].XRef)
        then ; // AddDiag(aTree.Records[i].XRef, 'Объект дублирован');
      end;
    end;}

    idCheck := True;
    i := 0;
    while (i < aTree.RecordsCount) do begin
      rec := aTree.Records[i];
      CheckRecord(aTree, rec, format);

      if (format <> gf_Native) then begin
        if (idCheck) and (GetId(rec) < 0)
        then idCheck := False;
      end;

      Inc(i);
      TfmProgress.ProgressStep();
    end;

    if not(idCheck) then begin
      if (TGKUtils.ShowQuestion(LSList[LSID_IDsCorrectNeed]) = System.Windows.Forms.DialogResult.Yes)
      then CorrectIds(aTree);
    end;

    Result := True;
  finally
    TfmProgress.ProgressDone();
  end;
end;

{==============================================================================}

class procedure TGenEngine.TreeWalk(iRec: TGEDCOMIndividualRecord; aMode: TTreeWalkMode; aList: TList);
var
  rel_person: TGEDCOMIndividualRecord;
  sp: TGEDCOMPointer;
  family: TGEDCOMFamilyRecord;
  i, k: Integer;
  int_mode: TTreeWalkMode; // twmAll, twmFamily, twmAncestors, twmDescendants, twmNone
begin
  if (iRec = nil) or (aList.IndexOf(iRec) >= 0) then Exit;

  aList.Add(iRec);

  if (aMode = twmNone) then Exit;

  // родители
  if (aMode in [twmAll, twmAncestors]) then begin
    if (iRec.ChildToFamilyLinksCount > 0) then begin
      family := iRec.ChildToFamilyLinks[0].Family;

      rel_person := TGEDCOMIndividualRecord(family.Husband.Value);
      TreeWalk(rel_person, aMode, aList);

      rel_person := TGEDCOMIndividualRecord(family.Wife.Value);
      TreeWalk(rel_person, aMode, aList);
    end;
  end;

  // супруги и дети
  if (aMode in [twmAll, twmFamily, twmDescendants]) then begin
    for i := 0 to iRec.SpouseToFamilyLinksCount - 1 do begin
      family := iRec.SpouseToFamilyLinks[i].Family;

      if (iRec.Sex = svMale)
      then sp := family.Wife
      else sp := family.Husband;

      if (aMode = twmAll)
      then int_mode := twmAll
      else int_mode := twmNone;

      rel_person := TGEDCOMIndividualRecord(sp.Value);
      TreeWalk(rel_person, int_mode, aList);

      case aMode of
        twmAll: int_mode := twmAll;
        twmFamily: int_mode := twmNone;
        twmDescendants: int_mode := twmDescendants;
      end;

      for k := 0 to family.ChildrenCount - 1 do begin
        rel_person := TGEDCOMIndividualRecord(family.Children[k].Value);
        TreeWalk(rel_person, int_mode, aList);
      end;
    end;
  end;
end;

class procedure TGenEngine.TreeMerge(aMainTree: TGEDCOMTree; aFileName: string; aLog: TextBox);
var
  repMap: TXRefReplaceMap;
  i: Integer;
  extTree: TGEDCOMTree;
  rec: TGEDCOMRecord;
  newXRef: string;
begin
  if (aLog <> nil) then begin
    aLog.Clear;
    aLog.AppendText(System.&String.Format(LSList[LSID_MainBaseSize], [aMainTree.RecordsCount.ToString()])+#13#10);
  end;

  extTree := TGEDCOMTree.Create;
  repMap := TXRefReplaceMap.Create;
  try
    extTree.LoadFromFile(aFileName);
    extTree.Header.Clear;

    while (extTree.RecordsCount > 0) do begin
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

    if (aLog <> nil)
    then aLog.AppendText(System.&String.Format(LSList[LSID_MainBaseSize], [System.Convert.ToString(aMainTree.RecordsCount)])+#13#10);
  finally
    repMap.Free;
    extTree.Destroy;
  end;
end;

class procedure TGenEngine.TreeSync(aMainTree: TGEDCOMTree; aFileName: string; aLog: TextBox);
// текущая реализация - доверенная синхронизация
var
  repMap: TXRefReplaceMap;
  extTree: TGEDCOMTree;
  i: Integer;
  rec: TGEDCOMRecord;
  sync_list: TObjectList;
  sync_rec: TSyncRec;
  {s, }newXRef, backUID: string;
begin
  aLog.Clear;

  extTree := TGEDCOMTree.Create;
  repMap := TXRefReplaceMap.Create;
  sync_list := TObjectList.Create(True);
  try
    extTree.LoadFromFile(aFileName);
    extTree.Header.Clear;

    CheckGEDCOMFormat(extTree);

    // создание списка объектов синхронизации
    for i := 0 to extTree.RecordsCount - 1 do begin
      rec := extTree.Records[i];

      sync_rec := TSyncRec.Create;
      sync_rec.MasterRecord := nil;
      sync_rec.UpdateRecord := rec;
      sync_rec.State := ssUndefined;
      sync_rec.UpdateOldXRef := '';
      sync_rec.UpdateNewXRef := '';
      sync_list.Add(sync_rec);
    end;

    // поиск записей в мастер-базе
    for i := 0 to sync_list.Count - 1 do begin
      sync_rec := TSyncRec(sync_list[i]);
      rec := aMainTree.FindUID(sync_rec.UpdateRecord.UID);

      if (rec <> nil) then begin
        sync_rec.MasterRecord := rec;
        sync_rec.State := ssHasMaster;
      end else begin
        sync_rec.State := ssNoMaster;

        // если не найдена - просто переносим,
        // в них ничего менять не нужно
        rec := extTree.Extract(extTree.IndexOfRecord(sync_rec.UpdateRecord));
        newXRef := aMainTree.XRefIndex_NewXRef(rec);
        repMap.AddXRef(rec, rec.XRef, newXRef);
        rec.XRef := newXRef;
        rec.ResetOwner(aMainTree);
        aMainTree.AddRecord(rec);
      end;
    end;

    // обновить ссылки в перенесенных
    for i := 0 to repMap.Count - 1 do begin
      rec := repMap.Records[i].Rec;
      rec.ReplaceXRefs(repMap);
    end;

    // обновить ссылки в оставшихся
    for i := 0 to extTree.RecordsCount - 1 do begin
      rec := extTree.Records[i];
      rec.ReplaceXRefs(repMap);
    end;

    // слить оставшиеся записи в их оригиналы
    for i := 0 to sync_list.Count - 1 do begin
      sync_rec := TSyncRec(sync_list[i]);

      if (sync_rec.State = ssHasMaster) then begin
        // подготовка
        rec := extTree.Extract(extTree.IndexOfRecord(sync_rec.UpdateRecord));
        rec.XRef := aMainTree.XRefIndex_NewXRef(rec);
        rec.ResetOwner(aMainTree);
        aMainTree.AddRecord(rec);

        // в мастер-записи нужно сохранить UID
        backUID := sync_rec.MasterRecord.UID;

        // перенос
        sync_rec.UpdateRecord.MoveTo(sync_rec.MasterRecord, True);

        // восстановить UID
        sync_rec.MasterRecord.UID := backUID;

        // зачистка
        aMainTree.DeleteRecord(rec);
      end;
    end;

    // диагностика
    {for i := 0 to sync_list.Count - 1 do begin
      sync_rec := TSyncRec(sync_list[i]);

      s := IntToStr(i) + ': [' + sync_rec.UpdateRecord.XRef + '] -> [';
      if (sync_rec.MasterRecord <> nil)
      then s := s + sync_rec.MasterRecord.XRef + '] '
      else s := s + '-] ';

      aLog.Add(s);
    end;}

    aLog.AppendText(LSList[LSID_SyncFin]+#13#10);
  finally
    sync_list.Free;
    repMap.Free;
    extTree.Destroy;
  end;
end;

end.
