unit GKLists;

{$I GEDKeeper.inc}

interface

uses
  Windows, Classes, GedCom551, GKCommon, ComCtrls, GKSheetList
  {$IFNDEF EXT_LISTS}, bsCtrls{$ELSE}, ExtListView{$ENDIF};

type
  TGroupMode = (gmAll, gmNone, gmAny, gmSelected);

  TPersonsFilter = class(TObject)
  private
    Back_AliveBeforeDate: string;
    Back_GroupMode: TGroupMode;
    Back_GroupRef: string;
    Back_LifeMode: TLifeMode;
    Back_Name: string;
    Back_PatriarchOnly: Boolean;
    Back_Residence: string;
    Back_Sex: TGEDCOMSex;
    Back_SourceMode: TGroupMode;
    Back_SourceRef: string;
  public
    AliveBeforeDate: string;
    GroupMode: TGroupMode;
    GroupRef: string;
    LifeMode: TLifeMode;
    Name: string;
    PatriarchOnly: Boolean;
    Residence: string;
    Sex: TGEDCOMSex;
    SourceMode: TGroupMode;
    SourceRef: string;

    List: (flCommon, flSelector);
    ChildSelector: Boolean; // special mode
    TimeLineYear: Integer;

    constructor Create();

    procedure Clear();
    procedure Backup();
    procedure Restore();
  end;

  TListManager = class(TObject)
  private
    FTree: TGEDCOMTree;
  public
    constructor Create(aTree: TGEDCOMTree); virtual;
    destructor Destroy; override;

    procedure UpdateTitles(aList: TGKListView; isMain: Boolean);

    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; virtual; abstract;
    procedure Fetch(aRec: TGEDCOMRecord); virtual; abstract;
    function  GetColumnValue(aList: TGKListView; aColIndex: Integer; isMain: Boolean): string; virtual;
    procedure InitFilter(aFilter: TPersonsFilter); virtual;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); virtual; abstract;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); virtual; abstract;
  end;

  TColumnsMap = array [Byte] of record
    col_type: Byte;
    col_subtype: Byte;
  end;

  TIndividualListMan = class(TListManager)
  private
    FRec: TGEDCOMIndividualRecord;

    filter_grp: TGEDCOMGroupRecord;
    filter_abd: TDateTime;
    filter_source: TGEDCOMSourceRecord;

    // timeline runtime
    FYearMin, FYearMax: Integer;

    // filter runtime
    isLive: Boolean;
    bd, dd: TDateTime;
    age_year, bdy, ddy: Integer;
    nm, bi_date, de_date, bi_place, de_place, resi_place: string;
    p_tag: TGEDCOMTag;
    FGroups, FReligion, FNationality, FEducation, FOccupation, FCaste: string;
    FMili, FMiliInd, FMiliDis, FMiliRank: string;
  protected
    FColumnsMap: TColumnsMap;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aList: TGKListView; aColIndex: Integer; isMain: Boolean): string; override;
    procedure InitFilter(aFilter: TPersonsFilter); override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;

    property YearMin: Integer read FYearMin;
    property YearMax: Integer read FYearMax;
  end;

  TFamilyListMan = class(TListManager)
  private
    FRec: TGEDCOMFamilyRecord;

    // runtime
    FName: string;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aList: TGKListView; aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TNoteListMan = class(TListManager)
  private
    FRec: TGEDCOMNoteRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aList: TGKListView; aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TMultimediaListMan = class(TListManager)
  private
    FRec: TGEDCOMMultimediaRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aList: TGKListView; aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TSourceListMan = class(TListManager)
  private
    FRec: TGEDCOMSourceRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aList: TGKListView; aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TRepositoryListMan = class(TListManager)
  private
    FRec: TGEDCOMRepositoryRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aList: TGKListView; aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TGroupListMan = class(TListManager)
  private
    FRec: TGEDCOMGroupRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aList: TGKListView; aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TResearchListMan = class(TListManager)
  private
    FRec: TGEDCOMResearchRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aList: TGKListView; aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TTaskListMan = class(TListManager)
  private
    FRec: TGEDCOMTaskRecord;

    // runtime
    taskName: string;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aList: TGKListView; aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TCommunicationListMan = class(TListManager)
  private
    FRec: TGEDCOMCommunicationRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aList: TGKListView; aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

  TLocationListMan = class(TListManager)
  private
    FRec: TGEDCOMLocationRecord;
  public
    function  CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function  GetColumnValue(aList: TGKListView; aColIndex: Integer; isMain: Boolean): string; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TGKListView; isMain: Boolean); override;
  end;

{==============================================================================}

type
  TRecordsView = class(TGKListView)
  private
    FContentList: TList;
    FFilteredCount: Integer;
    FIsMainList: Boolean;
    FLastCached: TGEDCOMRecord;
    FListMan: TListManager;
    FRecordType: TGEDCOMRecordType;
    FTotalCount: Integer;
    FTree: TGEDCOMTree;

    procedure SetRecordType(const Value: TGEDCOMRecordType);

    {$IFDEF EXT_LISTS}
    procedure ListVMGetItemInfo(Sender: TObject; Item, SubItem: Integer;
      var Mask: TLVVMMaskItems; var Image: Integer; var OverlayImage,
      StateImage: word; var Param: LPARAM; var State: UINT; var StateMask: UINT;
      var Indent: Integer; var Text: string);
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateContents(aShieldState: TShieldState; aTitles: Boolean;
      aFilter: TPersonsFilter; aAutoSizeColumn: Integer = -1);
    procedure UpdateTitles();

    function GetSelectedRecord(): TGEDCOMRecord;

    property FilteredCount: Integer read FFilteredCount;
    property IsMainList: Boolean read FIsMainList write FIsMainList;
    property ListMan: TListManager read FListMan;
    property RecordType: TGEDCOMRecordType read FRecordType write SetRecordType;
    property TotalCount: Integer read FTotalCount;
    property Tree: TGEDCOMTree read FTree write FTree;
  end;

function GetSelectedRecord(aList: TCustomListView): TGEDCOMRecord;

implementation

uses
  {$IFDEF PROFILER}ZProfiler, {$ENDIF}
  SysUtils, GKMain, bsComUtils, Controls;

function GetSelectedRecord(aList: TCustomListView): TGEDCOMRecord;
begin
  if (aList is TRecordsView)
  then Result := TRecordsView(aList).GetSelectedRecord()
  else
    if (aList.Selected = nil)
    then Result := nil
    else Result := TGEDCOMRecord(aList.Selected.Data);
end;

{ TPersonsFilter }

constructor TPersonsFilter.Create;
begin
  inherited Create;
  Clear();
end;

procedure TPersonsFilter.Backup();
begin
  Back_AliveBeforeDate := AliveBeforeDate;
  Back_GroupMode := GroupMode;
  Back_GroupRef := GroupRef;
  Back_LifeMode := LifeMode;
  Back_Name := Name;
  Back_PatriarchOnly := PatriarchOnly;
  Back_Residence := Residence;
  Back_Sex := Sex;
  Back_SourceMode := SourceMode;
  Back_SourceRef := SourceRef;
end;

procedure TPersonsFilter.Restore();
begin
  AliveBeforeDate := Back_AliveBeforeDate;
  GroupMode := Back_GroupMode;
  GroupRef := Back_GroupRef;
  LifeMode := Back_LifeMode;
  Name := Back_Name;
  PatriarchOnly := Back_PatriarchOnly;
  Residence := Back_Residence;
  Sex := Back_Sex;
  SourceMode := Back_SourceMode;
  SourceRef := Back_SourceRef;
end;

procedure TPersonsFilter.Clear();
begin
  GroupMode := gmAll;
  GroupRef := '';
  if (LifeMode <> lmTimeLine) then begin
    LifeMode := lmAll;
    TimeLineYear := -1;
  end;
  Name := '*';
  AliveBeforeDate := '';
  PatriarchOnly := False;
  Residence := '*';
  Sex := svNone;
  SourceMode := gmAll;
  SourceRef := '';
end;

{ TListManager }

constructor TListManager.Create(aTree: TGEDCOMTree);
begin
  inherited Create;
  FTree := aTree;
end;

destructor TListManager.Destroy;
begin
  inherited Destroy;
end;

function TListManager.GetColumnValue(aList: TGKListView;
  aColIndex: Integer; isMain: Boolean): string;
begin
  Result := '';
end;

procedure TListManager.InitFilter(aFilter: TPersonsFilter);
begin
  //
end;

procedure TListManager.UpdateTitles(aList: TGKListView; isMain: Boolean);
begin
  aList.Columns.BeginUpdate;
  try
    aList.Columns.Clear;
    UpdateColumns(aList, isMain);
  finally
    aList.Columns.EndUpdate;
  end;
end;

{ TIndividualListMan }

procedure TIndividualListMan.Fetch(aRec: TGEDCOMRecord);

  function GetGroups(): string;
  var
    idx: Integer;
    grp: TGEDCOMGroupRecord;
  begin
    Result := '';

    for idx := 0 to FRec.GroupsCount - 1 do begin
      grp := TGEDCOMGroupRecord(FRec.Groups[idx].Value);
      if (grp <> nil) then begin
        Result := Result + grp.Name;

        if (idx < FRec.GroupsCount - 1)
        then Result := Result + '; ';
      end;
    end;
  end;

var
  ev: TGEDCOMCustomEvent;
  i: Integer;
  m, d: Word;
begin
  FRec := TGEDCOMIndividualRecord(aRec);

  nm := GetNameStr(FRec);
  p_tag := FRec.FindTag(PatriarchTag);

  bd := 0.0;
  dd := 0.0;
  bdy := -1;
  ddy := -1;
  bi_date := '';
  de_date := '';
  bi_place := '';
  de_place := '';
  resi_place := '';
  isLive := True;

  for i := 0 to FRec.IndividualEventsCount - 1 do begin
    ev := FRec.IndividualEvents[i];

    if (ev.Name = 'BIRT') then begin
      bd := GEDCOMDateToDate(ev.Detail.Date.Value);
      bi_date := GEDCOMCustomDateToStr(ev.Detail.Date.Value, fmGEDKeeper.Options.DefDateFormat);
      bi_place := ev.Detail.Place.StringValue;
      GetIndependentDate(ev.Detail.Date.Value, bdy, m, d);

      // timeline begin
      if (bdy > 0) then begin
        if (FYearMin > bdy) then FYearMin := bdy;
        if (FYearMax < bdy) then FYearMax := bdy;
      end;
      // timeline end
    end
    else
    if (ev.Name = 'DEAT') then begin
      dd := GEDCOMDateToDate(ev.Detail.Date.Value);
      de_date := GEDCOMCustomDateToStr(ev.Detail.Date.Value, fmGEDKeeper.Options.DefDateFormat);
      de_place := ev.Detail.Place.StringValue;
      GetIndependentDate(ev.Detail.Date.Value, ddy, m, d);
      isLive := False;
    end;
  end;

  FReligion := '';
  FNationality := '';
  FEducation := '';
  FOccupation := '';
  FCaste := '';
  FMili := '';
  FMiliInd := '';
  FMiliDis := '';
  FMiliRank := '';

  resi_place := GetResidencePlace(FRec, fmGEDKeeper.Options.PlacesWithAddress);

  for i := 0 to FRec.IndividualAttributesCount - 1 do begin
    ev := FRec.IndividualAttributes[i];

    if (ev.Name = 'RELI') then FReligion := ev.StringValue
    else
    if (ev.Name = 'NATI') then FNationality := ev.StringValue
    else
    if (ev.Name = 'EDUC') then FEducation := ev.StringValue
    else
    if (ev.Name = 'OCCU') then FOccupation := ev.StringValue
    else
    if (ev.Name = 'CAST') then FCaste := ev.StringValue
    else
    if (ev.Name = '_MILI') then FMili := ev.StringValue
    else
    if (ev.Name = '_MILI_IND') then FMiliInd := ev.StringValue
    else
    if (ev.Name = '_MILI_DIS') then FMiliDis := ev.StringValue
    else
    if (ev.Name = '_MILI_RANK') then FMiliRank := ev.StringValue;
  end;

  FGroups := GetGroups();
end;

procedure TIndividualListMan.InitFilter(aFilter: TPersonsFilter);
begin
  // filters
  if (aFilter.AliveBeforeDate = '') or (aFilter.AliveBeforeDate = '  .  .    ')
  then filter_abd := 0
  else filter_abd := StrToDate(aFilter.AliveBeforeDate);

  if (aFilter.LifeMode <> lmTimeLine)
  then age_year := -1
  else age_year := aFilter.TimeLineYear;

  if (aFilter.GroupRef = '')
  then filter_grp := nil
  else filter_grp := FTree.XRefIndex_Find(aFilter.GroupRef) as TGEDCOMGroupRecord;

  if (aFilter.SourceRef = '')
  then filter_source := nil
  else filter_source := FTree.XRefIndex_Find(aFilter.SourceRef) as TGEDCOMSourceRecord;

  // timeline
  FYearMin := 10000;
  FYearMax := 0;
end;

function TIndividualListMan.CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean;
begin
  Result := False;

  if (FRec.Restriction = rnPrivacy) and (aShieldState <> ssNone)
  then Exit;

  if ((aFilter.Sex <> svNone) and (FRec.Sex <> aFilter.Sex))
  or ((aFilter.Name <> '*') and not(IsMatchesMask(nm, aFilter.Name)))
  or ((aFilter.Residence <> '*') and not(IsMatchesMask(resi_place, aFilter.Residence)))
  or ((aFilter.PatriarchOnly and (p_tag = nil)))
  then Exit;

  case aFilter.LifeMode of
    lmAll: ;
    lmOnlyAlive:
      if not(isLive) then Exit;
    lmOnlyDead:
      if (isLive) then Exit;
    lmAliveBefore:
      if not(((bd = 0) or ((bd <> 0) and (bd < filter_abd)))
         and ((dd = 0) or ((dd <> 0) and (dd > filter_abd))))
      then Exit;
    lmTimeLine:
      if (age_year > 0)
      and not(((bdy > 0) and (bdy < age_year))
          and ((ddy <= 0) or ((ddy > 0) and (ddy > age_year))))
      then Exit;
  end;

  case aFilter.GroupMode of
    gmAll: ;
    gmNone: if (FRec.GroupsCount <> 0) then Exit;
    gmAny: if (FRec.GroupsCount = 0) then Exit;
    gmSelected: if (FRec.IndexOfGroup(filter_grp) < 0) then Exit;
  end;

  case aFilter.SourceMode of
    gmAll: ;
    gmNone: if (FRec.SourceCitationsCount <> 0) then Exit;
    gmAny: if (FRec.SourceCitationsCount = 0) then Exit;
    gmSelected: if (FRec.IndexOfSource(filter_source) < 0) then Exit;
  end;

  // special mode
  if aFilter.ChildSelector and (FRec.ChildToFamilyLinksCount <> 0)
  then Exit;

  Result := True;
end;

procedure TIndividualListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
var
  i: Integer;
  pct: TPersonColumnType;
  columns: TPersonColumnsList;
  f, n, p: string;
begin
  columns := fmGEDKeeper.Options.ListPersonsColumns;

  for i := 0 to High(columns) do begin
    pct := columns[i].colType;
    if not(columns[i].colActive) then Continue;

    case pct of
      pctPatriarch: begin
        if (p_tag = nil)
        then aItem.SubItems.Add(' ')
        else aItem.SubItems.Add('*');
      end;

      pctName: begin
        GetNameParts(FRec, f, n, p);

        case fmGEDKeeper.Options.DefNameFormat of
          nfFNP: begin
            aItem.SubItems.Add(nm);
          end;
          nfF_NP: begin
            aItem.SubItems.Add(f);
            aItem.SubItems.Add(n + ' ' + p);
          end;
          nfF_N_P: begin
            aItem.SubItems.Add(f);
            aItem.SubItems.Add(n);
            aItem.SubItems.Add(p);
          end;
        end;
      end;

      pctNick:
        aItem.SubItems.Add(GetNickStr(FRec));

      pctSex:
        aItem.SubItems.Add(SexSigns[FRec.Sex]);

      pctBirthDate:
        aItem.SubItems.Add(bi_date);

      pctDeathDate:
        aItem.SubItems.Add(de_date);

      pctBirthPlace:
        aItem.SubItems.Add(bi_place);

      pctDeathPlace:
        aItem.SubItems.Add(de_place);

      pctResidence:
        aItem.SubItems.Add(resi_place);

      pctAge:
        if (isMain) then aItem.SubItems.Add(GetAge(FRec, age_year));

      pctLifeExpectancy:
        if (isMain) then aItem.SubItems.Add(GetLifeExpectancy(FRec));

      pctDaysForBirth:
        if (isMain) then aItem.SubItems.Add(GetDaysForBirth(FRec));

      pctGroups:
        if (isMain) then aItem.SubItems.Add(FGroups);

      pctReligion:
        if (isMain) then aItem.SubItems.Add(FReligion);

      pctNationality:
        if (isMain) then aItem.SubItems.Add(FNationality);

      pctEducation:
        if (isMain) then aItem.SubItems.Add(FEducation);

      pctOccupation:
        if (isMain) then aItem.SubItems.Add(FOccupation);

      pctCaste:
        if (isMain) then aItem.SubItems.Add(FCaste);

      //
      pctMili:
        if (isMain) then aItem.SubItems.Add(FMili);
      pctMiliInd:
        if (isMain) then aItem.SubItems.Add(FMiliInd);
      pctMiliDis:
        if (isMain) then aItem.SubItems.Add(FMiliDis);
      pctMiliRank:
        if (isMain) then aItem.SubItems.Add(FMiliRank);
      //

      pctChangeDate:
        if (isMain) then aItem.SubItems.Add(GetChangeDate(FRec));
    end;
  end;
end;

function TIndividualListMan.GetColumnValue(aList: TGKListView;
  aColIndex: Integer; isMain: Boolean): string;
var
  pct: TPersonColumnType;
  sub_index: Integer;
  f, n, p: string;
begin
  pct := TPersonColumnType(FColumnsMap[aColIndex].col_type);
  sub_index := FColumnsMap[aColIndex].col_subtype;

  case pct of
    pctPatriarch: begin
      if (p_tag = nil)
      then Result := ' '
      else Result := '*';
    end;

    pctName: begin
      GetNameParts(FRec, f, n, p);

      case fmGEDKeeper.Options.DefNameFormat of
        nfFNP: begin
          Result := nm;
        end;
        nfF_NP: begin
          case sub_index of
            0: Result := f;
            1: Result := n + ' ' + p;
          end;
        end;
        nfF_N_P: begin
          case sub_index of
            0: Result := f;
            1: Result := n;
            2: Result := p;
          end;
        end;
      end;
    end;

    pctNick: Result := GetNickStr(FRec);
    pctSex: Result := SexSigns[FRec.Sex];
    pctBirthDate: Result := bi_date;
    pctDeathDate: Result := de_date;
    pctBirthPlace: Result := bi_place;
    pctDeathPlace: Result := de_place;
    pctResidence: Result := resi_place;
    pctAge: Result := GetAge(FRec);
    pctLifeExpectancy: Result := GetLifeExpectancy(FRec);
    pctDaysForBirth: Result := GetDaysForBirth(FRec);
    pctGroups: Result := FGroups;
    pctReligion: Result := FReligion;
    pctNationality: Result := FNationality;
    pctEducation: Result := FEducation;
    pctOccupation: Result := FOccupation;
    pctCaste: Result := FCaste;

    pctMili: Result := FMili;
    pctMiliInd: Result := FMiliInd;
    pctMiliDis: Result := FMiliDis;
    pctMiliRank: Result := FMiliRank;

    pctChangeDate: Result := GetChangeDate(FRec);
  end;
end;

procedure TIndividualListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
var
  cols: Integer;

  procedure SetColMap(aType, aSubType: Byte);
  begin
    Inc(cols);
    FColumnsMap[cols].col_type := aType;
    FColumnsMap[cols].col_subtype := aSubType;
  end;

var
  i: Integer;
  columns: TPersonColumnsList;
  col_type: TPersonColumnType;
  asz: Boolean;
begin
  columns := fmGEDKeeper.Options.ListPersonsColumns;

  cols := 0;

  AddListColumn(aList, '№', 50);

  for i := 0 to High(columns) do
    if columns[i].colActive then begin
      col_type := columns[i].colType;

      case col_type of
        pctPatriarch: begin
          AddListColumn(aList, 'П', 25);
          SetColMap(Ord(col_type), 0);
        end;

        pctName: begin
          asz := {$IFDEF EXT_LISTS}True{$ELSE}False{$ENDIF};

          case fmGEDKeeper.Options.DefNameFormat of
            nfFNP: begin
              AddListColumn(aList, 'Фамилия,Имя,Отчество', 300, asz);
              SetColMap(Ord(col_type), 0);
            end;

            nfF_NP: begin
              AddListColumn(aList, 'Фамилия', 150, asz);
              SetColMap(Ord(col_type), 0);
              AddListColumn(aList, 'Имя,Отчество', 150, asz);
              SetColMap(Ord(col_type), 1);
            end;

            nfF_N_P: begin
              AddListColumn(aList, 'Фамилия', 150, asz);
              SetColMap(Ord(col_type), 0);
              AddListColumn(aList, 'Имя', 100, asz);
              SetColMap(Ord(col_type), 1);
              AddListColumn(aList, 'Отчество', 150, asz);
              SetColMap(Ord(col_type), 2);
            end;
          end;
        end;

        pctNick, pctSex, pctBirthDate, pctDeathDate,
        pctBirthPlace, pctDeathPlace, pctResidence: begin
          AddListColumn(aList, PersonColumnsName[col_type].Name, PersonColumnsName[col_type].DefWidth);
          SetColMap(Ord(col_type), 0);
        end;

        pctAge, pctLifeExpectancy, pctDaysForBirth, pctGroups,
        pctReligion, pctNationality, pctEducation, pctOccupation, pctCaste,
        pctMili, pctMiliInd, pctMiliDis, pctMiliRank,
        pctChangeDate: if isMain then begin
          AddListColumn(aList, PersonColumnsName[col_type].Name, PersonColumnsName[col_type].DefWidth);
          SetColMap(Ord(col_type), 0);
        end;
      end;
    end;
end;

{ TFamilyListMan }

function TFamilyListMan.CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean;
begin
  Result := False;

  if (FRec.Restriction = rnPrivacy) and (aShieldState <> ssNone)
  then Exit;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(FName, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TFamilyListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMFamilyRecord(aRec);
  FName := GetFamilyStr(FRec);
end;

procedure TFamilyListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(FName);
  aItem.SubItems.Add(GetMarriageDate(FRec, fmGEDKeeper.Options.DefDateFormat));

  if isMain
  then aItem.SubItems.Add(GetChangeDate(FRec));
end;

function TFamilyListMan.GetColumnValue(aList: TGKListView;
  aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := FName;
    2: Result := GetMarriageDate(FRec, fmGEDKeeper.Options.DefDateFormat);
    3: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

procedure TFamilyListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Супруги', 300);
  AddListColumn(aList, 'Дата брака', 100);
  if isMain then begin
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

{ TNoteListMan }

function TNoteListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
begin
  Result := True;
end;

procedure TNoteListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMNoteRecord(aRec);
end;

procedure TNoteListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
var
  st: string;
begin
  if (FRec.Notes.Count > 0) then begin
    st := Trim(FRec.Notes[0]);
    if (st = '') and (FRec.Notes.Count > 1)
    then st := Trim(FRec.Notes[1]);
  end else st := '';

  aItem.SubItems.Add(st);

  if isMain
  then aItem.SubItems.Add(GetChangeDate(FRec));
end;

function TNoteListMan.GetColumnValue(aList: TGKListView;
  aColIndex: Integer; isMain: Boolean): string;
var
  st: string;
begin
  case aColIndex of
    1: begin
      if (FRec.Notes.Count > 0) then begin
        st := Trim(FRec.Notes[0]);
        if (st = '') and (FRec.Notes.Count > 1)
        then st := Trim(FRec.Notes[1]);
      end else st := '';

      Result := st;
    end;
    2: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

procedure TNoteListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Заметка', 400);

  if isMain then begin
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

{ TMultimediaListMan }

function TMultimediaListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
var
  file_ref: TGEDCOMFileReferenceWithTitle;
begin
  Result := False;

  file_ref := FRec.FileReferences[0];

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(file_ref.Title, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TMultimediaListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMMultimediaRecord(aRec);
end;

procedure TMultimediaListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
var
  file_ref: TGEDCOMFileReferenceWithTitle;
begin
  file_ref := FRec.FileReferences[0];

  aItem.SubItems.Add(file_ref.Title);
  aItem.SubItems.Add(MediaTypes[file_ref.MediaType].Name);

  if isMain then begin
    aItem.SubItems.Add(file_ref.StringValue);
    aItem.SubItems.Add(GetChangeDate(FRec));
  end;
end;

function TMultimediaListMan.GetColumnValue(aList: TGKListView;
  aColIndex: Integer; isMain: Boolean): string;
var
  file_ref: TGEDCOMFileReferenceWithTitle;
begin
  file_ref := FRec.FileReferences[0];

  case aColIndex of
    1: Result := file_ref.Title;
    2: Result := MediaTypes[file_ref.MediaType].Name;
    3: Result := file_ref.StringValue;
    4: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

procedure TMultimediaListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Название', 150);
  AddListColumn(aList, 'Тип', 85);

  if isMain then begin
    AddListColumn(aList, 'Файл', 300);
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

{ TSourceListMan }

function TSourceListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(FRec.FiledByEntry, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TSourceListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMSourceRecord(aRec);
end;

procedure TSourceListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(Trim(FRec.FiledByEntry));

  if isMain then begin
    aItem.SubItems.Add(Trim(FRec.Originator.Text));
    aItem.SubItems.Add(Trim(FRec.Title.Text));
    aItem.SubItems.Add(GetChangeDate(FRec));
  end;
end;

function TSourceListMan.GetColumnValue(aList: TGKListView;
  aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := Trim(FRec.FiledByEntry);
    2: Result := Trim(FRec.Originator.Text);
    3: Result := Trim(FRec.Title.Text);
    4: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

procedure TSourceListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Краткое название', 120);

  if isMain then begin
    AddListColumn(aList, 'Автор', 200);
    AddListColumn(aList, 'Название', 200);
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

{ TRepositoryListMan }

function TRepositoryListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(FRec.RepositoryName, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TRepositoryListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMRepositoryRecord(aRec);
end;

procedure TRepositoryListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Архив', 400);

  if isMain then begin
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

procedure TRepositoryListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(FRec.RepositoryName);

  if isMain
  then aItem.SubItems.Add(GetChangeDate(FRec));
end;

function TRepositoryListMan.GetColumnValue(aList: TGKListView;
  aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := FRec.RepositoryName;
    2: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

{ TGroupListMan }

function TGroupListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(FRec.Name, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TGroupListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMGroupRecord(aRec);
end;

procedure TGroupListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Группа', 400);

  if isMain then begin
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

procedure TGroupListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(FRec.Name);

  if isMain
  then aItem.SubItems.Add(GetChangeDate(FRec));
end;

function TGroupListMan.GetColumnValue(aList: TGKListView;
  aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := FRec.Name;
    2: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

{ TResearchListMan }

function TResearchListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(FRec.Name, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TResearchListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMResearchRecord(aRec);
end;

procedure TResearchListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Исследование', 300);
  AddListColumn(aList, 'Приоритет', 90);
  AddListColumn(aList, 'Состояние', 90);
  AddListColumn(aList, 'Запущено', 90);
  AddListColumn(aList, 'Завершено', 90);
  AddListColumn(aList, 'Процент', 90);

  if isMain then begin
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

procedure TResearchListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(FRec.Name);
  aItem.SubItems.Add(PriorityNames[FRec.Priority]);
  aItem.SubItems.Add(StatusNames[FRec.Status]);
  aItem.SubItems.Add(GEDCOMDateToStr(FRec.StartDate, fmGEDKeeper.Options.DefDateFormat));
  aItem.SubItems.Add(GEDCOMDateToStr(FRec.StopDate, fmGEDKeeper.Options.DefDateFormat));
  aItem.SubItems.Add(IntToStr(FRec.Percent));

  if isMain
  then aItem.SubItems.Add(GetChangeDate(FRec));
end;

function TResearchListMan.GetColumnValue(aList: TGKListView;
  aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := FRec.Name;
    2: Result := PriorityNames[FRec.Priority];
    3: Result := StatusNames[FRec.Status];
    4: Result := GEDCOMDateToStr(FRec.StartDate, fmGEDKeeper.Options.DefDateFormat);
    5: Result := GEDCOMDateToStr(FRec.StopDate, fmGEDKeeper.Options.DefDateFormat);
    6: Result := IntToStr(FRec.Percent);
    7: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

{ TTaskListMan }

function TTaskListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(taskName, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TTaskListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMTaskRecord(aRec);
  taskName := GetTaskGoalStr(FTree, FRec);
end;

procedure TTaskListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Цель', 300);
  AddListColumn(aList, 'Приоритет', 90);
  AddListColumn(aList, 'Запущено', 90);
  AddListColumn(aList, 'Завершено', 90);

  if isMain then begin
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

procedure TTaskListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(taskName);
  aItem.SubItems.Add(PriorityNames[FRec.Priority]);
  aItem.SubItems.Add(GEDCOMDateToStr(FRec.StartDate, fmGEDKeeper.Options.DefDateFormat));
  aItem.SubItems.Add(GEDCOMDateToStr(FRec.StopDate, fmGEDKeeper.Options.DefDateFormat));

  if isMain
  then aItem.SubItems.Add(GetChangeDate(FRec));
end;

function TTaskListMan.GetColumnValue(aList: TGKListView;
  aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := taskName;
    2: Result := PriorityNames[FRec.Priority];
    3: Result := GEDCOMDateToStr(FRec.StartDate, fmGEDKeeper.Options.DefDateFormat);
    4: Result := GEDCOMDateToStr(FRec.StopDate, fmGEDKeeper.Options.DefDateFormat);
    5: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

{ TCommunicationListMan }

function TCommunicationListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(FRec.Name, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TCommunicationListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMCommunicationRecord(aRec);
end;

procedure TCommunicationListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Тема', 300);
  AddListColumn(aList, 'Корреспондент', 200);
  AddListColumn(aList, 'Тип', 90);
  AddListColumn(aList, 'Дата', 90);

  if isMain then begin
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

procedure TCommunicationListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(FRec.Name);
  aItem.SubItems.Add(GetCorresponderStr(FTree, FRec, False));
  aItem.SubItems.Add(CommunicationNames[FRec.CommunicationType]);
  aItem.SubItems.Add(GEDCOMDateToStr(FRec.Date, fmGEDKeeper.Options.DefDateFormat));

  if isMain
  then aItem.SubItems.Add(GetChangeDate(FRec));
end;

function TCommunicationListMan.GetColumnValue(aList: TGKListView;
  aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := FRec.Name;
    2: Result := GetCorresponderStr(FTree, FRec, False);
    3: Result := CommunicationNames[FRec.CommunicationType];
    4: Result := GEDCOMDateToStr(FRec.Date, fmGEDKeeper.Options.DefDateFormat);
    5: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

{ TLocationListMan }

function TLocationListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
begin
  Result := False;

  if (aFilter.List = flSelector)
  and ((aFilter.Name <> '*') and not(IsMatchesMask(FRec.Name, aFilter.Name)))
  then Exit;

  Result := True;
end;

procedure TLocationListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMLocationRecord(aRec);
end;

procedure TLocationListMan.UpdateColumns(aList: TGKListView; isMain: Boolean);
begin
  AddListColumn(aList, '№', 50);
  AddListColumn(aList, 'Название', 300);
  AddListColumn(aList, 'Широта', 120);
  AddListColumn(aList, 'Долгота', 120);

  if isMain then begin
    AddListColumn(aList, 'Изменено', 150);
  end;
end;

procedure TLocationListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(FRec.Name);
  aItem.SubItems.Add(FRec.Map.Lati);
  aItem.SubItems.Add(FRec.Map.Long);

  if isMain
  then aItem.SubItems.Add(GetChangeDate(FRec));
end;

function TLocationListMan.GetColumnValue(aList: TGKListView;
  aColIndex: Integer; isMain: Boolean): string;
begin
  case aColIndex of
    1: Result := FRec.Name;
    2: Result := FRec.Map.Lati;
    3: Result := FRec.Map.Long;
    4: Result := GetChangeDate(FRec);
    else Result := '';
  end;
end;

{ TRecordsView }

constructor TRecordsView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  {$IFDEF EXT_LISTS}
  OnVMGetItemInfo := ListVMGetItemInfo;
  {$ENDIF}

  FContentList := TList.Create;

  FListMan := nil;
  FRecordType := rtNone;

  FLastCached := nil;
end;

destructor TRecordsView.Destroy;
begin
  if Assigned(FListMan) then FreeAndNil(FListMan);

  FContentList.Destroy;

  inherited Destroy;
end;

procedure TRecordsView.SetRecordType(const Value: TGEDCOMRecordType);
begin
  FRecordType := Value;

  if Assigned(FListMan) then FreeAndNil(FListMan);

  case FRecordType of
    rtNone: ;
    rtIndividual: FListMan := TIndividualListMan.Create(FTree);
    rtFamily: FListMan := TFamilyListMan.Create(FTree);
    rtNote: FListMan := TNoteListMan.Create(FTree);
    rtMultimedia: FListMan := TMultimediaListMan.Create(FTree);
    rtSource: FListMan := TSourceListMan.Create(FTree);
    rtRepository: FListMan := TRepositoryListMan.Create(FTree);
    rtGroup: FListMan := TGroupListMan.Create(FTree);
    rtResearch: FListMan := TResearchListMan.Create(FTree);
    rtTask: FListMan := TTaskListMan.Create(FTree);
    rtCommunication: FListMan := TCommunicationListMan.Create(FTree);
    rtLocation: FListMan := TLocationListMan.Create(FTree);
    rtSubmission: FListMan := nil;
    rtSubmitter: FListMan := nil;
  end;
end;

function TRecordsView.GetSelectedRecord(): TGEDCOMRecord;
begin
  Result := nil;

  {$IFDEF VIRTUAL_LISTS}
  if (ItemIndex < 0)
  then Result := nil
  else Result := TGEDCOMRecord(FContentList[ItemIndex]);
  {$ELSE}
  if (Selected = nil)
  then Result := nil
  else Result := TGEDCOMRecord(Selected.Data);
  {$ENDIF}
end;

procedure TRecordsView.UpdateTitles();
begin
  FListMan.UpdateTitles(Self, FIsMainList);
end;

procedure TRecordsView.UpdateContents(aShieldState: TShieldState; aTitles: Boolean;
  aFilter: TPersonsFilter; aAutoSizeColumn: Integer = -1);
var
  {$IFNDEF VIRTUAL_LISTS}
  item: TListItem;
  {$ENDIF}
  i: Integer;
  rec: TGEDCOMRecord;
begin
  {$IFDEF PROFILER}Profiler.Mark(8, True);{$ENDIF}

  try
    FTotalCount := 0;
    FFilteredCount := 0;

    if (aTitles) and (FListMan <> nil)
    then FListMan.UpdateTitles(Self, FIsMainList);

    Items.BeginUpdate();
    try
      // фильтрация
      {$IFDEF PROFILER}Profiler.Mark(2, True);{$ENDIF}
      FListMan.InitFilter(aFilter);
      FContentList.Clear;
      for i := 0 to FTree.RecordsCount - 1 do begin
        rec := FTree.Records[i];

        {$IFDEF PROFILER}Profiler.Mark(3, True);{$ENDIF}
        if RecordIsType(FRecordType, rec) then begin
          Inc(FTotalCount);

          FListMan.Fetch(rec);
          if FListMan.CheckFilter(aFilter, aShieldState)
          then FContentList.Add(rec);
        end;
        {$IFDEF PROFILER}Profiler.Mark(3, False);{$ENDIF}
      end;
      FFilteredCount := FContentList.Count;
      {$IFDEF PROFILER}Profiler.Mark(2, False);{$ENDIF}

      //

      {$IFDEF VIRTUAL_LISTS}
        SetItemCountEx(FContentList.Count, [lvsicfNoScroll]);
      {$ELSE}
        // вывод
        {$IFDEF PROFILER}Profiler.Mark(4, True);{$ENDIF}
        Items.Clear();
        for i := 0 to FContentList.Count - 1 do begin
          rec := TGEDCOMRecord(FContentList[i]);

          {$IFDEF PROFILER}Profiler.Mark(5, True);{$ENDIF}
          item := Items.Add();
          item.Caption := IntToStr(GetId(rec));
          item.Data := rec;

          FListMan.Fetch(rec);
          FListMan.UpdateItem(item, FIsMainList);
          {$IFDEF PROFILER}Profiler.Mark(5, False);{$ENDIF}
        end;
        {$IFDEF PROFILER}Profiler.Mark(4, False);{$ENDIF}

        {$IFDEF PROFILER}Profiler.Mark(6, True);{$ENDIF}
        if (aAutoSizeColumn >= 0)
        then ResizeColumn(Self, aAutoSizeColumn);
        {$IFDEF PROFILER}Profiler.Mark(6, False);{$ENDIF}
      {$ENDIF}
    finally
      Items.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('UpdateContents(): ' + E.Message);
  end;

  {$IFDEF PROFILER}Profiler.Mark(8, False);{$ENDIF}
end;

{$IFDEF EXT_LISTS}
procedure TRecordsView.ListVMGetItemInfo(Sender: TObject; Item, SubItem: Integer;
  var Mask: TLVVMMaskItems; var Image: Integer; var OverlayImage,
  StateImage: word; var Param: LPARAM; var State: UINT; var StateMask: UINT;
  var Indent: Integer; var Text: string);
var
  rec: TGEDCOMRecord;
begin
  {if (lvifImage in Mask) then begin
    //image := f_obj.ImgIndex;
  end;}

  if (lvifText in Mask) then begin
    {$IFDEF PROFILER}Profiler.Mark(11, True);{$ENDIF}

    rec := TGEDCOMRecord(FContentList[Item]);

    if (FLastCached <> rec) then begin
      FLastCached := rec;
      FListMan.Fetch(rec);
    end;

    if (SubItem = 0)
    then Text := IntToStr(GetId(rec))
    else Text := FListMan.GetColumnValue(Self, SubItem, FIsMainList);

    {$IFDEF PROFILER}Profiler.Mark(11, False);{$ENDIF}
  end;
end;
{$ENDIF}

end.
