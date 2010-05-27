unit GKLists;

interface

uses
  GedCom551, GKCommon, ComCtrls, bsCtrls;

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
  public
    AliveBeforeDate: string;
    GroupMode: TGroupMode;
    GroupRef: string;
    LifeMode: TLifeMode;
    Name: string;
    PatriarchOnly: Boolean;
    Residence: string;
    Sex: TGEDCOMSex;

    List: (flCommon, flSelector);
    ChildSelector: Boolean; // special mode

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

    procedure UpdateTitles(aList: TBSListView; isMain: Boolean); 

    procedure Fetch(aRec: TGEDCOMRecord); virtual; abstract;
    function CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; virtual; abstract;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); virtual; abstract;
    procedure UpdateColumns(aList: TBSListView; isMain: Boolean); virtual; abstract;
  end;

  TIndividualListMan = class(TListManager)
  private
    FRec: TGEDCOMIndividualRecord;

    // runtime
    isLive: Boolean;
    bd, dd, fdt: TDateTime;
    f, n, p, nm: string;
    bi_date, de_date, bi_place, de_place, resi_place: string;
    p_tag: TGEDCOMTag;
    FGroups, FReligion, FNationality, FEducation, FOccupation, FCaste: string;
  public
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TBSListView; isMain: Boolean); override;
  end;

  TFamilyListMan = class(TListManager)
  private
    FRec: TGEDCOMFamilyRecord;

    // runtime
  public
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TBSListView; isMain: Boolean); override;
  end;

  TNoteListMan = class(TListManager)
  private
    FRec: TGEDCOMNoteRecord;

    // runtime
  public
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TBSListView; isMain: Boolean); override;
  end;

  TMultimediaListMan = class(TListManager)
  private
    FRec: TGEDCOMMultimediaRecord;

    // runtime
  public
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TBSListView; isMain: Boolean); override;
  end;

  TSourceListMan = class(TListManager)
  private
    FRec: TGEDCOMSourceRecord;

    // runtime
  public
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TBSListView; isMain: Boolean); override;
  end;

  TRepositoryListMan = class(TListManager)
  private
    FRec: TGEDCOMRepositoryRecord;

    // runtime
  public
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TBSListView; isMain: Boolean); override;
  end;

  TGroupListMan = class(TListManager)
  private
    FRec: TGEDCOMGroupRecord;

    // runtime
  public
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TBSListView; isMain: Boolean); override;
  end;

  TResearchListMan = class(TListManager)
  private
    FRec: TGEDCOMResearchRecord;

    // runtime
  public
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TBSListView; isMain: Boolean); override;
  end;

  TTaskListMan = class(TListManager)
  private
    FRec: TGEDCOMTaskRecord;

    // runtime
    taskName: string;
  public
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TBSListView; isMain: Boolean); override;
  end;

  TCommunicationListMan = class(TListManager)
  private
    FRec: TGEDCOMCommunicationRecord;

    // runtime
  public
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TBSListView; isMain: Boolean); override;
  end;

  TLocationListMan = class(TListManager)
  private
    FRec: TGEDCOMLocationRecord;

    // runtime
  public
    procedure Fetch(aRec: TGEDCOMRecord); override;
    function CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean; override;
    procedure UpdateItem(aItem: TListItem; isMain: Boolean); override;
    procedure UpdateColumns(aList: TBSListView; isMain: Boolean); override;
  end;

implementation

uses SysUtils, GKMain, bsComUtils, GKSheetList;

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
end;

procedure TPersonsFilter.Clear();
begin
  GroupMode := gmAll;
  GroupRef := '';
  LifeMode := lmAll;
  Name := '*';
  AliveBeforeDate := '';
  PatriarchOnly := False;
  Residence := '*';
  Sex := svNone;
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

procedure TListManager.UpdateTitles(aList: TBSListView; isMain: Boolean);
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
begin
  FRec := TGEDCOMIndividualRecord(aRec);

  nm := GetNameStr(FRec);
  GetNameParts(FRec, f, n, p);
  p_tag := FRec.FindTag(PatriarchTag);

  bd := 0.0;
  dd := 0.0;
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
    end
    else
    if (ev.Name = 'DEAT') then begin
      dd := GEDCOMDateToDate(ev.Detail.Date.Value);
      de_date := GEDCOMCustomDateToStr(ev.Detail.Date.Value, fmGEDKeeper.Options.DefDateFormat);
      de_place := ev.Detail.Place.StringValue;
      isLive := False;
    end;
  end;

  FReligion := '';
  FNationality := '';
  FEducation := '';
  FOccupation := '';
  FCaste := '';

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
    if (ev.Name = 'CAST') then FCaste := ev.StringValue;
  end;

  FGroups := GetGroups();
end;

function TIndividualListMan.CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean;
var
  res: Boolean;
  priv: Boolean;
  grp: TGEDCOMGroupRecord;
begin
  Result := False;

  priv := (FRec.Restriction = rnPrivacy) and (aShieldState <> ssNone);
  if priv then Exit;

  if ((aFilter.Sex <> svNone) and (FRec.Sex <> aFilter.Sex))
  or ((aFilter.Name <> '*') and not(IsMatchesMask(nm, aFilter.Name)))
  or ((aFilter.Residence <> '*') and not(IsMatchesMask(resi_place, aFilter.Residence)))
  or ((aFilter.PatriarchOnly and (p_tag = nil)))
  then Exit;

  case aFilter.LifeMode of
    lmAll: ;

    lmOnlyAlive: if not(isLive) then Exit;

    lmOnlyDead: if (isLive) then Exit;

    lmAliveBefore: begin
      Hole(bi_date);
      Hole(de_date);

      fdt := StrToDate(aFilter.AliveBeforeDate);
      res := ((bd = 0) or ((bd <> 0) and (bd < fdt))) and ((dd = 0) or ((dd <> 0) and (dd > fdt)));
      if not(res) then Exit;
    end;
  end;

  case aFilter.GroupMode of
    gmAll: ;
    gmNone: if (FRec.GroupsCount <> 0) then Exit;
    gmAny: if (FRec.GroupsCount = 0) then Exit;
    gmSelected: begin
      grp := FTree.XRefIndex_Find(aFilter.GroupRef) as TGEDCOMGroupRecord;
      if (FRec.IndexOfGroup(grp) < 0) then Exit;
    end;
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
        if (isMain) then aItem.SubItems.Add(GetAge(FRec));

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

      pctChangeDate:
        if (isMain) then aItem.SubItems.Add(GetChangeDate(FRec));
    end;
  end;
end;

procedure TIndividualListMan.UpdateColumns(aList: TBSListView; isMain: Boolean);
var
  i: Integer;
  columns: TPersonColumnsList;
  col_type: TPersonColumnType;
begin
  columns := fmGEDKeeper.Options.ListPersonsColumns;

  AddListColumn(aList, '№', 50);
  for i := 0 to High(columns) do
    if columns[i].colActive then begin
      col_type := columns[i].colType;

      case col_type of
        pctPatriarch:
          AddListColumn(aList, 'П', 25);

        pctName: begin
          case fmGEDKeeper.Options.DefNameFormat of
            nfFNP: begin
              AddListColumn(aList, 'Фамилия,Имя,Отчество', 300);
            end;

            nfF_NP: begin
              AddListColumn(aList, 'Фамилия', 150);
              AddListColumn(aList, 'Имя,Отчество', 150);
            end;

            nfF_N_P: begin
              AddListColumn(aList, 'Фамилия', 150);
              AddListColumn(aList, 'Имя', 100);
              AddListColumn(aList, 'Отчество', 150);
            end;
          end;
        end;

        pctSex, pctBirthDate, pctDeathDate,
        pctBirthPlace, pctDeathPlace, pctResidence:
          AddListColumn(aList, PersonColumnsName[col_type].Name, PersonColumnsName[col_type].DefWidth);

        pctAge, pctLifeExpectancy, pctDaysForBirth, pctGroups,
        pctReligion, pctNationality, pctEducation, pctOccupation, pctCaste,
        pctChangeDate:
          if isMain
          then AddListColumn(aList, PersonColumnsName[col_type].Name, PersonColumnsName[col_type].DefWidth);
      end;
    end;
end;

{ TFamilyListMan }

function TFamilyListMan.CheckFilter(aFilter: TPersonsFilter; aShieldState: TShieldState): Boolean;
var
  priv: Boolean;
begin
  Result := False;

  priv := (FRec.Restriction = rnPrivacy) and (aShieldState <> ssNone);
  if priv then Exit;

  Result := True;
end;

procedure TFamilyListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMFamilyRecord(aRec);
end;

procedure TFamilyListMan.UpdateItem(aItem: TListItem; isMain: Boolean);
begin
  aItem.SubItems.Add(GetFamilyStr(FRec));
  aItem.SubItems.Add(GetMarriageDate(FRec, fmGEDKeeper.Options.DefDateFormat));

  if isMain
  then aItem.SubItems.Add(GetChangeDate(FRec));
end;

procedure TFamilyListMan.UpdateColumns(aList: TBSListView; isMain: Boolean);
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

procedure TNoteListMan.UpdateColumns(aList: TBSListView; isMain: Boolean);
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

procedure TMultimediaListMan.UpdateColumns(aList: TBSListView; isMain: Boolean);
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

procedure TSourceListMan.UpdateColumns(aList: TBSListView; isMain: Boolean);
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

procedure TRepositoryListMan.UpdateColumns(aList: TBSListView; isMain: Boolean);
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

procedure TGroupListMan.UpdateColumns(aList: TBSListView; isMain: Boolean);
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

{ TResearchListMan }

function TResearchListMan.CheckFilter(aFilter: TPersonsFilter;
  aShieldState: TShieldState): Boolean;
begin
  Result := False;
  Result := True;
end;

procedure TResearchListMan.Fetch(aRec: TGEDCOMRecord);
begin
  FRec := TGEDCOMResearchRecord(aRec);
end;

procedure TResearchListMan.UpdateColumns(aList: TBSListView; isMain: Boolean);
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

procedure TTaskListMan.UpdateColumns(aList: TBSListView; isMain: Boolean);
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

procedure TCommunicationListMan.UpdateColumns(aList: TBSListView; isMain: Boolean);
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

procedure TLocationListMan.UpdateColumns(aList: TBSListView; isMain: Boolean);
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

end.
