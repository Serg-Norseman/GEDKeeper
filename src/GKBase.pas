unit GKBase;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Buttons, Menus, Masks, ActnList,
  GKCommon, GedCom551, HTMemo, bsCtrls, GKUIToolkit, GKLists;

type
  TFilePropertiesMode = (fpmAuthor, fpmDiags, fpmAdvanced);

  TRecCount = record
    Total: Integer;
    Filtered: Integer;
  end;

  TfmBase = class(TForm)
    PageRecords: TPageControl;
    SheetPersons: TTabSheet;
    SheetNotes: TTabSheet;
    SheetMultimedia: TTabSheet;
    SheetSources: TTabSheet;
    SheetFamilies: TTabSheet;
    SheetGroups: TTabSheet;
    ActionList1: TActionList;
    SheetRepositories: TTabSheet;
    actTest: TAction;
    SheetResearches: TTabSheet;
    SheetTasks: TTabSheet;
    SheetCommunications: TTabSheet;
    SheetLocations: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PageRecordsChange(Sender: TObject);
    procedure mPersonSummaryLink(Sender: TObject; LinkName: String);
    procedure actTestExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    FBackman: TBackManager;
    FChangedRecords: array [TGEDCOMRecordType] of TList;
    FFileName: string;
    FLockedRecords: TList;
    FModified: Boolean;
    FShieldState: TShieldState;
    FTree: TGEDCOMTree;
    FUndoman: TUndoManager;
    FXFilter: TPersonsFilter;

    procedure CleanFamily(aFamily: TGEDCOMFamilyRecord);
    function  IsMainList(aRecType: TGEDCOMRecordType; aList: TGKListView): Boolean;
    procedure ListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure NavAdd(aRec: TGEDCOMRecord);
    procedure SetFileName(const Value: string);
    procedure SetMainTitle();
    procedure SetModified(const Value: Boolean);
    procedure ShowAddress(anAddress: TGEDCOMAddress; aSummary: TStrings);
    procedure SetShieldState(const Value: TShieldState);

    {procedure ShowLockMsg();
    procedure LockRecord(aRecord: TGEDCOMRecord; aLock: Boolean);
    function  RequestModify(aRec: TGEDCOMRecord): Boolean;
    function ModifyRecord(var aRecord: TGEDCOMRecord; anAction: TRecAction; aConfirm: Boolean): Boolean;}
  public
    // GUI
    ListPersons: TRecordsView;
    mPersonSummary: THTMemo;
    ListFamilies: TRecordsView;
    mFamilySummary: THTMemo;
    ListNotes: TRecordsView;
    mNoteSummary: THTMemo;
    ListMultimedia: TRecordsView;
    mMediaSummary: THTMemo;
    ListSources: TRecordsView;
    mSourceSummary: THTMemo;
    ListRepositories: TRecordsView;
    mRepositorySummary: THTMemo;
    ListGroups: TRecordsView;
    mGroupSummary: THTMemo;
    ListResearches: TRecordsView;
    mResearchSummary: THTMemo;
    ListTasks: TRecordsView;
    mTaskSummary: THTMemo;
    ListCommunications: TRecordsView;
    mCommunicationSummary: THTMemo;
    ListLocations: TRecordsView;
    mLocationSummary: THTMemo;
    //

    FCounts: array [TGEDCOMRecordType] of TRecCount;

    procedure ApplyFilter();
    procedure ChangeRecord(aRecord: TGEDCOMRecord);
    procedure ChangesClear();
    function  CheckModified: Boolean;
    procedure Clear();
    procedure CreateListView(aOwner: TComponent; aParent: TWinControl; var aList: TGKListView);
    procedure CreatePage(aPage: TTabSheet; aRecType: TGEDCOMRecordType;
      var aList: TRecordsView; var aSummary: THTMemo);
    function  CreatePersonDialog(aTarget: TGEDCOMIndividualRecord;
      aTargetMode: TTargetMode; aNeedSex: TGEDCOMSex): TGEDCOMIndividualRecord;
    procedure CreateRecordsView(aOwner: TComponent; aParent: TWinControl;
      aRecordType: TGEDCOMRecordType; var aList: TRecordsView);
    procedure ImportDB();
    procedure ExportToExcel();
    procedure ExportToWeb();
    procedure FileLoad(aFileName: string);
    procedure FileNew();
    function  FileProperties(aMode: TFilePropertiesMode = fpmAuthor): Integer;
    procedure FileSave(const aFileName: string);
    procedure GenPedigree_dAboville();
    procedure GenPedigree_Konovalov();
    function  GetChildFamily(iChild: TGEDCOMIndividualRecord; aCanCreate: Boolean;
      aNewParent: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;
    function  GetCurFileTempPath(): string;
    function  GenRecordLink(aRecord: TGEDCOMRecord; aSigned: Boolean = True): string;
    function  GetSelectedPerson(): TGEDCOMIndividualRecord;
    procedure ListsRefresh(aTitles: Boolean = False);
    procedure NavNext();
    procedure NavPrev();
    procedure PersonScan();
    procedure RecordAdd();
    procedure RecordDelete();
    procedure RecordEdit(Sender: TObject);
    procedure SearchSubjectLinks(aInRecord, aSubject: TGEDCOMRecord; aToList: TStrings);
    function  SelectFamily(aTarget: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;
    function  SelectPerson(aTarget: TGEDCOMIndividualRecord; aTargetMode: TTargetMode;
      aNeedSex: TGEDCOMSex): TGEDCOMIndividualRecord;
    function  SelectRecord(aMode: TSelectMode; anArgs: array of const): TGEDCOMRecord;
    procedure SelectRecordByXRef(XRef: string);
    procedure SetFilter();
    procedure ShowMap();
    procedure ShowMedia(aMediaRec: TGEDCOMMultimediaRecord);
    procedure ShowOrganizer();
    procedure ShowStats();
    procedure ShowStereoView();
    procedure ShowTips();
    procedure ShowTreeAncestors();
    procedure ShowTreeDescendants();
    procedure TreeTools();

    function CheckPath(): Boolean;
    function GetArcFileName(): string;
    function GetStoreFolder(): string;
    function GetExtName(): string;
    function GetStoreType(aFileRef: string; var aFileName: string): TGKStoreType;
    function IsAdvanced(): Boolean;
    procedure MediaLoad(aRefName: string; var aStream: TStream); overload;
    procedure MediaLoad(aRefName: string; var aFileName: string); overload;
    procedure MediaSave(aFileName: string; aStoreType: TGKStoreType; var aRefPath: string);

    function GroupMemberAdd(aGroup: TGEDCOMGroupRecord; aMember: TGEDCOMIndividualRecord): Boolean;
    function GroupMemberRemove(aGroup: TGEDCOMGroupRecord; aMember: TGEDCOMIndividualRecord): Boolean;

    function FamilyChildAdd(aFamily: TGEDCOMFamilyRecord; aChild: TGEDCOMIndividualRecord): Boolean;
    function FamilyChildRemove(aFamily: TGEDCOMFamilyRecord; aChild: TGEDCOMIndividualRecord): Boolean;

    function DeleteFamilyRecord(aFamily: TGEDCOMFamilyRecord; aConfirm: Boolean): Boolean;
    function DeleteGroupRecord(groupRec: TGEDCOMGroupRecord; aConfirm: Boolean): Boolean;
    function DeleteIndividualRecord(iRec: TGEDCOMIndividualRecord; aConfirm: Boolean): Boolean;
    function DeleteMediaRecord(mRec: TGEDCOMMultimediaRecord; aConfirm: Boolean): Boolean;
    function DeleteNoteRecord(nRec: TGEDCOMNoteRecord; aConfirm: Boolean): Boolean;
    function DeleteRepositoryRecord(repRec: TGEDCOMRepositoryRecord; aConfirm: Boolean): Boolean;
    function DeleteResearchRecord(resRec: TGEDCOMResearchRecord; aConfirm: Boolean): Boolean;
    function DeleteSourceRecord(srcRec: TGEDCOMSourceRecord; aConfirm: Boolean): Boolean;

    function DeleteTaskRecord(TaskRec: TGEDCOMTaskRecord; aConfirm: Boolean): Boolean;
    function DeleteCommunicationRecord(ComRec: TGEDCOMCommunicationRecord; aConfirm: Boolean): Boolean;
    function DeleteLocationRecord(LocRec: TGEDCOMLocationRecord; aConfirm: Boolean): Boolean;

    procedure DoUndo();
    procedure DoRedo();
    procedure DoPersonChangeSex(aPerson: TGEDCOMIndividualRecord; NewSex: TGEDCOMSex);
    procedure DoPersonChangePatriarch(aPerson: TGEDCOMIndividualRecord; NewValue: Boolean);

    function ModifyPerson(var aIndivRec: TGEDCOMIndividualRecord): Boolean;
    function ModifyFamily(var aFamilyRec: TGEDCOMFamilyRecord;
      aTarget: TFamilyTarget = ftNone; aPerson: TGEDCOMIndividualRecord = nil): Boolean;
    function ModifyNote(var aNoteRec: TGEDCOMNoteRecord): Boolean;
    function ModifyMedia(var aMediaRec: TGEDCOMMultimediaRecord): Boolean;
    function ModifySource(var aSourceRec: TGEDCOMSourceRecord): Boolean;
    function ModifyRepository(var aRepRec: TGEDCOMRepositoryRecord): Boolean;
    function ModifyGroup(var aGroupRec: TGEDCOMGroupRecord): Boolean;
    function ModifyResearch(var aResearchRec: TGEDCOMResearchRecord): Boolean;
    function ModifyTask(var aTaskRec: TGEDCOMTaskRecord): Boolean;
    function ModifyCommunication(var aCommunicationRec: TGEDCOMCommunicationRecord): Boolean;
    function ModifyLocation(var aLocationRec: TGEDCOMLocationRecord): Boolean;

    function ModifyAddress(aSender: TForm; anAddress: TGEDCOMAddress): Boolean;

    function ModifyRecAssociation(aSender: TForm; aRecord: TGEDCOMIndividualRecord; aAssociation: TGEDCOMAssociation; anAction: TRecAction): Boolean;
    function ModifyRecEvent(aSender: TForm; aRecord: TGEDCOMRecord; aEvent: TGEDCOMCustomEvent; anAction: TRecAction): Boolean;
    function ModifyRecMultimedia(aSender: TForm; aRecord: TGEDCOMRecord; aLink: TGEDCOMMultimediaLink; anAction: TRecAction): Boolean;
    function ModifyRecNote(aSender: TForm; aRecord: TGEDCOMRecord; aNote: TGEDCOMNotes; anAction: TRecAction): Boolean;
    function ModifyRecSource(aSender: TForm; aRecord: TGEDCOMRecord; aCit: TGEDCOMSourceCitation; anAction: TRecAction): Boolean;
    function ModifyRecUserRef(aSender: TForm; aRecord: TGEDCOMRecord; aUserRef: TGEDCOMUserReference; anAction: TRecAction): Boolean;

    function ModifyTagMultimedia(aTag: TGEDCOMTagWithLists;
      aLink: TGEDCOMMultimediaLink; anAction: TRecAction): Boolean;
    function ModifyTagNote(aTag: TGEDCOMTagWithLists;
      aNote: TGEDCOMNotes; anAction: TRecAction): Boolean;
    function ModifyTagSource(aTag: TGEDCOMTagWithLists;
      aCit: TGEDCOMSourceCitation; anAction: TRecAction): Boolean;

    procedure RecListAssociationsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListFamilyEventsRefresh(aRecord: TGEDCOMFamilyRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListGroupsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListIndividualEventsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListMediaRefresh(aRecord: TGEDCOMRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListNotesRefresh(aRecord: TGEDCOMRecord; aList: TCustomListControl; aSummary: TStrings);
    procedure RecListSourcesRefresh(aRecord: TGEDCOMRecord; aList: TBSListView; aSummary: TStrings);

    procedure SetupRecEventsList(aList: TSheetList; PersonsMode: Boolean);
    procedure SetupRecMediaList(aList: TSheetList);
    procedure SetupRecNotesList(aList: TSheetList);
    procedure SetupRecSourcesList(aList: TSheetList);

    procedure ShowDetailCause(aDetail: TGEDCOMEventDetail; aSummary: TStrings);
    procedure ShowDetailInfo(aDetail: TGEDCOMEventDetail; aSummary: TStrings);
    procedure ShowFamilyInfo(aFamily: TGEDCOMFamilyRecord; aSummary: TStrings);
    procedure ShowGroupInfo(aGroup: TGEDCOMGroupRecord; aSummary: TStrings);
    procedure ShowMultimediaInfo(aMultimediaRec: TGEDCOMMultimediaRecord; aSummary: TStrings);
    procedure ShowNoteInfo(aNoteRec: TGEDCOMNoteRecord; aSummary: TStrings);
    procedure ShowPersonInfo(iRec: TGEDCOMIndividualRecord; aSummary: TStrings);
    procedure ShowSourceInfo(aSourceRec: TGEDCOMSourceRecord; aSummary: TStrings);
    procedure ShowRepositoryInfo(aRepositoryRec: TGEDCOMRepositoryRecord; aSummary: TStrings);
    procedure ShowResearchInfo(aResearchRec: TGEDCOMResearchRecord; aSummary: TStrings);
    procedure ShowTaskInfo(aTaskRec: TGEDCOMTaskRecord; aSummary: TStrings);
    procedure ShowCommunicationInfo(aCommunicationRec: TGEDCOMCommunicationRecord; aSummary: TStrings);
    procedure ShowLocationInfo(aLocationRec: TGEDCOMLocationRecord; aSummary: TStrings);

    procedure TimeLine_Init();
    procedure TimeLine_Done();
    function  TimeLine_GetYear(): Integer;
    procedure TimeLine_SetYear(aYear: Integer);

    property Backman: TBackManager read FBackman;
    property FileName: string read FFileName write SetFileName;
    property Filter: TPersonsFilter read FXFilter;
    property Modified: Boolean read FModified write SetModified;
    property ShieldState: TShieldState read FShieldState write SetShieldState;
    property Tree: TGEDCOMTree read FTree;
    property Undoman: TUndoManager read FUndoman;
  end;

implementation

uses
  {$IFDEF DELPHI_NET}System.IO, {$ENDIF}
  {$IFDEF PROFILER}ZProfiler, {$ENDIF}
  {$IFNDEF DELPHI_NET}AbZipper, AbZipTyp, AbZipKit, AbArcTyp, GKMaps, GKMediaView, {$ENDIF}
  Types, IniFiles, bsComUtils, bsWinUtils, GKPersonNew, GKRecordSelect, GKStats,
  GKNoteEdit, GKChart, GKSourceEdit, GKEventEdit, GKAbout, GKChartCore,
  GKFileProperties, GKPersonEdit, GKExport, GKOptions, GKFamilyEdit,
  GKAssociationEdit, GKFilter, GKTreeTools, GKGroupEdit, GKPersonScan, GKMain,
  GKProgress, GKSourceCitEdit, GKRepositoryEdit, GKMediaEdit, Clipbrd,
  bsMiscUtils, GKResearchEdit, GKTaskEdit, GKCommunicationEdit, GKLocationEdit,
  GKCommands, GKTipsDlg, GKUserRefEdit, GKStereoView, GKTimeLine,
  GKOrganizer, GKDBImport, GKAddressEdit;

{$R *.dfm}

{ TfmGEDKeeper }

procedure TfmBase.FormCreate(Sender: TObject);
var
  rt: TGEDCOMRecordType;
begin
  for rt := Low(TGEDCOMRecordType) to High(TGEDCOMRecordType) do
    FChangedRecords[rt] := TList.Create;

  FTree := TGEDCOMTree.Create;
  FXFilter := TPersonsFilter.Create;

  FLockedRecords := TList.Create;

  FBackman := TBackManager.Create;
  FUndoman := TUndoManager.Create(FTree, manualCommit);
  
  CreatePage(SheetPersons, rtIndividual, ListPersons, mPersonSummary);
  CreatePage(SheetFamilies, rtFamily, ListFamilies, mFamilySummary);
  CreatePage(SheetNotes, rtNote, ListNotes, mNoteSummary);
  CreatePage(SheetMultimedia, rtMultimedia, ListMultimedia, mMediaSummary);
  CreatePage(SheetSources, rtSource, ListSources, mSourceSummary);
  CreatePage(SheetRepositories, rtRepository, ListRepositories, mRepositorySummary);
  CreatePage(SheetGroups, rtGroup, ListGroups, mGroupSummary);
  CreatePage(SheetResearches, rtResearch, ListResearches, mResearchSummary);
  CreatePage(SheetTasks, rtTask, ListTasks, mTaskSummary);
  CreatePage(SheetCommunications, rtCommunication, ListCommunications, mCommunicationSummary);
  CreatePage(SheetLocations, rtLocation, ListLocations, mLocationSummary);

  PageRecords.ActivePage := SheetPersons;
end;

procedure TfmBase.FormDestroy(Sender: TObject);
var
  rt: TGEDCOMRecordType;
begin
  ListPersons.Clear;

  FBackman.Destroy;
  FUndoman.Destroy;

  FLockedRecords.Destroy;

  FXFilter.Free;
  FTree.Destroy;

  for rt := Low(TGEDCOMRecordType) to High(TGEDCOMRecordType) do
    FChangedRecords[rt].Free;

  fmGEDKeeper.UpdateControls(True);
end;

procedure TfmBase.CreateListView(aOwner: TComponent; aParent: TWinControl; var aList: TGKListView);
begin
  aList := TGKListView.Create(Self);
  with aList do begin
    Parent := aParent;
    Align := alClient;
    HideSelection := False;
    ReadOnly := True;
    RowSelect := True;
    SortType := stText;
    SortColumn := 0;
    SortDirection := sdAscending;
    ShowSortSign := True;
    ViewStyle := vsReport;
  end;
end;

procedure TfmBase.CreateRecordsView(aOwner: TComponent; aParent: TWinControl;
  aRecordType: TGEDCOMRecordType; var aList: TRecordsView);
begin
  aList := TRecordsView.Create(Self);
  with aList do begin
    Parent := aParent;
    Align := alClient;
    HideSelection := False;
    ReadOnly := True;
    ViewStyle := vsReport;
    RowSelect := True;
    SortType := stData{stText};
    SortColumn := 0;
    SortDirection := sdAscending;
    ShowSortSign := True;

    Tree := FTree;
    RecordType := aRecordType;
  end;
end;

procedure TfmBase.CreatePage(aPage: TTabSheet; aRecType: TGEDCOMRecordType;
  var aList: TRecordsView; var aSummary: THTMemo);
begin
  aSummary := THTMemo.Create(Self);
  with aSummary do begin
    Parent := aPage;
    BorderWidth := 4;
    Width := 400;
    Font.Name := 'Tahoma';
    Align := alRight;

    OnLink := mPersonSummaryLink;
  end;

  with TSplitter.Create(Self) do begin
    Parent := aPage;
    Width := 4;
    Align := alRight;
    Beveled := True;
  end;

  CreateRecordsView(Self, aPage, aRecType, aList);
  aList.IsMainList := IsMainList(aRecType, aList);
  aList.OnDblClick := RecordEdit;
  aList.OnSelectItem := ListSelectItem;
  aList.UpdateTitles();
end;

function TfmBase.CheckModified(): Boolean;
begin
  Result := True;

  if Modified then begin
    case MessageDlg('Файл изменен. Сохранить?', mtWarning, [mbYes, mbNo, mbCancel], 0) of
      mrYes: fmGEDKeeper.actFileSaveExecute(nil);
      mrNo: {dummy};
      mrCancel: Result := False;
    end;
  end;
end;

procedure TfmBase.Clear();
begin
  FTree.Clear();
  FBackman.Clear();
  FUndoman.Clear();
end;

procedure TfmBase.SelectRecordByXRef(XRef: string);

  procedure SelectItemByRec(aList: TRecordsView; aRec: TGEDCOMRecord; aTab: Integer);
  begin
    PageRecords.TabIndex := aTab;
    PageRecordsChange(nil);
    ActiveControl := aList;
    aList.SelectItemByRec(aRec);
  end;

var
  rec: TGEDCOMRecord;
begin
  rec := FTree.XRefIndex_Find(XRef);

  if (rec is TGEDCOMIndividualRecord)
  then SelectItemByRec(ListPersons, rec, 0)
  else
  if (rec is TGEDCOMFamilyRecord)
  then SelectItemByRec(ListFamilies, rec, 1)
  else
  if (rec is TGEDCOMNoteRecord)
  then SelectItemByRec(ListNotes, rec, 2)
  else
  if (rec is TGEDCOMMultimediaRecord)
  then SelectItemByRec(ListMultimedia, rec, 3)
  else
  if (rec is TGEDCOMSourceRecord)
  then SelectItemByRec(ListSources, rec, 4)
  else
  if (rec is TGEDCOMRepositoryRecord)
  then SelectItemByRec(ListRepositories, rec, 5)
  else
  if (rec is TGEDCOMGroupRecord)
  then SelectItemByRec(ListGroups, rec, 6)
  else
  if (rec is TGEDCOMResearchRecord)
  then SelectItemByRec(ListResearches, rec, 7)
  else
  if (rec is TGEDCOMTaskRecord)
  then SelectItemByRec(ListTasks, rec, 8)
  else
  if (rec is TGEDCOMCommunicationRecord)
  then SelectItemByRec(ListCommunications, rec, 9)
  else
  if (rec is TGEDCOMLocationRecord)
  then SelectItemByRec(ListLocations, rec, 10)
  else ;
end;

function TfmBase.GetChildFamily(iChild: TGEDCOMIndividualRecord; aCanCreate: Boolean;
  aNewParent: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;

  function GetFamilyBySpouse(): TGEDCOMFamilyRecord;
  var
    i: Integer;
    fam: TGEDCOMFamilyRecord;
    husb, wife: TGEDCOMIndividualRecord;
    msg: string;
  begin
    Result := nil;

    for i := 0 to FTree.RecordsCount - 1 do begin
      if (FTree.Records[i] is TGEDCOMFamilyRecord) then begin
        fam := (FTree.Records[i] as TGEDCOMFamilyRecord);

        husb := TGEDCOMIndividualRecord(fam.Husband.Value);
        wife := TGEDCOMIndividualRecord(fam.Wife.Value);

        if (husb = aNewParent) or (wife = aNewParent) then begin
          msg := 'У заданного родителя найдена семья "' + GetFamilyStr(fam) + '". '
            + #13#10'Включить ребенка в эту семью?';

          if (MessageDlg(msg, mtWarning, [mbYes, mbNo], 0) = mrYes) then begin
            Result := fam;
            Exit;
          end;
        end;
      end;
    end;
  end;

var
  fam: TGEDCOMFamilyRecord;
begin
  Result := nil;
  if (iChild = nil) then Exit;

  if (iChild.ChildToFamilyLinksCount <> 0)
  then Result := iChild.ChildToFamilyLinks[0].Family
  else begin
    if (aCanCreate) then begin
      fam := GetFamilyBySpouse();

      if (fam = nil)
      then fam := CreateFamilyEx(FTree);

      FamilyChildAdd(fam, iChild);

      Result := fam;
    end;
  end;
end;

function TfmBase.FamilyChildAdd(aFamily: TGEDCOMFamilyRecord;
  aChild: TGEDCOMIndividualRecord): Boolean;
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

function TfmBase.FamilyChildRemove(aFamily: TGEDCOMFamilyRecord;
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

procedure TfmBase.CleanFamily(aFamily: TGEDCOMFamilyRecord);
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
  RemoveFamilySpouse(FTree, aFamily, spouse);

  spouse := TGEDCOMIndividualRecord(aFamily.Wife.Value);
  RemoveFamilySpouse(FTree, aFamily, spouse);
end;

function TfmBase.DeleteFamilyRecord(aFamily: TGEDCOMFamilyRecord; aConfirm: Boolean): Boolean;
begin
  Result := False;
  if (aFamily = nil) then Exit;
  if (aConfirm) and (MessageDlg('Удалить семью "'+GetFamilyStr(aFamily)+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  CleanFamily(aFamily);
  FTree.Delete(FTree.IndexOfRecord(aFamily));
  Modified := True;

  Result := True;
end;

function TfmBase.DeleteIndividualRecord(iRec: TGEDCOMIndividualRecord; aConfirm: Boolean): Boolean;
var
  i: Integer;
  family: TGEDCOMFamilyRecord;
begin
  Result := False;
  if (iRec = nil) then Exit;
  if (aConfirm) and (MessageDlg('Удалить персональную запись "'+GetNameStr(iRec)+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  // могут быть также ссылки в группах, задачах и коммуникациях

  for i := iRec.ChildToFamilyLinksCount - 1 downto 0 do begin
    family := iRec.ChildToFamilyLinks[i].Family;
    family.DeleteChild(iRec);
  end;

  for i := iRec.SpouseToFamilyLinksCount - 1 downto 0 do begin
    family := iRec.SpouseToFamilyLinks[i].Family;
    RemoveFamilySpouse(FTree, family, iRec);
  end;

  FTree.Delete(FTree.IndexOfRecord(iRec));
  Modified := True;

  Result := True;
end;

function TfmBase.DeleteNoteRecord(nRec: TGEDCOMNoteRecord; aConfirm: Boolean): Boolean;
var
  i, k: Integer;
  rec: TGEDCOMRecord;
begin
  {if not(RequestModify(nRec)) then begin
    ShowLockMsg();
    Exit;
  end;}

  Result := False;
  if (nRec = nil) then Exit;
  if (aConfirm) and (MessageDlg('Удалить заметку?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  // могут быть также ссылки в тэгах

  for i := 0 to FTree.RecordsCount - 1 do begin
    rec := FTree.Records[i];

    for k := rec.NotesCount - 1 downto 0 do begin
      if (rec.Notes[k].Value = nRec)
      then rec.DeleteNotes(k);
    end;
  end;

  FTree.Delete(FTree.IndexOfRecord(nRec));
  Modified := True;

  Result := True;
end;

function TfmBase.DeleteSourceRecord(srcRec: TGEDCOMSourceRecord; aConfirm: Boolean): Boolean;
var
  i, k: Integer;
  rec: TGEDCOMRecord;
begin
  Result := False;
  if (srcRec = nil) then Exit;
  if (aConfirm) and (MessageDlg('Удалить источник "'+srcRec.FiledByEntry+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  // могут быть также ссылки в тэгах

  for i := 0 to FTree.RecordsCount - 1 do begin
    rec := FTree.Records[i];

    for k := rec.SourceCitationsCount - 1 downto 0 do begin
      if (rec.SourceCitations[k].Value = srcRec)
      then rec.DeleteSourceCitation(k);
    end;
  end;

  FTree.Delete(FTree.IndexOfRecord(srcRec));
  Modified := True;

  Result := True;
end;

function TfmBase.DeleteMediaRecord(mRec: TGEDCOMMultimediaRecord; aConfirm: Boolean): Boolean;
var
  i, k: Integer;
  rec: TGEDCOMRecord;
begin
  Result := False;
  if (mRec = nil) then Exit;
  if (aConfirm) and (MessageDlg('Удалить мультимедиа "'+mRec.StringValue+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  // могут быть также ссылки в тэгах

  for i := 0 to FTree.RecordsCount - 1 do begin
    rec := FTree.Records[i];

    for k := rec.MultimediaLinksCount - 1 downto 0 do begin
      if (rec.MultimediaLinks[k].Value = mRec)
      then rec.DeleteMultimediaLink(k);
    end;
  end;

  FTree.Delete(FTree.IndexOfRecord(mRec));
  Modified := True;

  Result := True;
end;

function TfmBase.DeleteRepositoryRecord(repRec: TGEDCOMRepositoryRecord; aConfirm: Boolean): Boolean;
var
  i, k: Integer;
  rec: TGEDCOMRecord;
  srcRec: TGEDCOMSourceRecord;
begin
  Result := False;
  if (repRec = nil) then Exit;
  if (aConfirm) and (MessageDlg('Удалить архив "'+repRec.RepositoryName+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  for i := 0 to FTree.RecordsCount - 1 do begin
    rec := FTree.Records[i];

    if (rec is TGEDCOMSourceRecord) then begin
      srcRec := (rec as TGEDCOMSourceRecord);

      for k := srcRec.RepositoryCitationsCount - 1 downto 0 do begin
        if (srcRec.RepositoryCitations[k].Value = repRec)
        then srcRec.DeleteRepositoryCitation(srcRec.RepositoryCitations[k]);
      end;
    end;
  end;

  FTree.Delete(FTree.IndexOfRecord(repRec));
  Modified := True;

  Result := True;
end;

function TfmBase.DeleteGroupRecord(groupRec: TGEDCOMGroupRecord; aConfirm: Boolean): Boolean;
var
  i: Integer;
  member: TGEDCOMIndividualRecord;
begin
  Result := False;
  if (groupRec = nil) then Exit;
  if (aConfirm) and (MessageDlg('Удалить группу "'+groupRec.Name+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  for i := 0 to groupRec.MembersCount - 1 do begin
    member := TGEDCOMIndividualRecord(groupRec.Members[i].Value);
    member.DeleteGroup(member.IndexOfGroup(groupRec));
  end;

  FTree.Delete(FTree.IndexOfRecord(groupRec));
  Modified := True;

  Result := True;
end;

function TfmBase.DeleteResearchRecord(resRec: TGEDCOMResearchRecord; aConfirm: Boolean): Boolean;
begin
  Result := False;
  if (resRec = nil) then Exit;
  if (aConfirm) and (MessageDlg('Удалить исследование "'+resRec.Name+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  // dummy, because there is not links from other records and tags

  FTree.Delete(FTree.IndexOfRecord(resRec));
  Modified := True;

  Result := True;
end;

function TfmBase.DeleteTaskRecord(TaskRec: TGEDCOMTaskRecord; aConfirm: Boolean): Boolean;
var
  i, k: Integer;
  resRec: TGEDCOMResearchRecord;
  rec: TGEDCOMRecord;
begin
  Result := False;
  if (TaskRec = nil) then Exit;
  if (aConfirm) and (MessageDlg('Удалить задачу "'+GetTaskGoalStr(FTree, TaskRec)+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  for i := 0 to FTree.RecordsCount - 1 do begin
    rec := FTree.Records[i];

    if (rec is TGEDCOMResearchRecord) then begin
      resRec := (rec as TGEDCOMResearchRecord);

      for k := resRec.TasksCount - 1 downto 0 do begin
        if (resRec.Tasks[k].Value = TaskRec)
        then resRec.DeleteTask(k);
      end;
    end;
  end;

  FTree.Delete(FTree.IndexOfRecord(TaskRec));
  Modified := True;

  Result := True;
end;

function TfmBase.DeleteCommunicationRecord(ComRec: TGEDCOMCommunicationRecord; aConfirm: Boolean): Boolean;
var
  i, k: Integer;
  resRec: TGEDCOMResearchRecord;
  rec: TGEDCOMRecord;
begin
  Result := False;
  if (ComRec = nil) then Exit;
  if (aConfirm) and (MessageDlg('Удалить коммуникацию "'+ComRec.Name+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  for i := 0 to FTree.RecordsCount - 1 do begin
    rec := FTree.Records[i];

    if (rec is TGEDCOMResearchRecord) then begin
      resRec := (rec as TGEDCOMResearchRecord);

      for k := resRec.CommunicationsCount - 1 downto 0 do begin
        if (resRec.Communications[k].Value = ComRec)
        then resRec.DeleteCommunication(k);
      end;
    end;
  end;

  FTree.Delete(FTree.IndexOfRecord(ComRec));
  Modified := True;

  Result := True;
end;

function TfmBase.DeleteLocationRecord(LocRec: TGEDCOMLocationRecord; aConfirm: Boolean): Boolean;
var
  i, k: Integer;
  iRec: TGEDCOMIndividualRecord;
  fRec: TGEDCOMFamilyRecord;
  rec: TGEDCOMRecord;
  ev: TGEDCOMCustomEvent;
begin
  Result := False;
  if (LocRec = nil) then Exit;
  if (aConfirm) and (MessageDlg('Удалить место "'+LocRec.Name+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  for i := 0 to FTree.RecordsCount - 1 do begin
    rec := FTree.Records[i];

    if (rec is TGEDCOMIndividualRecord) then begin
      iRec := (rec as TGEDCOMIndividualRecord);

      for k := iRec.IndividualEventsCount - 1 downto 0 do begin
        ev := iRec.IndividualEvents[k];
        if (ev.Detail.Place.Location.Value = LocRec)
        then ev.Detail.Place.DeleteTag('_LOC');
      end;

      for k := iRec.IndividualAttributesCount - 1 downto 0 do begin
        ev := iRec.IndividualAttributes[k];
        if (ev.Detail.Place.Location.Value = LocRec)
        then ev.Detail.Place.DeleteTag('_LOC');
      end;
    end
    else
    if (rec is TGEDCOMFamilyRecord) then begin
      fRec := (rec as TGEDCOMFamilyRecord);

      for k := fRec.FamilyEventCount - 1 downto 0 do begin
        ev := fRec.FamilyEvents[k];
        if (ev.Detail.Place.Location.Value = LocRec)
        then ev.Detail.Place.DeleteTag('_LOC');
      end;
    end;
  end;

  FTree.Delete(FTree.IndexOfRecord(LocRec));
  Modified := True;

  Result := True;
end;

procedure TfmBase.ListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  data: TObject;
begin
  {if (Item = nil) or not(Selected)
  then data := nil
  else data := TObject(Item.Data);}

  data := GetSelectedRecord(TCustomListView(Sender));

  NavAdd(TGEDCOMRecord(data));

  if (Sender = ListPersons)
  then ShowPersonInfo(TGEDCOMIndividualRecord(data), mPersonSummary.Lines)
  else
  if (Sender = ListFamilies)
  then ShowFamilyInfo(TGEDCOMFamilyRecord(data), mFamilySummary.Lines)
  else
  if (Sender = ListNotes)
  then ShowNoteInfo(TGEDCOMNoteRecord(data), mNoteSummary.Lines)
  else
  if (Sender = ListMultimedia)
  then ShowMultimediaInfo(TGEDCOMMultimediaRecord(data), mMediaSummary.Lines)
  else
  if (Sender = ListSources)
  then ShowSourceInfo(TGEDCOMSourceRecord(data), mSourceSummary.Lines)
  else
  if (Sender = ListRepositories)
  then ShowRepositoryInfo(TGEDCOMRepositoryRecord(data), mRepositorySummary.Lines)
  else
  if (Sender = ListGroups)
  then ShowGroupInfo(TGEDCOMGroupRecord(data), mGroupSummary.Lines)
  else
  if (Sender = ListResearches)
  then ShowResearchInfo(TGEDCOMResearchRecord(data), mResearchSummary.Lines)
  else
  if (Sender = ListTasks)
  then ShowTaskInfo(TGEDCOMTaskRecord(data), mTaskSummary.Lines)
  else
  if (Sender = ListCommunications)
  then ShowCommunicationInfo(TGEDCOMCommunicationRecord(data), mCommunicationSummary.Lines)
  else
  if (Sender = ListLocations)
  then ShowLocationInfo(TGEDCOMLocationRecord(data), mLocationSummary.Lines);
end;

procedure TfmBase.ShowAddress(anAddress: TGEDCOMAddress; aSummary: TStrings);
var
  k: Integer;
  ts: string;
begin
  if not(anAddress.IsEmpty) and (aSummary <> nil) then begin
    aSummary.Add('    Адрес:');

    ts := '';
    if (anAddress.AddressCountry <> '')
    then ts := ts + anAddress.AddressCountry + ', ';
    if (anAddress.AddressState <> '')
    then ts := ts + anAddress.AddressState + ', ';
    if (anAddress.AddressCity <> '')
    then ts := ts + anAddress.AddressCity;

    if (ts <> '')
    then aSummary.Add('    ' + ts);

    ts := '';
    if (anAddress.AddressPostalCode <> '')
    then ts := ts + anAddress.AddressPostalCode + ', ';
    if (Trim(anAddress.Address.Text) <> '')
    then ts := ts + Trim(anAddress.Address.Text);

    if (ts <> '')
    then aSummary.Add('    ' + ts);

    for k := 0 to anAddress.PhoneNumbersCount - 1 do
      aSummary.Add('    ' + anAddress.PhoneNumbers[k]);

    for k := 0 to anAddress.EmailAddressesCount - 1 do
      aSummary.Add('    ' + anAddress.EmailAddresses[k]);

    for k := 0 to anAddress.WebPagesCount - 1 do
      aSummary.Add('    ' + anAddress.WebPages[k]);
  end;
end;

procedure TfmBase.ShowDetailInfo(aDetail: TGEDCOMEventDetail; aSummary: TStrings);
var
  idx: Integer;
  cit: TGEDCOMSourceCitation;
  sourceRec: TGEDCOMSourceRecord;
  nm: string;
begin
  if (aSummary <> nil) and (aDetail.SourceCitationsCount <> 0) then begin
    aSummary.Add('    Источники (' + IntToStr(aDetail.SourceCitationsCount) + '):');
    for idx := 0 to aDetail.SourceCitationsCount - 1 do begin
      cit := aDetail.SourceCitations[idx];
      sourceRec := TGEDCOMSourceRecord(cit.Value);
      if (sourceRec <> nil) then begin
        nm := '"'+sourceRec.FiledByEntry+'"';
        if (cit.Page <> '') then nm := nm + ', ' + cit.Page;

        aSummary.Add('      ' + HyperLink(sourceRec.XRef, nm));
      end;
    end;
  end;
end;

procedure TfmBase.ShowDetailCause(aDetail: TGEDCOMEventDetail; aSummary: TStrings);
var
  cause: string;
begin
  cause := GetEventCause(aDetail);
  if (aSummary <> nil) and (cause <> '')
  then aSummary.Add('    ' + cause);
end;

procedure TfmBase.RecListIndividualEventsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
var
  idx, ev: Integer;
  event: TGEDCOMIndividualEvent;
  attr: TGEDCOMIndividualAttribute;
  st: string;
  item: TListItem;
begin
  try
    if (aList <> nil)
    then aList.Clear();

    if (aRecord.IndividualEventsCount <> 0) or (aRecord.IndividualAttributesCount <> 0)
    then begin
      if (aSummary <> nil) then begin
        aSummary.Add('');
        aSummary.Add({#13#10}'Факты:');
      end;

      for idx := 0 to aRecord.IndividualEventsCount - 1 do begin
        event := aRecord.IndividualEvents[idx];

        ev := GetPersonEventIndex(event.Name);
        if (ev = 0) then st := event.Detail.Classification
        else
        if (ev > 0) then st := PersonEvents[ev].Name
        else st := event.Name;

        if (aSummary <> nil) then begin
          aSummary.Add(st + ': ' + GetEventDesc(event.Detail));
          ShowDetailCause(event.Detail, aSummary);
          ShowAddress(event.Detail.Address, aSummary);
          ShowDetailInfo(event.Detail, aSummary);
        end;

        if (aList <> nil) then begin
          item := aList.Items.Add();
          item.Caption := st;
          item.SubItems.Add(GEDCOMCustomDateToStr(event.Detail.Date.Value, fmGEDKeeper.Options.DefDateFormat));
          item.SubItems.Add(event.Detail.Place.StringValue);
          item.SubItems.Add(GetEventCause(event.Detail));
          item.Data := event;
        end;
      end;

      for idx := 0 to aRecord.IndividualAttributesCount - 1 do begin
        attr := aRecord.IndividualAttributes[idx];

        ev := GetPersonEventIndex(attr.Name);
        if (ev = 0) then st := attr.Detail.Classification
        else
        if (ev > 0) then st := PersonEvents[ev].Name
        else st := attr.Name;

        if (aSummary <> nil) then begin
          aSummary.Add(st + ': ' + GetEventDesc(attr.Detail));
          if (attr.StringValue <> '') then aSummary.Add('    ' + attr.StringValue);
          ShowDetailCause(attr.Detail, aSummary);
          ShowAddress(attr.Detail.Address, aSummary);
          ShowDetailInfo(attr.Detail, aSummary);
        end;

        if (aList <> nil) then begin
          item := aList.Items.Add();
          item.Caption := st;
          item.SubItems.Add(GEDCOMCustomDateToStr(attr.Detail.Date.Value, fmGEDKeeper.Options.DefDateFormat));

          st := attr.Detail.Place.StringValue;
          if (attr.StringValue <> '')
          then st := st + ' [' + attr.StringValue + ']';

          item.SubItems.Add(st);
          item.SubItems.Add(GetEventCause(attr.Detail));
          item.Data := attr;
        end;
      end;

      if (aList <> nil) then begin
        //ResizeColumn(aList, 2);
        aList.SortColumn := 1;
      end;
    end;
  except
    on E: Exception do LogWrite('RecListIndividualEventsRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.RecListFamilyEventsRefresh(aRecord: TGEDCOMFamilyRecord; aList: TBSListView; aSummary: TStrings);
var
  idx, ev: Integer;
  event: TGEDCOMFamilyEvent;
  st: string;
  item: TListItem;
begin
  try
    if (aList <> nil)
    then aList.Clear();

    if (aRecord.FamilyEventCount <> 0) then begin
      if (aSummary <> nil) then begin
        aSummary.Add('');
        aSummary.Add({#13#10}'События:');
      end;

      for idx := 0 to aRecord.FamilyEventCount - 1 do begin
        event := aRecord.FamilyEvents[idx];

        ev := GetFamilyEventIndex(event.Name);
        if (ev = 0) then st := event.Detail.Classification
        else
        if (ev > 0) then st := FamilyEvents[ev].Name
        else st := event.Name;

        if (aSummary <> nil) then begin
          aSummary.Add(st + ': ' + GetEventDesc(event.Detail));
          ShowDetailCause(event.Detail, aSummary);
        end;

        ShowDetailInfo(event.Detail, aSummary);

        if (aList <> nil) then begin
          item := aList.Items.Add();
          item.Caption := st;
          item.SubItems.Add(GEDCOMCustomDateToStr(event.Detail.Date.Value, fmGEDKeeper.Options.DefDateFormat));
          item.SubItems.Add(event.Detail.Place.StringValue);
          item.SubItems.Add(GetEventCause(event.Detail));
          item.Data := event;
        end;
      end;
    end;
  except
    on E: Exception do LogWrite('RecListFamilyEventsRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.RecListNotesRefresh(aRecord: TGEDCOMRecord; aList: TCustomListControl; aSummary: TStrings);
var
  idx, k: Integer;
  note: TGEDCOMNotes;
  st: string;
begin
  try
    if (aList <> nil)
    then aList.Clear();

    if (aRecord.NotesCount <> 0) then begin
      if (aSummary <> nil) then begin
        aSummary.Add('');
        aSummary.Add({#13#10}'Заметки (' + IntToStr(aRecord.NotesCount) + '):');
      end;

      for idx := 0 to aRecord.NotesCount - 1 do begin
        note := aRecord.Notes[idx];

        if (aSummary <> nil) then begin
          for k := 0 to note.Notes.Count - 1 do begin
            st := note.Notes[k];
            aSummary.Add(st);
          end;
          {fixme!!!}
          //aSummary.AddStrings(note.Notes);

          if (idx < aRecord.NotesCount - 1) then aSummary.Add('');
        end;

        if (aList <> nil)
        then aList.AddItem(Trim(note.Notes.Text), note);
      end;
    end;
  except
    on E: Exception do LogWrite('RecListNotesRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.RecListMediaRefresh(aRecord: TGEDCOMRecord; aList: TBSListView; aSummary: TStrings);
var
  idx: Integer;
  mmLink: TGEDCOMMultimediaLink;
  mmRec: TGEDCOMMultimediaRecord;
  st: string;
begin
  try
    if (aList <> nil)
    then aList.Clear();

    if (aRecord.MultimediaLinksCount <> 0) then begin
      if (aList = nil) and (aSummary <> nil) 
      then begin
        aSummary.Add('');
        aSummary.Add({#13#10}'Мультимедиа (' + IntToStr(aRecord.MultimediaLinksCount) + '):');
      end;

      for idx := 0 to aRecord.MultimediaLinksCount - 1 do begin
        mmLink := aRecord.MultimediaLinks[idx];
        mmRec := TGEDCOMMultimediaRecord(mmLink.Value);

        if (mmRec <> nil) and (mmRec.FileReferencesCount <> 0) then begin
          st := mmRec.FileReferences[0].Title;

          if (aList = nil) and (aSummary <> nil)
          then aSummary.Add('  ' + HyperLink(mmRec.XRef, st) + ' (' + HyperLink(MLinkPrefix+mmRec.XRef, 'просмотр') + ')');

          if (aList <> nil)
          then aList.AddItem(st, TObject(idx));
        end;
      end;
    end;
  except
    on E: Exception do LogWrite('RecListMediaRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.RecListSourcesRefresh(aRecord: TGEDCOMRecord; aList: TBSListView; aSummary: TStrings);
var
  idx: Integer;
  cit: TGEDCOMSourceCitation;
  sourceRec: TGEDCOMSourceRecord;
  item: TListItem;
  nm: string;
begin
  try
    if (aList <> nil)
    then aList.Clear();

    if (aRecord.SourceCitationsCount <> 0) then begin
      if (aList = nil) and (aSummary <> nil)
      then begin
        aSummary.Add('');
        aSummary.Add({#13#10}'Источники (' + IntToStr(aRecord.SourceCitationsCount) + '):');
      end;

      for idx := 0 to aRecord.SourceCitationsCount - 1 do begin
        cit := aRecord.SourceCitations[idx];
        sourceRec := TGEDCOMSourceRecord(cit.Value);
        if (sourceRec <> nil) then begin
          nm := '"'+sourceRec.FiledByEntry+'"';
          if (cit.Page <> '') then nm := nm + ', ' + cit.Page;

          if (aList = nil) and (aSummary <> nil)
          then aSummary.Add('  ' + HyperLink(sourceRec.XRef, nm));

          if (aList <> nil) then begin
            item := aList.Items.Add();
            item.Caption := Trim(sourceRec.Originator.Text);
            item.SubItems.Add(nm);
            item.Data := cit;
          end;
        end;
      end;

      if (aList <> nil) then ResizeColumn(aList, 1);
    end;
  except
    on E: Exception do LogWrite('RecListSourcesRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.SetupRecEventsList(aList: TSheetList; PersonsMode: Boolean);
begin
  aList.Columns_BeginUpdate;
  aList.Columns_Clear;

  //aList.AddColumn('№', 25);
  aList.AddColumn('Событие', 75);
  aList.AddColumn('Дата', 80);

  if not(PersonsMode)
  then aList.AddColumn('Место', 200)
  else aList.AddColumn('Место/Атрибут', 200);

  aList.AddColumn('Причина', 130);

  aList.Columns_EndUpdate;
end;

procedure TfmBase.SetupRecNotesList(aList: TSheetList);
begin
  aList.Columns_BeginUpdate();
  aList.Columns_Clear();

  aList.AddColumn('Заметка', 300);

  aList.Columns_EndUpdate();
end;

procedure TfmBase.SetupRecMediaList(aList: TSheetList);
begin
  aList.Columns_BeginUpdate;
  aList.Columns_Clear;

  aList.AddColumn('Мультимедиа', 300);

  aList.Columns_EndUpdate;
end;

procedure TfmBase.SetupRecSourcesList(aList: TSheetList);
begin
  aList.Columns_BeginUpdate;
  aList.Columns_Clear;

  aList.AddColumn('Автор', 120);
  aList.AddColumn('Название', 180);

  aList.Columns_EndUpdate;
end;

procedure TfmBase.RecListAssociationsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
var
  idx: Integer;
  ast: TGEDCOMAssociation;
  item: TListItem;
  nm: string;
begin
  try
    if (aList <> nil)
    then aList.Clear();

    if (aRecord.AssociationsCount <> 0) then begin
      if (aList = nil) and (aSummary <> nil)
      then begin
        aSummary.Add('');
        aSummary.Add({#13#10}'Ассоциации:');
      end;

      for idx := 0 to aRecord.AssociationsCount - 1 do begin
        ast := aRecord.Associations[idx];
        nm := GetNameStr(ast.Individual);

        if (aList = nil) and (aSummary <> nil)
        then aSummary.Add('    ' + ast.Relation + ' ' + HyperLink(ast.Individual.XRef, nm));

        if (aList <> nil) then begin
          item := aList.Items.Add();
          item.Caption := ast.Relation;
          item.SubItems.Add(nm);
          item.Data := ast;
        end;
      end;
    end;                               
  except
    on E: Exception do LogWrite('RecListAssociationsRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.RecListGroupsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
var
  idx: Integer;
  grp: TGEDCOMGroupRecord;
  ptr: TGEDCOMPointer;
  item: TListItem;
begin
  try
    if (aList <> nil)
    then aList.Clear();

    if (aRecord.GroupsCount <> 0) then begin
      if (aList = nil) and (aSummary <> nil)
      then begin
        aSummary.Add('');
        aSummary.Add({#13#10}'Группы:');
      end;

      for idx := 0 to aRecord.GroupsCount - 1 do begin
        ptr := aRecord.Groups[idx];
        grp := TGEDCOMGroupRecord(ptr.Value);
        if (grp <> nil) then begin
          if (aList = nil) and (aSummary <> nil)
          then aSummary.Add('    '+HyperLink(grp.XRef, grp.Name));

          if (aList <> nil) then begin
            item := aList.Items.Add();
            item.Caption := grp.Name;
            item.Data := grp;
          end;
        end;
      end;
    end;
  except
    on E: Exception do LogWrite('RecListGroupsRefresh(): ' + E.Message);
  end;
end;

function TfmBase.IsMainList(aRecType: TGEDCOMRecordType; aList: TGKListView): Boolean;
begin
  Result := False;
  
  case aRecType of
    rtNone: ;
    rtIndividual: Result := (aList = ListPersons);
    rtFamily: Result := (aList = ListFamilies);
    rtNote: Result := (aList = ListNotes);
    rtMultimedia: Result := (aList = ListMultimedia);
    rtSource: Result := (aList = ListSources);
    rtRepository: Result := (aList = ListRepositories);
    rtGroup: Result := (aList = ListGroups);
    rtResearch: Result := (aList = ListResearches);
    rtTask: Result := (aList = ListTasks);
    rtCommunication: Result := (aList = ListCommunications);
    rtLocation: Result := (aList = ListLocations);
    rtSubmission: ;
    rtSubmitter: ;
  end;
end;

procedure TfmBase.ListsRefresh(aTitles: Boolean = False);

  procedure IntUpdate(aRecView: TRecordsView; ASCol: Integer);
  var
    rec: TGEDCOMRecord;
  begin
    rec := aRecView.GetSelectedRecord();
    aRecView.UpdateContents(FShieldState, aTitles, FXFilter, ASCol);
    if (rec <> nil) then aRecView.SelectItemByRec(rec);

    FCounts[aRecView.RecordType].Total := aRecView.TotalCount;
    FCounts[aRecView.RecordType].Filtered := aRecView.FilteredCount;
  end;

begin
  {$IFDEF PROFILER}Profiler.Mark(9, True);{$ENDIF}

  IntUpdate(ListPersons, 2);
  IntUpdate(ListFamilies, 1);
  IntUpdate(ListNotes, -1);
  IntUpdate(ListMultimedia, 1);
  IntUpdate(ListSources, 1);
  IntUpdate(ListRepositories, 1);
  IntUpdate(ListGroups, 1);
  IntUpdate(ListResearches, 1);
  IntUpdate(ListTasks, 1);
  IntUpdate(ListCommunications, 1);
  IntUpdate(ListLocations, 1);

  {$IFDEF PROFILER}Profiler.Mark(9, False);{$ENDIF}

  PageRecordsChange(nil);

  if Assigned(fmTimeLine) then fmTimeLine.CheckTimeWin(Self);
end;

procedure TfmBase.ShowPersonInfo(iRec: TGEDCOMIndividualRecord; aSummary: TStrings);
var
  idx, k: Integer;
  family: TGEDCOMFamilyRecord;
  sp: TGEDCOMPointer;
  rel_person: TGEDCOMIndividualRecord;
  rec: TGEDCOMRecord;
  st, unk, marr: string;
  namesakes: TStringList;
begin
  try
    aSummary.BeginUpdate();
    aSummary.Clear();
    try
      if (iRec <> nil) then begin
        aSummary.Add('');
        aSummary.Add('~ub+1~' + GetNameStr(iRec, True, True) + '~bu-1~');
        aSummary.Add('Пол: ' + Sex[iRec.Sex]);

        try
          if (iRec.ChildToFamilyLinksCount <> 0) then begin
            aSummary.Add('');
            aSummary.Add({#13#10}'Родители:');

            family := iRec.ChildToFamilyLinks[0].Family;

            rel_person := TGEDCOMIndividualRecord(family.Husband.Value);
            if (rel_person <> nil)
            then st := HyperLink(rel_person.XRef, GetNameStr(rel_person))
            else st := UnkMale;
            aSummary.Add('  Отец: ' + st + GetLifeStr(rel_person));

            rel_person := TGEDCOMIndividualRecord(family.Wife.Value);
            if (rel_person <> nil)
            then st := HyperLink(rel_person.XRef, GetNameStr(rel_person))
            else st := UnkFemale;
            aSummary.Add('  Мать: ' + st + GetLifeStr(rel_person));
          end;
        except
          on E: Exception do LogWrite('ShowPersonInfo().Parents(): ' + E.Message);
        end;

        try
          for idx := 0 to iRec.SpouseToFamilyLinksCount - 1 do begin
            family := iRec.SpouseToFamilyLinks[idx].Family;
            if (family = nil) then begin
              LogWrite('File ('+FFileName+'), iRec ('+iRec.XRef+'): empty family entry');
              Continue;
            end;

            if not(IsRecordAccess(family.Restriction, FShieldState))
            then Continue;

            if (iRec.Sex = svMale) then begin
              sp := family.Wife;
              st := 'Жена: ';
              unk := UnkFemale;
            end else begin
              sp := family.Husband;
              st := 'Муж: ';
              unk := UnkMale;
            end;

            marr := GetMarriageDate(family, dfDD_MM_YYYY);
            if (marr <> '')
            then marr := 'брак ' + marr
            else marr := 'семья';

            rel_person := TGEDCOMIndividualRecord(sp.Value);

            aSummary.Add('');
            if (rel_person <> nil)
            then st := st + HyperLink(rel_person.XRef, GetNameStr(rel_person)) + ' (' + HyperLink(family.XRef, marr) + ')'
            else st := st + unk + ' (' + HyperLink(family.XRef, marr) + ')';
            aSummary.Add({#13#10 + }st);

            if (family.ChildrenCount <> 0) then begin
              aSummary.Add('');
              aSummary.Add({#13#10}'Дети:');
            end;

            for k := 0 to family.ChildrenCount - 1 do begin
              rel_person := TGEDCOMIndividualRecord(family.Children[k].Value);
              aSummary.Add({#09}'    ' + HyperLink(rel_person.XRef, GetNameStr(rel_person)) + GetLifeStr(rel_person));
            end;
          end;
        except
          on E: Exception do LogWrite('ShowPersonInfo().Families(): ' + E.Message);
        end;

        RecListIndividualEventsRefresh(iRec, nil, aSummary);
        RecListNotesRefresh(iRec, nil, aSummary);
        RecListMediaRefresh(iRec, nil, aSummary);
        RecListSourcesRefresh(iRec, nil, aSummary);
        RecListAssociationsRefresh(iRec, nil, aSummary);
        RecListGroupsRefresh(iRec, nil, aSummary);

        // Тёзки
        namesakes := TStringList.Create;
        try
          st := GetNameStr(iRec, True, False);
          for k := 0 to FTree.RecordsCount - 1 do begin
            rec := FTree.Records[k];

            if (rec is TGEDCOMIndividualRecord) and (rec <> iRec) then begin
              rel_person := (rec as TGEDCOMIndividualRecord);
              unk := GetNameStr(rel_person, True, False);

              if (st = unk)
              then namesakes.AddObject(unk + GetLifeStr(rel_person), rel_person);
            end;
          end;

          if (namesakes.Count > 0) then begin
            aSummary.Add('');
            aSummary.Add('Тёзки:');
            for k := 0 to namesakes.Count - 1 do begin
              rel_person := (namesakes.Objects[k] as TGEDCOMIndividualRecord);
              aSummary.Add('    '+HyperLink(rel_person.XRef, namesakes[k]));
            end;
          end;
        finally
          namesakes.Destroy;
        end;
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('ShowPersonInfo(): ' + E.Message);
  end;
end;

procedure TfmBase.ShowFamilyInfo(aFamily: TGEDCOMFamilyRecord; aSummary: TStrings);
var
  irec: TGEDCOMIndividualRecord;
  k: Integer;
  st: string;
begin
  try
    aSummary.BeginUpdate();
    try
      aSummary.Clear();
      if (aFamily <> nil) then begin
        aSummary.Add('');

        irec := TGEDCOMIndividualRecord(aFamily.Husband.Value);
        if (irec <> nil)
        then st := HyperLink(irec.XRef, GetNameStr(irec))
        else st := UnkMale;
        aSummary.Add('Муж: ' + st + GetLifeStr(irec));

        irec := TGEDCOMIndividualRecord(aFamily.Wife.Value);
        if (irec <> nil)
        then st := HyperLink(irec.XRef, GetNameStr(irec))
        else st := UnkFemale;
        aSummary.Add('Жена: ' + st + GetLifeStr(irec));

        aSummary.Add('');

        if (aFamily.ChildrenCount <> 0)
        then aSummary.Add('Дети:');

        for k := 0 to aFamily.ChildrenCount - 1 do begin
          irec := TGEDCOMIndividualRecord(aFamily.Children[k].Value);
          aSummary.Add('    ' + HyperLink(irec.XRef, GetNameStr(irec)) + GetLifeStr(irec));
        end;

        aSummary.Add('');

        RecListFamilyEventsRefresh(aFamily, nil, aSummary);
        RecListNotesRefresh(aFamily, nil, aSummary);
        RecListMediaRefresh(aFamily, nil, aSummary);
        RecListSourcesRefresh(aFamily, nil, aSummary);
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('ShowFamilyInfo(): ' + E.Message);
  end;
end;

procedure TfmBase.ShowMultimediaInfo(aMultimediaRec: TGEDCOMMultimediaRecord; aSummary: TStrings);
var
  i: Integer;
begin
  try
    aSummary.BeginUpdate();
    try
      aSummary.Clear();
      if (aMultimediaRec <> nil) then begin
        aSummary.Add('');
        aSummary.Add('~ub+1~' + aMultimediaRec.FileReferences[0].Title + '~bu-1~');
        aSummary.Add('');
        aSummary.Add('Ссылки:');
        for i := 0 to FTree.RecordsCount - 1 do SearchSubjectLinks(FTree.Records[i], aMultimediaRec, aSummary);

        RecListNotesRefresh(aMultimediaRec, nil, aSummary);
        RecListSourcesRefresh(aMultimediaRec, nil, aSummary);
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('ShowMultimediaInfo(): ' + E.Message);
  end;
end;

procedure TfmBase.ShowNoteInfo(aNoteRec: TGEDCOMNoteRecord; aSummary: TStrings);
var
  i: Integer;
begin
  try
    aSummary.BeginUpdate();
    try
      aSummary.Clear();
      if (aNoteRec <> nil) then begin
        aSummary.Add('');
        aSummary.AddStrings(aNoteRec.Notes);
        aSummary.Add('');
        aSummary.Add('Ссылки:');
        for i := 0 to FTree.RecordsCount - 1 do SearchSubjectLinks(FTree.Records[i], aNoteRec, aSummary);
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('ShowNoteInfo(): ' + E.Message);
  end;
end;

procedure TfmBase.ShowSourceInfo(aSourceRec: TGEDCOMSourceRecord; aSummary: TStrings);
var
  i, k: Integer;
  rep: TGEDCOMRepositoryRecord;
  link_list: TStringList;
begin
  try
    aSummary.BeginUpdate();
    link_list := TStringList.Create;
    try
      aSummary.Clear();
      if (aSourceRec <> nil) then begin
        aSummary.Add('');
        aSummary.Add('~ub+1~' + aSourceRec.FiledByEntry + '~bu-1~');
        aSummary.Add('');
        aSummary.Add('Автор: ' + Trim(aSourceRec.Originator.Text));
        aSummary.Add('Название: "' + Trim(aSourceRec.Title.Text) + '"');
        aSummary.Add('Опубликовано: "' + Trim(aSourceRec.Publication.Text) + '"');

        if (aSourceRec.RepositoryCitationsCount > 0) then begin
          aSummary.Add('');
          aSummary.Add('Архивы:');

          for k := 0 to aSourceRec.RepositoryCitationsCount - 1 do begin
            rep := TGEDCOMRepositoryRecord(aSourceRec.RepositoryCitations[k].Value);
            aSummary.Add('    '+HyperLink(rep.XRef, rep.RepositoryName));
          end;
        end;

        aSummary.Add('');
        aSummary.Add('Ссылки:');
        for i := 0 to FTree.RecordsCount - 1 do SearchSubjectLinks(FTree.Records[i], aSourceRec, link_list);
        link_list.Sort();
        for i := 0 to link_list.Count - 1 do aSummary.Add(link_list[i]);

        RecListNotesRefresh(aSourceRec, nil, aSummary);
        RecListMediaRefresh(aSourceRec, nil, aSummary);
      end;
    finally
      link_list.Free;
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('ShowSourceInfo(): ' + E.Message);
  end;
end;

procedure TfmBase.ShowRepositoryInfo(aRepositoryRec: TGEDCOMRepositoryRecord; aSummary: TStrings);
var
  i, k: Integer;
  rec: TGEDCOMRecord;
  srcRec: TGEDCOMSourceRecord;
begin
  try
    aSummary.BeginUpdate();
    try
      aSummary.Clear();
      if (aRepositoryRec <> nil) then begin
        aSummary.Add('');
        aSummary.Add('~ub+1~' + Trim(aRepositoryRec.RepositoryName) + '~bu-1~');

        aSummary.Add('');
        aSummary.Add('Источники:');
        for i := 0 to FTree.RecordsCount - 1 do begin
          rec := FTree.Records[i];

          if (rec is TGEDCOMSourceRecord) then begin
            srcRec := (rec as TGEDCOMSourceRecord);

            for k := 0 to srcRec.RepositoryCitationsCount - 1 do
              if (srcRec.RepositoryCitations[k].Value = aRepositoryRec)
              then aSummary.Add('    '+GenRecordLink(srcRec, False));
          end;
        end;

        RecListNotesRefresh(aRepositoryRec, nil, aSummary);
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('ShowRepositoryInfo(): ' + E.Message);
  end;
end;

procedure TfmBase.ShowGroupInfo(aGroup: TGEDCOMGroupRecord; aSummary: TStrings);
var
  i: Integer;
  member: TGEDCOMIndividualRecord;
  mbrList: TStringList;
begin
  try
    mbrList := TStringList.Create;
    aSummary.BeginUpdate();
    try
      aSummary.Clear;
      if (aGroup <> nil) then begin
        aSummary.Add('');
        aSummary.Add('~ub+1~' + aGroup.Name + '~bu-1~');
        aSummary.Add('');
        aSummary.Add('Участники (' + IntToStr(aGroup.MembersCount) + '):');

        for i := 0 to aGroup.MembersCount - 1 do begin
          member := TGEDCOMIndividualRecord(aGroup.Members[i].Value);
          mbrList.AddObject(GetNameStr(member), member);
        end;

        mbrList.Sort;

        for i := 0 to mbrList.Count - 1 do begin
          member := TGEDCOMIndividualRecord(mbrList.Objects[i]);
          aSummary.Add('    '+HyperLink(member.XRef, mbrList[i], i+1));
        end;

        RecListNotesRefresh(aGroup, nil, aSummary);
        RecListMediaRefresh(aGroup, nil, aSummary);
      end;
    finally
      aSummary.EndUpdate();
      mbrList.Free;
    end;
  except
    on E: Exception do LogWrite('ShowGroupInfo(): ' + E.Message);
  end;
end;

procedure TfmBase.ShowResearchInfo(aResearchRec: TGEDCOMResearchRecord; aSummary: TStrings);
var
  i: Integer;
  taskRec: TGEDCOMTaskRecord;
  corrRec: TGEDCOMCommunicationRecord;
  grp: TGEDCOMGroupRecord;
begin
  try
    aSummary.BeginUpdate();
    try
      aSummary.Clear();
      if (aResearchRec <> nil) then begin
        aSummary.Add('');
        aSummary.Add('Название: "~ub+1~' + Trim(aResearchRec.Name) + '~bu-1~"');
        aSummary.Add('');
        aSummary.Add('Приоритет: ' + PriorityNames[aResearchRec.Priority]);
        aSummary.Add('Состояние: ' + StatusNames[aResearchRec.Status] + ' (' + IntToStr(aResearchRec.Percent) + '%)');
        aSummary.Add('Запущено: ' + GEDCOMDateToStr(aResearchRec.StartDate));
        aSummary.Add('Завершено: ' + GEDCOMDateToStr(aResearchRec.StopDate));

        if (aResearchRec.TasksCount > 0) then begin
          aSummary.Add('');
          aSummary.Add('Задачи:');
          for i := 0 to aResearchRec.TasksCount - 1 do begin
            taskRec := TGEDCOMTaskRecord(aResearchRec.Tasks[i].Value);
            aSummary.Add('    '+GenRecordLink(taskRec, False));
          end;
        end;

        if (aResearchRec.CommunicationsCount > 0) then begin
          aSummary.Add('');
          aSummary.Add('Коммуникации:');
          for i := 0 to aResearchRec.CommunicationsCount - 1 do begin
            corrRec := TGEDCOMCommunicationRecord(aResearchRec.Communications[i].Value);
            aSummary.Add('    '+GenRecordLink(corrRec, False));
          end;
        end;

        if (aResearchRec.GroupsCount <> 0) then begin
          aSummary.Add('');
          aSummary.Add('Группы:');
          for i := 0 to aResearchRec.GroupsCount - 1 do begin
            grp := TGEDCOMGroupRecord(aResearchRec.Groups[i].Value);
            aSummary.Add('    '+HyperLink(grp.XRef, grp.Name));
          end;
        end;

        RecListNotesRefresh(aResearchRec, nil, aSummary);
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('ShowResearchInfo(): ' + E.Message);
  end;
end;

procedure TfmBase.ShowTaskInfo(aTaskRec: TGEDCOMTaskRecord; aSummary: TStrings);
begin
  try
    aSummary.BeginUpdate();
    try
      aSummary.Clear();
      if (aTaskRec <> nil) then begin
        aSummary.Add('');
        aSummary.Add('Цель: ~ub+1~' + GetTaskGoalStr(FTree, aTaskRec) + '~bu-1~');
        aSummary.Add('');
        aSummary.Add('Приоритет: ' + PriorityNames[aTaskRec.Priority]);
        aSummary.Add('Запущено: ' + GEDCOMDateToStr(aTaskRec.StartDate));
        aSummary.Add('Завершено: ' + GEDCOMDateToStr(aTaskRec.StopDate));

        RecListNotesRefresh(aTaskRec, nil, aSummary);
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('ShowTaskInfo(): ' + E.Message);
  end;
end;

procedure TfmBase.ShowCommunicationInfo(aCommunicationRec: TGEDCOMCommunicationRecord; aSummary: TStrings);
begin
  try
    aSummary.BeginUpdate();
    try
      aSummary.Clear();
      if (aCommunicationRec <> nil) then begin
        aSummary.Add('');
        aSummary.Add('Тема: "~ub+1~' + Trim(aCommunicationRec.Name) + '~bu-1~"');
        aSummary.Add('');
        aSummary.Add('Корреспондент: ' + GetCorresponderStr(FTree, aCommunicationRec, True));
        aSummary.Add('Тип: ' + CommunicationNames[aCommunicationRec.CommunicationType]);
        aSummary.Add('Дата: ' + GEDCOMDateToStr(aCommunicationRec.Date));

        RecListNotesRefresh(aCommunicationRec, nil, aSummary);
        RecListMediaRefresh(aCommunicationRec, nil, aSummary);
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('ShowCommunicationInfo(): ' + E.Message);
  end;
end;

procedure TfmBase.ShowLocationInfo(aLocationRec: TGEDCOMLocationRecord; aSummary: TStrings);

  procedure PrepareEvent(aRec: TGEDCOMRecord; event: TGEDCOMCustomEvent; aList: TStringList);
  var
    loc: TGEDCOMLocationRecord;
    suffix: string;
  begin
    loc := TGEDCOMLocationRecord(event.Detail.Place.Location.Value);
    if (loc = aLocationRec) then begin
      if (event <> nil) and (event is TGEDCOMCustomEvent)
      then suffix := ', ' + AnsiLowerCase(GetEventName(TGEDCOMCustomEvent(event)))
      else suffix := '';

      aList.Add('    ' + GenRecordLink(aRec, True) + suffix);
    end;
  end;

var
  link_list: TStringList;
  i, k: Integer;
  rec: TGEDCOMRecord;
  i_rec: TGEDCOMIndividualRecord;
  f_rec: TGEDCOMFamilyRecord;
begin
  try
    aSummary.BeginUpdate();
    link_list := TStringList.Create;
    try
      aSummary.Clear();
      if (aLocationRec <> nil) then begin
        aSummary.Add('');
        aSummary.Add('~ub+1~' + Trim(aLocationRec.Name) + '~bu-1~');
        aSummary.Add('');
        aSummary.Add('Широта: ' + aLocationRec.Map.Lati);
        aSummary.Add('Долгота: ' + aLocationRec.Map.Long);

        // search links
        for i := 0 to FTree.RecordsCount - 1 do begin
          rec := FTree.Records[i];

          if (rec is TGEDCOMIndividualRecord) then begin
            i_rec := (rec as TGEDCOMIndividualRecord);

            for k := 0 to i_rec.IndividualEventsCount - 1 do
              PrepareEvent(i_rec, i_rec.IndividualEvents[k], link_list);

            for k := 0 to i_rec.IndividualAttributesCount - 1 do
              PrepareEvent(i_rec, i_rec.IndividualAttributes[k], link_list);
          end
          else
          if (rec is TGEDCOMFamilyRecord) then begin
            f_rec := (rec as TGEDCOMFamilyRecord);

            for k := 0 to f_rec.FamilyEventCount - 1 do
              PrepareEvent(f_rec, f_rec.FamilyEvents[k], link_list);
          end;
        end;
        link_list.Sort();
        if (link_list.Count > 0) then begin
          aSummary.Add('');
          aSummary.Add('Ссылки:');
          for i := 0 to link_list.Count - 1 do aSummary.Add(link_list[i]);
        end;

        RecListNotesRefresh(aLocationRec, nil, aSummary);
        RecListMediaRefresh(aLocationRec, nil, aSummary);
      end;
    finally
      link_list.Destroy;
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('ShowLocationInfo(): ' + E.Message);
  end;
end;

function TfmBase.GetSelectedPerson(): TGEDCOMIndividualRecord;
begin
  Result := TGEDCOMIndividualRecord(GetSelectedRecord(ListPersons));
end;

function TfmBase.ModifyRecEvent(aSender: TForm; aRecord: TGEDCOMRecord;
  aEvent: TGEDCOMCustomEvent; anAction: TRecAction): Boolean;
var
  fmEventEdit: TfmEventEdit;
  event: TGEDCOMCustomEvent; // TGEDCOMIndividualEvent + TGEDCOMFamilyEvent
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить факт?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    if (aRecord is TGEDCOMIndividualRecord) then begin
      if (aEvent is TGEDCOMIndividualEvent)
      then TGEDCOMIndividualRecord(aRecord).DeleteIndividualEvent(TGEDCOMIndividualEvent(aEvent))
      else TGEDCOMIndividualRecord(aRecord).DeleteIndividualAttribute(TGEDCOMIndividualAttribute(aEvent));
    end else begin
      TGEDCOMFamilyRecord(aRecord).DeleteFamilyEvent(TGEDCOMFamilyEvent(aEvent));
    end;

    Result := True;
    Modified := True;

    Exit;
  end;

  fmEventEdit := TfmEventEdit.Create(Self);

  try
    if (aEvent <> nil) then begin
      event := aEvent;
    end else begin
      if (aRecord is TGEDCOMIndividualRecord)
      then event := TGEDCOMIndividualEvent.Create(FTree, aRecord)
      else event := TGEDCOMFamilyEvent.Create(FTree, aRecord);
    end;

    fmEventEdit.Event := event;

    case ShowModalEx(fmEventEdit, aSender, True) of
      mrOk: begin
        if (aEvent = nil) then begin
          if (aRecord is TGEDCOMIndividualRecord) then begin
            event := fmEventEdit.Event;

            if (event is TGEDCOMIndividualEvent)
            then TGEDCOMIndividualRecord(aRecord).AddIndividualEvent(TGEDCOMIndividualEvent(event))
            else TGEDCOMIndividualRecord(aRecord).AddIndividualAttribute(TGEDCOMIndividualAttribute(event));
          end else TGEDCOMFamilyRecord(aRecord).AddFamilyEvent(TGEDCOMFamilyEvent(event));
        end else begin
          // hack, need refactoring
          if (aRecord is TGEDCOMIndividualRecord) and (fmEventEdit.Event <> aEvent) then begin
            if (aEvent is TGEDCOMIndividualEvent)
            then TGEDCOMIndividualRecord(aRecord).DeleteIndividualEvent(TGEDCOMIndividualEvent(aEvent))
            else TGEDCOMIndividualRecord(aRecord).DeleteIndividualAttribute(TGEDCOMIndividualAttribute(aEvent));

            event := fmEventEdit.Event;

            if (event is TGEDCOMIndividualEvent)
            then TGEDCOMIndividualRecord(aRecord).AddIndividualEvent(TGEDCOMIndividualEvent(event))
            else TGEDCOMIndividualRecord(aRecord).AddIndividualAttribute(TGEDCOMIndividualAttribute(event));
          end;
        end;

        Result := True;
        Modified := True;
      end;

      mrCancel: begin
        if (aEvent = nil)
        then event.Destroy;
      end;
    end;
  finally
    fmEventEdit.Destroy;
  end;
end;

function TfmBase.CreatePersonDialog(aTarget: TGEDCOMIndividualRecord;
  aTargetMode: TTargetMode; aNeedSex: TGEDCOMSex): TGEDCOMIndividualRecord;
var
  dlg: TfmPersonNew;
begin
  Result := nil;

  dlg := TfmPersonNew.Create(Self);
  try
    dlg.EditSex.ItemIndex := Ord(aNeedSex);
    dlg.TargetMode := aTargetMode;
    dlg.Target := aTarget;
    if (ShowModalEx(dlg, Self) = mrOk) then begin
      Result := CreatePersonEx(FTree, dlg.edName.Text, dlg.edPatronymic.Text,
        dlg.edFamily.Text, TGEDCOMSex(dlg.EditSex.ItemIndex), True);
      ChangeRecord(Result);
    end;
  finally
    dlg.Destroy;
  end;
end;

function TfmBase.ModifyPerson(var aIndivRec: TGEDCOMIndividualRecord): Boolean;
var
  dlg: TfmPersonEdit;
  //exists: Boolean;
begin
  Result := False;
  //exists := (aIndivRec <> nil);
  if (aIndivRec = nil) then Exit;

  dlg := TfmPersonEdit.Create(Self);
  try
    dlg.Person := aIndivRec;
    Result := (ShowModalEx(dlg, Self) = mrOk);
  finally
    dlg.Destroy;
    //dlg := nil;
  end;
end;

function TfmBase.ModifyFamily(var aFamilyRec: TGEDCOMFamilyRecord;
  aTarget: TFamilyTarget = ftNone; aPerson: TGEDCOMIndividualRecord = nil): Boolean;
var
  fmFamEdit: TfmFamilyEdit;
  exists: Boolean;
begin
  Result := False;

  if (aTarget = ftSpouse) and (aPerson <> nil)
  and not(aPerson.Sex in [svMale, svFemale]) then begin
    MessageDlg('У данного человека не задан пол.', mtError, [mbOk], 0);
    Exit;
  end;

  fmFamEdit := TfmFamilyEdit.Create(Self);
  try
    exists := (aFamilyRec <> nil);

    if not(exists) then begin
      aFamilyRec := TGEDCOMFamilyRecord.Create(FTree, FTree);
      aFamilyRec.InitNew();
    end;

    case aTarget of
      ftNone: ;
      ftSpouse:
        if (aPerson <> nil)
        then AddSpouseToFamily(FTree, aFamilyRec, aPerson);
      ftChild:
        if (aPerson <> nil)
        then FamilyChildAdd(aFamilyRec, aPerson);
    end;

    fmFamEdit.Family := aFamilyRec;

    Result := (ShowModalEx(fmFamEdit, Self) = mrOk);

    if (Result) then begin
      if not(exists)
      then FTree.AddRecord(aFamilyRec);
    end else begin
      if not(exists) then begin
        CleanFamily(aFamilyRec); // очищает и вставленных супругов и детей
        FreeAndNil(aFamilyRec);
      end;
    end;
  finally
    fmFamEdit.Destroy;
  end;
end;

function TfmBase.ModifyNote(var aNoteRec: TGEDCOMNoteRecord): Boolean;
var
  fmNoteEdit: TfmNoteEdit;
  exists: Boolean;
begin
  Result := False;

  {if (aNoteRec <> nil) then begin
    if not(RequestModify(aNoteRec)) then begin
      ShowLockMsg();
      Exit;
    end else LockRecord(aNoteRec, True);
  end;}

  fmNoteEdit := TfmNoteEdit.Create(Self);
  try
    exists := (aNoteRec <> nil);

    if not(exists) then begin
      aNoteRec := TGEDCOMNoteRecord.Create(FTree, FTree);
      aNoteRec.InitNew();
    end;

    fmNoteEdit.NoteRecord := aNoteRec;

    if (ShowModalEx(fmNoteEdit, Self) = mrOk) then begin
      if not(exists)
      then FTree.AddRecord(aNoteRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aNoteRec);
    end;
  finally
    fmNoteEdit.Destroy;
    {LockRecord(aNoteRec, False);}
  end;
end;

function TfmBase.ModifyMedia(var aMediaRec: TGEDCOMMultimediaRecord): Boolean;
var
  fmMediaEdit: TfmMediaEdit;
  exists: Boolean;
begin
  Result := False;

  fmMediaEdit := TfmMediaEdit.Create(Self);
  try
    exists := (aMediaRec <> nil);

    if not(exists) then begin
      aMediaRec := TGEDCOMMultimediaRecord.Create(FTree, FTree);
      aMediaRec.AddFileReference(TGEDCOMFileReferenceWithTitle.Create(FTree, aMediaRec));
      aMediaRec.InitNew();
    end;

    fmMediaEdit.MediaRec := aMediaRec;

    if (ShowModalEx(fmMediaEdit, Self) = mrOk) then begin
      if not(exists)
      then FTree.AddRecord(aMediaRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aMediaRec);
    end;
  finally
    fmMediaEdit.Destroy;
  end;
end;

function TfmBase.ModifySource(var aSourceRec: TGEDCOMSourceRecord): Boolean;
var
  fmSrcEdit: TfmSourceEdit;
  exists: Boolean;
begin
  Result := False;

  fmSrcEdit := TfmSourceEdit.Create(Self);
  try
    exists := (aSourceRec <> nil);

    if not(exists) then begin
      aSourceRec := TGEDCOMSourceRecord.Create(FTree, FTree);
      aSourceRec.InitNew();
    end;

    fmSrcEdit.SourceRecord := aSourceRec;

    if (ShowModalEx(fmSrcEdit, Self) = mrOk) then begin
      if not(exists)
      then FTree.AddRecord(aSourceRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aSourceRec);
    end;
  finally
    fmSrcEdit.Destroy;
  end;
end;

function TfmBase.ModifyRepository(var aRepRec: TGEDCOMRepositoryRecord): Boolean;
var
  fmRepEdit: TfmRepositoryEdit;
  exists: Boolean;
begin
  Result := False;

  fmRepEdit := TfmRepositoryEdit.Create(Self);
  try
    exists := (aRepRec <> nil);

    if not(exists) then begin
      aRepRec := TGEDCOMRepositoryRecord.Create(FTree, FTree);
      aRepRec.InitNew();
    end;

    fmRepEdit.RepositoryRecord := aRepRec;

    if (ShowModalEx(fmRepEdit, Self) = mrOk) then begin
      if not(exists)
      then FTree.AddRecord(aRepRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aRepRec);
    end;
  finally
    fmRepEdit.Destroy;
  end;
end;

function TfmBase.ModifyGroup(var aGroupRec: TGEDCOMGroupRecord): Boolean;
var
  fmGrpEdit: TfmGroupEdit;
  exists: Boolean;
begin
  Result := False;

  fmGrpEdit := TfmGroupEdit.Create(Self);
  try
    exists := (aGroupRec <> nil);

    if not(exists) then begin
      aGroupRec := TGEDCOMGroupRecord.Create(FTree, FTree);
      aGroupRec.InitNew();
    end;

    fmGrpEdit.Group := aGroupRec;

    if (ShowModalEx(fmGrpEdit, Self) = mrOk) then begin
      if not(exists)
      then FTree.AddRecord(aGroupRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aGroupRec);
    end;
  finally
    fmGrpEdit.Destroy;
  end;
end;

function TfmBase.ModifyResearch(var aResearchRec: TGEDCOMResearchRecord): Boolean;
var
  fmResEdit: TfmResearchEdit;
  exists: Boolean;
begin
  Result := False;

  fmResEdit := TfmResearchEdit.Create(Self);
  try
    exists := (aResearchRec <> nil);

    if not(exists) then begin
      aResearchRec := TGEDCOMResearchRecord.Create(FTree, FTree);
      aResearchRec.InitNew();
    end;

    fmResEdit.Research := aResearchRec;

    if (ShowModalEx(fmResEdit, Self) = mrOk) then begin
      if not(exists)
      then FTree.AddRecord(aResearchRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aResearchRec);
    end;
  finally
    fmResEdit.Destroy;
  end;
end;

function TfmBase.ModifyTask(var aTaskRec: TGEDCOMTaskRecord): Boolean;
var
  fmTaskEdit: TfmTaskEdit;
  exists: Boolean;
begin
  Result := False;

  fmTaskEdit := TfmTaskEdit.Create(Self);
  try
    exists := (aTaskRec <> nil);

    if not(exists) then begin
      aTaskRec := TGEDCOMTaskRecord.Create(FTree, FTree);
      aTaskRec.InitNew();
    end;

    fmTaskEdit.Task := aTaskRec;

    if (ShowModalEx(fmTaskEdit, Self) = mrOk) then begin
      if not(exists)
      then FTree.AddRecord(aTaskRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aTaskRec);
    end;
  finally
    fmTaskEdit.Destroy;
  end;
end;

function TfmBase.ModifyCommunication(var aCommunicationRec: TGEDCOMCommunicationRecord): Boolean;
var
  fmCorrEdit: TfmCommunicationEdit;
  exists: Boolean;
begin
  Result := False;

  fmCorrEdit := TfmCommunicationEdit.Create(Self);
  try
    exists := (aCommunicationRec <> nil);

    if not(exists) then begin
      aCommunicationRec := TGEDCOMCommunicationRecord.Create(FTree, FTree);
      aCommunicationRec.InitNew();
    end;

    fmCorrEdit.Communication := aCommunicationRec;

    if (ShowModalEx(fmCorrEdit, Self) = mrOk) then begin
      if not(exists)
      then FTree.AddRecord(aCommunicationRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aCommunicationRec);
    end;
  finally
    fmCorrEdit.Destroy;
  end;
end;

function TfmBase.ModifyLocation(var aLocationRec: TGEDCOMLocationRecord): Boolean;
var
  fmLocEdit: TfmLocationEdit;
  exists: Boolean;
begin
  Result := False;

  fmLocEdit := TfmLocationEdit.Create(Self);
  try
    exists := (aLocationRec <> nil);

    if not(exists) then begin
      aLocationRec := TGEDCOMLocationRecord.Create(FTree, FTree);
      aLocationRec.InitNew();
    end;

    fmLocEdit.LocationRecord := aLocationRec;

    if (ShowModalEx(fmLocEdit, Self) = mrOk) then begin
      if not(exists)
      then FTree.AddRecord(aLocationRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aLocationRec);
    end;
  finally
    fmLocEdit.Destroy;
  end;
end;

function TfmBase.ModifyAddress(aSender: TForm; anAddress: TGEDCOMAddress): Boolean;
var
  fmAddressEdit: TfmAddressEdit;
begin
  fmAddressEdit := TfmAddressEdit.Create(Application);
  try
    fmAddressEdit.Address := anAddress;
    Result := (ShowModalEx(fmAddressEdit, aSender) = mrOk);
  finally
    fmAddressEdit.Destroy;
  end;
end;

function TfmBase.ModifyRecNote(aSender: TForm; aRecord: TGEDCOMRecord;
  aNote: TGEDCOMNotes; anAction: TRecAction): Boolean;
var
  noteRec: TGEDCOMNoteRecord;
  note: TGEDCOMNotes;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ссылку на заметку?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    aRecord.DeleteNotes(aNote);

    Result := True;
    Modified := True;

    Exit;
  end;

  if (anAction = raEdit) and (aNote <> nil) then begin
    note := aNote;
    noteRec := TGEDCOMNoteRecord(note.Value);
    Result := ModifyNote(noteRec);
  end else begin
    noteRec := TGEDCOMNoteRecord(SelectRecord(smNote, []));
    if (noteRec <> nil) then begin
      note := TGEDCOMNotes.Create(FTree, aRecord);
      note.Value := noteRec;
      aRecord.AddNotes(note);

      Result := True;
    end;
  end;
end;

function TfmBase.ModifyRecMultimedia(aSender: TForm; aRecord: TGEDCOMRecord;
  aLink: TGEDCOMMultimediaLink; anAction: TRecAction): Boolean;
var
  mmRec: TGEDCOMMultimediaRecord;
  mmLink: TGEDCOMMultimediaLink;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ссылку на мультимедиа?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    aRecord.DeleteMultimediaLink(aLink);

    Result := True;
    Modified := True;

    Exit;
  end;

  if (anAction = raEdit) and (aLink <> nil) then begin
    mmLink := aLink;
    mmRec := TGEDCOMMultimediaRecord(mmLink.Value);
    Result := ModifyMedia(mmRec);
  end else begin
    mmRec := TGEDCOMMultimediaRecord(SelectRecord(smMultimedia, []));
    if (mmRec <> nil) then begin
      mmLink := TGEDCOMMultimediaLink.Create(FTree, aRecord);
      mmLink.Value := mmRec;
      aRecord.AddMultimediaLink(mmLink);

      Result := True;
    end;
  end;
end;

function TfmBase.ModifyRecSource(aSender: TForm; aRecord: TGEDCOMRecord;
  aCit: TGEDCOMSourceCitation; anAction: TRecAction): Boolean;
var
  cit: TGEDCOMSourceCitation;
  fmSrcCitEdit: TfmSourceCitEdit;
  res: Integer;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ссылку на источник?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    aRecord.DeleteSourceCitation(aCit);

    Result := True;
    Modified := True;

    Exit;
  end;

  fmSrcCitEdit := TfmSourceCitEdit.Create(Self);
  try
    if (anAction = raEdit) and (aCit <> nil)
    then cit := aCit
    else cit := TGEDCOMSourceCitation.Create(FTree, aRecord);

    fmSrcCitEdit.SourceCitation := cit;
    res := ShowModalEx(fmSrcCitEdit, aSender);

    case anAction of
      raAdd: begin
        if (res = mrOk)
        then aRecord.AddSourceCitation(cit)
        else cit.Destroy;
      end;
      raEdit: {dummy};
    end;

    Result := (res = mrOk);
  finally
    fmSrcCitEdit.Destroy;
  end;
end;

function TfmBase.ModifyRecUserRef(aSender: TForm; aRecord: TGEDCOMRecord;
  aUserRef: TGEDCOMUserReference; anAction: TRecAction): Boolean;
var
  dlg: TfmUserRefEdit;
  ref: TGEDCOMUserReference;
  res: Integer;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ассоциацию?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    aRecord.DeleteUserReference(aUserRef);

    Result := True;
    Modified := True;

    Exit;
  end;

  dlg := TfmUserRefEdit.Create(Self);
  try
    if (anAction = raEdit) and (aUserRef <> nil)
    then ref := aUserRef
    else ref := TGEDCOMUserReference.Create(FTree, aRecord);

    dlg.UserRef := ref;
    res := ShowModalEx(dlg, aSender);

    case anAction of
      raAdd: begin
        if (res = mrOk)
        then aRecord.AddUserReference(ref)
        else ref.Destroy;
      end;
      raEdit: {dummy};
    end;

    Result := (res = mrOk);
  finally
    dlg.Destroy;
  end;
end;

function TfmBase.ModifyRecAssociation(aSender: TForm; aRecord: TGEDCOMIndividualRecord;
  aAssociation: TGEDCOMAssociation; anAction: TRecAction): Boolean;
var
  fmAstEdit: TfmAssociationEdit;
  ast: TGEDCOMAssociation;
  res: Integer;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ассоциацию?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    aRecord.DeleteAssociation(aAssociation);

    Result := True;
    Modified := True;

    Exit;
  end;

  fmAstEdit := TfmAssociationEdit.Create(Self);
  try
    if (anAction = raEdit) and (aAssociation <> nil)
    then ast := aAssociation
    else ast := TGEDCOMAssociation.Create(FTree, aRecord);

    fmAstEdit.Association := ast;
    res := ShowModalEx(fmAstEdit, aSender);

    case anAction of
      raAdd: begin
        if (res = mrOk)
        then aRecord.AddAssociation(ast)
        else ast.Destroy;
      end;
      raEdit: {dummy};
    end;

    Result := (res = mrOk);
  finally
    fmAstEdit.Destroy;
  end;
end;

function TfmBase.ModifyTagNote(aTag: TGEDCOMTagWithLists;
  aNote: TGEDCOMNotes; anAction: TRecAction): Boolean;
var
  noteRec: TGEDCOMNoteRecord;
  note: TGEDCOMNotes;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ссылку на заметку?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    aTag.DeleteNotes(aNote);
    Modified := True;

    Result := True;
    Exit;
  end;

  if (anAction = raEdit) then begin
    if (aNote <> nil) then begin
      note := aNote;
      noteRec := TGEDCOMNoteRecord(note.Value);
      Result := ModifyNote(noteRec);
    end;
  end else begin
    noteRec := TGEDCOMNoteRecord(SelectRecord(smNote, []));
    if (noteRec <> nil) then begin
      note := TGEDCOMNotes.Create(FTree, aTag);
      note.Value := noteRec;
      aTag.AddNotes(note);

      Result := True;
    end;
  end;
end;

function TfmBase.ModifyTagMultimedia(aTag: TGEDCOMTagWithLists;
  aLink: TGEDCOMMultimediaLink; anAction: TRecAction): Boolean;
var
  mmRec: TGEDCOMMultimediaRecord;
  mmLink: TGEDCOMMultimediaLink;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ссылку на мультимедиа?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    aTag.DeleteMultimediaLink(aLink);
    Modified := True;

    Result := True;
    Exit;
  end;

  if (anAction = raEdit) then begin
    if (aLink <> nil) then begin
      mmLink := aLink;
      mmRec := TGEDCOMMultimediaRecord(mmLink.Value);
      Result := ModifyMedia(mmRec);
      Modified := Modified or Result;
    end;
  end else begin
    mmRec := TGEDCOMMultimediaRecord(SelectRecord(smMultimedia, []));
    if (mmRec <> nil) then begin
      mmLink := TGEDCOMMultimediaLink.Create(FTree, aTag);
      mmLink.Value := mmRec;
      aTag.AddMultimediaLink(mmLink);
      Modified := True;
      Result := True;
    end;
  end;
end;

function TfmBase.ModifyTagSource(aTag: TGEDCOMTagWithLists;
  aCit: TGEDCOMSourceCitation; anAction: TRecAction): Boolean;
var
  cit: TGEDCOMSourceCitation;
  fmSrcCitEdit: TfmSourceCitEdit;
  res: Integer;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ссылку на источник?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    aTag.DeleteSourceCitation(aCit);
    Modified := True;
    Result := True;

    Exit;
  end;

  fmSrcCitEdit := TfmSourceCitEdit.Create(Self);
  try
    if (anAction = raEdit) and (aCit <> nil)
    then cit := aCit
    else cit := TGEDCOMSourceCitation.Create(FTree, aTag);

    fmSrcCitEdit.SourceCitation := cit;
    res := ShowModalEx(fmSrcCitEdit);

    case anAction of
      raAdd: begin
        if (res = mrOk)
        then aTag.AddSourceCitation(cit)
        else cit.Destroy;
      end;
      raEdit: {dummy};
    end;

    Result := (res = mrOk);
  finally
    fmSrcCitEdit.Destroy;
  end;
end;

procedure TfmBase.SetFileName(const Value: string);
begin
  FFileName := Value;
  SetMainTitle();

  fmGEDKeeper.Options.LastDir := ExtractFilePath(FFileName);
end;

procedure TfmBase.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if not(CheckModified())
  then Action := caNone
  else begin
    Action := caFree;
  end;
end;

procedure TfmBase.PageRecordsChange(Sender: TObject);
begin
  fmGEDKeeper.UpdateControls();
end;

function TfmBase.GenRecordLink(aRecord: TGEDCOMRecord; aSigned: Boolean = True): string;
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
    if (aRecord is TGEDCOMCommunicationRecord) then sign := 'Корреспонденция: ';
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
  then st := GetTaskGoalStr(FTree, TGEDCOMTaskRecord(aRecord))
  else
  if (aRecord is TGEDCOMCommunicationRecord)
  then st := TGEDCOMCommunicationRecord(aRecord).Name
  else st := aRecord.XRef;

  Result := HyperLink(aRecord.XRef, sign + st);
end;

procedure TfmBase.SearchSubjectLinks(aInRecord, aSubject: TGEDCOMRecord; aToList: TStrings);
// поиск ссылок на субъект поиска в других записях и тэгах

  procedure OutLink(aRec: TGEDCOMRecord; aTag: TGEDCOMTag; aExt: TGEDCOMPointer);
  var
    prefix, suffix: string;
  begin
    if (aSubject is TGEDCOMSourceRecord) and (aExt <> nil) then begin
      if (TGEDCOMSourceCitation(aExt).Page <> '')
      then prefix := TGEDCOMSourceCitation(aExt).Page + ': '
      else prefix := '';
    end else begin
      prefix := '';
    end;

    if (aTag <> nil) and (aTag is TGEDCOMCustomEvent)
    then suffix := ', ' + AnsiLowerCase(GetEventName(TGEDCOMCustomEvent(aTag)))
    else suffix := '';

    aToList.Add('    ' + prefix + GenRecordLink(aRec, True) + suffix);
  end;

  procedure PrepareEvent(aRec: TGEDCOMRecord; event: TGEDCOMCustomEvent);
  var
    i: Integer;
  begin
    if (aSubject is TGEDCOMNoteRecord) then begin
      for i := 0 to event.Detail.NotesCount - 1 do
        if (event.Detail.Notes[i].Value = aSubject)
        then OutLink(aRec, event, nil);
    end
    else
    if (aSubject is TGEDCOMMultimediaRecord) then begin
      for i := 0 to event.Detail.MultimediaLinksCount - 1 do
        if (event.Detail.MultimediaLinks[i].Value = aSubject)
        then OutLink(aRec, event, nil);
    end
    else
    if (aSubject is TGEDCOMSourceRecord) then begin
      for i := 0 to event.Detail.SourceCitationsCount - 1 do
        if (event.Detail.SourceCitations[i].Value = aSubject)
        then OutLink(aRec, event, event.Detail.SourceCitations[i]);
    end;
  end;

var
  k: Integer;
  i_rec: TGEDCOMIndividualRecord;
  f_rec: TGEDCOMFamilyRecord;
begin
  try
    if (aSubject is TGEDCOMNoteRecord) then begin
      for k := 0 to aInRecord.NotesCount - 1 do
        if (aInRecord.Notes[k].Value = aSubject)
        then OutLink(aInRecord, nil, nil);
    end
    else
    if (aSubject is TGEDCOMMultimediaRecord) then begin
      for k := 0 to aInRecord.MultimediaLinksCount - 1 do
        if (aInRecord.MultimediaLinks[k].Value = aSubject)
        then OutLink(aInRecord, nil, nil);
    end
    else
    if (aSubject is TGEDCOMSourceRecord) then begin
      for k := 0 to aInRecord.SourceCitationsCount - 1 do
        if (aInRecord.SourceCitations[k].Value = aSubject)
        then OutLink(aInRecord, nil, aInRecord.SourceCitations[k]);
    end;

    if (aInRecord is TGEDCOMIndividualRecord) then begin
      i_rec := (aInRecord as TGEDCOMIndividualRecord);

      for k := 0 to i_rec.IndividualEventsCount - 1 do
        PrepareEvent(i_rec, i_rec.IndividualEvents[k]);

      for k := 0 to i_rec.IndividualAttributesCount - 1 do
        PrepareEvent(i_rec, i_rec.IndividualAttributes[k]);
    end
    else
    if (aInRecord is TGEDCOMFamilyRecord) then begin
      f_rec := (aInRecord as TGEDCOMFamilyRecord);

      for k := 0 to f_rec.FamilyEventCount - 1 do
        PrepareEvent(f_rec, f_rec.FamilyEvents[k]);
    end;
  except
    on E: Exception do LogWrite('SearchSubjectLinks(): ' + E.Message);
  end;
end;

procedure TfmBase.mPersonSummaryLink(Sender: TObject; LinkName: String);
var
  xref: string;
  rec: TGEDCOMRecord;
begin
  if (Pos(MLinkPrefix, LinkName) > 0) then begin
    xref := LinkName;
    Delete(xref, 1, Length(MLinkPrefix));

    rec := FTree.XRefIndex_Find(xref);
    if (rec <> nil) then ShowMedia(TGEDCOMMultimediaRecord(rec));
  end else SelectRecordByXRef(LinkName);
end;

procedure TfmBase.ShowMedia(aMediaRec: TGEDCOMMultimediaRecord);
begin
  {$IFNDEF DELPHI_NET}
  fmMediaView := TfmMediaView.Create(Self);
  try
    fmMediaView.FileRef := aMediaRec.FileReferences[0];
    if not(fmMediaView.Extern) then ShowModalEx(fmMediaView);
  finally
    fmMediaView.Destroy;
  end;
  {$ENDIF}
end;

procedure TfmBase.SetModified(const Value: Boolean);
begin
  FModified := Value;
  SetMainTitle();
end;

procedure TfmBase.SetMainTitle();
begin
  Caption := ExtractFileName(FFileName);

  if FModified
  then Caption := '* ' + Caption;
end;

procedure TfmBase.FileNew();
begin
  ChangesClear();
  Clear();

  ListsRefresh();
  ShowPersonInfo(nil, mPersonSummary.Lines);
  FileName := 'Неизвестный';
  Modified := False;
end;

procedure TfmBase.FileLoad(aFileName: string);
begin
  ChangesClear();
  Clear();

  {$IFDEF PROFILER}Profiler.Mark(1, True);{$ENDIF}
  FTree.LoadFromFile(aFileName);
  {$IFDEF PROFILER}Profiler.Mark(1, False);{$ENDIF}

  {$IFDEF PROFILER}Profiler.Mark(2, True);{$ENDIF}
  CheckGEDCOMFormat(FTree);
  {$IFDEF PROFILER}Profiler.Mark(2, False);{$ENDIF}

  FileName := aFileName;
  Modified := False;

  fmGEDKeeper.NamesTable.ImportNames(FTree);

  fmGEDKeeper.AddMRU(aFileName);

  ListsRefresh();

  ShowTips();
end;

procedure TfmBase.FileSave(const aFileName: string);
var
  fs: TFileStream;
  subm, ext_name: string;
  is_advanced: Boolean;
begin
  subm := FTree.Header.TagStringValue('SUBM');
  is_advanced := IsAdvanced();
  ext_name := FTree.Header.TagStringValue(ExtTag);

  FTree.Header.Clear;
  FTree.Header.Source := AppName;
  FTree.Header.ReceivingSystemName := AppName;
  FTree.Header.CharacterSet := fmGEDKeeper.Options.DefCharacterSet;
  FTree.Header.Language := 'Russian';
  FTree.Header.GEDCOMVersion := '5.5';
  FTree.Header.GEDCOMForm := 'LINEAGE-LINKED';
  FTree.Header.FileName := ExtractFileName(aFileName);
  FTree.Header.TransmissionDate.Date := Now();

  if (subm <> '')
  then FTree.Header.SetTagStringValue('SUBM', subm);

  if (is_advanced) and (ext_name <> '') then begin
    FTree.Header.AddTag(AdvTag);
    FTree.Header.AddTag(ExtTag, ext_name);
  end;

  FTree.Pack();

  fs := TFileStream.Create(aFileName, fmCreate);
  try
    FTree.SaveToStream(fs);
    FTree.Header.CharacterSet := csASCII;
  finally
    fs.Destroy;
  end;

  FileName := aFileName;
  fmGEDKeeper.AddMRU(aFileName);
  Modified := False;
end;

{procedure TfmBase.LockRecord(aRecord: TGEDCOMRecord; aLock: Boolean);
begin
  if (aLock)
  then FLockedRecords.Add(aRecord)
  else FLockedRecords.Remove(aRecord);
end;

function TfmBase.RequestModify(aRec: TGEDCOMRecord): Boolean;
begin
  Result := (FLockedRecords.IndexOf(aRec) < 0);
end;

procedure TfmBase.ShowLockMsg();
begin
  MessageDlg('Данная запись уже модифицируется, редактировать или удалять её сейчас нельзя!', mtWarning, [mbOk], 0);
end;}

procedure TfmBase.RecordAdd();
var
  rec: TGEDCOMRecord;
  res: Boolean;
begin
  rec := nil;
  res := False;

  case PageRecords.TabIndex of
    0: begin // персоны
      rec := CreatePersonDialog(nil, tmAncestor, svNone);
      res := (rec <> nil);
    end;

    1: begin // семьи
      res := ModifyFamily(TGEDCOMFamilyRecord(rec));
    end;

    2: begin // заметки
      res := ModifyNote(TGEDCOMNoteRecord(rec));
    end;

    3: begin // мультимедиа
      res := ModifyMedia(TGEDCOMMultimediaRecord(rec));
    end;

    4: begin // источники
      res := ModifySource(TGEDCOMSourceRecord(rec));
    end;

    5: begin // архивы
      res := ModifyRepository(TGEDCOMRepositoryRecord(rec));
    end;

    6: begin // группы
      res := ModifyGroup(TGEDCOMGroupRecord(rec));
    end;

    7: begin // исследования
      res := ModifyResearch(TGEDCOMResearchRecord(rec));
    end;

    8: begin // задачи
      res := ModifyTask(TGEDCOMTaskRecord(rec));
    end;

    9: begin // переписка
      res := ModifyCommunication(TGEDCOMCommunicationRecord(rec));
    end;

    10: begin // места
      res := ModifyLocation(TGEDCOMLocationRecord(rec));
    end;
  end;

  if (res) then begin
    ListsRefresh();
    SelectRecordByXRef(rec.XRef);
  end;
end;

procedure TfmBase.RecordEdit(Sender: TObject);
var
  rec: TGEDCOMRecord;
  res: Boolean;
begin
  res := False;

  case PageRecords.TabIndex of
    0: begin // персоны
      rec := GetSelectedRecord(ListPersons);
      res := ModifyPerson(TGEDCOMIndividualRecord(rec));
    end;

    1: begin // семьи
      rec := GetSelectedRecord(ListFamilies);
      res := ModifyFamily(TGEDCOMFamilyRecord(rec));
    end;

    2: begin // заметки
      rec := GetSelectedRecord(ListNotes);
      res := ModifyNote(TGEDCOMNoteRecord(rec));
    end;

    3: begin // мультимедиа
      rec := GetSelectedRecord(ListMultimedia);
      res := ModifyMedia(TGEDCOMMultimediaRecord(rec));
    end;

    4: begin // источники
      rec := GetSelectedRecord(ListSources);
      res := ModifySource(TGEDCOMSourceRecord(rec));
    end;

    5: begin // архивы
      rec := GetSelectedRecord(ListRepositories);
      res := ModifyRepository(TGEDCOMRepositoryRecord(rec));
    end;

    6: begin // группы
      rec := GetSelectedRecord(ListGroups);
      res := ModifyGroup(TGEDCOMGroupRecord(rec));
    end;

    7: begin // исследования
      rec := GetSelectedRecord(ListResearches);
      res := ModifyResearch(TGEDCOMResearchRecord(rec));
    end;

    8: begin // задачи
      rec := GetSelectedRecord(ListTasks);
      res := ModifyTask(TGEDCOMTaskRecord(rec));
    end;

    9: begin // переписка
      rec := GetSelectedRecord(ListCommunications);
      res := ModifyCommunication(TGEDCOMCommunicationRecord(rec));
    end;

    10: begin // места
      rec := GetSelectedRecord(ListLocations);
      res := ModifyLocation(TGEDCOMLocationRecord(rec));
    end;
  end;

  if (res) then ListsRefresh();
end;

procedure TfmBase.RecordDelete();

  function InternalDelete(aListView: TRecordsView): Boolean;
  var
    rec: TGEDCOMRecord;
  begin
    Result := False;

    rec := GetSelectedRecord(aListView);

    if (rec is TGEDCOMIndividualRecord)
    then Result := DeleteIndividualRecord(rec as TGEDCOMIndividualRecord, True)
    else
    if (rec is TGEDCOMFamilyRecord)
    then Result := DeleteFamilyRecord(rec as TGEDCOMFamilyRecord, True)
    else
    if (rec is TGEDCOMNoteRecord)
    then Result := DeleteNoteRecord(rec as TGEDCOMNoteRecord, True)
    else
    if (rec is TGEDCOMMultimediaRecord)
    then Result := DeleteMediaRecord(rec as TGEDCOMMultimediaRecord, True)
    else
    if (rec is TGEDCOMSourceRecord)
    then Result := DeleteSourceRecord(rec as TGEDCOMSourceRecord, True)
    else
    if (rec is TGEDCOMRepositoryRecord)
    then Result := DeleteRepositoryRecord(rec as TGEDCOMRepositoryRecord, True)
    else
    if (rec is TGEDCOMGroupRecord)
    then Result := DeleteGroupRecord(rec as TGEDCOMGroupRecord, True)
    else
    if (rec is TGEDCOMResearchRecord)
    then Result := DeleteResearchRecord(rec as TGEDCOMResearchRecord, True)
    else
    if (rec is TGEDCOMTaskRecord)
    then Result := DeleteTaskRecord(rec as TGEDCOMTaskRecord, True)
    else
    if (rec is TGEDCOMCommunicationRecord)
    then Result := DeleteCommunicationRecord(rec as TGEDCOMCommunicationRecord, True)
    else
    if (rec is TGEDCOMLocationRecord)
    then Result := DeleteLocationRecord(rec as TGEDCOMLocationRecord, True);

    if (Result) then aListView.DeleteRecord(rec);
  end;

var
  res: Boolean;
begin
  res := False;

  case PageRecords.TabIndex of
    0: res := InternalDelete(ListPersons); // персоны
    1: res := InternalDelete(ListFamilies); // семьи
    2: res := InternalDelete(ListNotes); // заметки
    3: res := InternalDelete(ListMultimedia); // мультимедиа
    4: res := InternalDelete(ListSources); // источники
    5: res := InternalDelete(ListRepositories); // архивы
    6: res := InternalDelete(ListGroups); // группы
    7: res := InternalDelete(ListResearches); // исследования
    8: res := InternalDelete(ListTasks); // задачи
    9: res := InternalDelete(ListCommunications); // переписка
    10: res := InternalDelete(ListLocations); // места
  end;

  if (res) then ListsRefresh();
end;

procedure TfmBase.ExportToExcel();
var
  ex_exp: TExcelExporter;
begin
  ex_exp := TExcelExporter.Create(FTree, GetCurFileTempPath());
  try
    ex_exp.SelectedRecords := ListPersons.ContentList;
    ex_exp.Generate();
  finally
    ex_exp.Destroy;
  end;
end;

function TfmBase.GetCurFileTempPath(): string;
begin
  Result := ExtractFilePath(FFileName) + '~temp\';
end;

procedure TfmBase.ExportToWeb();
var
  web: TWebExporter;
begin
  web := TWebExporter.Create(FTree, GetCurFileTempPath());
  try
    web.Generate();
  finally
    web.Destroy;
  end;
end;

function TfmBase.FileProperties(aMode: TFilePropertiesMode = fpmAuthor): Integer;
var
  fmFileProps: TfmFileProperties;
begin
  fmFileProps := TfmFileProperties.Create(Self);
  try
    fmFileProps.Tree := FTree;
    fmFileProps.PageControl1.TabIndex := Ord(aMode);
    Result := ShowModalEx(fmFileProps, Self);
  finally
    fmFileProps.Destroy;
  end;
end;

procedure TfmBase.ShowStats();
var
  fmStats: TfmStats;
begin
  fmStats := TfmStats.Create(Self);
  fmStats.Show;
end;

procedure TfmBase.SetFilter();
var
  fmFilter: TfmFilter;
begin
  fmFilter := TfmFilter.Create(Self);
  try
    ShowModalEx(fmFilter, Self);
  finally
    fmFilter.Destroy;
  end;
end;

procedure TfmBase.TimeLine_Done();
begin
  FXFilter.LifeMode := lmAll;
  FXFilter.TimeLineYear := -1;
  ApplyFilter();
end;

procedure TfmBase.TimeLine_Init();
begin
  FXFilter.LifeMode := lmTimeLine;
  //FXFilter.TimeLineYear := 1;
end;

function TfmBase.TimeLine_GetYear(): Integer;
begin
  Result := FXFilter.TimeLineYear;
end;

procedure TfmBase.TimeLine_SetYear(aYear: Integer);
begin
  FXFilter.TimeLineYear := aYear;
  ApplyFilter();
end;

procedure TfmBase.TreeTools();
var
  fmTreeTools: TfmTreeTools;
begin
  fmTreeTools := TfmTreeTools.Create(Self);
  try
    ShowModalEx(fmTreeTools, Self);
  finally
    fmTreeTools.Destroy;
  end;
end;

procedure TfmBase.ShowTreeAncestors();
var
  fmChart: TfmChart;
begin
  fmChart := TfmChart.Create(nil);
  fmChart.Base := Self;
  fmChart.Tree := FTree;
  fmChart.Person := GetSelectedPerson();
  fmChart.ChartKind := ckAncestors;
  fmChart.FileName := ExtractFileName(FFileName);
  fmChart.GenChart();
end;

procedure TfmBase.ShowTreeDescendants();
var
  fmChart: TfmChart;
begin
  fmChart := TfmChart.Create(nil);
  fmChart.Base := Self;
  fmChart.Tree := FTree;
  fmChart.Person := GetSelectedPerson();
  fmChart.ChartKind := ckDescendants;
  fmChart.FileName := ExtractFileName(FFileName);
  fmChart.GenChart();
end;

procedure TfmBase.GenPedigree_dAboville();
var
  p: TPedigree;
begin
  p := TPedigree.Create;
  try
    p.Options := fmGEDKeeper.Options.PedigreeOptions;
    p.ShieldState := FShieldState;
    p.Kind := pk_dAboville;
    p.Generate(GetCurFileTempPath(), FTree, GetSelectedPerson());
  finally
    p.Free;
  end;
end;

procedure TfmBase.GenPedigree_Konovalov();
var
  p: TPedigree;
begin
  p := TPedigree.Create;
  try
    p.Options := fmGEDKeeper.Options.PedigreeOptions;
    p.ShieldState := FShieldState;
    p.Kind := pk_Konovalov;
    p.Generate(GetCurFileTempPath(), FTree, GetSelectedPerson());
  finally
    p.Free;
  end;
end;

procedure TfmBase.PersonScan();
var
  fmPersonScan: TfmPersonScan;
begin
  fmPersonScan := TfmPersonScan.Create(Self);
  try
    ShowModalEx(fmPersonScan, Self);
  finally
    fmPersonScan.Destroy;
  end;
end;

procedure TfmBase.ShowMap();
var
  {$IFNDEF DELPHI_NET}
  fmMaps: TfmMaps;
  {$ENDIF}
begin
  {$IFNDEF DELPHI_NET}
  fmMaps := TfmMaps.Create(Application);
  try
    fmMaps.SelectedPersons := ListPersons.ContentList;
    fmMaps.Tree := FTree;
    fmMaps.Show;
  finally
    //fmMaps.Destroy;
  end;
  {$ENDIF}
end;

procedure TfmBase.ChangeRecord(aRecord: TGEDCOMRecord);
var
  rt: TGEDCOMRecordType;
begin
  rt := GetRecordType(aRecord);
  FChangedRecords[rt].Add(aRecord);

  aRecord.ChangeDate.ChangeDateTime := Now();
  Modified := True;
end;

procedure TfmBase.ChangesClear();
var
  rt: TGEDCOMRecordType;
begin
  for rt := Low(TGEDCOMRecordType) to High(TGEDCOMRecordType) do
    FChangedRecords[rt].Clear;
end;

procedure TfmBase.ApplyFilter();
begin
  if (FTree.RecordsCount > 0)
  then ListsRefresh();
end;

procedure TfmBase.NavPrev();
var
  rec: TGEDCOMRecord;
begin
  FBackman.BeginNav();
  try
    rec := TGEDCOMRecord(FBackman.Back());
    SelectRecordByXRef(rec.XRef);

    fmGEDKeeper.UpdateControls();
  finally
    FBackman.EndNav();
  end;
end;

procedure TfmBase.NavNext();
var
  rec: TGEDCOMRecord;
begin
  FBackman.BeginNav();
  try
    rec := TGEDCOMRecord(FBackman.Next());
    SelectRecordByXRef(rec.XRef);

    fmGEDKeeper.UpdateControls();
  finally
    FBackman.EndNav();
  end;
end;

procedure TfmBase.NavAdd(aRec: TGEDCOMRecord);
begin
  if (aRec <> nil) and not(FBackman.Busy) then begin
    FBackman.Current := aRec;

    fmGEDKeeper.UpdateControls();
  end;
end;

procedure TfmBase.actTestExecute(Sender: TObject);
(*var
  rec, temp_src: TGEDCOMRecord;
  cit: TGEDCOMSourceCitation;
  i: Integer;*)

  (*i: Integer;
  rec, frec: TGEDCOMRecord;
  ht: TBSHashTable;
  entry: TBSHashEntry;
  //
  fs: TFileStream;*)
begin
  (*temp_src := TGEDCOMSourceRecord(SelectRecord(smSource, []));
  if (temp_src = nil) then Exit;
  for i := 0 to FTree.RecordsCount - 1 do begin
    rec := FTree.Records[i];

    if (rec is TGEDCOMIndividualRecord) and (ListPersons.ContentList.IndexOf(rec) >= 0)
    then begin
      cit := TGEDCOMSourceCitation.Create(FTree, rec);
      cit.Value := temp_src;
      cit.CertaintyAssessment := 3;
      rec.AddSourceCitation(cit);
    end;
  end;
  ListsRefresh();*)

  (*for i := 0 to FTree.Count - 1 do begin
    rec := FTree.Records[i];

    {$IFDEF PROFILER}Profiler.Mark(17, True);{$ENDIF}
    frec := FTree.XRefIndex_Find(rec.XRef);
    {$IFDEF PROFILER}Profiler.Mark(17, False);{$ENDIF}

    Hole(frec);
  end;

  ht := TBSHashTable.Create(nil, TBSHashEntry, hkCRC32);
  try
    for i := 0 to FTree.Count - 1 do begin
      rec := FTree.Records[i];
      entry := ht.AddEntry(rec.XRef);
    end;

    for i := 0 to FTree.Count - 1 do begin
      rec := FTree.Records[i];

      {$IFDEF PROFILER}Profiler.Mark(18, True);{$ENDIF}
      entry := ht.FindEntry(rec.XRef);
      {$IFDEF PROFILER}Profiler.Mark(18, False);{$ENDIF}

      Hole(entry);
    end;
  finally
    ht.Destroy;
  end;*)
end;

function TfmBase.IsAdvanced(): Boolean;
begin
  Result := (FTree.Header.FindTag(AdvTag) <> nil);
end;

function TfmBase.GetExtName(): string;
var
  tag: TGEDCOMTag;
  p: Integer;
begin
  tag := FTree.Header.FindTag(ExtTag);
  if (tag = nil) then begin
    Result := ExtractFileName(FFileName);

    p := Pos('.ged', Result);
    if (p > 0) then Result := Copy(Result, 1, p - 1);
  end else Result := tag.StringValue;
end;

function TfmBase.GetArcFileName(): string;
begin
  Result := ExtractFilePath(FFileName) + GetExtName() + '.zip';
end;

function TfmBase.GetStoreFolder(): string;
begin
  Result := ExtractFilePath(FFileName) + GetExtName() + '\';

  if not(DirectoryExists(Result))
  then CreateDir(Result);
end;

function TfmBase.CheckPath(): Boolean;
var
  path: string;
begin
  path := ExtractFilePath(FFileName);
  Result := (path <> '');
  if not(Result)
  then MessageDlg('Для типов хранения "архив" и "хранилище" новый файл БД нужно предварительно сохранить', mtError, [mbOk], 0);
end;

function TfmBase.GetStoreType(aFileRef: string; var aFileName: string): TGKStoreType;
begin
  aFileName := aFileRef;

  if (Pos(GKStoreType[gstArchive].Sign, aFileRef) > 0) then begin
    Result := gstArchive;
    Delete(aFileName, 1, 4);
  end
  else
  if (Pos(GKStoreType[gstStorage].Sign, aFileRef) > 0) then begin
    Result := gstStorage;
    Delete(aFileName, 1, 4);
  end
  else begin
    Result := gstReference;
  end;
end;

procedure TfmBase.MediaLoad(aRefName: string; var aStream: TStream);
var
  gst: TGKStoreType;
  target_fn: string;
  {$IFNDEF DELPHI_NET}
  az: TAbZipKit;
  {$ENDIF}
begin
  gst := GetStoreType(aRefName, target_fn);

  case gst of
    gstReference: begin
      aStream := TFileStream.Create(target_fn, fmOpenRead);
    end;

    gstArchive: begin
      aStream := TMemoryStream.Create();

      if not(FileExists(GetArcFileName()))
      then MessageDlg('Архив не найден, данные не загружены', mtError, [mbOk], 0)
      else begin
        {$IFNDEF DELPHI_NET}
        AnsiToOem(PChar(target_fn), PChar(target_fn));

        az := TAbZipKit.Create(nil);
        try
          az.OpenArchive(GetArcFileName());
          az.ExtractToStream(target_fn, aStream);
          aStream.Seek(0, soFromBeginning);
        finally
          az.Destroy;
        end;
        {$ENDIF}
      end;
    end;

    gstStorage: begin
      target_fn := GetStoreFolder() + target_fn;
      aStream := TFileStream.Create(target_fn, fmOpenRead);
    end;
  end;
end;

procedure TfmBase.MediaLoad(aRefName: string; var aFileName: string);
var
  gst: TGKStoreType;
  target_fn: string;
  fs: TFileStream;
  {$IFNDEF DELPHI_NET}
  az: TAbZipKit;
  {$ENDIF}
begin
  gst := GetStoreType(aRefName, target_fn);

  case gst of
    gstReference: begin
      aFileName := target_fn;
    end;

    gstArchive: begin
      aFileName := GetTempDir() + ExtractFileName(target_fn);

      fs := TFileStream.Create(aFileName, fmCreate);
      try
        if not(FileExists(GetArcFileName()))
        then MessageDlg('Архив не найден, данные не загружены', mtError, [mbOk], 0)
        else begin
          {$IFNDEF DELPHI_NET}
          AnsiToOem(PChar(target_fn), PChar(target_fn));
          target_fn := StringReplace(target_fn, '\', '/', [rfReplaceAll]);

          az := TAbZipKit.Create(nil);
          try
            az.OpenArchive(GetArcFileName());
            az.ExtractToStream(target_fn, fs);
          finally
            az.Destroy;
          end;
          {$ENDIF}
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

procedure TfmBase.MediaSave(aFileName: string; aStoreType: TGKStoreType; var aRefPath: string);
var
  target_fn, sfn, spath: string;
  {$IFNDEF DELPHI_NET}
  az: TAbZipper;
  arc_item: TAbArchiveItem;
  {$ENDIF}
  fs: TFileStream;
  mmFormat: TGEDCOMMultimediaFormat;
  idx, fdt: Longint;
begin
  sfn := ExtractFileName(aFileName);

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

    gstStorage: begin
      sfn := spath + sfn;
      aRefPath := GKStoreType[aStoreType].Sign + sfn;

      target_fn := GetStoreFolder() + '\' + sfn;
      {$IFNDEF DELPHI_NET}
      MoveFile(PChar(aFileName), PChar(target_fn));
      {$ENDIF}
    end;
  end;
end;

procedure TfmBase.FormActivate(Sender: TObject);
begin
  fmGEDKeeper.UpdateControls();

  if Assigned(fmTimeLine) then fmTimeLine.CheckTimeWin(Self);
end;

procedure TfmBase.FormDeactivate(Sender: TObject);
begin
  fmGEDKeeper.UpdateControls();
end;

function TfmBase.SelectPerson(aTarget: TGEDCOMIndividualRecord; aTargetMode: TTargetMode;
  aNeedSex: TGEDCOMSex): TGEDCOMIndividualRecord;
var
  dlg: TfmRecordSelect;
begin
  dlg := TfmRecordSelect.Create(Self);
  try
    dlg.FTarget := aTarget;
    dlg.FNeedSex := aNeedSex;
    dlg.TargetMode := aTargetMode;
    dlg.Mode := smPerson;

    if (ShowModalEx(dlg, Self) = mrOk)
    then Result := TGEDCOMIndividualRecord(dlg.ResultRecord)
    else Result := nil;
  finally
    dlg.Destroy;
  end;
end;

function TfmBase.SelectFamily(aTarget: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;
var
  dlg: TfmRecordSelect;
begin
  dlg := TfmRecordSelect.Create(Self);
  try
    dlg.FTarget := aTarget;
    dlg.FNeedSex := svNone;
    dlg.TargetMode := tmChildToFamily;
    dlg.Mode := smFamily;

    if (ShowModalEx(dlg, Self) = mrOk)
    then Result := TGEDCOMFamilyRecord(dlg.ResultRecord)
    else Result := nil;
  finally
    dlg.Destroy;
  end;
end;

function TfmBase.SelectRecord(aMode: TSelectMode; anArgs: array of const): TGEDCOMRecord;
var
  dlg: TfmRecordSelect;
  args_cnt: Integer;
begin
  dlg := TfmRecordSelect.Create(Self);

  args_cnt := High(anArgs) + 1; 
  if (args_cnt > 0) and (anArgs[0].VType = vtAnsiString)
  then dlg.edFastFilter.Text := string(anArgs[0].VAnsiString);

  try
    dlg.Mode := aMode;

    if (ShowModalEx(dlg, Self) = mrOk)
    then Result := dlg.ResultRecord
    else Result := nil;
  finally
    dlg.Destroy;
  end;
end;

function TfmBase.GroupMemberAdd(aGroup: TGEDCOMGroupRecord;
  aMember: TGEDCOMIndividualRecord): Boolean;
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

function TfmBase.GroupMemberRemove(aGroup: TGEDCOMGroupRecord;
  aMember: TGEDCOMIndividualRecord): Boolean;
begin
  try
    aGroup.DeleteMember(aGroup.IndexOfMember(aMember));
    aMember.DeleteGroup(aMember.IndexOfGroup(aGroup));

    Result := True;
  except
    Result := False;
  end;
end;

procedure TfmBase.DoRedo();
begin
  FUndoman.CmdRedo();
  ListsRefresh();
  fmGEDKeeper.UpdateControls();
end;

procedure TfmBase.DoUndo();
begin
  FUndoman.CmdUndo();
  ListsRefresh();
  fmGEDKeeper.UpdateControls();
end;

procedure TfmBase.DoPersonChangeSex(aPerson: TGEDCOMIndividualRecord;
  NewSex: TGEDCOMSex);
begin
  if (aPerson.Sex <> NewSex) then begin
    FUndoman.CmdDo(TCmdPersonChangeSex.Create(FUndoman, aPerson, NewSex));
    FUndoman.Commit();
  end;
end;

procedure TfmBase.DoPersonChangePatriarch(aPerson: TGEDCOMIndividualRecord;
  NewValue: Boolean);
begin
  if (aPerson.Patriarch <> NewValue) then begin
    FUndoman.CmdDo(TCmdPersonChangePatriarch.Create(FUndoman, aPerson, NewValue));
    FUndoman.Commit();
  end;
end;

procedure TfmBase.SetShieldState(const Value: TShieldState);
var
  up: Boolean;
begin
  up := ((FShieldState <> ssNone) and (Value = ssNone))
     or ((FShieldState = ssNone) and (Value <> ssNone));

  FShieldState := Value;

  if up then ListsRefresh();
end;

procedure TfmBase.ShowTips();
var
  birth_days: TStringList;
  i: Integer;
  rec: TGEDCOMRecord;
  i_rec: TGEDCOMIndividualRecord;
  nm, days: string;
begin
  if not(fmGEDKeeper.Options.ShowTips) then Exit;

  birth_days := TStringList.Create;
  try
    for i := 0 to FTree.RecordsCount - 1 do begin
      rec := FTree.Records[i];

      if (rec is TGEDCOMIndividualRecord) then begin
        i_rec := (rec as TGEDCOMIndividualRecord);
        nm := GetNameStr(i_rec);
        days := GetDaysForBirth(i_rec);

        if (days <> '') and (StrToInt(days) < 3)
        then birth_days.Add('До дня рождения "'+nm+'" осталось '+days+' дня(-ей)');  
      end;
    end;

    if (birth_days.Count > 0)
    then fmGEDKeeper.Options.ShowTips := TfmTipsDialog.ShowTipsEx('Дни рождения', fmGEDKeeper.Options.ShowTips, birth_days);
  finally
    birth_days.Destroy;
  end;
end;

procedure TfmBase.ShowStereoView();
begin
  fmStereoView := TfmStereoView.Create(Self);
  fmStereoView.Show;
end;

procedure TfmBase.ShowOrganizer();
begin
  fmOrganizer := TfmOrganizer.Create(Self);
  try
    ShowModalEx(fmOrganizer, Self);
  finally
    fmOrganizer.Destroy;
  end;
end;

procedure TfmBase.ImportDB();
begin
  fmDBImport := TfmDBImport.Create(Self);
  try
    ShowModalEx(fmDBImport, Self);
  finally
    fmDBImport.Destroy;
  end;
end;

end.
