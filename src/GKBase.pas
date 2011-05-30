unit GKBase; {prepare:partial; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Buttons, Menus, Masks, ActnList, GKEngine,
  GKCommon, GedCom551, GKCtrls, GKLists, GKLangs;

type
  TFilePropertiesMode = (fpmAuthor, fpmDiags, fpmAdvanced);

  TRecCount = record
    Total: Integer;
    Filtered: Integer;
  end;

  TRecNotify = (rnDelete);

  TfmBase = class(TForm, ILocalization)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PageRecordsChange(Sender: TObject);
    procedure mPersonSummaryLink(Sender: TObject; LinkName: String);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    FBackman: TBackManager;
    FChangedRecords: array [TGEDCOMRecordType] of TList;
    FEngine: TGenEngine;
    FLockedRecords: TList;
    FModified: Boolean;
    FShieldState: TShieldState;
    FTree: TGEDCOMTree;
    FUndoman: TUndoManager;
    FXFilter: TPersonsFilter;

    procedure InitializeComponent;

    function  IsMainList(aRecType: TGEDCOMRecordType; aList: TGKListView): Boolean;
    procedure ListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure NavAdd(aRec: TGEDCOMRecord);
    procedure SetFileName(const Value: string);
    procedure SetMainTitle();
    procedure SetModified(const Value: Boolean);
    procedure ShowAddress(anAddress: TGEDCOMAddress; aSummary: TStrings);
    procedure SetShieldState(const Value: TShieldState);
    function GetFileName: string;

    {procedure ShowLockMsg();
    procedure LockRecord(aRecord: TGEDCOMRecord; aLock: Boolean);
    function  RequestModify(aRec: TGEDCOMRecord): Boolean;
    function ModifyRecord(var aRecord: TGEDCOMRecord; anAction: TRecAction; aConfirm: Boolean): Boolean;}
  public
    PageRecords: TPageControl;

    // GUI
    ListPersons: TRecordsView;
    mPersonSummary: TGKHyperView;
    ListFamilies: TRecordsView;
    mFamilySummary: TGKHyperView;
    ListNotes: TRecordsView;
    mNoteSummary: TGKHyperView;
    ListMultimedia: TRecordsView;
    mMediaSummary: TGKHyperView;
    ListSources: TRecordsView;
    mSourceSummary: TGKHyperView;
    ListRepositories: TRecordsView;
    mRepositorySummary: TGKHyperView;
    ListGroups: TRecordsView;
    mGroupSummary: TGKHyperView;
    ListResearches: TRecordsView;
    mResearchSummary: TGKHyperView;
    ListTasks: TRecordsView;
    mTaskSummary: TGKHyperView;
    ListCommunications: TRecordsView;
    mCommunicationSummary: TGKHyperView;
    ListLocations: TRecordsView;
    mLocationSummary: TGKHyperView;
    //

    FCounts: array [TGEDCOMRecordType] of TRecCount;

    procedure ApplyFilter();
    procedure ChangeRecord(aRecord: TGEDCOMRecord);
    procedure ChangesClear();
    function  CheckModified: Boolean;
    procedure Clear();
    procedure CreateListView(aOwner: TComponent; aParent: TWinControl; var aList: TGKListView);
    procedure CreatePage(aPageText: string; aRecType: TGEDCOMRecordType;
      var aList: TRecordsView; var aSummary: TGKHyperView);
    function  CreatePersonDialog(aTarget: TGEDCOMIndividualRecord;
      aTargetMode: TTargetMode; aNeedSex: TGEDCOMSex): TGEDCOMIndividualRecord;
    procedure CreateRecordsView(aOwner: TComponent; aParent: TWinControl;
      aRecordType: TGEDCOMRecordType; var aList: TRecordsView);
    procedure ImportDB();
    procedure ExportToExcel(appmode: Boolean);
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
    function  GetSelectedPerson(): TGEDCOMIndividualRecord;
    procedure ListsRefresh(aTitles: Boolean = False);
    procedure NavNext();
    procedure NavPrev();
    procedure PersonScan();
    procedure RecordAdd();
    procedure RecordDelete();
    procedure RecordEdit(Sender: TObject);
    procedure RecordNotify(aRecord: TGEDCOMRecord; aNotify: TRecNotify);
    procedure SearchSubjectLinks(aInRecord, aSubject: TGEDCOMRecord; aToList: TStrings);
    function  SelectFamily(aTarget: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;
    function  SelectPerson(aTarget: TGEDCOMIndividualRecord; aTargetMode: TTargetMode;
      aNeedSex: TGEDCOMSex): TGEDCOMIndividualRecord;
    function  SelectRecord(aMode: TGEDCOMRecordType; anArgs: array of const): TGEDCOMRecord;
    procedure SelectRecordByXRef(XRef: string);
    procedure SetFilter();
    procedure ShowMap();
    procedure ShowMedia(aMediaRec: TGEDCOMMultimediaRecord);
    procedure ShowOrganizer();
    procedure ShowRecordInfo(aRecord: TGEDCOMRecord);
    procedure ShowScriptDaemon(); 
    procedure ShowStats();
    procedure ShowTips();
    procedure ShowTreeAncestors();
    procedure ShowTreeDescendants();
    procedure ShowTreeBoth();
    procedure TreeTools();

    function IsAdvanced(): Boolean;

    function ModifyName(var aName: TName): Boolean;
    function DefinePatronymic(aName: string; aSex: TGEDCOMSex; aConfirm: Boolean): string;

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

    function DeleteRecord(aRecord: TGEDCOMRecord; aConfirm: Boolean): Boolean;

    function RecordIsFiltered(aRecord: TGEDCOMRecord): Boolean;

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

    procedure RecListAssociationsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TGKListView; aSummary: TStrings);
    procedure RecListFamilyEventsRefresh(aRecord: TGEDCOMFamilyRecord; aList: TGKListView; aSummary: TStrings);
    procedure RecListGroupsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TGKListView; aSummary: TStrings);
    procedure RecListIndividualEventsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TGKListView; aSummary: TStrings);
    procedure RecListMediaRefresh(aRecord: TGEDCOMRecord; aList: TGKListView; aSummary: TStrings);
    procedure RecListNotesRefresh(aRecord: TGEDCOMRecord; aList: TCustomListControl; aSummary: TStrings);
    procedure RecListSourcesRefresh(aRecord: TGEDCOMRecord; aList: TGKListView; aSummary: TStrings);

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

    procedure SetLang();

    procedure TimeLine_Init();
    procedure TimeLine_Done();
    function  TimeLine_GetYear(): Integer;
    procedure TimeLine_SetYear(aYear: Integer);

    property Backman: TBackManager read FBackman;
    property Engine: TGenEngine read FEngine;
    property FileName: string read GetFileName write SetFileName;
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
  GKUtils, GKPersonNew, GKRecordSelect, GKStats, GKNoteEdit, GKChart, GKTipsDlg,
  GKSourceEdit, GKEventEdit, GKAbout, GKChartCore, GKFileProperties, GKTaskEdit,
  GKPersonEdit, GKExport, GKOptions, GKFamilyEdit, GKMain, GKAssociationEdit,
  GKFilter, GKTreeTools, GKGroupEdit, GKPersonScan, GKProgress, GKSourceCitEdit,
  GKRepositoryEdit, GKMediaEdit, GKResearchEdit, GKCommunicationEdit,
  GKLocationEdit, GKCommands, GKUserRefEdit, GKTimeLine, GKOrganizer,
  GKDBImport, GKAddressEdit, GKScriptDaemon, GKNameEdit, GKMediaView
  {$IFNDEF DELPHI_NET}, GKMaps {$ENDIF};

{$R *.dfm}

procedure TfmBase.InitializeComponent;
begin
  //
end;

procedure TfmBase.FormCreate(Sender: TObject);
var
  rt: TGEDCOMRecordType;
begin
  for rt := Low(TGEDCOMRecordType) to High(TGEDCOMRecordType) do
    FChangedRecords[rt] := TList.Create;

  FEngine := TGenEngine.Create;
  FTree := FEngine.Tree;

  FXFilter := TPersonsFilter.Create;

  FLockedRecords := TList.Create;

  FBackman := TBackManager.Create;
  FUndoman := TUndoManager.Create(FTree, manualCommit);

  PageRecords := TPageControl.Create(Self);
  with PageRecords do begin
    Parent := Self;
    Align := alClient;
    OnChange := PageRecordsChange;
  end;

  CreatePage(LSList[LSID_RPIndividuals], rtIndividual, ListPersons, mPersonSummary);
  CreatePage(LSList[LSID_RPFamilies], rtFamily, ListFamilies, mFamilySummary);
  CreatePage(LSList[LSID_RPNotes], rtNote, ListNotes, mNoteSummary);
  CreatePage(LSList[LSID_RPMultimedia], rtMultimedia, ListMultimedia, mMediaSummary);
  CreatePage(LSList[LSID_RPSources], rtSource, ListSources, mSourceSummary);
  CreatePage(LSList[LSID_RPRepositories], rtRepository, ListRepositories, mRepositorySummary);
  CreatePage(LSList[LSID_RPGroups], rtGroup, ListGroups, mGroupSummary);
  CreatePage(LSList[LSID_RPResearches], rtResearch, ListResearches, mResearchSummary);
  CreatePage(LSList[LSID_RPTasks], rtTask, ListTasks, mTaskSummary);
  CreatePage(LSList[LSID_RPCommunications], rtCommunication, ListCommunications, mCommunicationSummary);
  CreatePage(LSList[LSID_RPLocations], rtLocation, ListLocations, mLocationSummary);

  PageRecords.ActivePageIndex := 0;
end;

procedure TfmBase.SetLang();
begin
  PageRecords.Pages[ 0].Caption := LSList[LSID_RPIndividuals];
  PageRecords.Pages[ 1].Caption := LSList[LSID_RPFamilies];
  PageRecords.Pages[ 2].Caption := LSList[LSID_RPNotes];
  PageRecords.Pages[ 3].Caption := LSList[LSID_RPMultimedia];
  PageRecords.Pages[ 4].Caption := LSList[LSID_RPSources];
  PageRecords.Pages[ 5].Caption := LSList[LSID_RPRepositories];
  PageRecords.Pages[ 6].Caption := LSList[LSID_RPGroups];
  PageRecords.Pages[ 7].Caption := LSList[LSID_RPResearches];
  PageRecords.Pages[ 8].Caption := LSList[LSID_RPTasks];
  PageRecords.Pages[ 9].Caption := LSList[LSID_RPCommunications];
  PageRecords.Pages[10].Caption := LSList[LSID_RPLocations];
end;

procedure TfmBase.FormDestroy(Sender: TObject);
var
  rt: TGEDCOMRecordType;
begin
  ListPersons.Clear;

  FBackman.Destroy;
  FUndoman.Destroy;

  FLockedRecords.Free;

  FXFilter.Free;

  FTree := nil;
  FEngine.Destroy;

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

procedure TfmBase.CreatePage(aPageText: string; aRecType: TGEDCOMRecordType;
  var aList: TRecordsView; var aSummary: TGKHyperView);
var
  sheet: TTabSheet;
begin
  sheet := TTabSheet.Create(PageRecords);
  sheet.PageControl := PageRecords;
  sheet.Caption := aPageText;

  aSummary := TGKHyperView.Create(Self);
  with aSummary do begin
    Parent := sheet;
    BorderWidth := 4;
    Width := 400;
    Font.Name := 'Tahoma';
    Align := alRight;

    OnLink := mPersonSummaryLink;
  end;

  with TSplitter.Create(Self) do begin
    Parent := sheet;
    Width := 4;
    Align := alRight;
    Beveled := True;
  end;

  CreateRecordsView(Self, sheet, aRecType, aList);
  aList.IsMainList := IsMainList(aRecType, aList);
  aList.OnDblClick := RecordEdit;
  aList.OnSelectItem := ListSelectItem;
  aList.UpdateTitles();
end;

function TfmBase.CheckModified(): Boolean;
begin
  Result := True;

  if Modified then begin
    case MessageDlg(LSList[LSID_FileSaveQuery], mtWarning, [mbYes, mbNo, mbCancel], 0) of
      mrYes: fmGEDKeeper.miFileSaveClick(nil);
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
  if (rec = nil) then Exit;  

  case rec.RecordType of
    rtIndividual: SelectItemByRec(ListPersons, rec, 0);
    rtFamily: SelectItemByRec(ListFamilies, rec, 1);
    rtNote: SelectItemByRec(ListNotes, rec, 2);
    rtMultimedia: SelectItemByRec(ListMultimedia, rec, 3);
    rtSource: SelectItemByRec(ListSources, rec, 4);
    rtRepository: SelectItemByRec(ListRepositories, rec, 5);
    rtGroup: SelectItemByRec(ListGroups, rec, 6);
    rtResearch: SelectItemByRec(ListResearches, rec, 7);
    rtTask: SelectItemByRec(ListTasks, rec, 8);
    rtCommunication: SelectItemByRec(ListCommunications, rec, 9);
    rtLocation: SelectItemByRec(ListLocations, rec, 10);
  end;
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
          msg := Format(LSList[LSID_ParentsQuery], [GetFamilyStr(fam)]);
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

      FEngine.AddFamilyChild(fam, iChild);

      Result := fam;
    end;
  end;
end;

function TfmBase.DeleteFamilyRecord(aFamily: TGEDCOMFamilyRecord; aConfirm: Boolean): Boolean;
begin
  Result := False;
  if (aFamily = nil) then Exit;
  if (aConfirm) and (MessageDlg(Format(LSList[LSID_FamilyDeleteQuery], [GetFamilyStr(aFamily)]), mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  FEngine.CleanFamily(aFamily);

  RecordNotify(aFamily, rnDelete);
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
  if (aConfirm) and (MessageDlg(Format(LSList[LSID_PersonDeleteQuery], [GetNameStr(iRec)]), mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  // могут быть также ссылки в группах, задачах и коммуникациях

  for i := iRec.ChildToFamilyLinksCount - 1 downto 0 do begin
    family := iRec.ChildToFamilyLinks[i].Family;
    family.DeleteChild(iRec);
  end;

  for i := iRec.SpouseToFamilyLinksCount - 1 downto 0 do begin
    family := iRec.SpouseToFamilyLinks[i].Family;
    FEngine.RemoveFamilySpouse(family, iRec);
  end;

  RecordNotify(iRec, rnDelete);
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
  if (aConfirm) and (MessageDlg(LSList[LSID_NoteDeleteQuery], mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  // могут быть также ссылки в тэгах

  for i := 0 to FTree.RecordsCount - 1 do begin
    rec := FTree.Records[i];

    for k := rec.NotesCount - 1 downto 0 do begin
      if (rec.Notes[k].Value = nRec)
      then rec.DeleteNotes(k);
    end;
  end;

  RecordNotify(nRec, rnDelete);
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
  if (aConfirm) and (MessageDlg(Format(LSList[LSID_SourceDeleteQuery], [srcRec.FiledByEntry]), mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  // могут быть также ссылки в тэгах

  for i := 0 to FTree.RecordsCount - 1 do begin
    rec := FTree.Records[i];

    for k := rec.SourceCitationsCount - 1 downto 0 do begin
      if (rec.SourceCitations[k].Value = srcRec)
      then rec.DeleteSourceCitation(k);
    end;
  end;

  RecordNotify(srcRec, rnDelete);
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
  if (aConfirm) and (MessageDlg(Format(LSList[LSID_MediaDeleteQuery], [mRec.StringValue]), mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  // могут быть также ссылки в тэгах

  for i := 0 to FTree.RecordsCount - 1 do begin
    rec := FTree.Records[i];

    for k := rec.MultimediaLinksCount - 1 downto 0 do begin
      if (rec.MultimediaLinks[k].Value = mRec)
      then rec.DeleteMultimediaLink(k);
    end;
  end;

  RecordNotify(mRec, rnDelete);
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
  if (aConfirm) and (MessageDlg(Format(LSList[LSID_RepositoryDeleteQuery], [repRec.RepositoryName]), mtConfirmation, [mbNo, mbYes], 0) = mrNo)
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

  RecordNotify(repRec, rnDelete);
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
  if (aConfirm) and (MessageDlg(Format(LSList[LSID_GroupDeleteQuery], [groupRec.Name]), mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  for i := 0 to groupRec.MembersCount - 1 do begin
    member := TGEDCOMIndividualRecord(groupRec.Members[i].Value);
    member.DeleteGroup(member.IndexOfGroup(groupRec));
  end;

  RecordNotify(groupRec, rnDelete);
  FTree.Delete(FTree.IndexOfRecord(groupRec));
  Modified := True;

  Result := True;
end;

function TfmBase.DeleteResearchRecord(resRec: TGEDCOMResearchRecord; aConfirm: Boolean): Boolean;
begin
  Result := False;
  if (resRec = nil) then Exit;
  if (aConfirm) and (MessageDlg(Format(LSList[LSID_ResearchDeleteQuery], [resRec.Name]), mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  // dummy, because there is not links from other records and tags

  RecordNotify(resRec, rnDelete);
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
  if (aConfirm) and (MessageDlg(Format(LSList[LSID_TaskDeleteQuery], [GetTaskGoalStr(FTree, TaskRec)]), mtConfirmation, [mbNo, mbYes], 0) = mrNo)
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

  RecordNotify(TaskRec, rnDelete);
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
  if (aConfirm) and (MessageDlg(Format(LSList[LSID_CommunicationDeleteQuery], [ComRec.Name]), mtConfirmation, [mbNo, mbYes], 0) = mrNo)
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

  RecordNotify(ComRec, rnDelete);
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
  if (aConfirm) and (MessageDlg(Format(LSList[LSID_LocationDeleteQuery], [LocRec.Name]), mtConfirmation, [mbNo, mbYes], 0) = mrNo)
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

  RecordNotify(LocRec, rnDelete);
  FTree.Delete(FTree.IndexOfRecord(LocRec));
  Modified := True;

  Result := True;
end;

procedure TfmBase.ListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  data: TGEDCOMRecord;
begin
  data := GetSelectedRecord(TCustomListView(Sender));
  if Selected then NavAdd(data);

  ShowRecordInfo(data);
end;

procedure TfmBase.ShowAddress(anAddress: TGEDCOMAddress; aSummary: TStrings);
var
  k: Integer;
  ts: string;
begin
  if not(anAddress.IsEmpty) and (aSummary <> nil) then begin
    aSummary.Add('    '+LSList[LSID_Address]+':');

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
    aSummary.Add('    '+LSList[LSID_RPSources]+' (' + IntToStr(aDetail.SourceCitationsCount) + '):');
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

procedure TfmBase.RecListIndividualEventsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TGKListView; aSummary: TStrings);
var
  idx, ev: Integer;
  event: TGEDCOMCustomEvent;
  st: string;
  item: TListItem;
begin
  try
    if (aList <> nil)
    then aList.Clear();

    if (aRecord.IndividualEventsCount <> 0) then begin
      if (aSummary <> nil) then begin
        aSummary.Add('');
        aSummary.Add(LSList[LSID_Events]+':');
      end;

      for idx := 0 to aRecord.IndividualEventsCount - 1 do begin
        event := aRecord.IndividualEvents[idx];

        ev := GetPersonEventIndex(event.Name);
        if (ev = 0) then st := event.Detail.Classification
        else
        if (ev > 0) then st := LSList[PersonEvents[ev].Name]
        else st := event.Name;

        if (aSummary <> nil) then begin
          aSummary.Add(st + ': ' + GetEventDesc(event.Detail));
          if (event.StringValue <> '') then aSummary.Add('    ' + event.StringValue);
          ShowDetailCause(event.Detail, aSummary);
          ShowAddress(event.Detail.Address, aSummary);
          ShowDetailInfo(event.Detail, aSummary);
        end;

        if (aList <> nil) then begin
          item := aList.Items.Add();
          item.Caption := IntToStr(idx + 1);
          item.SubItems.Add(st);
          item.SubItems.Add(GEDCOMCustomDateToStr(event.Detail.Date.Value, fmGEDKeeper.Options.DefDateFormat));

          st := event.Detail.Place.StringValue;
          if (event.StringValue <> '')
          then st := st + ' [' + event.StringValue + ']';
          item.SubItems.Add(st);

          item.SubItems.Add(GetEventCause(event.Detail));
          item.Data := event;
        end;
      end;

      if (aList <> nil) then begin
        //ResizeColumn(aList, 2);
        aList.SortColumn := 0;
      end;
    end;
  except
    on E: Exception do LogWrite('GKBase.RecListIndividualEventsRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.RecListFamilyEventsRefresh(aRecord: TGEDCOMFamilyRecord; aList: TGKListView; aSummary: TStrings);
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
        aSummary.Add(LSList[LSID_Events]+':');
      end;

      for idx := 0 to aRecord.FamilyEventCount - 1 do begin
        event := aRecord.FamilyEvents[idx];

        ev := GetFamilyEventIndex(event.Name);
        if (ev = 0) then st := event.Detail.Classification
        else
        if (ev > 0) then st := LSList[FamilyEvents[ev].Name]
        else st := event.Name;

        if (aSummary <> nil) then begin
          aSummary.Add(st + ': ' + GetEventDesc(event.Detail));
          ShowDetailCause(event.Detail, aSummary);
        end;

        ShowDetailInfo(event.Detail, aSummary);

        if (aList <> nil) then begin
          item := aList.Items.Add();
          item.Caption := IntToStr(idx + 1);
          item.SubItems.Add(st);
          item.SubItems.Add(GEDCOMCustomDateToStr(event.Detail.Date.Value, fmGEDKeeper.Options.DefDateFormat));
          item.SubItems.Add(event.Detail.Place.StringValue);
          item.SubItems.Add(GetEventCause(event.Detail));
          item.Data := event;
        end;
      end;
    end;
  except
    on E: Exception do LogWrite('GKBase.RecListFamilyEventsRefresh(): ' + E.Message);
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
        aSummary.Add(LSList[LSID_RPNotes]+' (' + IntToStr(aRecord.NotesCount) + '):');
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
    on E: Exception do LogWrite('GKBase.RecListNotesRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.RecListMediaRefresh(aRecord: TGEDCOMRecord; aList: TGKListView; aSummary: TStrings);
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
        aSummary.Add(LSList[LSID_RPMultimedia]+' (' + IntToStr(aRecord.MultimediaLinksCount) + '):');
      end;

      for idx := 0 to aRecord.MultimediaLinksCount - 1 do begin
        mmLink := aRecord.MultimediaLinks[idx];
        mmRec := TGEDCOMMultimediaRecord(mmLink.Value);

        if (mmRec <> nil) and (mmRec.FileReferencesCount <> 0) then begin
          st := mmRec.FileReferences[0].Title;

          if (aList = nil) and (aSummary <> nil)
          then aSummary.Add('  ' + HyperLink(mmRec.XRef, st) + ' (' + HyperLink(MLinkPrefix+mmRec.XRef, 'просмотр') + ')');

          if (aList <> nil)
          then aList.AddItem(st, mmLink);
        end;
      end;
    end;
  except
    on E: Exception do LogWrite('GKBase.RecListMediaRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.RecListSourcesRefresh(aRecord: TGEDCOMRecord; aList: TGKListView; aSummary: TStrings);
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
        aSummary.Add(LSList[LSID_RPSources]+' (' + IntToStr(aRecord.SourceCitationsCount) + '):');
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
    on E: Exception do LogWrite('GKBase.RecListSourcesRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.SetupRecEventsList(aList: TSheetList; PersonsMode: Boolean);
begin
  aList.Columns_BeginUpdate;
  aList.Columns_Clear;

  aList.AddColumn('№', 25);
  aList.AddColumn(LSList[LSID_Event], 90);
  aList.AddColumn(LSList[LSID_Date], 80);

  if not(PersonsMode)
  then aList.AddColumn(LSList[LSID_Place], 200)
  else aList.AddColumn(LSList[LSID_PlaceAndAttribute], 200);

  aList.AddColumn(LSList[LSID_Cause], 130);

  aList.Columns_EndUpdate;
end;

procedure TfmBase.SetupRecNotesList(aList: TSheetList);
begin
  aList.Columns_BeginUpdate();
  aList.Columns_Clear();

  aList.AddColumn(LSList[LSID_Note], 300);

  aList.Columns_EndUpdate();
end;

procedure TfmBase.SetupRecMediaList(aList: TSheetList);
begin
  aList.Columns_BeginUpdate;
  aList.Columns_Clear;

  aList.AddColumn(LSList[LSID_RPMultimedia], 300);

  aList.Columns_EndUpdate;
end;

procedure TfmBase.SetupRecSourcesList(aList: TSheetList);
begin
  aList.Columns_BeginUpdate;
  aList.Columns_Clear;

  aList.AddColumn(LSList[LSID_Author], 120);
  aList.AddColumn(LSList[LSID_Title], 180);

  aList.Columns_EndUpdate;
end;

procedure TfmBase.RecListAssociationsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TGKListView; aSummary: TStrings);
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
        aSummary.Add(LSList[LSID_Associations]+':');
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
    on E: Exception do LogWrite('GKBase.RecListAssociationsRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.RecListGroupsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TGKListView; aSummary: TStrings);
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
        aSummary.Add(LSList[LSID_RPGroups]+':');
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
    on E: Exception do LogWrite('GKBase.RecListGroupsRefresh(): ' + E.Message);
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
        aSummary.Add(LSList[LSID_Sex] + ': ' + SexStr(iRec.Sex));

        try
          if (iRec.ChildToFamilyLinksCount <> 0) then begin
            aSummary.Add('');
            aSummary.Add(LSList[LSID_Parents]+':');

            family := iRec.ChildToFamilyLinks[0].Family;

            rel_person := TGEDCOMIndividualRecord(family.Husband.Value);
            if (rel_person <> nil)
            then st := HyperLink(rel_person.XRef, GetNameStr(rel_person))
            else st := LSList[LSID_UnkMale];
            aSummary.Add('  '+LSList[LSID_Father]+': ' + st + GetLifeStr(rel_person));

            rel_person := TGEDCOMIndividualRecord(family.Wife.Value);
            if (rel_person <> nil)
            then st := HyperLink(rel_person.XRef, GetNameStr(rel_person))
            else st := LSList[LSID_UnkFemale];
            aSummary.Add('  '+LSList[LSID_Mother]+': ' + st + GetLifeStr(rel_person));
          end;
        except
          on E: Exception do LogWrite('GKBase.ShowPersonInfo().Parents(): ' + E.Message);
        end;

        try
          for idx := 0 to iRec.SpouseToFamilyLinksCount - 1 do begin
            family := iRec.SpouseToFamilyLinks[idx].Family;
            if (family = nil) then begin
              LogWrite('File ('+FileName+'), iRec ('+iRec.XRef+'): empty family entry');
              Continue;
            end;

            if not(IsRecordAccess(family.Restriction, FShieldState))
            then Continue;

            if (iRec.Sex = svMale) then begin
              sp := family.Wife;
              st := LSList[LSID_Wife]+': ';
              unk := LSList[LSID_UnkFemale];
            end else begin
              sp := family.Husband;
              st := LSList[LSID_Husband]+': ';
              unk := LSList[LSID_UnkMale];
            end;

            marr := GetMarriageDate(family, dfDD_MM_YYYY);
            if (marr <> '')
            then marr := LSList[LSID_LMarriage]+' ' + marr
            else marr := LSList[LSID_LFamily];

            rel_person := TGEDCOMIndividualRecord(sp.Value);

            aSummary.Add('');
            if (rel_person <> nil)
            then st := st + HyperLink(rel_person.XRef, GetNameStr(rel_person)) + ' (' + HyperLink(family.XRef, marr) + ')'
            else st := st + unk + ' (' + HyperLink(family.XRef, marr) + ')';
            aSummary.Add({#13#10 + }st);

            if (family.ChildrenCount <> 0) then begin
              aSummary.Add('');
              aSummary.Add(LSList[LSID_Childs]+':');
            end;

            for k := 0 to family.ChildrenCount - 1 do begin
              rel_person := TGEDCOMIndividualRecord(family.Children[k].Value);
              aSummary.Add({#09}'    ' + HyperLink(rel_person.XRef, GetNameStr(rel_person)) + GetLifeStr(rel_person));
            end;
          end;
        except
          on E: Exception do LogWrite('GKBase.ShowPersonInfo().Families(): ' + E.Message);
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
            aSummary.Add(LSList[LSID_Namesakes]+':');
            for k := 0 to namesakes.Count - 1 do begin
              rel_person := (namesakes.Objects[k] as TGEDCOMIndividualRecord);
              aSummary.Add('    '+HyperLink(rel_person.XRef, namesakes[k]));
            end;
          end;
        finally
          namesakes.Free;
        end;
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('GKBase.ShowPersonInfo(): ' + E.Message);
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
        else st := LSList[LSID_UnkMale];
        aSummary.Add(LSList[LSID_Husband]+': ' + st + GetLifeStr(irec));

        irec := TGEDCOMIndividualRecord(aFamily.Wife.Value);
        if (irec <> nil)
        then st := HyperLink(irec.XRef, GetNameStr(irec))
        else st := LSList[LSID_UnkFemale];
        aSummary.Add(LSList[LSID_Wife]+': ' + st + GetLifeStr(irec));

        aSummary.Add('');

        if (aFamily.ChildrenCount <> 0)
        then aSummary.Add(LSList[LSID_Childs]+':');

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
    on E: Exception do LogWrite('GKBase.ShowFamilyInfo(): ' + E.Message);
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
        aSummary.Add('[ ' + HyperLink(MLinkPrefix+aMultimediaRec.XRef, LSList[LSID_View]) + ' ]');
        aSummary.Add('');

        aSummary.Add(LSList[LSID_Links]+':');
        for i := 0 to FTree.RecordsCount - 1 do SearchSubjectLinks(FTree.Records[i], aMultimediaRec, aSummary);

        RecListNotesRefresh(aMultimediaRec, nil, aSummary);
        RecListSourcesRefresh(aMultimediaRec, nil, aSummary);
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('GKBase.ShowMultimediaInfo(): ' + E.Message);
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
        aSummary.Add(LSList[LSID_Links]+':');
        for i := 0 to FTree.RecordsCount - 1 do SearchSubjectLinks(FTree.Records[i], aNoteRec, aSummary);
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('GKBase.ShowNoteInfo(): ' + E.Message);
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
        aSummary.Add(LSList[LSID_Author]+': ' + Trim(aSourceRec.Originator.Text));
        aSummary.Add(LSList[LSID_Title]+': "' + Trim(aSourceRec.Title.Text) + '"');
        aSummary.Add(LSList[LSID_Publication]+': "' + Trim(aSourceRec.Publication.Text) + '"');

        if (aSourceRec.RepositoryCitationsCount > 0) then begin
          aSummary.Add('');
          aSummary.Add(LSList[LSID_RPRepositories]+':');

          for k := 0 to aSourceRec.RepositoryCitationsCount - 1 do begin
            rep := TGEDCOMRepositoryRecord(aSourceRec.RepositoryCitations[k].Value);
            aSummary.Add('    '+HyperLink(rep.XRef, rep.RepositoryName));
          end;
        end;

        aSummary.Add('');
        aSummary.Add(LSList[LSID_Links]+':');
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
    on E: Exception do LogWrite('GKBase.ShowSourceInfo(): ' + E.Message);
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
        ShowAddress(aRepositoryRec.Address, aSummary);

        aSummary.Add('');
        aSummary.Add(LSList[LSID_RPSources]+':');
        for i := 0 to FTree.RecordsCount - 1 do begin
          rec := FTree.Records[i];

          if (rec is TGEDCOMSourceRecord) then begin
            srcRec := (rec as TGEDCOMSourceRecord);

            for k := 0 to srcRec.RepositoryCitationsCount - 1 do
              if (srcRec.RepositoryCitations[k].Value = aRepositoryRec)
              then aSummary.Add('    '+GenRecordLink(FTree, srcRec, False));
          end;
        end;

        RecListNotesRefresh(aRepositoryRec, nil, aSummary);
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('GKBase.ShowRepositoryInfo(): ' + E.Message);
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
        aSummary.Add(LSList[LSID_Members]+' (' + IntToStr(aGroup.MembersCount) + '):');

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
    on E: Exception do LogWrite('GKBase.ShowGroupInfo(): ' + E.Message);
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
        aSummary.Add(LSList[LSID_Title]+': "~ub+1~' + Trim(aResearchRec.Name) + '~bu-1~"');
        aSummary.Add('');
        aSummary.Add(LSList[LSID_Priority]+': ' + LSList[PriorityNames[aResearchRec.Priority]]);
        aSummary.Add(LSList[LSID_Status]+': ' + LSList[StatusNames[aResearchRec.Status]] + ' (' + IntToStr(aResearchRec.Percent) + '%)');
        aSummary.Add(LSList[LSID_StartDate]+': ' + GEDCOMDateToStr(aResearchRec.StartDate));
        aSummary.Add(LSList[LSID_StopDate]+': ' + GEDCOMDateToStr(aResearchRec.StopDate));

        if (aResearchRec.TasksCount > 0) then begin
          aSummary.Add('');
          aSummary.Add(LSList[LSID_RPTasks]+':');
          for i := 0 to aResearchRec.TasksCount - 1 do begin
            taskRec := TGEDCOMTaskRecord(aResearchRec.Tasks[i].Value);
            aSummary.Add('    '+GenRecordLink(FTree, taskRec, False));
          end;
        end;

        if (aResearchRec.CommunicationsCount > 0) then begin
          aSummary.Add('');
          aSummary.Add(LSList[LSID_RPCommunications]+':');
          for i := 0 to aResearchRec.CommunicationsCount - 1 do begin
            corrRec := TGEDCOMCommunicationRecord(aResearchRec.Communications[i].Value);
            aSummary.Add('    '+GenRecordLink(FTree, corrRec, False));
          end;
        end;

        if (aResearchRec.GroupsCount <> 0) then begin
          aSummary.Add('');
          aSummary.Add(LSList[LSID_RPGroups]+':');
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
    on E: Exception do LogWrite('GKBase.ShowResearchInfo(): ' + E.Message);
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
        aSummary.Add(LSList[LSID_Goal]+': ~ub+1~' + GetTaskGoalStr(FTree, aTaskRec) + '~bu-1~');
        aSummary.Add('');
        aSummary.Add(LSList[LSID_Priority]+': ' + LSList[PriorityNames[aTaskRec.Priority]]);
        aSummary.Add(LSList[LSID_StartDate]+': ' + GEDCOMDateToStr(aTaskRec.StartDate));
        aSummary.Add(LSList[LSID_StopDate]+': ' + GEDCOMDateToStr(aTaskRec.StopDate));

        RecListNotesRefresh(aTaskRec, nil, aSummary);
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('GKBase.ShowTaskInfo(): ' + E.Message);
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
        aSummary.Add(LSList[LSID_Theme]+': "~ub+1~' + Trim(aCommunicationRec.Name) + '~bu-1~"');
        aSummary.Add('');
        aSummary.Add(LSList[LSID_Corresponder]+': ' + GetCorresponderStr(FTree, aCommunicationRec, True));
        aSummary.Add(LSList[LSID_Type]+': ' + LSList[CommunicationNames[aCommunicationRec.CommunicationType]]);
        aSummary.Add(LSList[LSID_Date]+': ' + GEDCOMDateToStr(aCommunicationRec.Date));

        RecListNotesRefresh(aCommunicationRec, nil, aSummary);
        RecListMediaRefresh(aCommunicationRec, nil, aSummary);
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('GKBase.ShowCommunicationInfo(): ' + E.Message);
  end;
end;

procedure TfmBase.ShowLocationInfo(aLocationRec: TGEDCOMLocationRecord; aSummary: TStrings);
var
  link_list: TStringList;
  i: Integer;
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
        aSummary.Add(LSList[LSID_Latitude]+': ' + aLocationRec.Map.Lati);
        aSummary.Add(LSList[LSID_Longitude]+': ' + aLocationRec.Map.Long);

        GetLocationLinks(FTree, aLocationRec, link_list);

        link_list.Sort();
        if (link_list.Count > 0) then begin
          aSummary.Add('');
          aSummary.Add(LSList[LSID_Links]+':');
          for i := 0 to link_list.Count - 1 do aSummary.Add('    ' + link_list[i]);
        end;

        RecListNotesRefresh(aLocationRec, nil, aSummary);
        RecListMediaRefresh(aLocationRec, nil, aSummary);
      end;
    finally
      link_list.Free;
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('GKBase.ShowLocationInfo(): ' + E.Message);
  end;
end;

procedure TfmBase.ShowRecordInfo(aRecord: TGEDCOMRecord);
begin
  if (aRecord = nil) then Exit;

  try
    case aRecord.RecordType of
      rtIndividual: ShowPersonInfo(TGEDCOMIndividualRecord(aRecord), mPersonSummary.Lines);
      rtFamily: ShowFamilyInfo(TGEDCOMFamilyRecord(aRecord), mFamilySummary.Lines);
      rtNote: ShowNoteInfo(TGEDCOMNoteRecord(aRecord), mNoteSummary.Lines);
      rtMultimedia: ShowMultimediaInfo(TGEDCOMMultimediaRecord(aRecord), mMediaSummary.Lines);
      rtSource: ShowSourceInfo(TGEDCOMSourceRecord(aRecord), mSourceSummary.Lines);
      rtRepository: ShowRepositoryInfo(TGEDCOMRepositoryRecord(aRecord), mRepositorySummary.Lines);
      rtGroup: ShowGroupInfo(TGEDCOMGroupRecord(aRecord), mGroupSummary.Lines);
      rtResearch: ShowResearchInfo(TGEDCOMResearchRecord(aRecord), mResearchSummary.Lines);
      rtTask: ShowTaskInfo(TGEDCOMTaskRecord(aRecord), mTaskSummary.Lines);
      rtCommunication: ShowCommunicationInfo(TGEDCOMCommunicationRecord(aRecord), mCommunicationSummary.Lines);
      rtLocation: ShowLocationInfo(TGEDCOMLocationRecord(aRecord), mLocationSummary.Lines);
    end;
  except
    on E: Exception do LogWrite('GKBase.ShowRecordInfo(): ' + E.Message);
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
    if (MessageDlg(LSList[LSID_RemoveEventQuery], mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    if (aRecord is TGEDCOMIndividualRecord)
    then TGEDCOMIndividualRecord(aRecord).DeleteIndividualEvent(aEvent)
    else TGEDCOMFamilyRecord(aRecord).DeleteFamilyEvent(TGEDCOMFamilyEvent(aEvent));

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
          event := fmEventEdit.Event;

          if (aRecord is TGEDCOMIndividualRecord)
          then TGEDCOMIndividualRecord(aRecord).AddIndividualEvent(event)
          else TGEDCOMFamilyRecord(aRecord).AddFamilyEvent(TGEDCOMFamilyEvent(event));
        end else begin
          // hack, need refactoring (this for replacing event vs attr in his editor)
          if (aRecord is TGEDCOMIndividualRecord) and (fmEventEdit.Event <> aEvent) then begin
            TGEDCOMIndividualRecord(aRecord).DeleteIndividualEvent(aEvent);
            event := fmEventEdit.Event;
            TGEDCOMIndividualRecord(aRecord).AddIndividualEvent(event);
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
    MessageDlg(LSList[LSID_IsNotDefinedSex], mtError, [mbOk], 0);
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
        then FEngine.AddFamilySpouse(aFamilyRec, aPerson);
      ftChild:
        if (aPerson <> nil)
        then FEngine.AddFamilyChild(aFamilyRec, aPerson);
    end;

    fmFamEdit.Family := aFamilyRec;

    Result := (ShowModalEx(fmFamEdit, Self) = mrOk);

    if (Result) then begin
      if not(exists)
      then FTree.AddRecord(aFamilyRec);
    end else begin
      if not(exists) then begin
        FEngine.CleanFamily(aFamilyRec); // очищает и вставленных супругов и детей
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

    fmRepEdit.Repository := aRepRec;

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
    if (MessageDlg(LSList[LSID_DetachNoteQuery], mtConfirmation, [mbNo, mbYes], 0) = mrNo)
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
    noteRec := TGEDCOMNoteRecord(SelectRecord(rtNote, []));
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
    if (MessageDlg(LSList[LSID_DetachMultimediaQuery], mtConfirmation, [mbNo, mbYes], 0) = mrNo)
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
    mmRec := TGEDCOMMultimediaRecord(SelectRecord(rtMultimedia, []));
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
    if (MessageDlg(LSList[LSID_DetachSourceQuery], mtConfirmation, [mbNo, mbYes], 0) = mrNo)
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
    if (MessageDlg(LSList[LSID_RemoveUserRefQuery], mtConfirmation, [mbNo, mbYes], 0) = mrNo)
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
    if (MessageDlg(LSList[LSID_RemoveAssociationQuery], mtConfirmation, [mbNo, mbYes], 0) = mrNo)
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
    if (MessageDlg(LSList[LSID_DetachNoteQuery], mtConfirmation, [mbNo, mbYes], 0) = mrNo)
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
    noteRec := TGEDCOMNoteRecord(SelectRecord(rtNote, []));
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
    if (MessageDlg(LSList[LSID_DetachMultimediaQuery], mtConfirmation, [mbNo, mbYes], 0) = mrNo)
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
    mmRec := TGEDCOMMultimediaRecord(SelectRecord(rtMultimedia, []));
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
    if (MessageDlg(LSList[LSID_DetachSourceQuery], mtConfirmation, [mbNo, mbYes], 0) = mrNo)
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

function TfmBase.GetFileName: string;
begin
  Result := FEngine.FileName;
end;

procedure TfmBase.SetFileName(const Value: string);
begin
  FEngine.FileName := Value;

  SetMainTitle();

  fmGEDKeeper.Options.LastDir := ExtractFilePath(FEngine.FileName);
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

    aToList.Add('    ' + prefix + GenRecordLink(FTree, aRec, True) + suffix);
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
    end
    else
    if (aInRecord is TGEDCOMFamilyRecord) then begin
      f_rec := (aInRecord as TGEDCOMFamilyRecord);

      for k := 0 to f_rec.FamilyEventCount - 1 do
        PrepareEvent(f_rec, f_rec.FamilyEvents[k]);
    end;
  except
    on E: Exception do LogWrite('GKBase.SearchSubjectLinks(): ' + E.Message);
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
var
  fmMediaView: TfmMediaView;
begin
  fmMediaView := TfmMediaView.Create(Self);
  try
    fmMediaView.FileRef := aMediaRec.FileReferences[0];
    if not(fmMediaView.Extern) then ShowModalEx(fmMediaView);
  finally
    fmMediaView.Destroy;
  end;
end;

procedure TfmBase.SetModified(const Value: Boolean);
begin
  FModified := Value;
  SetMainTitle();
end;

procedure TfmBase.SetMainTitle();
begin
  Caption := ExtractFileName(FileName);

  if FModified
  then Caption := '* ' + Caption;
end;

procedure TfmBase.FileNew();
begin
  ChangesClear();
  Clear();

  ListsRefresh();
  ShowPersonInfo(nil, mPersonSummary.Lines);
  FileName := LSList[LSID_Unknown];
  Modified := False;
end;

procedure TfmBase.FileLoad(aFileName: string);
begin
  ChangesClear();
  Clear();

  try
    {$IFDEF PROFILER}Profiler.Mark(1, True);{$ENDIF}
    FTree.LoadFromFile(aFileName);
    {$IFDEF PROFILER}Profiler.Mark(1, False);{$ENDIF}
  except
    on E: Exception do begin
      LogWrite('GKBase.FileLoad().TreeLoad(): ' + E.Message);
      MessageDlg(LSList[LSID_LoadGedComFailed], mtError, [mbOk], 0);
    end;
  end;

  try
    CheckGEDCOMFormat(FTree);
  except
    on E: Exception do begin
      LogWrite('GKBase.FileLoad().CheckFormat(): ' + E.Message);
      MessageDlg(LSList[LSID_CheckGedComFailed], mtError, [mbOk], 0);
    end;
  end;

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
  subm := FTree.Header.GetTagStringValue('SUBM');
  is_advanced := IsAdvanced();
  ext_name := FTree.Header.GetTagStringValue(ExtTag);

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
  fam: TGEDCOMFamilyRecord;
  note: TGEDCOMNoteRecord;
  mm_rec: TGEDCOMMultimediaRecord;
  src: TGEDCOMSourceRecord;
  rep: TGEDCOMRepositoryRecord;
  grp: TGEDCOMGroupRecord;
  rsr: TGEDCOMResearchRecord;
  tsk: TGEDCOMTaskRecord;
  comm: TGEDCOMCommunicationRecord;
  loc: TGEDCOMLocationRecord;
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
      fam := TGEDCOMFamilyRecord(rec);
      res := ModifyFamily(fam);
      rec := fam;
    end;

    2: begin // заметки
      note := TGEDCOMNoteRecord(rec);
      res := ModifyNote(note);
      rec := note;
    end;

    3: begin // мультимедиа
      mm_rec := TGEDCOMMultimediaRecord(rec);
      res := ModifyMedia(mm_rec);
      rec := mm_rec;
    end;

    4: begin // источники
      src := TGEDCOMSourceRecord(rec);
      res := ModifySource(src);
      rec := src;
    end;

    5: begin // архивы
      rep := TGEDCOMRepositoryRecord(rec);
      res := ModifyRepository(rep);
      rec := rep;
    end;

    6: begin // группы
      grp := TGEDCOMGroupRecord(rec);
      res := ModifyGroup(grp);
      rec := grp;
    end;

    7: begin // исследования
      rsr := TGEDCOMResearchRecord(rec);
      res := ModifyResearch(rsr);
      rec := rsr;
    end;

    8: begin // задачи
      tsk := TGEDCOMTaskRecord(rec);
      res := ModifyTask(tsk);
      rec := tsk;
    end;

    9: begin // переписка
      comm := TGEDCOMCommunicationRecord(rec);
      res := ModifyCommunication(comm);
      rec := comm;
    end;

    10: begin // места
      loc := TGEDCOMLocationRecord(rec);
      res := ModifyLocation(loc);
      rec := loc;
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
  ind: TGEDCOMIndividualRecord;
  fam: TGEDCOMFamilyRecord;
  note: TGEDCOMNoteRecord;
  mm_rec: TGEDCOMMultimediaRecord;
  src: TGEDCOMSourceRecord;
  rep: TGEDCOMRepositoryRecord;
  grp: TGEDCOMGroupRecord;
  rsr: TGEDCOMResearchRecord;
  tsk: TGEDCOMTaskRecord;
  comm: TGEDCOMCommunicationRecord;
  loc: TGEDCOMLocationRecord;
  res: Boolean;
begin
  res := False;

  case PageRecords.TabIndex of
    0: begin // персоны
      rec := GetSelectedRecord(ListPersons);
      ind := TGEDCOMIndividualRecord(rec);
      res := ModifyPerson(ind);
    end;

    1: begin // семьи
      rec := GetSelectedRecord(ListFamilies);
      fam := TGEDCOMFamilyRecord(rec);
      res := ModifyFamily(fam);
    end;

    2: begin // заметки
      rec := GetSelectedRecord(ListNotes);
      note := TGEDCOMNoteRecord(rec);
      res := ModifyNote(note);
    end;

    3: begin // мультимедиа
      rec := GetSelectedRecord(ListMultimedia);
      mm_rec := TGEDCOMMultimediaRecord(rec);
      res := ModifyMedia(mm_rec);
    end;

    4: begin // источники
      rec := GetSelectedRecord(ListSources);
      src := TGEDCOMSourceRecord(rec);
      res := ModifySource(src);
    end;

    5: begin // архивы
      rec := GetSelectedRecord(ListRepositories);
      rep := TGEDCOMRepositoryRecord(rec);
      res := ModifyRepository(rep);
    end;

    6: begin // группы
      rec := GetSelectedRecord(ListGroups);
      grp := TGEDCOMGroupRecord(rec);
      res := ModifyGroup(grp);
    end;

    7: begin // исследования
      rec := GetSelectedRecord(ListResearches);
      rsr := TGEDCOMResearchRecord(rec);
      res := ModifyResearch(rsr);
    end;

    8: begin // задачи
      rec := GetSelectedRecord(ListTasks);
      tsk := TGEDCOMTaskRecord(rec);
      res := ModifyTask(tsk);
    end;

    9: begin // переписка
      rec := GetSelectedRecord(ListCommunications);
      comm := TGEDCOMCommunicationRecord(rec);
      res := ModifyCommunication(comm);
    end;

    10: begin // места
      rec := GetSelectedRecord(ListLocations);
      loc := TGEDCOMLocationRecord(rec);
      res := ModifyLocation(loc);
    end;
  end;

  if (res) then begin
    ListsRefresh();
    ShowRecordInfo(rec);
  end;
end;

function TfmBase.DeleteRecord(aRecord: TGEDCOMRecord; aConfirm: Boolean): Boolean;
begin
  Result := False;
  if (aRecord = nil) then Exit;  

  case aRecord.RecordType of
    rtIndividual: Result := DeleteIndividualRecord(aRecord as TGEDCOMIndividualRecord, aConfirm);
    rtFamily: Result := DeleteFamilyRecord(aRecord as TGEDCOMFamilyRecord, aConfirm);
    rtNote: Result := DeleteNoteRecord(aRecord as TGEDCOMNoteRecord, aConfirm);
    rtMultimedia: Result := DeleteMediaRecord(aRecord as TGEDCOMMultimediaRecord, aConfirm);
    rtSource: Result := DeleteSourceRecord(aRecord as TGEDCOMSourceRecord, aConfirm);
    rtRepository: Result := DeleteRepositoryRecord(aRecord as TGEDCOMRepositoryRecord, aConfirm);
    rtGroup: Result := DeleteGroupRecord(aRecord as TGEDCOMGroupRecord, aConfirm);
    rtResearch: Result := DeleteResearchRecord(aRecord as TGEDCOMResearchRecord, aConfirm);
    rtTask: Result := DeleteTaskRecord(aRecord as TGEDCOMTaskRecord, aConfirm);
    rtCommunication: Result := DeleteCommunicationRecord(aRecord as TGEDCOMCommunicationRecord, aConfirm);
    rtLocation: Result := DeleteLocationRecord(aRecord as TGEDCOMLocationRecord, aConfirm);
  end;
end;

procedure TfmBase.RecordDelete();
var
  res: Boolean;
begin
  res := False;

  case PageRecords.TabIndex of
    0: res := DeleteRecord(GetSelectedRecord(ListPersons), True); // персоны
    1: res := DeleteRecord(GetSelectedRecord(ListFamilies), True); // семьи
    2: res := DeleteRecord(GetSelectedRecord(ListNotes), True); // заметки
    3: res := DeleteRecord(GetSelectedRecord(ListMultimedia), True); // мультимедиа
    4: res := DeleteRecord(GetSelectedRecord(ListSources), True); // источники
    5: res := DeleteRecord(GetSelectedRecord(ListRepositories), True); // архивы
    6: res := DeleteRecord(GetSelectedRecord(ListGroups), True); // группы
    7: res := DeleteRecord(GetSelectedRecord(ListResearches), True); // исследования
    8: res := DeleteRecord(GetSelectedRecord(ListTasks), True); // задачи
    9: res := DeleteRecord(GetSelectedRecord(ListCommunications), True); // переписка
    10: res := DeleteRecord(GetSelectedRecord(ListLocations), True); // места
  end;

  if (res) then ListsRefresh();
end;

procedure TfmBase.ExportToExcel(appmode: Boolean);
var
  ex_exp: TExcelExporter;
begin
  ex_exp := TExcelExporter.Create(FEngine, GetCurFileTempPath());
  try
    ex_exp.Options := fmGEDKeeper.Options;
    ex_exp.SelectedRecords := ListPersons.ContentList;
    ex_exp.AppMode := appmode;
    ex_exp.Generate();
  finally
    ex_exp.Destroy;
  end;
end;

function TfmBase.GetCurFileTempPath(): string;
begin
  Result := ExtractFilePath(FileName) + '~temp\';
end;

procedure TfmBase.ExportToWeb();
var
  web: TWebExporter;
begin
  web := TWebExporter.Create(FEngine, GetCurFileTempPath());
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
  if not TfmChart.CheckData(FTree, GetSelectedPerson(), ckAncestors) then Exit;

  fmChart := TfmChart.Create(Self);
  fmChart.Base := Self;
  fmChart.Tree := FTree;
  fmChart.Person := GetSelectedPerson();
  fmChart.ChartKind := ckAncestors;
  fmChart.FileName := ExtractFileName(FileName);
  fmChart.GenChart();
end;

procedure TfmBase.ShowTreeDescendants();
var
  fmChart: TfmChart;
begin
  if not TfmChart.CheckData(FTree, GetSelectedPerson(), ckDescendants) then Exit;

  fmChart := TfmChart.Create(Self);
  fmChart.Base := Self;
  fmChart.Tree := FTree;
  fmChart.Person := GetSelectedPerson();
  fmChart.ChartKind := ckDescendants;
  fmChart.FileName := ExtractFileName(FileName);
  fmChart.GenChart();
end;

procedure TfmBase.ShowTreeBoth();
var
  fmChart: TfmChart;
begin
  if not TfmChart.CheckData(FTree, GetSelectedPerson(), ckBoth) then Exit;

  fmChart := TfmChart.Create(Self);
  fmChart.Base := Self;
  fmChart.Tree := FTree;
  fmChart.Person := GetSelectedPerson();
  fmChart.ChartKind := ckBoth;
  fmChart.FileName := ExtractFileName(FileName);
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
{$IFNDEF DELPHI_NET}
var
  fmMaps: TfmMaps;
{$ENDIF}
begin
  {$IFNDEF DELPHI_NET}
  fmMaps := TfmMaps.Create(Self);
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
  rt := aRecord.RecordType;
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

function TfmBase.IsAdvanced(): Boolean;
begin
  Result := (FTree.Header.FindTag(AdvTag) <> nil);
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
    dlg.Mode := rtIndividual;

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
    dlg.Mode := rtFamily;

    if (ShowModalEx(dlg, Self) = mrOk)
    then Result := TGEDCOMFamilyRecord(dlg.ResultRecord)
    else Result := nil;
  finally
    dlg.Destroy;
  end;
end;

function TfmBase.SelectRecord(aMode: TGEDCOMRecordType; anArgs: array of const): TGEDCOMRecord;
var
  dlg: TfmRecordSelect;
  args_cnt: Integer;
begin
  dlg := TfmRecordSelect.Create(Self);
  try
    dlg.Mode := aMode;

    {$IFNDEF DELPHI_NET}
    args_cnt := High(anArgs) + 1;
    if (args_cnt > 0) and (anArgs[0].VType = vtAnsiString)
    then dlg.edFastFilter.Text := string(anArgs[0].VAnsiString);
    {$ENDIF}

    if (ShowModalEx(dlg, Self) = mrOk)
    then Result := dlg.ResultRecord
    else Result := nil;
  finally
    dlg.Destroy;
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
  try
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
          then birth_days.Add(Format(LSList[LSID_DaysRemained], [nm, days]));
        end;
      end;

      if (birth_days.Count > 0)
      then fmGEDKeeper.Options.ShowTips := TfmTipsDialog.ShowTipsEx(LSList[LSID_BirthDays], fmGEDKeeper.Options.ShowTips, birth_days);
    finally
      birth_days.Free;
    end;
  except
    on E: Exception do LogWrite('GKBase.ShowTips(): ' + E.Message);
  end;
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

procedure TfmBase.RecordNotify(aRecord: TGEDCOMRecord; aNotify: TRecNotify);
var
  list: TRecordsView;
begin
  if (aRecord = nil) then Exit;  

  list := nil;
  case aRecord.RecordType of
    rtIndividual: list := ListPersons;
    rtFamily: list := ListFamilies;
    rtNote: list := ListNotes;
    rtMultimedia: list := ListMultimedia;
    rtSource: list := ListSources;
    rtRepository: list := ListRepositories;
    rtGroup: list := ListGroups;
    rtResearch: list := ListResearches;
    rtTask: list := ListTasks;
    rtCommunication: list := ListCommunications;
    rtLocation: list := ListLocations;
  end;

  if (list <> nil) then begin
    if (aNotify = rnDelete) then list.DeleteRecord(aRecord);
  end;
end;

procedure TfmBase.ShowScriptDaemon();
begin
  fmScriptDaemon := TfmScriptDaemon.Create(Self);
  try
    ShowModalEx(fmScriptDaemon, Self);
  finally
    fmScriptDaemon.Destroy;
  end;
end;

function TfmBase.RecordIsFiltered(aRecord: TGEDCOMRecord): Boolean;
begin
  Result := False;
  if (aRecord = nil) then Exit;  

  case aRecord.RecordType of
    rtIndividual: Result := (ListPersons.ContentList.IndexOf(aRecord) >= 0);
    rtFamily: Result := (ListFamilies.ContentList.IndexOf(aRecord) >= 0);
    rtNote: Result := (ListNotes.ContentList.IndexOf(aRecord) >= 0);
    rtMultimedia: Result := (ListMultimedia.ContentList.IndexOf(aRecord) >= 0);
    rtSource: Result := (ListSources.ContentList.IndexOf(aRecord) >= 0);
    rtRepository: Result := (ListRepositories.ContentList.IndexOf(aRecord) >= 0);
    rtGroup: Result := (ListGroups.ContentList.IndexOf(aRecord) >= 0);
    rtResearch: Result := (ListResearches.ContentList.IndexOf(aRecord) >= 0);
    rtTask: Result := (ListTasks.ContentList.IndexOf(aRecord) >= 0);
    rtCommunication: Result := (ListCommunications.ContentList.IndexOf(aRecord) >= 0);
    rtLocation: Result := (ListLocations.ContentList.IndexOf(aRecord) >= 0);
  end;
end;

function TfmBase.ModifyName(var aName: TName): Boolean;
var
  dlg: TfmNameEdit;
begin
  dlg := TfmNameEdit.Create(Self);
  try
    dlg.IName := aName;
    Result := (ShowModalEx(dlg, Self) = mrOk);
  finally
    dlg.Destroy;
  end;
end;

function TfmBase.DefinePatronymic(aName: string; aSex: TGEDCOMSex; aConfirm: Boolean): string;
var
  n: TName;
begin
  Result := '';

  n := fmGEDKeeper.NamesTable.FindName(aName);
  if (n = nil) then begin
    if (aConfirm)
    then n := fmGEDKeeper.NamesTable.AddName(aName)
    else Exit;
  end;

  case aSex of
    svMale: Result := n.M_Patronymic;
    svFemale: Result := n.F_Patronymic;
  end;

  if (Result = '') then begin
    if (aConfirm)
    then ModifyName(n)
    else Exit;
  end;

  case aSex of
    svMale: Result := n.M_Patronymic;
    svFemale: Result := n.F_Patronymic;
  end;
end;

end.
