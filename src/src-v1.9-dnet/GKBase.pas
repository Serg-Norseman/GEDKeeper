unit GKBase; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.IO, System.Drawing, System.ComponentModel, System.Windows.Forms,
  System.Text, VCLStub,
  GKEngine, GKCommon, GedCom551, GKCtrls, GKLists, GKLangs;

type
  TfmBase = class(System.Windows.Forms.Form, ILocalization)
  strict private
    ImageList1: System.Windows.Forms.ImageList;
    components: System.ComponentModel.IContainer;

    FBackman: TBackManager;
    FChangedRecords: array [TGEDCOMRecord.TGEDCOMRecordType] of TList;
    FEngine: TGenEngine;
    FLockedRecords: TList;
    FModified: Boolean;
    FShieldState: TGenEngine.TShieldState;
    FTree: TGEDCOMTree;
    FUndoman: TUndoManager;
    FXFilter: TPersonsFilter;

    procedure OutLink(aSubject: TGEDCOMRecord; aToList: TStrings;
      aRec: TGEDCOMRecord; aTag: TGEDCOMTag; aExt: TGEDCOMPointer);
    procedure PrepareEvent(aSubject: TGEDCOMRecord; aToList: TStrings;
      aRec: TGEDCOMRecord; event: TGEDCOMCustomEvent);
    function GetFamilyBySpouse(aNewParent: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;
    procedure IntUpdate(aRecView: TRecordsView; ASCol: Integer; aTitles: Boolean = False);

    function  IsMainList(aRecType: TGEDCOMRecord.TGEDCOMRecordType; aList: TGKListView): Boolean;
    procedure List_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
    procedure NavAdd(aRec: TGEDCOMRecord);
    procedure SetFileName(const Value: string);
    procedure SetMainTitle();
    procedure SetModified(const Value: Boolean);
    procedure ShowAddress(anAddress: TGEDCOMAddress; aSummary: TStrings);
    procedure SetShieldState(const Value: TGenEngine.TShieldState);
    function GetFileName(): string;

    procedure mPersonSummaryLink(sender: System.Object; LinkName: String);
    procedure FormActivate(sender: System.Object; e: System.EventArgs);
    procedure FormDeactivate(sender: System.Object; e: System.EventArgs);
    procedure InitializeComponent;
    procedure PageRecords_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
    procedure TfmBase_Closing(sender: System.Object; e: System.ComponentModel.CancelEventArgs);
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  public
    type
      TFilePropertiesMode = (fpmAuthor, fpmDiags, fpmAdvanced);

      TRecCount = record
        Total: Integer;
        Filtered: Integer;
      end;

      TRecNotify = (rnDelete);

  var
    PageRecords: System.Windows.Forms.TabControl;

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

    FCounts: array [TGEDCOMRecord.TGEDCOMRecordType] of TRecCount;

    constructor Create;

    procedure ApplyFilter();
    procedure ChangeRecord(aRecord: TGEDCOMRecord);
    procedure ChangesClear();
    function  CheckModified(): Boolean;
    procedure Clear();

    procedure CreateListView(aOwner: System.Windows.Forms.Control; var aList: TGKListView); overload;
    procedure CreatePage(aPageText: string; aRecType: TGEDCOMRecord.TGEDCOMRecordType;
      var aList: TRecordsView; var aSummary: TGKHyperView);
    function  CreatePersonDialog(aTarget: TGEDCOMIndividualRecord;
      aTargetMode: TGenEngine.TTargetMode; aNeedSex: TGEDCOMObject.TGEDCOMSex): TGEDCOMIndividualRecord;
    procedure CreateRecordsView(aParent: System.Windows.Forms.Control;
      aRecordType: TGEDCOMRecord.TGEDCOMRecordType; var aList: TRecordsView);
    procedure ExportToExcel(appmode: Boolean);
    procedure ExportToWeb();
    procedure FileLoad(aFileName: string);
    procedure FileNew();
    function  FileProperties(aMode: TFilePropertiesMode = fpmAuthor): System.Windows.Forms.DialogResult;
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
    procedure RecordEdit(sender: System.Object; e: System.EventArgs);
    procedure RecordNotify(aRecord: TGEDCOMRecord; aNotify: TRecNotify);
    procedure SearchSubjectLinks(aInRecord, aSubject: TGEDCOMRecord; aToList: TStrings);
    function  SelectFamily(aTarget: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;
    function  SelectPerson(aTarget: TGEDCOMIndividualRecord; aTargetMode: TGenEngine.TTargetMode;
      aNeedSex: TGEDCOMObject.TGEDCOMSex): TGEDCOMIndividualRecord;
    function  SelectRecord(aMode: TGEDCOMRecord.TGEDCOMRecordType; anArgs: array of const): TGEDCOMRecord;
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

    function ModifyName(var aName: TNamesTable.TName): Boolean;
    function DefinePatronymic(aName: string; aSex: TGEDCOMObject.TGEDCOMSex; aConfirm: Boolean): string;

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
    procedure DoPersonChangeSex(aPerson: TGEDCOMIndividualRecord; NewSex: TGEDCOMObject.TGEDCOMSex);
    procedure DoPersonChangePatriarch(aPerson: TGEDCOMIndividualRecord; NewValue: Boolean);

    function ModifyPerson(var aIndivRec: TGEDCOMIndividualRecord): Boolean;
    function ModifyFamily(var aFamilyRec: TGEDCOMFamilyRecord;
      aTarget: TGenEngine.TFamilyTarget = ftNone; aPerson: TGEDCOMIndividualRecord = nil): Boolean;
    function ModifyNote(var aNoteRec: TGEDCOMNoteRecord): Boolean;
    function ModifyMedia(var aMediaRec: TGEDCOMMultimediaRecord): Boolean;
    function ModifySource(var aSourceRec: TGEDCOMSourceRecord): Boolean;
    function ModifyRepository(var aRepRec: TGEDCOMRepositoryRecord): Boolean;
    function ModifyGroup(var aGroupRec: TGEDCOMGroupRecord): Boolean;
    function ModifyResearch(var aResearchRec: TGEDCOMResearchRecord): Boolean;
    function ModifyTask(var aTaskRec: TGEDCOMTaskRecord): Boolean;
    function ModifyCommunication(var aCommunicationRec: TGEDCOMCommunicationRecord): Boolean;
    function ModifyLocation(var aLocationRec: TGEDCOMLocationRecord): Boolean;

    function ModifyAddress(aSender: System.Windows.Forms.Form;
      anAddress: TGEDCOMAddress): Boolean;

    function ModifyRecAssociation(aSender: System.Windows.Forms.Form;
      aRecord: TGEDCOMIndividualRecord; aAssociation: TGEDCOMAssociation; anAction: TGenEngine.TRecAction): Boolean;
    function ModifyRecEvent(aSender: System.Windows.Forms.Form;
      aRecord: TGEDCOMRecord; aEvent: TGEDCOMCustomEvent; anAction: TGenEngine.TRecAction): Boolean;
    function ModifyRecMultimedia(aSender: System.Windows.Forms.Form;
      aRecord: TGEDCOMRecord; aLink: TGEDCOMMultimediaLink; anAction: TGenEngine.TRecAction): Boolean;
    function ModifyRecNote(aSender: System.Windows.Forms.Form;
      aRecord: TGEDCOMRecord; aNote: TGEDCOMNotes; anAction: TGenEngine.TRecAction): Boolean;
    function ModifyRecSource(aSender: System.Windows.Forms.Form;
      aRecord: TGEDCOMRecord; aCit: TGEDCOMSourceCitation; anAction: TGenEngine.TRecAction): Boolean;
    function ModifyRecUserRef(aSender: System.Windows.Forms.Form;
      aRecord: TGEDCOMRecord; aUserRef: TGEDCOMUserReference; anAction: TGenEngine.TRecAction): Boolean;

    function ModifyTagMultimedia(aTag: TGEDCOMTagWithLists;
      aLink: TGEDCOMMultimediaLink; anAction: TGenEngine.TRecAction): Boolean;
    function ModifyTagNote(aTag: TGEDCOMTagWithLists;
      aNote: TGEDCOMNotes; anAction: TGenEngine.TRecAction): Boolean;
    function ModifyTagSource(aTag: TGEDCOMTagWithLists;
      aCit: TGEDCOMSourceCitation; anAction: TGenEngine.TRecAction): Boolean;

    procedure RecListAssociationsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TGKListView; aSummary: TStrings);
    procedure RecListFamilyEventsRefresh(aRecord: TGEDCOMFamilyRecord; aList: TGKListView; aSummary: TStrings);
    procedure RecListGroupsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TGKListView; aSummary: TStrings);
    procedure RecListIndividualEventsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TGKListView; aSummary: TStrings);
    procedure RecListMediaRefresh(aRecord: TGEDCOMRecord; aList: TGKListView; aSummary: TStrings);
    procedure RecListNotesRefresh(aRecord: TGEDCOMRecord; aList: TGKListView; aSummary: TStrings);
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
    property ShieldState: TGenEngine.TShieldState read FShieldState write SetShieldState;
    property Tree: TGEDCOMTree read FTree;
    property Undoman: TUndoManager read FUndoman;
  end;

implementation

uses
  GKUtils, GKPersonNew, GKRecordSelect, GKStats, GKNoteEdit, GKChart, GKTipsDlg,
  GKSourceEdit, GKEventEdit, GKChartCore, GKFileProperties, GKTaskEdit, GKMain,
  GKPersonEdit, GKFamilyEdit, GKAssociationEdit, GKFilter, GKTreeTools, GKExport,
  GKGroupEdit, GKPersonScan, GKProgress, GKSourceCitEdit, GKRepositoryEdit,
  GKMediaEdit, GKResearchEdit, GKCommunicationEdit, GKLocationEdit, GKCommands,
  GKUserRefEdit, GKTimeLine, GKOrganizer, GKAddressEdit, GKScriptDaemon, GKMaps,
  GKNameEdit, GKMediaView;

constructor TfmBase.Create;
var
  rt: TGEDCOMRecord.TGEDCOMRecordType;
begin
  inherited Create;
  InitializeComponent;

  for rt := Low(TGEDCOMRecord.TGEDCOMRecordType) to High(TGEDCOMRecord.TGEDCOMRecordType) do
    FChangedRecords[rt] := TList.Create;

  FEngine := TGenEngine.Create;
  FTree := FEngine.Tree;

  FXFilter := TPersonsFilter.Create;

  FLockedRecords := TList.Create;

  FBackman := TBackManager.Create;
  FUndoman := TUndoManager.Create(FTree, manualCommit);

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

  PageRecords.SelectedIndex := 0;
end;

procedure TfmBase.Dispose(Disposing: Boolean);
var
  rt: TGEDCOMRecord.TGEDCOMRecordType;
begin
  if Disposing then begin
    //ListPersons.Clear;

    FBackman.Destroy;
    FUndoman.Destroy;

    FLockedRecords.Free;

    FXFilter.Free;

    FTree := nil;
    FEngine.Destroy;

    for rt := Low(TGEDCOMRecord.TGEDCOMRecordType) to High(TGEDCOMRecord.TGEDCOMRecordType) do
      FChangedRecords[rt].Free;
  end;
  inherited Dispose(Disposing);
end;

procedure TfmBase.SetLang();
begin
  PageRecords.TabPages[ 0].Text := LSList[LSID_RPIndividuals];
  PageRecords.TabPages[ 1].Text := LSList[LSID_RPFamilies];
  PageRecords.TabPages[ 2].Text := LSList[LSID_RPNotes];
  PageRecords.TabPages[ 3].Text := LSList[LSID_RPMultimedia];
  PageRecords.TabPages[ 4].Text := LSList[LSID_RPSources];
  PageRecords.TabPages[ 5].Text := LSList[LSID_RPRepositories];
  PageRecords.TabPages[ 6].Text := LSList[LSID_RPGroups];
  PageRecords.TabPages[ 7].Text := LSList[LSID_RPResearches];
  PageRecords.TabPages[ 8].Text := LSList[LSID_RPTasks];
  PageRecords.TabPages[ 9].Text := LSList[LSID_RPCommunications];
  PageRecords.TabPages[10].Text := LSList[LSID_RPLocations];
end;

procedure TfmBase.InitializeComponent;
begin
  Self.components := System.ComponentModel.Container.Create;
  Self.ImageList1 := System.Windows.Forms.ImageList.Create(Self.components);
  Self.PageRecords := System.Windows.Forms.TabControl.Create;
  Self.SuspendLayout;
  // 
  // ImageList1
  // 
  Self.ImageList1.ImageSize := System.Drawing.Size.Create(16, 16);
  Self.ImageList1.TransparentColor := System.Drawing.Color.Transparent;
  // 
  // PageRecords
  // 
  Self.PageRecords.Dock := System.Windows.Forms.DockStyle.Fill;
  Self.PageRecords.Location := System.Drawing.Point.Create(0, 0);
  Self.PageRecords.Name := 'PageRecords';
  Self.PageRecords.SelectedIndex := 0;
  Self.PageRecords.Size := System.Drawing.Size.Create(762, 290);
  Self.PageRecords.TabIndex := 0;
  Include(Self.PageRecords.SelectedIndexChanged, Self.PageRecords_SelectedIndexChanged);
  // 
  // TfmBase
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.ClientSize := System.Drawing.Size.Create(762, 290);
  Self.Controls.Add(Self.PageRecords);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.Name := 'TfmBase';
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'GEDKeeper';
  Include(Self.Closing, Self.TfmBase_Closing);
  Include(Self.Activated, Self.FormActivate);
  Include(Self.Deactivate, Self.FormDeactivate);
  Self.ResumeLayout(False);
end;

procedure TfmBase.CreateListView(aOwner: System.Windows.Forms.Control; var aList: TGKListView);
begin
  aList := TGKListView.Create(nil);
  aList.HideSelection := False;
  aList.LabelEdit := False;
  aList.FullRowSelect := True;
  aList.View := System.Windows.Forms.View.Details;
  aList.Dock := System.Windows.Forms.DockStyle.Fill;
  //aList.ShowSortSign := True;

  aOwner.Controls.Add(aList);
end;

procedure TfmBase.CreateRecordsView(aParent: System.Windows.Forms.Control;
  aRecordType: TGEDCOMRecord.TGEDCOMRecordType; var aList: TRecordsView);
begin
  aList := TRecordsView.Create(Self);
  aList.HideSelection := False;
  aList.LabelEdit := False;
  aList.FullRowSelect := True;
  aList.View := System.Windows.Forms.View.Details;
  aList.Tree := FTree;
  aList.RecordType := aRecordType;
  aList.Dock := System.Windows.Forms.DockStyle.Fill;
  //aList.ShowSortSign := True;

  aParent.Controls.Add(aList);
  aParent.Controls.SetChildIndex(aList, 0);
end;

procedure TfmBase.CreatePage(aPageText: string; aRecType: TGEDCOMRecord.TGEDCOMRecordType;
  var aList: TRecordsView; var aSummary: TGKHyperView);
var
  sheet: System.Windows.Forms.TabPage;
  spl: System.Windows.Forms.Splitter;
begin
  PageRecords.SuspendLayout;

  sheet := System.Windows.Forms.TabPage.Create();
  sheet.Text := aPageText;

  PageRecords.Controls.Add(sheet);
  PageRecords.ResumeLayout(False);

  ///

  aSummary := TGKHyperView.Create();
  spl := System.Windows.Forms.Splitter.Create();

  //sheet.SuspendLayout;

  aSummary.BorderWidth := 4;
  //aSummary.Font.Name := 'Tahoma';
  aSummary.Dock := System.Windows.Forms.DockStyle.Right;
  aSummary.Size := System.Drawing.Size.Create(300, 290);
  aSummary.OnLink := mPersonSummaryLink;

  spl.Dock := System.Windows.Forms.DockStyle.Right;
  spl.Size := System.Drawing.Size.Create(4, 290);
  spl.MinExtra := 100;
  spl.MinSize := 100;

  sheet.Controls.Add(aSummary);
  sheet.Controls.Add(spl);

  CreateRecordsView(sheet, aRecType, aList);
  aList.IsMainList := IsMainList(aRecType, aList);
  Include(aList.DoubleClick, RecordEdit);
  Include(aList.SelectedIndexChanged, List_SelectedIndexChanged);

  aList.UpdateTitles();

  sheet.Controls.SetChildIndex(spl, 1);
  sheet.Controls.SetChildIndex(aSummary, 2);

  //sheet.ResumeLayout(False);
end;

function TfmBase.CheckModified(): Boolean;
begin
  Result := True;

  if Modified then begin
    case MessageBox.Show(LSList[LSID_FileSaveQuery], TGenEngine.AppName, MessageBoxButtons.YesNoCancel, MessageBoxIcon.Warning) of
      System.Windows.Forms.DialogResult.Yes: fmGEDKeeper.miFileSaveClick(nil, nil);
      System.Windows.Forms.DialogResult.No: {dummy};
      System.Windows.Forms.DialogResult.Cancel: Result := False;
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
    PageRecords.SelectedIndex := aTab;
    PageRecords_SelectedIndexChanged(nil, nil);
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

function TfmBase.GetFamilyBySpouse(aNewParent: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;
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
        msg := System.&String.Format(LSList[LSID_ParentsQuery], [TGenEngine.GetFamilyStr(fam)]);
        if (TGKUtils.ShowQuestion(msg) = System.Windows.Forms.DialogResult.Yes) then begin
          Result := fam;
          Exit;
        end;
      end;
    end;
  end;
end;

function TfmBase.GetChildFamily(iChild: TGEDCOMIndividualRecord; aCanCreate: Boolean;
  aNewParent: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;
var
  fam: TGEDCOMFamilyRecord;
begin
  Result := nil;
  if (iChild = nil) then Exit;

  if (iChild.ChildToFamilyLinksCount <> 0)
  then Result := iChild.ChildToFamilyLinks[0].Family
  else begin
    if (aCanCreate) then begin
      fam := GetFamilyBySpouse(aNewParent);

      if (fam = nil)
      then fam := TGenEngine.CreateFamilyEx(FTree);

      FEngine.AddFamilyChild(fam, iChild);

      Result := fam;
    end;
  end;
end;

function TfmBase.DeleteFamilyRecord(aFamily: TGEDCOMFamilyRecord; aConfirm: Boolean): Boolean;
begin
  Result := False;
  if (aFamily = nil) then Exit;

  if (aConfirm) and (TGKUtils.ShowQuestion(System.&String.Format(LSList[LSID_FamilyDeleteQuery], [TGenEngine.GetFamilyStr(aFamily)])) = System.Windows.Forms.DialogResult.No)
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

  if (aConfirm) and (TGKUtils.ShowQuestion(System.&String.Format(LSList[LSID_PersonDeleteQuery], [TGenEngine.GetNameStr(iRec)])) = System.Windows.Forms.DialogResult.No)
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
  Result := False;
  if (nRec = nil) then Exit;

  if (aConfirm) and (TGKUtils.ShowQuestion(LSList[LSID_NoteDeleteQuery]) = System.Windows.Forms.DialogResult.No)
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

  if (aConfirm) and (TGKUtils.ShowQuestion(System.&String.Format(LSList[LSID_SourceDeleteQuery], [srcRec.FiledByEntry])) = System.Windows.Forms.DialogResult.No)
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

  if (aConfirm) and (TGKUtils.ShowQuestion(System.&String.Format(LSList[LSID_MediaDeleteQuery], [mRec.StringValue])) = System.Windows.Forms.DialogResult.No)
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

  if (aConfirm) and (TGKUtils.ShowQuestion(System.&String.Format(LSList[LSID_RepositoryDeleteQuery], [repRec.RepositoryName])) = System.Windows.Forms.DialogResult.No)
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

  if (aConfirm) and (TGKUtils.ShowQuestion(System.&String.Format(LSList[LSID_GroupDeleteQuery], [groupRec.Name])) = System.Windows.Forms.DialogResult.No)
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

  if (aConfirm) and (TGKUtils.ShowQuestion(System.&String.Format(LSList[LSID_ResearchDeleteQuery], [resRec.Name])) = System.Windows.Forms.DialogResult.No)
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

  if (aConfirm) and (TGKUtils.ShowQuestion(System.&String.Format(LSList[LSID_TaskDeleteQuery], [TGenEngine.GetTaskGoalStr(FTree, TaskRec)])) = System.Windows.Forms.DialogResult.No)
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

  if (aConfirm) and (TGKUtils.ShowQuestion(System.&String.Format(LSList[LSID_CommunicationDeleteQuery], [ComRec.Name])) = System.Windows.Forms.DialogResult.No)
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

  if (aConfirm) and (TGKUtils.ShowQuestion(System.&String.Format(LSList[LSID_LocationDeleteQuery], [LocRec.Name])) = System.Windows.Forms.DialogResult.No)
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

procedure TfmBase.List_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
var
  data: TGEDCOMRecord;
begin
  data := TRecordsView(sender).GetSelectedRecord();
  if (data <> nil) then NavAdd(data);
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
    if (anAddress.Address.Text.Trim() <> '')
    then ts := ts + anAddress.Address.Text.Trim();

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
    aSummary.Add('    '+LSList[LSID_RPSources]+' (' + aDetail.SourceCitationsCount.ToString() + '):');
    for idx := 0 to aDetail.SourceCitationsCount - 1 do begin
      cit := aDetail.SourceCitations[idx];
      sourceRec := TGEDCOMSourceRecord(cit.Value);
      if (sourceRec <> nil) then begin
        nm := '"'+sourceRec.FiledByEntry+'"';
        if (cit.Page <> '') then nm := nm + ', ' + cit.Page;

        aSummary.Add('      ' + TGenEngine.HyperLink(sourceRec.XRef, nm));
      end;
    end;
  end;
end;

procedure TfmBase.ShowDetailCause(aDetail: TGEDCOMEventDetail; aSummary: TStrings);
var
  cause: string;
begin
  cause := TGenEngine.GetEventCause(aDetail);
  if (aSummary <> nil) and (cause <> '')
  then aSummary.Add('    ' + cause);
end;

procedure TfmBase.RecListIndividualEventsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TGKListView; aSummary: TStrings);
var
  idx, ev: Integer;
  event: TGEDCOMCustomEvent;
  st: string;
  item: TExtListItem;
begin
  try
    if (aList <> nil)
    then aList.Items.Clear();

    if (aRecord.IndividualEventsCount <> 0) then begin
      if (aSummary <> nil) then begin
        aSummary.Add('');
        aSummary.Add(LSList[LSID_Events]+':');
      end;

      for idx := 0 to aRecord.IndividualEventsCount - 1 do begin
        event := aRecord.IndividualEvents[idx];

        ev := TGenEngine.GetPersonEventIndex(event.Name);
        if (ev = 0) then st := event.Detail.Classification
        else
        if (ev > 0) then st := LSList[TGenEngine.PersonEvents[ev].Name]
        else st := event.Name;

        if (aSummary <> nil) then begin
          aSummary.Add(st + ': ' + TGenEngine.GetEventDesc(event.Detail));
          if (event.StringValue <> '') then aSummary.Add('    ' + event.StringValue);
          ShowDetailCause(event.Detail, aSummary);
          ShowAddress(event.Detail.Address, aSummary);
          ShowDetailInfo(event.Detail, aSummary);
        end;

        if (aList <> nil) then begin
          item := aList.AddItem(System.Convert.ToString(idx + 1), event);
          item.SubItems.Add(st);
          item.SubItems.Add(TGenEngine.GEDCOMCustomDateToStr(event.Detail.Date.Value, fmGEDKeeper.Options.DefDateFormat));

          st := event.Detail.Place.StringValue;
          if (event.StringValue <> '')
          then st := st + ' [' + event.StringValue + ']';
          item.SubItems.Add(st);

          item.SubItems.Add(TGenEngine.GetEventCause(event.Detail));
        end;
      end;

      if (aList <> nil) then begin
        aList.ResizeColumn(2);
      end;
    end;
  except
    on E: Exception do TGKUtils.LogWrite('GKBase.RecListIndividualEventsRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.RecListFamilyEventsRefresh(aRecord: TGEDCOMFamilyRecord; aList: TGKListView; aSummary: TStrings);
var
  idx, ev: Integer;
  event: TGEDCOMFamilyEvent;
  st: string;
  item: TExtListItem;
begin
  try
    if (aList <> nil)
    then aList.Items.Clear();

    if (aRecord.FamilyEventCount <> 0) then begin
      if (aSummary <> nil) then begin
        aSummary.Add('');
        aSummary.Add(LSList[LSID_Events]+':');
      end;

      for idx := 0 to aRecord.FamilyEventCount - 1 do begin
        event := aRecord.FamilyEvents[idx];

        ev := TGenEngine.GetFamilyEventIndex(event.Name);
        if (ev = 0) then st := event.Detail.Classification
        else
        if (ev > 0) then st := LSList[TGenEngine.FamilyEvents[ev].Name]
        else st := event.Name;

        if (aSummary <> nil) then begin
          aSummary.Add(st + ': ' + TGenEngine.GetEventDesc(event.Detail));
          ShowDetailCause(event.Detail, aSummary);
        end;

        ShowDetailInfo(event.Detail, aSummary);

        if (aList <> nil) then begin
          item := aList.AddItem(System.Convert.ToString(idx + 1), event);
          item.SubItems.Add(st);
          item.SubItems.Add(TGenEngine.GEDCOMCustomDateToStr(event.Detail.Date.Value, fmGEDKeeper.Options.DefDateFormat));
          item.SubItems.Add(event.Detail.Place.StringValue);
          item.SubItems.Add(TGenEngine.GetEventCause(event.Detail));
        end;
      end;
    end;
  except
    on E: Exception do TGKUtils.LogWrite('GKBase.RecListFamilyEventsRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.RecListNotesRefresh(aRecord: TGEDCOMRecord; aList: TGKListView; aSummary: TStrings);
var
  idx, k: Integer;
  note: TGEDCOMNotes;
  st: string;
begin
  try
    if (aList <> nil)
    then aList.Items.Clear();

    if (aRecord.NotesCount <> 0) then begin
      if (aSummary <> nil) then begin
        aSummary.Add('');
        aSummary.Add(LSList[LSID_RPNotes]+' (' + aRecord.NotesCount.ToString() + '):');
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
        then aList.AddItem(note.Notes.Text.Trim(), note);
      end;
    end;
  except
    on E: Exception do TGKUtils.LogWrite('GKBase.RecListNotesRefresh(): ' + E.Message);
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
    then aList.Items.Clear();

    if (aRecord.MultimediaLinksCount <> 0) then begin
      if (aList = nil) and (aSummary <> nil) 
      then begin
        aSummary.Add('');
        aSummary.Add(LSList[LSID_RPMultimedia]+' (' + aRecord.MultimediaLinksCount.ToString() + '):');
      end;

      for idx := 0 to aRecord.MultimediaLinksCount - 1 do begin
        mmLink := aRecord.MultimediaLinks[idx];
        mmRec := TGEDCOMMultimediaRecord(mmLink.Value);

        if (mmRec <> nil) and (mmRec.FileReferencesCount <> 0) then begin
          st := mmRec.FileReferences[0].Title;

          if (aList = nil) and (aSummary <> nil)
          then aSummary.Add('  ' + TGenEngine.HyperLink(mmRec.XRef, st) + ' (' + TGenEngine.HyperLink(TGenEngine.MLinkPrefix+mmRec.XRef, 'просмотр') + ')');

          if (aList <> nil)
          then aList.AddItem(st, mmLink);
        end;
      end;
    end;
  except
    on E: Exception do TGKUtils.LogWrite('GKBase.RecListMediaRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.RecListSourcesRefresh(aRecord: TGEDCOMRecord; aList: TGKListView; aSummary: TStrings);
var
  idx: Integer;
  cit: TGEDCOMSourceCitation;
  sourceRec: TGEDCOMSourceRecord;
  item: TExtListItem;
  nm: string;
begin
  try
    if (aList <> nil)
    then aList.Items.Clear();

    if (aRecord.SourceCitationsCount <> 0) then begin
      if (aList = nil) and (aSummary <> nil)
      then begin
        aSummary.Add('');
        aSummary.Add(LSList[LSID_RPSources]+' (' + aRecord.SourceCitationsCount.ToString() + '):');
      end;

      for idx := 0 to aRecord.SourceCitationsCount - 1 do begin
        cit := aRecord.SourceCitations[idx];
        sourceRec := TGEDCOMSourceRecord(cit.Value);
        if (sourceRec <> nil) then begin
          nm := '"'+sourceRec.FiledByEntry+'"';
          if (cit.Page <> '') then nm := nm + ', ' + cit.Page;

          if (aList = nil) and (aSummary <> nil)
          then aSummary.Add('  ' + TGenEngine.HyperLink(sourceRec.XRef, nm));

          if (aList <> nil) then begin
            item := aList.AddItem(sourceRec.Originator.Text.Trim(), cit);
            item.SubItems.Add(nm);
          end;
        end;
      end;

      if (aList <> nil) then aList.ResizeColumn(1);
    end;
  except
    on E: Exception do TGKUtils.LogWrite('GKBase.RecListSourcesRefresh(): ' + E.Message);
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
  item: TExtListItem;
  nm: string;
begin
  try
    if (aList <> nil)
    then aList.Items.Clear();

    if (aRecord.AssociationsCount <> 0) then begin
      if (aList = nil) and (aSummary <> nil)
      then begin
        aSummary.Add('');
        aSummary.Add(LSList[LSID_Associations]+':');
      end;

      for idx := 0 to aRecord.AssociationsCount - 1 do begin
        ast := aRecord.Associations[idx];
        nm := TGenEngine.GetNameStr(ast.Individual);

        if (aList = nil) and (aSummary <> nil)
        then aSummary.Add('    ' + ast.Relation + ' ' + TGenEngine.HyperLink(ast.Individual.XRef, nm));

        if (aList <> nil) then begin
          item := aList.AddItem(ast.Relation, ast);
          item.SubItems.Add(nm);
        end;
      end;
    end;                               
  except
    on E: Exception do TGKUtils.LogWrite('GKBase.RecListAssociationsRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.RecListGroupsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TGKListView; aSummary: TStrings);
var
  idx: Integer;
  grp: TGEDCOMGroupRecord;
  ptr: TGEDCOMPointer;
  item: TExtListItem;
begin
  try
    if (aList <> nil)
    then aList.Items.Clear();

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
          then aSummary.Add('    '+TGenEngine.HyperLink(grp.XRef, grp.Name));

          if (aList <> nil) then begin
            item := aList.AddItem(grp.Name, grp);
          end;
        end;
      end;
    end;
  except
    on E: Exception do TGKUtils.LogWrite('GKBase.RecListGroupsRefresh(): ' + E.Message);
  end;
end;

function TfmBase.IsMainList(aRecType: TGEDCOMRecord.TGEDCOMRecordType; aList: TGKListView): Boolean;
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

procedure TfmBase.IntUpdate(aRecView: TRecordsView; ASCol: Integer;
  aTitles: Boolean = False);
var
  rec: TGEDCOMRecord;
begin
  rec := aRecView.GetSelectedRecord();
  aRecView.UpdateContents(FShieldState, aTitles, FXFilter, ASCol);
  if (rec <> nil) then aRecView.SelectItemByRec(rec);

  FCounts[aRecView.RecordType].Total := aRecView.TotalCount;
  FCounts[aRecView.RecordType].Filtered := aRecView.FilteredCount;
end;

procedure TfmBase.ListsRefresh(aTitles: Boolean = False);
begin
  IntUpdate(ListPersons, 2, aTitles);
  IntUpdate(ListFamilies, 1, aTitles);
  IntUpdate(ListNotes, -1, aTitles);
  IntUpdate(ListMultimedia, 1, aTitles);
  IntUpdate(ListSources, 1, aTitles);
  IntUpdate(ListRepositories, 1, aTitles);
  IntUpdate(ListGroups, 1, aTitles);
  IntUpdate(ListResearches, 1, aTitles);
  IntUpdate(ListTasks, 1, aTitles);
  IntUpdate(ListCommunications, 1, aTitles);
  IntUpdate(ListLocations, 1, aTitles);

  PageRecords_SelectedIndexChanged(nil, nil);

  if Assigned(fmGEDKeeper.fmTimeLine) then fmGEDKeeper.fmTimeLine.CheckTimeWin(Self);
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
        aSummary.Add('~ub+1~' + TGenEngine.GetNameStr(iRec, True, True) + '~bu-1~');
        aSummary.Add(LSList[LSID_Sex] + ': ' + TGenEngine.SexStr(iRec.Sex));

        try
          if (iRec.ChildToFamilyLinksCount <> 0) then begin
            aSummary.Add('');
            aSummary.Add(LSList[LSID_Parents]+':');

            family := iRec.ChildToFamilyLinks[0].Family;

            rel_person := TGEDCOMIndividualRecord(family.Husband.Value);
            if (rel_person <> nil)
            then st := TGenEngine.HyperLink(rel_person.XRef, TGenEngine.GetNameStr(rel_person))
            else st := LSList[LSID_UnkMale];
            aSummary.Add('  '+LSList[LSID_Father]+': ' + st + TGenEngine.GetLifeStr(rel_person));

            rel_person := TGEDCOMIndividualRecord(family.Wife.Value);
            if (rel_person <> nil)
            then st := TGenEngine.HyperLink(rel_person.XRef, TGenEngine.GetNameStr(rel_person))
            else st := LSList[LSID_UnkFemale];
            aSummary.Add('  '+LSList[LSID_Mother]+': ' + st + TGenEngine.GetLifeStr(rel_person));
          end;
        except
          on E: Exception do TGKUtils.LogWrite('GKBase.ShowPersonInfo().Parents(): ' + E.Message);
        end;

        try
          for idx := 0 to iRec.SpouseToFamilyLinksCount - 1 do begin
            family := iRec.SpouseToFamilyLinks[idx].Family;
            if (family = nil) then begin
              TGKUtils.LogWrite('File ('+FileName+'), iRec ('+iRec.XRef+'): empty family entry');
              Continue;
            end;

            if not(TGenEngine.IsRecordAccess(family.Restriction, FShieldState))
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

            marr := TGenEngine.GetMarriageDate(family, dfDD_MM_YYYY);
            if (marr <> '')
            then marr := LSList[LSID_LMarriage]+' ' + marr
            else marr := LSList[LSID_LFamily];

            rel_person := TGEDCOMIndividualRecord(sp.Value);

            aSummary.Add('');
            if (rel_person <> nil)
            then st := st + TGenEngine.HyperLink(rel_person.XRef, TGenEngine.GetNameStr(rel_person)) + ' (' + TGenEngine.HyperLink(family.XRef, marr) + ')'
            else st := st + unk + ' (' + TGenEngine.HyperLink(family.XRef, marr) + ')';
            aSummary.Add({#13#10 + }st);

            if (family.ChildrenCount <> 0) then begin
              aSummary.Add('');
              aSummary.Add(LSList[LSID_Childs]+':');
            end;

            for k := 0 to family.ChildrenCount - 1 do begin
              rel_person := TGEDCOMIndividualRecord(family.Children[k].Value);
              aSummary.Add({#09}'    ' + TGenEngine.HyperLink(rel_person.XRef, TGenEngine.GetNameStr(rel_person)) + TGenEngine.GetLifeStr(rel_person));
            end;
          end;
        except
          on E: Exception do TGKUtils.LogWrite('GKBase.ShowPersonInfo().Families(): ' + E.Message);
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
          st := TGenEngine.GetNameStr(iRec, True, False);
          for k := 0 to FTree.RecordsCount - 1 do begin
            rec := FTree.Records[k];

            if (rec is TGEDCOMIndividualRecord) and (rec <> iRec) then begin
              rel_person := (rec as TGEDCOMIndividualRecord);
              unk := TGenEngine.GetNameStr(rel_person, True, False);

              if (st = unk)
              then namesakes.AddObject(unk + TGenEngine.GetLifeStr(rel_person), rel_person);
            end;
          end;

          if (namesakes.Count > 0) then begin
            aSummary.Add('');
            aSummary.Add(LSList[LSID_Namesakes]+':');
            for k := 0 to namesakes.Count - 1 do begin
              rel_person := (namesakes.Objects[k] as TGEDCOMIndividualRecord);
              aSummary.Add('    '+TGenEngine.HyperLink(rel_person.XRef, namesakes[k]));
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
    on E: Exception do TGKUtils.LogWrite('GKBase.ShowPersonInfo(): ' + E.Message);
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
        then st := TGenEngine.HyperLink(irec.XRef, TGenEngine.GetNameStr(irec))
        else st := LSList[LSID_UnkMale];
        aSummary.Add(LSList[LSID_Husband]+': ' + st + TGenEngine.GetLifeStr(irec));

        irec := TGEDCOMIndividualRecord(aFamily.Wife.Value);
        if (irec <> nil)
        then st := TGenEngine.HyperLink(irec.XRef, TGenEngine.GetNameStr(irec))
        else st := LSList[LSID_UnkFemale];
        aSummary.Add(LSList[LSID_Wife]+': ' + st + TGenEngine.GetLifeStr(irec));

        aSummary.Add('');

        if (aFamily.ChildrenCount <> 0)
        then aSummary.Add(LSList[LSID_Childs]+':');

        for k := 0 to aFamily.ChildrenCount - 1 do begin
          irec := TGEDCOMIndividualRecord(aFamily.Children[k].Value);
          aSummary.Add('    ' + TGenEngine.HyperLink(irec.XRef, TGenEngine.GetNameStr(irec)) + TGenEngine.GetLifeStr(irec));
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
    on E: Exception do TGKUtils.LogWrite('GKBase.ShowFamilyInfo(): ' + E.Message);
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
        aSummary.Add('[ ' + TGenEngine.HyperLink(TGenEngine.MLinkPrefix+aMultimediaRec.XRef, LSList[LSID_View]) + ' ]');
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
    on E: Exception do TGKUtils.LogWrite('GKBase.ShowMultimediaInfo(): ' + E.Message);
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
    on E: Exception do TGKUtils.LogWrite('GKBase.ShowNoteInfo(): ' + E.Message);
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
        aSummary.Add(LSList[LSID_Author]+': ' + aSourceRec.Originator.Text.Trim());
        aSummary.Add(LSList[LSID_Title]+': "' + aSourceRec.Title.Text.Trim() + '"');
        aSummary.Add(LSList[LSID_Publication]+': "' + aSourceRec.Publication.Text.Trim() + '"');

        if (aSourceRec.RepositoryCitationsCount > 0) then begin
          aSummary.Add('');
          aSummary.Add(LSList[LSID_RPRepositories]+':');

          for k := 0 to aSourceRec.RepositoryCitationsCount - 1 do begin
            rep := TGEDCOMRepositoryRecord(aSourceRec.RepositoryCitations[k].Value);
            aSummary.Add('    '+TGenEngine.HyperLink(rep.XRef, rep.RepositoryName));
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
    on E: Exception do TGKUtils.LogWrite('GKBase.ShowSourceInfo(): ' + E.Message);
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
        aSummary.Add('~ub+1~' + aRepositoryRec.RepositoryName.Trim() + '~bu-1~');

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
              then aSummary.Add('    '+TGenEngine.GenRecordLink(FTree, srcRec, False));
          end;
        end;

        RecListNotesRefresh(aRepositoryRec, nil, aSummary);
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do TGKUtils.LogWrite('GKBase.ShowRepositoryInfo(): ' + E.Message);
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
        aSummary.Add(LSList[LSID_Members]+' (' + aGroup.MembersCount.ToString() + '):');

        for i := 0 to aGroup.MembersCount - 1 do begin
          member := TGEDCOMIndividualRecord(aGroup.Members[i].Value);
          mbrList.AddObject(TGenEngine.GetNameStr(member), member);
        end;

        mbrList.Sort;

        for i := 0 to mbrList.Count - 1 do begin
          member := TGEDCOMIndividualRecord(mbrList.Objects[i]);
          aSummary.Add('    '+TGenEngine.HyperLink(member.XRef, mbrList[i], i+1));
        end;

        RecListNotesRefresh(aGroup, nil, aSummary);
        RecListMediaRefresh(aGroup, nil, aSummary);
      end;
    finally
      aSummary.EndUpdate();
      mbrList.Free;
    end;
  except
    on E: Exception do TGKUtils.LogWrite('GKBase.ShowGroupInfo(): ' + E.Message);
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
        aSummary.Add(LSList[LSID_Title]+': "~ub+1~' + aResearchRec.Name.Trim() + '~bu-1~"');
        aSummary.Add('');
        aSummary.Add(LSList[LSID_Priority]+': ' + LSList[TGenEngine.PriorityNames[aResearchRec.Priority]]);
        aSummary.Add(LSList[LSID_Status]+': ' + LSList[TGenEngine.StatusNames[aResearchRec.Status]] + ' (' + aResearchRec.Percent.ToString() + '%)');
        aSummary.Add(LSList[LSID_StartDate]+': ' + TGenEngine.GEDCOMDateToStr(aResearchRec.StartDate));
        aSummary.Add(LSList[LSID_StopDate]+': ' + TGenEngine.GEDCOMDateToStr(aResearchRec.StopDate));

        if (aResearchRec.TasksCount > 0) then begin
          aSummary.Add('');
          aSummary.Add(LSList[LSID_RPTasks]+':');
          for i := 0 to aResearchRec.TasksCount - 1 do begin
            taskRec := TGEDCOMTaskRecord(aResearchRec.Tasks[i].Value);
            aSummary.Add('    '+TGenEngine.GenRecordLink(FTree, taskRec, False));
          end;
        end;

        if (aResearchRec.CommunicationsCount > 0) then begin
          aSummary.Add('');
          aSummary.Add(LSList[LSID_RPCommunications]+':');
          for i := 0 to aResearchRec.CommunicationsCount - 1 do begin
            corrRec := TGEDCOMCommunicationRecord(aResearchRec.Communications[i].Value);
            aSummary.Add('    '+TGenEngine.GenRecordLink(FTree, corrRec, False));
          end;
        end;

        if (aResearchRec.GroupsCount <> 0) then begin
          aSummary.Add('');
          aSummary.Add(LSList[LSID_RPGroups]+':');
          for i := 0 to aResearchRec.GroupsCount - 1 do begin
            grp := TGEDCOMGroupRecord(aResearchRec.Groups[i].Value);
            aSummary.Add('    '+TGenEngine.HyperLink(grp.XRef, grp.Name));
          end;
        end;

        RecListNotesRefresh(aResearchRec, nil, aSummary);
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do TGKUtils.LogWrite('GKBase.ShowResearchInfo(): ' + E.Message);
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
        aSummary.Add(LSList[LSID_Goal]+': ~ub+1~' + TGenEngine.GetTaskGoalStr(FTree, aTaskRec) + '~bu-1~');
        aSummary.Add('');
        aSummary.Add(LSList[LSID_Priority]+': ' + LSList[TGenEngine.PriorityNames[aTaskRec.Priority]]);
        aSummary.Add(LSList[LSID_StartDate]+': ' + TGenEngine.GEDCOMDateToStr(aTaskRec.StartDate));
        aSummary.Add(LSList[LSID_StopDate]+': ' + TGenEngine.GEDCOMDateToStr(aTaskRec.StopDate));

        RecListNotesRefresh(aTaskRec, nil, aSummary);
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do TGKUtils.LogWrite('GKBase.ShowTaskInfo(): ' + E.Message);
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
        aSummary.Add(LSList[LSID_Theme]+': "~ub+1~' + aCommunicationRec.Name.Trim() + '~bu-1~"');
        aSummary.Add('');
        aSummary.Add(LSList[LSID_Corresponder]+': ' + TGenEngine.GetCorresponderStr(FTree, aCommunicationRec, True));
        aSummary.Add(LSList[LSID_Type]+': ' + LSList[TGenEngine.CommunicationNames[aCommunicationRec.CommunicationType]]);
        aSummary.Add(LSList[LSID_Date]+': ' + TGenEngine.GEDCOMDateToStr(aCommunicationRec.Date));

        RecListNotesRefresh(aCommunicationRec, nil, aSummary);
        RecListMediaRefresh(aCommunicationRec, nil, aSummary);
      end;
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do TGKUtils.LogWrite('GKBase.ShowCommunicationInfo(): ' + E.Message);
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
        aSummary.Add('~ub+1~' + aLocationRec.Name.Trim() + '~bu-1~');
        aSummary.Add('');
        aSummary.Add(LSList[LSID_Latitude]+': ' + aLocationRec.Map.Lati);
        aSummary.Add(LSList[LSID_Longitude]+': ' + aLocationRec.Map.Long);

        TGenEngine.GetLocationLinks(FTree, aLocationRec, link_list);

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
    on E: Exception do TGKUtils.LogWrite('GKBase.ShowLocationInfo(): ' + E.Message);
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
    on E: Exception do TGKUtils.LogWrite('GKBase.ShowRecordInfo(): ' + E.Message);
  end;
end;

function TfmBase.GetSelectedPerson(): TGEDCOMIndividualRecord;
begin
  Result := TGEDCOMIndividualRecord(ListPersons.GetSelectedRecord());
end;

function TfmBase.ModifyRecEvent(aSender: System.Windows.Forms.Form; aRecord: TGEDCOMRecord;
  aEvent: TGEDCOMCustomEvent; anAction: TGenEngine.TRecAction): Boolean;
var
  fmEventEdit: TfmEventEdit;
  event: TGEDCOMCustomEvent; // TGEDCOMIndividualEvent + TGEDCOMFamilyEvent
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (TGKUtils.ShowQuestion(LSList[LSID_RemoveEventQuery]) = System.Windows.Forms.DialogResult.No)
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

    case fmGEDKeeper.ShowModalEx(fmEventEdit, aSender, True) of
      System.Windows.Forms.DialogResult.OK: begin
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

      System.Windows.Forms.DialogResult.Cancel: begin
        if (aEvent = nil)
        then event.Destroy;
      end;
    end;
  finally
    fmEventEdit.Free;
  end;
end;

function TfmBase.CreatePersonDialog(aTarget: TGEDCOMIndividualRecord;
  aTargetMode: TGenEngine.TTargetMode; aNeedSex: TGEDCOMObject.TGEDCOMSex): TGEDCOMIndividualRecord;
var
  dlg: TfmPersonNew;
begin
  Result := nil;

  dlg := TfmPersonNew.Create();
  try
    dlg.EditSex.SelectedIndex := Ord(aNeedSex);
    dlg.TargetMode := aTargetMode;
    dlg.Target := aTarget;
    if (fmGEDKeeper.ShowModalEx(dlg, Self) = System.Windows.Forms.DialogResult.OK) then begin
      Result := TGenEngine.CreatePersonEx(FTree, dlg.edName.Text, dlg.edPatronymic.Text,
        dlg.edFamily.Text, TGEDCOMObject.TGEDCOMSex(dlg.EditSex.SelectedIndex), True);
      ChangeRecord(Result);
    end;
  finally
    dlg.Free;
  end;
end;

function TfmBase.ModifyPerson(var aIndivRec: TGEDCOMIndividualRecord): Boolean;
var
  dlg: TfmPersonEdit;
begin
  Result := False;
  if (aIndivRec = nil) then Exit;

  dlg := TfmPersonEdit.Create(Self);
  try
    dlg.Person := aIndivRec;
    Result := (fmGEDKeeper.ShowModalEx(dlg, Self) = System.Windows.Forms.DialogResult.OK);
  finally
    dlg.Free;
  end;
end;

function TfmBase.ModifyFamily(var aFamilyRec: TGEDCOMFamilyRecord;
  aTarget: TGenEngine.TFamilyTarget = ftNone; aPerson: TGEDCOMIndividualRecord = nil): Boolean;
var
  fmFamEdit: TfmFamilyEdit;
  exists: Boolean;
begin
  Result := False;

  if (aTarget = ftSpouse) and (aPerson <> nil)
  and not(aPerson.Sex in [svMale, svFemale]) then begin
    TGKUtils.ShowError(LSList[LSID_IsNotDefinedSex]);
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

    Result := (fmGEDKeeper.ShowModalEx(fmFamEdit, Self) = System.Windows.Forms.DialogResult.OK);

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
    fmFamEdit.Free;
  end;
end;

function TfmBase.ModifyNote(var aNoteRec: TGEDCOMNoteRecord): Boolean;
var
  fmNoteEdit: TfmNoteEdit;
  exists: Boolean;
begin
  Result := False;

  fmNoteEdit := TfmNoteEdit.Create(Self);
  try
    exists := (aNoteRec <> nil);

    if not(exists) then begin
      aNoteRec := TGEDCOMNoteRecord.Create(FTree, FTree);
      aNoteRec.InitNew();
    end;

    fmNoteEdit.NoteRecord := aNoteRec;

    if (fmGEDKeeper.ShowModalEx(fmNoteEdit, Self) = System.Windows.Forms.DialogResult.OK) then begin
      if not(exists)
      then FTree.AddRecord(aNoteRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aNoteRec);
    end;
  finally
    fmNoteEdit.Free;
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

    if (fmGEDKeeper.ShowModalEx(fmMediaEdit, Self) = System.Windows.Forms.DialogResult.OK) then begin
      if not(exists)
      then FTree.AddRecord(aMediaRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aMediaRec);
    end;
  finally
    fmMediaEdit.Free;
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

    if (fmGEDKeeper.ShowModalEx(fmSrcEdit, Self) = System.Windows.Forms.DialogResult.OK) then begin
      if not(exists)
      then FTree.AddRecord(aSourceRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aSourceRec);
    end;
  finally
    fmSrcEdit.Free;
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

    if (fmGEDKeeper.ShowModalEx(fmRepEdit, Self) = System.Windows.Forms.DialogResult.OK) then begin
      if not(exists)
      then FTree.AddRecord(aRepRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aRepRec);
    end;
  finally
    fmRepEdit.Free;
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

    if (fmGEDKeeper.ShowModalEx(fmGrpEdit, Self) = System.Windows.Forms.DialogResult.OK) then begin
      if not(exists)
      then FTree.AddRecord(aGroupRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aGroupRec);
    end;
  finally
    fmGrpEdit.Free;
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

    if (fmGEDKeeper.ShowModalEx(fmResEdit, Self) = System.Windows.Forms.DialogResult.OK) then begin
      if not(exists)
      then FTree.AddRecord(aResearchRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aResearchRec);
    end;
  finally
    fmResEdit.Free;
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

    if (fmGEDKeeper.ShowModalEx(fmTaskEdit, Self) = System.Windows.Forms.DialogResult.OK) then begin
      if not(exists)
      then FTree.AddRecord(aTaskRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aTaskRec);
    end;
  finally
    fmTaskEdit.Free;
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

    if (fmGEDKeeper.ShowModalEx(fmCorrEdit, Self) = System.Windows.Forms.DialogResult.OK) then begin
      if not(exists)
      then FTree.AddRecord(aCommunicationRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aCommunicationRec);
    end;
  finally
    fmCorrEdit.Free;
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

    if (fmGEDKeeper.ShowModalEx(fmLocEdit, Self) = System.Windows.Forms.DialogResult.OK) then begin
      if not(exists)
      then FTree.AddRecord(aLocationRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aLocationRec);
    end;
  finally
    fmLocEdit.Free;
  end;
end;

function TfmBase.ModifyAddress(aSender: System.Windows.Forms.Form; anAddress: TGEDCOMAddress): Boolean;
var
  dlg: TfmAddressEdit;
begin
  dlg := TfmAddressEdit.Create();
  try
    dlg.Address := anAddress;
    Result := (fmGEDKeeper.ShowModalEx(dlg, aSender) = System.Windows.Forms.DialogResult.OK);
  finally
    dlg.Free;
  end;
end;

function TfmBase.ModifyRecNote(aSender: System.Windows.Forms.Form; aRecord: TGEDCOMRecord;
  aNote: TGEDCOMNotes; anAction: TGenEngine.TRecAction): Boolean;
var
  noteRec: TGEDCOMNoteRecord;
  note: TGEDCOMNotes;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (TGKUtils.ShowQuestion(LSList[LSID_DetachNoteQuery]) = System.Windows.Forms.DialogResult.No)
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

function TfmBase.ModifyRecMultimedia(aSender: System.Windows.Forms.Form; aRecord: TGEDCOMRecord;
  aLink: TGEDCOMMultimediaLink; anAction: TGenEngine.TRecAction): Boolean;
var
  mmRec: TGEDCOMMultimediaRecord;
  mmLink: TGEDCOMMultimediaLink;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (TGKUtils.ShowQuestion(LSList[LSID_DetachMultimediaQuery]) = System.Windows.Forms.DialogResult.No)
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

function TfmBase.ModifyRecSource(aSender: System.Windows.Forms.Form; aRecord: TGEDCOMRecord;
  aCit: TGEDCOMSourceCitation; anAction: TGenEngine.TRecAction): Boolean;
var
  cit: TGEDCOMSourceCitation;
  fmSrcCitEdit: TfmSourceCitEdit;
  res: System.Windows.Forms.DialogResult;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (TGKUtils.ShowQuestion(LSList[LSID_DetachSourceQuery]) = System.Windows.Forms.DialogResult.No)
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
    res := fmGEDKeeper.ShowModalEx(fmSrcCitEdit, aSender);

    case anAction of
      raAdd: begin
        if (res = System.Windows.Forms.DialogResult.OK)
        then aRecord.AddSourceCitation(cit)
        else cit.Destroy;
      end;
      raEdit: {dummy};
    end;

    Result := (res = System.Windows.Forms.DialogResult.OK);
  finally
    fmSrcCitEdit.Free;
  end;
end;

function TfmBase.ModifyRecUserRef(aSender: System.Windows.Forms.Form; aRecord: TGEDCOMRecord;
  aUserRef: TGEDCOMUserReference; anAction: TGenEngine.TRecAction): Boolean;
var
  dlg: TfmUserRefEdit;
  ref: TGEDCOMUserReference;
  res: System.Windows.Forms.DialogResult;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (TGKUtils.ShowQuestion(LSList[LSID_RemoveUserRefQuery]) = System.Windows.Forms.DialogResult.No)
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
    res := fmGEDKeeper.ShowModalEx(dlg, aSender);

    case anAction of
      raAdd: begin
        if (res = System.Windows.Forms.DialogResult.OK)
        then aRecord.AddUserReference(ref)
        else ref.Destroy;
      end;
      raEdit: {dummy};
    end;

    Result := (res = System.Windows.Forms.DialogResult.OK);
  finally
    dlg.Free;
  end;
end;

function TfmBase.ModifyRecAssociation(aSender: System.Windows.Forms.Form; aRecord: TGEDCOMIndividualRecord;
  aAssociation: TGEDCOMAssociation; anAction: TGenEngine.TRecAction): Boolean;
var
  fmAstEdit: TfmAssociationEdit;
  ast: TGEDCOMAssociation;
  res: System.Windows.Forms.DialogResult;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (TGKUtils.ShowQuestion(LSList[LSID_RemoveAssociationQuery]) = System.Windows.Forms.DialogResult.No)
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

    res := fmGEDKeeper.ShowModalEx(fmAstEdit, aSender);

    case anAction of
      raAdd: begin
        if (res = System.Windows.Forms.DialogResult.OK)
        then aRecord.AddAssociation(ast)
        else ast.Destroy;
      end;
      raEdit: {dummy};
    end;

    Result := (res = System.Windows.Forms.DialogResult.OK);
  finally
    fmAstEdit.Free;
  end;
end;

function TfmBase.ModifyTagNote(aTag: TGEDCOMTagWithLists;
  aNote: TGEDCOMNotes; anAction: TGenEngine.TRecAction): Boolean;
var
  noteRec: TGEDCOMNoteRecord;
  note: TGEDCOMNotes;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (TGKUtils.ShowQuestion(LSList[LSID_DetachNoteQuery]) = System.Windows.Forms.DialogResult.No)
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
  aLink: TGEDCOMMultimediaLink; anAction: TGenEngine.TRecAction): Boolean;
var
  mmRec: TGEDCOMMultimediaRecord;
  mmLink: TGEDCOMMultimediaLink;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (TGKUtils.ShowQuestion(LSList[LSID_DetachMultimediaQuery]) = System.Windows.Forms.DialogResult.No)
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
  aCit: TGEDCOMSourceCitation; anAction: TGenEngine.TRecAction): Boolean;
var
  cit: TGEDCOMSourceCitation;
  fmSrcCitEdit: TfmSourceCitEdit;
  res: System.Windows.Forms.DialogResult;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (TGKUtils.ShowQuestion(LSList[LSID_DetachSourceQuery]) = System.Windows.Forms.DialogResult.No)
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
    res := fmGEDKeeper.ShowModalEx(fmSrcCitEdit, fmGEDKeeper);

    case anAction of
      raAdd: begin
        if (res = System.Windows.Forms.DialogResult.OK)
        then aTag.AddSourceCitation(cit)
        else cit.Destroy;
      end;
      raEdit: {dummy};
    end;

    Result := (res = System.Windows.Forms.DialogResult.OK);
  finally
    fmSrcCitEdit.Free;
  end;
end;

function TfmBase.GetFileName(): string;
begin
  Result := FEngine.FileName;
end;

procedure TfmBase.SetFileName(const Value: string);
begin
  FEngine.FileName := Value;

  SetMainTitle();

  fmGEDKeeper.Options.LastDir := ExtractFilePath(FEngine.FileName);
end;

procedure TfmBase.PageRecords_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
begin
  fmGEDKeeper.UpdateControls();
end;

procedure TfmBase.OutLink(aSubject: TGEDCOMRecord; aToList: TStrings; aRec: TGEDCOMRecord; aTag: TGEDCOMTag; aExt: TGEDCOMPointer);
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
  then suffix := ', ' + TGenEngine.GetEventName(TGEDCOMCustomEvent(aTag)).ToLower()
  else suffix := '';

  aToList.Add('    ' + prefix + TGenEngine.GenRecordLink(FTree, aRec, True) + suffix);
end;

procedure TfmBase.PrepareEvent(aSubject: TGEDCOMRecord; aToList: TStrings;
  aRec: TGEDCOMRecord; event: TGEDCOMCustomEvent);
var
  i: Integer;
begin
  if (aSubject is TGEDCOMNoteRecord) then begin
    for i := 0 to event.Detail.NotesCount - 1 do
      if (event.Detail.Notes[i].Value = aSubject)
      then OutLink(aSubject, aToList, aRec, event, nil);
  end
  else
  if (aSubject is TGEDCOMMultimediaRecord) then begin
    for i := 0 to event.Detail.MultimediaLinksCount - 1 do
      if (event.Detail.MultimediaLinks[i].Value = aSubject)
      then OutLink(aSubject, aToList, aRec, event, nil);
  end
  else
  if (aSubject is TGEDCOMSourceRecord) then begin
    for i := 0 to event.Detail.SourceCitationsCount - 1 do
      if (event.Detail.SourceCitations[i].Value = aSubject)
      then OutLink(aSubject, aToList, aRec, event, event.Detail.SourceCitations[i]);
  end;
end;

procedure TfmBase.SearchSubjectLinks(aInRecord, aSubject: TGEDCOMRecord; aToList: TStrings);
// поиск ссылок на субъект поиска в других записях и тэгах
var
  k: Integer;
  i_rec: TGEDCOMIndividualRecord;
  f_rec: TGEDCOMFamilyRecord;
begin
  try
    if (aSubject is TGEDCOMNoteRecord) then begin
      for k := 0 to aInRecord.NotesCount - 1 do
        if (aInRecord.Notes[k].Value = aSubject)
        then OutLink(aSubject, aToList, aInRecord, nil, nil);
    end
    else
    if (aSubject is TGEDCOMMultimediaRecord) then begin
      for k := 0 to aInRecord.MultimediaLinksCount - 1 do
        if (aInRecord.MultimediaLinks[k].Value = aSubject)
        then OutLink(aSubject, aToList, aInRecord, nil, nil);
    end
    else
    if (aSubject is TGEDCOMSourceRecord) then begin
      for k := 0 to aInRecord.SourceCitationsCount - 1 do
        if (aInRecord.SourceCitations[k].Value = aSubject)
        then OutLink(aSubject, aToList, aInRecord, nil, aInRecord.SourceCitations[k]);
    end;

    if (aInRecord is TGEDCOMIndividualRecord) then begin
      i_rec := (aInRecord as TGEDCOMIndividualRecord);

      for k := 0 to i_rec.IndividualEventsCount - 1 do
        PrepareEvent(aSubject, aToList, i_rec, i_rec.IndividualEvents[k]);
    end
    else
    if (aInRecord is TGEDCOMFamilyRecord) then begin
      f_rec := (aInRecord as TGEDCOMFamilyRecord);

      for k := 0 to f_rec.FamilyEventCount - 1 do
        PrepareEvent(aSubject, aToList, f_rec, f_rec.FamilyEvents[k]);
    end;
  except
    on E: Exception do TGKUtils.LogWrite('GKBase.SearchSubjectLinks(): ' + E.Message);
  end;
end;

procedure TfmBase.mPersonSummaryLink(sender: System.Object; LinkName: String);
var
  xref: string;
  rec: TGEDCOMRecord;
begin
  if (Pos(TGenEngine.MLinkPrefix, LinkName) > 0) then begin
    xref := LinkName;
    StrDelete(xref, 1, Length(TGenEngine.MLinkPrefix));

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
    if not(fmMediaView.Extern)
    then fmGEDKeeper.ShowModalEx(fmMediaView, fmGEDKeeper);
  finally
    fmMediaView.Free;
  end;
end;

procedure TfmBase.SetModified(const Value: Boolean);
begin
  FModified := Value;
  SetMainTitle();
end;

procedure TfmBase.SetMainTitle();
begin
  Text := System.IO.Path.GetFileName(FileName);

  if FModified
  then Text := '* ' + Text;
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
    FTree.LoadFromFile(aFileName);
  except
    on E: Exception do begin
      TGKUtils.LogWrite('GKBase.FileLoad().TreeLoad(): ' + E.Message);
      TGKUtils.ShowError(LSList[LSID_LoadGedComFailed]);
    end;
  end;

  try
    TGenEngine.CheckGEDCOMFormat(FTree);
  except
    on E: Exception do begin
      TGKUtils.LogWrite('GKBase.FileLoad().CheckFormat(): ' + E.Message);
      TGKUtils.ShowError(LSList[LSID_CheckGedComFailed]);
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
  fs: System.IO.StreamWriter;
  subm, ext_name: string;
  is_advanced: Boolean;
begin
  subm := FTree.Header.GetTagStringValue('SUBM');
  is_advanced := IsAdvanced();
  ext_name := FTree.Header.GetTagStringValue(TGenEngine.ExtTag);

  FTree.Header.Clear;
  FTree.Header.Source := TGenEngine.AppName;
  FTree.Header.ReceivingSystemName := TGenEngine.AppName;
  FTree.Header.CharacterSet := fmGEDKeeper.Options.DefCharacterSet;
  FTree.Header.Language := 'Russian';
  FTree.Header.GEDCOMVersion := '5.5';
  FTree.Header.GEDCOMForm := 'LINEAGE-LINKED';
  FTree.Header.FileName := System.IO.Path.GetFileName(aFileName);
  FTree.Header.TransmissionDate.Date := DateTime.Now;

  if (subm <> '')
  then FTree.Header.SetTagStringValue('SUBM', subm);

  if (is_advanced) and (ext_name <> '') then begin
    FTree.Header.AddTag(TGenEngine.AdvTag);
    FTree.Header.AddTag(TGenEngine.ExtTag, ext_name);
  end;

  FTree.Pack();

  fs := System.IO.StreamWriter.Create(aFileName, False, Encoding.GetEncoding(1251));
  try
    FTree.SaveToStream(fs);
    FTree.Header.CharacterSet := csASCII;
  finally
    fs.Free;
  end;

  FileName := aFileName;
  fmGEDKeeper.AddMRU(aFileName);
  Modified := False;
end;

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

  case PageRecords.SelectedIndex of
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

procedure TfmBase.RecordEdit(sender: System.Object; e: System.EventArgs);
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

  case PageRecords.SelectedIndex of
    0: begin // персоны
      rec := ListPersons.GetSelectedRecord();
      ind := TGEDCOMIndividualRecord(rec);
      res := ModifyPerson(ind);
    end;

    1: begin // семьи
      rec := ListFamilies.GetSelectedRecord();
      fam := TGEDCOMFamilyRecord(rec);
      res := ModifyFamily(fam);
    end;

    2: begin // заметки
      rec := ListNotes.GetSelectedRecord();
      note := TGEDCOMNoteRecord(rec);
      res := ModifyNote(note);
    end;

    3: begin // мультимедиа
      rec := ListMultimedia.GetSelectedRecord();
      mm_rec := TGEDCOMMultimediaRecord(rec);
      res := ModifyMedia(mm_rec);
    end;

    4: begin // источники
      rec := ListSources.GetSelectedRecord();
      src := TGEDCOMSourceRecord(rec);
      res := ModifySource(src);
    end;

    5: begin // архивы
      rec := ListRepositories.GetSelectedRecord();
      rep := TGEDCOMRepositoryRecord(rec);
      res := ModifyRepository(rep);
    end;

    6: begin // группы
      rec := ListGroups.GetSelectedRecord();
      grp := TGEDCOMGroupRecord(rec);
      res := ModifyGroup(grp);
    end;

    7: begin // исследования
      rec := ListResearches.GetSelectedRecord();
      rsr := TGEDCOMResearchRecord(rec);
      res := ModifyResearch(rsr);
    end;

    8: begin // задачи
      rec := ListTasks.GetSelectedRecord();
      tsk := TGEDCOMTaskRecord(rec);
      res := ModifyTask(tsk);
    end;

    9: begin // переписка
      rec := ListCommunications.GetSelectedRecord();
      comm := TGEDCOMCommunicationRecord(rec);
      res := ModifyCommunication(comm);
    end;

    10: begin // места
      rec := ListLocations.GetSelectedRecord();
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

  case PageRecords.SelectedIndex of
    0: res := DeleteRecord(ListPersons.GetSelectedRecord(), True); // персоны
    1: res := DeleteRecord(ListFamilies.GetSelectedRecord(), True); // семьи
    2: res := DeleteRecord(ListNotes.GetSelectedRecord(), True); // заметки
    3: res := DeleteRecord(ListMultimedia.GetSelectedRecord(), True); // мультимедиа
    4: res := DeleteRecord(ListSources.GetSelectedRecord(), True); // источники
    5: res := DeleteRecord(ListRepositories.GetSelectedRecord(), True); // архивы
    6: res := DeleteRecord(ListGroups.GetSelectedRecord(), True); // группы
    7: res := DeleteRecord(ListResearches.GetSelectedRecord(), True); // исследования
    8: res := DeleteRecord(ListTasks.GetSelectedRecord(), True); // задачи
    9: res := DeleteRecord(ListCommunications.GetSelectedRecord(), True); // переписка
    10: res := DeleteRecord(ListLocations.GetSelectedRecord(), True); // места
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

function TfmBase.FileProperties(aMode: TFilePropertiesMode = fpmAuthor): System.Windows.Forms.DialogResult;
var
  fmFileProps: TfmFileProperties;
begin
  fmFileProps := TfmFileProperties.Create(Self);
  try
    fmFileProps.PageControl1.SelectedIndex := Ord(aMode);
    Result := fmGEDKeeper.ShowModalEx(fmFileProps, Self);
  finally
    fmFileProps.Free;
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
    fmGEDKeeper.ShowModalEx(fmFilter, Self);
  finally
    fmFilter.Free;
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

procedure TfmBase.TfmBase_Closing(sender: System.Object; e: System.ComponentModel.CancelEventArgs);
begin
  if not(CheckModified())
  then e.Cancel := True
  else e.Cancel := False;
end;

procedure TfmBase.TreeTools();
var
  fmTreeTools: TfmTreeTools;
begin
  fmTreeTools := TfmTreeTools.Create(Self);
  try
    fmGEDKeeper.ShowModalEx(fmTreeTools, Self);
  finally
    fmTreeTools.Free;
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
  fmChart.FileName := System.IO.Path.GetFileName(FileName);
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
  fmChart.FileName := System.IO.Path.GetFileName(FileName);
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
  fmChart.FileName := System.IO.Path.GetFileName(FileName);
  fmChart.GenChart();
end;

procedure TfmBase.GenPedigree_dAboville();
var
  p: TPedigree;
begin
  p := TPedigree.Create(FEngine, GetCurFileTempPath());
  try
    p.Ancestor := GetSelectedPerson();
    p.Options := fmGEDKeeper.Options;
    p.ShieldState := FShieldState;
    p.Kind := pk_dAboville;
    p.Generate();
  finally
    p.Free;
  end;
end;

procedure TfmBase.GenPedigree_Konovalov();
var
  p: TPedigree;
begin
  p := TPedigree.Create(FEngine, GetCurFileTempPath());
  try
    p.Ancestor := GetSelectedPerson();
    p.Options := fmGEDKeeper.Options;
    p.ShieldState := FShieldState;
    p.Kind := pk_Konovalov;
    p.Generate();
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
    fmGEDKeeper.ShowModalEx(fmPersonScan, Self);
  finally
    fmPersonScan.Free;
  end;
end;

procedure TfmBase.ShowMap();
var
  frm_maps: TfmMaps;
begin
  frm_maps := TfmMaps.Create(FTree, ListPersons.ContentList);
  frm_maps.MdiParent := fmGEDKeeper;
  frm_maps.Show;
end;

procedure TfmBase.ChangeRecord(aRecord: TGEDCOMRecord);
var
  rt: TGEDCOMRecord.TGEDCOMRecordType;
begin
  rt := aRecord.RecordType;
  FChangedRecords[rt].Add(aRecord);

  aRecord.ChangeDate.ChangeDateTime := DateTime.Now;
  Modified := True;
end;

procedure TfmBase.ChangesClear();
var
  rt: TGEDCOMRecord.TGEDCOMRecordType;
begin
  for rt := Low(TGEDCOMRecord.TGEDCOMRecordType) to High(TGEDCOMRecord.TGEDCOMRecordType) do
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
  Result := (FTree.Header.FindTag(TGenEngine.AdvTag) <> nil);
end;

procedure TfmBase.FormActivate(sender: System.Object; e: System.EventArgs);
begin
  fmGEDKeeper.UpdateControls();
  if Assigned(fmGEDKeeper.fmTimeLine) then fmGEDKeeper.fmTimeLine.CheckTimeWin(Self);
end;

procedure TfmBase.FormDeactivate(sender: System.Object; e: System.EventArgs);
begin
  fmGEDKeeper.UpdateControls(True);
  if Assigned(fmGEDKeeper.fmTimeLine) then fmGEDKeeper.fmTimeLine.CheckTimeWin(nil);
end;

function TfmBase.SelectPerson(aTarget: TGEDCOMIndividualRecord; aTargetMode: TGenEngine.TTargetMode;
  aNeedSex: TGEDCOMObject.TGEDCOMSex): TGEDCOMIndividualRecord;
var
  dlg: TfmRecordSelect;
begin
  dlg := TfmRecordSelect.Create(Self);
  try
    dlg.FTarget := aTarget;
    dlg.FNeedSex := aNeedSex;
    dlg.TargetMode := aTargetMode;
    dlg.Mode := rtIndividual;

    if (fmGEDKeeper.ShowModalEx(dlg, Self) = System.Windows.Forms.DialogResult.OK)
    then Result := TGEDCOMIndividualRecord(dlg.ResultRecord)
    else Result := nil;
  finally
    dlg.Free;
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

    if (fmGEDKeeper.ShowModalEx(dlg, Self) = System.Windows.Forms.DialogResult.OK)
    then Result := TGEDCOMFamilyRecord(dlg.ResultRecord)
    else Result := nil;
  finally
    dlg.Free;
  end;
end;

function TfmBase.SelectRecord(aMode: TGEDCOMRecord.TGEDCOMRecordType; anArgs: array of const): TGEDCOMRecord;
var
  dlg: TfmRecordSelect;
  args_cnt: Integer;
begin
  dlg := TfmRecordSelect.Create(Self);
  try
    dlg.Mode := aMode;

    args_cnt := High(anArgs) + 1;
    if (args_cnt > 0)
    then dlg.edFastFilter.Text := string(anArgs[0]);

    if (fmGEDKeeper.ShowModalEx(dlg, Self) = System.Windows.Forms.DialogResult.OK)
    then Result := dlg.ResultRecord
    else Result := nil;
  finally
    dlg.Free;
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
  NewSex: TGEDCOMObject.TGEDCOMSex);
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

procedure TfmBase.SetShieldState(const Value: TGenEngine.TShieldState);
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
          nm := TGenEngine.GetNameStr(i_rec);
          days := TGenEngine.GetDaysForBirth(i_rec);

          if (days <> '') and (Int32.Parse(days) < 3)
          then birth_days.Add(System.&String.Format(LSList[LSID_DaysRemained], [nm, days]));
        end;
      end;

      if (birth_days.Count > 0)
      then fmGEDKeeper.Options.ShowTips := TfmTipsDialog.ShowTipsEx(LSList[LSID_BirthDays], fmGEDKeeper.Options.ShowTips, birth_days);
    finally
      birth_days.Free;
    end;
  except
    on E: Exception do TGKUtils.LogWrite('GKBase.ShowTips(): ' + E.Message);
  end;
end;

procedure TfmBase.ShowOrganizer();
var
  dlg: TfmOrganizer;
begin
  dlg := TfmOrganizer.Create(Self);
  try
    fmGEDKeeper.ShowModalEx(dlg, Self);
  finally
    dlg.Free;
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
var
  dmn: TfmScriptDaemon;
begin
  dmn := TfmScriptDaemon.Create(Self);
  try
    fmGEDKeeper.ShowModalEx(dmn, Self);
  finally
    dmn.Free;
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

function TfmBase.ModifyName(var aName: TNamesTable.TName): Boolean;
var
  dlg: TfmNameEdit;
begin
  dlg := TfmNameEdit.Create();
  try
    dlg.IName := aName;
    Result := (fmGEDKeeper.ShowModalEx(dlg, Self) = System.Windows.Forms.DialogResult.OK);
  finally
    dlg.Free;
  end;
end;

function TfmBase.DefinePatronymic(aName: string; aSex: TGEDCOMObject.TGEDCOMSex; aConfirm: Boolean): string;
var
  n: TNamesTable.TName;
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
