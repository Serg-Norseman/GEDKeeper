unit GKBase;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, Menus, Masks, ActnList, 
  GKCommon, GedCom551, Htmemo, bsCtrls;

type
  TFilePropertiesMode = (fpmAuthor, fpmDiags, fpmAdvanced);

  TRecCount = record
    Total: Integer;
    Filtered: Integer;
  end;

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

    constructor Create();

    procedure Clear();
    procedure Backup();
    procedure Restore();
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PageRecordsChange(Sender: TObject);
    procedure mPersonSummaryLink(Sender: TObject; LinkName: String);
    procedure actTestExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    FChangedRecords: TList;
    FFileName: string;
    FFilter: TPersonsFilter;
    FModified: Boolean;
    FTree: TGEDCOMTree;

    FNavBusy: Boolean;
    FNavHistory: TList;
    FNavPos: Integer;

    // GUI
    ListPersons: TBSListView;
    mPersonSummary: THTMemo;
    ListFamilies: TBSListView;
    mFamilySummary: THTMemo;
    ListNotes: TBSListView;
    mNoteSummary: THTMemo;
    ListMultimedia: TBSListView;
    mMediaSummary: THTMemo;
    ListSources: TBSListView;
    mSourceSummary: THTMemo;
    ListRepositories: TBSListView;
    mRepositorySummary: THTMemo;
    ListGroups: TBSListView;
    mGroupSummary: THTMemo;
    //

    procedure CleanFamily(aFamily: TGEDCOMFamilyRecord);
    function  GetChangeDate(aRec: TGEDCOMRecord): string;
    procedure ListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure NavAdd(aRec: TGEDCOMRecord);
    procedure SetFileName(const Value: string);
    procedure SetMainTitle();
    procedure SetModified(const Value: Boolean);
    procedure ShowAddress(anAddress: TGEDCOMAddress; aSummary: TStrings);
  public
    FCounts: array [TGEDCOMRecordType] of TRecCount;

    procedure ApplyFilter();
    procedure CalcCounts();
    procedure ChangeRecord(aRecord: TGEDCOMRecord);
    procedure ChangesClear();
    function  CheckModified: Boolean;
    procedure Clear();
    procedure ExportToExcel();
    procedure ExportToWeb();
    procedure FileLoad(aFileName: string);
    procedure FileNew();
    function  FileProperties(aMode: TFilePropertiesMode = fpmAuthor): Integer;
    procedure FileSave(const aFileName: string);
    procedure GenPedigree_dAboville();
    procedure GenPedigree_Konovalov();
    function  GetChildFamily(iRec: TGEDCOMIndividualRecord; aCanCreate: Boolean;
      aNewParent: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;
    function  GetCurFileTempPath(): string;
    function  GetRecordLink(aRecord: TGEDCOMRecord; aLinkDesc: Boolean): string;
    function  GetSelectedPerson(): TGEDCOMIndividualRecord;
    function  GetSelectedRecord(aList: TBSListView): TGEDCOMRecord;
    procedure ListsRefresh(aTitles: Boolean = False);
    procedure NavClear();
    procedure NavNext();
    procedure NavPrev();
    procedure NavUpdate();
    procedure PersonScan();
    procedure RecordAdd();
    procedure RecordDelete();
    procedure RecordEdit(Sender: TObject);
    procedure SelectRecordByXRef(XRef: string);
    procedure SetFilter();
    procedure ShowMap();
    procedure ShowStats();
    procedure ShowTreeAncestors();
    procedure ShowTreeDescendants();
    procedure TreeTools();

    procedure CreatePage(aPage: TTabSheet; aRecType: TGEDCOMRecordType;
      var aList: TBSListView; var aSummary: THTMemo);

    function CheckPath(): Boolean;
    function GetArcFileName(): string;
    function GetStoreFolder(): string;
    function GetExtName(): string;
    function GetStoreType(aFileRef: string; var aFileName: string): TGKStoreType;
    function IsAdvanced(): Boolean;
    procedure MediaSave(aFileName: string; aStoreType: TGKStoreType; var aRefPath: string);
    procedure MediaLoad(aRefName: string; var aStream: TStream); overload;
    procedure MediaLoad(aRefName: string; var aFileName: string); overload;

    function SelectPerson(aTarget: TGEDCOMIndividualRecord; aTargetMode: TTargetMode;
      aNeedSex: TGEDCOMSex): TGEDCOMIndividualRecord;
    function SelectRecord(aMode: TSelectMode): TGEDCOMRecord;

    procedure ComListFamiliesRefresh(aList: TBSListView; aTitles: Boolean);
    procedure ComListGroupsRefresh(aList: TBSListView; aTitles: Boolean);
    procedure ComListPersonsRefresh(aList: TBSListView; aTitles: Boolean);
    procedure ComListMultimediaRefresh(aList: TBSListView; aTitles: Boolean);
    procedure ComListNotesRefresh(aList: TBSListView; aTitles: Boolean);
    procedure ComListSourcesRefresh(aList: TBSListView; aTitles: Boolean);
    procedure ComListRepositoriesRefresh(aList: TBSListView; aTitles: Boolean);

    function CreatePerson(aName, aPatronymic, aFamily: string;
      aSex: TGEDCOMSex; aBirthEvent: Boolean = False): TGEDCOMIndividualRecord;
    function CreatePersonDialog(aTarget: TGEDCOMIndividualRecord;
      aTargetMode: TTargetMode; aNeedSex: TGEDCOMSex): TGEDCOMIndividualRecord;

    procedure DeleteFamily(aFamily: TGEDCOMFamilyRecord);
    procedure DeleteIndividualRecord(iRec: TGEDCOMIndividualRecord);
    procedure DeleteNoteRecord(nRec: TGEDCOMNoteRecord);
    procedure DeleteSourceRecord(srcRec: TGEDCOMSourceRecord);

    function ModifyPerson(var aIndivRec: TGEDCOMIndividualRecord): Boolean;
    function ModifyFamily(var aFamilyRec: TGEDCOMFamilyRecord; aSpouse: TGEDCOMIndividualRecord = nil): Boolean;
    function ModifyNote(var aNoteRec: TGEDCOMNoteRecord): Boolean;
    function ModifyMedia(var aMediaRec: TGEDCOMMultimediaRecord): Boolean;
    function ModifySource(var aSourceRec: TGEDCOMSourceRecord): Boolean;
    function ModifyRepository(var aRepRec: TGEDCOMRepositoryRecord): Boolean;
    function ModifyGroup(var aGroupRec: TGEDCOMGroupRecord): Boolean;

    function ModifyRecAssociation(aRecord: TGEDCOMIndividualRecord; aIndex: Integer; anAction: TRecAction): Boolean;
    function ModifyRecEvent(aRecord: TGEDCOMRecord; aEvent: TGEDCOMCustomEvent; anAction: TRecAction): Boolean;
    function ModifyRecMultimedia(aRecord: TGEDCOMRecord; aIndex: Integer; anAction: TRecAction): Boolean;
    function ModifyRecNote(aRecord: TGEDCOMRecord; aIndex: Integer; anAction: TRecAction): Boolean;
    function ModifyRecSource(aRecord: TGEDCOMRecord; aIndex: Integer; anAction: TRecAction): Boolean;

    procedure RecListAssociationsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListFamilyEventsRefresh(aRecord: TGEDCOMFamilyRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListGroupsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListIndividualEventsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListMediaRefresh(aRecord: TGEDCOMRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListNotesRefresh(aRecord: TGEDCOMRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListSourcesRefresh(aRecord: TGEDCOMRecord; aList: TBSListView; aSummary: TStrings);

    procedure SetupRecAssociationsList(aList: TBSListView);
    procedure SetupRecChildsList(aList: TBSListView);
    procedure SetupRecEventsList(aList: TBSListView; PersonsMode: Boolean);
    procedure SetupRecGroupsList(aList: TBSListView);
    procedure SetupRecMediaList(aList: TBSListView);
    procedure SetupRecMembersList(aList: TBSListView);
    procedure SetupRecNotesList(aList: TBSListView);
    procedure SetupRecRepositoriesList(aList: TBSListView);
    procedure SetupRecSourcesList(aList: TBSListView);
    procedure SetupRecSpousesList(aList: TBSListView);

    procedure ShowDetailInfo(aDetail: TGEDCOMEventDetail; aSummary: TStrings);
    procedure ShowFamilyInfo(aFamily: TGEDCOMFamilyRecord; aSummary: TStrings);
    procedure ShowGroupInfo(aGroup: TGEDCOMGroupRecord; aSummary: TStrings);
    procedure ShowMultimediaInfo(aMultimediaRec: TGEDCOMMultimediaRecord; aSummary: TStrings);
    procedure ShowNoteInfo(aNoteRec: TGEDCOMNoteRecord; aSummary: TStrings);
    procedure ShowPersonInfo(iRec: TGEDCOMIndividualRecord; aSummary: TStrings);
    procedure ShowSourceInfo(aSourceRec: TGEDCOMSourceRecord; aSummary: TStrings);
    procedure ShowRepositoryInfo(aRepositoryRec: TGEDCOMRepositoryRecord; aSummary: TStrings);

    property FileName: string read FFileName write SetFileName;
    property Filter: TPersonsFilter read FFilter;
    property Modified: Boolean read FModified write SetModified;
    property Tree: TGEDCOMTree read FTree;
  end;

  TModifyEvent = procedure (Sender: TObject; Index: Integer; Action: TRecAction) of object;

  TListButtons = set of (lbAdd, lbEdit, lbDelete, lbJump, lbMoveUp, lbMoveDown);

  TSheetList = class(TCustomControl)
  private
    //FActionList: TActionList;
    FActionAdd: TAction;
    FActionDelete: TAction;
    FActionEdit: TAction;
    FActionJump: TAction;
    FActionMoveUp: TAction;
    FActionMoveDown: TAction;

    FBtnAdd: TToolButton;
    FBtnDelete: TToolButton;
    FBtnEdit: TToolButton;
    FBtnLinkJump: TToolButton;
    FBtnMoveUp: TToolButton;
    FBtnMoveDown: TToolButton;

    FList: TBSListView;
    FToolBar: TToolBar;

    FOnModify: TModifyEvent;
    FButtons: TListButtons;

    procedure ButtonClick(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    //procedure SetActionList(const Value: TActionList);
    procedure SheetShow(Sender: TObject);
    procedure SetButtons(const Value: TListButtons);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //property ActionList: TActionList read FActionList write SetActionList;
    property Buttons: TListButtons read FButtons write SetButtons;
    property List: TBSListView read FList;
    property ToolBar: TToolBar read FToolBar;

    procedure ItemAdd();
    procedure ItemEdit();
    procedure ItemDelete();
    procedure ItemJump();
    procedure ItemMoveUp();
    procedure ItemMoveDown();

    property OnModify: TModifyEvent read FOnModify write FOnModify;
  end;

procedure AddColumn(aList: TBSListView; aCaption: string; aWidth: Integer;
  aAutoSize: Boolean = False);
  
implementation

uses
  Types, IniFiles, bsComUtils, bsWinUtils, GKPersonNew, GKRecordSelect, GKStats,
  GKNoteEdit, GKChart, GKSourceEdit, GKEventEdit, GKAbout, GKChartCore,
  GKFileProperties, GKPersonEdit, GKExport, GKOptions, GKFamilyEdit,
  GKAssociationEdit, GKFilter, GKTreeTools, GKGroupEdit, GKPersonScan, GKMain,
  GKProgress, GKSourceCitEdit, GKRepositoryEdit, GKMediaEdit
  {$IFDEF PROFILER}, ZProfiler{$ENDIF}, Clipbrd, bsMiscUtils
  {$IFNDEF DELPHI_NET}, AbZipper, AbZipTyp, AbZipKit, AbArcTyp, GKMaps{$ENDIF};

{$R *.dfm}

procedure AddColumn(aList: TBSListView; aCaption: string; aWidth: Integer;
  aAutoSize: Boolean = False);
begin
  with aList.Columns.Add() do begin
    Caption := aCaption;
    Width := aWidth;
    AutoSize := aAutoSize;
  end;
end;

procedure ResizeColumn(aList: TBSListView; aColumnIndex: Integer);
var
  i, max_w, w: Integer;
  item: TListItem;
begin
  max_w := 0;

  for i := 0 to aList.Items.Count - 1 do begin
    item := aList.Items[i];

    if (aColumnIndex = 0)
    then w := aList.StringWidth(item.Caption)
    else w := aList.StringWidth(item.SubItems[aColumnIndex - 1]);

    if (max_w < w) then max_w := w;
  end;

  if (max_w <> 0)
  then aList.Columns[aColumnIndex].Width := max_w + 16;
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

{ TfmGEDKeeper }

procedure TfmBase.FormCreate(Sender: TObject);
begin
  FNavHistory := TList.Create;
  FChangedRecords := TList.Create;
  FTree := TGEDCOMTree.Create;
  FFilter := TPersonsFilter.Create;

  CreatePage(SheetPersons, rtIndividual, ListPersons, mPersonSummary);
  CreatePage(SheetFamilies, rtFamily, ListFamilies, mFamilySummary);
  CreatePage(SheetNotes, rtNote, ListNotes, mNoteSummary);
  CreatePage(SheetMultimedia, rtMultimedia, ListMultimedia, mMediaSummary);
  CreatePage(SheetSources, rtSource, ListSources, mSourceSummary);
  CreatePage(SheetRepositories, rtRepository, ListRepositories, mRepositorySummary);
  CreatePage(SheetGroups, rtGroup, ListGroups, mGroupSummary);

  ListsRefresh(True);
end;

procedure TfmBase.FormDestroy(Sender: TObject);
begin
  ListPersons.Clear;

  FFilter.Free;
  FTree.Destroy;
  FChangedRecords.Free;
  FNavHistory.Free;
end;

procedure TfmBase.CreatePage(aPage: TTabSheet; aRecType: TGEDCOMRecordType;
  var aList: TBSListView; var aSummary: THTMemo);
begin
  aSummary := THTMemo.Create(Self);
  with aSummary do begin
    Parent := aPage;
    BorderWidth := 4;
    Width := 318;
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

  aList := TBSListView.Create(Self);
  with aList do begin
    Parent := aPage;
    Align := alClient;
    HideSelection := False;
    ReadOnly := True;
    RowSelect := True;
    SortType := stText;
    ViewStyle := vsReport;
    SortColumn := 0;
    SortDirection := sdAscending;
    ShowSortSign := True;

    OnDblClick := RecordEdit;
    OnSelectItem := ListSelectItem;
  end;

  case aRecType of
    rtNone: {dummy};

    rtIndividual: begin
      ComListPersonsRefresh(aList, True);
    end;

    rtFamily: begin
      ComListFamiliesRefresh(aList, True);
    end;

    rtNote: begin
      ComListNotesRefresh(aList, True);
    end;

    rtMultimedia: begin
      ComListMultimediaRefresh(aList, True);
    end;

    rtSource: begin
      ComListSourcesRefresh(aList, True);
    end;

    rtRepository: begin
      ComListRepositoriesRefresh(aList, True);
    end;

    rtGroup: begin
      ComListGroupsRefresh(aList, True);
    end;

    rtSubmission, rtSubmitter: {dummy};
  end;
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

  NavClear();
end;

procedure TfmBase.SelectRecordByXRef(XRef: string);

  procedure SelectItemByRec(aList: TBSListView; aRec: TGEDCOMRecord; aTab: Integer);
  var
    i: Integer;
    item: TListItem;
  begin
    PageRecords.TabIndex := aTab;
    PageRecordsChange(nil);

    for i := 0 to aList.Items.Count - 1 do begin
      item := aList.Items[i];

      if (item.Data = aRec) then begin
        aList.Selected := item;
        item.MakeVisible(False);
        Break;
      end;
    end;
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
  else ;
end;

function TfmBase.GetChildFamily(iRec: TGEDCOMIndividualRecord; aCanCreate: Boolean;
  aNewParent: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;

  function GetFamilyBySpouse(): TGEDCOMFamilyRecord;
  var
    i: Integer;
    fam: TGEDCOMFamilyRecord;
    husb, wife: TGEDCOMIndividualRecord;
    msg: string;
  begin
    Result := nil;

    for i := 0 to FTree.Count - 1 do begin
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

  function CreateFamily(): TGEDCOMFamilyRecord;
  begin
    Result := TGEDCOMFamilyRecord.Create(FTree, FTree);
    Result.NewXRef();
    Result.ChangeDate.ChangeDateTime := Now();
    FTree.AddRecord(Result);
  end;

var
  fam_link: TGEDCOMChildToFamilyLink;
  fam: TGEDCOMFamilyRecord;
begin
  Result := nil;
  if (iRec = nil) then Exit;

  if (iRec.ChildToFamilyLinksCount <> 0) then begin
    Result := iRec.ChildToFamilyLinks[0].Family;
  end else begin
    if (aCanCreate) then begin
      fam := GetFamilyBySpouse();

      if (fam = nil)
      then fam := CreateFamily();

      fam.AddChild(TGEDCOMPointer.CreateTag(FTree, fam, 'CHIL', '@'+iRec.XRef+'@'));
      fam_link := TGEDCOMChildToFamilyLink.CreateTag(FTree, iRec, 'FAMC', fam.XRef);
      fam_link.Family := fam;
      iRec.AddChildToFamilyLink(fam_link);

      Result := fam;
    end;
  end;
end;

function TfmBase.CreatePerson(aName, aPatronymic, aFamily: string;
  aSex: TGEDCOMSex; aBirthEvent: Boolean = False): TGEDCOMIndividualRecord;
begin
  Result := CreatePersonEx(FTree, aName, aPatronymic, aFamily, aSex, aBirthEvent);
  ChangeRecord(Result);
  Modified := True;
end;

function TfmBase.CreatePersonDialog(aTarget: TGEDCOMIndividualRecord;
  aTargetMode: TTargetMode; aNeedSex: TGEDCOMSex): TGEDCOMIndividualRecord;
var
  dlg: TfmPersonNew;
begin
  Result := nil;

  dlg := TfmPersonNew.Create(nil);
  try
    dlg.EditSex.ItemIndex := Ord(aNeedSex);
    dlg.TargetMode := aTargetMode;
    dlg.Target := aTarget;
    if (dlg.ShowModal = mrOk) then begin
      Result := CreatePerson(dlg.EditName.Text, dlg.EditPatronymic.Text,
        dlg.EditFamily.Text, TGEDCOMSex(dlg.EditSex.ItemIndex), True);
    end;
  finally
    dlg.Destroy;
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

procedure TfmBase.DeleteFamily(aFamily: TGEDCOMFamilyRecord);
begin
  if (aFamily = nil) then Exit;

  if MessageDlg('Удалить семью "'+GetFamilyStr(aFamily)+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrNo
  then Exit;

  CleanFamily(aFamily);

  FTree.Delete(FTree.IndexOfRecord(aFamily));
end;

procedure TfmBase.DeleteIndividualRecord(iRec: TGEDCOMIndividualRecord);
var
  i: Integer;
  family: TGEDCOMFamilyRecord;
begin
  for i := iRec.ChildToFamilyLinksCount - 1 downto 0 do begin
    family := iRec.ChildToFamilyLinks[i].Family;
    family.RemoveChild(iRec.XRef);
  end;

  for i := iRec.SpouseToFamilyLinksCount - 1 downto 0 do begin
    family := iRec.SpouseToFamilyLinks[i].Family;
    RemoveFamilySpouse(FTree, family, iRec);
  end;

  FTree.Delete(FTree.IndexOfRecord(iRec));

  Modified := True;
end;

procedure TfmBase.DeleteNoteRecord(nRec: TGEDCOMNoteRecord);
var
  i, k: Integer;
  rec: TGEDCOMRecord;
begin
  for i := 0 to FTree.Count - 1 do begin
    rec := FTree.Records[i];

    for k := rec.NotesCount - 1 downto 0 do begin
      if (rec.Notes[k].Value = nRec)
      then rec.DeleteNotes(k);
    end;
  end;

  FTree.Delete(FTree.IndexOfRecord(nRec));

  Modified := True;
end;

procedure TfmBase.DeleteSourceRecord(srcRec: TGEDCOMSourceRecord);
var
  i, k: Integer;
  rec: TGEDCOMRecord;
begin
  for i := 0 to FTree.Count - 1 do begin
    rec := FTree.Records[i];

    for k := rec.SourceCitationsCount - 1 downto 0 do begin
      if (rec.SourceCitations[k].Value = srcRec)
      then rec.DeleteSourceCitation(k);
    end;
  end;

  FTree.Delete(FTree.IndexOfRecord(srcRec));

  Modified := True;
end;

procedure TfmBase.ListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  person: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
  source: TGEDCOMSourceRecord;
  media: TGEDCOMMultimediaRecord;
  note: TGEDCOMNoteRecord;
  rep: TGEDCOMRepositoryRecord;
  group: TGEDCOMGroupRecord;
begin
  if (Sender = ListPersons) then begin
    if (Item = nil)
    then ShowPersonInfo(nil, mPersonSummary.Lines)
    else begin
      person := TGEDCOMIndividualRecord(Item.Data);
      if (person = nil) then Exit;

      if not(Selected)
      then ShowPersonInfo(nil, mPersonSummary.Lines)
      else ShowPersonInfo(person, mPersonSummary.Lines);
    end;
  end
  else
  if (Sender = ListFamilies) then begin
    if (Item = nil)
    then ShowFamilyInfo(nil, mFamilySummary.Lines)
    else begin
      family := TGEDCOMFamilyRecord(Item.Data);
      if (family = nil) then Exit;

      if not(Selected)
      then ShowFamilyInfo(nil, mFamilySummary.Lines)
      else ShowFamilyInfo(family, mFamilySummary.Lines);
    end;
  end
  else
  if (Sender = ListNotes) then begin
    if (Item = nil) or not(Selected) then Exit;

    try
      note := TGEDCOMNoteRecord(Item.Data);
      ShowNoteInfo(note, mNoteSummary.Lines);
    except
      on E: Exception do LogWrite('ListNotesSelectItem(): ' + E.Message);
    end;
  end
  else
  if (Sender = ListMultimedia) then begin
    if (Item = nil) or not(Selected) then Exit;

    try
      media := TGEDCOMMultimediaRecord(Item.Data);
      ShowMultimediaInfo(media, mMediaSummary.Lines);
    except
      on E: Exception do LogWrite('ListMultimediaSelectItem(): ' + E.Message);
    end;
  end
  else
  if (Sender = ListSources) then begin
    if (Item = nil) or not(Selected) then Exit;
    try
      source := TGEDCOMSourceRecord(Item.Data);
      ShowSourceInfo(source, mSourceSummary.Lines);
    except
      on E: Exception do LogWrite('ListSourcesSelectItem(): ' + E.Message);
    end;
  end
  else
  if (Sender = ListRepositories) then begin
    if (Item = nil) or not(Selected) then Exit;

    try
      rep := TGEDCOMRepositoryRecord(Item.Data);
      ShowRepositoryInfo(rep, mRepositorySummary.Lines);
    except
      on E: Exception do LogWrite('ListRepositoriesSelectItem(): ' + E.Message);
    end;
  end
  else
  if (Sender = ListGroups) then begin
    if (Item = nil) or not(Selected) then Exit;

    try
      group := TGEDCOMGroupRecord(Item.Data);
      ShowGroupInfo(group, mGroupSummary.Lines);
    except
      on E: Exception do LogWrite('ListGroupsSelectItem(): ' + E.Message);
    end;
  end;
end;

procedure TfmBase.ShowPersonInfo(iRec: TGEDCOMIndividualRecord; aSummary: TStrings);
var
  idx, k: Integer;
  family: TGEDCOMFamilyRecord;
  sp: TGEDCOMPointer;
  rel_person: TGEDCOMIndividualRecord;
  st, unk, marr: string;
begin
  try
    if (iRec = nil) then Exit;

    NavAdd(iRec);

    try
      aSummary.BeginUpdate();
      aSummary.Clear();
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
        on E: Exception do LogWrite('PersonRefresh().Parents(): ' + E.Message);
      end;

      try
        for idx := 0 to iRec.SpouseToFamilyLinksCount - 1 do begin
          family := iRec.SpouseToFamilyLinks[idx].Family;
          if (family = nil) then begin
            LogWrite('File ('+FFileName+'), iRec ('+iRec.XRef+'): empty family entry');
            Continue;
          end;

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
        on E: Exception do LogWrite('PersonRefresh().Families(): ' + E.Message);
      end;

      RecListIndividualEventsRefresh(iRec, nil, aSummary);
      RecListNotesRefresh(iRec, nil, aSummary);
      RecListMediaRefresh(iRec, nil, aSummary);
      RecListSourcesRefresh(iRec, nil, aSummary);
      RecListAssociationsRefresh(iRec, nil, aSummary);
      RecListGroupsRefresh(iRec, nil, aSummary);
    finally
      aSummary.EndUpdate();
    end;
  except
    on E: Exception do LogWrite('PersonRefresh(): ' + E.Message);
  end;
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
    aSummary.Add('    Источники:');
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
      if (aList = nil) and (aSummary <> nil) then begin
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

        if (aList = nil) and (aSummary <> nil)
        then aSummary.Add(st + ': ' + GetEventDesc(event.Detail));

        ShowAddress(event.Detail.Address, aSummary);

        ShowDetailInfo(event.Detail, aSummary);

        if (aList <> nil) then begin
          item := aList.Items.Add();
          item.Caption := st;
          item.SubItems.Add(GEDCOMCustomDateToStr(event.Detail.Date.Value, fmGEDKeeper.Options.DefDateFormat));
          item.SubItems.Add(event.Detail.Place);
          item.SubItems.Add(event.Detail.Cause);
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

        if (aList = nil) and (aSummary <> nil)
        then aSummary.Add(st + ': ' + attr.StringValue);

        ShowAddress(attr.Detail.Address, aSummary);

        ShowDetailInfo(attr.Detail, aSummary);

        if (aList <> nil) then begin
          item := aList.Items.Add();
          item.Caption := st;
          item.SubItems.Add(GEDCOMCustomDateToStr(attr.Detail.Date.Value, fmGEDKeeper.Options.DefDateFormat));

          st := attr.StringValue;
          if (attr.Detail.Place <> '')
          then st := st + ' [' + attr.Detail.Place + ']';

          item.SubItems.Add(st);
          item.SubItems.Add(attr.Detail.Cause);
          item.Data := attr;
        end;
      end;

      if (aList <> nil) then begin
        //ResizeColumn(aList, 2);
        aList.SortColumn := 1;
      end;
    end;
  except
    on E: Exception do LogWrite('ListIndividualEventsRefresh(): ' + E.Message);
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
      if (aList = nil) and (aSummary <> nil)
      then begin
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

        if (aList = nil) and (aSummary <> nil)
        then aSummary.Add(st + ': ' + GetEventDesc(event.Detail));

        ShowDetailInfo(event.Detail, aSummary);

        if (aList <> nil) then begin
          item := aList.Items.Add();
          item.Caption := st;
          item.SubItems.Add(GEDCOMCustomDateToStr(event.Detail.Date.Value, fmGEDKeeper.Options.DefDateFormat));
          item.SubItems.Add(event.Detail.Place);
          item.SubItems.Add(event.Detail.Cause);
          item.Data := TObject(event);
        end;
      end;
    end;
  except
    on E: Exception do LogWrite('ListFamilyEventsRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.RecListNotesRefresh(aRecord: TGEDCOMRecord; aList: TBSListView; aSummary: TStrings);
var
  idx, k: Integer;
  note: TGEDCOMNotes;
  st: string;
begin
  try
    if (aList <> nil)
    then aList.Clear();

    if (aRecord.NotesCount <> 0) then begin
      if (aList = nil) and (aSummary <> nil)
      then begin
        aSummary.Add('');
        aSummary.Add({#13#10}'Заметки:');
      end;

      for idx := 0 to aRecord.NotesCount - 1 do begin
        note := aRecord.Notes[idx];

        //st := note.Notes.Text;
        //Hole(st);

        if (aList = nil) and (aSummary <> nil)
        then begin
          for k := 0 to note.Notes.Count - 1 do begin
            st := note.Notes[k];
            aSummary.Add(st);
          end;
          // fixme!!!
          //aSummary.AddStrings(note.Notes);
        end;

        st := Trim(note.Notes[0]);
        if (st = '') and (note.Notes.Count > 1)
        then st := Trim(note.Notes[1]);

        if (aList <> nil)
        then aList.AddItem(st, TObject(idx));
      end;
    end;
  except
    on E: Exception do LogWrite('ListNotesRefresh(): ' + E.Message);
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
        aSummary.Add({#13#10}'Мультимедиа:');
      end;

      for idx := 0 to aRecord.MultimediaLinksCount - 1 do begin
        mmLink := aRecord.MultimediaLinks[idx];
        mmRec := TGEDCOMMultimediaRecord(mmLink.Value);

        if (mmRec <> nil) and (mmRec.FileReferencesCount <> 0) then begin
          st := mmRec.FileReferences[0].Title;

          if (aList = nil) and (aSummary <> nil)
          then aSummary.Add('  ' + HyperLink(mmRec.XRef, st));

          if (aList <> nil)
          then aList.AddItem(st, TObject(idx));
        end;
      end;
    end;
  except
    on E: Exception do LogWrite('ListMediaRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.SetupRecChildsList(aList: TBSListView);
begin
  aList.Columns.BeginUpdate;
  aList.Columns.Clear;

  AddColumn(aList, 'Имя ребенка', 300);
  AddColumn(aList, 'Дата рождения', 100);

  aList.Columns.EndUpdate;
end;

procedure TfmBase.SetupRecEventsList(aList: TBSListView; PersonsMode: Boolean);
begin
  aList.Columns.BeginUpdate;
  aList.Columns.Clear;

  AddColumn(aList, 'Событие', 75);
  AddColumn(aList, 'Дата', 80);

  if not(PersonsMode)
  then AddColumn(aList, 'Место', 200)
  else AddColumn(aList, 'Место/Атрибут', 200);

  AddColumn(aList, 'Причина', 130);

  aList.Columns.EndUpdate;
end;

procedure TfmBase.SetupRecMembersList(aList: TBSListView);
begin
  aList.Columns.BeginUpdate;
  aList.Columns.Clear;

  AddColumn(aList, 'Имя участника группы', 300);

  aList.Columns.EndUpdate;
end;

procedure TfmBase.SetupRecSpousesList(aList: TBSListView);
begin
  aList.Columns.BeginUpdate;
  aList.Columns.Clear;

  AddColumn(aList, 'Имя супруга', 300);
  AddColumn(aList, 'Дата брака', 100);

  aList.Columns.EndUpdate;
end;

procedure TfmBase.SetupRecAssociationsList(aList: TBSListView);
begin
  aList.Columns.BeginUpdate;
  aList.Columns.Clear;

  AddColumn(aList, 'Отношение', 300);
  AddColumn(aList, 'Персона', 200);

  aList.Columns.EndUpdate;
end;

procedure TfmBase.SetupRecGroupsList(aList: TBSListView);
begin
  aList.Columns.BeginUpdate;
  aList.Columns.Clear;

  AddColumn(aList, 'Группа', 350);

  aList.Columns.EndUpdate;
end;

procedure TfmBase.SetupRecNotesList(aList: TBSListView);
begin
  aList.Columns.BeginUpdate;
  aList.Columns.Clear;

  AddColumn(aList, 'Заметка', 300);

  aList.Columns.EndUpdate;
end;

procedure TfmBase.SetupRecMediaList(aList: TBSListView);
begin
  aList.Columns.BeginUpdate;
  aList.Columns.Clear;

  AddColumn(aList, 'Мультимедиа', 300);

  aList.Columns.EndUpdate;
end;

procedure TfmBase.SetupRecRepositoriesList(aList: TBSListView);
begin
  aList.Columns.BeginUpdate;
  aList.Columns.Clear;

  AddColumn(aList, 'Архив', 300);

  aList.Columns.EndUpdate;
end;

procedure TfmBase.SetupRecSourcesList(aList: TBSListView);
begin
  aList.Columns.BeginUpdate;
  aList.Columns.Clear;

  AddColumn(aList, 'Автор', 120);
  AddColumn(aList, 'Название', 180);

  aList.Columns.EndUpdate;
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
        aSummary.Add({#13#10}'Источники:');
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
            item.Data := TObject(idx);
          end;
        end;
      end;

      if (aList <> nil) then ResizeColumn(aList, 1);
    end;
  except
    on E: Exception do LogWrite('ListSourcesRefresh(): ' + E.Message);
  end;
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
          item.Data := TObject(idx);
        end;
      end;
    end;
  except
    on E: Exception do LogWrite('ListAssociationsRefresh(): ' + E.Message);
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
            item.Data := TObject(idx);
          end;
        end;
      end;
    end;
  except
    on E: Exception do LogWrite('ListGroupsRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.CalcCounts();
var
  i: Integer;
  rec: TGEDCOMRecord;
begin
  for i := Ord(Low(TGEDCOMRecordType)) to Ord(High(TGEDCOMRecordType)) do begin
    FCounts[TGEDCOMRecordType(i)].Total := 0;
    FCounts[TGEDCOMRecordType(i)].Filtered := 0;
  end;

  for i := 0 to FTree.Count - 1 do begin
    rec := FTree.Records[i];

    if (rec is TGEDCOMFamilyRecord)
    then Inc(FCounts[rtFamily].Total)
    else
    if (rec is TGEDCOMIndividualRecord)
    then Inc(FCounts[rtIndividual].Total)
    else
    if (rec is TGEDCOMMultimediaRecord)
    then Inc(FCounts[rtMultimedia].Total)
    else
    if (rec is TGEDCOMNoteRecord)
    then Inc(FCounts[rtNote].Total)
    else
    if (rec is TGEDCOMRepositoryRecord)
    then Inc(FCounts[rtRepository].Total)
    else
    if (rec is TGEDCOMSourceRecord)
    then Inc(FCounts[rtSource].Total)
    else
    if (rec is TGEDCOMSubmissionRecord)
    then Inc(FCounts[rtSubmission].Total)
    else
    if (rec is TGEDCOMSubmitterRecord)
    then Inc(FCounts[rtSubmitter].Total)
    else
    if (rec is TGEDCOMGroupRecord)
    then Inc(FCounts[rtGroup].Total);
  end;

  for i := Ord(Low(TGEDCOMRecordType)) to Ord(High(TGEDCOMRecordType)) do begin
    FCounts[TGEDCOMRecordType(i)].Filtered := FCounts[TGEDCOMRecordType(i)].Total;
  end;
end;

procedure TfmBase.ListsRefresh(aTitles: Boolean = False);
var
  bm: {$IFNDEF DELPHI_NET}Pointer{$ELSE}TObject{$ENDIF};

  procedure SaveBookmark(aList: TBSListView);
  begin
    if (aList.Selected = nil)
    then bm := nil
    else bm := aList.Selected.Data;
  end;

  procedure RestoreBookmark(aList: TBSListView);
  var
    item: TListItem;
  begin
    item := aList.FindData(0, bm, True, False);
    if (item <> nil) then begin
      item.Selected := True;
      item.MakeVisible(False);
    end;
  end;

begin
  CalcCounts();

  SaveBookmark(ListPersons);
  ComListPersonsRefresh(ListPersons, aTitles);
  RestoreBookmark(ListPersons);

  SaveBookmark(ListFamilies);
  ComListFamiliesRefresh(ListFamilies, aTitles);
  RestoreBookmark(ListFamilies);

  SaveBookmark(ListNotes);
  ComListNotesRefresh(ListNotes, aTitles);
  RestoreBookmark(ListNotes);

  SaveBookmark(ListMultimedia);
  ComListMultimediaRefresh(ListMultimedia, aTitles);
  RestoreBookmark(ListMultimedia);

  SaveBookmark(ListSources);
  ComListSourcesRefresh(ListSources, aTitles);
  RestoreBookmark(ListSources);

  SaveBookmark(ListRepositories);
  ComListRepositoriesRefresh(ListRepositories, aTitles);
  RestoreBookmark(ListRepositories);

  SaveBookmark(ListGroups);
  ComListGroupsRefresh(ListGroups, aTitles);
  RestoreBookmark(ListGroups);

  PageRecordsChange(nil);
end;

procedure TfmBase.ComListPersonsRefresh(aList: TBSListView; aTitles: Boolean);

  function GetGroups(iRec: TGEDCOMIndividualRecord): string;
  var
    idx: Integer;
    grp: TGEDCOMGroupRecord;
    ptr: TGEDCOMPointer;
  begin
    Result := '';

    for idx := 0 to iRec.GroupsCount - 1 do begin
      ptr := iRec.Groups[idx];
      grp := TGEDCOMGroupRecord(ptr.Value);
      if (grp <> nil) then begin
        Result := Result + grp.Name;

        if (idx < iRec.GroupsCount - 1)
        then Result := Result + '; ';
      end;
    end;
  end;

  function IsMatchesMask(const aName, Mask: string): Boolean;
  var
    i, tok_count: Integer;
  begin
    Result := False;
    tok_count := GetTokensCount(Mask, '|');
    for i := 1 to tok_count do
      Result := Result or MatchesMask(aName, GetToken(Mask, '|', i));
  end;

  procedure PrepareRec(iRec: TGEDCOMIndividualRecord; aItem: TListItem);
  var
    f, n, p, nm{, gcd}: string;
    ev: TGEDCOMCustomEvent;
    bd, dd, fdt: TDateTime;
    res, isLive: Boolean;
    p_tag: TGEDCOMTag;

    bi_date, de_date, bi_place, de_place, resi_place: string;
    i: Integer;
    grp: TGEDCOMGroupRecord;
  begin
    nm := GetNameStr(iRec);
    p_tag := iRec.FindTag(PatriarchTag);

    // new begin
    bd := 0.0;
    dd := 0.0;

    bi_date := '';
    de_date := '';
    bi_place := '';
    de_place := '';

    isLive := True;

    for i := 0 to iRec.IndividualEventsCount - 1 do begin
      ev := iRec.IndividualEvents[i];

      if (ev.Name = 'BIRT') then begin
        bi_date := GEDCOMCustomDateToStr(ev.Detail.Date.Value, fmGEDKeeper.Options.DefDateFormat);
        bi_place := ev.Detail.Place;
        bd := GEDCOMDateToDate(ev.Detail.Date.Value);
      end
      else
      if (ev.Name = 'DEAT') then begin
        de_date := GEDCOMCustomDateToStr(ev.Detail.Date.Value, fmGEDKeeper.Options.DefDateFormat);
        de_place := ev.Detail.Place;
        dd := GEDCOMDateToDate(ev.Detail.Date.Value);
        isLive := False;
      end;
    end;

    resi_place := GetResidencePlace(iRec, fmGEDKeeper.Options.PlacesWithAddress);
    // new end

    if (aList = ListPersons) then begin
      case FFilter.LifeMode of
        lmAll: ;

        lmOnlyAlive: if not(isLive) then Exit;

        lmOnlyDead: if (isLive) then Exit;

        lmAliveBefore: begin
          fdt := StrToDate(FFilter.AliveBeforeDate);
          res := ((bd <> 0) and (bd < fdt)) and ((dd = 0) or ((dd <> 0) and (dd > fdt)));
          if not(res) then Exit;
        end;
      end;

      if ((FFilter.Sex <> svNone) and (iRec.Sex <> FFilter.Sex))
      or ((FFilter.Name <> '*') and not(IsMatchesMask(nm, FFilter.Name)))
      or ((FFilter.Residence <> '*') and not(MatchesMask(resi_place, FFilter.Residence)))
      or ((FFilter.PatriarchOnly and (p_tag = nil)))
      then Exit;

      case FFilter.GroupMode of
        gmAll: ;
        gmNone: if (iRec.GroupsCount <> 0) then Exit;
        gmAny: if (iRec.GroupsCount = 0) then Exit;
        gmSelected: begin
          grp := FTree.XRefIndex_Find(FFilter.GroupRef) as TGEDCOMGroupRecord;
          if (iRec.IndexOfGroup(grp) < 0) then Exit;
        end;
      end;
    end else begin
      if ((FFilter.Sex <> svNone) and (iRec.Sex <> FFilter.Sex))
      then Exit;
    end;

    FCounts[rtIndividual].Filtered := FCounts[rtIndividual].Filtered + 1;

    if (aItem = nil)
    then aItem := aList.Items.Add()
    else aItem.SubItems.Clear();

    //

    aItem.Caption := IntToStr(GetId(iRec));
    aItem.Data := iRec;

    if (p_tag = nil)
    then aItem.SubItems.Add(' ')
    else aItem.SubItems.Add('*');

    case fmGEDKeeper.Options.DefNameFormat of
      nfFNP: begin
        aItem.SubItems.Add(nm);
      end;
      nfF_NP: begin
        GetNameParts(iRec, f, n, p);
        aItem.SubItems.Add(f);
        aItem.SubItems.Add(n + ' ' + p);
      end;
      nfF_N_P: begin
        GetNameParts(iRec, f, n, p);
        aItem.SubItems.Add(f);
        aItem.SubItems.Add(n);
        aItem.SubItems.Add(p);
      end;
    end;

    aItem.SubItems.Add(SexSigns[iRec.Sex]);
    aItem.SubItems.Add(bi_date);
    aItem.SubItems.Add(de_date);
    aItem.SubItems.Add(bi_place);
    aItem.SubItems.Add(de_place);
    aItem.SubItems.Add(resi_place);

    if (aList = ListPersons) then begin
      aItem.SubItems.Add(GetAge(iRec));
      aItem.SubItems.Add(GetLifeExpectancy(iRec));
      aItem.SubItems.Add(GetDaysForBirth(iRec));
      aItem.SubItems.Add(GetGroups(iRec));
      aItem.SubItems.Add(GetChangeDate(iRec));
    end;
  end;

var
  i: Integer;
  fullRefresh: Boolean;
  item: TListItem;
  rec: TGEDCOMRecord;
begin
  if (aTitles) then begin
    aList.Columns.BeginUpdate;
    aList.Columns.Clear;

    AddColumn(aList, '№', 50);
    AddColumn(aList, 'П', 25);
    case fmGEDKeeper.Options.DefNameFormat of
      nfFNP: begin
        AddColumn(aList, 'Фамилия,Имя,Отчество', 300);
      end;

      nfF_NP: begin
        AddColumn(aList, 'Фамилия', 150);
        AddColumn(aList, 'Имя,Отчество', 150);
      end;

      nfF_N_P: begin
        AddColumn(aList, 'Фамилия', 150);
        AddColumn(aList, 'Имя', 100);
        AddColumn(aList, 'Отчество', 150);
      end;
    end;
    AddColumn(aList, 'Пол', 45);
    AddColumn(aList, 'Дата рождения', 100);
    AddColumn(aList, 'Дата смерти', 100);
    AddColumn(aList, 'Место рождения', 100);
    AddColumn(aList, 'Место смерти', 100);
    AddColumn(aList, 'Местожительство', 100);
    if (aList = ListPersons) then begin
      AddColumn(aList, 'Возраст', 100);
      AddColumn(aList, 'Продолжительность жизни', 100);
      AddColumn(aList, 'Дней до ДР', 100);
      AddColumn(aList, 'Группа', 200);
      AddColumn(aList, 'Изменено', 150);
    end;

    aList.Columns.EndUpdate;
  end;

  FCounts[rtIndividual].Filtered := 0;

  if (aList <> ListPersons)
  then fullRefresh := True
  else
    if (FChangedRecords.Count = 0)
    then fullRefresh := True
    else fullRefresh := False;

  aList.Items.BeginUpdate();

  {$IFDEF PROFILER}Profiler.Mark(2, True);{$ENDIF}

  if (fullRefresh) then begin
    aList.Items.Clear();
    for i := 0 to FTree.Count - 1 do begin
      rec := FTree.Records[i];

      if (rec is TGEDCOMIndividualRecord)
      then PrepareRec(rec as TGEDCOMIndividualRecord, nil);
    end;
  end else begin
    for i := FChangedRecords.Count - 1 downto 0 do begin
      rec := TObject(FChangedRecords[i]) as TGEDCOMRecord;

      if (rec is TGEDCOMIndividualRecord) then begin
        item := aList.FindData(0, rec, True, False);
        PrepareRec(rec as TGEDCOMIndividualRecord, item);
        FChangedRecords.Delete(i);
      end;
    end;
  end;

  {$IFDEF PROFILER}Profiler.Mark(2, False);{$ENDIF}

  {$IFDEF PROFILER}Profiler.Mark(3, True);{$ENDIF}
  ResizeColumn(aList, 2);
  {$IFDEF PROFILER}Profiler.Mark(3, False);{$ENDIF}

  aList.Items.EndUpdate();
end;

procedure TfmBase.ComListFamiliesRefresh(aList: TBSListView; aTitles: Boolean);
var
  i: Integer;
  item: TListItem;
  famRec: TGEDCOMFamilyRecord;
begin
  if (aTitles) then begin
    aList.Columns.BeginUpdate;
    aList.Columns.Clear;
    AddColumn(aList, '№', 50);
    AddColumn(aList, 'Супруги', 300);
    AddColumn(aList, 'Дата брака', 100);
    if (aList = ListFamilies) then begin
      AddColumn(aList, 'Изменено', 150);
    end;
    aList.Columns.EndUpdate;
  end;

  aList.Items.BeginUpdate();
  aList.Items.Clear();

  for i := 0 to FTree.Count - 1 do
    if (FTree.Records[i] is TGEDCOMFamilyRecord) then begin
      famRec := FTree.Records[i] as TGEDCOMFamilyRecord;

      item := aList.Items.Add();
      item.Caption := IntToStr(GetId(famRec));
      item.SubItems.Add(GetFamilyStr(famRec));
      item.SubItems.Add(GetMarriageDate(famRec, fmGEDKeeper.Options.DefDateFormat));

      if (aList = ListFamilies)
      then item.SubItems.Add(GetChangeDate(famRec));

      item.Data := famRec;
    end;

  ResizeColumn(aList, 1);

  aList.Items.EndUpdate();
end;

procedure TfmBase.ComListMultimediaRefresh(aList: TBSListView; aTitles: Boolean);
var
  i: Integer;
  item: TListItem;
  mmRec: TGEDCOMMultimediaRecord;
  file_ref: TGEDCOMFileReferenceWithTitle;
begin
  if (aTitles) then begin
    aList.Columns.BeginUpdate;
    aList.Columns.Clear;
    AddColumn(aList, '№', 50);
    AddColumn(aList, 'Название', 150);
    AddColumn(aList, 'Тип', 85);
    AddColumn(aList, 'Файл', 300);
    if (aList = ListMultimedia) then begin
      AddColumn(aList, 'Изменено', 150);
    end;
    aList.Columns.EndUpdate;
  end;

  aList.Items.BeginUpdate();
  aList.Items.Clear();

  for i := 0 to FTree.Count - 1 do
    if (FTree.Records[i] is TGEDCOMMultimediaRecord) then begin
      mmRec := FTree.Records[i] as TGEDCOMMultimediaRecord;
      file_ref := mmRec.FileReferences[0];

      item := aList.Items.Add();
      item.Caption := IntToStr(GetId(mmRec));

      item.SubItems.Add(file_ref.Title);
      item.SubItems.Add(MediaTypes[file_ref.MediaType].Name);
      item.SubItems.Add(file_ref.StringValue);

      if (aList = ListMultimedia)
      then item.SubItems.Add(GetChangeDate(mmRec));

      item.Data := mmRec;
    end;

  ResizeColumn(aList, 1);

  aList.Items.EndUpdate();
end;

procedure TfmBase.ComListNotesRefresh(aList: TBSListView; aTitles: Boolean);
var
  i: Integer;
  item: TListItem;
  note: TGEDCOMNoteRecord;
  st: string;
begin
  if (aTitles) then begin
    aList.Columns.BeginUpdate;
    aList.Columns.Clear;
    AddColumn(aList, '№', 50);
    AddColumn(aList, 'Заметка', 400);
    if (aList = ListNotes) then begin
      AddColumn(aList, 'Изменено', 150);
    end;
    aList.Columns.EndUpdate;
  end;

  aList.Items.BeginUpdate();
  aList.Items.Clear();

  for i := 0 to FTree.Count - 1 do
    if (FTree.Records[i] is TGEDCOMNoteRecord) then begin
      note := FTree.Records[i] as TGEDCOMNoteRecord;

      if (note.Notes.Count > 0) then begin
        st := Trim(note.Notes[0]);
        if (st = '') and (note.Notes.Count > 1)
        then st := Trim(note.Notes[1]);
      end else st := '';

      item := aList.Items.Add();
      item.Caption := IntToStr(GetId(note));
      item.SubItems.Add(st);

      if (aList = ListNotes)
      then item.SubItems.Add(GetChangeDate(note));

      item.Data := note;
    end;

  aList.Items.EndUpdate();
end;

procedure TfmBase.ComListSourcesRefresh(aList: TBSListView; aTitles: Boolean);
var
  i: Integer;
  item: TListItem;
  srcRec: TGEDCOMSourceRecord;
begin
  if (aTitles) then begin
    aList.Columns.BeginUpdate;
    aList.Columns.Clear;
    AddColumn(aList, '№', 50);
    AddColumn(aList, 'Краткое название', 120);
    AddColumn(aList, 'Автор', 200);
    AddColumn(aList, 'Название', 200);
    if (aList = ListSources) then begin
      AddColumn(aList, 'Изменено', 150);
    end;
    aList.Columns.EndUpdate;
  end;

  aList.Items.BeginUpdate();
  aList.Items.Clear();

  for i := 0 to FTree.Count - 1 do
    if (FTree.Records[i] is TGEDCOMSourceRecord) then begin
      srcRec := FTree.Records[i] as TGEDCOMSourceRecord;

      item := aList.Items.Add();
      item.Caption := IntToStr(GetId(srcRec));
      item.SubItems.Add(Trim(srcRec.FiledByEntry));
      item.SubItems.Add(Trim(srcRec.Originator.Text));
      item.SubItems.Add(Trim(srcRec.Title.Text));

      if (aList = ListSources)
      then item.SubItems.Add(GetChangeDate(srcRec));

      item.Data := srcRec;
    end;

  ResizeColumn(aList, 1);

  aList.Items.EndUpdate();
end;

procedure TfmBase.ComListRepositoriesRefresh(aList: TBSListView; aTitles: Boolean);
var
  i: Integer;
  item: TListItem;
  repRec: TGEDCOMRepositoryRecord;
begin
  if (aTitles) then begin
    aList.Columns.BeginUpdate;
    aList.Columns.Clear;
    AddColumn(aList, '№', 50);
    AddColumn(aList, 'Архив', 400);
    if (aList = ListRepositories) then begin
      AddColumn(aList, 'Изменено', 150);
    end;
    aList.Columns.EndUpdate;
  end;

  aList.Items.BeginUpdate();
  aList.Items.Clear();

  for i := 0 to FTree.Count - 1 do
    if (FTree.Records[i] is TGEDCOMRepositoryRecord) then begin
      repRec := FTree.Records[i] as TGEDCOMRepositoryRecord;

      item := aList.Items.Add();
      item.Caption := IntToStr(GetId(repRec));
      item.SubItems.Add(repRec.RepositoryName);

      if (aList = ListRepositories)
      then item.SubItems.Add(GetChangeDate(repRec));

      item.Data := repRec;
    end;

  aList.Items.EndUpdate();
end;

procedure TfmBase.ComListGroupsRefresh(aList: TBSListView; aTitles: Boolean);
var
  i: Integer;
  item: TListItem;
  groupRec: TGEDCOMGroupRecord;
begin
  if (aTitles) then begin
    aList.Columns.BeginUpdate;
    aList.Columns.Clear;
    AddColumn(aList, '№', 50);
    AddColumn(aList, 'Группа', 400);
    if (aList = ListGroups) then begin
      AddColumn(aList, 'Изменено', 150);
    end;
    aList.Columns.EndUpdate;
  end;

  aList.Items.BeginUpdate();
  aList.Items.Clear();

  for i := 0 to FTree.Count - 1 do
    if (FTree.Records[i] is TGEDCOMGroupRecord) then begin
      groupRec := FTree.Records[i] as TGEDCOMGroupRecord;

      item := aList.Items.Add();
      item.Caption := IntToStr(GetId(groupRec));
      item.SubItems.Add(groupRec.Name);

      if (aList = ListGroups)
      then item.SubItems.Add(GetChangeDate(groupRec));

      item.Data := groupRec;
    end;

  aList.Items.EndUpdate();
end;

function TfmBase.GetSelectedRecord(aList: TBSListView): TGEDCOMRecord;
begin
  if (aList.Selected = nil)
  then Result := nil
  else Result := TGEDCOMRecord(aList.Selected.Data);
end;

function TfmBase.GetSelectedPerson(): TGEDCOMIndividualRecord;
begin
  Result := TGEDCOMIndividualRecord(GetSelectedRecord(ListPersons));
end;

function TfmBase.ModifyRecEvent(aRecord: TGEDCOMRecord;
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

    case fmEventEdit.ShowModal of
      mrOk: begin
        if (aEvent = nil) then begin
          if (aRecord is TGEDCOMIndividualRecord) then begin
            event := fmEventEdit.Event;

            if (event is TGEDCOMIndividualEvent)
            then TGEDCOMIndividualRecord(aRecord).AddIndividualEvent(TGEDCOMIndividualEvent(event))
            else TGEDCOMIndividualRecord(aRecord).AddIndividualAttribute(TGEDCOMIndividualAttribute(event));
          end else TGEDCOMFamilyRecord(aRecord).AddFamilyEvent(TGEDCOMFamilyEvent(event));
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

function TfmBase.ModifyPerson(var aIndivRec: TGEDCOMIndividualRecord): Boolean;
begin
  fmPersonEdit := TfmPersonEdit.Create(Self);
  try
    fmPersonEdit.Person := aIndivRec;
    Result := (fmPersonEdit.ShowModal = mrOk);
  finally
    fmPersonEdit.Destroy;
    fmPersonEdit := nil;
  end;
end;

function TfmBase.ModifyFamily(var aFamilyRec: TGEDCOMFamilyRecord; aSpouse: TGEDCOMIndividualRecord = nil): Boolean;
var
  fmFamEdit: TfmFamilyEdit;
  exists: Boolean;
begin
  Result := False;

  if (aSpouse <> nil) and not(aSpouse.Sex in [svMale, svFemale]) then begin
    MessageDlg('У данного человека не задан пол.', mtError, [mbOk], 0);
    Exit;
  end;

  fmFamEdit := TfmFamilyEdit.Create(Self);
  try
    exists := (aFamilyRec <> nil);

    if not(exists) then begin
      aFamilyRec := TGEDCOMFamilyRecord.Create(FTree, FTree);
      aFamilyRec.NewXRef;
    end;

    if (aSpouse <> nil)
    then AddSpouseToFamily(FTree, aFamilyRec, aSpouse);

    fmFamEdit.Tree := FTree;
    fmFamEdit.Family := aFamilyRec;

    if (fmFamEdit.ShowModal = mrOk) then begin
      if not(exists)
      then FTree.AddRecord(aFamilyRec);

      Result := True;
    end else begin
      if not(exists) then begin
        CleanFamily(aFamilyRec);
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

  fmNoteEdit := TfmNoteEdit.Create(Self);
  try
    exists := (aNoteRec <> nil);

    if not(exists) then begin
      aNoteRec := TGEDCOMNoteRecord.Create(FTree, FTree);
      aNoteRec.NewXRef;
    end;

    fmNoteEdit.NoteRecord := aNoteRec;

    if (fmNoteEdit.ShowModal = mrOk) then begin
      if not(exists)
      then FTree.AddRecord(aNoteRec);

      Result := True;
    end else begin
      if not(exists)
      then FreeAndNil(aNoteRec);
    end;
  finally
    fmNoteEdit.Destroy;
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
      aMediaRec.NewXRef;
    end;

    fmMediaEdit.MediaRec := aMediaRec;

    if (fmMediaEdit.ShowModal = mrOk) then begin
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
      aSourceRec.NewXRef;
    end;

    fmSrcEdit.SourceRecord := aSourceRec;

    if (fmSrcEdit.ShowModal = mrOk) then begin
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
      aRepRec.NewXRef;
    end;

    fmRepEdit.RepositoryRecord := aRepRec;

    if (fmRepEdit.ShowModal = mrOk) then begin
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
      aGroupRec.NewXRef;
    end;

    fmGrpEdit.Group := aGroupRec;

    if (fmGrpEdit.ShowModal = mrOk) then begin
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

function TfmBase.ModifyRecNote(aRecord: TGEDCOMRecord;
  aIndex: Integer; anAction: TRecAction): Boolean;
var
  noteRec: TGEDCOMNoteRecord;
  note: TGEDCOMNotes;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ссылку на заметку?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    aRecord.DeleteNotes(aIndex);

    Result := True;
    Modified := True;

    Exit;
  end;

  if (anAction = raEdit) then begin
    if (aIndex > -1) then begin
      note := aRecord.Notes[aIndex];
      noteRec := TGEDCOMNoteRecord(note.Value);
      Result := ModifyNote(noteRec);
    end;
  end else begin
    noteRec := TGEDCOMNoteRecord(SelectRecord(smNote));
    if (noteRec <> nil) then begin
      note := TGEDCOMNotes.Create(FTree, aRecord);
      note.Value := noteRec;
      aRecord.AddNotes(note);

      Result := True;
    end;
  end;
end;

function TfmBase.ModifyRecMultimedia(aRecord: TGEDCOMRecord;
  aIndex: Integer; anAction: TRecAction): Boolean;
var
  mmRec: TGEDCOMMultimediaRecord;
  mmLink: TGEDCOMMultimediaLink;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ссылку на мультимедиа?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    aRecord.DeleteMultimediaLink(aIndex);

    Result := True;
    Modified := True;

    Exit;
  end;

  if (anAction = raEdit) then begin
    if (aIndex > -1) then begin
      mmLink := aRecord.MultimediaLinks[aIndex];
      mmRec := TGEDCOMMultimediaRecord(mmLink.Value);
      Result := ModifyMedia(mmRec);
    end;
  end else begin
    mmRec := TGEDCOMMultimediaRecord(SelectRecord(smMultimedia));
    if (mmRec <> nil) then begin
      mmLink := TGEDCOMMultimediaLink.Create(FTree, aRecord);
      mmLink.Value := mmRec;
      aRecord.AddMultimediaLink(mmLink);

      Result := True;
    end;
  end;
end;

function TfmBase.ModifyRecSource(aRecord: TGEDCOMRecord;
  aIndex: Integer; anAction: TRecAction): Boolean;
var
  cit: TGEDCOMSourceCitation;
  fmSrcCitEdit: TfmSourceCitEdit;
  res: Integer;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ссылку на источник?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    aRecord.DeleteSourceCitation(aIndex);

    Result := True;
    Modified := True;

    Exit;
  end;

  fmSrcCitEdit := TfmSourceCitEdit.Create(Self);
  try
    if (anAction = raEdit) and (aIndex > -1)
    then cit := aRecord.SourceCitations[aIndex]
    else cit := TGEDCOMSourceCitation.Create(FTree, aRecord);

    fmSrcCitEdit.SourceCitation := cit;
    res := fmSrcCitEdit.ShowModal;

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

function TfmBase.ModifyRecAssociation(aRecord: TGEDCOMIndividualRecord;
  aIndex: Integer; anAction: TRecAction): Boolean;
var
  fmAstEdit: TfmAssociationEdit;
  ast: TGEDCOMAssociation;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ассоциацию?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    aRecord.DeleteAssociation(aIndex);

    Result := True;
    Modified := True;

    Exit;
  end;

  fmAstEdit := TfmAssociationEdit.Create(Self);
  try
    if (aIndex > -1) then begin
      ast := aRecord.Associations[aIndex];
    end else begin
      ast := TGEDCOMAssociation.CreateTag(FTree, aRecord, 'ASSO', '');
    end;

    fmAstEdit.Association := ast;

    if (fmAstEdit.ShowModal = mrOk) then begin
      if (aIndex = -1)
      then aRecord.AddAssociation(ast);

      Result := True;
    end;
  finally
    fmAstEdit.Destroy;
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

procedure TfmBase.ShowNoteInfo(aNoteRec: TGEDCOMNoteRecord; aSummary: TStrings);

  function FindInTag(aTag: TGEDCOMTagWithLists): Boolean;
  var
    i: Integer;
  begin
    Result := False;

    for i := 0 to aTag.NotesCount - 1 do
      if (aTag.Notes[i].Value = aNoteRec) then begin
        Result := True;
        Break;
      end;
  end;

var
  i, k, m: Integer;
  rec: TGEDCOMRecord;
  iRec: TGEDCOMIndividualRecord;
  famRec: TGEDCOMFamilyRecord;
begin
  aSummary.BeginUpdate();
  try
    aSummary.Clear();

    aSummary.Add('');
    aSummary.AddStrings(aNoteRec.Notes);

    aSummary.Add('');
    aSummary.Add('Ссылки:');

    for i := 0 to FTree.Count - 1 do begin
      rec := FTree.Records[i];

      // notes in record events
      if (rec is TGEDCOMIndividualRecord) then begin
        iRec := TGEDCOMIndividualRecord(rec);

        for m := 0 to iRec.IndividualEventsCount - 1 do begin
          if FindInTag(iRec.IndividualEvents[m].Detail) then begin
            aSummary.Add('    '+HyperLink(rec.XRef, GetNameStr(iRec)) + ', события');
            Break;
          end;
        end;

        for m := 0 to iRec.IndividualAttributesCount - 1 do begin
          if FindInTag(iRec.IndividualAttributes[m].Detail) then begin
            aSummary.Add('    '+HyperLink(rec.XRef, GetNameStr(iRec)) + ', атрибуты');
            Break;
          end;
        end;
      end;

      if (rec is TGEDCOMFamilyRecord) then begin
        famRec := TGEDCOMFamilyRecord(rec);

        for m := 0 to famRec.FamilyEventCount - 1 do begin
          if FindInTag(famRec.FamilyEvents[m].Detail) then begin
            aSummary.Add('    '+HyperLink(rec.XRef, 'Семья: ' + GetFamilyStr(famRec)) + ', события');
            Break;
          end;
        end;
      end;

      // notes in record
      for k := 0 to rec.NotesCount - 1 do
        if (rec.Notes[k].Value = aNoteRec)
        then aSummary.Add('    '+GetRecordLink(rec, True));
    end;
  finally
    aSummary.EndUpdate();
  end;
end;

function TfmBase.GetRecordLink(aRecord: TGEDCOMRecord; aLinkDesc: Boolean): string;
var
  st: string;
begin
  if (aRecord is TGEDCOMIndividualRecord)
  then st := GetNameStr(aRecord as TGEDCOMIndividualRecord)
  else
  if (aRecord is TGEDCOMFamilyRecord)
  then st := 'Семья: ' + GetFamilyStr(aRecord as TGEDCOMFamilyRecord)
  else
  if (aRecord is TGEDCOMMultimediaRecord)
  then st := 'Медиа-объект: ' + TGEDCOMMultimediaRecord(aRecord).FileReferences[0].Title
  else
  if (aRecord is TGEDCOMGroupRecord)
  then st := 'Группа: ' + TGEDCOMGroupRecord(aRecord).Name
  else
  if (aRecord is TGEDCOMSourceRecord)
  then st := 'Источник: ' + TGEDCOMSourceRecord(aRecord).FiledByEntry
  else
  if (aRecord is TGEDCOMRepositoryRecord)
  then st := 'Архив: ' + TGEDCOMRepositoryRecord(aRecord).RepositoryName
  else st := aRecord.XRef;

  Result := HyperLink(aRecord.XRef, st);
end;

procedure TfmBase.ShowMultimediaInfo(aMultimediaRec: TGEDCOMMultimediaRecord; aSummary: TStrings);
var
  i, k: Integer;
  rec: TGEDCOMRecord;
begin
  aSummary.BeginUpdate();
  try
    aSummary.Clear();
    aSummary.Add('');
    aSummary.Add('Ссылки:');
    for i := 0 to FTree.Count - 1 do begin
      rec := FTree.Records[i];

      for k := 0 to rec.MultimediaLinksCount - 1 do
        if (rec.MultimediaLinks[k].Value = aMultimediaRec)
        then aSummary.Add('    '+GetRecordLink(rec, True));
    end;
  finally
    aSummary.EndUpdate();
  end;
end;

procedure TfmBase.ShowSourceInfo(aSourceRec: TGEDCOMSourceRecord; aSummary: TStrings);
var
  link_list: TStringList;

  procedure OutLink(aRec: TGEDCOMRecord; aCit: TGEDCOMSourceCitation);
  var
    st, page: string;
  begin
    if (aRec is TGEDCOMIndividualRecord)
    then st := GetNameStr(aRec as TGEDCOMIndividualRecord)
    else
    if (aRec is TGEDCOMMultimediaRecord)
    then st := 'Медиа-объект: ' + TGEDCOMMultimediaRecord(aRec).FileReferences[0].Title
    else
    if (aRec is TGEDCOMFamilyRecord)
    then st := 'Семья: ' + GetFamilyStr(aRec as TGEDCOMFamilyRecord)
    else st := aRec.XRef;

    if (aCit.Page <> '')
    then page := aCit.Page+': '
    else page := '';

    {aSummary}link_list.Add('    '+page+HyperLink(aRec.XRef, st));
  end;

  procedure PrepareTag(aRec: TGEDCOMRecord; tag: TGEDCOMTagWithLists);
  var
    i: Integer;
  begin
    for i := 0 to tag.SourceCitationsCount - 1 do begin
      if (tag.SourceCitations[i].Value = aSourceRec)
      then OutLink(aRec, tag.SourceCitations[i]);
    end;
  end;

  procedure PreparePerson(iRec: TGEDCOMIndividualRecord);
  var
    i: Integer;
  begin
    for i := 0 to iRec.IndividualEventsCount - 1 do
      PrepareTag(iRec, iRec.IndividualEvents[i].Detail);

    for i := 0 to iRec.IndividualAttributesCount - 1 do
      PrepareTag(iRec, iRec.IndividualAttributes[i].Detail);
  end;

  procedure PrepareFamily(fRec: TGEDCOMFamilyRecord);
  var
    i: Integer;
  begin
    for i := 0 to fRec.FamilyEventCount - 1 do
      PrepareTag(fRec, fRec.FamilyEvents[i].Detail);
  end;

var
  i, k: Integer;
  rec: TGEDCOMRecord;
  rep: TGEDCOMRepositoryRecord;
begin
  NavAdd(aSourceRec);

  aSummary.BeginUpdate();
  link_list := TStringList.Create;
  try
    aSummary.Clear();

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
    for i := 0 to FTree.Count - 1 do begin
      rec := FTree.Records[i];

      for k := 0 to rec.SourceCitationsCount - 1 do
        if (rec.SourceCitations[k].Value = aSourceRec)
        then OutLink(rec, rec.SourceCitations[k]);

      if (rec is TGEDCOMIndividualRecord)
      then PreparePerson(rec as TGEDCOMIndividualRecord)
      else
      if (rec is TGEDCOMFamilyRecord)
      then PrepareFamily(rec as TGEDCOMFamilyRecord);
    end;

    link_list.Sort();
    for i := 0 to link_list.Count - 1 do aSummary.Add(link_list[i]);
  finally
    link_list.Free;
    aSummary.EndUpdate();
  end;
end;

procedure TfmBase.ShowFamilyInfo(aFamily: TGEDCOMFamilyRecord; aSummary: TStrings);
var
  irec: TGEDCOMIndividualRecord;
  k: Integer;
  st: string;
begin
  try
    NavAdd(aFamily);

    aSummary.BeginUpdate();
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

    aSummary.EndUpdate();
  except
    on E: Exception do LogWrite('FamilyRefresh(): ' + E.Message);
  end;
end;

procedure TfmBase.mPersonSummaryLink(Sender: TObject; LinkName: String);
begin
  SelectRecordByXRef(LinkName);
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

procedure TfmBase.ShowGroupInfo(aGroup: TGEDCOMGroupRecord; aSummary: TStrings);
var
  i: Integer;
  member: TGEDCOMIndividualRecord;
  mbrList: TStringList;
begin
  NavAdd(aGroup);

  mGroupSummary.Lines.BeginUpdate;
  mGroupSummary.Lines.Clear;
  mGroupSummary.Lines.Add('');
  mGroupSummary.Lines.Add('~bu~' + 'Группа: ' + aGroup.Name + '~ub~');
  mGroupSummary.Lines.Add('');
  mGroupSummary.Lines.Add('Участники группы:');

  mbrList := TStringList.Create;
  try
    for i := 0 to aGroup.MembersCount - 1 do begin
      member := TGEDCOMIndividualRecord(aGroup.Members[i].Value);
      mbrList.AddObject(GetNameStr(member), member);
    end;

    mbrList.Sort;

    for i := 0 to mbrList.Count - 1 do begin
      member := TGEDCOMIndividualRecord(mbrList.Objects[i]);
      mGroupSummary.Lines.Add('    '+HyperLink(member.XRef, mbrList[i]));
    end;
  finally
    mbrList.Free;
  end;

  mGroupSummary.Lines.EndUpdate;
end;

procedure TfmBase.FileNew();
begin
  Clear();
  ListsRefresh();
  ShowPersonInfo(nil, mPersonSummary.Lines);
  FileName := 'Неизвестный';
  Modified := False;
end;

procedure TfmBase.FileLoad(aFileName: string);
begin
  {$IFDEF PROFILER}Profiler.Mark(1, True);{$ENDIF}

  FTree.LoadFromFile(aFileName);

  {$IFDEF PROFILER}Profiler.Mark(1, False);{$ENDIF}

  CheckGEDCOMFormat(FTree);

  FileName := aFileName;
  Modified := False;

  fmGEDKeeper.NamesTable.ImportNames(FTree);

  fmGEDKeeper.AddMRU(aFileName);

  ListsRefresh();
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

procedure TfmBase.RecordAdd();
var
  iRec: TGEDCOMIndividualRecord;
  nRec: TGEDCOMNoteRecord;
  mmRec: TGEDCOMMultimediaRecord;
  sRec: TGEDCOMSourceRecord;
  groupRec: TGEDCOMGroupRecord;
  famRec: TGEDCOMFamilyRecord;
  repRec: TGEDCOMRepositoryRecord;
begin
  case PageRecords.TabIndex of
    0: begin // персоны
      iRec := CreatePersonDialog(nil, tmAncestor, svNone);
      if (iRec <> nil) then begin
        ListsRefresh();
        SelectRecordByXRef(iRec.XRef);
      end;
    end;

    1: begin // семьи
      famRec := nil;
      ModifyFamily(famRec);
      if (famRec <> nil) then ListsRefresh();
    end;

    2: begin // заметки
      nRec := nil;
      ModifyNote(nRec);
      if (nRec <> nil) then ListsRefresh();
    end;

    3: begin // мультимедиа
      mmRec := nil;
      ModifyMedia(mmRec);
      if (mmRec <> nil) then ListsRefresh();
    end;

    4: begin // источники
      sRec := nil;
      ModifySource(sRec);
      if (sRec <> nil) then ListsRefresh();
    end;

    5: begin // архивы
      repRec := nil;
      ModifyRepository(repRec);
      if (repRec <> nil) then ListsRefresh();
    end;

    6: begin // группы
      groupRec := nil;
      ModifyGroup(groupRec);  
      if (groupRec <> nil) then ListsRefresh();
    end;
  end;
end;

procedure TfmBase.RecordEdit(Sender: TObject);
var
  iRec: TGEDCOMIndividualRecord;
  nRec: TGEDCOMNoteRecord;
  mmRec: TGEDCOMMultimediaRecord;
  srcRec: TGEDCOMSourceRecord;
  family: TGEDCOMFamilyRecord;
  groupRec: TGEDCOMGroupRecord;
  repRec: TGEDCOMRepositoryRecord;
begin
  case PageRecords.TabIndex of
    0: begin // персоны
      iRec := GetSelectedPerson();
      if (iRec <> nil) then begin
        ModifyPerson(iRec);
        ListsRefresh();
      end;
    end;

    1: begin // семьи
      family := GetSelectedRecord(ListFamilies) as TGEDCOMFamilyRecord;
      if (family <> nil) then begin
        ModifyFamily(family);
        ListsRefresh();
      end;
    end;

    2: begin // заметки
      nRec := GetSelectedRecord(ListNotes) as TGEDCOMNoteRecord;
      if (nRec <> nil) then begin
        ModifyNote(nRec);
        ListsRefresh();
      end;
    end;

    3: begin // мультимедиа
      mmRec := GetSelectedRecord(ListMultimedia) as TGEDCOMMultimediaRecord;
      if (mmRec <> nil) then begin
        ModifyMedia(mmRec);
        ListsRefresh();
      end;
    end;

    4: begin // источники
      srcRec := GetSelectedRecord(ListSources) as TGEDCOMSourceRecord;
      if (srcRec <> nil) then begin
        ModifySource(srcRec);
        ListsRefresh();
      end;
    end;

    5: begin // архивы
      repRec := GetSelectedRecord(ListRepositories) as TGEDCOMRepositoryRecord;
      if (repRec <> nil) then begin
        ModifyRepository(repRec);
        ListsRefresh();
      end;
    end;

    6: begin // группы
      groupRec := GetSelectedRecord(ListGroups) as TGEDCOMGroupRecord;
      if (groupRec <> nil) then begin
        ModifyGroup(groupRec);
        ListsRefresh();
      end;
    end;
  end;
end;

procedure TfmBase.RecordDelete();
var
  iRec: TGEDCOMIndividualRecord;
  nRec: TGEDCOMNoteRecord;

  procedure DeleteMediaRec();
  var
    mRec: TGEDCOMMultimediaRecord;
    i, k: Integer;
    rec: TGEDCOMRecord;
  begin
    mRec := GetSelectedRecord(ListMultimedia) as TGEDCOMMultimediaRecord;
    if (mRec = nil) then Exit;

    if (MessageDlg('Удалить мультимедиа "'+mRec.StringValue+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrYes)
    then begin
      for i := 0 to FTree.Count - 1 do begin
        rec := FTree.Records[i];

        for k := rec.MultimediaLinksCount - 1 downto 0 do begin
          if (rec.MultimediaLinks[k].Value = mRec)
          then rec.DeleteMultimediaLink(k);
        end;
      end;

      FTree.Delete(FTree.IndexOfRecord(mRec));

      Modified := True;

      ListsRefresh();
    end;
  end;

  procedure DeleteSourceRec();
  var
    sRec: TGEDCOMSourceRecord;
    i, k: Integer;
    rec: TGEDCOMRecord;
  begin
    sRec := GetSelectedRecord(ListSources) as TGEDCOMSourceRecord;
    if (sRec = nil) then Exit;

    if (MessageDlg('Удалить источник "'+sRec.FiledByEntry+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrYes)
    then begin
      for i := 0 to FTree.Count - 1 do begin
        rec := FTree.Records[i];

        for k := rec.SourceCitationsCount - 1 downto 0 do begin
          if (rec.SourceCitations[k].Value = sRec)
          then rec.DeleteSourceCitation(k);
        end;
      end;

      FTree.Delete(FTree.IndexOfRecord(sRec));

      Modified := True;

      ListsRefresh();
    end;
  end;

  procedure DeleteRepositoryRec();
  var
    repRec: TGEDCOMRepositoryRecord;
    i, k: Integer;
    rec: TGEDCOMRecord;
    srcRec: TGEDCOMSourceRecord;
  begin
    repRec := GetSelectedRecord(ListRepositories) as TGEDCOMRepositoryRecord;
    if (repRec = nil) then Exit;

    if (MessageDlg('Удалить архив "'+repRec.RepositoryName+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrYes)
    then begin
      for i := 0 to FTree.Count - 1 do begin
        rec := FTree.Records[i];

        if (rec is TGEDCOMSourceRecord) then begin
          srcRec := (rec as TGEDCOMSourceRecord);

          for k := srcRec.RepositoryCitationsCount - 1 downto 0 do begin
            if (srcRec.RepositoryCitations[k].Value = repRec)
            then srcRec.RemoveRepositoryCitation(srcRec.RepositoryCitations[k]);
          end;
        end;
      end;

      FTree.Delete(FTree.IndexOfRecord(repRec));

      Modified := True;

      ListsRefresh();
    end;
  end;

  procedure DeleteGroupRec();
  var
    groupRec: TGEDCOMGroupRecord;
    i: Integer;
    member: TGEDCOMIndividualRecord;
  begin
    groupRec := GetSelectedRecord(ListGroups) as TGEDCOMGroupRecord;
    if (groupRec = nil) then Exit;

    if (MessageDlg('Удалить группу "'+groupRec.Name+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrYes)
    then begin
      for i := 0 to groupRec.MembersCount - 1 do begin
        member := TGEDCOMIndividualRecord(groupRec.Members[i].Value);
        member.DeleteGroup(member.IndexOfGroup(groupRec));
      end;

      FTree.Delete(FTree.IndexOfRecord(groupRec));

      Modified := True;

      ListsRefresh();
    end;
  end;

begin
  case PageRecords.TabIndex of
    0: begin // персоны
      iRec := GetSelectedPerson();
      if (iRec = nil) then Exit;

      if (MessageDlg('Удалить персональную запись "'+GetNameStr(iRec)+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrYes)
      then begin
        DeleteIndividualRecord(iRec);
        ListsRefresh();
      end;
    end;

    1: begin // семьи
      DeleteFamily(GetSelectedRecord(ListFamilies) as TGEDCOMFamilyRecord);
      ListsRefresh();
    end;

    2: begin // заметки
      nRec := GetSelectedRecord(ListNotes) as TGEDCOMNoteRecord;
      if (nRec = nil) then Exit;

      if (MessageDlg('Удалить заметку?', mtConfirmation, [mbNo, mbYes], 0) = mrYes)
      then begin
        DeleteNoteRecord(nRec);
        ListsRefresh();
      end;
    end;

    3: begin // мультимедиа
      DeleteMediaRec();
    end;

    4: begin // источники
      DeleteSourceRec();
    end;

    5: begin // архивы
      DeleteRepositoryRec();
    end;

    6: begin // группы
      DeleteGroupRec();
    end;
  end;
end;

procedure TfmBase.ExportToExcel();
var
  ex_exp: TExcelExporter;
begin
  ex_exp := TExcelExporter.Create(FTree, GetCurFileTempPath());
  try
    ex_exp.Generate();
  finally
    ex_exp.Destroy;
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
    Result := fmFileProps.ShowModal();
  finally
    fmFileProps.Destroy;
  end;
end;

procedure TfmBase.ShowStats();
var
  fmStats: TfmStats;
begin
  fmStats := TfmStats.Create(Self);
  fmStats.Base := Self;
  fmStats.Show;
end;

procedure TfmBase.SetFilter();
var
  fmFilter: TfmFilter;
begin
  fmFilter := TfmFilter.Create(Self);
  try
    fmFilter.ShowModal;
  finally
    fmFilter.Destroy;
  end;
end;

procedure TfmBase.TreeTools();
var
  fmTreeTools: TfmTreeTools;
begin
  fmTreeTools := TfmTreeTools.Create(Self);
  try
    fmTreeTools.ShowModal();
  finally
    fmTreeTools.Destroy;
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
    p.Generate(ExtractFilePath(FFileName), FTree, GetSelectedPerson(), pk_dAboville);
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
    p.Generate(ExtractFilePath(FFileName), FTree, GetSelectedPerson(), pk_Konovalov);
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
    fmPersonScan.ShowModal;
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
  fmMaps := TfmMaps.Create(Application);
  try
    fmMaps.Tree := FTree;
    fmMaps.Show;
  finally
    //fmMaps.Destroy;
  end;
  {$ENDIF}
end;

procedure TfmBase.ChangeRecord(aRecord: TGEDCOMRecord);
begin
  FChangedRecords.Add(aRecord);
end;

procedure TfmBase.ChangesClear();
begin
  FChangedRecords.Clear;
end;

procedure TfmBase.ApplyFilter();
begin
  if (FTree.Count > 0)
  then ListsRefresh();
end;

procedure TfmBase.NavPrev();
var
  rec: TGEDCOMRecord;
begin
  FNavBusy := True;
  try
    Dec(FNavPos);
    rec := TGEDCOMRecord(FNavHistory[FNavPos]);
    SelectRecordByXRef(rec.XRef);

    NavUpdate();
  finally
    FNavBusy := False;
  end;
end;

procedure TfmBase.NavNext();
var
  rec: TGEDCOMRecord;
begin
  FNavBusy := True;
  try
    Inc(FNavPos);
    rec := TGEDCOMRecord(FNavHistory[FNavPos]);
    SelectRecordByXRef(rec.XRef);

    NavUpdate();
  finally
    FNavBusy := False;
  end;
end;

procedure TfmBase.NavClear();
begin
  FNavHistory.Clear();
  FNavPos := -1;
end;

procedure TfmBase.NavAdd(aRec: TGEDCOMRecord);
begin
  if (aRec <> nil) and not(FNavBusy) then begin
    FNavPos := FNavHistory.Add(aRec);
    NavUpdate();
  end;
end;

procedure TfmBase.NavUpdate();
begin
  fmGEDKeeper.actPrev.Enabled := (FNavPos > 0);
  fmGEDKeeper.actNext.Enabled := (FNavPos < FNavHistory.Count - 1);
end;

procedure TfmBase.actTestExecute(Sender: TObject);
var
  i: Integer;
  rec, frec: TGEDCOMRecord;
  ht: TBSHashTable;
  entry: TBSHashEntry;
begin
  for i := 0 to FTree.Count - 1 do begin
    rec := FTree.Records[i];

    {$IFDEF PROFILER}Profiler.Mark(5, True);{$ENDIF}
    frec := FTree.XRefIndex_Find(rec.XRef);
    {$IFDEF PROFILER}Profiler.Mark(5, False);{$ENDIF}

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

      {$IFDEF PROFILER}Profiler.Mark(6, True);{$ENDIF}
      entry := ht.FindEntry(rec.XRef);
      {$IFDEF PROFILER}Profiler.Mark(6, False);{$ENDIF}

      Hole(entry);
    end;
  finally
    ht.Destroy;
  end;
end;

procedure TfmBase.ShowRepositoryInfo(aRepositoryRec: TGEDCOMRepositoryRecord; aSummary: TStrings);

  procedure OutLink(aRec: TGEDCOMRecord; aCit: TGEDCOMRepositoryCitation);
  var
    st: string;
  begin
    if (aRec is TGEDCOMSourceRecord)
    then st := 'Источник: ' + (aRec as TGEDCOMSourceRecord).FiledByEntry
    else st := aRec.XRef;

    aSummary.Add('    '+HyperLink(aRec.XRef, st));
  end;

var
  i, k: Integer;
  rec: TGEDCOMRecord;
  srcRec: TGEDCOMSourceRecord;
begin
  NavAdd(aRepositoryRec);

  aSummary.Clear;

  aSummary.Add('');
  aSummary.Add('Название: "' + Trim(aRepositoryRec.RepositoryName) + '"');

  aSummary.Add('');
  aSummary.Add('Ссылки:');
  for i := 0 to FTree.Count - 1 do begin
    rec := FTree.Records[i];

    if (rec is TGEDCOMSourceRecord) then begin
      srcRec := (rec as TGEDCOMSourceRecord);

      for k := 0 to srcRec.RepositoryCitationsCount - 1 do
        if (srcRec.RepositoryCitations[k].Value = aRepositoryRec)
        then OutLink(srcRec, srcRec.RepositoryCitations[k]);
    end;
  end;
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
end;

procedure TfmBase.FormDeactivate(Sender: TObject);
begin
  fmGEDKeeper.UpdateControls();
end;

function TfmBase.SelectPerson(aTarget: TGEDCOMIndividualRecord; aTargetMode: TTargetMode;
  aNeedSex: TGEDCOMSex): TGEDCOMIndividualRecord;
var
  fmRecordSelect: TfmRecordSelect;
begin
  fmRecordSelect := TfmRecordSelect.Create(Self);

  try
    Result := nil;

    fmRecordSelect.FMode := smPerson;
    fmRecordSelect.FTarget := aTarget;
    fmRecordSelect.FNeedSex := aNeedSex;
    fmRecordSelect.FTargetMode := aTargetMode;

    FFilter.Backup();
    FFilter.Clear();
    FFilter.Sex := fmRecordSelect.FNeedSex;

    ComListPersonsRefresh(fmRecordSelect.ListRecords, True);

    FFilter.Restore();

    case fmRecordSelect.ShowModal() of
      mrOk: Result := TGEDCOMIndividualRecord(fmRecordSelect.ResultRecord);
      mrCancel: ;
    end;
  finally
    fmRecordSelect.Destroy;
  end;
end;

function TfmBase.SelectRecord(aMode: TSelectMode): TGEDCOMRecord;
var
  fmRecordSelect: TfmRecordSelect;
begin
  fmRecordSelect := TfmRecordSelect.Create(Self);

  try
    Result := nil;

    fmRecordSelect.FMode := aMode;
    case aMode of
      smPerson: begin
        FFilter.Backup();
        FFilter.Clear();

        ComListPersonsRefresh(fmRecordSelect.ListRecords, True);

        FFilter.Restore();
      end;
      smNote: ComListNotesRefresh(fmRecordSelect.ListRecords, True);
      smMultimedia: ComListMultimediaRefresh(fmRecordSelect.ListRecords, True);
      smSource: ComListSourcesRefresh(fmRecordSelect.ListRecords, True);
      smRepository: ComListRepositoriesRefresh(fmRecordSelect.ListRecords, True);
      smGroup: ComListGroupsRefresh(fmRecordSelect.ListRecords, True);
    end;

    case fmRecordSelect.ShowModal() of
      mrOk: Result := fmRecordSelect.ResultRecord;
      mrCancel: ;
    end;
  finally
    fmRecordSelect.Destroy;
  end;
end;

function TfmBase.GetChangeDate(aRec: TGEDCOMRecord): string;
begin
  try
    if (aRec.ChangeDate.ChangeDateTime <> 0)
    then Result := DateTimeToStr(aRec.ChangeDate.ChangeDateTime)
    else Result := '';
  except
    Result := '';
  end;
end;

procedure {TfmBase.}CreateListSheet(aSheet: TTabSheet; var aList: TBSListView);
begin
end;

{ TSheetList }

constructor TSheetList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);

  HandleNeeded;
  Align := alClient;

  if (AOwner is TTabSheet)
  then TTabSheet(AOwner).OnShow := SheetShow;

  FActionAdd := TAction.Create(Self);
  with FActionAdd do begin
    Name := 'actRecAdd';
    Category := 'Tools';
    Caption := 'Add';
    Hint := 'Добавить запись';
    ImageIndex := 3;
    ShortCut := TextToShortCut('Ctrl+I');
    OnExecute := ButtonClick;
  end;

  FActionEdit := TAction.Create(Self);
  with FActionEdit do begin
    Name := 'actRecEdit';
    Category := 'Tools';
    Caption := 'Edit';
    Hint := 'Изменить запись';
    ImageIndex := 4;
    ShortCut := TextToShortCut('Ctrl+Enter');
    OnExecute := ButtonClick;
  end;

  FActionDelete := TAction.Create(Self);
  with FActionDelete do begin
    Name := 'actRecDelete';
    Category := 'Tools';
    Caption := 'Delete';
    Hint := 'Удалить запись';
    ImageIndex := 5;
    ShortCut := TextToShortCut('Ctrl+D');
    OnExecute := ButtonClick;
  end;

  FActionJump := TAction.Create(Self);
  with FActionJump do begin
    Name := 'actLinkJump';
    Category := 'Tools';
    Caption := 'LinkJump';
    Hint := 'Перейти на запись';
    ImageIndex := 28;
    OnExecute := ButtonClick;
  end;

  FActionMoveUp := TAction.Create(Self);
  with FActionMoveUp do begin
    Name := 'actMoveUp';
    Category := 'Tools';
    Caption := 'MoveUp';
    Hint := 'Поместить выше';
    ImageIndex := 29;
    OnExecute := ButtonClick;
  end;

  FActionMoveDown := TAction.Create(Self);
  with FActionMoveDown do begin
    Name := 'actMoveDown';
    Category := 'Tools';
    Caption := 'MoveDown';
    Hint := 'Поместить ниже';
    ImageIndex := 30;
    OnExecute := ButtonClick;
  end;

  //

  FToolBar := TToolBar.Create(Self);
  FToolBar.Parent := Self;
  FToolBar.BorderWidth := 1;
  FToolBar.ButtonHeight := 28;
  FToolBar.ButtonWidth := 30;
  FToolBar.EdgeBorders := [];
  FToolBar.Flat := True;
  FToolBar.Images := fmGEDKeeper.ImageList1;
  FToolBar.ShowHint := True;
  FToolBar.Align := alTop;
  FToolBar.AutoSize := True;

  FBtnMoveDown := TToolButton.Create(Self);
  FBtnMoveDown.Parent := FToolBar;
  FBtnMoveDown.Wrap := True;
  FBtnMoveDown.Action := FActionMoveDown;

  FBtnMoveUp := TToolButton.Create(Self);
  FBtnMoveUp.Parent := FToolBar;
  FBtnMoveUp.Wrap := True;
  FBtnMoveUp.Action := FActionMoveUp;

  FBtnLinkJump := TToolButton.Create(Self);
  FBtnLinkJump.Parent := FToolBar;
  FBtnLinkJump.Wrap := True;
  FBtnLinkJump.Action := FActionJump;

  FBtnDelete := TToolButton.Create(Self);
  FBtnDelete.Parent := FToolBar;
  FBtnDelete.Wrap := True;
  FBtnDelete.Action := FActionDelete;

  FBtnEdit := TToolButton.Create(Self);
  FBtnEdit.Parent := FToolBar;
  FBtnEdit.Wrap := True;
  FBtnEdit.Action := FActionEdit;

  FBtnAdd := TToolButton.Create(Self);
  FBtnAdd.Parent := FToolBar;
  FBtnAdd.Wrap := True;
  FBtnAdd.Action := FActionAdd;

  FList := TBSListView.Create(Self);
  with FList do begin
    Parent := Self;
    Align := alClient;
    HideSelection := False;
    ReadOnly := True;
    RowSelect := True;
    SortType := stText;
    ViewStyle := vsReport;
    SortColumn := 0;
    SortDirection := sdAscending;
    ShowSortSign := True;

    OnDblClick := ListDblClick;
    OnKeyDown := ListKeyDown;
  end;

  FToolBar.Align := alRight;
  FToolBar.List := True;

  SetButtons([lbAdd..lbDelete]);
end;

destructor TSheetList.Destroy;
begin
  FList.Free;

  FBtnDelete.Free;
  FBtnEdit.Free;
  FBtnAdd.Free;

  FToolBar.Free;

  inherited Destroy;
end;

procedure TSheetList.ButtonClick(Sender: TObject);
begin
  if (Sender = FActionAdd) then ItemAdd()
  else
  if (Sender = FActionEdit) then ItemEdit()
  else
  if (Sender = FActionDelete) then ItemDelete()
  else
  if (Sender = FActionJump) then ItemJump()
  else
  if (Sender = FActionMoveUp) then ItemMoveUp()
  else
  if (Sender = FActionMoveDown) then ItemMoveDown();
end;

procedure TSheetList.ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) then
    case Key of
      Ord('I'): ItemAdd();
      Ord('D'): ItemDelete();
      VK_RETURN: ItemEdit();
    end;
end;

procedure TSheetList.ItemAdd();
begin
  if Assigned(FOnModify) then FOnModify(Self, -1, raAdd);
end;

procedure TSheetList.ItemEdit();
begin
  if Assigned(FOnModify) then FOnModify(Self, GetSelIndex(FList), raEdit);
end;

procedure TSheetList.ItemDelete();
begin
  if Assigned(FOnModify) then FOnModify(Self, GetSelIndex(FList), raDelete);
end;

procedure TSheetList.ItemJump();
begin
  if Assigned(FOnModify) then FOnModify(Self, GetSelIndex(FList), raJump);
end;

procedure TSheetList.ItemMoveDown();
begin
  if Assigned(FOnModify) then FOnModify(Self, GetSelIndex(FList), raMoveDown);
end;

procedure TSheetList.ItemMoveUp();
begin
  if Assigned(FOnModify) then FOnModify(Self, GetSelIndex(FList), raMoveUp);
end;

procedure TSheetList.ListDblClick(Sender: TObject);
begin
  ItemEdit();
end;

(*procedure TSheetList.SetActionList(const Value: TActionList);
begin
  {if (Value = nil) then begin
    FActionAdd.ActionList := nil;
    FActionEdit.ActionList := nil;
    FActionDelete.ActionList := nil;
  end else begin
    FActionAdd.ActionList := Value;
    FActionEdit.ActionList := Value;
    FActionDelete.ActionList := Value;
  end;}

  FActionList := Value;
end;*)

procedure TSheetList.SheetShow(Sender: TObject);
begin
  FList.SetFocus();
end;

procedure TSheetList.SetButtons(const Value: TListButtons);
begin
  FButtons := Value;

  FActionAdd.Visible := (lbAdd in FButtons);
  FActionDelete.Visible := (lbDelete in FButtons);
  FActionEdit.Visible := (lbEdit in FButtons);
  FActionJump.Visible := (lbJump in FButtons);
  FActionMoveUp.Visible := (lbMoveUp in FButtons);
  FActionMoveDown.Visible := (lbMoveDown in FButtons);
end;

end.
