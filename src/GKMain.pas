unit GKMain;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, ImgList, ToolWin, Buttons, Menus,
  GKCommon, GedCom551, Htmemo, Masks, ActnList, bsCtrls;

type
  TRecCount = record
    Total: Integer;
    Filtered: Integer;
  end;

  TPersonsFilter = class(TObject)
  private
    Back_AliveBeforeDate: string;
    Back_LifeMode: TLifeMode;
    Back_Name: string;
    Back_PatriarchOnly: Boolean;
    Back_Residence: string;
    Back_Sex: TGEDCOMSex;
  public
    AliveBeforeDate: string;
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

  TfmGEDKeeper = class(TForm)
    PageRecords: TPageControl;
    SheetPersons: TTabSheet;
    ListPersons: TBSListView;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ImageList1: TImageList;
    tbFileNew: TToolButton;
    tbFileLoad: TToolButton;
    tbFileSave: TToolButton;
    ToolButton4: TToolButton;
    tbRecordAdd: TToolButton;
    tbRecordEdit: TToolButton;
    tbRecordDelete: TToolButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    MainMenu1: TMainMenu;
    miFile: TMenuItem;
    miEdit: TMenuItem;
    miHelp: TMenuItem;
    miContext: TMenuItem;
    N2: TMenuItem;
    miAbout: TMenuItem;
    miFileNew: TMenuItem;
    miFileLoad: TMenuItem;
    miFileSave: TMenuItem;
    N4: TMenuItem;
    miExit: TMenuItem;
    miRecordAdd: TMenuItem;
    miRecordEdit: TMenuItem;
    miRecordDelete: TMenuItem;
    miDocAuthor: TMenuItem;
    miTreeAncestors: TMenuItem;
    miTreeDescendants: TMenuItem;
    miStats: TMenuItem;
    SheetNotes: TTabSheet;
    SheetMultimedia: TTabSheet;
    SheetSources: TTabSheet;
    SheetFamilies: TTabSheet;
    ListSources: TBSListView;
    ListFamilies: TBSListView;
    ListNotes: TBSListView;
    ListMultimedia: TBSListView;
    miTreeTools: TMenuItem;
    miExportToWeb: TMenuItem;
    N7: TMenuItem;
    miOptions: TMenuItem;
    N1: TMenuItem;
    miPedigree: TMenuItem;
    miPedigree_dAboville: TMenuItem;
    miPedigree_Konovalov: TMenuItem;
    miExportToExcel: TMenuItem;
    N10: TMenuItem;
    ScrollBox1: TScrollBox;
    mSummary: THTMemo;
    Splitter2: TSplitter;
    ScrollBox2: TScrollBox;
    mFamilySummary: THTMemo;
    miFilter: TMenuItem;
    Splitter3: TSplitter;
    ScrollBox3: TScrollBox;
    mNoteSummary: THTMemo;
    ScrollBox4: TScrollBox;
    mMediaSummary: THTMemo;
    Splitter4: TSplitter;
    ScrollBox5: TScrollBox;
    mSourceSummary: THTMemo;
    Splitter5: TSplitter;
    SheetGroups: TTabSheet;
    ListGroups: TBSListView;
    Splitter6: TSplitter;
    ScrollBox6: TScrollBox;
    mGroupSummary: THTMemo;
    ActionList1: TActionList;
    actFileNew: TAction;
    actFileLoad: TAction;
    actFileSave: TAction;
    actRecordAdd: TAction;
    actRecordEdit: TAction;
    actRecordDelete: TAction;
    actExportToExcel: TAction;
    actContextHelp: TAction;
    actFileProperties: TAction;
    actStats: TAction;
    actExit: TAction;
    actOptions: TAction;
    actAbout: TAction;
    actFilter: TAction;
    actTreeTools: TAction;
    actExportToWeb: TAction;
    actTreeAncestors: TAction;
    actTreeDescendants: TAction;
    ToolButton1: TToolButton;
    tbTreeAncestors: TToolButton;
    tbTreeDescendants: TToolButton;
    actPedigree_dAboville: TAction;
    actPedigree_Konovalov: TAction;
    ToolButton2: TToolButton;
    tbPedigree: TToolButton;
    MenuPedigree: TPopupMenu;
    miPedigree_dAboville2: TMenuItem;
    miPedigree_Konovalov2: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    ToolButton3: TToolButton;
    tbFilter: TToolButton;
    ToolButton6: TToolButton;
    tbStats: TToolButton;
    N8: TMenuItem;
    miPersonScan: TMenuItem;
    StatusBar1: TStatusBar;
    N11: TMenuItem;
    N9: TMenuItem;
    miLifeGame: TMenuItem;
    actLifeGame: TAction;
    miMap: TMenuItem;
    actMap: TAction;
    miGenResources: TMenuItem;
    actGenResources: TAction;
    miKinshipTerms: TMenuItem;
    actKinshipTerms: TAction;
    N3: TMenuItem;
    N12: TMenuItem;
    ToolButton5: TToolButton;
    tbPrev: TToolButton;
    tbNext: TToolButton;
    actPrev: TAction;
    actNext: TAction;
    miMRUFiles: TMenuItem;
    MenuMRU: TPopupMenu;
    MenuLists: TPopupMenu;
    miPersonNameCopy: TMenuItem;
    actPersonNameCopy: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListPersonsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PageRecordsChange(Sender: TObject);
    procedure ListNotesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListMultimediaSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListSourcesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListFamiliesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure mSummaryLink(Sender: TObject; LinkName: String);
    procedure ListGroupsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileLoadExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actRecordAddExecute(Sender: TObject);
    procedure actRecordEditExecute(Sender: TObject);
    procedure actRecordDeleteExecute(Sender: TObject);
    procedure actExportToExcelExecute(Sender: TObject);
    procedure actContextHelpExecute(Sender: TObject);
    procedure actFilePropertiesExecute(Sender: TObject);
    procedure actStatsExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actFilterExecute(Sender: TObject);
    procedure actTreeToolsExecute(Sender: TObject);
    procedure ListPersonsDblClick(Sender: TObject);
    procedure ListFamiliesDblClick(Sender: TObject);
    procedure ListNotesDblClick(Sender: TObject);
    procedure ListMultimediaDblClick(Sender: TObject);
    procedure ListSourcesDblClick(Sender: TObject);
    procedure ListGroupsDblClick(Sender: TObject);
    procedure actExportToWebExecute(Sender: TObject);
    procedure actTreeAncestorsExecute(Sender: TObject);
    procedure actTreeDescendantsExecute(Sender: TObject);
    procedure actPedigree_dAbovilleExecute(Sender: TObject);
    procedure actPedigree_KonovalovExecute(Sender: TObject);
    procedure miPersonScanClick(Sender: TObject);
    procedure actLifeGameExecute(Sender: TObject);
    procedure actMapExecute(Sender: TObject);
    procedure actGenResourcesExecute(Sender: TObject);
    procedure actKinshipTermsExecute(Sender: TObject);
    procedure actPrevExecute(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure actPersonNameCopyExecute(Sender: TObject);
  private
    FChangedRecords: TList;
    FCounts: array [TGEDCOMRecordType] of TRecCount;
    FFileName: string;
    FFilter: TPersonsFilter;
    FModified: Boolean;
    FOptions: TGlobalOptions;

    FNavBusy: Boolean;
    FNavHistory: TList;
    FNavPos: Integer;

    function CheckModified: Boolean;
    procedure CleanFamily(aFamily: TGEDCOMFamilyRecord);

    procedure LoadFromFile(aFileName: string);
    procedure SaveToFile(const aFileName: string);

    procedure SetFileName(const Value: string);
    procedure SetMainTitle();
    procedure SetModified(const Value: Boolean);
    procedure ShowAddress(anAddress: TGEDCOMAddress; aSummary: TStrings);
    procedure NavAdd(aRec: TGEDCOMRecord);

    procedure AddMRU(const aFileName: string);
    procedure UpdateMRU();
    procedure MRUFileClick(Sender: TObject);
  public
    FNamesTable: TNamesTable;
    FTree: TGEDCOMTree;

    procedure ApplyFilter();

    procedure CalcCounts();
    procedure ChangeRecord(aRecord: TGEDCOMRecord);
    procedure ChangesClear();
    procedure Clear();
    procedure ComListFamiliesRefresh(aList: TBSListView);
    procedure ComListGroupsRefresh(aList: TBSListView; aTitles: Boolean);
    procedure ComListPersonsRefresh(aList: TBSListView; aTitles: Boolean);
    procedure ComListMultimediaRefresh(aList: TBSListView);
    procedure ComListNotesRefresh(aList: TBSListView; aTitles: Boolean);
    procedure ComListSourcesRefresh(aList: TBSListView; aTitles: Boolean);

    function CreatePerson(aName, aPatronymic, aFamily: string;
      aSex: TGEDCOMSex; aBirthEvent: Boolean = False): TGEDCOMIndividualRecord;
    function CreatePersonDialog(aTarget: TGEDCOMIndividualRecord;
      aTargetMode: TTargetMode; aNeedSex: TGEDCOMSex): TGEDCOMIndividualRecord;

    procedure DeleteFamily(aFamily: TGEDCOMFamilyRecord);
    procedure DeleteIndividualRecord(iRec: TGEDCOMIndividualRecord);
    procedure DeleteNoteRecord(nRec: TGEDCOMNoteRecord);

    function  GetCurFileTempPath(): string;

    function  GetChildFamily(iRec: TGEDCOMIndividualRecord; aCanCreate: Boolean;
      aNewParent: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;

    function  GetSelectedRecord(aList: TBSListView): TGEDCOMRecord;

    function  GetSelectedFamily(): TGEDCOMFamilyRecord;
    function  GetSelectedMultimedia(): TGEDCOMMultimediaRecord;
    function  GetSelectedNote(): TGEDCOMNoteRecord;
    function  GetSelectedPerson(): TGEDCOMIndividualRecord;
    function  GetSelectedSource(): TGEDCOMSourceRecord;
    function  GetSelectedGroup(): TGEDCOMGroupRecord;

    procedure ListsRefresh(aTitles: Boolean = False);

    //function ModifyPerson(var aIndivRec: TGEDCOMIndividualRecord): Boolean;
    function ModifyFamily(var aFamilyRec: TGEDCOMFamilyRecord; aSpouse: TGEDCOMIndividualRecord = nil): Boolean;
    function ModifyNote(var aNoteRec: TGEDCOMNoteRecord): Boolean;
    function ModifySource(var aSourceRec: TGEDCOMSourceRecord): Boolean;
    function ModifyGroup(var aGroupRec: TGEDCOMGroupRecord): Boolean;

    function ModifyRecAssociation(aRecord: TGEDCOMIndividualRecord; aIndex: Integer; anAction: TRecAction): Boolean;
    function ModifyRecEvent(aRecord: TGEDCOMRecordWithLists; aEvent: TGEDCOMCustomEvent; anAction: TRecAction): Boolean;
    function ModifyRecMultimedia(aRecord: TGEDCOMRecordWithLists; aIndex: Integer; anAction: TRecAction): Boolean;
    function ModifyRecNote(aRecord: TGEDCOMRecordWithLists; aIndex: Integer; anAction: TRecAction): Boolean;
    function ModifyRecSource(aRecord: TGEDCOMRecordWithLists; aIndex: Integer; anAction: TRecAction): Boolean;

    procedure NavClear();
    procedure NavUpdate();

    procedure RecListAssociationsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListFamilyEventsRefresh(aRecord: TGEDCOMFamilyRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListGroupsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListIndividualEventsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListMediaRefresh(aRecord: TGEDCOMRecordWithLists; aList: TListBox; aSummary: TStrings);
    procedure RecListNotesRefresh(aRecord: TGEDCOMRecordWithLists; aList: TListBox; aSummary: TStrings);
    procedure RecListSourcesRefresh(aRecord: TGEDCOMRecordWithLists; aList: TBSListView; aSummary: TStrings);

    procedure SelectRecordByXRef(XRef: string);
    procedure ShowDetailInfo(aDetail: TGEDCOMEventDetail; aSummary: TStrings);
    procedure ShowFamilyInfo(aFamily: TGEDCOMFamilyRecord; aSummary: TStrings);
    procedure ShowNoteInfo(aNoteRec: TGEDCOMNoteRecord; aSummary: TStrings);
    procedure ShowPersonInfo(iRec: TGEDCOMIndividualRecord; aSummary: TStrings);
    procedure ShowSourceInfo(aSourceRec: TGEDCOMSourceRecord; aSummary: TStrings);

    property FileName: string read FFileName write SetFileName;
    property Filter: TPersonsFilter read FFilter;
    property Modified: Boolean read FModified write SetModified;
    property Options: TGlobalOptions read FOptions;
  end;

var
  fmGEDKeeper: TfmGEDKeeper;

implementation

uses
  Types, IniFiles, bsComUtils, GKPersonNew, GKRecordSelect, GKStats, GKNoteEdit,
  GKChart, GKSourceEdit, GKEventEdit, GKAbout, GKChartCore, GKFileProperties,
  GKMaps, GKPersonEdit, GKExport, GKOptions, GKFamilyEdit, GKAssociationEdit,
  GKFilter, GKTreeTools, GKGroupEdit, GKPersonScan, GKLifeMain, GKProgress
  {$IFDEF PROFILER}, ZProfiler{$ENDIF}, GKSourceCitEdit, Clipbrd;

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
  Clear();
end;

procedure TPersonsFilter.Backup();
begin
  Back_AliveBeforeDate := AliveBeforeDate;
  Back_LifeMode := LifeMode;
  Back_Name := Name;
  Back_PatriarchOnly := PatriarchOnly;
  Back_Residence := Residence;
  Back_Sex := Sex;
end;

procedure TPersonsFilter.Restore();
begin
  AliveBeforeDate := Back_AliveBeforeDate;
  LifeMode := Back_LifeMode;
  Name := Back_Name;
  PatriarchOnly := Back_PatriarchOnly;
  Residence := Back_Residence;
  Sex := Back_Sex;
end;

procedure TPersonsFilter.Clear();
begin
  LifeMode := lmAll;
  Name := '*';
  AliveBeforeDate := '';
  PatriarchOnly := False;
  Residence := '*';
  Sex := svNone;
end;

{ TfmGEDKeeper }

procedure TfmGEDKeeper.FormCreate(Sender: TObject);
begin
  LogInit(GetAppPath() + 'GEDKeeper.log');

  {$IFDEF PROFILER}InitProfiler();{$ENDIF}

  LongDateFormat := 'DD MMM YYYY';

  FNavHistory := TList.Create;
  FChangedRecords := TList.Create;
  FOptions := TGlobalOptions.Create;
  FNamesTable := TNamesTable.Create;
  FTree := TGEDCOMTree.Create;
  FFilter := TPersonsFilter.Create;

  FNamesTable.LoadFromFile(GetAppPath() + 'GEDKeeper.nms');
  FOptions.LoadFromFile(GetAppPath() + 'GEDKeeper.ini');

  UpdateMRU();
  ListsRefresh(True);

  if (ParamStr(1) = '')
  then actFileNewExecute(nil)
  else LoadFromFile(ParamStr(1));
end;

procedure TfmGEDKeeper.FormDestroy(Sender: TObject);
begin
  ListPersons.Clear;

  FOptions.SaveToFile(GetAppPath() + 'GEDKeeper.ini');
  FNamesTable.SaveToFile(GetAppPath() + 'GEDKeeper.nms');

  FFilter.Destroy;
  FTree.Destroy;
  FNamesTable.Destroy;
  FOptions.Destroy;
  FChangedRecords.Destroy;
  FNavHistory.Destroy;

  {$IFDEF PROFILER}DoneProfiler();{$ENDIF}

  LogDone();
end;

function TfmGEDKeeper.CheckModified(): Boolean;
begin
  Result := True;

  if Modified then begin
    case MessageDlg('Файл изменен. Сохранить?', mtWarning, [mbYes, mbNo, mbCancel], 0) of
      mrYes: actFileSaveExecute(nil);
      mrNo: {dummy};
      mrCancel: Result := False;
    end;
  end;
end;

procedure TfmGEDKeeper.Clear();
begin
  FTree.Clear();

  NavClear();
end;

procedure TfmGEDKeeper.SaveToFile(const aFileName: string);
var
  fs: TFileStream;
  subm: string;
begin
  subm := FTree.Header.TagStringValue('SUBM');

  FTree.Header.Clear;
  FTree.Header.Source := AppName;
  FTree.Header.ReceivingSystemName := AppName;
  FTree.Header.CharacterSet := FOptions.DefCharacterSet;
  FTree.Header.Language := 'Russian';
  FTree.Header.GEDCOMVersion := '5.5';
  FTree.Header.GEDCOMForm := 'LINEAGE-LINKED';
  FTree.Header.FileName := ExtractFileName(aFileName);
  FTree.Header.TransmissionDate.Date := Now();

  if (subm <> '')
  then FTree.Header.SetTagStringValue('SUBM', subm);

  fs := TFileStream.Create(aFileName, fmCreate);
  try
    FTree.SaveToStream(fs);
    FTree.Header.CharacterSet := csASCII;
  finally
    fs.Destroy;
  end;
end;

procedure TfmGEDKeeper.SelectRecordByXRef(XRef: string);

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
  if (rec is TGEDCOMSourceRecord)
  then SelectItemByRec(ListSources, rec, 4)
  else
  if (rec is TGEDCOMGroupRecord)
  then SelectItemByRec(ListGroups, rec, 5)
  else ;
end;

function TfmGEDKeeper.GetChildFamily(iRec: TGEDCOMIndividualRecord; aCanCreate: Boolean;
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

function TfmGEDKeeper.CreatePerson(aName, aPatronymic, aFamily: string;
  aSex: TGEDCOMSex; aBirthEvent: Boolean = False): TGEDCOMIndividualRecord;
begin
  Result := CreatePersonEx(FTree, aName, aPatronymic, aFamily, aSex, aBirthEvent);
  ChangeRecord(Result);
  Modified := True;
end;

function TfmGEDKeeper.CreatePersonDialog(aTarget: TGEDCOMIndividualRecord;
  aTargetMode: TTargetMode; aNeedSex: TGEDCOMSex): TGEDCOMIndividualRecord;
var
  fmPersonNew: TfmPersonNew;
begin
  Result := nil;

  fmPersonNew := TfmPersonNew.Create(nil);
  try
    fmPersonNew.EditSex.ItemIndex := Ord(aNeedSex);
    fmPersonNew.TargetMode := aTargetMode;
    fmPersonNew.Target := aTarget;

    case fmPersonNew.ShowModal of
      mrOk: begin
        Result := CreatePerson(fmPersonNew.EditName.Text,
          fmPersonNew.EditPatronymic.Text, fmPersonNew.EditFamily.Text,
          TGEDCOMSex(fmPersonNew.EditSex.ItemIndex), True);
      end;

      mrCancel: ;
    end;
  finally
    fmPersonNew.Destroy;
  end;
end;

procedure TfmGEDKeeper.CleanFamily(aFamily: TGEDCOMFamilyRecord);
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

procedure TfmGEDKeeper.DeleteFamily(aFamily: TGEDCOMFamilyRecord);
begin
  if (aFamily = nil) then Exit;

  if MessageDlg('Удалить семью "'+GetFamilyStr(aFamily)+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrNo
  then Exit;

  CleanFamily(aFamily);

  FTree.Delete(FTree.IndexOfRecord(aFamily));
end;

procedure TfmGEDKeeper.DeleteIndividualRecord(iRec: TGEDCOMIndividualRecord);
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

procedure TfmGEDKeeper.DeleteNoteRecord(nRec: TGEDCOMNoteRecord);
var
  i, k: Integer;
  rec: TGEDCOMRecordWithLists;
begin
  for i := 0 to FTree.Count - 1 do begin
    if (FTree.Records[i] is TGEDCOMRecordWithLists) then begin
      rec := FTree.Records[i] as TGEDCOMRecordWithLists;

      for k := rec.NotesCount - 1 downto 0 do begin
        if (rec.Notes[k].Value = nRec)
        then rec.DeleteNotes(k);
      end;
    end;
  end;

  FTree.Delete(FTree.IndexOfRecord(nRec));

  Modified := True;
end;

procedure TfmGEDKeeper.ListPersonsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  person: TGEDCOMIndividualRecord;
begin
  if (Item = nil)
  then ShowPersonInfo(nil, mSummary.Lines)
  else begin
    person := TGEDCOMIndividualRecord(Item.Data);
    if (person = nil) then Exit;

    if not(Selected)
    then ShowPersonInfo(nil, mSummary.Lines)
    else ShowPersonInfo(person, mSummary.Lines);
  end;
end;

procedure TfmGEDKeeper.ShowPersonInfo(iRec: TGEDCOMIndividualRecord; aSummary: TStrings);
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
      aSummary.BeginUpdate;
      aSummary.Clear;
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
      aSummary.EndUpdate;
    end;
  except
    on E: Exception do LogWrite('PersonRefresh(): ' + E.Message);
  end;
end;

procedure TfmGEDKeeper.ShowAddress(anAddress: TGEDCOMAddress; aSummary: TStrings);
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

procedure TfmGEDKeeper.ShowDetailInfo(aDetail: TGEDCOMEventDetail; aSummary: TStrings);
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

procedure TfmGEDKeeper.RecListIndividualEventsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
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
          item.SubItems.Add(GEDCOMCustomDateToStr(event.Detail.Date.Value, FOptions.DefDateFormat));
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
          item.SubItems.Add(GEDCOMCustomDateToStr(attr.Detail.Date.Value, FOptions.DefDateFormat));

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

procedure TfmGEDKeeper.RecListFamilyEventsRefresh(aRecord: TGEDCOMFamilyRecord; aList: TBSListView; aSummary: TStrings);
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
          item.SubItems.Add(GEDCOMCustomDateToStr(event.Detail.Date.Value, FOptions.DefDateFormat));
          item.SubItems.Add(event.Detail.Place);
          item.SubItems.Add(event.Detail.Cause);
          item.Data := TObject(idx);
        end;
      end;
    end;
  except
    on E: Exception do LogWrite('ListFamilyEventsRefresh(): ' + E.Message);
  end;
end;

procedure TfmGEDKeeper.RecListNotesRefresh(aRecord: TGEDCOMRecordWithLists; aList: TListBox; aSummary: TStrings);
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
        then aList.AddItem(st, note);
      end;
    end;
  except
    on E: Exception do LogWrite('ListNotesRefresh(): ' + E.Message);
  end;
end;

procedure TfmGEDKeeper.RecListMediaRefresh(aRecord: TGEDCOMRecordWithLists; aList: TListBox; aSummary: TStrings);
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
          st := mmRec.FileReferences[0].StringValue;

          if (aList = nil) and (aSummary <> nil)
          then aSummary.Add(st);

          if (aList <> nil)
          then aList.AddItem(st, mmLink);
        end;
      end;
    end;
  except
    on E: Exception do LogWrite('ListMediaRefresh(): ' + E.Message);
  end;
end;

procedure TfmGEDKeeper.RecListSourcesRefresh(aRecord: TGEDCOMRecordWithLists; aList: TBSListView; aSummary: TStrings);
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

procedure TfmGEDKeeper.RecListAssociationsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
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

procedure TfmGEDKeeper.RecListGroupsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
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

procedure TfmGEDKeeper.CalcCounts();
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

procedure TfmGEDKeeper.ListsRefresh(aTitles: Boolean = False);
var
  bm: Pointer;

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
  ComListFamiliesRefresh(ListFamilies);
  RestoreBookmark(ListFamilies);

  SaveBookmark(ListNotes);
  ComListNotesRefresh(ListNotes, aTitles);
  RestoreBookmark(ListNotes);

  SaveBookmark(ListMultimedia);
  ComListMultimediaRefresh(ListMultimedia);
  RestoreBookmark(ListMultimedia);

  SaveBookmark(ListSources);
  ComListSourcesRefresh(ListSources, aTitles);
  RestoreBookmark(ListSources);

  SaveBookmark(ListGroups);
  ComListGroupsRefresh(ListGroups, aTitles);
  RestoreBookmark(ListGroups);

  PageRecordsChange(nil);
end;

procedure TfmGEDKeeper.ComListPersonsRefresh(aList: TBSListView; aTitles: Boolean);

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

  procedure PrepareRec(iRec: TGEDCOMIndividualRecord; aItem: TListItem);
  var
    f, n, p, nm{, gcd}: string;
    ev: TGEDCOMCustomEvent;
    bd, dd, fdt: TDateTime;
    res, isLive: Boolean;
    p_tag: TGEDCOMTag;

    bi_date, de_date, bi_place, de_place, resi_place: string;
    i: Integer;
  begin
    nm := GetNameStr(iRec);
    p_tag := iRec.FindTag('_PATRIARCH');

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
        bi_date := GEDCOMCustomDateToStr(ev.Detail.Date.Value, FOptions.DefDateFormat);
        bi_place := ev.Detail.Place;
        bd := GEDCOMDateToDate(ev.Detail.Date.Value);
      end
      else
      if (ev.Name = 'DEAT') then begin
        de_date := GEDCOMCustomDateToStr(ev.Detail.Date.Value, FOptions.DefDateFormat);
        de_place := ev.Detail.Place;
        dd := GEDCOMDateToDate(ev.Detail.Date.Value);
        isLive := False;
      end;
    end;

    resi_place := GetResidencePlace(iRec, FOptions.PlacesWithAddress);
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
      or ((FFilter.Name <> '*') and not(MatchesMask(nm, FFilter.Name)))
      or ((FFilter.Residence <> '*') and not(MatchesMask(resi_place, FFilter.Residence)))
      or ((FFilter.PatriarchOnly and (p_tag = nil)))
      then Exit;
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

    case FOptions.DefNameFormat of
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
    case FOptions.DefNameFormat of
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

procedure TfmGEDKeeper.ComListFamiliesRefresh(aList: TBSListView);
var
  i: Integer;
  item: TListItem;
  famRec: TGEDCOMFamilyRecord;
begin
  aList.Items.BeginUpdate();
  aList.Items.Clear();

  for i := 0 to FTree.Count - 1 do
    if (FTree.Records[i] is TGEDCOMFamilyRecord) then begin
      famRec := FTree.Records[i] as TGEDCOMFamilyRecord;

      item := aList.Items.Add();
      item.Caption := IntToStr(GetId(famRec));
      item.SubItems.Add(GetFamilyStr(famRec));
      item.SubItems.Add(GetMarriageDate(famRec, FOptions.DefDateFormat));

      if (aList = ListFamilies)
      then item.SubItems.Add(GetChangeDate(famRec));

      item.Data := famRec;
    end;

  ResizeColumn(aList, 1);

  aList.Items.EndUpdate();
end;

procedure TfmGEDKeeper.ComListMultimediaRefresh(aList: TBSListView);
var
  i: Integer;
  item: TListItem;
  mmRec: TGEDCOMMultimediaRecord;
  st: string;
begin
  aList.Items.BeginUpdate();
  aList.Items.Clear();

  for i := 0 to FTree.Count - 1 do
    if (FTree.Records[i] is TGEDCOMMultimediaRecord) then begin
      mmRec := FTree.Records[i] as TGEDCOMMultimediaRecord;

      item := aList.Items.Add();
      item.Caption := IntToStr(GetId(mmRec));

      if (mmRec <> nil) and (mmRec.FileReferencesCount <> 0)
      then st := mmRec.FileReferences[0].StringValue
      else st := '';

      item.SubItems.Add(st);

      if (aList = ListMultimedia)
      then item.SubItems.Add(GetChangeDate(mmRec));

      item.Data := mmRec;
    end;

  aList.Items.EndUpdate();
end;

procedure TfmGEDKeeper.ComListNotesRefresh(aList: TBSListView; aTitles: Boolean);
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

procedure TfmGEDKeeper.ComListSourcesRefresh(aList: TBSListView; aTitles: Boolean);
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

procedure TfmGEDKeeper.ComListGroupsRefresh(aList: TBSListView; aTitles: Boolean);
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

function TfmGEDKeeper.GetSelectedRecord(aList: TBSListView): TGEDCOMRecord;
begin
  if (aList.Selected = nil)
  then Result := nil
  else Result := TGEDCOMRecord(aList.Selected.Data);
end;

function TfmGEDKeeper.GetSelectedFamily(): TGEDCOMFamilyRecord;
begin
  Result := nil;
  if (ListFamilies.Selected = nil) then Exit;

  Result := TGEDCOMFamilyRecord(ListFamilies.Selected.Data);
end;

function TfmGEDKeeper.GetSelectedPerson(): TGEDCOMIndividualRecord;
begin
  Result := nil;
  if (ListPersons.Selected = nil) then Exit;

  Result := TGEDCOMIndividualRecord(ListPersons.Selected.Data);
end;

function TfmGEDKeeper.GetSelectedMultimedia(): TGEDCOMMultimediaRecord;
begin
  Result := nil;
  if (ListMultimedia.Selected = nil) then Exit;

  Result := TGEDCOMMultimediaRecord(ListMultimedia.Selected.Data);
end;

function TfmGEDKeeper.GetSelectedNote(): TGEDCOMNoteRecord;
begin
  Result := nil;
  if (ListNotes.Selected = nil) then Exit;

  Result := TGEDCOMNoteRecord(ListNotes.Selected.Data);
end;

function TfmGEDKeeper.GetSelectedSource(): TGEDCOMSourceRecord;
begin
  Result := nil;
  if (ListSources.Selected = nil) then Exit;

  Result := TGEDCOMSourceRecord(ListSources.Selected.Data);
end;

function TfmGEDKeeper.GetSelectedGroup(): TGEDCOMGroupRecord;
begin
  Result := nil;
  if (ListGroups.Selected = nil) then Exit;

  Result := TGEDCOMGroupRecord(ListGroups.Selected.Data);
end;

function TfmGEDKeeper.ModifyRecEvent(aRecord: TGEDCOMRecordWithLists;
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

  fmEventEdit := TfmEventEdit.Create(nil);

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

function TfmGEDKeeper.ModifyFamily(var aFamilyRec: TGEDCOMFamilyRecord; aSpouse: TGEDCOMIndividualRecord = nil): Boolean;
var
  fmFamEdit: TfmFamilyEdit;
  exists: Boolean;
begin
  Result := False;

  if (aSpouse <> nil) and not(aSpouse.Sex in [svMale, svFemale]) then begin
    MessageDlg('У данного человека не задан пол.', mtError, [mbOk], 0);
    Exit;
  end;

  fmFamEdit := TfmFamilyEdit.Create(nil);
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

function TfmGEDKeeper.ModifyNote(var aNoteRec: TGEDCOMNoteRecord): Boolean;
var
  fmNoteEdit: TfmNoteEdit;
  exists: Boolean;
begin
  Result := False;

  fmNoteEdit := TfmNoteEdit.Create(nil);
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

function TfmGEDKeeper.ModifySource(var aSourceRec: TGEDCOMSourceRecord): Boolean;
var
  fmSrcEdit: TfmSourceEdit;
  exists: Boolean;
begin
  Result := False;

  fmSrcEdit := TfmSourceEdit.Create(nil);
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

function TfmGEDKeeper.ModifyGroup(var aGroupRec: TGEDCOMGroupRecord): Boolean;
var
  fmGrpEdit: TfmGroupEdit;
  exists: Boolean;
begin
  Result := False;

  fmGrpEdit := TfmGroupEdit.Create(nil);
  try
    exists := (aGroupRec <> nil);

    if not(exists) then begin
      aGroupRec := TGEDCOMGroupRecord.Create(FTree, FTree);
      aGroupRec.NewXRef;
    end;

    fmGrpEdit.Tree := FTree;
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

function TfmGEDKeeper.ModifyRecNote(aRecord: TGEDCOMRecordWithLists;
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

function TfmGEDKeeper.ModifyRecMultimedia(aRecord: TGEDCOMRecordWithLists;
  aIndex: Integer; anAction: TRecAction): Boolean;
var
  fileRef: TGEDCOMFileReferenceWithTitle;
  mmRec: TGEDCOMMultimediaRecord;
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

  OpenDialog1.FilterIndex := 2;
  if OpenDialog1.Execute then begin
    if (aIndex > -1) then begin

    end else begin
      mmRec := TGEDCOMMultimediaRecord.Create(FTree, FTree);
      mmRec.NewXRef;
      FTree.AddRecord(mmRec);

      fileRef := TGEDCOMFileReferenceWithTitle.Create(FTree, mmRec);
      fileRef.LinkFile(OpenDialog1.FileName);
      mmRec.AddFileReference(fileRef);

      aRecord.AddMultimediaLink(
        TGEDCOMMultimediaLink.CreateTag(FTree, aRecord, 'OBJE', '@'+mmRec.XRef+'@'));
    end;

    Result := True;
    Modified := True;
  end;
end;

function TfmGEDKeeper.ModifyRecSource(aRecord: TGEDCOMRecordWithLists;
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

  fmSrcCitEdit := TfmSourceCitEdit.Create(Application);
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

function TfmGEDKeeper.ModifyRecAssociation(aRecord: TGEDCOMIndividualRecord;
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

  fmAstEdit := TfmAssociationEdit.Create(nil);
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

procedure TfmGEDKeeper.SetFileName(const Value: string);
begin
  FFileName := Value;
  SetMainTitle();

  FOptions.LastDir := ExtractFilePath(FFileName);
end;

procedure TfmGEDKeeper.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if not(CheckModified())
  then Action := caNone;
end;

procedure TfmGEDKeeper.PageRecordsChange(Sender: TObject);
var
  rt: TGEDCOMRecordType;
begin
  actRecordAdd.Enabled := True;
  actRecordEdit.Enabled := True;
  actRecordDelete.Enabled := True;

  miPedigree.Enabled := (PageRecords.TabIndex = 0);
  actTreeAncestors.Enabled := miPedigree.Enabled;
  actTreeDescendants.Enabled := miPedigree.Enabled;
  tbPedigree.Enabled := miPedigree.Enabled;
  actPedigree_dAboville.Enabled := miPedigree.Enabled;
  actPedigree_Konovalov.Enabled := miPedigree.Enabled;

  case PageRecords.TabIndex of
    0: begin // персоны
      rt := rtIndividual;
    end;

    1: begin // семьи
      rt := rtFamily;
    end;

    2: begin // заметки
      rt := rtNote;
    end;

    3: begin // мультимедиа
      rt := rtMultimedia;
      actRecordAdd.Enabled := False;
      actRecordEdit.Enabled := False;
    end;

    4: begin // источники
      rt := rtSource;
    end;

    5: begin // группы
      rt := rtGroup;
      actRecordDelete.Enabled := False;
    end;
  end;

  StatusBar1.SimpleText := 'Записей: ' + IntToStr(FCounts[rt].Total);
  if (rt = rtIndividual)
  then StatusBar1.SimpleText := StatusBar1.SimpleText + ', фильтр: ' + IntToStr(FCounts[rt].Filtered);
end;

procedure TfmGEDKeeper.ShowNoteInfo(aNoteRec: TGEDCOMNoteRecord; aSummary: TStrings);

  function FindInTag(aTag: TGEDCOMTagWithListsEx): Boolean;
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
  rec: TGEDCOMRecordWithLists;
  iRec: TGEDCOMIndividualRecord;
  famRec: TGEDCOMFamilyRecord;
  st: string;
begin
  aSummary.Clear;

  aSummary.Add('');
  aSummary.AddStrings(aNoteRec.Notes);

  aSummary.Add('');
  aSummary.Add('Ссылки:');

  for i := 0 to FTree.Count - 1 do
    if (FTree.Records[i] is TGEDCOMRecordWithLists) then begin
      rec := FTree.Records[i] as TGEDCOMRecordWithLists;

      // notes in record events
      if (rec is TGEDCOMIndividualRecord) then begin
        iRec := TGEDCOMIndividualRecord(rec);

        for m := 0 to iRec.IndividualEventsCount - 1 do begin
          if FindInTag(TGEDCOMTagWithListsEx(iRec.IndividualEvents[m].Detail)) then begin
            aSummary.Add('    '+HyperLink(rec.XRef, GetNameStr(iRec)) + ', события');
            Break;
          end;
        end;

        for m := 0 to iRec.IndividualAttributesCount - 1 do begin
          if FindInTag(TGEDCOMTagWithListsEx(iRec.IndividualAttributes[m].Detail)) then begin
            aSummary.Add('    '+HyperLink(rec.XRef, GetNameStr(iRec)) + ', атрибуты');
            Break;
          end;
        end;

        for m := 0 to iRec.IndividualOrdinancesCount - 1 do begin
          if FindInTag(TGEDCOMTagWithListsEx(iRec.IndividualOrdinances[m])) then begin
            aSummary.Add('    '+HyperLink(rec.XRef, GetNameStr(iRec)) + ', прочее');
            Break;
          end;
        end;
      end;

      if (rec is TGEDCOMFamilyRecord) then begin
        famRec := TGEDCOMFamilyRecord(rec);

        for m := 0 to famRec.FamilyEventCount - 1 do begin
          if FindInTag(TGEDCOMTagWithListsEx(famRec.FamilyEvents[m].Detail)) then begin
            aSummary.Add('    '+HyperLink(rec.XRef, 'Семья: ' + GetFamilyStr(famRec)) + ', события');
            Break;
          end;
        end;
      end;

      // notes in record
      for k := 0 to rec.NotesCount - 1 do
        if (rec.Notes[k].Value = aNoteRec) then begin
          if (rec is TGEDCOMIndividualRecord)
          then st := GetNameStr(rec as TGEDCOMIndividualRecord)
          else
          if (rec is TGEDCOMFamilyRecord)
          then st := 'Семья: ' + GetFamilyStr(rec as TGEDCOMFamilyRecord)
          else
          if (rec is TGEDCOMGroupRecord)
          then st := 'Группа: ' + TGEDCOMGroupRecord(rec).Name
          else
          if (rec is TGEDCOMSourceRecord)
          then st := 'Источник: ' + TGEDCOMSourceRecord(rec).FiledByEntry
          else st := rec.XRef;

          aSummary.Add('    '+HyperLink(rec.XRef, st));
          Break;
        end;
    end;
end;

procedure TfmGEDKeeper.ListNotesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  note: TGEDCOMNoteRecord;
begin
  if (Item = nil) or not(Selected) then Exit;

  try
    note := TGEDCOMNoteRecord(Item.Data);
    ShowNoteInfo(note, mNoteSummary.Lines);
  except
    on E: Exception do LogWrite('ListNotesSelectItem(): ' + E.Message);
  end;
end;

procedure TfmGEDKeeper.ListMultimediaSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  media: TGEDCOMMultimediaRecord;
  i, k: Integer;
  rec: TGEDCOMRecordWithLists;
  st: string;
begin
  if (Item = nil) or not(Selected) then Exit;

  try
    media := TGEDCOMMultimediaRecord(Item.Data);

    mMediaSummary.Lines.Clear;
    mMediaSummary.Lines.Add('');
    mMediaSummary.Lines.Add('Ссылки:');
    for i := 0 to FTree.Count - 1 do
      if (FTree.Records[i] is TGEDCOMRecordWithLists) then begin
        rec := FTree.Records[i] as TGEDCOMRecordWithLists;

        for k := 0 to rec.MultimediaLinksCount - 1 do
          if (rec.MultimediaLinks[k].Value = media) then begin
            if (rec is TGEDCOMIndividualRecord)
            then st := GetNameStr(rec as TGEDCOMIndividualRecord)
            else
            if (rec is TGEDCOMFamilyRecord)
            then st := 'Семья: ' + GetFamilyStr(rec as TGEDCOMFamilyRecord)
            else st := rec.XRef;

            mMediaSummary.Lines.Add('    '+HyperLink(rec.XRef, st));
            Break;
          end;
      end;
  except
    on E: Exception do LogWrite('ListMultimediaSelectItem(): ' + E.Message);
  end;
end;

procedure TfmGEDKeeper.ShowSourceInfo(aSourceRec: TGEDCOMSourceRecord; aSummary: TStrings);

  procedure OutLink(aRec: TGEDCOMRecord; aCit: TGEDCOMSourceCitation);
  var
    st, page: string;
  begin
    if (aRec is TGEDCOMIndividualRecord)
    then st := GetNameStr(aRec as TGEDCOMIndividualRecord)
    else
    if (aRec is TGEDCOMFamilyRecord)
    then st := 'Семья: ' + GetFamilyStr(aRec as TGEDCOMFamilyRecord)
    else st := aRec.XRef;

    if (aCit.Page <> '')
    then page := aCit.Page+': '
    else page := '';

    aSummary.Add('    '+page+HyperLink(aRec.XRef, st));
  end;

  procedure PrepareTag(aRec: TGEDCOMRecord; tag: TGEDCOMTagWithLists);
  var
    i: Integer;
    exTag: TGEDCOMTagWithListsEx;
  begin
    exTag := TGEDCOMTagWithListsEx(tag);

    for i := 0 to exTag.SourceCitationsCount - 1 do begin
      if (exTag.SourceCitations[i].Value = aSourceRec)
      then OutLink(aRec, exTag.SourceCitations[i]);
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
  rec: TGEDCOMRecordWithLists;
begin
  NavAdd(aSourceRec);

  aSummary.Clear;

  aSummary.Add('');
  aSummary.Add('Автор: ' + Trim(aSourceRec.Originator.Text));
  aSummary.Add('Название: "' + Trim(aSourceRec.Title.Text) + '"');

  aSummary.Add('');
  aSummary.Add('Ссылки:');
  for i := 0 to FTree.Count - 1 do
    if (FTree.Records[i] is TGEDCOMRecordWithLists) then begin
      rec := FTree.Records[i] as TGEDCOMRecordWithLists;

      for k := 0 to rec.SourceCitationsCount - 1 do
        if (rec.SourceCitations[k].Value = aSourceRec)
        then OutLink(rec, rec.SourceCitations[k]);

      if (rec is TGEDCOMIndividualRecord)
      then PreparePerson(rec as TGEDCOMIndividualRecord)
      else
      if (rec is TGEDCOMFamilyRecord)
      then PrepareFamily(rec as TGEDCOMFamilyRecord);
    end;
end;

procedure TfmGEDKeeper.ListSourcesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  source: TGEDCOMSourceRecord;
begin
  if (Item = nil) or not(Selected) then Exit;

  try
    source := TGEDCOMSourceRecord(Item.Data);
    ShowSourceInfo(source, mSourceSummary.Lines);
  except
    on E: Exception do LogWrite('ListSourcesSelectItem(): ' + E.Message);
  end;
end;

procedure TfmGEDKeeper.ShowFamilyInfo(aFamily: TGEDCOMFamilyRecord; aSummary: TStrings);
var
  irec: TGEDCOMIndividualRecord;
  k: Integer;
  st: string;
begin
  try
    NavAdd(aFamily);

    aSummary.Clear;

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
  except
    on E: Exception do LogWrite('FamilyRefresh(): ' + E.Message);
  end;
end;

procedure TfmGEDKeeper.ListFamiliesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  family: TGEDCOMFamilyRecord;
begin
  if (Item = nil)
  then ShowFamilyInfo(nil, mFamilySummary.Lines)
  else begin
    family := TGEDCOMFamilyRecord(Item.Data);
    if (family = nil) then Exit;

    if not(Selected)
    then ShowFamilyInfo(nil, mFamilySummary.Lines)
    else ShowFamilyInfo(family, mFamilySummary.Lines);
  end;
end;

procedure TfmGEDKeeper.mSummaryLink(Sender: TObject; LinkName: String);
begin
  SelectRecordByXRef(LinkName);
end;

procedure TfmGEDKeeper.SetModified(const Value: Boolean);
begin
  FModified := Value;
  SetMainTitle();
end;

procedure TfmGEDKeeper.SetMainTitle();
begin
  Caption := ExtractFileName(FFileName) + ' - ' + AppName;

  if FModified
  then Caption := '* ' + Caption;
end;

procedure TfmGEDKeeper.ListGroupsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  group: TGEDCOMGroupRecord;
  i: Integer;
  member: TGEDCOMIndividualRecord;
  mbrList: TStringList;
begin
  if (Item = nil) or not(Selected) then Exit;

  try
    group := TGEDCOMGroupRecord(Item.Data);

    NavAdd(group);

    mGroupSummary.Lines.BeginUpdate;
    mGroupSummary.Lines.Clear;
    mGroupSummary.Lines.Add('');
    mGroupSummary.Lines.Add('~bu~' + 'Группа: ' + group.Name + '~ub~');
    mGroupSummary.Lines.Add('');
    mGroupSummary.Lines.Add('Участники группы:');

    mbrList := TStringList.Create;
    try
      for i := 0 to group.MembersCount - 1 do begin
        member := TGEDCOMIndividualRecord(group.Members[i].Value);
        mbrList.AddObject(GetNameStr(member), member);
      end;

      mbrList.Sort;

      for i := 0 to mbrList.Count - 1 do begin
        member := TGEDCOMIndividualRecord(mbrList.Objects[i]);
        mGroupSummary.Lines.Add('    '+HyperLink(member.XRef, mbrList[i]));
      end;
    finally
      mbrList.Destroy;
    end;

    mGroupSummary.Lines.EndUpdate;
  except
    on E: Exception do LogWrite('ListGroupsSelectItem(): ' + E.Message);
  end;
end;

procedure TfmGEDKeeper.actFileNewExecute(Sender: TObject);
begin
  if not(CheckModified()) then Exit;

  Clear();
  ListsRefresh();
  ShowPersonInfo(nil, mSummary.Lines);
  FileName := 'Неизвестный';
  Modified := False;
end;

procedure TfmGEDKeeper.LoadFromFile(aFileName: string);
begin
  {$IFDEF PROFILER}Profiler.Mark(1, True);{$ENDIF}

  FTree.LoadFromFile(aFileName);

  {$IFDEF PROFILER}Profiler.Mark(1, False);{$ENDIF}

  CheckGEDCOMFormat(FTree);

  FileName := aFileName;
  Modified := False;

  FNamesTable.ImportNames(FTree);

  AddMRU(aFileName);

  ListsRefresh();
end;

procedure TfmGEDKeeper.actFileLoadExecute(Sender: TObject);
begin
  if not(CheckModified()) then Exit;

  OpenDialog1.InitialDir := FOptions.LastDir;
  if OpenDialog1.Execute
  then LoadFromFile(OpenDialog1.FileName);
end;

procedure TfmGEDKeeper.actFileSaveExecute(Sender: TObject);
begin
  SaveDialog1.InitialDir := ExtractFilePath(ParamStr(0)) + 'files';
  SaveDialog1.FileName := FileName;
  if SaveDialog1.Execute then begin
    SaveToFile(SaveDialog1.FileName);
    FileName := SaveDialog1.FileName;
    AddMRU(SaveDialog1.FileName);
    Modified := False;
  end;
end;

procedure TfmGEDKeeper.actRecordAddExecute(Sender: TObject);
var
  iRec: TGEDCOMIndividualRecord;
  nRec: TGEDCOMNoteRecord;
  sRec: TGEDCOMSourceRecord;
  groupRec: TGEDCOMGroupRecord;
  famRec: TGEDCOMFamilyRecord;
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
    end;

    4: begin // источники
      sRec := nil;
      ModifySource(sRec);
      if (sRec <> nil) then ListsRefresh();
    end;

    5: begin // группы
      groupRec := nil;
      ModifyGroup(groupRec);  
      if (groupRec <> nil) then ListsRefresh();
    end;
  end;
end;

procedure TfmGEDKeeper.actRecordEditExecute(Sender: TObject);
var
  iRec: TGEDCOMIndividualRecord;
  nRec: TGEDCOMNoteRecord;
  srcRec: TGEDCOMSourceRecord;
  family: TGEDCOMFamilyRecord;
  groupRec: TGEDCOMGroupRecord;
begin
  case PageRecords.TabIndex of
    0: begin // персоны
      iRec := GetSelectedPerson();
      if (iRec <> nil) then begin
        fmPersonEdit := TfmPersonEdit.Create(Application);
        try
          fmPersonEdit.Tree := FTree;
          fmPersonEdit.Person := iRec;
          fmPersonEdit.ShowModal;

          ListsRefresh();
        finally
          fmPersonEdit.Destroy;
          fmPersonEdit := nil;
        end;
      end;
    end;

    1: begin // семьи
      family := GetSelectedFamily();
      if (family <> nil) then begin
        ModifyFamily(family);
        ListsRefresh();
      end;
    end;

    2: begin // заметки
      nRec := GetSelectedNote();
      if (nRec <> nil) then begin
        ModifyNote(nRec);
        ListsRefresh();
      end;
    end;

    3: begin // мультимедиа
    end;

    4: begin // источники
      srcRec := GetSelectedSource();
      if (srcRec <> nil) then begin
        ModifySource(srcRec);
        ListsRefresh();
      end;
    end;

    5: begin // группы
      groupRec := GetSelectedGroup();
      if (groupRec <> nil) then begin
        ModifyGroup(groupRec);
        ListsRefresh();
      end;
    end;
  end;
end;

procedure TfmGEDKeeper.actRecordDeleteExecute(Sender: TObject);
var
  iRec: TGEDCOMIndividualRecord;
  nRec: TGEDCOMNoteRecord;

  procedure DeleteMediaRec();
  var
    mRec: TGEDCOMMultimediaRecord;
    i, k: Integer;
    rec: TGEDCOMRecordWithLists;
  begin
    mRec := GetSelectedMultimedia();
    if (mRec = nil) then Exit;

    if (MessageDlg('Удалить мультимедиа "'+mRec.StringValue+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrYes)
    then begin
      for i := 0 to FTree.Count - 1 do begin
        if (FTree.Records[i] is TGEDCOMRecordWithLists) then begin
          rec := FTree.Records[i] as TGEDCOMRecordWithLists;

          for k := rec.MultimediaLinksCount - 1 downto 0 do begin
            if (rec.MultimediaLinks[k].Value = mRec)
            then rec.DeleteMultimediaLink(k);
          end;
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
    rec: TGEDCOMRecordWithLists;
  begin
    sRec := GetSelectedSource();
    if (sRec = nil) then Exit;

    if (MessageDlg('Удалить источник "'+sRec.FiledByEntry+'"?', mtConfirmation, [mbNo, mbYes], 0) = mrYes)
    then begin
      for i := 0 to FTree.Count - 1 do begin
        if (FTree.Records[i] is TGEDCOMRecordWithLists) then begin
          rec := FTree.Records[i] as TGEDCOMRecordWithLists;

          for k := rec.SourceCitationsCount - 1 downto 0 do begin
            if (rec.SourceCitations[k].Value = sRec)
            then rec.DeleteSourceCitation(k);
          end;
        end;
      end;

      FTree.Delete(FTree.IndexOfRecord(sRec));

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
      DeleteFamily(GetSelectedFamily());
      ListsRefresh();
    end;

    2: begin // заметки
      nRec := GetSelectedNote();
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

    5: begin // группы
    end;
  end;
end;

procedure TfmGEDKeeper.actExportToExcelExecute(Sender: TObject);
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

procedure TfmGEDKeeper.actContextHelpExecute(Sender: TObject);
begin
  LoadExtFile(GetAppPath() + 'help\GEDKeeper.htm');
end;

procedure TfmGEDKeeper.actFilePropertiesExecute(Sender: TObject);
var
  fmFileProps: TfmFileProperties;
begin
  fmFileProps := TfmFileProperties.Create(nil);
  try
    fmFileProps.Tree := FTree;
    fmFileProps.ShowModal();
  finally
    fmFileProps.Destroy;
  end;
end;

procedure TfmGEDKeeper.actStatsExecute(Sender: TObject);
begin
  fmStats.Show;
end;

procedure TfmGEDKeeper.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmGEDKeeper.actOptionsExecute(Sender: TObject);
var
  fmOptions: TfmOptions;
begin
  fmOptions := TfmOptions.Create(Application);
  try
    fmOptions.Options := Options;
    if (fmOptions.ShowModal = mrOk)
    then ListsRefresh(True);
  finally
    fmOptions.Destroy;
  end;
end;

procedure TfmGEDKeeper.actAboutExecute(Sender: TObject);
begin
  AboutDialog(AppName, 'Serg V. Zhdanovskih', '');
end;

procedure TfmGEDKeeper.actFilterExecute(Sender: TObject);
var
  fmFilter: TfmFilter;
begin
  fmFilter := TfmFilter.Create(Application);
  try
    fmFilter.ShowModal;
  finally
    fmFilter.Destroy;
  end;
end;

procedure TfmGEDKeeper.actTreeToolsExecute(Sender: TObject);
var
  fmTreeTools: TfmTreeTools;
begin
  fmTreeTools := TfmTreeTools.Create(Application);
  try
    fmTreeTools.ShowModal();
  finally
    fmTreeTools.Destroy;
  end;
end;

procedure TfmGEDKeeper.ListPersonsDblClick(Sender: TObject);
begin
  actRecordEditExecute(nil);
end;

procedure TfmGEDKeeper.ListFamiliesDblClick(Sender: TObject);
begin
  actRecordEditExecute(nil);
end;

procedure TfmGEDKeeper.ListNotesDblClick(Sender: TObject);
begin
  actRecordEditExecute(nil);
end;

procedure TfmGEDKeeper.ListMultimediaDblClick(Sender: TObject);
begin
  actRecordEditExecute(nil);
end;

procedure TfmGEDKeeper.ListSourcesDblClick(Sender: TObject);
begin
  actRecordEditExecute(nil);
end;

procedure TfmGEDKeeper.ListGroupsDblClick(Sender: TObject);
begin
  actRecordEditExecute(nil);
end;

function TfmGEDKeeper.GetCurFileTempPath(): string;
begin
  Result := ExtractFilePath(FFileName) + '~temp\';
end;

procedure TfmGEDKeeper.actExportToWebExecute(Sender: TObject);
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

procedure TfmGEDKeeper.actTreeAncestorsExecute(Sender: TObject);
var
  fmChart: TfmChart;
begin
  fmChart := TfmChart.Create(nil);
  fmChart.Tree := FTree;
  fmChart.Person := GetSelectedPerson();
  fmChart.ChartKind := ckAncestors;
  fmChart.GenChart();
end;

procedure TfmGEDKeeper.actTreeDescendantsExecute(Sender: TObject);
var
  fmChart: TfmChart;
begin
  fmChart := TfmChart.Create(nil);
  fmChart.Tree := FTree;
  fmChart.Person := GetSelectedPerson();
  fmChart.ChartKind := ckDescendants;
  fmChart.GenChart();
end;

procedure TfmGEDKeeper.actPedigree_dAbovilleExecute(Sender: TObject);
var
  p: TPedigree;
begin
  p := TPedigree.Create;
  try
    p.Options := FOptions.PedigreeOptions;
    p.Generate(ExtractFilePath(FFileName), FTree, GetSelectedPerson(), pk_dAboville);
  finally
    p.Destroy;
  end;
end;

procedure TfmGEDKeeper.actPedigree_KonovalovExecute(Sender: TObject);
var
  p: TPedigree;
begin
  p := TPedigree.Create;
  try
    p.Options := FOptions.PedigreeOptions;
    p.Generate(ExtractFilePath(FFileName), FTree, GetSelectedPerson(), pk_Konovalov);
  finally
    p.Destroy;
  end;
end;

procedure TfmGEDKeeper.miPersonScanClick(Sender: TObject);
var
  fmPersonScan: TfmPersonScan;
begin
  fmPersonScan := TfmPersonScan.Create(Application);
  try
    fmPersonScan.ShowModal;
  finally
    fmPersonScan.Destroy;
  end;
end;

procedure TfmGEDKeeper.actLifeGameExecute(Sender: TObject);
var
  fmLife: TfmLife;
begin
  try
    fmLife := TfmLife.Create(Application);
    try
      fmLife.ShowModal;
    finally
      fmLife.Destroy;
    end;
  except
    Hole(fmLife);
  end;
end;

procedure TfmGEDKeeper.actMapExecute(Sender: TObject);
var
  fmMaps: TfmMaps;
begin
  fmMaps := TfmMaps.Create(Application);
  try
    fmMaps.Tree := FTree;
    fmMaps.Show;
  finally
    //fmMaps.Destroy;
  end;
end;

procedure TfmGEDKeeper.actGenResourcesExecute(Sender: TObject);
begin
  LoadExtFile(GetAppPath() + 'help\genres.htm');
end;

procedure TfmGEDKeeper.actKinshipTermsExecute(Sender: TObject);
begin
  LoadExtFile(GetAppPath() + 'help\relations.htm');
end;

procedure TfmGEDKeeper.ChangeRecord(aRecord: TGEDCOMRecord);
begin
  FChangedRecords.Add(aRecord);
end;

procedure TfmGEDKeeper.ChangesClear();
begin
  FChangedRecords.Clear;
end;

procedure TfmGEDKeeper.ApplyFilter();
begin
  if (FTree.Count > 0)
  then ListsRefresh();
end;

procedure TfmGEDKeeper.actPrevExecute(Sender: TObject);
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

procedure TfmGEDKeeper.actNextExecute(Sender: TObject);
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

procedure TfmGEDKeeper.NavClear();
begin
  FNavHistory.Clear();
  FNavPos := -1;
end;

procedure TfmGEDKeeper.NavAdd(aRec: TGEDCOMRecord);
begin
  if (aRec <> nil) and not(FNavBusy) then begin
    FNavPos := FNavHistory.Add(aRec);
    NavUpdate();
  end;
end;

procedure TfmGEDKeeper.NavUpdate();
begin
  actPrev.Enabled := (FNavPos > 0);
  actNext.Enabled := (FNavPos < FNavHistory.Count - 1);
end;

procedure TfmGEDKeeper.UpdateMRU();
var
  i: Integer;
  mi: TMenuItem;
begin
  miMRUFiles.Enabled := (FOptions.MRUFiles.Count > 0);
  miMRUFiles.Clear();

  MenuMRU.Items.Clear;

  for i := 0 to FOptions.MRUFiles.Count - 1 do begin
    mi := TMenuItem.Create(Self);
    mi.Caption := FOptions.MRUFiles[i];
    mi.Tag := i;
    mi.OnClick := MRUFileClick;
    miMRUFiles.Add(mi);

    mi := TMenuItem.Create(Self);
    mi.Caption := FOptions.MRUFiles[i];
    mi.Tag := i;
    mi.OnClick := MRUFileClick;
    MenuMRU.Items.Add(mi);
  end;
end;

procedure TfmGEDKeeper.AddMRU(const aFileName: string);
var
  idx: Integer;
begin
  idx := FOptions.MRUFiles.IndexOf(aFileName);

  if (idx < 0)
  then FOptions.MRUFiles.Insert(0, aFileName)
  else begin
    if (idx > 0) then begin
      FOptions.MRUFiles.Delete(idx);
      FOptions.MRUFiles.Insert(0, aFileName);
    end;
  end;

  UpdateMRU();
end;

procedure TfmGEDKeeper.MRUFileClick(Sender: TObject);
var
  idx: Integer;
begin
  if not(CheckModified()) then Exit;

  idx := (Sender as TMenuItem).Tag;
  LoadFromFile(FOptions.MRUFiles[idx]);
end;

procedure TfmGEDKeeper.actPersonNameCopyExecute(Sender: TObject);
var
  person: TGEDCOMIndividualRecord;
begin
  if (ActiveControl <> ListPersons) then Exit;
  if (ListPersons.Selected = nil) then Exit;

  person := TGEDCOMIndividualRecord(ListPersons.Selected.Data);
  if (person = nil) then Exit;

  Clipboard.AsText := GetNameStr(person);
end;

end.
