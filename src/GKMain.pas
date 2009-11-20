unit GKMain;

// 08.01.2009

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, ImgList, ToolWin, Buttons, Menus,
  GKCommon, GedCom551, GKCtrls, Htmemo, Masks, ActnList;

type
  TPersonsFilter = class(TObject)
  public
    AliveBeforeDate: string;
    LifeMode: TLifeMode;
    Name: string;
    PatriarchOnly: Boolean;
    Sex: TGEDCOMSex;

    constructor Create();
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
    miDuplicatesMerge: TMenuItem;
    miDocAuthor: TMenuItem;
    miTreeAncestors: TMenuItem;
    miTreeDescendants: TMenuItem;
    miDataCheck: TMenuItem;
    miTools: TMenuItem;
    miStats: TMenuItem;
    SheetNotes: TTabSheet;
    SheetMultimedia: TTabSheet;
    SheetSources: TTabSheet;
    SheetFamilies: TTabSheet;
    ListSources: TBSListView;
    ListFamilies: TBSListView;
    ListNotes: TBSListView;
    ListMultimedia: TBSListView;
    miBaseCompare: TMenuItem;
    miFileMerge: TMenuItem;
    miExportToWeb: TMenuItem;
    N7: TMenuItem;
    miOptions: TMenuItem;
    miSplitBase: TMenuItem;
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
    N3: TMenuItem;
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
    actFileMerge: TAction;
    actExit: TAction;
    actOptions: TAction;
    actAbout: TAction;
    actDataCheck: TAction;
    actDuplicatesMerge: TAction;
    actSplitBase: TAction;
    actFilter: TAction;
    actBaseCompare: TAction;
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
    PopupMenu1: TPopupMenu;
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
    N12: TMenuItem;
    miMaps: TMenuItem;
    actMaps: TAction;
    miGenResources: TMenuItem;
    actGenResources: TAction;
    miKinshipTerms: TMenuItem;
    actKinshipTerms: TAction;
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
    procedure actFileMergeExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actDataCheckExecute(Sender: TObject);
    procedure actDuplicatesMergeExecute(Sender: TObject);
    procedure actSplitBaseExecute(Sender: TObject);
    procedure actFilterExecute(Sender: TObject);
    procedure actBaseCompareExecute(Sender: TObject);
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
    procedure actMapsExecute(Sender: TObject);
    procedure actGenResourcesExecute(Sender: TObject);
    procedure actKinshipTermsExecute(Sender: TObject);
  private
    FChangedRecords: TList;
    FCounts: array [TGEDCOMRecordType] of Integer;
    FDefDateFormat: TDateFormat;
    FDefNameFormat: TNameFormat;
    FFileName: string;
    FFilter: TPersonsFilter;
    FLastDir: string;
    FModified: Boolean;
    FOptions: TGlobalOptions;

    function CheckModified: Boolean;
    procedure CleanFamily(aFamily: TGEDCOMFamilyRecord);
    procedure LoadFile(aFileName: string);

    procedure SetDefDateFormat(const Value: TDateFormat);
    procedure SetDefNameFormat(const Value: TNameFormat);
    procedure SetFileName(const Value: string);
    procedure SetMainTitle();
    procedure SetModified(const Value: Boolean);
    procedure ShowAddress(anAddress: TGEDCOMAddress; aSummary: TStrings);
  public
    FDefCharacterSet: TGEDCOMCharacterSet;
    FNamesTable: TNamesTable;
    FTree: TGEDCOMTree;

    procedure AddSpouseToFamily(aFamily: TGEDCOMFamilyRecord;
      aSpouse: TGEDCOMIndividualRecord);
    procedure ApplyFilter();

    procedure CalcCounts();
    procedure ChangeRecord(aRecord: TGEDCOMRecord);
    procedure ChangesClear();
    procedure Clear();
    procedure ComListFamiliesRefresh(aList: TBSListView);
    procedure ComListGroupsRefresh(aList: TBSListView; aTitles: Boolean);
    procedure ComListPersonsRefresh(aSexFilter: TGEDCOMSex; aList: TBSListView;
      aTitles: Boolean);
    procedure ComListPersonsTitlesRefresh(aList: TBSListView);
    procedure ComListMultimediaRefresh(aList: TBSListView);
    procedure ComListNotesRefresh(aList: TBSListView; aTitles: Boolean);
    procedure ComListSourcesRefresh(aList: TBSListView; aTitles: Boolean);

    function CreateFamily(): TGEDCOMFamilyRecord;
    function CreatePerson(aName, aPatronymic, aFamily: string;
      aSex: TGEDCOMSex; aBirthEvent: Boolean = False): TGEDCOMIndividualRecord;
    function CreatePersonDialog(aTarget: TGEDCOMIndividualRecord;
      aTargetMode: TTargetMode; aNeedSex: TGEDCOMSex): TGEDCOMIndividualRecord;

    procedure DeleteFamily(aFamily: TGEDCOMFamilyRecord);
    procedure DeleteIndividualRecord(iRec: TGEDCOMIndividualRecord);
    procedure DeleteNoteRecord(nRec: TGEDCOMNoteRecord);

    function  GetChildFamily(iRec: TGEDCOMIndividualRecord; aCanCreate: Boolean;
      aNewParent: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;

    function  GetSelectedFamily(): TGEDCOMFamilyRecord;
    function  GetSelectedMultimedia(): TGEDCOMMultimediaRecord;
    function  GetSelectedNote(): TGEDCOMNoteRecord;
    function  GetSelectedPerson(): TGEDCOMIndividualRecord;
    function  GetSelectedSource(): TGEDCOMSourceRecord;
    function  GetSelectedGroup(): TGEDCOMGroupRecord;

    procedure ListsRefresh();
    procedure LoadOptions();

    //function ModifyPerson(var aIndivRec: TGEDCOMIndividualRecord): Boolean;
    function ModifyFamily(var aFamilyRec: TGEDCOMFamilyRecord; aSpouse: TGEDCOMIndividualRecord = nil): Boolean;
    function ModifyNote(var aNoteRec: TGEDCOMNoteRecord): Boolean;
    function ModifySource(var aSourceRec: TGEDCOMSourceRecord): Boolean;
    function ModifyGroup(var aGroupRec: TGEDCOMGroupRecord): Boolean;

    function ModifyRecAssociation(aRecord: TGEDCOMIndividualRecord; aIndex: Integer; anAction: TRecAction): Boolean;
    function ModifyRecAttribute(aRecord: TGEDCOMRecordWithLists; aIndex: Integer; anAction: TRecAction): Boolean;
    function ModifyRecEvent(aRecord: TGEDCOMRecordWithLists; aIndex: Integer; anAction: TRecAction): Boolean;
    function ModifyRecMultimedia(aRecord: TGEDCOMRecordWithLists; aIndex: Integer; anAction: TRecAction): Boolean;
    function ModifyRecNote(aRecord: TGEDCOMRecordWithLists; aIndex: Integer; anAction: TRecAction): Boolean;
    function ModifyRecSource(aRecord: TGEDCOMRecordWithLists; aIndex: Integer; anAction: TRecAction): Boolean;

    procedure RecListAssociationsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListFamilyEventsRefresh(aRecord: TGEDCOMFamilyRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListGroupsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListIndividualEventsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListIndividualAttributesRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
    procedure RecListMediaRefresh(aRecord: TGEDCOMRecordWithLists; aList: TListBox; aSummary: TStrings);
    procedure RecListNotesRefresh(aRecord: TGEDCOMRecordWithLists; aList: TListBox; aSummary: TStrings);
    procedure RecListSourcesRefresh(aRecord: TGEDCOMRecordWithLists; aList: TBSListView; aSummary: TStrings);

    procedure RemoveFamilySpouse(family: TGEDCOMFamilyRecord; spouse: TGEDCOMIndividualRecord);

    procedure SaveOptions();
    procedure SaveToFile(const aFileName: string);

    procedure SelectFamilyByIRec(aFRec: TGEDCOMFamilyRecord);
    procedure SelectGroupByIRec(aRec: TGEDCOMGroupRecord);
    procedure SelectPersonByIRec(aIRec: TGEDCOMIndividualRecord);
    procedure SelectSourceByIRec(aRec: TGEDCOMSourceRecord);
    procedure SelectRecordByXRef(XRef: string);

    procedure ShowFamilyInfo(aFamily: TGEDCOMFamilyRecord; aSummary: TStrings);
    procedure ShowNoteInfo(aNoteRec: TGEDCOMNoteRecord; aSummary: TStrings);
    procedure ShowPersonInfo(iRec: TGEDCOMIndividualRecord; aSummary: TStrings);

    property DefDateFormat: TDateFormat read FDefDateFormat write SetDefDateFormat;
    property DefNameFormat: TNameFormat read FDefNameFormat write SetDefNameFormat;
    property FileName: string read FFileName write SetFileName;
    property Filter: TPersonsFilter read FFilter;
    property Modified: Boolean read FModified write SetModified;
    property Options: TGlobalOptions read FOptions;
  end;

var
  fmGEDKeeper: TfmGEDKeeper;

implementation

uses
  Types, IniFiles, GKUtils, GKPersonNew, GKRecordSelect, GKStats, GKNoteEdit,
  GKSourceEdit, GKEventEdit, GKAbout, GKChartCore, GKFileProperties, GKMerge,
  GKPersonEdit, GKExport, GKOptions, GKFamilyEdit, GKAssociationEdit, GKFilter,
  GKSplitBase, GKDiagnosis, GKGroupEdit, GKChart, GKPersonScan, GKLifeMain,
  GKMaps, GKProgress;

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
  LifeMode := lmAll;
  Name := '*';
end;

{ TfmGEDKeeper }

procedure TfmGEDKeeper.FormCreate(Sender: TObject);
begin
  LogInit(GetAppPath() + 'GEDKeeper.log');

  LongDateFormat := 'DD MMM YYYY';

  FChangedRecords := TList.Create;
  FOptions := TGlobalOptions.Create;
  FNamesTable := TNamesTable.Create;
  FTree := TGEDCOMTree.Create;
  FFilter := TPersonsFilter.Create;

  FNamesTable.LoadFromFile(GetAppPath() + 'GEDKeeper.nms');
  LoadOptions();

  if (ParamStr(1) = '')
  then actFileNewExecute(nil)
  else LoadFile(ParamStr(1));
end;

procedure TfmGEDKeeper.FormDestroy(Sender: TObject);
begin
  ListPersons.Clear;

  SaveOptions();
  FNamesTable.SaveToFile(GetAppPath() + 'GEDKeeper.nms');

  FFilter.Destroy;
  FTree.Destroy;
  FNamesTable.Destroy;
  FOptions.Destroy;
  FChangedRecords.Destroy;

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
  FTree.Header.CharacterSet := FDefCharacterSet;
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
var
  rec: TGEDCOMRecord;
begin
  rec := FTree.XRefIndex_Find(XRef);

  if (rec is TGEDCOMIndividualRecord)
  then SelectPersonByIRec(rec as TGEDCOMIndividualRecord)
  else
  if (rec is TGEDCOMFamilyRecord)
  then SelectFamilyByIRec(rec as TGEDCOMFamilyRecord)
  else
  if (rec is TGEDCOMGroupRecord)
  then SelectGroupByIRec(rec as TGEDCOMGroupRecord)
  else
  if (rec is TGEDCOMSourceRecord)
  then SelectSourceByIRec(rec as TGEDCOMSourceRecord)
  else ;
end;

function TfmGEDKeeper.CreateFamily(): TGEDCOMFamilyRecord;
begin
  Result := TGEDCOMFamilyRecord.Create(FTree, FTree);
  Result.NewXRef();
  Result.ChangeDate.ChangeDateTime := Now();
  FTree.AddRecord(Result);
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

procedure TfmGEDKeeper.AddSpouseToFamily(aFamily: TGEDCOMFamilyRecord;
  aSpouse: TGEDCOMIndividualRecord);
begin
  case aSpouse.Sex of
    svNone: ;
    svMale: begin
      aFamily.SetTagStringValue('HUSB', '@'+aSpouse.XRef+'@');
    end;
    svFemale: begin
      aFamily.SetTagStringValue('WIFE', '@'+aSpouse.XRef+'@');
    end;
    svUndetermined: ;
  end;

  aSpouse.AddSpouseToFamilyLink(
    TGEDCOMSpouseToFamilyLink.CreateTag(
      FTree, aSpouse, 'FAMS', '@'+aFamily.XRef+'@'));
end;

function TfmGEDKeeper.CreatePerson(aName, aPatronymic, aFamily: string;
  aSex: TGEDCOMSex; aBirthEvent: Boolean = False): TGEDCOMIndividualRecord;
var
  event: TGEDCOMCustomEvent;
begin
  Result := TGEDCOMIndividualRecord.Create(FTree, FTree);
  Result.NewXRef;
  Result.AddPersonalName(
    TGEDCOMPersonalName.CreateTag(FTree, Result,
      'NAME', Trim(aName) + ' ' + Trim(aPatronymic) + ' /' + Trim(aFamily) + '/'));
  Result.Sex := aSex;
  Result.ChangeDate.ChangeDateTime := Now();

  FTree.AddRecord(Result);

  if (aBirthEvent) then begin
    event := TGEDCOMIndividualEvent.Create(FTree, Result);
    event.Name := 'BIRT';
    Result.AddIndividualEvent(TGEDCOMIndividualEvent(event));
  end;

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

procedure TfmGEDKeeper.RemoveFamilySpouse(family: TGEDCOMFamilyRecord;
  spouse: TGEDCOMIndividualRecord);
begin
  if (spouse <> nil) then begin
    spouse.DeleteSpouseToFamilyLink(family);

    case spouse.Sex of
      svNone: ;
      svMale: family.SetTagStringValue('HUSB', '');
      svFemale: family.SetTagStringValue('WIFE', '');
      svUndetermined: ;
    end;
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
  RemoveFamilySpouse(aFamily, spouse);

  spouse := TGEDCOMIndividualRecord(aFamily.Wife.Value);
  RemoveFamilySpouse(aFamily, spouse);
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
    RemoveFamilySpouse(family, iRec);
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
  st, unk: string;
  fam, nam, pat, marr: string;
begin
  try
    if (iRec = nil) then Exit;

    try
      GetNameParts(iRec, fam, nam, pat);

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
        //aSummary.Add(IntToStr(iRec.SpouseToFamilyLinksCount));
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
          if (rel_person <> nil) then begin
            aSummary.Add({#13#10 + }st + HyperLink(rel_person.XRef, GetNameStr(rel_person)) + ' (' + HyperLink(family.XRef, marr) + ')');
          end else begin
            aSummary.Add({#13#10 + }st + unk + ' (' + HyperLink(family.XRef, marr) + ')');
          end;

          if (family.ChildrenCount <> 0) then begin
            aSummary.Add('');
            aSummary.Add({#13#10}'Дети:');
          end;

          for k := 0 to family.ChildrenCount - 1 do begin
            rel_person := TGEDCOMIndividualRecord(family.Children[k].Value);
            aSummary.Add({#09}'    ' + HyperLink(rel_person.XRef, GetNameStr(rel_person)) + GetLifeStr(rel_person));
          end;
        end;

        RecListIndividualEventsRefresh(iRec, nil, aSummary);
        RecListIndividualAttributesRefresh(iRec, nil, aSummary);
        RecListNotesRefresh(iRec, nil, aSummary);
        RecListMediaRefresh(iRec, nil, aSummary);
        RecListSourcesRefresh(iRec, nil, aSummary);
        RecListAssociationsRefresh(iRec, nil, aSummary);
        RecListGroupsRefresh(iRec, nil, aSummary);
      except
        on E: Exception do LogWrite('PersonRefresh().Families(): ' + E.Message);
      end;
    finally
      aSummary.EndUpdate;
    end;
  except
    on E: Exception do LogWrite('PersonRefresh(): ' + E.Message);
  end;
end;

procedure TfmGEDKeeper.RecListIndividualEventsRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
var
  idx, ev: Integer;
  event: TGEDCOMIndividualEvent;
  st: string;
  item: TListItem;
begin
  try
    if (aList <> nil)
    then aList.Clear();

    if (aRecord.IndividualEventsCount <> 0) then begin
      if (aList = nil) and (aSummary <> nil) then begin
        aSummary.Add('');
        aSummary.Add({#13#10}'События:');
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

        if (aList <> nil) then begin
          item := aList.Items.Add();
          item.Caption := st;
          item.SubItems.Add(GEDCOMCustomDateToStr(event.Detail.Date.Value, FDefDateFormat));
          item.SubItems.Add(event.Detail.Place);
          item.Data := TObject(idx);
        end;
      end;
    end;
  except
    on E: Exception do LogWrite('ListIndividualEventsRefresh(): ' + E.Message);
  end;
end;

procedure TfmGEDKeeper.ShowAddress(anAddress: TGEDCOMAddress; aSummary: TStrings);
var
  k: Integer;
  ts: string;
begin
  if not(anAddress.IsEmpty) and (aSummary <> nil) then begin
    ts := '    ';
    if (anAddress.AddressCountry <> '')
    then ts := ts + anAddress.AddressCountry + ', ';
    if (anAddress.AddressState <> '')
    then ts := ts + anAddress.AddressState + ', ';
    aSummary.Add(ts + anAddress.AddressCity);

    ts := '    ';
    if (anAddress.AddressPostalCode <> '')
    then ts := ts + anAddress.AddressPostalCode + ', ';
    aSummary.Add(ts + Trim(anAddress.Address.Text));

    for k := 0 to anAddress.PhoneNumbersCount - 1 do
      aSummary.Add('    ' + anAddress.PhoneNumbers[k]);

    for k := 0 to anAddress.EmailAddressesCount - 1 do
      aSummary.Add('    ' + anAddress.EmailAddresses[k]);

    for k := 0 to anAddress.WebPagesCount - 1 do
      aSummary.Add('    ' + anAddress.WebPages[k]);
  end;
end;

procedure TfmGEDKeeper.RecListIndividualAttributesRefresh(aRecord: TGEDCOMIndividualRecord; aList: TBSListView; aSummary: TStrings);
var
  idx, ev: Integer;
  attr: TGEDCOMIndividualAttribute;
  st: string;
  item: TListItem;
begin
  try
    if (aList <> nil)
    then aList.Clear();

    if (aRecord.IndividualAttributesCount <> 0) then begin
      if (aList = nil) and (aSummary <> nil) then begin
        aSummary.Add('');
        aSummary.Add({#13#10}'Атрибуты:');
      end;

      for idx := 0 to aRecord.IndividualAttributesCount - 1 do begin
        attr := aRecord.IndividualAttributes[idx];

        ev := GetPersonAttributeIndex(attr.Name);
        if (ev = 0) then st := attr.Detail.Classification
        else
        if (ev > 0) then st := PersonAttributes[ev].Name
        else st := attr.Name;

        if (aList = nil) and (aSummary <> nil)
        then aSummary.Add(st + ': ' + attr.StringValue);

        ShowAddress(attr.Detail.Address, aSummary);

        if (aList <> nil) then begin
          item := aList.Items.Add();
          item.Caption := st;
          item.SubItems.Add(GEDCOMCustomDateToStr(attr.Detail.Date.Value, FDefDateFormat));
          item.SubItems.Add(attr.StringValue);
          item.Data := TObject(idx);
        end;
      end;
    end;
  except
    on E: Exception do LogWrite('RecListIndividualAttributesRefresh(): ' + E.Message);
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

        if (aList <> nil) then begin
          item := aList.Items.Add();
          item.Caption := st;
          item.SubItems.Add(GEDCOMCustomDateToStr(event.Detail.Date.Value, FDefDateFormat));
          item.SubItems.Add(event.Detail.Place);
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
          if (aList = nil) and (aSummary <> nil)
          then aSummary.Add('  ' + HyperLink(sourceRec.XRef, '"'+sourceRec.FiledByEntry+'"'));

          if (aList <> nil) then begin
            item := aList.Items.Add();
            item.Caption := Trim(sourceRec.Originator.Text);
            item.SubItems.Add(sourceRec.FiledByEntry);
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
  for i := Ord(Low(TGEDCOMRecordType)) to Ord(High(TGEDCOMRecordType)) do
    FCounts[TGEDCOMRecordType(i)] := 0;

  for i := 0 to FTree.Count - 1 do begin
    rec := FTree.Records[i];

    if (rec is TGEDCOMFamilyRecord)
    then Inc(FCounts[rtFamily])
    else
    if (rec is TGEDCOMIndividualRecord)
    then Inc(FCounts[rtIndividual])
    else
    if (rec is TGEDCOMMultimediaRecord)
    then Inc(FCounts[rtMultimedia])
    else
    if (rec is TGEDCOMNoteRecord)
    then Inc(FCounts[rtNote])
    else
    if (rec is TGEDCOMRepositoryRecord)
    then Inc(FCounts[rtRepository])
    else
    if (rec is TGEDCOMSourceRecord)
    then Inc(FCounts[rtSource])
    else
    if (rec is TGEDCOMSubmissionRecord)
    then Inc(FCounts[rtSubmission])
    else
    if (rec is TGEDCOMSubmitterRecord)
    then Inc(FCounts[rtSubmitter])
    else
    if (rec is TGEDCOMGroupRecord)
    then Inc(FCounts[rtGroup]);
  end;
end;

procedure TfmGEDKeeper.ListsRefresh();
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
  ComListPersonsRefresh(svNone, ListPersons, False);
  RestoreBookmark(ListPersons);

  SaveBookmark(ListFamilies);
  ComListFamiliesRefresh(ListFamilies);
  RestoreBookmark(ListFamilies);

  SaveBookmark(ListNotes);
  ComListNotesRefresh(ListNotes, False);
  RestoreBookmark(ListNotes);

  SaveBookmark(ListMultimedia);
  ComListMultimediaRefresh(ListMultimedia);
  RestoreBookmark(ListMultimedia);

  SaveBookmark(ListSources);
  ComListSourcesRefresh(ListSources, False);
  RestoreBookmark(ListSources);

  SaveBookmark(ListGroups);
  ComListGroupsRefresh(ListGroups, False);
  RestoreBookmark(ListGroups);

  PageRecordsChange(nil);
end;

procedure TfmGEDKeeper.ComListPersonsTitlesRefresh(aList: TBSListView);
begin
  aList.Columns.BeginUpdate;
  aList.Columns.Clear;

  AddColumn(aList, '№', 50);
  AddColumn(aList, 'П', 25);
  case FDefNameFormat of
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
  AddColumn(aList, 'Возраст', 100);
  AddColumn(aList, 'Продолжительность жизни', 100);
  AddColumn(aList, 'Дней до ДР', 100);
  AddColumn(aList, 'Группа', 200);
  if (aList = ListPersons) then begin
    AddColumn(aList, 'Изменено', 150);
  end;

  aList.Columns.EndUpdate;
end;

procedure TfmGEDKeeper.ComListPersonsRefresh(aSexFilter: TGEDCOMSex;
  aList: TBSListView; aTitles: Boolean);

  procedure SetPersonItem(aItem: TListItem; aPerson: TGEDCOMIndividualRecord);

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

  var
    p_tag: TGEDCOMTag;
    f, n, p, nm: string;
  begin
    aItem.Caption := IntToStr(GetId(aPerson));

    p_tag := aPerson.FindTag('_PATRIARCH');

    if (p_tag = nil)
    then aItem.SubItems.Add(' ')
    else aItem.SubItems.Add('*');

    case FDefNameFormat of
      nfFNP: begin
        nm := GetNameStr(aPerson);
        aItem.SubItems.Add(nm);
      end;
      nfF_NP: begin
        GetNameParts(aPerson, f, n, p);
        aItem.SubItems.Add(f);
        aItem.SubItems.Add(n + ' ' + p);
      end;
      nfF_N_P: begin
        GetNameParts(aPerson, f, n, p);
        aItem.SubItems.Add(f);
        aItem.SubItems.Add(n);
        aItem.SubItems.Add(p);
      end;
    end;

    aItem.SubItems.Add(SexSigns[aPerson.Sex]);

    aItem.SubItems.Add(GetBirthDate(aPerson, FDefDateFormat));
    aItem.SubItems.Add(GetDeathDate(aPerson, FDefDateFormat));

    aItem.SubItems.Add(GetBirthPlace(aPerson));
    aItem.SubItems.Add(GetDeathPlace(aPerson));

    aItem.SubItems.Add(GetAttributeValue(aPerson, 'RESI'));

    aItem.SubItems.Add(GetAge(aPerson));
    aItem.SubItems.Add(GetLifeExpectancy(aPerson));
    aItem.SubItems.Add(GetDaysForBirth(aPerson));

    aItem.SubItems.Add(GetGroups(aPerson));

    if (aList = ListPersons)
    then aItem.SubItems.Add(GetChangeDate(aPerson));

    aItem.Data := aPerson;
  end;

  procedure PrepareRec(iRec: TGEDCOMIndividualRecord);
  var
    item: TListItem;
    nm{, gcd}: string;
    ev: TGEDCOMCustomEvent;
    bd, dd, fdt: TDateTime;
    res: Boolean;
    p_tag: TGEDCOMTag;
  begin
    nm := GetNameStr(iRec);
    p_tag := iRec.FindTag('_PATRIARCH');

    if (aSexFilter <> svNone) and (iRec.Sex <> aSexFilter)
    then Exit;

    if (aList = ListPersons) then begin
      if (FFilter.LifeMode = lmOnlyAlive) and not(IsLive(iRec))
      or (FFilter.LifeMode = lmOnlyDead) and (IsLive(iRec))
      then Exit;

      if (FFilter.LifeMode = lmAliveBefore) then begin
        //gcd := StrToGEDCOMDate(FFilterAliveBeforeDate);
        fdt := StrToDate(FFilter.AliveBeforeDate);

        ev := GetIndividualEvent(iRec, 'BIRT');
        if (ev <> nil)
        then bd := GEDCOMDateToDate(ev.Detail.Date.Value)
        else bd := 0.0;

        ev := GetIndividualEvent(iRec, 'DEAT');
        if (ev <> nil)
        then dd := GEDCOMDateToDate(ev.Detail.Date.Value)
        else dd := 0.0;

        res := ((bd <> 0) and (bd < fdt)) and ((dd = 0) or ((dd <> 0) and (dd > fdt)));

        if not(res) then Exit;

        //if (GetDeathDate(iRec, dfDD_MM_YYYY) > gcd)
      end;

      if (FFilter.Sex <> svNone) and (iRec.Sex <> FFilter.Sex)
      then Exit;

      if not MatchesMask(nm, FFilter.Name)
      then Exit;

      if (FFilter.PatriarchOnly and (p_tag = nil))
      then Exit;
    end;

    item := aList.FindData(0, iRec, True, False);
    if (item = nil)
    then item := aList.Items.Add()
    else item.SubItems.Clear();
    
    SetPersonItem(item, iRec);
  end;

var
  i: Integer;
begin
  if (aTitles)
  then ComListPersonsTitlesRefresh(aList);

  aList.Items.BeginUpdate();

  if (FChangedRecords.Count = 0) then begin
    aList.Items.Clear();
    for i := 0 to FTree.Count - 1 do
      if (FTree.Records[i] is TGEDCOMIndividualRecord)
      then PrepareRec(FTree.Records[i] as TGEDCOMIndividualRecord);
  end else begin
    for i := FChangedRecords.Count - 1 downto 0 do
      if (TObject(FChangedRecords[i]) is TGEDCOMIndividualRecord) then begin
        PrepareRec(TObject(FChangedRecords[i]) as TGEDCOMIndividualRecord);
        FChangedRecords.Delete(i);
      end;
  end;

  ResizeColumn(aList, 2);

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
      item.SubItems.Add(GetMarriageDate(famRec, FDefDateFormat));

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

procedure TfmGEDKeeper.SelectPersonByIRec(aIRec: TGEDCOMIndividualRecord);
var
  i: Integer;
  item: TListItem;
begin
  PageRecords.TabIndex := 0;
  PageRecordsChange(nil);

  for i := 0 to ListPersons.Items.Count - 1 do begin
    item := ListPersons.Items[i];

    if (item.Data = aIRec) then begin
      ListPersons.Selected := item;
      item.MakeVisible(False);
      Break;
    end;
  end;
end;

procedure TfmGEDKeeper.SelectSourceByIRec(aRec: TGEDCOMSourceRecord);
var
  i: Integer;
  item: TListItem;
begin
  PageRecords.TabIndex := 4;
  PageRecordsChange(nil);

  for i := 0 to ListSources.Items.Count - 1 do begin
    item := ListSources.Items[i];

    if (item.Data = aRec) then begin
      ListSources.Selected := item;
      item.MakeVisible(False);
      Break;
    end;
  end;
end;

procedure TfmGEDKeeper.LoadOptions();
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(GetAppPath() + 'GEDKeeper.ini');
  try
    FOptions.ChartOptions.FamilyVisible := ini.ReadBool('Chart', 'FamilyVisible', True);
    FOptions.ChartOptions.NameVisible := ini.ReadBool('Chart', 'NameVisible', True);
    FOptions.ChartOptions.PatronymicVisible := ini.ReadBool('Chart', 'PatronymicVisible', True);
    FOptions.ChartOptions.DiffLines := ini.ReadBool('Chart', 'DiffLines', False);
    FOptions.ChartOptions.BirthDateVisible := ini.ReadBool('Chart', 'BirthDateVisible', False);
    FOptions.ChartOptions.DeathDateVisible := ini.ReadBool('Chart', 'DeathDateVisible', False);
    FOptions.ChartOptions.Kinship := ini.ReadBool('Chart', 'Kinship', False);

    FOptions.ChartOptions.MaleColor := ini.ReadInteger('Chart', 'MaleColor', $00FFC6C6);
    FOptions.ChartOptions.FemaleColor := ini.ReadInteger('Chart', 'FemaleColor', $00C6C6FF);
    FOptions.ChartOptions.UnkSexColor := ini.ReadInteger('Chart', 'UnkSexColor', $00FFC6FF);
    FOptions.ChartOptions.UnHusbandColor := ini.ReadInteger('Chart', 'UnHusbandColor', $00FFD7D7);
    FOptions.ChartOptions.UnWifeColor := ini.ReadInteger('Chart', 'UnWifeColor', $00D7D7FF);

    FDefCharacterSet := TGEDCOMCharacterSet(ini.ReadInteger('Common', 'DefCharacterSet', Ord(csUTF8)));
    DefNameFormat := TNameFormat(ini.ReadInteger('Common', 'DefNameFormat', Ord(nfFNP)));
    DefDateFormat := TDateFormat(ini.ReadInteger('Common', 'DefDateFormat', Ord(dfDD_MM_YYYY)));

    FLastDir := ini.ReadString('Common', 'LastDir', '');

    FOptions.Proxy.UseProxy := ini.ReadBool('Proxy', 'UseProxy', False);
    FOptions.Proxy.Server := ini.ReadString('Proxy', 'Server', '');
    FOptions.Proxy.Port := ini.ReadString('Proxy', 'Port', '');
    FOptions.Proxy.Login := ini.ReadString('Proxy', 'Login', '');
    FOptions.Proxy.Password := Decrypt(ini.ReadString('Proxy', 'Password', ''), 777);

    FOptions.PedigreeOptions.IncludeAttributes := ini.ReadBool('Pedigree', 'IncludeAttributes', True);
    FOptions.PedigreeOptions.IncludeNotes := ini.ReadBool('Pedigree', 'IncludeNotes', True);
    FOptions.PedigreeOptions.IncludeSources := ini.ReadBool('Pedigree', 'IncludeSources', True);
  finally
    ini.Destroy;
  end;
end;

procedure TfmGEDKeeper.SaveOptions();
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(GetAppPath() + 'GEDKeeper.ini');
  try
    ini.WriteBool('Chart', 'FamilyVisible', FOptions.ChartOptions.FamilyVisible);
    ini.WriteBool('Chart', 'NameVisible', FOptions.ChartOptions.NameVisible);
    ini.WriteBool('Chart', 'PatronymicVisible', FOptions.ChartOptions.PatronymicVisible);
    ini.WriteBool('Chart', 'DiffLines', FOptions.ChartOptions.DiffLines);
    ini.WriteBool('Chart', 'BirthDateVisible', FOptions.ChartOptions.BirthDateVisible);
    ini.WriteBool('Chart', 'DeathDateVisible', FOptions.ChartOptions.DeathDateVisible);
    ini.WriteBool('Chart', 'Kinship', FOptions.ChartOptions.Kinship);

    ini.WriteInteger('Chart', 'MaleColor', FOptions.ChartOptions.MaleColor);
    ini.WriteInteger('Chart', 'FemaleColor', FOptions.ChartOptions.FemaleColor);
    ini.WriteInteger('Chart', 'UnkSexColor', FOptions.ChartOptions.UnkSexColor);
    ini.WriteInteger('Chart', 'UnHusbandColor', FOptions.ChartOptions.UnHusbandColor);
    ini.WriteInteger('Chart', 'UnWifeColor', FOptions.ChartOptions.UnWifeColor);

    ini.WriteInteger('Common', 'DefCharacterSet', Ord(FDefCharacterSet));
    ini.WriteInteger('Common', 'DefNameFormat', Ord(FDefNameFormat));
    ini.WriteInteger('Common', 'DefDateFormat', Ord(FDefDateFormat));

    ini.WriteString('Common', 'LastDir', FLastDir);

    ini.WriteBool('Proxy', 'UseProxy', FOptions.Proxy.UseProxy);
    ini.WriteString('Proxy', 'Server', FOptions.Proxy.Server);
    ini.WriteString('Proxy', 'Port', FOptions.Proxy.Port);
    ini.WriteString('Proxy', 'Login', FOptions.Proxy.Login);
    ini.WriteString('Proxy', 'Password', Encrypt(FOptions.Proxy.Password, 777));

    ini.WriteBool('Pedigree', 'IncludeAttributes', FOptions.PedigreeOptions.IncludeAttributes);
    ini.WriteBool('Pedigree', 'IncludeNotes', FOptions.PedigreeOptions.IncludeNotes);
    ini.WriteBool('Pedigree', 'IncludeSources', FOptions.PedigreeOptions.IncludeSources);
  finally
    ini.Destroy;
  end;
end;

function TfmGEDKeeper.ModifyRecEvent(aRecord: TGEDCOMRecordWithLists;
  aIndex: Integer; anAction: TRecAction): Boolean;
var
  fmEventEdit: TfmEventEdit;
  event: TGEDCOMCustomEvent; // TGEDCOMIndividualEvent + TGEDCOMFamilyEvent
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить событие?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    if (aRecord is TGEDCOMIndividualRecord)
    then TGEDCOMIndividualRecord(aRecord).DeleteIndividualEvent(aIndex)
    else TGEDCOMFamilyRecord(aRecord).DeleteFamilyEvent(aIndex);

    Result := True;
    Modified := True;

    Exit;
  end;

  fmEventEdit := TfmEventEdit.Create(nil);

  try
    if (aIndex > -1) then begin
      if (aRecord is TGEDCOMIndividualRecord)
      then event := TGEDCOMIndividualRecord(aRecord).IndividualEvents[aIndex]
      else event := TGEDCOMFamilyRecord(aRecord).FamilyEvents[aIndex];
    end else begin
      if (aRecord is TGEDCOMIndividualRecord)
      then event := TGEDCOMIndividualEvent.Create(FTree, aRecord)
      else event := TGEDCOMFamilyEvent.Create(FTree, aRecord);
    end;

    fmEventEdit.Event := event;

    case fmEventEdit.ShowModal of
      mrOk: begin
        if (aIndex = -1) then begin
          if (aRecord is TGEDCOMIndividualRecord)
          then TGEDCOMIndividualRecord(aRecord).AddIndividualEvent(TGEDCOMIndividualEvent(event))
          else TGEDCOMFamilyRecord(aRecord).AddFamilyEvent(TGEDCOMFamilyEvent(event));
        end;

        Result := True;
        Modified := True;
      end;

      mrCancel: begin
        if (aIndex <= -1)
        then event.Destroy;
      end;
    end;
  finally
    fmEventEdit.Destroy;
  end;
end;

function TfmGEDKeeper.ModifyRecAttribute(aRecord: TGEDCOMRecordWithLists;
  aIndex: Integer; anAction: TRecAction): Boolean;
var
  fmEventEdit: TfmEventEdit;
  attr: TGEDCOMCustomEvent; // TGEDCOMIndividualAttribute
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить атрибут?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    TGEDCOMIndividualRecord(aRecord).DeleteIndividualAttribute(aIndex);

    Result := True;
    Modified := True;

    Exit;
  end;

  fmEventEdit := TfmEventEdit.Create(nil);

  try
    if (aIndex > -1)
    then attr := TGEDCOMIndividualRecord(aRecord).IndividualAttributes[aIndex]
    else attr := TGEDCOMIndividualAttribute.Create(FTree, aRecord);

    fmEventEdit.Event := attr;

    case fmEventEdit.ShowModal of
      mrOk: begin
        if (aIndex = -1)
        then TGEDCOMIndividualRecord(aRecord).AddIndividualAttribute(TGEDCOMIndividualAttribute(attr));

        Result := True;
        Modified := True;
      end;

      mrCancel: begin
        if (aIndex <= -1)
        then attr.Destroy;
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
    then AddSpouseToFamily(aFamilyRec, aSpouse);

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
  sourceRec: TGEDCOMSourceRecord;
  cit: TGEDCOMSourceCitation;
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

  if (anAction = raEdit) then begin
    if (aIndex > -1) then begin
      cit := aRecord.SourceCitations[aIndex];
      sourceRec := TGEDCOMSourceRecord(cit.Value);
      Result := ModifySource(sourceRec);
    end;
  end else begin
    sourceRec := TGEDCOMSourceRecord(SelectRecord(smSource));
    if (sourceRec <> nil) then begin
      cit := TGEDCOMSourceCitation.Create(FTree, aRecord);
      cit.Value := sourceRec;
      aRecord.AddSourceCitation(cit);

      Result := True;
    end;
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

  FLastDir := ExtractFilePath(FFileName); 
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

  StatusBar1.SimpleText := 'Записей: ' + IntToStr(FCounts[rt]);
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

procedure TfmGEDKeeper.SelectFamilyByIRec(aFRec: TGEDCOMFamilyRecord);
var
  i: Integer;
  item: TListItem;
begin
  PageRecords.TabIndex := 1;
  PageRecordsChange(nil);

  for i := 0 to ListFamilies.Items.Count - 1 do begin
    item := ListFamilies.Items[i];

    if (item.Data = aFRec) then begin
      ListFamilies.Selected := item;
      item.MakeVisible(False);
      Break;
    end;
  end;
end;

procedure TfmGEDKeeper.SelectGroupByIRec(aRec: TGEDCOMGroupRecord);
var
  i: Integer;
  item: TListItem;
begin
  PageRecords.TabIndex := 5;
  PageRecordsChange(nil);

  for i := 0 to ListGroups.Items.Count - 1 do begin
    item := ListGroups.Items[i];

    if (item.Data = aRec) then begin
      ListGroups.Selected := item;
      item.MakeVisible(False);
      Break;
    end;
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

procedure TfmGEDKeeper.ListSourcesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);

  procedure ShowSourceInfo(aSourceRec: TGEDCOMSourceRecord; aSummary: TStrings);
  var
    i, k: Integer;
    rec: TGEDCOMRecordWithLists;
    st: string;
  begin
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
          if (rec.SourceCitations[k].Value = aSourceRec) then begin
            if (rec is TGEDCOMIndividualRecord)
            then st := GetNameStr(rec as TGEDCOMIndividualRecord)
            else
            if (rec is TGEDCOMFamilyRecord)
            then st := 'Семья: ' + GetFamilyStr(rec as TGEDCOMFamilyRecord)
            else st := rec.XRef;

            aSummary.Add('    '+HyperLink(rec.XRef, st));
            Break;
          end;
      end;
  end;

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

procedure TfmGEDKeeper.SetDefNameFormat(const Value: TNameFormat);
begin
  FDefNameFormat := Value;

  ComListPersonsTitlesRefresh(ListPersons);

  if (FTree.Count > 0)
  then ListsRefresh();
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

procedure TfmGEDKeeper.SetDefDateFormat(const Value: TDateFormat);
begin
  FDefDateFormat := Value;

  if (FTree.Count > 0)
  then ListsRefresh();
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

procedure TfmGEDKeeper.LoadFile(aFileName: string);
begin
  FTree.LoadFromFile(aFileName);

  CheckGEDCOMFormat(FTree);

  FileName := aFileName;
  Modified := False;

  FNamesTable.ImportNames(FTree);

  ListsRefresh();
end;

procedure TfmGEDKeeper.actFileLoadExecute(Sender: TObject);
begin
  if not(CheckModified()) then Exit;

  OpenDialog1.InitialDir := FLastDir;
  //ExtractFilePath(ParamStr(0)) + 'files';
  if OpenDialog1.Execute then begin
    LoadFile(OpenDialog1.FileName);
  end;
end;

procedure TfmGEDKeeper.actFileSaveExecute(Sender: TObject);
begin
  SaveDialog1.InitialDir := ExtractFilePath(ParamStr(0)) + 'files';
  SaveDialog1.FileName := FileName;
  if SaveDialog1.Execute then begin
    SaveToFile(SaveDialog1.FileName);
    FileName := SaveDialog1.FileName;
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
        SelectPersonByIRec(iRec);
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
begin
  ExportToExcel(ExtractFilePath(FFileName), FTree);
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

procedure TfmGEDKeeper.actFileMergeExecute(Sender: TObject);
begin
  if (OpenDialog1.Execute) then begin
    MergeFiles(FTree, OpenDialog1.FileName);
    ListsRefresh();
  end;
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
    fmOptions.ShowModal;
  finally
    fmOptions.Destroy;
  end;
end;

procedure TfmGEDKeeper.actAboutExecute(Sender: TObject);
begin
  AboutDialog(AppName, 'Serg V. Zhdanovskih', '');
end;

procedure TfmGEDKeeper.actDataCheckExecute(Sender: TObject);
var
  fmDiag: TfmDiagnosis;
begin
  fmDiag := TfmDiagnosis.Create(nil);
  try
    fmDiag.Check(FTree);
    fmDiag.ShowModal;
  finally
    fmDiag.Destroy;
  end;
end;

procedure TfmGEDKeeper.actDuplicatesMergeExecute(Sender: TObject);
var
  fmMerge: TfmMerge;
begin
  fmMerge := TfmMerge.Create(Application);
  try
    fmMerge.ShowModal;
  finally
    fmMerge.Destroy;
  end;
end;

procedure TfmGEDKeeper.actSplitBaseExecute(Sender: TObject);
var
  fmSplitBase: TfmSplitBase;
begin
  fmSplitBase := TfmSplitBase.Create(Application);
  try
    fmSplitBase.ShowModal;
  finally
    fmSplitBase.Destroy;
  end;
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

procedure TfmGEDKeeper.actBaseCompareExecute(Sender: TObject);
var
  fmDiag: TfmDiagnosis;
begin
  if OpenDialog1.Execute() then begin
    fmDiag := TfmDiagnosis.Create(Application);
    try
      fmDiag.Compare(FTree, OpenDialog1.FileName);
      fmDiag.ShowModal();
    finally
      fmDiag.Destroy;
    end;
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

procedure TfmGEDKeeper.actExportToWebExecute(Sender: TObject);
var
  web: TWebExporter;
begin
  web := TWebExporter.Create;
  try
    web.Generate(ExtractFilePath(FFileName) + 'html\', FTree);
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

procedure TfmGEDKeeper.actMapsExecute(Sender: TObject);
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

end.
