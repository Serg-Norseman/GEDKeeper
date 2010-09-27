unit GKTreeTools;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, GedCom551, GKImport, bsCtrls, GKBase;

type
  TTreeWalkMode = (twmAll, twmFamily, twmAncestors, twmDescendants, twmNone);

  TMergeMode = (mmPerson, mmNote, mmFamily, mmSource);

  TfmTreeTools = class(TForm)
    PageControl: TPageControl;
    SheetChoice: TTabSheet;
    SheetTreeCompare: TTabSheet;
    ListCompare: TListView;
    btnClose: TBitBtn;
    rgOperation: TRadioGroup;
    btnBack: TBitBtn;
    btnNext: TBitBtn;
    Label1: TLabel;
    edCompareFile: TEdit;
    btnFileChoose: TBitBtn;
    OpenDialog1: TOpenDialog;
    SheetTreeMerge: TTabSheet;
    SheetTreeSplit: TTabSheet;
    Label2: TLabel;
    edMergeFile: TEdit;
    btnMergeFileChoose: TBitBtn;
    mRes: TMemo;
    SaveDialog1: TSaveDialog;
    btnSelectAll: TBitBtn;
    ListSelected: TListBox;
    ListSkipped: TListBox;
    btnSelectFamily: TBitBtn;
    btnSelectAncestors: TBitBtn;
    btnSelectDescendants: TBitBtn;
    btnDelete: TBitBtn;
    btnSave: TBitBtn;
    SheetRecMerge: TTabSheet;
    PageControl1: TPageControl;
    SheetMerge: TTabSheet;
    Lab1: TLabel;
    Lab2: TLabel;
    btnSearch: TBitBtn;
    Edit1: TEdit;
    Edit2: TEdit;
    btnRec1Select: TBitBtn;
    btnRec2Select: TBitBtn;
    Memo1: TMemo;
    Memo2: TMemo;
    btnMergeToLeft: TBitBtn;
    btnMergeToRight: TBitBtn;
    btnSkip: TBitBtn;
    ProgressBar1: TProgressBar;
    SheetOptions: TTabSheet;
    rgMode: TRadioGroup;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    rbDirectMatching: TRadioButton;
    rbIndistinctMatching: TRadioButton;
    edNameAccuracy: TEdit;
    udNameAccuracy: TUpDown;
    edYearInaccuracy: TEdit;
    udYearInaccuracy: TUpDown;
    chkBirthYear: TCheckBox;
    SheetImport: TTabSheet;
    Label3: TLabel;
    edImportFile: TEdit;
    btnImportFileChoose: TBitBtn;
    ListBox1: TListBox;
    OpenDialog2: TOpenDialog;
    SheetFamilyGroups: TTabSheet;
    TreeView1: TTreeView;
    SheetDeads: TTabSheet;
    btnPrepare: TBitBtn;
    Panel1: TPanel;
    SheetSync: TTabSheet;
    Label4: TLabel;
    edMasterBase: TEdit;
    Label7: TLabel;
    edUpdateBase: TEdit;
    btnUpdateSelect: TBitBtn;
    GroupBox2: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Panel2: TPanel;
    mSyncRes: TMemo;
    SheetPatSearch: TTabSheet;
    btnPatSearch: TBitBtn;
    Panel3: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnBackClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnFileChooseClick(Sender: TObject);
    procedure btnMergeFileChooseClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnSelectFamilyClick(Sender: TObject);
    procedure btnSelectAncestorsClick(Sender: TObject);
    procedure btnSelectDescendantsClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnRec1SelectClick(Sender: TObject);
    procedure btnRec2SelectClick(Sender: TObject);
    procedure btnMergeToLeftClick(Sender: TObject);
    procedure btnMergeToRightClick(Sender: TObject);
    procedure rgModeClick(Sender: TObject);
    procedure btnSkipClick(Sender: TObject);
    procedure btnImportFileChooseClick(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure btnPrepareClick(Sender: TObject);
    procedure btnUpdateSelectClick(Sender: TObject);
    procedure btnPatSearchClick(Sender: TObject);
  private
    FSplitCounter: Integer;
    FSplitList: TList;
    FTree: TGEDCOMTree;

    FRec1, FRec2: TGEDCOMRecord;
    FRMMode: TMergeMode;
    FRMSkip: TStringList;

    ListDeads: TBSListView;
    ListPatriarchs: TBSListView;

    procedure RecordMerge(aRecBase, aRecCopy: TGEDCOMRecord);
    procedure SetRec1(const Value: TGEDCOMRecord);
    procedure SetRec2(const Value: TGEDCOMRecord);

    procedure TreeWalk(iRec: TGEDCOMIndividualRecord; aMode: TTreeWalkMode);
    procedure Select(aPerson: TGEDCOMIndividualRecord; aMode: TTreeWalkMode);
    procedure CheckRelations();

    procedure AddDiag(aObj, aDiag: string);
    procedure TreeCompare(aMainTree: TGEDCOMTree; aFileName: string);

    procedure CheckDeads();
    procedure CheckGroups();

    procedure SelectTool(aToolIndex: Integer);
    function GetBase: TfmBase;

    procedure PrepareDeadsList();

    procedure PreparePatriarchsList();
    procedure ListPatriarchsDblClick(Sender: TObject);
  public
    property Base: TfmBase read GetBase;
  end;

implementation

uses
  {$IFDEF DELPHI_NET}System.IO,{$ENDIF}
  Contnrs, GKCommon, GKMain, GKRecordSelect, GKProgress, GKSheetList;

{$R *.dfm}

{ TfmTreeWizard }

procedure TfmTreeTools.FormCreate(Sender: TObject);
begin
  FTree := Base.Tree;
  PageControl.ActivePageIndex := 0;

  FSplitList := TList.Create;

  FRMSkip := TStringList.Create;
  SetRec1(nil);
  SetRec2(nil);
  FRMMode := mmPerson;
  rgMode.ItemIndex := 0;

  PrepareDeadsList();
  PreparePatriarchsList();
end;

procedure TfmTreeTools.FormDestroy(Sender: TObject);
begin
  FRMSkip.Free;

  FSplitList.Free;
end;

procedure TfmTreeTools.AddDiag(aObj, aDiag: string);
var
  item: TListItem;
begin
  item := ListCompare.Items.Add();
  item.Caption := aObj;
  item.SubItems.Add(aDiag);
end;

procedure TfmTreeTools.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then Close;
end;

procedure TfmTreeTools.TreeCompare(aMainTree: TGEDCOMTree; aFileName: string);
var
  tempTree: TGEDCOMTree;
  i, idx, k: Integer;
  iRec: TGEDCOMIndividualRecord;
  fams, names: TStringList;
  fam, nam, pat, tm: string;
  lst: TList;
begin
  ListCompare.Clear;

  tempTree := TGEDCOMTree.Create;
  tempTree.LoadFromFile(aFileName);
  fams := TStringList.Create;
  names := TStringList.Create;
  try
    AddDiag('', 'Поиск совпадений...');

    for i := 0 to aMainTree.RecordsCount - 1 do
      if (aMainTree.Records[i] is TGEDCOMIndividualRecord) then begin
        iRec := aMainTree.Records[i] as TGEDCOMIndividualRecord;

        idx := names.AddObject(GetNameStr(iRec), TList.Create);
        TList(names.Objects[idx]).Add(iRec);

        GetNameParts(iRec, fam, nam, pat);
        fams.AddObject(PrepareRusFamily(fam, (iRec.Sex = svFemale)), nil);
      end;

    for i := 0 to tempTree.RecordsCount - 1 do
      if (tempTree.Records[i] is TGEDCOMIndividualRecord) then begin
        iRec := tempTree.Records[i] as TGEDCOMIndividualRecord;
        tm := GetNameStr(iRec);

        idx := names.IndexOf(tm);
        if (idx >= 0)
        then TList(names.Objects[idx]).Add(iRec);

        GetNameParts(iRec, fam, nam, pat);
        tm := PrepareRusFamily(fam, (iRec.Sex = svFemale));

        idx := fams.IndexOf(tm);
        if (idx >= 0)
        then fams.Objects[idx] := TObject(1);
      end;

    for i := fams.Count - 1 downto 0 do
      if (fams.Objects[i] = nil) or (fams[i] = '?') then fams.Delete(i);

    for i := names.Count - 1 downto 0 do
      if (TList(names.Objects[i]).Count = 1) then begin
        TList(names.Objects[i]).Free;
        names.Delete(i);
      end;

    AddDiag('', 'Схожие фамилии:');
    if (fams.Count <> 0) then begin
      for i := 0 to fams.Count - 1 do AddDiag('', '    ' + fams[i]);
    end else AddDiag('', '    нет.');

    AddDiag('', 'Схожие имена:');
    if (names.Count <> 0) then begin
      for i := 0 to names.Count - 1 do begin
        AddDiag('', '    ' + names[i]);
        lst := TList(names.Objects[i]);
        for k := 0 to lst.Count - 1 do begin
          iRec := TGEDCOMIndividualRecord(lst[k]);
          AddDiag('', '      * ' + GetNameStr(iRec) + ' ' + GetLifeStr(iRec));
        end;
      end;
    end else AddDiag('', '    нет.');
  finally
    for i := 0 to names.Count - 1 do TObject(names.Objects[i]).Free;
    names.Free;

    fams.Free;
    tempTree.Destroy;
  end;
end;

procedure TfmTreeTools.SelectTool(aToolIndex: Integer);
begin
  case aToolIndex of
    0..4: ;
    5: CheckGroups();
    6: CheckDeads();
    7: ;
  end;
end;

procedure TfmTreeTools.btnBackClick(Sender: TObject);
begin
  PageControl.ActivePageIndex := 0;
  btnBack.Enabled := False;
  btnNext.Enabled := True;
end;

procedure TfmTreeTools.btnNextClick(Sender: TObject);
begin
  PageControl.ActivePageIndex := rgOperation.ItemIndex + 1;
  btnBack.Enabled := True;
  btnNext.Enabled := False;

  SelectTool(rgOperation.ItemIndex);
end;

procedure TfmTreeTools.btnFileChooseClick(Sender: TObject);
begin
  if OpenDialog1.Execute() then begin
    edCompareFile.Text := OpenDialog1.FileName;
    TreeCompare(FTree, edCompareFile.Text);
  end;
end;

procedure TfmTreeTools.btnMergeFileChooseClick(Sender: TObject);
var
  old_count, new_count: Integer;
begin
  if OpenDialog1.Execute() then begin
    edMergeFile.Text := OpenDialog1.FileName;

    mRes.Clear;

    old_count := FTree.RecordsCount;
    mRes.Lines.Add('Количество объектов в основной базе: ' + IntToStr(old_count));

    Base.GenBase.TreeMerge(edMergeFile.Text);

    new_count := FTree.RecordsCount;
    mRes.Lines.Add('Новое количество объектов в основной базе: ' + IntToStr(new_count));

    Base.ListsRefresh();
  end;
end;

procedure TfmTreeTools.TreeWalk(iRec: TGEDCOMIndividualRecord; aMode: TTreeWalkMode);
var
  rel_person: TGEDCOMIndividualRecord;
  sp: TGEDCOMPointer;
  family: TGEDCOMFamilyRecord;
  i, k: Integer;
  int_mode: TTreeWalkMode; // twmAll, twmFamily, twmAncestors, twmDescendants, twmNone
begin
  if (iRec = nil) then Exit;
  if (FSplitList.IndexOf(iRec) >= 0) then Exit;

  Inc(FSplitCounter);
  FSplitList.Add(iRec);

  if (aMode = twmNone) then Exit;

  // родители
  if (aMode in [twmAll, twmAncestors]) then begin
    if (iRec.ChildToFamilyLinksCount <> 0) then begin
      family := iRec.ChildToFamilyLinks[0].Family;

      rel_person := TGEDCOMIndividualRecord(family.Husband.Value);
      TreeWalk(rel_person, aMode);

      rel_person := TGEDCOMIndividualRecord(family.Wife.Value);
      TreeWalk(rel_person, aMode);
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
      TreeWalk(rel_person, int_mode);

      case aMode of
        twmAll: int_mode := twmAll;
        twmFamily: int_mode := twmNone;
        twmDescendants: int_mode := twmDescendants;
      end;

      for k := 0 to family.ChildrenCount - 1 do begin
        rel_person := TGEDCOMIndividualRecord(family.Children[k].Value);
        TreeWalk(rel_person, int_mode);
      end;
    end;
  end;
end;

procedure TfmTreeTools.Select(aPerson: TGEDCOMIndividualRecord; aMode: TTreeWalkMode);
var
  i, cnt: Integer;
  i_rec: TGEDCOMIndividualRecord;
begin
  FSplitCounter := 0;

  ListSelected.Items.BeginUpdate;
  ListSelected.Items.Clear;

  ListSkipped.Items.BeginUpdate;
  ListSkipped.Items.Clear;

  try
    FSplitList.Clear;
    i_rec := aPerson;

    TreeWalk(i_rec, aMode);

    cnt := 0;
    for i := 0 to FTree.RecordsCount - 1 do
      if (FTree.Records[i] is TGEDCOMIndividualRecord) then begin
        Inc(cnt);

        i_rec := (FTree.Records[i] as TGEDCOMIndividualRecord);

        if (FSplitList.IndexOf(i_rec) < 0)
        then ListSkipped.Items.Add(i_rec.XRef + ' / ' + GetNameStr(i_rec))
        else ListSelected.Items.Add(i_rec.XRef + ' / ' + GetNameStr(i_rec));
      end;

    Caption := IntToStr(FSplitList.Count) + ' / ' + IntToStr(cnt);
  finally
    ListSelected.Items.EndUpdate;
    ListSkipped.Items.EndUpdate;
  end;
end;

procedure TfmTreeTools.CheckRelations();

  procedure AddRel(aRec: TGEDCOMRecord);
  begin
    if (FSplitList.IndexOf(aRec) < 0)
    then FSplitList.Add(aRec);
  end;

  procedure CheckRecord(rec: TGEDCOMRecord);
  var
    i: Integer;
  begin
    for i := 0 to rec.MultimediaLinksCount - 1 do AddRel(rec.MultimediaLinks[i].Value);
    for i := 0 to rec.NotesCount - 1 do AddRel(rec.Notes[i].Value);
    for i := 0 to rec.SourceCitationsCount - 1 do AddRel(rec.SourceCitations[i].Value);
  end;

  procedure CheckTag(tag: TGEDCOMTagWithLists);
  var
    i: Integer;
  begin
    for i := 0 to tag.MultimediaLinksCount - 1 do AddRel(tag.MultimediaLinks[i].Value);
    for i := 0 to tag.NotesCount - 1 do AddRel(tag.Notes[i].Value);
    for i := 0 to tag.SourceCitationsCount - 1 do AddRel(tag.SourceCitations[i].Value);
  end;

  procedure CheckIndividual(iRec: TGEDCOMIndividualRecord);
  var
    i: Integer;
  begin
    CheckRecord(iRec);

    // PersonalNames
    for i := 0 to iRec.ChildToFamilyLinksCount - 1 do AddRel(iRec.ChildToFamilyLinks[i].Family);
    for i := 0 to iRec.SpouseToFamilyLinksCount - 1 do AddRel(iRec.SpouseToFamilyLinks[i].Family);
    for i := 0 to iRec.IndividualEventsCount - 1 do CheckTag(iRec.IndividualEvents[i].Detail);
    for i := 0 to iRec.IndividualAttributesCount - 1 do CheckTag(iRec.IndividualAttributes[i].Detail);
    for i := 0 to iRec.IndividualOrdinancesCount - 1 do CheckTag(iRec.IndividualOrdinances[i]);
    for i := 0 to iRec.SubmittorsCount - 1 do AddRel(iRec.Submittors[i].Value);
    for i := 0 to iRec.AssociationsCount - 1 do AddRel(iRec.Associations[i].Value);
    for i := 0 to iRec.AliassesCount - 1 do AddRel(iRec.Aliasses[i].Value);
    for i := 0 to iRec.AncestorsInterestCount - 1 do AddRel(iRec.AncestorsInterest[i].Value);
    for i := 0 to iRec.DescendantsInterestCount - 1 do AddRel(iRec.DescendantsInterest[i].Value);
    // UserReferencesCount
    for i := 0 to iRec.GroupsCount - 1 do AddRel(iRec.Groups[i].Value);
  end;

  procedure CheckFamily(fRec: TGEDCOMFamilyRecord);
  var
    i: Integer;
  begin
    CheckRecord(fRec);

    for i := 0 to fRec.FamilyEventCount - 1 do CheckTag(fRec.FamilyEvents[i].Detail);
    AddRel(fRec.Submitter.Value);
    for i := 0 to fRec.SpouseSealingCount - 1 do CheckTag(fRec.SpouseSealing[i]);
  end;

  procedure CheckSource(sRec: TGEDCOMSourceRecord);
  var
    i: Integer;
  begin
    CheckRecord(sRec);

    for i := 0 to sRec.RepositoryCitationsCount - 1 do AddRel(sRec.RepositoryCitations[i].Value);
  end;

var
  i: Integer;
  rec: TGEDCOMRecord;
begin
  i := 0;
  while (i < FSplitList.Count) do begin
    rec := TGEDCOMRecord(FSplitList[i]);

    {fixme}
    if (rec is TGEDCOMFamilyRecord)
    then CheckFamily(rec as TGEDCOMFamilyRecord)
    else
    if (rec is TGEDCOMIndividualRecord)
    then CheckIndividual(rec as TGEDCOMIndividualRecord)
    else
    if (rec is TGEDCOMMultimediaRecord)
    then CheckRecord(rec)
    else
    if (rec is TGEDCOMNoteRecord)
    then CheckRecord(rec)
    else
    if (rec is TGEDCOMRepositoryRecord)
    then CheckRecord(rec)
    else
    if (rec is TGEDCOMSourceRecord)
    then CheckSource(rec as TGEDCOMSourceRecord)
    else
    if (rec is TGEDCOMSubmissionRecord)
    then
    else
    if (rec is TGEDCOMSubmitterRecord)
    then CheckRecord(rec)
    else
    if (rec is TGEDCOMGroupRecord)
    then {!};

    Inc(i);
  end;
end;

procedure TfmTreeTools.btnSelectAllClick(Sender: TObject);
begin
  Select(Base.GetSelectedPerson(), twmAll);
end;

procedure TfmTreeTools.btnSelectFamilyClick(Sender: TObject);
begin
  Select(Base.GetSelectedPerson(), twmFamily);
end;

procedure TfmTreeTools.btnSelectAncestorsClick(Sender: TObject);
begin
  Select(Base.GetSelectedPerson(), twmAncestors);
end;

procedure TfmTreeTools.btnSelectDescendantsClick(Sender: TObject);
begin
  Select(Base.GetSelectedPerson(), twmDescendants);
end;

procedure TfmTreeTools.btnDeleteClick(Sender: TObject);
var
  i: Integer;
  p: TGEDCOMIndividualRecord;
begin
  //alert

  for i := 0 to FSplitList.Count - 1 do begin
    p := TGEDCOMIndividualRecord(FSplitList[i]);
    Base.DeleteIndividualRecord(p, False);
  end;

  Base.ListsRefresh();
end;

procedure TfmTreeTools.btnSaveClick(Sender: TObject);
var
  fs: TFileStream;
  subm: string;
  i: Integer;
  rec: TGEDCOMRecord;
begin
  if not(SaveDialog1.Execute) then Exit;

  CheckRelations();

  subm := FTree.Header.TagStringValue('SUBM');

  FTree.Header.Clear;
  FTree.Header.Source := AppName;
  FTree.Header.ReceivingSystemName := AppName;
  FTree.Header.CharacterSet := fmGEDKeeper.Options.DefCharacterSet;
  FTree.Header.Language := 'Russian';
  FTree.Header.GEDCOMVersion := '5.5';
  FTree.Header.GEDCOMForm := 'LINEAGE-LINKED';
  FTree.Header.FileName := ExtractFileName(SaveDialog1.FileName);
  FTree.Header.TransmissionDate.Date := Now();

  if (subm <> '')
  then FTree.Header.SetTagStringValue('SUBM', subm);

  fs := TFileStream.Create(SaveDialog1.FileName, fmCreate);
  try
    FTree.SaveHeaderToStream(fs);

    for i := 0 to FTree.RecordsCount - 1 do begin
      rec := TGEDCOMRecord(FTree.Records[i]);

      if (FSplitList.IndexOf(rec) >= 0)
      then rec.SaveToStream(fs);
    end;

    FTree.SaveFooterToStream(fs);

    FTree.Header.CharacterSet := csASCII;
  finally
    fs.Destroy;
  end;
end;

procedure TfmTreeTools.FormShow(Sender: TObject);
begin
  rgOperation.Left := (SheetChoice.Width - rgOperation.Width) div 2; 
  rgOperation.Top := (SheetChoice.Height - rgOperation.Height) div 2; 
end;

procedure TfmTreeTools.btnSearchClick(Sender: TObject);

  function GetIndivName(iRec: TGEDCOMIndividualRecord; var Unk: Boolean): string;
  var
    np: TGEDCOMPersonalName;
  begin
    np := iRec.PersonalNames[0];
    Result := np.StringValue;

    Unk := (Length(np.FirstPart) <= 3);
  end;

var
  i, k: Integer;
  iRec, kRec: TGEDCOMIndividualRecord;
  iName, kName: string;
  iNote, kNote: TGEDCOMNoteRecord;
  iFam, kFam: TGEDCOMFamilyRecord;
  iSrc, kSrc: TGEDCOMSourceRecord;
  res, unk: Boolean;
  year1, year2, nameAccuracy, yearInaccuracy: Integer;
  ev: TGEDCOMIndividualEvent;
begin
  nameAccuracy := udNameAccuracy.Position;
  yearInaccuracy := udYearInaccuracy.Position;

  res := False;

  try
    ProgressBar1.Min := 0;
    ProgressBar1.Max := FTree.RecordsCount;
    ProgressBar1.Position := 0;

    for i := 0 to FTree.RecordsCount - 1 do begin
      case FRMMode of
        mmPerson: if (FTree.Records[i] is TGEDCOMIndividualRecord) then begin
          iRec := FTree.Records[i] as TGEDCOMIndividualRecord;
          iName := GetIndivName(iRec, unk);
          if (unk) then Continue;

          for k := i + 1 to FTree.RecordsCount - 1 do
            if (FTree.Records[k] is TGEDCOMIndividualRecord) then begin
              kRec := FTree.Records[k] as TGEDCOMIndividualRecord;
              kName := GetIndivName(kRec, unk);
              if (unk) then Continue;

              if (FRMSkip.IndexOf(iRec.XRef + '-' + kRec.XRef) >= 0)
              or (iRec.Sex <> kRec.Sex)
              then Continue;

              if (rbDirectMatching.Checked)
              then res := (iName = kName)
              else
              if (rbIndistinctMatching.Checked)
              then res := (IndistinctMatching(4, iName, kName) > nameAccuracy);

              if (chkBirthYear.Checked) then begin
                ev := GetIndividualEvent(iRec, 'BIRT');
                year1 := TGEDCOMDate(ev.Detail.Date.Value).Year;

                ev := GetIndividualEvent(kRec, 'BIRT');
                year2 := TGEDCOMDate(ev.Detail.Date.Value).Year;

                if (year1 > 0) and (year2 > 0)
                then res := res and (Abs(year1 - year2) <= yearInaccuracy);
              end;

              if (res) then begin
                SetRec1(iRec);
                SetRec2(kRec);
                Break;
              end;
            end;
        end;

        mmNote: if (FTree.Records[i] is TGEDCOMNoteRecord) then begin
          iNote := FTree.Records[i] as TGEDCOMNoteRecord;
          iName := iNote.Notes.Text;

          for k := i + 1 to FTree.RecordsCount - 1 do
            if (FTree.Records[k] is TGEDCOMNoteRecord) then begin
              kNote := FTree.Records[k] as TGEDCOMNoteRecord;
              kName := kNote.Notes.Text;

              res := (iName = kName)
                and (FRMSkip.IndexOf(iNote.XRef + '-' + kNote.XRef) < 0);

              if (res) then begin
                SetRec1(iNote);
                SetRec2(kNote);
                Break;
              end;
            end;
        end;

        mmFamily: if (FTree.Records[i] is TGEDCOMFamilyRecord) then begin
          iFam := FTree.Records[i] as TGEDCOMFamilyRecord;
          iName := GetFamilyStr(iFam);

          for k := i + 1 to FTree.RecordsCount - 1 do
            if (FTree.Records[k] is TGEDCOMFamilyRecord) then begin
              kFam := FTree.Records[k] as TGEDCOMFamilyRecord;
              kName := GetFamilyStr(kFam);

              res := (iName = kName)
                and (FRMSkip.IndexOf(iFam.XRef + '-' + kFam.XRef) < 0);

              if (res) then begin
                SetRec1(iFam);
                SetRec2(kFam);
                Break;
              end;
            end;
        end;

        mmSource: if (FTree.Records[i] is TGEDCOMSourceRecord) then begin
          iSrc := FTree.Records[i] as TGEDCOMSourceRecord;
          iName := iSrc.FiledByEntry;

          for k := i + 1 to FTree.RecordsCount - 1 do
            if (FTree.Records[k] is TGEDCOMSourceRecord) then begin
              kSrc := FTree.Records[k] as TGEDCOMSourceRecord;
              kName := kSrc.FiledByEntry;

              res := (iName = kName)
                and (FRMSkip.IndexOf(iSrc.XRef + '-' + kSrc.XRef) < 0);

              if (res) then begin
                SetRec1(iSrc);
                SetRec2(kSrc);
                Break;
              end;
            end;
        end;
      end;

      if (res) then Break;

      ProgressBar1.StepIt();
    end;
  finally
  end;
end;

procedure TfmTreeTools.SetRec1(const Value: TGEDCOMRecord);
begin
  FRec1 := Value;

  btnMergeToLeft.Enabled := (FRec1 <> nil) and (FRec2 <> nil);
  btnMergeToRight.Enabled := (FRec1 <> nil) and (FRec2 <> nil);

  if (FRec1 = nil) then begin
    Lab1.Caption := 'XXX1';
    Edit1.Text := '';
    Memo1.Lines.Clear;
  end else begin
    Lab1.Caption := FRec1.XRef;

    case FRMMode of
      mmPerson: begin
        Edit1.Text := GetNameStr(TGEDCOMIndividualRecord(FRec1));
        Base.ShowPersonInfo(TGEDCOMIndividualRecord(FRec1), Memo1.Lines);
      end;

      mmNote: begin
        Edit1.Text := TGEDCOMNoteRecord(FRec1).Notes[0];
        Base.ShowNoteInfo(TGEDCOMNoteRecord(FRec1), Memo1.Lines);
      end;

      mmFamily: begin
        Edit1.Text := GetFamilyStr(TGEDCOMFamilyRecord(FRec1));
        Base.ShowFamilyInfo(TGEDCOMFamilyRecord(FRec1), Memo1.Lines);
      end;

      mmSource: begin
        Edit1.Text := TGEDCOMSourceRecord(FRec1).FiledByEntry;
        Base.ShowSourceInfo(TGEDCOMSourceRecord(FRec1), Memo1.Lines);
      end;
    end;
  end;
end;

procedure TfmTreeTools.SetRec2(const Value: TGEDCOMRecord);
begin
  FRec2 := Value;

  btnMergeToLeft.Enabled := (FRec1 <> nil) and (FRec2 <> nil);
  btnMergeToRight.Enabled := (FRec1 <> nil) and (FRec2 <> nil);

  if (FRec2 = nil) then begin
    Lab2.Caption := 'XXX2';
    Edit2.Text := '';
    Memo2.Lines.Clear;
  end else begin
    Lab2.Caption := FRec2.XRef;

    case FRMMode of
      mmPerson: begin
        Edit2.Text := GetNameStr(TGEDCOMIndividualRecord(FRec2));
        Base.ShowPersonInfo(TGEDCOMIndividualRecord(FRec2), Memo2.Lines);
      end;

      mmNote: begin
        Edit2.Text := TGEDCOMNoteRecord(FRec2).Notes[0];
        Base.ShowNoteInfo(TGEDCOMNoteRecord(FRec2), Memo2.Lines);
      end;

      mmFamily: begin
        Edit2.Text := GetFamilyStr(TGEDCOMFamilyRecord(FRec2));
        Base.ShowFamilyInfo(TGEDCOMFamilyRecord(FRec2), Memo2.Lines);
      end;

      mmSource: begin
        Edit2.Text := TGEDCOMSourceRecord(FRec2).FiledByEntry;
        Base.ShowSourceInfo(TGEDCOMSourceRecord(FRec2), Memo2.Lines);
      end;
    end;
  end;
end;

procedure TfmTreeTools.btnRec1SelectClick(Sender: TObject);
var
  irec: TGEDCOMRecord;
  sm: TSelectMode;
begin
  case FRMMode of
    mmPerson: sm := smPerson;
    mmNote: sm := smNote;
  end;

  irec := Base.SelectRecord(sm);
  if (irec <> nil) then SetRec1(irec);
end;

procedure TfmTreeTools.btnRec2SelectClick(Sender: TObject);
var
  irec: TGEDCOMRecord;
  sm: TSelectMode;
begin
  case FRMMode of
    mmPerson: sm := smPerson;
    mmNote: sm := smNote;
  end;

  irec := Base.SelectRecord(sm);
  if (irec <> nil) then SetRec2(irec);
end;

procedure TfmTreeTools.RecordMerge(aRecBase, aRecCopy: TGEDCOMRecord);
var
  repMap: TXRefReplaceMap;
  i: Integer;
begin
  repMap := TXRefReplaceMap.Create;
  try
    repMap.AddXRef(aRecCopy, aRecCopy.XRef, aRecBase.XRef);
    for i := 0 to FTree.RecordsCount - 1 do
      FTree.Records[i].ReplaceXRefs(repMap);

    case FRMMode of
      mmPerson: begin
        TGEDCOMIndividualRecord(aRecCopy).MoveTo(aRecBase);
        Base.DeleteIndividualRecord(TGEDCOMIndividualRecord(aRecCopy), False);
      end;

      mmNote: begin
        TGEDCOMNoteRecord(aRecCopy).MoveTo(aRecBase);
        Base.DeleteNoteRecord(TGEDCOMNoteRecord(aRecCopy), False);
      end;

      mmFamily: begin
        TGEDCOMFamilyRecord(aRecCopy).MoveTo(aRecBase);
        Base.DeleteFamilyRecord(TGEDCOMFamilyRecord(aRecCopy), False);
      end;

      mmSource: begin
        TGEDCOMSourceRecord(aRecCopy).MoveTo(aRecBase);
        Base.DeleteSourceRecord(TGEDCOMSourceRecord(aRecCopy), False);
      end;
    end;

    Base.ChangeRecord(aRecBase);
    Base.ListsRefresh();
  finally
    repMap.Free;
  end;
end;

procedure TfmTreeTools.btnMergeToLeftClick(Sender: TObject);
begin
  RecordMerge(FRec1, FRec2);
  SetRec1(FRec1);
  SetRec2(nil);
end;

procedure TfmTreeTools.btnMergeToRightClick(Sender: TObject);
begin
  RecordMerge(FRec2, FRec1);
  SetRec1(nil);
  SetRec2(FRec2);
end;

procedure TfmTreeTools.rgModeClick(Sender: TObject);
begin
  FRMMode := TMergeMode(rgMode.ItemIndex);

  btnRec1Select.Enabled := (FRMMode <> mmFamily);
  btnRec2Select.Enabled := (FRMMode <> mmFamily);
end;

procedure TfmTreeTools.btnSkipClick(Sender: TObject);
begin 
  if (FRec1 <> nil) and (FRec2 <> nil)
  then FRMSkip.Add(FRec1.XRef + '-' + FRec2.XRef);

  btnSearchClick(nil);
end;

procedure TfmTreeTools.btnImportFileChooseClick(Sender: TObject);
var
  imp: TGKImporter;
begin
  if OpenDialog2.Execute() then begin
    edImportFile.Text := OpenDialog2.FileName;

    imp := TGKImporter.Create(FTree, ListBox1.Items);
    try
      imp.TreeImportEx(edImportFile.Text);
    finally
      imp.Destroy;
    end;

    Base.ListsRefresh(False);
  end;
end;

procedure TfmTreeTools.CheckGroups();
var
  i, k, group: Integer;
  iRec: TGEDCOMIndividualRecord;
  prepared: TList;
  root, sub: TTreeNode;
  pn: string;
begin
  prepared := TList.Create;
  try
    group := 0;

    TreeView1.Items.Clear();

    ProgressInit(FTree.RecordsCount, 'Проверка связности семей');

    for i := 0 to FTree.RecordsCount - 1 do begin
      if (FTree.Records[i] is TGEDCOMIndividualRecord) then begin
        iRec := FTree.Records[i] as TGEDCOMIndividualRecord;
        if (prepared.IndexOf(iRec) >= 0) then Continue;

        Inc(group);

        FSplitList.Clear();
        TreeWalk(iRec, twmAll);

        root := TreeView1.Items.AddChild(nil, IntToStr(group) + ' группа (' + IntToStr(FSplitList.Count) + ' перс.)');
        for k := 0 to FSplitList.Count - 1 do begin
          iRec := TObject(FSplitList[k]) as TGEDCOMIndividualRecord;
          prepared.Add(iRec);

          pn := GetNameStr(iRec);
          if (iRec.FindTag(PatriarchTag) <> nil) then pn := '(*) ' + pn;

          sub := TreeView1.Items.AddChildObject(root, pn, iRec);
        end;
        root.Expand(True);
      end;

      ProgressStep();
    end;
  finally
    ProgressDone();
    FSplitList.Clear();
    prepared.Free;
  end;
end;

procedure TfmTreeTools.TreeView1DblClick(Sender: TObject);
var
  node: TTreeNode;
  i_rec: TGEDCOMIndividualRecord;
begin
  node := TreeView1.Selected;
  if (node = nil) then Exit;

  i_rec := TGEDCOMIndividualRecord(node.Data);
  if (i_rec = nil) then Exit;

  Base.SelectRecordByXRef(i_rec.XRef);
  Close;
end;

procedure TfmTreeTools.CheckDeads();
var
  i, iAge: Integer;
  iRec: TGEDCOMIndividualRecord;
  item: TListItem;
  dead_event: TGEDCOMCustomEvent;
  age: string;
begin
  try
    ProgressInit(FTree.RecordsCount, 'Проверка умерших людей');

    ListDeads.Items.Clear();
    for i := 0 to FTree.RecordsCount - 1 do begin
      ProgressStep();

      if (FTree.Records[i] is TGEDCOMIndividualRecord) then begin
        iRec := FTree.Records[i] as TGEDCOMIndividualRecord;
        dead_event := GetIndividualEvent(iRec, 'DEAT');

        if (dead_event = nil) then begin
          age := GetAge(iRec);
          if (age <> '') and (age <> '?') then begin
            iAge := StrToInt(age);
            if (iAge >= 100) then begin
              item := ListDeads.Items.Add();
              item.Caption := GetNameStr(iRec);
              item.SubItems.Add('Возраст = ' + age);
              item.Data := iRec;
            end;
          end;
        end;
      end;
    end;
  finally
    ProgressDone();
  end;
end;

procedure TfmTreeTools.btnPatSearchClick(Sender: TObject);
var
  lst: TObjectList;

  function GetLinks(pObj: TPatriarchObj): string;
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

var
  i: Integer;
  p_obj: TPatriarchObj;
  item: TListItem;
  p_tag: TGEDCOMTag;
  p_sign: string;
begin
  ListPatriarchs.Items.BeginUpdate();
  lst := TObjectList.Create(True);
  try
    ListPatriarchs.Clear();
    GetPatriarchsList(FTree, True, False, lst);

    for i := 0 to lst.Count - 1 do begin
      p_obj := TPatriarchObj(lst[i]);

      p_tag := p_obj.IRec.FindTag(PatriarchTag);
      if (p_tag = nil) then p_sign := '' else p_sign := '[*] ';      

      item := ListPatriarchs.Items.Add();
      item.Caption := p_sign + GetNameStr(p_obj.IRec);
      item.SubItems.Add(IntToStr(p_obj.IBirthYear));
      item.SubItems.Add(IntToStr(p_obj.IDescendantsCount));
      item.SubItems.Add(IntToStr(p_obj.IDescGenerations));
      item.Data := p_obj.IRec;
      //item.SubItems.Add(GetLinks(p_obj));
    end;
  finally
    lst.Destroy;
    ListPatriarchs.Items.EndUpdate();
  end;
end;

procedure TfmTreeTools.ListPatriarchsDblClick(Sender: TObject);
var
  item: TListItem;
  i_rec: TGEDCOMIndividualRecord;
begin
  item := ListPatriarchs.Selected;
  if (item = nil) then Exit;

  i_rec := TGEDCOMIndividualRecord(item.Data);
  if (i_rec = nil) then Exit;

  Base.SelectRecordByXRef(i_rec.XRef);
  Close;
end;

procedure TfmTreeTools.PreparePatriarchsList();
begin
  Base.CreateListView(Self, Panel3, ListPatriarchs);
  //ListPatriarchs.Checkboxes := True;
  ListPatriarchs.OnDblClick := ListPatriarchsDblClick;
  AddListColumn(ListPatriarchs, 'Патриарх', 400);
  AddListColumn(ListPatriarchs, 'Родился', 90);
  AddListColumn(ListPatriarchs, 'Потомков', 90);
  AddListColumn(ListPatriarchs, 'Поколений', 90);
end;

procedure TfmTreeTools.btnPrepareClick(Sender: TObject);
var
  i: Integer;
  item: TListItem;
  iRec: TGEDCOMIndividualRecord;
begin
  try
    for i := 0 to ListDeads.Items.Count - 1 do begin
      item := ListDeads.Items[i];

      if item.Checked then begin
        iRec := TGEDCOMIndividualRecord(item.Data);
        CreateIEvent(FTree, iRec, 'DEAT', '', '');
        Base.ChangeRecord(iRec);
      end;
    end;
  finally
    Base.ListsRefresh();
    CheckDeads();
  end;
end;

function TfmTreeTools.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmTreeTools.PrepareDeadsList();
begin
  Base.CreateListView(Self, Panel1, ListDeads);
  ListDeads.Checkboxes := True;
  AddListColumn(ListDeads, 'Персона', 400);
  AddListColumn(ListDeads, 'Возможная причина смерти', 200);
end;

procedure TfmTreeTools.btnUpdateSelectClick(Sender: TObject);
begin
  if OpenDialog1.Execute() then begin
    edUpdateBase.Text := OpenDialog1.FileName;
    Base.GenBase.TreeSync(edUpdateBase.Text, mSyncRes.Lines);
    Base.ListsRefresh();
  end;
end;

end.
