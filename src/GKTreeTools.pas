unit GKTreeTools;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, GedCom551, StdCtrls, Buttons, ExtCtrls, GKImport, bsCtrls;

type
  TTreeWalkMode = (twmAll, twmFamily, twmAncestors, twmDescendants, twmNone);

  TMergeMode = (mmPerson, mmNote, mmFamily);

  TfmTreeTools = class(TForm)
    PageControl: TPageControl;
    SheetChoice: TTabSheet;
    SheetTreeCompare: TTabSheet;
    ListErrors: TBSListView;
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
    btnGroup: TBitBtn;
    ProgressBar2: TProgressBar;
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
    procedure btnGroupClick(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
  private
    FSplitCounter: Integer;
    FSplitList: TList;
    FTree: TGEDCOMTree;

    FRec1, FRec2: TGEDCOMRecordWithLists;
    FRMMode: TMergeMode;
    FRMSkip: TStringList;
    procedure RecordMerge(aRecBase, aRecCopy: TGEDCOMRecordWithLists);
    procedure SetRec1(const Value: TGEDCOMRecordWithLists);
    procedure SetRec2(const Value: TGEDCOMRecordWithLists);

    procedure TreeWalk(iRec: TGEDCOMIndividualRecord; aMode: TTreeWalkMode);
    procedure Select(aPerson: TGEDCOMIndividualRecord; aMode: TTreeWalkMode);
    procedure CheckRelations();

    procedure AddDiag(aObj, aDiag: string);
    procedure TreeMerge(aMainTree: TGEDCOMTree; aFileName: string);
    procedure TreeCompare(aMainTree: TGEDCOMTree; aFileName: string);
  public
  end;

implementation

uses GKCommon, GKMain, GKRecordSelect;

{$R *.dfm}

{ TfmTreeWizard }

procedure TfmTreeTools.FormCreate(Sender: TObject);
begin
  FTree := fmGEDKeeper.FTree;
  PageControl.ActivePageIndex := 0;

  FSplitList := TList.Create;

  FRMSkip := TStringList.Create;
  SetRec1(nil);
  SetRec2(nil);
  FRMMode := mmPerson;
  rgMode.ItemIndex := 0;
end;

procedure TfmTreeTools.FormDestroy(Sender: TObject);
begin
  FRMSkip.Destroy;

  FSplitList.Destroy;
end;

procedure TfmTreeTools.AddDiag(aObj, aDiag: string);
var
  item: TListItem;
begin
  item := ListErrors.Items.Add();
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
  i, idx: Integer;
  iRec: TGEDCOMIndividualRecord;
  fams, names: TStringList;
  fam, nam, pat, tm: string;
begin
  tempTree := TGEDCOMTree.Create;
  tempTree.LoadFromFile(aFileName);
  fams := TStringList.Create;
  names := TStringList.Create;
  try
    AddDiag('', 'Поиск совпадений...');

    for i := 0 to aMainTree.Count - 1 do
      if (aMainTree.Records[i] is TGEDCOMIndividualRecord) then begin
        iRec := aMainTree.Records[i] as TGEDCOMIndividualRecord;
        names.AddObject(GetNameStr(iRec), nil);

        GetNameParts(iRec, fam, nam, pat);
        fams.AddObject(PrepareRusFamily(fam, (iRec.Sex = svFemale)), nil);
      end;

    for i := 0 to tempTree.Count - 1 do
      if (tempTree.Records[i] is TGEDCOMIndividualRecord) then begin
        iRec := tempTree.Records[i] as TGEDCOMIndividualRecord;
        tm := GetNameStr(iRec);

        idx := names.IndexOf(tm);
        if (idx >= 0)
        then names.Objects[idx] := TObject(1);

        GetNameParts(iRec, fam, nam, pat);
        tm := PrepareRusFamily(fam, (iRec.Sex = svFemale));

        idx := fams.IndexOf(tm);
        if (idx >= 0)
        then fams.Objects[idx] := TObject(1);
      end;

    for i := fams.Count - 1 downto 0 do
      if (fams.Objects[i] = nil) or (fams[i] = '?') then fams.Delete(i);

    for i := names.Count - 1 downto 0 do
      if (names.Objects[i] = nil) then names.Delete(i);

    AddDiag('', 'Схожие фамилии:');
    if (fams.Count <> 0) then begin
      for i := 0 to fams.Count - 1 do AddDiag('', '    ' + fams[i]);
    end else AddDiag('', '    нет.');

    AddDiag('', 'Схожие имена:');
    if (names.Count <> 0) then begin
      for i := 0 to names.Count - 1 do AddDiag('', '    ' + names[i]);
    end else AddDiag('', '    нет.');
  finally
    names.Destroy;
    fams.Destroy;
    tempTree.Destroy;
  end;
end;

procedure TfmTreeTools.TreeMerge(aMainTree: TGEDCOMTree; aFileName: string);
var
  repMap: TXRefReplaceMap;
  i: Integer;
  extTree: TGEDCOMTree;
  rec: TGEDCOMRecord;
  newXRef: string;
begin
  extTree := TGEDCOMTree.Create;
  repMap := TXRefReplaceMap.Create;
  try
    extTree.LoadFromFile(aFileName);
    while (extTree.Count > 0) do begin
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
  finally
    repMap.Destroy;
    extTree.Destroy;
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

    old_count := FTree.Count;
    mRes.Lines.Add('Количество объектов в основной базе: ' + IntToStr(old_count));

    TreeMerge(FTree, edMergeFile.Text);

    new_count := FTree.Count;
    mRes.Lines.Add('Новое количество объектов в основной базе: ' + IntToStr(new_count));

    fmGEDKeeper.ListsRefresh();
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
    for i := 0 to FTree.Count - 1 do
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
    if (FSplitList.IndexOf(aRec) < 0) then FSplitList.Add(aRec);
  end;

  procedure CheckRecord(rec: TGEDCOMRecordWithLists);
  var
    i: Integer;
  begin
    for i := 0 to rec.MultimediaLinksCount - 1 do AddRel(rec.MultimediaLinks[i].Value);
    for i := 0 to rec.NotesCount - 1 do AddRel(rec.Notes[i].Value);
    for i := 0 to rec.SourceCitationsCount - 1 do AddRel(rec.SourceCitations[i].Value);
  end;

  procedure CheckTag(tag: TGEDCOMTagWithListsEx);
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

    for i := 0 to iRec.IndividualEventsCount - 1 do CheckTag(TGEDCOMTagWithListsEx(iRec.IndividualEvents[i].Detail));
    for i := 0 to iRec.IndividualAttributesCount - 1 do CheckTag(TGEDCOMTagWithListsEx(iRec.IndividualAttributes[i].Detail));
    for i := 0 to iRec.IndividualOrdinancesCount - 1 do CheckTag(TGEDCOMTagWithListsEx(iRec.IndividualOrdinances[i]));

    for i := 0 to iRec.SubmittorsCount - 1 do AddRel(iRec.Submittors[i].Value);
    for i := 0 to iRec.AssociationsCount - 1 do AddRel(iRec.Associations[i].Value);
    for i := 0 to iRec.AliassesCount - 1 do AddRel(iRec.Aliasses[i].Value);

    for i := 0 to iRec.AncestorsInterestCount - 1 do AddRel(iRec.AncestorsInterest[i].Value);
    for i := 0 to iRec.DescendantsInterestCount - 1 do AddRel(iRec.DescendantsInterest[i].Value);
    // UserReferencesCount

    for i := 0 to iRec.GroupsCount - 1 do AddRel(iRec.Groups[i].Value);
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
    then
    else
    if (rec is TGEDCOMIndividualRecord)
    then CheckIndividual(rec as TGEDCOMIndividualRecord)
    else
    if (rec is TGEDCOMMultimediaRecord)
    then
    else
    if (rec is TGEDCOMNoteRecord)
    then
    else
    if (rec is TGEDCOMRepositoryRecord)
    then
    else
    if (rec is TGEDCOMSourceRecord)
    then
    else
    if (rec is TGEDCOMSubmissionRecord)
    then
    else
    if (rec is TGEDCOMSubmitterRecord)
    then
    else
    if (rec is TGEDCOMGroupRecord)
    then ;

    Inc(i);
  end;
end;

procedure TfmTreeTools.btnSelectAllClick(Sender: TObject);
begin
  Select(fmGEDKeeper.GetSelectedPerson(), twmAll);
end;

procedure TfmTreeTools.btnSelectFamilyClick(Sender: TObject);
begin
  Select(fmGEDKeeper.GetSelectedPerson(), twmFamily);
end;

procedure TfmTreeTools.btnSelectAncestorsClick(Sender: TObject);
begin
  Select(fmGEDKeeper.GetSelectedPerson(), twmAncestors);
end;

procedure TfmTreeTools.btnSelectDescendantsClick(Sender: TObject);
begin
  Select(fmGEDKeeper.GetSelectedPerson(), twmDescendants);
end;

procedure TfmTreeTools.btnDeleteClick(Sender: TObject);
var
  i: Integer;
  p: TGEDCOMIndividualRecord;
begin
  for i := 0 to FSplitList.Count - 1 do begin
    p := TGEDCOMIndividualRecord(FSplitList[i]);
    fmGEDKeeper.DeleteIndividualRecord(p);
  end;

  fmGEDKeeper.ListsRefresh();
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

    for i := 0 to FTree.Count - 1 do begin
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
var
  i, k: Integer;
  iRec, kRec: TGEDCOMIndividualRecord;
  iName, kName, f, n, p: string;
  iNote, kNote: TGEDCOMNoteRecord;
  iFam, kFam: TGEDCOMFamilyRecord;
  res: Boolean;
  year1, year2, nameAccuracy, yearInaccuracy: Integer;
  ev: TGEDCOMIndividualEvent;
begin //
  nameAccuracy := udNameAccuracy.Position;
  yearInaccuracy := udYearInaccuracy.Position;

  res := False;

  try
    ProgressBar1.Min := 0;
    ProgressBar1.Max := FTree.Count;
    ProgressBar1.Position := 0;

    for i := 0 to FTree.Count - 1 do begin
      case FRMMode of
        mmPerson: if (FTree.Records[i] is TGEDCOMIndividualRecord) then begin
          iRec := FTree.Records[i] as TGEDCOMIndividualRecord;

          GetNameParts(iRec, f, n, p);
          if (Length(n) < 3) then Continue;
          iName := GetNameStr(iRec);

          for k := i + 1 to FTree.Count - 1 do
            if (FTree.Records[k] is TGEDCOMIndividualRecord) then begin
              kRec := FTree.Records[k] as TGEDCOMIndividualRecord;

              GetNameParts(kRec, f, n, p);
              if (Length(n) < 3) then Continue;
              kName := GetNameStr(kRec);

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

          for k := i + 1 to FTree.Count - 1 do
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

          for k := i + 1 to FTree.Count - 1 do
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
      end;

      if (res) then Break;

      ProgressBar1.StepIt();
    end;
  finally
  end;
end;

procedure TfmTreeTools.SetRec1(const Value: TGEDCOMRecordWithLists);
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
        fmGEDKeeper.ShowPersonInfo(TGEDCOMIndividualRecord(FRec1), Memo1.Lines);
      end;

      mmNote: begin
        Edit1.Text := TGEDCOMNoteRecord(FRec1).Notes[0];
        fmGEDKeeper.ShowNoteInfo(TGEDCOMNoteRecord(FRec1), Memo1.Lines);
      end;

      mmFamily: begin
        Edit1.Text := GetFamilyStr(TGEDCOMFamilyRecord(FRec1));
        fmGEDKeeper.ShowFamilyInfo(TGEDCOMFamilyRecord(FRec1), Memo1.Lines);
      end;
    end;
  end;
end;

procedure TfmTreeTools.SetRec2(const Value: TGEDCOMRecordWithLists);
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
        fmGEDKeeper.ShowPersonInfo(TGEDCOMIndividualRecord(FRec2), Memo2.Lines);
      end;

      mmNote: begin
        Edit2.Text := TGEDCOMNoteRecord(FRec2).Notes[0];
        fmGEDKeeper.ShowNoteInfo(TGEDCOMNoteRecord(FRec2), Memo2.Lines);
      end;

      mmFamily: begin
        Edit2.Text := GetFamilyStr(TGEDCOMFamilyRecord(FRec2));
        fmGEDKeeper.ShowFamilyInfo(TGEDCOMFamilyRecord(FRec2), Memo2.Lines);
      end;
    end;
  end;
end;

procedure TfmTreeTools.btnRec1SelectClick(Sender: TObject);
var
  irec: TGEDCOMRecordWithLists;
  sm: TSelectMode;
begin //
  case FRMMode of
    mmPerson: sm := smPerson;
    mmNote: sm := smNote;
  end;

  irec := SelectRecord(sm);
  if (irec <> nil) then SetRec1(irec);
end;

procedure TfmTreeTools.btnRec2SelectClick(Sender: TObject);
var
  irec: TGEDCOMRecordWithLists;
  sm: TSelectMode;
begin //
  case FRMMode of
    mmPerson: sm := smPerson;
    mmNote: sm := smNote;
  end;

  irec := SelectRecord(sm);
  if (irec <> nil) then SetRec2(irec);
end;

procedure TfmTreeTools.RecordMerge(aRecBase, aRecCopy: TGEDCOMRecordWithLists);
var
  repMap: TXRefReplaceMap;
  i: Integer;
begin
  repMap := TXRefReplaceMap.Create;
  try
    repMap.AddXRef(aRecCopy, aRecCopy.XRef, aRecBase.XRef);
    for i := 0 to FTree.Count - 1 do
      FTree.Records[i].ReplaceXRefs(repMap);

    case FRMMode of
      mmPerson: begin
        TGEDCOMIndividualRecord(aRecCopy).MoveTo(aRecBase);
        fmGEDKeeper.DeleteIndividualRecord(TGEDCOMIndividualRecord(aRecCopy));
      end;

      mmNote: begin
        TGEDCOMNoteRecord(aRecCopy).MoveTo(aRecBase);
        fmGEDKeeper.DeleteNoteRecord(TGEDCOMNoteRecord(aRecCopy));
      end;

      mmFamily: begin
        TGEDCOMFamilyRecord(aRecCopy).MoveTo(aRecBase);
        fmGEDKeeper.DeleteFamily(TGEDCOMFamilyRecord(aRecCopy));
      end;
    end;

    fmGEDKeeper.ListsRefresh();
  finally
    repMap.Destroy;
  end;
end;

procedure TfmTreeTools.btnMergeToLeftClick(Sender: TObject);
begin //
  RecordMerge(FRec1, FRec2);
  SetRec1(FRec1);
  SetRec2(nil);
end;

procedure TfmTreeTools.btnMergeToRightClick(Sender: TObject);
begin //
  RecordMerge(FRec2, FRec1);
  SetRec1(nil);
  SetRec2(FRec2);
end;

procedure TfmTreeTools.rgModeClick(Sender: TObject);
begin //
  FRMMode := TMergeMode(rgMode.ItemIndex);

  btnRec1Select.Enabled := (FRMMode <> mmFamily);
  btnRec2Select.Enabled := (FRMMode <> mmFamily);
end;

procedure TfmTreeTools.btnSkipClick(Sender: TObject);
begin //
  if (FRec1 <> nil) and (FRec2 <> nil)
  then FRMSkip.Add(FRec1.XRef + '-' + FRec2.XRef);

  btnSearchClick(nil);
end;

procedure TfmTreeTools.btnImportFileChooseClick(Sender: TObject);
begin
  if OpenDialog2.Execute() then begin
    edImportFile.Text := OpenDialog2.FileName;

    TreeImportEx(FTree, edImportFile.Text, TFileType(OpenDialog2.FilterIndex - 1), ListBox1.Items);

    fmGEDKeeper.ListsRefresh(False);
  end;
end;

procedure TfmTreeTools.btnGroupClick(Sender: TObject);
var
  i, k, group: Integer;
  iRec: TGEDCOMIndividualRecord;
  prepared: TList;
  root, sub: TTreeNode;
begin
  prepared := TList.Create;
  try
    group := 0;

    TreeView1.Items.Clear();

    ProgressBar2.Position := 0;
    ProgressBar2.Max := FTree.Count;
    for i := 0 to FTree.Count - 1 do begin
      ProgressBar2.Position := i + 1;

      if (FTree.Records[i] is TGEDCOMIndividualRecord) then begin
        iRec := FTree.Records[i] as TGEDCOMIndividualRecord;
        if (prepared.IndexOf(iRec) >= 0) then Continue;

        Inc(group);

        FSplitList.Clear();
        TreeWalk(iRec, twmAll);

        root := TreeView1.Items.AddChild(nil, IntToStr(group));
        for k := 0 to FSplitList.Count - 1 do begin
          iRec := TObject(FSplitList[k]) as TGEDCOMIndividualRecord;
          prepared.Add(iRec);
          sub := TreeView1.Items.AddChildObject(root, GetNameStr(iRec), iRec);
        end;
        root.Expand(True);
      end;
    end;
  finally
    ProgressBar2.Position := 0;
    FSplitList.Clear();

    prepared.Destroy;
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

  fmGEDKeeper.SelectRecordByXRef(i_rec.XRef);
  Close;
end;

end.
