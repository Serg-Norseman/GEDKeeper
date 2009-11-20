unit GKSplitBase;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GedCom551, Buttons;

type
  TTreeWalkMode = (twmAll, twmFamily, twmAncestors, twmDescendants, twmNone);

  TfmSplitBase = class(TForm)
    btnSelectAll: TBitBtn;
    ListSelected: TListBox;
    ListSkipped: TListBox;
    btnClose: TBitBtn;
    btnSelectFamily: TBitBtn;
    btnSelectAncestors: TBitBtn;
    btnSelectDescendants: TBitBtn;
    btnDelete: TBitBtn;
    btnSave: TBitBtn;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnSelectFamilyClick(Sender: TObject);
    procedure btnSelectAncestorsClick(Sender: TObject);
    procedure btnSelectDescendantsClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    FCounter: Integer;
    FList: TList;
    FTree: TGEDCOMTree;

    procedure TreeWalk(iRec: TGEDCOMIndividualRecord; aMode: TTreeWalkMode);
    procedure Select(aPerson: TGEDCOMIndividualRecord; aMode: TTreeWalkMode);
    procedure CheckRelations();
  public
  end;

implementation

uses GKMain, GKCommon;

{$R *.dfm}

procedure TfmSplitBase.TreeWalk(iRec: TGEDCOMIndividualRecord; aMode: TTreeWalkMode);
var
  rel_person: TGEDCOMIndividualRecord;
  sp: TGEDCOMPointer;
  family: TGEDCOMFamilyRecord;
  i, k: Integer;
  int_mode: TTreeWalkMode; // twmAll, twmFamily, twmAncestors, twmDescendants, twmNone
begin
  if (iRec = nil) then Exit;
  if (FList.IndexOf(iRec) >= 0) then Exit;

  Inc(FCounter);
  FList.Add(iRec);

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

procedure TfmSplitBase.Select(aPerson: TGEDCOMIndividualRecord; aMode: TTreeWalkMode);
var
  i, cnt: Integer;
  i_rec: TGEDCOMIndividualRecord;
begin
  FCounter := 0;

  ListSelected.Items.BeginUpdate;
  ListSelected.Items.Clear;

  ListSkipped.Items.BeginUpdate;
  ListSkipped.Items.Clear;

  try
    FList.Clear;
    FTree := fmGEDKeeper.FTree;
    i_rec := aPerson;

    TreeWalk(i_rec, aMode);

    cnt := 0;
    for i := 0 to FTree.Count - 1 do
      if (FTree.Records[i] is TGEDCOMIndividualRecord) then begin
        Inc(cnt);

        i_rec := (FTree.Records[i] as TGEDCOMIndividualRecord);

        if (FList.IndexOf(i_rec) < 0)
        then ListSkipped.Items.Add(i_rec.XRef + ' / ' + GetNameStr(i_rec))
        else ListSelected.Items.Add(i_rec.XRef + ' / ' + GetNameStr(i_rec));
      end;

    Caption := IntToStr(FList.Count) + ' / ' + IntToStr(cnt);
  finally
    ListSelected.Items.EndUpdate;
    ListSkipped.Items.EndUpdate;
  end;
end;

procedure TfmSplitBase.FormCreate(Sender: TObject);
begin
  FList := TList.Create;
end;

procedure TfmSplitBase.FormDestroy(Sender: TObject);
begin
  FList.Destroy;
end;

procedure TfmSplitBase.btnDeleteClick(Sender: TObject);
var
  i: Integer;
  p: TGEDCOMIndividualRecord;
begin
  for i := 0 to FList.Count - 1 do begin
    p := TGEDCOMIndividualRecord(FList[i]);
    fmGEDKeeper.DeleteIndividualRecord(p);
  end;

  fmGEDKeeper.ListsRefresh();
end;

procedure TfmSplitBase.btnSelectAllClick(Sender: TObject);
begin
  Select(fmGEDKeeper.GetSelectedPerson(), twmAll);
end;

procedure TfmSplitBase.btnSelectFamilyClick(Sender: TObject);
begin
  Select(fmGEDKeeper.GetSelectedPerson(), twmFamily);
end;

procedure TfmSplitBase.btnSelectAncestorsClick(Sender: TObject);
begin
  Select(fmGEDKeeper.GetSelectedPerson(), twmAncestors);
end;

procedure TfmSplitBase.btnSelectDescendantsClick(Sender: TObject);
begin
  Select(fmGEDKeeper.GetSelectedPerson(), twmDescendants);
end;

procedure TfmSplitBase.CheckRelations();

  procedure AddRel(aRec: TGEDCOMRecord);
  begin
    if (FList.IndexOf(aRec) < 0) then FList.Add(aRec);
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
  while (i < FList.Count) do begin
    rec := TGEDCOMRecord(FList[i]);

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

procedure TfmSplitBase.btnSaveClick(Sender: TObject);
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
  FTree.Header.CharacterSet := fmGEDKeeper.FDefCharacterSet;
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

    for i := 0 to FList.Count - 1 do begin
      rec := TGEDCOMRecord(FList[i]);
      rec.SaveToStream(fs);
    end;

    FTree.SaveFooterToStream(fs);

    FTree.Header.CharacterSet := csASCII;
  finally
    fs.Destroy;
  end;
end;

end.
