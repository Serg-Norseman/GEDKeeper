unit GKSplitBase;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GedCom551, Buttons;

type
  TTreeWalkMode = set of (twmAll, twmAncestors, twmDescendants);

  TfmSplitBase = class(TForm)
    btnSelectAll: TBitBtn;
    ListBox1: TListBox;
    ListBox2: TListBox;
    btnClose: TBitBtn;
    btnSelectFamily: TBitBtn;
    btnSelectAncestors: TBitBtn;
    btnSelectDescendants: TBitBtn;
    btnDelete: TBitBtn;
    procedure btnSelectAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    FCounter: Integer;
    FList: TList;
    FTree: TGEDCOMTree;
  public
    procedure TreeWalk(iRec: TGEDCOMIndividualRecord);
  end;

implementation

uses GKMain, GKCommon;

{$R *.dfm}

procedure TfmSplitBase.btnSelectAllClick(Sender: TObject);
var
  p: TGEDCOMIndividualRecord;
  i, cnt: Integer;
begin
  FCounter := 0;

  try
    FList.Clear;
    FTree := fmGEDKeeper.FTree;
    p := fmGEDKeeper.GetSelectedPerson();

    TreeWalk(p);

    cnt := 0;
    for i := 0 to FTree.Count - 1 do
      if (FTree.Records[i] is TGEDCOMIndividualRecord) then begin
        Inc(cnt);

        p := (FTree.Records[i] as TGEDCOMIndividualRecord);

        if (FList.IndexOf(p) < 0)
        then ListBox2.Items.Add(p.XRef + ' / ' + GetNameStr(p));
      end;

    Caption := IntToStr(FList.Count) + ' / ' + IntToStr(cnt);
  finally
  end;
end;

procedure TfmSplitBase.TreeWalk(iRec: TGEDCOMIndividualRecord);
var
  rel_person: TGEDCOMIndividualRecord;
  sp: TGEDCOMPointer;
  family: TGEDCOMFamilyRecord;
  i, k: Integer;
begin
  if (iRec = nil) then Exit;
  if (FList.IndexOf(iRec) >= 0) then Exit;

  Inc(FCounter);
  FList.Add(iRec);
  ListBox1.Items.Add(iRec.XRef + ' / ' + GetNameStr(iRec));

  if (iRec.ChildToFamilyLinksCount <> 0) then begin
    family := iRec.ChildToFamilyLinks[0].Family;

    rel_person := TGEDCOMIndividualRecord(family.Husband.Value);
    TreeWalk(rel_person);

    rel_person := TGEDCOMIndividualRecord(family.Wife.Value);
    TreeWalk(rel_person);
  end;

  for i := 0 to iRec.SpouseToFamilyLinksCount - 1 do begin
    family := iRec.SpouseToFamilyLinks[i].Family;

    if (iRec.Sex = svMale)
    then sp := family.Wife
    else sp := family.Husband;

    rel_person := TGEDCOMIndividualRecord(sp.Value);
    TreeWalk(rel_person);

    for k := 0 to family.ChildrenCount - 1 do begin
      rel_person := TGEDCOMIndividualRecord(family.Children[k].Value);
      TreeWalk(rel_person);
    end;
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

end.
