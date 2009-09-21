unit GKMerge;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GedCom551, ComCtrls, ExtCtrls;

type
  TMergeMode = (mmPerson, mmNote, mmFamily);

  TfmMerge = class(TForm)
    btnClose: TBitBtn;
    PageControl1: TPageControl;
    SheetMerge: TTabSheet;
    SheetOptions: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    btnSearch: TBitBtn;
    Edit1: TEdit;
    Edit2: TEdit;
    btnRec1Select: TBitBtn;
    btnRec2Select: TBitBtn;
    Memo1: TMemo;
    Memo2: TMemo;
    btnMergeToLeft: TBitBtn;
    btnMergeToRight: TBitBtn;
    rgMode: TRadioGroup;
    btnSkip: TBitBtn;
    procedure btnSearchClick(Sender: TObject);
    procedure btnRec1SelectClick(Sender: TObject);
    procedure btnRec2SelectClick(Sender: TObject);
    procedure btnMergeToLeftClick(Sender: TObject);
    procedure btnMergeToRightClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure rgModeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSkipClick(Sender: TObject);
  private
    FTree: TGEDCOMTree;
    FRec1, FRec2: TGEDCOMRecordWithLists;
    FMode: TMergeMode;
    FSkip: TStringList;
    procedure Merge(aRecBase, aRecCopy: TGEDCOMRecordWithLists);
    procedure SetRec1(const Value: TGEDCOMRecordWithLists);
    procedure SetRec2(const Value: TGEDCOMRecordWithLists);
    procedure SetMode(const Value: TMergeMode);
  public
    property Rec1: TGEDCOMRecordWithLists read FRec1 write SetRec1;
    property Rec2: TGEDCOMRecordWithLists read FRec2 write SetRec2;
    property Mode: TMergeMode read FMode write SetMode;
  end;

implementation

uses GKMain, GKCommon, GKRecordSelect;

{$R *.dfm}

procedure TfmMerge.btnSearchClick(Sender: TObject);
var
  i, k: Integer;
  iRec, kRec: TGEDCOMIndividualRecord;
  iName, kName: string;
  iNote, kNote: TGEDCOMNoteRecord;
  iFam, kFam: TGEDCOMFamilyRecord;
begin
  try
    for i := 0 to FTree.Count - 1 do begin
      case FMode of
        mmPerson: if (FTree.Records[i] is TGEDCOMIndividualRecord) then begin
          iRec := FTree.Records[i] as TGEDCOMIndividualRecord;
          iName := GetNameStr(iRec);

          for k := i + 1 to FTree.Count - 1 do
            if (FTree.Records[k] is TGEDCOMIndividualRecord) then begin
              kRec := FTree.Records[k] as TGEDCOMIndividualRecord;
              kName := GetNameStr(kRec);

              if (iRec.Sex = kRec.Sex) and (iName = kName)
              and (FSkip.IndexOf(iRec.XRef + '-' + kRec.XRef) < 0) then begin
                Rec1 := iRec;
                Rec2 := kRec;
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

              if (iName = kName)
              and (FSkip.IndexOf(iNote.XRef + '-' + kNote.XRef) < 0)  then begin
                Rec1 := iNote;
                Rec2 := kNote;
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

              if (iName = kName)
              and (FSkip.IndexOf(iFam.XRef + '-' + kFam.XRef) < 0)  then begin
                Rec1 := iFam;
                Rec2 := kFam;
                Break;
              end;
            end;
        end;
      end;
    end;
  finally
  end;
end;

procedure TfmMerge.SetRec1(const Value: TGEDCOMRecordWithLists);
begin
  FRec1 := Value;

  btnMergeToLeft.Enabled := (FRec1 <> nil) and (FRec2 <> nil);
  btnMergeToRight.Enabled := (FRec1 <> nil) and (FRec2 <> nil);

  if (FRec1 = nil) then begin
    Label1.Caption := 'XXX1';
    Edit1.Text := '';
    Memo1.Lines.Clear;
  end else begin
    Label1.Caption := FRec1.XRef;

    case FMode of
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

procedure TfmMerge.SetRec2(const Value: TGEDCOMRecordWithLists);
begin
  FRec2 := Value;

  btnMergeToLeft.Enabled := (FRec1 <> nil) and (FRec2 <> nil);
  btnMergeToRight.Enabled := (FRec1 <> nil) and (FRec2 <> nil);

  if (FRec2 = nil) then begin
    Label2.Caption := 'XXX2';
    Edit2.Text := '';
    Memo2.Lines.Clear;
  end else begin
    Label2.Caption := FRec2.XRef;

    case FMode of
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

procedure TfmMerge.btnRec1SelectClick(Sender: TObject);
var
  irec: TGEDCOMRecordWithLists;
  sm: TSelectMode;
begin
  case FMode of
    mmPerson: sm := smPerson;
    mmNote: sm := smNote;
  end;

  irec := SelectRecord(sm);
  if (irec <> nil) then Rec1 := irec;
end;

procedure TfmMerge.btnRec2SelectClick(Sender: TObject);
var
  irec: TGEDCOMRecordWithLists;
  sm: TSelectMode;
begin
  case FMode of
    mmPerson: sm := smPerson;
    mmNote: sm := smNote;
  end;

  irec := SelectRecord(sm);
  if (irec <> nil) then Rec2 := irec;
end;

procedure TfmMerge.Merge(aRecBase, aRecCopy: TGEDCOMRecordWithLists);
var
  repMap: TXRefReplaceMap;
  i: Integer;
begin
  repMap := TXRefReplaceMap.Create;
  try
    repMap.AddXRef(aRecCopy, aRecCopy.XRef, aRecBase.XRef);
    for i := 0 to FTree.Count - 1 do
      FTree.Records[i].ReplaceXRefs(repMap);

    case FMode of
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

procedure TfmMerge.btnMergeToLeftClick(Sender: TObject);
begin
  Merge(FRec1, FRec2);
  Rec1 := FRec1;
  Rec2 := nil;
end;

procedure TfmMerge.btnMergeToRightClick(Sender: TObject);
begin
  Merge(FRec2, FRec1);
  Rec1 := nil;
  Rec2 := FRec2;
end;

procedure TfmMerge.FormCreate(Sender: TObject);
begin
  FSkip := TStringList.Create;
  FTree := fmGEDKeeper.FTree;
  Rec1 := nil;
  Rec2 := nil;
  FMode := mmPerson;
  rgMode.ItemIndex := 0;
end;

procedure TfmMerge.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmMerge.SetMode(const Value: TMergeMode);
begin
  FMode := Value;
end;

procedure TfmMerge.rgModeClick(Sender: TObject);
begin
  Mode := TMergeMode(rgMode.ItemIndex);

  btnRec1Select.Enabled := (Mode <> mmFamily);
  btnRec2Select.Enabled := (Mode <> mmFamily);
end;

procedure TfmMerge.FormDestroy(Sender: TObject);
begin
  FSkip.Destroy;
end;

procedure TfmMerge.btnSkipClick(Sender: TObject);
begin
  if (FRec1 <> nil) and (FRec2 <> nil)
  then FSkip.Add(FRec1.XRef + '-' + FRec2.XRef);

  btnSearchClick(nil);
end;

end.
