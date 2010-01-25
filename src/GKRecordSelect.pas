unit GKRecordSelect;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, StdCtrls,
  GedCom551, GKCommon, Buttons, bsCtrls;

type
  TfmRecordSelect = class(TForm)
    ListRecords: TBSListView;
    btnFind: TBitBtn;
    btnSelect: TBitBtn;
    btnCreate: TBitBtn;
    btnCancel: TBitBtn;
    procedure btnSelectClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
    FMode: TSelectMode;
    FTarget: TGEDCOMIndividualRecord;
    FTargetMode: TTargetMode;
    FNeedSex: TGEDCOMSex;
    ResultRecord: TGEDCOMRecordWithLists;
  public
  end;

function SelectRecord(aMode: TSelectMode): TGEDCOMRecordWithLists;

function SelectPerson(aTarget: TGEDCOMIndividualRecord; aTargetMode: TTargetMode;
  aNeedSex: TGEDCOMSex): TGEDCOMIndividualRecord;

implementation

uses GKMain;

{$R *.dfm}

function SelectPerson(aTarget: TGEDCOMIndividualRecord; aTargetMode: TTargetMode;
  aNeedSex: TGEDCOMSex): TGEDCOMIndividualRecord;
var
  fmRecordSelect: TfmRecordSelect;
begin
  fmRecordSelect := TfmRecordSelect.Create(nil);

  try
    Result := nil;

    fmRecordSelect.FMode := smPerson;
    fmRecordSelect.FTarget := aTarget;
    fmRecordSelect.FNeedSex := aNeedSex;
    fmRecordSelect.FTargetMode := aTargetMode;

    fmGEDKeeper.Filter.Backup();
    fmGEDKeeper.Filter.Clear();
    fmGEDKeeper.Filter.Sex := fmRecordSelect.FNeedSex;

    fmGEDKeeper.ComListPersonsRefresh(fmRecordSelect.ListRecords, True);

    fmGEDKeeper.Filter.Restore();

    case fmRecordSelect.ShowModal() of
      mrOk: Result := TGEDCOMIndividualRecord(fmRecordSelect.ResultRecord);
      mrCancel: ;
    end;
  finally
    fmRecordSelect.Destroy;
  end;
end;

function SelectRecord(aMode: TSelectMode): TGEDCOMRecordWithLists;
var
  fmRecordSelect: TfmRecordSelect;
begin
  fmRecordSelect := TfmRecordSelect.Create(nil);

  try
    Result := nil;

    fmRecordSelect.FMode := aMode;
    case aMode of
      smPerson: begin
        fmGEDKeeper.Filter.Backup();
        fmGEDKeeper.Filter.Clear();

        fmGEDKeeper.ComListPersonsRefresh(fmRecordSelect.ListRecords, True);

        fmGEDKeeper.Filter.Restore();
      end;
      smNote: fmGEDKeeper.ComListNotesRefresh(fmRecordSelect.ListRecords, True);
      smMultimedia: fmGEDKeeper.ComListMultimediaRefresh(fmRecordSelect.ListRecords);
      smSource: fmGEDKeeper.ComListSourcesRefresh(fmRecordSelect.ListRecords, True);
      smGroup: fmGEDKeeper.ComListGroupsRefresh(fmRecordSelect.ListRecords, True);
    end;

    case fmRecordSelect.ShowModal() of
      mrOk: Result := fmRecordSelect.ResultRecord;
      mrCancel: ;
    end;
  finally
    fmRecordSelect.Destroy;
  end;
end;

procedure TfmRecordSelect.btnCreateClick(Sender: TObject);
var
  iRec: TGEDCOMIndividualRecord;
  noteRec: TGEDCOMNoteRecord;
  sourceRec: TGEDCOMSourceRecord;
  groupRec: TGEDCOMGroupRecord;
begin
  case FMode of
    smPerson: begin
      iRec := fmGEDKeeper.CreatePersonDialog(FTarget, FTargetMode, FNeedSex);
      if (iRec <> nil) then begin
        ResultRecord := iRec;
        ModalResult := mrOk;
      end;
    end;

    smNote: begin
      noteRec := nil;
      fmGEDKeeper.ModifyNote(noteRec);
      if (noteRec <> nil) then begin
        ResultRecord := noteRec;
        ModalResult := mrOk;
      end;
    end;

    smMultimedia: begin
    end;

    smSource: begin
      sourceRec := nil;
      fmGEDKeeper.ModifySource(sourceRec);
      if (sourceRec <> nil) then begin
        ResultRecord := sourceRec;
        ModalResult := mrOk;
      end;
    end;

    smGroup: begin
      groupRec := nil;
      fmGEDKeeper.ModifyGroup(groupRec);
      if (groupRec <> nil) then begin
        ResultRecord := groupRec;
        ModalResult := mrOk;
      end;
    end;
  end;
end;

procedure TfmRecordSelect.btnSelectClick(Sender: TObject);
var
  sel: TListItem;
begin
  sel := ListRecords.Selected;
  if (sel = nil) then Exit;

  ResultRecord := TGEDCOMRecordWithLists(sel.Data);
  ModalResult := mrOk;
end;

procedure TfmRecordSelect.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then Close;
end;

end.
