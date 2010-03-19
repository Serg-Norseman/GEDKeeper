unit GKRecordSelect;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, StdCtrls,
  GedCom551, GKCommon, Buttons, bsCtrls, GKBase;

type
  TfmRecordSelect = class(TForm)
    ListRecords: TBSListView;
    btnSelect: TBitBtn;
    btnCreate: TBitBtn;
    btnCancel: TBitBtn;
    procedure btnSelectClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    function GetBase: TfmBase;
  protected
  public
    FMode: TSelectMode;
    FTarget: TGEDCOMIndividualRecord;
    FTargetMode: TTargetMode;
    FNeedSex: TGEDCOMSex;
    ResultRecord: TGEDCOMRecord;

    property Base: TfmBase read GetBase;
  end;

implementation

uses GKMain;

{$R *.dfm}

procedure TfmRecordSelect.btnCreateClick(Sender: TObject);
var
  iRec: TGEDCOMIndividualRecord;
  noteRec: TGEDCOMNoteRecord;
  sourceRec: TGEDCOMSourceRecord;
  groupRec: TGEDCOMGroupRecord;
  repRec: TGEDCOMRepositoryRecord;
  mmRec: TGEDCOMMultimediaRecord;
begin
  case FMode of
    smPerson: begin
      iRec := Base.CreatePersonDialog(FTarget, FTargetMode, FNeedSex);
      if (iRec <> nil) then begin
        ResultRecord := iRec;
        ModalResult := mrOk;
      end;
    end;

    smNote: begin
      noteRec := nil;
      Base.ModifyNote(noteRec);
      if (noteRec <> nil) then begin
        ResultRecord := noteRec;
        ModalResult := mrOk;
      end;
    end;

    smMultimedia: begin
      mmRec := nil;
      Base.ModifyMedia(mmRec);
      if (mmRec <> nil) then begin
        ResultRecord := mmRec;
        ModalResult := mrOk;
      end;
    end;

    smSource: begin
      sourceRec := nil;
      Base.ModifySource(sourceRec);
      if (sourceRec <> nil) then begin
        ResultRecord := sourceRec;
        ModalResult := mrOk;
      end;
    end;

    smRepository: begin
      repRec := nil;
      Base.ModifyRepository(repRec);
      if (repRec <> nil) then begin
        ResultRecord := repRec;
        ModalResult := mrOk;
      end;
    end;

    smGroup: begin
      groupRec := nil;
      Base.ModifyGroup(groupRec);
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

  ResultRecord := TGEDCOMRecord(sel.Data);
  ModalResult := mrOk;
end;

procedure TfmRecordSelect.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then Close;
end;

function TfmRecordSelect.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

end.
