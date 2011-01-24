unit GKRecordSelect;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, StdCtrls,
  Buttons, ExtCtrls, bsCtrls, GedCom551, GKEngine, GKBase, GKLists;

type
  TfmRecordSelect = class(TForm)
    btnSelect: TBitBtn;
    btnCreate: TBitBtn;
    btnCancel: TBitBtn;
    panList: TPanel;
    panFilter: TPanel;
    edFastFilter: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edFastFilterChange(Sender: TObject);
  private
    FMode: TGEDCOMRecordType;
    FFilter: string;
    FTargetMode: TTargetMode;
    FLocalFilter: TPersonsFilter;

    function GetBase: TfmBase;
    procedure SetMode(const Value: TGEDCOMRecordType);
    procedure DataRefresh();
    procedure SetFilter(const Value: string);
    procedure SetTargetMode(const Value: TTargetMode);
  protected
  public
    FTarget: TGEDCOMIndividualRecord;
    FNeedSex: TGEDCOMSex;
    ResultRecord: TGEDCOMRecord;

    ListRecords: TRecordsView;

    property Base: TfmBase read GetBase;
    property Filter: string read FFilter write SetFilter;
    property Mode: TGEDCOMRecordType read FMode write SetMode;
    property TargetMode: TTargetMode read FTargetMode write SetTargetMode;
  end;

implementation

uses GKMain;

{$R *.dfm}

procedure TfmRecordSelect.FormCreate(Sender: TObject);
begin
  FLocalFilter := TPersonsFilter.Create;
  FLocalFilter.List := flSelector;

  FFilter := '*';
end;

procedure TfmRecordSelect.DataRefresh();
begin
  FLocalFilter.Clear();
  FLocalFilter.Name := FFilter;
  FLocalFilter.Sex := FNeedSex;

  if Assigned(ListRecords) then FreeAndNil(ListRecords);
  Base.CreateRecordsView(Self, panList, FMode, ListRecords);
  ListRecords.UpdateContents(Base.ShieldState, True, FLocalFilter, 1);
end;

procedure TfmRecordSelect.FormDestroy(Sender: TObject);
begin
  FLocalFilter.Destroy;
end;

procedure TfmRecordSelect.btnCreateClick(Sender: TObject);
var
  iRec: TGEDCOMIndividualRecord;
  famRec: TGEDCOMFamilyRecord;
  noteRec: TGEDCOMNoteRecord;
  sourceRec: TGEDCOMSourceRecord;
  groupRec: TGEDCOMGroupRecord;
  repRec: TGEDCOMRepositoryRecord;
  mmRec: TGEDCOMMultimediaRecord;
  taskRec: TGEDCOMTaskRecord;
  corrRec: TGEDCOMCommunicationRecord;
  locRec: TGEDCOMLocationRecord;
  fam_target: TFamilyTarget;
begin
  //Hide;

  case FMode of
    rtIndividual: begin
      iRec := Base.CreatePersonDialog(FTarget, FTargetMode, FNeedSex);
      if (iRec <> nil) then begin
        ResultRecord := iRec;
        ModalResult := mrOk;
      end;
    end;

    rtFamily: begin
      famRec := nil;

      if (FTargetMode = tmChildToFamily)
      then fam_target := ftChild
      else fam_target := ftNone;

      if Base.ModifyFamily(famRec, fam_target, FTarget) then begin
        ResultRecord := famRec;
        ModalResult := mrOk;
      end;
    end;

    rtNote: begin
      noteRec := nil;
      if Base.ModifyNote(noteRec) then begin
        ResultRecord := noteRec;
        ModalResult := mrOk;
      end;
    end;

    rtMultimedia: begin
      mmRec := nil;
      if Base.ModifyMedia(mmRec) then begin
        ResultRecord := mmRec;
        ModalResult := mrOk;
      end;
    end;

    rtSource: begin
      sourceRec := nil;
      if Base.ModifySource(sourceRec) then begin
        ResultRecord := sourceRec;
        ModalResult := mrOk;
      end;
    end;

    rtRepository: begin
      repRec := nil;
      if Base.ModifyRepository(repRec) then begin
        ResultRecord := repRec;
        ModalResult := mrOk;
      end;
    end;

    rtGroup: begin
      groupRec := nil;
      if Base.ModifyGroup(groupRec) then begin
        ResultRecord := groupRec;
        ModalResult := mrOk;
      end;
    end;

    rtTask: begin
      taskRec := nil;
      if Base.ModifyTask(taskRec) then begin
        ResultRecord := taskRec;
        ModalResult := mrOk;
      end;
    end;

    rtCommunication: begin
      corrRec := nil;
      if Base.ModifyCommunication(corrRec) then begin
        ResultRecord := corrRec;
        ModalResult := mrOk;
      end;
    end;

    rtLocation: begin
      locRec := nil;
      if Base.ModifyLocation(locRec) then begin
        ResultRecord := locRec;
        ModalResult := mrOk;
      end;
    end;
  end;
end;

procedure TfmRecordSelect.btnSelectClick(Sender: TObject);
begin
  ResultRecord := GetSelectedRecord(ListRecords);
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

procedure TfmRecordSelect.edFastFilterChange(Sender: TObject);
begin
  SetFilter(edFastFilter.Text);
end;

procedure TfmRecordSelect.SetMode(const Value: TGEDCOMRecordType);
begin
  FMode := Value;
  DataRefresh();
end;

procedure TfmRecordSelect.SetFilter(const Value: string);
begin
  FFilter := Value;

  if (FFilter = '')
  then FFilter := '*'
  else
    if (FFilter <> '*')
    then FFilter := '*' + FFilter + '*';

  DataRefresh();
end;

procedure TfmRecordSelect.SetTargetMode(const Value: TTargetMode);
begin
  FTargetMode := Value;

  FLocalFilter.ChildSelector := (FTargetMode = tmAncestor);
end;

end.
