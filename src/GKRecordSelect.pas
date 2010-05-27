unit GKRecordSelect;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls, StdCtrls,
  GedCom551, GKCommon, Buttons, bsCtrls, GKBase, ExtCtrls, GKLists;

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
    FMode: TSelectMode;
    FFilter: string;
    FTargetMode: TTargetMode;
    FLocalFilter: TPersonsFilter;

    function GetBase: TfmBase;
    procedure SetMode(const Value: TSelectMode);
    procedure DataRefresh();
    procedure SetFilter(const Value: string);
    procedure SetTargetMode(const Value: TTargetMode);
  protected
  public
    FTarget: TGEDCOMIndividualRecord;
    FNeedSex: TGEDCOMSex;
    ResultRecord: TGEDCOMRecord;

    ListRecords: TBSListView;

    property Base: TfmBase read GetBase;
    property Filter: string read FFilter write SetFilter;
    property Mode: TSelectMode read FMode write SetMode;
    property TargetMode: TTargetMode read FTargetMode write SetTargetMode;
  end;

implementation

uses GKMain;

{$R *.dfm}

procedure TfmRecordSelect.FormCreate(Sender: TObject);
begin
  FLocalFilter := TPersonsFilter.Create;
  FLocalFilter.List := flSelector;

  Base.CreateListView(Self, panList, ListRecords);
  FFilter := '*';
end;

procedure TfmRecordSelect.FormDestroy(Sender: TObject);
begin
  FLocalFilter.Destroy;
end;

procedure TfmRecordSelect.btnCreateClick(Sender: TObject);
var
  iRec: TGEDCOMIndividualRecord;
  noteRec: TGEDCOMNoteRecord;
  sourceRec: TGEDCOMSourceRecord;
  groupRec: TGEDCOMGroupRecord;
  repRec: TGEDCOMRepositoryRecord;
  mmRec: TGEDCOMMultimediaRecord;
  taskRec: TGEDCOMTaskRecord;
  corrRec: TGEDCOMCommunicationRecord;
  locRec: TGEDCOMLocationRecord;
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
      if Base.ModifyNote(noteRec) then begin
        ResultRecord := noteRec;
        ModalResult := mrOk;
      end;
    end;

    smMultimedia: begin
      mmRec := nil;
      if Base.ModifyMedia(mmRec) then begin
        ResultRecord := mmRec;
        ModalResult := mrOk;
      end;
    end;

    smSource: begin
      sourceRec := nil;
      if Base.ModifySource(sourceRec) then begin
        ResultRecord := sourceRec;
        ModalResult := mrOk;
      end;
    end;

    smRepository: begin
      repRec := nil;
      if Base.ModifyRepository(repRec) then begin
        ResultRecord := repRec;
        ModalResult := mrOk;
      end;
    end;

    smGroup: begin
      groupRec := nil;
      if Base.ModifyGroup(groupRec) then begin
        ResultRecord := groupRec;
        ModalResult := mrOk;
      end;
    end;

    smTask: begin
      taskRec := nil;
      if Base.ModifyTask(taskRec) then begin
        ResultRecord := taskRec;
        ModalResult := mrOk;
      end;
    end;

    smCommunication: begin
      corrRec := nil;
      if Base.ModifyCommunication(corrRec) then begin
        ResultRecord := corrRec;
        ModalResult := mrOk;
      end;
    end;

    smLocation: begin
      locRec := nil;
      if Base.ModifyLocation(locRec) then begin
        ResultRecord := locRec;
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

procedure TfmRecordSelect.edFastFilterChange(Sender: TObject);
begin
  SetFilter(edFastFilter.Text);
end;

procedure TfmRecordSelect.SetMode(const Value: TSelectMode);
begin
  FMode := Value;
  DataRefresh();
end;

procedure TfmRecordSelect.DataRefresh();
begin
  FLocalFilter.Clear();
  FLocalFilter.Name := FFilter;
  FLocalFilter.Sex := FNeedSex;

  Base.UpdateList(SelectRecords[FMode], ListRecords, True, FLocalFilter, 1); //+
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
