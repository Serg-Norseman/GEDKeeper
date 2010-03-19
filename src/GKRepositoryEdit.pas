unit GKRepositoryEdit;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, ComCtrls, ExtCtrls,
  GedCom551, GKBase, GKCommon;

type
  TfmRepositoryEdit = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edName: TEdit;
    PagesData: TPageControl;
    SheetNotes: TTabSheet;
    btnAddress: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure btnAddressClick(Sender: TObject);
  private
    FRepositoryRecord: TGEDCOMRepositoryRecord;

    FNotesList: TSheetList;

    procedure ControlsRefresh();
    function GetBase: TfmBase;
    procedure ListModify(Sender: TObject; Index: Integer; Action: TRecAction);
    procedure SetRepositoryRecord(const Value: TGEDCOMRepositoryRecord);
  public
    property Base: TfmBase read GetBase;
    property RepositoryRecord: TGEDCOMRepositoryRecord
      read FRepositoryRecord write SetRepositoryRecord;
  end;

implementation

uses GKAddressEdit;

{$R *.dfm}

procedure TfmRepositoryEdit.FormCreate(Sender: TObject);
begin
  FNotesList := TSheetList.Create(SheetNotes);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList.List);
end;

procedure TfmRepositoryEdit.ControlsRefresh();
begin
  Base.RecListNotesRefresh(FRepositoryRecord, FNotesList.List, nil);
end;

procedure TfmRepositoryEdit.SetRepositoryRecord(const Value: TGEDCOMRepositoryRecord);
begin
  FRepositoryRecord := Value;

  edName.Text := FRepositoryRecord.RepositoryName;

  ControlsRefresh();
end;

procedure TfmRepositoryEdit.btnAcceptClick(Sender: TObject);
begin
  FRepositoryRecord.RepositoryName := edName.Text;

  FRepositoryRecord.ChangeDate.ChangeDateTime := Now();

  Base.Modified := True;
end;

function TfmRepositoryEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmRepositoryEdit.btnAddressClick(Sender: TObject);
var
  fmAddressEdit: TfmAddressEdit;
begin
  fmAddressEdit := TfmAddressEdit.Create(Application);
  try
    fmAddressEdit.Address := FRepositoryRecord.Address;
    fmAddressEdit.ShowModal;
  finally
    fmAddressEdit.Destroy;
  end;
end;

procedure TfmRepositoryEdit.ListModify(Sender: TObject; Index: Integer;
  Action: TRecAction);
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(FRepositoryRecord, Index, Action)
    then ControlsRefresh();
  end;
end;

end.
