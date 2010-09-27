unit GKNoteEdit;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, Buttons,
  GedCom551, GKBase;

type
  TfmNoteEdit = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    mmNote: TMemo;
    procedure btnAcceptClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnCancelClick(Sender: TObject);
  private
    FNoteRecord: TGEDCOMNoteRecord;
    procedure SetNoteRecord(const Value: TGEDCOMNoteRecord);
    function GetBase: TfmBase;
  public
    property Base: TfmBase read GetBase;
    property NoteRecord: TGEDCOMNoteRecord read FNoteRecord write SetNoteRecord;
  end;

implementation

uses GKMain, GKCommon;

{$R *.dfm}

procedure TfmNoteEdit.SetNoteRecord(const Value: TGEDCOMNoteRecord);
begin
  FNoteRecord := Value;
  mmNote.Text := Trim(FNoteRecord.Notes.Text);
end;

procedure TfmNoteEdit.btnAcceptClick(Sender: TObject);
begin
  FNoteRecord.Notes := mmNote.Lines;
  Base.ChangeRecord(FNoteRecord);
  ModalResult := mrOk;
end;

function TfmNoteEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmNoteEdit.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfmNoteEdit.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then ModalResult := mrCancel;
end;

end.
