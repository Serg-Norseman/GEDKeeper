unit GKNoteEdit;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Buttons,
  GedCom551;

type
  TfmNoteEdit = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    MemoNote: TMemo;
    procedure btnAcceptClick(Sender: TObject);
  private
    FNoteRecord: TGEDCOMNoteRecord;
    procedure SetNoteRecord(const Value: TGEDCOMNoteRecord);
  public
    property NoteRecord: TGEDCOMNoteRecord read FNoteRecord write SetNoteRecord;
  end;

implementation

uses GKMain, GKCommon;

{$R *.dfm}

procedure TfmNoteEdit.SetNoteRecord(const Value: TGEDCOMNoteRecord);
begin
  FNoteRecord := Value;
  MemoNote.Text := Trim(FNoteRecord.Notes.Text);
end;

procedure TfmNoteEdit.btnAcceptClick(Sender: TObject);
begin
  FNoteRecord.Notes := MemoNote.Lines;
  FNoteRecord.ChangeDate.ChangeDateTime := Now();
  fmGEDKeeper.Modified := True;
end;

end.
