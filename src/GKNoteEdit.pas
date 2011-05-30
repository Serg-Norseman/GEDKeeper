unit GKNoteEdit; {prepare:fin; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, Buttons,
  GedCom551, GKBase, GKLangs;

type
  TfmNoteEdit = class(TForm, ILocalization)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    mmNote: TMemo;
    procedure btnAcceptClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FNoteRecord: TGEDCOMNoteRecord;
    procedure SetNoteRecord(const Value: TGEDCOMNoteRecord);
    function GetBase: TfmBase;
  public
    property Base: TfmBase read GetBase;
    property NoteRecord: TGEDCOMNoteRecord read FNoteRecord write SetNoteRecord;

    procedure SetLang();
  end;

implementation

uses GKMain;

{$R *.dfm}

{ TfmNoteEdit }

procedure TfmNoteEdit.SetLang();
begin
  btnAccept.Caption := LSList[LSID_DlgAccept];
  btnCancel.Caption := LSList[LSID_DlgCancel];
  Caption := LSList[LSID_Note];
end;

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

procedure TfmNoteEdit.FormCreate(Sender: TObject);
begin
  SetLang();
end;

procedure TfmNoteEdit.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then ModalResult := mrCancel;
end;

end.
