unit GKPersonScan;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, Buttons, ExtCtrls;

type
  TfmPersonScan = class(TForm)
    Label1: TLabel;
    EditName: TEdit;
    Label2: TLabel;
    MemoNote: TMemo;
    btnCreate: TBitBtn;
    btnCancel: TBitBtn;
    Panel1: TPanel;
    Panel2: TPanel;
    Label3: TLabel;
    EditBirthDate: TMaskEdit;
    EditBirthPlace: TEdit;
    Label5: TLabel;
    CheckBirth: TCheckBox;
    CheckDeath: TCheckBox;
    Label6: TLabel;
    EditDeathDate: TMaskEdit;
    Label7: TLabel;
    EditDeathPlace: TEdit;
    procedure btnCreateClick(Sender: TObject);
    procedure EditBirthDateChange(Sender: TObject);
    procedure EditDeathDateChange(Sender: TObject);
  private
  public
  end;

implementation

uses bsComUtils, GKMain, GedCom551, GKCommon;

{$R *.dfm}

procedure CreateIEvent(aTree: TGEDCOMTree; iRec: TGEDCOMIndividualRecord;
  evSign, evDate, evPlace: string);
var
  event: TGEDCOMIndividualEvent;
begin
  event := TGEDCOMIndividualEvent.Create(aTree, iRec);
  event.Name := evSign;
  event.Detail.Date.ParseString(evDate);
  event.Detail.Place := evPlace;
  iRec.AddIndividualEvent(event);
end;

procedure TfmPersonScan.btnCreateClick(Sender: TObject);
var
  iRec: TGEDCOMIndividualRecord;
  tokCount: Integer;
  nam, pat, fam, tmp: string;
  noteRec: TGEDCOMNoteRecord;
  note: TGEDCOMNotes;
begin
  tmp := AnsiLowerCase(EditName.Text);
  tokCount := GetTokensCount(tmp, ' ');
  if (tokCount < 3) then begin
    MessageDlg('Количество компонентов имени меньше трех.', mtError, [mbOk], 0);
    Exit;
  end;

  fam := GetToken(tmp, ' ', 1);
  nam := GetToken(tmp, ' ', 2);
  pat := GetToken(tmp, ' ', 3);

  fam[1] := AnsiUpperCase(fam)[1];
  nam[1] := AnsiUpperCase(nam)[1];
  pat[1] := AnsiUpperCase(pat)[1];

  iRec := fmGEDKeeper.CreatePerson(nam, pat, fam, svNone);

  if (CheckBirth.Checked)
  then CreateIEvent(fmGEDKeeper.FTree, iRec, 'BIRT', StrToGEDCOMDate(EditBirthDate.Text), EditBirthPlace.Text);

  if (CheckDeath.Checked)
  then CreateIEvent(fmGEDKeeper.FTree, iRec, 'DEAT', StrToGEDCOMDate(EditDeathDate.Text), EditDeathPlace.Text);

  if (MemoNote.Text <> '') then begin
    noteRec := TGEDCOMNoteRecord.Create(fmGEDKeeper.FTree, fmGEDKeeper.FTree);
    noteRec.NewXRef;
    noteRec.Notes := MemoNote.Lines;
    fmGEDKeeper.FTree.AddRecord(noteRec);

    note := TGEDCOMNotes.Create(fmGEDKeeper.FTree, iRec);
    note.Value := noteRec;
    iRec.AddNotes(note);
  end;

  EditName.Text := '';
  EditBirthDate.Text := '';
  EditBirthPlace.Text := '';
  CheckBirth.Checked := False;
  EditDeathDate.Text := '';
  EditDeathPlace.Text := '';
  CheckDeath.Checked := False;
  MemoNote.Text := '';

  fmGEDKeeper.ListsRefresh();
end;

procedure TfmPersonScan.EditBirthDateChange(Sender: TObject);
begin
  CheckBirth.Checked := True;
end;

procedure TfmPersonScan.EditDeathDateChange(Sender: TObject);
begin
  CheckDeath.Checked := True;
end;

end.
