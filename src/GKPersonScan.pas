unit GKPersonScan;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, Buttons, ExtCtrls, GKBase;

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
    function GetBase: TfmBase;
  public
    property Base: TfmBase read GetBase;
  end;

implementation

uses bsComUtils, GKMain, GedCom551, GKCommon;

{$R *.dfm}

procedure TfmPersonScan.btnCreateClick(Sender: TObject);
var
  iRec: TGEDCOMIndividualRecord;
  tokCount: Integer;
  nam, pat, fam, tmp: string;
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

  iRec := CreatePersonEx(Base.Tree, nam, pat, fam, svNone, False);
  Base.ChangeRecord(iRec);

  if (CheckBirth.Checked)
  then CreateIEvent(Base.Tree, iRec, 'BIRT', StrToGEDCOMDate(EditBirthDate.Text), EditBirthPlace.Text);

  if (CheckDeath.Checked)
  then CreateIEvent(Base.Tree, iRec, 'DEAT', StrToGEDCOMDate(EditDeathDate.Text), EditDeathPlace.Text);

  if (MemoNote.Text <> '')
  then CreateNoteEx(Base.Tree, MemoNote.Lines, iRec);

  EditName.Text := '';
  EditBirthDate.Text := '';
  EditBirthPlace.Text := '';
  CheckBirth.Checked := False;
  EditDeathDate.Text := '';
  EditDeathPlace.Text := '';
  CheckDeath.Checked := False;
  MemoNote.Text := '';

  Base.ListsRefresh();
end;

procedure TfmPersonScan.EditBirthDateChange(Sender: TObject);
begin
  CheckBirth.Checked := True;
end;

procedure TfmPersonScan.EditDeathDateChange(Sender: TObject);
begin
  CheckDeath.Checked := True;
end;

function TfmPersonScan.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

end.
