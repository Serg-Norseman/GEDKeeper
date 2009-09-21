unit GKSourceEdit;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Buttons,
  GedCom551;

type
  TfmSourceEdit = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    EditShortTitle: TEdit;
    EditAuthor: TMemo;
    EditTitle: TMemo;
    EditPublication: TMemo;
    procedure btnAcceptClick(Sender: TObject);
  private
    FSourceRecord: TGEDCOMSourceRecord;
    procedure SetSourceRecord(const Value: TGEDCOMSourceRecord);
  public
    property SourceRecord: TGEDCOMSourceRecord read FSourceRecord write SetSourceRecord;
  end;

implementation

uses GKMain;

{$R *.dfm}

procedure TfmSourceEdit.SetSourceRecord(const Value: TGEDCOMSourceRecord);
begin
  FSourceRecord := Value;

  EditShortTitle.Text := FSourceRecord.FiledByEntry;
  EditAuthor.Text := Trim(FSourceRecord.Originator.Text);
  EditTitle.Text := Trim(FSourceRecord.Title.Text);
  EditPublication.Text := Trim(FSourceRecord.Publication.Text);
end;

procedure TfmSourceEdit.btnAcceptClick(Sender: TObject);
begin
  FSourceRecord.FiledByEntry := EditShortTitle.Text;

  FSourceRecord.Originator.Clear;
  FSourceRecord.Originator := EditAuthor.Lines;

  FSourceRecord.Title.Clear;
  FSourceRecord.Title := EditTitle.Lines;

  FSourceRecord.Publication.Clear;
  FSourceRecord.Publication := EditPublication.Lines;

  FSourceRecord.ChangeDate.ChangeDateTime := Now();

  fmGEDKeeper.Modified := True;
end;

end.
