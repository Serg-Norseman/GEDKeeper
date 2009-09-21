unit GKFileProperties;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Buttons,
  GedCom551, ComCtrls;

type
  TfmFileProperties = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    PageControl1: TPageControl;
    SheetAuthor: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EditName: TEdit;
    EditTel: TEdit;
    MemoAddress: TMemo;
    procedure btnAcceptClick(Sender: TObject);
  private
    FTree: TGEDCOMTree;
    procedure SetTree(const Value: TGEDCOMTree);
  public
    property Tree: TGEDCOMTree read FTree write SetTree;
  end;

implementation

uses GKMain;

{$R *.dfm}

procedure TfmFileProperties.SetTree(const Value: TGEDCOMTree);
var
  submitter: TGEDCOMSubmitterRecord;
begin
  FTree := Value;

  submitter := TGEDCOMSubmitterRecord(FTree.Header.Submitter.Value);
  if (submitter = nil) then begin
    submitter := TGEDCOMSubmitterRecord.Create(FTree, FTree);
    submitter.NewXRef;
    FTree.AddRecord(submitter);
    FTree.Header.SetTagStringValue('SUBM', '@'+submitter.XRef+'@');
  end;

  EditName.Text := submitter.Name.FullName;
  MemoAddress.Text := submitter.Address.Address.Text;
  EditTel.Text := submitter.Address.PhoneNumbers[0];
end;

procedure TfmFileProperties.btnAcceptClick(Sender: TObject);
var
  submitter: TGEDCOMSubmitterRecord;
begin
  submitter := TGEDCOMSubmitterRecord(FTree.Header.Submitter.Value);
  submitter.Name.StringValue := EditName.Text;
  submitter.Address.Address := MemoAddress.Lines;
  submitter.Address.PhoneNumbers[0] := EditTel.Text;
  submitter.ChangeDate.ChangeDateTime := Now();

  fmGEDKeeper.Modified := True;
end;

end.
