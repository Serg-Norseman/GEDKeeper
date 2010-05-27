unit GKFileProperties;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Buttons,
  GedCom551, ComCtrls, bsCtrls, GKBase;

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
    SheetAdvanced: TTabSheet;
    CheckAdvanced: TCheckBox;
    Label4: TLabel;
    edExtName: TEdit;
    procedure btnAcceptClick(Sender: TObject);
  private
    FTree: TGEDCOMTree;
    procedure SetTree(const Value: TGEDCOMTree);

    function GetBase(): TfmBase;
  public
    property Base: TfmBase read GetBase;
    property Tree: TGEDCOMTree read FTree write SetTree;
  end;

implementation

uses GKMain, GKCommon;

{$R *.dfm}

procedure TfmFileProperties.SetTree(const Value: TGEDCOMTree);
var
  submitter: TGEDCOMSubmitterRecord;
begin
  FTree := Value;

  submitter := TGEDCOMSubmitterRecord(FTree.Header.Submitter.Value);
  if (submitter = nil) then begin
    submitter := TGEDCOMSubmitterRecord.Create(FTree, FTree);
    submitter.InitNew();
    FTree.AddRecord(submitter);
    FTree.Header.SetTagStringValue('SUBM', '@'+submitter.XRef+'@');
  end;

  EditName.Text := submitter.Name.FullName;
  MemoAddress.Text := submitter.Address.Address.Text;
  EditTel.Text := submitter.Address.PhoneNumbers[0];

  //
  CheckAdvanced.Checked := (FTree.Header.FindTag(AdvTag) <> nil);
  edExtName.Text := Base.GetExtName();
  //
end;

procedure TfmFileProperties.btnAcceptClick(Sender: TObject);
var
  submitter: TGEDCOMSubmitterRecord;
  tag: TGEDCOMTag;
begin
  submitter := TGEDCOMSubmitterRecord(FTree.Header.Submitter.Value);
  submitter.Name.StringValue := EditName.Text;
  submitter.Address.Address := MemoAddress.Lines;
  submitter.Address.PhoneNumbers[0] := EditTel.Text;
  submitter.ChangeDate.ChangeDateTime := Now();

  if (CheckAdvanced.Checked) then begin
    tag := FTree.Header.FindTag(AdvTag);
    if (tag = nil) then FTree.Header.AddTag(AdvTag);

    tag := FTree.Header.FindTag(ExtTag);
    if (tag = nil) then tag := FTree.Header.AddTag(ExtTag);
    tag.StringValue := edExtName.Text;
  end else begin
    FTree.Header.DeleteTag(AdvTag);
    FTree.Header.DeleteTag(ExtTag);
  end;

  Base.Modified := True;
end;

function TfmFileProperties.GetBase(): TfmBase;
begin
  Result := TfmBase(Owner);
end;

end.
