unit GKFileProperties; {prepare:fin}

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, ComCtrls, 
  GKBase;

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
    procedure FormCreate(Sender: TObject);
  private
    function GetBase(): TfmBase;
    procedure UpdateControls();
  public
    property Base: TfmBase read GetBase;
  end;

implementation

uses GedCom551, GKMain, GKEngine;

{$R *.dfm}

procedure TfmFileProperties.UpdateControls();
var
  submitter: TGEDCOMSubmitterRecord;
begin
  submitter := Base.Engine.GetSubmitter();

  EditName.Text := submitter.Name.FullName;
  MemoAddress.Text := submitter.Address.Address.Text;
  EditTel.Text := submitter.Address.PhoneNumbers[0];

  CheckAdvanced.Checked := Base.Engine.IsAdvanced;
  edExtName.Text := Base.GetSpecExtName();
end;

procedure TfmFileProperties.btnAcceptClick(Sender: TObject);
var
  submitter: TGEDCOMSubmitterRecord;
begin
  submitter := Base.Engine.GetSubmitter();
  submitter.Name.StringValue := EditName.Text;
  submitter.Address.Address := MemoAddress.Lines;
  submitter.Address.PhoneNumbers[0] := EditTel.Text;
  submitter.ChangeDate.ChangeDateTime := Now();

  Base.Engine.IsAdvanced := CheckAdvanced.Checked;
  Base.Engine.ExtName := edExtName.Text;

  Base.Modified := True;
end;

function TfmFileProperties.GetBase(): TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmFileProperties.FormCreate(Sender: TObject);
begin
  UpdateControls();
end;

end.
