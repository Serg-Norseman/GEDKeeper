unit GKFileProperties; {prepare:fin; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, ComCtrls, 
  GKBase, GKLangs;

type
  TfmFileProperties = class(TForm, ILocalization)
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

    procedure SetLang();
  end;

implementation

uses GedCom551, GKMain, GKEngine;

{$R *.dfm}

procedure TfmFileProperties.FormCreate(Sender: TObject);
begin
  UpdateControls();

  SetLang();
end;

procedure TfmFileProperties.SetLang();
begin
  btnAccept.Caption := LSList[LSID_DlgAccept];
  btnCancel.Caption := LSList[LSID_DlgCancel];

  SheetAuthor.Caption := LSList[LSID_Author];
  Label1.Caption := LSList[LSID_Name];
  Label2.Caption := LSList[LSID_Address];
  Label3.Caption := LSList[LSID_Telephone];

  SheetAdvanced.Caption := LSList[LSID_Advanced];
  CheckAdvanced.Caption := LSList[LSID_AdvancedSupport];
  Label4.Caption := LSList[LSID_ExtName];
end;

procedure TfmFileProperties.UpdateControls();
var
  submitter: TGEDCOMSubmitterRecord;
begin
  submitter := Base.Engine.GetSubmitter();

  EditName.Text := submitter.Name.FullName;
  MemoAddress.Text := submitter.Address.Address.Text;
  EditTel.Text := submitter.Address.PhoneNumbers[0];

  CheckAdvanced.Checked := Base.Engine.IsAdvanced;
  edExtName.Text := Base.Engine.GetSpecExtName();
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

end.
