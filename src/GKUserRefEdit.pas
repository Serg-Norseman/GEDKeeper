unit GKUserRefEdit; {prepare:fin; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons,
  GedCom551, GKBase, GKLangs;

type
  TfmUserRefEdit = class(TForm, ILocalization)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    Label1: TLabel;
    EditRef: TComboBox;
    Label2: TLabel;
    EditType: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
  private
    FUserRef: TGEDCOMUserReference;

    procedure SetUserRef(const Value: TGEDCOMUserReference);
    function GetBase: TfmBase;
  public
    property UserRef: TGEDCOMUserReference read FUserRef write SetUserRef;
    property Base: TfmBase read GetBase;

    procedure SetLang();
  end;

implementation

uses GKMain;

{$R *.dfm}

{ TfmUserRefEdit }

procedure TfmUserRefEdit.FormCreate(Sender: TObject);
begin
  //EditRelation.Items.Assign(fmGEDKeeper.Options.Relations);

  SetLang();
end;

procedure TfmUserRefEdit.SetLang();
begin
  btnAccept.Caption := LSList[LSID_DlgAccept];
  btnCancel.Caption := LSList[LSID_DlgCancel];

  Caption := LSList[LSID_WinUserRefEdit];

  Label1.Caption := LSList[LSID_Reference];
  Label2.Caption := LSList[LSID_Type];
end;

procedure TfmUserRefEdit.SetUserRef(const Value: TGEDCOMUserReference);
begin
  FUserRef := Value;

  EditRef.Text := FUserRef.StringValue;
  EditType.Text := FUserRef.ReferenceType;
end;

procedure TfmUserRefEdit.btnAcceptClick(Sender: TObject);
{var
  rel: string;}
begin
  {rel := Trim(EditRelation.Text);
  if (rel <> '') then begin
    if (fmGEDKeeper.Options.Relations.IndexOf(rel) < 0)
    then fmGEDKeeper.Options.Relations.Add(rel);
  end;}

  FUserRef.StringValue := EditRef.Text;
  FUserRef.ReferenceType := EditType.Text;
end;

function TfmUserRefEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

end.
