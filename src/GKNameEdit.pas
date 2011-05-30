unit GKNameEdit; {prepare:fin; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, Buttons,
  GedCom551, GKEngine, GKCommon, GKLangs;

type
  TfmNameEdit = class(TForm, ILocalization)
    Label2: TLabel;
    edName: TEdit;
    Label4: TLabel;
    edSex: TComboBox;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    edFPatr: TEdit;
    Label1: TLabel;
    edMPatr: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure edFPatrKeyPress(Sender: TObject; var Key: Char);
    procedure btnAcceptClick(Sender: TObject);
  private
    FIName: TName;
    procedure SetIName(const Value: TName);
  public
    property IName: TName read FIName write SetIName;

    procedure SetLang();
  end;

implementation

uses GKMain, uVista;

{$R *.dfm}

procedure TfmNameEdit.FormCreate(Sender: TObject);
var
  sx: TGEDCOMSex;
begin
  if IsWindowsVista() then SetVistaFonts(Self);

  for sx := Low(TGEDCOMSex) to High(TGEDCOMSex) do edSex.Items.Add(SexStr(sx));

  SetLang();
end;

procedure TfmNameEdit.SetLang();
begin
  btnAccept.Caption := LSList[LSID_DlgAccept];
  btnCancel.Caption := LSList[LSID_DlgCancel];

  Caption := LSList[LSID_Name];

  Label2.Caption := LSList[LSID_Name];
  Label4.Caption := LSList[LSID_Sex];
  GroupBox1.Caption := LSList[LSID_Patronymic];
  Label3.Caption := LSList[LSID_PatFemale];
  Label1.Caption := LSList[LSID_PatMale];
end;

procedure TfmNameEdit.SetIName(const Value: TName);
begin
  FIName := Value;

  if (FIName = nil) then begin
    edName.Text := '';
    edSex.ItemIndex := 0;
    edFPatr.Text := '';
    edMPatr.Text := '';
  end else begin
    edName.Text := FIName.Name;
    edSex.ItemIndex := Ord(FIName.Sex);
    edFPatr.Text := FIName.F_Patronymic;
    edMPatr.Text := FIName.M_Patronymic;
  end;
end;

procedure TfmNameEdit.btnAcceptClick(Sender: TObject);
begin
  FIName.Name := edName.Text;
  FIName.Sex := TGEDCOMSex(edSex.ItemIndex);
  FIName.F_Patronymic := edFPatr.Text;
  FIName.M_Patronymic := edMPatr.Text;
end;

procedure TfmNameEdit.edFPatrKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = '/') then begin
    Key := #0;
    MessageBeep(MB_ICONEXCLAMATION);
  end;
end;

end.
