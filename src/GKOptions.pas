unit GKOptions;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, GKCommon;

type
  TfmOptions = class(TForm)
    PageControl1: TPageControl;
    SheetCommon: TTabSheet;
    rgCode: TRadioGroup;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    rgFNPFormat: TRadioGroup;
    SheetPedigree: TTabSheet;
    GroupBox1: TGroupBox;
    CheckFamily: TCheckBox;
    CheckName: TCheckBox;
    CheckPatronymic: TCheckBox;
    CheckDiffLines: TCheckBox;
    CheckBirthDate: TCheckBox;
    CheckDeathDate: TCheckBox;
    CheckKinship: TCheckBox;
    GroupBox2: TGroupBox;
    PanMaleColor: TPanel;
    PanFemaleColor: TPanel;
    PanUnkSexColor: TPanel;
    PanUnHusbandColor: TPanel;
    PanUnWifeColor: TPanel;
    ColorDialog1: TColorDialog;
    rgDateFormat: TRadioGroup;
    GroupBox3: TGroupBox;
    CheckAppRegister: TCheckBox;
    CheckExtRegister: TCheckBox;
    SheetTools: TTabSheet;
    CheckCleanEmptyFamilies: TCheckBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    chkProxy: TCheckBox;
    edProxyServer: TEdit;
    edProxyPort: TEdit;
    edProxyLogin: TEdit;
    edProxyPass: TEdit;
    GroupBox5: TGroupBox;
    CheckAttributes: TCheckBox;
    CheckNotes: TCheckBox;
    CheckSources: TCheckBox;
    CheckPlacesWithAddress: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure PanMaleColorClick(Sender: TObject);
  private
    FOptions: TGlobalOptions;
  public
    property Options: TGlobalOptions read FOptions write FOptions;
  end;

implementation

uses GedCom551, GKMain, bsWinUtils;

{$R *.dfm}

procedure TfmOptions.FormShow(Sender: TObject);
begin
  case FOptions.DefCharacterSet of
    csASCII: rgCode.ItemIndex := 0;
    csUTF8: rgCode.ItemIndex := 1;
  end;
  
  rgFNPFormat.ItemIndex := Ord(FOptions.DefNameFormat);
  rgDateFormat.ItemIndex := Ord(FOptions.DefDateFormat);
  CheckPlacesWithAddress.Checked := FOptions.PlacesWithAddress;

  CheckFamily.Checked := FOptions.ChartOptions.FamilyVisible;
  CheckName.Checked := FOptions.ChartOptions.NameVisible;
  CheckPatronymic.Checked := FOptions.ChartOptions.PatronymicVisible;
  CheckDiffLines.Checked := FOptions.ChartOptions.DiffLines;
  CheckBirthDate.Checked := FOptions.ChartOptions.BirthDateVisible;
  CheckDeathDate.Checked := FOptions.ChartOptions.DeathDateVisible;
  CheckKinship.Checked := FOptions.ChartOptions.Kinship;

  PanMaleColor.Color := FOptions.ChartOptions.MaleColor;
  PanFemaleColor.Color := FOptions.ChartOptions.FemaleColor;
  PanUnkSexColor.Color := FOptions.ChartOptions.UnkSexColor;
  PanUnHusbandColor.Color := FOptions.ChartOptions.UnHusbandColor;
  PanUnWifeColor.Color := FOptions.ChartOptions.UnWifeColor;

  {$IFNDEF DELPHI_NET}
  CheckAppRegister.Checked := ProgramIsRegistered();
  CheckExtRegister.Checked := ExtIsRegistered('.ged', 'GEDCOM.File');
  {$ELSE}
  CheckAppRegister.Enabled := False;
  CheckExtRegister.Enabled := False;
  {$ENDIF}

  CheckCleanEmptyFamilies.Checked := FOptions.CleanEmptyFamilies;

  chkProxy.Checked := FOptions.Proxy.UseProxy;
  edProxyServer.Text := FOptions.Proxy.Server;
  edProxyPort.Text := FOptions.Proxy.Port;
  edProxyLogin.Text := FOptions.Proxy.Login;
  edProxyPass.Text := FOptions.Proxy.Password;

  CheckAttributes.Checked := FOptions.PedigreeOptions.IncludeAttributes;
  CheckNotes.Checked := FOptions.PedigreeOptions.IncludeNotes;
  CheckSources.Checked := FOptions.PedigreeOptions.IncludeSources;
end;

procedure TfmOptions.btnAcceptClick(Sender: TObject);
begin
  case rgCode.ItemIndex of
    0: FOptions.DefCharacterSet := csASCII;
    1: FOptions.DefCharacterSet := csUTF8;
  end;

  FOptions.DefNameFormat := TNameFormat(rgFNPFormat.ItemIndex);
  FOptions.DefDateFormat := TDateFormat(rgDateFormat.ItemIndex);
  FOptions.PlacesWithAddress := CheckPlacesWithAddress.Checked;

  FOptions.ChartOptions.FamilyVisible := CheckFamily.Checked;
  FOptions.ChartOptions.NameVisible := CheckName.Checked;
  FOptions.ChartOptions.PatronymicVisible := CheckPatronymic.Checked;
  FOptions.ChartOptions.DiffLines := CheckDiffLines.Checked;
  FOptions.ChartOptions.BirthDateVisible := CheckBirthDate.Checked;
  FOptions.ChartOptions.DeathDateVisible := CheckDeathDate.Checked;
  FOptions.ChartOptions.Kinship := CheckKinship.Checked;

  FOptions.ChartOptions.MaleColor := PanMaleColor.Color;
  FOptions.ChartOptions.FemaleColor := PanFemaleColor.Color;
  FOptions.ChartOptions.UnkSexColor := PanUnkSexColor.Color;
  FOptions.ChartOptions.UnHusbandColor := PanUnHusbandColor.Color;
  FOptions.ChartOptions.UnWifeColor := PanUnWifeColor.Color;

  {$IFNDEF DELPHI_NET}
  RegisterProgram(CheckAppRegister.Checked);
  RegisterExt('.ged', 'GEDCOM.File', 'GEDCOM File', 0, CheckExtRegister.Checked);
  {$ENDIF}

  FOptions.CleanEmptyFamilies := CheckCleanEmptyFamilies.Checked;

  FOptions.Proxy.UseProxy := chkProxy.Checked;
  FOptions.Proxy.Server := edProxyServer.Text;
  FOptions.Proxy.Port := edProxyPort.Text;
  FOptions.Proxy.Login := edProxyLogin.Text;
  FOptions.Proxy.Password := edProxyPass.Text;

  FOptions.PedigreeOptions.IncludeAttributes := CheckAttributes.Checked;
  FOptions.PedigreeOptions.IncludeNotes := CheckNotes.Checked;
  FOptions.PedigreeOptions.IncludeSources := CheckSources.Checked;
end;

procedure TfmOptions.PanMaleColorClick(Sender: TObject);
begin
  ColorDialog1.Color := TPanel(Sender).Color;

  if ColorDialog1.Execute
  then TPanel(Sender).Color := ColorDialog1.Color;
end;

end.
