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
    SheetChart: TTabSheet;
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
    procedure FormShow(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure PanMaleColorClick(Sender: TObject);
  private
    FOptions: TGlobalOptions;
  public
    property Options: TGlobalOptions read FOptions write FOptions;
  end;

implementation

uses GedCom551, GKUtils, GKMain;

{$R *.dfm}

procedure TfmOptions.FormShow(Sender: TObject);
begin
  rgCode.ItemIndex := Ord(fmGEDKeeper.FDefCharacterSet);
  rgFNPFormat.ItemIndex := Ord(fmGEDKeeper.DefNameFormat);
  rgDateFormat.ItemIndex := Ord(fmGEDKeeper.DefDateFormat);

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

  CheckAppRegister.Checked := ProgramIsRegistered();
  CheckExtRegister.Checked := ExtIsRegistered('.ged', 'GEDCOM.File');

  CheckCleanEmptyFamilies.Checked := FOptions.CleanEmptyFamilies;
end;

procedure TfmOptions.btnAcceptClick(Sender: TObject);
begin
  fmGEDKeeper.FDefCharacterSet := TGEDCOMCharacterSet(rgCode.ItemIndex);
  fmGEDKeeper.DefNameFormat := TNameFormat(rgFNPFormat.ItemIndex);
  fmGEDKeeper.DefDateFormat := TDateFormat(rgDateFormat.ItemIndex);

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

  RegisterProgram(CheckAppRegister.Checked);
  RegisterExt('.ged', 'GEDCOM.File', 'GEDCOM File', 0, CheckExtRegister.Checked);

  FOptions.CleanEmptyFamilies := CheckCleanEmptyFamilies.Checked;
end;

procedure TfmOptions.PanMaleColorClick(Sender: TObject);
begin
  ColorDialog1.Color := TPanel(Sender).Color;

  if ColorDialog1.Execute
  then TPanel(Sender).Color := ColorDialog1.Color;
end;

end.
