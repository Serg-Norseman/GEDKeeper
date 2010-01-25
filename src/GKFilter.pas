unit GKFilter;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, Mask;

type
  TfmFilter = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    rgLife: TRadioGroup;
    Label1: TLabel;
    EditName: TComboBox;
    rgSex: TRadioGroup;
    Label2: TLabel;
    edAliveBeforeDate: TMaskEdit;
    GroupBox1: TGroupBox;
    CheckPatriarch: TCheckBox;
    Label3: TLabel;
    cbResidence: TComboBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rgLifeClick(Sender: TObject);
  private
  public
  end;

implementation

uses GKMain, GKCommon, GedCom551;

{$R *.dfm}

procedure TfmFilter.btnCancelClick(Sender: TObject);
begin
  fmGEDKeeper.Filter.Clear();
  fmGEDKeeper.ApplyFilter();
end;

procedure TfmFilter.btnAcceptClick(Sender: TObject);
var
  dt: TDateTime;
  fs: string;
begin
  fs := Trim(EditName.Text);
  if (fs <> '') and (fs <> '*') then begin
    if (fmGEDKeeper.Options.NameFilters.IndexOf(fs) < 0)
    then fmGEDKeeper.Options.NameFilters.Add(fs);
  end;

  fs := Trim(cbResidence.Text);
  if (fs <> '') and (fs <> '*') then begin
    if (fmGEDKeeper.Options.ResidenceFilters.IndexOf(fs) < 0)
    then fmGEDKeeper.Options.ResidenceFilters.Add(fs);
  end;
  //

  fmGEDKeeper.Filter.AliveBeforeDate := edAliveBeforeDate.Text;
  fmGEDKeeper.Filter.PatriarchOnly := CheckPatriarch.Checked;

  if (rgLife.ItemIndex = 3) then begin
    try
      dt := StrToDate(edAliveBeforeDate.Text);
    except
      MessageDlg('Дата неверна', mtError, [mbOk], 0);
      ModalResult := mrNone;
    end;
  end;

  fmGEDKeeper.Filter.LifeMode := TLifeMode(rgLife.ItemIndex);
  fmGEDKeeper.Filter.Sex := TGEDCOMSex(rgSex.ItemIndex);

  if (EditName.Text = '') then EditName.Text := '*';
  fmGEDKeeper.Filter.Name := EditName.Text;

  if (cbResidence.Text = '') then cbResidence.Text := '*';
  fmGEDKeeper.Filter.Residence := cbResidence.Text;

  fmGEDKeeper.ApplyFilter();
end;

procedure TfmFilter.FormShow(Sender: TObject);
begin
  EditName.Items.Assign(fmGEDKeeper.Options.NameFilters);
  cbResidence.Items.Assign(fmGEDKeeper.Options.ResidenceFilters);

  rgLife.ItemIndex := Ord(fmGEDKeeper.Filter.LifeMode);
  rgSex.ItemIndex := Ord(fmGEDKeeper.Filter.Sex);
  EditName.Text := fmGEDKeeper.Filter.Name;
  cbResidence.Text := fmGEDKeeper.Filter.Residence;
  edAliveBeforeDate.Text := fmGEDKeeper.Filter.AliveBeforeDate;
  CheckPatriarch.Checked := fmGEDKeeper.Filter.PatriarchOnly;
end;

procedure TfmFilter.rgLifeClick(Sender: TObject);
begin
  edAliveBeforeDate.Enabled := (rgLife.ItemIndex = 3);
end;

end.
