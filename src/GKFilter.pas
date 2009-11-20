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
    EditName: TEdit;
    rgSex: TRadioGroup;
    Label2: TLabel;
    edAliveBeforeDate: TMaskEdit;
    GroupBox1: TGroupBox;
    CheckPatriarch: TCheckBox;
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
  fmGEDKeeper.Filter.LifeMode := lmAll;
  fmGEDKeeper.Filter.Name := '*';
  fmGEDKeeper.Filter.AliveBeforeDate := '';
  fmGEDKeeper.Filter.PatriarchOnly := False;
  fmGEDKeeper.Filter.Sex := svNone;

  fmGEDKeeper.ApplyFilter();
end;

procedure TfmFilter.btnAcceptClick(Sender: TObject);
var
  dt: TDateTime;
begin
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

  fmGEDKeeper.ApplyFilter();
end;

procedure TfmFilter.FormShow(Sender: TObject);
begin
  rgLife.ItemIndex := Ord(fmGEDKeeper.Filter.LifeMode);
  rgSex.ItemIndex := Ord(fmGEDKeeper.Filter.Sex);
  EditName.Text := fmGEDKeeper.Filter.Name;
  edAliveBeforeDate.Text := fmGEDKeeper.Filter.AliveBeforeDate;
  CheckPatriarch.Checked := fmGEDKeeper.Filter.PatriarchOnly;
end;

procedure TfmFilter.rgLifeClick(Sender: TObject);
begin
  edAliveBeforeDate.Enabled := (rgLife.ItemIndex = 3);
end;

end.
