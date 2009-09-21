unit GKFilter;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons;

type
  TfmFilter = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    rgLife: TRadioGroup;
    Label1: TLabel;
    EditName: TEdit;
    rgSex: TRadioGroup;
    procedure btnCancelClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

implementation

uses GKMain, GKCommon, GedCom551;

{$R *.dfm}

procedure TfmFilter.btnCancelClick(Sender: TObject);
begin
  fmGEDKeeper.FilterLifeMode := lmAll;
  fmGEDKeeper.FilterSex := svNone;
  fmGEDKeeper.FilterName := '*';
end;

procedure TfmFilter.btnAcceptClick(Sender: TObject);
begin
  fmGEDKeeper.FilterLifeMode := TLifeMode(rgLife.ItemIndex);
  fmGEDKeeper.FilterSex := TGEDCOMSex(rgSex.ItemIndex);
  fmGEDKeeper.FilterName := EditName.Text;
end;

procedure TfmFilter.FormShow(Sender: TObject);
begin
  rgLife.ItemIndex := Ord(fmGEDKeeper.FilterLifeMode);
  rgSex.ItemIndex := Ord(fmGEDKeeper.FilterSex);
  EditName.Text := fmGEDKeeper.FilterName;
end;

end.
