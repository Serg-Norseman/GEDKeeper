unit GKTipsDlg;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  TfmTipsDialog = class(TForm)
    Shape1: TShape;
    ShowCheck: TCheckBox;
    NextTipBtn: TButton;
    CancelBtn: TButton;
    Shape2: TShape;
    Shape3: TShape;
    TitleLabel: TLabel;
    Image1: TImage;
    TipWindow: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure NextTipBtnClick(Sender: TObject);
  private
    FTips: TStringList;
  protected
    procedure GetTips();
  public
    class function ShowTipsEx(const ACaption: string;
      ShowTipsChecked: Boolean; Tips: TStrings): Boolean;
  end;

implementation

{$R *.DFM}

procedure TfmTipsDialog.FormCreate(Sender: TObject);
begin
  FTips := TStringList.Create;
end;

procedure TfmTipsDialog.FormDestroy(Sender: TObject);
begin
  FTips.Destroy;
end;

procedure TfmTipsDialog.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TfmTipsDialog.NextTipBtnClick(Sender: TObject);
begin
  GetTips();
end;

procedure TfmTipsDialog.GetTips();
begin
  if (FTips.Count > 0) then begin
    TipWindow.Text := FTips[0];
    FTips.Delete(0);
  end;

  NextTipBtn.Enabled := (FTips.Count > 0);
end;

class function TfmTipsDialog.ShowTipsEx(const ACaption: string; ShowTipsChecked: Boolean;
  Tips: TStrings): Boolean;
var
  f: TfmTipsDialog;
begin
  f := Self.Create(Application);
  try
    f.ShowCheck.Checked := ShowTipsChecked;
    f.Caption := ACaption;
    f.TitleLabel.Caption := ACaption;
    f.FTips.Assign(Tips);
    f.GetTips();
    f.ShowModal;
    Result := f.ShowCheck.Checked;
  finally
    f.Free;
  end;
end;

end.
