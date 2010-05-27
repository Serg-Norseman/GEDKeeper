unit GKExpCalc;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExpCalc;

type
  TfmCalcWidget = class(TForm)
    ListOutput: TListBox;
    edExpression: TEdit;
    chkPutToClipboard: TCheckBox;
    procedure edExpressionKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    calc: TCalculator;
  public
  end;

var
  fmCalcWidget: TfmCalcWidget;

implementation

uses GKMain, Clipbrd;

{$R *.dfm}

procedure TfmCalcWidget.edExpressionKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  res: string;
begin
  if (Key = VK_RETURN) then begin
    try
      calc.Expression := edExpression.Text;
      res := FloatToStr(calc.Result);

      if (chkPutToClipboard.Checked)
      then Clipboard.AsText := res;      
    except
      res := '[ошибка]';
    end;

    ListOutput.Items.Add('= ' + edExpression.Text);
    ListOutput.Items.Add('> ' + res);
    ListOutput.ItemIndex := ListOutput.Items.Count - 1;
  end;
end;

procedure TfmCalcWidget.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fmGEDKeeper.miCalc.Checked := False;
end;

procedure TfmCalcWidget.FormCreate(Sender: TObject);
begin
  calc := TCalculator.Create;
end;

procedure TfmCalcWidget.FormDestroy(Sender: TObject);
begin
  calc.Destroy;
end;

end.
