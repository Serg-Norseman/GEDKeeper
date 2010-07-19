unit GKExpCalc;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExpCalc;

type
  TfmCalcWidget = class(TForm)
    ListOutput: TListBox;
    edExpression: TEdit;
    chkPutToClipboard: TCheckBox;
    edCalcResult: TEdit;
    procedure edExpressionKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edCalcResultDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    calc: TCalculator;
    procedure OnModalBegin(Sender: TObject);
  public
  end;

var
  fmCalcWidget: TfmCalcWidget;

implementation

uses Clipbrd, GKMain;

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
      on E: Exception do res := '[ошибка]: ' + E.Message;
    end;

    ListOutput.Items.Add('> ' + edExpression.Text);
    ListOutput.Items.Add('= ' + res);
    ListOutput.ItemIndex := ListOutput.Items.Count - 1;
    edCalcResult.Text := res;
  end;
end;

procedure TfmCalcWidget.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fmGEDKeeper.actExpCalc.Checked := False;
  fmCalcWidget := nil;
  Action := caFree;
end;

procedure TfmCalcWidget.FormCreate(Sender: TObject);
begin
  calc := TCalculator.Create;

  (*
      OnModalBegin only occurs for the first modal form. That is,
      if the modal form displays another modal form, there is no
      second OnModalBegin event when the second modal form is launched.
  *)
  Application.OnModalBegin := OnModalBegin;
end;

procedure TfmCalcWidget.FormDestroy(Sender: TObject);
begin
  Application.OnModalBegin := nil;

  calc.Destroy;
end;

procedure TfmCalcWidget.OnModalBegin(Sender: TObject);
begin
  PostMessage(Application.MainForm.Handle, WM_KEEPMODELESS, 0, 0);
end;

procedure TfmCalcWidget.edCalcResultDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (Sender is TCustomEdit) and (Source is TCustomEdit) then begin
    Accept := False;
  end;
end;

end.
