unit GKExpCalc; {prepare:fin; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls
  {$IFNDEF DELPHI_NET}, ExpCalc {$ENDIF}, GKLangs;

type
  TfmCalcWidget = class(TForm, ILocalization)
    lbOutput: TListBox;
    edExpression: TEdit;
    chkPutToClipboard: TCheckBox;
    edCalcResult: TEdit;
    procedure edExpressionKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edCalcResultDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure edExpressionKeyPress(Sender: TObject; var Key: Char);
  private
    {$IFNDEF DELPHI_NET}
    calc: TCalculator;
    {$ENDIF}
    last_key: Char;
    procedure OnModalBegin(Sender: TObject);
  public
    procedure SetLang();
  end;

var
  fmCalcWidget: TfmCalcWidget;

implementation

uses Clipbrd, GKMain;

{$R *.dfm}

procedure TfmCalcWidget.FormCreate(Sender: TObject);
begin
  {$IFNDEF DELPHI_NET}
  calc := TCalculator.Create;
  {$ENDIF}

  (*
      OnModalBegin only occurs for the first modal form. That is,
      if the modal form displays another modal form, there is no
      second OnModalBegin event when the second modal form is launched.
  *)
  Application.OnModalBegin := OnModalBegin;

  SetLang();
end;

procedure TfmCalcWidget.FormDestroy(Sender: TObject);
begin
  Application.OnModalBegin := nil;

  {$IFNDEF DELPHI_NET}
  calc.Destroy;
  {$ENDIF}
end;

procedure TfmCalcWidget.SetLang();
begin
  Caption := LSList[LSID_MICalc];
  chkPutToClipboard.Caption := LSList[LSID_CopyResultToClipboard];
end;

procedure TfmCalcWidget.edExpressionKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  res: string;
begin
  last_key := Chr(Key);

  if (Key = VK_RETURN) then begin
    try
      {$IFNDEF DELPHI_NET}
      calc.Expression := edExpression.Text;
      res := FloatToStr(calc.Result);
      {$ENDIF}

      if (chkPutToClipboard.Checked)
      then Clipboard.AsText := res;
    except
      on E: Exception do res := '[ошибка]: ' + E.Message;
    end;

    lbOutput.Items.Add('> ' + edExpression.Text);
    lbOutput.Items.Add('= ' + res);
    lbOutput.ItemIndex := lbOutput.Items.Count - 1;
    edCalcResult.Text := res;
  end;
end;

procedure TfmCalcWidget.edExpressionKeyPress(Sender: TObject; var Key: Char);
var
  k: Char;
begin
  k := AnsiLowerCase(Key)[1];
  if (k in ['а'..'я']) then Key := AnsiLowerCase(last_key)[1];
end;

procedure TfmCalcWidget.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fmGEDKeeper.miCalc.Checked := False;
  fmCalcWidget := nil;
  Action := caFree;
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
