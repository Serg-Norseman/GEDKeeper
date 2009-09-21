unit GKLifeMain;

interface

uses
  Buttons, Classes, ComCtrls, Controls, Dialogs, ExtCtrls, Forms, Menus, Messages,
  StdCtrls, ULife, Graphics, ToolWin;

type
  TfmLife = class(TForm)
    cmpLife: TLife;
    pnlInformation: TPanel;
    lblGeneration: TLabel;
    lblGenerationValue: TLabel;
    lblLivingCells: TLabel;
    lblLivingCellsValue: TLabel;
    barStatusLine: TStatusBar;
    tmrNextGeneration: TTimer;
    ToolBar1: TToolBar;
    tbStep: TToolButton;
    tbStart: TToolButton;
    ToolButton4: TToolButton;
    btnSetCells: TToolButton;
    tbClear: TToolButton;
    tbRandomise: TToolButton;
    ToolButton2: TToolButton;
    tbOptions: TToolButton;
    procedure tbRandomiseClick(Sender: TObject);
    procedure cmpLifeChange(Sender: TObject);
    procedure cmpLifeDoesCellLive(const X: TGridWidthRange; const Y: TGridHeightRange;
                                  const Grid: TLifeGrid; var Result: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure Minimised(Sender: TObject);
    procedure Restored(Sender: TObject);
    procedure ShowHint(Sender: TObject);
    procedure tmrNextGenerationTimer(Sender: TObject);
    procedure tbOptionsClick(Sender: TObject);
    procedure tbStepClick(Sender: TObject);
    procedure tbStartClick(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure tbClearClick(Sender: TObject);
  private
    FIsMinimised: Boolean;
    procedure PatternStabilised(const Periodicity: Cardinal);
    procedure ReadOptions;
    procedure UpdateMenusAndButtons;
  protected
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
  end;

var
  fmLife: TfmLife;

implementation

uses
  GKLifeSettings, SysUtils, UOptions, Windows;

resourcestring
  PatternStabilisedTitle = 'Стабильность образца';
  RepeatingPattern = 'Образец повторяется через каждые %d поколений!';
  StaticPattern = 'Образец статичен!';

{$R *.DFM}

procedure TfmLife.tbRandomiseClick(Sender: TObject);
var
  x: Integer;
  y: Integer;
begin
  with cmpLife do
    for x := 0 to GridWidth - 1 do
      for y := 0 to GridHeight - 1 do
        if Random < 0.5 then
          Cells[x, y] := True
        else
          Cells[x, y] := False
end;

procedure TfmLife.cmpLifeChange(Sender: TObject);
begin
  lblGenerationValue.Caption := ' ' + IntToStr(cmpLife.Generation);
  lblLivingCellsValue.Caption := ' ' + IntToStr(cmpLife.LiveCellCount);
  if FIsMinimised then
    Application.Title := Caption + ' [Поколение ' + IntToStr(cmpLife.Generation) + ']';
  UpdateMenusAndButtons
end;

procedure TfmLife.cmpLifeDoesCellLive(const X: TGridWidthRange; const Y: TGridHeightRange;
                                       const Grid: TLifeGrid; var Result: Boolean);
begin
  if Grid[X, Y] then
    Result := Rules.LiveCells[Grid.NumberOfNeighbours(X, Y)]
  else
    Result := Rules.DeadCells[Grid.NumberOfNeighbours(X, Y)]
end;

procedure TfmLife.FormCreate(Sender: TObject);
begin
  Application.OnHint := ShowHint;
  Application.OnMinimize := Minimised;
  Application.OnRestore := Restored;

  ReadOptions;

  UpdateMenusAndButtons;
  Randomize
end;

procedure TfmLife.Minimised(Sender: TObject);
begin
  FIsMinimised := True
end;

procedure TfmLife.PatternStabilised(const Periodicity: Cardinal);
var
  msg: String;
begin
  if (Periodicity = 1) then
    msg := StaticPattern
  else
    msg := Format(RepeatingPattern, [Periodicity]);
  MessageBeep(MB_ICONASTERISK);
  Application.MessageBox(PChar(msg), PChar(PatternStabilisedTitle), MB_ICONINFORMATION or MB_OK)
end;

procedure TfmLife.ReadOptions;
begin
  cmpLife.GridHeight := Options.GridHeight;
  cmpLife.GridWidth := Options.GridWidth;
  cmpLife.Color := Options.BackgroundColor;
  cmpLife.CellColor := Options.LivingCellColor;
  cmpLife.GridLineColor := cmpLife.CellColor;
  pnlInformation.Visible := Options.DisplayGeneration or Options.DisplayLivingCells;
  if pnlInformation.Visible then begin
    pnlInformation.Color := Options.BackgroundColor;
    lblGenerationValue.Visible := Options.DisplayGeneration;
    lblGeneration.Visible := Options.DisplayGeneration;
    if Options.DisplayGeneration then begin
      lblGeneration.Font.Color := Options.DisplayedInformationColor;
      lblGenerationValue.Font.Color := Options.DisplayedInformationColor
    end;
    lblLivingCells.Visible := Options.DisplayLivingCells;
    lblLivingCellsValue.Visible := Options.DisplayLivingCells;
    if Options.DisplayLivingCells then begin
      lblLivingCells.Font.Color := Options.DisplayedInformationColor;
      lblLivingCellsValue.Font.Color := Options.DisplayedInformationColor
    end
  end;
  tmrNextGeneration.Interval := Options.AnimationDelay
end;

procedure TfmLife.Restored(Sender: TObject);
begin
  FIsMinimised := False;
  Application.Title := Caption
end;

procedure TfmLife.ShowHint(Sender: TObject);
begin
  barStatusLine.SimpleText := Application.Hint
end;

procedure TfmLife.tmrNextGenerationTimer(Sender: TObject);
var
  Periodicity: Cardinal;
begin
  Periodicity := cmpLife.NextGeneration;
  if (Periodicity > 0) then begin
    tmrNextGeneration.Enabled := False;
    tbStart.Down := False;
    UpdateMenusAndButtons;
    PatternStabilised(Periodicity)
  end
end;

procedure TfmLife.UpdateMenusAndButtons;
begin
  tbOptions.Enabled := not tbStart.Down and not btnSetCells.Down;
  tbStep.Enabled := not tbStart.Down and not btnSetCells.Down;
  with tbStart do begin
    Enabled := not btnSetCells.Down;
    if Down then begin
      Caption := 'Стоп';
      Hint := 'Остановка хода поколений'
    end else begin
      Caption := 'Старт';
      Hint := 'Автоматическое прохождение поколений'
    end
  end;
  btnSetCells.Enabled := not tbStart.Down;
  tbClear.Enabled := btnSetCells.Down and (cmpLife.LiveCellCount > 0);
  tbRandomise.Enabled := btnSetCells.Down;
  with cmpLife do
    if AcceptMouseClicks then
      Cursor := crHandPoint
    else
      Cursor := crDefault
end;

procedure TfmLife.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
begin
  inherited;
  with Message.MinMaxInfo^.ptMinTrackSize do begin
    x := 800;
    y := 600
  end
end;

procedure TfmLife.tbOptionsClick(Sender: TObject);
var
  fmLifeSettings: TfmLifeSettings;
begin
  fmLifeSettings := TfmLifeSettings.Create(Self);
  try
    if (fmLifeSettings.ShowModal = mrOK) then begin
      ReadOptions;
      if Rules.Modified then
        cmpLife.ResetGeneration
    end
  finally
    fmLifeSettings.Free;
  end;
end;

procedure TfmLife.tbStepClick(Sender: TObject);
var
  Periodicity: Cardinal;
begin
  Periodicity := cmpLife.NextGeneration;
  if Periodicity > 0 then
    PatternStabilised(Periodicity)
end;

procedure TfmLife.tbStartClick(Sender: TObject);
begin
  tmrNextGeneration.Enabled := tbStart.Down;
  UpdateMenusAndButtons
end;

procedure TfmLife.ToolButton5Click(Sender: TObject);
begin
  with cmpLife do begin
    AcceptMouseClicks := btnSetCells.Down;
    ShowGridLines := AcceptMouseClicks
  end
end;

procedure TfmLife.tbClearClick(Sender: TObject);
begin
  cmpLife.ClearCells;
end;

end.
