unit UOptions;

interface

uses
  Graphics;

const
  RegistryKey = 'Software\Ian Lane\Life';

type
  TOptions = class
  private
    FAnimationDelay: Integer;
    FBackgroundColor: TColor;
    FDisplayedInformationColor: TColor;
    FDisplayGeneration: Boolean;
    FDisplayLivingCells: Boolean;
    FGridHeight: Cardinal;
    FGridWidth: Cardinal;
    FLivingCellColor: TColor;
    FModified: Boolean;
    procedure SetAnimationDelay(const Value: Integer);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetDisplayGeneration(const Value: Boolean);
    procedure SetDisplayedInformationColor(const Value: TColor);
    procedure SetDisplayLivingCells(const Value: Boolean);
    procedure SetGridHeight(const Value: Cardinal);
    procedure SetGridWidth(const Value: Cardinal);
    procedure SetLivingCellColor(const Value: TColor);
  public
    constructor Create;
    procedure LoadFromRegistry;
    procedure RestoreDefaults;
    procedure SaveToRegistry;
    property AnimationDelay: Integer read FAnimationDelay write SetAnimationDelay;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property DisplayedInformationColor: TColor read FDisplayedInformationColor
                                                write SetDisplayedInformationColor;
    property DisplayGeneration: Boolean read FDisplayGeneration write SetDisplayGeneration;
    property DisplayLivingCells: Boolean read FDisplayLivingCells write SetDisplayLivingCells;
    property GridHeight: Cardinal read FGridHeight write SetGridHeight;
    property GridWidth: Cardinal read FGridWidth write SetGridWidth;
    property LivingCellColor: TColor read FLivingCellColor write SetLivingCellColor;
    property Modified: Boolean read FModified;
  end;

type
  TRuleArray = array[0..8] of Boolean;

  TRules = class
  private
    FDeadCells: TRuleArray;
    FLiveCells: TRuleArray;
    FModified: Boolean;
    function GetDeadCells(const Index: Integer): Boolean;
    function GetLiveCells(const Index: Integer): Boolean;
    procedure SetDeadCells(const Index: Integer; const Value: Boolean);
    procedure SetLiveCells(const Index: Integer; const Value: Boolean);
  public
    constructor Create;
    procedure LoadFromRegistry;
    procedure RestoreDefaults;
    procedure SaveToRegistry;
    property DeadCells[const Index: Integer]: Boolean read GetDeadCells write SetDeadCells;
    property LiveCells[const Index: Integer]: Boolean read GetLiveCells write SetLiveCells;
    property Modified: Boolean read FModified;
  end;

var
  Options: TOptions;
  Rules: TRules;

implementation

uses
  Registry;

const
  DefaultAnimationDelay = 750;
  DefaultBackgroundColor = clBlack;
  DefaultDisplayGeneration = True;
  DefaultDisplayLivingCells = True;
  DefaultGridHeight = 80;
  DefaultGridWidth = 60;
  DefaultLivingCellColor = clLime;
  DefaultDisplayedInformationColor = clYellow;

const
  DefaultDeadCells: TRuleArray = (False, False, False, True, False, False, False, False, False);
  DefaultLiveCells: TRuleArray = (False, False, True, True, False, False, False, False, False);

{ TOptions }

constructor TOptions.Create;
begin
  inherited Create;
  RestoreDefaults;
  LoadFromRegistry
end;

procedure TOptions.LoadFromRegistry;
var
  Registry: TRegistry;

  function ReadInteger(const Key: String; const Default: Integer): Integer;
  begin
    try
      Result := Registry.ReadInteger(Key)
    except
      Result := Default
    end
  end;

  function ReadRestrictedInteger(const Key: String; const Min, Max, Default: Integer): Integer;
  begin
    Result := ReadInteger(Key, Default);
    if Result < Min then
      Result := Min
    else
      if Result > Max then
        Result := Max
  end;

begin { TOptions.LoadFromRegistry }
  Registry := TRegistry.Create;
  try
    if Registry.OpenKey(RegistryKey + '\Grid Size', False) then
    try
      FGridHeight := ReadRestrictedInteger('Down', 10, 100, DefaultGridHeight);
      FGridWidth := ReadRestrictedInteger('Across', 10, 100, DefaultGridWidth)
    finally
      Registry.CloseKey
    end;
    if Registry.OpenKey(RegistryKey + '\Color', False) then
    try
      FBackgroundColor := ReadInteger('Background', DefaultBackgroundColor);
      FLivingCellColor := ReadInteger('Living Cells', DefaultLivingCellColor)
    finally
      Registry.CloseKey
    end;
    if Registry.OpenKey(RegistryKey + '\Information', False) then
    try
      try
        FDisplayGeneration := Registry.ReadBool('Display Generation')
      except
        FDisplayGeneration := DefaultDisplayGeneration
      end;
      try
        FDisplayLivingCells := Registry.ReadBool('Display Living Cells')
      except
        FDisplayLivingCells := DefaultDisplayLivingCells
      end;
      FDisplayedInformationColor := ReadInteger('Color', DefaultDisplayedInformationColor)
    finally
      Registry.CloseKey
    end;
    if Registry.OpenKey(RegistryKey, False) then
    try
      FAnimationDelay := ReadRestrictedInteger('Animation Delay', 0, 30000, DefaultAnimationDelay)
    finally
      Registry.CloseKey
    end;
    FModified := False
  finally
    Registry.Free
  end
end;

procedure TOptions.RestoreDefaults;
begin
  AnimationDelay := DefaultAnimationDelay;
  BackgroundColor := DefaultBackgroundColor;
  DisplayedInformationColor := DefaultDisplayedInformationColor;
  DisplayGeneration := DefaultDisplayGeneration;
  DisplayLivingCells := DefaultDisplayLivingCells;
  GridHeight := DefaultGridHeight;
  GridWidth := DefaultGridWidth;
  LivingCellColor := DefaultLivingCellColor
end;

procedure TOptions.SaveToRegistry;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    if Registry.OpenKey(RegistryKey + '\Grid Size', True) then
    try
      Registry.WriteInteger('Down', GridHeight);
      Registry.WriteInteger('Across', GridWidth)
    finally
      Registry.CloseKey
    end;
    if Registry.OpenKey(RegistryKey + '\Color', True) then
    try
      Registry.WriteInteger('Background', BackgroundColor);
      Registry.WriteInteger('Living Cells', LivingCellColor)
    finally
      Registry.CloseKey
    end;
    if Registry.OpenKey(RegistryKey + '\Information', True) then
    try
      Registry.WriteBool('Display Generation', DisplayGeneration);
      Registry.WriteBool('Display Living Cells', DisplayLivingCells);
      Registry.WriteInteger('Color', DisplayedInformationColor)
    finally
      Registry.CloseKey
    end;
    if Registry.OpenKey(RegistryKey, True) then
    try
      Registry.WriteInteger('Animation Delay', AnimationDelay)
    finally
      Registry.CloseKey
    end;
    FModified := True
  finally
    Registry.Free
  end
end;

procedure TOptions.SetAnimationDelay(const Value: Integer);
begin
  if (Value <> FAnimationDelay) then begin
    FAnimationDelay := Value;
    FModified := True
  end
end;

procedure TOptions.SetBackgroundColor(const Value: TColor);
begin
  if (Value <> FBackgroundColor) then begin
    FBackgroundColor := Value;
    FModified := True
  end
end;

procedure TOptions.SetDisplayGeneration(const Value: Boolean);
begin
  if (Value <> FDisplayGeneration) then begin
    FDisplayGeneration := Value;
    FModified := True
  end
end;

procedure TOptions.SetDisplayedInformationColor(const Value: TColor);
begin
  if (Value <> FDisplayedInformationColor) then begin
    FDisplayedInformationColor := Value;
    FModified := True
  end
end;

procedure TOptions.SetDisplayLivingCells(const Value: Boolean);
begin
  if (Value <> FDisplayLivingCells) then begin
    FDisplayLivingCells := Value;
    FModified := True
  end
end;

procedure TOptions.SetGridHeight(const Value: Cardinal);
begin
  if (Value <> FGridHeight) then begin
    FGridHeight := Value;
    FModified := True
  end
end;

procedure TOptions.SetGridWidth(const Value: Cardinal);
begin
  if (Value <> FGridWidth) then begin
    FGridWidth := Value;
    FModified := True
  end
end;

procedure TOptions.SetLivingCellColor(const Value: TColor);
begin
  if (Value <> FLivingCellColor) then begin
    FLivingCellColor := Value;
    FModified := True
  end
end;

{ TRules }

constructor TRules.Create;
begin
  inherited Create;
  LoadFromRegistry
end;

function TRules.GetDeadCells(const Index: Integer): Boolean;
begin
  Result := FDeadCells[Index]
end;

function TRules.GetLiveCells(const Index: Integer): Boolean;
begin
  Result := FLiveCells[Index]
end;

procedure TRules.LoadFromRegistry;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    if Registry.OpenKey(RegistryKey + '\Rules', False) then
    try
      try
        Registry.ReadBinaryData('Dead Cells', FDeadCells, SizeOf(FDeadCells));
      except
        FDeadCells := DefaultDeadCells
      end;
      try
        Registry.ReadBinaryData('Live Cells', FLiveCells, SizeOf(FLiveCells))
      except
        FLiveCells := DefaultLiveCells
      end
    finally
      Registry.CloseKey
    end
    else
      RestoreDefaults;
    FModified := False
  finally
    Registry.Free
  end
end;

procedure TRules.RestoreDefaults;
begin
  FDeadCells := DefaultDeadCells;
  FLiveCells := DefaultLiveCells;
  FModified := True
end;

procedure TRules.SaveToRegistry;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    if Registry.OpenKey(RegistryKey + '\Rules', True) then
    try
      Registry.WriteBinaryData('Dead Cells', FDeadCells, SizeOf(FDeadCells));
      Registry.WriteBinaryData('Live Cells', FLiveCells, SizeOf(FLiveCells))
    finally
      Registry.CloseKey
    end
  finally
    Registry.Free
  end
end;

procedure TRules.SetDeadCells(const Index: Integer; const Value: Boolean);
begin
  if (Value <> FDeadCells[Index]) then begin
    FDeadCells[Index] := Value;
    FModified := True
  end
end;

procedure TRules.SetLiveCells(const Index: Integer; const Value: Boolean);
begin
  if (Value <> FLiveCells[Index]) then begin
    FLiveCells[Index] := Value;
    FModified := True
  end
end;

initialization
  Options := TOptions.Create;
  Rules := TRules.Create;

finalization
  Options.Free;
  Rules.Free;

end.
