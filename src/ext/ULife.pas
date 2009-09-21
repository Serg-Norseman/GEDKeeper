{*******************************************************************************}
{                                                                               }
{ FileName     : ULife.pas                                                      }
{                                                                               }
{ Author       : Ian Lane (Email: lanei@ideal.net.au)                           }
{                                                                               }
{ Synopsis     : A Delphi control which implements the old computer simulation  }
{                of Life. Useful for about boxes, screen savers or even as the  }
{                core of a "Life" application.                                  }
{                                                                               }
{ Distribution : This control is free for public use and components may be      }
{                freely descended from it as long as credit is given to the     }
{                author.                                                        }
{                                                                               }
{                          Copyright (c) 1998 Ian Lane                          }
{                                                                               }
{*******************************************************************************}
unit ULife;

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics;

const
  { The absolute minimum and maximum dimensions of the grid }
  MinGridHeight = 5;
{$IFDEF VER80}
  MaxGridHeight = 250;
{$ELSE}
  MaxGridHeight = 46000;
{$ENDIF}
  MinGridWidth = 5;
{$IFDEF VER80}
  MaxGridWidth = 250;
{$ELSE}
  MaxGridWidth = 46000;
{$ENDIF}
  AbsoluteMaxNumberOfHistoryLevels = MaxInt;

  { Some default values }
  DefaultCellColor = clWindowText;
  DefaultGridLineColor = clWindowText;
  DefaultGridLineStyle = psDot;
  DefaultGridHeight = 10;
  DefaultGridWidth = 10;
  DefaultMaxNumberOfHistoryLevels = 10;

type
  TPermissableGridHeight = MinGridHeight..MaxGridHeight;
  TPermissableGridWidth = MinGridWidth..MaxGridWidth;
  TGridHeightRange = 0..MaxGridHeight - 1;
  TGridWidthRange = 0..MaxGridWidth - 1;

  TGrid = array [0..MaxGridHeight * MaxGridWidth - 1] of Boolean;
  PGrid = ^TGrid;

  TLifeGrid = class
  private
    FGrid: PGrid;
    FGridHeight: TPermissableGridHeight;
    FGridWidth: TPermissableGridWidth;
    procedure AllocGrid;
    function CellCoordToGridOffset(const X: TGridWidthRange;
                                   const Y: TGridHeightRange): Cardinal;
    procedure FreeGrid;
    function GetCell(const X: TGridWidthRange; const Y: TGridHeightRange): Boolean;
    function GetLiveCellCount: Integer;
    procedure SetCell(const X: TGridWidthRange; const Y: TGridHeightRange;
                      const Value: Boolean);
  public
    constructor Create(const GridWidth: TPermissableGridWidth;
                       const GridHeight: TPermissableGridHeight);
    constructor CreateEmpty;
    destructor Destroy; override;

    procedure Assign(const Source: TLifeGrid);
    procedure Clear;
    function DoesCellLive(const X: TGridWidthRange; const Y: TGridHeightRange): Boolean;
    function Equal(const Source: TLifeGrid): Boolean;

    function NumberOfNeighbours(const X: TGridWidthRange;
                                const Y: TGridHeightRange): Integer;

    procedure SetGridSize(const NewGridWidth: TPermissableGridWidth;
                          const NewGridHeight: TPermissableGridHeight);

    property Cells[const X: TGridWidthRange; const Y: TGridHeightRange]: Boolean
             read GetCell write SetCell; default;
    property GridHeight: TPermissableGridHeight read FGridHeight;
    property GridWidth: TPermissableGridWidth read FGridWidth;
    property LiveCellCount: Integer read GetLiveCellCount;
  end;


  TLifeHistory = class
  private
    FList: TList;
    FMostRecent: Integer;
    function GetCount: Cardinal;
    function GetGrid(const Index: Cardinal): TLifeGrid;
    function GetMaxNumberOfLevels: Cardinal;
    procedure SetMaxNumberOfLevels(const Value: Cardinal);
  public
    constructor Create(const NumberOfLevels: Cardinal);
    function Add(const Grid: TLifeGrid): TLifeGrid;
    procedure Clear;
    function Contains(const Grid: TLifeGrid): Integer;

    destructor Destroy; override;
    property Count: Cardinal read GetCount;
    property Grids[const Index: Cardinal]: TLifeGrid read GetGrid; default;
    property MaxNumberOfLevels: Cardinal read GetMaxNumberOfLevels
                                          write SetMaxNumberOfLevels;
  end;
  

  TDoesCellLiveEvent = procedure (const X: TGridWidthRange; const Y: TGridHeightRange;
                                  const Grid: TLifeGrid; var Result: Boolean) of object;
  TLife = class(TCustomControl)
  private
    FAcceptMouseClicks: Boolean;
    FCellColor: TColor;
    FGeneration: Cardinal;
    FGrid: TLifeGrid;
    FGridLineColor: TColor;
    FGridLineStyle: TPenStyle;
    FHistory: TLifeHistory;
    FOnChange: TNotifyEvent;
    FOnDoesCellLive: TDoesCellLiveEvent;
    FShowGridLines: Boolean;
    function CellEdge(const Coordinate, Size, Divisions: Integer): Integer;
    function GetCell(const X: TGridWidthRange; const Y: TGridHeightRange): Boolean;
    function GetGridHeight: TPermissableGridHeight;
    function GetGridWidth: TPermissableGridWidth;
    function GetLiveCellCount: Integer;
    function GetMaxNumberOfHistoryLevels: Cardinal;
    procedure SetAcceptMouseClicks(const Value: Boolean);
    procedure SetCellColor(Const Value: TColor);
    procedure SetCells(const X: TGridWidthRange; const Y: TGridHeightRange;
                       const Value: Boolean);
    procedure SetGridHeight(const NewGridHeight: TPermissableGridHeight);
    procedure SetGridLineColor(Const Value: TColor);
    procedure SetGridLineStyle(Const Value: TPenStyle);
    procedure SetGridWidth(const NewGridWidth: TPermissableGridWidth);
    procedure SetMaxNumberOfHistoryLevels(const Value: Cardinal);
    procedure SetShowGridLines(const Value: Boolean);
  protected
    function CellAtPos(const X, Y: Longint): TPoint;
    function CellCoords(const X: TGridWidthRange; const Y: TGridHeightRange): TRect;
    procedure Change; dynamic;
    function DoesCellLive(const X: TGridWidthRange; const Y: TGridHeightRange;
                          const Grid: TLifeGrid): Boolean; dynamic;
    procedure InvalidateCell(const X: TGridWidthRange; const Y: TGridHeightRange);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure SetCell(const X: TGridWidthRange; const Y: TGridHeightRange;
                      const Value: Boolean);
  public
    constructor Create(aOwner: TComponent); override;
    procedure ClearCells;
    function NextGeneration: Cardinal;

    procedure SetGridSize(const NewGridWidth: TGridWidthRange;
                          const NewGridHeight: TGridHeightRange);
    destructor Destroy; override;
    procedure ResetGeneration; 
    property Cells[const X: TGridWidthRange; const Y: TGridHeightRange]: Boolean
             read GetCell write SetCells;
    property Generation: Cardinal read FGeneration;
    property History: TLifeHistory read FHistory;
    property LiveCellCount: Integer read GetLiveCellCount;
  published
    property AcceptMouseClicks: Boolean read FAcceptMouseClicks write SetAcceptMouseClicks
             default False;
    property Align;
    property CellColor: TColor read FCellColor write SetCellColor default DefaultCellColor;
    property Color;
    property DragCursor;
    property DragMode;
    property GridHeight: TPermissableGridHeight read GetGridHeight write SetGridHeight
             default DefaultGridHeight;
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor
             default DefaultGridLineColor;
    property GridLineStyle: TPenStyle read FGridLineStyle write SetGridLineStyle
             default DefaultGridLineStyle;
    property GridWidth: TPermissableGridWidth read GetGridWidth write SetGridWidth
             default DefaultGridWidth;
    property MaxNumberOfHistoryLevels: Cardinal read GetMaxNumberOfHistoryLevels
                                                 write SetMaxNumberOfHistoryLevels
                                                 default DefaultMaxNumberOfHistoryLevels;
    property ParentColor;
    property PopupMenu;
    property ShowHint;
    property ShowGridLines: Boolean read FShowGridLines write SetShowGridLines
             default False;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDoesCellLive: TDoesCellLiveEvent read FOnDoesCellLive write FOnDoesCellLive;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$ifndef VER80}
    property OnStartDrag;
{$endif}
  end;

procedure Register;

implementation

const
  CurrentVersion = '1.2';
  StreamHeader: array [1..4] of Char = 'Life';
  StreamVersion: Byte = 2;

{ Returns the cell containing the specified pixel. See the CellCoords method for the reason
  why the mathematics are so complicated. }
function TLife.CellAtPos(const X, Y: Longint): TPoint;
var
  CellHeight, CellWidth, Remainder: Integer;
begin
{$IFOPT R+}
  if (X < 0) or (X >= ClientWidth) then
    raise ERangeError.Create('X coordinate is outside the control''s bounds');
  if (Y < 0) or (Y >= ClientHeight) then
    raise ERangeError.Create('Y coordinate is outside the control''s bounds');
{$ENDIF}
  CellWidth := ClientWidth div GridWidth;
  Remainder := ClientWidth mod GridWidth;
  if X <= Remainder * (CellWidth + 1) then
    Result.X := X div (CellWidth + 1)
  else
    Result.X := Remainder + (X - Remainder * (CellWidth + 1)) div CellWidth;
  CellHeight := ClientHeight div GridHeight;
  Remainder := ClientHeight mod GridHeight;
  if Y <= Remainder * (CellHeight + 1) then
    Result.Y := Y div (CellHeight + 1)
  else
    Result.Y := Remainder + (Y - Remainder * (CellHeight + 1)) div CellHeight
end;

{ Returns the bounds of the specified cell. The mathematics for doing this are complicated by
  the fact that (a) we have to allow for the fact that the component dimensions will often
  not be an even multiple of the grid dimensions; and (b) we have to be able to reverse these
  calculations in the CellAtPos method. }
function TLife.CellCoords(const X: TGridWidthRange; const Y: TGridHeightRange): TRect;
begin
{$IFOPT R+}
  if X >= GridWidth then
    raise ERangeError.Create('X parameter out of range');
  if Y >= GridHeight then
    raise ERangeError.Create('Y parameter out of range');
{$ENDIF}
  with Result do begin
    Left := CellEdge(X, ClientWidth, GridWidth);
    Top := CellEdge(Y, ClientHeight, GridHeight);
    Right := CellEdge(X + 1, ClientWidth, GridWidth);
    Bottom := CellEdge(Y + 1, ClientHeight, GridHeight)
  end
end;

{ This function calculates the left or top edge of a cell when supplied with the correct
  information. Coordinate is the cell position (x or y) that you wish to calculate; Size is
  the size of the component in the appropriate direction (ClientWidth or ClientHeight); and
  Divisions is the number of divisions in the appropriate direction (GridWidth or GridHeight). }
function TLife.CellEdge(const Coordinate, Size, Divisions: Integer): Integer;
var
  CellSize, Remainder: Integer;
begin
  CellSize := Size div Divisions;
  Remainder := Size mod Divisions;
  Result := Coordinate * CellSize;
  if Coordinate < Remainder then
    Inc(Result, Coordinate)
  else
    Inc(Result, Remainder)
end;

{ Call the OnChange event. }
procedure TLife.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self)
end;

{ Mark all the cells as "dead". }
procedure TLife.ClearCells;
begin
  FGrid.Clear;
  FHistory.Clear;
  Invalidate;
  Change
end;

constructor TLife.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FGrid := TLifeGrid.Create(DefaultGridWidth, DefaultGridHeight);
  FHistory := TLifeHistory.Create(DefaultMaxNumberOfHistoryLevels);
  FCellColor := DefaultCellColor;
  FGridLineColor := DefaultGridLineColor;
  FGridLineStyle := DefaultGridLineStyle;
  Height := 50;
  Width := 50
end;

destructor TLife.Destroy;
begin
  FGrid.Free;
  FHistory.Free;
  inherited Destroy
end;

{ Applies the Rules of Life to determine if the specified cell should live or die. }
function TLife.DoesCellLive(const X: TGridWidthRange; const Y: TGridHeightRange;
                            const Grid: TLifeGrid): Boolean;
begin
  Result := Grid.DoesCellLive(X, Y);
  if Assigned(FOnDoesCellLive) then
    FOnDoesCellLive(X, Y, Grid, Result)
end;

{ Is the cell at the specified position alive? }
function TLife.GetCell(const X: TGridWidthRange; const Y: TGridHeightRange): Boolean;
begin
  Result := FGrid.Cells[X, Y]
end;

function TLife.GetGridHeight: TPermissableGridHeight;
begin
  Result := FGrid.GridHeight
end;

function TLife.GetGridWidth: TPermissableGridWidth;
begin
  Result := FGrid.GridWidth
end;

{ Returns the number of "live" cells. }
function TLife.GetLiveCellCount: Integer;
begin
  Result := FGrid.LiveCellCount
end;

function TLife.GetMaxNumberOfHistoryLevels: Cardinal;
begin
  Result := FHistory.MaxNumberOfLevels
end;

{ Invalidate the specified cell so that it is redrawn. }
procedure TLife.InvalidateCell(const X: TGridWidthRange; const Y: TGridHeightRange);
var
  Rect: TRect;
begin
{$IFOPT R+}
  if X >= GridWidth then
    raise ERangeError.Create('X parameter out of range');
  if Y >= GridHeight then
    raise ERangeError.Create('Y parameter out of range');
{$ENDIF}
  Rect := CellCoords(X, Y);
  InvalidateRect(Handle, @Rect, True)
end;

procedure TLife.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if AcceptMouseClicks and (Button = mbLeft) then
  begin
    with CellAtPos(X, Y) do
      Cells[X, Y] := not Cells[X, Y]
  end
end;

{ Save the current state of FGrid to FHistory, then determine the new state of FGrid by applying
  the Rules of Life. }
function TLife.NextGeneration: Cardinal;
var
  MostRecentGrid: TLifeGrid;
  x, y: Integer;
begin
  MostRecentGrid := FHistory.Add(FGrid);
  for y := 0 to GridHeight - 1 do
    for x := 0 to GridWidth - 1 do
      SetCell(x, y, DoesCellLive(x, y, MostRecentGrid));
  Inc(FGeneration);
  Change;
  Result := FHistory.Contains(FGrid) + 1
end;

procedure TLife.Paint;
var
  r: TRect;
  x, y: Integer;

  procedure DrawGridLines;
  var
    Coordinate, i: Integer;
  begin
    with Canvas do
    begin
{$IFDEF VER100}
      Brush.Color := Color;
{$ENDIF}
      Pen.Color := GridLineColor;
{$IFDEF VER90}
      Pen.Mode := pmMask;
{$ENDIF}
      Pen.Style := GridLineStyle;
      for i := 1 to GridWidth - 1 do
      begin
        Coordinate := CellEdge(i, ClientWidth, GridWidth);
        Canvas.MoveTo(Coordinate, 0);
        Canvas.LineTo(Coordinate, ClientHeight)
      end;
      for i := 1 to GridHeight - 1 do
      begin
        Coordinate := CellEdge(i, ClientHeight, GridHeight);
        Canvas.MoveTo(0, Coordinate);
        Canvas.LineTo(ClientWidth, Coordinate)
      end
    end
  end;

begin
  with Canvas do begin
    Brush.Style := bsSolid;
    if ShowGridLines then
      DrawGridLines;
    { Draw all the live cells }
{$IFDEF VER90}
    Pen.Mode := pmCopy;
{$ENDIF}
    Pen.Style := psClear;
    Brush.Color := CellColor;
    for y := 0 to GridHeight - 1 do
      for x := 0 to GridWidth - 1 do
        if (Cells[x, y]) then begin
          r := CellCoords(x, y);
          with r do
            Ellipse(Left + 2, Top + 1, Right + 1, Bottom)
        end;
    { At design-time, draw a dashed line around the component }
    if (csDesigning in ComponentState) then begin
      Brush.Color := Color;
      Brush.Style := bsCross;
      FrameRect(Rect(0, 0, Width, Height))
    end
  end
end;

{ Reset the current Generation to zero }
procedure TLife.ResetGeneration;
begin
  FGeneration := 0;
  Change
end;

procedure TLife.SetAcceptMouseClicks(const Value: Boolean);
begin
  if (Value <> FAcceptMouseClicks) then begin
    FAcceptMouseClicks := Value;
    Change
  end
end;

{ Set the value of a single cell. The area of the component relating to the cell is then
  invalidated so that the visible image correctly reflects the state of the cell. }
procedure TLife.SetCell(const X: TGridWidthRange; const Y: TGridHeightRange;
                        const Value: Boolean);
begin
  if (Cells[X, Y] <> Value) then begin
    FGrid.Cells[X, Y] := Value;
    InvalidateCell(X, Y)
  end
end;

procedure TLife.SetCellColor(Const Value: TColor);
begin
  if (Value <> FCellColor) then begin
    FCellColor := Value;
    Invalidate
  end
end;

{ Set the value of a single cell by calling SetCell. The Generation is then reset to 0, the
  history cleared and the Change method called. This is the method called by the Cells property. }
procedure TLife.SetCells(const X: TGridWidthRange; const Y: TGridHeightRange;
                         const Value: Boolean);
begin
  if (Cells[X, Y] <> Value) then begin
    SetCell(X, Y, Value);
    ResetGeneration;
    History.Clear
  end
end;

procedure TLife.SetGridHeight(const NewGridHeight: TPermissableGridHeight);
begin
  SetGridSize(GridWidth, NewGridHeight)
end;

procedure TLife.SetGridLineColor(Const Value: TColor);
begin
  if (Value <> FGridLineColor) then begin
    FGridLineColor := Value;
    Invalidate
  end
end;

procedure TLife.SetGridLineStyle(Const Value: TPenStyle);
begin
  if (Value <> FGridLineStyle) then begin
    FGridLineStyle := Value;
    Invalidate
  end
end;

procedure TLife.SetGridSize(const NewGridWidth: TGridWidthRange;
                            const NewGridHeight: TGridHeightRange);
begin
  if (NewGridWidth <> GridWidth) or (NewGridHeight <> GridHeight) then
  begin
    FHistory.Clear;
    FGrid.SetGridSize(NewGridWidth, NewGridHeight);
    Invalidate;
    Change
  end
end;

procedure TLife.SetGridWidth(const NewGridWidth: TPermissableGridWidth);
begin
  SetGridSize(NewGridWidth, GridHeight)
end;

procedure TLife.SetMaxNumberOfHistoryLevels(const Value: Cardinal);
begin
  if Value < 1 then
    raise ERangeError.Create('MaxNumberOfHistoryLevels must be greater than 0');
  if Value > AbsoluteMaxNumberOfHistoryLevels then
    raise ERangeError.CreateFmt('MaxNumberOfHistoryLevels must be greater than %s',
                                [AbsoluteMaxNumberOfHistoryLevels]);
  FHistory.MaxNumberOfLevels := Value
end;

procedure TLife.SetShowGridLines(const Value: Boolean);
begin
  if Value <> FShowGridLines then
  begin
    FShowGridLines := Value;
    Invalidate
  end
end;


{ Allocate memory for FGrid }
procedure TLifeGrid.AllocGrid;
begin
  FGrid := AllocMem(FGridWidth * FGridHeight)
end;

{ Assign the contents of another grid to this grid }
procedure TLifeGrid.Assign(const Source: TLifeGrid);
begin
  SetGridSize(Source.FGridWidth, Source.FGridHeight);
  Move(Source.FGrid^, FGrid^, FGridWidth * FGridHeight)
end;

{ Returns the offset in FGrid (which is only 1-dimensional) of the specified cell
  (which is on a 2-dimensional plane) }
function TLifeGrid.CellCoordToGridOffset(const X: TGridWidthRange;
                                         const Y: TGridHeightRange): Cardinal;
begin
{$IFOPT R+}
  if X >= FGridWidth then
    raise ERangeError.Create('X parameter out of range');
  if Y >= FGridHeight then
    raise ERangeError.Create('Y parameter out of range');
{$ENDIF}
  Result := Y * FGridWidth + X
end;

{ Set the state of all the cells in FGrid to False (dead) }
procedure TLifeGrid.Clear;
begin
  FillChar(FGrid^, FGridWidth * FGridHeight, False)
end;

constructor TLifeGrid.Create(const GridWidth: TPermissableGridWidth;
                             const GridHeight: TPermissableGridHeight);
begin
  CreateEmpty;
  FGridHeight := GridHeight;
  FGridWidth := GridWidth;
  AllocGrid
end;

constructor TLifeGrid.CreateEmpty;
begin
  inherited Create;
end;

destructor TLifeGrid.Destroy;
begin
  FreeGrid;
  inherited Destroy
end;

{ Applies the Rules of Life to determine if the specified cell should live or die }
function TLifeGrid.DoesCellLive(const X: TGridWidthRange;
                                const Y: TGridHeightRange): Boolean;
begin
{$IFOPT R+}
  if X >= FGridWidth then
    raise ERangeError.Create('X parameter out of range');
  if Y >= FGridHeight then
    raise ERangeError.Create('Y parameter out of range');
{$ENDIF}
  case NumberOfNeighbours(X, Y) of
    2: Result := Cells[X, Y];
    3: Result := True;
    else   Result := False
  end
end;

{ Test to see if this grid is equal to Source. }
function TLifeGrid.Equal(const Source: TLifeGrid): Boolean;
begin
  if (Source.FGridWidth <> FGridWidth) or (Source.FGridHeight <> FGridHeight) then
    Result := False
  else
    Result := CompareMem(FGrid, Source.FGrid, FGridWidth * FGridHeight)
end;

{ Free the memory used by FGrid }
procedure TLifeGrid.FreeGrid;
begin
{$IFDEF VER80}
  FreeMem(FGrid, FGridWidth * FGridHeight)
{$ELSE}
  FreeMem(FGrid)
{$ENDIF}
end;

{ Get the state of the specified cell }
function TLifeGrid.GetCell(const X: TGridWidthRange; const Y: TGridHeightRange): Boolean;
begin
  Result := FGrid^[CellCoordToGridOffset(X, Y)]
end;

{ Counts the number of "live" cells in FGrid }
function TLifeGrid.GetLiveCellCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FGridWidth * FGridHeight - 1 do
    Inc(Result, Ord(FGrid^[i]))
end;

{ Count the number of live neighbours that the specified cell has. Cells on the edge of the
  grid are considered to have the cell on the opposite edge of the grid as a neighbour }
function TLifeGrid.NumberOfNeighbours(const X: TGridWidthRange;
                                      const Y: TGridHeightRange): Integer;
var
  xMinus1, xPlus1, yMinus1, yPlus1: Integer;
begin
{$IFOPT R+}
  if X >= GridWidth then
    raise ERangeError.Create('X parameter out of range');
  if Y >= GridHeight then
    raise ERangeError.Create('Y parameter out of range');
{$ENDIF}
  { Pre-calculate some values for the sake of efficiency }
  xMinus1 := (X + GridWidth - 1) mod GridWidth; { Equivalent to (X - 1) with a "wrap" }
  xPlus1 := (X + 1) mod GridWidth;
  yMinus1 := (Y + GridHeight - 1) mod GridHeight; { Equivalent to (Y - 1) with a "wrap" }
  yPlus1 := (Y + 1) mod GridHeight;
  { Count the number of live neighbours that this cell has }
  Result := 0;
  if Cells[xMinus1, yMinus1] then
    Inc(Result);
  if Cells[X, yMinus1] then
    Inc(Result);
  if Cells[xPlus1, yMinus1] then
    Inc(Result);
  if Cells[xMinus1, Y] then
    Inc(Result);
  if Cells[xPlus1, Y] then
    Inc(Result);
  if Cells[xMinus1, yPlus1] then
    Inc(Result);
  if Cells[X, yPlus1] then
    Inc(Result);
  if Cells[xPlus1, yPlus1] then
    Inc(Result)
end;

{ Set the state of the specified cell }
procedure TLifeGrid.SetCell(const X: TGridWidthRange; const Y: TGridHeightRange;
                            const Value: Boolean);
begin
  FGrid^[CellCoordToGridOffset(X, Y)] := Value
end;

{ Resize FGrid to the specified dimensions. This will have the side-effect of "clearing"
  FGrid }
procedure TLifeGrid.SetGridSize(const NewGridWidth: TPermissableGridWidth;
                                const NewGridHeight: TPermissableGridHeight);
begin
  FreeGrid;
  FGridWidth := NewGridWidth;
  FGridHeight := NewGridHeight;
  AllocGrid
end;



{ Add a new grid to the circular buffer }
function TLifeHistory.Add(const Grid: TLifeGrid): TLifeGrid;
begin
  with FList do
    if Count < Capacity then
      FMostRecent := Add(TLifeGrid.CreateEmpty)
    else
      FMostRecent := (FMostRecent + 1) mod Count;
  Result := FList[FMostRecent];
  Result.Assign(Grid)
end;

{ Remove all the grids from the buffer }
procedure TLifeHistory.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TLifeGrid(FList[i]).Free;
  FList.Count := 0
end;

{ Searches the circular buffer for a grid matching the contents of the specified grid. The
  search i performed starting with the most recently added grid and proceeding "backwards".
  Returns the index of the matching grid (0 <= Result <= Count - 1) on success and -1 on no
  match found }
function TLifeHistory.Contains(const Grid: TLifeGrid): Integer;
begin
  Result := 0;
  while (Result < Count) and not Grid.Equal(Grids[Result]) do
    Inc(Result);
  if Result >= Count then
    Result := -1
end;

constructor TLifeHistory.Create(const NumberOfLevels: Cardinal);
begin
  inherited Create;
  FList := TList.Create;
  FList.Capacity := NumberOfLevels
end;

destructor TLifeHistory.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TLifeHistory.GetCount: Cardinal;
begin
  Result := FList.Count
end;

{ Return a grid based on its "distance" from the most recently added grid. So, 0 returns the
  most recently added grid; 1 returns the grid added immediately before the most recently added
  grid; and so forth }
function TLifeHistory.GetGrid(const Index: Cardinal): TLifeGrid;
begin
  Result := FList[(FMostRecent + Count - Index) mod Count]
end;

function TLifeHistory.GetMaxNumberOfLevels: Cardinal;
begin
  Result := FList.Capacity
end;

procedure TLifeHistory.SetMaxNumberOfLevels(const Value: Cardinal);
begin
  Clear;
  FList.Capacity := Value
end;


procedure Register;
begin
  RegisterComponents('Extras', [TLife])
end;

end.
