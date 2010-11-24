{
  Author: Grega Loboda
  E-mail: grega.loboda@email.si
  Web: http://delphistep.cis.si

  Copyright (c) 2000 Grega Loboda

  ver 1.1
  -------------------------------------------------------------------
  TdsCalendar
  TdsMonths
  TdsPlanner
}
unit dsCalendar;

interface

uses
  Windows, Forms, Controls, Classes, Graphics, Buttons, ComCtrls, Menus,
  Messages, SysUtils, DsgnIntf, Extctrls, Stdctrls, Dialogs;

type
  TdsCalendar = class;

  TDay = 1..31;
  TMonth = 1..12;

  TButtonDirection = (bdMonthUp, bdMonthDown);
  TSelectedShape = (ssEllipse, ssRectangle);
  TSelectType = (stDateSelect, stBlockSelect);
  TDayOfWeek = (dwMonday, dwTuesday, dwWednesday, dwThursday, dwFriday, dwSaturday, dwSunday);

  TSelectedColor = (scNone, scDate, scSelected);

  TOnDateChange = procedure(Sender: TObject; FromDate, ToDate: TDateTime) of object;

  THoliday = class(TCollectionItem)
  private
    FDay: TDay;
    FMonth: TMonth;
    FName: String;
  protected
    function GetDisplayName: String; override;
  published
    property Day: TDay read FDay write FDay;
    property Month: TMonth read FMonth write FMonth;
    property Name: String read FName write FName;
  end;

  THolidays = class(TCollection)
  private
    FCalendar: TdsCalendar;

    function GetHoliday(index: Integer): THoliday;
    procedure SetHoliday(index: Integer; Value: THoliday);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TdsCalendar);
    function Add(ADay, AMonth: Word): THoliday;
    property Items[index: integer]: THoliday read GetHoliday write SetHoliday;
  end;

  TdsMonthsColors = class(TPersistent)
  private
    FControl: TControl;

    FBkColor: TColor;           //background
    FTitleColor: TColor;        //title, days names, lines, cursor
    FCircleColor: TColor;       //today's circle

    procedure SetBkColor(Value: TColor);
    procedure SetTitleColor(Value: TColor);
    procedure SetCircleColor(Value: TColor);
  public
    constructor Create(AControl: TControl);
  published
    property BkColor: TColor read FBkColor write SetBkColor;
    property TitleColor: TColor read FTitleColor write SetTitleColor;
    property CircleColor: TColor read FCircleColor write SetCircleColor;
  end;

  TdsCalendarColors = class(TdsMonthsColors)
  private
    FMarkColor: TColor;         //sundays, holidays
    FSelectedColor: TColor;     //selected dates
    FTrailingDasyColor: TColor; //trailing days

    procedure SetMarkColor(Value: TColor);
    procedure SetSelectedColor(Value: TColor);
    procedure SetTrailingDaysColor(Value: TColor);
  public
    constructor Create(AControl: TControl);
  published
    property MarkColor: TColor read FMarkColor write SetMarkColor;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property TrailingDasyColor: TColor read FTrailingDasyColor write SetTrailingDaysColor;
  end;

  TdsCustomCalendar = class(TCustomControl)
  private
    FYear: Word;
    FMonth: Word;
    FDay: Word;

    FCellHeight: Integer;

    FUpButton: TSpeedButton;   //up button
    FDownButton: TSpeedButton; //down button

    FTimer: TTimer;                    //timer for up/down button
    FTimerDirection: TButtonDirection; //which button is holded down

    FTextFont: TFont;  //font for days
    FTitleFont: TFont; //font for title (month and year)

    FSelectedShape: TSelectedShape; //shape for selection

    FTransparentCursor: Boolean;

    FMinDate: TDate; //min date, that can be selected
    FMaxDate: TDate; //max date, that can be selected

    FBorderStyle: TBorderStyle; //border

    //FShowFocusRect: Boolean; //draw focus rect when calendar has focus

    procedure SetTextFont(Value: TFont);
    procedure SetTitleFont(Value: TFont);
    procedure SetBorder(Value: TBorderStyle);
    procedure SetSelectedShape(Value: TSelectedShape);
    procedure SetTransparentCursor(Value: Boolean);

    procedure FontChanged(Sender: TObject);
    procedure BtnPress(Sender: TObject);
    procedure BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnTimerEvent(Sender: TObject);
    procedure PositionButtons;
    procedure SetButtonsParent(ToSelf: Boolean);
    procedure RecalculateSize;

    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;

  protected
    procedure BtnUp; virtual; abstract;
    procedure BtnDown; virtual; abstract;

    procedure DrawBorder; virtual;
    //procedure DrawFocusRect; virtual;

    function CanChangeDate(ToDate: TDate): Boolean; virtual;

    procedure Paint; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property SelectedShape: TSelectedShape read FSelectedShape write SetSelectedShape default ssEllipse;
    property TextFont: TFont read FTextFont write SetTextFont;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property MinDate: TDate read FMinDate write FMinDate;
    property MaxDate: TDate read FMaxDate write FMaxDate;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorder default bsNone;
    //property ShowFocusRect: Boolean read FShowFocusRect write FShowFocusRect default true;
    property TransparentCursor: Boolean read FTransparentCursor write SetTransparentCursor;

    property Visible;

    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
  end;

  TdsCalendar = class(TdsCustomCalendar)
  private
    FUpDown: TUpDown;          //year up and down spin button
    FPopUpMonth: TPopupMenu;   //month popup menu

    FColors: TdsCalendarColors;
    FHolidays: THolidays;

    FShowTodayCircle: Boolean;
    FShowYear: Boolean;
    FShowTitle: Boolean;
    FShowWeekNumbers: Boolean;
    FShowToday: Boolean;

    FCellWidth: Integer;

    FNumCols: Byte;
    FNumRows: Byte;

    FFirstDayOfWeekByte: Byte;
    FFirstDayOfWeek: TDayOfWeek;

    FDayOffset: Word; //number of days from the previous month visible in this month

    FTodayString: String;

    FMouseDown: Boolean;
    FStartDate, FEndDate: TDate;

    FSelectType: TSelectType;

    FCanChangeMonthYear: Boolean;

    FOnDateSelect: TNotifyEvent;  //occurs when date is selected with a mouse
    FOnBlockSelect: TNotifyEvent; //occurs when block select is finished with a mouse
    FOnDateChange: TOnDateChange; //occurs when date changes

    procedure UpDownPress(Sender: TObject; Button: TUDBtnType);
    procedure PopupMonthClick(Sender: TObject);
    procedure JumpToToday;
    procedure SetMinMaxForMultiSelect(var min, max: TDate);
    procedure ResetStartEndDate;
    procedure SetNumRows;
    procedure SetNumCols;
    procedure GetPrevMonth(var AYear, AMonth: Word);
    procedure GetNextMonth(var AYear, AMonth: Word);

    function RectOfToday: TRect;
    function RectOfDay(ADay: Word): TRect;
    function RectOfMonth: TRect;
    function RectOfYear: TRect;
    function RectOfCell(row, col: Word): TRect;
    function RectOfDays: TRect;

    function IsDateHoliday(ADay, AMonth: Word): Boolean;
    function GetDate: TDate;
    function GetDayRow(y: Integer): Integer;
    function GetDayCol(x: Integer): Integer;
    function GetFirstDayCell: Integer;
    function GetStartDate: TDate;
    function GetEndDate: TDate;

    procedure SetCellWidth(Value: Integer);
    procedure SetCellHeight(Value: Integer);
    procedure SetShowWeekNumbers(Value: Boolean);
    procedure SetShowToday(Value: Boolean);
    procedure SetShowTodayCircle(Value: Boolean);
    procedure SetShowYear(Value: Boolean);
    procedure SetShowTitle(Value: Boolean);
    procedure SetHolidays(Value: THolidays);
    procedure SetFirstDayOfWeek(Value: TDayOfWeek);
    procedure SetDate(Value: TDate);
    procedure SetColors(Value: TdsCalendarColors);
    procedure SetSelectType(Value: TSelectType);
    procedure SetCanChangeMonthYear(Value: Boolean);
    procedure SetYear(Value: Word);
    procedure SetMonth(Value: Word);
    procedure SetDay(Value: Word);

    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  protected
    function DoDateChange(FromDate, ToDate: TDateTime; MoveMouse, ResetStartEnd: Boolean): Boolean; virtual;
    function AllowMultiSelect: Boolean; virtual;

    function CanChangeDate(ToDate: TDate): Boolean; override;

    procedure BtnUp; override;
    procedure BtnDown; override;

    procedure DoOnSelectDate; virtual;
    procedure DoOnBlockSelect; virtual;

    procedure SetNormalCanvas(AYear, AMonth, ADay: Word); virtual;
    procedure SetSelectedCanvas(AYear, AMonth, ADay: Word; Grayed: Boolean); virtual;

    procedure DrawMonth;
    procedure DrawDayNames;
    procedure DrawDays;
    procedure DrawMultiSelect(selected: Boolean);
    procedure DrawWeekNumbers;
    procedure DrawToday;
    procedure DrawTodayCircle;
    procedure DrawDatePosition;

    procedure DrawDayText(R: TRect; const s: String);
    procedure DrawNormalDay(R: TRect; AYear, AMonth, ADay: Word);
    procedure DrawSelectedDay(R: TRect; AYear, AMonth, ADay: Word);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function DayNumber: Word; //which day number is selected day
    function WeekNumber(ADate: TDate): Integer; //which week number is selected day
    function IsHoliday(ADate: TDate): Boolean;

    property Year: Word read FYear write SetYear;
    property Month: Word read FMonth write SetMonth;
    property Day: Word read FDay write SetDay;

    property StartDate: TDate read GetStartDate;
    property EndDate: TDate read GetEndDate;
  published
    property Colors: TdsCalendarColors read FColors write SetColors;

    property Date: TDate read GetDate write SetDate;
    property FirstDayOfWeek: TDayOfWeek read FFirstDayOfWeek write SetFirstDayOfWeek default dwMonday;

    property ShowToday: Boolean read FShowToday write SetShowToday default true;
    property ShowTodayCircle: Boolean read FShowTodayCircle write SetShowTodayCircle default true;
    property ShowWeekNumbers: Boolean read FShowWeekNumbers write SetShowWeekNumbers default false;
    property ShowYear: Boolean read FShowYear write SetShowYear default true;
    property ShowTitle: Boolean read FShowTitle write SetShowTitle default true;
    property TodayString: String read FTodayString write FTodayString;

    property Holidays: THolidays read FHolidays write SetHolidays;

    property CellWidth: Integer read FCellWidth write SetCellWidth default 28;
    property CellHeight: Integer read FCellHeight write SetCellHeight default 22;

    property SelectType: TSelectType read FSelectType write SetSelectType default stDateSelect;

    property CanChangeMonthYear: Boolean read FCanChangeMonthYear write SetCanChangeMonthYear default true;

    property OnDateSelect: TNotifyEvent read FOnDateSelect write FOnDateSelect;
    property OnBlockSelect: TNotifyEvent read FOnBlockSelect write FOnBlockSelect;
    property OnDateChange: TOnDateChange read FOnDateChange write FOnDateChange;
  end;

  TdsMonths = class(TdsCustomCalendar)
  private
    FColors: TdsMonthsColors;

    FDay: Word; //always 1
    FMonth: Word;
    FYear: Word;

    FNumRows: Integer;

    FShowThisMonth: Boolean; //show this month in red circle

    FCanChangeYear: Boolean;
    FShowTitle: Boolean;

    FOnSelectMonth: TNotifyEvent;  //when month is selected with a mouse
    FOnDateChange: TOnDateChange;  //when month changes

    function GetDate: TDate;

    procedure SetDate(Value: TDate);
    procedure SetShowThisMonth(Value: Boolean);
    procedure SetColors(Value: TdsMonthsColors);
    procedure SetCanChangeYear(Value: Boolean);
    procedure SetCellHeight(Value: Integer);
    procedure SetShowTitle(Value: Boolean);
    procedure SetYear(Value: Word);
    procedure SetMonth(Value: Word);

    function RectOfMonths: TRect;
    function RectOfMonth(AMonth: Word): TRect;

    procedure DrawYear;
    procedure DrawMonths;
    procedure DrawThisMonthCircle;
    procedure DrawMonth(R: TRect; AMonth: Word; Selected: Boolean);
    procedure JumpToThisMonth;
    procedure SetNumRows;

    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  protected
    procedure BtnUp; override;
    procedure BtnDown; override;

    procedure DoDateChange(FromDate, ToDate: TDateTime); virtual;
    procedure DoOnSelectMonth; virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Year: Word read FYear write SetYear;
    property Month: Word read FMonth write SetMonth;
  published
    property Colors: TdsMonthsColors read FColors write SetColors;
    property CanChangeYear: Boolean read FCanChangeYear write SetCanChangeYear default true;
    property CellHeight: Integer read FCellHeight write SetCellHeight default 22;
    property ShowTitle: Boolean read FShowTitle write SetShowTitle default true;

    property Date: TDate read GetDate write SetDate; //selected date
    property ShowThisMonth: Boolean read FShowThisMonth write SetShowThisMonth default true;

    property OnSelectMonth: TNotifyEvent read FOnSelectMonth write FOnSelectMonth;
  end;

  TdsPlanner = class;

  TdsPlanerCategorie = class(TCollectionItem)
  private
    FName: String;
    FDates: TStrings;
    FColor: TColor;
    FVisible: Boolean;

    procedure SetDates(Value: TStrings);
    procedure SetColor(Value: TColor);
    procedure SetVisible(Value: Boolean);

    procedure DatesChange(Sender: TObject);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Name: String read FName write FName;
    property Dates: TStrings read FDates write SetDates;
    property Color: TColor read FColor write SetColor;
    property Visible: Boolean read FVisible write SetVisible default true;
  end;

  TdsPlanerCategories = class(TCollection)
  private
    FCalendar: TdsPlanner;

    function GetField(index: Integer): TdsPlanerCategorie;
    procedure SetField(index: Integer; Value: TdsPlanerCategorie);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(ACalendar: TdsPlanner);
    function Add: TdsPlanerCategorie;
    property Items[index: integer]: TdsPlanerCategorie read GetField write SetField; default;
  end;

  TdsPlanner = class(TdsCalendar)
  private
    FCategories: TdsPlanerCategories;
    FActiveCategorie: Integer;
    FOverlapDates: Boolean;
    FShift: Boolean;

    procedure SetCategories(Value: TdsPlanerCategories);
  protected
    procedure SetNormalCanvas(AYear, AMonth, ADay: Word); override;
    procedure DoOnBlockSelect; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    function AllowMultiSelect: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SelectDeselectDate(d: TDate);
    procedure RemoveDateFromCategories(d: TDate);

    function GetDateCategorie(d: TDate): TdsPlanerCategorie;
  published
    property Categories: TdsPlanerCategories read FCategories write SetCategories;
    property ActiveCategorie: Integer read FActiveCategorie write FActiveCategorie;
    property OverlapDates: Boolean read FOverlapDates write FOverlapDates;
  end;

function DaysInMonth(AYear, AMonth: Word): Word;
function DayOfWeek(Date: TDateTime): TDayOfWeek;
function LocaleFirstDayOfWeek: TDayOfWeek;

procedure Register;

implementation

{$RESOURCE dsCalendar.res}

function ByteToDayOfWeek(Value: Byte): TDayOfWeek;
begin
  case Value of
    1: Result := dwSunday;
    2: Result := dwMonday;
    3: Result := dwTuesday;
    4: Result := dwWednesday;
    5: Result := dwThursday;
    6: Result := dwFriday;
    7: Result := dwSaturday;
    else raise Exception.Create('');
  end;
end;

function DaysInMonth(AYear, AMonth: Word): Word;
const
  c_DaysInMonth: array[1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  //how many days are in AMonth depends on AYear, as it could be leap year
  Result := c_DaysInMonth[AMonth];
  if (AMonth = 2) and IsLeapYear(AYear) then Inc(Result); //leap-year Feb is special
end;

function DayOfWeek(Date: TDateTime): TDayOfWeek;
begin
  Result := ByteToDayOfWeek(SysUtils.DayOfWeek(Date));
end;

function LocaleFirstDayOfWeek: TDayOfWeek;
var
  fdw: Integer;
  c: array[0..1] of Char;
begin
  GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTDAYOFWEEK, c, SizeOf(c));
  fdw := Ord(c[0]) - Ord('0'); //0 for Monday...6 for Sunday
  Result := ByteToDayOfWeek((fdw + 2) mod 7);   //1 for Sunday...7 for Saturday
end;

procedure DrawSelectedShape(ACanvas: TCanvas; AShape: TSelectedShape; x1, y1, x2, y2: Integer);
  {
  procedure DrawArtisticShape(Rect: TRect);
  var
    Height, Width : Integer;
  begin
    Height := (Rect.Bottom - Rect.Top);
    Width := Rect.Right - Rect.Left;

    ACanvas.Arc(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom,
       Rect.Left, Rect.Top + Height div 2,
       Rect.Left+Width div 2, Rect.Top);
    ACanvas.Arc(Rect.Left, Rect.Top+(Height div 16), Rect.Right, Rect.Bottom-(Height div 16),
       Rect.Left+Width div 2, Rect.Top,
       Rect.Left, Rect.Top + Height div 2);
    ACanvas.Arc(Rect.Left-(Width div 2), Rect.Top, Rect.Right+Width div 2, Rect.Bottom-(Height div 4),
       Rect.Left+Width div 2, Rect.Top,
       Rect.Left+(Width div 8), Rect.Top + Height div 10);
    ACanvas.Brush.Style := bsSolid;
  end;
  }
begin
  case AShape of
    ssRectangle: ACanvas.Rectangle(x1, y1, x2, y2);
    ssEllipse: ACanvas.Ellipse(x1, y1, x2, y2);
  end;
end;

//**********************************************************************************
// TdsMonthsColors and TdsCalendarColors
//**********************************************************************************
constructor TdsMonthsColors.Create(AControl: TControl);
begin
  inherited Create;

  FControl := AControl;

  FBkColor := clInfoBk;
  FTitleColor := clNavy;
  FCircleColor := clRed;
end;

procedure TdsMonthsColors.SetBkColor(Value: TColor);
begin
  if Value <> FBkColor then
  begin
    FBkColor := Value;
    FControl.Refresh;
  end;
end;

procedure TdsMonthsColors.SetTitleColor(Value: TColor);
begin
  if Value <> FTitleColor then
  begin
    FTitleColor := Value;
    FControl.Refresh;
  end;
end;

procedure TdsMonthsColors.SetCircleColor(Value: TColor);
begin
  if Value <> FCircleColor then
  begin
    FCircleColor := Value;
    FControl.Refresh;
  end;
end;

constructor TdsCalendarColors.Create(AControl: TControl);
begin
  inherited;
  FMarkColor := clRed;
  FSelectedColor := clGreen;
  FTrailingDasyColor := clSilver;
end;

procedure TdsCalendarColors.SetMarkColor(Value: TColor);
begin
  if Value <> FMarkColor then
  begin
    FMarkColor := Value;
    FControl.Refresh;
  end;
end;

procedure TdsCalendarColors.SetSelectedColor(Value: TColor);
begin
  if Value <> FSelectedColor then
  begin
    FSelectedColor := Value;
    FControl.Refresh;
  end;
end;

procedure TdsCalendarColors.SetTrailingDaysColor(Value: TColor);
begin
  if Value <> FTrailingDasyColor then
  begin
    FTrailingDasyColor := Value;
    FControl.Refresh;
  end;
end;

//**********************************************************************************
// THolidays
//**********************************************************************************
function THoliday.GetDisplayName: String;
begin
  Result := FName;
  if Result = '' then
    Result := inherited GetDisplayName;
end;


constructor THolidays.Create(AOwner: TdsCalendar);
begin
  inherited Create(THoliday);
  FCalendar := AOwner;
end;

function THolidays.GetOwner: TPersistent;
begin
  Result := FCalendar;
end;

function THolidays.Add(ADay, AMonth: Word): THoliday;
begin
  Result := THoliday(inherited Add);

  Result.Day := ADay;
  Result.Month := AMonth;
end;

function THolidays.GetHoliday(index: Integer): THoliday;
begin
  Result := THoliday(inherited Items[index]);
end;

procedure THolidays.SetHoliday(index: Integer; Value: THoliday);
begin
  Items[index].Assign(Value);
end;

//**********************************************************************************
// TdsCustomCalendar
//**********************************************************************************
constructor TdsCustomCalendar.Create(AOwner: TComponent);
begin
  inherited;

  //control takes whole canvas of the control
  ControlStyle := ControlStyle + [csOpaque];

  //create font for days and title
  FTextFont := TFont.Create;
  FTextFont.OnChange := FontChanged;

  FTitleFont := TFont.Create;
  FTitleFont.Color := clWhite;
  FTitleFont.Style := [fsBold];
  FTitleFont.OnChange := FontChanged;

  FSelectedShape := ssEllipse;
  //FShowFocusRect := true;

  FUpButton := TSpeedButton.Create(Self);
  with FUpButton do
  begin
    Parent := Self;
    Glyph.LoadFromResourceName(hInstance, 'NEXTMONTHBUTTON');
    OnClick := BtnPress;
    OnMouseDown := BtnMouseDown;
    OnMouseUp := BtnMouseUp;
  end;

  FDownButton := TSpeedButton.Create(Self);
  with FDownButton do
  begin
    Parent := Self;
    Glyph.LoadFromResourceName(hInstance, 'PREVMONTHBUTTON');
    OnClick := BtnPress;
    OnMouseDown := BtnMouseDown;
    OnMouseUp := BtnMouseUp;
  end;

  FTimer := TTimer.Create(Self);
  with FTimer do
  begin
    Enabled := false;
    Interval := 600;
    OnTimer := OnTimerEvent;
  end;

  TabStop := true;
end;

destructor TdsCustomCalendar.Destroy;
begin
  FTextFont.Free;
  FTitleFont.Free;
  inherited;
end;

procedure TdsCustomCalendar.WMGetDlgCode;
begin
  //accept cursor keys
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

procedure TdsCustomCalendar.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  //Repaint;
end;

procedure TdsCustomCalendar.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
  //Repaint;
end;

procedure TdsCustomCalendar.SetTextFont(Value: TFont);
begin
  FTextFont.Assign(Value);
end;

procedure TdsCustomCalendar.SetTitleFont(Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

procedure TdsCustomCalendar.SetBorder(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TdsCustomCalendar.SetSelectedShape(Value: TSelectedShape);
begin
  if Value <> FSelectedShape then
  begin
    FSelectedShape := Value;
    Invalidate;
  end;
end;

procedure TdsCustomCalendar.SetTransparentCursor(Value: Boolean);
begin
  if Value <> FTransparentCursor then
  begin
    FTransparentCursor := Value;
    Invalidate;
  end;
end;

procedure TdsCustomCalendar.FontChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TdsCustomCalendar.BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Sender = FUpButton then
    FTimerDirection := bdMonthUp
  else
    FTimerDirection := bdMonthDown;

  FTimer.Interval := 500; //before timer starts, interval is longer
  FTimer.Enabled := true;
end;

procedure TdsCustomCalendar.BtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FTimer.Enabled := false;
end;

procedure TdsCustomCalendar.OnTimerEvent(Sender: TObject);
begin
  case FTimerDirection of
    bdMonthUp:
      if PtInRect(FUpButton.BoundsRect, ScreenToClient(Mouse.CursorPos)) then
      begin
        FTimer.Interval := 200; //when timer starts, shorten interval
        BtnUp;
      end;

    bdMonthDown:
      if PtInRect(FDownButton.BoundsRect, ScreenToClient(Mouse.CursorPos)) then
      begin
        FTimer.Interval := 200; //when timer starts, shorten interval
        BtnDown;
      end;
  end;
end;

procedure TdsCustomCalendar.PositionButtons;
begin
  FUpButton.SetBounds((Width - FUpButton.Width - 5), (2 * FCellHeight div 2 - FUpButton.Height div 2), 20, 20);
  FDownButton.SetBounds((FUpButton.Left - FDownButton.Width - 5), FUpButton.Top, 20, 20);
end;

procedure TdsCustomCalendar.SetButtonsParent(ToSelf: Boolean);
begin
  if (csDesigning in ComponentState) then
    if ToSelf then
    begin
      FUpButton.Parent := Self;
      FDownButton.Parent := Self;
    end
    else
    begin
      FUpButton.Parent := nil;
      FDownButton.Parent := nil;
    end;
end;

procedure TdsCustomCalendar.RecalculateSize;
begin
  //this will cause WMSize to be called
  //that means width and height will be recalculated
  Width := Width + 1;
end;

procedure TdsCustomCalendar.BtnPress(Sender: TObject);
begin
  if not Focused then SetFocus;

  if Sender = FUpButton then
    BtnUp
  else
    BtnDown;
end;

procedure TdsCustomCalendar.DrawBorder;
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 1;
  Canvas.Rectangle(0, 0, Width, Height);
end;
{
procedure TdsCustomCalendar.DrawFocusRect;
var
  R: TRect;
begin
  R := Rect(1, 1, ClientWidth - 1, ClientHeight - 1);
  Canvas.Pen.Color := clWindowFrame;
  Canvas.Brush.Color := clBtnFace;
  Canvas.DrawFocusRect(R);
end;
}
function TdsCustomCalendar.CanChangeDate(ToDate: TDate): Boolean;
begin
  if FMinDate <> 0 then
    Result := ToDate >= FMinDate
  else
    Result := true;

  if FMaxDate <> 0 then
    Result := Result and (ToDate <= FMaxDate);
end;

procedure TdsCustomCalendar.Paint;
begin
  if BorderStyle = bsSingle then DrawBorder;
  //if Focused and ShowFocusRect then DrawFocusRect;
end;

//**********************************************************************************
// TdsCalendar
//**********************************************************************************
constructor TdsCalendar.Create(AOwner: TComponent);
var
  i: Integer;
  mi: TMenuItem;
begin
  inherited;

  FCellWidth := 28;
  FCellHeight := 22;

  FTodayString := 'Today: ';

  FNumCols := 7;
  FNumRows := 10;

  FShowTodayCircle := true;
  FShowYear := true;
  FShowTitle := true;
  FShowToday := true;
  FCanChangeMonthYear := true;

  FSelectedShape := ssEllipse;

  DecodeDate(SysUtils.Date, FYear, FMonth, FDay); //by default, date is today

  FFirstDayOfWeekByte := 2;
  FirstDayOfWeek := LocaleFirstDayOfWeek; //get locale first day of the week

  FColors := TdsCalendarColors.Create(Self);
  FHolidays := THolidays.Create(Self);

  FUpDown := TUpDown.Create(Self);
  with FUpDown do
  begin
    //we don't want to show UpDown at design time
    if not (csDesigning in ComponentState) then Parent := Self;
    Visible := false;
    Min := 1800;
    Max := 9999;
    Position := FYear;
    OnClick := UpDownPress;
  end;

  FPopUpMonth := TPopupMenu.Create(Self);
  for i := 1 to 12 do
  begin
    mi := TMenuItem.Create(FPopupMonth);
    mi.Caption := LongMonthNames[i];
    mi.Tag := i;
    mi.OnClick := PopupMonthClick;
    FPopUpMonth.Items.Add(mi);
  end;

  RecalculateSize;
  PositionButtons;
end;

destructor TdsCalendar.Destroy;
begin
  FColors.Free;
  FHolidays.Free;
  inherited;
end;

procedure TdsCalendar.KeyDown(var Key: Word; Shift: TShiftState);

  procedure SetStartEndDate;
  begin
    if (FStartDate = 0) then
    begin
      FStartDate := Date;
      FEndDate := FStartDate;
    end;
  end;

begin
  inherited;

  FUpDown.Visible := false;

  case Key of
    VK_NEXT:  //PageUp - month up, if Ctrl is pressed, year up
      if CanChangeMonthYear then
        if ssCtrl in Shift then
          DoDateChange(Date, EncodeDate(FYear - 1, FMonth, FDay), false, true)
        else
          DoDateChange(Date, IncMonth(Date, -1), false, true);

    VK_PRIOR: //PageDown - month down, if Ctrl is pressed, year down
      if CanChangeMonthYear then
        if ssCtrl in Shift then
          DoDateChange(Date, EncodeDate(FYear + 1, FMonth, FDay), false, true)
        else
          DoDateChange(Date, IncMonth(Date, 1), false, true);

    VK_LEFT:
      if (SelectType = stBlockSelect) and (ssShift in Shift) then
      begin
        SetStartEndDate;
        DoDateChange(Date, Date - 1, false, false);
      end
      else
      begin
        if CanChangeDate(Date - 1) then
          DrawMultiSelect(false);
        DoDateChange(Date, Date - 1, false, true);
      end;

    VK_RIGHT:
      if (SelectType = stBlockSelect) and (ssShift in Shift) then
      begin
        SetStartEndDate;
        DoDateChange(Date, Date + 1, false, false);
      end
      else
      begin
        if CanChangeDate(Date + 1) then
          DrawMultiSelect(false);
        DoDateChange(Date, Date + 1, false, true);
      end;

    VK_UP:
      if (SelectType = stBlockSelect) and (ssShift in Shift) then
      begin
        SetStartEndDate;
        DoDateChange(Date, Date - 7, false, false);
      end
      else
      begin
        if CanChangeDate(Date - 7) then
          DrawMultiSelect(false);
        DoDateChange(Date, Date - 7, false, true);
      end;

    VK_DOWN:
      if (SelectType = stBlockSelect) and (ssShift in Shift) then
      begin
        SetStartEndDate;
        DoDateChange(Date, Date + 7, false, false);
      end
      else
      begin
        if CanChangeDate(Date + 7) then
          DrawMultiSelect(false);
        DoDateChange(Date, Date + 7, false, true);
      end;

    VK_SPACE: JumpToToday;
    VK_RETURN:
      case SelectType of
        stDateSelect: DoOnSelectDate;
        stBlockSelect: DoOnBlockSelect;
      end;
  end;

  if (SelectType = stBlockSelect) and (FStartDate > 0) and (FEndDate > 0) and (FEndDate <> Date) then
  begin
    DrawMultiSelect(false);
    FEndDate := Date;
    DrawMultiSelect(true);
  end;
end;

procedure TdsCalendar.BtnUp;
begin
  FUpDown.Visible := false;
  DoDateChange(Date, IncMonth(Date, 1), false, true);
end;

procedure TdsCalendar.BtnDown;
begin
  FUpDown.Visible := false;
  DoDateChange(Date, IncMonth(Date, -1), false, true);
end;

procedure TdsCalendar.UpDownPress(Sender: TObject; Button: TUDBtnType);
begin
  case Button of
    btNext: DoDateChange(Date, EncodeDate(FYear + 1, FMonth, FDay), false, true);
    btPrev: DoDateChange(Date, EncodeDate(FYear - 1, FMonth, FDay), false, true);
  end;
end;

procedure TdsCalendar.PopupMonthClick(Sender: TObject);
var
  y, m, d: Word;
begin
  DecodeDate(Date, y, m, d);

  m := TMenuItem(Sender).Tag;
  if d > DaysInMonth(y, m) then d := DaysInMonth(y, m);

  DoDateChange(Date, EncodeDate(y, m, d), false, true);
end;

function TdsCalendar.GetDate: TDate;
begin
  Result := EncodeDate(FYear, FMonth, FDay);
end;

function TdsCalendar.GetDayRow(y: Integer): Integer;
begin
  if ShowTitle then
    Result := y div FCellHeight - 3
  else
    Result := y div FCellHeight - 1;
end;

function TdsCalendar.GetDayCol(x: Integer): Integer;
begin
  if ShowWeekNumbers then
    Result := (x - 5) div FCellWidth - 1
  else
    Result := (x - 5) div FCellWidth;
end;

function TdsCalendar.GetFirstDayCell: Integer;
var
  FirstDay: Integer;
begin
  FirstDay := SysUtils.DayOfWeek(EncodeDate(FYear, FMonth, 1)); //1=sunday do 7=Saturday

  if FFirstDayOfWeekByte < FirstDay then
    Result := FirstDay - FFirstDayOfWeekByte
  else
    Result := 7 - FFirstDayOfWeekByte + FirstDay;
end;

function TdsCalendar.GetStartDate: TDate;
begin
  if FStartDate > FEndDate then
    Result := FEndDate
  else
    Result := FStartDate;
end;

function TdsCalendar.GetEndDate: TDate;
begin
  if FEndDate > FStartDate then
    Result := FEndDate
  else
    Result := FStartDate;
end;

procedure TdsCalendar.WMSize(var Msg: TWMSize);
begin
  inherited;
  Width := FNumCols * FCellWidth + 10;
  Height := FNumRows * FCellHeight + 6
end;

function TdsCalendar.RectOfToday: TRect;
begin
  Canvas.Font.Assign(FTitleFont);
  Result.Left := 5;
  Result.Right := 5 + FCellWidth + 5 + Canvas.TextWidth(FTodayString + DateTimeToStr(Date)) + 5;
  Result.Bottom := Height - 3;
  Result.Top := Result.Bottom - FCellHeight;
end;

function TdsCalendar.RectOfDay(ADay: Word): TRect;
var
  row, col: Word;
begin
  row := (FDayOffset + ADay - 1) div 7;
  col := (FDayOffset + ADay - 1) mod 7;
  Result := RectOfCell(row, col);
end;

function TdsCalendar.RectOfMonth: TRect;
begin
  Canvas.Font.Assign(FTitleFont);
  Result.Left := 5;
  Result.Right := Result.Left + Canvas.TextWidth(LongMonthNames[FMonth]);
  Result.Top := FCellHeight - (Canvas.TextHeight(LongMonthNames[FMonth]) div 2);
  Result.Bottom := Result.Top + FCellHeight;
end;

function TdsCalendar.RectOfYear: TRect;
begin
  Canvas.Font.Assign(FTitleFont);
  Result.Left := Canvas.TextWidth(LongMonthNames[FMonth]) + 6;
  Result.Right := Result.Left + Canvas.TextWidth(IntToStr(FYear));
  Result.Top := FCellHeight - (Canvas.TextHeight(LongMonthNames[FMonth]) div 2);
  Result.Bottom := Result.Top + FCellHeight;
end;

function TdsCalendar.RectOfCell(row, col: Word): TRect;
begin
  if ShowWeekNumbers then
    Result.Left := (col + 1) * FCellWidth + 5
  else
    Result.Left := col * FCellWidth + 5;

  Result.Right := Result.Left + FCellWidth;

  if ShowTitle then
    Result.Top := (FCellHeight * 3) + row * FCellHeight
  else
    Result.Top := FCellHeight + row * FCellHeight;

  Result.Bottom := Result.Top + FCellHeight;
end;

function TdsCalendar.RectOfDays: TRect;
begin
  if ShowTitle then
    Result.Top := 3 * FCellHeight
  else
    Result.Top := FCellHeight;

  Result.Bottom := Result.Top + 6 * FCellHeight;

  if ShowWeekNumbers then
    Result.Left := FCellWidth + 5
  else
    Result.Left := 5;

  Result.Right := Width - 5;
end;

function TdsCalendar.DoDateChange(FromDate, ToDate: TDateTime; MoveMouse, ResetStartEnd: Boolean): Boolean;
var
  OldYear, OldMonth, OldDay: Word;
  p: TPoint;
  r: TRect;
begin
  if CanChangeDate(ToDate) then
  begin
    Result := true;

    if ResetStartEnd then ResetStartEndDate;

    DecodeDate(ToDate, FYear, FMonth, FDay);
    DecodeDate(FromDate, OldYear, OldMonth, OldDay); //get old year, month and day

    if (OldMonth <> FMonth) or (OldYear <> FYear) then
    begin
      Refresh;

      if MoveMouse then
      begin
        r := RectOfDay(FDay);
        p.x := r.Left + ((r.Right - r.Left) div 2) + ClientOrigin.x;
        p.y := r.Top + ((r.Bottom - r.Top) div 2) + ClientOrigin.y;
        Mouse.CursorPos := p;
      end;
    end
    else
    begin
      DrawNormalDay(RectOfDay(OldDay), OldYear, OldMonth, OldDay);
      DrawSelectedDay(RectOfDay(FDay), FYear, FMonth, FDay);
      DrawTodayCircle;
    end;

    if Assigned(FOnDateChange) then FOnDateChange(Self, FromDate, ToDate);

    if ResetStartEnd and (SelectType = stBlockSelect) then
    begin
      FStartDate := Date;
      FEndDate := FStartDate;
    end;
  end
  else
  begin
    Result := false;
    DecodeDate(FromDate, FYear, FMonth, FDay); //set date back to old date
  end;
end;

function TdsCalendar.AllowMultiSelect: Boolean;
begin
  Result := SelectType = stBlockSelect;
end;

function TdsCalendar.CanChangeDate(ToDate: TDate): Boolean;

  function DateInThisMonth: Boolean;
  var
    y, m, d: Word;
  begin
    DecodeDate(ToDate, y, m, d);
    Result := (m = FMonth);
  end;

begin
  Result := inherited CanChangeDate(ToDate);

  if not CanChangeMonthYear then
    Result := Result and DateInThisMonth;
end;

procedure TdsCalendar.DoOnSelectDate;
begin
  if Assigned(FOnDateSelect) then FOnDateSelect(Self);
end;

procedure TdsCalendar.DoOnBlockSelect;
begin
  if Assigned(FOnBlockSelect) then FOnBlockSelect(Self);
end;

procedure TdsCalendar.SetCellWidth(Value: Integer);
begin
  if FCellWidth <> Value then
  begin
    FCellWidth := Value;
    RecalculateSize;
  end;
end;

procedure TdsCalendar.SetCellHeight(Value: Integer);
begin
  if FCellHeight <> Value then
  begin
    FCellHeight := Value;
    RecalculateSize;
  end;
end;

procedure TdsCalendar.SetShowToday(Value: Boolean);
begin
  if ShowToday <> Value then
  begin
    FShowToday := Value;
    SetNumRows;
    RecalculateSize;
  end;
end;

procedure TdsCalendar.SetShowWeekNumbers(Value: Boolean);
begin
  if FShowWeekNumbers <> Value then
  begin
    FShowWeekNumbers := Value;
    SetNumCols;
    RecalculateSize;
  end;
end;

procedure TdsCalendar.SetShowTodayCircle(Value: Boolean);
begin
  if FShowTodayCircle <> Value then
  begin
    FShowTodayCircle := Value;
    Refresh;
  end;
end;

procedure TdsCalendar.SetShowYear(Value: Boolean);
begin
  if FShowYear <> Value then
  begin
    FShowYear := Value;
    Invalidate;
  end;
end;

procedure TdsCalendar.SetShowTitle(Value: Boolean);
begin
  if FShowTitle <> Value then
  begin
    FShowTitle := Value;
    SetButtonsParent(FShowTitle);
    SetNumRows;
    RecalculateSize;
  end;
end;

procedure TdsCalendar.SetHolidays(Value: THolidays);
begin
  FHolidays.Assign(Value);
end;

procedure TdsCalendar.SetFirstDayOfWeek(Value: TDayOfWeek);
begin
  if FirstDayOfWeek <> Value then
  begin
    FFirstDayOfWeek := Value;
    case Value of
      dwMonday:        FFirstDayOfWeekByte := 2;
      dwTuesday:       FFirstDayOfWeekByte := 3;
      dwWednesday:     FFirstDayOfWeekByte := 4;
      dwThursday:      FFirstDayOfWeekByte := 5;
      dwFriday:        FFirstDayOfWeekByte := 6;
      dwSaturday:      FFirstDayOfWeekByte := 7;
      dwSunday:        FFirstDayOfWeekByte := 1;
    end;
    Refresh;
  end;
end;

procedure TdsCalendar.JumpToToday;
begin
  if DoDateChange(Date, SysUtils.Date, false, false) then
    if SelectType = stBlockSelect then
    begin
      DrawMultiSelect(false);
      FStartDate := Date;
      FEndDate := Date;
      DoOnBlockSelect;
    end;
end;

procedure TdsCalendar.SetMinMaxForMultiSelect(var min, max: TDate);
begin
  if FStartDate < FEndDate then
  begin
    min := FStartDate;
    max := FEndDate;
  end
  else
  begin
    min := FEndDate;
    max := FStartDate;
  end;
end;

procedure TdsCalendar.ResetStartEndDate;
begin
  FStartDate := 0;
  FEndDate := 0;
end;

procedure TdsCalendar.SetNumRows;
var
  i: Integer;
begin
  if ShowTitle then
    i := 8
  else
    i := 6;

  Inc(i);
  if ShowToday then Inc(i);

  FNumRows := i;
end;

procedure TdsCalendar.SetNumCols;
begin
  if FShowWeekNumbers then
    FNumCols := 8
  else
    FNumCols := 7;
end;

procedure TdsCalendar.GetPrevMonth(var AYear, AMonth: Word);
begin
  AMonth := AMonth - 1;
  if AMonth < 1 then
  begin
    AMonth := 12;
    AYear := AYear - 1;
  end;
end;

procedure TdsCalendar.GetNextMonth(var AYear, AMonth: Word);
begin
  AMonth := AMonth + 1;
  if AMonth > 12 then
  begin
    AMonth := 1;
    AYear := AYear + 1;
  end;
end;

procedure TdsCalendar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;

  procedure DoMonth;
  begin
    FUpDown.Visible := false;

    if CanChangeMonthYear then
    begin
      p := ClientToScreen(p);
      FPopupMonth.Popup(p.x, p.y);
    end;
  end;

  procedure DoToday;
  begin
    FUpDown.Visible := false;
    JumpToToday;
  end;

  procedure DoYear;
  var
    R: TRect;
  begin
    if CanChangeMonthYear and not FUpDown.Visible then
    begin
      R := RectOfYear;
      FUpDown.SetBounds(R.Right + 5, FCellHeight - 12, 20, 24);
      FUpDown.BringToFront;
      FUpDown.Visible := true;
    end;
  end;

  procedure DoDay;
  var
    diff: Integer;
    col, row: Integer;
    OldMonth: Word;
    b: Boolean;
  begin
    FUpDown.Visible := false;

    OldMonth := FMonth;

    row := GetDayRow(y);
    col := GetDayCol(x);

    diff := (row * 7 + (col + 1) - FDayOffset) - FDay;

    if (FStartDate <> FEndDate) and CanChangeDate(Date + diff) then
      DrawMultiSelect(false);

    b := DoDateChange(Date, Date + diff, SelectType = stBlockSelect, true);

    if (FMonth = OldMonth) and (SelectType = stDateSelect) then DoOnSelectDate;

    if b and AllowMultiSelect then
      FMouseDown := true;
  end;

begin
  inherited;

  if not Focused then SetFocus;

  p.x := x;
  p.y := y;

  if ShowTitle and PtInRect(RectOfMonth, p) then DoMonth                 //if user clicked onto month, bring up pop up menu to select month
  else if ShowTitle and ShowYear and PtInRect(RectOfYear, p) then DoYear //if user clicked onto year, bring up spin edit to change year
  else if PtInRect(RectOfDays, p) then DoDay                             //if user clicked onto one of the days, switch to that day
  else if ShowToday and PtInRect(RectOfToday, p) then DoToday            //if user clicked onto today, switch to today
end;

procedure TdsCalendar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
  diff: Integer;
  col, row: Integer;
begin
  inherited;

  if FMouseDown then
  begin
    p.x := x;
    p.y := y;

    if PtInRect(RectOfDays, p) then
    begin
      row := GetDayRow(y);
      col := GetDayCol(x);

      diff := (row * 7 + (col + 1) - FDayOffset) - FDay;

      if diff <> 0 then
        DoDateChange(Date, Date + diff, true, false);
    end;

    if FEndDate <> Date then
    begin
      DrawMultiSelect(false);
      FEndDate := Date;
      DrawMultiSelect(true);
    end;
  end;
end;

procedure TdsCalendar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FMouseDown then
  begin
    FMouseDown := false;
    DoOnBlockSelect;
  end;
end;

procedure TdsCalendar.DoExit;
begin
  FUpDown.Visible := false;
  inherited;
end;

function TdsCalendar.IsHoliday(ADate: TDate): Boolean;
var
  y, m, d: Word;
begin
  DecodeDate(ADate, y, m, d);
  Result := IsDateHoliday(d, m);
end;

function TdsCalendar.IsDateHoliday(ADay, AMonth: Word): Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to FHolidays.Count - 1 do
    if (FHolidays.Items[i].Month = AMonth) and (FHolidays.Items[i].Day = ADay) then
    begin
      Result := true;
      Break;
    end;
end;

function TdsCalendar.DayNumber: Word;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to FMonth - 1 do
    Result := Result + DaysInMonth(FYear, i);
  Result := Result + FDay;
end;

procedure TdsCalendar.SetDate(Value: TDate);
begin
  if Date <> Value then
  begin
    DecodeDate(Value, FYear, FMonth, FDay);
    Invalidate;
  end;
end;

procedure TdsCalendar.SetColors(Value: TdsCalendarColors);
begin
  FColors.Assign(Value);
end;

procedure TdsCalendar.SetSelectType(Value: TSelectType);
begin
  if FSelectType <> Value then
  begin
    FSelectType := Value;
    Invalidate;
  end;
end;

procedure TdsCalendar.SetCanChangeMonthYear(Value: Boolean);
begin
  if FCanChangeMonthYear <> Value then
  begin
    FCanChangeMonthYear := Value;
    SetButtonsParent(FCanChangeMonthYear);
  end;
end;

procedure TdsCalendar.SetYear(Value: Word);
begin
  if Value <> FMonth then
  begin
    FYear := Value;
    Invalidate;
  end;
end;

procedure TdsCalendar.SetMonth(Value: Word);
begin
  if Value <> FMonth then
  begin
    FMonth := Value;
    Invalidate;
  end;
end;

procedure TdsCalendar.SetDay(Value: Word);
begin
  if Value <> FMonth then
  begin
    FDay := Value;
    Invalidate;
  end;
end;

function TdsCalendar.WeekNumber(ADate: TDate): Integer;
var
  AYear, AMonth, ADay: Word;
  StartOfYear:  TDate;
  plus: Integer;
  DaysToEndOfWeek: Integer;


  function GetDaysToEndOfWeek: Integer;
  var
    ADay: Integer;
  begin
    Result := 0;
    ADay := SysUtils.DayOfWeek(StartOfYear);
    while ADay <> FFirstDayOfWeekByte do
    begin
      Inc(Result);
      Inc(ADay);
      if ADay > 7 then ADay := 1;
    end;
  end;

begin
  DecodeDate(ADate, AYear, AMonth, ADay);
  StartOfYear := EncodeDate(AYear, 1, 1);

  DaysToEndOfWeek := GetDaysToEndOfWeek;

  if DaysToEndOfWeek > 3 then
    plus := 6 + SysUtils.DayOfWeek(StartOfYear) - FFirstDayOfWeekByte
  else
    plus := 7 - DaysToEndOfWeek;

  Result := (Trunc(ADate - StartOfYear) + 1 + plus) div 7;
  if Result = 53 then
  begin
    //if we have week number 53, it can very much be first week in the next year
    //it's easy to check
    //if next week is number 2 that means that this week is number 1
    //in next year and not number 53 in this year
    if WeekNumber(ADate + 7) = 2 then Result := 1;
  end;
end;

procedure TdsCalendar.DrawMonth;
var
  R: TRect;
  str: String;
begin
  FUpButton.Visible := CanChangeMonthYear and ShowTitle;
  FDownButton.Visible := CanChangeMonthYear and ShowTitle;

  if ShowTitle then
  begin
    R := ClientRect;
    R.Bottom := 2 * FCellHeight; //this rect will paint across first two rows
    Canvas.Brush.Color := FColors.TitleColor;
    Canvas.FillRect(R);

    R.Left := 5; //We want a bit of a margin at the left
    Canvas.Font.Assign(FTitleFont);
    str := LongMonthNames[FMonth];
    if FShowYear then
      str := str + ' ' + IntToStr(FYear);
    DrawText(Canvas.Handle, PChar(str), Length(str), R, DT_SINGLELINE + DT_VCENTER);

    PositionButtons;
  end;
end;

procedure TdsCalendar.DrawDayNames;
var
  R: TRect;
  str: String;
  col: Integer;
begin
  R := ClientRect;
  if ShowTitle then
    R.Top := 2 * FCellHeight
  else
    R.Top := 0;
  R.Bottom := R.Top + FCellHeight;
  Canvas.Brush.Color := FColors.BkColor;
  Canvas.FillRect(R);

  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := FColors.TitleColor;
  Canvas.MoveTo(5, R.Bottom - 1);
  Canvas.LineTo(Width - 5, R.Bottom - 1);

  Canvas.Font.Assign(FTextFont);
  Canvas.Font.Color := FColors.TitleColor;

  for col := 0 to 6 do
  begin
    R.Left := (col + (FNumCols - 7)) * FCellWidth + 5;
    R.Right := R.Left + FCellWidth;
    str := ShortDayNames[(col + FFirstDayOfWeekByte - 1) mod 7 + 1];
    DrawText(Canvas.Handle, PChar(str), Length(str), R, DT_SINGLELINE + DT_VCENTER + DT_CENTER);
  end;
end;

procedure TdsCalendar.DrawDays;
var
  R: TRect;
  i: Integer;
  cell: Byte;
  TmpMonth, TmpYear: Word;
begin
  R := ClientRect;
  if ShowWeekNumbers then
    R.Left := FCellWidth;
  if ShowTitle then
    R.Top := 3 * FCellHeight
  else
    R.Top := FCellHeight;
  R.Bottom := R.Top + 6 * FCellHeight + 2;

  Canvas.Brush.Color := FColors.BkColor;
  Canvas.FillRect(R);

  cell := 0;
  FDayOffset := GetFirstDayCell;

  //draw days of previous month
  TmpYear := FYear;
  TmpMonth := FMonth;
  GetPrevMonth(TmpYear, TmpMonth);

  for i := DaysInMonth(TmpYear, TmpMonth) - FDayOffset + 1 to DaysInMonth(TmpYear, TmpMonth) do
  begin
    DrawNormalDay(RectOfCell(cell div 7, cell mod 7), TmpYear, TmpMonth, i);
    Inc(cell);
  end;

  //draw days of this month
  for i := 1 to DaysInMonth(FYear, FMonth) do
  begin
    DrawNormalDay(RectOfCell(cell div 7, cell mod 7), FYear, FMonth, i);
    Inc(cell);
  end;

  //draw days of next month
  TmpYear := FYear;
  TmpMonth := FMonth;
  GetNextMonth(TmpYear, TmpMonth);

  for i := 1 to 42 - cell do
  begin
    DrawNormalDay(RectOfCell(cell div 7, cell mod 7), TmpYear, TmpMonth, i);
    Inc(cell);
  end;

  if SelectType = stBlockSelect then
    DrawMultiSelect(true);

  DrawDatePosition;
end;

procedure TdsCalendar.DrawMultiSelect(selected: Boolean);
var
  i, min, max: TDate;
  AYear, AMonth, ADay: Word;
  cell: Integer;

  function PrevMonth(AMonth: Word): Word;
  var
    TmpYear: Word;
  begin
    TmpYear := FYear;
    Result := AMonth;
    GetPrevMonth(TmpYear, Result);

    if (Result = 12) and not (TmpYear = FYear - 1) then
      Result := 0;
  end;

  function NextMonth(AMonth: Word): Word;
  var
    TmpYear: Word;
  begin
    TmpYear := FYear;
    Result := AMonth;
    GetNextMonth(TmpYear, Result);

    if (Result = 1) and not (TmpYear = FYear + 1) then
      Result := 0;
  end;

  procedure Draw(R: TRect; Grayed: Boolean);
  begin
    if selected then
      DrawSelectedDay(R, AYear, AMonth, ADay)
    else
      DrawNormalDay(R, AYear, AMonth, ADay)
  end;

begin
  SetMinMaxForMultiSelect(min, max);

  if (min <> 0) and (max <> 0) then
  begin
    i := min;

    while i <= max do
    begin
      DecodeDate(i, AYear, AMonth, ADay);

      if (AYear = FYear) and (AMonth = FMonth) then
        Draw(RectOfDay(ADay), false)
      else
      begin
        if (AMonth = PrevMonth(FMonth)) and (ADay > DaysInMonth(FYear, AMonth) - GetFirstDayCell) then
        begin
          cell := ADay - DaysInMonth(FYear - 1, AMonth) + GetFirstDayCell - 1;
          Draw(RectOfCell(cell div 7, cell mod 7), true);
        end
        else if (AMonth = NextMonth(FMonth)) and (ADay + DaysInMonth(FYear, AMonth) + GetFirstDayCell < 42) then
        begin
          cell := GetFirstDayCell + DaysInMonth(FYear, FMonth) + ADay - 1;
          Draw(RectOfCell(cell div 7, cell mod 7), true);
        end;
      end;

      i := i + 1;
    end;

    DrawTodayCircle;
  end;
end;

procedure TdsCalendar.DrawWeekNumbers;
var
  R: TRect;
  str: String;
  row: Integer;
  ADate: TDate;
begin
  if ShowWeekNumbers then
  begin
    R := ClientRect;
    if ShowTitle then
      R.Top := 3 * FCellHeight
    else
      R.Top := FCellHeight;
    R.Right := FCellWidth;
    R.Bottom := R.Top + 6 * FCellHeight + 6;
    Canvas.Brush.Color := FColors.BkColor;
    Canvas.FillRect(R);

    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := FColors.TitleColor;
    Canvas.MoveTo(FCellWidth - 1, R.Top + 2);
    Canvas.LineTo(FCellWidth - 1, R.Bottom + 1);

    Canvas.Brush.Color := FColors.BkColor;
    Canvas.Font.Color := FColors.TitleColor;
    Canvas.Font.Style := [];

    //first day on the calendar
    ADate := EncodeDate(FYear, FMonth, 1) - FDayOffset;

    if ShowTitle then
      row := 3
    else
      row := 1;

    for row := row to row + 5 do
    begin
      R.Top := row * FCellHeight;
      R.Bottom := R.Top + FCellHeight;

      str := IntToStr(WeekNumber(ADate));
      ADate := ADate + 7;

      DrawText(Canvas.Handle, PChar(str), Length(str), R, DT_SINGLELINE + DT_VCENTER + DT_CENTER);
    end;
  end;
end;

procedure TdsCalendar.DrawToday;
var
  R: TRect;
  str: String;
begin
  if ShowToday then
  begin
    R := ClientRect;
    R.Top := (FNumRows - 1) * FCellHeight;
    R.Bottom := FNumRows * FCellHeight + 6;
    Canvas.Brush.Color := FColors.BkColor;
    Canvas.FillRect(R);

    R := RectOfToday;
    str := FTodayString + DateTimeToStr(SysUtils.Date);

    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := FColors.TitleColor;
    Canvas.MoveTo(5, R.Top - 1);
    Canvas.LineTo(FNumCols * FCellWidth + 5, R.Top - 1);

    Canvas.Pen.Width := 3;
    Canvas.Pen.Color := FColors.CircleColor;
    DrawSelectedShape(Canvas, FSelectedShape, R.Left + 1, R.Top + 3, R.Left + FCellWidth - 1, R.Bottom - 1);
    Canvas.Font.Color := FTextFont.Color;
    Canvas.Font.Style := [fsBold];
    Canvas.TextOut(R.Left + FCellWidth + 5, (R.Top + (R.Bottom - R.Top) div 2) - Canvas.TextHeight(str) div 2 + 1, str);
    Canvas.Font.Style := [];
  end;
end;

procedure TdsCalendar.DrawTodayCircle;
var
  R: TRect;
  AYear, AMonth, ADay: Word;
begin
  if FShowTodayCircle then
  begin
    DecodeDate(SysUtils.Date, AYear, AMonth, ADay);
    if (AYear = FYear) and (AMonth = FMonth) then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Width := 3;
      Canvas.Pen.Color := FColors.CircleColor;
      R := RectOfDay(ADay);
      InflateRect(R, 1, 1);
      DrawSelectedShape(Canvas, FSelectedShape, R.Left + 1, R.Top + 1, R.Right - 1, R.Bottom - 1)
    end;
  end;
end;

procedure TdsCalendar.DrawDatePosition;
var
  cell: Byte;
  R: TRect;
begin
  cell := FDay + FDayOffset - 1;
  R := RectOfCell(cell div 7, cell mod 7);
  DrawSelectedDay(R, FYear, FMonth, FDay);
end;

procedure TdsCalendar.SetNormalCanvas(AYear, AMonth, ADay: Word);
begin
  Canvas.Font.Assign(FTextFont);
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := FColors.BkColor;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := FColors.TitleColor;

  if AMonth <> Month then
    Canvas.Font.Color := FColors.TrailingDasyColor
  else
    if (SysUtils.DayOfWeek(EncodeDate(AYear, AMonth, ADay)) = 1) or IsDateHoliday(ADay, AMonth) then
      Canvas.Font.Color := FColors.MarkColor;
end;

procedure TdsCalendar.SetSelectedCanvas(AYear, AMonth, ADay: Word; Grayed: Boolean);
begin
  Canvas.Font.Assign(FTextFont);
  Canvas.Pen.Color := FColors.TitleColor;
  Canvas.Brush.Color := FColors.TitleColor;

  if TransparentCursor then
    Canvas.Brush.Style := bsClear;

  if Grayed then
    Canvas.Font.Color := FColors.TrailingDasyColor
  else
    if TransparentCursor then
      Canvas.Font.Color := clBlack
    else
      Canvas.Font.Color := clWhite;
end;

procedure TdsCalendar.DrawDayText(R: TRect; const s: String);
begin
  Canvas.Brush.Style := bsClear;
  DrawText(Canvas.Handle, PChar(s), Length(s), R, DT_SINGLELINE + DT_VCENTER + DT_CENTER);
end;

procedure TdsCalendar.DrawNormalDay(R: TRect; AYear, AMonth, ADay: Word);
begin
  SetNormalCanvas(AYear, AMonth, ADay);
  Canvas.FillRect(R);
  DrawDayText(R, IntToStr(ADay));
end;

procedure TdsCalendar.DrawSelectedDay(R: TRect; AYear, AMonth, ADay: Word);
begin
  SetNormalCanvas(AYear, AMonth, ADay);
  Canvas.FillRect(R);
  SetSelectedCanvas(AYear, AMonth, ADay, AMonth <> Month);
  DrawSelectedShape(Canvas, FSelectedShape, R.Left + 2, R.Top + 2, R.Right - 2, R.Bottom - 2);
  DrawDayText(R, IntToStr(ADay));
end;

procedure TdsCalendar.Paint;
begin
  DrawMonth;
  DrawDayNames;
  DrawDays;
  DrawTodayCircle;
  DrawWeekNumbers;
  DrawToday;
  inherited;
end;

//**********************************************************************************
// TdsMonths
//**********************************************************************************
constructor TdsMonths.Create(AOwner: TComponent);
begin
  inherited;
  FCellHeight := 22;
  FNumRows := 14;
  Width := 153;
  FShowThisMonth := true;
  DecodeDate(SysUtils.Date, FYear, FMonth, FDay);
  FColors := TdsMonthsColors.Create(Self);
  FCanChangeYear := true;
  FShowTitle := true;
  PositionButtons;
end;

destructor TdsMonths.Destroy;
begin
  FColors.Free;
  inherited;
end;

function TdsMonths.GetDate: TDate;
begin
  Result := EncodeDate(FYear, FMonth, 1);
end;

procedure TdsMonths.SetDate(Value: TDate);
begin
  if Date <> Value then DoDateChange(Date, Value);
end;

procedure TdsMonths.SetShowThisMonth(Value: Boolean);
begin
  if FShowThisMonth <> Value then
  begin
    FShowThisMonth := Value;
    Refresh;
  end;
end;

procedure TdsMonths.SetColors(Value: TdsMonthsColors);
begin
  FColors.Assign(Value);
end;

procedure TdsMonths.SetCanChangeYear(Value: Boolean);
begin
  if FCanChangeYear <> Value then
  begin
    FCanChangeYear := Value;
    SetButtonsParent(FCanChangeYear);
  end;
end;

procedure TdsMonths.SetCellHeight(Value: Integer);
begin
  if FCellHeight <> Value then
  begin
    FCellHeight := Value;
    RecalculateSize;
  end;
end;

procedure TdsMonths.SetShowTitle(Value: Boolean);
begin
  if FShowTitle <> Value then
  begin
    FShowTitle := Value;
    SetButtonsParent(FShowTitle);
    SetNumRows;
    RecalculateSize;
  end;
end;

procedure TdsMonths.SetYear(Value: Word);
begin
  if Value <> FMonth then
  begin
    FYear := Value;
    Invalidate;
  end;
end;

procedure TdsMonths.SetMonth(Value: Word);
begin
  if Value <> FMonth then
  begin
    FMonth := Value;
    Invalidate;
  end;
end;

procedure TdsMonths.WMSize(var Msg: TWMSize);
begin
  inherited;
  Height := FNumRows * FCellHeight;
end;

procedure TdsMonths.BtnUp;
begin
  DoDateChange(Date, IncMonth(Date, 12));
end;

procedure TdsMonths.BtnDown;
begin
  DoDateChange(Date, IncMonth(Date, -12));
end;

procedure TdsMonths.DoDateChange(FromDate, ToDate: TDateTime);
var
  OldYear, OldMonth, OldDay: Word;
begin
  if CanChangeDate(ToDate) then
  begin
    DecodeDate(ToDate, FYear, FMonth, FDay);
    DecodeDate(FromDate, OldYear, OldMonth, OldDay); //get old year, month and day

    if OldYear <> FYear then
      Refresh
    else
    begin
      DrawMonth(RectOfMonth(OldMonth), OldMonth, false);
      DrawMonth(RectOfMonth(FMonth), FMonth, true);
      DrawThisMonthCircle;
    end;

    if Assigned(FOnDateChange) then FOnDateChange(Self, FromDate, ToDate);
  end
  else DecodeDate(FromDate, FYear, FMonth, FDay); //set date back to old date
end;

procedure TdsMonths.DoOnSelectMonth;
begin
  if Assigned(FOnSelectMonth) then FOnSelectMonth(Self);
end;

procedure TdsMonths.Paint;
begin
  DrawYear;
  DrawMonths;
  DrawThisMonthCircle;
  inherited;
end;

procedure TdsMonths.DrawYear;
var
  R: TRect;
  str: String;
begin
  FUpButton.Visible := CanChangeYear and ShowTitle;
  FDownButton.Visible := CanChangeYear and ShowTitle;

  if FShowTitle then
  begin
    R := ClientRect;
    R.Bottom := 2 * FCellHeight; //this rect will paint across first two rows
    Canvas.Brush.Color := FColors.TitleColor;
    Canvas.FillRect(R);

    R.Left := 5; //margin at the left
    Canvas.Font.Assign(TitleFont);
    str := IntToStr(FYear);
    DrawText(Canvas.Handle, PChar(str), Length(str), R, DT_SINGLELINE + DT_VCENTER);

    PositionButtons;
  end;
end;

procedure TdsMonths.DrawMonths;
var
  R: TRect;
  i: Word;
  DrawDate: TDate;
begin
  R := ClientRect;
  if ShowTitle then
    R.Top := 2 * FCellHeight;
  R.Bottom := R.Top + 12 * FCellHeight;
  Canvas.Brush.Color := FColors.BkColor;
  Canvas.FillRect(R);

  for i := 1 to 12 do
  begin
    R := RectOfMonth(i);
    DrawDate := EncodeDate(FYear, i, 1);
    DrawMonth(R, i, DrawDate = EncodeDate(Year, Month, 1));
  end;
end;

procedure TdsMonths.DrawThisMonthCircle;
var
  R: TRect;
  AYear, AMonth, ADay: Word;
begin
  if FShowThisMonth then
  begin
    DecodeDate(SysUtils.Date, AYear, AMonth, ADay);
    if AYear = FYear then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Width := 3;
      Canvas.Pen.Color := FColors.CircleColor;
      R := RectOfMonth(AMonth);
      InflateRect(R, 2, 2);
      DrawSelectedShape(Canvas, FSelectedShape, R.Left + 1, R.Top + 1, R.Right - 1, R.Bottom - 1)
    end;
  end;
end;

procedure TdsMonths.DrawMonth(R: TRect; AMonth: Word; Selected: Boolean);
var
  str: String;
begin
  str := LongMonthNames[AMonth];

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := FColors.BkColor;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := FColors.TitleColor;
  Canvas.FillRect(R);

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Assign(FTextFont);

  if Selected then
  begin
    Canvas.Pen.Color := FColors.TitleColor;
    Canvas.Brush.Color := FColors.TitleColor;
    if TransparentCursor then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Font.Color := clBlack;
    end
    else Canvas.Font.Color := clWhite;
    DrawSelectedShape(Canvas, FSelectedShape, R.Left + 2, R.Top + 2, R.Right - 2, R.Bottom - 2);
  end;

  DrawText(Canvas.Handle, PChar(str), Length(str), R, DT_SINGLELINE + DT_VCENTER + DT_CENTER);
end;

procedure TdsMonths.JumpToThisMonth;
begin
  DoDateChange(Date, SysUtils.Date);
end;

procedure TdsMonths.SetNumRows;
begin
  if ShowTitle then
    FNumRows := 14
  else
    FnumRows := 12;
end;

function TdsMonths.RectOfMonths: TRect;
begin
  Result.Top := 2 * FCellHeight;
  Result.Bottom := 14 * FCellHeight;
  Result.Left := 5;
  Result.Right := Width - 5;
end;

function TdsMonths.RectOfMonth(AMonth: Word): TRect;
begin
  Result := ClientRect;
  Result.Left := 5;
  Result.Right := Result.Right - 5;
  if ShowTitle then
    Result.Top := 2 * FCellHeight;
  Result.Top := Result.Top + ((AMonth - 1) * FCellHeight);
  Result.Bottom := Result.Top + FCellheight;
end;

procedure TdsMonths.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  case Key of
    VK_NEXT: BtnPress(FUpButton);                   //PageDown - year down
    VK_PRIOR: BtnPress(FDownButton);                //PageUp - year up
    VK_UP: DoDateChange(Date, IncMonth(Date, -1));
    VK_DOWN: DoDateChange(Date, IncMonth(Date, 1));
    VK_SPACE: JumpToThisMonth;                      //space jumps to this month
    VK_RETURN: DoOnSelectMonth;
  end;
end;

procedure TdsMonths.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;

  procedure SelectMonth;
  var
    NewDate: TDate;
    row: Integer;
    OldMonth: Word;
  begin
    OldMonth := FMonth;
    row := y div FCellHeight - 1;
    NewDate := IncMonth(Date, row - OldMonth);
    DoDateChange(Date, NewDate);
    DoOnSelectMonth;
  end;

begin
  inherited;

  if not Focused then SetFocus;

  p.x := x;
  p.y := y;

  if PtInRect(RectOfMonths, p) then SelectMonth; //if user clicked onto one of the months, switch to that month
end;



constructor TdsPlanerCategorie.Create(ACollection: TCollection);
begin
  inherited;
  FDates := TStringList.Create;
  TStringList(FDates).Duplicates := dupIgnore;
  TStringList(FDates).OnChange := DatesChange;
  FVisible := true;
  FColor := clGreen;
end;

destructor TdsPlanerCategorie.Destroy;
begin
  FDates.Free;
  inherited;
end;

procedure TdsPlanerCategorie.SetDates(Value: TStrings);
begin
  FDates.Assign(Value);
end;

procedure TdsPlanerCategorie.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    TdsPlanerCategories(Collection).FCalendar.DrawDays;
    TdsPlanerCategories(Collection).FCalendar.DrawTodayCircle;
  end;
end;

procedure TdsPlanerCategorie.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    TdsPlanerCategories(Collection).FCalendar.DrawDays;
    TdsPlanerCategories(Collection).FCalendar.DrawTodayCircle;
  end;
end;

procedure TdsPlanerCategorie.DatesChange(Sender: TObject);
begin
  TdsPlanerCategories(Collection).FCalendar.DrawDays;
  TdsPlanerCategories(Collection).FCalendar.DrawTodayCircle;
end;

function TdsPlanerCategorie.GetDisplayName: string;
begin
  Result := FName;
  if Result = '' then Result := inherited GetDisplayName;
end;


constructor TdsPlanerCategories.Create(ACalendar: TdsPlanner);
begin
  inherited Create(TdsPlanerCategorie);
  FCalendar := ACalendar;
end;

function TdsPlanerCategories.GetField(index: Integer): TdsPlanerCategorie;
begin
  Result := TdsPlanerCategorie(inherited Items[index]);
end;

procedure TdsPlanerCategories.SetField(index: Integer; Value: TdsPlanerCategorie);
begin
  Items[index].Assign(Value);
end;

function TdsPlanerCategories.GetOwner: TPersistent;
begin
  Result := FCalendar;
end;

function TdsPlanerCategories.Add: TdsPlanerCategorie;
begin
  Result := TdsPlanerCategorie(inherited Add);
end;


constructor TdsPlanner.Create(AOwner: TComponent);
begin
  inherited;
  FCategories := TdsPlanerCategories.Create(Self);
end;

destructor TdsPlanner.Destroy;
begin
  FCategories.Free;
  inherited;
end;

procedure TdsPlanner.SelectDeselectDate(d: TDate);
var
  i: Integer;
begin
  i := FCategories[FActiveCategorie].Dates.IndexOf(DateToStr(d));

  if i <> - 1 then
    FCategories[FActiveCategorie].Dates.Delete(i)
  else
  begin
    if not FOverlapDates then
      RemoveDateFromCategories(d);

    FCategories[FActiveCategorie].Dates.Add(DateToStr(d));
  end;
end;

procedure TdsPlanner.SetCategories(Value: TdsPlanerCategories);
begin
  FCategories.Assign(Value);
end;

procedure TdsPlanner.SetNormalCanvas(AYear, AMonth, ADay: Word);
var
  c: TdsPlanerCategorie;
begin
  inherited;

  c := GetDateCategorie(EncodeDate(AYear, AMonth, ADay));

  if Assigned(c) then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := c.Color;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := c.Color;
    Canvas.Font.Color := clWhite;
  end;
end;

procedure TdsPlanner.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    FShift := true;
    if SelectType = stDateSelect then
      SelectDeselectDate(Date);
  end;
  inherited;
end;

procedure TdsPlanner.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FShift := ssShift in Shift;
  inherited;
end;

procedure TdsPlanner.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (ssShift in Shift) and (SelectType = stDateSelect) then
    SelectDeselectDate(Date);
end;

procedure TdsPlanner.DoOnBlockSelect;
var
  i: Integer;
begin
  if FShift then
  begin
    FShift := false;
    for i := Trunc(StartDate) to Trunc(EndDate) do
    begin
      SelectDeselectDate(i);
      DrawMultiSelect(false);
    end;
    DrawDatePosition;
  end;
end;

function TdsPlanner.AllowMultiSelect: Boolean;
begin
  Result := (SelectType = stBlockSelect) and FShift;
end;

function TdsPlanner.GetDateCategorie(d: TDate): TdsPlanerCategorie;
var
  i, j, r: Integer;
  s: String;
begin
  Result := nil;

  s := DateToStr(d);

  for i := 0 to FCategories.Count - 1 do
    for j := 0 to FCategories[i].Dates.Count - 1 do
    begin
      r := FCategories[i].Dates.IndexOf(s);
      if r <> -1 then
      begin
        Result := FCategories[i];
        Exit;
      end;
    end;
end;

procedure TdsPlanner.RemoveDateFromCategories(d: TDate);
var
  i, j, r: Integer;
  s: String;
begin
  s := DateToStr(d);

  for i := 0 to FCategories.Count - 1 do
    for j := 0 to FCategories[i].Dates.Count - 1 do
    begin
      r := FCategories[i].Dates.IndexOf(s);
      if r <> -1 then
        FCategories[i].Dates.Delete(r);
    end;
end;

procedure Register;
begin
  RegisterComponents('Delphi step', [TdsCalendar, TdsMonths, TdsPlanner]);
end;

end.
