//----------------------------------------------------------------------------//
//- Q T C A L E N D A R                                                      -//
//----------------------------------------------------------------------------//
//- Info:        Calendar-component in MS-Money-Style                        -//
//-              (Freeware with source)                                      -//
//-              Some code is used from Borland-calendar component and other -//
//-              sample sources from Borland                                 -//
//- Version:     0.75        10.May 1999         (not ready!!!)              -//
//- Author:      Frank Demmig                                                -//
//- E-Mail:      Container-Becker@t-online.de                                -//
//-                                                                          -//
//- Properties:  Colors                                                      -//
//-                Colorattributes for various areas in Control              -//
//-              Options                                                     -//
//-                Setups look and behaviour of control                      -//
//-              ViewStyle                                                   -//
//-                Calendardisplay in year- or monthview                     -//
//-              Caption4Year                                                -//
//-                String to display in heading during yearview              -//
//-              Day, Month, Year                                            -//
//-                Actual Date, represented by dateelements                  -//
//-                                                                          -//
//- Events:      OnChange                                                    -//
//-                Triggers, whenever date changes                           -//
//-              OnViewChange                                                -//
//-                Triggers, when user switches between year-/monthview      -//
//-                                                                          -//
//- Not ready:   Font-behaviour not yet ok                                   -//
//-              Color attributes have some mistakes                         -//
//-              Weekend-functionality not yet implemented                   -//
//-              Min/Max properties not yet implemented                      -//
//-              Missing several customization like ownerdrawing, holiday-   -//
//-              highlighting, width and height of fixed cells and so on...  -//
//- Warranties:  Use it at your own risk                                     -//
//----------------------------------------------------------------------------//
unit QtCalendar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Grids;

const
  InitRepeatPause = 400;
  RepeatPause     = 100;

type

  TDayOfWeek = ( Sunday, Monday, Tuesday, Wednesday,
                  Thursday, Friday, Saturday );
  TTypeOfCell = ( dtHeading, dtWeekNo, dtTrailingday, dtDay, dtMonth );

  // Options for Calendar-Control
  TCalOption  = (coAllowViewChange,   // Change view style
                 coPerformTrailingDay,// Skip to next/prev month
                 coShowTrailingDays,  // Show trailing days
                 coShowHorzLines,     // Horizontal Lines
                 coShowHeading,       // Column-heading (not yet implemented)
                 coAcceptHoliday,     // Accept holidays (not yet implemented)
                 coShowFocusRect,     // Show Focus-Rect
                 coShowFrame,         // Border for control
                 coShowCellFrames,    // Show cellframes
                 coShowWeekNumbers,   // Show Weeknumbers
                 coShowNavButtons,    // Show Navigator-Buttons
                 coShowTitle);        // Show Title

  TCalendarOptions = set of TCalOption;

  TCalendarView = (cvMonthView, cvYearView);

  TDrawCalendarCellEvent = procedure (Sender: TObject; AText: string;
         Col, Row: Longint; Rect: TRect; State: TGridDrawState;
         AType: TTypeOfCell) of object;

  TCalendarGrid = class;
  TQtCalendarColors = class;
  TCalendarButton = class;

  TQtCalendar = class(TCustomPanel)
  private
    FToolPanel : TPanel;
    FGrid : TCalendarGrid;
    FTitleLabel : TCalendarButton;
    FBtnLeft : TCalendarButton;
    FBtnRight : TCalendarButton;
    FOptions: TCalendarOptions;
    FViewStyle: TCalendarView;
    FCalendarColors: TQtCalendarColors;
    FOnDrawCalendarCell: TDrawCalendarCellEvent;
    FOnViewChange: TNotifyEvent;
    FOnChange: TNotifyEvent;

    procedure SetCalendarColors(Value: TQtCalendarColors);
    procedure SetCalendarView(Value: TCalendarView);
    procedure SetOptions(Value: TCalendarOptions);
    procedure SetCaption4Year(Value: string);
    procedure SetTitleText;
    procedure SetColor(Index: Integer; Value: TColor);
    function GetOptions: TCalendarOptions;
    function GetDateElement(Index: Integer): Integer;
    function GetCalendarView: TCalendarView;

    procedure SetCalendarDate(Value: TDateTime);
    function GetCalendarDate: TDateTime;
    procedure SetStartOfWeek(Value: TDayOfWeek);
    function GetStartOfWeek : TDayOfWeek;

    function GetCaption4Year : string;

    procedure DoToolBtnsClick(Sender: TObject);
    procedure DoChange(Sender: TObject);
    procedure DoViewChange(Sender: TObject);
    procedure SetDateElement(Index: Integer; Value: Integer);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;

  protected
    procedure DoDrawCell(Sender: TObject; AText: string; ACol, ARow: Longint;
          ARect: TRect; AState: TGridDrawState; AType: TTypeOfCell);
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure NextMonth;
    procedure NextYear;
    procedure PrevMonth;
    procedure PrevYear;
    property CalendarDate: TDateTime read GetCalendarDate write SetCalendarDate;
         //stored StoreCalendarDate;

  published
    property Align;
    property BorderStyle;
    property Caption4Year : string read GetCaption4Year write SetCaption4Year;
    property Ctl3D;
    property Enabled;
    property Font;
    property ParentFont;
    property ShowHint;
    property StartOfWeek: TDayOfWeek read GetStartOfWeek write SetStartOfWeek;
    property Day: Integer index 3 read GetDateElement write SetDateElement stored False;
    property Month: Integer index 2 read GetDateElement write SetDateElement stored False;
    property Year: Integer index 1 read GetDateElement write SetDateElement stored False;
    property Colors: TQtCalendarColors read FCalendarColors write SetCalendarColors;
    property ViewStyle: TCalendarView read GetCalendarView write SetCalendarView
                default cvYearView;
    property Options: TCalendarOptions read GetOptions write SetOptions
      default [coShowTrailingDays, coShowHorzLines, coShowHeading,
               coAcceptHoliday, coShowFocusRect, {coShowCellFrames,}
               coShowWeekNumbers, coShowNavButtons, coShowTitle, coShowFrame];
    property OnViewChange: TNotifyEvent read FOnViewChange write FOnViewChange;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;


  end;

  TCalendarGrid = class(TCustomGrid)
  private
    FOwner : TQtCalendar;
    FStartOfWeek : TDayOfWeek;
    FDate: TDateTime;
    FCaption : string;
    FCalendarView : TCalendarView;
    FCalOptions: TCalendarOptions;
    FOnChange: TNotifyEvent;
    FOnViewChange: TNotifyEvent;
    FOnDrawCalendarCell: TDrawCalendarCellEvent;

    FUpdating: Boolean;
    FMonthOffset: Integer;
    FReadOnly: Boolean;
    FSkipMouseUp:Boolean;
    FUseCurrentDate : Boolean;

    FColWidthMonth : Integer;
    FRowHeightMonth : Integer;
    FColWidthMonthWeekNo : Integer;
    FColWidthYear : Integer;
    FRowHeightYear : Integer;

    function GetDateElement(Index: Integer): Integer;
    function GetDayNum(ACol, ARow: Integer): Integer;
    function GetCalendarWeek: Integer;
    function CalcCalendarWeek(ADate: TDateTime): Integer;
    procedure SetDateElement(Index: Integer; Value: Integer);
    procedure SetStartOfWeek(Value: TDayOfWeek);
    procedure SetCalOptions(Value: TCalendarOptions);
    procedure SetCalendarView(Value: TCalendarView);
    procedure SetCaption(Value: string);
    procedure SetCellDimensions;
    procedure SetCalendarDate(Value: TDateTime);
    procedure CalendarUpdate(DayOnly: Boolean);
    procedure ChangeMonth(Delta: Integer);
    function GetCellText(ACol, ARow: Integer): string;
    function StoreCalendarDate: Boolean;
    procedure SetUseCurrentDate(Value: Boolean);

  protected
    procedure Change; dynamic;
    procedure Click; override;
    procedure DblClick; override;
    procedure Paint; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    function DaysThisMonth: Integer; virtual;
    function DaysPerMonth(AYear, AMonth: Integer): Integer; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseUp(Button: TMouseButton;Shift: TShiftState; X, Y: Integer); override;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
    constructor Create( AOwner: TComponent ); override;
    procedure UpdateCalendar; virtual;
    function IsLeapYear(AYear: Integer): Boolean;
    function IsTrailingDay(ACol, ARow: Integer): Boolean;
    function IsCalendarWeek(ACol, ARow: Integer): Boolean;
    function GetCellType(ACol, ARow: Integer): TTypeOfCell;
    function IsToday(ACol, ARow: Integer): Boolean;
    function IsWeekend(ACol, ARow: Integer): Boolean;
    function GetDateFromCell(ACol, ARow: Integer): TDateTime;
    property CellText[ACol, ARow: Integer]: string read GetCellText;
    property CalendarDate: TDateTime  read FDate write SetCalendarDate stored StoreCalendarDate;
  published
    property Day: Integer index 3 read GetDateElement write SetDateElement stored False;
    property Month: Integer index 2 read GetDateElement write SetDateElement stored False;
    property Year: Integer index 1 read GetDateElement write SetDateElement stored False;
    property Caption4Year : string read FCaption write SetCaption;
    property Font;
    property ParentFont;
    property StartOfWeek: TDayOfWeek read FStartOfWeek write SetStartOfWeek;
    property CalOptions: TCalendarOptions read FCalOptions write SetCalOptions
      default [coShowTrailingDays, coShowHorzLines, coAcceptHoliday,
               coShowFocusRect, coShowCellFrames, coShowWeekNumbers,
               coPerformTrailingDay];
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDrawCalendarCell: TDrawCalendarCellEvent
              read FOnDrawCalendarCell write FOnDrawCalendarCell;
    property OnViewChange: TNotifyEvent read FOnViewChange write FOnViewChange;
    property WeekNumber: Integer read GetCalendarWeek;
    property CalendarView: TCalendarView read FCalendarView
                 write SetCalendarView default cvMonthView;

  end;

  // Should be redesigned from scratch
  TQtCalendarColors = class(TPersistent)
  private
    Owner: TQtCalendar;               // Owing component
                                      // Colors for ...
    FBackColor: TColor;               // Background
    FTextColor: TColor;               // Font
    FTitleBackColor: TColor;          // Background Title
    FTitleTextColor: TColor;          // Font Title
    FMonthBackColor: TColor;          // Background Month
    FTrailingTextColor: TColor;       // Font Trailingday
    FTodaysColor: TColor;             // Background Today
    FTodaysBorderColor: TColor;       // Framecolor Today
    FTodaysTextColor: TColor;         // Font Today
    FSelectedBorderColor: TColor;     // Framecolor selected cell
    FSelectedColor: TColor;           // Background selected cell
    FSelectedTextColor: TColor;       // Font selected cell
    FFlyByColor : TColor;             // Flyby

    procedure SetColor(Index: Integer; Value: TColor);
    procedure SetAllColors;
  public
    constructor Create(AOwner: TQtCalendar);
    procedure Assign(Source: TPersistent); override;
  published
    property BackColor: TColor index 0 read FBackColor
                        write SetColor default clWindow;
    property TextColor: TColor index 1 read FTextColor
                        write SetColor default clWindowText;
    property TitleBackColor: TColor index 2 read FTitleBackColor
                        write SetColor default clActiveCaption;
    property TitleTextColor: TColor index 3 read FTitleTextColor
                        write SetColor default clWhite;
    property MonthBackColor: TColor index 4 read FMonthBackColor
                        write SetColor default clWhite;
    property TrailingTextColor: TColor index 5 read FTrailingTextColor
                        write SetColor default clInactiveCaptionText;
    property TodaysColor: TColor index 6 read FTodaysColor write SetColor
                        default $00BF5F5F;
    property TodaysBorderColor: TColor  index 7 read FTodaysBorderColor
                        write SetColor default $00BF5F5F;
    property TodaysTextColor: TColor index 8 read FTodaysTextColor
                        write SetColor default clWhite;
    property SelectedBorderColor: TColor  index 9 read FSelectedBorderColor
                        write SetColor default clGray;
    property SelectedColor: TColor  index 10 read FSelectedColor
                        write SetColor default clGray;
    property SelectedTextColor: TColor  index 11 read FSelectedTextColor
                        write SetColor default clWhite;
    property FlyByColor: TColor  index 12 read FFlyByColor
                        write SetColor default clRed;

  end;

  TCalendarButton = class(TCustomLabel)
  private
    FFlyByColor : TColor;
    FEnabled : Boolean;
    FOrigColor : TColor;
    FVertAlign : TAlignment;
    FRepeatTimer : TTimer;
    FAllowTimer : Boolean;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure SetFlyByColor( Value: TColor); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property Caption;
    property OnClick;
    property Font;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property ParentFont;
    property AutoSize;
    property FlyByColor : TColor read FFlyByColor write SetFlyByColor
      default clHighLight;
    property FocusControl;
    property Layout;
    property AllowTimer : Boolean read FAllowTimer write FAllowTimer;
  end;


procedure Register;

implementation

const
   SAssignError = '%s íå ìîæåò áûòü  %s íàçíà÷åí';

//----------------------------------------------------------------------------//
//- C R E A T E                                                              -//
//----------------------------------------------------------------------------//
//- Info:        Creating Control                                            -//
//- Parameter:   (Standard)                                                  -//
//- Returns:     (none)                                                      -//
//----------------------------------------------------------------------------//
constructor TQtCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 158;
  Height := 170;
  Ctl3D := False;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  BorderStyle := bsNone;
  BorderWidth := 8;

  FCalendarColors := TQtCalendarColors.Create(Self);

  // whole Title, which keeps Navigato-Buttons and Title-label
  // Using Panel for aligning and resizing capabilities
  FToolPanel := TPanel.Create(Self);
  with TPanel(FToolPanel) do begin
    Parent := Self;
    Align := alTop;
    ParentColor := True;
    Caption := '';
    Ctl3D := False;
    BevelOuter := bvNone;
    BevelInner := bvNone;
    BorderStyle := bsNone;
    Height := 15;
    Visible := True;
  end;

  // This does the hard-work
  FGrid := TCalendarGrid.Create(Self);
  with TCalendarGrid(FGrid) do begin
    Parent := Self;
    Align := alClient;
    Caption4Year := 'Âûáåðèòå ìåñÿö';
    OnDrawCalendarCell := DoDrawCell;
    OnChange := DoChange;
    OnViewChange := DoViewChange;
  end;

  // Precious-Navigator-Button
  FBtnLeft := TCalendarButton.Create(Self);
  with TCalendarButton(FBtnLeft) do begin
    Parent := FToolPanel;
    Align := alLeft;
    ParentFont := False;
    FocusControl := FGrid;
    Font.Name := 'Wingdings 3';
    Font.Size := 9;
    Font.Color := Colors.TitleTextColor;
    Caption := #197;
    OnClick := DoToolBtnsClick;
  end;

  // Label of the Title, alowing to change the view-style
  FTitleLabel := TCalendarButton.Create(Self);
  with TCalendarButton(FTitleLabel) do begin
    Parent := FToolPanel;
    Caption := ' ';
    FocusControl := FGrid;
    Align := alClient;
    Font.Color := Colors.TitleTextColor;
    AllowTimer := False;
    OnClick := DoToolBtnsClick;
    Alignment := taCenter;
  end;

  // Next-Navigator-Button
  FBtnRight := TCalendarButton.Create(Self);
  with TCalendarButton(FBtnRight) do begin
    Parent := FToolPanel;
    Align := alRight;
    FocusControl := FGrid;
    ParentFont := False;
    Font.Name := 'Wingdings 3';
    Font.Size := 9;
    Font.Color := Colors.TitleTextColor;
    Caption := #198;
    OnClick := DoToolBtnsClick;
  end;

  // use Standard FGrid-Options and add some
//  FGrid.CalOptions := FGrid.CalOptions + [coShowFrame, coShowHeading,
//                      coShowNavButtons, coShowNavButtons, coShowTitle];
  Options := FGrid.CalOptions + [coShowFrame, coShowHeading,
                      coShowNavButtons, coShowNavButtons, coShowTitle];

  FCalendarColors.SetAllColors;
  DoChange(Self);

end;

//----------------------------------------------------------------------------//
//- D E S T R O Y                                                            -//
//----------------------------------------------------------------------------//
//- Info:        Destroy control and freeing memory                          -//
//- Parameter:   (Standard)                                                  -//
//- Returns:     (none)                                                      -//
//----------------------------------------------------------------------------//
destructor TQtCalendar.Destroy;
begin
  // free buttons before FToolpanel
  FBtnRight.Free;
  FBtnLeft.Free;
  FTitleLabel.Free;
  FToolPanel.Free;
  FGrid.Free;
  FCalendarColors.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------//
//- P A I N T                                                                -//
//----------------------------------------------------------------------------//
//- Info:        Just paint the frame if wished                              -//
//- Parameter:   (Standard)                                                  -//
//- Returns:     (none)                                                      -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.Paint;
var
  TheRect: TRect;
begin
  inherited Paint;

  // Just draw the frame, rest will be done by child-controls
  if (coShowFrame in Options) then begin
    TheRect := ClientRect;
    InflateRect(TheRect, -3, -3);
    Canvas.Brush.Color := FGrid.FixedColor;
    Canvas.FrameRect(TheRect);
  end;

end;

//----------------------------------------------------------------------------//
//- D O D R A W C E L L                                                      -//
//----------------------------------------------------------------------------//
//- Info:        Paint specific cells                                        -//
//- Parameter:   Sender     Childcontrol (FGrid)                             -//
//-              AText      Contents of cell                                 -//
//-              ACol, ARow Coordinates of Cell                              -//
//-              ARect      Bounding rectangle of cell                       -//
//-              AState     Status of cell (see TCustomGrid Info)            -//
//-              AType      Type of Cell (See GetCelltype)                   -//
//- Returns:     (none)                                                      -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.DoDrawCell(Sender: TObject; AText: string;
          ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState;
          AType: TTypeOfCell);
var
  TheRect: TRect;
  ABorderColor, AFontColor, ABackColor, AFrameColor : TColor;
begin

  // First Row in Year has to be painted by Paint-Method, not Drawcell
  if (ARow = 0) and (FViewStyle = cvYearView) then Exit;

  TheRect := ARect;

  ABackColor := FCalendarColors.BackColor;
  AFrameColor := ABackColor;
  ABorderColor := ABackColor;
  AFontColor := clWhite;

  if (AType = dtTrailingday) then
    AFontColor := FCalendarColors.TrailingTextColor;

  // Colors for the Fixed row and col
  if (gdFixed in AState) then begin
    ABackColor := FCalendarColors.TitleBackColor;
    AFrameColor := ABackColor;
    ABorderColor := FCalendarColors.TitleBackColor;
    AFontColor := FCalendarColors.TitleTextColor;
  end;

  // special treatment of todays-cell
  if FGrid.IsToday(ACol, ARow) then begin
    ABackColor := FCalendarColors.TodaysColor;
    AFontColor := FCalendarColors.TodaysTextColor;
    ABorderColor := FCalendarColors.TodaysBorderColor;
  end;

  if {(gdFocused in AState) or }(gdSelected in AState) then begin
    ABackColor := FCalendarColors.SelectedColor;
    AFontColor := FCalendarColors.TitleTextColor;
    ABorderColor := FCalendarColors.SelectedBorderColor;
  end;

  // setting font and do drawing
  FGrid.Canvas.Font := Font;
  with TheRect, FGrid.Canvas do begin

    // print the contents of cell
    Font.Color := AFontColor;
    Brush.Color := ABackColor;
    TextRect(TheRect, Left + (Right - Left - TextWidth(AText)) div 2,
      Top + (Bottom - Top - TextHeight(AText)) div 2, AText);

    // check for horz. lines
    if (coShowHorzLines in Options) then
      Inc(TheRect.Top);
    Brush.Style := bsSolid;
    if not (gdFixed in AState) then begin
      Brush.Color := AFrameColor;
      FrameRect(TheRect);
    end;

    // do we have the focus and should we show it
    if (gdFocused in AState) and (coShowFocusRect in Options) then
      DrawFocusRect(TheRect);

    // frame wished
    if (coShowCellFrames in Options) then begin
      InflateRect(TheRect, -1, -1);
      Brush.Color := ABorderColor;
      FrameRect(TheRect);
    end;
  end;

end;

//----------------------------------------------------------------------------//
//- S E T C A L E N D A R C O L O R S                                        -//
//----------------------------------------------------------------------------//
//- Info:        Setting Colorattributes                                     -//
//- Parameter:   Value    Value for new colors                               -//
//- Returns:     (none)                                                      -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.SetCalendarColors(Value: TQtCalendarColors);
begin
  if FCalendarColors <> Value then FCalendarColors.Assign(Value);
end;

//----------------------------------------------------------------------------//
//- S E T O P T I O N S                                                      -//
//----------------------------------------------------------------------------//
//- Info:        Setting Options for Calendar                                -//
//- Parameter:   Value    Value for Options                                  -//
//- Returns:     (none)                                                      -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.SetOptions(Value: TCalendarOptions);
begin
  // Just for information
  // Exclude(Value, co...);   // Nimmt eine Option heraus;
  // if [goColSizing, goRowSizing] * EffectiveOptions <> [] then...
  // if [goRangeSelect, goEditing] * Options = [goRangeSelect] then ...

  if FGrid.CalOptions <> Value then begin
    // Check specified Option and perform concerning action

    // Frame for the Calendarcontrol
    if ([coShowFrame] * Value) <> ([coShowFrame] * FGrid.CalOptions) then
       Invalidate;

    // Navigator-Buttons
    if ([coShowNavButtons] * Value) <> ([coShowNavButtons] * FGrid.CalOptions) then begin
      FBtnLeft.Visible := ([coShowNavButtons] * Value = [coShowNavButtons]);
      FBtnRight.Visible := ([coShowNavButtons] * Value = [coShowNavButtons]);
    end;

    // Visibility of title
    if ([coShowTitle] * Value) <> ([coShowTitle] * FGrid.CalOptions) then begin
      FToolPanel.Visible := ([coShowTitle] * Value = [coShowTitle]);
    end;

    // Allow-Titlelabel to switch between views
    if ([coAllowViewChange ] * Value) <> ([coAllowViewChange ] * FGrid.CalOptions) then begin
      FTitleLabel.Enabled := ([coAllowViewChange] * Value = [coAllowViewChange]);
    end;

    // Save Options in private FGrid-variable
    FGrid.CalOptions := Value;
  end;
end;

//----------------------------------------------------------------------------//
//- G E T O P T I O N S                                                      -//
//----------------------------------------------------------------------------//
//- Info:        Returning Options for Calendar                              -//
//- Parameter:   (none)                                                      -//
//- Returns:     (none)                                                      -//
//----------------------------------------------------------------------------//
function TQtCalendar.GetOptions: TCalendarOptions;
begin
  Result := FGrid.CalOptions;
end;

//----------------------------------------------------------------------------//
//- G E T C A L E N D A R V I E W                                            -//
//----------------------------------------------------------------------------//
//- Info:        Returning ViewStyle                                         -//
//- Parameter:   (none)                                                      -//
//- Returns:     cvMonthView or cvYearView                                   -//
//----------------------------------------------------------------------------//
function TQtCalendar.GetCalendarView: TCalendarView;
begin
  Result := FGrid.CalendarView;
end;

//----------------------------------------------------------------------------//
//- G E T C A P T I O N 4 Y E A R                                            -//
//----------------------------------------------------------------------------//
//- Info:        Returning caption                                           -//
//- Parameter:   (none)                                                      -//
//- Returns:     String for heading during YearView                          -//
//----------------------------------------------------------------------------//
function TQtCalendar.GetCaption4Year: string;
begin
  Result := FGrid.Caption4Year;
end;

//----------------------------------------------------------------------------//
//- S E T C A P T I O N 4 Y E A R                                            -//
//----------------------------------------------------------------------------//
//- Info:        Setting caption in yearview                                 -//
//- Parameter:   Value     String for heading during YearView                -//
//- Returns:     (none)                                                      -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.SetCaption4Year(Value: string);
begin
  if FGrid.Caption4Year <> Value then
    FGrid.Caption4Year := Value;
end;

//----------------------------------------------------------------------------//
//- S E T C A L E N D A R V I E W                                            -//
//----------------------------------------------------------------------------//
//- Info:        Setting viewstyle                                           -//
//- Parameter:   Value     Viewstlye                                         -//
//- Returns:     (none)                                                      -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.SetCalendarView(Value: TCalendarView);
begin
  If FGrid.CalendarView <> Value then
    FGrid.CalendarView := Value;
end;

//----------------------------------------------------------------------------//
//- S E T C O L O R                                                          -//
//----------------------------------------------------------------------------//
//- Info:        Setting specific color for the calendar-control             -//
//- Parameter:   Index     Index for the specific color                      -//
//-              Value     for color                                         -//
//- Returns:     (none)                                                      -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.SetColor(Index: Integer; Value: TColor);
begin
  case Index of
    0: Color := Value;
//    1: FTextColor         := Value;
    2: FGrid.FixedColor := Value;
//    3: FTitleTextColor    := Value;
//    4: FMonthBackColor    := Value;
//    5: FTrailingTextColor := Value;
//    6: FTodaysColor       := Value;
//    7: FTodaysBorderColor := Value;
//    8: FTodaysTextColor   := Value;
//    9: FSelectedBorderColor := Value;
//   10: FSelectedColor     := Value;
//   11: FSelectedTextColor := Value;
   12: begin
         FBtnRight.FlyByColor := Value;
         FTitleLabel.FlyByColor := Value;
         FBtnLeft.FlyByColor := Value;
       end;
  end;
end;

//----------------------------------------------------------------------------//
//- G E T D A T E E L E M E N T                                              -//
//----------------------------------------------------------------------------//
//- Info:        Returns dm or for the actual Date. Uses Variable of FGrid   -//
//- Parameter:   Index     Index for the date element                        -//
//- Returns:     dm or y                                                     -//
//----------------------------------------------------------------------------//
function TQtCalendar.GetDateElement(Index: Integer): Integer;
begin
  case Index of
    1: Result := FGrid.Year;
    2: Result := FGrid.Month;
    3: Result := FGrid.Day;
    else Result := -1;
  end;
end;

//----------------------------------------------------------------------------//
//- S E T D A T E E L E M E N T                                              -//
//----------------------------------------------------------------------------//
//- Info:        Setting dm or for the actual Date. Uses Variable of FGrid   -//
//- Parameter:   Index     Index for the date element                        -//
//- Returns:     (none)                                                      -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.SetDateElement(Index: Integer; Value: Integer);
begin
  case Index of
    1: FGrid.Year := Value;
    2: FGrid.Month := Value;
    3: FGrid.Day := Value;
    else
      Exit;
  end;
end;

//----------------------------------------------------------------------------//
//- P R E V M O N T H                                                        -//
//----------------------------------------------------------------------------//
//- Info:        Skip to previous month                                      -//
//- Parameter:   (none)                                                      -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.PrevMonth;
begin
  FGrid.ChangeMonth(-1);
end;

//----------------------------------------------------------------------------//
//- N E X T M O N T H                                                        -//
//----------------------------------------------------------------------------//
//- Info:        Skip to next month                                          -//
//- Parameter:   (none)                                                      -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.NextMonth;
begin
  FGrid.ChangeMonth(1);
end;

//----------------------------------------------------------------------------//
//- N E X T Y E A R                                                          -//
//----------------------------------------------------------------------------//
//- Info:        Skip to next year                                           -//
//- Parameter:   (none)                                                      -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.NextYear;
begin
  if IsLeapYear(Year) and (Month = 2) and (Day = 29) then Day := 28;
  Year := Year + 1;
end;

//----------------------------------------------------------------------------//
//- P R E V Y E A R                                                          -//
//----------------------------------------------------------------------------//
//- Info:        Skip to previous year                                       -//
//- Parameter:   (none)                                                      -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.PrevYear;
begin
  if IsLeapYear(Year) and (Month = 2) and (Day = 29) then Day := 28;
  Year := Year - 1;
end;


//----------------------------------------------------------------------------//
//- B T N L E F T C L I C K                                                  -//
//----------------------------------------------------------------------------//
//- Info:        Handling Click-Event of Navigator-Buttons and Titlelabel    -//
//- Parameter:   (Standard)                                                  -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.DoToolBtnsClick(Sender: TObject);
begin
  // Left navigator-Button
  if (Sender = FBtnLeft) then begin
    if FGrid.CalendarView = cvMonthView then
      PrevMonth
    else
      PrevYear;
    Exit;
  end;

  // Right navigator-Button
  if (Sender = FBtnRight) then begin
    if FGrid.CalendarView = cvMonthView then
      NextMonth
    else
      NextYear;
    Exit;
  end;

  // Title-Label
  if (Sender = FTitleLabel) and (coAllowViewChange in Options) then begin
    if FGrid.CalendarView = cvMonthView then
      FGrid.CalendarView := cvYearView
    else
      FGrid.CalendarView := cvMonthView;
    Exit;
  end;

end;

//----------------------------------------------------------------------------//
//- D O C H A N G E                                                          -//
//----------------------------------------------------------------------------//
//- Info:        Handling Change-Event of FGrid                              -//
//- Parameter:   (Standard)                                                  -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.DoChange(Sender: TObject);
begin
  SetTitleText;
  if Assigned(FOnChange) then FOnChange(Self);
end;

//----------------------------------------------------------------------------//
//- D O V I E W C H A N G E                                                  -//
//----------------------------------------------------------------------------//
//- Info:        Handling ViewChange-Event of FGrid                          -//
//- Parameter:   (Standard)                                                  -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.DoViewChange(Sender: TObject);
begin
  SetTitleText;
  if Assigned(FOnViewChange) then FOnViewChange(Self);
end;

//----------------------------------------------------------------------------//
//- C M F O N T C H A N G E D                                                -//
//----------------------------------------------------------------------------//
//- Info:        Handling Event if font changes                              -//
//- Parameter:   (Standard)                                                  -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.CMFontChanged(var Message: TMessage);
begin
//  if FInplaceEdit <> nil then FInplaceEdit.Font := Font;
  inherited;
  FGrid.Font := Font;
  FTitleLabel.Font := Font;
  FTitleLabel.Font.Color := Colors.TitleTextColor;
  FBtnLeft.Font.Color := Colors.TitleTextColor;
  FBtnRight.Font.Color := Colors.TitleTextColor;
end;

//----------------------------------------------------------------------------//
//- S E T T I T L E T E X T                                                  -//
//----------------------------------------------------------------------------//
//- Info:        Sets the Caption, depending in Viewstyle                    -//
//- Parameter:   (Standard)                                                  -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.SetTitleText;
var
  TheText: string;
begin
  if FGrid.CalendarView = cvMonthView then
    TheText := LongMonthNames[FGrid.Month] + ' ' + IntToStr(FGrid.Year)
  else
    TheText := IntToStr(FGrid.Year);

  if TheText <> FTitleLabel.Caption then
    FTitleLabel.Caption := TheText;
end;

//----------------------------------------------------------------------------//
//- S E T C A L E N D A R D A T E                                            -//
//----------------------------------------------------------------------------//
//- Info:        Sets date-property for control                              -//
//- Parameter:   Value    date to set                                        -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.SetCalendarDate(Value: TDateTime);
begin
  FGrid.SetCalendarDate(Value);
end;

//----------------------------------------------------------------------------//
//- G E T C A L E N D A R D A T E                                            -//
//----------------------------------------------------------------------------//
//- Info:        Gets the actual date                                        -//
//- Parameter:   (none)                                                      -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
function TQtCalendar.GetCalendarDate: TDateTime;
begin
  Result := FGrid.CalendarDate;
end;

//----------------------------------------------------------------------------//
//- S E T S T A R T O F W E E K                                              -//
//----------------------------------------------------------------------------//
//- Info:        Sets the start of week                                      -//
//- Parameter:   (none)                                                      -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TQtCalendar.SetStartOfWeek(Value: TDayOfWeek);
begin
  FGrid.SetStartOfWeek(Value);
end;

//----------------------------------------------------------------------------//
//- G E T S T A R T O F W E E K                                              -//
//----------------------------------------------------------------------------//
//- Info:        Gets the start of week                                      -//
//- Parameter:   (none)                                                      -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
function TQtCalendar.GetStartOfWeek : TDayOfWeek;
begin
  Result := FGrid.StartOfWeek;
end;

//----------------------------------------------------------------------------//
//- C R E A T E                                                              -//
//----------------------------------------------------------------------------//
//- Info:        Creating Kalendargrid                                       -//
//- Parameter:   (Standard)                                                  -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
constructor TCalendarGrid.Create( AOwner: TComponent );
var
  SourceName: string;
begin
  inherited Create(AOwner);

  // Check Owner and raise error if necessary
  FOwner := TQtCalendar(AOwner);
  if FOwner = nil then SourceName := 'nil'
  else SourceName := FOwner.ClassName;
  if (FOwner = nil) or not (FOwner is TQtCalendar) then
     raise EConvertError.CreateFmt(SAssignError, [SourceName, ClassName]);

  // setting defaults
  Options := Options - [goRangeSelect] + [goDrawFocusSelected]
                     - [goVertLine ] - [goHorzLine]
                     - [goFixedVertLine] - [goFixedHorzLine];
  Ctl3D := False;
  BorderStyle := bsNone;
  ScrollBars := ssNone;
  GridLineWidth := 0;
  ParentColor := True;
//  FStartOfWeek := Integer(Monday);
  FixedColor := clGray;
//  ParentFont := True;
  FDate := Date;

  FCalOptions := [coShowTrailingDays, coShowHorzLines, coAcceptHoliday,
               coShowFocusRect, coShowCellFrames, coShowWeekNumbers,
               coPerformTrailingDay, coAllowViewChange];

  DefaultDrawing := False;
  FCalendarView := cvYearView;

  UpdateCalendar;
end;

//----------------------------------------------------------------------------//
//- C H A N G E                                                              -//
//----------------------------------------------------------------------------//
//- Info:        Implements a Change Event if date changes                   -//
//- Parameter:   (none)                                                      -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

//----------------------------------------------------------------------------//
//- C L I C K                                                                -//
//----------------------------------------------------------------------------//
//- Info:        Handling Click Event and sets date-property accordingly     -//
//- Parameter:   (Standard)                                                  -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.Click;
var
  TheCellText: string;
  AMonth : Integer;
begin
  inherited Click;
  TheCellText := CellText[Col, Row];
  if (FCalendarView = cvMonthView) then begin
    if TheCellText <> '' then begin
      Day := StrToInt(TheCellText);
    end;
  end
  else begin
    AMonth := (Row - 1) * 4 + Col + 1;
    if Day > DaysPerMonth(Year, AMonth) then
      Day := DaysPerMonth(Year, AMonth);
    Month := AMonth;
  end;
end;

//----------------------------------------------------------------------------//
//- D B L C L I C K                                                          -//
//----------------------------------------------------------------------------//
//- Info:        Handling Dblclick Event, if yearview is active              -//
//- Parameter:   (Standard)                                                  -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.DblClick;
begin
  inherited DblClick;
  if (FCalendarView = cvYearView) and (coAllowViewChange in FCalOptions) then
  begin
    FSkipMouseUp := True;         // avoid jumping arround
    Month := (Row - 1) * 4 + Col + 1;
    CalendarView := cvMonthView;
  end;
end;

//----------------------------------------------------------------------------//
//- K E Y D O W N                                                            -//
//----------------------------------------------------------------------------//
//- Info:        Handling Keys for navigation in the calendar                -//
//- Parameter:   (Standard)                                                  -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
    if FCalendarView = cvMonthView then begin
      case Key of
        VK_LEFT, VK_SUBTRACT:
          begin
            if (Day > 1) then
              Day := Day - 1
            else
              CalendarDate := CalendarDate - 1;
            Exit;
          end;
        VK_RIGHT, VK_ADD:
          begin
            if (Day < DaysThisMonth) then
              Day := Day + 1
            else
              CalendarDate := CalendarDate + 1;
            Exit;
          end;
      end;
    end
    else begin
      case Key of
        VK_LEFT, VK_SUBTRACT:
          begin
            ChangeMonth(-1);
            Exit;
          end;
        VK_RIGHT, VK_ADD:
          begin
            ChangeMonth(1);
            Exit;
          end;
      end;
    end;
  inherited KeyDown(Key, Shift);
end;

//----------------------------------------------------------------------------//
//- M O U S E U P                                                            -//
//----------------------------------------------------------------------------//
//- Info:        Handling MouseUp-event for Navigation by Trailing-Day       -//
//- Parameter:   (Standard)                                                  -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.MouseUp(Button: TMouseButton;
                  Shift: TShiftState; X, Y: Integer);
var
  ACoord : TGridCoord;
begin
  inherited MouseUp(Button, Shift, X, Y);

  if (Button = mbLeft) and (coShowTrailingDays in FCalOptions) then
  begin
    ACoord := MouseCoord(X, Y);
    if IsTrailingDay(ACoord.X, ACoord.Y) and (ACoord.Y > 0) and
        (FSkipMouseUp = False) and (coPerformTrailingDay in CalOptions) then
    begin
      FDate := GetDateFromCell(ACoord.X, ACoord.Y);
      UpdateCalendar;
      Change;
    end;
  end;
  FSkipMouseUp := False;
end;

//----------------------------------------------------------------------------//
//- W M S I Z E                                                              -//
//----------------------------------------------------------------------------//
//- Info:        Setting Heights and Widths for Rows and Cols                -//
//- Parameter:   (Standard)                                                  -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.WMSize(var Message: TWMSize);
var
  GridLines: Integer;
begin
  // colwidth and rowheight in monthview
  GridLines := 6 * GridLineWidth;
  FColWidthMonth := (Message.Width - GridLines) div 7;
  FRowHeightMonth := (Message.Height - GridLines) div 7;

  // colwidth and rowheight in monthview with visible weekno.
  GridLines := 7 * GridLineWidth;
  FColWidthMonthWeekNo := (Message.Width - GridLines) div 8;

  // colwidth and rowheight in yearview
  GridLines := 3 * GridLineWidth;
  FColWidthYear := (Message.Width - GridLines) div 4;
  FRowHeightYear := (Message.Height - GridLines - FRowHeightMonth) div 3;

  // setup properties 
  SetCellDimensions;

end;

//----------------------------------------------------------------------------//
//- P A I N T                                                                -//
//----------------------------------------------------------------------------//
//- Info:        Paints only Heading in Yearview and horizontal lines        -//
//- Parameter:   (none)                                                      -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.Paint;
var
  TheRect : TRect;
  I, Y : Integer;
begin
  inherited Paint;
  if (FCalendarView = cvYearView) then begin

    // paint title in yearview
    TheRect := BoxRect(0, 0, 3, 0);
    with TheRect, Canvas do begin
      Brush.Color := FixedColor;
//      Font.Color := FCalendarColors.TitleTextColor;
      TextRect(TheRect, Left + (Right - Left - TextWidth(FCaption)) div 2,
        Top + (Bottom - Top - TextHeight(FCaption)) div 2, FCaption);

    end;
  end;

  if (coShowHorzLines in CalOptions) then begin
    // paint horizontal lines
    with Canvas do begin
      Pen.Style := psSolid;
      Pen.Width := 1;
      Pen.Color := FixedColor;
      Y := 0;
      for I := 0 to RowCount - 2 do begin
        Y := Y + RowHeights[I];
        MoveTo(0, Y);
        LineTo(GridWidth, Y);
      end;
    end;
  end;

end;


//----------------------------------------------------------------------------//
//- D R A W C E L L                                                          -//
//----------------------------------------------------------------------------//
//- Info:        Draws without styles the contents of cells. By default this -//
//-              is done by QtCalendar                                       -//
//- Parameter:   ACol, ARow  Coordinates of cell                             -//
//-              ARect       Rectangle of cell                               -//
//-              AState      State of cell                                   -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  AText : string;
begin
  AText := CellText[ACol, ARow];

  if Assigned(FOnDrawCalendarCell) then begin
     FOnDrawCalendarCell(Self, AText, ACol, ARow, ARect, AState, GetCellType(ACol, ARow));
     Exit;
   end;

  with ARect, Canvas do begin
    TextRect(ARect, Left + (Right - Left - TextWidth(AText)) div 2,
      Top + (Bottom - Top - TextHeight(AText)) div 2, AText);
  end;

end;

//----------------------------------------------------------------------------//
//- S E L E C T C E L L                                                      -//
//----------------------------------------------------------------------------//
//- Info:        Checks, if specified cell is selectable                     -//
//- Parameter:   (Standard)                                                  -//
//- Returns:     True, if selection is ok, otherwise false                   -//
//----------------------------------------------------------------------------//
function TCalendarGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  if ((not FUpdating) and FReadOnly) or (CellText[ACol, ARow] = '')
       or (IsTrailingDay(ACol, ARow)) then
    Result := False
  else
    Result := inherited SelectCell(ACol, ARow);
end;

//----------------------------------------------------------------------------//
//- C A L C C A L E N D A R W E E K                                          -//
//----------------------------------------------------------------------------//
//- Info:        Calculates the weekno. for a specific date                  -//
//- Parameter:   ADate    date                                               -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
function TCalendarGrid.CalcCalendarWeek(ADate: TDateTime): Integer;
var
  AYear, AMonth, ADay: Word;
  //TheYear : Word;      // week, to which the week belongs
  AWeekDay : Word;     // Day of week for 1. Jan
  ANumDays : Word;     // Days since 1. Jan
  AFirstDayOfYear : TDateTime;  // Date of 1. Jn
begin
  try
    DecodeDate(ADate, AYear, AMonth, ADay);
    //TheYear := AYear;
    AFirstDayOfYear := EncodeDate(AYear, 1, 1);
    AWeekDay := DayOfWeek(AFirstDayOfYear);
    ANumDays := Trunc(Int(ADate) - AFirstDayOfYear) +
                (7 - DayOfWeek(ADate - 1)) +
                (7 * Ord(AWeekDay in[2..5]));
    Result := ANumDays div 7;
    if Result = 0 then begin
      if(DayOfWeek(EncodeDate(AYear - 1, 1, 1)) > 5) or
        (DayOfWeek(EncodeDate(AYear - 1, 12, 31)) < 5) then
        Result := 52
      else
        Result := 53;
      //TheYear := AYear - 1;
    end
    else if Result = 53 then
      if (AWeekDay > 5) or (DayOfWeek(EncodeDate(AYear, 12, 31)) < 5) then
      begin
        Result := 1;
        //TheYear := AYear + 1;
      end;
  except
    Result := 0;
  end;
end;

//----------------------------------------------------------------------------//
//- I S L E A P Y E A R                                                      -//
//----------------------------------------------------------------------------//
//- Info:        Checks, if specified Year is a leap year                    -//
//- Parameter:   AYear     Year to check                                     -//
//- Returns:     True for leapyear, otherwise false                          -//
//----------------------------------------------------------------------------//
function TCalendarGrid.IsLeapYear(AYear: Integer): Boolean;
begin
  Result := (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0));
end;

//----------------------------------------------------------------------------//
//- D A Y S P E R M O N T H                                                  -//
//----------------------------------------------------------------------------//
//- Info:        Return number of days for a specified month and year        -//
//- Parameter:   AYear, AMonth selfexplaining                                -//
//- Returns:     Number of days for the month                                -//
//----------------------------------------------------------------------------//
function TCalendarGrid.DaysPerMonth(AYear, AMonth: Integer): Integer;
const
  DaysInMonth: array[1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result := DaysInMonth[AMonth];
  // recognize leapyear
  if (AMonth = 2) and IsLeapYear(AYear) then Inc(Result);
end;

//----------------------------------------------------------------------------//
//- D A Y S T H I S M O N T H                                                -//
//----------------------------------------------------------------------------//
//- Info:        Return number of days for actual month                      -//
//- Parameter:   (none)                                                      -//
//- Returns:     Number of days for actual month                             -//
//----------------------------------------------------------------------------//
function TCalendarGrid.DaysThisMonth: Integer;
begin
  Result := DaysPerMonth(Year, Month);
end;

//----------------------------------------------------------------------------//
//- C H A N G E M O N T H                                                    -//
//----------------------------------------------------------------------------//
//- Info:        Changes Month by Delta                                      -//
//- Parameter:   Delta   Number of month to jump                             -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.ChangeMonth(Delta: Integer);
var
  AYear, AMonth, ADay: Word;
  NewDate: TDateTime;
  CurDay: Integer;
begin
  // Given a value of 1 or -1, moves to Next or Prev month accordingly
  DecodeDate(FDate, AYear, AMonth, ADay);
  CurDay := ADay;
  if Delta > 0 then
    ADay := DaysPerMonth(AYear, AMonth)
  else
    ADay := 1;
  NewDate := EncodeDate(AYear, AMonth, ADay);
  NewDate := NewDate + Delta;
  DecodeDate(NewDate, AYear, AMonth, ADay);
  if DaysPerMonth(AYear, AMonth) > CurDay then
    ADay := CurDay
  else
    ADay := DaysPerMonth(AYear, AMonth);
  CalendarDate := EncodeDate(AYear, AMonth, ADay);
end;

//----------------------------------------------------------------------------//
//- G E T C A L E N D A R W E E K                                            -//
//----------------------------------------------------------------------------//
//- Info:        Returns Weekno. for actual date                             -//
//- Parameter:   (none)                                                      -//
//- Returns:     Calendar-Week                                               -//
//----------------------------------------------------------------------------//
function TCalendarGrid.GetCalendarWeek: Integer;
begin
  Result := CalcCalendarWeek(FDate);
end;

//----------------------------------------------------------------------------//
//- I S T R A I L I N G D A Y                                                -//
//----------------------------------------------------------------------------//
//- Info:        Checks if specified cell is a trailing-day                  -//
//- Parameter:   ACol, ARow  Coordinates of cell                             -//
//- Returns:     True for trailingday, otherwise false                       -//
//----------------------------------------------------------------------------//
function TCalendarGrid.IsTrailingDay(ACol, ARow: Integer): Boolean;
begin
  Result := GetCellType(ACol, ARow) = dtTrailingday;
end;

//----------------------------------------------------------------------------//
//- I S T O D A Y                                                            -//
//----------------------------------------------------------------------------//
//- Info:        Checks if specific cell represents today                    -//
//-              stellt                                                      -//
//- Parameter:   ACol, ARow  Coordinates of cell                             -//
//- Returns:     True for today, otherwise false                             -//
//----------------------------------------------------------------------------//
function TCalendarGrid.IsToday(ACol, ARow: Integer): Boolean;
begin
  Result := False;
  if (GetCellType(ACol, ARow) = dtDay) then
    Result := (GetDateFromCell(ACol, ARow) = SysUtils.Date);
end;

//----------------------------------------------------------------------------//
//- I S C A L E N D A R W E E K                                              -//
//----------------------------------------------------------------------------//
//- Info:        Checks if specific cell represents calendarweek             -//
//- Parameter:   ACol, ARow  Coordinates of cell                             -//
//- Returns:     True for Weekno., otherwise false                           -//
//----------------------------------------------------------------------------//
function TCalendarGrid.IsCalendarWeek(ACol, ARow: Integer): Boolean;
begin
  Result := GetCellType(ACol, ARow) = dtWeekNo;
end;

//----------------------------------------------------------------------------//
//- I S W E E K E N D                                                        -//
//----------------------------------------------------------------------------//
//- Info:        Checks if specific cell represents a weekend-cell           -//
//- Parameter:   ACol, ARow  Coordinates of cell                             -//
//- Returns:     True for weekend-cell, otherwise false                      -//
//----------------------------------------------------------------------------//
function TCalendarGrid.IsWeekend(ACol, ARow: Integer): Boolean;
begin
  Result := False;
end;


//----------------------------------------------------------------------------//
//- G E T D A T E E L E M E N T                                              -//
//----------------------------------------------------------------------------//
//- Info:        Returns, depending on index, a dateelement                  -//
//- Parameter:   ACol, ARow  Coordinates of cell                             -//
//- Returns:     Value for the specific dateelement                          -//
//----------------------------------------------------------------------------//
function TCalendarGrid.GetDateElement(Index: Integer): Integer;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  case Index of
    1: Result := AYear;
    2: Result := AMonth;
    3: Result := ADay;
    else Result := -1;
  end;
end;

//----------------------------------------------------------------------------//
//- G E T C E L L T E X T                                                    -//
//----------------------------------------------------------------------------//
//- Info:        Returns the contents of a specific cell                     -//
//- Parameter:   ACol, ARow  Coordinates of cell                             -//
//- Returns:     String for the contents of the cell                         -//
//----------------------------------------------------------------------------//
function TCalendarGrid.GetCellText(ACol, ARow: Integer): string;
var
  CellType : TTypeOfCell;
  AYear, AMonth, ADay: Word;
begin
  Result := '';                                 // assume empty string

  CellType := GetCellType(ACol, ARow);
  case CellType of

  dtHeading:
    begin
      if FCalendarView = cvMonthView then begin
        if (coShowWeekNumbers in CalOptions) then begin
          if ACol > 0 then
            Result := ShortDayNames[(Integer(StartOfWeek) + ACol - 1) mod 7 + 1];
        end
        else
          Result := ShortDayNames[(Integer(StartOfWeek) + ACol) mod 7 + 1];
      end;
    end;

  dtMonth:
    begin    //formats monthnames
      Result := LongMonthNames[ACol+(ARow-1) * 4 + 1];
      if Length(Result) > 4 then
        Result := ShortMonthNames[ACol+(ARow-1) * 4 + 1] + '.';
    end;

  dtWeekNo:
    Result := IntToStr(CalcCalendarWeek(GetDateFromCell(1, ARow)));

  dtTrailingday, dtDay:
    begin
      DecodeDate(GetDateFromCell(ACol, ARow), AYear, AMonth, ADay);
      if (CellType = dtDay) or (coShowTrailingDays in CalOptions) then
        Result := IntToStr(ADay);
    end;

  else
  end;
end;

//----------------------------------------------------------------------------//
//- G E T C E L L T Y P E                                                    -//
//----------------------------------------------------------------------------//
//- Info:        Defines the type of cell                                    -//
//- Parameter:   ACol, ARow  Coordinates of cell                             -//
//- Returns:     Code for cell (TTypeOfCell)                                 -//
//----------------------------------------------------------------------------//
function TCalendarGrid.GetCellType(ACol, ARow: Integer): TTypeOfCell;
var
  DayNum : Integer;
begin
  if ARow = 0 then                                       // title, easy to check
    Result := dtHeading
  else begin
    if FCalendarView = cvYearView then //during yearview, every line under title
      Result := dtMonth                      // is a month; during monthview the
    else begin                      // first col maybe represents an weekno.alte
      if (ACol = 0) and (coShowWeekNumbers in CalOptions) then        // je nach
        Result := dtWeekNo                          // Optionen die Wochennummer
      else begin
        DayNum := GetDayNum(ACol, ARow);           // Zuletzt prüfen, ob Tag zum
        if (DayNum < 1) or (DayNum > DaysThisMonth) then
          Result := dtTrailingday                     // angrenzenden Monat oder
        else
          Result := dtDay;                             // aktuellen Monat gehört
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------//
//- G E T D A Y N U M                                                        -//
//----------------------------------------------------------------------------//
//- Info:        Returns Day-Number for a specific cell                      -//
//- Parameter:   ACol, ARow  Coordinates of cell                             -//
//- Returns:     daynum for specific cell                                    -//
//----------------------------------------------------------------------------//
function TCalendarGrid.GetDayNum(ACol, ARow: Integer): Integer;
var
  TheCol : Integer;
begin

  // Col-adjustment, if weeknumbers are shown
  if (coShowWeekNumbers in CalOptions) then
    TheCol := ACol - 1
  else
    TheCol := ACol;

  // Move first row to second, if upper-left cell is valid day of actual month,
  // in other words, always keep leading cells for trailing days
  if FMonthOffset = 1 then
    Result := FMonthOffset + TheCol + (ARow - 2) * 7
  else
    Result := FMonthOffset + TheCol + (ARow - 1) * 7;
end;

//----------------------------------------------------------------------------//
//- G E T D A T E F R O M C E L L                                            -//
//----------------------------------------------------------------------------//
//- Info:        Returns date for a specific cell                            -//
//- Parameter:   ACol, ARow  Coordinates of cell                             -//
//- Returns:     date                                                        -//
//----------------------------------------------------------------------------//
function TCalendarGrid.GetDateFromCell(ACol, ARow: Integer): TDateTime;
var
  DayNum : Integer;
  CellType: TTypeOfCell;
  AYear, AMonth, ADay: Word;
begin
  CellType := GetCellType(ACol, ARow);
  DecodeDate(FDate, AYear, AMonth, ADay);
  DayNum := GetDayNum(ACol, ARow);

  if (CellType = dtDay) then
     ADay := DayNum;

  if (CellType = dtTrailingDay) then begin
    if DayNum > 0 then begin
      ADay := DayNum - DaysPerMonth(AYear, AMonth);
      Inc(AMonth);
      if AMonth > 12 then begin
        AMonth := 1;
        Inc(AYear);
      end;
    end
    else begin
      Dec(AMonth);
      if AMonth < 1 then begin
        Dec(AYear);
        AMonth := 12;
      end;
      ADay := DaysPerMonth(AYear, AMonth) + DayNum;
    end;
  end;

  Result := EncodeDate(AYear, AMonth, ADay);
end;


//----------------------------------------------------------------------------//
//- S E T D A T E E L E M E N T                                              -//
//----------------------------------------------------------------------------//
//- Info:        Setting dm or for the actual Date.                          -//
//- Parameter:   Index     Index for the date element                        -//
//- Returns:     (none)                                                      -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.SetDateElement(Index: Integer; Value: Integer);
var
  AYear, AMonth, ADay: Word;
begin
  if Value > 0 then
  begin
    DecodeDate(FDate, AYear, AMonth, ADay);
    case Index of
      1: if AYear <> Value then
                   AYear := Value else Exit;
      2: if (Value <= 12) and (Value <> AMonth) then
                   AMonth := Value else Exit;
      3: if (Value <= DaysThisMonth) and (Value <> ADay) then
                   ADay := Value else Exit;
      else Exit;
    end;
    FDate := EncodeDate(AYear, AMonth, ADay);
    FUseCurrentDate := False;
    CalendarUpdate((Index = 3) or ((Index = 2) and (FCalendarView = cvYearView)));
    Change;
  end;
end;

//----------------------------------------------------------------------------//
//- S E T S T A R T O F W E E K                                              -//
//----------------------------------------------------------------------------//
//- Info:        Sets beginning of week                                      -//
//- Parameter:   Value    0-6 for weekday                                    -//
//- Return:      (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.SetStartOfWeek(Value: TDayOfWeek);
begin
  if Value <> FStartOfWeek then
  begin
    FStartOfWeek := Value;
    UpdateCalendar;
  end;
end;

//----------------------------------------------------------------------------//
//- S T O R E C A L E N D A R D A T E                                        -//
//----------------------------------------------------------------------------//
//- Info:        Prüft, ob das aktuelle Datum als das ausgewählte Datum an-  -//
//-              genommen wird                                               -//
//- Parameter:   (none)                                                      -//
//- Returns:     True, für Modus aktuelles Datum, ansonsten False            -//
//----------------------------------------------------------------------------//
function TCalendarGrid.StoreCalendarDate: Boolean;
begin
  Result := not FUseCurrentDate;
end;

//----------------------------------------------------------------------------//
//- U P D A T E C A L E N D A R                                              -//
//----------------------------------------------------------------------------//
//- Info:        Initiert eine komplette Aktualisierung des Kalendars        -//
//- Parameter:   (none)                                                      -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.UpdateCalendar;
begin
  CalendarUpdate(False);
end;

//----------------------------------------------------------------------------//
//- C A L E N D A R U P D A T E                                              -//
//----------------------------------------------------------------------------//
//- Info:        Führt eine Aktualisierung des Kalenders aus                 -//
//- Parameter:   DayOnly  Wenn True, dann ist kein komplettes Neuzeichnen    -//
//-                       erfoderlich, andernfalls False                     -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.CalendarUpdate(DayOnly: Boolean);
var
  AYear, AMonth, ADay: Word;
  ACol, ARow : Integer;
  FirstDate: TDateTime;
begin
  FUpdating := True;
  try
    DecodeDate(FDate, AYear, AMonth, ADay);
    FirstDate := EncodeDate(AYear, AMonth, 1);
    // Monatstag für den ersten des Monats
    FMonthOffset := 2 - ((DayOfWeek(FirstDate)
                          - Integer(StartOfWeek) + 7) mod 7);
    if FMonthOffset = 2 then FMonthOffset := -5;

    // Monatsansicht: Unterscheide, ob die erste Zeile um eine
    // (Siehe auch GetCellText-Funktion) nach unten verschoben wird.
    if FCalendarView = cvMonthView then begin
      ACol := (ADay - FMonthOffset) mod 7;
      ARow := (ADay - FMonthOffset) div 7 + 1;
      if FMonthOffset = 1 then
        Inc(ARow);
      if (coShowWeekNumbers in CalOptions) then
        Inc(ACol);
      MoveColRow(ACol, ARow, False, False);
    end else
      MoveColRow(((AMonth - 1) mod 4), ((AMonth - 1) div 4) + 1 , False, False);

    if DayOnly then Update else Invalidate;

  finally
    FUpdating := False;
  end;
end;

//----------------------------------------------------------------------------//
//- S E T C A L O P T I O N S                                                -//
//----------------------------------------------------------------------------//
//- Info:        Legt die Optionen des kalendercontrols fest und initiert    -//
//-              notwendige Anpassungen                                      -//
//- Parameter:   Value    Optionen für das Control                           -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.SetCalOptions(Value: TCalendarOptions);
begin
  // Exclude(Value, co...);   // Nimmt eine Option heraus;

  if FCalOptions <> Value then
  begin
    FCalOptions := Value;
    SetCellDimensions;
    Invalidate;
    Updatecalendar;
  end;

end;

//----------------------------------------------------------------------------//
//- S E T C A L E N D A R D A T E                                            -//
//----------------------------------------------------------------------------//
//- Info:        Setzt das gesamte aktuelle Datum für den Kalender           -//
//- Parameter:   Value    zu setzendes Datum                                 -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.SetCalendarDate(Value: TDateTime);
var
  AYear, AMonth, ADay, VYear, VMonth, VDay: Word;
  AUpdateFlag : Boolean;
begin
  if Value <> FDate then begin
    DecodeDate(FDate, AYear, AMonth, ADay);
    DecodeDate(Value, VYear, VMonth, VDay);
    FDate := Value;
    AUpDateFlag := ((AYear = VYear) and (AMonth = VMonth)
                                    and (FCalendarView = cvMonthView));
    CalendarUpdate(AUpdateFlag);
    Change;
  end;
end;

//----------------------------------------------------------------------------//
//- S E T U S E C U R R E N T D A T E                                        -//
//----------------------------------------------------------------------------//
//- Info:        Bestimmt, ob das aktuelle Datum verwendet werden soll       -//
//- Parameter:   Value    True, für aktuelles Datum, ansonsten False         -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.SetUseCurrentDate(Value: Boolean);
begin
  if Value <> FUseCurrentDate then
  begin
    FUseCurrentDate := Value;
    if Value then
    begin
      FDate := Date; // aktuelles Datum nehmen und dann ...
      UpdateCalendar;
    end;
  end;
end;

//----------------------------------------------------------------------------//
//- S E T C A P T I O N                                                      -//
//----------------------------------------------------------------------------//
//- Info:        Setzt den Titelstring für die Jahresansicht fest            -//
//- Parameter:   Value    Inhalt der Titelzeile in der Jahresansicht         -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.SetCaption(Value: string);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    if FCalendarView = cvYearView then InvalidateRow(0);
  end;
end;

//----------------------------------------------------------------------------//
//- S E T C A L V I E W                                                      -//
//----------------------------------------------------------------------------//
//- Info:        Setzt den Modus (Jahres-/Monatsansicht) für das Control und -//
//-              aktualisiert das Control                                    -//
//- Parameter:   Value    Kalendaransicht                                    -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.SetCalendarView(Value: TCalendarView);
begin
 if FCalendarView <> Value then begin
   FCalendarView := Value;
   SetCellDimensions;
   UpdateCalendar;
   if Assigned(FOnViewChange) then
     FOnViewChange(Self);
 end;
end;

//----------------------------------------------------------------------------//
//- S E T C E L L D I M E N S I O N S                                        -//
//----------------------------------------------------------------------------//
//- Info:        Setzt je nach Modus und Optionen, die Zahl der Zeilen und   -//
//-              Spalten und deren Höhe bzw. Breite                          -//
//- Parameter:   (none)                                                      -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarGrid.SetCellDimensions;
begin
  if (FCalendarView = cvMonthView) then begin
    RowCount := 7;
    DefaultRowHeight := FRowHeightMonth;
    if (coShowWeekNumbers in CalOptions) then begin
      FixedCols := 1;
      ColCount := 8;
      DefaultColWidth := FColWidthMonthWeekNo;
      ColWidths[0] := ClientWidth - (7 * GridLineWidth)
                                   - (7 * FColWidthMonthWeekNo);
    end
    else begin
      FixedCols := 0;
      ColCount := 7;
      DefaultColWidth := FColWidthMonth;
    end;
  end

  else begin
    FixedCols := 0;
    ColCount := 4;
    RowCount := 4;
    DefaultColWidth := FColWidthYear;
    DefaultRowHeight := FRowHeightYear;
    RowHeights[0] := FRowHeightMonth;
  end;
end;

//----------------------------------------------------------------------------//
//-                  C A L E N D A R C O L O R S                             -//
//----------------------------------------------------------------------------//
//- Info:        Verwaltet für das Kalendarcontrol die Farbattribute          //
//----------------------------------------------------------------------------//

constructor TQtCalendarColors.Create(AOwner: TQtCalendar);
begin
  Owner              := AOwner;
  FBackColor         := clBlack;
  FTextColor         := clWhite;
  FTitleBackColor    := $004F4F4F;
  FTitleTextColor    := clWhite;
  FMonthBackColor    := clBlack;
  FTrailingTextColor := clGray;
  FTodaysColor       := $00BF5F5F;
  FTodaysBorderColor := clBlue;
  FTodaysTextColor   := clWhite;
  FSelectedBorderColor := clSilver;
  FSelectedColor     := clGray;
  FSelectedTextColor := clWhite;
  FFlyByColor        := clRed;
end;

//----------------------------------------------------------------------------//
//- A S S I G N                                                              -//
//----------------------------------------------------------------------------//
//- Info:        Sets all colors of the calendar-control                     -//
//- Parameter:   Source     Parent-Control                                   -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TQtCalendarColors.Assign(Source: TPersistent);
var
  SourceName: string;
begin
  if Source = nil then SourceName := 'nil'
  else SourceName := Source.ClassName;
  if (Source = nil) or not (Source is TQtCalendarColors) then
    raise EConvertError.CreateFmt(SAssignError, [SourceName, ClassName]);
  FBackColor         := TQtCalendarColors(Source).BackColor;
  FTextColor         := TQtCalendarColors(Source).TextColor;
  FTitleBackColor    := TQtCalendarColors(Source).TitleBackColor;
  FTitleTextColor    := TQtCalendarColors(Source).TitleTextColor;
  FMonthBackColor    := TQtCalendarColors(Source).MonthBackColor;
  FTrailingTextColor := TQtCalendarColors(Source).TrailingTextColor;
  FTodaysColor       := TQtCalendarColors(Source).TodaysColor;
  FTodaysBorderColor := TQtCalendarColors(Source).TodaysBorderColor;
  FTodaysTextColor   := TQtCalendarColors(Source).TodaysTextColor;
  FSelectedBorderColor := TQtCalendarColors(Source).SelectedBorderColor;
  FSelectedColor     := TQtCalendarColors(Source).SelectedColor;
  FSelectedTextColor := TQtCalendarColors(Source).SelectedTextColor;
  FFlyByColor        := TQtCalendarColors(Source).FlyByColor;
end;

//----------------------------------------------------------------------------//
//- S E T C O L O R                                                          -//
//----------------------------------------------------------------------------//
//- Info:        Sets all colors of the calendar-control                     -//
//- Parameter:   Index      Index of color value                             -//
//-              Value      Color for xcontrolelement                        -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TQtCalendarColors.SetColor(Index: Integer; Value: TColor);
begin
  Owner.SetColor(Index, Value);
  case Index of
    0: FBackColor         := Value;
    1: FTextColor         := Value;
    2: FTitleBackColor    := Value;
    3: FTitleTextColor    := Value;
    4: FMonthBackColor    := Value;
    5: FTrailingTextColor := Value;
    6: FTodaysColor       := Value;
    7: FTodaysBorderColor := Value;
    8: FTodaysTextColor   := Value;
    9: FSelectedBorderColor := Value;
   10: FSelectedColor     := Value;
   11: FSelectedTextColor := Value;
   12: FFlyByColor        := Value;
  end;
  Owner.Invalidate;
end;

//----------------------------------------------------------------------------//
//- S E T A L L C O L O R S                                                  -//
//----------------------------------------------------------------------------//
//- Info:        Sets all colors in one way                                  -//
//- Parameter:   (none)                                                      -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TQtCalendarColors.SetAllColors;
begin
  SetColor(0, FBackColor);
  SetColor(1, FTextColor);
  SetColor(2, FTitleBackColor);
  SetColor(3, FTitleTextColor);
  SetColor(4, FMonthBackColor);
  SetColor(5, FTrailingTextColor);
  SetColor(6, FTodaysColor);
  SetColor(7, FTodaysBorderColor);
  SetColor(8, FTodaysTextColor);
  SetColor(9, FSelectedBorderColor);
  SetColor(10, FSelectedColor);
  SetColor(11, FSelectedTextColor);
  SetColor(12, FFlyByColor);
end;


//----------------------------------------------------------------------------//
//-                  T C A L E N D A R B U T T O N S                         -//
//----------------------------------------------------------------------------//
//- Info:        Schaltflächen für die Steuerung des Kalendercontrols        -//
//- Erstellt:    31.03.1999                                                  -//
//- Update:      31.03.1999                                                  -//
//- Autor:       (c) 1999 Frank Demmig                                       -//
//----------------------------------------------------------------------------//

//----------------------------------------------------------------------------//
//- C R E A T E                                                              -//
//----------------------------------------------------------------------------//
//- Info:        Create component and setup defaults                         -//
//- Parameter:   (Standard)                                                  -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
constructor TCalendarButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSize := True;
  Transparent := True;
  FFlyByColor := clRed;
  FEnabled := True;
  Layout := tlCenter;
  AllowTimer := True;
end;

//----------------------------------------------------------------------------//
//- D E S T R O Y                                                            -//
//----------------------------------------------------------------------------//
//- Info:        Destroy Component and free used components                  -//
//- Parameter:   (none)                                                      -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
destructor TCalendarButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------//
//- S E T F L Y B Y C O L R                                                  -//
//----------------------------------------------------------------------------//
//- Info:        Destroy Component and free used components                  -//
//- Parameter:   Value    New color for text, if mouse is over control       -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarButton.SetFlyByColor(Value: TColor);
begin
  if FFlyByColor <> Value then begin
    FFlyByColor := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------//
//- S E T E N A B L E D                                                      -//
//----------------------------------------------------------------------------//
//- Info:        Sets enabled-state of control                               -//
//- Parameter:   Value    True for enabling, otherwise false                 -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarButton.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------//
//- C M M O U S E E N T E R                                                  -//
//----------------------------------------------------------------------------//
//- Info:        Handles event, if mouse enters components client area       -//
//- Parameter:   Msg      TMessage-structure                                 -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarButton.CMMouseEnter( var Msg: TMessage );
begin
  inherited;
  if Enabled = True then begin
    FOrigColor := Font.Color;
    Font.Color := FFlyByColor;
    if (FRepeatTimer <> nil) and AllowTimer then
      FRepeatTimer.Enabled := True;
  end;
end;

//----------------------------------------------------------------------------//
//- C M M O U S E L E A V E                                                  -//
//----------------------------------------------------------------------------//
//- Info:        Handles event, if mouse leaves components client area       -//
//- Parameter:   Msg      TMessage-structure                                 -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarButton.CMMouseLeave( var Msg: TMessage );
begin
  inherited;
  Font.Color := FOrigColor;
  if (FRepeatTimer <> nil) then
    FRepeatTimer.Enabled := False;
end;

//----------------------------------------------------------------------------//
//- M O U S E D O W N                                                        -//
//----------------------------------------------------------------------------//
//- Info:        Setups Timer, if left mousebutton is pressed                -//
//- Parameter:   (standard)                                                  -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
                                     X, Y: Integer);
begin
  inherited MouseDown(Button, Shift,X, Y);

  // Let grid get focus, if user clicked on another area of control
  if FocusControl <> nil then
    FocusControl.SetFocus;

  // Create TTimer, if necessary
  if FAllowTimer and Enabled then begin
    if FRepeatTimer = nil then
       FRepeatTimer := TTimer.Create(Self);
    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled := True;
  end;
end;

//----------------------------------------------------------------------------//
//- M O U S E U P                                                            -//
//----------------------------------------------------------------------------//
//- Info:        Disabling Timer, if left mousebutton is up                  -//
//- Parameter:   (standard)                                                  -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled := False;
end;

//----------------------------------------------------------------------------//
//- T I M E R E X P I R E D                                                  -//
//----------------------------------------------------------------------------//
//- Info:        Check, if enough time passed and triggers click-event       -//
//-              accordingly                                                 -//
//- Parameter:   (standard)                                                  -//
//- Returns:     (nothing)                                                   -//
//----------------------------------------------------------------------------//
procedure TCalendarButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if MouseCapture then begin
    try
      if Enabled then
        Click
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Frank Demmig', [TQtCalendar]);
end;

end.



