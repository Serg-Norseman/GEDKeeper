unit GKCalendar;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, QtCalendar;

type
  TfmCalendar = class(TForm)
    lvDates: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    qtc: TQtCalendar;
    procedure DateChange(Sender: TObject);
  public
  end;

var
  fmCalendar: TfmCalendar;

implementation

uses Calendar, GKMain;

{$R *.dfm}

type
  TDay = 1..31;
  TMonth = 1..12;

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
    FCalendar: TQtCalendar;

    function GetHoliday(index: Integer): THoliday;
    procedure SetHoliday(index: Integer; Value: THoliday);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TQtCalendar);
    function Add(ADay, AMonth: Word): THoliday;
    property Items[index: integer]: THoliday read GetHoliday write SetHoliday;
  end;

  TQtExtCalendar = class(TQtCalendar)
  private
    //function IsDateHoliday(ADay, AMonth: Word): Boolean;
  public
  end;

{ THoliday }

function THoliday.GetDisplayName: String;
begin
  Result := FName;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

{ THolidays }

constructor THolidays.Create(AOwner: TQtCalendar);
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

{
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
}

{==============================================================================}

{ TfmCalendar }

procedure TfmCalendar.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fmGEDKeeper.actCalendar.Checked := False;
  fmCalendar := nil;
  Action := caFree;
end;

procedure TfmCalendar.FormCreate(Sender: TObject);
begin
  qtc := TQtCalendar.Create(Self);
  qtc.Parent := Self;
  qtc.Left := 8;
  qtc.Top := 8;
  qtc.StartOfWeek := Monday;
  qtc.OnChange := Self.DateChange;
  qtc.Width := lvDates.Width;

  DateChange(nil);
end;

procedure TfmCalendar.DateChange(Sender: TObject);

  procedure AddItem(aCalendar, aDate: string);
  var
    item: TListItem;
  begin
    item := lvDates.Items.Add;
    item.Caption := aCalendar;
    item.SubItems.Add(aDate);
  end;

var
  gdt: TDate;
  edt: TExDate;
  jd: Double;
  major, cycle, year, month, day: Integer;
  s: string;
begin
  gdt := qtc.CalendarDate;

  lvDates.Clear;

  edt.Era := AD;

  DecodeDate(gdt, edt.Year, edt.Month, edt.Day);
  s := gkDateToStr(edt) + ', ' + LongDayNames[DayOfWeek(gdt)];
  AddItem('Григорианский', s);

  jd := gregorian_to_jd(edt.Year, edt.Month, edt.Day) { + (Math.floor(sec + 60 * (min + 60 * hour) + 0.5) / 86400.0);};
  jd_to_julian(jd, year, month, day);
  edt.Day := day;
  edt.Month := month;
  edt.Year := year;
  AddItem('Юлианский', gkDateToStr(edt));

  jd_to_hebrew(jd, year, month, day);
  s := IntToStr(day) + ' ';
  s := s + HebrewMonths[month];
  s := s + ' ' + IntToStr(year) + ', ' + HebrewWeekdays[jwday(jd)];
  AddItem('Еврейский', s);

  jd_to_islamic(jd, year, month, day);
  s := IntToStr(day) + ' ';
  s := s + IslamicMonths[month];
  s := s + ' ' + IntToStr(year) + ', йаум ' + IslamicWeekdays[jwday(jd)];
  AddItem('Исламский (Хиджры)', s);

  jd_to_persian(jd, year, month, day);
  s := IntToStr(day) + ' ';
  s := s + PersianMonths[month];
  s := s + ' ' + IntToStr(year) + ', ' + PersianWeekdays[jwday(jd)];
  AddItem('Иранский', s);

  jd_to_indian_civil(jd, year, month, day);
  s := IntToStr(day) + ' ';
  s := s + IndianCivilMonths[month];
  s := s + ' ' + IntToStr(year) + ', ' + IndianCivilWeekdays[jwday(jd)];
  AddItem('Индийский', s);

  jd_to_bahai(jd, major, cycle, year, month, day);
  s := 'Кулл-и Шай'' ' + IntToStr(major) + ', Вахид ' + IntToStr(cycle) + ', ';
  s := s + IntToStr(day) + ' ';
  s := s + BahaiMonths[month];
  s := s + ' ' + IntToStr(year) + ', ' + BahaiWeekdays[jwday(jd)];
  AddItem('Бахаи', s);
end;

end.
