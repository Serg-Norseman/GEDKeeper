unit GKCalendar;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, QtCalendar;

type
  TfmCalendar = class(TForm)
    ListView1: TListView;
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
  qtc.Width := ListView1.Width;

  ListView1.ReadOnly := True;
  ListView1.RowSelect := True;

  DateChange(nil);
end;

procedure TfmCalendar.DateChange(Sender: TObject);

  procedure AddItem(aCalendar, aDate: string);
  var
    item: TListItem;
  begin
    item := ListView1.Items.Add;
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

  ListView1.Clear;

  edt.Era := AD;

  DecodeDate(gdt, edt.Year, edt.Month, edt.Day);
  AddItem('Григорианский', gkDateToStr(edt) + ', ' + LongDayNames[DayOfWeek(gdt)]);

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
