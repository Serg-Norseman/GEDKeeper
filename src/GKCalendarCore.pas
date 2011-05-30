unit GKCalendarCore; {trans:partial}

(*
 * Partial copyright by John Walker
 * Project: Fourmilab Calendar Converter
 * http://www.fourmilab.ch/documents/calendar/
 *)

interface

uses Windows;

const
  HebrewMonths: array [1..13] of string = (
    'Нисан', 'Ияр', 'Сиван', 'Тамуз',
    'Ав', 'Элул', 'Тишрей', 'Хешван',
    'Кислев', 'Тевет', 'Шват', 'Адар',
    'Адар бет'
  );
  HebrewWeekdays: array [0..6] of string = (
    'алеф', 'бейт', 'гимел', 'далет', 'хей', 'вав', 'зайин'
  );

  IslamicMonths: array [1..12] of string = (
    'мухаррам', 'сафар', 'рабии`у ль-авваль', 'рабии`у с-саании',
    'джумаада ль-ууля', 'джумаада ль-аахыр', 'раджаб', 'шаабан',
    'рамадан', 'шавваль', 'зуль-ка`да', 'зульхиджа'
  );
  IslamicWeekdays: array [0..6] of string = (
    'аль-ахад', 'аль-иснайн', 'ас-саласа''', 'аль-арба''а', 'аль-хамис', 'аль-джум''а', 'ас-сабт'
  );


  PersianMonths: array [1..12] of string = (
    'Фарвардин', 'Ордибехешт', 'Хордад', 'Тир',
    'Мордад', 'Шахривар', 'Мехр', 'Абан',
    'Азар', 'Дей', 'Бахман', 'Эсфанд'
  );
  PersianWeekdays: array [0..6] of string = (
    'йекшанбе', 'душанбе', 'сешанбе', 'чахаршанбе', 'панджшанбе', 'джоме', 'шанбе'
  );


  IndianCivilMonths: array [1..12] of string = (
    'Чайтра', 'Ваисакха', 'Джанштха', 'Асадха',
    'Сравана', 'Бхадра', 'Азвина', 'Картика',
    'Аграхайана', 'Пауза', 'Магха', 'Пхалгуна'
  );
  IndianCivilWeekdays: array [0..6] of string = (
    'равивар', 'сомвар', 'мангалвар', 'будхвар',
    'брихаспативар', 'шукрвар', 'шанивар'
  );


  BahaiMonths: array [1..20] of string = (
    'Бахa', 'Джалaл', 'Джамaл', 'Азамат',
    'Нур', 'Рахмат', 'Калимaт', 'Камaл',
    'Асмa', 'Иззат', 'Машиййат', 'Ильм',
    'Кудрат', 'Каул', 'Масa’иль',
    'Шараф', 'Султан', 'Мульк', 'Аййaм-и Хa',
    'Алa'
  );
  BahaiWeekdays: array [0..6] of string = (
    'Джамаль', 'Камаль', 'Фидаль', 'Идаль', 'Истиджлаль', 'Истиклаль', 'Джалаль'
  );


function jwday(j: Double): Longint;

function gregorian_to_jd(year, month, day: Integer): Double;
procedure jd_to_gregorian(jd: Double; var year, month, day: Longint);

function julian_to_jd(year, month, day: Longint): Double;
procedure jd_to_julian(jd: Double; var year, month, day: Integer);

function hebrew_to_jd(year, month, day: Longint): Double;
procedure jd_to_hebrew(jd: Double; var year, month, day: Longint);

function islamic_to_jd(year, month, day: Longint): Double;
procedure jd_to_islamic(jd: Double; var year, month, day: Longint);

function persian_to_jd(year, month, day: Longint): Double;
procedure jd_to_persian(jd: Double; var year, month, day: Longint);

function indian_civil_to_jd(year, month, day: Longint): Double;
procedure jd_to_indian_civil(jd: Double; var year, month, day: Longint);

function bahai_to_jd(major, cycle, year, month, day: Longint): Double;
procedure jd_to_bahai(jd: Double; var major, cycle, year, month, day: Longint);

type
  TExDate = record
    Day, Month, Year: Word;
    Era: (AD, BC); // (AD - Нашей эры, BC - До нашей эры)
  end;

function gkDateToStr(aDate: TExDate): string;

implementation

uses
  SysUtils, Math;

(*  MOD  --  Modulus function which works for non-integers.  *)
function _modf(a, b: Double): Double;
begin
  Result := a - (b * Floor(a / b));
end;

function _modi(a, b: Double): Longint;
begin
  Result := Trunc(a - (b * Floor(a / b)));
end;

function jwday(j: Double): Longint;
begin
  Result := _modi(Floor((j + 1.5)), 7);
end;

function cIf(aCond: Boolean; aThen, aElse: Longint): Longint;
begin
  if aCond
  then Result := aThen
  else Result := aElse;
end;

function gkDateToStr(aDate: TExDate): string;
begin
  Result := IntToStr(aDate.Day) + ' ' + ShortMonthNames[aDate.Month] + ' ' + IntToStr(aDate.Year);
  if not(aDate.Era = AD) then Result := Result + ' до н.э.';
end;

{================================== Julian day ================================}

//  LEAP_GREGORIAN  --  Is a given year in the Gregorian calendar a leap year ?

function leap_gregorian(year: Longint): Boolean;
begin
  Result := ((year mod 4) = 0) and
            (not(((year mod 100) = 0) and ((year mod 400) <> 0)));
end;

//  GREGORIAN_TO_JD  --  Determine Julian day number from Gregorian calendar date

const
  GREGORIAN_EPOCH = 1721425.5;

function gregorian_to_jd(year, month, day: Longint): Double;
begin
  Result := (GREGORIAN_EPOCH - 1) +
            (365 * (year - 1)) +
            Math.floor((year - 1) / 4) +
            (-Math.floor((year - 1) / 100)) +
            Math.floor((year - 1) / 400) +
            Math.floor((((367 * month) - 362) / 12) +
            (cIf(month <= 2, 0, cIf(leap_gregorian(year), -1, -2))) + day);
end;

//  JD_TO_GREGORIAN  --  Calculate Gregorian calendar date from Julian day

procedure jd_to_gregorian(jd: Double; var year, month, day: Longint);
var
  wjd, depoch, dqc, dcent, dquad, yearday: Double;
  quadricent, cent, quad, yindex, leapadj: Longint;
begin
  wjd := floor(jd - 0.5) + 0.5;
  depoch := wjd - GREGORIAN_EPOCH;
  quadricent := floor(depoch / 146097);
  dqc := _modf(depoch, 146097);
  cent := floor(dqc / 36524);
  dcent := _modf(dqc, 36524);
  quad := floor(dcent / 1461);
  dquad := _modf(dcent, 1461);
  yindex := floor(dquad / 365);
  year := (quadricent * 400) + (cent * 100) + (quad * 4) + yindex;
  if (not((cent = 4) or (yindex = 4))) then Inc(year);

  yearday := wjd - gregorian_to_jd(year, 1, 1);
  leapadj := (cIf(wjd < gregorian_to_jd(year, 3, 1), 0, cIf(leap_gregorian(year), 1, 2)));
  month := floor((((yearday + leapadj) * 12) + 373) / 367);
  day := Trunc(wjd - gregorian_to_jd(year, month, 1)) + 1;
end;

{=============================== Julian Calendar ==============================}

//  JULIAN_TO_JD  --  Determine Julian day number from Julian calendar date

const
  JULIAN_EPOCH = 1721423.5;

function leap_julian(year: Longint): Boolean;
begin
  Result := _modf(year, 4) = cIf(year > 0, 0, 3);
end;

function julian_to_jd(year, month, day: Longint): Double;
begin
  (* Adjust negative common era years to the zero-based notation we use.  *)
  if (year < 1) then Inc(year);

  (* Algorithm as given in Meeus, Astronomical Algorithms, Chapter 7, page 61 *)
  if (month <= 2) then begin
    Dec(year);
    month := month + 12;
  end;

  Result := ((Math.floor((365.25 * (year + 4716))) +
            Math.floor((30.6001 * (month + 1))) + day) - 1524.5);
end;

//  JD_TO_JULIAN  --  Calculate Julian calendar date from Julian day

procedure jd_to_julian(jd: Double; var year, month, day: Longint);
var
  b, c, d, e: Longint;
begin
  jd := jd + 0.5;

  b := Math.floor(jd) + 1524;
  c := Math.floor((b - 122.1) / 365.25);
  d := Math.floor(365.25 * c);
  e := Math.floor((b - d) / 30.6001);

  month := Math.floor(cIf(e < 14, e - 1, e - 13));
  year := Math.floor(cIf(month > 2, c - 4716, c - 4715));
  day := b - d - Math.floor(30.6001 * e);

  (*  If year is less than 1, subtract one to convert from
      a zero based date system to the common era system in
      which the year -1 (1 B.C.E) is followed by year 1 (1 C.E.).  *)

  if (year < 1) then Dec(year);
end;

{=============================== Hebrew Calendar ==============================}

const
  HEBREW_EPOCH = 347995.5;

//  Is a given Hebrew year a leap year ?
function hebrew_leap(year: Longint): Boolean;
begin
  Result := _modf(((year * 7) + 1), 19) < 7;
end;

//  How many months are there in a Hebrew year (12 = normal, 13 = leap)
function hebrew_year_months(year: Longint): Longint;
begin
  Result := cIf(hebrew_leap(year), 13, 12);
end;

//  Test for delay of start of new year and to avoid
//  Sunday, Wednesday, and Friday as start of the new year.
function hebrew_delay_1(year: Longint): Longint;
var
  months, parts, day: Longint;
begin
  months := Math.floor(((235 * year) - 234) / 19);
  parts := 12084 + (13753 * months);
  day := (months * 29) + Math.floor(parts / 25920);

  if (_modf((3 * (day + 1)), 7) < 3) then Inc(day);

  Result := day;
end;

//  Check for delay in start of new year due to length of adjacent years
function hebrew_delay_2(year: Longint): Longint;
var
  last, present, next: Longint;
begin
  last := hebrew_delay_1(year - 1);
  present := hebrew_delay_1(year);
  next := hebrew_delay_1(year + 1);

  Result := (cIf((next - present) = 356, 2, (cIf((present - last) = 382, 1, 0))));
end;

//  How many days are in a Hebrew year ?
function hebrew_year_days(year: Longint): Double;
begin
  Result := hebrew_to_jd(year + 1, 7, 1) - hebrew_to_jd(year, 7, 1);
end;

//  How many days are in a given month of a given year
function hebrew_month_days(year, month: Longint): Longint;
begin
  //  First of all, dispose of fixed-length 29 day months
  if (month = 2) or (month = 4) or (month = 6) or (month = 10) or (month = 13)
  then begin
    Result := 29;
    Exit;
  end;

  //  If it's not a leap year, Adar has 29 days
  if (month = 12) and not(hebrew_leap(year)) then begin
    Result := 29;
    Exit;
  end;

  //  If it's Heshvan, days depend on length of year
  if (month = 8) and not(_modf(hebrew_year_days(year), 10) = 5) then begin
    Result := 29;
    Exit;
  end;

  //  Similarly, Kislev varies with the length of year
  if (month = 9) and (_modf(hebrew_year_days(year), 10) = 3) then begin
    Result := 29;
    Exit;
  end;

  //  Nope, it's a 30 day month
  Result := 30;
end;

(*  HEBREW_TO_JD  --  Determine Julian day from Hebrew date *)
function hebrew_to_jd(year, month, day: Longint): Double;
var
  jd: Double;
  mon, months: Longint;
begin
  months := hebrew_year_months(year);
  jd := HEBREW_EPOCH + hebrew_delay_1(year) + hebrew_delay_2(year) + day + 1;

  if (month < 7) then begin
    for mon := 7 to months do
      jd := jd + hebrew_month_days(year, mon);

    for mon := 1 to month - 1 do
      jd := jd + hebrew_month_days(year, mon);
  end else begin
    for mon := 7 to month - 1 do
      jd := jd + hebrew_month_days(year, mon);
  end;

  Result := jd;
end;

(*  JD_TO_HEBREW  --  Convert Julian date to Hebrew date
                      This works by making multiple calls to
                      the inverse function, and is this very slow.  *)

procedure jd_to_hebrew(jd: Double; var year, month, day: Longint);
var
  i, count, first: Longint;
begin
  jd := Math.floor(jd) + 0.5;
  count := Math.floor(((jd - HEBREW_EPOCH) * 98496.0) / 35975351.0);
  year := count - 1;
  i := count;
  while (jd >= hebrew_to_jd(i, 7, 1)) do begin
    Inc(i);
    Inc(year);
  end;
  first := cIf(jd < hebrew_to_jd(year, 1, 1), 7, 1);
  month := first;
  i := first;
  while (jd > hebrew_to_jd(year, i, hebrew_month_days(year, i))) do begin
    Inc(i);
    Inc(month);
  end;
  day := Trunc((jd - hebrew_to_jd(year, month, 1)) + 1);
end;

{============================== Islamic Calendar ==============================}

//  LEAP_ISLAMIC  --  Is a given year a leap year in the Islamic calendar ?

function leap_islamic(year: Longint): Boolean;
begin
  Result := (((year * 11) + 14) mod 30) < 11;
end;

//  ISLAMIC_TO_JD  --  Determine Julian day from Islamic date

const
  ISLAMIC_EPOCH = 1948439.5;

function islamic_to_jd(year, month, day: Longint): Double;
begin
  Result := (day + Ceil(29.5 * (month - 1)) + (year - 1) * 354 +
             Floor((3 + (11 * year)) / 30) + ISLAMIC_EPOCH) - 1;
end;

//  JD_TO_ISLAMIC  --  Calculate Islamic date from Julian day

procedure jd_to_islamic(jd: Double; var year, month, day: Longint);
begin
  jd := Floor(jd) + 0.5;
  year := Floor(((30 * (jd - ISLAMIC_EPOCH)) + 10646) / 10631);
  month := Min(12, Ceil((jd - (29 + islamic_to_jd(year, 1, 1))) / 29.5) + 1);
  day := Trunc((jd - islamic_to_jd(year, month, 1)) + 1);
end;

{============================== Persian Calendar ==============================}

//  LEAP_PERSIAN  --  Is a given year a leap year in the Persian calendar ?

function leap_persian(year: Longint): Boolean;
begin
  Result := ((((((year - cIf((year > 0), 474, 473)) mod 2820) + 474) + 38) * 682) mod 2816) < 682;
end;

//  PERSIAN_TO_JD  --  Determine Julian day from Persian date

const
  PERSIAN_EPOCH = 1948320.5;

function persian_to_jd(year, month, day: Longint): Double;
var
  epbase, epyear: Double;
begin
  epbase := year - cIf((year >= 0), 474, 473);
  epyear := 474 + _modf(epbase, 2820);

  Result := day +
          cIf((month <= 7), ((month - 1) * 31), (((month - 1) * 30) + 6)) +
          Floor(((epyear * 682) - 110) / 2816) +
          (epyear - 1) * 365 + Floor(epbase / 2820) * 1029983 + (PERSIAN_EPOCH - 1);
end;

//  JD_TO_PERSIAN  --  Calculate Persian date from Julian day

procedure jd_to_persian(jd: Double; var year, month, day: Longint);
var
  depoch, yday: Double;
  cycle, cyear, ycycle, aux1, aux2: Longint;
begin
  jd := Math.floor(jd) + 0.5;
  depoch := jd - persian_to_jd(475, 1, 1);
  cycle := Math.floor(depoch / 1029983);
  cyear := _modi(depoch, 1029983);
  if (cyear = 1029982)
  then ycycle := 2820
  else begin
    aux1 := Math.floor(cyear / 366);
    aux2 := _modi(cyear, 366);
    ycycle := Math.floor(((2134 * aux1) + (2816 * aux2) + 2815) / 1028522) + aux1 + 1;
  end;
  year := ycycle + (2820 * cycle) + 474;
  if (year <= 0) then Dec(year);
  yday := (jd - persian_to_jd(year, 1, 1)) + 1;
  month := cIf((yday <= 186), Math.ceil(yday / 31), Math.ceil((yday - 6) / 30));
  day := Trunc((jd - persian_to_jd(year, month, 1)) + 1);
end;

{=========================== Indian civil Calendar ============================}

//  INDIAN_CIVIL_TO_JD  --  Obtain Julian day for Indian Civil date

function indian_civil_to_jd(year, month, day: Longint): Double;
var
  Caitra, gyear, m: Longint;
  leap: Boolean;
  start, jd: Double;
begin
  gyear := year + 78;
  leap := leap_gregorian(gyear);     // Is this a leap year ?
  start := gregorian_to_jd(gyear, 3, cIf(leap, 21, 22));
  Caitra := cIf(leap, 31, 30);

  if (month = 1)
  then jd := start + (day - 1)
  else begin
    jd := start + Caitra;
    m := month - 2;
    m := Math.min(m, 5);
    jd := jd + m * 31;
    if (month >= 8) then begin
      m := month - 7;
      jd := jd + m * 30;
    end;
    jd := jd + day - 1;
  end;

  Result := jd;
end;

//  JD_TO_INDIAN_CIVIL  --  Calculate Indian Civil date from Julian day

procedure jd_to_indian_civil(jd: Double; var year, month, day: Longint);
var
  Caitra, Saka, mday, greg_y, greg_m, greg_d: Longint;
  leap: Boolean;
  yday, greg0, start: Double;
begin
  Saka := 79 - 1;                    // Offset in years from Saka era to Gregorian epoch
  start := 80;                       // Day offset between Saka and Gregorian

  jd := Math.floor(jd) + 0.5;
  jd_to_gregorian(jd, greg_y, greg_m, greg_d);       // Gregorian date for Julian day
  leap := leap_gregorian(greg_y);   // Is this a leap year?
  year := greg_y - Saka;            // Tentative year in Saka era
  greg0 := gregorian_to_jd(greg_y, 1, 1); // JD at start of Gregorian year
  yday := jd - greg0;                // Day number (0 based) in Gregorian year
  Caitra := cIf(leap, 31, 30);          // Days in Caitra this year

  if (yday < start) then begin
    //  Day is at the end of the preceding Saka year
    Dec(year);
    yday := yday + Caitra + (31 * 5) + (30 * 3) + 10 + start;
  end;

  yday := yday - start;
  if (yday < Caitra) then begin
    month := 1;
    day := Floor(yday + 1);
  end else begin
    mday := Floor(yday - Caitra);
    if (mday < (31 * 5)) then begin
      month := Math.floor(mday / 31) + 2;
      day := (mday mod 31) + 1;
    end else begin
      mday := mday - 31 * 5;
      month := Math.floor(mday / 30) + 7;
      day := (mday mod 30) + 1;
    end;
  end;
end;

{=============================== Bahai Calendar ===============================}

//  BAHAI_TO_JD  --  Determine Julian day from Bahai date

const
  BAHAI_EPOCH = 2394646.5;

function bahai_to_jd(major, cycle, year, month, day: Longint): Double;
var
  gy, by, dummy: Integer;
begin
  jd_to_gregorian(BAHAI_EPOCH, by, dummy, dummy);

  gy := (361 * (major - 1)) + (19 * (cycle - 1)) + (year - 1) + by;

  Result := gregorian_to_jd(gy, 3, 20) + (19 * (month - 1)) +
         cIf((month <> 20), 0, cIf(leap_gregorian(gy + 1), -14, -15)) + day;
end;

//  JD_TO_BAHAI  --  Calculate Bahai date from Julian day

procedure jd_to_bahai(jd: Double; var major, cycle, year, month, day: Longint);
var
  gy, bstarty, bys, dummy: Integer;
  days, bld: Double;
begin
  jd := Math.floor(jd) + 0.5;
  jd_to_gregorian(jd, gy, dummy, dummy);
  jd_to_gregorian(BAHAI_EPOCH, bstarty, dummy, dummy);
  bys := gy - (bstarty + cIf(((gregorian_to_jd(gy, 1, 1) <= jd) and (jd <= gregorian_to_jd(gy, 3, 20))), 1, 0));
  major := Math.floor(bys / 361) + 1;
  cycle := Math.floor(_modf(bys, 361) / 19) + 1;
  year := _modi(bys, 19) + 1;
  days := jd - bahai_to_jd(major, cycle, year, 1, 1);
  bld := bahai_to_jd(major, cycle, year, 20, 1);
  month := cIf((jd >= bld), 20, (Math.floor(days / 19) + 1));
  day := Floor((jd + 1) - bahai_to_jd(major, cycle, year, month, 1));
end;

end.
