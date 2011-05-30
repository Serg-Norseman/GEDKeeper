unit bsComUtils;

// Проект: Brainstorm
// Автор: Сергей В. Ждановских (Alchemist)
// Модуль: bsUtils
// Создан: 05.09.1997
// Описание: Вспомогательные функции.

{$I compiler.inc}

interface

type
  TByteSet = set of Byte;
  
  // Тип условия проверки
  TConditionKind = (ck_NotEq, ck_LT, ck_LET, ck_Eq, ck_GET, ck_GT, ck_Between);

  // Тип значений
  TValueKind = (vkInteger, vkFloat);

  // Узел проверки условия
  TConditionNode = packed record
    // Тип объекта проверки
    objKind: Longword;
    // Объект проверки
    objName: string;
    // Тип значений
    valKind: TValueKind;
    // Тип условия проверки
    case cnKind: TConditionKind of
      // Значение
      ck_NotEq, ck_LT, ck_LET, ck_Eq, ck_GET, ck_GT: (
        case TValueKind of
          vkInteger: (cniValue: Integer);
          vkFloat: (cnsValue: Single);
      );
      // Границы диапазона
      ck_Between: (
        case TValueKind of
          vkInteger: (cniLowBound, cniHighBound: Integer);
          vkFloat: (cnsLowBound, cnsHighBound: Single);
      );
  end;

const
  ConditionKinds: array [TConditionKind] of string = (
    '? <> a', '? < a', '? =< a', '? = a', '? >= a', '? > a', '[a >= ? =< b]'
  );

// Конвертировать путь (Linux/Windows)
function ConvertPath(aPath: string): string;

// Путь приложения
function GetAppPath(): string;

// Дополнить число нулями
function NumUpdate(val, up: Integer): string;

// Бинарная строка
function ShowBits(B: Byte): string;

// Установить бит
procedure SetBit(var ChangeValue: Cardinal; Position: Integer; Value: Byte = 1);

// Существование бита
function IsBitSet(const Value: Longint; const BitPos: Byte): Boolean;

// Hole функция предотвращает распределение переменной
// в регистрах CPU подлежащих оптимизации
function Hole(var A): Integer;

// Конвертирование между кодовыми страницами
type
  TCodePage = (cpKOI8R, cpWin1251, cpDOS866, cpMAC, cpISO8859_5);

function ConvertString(PageFrom, PageTo: TCodePage; str: string): string;

// извлечение токена
function GetToken(aString, SepChar: string; TokenNum: Byte): string;

// количество токенов
function GetTokensCount(aString, SepChar: string): Byte;

// Получить число римскими цифрами
function GetRome(N: Integer): string;

// Начать протоколирование
procedure LogInit(const aFileName: string);
// Записать в протокол
procedure LogWrite(const aMsg: string);
// Завершить протоколирование
procedure LogDone();

implementation

uses
  SysUtils
  {$IFDEF DELPHI_NET}, System.Reflection, System.IO{$ENDIF};

// Конвертировать путь (Linux/Windows)
function ConvertPath(aPath: string): string;
var
  i: Integer;
begin
  Result := aPath;
  {$IFDEF OS_LINUX}
  for i := 1 to Length(aPath) do
    if (aPath[i] = '\') then Result[i] := '/';
  {$ELSE}
  for i := 1 to Length(aPath) do
    if (aPath[i] = '/') then Result[i] := '\';
  {$ENDIF}
end;

// Путь приложения
function GetAppPath(): string;
{$IFDEF DELPHI_NET}
var
  fn{, nm}: string;
{$ENDIF}
begin
  {$IFNDEF DELPHI_NET}
  Result := ExtractFilePath(ParamStr(0));
  {$ELSE}
  // strictly CF compatible
  fn := System.Reflection.Assembly.GetExecutingAssembly.GetModules[0].FullyQualifiedName;
  //nm := System.IO.Path.GetFileName(fn);
  Result := System.IO.Path.GetDirectoryName(fn) + '\';
  {$ENDIF}
end;

// Дополнить число нулями
function NumUpdate(val, up: Integer): string;
begin
  Result := IntToStr(val);
  while (Length(Result) < up) do Result := '0' + Result;
end;

// Бинарная строка
function ShowBits(B: Byte): string;
var
  i: Integer;
  bt: Byte;
  s: string;
begin
  bt := $01;
  s := '';
  for i := 1 to 8 do begin
    if (b and bt) > 0
    then s := '1' + s
    else s := '0' + s;
    bt := (bt shl 1);
  end;
  Result := s;
end;

// Установить бит
procedure SetBit(var ChangeValue: Cardinal; Position: Integer; Value: Byte = 1);
var
  bt: Cardinal;
begin
  bt := $01 shl Position;
  if (Value = 1)
  then ChangeValue := ChangeValue or bt
  else ChangeValue := ChangeValue and (bt xor $FFFFFFFF);
end;

// Существование бита
function IsBitSet(const Value: Longint; const BitPos: Byte): Boolean;
begin
  Result := (Value and ($01 shl BitPos)) <> 0;
end;

function Hole(var A): Integer;
{$IFNDEF DELPHI_NET}
asm
{$ELSE}
begin
{$ENDIF}
end;

const
  CodeTableLen = 72;
  CodeTables: array [1..CodeTableLen, TCodePage] of Char = (
    (#$e1, #$c0, #$80, #$80, #$b0),
    (#$e2, #$c1, #$81, #$81, #$b1),
    (#$f7, #$c2, #$82, #$82, #$b2),
    (#$e7, #$c3, #$83, #$83, #$b3),
    (#$e4, #$c4, #$84, #$84, #$b4),
    (#$e5, #$c5, #$85, #$85, #$b5),
    (#$f6, #$c6, #$86, #$86, #$b6),
    (#$fa, #$c7, #$87, #$87, #$b7),
    (#$e9, #$c8, #$88, #$88, #$b8),
    (#$ea, #$c9, #$89, #$89, #$b9),
    (#$eb, #$ca, #$8a, #$8a, #$ba),
    (#$ec, #$cb, #$8b, #$8b, #$bb),
    (#$ed, #$cc, #$8c, #$8c, #$bc),
    (#$ee, #$cd, #$8d, #$8d, #$bd),
    (#$ef, #$ce, #$8e, #$8e, #$be),
    (#$f0, #$cf, #$8f, #$8f, #$bf),
    (#$f2, #$d0, #$90, #$90, #$c0),
    (#$f3, #$d1, #$91, #$91, #$c1),
    (#$f4, #$d2, #$92, #$92, #$c2),
    (#$f5, #$d3, #$93, #$93, #$c3),
    (#$e6, #$d4, #$94, #$94, #$c4),
    (#$e8, #$d5, #$95, #$95, #$c5),
    (#$e3, #$d6, #$96, #$96, #$c6),
    (#$fe, #$d7, #$97, #$97, #$c7),
    (#$fb, #$d8, #$98, #$98, #$c8),
    (#$fd, #$d9, #$99, #$99, #$c9),
    (#$ff, #$da, #$9a, #$9a, #$ca),
    (#$f9, #$db, #$9b, #$9b, #$cb),
    (#$f8, #$dc, #$9c, #$9c, #$cc),
    (#$fc, #$dd, #$9d, #$9d, #$cd),
    (#$e0, #$de, #$9e, #$9e, #$ce),
    (#$f1, #$df, #$9f, #$9f, #$cf),
    (#$c1, #$e0, #$a0, #$e0, #$d0),
    (#$c2, #$e1, #$a1, #$e1, #$d1),
    (#$d7, #$e2, #$a2, #$e2, #$d2),
    (#$c7, #$e3, #$a3, #$e3, #$d3),
    (#$c4, #$e4, #$a4, #$e4, #$d4),
    (#$c5, #$e5, #$a5, #$e5, #$d5),
    (#$d6, #$e6, #$a6, #$e6, #$d6),
    (#$da, #$e7, #$a7, #$e7, #$d7),
    (#$c9, #$e8, #$a8, #$e8, #$d8),
    (#$ca, #$e9, #$a9, #$e9, #$d9),
    (#$cb, #$ea, #$aa, #$ea, #$da),
    (#$cc, #$eb, #$ab, #$eb, #$db),
    (#$cd, #$ec, #$ac, #$ec, #$dc),
    (#$ce, #$ed, #$ad, #$ed, #$dd),
    (#$cf, #$ee, #$ae, #$ee, #$de),
    (#$d0, #$ef, #$af, #$ef, #$df),
    (#$d2, #$f0, #$e0, #$f0, #$e0),
    (#$d3, #$f1, #$e1, #$f1, #$e1),
    (#$d4, #$f2, #$e2, #$f2, #$e2),
    (#$d5, #$f3, #$e3, #$f3, #$e3),
    (#$c6, #$f4, #$e4, #$f4, #$e4),
    (#$c8, #$f5, #$e5, #$f5, #$e5),
    (#$c3, #$f6, #$e6, #$f6, #$e6),
    (#$de, #$f7, #$e7, #$f7, #$e7),
    (#$db, #$f8, #$e8, #$f8, #$e8),
    (#$dd, #$f9, #$e9, #$f9, #$e9),
    (#$df, #$fa, #$ea, #$fa, #$ea),
    (#$d9, #$fb, #$eb, #$fb, #$eb),
    (#$d8, #$fc, #$ec, #$fc, #$ec),
    (#$dc, #$fd, #$ed, #$fd, #$ed),
    (#$c0, #$fe, #$ee, #$fe, #$ee),
    (#$d1, #$ff, #$ef, #$df, #$ef),
    (#$b3, #$a8, #$f0, #$dd, #$a1),
    (#$a3, #$b8, #$f1, #$de, #$f1),
    (#$00, #$ab, #$00, #$c7, #$00),
    (#$00, #$bb, #$00, #$c8, #$00),
    (#$be, #$b9, #$fc, #$dc, #$f0),
    (#$00, #$96, #$00, #$d0, #$00),
    (#$00, #$97, #$00, #$d1, #$00),
    (#$9a, #$a0, #$ff, #$ca, #$a0)
  );

// Конвертирование строки между кодовыми страницами
function ConvertString(PageFrom, PageTo: TCodePage; str: string): string;

  function ConvertChar(sym: Char): Char;
  var
    i: Integer;
  begin
    for i := 1 to CodeTableLen do begin
      if (sym = CodeTables[i, PageFrom]) then begin
        Result := CodeTables[i, PageTo];
        Exit;
      end;
    end;

    Result := sym;
  end;

var
  i: Integer;
begin
  Result := str;
  for i := 1 to Length(Result) do
    Result[i] := ConvertChar(Result[i]);
end;

// извлечение токена
function GetToken(aString, SepChar: string; TokenNum: Byte): string;
var
  Token: string;
  StrLen, TNum, TEnd: Integer;
begin
  StrLen := Length(aString);
  TNum := 1;
  TEnd := StrLen;

  while ((TNum <= TokenNum) and (TEnd <> 0)) do begin
    TEnd := Pos(SepChar, aString);
    if (TEnd <> 0) then begin
      Token := Copy(aString, 1, TEnd-1);
      Delete(aString, 1, TEnd);
      Inc(TNum);
    end else Token := aString;
  end;

  if (TNum >= TokenNum)
  then Result := Token
  else Result := '';
end;

// количество токенов
function GetTokensCount(aString, SepChar: string): Byte;
var
  RChar: Char;
  TNum, TEnd: Integer;
begin
  RChar := #01;
  TNum := 0;
  TEnd := Length(aString);

  while (TEnd <> 0) do begin
    Inc(TNum);
    TEnd := Pos(SepChar, aString);
    if (TEnd <> 0) then aString[TEnd] := RChar;
  end;

  Result := TNum;
end;

// Получить число римскими цифрами
function GetRome(N: Integer): string;
const
  Rn_N: array [1..13] of Integer = (
    1, 4, 5, 9, 10, 40, 50, 90, 100, 400, 500, 900, 1000);
  Rn_S: array [1..13] of string = (
    'I', 'IV', 'V', 'IX', 'X', 'XL', 'L', 'XC', 'C', 'CD', 'D', 'CM', 'M');
var
  S: string;
  T: Byte;
begin
  S := '';
  T := 13;
  while (N > 0) do begin
    while (N < Rn_N[T]) do Dec(T);
    while (N >= Rn_N[T]) do begin
      Dec(N, Rn_N[T]);
      S := S + Rn_S[T];
    end;
  end;
  Result := S;
end;

var
  LogFilename: string;

// Начать протоколирование
procedure LogInit(const aFileName: string);
begin
  LogFilename := ConvertPath(aFileName);
end;

// Записать в протокол
procedure LogWrite(const aMsg: string);
var
  Log: {$IFNDEF FREEPASCAL}TextFile{$ELSE}Text{$ENDIF};
begin
  AssignFile(Log, LogFilename);

  if FileExists(LogFilename)
  then Append(Log)
  else Rewrite(Log);

  Writeln(Log, '[' + DateTimeToStr(Now) + '] -> ' + aMsg);

  Flush(Log);
  CloseFile(Log);
end;

// Завершить протоколирование
procedure LogDone();
begin
end;

end.
