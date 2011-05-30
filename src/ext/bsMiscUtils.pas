unit bsMiscUtils;

// Проект: Brainstorm
// Автор: Сергей В. Ждановских (Alchemist)
// Модуль: bsUtils
// Создан: 05.09.1997
// Описание: Вспомогательные функции.

{$I compiler.inc}

interface

uses
  {$IFDEF OS_LINUX}
  Libc,
  {$ELSE}
  Windows,
    {$IFDEF DELPHI_NET}
    System.IO, System.Text, System.Threading, Borland.Vcl.StrUtils,
    {$ENDIF}
  {$ENDIF}
  Types, SysUtils, Classes, bsComUtils;

{==============================================================================}

const
  CRLF = {$IFDEF OS_LINUX}#10{$ENDIF} {$IFDEF OS_MSWIN}#13#10{$ENDIF};

  NullPoint: TPoint = (X: 0; Y: 0);
  NegPoint: TPoint = (X: -1; Y: -1);

type
  TCharSet = set of AnsiChar;  

type
  // сигнатура файла
  TFileSign = array [0..2] of AnsiChar;

  // версия файла
  TFileVersion = packed record
    Release, Revision: Word;
  end;

  // стандартный заголовок файла
  TFileHeader = packed record
    Sign: TFileSign;
    Version: TFileVersion;
  end;

{==============================================================================}

{$IFNDEF DELPHI_NET}
function LoadLibrary(ModuleName: PChar): HMODULE;
function FreeLibrary(Module: HMODULE): LongBool;
function GetProcAddress(Module: HMODULE; Proc: PChar): Pointer;
{$ENDIF}

// Получить сигнатуру числа (знак)
function Sgn(aValue: Longint): Shortint;

// Определить корректность целого числа
function IsValidInt(const Number: string): Boolean;

// Определить корректность дробного числа
function IsValidFloat(const Number: string): Boolean;

// Транслитерация
type
  TScheme = (
    // Russian, Library of Congress, Pokrovsky EuroTex-92
    ts_Russian, ts_LibCongress, ts_Pokrovsky,
    // Volapuyk, GOST 16876-71, Simplified
    ts_Volapuyk, ts_GOST, ts_Simplified);

function Transliterate(s, t: TScheme; str: string): string;

// Наличие параметра командной строки
function HasParam(aName: string): Boolean;

// Поменять числа местами
procedure SwapValues(var aValue1: Integer; var aValue2: Integer);

// Случайное число
function RealRnd(HighBound: Integer): Single;

// Точка в прямоугольнике
function PointInRect(ptX, ptY: Integer; R: TRect): Boolean;

// Равенство точек
function EqualPoints(p1, p2: TPoint): Boolean; 

// Случайное число в диапазоне
function BoundedRnd(LB, HB: Integer): Integer;

// Сместить границы прямоугольника
procedure OffsetRect(var R: TRect; dX, dY: Integer);

// Прямоугольник в прямоугольнике
function RectInRect(r1, r2: TRect): Boolean;

// Значение в границах
function IsValueBetween(aValue, aLowLimit, aTopLimit: Integer; anIncludeLimits: Boolean): Boolean;

// Вернуть строку заданной длины из случайных символов таблицы
function RandomText(Len: Integer; Table: AnsiString): AnsiString;

// Получить строчное написание числа
function SpellNum(x: Extended): string;

// Получение числа тиков ОС
function GetTickCount: Cardinal;

// Определение типа ОС
type
  TOSType = (osUnknown, osLinux,
    osWin31, osWin95, osWin98, osWinME, osWinNT, osWin2k, osWinXP);

function GetOSType: TOSType;

// Сканирование папки
type
  TFilePrepareProc = procedure (const FileName: string) of object;

procedure ScanDir(const aPath: string; aPrepareProc: TFilePrepareProc;
  aIncludeFolders: Boolean; FileAttrs: Integer; aMask: string = '*.*');

// Пересечение прямоугольников
function IsRectIntersect(aCheckedRect1, aCheckedRect2: TRect): Boolean;

// Проверить, что точка лежит на границе области
function IsBorder(x, y: Integer; r: TRect): Boolean;

// Взвешенный Random-генератор
type
  TWRData = array of record
    id, weight: Integer;
  end;

function wr_Gen(data: TWRData): Integer;

// Линия по методу Брезенхема
type
  TPointProc = procedure(aX, aY: Integer; var aContinue: Boolean; aData: TObject);

function DoLine(x1, y1, x2, y2: Integer; aPointProc: TPointProc; aData: TObject): Boolean;

{$IFDEF OS_LINUX}
function MakeWord(a, b: Byte): Word;
{$ENDIF}

{$IFDEF FREEPASCAL}
// Extracted from Math unit, Copyright(c) by Borland Delphi
type
  TRoundToRange = -37..37;

function SimpleRoundTo(const AValue: Double; const ADigit: TRoundToRange = -2): Double;
{$ENDIF}

// Дистанция
function Distance(X1, Y1, X2, Y2: Integer): Integer; overload;
function Distance(P1, P2: TPoint): Integer; overload;

// Пролистать все элементы в множестве (enums in set)
type
  TForEachProc = procedure(Element: Integer; var Continue: Boolean);

procedure ForEach(const SetVar; DoProc: TForEachProc);

function GetRValue(rgb: Longword): Byte;
function GetGValue(rgb: Longword): Byte;
function GetBValue(rgb: Longword): Byte;
function RGB(r, g, b: Byte): Longword;

// Шанс
function Chance(aPercent: Integer): Boolean;

{================================ Сериализация ================================}

function SReadBoolean(S: TStream): Boolean;
function SReadByte(S: TStream): Byte;
function SReadChar(S: TStream): AnsiChar;
function SReadDouble(S: TStream): Double;
function SReadFileHeader(S: TStream): TFileHeader;
function SReadFileVersion(S: TStream): TFileVersion;
function SReadLongint(S: TStream): Longint;
function SReadShortString(S: TStream): AnsiString;
function SReadSingle(S: TStream): Single;
function SReadString(S: TStream): AnsiString;
function SReadText(S: TStream): AnsiString;
function SReadWord(S: TStream): Word;
procedure SWriteBoolean(S: TStream; B: Boolean);
procedure SWriteByte(S: TStream; B: Byte);
procedure SWriteChar(S: TStream; C: AnsiChar);
procedure SWriteDouble(S: TStream; D: Double);
procedure SWriteFileHeader(S: TStream; FH: TFileHeader);
procedure SWriteFileVersion(S: TStream; FV: TFileVersion);
procedure SWriteLongint(S: TStream; B: Integer);
procedure SWriteShortString(S: TStream; B: AnsiString);
procedure SWriteSingle(S: TStream; V: Single);
procedure SWriteString(S: TStream; B: AnsiString);
procedure SWriteWord(S: TStream; W: Word);

{============================= Функции хэширования ============================}

function HashPJW(const S: string): Longword;
function HashELF(const S: string): Longword;
function HashBKDR(const S: string): Longword;
function HashRS(const S: string): Cardinal;
function HashJS(const S: string): Cardinal;
function HashSDBM(const S: string): Cardinal;
function HashDJB(const S: string): Cardinal;
function HashDEK(const S: string): Cardinal;
function HashAP(const S: string): Cardinal;

function Crc32(crc: Longword; const c: Byte): Longword;
function CrcStr(const Str: string): Longword;

{==============================================================================}

{$IFNDEF FREEPASCAL}
{$IFNDEF DELPHI_NET}
type
  TBSMemSize = Integer;

  TBSBufferedStream = class(TStream)
  private
    FBufCount: TBSMemSize;   {count of valid bytes in buffer}
    FBuffer: PAnsiChar;    {buffer into underlying stream}
    FBufOfs: Longint;      {offset of buffer in underlying stream}
    FBufPos: TBSMemSize;   {current position in buffer}
    FBufSize: TBSMemSize;   {size of buffer}
    FDirty: Boolean;      {has data in buffer been changed?}
    FSize: Longint;      {size of underlying stream}
    FStream: TStream;      {underlying stream}
  protected
    procedure bsSetStream(aValue: TStream);

    procedure bsInitForNewStream; virtual;
    function bsReadChar(var aCh: AnsiChar): Boolean;
    procedure bsReadFromStream;
    procedure bsWriteToStream;

    {$IFDEF DELPHI3}
    procedure SetSize(NewSize: Longint); override;
    {$ENDIF}
  public
    constructor Create(aStream: TStream);
    constructor CreateEmpty;
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    {$IFDEF DELPHI4UP}
    procedure SetSize(NewSize: Longint); override;
    {$ENDIF}

    property FastSize: Longint read FSize;
    property Stream: TStream read FStream write bsSetStream;
  end;
{$ENDIF}
{$ENDIF}

const
  DEFAULT_INTERVAL = 1000;

type
  TBSTimer = class;

  {$IFNDEF DELPHI_NET}
  TBSTimerThread = class(TThread)
  private
    FTimer: TBSTimer;
  protected
    procedure Execute; override;
  end;
  {$ENDIF}

  TBSTimer = class(TComponent)
  private
    FEnabled: Boolean;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;
    {$IFNDEF DELPHI_NET}
    FTimerThread: TBSTimerThread;
    {$ELSE}
    FTimer: System.Threading.Timer;
    {$ENDIF}

    {$IFNDEF DELPHI_NET}
    procedure DoTimer;
    {$ELSE}
    procedure TimerCallback(sender: System.Object);
    {$ENDIF}
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Interval: Cardinal read FInterval write SetInterval default DEFAULT_INTERVAL;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

{==============================================================================}

type
  TBSObjectList = class(TList)
  private
    FOwnsObjects: Boolean;
  protected
    {$IFNDEF DELPHI_NET}
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    {$ELSE}
    procedure Notify(Instance: TObject; Action: TListNotification); override;
    {$ENDIF}
    function GetItem(Index: Integer): TObject;
    procedure SetItem(Index: Integer; AObject: TObject);
  public
    constructor Create; overload;
    constructor Create(AOwnsObjects: Boolean); overload;

    function Add(AObject: TObject): Integer;
    function Extract(Item: TObject): TObject;
    function Remove(AObject: TObject): Integer;
    function IndexOf(AObject: TObject): Integer;
    procedure Insert(Index: Integer; AObject: TObject);
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
  end;

{==============================================================================}

type
  TBSSerializableObjectClass = class of TBSSerializableObject;

  TBSSerializableObject = class(TObject)
  private
    FOwner: TObject;
  protected
    function GetSerializeKind: Byte; virtual;
  public
    constructor Create(aOwner: TObject); virtual;

    procedure LoadFromStream(aStream: TStream; aVersion: TFileVersion); virtual;
    procedure SaveToStream(aStream: TStream; aVersion: TFileVersion); virtual;

    property Owner: TObject read FOwner write FOwner;
  end;

function GetSerializableClass(aKind: Byte): TBSSerializableObjectClass;
procedure RegisterSerializable(aKind: Byte; aClass: TBSSerializableObjectClass);

{================================ Хэш-таблица =================================}

type
  THashCode = Word;

const
  HASH_SIZE = High(THashCode);

type
  THashKind = (
    hkQuick,    { неплохо, но совпадения есть [7] }
    hkPJW,      { качество неплохое, совпадений немного [35] }
    hkELF,      { самый популярный, судя по аналогам (модификация PJW) [35] }
    hkBKDR,     { совпадений нет, но на больших объемах наверное будет хуже [0] }
    hkCRC32,    { очень неплохо! всего одно совпадение! [1] }
    hkRS,
    hkJS,
    hkSDBM,
    hkDJB,
    hkDEK,
    hkAP
  );

  TBSHashEntry = class(TBSSerializableObject)
  private
    FSign: string;
  protected
    Next: TBSHashEntry;
  public
    constructor Create(aOwner: TObject); override;

    property Sign: string read FSign write FSign;
  end;

  TBSHashEntryClass = class of TBSHashEntry;

  THTEnumProc = procedure (Entry: TBSHashEntry; Index: Longint; var Continue: Boolean) of object;

  TBSHashTable = class(TBSSerializableObject)
  private
    FCount: Integer;
    FEntryClass: TBSHashEntryClass;
    FFindLastCode: THashCode;
    FFindLastEntry: TBSHashEntry;
    FKind: THashKind;
    FTable: array [THashCode] of TBSHashEntry;

    function Hash(aSign: string): THashCode;
    function GetEntryByIndex(aIndex: Integer): TBSHashEntry;
    function GetEntryBySign(const aSign: string; aCanCreate: Boolean): TBSHashEntry;
  public
    constructor Create(aOwner: TObject); overload; override;
    constructor Create(aOwner: TObject; aEntryClass: TBSHashEntryClass; aKind: THashKind = hkELF); overload;
    destructor Destroy; override;

    function  AddEntry(const aSign: string): TBSHashEntry; overload;
    function  AddEntry(const aSign: string; aEntryClass: TBSHashEntryClass): TBSHashEntry; overload;
    procedure Clear;
    procedure DeleteEntry(const aSign: string);
    procedure Enumerate(EnumProc: THTEnumProc);
    function  FindEntry(const aSign: string): TBSHashEntry;
    function  FindFirst: TBSHashEntry;
    function  FindNext: TBSHashEntry;
    procedure LoadFromStream(aStream: TStream; aVersion: TFileVersion); override;
    procedure PutEntry(aEntry: TBSHashEntry);
    procedure SaveToStream(aStream: TStream; aVersion: TFileVersion); override;

    property Entries[Index: Integer]: TBSHashEntry read GetEntryByIndex;
    property EntriesCount: Integer read FCount;
  end;

{==============================================================================}

implementation

resourcestring
  // TBSBufferedStream
  SNilStream            = 'Buffered/text stream: Attempted to read, write, or seek and underlying stream is nil';
  SNoSeekForRead        = 'Buffered/text stream: Could not seek to the correct position in the underlying stream (for read request)';
  SNoSeekForWrite       = 'Buffered/text stream: Could not seek to the correct position in the underlying stream (for write request)';
  SCannotWrite          = 'Buffered/text stream: Could not write the entire buffer to the underlying stream';
  SBadOrigin            = 'Bad origin parameter for call to Seek';
  // GetSerializableClass
  SObjectKindUnknown    = 'Object kind unknown (%d)';
  // TBSHashTable
  SDuplicateExists      = 'Duplicate exists (%s)';

{$IFNDEF DELPHI_NET}
function LoadLibrary(ModuleName: PChar): HMODULE;
begin
  {$IFDEF OS_MSWIN}
  Result := Windows.LoadLibrary(ModuleName);
  {$ENDIF}
  {$IFDEF OS_LINUX}
  Result := SysUtils.LoadLibrary(ModuleName);
  {$ENDIF}
end;

function FreeLibrary(Module: HMODULE): LongBool;
begin
  {$IFDEF OS_MSWIN}
  Result := Windows.FreeLibrary(Module);
  {$ENDIF}
  {$IFDEF OS_LINUX}
  Result := SysUtils.FreeLibrary(Module);
  {$ENDIF}
end;

function GetProcAddress(Module: HMODULE; Proc: PChar): Pointer;
begin
  {$IFDEF OS_MSWIN}
  Result := Windows.GetProcAddress(Module, Proc);
  {$ENDIF}
  {$IFDEF OS_LINUX}
  Result := SysUtils.GetProcAddress(Module, Proc);
  {$ENDIF}
end;
{$ENDIF}

// Получить сигнатуру числа (знак)
function Sgn(aValue: Longint): Shortint;
begin
  if (aValue > 0)
  then Result := +1
  else
  if (aValue < 0)
  then Result := -1
  else Result := 0;
end;

// Определить корректность целого числа
function IsValidInt(const Number: string): Boolean;
const
  Nums: TCharSet = ['0'..'9', '+', '-'];
var
  i: Integer;
begin
  Result := False;

  if (Length(Number) = 0) then Exit;

  for i := 1 to Length(Number) do
    if not(Number[i] in Nums)
    then Exit;

  Result := True;
end;

// Определить корректность дробного числа
function IsValidFloat(const Number: string): Boolean;
const
  Nums: TCharSet = ['0'..'9', ',', '.', '+', '-', 'e', 'E'];
var
  i: Integer;
begin
  Result := False;

  if (Length(Number) = 0) then Exit;

  for i := 1 to Length(Number) do
    if not(Number[i] in Nums)
    then Exit;

  Result := True;
end;

// Транслитерация
type
  TRusCharSet = 1..33;

const
  TranslitTable: array [TRusCharSet, TScheme] of string[4] = (
    ('А',    'A',  'A',  'A',  'A',   'A'),
    ('Б',    'B',  'B',  'B',  'B',   'B'),
    ('В',    'V',  'V',  'V',  'V',   'V'),
    ('Г',    'G',  'G',  'G',  'G',   'G'),
    ('Д',    'D',  'D',  'D',  'D',   'D'),
    ('Е',    'E',  'E',  'E',  'E',   'E'),
    ('Ё',   'JO', 'JO', 'Y`', 'JO',  'JO'),
    ('Ж',   'ZH', 'ZH',  'J', 'ZH',  'ZH'),
    ('З',    'Z',  'Z',  'Z',  'Z',   'Z'),
    ('И',    'I',  'I',  'I',  'I',   'I'),
    ('Й',    'J', 'JI', 'I`', 'JJ',   'J'),
    ('К',    'K',  'K',  'K',  'K',   'K'),
    ('Л',    'L',  'L',  'L',  'L',   'L'),
    ('М',    'M',  'M',  'M',  'M',   'M'),
    ('Н',    'N',  'N',  'N',  'N',   'N'),
    ('О',    'O',  'O',  'O',  'O',   'O'),
    ('П',    'P',  'P',  'P',  'P',   'P'),
    ('Р',    'R',  'R',  'R',  'R',   'R'),
    ('С',    'S',  'S',  'S',  'S',   'S'),
    ('Т',    'T',  'T',  'T',  'T',   'T'),
    ('У',    'U',  'U',  'U',  'U',   'U'),
    ('Ф',    'F',  'F',  'F',  'F',   'F'),
    ('Х',   'KH', 'KH',  'H', 'KH',   'X'),
    ('Ц',    'C',  'C',  'C',  'C',   'C'),
    ('Ч',   'CH', 'CH', 'C`', 'CH',  'CH'),
    ('Ш',   'SH', 'SH', 'S`', 'SH',  'SH'),
    ('Щ', 'SHCH',  'W', 'H`', 'HH', 'SCH'),
    ('Ъ',    '"',  'X', 'X`', '``',   '`'),
    ('Ы',    'Y',  'Y',  'Y',  'Y',   'Y'),
    ('Ь',   '''',  'Q',  'X',  '`',  ''''),
    ('Э',   'EH', 'EH', 'E`', 'EH',   'E'),
    ('Ю',   'JU', 'JU', 'U`', 'JU',  'YU'),
    ('Я',   'JA', 'JA', 'A`', 'JA',  'YA')
  );

  RusUpperChars: set of Char = ['А'..'Я', 'Ё'];
  RusLowerChars: set of Char = ['а'..'я', 'ё'];

  EngUpperChars: set of Char = ['A'..'Z'];
  EngLowerChars: set of Char = ['a'..'z'];

function Transliterate(s, t: TScheme; str: string): string;
var
  i, idx, dCnt: Integer;
  src, tgt, tmp: string;
  order: array [1..33] of Integer;
  nextLower: Boolean;

  procedure Order_Prepare;
  var
    i, k, t: Integer;
  begin
    for i := 1 to 33 do order[i] := i;

    for i := 1 to 33 do
      for k := i + 1 to 33 do
        if Length(TranslitTable[k, s]) > Length(TranslitTable[i, s])
        then begin
          t := order[i];
          order[i] := order[k];
          order[k] := t;
        end;
  end;

  function FindChar(ch: Char; var idx: Integer): Boolean;
  var
    i: Integer;
  begin
    idx := -1;
    Result := False;
    for i := 1 to 33 do
      if TranslitTable[i, ts_Russian] = ch then begin
        Result := True;
        idx := i;
        Break;
      end;
  end;

  function FindSymbol(tStr: string; var idx: Integer): Boolean;
  var
    i, k: Integer;
  begin
    Result := False;
    for i := 1 to 33 do begin
      k := order[i];
      if Pos(TranslitTable[k, s], tStr) = 1 then begin
        idx := k;
        Result := True;
        Break;
      end;
    end;
  end;

begin
  Result := '';
  if (s = ts_Russian) then begin
    nextLower := False;
    
    for i := 1 to Length(str) do begin
      src := str[i];

      if (i+1 <= Length(str)) then nextLower := str[i+1] in RusLowerChars;

      tmp := AnsiUpperCase(src);
      if FindChar(tmp[1], idx) then begin
        tgt := TranslitTable[idx, t];
        if (src[1] in RusUpperChars) and (Length(tgt) > 1) then begin
          if nextLower then begin
            tgt := AnsiLowerCase(tgt);
            tgt[1] := AnsiUpperCase(tgt[1])[1];
          end;
        end else if (src[1] in RusLowerChars) then begin
          tgt := AnsiLowerCase(tgt);
        end;
      end else tgt := src;
      Result := Result + tgt;
    end;
  end else begin
    Order_Prepare;
    tmp := AnsiUpperCase(str);
    while (str <> '') do begin
      if FindSymbol(tmp, idx) then begin
        dCnt := Length(TranslitTable[idx, s]);
        src := Copy(str, 1, dCnt);
        tgt := TranslitTable[idx, t];
        if (src[1] in EngLowerChars)
        then tgt := AnsiLowerCase(tgt);
        Result := Result + tgt;
      end else begin
        dCnt := 1;
        Result := Result + str[1];
      end;
      Delete(tmp, 1, dCnt);
      Delete(str, 1, dCnt);
    end;
  end;
end;

// Наличие параметра командной строки
function HasParam(aName: string): Boolean;
var
  i: Integer;
  p: string;
begin
  Result := False;

  for i := 1 to ParamCount do begin
    p := AnsiLowerCase(ParamStr(i));

    if (p[1] = '-') or (p[1] = '/')
    then Delete(p, 1, 1);

    if (p = aName) then begin
      Result := True;
      Break;
    end;
  end;
end;

// Поменять числа местами
procedure SwapValues(var aValue1: Integer; var aValue2: Integer);
var
  temp: Integer;
begin
  temp := aValue1;
  aValue1 := aValue2;
  aValue2 := temp;
end;

// Случайное число
function RealRnd(HighBound: Integer): Single;
// Диапазон HighBound = 1..9
var
  rTemp: Single;
begin
  rTemp := Random(HighBound * 10);
  Result := rTemp / 10;
end;

// Точка в прямоугольнике
function PointInRect(ptX, ptY: Integer; R: TRect): Boolean;
begin
  Result :=
    (ptX >= R.Left) and (ptX <= R.Right)
    and (ptY >= R.Top) and (ptY <= R.Bottom);
end;

// Равенство точек
function EqualPoints(p1, p2: TPoint): Boolean;
begin
  Result := (p1.X = p2.X) and (p1.Y = p2.Y);
end;

// Случайное число в диапазоне
function BoundedRnd(LB, HB: Integer): Integer;
begin
  // Схватка двух якадзун
  if (LB > HB) then SwapValues(LB, HB);
  Result := LB + Random((HB - LB) + 1);
end;

// Сместить границы прямоугольника
procedure OffsetRect(var R: TRect; dX, dY: Integer);
begin
  R.Left := R.Left - dX;
  R.Top := R.Top - dY;
  R.Right := R.Right + dX;
  R.Bottom := R.Bottom + dY;
end;

// Прямоугольник в прямоугольнике
function RectInRect(r1, r2: TRect): Boolean;
begin
  Result := PointInRect(r1.Left, r1.Top, r2) and
            PointInRect(r1.Right, r1.Top, r2) and
            PointInRect(r1.Left, r1.Bottom, r2) and
            PointInRect(r1.Right, r1.Bottom, r2);
end;

// Значение в границах
function IsValueBetween(aValue, aLowLimit, aTopLimit: Integer; anIncludeLimits: Boolean): Boolean;
begin
  // Схватка двух якадзун
  if (aLowLimit > aTopLimit)
  then SwapValues(aLowLimit, aTopLimit);

  if not anIncludeLimits then begin
    Inc(aLowLimit);
    Dec(aTopLimit);
  end;
  Result := ((aValue >= aLowLimit) and (aValue <= aTopLimit));
end;

// Вернуть строку заданной длины из случайных символов таблицы
function RandomText(Len: Integer; Table: AnsiString): AnsiString;
var
  R: Integer;
begin
  SetLength(Result, 0);
  R := Length(Table);
  if (Len > 0) and (R > 0) then begin
    Randomize;
    SetLength(Result, Len);
    while (Len > 0) do begin
      Result[Len] := Table[1 + Random(R)];
      Dec(Len);
    end;
  end;
end;

const
  hundrs: array [0..9] of string[10] = (
        '', 'сто ', 'двести ', 'триста ', 'четыреста ', 'пятьсот ',
        'шестьсот ', 'семьсот ', 'восемьсот ', 'девятьсот ' );

  tens: array [2..9] of string[12] = (
        'двадцать ', 'тридцать ', 'сорок ', 'пятьдесят ',
        'шестьдесят ', 'семьдесят ', 'восемьдесят ', 'девяносто ' );

  {$J+}
  ones: array [0..19] of string[20] = (
        '', 'один ', 'два ', 'три ', 'четыре ', 'пять ', 'шесть ',
        'семь ', 'восемь ', 'девять ', 'десять ', 'одиннадцать ',
        'двенадцать ', 'тринадцать ', 'четырнадцать ', 'пятнадцать ',
        'шестнадцать ', 'семнадцать ', 'восемнадцать ', 'девятнадцать ' );
  {$J-}

  onetwo: array [0..1, 1..2] of string[5] = (
        ('один ', 'два '), ('одна ', 'две ') );

  abbrs: array [0..3, 1..5] of string[12] = (
         ('миллиарда ', 'миллиард ', 'миллиардов ', 'млрд. ', ''),
         ('миллиона ', 'миллион ', 'миллионов ', 'млн. ', ''),
         ('тысячи ', 'тысяча ', 'тысяч ', 'тыс. ', ''),
         ('', '', '', '', '') );

function SpellNum(x: Extended): string;
var
  S: string;
  N: string;
  i, j, z: Integer;
  x1, x2, x3: Byte;
begin
  S := '';
  if (x < 0) then begin
    x := -x;
    S := 'минус ';
  end;

  Str(x:12:0, N);
  i := 1;

  while (N[i] = ' ') do begin
    N[i] := '0';
    inc(i);
  end;

  for j := 0 to 3 do begin
    ones[1] := onetwo[Ord(j >= 2), 1];
    ones[2] := onetwo[Ord(j >= 2), 2];

    //i := Trans1000(S, N, j * 3 + 1);
    //function Trans1000(var S, N: string; z: Integer): Integer;
    z := j * 3 + 1;
    x1 := Ord(N[z  ]) - 48;
    x2 := Ord(N[z+1]) - 48;
    x3 := Ord(N[z+2]) - 48;

    if (x1 + x2 + x3 = 0) then begin
      if (z = 10) and (N = '000000000000')
      then S := S + 'ноль ';

      i := 5;
    end else begin
      S := S + hundrs[x1];

      if (x2 < 2)
      then inc(x3, 10 * x2)
      else S := S + tens[x2];

      S := S + ones[x3];

      if (x3 > 4) or (x3 = 0)
      then i := 3
      else i := 1 + Ord(x3 = 1);
    end;
    //

    S := S + abbrs[j, i];
  end;

  i := Length(S);

  while (i > 0) and (S[i] = ' ') do begin
    SetLength(s, Length(s) - 1);
    Dec(i);
  end;

  Result := S;
end;

// Получение числа тиков ОС
function GetTickCount: Cardinal;
{$IFDEF OS_LINUX}
var
  tv: timeval;
  {Stamp: TTimeStamp;}
{$ENDIF}
begin
  {$IFDEF OS_LINUX}
  {Stamp := DateTimeToTimeStamp(Now);
  Result := Stamp.Time;}
  gettimeofday(tv, nil);
  {$RANGECHECKS OFF}
  Result := int64(tv.tv_sec) * 1000 + tv.tv_usec div 1000;
  {$ENDIF}

  {$IFDEF OS_MSWIN}
    {$IFNDEF DELPHI_NET}
    Result := Windows.GetTickCount;
    {$ELSE}
    Result := System.Environment.TickCount;
    {$ENDIF}
  {$ENDIF}
end;

// Определение типа ОС
function GetOSType: TOSType;
{$IFDEF OS_MSWIN}
{$IFNDEF DELPHI_NET}
var
  lpVersionInformation: TOSVersionInfo;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF OS_MSWIN}
  {$IFNDEF DELPHI_NET}
  Result := osUnknown;

  lpVersionInformation.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if (getVersionEx(lpVersionInformation)) then begin
    case (lpVersionInformation.dwPlatformId) of
      VER_PLATFORM_WIN32_WINDOWS: begin
        Result := osWin95;
        if (lpVersionInformation.dwMinorVersion >= 10) then Result := osWin98;
        if (lpVersionInformation.dwMinorVersion >= 90) then Result := osWinME;
      end;

      VER_PLATFORM_WIN32_NT: begin
        if (lpVersionInformation.dwMajorVersion <= 4) then Result := osWinNT
        else
        if (lpVersionInformation.dwMajorVersion = 5) then begin
          Result := osWin2k;
          if (lpVersionInformation.dwMinorVersion >= 1) then Result := osWinXP;
        end;
      end;

      VER_PLATFORM_WIN32s: begin
        Result := osWin31;
      end;
    end;
  end;
  {$ELSE}
  Result := osWinNT;
  {$ENDIF}
  {$ENDIF}

  {$IFDEF OS_LINUX}
  Result := osLinux;
  {$ENDIF}
end;

// сканирование папки
procedure ScanDir(const aPath: string; aPrepareProc: TFilePrepareProc;
  aIncludeFolders: Boolean; FileAttrs: Integer; aMask: string = '*.*');
var
  sr: TSearchRec;
  res: Integer;
  newf: string;
begin
  res := FindFirst(aPath + PathDelim + aMask, FileAttrs, sr);
  while res = 0 do begin
    if (sr.Name <> '.') and (sr.Name <> '..') then begin
      newf := aPath + PathDelim + sr.Name;

      if ((sr.Attr and faDirectory) = faDirectory)
      then begin
        if aIncludeFolders
        then ScanDir(newf, aPrepareProc, aIncludeFolders, FileAttrs)
      end else aPrepareProc(newf);
    end;

    res := FindNext(sr);
  end;
  FindClose(sr);
end;

// Ага. Надо функцию, которая бы определяла, пересекаются ли две
// прямоугольные области... Пошарили в стандартных модулях от
// Delphi и, решив, соптимизировать мальца функцию IntersectRect(),
// а заодно и размер исполняемого модуля конечного приложения,
// нагло содрали код из сырцов к Delph'е.
function IsRectIntersect(aCheckedRect1, aCheckedRect2: TRect): Boolean;
var
  r: TRect;
begin
  r := aCheckedRect1;
  if (aCheckedRect2.Left > aCheckedRect1.Left) then r.Left := aCheckedRect2.Left;
  if (aCheckedRect2.Top > aCheckedRect1.Top) then r.Top := aCheckedRect2.Top;
  if (aCheckedRect2.Right < aCheckedRect1.Right) then r.Right := aCheckedRect2.Right;
  if (aCheckedRect2.Bottom < aCheckedRect1.Bottom) then r.Bottom := aCheckedRect2.Bottom;
  Result := not IsRectEmpty(r);
end;

// Проверить, что точка лежит на границе области
function IsBorder(x, y: Integer; r: TRect): Boolean;
begin
  Result :=
    (((x >= r.Left) and (x <= r.Right)) and (y = r.Top))
    or (((x >= r.Left) and (x <= r.Right)) and (y = r.Bottom))
    or (((y >= r.Top) and (y <= r.Bottom)) and (x = r.Left))
    or (((y >= r.Top) and (y <= r.Bottom)) and (x = r.Right));
end;

// Взвешенный Random-генератор
function wr_Gen(data: TWRData): Integer;
var
  idx, cnt, oCnt, i, j: Integer;
  iData: array of Integer;
begin
  cnt := Length(data);

  oCnt := 0;
  for i := 0 to cnt - 1 do oCnt := oCnt + data[i].weight;
  SetLength(iData, oCnt);

  idx := 0;
  for i := 0 to cnt - 1 do begin
    for j := idx to (idx + data[i].weight - 1) do
      iData[j] := data[i].id;
    idx := idx + data[i].weight;
  end;

  idx := BoundedRnd(0, oCnt - 1);
  Result := iData[idx];

  {$IFNDEF DELPHI_NET}
  Finalize(iData);
  {$ELSE}
  iData := nil;
  {$ENDIF}
end;

// Линия по методу Брезенхема
function DoLine(x1, y1, x2, y2: Integer; aPointProc: TPointProc; aData: TObject): Boolean;
var
  dX, dY, XInc, YInc, S, dX2, dY2, dXY, X, Y, i: Integer;
begin
  Result := True;

  dX := Abs(x1 - x2);
  dY := Abs(y1 - y2);

  dX2 := dX shl 1{* 2};
  dY2 := dY shl 1{* 2};

  if (x1 < x2)
  then XInc := +1
  else XInc := -1;

  if (y1 < y2)
  then YInc := +1
  else YInc := -1;

  X := x1;
  Y := y1;

  aPointProc(X, Y, Result, aData);
  if not Result then Exit;

  if (dX > dY) then begin
    { x изменяется быстрее, чем y }
    S := dY2 - dX;
    dXY := dY2 - dX2;

    for i := 1 to dX do begin
      if (S >= 0) then begin
        Y := Y + YInc;
        S := S + dXY;
      end else S := S + dY2;

      X := X + XInc;

      aPointProc(X, Y, Result, aData);
      if not Result then Exit;
    end;
  end else begin
    { y изменяется быстрее, чем x }
    S := dX2 - dY;
    dXY := dX2 - dY2;

    for i := 1 to dY do begin
      if (S >= 0) then begin
        X := X + XInc;
        S := S + dXY;
      end else S := S + dX2;

      Y := Y + YInc;

      aPointProc(X, Y, Result, aData);
      if not Result then Exit;
    end;
  end;
end;

{$IFDEF OS_LINUX}
function MakeWord(a, b: Byte): Word;
begin
  Result := a or b shl 8;
end;
{$ENDIF}

{$IFDEF FREEPASCAL}

function IntPower(const Base: Extended; const Exponent: Integer): Extended;
asm
        mov     ecx, eax
        cdq
        fld1                      { Result := 1 }
        xor     eax, edx
        sub     eax, edx          { eax := Abs(Exponent) }
        jz      @@3
        fld     Base
        jmp     @@2
@@1:    fmul    ST, ST            { X := Base * Base }
@@2:    shr     eax,1
        jnc     @@1
        fmul    ST(1),ST          { Result := Result * X }
        jnz     @@1
        fstp    st                { pop X from FPU stack }
        cmp     ecx, 0
        jge     @@3
        fld1
        fdivrp                    { Result := 1 / Result }
@@3:
        fwait
end;

function SimpleRoundTo(const AValue: Double; const ADigit: TRoundToRange = -2): Double;
var
  LFactor: Double;
begin
  LFactor := IntPower(10, ADigit);
  Result := Trunc((AValue / LFactor) + 0.5) * LFactor;
end;

{$ENDIF}

// Дистанция
function Distance(X1, Y1, X2, Y2: Integer): Integer; overload;
var
  dX, dY: Integer;
begin
  dX := X2 - X1;
  dY := Y2 - Y1;
  Result := Round(Sqrt((dX * dX) + (dY * dY)));
end;

function Distance(P1, P2: TPoint): Integer; overload;
begin
  Result := Distance(P1.X, P1.Y, P2.X, P2.Y);
end;

// Пролистать все элементы в множестве (enums in set)
procedure ForEach(const SetVar; DoProc: TForEachProc);
var
  continue: Boolean;
  FSet, i: Byte;
begin
  FSet := Byte(SetVar);
  continue := True;
  for i := Low(FSet) to High(FSet) do begin
    if (FSet and 1) = 1 then begin
      if Assigned(DoProc) then DoProc(i, continue);
      if not continue then Break;
    end;

    FSet := FSet shr 1;
  end;
end;

{$IFNDEF SUPPORTS_RGB}

function GetRValue(rgb: Longword): Byte;
begin
  Result := Byte(rgb);
end;

function GetGValue(rgb: Longword): Byte;
begin
  Result := Byte(rgb shr 8);
end;

function GetBValue(rgb: Longword): Byte;
begin
  Result := Byte(rgb shr 16);
end;

function RGB(r, g, b: Byte): Longword;
begin
  Result := (r or (g shl 8) or (b shl 16));
end;

{$ELSE}

function GetRValue(rgb: Longword): Byte;
begin
  Result := Windows.GetRValue(rgb);
end;

function GetGValue(rgb: Longword): Byte;
begin
  Result := Windows.GetGValue(rgb);
end;

function GetBValue(rgb: Longword): Byte;
begin
  Result := Windows.GetBValue(rgb);
end;

function RGB(r, g, b: Byte): Longword;
begin
  Result := Windows.RGB(r, g, b);
end;

{$ENDIF}

// Шанс
function Chance(aPercent: Integer): Boolean;
begin
  Result := (Random(101) < aPercent);
end;

{================================ Сериализация ================================}

function SReadBoolean(S: TStream): Boolean;
begin
  Result := Boolean(SReadByte(S));
end;

function SReadByte(S: TStream): Byte;
begin
  S.Read(Result{$IFNDEF DELPHI_NET}, SizeOf(Byte){$ENDIF});
end;

function SReadChar(S: TStream): AnsiChar;
begin
  S.Read(Result{$IFNDEF DELPHI_NET}, SizeOf(AnsiChar){$ENDIF});
end;

function SReadDouble(S: TStream): Double;
begin
  S.Read(Result{$IFNDEF DELPHI_NET}, SizeOf(Double){$ENDIF});
end;

function SReadFileHeader(S: TStream): TFileHeader;
var
  i: Byte;
begin
  for i := 0 to 2 do Result.Sign[i] := SReadChar(S);
  Result.Version := SReadFileVersion(S);
end;

function SReadFileVersion(S: TStream): TFileVersion;
begin
  Result.Release := SReadWord(S);
  Result.Revision := SReadWord(S);
end;

function SReadLongint(S: TStream): Longint;
begin
  S.Read(Result{$IFNDEF DELPHI_NET}, SizeOf(Longint){$ENDIF});
end;

function SReadShortString(S: TStream): AnsiString;
var
  sLen: Byte;
  {$IFDEF DELPHI_NET}
  i: Byte;
  c: AnsiChar;
  {$ENDIF}
begin
  sLen := SReadByte(S);
  SetLength(Result, sLen);
  {$IFNDEF DELPHI_NET}
  {$R-}
  S.Read(Result[1], sLen);
  {$R+}
  {$ELSE}
  for i := 1 to sLen do begin
    S.Read(c);
    Result[i] := c;
  end;
  {$ENDIF}
end;

function SReadSingle(S: TStream): Single;
begin
  S.Read(Result{$IFNDEF DELPHI_NET}, SizeOf(Single){$ENDIF});
end;

function SReadString(S: TStream): AnsiString;
var
  sLen: Longint;
  {$IFDEF DELPHI_NET}
  i: Longint;
  c: AnsiChar;
  {$ENDIF}
begin
  sLen := SReadLongint(S);
  SetLength(Result, sLen);
  {$IFNDEF DELPHI_NET}
  {$R-}
  S.Read(Result[1], sLen);
  {$R+}
  {$ELSE}
  for i := 1 to sLen do begin
    S.Read(c);
    Result[i] := c;
  end;
  {$ENDIF}
end;

function SReadText(S: TStream): AnsiString;
var
  sLen: Longint;
  {$IFDEF DELPHI_NET}
  i: Longint;
  c: AnsiChar;
  {$ENDIF}
begin
  sLen := S.Size;
  SetLength(Result, sLen);
  {$IFNDEF DELPHI_NET}
  {$R-}
  S.Read(Result[1], sLen);
  {$R+}
  {$ELSE}
  for i := 1 to sLen do begin
    S.Read(c);
    Result[i] := c;
  end;
  {$ENDIF}
end;

function SReadWord(S: TStream): Word;
begin
  S.Read(Result{$IFNDEF DELPHI_NET}, SizeOf(Word){$ENDIF});
end;

procedure SWriteBoolean(S: TStream; B: Boolean);
begin
  SWriteByte(S, Byte(B));
end;

procedure SWriteByte(S: TStream; B: Byte);
begin
  S.Write(B, SizeOf(Byte));
end;

procedure SWriteChar(S: TStream; C: AnsiChar);
begin
  S.Write(C{$IFNDEF DELPHI_NET}, SizeOf(AnsiChar){$ENDIF});
end;

procedure SWriteDouble(S: TStream; D: Double);
begin
  S.Write(D{$IFNDEF DELPHI_NET}, SizeOf(Double){$ENDIF});
end;

procedure SWriteFileHeader(S: TStream; FH: TFileHeader);
var
  i: Byte;
begin
  for i := 0 to 2 do SWriteChar(S, FH.Sign[i]);
  SWriteFileVersion(S, FH.Version);
end;

procedure SWriteFileVersion(S: TStream; FV: TFileVersion);
begin
  SWriteWord(S, FV.Release);
  SWriteWord(S, FV.Revision);
end;

procedure SWriteLongint(S: TStream; B: Longint);
begin
  S.Write(B{$IFNDEF DELPHI_NET}, SizeOf(Longint){$ENDIF});
end;

procedure SWriteShortString(S: TStream; B: AnsiString);
var
  sLen: Byte;
  {$IFDEF DELPHI_NET}
  i: Byte;
  {$ENDIF}
begin
  sLen := Length(B);
  SWriteByte(S, sLen);
  {$IFNDEF DELPHI_NET}
  {$R-}
  S.Write(B[1], sLen);
  {$R+}
  {$ELSE}
  for i := 1 to sLen do S.Write(B[i]);
  {$ENDIF}
end;

procedure SWriteSingle(S: TStream; V: Single);
begin
  S.Write(V{$IFNDEF DELPHI_NET}, SizeOf(V){$ENDIF});
end;

procedure SWriteString(S: TStream; B: AnsiString);
var
  sLen: Longint;
  {$IFDEF DELPHI_NET}
  i: Longint;
  {$ENDIF}
begin
  sLen := Length(B);
  SWriteLongint(S, sLen);
  {$IFNDEF DELPHI_NET}
  {$R-}
  S.Write(B[1], sLen);
  {$R+}
  {$ELSE}
  for i := 1 to sLen do S.Write(B[i]);
  {$ENDIF}
end;

procedure SWriteWord(S: TStream; W: Word);
begin
  S.Write(W{$IFNDEF DELPHI_NET}, SizeOf(Word){$ENDIF});
end;

{============================= Функции хэширования ============================}

function HashPJW(const S: string): Cardinal;
{ P. J. Weinberger функция описана в "Practical Algorithms For
  Programmers" by Andrew Binstock and John Rex, Addison Wesley}
var
  i: Longword;
  G: Longword;
begin
  Result := 0;
  for i := 1 to Length(S) do begin
    Result := (Result shl 4) + Byte(S[i]);
    G := Result and $F0000000;
    if (G <> 0) then Result := (Result xor (G shr 24)) and not $F0000000;
  end;
end;

function HashELF(const S: string): Cardinal;
{ эта функция описана в "Practical Algorithms For
  Programmers" by Andrew Binstock and John Rex, Addison Wesley,
  with modifications in Dr Dobbs Journal, April 1996}
var
  G: Longword;
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do begin
    Result := (Result shl 4) + Byte(S[i]);
    G := Result and $F0000000;
    if (G <> 0) then Result := Result xor (G shr 24);
    Result := Result and (not G);
  end;
end;

function HashBKDR(const S: string): Cardinal;
{ эта функция описана в "The C Programming Language"
  by Brian Kernighan and Donald Ritchie, Prentice Hall}
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    Result := (Result * 31) + Byte(S[i]);
end;

function HashRS(const S: string): Cardinal;
const
  b = 378551;
var
  a: Cardinal;
  i: Integer;
begin
  a := 63689;
  Result := 0;

  for i := 1 to Length(S) do begin
    Result := Result * a + Ord(S[i]);
    a := a * b;
  end;
end;

function HashJS(const S: string): Cardinal;
var
  i: Integer;
begin
  Result := 1315423911;

  for i := 1 to Length(S) do begin
    Result := Result xor ((Result shl 5) + Ord(S[i]) + (Result shr 2));
  end;
end;

function HashSDBM(const S: string): Cardinal;
var
  i: Cardinal;
begin
  Result := 0;

  for i := 1 to Length(S) do begin
    Result := Ord(S[i]) + (Result shl 6) + (Result shl 16) - Result;
  end;
end;

function HashDJB(const S: string): Cardinal;
var
  i: Cardinal;
begin
  Result := 5381;

  for i := 1 to Length(S) do begin
    Result := ((Result shl 5) + Result) + Ord(S[i]);
  end;
end;

function HashDEK(const S: string): Cardinal;
var
  i: Cardinal;
begin
  Result := Length(S);

  for i := 1 to Length(S) do begin
    Result := ((Result shr 5) xor (Result shl 27)) xor Ord(S[i]);
  end;
end;

function HashAP(const S: string): Cardinal;
var
  i: Cardinal;
begin
  Result := 0;

  for i := 1 to Length(S) do begin
    if ((i - 1) and 1) = 0
    then Result := Result xor ((Result shl 7) xor Ord(S[i]) xor (Result shr 3))
    else Result := Result xor (not((Result shl 11) xor Ord(S[i]) xor (Result shr 5)));
  end;
end;

var
  Ccitt32Table: array [0..255] of Longword;

function CrcStr(const Str: string): Longword;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Str) do
    Result := Crc32(Result, Byte(Str[i]));
end;

function Crc32(crc: Longword; const c: Byte): Longword;
begin
  Result := (((crc shr 8) and $00FFFFFF) xor (Ccitt32Table[(crc xor c) and $FF]));
end;

procedure BuildCRCTable;
const
  CRC32_POLYNOMIAL = $EDB88320;
var
  i, j, value: Longword;
begin
  for i := 0 to 255 do begin
    value := i;
    for j := 8 downto 1 do begin
      if ((value and 1) <> 0)
      then value := (value shr 1) xor CRC32_POLYNOMIAL
      else value := value shr 1;
    end;
    Ccitt32Table[i] := value;
  end;
end;

{==============================================================================}

{$IFNDEF FREEPASCAL}
{$IFNDEF DELPHI_NET}

function MinLong(A, B: Longint): Longint;
begin
  if A < B
  then Result := A
  else Result := B;
end;

{ TBSBufferedStream }

constructor TBSBufferedStream.Create(aStream: TStream);
begin
  inherited Create;

  {allocate the buffer}
  FBufSize := {4096}4096 * 16;
  GetMem(FBuffer, FBufSize);

  {save the stream}
  if (aStream = nil)
  then raise Exception.Create(SNilStream);
  FStream := aStream;

  bsInitForNewStream;
end;

constructor TBSBufferedStream.CreateEmpty;
begin
  inherited Create;

  {allocate the buffer}
  FBufSize := 4096;
  GetMem(FBuffer, FBufSize);

  bsInitForNewStream();
end;

destructor TBSBufferedStream.Destroy;
begin
  if (FBuffer <> nil) then begin
    if FDirty and (FStream <> nil)
    then bsWriteToStream;

    FreeMem(FBuffer, FBufSize);
  end;

  inherited Destroy;
end;

procedure TBSBufferedStream.bsInitForNewStream;
begin
  if (FStream <> nil)
  then FSize := FStream.Size
  else FSize := 0;

  FBufCount := 0;
  FBufOfs := 0;
  FBufPos := 0;
  FDirty := False;
end;

function TBSBufferedStream.bsReadChar(var aCh: AnsiChar): Boolean;
begin
  {is there anything to read?}
  if (FSize = (FBufOfs + FBufPos)) then begin
    Result := False;
    Exit;
  end;
  {if we get here, we'll definitely read a character}
  Result := true;
  {make sure that the buffer has some data in it}
  if (FBufCount = 0)
  then bsReadFromStream()
  else
  if (FBufPos = FBufCount) then begin
    if FDirty then bsWriteToStream;
    FBufPos := 0;
    Inc(FBufOfs, FBufSize);
    bsReadFromStream;
  end;
  {get the next character}
  aCh := AnsiChar(FBuffer[FBufPos]);
  Inc(FBufPos);
end;

procedure TBSBufferedStream.bsReadFromStream;
var
  NewPos: Longint;
begin
  {assumptions: FBufOfs is where to read the buffer
                FBufSize is the number of bytes to read
                FBufCount will be the number of bytes read}
  NewPos := FStream.Seek(FBufOfs, soFromBeginning);
  if (NewPos <> FBufOfs)
  then raise Exception.Create(SNoSeekForRead);
  FBufCount := FStream.Read(FBuffer^, FBufSize);
end;

procedure TBSBufferedStream.bsSetStream(aValue: TStream);
begin
  if (aValue <> FStream) then begin
    {if the buffer is dirty, flush it to the current stream}
    if FDirty and (FStream <> nil)
    then bsWriteToStream;
    {remember the stream and initialize all fields}
    FStream := aValue;
    bsInitForNewStream;
  end;
end;

procedure TBSBufferedStream.bsWriteToStream;
var
  NewPos: Longint;
  BytesWritten: Longint;
begin
  {assumptions: FDirty is true
                FBufOfs is where to write the buffer
                FBufCount is the number of bytes to write
                FDirty will be set false afterwards}
  NewPos := FStream.Seek(FBufOfs, soFromBeginning);
  if (NewPos <> FBufOfs)
  then raise Exception.Create(SNoSeekForWrite);
  BytesWritten := FStream.Write(FBuffer^, FBufCount);
  if (BytesWritten <> FBufCount)
  then raise Exception.Create(SCannotWrite);
  FDirty := False;
end;

function TBSBufferedStream.Read(var Buffer; Count: Longint): Longint;
var
  BytesToGo: Longint;
  BytesToRead: Longint;
  BufAsBytes: PChar;
begin
  BufAsBytes := @Buffer;

  if (FStream = nil) then raise Exception.Create(SNilStream);
  {calculate the number of bytes we could read if possible}
  BytesToGo := MinLong(Count, FSize - (FBufOfs + FBufPos));
  {we will return this number of bytes or raise an exception}
  Result := BytesToGo;
  {are we going to read some data after all?}
  if (BytesToGo > 0) then begin
    {make sure that the buffer has some data in it}
    if (FBufCount = 0) then bsReadFromStream;
    {read as much as we can from the current buffer}
    BytesToRead := MinLong(BytesToGo, FBufCount - FBufPos);
    {transfer that number of bytes}
    Move(FBuffer[FBufPos], BufAsBytes^, BytesToRead);
    {update our counters}
    Inc(FBufPos, BytesToRead);
    Dec(BytesToGo, BytesToRead);
    {if we have more bytes to read then we've reached the end of the
     buffer and so we need to read another, and another, etc}
    while BytesToGo > 0 do begin
      {if the current buffer is dirty, write it out}
      if FDirty then bsWriteToStream;
      {position and read the next buffer}
      FBufPos := 0;
      Inc(FBufOfs, FBufSize);
      bsReadFromStream;
      {calculate the new destination position, and the number of bytes
       to read from this buffer}
      Inc(BufAsBytes, BytesToRead);
      BytesToRead := MinLong(BytesToGo, FBufCount - FBufPos);
      {transfer that number of bytes}
      Move(FBuffer[FBufPos], BufAsBytes^, BytesToRead);

      {update our counters}
      Inc(FBufPos, BytesToRead);
      Dec(BytesToGo, BytesToRead);
    end;
  end;
end;

function TBSBufferedStream.Seek(Offset: Longint; Origin: word): Longint;
var
  NewPos: Longint;
  NewOfs: Longint;
begin
  if (FStream = nil)
  then raise Exception.Create(SNilStream);
  {optimization: to help code that just wants the current stream
   position (ie, reading the Position property), check for this as a
   special case}
  if (Offset = 0) and (Origin = soFromCurrent) then begin
    Result := FBufOfs + FBufPos;
    Exit;
  end;
  {calculate the desired position}
  case Origin of
    soFromBeginning: NewPos := Offset;
    soFromCurrent: NewPos := (FBufOfs + FBufPos) + Offset;
    soFromEnd: NewPos := FSize + Offset;
    else
      raise Exception.Create(SBadOrigin);
      NewPos := 0; {to fool the compiler's warning--we never get here}
  end;
  {force the new position to be valid}
  if (NewPos < 0) then NewPos := 0
  else
  if (NewPos > FSize) then NewPos := FSize;
  {calculate the offset for the buffer}
  NewOfs := (NewPos div FBufSize) * FBufSize;
  {if the offset differs, we have to move the buffer window}
  if (NewOfs <> FBufOfs) then begin
    {check to see whether we have to write the current buffer to the
     original stream first}
    if FDirty then bsWriteToStream;
    {mark the buffer as empty}
    FBufOfs := NewOfs;
    FBufCount := 0;
  end;
  {set the position within the buffer}
  FBufPos := NewPos - FBufOfs;
  Result := NewPos;
end;

procedure TBSBufferedStream.SetSize(NewSize: Longint);
var
  NewPos: Longint;
begin
  {get rid of the simple case first where the new size and the old
   size are the same}
  if (NewSize = FSize) then Exit;
  {if the buffer is dirty, write it out}
  if FDirty then bsWriteToStream;
  {now set the size of the underlying stream}
  FStream.Size := NewSize;
  {patch up the buffer fields so that the buffered stream points to
   somewhere in the newly resized stream}
  NewPos := FBufOfs + FBufPos;
  if (NewPos > NewSize) then NewPos := NewSize;
  bsInitForNewStream;
  Seek(NewPos, soFromBeginning);
end;

function TBSBufferedStream.Write(const Buffer; Count: Longint): Longint;
var
  BytesToGo: Longint;
  BytesToWrite: Longint;
  BufAsBytes: PChar;
begin
  BufAsBytes := @Buffer;

  if (FStream = nil)
  then raise Exception.Create(SNilStream);
  {calculate the number of bytes we should be able to write}
  BytesToGo := Count;
  {we will return this number of bytes or raise an exception}
  Result := BytesToGo;
  {are we going to write some data?}
  if (BytesToGo > 0) then begin
    {try and make sure that the buffer has some data in it}
    if (FBufCount = 0) then bsReadFromStream;
    {write as much as we can to the current buffer}
    BytesToWrite := MinLong(BytesToGo, FBufSize - FBufPos);
    {transfer that number of bytes}
    Move(BufAsBytes^, FBuffer[FBufPos], BytesToWrite);
    FDirty := true;
    {update our counters}
    Inc(FBufPos, BytesToWrite);
    if (FBufCount < FBufPos) then begin
      FBufCount := FBufPos;
      FSize := FBufOfs + FBufPos;
    end;
    Dec(BytesToGo, BytesToWrite);
    {if we have more bytes to write then we've reached the end of the
     buffer and so we need to write another, and another, etc}
    while BytesToGo > 0 do begin
      {as the current buffer is dirty, write it out}
      bsWriteToStream;
      {position and read the next buffer, if required}
      FBufPos := 0;
      Inc(FBufOfs, FBufSize);
      if (FBufOfs < FSize)
      then bsReadFromStream
      else FBufCount := 0;
      {calculate the new destination position, and the number of bytes
       to write to this buffer}
      Inc(BufAsBytes, BytesToWrite);
      BytesToWrite := MinLong(BytesToGo, FBufSize - FBufPos);
      {transfer that number of bytes}
      Move(BufAsBytes^, FBuffer[0], BytesToWrite);
      FDirty := true;
      {update our counters}
      Inc(FBufPos, BytesToWrite);
      if (FBufCount < FBufPos) then begin
        FBufCount := FBufPos;
        FSize := FBufOfs + FBufPos;
      end;
      Dec(BytesToGo, BytesToWrite);
    end;
  end;
end;

{$ENDIF}
{$ENDIF}

{ TBSTimerThread }

{$IFNDEF DELPHI_NET}
procedure TBSTimerThread.Execute;
begin
  repeat
    Sleep(FTimer.FInterval);
    FTimer.DoTimer;
  until Terminated;
end;
{$ENDIF}

{ TBSTimer }

constructor TBSTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FInterval := DEFAULT_INTERVAL;

  {$IFNDEF DELPHI_NET}
  FTimerThread := TBSTimerThread.Create({$IFNDEF DELPHI_NET_CF}True{$ELSE}False{$ENDIF});
  FTimerThread.FTimer := Self;
  {$ENDIF}
end;

destructor TBSTimer.Destroy;
begin
  Enabled := False;

  {$IFNDEF DELPHI_NET}
  FTimerThread.Destroy;
  {$ENDIF}

  inherited Destroy;
end;

{$IFNDEF DELPHI_NET}
procedure TBSTimer.DoTimer;
begin
  if not(FEnabled) then Exit;

  try
    if Assigned(FOnTimer)
    then FOnTimer(Self);
  except
    Hole(Self);
  end;
end;
{$ELSE}
procedure TBSTimer.TimerCallback(sender: System.Object);
begin
  if not(FEnabled) then Exit;

  try
    if Assigned(FOnTimer)
    then FOnTimer(Self);
  except
  end;
end;
{$ENDIF}

procedure TBSTimer.SetEnabled(Value: Boolean);
begin
  if (FEnabled = Value) then Exit;
  FEnabled := Value;

  {$IFNDEF DELPHI_NET}
  if FEnabled then begin
    if (FInterval > 0)
    then FTimerThread.Resume;
  end else FTimerThread.Suspend;
  {$ELSE}
  if FEnabled then begin
    if (FInterval > 0)
    then FTimer := System.Threading.Timer.Create(TimerCallback, Self, 0, FInterval);
  end else FTimer.Dispose;
  {$ENDIF}
end;

procedure TBSTimer.SetInterval(Value: Cardinal);
var
  PrevEnabled: Boolean;
begin
  if (FInterval = Value) then Exit;

  PrevEnabled := FEnabled;
  Enabled := False;
  FInterval := Value;
  Enabled := PrevEnabled;
end;

{==============================================================================}

{ TBSObjectList }

constructor TBSObjectList.Create;
begin
  inherited Create;
  FOwnsObjects := True;
end;

constructor TBSObjectList.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

function TBSObjectList.Add(AObject: TObject): Integer;
begin
  Result := inherited Add(AObject);
end;

function TBSObjectList.Extract(Item: TObject): TObject;
begin
  Result := TObject(inherited Extract(Item));
end;

function TBSObjectList.GetItem(Index: Integer): TObject;
begin
  Result := inherited Items[Index];
end;

function TBSObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result := inherited IndexOf(AObject);
end;

procedure TBSObjectList.Insert(Index: Integer; AObject: TObject);
begin
  inherited Insert(Index, AObject);
end;

{$IFNDEF DELPHI_NET}
procedure TBSObjectList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if OwnsObjects then
    if Action = lnDeleted then
      TObject(Ptr).Free;
  inherited Notify(Ptr, Action);
end;
{$ELSE}
procedure TBSObjectList.Notify(Instance: TObject; Action: TListNotification);
begin
  if OwnsObjects then
    if Action = lnDeleted then
      TObject(Instance).Free;
  inherited Notify(Instance, Action);
end;
{$ENDIF}

function TBSObjectList.Remove(AObject: TObject): Integer;
begin
  Result := inherited Remove(AObject);
end;

procedure TBSObjectList.SetItem(Index: Integer; AObject: TObject);
begin
  inherited Items[Index] := AObject;
end;

{==============================================================================}

var
  Serializables: array [Byte] of TBSSerializableObjectClass;

procedure ClearSerializables();
var
  i: Byte;
begin
  for i := 0 to High(Byte) do Serializables[i] := nil;
end;

function GetSerializableClass(aKind: Byte): TBSSerializableObjectClass;
begin
  Result := Serializables[aKind];

  if (Result = nil)
  then raise Exception.CreateFmt(SObjectKindUnknown, [aKind]);
end;

procedure RegisterSerializable(aKind: Byte; aClass: TBSSerializableObjectClass);
begin
  Serializables[aKind] := aClass;
end;

{ TBSSerializableObject }

constructor TBSSerializableObject.Create(aOwner: TObject);
begin
  inherited Create();

  FOwner := aOwner;
end;

function TBSSerializableObject.GetSerializeKind: Byte;
begin
  Result := 0;
end;

procedure TBSSerializableObject.LoadFromStream(aStream: TStream; aVersion: TFileVersion);
begin
  // dummy
end;

procedure TBSSerializableObject.SaveToStream(aStream: TStream; aVersion: TFileVersion);
begin
  // dummy
end;

{================================ Хэш-таблица =================================}

{ TBSHashEntry }

constructor TBSHashEntry.Create(aOwner: TObject);
begin
  inherited Create(aOwner);
end;

{ TBSHashTable }

constructor TBSHashTable.Create(aOwner: TObject);
var
  i: Integer;
begin
  inherited Create(aOwner);

  FCount := 0;
  FEntryClass := nil;
  FKind := hkELF;
  for i := 0 to HASH_SIZE do FTable[i] := nil;

  FFindLastCode := 0;
  FFindLastEntry := nil;
end;

constructor TBSHashTable.Create(aOwner: TObject; aEntryClass: TBSHashEntryClass; aKind: THashKind = hkELF);
var
  i: THashCode;
begin
  inherited Create(aOwner);

  FCount := 0;
  FEntryClass := aEntryClass;
  FKind := aKind;
  for i := 0 to HASH_SIZE do FTable[i] := nil;

  FFindLastCode := 0;
  FFindLastEntry := nil;
end;

destructor TBSHashTable.Destroy;
begin
  Clear;

  inherited Destroy;
end;

procedure TBSHashTable.Clear;
var
  i: THashCode;

  procedure KillLink(E: TBSHashEntry);
  begin
    if (E.Next <> nil) then KillLink(E.Next);
    E.Free;
  end;

begin
  for i := 0 to HASH_SIZE do
    if (FTable[i] <> nil) then begin
      KillLink(FTable[i]);
      FTable[i] := nil;
    end;

  FCount := 0;
end;

procedure TBSHashTable.PutEntry(aEntry: TBSHashEntry);
var
  i: Longint;
  t: TBSHashEntry;
begin
  t := GetEntryBySign(aEntry.Sign, False);
  if (t <> nil)
  then raise Exception.CreateFmt(SDuplicateExists, [aEntry.Sign]);

  i := Hash(aEntry.Sign);
  t := FTable[i];
  FTable[i] := aEntry;
  aEntry.Next := t;
  Inc(FCount);
end;

function TBSHashTable.FindEntry(const aSign: string): TBSHashEntry;
var
  i: Longint;
  t: TBSHashEntry;
begin
  Result := nil;
  i := Hash(aSign);
  t := FTable[i];

  if (t <> nil) then begin
    repeat
      if (t.Sign = aSign) then begin
        Result := t;
        Break;
      end;

      t := t.Next;
    until (t = nil);
  end;
end;

function TBSHashTable.GetEntryBySign(const aSign: string; aCanCreate: Boolean): TBSHashEntry;
var
  i: Longint;
  t, newEntry: TBSHashEntry;
begin
  Result := nil;
  i := Hash(aSign);

  if (FTable[i] = nil) then begin
    if aCanCreate then begin
      newEntry := FEntryClass.Create(Owner);
      newEntry.Sign := aSign;
      newEntry.Next := nil;

      FTable[i] := newEntry;
      Result := newEntry;
      Inc(FCount);
    end;
  end else begin
    t := FTable[i];

    while (aSign <> t.Sign) and (t.Next <> nil) do t := t.Next;

    if (aSign = t.Sign)
    then Result := t
    else begin
      if aCanCreate then begin
        t := FTable[i];

        newEntry := FEntryClass.Create(Owner);
        newEntry.Sign := aSign;
        newEntry.Next := t;

        FTable[i] := newEntry;
        Result := newEntry;
        Inc(FCount);
      end;
    end;
  end;
end;

function TBSHashTable.AddEntry(const aSign: string): TBSHashEntry;
begin
  Result := GetEntryBySign(aSign, True);
end;

function TBSHashTable.AddEntry(const aSign: string; aEntryClass: TBSHashEntryClass): TBSHashEntry;
var
  oldClass: TBSHashEntryClass;
begin
  oldClass := FEntryClass;

  FEntryClass := aEntryClass;
  Result := GetEntryBySign(aSign, True);
  FEntryClass := oldClass;
end;

procedure TBSHashTable.DeleteEntry(const aSign: string);
var
  i: Longint;
  t, prv: TBSHashEntry;
begin
  i := Hash(aSign);

  if (FTable[i] = nil) then Exit;

  t := FTable[i];
  prv := nil;

  while (aSign <> t.Sign) and (t.Next <> nil) do begin
    prv := t;
    t := t.Next;
  end;

  if (aSign = t.Sign) then begin
    if (prv <> nil)
    then prv.Next := t.Next
    else FTable[i] := t.Next;

    t.Free;

    Dec(FCount);
  end;
end;

function TBSHashTable.Hash(aSign: string): THashCode;
var
  h: Cardinal;
begin
  //aSign := AnsiLowerCase(aSign);

  case FKind of
    hkPJW: h := HashPJW(aSign);
    hkELF: h := HashELF(aSign);
    hkBKDR: h := HashBKDR(aSign);
    hkCRC32: h := CrcStr(aSign);
    hkRS: h := HashRS(aSign);
    hkJS: h := HashJS(aSign);
    hkSDBM: h := HashSDBM(aSign);
    hkDJB: h := HashDJB(aSign);
    hkDEK: h := HashDEK(aSign);
    hkAP: h := HashAP(aSign);
  end;

  Result := h mod (HASH_SIZE+1);
end;

procedure TBSHashTable.Enumerate(EnumProc: THTEnumProc);
var
  i: THashCode;
  entry: TBSHashEntry;
  idx: Longint;
  continue: Boolean;
begin
  idx := -1;

  for i := 0 to HASH_SIZE do begin
    entry := FTable[i];

    while (entry <> nil) do begin
      Inc(idx);
      continue := True;
      EnumProc(entry, idx, continue);

      if not continue then Exit;

      entry := entry.Next;
    end;
  end;
end;

function TBSHashTable.FindFirst: TBSHashEntry;
begin
  FFindLastCode := 0;
  FFindLastEntry := nil;

  Result := FindNext();
end;

function TBSHashTable.FindNext: TBSHashEntry;
var
  i: Integer;
begin
  Result := nil;

  for i := FFindLastCode to HASH_SIZE do begin
    if (FFindLastEntry = nil)
    then FFindLastEntry := FTable[i]
    else FFindLastEntry := FFindLastEntry.Next;

    if (FFindLastEntry <> nil) then begin
      FFindLastCode := i;
      Result := FFindLastEntry;
      Break;
    end
  end;
end;

function TBSHashTable.GetEntryByIndex(aIndex: Integer): TBSHashEntry;
var
  i: THashCode;
  entry: TBSHashEntry;
  idx: Longint;
begin
  Result := nil;

  idx := -1;
  for i := 0 to HASH_SIZE do begin
    entry := FTable[i];

    while (entry <> nil) do begin
      Inc(idx);

      if (idx = aIndex) then begin
        Result := entry;
        Exit;
      end;

      entry := entry.Next;
    end;
  end;
end;

procedure TBSHashTable.LoadFromStream(aStream: TStream; aVersion: TFileVersion);
var
  cnt, i: Longint;
  item: TBSHashEntry;
  kind: Byte;
  aClass: TBSSerializableObjectClass;
begin
  Clear;

  cnt := SReadLongint(aStream);
  for i := 0 to cnt - 1 do begin
    kind := SReadByte(aStream);
    aClass := GetSerializableClass(kind);

    item := TBSHashEntryClass(aClass).Create(Owner);
    item.LoadFromStream(aStream, aVersion);
    PutEntry(item);
  end;
end;

procedure TBSHashTable.SaveToStream(aStream: TStream; aVersion: TFileVersion);
var
  entry: TBSHashEntry;
  kind: Byte;
begin
  SWriteLongint(aStream, EntriesCount);

  entry := FindFirst();
  while (entry <> nil) do begin
    kind := entry.GetSerializeKind;
    SWriteByte(aStream, kind);

    entry.SaveToStream(aStream, aVersion);
    entry := FindNext();
  end;
end;

{==============================================================================}

initialization
  BuildCRCTable;
  ClearSerializables();
  
end.
