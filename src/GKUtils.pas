unit GKUtils;

{$I GEDKeeper.inc}

interface

uses
  Classes;

function GetComputerName(): string;
function GetTempDir(): string;
function GetUserName(): string;

function GetFileVersion(): string;

function FileMove(OldName, NewName: string): Boolean;

{==============================================================================}

function scDecrypt(const S: AnsiString; Key: Word): AnsiString;
function scEncrypt(const S: AnsiString; Key: Word): AnsiString; 
function GenKey(aLength: Integer; IsNum: Boolean = False): string;

{==============================================================================}

type
  TCharSet = set of AnsiChar;  

procedure LoadExtFile(const aFileName: string);

function SReadLongint(S: TStream): Longint;
function SReadString(S: TStream): AnsiString;
procedure SWriteLongint(S: TStream; B: Longint);
procedure SWriteString(S: TStream; B: AnsiString);

// замена данных в потоке с кодировки 1251 на UTF-8
function StreamToUtf8Stream(Stream: TStream): UTF8String;

function ConStrings(aStrings: TStrings): string;

type
  TQSortCompare = function (Item1, Item2: TObject): Integer of object;

procedure QuickSort(SortList: TList; SCompare: TQSortCompare);
{$IFNDEF DELPHI_NET}
procedure MergeSort(aList: TList; aCompare: TQSortCompare);
{$ENDIF}

{==============================================================================}

function Crc32(crc: Longword; const c: Byte): Longword;
function CrcStr(const Str: string): Longword;

{==============================================================================}

function SafeDiv(aDividend, aDivisor: Double): Double;

// извлечение токена
function GetToken(aString, SepChar: string; TokenNum: Byte): string;

// количество токенов
function GetTokensCount(aString, SepChar: string): Byte;

// Получить число римскими цифрами
function GetRome(N: Integer): string;

// Дополнить число нулями
function NumUpdate(val, up: Integer): string;

{==============================================================================}

type
  TTextFileEx = class(TObject)
  private
    FStream: TStream;
  public
    constructor Create(aStream: TStream);

    function Eof(): Boolean;
    function ReadLn(): string;
    procedure WriteLn(const s: string); overload;
  end;

const
  LineEnd = #13#10;

{==============================================================================}

// Hole функция предотвращает распределение переменной
// в регистрах CPU подлежащих оптимизации
function Hole(var A): Integer;

// Путь приложения
function GetAppPath(): string;

// Начать протоколирование
procedure LogInit(const aFileName: string);
// Записать в протокол
procedure LogWrite(const aMsg: string);

implementation

uses
  {$IFDEF DELPHI_NET}System.IO, System.Reflection, {$ENDIF}
  Windows, SysUtils, ShellAPI, Math;

function GetComputerName(): string;
{$IFNDEF DELPHI_NET}
var
  buf: PChar;
  len: Cardinal;
{$ENDIF}
begin
  {$IFNDEF DELPHI_NET}
  GetMem(buf, 255);
  len := 255;
  Windows.GetComputerName(buf, len);
  Result := StrPas(buf);
  FreeMem(buf);
  {$ELSE}
  Result := Environment.MachineName;
  {$ENDIF}
end;

function GetTempDir(): string;
{$IFNDEF DELPHI_NET}
var
  Buffer: array [0..1023] of Char;
{$ENDIF}
begin
  {$IFNDEF DELPHI_NET}
  SetString(Result, Buffer, GetTempPath(SizeOf(Buffer), Buffer));
  {$ELSE}
  Result := Environment.GetEnvironmentVariable('TEMP');
  {$ENDIF}
end;

function GetUserName(): string;
{$IFNDEF DELPHI_NET}
var
  buf: array [0..255] of Char;
  len: Cardinal;
{$ENDIF}
begin
  {$IFNDEF DELPHI_NET}
  len := 255;
  Windows.GetUserName(@buf, len);
  Result := StrPas(buf);
  {$ELSE}
  Result := Environment.UserName;
  {$ENDIF}
end;

type
  EVerInfoError = class(Exception);

function GetFileVersion(): string;
{$IFNDEF DELPHI_NET}
var
  Size: Cardinal;
  Handle: DWord;
  RezBuffer, FFileName: string;
  fiBuf: PVSFixedFileInfo;
  Ms, Ls: Longint;
{$ENDIF}
begin
  {$IFNDEF DELPHI_NET}
  FFileName := ParamStr(0);
  Size := GetFileVersionInfoSize(PChar(FFileName), Handle);

  if (Size <= 0)
  then raise EVerInfoError.Create('Information of version inaccessible.');

  SetLength(RezBuffer, Size);

  if not GetFileVersionInfo(PChar(FFileName), Handle, Size, PChar(RezBuffer))
  then raise EVerInfoError.Create('Impossible define version of file.');

  if VerQueryValue(PChar(RezBuffer), '\', Pointer(fiBuf), Size) then begin
    if (Size < SizeOf(TVSFixedFileInfo))
    then raise EVerInfoError.Create('No fixed file info');
  end else raise EVerInfoError.Create('No fixed file info');

  Ms := fiBuf^.dwFileVersionMS;
  Ls := fiBuf^.dwFileVersionLS;

  Result := Format('%d.%d.%d.%d', [HIWORD(Ms), LOWORD(Ms), HIWORD(Ls), LOWORD(Ls)]);
  {$ENDIF}
end;

function FileMove(OldName, NewName: string): Boolean;
begin
  {$IFNDEF DELPHI_NET}
  Result := MoveFile(PChar(OldName), PChar(NewName));
  {$ELSE}
  Result := System.IO.File.Move(OldName, NewName);
  {$ENDIF}
end;

{==============================================================================}

{$IFDEF DELPHI_NET}
{procedure Move(const Source; var Dest; count: Integer);
var
  S, D: PChar;
  I: Integer;
begin
  S := PChar(@Source);
  D := PChar(@Dest);
  if S = D then Exit;
  if Cardinal(D) > Cardinal(S) then
    for I := count-1 downto 0 do
      D[I] := S[I]
  else
    for I := 0 to count-1 do
      D[I] := S[I];
end;}

function LoWord(L: DWORD): Word;
begin
  Result := L;
end;

function HiWord(L: DWORD): Word;
begin
  Result := L shr 16;
end;

function LoByte(W: Word): Byte;
begin
  Result := W;
end;

function HiByte(W: Word): Byte;
begin
  Result := W shr 8;
end;

function MakeWord(A, B: Byte): Word;
begin
  Result := A or B shl 8;
end;

function MakeLong(A, B: Word): Longint;
begin
  Result := A or B shl 16;
end;

procedure MoveL2S(const Source: Longint; var Dest: AnsiString; count: Integer);
var
  I: Integer;
  wl, wh: Word;
  bytes: array [0..3] of Byte;
begin
  wl := LoWord(Source);
  wh := HiWord(Source);
  bytes[0] := LoByte(wl);
  bytes[1] := HiByte(wl);
  bytes[2] := LoByte(wh);
  bytes[3] := HiByte(wh);

  for I := 1 to count do Dest[I] := AnsiChar(bytes[I - 1]);
end;

procedure MoveS2L(const Source: AnsiString; var Dest: Longint; count: Integer);
var
  bytes: array [0..3] of Byte;
  I: Integer;
begin
  for I := 1 to 4 do begin
    if (I <= count)
    then bytes[I-1] := Byte(Source[I])
    else bytes[i-1] := 0;
  end;

  Dest := MakeLong(MakeWord(bytes[0], bytes[1]), MakeWord(bytes[2], bytes[3]));
end;

{$ENDIF}

{==============================================================================}

const
  C1 = 94268; //52845;
  C2 = 28446; //22719;
  
function GenKey(aLength: Integer; IsNum: Boolean = False): string;

  function GenSymbol: string;
  var
    r: Integer;
  begin
    Randomize;
    r := Trunc(Random);
    if Odd(r)
    then Result := IntToStr(Random(9))
    else Result := Chr( Byte(RandomRange(65, 90)) );
    if (Result = '&') OR (Result = '>') OR (Result = #13) OR (Result = #10) then GenSymbol;
  end;

var
  i: Integer;
  res: string;
begin
  res := '';
  if IsNum then begin
    Randomize;
    for i := 1 to aLength do begin
      res := res + IntToStr(Random(9));
    end;
  end
  else begin
    for i := 1 to aLength do begin
      res := res + GenSymbol;
    end;
  end;
  Result := res;
end;

function Decode(const S: AnsiString): AnsiString; 
const 
  Map: array [AnsiChar] of Byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 62, 0, 0, 0, 63, 52, 53, 
    54, 55, 56, 57, 58, 59, 60, 61, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 
    3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 
    20, 21, 22, 23, 24, 25, 0, 0, 0, 0, 0, 0, 26, 27, 28, 29, 30, 
    31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 
    46, 47, 48, 49, 50, 51, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0); 
var 
  I: LongInt;
begin
  case Length(S) of
    2:
      begin
        I := Map[S[1]] + (Map[S[2]] shl 6);
        SetLength(Result, 1);
        {$IFNDEF DELPHI_NET}
        Move(I, Result[1], Length(Result));
        {$ELSE}
        MoveL2S(I, Result, Length(Result));
        {$ENDIF}
      end;
    3:
      begin
        I := Map[S[1]] + (Map[S[2]] shl 6) + (Map[S[3]] shl 12);
        SetLength(Result, 2);
        {$IFNDEF DELPHI_NET}
        Move(I, Result[1], Length(Result));
        {$ELSE}
        MoveL2S(I, Result, Length(Result));
        {$ENDIF}
      end;
    4:
      begin
        I := Map[S[1]] + (Map[S[2]] shl 6) + (Map[S[3]] shl 12) +
          (Map[S[4]] shl 18);
        SetLength(Result, 3);
        {$IFNDEF DELPHI_NET}
        Move(I, Result[1], Length(Result));
        {$ELSE}
        MoveL2S(I, Result, Length(Result));
        {$ENDIF}
      end 
  end 
end; 

function PreProcess(const S: AnsiString): AnsiString; 
var 
  SS: AnsiString; 
begin 
  SS := S;
  Result := '';
  while SS <> '' do
  begin 
    Result := Result + Decode(Copy(SS, 1, 4)); 
    Delete(SS, 1, 4) 
  end 
end; 

function InternalDecrypt(const S: AnsiString; Key: Word): AnsiString; 
var 
  I: Word; 
  Seed: Word; 
begin 
  Result := S; 
  Seed := Key; 
  for I := 1 to Length(Result) do 
  begin 
    Result[I] := AnsiChar(Byte(Result[I]) xor (Seed shr 8));
    Seed := (Byte(S[I]) + Seed) * Word(C1) + Word(C2) 
  end 
end; 

function scDecrypt(const S: AnsiString; Key: Word): AnsiString; 
begin 
  Result := InternalDecrypt(PreProcess(S), Key) 
end; 

function Encode(const S: AnsiString): AnsiString; 
const 
  Map: array[0..63] of Char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' + 
    'abcdefghijklmnopqrstuvwxyz0123456789+/'; 
var 
  I: LongInt; 
begin 
  I := 0; 

  {$IFNDEF DELPHI_NET}
  Move(S[1], I, Length(S));
  {$ELSE}
  MoveS2L(S, I, Length(S));
  {$ENDIF}

  case Length(S) of
    1: 
      Result := Map[I mod 64] + Map[(I shr 6) mod 64]; 
    2: 
      Result := Map[I mod 64] + Map[(I shr 6) mod 64] + 
        Map[(I shr 12) mod 64]; 
    3: 
      Result := Map[I mod 64] + Map[(I shr 6) mod 64] + 
        Map[(I shr 12) mod 64] + Map[(I shr 18) mod 64] 
  end 
end; 

function PostProcess(const S: AnsiString): AnsiString; 
var 
  SS: AnsiString; 
begin 
  SS := S;
  Result := ''; 
  while SS <> '' do 
  begin 
    Result := Result + Encode(Copy(SS, 1, 3)); 
    Delete(SS, 1, 3) 
  end 
end; 

function InternalEncrypt(const S: AnsiString; Key: Word): AnsiString; 
var 
  I: Word; 
  Seed: Word; 
begin 
  Result := S; 
  Seed := Key; 
  for I := 1 to Length(Result) do 
  begin 
    Result[I] := AnsiChar(Byte(Result[I]) xor (Seed shr 8)); 
    Seed := (Byte(Result[I]) + Seed) * Word(C1) + Word(C2) 
  end 
end; 

function scEncrypt(const S: AnsiString; Key: Word): AnsiString; 
begin 
  Result := PostProcess(InternalEncrypt(S, Key)) 
end; 

{==============================================================================}

procedure LoadExtFile(const aFileName: string);
begin
  {$IFNDEF DELPHI_NET}
  ShellExecute(0, 'open', PChar(aFileName), nil, nil, SW_SHOW);
  {$ELSE}
  ShellExecute(0, 'open', (aFileName), '', '', SW_SHOW);
  {$ENDIF}
end;

function SReadLongint(S: TStream): Longint;
begin
  S.Read(Result{$IFNDEF DELPHI_NET}, SizeOf(Longint){$ENDIF});
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

procedure SWriteLongint(S: TStream; B: Longint);
begin
  S.Write(B{$IFNDEF DELPHI_NET}, SizeOf(Longint){$ENDIF});
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

// замена данных в потоке с кодировки 1251 на UTF-8
function StreamToUtf8Stream(Stream: TStream): UTF8String;
var
  s: string;
begin
  SetLength(s, Stream.Size);
  Stream.Seek(0, soFromBeginning);

  {$IFNDEF DELPHI_NET}
  Stream.Read(s[1], Stream.Size);
  {$ELSE}
  s := SReadString(Stream);
  {$ENDIF}

  Result := AnsiToUtf8(s);
  Stream.Size := 0;

  {$IFNDEF DELPHI_NET}
  Stream.Write(Result[1], Length(Result));
  {$ELSE}
  SWriteString(Stream, Result);
  {$ENDIF}
end;

function ConStrings(aStrings: TStrings): string;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to aStrings.Count - 1 do begin
    if (Result <> '') then Result := Result + ' ';
    Result := Result + Trim(aStrings[i]);
  end;
end;

procedure QuickSort(SortList: TList; SCompare: TQSortCompare);

  procedure IQuickSort(L, R: Integer);
  var
    I, J: Integer;
    P, T: TObject;
  begin
    repeat
      I := L;
      J := R;
      P := SortList[(L + R) shr 1];
      repeat
        while SCompare(SortList[I], P) < 0 do Inc(I);
        while SCompare(SortList[J], P) > 0 do Dec(J);

        if (I <= J) then begin
          T := SortList[I];
          SortList[I] := SortList[J];
          SortList[J] := T;
          Inc(I);
          Dec(J);
        end;
      until (I > J);

      if (L < J) then IQuickSort(L, J);

      L := I;
    until (I >= R);
  end;

begin
  if (SortList <> nil) and (SortList.Count > 0) then
    IQuickSort(0, SortList.Count - 1);
end;

{$IFNDEF DELPHI_NET}

{ Standard mergesort }
procedure MSS(aList: TList; aFirst, aLast: Integer; aCompare: TQSortCompare;
  aTempList: PPointerList);
var
  Mid, i, j, ToInx, FirstCount: Integer;
begin
  {calculate the midpoint}
  Mid := (aFirst + aLast) div 2;
  {recursively mergesort the 1st half and the 2nd half of the list}
  if (aFirst < Mid) then
    MSS(aList, aFirst, Mid, aCompare, aTempList);
  if ((Mid + 1) < aLast) then
    MSS(aList, (Mid + 1), aLast, aCompare, aTempList);
  {copy the first half of the list to our temporary list}
  FirstCount := (Mid - aFirst) + 1;
  Move(aList.List^[aFirst], aTempList^[0], FirstCount * sizeof(pointer));
  {set up the indexes: i is the index for the temporary list (ie the
   first half of the list), j is the index for the second half of the
   list, ToInx is the index in the merged where items will be copied}
  i := 0;
  j := (Mid + 1);
  ToInx := aFirst;
  {now merge the two lists}
  {repeat until one of the lists empties...}
  while (i < FirstCount) and (j <= aLast) do begin
    {calculate the smaller item from the next items in both lists and
     copy it over; increment the relevant index}
    if (aCompare(aTempList^[i], aList.List^[j]) <= 0) then begin
      aList.List^[ToInx] := aTempList^[i];
      Inc(i);
    end else begin
      aList.List^[ToInx] := aList.List^[j];
      Inc(j);
    end;
    {there's one more item in the merged list}
    Inc(ToInx);
  end;
  {if there are any more items in the first list, copy them back over}
  if (i < FirstCount) then
    Move(aTempList^[i], aList.List^[ToInx], (FirstCount - i) * sizeof(pointer));
  {if there are any more items in the second list then they're already
   in place and we're done; if there aren't, we're still done}
end;

procedure MergeSort(aList: TList; aCompare: TQSortCompare);
var
  TempList: PPointerList;
  ItemCount, size: Integer;
begin
  ItemCount := aList.Count;

  {if there is at least two items to sort}
  if (ItemCount > 1) then begin
    {create a temporary pointer list}
    size := ((ItemCount + 1) div 2) * SizeOf(Pointer);

    GetMem(TempList, size);
    try
      MSS(aList, 0, ItemCount-1, aCompare, TempList);
    finally
      FreeMem(TempList, size);
    end;
  end;
end;

{$ENDIF}

{==============================================================================}

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

function SafeDiv(aDividend, aDivisor: Double): Double;
begin
  if (aDivisor = 0.0)
  then Result := 0.0
  else Result := aDividend / aDivisor;
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

// Дополнить число нулями
function NumUpdate(val, up: Integer): string;
begin
  Result := IntToStr(val);
  while (Length(Result) < up) do Result := '0' + Result;
end;

{==============================================================================}

{ TTextFileEx }

constructor TTextFileEx.Create(aStream: TStream);
begin
  inherited Create;
  FStream := aStream;
end;

function TTextFileEx.Eof(): Boolean;
begin
  Result := (FStream.Position >= FStream.Size);
end;

function TTextFileEx.ReadLn(): string;
var
  c: Char;
begin
  Result := '';

  while not Eof do begin
    FStream.Read(c, SizeOf(c));

    if (c = #13) then begin
      FStream.Read(c, SizeOf(c));

      if (c <> #10)
      then FStream.Seek(-SizeOf(c), soFromCurrent); // Attention

      Break;
    end;

    if (c = #10)
    then Break;

    Result := Result + c;
  end;
end;

procedure TTextFileEx.WriteLn(const s: string);
var
  i: Integer;
begin
  for i := 1 to Length(s) do
    Write(s[i], SizeOf(s[i]));
  Write(LineEnd, Length(LineEnd));
end;

{==============================================================================}

function Hole(var A): Integer;
{$IFNDEF DELPHI_NET}
asm
{$ELSE}
begin
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

var
  LogFilename: string;

// Начать протоколирование
procedure LogInit(const aFileName: string);
begin
  LogFilename := aFileName;
end;

// Записать в протокол
procedure LogWrite(const aMsg: string);
var
  Log: TextFile;
begin
  AssignFile(Log, LogFilename);

  if FileExists(LogFilename)
  then Append(Log)
  else Rewrite(Log);

  Writeln(Log, '[' + DateTimeToStr(Now) + '] -> ' + aMsg);

  Flush(Log);
  CloseFile(Log);
end;

initialization
  BuildCRCTable;
  
end.
