unit GKUtils; {trans:none}

{$I GEDKeeper2.inc}

interface

uses
  System.IO, System.Reflection, System.Windows.Forms, System.Text,
  System.Globalization, System.Security.Permissions, System.Threading,
  System.Runtime.InteropServices, VCLStub;

type
  TGKUtils = class(System.Object)
  public
    type
      TFilePrepareProc = procedure (const FileName: string) of object;
      TSortCompareFunc = function (Item1, Item2: System.Object): Integer of object;
  private
    const
      _events: array [0..8] of System.Windows.Forms.ScrollEventType = (
        ScrollEventType.SmallDecrement,
        ScrollEventType.SmallIncrement,
        ScrollEventType.LargeDecrement,
        ScrollEventType.LargeIncrement,
        ScrollEventType.ThumbPosition,
        ScrollEventType.ThumbTrack,
        ScrollEventType.First,
        ScrollEventType.Last,
        ScrollEventType.EndScroll
      );

    const
      SInvalidInteger = '''{0}'' is not a valid integer value';

    class var
      LogFilename: string;

    class function FindMatchingFile(var F: TSearchRec): Integer;
    class function FindFirst(const Path: string; Attr: Integer;
      var F: TSearchRec): Integer;
    class function FindNext(var F: TSearchRec): Integer;
    class procedure FindClose(var F: TSearchRec);

    class function Decode(const S: AnsiString): AnsiString; static;
    class function Encode(const S: AnsiString): AnsiString; static;
    class function MakeLong(A, B: Word): Longint; static; inline;
    class function MakeWord(A, B: Byte): Word; static; inline;
    class procedure MoveL2S(const Source: Integer; var Dest: AnsiString;
      count: Integer); static;
    class procedure MoveS2L(const Source: AnsiString; var Dest: Integer;
      count: Integer); static;

    class procedure IQuickSort(SortList: TList; SCompare: TSortCompareFunc; L, R: Integer);

    class var Ccitt32Table: array [0..255] of Longword;

    const
      C1 = 94268; //52845;
      C2 = 28446; //22719;
  public
    class function GetTempDir(): string;
    class function GetFileVersion(): string;
    class function GetAppPath(): string;
    class procedure LoadExtFile(const aFileName: string);

    class procedure ScanDir(const aPath: string; aPrepareProc: TFilePrepareProc;
      aIncludeFolders: Boolean; FileAttrs: Integer; aMask: string = '*.*');

    class function SafeDiv(aDividend, aDivisor: Double): Double;

    class function GetToken(S: string; SepChar: Char; TokenNum: Integer): string;
    class function GetTokensCount(S: string; SepChar: Char): Integer;

    class function GetRome(N: Integer): string;

    class function NumUpdate(val, up: Integer): string;

    class function Hole(var A): Integer;

    class procedure LogInit(const aFileName: string);
    class procedure LogWrite(const aMsg: string);

    class function ConStrings(aStrings: TStrings): string;

    class procedure BuildCRCTable();
    class function CrcStr(const Str: string): Longword;

    class function agCompare(Str1, Str2: string): Integer;

    class procedure QuickSort(SortList: TList; SCompare: TSortCompareFunc);
    class procedure MergeSort(aList: TList; aCompare: TSortCompareFunc);

    class procedure ShowMessage(const Msg: string);
    class procedure ShowError(const Msg: string);
    class function ShowQuestion(const Msg: string): System.Windows.Forms.DialogResult;

    class function scDecrypt(const S: AnsiString; Key: Word): AnsiString; static;
    class function scEncrypt(const S: AnsiString; Key: Word): AnsiString; static;

    class function GetScrollEventType(wParam: Cardinal): ScrollEventType;

    class function StrToFloatDef(const S: string; const Default: Double): Double;
    class function DaysBetween(const ANow, AThen: DateTime): Integer;

    class function IsDigit(C: Char): Boolean;
    class function IsDigits(const S: string): Boolean;

    class function StrToInt(const S: string): Integer;
  end;

implementation

uses GKEngine;

class function TGKUtils.FindMatchingFile(var F: TSearchRec): Integer;
begin
  with F do begin
    while FindData.dwFileAttributes and ExcludeAttr <> 0 do
      if not FindNextFile(FindHandle, FindData) then begin
        Result := GetLastError;
        Exit;
      end;

    Attr := FindData.dwFileAttributes;
    Name := FindData.cFileName;
  end;

  Result := 0;
end;

[FileIOPermission(SecurityAction.LinkDemand, Unrestricted=True)]
class function TGKUtils.FindFirst(const Path: string; Attr: Integer;
  var F: TSearchRec): Integer;
const
  faSpecial = faHidden or faSysFile or faDirectory;
begin
  F.ExcludeAttr := not Attr and faSpecial;
  F.FindHandle := FindFirstFile(Path, F.FindData);
  if F.FindHandle <> INVALID_HANDLE_VALUE then begin
    Result := FindMatchingFile(F);
    if Result <> 0 then FindClose(F);
  end else Result := GetLastError;
end;

[FileIOPermission(SecurityAction.LinkDemand, Unrestricted=True)]
class function TGKUtils.FindNext(var F: TSearchRec): Integer;
begin
  if FindNextFile(F.FindHandle, F.FindData)
  then Result := FindMatchingFile(F)
  else Result := GetLastError;
end;

[FileIOPermission(SecurityAction.LinkDemand, Unrestricted=True)]
class procedure TGKUtils.FindClose(var F: TSearchRec);
begin
  if (F.FindHandle <> INVALID_HANDLE_VALUE) then begin
    VCLStub.FindClose(F.FindHandle);
    F.FindHandle := INVALID_HANDLE_VALUE;
  end;
end;

class function TGKUtils.GetTempDir(): string;
begin
  Result := Environment.GetEnvironmentVariable('TEMP');
end;

class function TGKUtils.GetFileVersion(): string;
var
  InfoSize, Wnd, VerSize: DWORD;
  VerBuf: TBytes;
  FI: TVSFixedFileInfo;
  PFI: IntPtr;
  Ms, Ls: Longint;
  fn: string;
begin
  Result := '';
  fn := ParamStr(0);
  InfoSize := GetFileVersionInfoSize(fn, Wnd);
  if (InfoSize <> 0) then begin
    SetLength(VerBuf, InfoSize);
    if GetFileVersionInfo(fn, Wnd, InfoSize, VerBuf) then
      if VerQueryValue(VerBuf, '\', PFI, VerSize) then begin
        FI := TVSFixedFileInfo(Marshal.PtrToStructure(PFI, TypeOf(TVSFixedFileInfo)));
        Ms := FI.dwFileVersionMS;
        Ls := FI.dwFileVersionLS;
        Result := System.&String.Format('{0}.{1}.{2}.{3}', [Word(Ms shr 16), Word(Ms), Word(Ls shr 16), Word(Ls)]);
      end;
  end;
end;

// сканирование папки
class procedure TGKUtils.ScanDir(const aPath: string; aPrepareProc: TFilePrepareProc;
  aIncludeFolders: Boolean; FileAttrs: Integer; aMask: string = '*.*');
var
  sr: TSearchRec;
  res: Integer;
  newf, PathDelim: string;
begin
  PathDelim := System.IO.Path.DirectorySeparatorChar;

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

class function TGKUtils.GetAppPath(): string;
var
  fn: string;
begin
  fn := System.Reflection.Assembly.GetExecutingAssembly.GetModules[0].FullyQualifiedName;
  Result := System.IO.Path.GetDirectoryName(fn) + '\';
end;

class procedure TGKUtils.LoadExtFile(const aFileName: string);
begin
  ShellExecute(0, 'open', aFileName, '', '', SW_SHOW);
end;

class function TGKUtils.SafeDiv(aDividend, aDivisor: Double): Double;
begin
  if (aDivisor = 0.0)
  then Result := 0.0
  else Result := aDividend / aDivisor;
end;

// извлечение токена
class function TGKUtils.GetToken(S: string; SepChar: Char; TokenNum: Integer): string;
var
  cur_tok, p, sp: Integer;
begin
  Result := '';
  if (S = '') then Exit;

  if (S[Length(S)] <> SepChar)
  then S := S + SepChar;

  sp := 1;
  cur_tok := 0;
  for p := 1 to Length(S) do
    if (S[p] = SepChar) then begin
      Inc(cur_tok);

      if (cur_tok = TokenNum) then begin
        Result := Copy(S, sp, p - sp);
        Exit;
      end;

      sp := p + 1;
    end;
end;

// количество токенов
class function TGKUtils.GetTokensCount(S: string; SepChar: Char): Integer;
var
  p: Integer;
begin
  Result := 0;
  if (S = '') then Exit;

  for p := 1 to Length(S) do
    if (S[p] = SepChar) then Inc(Result);

  Inc(Result);
end;

// Получить число римскими цифрами
class function TGKUtils.GetRome(N: Integer): string;
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
class function TGKUtils.NumUpdate(val, up: Integer): string;
begin
  Result := val.ToString();
  while (Length(Result) < up) do Result := '0' + Result;
end;

class function TGKUtils.Hole(var A): Integer;
begin
end;

// Начать протоколирование
class procedure TGKUtils.LogInit(const aFileName: string);
begin
  LogFilename := aFileName;
end;

// Записать в протокол
class procedure TGKUtils.LogWrite(const aMsg: string);
var
  Log: System.IO.StreamWriter;
begin
  Log := System.IO.StreamWriter.Create(LogFilename, True, Encoding.GetEncoding(1251));
  Log.WriteLine('[' + DateTime.Now.ToString() + '] -> ' + aMsg);
  Log.Flush();
  Log.Close();
end;

class function TGKUtils.ConStrings(aStrings: TStrings): string;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to aStrings.Count - 1 do begin
    if (Result <> '') then Result := Result + ' ';
    Result := Result + aStrings[i].Trim();
  end;
end;

class procedure TGKUtils.BuildCRCTable;
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

class function TGKUtils.CrcStr(const Str: string): Longword;
var
  i: Integer;
  crc: Longword;
  c: Byte;
begin
  crc := 0;
  for i := 1 to Length(Str) do begin
    c := Byte(Str[i]);
    crc := (((crc shr 8) and $00FFFFFF) xor (Ccitt32Table[(crc xor c) and $FF]));
  end;

  Result := crc;
end;

class function TGKUtils.agCompare(Str1, Str2: string): Integer;
var
  Val1, Val2: Double;
  v1, v2: Boolean;
  NumCode: Integer;
begin
  Val(Str1, Val1, NumCode);
  v1 := (NumCode = 0);
  Val(Str2, Val2, NumCode);
  v2 := (NumCode = 0);

  if (v1) and (v2) then begin
    if (Val1 < Val2)
    then Result := -1
    else
      if (Val1 > Val2)
      then Result := 1
      else Result := 0;
  end else begin
    Result := System.String.Compare(Str1, Str2, False);
    if (Str1 <> '') and (Str2 = '') then Result := -1;
    if (Str1 = '') and (Str2 <> '') then Result := 1;
  end;
end;

class procedure TGKUtils.IQuickSort(SortList: TList; SCompare: TSortCompareFunc; L, R: Integer);
var
  I, J: Integer;
  P, T: System.Object;
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

    if (L < J) then IQuickSort(SortList, SCompare, L, J);

    L := I;
  until (I >= R);
end;

class procedure TGKUtils.QuickSort(SortList: TList; SCompare: TSortCompareFunc);
begin
  if (SortList <> nil) and (SortList.Count > 0)
  then IQuickSort(SortList, SCompare, 0, SortList.Count - 1);
end;

{$IFNDEF DELPHI_NET}
{ Standard mergesort }
procedure MSS(aList: TList; aFirst, aLast: Integer; aCompare: TSortCompareFunc;
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
{$ENDIF}

class procedure TGKUtils.MergeSort(aList: TList; aCompare: TSortCompareFunc);
{$IFNDEF DELPHI_NET}
var
  TempList: PPointerList;
  ItemCount, size: Integer;
{$ENDIF}
begin
  {$IFNDEF DELPHI_NET}
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
  {$ENDIF}
end;

class procedure TGKUtils.ShowMessage(const Msg: string);
begin
  System.Windows.Forms.MessageBox.Show(Msg, TGenEngine.AppName,
    MessageBoxButtons.OK, MessageBoxIcon.Information);
end;

class procedure TGKUtils.ShowError(const Msg: string);
begin
  System.Windows.Forms.MessageBox.Show(Msg, TGenEngine.AppName,
    MessageBoxButtons.OK, MessageBoxIcon.Error);
end;

class function TGKUtils.ShowQuestion(const Msg: string): System.Windows.Forms.DialogResult;
begin
  Result := System.Windows.Forms.MessageBox.Show(Msg, TGenEngine.AppName,
    MessageBoxButtons.YesNo, MessageBoxIcon.Question);
end;





class function TGKUtils.MakeWord(A, B: Byte): Word;
begin
  Result := A or B shl 8;
end;

class function TGKUtils.MakeLong(A, B: Word): Longint;
begin
  Result := A or B shl 16;
end;

class procedure TGKUtils.MoveL2S(const Source: Longint; var Dest: AnsiString; count: Integer);
var
  I: Integer;
  wl, wh: Word;
  bytes: array [0..3] of Byte;
begin
  wl := Word(Source);
  wh := Word(Source shr 16);
  bytes[0] := Byte(wl);
  bytes[1] := Byte(wl shr 8);
  bytes[2] := Byte(wh);
  bytes[3] := Byte(wh shr 8);

  for I := 1 to count do Dest[I] := AnsiChar(bytes[I - 1]);
end;

class procedure TGKUtils.MoveS2L(const Source: AnsiString; var Dest: Longint; count: Integer);
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

class function TGKUtils.Decode(const S: AnsiString): AnsiString;
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
  I: Longint;
begin
  case Length(S) of
    2: begin
      I := Map[S[1]] + (Map[S[2]] shl 6);
      SetLength(Result, 1);
      {$IFNDEF DELPHI_NET}
      Move(I, Result[1], Length(Result));
      {$ELSE}
      MoveL2S(I, Result, Length(Result));
      {$ENDIF}
    end;

    3: begin
      I := Map[S[1]] + (Map[S[2]] shl 6) + (Map[S[3]] shl 12);
      SetLength(Result, 2);
      {$IFNDEF DELPHI_NET}
      Move(I, Result[1], Length(Result));
      {$ELSE}
      MoveL2S(I, Result, Length(Result));
      {$ENDIF}
    end;

    4: begin
      I := Map[S[1]] + (Map[S[2]] shl 6) + (Map[S[3]] shl 12) + (Map[S[4]] shl 18);
      SetLength(Result, 3);
      {$IFNDEF DELPHI_NET}
      Move(I, Result[1], Length(Result));
      {$ELSE}
      MoveL2S(I, Result, Length(Result));
      {$ENDIF}
    end;
  end;
end;

class function TGKUtils.scDecrypt(const S: AnsiString; Key: Word): AnsiString;
var
  pp, SS: AnsiString;
  I, Seed: Word;
begin
  // pre process
  SS := S;
  pp := '';
  while (SS <> '') do begin
    pp := pp + Decode(Copy(SS, 1, 4));
    Delete(SS, 1, 4);
  end;

  // internal decrypt
  Result := pp;
  Seed := Key;
  for I := 1 to Length(Result) do begin
    Result[I] := AnsiChar(Byte(Result[I]) xor (Seed shr 8));
    Seed := (Byte(pp[I]) + Seed) * Word(C1) + Word(C2);
  end;
end;

class function TGKUtils.Encode(const S: AnsiString): AnsiString;
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
        Map[(I shr 12) mod 64] + Map[(I shr 18) mod 64];
  end;
end;

class function TGKUtils.scEncrypt(const S: AnsiString; Key: Word): AnsiString;
var
  int, SS: AnsiString;
  I, Seed: Word;
begin
  // internal encrypt
  int := S;
  Seed := Key;
  for I := 1 to Length(int) do begin
    int[I] := AnsiChar(Byte(int[I]) xor (Seed shr 8));
    Seed := (Byte(int[I]) + Seed) * Word(C1) + Word(C2);
  end;

  // postprocess
  SS := int;
  Result := '';
  while (SS <> '') do begin
    Result := Result + Encode(Copy(SS, 1, 3));
    Delete(SS, 1, 3);
  end;
end;

class function TGKUtils.GetScrollEventType(wParam: Cardinal): ScrollEventType;
begin
  if (wParam <= High(_events))
  then Result := _events[wParam]
  else Result := ScrollEventType.EndScroll;
end;

class function TGKUtils.StrToFloatDef(const S: string; const Default: Double): Double;
var
  LFormat: NumberFormatInfo;
  Value: Double;
begin
  LFormat := NumberFormatInfo(System.Threading.Thread.CurrentThread.CurrentCulture.NumberFormat.Clone);
  with LFormat do begin
    if NumberDecimalSeparator <> '.' then NumberDecimalSeparator := '.';
    if NumberGroupSeparator <> ' ' then NumberGroupSeparator := ' ';
  end;

  if System.Double.TryParse(S, NumberStyles.Float, LFormat, Value)
  then Result := Value
  else Result := Default;
end;

class function TGKUtils.DaysBetween(const ANow, AThen: DateTime): Integer;
var
  span: TimeSpan;
begin
  if ANow < AThen then
    span := AThen - ANow
  else
    span := ANow - AThen;

  Result := span.Days;
end;

class function TGKUtils.IsDigit(C: Char): Boolean;
{ Deze functie geeft True terug als het teken C in de reeks '0' tot '9' zit. }
begin
  Result := C in ['0'..'9'];
end;

class function TGKUtils.IsDigits(const S: string): Boolean;
{ Deze functie geeft True terug als de string S niet leeg is en alle tekens in
  de string in de reeks '0' tot '9' zitten. }
var
  I: Integer;
begin
  I := 1;
  while (I <= Length(S)) and (S[I] in ['0'..'9']) do
    Inc(I);
  Result := (Length(S) > 0) and (I > Length(S));
end;

class function TGKUtils.StrToInt(const S: string): Integer;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0
  then raise EConvertError.Create(System.&String.Format(SInvalidInteger, [S]));
end;

end.
