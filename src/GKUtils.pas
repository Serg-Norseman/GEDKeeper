unit GKUtils;

interface

uses
  Classes;

{==============================================================================}

function scDecrypt(const S: AnsiString; Key: Word): AnsiString;
function scEncrypt(const S: AnsiString; Key: Word): AnsiString; 
function GenKey(aLength: Integer; IsNum: Boolean = False): string;

{==============================================================================}

procedure LoadExtFile(const aFileName: string);

// замена данных в потоке с кодировки 1251 на UTF-8
function StreamToUtf8Stream(Stream: TStream): UTF8String;

function ConStrings(aStrings: TStrings): string;

type
  TQSortCompare = function (Item1, Item2: Pointer): Integer of object;

procedure QuickSort(SortList: TList; SCompare: TQSortCompare);

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

implementation

uses
  {$IFDEF DELPHI_NET}System.IO,{$ENDIF}
  Windows, SysUtils, ShellAPI, Math;

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
  Map: array[Char] of Byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
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
        Move(I, Result[1], Length(Result)) 
      end; 
    3: 
      begin 
        I := Map[S[1]] + (Map[S[2]] shl 6) + (Map[S[3]] shl 12); 
        SetLength(Result, 2); 
        Move(I, Result[1], Length(Result)) 
      end; 
    4: 
      begin 
        I := Map[S[1]] + (Map[S[2]] shl 6) + (Map[S[3]] shl 12) + 
          (Map[S[4]] shl 18); 
        SetLength(Result, 3); 
        Move(I, Result[1], Length(Result)) 
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
    Result[I] := Char(Byte(Result[I]) xor (Seed shr 8)); 
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
  Move(S[1], I, Length(S)); 
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
    Result[I] := Char(Byte(Result[I]) xor (Seed shr 8)); 
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
    P, T: Pointer;
  begin
    repeat
      I := L;
      J := R;
      P := SortList[(L + R) shr 1];
      repeat
        while SCompare(SortList[I], P) < 0 do
          Inc(I);
        while SCompare(SortList[J], P) > 0 do
          Dec(J);
        if I <= J then
        begin
          T := SortList[I];
          SortList[I] := SortList[J];
          SortList[J] := T;
          Inc(I);
          Dec(J);
        end;
      until (I > J);

      if (L < J) then IQuickSort(L, J);

      L := I;
    until I >= R;
  end;

begin
  if (SortList <> nil) and (SortList.Count > 0) then
    IQuickSort(0, SortList.Count - 1);
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
      then FStream.Seek(SizeOf(c), soFromCurrent);

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

end.
