unit GKImport;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, GedCom551;

type
  TFileType = (ftTextPedigree);

procedure TreeImportEx(aTree: TGEDCOMTree; aFileName: string; aFileType: TFileType;
  aLog: TStrings);

implementation

uses
  RTLConsts, bsComUtils, GKCommon, Controls, Dialogs;

{==============================================================================}

{$IFNDEF DELPHI_NET}
const
{ TParser special tokens }

  toEOF     = Char(0);
  toSymbol  = Char(1);
  toString  = Char(2);
  toInteger = Char(3);
  toFloat   = Char(4);

  {
  нужно: символ, слово, число
  }

type
  TGKParser = class(TObject)
  private
    FStream: TStream;
    FOrigin: Longint;
    FBuffer: PChar;
    FBufPtr: PChar;
    FBufEnd: PChar;
    FSourcePtr: PChar;
    FSourceEnd: PChar;
    FTokenPtr: PChar;
    FStringPtr: PChar;
    FSourceLine: Integer;
    FSaveChar: Char;
    FToken: Char;
    FFloatType: Char;

    procedure ReadBuffer();
    procedure SkipBlanks();
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;

    procedure Error(const Ident: string);
    procedure ErrorStr(const Message: string);

    function NextToken(): Char;
    function SourcePos(): Longint;
    function TokenFloat(): Extended;
    function TokenInt(): Int64;
    function TokenString(): string;
    function TokenSymbolIs(const S: string): Boolean;

    property FloatType: Char read FFloatType;
    property SourceLine: Integer read FSourceLine;
    property Token: Char read FToken;
  end;

const
  ParseBufSize = 4096;

constructor TGKParser.Create(Stream: TStream);
begin
  FStream := Stream;
  GetMem(FBuffer, ParseBufSize);
  FBuffer[0] := #0;
  FBufPtr := FBuffer;
  FBufEnd := FBuffer + ParseBufSize;
  FSourcePtr := FBuffer;
  FSourceEnd := FBuffer;
  FTokenPtr := FBuffer;
  FSourceLine := 1;
  NextToken();
end;

destructor TGKParser.Destroy;
begin
  if (FBuffer <> nil) then begin
    FStream.Seek(Longint(FTokenPtr) - Longint(FBufPtr), 1);
    FreeMem(FBuffer, ParseBufSize);
  end;
end;

procedure TGKParser.Error(const Ident: string);
begin
  ErrorStr(Ident);
end;

procedure TGKParser.ErrorStr(const Message: string);
begin
  raise EParserError.CreateResFmt(@SParseError, [Message, FSourceLine]);
end;

procedure TGKParser.SkipBlanks();
begin
  while True do begin
    case FSourcePtr^ of
      #0:
        begin
          ReadBuffer();
          if (FSourcePtr^ = #0) then Exit;
          Continue;
        end;
      #10:
        Inc(FSourceLine);
      #33..#255:
        Exit;
    end;
    Inc(FSourcePtr);
  end;
end;

function TGKParser.NextToken(): Char;
var
  //I, J: Integer;
  P{, S}: PChar;
begin
  SkipBlanks();
  P := FSourcePtr;
  FTokenPtr := P;
  case P^ of
    'A'..'Z', 'a'..'z', '_', 'А'..'Я', 'а'..'я':
      begin
        Inc(P);
        while P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_', 'А'..'Я', 'а'..'я'] do Inc(P);
        Result := toSymbol;
      end;
    '-', '0'..'9':
      begin
        Inc(P);
        while (P^ in ['0'..'9']) do Inc(P);
        Result := toInteger;
        while (P^ in ['0'..'9', '.', 'e', 'E', '+', '-']) do begin
          Inc(P);
          Result := toFloat;
        end;
        if (P^ in ['c', 'C', 'd', 'D', 's', 'S']) then begin
          Result := toFloat;
          FFloatType := P^;
          Inc(P);
        end else FFloatType := #0;
      end;
    else begin
      Result := P^;
      if (Result <> toEOF) then Inc(P);
    end;
  end;
  FSourcePtr := P;
  FToken := Result;
end;

procedure TGKParser.ReadBuffer();
var
  Count: Integer;
begin
  Inc(FOrigin, FSourcePtr - FBuffer);
  FSourceEnd[0] := FSaveChar;
  Count := FBufPtr - FSourcePtr;
  if (Count <> 0) then Move(FSourcePtr[0], FBuffer[0], Count);
  FBufPtr := FBuffer + Count;
  Inc(FBufPtr, FStream.Read(FBufPtr[0], FBufEnd - FBufPtr));
  FSourcePtr := FBuffer;
  FSourceEnd := FBufPtr;
  if (FSourceEnd = FBufEnd) then begin
    FSourceEnd := LineStart(FBuffer, FSourceEnd - 1);
    if (FSourceEnd = FBuffer) then Error(SLineTooLong);
  end;
  FSaveChar := FSourceEnd[0];
  FSourceEnd[0] := #0;
end;

function TGKParser.SourcePos(): Longint;
begin
  Result := FOrigin + (FTokenPtr - FBuffer);
end;

function TGKParser.TokenFloat(): Extended;
begin
  if FFloatType <> #0 then Dec(FSourcePtr);
  Result := StrToFloat(TokenString);
  if FFloatType <> #0 then Inc(FSourcePtr);
end;

function TGKParser.TokenInt(): Int64;
begin
  Result := StrToInt64(TokenString);
end;

function TGKParser.TokenString(): string;
var
  L: Integer;
begin
  if (FToken = toString)
  then L := FStringPtr - FTokenPtr
  else L := FSourcePtr - FTokenPtr;

  SetString(Result, FTokenPtr, L);
end;

function TGKParser.TokenSymbolIs(const S: string): Boolean;
begin
  Result := (Token = toSymbol) and SameText(S, TokenString);
end;

{$ENDIF}

{==============================================================================}

function AddNote(aTree: TGEDCOMTree; iRec: TGEDCOMIndividualRecord): TGEDCOMNoteRecord;
var
  note: TGEDCOMNotes;
begin
  Result := TGEDCOMNoteRecord.Create(aTree, aTree);
  Result.NewXRef();
  aTree.AddRecord(Result);

  if (iRec <> nil) then begin
    note := TGEDCOMNotes.Create(aTree, iRec);
    note.Value := Result;
    iRec.AddNotes(note);
  end;
end;

function IsNumber(aValue: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  if (aValue = '') then Exit;

  for i := 1 to Length(aValue) do
    if not(aValue[i] in ['0'..'9'])
    then Exit;

  Result := True;
end;

function IsRomeLine(const aStr: string): Boolean;
const
  Romes: set of Char = ['I', 'V', 'X', 'L', 'C', 'D', 'M'];
var
  i: Integer;
  rs: string;
begin
  i := 1;
  rs := '';
  while (i <= Length(aStr)) and (aStr[i] in Romes) do begin
    rs := rs + aStr[i];
    Inc(i);
  end;

  Result := (rs <> '') and (rs = aStr);
end;

function IsPersonLine(const aStr: string; var p_id: string): Boolean;
var
  i: Integer;
begin
  i := 1;
  p_id := '';
  while (i <= Length(aStr)) and (aStr[i] in ['-', '0'..'9', '(', ')', '/', '?']) do begin
    p_id := p_id + aStr[i];
    Inc(i);
  end;

  Result := (p_id <> '') and (aStr[i] = '.') and (aStr[i + 1] = ' ');
end;

function CheckDot(aStr: string): string;
begin
  if (aStr[Length(aStr)] = '.')
  then aStr := Copy(aStr, 1, Length(aStr) - 1);

  Result := Trim(aStr);
end;

procedure DefinePersonName(const aStr, p_id: string; var f_name, f_pat, f_fam, bd, dd: string);
var
  b_pos, d_pos, toks: Integer;
  tmp: string;
begin
  f_name := '';
  f_pat := '';
  f_fam := '';
  bd := '';
  dd := '';

  tmp := aStr;
  Delete(tmp, 1, Length(p_id) + 2);

  tmp := CheckDot(tmp);

  b_pos := Pos('*', tmp);
  d_pos := Pos('+', tmp);

  if (d_pos > 0) and (d_pos > b_pos) then begin
    dd := Copy(tmp, d_pos + 1, Length(tmp) - d_pos);
    Delete(tmp, d_pos, Length(dd) + 1);
    tmp := Trim(tmp);
  end;

  if (b_pos > 0) then begin
    bd := Copy(tmp, b_pos + 1, Length(tmp) - b_pos);
    Delete(tmp, b_pos, Length(bd) + 1);
    tmp := Trim(tmp);
  end;

  tmp := Trim(tmp);
  toks := GetTokensCount(tmp, ' ');
  if (toks > 0)
  then f_name := CheckDot(GetToken(tmp, ' ', 1));
  if (toks > 1)
  then f_pat := CheckDot(GetToken(tmp, ' ', 2));
  if (toks > 2)
  then f_fam := CheckDot(GetToken(tmp, ' ', 3));
end;

procedure TreeImportEx(aTree: TGEDCOMTree; aFileName: string; aFileType: TFileType;
  aLog: TStrings);
var
  content, buf, p_list: TStringList;

  procedure CheckBuf(iRec: TGEDCOMIndividualRecord);
  var
    noteRec: TGEDCOMNoteRecord;
  begin
    if (buf.Text <> '') then begin
      noteRec := AddNote(aTree, iRec {nil});
      noteRec.Notes := buf;
    end;
    buf.Clear;
  end;

  procedure SetEvent(iRec: TGEDCOMIndividualRecord; evName, date: string);
  var
    ev: TGEDCOMCustomEvent;
    prefix, tmp: string;
    toks, i: Integer;
    val: array [1..3] of Integer;
  begin
    ev := CreateEventEx(aTree, iRec, evName);

    try
      if (Pos('п.', date) = 1) then begin
        prefix := 'AFT ';
        Delete(date, 1, 2);
        date := Trim(date);
      end
      else
      if (Pos('до', date) = 1) then begin
        prefix := 'BEF ';
        Delete(date, 1, 3);
        date := Trim(date);
      end;

      tmp := '';
      toks := GetTokensCount(date, '.');
      if (toks > 3)
      then raise Exception.Create('date failed');

      for i := 1 to toks do val[i] := StrToInt(GetToken(date, '.', i));

      case toks of
        1: tmp := IntToStr(val[1]);
        2: tmp := GEDCOMMonthArray[val[1]] + ' ' + IntToStr(val[2]);
        3: tmp := IntToStr(val[1]) + ' ' + GEDCOMMonthArray[val[2]] + ' ' + IntToStr(val[3]);
      end;

      ev.Detail.Date.ParseString(prefix + tmp);
    except
      on E: Exception do begin
        aLog.Add('>>>> Ошибка разбора: дата "'+date+'"');
      end;
    end;
  end;

  procedure AddFamily(parent: TGEDCOMIndividualRecord);
  var
    family: TGEDCOMFamilyRecord;
  begin
    family := TGEDCOMFamilyRecord.Create(aTree, aTree);
    family.NewXRef;
    aTree.AddRecord(family);
    AddSpouseToFamily(aTree, family, parent);
  end;

  procedure AddChild(parent, child: TGEDCOMIndividualRecord; mar_id: Integer);
  var
    family: TGEDCOMFamilyRecord;
    fam_link: TGEDCOMChildToFamilyLink;
  begin
    if (mar_id < 0) then mar_id := 1;

    if (mar_id > 1)
    then Hole(mar_id);

    while (parent.SpouseToFamilyLinksCount < mar_id) do AddFamily(parent);
    Dec(mar_id);

    family := parent.SpouseToFamilyLinks[mar_id].Family;

    family.AddChild(TGEDCOMPointer.CreateTag(aTree, family, 'CHIL', '@'+child.XRef+'@'));
    fam_link := TGEDCOMChildToFamilyLink.CreateTag(aTree, child, 'FAMC', family.XRef);
    fam_link.Family := family;
    child.AddChildToFamilyLink(fam_link);
  end;

  function GetSex(f_name, f_pat: string): TGEDCOMSex;
  begin
    Result := svNone;
    case f_name[Length(f_name)] of
      'а', 'я':
        if (Length(f_pat) > 1) then begin
          if (f_pat[Length(f_pat)] in ['а', 'я'])
          then Result := svFemale
          else
          if (f_pat[Length(f_pat)] in ['в', 'г', 'д', 'й', 'л', 'м', 'н', 'о', 'п', 'р'])
          then Result := svMale;
        end;

      'в', 'г', 'д', 'й', 'л', 'м', 'н', 'о', 'п', 'р':
        Result := svMale;
    end;

    if (Result = svNone) then begin
      if (MessageDlg('Не определяется пол человека по имени "'+f_name+' '+f_pat+'". Это мужской пол?', mtConfirmation, [mbYes, mbNo], 0) = mrYes)
      then Result := svMale
      else Result := svFemale;
    end;
  end;

  function ParsePerson(aStr, p_id: string; var self_id: Integer): TGEDCOMIndividualRecord;
  var
    S, f_name, f_pat, f_fam, bd, dd: string;
    sx: TGEDCOMSex;
    x, parent_id, mar_id: Integer;
    parent: TGEDCOMIndividualRecord;
  begin
    self_id := -1;
    parent_id := -1;
    mar_id := -1;

    S := p_id;
    S := ExtractNumber(S, self_id, True, -1);
    if (S <> '') and (S[1] = '-') then begin
      Delete(S, 1, 1);
      S := ExtractNumber(S, parent_id, True, -1);

      if (S <> '') and (S[1] = '/') then begin
        Delete(S, 1, 1);
        S := ExtractNumber(S, mar_id, True, -1);
      end;
    end;

    // parse name and dates
    DefinePersonName(aStr, p_id, f_name, f_pat, f_fam, bd, dd);

    sx := GetSex(f_name, f_pat);

    Result := CreatePersonEx(aTree, f_name, f_pat, f_fam, sx, False);
    p_list.AddObject(IntToStr(self_id), Result);

    buf.Add(aStr);

    if (bd <> '') then SetEvent(Result, 'BIRT', bd);
    if (dd <> '') then SetEvent(Result, 'DEAT', dd);

    if (parent_id > 0) then begin
      x := p_list.IndexOf(IntToStr(parent_id));
      if (x >= 0) then begin
        parent := TGEDCOMIndividualRecord(p_list.Objects[x]);
        AddChild(parent, Result, mar_id);
      end else aLog.Add('>>>> Возможна ошибка разбора: в списке не обнаружен предок с номером "'+IntToStr(parent_id)+'".');
    end;
  end;

var
  i, prev_id, self_id: Integer;
  i_rec: TGEDCOMIndividualRecord;
  s, p_id: string;
begin
  aLog.Clear;

  content := TStringList.Create;
  buf := TStringList.Create;
  p_list := TStringList.Create;
  try
    content.LoadFromFile(aFileName);
    content.Add('');

    prev_id := 0;
    i_rec := nil;

    for i := 0 to content.Count - 1 do begin
      s := Trim(content[i]);

      if (s = '') then begin
        CheckBuf(i_rec);
        i_rec := nil;
      end else begin
        if IsRomeLine(s) then begin
          aLog.Add('> Поколение "'+s+'"');
          i_rec := nil;
        end else begin
          if not(IsPersonLine(s, p_id))
          then buf.Add(s)
          else begin
            CheckBuf(i_rec);
            i_rec := ParsePerson(s, p_id, self_id);

            aLog.Add('> Распознана персональная запись "'+p_id+'".');

            if (self_id - prev_id > 1)
            then aLog.Add('>>>> Возможна ошибка разбора: номера записей содержат пропуск.');
            prev_id := self_id;
          end;
        end;
      end;
    end;
  finally
    p_list.Free;
    buf.Free;
    content.Free;
  end;
end;

{procedure TreeImportEx(aTree: TGEDCOMTree; aFileName: string; aFileType: TFileType;
  aLog: TStrings);
var
  f_stream: TStream;
  parser: TGKParser;
  i, prevLine: Integer;
  i_rec: TGEDCOMIndividualRecord;
  s: string;
begin
  f_stream := TFileStream.Create(aFileName, fmOpenRead);
  try
    parser := TGKParser.Create(f_stream);
    try
      i_rec := nil;
      s := '';
      prevLine := 0;

      repeat
        if (prevLine <> parser.SourceLine) then begin
          aLog.Add(s);
          s := '';
        end;

        prevLine := parser.SourceLine;
        s := s + parser.TokenString + ' ';
      until (parser.NextToken = toEOF);
    finally
      parser.Free;
    end;
  finally
    f_stream.Free;
  end;
end;}

end.
