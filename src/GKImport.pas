unit GKImport;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, GedCom551;

type
  TGKImporter = class(TObject)
  private
    FTree: TGEDCOMTree;
    FLog: TStrings;
    FPersonsList: TStringList;

    procedure AddChild(parent, child: TGEDCOMIndividualRecord; mar_id: Integer);
    function AddFamily(parent: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;
    function CheckDot(aStr: string): string;
    procedure DefinePersonName(const aStr, p_id: string; var f_name, f_pat,
      f_fam, bd, dd: string);
    function DeleteBlanks(const S: string): string;
    function ExtractNumComment(const S: string; var Comment: string; NoException: Boolean): string;
    function GetSex(f_name, f_pat: string): TGEDCOMSex;
    function IsPersonLine(const aStr: string; var p_id: string): Boolean;
    function IsRomeLine(const aStr: string): Boolean;
    procedure SetEvent(iRec: TGEDCOMIndividualRecord; evName, date: string);
    function ParsePerson(buf: TStringList; aStr, p_id: string;
      var self_id: Integer): TGEDCOMIndividualRecord;
    procedure Import_PlainText(aFileName: string);
    procedure Import_Excel(aFileName: string);
    procedure Import_Word(aFileName: string);
    procedure CheckBuf(buf: TStringList; iRec: TGEDCOMIndividualRecord);
    procedure CheckSpouses(buf: TStringList;
      iRec: TGEDCOMIndividualRecord);
  public
    constructor Create(aTree: TGEDCOMTree; aLog: TStrings);

    procedure TreeImportEx(aFileName: string);
  end;

implementation

uses
  bsComUtils, GKCommon, Controls, Dialogs, Variants, ComObj, bsMiscUtils;

{ TGKImporter }

constructor TGKImporter.Create(aTree: TGEDCOMTree; aLog: TStrings);
begin
  inherited Create;
  FTree := aTree;
  FLog := aLog;
end;

function TGKImporter.IsRomeLine(const aStr: string): Boolean;
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

function TGKImporter.IsPersonLine(const aStr: string; var p_id: string): Boolean;
var
  i: Integer;
begin
  i := 1;
  p_id := '';
  while (i <= Length(aStr)) and (aStr[i] in ['-', '0'..'9', '(', ')', '/', '?', ' ']) do begin
    p_id := p_id + aStr[i];
    Inc(i);

    if (aStr[i] = '(') then begin
      Hole(i);
      
      while (i <= Length(aStr)) and (aStr[i] <> ')') do begin
        p_id := p_id + aStr[i];
        Inc(i);
      end;
    end;
  end;

  Result := (p_id <> '') and (aStr[i] = '.') and (aStr[i + 1] = ' ');
end;

function TGKImporter.CheckDot(aStr: string): string;
begin
  if (aStr[Length(aStr)] = '.')
  then aStr := Copy(aStr, 1, Length(aStr) - 1);

  Result := Trim(aStr);
end;

procedure TGKImporter.DefinePersonName(const aStr, p_id: string; var f_name, f_pat, f_fam, bd, dd: string);
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

function TGKImporter.AddFamily(parent: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;
begin
  Result := CreateFamilyEx(FTree);
  AddSpouseToFamily(FTree, Result, parent);
end;

procedure TGKImporter.AddChild(parent, child: TGEDCOMIndividualRecord; mar_id: Integer);
var
  family: TGEDCOMFamilyRecord;
  chLink: TGEDCOMChildToFamilyLink;
  ptr: TGEDCOMPointer;
begin
  if (mar_id < 0) then mar_id := 1;

  if (mar_id > 1)
  then Hole(mar_id);

  while (parent.SpouseToFamilyLinksCount < mar_id) do AddFamily(parent);
  Dec(mar_id);

  family := parent.SpouseToFamilyLinks[mar_id].Family;

  ptr := TGEDCOMPointer.Create(FTree, family);
  ptr.SetNamedValue('CHIL', child);
  family.AddChild(ptr);

  chLink := TGEDCOMChildToFamilyLink.Create(FTree, child);
  chLink.Family := family;
  child.AddChildToFamilyLink(chLink);
end;

function TGKImporter.GetSex(f_name, f_pat: string): TGEDCOMSex;
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

function TGKImporter.DeleteBlanks(const S: string): string;
var
  I: Integer;
begin
  Result := S;

  I := 1;
  while (I <= Length(Result)) do
    if (Result[I] = ' ')
    then Delete(Result, I, 1)
    else Inc(I);
end;

function TGKImporter.ExtractNumComment(const S: string; var Comment: string;
  NoException: Boolean): string;
var
  I: Integer;
begin
  Result := S;
  Comment := '';

  Delete(Result, 1, 1);

  I := 0;
  while (I < Length(Result)) and (Result[I+1] <> ')') do
    Inc(I);

  if (I > 0) then begin
    Comment := Copy(Result, 1, I);
    Delete(Result, 1, I);
  end;

  Delete(Result, 1, 1);
end;

procedure TGKImporter.SetEvent(iRec: TGEDCOMIndividualRecord; evName, date: string);
var
  ev: TGEDCOMCustomEvent;
  prefix, tmp, ym: string;
  toks, i, x: Integer;
  val: array [1..3] of Integer;
begin
  ev := CreateEventEx(FTree, iRec, evName);

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

    ym := '';
    for i := 1 to toks do begin
      tmp := GetToken(date, '.', i);

      x := Pos('/', tmp);
      if (x > 0) then begin
        ym := Copy(tmp, x + 1, Length(tmp) - (x));
        Delete(tmp, x, Length(ym) + 1);
      end;

      val[i] := StrToInt(tmp);
    end;

    case toks of
      1: tmp := IntToStr(val[1]);
      2: tmp := GEDCOMMonthArray[val[1]] + ' ' + IntToStr(val[2]);
      3: tmp := IntToStr(val[1]) + ' ' + GEDCOMMonthArray[val[2]] + ' ' + IntToStr(val[3]);
    end;

    tmp := prefix + tmp;
    if (ym <> '')
    then tmp := tmp + GEDCOMYearModifierSeparator + ym;

    ev.Detail.Date.ParseString(tmp);
  except
    on E: Exception do FLog.Add('>>>> Ошибка разбора: дата "'+date+'"');
  end;
end;

function TGKImporter.ParsePerson(buf: TStringList; aStr, p_id: string; var self_id: Integer): TGEDCOMIndividualRecord;
var
  S, f_name, f_pat, f_fam, bd, dd, com: string;
  sx: TGEDCOMSex;
  x, parent_id, mar_id: Integer;
  parent: TGEDCOMIndividualRecord;
begin
  self_id := -1;
  parent_id := -1;
  mar_id := -1;

  S := DeleteBlanks(p_id);
  S := ExtractNumber(S, self_id, True, -1);
  if (S <> '') and (S[1] = '-') then begin
    Delete(S, 1, 1);
    S := ExtractNumber(S, parent_id, True, -1);

    if (S <> '') and (S[1] = '(') then begin
      S := ExtractNumComment(S, com, True);
    end;

    if (S <> '') and (S[1] = '/') then begin
      Delete(S, 1, 1);
      S := ExtractNumber(S, mar_id, True, -1);
    end;
  end;

  // parse name and dates
  DefinePersonName(aStr, p_id, f_name, f_pat, f_fam, bd, dd);

  sx := GetSex(f_name, f_pat);

  Result := CreatePersonEx(FTree, f_name, f_pat, f_fam, sx, False);
  FPersonsList.AddObject(IntToStr(self_id), Result);

  // в основной комментарий помещается также и первая линия,
  // поэтому доп. комментарий не нужен.
  //if (com <> '') then AddCommentStr(Result, com);

  buf.Add(aStr);

  if (bd <> '') then SetEvent(Result, 'BIRT', bd);
  if (dd <> '') then SetEvent(Result, 'DEAT', dd);

  if (parent_id > 0) then begin
    x := FPersonsList.IndexOf(IntToStr(parent_id));
    if (x >= 0) then begin
      parent := TGEDCOMIndividualRecord(FPersonsList.Objects[x]);
      AddChild(parent, Result, mar_id);
    end else FLog.Add('>>>> Ошибка разбора: в списке не обнаружен предок с номером "'+IntToStr(parent_id)+'".');
  end;
end;

procedure TGKImporter.CheckSpouses(buf: TStringList; iRec: TGEDCOMIndividualRecord);
var
  i, num, p: Integer;
  es, s, name, f_name, f_pat, f_fam, mar_date: string;
  fam: TGEDCOMFamilyRecord;
  sp: TGEDCOMIndividualRecord;
  sx: TGEDCOMSex;
begin
  for i := 0 to buf.Count - 1 do begin
    s := buf[i];

    // warning: temp stub
    while (Length(s) > 0) and (s[1] in [' ', '.']) do Delete(s, 1, 1);

    if (s[1] in ['М', 'Ж']) then begin
      try
        // define sex
        if (s[1] = 'М')
        then sx := svMale
        else sx := svFemale;
        Delete(s, 1, 1);

        // number of spouse
        s := ExtractNumber(s, num, True, 1);

        // skip blanks
        while (Length(s) > 0) and (s[1] = ' ') do Delete(s, 1, 1);

        // extract date of marriage
        if (s[1] = '(') then begin
          p := 0;
          while (p < Length(s)) and not(s[p+1] = ')') do Inc(p);
          if (p > 0) then begin
            Inc(p);
            mar_date := Trim(Copy(s, 1, p));
            Delete(s, 1, p);
          end;
        end;

        // skip interval before name
        while (Length(s) > 0) and (s[1] in [' ', '-']) do Delete(s, 1, 1);

        // extract name
        p := 0;
        while (p < Length(s)) and not(s[p+1] in ['*', '+', '.']) do Inc(p);
        if (p > 0) then begin
          name := Trim(Copy(s, 1, p));
          Delete(s, 1, p);
        end;

        if (name <> '') then begin
          fam := AddFamily(iRec);

          name := Trim(name);
          p := GetTokensCount(name, ' ');
          if (p > 0) then f_name := CheckDot(GetToken(name, ' ', 1));
          if (p > 1) then f_pat := CheckDot(GetToken(name, ' ', 2));
          if (p > 2) then f_fam := CheckDot(GetToken(name, ' ', 3));

          sp := CreatePersonEx(FTree, f_name, f_pat, f_fam, sx, False);
          AddSpouseToFamily(FTree, fam, sp);
        end;
      except
        es := buf[i];
        Hole(es);
        Hole(s);
      end;
    end;
  end;
end;

procedure TGKImporter.CheckBuf(buf: TStringList; iRec: TGEDCOMIndividualRecord);
begin
  CheckSpouses(buf, iRec);

  if (buf.Text <> '') then CreateNoteEx(FTree, buf, iRec {nil});
  buf.Clear;
end;

procedure TGKImporter.Import_PlainText(aFileName: string);
var
  content, buf: TStringList;

  {procedure AddCommentStr(iRec: TGEDCOMIndividualRecord; aStr: string);
  var
    sl: TStringList;
  begin
    sl := TStringList.Create;
    try
      sl.Text := aStr;
      AddNote(FTree, iRec, sl);
    finally
      sl.Destroy;
    end;
  end;}

var
  i, prev_id, self_id: Integer;
  i_rec: TGEDCOMIndividualRecord;
  s, p_id: string;
begin
  FLog.Clear;

  content := TStringList.Create;
  buf := TStringList.Create;
  FPersonsList := TStringList.Create;
  try
    content.LoadFromFile(aFileName);
    content.Add('');

    prev_id := 0;
    i_rec := nil;

    for i := 0 to content.Count - 1 do begin
      s := Trim(content[i]);

      if (s = '') then begin
        CheckBuf(buf, i_rec);
        i_rec := nil;
      end else begin
        if IsRomeLine(s) then begin
          FLog.Add('> Поколение "'+s+'"');
          i_rec := nil;
        end else begin
          if not(IsPersonLine(s, p_id))
          then buf.Add(s)
          else begin
            CheckBuf(buf, i_rec);
            i_rec := ParsePerson(buf, s, p_id, self_id);

            FLog.Add('> Распознана персональная запись "'+p_id+'".');

            if (self_id - prev_id > 1)
            then FLog.Add('>>>> Ошибка разбора: номера записей содержат пропуск.');

            prev_id := self_id;
          end;
        end;
      end;
    end;
  finally
    FPersonsList.Free;
    buf.Free;
    content.Free;
  end;
end;

procedure TGKImporter.Import_Excel(aFileName: string);
var
  excel, sheet: Variant;
  buf: TStringList;
  row, {cols_count, }rows_count, prev_id, self_id: Integer;
  c1, c2, c3, c4, c5, s, p_id: string;
  i_rec: TGEDCOMIndividualRecord;
begin
  try
    excel := GetActiveOleObject('Excel.Application');
  except
    on EOLESysError do excel := CreateOleObject('Excel.Application');
  end;

  excel.Visible := True;
  // не показывать предупреждающие сообщения
  excel.DisplayAlerts := False;
  // открывать Excel на полный экран
  excel.WindowState := -4137;
  // открываем рабочую книгу
  excel.Workbooks.Open(aFileName);
  // становимся на первый лист
  excel.WorkSheets[1].Activate;
  sheet := excel.Sheets[1];
  //sheet := excel.Workbooks[ExtractFileName(aFileName)].WorkSheets[1];

  // получаем используемое количество строк и столбцов
  rows_count := sheet.UsedRange.Rows.Count;
  //cols_count := sheet.UsedRange.Columns.Count;

  FLog.Clear;

  buf := TStringList.Create;
  FPersonsList := TStringList.Create;
  try
    prev_id := 0;
    i_rec := nil;

    for row := 1 to rows_count do begin
      c1 := Trim(sheet.Cells[row, 1].Text);
      c2 := Trim(sheet.Cells[row, 2].Text);
      c3 := Trim(sheet.Cells[row, 3].Text);
      c4 := Trim(sheet.Cells[row, 4].Text);
      c5 := Trim(sheet.Cells[row, 5].Text);
      //sheet.Cells[y, x].Value

      if (c1 = '') and (c3 = '') then begin
        CheckBuf(buf, i_rec);
        i_rec := nil;
      end else begin
        if IsRomeLine(c3) then begin
          FLog.Add('> Поколение "'+c3+'"');
          i_rec := nil;
        end else begin
          if c3[1] = '/'
          then s := c1 + c2 + c3 + ' ' + c4 + ' ' + c5
          else s := c1 + c2 + '. ' + c3 + ' ' + c4 + ' ' + c5;

          if not(IsPersonLine(s, p_id))
          then buf.Add(s)
          else begin
            CheckBuf(buf, i_rec);
            i_rec := ParsePerson(buf, s, p_id, self_id);

            FLog.Add('> Распознана персональная запись "'+p_id+'".');

            if (self_id - prev_id > 1)
            then FLog.Add('>>>> Ошибка разбора: номера записей содержат пропуск.');

            prev_id := self_id;
          end;
        end;
      end;
    end;
  finally
    FPersonsList.Free;
    buf.Free;
  end;

  excel.Quit;
  excel := Unassigned;
end;

procedure TGKImporter.Import_Word(aFileName: string);
var
  msword: Variant;
  s: string;
begin
  FLog.Clear;

  try
    msword := GetActiveOleObject('Word.Application');
  except
    on EOLESysError do msword := CreateOleObject('Word.Application');
  end;

  msword.Visible := True;
  msword.WindowState := -4137;
  msword.Documents.Open(aFileName);

  s := msword.ActiveDocument.Range.Text;
  ExtractStrings([#11], [], PChar(s), FLog);

  msword.Quit;
  msword := Unassigned;
end;

procedure TGKImporter.TreeImportEx(aFileName: string);
var
  E: string;
begin
  E := LowerCase(ExtractFileExt(aFileName));

  if (E = '.txt')
  then Import_PlainText(aFileName)
  else
  if (E = '.csv')
  then //
  else
  if (E = '.doc')
  then Import_Word(aFileName)
  else
  if (E = '.xls')
  then Import_Excel(aFileName)
  else
    raise Exception.Create('Формат не поддерживается');
end;

end.
