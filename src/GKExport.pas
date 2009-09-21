unit GKExport;

interface

uses
  SysUtils, Classes, Contnrs, GedCom551;

procedure WriteStr(aStream: TFileStream; aStr: string);
procedure WriteHeader(aStream: TFileStream; aTitle: string);
procedure WriteFooter(aStream: TFileStream);

{==============================================================================}

type
  TWebExporter = class(TObject)
  private
    FDir: string;
    FTree: TGEDCOMTree;
    FSurnamesCount: Integer;

    procedure WritePersons();
    procedure WriteTimeLineIndex(aStream: TFileStream; evName, tlFileName: string);
    procedure WriteNameIndex(aStream: TFileStream);
    procedure WriteFamilyIndex(aStream: TFileStream);
    procedure WriteSurnamesIndex(aStream: TFileStream; aSym: Char;
      aNames: TStringList);
    procedure WriteIndexFile(aFileName: string; aIndex: TStringList);
    procedure GenTree(aStream: TFileStream; iRec: TGEDCOMIndividualRecord);
  public
    procedure Generate(aDir: string; aTree: TGEDCOMTree);
  end;

{==============================================================================}

procedure ExportToExcel(aDir: string; aTree: TGEDCOMTree);

{==============================================================================}

type
  TPedigreeKind = (pk_dAboville, pk_Konovalov);

  TPersonObj = class
    Parent: TPersonObj;
    Id: string;
    iRec: TGEDCOMIndividualRecord;
    Level: Integer;

    ChildIdx: Integer;
  end;

  TPedigree = class
  private
    FPersonList: TObjectList;

    function FindPerson(iRec: TGEDCOMIndividualRecord): TPersonObj;
    procedure WritePerson(aStream: TFileStream; aTree: TGEDCOMTree;
      aId: string; aPerson: TGEDCOMIndividualRecord);
  public
    procedure Generate(aDir: string; aTree: TGEDCOMTree;
      iRec: TGEDCOMIndividualRecord; aKind: TPedigreeKind);
  end;

implementation

uses
  Windows, ShellAPI, GKCommon, XLSFile, StrUtils, Dialogs;

procedure WriteStr(aStream: TFileStream; aStr: string);
begin
  aStream.WriteBuffer(aStr[1], Length(aStr));
  //aStream.WriteBuffer(sLineBreak[1], 2);
end;

procedure WriteHeader(aStream: TFileStream; aTitle: string);
begin
  WriteStr(aStream, '<html>');
  WriteStr(aStream, '<head>');
  WriteStr(aStream, '<meta HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=windows-1251">');
  WriteStr(aStream, '<title>'+aTitle+'</title>');
  WriteStr(aStream, '</head>');
  WriteStr(aStream, '<body>');
end;

procedure WriteFooter(aStream: TFileStream);
begin
  WriteStr(aStream, '</body>');
  WriteStr(aStream, '</html>');
end;

{==============================================================================}

procedure TWebExporter.WriteIndexFile(aFileName: string; aIndex: TStringList);
var
  fs_index: TFileStream;
  i_rec: TGEDCOMIndividualRecord;
  i: Integer;
begin
  fs_index := TFileStream.Create(aFileName, fmCreate);
  WriteHeader(fs_index, 'Генеалогическая база данных');

  WriteStr(fs_index, '<b>Индекс:</b><ul>');
  for i := 0 to aIndex.Count - 1 do begin
    i_rec := TGEDCOMIndividualRecord(aIndex.Objects[i]);
    WriteStr(fs_index, '<li><a href="persons.htm#' + i_rec.XRef + '">' + aIndex[i] + '</a></li>');
  end;
  WriteStr(fs_index, '</ul><hr>');

  WriteFooter(fs_index);
  fs_index.Destroy;
end;

procedure TWebExporter.WriteSurnamesIndex(aStream: TFileStream; aSym: Char; aNames: TStringList);
var
  index, nList: TStringList;
  f, n, p: string;
  i, idx: Integer;
  i_rec: TGEDCOMIndividualRecord;
begin
  WriteStr(aStream, '<li><b>' + aSym + '</b><ul>');

  index := TStringList.Create;
  try
    for i := 0 to aNames.Count - 1 do begin
      i_rec := TGEDCOMIndividualRecord(aNames.Objects[i]);
      GetNameParts(i_rec, f, n, p);

      f := PrepareRusFamily(f, (i_rec.Sex = svFemale));
      idx := index.IndexOf(f);

      if (idx < 0)
      then idx := index.AddObject(f, TStringList.Create());

      TStringList(index.Objects[idx]).AddObject(aNames[i], i_rec);
    end;

    FSurnamesCount := FSurnamesCount + index.Count;

    for i := 0 to index.Count - 1 do begin
      nList := TStringList(index.Objects[i]);
      WriteStr(aStream, '<li>' + index[i] + ' (' + IntToStr(nList.Count) + ')</li>');
    end;
  finally
    for i := 0 to index.Count - 1 do index.Objects[i].Free;
    index.Destroy;
  end;

  WriteStr(aStream, '</ul></li>');
end;

procedure TWebExporter.WriteFamilyIndex(aStream: TFileStream);
const
  LatSymSet: set of Char = ['A'..'Z'];
  RusSymSet: set of Char = ['А'..'Я'];
type
  TLatSyms = 'A'..'Z';
  TRusSyms = 'А'..'Я';
var
  i: Integer;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  index_str, fam, nam, pat, link, fn: string;
  c: Char;
  lat: array [TLatSyms] of TStringList;
  rus: array [TRusSyms] of TStringList;
  unk: TStringList;
  fs_surnames: TFileStream;
begin
  for c := Low(TLatSyms) to High(TLatSyms) do lat[c] := nil;
  for c := Low(TRusSyms) to High(TRusSyms) do rus[c] := nil;
  unk := nil;

  fs_surnames := TFileStream.Create(FDir + 'index_surnames.htm', fmCreate);
  WriteHeader(fs_surnames, 'Генеалогическая база данных');
  WriteStr(fs_surnames, '<ul>');

  try
    for i := 0 to FTree.Count - 1 do begin
      rec := FTree.Records[i];

      if (rec is TGEDCOMIndividualRecord) then begin
        ind := rec as TGEDCOMIndividualRecord;

        GetNameParts(ind, fam, nam, pat);
        if (fam = '') then begin
          if (unk = nil) then begin
            unk := TStringList.Create;
            unk.Sorted := True;
          end;
          unk.AddObject(GetNameStr(ind), ind);
        end else begin
          c := fam[1];
          if (c in LatSymSet) then begin
            if (lat[c] = nil) then begin
              lat[c] := TStringList.Create;
              lat[c].Sorted := True;
            end;
            lat[c].AddObject(GetNameStr(ind), ind);
          end
          else
          if (c in RusSymSet) then begin
            if (rus[c] = nil) then begin
              rus[c] := TStringList.Create;
              rus[c].Sorted := True;
            end;
            rus[c].AddObject(GetNameStr(ind), ind);
          end
          else begin
            if (unk = nil) then begin
              unk := TStringList.Create;
              unk.Sorted := True;
            end;
            unk.AddObject(GetNameStr(ind), ind);
          end;
        end;
      end;
    end;

    index_str := '<b>Алфавитный:</b> ';

    for c := Low(TLatSyms) to High(TLatSyms) do begin
      if (lat[c] <> nil) then begin
        fn := 'index_fam_' + NumUpdate(Ord(c), 3) + '.htm';
        link := '<a href="' + fn + '">' + c + '</a>';
        index_str := index_str + link + '&nbsp;';
        WriteIndexFile(FDir + fn, lat[c]);
        WriteSurnamesIndex(fs_surnames, c, lat[c]);
      end else begin
        index_str := index_str + c + '&nbsp;';
      end;
    end;
    for c := Low(TRusSyms) to High(TRusSyms) do begin
      if (rus[c] <> nil) then begin
        fn := 'index_fam_' + NumUpdate(Ord(c), 3) + '.htm';
        link := '<a href="' + fn + '">' + c + '</a>';
        index_str := index_str + link + '&nbsp;';
        WriteIndexFile(FDir + fn, rus[c]);
        WriteSurnamesIndex(fs_surnames, c, rus[c]);
      end else begin
        index_str := index_str + c + '&nbsp;';
      end;
    end;

    if (unk <> nil) then begin
      fn := 'index_fam_other.htm';
      link := '<a href="' + fn + '">Другое</a>';
      index_str := index_str + link + '&nbsp;';
      WriteIndexFile(FDir + fn, unk);
      WriteSurnamesIndex(fs_surnames, '?', unk);
    end;

    WriteStr(aStream, '<li>' + index_str + '</li>');
    WriteStr(aStream, '<li><a href="index_surnames.htm">Развернутый</a></li>');
  finally
    WriteStr(fs_surnames, '</ul>');
    WriteFooter(fs_surnames);
    fs_surnames.Destroy;

    for c := Low(TLatSyms) to High(TLatSyms) do lat[c].Free;
    for c := Low(TRusSyms) to High(TRusSyms) do rus[c].Free;
    unk.Free;
  end;
end;

procedure TWebExporter.WriteNameIndex(aStream: TFileStream);
const
  LatSymSet: set of Char = ['A'..'Z'];
  RusSymSet: set of Char = ['А'..'Я'];
type
  TLatSyms = 'A'..'Z';
  TRusSyms = 'А'..'Я';
var
  i: Integer;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  index_str, fam, nam, pat, link, fn: string;
  c: Char;
  lat: array [TLatSyms] of TStringList;
  rus: array [TRusSyms] of TStringList;
  unk: TStringList;
begin
  for c := Low(TLatSyms) to High(TLatSyms) do lat[c] := nil;
  for c := Low(TRusSyms) to High(TRusSyms) do rus[c] := nil;
  unk := nil;

  try
    for i := 0 to FTree.Count - 1 do begin
      rec := FTree.Records[i];

      if (rec is TGEDCOMIndividualRecord) then begin
        ind := rec as TGEDCOMIndividualRecord;

        GetNameParts(ind, fam, nam, pat);

        if (nam = '') then begin
          if (unk = nil) then begin
            unk := TStringList.Create;
            unk.Sorted := True;
          end;
          unk.AddObject(GetNameStr(ind, False), ind);
        end else begin
          c := nam[1];
          if (c in LatSymSet) then begin
            if (lat[c] = nil) then begin
              lat[c] := TStringList.Create;
              lat[c].Sorted := True;
            end;
            lat[c].AddObject(GetNameStr(ind, False), ind);
          end
          else
          if (c in RusSymSet) then begin
            if (rus[c] = nil) then begin
              rus[c] := TStringList.Create;
              rus[c].Sorted := True;
            end;
            rus[c].AddObject(GetNameStr(ind, False), ind);
          end
          else begin
            if (unk = nil) then begin
              unk := TStringList.Create;
              unk.Sorted := True;
            end;
            unk.AddObject(GetNameStr(ind, False), ind);
          end;
        end;
      end;
    end;

    index_str := '<b>Алфавитный:</b> ';

    for c := Low(TLatSyms) to High(TLatSyms) do begin
      if (lat[c] <> nil) then begin
        fn := 'index_nam_' + NumUpdate(Ord(c), 3) + '.htm';
        link := '<a href="' + fn + '">' + c + '</a>';
        index_str := index_str + link + '&nbsp;';
        WriteIndexFile(FDir + fn, lat[c]);
      end else begin
        index_str := index_str + c + '&nbsp;';
      end;
    end;
    for c := Low(TRusSyms) to High(TRusSyms) do begin
      if (rus[c] <> nil) then begin
        fn := 'index_nam_' + NumUpdate(Ord(c), 3) + '.htm';
        link := '<a href="' + fn + '">' + c + '</a>';
        index_str := index_str + link + '&nbsp;';
        WriteIndexFile(FDir + fn, rus[c]);
      end else begin
        index_str := index_str + c + '&nbsp;';
      end;
    end;

    if (unk <> nil) then begin
      fn := 'index_nam_other.htm';
      link := '<a href="' + fn + '">Другое</a>';
      index_str := index_str + link + '&nbsp;';
      WriteIndexFile(FDir + fn, unk);
    end;

    WriteStr(aStream, '<li>' + index_str + '</li>');
  finally
    for c := Low(TLatSyms) to High(TLatSyms) do lat[c].Free;
    for c := Low(TRusSyms) to High(TRusSyms) do rus[c].Free;
    unk.Free;
  end;
end;

procedure TWebExporter.WriteTimeLineIndex(aStream: TFileStream; evName, tlFileName: string);
var
  i, year, k: Integer;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  ev: TGEDCOMIndividualEvent;
  years, fams: TStringList;
  yst: string;
  fs_timeline: TFileStream;
begin
  years := TStringList.Create;
  try
    for i := 0 to FTree.Count - 1 do begin
      rec := FTree.Records[i];

      if (rec is TGEDCOMIndividualRecord) then begin
        ind := rec as TGEDCOMIndividualRecord;

        ev := GetIndividualEvent(ind, evName);
        if (ev = nil)
        then year := -1
        else begin
          year := TGEDCOMDate(ev.Detail.Date.Value).Year;
          if (year = 0) then year := -1;
        end;

        if (year < 0)
        then yst := '?'
        else yst := IntToStr(year);

        k := years.IndexOf(yst);
        if (k < 0)
        then k := years.AddObject(yst, TStringList.Create);

        TStringList(years.Objects[k]).AddObject(GetNameStr(ind), ind);
      end;
    end;

    fs_timeline := TFileStream.Create(FDir + tlFileName, fmCreate);
    WriteHeader(fs_timeline, 'Генеалогическая база данных');
    WriteStr(fs_timeline, '<b>Индекс:</b><ul>');

    years.Sort;
    WriteStr(aStream, '<center>');
    for i := 0 to years.Count - 1 do begin
      WriteStr(aStream, '<a href="' + tlFileName + '#' + years[i] + '">' + years[i] + '</a>');
      if (i < years.Count - 1) then WriteStr(aStream, ' | ');

      WriteStr(fs_timeline, '<li><a name="' + years[i] + '">' + years[i] + '</a><ul>');

      fams := TStringList(years.Objects[i]);
      fams.Sort();
      for k := 0 to fams.Count - 1 do begin
        ind := TGEDCOMIndividualRecord(fams.Objects[k]);
        WriteStr(fs_timeline, '<li><a href="persons.htm#'+ind.XRef+'">' + fams[k] + '</a></li>');
      end;

      WriteStr(fs_timeline, '</ul></li>');
    end;
    WriteStr(aStream, '</center>');

    WriteStr(fs_timeline, '</ul><hr>');
    WriteFooter(fs_timeline);
    fs_timeline.Destroy;
  finally
    for i := 0 to years.Count - 1 do years.Objects[i].Free;
    years.Free;
  end;
end;

type
  TTreePerson = class(TObject)
  private
  public
    iRec: TGEDCOMIndividualRecord;

    FGeneration: Integer;
    FFather: TTreePerson;
    FMother: TTreePerson;
    FName: string;
  end;

procedure TWebExporter.GenTree(aStream: TFileStream; iRec: TGEDCOMIndividualRecord);
var
  gen: Integer;

  function Step(aChild: TTreePerson; aPerson: TGEDCOMIndividualRecord; aGeneration: Integer): TTreePerson;
  var
    family: TGEDCOMFamilyRecord;
    iFather, iMother: TGEDCOMIndividualRecord;
  begin
    if (aPerson = nil) then begin
      Result := nil;
      Exit;
    end;

    Result := TTreePerson.Create();
    Result.FName := GetNameStr(aPerson);
    {Result.BirthDate := GetBirthDate(iRec);
    Result.DeathDate := GetDeathDate(iRec);
    Result.IsDead := not(IsLive(iRec));
    Result.Sex := iRec.Sex;
    Result.Spouse := nil;}
    Result.FGeneration := aGeneration;

    if (gen < aGeneration) then gen := aGeneration;

    if (aPerson.ChildToFamilyLinksCount > 0) then begin
      family := aPerson.ChildToFamilyLinks[0].Family;
      iFather := TGEDCOMIndividualRecord(family.Husband.Value);
      iMother := TGEDCOMIndividualRecord(family.Wife.Value);

      Result.FFather := Step(Result, iFather, aGeneration + 1);
      Result.FMother := Step(Result, iMother, aGeneration + 1);
    end;
  end;

var
  mtx: array of array of string;
  m_rows, m_cols, i, k: Integer;

  function RowsByGens(gens: Integer): Integer;
  var
    rows, i: Integer;
  begin
    rows := 1;
    Result := 1;
    for i := 1 to gens - 1 do begin
      rows := rows * 2;
      Result := Result + rows;
    end;
  end;

  procedure SetMatrix(p: TTreePerson; rb, re: Integer);
  var
    col, row, sz: Integer;
  begin
    if (p = nil) then Exit;
    try
      col := p.FGeneration;

      row := rb + (re - rb + 1) div 2;

      if (col < m_cols) and (row < m_rows) and (col >= 0) and (row >= 0)
      then mtx[row, col] := p.FName;

      sz := RowsByGens(gen - (col + 1));
      SetMatrix(p.FFather, row - sz, row - 1);
      SetMatrix(p.FMother, row + 1, row + sz);
    except
      Hole(col);
      Hole(row);
    end;
  end;

var
  root: TTreePerson;
begin
  try
    gen := 0;
    root := Step(nil, iRec, 0);

    gen := gen + 1;
    m_cols := gen;
    m_rows := RowsByGens(gen);

    SetLength(mtx, m_rows, m_cols);
    for i := 0 to m_rows - 1 do
      for k := 0 to m_cols - 1 do
        mtx[i, k] := '*';

    SetMatrix(root, 0, m_rows - 1);

    WriteStr(aStream, '<table border=1>');
    for i := 0 to m_rows - 1 do begin
      WriteStr(aStream, '<tr>');
      for k := 0 to m_cols - 1 do begin
        WriteStr(aStream, '<td>' + mtx[i, k] + '</td>');
      end;
      WriteStr(aStream, '</tr>');
    end;
    WriteStr(aStream, '</tr></table>');
  except
    on E: Exception do WriteStr(aStream, E.Message);
  end;
end;

procedure TWebExporter.WritePersons();
var
  i: Integer;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  names: TStringList;
  fs_persons: TFileStream;
begin
  fs_persons := TFileStream.Create(FDir + 'persons.htm', fmCreate);
  WriteHeader(fs_persons, 'Генеалогическая база данных');
  WriteStr(fs_persons, '<b>Индекс:</b><ul>');

  names := TStringList.Create;
  try
    for i := 0 to FTree.Count - 1 do begin
      rec := FTree.Records[i];

      if (rec is TGEDCOMIndividualRecord) then begin
        ind := rec as TGEDCOMIndividualRecord;
        names.AddObject(GetNameStr(ind), ind);
      end;
    end;

    names.Sort;
    for i := 0 to names.Count - 1 do begin
      ind := TGEDCOMIndividualRecord(names.Objects[i]);
      WriteStr(fs_persons, '<li><a name="' + ind.XRef + '">' + GetNameStr(ind) + '</a><p>');
      //GenTree(fs_persons, ind);
      WriteStr(fs_persons, '</p></li>');
    end;
  finally
    names.Free;

    WriteStr(fs_persons, '</ul><hr>');
    WriteFooter(fs_persons);
    fs_persons.Destroy;
  end;
end;

procedure TWebExporter.Generate(aDir: string; aTree: TGEDCOMTree);
var
  fs_index: TFileStream;
  stats: TCommonStats;
begin
  FDir := aDir;
  FTree := aTree;

  CreateDir(FDir);

  fs_index := TFileStream.Create(FDir + 'index.htm', fmCreate);
  WriteHeader(fs_index, 'Генеалогическая база данных');

  FSurnamesCount := 0;

  WriteStr(fs_index, '<b>Индекс фамилий:</b><ul>');
  WriteFamilyIndex(fs_index);
  WriteStr(fs_index, '</ul><hr>');

  WriteStr(fs_index, '<b>Индекс имен:</b><ul>');
  WriteNameIndex(fs_index);
  WriteStr(fs_index, '</ul><hr>');

  WriteStr(fs_index, '<b>Индекс годов рождения:</b><ul>');
  WriteTimeLineIndex(fs_index, 'BIRT', 'timeline_birth.htm');
  WriteStr(fs_index, '</ul><hr>');

  WriteStr(fs_index, '<b>Индекс годов смерти:</b><ul>');
  WriteTimeLineIndex(fs_index, 'DEAT', 'timeline_death.htm');
  WriteStr(fs_index, '</ul><hr>');

  WritePersons();

  GetCommonStats(aTree, stats);
  WriteStr(fs_index, '<b>Общая статистика:</b><ul>');
  WriteStr(fs_index, '<li>Персон: ' + IntToStr(stats.persons) + '</li>');
  WriteStr(fs_index, '<li>Фамилий: ' + IntToStr(FSurnamesCount) + '</li>');
  WriteStr(fs_index, '</ul><hr>');

  WriteFooter(fs_index);
  fs_index.Destroy;

  ShellExecute(0, 'open', PChar(FDir + 'index.htm'), nil, nil, SW_SHOW);
end;

{==============================================================================}

procedure ExportToExcel(aDir: string; aTree: TGEDCOMTree);
const
  AllBorders: TSetOfAtribut = [acBottomBorder, acTopBorder,
    acRightBorder, acLeftBorder];
var
  xls: TXLSFile;
  i, row: Integer;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  fam, nam, pat: string;
begin
  xls := TXLSFile.Create(nil);
  try
    xls.AddStrCell( 1, 1, AllBorders, '№');
    xls.AddStrCell( 2, 1, AllBorders, 'Фамилия');
    xls.AddStrCell( 3, 1, AllBorders, 'Имя');
    xls.AddStrCell( 4, 1, AllBorders, 'Отчество');
    xls.AddStrCell( 5, 1, AllBorders, 'Пол');
    xls.AddStrCell( 6, 1, AllBorders, 'Дата рождения');
    xls.AddStrCell( 7, 1, AllBorders, 'Дата смерти');
    xls.AddStrCell( 8, 1, AllBorders, 'Место рождения');
    xls.AddStrCell( 9, 1, AllBorders, 'Место смерти');
    xls.AddStrCell(10, 1, AllBorders, 'Возраст');
    xls.AddStrCell(11, 1, AllBorders, 'Продолжительность жизни');

    row := 1;
    for i := 0 to aTree.Count - 1 do begin
      rec := aTree.Records[i];

      if (rec is TGEDCOMIndividualRecord) then begin
        ind := rec as TGEDCOMIndividualRecord;

        GetNameParts(ind, fam, nam, pat);

        Inc(row);
        xls.AddStrCell( 1, row, AllBorders, IntToStr(GetId(ind)));
        xls.AddStrCell( 2, row, AllBorders, fam);
        xls.AddStrCell( 3, row, AllBorders, nam);
        xls.AddStrCell( 4, row, AllBorders, pat);
        xls.AddStrCell( 5, row, AllBorders, SexSigns[ind.Sex]);
        xls.AddStrCell( 6, row, AllBorders, GetBirthDate(ind, dfDD_MM_YYYY));
        xls.AddStrCell( 7, row, AllBorders, GetDeathDate(ind, dfDD_MM_YYYY));
        xls.AddStrCell( 8, row, AllBorders, GetBirthPlace(ind));
        xls.AddStrCell( 9, row, AllBorders, GetDeathPlace(ind));
        xls.AddStrCell(10, row, AllBorders, GetAge(ind));
        xls.AddStrCell(11, row, AllBorders, GetLifeExpectancy(ind));
      end;
    end;

    xls.FileName := aDir + 'export.xls';
    xls.Write;

    ShellExecute(0, 'open', PChar(xls.FileName), nil, nil, SW_SHOW);
  finally
    xls.Destroy;
  end;
end;

{==============================================================================}

type
  TEventObj = class
    Event: TGEDCOMIndividualEvent;
    iRec: TGEDCOMIndividualRecord;

    function GetDate(): TDateTime;
  end;

function TEventObj.GetDate(): TDateTime;
begin
  if (Event <> nil)
  then Result := GEDCOMDateToDate(Event.Detail.Date.Value)
  else Result := 0;
end;

procedure TPedigree.WritePerson(aStream: TFileStream; aTree: TGEDCOMTree; aId: string; aPerson: TGEDCOMIndividualRecord);
var
  ev_list: TObjectList;

  procedure AddEvent(aEvent: TGEDCOMIndividualEvent; iRec: TGEDCOMIndividualRecord);
  var
    evObj: TEventObj;
  begin
    evObj := TEventObj.Create;
    evObj.Event := aEvent;
    evObj.iRec := iRec;
    ev_list.Add(evObj);
  end;

  function idAnchor(aId: string): string;
  begin
    Result := '<a name="' + aId + '">' + aId + '</a>';
  end;

  function idLink(aObj: TPersonObj): string;
  begin
    Result := '';
    if (aObj = nil) then Exit;

    Result := ' [<a href="#' + aObj.Id + '">' + aObj.Id + '</a>]';
  end;

  procedure WriteEventList();
  var
    i, k, ev: Integer;
    event: TGEDCOMIndividualEvent;
    evObj: TEventObj;
    st, dt: string;
  begin
    // сортировка
    for i := 0 to ev_list.Count - 1 do begin
      for k := i + 1 to ev_list.Count - 1 do begin
        if (TEventObj(ev_list[i]).GetDate() > TEventObj(ev_list[k]).GetDate())
        then ev_list.Exchange(i, k);
      end;
    end;

    for i := 0 to ev_list.Count - 1 do begin
      event := TEventObj(ev_list[i]).Event;

      if (event <> nil) and (TEventObj(ev_list[i]).iRec = aPerson) then begin
        if (event.Name = 'BIRT') then ev_list.Exchange(i, 0)
        else
        if (event.Name = 'DEAT') then ev_list.Exchange(i, ev_list.Count - 1);
      end;
    end;

    // вывод
    for i := 0 to ev_list.Count - 1 do begin
      evObj := TEventObj(ev_list[i]);
      event := evObj.Event;

      if (evObj.iRec = aPerson) then begin
        ev := GetPersonEventIndex(event.Name);
        if (ev = 0) then st := event.Detail.Classification
        else
        if (ev > 0) then st := PersonEvents[ev].Name
        else st := event.Name;

        dt := GEDCOMCustomDateToStr(event.Detail.Date.Value, dfDD_MM_YYYY);

        WriteStr(aStream, '<li>' + dt + ': ' + st + '.');

        if (event.Detail.Place <> '')
        then WriteStr(aStream, ' Место: ' + event.Detail.Place + '</li>');
      end else begin
        if (event = nil)
        then dt := '?'
        else dt := GEDCOMCustomDateToStr(event.Detail.Date.Value, dfDD_MM_YYYY);

        if (evObj.iRec.Sex = svMale) then begin
          st := ': Родился ';
        end else begin
          st := ': Родилась ';
        end;

        WriteStr(aStream, '<li>' + dt + st + GetNameStr(evObj.iRec) + idLink(FindPerson(evObj.iRec)) + '</li>');
      end;
    end;
  end;

var
  i, k: Integer;
  event: TGEDCOMIndividualEvent;
  family: TGEDCOMFamilyRecord;
  irec: TGEDCOMIndividualRecord;
  st, unk: string;
  sp: TGEDCOMPointer;
begin
  WriteStr(aStream, '<li>');

  WriteStr(aStream, '<b>' + idAnchor(aId) + '. ' + GetNameStr(aPerson) + '</b>' + GetLifeStr(aPerson));
  WriteStr(aStream, '<br>Пол: ' + Sex[aPerson.Sex]);

  st := GetLifeExpectancy(aPerson);
  if (st <> '?')
  then WriteStr(aStream, '<br>Продолжительность жизни: ' + st);

  if (aPerson.ChildToFamilyLinksCount <> 0) then begin
    family := aPerson.ChildToFamilyLinks[0].Family;

    irec := TGEDCOMIndividualRecord(family.Husband.Value);
    if (irec <> nil)
    then WriteStr(aStream, '<br>Отец: ' + GetNameStr(irec) + idLink(FindPerson(irec)));

    irec := TGEDCOMIndividualRecord(family.Wife.Value);
    if (irec <> nil)
    then WriteStr(aStream, '<br>Мать: ' + GetNameStr(irec) + idLink(FindPerson(irec)));
  end;


  ev_list := TObjectList.Create(True);
  try
    // загрузка событий
    if (aPerson.IndividualEventsCount > 0) then begin
      WriteStr(aStream, '<br>События: <ul>');
      for i := 0 to aPerson.IndividualEventsCount - 1 do begin
        event := aPerson.IndividualEvents[i];
        AddEvent(event, aPerson);
      end;
      WriteEventList();
      WriteStr(aStream, '</ul>');
    end;

    for i := 0 to aPerson.SpouseToFamilyLinksCount - 1 do begin
      family := aPerson.SpouseToFamilyLinks[i].Family;

      if (aPerson.Sex = svMale) then begin
        sp := family.Wife;
        st := 'Жена: ';
        unk := 'неизвестна';
      end else begin
        sp := family.Husband;
        st := 'Муж: ';
        unk := 'неизвестен';
      end;

      irec := TGEDCOMIndividualRecord(sp.Value);
      if (irec <> nil)
      then WriteStr(aStream, '<br>' + st + GetNameStr(irec) + GetLifeStr(irec) + idLink(FindPerson(irec)))
      else WriteStr(aStream, '<br>' + st + unk);

      WriteStr(aStream, '<br><ul>');
      ev_list.Clear;
      for k := 0 to family.ChildrenCount - 1 do begin
        irec := TGEDCOMIndividualRecord(family.Children[k].Value);
        AddEvent(GetIndividualEvent(irec, 'BIRT'), irec);
      end;
      WriteEventList();
      WriteStr(aStream, '</ul>');
    end;
  finally
    ev_list.Destroy;
  end;

  WriteStr(aStream, '</li><br>');
end;

procedure TPedigree.Generate(aDir: string; aTree: TGEDCOMTree;
  iRec: TGEDCOMIndividualRecord; aKind: TPedigreeKind);
var
  levCount: Integer;
  fs_index: TFileStream;

  procedure Step(aParent: TPersonObj; iRec: TGEDCOMIndividualRecord; aLevel: Integer);
  var
    family: TGEDCOMFamilyRecord;
    child: TGEDCOMIndividualRecord;
    i, k: Integer;
    res: TPersonObj;
  begin
    if (iRec = nil) then Exit;

    res := TPersonObj.Create;
    res.Parent := aParent;
    res.iRec := iRec;
    res.Level := aLevel;
    res.ChildIdx := 0;
    FPersonList.Add(res);

    if (levCount < aLevel)
    then levCount := aLevel;
    //WriteStr(fs_index, GetNameStr(iRec) + ' (' + IntToStr(aLevel) + ')<br>');

    for k := 0 to iRec.SpouseToFamilyLinksCount - 1 do begin
      family := iRec.SpouseToFamilyLinks[k].Family;

      for i := 0 to family.ChildrenCount - 1 do begin
        child := TGEDCOMIndividualRecord(family.Children[i].Value);
        Step(res, child, aLevel + 1);
      end;
    end;
  end;

  procedure ReIndex();
  var
    i, k, p: Integer;
    obj, obj2: TPersonObj;
    pid: string;
  begin
    // сортировка
    for i := 0 to FPersonList.Count - 1 do begin
      for k := i + 1 to FPersonList.Count - 1 do begin
        if (TPersonObj(FPersonList[i]).Level > TPersonObj(FPersonList[k]).Level)
        then FPersonList.Exchange(i, k);
      end;
    end;

    for i := 0 to FPersonList.Count - 1 do begin
      for k := i + 1 to FPersonList.Count - 1 do begin
        obj := TPersonObj(FPersonList[i]);
        obj2 := TPersonObj(FPersonList[k]);

        if (GetNameStr(obj.iRec) > GetNameStr(obj2.iRec))
        and (obj.Level = obj2.Level)
        then FPersonList.Exchange(i, k);
      end;
    end;

    // реиндексация
    for i := 0 to FPersonList.Count - 1 do begin
      obj := TPersonObj(FPersonList[i]);

      case aKind of
        pk_dAboville: begin
          if (obj.Parent = nil)
          then obj.Id := '1'
          else begin
            obj.Parent.ChildIdx := obj.Parent.ChildIdx + 1;
            obj.Id := obj.Parent.Id + '.' + IntToStr(obj.Parent.ChildIdx);
          end;
        end;

        pk_Konovalov: begin
          obj.Id := IntToStr(i + 1);

          if (obj.Parent <> nil) then begin
            pid := obj.Parent.Id;
            p := Pos('-', pid);
            if (p > 0) then pid := Copy(pid, 1, p - 1);

            obj.Id := obj.Id + '-' + pid;
          end;
        end;
      end;
    end;
  end;

var
  title: string;
  i, k: Integer;
  pObj: TPersonObj;
begin
  if (iRec = nil) then begin
    MessageDlg('Не выбрана персональная запись', mtError, [mbOk], 0);
    Exit;
  end;

  title := 'Родословная роспись: ' + GetNameStr(iRec);

  aDir := aDir + 'html\';
  CreateDir(aDir);

  fs_index := TFileStream.Create(aDir + 'pedigree.htm', fmCreate);
  WriteHeader(fs_index, title);
  WriteStr(fs_index, '<h2>' + title + '</h2>');

  FPersonList := TObjectList.Create(True);
  try
    levCount := 0;
    Step(nil, iRec, 1);
    ReIndex();

    for i := 1 to levCount do begin
      WriteStr(fs_index, '<h3>Поколение ' + IntToStr(i) + '</h3>');

      WriteStr(fs_index, '<ul>');
      for k := 0 to FPersonList.Count - 1 do begin
        pObj := TPersonObj(FPersonList[k]);

        if (pObj.Level = i)
        then WritePerson(fs_index, aTree, pObj.Id, pObj.iRec);
      end;
      WriteStr(fs_index, '</ul>');
    end;
  finally
    FPersonList.Destroy;
  end;

  WriteFooter(fs_index);
  fs_index.Destroy;

  ShellExecute(0, 'open', PChar(aDir + 'pedigree.htm'), nil, nil, SW_SHOW);
end;

function TPedigree.FindPerson(iRec: TGEDCOMIndividualRecord): TPersonObj;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to FPersonList.Count - 1 do 
    if (TPersonObj(FPersonList[i]).iRec = iRec) then begin
      Result := TPersonObj(FPersonList[i]);
      Break;
    end;
end;

end.
