unit GKExport;

interface

uses
  SysUtils, Classes, Contnrs, GedCom551, GKCommon;

procedure WriteStr(aStream: TFileStream; aStr: string);
procedure WriteHeader(aStream: TFileStream; aTitle: string);
procedure WriteFooter(aStream: TFileStream);

{==============================================================================}

type
  TExporter = class(TObject)
  private
    FPath: string;
    FTree: TGEDCOMTree;
  public
    constructor Create(aTree: TGEDCOMTree; aPath: string);
    destructor Destroy; override;

    procedure Generate(); virtual; abstract;
  end;

{==============================================================================}

type
  THTMLExporter = class(TExporter)
  private
  public
  end;

{==============================================================================}

type
  TWebExporter = class(THTMLExporter)
  private
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
    procedure Generate(); override;
  end;

{==============================================================================}

type
  TExcelExporter = class(TExporter)
  private
  public
    procedure Generate(); override;
  end;

{==============================================================================}

type
  TPedigreeKind = (pk_dAboville, pk_Konovalov);

  TPersonObj = class
    Parent: TPersonObj;
    Id: string;
    iRec: TGEDCOMIndividualRecord;
    Level: Integer;
    Sources: string;

    ChildIdx: Integer;
  end;

  TPedigree = class
  private
    FPersonList: TObjectList;
    FSourceList: TStringList;
    FOptions: TPedigreeOptions;

    function FindPerson(iRec: TGEDCOMIndividualRecord): TPersonObj;
    procedure WritePerson(aStream: TFileStream; aTree: TGEDCOMTree;
      aPerson: TPersonObj);
  public
    procedure Generate(aDir: string; aTree: TGEDCOMTree;
      iRec: TGEDCOMIndividualRecord; aKind: TPedigreeKind);

    property Options: TPedigreeOptions read FOptions write FOptions;
  end;

implementation

uses
  Windows, ShellAPI, XLSFile, Dialogs, bsComUtils, GKProgress;

procedure WriteStr(aStream: TFileStream; aStr: string);
begin
  aStream.WriteBuffer(aStr[1], Length(aStr));
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

{ TExporter }

constructor TExporter.Create(aTree: TGEDCOMTree; aPath: string);
begin
  FTree := aTree;
  FPath := aPath;
end;

destructor TExporter.Destroy;
begin
  inherited Destroy;
end;

{==============================================================================}

procedure TWebExporter.WriteIndexFile(aFileName: string; aIndex: TStringList);
var
  fs_index: TFileStream;
  i_rec: TGEDCOMIndividualRecord;
  i: Integer;
begin
  fs_index := TFileStream.Create(aFileName, fmCreate);
  try
    WriteHeader(fs_index, 'Генеалогическая база данных');

    WriteStr(fs_index, '<b>Индекс:</b><ul>');
    for i := 0 to aIndex.Count - 1 do begin
      i_rec := TGEDCOMIndividualRecord(aIndex.Objects[i]);
      WriteStr(fs_index, '<li><a href="persons.htm#' + i_rec.XRef + '">' + aIndex[i] + '</a></li>');
    end;
    WriteStr(fs_index, '</ul><hr>');

    WriteFooter(fs_index);
  finally
    fs_index.Destroy;
  end;
end;

procedure TWebExporter.WriteSurnamesIndex(aStream: TFileStream; aSym: Char; aNames: TStringList);
var
  index, nList{, surnames}: TStringList;
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

  fs_surnames := TFileStream.Create(FPath + 'index_surnames.htm', fmCreate);
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
        WriteIndexFile(FPath + fn, lat[c]);
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
        WriteIndexFile(FPath + fn, rus[c]);
        WriteSurnamesIndex(fs_surnames, c, rus[c]);
      end else begin
        index_str := index_str + c + '&nbsp;';
      end;
    end;

    if (unk <> nil) then begin
      fn := 'index_fam_other.htm';
      link := '<a href="' + fn + '">Другое</a>';
      index_str := index_str + link + '&nbsp;';
      WriteIndexFile(FPath + fn, unk);
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
        WriteIndexFile(FPath + fn, lat[c]);
      end else begin
        index_str := index_str + c + '&nbsp;';
      end;
    end;
    for c := Low(TRusSyms) to High(TRusSyms) do begin
      if (rus[c] <> nil) then begin
        fn := 'index_nam_' + NumUpdate(Ord(c), 3) + '.htm';
        link := '<a href="' + fn + '">' + c + '</a>';
        index_str := index_str + link + '&nbsp;';
        WriteIndexFile(FPath + fn, rus[c]);
      end else begin
        index_str := index_str + c + '&nbsp;';
      end;
    end;

    if (unk <> nil) then begin
      fn := 'index_nam_other.htm';
      link := '<a href="' + fn + '">Другое</a>';
      index_str := index_str + link + '&nbsp;';
      WriteIndexFile(FPath + fn, unk);
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

    fs_timeline := TFileStream.Create(FPath + tlFileName, fmCreate);
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
  TCellKind = (ckSpace, ckLine, ckPerson);

  TTreeCell = class(TObject)
  public
    Name: string;
    Kind: TCellKind;
  end;

procedure TWebExporter.GenTree(aStream: TFileStream; iRec: TGEDCOMIndividualRecord);
var
  table_rows: TObjectList;

  procedure AddCell(aRow: TObjectList; iRec: TGEDCOMIndividualRecord; aKind: TCellKind);
  var
    cell: TTreeCell;
  begin
    cell := TTreeCell.Create;
    aRow.Add(cell);

    if (iRec <> nil)
    then cell.Name := GetNameStr(iRec);

    cell.Kind := aKind;
  end;

  procedure Step(row_index, col_index: Integer; prev, cur: TGEDCOMIndividualRecord);
  var
    row: TObjectList;
    i: Integer;
    family: TGEDCOMFamilyRecord;
    iFather, iMother: TGEDCOMIndividualRecord;
  begin
    if (row_index < 0) then row_index := 0;
    if (row_index > table_rows.Count) then row_index := table_rows.Count;

    row := TObjectList.Create;
    table_rows.Insert(row_index, row);

    for i := 0 to col_index - 1 do begin
      AddCell(row, nil, ckSpace);
      if (i < col_index - 1) then AddCell(row, nil, ckSpace);
    end;
    if (prev <> nil) then AddCell(row, nil, ckLine);
    AddCell(row, cur, ckPerson);

    if (cur = nil) then Exit;

    if (cur.ChildToFamilyLinksCount > 0) then begin
      family := cur.ChildToFamilyLinks[0].Family;
      iFather := TGEDCOMIndividualRecord(family.Husband.Value);
      iMother := TGEDCOMIndividualRecord(family.Wife.Value);

      if (iFather <> nil) or (iMother <> nil) then begin
        AddCell(row, nil, ckLine);

        row_index := table_rows.IndexOf(row);
        Step(row_index - 1, col_index + 1, cur, iFather);

        row_index := table_rows.IndexOf(row);
        Step(row_index + 1, col_index + 1, cur, iMother);
      end;
    end;
  end;

var
  r, c: Integer;
  row: TObjectList;
  cell: TTreeCell;
  nm, st: string;
begin
  try
    table_rows := TObjectList.Create(True);
    try
      Step(0, 0, nil, iRec);

      // write table to stream
      WriteStr(aStream, '<table border="0" cellspacing="0">');
      for r := 0 to table_rows.Count - 1 do begin
        row := TObjectList(table_rows[r]);

        WriteStr(aStream, '<tr>');
        for c := 0 to row.Count - 1 do begin
          cell := TTreeCell(row[c]);

          if (cell.Kind = ckSpace) then begin
            nm := '.';//'&nbsp;';
            st := '';
          end else begin
            if (cell.Kind = ckLine)
            then nm := '+'
            else
              if (cell.Name = '')
              then nm := '&nbsp;'
              else nm := cell.Name;

            st := ' bgcolor="silver"';
          end;

          WriteStr(aStream, '<td' + st + '>' + nm + '</td>');
        end;
        WriteStr(aStream, '</tr>');
      end;
      WriteStr(aStream, '</tr></table>');
    finally
      table_rows.Destroy;
    end;
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
  fs_persons := TFileStream.Create(FPath + 'persons.htm', fmCreate);
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
      WriteStr(fs_persons, '<li><a name="' + ind.XRef + '">' + names[i] + '</a><p>');
      GenTree(fs_persons, ind);
      WriteStr(fs_persons, '</p></li>');
    end;
  finally
    names.Free;

    WriteStr(fs_persons, '</ul><hr>');
    WriteFooter(fs_persons);
    fs_persons.Destroy;
  end;
end;

procedure TWebExporter.Generate();
var
  main_index: TFileStream;
  stats: TCommonStats;
begin
  CreateDir(FPath);

  main_index := TFileStream.Create(FPath + 'index.htm', fmCreate);
  try
    WriteHeader(main_index, 'Генеалогическая база данных');

    FSurnamesCount := 0;

    WriteStr(main_index, '<b>Индекс фамилий:</b><ul>');
    WriteFamilyIndex(main_index);
    WriteStr(main_index, '</ul><hr>');

    WriteStr(main_index, '<b>Индекс имен:</b><ul>');
    WriteNameIndex(main_index);
    WriteStr(main_index, '</ul><hr>');

    WriteStr(main_index, '<b>Индекс годов рождения:</b><ul>');
    WriteTimeLineIndex(main_index, 'BIRT', 'timeline_birth.htm');
    WriteStr(main_index, '</ul><hr>');

    WriteStr(main_index, '<b>Индекс годов смерти:</b><ul>');
    WriteTimeLineIndex(main_index, 'DEAT', 'timeline_death.htm');
    WriteStr(main_index, '</ul><hr>');

    WritePersons();

    GetCommonStats(FTree, stats);
    WriteStr(main_index, '<b>Общая статистика:</b><ul>');
    WriteStr(main_index, '<li>Персон: ' + IntToStr(stats.persons) + '</li>');
    WriteStr(main_index, '<li>Фамилий: ' + IntToStr(FSurnamesCount) + '</li>');
    WriteStr(main_index, '</ul><hr>');

    WriteFooter(main_index);
  finally
    main_index.Destroy;
  end;

  ShellExecute(0, 'open', PChar(FPath + 'index.htm'), nil, nil, SW_SHOW);
end;

{==============================================================================}

{ TExcelExporter }

procedure TExcelExporter.Generate();
const
  AllBorders: TSetOfAtribut = [acBottomBorder, acTopBorder, acRightBorder, acLeftBorder];
var
  xls: TXLSFile;
  i, row: Integer;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  fam, nam, pat: string;
begin
  xls := TXLSFile.Create(nil);
  ProgressInit(FTree.Count, 'Экспорт...');
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
    for i := 0 to FTree.Count - 1 do begin
      rec := FTree.Records[i];

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

      ProgressStep();
    end;

    xls.FileName := FPath + 'export.xls';
    xls.Write;

    ShellExecute(0, 'open', PChar(xls.FileName), nil, nil, SW_SHOW);
  finally
    ProgressDone();
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

procedure TPedigree.WritePerson(aStream: TFileStream; aTree: TGEDCOMTree; aPerson: TPersonObj);
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

      if (event <> nil) and (TEventObj(ev_list[i]).iRec = aPerson.iRec) then begin
        if (event.Name = 'BIRT') then ev_list.Exchange(i, 0)
        else
        if (event.Name = 'DEAT') then ev_list.Exchange(i, ev_list.Count - 1);
      end;
    end;

    // вывод
    for i := 0 to ev_list.Count - 1 do begin
      evObj := TEventObj(ev_list[i]);
      event := evObj.Event;

      if (evObj.iRec = aPerson.iRec) then begin
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
  attr: TGEDCOMIndividualAttribute;
  family: TGEDCOMFamilyRecord;
  irec: TGEDCOMIndividualRecord;
  st, unk: string;
  sp: TGEDCOMPointer;
  note: TGEDCOMNotes;
begin
  WriteStr(aStream, '<li>');

  WriteStr(aStream, '<b>' + idAnchor(aPerson.Id) + '. ' + GetNameStr(aPerson.iRec) + '</b>' + GetLifeStr(aPerson.iRec));

  if (FOptions.IncludeSources)
  then WriteStr(aStream, '&nbsp;<sup>' + aPerson.Sources + '</sup>');

  WriteStr(aStream, '<br>Пол: ' + Sex[aPerson.iRec.Sex]);

  st := GetLifeExpectancy(aPerson.iRec);
  if (st <> '?') and (st <> '')
  then WriteStr(aStream, '<br>Продолжительность жизни: ' + st);

  if (FOptions.IncludeAttributes) then begin
    for i := 0 to aPerson.iRec.IndividualAttributesCount - 1 do begin
      attr := aPerson.iRec.IndividualAttributes[i];
      WriteStr(aStream, '<br>' + GetAttributeStr(attr));
    end;
  end;

  if (aPerson.iRec.ChildToFamilyLinksCount <> 0) then begin
    family := aPerson.iRec.ChildToFamilyLinks[0].Family;

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
    if (aPerson.iRec.IndividualEventsCount > 0) then begin
      WriteStr(aStream, '<p>События: <ul>');
      for i := 0 to aPerson.iRec.IndividualEventsCount - 1 do begin
        event := aPerson.iRec.IndividualEvents[i];
        AddEvent(event, aPerson.iRec);
      end;
      WriteEventList();
      WriteStr(aStream, '</ul></p>');
    end;

    for i := 0 to aPerson.iRec.SpouseToFamilyLinksCount - 1 do begin
      family := aPerson.iRec.SpouseToFamilyLinks[i].Family;

      if (aPerson.iRec.Sex = svMale) then begin
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
      then WriteStr(aStream, '<p>' + st + GetNameStr(irec) + GetLifeStr(irec) + idLink(FindPerson(irec)))
      else WriteStr(aStream, '<p>' + st + unk);

      WriteStr(aStream, '<ul>');
      ev_list.Clear;
      for k := 0 to family.ChildrenCount - 1 do begin
        irec := TGEDCOMIndividualRecord(family.Children[k].Value);
        AddEvent(GetIndividualEvent(irec, 'BIRT'), irec);
      end;
      WriteEventList();
      WriteStr(aStream, '</ul></p>');
    end;
  finally
    ev_list.Destroy;
  end;

  if (FOptions.IncludeNotes) and (aPerson.iRec.NotesCount <> 0) then begin
    WriteStr(aStream, '<p>Заметки:<ul>');
    for i := 0 to aPerson.iRec.NotesCount - 1 do begin
      note := aPerson.iRec.Notes[i];
      st := ConStrings(note.Notes);

      WriteStr(aStream, '<li>' + st + '</li>');
    end;
    WriteStr(aStream, '</ul></p>');
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
    sn, src_name: string;
    cit: TGEDCOMSourceCitation;
    sourceRec: TGEDCOMSourceRecord;
  begin
    if (iRec = nil) then Exit;

    res := TPersonObj.Create;
    res.Parent := aParent;
    res.iRec := iRec;
    res.Level := aLevel;
    res.ChildIdx := 0;
    FPersonList.Add(res);

    if (FOptions.IncludeSources) then begin
      for i := 0 to iRec.SourceCitationsCount - 1 do begin
        cit := iRec.SourceCitations[i];
        sourceRec := TGEDCOMSourceRecord(cit.Value);
        if (sourceRec <> nil) then begin
          src_name := ConStrings(sourceRec.Title);
          if (src_name = '')
          then src_name := sourceRec.FiledByEntry;

          k := FSourceList.IndexOf(src_name);
          if (k < 0)
          then k := FSourceList.Add(src_name);

          if (res.Sources <> '') then res.Sources := res.Sources + ',';

          sn := IntToStr(k + 1);
          res.Sources := res.Sources + '<a href="#src' + sn + '">' + sn + '</a>';
        end;
      end;
    end;

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
  title, sn: string;
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
  FSourceList := TStringList.Create;
  try
    levCount := 0;
    Step(nil, iRec, 1);
    ReIndex();

    for i := 1 to levCount do begin
      WriteStr(fs_index, '<h3>Поколение ' + GetRome(i) + '</h3>');

      WriteStr(fs_index, '<ul>');
      for k := 0 to FPersonList.Count - 1 do begin
        pObj := TPersonObj(FPersonList[k]);

        if (pObj.Level = i)
        then WritePerson(fs_index, aTree, pObj);
      end;
      WriteStr(fs_index, '</ul>');
    end;

    WriteStr(fs_index, '<h3>Источники</h3>');
    for i := 0 to FSourceList.Count - 1 do begin
      sn := IntToStr(i + 1);
      WriteStr(fs_index, '<p><sup><a name="src' + sn + '">' + sn + '</a></sup>&nbsp;');
      WriteStr(fs_index, FSourceList[i] + '</p>');
    end;
  finally
    FSourceList.Destroy;
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
