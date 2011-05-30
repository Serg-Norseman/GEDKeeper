unit GKExport; {trans:fin}

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Contnrs, GedCom551, GKEngine, GKCommon;

procedure WriteStr(aStream: TFileStream; aStr: string);
procedure WriteHeader(aStream: TFileStream; aTitle: string);
procedure WriteFooter(aStream: TFileStream);

{==============================================================================}

type
  TExporter = class(TObject)
  private
    FEngine: TGenEngine;
    FOptions: TGlobalOptions;
    FPath: string;
    FTree: TGEDCOMTree;
  public
    constructor Create(aEngine: TGenEngine; aPath: string);
    destructor Destroy; override;

    property Options: TGlobalOptions read FOptions write FOptions;

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

    procedure GenTree(aStream: TFileStream; iRec: TGEDCOMIndividualRecord);
    procedure WritePersons();
    procedure WriteTimeLineIndex(aStream: TFileStream; evName, tlFileName: string);
    procedure WriteNameIndex(aStream: TFileStream);
    procedure WriteFamilyIndex(aStream: TFileStream);
    procedure WriteIndex(aStream: TFileStream; aIndex: TStringList);
  public
    procedure Generate(); override;
  end;

{==============================================================================}

type
  TExcelExporter = class(TExporter)
  private
    FAppMode: Boolean;
    FSelectedRecords: TList;

    procedure ExpXLS();
    procedure ExpApp();
  public
    procedure Generate(); override;

    property AppMode: Boolean read FAppMode write FAppMode;
    property SelectedRecords: TList read FSelectedRecords write FSelectedRecords;
  end;

{==============================================================================}

type
  TPedigreeKind = (pk_dAboville, pk_Konovalov);

  TPersonObj = class(TObject)
  public
    Parent: TPersonObj;
    Id: string;
    iRec: TGEDCOMIndividualRecord;
    Level: Integer;
    BirthDate, Sources: string;
    FamilyOrder: Integer;

    // runtime for dAboville index
    ChildIdx: Integer;

    function GetOrderStr(): string;
  end;

  TPedigree = class
  private
    FKind: TPedigreeKind;
    FOptions: TPedigreeOptions;
    FPersonList: TObjectList;
    FShieldState: TShieldState;
    FSourceList: TStringList;

    function FindPerson(iRec: TGEDCOMIndividualRecord): TPersonObj;
    procedure WritePerson(aStream: TFileStream; aTree: TGEDCOMTree;
      aPerson: TPersonObj);
  public
    procedure Generate(aDir: string; aTree: TGEDCOMTree;
      iRec: TGEDCOMIndividualRecord);

    property Kind: TPedigreeKind read FKind write FKind;
    property Options: TPedigreeOptions read FOptions write FOptions;
    property ShieldState: TShieldState read FShieldState write FShieldState;
  end;

implementation

uses
  {$IFDEF DELPHI_NET} System.IO, {$ENDIF}
  Windows, ShellAPI, XLSFile, Dialogs, Math, Variants, ComObj,
  GKUtils, GKProgress, GKLangs;

procedure WriteStr(aStream: TFileStream; aStr: string);
begin
  aStream.WriteBuffer(aStr[1], Length(aStr));
end;

procedure WriteHeader(aStream: TFileStream; aTitle: string);
begin
  WriteStr(aStream, '<html>');
  WriteStr(aStream, '<head>');
  WriteStr(aStream, '<meta HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=windows-1251">');
  WriteStr(aStream, '<link rel="stylesheet" href="style.css" type="text/css"/>');
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

constructor TExporter.Create(aEngine: TGenEngine; aPath: string);
begin
  inherited Create;
  FEngine := aEngine;
  FTree := FEngine.Tree;
  FPath := aPath;

  if not(DirectoryExists(FPath))
  then CreateDir(FPath);
end;

destructor TExporter.Destroy;
begin
  inherited Destroy;
end;

{==============================================================================}

{ TWebExporter }

procedure TWebExporter.WriteIndex(aStream: TFileStream; aIndex: TStringList);
var
  i_rec: TGEDCOMIndividualRecord;
  i: Integer;
begin
  WriteStr(aStream, '<ul>');
  for i := 0 to aIndex.Count - 1 do begin
    i_rec := TGEDCOMIndividualRecord(aIndex.Objects[i]);
    WriteStr(aStream, '<li><a href="persons.htm#' + i_rec.XRef + '">' + aIndex[i] + '</a></li>');
  end;
  WriteStr(aStream, '</ul>');
end;

procedure TWebExporter.WriteFamilyIndex(aStream: TFileStream);

  procedure WriteSurnames(aStream: TFileStream; aSym: Char; aNames: TStringList);
  var
    index, nList: TStringList;
    f, n, p: string;
    i, idx: Integer;
    i_rec: TGEDCOMIndividualRecord;
  begin
    WriteStr(aStream, '<li><a name="'+NumUpdate(Ord(aSym), 3)+'"><b>' + aSym + '</b></a><ul>');

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
        WriteStr(aStream, '<li><u>' + index[i] + '</u> (' + IntToStr(nList.Count) + ')');
        WriteIndex(aStream, nList);
        WriteStr(aStream, '</li>');
      end;
    finally
      for i := 0 to index.Count - 1 do index.Objects[i].Free;
      index.Free;
    end;

    WriteStr(aStream, '</ul></li>');
  end;

const
  LatSymSet: set of AnsiChar = ['A'..'Z'];
  RusSymSet: set of AnsiChar = ['А'..'Я'];
type
  TLatSyms = 'A'..'Z';
  TRusSyms = 'А'..'Я';
var
  i: Integer;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  index_str, fam, nam, pat: string;
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
  WriteHeader(fs_surnames, LSList[LSID_GenDB]);
  WriteStr(fs_surnames, '<ul>');

  try
    for i := 0 to FTree.RecordsCount - 1 do begin
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

    index_str := '';//'<b>Алфавитный:</b> ';

    for c := Low(TLatSyms) to High(TLatSyms) do begin
      if (lat[c] <> nil) then begin
        index_str := index_str + '<a href="index_surnames.htm#' + NumUpdate(Ord(c), 3) + '">' + c + '</a>&nbsp;';
        WriteSurnames(fs_surnames, c, lat[c]);
      end else begin
        index_str := index_str + c + '&nbsp;';
      end;
    end;

    for c := Low(TRusSyms) to High(TRusSyms) do begin
      if (rus[c] <> nil) then begin
        index_str := index_str + '<a href="index_surnames.htm#' + NumUpdate(Ord(c), 3) + '">' + c + '</a>&nbsp;';
        WriteSurnames(fs_surnames, c, rus[c]);
      end else begin
        index_str := index_str + c + '&nbsp;';
      end;
    end;

    if (unk <> nil) then begin
      index_str := index_str + '<a href="index_surnames.htm#' + NumUpdate(Ord('?'), 3) + '">?</a>' + '&nbsp;';
      WriteSurnames(fs_surnames, '?', unk);
    end;

    WriteStr(aStream, '<li>' + index_str + '</li>');
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
  index_str, fam, nam, pat: string;
  c: Char;
  lat: array [TLatSyms] of TStringList;
  rus: array [TRusSyms] of TStringList;
  unk: TStringList;
  fs_names: TFileStream;
begin
  for c := Low(TLatSyms) to High(TLatSyms) do lat[c] := nil;
  for c := Low(TRusSyms) to High(TRusSyms) do rus[c] := nil;
  unk := nil;

  fs_names := TFileStream.Create(FPath + 'index_names.htm', fmCreate);
  WriteHeader(fs_names, LSList[LSID_GenDB]);
  WriteStr(fs_names, '<ul>');

  try
    for i := 0 to FTree.RecordsCount - 1 do begin
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

    index_str := '';//'<b>Алфавитный:</b> ';

    for c := Low(TLatSyms) to High(TLatSyms) do begin
      if (lat[c] <> nil) then begin
        index_str := index_str + '<a href="index_names.htm#' + NumUpdate(Ord(c), 3) + '">' + c + '</a>&nbsp;';

        WriteStr(fs_names, '<li><a name="'+NumUpdate(Ord(c), 3)+'"><b>' + c + '</b></a>');
        WriteIndex(fs_names, lat[c]);
        WriteStr(fs_names, '</li>');
      end else begin
        index_str := index_str + c + '&nbsp;';
      end;
    end;
    for c := Low(TRusSyms) to High(TRusSyms) do begin
      if (rus[c] <> nil) then begin
        index_str := index_str + '<a href="index_names.htm#' + NumUpdate(Ord(c), 3) + '">' + c + '</a>&nbsp;';

        WriteStr(fs_names, '<li><a name="'+NumUpdate(Ord(c), 3)+'"><b>' + c + '</b></a>');
        WriteIndex(fs_names, rus[c]);
        WriteStr(fs_names, '</li>');
      end else begin
        index_str := index_str + c + '&nbsp;';
      end;
    end;

    if (unk <> nil) then begin
      index_str := index_str + '<a href="index_names.htm#' + NumUpdate(Ord('?'), 3) + '">?</a>&nbsp;';

      WriteStr(fs_names, '<li><a name="'+NumUpdate(Ord('?'), 3)+'"><b>' + '?' + '</b></a>');
      WriteIndex(fs_names, unk);
      WriteStr(fs_names, '</li>');
    end;

    WriteStr(aStream, '<li>' + index_str + '</li>');
  finally
    WriteStr(fs_names, '</ul>');
    WriteFooter(fs_names);
    fs_names.Destroy;

    for c := Low(TLatSyms) to High(TLatSyms) do lat[c].Free;
    for c := Low(TRusSyms) to High(TRusSyms) do rus[c].Free;
    unk.Free;
  end;
end;

procedure TWebExporter.WriteTimeLineIndex(aStream: TFileStream; evName, tlFileName: string);
var
  i, year, k: Integer;
  m, d: Word;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  ev: TGEDCOMCustomEvent;
  years, fams: TStringList;
  yst, index_str: string;
  fs_timeline: TFileStream;
begin
  years := TStringList.Create;
  try
    for i := 0 to FTree.RecordsCount - 1 do begin
      rec := FTree.Records[i];

      if (rec is TGEDCOMIndividualRecord) then begin
        ind := rec as TGEDCOMIndividualRecord;

        ev := GetIndividualEvent(ind, evName);
        if (ev = nil)
        then year := -1
        else begin
          GetIndependentDate(ev.Detail.Date.Value, year, m, d);
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
    try
      WriteHeader(fs_timeline, LSList[LSID_GenDB]);
      WriteStr(fs_timeline, '<b>Индекс:</b><ul>');

      years.Sort;
      index_str := '';
      for i := 0 to years.Count - 1 do begin
        index_str := index_str + '<a href="' + tlFileName + '#' + years[i] + '">' + years[i] + '</a>';
        if (i < years.Count - 1) then index_str := index_str + ' | ';

        ////

        WriteStr(fs_timeline, '<li><a name="' + years[i] + '">' + years[i] + '</a><ul>');
        fams := TStringList(years.Objects[i]);
        fams.Sort();
        for k := 0 to fams.Count - 1 do begin
          ind := TGEDCOMIndividualRecord(fams.Objects[k]);
          WriteStr(fs_timeline, '<li><a href="persons.htm#'+ind.XRef+'">' + fams[k] + '</a></li>');
        end;
        WriteStr(fs_timeline, '</ul></li>');
      end;
      WriteStr(aStream, '<center>' + index_str + '</center>');

      WriteStr(fs_timeline, '</ul><hr>');
      WriteFooter(fs_timeline);
    finally
      fs_timeline.Destroy;
    end;
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

    ColIndex: Integer;
    Row: TObjectList;
    
    Rec: TGEDCOMIndividualRecord;
  end;

procedure TWebExporter.GenTree(aStream: TFileStream; iRec: TGEDCOMIndividualRecord);
var
  table_rows: TObjectList;

  function AddCell(aRow: TObjectList; iRec: TGEDCOMIndividualRecord; aKind: TCellKind): TTreeCell;
  begin
    Result := TTreeCell.Create;
    Result.ColIndex := aRow.Add(Result);
    Result.Kind := aKind;
    Result.Rec := iRec;
    Result.Row := aRow;

    if (iRec <> nil)
    then Result.Name := GetNameStr(iRec);
  end;

  function AddRow(aRowIndex: Integer; aSpaces: Integer = 0): TObjectList;
  begin
    Result := TObjectList.Create;
    table_rows.Insert(aRowIndex, Result);
  end;

  procedure WideTable(cols: Integer);
  var
    i: Integer;
    row: TObjectList;
  begin
    for i := 0 to table_rows.Count - 1 do begin
      row := TObjectList(table_rows[i]);
      while (row.Count < cols) do
        AddCell(row, nil, ckSpace);
    end;
  end;

  procedure DrawLine(cell_ancestor, cell_descendant: TTreeCell);
  var
    x, y, r1, r2: Integer;
    row: TObjectList;
  begin
    r1 := table_rows.IndexOf(cell_descendant.Row);
    r2 := table_rows.IndexOf(cell_ancestor.Row);

    if (r1 > r2) then begin
      y := r2;
      r2 := r1;
      r1 := y;
    end;

    x := (cell_descendant.ColIndex - 1);

    for y := r1 to r2 do begin
      row := TObjectList(table_rows[y]);
      TTreeCell(row[x]).Kind := ckLine;
    end;
  end;

  procedure Step(row_index, col_index: Integer; prev: TTreeCell; cur: TGEDCOMIndividualRecord);
  var
    row: TObjectList;
    i: Integer;
    family: TGEDCOMFamilyRecord;
    iFather, iMother: TGEDCOMIndividualRecord;
    cur_cell: TTreeCell;
  begin
    if (cur = nil) then Exit;

    if (row_index < 0) then row_index := 0;
    if (row_index > table_rows.Count) then row_index := table_rows.Count;

    row := AddRow(row_index);

    for i := 0 to (col_index - 1) * 2 do AddCell(row, nil, ckSpace);
    if (prev <> nil) then AddCell(row, nil, ckLine);

    cur_cell := AddCell(row, cur, ckPerson);

    if (cur.ChildToFamilyLinksCount > 0) then begin
      family := cur.ChildToFamilyLinks[0].Family;
      iFather := TGEDCOMIndividualRecord(family.Husband.Value);
      iMother := TGEDCOMIndividualRecord(family.Wife.Value);

      if (iFather <> nil) or (iMother <> nil) then begin
        AddCell(row, nil, ckLine);
        AddCell(row, nil, ckSpace);

        row_index := table_rows.IndexOf(row);
        if (iFather <> nil) then AddRow(row_index, col_index + 1);
        Step(row_index, col_index + 1, cur_cell, iFather);

        row_index := table_rows.IndexOf(row);
        if (iMother <> nil) then AddRow(row_index + 1, col_index + 1);
        Step(row_index + 2, col_index + 1, cur_cell, iMother);
      end;
    end;

    WideTable(cur_cell.ColIndex + 1);

    if (prev <> nil)
    then DrawLine(prev, cur_cell);
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

          nm := '&nbsp;';
          st := '';

          if (cell.Kind <> ckSpace) then begin
            if (cell.Kind = ckPerson)
            then begin
              if (cell.Name <> '')
              then nm := '<a href="#' + cell.Rec.XRef + '">' + cell.Name + '</a>';
            end;

            st := ' bgcolor="silver"';
          end;

          WriteStr(aStream, '<td' + st + '>' + nm + '</td>');
        end;
        WriteStr(aStream, '</tr>');
      end;
      WriteStr(aStream, '</tr></table>');
    finally
      table_rows.Free;
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
  WriteHeader(fs_persons, LSList[LSID_GenDB]);
  WriteStr(fs_persons, '<b>'+LSList[LSID_GenIndex]+':</b><ul>');

  names := TStringList.Create;
  try
    for i := 0 to FTree.RecordsCount - 1 do begin
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
  main_index := TFileStream.Create(FPath + 'index.htm', fmCreate);
  try
    WriteHeader(main_index, LSList[LSID_GenDB]);

    FSurnamesCount := 0;

    //WriteStr(main_index, '<form><table><tr><td>');

    WriteStr(main_index, '<b>'+LSList[LSID_SurnamesIndex]+':</b><ul>');
    WriteFamilyIndex(main_index);
    WriteStr(main_index, '</ul><hr>');

    WriteStr(main_index, '<b>'+LSList[LSID_NamesIndex]+':</b><ul>');
    WriteNameIndex(main_index);
    WriteStr(main_index, '</ul><hr>');

    WriteStr(main_index, '<b>'+LSList[LSID_BirthIndex]+':</b><ul>');
    WriteTimeLineIndex(main_index, 'BIRT', 'index_birth.htm');
    WriteStr(main_index, '</ul><hr>');

    WriteStr(main_index, '<b>'+LSList[LSID_DeathIndex]+':</b><ul>');
    WriteTimeLineIndex(main_index, 'DEAT', 'index_death.htm');
    WriteStr(main_index, '</ul><hr>');

    WritePersons();

    {fixme!!!}
    FEngine.GetCommonStats(stats);
    WriteStr(main_index, '<b>'+LSList[LSID_CommonStats]+':</b><ul>');
    WriteStr(main_index, '<li>Персон: ' + IntToStr(stats.persons) + '</li>');
    WriteStr(main_index, '<li>Фамилий: ' + IntToStr(FSurnamesCount) + '</li>');
    WriteStr(main_index, '</ul><hr>');

    //WriteStr(main_index, '</td></tr></table></form>');

    WriteFooter(main_index);
  finally
    main_index.Destroy;
  end;

  LoadExtFile(FPath + 'index.htm');
end;

{==============================================================================}

{ TExcelExporter }

procedure TExcelExporter.ExpXLS();
const
  AllBorders: TSetOfAttribute = [acBottomBorder, acTopBorder, acRightBorder, acLeftBorder];
var
  xls: TXLSFile;
  i, row: Integer;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  fam, nam, pat: string;
  fname: string;
begin
  xls := TXLSFile.Create(nil);
  ProgressInit(FTree.RecordsCount, LSList[LSID_MIExport]+'...');
  try
    xls.AddStrCell( 1, 1, AllBorders, '№');
    xls.AddStrCell( 2, 1, AllBorders, LSList[LSID_Surname]);
    xls.AddStrCell( 3, 1, AllBorders, LSList[LSID_Name]);
    xls.AddStrCell( 4, 1, AllBorders, LSList[LSID_Patronymic]);
    xls.AddStrCell( 5, 1, AllBorders, LSList[LSID_Sex]);
    xls.AddStrCell( 6, 1, AllBorders, LSList[LSID_BirthDate]);
    xls.AddStrCell( 7, 1, AllBorders, LSList[LSID_DeathDate]);
    xls.AddStrCell( 8, 1, AllBorders, LSList[LSID_BirthPlace]);
    xls.AddStrCell( 9, 1, AllBorders, LSList[LSID_DeathPlace]);
    xls.AddStrCell(10, 1, AllBorders, LSList[LSID_Residence]);
    xls.AddStrCell(11, 1, AllBorders, LSList[LSID_Age]);
    xls.AddStrCell(12, 1, AllBorders, LSList[LSID_LifeExpectancy]);

    row := 1;
    for i := 0 to FTree.RecordsCount - 1 do begin
      rec := FTree.Records[i];

      if (rec is TGEDCOMIndividualRecord) then begin
        ind := rec as TGEDCOMIndividualRecord;

        if (FSelectedRecords <> nil) then begin
          if (FSelectedRecords.IndexOf(rec) < 0) then Continue;
        end;

        GetNameParts(ind, fam, nam, pat);

        Inc(row);
        xls.AddStrCell( 1, row, AllBorders, IntToStr(GetId(ind)));
        xls.AddStrCell( 2, row, AllBorders, fam);
        xls.AddStrCell( 3, row, AllBorders, nam);
        xls.AddStrCell( 4, row, AllBorders, pat);
        xls.AddStrCell( 5, row, AllBorders, SexStr(ind.Sex)[1]);
        xls.AddStrCell( 6, row, AllBorders, GetBirthDate(ind, dfDD_MM_YYYY));
        xls.AddStrCell( 7, row, AllBorders, GetDeathDate(ind, dfDD_MM_YYYY));
        xls.AddStrCell( 8, row, AllBorders, GetBirthPlace(ind));
        xls.AddStrCell( 9, row, AllBorders, GetDeathPlace(ind));
        xls.AddStrCell(10, row, AllBorders, GetResidencePlace(ind, FOptions.PlacesWithAddress));
        xls.AddStrCell(11, row, AllBorders, GetAge(ind));
        xls.AddStrCell(12, row, AllBorders, GetLifeExpectancy(ind));
      end;

      ProgressStep();
    end;

    fname := FPath + 'export.xls';
    xls.SaveToFile(fname);

    LoadExtFile(fname);
  finally
    ProgressDone();
    xls.Destroy;
  end;
end;

procedure TExcelExporter.ExpApp();
var
  excel, sheet, wb: Variant;
  i, row: Integer;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  fam, nam, pat: string;
begin
  {$IFNDEF DELPHI_NET}
  try
    ProgressInit(FTree.RecordsCount, LSList[LSID_MIExport]+'...');

    try
      try
        excel := GetActiveOleObject('Excel.Application');
      except
        on EOLESysError do excel := CreateOleObject('Excel.Application');
      end;

      excel.Visible := True;
      excel.DisplayAlerts := False;
      excel.WindowState := -4137;
      wb := excel.Workbooks.Add;
      wb.Activate;
      sheet := excel.Sheets[1];

      sheet.Cells[1,  1] := '№';
      sheet.Cells[1,  2] := LSList[LSID_Surname];
      sheet.Cells[1,  3] := LSList[LSID_Name];
      sheet.Cells[1,  4] := LSList[LSID_Patronymic];
      sheet.Cells[1,  5] := LSList[LSID_Sex];
      sheet.Cells[1,  6] := LSList[LSID_BirthDate];
      sheet.Cells[1,  7] := LSList[LSID_DeathDate];
      sheet.Cells[1,  8] := LSList[LSID_BirthPlace];
      sheet.Cells[1,  9] := LSList[LSID_DeathPlace];
      sheet.Cells[1, 10] := LSList[LSID_Residence];
      sheet.Cells[1, 11] := LSList[LSID_Age];
      sheet.Cells[1, 12] := LSList[LSID_LifeExpectancy];

      row := 1;
      for i := 0 to FTree.RecordsCount - 1 do begin
        rec := FTree.Records[i];

        if (rec is TGEDCOMIndividualRecord) then begin
          ind := rec as TGEDCOMIndividualRecord;

          if (FSelectedRecords <> nil) then begin
            if (FSelectedRecords.IndexOf(rec) < 0) then Continue;
          end;

          GetNameParts(ind, fam, nam, pat);

          Inc(row);
          sheet.Cells[row,  1] := IntToStr(GetId(ind));
          sheet.Cells[row,  2] := fam;
          sheet.Cells[row,  3] := nam;
          sheet.Cells[row,  4] := pat;
          sheet.Cells[row,  5] := SexStr(ind.Sex)[1];
          sheet.Cells[row,  6] := GetBirthDate(ind, dfDD_MM_YYYY);
          sheet.Cells[row,  7] := GetDeathDate(ind, dfDD_MM_YYYY);
          sheet.Cells[row,  8] := GetBirthPlace(ind);
          sheet.Cells[row,  9] := GetDeathPlace(ind);
          sheet.Cells[row, 10] := GetResidencePlace(ind, FOptions.PlacesWithAddress);
          sheet.Cells[row, 11] := GetAge(ind);
          sheet.Cells[row, 12] := GetLifeExpectancy(ind);
        end;

        ProgressStep();
      end;
    finally
      //excel.Quit;
      //excel := Unassigned;

      ProgressDone();
    end;
  except
    on E: Exception do begin
      LogWrite('TExcelExporter.ExpApp(): ' + E.Message);
    end;
  end;
  {$ENDIF}
end;

procedure TExcelExporter.Generate();
begin
  if AppMode
  then ExpApp()
  else ExpXLS();
end;

{==============================================================================}

function TPersonObj.GetOrderStr(): string;
var
  order: Char;
begin
  order := Chr(FamilyOrder);

  if (Parent = nil)
  then Result := order
  else Result := Parent.GetOrderStr() + order;
end;

{ TPedigree }

type
  TEventObj = class
    Event: TGEDCOMCustomEvent;
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
const
  p_style = ' style="margin-top:2pt; margin-bottom:2pt"';
var
  ev_list: TObjectList;

  procedure AddEvent(aEvent: TGEDCOMCustomEvent; iRec: TGEDCOMIndividualRecord);
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
    event: TGEDCOMCustomEvent;
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
        if (ev > 0) then st := LSList[PersonEvents[ev].Name]
        else st := event.Name;

        dt := GEDCOMCustomDateToStr(event.Detail.Date.Value, dfDD_MM_YYYY);

        WriteStr(aStream, '<li>' + dt + ': ' + st + '.');

        if (event.Detail.Place.StringValue <> '')
        then WriteStr(aStream, ' '+LSList[LSID_Place]+': ' + event.Detail.Place.StringValue + '</li>');
      end else begin
        if (event = nil)
        then dt := '?'
        else dt := GEDCOMCustomDateToStr(event.Detail.Date.Value, dfDD_MM_YYYY);

        {fixme!}
        if (evObj.iRec.Sex = svMale) then begin
          st := ': Родился ';
        end else begin
          st := ': Родилась ';
        end;

        WriteStr(aStream, '<li>' + dt + st + GetNameStr(evObj.iRec) + idLink(FindPerson(evObj.iRec)) + '</li>');
      end;
    end;
  end;

  function GetIdStr(): string;
  var
    family: TGEDCOMFamilyRecord;
    idx: Integer;
    sp_str: string;
  begin
    Result := idAnchor(aPerson.Id);

    if (FKind = pk_Konovalov) and (aPerson.Parent <> nil) then begin
      family := aPerson.iRec.ChildToFamilyLinks[0].Family;

      sp_str := '';
      idx := aPerson.Parent.iRec.IndexOfSpouse(family);
      if (aPerson.Parent.iRec.SpouseToFamilyLinksCount > 1)
      then sp_str := '/' + IntToStr(idx + 1);

      Result := Result + sp_str;
    end;
  end;

  function GetPedigreeLifeStr(iRec: TGEDCOMIndividualRecord): string;
  var
    ds, ps, res_str: string;
    ev: TGEDCOMCustomEvent;
  begin
    res_str := '';

    case FOptions.Format of
      pfExcess: begin
        ds := GetBirthDate(iRec, dfDD_MM_YYYY, True);
        if (ds = '') then ds := '?';
        res_str := res_str + ds;

        ds := GetDeathDate(iRec, dfDD_MM_YYYY, True);
        if (ds = '') then begin
          ev := GetIndividualEvent(iRec, 'DEAT');
          if (ev <> nil) then ds := '?';
        end;

        if (ds <> '')
        then res_str := res_str + ' - ' + ds;
      end;

      pfCompact: begin
        ds := GetBirthDate(iRec, dfDD_MM_YYYY, True);
        ps := GetBirthPlace(iRec);
        if (ps <> '') then begin
          if (ds <> '') then ds := ds + ', ';
          ds := ds + ps;
        end;
        if (ds <> '') then ds := '*' + ds;
        res_str := res_str + ds;

        ds := GetDeathDate(iRec, dfDD_MM_YYYY, True);
        ps := GetDeathPlace(iRec);
        if (ps <> '') then begin
          if (ds <> '') then ds := ds + ', ';
          ds := ds + ps;
        end;
        if (ds <> '') then ds := '+' + ds;

        if (ds <> '')
        then res_str := res_str + ' ' + ds;
      end;
    end;

    if (res_str = '') or (res_str = ' ')
    then Result := ''
    else Result := ' (' + res_str + ')';
  end;

  procedure WriteExcessFmt();
  var
    i, k: Integer;
    event: TGEDCOMCustomEvent;
    family: TGEDCOMFamilyRecord;
    irec: TGEDCOMIndividualRecord;
    st, unk: string;
    sp: TGEDCOMPointer;
    note: TGEDCOMNotes;
  begin
    WriteStr(aStream, '<br>'+LSList[LSID_Sex]+': ' + SexStr(aPerson.iRec.Sex));

    st := GetLifeExpectancy(aPerson.iRec);
    if (st <> '?') and (st <> '')
    then WriteStr(aStream, '<br>'+LSList[LSID_LifeExpectancy]+': ' + st);

    if (aPerson.iRec.ChildToFamilyLinksCount <> 0) then begin
      family := aPerson.iRec.ChildToFamilyLinks[0].Family;

      irec := TGEDCOMIndividualRecord(family.Husband.Value);
      if (irec <> nil)
      then WriteStr(aStream, '<br>'+LSList[LSID_Father]+': ' + GetNameStr(irec) + idLink(FindPerson(irec)));

      irec := TGEDCOMIndividualRecord(family.Wife.Value);
      if (irec <> nil)
      then WriteStr(aStream, '<br>'+LSList[LSID_Mother]+': ' + GetNameStr(irec) + idLink(FindPerson(irec)));
    end;

    ev_list := TObjectList.Create(True);
    try
      // загрузка событий
      if (aPerson.iRec.IndividualEventsCount > 0) then begin
        WriteStr(aStream, '<p>'+LSList[LSID_Events]+': <ul>');
        for i := 0 to aPerson.iRec.IndividualEventsCount - 1 do begin
          event := aPerson.iRec.IndividualEvents[i];

          if not(event is TGEDCOMIndividualAttribute)
          or ((event is TGEDCOMIndividualAttribute) and (FOptions.IncludeAttributes))
          then AddEvent(event, aPerson.iRec);
        end;
        WriteEventList();
        WriteStr(aStream, '</ul></p>');
      end;

      for i := 0 to aPerson.iRec.SpouseToFamilyLinksCount - 1 do begin
        family := aPerson.iRec.SpouseToFamilyLinks[i].Family;
        if not(IsRecordAccess(family.Restriction, FShieldState)) then Continue;

        if (aPerson.iRec.Sex = svMale) then begin
          sp := family.Wife;
          st := LSList[LSID_Wife]+': ';
          unk := LSList[LSID_UnkFemale];
        end else begin
          sp := family.Husband;
          st := LSList[LSID_Husband]+': ';
          unk := LSList[LSID_UnkMale];
        end;

        irec := TGEDCOMIndividualRecord(sp.Value);
        if (irec <> nil)
        then WriteStr(aStream, '<p>' + st + GetNameStr(irec) + GetPedigreeLifeStr(irec) + idLink(FindPerson(irec)))
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
      ev_list.Free;
    end;

    if (FOptions.IncludeNotes) and (aPerson.iRec.NotesCount <> 0) then begin
      WriteStr(aStream, '<p>'+LSList[LSID_RPNotes]+':<ul>');
      for i := 0 to aPerson.iRec.NotesCount - 1 do begin
        note := aPerson.iRec.Notes[i];
        WriteStr(aStream, '<li>' + ConStrings(note.Notes) + '</li>');
      end;
      WriteStr(aStream, '</ul></p>');
    end;
  end;

  procedure WriteCompactFmt();
  var
    i: Integer;
    family: TGEDCOMFamilyRecord;
    irec: TGEDCOMIndividualRecord;
    st, unk: string;
    sp: TGEDCOMPointer;
    note: TGEDCOMNotes;
    sp_index: Boolean;
  begin
    if (FOptions.IncludeNotes) and (aPerson.iRec.NotesCount <> 0) then begin
      for i := 0 to aPerson.iRec.NotesCount - 1 do begin
        note := aPerson.iRec.Notes[i];
        WriteStr(aStream, '<p'+p_style+'>' + ConStrings(note.Notes) + '</p>');
      end;
    end;

    try
      sp_index := (aPerson.iRec.SpouseToFamilyLinksCount > 1);

      for i := 0 to aPerson.iRec.SpouseToFamilyLinksCount - 1 do begin
        family := aPerson.iRec.SpouseToFamilyLinks[i].Family;
        if not(IsRecordAccess(family.Restriction, FShieldState)) then Continue;

        if (aPerson.iRec.Sex = svMale) then begin
          sp := family.Wife;
          st := 'Ж';
          unk := LSList[LSID_UnkFemale];
        end else begin
          sp := family.Husband;
          st := 'М';
          unk := LSList[LSID_UnkMale];
        end;

        if (sp_index) then st := st + IntToStr(i + 1);
        st := st + ' - ';

        irec := TGEDCOMIndividualRecord(sp.Value);
        if (irec <> nil)
        then st := st + GetNameStr(irec) + GetPedigreeLifeStr(irec) + idLink(FindPerson(irec))
        else st := st + unk;

        WriteStr(aStream, '<p'+p_style+'>' + st + '</p>');
      end;
    finally
    end;
  end;

begin
  WriteStr(aStream, '<li>');
  WriteStr(aStream, '<b>' + GetIdStr() + '. ' + GetNameStr(aPerson.iRec) + '</b>' + GetPedigreeLifeStr(aPerson.iRec));

  if (FOptions.IncludeSources)
  then WriteStr(aStream, '&nbsp;<sup>' + aPerson.Sources + '</sup>');

  case FOptions.Format of
    pfExcess: WriteExcessFmt();
    pfCompact: WriteCompactFmt();
  end;

  WriteStr(aStream, '</li><br>');
end;

procedure TPedigree.Generate(aDir: string; aTree: TGEDCOMTree;
  iRec: TGEDCOMIndividualRecord);
var
  fs_index: TFileStream;

  procedure Step(aParent: TPersonObj; iRec: TGEDCOMIndividualRecord; aLevel, aFamilyOrder: Integer);
  var
    family: TGEDCOMFamilyRecord;
    child: TGEDCOMIndividualRecord;
    i, k: Integer;
    res: TPersonObj;
    sn, src_name, i_sources: string;
    cit: TGEDCOMSourceCitation;
    sourceRec: TGEDCOMSourceRecord;
  begin
    if (iRec = nil) then Exit;

    //

    res := TPersonObj.Create;
    res.Parent := aParent;
    res.iRec := iRec;
    res.Level := aLevel;
    res.ChildIdx := 0;
    res.BirthDate := GetBirthDate(iRec, dfYYYY_MM_DD, True);
    res.FamilyOrder := aFamilyOrder;
    FPersonList.Add(res);

    i_sources := '';
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

          if (i_sources <> '') then i_sources := i_sources + ',';

          sn := IntToStr(k + 1);
          i_sources := i_sources + '<a href="#src' + sn + '">' + sn + '</a>';
        end;
      end;
    end;
    res.Sources := i_sources;

    //

    for k := 0 to iRec.SpouseToFamilyLinksCount - 1 do begin
      family := iRec.SpouseToFamilyLinks[k].Family;
      if not(IsRecordAccess(family.Restriction, FShieldState)) then Continue;

      family.SortChilds();

      for i := 0 to family.ChildrenCount - 1 do begin
        child := TGEDCOMIndividualRecord(family.Children[i].Value);
        Step(res, child, aLevel + 1, i + 1);
      end;
    end;
  end;

  procedure ReIndex();
  var
    i, k, p: Integer;
    obj, obj2: TPersonObj;
    pid, i_str, k_str: string;
  begin
    //// сортировка
    // не самая хорошая реализация по эффективности и скорости...
    // но зато получается индексированная сортировка по нескольким критериям
    for i := 0 to FPersonList.Count - 1 do begin
      for k := i + 1 to FPersonList.Count - 1 do begin
        obj := TPersonObj(FPersonList[i]);
        obj2 := TPersonObj(FPersonList[k]);

        i_str := Chr(obj.Level) + obj.GetOrderStr();
        k_str := Chr(obj2.Level) + obj2.GetOrderStr();

        if (i_str > k_str)
        then FPersonList.Exchange(i, k);
      end;
    end;

    //// реиндексация в общепринятых форматах
    for i := 0 to FPersonList.Count - 1 do begin
      obj := TPersonObj(FPersonList[i]);

      case FKind of
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
  i, k, cur_level: Integer;
  pObj: TPersonObj;
begin
  if (iRec = nil) then begin
    MessageDlg(LSList[LSID_NotSelectedPerson], mtError, [mbOk], 0);
    Exit;
  end;

  title := LSList[LSID_ExpPedigree]+': ' + GetNameStr(iRec);

  CreateDir(aDir);

  fs_index := TFileStream.Create(aDir + 'pedigree.htm', fmCreate);
  WriteHeader(fs_index, title);
  WriteStr(fs_index, '<h2>' + title + '</h2>');

  FPersonList := TObjectList.Create(True);
  FSourceList := TStringList.Create;
  try
    Step(nil, iRec, 1, 1);
    ReIndex();

    // вывод по поколениям
    cur_level := 0;
    for k := 0 to FPersonList.Count - 1 do begin
      pObj := TPersonObj(FPersonList[k]);

      // определение момента вывода заголовка поколения
      if (cur_level <> pObj.Level) then begin
        if (cur_level > 0) then WriteStr(fs_index, '</ul>');

        cur_level := pObj.Level;

        WriteStr(fs_index, '<h3>'+LSList[LSID_Generation]+' ' + GetRome(cur_level) + '</h3><ul>');
      end;

      WritePerson(fs_index, aTree, pObj);
    end;
    WriteStr(fs_index, '</ul>');

    if (FSourceList.Count > 0) then begin
      WriteStr(fs_index, '<h3>'+LSList[LSID_RPSources]+'</h3>');
      for i := 0 to FSourceList.Count - 1 do begin
        sn := IntToStr(i + 1);
        WriteStr(fs_index, '<p><sup><a name="src' + sn + '">' + sn + '</a></sup>&nbsp;');
        WriteStr(fs_index, FSourceList[i] + '</p>');
      end;
    end;
  finally
    FSourceList.Free;
    FPersonList.Free;
  end;

  WriteFooter(fs_index);
  fs_index.Destroy;

  LoadExtFile(aDir + 'pedigree.htm');
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
