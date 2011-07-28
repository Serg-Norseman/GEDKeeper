unit GKExport; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.IO, System.Text, XLSFile, VCLStub,
  GedCom551, GKEngine, GKCommon, GKUtils, GKProgress, GKLangs;

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
  protected
    procedure WriteHeader(aStream: System.IO.StreamWriter; aTitle: string);
    procedure WriteFooter(aStream: System.IO.StreamWriter);
  public
  end;

{==============================================================================}

type
  TWebExporter = class(THTMLExporter)
  strict private
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

    const
      LatSymSet: set of Char = ['A'..'Z'];
      RusSymSet: set of Char = ['А'..'Я'];
    type
      TLatSyms = 'A'..'Z';
      TRusSyms = 'А'..'Я';

  private
    FSurnamesCount: Integer;

    function AddCell(aRow: TObjectList; iRec: TGEDCOMIndividualRecord;
      aKind: TCellKind): TTreeCell;
    function AddRow(aTable: TObjectList; aRowIndex: Integer; aSpaces: Integer = 0): TObjectList;
    procedure DrawLine(aTable: TObjectList; cell_ancestor, cell_descendant: TTreeCell);
    procedure Step(aTable: TObjectList; row_index, col_index: Integer; prev: TTreeCell;
      cur: TGEDCOMIndividualRecord);
    procedure WideTable(aTable: TObjectList; cols: Integer);

    procedure GenTree(aStream: System.IO.StreamWriter; iRec: TGEDCOMIndividualRecord);
    procedure WritePersons();
    procedure WriteTimeLineIndex(aStream: System.IO.StreamWriter; evName, tlFileName: string);
    procedure WriteNameIndex(aStream: System.IO.StreamWriter);
    procedure WriteFamilyIndex(aStream: System.IO.StreamWriter);
    procedure WriteIndex(aStream: System.IO.StreamWriter; aIndex: TStringList);
    procedure WriteSurnames(aStream: System.IO.StreamWriter; aSym: Char; aNames: TStringList);
  public
    procedure Generate(); override;
  end;

{==============================================================================}

type
  TExcelExporter = class(TExporter)
  private
  const
    AllBorders: TCellAttributeSet = [acBottomBorder, acTopBorder, acRightBorder, acLeftBorder];

  var
    FAppMode: Boolean;
    FSelectedRecords: TList;
  public
    procedure Generate(); override;

    property AppMode: Boolean read FAppMode write FAppMode;
    property SelectedRecords: TList read FSelectedRecords write FSelectedRecords;
  end;

{==============================================================================}

type
  TPedigree = class(THTMLExporter)
  strict private
    const
      p_style = ' style="margin-top:2pt; margin-bottom:2pt"';

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

      TEventObj = class
        Event: TGEDCOMCustomEvent;
        iRec: TGEDCOMIndividualRecord;

        constructor Create(aEvent: TGEDCOMCustomEvent; aRec: TGEDCOMIndividualRecord);
        function GetDate(): DateTime;
      end;

  private
    FAncestor: TGEDCOMIndividualRecord;
    FKind: TPedigreeKind;
    FPersonList: TObjectList;
    FShieldState: TGenEngine.TShieldState;
    FSourceList: TStringList;

    function FindPerson(iRec: TGEDCOMIndividualRecord): TPersonObj;
    procedure WritePerson(aStream: System.IO.StreamWriter; aTree: TGEDCOMTree;
      aPerson: TPersonObj);
    function idLink(aObj: TPersonObj): string;
    function GetIdStr(aPerson: TPersonObj): string;
    procedure WriteExcessFmt(aStream: System.IO.StreamWriter; aPerson: TPersonObj);
    procedure WriteCompactFmt(aStream: System.IO.StreamWriter; aPerson: TPersonObj);
    function GetPedigreeLifeStr(iRec: TGEDCOMIndividualRecord): string;
    procedure WriteEventList(aStream: System.IO.StreamWriter; aPerson: TPersonObj; ev_list: TObjectList);
  public
    procedure Generate(); override;

    property Ancestor: TGEDCOMIndividualRecord read FAncestor write FAncestor;
    property Kind: TPedigreeKind read FKind write FKind;
    property ShieldState: TGenEngine.TShieldState read FShieldState write FShieldState;
  end;

implementation

{==============================================================================}

{ TExporter }

constructor TExporter.Create(aEngine: TGenEngine; aPath: string);
begin
  inherited Create;
  FEngine := aEngine;
  FTree := FEngine.Tree;
  FPath := aPath;

  if not(System.IO.Directory.Exists(FPath))
  then CreateDir(FPath);
end;

destructor TExporter.Destroy;
begin
  inherited Destroy;
end;

{==============================================================================}

procedure THTMLExporter.WriteHeader(aStream: System.IO.StreamWriter; aTitle: string);
begin
  aStream.WriteLine('<html>');
  aStream.WriteLine('<head>');
  aStream.WriteLine('<meta HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=windows-1251">');
  aStream.WriteLine('<link rel="stylesheet" href="style.css" type="text/css"/>');
  aStream.WriteLine('<title>'+aTitle+'</title>');
  aStream.WriteLine('</head>');
  aStream.WriteLine('<body>');
end;

procedure THTMLExporter.WriteFooter(aStream: System.IO.StreamWriter);
begin
  aStream.WriteLine('</body>');
  aStream.WriteLine('</html>');
end;

{==============================================================================}

{ TWebExporter }

procedure TWebExporter.WriteIndex(aStream: System.IO.StreamWriter; aIndex: TStringList);
var
  i_rec: TGEDCOMIndividualRecord;
  i: Integer;
begin
  aStream.WriteLine('<ul>');
  for i := 0 to aIndex.Count - 1 do begin
    i_rec := TGEDCOMIndividualRecord(aIndex.Objects[i]);
    aStream.WriteLine('<li><a href="persons.htm#' + i_rec.XRef + '">' + aIndex[i] + '</a></li>');
  end;
  aStream.WriteLine('</ul>');
end;

procedure TWebExporter.WriteSurnames(aStream: System.IO.StreamWriter; aSym: Char; aNames: TStringList);
var
  index, nList: TStringList;
  f, n, p: string;
  i, idx: Integer;
  i_rec: TGEDCOMIndividualRecord;
begin
  aStream.WriteLine('<li><a name="'+TGKUtils.NumUpdate(Ord(aSym), 3)+'"><b>' + aSym + '</b></a><ul>');

  index := TStringList.Create;
  try
    for i := 0 to aNames.Count - 1 do begin
      i_rec := TGEDCOMIndividualRecord(aNames.Objects[i]);
      TGenEngine.GetNameParts(i_rec, f, n, p);

      f := TGenEngine.PrepareRusFamily(f, (i_rec.Sex = svFemale));
      idx := index.IndexOf(f);

      if (idx < 0)
      then idx := index.AddObject(f, TStringList.Create());

      TStringList(index.Objects[idx]).AddObject(aNames[i], i_rec);
    end;

    FSurnamesCount := FSurnamesCount + index.Count;

    for i := 0 to index.Count - 1 do begin
      nList := TStringList(index.Objects[i]);
      aStream.WriteLine('<li><u>' + index[i] + '</u> (' + nList.Count.ToString() + ')');
      WriteIndex(aStream, nList);
      aStream.WriteLine('</li>');
    end;
  finally
    for i := 0 to index.Count - 1 do index.Objects[i].Free;
    index.Free;
  end;

  aStream.WriteLine('</ul></li>');
end;

procedure TWebExporter.WriteFamilyIndex(aStream: System.IO.StreamWriter);
var
  i: Integer;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  index_str, fam, nam, pat: string;
  c: Char;
  lat: array [TLatSyms] of TStringList;
  rus: array [TRusSyms] of TStringList;
  unk: TStringList;
  fs_surnames: System.IO.StreamWriter;
begin
  for c := Low(TLatSyms) to High(TLatSyms) do lat[c] := nil;
  for c := Low(TRusSyms) to High(TRusSyms) do rus[c] := nil;
  unk := nil;

  fs_surnames := System.IO.StreamWriter.Create(FPath + 'index_surnames.htm', False, Encoding.GetEncoding(1251));
  WriteHeader(fs_surnames, LSList[LSID_GenDB]);
  fs_surnames.WriteLine('<ul>');

  try
    for i := 0 to FTree.RecordsCount - 1 do begin
      rec := FTree.Records[i];

      if (rec is TGEDCOMIndividualRecord) then begin
        ind := rec as TGEDCOMIndividualRecord;

        TGenEngine.GetNameParts(ind, fam, nam, pat);
        if (fam = '') then begin
          if (unk = nil) then begin
            unk := TStringList.Create;
            unk.Sorted := True;
          end;
          unk.AddObject(TGenEngine.GetNameStr(ind), ind);
        end else begin
          c := fam[1];
          if (c in LatSymSet) then begin
            if (lat[c] = nil) then begin
              lat[c] := TStringList.Create;
              lat[c].Sorted := True;
            end;
            lat[c].AddObject(TGenEngine.GetNameStr(ind), ind);
          end
          else
          if (c in RusSymSet) then begin
            if (rus[c] = nil) then begin
              rus[c] := TStringList.Create;
              rus[c].Sorted := True;
            end;
            rus[c].AddObject(TGenEngine.GetNameStr(ind), ind);
          end
          else begin
            if (unk = nil) then begin
              unk := TStringList.Create;
              unk.Sorted := True;
            end;
            unk.AddObject(TGenEngine.GetNameStr(ind), ind);
          end;
        end;
      end;
    end;

    index_str := '';//'<b>Алфавитный:</b> ';

    for c := Low(TLatSyms) to High(TLatSyms) do begin
      if (lat[c] <> nil) then begin
        index_str := index_str + '<a href="index_surnames.htm#' + TGKUtils.NumUpdate(Ord(c), 3) + '">' + c + '</a>&nbsp;';
        WriteSurnames(fs_surnames, c, lat[c]);
      end else begin
        index_str := index_str + c + '&nbsp;';
      end;
    end;

    for c := Low(TRusSyms) to High(TRusSyms) do begin
      if (rus[c] <> nil) then begin
        index_str := index_str + '<a href="index_surnames.htm#' + TGKUtils.NumUpdate(Ord(c), 3) + '">' + c + '</a>&nbsp;';
        WriteSurnames(fs_surnames, c, rus[c]);
      end else begin
        index_str := index_str + c + '&nbsp;';
      end;
    end;

    if (unk <> nil) then begin
      index_str := index_str + '<a href="index_surnames.htm#' + TGKUtils.NumUpdate(Ord('?'), 3) + '">?</a>' + '&nbsp;';
      WriteSurnames(fs_surnames, '?', unk);
    end;

    aStream.WriteLine('<li>' + index_str + '</li>');
  finally
    fs_surnames.WriteLine('</ul>');
    WriteFooter(fs_surnames);
    fs_surnames.Free;

    for c := Low(TLatSyms) to High(TLatSyms) do lat[c].Free;
    for c := Low(TRusSyms) to High(TRusSyms) do rus[c].Free;
    unk.Free;
  end;
end;

procedure TWebExporter.WriteNameIndex(aStream: System.IO.StreamWriter);
var
  i: Integer;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  index_str, fam, nam, pat: string;
  c: Char;
  lat: array [TLatSyms] of TStringList;
  rus: array [TRusSyms] of TStringList;
  unk: TStringList;
  fs_names: System.IO.StreamWriter;
begin
  for c := Low(TLatSyms) to High(TLatSyms) do lat[c] := nil;
  for c := Low(TRusSyms) to High(TRusSyms) do rus[c] := nil;
  unk := nil;

  fs_names := System.IO.StreamWriter.Create(FPath + 'index_names.htm', False, Encoding.GetEncoding(1251));
  WriteHeader(fs_names, LSList[LSID_GenDB]);
  fs_names.WriteLine('<ul>');

  try
    for i := 0 to FTree.RecordsCount - 1 do begin
      rec := FTree.Records[i];

      if (rec is TGEDCOMIndividualRecord) then begin
        ind := rec as TGEDCOMIndividualRecord;

        TGenEngine.GetNameParts(ind, fam, nam, pat);

        if (nam = '') then begin
          if (unk = nil) then begin
            unk := TStringList.Create;
            unk.Sorted := True;
          end;
          unk.AddObject(TGenEngine.GetNameStr(ind, False), ind);
        end else begin
          c := nam[1];
          if (c in LatSymSet) then begin
            if (lat[c] = nil) then begin
              lat[c] := TStringList.Create;
              lat[c].Sorted := True;
            end;
            lat[c].AddObject(TGenEngine.GetNameStr(ind, False), ind);
          end
          else
          if (c in RusSymSet) then begin
            if (rus[c] = nil) then begin
              rus[c] := TStringList.Create;
              rus[c].Sorted := True;
            end;
            rus[c].AddObject(TGenEngine.GetNameStr(ind, False), ind);
          end
          else begin
            if (unk = nil) then begin
              unk := TStringList.Create;
              unk.Sorted := True;
            end;
            unk.AddObject(TGenEngine.GetNameStr(ind, False), ind);
          end;
        end;
      end;
    end;

    index_str := '';//'<b>Алфавитный:</b> ';

    for c := Low(TLatSyms) to High(TLatSyms) do begin
      if (lat[c] <> nil) then begin
        index_str := index_str + '<a href="index_names.htm#' + TGKUtils.NumUpdate(Ord(c), 3) + '">' + c + '</a>&nbsp;';

        fs_names.WriteLine('<li><a name="'+TGKUtils.NumUpdate(Ord(c), 3)+'"><b>' + c + '</b></a>');
        WriteIndex(fs_names, lat[c]);
        fs_names.WriteLine('</li>');
      end else begin
        index_str := index_str + c + '&nbsp;';
      end;
    end;
    for c := Low(TRusSyms) to High(TRusSyms) do begin
      if (rus[c] <> nil) then begin
        index_str := index_str + '<a href="index_names.htm#' + TGKUtils.NumUpdate(Ord(c), 3) + '">' + c + '</a>&nbsp;';

        fs_names.WriteLine('<li><a name="'+TGKUtils.NumUpdate(Ord(c), 3)+'"><b>' + c + '</b></a>');
        WriteIndex(fs_names, rus[c]);
        fs_names.WriteLine('</li>');
      end else begin
        index_str := index_str + c + '&nbsp;';
      end;
    end;

    if (unk <> nil) then begin
      index_str := index_str + '<a href="index_names.htm#' + TGKUtils.NumUpdate(Ord('?'), 3) + '">?</a>&nbsp;';

      fs_names.WriteLine('<li><a name="'+TGKUtils.NumUpdate(Ord('?'), 3)+'"><b>' + '?' + '</b></a>');
      WriteIndex(fs_names, unk);
      fs_names.WriteLine('</li>');
    end;

    aStream.WriteLine('<li>' + index_str + '</li>');
  finally
    fs_names.WriteLine('</ul>');
    WriteFooter(fs_names);
    fs_names.Free;

    for c := Low(TLatSyms) to High(TLatSyms) do lat[c].Free;
    for c := Low(TRusSyms) to High(TRusSyms) do rus[c].Free;
    unk.Free;
  end;
end;

procedure TWebExporter.WriteTimeLineIndex(aStream: System.IO.StreamWriter; evName, tlFileName: string);
var
  i, year, k: Integer;
  m, d: Word;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  ev: TGEDCOMCustomEvent;
  years, fams: TStringList;
  yst, index_str: string;
  fs_timeline: System.IO.StreamWriter;
begin
  years := TStringList.Create;
  try
    for i := 0 to FTree.RecordsCount - 1 do begin
      rec := FTree.Records[i];

      if (rec is TGEDCOMIndividualRecord) then begin
        ind := rec as TGEDCOMIndividualRecord;

        ev := TGenEngine.GetIndividualEvent(ind, evName);
        if (ev = nil)
        then year := -1
        else begin
          TGenEngine.GetIndependentDate(ev.Detail.Date.Value, year, m, d);
          if (year = 0) then year := -1;
        end;

        if (year < 0)
        then yst := '?'
        else yst := year.ToString();

        k := years.IndexOf(yst);
        if (k < 0)
        then k := years.AddObject(yst, TStringList.Create);

        TStringList(years.Objects[k]).AddObject(TGenEngine.GetNameStr(ind), ind);
      end;
    end;

    fs_timeline := System.IO.StreamWriter.Create(FPath + tlFileName, False, Encoding.GetEncoding(1251));
    try
      WriteHeader(fs_timeline, LSList[LSID_GenDB]);
      fs_timeline.WriteLine('<b>Индекс:</b><ul>');

      years.Sort;
      index_str := '';
      for i := 0 to years.Count - 1 do begin
        index_str := index_str + '<a href="' + tlFileName + '#' + years[i] + '">' + years[i] + '</a>';
        if (i < years.Count - 1) then index_str := index_str + ' | ';

        ////

        fs_timeline.WriteLine('<li><a name="' + years[i] + '">' + years[i] + '</a><ul>');
        fams := TStringList(years.Objects[i]);
        fams.Sort();
        for k := 0 to fams.Count - 1 do begin
          ind := TGEDCOMIndividualRecord(fams.Objects[k]);
          fs_timeline.WriteLine('<li><a href="persons.htm#'+ind.XRef+'">' + fams[k] + '</a></li>');
        end;
        fs_timeline.WriteLine('</ul></li>');
      end;
      aStream.WriteLine('<center>' + index_str + '</center>');

      fs_timeline.WriteLine('</ul><hr>');
      WriteFooter(fs_timeline);
    finally
      fs_timeline.Free;
    end;
  finally
    for i := 0 to years.Count - 1 do years.Objects[i].Free;
    years.Free;
  end;
end;

function TWebExporter.AddCell(aRow: TObjectList; iRec: TGEDCOMIndividualRecord; aKind: TCellKind): TTreeCell;
begin
  Result := TTreeCell.Create;
  Result.ColIndex := aRow.Add(Result);
  Result.Kind := aKind;
  Result.Rec := iRec;
  Result.Row := aRow;

  if (iRec <> nil)
  then Result.Name := TGenEngine.GetNameStr(iRec);
end;

function TWebExporter.AddRow(aTable: TObjectList; aRowIndex: Integer; aSpaces: Integer = 0): TObjectList;
begin
  Result := TObjectList.Create;
  aTable.Insert(aRowIndex, Result);
end;

procedure TWebExporter.WideTable(aTable: TObjectList; cols: Integer);
var
  i: Integer;
  row: TObjectList;
begin
  for i := 0 to aTable.Count - 1 do begin
    row := TObjectList(aTable[i]);
    while (row.Count < cols) do
      AddCell(row, nil, ckSpace);
  end;
end;

procedure TWebExporter.DrawLine(aTable: TObjectList; cell_ancestor, cell_descendant: TTreeCell);
var
  x, y, r1, r2: Integer;
  row: TObjectList;
begin
  r1 := aTable.IndexOf(cell_descendant.Row);
  r2 := aTable.IndexOf(cell_ancestor.Row);

  if (r1 > r2) then begin
    y := r2;
    r2 := r1;
    r1 := y;
  end;

  x := (cell_descendant.ColIndex - 1);

  for y := r1 to r2 do begin
    row := TObjectList(aTable[y]);
    TTreeCell(row[x]).Kind := ckLine;
  end;
end;

procedure TWebExporter.Step(aTable: TObjectList; row_index, col_index: Integer; prev: TTreeCell; cur: TGEDCOMIndividualRecord);
var
  row: TObjectList;
  i: Integer;
  family: TGEDCOMFamilyRecord;
  iFather, iMother: TGEDCOMIndividualRecord;
  cur_cell: TTreeCell;
begin
  if (cur = nil) then Exit;

  if (row_index < 0) then row_index := 0;
  if (row_index > aTable.Count) then row_index := aTable.Count;

  row := AddRow(aTable, row_index);

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

      row_index := aTable.IndexOf(row);
      if (iFather <> nil) then AddRow(aTable, row_index, col_index + 1);
      Step(aTable, row_index, col_index + 1, cur_cell, iFather);

      row_index := aTable.IndexOf(row);
      if (iMother <> nil) then AddRow(aTable, row_index + 1, col_index + 1);
      Step(aTable, row_index + 2, col_index + 1, cur_cell, iMother);
    end;
  end;

  WideTable(aTable, cur_cell.ColIndex + 1);

  if (prev <> nil)
  then DrawLine(aTable, prev, cur_cell);
end;

procedure TWebExporter.GenTree(aStream: System.IO.StreamWriter; iRec: TGEDCOMIndividualRecord);
var
  table_rows: TObjectList;
  r, c: Integer;
  row: TObjectList;
  cell: TTreeCell;
  nm, st: string;
begin
  try
    table_rows := TObjectList.Create(True);
    try
      Step(table_rows, 0, 0, nil, iRec);

      // write table to stream
      aStream.WriteLine('<table border="0" cellspacing="0">');
      for r := 0 to table_rows.Count - 1 do begin
        row := TObjectList(table_rows[r]);

        aStream.WriteLine('<tr>');
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

          aStream.WriteLine('<td' + st + '>' + nm + '</td>');
        end;
        aStream.WriteLine('</tr>');
      end;
      aStream.WriteLine('</tr></table>');
    finally
      table_rows.Free;
    end;
  except
    on E: Exception do aStream.WriteLine(E.Message);
  end;
end;

procedure TWebExporter.WritePersons();
var
  i: Integer;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  names: TStringList;
  fs_persons: System.IO.StreamWriter;
begin
  fs_persons := System.IO.StreamWriter.Create(FPath + 'persons.htm', False, Encoding.GetEncoding(1251));
  WriteHeader(fs_persons, LSList[LSID_GenDB]);
  fs_persons.WriteLine('<b>'+LSList[LSID_GenIndex]+':</b><ul>');

  names := TStringList.Create;
  try
    for i := 0 to FTree.RecordsCount - 1 do begin
      rec := FTree.Records[i];

      if (rec is TGEDCOMIndividualRecord) then begin
        ind := rec as TGEDCOMIndividualRecord;
        names.AddObject(TGenEngine.GetNameStr(ind), ind);
      end;
    end;

    names.Sort;
    for i := 0 to names.Count - 1 do begin
      ind := TGEDCOMIndividualRecord(names.Objects[i]);
      fs_persons.WriteLine('<li><a name="' + ind.XRef + '">' + names[i] + '</a><p>');
      GenTree(fs_persons, ind);
      fs_persons.WriteLine('</p></li>');
    end;
  finally
    names.Free;

    fs_persons.WriteLine('</ul><hr>');
    WriteFooter(fs_persons);
    fs_persons.Free;
  end;
end;

procedure TWebExporter.Generate();
var
  main_index: System.IO.StreamWriter;
  stats: TGenEngine.TCommonStats;
begin
  main_index := System.IO.StreamWriter.Create(FPath + 'index.htm', False, Encoding.GetEncoding(1251));
  try
    WriteHeader(main_index, LSList[LSID_GenDB]);

    FSurnamesCount := 0;

    //WriteStr(main_index.WriteLine('<form><table><tr><td>');

    main_index.WriteLine('<b>'+LSList[LSID_SurnamesIndex]+':</b><ul>');
    WriteFamilyIndex(main_index);
    main_index.WriteLine('</ul><hr>');

    main_index.WriteLine('<b>'+LSList[LSID_NamesIndex]+':</b><ul>');
    WriteNameIndex(main_index);
    main_index.WriteLine('</ul><hr>');

    main_index.WriteLine('<b>'+LSList[LSID_BirthIndex]+':</b><ul>');
    WriteTimeLineIndex(main_index, 'BIRT', 'index_birth.htm');
    main_index.WriteLine('</ul><hr>');

    main_index.WriteLine('<b>'+LSList[LSID_DeathIndex]+':</b><ul>');
    WriteTimeLineIndex(main_index, 'DEAT', 'index_death.htm');
    main_index.WriteLine('</ul><hr>');

    WritePersons();

    {fixme!!!}
    FEngine.GetCommonStats(stats);
    main_index.WriteLine('<b>'+LSList[LSID_CommonStats]+':</b><ul>');
    main_index.WriteLine('<li>Персон: ' + stats.persons.ToString() + '</li>');
    main_index.WriteLine('<li>Фамилий: ' + FSurnamesCount.ToString() + '</li>');
    main_index.WriteLine('</ul><hr>');

    //WriteStr(main_index.WriteLine('</td></tr></table></form>');

    WriteFooter(main_index);
  finally
    main_index.Free;
  end;

  TGKUtils.LoadExtFile(FPath + 'index.htm');
end;

{==============================================================================}

{ TExcelExporter }

procedure TExcelExporter.Generate();
var
  xls: TXLSFile;
  i, row: Integer;
  rec: TGEDCOMRecord;
  ind: TGEDCOMIndividualRecord;
  fam, nam, pat: string;
  fname: string;
begin
  xls := TXLSFile.Create();
  TfmProgress.ProgressInit(FTree.RecordsCount, LSList[LSID_MIExport]+'...');
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

        TGenEngine.GetNameParts(ind, fam, nam, pat);

        Inc(row);
        xls.AddStrCell( 1, row, AllBorders, TGenEngine.GetId(ind).ToString());
        xls.AddStrCell( 2, row, AllBorders, fam);
        xls.AddStrCell( 3, row, AllBorders, nam);
        xls.AddStrCell( 4, row, AllBorders, pat);
        xls.AddStrCell( 5, row, AllBorders, TGenEngine.SexStr(ind.Sex)[1]);
        xls.AddStrCell( 6, row, AllBorders, TGenEngine.GetBirthDate(ind, dfDD_MM_YYYY));
        xls.AddStrCell( 7, row, AllBorders, TGenEngine.GetDeathDate(ind, dfDD_MM_YYYY));
        xls.AddStrCell( 8, row, AllBorders, TGenEngine.GetBirthPlace(ind));
        xls.AddStrCell( 9, row, AllBorders, TGenEngine.GetDeathPlace(ind));
        xls.AddStrCell(10, row, AllBorders, TGenEngine.GetResidencePlace(ind, FOptions.PlacesWithAddress));
        xls.AddStrCell(11, row, AllBorders, TGenEngine.GetAge(ind));
        xls.AddStrCell(12, row, AllBorders, TGenEngine.GetLifeExpectancy(ind));
      end;

      TfmProgress.ProgressStep();
    end;

    fname := FPath + 'export.xls';
    xls.SaveToFile(fname);

    TGKUtils.LoadExtFile(fname);
  finally
    TfmProgress.ProgressDone();
    xls.Destroy;
  end;
end;

{==============================================================================}

{ TPedigree }

function TPedigree.TPersonObj.GetOrderStr(): string;
var
  order: Char;
begin
  order := Chr(FamilyOrder);

  if (Parent = nil)
  then Result := order
  else Result := Parent.GetOrderStr() + order;
end;

constructor TPedigree.TEventObj.Create(aEvent: TGEDCOMCustomEvent; aRec: TGEDCOMIndividualRecord);
begin
  inherited Create;
  Event := aEvent;
  iRec := aRec;
end;

function TPedigree.TEventObj.GetDate(): DateTime;
begin
  if (Event <> nil)
  then Result := TGenEngine.GEDCOMDateToDate(Event.Detail.Date.Value)
  else Result := DateTime.Create(0);
end;


function TPedigree.idLink(aObj: TPersonObj): string;
begin
  Result := '';
  if (aObj = nil) then Exit;

  Result := ' [<a href="#' + aObj.Id + '">' + aObj.Id + '</a>]';
end;

procedure TPedigree.WriteEventList(aStream: System.IO.StreamWriter; aPerson: TPersonObj; ev_list: TObjectList);
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
      ev := TGenEngine.GetPersonEventIndex(event.Name);
      if (ev = 0) then st := event.Detail.Classification
      else
      if (ev > 0) then st := LSList[TGenEngine.PersonEvents[ev].Name]
      else st := event.Name;

      dt := TGenEngine.GEDCOMCustomDateToStr(event.Detail.Date.Value, dfDD_MM_YYYY);

      aStream.WriteLine('<li>' + dt + ': ' + st + '.');

      if (event.Detail.Place.StringValue <> '')
      then aStream.WriteLine(' '+LSList[LSID_Place]+': ' + event.Detail.Place.StringValue + '</li>');
    end else begin
      if (event = nil)
      then dt := '?'
      else dt := TGenEngine.GEDCOMCustomDateToStr(event.Detail.Date.Value, dfDD_MM_YYYY);

      {fixme!}
      if (evObj.iRec.Sex = svMale) then begin
        st := ': Родился ';
      end else begin
        st := ': Родилась ';
      end;

      aStream.WriteLine('<li>' + dt + st + TGenEngine.GetNameStr(evObj.iRec) + idLink(FindPerson(evObj.iRec)) + '</li>');
    end;
  end;
end;

function TPedigree.GetPedigreeLifeStr(iRec: TGEDCOMIndividualRecord): string;
var
  ds, ps, res_str: string;
  ev: TGEDCOMCustomEvent;
begin
  res_str := '';

  case FOptions.PedigreeOptions.Format of
    pfExcess: begin
      ds := TGenEngine.GetBirthDate(iRec, dfDD_MM_YYYY, True);
      if (ds = '') then ds := '?';
      res_str := res_str + ds;

      ds := TGenEngine.GetDeathDate(iRec, dfDD_MM_YYYY, True);
      if (ds = '') then begin
        ev := TGenEngine.GetIndividualEvent(iRec, 'DEAT');
        if (ev <> nil) then ds := '?';
      end;

      if (ds <> '')
      then res_str := res_str + ' - ' + ds;
    end;

    pfCompact: begin
      ds := TGenEngine.GetBirthDate(iRec, dfDD_MM_YYYY, True);
      ps := TGenEngine.GetBirthPlace(iRec);
      if (ps <> '') then begin
        if (ds <> '') then ds := ds + ', ';
        ds := ds + ps;
      end;
      if (ds <> '') then ds := '*' + ds;
      res_str := res_str + ds;

      ds := TGenEngine.GetDeathDate(iRec, dfDD_MM_YYYY, True);
      ps := TGenEngine.GetDeathPlace(iRec);
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

function TPedigree.GetIdStr(aPerson: TPersonObj): string;
var
  family: TGEDCOMFamilyRecord;
  idx: Integer;
  sp_str: string;
begin
  Result := '<a name="' + aPerson.Id + '">' + aPerson.Id + '</a>';

  if (FKind = pk_Konovalov) and (aPerson.Parent <> nil) then begin
    family := aPerson.iRec.ChildToFamilyLinks[0].Family;

    sp_str := '';
    idx := aPerson.Parent.iRec.IndexOfSpouse(family);
    if (aPerson.Parent.iRec.SpouseToFamilyLinksCount > 1)
    then sp_str := '/' + Int32(idx + 1).ToString();

    Result := Result + sp_str;
  end;
end;

procedure TPedigree.WriteExcessFmt(aStream: System.IO.StreamWriter; aPerson: TPersonObj);
var
  ev_list: TObjectList;
  i, k: Integer;
  event: TGEDCOMCustomEvent;
  family: TGEDCOMFamilyRecord;
  irec: TGEDCOMIndividualRecord;
  st, unk: string;
  sp: TGEDCOMPointer;
  note: TGEDCOMNotes;
begin
  aStream.WriteLine('<br>'+LSList[LSID_Sex]+': ' + TGenEngine.SexStr(aPerson.iRec.Sex));

  st := TGenEngine.GetLifeExpectancy(aPerson.iRec);
  if (st <> '?') and (st <> '')
  then aStream.WriteLine('<br>'+LSList[LSID_LifeExpectancy]+': ' + st);

  if (aPerson.iRec.ChildToFamilyLinksCount <> 0) then begin
    family := aPerson.iRec.ChildToFamilyLinks[0].Family;

    irec := TGEDCOMIndividualRecord(family.Husband.Value);
    if (irec <> nil)
    then aStream.WriteLine('<br>'+LSList[LSID_Father]+': ' + TGenEngine.GetNameStr(irec) + idLink(FindPerson(irec)));

    irec := TGEDCOMIndividualRecord(family.Wife.Value);
    if (irec <> nil)
    then aStream.WriteLine('<br>'+LSList[LSID_Mother]+': ' + TGenEngine.GetNameStr(irec) + idLink(FindPerson(irec)));
  end;

  ev_list := TObjectList.Create(True);
  try
    // загрузка событий
    if (aPerson.iRec.IndividualEventsCount > 0) then begin
      aStream.WriteLine('<p>'+LSList[LSID_Events]+': <ul>');
      for i := 0 to aPerson.iRec.IndividualEventsCount - 1 do begin
        event := aPerson.iRec.IndividualEvents[i];

        if not(event is TGEDCOMIndividualAttribute)
        or ((event is TGEDCOMIndividualAttribute) and (FOptions.PedigreeOptions.IncludeAttributes))
        then ev_list.Add(TEventObj.Create(event, aPerson.iRec));
      end;
      WriteEventList(aStream, aPerson, ev_list);
      aStream.WriteLine('</ul></p>');
    end;

    for i := 0 to aPerson.iRec.SpouseToFamilyLinksCount - 1 do begin
      family := aPerson.iRec.SpouseToFamilyLinks[i].Family;
      if not(TGenEngine.IsRecordAccess(family.Restriction, FShieldState)) then Continue;

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
      then aStream.WriteLine('<p>' + st + TGenEngine.GetNameStr(irec) + GetPedigreeLifeStr(irec) + idLink(FindPerson(irec)))
      else aStream.WriteLine('<p>' + st + unk);

      aStream.WriteLine('<ul>');
      ev_list.Clear;
      for k := 0 to family.ChildrenCount - 1 do begin
        irec := TGEDCOMIndividualRecord(family.Children[k].Value);
        ev_list.Add(TEventObj.Create(TGenEngine.GetIndividualEvent(irec, 'BIRT'), irec));
      end;
      WriteEventList(aStream, aPerson, ev_list);
      aStream.WriteLine('</ul></p>');
    end;
  finally
    ev_list.Free;
  end;

  if (FOptions.PedigreeOptions.IncludeNotes) and (aPerson.iRec.NotesCount <> 0) then begin
    aStream.WriteLine('<p>'+LSList[LSID_RPNotes]+':<ul>');
    for i := 0 to aPerson.iRec.NotesCount - 1 do begin
      note := aPerson.iRec.Notes[i];
      aStream.WriteLine('<li>' + TGKUtils.ConStrings(note.Notes) + '</li>');
    end;
    aStream.WriteLine('</ul></p>');
  end;
end;

procedure TPedigree.WriteCompactFmt(aStream: System.IO.StreamWriter; aPerson: TPersonObj);
var
  i: Integer;
  family: TGEDCOMFamilyRecord;
  irec: TGEDCOMIndividualRecord;
  st, unk: string;
  sp: TGEDCOMPointer;
  note: TGEDCOMNotes;
  sp_index: Boolean;
begin
  if (FOptions.PedigreeOptions.IncludeNotes) and (aPerson.iRec.NotesCount <> 0) then begin
    for i := 0 to aPerson.iRec.NotesCount - 1 do begin
      note := aPerson.iRec.Notes[i];
      aStream.WriteLine('<p'+p_style+'>' + TGKUtils.ConStrings(note.Notes) + '</p>');
    end;
  end;

  try
    sp_index := (aPerson.iRec.SpouseToFamilyLinksCount > 1);

    for i := 0 to aPerson.iRec.SpouseToFamilyLinksCount - 1 do begin
      family := aPerson.iRec.SpouseToFamilyLinks[i].Family;
      if not(TGenEngine.IsRecordAccess(family.Restriction, FShieldState)) then Continue;

      if (aPerson.iRec.Sex = svMale) then begin
        sp := family.Wife;
        st := 'Ж';
        unk := LSList[LSID_UnkFemale];
      end else begin
        sp := family.Husband;
        st := 'М';
        unk := LSList[LSID_UnkMale];
      end;

      if (sp_index) then st := st + Int32(i + 1).ToString();
      st := st + ' - ';

      irec := TGEDCOMIndividualRecord(sp.Value);
      if (irec <> nil)
      then st := st + TGenEngine.GetNameStr(irec) + GetPedigreeLifeStr(irec) + idLink(FindPerson(irec))
      else st := st + unk;

      aStream.WriteLine('<p'+p_style+'>' + st + '</p>');
    end;
  finally
  end;
end;


procedure TPedigree.WritePerson(aStream: System.IO.StreamWriter; aTree: TGEDCOMTree; aPerson: TPersonObj);
begin
  aStream.WriteLine('<li>');
  aStream.WriteLine('<b>' + GetIdStr(aPerson) + '. ' + TGenEngine.GetNameStr(aPerson.iRec) + '</b>' + GetPedigreeLifeStr(aPerson.iRec));

  if (FOptions.PedigreeOptions.IncludeSources)
  then aStream.WriteLine('&nbsp;<sup>' + aPerson.Sources + '</sup>');

  case FOptions.PedigreeOptions.Format of
    pfExcess: WriteExcessFmt(aStream, aPerson);
    pfCompact: WriteCompactFmt(aStream, aPerson);
  end;

  aStream.WriteLine('</li><br>');
end;

procedure TPedigree.Generate();
var
  fs_index: System.IO.StreamWriter;

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
    res.BirthDate := TGenEngine.GetBirthDate(iRec, dfYYYY_MM_DD, True);
    res.FamilyOrder := aFamilyOrder;
    FPersonList.Add(res);

    i_sources := '';
    if (FOptions.PedigreeOptions.IncludeSources) then begin
      for i := 0 to iRec.SourceCitationsCount - 1 do begin
        cit := iRec.SourceCitations[i];
        sourceRec := TGEDCOMSourceRecord(cit.Value);
        if (sourceRec <> nil) then begin
          src_name := TGKUtils.ConStrings(sourceRec.Title);

          if (src_name = '')
          then src_name := sourceRec.FiledByEntry;

          k := FSourceList.IndexOf(src_name);
          if (k < 0)
          then k := FSourceList.Add(src_name);

          if (i_sources <> '') then i_sources := i_sources + ',';

          sn := Int32(k + 1).ToString();
          i_sources := i_sources + '<a href="#src' + sn + '">' + sn + '</a>';
        end;
      end;
    end;
    res.Sources := i_sources;

    //

    for k := 0 to iRec.SpouseToFamilyLinksCount - 1 do begin
      family := iRec.SpouseToFamilyLinks[k].Family;
      if not(TGenEngine.IsRecordAccess(family.Restriction, FShieldState)) then Continue;

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
            obj.Id := obj.Parent.Id + '.' + obj.Parent.ChildIdx.ToString();
          end;
        end;

        pk_Konovalov: begin
          obj.Id := Int32(i + 1).ToString();

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
  if (FAncestor = nil) then begin
    TGKUtils.ShowError(LSList[LSID_NotSelectedPerson]);
    Exit;
  end;

  title := LSList[LSID_ExpPedigree]+': ' + TGenEngine.GetNameStr(FAncestor);

  CreateDir(FPath);

  fs_index := System.IO.StreamWriter.Create(FPath + 'pedigree.htm', False, Encoding.GetEncoding(1251));
  WriteHeader(fs_index, title);
  fs_index.WriteLine('<h2>' + title + '</h2>');

  FPersonList := TObjectList.Create(True);
  FSourceList := TStringList.Create;
  try
    Step(nil, FAncestor, 1, 1);
    ReIndex();

    // вывод по поколениям
    cur_level := 0;
    for k := 0 to FPersonList.Count - 1 do begin
      pObj := TPersonObj(FPersonList[k]);

      // определение момента вывода заголовка поколения
      if (cur_level <> pObj.Level) then begin
        if (cur_level > 0) then fs_index.WriteLine('</ul>');

        cur_level := pObj.Level;

        fs_index.WriteLine('<h3>'+LSList[LSID_Generation]+' ' + TGKUtils.GetRome(cur_level) + '</h3><ul>');
      end;

      WritePerson(fs_index, FTree, pObj);
    end;
    fs_index.WriteLine('</ul>');

    if (FSourceList.Count > 0) then begin
      fs_index.WriteLine('<h3>'+LSList[LSID_RPSources]+'</h3>');
      for i := 0 to FSourceList.Count - 1 do begin
        sn := Int32(i + 1).ToString();
        fs_index.WriteLine('<p><sup><a name="src' + sn + '">' + sn + '</a></sup>&nbsp;');
        fs_index.WriteLine(FSourceList[i] + '</p>');
      end;
    end;
  finally
    FSourceList.Free;
    FPersonList.Free;
  end;

  WriteFooter(fs_index);
  fs_index.Free;

  TGKUtils.LoadExtFile(FPath + 'pedigree.htm');
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
