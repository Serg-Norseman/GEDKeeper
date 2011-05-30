unit GKNamesBook; {prepare:fin; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Contnrs, GKLangs;

type
  TfmNamesBook = class(TForm, ILocalization)
    cbNames: TComboBox;
    mmDesc: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbNamesSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FNames: TObjectList;
    FChurchFNames, FChurchMNames: TStringList;
    procedure PrepareList();
  public
    procedure SetLang();
  end;

var
  fmNamesBook: TfmNamesBook;

implementation

{$R *.dfm}
{$R res\names.res}

uses
  {$IFDEF DELPHI_NET}Borland.Vcl.WinUtils, {$ENDIF}
  GKUtils, GKEngine, GKMain;

type
  TNameRecord = class(TObject)
  public
    Name: string;
    Desc: string;
    Sex: (nsMale, nsFemale);
    ChIndex: Integer;
  end;

procedure TfmNamesBook.FormCreate(Sender: TObject);
begin
  FNames := TObjectList.Create(True);
  FChurchFNames := TStringList.Create;
  FChurchMNames := TStringList.Create;
  PrepareList();

  SetLang();
end;

procedure TfmNamesBook.FormDestroy(Sender: TObject);
begin
  FChurchFNames.Destroy;
  FChurchMNames.Destroy;
  FNames.Free;
end;

procedure TfmNamesBook.SetLang();
begin
  Caption := LSList[LSID_MINamesBook];
end;

procedure TfmNamesBook.PrepareList();

  function ExtractFlags(var st: string): Boolean;
  begin
    Result := (Length(st) >= 2) and (st[1] = '[') and (st[Length(st)] = ']');
    if Result
    then st := Copy(st, 2, Length(st) - 2);
  end;

var
  fs: TResourceStream;
  tf: TStAnsiTextStream;
  ns, st: string;
  rec: TNameRecord;
  i, k: Integer;
  lst: TStringList;
begin
  // loading
  fs := TResourceStream.Create(HInstance, 'NAMES_DATA', RT_RCDATA);
  tf := TStAnsiTextStream.Create(fs);
  try
    while not(tf.AtEndOfStream) do begin
      ns := Trim(tf.ReadLine());

      if (ns <> '') and (GetTokensCount(ns, '/') >= 3) then begin
        rec := TNameRecord.Create;
        rec.Name := Trim(GetToken(ns, '/', 1));
        rec.Desc := Trim(GetToken(ns, '/', 3));

        st := Trim(GetToken(ns, '/', 2));
        if ExtractFlags(st) then begin
          case st[1] of
            'm': rec.Sex := nsMale;
            'f': rec.Sex := nsFemale;
          end;
        end;

        FNames.Add(rec);
      end;
    end;
  finally
    tf.Free;
    fs.Free;
  end;

  ///
  fs := TResourceStream.Create(HInstance, 'CHURCH_FNAMES_DATA', RT_RCDATA);
  try
    FChurchFNames.LoadFromStream(fs);
  finally
    fs.Free;
  end;
  //
  fs := TResourceStream.Create(HInstance, 'CHURCH_MNAMES_DATA', RT_RCDATA);
  try
    FChurchMNames.LoadFromStream(fs);
  finally
    fs.Free;
  end;
  ///

  // fill list
  cbNames.Items.BeginUpdate;
  try
    cbNames.Items.Clear;

    for i := 0 to FNames.Count - 1 do begin
      rec := TNameRecord(FNames[i]);
      ns := rec.Name;
      cbNames.Items.AddObject(ns, rec);

      rec.ChIndex := -1;
      ns := AnsiUpperCase(ns);
      if (rec.Sex = nsMale) then lst := FChurchMNames
      else
      if (rec.Sex = nsFemale) then lst := FChurchFNames;

      for k := 0 to lst.Count - 1 do begin
        st := lst[k];
        if (st[1] = '-') and (Pos(ns, st) > 0) then begin
          rec.ChIndex := k;
          Break;
        end;
      end;
    end;
  finally
    cbNames.Items.EndUpdate;
  end;
end;

procedure TfmNamesBook.cbNamesSelect(Sender: TObject);
var
  idx, k: Integer;
  rec: TNameRecord;
  lst: TStringList;
  st: string;
begin
  idx := cbNames.ItemIndex;
  if (idx < 0) or (idx >= cbNames.Items.Count) then Exit;
  rec := TNameRecord(cbNames.Items.Objects[idx]);

  mmDesc.Lines.Clear;
  mmDesc.Lines.Add(rec.Name);
  mmDesc.Lines.Add(rec.Desc);

  if (rec.ChIndex >= 0) then begin
    mmDesc.Lines.Add('');
    mmDesc.Lines.Add('Святцы:');

    if (rec.Sex = nsMale) then lst := FChurchMNames
    else
    if (rec.Sex = nsFemale) then lst := FChurchFNames;

    for k := rec.ChIndex + 1 to lst.Count - 1 do begin
      st := Trim(lst[k]);

      if (st[1] = '-')
      then Break
      else begin
        Delete(st, 1, 1);
        mmDesc.Lines.Add(st);
      end;
    end;
  end;
end;

procedure TfmNamesBook.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fmGEDKeeper.miNamesBook.Checked := False;
  fmNamesBook := nil;
  Action := caFree;
end;

end.
