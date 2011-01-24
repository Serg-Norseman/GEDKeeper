unit GKNamesBook;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Contnrs;

type
  TfmNamesBook = class(TForm)
    cbNames: TComboBox;
    mmDesc: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbNamesSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FNames: TObjectList;

    procedure Load();
    procedure PrepareList();
  public
  end;

var
  fmNamesBook: TfmNamesBook;

implementation

{$R *.dfm}
{$R res\names.res}

uses
  bsComUtils, GKUtils, GKEngine, GKMain;

type
  TNameRecord = class(TObject)
  public
    Name: string;
    Desc: string;
    Sex: (nsMale, nsFemale);
  end;

procedure TfmNamesBook.FormCreate(Sender: TObject);
begin
  FNames := TObjectList.Create(True);
  Load();
  PrepareList();
end;

procedure TfmNamesBook.FormDestroy(Sender: TObject);
begin
  FNames.Destroy;
end;

procedure TfmNamesBook.Load();

  function ExtractFlags(var st: string): Boolean;
  begin
    Result := (Length(st) >= 2) and (st[1] = '[') and (st[Length(st)] = ']');
    if Result
    then st := Copy(st, 2, Length(st) - 2);
  end;

var
  fs: TResourceStream;
  tf: TTextFileEx;
  ns, st: string;
  rec: TNameRecord;
begin
  fs := TResourceStream.Create(HInstance, 'NAMES_DATA', RT_RCDATA);
  tf := TTextFileEx.Create(fs);
  try
    while not(tf.Eof) do begin
      ns := tf.ReadLn();
      ns := Trim(ns);

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
    tf.Destroy;
    fs.Destroy;
  end;
end;

procedure TfmNamesBook.PrepareList();
var
  i: Integer;
  rec: TNameRecord;
begin
  cbNames.Items.BeginUpdate;
  try
    cbNames.Items.Clear;

    for i := 0 to FNames.Count - 1 do begin
      rec := TNameRecord(FNames[i]);
      cbNames.Items.AddObject(rec.Name, rec);
    end;
  finally
    cbNames.Items.EndUpdate;
  end;
end;

procedure TfmNamesBook.cbNamesSelect(Sender: TObject);
var
  idx: Integer;
  rec: TNameRecord;
begin
  idx := cbNames.ItemIndex;
  if (idx < 0) or (idx >= cbNames.Items.Count) then Exit;
  rec := TNameRecord(cbNames.Items.Objects[idx]);
  mmDesc.Lines.Clear;
  mmDesc.Lines.Add(rec.Name);
  mmDesc.Lines.Add(rec.Desc);
end;

procedure TfmNamesBook.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fmGEDKeeper.actNamesBook.Checked := False;
  fmNamesBook := nil;
  Action := caFree;
end;

end.
