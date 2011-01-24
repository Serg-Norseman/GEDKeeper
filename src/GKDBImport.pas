unit GKDBImport;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ToolWin,
  ComCtrls, ExtCtrls, Grids, DBGrids, DB, ADODB, GKBase;

type
  TfmDBImport = class(TForm)
    ADOConnection1: TADOConnection;
    OpenDialog1: TOpenDialog;
    ToolBar1: TToolBar;
    btnDBLoad: TToolButton;
    PageControl1: TPageControl;
    SheetDBProperties: TTabSheet;
    SheetDBQueries: TTabSheet;
    mProps: TMemo;       
    mQuery: TMemo;
    ToolButton1: TToolButton;
    btnQueryExec: TToolButton;
    Splitter1: TSplitter;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    SheetDataTransfer: TTabSheet;
    BoxFields: TScrollBox;
    ToolButton2: TToolButton;
    btnDataTransfer: TToolButton;
    procedure btnDBLoadClick(Sender: TObject);
    procedure btnQueryExecClick(Sender: TObject);
    procedure btnDataTransferClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDataSet: TDataSet;

    function GetBase(): TfmBase;

    procedure dbLoad(const aFileName: string);
    procedure queryExecute(const aQuery: string);
    procedure fieldsClear();
    procedure fieldsPrepare();
  public
    property Base: TfmBase read GetBase;
  end;

var
  fmDBImport: TfmDBImport;

implementation

uses GedCom551, bsComUtils, GKEngine, GKProgress, SdfData;

{$R *.dfm}

type
  TGEDCOMField = (
    gfFullName, gfBirthDate, gfBirthPlace,
    gfResiPlace, gfResiAddress, gfPostIndex,
    gfResiPlace2, gfResiAddress2, gfPostIndex2, gfTel
  );

  TTranferRecord = array [TGEDCOMField] of string;

const
  GEDCOMFields: array [TGEDCOMField] of string = (
    'Полное имя', 'Дата рождения', 'Место рождения',
    'Местожительство', 'Адрес проживания', 'Почтовый индекс',
    'Местожительство (2)', 'Адрес проживания (2)', 'Почтовый индекс (2)', 'Телефон'
  );

  DummyRec: TTranferRecord = (
    '', '', '', '', '', '', '', '', '', ''
  );

type
  TFieldPan = class(TGroupBox)
  private
    FFieldName: string;
    FCheckTransfer: TCheckBox;
    FGEDCOMField: TComboBox;

    function GetFieldName(): string;
    procedure SetFieldName(const Value: string);
    function GetTransfer(): Boolean;
    procedure SetTransfer(const Value: Boolean);
    function GetGEDCOMField: TGEDCOMField;
  protected
   procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;

    property FieldName: string read GetFieldName write SetFieldName;
    property Transfer: Boolean read GetTransfer write SetTransfer;
    property GEDCOMField: TGEDCOMField read GetGEDCOMField;
  end;

{ TFieldPan }

constructor TFieldPan.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Height := 49;
  Align := alTop;

  FCheckTransfer := TCheckBox.Create(Self);
  FCheckTransfer.Parent := Self;
  FCheckTransfer.Left := 8;
  FCheckTransfer.Top := 24;
  FCheckTransfer.Width := 129;
  FCheckTransfer.Caption := 'Импортировать как';

  {FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Left := 8;
  FLabel.Top := 48;
  FLabel.Caption := 'Назначение';}

  FGEDCOMField := TComboBox.Create(Self);
  FGEDCOMField.Parent := Self;
  FGEDCOMField.Left := 136;
  FGEDCOMField.Top := 16;
  FGEDCOMField.Width := 265;
  FGEDCOMField.Name := 'GEDCOMField';
  FGEDCOMField.Style := csDropDownList;
end;

function TFieldPan.GetFieldName(): string;
begin
  Result := FFieldName;
end;

procedure TFieldPan.SetFieldName(const Value: string);
begin
  FFieldName := Value;
  Caption := '[ ' + FFieldName + ' ]';
end;

function TFieldPan.GetTransfer(): Boolean;
begin
  Result := FCheckTransfer.Checked;
end;

procedure TFieldPan.SetTransfer(const Value: Boolean);
begin
  FCheckTransfer.Checked := False;
end;

procedure TFieldPan.SetParent(AParent: TWinControl);
var
  f: TGEDCOMField;
begin
  inherited;

  if (AParent <> nil) then begin
    FGEDCOMField.Items.BeginUpdate;
    for f := Low(TGEDCOMField) to High(TGEDCOMField) do
      FGEDCOMField.Items.Add(GEDCOMFields[f]);
    FGEDCOMField.Items.EndUpdate;
    FGEDCOMField.ItemIndex := 0;
  end;
end;

function TFieldPan.GetGEDCOMField(): TGEDCOMField;
begin
  Result := TGEDCOMField(FGEDCOMField.ItemIndex);
end;

{ TfmDBImport }

procedure TfmDBImport.dbLoad(const aFileName: string);
var
  tables, fields: TStringList;
  i, k: Integer;
begin
  FDataSet.Free;

  if (ExtractFileExt(aFileName) = '.csv') then begin
    FDataSet := TSdfDataSet.Create(Self);
    TSdfDataSet(FDataSet).FileName := aFileName;
    TSdfDataSet(FDataSet).FirstLineAsSchema := True;
    TSdfDataSet(FDataSet).Delimiter := ';';
  end else begin
    ADOConnection1.ConnectionString :=
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=' + aFileName + ';User Id=admin;Password=;';
    ADOConnection1.Provider := 'MSDASQL';
    ADOConnection1.Mode := cmRead;
    ADOConnection1.LoginPrompt := False;
    ADOConnection1.Connected := True;

    FDataSet := TADOQuery.Create(Self);
    TADOQuery(FDataSet).Connection := ADOConnection1;
  end;

  fieldsClear();

  mProps.Lines.BeginUpdate;
  tables := TStringList.Create;
  fields := TStringList.Create;
  try
    mProps.Lines.Clear;

    if (FDataSet is TADOQuery) then begin
      ADOConnection1.GetTableNames(tables, False);

      mProps.Lines.Add('База данных: ' + aFileName);
      mProps.Lines.Add('Таблицы:');
      for i := 0 to tables.Count - 1 do begin
        mProps.Lines.Add('  [ ' + tables[i] + ' ]');

        ADOConnection1.GetFieldNames(tables[i], fields);
        for k := 0 to fields.Count - 1 do begin
          mProps.Lines.Add('    - ' + fields[k]);
        end;
      end;

      btnQueryExec.Enabled := ADOConnection1.Connected;
    end else begin
      btnQueryExec.Enabled := False;
      mQuery.ReadOnly := True;
      mQuery.Color := clBtnFace;
      btnDataTransfer.Enabled := True;

      TSdfDataSet(FDataSet).Open;
      DataSource1.DataSet := FDataSet;

      fieldsPrepare();
    end;
  finally
    fields.Destroy;
    tables.Destroy;
    mProps.Lines.EndUpdate;
  end;
end;

procedure TfmDBImport.btnDBLoadClick(Sender: TObject);
begin
  if (OpenDialog1.Execute) then dbLoad(OpenDialog1.FileName);
end;

procedure TfmDBImport.btnQueryExecClick(Sender: TObject);
begin
  queryExecute(mQuery.Lines.Text);
end;

procedure TfmDBImport.queryExecute(const aQuery: string);
begin
  TADOQuery(FDataSet).SQL.Text := aQuery;
  FDataSet.Open;

  fieldsPrepare();
end;

procedure TfmDBImport.fieldsPrepare();
var
  i: Integer;
  fpan: TFieldPan;
begin
  fieldsClear();

  for i := 0 to FDataSet.Fields.Count - 1 do begin
    fpan := TFieldPan.Create(BoxFields);
    fpan.Parent := BoxFields;
    fpan.FieldName := FDataSet.Fields[i].FieldName;
  end;

  btnDataTransfer.Enabled := True;
end;

procedure TfmDBImport.fieldsClear();
var
  i: Integer;
begin
  for i := BoxFields.ComponentCount - 1 downto 0 do
    BoxFields.Components[i].Free;
end;

procedure TfmDBImport.btnDataTransferClick(Sender: TObject);
var
  tree: TGEDCOMTree;

  procedure AddPerson(aTransferRec: TTranferRecord);
  var
    iRec: TGEDCOMIndividualRecord;
    tokCount: Integer;
    nam, pat, fam, tmp, bd: string;
    sx: TGEDCOMSex;
    evt: TGEDCOMCustomEvent;
  begin
    tmp := AnsiLowerCase(aTransferRec[gfFullName]);
    tokCount := GetTokensCount(tmp, ' ');

    fam := GetToken(tmp, ' ', 1);
    fam[1] := AnsiUpperCase(fam)[1];

    if (tokCount > 1) then begin
      nam := GetToken(tmp, ' ', 2);
      nam[1] := AnsiUpperCase(nam)[1];
    end else nam := '';

    if (tokCount > 2) then begin
      pat := GetToken(tmp, ' ', 3);
      pat[1] := AnsiUpperCase(pat)[1];
    end else pat := '';

    sx := svNone;
    iRec := CreatePersonEx(tree, nam, pat, fam, sx, False);
    Base.ChangeRecord(iRec);

    bd := StringReplace(aTransferRec[gfBirthDate], '_', ' ', [rfReplaceAll]);

    if (bd <> '') or (aTransferRec[gfBirthPlace] <> '')
    then CreateEventEx(tree, iRec, 'BIRT', StrToGEDCOMDate(bd, False), aTransferRec[gfBirthPlace]);

    if (aTransferRec[gfResiPlace] <> '') or (aTransferRec[gfResiAddress] <> '') then begin
      evt := CreateEventEx(tree, iRec, 'RESI', '', aTransferRec[gfResiPlace]);
      SetAddressValue(evt.Detail.Address, aTransferRec[gfResiAddress]);

      if (aTransferRec[gfPostIndex] <> '')
      then evt.Detail.Address.AddressPostalCode := aTransferRec[gfPostIndex];

      if (aTransferRec[gfTel] <> '')
      then evt.Detail.Address.PhoneNumbers[evt.Detail.Address.PhoneNumbersCount] := aTransferRec[gfTel];
    end;

    if (aTransferRec[gfResiPlace2] <> '') or (aTransferRec[gfResiAddress2] <> '') then begin
      evt := CreateEventEx(tree, iRec, 'RESI', '', aTransferRec[gfResiPlace2]);
      SetAddressValue(evt.Detail.Address, aTransferRec[gfResiAddress2]);

      if (aTransferRec[gfPostIndex2] <> '')
      then evt.Detail.Address.AddressPostalCode := aTransferRec[gfPostIndex2];
    end;
  end;

var
  i: Integer;
  fpan: TFieldPan;
  trans_rec: TTranferRecord;
begin
  tree := Base.Tree;

  ProgressInit(FDataSet.RecordCount, 'Перенос данных');
  FDataSet.DisableControls;
  try
    FDataSet.First;
    while not (FDataSet.Eof) do begin
      trans_rec := DummyRec;
      for i := 0 to BoxFields.ComponentCount - 1 do begin
        fpan := TFieldPan(BoxFields.Components[i]);

        if (fpan.Transfer)
        then trans_rec[fpan.GEDCOMField] := FDataSet.FieldByName(fpan.FieldName).AsString;
      end;

      AddPerson(trans_rec);

      FDataSet.Next;

      ProgressStep();
    end;
  finally
    FDataSet.EnableControls;
    Base.ListsRefresh();
    ProgressDone();
  end;
end;

function TfmDBImport.GetBase(): TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmDBImport.FormDestroy(Sender: TObject);
begin
  FDataSet.Free;
end;

end.
