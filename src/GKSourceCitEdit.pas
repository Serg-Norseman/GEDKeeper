unit GKSourceCitEdit; {prepare:fin}

{$I GEDKeeper.inc}

interface

uses
  Classes, Controls, Forms, StdCtrls, Buttons, GedCom551, GKEngine, GKBase,
  ComCtrls;

type
  TfmSourceCitEdit = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    Label1: TLabel;
    EditPage: TEdit;
    Label2: TLabel;
    edOld: TEdit;
    btnSourceAdd: TSpeedButton;
    Label3: TLabel;
    EditCertainty: TComboBox;
    cbSource: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSourceAddClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure cbSourceKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbSourceSelect(Sender: TObject);
  private
    FSourceCitation: TGEDCOMSourceCitation;
    FTempSrc: TGEDCOMSourceRecord;

    FSourcesList: TStringList;

    procedure RefreshSourcesList(aFilter: string);

    procedure SetSourceCitation(const Value: TGEDCOMSourceCitation);
    function GetBase: TfmBase;
  public
    property Base: TfmBase read GetBase;
    property SourceCitation: TGEDCOMSourceCitation read FSourceCitation write SetSourceCitation;
  end;

implementation

uses SysUtils, GKRecordSelect;

{$R *.dfm}

{ TfmSourceCitEdit }

procedure TfmSourceCitEdit.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 3 do EditCertainty.AddItem(CertaintyAssessments[i], nil);

  FSourcesList := TStringList.Create();

  Base.Engine.GetSourcesList(FSourcesList);
  RefreshSourcesList('');
end;

procedure TfmSourceCitEdit.FormDestroy(Sender: TObject);
begin
  FSourcesList.Free;
end;

procedure TfmSourceCitEdit.SetSourceCitation(const Value: TGEDCOMSourceCitation);
begin
  FSourceCitation := Value;

  FTempSrc := TGEDCOMSourceRecord(FSourceCitation.Value);
  if (FTempSrc <> nil) then begin
    //EditSource.Text := FTempSrc.FiledByEntry;
    cbSource.ItemIndex := cbSource.Items.IndexOf(FTempSrc.FiledByEntry);
  end;

  EditPage.Text := FSourceCitation.Page;
  EditCertainty.ItemIndex := FSourceCitation.CertaintyAssessment;
end;

procedure TfmSourceCitEdit.btnSourceAddClick(Sender: TObject);
var
  src: TGEDCOMSourceRecord;
begin
  src := TGEDCOMSourceRecord(Base.SelectRecord(rtSource, []));

  if (src <> nil) then begin
    Base.Engine.GetSourcesList(FSourcesList);
    RefreshSourcesList('');

    //EditSource.Text := FTempSrc.FiledByEntry;
    cbSource.ItemIndex := cbSource.Items.IndexOf(src.FiledByEntry);
  end;
end;

procedure TfmSourceCitEdit.cbSourceKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  s: Integer;
begin
  s := cbSource.SelStart;
  RefreshSourcesList(cbSource.Text);
  cbSource.SelStart := s;
end;

procedure TfmSourceCitEdit.cbSourceSelect(Sender: TObject);
var
  idx: Integer;
begin
  idx := cbSource.ItemIndex;
  if (idx < 0)
  then FTempSrc := nil
  else FTempSrc := TGEDCOMSourceRecord(cbSource.Items.Objects[idx]);
end;

procedure TfmSourceCitEdit.btnAcceptClick(Sender: TObject);
begin
  if (FTempSrc = nil) then begin
    ModalResult := mrNone;
    raise Exception.Create('Не задан источник');
  end;

  FSourceCitation.Value := FTempSrc;
  FSourceCitation.Page := EditPage.Text;
  FSourceCitation.CertaintyAssessment := EditCertainty.ItemIndex;
end;

function TfmSourceCitEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmSourceCitEdit.RefreshSourcesList(aFilter: string);
var
  i: Integer;
  st, flt: string;
begin
  flt := '*' + aFilter + '*';

  cbSource.Items.BeginUpdate();
  try
    FTempSrc := nil;

    cbSource.Items.Clear;
    for i := 0 to FSourcesList.Count - 1 do begin
      st := FSourcesList[i];

      if (aFilter = '') or IsMatchesMask(st, flt)
      then cbSource.Items.AddObject(st, FSourcesList.Objects[i]);
    end;
  finally
    cbSource.Items.EndUpdate();
  end;
end;

end.
