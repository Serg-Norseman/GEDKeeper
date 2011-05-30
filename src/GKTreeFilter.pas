unit GKTreeFilter; {prepare:fin; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Mask, GKBase, GKChartCore, ComCtrls, GKLists, GKEngine, GKLangs;

type
  TfmTreeFilter = class(TForm, ILocalization)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    Label5: TLabel;
    cbSource: TComboBox;
    rgBranchCut: TGroupBox;
    rbCutNone: TRadioButton;
    rbCutYears: TRadioButton;
    rbCutPersons: TRadioButton;
    Label1: TLabel;
    edYear: TEdit;
    UpDown1: TUpDown;
    Panel1: TPanel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rbCutNoneClick(Sender: TObject);
  private
    FFilter: TChartFilter;
    FPersonsList: TSheetList;
    FTemp: string;

    function GetBase: TfmBase;
    procedure ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
    procedure UpdateControls();
  public
    property Base: TfmBase read GetBase;
    property Filter: TChartFilter read FFilter write FFilter;

    procedure SetLang();
  end;

implementation

uses GKMain, GedCom551, GKUtils;

{$R *.dfm}

procedure TfmTreeFilter.FormCreate(Sender: TObject);
begin
  FPersonsList := TSheetList.Create(Panel1);
  FPersonsList.Buttons := [lbAdd, lbDelete];
  FPersonsList.OnModify := ListModify;
  AddListColumn(FPersonsList.List, LSList[LSID_RPIndividuals], 350, False);

  SetLang();
end;

procedure TfmTreeFilter.SetLang();
begin
  btnAccept.Caption := LSList[LSID_DlgAccept];
  btnCancel.Caption := LSList[LSID_DlgCancel];

  Caption := LSList[LSID_MIFilter];

  rgBranchCut.Caption := LSList[LSID_BranchCut];
  rbCutNone.Caption := LSList[LSID_Not];
  rbCutYears.Caption := LSList[LSID_BCut_Years];
  Label1.Caption := LSList[LSID_Year];
  rbCutPersons.Caption := LSList[LSID_BCut_Persons];
  Label5.Caption := LSList[LSID_RPSources];
end;

procedure TfmTreeFilter.UpdateControls();
var
  i: Integer;
  xref: string;
  p: TGEDCOMIndividualRecord;
begin
  case FFilter.BranchCut of
    bcNone: rbCutNone.Checked := True;
    bcYears: rbCutYears.Checked := True;
    bcPersons: rbCutPersons.Checked := True;
  end;

  edYear.Enabled := (FFilter.BranchCut = bcYears);
  UpDown1.Enabled := (FFilter.BranchCut = bcYears);
  FPersonsList.Enabled := (FFilter.BranchCut = bcPersons);
  edYear.Text := IntToStr(FFilter.BranchYear);

  FPersonsList.List.Clear;
  for i := 1 to GetTokensCount(FTemp, ';') do begin
    xref := GetToken(FTemp, ';', i);
    p := TGEDCOMIndividualRecord(Base.Tree.XRefIndex_Find(xref));
    FPersonsList.List.AddItem(GetNameStr(p), p);
  end;
end;

procedure TfmTreeFilter.ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
var
  i_rec: TGEDCOMIndividualRecord;
begin
  if (Sender = FPersonsList) then begin
    case Action of
      raAdd: begin
        i_rec := TGEDCOMIndividualRecord(Base.SelectPerson(nil, tmNone, svNone));

        if (i_rec <> nil)
        then FTemp := FTemp + i_rec.XRef + ';';
      end;
      raEdit: ;
      raDelete: begin
        i_rec := TGEDCOMIndividualRecord(ItemData);

        if (i_rec <> nil)
        then FTemp := StringReplace(FTemp, i_rec.XRef + ';', '', []);
      end;
    end;
  end;

  UpdateControls();
end;

procedure TfmTreeFilter.btnCancelClick(Sender: TObject);
begin
  FFilter.Clear();
  //Base.ApplyFilter();
end;

procedure TfmTreeFilter.btnAcceptClick(Sender: TObject);
var
  rec: TGEDCOMRecord;
begin
  if rbCutNone.Checked
  then FFilter.BranchCut := bcNone
  else
  if rbCutYears.Checked then begin
    FFilter.BranchCut := bcYears;
    FFilter.BranchYear := StrToInt(edYear.Text);
  end
  else
  if rbCutPersons.Checked then begin
    FFilter.BranchCut := bcPersons;
    FFilter.BranchPersons := FTemp;
  end;

  if (cbSource.ItemIndex in [0..2]) then begin
    FFilter.SourceMode := TGroupMode(cbSource.ItemIndex);
    FFilter.SourceRef := '';
  end else begin
    rec := TGEDCOMRecord(cbSource.Items.Objects[cbSource.ItemIndex]);
    if (rec <> nil) then begin
      FFilter.SourceMode := gmSelected;
      FFilter.SourceRef := rec.XRef;
    end else begin
      FFilter.SourceMode := gmAll;
      FFilter.SourceRef := '';
    end;
  end;

  //Base.ApplyFilter();
end;

procedure TfmTreeFilter.FormShow(Sender: TObject);
var
  i: Integer;
  tree: TGEDCOMTree;
begin
  tree := Base.Tree;
  FTemp := FFilter.BranchPersons;

  UpdateControls();

  cbSource.Sorted := True;
  for i := 0 to tree.RecordsCount - 1 do
    if (tree.Records[i] is TGEDCOMSourceRecord)
    then cbSource.AddItem(TGEDCOMSourceRecord(tree.Records[i]).FiledByEntry, tree.Records[i]);
  cbSource.Sorted := False;
  cbSource.Items.InsertObject(0, LSList[LSID_SrcAll], nil);
  cbSource.Items.InsertObject(1, LSList[LSID_SrcNot], nil);
  cbSource.Items.InsertObject(2, LSList[LSID_SrcAny], nil);

  if (FFilter.SourceMode <> gmSelected) then begin
    cbSource.ItemIndex := Ord(FFilter.SourceMode);
  end else begin
    cbSource.ItemIndex := cbSource.Items.IndexOfObject(tree.XRefIndex_Find(FFilter.SourceRef));
  end;
end;

function TfmTreeFilter.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmTreeFilter.rbCutNoneClick(Sender: TObject);
begin
  if rbCutNone.Checked
  then FFilter.BranchCut := bcNone
  else
  if rbCutYears.Checked
  then FFilter.BranchCut := bcYears
  else
  if rbCutPersons.Checked
  then FFilter.BranchCut := bcPersons;

  UpdateControls();
end;

end.
