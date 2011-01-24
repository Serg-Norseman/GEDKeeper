unit GKFilter;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Mask, GKBase;

type
  TfmFilter = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    rgLife: TRadioGroup;
    Label1: TLabel;
    edName: TComboBox;
    rgSex: TRadioGroup;
    Label2: TLabel;
    edAliveBeforeDate: TMaskEdit;
    GroupBox1: TGroupBox;
    CheckPatriarch: TCheckBox;
    Label3: TLabel;
    cbResidence: TComboBox;
    Label4: TLabel;
    cbGroup: TComboBox;
    Label5: TLabel;
    cbSource: TComboBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rgLifeClick(Sender: TObject);
  private
    function GetBase: TfmBase;
  public
    property Base: TfmBase read GetBase;
  end;

implementation

uses GKMain, GedCom551, GKEngine, GKLists;

{$R *.dfm}

procedure TfmFilter.btnCancelClick(Sender: TObject);
begin
  Base.Filter.Clear();
  Base.ApplyFilter();
end;

procedure TfmFilter.btnAcceptClick(Sender: TObject);
var
  dt: TDateTime;
  fs: string;
  rec: TGEDCOMRecord;
begin
  fs := Trim(edName.Text);
  if (fs <> '') and (fs <> '*') then begin
    if (fmGEDKeeper.Options.NameFilters.IndexOf(fs) < 0)
    then fmGEDKeeper.Options.NameFilters.Add(fs);
  end;

  fs := Trim(cbResidence.Text);
  if (fs <> '') and (fs <> '*') then begin
    if (fmGEDKeeper.Options.ResidenceFilters.IndexOf(fs) < 0)
    then fmGEDKeeper.Options.ResidenceFilters.Add(fs);
  end;
  //

  Base.Filter.PatriarchOnly := CheckPatriarch.Checked;

  if (Base.Filter.LifeMode <> lmTimeLine) then begin
    Base.Filter.AliveBeforeDate := edAliveBeforeDate.Text;
    if (rgLife.ItemIndex = 3) then begin
      try
        dt := StrToDate(edAliveBeforeDate.Text);
        //Hole(dt);
      except
        MessageDlg('Дата неверна', mtError, [mbOk], 0);
        ModalResult := mrNone;
      end;
    end;
    Base.Filter.LifeMode := TLifeMode(rgLife.ItemIndex);
  end;

  Base.Filter.Sex := TGEDCOMSex(rgSex.ItemIndex);

  if (edName.Text = '') then edName.Text := '*';
  Base.Filter.Name := edName.Text;

  if (cbResidence.Text = '') then cbResidence.Text := '*';
  Base.Filter.Residence := cbResidence.Text;

  if (cbGroup.ItemIndex in [0..2]) then begin
    Base.Filter.GroupMode := TGroupMode(cbGroup.ItemIndex);
    Base.Filter.GroupRef := '';
  end else begin
    rec := TGEDCOMRecord(cbGroup.Items.Objects[cbGroup.ItemIndex]);
    if (rec <> nil) then begin
      Base.Filter.GroupMode := gmSelected;
      Base.Filter.GroupRef := rec.XRef;
    end else begin
      Base.Filter.GroupMode := gmAll;
      Base.Filter.GroupRef := '';
    end;
  end;

  if (cbSource.ItemIndex in [0..2]) then begin
    Base.Filter.SourceMode := TGroupMode(cbSource.ItemIndex);
    Base.Filter.SourceRef := '';
  end else begin
    rec := TGEDCOMRecord(cbSource.Items.Objects[cbSource.ItemIndex]);
    if (rec <> nil) then begin
      Base.Filter.SourceMode := gmSelected;
      Base.Filter.SourceRef := rec.XRef;
    end else begin
      Base.Filter.SourceMode := gmAll;
      Base.Filter.SourceRef := '';
    end;
  end;

  Base.ApplyFilter();
end;

procedure TfmFilter.FormShow(Sender: TObject);
var
  i: Integer;
  tree: TGEDCOMTree;
begin
  edName.Items.Assign(fmGEDKeeper.Options.NameFilters);
  cbResidence.Items.Assign(fmGEDKeeper.Options.ResidenceFilters);

  if (Base.Filter.LifeMode <> lmTimeLine) then begin
    rgLife.ItemIndex := Ord(Base.Filter.LifeMode);
    rgLife.Enabled := True;
    edAliveBeforeDate.Text := Base.Filter.AliveBeforeDate;
  end else begin
    rgLife.ItemIndex := -1;
    rgLife.Enabled := False;
    edAliveBeforeDate.Text := '';
  end;

  rgSex.ItemIndex := Ord(Base.Filter.Sex);
  edName.Text := Base.Filter.Name;
  cbResidence.Text := Base.Filter.Residence;
  CheckPatriarch.Checked := Base.Filter.PatriarchOnly;

  tree := Base.Tree;

  cbGroup.Sorted := True;
  for i := 0 to tree.RecordsCount - 1 do
    if (tree.Records[i] is TGEDCOMGroupRecord)
    then cbGroup.AddItem(TGEDCOMGroupRecord(tree.Records[i]).Name, tree.Records[i]);
  cbGroup.Sorted := False;
  cbGroup.Items.InsertObject(0, '- всё -', nil);
  cbGroup.Items.InsertObject(1, '- нет групп -', nil);
  cbGroup.Items.InsertObject(2, '- любые -', nil);

  if (Base.Filter.GroupMode <> gmSelected) then begin
    cbGroup.ItemIndex := Ord(Base.Filter.GroupMode);
  end else begin
    cbGroup.ItemIndex := cbGroup.Items.IndexOfObject(tree.XRefIndex_Find(Base.Filter.GroupRef));
  end;

  cbSource.Sorted := True;
  for i := 0 to tree.RecordsCount - 1 do
    if (tree.Records[i] is TGEDCOMSourceRecord)
    then cbSource.AddItem(TGEDCOMSourceRecord(tree.Records[i]).FiledByEntry, tree.Records[i]);
  cbSource.Sorted := False;
  cbSource.Items.InsertObject(0, '- всё -', nil);
  cbSource.Items.InsertObject(1, '- нет источников -', nil);
  cbSource.Items.InsertObject(2, '- любые -', nil);

  if (Base.Filter.SourceMode <> gmSelected) then begin
    cbSource.ItemIndex := Ord(Base.Filter.SourceMode);
  end else begin
    cbSource.ItemIndex := cbSource.Items.IndexOfObject(tree.XRefIndex_Find(Base.Filter.SourceRef));
  end;
end;

procedure TfmFilter.rgLifeClick(Sender: TObject);
begin
  edAliveBeforeDate.Enabled := (rgLife.ItemIndex = 3);
end;

function TfmFilter.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

end.
