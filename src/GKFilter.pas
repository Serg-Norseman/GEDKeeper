unit GKFilter;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, Mask, GKBase;

type
  TfmFilter = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    rgLife: TRadioGroup;
    Label1: TLabel;
    EditName: TComboBox;
    rgSex: TRadioGroup;
    Label2: TLabel;
    edAliveBeforeDate: TMaskEdit;
    GroupBox1: TGroupBox;
    CheckPatriarch: TCheckBox;
    Label3: TLabel;
    cbResidence: TComboBox;
    Label4: TLabel;
    cbGroup: TComboBox;
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

uses GKMain, GKCommon, GedCom551, GKLists;

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
  fs := Trim(EditName.Text);
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

  Base.Filter.AliveBeforeDate := edAliveBeforeDate.Text;
  Base.Filter.PatriarchOnly := CheckPatriarch.Checked;

  if (rgLife.ItemIndex = 3) then begin
    try
      dt := StrToDate(edAliveBeforeDate.Text);
    except
      MessageDlg('Дата неверна', mtError, [mbOk], 0);
      ModalResult := mrNone;
    end;
  end;

  Base.Filter.LifeMode := TLifeMode(rgLife.ItemIndex);
  Base.Filter.Sex := TGEDCOMSex(rgSex.ItemIndex);

  if (EditName.Text = '') then EditName.Text := '*';
  Base.Filter.Name := EditName.Text;

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

  Base.ApplyFilter();
end;

procedure TfmFilter.FormShow(Sender: TObject);
var
  i: Integer;
  tree: TGEDCOMTree;
begin
  EditName.Items.Assign(fmGEDKeeper.Options.NameFilters);
  cbResidence.Items.Assign(fmGEDKeeper.Options.ResidenceFilters);

  rgLife.ItemIndex := Ord(Base.Filter.LifeMode);
  rgSex.ItemIndex := Ord(Base.Filter.Sex);
  EditName.Text := Base.Filter.Name;
  cbResidence.Text := Base.Filter.Residence;
  edAliveBeforeDate.Text := Base.Filter.AliveBeforeDate;
  CheckPatriarch.Checked := Base.Filter.PatriarchOnly;

  tree := Base.Tree;
  cbGroup.AddItem('- всё -', nil);
  cbGroup.AddItem('- нет групп -', nil);
  cbGroup.AddItem('- любые -', nil);
  for i := 0 to tree.RecordsCount - 1 do
    if (tree.Records[i] is TGEDCOMGroupRecord)
    then cbGroup.AddItem(TGEDCOMGroupRecord(tree.Records[i]).Name, tree.Records[i]);

  if (Base.Filter.GroupMode <> gmSelected) then begin
    cbGroup.ItemIndex := Ord(Base.Filter.GroupMode);
  end else begin
    cbGroup.ItemIndex := cbGroup.Items.IndexOfObject(tree.XRefIndex_Find(Base.Filter.GroupRef));
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
