unit GKFilter; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  VCLStub, GedCom551, GKBase, GKEngine, GKMain, GKCtrls, GKLists, GKUtils, GKLangs;

type
  TfmFilter = class(System.Windows.Forms.Form)
  private
    type
      TStringArray = array of TObject;
      
  strict private
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    rgLife: System.Windows.Forms.GroupBox;
    Label1: System.Windows.Forms.Label;
    edName: System.Windows.Forms.ComboBox;
    rgSex: System.Windows.Forms.GroupBox;
    Label2: System.Windows.Forms.Label;
    edAliveBeforeDate: System.Windows.Forms.TextBox;
    GroupBox1: System.Windows.Forms.GroupBox;
    CheckPatriarch: System.Windows.Forms.CheckBox;
    Label3: System.Windows.Forms.Label;
    cbResidence: System.Windows.Forms.ComboBox;
    Label4: System.Windows.Forms.Label;
    cbGroup: System.Windows.Forms.ComboBox;
    Label5: System.Windows.Forms.Label;
    cbSource: System.Windows.Forms.ComboBox;
    Label6: System.Windows.Forms.Label;
    cbEventVal: System.Windows.Forms.ComboBox;
    RadioButton1: System.Windows.Forms.RadioButton;
    RadioButton2: System.Windows.Forms.RadioButton;
    RadioButton3: System.Windows.Forms.RadioButton;
    RadioButton4: System.Windows.Forms.RadioButton;
    RadioButton5: System.Windows.Forms.RadioButton;
    RadioButton6: System.Windows.Forms.RadioButton;
    RadioButton7: System.Windows.Forms.RadioButton;

    FBase: TfmBase;

    function StringsToArray(aStrings: TStrings): TStringArray;

    procedure InitializeComponent;
    procedure rgLifeClick(sender: System.Object; e: System.EventArgs);
    procedure btnCancel_Click(sender: System.Object; e: System.EventArgs);
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
    procedure TfmFilter_Load(sender: System.Object; e: System.EventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;

    procedure SetLang();
  end;

implementation

procedure TfmFilter.InitializeComponent;
begin
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.rgLife := System.Windows.Forms.GroupBox.Create;
  Self.RadioButton4 := System.Windows.Forms.RadioButton.Create;
  Self.RadioButton3 := System.Windows.Forms.RadioButton.Create;
  Self.RadioButton2 := System.Windows.Forms.RadioButton.Create;
  Self.RadioButton1 := System.Windows.Forms.RadioButton.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.edName := System.Windows.Forms.ComboBox.Create;
  Self.rgSex := System.Windows.Forms.GroupBox.Create;
  Self.RadioButton7 := System.Windows.Forms.RadioButton.Create;
  Self.RadioButton5 := System.Windows.Forms.RadioButton.Create;
  Self.RadioButton6 := System.Windows.Forms.RadioButton.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.edAliveBeforeDate := System.Windows.Forms.TextBox.Create;
  Self.GroupBox1 := System.Windows.Forms.GroupBox.Create;
  Self.CheckPatriarch := System.Windows.Forms.CheckBox.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.cbResidence := System.Windows.Forms.ComboBox.Create;
  Self.Label4 := System.Windows.Forms.Label.Create;
  Self.cbGroup := System.Windows.Forms.ComboBox.Create;
  Self.Label5 := System.Windows.Forms.Label.Create;
  Self.cbSource := System.Windows.Forms.ComboBox.Create;
  Self.Label6 := System.Windows.Forms.Label.Create;
  Self.cbEventVal := System.Windows.Forms.ComboBox.Create;
  Self.rgLife.SuspendLayout;
  Self.rgSex.SuspendLayout;
  Self.GroupBox1.SuspendLayout;
  Self.SuspendLayout;
  // 
  // btnAccept
  // 
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(120, 464);
  Self.btnAccept.Name := 'btnAccept';
  Self.btnAccept.Size := System.Drawing.Size.Create(81, 25);
  Self.btnAccept.TabIndex := 8;
  Self.btnAccept.Text := 'Принять';
  Self.btnAccept.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  Include(Self.btnAccept.Click, Self.btnAccept_Click);
  // 
  // btnCancel
  // 
  Self.btnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnCancel.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnCancel.Location := System.Drawing.Point.Create(208, 464);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 9;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  Include(Self.btnCancel.Click, Self.btnCancel_Click);
  // 
  // rgLife
  // 
  Self.rgLife.Controls.Add(Self.RadioButton4);
  Self.rgLife.Controls.Add(Self.RadioButton3);
  Self.rgLife.Controls.Add(Self.RadioButton2);
  Self.rgLife.Controls.Add(Self.RadioButton1);
  Self.rgLife.Location := System.Drawing.Point.Create(8, 8);
  Self.rgLife.Name := 'rgLife';
  Self.rgLife.Size := System.Drawing.Size.Create(137, 104);
  Self.rgLife.TabIndex := 0;
  Self.rgLife.TabStop := False;
  // 
  // RadioButton4
  // 
  Self.RadioButton4.Location := System.Drawing.Point.Create(8, 72);
  Self.RadioButton4.Name := 'RadioButton4';
  Self.RadioButton4.Size := System.Drawing.Size.Create(114, 24);
  Self.RadioButton4.TabIndex := 6;
  Self.RadioButton4.Text := 'в живых до';
  Include(Self.RadioButton4.Click, Self.rgLifeClick);
  // 
  // RadioButton3
  // 
  Self.RadioButton3.Location := System.Drawing.Point.Create(8, 56);
  Self.RadioButton3.Name := 'RadioButton3';
  Self.RadioButton3.Size := System.Drawing.Size.Create(114, 24);
  Self.RadioButton3.TabIndex := 2;
  Self.RadioButton3.Text := 'только умершие';
  Include(Self.RadioButton3.Click, Self.rgLifeClick);
  // 
  // RadioButton2
  // 
  Self.RadioButton2.Location := System.Drawing.Point.Create(8, 40);
  Self.RadioButton2.Name := 'RadioButton2';
  Self.RadioButton2.Size := System.Drawing.Size.Create(114, 24);
  Self.RadioButton2.TabIndex := 1;
  Self.RadioButton2.Text := 'только живые';
  Include(Self.RadioButton2.Click, Self.rgLifeClick);
  // 
  // RadioButton1
  // 
  Self.RadioButton1.Location := System.Drawing.Point.Create(8, 24);
  Self.RadioButton1.Name := 'RadioButton1';
  Self.RadioButton1.TabIndex := 0;
  Self.RadioButton1.Text := 'все';
  Include(Self.RadioButton1.Click, Self.rgLifeClick);
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 168);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(75, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Маска имени';
  // 
  // edName
  // 
  Self.edName.Location := System.Drawing.Point.Create(8, 184);
  Self.edName.Name := 'edName';
  Self.edName.Size := System.Drawing.Size.Create(281, 21);
  Self.edName.Sorted := True;
  Self.edName.TabIndex := 3;
  Self.edName.Text := '*';
  // 
  // rgSex
  // 
  Self.rgSex.Controls.Add(Self.RadioButton7);
  Self.rgSex.Controls.Add(Self.RadioButton5);
  Self.rgSex.Controls.Add(Self.RadioButton6);
  Self.rgSex.Location := System.Drawing.Point.Create(152, 8);
  Self.rgSex.Name := 'rgSex';
  Self.rgSex.Size := System.Drawing.Size.Create(137, 104);
  Self.rgSex.TabIndex := 1;
  Self.rgSex.TabStop := False;
  // 
  // RadioButton7
  // 
  Self.RadioButton7.Location := System.Drawing.Point.Create(8, 72);
  Self.RadioButton7.Name := 'RadioButton7';
  Self.RadioButton7.Size := System.Drawing.Size.Create(114, 24);
  Self.RadioButton7.TabIndex := 5;
  Self.RadioButton7.Text := 'только женщины';
  // 
  // RadioButton5
  // 
  Self.RadioButton5.Location := System.Drawing.Point.Create(8, 24);
  Self.RadioButton5.Name := 'RadioButton5';
  Self.RadioButton5.TabIndex := 4;
  Self.RadioButton5.Text := 'все';
  // 
  // RadioButton6
  // 
  Self.RadioButton6.Location := System.Drawing.Point.Create(8, 48);
  Self.RadioButton6.Name := 'RadioButton6';
  Self.RadioButton6.Size := System.Drawing.Size.Create(114, 24);
  Self.RadioButton6.TabIndex := 3;
  Self.RadioButton6.Text := 'только мужчины';
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(8, 120);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(70, 13);
  Self.Label2.TabIndex := 1;
  Self.Label2.Text := 'В живых до:';
  // 
  // edAliveBeforeDate
  // 
  Self.edAliveBeforeDate.Enabled := False;
  Self.edAliveBeforeDate.Location := System.Drawing.Point.Create(8, 136);
  Self.edAliveBeforeDate.MaxLength := 10;
  Self.edAliveBeforeDate.Name := 'edAliveBeforeDate';
  Self.edAliveBeforeDate.Size := System.Drawing.Size.Create(137, 21);
  Self.edAliveBeforeDate.TabIndex := 2;
  Self.edAliveBeforeDate.Text := '  .  .    ';
  // 
  // GroupBox1
  // 
  Self.GroupBox1.Controls.Add(Self.CheckPatriarch);
  Self.GroupBox1.Location := System.Drawing.Point.Create(8, 408);
  Self.GroupBox1.Name := 'GroupBox1';
  Self.GroupBox1.Size := System.Drawing.Size.Create(281, 41);
  Self.GroupBox1.TabIndex := 7;
  Self.GroupBox1.TabStop := False;
  // 
  // CheckPatriarch
  // 
  Self.CheckPatriarch.Location := System.Drawing.Point.Create(8, 16);
  Self.CheckPatriarch.Name := 'CheckPatriarch';
  Self.CheckPatriarch.Size := System.Drawing.Size.Create(185, 17);
  Self.CheckPatriarch.TabIndex := 0;
  Self.CheckPatriarch.Text := 'Только главы семей';
  // 
  // Label3
  // 
  Self.Label3.Location := System.Drawing.Point.Create(8, 216);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(130, 13);
  Self.Label3.TabIndex := 2;
  Self.Label3.Text := 'Маска местожительства';
  // 
  // cbResidence
  // 
  Self.cbResidence.Location := System.Drawing.Point.Create(8, 232);
  Self.cbResidence.Name := 'cbResidence';
  Self.cbResidence.Size := System.Drawing.Size.Create(281, 21);
  Self.cbResidence.Sorted := True;
  Self.cbResidence.TabIndex := 4;
  Self.cbResidence.Text := '*';
  // 
  // Label4
  // 
  Self.Label4.Location := System.Drawing.Point.Create(8, 312);
  Self.Label4.Name := 'Label4';
  Self.Label4.Size := System.Drawing.Size.Create(45, 13);
  Self.Label4.TabIndex := 3;
  Self.Label4.Text := 'Группы';
  // 
  // cbGroup
  // 
  Self.cbGroup.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.cbGroup.Location := System.Drawing.Point.Create(8, 328);
  Self.cbGroup.Name := 'cbGroup';
  Self.cbGroup.Size := System.Drawing.Size.Create(281, 21);
  Self.cbGroup.TabIndex := 5;
  // 
  // Label5
  // 
  Self.Label5.Location := System.Drawing.Point.Create(8, 360);
  Self.Label5.Name := 'Label5';
  Self.Label5.Size := System.Drawing.Size.Create(60, 13);
  Self.Label5.TabIndex := 4;
  Self.Label5.Text := 'Источники';
  // 
  // cbSource
  // 
  Self.cbSource.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.cbSource.Location := System.Drawing.Point.Create(8, 376);
  Self.cbSource.Name := 'cbSource';
  Self.cbSource.Size := System.Drawing.Size.Create(281, 21);
  Self.cbSource.TabIndex := 6;
  // 
  // Label6
  // 
  Self.Label6.Location := System.Drawing.Point.Create(8, 264);
  Self.Label6.Name := 'Label6';
  Self.Label6.Size := System.Drawing.Size.Create(85, 13);
  Self.Label6.TabIndex := 5;
  Self.Label6.Text := 'Маска фактов';
  // 
  // cbEventVal
  // 
  Self.cbEventVal.Location := System.Drawing.Point.Create(8, 280);
  Self.cbEventVal.Name := 'cbEventVal';
  Self.cbEventVal.Size := System.Drawing.Size.Create(281, 21);
  Self.cbEventVal.Sorted := True;
  Self.cbEventVal.TabIndex := 10;
  Self.cbEventVal.Text := '*';
  // 
  // TfmFilter
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(297, 497);
  Self.Controls.Add(Self.Label1);
  Self.Controls.Add(Self.Label2);
  Self.Controls.Add(Self.Label3);
  Self.Controls.Add(Self.Label4);
  Self.Controls.Add(Self.Label5);
  Self.Controls.Add(Self.Label6);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Controls.Add(Self.rgLife);
  Self.Controls.Add(Self.edName);
  Self.Controls.Add(Self.rgSex);
  Self.Controls.Add(Self.edAliveBeforeDate);
  Self.Controls.Add(Self.GroupBox1);
  Self.Controls.Add(Self.cbResidence);
  Self.Controls.Add(Self.cbGroup);
  Self.Controls.Add(Self.cbSource);
  Self.Controls.Add(Self.cbEventVal);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmFilter';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Фильтр';
  Include(Self.Load, Self.TfmFilter_Load);
  Self.rgLife.ResumeLayout(False);
  Self.rgSex.ResumeLayout(False);
  Self.GroupBox1.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmFilter.Create(aBase: TfmBase);
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  //OnShow = FormShow
  //Self.edAliveBeforeDate.EditMask := '!99/99/9999;1;_';

  SetLang();
end;

procedure TfmFilter.SetLang();
begin
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  RadioButton1.Text := LSList[LSID_All];
  RadioButton2.Text := LSList[LSID_OnlyAlive];
  RadioButton3.Text := LSList[LSID_OnlyDied];
  RadioButton4.Text := LSList[LSID_AliveBefore].ToLower();

  RadioButton5.Text := LSList[LSID_All];
  RadioButton6.Text := LSList[LSID_OnlyMans];
  RadioButton7.Text := LSList[LSID_OnlyWomans];

  Label2.Text := LSList[LSID_AliveBefore] + ':';
  Label1.Text := LSList[LSID_NameMask];
  Label3.Text := LSList[LSID_PlaceMask];
  Label6.Text := LSList[LSID_EventMask];
  Label4.Text := LSList[LSID_RPGroups];
  Label5.Text := LSList[LSID_RPSources];
  CheckPatriarch.Text := LSList[LSID_OnlyPatriarchs];
end;

procedure TfmFilter.btnCancel_Click(sender: System.Object; e: System.EventArgs);
begin
  Base.Filter.Clear();
  Base.ApplyFilter();
end;

procedure TfmFilter.btnAccept_Click(sender: System.Object; e: System.EventArgs);
var
  dt: DateTime;
  fs: string;
  rec: TGEDCOMRecord;
  life_sel, sex_sel: Integer;
begin
  fs := edName.Text.Trim();
  if (fs <> '') and (fs <> '*') then begin
    if (fmGEDKeeper.Options.NameFilters.IndexOf(fs) < 0)
    then fmGEDKeeper.Options.NameFilters.Add(fs);
  end;

  fs := cbResidence.Text.Trim();
  if (fs <> '') and (fs <> '*') then begin
    if (fmGEDKeeper.Options.ResidenceFilters.IndexOf(fs) < 0)
    then fmGEDKeeper.Options.ResidenceFilters.Add(fs);
  end;

  fs := cbEventVal.Text.Trim();
  if (fs <> '') and (fs <> '*') then begin
    if (fmGEDKeeper.Options.EventFilters.IndexOf(fs) < 0)
    then fmGEDKeeper.Options.EventFilters.Add(fs);
  end;
  //

  Base.Filter.PatriarchOnly := CheckPatriarch.Checked;

  if RadioButton1.Checked then life_sel := 0;
  if RadioButton2.Checked then life_sel := 1;
  if RadioButton3.Checked then life_sel := 2;
  if RadioButton4.Checked then life_sel := 3;

  if (Base.Filter.LifeMode <> lmTimeLine) then begin
    Base.Filter.AliveBeforeDate := edAliveBeforeDate.Text;
    if (life_sel = 3) then begin
      try
        dt := DateTime.Parse(edAliveBeforeDate.Text);
        //Hole(dt);
      except
        TGKUtils.ShowError(LSList[LSID_DateInvalid]);
        Self.DialogResult := System.Windows.Forms.DialogResult.None;
      end;
    end;
    Base.Filter.LifeMode := TGenEngine.TLifeMode(life_sel);
  end;

  if RadioButton5.Checked then sex_sel := 0;
  if RadioButton6.Checked then sex_sel := 1;
  if RadioButton7.Checked then sex_sel := 2;

  Base.Filter.Sex := TGEDCOMObject.TGEDCOMSex(sex_sel);

  if (edName.Text = '') then edName.Text := '*';
  Base.Filter.Name := edName.Text;

  if (cbResidence.Text = '') then cbResidence.Text := '*';
  Base.Filter.Residence := cbResidence.Text;

  if (cbEventVal.Text = '') then cbEventVal.Text := '*';
  Base.Filter.EventVal := cbEventVal.Text;

  if (cbGroup.SelectedIndex in [0..2]) then begin
    Base.Filter.GroupMode := TFilter.TGroupMode(cbGroup.SelectedIndex);
    Base.Filter.GroupRef := '';
  end else begin
    rec := TGEDCOMRecord(TComboItem(cbGroup.Items[cbGroup.SelectedIndex]).Data);
    if (rec <> nil) then begin
      Base.Filter.GroupMode := gmSelected;
      Base.Filter.GroupRef := rec.XRef;
    end else begin
      Base.Filter.GroupMode := gmAll;
      Base.Filter.GroupRef := '';
    end;
  end;

  if (cbSource.SelectedIndex in [0..2]) then begin
    Base.Filter.SourceMode := TFilter.TGroupMode(cbSource.SelectedIndex);
    Base.Filter.SourceRef := '';
  end else begin
    rec := TGEDCOMRecord(TComboItem(cbSource.Items[cbSource.SelectedIndex]).Data);
    if (rec <> nil) then begin
      Base.Filter.SourceMode := gmSelected;
      Base.Filter.SourceRef := rec.XRef;
    end else begin
      Base.Filter.SourceMode := gmAll;
      Base.Filter.SourceRef := '';
    end;
  end;

  Base.ApplyFilter();

  Self.DialogResult := System.Windows.Forms.DialogResult.OK;
end;

function TfmFilter.StringsToArray(aStrings: TStrings): TStringArray;
var
  i: Integer;
begin
  SetLength(Result, aStrings.Count);
  for i := 0 to aStrings.Count - 1 do Result[i] := aStrings[i];
end;

procedure TfmFilter.TfmFilter_Load(sender: System.Object; e: System.EventArgs);
var
  i, life_sel, sex_sel: Integer;
  tree: TGEDCOMTree;
begin
  edName.Items.AddRange(StringsToArray(fmGEDKeeper.Options.NameFilters));
  cbResidence.Items.AddRange(StringsToArray(fmGEDKeeper.Options.ResidenceFilters));
  cbEventVal.Items.AddRange(StringsToArray(fmGEDKeeper.Options.EventFilters));

  if (Base.Filter.LifeMode <> lmTimeLine) then begin
    life_sel := Ord(Base.Filter.LifeMode);
    rgLife.Enabled := True;
    edAliveBeforeDate.Text := Base.Filter.AliveBeforeDate;
  end else begin
    life_sel := -1;
    rgLife.Enabled := False;
    edAliveBeforeDate.Text := '';
  end;

  if (life_sel = 0) then RadioButton1.Checked := True
  else
  if (life_sel = 1) then RadioButton2.Checked := True
  else
  if (life_sel = 2) then RadioButton3.Checked := True
  else
  if (life_sel = 3) then RadioButton4.Checked := True;

  sex_sel := Ord(Base.Filter.Sex);

  if (sex_sel = 0) then RadioButton5.Checked := True
  else
  if (sex_sel = 1) then RadioButton6.Checked := True
  else
  if (sex_sel = 2) then RadioButton7.Checked := True;

  edName.Text := Base.Filter.Name;
  cbResidence.Text := Base.Filter.Residence;
  cbEventVal.Text := Base.Filter.EventVal;
  CheckPatriarch.Checked := Base.Filter.PatriarchOnly;

  tree := Base.Tree;

  cbGroup.Sorted := True;
  for i := 0 to tree.RecordsCount - 1 do
    if (tree.Records[i] is TGEDCOMGroupRecord)
    then cbGroup.Items.Add(TComboItem.Create(TGEDCOMGroupRecord(tree.Records[i]).Name, tree.Records[i]));
  cbGroup.Sorted := False;
  cbGroup.Items.Insert(0, TComboItem.Create(LSList[LSID_SrcAll], nil));
  cbGroup.Items.Insert(1, TComboItem.Create(LSList[LSID_SrcNot], nil));
  cbGroup.Items.Insert(2, TComboItem.Create(LSList[LSID_SrcAny], nil));

  if (Base.Filter.GroupMode <> gmSelected) then begin
    cbGroup.SelectedIndex := Ord(Base.Filter.GroupMode);
  end else begin
    cbGroup.SelectedIndex := cbGroup.Items.IndexOf(tree.XRefIndex_Find(Base.Filter.GroupRef));
  end;

  cbSource.Sorted := True;
  for i := 0 to tree.RecordsCount - 1 do
    if (tree.Records[i] is TGEDCOMSourceRecord)
    then cbSource.Items.Add(TComboItem.Create(TGEDCOMSourceRecord(tree.Records[i]).FiledByEntry, tree.Records[i]));
  cbSource.Sorted := False;
  cbSource.Items.Insert(0, TComboItem.Create(LSList[LSID_SrcAll], nil));
  cbSource.Items.Insert(1, TComboItem.Create(LSList[LSID_SrcNot], nil));
  cbSource.Items.Insert(2, TComboItem.Create(LSList[LSID_SrcAny], nil));

  if (Base.Filter.SourceMode <> gmSelected) then begin
    cbSource.SelectedIndex := Ord(Base.Filter.SourceMode);
  end else begin
    cbSource.SelectedIndex := cbSource.Items.IndexOf(tree.XRefIndex_Find(Base.Filter.SourceRef));
  end;
end;

procedure TfmFilter.rgLifeClick(sender: System.Object; e: System.EventArgs);
begin
  edAliveBeforeDate.Enabled := (RadioButton4.Checked);
end;

end.
