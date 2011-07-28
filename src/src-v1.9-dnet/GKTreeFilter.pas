unit GKTreeFilter; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  VCLStub, GedCom551, GKCtrls, GKBase, GKChartCore, GKLists, GKEngine, GKUtils,
  GKLangs;

type
  TfmTreeFilter = class(System.Windows.Forms.Form)
  strict private
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    Label5: System.Windows.Forms.Label;
    cbSource: System.Windows.Forms.ComboBox;
    rgBranchCut: System.Windows.Forms.GroupBox;
    rbCutNone: System.Windows.Forms.RadioButton;
    rbCutYears: System.Windows.Forms.RadioButton;
    rbCutPersons: System.Windows.Forms.RadioButton;
    Label1: System.Windows.Forms.Label;
    edYear: System.Windows.Forms.NumericUpDown;
    Panel1: System.Windows.Forms.Panel;

    FBase: TfmBase;
    FFilter: TChartFilter;
    FPersonsList: TSheetList;
    FTemp: string;

    procedure ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
    procedure UpdateControls();
    procedure InitializeComponent;
    procedure rbCutNoneClick(sender: System.Object; e: System.EventArgs);
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
    procedure btnCancel_Click(sender: System.Object; e: System.EventArgs);
    procedure TfmTreeFilter_Load(sender: System.Object; e: System.EventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property Filter: TChartFilter read FFilter write FFilter;
  end;

implementation

procedure TfmTreeFilter.InitializeComponent;
type
  TArrayOfInteger = array of Integer;
begin
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.Label5 := System.Windows.Forms.Label.Create;
  Self.cbSource := System.Windows.Forms.ComboBox.Create;
  Self.rgBranchCut := System.Windows.Forms.GroupBox.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.rbCutNone := System.Windows.Forms.RadioButton.Create;
  Self.rbCutYears := System.Windows.Forms.RadioButton.Create;
  Self.rbCutPersons := System.Windows.Forms.RadioButton.Create;
  Self.edYear := System.Windows.Forms.NumericUpDown.Create;
  Self.Panel1 := System.Windows.Forms.Panel.Create;
  Self.rgBranchCut.SuspendLayout;
  (System.ComponentModel.ISupportInitialize(Self.edYear)).BeginInit;
  Self.SuspendLayout;
  // 
  // btnAccept
  // 
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(216, 328);
  Self.btnAccept.Name := 'btnAccept';
  Self.btnAccept.Size := System.Drawing.Size.Create(81, 25);
  Self.btnAccept.TabIndex := 1;
  Self.btnAccept.Text := 'Принять';
  Self.btnAccept.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  Include(Self.btnAccept.Click, Self.btnAccept_Click);
  // 
  // btnCancel
  // 
  Self.btnCancel.AccessibleName := '';
  Self.btnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnCancel.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnCancel.Location := System.Drawing.Point.Create(304, 328);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 2;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  Include(Self.btnCancel.Click, Self.btnCancel_Click);
  // 
  // Label5
  // 
  Self.Label5.Location := System.Drawing.Point.Create(8, 280);
  Self.Label5.Name := 'Label5';
  Self.Label5.Size := System.Drawing.Size.Create(54, 13);
  Self.Label5.TabIndex := 0;
  Self.Label5.Text := 'Источники';
  // 
  // cbSource
  // 
  Self.cbSource.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.cbSource.Location := System.Drawing.Point.Create(8, 296);
  Self.cbSource.Name := 'cbSource';
  Self.cbSource.Size := System.Drawing.Size.Create(377, 21);
  Self.cbSource.TabIndex := 0;
  // 
  // rgBranchCut
  // 
  Self.rgBranchCut.Controls.Add(Self.Label1);
  Self.rgBranchCut.Controls.Add(Self.rbCutNone);
  Self.rgBranchCut.Controls.Add(Self.rbCutYears);
  Self.rgBranchCut.Controls.Add(Self.rbCutPersons);
  Self.rgBranchCut.Controls.Add(Self.edYear);
  Self.rgBranchCut.Location := System.Drawing.Point.Create(8, 8);
  Self.rgBranchCut.Name := 'rgBranchCut';
  Self.rgBranchCut.Size := System.Drawing.Size.Create(377, 265);
  Self.rgBranchCut.TabIndex := 3;
  Self.rgBranchCut.TabStop := False;
  Self.rgBranchCut.Text := 'Отсечение ветвей';
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(32, 80);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(23, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Год';
  // 
  // rbCutNone
  // 
  Self.rbCutNone.Checked := True;
  Self.rbCutNone.Location := System.Drawing.Point.Create(16, 24);
  Self.rbCutNone.Name := 'rbCutNone';
  Self.rbCutNone.Size := System.Drawing.Size.Create(249, 17);
  Self.rbCutNone.TabIndex := 0;
  Self.rbCutNone.TabStop := True;
  Self.rbCutNone.Text := 'нет';
  Include(Self.rbCutNone.Click, Self.rbCutNoneClick);
  // 
  // rbCutYears
  // 
  Self.rbCutYears.Location := System.Drawing.Point.Create(16, 48);
  Self.rbCutYears.Name := 'rbCutYears';
  Self.rbCutYears.Size := System.Drawing.Size.Create(249, 17);
  Self.rbCutYears.TabIndex := 1;
  Self.rbCutYears.Text := 'по границе лет';
  Include(Self.rbCutYears.Click, Self.rbCutNoneClick);
  // 
  // rbCutPersons
  // 
  Self.rbCutPersons.Location := System.Drawing.Point.Create(16, 104);
  Self.rbCutPersons.Name := 'rbCutPersons';
  Self.rbCutPersons.Size := System.Drawing.Size.Create(249, 17);
  Self.rbCutPersons.TabIndex := 2;
  Self.rbCutPersons.Text := 'по заданным лицам';
  Include(Self.rbCutPersons.Click, Self.rbCutNoneClick);
  // 
  // edYear
  // 
  Self.edYear.Increment := System.Decimal.Create(TArrayOfInteger.Create(10, 0, 
          0, 0));
  Self.edYear.Location := System.Drawing.Point.Create(64, 72);
  Self.edYear.Maximum := System.Decimal.Create(TArrayOfInteger.Create(3000, 0, 
          0, 0));
  Self.edYear.Name := 'edYear';
  Self.edYear.Size := System.Drawing.Size.Create(121, 21);
  Self.edYear.TabIndex := 3;
  // 
  // Panel1
  // 
  Self.Panel1.BorderStyle := System.Windows.Forms.BorderStyle.Fixed3D;
  Self.Panel1.Location := System.Drawing.Point.Create(16, 128);
  Self.Panel1.Name := 'Panel1';
  Self.Panel1.Size := System.Drawing.Size.Create(360, 136);
  Self.Panel1.TabIndex := 5;
  // 
  // TfmTreeFilter
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(393, 361);
  Self.Controls.Add(Self.Label5);
  Self.Controls.Add(Self.cbSource);
  Self.Controls.Add(Self.Panel1);
  Self.Controls.Add(Self.rgBranchCut);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmTreeFilter';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Фильтр';
  Include(Self.Load, Self.TfmTreeFilter_Load);
  Self.rgBranchCut.ResumeLayout(False);
  (System.ComponentModel.ISupportInitialize(Self.edYear)).EndInit;
  Self.ResumeLayout(False);
end;

constructor TfmTreeFilter.Create(aBase: TfmBase);
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  FPersonsList := TSheetList.Create(Panel1);
  FPersonsList.Buttons := TEnumSet.Create([lbAdd, lbDelete]);
  FPersonsList.OnModify := ListModify;
  FPersonsList.List.AddListColumn(LSList[LSID_RPIndividuals], 350, False);

  // SetLang
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  Text := LSList[LSID_MIFilter];

  rgBranchCut.Text := LSList[LSID_BranchCut];
  rbCutNone.Text := LSList[LSID_Not];
  rbCutYears.Text := LSList[LSID_BCut_Years];
  Label1.Text := LSList[LSID_Year];
  rbCutPersons.Text := LSList[LSID_BCut_Persons];
  Label5.Text := LSList[LSID_RPSources];
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
  FPersonsList.Enabled := (FFilter.BranchCut = bcPersons);
  edYear.Text := FFilter.BranchYear.ToString();

  FPersonsList.List.Items.Clear;
  for i := 1 to TGKUtils.GetTokensCount(FTemp, ';') do begin
    xref := TGKUtils.GetToken(FTemp, ';', i);
    p := TGEDCOMIndividualRecord(Base.Tree.XRefIndex_Find(xref));
    FPersonsList.List.AddItem(TGenEngine.GetNameStr(p), p);
  end;
end;

procedure TfmTreeFilter.ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
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
        then FTemp := FTemp.Replace(i_rec.XRef + ';', '');
      end;
    end;
  end;

  UpdateControls();
end;

procedure TfmTreeFilter.btnCancel_Click(sender: System.Object; e: System.EventArgs);
begin
  FFilter.Clear();
  //Base.ApplyFilter();
end;

procedure TfmTreeFilter.btnAccept_Click(sender: System.Object; e: System.EventArgs);
var
  rec: TGEDCOMRecord;
begin
  if rbCutNone.Checked
  then FFilter.BranchCut := bcNone
  else
  if rbCutYears.Checked then begin
    FFilter.BranchCut := bcYears;
    FFilter.BranchYear := Int32.Parse(edYear.Text);
  end
  else
  if rbCutPersons.Checked then begin
    FFilter.BranchCut := bcPersons;
    FFilter.BranchPersons := FTemp;
  end;

  if (cbSource.SelectedIndex in [0..2]) then begin
    FFilter.SourceMode := TFilter.TGroupMode(cbSource.SelectedIndex);
    FFilter.SourceRef := '';
  end else begin
    rec := TGEDCOMRecord(TComboItem(cbSource.Items[cbSource.SelectedIndex]).Data);
    if (rec <> nil) then begin
      FFilter.SourceMode := gmSelected;
      FFilter.SourceRef := rec.XRef;
    end else begin
      FFilter.SourceMode := gmAll;
      FFilter.SourceRef := '';
    end;
  end;

  Self.DialogResult := System.Windows.Forms.DialogResult.OK; 
  //Base.ApplyFilter();
end;

procedure TfmTreeFilter.TfmTreeFilter_Load(sender: System.Object; e: System.EventArgs);
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
    then cbSource.Items.Add(TComboItem.Create(TGEDCOMSourceRecord(tree.Records[i]).FiledByEntry, tree.Records[i]));
  cbSource.Sorted := False;
  cbSource.Items.Insert(0, LSList[LSID_SrcAll]);
  cbSource.Items.Insert(1, LSList[LSID_SrcNot]);
  cbSource.Items.Insert(2, LSList[LSID_SrcAny]);

  if (FFilter.SourceMode <> gmSelected) then begin
    cbSource.SelectedIndex := Ord(FFilter.SourceMode);
  end else begin
    cbSource.SelectedIndex := cbSource.Items.IndexOf(tree.XRefIndex_Find(FFilter.SourceRef));
  end;
end;

procedure TfmTreeFilter.rbCutNoneClick(sender: System.Object; e: System.EventArgs);
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
