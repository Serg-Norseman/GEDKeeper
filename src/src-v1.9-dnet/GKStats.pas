unit GKStats; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  System.Collections, VCLStub, ZedGraph,
  GedCom551, GKCtrls, GKBase, GKLists, GKEngine, GKMain, GKUtils, GKLangs;

type
  TfmStats = class(System.Windows.Forms.Form)
  strict private
    type
      TChartStyle = (csBar, csPoint);

      TTitleRec = record
        Title, Cap: LSID;
      end;

    const
      Titles: array [TGenEngine.TStatMode] of TTitleRec = (
        (Title: LSID_AncestorsCount; Cap: LSID_Name),
        (Title: LSID_DescendantsCount; Cap: LSID_Name),
        (Title: LSID_GenerationsCount; Cap: LSID_Name),
        (Title: LSID_Surname; Cap: LSID_Surname),
        (Title: LSID_Name; Cap: LSID_Name),
        (Title: LSID_Patronymic; Cap: LSID_Patronymic),
        (Title: LSID_Age; Cap: LSID_Age),
        (Title: LSID_LifeExpectancy; Cap: LSID_Age),
        (Title: LSID_BirthYears; Cap: LSID_BirthYears),
        (Title: LSID_BirthYearsDec; Cap: LSID_BirthYears),
        (Title: LSID_DeathYears; Cap: LSID_DeathYears),
        (Title: LSID_DeathYearsDec; Cap: LSID_DeathYears),
        (Title: LSID_ChildsCount; Cap: LSID_Name),
        (Title: LSID_DistrChilds; Cap: LSID_ChildsCount),
        (Title: LSID_BirthPlace; Cap: LSID_BirthPlace),
        (Title: LSID_DeathPlace; Cap: LSID_DeathPlace),
        (Title: LSID_Residence; Cap: LSID_Residence),
        (Title: LSID_Occupation; Cap: LSID_Occupation),

        (Title: LSID_Religion; Cap: LSID_Religion),
        (Title: LSID_Nationality; Cap: LSID_Nationality),
        (Title: LSID_Education; Cap: LSID_Education),
        (Title: LSID_Caste; Cap: LSID_Caste),

        (Title: LSID_AgeFirstborn; Cap: LSID_Name),
        (Title: LSID_MarriagesCount; Cap: LSID_Name),
        (Title: LSID_MarriagesAge; Cap: LSID_Name),
        (Title: LSID_DiffSpouses; Cap: LSID_Family),

        (Title: LSID_Hobby; Cap: LSID_Hobby),
        (Title: LSID_Award; Cap: LSID_Award),

        (Title: LSID_Mili; Cap: LSID_Mili),
        (Title: LSID_MiliInd; Cap: LSID_MiliInd),
        (Title: LSID_MiliDis; Cap: LSID_MiliDis),
        (Title: LSID_MiliRank; Cap: LSID_MiliRank)
      );

  var
    GroupBox1: System.Windows.Forms.GroupBox;
    Panel1: System.Windows.Forms.Panel;
    ToolBar1: System.Windows.Forms.Panel;
    cbType: System.Windows.Forms.ComboBox;
    ListCommon: System.Windows.Forms.ListView;
    ColumnHeader1: System.Windows.Forms.ColumnHeader;
    ColumnHeader2: System.Windows.Forms.ColumnHeader;
    ColumnHeader3: System.Windows.Forms.ColumnHeader;
    ColumnHeader4: System.Windows.Forms.ColumnHeader;

    ListStats: TGKListView;
    zgc: ZedGraphControl;

    FBase: TfmBase;
    ChartTitle, ChartXTitle, ChartYTitle: string;

    procedure PrepareArray(gPane: GraphPane; aStyle: TChartStyle; aExcludeUnknowns: Boolean = True);
    procedure CalcStats(aTree: TGEDCOMTree; aMode: TGenEngine.TStatMode);

    procedure InitializeComponent;
    procedure TfmStats_Load(sender: System.Object; e: System.EventArgs);
    procedure TfmStats_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
    procedure cbType_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;

    procedure SetLang();
  end;

implementation

procedure TfmStats.InitializeComponent;
type
  TArrayOfSystem_Windows_Forms_ColumnHeader = array of System.Windows.Forms.ColumnHeader;
begin
  Self.GroupBox1 := System.Windows.Forms.GroupBox.Create;
  Self.ListCommon := System.Windows.Forms.ListView.Create;
  Self.ColumnHeader1 := System.Windows.Forms.ColumnHeader.Create;
  Self.ColumnHeader2 := System.Windows.Forms.ColumnHeader.Create;
  Self.ColumnHeader3 := System.Windows.Forms.ColumnHeader.Create;
  Self.ColumnHeader4 := System.Windows.Forms.ColumnHeader.Create;
  Self.Panel1 := System.Windows.Forms.Panel.Create;
  Self.ToolBar1 := System.Windows.Forms.Panel.Create;
  Self.cbType := System.Windows.Forms.ComboBox.Create;
  Self.GroupBox1.SuspendLayout;
  Self.Panel1.SuspendLayout;
  Self.ToolBar1.SuspendLayout;
  Self.SuspendLayout;
  // 
  // GroupBox1
  // 
  Self.GroupBox1.Controls.Add(Self.ListCommon);
  Self.GroupBox1.Dock := System.Windows.Forms.DockStyle.Top;
  Self.GroupBox1.Location := System.Drawing.Point.Create(0, 0);
  Self.GroupBox1.Name := 'GroupBox1';
  Self.GroupBox1.Size := System.Drawing.Size.Create(754, 192);
  Self.GroupBox1.TabIndex := 0;
  Self.GroupBox1.TabStop := False;
  Self.GroupBox1.Text := 'Сводка';
  // 
  // ListCommon
  //
  Self.ListCommon.Columns.AddRange(TArrayOfSystem_Windows_Forms_ColumnHeader.Create(Self.ColumnHeader1, 
          Self.ColumnHeader2, Self.ColumnHeader3, Self.ColumnHeader4));
  Self.ListCommon.Dock := System.Windows.Forms.DockStyle.Fill;
  Self.ListCommon.FullRowSelect := True;
  Self.ListCommon.Location := System.Drawing.Point.Create(3, 17);
  Self.ListCommon.Name := 'ListCommon';
  Self.ListCommon.Size := System.Drawing.Size.Create(748, 172);
  Self.ListCommon.TabIndex := 0;
  Self.ListCommon.View := System.Windows.Forms.View.Details;
  // 
  // ColumnHeader1
  //
  Self.ColumnHeader1.Text := 'Параметр';
  Self.ColumnHeader1.Width := 300;
  // 
  // ColumnHeader2
  // 
  Self.ColumnHeader2.Text := 'Всего';
  Self.ColumnHeader2.Width := 100;
  // 
  // ColumnHeader3
  // 
  Self.ColumnHeader3.Text := 'Мужчины';
  Self.ColumnHeader3.Width := 100;
  // 
  // ColumnHeader4
  // 
  Self.ColumnHeader4.Text := 'Женщины';
  Self.ColumnHeader4.Width := 100;
  // 
  // Panel1
  // 
  Self.Panel1.Controls.Add(Self.ToolBar1);
  Self.Panel1.Dock := System.Windows.Forms.DockStyle.Fill;
  Self.Panel1.Location := System.Drawing.Point.Create(0, 192);
  Self.Panel1.Name := 'Panel1';
  Self.Panel1.Size := System.Drawing.Size.Create(754, 311);
  Self.Panel1.TabIndex := 2;
  // 
  // ToolBar1
  // 
  Self.ToolBar1.Controls.Add(Self.cbType);
  Self.ToolBar1.Dock := System.Windows.Forms.DockStyle.Top;
  Self.ToolBar1.Location := System.Drawing.Point.Create(0, 0);
  Self.ToolBar1.Name := 'ToolBar1';
  Self.ToolBar1.Size := System.Drawing.Size.Create(754, 21);
  Self.ToolBar1.TabIndex := 0;
  // 
  // cbType
  // 
  Self.cbType.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.cbType.Location := System.Drawing.Point.Create(8, 0);
  Self.cbType.Name := 'cbType';
  Self.cbType.Size := System.Drawing.Size.Create(233, 21);
  Self.cbType.TabIndex := 0;
  Include(Self.cbType.SelectedIndexChanged, Self.cbType_SelectedIndexChanged);
  // 
  // TfmStats
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.ClientSize := System.Drawing.Size.Create(754, 503);
  Self.Controls.Add(Self.Panel1);
  Self.Controls.Add(Self.GroupBox1);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.KeyPreview := True;
  Self.Name := 'TfmStats';
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Статистика';
  Include(Self.KeyDown, Self.TfmStats_KeyDown);
  Include(Self.Load, Self.TfmStats_Load);
  Self.GroupBox1.ResumeLayout(False);
  Self.Panel1.ResumeLayout(False);
  Self.ToolBar1.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmStats.Create(aBase: TfmBase);
var
  i: TGenEngine.TStatMode;
  spl: Splitter;
begin
  inherited Create;
  InitializeComponent;

  MdiParent := fmGEDKeeper;
  FBase := aBase;

  zgc := ZedGraphControl.Create;
  zgc.IsShowPointValues := True;
  zgc.Dock := DockStyle.Right;
  zgc.Size := Size.Create( 400, 200 );

  spl := Splitter.Create();
  spl.Dock := System.Windows.Forms.DockStyle.Right;
  spl.Size := System.Drawing.Size.Create(4, 290);
  spl.MinExtra := 100;
  spl.MinSize := 100;

  Self.Panel1.Controls.Add(zgc);
  Self.Panel1.Controls.Add(spl);

  Base.CreateListView(Panel1, ListStats);
  ListStats.AddListColumn('-', 250);
  ListStats.AddListColumn('-', 150);

  Self.Panel1.Controls.SetChildIndex(ListStats, 0);
  Self.Panel1.Controls.SetChildIndex(spl, 2);
  Self.Panel1.Controls.SetChildIndex(zgc, 3);
  Self.Panel1.Controls.SetChildIndex(ToolBar1, 4);

  cbType.Items.Clear;
  for i := Low(TGenEngine.TStatMode) to High(TGenEngine.TStatMode) do
    cbType.Items.Add(LSList[Titles[i].Title]);

  SetLang();
end;

procedure TfmStats.SetLang();
begin
  Text := LSList[LSID_MIStats];
end;

procedure TfmStats.PrepareArray(gPane: GraphPane; aStyle: TChartStyle; aExcludeUnknowns: Boolean = True);
var
  s: string;
  i, val, lab: Integer;
  ppList: PointPairList;
begin
  gPane.Title.Text := 'ChartTitle';
  gPane.XAxis.Title.Text := 'ChartXTitle';
  gPane.YAxis.Title.Text := 'ChartYTitle';

  ppList := PointPairList.Create();
  for i := 0 to ListStats.Items.Count - 1 do begin
    s := ListStats.Items[i].Text;
    if (s = '?') then lab := 0 else lab := Int32.Parse(s);
    if (lab = 0) and (aExcludeUnknowns) then Continue;
    val := Int32.Parse(ListStats.Items[i].SubItems[1].Text);
    ppList.Add( lab, val );
  end;
  ppList.Sort();

  case aStyle of
    GKStats.csBar: gPane.AddBar('-', ppList, Color.Green);
    GKStats.csPoint: gPane.AddCurve('-', ppList, Color.Green, SymbolType.Diamond);
  end;
end;

procedure TfmStats.CalcStats(aTree: TGEDCOMTree; aMode: TGenEngine.TStatMode);
var
  gPane: GraphPane;
  vals: System.Collections.Hashtable;
  item: ListViewItem;
  Enumerator: IEnumerator;
  Entry: DictionaryEntry;
begin
  ListStats.Columns[0].Text := LSList[Titles[aMode].Cap];
  ListStats.Columns[1].Text := LSList[LSID_Value];

  ListStats.BeginUpdate;
  ListStats.Items.Clear;
  vals := System.Collections.Hashtable.Create;
  try
    Base.Engine.GetSpecStats(aMode, vals);

    Enumerator := vals.GetEnumerator();
    while Enumerator.MoveNext do begin
      Entry := DictionaryEntry(Enumerator.Current);

      item := ListStats.Items.Add(System.String(Entry.Key));
      item.SubItems.Add(Entry.Value.ToString());
    end;
  finally
    vals.Free;
    ListStats.EndUpdate;
  end;

  ///

  gPane := zgc.GraphPane;
  try
    gPane.CurveList.Clear;

    ChartTitle := LSList[Titles[aMode].Title];
    case aMode of
      smAge: begin
        ChartXTitle := LSList[LSID_Age];
        ChartYTitle := LSList[LSID_People];
        PrepareArray(gPane, csPoint);
      end;

      smLifeExpectancy: begin
        ChartXTitle := LSList[LSID_LifeExpectancy];
        ChartYTitle := LSList[LSID_People];
        PrepareArray(gPane, csPoint);
      end;

      smBirthYears, smBirthTenYears, smDeathYears, smDeathTenYears: begin
        case aMode of
          smBirthYears, smDeathYears: ChartXTitle := LSList[LSID_Years];
          smBirthTenYears, smDeathTenYears: ChartXTitle := LSList[LSID_Decennial];
        end;

        case aMode of
          smBirthYears, smBirthTenYears: ChartYTitle := LSList[LSID_HowBirthes];
          smDeathYears, smDeathTenYears: ChartYTitle := LSList[LSID_HowDeads];
        end;

        PrepareArray(gPane, csPoint);
      end;

      smChildsDistribution: begin
        ChartXTitle := LSList[LSID_Childs];
        ChartYTitle := LSList[LSID_Parents];
        PrepareArray(gPane, csBar);
      end;
    end;
  finally
    zgc.AxisChange();
  end;
end;

procedure TfmStats.cbType_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
begin
  CalcStats(Base.Tree, TGenEngine.TStatMode(cbType.SelectedIndex));
end;

procedure TfmStats.TfmStats_Load(sender: System.Object; e: System.EventArgs);

  function GetPercent(aDividend, aDivisor: Integer): string;
  var
    value: Double;
  begin
    if (aDivisor = 0)
    then value := 0
    else value := aDividend / aDivisor * 100;

    Result := System.String.Format(' ({0:0.00}%)', [value]);
  end;

var
  item: ListViewItem;
  stats: TGenEngine.TCommonStats;
begin
  Base.Engine.GetCommonStats(stats);

  ListCommon.Items.Clear;
  with stats do begin
    item := ListCommon.Items.Add(LSList[LSID_People]);
    item.SubItems.Add(persons.ToString());
    item.SubItems.Add(persons_m.ToString() + GetPercent(persons_m, persons));
    item.SubItems.Add(persons_f.ToString() + GetPercent(persons_f, persons));

    item := ListCommon.Items.Add(LSList[LSID_Living]);
    item.SubItems.Add(lives.ToString());
    item.SubItems.Add(lives_m.ToString());
    item.SubItems.Add(lives_f.ToString());

    item := ListCommon.Items.Add(LSList[LSID_Deads]);
    item.SubItems.Add(Int32(persons - lives).ToString());
    item.SubItems.Add(Int32(persons_m - lives_m).ToString());
    item.SubItems.Add(Int32(persons_f - lives_f).ToString());

    item := ListCommon.Items.Add(LSList[LSID_AvgAge]);
    item.SubItems.Add(Round(TGKUtils.SafeDiv(age, age_cnt)).ToString());
    item.SubItems.Add(Round(TGKUtils.SafeDiv(age_m, age_m_cnt)).ToString());
    item.SubItems.Add(Round(TGKUtils.SafeDiv(age_f, age_f_cnt)).ToString());

    item := ListCommon.Items.Add(LSList[LSID_AvgLife]);
    item.SubItems.Add(Round(TGKUtils.SafeDiv(life, life_cnt)).ToString());
    item.SubItems.Add(Round(TGKUtils.SafeDiv(life_m, life_m_cnt)).ToString());
    item.SubItems.Add(Round(TGKUtils.SafeDiv(life_f, life_f_cnt)).ToString());

    item := ListCommon.Items.Add(LSList[LSID_AvgChilds]);
    item.SubItems.Add(System.String.Format('{0:0.00}', [TGKUtils.SafeDiv(childs, childs_cnt)]));
    item.SubItems.Add(System.String.Format('{0:0.00}', [TGKUtils.SafeDiv(childs_m, childs_m_cnt)]));
    item.SubItems.Add(System.String.Format('{0:0.00}', [TGKUtils.SafeDiv(childs_f, childs_f_cnt)]));

    item := ListCommon.Items.Add(LSList[LSID_AvgBorn]);
    item.SubItems.Add(System.String.Format('{0:0.00}', [TGKUtils.SafeDiv(fba, fba_cnt)]));
    item.SubItems.Add(System.String.Format('{0:0.00}', [TGKUtils.SafeDiv(fba_m, fba_m_cnt)]));
    item.SubItems.Add(System.String.Format('{0:0.00}', [TGKUtils.SafeDiv(fba_f, fba_f_cnt)]));

    item := ListCommon.Items.Add(LSList[LSID_AvgMarriagesCount]);
    item.SubItems.Add(System.String.Format('{0:0.00}', [TGKUtils.SafeDiv(marr, marr_cnt)]));
    item.SubItems.Add(System.String.Format('{0:0.00}', [TGKUtils.SafeDiv(marr_m, marr_m_cnt)]));
    item.SubItems.Add(System.String.Format('{0:0.00}', [TGKUtils.SafeDiv(marr_f, marr_f_cnt)]));

    item := ListCommon.Items.Add(LSList[LSID_AvgMarriagesAge]);
    item.SubItems.Add(System.String.Format('{0:0.00}', [TGKUtils.SafeDiv(mage, mage_cnt)]));
    item.SubItems.Add(System.String.Format('{0:0.00}', [TGKUtils.SafeDiv(mage_m, mage_m_cnt)]));
    item.SubItems.Add(System.String.Format('{0:0.00}', [TGKUtils.SafeDiv(mage_f, mage_f_cnt)]));
  end;
end;

procedure TfmStats.TfmStats_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
begin
  if (e.KeyCode = Keys.Escape) then Close;
end;

end.
