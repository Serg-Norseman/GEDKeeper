unit GKStats; {prepare:fin}

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  GedCom551, StdCtrls, ToolWin, ExtCtrls, GKCtrls, GKBase, GKLists, GKEngine
  {$IFNDEF DELPHI_NET}, xygraph {$ENDIF};

const
  Titles: array [TStatMode] of record
    Title, Cap, Val: string;
  end = (
    (Title: 'Количество предков'; Cap: 'Человек'; Val: 'Предков'),
    (Title: 'Количество потомков'; Cap: 'Человек'; Val: 'Потомков'),
    (Title: 'Количество поколений потомков'; Cap: 'Человек'; Val: 'Поколений'),
    (Title: 'Фамилии'; Cap: 'Фамилия'; Val: 'Количество'),
    (Title: 'Имена'; Cap: 'Имя'; Val: 'Количество'),
    (Title: 'Отчества'; Cap: 'Отчество'; Val: 'Количество'),
    (Title: 'Возраст'; Cap: 'Возраст'; Val: 'Количество'),
    (Title: 'Продолжительность жизни'; Cap: 'Возраст'; Val: 'Количество'),
    (Title: 'Годы рождения'; Cap: 'Год рождения'; Val: 'Количество'),
    (Title: 'Годы рождения (десятилетиями)'; Cap: 'Годы рождения'; Val: 'Количество'),
    (Title: 'Годы смерти'; Cap: 'Год смерти'; Val: 'Количество'),
    (Title: 'Годы смерти (десятилетиями)'; Cap: 'Годы смерти'; Val: 'Количество'),
    (Title: 'Количество детей'; Cap: 'Имя'; Val: 'Количество'),
    (Title: 'Распределение количества детей'; Cap: 'Количество детей'; Val: 'Количество'),
    (Title: 'Место рождения'; Cap: 'Место рождения'; Val: 'Количество'),
    (Title: 'Место смерти'; Cap: 'Место смерти'; Val: 'Количество'),
    (Title: 'Местожительство'; Cap: 'Местожительство'; Val: 'Количество'),
    (Title: 'Занятия'; Cap: 'Занятие'; Val: 'Количество'),

    (Title: 'Вероисповедание'; Cap: 'Вероисповедание'; Val: 'Количество'),
    (Title: 'Национальность'; Cap: 'Национальность'; Val: 'Количество'),
    (Title: 'Образование'; Cap: 'Образование'; Val: 'Количество'),
    (Title: 'Социальное положение'; Cap: 'Социальное положение'; Val: 'Количество'),

    (Title: 'Возраст рождения первенца'; Cap: 'Имя'; Val: 'Возраст'),
    (Title: 'Количество браков'; Cap: 'Имя'; Val: 'Браков'),
    (Title: 'Возраст вступления в брак'; Cap: 'Имя'; Val: 'Возраст'),
    (Title: 'Разница возрастов супругов'; Cap: 'Семья'; Val: 'Разница'),

    (Title: 'Хобби'; Cap: 'Хобби'; Val: 'Количество'),
    (Title: 'Награда'; Cap: 'Награда'; Val: 'Количество'),

    (Title: 'Военная служба'; Cap: 'Военная служба'; Val: 'Количество'),
    (Title: 'Призван в ВС'; Cap: 'Призван в ВС'; Val: 'Количество'),
    (Title: 'Уволен из ВС'; Cap: 'Уволен из ВС'; Val: 'Количество'),
    (Title: 'Звание в ВС'; Cap: 'Звание в ВС'; Val: 'Количество')
  );

type
  TfmStats = class(TForm)
    GroupBox1: TGroupBox;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    cbType: TComboBox;
    ListCommon: TListView;
    procedure cbTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    ListStats: TGKListView;
    ChartStats: TVGPaintBox;

    {$IFNDEF DELPHI_NET}
    ChartData: TDataType;
    ChartXMax, ChartYMax: Single;
    ChartXMin, ChartYMin: Single;
    ChartTitle, ChartXTitle, ChartYTitle: string;
    ChartEmpty: Boolean;
    {$ENDIF}

    procedure CalcStats(aTree: TGEDCOMTree; aMode: TStatMode);
    function  GetBase(): TfmBase;

    procedure PrepareChart(aMode: TStatMode);

    procedure ChartPaint(Sender: TObject);
    procedure ChartMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ChartMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ChartMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    property Base: TfmBase read GetBase;
  end;

implementation

uses Dialogs, GKMain, GKUtils;

{$R *.dfm}

{ TfmStats }

procedure TfmStats.FormCreate(Sender: TObject);
var
  i: TStatMode;
begin
  ChartStats := TVGPaintBox.Create(Self);
  with ChartStats do begin
    Parent := Panel1;
    Width := 300;
    Align := alRight;
    OnMouseDown := ChartMouseDown;
    OnMouseMove := ChartMouseMove;
    OnMouseUp := ChartMouseUp;
    OnPaint := ChartPaint;
  end;

  with TSplitter.Create(Self) do begin
    Parent := Panel1;
    Align := alRight;
    Width := 3;
  end;

  Base.CreateListView(Self, Panel1, ListStats);
  AddListColumn(ListStats, '-', 250);
  AddListColumn(ListStats, '-', 150);

  //ChartStats.Visible := False;

  cbType.Clear;
  for i := Low(TStatMode) to High(TStatMode) do
    cbType.Items.Add(Titles[i].Title);

  cbType.DropDownCount := Ord(High(TStatMode)) + 1;
end;

procedure TfmStats.CalcStats(aTree: TGEDCOMTree; aMode: TStatMode);
var
  i: Integer;
  vals: TStringList;
  item: TListItem;
begin
  ListStats.Columns.Items[0].Caption := Titles[aMode].Cap;
  ListStats.Columns.Items[1].Caption := Titles[aMode].Val;

  ListStats.Items.BeginUpdate;
  ListStats.Clear;
  vals := TStringList.Create;
  try
    Base.Engine.GetSpecStats(aMode, vals);

    for i := 0 to vals.Count - 1 do begin
      item := ListStats.Items.Add();
      item.Caption := vals[i];
      item.SubItems.Add(IntToStr(Integer(vals.Objects[i])));
    end;
  finally
    vals.Free;
    ListStats.Items.EndUpdate;
  end;

  PrepareChart(aMode);
end;

procedure TfmStats.cbTypeChange(Sender: TObject);
begin
  CalcStats(Base.Tree, TStatMode(cbType.ItemIndex));
end;

procedure TfmStats.FormShow(Sender: TObject);

  function GetPercent(aDividend, aDivisor: Integer): string;
  var
    value: Double;
  begin
    if (aDivisor = 0)
    then value := 0
    else value := aDividend / aDivisor * 100;

    Result := ' (' + Format('%.2n', [value]) + '%)';
  end;

var
  item: TListItem;
  stats: TCommonStats;
begin
  Base.Engine.GetCommonStats(stats);

  ListCommon.Clear;
  with stats do begin
    item := ListCommon.Items.Add();
    item.Caption := 'Персон';
    item.SubItems.Add(IntToStr(persons));
    item.SubItems.Add(IntToStr(persons_m) + GetPercent(persons_m, persons));
    item.SubItems.Add(IntToStr(persons_f) + GetPercent(persons_f, persons));

    item := ListCommon.Items.Add();
    item.Caption := 'Живущие';
    item.SubItems.Add(IntToStr(lives));
    item.SubItems.Add(IntToStr(lives_m));
    item.SubItems.Add(IntToStr(lives_f));

    item := ListCommon.Items.Add();
    item.Caption := 'Умершие';
    item.SubItems.Add(IntToStr(persons - lives));
    item.SubItems.Add(IntToStr(persons_m - lives_m));
    item.SubItems.Add(IntToStr(persons_f - lives_f));

    item := ListCommon.Items.Add();
    item.Caption := 'Средний возраст';
    item.SubItems.Add(IntToStr(Round(SafeDiv(age, age_cnt))));
    item.SubItems.Add(IntToStr(Round(SafeDiv(age_m, age_m_cnt))));
    item.SubItems.Add(IntToStr(Round(SafeDiv(age_f, age_f_cnt))));

    item := ListCommon.Items.Add();
    item.Caption := 'Средняя продолжительность жизни';
    item.SubItems.Add(IntToStr(Round(SafeDiv(life, life_cnt))));
    item.SubItems.Add(IntToStr(Round(SafeDiv(life_m, life_m_cnt))));
    item.SubItems.Add(IntToStr(Round(SafeDiv(life_f, life_f_cnt))));

    item := ListCommon.Items.Add();
    item.Caption := 'Среднее число детей';
    item.SubItems.Add(Format('%.2n', [SafeDiv(childs, childs_cnt)]));
    item.SubItems.Add(Format('%.2n', [SafeDiv(childs_m, childs_m_cnt)]));
    item.SubItems.Add(Format('%.2n', [SafeDiv(childs_f, childs_f_cnt)]));

    item := ListCommon.Items.Add();
    item.Caption := 'Средний возраст рождения первенца';
    item.SubItems.Add(Format('%.2n', [SafeDiv(fba, fba_cnt)]));
    item.SubItems.Add(Format('%.2n', [SafeDiv(fba_m, fba_m_cnt)]));
    item.SubItems.Add(Format('%.2n', [SafeDiv(fba_f, fba_f_cnt)]));

    item := ListCommon.Items.Add();
    item.Caption := 'Среднее количество браков';
    item.SubItems.Add(Format('%.2n', [SafeDiv(marr, marr_cnt)]));
    item.SubItems.Add(Format('%.2n', [SafeDiv(marr_m, marr_m_cnt)]));
    item.SubItems.Add(Format('%.2n', [SafeDiv(marr_f, marr_f_cnt)]));

    item := ListCommon.Items.Add();
    item.Caption := 'Средний возраст заключения брака';
    item.SubItems.Add(Format('%.2n', [SafeDiv(mage, mage_cnt)]));
    item.SubItems.Add(Format('%.2n', [SafeDiv(mage_m, mage_m_cnt)]));
    item.SubItems.Add(Format('%.2n', [SafeDiv(mage_f, mage_f_cnt)]));
  end;
end;

procedure TfmStats.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then Close;
end;

procedure TfmStats.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

function TfmStats.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmStats.ChartMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  {$IFNDEF DELPHI_NET}
  xymousedown(Button, Shift, X, Y);
  {$ENDIF}
end;

procedure TfmStats.ChartMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  {$IFNDEF DELPHI_NET}
  xymousemove(Shift, X, Y);
  {$ENDIF}
end;

procedure TfmStats.ChartMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  {$IFNDEF DELPHI_NET}
  xymouseup(Button, Shift, X, Y);
  {$ENDIF}
end;

procedure TfmStats.ChartPaint(Sender: TObject);
begin
  {$IFNDEF DELPHI_NET}
  xycleargraph(ChartStats, clWhite, clBlack, 8/8);
  if not(ChartEmpty) then begin
    xystartgraph(0, 100, 0, 100, 40, 40, 70, 40, clipon);
    xyxaxis(clBlack, ChartXMin, ChartXMax, 0, 0, ChartXTitle, False, False, False);
    xyyaxis(clGreen, ChartYMin, ChartYMax, 0, 0, ChartYTitle, 1, False, False, False);
    xysymbol(2, 4, 2);
    xyplotarray(ChartData, 0, 2);
    xytitle(clMaroon, ChartTitle);
    xyinitruler(clMaroon, 8, 60 - xycharheight * 2, 1, 0);
  end;
  {$ENDIF}
end;

procedure TfmStats.PrepareChart(aMode: TStatMode);
{$IFNDEF DELPHI_NET}
type
  TChartStyle = (csBar, csSimplePoint, csCircle);
const
  Styles: array [TChartStyle] of array [1..7] of Integer = (
    (4, clTeal, 1, 4, 2, 2, 2),
    (1, clBlack, 2, 4, 0, 0, 0),
    (0, clGreen, 1, 0, 1, 4, 1)
  );

  procedure PrepareArray(aStyle: TChartStyle; aExcludeUnknowns: Boolean = True);
  var
    i, val, lab, count, idx: Integer;
    delta: Single;
    s: string;
  begin
    ChartXMax := -MaxInt;
    ChartYMax := -MaxInt;

    ChartXMin := MaxInt;
    ChartYMin := MaxInt;

    count := 0;
    for i := 0 to ListStats.Items.Count - 1 do begin
      s := ListStats.Items[i].Caption;
      if (s = '?') then lab := 0 else lab := StrToInt(s);
      if not((lab = 0) and (aExcludeUnknowns)) then Inc(count);
    end;

    xysetdataarray(ChartData, count + 7, 1);

    idx := 0;
    for i := 0 to ListStats.Items.Count - 1 do begin
      s := ListStats.Items[i].Caption;
      if (s = '?') then lab := 0 else lab := StrToInt(s);
      if (lab = 0) and (aExcludeUnknowns) then Continue;

      val := StrToInt(ListStats.Items[i].SubItems[0]);

      ChartData[idx + 8, 0] := lab;
      ChartData[idx + 8, 1] := val;

      if (ChartXMax < lab) then ChartXMax := lab;
      if (ChartYMax < val) then ChartYMax := val;

      if (ChartXMin > lab) then ChartXMin := lab;
      if (ChartYMin > val) then ChartYMin := val;

      Inc(idx);
    end;

    for i := 1 to 7 do ChartData[i, 1] := Styles[aStyle, i];

    ChartData[0, 0] := 1;
    ChartData[0, 1] := 1;

    delta := Abs(ChartXMax - ChartXMin);
    if (delta < 0.075) then delta := 0.075
    else
    if (delta < 0.5) then delta := 0.5
    else
    if (delta < 1) then delta := 1;
    delta := delta * 0.025;

    ChartXMin := ChartXMin - delta;
    ChartXMax := ChartXMax + delta;

    delta := Abs(ChartYMax - ChartYMin);
    if (delta < 0.075) then delta := 0.075
    else
    if (delta < 0.5) then delta := 0.5
    else
    if (delta < 1) then delta := 1;
    delta := delta * 0.025;

    ChartYMin := ChartYMin - delta;
    ChartYMax := ChartYMax + delta;
  end;
{$ENDIF}
begin
  {$IFNDEF DELPHI_NET}
  ChartEmpty := True;
  ChartTitle := Titles[aMode].Title;

  case aMode of
    smAncestors: ;
    smDescendants: ;
    smFamilies: ;
    smNames: ;
    smPatronymics: ;

    smAge: begin
      ChartXTitle := 'Возраст';
      ChartYTitle := 'Люди';
      ChartEmpty := False;
      PrepareArray(csSimplePoint);
    end;

    smLifeExpectancy: begin
      ChartXTitle := 'Продолжительность';
      ChartYTitle := 'Люди';
      ChartEmpty := False;
      PrepareArray(csSimplePoint);
    end;

    smBirthYears, smBirthTenYears, smDeathYears, smDeathTenYears: begin
      case aMode of
        smBirthYears, smDeathYears: ChartXTitle := 'Годы';
        smBirthTenYears, smDeathTenYears: ChartXTitle := 'Десятилетия';
      end;

      case aMode of
        smBirthYears, smBirthTenYears: ChartXTitle := 'Родилось';
        smDeathYears, smDeathTenYears: ChartXTitle := 'Умерло';
      end;

      ChartEmpty := False;
      PrepareArray(csCircle);
    end;

    smChildsCount: ;

    smChildsDistribution: begin
      ChartXTitle := 'Дети';
      ChartYTitle := 'Родители';
      ChartEmpty := False;
      PrepareArray(csBar);
    end;

    smBirthPlaces: ;
    smDeathPlaces: ;
    smResidences: ;
    smOccupation: ;
    smReligious: ;
    smNational: ;
    smEducation: ;
    smCaste: ;
    smFirstbornAge: ;
    smMarriages: ;
    smMarriageAge: ;
    smSpousesDiff: ;
    smHobby: ;
    smAward: ;
    smMili: ;
    smMiliInd: ;
    smMiliDis: ;
    smMiliRank: ;
  end;

  ChartStats.Repaint;
  {$ENDIF}
end;

end.
