unit GKStats;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, ComCtrls,
  GedCom551, StdCtrls, ToolWin, ExtCtrls, bsCtrls;

type
  TStatMode = (
    smAncestors, smDescendants,
    smFamilies, smNames, smPatronymics,
    smAge, smLifeExpectancy,
    smBirthYears, smBirthTenYears, smDeathYears, smDeathTenYears,
    smChildsCount, smChildsDistribution,
    smBirthPlaces, smDeathPlaces, smResidences, smOccupation,
    smReligious, smNational, smEducation,
    smFirstbornAge, smMarriages, smMarriageAge, smSpousesDiff,

    smHobby, smAward, smMili, smMiliInd, smMiliDis, smMiliRank);

const
  Titles: array [TStatMode] of record
    Title, Cap, Val: string;
  end = (
    (Title: 'Количество предков'; Cap: 'Человек'; Val: 'Предков'),
    (Title: 'Количество потомков'; Cap: 'Человек'; Val: 'Потомков'),
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
    ListStats: TBSListView;
    ListCommon: TBSListView;
    procedure cbTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    procedure AddItem(aTitle, aVal: string);
    procedure CalcStats(aTree: TGEDCOMTree; aMode: TStatMode);
    procedure InitTable(Col1, Col2: string);
  public
  end;

var
  fmStats: TfmStats;

implementation

uses GKCommon, GKMain;

{$R *.dfm}

function SafeDiv(aDividend, aDivisor: Double): Double;
begin
  if (aDivisor = 0.0)
  then Result := 0.0
  else Result := aDividend / aDivisor;
end;

{ TfmStats }

procedure TfmStats.InitTable(Col1, Col2: string);
begin
  ListStats.Columns.Items[0].Caption := Col1;
  ListStats.Columns.Items[1].Caption := Col2;
  ListStats.Clear;
end;

procedure TfmStats.AddItem(aTitle, aVal: string);
var
  item: TListItem;
begin
  item := ListStats.Items.Add();
  item.Caption := aTitle;
  item.SubItems.Add(aVal);
end;

procedure TfmStats.FormCreate(Sender: TObject);
var
  i: TStatMode;
begin
  cbType.Clear;
  for i := Low(TStatMode) to High(TStatMode) do
    cbType.Items.Add(Titles[i].Title);

  cbType.DropDownCount := Ord(High(TStatMode)) + 1;
end;

procedure TfmStats.CalcStats(aTree: TGEDCOMTree; aMode: TStatMode);
var
  vals, buffer: TStringList;

  function GetAncestorsCount(aPerson: TGEDCOMIndividualRecord): Integer;
  var
    family: TGEDCOMFamilyRecord;
    anc: TGEDCOMIndividualRecord;
    xref: string;
    idx: Integer;
  begin
    Result := 0;
    if (aPerson = nil) then Exit;

    xref := aPerson.XRef;

    idx := buffer.IndexOf(xref);
    if (idx >= 0) then begin
      Result := Integer(buffer.Objects[idx]);
    end else begin
      Result := 1;

      if (aPerson.ChildToFamilyLinksCount > 0) then begin
        family := aPerson.ChildToFamilyLinks[0].Family;

        anc := TGEDCOMIndividualRecord(family.Husband.Value);
        Result := Result + GetAncestorsCount(anc);

        anc := TGEDCOMIndividualRecord(family.Wife.Value);
        Result := Result + GetAncestorsCount(anc);
      end;

      buffer.AddObject(xref, TObject(Result));
    end;
  end;

  function GetDescendantsCount(aPerson: TGEDCOMIndividualRecord): Integer;
  var
    family: TGEDCOMFamilyRecord;
    iChild: TGEDCOMIndividualRecord;
    i, k: Integer;
    xref: string;
    idx: Integer;
  begin
    Result := 0;
    if (aPerson = nil) then Exit;

    xref := aPerson.XRef;

    idx := buffer.IndexOf(xref);
    if (idx >= 0) then begin
      Result := Integer(buffer.Objects[idx]);
    end else begin
      Result := 1;

      for i := 0 to aPerson.SpouseToFamilyLinksCount - 1 do begin
        family := aPerson.SpouseToFamilyLinks[i].Family;

        for k := 0 to family.ChildrenCount - 1 do begin
          iChild := TGEDCOMIndividualRecord(family.Children[k].Value);
          Result := Result + GetDescendantsCount(iChild);
        end;
      end;

      buffer.AddObject(xref, TObject(Result));
    end;
  end;

var
  i, k, idx, year: Integer;
  iRec: TGEDCOMIndividualRecord;
  event: TGEDCOMIndividualEvent;
  fRec: TGEDCOMFamilyRecord;
  V, fam, nam, pat: string;
begin
  InitTable(Titles[aMode].Cap, Titles[aMode].Val);

  buffer := TStringList.Create;
  vals := TStringList.Create;
  try
    for i := 0 to aTree.Count - 1 do begin
      if (aTree.Records[i] is TGEDCOMIndividualRecord) and (aMode <> smSpousesDiff) then begin
        iRec := aTree.Records[i] as TGEDCOMIndividualRecord;

        case aMode of
          smAncestors: begin
            AddItem(GetNameStr(iRec), IntToStr(GetAncestorsCount(iRec) - 1));
          end;

          smDescendants: begin
            AddItem(GetNameStr(iRec), IntToStr(GetDescendantsCount(iRec) - 1));
          end;

          smChildsCount: begin
            AddItem(GetNameStr(iRec), IntToStr(GetChildsCount(iRec)));
          end;

          smFirstbornAge: begin
            AddItem(GetNameStr(iRec), GetFirstbornAge(iRec));
          end;

          smMarriages: begin
            AddItem(GetNameStr(iRec), IntToStr(GetMarriagesCount(iRec)));
          end;

          smMarriageAge: begin
            AddItem(GetNameStr(iRec), GetMarriageAge(iRec));
          end;

          else begin
            case aMode of
              smNames, smFamilies, smPatronymics: begin
                GetNameParts(iRec, fam, nam, pat);

                case aMode of
                  smFamilies: V := PrepareRusFamily(fam, (iRec.Sex = svFemale));
                  smNames: V := nam;
                  smPatronymics: V := pat;
                end;
              end;

              smAge: V := GetAge(iRec);

              smLifeExpectancy: V := GetLifeExpectancy(iRec);

              smResidences: V := GetAttributeValue(iRec, 'RESI');

              smOccupation: V := GetAttributeValue(iRec, 'OCCU');

              smReligious: V := GetAttributeValue(iRec, 'RELI');

              smNational: V := GetAttributeValue(iRec, 'NATI');

              smEducation: V := GetAttributeValue(iRec, 'EDUC');

              smChildsDistribution: V := IntToStr(GetChildsCount(iRec));

              smBirthYears..smDeathTenYears, smBirthPlaces, smDeathPlaces: begin
                V := '?';
                for k := 0 to iRec.IndividualEventsCount - 1 do begin
                  event := iRec.IndividualEvents[k];
                  year := TGEDCOMDate(event.Detail.Date.Value).Year;

                  if (event.Name = 'BIRT') then begin
                    if (aMode = smBirthYears)
                    then V := IntToStr(year)
                    else
                    if (aMode = smBirthTenYears)
                    then V := IntToStr((year div 10) * 10)
                    else
                    if (aMode = smBirthPlaces)
                    then V := event.Detail.Place;
                  end
                  else
                  if (event.Name = 'DEAT') then begin
                    if (aMode = smDeathYears)
                    then V := IntToStr(year)
                    else
                    if (aMode = smDeathTenYears)
                    then V := IntToStr((year div 10) * 10)
                    else
                    if (aMode = smDeathPlaces)
                    then V := event.Detail.Place;
                  end;
                end;
              end;

              smHobby: begin
                V := GetAttributeValue(iRec, '_HOBBY');
              end;

              smAward: begin
                V := GetAttributeValue(iRec, '_AWARD');
              end;

              smMili: begin
                V := GetAttributeValue(iRec, '_MILI');
              end;

              smMiliInd: begin
                V := GetAttributeValue(iRec, '_MILI_IND');
              end;

              smMiliDis: begin
                V := GetAttributeValue(iRec, '_MILI_DIS');
              end;

              smMiliRank: begin
                V := GetAttributeValue(iRec, '_MILI_RANK');
              end;
            end;

            if (V = '-1') or (V = '') or (V = '0') then V := '?';

            idx := vals.IndexOf(V);
            if (idx < 0)
            then vals.AddObject(V, TObject(1))
            else vals.Objects[idx] := TObject(Integer(vals.Objects[idx]) + 1);
          end;
        end;
      end
      else
      if (aTree.Records[i] is TGEDCOMFamilyRecord) and (aMode = smSpousesDiff) then begin
        fRec := aTree.Records[i] as TGEDCOMFamilyRecord;
        AddItem(GetFamilyStr(fRec), GetSpousesDiff(fRec));
      end;
    end;

    for i := 0 to vals.Count - 1 do begin
      AddItem(vals[i], IntToStr(Integer(vals.Objects[i])));
    end;
  finally
    vals.Destroy;
    buffer.Destroy;
  end;
end;

procedure TfmStats.cbTypeChange(Sender: TObject);
begin
  CalcStats(fmGEDKeeper.FTree, TStatMode(cbType.ItemIndex));
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
  GetCommonStats(fmGEDKeeper.FTree, stats);

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

end.
