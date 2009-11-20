unit GKDiagnosis;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, GKCtrls, GedCom551;

type
  TfmDiagnosis = class(TForm)
    ListErrors: TBSListView;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    procedure AddDiag(aObj, aDiag: string);
  public
    procedure Check(aTree: TGEDCOMTree; aCorrection: Boolean = False);
    procedure Compare(aMainTree: TGEDCOMTree; aFileName: string);
  end;

implementation

uses GKCommon, GKMain;

{$R *.dfm}

{ TfmDataCheck }

procedure TfmDiagnosis.AddDiag(aObj, aDiag: string);
var
  item: TListItem;
begin
  item := ListErrors.Items.Add();
  item.Caption := aObj;
  item.SubItems.Add(aDiag);
end;

procedure TfmDiagnosis.Check(aTree: TGEDCOMTree; aCorrection: Boolean = False);

  {procedure CheckAttributes(ind: TGEDCOMIndividualRecord);
  var
    i: Integer;
    attr: TGEDCOMIndividualAttribute;
  begin
    for i := 0 to ind.IndividualAttributesCount - 1 do begin
      attr := ind.IndividualAttributes[i];

      if (attr.StringValue = '') and (attr.Detail.Place <> '') then begin
        attr.StringValue := attr.Detail.Place;
        attr.Detail.Place := '';
      end;
    end;
  end;}

  procedure CheckPerson(ind: TGEDCOMIndividualRecord);
  var
    k: Integer;
    fam: TGEDCOMFamilyRecord;
  begin
    {CheckAttributes(ind);}

    // проверки
    if (ind.ChildToFamilyLinksCount = 0) and (ind.SpouseToFamilyLinksCount = 0)
    then AddDiag(ind.XRef, 'Не указаны родители и супруги');

    for k := 0 to ind.ChildToFamilyLinksCount - 1 do begin
      fam := ind.ChildToFamilyLinks[k].Family;

      if (fam = nil)
      then AddDiag(ind.XRef, 'Пустая ссылка на семью родителей '+IntToStr(k));
    end;

    for k := 0 to ind.SpouseToFamilyLinksCount - 1 do begin
      fam := ind.SpouseToFamilyLinks[k].Family;

      if (fam = nil)
      then AddDiag(ind.XRef, 'Пустая ссылка на брак '+IntToStr(k))
      else begin
        case ind.Sex of
          svMale: begin
          end;

          svFemale: begin
          end;
        end;
      end;
    end;
  end;

  procedure CheckFamily(fam: TGEDCOMFamilyRecord);
  var
    k: Integer;
    rel, husb, wife: TGEDCOMIndividualRecord;
  begin
    husb := TGEDCOMIndividualRecord(fam.Husband.Value);
    wife := TGEDCOMIndividualRecord(fam.Wife.Value);

    if ((husb = nil) and (wife = nil))
    then AddDiag(fam.XRef, 'Пустые указатели супругов')
    else begin
      if (husb = nil) then begin
        AddDiag(fam.XRef, 'Пустой указатель мужа');
      end;

      if (wife = nil) then begin
        AddDiag(fam.XRef, 'Пустой указатель жены');
      end;
    end;

    if (fam.ChildrenCount = 0)
    then AddDiag(fam.XRef, 'Нет детей');

    for k := 0 to fam.ChildrenCount - 1 do begin
      rel := TGEDCOMIndividualRecord(fam.Children[k].Value);

      if (rel = nil)
      then AddDiag(fam.XRef, 'Пустой указатель ребенка '+IntToStr(k));
    end;
  end;

  procedure CleanEmptyFamilies();
  var
    i, k: Integer;
    famRec: TGEDCOMFamilyRecord;
    h, w, child: TGEDCOMIndividualRecord;
  begin
    i := 0;
    while (i < aTree.Count) do begin
      if (aTree.Records[i] is TGEDCOMFamilyRecord) then begin
        famRec := aTree.Records[i] as TGEDCOMFamilyRecord;

        h := TGEDCOMIndividualRecord(famRec.Husband.Value);
        w := TGEDCOMIndividualRecord(famRec.Wife.Value);

        if (h = nil) and (w = nil) then begin
          if (famRec.ChildrenCount > 0) then begin
            for k := 0 to famRec.ChildrenCount - 1 do begin
              child := TGEDCOMIndividualRecord(famRec.Children[k].Value);
              child.DeleteChildToFamilyLink(famRec);
            end;
          end;

          AddDiag(famRec.XRef, 'Удалена семья без родителей');
          aTree.Delete(i);

          Continue;
        end;
      end;

      Inc(i);
    end;
  end;

var
  i, k: Integer;
  rec: TGEDCOMRecord;
begin
  for i := 0 to aTree.Count - 1 do begin
    for k := i + 1 to aTree.Count - 1 do begin
      if (aTree.Records[i].XRef = aTree.Records[k].XRef)
      then AddDiag(aTree.Records[i].XRef, 'Объект дублирован');
    end;
  end;

  for i := 0 to aTree.Count - 1 do begin
    rec := aTree.Records[i];

    // автоматическая коррекция
    rec.DeleteTag('_UPD');
    rec.DeleteTag('_UID');

    if (rec is TGEDCOMIndividualRecord)
    then CheckPerson(rec as TGEDCOMIndividualRecord)
    else
    if (rec is TGEDCOMFamilyRecord)
    then CheckFamily(rec as TGEDCOMFamilyRecord);
  end;

  if fmGEDKeeper.Options.CleanEmptyFamilies
  then CleanEmptyFamilies();
end;

procedure TfmDiagnosis.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then Close;
end;

procedure TfmDiagnosis.Compare(aMainTree: TGEDCOMTree; aFileName: string);
var
  tempTree: TGEDCOMTree;
  i, idx: Integer;
  iRec: TGEDCOMIndividualRecord;
  fams, names: TStringList;
  fam, nam, pat, tm: string;
begin
  tempTree := TGEDCOMTree.Create;
  tempTree.LoadFromFile(aFileName);
  fams := TStringList.Create;
  names := TStringList.Create;
  try
    AddDiag('', 'Поиск совпадений...');

    for i := 0 to aMainTree.Count - 1 do
      if (aMainTree.Records[i] is TGEDCOMIndividualRecord) then begin
        iRec := aMainTree.Records[i] as TGEDCOMIndividualRecord;
        names.AddObject(GetNameStr(iRec), nil);

        GetNameParts(iRec, fam, nam, pat);
        fams.AddObject(PrepareRusFamily(fam, (iRec.Sex = svFemale)), nil);
      end;

    for i := 0 to tempTree.Count - 1 do
      if (tempTree.Records[i] is TGEDCOMIndividualRecord) then begin
        iRec := tempTree.Records[i] as TGEDCOMIndividualRecord;
        tm := GetNameStr(iRec);

        idx := names.IndexOf(tm);
        if (idx >= 0)
        then names.Objects[idx] := TObject(1);

        GetNameParts(iRec, fam, nam, pat);
        tm := PrepareRusFamily(fam, (iRec.Sex = svFemale));

        idx := fams.IndexOf(tm);
        if (idx >= 0)
        then fams.Objects[idx] := TObject(1);
      end;

    for i := fams.Count - 1 downto 0 do
      if (fams.Objects[i] = nil) or (fams[i] = '?') then fams.Delete(i);

    for i := names.Count - 1 downto 0 do
      if (names.Objects[i] = nil) then names.Delete(i);

    AddDiag('', 'Схожие фамилии:');
    if (fams.Count <> 0) then begin
      for i := 0 to fams.Count - 1 do AddDiag('', '    ' + fams[i]);
    end else AddDiag('', '    нет.');

    AddDiag('', 'Схожие имена:');
    if (names.Count <> 0) then begin
      for i := 0 to names.Count - 1 do AddDiag('', '    ' + names[i]);
    end else AddDiag('', '    нет.');
  finally
    names.Destroy;
    fams.Destroy;
    tempTree.Destroy;
  end;
end;

end.
