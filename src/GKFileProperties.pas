unit GKFileProperties;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Buttons,
  GedCom551, ComCtrls, bsCtrls;

type
  TfmFileProperties = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    PageControl1: TPageControl;
    SheetAuthor: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EditName: TEdit;
    EditTel: TEdit;
    MemoAddress: TMemo;
    SheetDiagnostics: TTabSheet;
    ListDiags: TBSListView;
    procedure btnAcceptClick(Sender: TObject);
  private
    FTree: TGEDCOMTree;
    procedure SetTree(const Value: TGEDCOMTree);

    procedure Check(aTree: TGEDCOMTree; aCorrection: Boolean = False);
  public
    property Tree: TGEDCOMTree read FTree write SetTree;
  end;

implementation

uses GKMain;

{$R *.dfm}

procedure TfmFileProperties.SetTree(const Value: TGEDCOMTree);
var
  submitter: TGEDCOMSubmitterRecord;
begin
  FTree := Value;

  submitter := TGEDCOMSubmitterRecord(FTree.Header.Submitter.Value);
  if (submitter = nil) then begin
    submitter := TGEDCOMSubmitterRecord.Create(FTree, FTree);
    submitter.NewXRef;
    FTree.AddRecord(submitter);
    FTree.Header.SetTagStringValue('SUBM', '@'+submitter.XRef+'@');
  end;

  EditName.Text := submitter.Name.FullName;
  MemoAddress.Text := submitter.Address.Address.Text;
  EditTel.Text := submitter.Address.PhoneNumbers[0];

  Check(FTree);
end;

procedure TfmFileProperties.btnAcceptClick(Sender: TObject);
var
  submitter: TGEDCOMSubmitterRecord;
begin
  submitter := TGEDCOMSubmitterRecord(FTree.Header.Submitter.Value);
  submitter.Name.StringValue := EditName.Text;
  submitter.Address.Address := MemoAddress.Lines;
  submitter.Address.PhoneNumbers[0] := EditTel.Text;
  submitter.ChangeDate.ChangeDateTime := Now();

  fmGEDKeeper.Modified := True;
end;

procedure TfmFileProperties.Check(aTree: TGEDCOMTree; aCorrection: Boolean = False);

  procedure AddDiag(aObj, aDiag: string);
  var
    item: TListItem;
  begin
    item := ListDiags.Items.Add();
    item.Caption := aObj;
    item.SubItems.Add(aDiag);
  end;

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

  {procedure CheckName(ind: TGEDCOMIndividualRecord);
  var
    fam, nam, pat, np: string;
    tag: TGEDCOMTag;
    pn: TGEDCOMPersonalName;
  begin
    GetNameParts(ind, fam, nam, pat);

    pn := ind.PersonalNames[0];

    if (nam = '')
    then np := pat
    else
    if (pat = '')
    then np := nam
    else np := nam + ' ' + pat;

    tag := pn.FindTag('GIVN');
    if (tag <> nil) and (tag.StringValue = np) then pn.DeleteTag('GIVN');

    tag := pn.FindTag('SURN');
    if (tag <> nil) and (tag.StringValue = fam) then pn.DeleteTag('SURN');
  end;}

  {procedure CheckPlace(ind: TGEDCOMIndividualRecord);
  var
    pn: TGEDCOMPersonalName;
    ev: TGEDCOMIndividualEvent;
  begin
    pn := ind.PersonalNames[0];

    if (pn.Surname <> '') and (pn.Surname[1] in ['Ж']) then begin
      ev := GetIndividualEvent(ind, 'BIRT');
      if (ev.Detail.Place = '')
      then ev.Detail.Place := 'Екатеринбургский уезд, пос. Верхний Тагил';
    end;
  end;}

  procedure CheckPerson(ind: TGEDCOMIndividualRecord);
  var
    k: Integer;
    fam: TGEDCOMFamilyRecord;
  begin
    {CheckAttributes(ind);}
    {CheckName(ind);}
    {CheckPlace(ind);}

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
      then AddDiag(ind.XRef, 'Пустая ссылка на брак '+IntToStr(k));
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
          fmGEDKeeper.Modified := True;

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

    if (rec is TGEDCOMIndividualRecord)
    then CheckPerson(rec as TGEDCOMIndividualRecord)
    else
    if (rec is TGEDCOMFamilyRecord)
    then CheckFamily(rec as TGEDCOMFamilyRecord);
  end;

  if (aCorrection) and (fmGEDKeeper.Options.CleanEmptyFamilies)
  then CleanEmptyFamilies();
end;

end.
