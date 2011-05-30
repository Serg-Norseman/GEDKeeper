unit GKPersonScan; {prepare:fin; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Mask, Buttons, ExtCtrls, GKBase, ComCtrls, Grids, GKLangs;

type
  TfmPersonScan = class(TForm, ILocalization)
    btnParse: TBitBtn;
    btnClose: TBitBtn;
    PageControl1: TPageControl;
    tsSimpleInput: TTabSheet;
    tsSourceInput: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    btnMale: TSpeedButton;
    btnFemale: TSpeedButton;
    EditName: TEdit;
    MemoNote: TMemo;
    Panel1: TPanel;
    Label3: TLabel;
    Label5: TLabel;
    EditBirthDate: TMaskEdit;
    EditBirthPlace: TEdit;
    CheckBirth: TCheckBox;
    Panel2: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    CheckDeath: TCheckBox;
    EditDeathDate: TMaskEdit;
    EditDeathPlace: TEdit;
    Label4: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    cbSource: TComboBox;
    edPage: TEdit;
    edSourceYear: TMaskEdit;
    edPlace: TEdit;
    sgData: TStringGrid;
    cbPersonLink: TComboBox;
    rgSourceKind: TRadioGroup;
    gbMetrics: TGroupBox;
    Label11: TLabel;
    Label12: TLabel;
    edEventDate: TMaskEdit;
    cbEventType: TComboBox;
    procedure btnParseClick(Sender: TObject);
    procedure EditBirthDateChange(Sender: TObject);
    procedure EditDeathDateChange(Sender: TObject);
    procedure EditNameKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure sgDataSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure cbPersonLinkKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sgDataKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure cbPersonLinkChange(Sender: TObject);
    procedure rgSourceKindClick(Sender: TObject);
  private
    FSourcesList: TStringList;

    function CheckCell(ACol, ARow: Integer): Boolean;
    function GetBase: TfmBase;
    procedure InitGrid();
    procedure InitSimpleControls();
    procedure InitSourceControls();
    procedure ParseSimple();
    procedure ParseSource();
  public
    property Base: TfmBase read GetBase;

    procedure SetLang();
  end;

implementation

uses GKUtils, GKMain, GedCom551, GKEngine, GKSexCheck;

{$R *.dfm}

type
  TPersonLink = (
    plNone, plPerson, plFather, plMother, plGodparent, plSpouse, plChild);

const
  PersonLinks: array [TPersonLink] of LSID = (
    LSID_RK_Unk, LSID_PLPerson, LSID_Father, LSID_Mother,
    LSID_PLGodparent, LSID_Spouse, LSID_Child
  );

function GetLinkByName(const aName: string): TPersonLink;
var
  pl: TPersonLink;
begin
  for pl := plPerson to High(TPersonLink) do
    if (LSList[PersonLinks[pl]] = aName) then begin
      Result := pl;
      Exit;
    end;

  Result := plNone;
end;

{ TfmPersonScan }

procedure TfmPersonScan.FormCreate(Sender: TObject);
var
  pl: TPersonLink;
begin
  FSourcesList := TStringList.Create();

  // simpleparse init
  InitSimpleControls();

  // sourceparse init
  InitGrid();
  InitSourceControls();

  for pl := plPerson to High(TPersonLink) do cbPersonLink.Items.Add(LSList[PersonLinks[pl]]);

  SetLang();
end;

procedure TfmPersonScan.FormDestroy(Sender: TObject);
begin
  FSourcesList.Free;
end;

procedure TfmPersonScan.SetLang();
begin
  btnParse.Caption := LSList[LSID_DlgAppend];
  btnClose.Caption := LSList[LSID_DlgClose];

  Caption := LSList[LSID_MIStreamInput];

  tsSimpleInput.Caption := LSList[LSID_InputSimple];

  btnMale.Caption := LSList[LSID_SexM][1];
  btnFemale.Caption := LSList[LSID_SexF][1];
  Label1.Caption := LSList[LSID_FullName];
  CheckBirth.Caption := LSList[LSID_Birth];
  Label3.Caption := LSList[LSID_BirthDate];
  Label5.Caption := LSList[LSID_BirthPlace];
  CheckDeath.Caption := LSList[LSID_Death];
  Label6.Caption := LSList[LSID_DeathDate];
  Label7.Caption := LSList[LSID_DeathPlace];
  Label2.Caption := LSList[LSID_Note];

  tsSourceInput.Caption := LSList[LSID_InputSource];

  rgSourceKind.Caption := LSList[LSID_SourceKind];
  rgSourceKind.Items[0] := LSList[LSID_SK_Rev];
  rgSourceKind.Items[0] := LSList[LSID_SK_Met];
  Label4.Caption := LSList[LSID_Source];
  Label8.Caption := LSList[LSID_Page];
  Label9.Caption := LSList[LSID_Year];
  Label10.Caption := LSList[LSID_Settlement];
  gbMetrics.Caption := LSList[LSID_SK_Met];
  Label11.Caption := LSList[LSID_EventDate];
  Label12.Caption := LSList[LSID_EventType];
end;

procedure TfmPersonScan.InitSimpleControls();
begin
  EditName.Text := '';
  EditBirthDate.Text := '';
  EditBirthPlace.Text := '';
  CheckBirth.Checked := False;
  EditDeathDate.Text := '';
  EditDeathPlace.Text := '';
  CheckDeath.Checked := False;
  MemoNote.Text := '';
  btnMale.Down := True;
end;

procedure TfmPersonScan.InitSourceControls();
var
  col, row: Integer;
begin
  Base.Engine.GetSourcesList(FSourcesList);
  cbSource.Items.Assign(FSourcesList);
  cbSource.Text := '';
  cbSource.ItemIndex := -1;

  edPage.Text := '';
  edSourceYear.Text := '';
  edPlace.Text := '';

  edEventDate.Text := '';
  cbEventType.ItemIndex := -1;

  for row := 1 to sgData.RowCount - 1 do
    for col := 0 to sgData.ColCount - 1 do
      sgData.Cells[col, row] := '';
end;

procedure TfmPersonScan.InitGrid();
begin
  sgData.Cells[0, 0] := LSList[LSID_Join];
  sgData.Cells[1, 0] := LSList[LSID_Name];
  sgData.Cells[2, 0] := LSList[LSID_Patronymic];
  sgData.Cells[3, 0] := LSList[LSID_Surname];
  sgData.Cells[4, 0] := LSList[LSID_Age];
  sgData.Cells[5, 0] := LSList[LSID_Comment];

  sgData.DefaultRowHeight := sgData.Canvas.TextHeight('A') + 7;
  sgData.ColWidths[4] := 60;
  sgData.ColWidths[5] := 150;
end;

procedure TfmPersonScan.ParseSimple();
var
  iRec: TGEDCOMIndividualRecord;
  tokCount: Integer;
  nam, pat, fam, tmp: string;
  sx: TGEDCOMSex;
begin
  tmp := AnsiLowerCase(EditName.Text);
  tokCount := GetTokensCount(tmp, ' ');
  if (tokCount < 3) then begin
    MessageDlg(LSList[LSID_NameInvalid], mtError, [mbOk], 0);
    Exit;
  end;

  fam := GetToken(tmp, ' ', 1);
  nam := GetToken(tmp, ' ', 2);
  pat := GetToken(tmp, ' ', 3);

  fam[1] := AnsiUpperCase(fam)[1];
  nam[1] := AnsiUpperCase(nam)[1];
  pat[1] := AnsiUpperCase(pat)[1];

  sx := svNone;
  if btnMale.Down then sx := svMale
  else
  if btnFemale.Down then sx := svFemale;

  iRec := CreatePersonEx(Base.Tree, nam, pat, fam, sx, False);
  Base.ChangeRecord(iRec);

  if (CheckBirth.Checked)
  then CreateEventEx(Base.Tree, iRec, 'BIRT', StrToGEDCOMDate(EditBirthDate.Text), EditBirthPlace.Text);

  if (CheckDeath.Checked)
  then CreateEventEx(Base.Tree, iRec, 'DEAT', StrToGEDCOMDate(EditDeathDate.Text), EditDeathPlace.Text);

  if (MemoNote.Text <> '')
  then CreateNoteEx(Base.Tree, MemoNote.Lines, iRec);

  InitSimpleControls();
end;

procedure TfmPersonScan.ParseSource();

  function GetParentsFamily(iRec: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;
  begin
    if (iRec.ChildToFamilyLinksCount > 0)
    then Result := iRec.ChildToFamilyLinks[0].Family
    else begin
      Result := CreateFamilyEx(Base.Tree);
      Base.Engine.AddFamilyChild(Result, iRec);
    end;
  end;

  function GetMarriageFamily(iRec: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;
  begin
    if (iRec.SpouseToFamilyLinksCount > 0)
    then Result := iRec.SpouseToFamilyLinks[0].Family
    else begin
      Result := CreateFamilyEx(Base.Tree);
      Base.Engine.AddFamilySpouse(Result, iRec);
    end;
  end;

  function CheckMain(aMain: TGEDCOMIndividualRecord): Boolean;
  begin
    Result := (aMain <> nil);
    if not(Result) then raise Exception.Create(LSList[LSID_BasePersonInvalid]);
  end;

var
  row, birth_year, src_year: Integer;
  lnk, nm, pt, fm, age, comment, place, src_name, src_page, ev_name: string;
  link: TPersonLink;
  iRec, iMain: TGEDCOMIndividualRecord;
  sx: TGEDCOMSex;
  note: TGEDCOMNoteRecord;
  family: TGEDCOMFamilyRecord;
  evt: TGEDCOMCustomEvent;
  src_rec: TGEDCOMSourceRecord;
begin
  src_name := cbSource.Text;
  src_page := edPage.Text;
  if not(IsDigits(edSourceYear.Text)) then begin
    MessageDlg(LSList[LSID_SourceYearInvalid], mtError, [mbOk], 0);
    Exit;
  end else src_year := StrToInt(edSourceYear.Text);
  place := edPlace.Text;
  iMain := nil;

  try
    for row := 1 to sgData.RowCount - 1 do begin
      lnk := sgData.Cells[0, row];
      nm := sgData.Cells[1, row];
      pt := sgData.Cells[2, row];
      fm := sgData.Cells[3, row];
      age := sgData.Cells[4, row];
      comment := sgData.Cells[5, row];

      if (lnk <> '') then begin
        link := GetLinkByName(lnk);

        sx := DefineSex(nm, pt, fmGEDKeeper.NamesTable);
        iRec := CreatePersonEx(Base.Tree, nm, pt, fm, sx, False);
        Base.ChangeRecord(iRec);

        if (age <> '') and (IsDigits(age)) then begin
          birth_year := src_year - StrToInt(age);
          CreateEventEx(Base.Tree, iRec, 'BIRT', 'ABT '+IntToStr(birth_year), '');
        end;

        if (place <> '') then begin
          evt := CreateEventEx(Base.Tree, iRec, 'RESI', '', '');
          evt.Detail.Place.StringValue := place;
        end;

        if (comment <> '') then begin
          note := CreateNoteEx(Base.Tree, nil, iRec);
          AddNoteText(note, comment);
        end;

        if (src_name <> '') then begin
          src_rec := Base.Engine.FindSource(src_name);
          if (src_rec = nil) then begin
            src_rec := CreateSource(Base.Tree);
            src_rec.FiledByEntry := src_name;
          end;
          BindRecordSource(Base.Tree, iRec, src_rec, src_page, 0);
        end;

        case link of
          plNone: ;

          plPerson: begin
            iMain := iRec;

            if (rgSourceKind.ItemIndex = 1) then begin // метрика
              { Рождение, Смерть, Брак }
              case cbEventType.ItemIndex of
                -1: ev_name := '';
                 0: ev_name := 'BIRT';
                 1: ev_name := 'DEAT';
                 2: ev_name := 'MARR';
              end;
            end;

            if (ev_name = 'BIRT') or (ev_name = 'DEAT') then begin
              evt := CreateEventEx(Base.Tree, iRec, ev_name, StrToGEDCOMDate(edEventDate.Text), '');
              evt.Detail.Place.StringValue := place;
            end
            else
            if (ev_name = 'MARR') then begin
              family := GetMarriageFamily(iRec);
              evt := CreateEventEx(Base.Tree, family, ev_name, StrToGEDCOMDate(edEventDate.Text), '');
              evt.Detail.Place.StringValue := place;
            end;
          end;

          plFather, plMother: begin
            CheckMain(iMain);
            family := GetParentsFamily(iMain);
            Base.Engine.AddFamilySpouse(family, iRec);
          end;

          plGodparent: begin
            CheckMain(iMain);
            Base.Engine.AddAssociation(iMain, LSList[LSID_PLGodparent], iRec);
          end;

          plSpouse: begin
            CheckMain(iMain);
            family := GetMarriageFamily(iMain);
            Base.Engine.AddFamilySpouse(family, iRec);
          end;

          plChild: begin
            CheckMain(iMain);
            family := GetMarriageFamily(iMain);
            Base.Engine.AddFamilyChild(family, iRec);
          end;
        end;
      end;
    end;
  finally

  end;

  InitSourceControls();
end;

procedure TfmPersonScan.rgSourceKindClick(Sender: TObject);
begin
  gbMetrics.Enabled := (rgSourceKind.ItemIndex = 1);
end;

procedure TfmPersonScan.btnParseClick(Sender: TObject);
begin
  case PageControl1.TabIndex of
    0: ParseSimple();
    1: ParseSource(); 
  end;

  Base.ListsRefresh();
end;

procedure TfmPersonScan.EditBirthDateChange(Sender: TObject);
begin
  CheckBirth.Checked := True;
end;

procedure TfmPersonScan.EditDeathDateChange(Sender: TObject);
begin
  CheckDeath.Checked := True;
end;

function TfmPersonScan.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmPersonScan.EditNameKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = '/') then begin
    Key := #0;
    MessageBeep(MB_ICONEXCLAMATION);
  end;
end;

procedure TfmPersonScan.sgDataSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  R: TRect;
  idx: Integer;
begin
  if (ACol = 0) then begin
    idx := cbPersonLink.Items.IndexOf(sgData.Cells[0, ARow]);
    if (idx < 0) then idx := 0;
    cbPersonLink.ItemIndex := idx;

    R := sgData.CellRect(ACol, ARow);
    R.Left  := R.Left + sgData.Left;
    R.Right := R.Right + sgData.Left;
    R.Top := R.Top + sgData.Top;
    R.Bottom := R.Bottom + sgData.Top;
    cbPersonLink.Top := R.Top + 2;
    cbPersonLink.Left := R.Left + 2;
    cbPersonLink.Width := (R.Right - R.Left);
    cbPersonLink.Height := (R.Bottom - R.Top);
    cbPersonLink.Visible := True;
    cbPersonLink.SetFocus;
  end else begin
    cbPersonLink.Visible := False;
  end;

  CanSelect := True;
end;

procedure TfmPersonScan.cbPersonLinkChange(Sender: TObject);
begin
  sgData.Cells[0, sgData.Row] := cbPersonLink.Text;
end;

function TfmPersonScan.CheckCell(ACol, ARow: Integer): Boolean;
var
  val: string;
begin
  Result := True;
  val := sgData.Cells[ACol, ARow];

  if (ACol = 0) then begin // связь
    Result := (GetLinkByName(val) <> plNone);
  end;
  if (ACol = 4) then begin // возраст
    Result := (val = '') or ((val <> '') and IsDigits(val));
  end;

  if not(Result)
  then MessageDlg(LSList[LSID_ValueInvalid], mtError, [mbOk], 0);
end;

procedure TfmPersonScan.cbPersonLinkKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  sgData.Cells[0, sgData.Row] := cbPersonLink.Text;

  if (Key = VK_RETURN) and CheckCell(sgData.Col, sgData.Row) then begin
    sgData.SetFocus;
    sgData.Col := sgData.Col + 1;
  end;
end;

procedure TfmPersonScan.sgDataKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and CheckCell(sgData.Col, sgData.Row) then begin
    if (sgData.Col = 5) then begin
      sgData.Col := 0;
      sgData.Row := sgData.Row + 1;
    end else sgData.Col := sgData.Col + 1;
  end;
end;

end.
