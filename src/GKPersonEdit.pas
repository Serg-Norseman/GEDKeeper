unit GKPersonEdit;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons, GedCom551, bsCtrls, GKBase, GKCommon;

type
  TfmPersonEdit = class(TForm)
    PagesPersonData: TPageControl;
    SheetEvents: TTabSheet;
    SheetNotes: TTabSheet;
    SheetMultimedia: TTabSheet;
    SheetSources: TTabSheet;
    SheetSpouses: TTabSheet;
    SheetAssociations: TTabSheet;
    SheetGroups: TTabSheet;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    Label5: TLabel;
    cbRestriction: TComboBox;
    SheetIndividual: TTabSheet;
    GroupMother: TGroupBox;
    btnMotherAdd: TSpeedButton;
    btnMotherDelete: TSpeedButton;
    btnMotherSel: TSpeedButton;
    EditMother: TEdit;
    GroupFather: TGroupBox;
    btnFatherAdd: TSpeedButton;
    btnFatherDelete: TSpeedButton;
    btnFatherSel: TSpeedButton;
    EditFather: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    EditFamily: TEdit;
    EditName: TEdit;
    EditPatronymic: TEdit;
    EditSex: TComboBox;
    CheckPatriarch: TCheckBox;
    btnNameCopy: TSpeedButton;
    GroupBox2: TGroupBox;
    Label8: TLabel;
    edPieceSurnamePrefix: TEdit;
    Label6: TLabel;
    edPiecePrefix: TEdit;
    Label9: TLabel;
    edPieceSuffix: TEdit;
    Label7: TLabel;
    edPieceNickname: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnFatherAddClick(Sender: TObject);
    procedure btnFatherDeleteClick(Sender: TObject);
    procedure btnFatherSelClick(Sender: TObject);
    procedure btnMotherAddClick(Sender: TObject);
    procedure btnMotherDeleteClick(Sender: TObject);
    procedure btnMotherSelClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure cbRestrictionChange(Sender: TObject);
    procedure EditFamilyChange(Sender: TObject);
    procedure EditNameChange(Sender: TObject);
    procedure EditPatronymicChange(Sender: TObject);
    procedure btnNameCopyClick(Sender: TObject);
  private
    FPerson: TGEDCOMIndividualRecord;

    FEventsList: TSheetList;

    FSpousesList: TSheetList;
    FAssociationsList: TSheetList;
    FGroupsList: TSheetList;

    FNotesList: TSheetList;
    FMediaList: TSheetList;
    FSourcesList: TSheetList;

    procedure ControlsRefresh();
    function  GetBase: TfmBase;
    procedure LockEditor(aLocked: Boolean);
    procedure SetPerson(const Value: TGEDCOMIndividualRecord);
    procedure UpdatePerson();
    procedure SetTitle();
    procedure ListModify(Sender: TObject; Index: Integer; Action: TRecAction);
  public
    property Base: TfmBase read GetBase;
    property Person: TGEDCOMIndividualRecord read FPerson write SetPerson;
  end;

var
  fmPersonEdit: TfmPersonEdit;

implementation

uses
  bsComUtils, ClipBrd, GKMain, GKRecordSelect, GKFamilyEdit;

{$R *.dfm}

type
  TPersonTabs = (
    ptCommon, ptEvents, ptSpouses, ptAssociations, ptGroups,
    ptNotes, ptMedia, ptSources);

procedure TfmPersonEdit.FormCreate(Sender: TObject);
var
  sx: TGEDCOMSex;
begin
  for sx := Low(TGEDCOMSex) to High(TGEDCOMSex) do
    EditSex.Items.Add(Sex[sx]);

  FEventsList := TSheetList.Create(SheetEvents);
  FEventsList.OnModify := ListModify;
  Base.SetupRecEventsList(FEventsList.List, True);

  //

  FSpousesList := TSheetList.Create(SheetSpouses);
  FSpousesList.OnModify := ListModify;
  FSpousesList.Buttons := [lbAdd..lbMoveDown];
  Base.SetupRecSpousesList(FSpousesList.List);

  FAssociationsList := TSheetList.Create(SheetAssociations);
  FAssociationsList.OnModify := ListModify;
  FAssociationsList.Buttons := [lbAdd..lbJump];
  Base.SetupRecAssociationsList(FAssociationsList.List);

  FGroupsList := TSheetList.Create(SheetGroups);
  FGroupsList.OnModify := ListModify;
  FGroupsList.Buttons := [lbAdd, lbDelete];
  Base.SetupRecGroupsList(FGroupsList.List);

  //

  FNotesList := TSheetList.Create(SheetNotes);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList.List);

  FMediaList := TSheetList.Create(SheetMultimedia);
  FMediaList.OnModify := ListModify;
  Base.SetupRecMediaList(FMediaList.List);

  FSourcesList := TSheetList.Create(SheetSources);
  FSourcesList.OnModify := ListModify;
  Base.SetupRecSourcesList(FSourcesList.List);
end;

procedure TfmPersonEdit.ControlsRefresh();
var
  unk: string;
  family: TGEDCOMFamilyRecord;
  sp: TGEDCOMPointer;
  rel_person: TGEDCOMIndividualRecord;
  idx: Integer;
  item: TListItem;
  np: TGEDCOMPersonalName;
begin
  np := FPerson.PersonalNames[0];

  edPiecePrefix.Text := np.Pieces.Prefix;
  edPieceNickname.Text := np.Pieces.Nickname;
  edPieceSurnamePrefix.Text := np.Pieces.SurnamePrefix;
  edPieceSuffix.Text := np.Pieces.Suffix;

  if (FPerson.ChildToFamilyLinksCount <> 0) then begin
    family := FPerson.ChildToFamilyLinks[0].Family;

    rel_person := TGEDCOMIndividualRecord(family.Husband.Value);
    if (rel_person <> nil) then begin
      EditFather.Text := GetNameStr(rel_person);

      btnFatherAdd.Enabled := False;
      btnFatherDelete.Enabled := True;
      btnFatherSel.Enabled := True;
    end else begin
      EditFather.Text := '';

      btnFatherAdd.Enabled := True;
      btnFatherDelete.Enabled := False;
      btnFatherSel.Enabled := False;
    end;

    rel_person := TGEDCOMIndividualRecord(family.Wife.Value);
    if (rel_person <> nil) then begin
      EditMother.Text := GetNameStr(rel_person);

      btnMotherAdd.Enabled := False;
      btnMotherDelete.Enabled := True;
      btnMotherSel.Enabled := True;
    end else begin
      EditMother.Text := '';

      btnMotherAdd.Enabled := True;
      btnMotherDelete.Enabled := False;
      btnMotherSel.Enabled := False;
    end;
  end else begin
    EditFather.Text := '';
    btnFatherAdd.Enabled := True;
    btnFatherDelete.Enabled := False;
    btnFatherSel.Enabled := False;

    EditMother.Text := '';
    btnMotherAdd.Enabled := True;
    btnMotherDelete.Enabled := False;
    btnMotherSel.Enabled := False;
  end;

  Base.RecListIndividualEventsRefresh(FPerson, FEventsList.List, nil);
  Base.RecListNotesRefresh(FPerson, FNotesList.List, nil);
  Base.RecListMediaRefresh(FPerson, FMediaList.List, nil);
  Base.RecListSourcesRefresh(FPerson, FSourcesList.List, nil);

  FSpousesList.List.Clear();
  for idx := 0 to FPerson.SpouseToFamilyLinksCount - 1 do begin
    family := FPerson.SpouseToFamilyLinks[idx].Family;
    if (family = nil) then begin
      //LogWrite('File ('+FFileName+'), iRec ('+iRec.XRef+'): empty family entry');
      Continue;
    end;

    if (FPerson.Sex = svMale) then begin
      sp := family.Wife;
      unk := UnkFemale;
    end else begin
      sp := family.Husband;
      unk := UnkMale;
    end;

    rel_person := TGEDCOMIndividualRecord(sp.Value);

    item := FSpousesList.List.Items.Add();
    item.Data := TObject(idx);
    if (rel_person <> nil)
    then item.Caption := GetNameStr(rel_person)
    else item.Caption := unk;
    item.SubItems.Add(GetMarriageDate(family, fmGEDKeeper.Options.DefDateFormat));
  end;

  Base.RecListAssociationsRefresh(FPerson, FAssociationsList.List, nil);
  Base.RecListGroupsRefresh(FPerson, FGroupsList.List, nil);
end;

procedure TfmPersonEdit.SetPerson(const Value: TGEDCOMIndividualRecord);
var
  fam, nam, pat: string;
begin
  FPerson := Value;

  try
    GetNameParts(FPerson, fam, nam, pat);

    EditFamily.Text := fam;
    EditName.Text := nam;
    EditPatronymic.Text := pat;
    EditSex.ItemIndex := Ord(FPerson.Sex);

    // extended begin
    CheckPatriarch.Checked := (FPerson.FindTag(PatriarchTag) <> nil);
    // extended end

    cbRestriction.ItemIndex := Ord(FPerson.Restriction);

    ControlsRefresh();
  except
    on E: Exception do LogWrite('PersonRefresh().Families(): ' + E.Message);
  end;
end;

procedure TfmPersonEdit.btnFatherAddClick(Sender: TObject);
var
  father: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
begin
  father := Base.SelectPerson(FPerson, tmDescendant, svMale);
  if (father <> nil) then begin
    family := Base.GetChildFamily(FPerson, True, father);

    if (family.Husband.StringValue = '') then begin
      family.SetTagStringValue('HUSB', '@'+father.XRef+'@');

      father.AddSpouseToFamilyLink(
        TGEDCOMSpouseToFamilyLink.CreateTag(
          Base.Tree, father, 'FAMS', '@'+family.XRef+'@'));
    end;

    ControlsRefresh();
  end;
end;

procedure TfmPersonEdit.btnFatherDeleteClick(Sender: TObject);
var
  father: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
begin
  if (MessageDlg('Удалить ссылку на отца?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  family := Base.GetChildFamily(FPerson, False, nil);
  if (family <> nil) then begin
    father := TGEDCOMIndividualRecord(family.Husband.Value);
    RemoveFamilySpouse(Base.Tree, family, father);

    ControlsRefresh();
  end;
end;

procedure TfmPersonEdit.btnFatherSelClick(Sender: TObject);
var
  father: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
begin
  family := Base.GetChildFamily(FPerson, False, nil);
  if (family <> nil) then begin
    father := TGEDCOMIndividualRecord(family.Husband.Value);
    SetPerson(father);
  end;
end;

procedure TfmPersonEdit.btnMotherAddClick(Sender: TObject);
var
  mother: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
begin
  mother := Base.SelectPerson(FPerson, tmDescendant, svFemale);
  if (mother <> nil) then begin
    family := Base.GetChildFamily(FPerson, True, mother);

    if (family.Wife.StringValue = '') then begin
      family.SetTagStringValue('WIFE', '@'+mother.XRef+'@');

      mother.AddSpouseToFamilyLink(
        TGEDCOMSpouseToFamilyLink.CreateTag(
          Base.Tree, mother, 'FAMS', '@'+family.XRef+'@'));
    end;

    ControlsRefresh();
  end;
end;

procedure TfmPersonEdit.btnMotherDeleteClick(Sender: TObject);
var
  mother: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
begin
  if (MessageDlg('Удалить ссылку на мать?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  family := Base.GetChildFamily(FPerson, False, nil);
  if (family <> nil) then begin
    mother := TGEDCOMIndividualRecord(family.Wife.Value);
    RemoveFamilySpouse(Base.Tree, family, mother);

    ControlsRefresh();
  end;
end;

procedure TfmPersonEdit.btnMotherSelClick(Sender: TObject);
var
  mother: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
begin
  family := Base.GetChildFamily(FPerson, False, nil);
  if (family <> nil) then begin
    mother := TGEDCOMIndividualRecord(family.Wife.Value);
    SetPerson(mother);
  end;
end;

procedure TfmPersonEdit.UpdatePerson();
var
  np: TGEDCOMPersonalName;
  pieces: TGEDCOMPersonalNamePieces;
begin
  np := FPerson.PersonalNames[0];
  np.SetNameParts(
    Trim(EditName.Text) + ' ' + Trim(EditPatronymic.Text),
    Trim(EditFamily.Text), np.LastPart);

  pieces := np.Pieces;
  if (pieces.Prefix <> edPiecePrefix.Text) then pieces.Prefix := edPiecePrefix.Text;
  if (pieces.Nickname <> edPieceNickname.Text) then pieces.Nickname := edPieceNickname.Text;
  if (pieces.SurnamePrefix <> edPieceSurnamePrefix.Text) then pieces.SurnamePrefix := edPieceSurnamePrefix.Text;
  if (pieces.Suffix <> edPieceSuffix.Text) then pieces.Suffix := edPieceSuffix.Text;

  FPerson.Sex := TGEDCOMSex(EditSex.ItemIndex);

  FPerson.ChangeDate.ChangeDateTime := Now();

  // extended begin
  if (CheckPatriarch.Checked) then begin
    if (FPerson.FindTag(PatriarchTag) = nil)
    then FPerson.AddTag(PatriarchTag);
  end else FPerson.DeleteTag(PatriarchTag);
  // extended end

  FPerson.Restriction := TGEDCOMRestriction(cbRestriction.ItemIndex);

  Base.ChangeRecord(FPerson);
  Base.Modified := True;
end;

procedure TfmPersonEdit.btnAcceptClick(Sender: TObject);
begin
  UpdatePerson();
end;

procedure TfmPersonEdit.LockEditor(aLocked: Boolean);
begin
  if aLocked
  then EditFamily.Color := clInactiveBorder
  else EditFamily.Color := clWindow;
  EditFamily.ReadOnly := aLocked;

  if aLocked
  then EditName.Color := clInactiveBorder
  else EditName.Color := clWindow;
  EditName.ReadOnly := aLocked;

  if aLocked
  then EditPatronymic.Color := clInactiveBorder
  else EditPatronymic.Color := clWindow;
  EditPatronymic.ReadOnly := aLocked;

  if aLocked
  then EditSex.Color := clInactiveBorder
  else EditSex.Color := clWindow;
  EditSex.Enabled := not(aLocked);

  if aLocked then begin
    edPieceSurnamePrefix.Color := clInactiveBorder;
    edPiecePrefix.Color := clInactiveBorder;
    edPieceSuffix.Color := clInactiveBorder;
    edPieceNickname.Color := clInactiveBorder;
  end else begin
    edPieceSurnamePrefix.Color := clWindow;
    edPiecePrefix.Color := clWindow;
    edPieceSuffix.Color := clWindow;
    edPieceNickname.Color := clWindow;
  end;

  btnFatherAdd.Enabled := not(aLocked);
  btnFatherDelete.Enabled := not(aLocked);

  btnMotherAdd.Enabled := not(aLocked);
  btnMotherDelete.Enabled := not(aLocked);
end;

procedure TfmPersonEdit.cbRestrictionChange(Sender: TObject);
begin
  //LockEditor(cbRestriction.ItemIndex = 2);
end;

function TfmPersonEdit.GetBase(): TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmPersonEdit.SetTitle();
begin
  Caption := 'Персона "'+EditFamily.Text+' '+EditName.Text+' '+EditPatronymic.Text+'"';
end;

procedure TfmPersonEdit.EditFamilyChange(Sender: TObject);
begin
  SetTitle();
end;

procedure TfmPersonEdit.EditNameChange(Sender: TObject);
begin
  SetTitle();
end;

procedure TfmPersonEdit.EditPatronymicChange(Sender: TObject);
begin
  SetTitle();
end;

procedure TfmPersonEdit.btnNameCopyClick(Sender: TObject);
begin
  Clipboard.AsText := GetNameStr(FPerson);
end;

procedure TfmPersonEdit.ListModify(Sender: TObject; Index: Integer; Action: TRecAction);
var
  family: TGEDCOMFamilyRecord;
  group: TGEDCOMGroupRecord;
  spouse: TGEDCOMIndividualRecord;
  sp: TGEDCOMPointer;
begin
  if (Sender = FEventsList) then begin
    if (Action = raAdd) then Index := Integer(nil);

    if Base.ModifyRecEvent(FPerson, TGEDCOMCustomEvent(Index), Action)
    then ControlsRefresh();
  end
  else
  //
  if (Sender = FSpousesList) then begin
    case Action of
      raAdd: begin
        family := nil;
        if Base.ModifyFamily(family, FPerson)
        then ControlsRefresh();
      end;

      raEdit: begin
        family := FPerson.SpouseToFamilyLinks[Index].Family;
        if (family <> nil) then begin
          Base.ModifyFamily(family);
          ControlsRefresh();
        end;
      end;

      raDelete: begin
        family := FPerson.SpouseToFamilyLinks[Index].Family;
        if (family = nil) then Exit;

        if (MessageDlg('Удалить ссылку на супруга?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
        then Exit;

        RemoveFamilySpouse(Base.Tree, family, FPerson);
        ControlsRefresh();
      end;

      raJump: begin
        UpdatePerson();

        family := FPerson.SpouseToFamilyLinks[Index].Family;
        if (family = nil) then Exit;

        case FPerson.Sex of
          svNone, svUndetermined: Exit;
          svMale: sp := family.Wife;
          svFemale: sp := family.Husband;
        end;

        spouse := TGEDCOMIndividualRecord(sp.Value);
        SetPerson(spouse);
      end;

      raMoveUp: begin
        FPerson.ExchangeSpouses(Index - 1, Index);
        ControlsRefresh();
      end;

      raMoveDown: begin
        FPerson.ExchangeSpouses(Index, Index + 1);
        ControlsRefresh();
      end;
    end;
  end
  else
  if (Sender = FAssociationsList) then begin
    if Base.ModifyRecAssociation(FPerson, Index, Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FGroupsList) then begin
    case Action of
      raAdd: begin
        group := TGEDCOMGroupRecord(Base.SelectRecord(smGroup));
        if (group <> nil) then begin
          group.AddMember(TGEDCOMPointer.CreateTag(Base.Tree, group, '_MEMBER', '@'+FPerson.XRef+'@'));
          FPerson.AddGroup(TGEDCOMPointer.CreateTag(Base.Tree, FPerson, '_GROUP', '@'+group.XRef+'@'));

          ControlsRefresh();
        end;
      end;
      raEdit: ;
      raDelete: begin
        group := TGEDCOMGroupRecord(FPerson.Groups[Index].Value);

        if (MessageDlg('Удалить ссылку на группу?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
        then Exit;

        group.RemoveMember(group.IndexOfMember(FPerson));
        FPerson.DeleteGroup(FPerson.IndexOfGroup(group));

        ControlsRefresh();
      end;
      raJump: ;
    end;
  end
  else
  //
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(FPerson, Index, Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FMediaList) then begin
    if Base.ModifyRecMultimedia(FPerson, Index, Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FSourcesList) then begin
    if Base.ModifyRecSource(FPerson, Index, Action)
    then ControlsRefresh();
  end;
end;

end.
