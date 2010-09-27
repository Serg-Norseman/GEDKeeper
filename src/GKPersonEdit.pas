unit GKPersonEdit;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons, GedCom551, bsCtrls, GKBase, GKCommon, GKSheetList;
                                                  
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
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    btnNameCopy: TSpeedButton;
    EditFamily: TEdit;
    EditName: TEdit;
    EditPatronymic: TEdit;
    EditSex: TComboBox;
    CheckPatriarch: TCheckBox;
    GroupBox2: TGroupBox;
    Label8: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    Label7: TLabel;
    edPieceSurnamePrefix: TEdit;
    edPiecePrefix: TEdit;
    edPieceSuffix: TEdit;
    edPieceNickname: TEdit;
    SheetUserRefs: TTabSheet;
    PageCtlParents: TPageControl;
    SheetCommonParents: TTabSheet;
    SheetSeparateParents: TTabSheet;
    Label12: TLabel;
    EditParents: TEdit;
    btnParentsAdd: TSpeedButton;
    btnParentsEdit: TSpeedButton;
    btnParentsDelete: TSpeedButton;
    Label10: TLabel;
    Label11: TLabel;
    EditMother: TEdit;
    EditFather: TEdit;
    btnMotherAdd: TSpeedButton;
    btnFatherAdd: TSpeedButton;
    btnFatherDelete: TSpeedButton;
    btnMotherDelete: TSpeedButton;
    btnMotherSel: TSpeedButton;
    btnFatherSel: TSpeedButton;
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
    procedure btnParentsAddClick(Sender: TObject);
    procedure btnParentsEditClick(Sender: TObject);
    procedure btnParentsDeleteClick(Sender: TObject);
  private
    FPerson: TGEDCOMIndividualRecord;

    FEventsList: TSheetList;
    FSpousesList: TSheetList;
    FAssociationsList: TSheetList;
    FGroupsList: TSheetList;
    FNotesList: TSheetList;
    FMediaList: TSheetList;
    FSourcesList: TSheetList;
    FUserRefList: TSheetList;

    procedure ControlsRefresh();
    function  GetBase: TfmBase;
    procedure SetPerson(const Value: TGEDCOMIndividualRecord);
    procedure AcceptChanges();
    procedure SetTitle();
    procedure ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
  public
    property Base: TfmBase read GetBase;
    property Person: TGEDCOMIndividualRecord read FPerson write SetPerson;
  end;

implementation

uses
  bsComUtils, ClipBrd, GKMain, GKRecordSelect, GKFamilyEdit, StorageCrypt;

{$R *.dfm}

{ TfmPersonEdit }

procedure TfmPersonEdit.FormCreate(Sender: TObject);
var
  sx: TGEDCOMSex;
  wm: Integer;
begin
  for sx := Low(TGEDCOMSex) to High(TGEDCOMSex) do
    EditSex.Items.Add(Sex[sx]);

  wm := Ord(fmGEDKeeper.Options.WorkMode);
  PageCtlParents.ActivePageIndex := wm;

  FEventsList := TSheetList.Create(SheetEvents{GroupBox3});
  FEventsList.OnModify := ListModify;
  Base.SetupRecEventsList(FEventsList, True);

  //

  FSpousesList := TSheetList.Create(SheetSpouses);
  FSpousesList.OnModify := ListModify;
  FSpousesList.Buttons := [lbAdd..lbMoveDown];
  AddListColumn(FSpousesList.List, '№', 25);
  AddListColumn(FSpousesList.List, 'Имя супруга', 300);
  AddListColumn(FSpousesList.List, 'Дата брака', 100);

  FAssociationsList := TSheetList.Create(SheetAssociations);
  FAssociationsList.OnModify := ListModify;
  FAssociationsList.Buttons := [lbAdd..lbJump];
  AddListColumn(FAssociationsList.List, 'Отношение', 300);
  AddListColumn(FAssociationsList.List, 'Персона', 200);

  FGroupsList := TSheetList.Create(SheetGroups);
  FGroupsList.OnModify := ListModify;
  FGroupsList.Buttons := [lbAdd, lbDelete];
  AddListColumn(FGroupsList.List, 'Группа', 350);

  //

  FNotesList := TSheetList.Create(SheetNotes, lmBox);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  FMediaList := TSheetList.Create(SheetMultimedia);
  FMediaList.OnModify := ListModify;
  Base.SetupRecMediaList(FMediaList);

  FSourcesList := TSheetList.Create(SheetSources);
  FSourcesList.OnModify := ListModify;
  Base.SetupRecSourcesList(FSourcesList);

  FUserRefList := TSheetList.Create(SheetUserRefs);
  FUserRefList.OnModify := ListModify;
  AddListColumn(FUserRefList.List, 'Сноска', 300);
  AddListColumn(FUserRefList.List, 'Тип', 200);
end;

procedure TfmPersonEdit.ControlsRefresh();

  procedure LockEditor(aLocked: Boolean);
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

    CheckPatriarch.Enabled := not(aLocked);

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

    btnFatherAdd.Enabled := btnFatherAdd.Enabled and not(aLocked);
    btnFatherDelete.Enabled := btnFatherDelete.Enabled and not(aLocked);

    btnMotherAdd.Enabled := btnMotherAdd.Enabled and not(aLocked);
    btnMotherDelete.Enabled := btnMotherDelete.Enabled and not(aLocked);

    FEventsList.ReadOnly := (aLocked);
    FSpousesList.ReadOnly := (aLocked);
    FAssociationsList.ReadOnly := (aLocked);
    FGroupsList.ReadOnly := (aLocked);
    FNotesList.ReadOnly := (aLocked);
    FMediaList.ReadOnly := (aLocked);
    FSourcesList.ReadOnly := (aLocked);
    FUserRefList.ReadOnly := (aLocked);
  end;

var
  rel_name: string;
  family: TGEDCOMFamilyRecord;
  rel_person: TGEDCOMIndividualRecord;
  idx: Integer;
  item: TListItem;
  np: TGEDCOMPersonalName;
  uref: TGEDCOMUserReference;
begin
  np := FPerson.PersonalNames[0];

  edPiecePrefix.Text := np.Pieces.Prefix;
  edPieceNickname.Text := np.Pieces.Nickname;
  edPieceSurnamePrefix.Text := np.Pieces.SurnamePrefix;
  edPieceSuffix.Text := np.Pieces.Suffix;

  if (FPerson.ChildToFamilyLinksCount <> 0) then begin
    family := FPerson.ChildToFamilyLinks[0].Family;

    EditParents.Text := GetFamilyStr(family);
    btnParentsAdd.Enabled := False;
    btnParentsEdit.Enabled := True;
    btnParentsDelete.Enabled := True;

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
    EditParents.Text := '';
    btnParentsAdd.Enabled := True;
    btnParentsEdit.Enabled := False;
    btnParentsDelete.Enabled := False;

    EditFather.Text := '';
    btnFatherAdd.Enabled := True;
    btnFatherDelete.Enabled := False;
    btnFatherSel.Enabled := False;

    EditMother.Text := '';
    btnMotherAdd.Enabled := True;
    btnMotherDelete.Enabled := False;
    btnMotherSel.Enabled := False;
  end;

  Base.RecListIndividualEventsRefresh(FPerson, TBSListView(FEventsList.List), nil);
  Base.RecListNotesRefresh(FPerson, FNotesList.List, nil);
  Base.RecListMediaRefresh(FPerson, TBSListView(FMediaList.List), nil);
  Base.RecListSourcesRefresh(FPerson, TBSListView(FSourcesList.List), nil);

  FSpousesList.List.Clear();
  for idx := 0 to FPerson.SpouseToFamilyLinksCount - 1 do begin
    family := FPerson.SpouseToFamilyLinks[idx].Family;
    if (family = nil) then begin
      //LogWrite('File ('+FFileName+'), iRec ('+iRec.XRef+'): empty family entry');
      Continue;
    end;

    if (FPerson.Sex = svMale) then begin
      rel_person := TGEDCOMIndividualRecord(family.Wife.Value);
      rel_name := UnkFemale;
    end else begin
      rel_person := TGEDCOMIndividualRecord(family.Husband.Value);
      rel_name := UnkMale;
    end;

    if (rel_person <> nil)
    then rel_name := GetNameStr(rel_person);

    item := TBSListView(FSpousesList.List).Items.Add();
    item.Data := family;
    item.Caption := IntToStr(idx + 1);
    item.SubItems.Add(rel_name);
    item.SubItems.Add(GetMarriageDate(family, fmGEDKeeper.Options.DefDateFormat));
  end;

  Base.RecListAssociationsRefresh(FPerson, TBSListView(FAssociationsList.List), nil);
  Base.RecListGroupsRefresh(FPerson, TBSListView(FGroupsList.List), nil);

  FUserRefList.List.Clear();
  for idx := 0 to FPerson.UserReferencesCount - 1 do begin
    uref := FPerson.UserReferences[idx];

    item := TBSListView(FUserRefList.List).Items.Add();
    item.Data := uref;
    item.Caption := uref.StringValue;
    item.SubItems.Add(uref.ReferenceType);
  end;

  LockEditor((FPerson.Restriction = rnLocked));
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
    CheckPatriarch.Checked := FPerson.Patriarch;
    // extended end

    cbRestriction.ItemIndex := Ord(FPerson.Restriction);

    ControlsRefresh();
  except
    on E: Exception do LogWrite('PersonEdit.SetPerson(): ' + E.Message);
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

    if (family.Husband.Value = nil)
    then AddSpouseToFamily(Base.Tree, family, father);

    ControlsRefresh();
  end;
end;

procedure TfmPersonEdit.btnFatherDeleteClick(Sender: TObject);
var
  family: TGEDCOMFamilyRecord;
begin
  if (MessageDlg('Удалить ссылку на отца?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  family := Base.GetChildFamily(FPerson, False, nil);
  if (family <> nil) then begin
    RemoveFamilySpouse(Base.Tree, family, TGEDCOMIndividualRecord(family.Husband.Value));
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
    AcceptChanges();
    father := TGEDCOMIndividualRecord(family.Husband.Value);
    Base.SelectRecordByXRef(father.XRef);
    Close;
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

    if (family.Wife.Value = nil)
    then AddSpouseToFamily(Base.Tree, family, mother);

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
    AcceptChanges();
    mother := TGEDCOMIndividualRecord(family.Wife.Value);
    Base.SelectRecordByXRef(mother.XRef);
    Close;
  end;
end;

procedure TfmPersonEdit.AcceptChanges();
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

  Base.DoPersonChangeSex(FPerson, TGEDCOMSex(EditSex.ItemIndex));
  //FPerson.Sex := TGEDCOMSex(EditSex.ItemIndex);

  Base.DoPersonChangePatriarch(FPerson, CheckPatriarch.Checked);
  //FPerson.Patriarch := CheckPatriarch.Checked;

  FPerson.Restriction := TGEDCOMRestriction(cbRestriction.ItemIndex);

  Base.ChangeRecord(FPerson);
end;

procedure TfmPersonEdit.btnAcceptClick(Sender: TObject);
begin
  AcceptChanges();
end;

procedure TfmPersonEdit.cbRestrictionChange(Sender: TObject);
begin
  ControlsRefresh();
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

procedure TfmPersonEdit.ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
var
  family: TGEDCOMFamilyRecord;
  group: TGEDCOMGroupRecord;
  spouse: TGEDCOMIndividualRecord;
  sp: TGEDCOMPointer;
  idx: Integer;
begin
  if (Sender = FEventsList) then begin
    if Base.ModifyRecEvent(Self, FPerson, TGEDCOMCustomEvent(ItemData), Action)
    then ControlsRefresh();
  end
  else
  //
  if (Sender = FSpousesList) then begin
    case Action of
      raAdd: begin
        family := nil;
        if Base.ModifyFamily(family, ftSpouse, FPerson)
        then ControlsRefresh();
      end;

      raEdit: begin
        family := TGEDCOMFamilyRecord(ItemData);
        if Base.ModifyFamily(family)
        then ControlsRefresh();
      end;

      raDelete: begin
        family := TGEDCOMFamilyRecord(ItemData);
        if (family = nil) then Exit;

        if (MessageDlg('Удалить ссылку на супруга?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
        then Exit;

        RemoveFamilySpouse(Base.Tree, family, FPerson);
        ControlsRefresh();
      end;

      raJump: begin
        family := TGEDCOMFamilyRecord(ItemData);
        if (family = nil) then Exit;

        case FPerson.Sex of
          svNone, svUndetermined: Exit;
          svMale: sp := family.Wife;
          svFemale: sp := family.Husband;
        end;

        spouse := TGEDCOMIndividualRecord(sp.Value);

        AcceptChanges();
        Base.SelectRecordByXRef(spouse.XRef);
        Close;
      end;

      raMoveUp: begin
        family := TGEDCOMFamilyRecord(ItemData);
        idx := FPerson.IndexOfSpouse(family);
        FPerson.ExchangeSpouses(idx - 1, idx);
        ControlsRefresh();
      end;

      raMoveDown: begin
        family := TGEDCOMFamilyRecord(ItemData);
        idx := FPerson.IndexOfSpouse(family);
        FPerson.ExchangeSpouses(idx, idx + 1);
        ControlsRefresh();
      end;
    end;
  end
  else
  if (Sender = FAssociationsList) then begin
    if Base.ModifyRecAssociation(FPerson, TGEDCOMAssociation(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FGroupsList) then begin
    case Action of
      raAdd: begin
        group := TGEDCOMGroupRecord(Base.SelectRecord(smGroup));
        if (group <> nil) and Base.GroupMemberAdd(group, FPerson)
        then ControlsRefresh();
      end;
      raEdit: ;
      raDelete: begin
        group := TGEDCOMGroupRecord(ItemData);

        if (MessageDlg('Удалить ссылку на группу?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
        then Exit;

        if Base.GroupMemberRemove(group, FPerson)
        then ControlsRefresh();
      end;
      raJump: ;
    end;
  end
  else
  //
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(FPerson, TGEDCOMNotes(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FMediaList) then begin
    if Base.ModifyRecMultimedia(FPerson, TGEDCOMMultimediaLink(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FSourcesList) then begin
    if Base.ModifyRecSource(FPerson, TGEDCOMSourceCitation(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FUserRefList) then begin
    if Base.ModifyRecUserRef(FPerson, TGEDCOMUserReference(ItemData), Action)
    then ControlsRefresh();
  end;
end;

procedure TfmPersonEdit.btnParentsAddClick(Sender: TObject);
var
  family: TGEDCOMFamilyRecord;
begin
  family := TGEDCOMFamilyRecord(Base.SelectFamily(FPerson));
  if (family <> nil) then begin
    // в режиме выбора из списка добавления ребенка не происходит,
    // поэтому дополнительная проверка;
    // этого не делается в режиме вставки новой семьи
    // т.к. ребенок уже должен быть в списке
    // на момент появления диалога семьи, поэтому
    // соответствующая вставка делается в ModifyFamily()
    if (family.IndexOfChild(FPerson) < 0)
    then Base.FamilyChildAdd(family, FPerson);

    ControlsRefresh();
  end;
end;

procedure TfmPersonEdit.btnParentsEditClick(Sender: TObject);
var
  family: TGEDCOMFamilyRecord;
begin
  family := Base.GetChildFamily(FPerson, False, nil);

  if (family <> nil) and Base.ModifyFamily(family)
  then ControlsRefresh();
end;

procedure TfmPersonEdit.btnParentsDeleteClick(Sender: TObject);
var
  family: TGEDCOMFamilyRecord;
begin
  if (MessageDlg('Удалить персону из семьи родителей?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  family := Base.GetChildFamily(FPerson, False, nil);
  if (family <> nil) then begin
    Base.FamilyChildRemove(family, FPerson);
    ControlsRefresh();
  end;
end;

end.
