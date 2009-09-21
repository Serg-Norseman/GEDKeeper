unit GKPersonEdit;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, GKCtrls, ExtCtrls, Buttons, GedCom551, Menus,
  ActnList;

type
  TfmPersonEdit = class(TForm)
    PageControl2: TPageControl;
    SheetIndividual: TTabSheet;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    EditFamily: TEdit;
    EditName: TEdit;
    EditPatronymic: TEdit;
    EditSex: TComboBox;
    GroupFather: TGroupBox;
    btnFatherAdd: TSpeedButton;
    btnFatherDelete: TSpeedButton;
    btnFatherSel: TSpeedButton;
    EditFather: TEdit;
    GroupMother: TGroupBox;
    btnMotherAdd: TSpeedButton;
    btnMotherDelete: TSpeedButton;
    btnMotherSel: TSpeedButton;
    EditMother: TEdit;
    PanelPersonData: TPanel;
    Panel2: TPanel;
    btnPersonDataAdd: TSpeedButton;
    btnPersonDataDelete: TSpeedButton;
    btnPersonDataEdit: TSpeedButton;
    PagesPersonData: TPageControl;
    SheetPersonEvents: TTabSheet;
    ListPersonEvents: TBSListView;
    SheetPersonNotes: TTabSheet;
    ListPersonNotes: TListBox;
    SheetPersonMultimedia: TTabSheet;
    ListPersonMedia: TListBox;
    SheetPersonSources: TTabSheet;
    ListPersonSources: TBSListView;
    SheetSpouses: TTabSheet;
    ListPersonFamilies: TBSListView;
    btnSpouseSel: TSpeedButton;
    ListNamePieces: TListBox;
    btnNamePieceAdd: TSpeedButton;
    btnNamePieceEdit: TSpeedButton;
    btnNamePieceDelete: TSpeedButton;
    SheetAssociations: TTabSheet;
    ListAssociations: TBSListView;
    SheetGroups: TTabSheet;
    ListGroups: TBSListView;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    SheetAttributes: TTabSheet;
    ListAttributes: TBSListView;
    ActionList1: TActionList;
    actRecordAdd: TAction;
    actRecordDelete: TAction;
    actRecordEdit: TAction;
    procedure FormCreate(Sender: TObject);
    procedure btnFatherAddClick(Sender: TObject);
    procedure btnFatherDeleteClick(Sender: TObject);
    procedure btnFatherSelClick(Sender: TObject);
    procedure btnMotherAddClick(Sender: TObject);
    procedure btnMotherDeleteClick(Sender: TObject);
    procedure btnMotherSelClick(Sender: TObject);
    procedure btnPersonDataAddClick(Sender: TObject);
    procedure btnPersonDataEditClick(Sender: TObject);
    procedure btnPersonDataDeleteClick(Sender: TObject);
    procedure PagesPersonDataChange(Sender: TObject);
    procedure btnSpouseSelClick(Sender: TObject);
    procedure btnNamePieceEditClick(Sender: TObject);
    procedure btnNamePieceAddClick(Sender: TObject);
    procedure btnNamePieceDeleteClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure EditFamilyChange(Sender: TObject);
    procedure EditNameChange(Sender: TObject);
    procedure EditPatronymicChange(Sender: TObject);
    procedure EditSexChange(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure actRecordAddExecute(Sender: TObject);
    procedure actRecordEditExecute(Sender: TObject);
    procedure actRecordDeleteExecute(Sender: TObject);
  private
    FPerson: TGEDCOMIndividualRecord;
    FTree: TGEDCOMTree;

    function  GetSelectedPersonFamily(): TGEDCOMFamilyRecord;
    procedure SetPerson(const Value: TGEDCOMIndividualRecord);
    procedure NamePiecesRefresh();
    procedure ControlsRefresh();
  public
    property Person: TGEDCOMIndividualRecord read FPerson write SetPerson;
    property Tree: TGEDCOMTree read FTree write FTree;
  end;

var
  fmPersonEdit: TfmPersonEdit;

implementation

uses GKCommon, GKMain, GKRecordSelect, GKFamilyEdit, GKNameEdit;

{$R *.dfm}

procedure TfmPersonEdit.NamePiecesRefresh();
var
  idx: Integer;
  np: TGEDCOMPersonalName;
begin
  np := FPerson.PersonalNames[0];
  ListNamePieces.Items.BeginUpdate();
  ListNamePieces.Clear;
  for idx := 0 to np.Count - 1 do
    ListNamePieces.Items.Add(np.Tags[idx].StringValue);
  ListNamePieces.Items.EndUpdate();
end;

procedure TfmPersonEdit.ControlsRefresh();
var
  unk: string;
  family: TGEDCOMFamilyRecord;
  sp: TGEDCOMPointer;
  rel_person: TGEDCOMIndividualRecord;
  idx: Integer;
  item: TListItem;
begin
  NamePiecesRefresh();

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

  fmGEDKeeper.RecListIndividualEventsRefresh(FPerson, ListPersonEvents, nil);
  fmGEDKeeper.RecListIndividualAttributesRefresh(FPerson, ListAttributes, nil);
  fmGEDKeeper.RecListNotesRefresh(FPerson, ListPersonNotes, nil);
  fmGEDKeeper.RecListMediaRefresh(FPerson, ListPersonMedia, nil);
  fmGEDKeeper.RecListSourcesRefresh(FPerson, ListPersonSources, nil);

  ListPersonFamilies.Clear();

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
    item := ListPersonFamilies.Items.Add();
    item.Data := family;
    if (rel_person <> nil)
    then item.Caption := GetNameStr(rel_person)
    else item.Caption := unk;
    item.SubItems.Add(GetMarriageDate(family, fmGEDKeeper.DefDateFormat));
  end;

  fmGEDKeeper.RecListAssociationsRefresh(FPerson, ListAssociations, nil);
  fmGEDKeeper.RecListGroupsRefresh(FPerson, ListGroups, nil);
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

    ControlsRefresh();
  except
    on E: Exception do LogWrite('PersonRefresh().Families(): ' + E.Message);
  end;
end;

function TfmPersonEdit.GetSelectedPersonFamily(): TGEDCOMFamilyRecord;
begin
  Result := nil;
  if (ListPersonFamilies.Selected = nil) then Exit;
  Result := TGEDCOMFamilyRecord(ListPersonFamilies.Selected.Data);
end;

procedure TfmPersonEdit.FormCreate(Sender: TObject);
var
  sx: TGEDCOMSex;
begin
  for sx := Low(TGEDCOMSex) to High(TGEDCOMSex) do
    EditSex.Items.Add(Sex[sx]);
end;

procedure TfmPersonEdit.btnFatherAddClick(Sender: TObject);
var
  father: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
begin
  father := SelectPerson(FPerson, svMale);
  if (father <> nil) then begin
    family := fmGEDKeeper.GetChildFamily(FPerson, True, father);

    if (family.Husband.StringValue = '') then begin
      family.SetTagStringValue('HUSB', '@'+father.XRef+'@');

      father.AddSpouseToFamilyLink(
        TGEDCOMSpouseToFamilyLink.CreateTag(
          FTree, father, 'FAMS', '@'+family.XRef+'@'));
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

  family := fmGEDKeeper.GetChildFamily(FPerson, False, nil);
  if (family <> nil) then begin
    father := TGEDCOMIndividualRecord(family.Husband.Value);
    fmGEDKeeper.RemoveFamilySpouse(family, father);

    ControlsRefresh();
  end;
end;

procedure TfmPersonEdit.btnFatherSelClick(Sender: TObject);
var
  father: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
begin
  family := fmGEDKeeper.GetChildFamily(FPerson, False, nil);
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
  mother := SelectPerson(FPerson, svFemale);
  if (mother <> nil) then begin
    family := fmGEDKeeper.GetChildFamily(FPerson, True, mother);

    if (family.Wife.StringValue = '') then begin
      family.SetTagStringValue('WIFE', '@'+mother.XRef+'@');

      mother.AddSpouseToFamilyLink(
        TGEDCOMSpouseToFamilyLink.CreateTag(
          FTree, mother, 'FAMS', '@'+family.XRef+'@'));
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

  family := fmGEDKeeper.GetChildFamily(FPerson, False, nil);
  if (family <> nil) then begin
    mother := TGEDCOMIndividualRecord(family.Wife.Value);
    fmGEDKeeper.RemoveFamilySpouse(family, mother);

    ControlsRefresh();
  end;
end;

procedure TfmPersonEdit.btnMotherSelClick(Sender: TObject);
var
  mother: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
begin
  family := fmGEDKeeper.GetChildFamily(FPerson, False, nil);
  if (family <> nil) then begin
    mother := TGEDCOMIndividualRecord(family.Wife.Value);
    SetPerson(mother);
  end;
end;

procedure TfmPersonEdit.btnPersonDataAddClick(Sender: TObject);
var
  family: TGEDCOMFamilyRecord;
  group: TGEDCOMGroupRecord;
begin
  case PagesPersonData.TabIndex of
    0: begin // События
      if fmGEDKeeper.ModifyRecEvent(FPerson, -1, raAdd)
      then ControlsRefresh();
    end;

    1: begin // Атрибуты
      if fmGEDKeeper.ModifyRecAttribute(FPerson, -1, raAdd)
      then ControlsRefresh();
    end;

    2: begin // Заметки
      if fmGEDKeeper.ModifyRecNote(FPerson, -1, raAdd)
      then ControlsRefresh();
    end;

    3: begin // Мультимедиа
      if fmGEDKeeper.ModifyRecMultimedia(FPerson, -1, raAdd)
      then ControlsRefresh();
    end;

    4: begin // Источники
      if fmGEDKeeper.ModifyRecSource(FPerson, -1, raAdd)
      then ControlsRefresh();
    end;

    5: begin // Супруги
      family := nil;
      if fmGEDKeeper.ModifyFamily(family, FPerson)
      then ControlsRefresh();
    end;

    6: begin // Ассоциации
      if fmGEDKeeper.ModifyRecAssociation(FPerson, -1, raAdd)
      then ControlsRefresh();
    end;

    7: begin // Группы
      group := TGEDCOMGroupRecord(SelectRecord(smGroup));
      if (group <> nil) then begin
        group.AddMember(TGEDCOMPointer.CreateTag(FTree, group, '_MEMBER', '@'+FPerson.XRef+'@'));
        FPerson.AddGroup(TGEDCOMPointer.CreateTag(FTree, FPerson, '_GROUP', '@'+group.XRef+'@'));

        ControlsRefresh();
      end;
    end;
  end;
end;

procedure TfmPersonEdit.btnPersonDataEditClick(Sender: TObject);
var
  family: TGEDCOMFamilyRecord;
begin
  case PagesPersonData.TabIndex of
    0: begin // События
      if fmGEDKeeper.ModifyRecEvent(FPerson, GetSelIndex(ListPersonEvents), raEdit)
      then ControlsRefresh();
    end;

    1: begin // Атрибуты
      if fmGEDKeeper.ModifyRecAttribute(FPerson, GetSelIndex(ListAttributes), raEdit)
      then ControlsRefresh();
    end;

    2: begin // Заметки
      if fmGEDKeeper.ModifyRecNote(FPerson, ListPersonNotes.ItemIndex, raEdit)
      then ControlsRefresh();
    end;

    3: begin // Мультимедиа
      if fmGEDKeeper.ModifyRecMultimedia(FPerson, ListPersonMedia.ItemIndex, raEdit)
      then ControlsRefresh();
    end;

    4: begin // Источники
      if fmGEDKeeper.ModifyRecSource(FPerson, GetSelIndex(ListPersonSources), raEdit)
      then ControlsRefresh();
    end;

    5: begin // Супруги
      family := GetSelectedPersonFamily();
      if (family <> nil) then begin
        fmGEDKeeper.ModifyFamily(family);
        ControlsRefresh();
      end;
    end;

    6: begin // Ассоциации
      if fmGEDKeeper.ModifyRecAssociation(FPerson, GetSelIndex(ListAssociations), raEdit)
      then ControlsRefresh();
    end;

    7: begin // Группы
      // empty
    end;
  end;
end;

procedure TfmPersonEdit.btnPersonDataDeleteClick(Sender: TObject);
var
  family: TGEDCOMFamilyRecord;
  group: TGEDCOMGroupRecord;
  ptr: TGEDCOMPointer;
begin
  case PagesPersonData.TabIndex of
    0: begin // События
      if fmGEDKeeper.ModifyRecEvent(FPerson, GetSelIndex(ListPersonEvents), raDelete)
      then ControlsRefresh();
    end;

    1: begin // Атрибуты
      if fmGEDKeeper.ModifyRecAttribute(FPerson, GetSelIndex(ListAttributes), raDelete)
      then ControlsRefresh();
    end;

    2: begin // Заметки
      if fmGEDKeeper.ModifyRecNote(FPerson, ListPersonNotes.ItemIndex, raDelete)
      then ControlsRefresh();
    end;

    3: begin // Мультимедиа
      if fmGEDKeeper.ModifyRecMultimedia(FPerson, ListPersonMedia.ItemIndex, raDelete)
      then ControlsRefresh();
    end;

    4: begin // Источники
      if fmGEDKeeper.ModifyRecSource(FPerson, GetSelIndex(ListPersonSources), raDelete)
      then ControlsRefresh();
    end;

    5: begin // Супруги
      family := GetSelectedPersonFamily();
      if (family = nil) then Exit;

      if (MessageDlg('Удалить ссылку на супруга?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
      then Exit;

      fmGEDKeeper.RemoveFamilySpouse(family, FPerson);

      ControlsRefresh();
    end;

    6: begin // Ассоциации
      if fmGEDKeeper.ModifyRecAssociation(FPerson, GetSelIndex(ListAssociations), raDelete)
      then ControlsRefresh();
    end;

    7: begin // Группы
      ptr := FPerson.Groups[GetSelIndex(ListGroups)];
      group := TGEDCOMGroupRecord(ptr.Value);

      if (MessageDlg('Удалить ссылку на группу?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
      then Exit;

      group.RemoveMember(group.IndexOfMember(FPerson));
      FPerson.DeleteGroup(FPerson.IndexOfGroup(group));

      ControlsRefresh();
    end;
  end;
end;

procedure TfmPersonEdit.PagesPersonDataChange(Sender: TObject);
begin
  btnPersonDataAdd.Enabled := True;
  btnPersonDataEdit.Enabled := True;
  btnPersonDataDelete.Enabled := True;
  btnSpouseSel.Enabled := False;

  case PagesPersonData.TabIndex of
    0: ;
    1: ;
    2: ;
    3: ;
    4: ;
    5: begin
      btnSpouseSel.Enabled := True;
    end;
    6: begin
      btnSpouseSel.Enabled := True;
    end;
    7: begin
      btnPersonDataEdit.Enabled := False;
      btnSpouseSel.Enabled := True;
    end;
  end;
end;

procedure TfmPersonEdit.btnSpouseSelClick(Sender: TObject);
var
  spouse: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
  sp: TGEDCOMPointer;
begin
  case PagesPersonData.TabIndex of
    5: begin
      family := GetSelectedPersonFamily();
      if (family = nil) then Exit;

      case person.Sex of
        svNone: ;
        svMale: sp := family.Wife;
        svFemale: sp := family.Husband;
        svUndetermined: ;
      end;

      spouse := TGEDCOMIndividualRecord(sp.Value);
      SetPerson(spouse);
    end;

    6: begin
    end;
  end;
end;

procedure TfmPersonEdit.btnNamePieceEditClick(Sender: TObject);
var
  fmNameEdit: TfmNameEdit;
  idx: Integer;
  tag: string;
  np: TGEDCOMPersonalName;
begin
  idx := ListNamePieces.ItemIndex;
  if (idx < 0) then Exit;

  np := FPerson.PersonalNames[0];
  tag := np.Tags[idx].Name;

  fmNameEdit := TfmNameEdit.Create(Application);
  try
    fmNameEdit.EditKind.ItemIndex := GetNamePieceIndex(tag);
    fmNameEdit.EditKind.Enabled := False;
    fmNameEdit.EditName.Text := np.Tags[idx].StringValue;

    if (fmNameEdit.ShowModal = mrOk) then begin
      np.Tags[idx].StringValue := fmNameEdit.EditName.Text;
    end;

    NamePiecesRefresh();

    ControlsRefresh();
  finally
    fmNameEdit.Destroy;
  end;
end;

procedure TfmPersonEdit.btnNamePieceAddClick(Sender: TObject);
var
  fmNameEdit: TfmNameEdit;
  np: TGEDCOMPersonalName;
begin
  fmNameEdit := TfmNameEdit.Create(Application);
  try
    fmNameEdit.EditKind.ItemIndex := 0;

    if (fmNameEdit.ShowModal = mrOk) then begin
      np := FPerson.PersonalNames[0];
      np.AddTag(NamePieces[fmNameEdit.EditKind.ItemIndex].Sign, fmNameEdit.EditName.Text);
    end;

    NamePiecesRefresh();
  finally
    fmNameEdit.Destroy;
  end;
end;

procedure TfmPersonEdit.btnNamePieceDeleteClick(Sender: TObject);
var
  idx: Integer;
begin
  idx := ListNamePieces.ItemIndex;
  if (idx < 0) then Exit;

  FPerson.PersonalNames[0].Delete(idx);

  NamePiecesRefresh();
end;

procedure TfmPersonEdit.btnAcceptClick(Sender: TObject);
var
  np: TGEDCOMPersonalName;
begin
  np := FPerson.PersonalNames[0];
  np.SetNameParts(EditName.Text + ' ' + EditPatronymic.Text, EditFamily.Text, np.LastPart);

  FPerson.Sex := TGEDCOMSex(EditSex.ItemIndex);

  FPerson.ChangeDate.ChangeDateTime := Now();
  fmGEDKeeper.Modified := True;
end;

procedure TfmPersonEdit.EditFamilyChange(Sender: TObject);
begin
  //
end;

procedure TfmPersonEdit.EditNameChange(Sender: TObject);
begin
  //
end;

procedure TfmPersonEdit.EditPatronymicChange(Sender: TObject);
begin
  //
end;

procedure TfmPersonEdit.EditSexChange(Sender: TObject);
begin
  //
end;

procedure TfmPersonEdit.ListDblClick(Sender: TObject);
begin
  btnPersonDataEditClick(nil);
end;

procedure TfmPersonEdit.actRecordAddExecute(Sender: TObject);
begin
  btnPersonDataAddClick(nil);
end;

procedure TfmPersonEdit.actRecordDeleteExecute(Sender: TObject);
begin
  btnPersonDataDeleteClick(nil);
end;

procedure TfmPersonEdit.actRecordEditExecute(Sender: TObject);
begin
  btnPersonDataEditClick(nil);
end;

end.
