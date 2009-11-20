unit GKGroupEdit;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GedCom551, ComCtrls, GKCtrls, ExtCtrls,
  ActnList;

type
  TfmGroupEdit = class(TForm)
    GroupBox1: TGroupBox;
    EditName: TEdit;
    Label1: TLabel;
    PanelFamilyData: TPanel;
    Panel3: TPanel;
    btnAdd: TSpeedButton;
    btnDelete: TSpeedButton;
    btnEdit: TSpeedButton;
    btnMemberSel: TSpeedButton;
    PagesGroupData: TPageControl;
    SheetNotes: TTabSheet;
    ListNotes: TListBox;
    SheetMedia: TTabSheet;
    ListMedia: TListBox;
    SheetMembers: TTabSheet;
    ListMembers: TBSListView;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    ActionList1: TActionList;
    actRecordAdd: TAction;
    actRecordEdit: TAction;
    actRecordDelete: TAction;
    procedure PagesGroupDataChange(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnMemberSelClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure actRecordAddExecute(Sender: TObject);
    procedure actRecordEditExecute(Sender: TObject);
    procedure actRecordDeleteExecute(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
  private
    FGroup: TGEDCOMGroupRecord;
    FTree: TGEDCOMTree;

    function  GetSelectedMember(): TGEDCOMIndividualRecord;
    procedure SetGroup(const Value: TGEDCOMGroupRecord);
    procedure ListsRefresh();
  public
    property Group: TGEDCOMGroupRecord read FGroup write SetGroup;
    property Tree: TGEDCOMTree read FTree write FTree;
  end;

implementation

uses GKMain, GKCommon, GKRecordSelect, GKPersonEdit;

{$R *.dfm}

{ TfmGroupEdit }

procedure TfmGroupEdit.ListsRefresh();
var
  k: Integer;
  member: TGEDCOMIndividualRecord;
  item: TListItem;
begin
  fmGEDKeeper.RecListNotesRefresh(FGroup, ListNotes, nil);
  fmGEDKeeper.RecListMediaRefresh(FGroup, ListMedia, nil);

  ListMembers.Clear();
  for k := 0 to FGroup.MembersCount - 1 do begin
    member := TGEDCOMIndividualRecord(FGroup.Members[k].Value);

    item := ListMembers.Items.Add();
    item.Caption := GetNameStr(member);
    item.Data := TObject(k);
  end;
end;

procedure TfmGroupEdit.SetGroup(const Value: TGEDCOMGroupRecord);
begin
  FGroup := Value;

  try
    if (FGroup = nil)
    then EditName.Text := ''
    else EditName.Text := FGroup.Name;

    ListsRefresh();
  except
    on E: Exception do LogWrite('GroupRefresh(): ' + E.Message);
  end;
end;

function TfmGroupEdit.GetSelectedMember(): TGEDCOMIndividualRecord;
var
  idx: Integer;
begin
  Result := nil;
  if (ListMembers.Selected = nil) then Exit;

  idx := Integer(ListMembers.Selected.Data);
  Result := TGEDCOMIndividualRecord(FGroup.Members[idx].Value);
end;

procedure TfmGroupEdit.PagesGroupDataChange(Sender: TObject);
begin
  btnAdd.Enabled := True;
  btnEdit.Enabled := True;
  btnDelete.Enabled := True;
  btnMemberSel.Enabled := False;

  case PagesGroupData.TabIndex of
    0: ;
    1: ;
    2: begin
      btnEdit.Enabled := False;
      btnMemberSel.Enabled := True;
    end;
  end;
end;

procedure TfmGroupEdit.btnAddClick(Sender: TObject);
var
  member: TGEDCOMIndividualRecord;
begin
  case PagesGroupData.TabIndex of
    0: begin // Заметки
      if fmGEDKeeper.ModifyRecNote(FGroup, -1, raAdd)
      then ListsRefresh();
    end;

    1: begin // Мультимедиа
      if fmGEDKeeper.ModifyRecMultimedia(FGroup, -1, raAdd)
      then ListsRefresh();
    end;

    2: begin // Члены
      member := SelectPerson(nil, tmNone, svNone);
      if (member <> nil) then begin
        FGroup.AddMember(TGEDCOMPointer.CreateTag(FTree, FGroup, '_MEMBER', '@'+member.XRef+'@'));
        member.AddGroup(TGEDCOMPointer.CreateTag(FTree, member, '_GROUP', '@'+FGroup.XRef+'@'));
        ListsRefresh();
      end;
    end;
  end;
end;

procedure TfmGroupEdit.btnEditClick(Sender: TObject);
begin
  case PagesGroupData.TabIndex of
    0: begin // Заметки
      if fmGEDKeeper.ModifyRecNote(FGroup, ListNotes.ItemIndex, raEdit)
      then ListsRefresh();
    end;

    1: begin // Мультимедиа
      if fmGEDKeeper.ModifyRecMultimedia(FGroup, ListMedia.ItemIndex, raEdit)
      then ListsRefresh();
    end;

    2: begin // Дети
    end;
  end;
end;

procedure TfmGroupEdit.btnDeleteClick(Sender: TObject);
var
  member: TGEDCOMIndividualRecord;
begin
  case PagesGroupData.TabIndex of
    0: begin // Заметки
      if fmGEDKeeper.ModifyRecNote(FGroup, ListNotes.ItemIndex, raDelete)
      then ListsRefresh();
    end;

    1: begin // Мультимедиа
      if fmGEDKeeper.ModifyRecMultimedia(FGroup, ListMedia.ItemIndex, raDelete)
      then ListsRefresh();
    end;

    2: begin // Дети
      member := GetSelectedMember();
      if (member = nil) then Exit;

      if (MessageDlg('Удалить ссылку на участника группы?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
      then Exit;

      FGroup.RemoveMember(FGroup.IndexOfMember(member));
      member.DeleteGroup(member.IndexOfGroup(FGroup));

      ListsRefresh();
    end;
  end;
end;

procedure TfmGroupEdit.btnMemberSelClick(Sender: TObject);
var
  member: TGEDCOMIndividualRecord;
begin
  member := GetSelectedMember();
  if (member <> nil) then begin
    if (fmPersonEdit <> nil)
    then fmPersonEdit.Person := member
    else fmGEDKeeper.SelectPersonByIRec(member);

    Close;
  end;
end;

procedure TfmGroupEdit.btnAcceptClick(Sender: TObject);
begin
  FGroup.Name := EditName.Text;
  FGroup.ChangeDate.ChangeDateTime := Now();
  fmGEDKeeper.Modified := True;
end;

procedure TfmGroupEdit.actRecordAddExecute(Sender: TObject);
begin
  btnAddClick(nil);
end;

procedure TfmGroupEdit.actRecordEditExecute(Sender: TObject);
begin
  btnEditClick(nil);
end;

procedure TfmGroupEdit.actRecordDeleteExecute(Sender: TObject);
begin
  btnDeleteClick(nil);
end;

procedure TfmGroupEdit.ListDblClick(Sender: TObject);
begin
  btnEditClick(nil);
end;

end.
