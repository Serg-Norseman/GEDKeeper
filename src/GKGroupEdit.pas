unit GKGroupEdit;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Buttons, ComCtrls,
  ExtCtrls, GedCom551, GKBase, GKCommon, GKSheetList, bsCtrls;

type
  TfmGroupEdit = class(TForm)
    GroupBox1: TGroupBox;
    edName: TEdit;
    Label1: TLabel;
    PagesGroupData: TPageControl;
    SheetNotes: TTabSheet;
    SheetMultimedia: TTabSheet;
    SheetMembers: TTabSheet;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    procedure btnAcceptClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FGroup: TGEDCOMGroupRecord;

    FMembersList: TSheetList;
    FNotesList: TSheetList;
    FMediaList: TSheetList;

    procedure AcceptChanges();
    procedure SetGroup(const Value: TGEDCOMGroupRecord);
    procedure ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
    procedure ListsRefresh();
    function GetBase: TfmBase;
  public
    property Base: TfmBase read GetBase;
    property Group: TGEDCOMGroupRecord read FGroup write SetGroup;
  end;

implementation

uses
  bsComUtils, GKMain, GKRecordSelect, GKPersonEdit;

{$R *.dfm}

{ TfmGroupEdit }

procedure TfmGroupEdit.FormCreate(Sender: TObject);
begin
  FMembersList := TSheetList.Create(SheetMembers);
  FMembersList.OnModify := ListModify;
  FMembersList.Buttons := [lbAdd..lbJump];
  AddListColumn(FMembersList.List, 'Имя участника группы', 300);

  FNotesList := TSheetList.Create(SheetNotes, lmBox);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  FMediaList := TSheetList.Create(SheetMultimedia);
  FMediaList.OnModify := ListModify;
  Base.SetupRecMediaList(FMediaList);
end;

procedure TfmGroupEdit.ListsRefresh();
var
  k: Integer;
  member: TGEDCOMIndividualRecord;
  item: TListItem;
begin
  Base.RecListNotesRefresh(FGroup, FNotesList.List, nil);
  Base.RecListMediaRefresh(FGroup, TBSListView(FMediaList.List), nil);

  with TListView(FMembersList.List) do begin
    Items.BeginUpdate();
    Items.Clear();
    for k := 0 to FGroup.MembersCount - 1 do begin
      member := TGEDCOMIndividualRecord(FGroup.Members[k].Value);

      item := Items.Add();
      item.Caption := GetNameStr(member);
      item.Data := member;
    end;
    Items.EndUpdate;
  end;
end;

procedure TfmGroupEdit.SetGroup(const Value: TGEDCOMGroupRecord);
begin
  FGroup := Value;

  try
    if (FGroup = nil)
    then edName.Text := ''
    else edName.Text := FGroup.Name;

    ListsRefresh();
  except
    on E: Exception do LogWrite('GroupEdit.SetGroup(): ' + E.Message);
  end;
end;

procedure TfmGroupEdit.AcceptChanges();
begin
  FGroup.Name := edName.Text;
  Base.ChangeRecord(FGroup);
end;

procedure TfmGroupEdit.btnAcceptClick(Sender: TObject);
begin
  AcceptChanges();
end;

function TfmGroupEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmGroupEdit.ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
var
  member: TGEDCOMIndividualRecord;
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(FGroup, TGEDCOMNotes(ItemData), Action)
    then ListsRefresh();
  end
  else
  if (Sender = FMediaList) then begin
    if Base.ModifyRecMultimedia(FGroup, TGEDCOMMultimediaLink(ItemData), Action)
    then ListsRefresh();
  end
  else
  if (Sender = FMembersList) then begin
    case Action of
      raAdd: begin
        member := Base.SelectPerson(nil, tmNone, svNone);
        if (member <> nil) and Base.GroupMemberAdd(FGroup, member)
        then ListsRefresh();
      end;
      raEdit: ;
      raDelete: begin
        member := TGEDCOMIndividualRecord(ItemData);

        if (member = nil) or (MessageDlg('Удалить ссылку на участника группы?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
        then Exit;

        if Base.GroupMemberRemove(FGroup, member)
        then ListsRefresh();
      end;
      raJump: begin
        member := TGEDCOMIndividualRecord(ItemData);
        if (member <> nil) then begin
          AcceptChanges();
          Base.SelectRecordByXRef(member.XRef);
          Close;
        end;
      end;
    end;
  end;
end;

end.
