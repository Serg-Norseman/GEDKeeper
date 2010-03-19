unit GKGroupEdit;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Buttons, ComCtrls,
  ExtCtrls, GedCom551, GKBase, GKCommon;

type
  TfmGroupEdit = class(TForm)
    GroupBox1: TGroupBox;
    EditName: TEdit;
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
    FTree: TGEDCOMTree;

    FMembersList: TSheetList;
    FNotesList: TSheetList;
    FMediaList: TSheetList;

    procedure SetGroup(const Value: TGEDCOMGroupRecord);
    procedure ListModify(Sender: TObject; Index: Integer; Action: TRecAction);
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
  Base.SetupRecMembersList(FMembersList.List);

  FNotesList := TSheetList.Create(SheetNotes);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList.List);

  FMediaList := TSheetList.Create(SheetMultimedia);
  FMediaList.OnModify := ListModify;
  Base.SetupRecMediaList(FMediaList.List);
end;

procedure TfmGroupEdit.ListsRefresh();
var
  k: Integer;
  member: TGEDCOMIndividualRecord;
  item: TListItem;
begin
  Base.RecListNotesRefresh(FGroup, FNotesList.List, nil);
  Base.RecListMediaRefresh(FGroup, FMediaList.List, nil);

  FMembersList.List.Clear();
  for k := 0 to FGroup.MembersCount - 1 do begin
    member := TGEDCOMIndividualRecord(FGroup.Members[k].Value);

    item := FMembersList.List.Items.Add();
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

procedure TfmGroupEdit.btnAcceptClick(Sender: TObject);
begin
  FGroup.Name := EditName.Text;
  FGroup.ChangeDate.ChangeDateTime := Now();
  Base.Modified := True;
end;

function TfmGroupEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmGroupEdit.ListModify(Sender: TObject; Index: Integer; Action: TRecAction);
var
  member: TGEDCOMIndividualRecord;
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(FGroup, Index, Action)
    then ListsRefresh();
  end
  else
  if (Sender = FMediaList) then begin
    if Base.ModifyRecMultimedia(FGroup, Index, Action)
    then ListsRefresh();
  end
  else
  if (Sender = FMembersList) then begin
    case Action of
      raAdd: begin
        member := Base.SelectPerson(nil, tmNone, svNone);
        if (member <> nil) then begin
          FGroup.AddMember(TGEDCOMPointer.CreateTag(FTree, FGroup, '_MEMBER', '@'+member.XRef+'@'));
          member.AddGroup(TGEDCOMPointer.CreateTag(FTree, member, '_GROUP', '@'+FGroup.XRef+'@'));
          ListsRefresh();
        end;
      end;
      raEdit: ;
      raDelete: begin
        member := TGEDCOMIndividualRecord(FGroup.Members[Index].Value);
        if (member = nil) then Exit;

        if (MessageDlg('Удалить ссылку на участника группы?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
        then Exit;

        FGroup.RemoveMember(FGroup.IndexOfMember(member));
        member.DeleteGroup(member.IndexOfGroup(FGroup));

        ListsRefresh();
      end;
      raJump: begin
        member := TGEDCOMIndividualRecord(FGroup.Members[Index].Value);
        if (member <> nil) then begin
          if (fmPersonEdit <> nil)
          then fmPersonEdit.Person := member
          else Base.SelectRecordByXRef(member.XRef);

          Close;
        end;
      end;
    end;
  end;
end;

end.
