unit GKGroupEdit; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms,
  VCLStub, GedCom551, GKBase, GKEngine, GKCtrls, GKLists, GKUtils, GKLangs;

type
  TfmGroupEdit = class(System.Windows.Forms.Form)
  strict private
    GroupBox1: System.Windows.Forms.GroupBox;
    edName: System.Windows.Forms.TextBox;
    Label1: System.Windows.Forms.Label;
    PagesGroupData: System.Windows.Forms.TabControl;
    SheetNotes: System.Windows.Forms.TabPage;
    SheetMultimedia: System.Windows.Forms.TabPage;
    SheetMembers: System.Windows.Forms.TabPage;
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;

    FBase: TfmBase;
    FGroup: TGEDCOMGroupRecord;

    FMembersList: TSheetList;
    FNotesList: TSheetList;
    FMediaList: TSheetList;

    procedure AcceptChanges();
    procedure SetGroup(const Value: TGEDCOMGroupRecord);
    procedure ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
    procedure ListsRefresh();
    procedure InitializeComponent;
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property Group: TGEDCOMGroupRecord read FGroup write SetGroup;
  end;

implementation

procedure TfmGroupEdit.InitializeComponent;
begin
  Self.GroupBox1 := System.Windows.Forms.GroupBox.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.edName := System.Windows.Forms.TextBox.Create;
  Self.PagesGroupData := System.Windows.Forms.TabControl.Create;
  Self.SheetMembers := System.Windows.Forms.TabPage.Create;
  Self.SheetNotes := System.Windows.Forms.TabPage.Create;
  Self.SheetMultimedia := System.Windows.Forms.TabPage.Create;
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.GroupBox1.SuspendLayout;
  Self.PagesGroupData.SuspendLayout;
  Self.SuspendLayout;
  // 
  // GroupBox1
  // 
  Self.GroupBox1.Controls.Add(Self.Label1);
  Self.GroupBox1.Controls.Add(Self.edName);
  Self.GroupBox1.Location := System.Drawing.Point.Create(0, 0);
  Self.GroupBox1.Name := 'GroupBox1';
  Self.GroupBox1.Size := System.Drawing.Size.Create(482, 49);
  Self.GroupBox1.TabIndex := 1;
  Self.GroupBox1.TabStop := False;
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 24);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(55, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Название';
  // 
  // edName
  // 
  Self.edName.Location := System.Drawing.Point.Create(72, 16);
  Self.edName.Name := 'edName';
  Self.edName.Size := System.Drawing.Size.Create(401, 21);
  Self.edName.TabIndex := 0;
  Self.edName.Text := '';
  // 
  // PagesGroupData
  // 
  Self.PagesGroupData.Controls.Add(Self.SheetMembers);
  Self.PagesGroupData.Controls.Add(Self.SheetNotes);
  Self.PagesGroupData.Controls.Add(Self.SheetMultimedia);
  Self.PagesGroupData.Location := System.Drawing.Point.Create(0, 49);
  Self.PagesGroupData.Name := 'PagesGroupData';
  Self.PagesGroupData.SelectedIndex := 0;
  Self.PagesGroupData.Size := System.Drawing.Size.Create(482, 360);
  Self.PagesGroupData.TabIndex := 0;
  // 
  // SheetMembers
  // 
  Self.SheetMembers.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetMembers.Name := 'SheetMembers';
  Self.SheetMembers.Size := System.Drawing.Size.Create(474, 334);
  Self.SheetMembers.TabIndex := 0;
  Self.SheetMembers.Text := 'Участники';
  // 
  // SheetNotes
  // 
  Self.SheetNotes.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetNotes.Name := 'SheetNotes';
  Self.SheetNotes.Size := System.Drawing.Size.Create(474, 334);
  Self.SheetNotes.TabIndex := 1;
  Self.SheetNotes.Text := 'Заметки';
  // 
  // SheetMultimedia
  // 
  Self.SheetMultimedia.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetMultimedia.Name := 'SheetMultimedia';
  Self.SheetMultimedia.Size := System.Drawing.Size.Create(474, 334);
  Self.SheetMultimedia.TabIndex := 2;
  Self.SheetMultimedia.Text := 'Мультимедиа';
  // 
  // btnAccept
  // 
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(304, 424);
  Self.btnAccept.Name := 'btnAccept';
  Self.btnAccept.Size := System.Drawing.Size.Create(81, 25);
  Self.btnAccept.TabIndex := 2;
  Self.btnAccept.Text := 'Принять';
  Self.btnAccept.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  Include(Self.btnAccept.Click, Self.btnAccept_Click);
  // 
  // btnCancel
  // 
  Self.btnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnCancel.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnCancel.Location := System.Drawing.Point.Create(392, 424);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 3;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // TfmGroupEdit
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(482, 457);
  Self.Controls.Add(Self.btnCancel);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.GroupBox1);
  Self.Controls.Add(Self.PagesGroupData);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmGroupEdit';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Правка группы';
  Self.GroupBox1.ResumeLayout(False);
  Self.PagesGroupData.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmGroupEdit.Create(aBase: TfmBase);
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  FMembersList := TSheetList.Create(SheetMembers);
  FMembersList.OnModify := ListModify;
  FMembersList.Buttons := TEnumSet.Create([lbAdd, lbEdit, lbDelete, lbJump]);
  FMembersList.List.AddListColumn(LSList[LSID_Name], 300);

  FNotesList := TSheetList.Create(SheetNotes);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  FMediaList := TSheetList.Create(SheetMultimedia);
  FMediaList.OnModify := ListModify;
  Base.SetupRecMediaList(FMediaList);

  // SetLang
  Text := LSList[LSID_WinGroupEdit];
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];
  Label1.Text := LSList[LSID_Title];
  SheetMembers.Text := LSList[LSID_Members];
  SheetNotes.Text := LSList[LSID_RPNotes];
  SheetMultimedia.Text := LSList[LSID_RPMultimedia];
end;

procedure TfmGroupEdit.ListsRefresh();
var
  k: Integer;
  member: TGEDCOMIndividualRecord;
  item: TExtListItem;
begin
  Base.RecListNotesRefresh(FGroup, FNotesList.List, nil);
  Base.RecListMediaRefresh(FGroup, FMediaList.List, nil);

  with FMembersList.List do begin
    BeginUpdate();
    Items.Clear();
    for k := 0 to FGroup.MembersCount - 1 do begin
      member := TGEDCOMIndividualRecord(FGroup.Members[k].Value);
      item := AddItem(TGenEngine.GetNameStr(member), member);
    end;
    EndUpdate;
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
    on E: Exception do TGKUtils.LogWrite('GroupEdit.SetGroup(): ' + E.Message);
  end;
end;

procedure TfmGroupEdit.btnAccept_Click(sender: System.Object; e: System.EventArgs);
begin
  try
    AcceptChanges();

    Self.DialogResult := System.Windows.Forms.DialogResult.OK;
  except
    Self.DialogResult := System.Windows.Forms.DialogResult.None;
  end;
end;

procedure TfmGroupEdit.AcceptChanges();
begin
  FGroup.Name := edName.Text;
  Base.ChangeRecord(FGroup);
end;

procedure TfmGroupEdit.ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
var
  member: TGEDCOMIndividualRecord;
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(Self, FGroup, TGEDCOMNotes(ItemData), Action)
    then ListsRefresh();
  end
  else
  if (Sender = FMediaList) then begin
    if Base.ModifyRecMultimedia(Self, FGroup, TGEDCOMMultimediaLink(ItemData), Action)
    then ListsRefresh();
  end
  else
  if (Sender = FMembersList) then begin
    case Action of
      raAdd: begin
        member := Base.SelectPerson(nil, tmNone, svNone);
        if (member <> nil) and Base.Engine.AddGroupMember(FGroup, member)
        then ListsRefresh();
      end;
      raEdit: ;
      raDelete: begin
        member := TGEDCOMIndividualRecord(ItemData);

        if (member = nil) or (TGKUtils.ShowQuestion(LSList[LSID_DetachMemberQuery]) = System.Windows.Forms.DialogResult.No)
        then Exit;

        if Base.Engine.RemoveGroupMember(FGroup, member)
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
