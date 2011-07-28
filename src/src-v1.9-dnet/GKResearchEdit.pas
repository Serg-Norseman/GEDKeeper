unit GKResearchEdit; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms,
  VCLStub, GedCom551, GKBase, GKEngine, GKCtrls, GKLists, GKUtils, GKMain, GKLangs;

type
  TfmResearchEdit = class(System.Windows.Forms.Form)
  strict private
    GroupBox1: System.Windows.Forms.GroupBox;
    EditName: System.Windows.Forms.TextBox;
    Label1: System.Windows.Forms.Label;
    PagesGroupData: System.Windows.Forms.TabControl;
    SheetNotes: System.Windows.Forms.TabPage;
    SheetTasks: System.Windows.Forms.TabPage;
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    Label2: System.Windows.Forms.Label;
    EditPriority: System.Windows.Forms.ComboBox;
    SheetCommunications: System.Windows.Forms.TabPage;
    Label3: System.Windows.Forms.Label;
    EditStatus: System.Windows.Forms.ComboBox;
    Label4: System.Windows.Forms.Label;
    EditStartDate: System.Windows.Forms.TextBox;
    Label5: System.Windows.Forms.Label;
    EditStopDate: System.Windows.Forms.TextBox;
    Label6: System.Windows.Forms.Label;
    EditPercent: System.Windows.Forms.NumericUpDown;
    SheetGroups: System.Windows.Forms.TabPage;

    FBase: TfmBase;
    FResearch: TGEDCOMResearchRecord;

    FTasksList: TSheetList;
    FCommunicationsList: TSheetList;
    FGroupsList: TSheetList;
    FNotesList: TSheetList;

    procedure AcceptChanges();
    procedure SetResearch(const Value: TGEDCOMResearchRecord);
    procedure ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
    procedure ListsRefresh();
    procedure InitializeComponent;
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property Research: TGEDCOMResearchRecord read FResearch write SetResearch;

    procedure SetLang();
  end;

implementation

procedure TfmResearchEdit.InitializeComponent;
type
  TArrayOfInteger = array of Integer;
begin
  Self.GroupBox1 := System.Windows.Forms.GroupBox.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.Label4 := System.Windows.Forms.Label.Create;
  Self.Label5 := System.Windows.Forms.Label.Create;
  Self.Label6 := System.Windows.Forms.Label.Create;
  Self.EditName := System.Windows.Forms.TextBox.Create;
  Self.EditPriority := System.Windows.Forms.ComboBox.Create;
  Self.EditStatus := System.Windows.Forms.ComboBox.Create;
  Self.EditStartDate := System.Windows.Forms.TextBox.Create;
  Self.EditStopDate := System.Windows.Forms.TextBox.Create;
  Self.EditPercent := System.Windows.Forms.NumericUpDown.Create;
  Self.PagesGroupData := System.Windows.Forms.TabControl.Create;
  Self.SheetTasks := System.Windows.Forms.TabPage.Create;
  Self.SheetCommunications := System.Windows.Forms.TabPage.Create;
  Self.SheetGroups := System.Windows.Forms.TabPage.Create;
  Self.SheetNotes := System.Windows.Forms.TabPage.Create;
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.GroupBox1.SuspendLayout;
  (System.ComponentModel.ISupportInitialize(Self.EditPercent)).BeginInit;
  Self.PagesGroupData.SuspendLayout;
  Self.SuspendLayout;
  // 
  // GroupBox1
  // 
  Self.GroupBox1.Controls.Add(Self.Label1);
  Self.GroupBox1.Controls.Add(Self.Label2);
  Self.GroupBox1.Controls.Add(Self.Label3);
  Self.GroupBox1.Controls.Add(Self.Label4);
  Self.GroupBox1.Controls.Add(Self.Label5);
  Self.GroupBox1.Controls.Add(Self.Label6);
  Self.GroupBox1.Controls.Add(Self.EditName);
  Self.GroupBox1.Controls.Add(Self.EditPriority);
  Self.GroupBox1.Controls.Add(Self.EditStatus);
  Self.GroupBox1.Controls.Add(Self.EditStartDate);
  Self.GroupBox1.Controls.Add(Self.EditStopDate);
  Self.GroupBox1.Controls.Add(Self.EditPercent);
  Self.GroupBox1.Location := System.Drawing.Point.Create(0, 0);
  Self.GroupBox1.Name := 'GroupBox1';
  Self.GroupBox1.Size := System.Drawing.Size.Create(609, 97);
  Self.GroupBox1.TabIndex := 1;
  Self.GroupBox1.TabStop := False;
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 24);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(60, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Название';
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(8, 48);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(60, 13);
  Self.Label2.TabIndex := 1;
  Self.Label2.Text := 'Приоритет';
  // 
  // Label3
  // 
  Self.Label3.Location := System.Drawing.Point.Create(248, 48);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(60, 13);
  Self.Label3.TabIndex := 2;
  Self.Label3.Text := 'Состояние';
  // 
  // Label4
  // 
  Self.Label4.Location := System.Drawing.Point.Create(8, 72);
  Self.Label4.Name := 'Label4';
  Self.Label4.Size := System.Drawing.Size.Create(60, 13);
  Self.Label4.TabIndex := 3;
  Self.Label4.Text := 'Запущено';
  // 
  // Label5
  // 
  Self.Label5.Location := System.Drawing.Point.Create(248, 72);
  Self.Label5.Name := 'Label5';
  Self.Label5.Size := System.Drawing.Size.Create(64, 13);
  Self.Label5.TabIndex := 4;
  Self.Label5.Text := 'Завершено';
  // 
  // Label6
  // 
  Self.Label6.Location := System.Drawing.Point.Create(488, 48);
  Self.Label6.Name := 'Label6';
  Self.Label6.Size := System.Drawing.Size.Create(50, 13);
  Self.Label6.TabIndex := 5;
  Self.Label6.Text := 'Процент';
  // 
  // EditName
  // 
  Self.EditName.Location := System.Drawing.Point.Create(72, 16);
  Self.EditName.Name := 'EditName';
  Self.EditName.Size := System.Drawing.Size.Create(529, 21);
  Self.EditName.TabIndex := 0;
  Self.EditName.Text := '';
  // 
  // EditPriority
  // 
  Self.EditPriority.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.EditPriority.Location := System.Drawing.Point.Create(72, 40);
  Self.EditPriority.Name := 'EditPriority';
  Self.EditPriority.Size := System.Drawing.Size.Create(161, 21);
  Self.EditPriority.TabIndex := 1;
  // 
  // EditStatus
  // 
  Self.EditStatus.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.EditStatus.Location := System.Drawing.Point.Create(312, 40);
  Self.EditStatus.Name := 'EditStatus';
  Self.EditStatus.Size := System.Drawing.Size.Create(161, 21);
  Self.EditStatus.TabIndex := 2;
  // 
  // EditStartDate
  // 
  Self.EditStartDate.Location := System.Drawing.Point.Create(72, 64);
  Self.EditStartDate.MaxLength := 10;
  Self.EditStartDate.Name := 'EditStartDate';
  Self.EditStartDate.Size := System.Drawing.Size.Create(161, 21);
  Self.EditStartDate.TabIndex := 5;
  Self.EditStartDate.Text := '  .  .    ';
  // 
  // EditStopDate
  // 
  Self.EditStopDate.Location := System.Drawing.Point.Create(312, 64);
  Self.EditStopDate.MaxLength := 10;
  Self.EditStopDate.Name := 'EditStopDate';
  Self.EditStopDate.Size := System.Drawing.Size.Create(161, 21);
  Self.EditStopDate.TabIndex := 6;
  Self.EditStopDate.Text := '  .  .    ';
  // 
  // EditPercent
  // 
  Self.EditPercent.Increment := System.Decimal.Create(TArrayOfInteger.Create(5, 
          0, 0, 0));
  Self.EditPercent.Location := System.Drawing.Point.Create(544, 40);
  Self.EditPercent.Name := 'EditPercent';
  Self.EditPercent.Size := System.Drawing.Size.Create(41, 21);
  Self.EditPercent.TabIndex := 3;
  // 
  // PagesGroupData
  // 
  Self.PagesGroupData.Controls.Add(Self.SheetTasks);
  Self.PagesGroupData.Controls.Add(Self.SheetCommunications);
  Self.PagesGroupData.Controls.Add(Self.SheetGroups);
  Self.PagesGroupData.Controls.Add(Self.SheetNotes);
  Self.PagesGroupData.Location := System.Drawing.Point.Create(0, 97);
  Self.PagesGroupData.Name := 'PagesGroupData';
  Self.PagesGroupData.SelectedIndex := 0;
  Self.PagesGroupData.Size := System.Drawing.Size.Create(609, 312);
  Self.PagesGroupData.TabIndex := 0;
  // 
  // SheetTasks
  // 
  Self.SheetTasks.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetTasks.Name := 'SheetTasks';
  Self.SheetTasks.Size := System.Drawing.Size.Create(601, 286);
  Self.SheetTasks.TabIndex := 0;
  Self.SheetTasks.Text := 'Задачи';
  // 
  // SheetCommunications
  // 
  Self.SheetCommunications.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetCommunications.Name := 'SheetCommunications';
  Self.SheetCommunications.Size := System.Drawing.Size.Create(601, 286);
  Self.SheetCommunications.TabIndex := 1;
  Self.SheetCommunications.Text := 'Коммуникации';
  // 
  // SheetGroups
  // 
  Self.SheetGroups.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetGroups.Name := 'SheetGroups';
  Self.SheetGroups.Size := System.Drawing.Size.Create(601, 286);
  Self.SheetGroups.TabIndex := 2;
  Self.SheetGroups.Text := 'Группы';
  // 
  // SheetNotes
  // 
  Self.SheetNotes.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetNotes.Name := 'SheetNotes';
  Self.SheetNotes.Size := System.Drawing.Size.Create(601, 286);
  Self.SheetNotes.TabIndex := 3;
  Self.SheetNotes.Text := 'Заметки';
  // 
  // btnAccept
  // 
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(432, 424);
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
  Self.btnCancel.Location := System.Drawing.Point.Create(520, 424);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 3;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // TfmResearchEdit
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(609, 457);
  Self.Controls.Add(Self.GroupBox1);
  Self.Controls.Add(Self.PagesGroupData);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmResearchEdit';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Редактирование исследования';
  Self.GroupBox1.ResumeLayout(False);
  (System.ComponentModel.ISupportInitialize(Self.EditPercent)).EndInit;
  Self.PagesGroupData.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmResearchEdit.Create(aBase: TfmBase);
var
  rp: TResearchPriority;
  rs: TResearchStatus;
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  //Self.EditStartDate.EditMask := '!99/99/9999;1;_';
  //Self.EditStopDate.EditMask = '!99/99/9999;1;_';

  for rp := Low(TResearchPriority) to High(TResearchPriority) do
    EditPriority.Items.Add(LSList[TGenEngine.PriorityNames[rp]]);

  for rs := Low(TResearchStatus) to High(TResearchStatus) do
    EditStatus.Items.Add(LSList[TGenEngine.StatusNames[rs]]);

  FTasksList := TSheetList.Create(SheetTasks);
  FTasksList.OnModify := ListModify;
  FTasksList.Buttons := TEnumSet.Create([lbAdd, lbEdit, lbDelete, lbJump]);
  FTasksList.List.AddListColumn(LSList[LSID_Goal], 250);
  FTasksList.List.AddListColumn(LSList[LSID_Priority], 90);
  FTasksList.List.AddListColumn(LSList[LSID_StartDate], 90);
  FTasksList.List.AddListColumn(LSList[LSID_StopDate], 90);

  FCommunicationsList := TSheetList.Create(SheetCommunications);
  FCommunicationsList.OnModify := ListModify;
  FCommunicationsList.Buttons := TEnumSet.Create([lbAdd, lbEdit, lbDelete, lbJump]);
  FCommunicationsList.List.AddListColumn(LSList[LSID_Theme], 150);
  FCommunicationsList.List.AddListColumn(LSList[LSID_Corresponder], 150);
  FCommunicationsList.List.AddListColumn(LSList[LSID_Type], 90);
  FCommunicationsList.List.AddListColumn(LSList[LSID_Date], 90);

  FGroupsList := TSheetList.Create(SheetGroups);
  FGroupsList.OnModify := ListModify;
  FGroupsList.Buttons := TEnumSet.Create([lbAdd, lbEdit, lbDelete, lbJump]);
  FGroupsList.List.AddListColumn(LSList[LSID_Group], 350);

  FNotesList := TSheetList.Create(SheetNotes);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  SetLang();
end;

procedure TfmResearchEdit.SetLang();
begin
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  Text := LSList[LSID_WinResearchEdit];

  SheetTasks.Text := LSList[LSID_RPTasks];
  SheetCommunications.Text := LSList[LSID_RPCommunications];
  SheetGroups.Text := LSList[LSID_RPGroups];
  SheetNotes.Text := LSList[LSID_RPNotes];

  Label1.Text := LSList[LSID_Title];
  Label2.Text := LSList[LSID_Priority];
  Label3.Text := LSList[LSID_Status];
  Label6.Text := LSList[LSID_Percent];
  Label4.Text := LSList[LSID_StartDate];
  Label5.Text := LSList[LSID_StopDate];
end;

procedure TfmResearchEdit.ListsRefresh();
var
  k: Integer;
  task: TGEDCOMTaskRecord;
  corr: TGEDCOMCommunicationRecord;
  grp: TGEDCOMGroupRecord;
  item: TExtListItem;
begin
  Base.RecListNotesRefresh(FResearch, FNotesList.List, nil);

  with TGKListView(FTasksList.List) do begin
    BeginUpdate;
    Items.Clear();
    for k := 0 to FResearch.TasksCount - 1 do begin
      task := TGEDCOMTaskRecord(FResearch.Tasks[k].Value);

      item := AddItem(TGenEngine.GetTaskGoalStr(Base.Tree, task), task);
      item.SubItems.Add(LSList[TGenEngine.PriorityNames[task.Priority]]);
      item.SubItems.Add(TGenEngine.GEDCOMDateToStr(task.StartDate, fmGEDKeeper.Options.DefDateFormat));
      item.SubItems.Add(TGenEngine.GEDCOMDateToStr(task.StopDate, fmGEDKeeper.Options.DefDateFormat));
    end;
    EndUpdate;
  end;

  with TGKListView(FCommunicationsList.List) do begin
    BeginUpdate;
    Items.Clear();
    for k := 0 to FResearch.CommunicationsCount - 1 do begin
      corr := TGEDCOMCommunicationRecord(FResearch.Communications[k].Value);

      item := AddItem(corr.Name, corr);
      item.SubItems.Add(TGenEngine.GetCorresponderStr(Base.Tree, corr, False));
      item.SubItems.Add(LSList[TGenEngine.CommunicationNames[corr.CommunicationType]]);
      item.SubItems.Add(TGenEngine.GEDCOMDateToStr(corr.Date, fmGEDKeeper.Options.DefDateFormat));
    end;
    EndUpdate;
  end;

  with TGKListView(FGroupsList.List) do begin
    BeginUpdate;
    Items.Clear();
    for k := 0 to FResearch.GroupsCount - 1 do begin
      grp := TGEDCOMGroupRecord(FResearch.Groups[k].Value);
      item := AddItem(grp.Name, grp);
    end;
    EndUpdate;
  end;
end;

procedure TfmResearchEdit.SetResearch(const Value: TGEDCOMResearchRecord);
begin
  FResearch := Value;

  try
    if (FResearch = nil) then begin
      EditName.Text := '';
      EditPriority.SelectedIndex := -1;
      EditStatus.SelectedIndex := -1;
      EditStartDate.Text := '';
      EditStopDate.Text := '';
      EditPercent.Text := '0';
    end else begin
      EditName.Text := FResearch.Name;
      EditPriority.SelectedIndex := Ord(FResearch.Priority);
      EditStatus.SelectedIndex := Ord(FResearch.Status);
      EditStartDate.Text := TGenEngine.GEDCOMDateToStr(FResearch.StartDate);
      EditStopDate.Text := TGenEngine.GEDCOMDateToStr(FResearch.StopDate);
      EditPercent.Text := FResearch.Percent.ToString();
    end;

    ListsRefresh();
  except
    on E: Exception do TGKUtils.LogWrite('ResearchEdit.SetResearch(): ' + E.Message);
  end;
end;

procedure TfmResearchEdit.AcceptChanges();
begin
  FResearch.Name := EditName.Text;
  FResearch.Priority := TResearchPriority(EditPriority.SelectedIndex);
  FResearch.Status := TResearchStatus(EditStatus.SelectedIndex);
  FResearch.StartDate.ParseString(TGenEngine.StrToGEDCOMDate(EditStartDate.Text));
  FResearch.StopDate.ParseString(TGenEngine.StrToGEDCOMDate(EditStopDate.Text));
  FResearch.Percent := Int32.Parse(EditPercent.Text);

  Base.ChangeRecord(FResearch);
end;

procedure TfmResearchEdit.btnAccept_Click(sender: System.Object; e: System.EventArgs);
begin
  try
    AcceptChanges();

    Self.DialogResult := System.Windows.Forms.DialogResult.OK;
  except
    Self.DialogResult := System.Windows.Forms.DialogResult.None;
  end;
end;

procedure TfmResearchEdit.ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
var
  task: TGEDCOMTaskRecord;
  comm: TGEDCOMCommunicationRecord;
  group: TGEDCOMGroupRecord;
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(Self, FResearch, TGEDCOMNotes(ItemData), Action)
    then ListsRefresh();
  end
  else
  if (Sender = FTasksList) then begin
    case Action of
      raAdd: begin
        task := TGEDCOMTaskRecord(Base.SelectRecord(rtTask, []));
        if Base.Engine.AddResearchTask(FResearch, task) then ListsRefresh();
      end;
      raEdit: begin
        task := TGEDCOMTaskRecord(ItemData);
        if (task <> nil) and Base.ModifyTask(task) then ListsRefresh();
      end;
      raDelete: begin
        task := TGEDCOMTaskRecord(ItemData);

        if (task = nil) or (TGKUtils.ShowQuestion(LSList[LSID_DetachTaskQuery]) = System.Windows.Forms.DialogResult.No)
        then Exit;

        Base.Engine.RemoveResearchTask(FResearch, task);

        ListsRefresh();
      end;
      raJump: begin
        task := TGEDCOMTaskRecord(ItemData);
        if (task <> nil) then begin
          AcceptChanges();
          Base.SelectRecordByXRef(task.XRef);
          Close;
        end;
      end;
    end;
  end
  else
  if (Sender = FCommunicationsList) then begin
    case Action of
      raAdd: begin
        comm := TGEDCOMCommunicationRecord(Base.SelectRecord(rtCommunication, []));
        if Base.Engine.AddResearchComm(FResearch, comm) then ListsRefresh();
      end;
      raEdit: begin
        comm := TGEDCOMCommunicationRecord(ItemData);
        if (comm <> nil) and Base.ModifyCommunication(comm) then ListsRefresh();
      end;
      raDelete: begin
        comm := TGEDCOMCommunicationRecord(ItemData);

        if (comm = nil) or (TGKUtils.ShowQuestion(LSList[LSID_DetachCommunicationQuery]) = System.Windows.Forms.DialogResult.No)
        then Exit;

        Base.Engine.RemoveResearchComm(FResearch, comm);

        ListsRefresh();
      end;
      raJump: begin
        comm := TGEDCOMCommunicationRecord(ItemData);
        if (comm <> nil) then begin
          AcceptChanges();
          Base.SelectRecordByXRef(comm.XRef);
          Close;
        end;
      end;
    end;
  end
  else
  if (Sender = FGroupsList) then begin
    case Action of
      raAdd: begin
        group := TGEDCOMGroupRecord(Base.SelectRecord(rtGroup, []));
        if Base.Engine.AddResearchGroup(FResearch, group) then ListsRefresh();
      end;
      raEdit: ;
      raDelete: begin
        group := TGEDCOMGroupRecord(ItemData);

        if (group = nil) or (TGKUtils.ShowQuestion(LSList[LSID_DetachGroupQuery]) = System.Windows.Forms.DialogResult.No)
        then Exit;

        Base.Engine.RemoveResearchGroup(FResearch, group);

        ListsRefresh();
      end;
      raJump: begin
        group := TGEDCOMGroupRecord(ItemData);
        if (group <> nil) then begin
          AcceptChanges();
          Base.SelectRecordByXRef(group.XRef);
          Close;
        end;
      end;
    end;
  end;
end;

end.
