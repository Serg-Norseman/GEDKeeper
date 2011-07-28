unit GKTaskEdit; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms,
  GedCom551, GKBase, GKEngine, GKLists, GKUtils, GKLangs;

type
  TfmTaskEdit = class(System.Windows.Forms.Form)
  strict private
    GroupBox1: System.Windows.Forms.GroupBox;
    PagesGroupData: System.Windows.Forms.TabControl;
    SheetNotes: System.Windows.Forms.TabPage;
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    Label2: System.Windows.Forms.Label;
    EditPriority: System.Windows.Forms.ComboBox;
    Label4: System.Windows.Forms.Label;
    EditStartDate: System.Windows.Forms.TextBox;
    EditStopDate: System.Windows.Forms.TextBox;
    Label5: System.Windows.Forms.Label;
    Label1: System.Windows.Forms.Label;
    cbGoalType: System.Windows.Forms.ComboBox;
    EditGoal: System.Windows.Forms.TextBox;
    btnGoalSelect: System.Windows.Forms.Button;

    FBase: TfmBase;
    FTask: TGEDCOMTaskRecord;
    FTempRec: TGEDCOMRecord;

    FNotesList: TSheetList;

    procedure SetTask(const Value: TGEDCOMTaskRecord);
    procedure ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
    procedure ListsRefresh();

    procedure InitializeComponent;
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
    procedure btnGoalSelect_Click(sender: System.Object; e: System.EventArgs);
    procedure cbGoalType_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property Task: TGEDCOMTaskRecord read FTask write SetTask;
  end;

implementation

procedure TfmTaskEdit.InitializeComponent;
begin
  Self.GroupBox1 := System.Windows.Forms.GroupBox.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.Label4 := System.Windows.Forms.Label.Create;
  Self.Label5 := System.Windows.Forms.Label.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.btnGoalSelect := System.Windows.Forms.Button.Create;
  Self.EditPriority := System.Windows.Forms.ComboBox.Create;
  Self.EditStartDate := System.Windows.Forms.TextBox.Create;
  Self.EditStopDate := System.Windows.Forms.TextBox.Create;
  Self.cbGoalType := System.Windows.Forms.ComboBox.Create;
  Self.EditGoal := System.Windows.Forms.TextBox.Create;
  Self.PagesGroupData := System.Windows.Forms.TabControl.Create;
  Self.SheetNotes := System.Windows.Forms.TabPage.Create;
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.GroupBox1.SuspendLayout;
  Self.PagesGroupData.SuspendLayout;
  Self.SuspendLayout;
  // 
  // GroupBox1
  // 
  Self.GroupBox1.Controls.Add(Self.Label2);
  Self.GroupBox1.Controls.Add(Self.Label4);
  Self.GroupBox1.Controls.Add(Self.Label5);
  Self.GroupBox1.Controls.Add(Self.Label1);
  Self.GroupBox1.Controls.Add(Self.btnGoalSelect);
  Self.GroupBox1.Controls.Add(Self.EditPriority);
  Self.GroupBox1.Controls.Add(Self.EditStartDate);
  Self.GroupBox1.Controls.Add(Self.EditStopDate);
  Self.GroupBox1.Controls.Add(Self.cbGoalType);
  Self.GroupBox1.Controls.Add(Self.EditGoal);
  Self.GroupBox1.Location := System.Drawing.Point.Create(0, 0);
  Self.GroupBox1.Name := 'GroupBox1';
  Self.GroupBox1.Size := System.Drawing.Size.Create(481, 97);
  Self.GroupBox1.TabIndex := 1;
  Self.GroupBox1.TabStop := False;
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(8, 48);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(60, 13);
  Self.Label2.TabIndex := 0;
  Self.Label2.Text := 'Приоритет';
  // 
  // Label4
  // 
  Self.Label4.Location := System.Drawing.Point.Create(8, 72);
  Self.Label4.Name := 'Label4';
  Self.Label4.Size := System.Drawing.Size.Create(60, 13);
  Self.Label4.TabIndex := 1;
  Self.Label4.Text := 'Запущено';
  // 
  // Label5
  // 
  Self.Label5.Location := System.Drawing.Point.Create(248, 72);
  Self.Label5.Name := 'Label5';
  Self.Label5.Size := System.Drawing.Size.Create(64, 13);
  Self.Label5.TabIndex := 2;
  Self.Label5.Text := 'Завершено';
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 24);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(30, 13);
  Self.Label1.TabIndex := 3;
  Self.Label1.Text := 'Цель';
  // 
  // btnGoalSelect
  // 
  Self.btnGoalSelect.AccessibleDescription := 'Выбрать запись цели';
  Self.btnGoalSelect.Location := System.Drawing.Point.Create(448, 14);
  Self.btnGoalSelect.Name := 'btnGoalSelect';
  Self.btnGoalSelect.Size := System.Drawing.Size.Create(26, 26);
  Self.btnGoalSelect.TabIndex := 4;
  Include(Self.btnGoalSelect.Click, Self.btnGoalSelect_Click);
  // 
  // EditPriority
  // 
  Self.EditPriority.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.EditPriority.Location := System.Drawing.Point.Create(72, 40);
  Self.EditPriority.Name := 'EditPriority';
  Self.EditPriority.Size := System.Drawing.Size.Create(161, 21);
  Self.EditPriority.TabIndex := 2;
  // 
  // EditStartDate
  // 
  Self.EditStartDate.Location := System.Drawing.Point.Create(72, 64);
  Self.EditStartDate.MaxLength := 10;
  Self.EditStartDate.Name := 'EditStartDate';
  Self.EditStartDate.Size := System.Drawing.Size.Create(161, 21);
  Self.EditStartDate.TabIndex := 3;
  Self.EditStartDate.Text := '  .  .    ';
  // 
  // EditStopDate
  // 
  Self.EditStopDate.Location := System.Drawing.Point.Create(312, 64);
  Self.EditStopDate.MaxLength := 10;
  Self.EditStopDate.Name := 'EditStopDate';
  Self.EditStopDate.Size := System.Drawing.Size.Create(161, 21);
  Self.EditStopDate.TabIndex := 4;
  Self.EditStopDate.Text := '  .  .    ';
  // 
  // cbGoalType
  // 
  Self.cbGoalType.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.cbGoalType.Location := System.Drawing.Point.Create(72, 16);
  Self.cbGoalType.Name := 'cbGoalType';
  Self.cbGoalType.Size := System.Drawing.Size.Create(113, 21);
  Self.cbGoalType.TabIndex := 0;
  Include(Self.cbGoalType.SelectedIndexChanged, Self.cbGoalType_SelectedIndexChanged);
  // 
  // EditGoal
  // 
  Self.EditGoal.Location := System.Drawing.Point.Create(192, 16);
  Self.EditGoal.Name := 'EditGoal';
  Self.EditGoal.ReadOnly := True;
  Self.EditGoal.Size := System.Drawing.Size.Create(249, 21);
  Self.EditGoal.TabIndex := 1;
  Self.EditGoal.Text := '';
  // 
  // PagesGroupData
  // 
  Self.PagesGroupData.Controls.Add(Self.SheetNotes);
  Self.PagesGroupData.Location := System.Drawing.Point.Create(0, 97);
  Self.PagesGroupData.Name := 'PagesGroupData';
  Self.PagesGroupData.SelectedIndex := 0;
  Self.PagesGroupData.Size := System.Drawing.Size.Create(481, 256);
  Self.PagesGroupData.TabIndex := 0;
  // 
  // SheetNotes
  // 
  Self.SheetNotes.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetNotes.Name := 'SheetNotes';
  Self.SheetNotes.Size := System.Drawing.Size.Create(473, 230);
  Self.SheetNotes.TabIndex := 0;
  Self.SheetNotes.Text := 'Заметки';
  // 
  // btnAccept
  // 
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(304, 368);
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
  Self.btnCancel.Location := System.Drawing.Point.Create(392, 368);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 3;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // TfmTaskEdit
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(481, 402);
  Self.Controls.Add(Self.GroupBox1);
  Self.Controls.Add(Self.PagesGroupData);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmTaskEdit';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Редактирование задачи';
  Self.GroupBox1.ResumeLayout(False);
  Self.PagesGroupData.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmTaskEdit.Create(aBase: TfmBase);
var
  rp: TResearchPriority;
  gt: TGoalType;
begin
  inherited Create;
  InitializeComponent;

  //Self.EditStartDate.EditMask = '!99/99/9999;1;_';
  //Self.EditStopDate.EditMask := '!99/99/9999;1;_';

  for rp := Low(TResearchPriority) to High(TResearchPriority) do
    EditPriority.Items.Add(LSList[TGenEngine.PriorityNames[rp]]);

  for gt := Low(TGoalType) to High(TGoalType) do
    cbGoalType.Items.Add(LSList[TGenEngine.GoalNames[gt]]);

  FNotesList := TSheetList.Create(SheetNotes);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  FTempRec := nil;

  // SetLang
  Text := LSList[LSID_WinTaskEdit];
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];
  SheetNotes.Text := LSList[LSID_RPNotes];
  Label1.Text := LSList[LSID_Goal];
  Label2.Text := LSList[LSID_Priority];
  Label4.Text := LSList[LSID_StartDate];
  Label5.Text := LSList[LSID_StopDate];
end;

procedure TfmTaskEdit.ListsRefresh();
begin
  Base.RecListNotesRefresh(FTask, FNotesList.List, nil);
end;

procedure TfmTaskEdit.SetTask(const Value: TGEDCOMTaskRecord);
var
  gt: TGoalType;
begin
  FTask := Value;

  try
    if (FTask = nil) then begin
      EditPriority.SelectedIndex := -1;
      EditStartDate.Text := '';
      EditStopDate.Text := '';
      cbGoalType.SelectedIndex := 0;
      EditGoal.Text := '';
    end else begin
      EditPriority.SelectedIndex := Ord(FTask.Priority);
      EditStartDate.Text := TGenEngine.GEDCOMDateToStr(TGEDCOMDate(FTask.StartDate));
      EditStopDate.Text := TGenEngine.GEDCOMDateToStr(TGEDCOMDate(FTask.StopDate));

      TGenEngine.GetTaskGoal(Base.Tree, FTask, gt, FTempRec);
      cbGoalType.SelectedIndex := Ord(gt);
      case gt of
        gtIndividual: EditGoal.Text := TGenEngine.GetNameStr((FTempRec as TGEDCOMIndividualRecord));
        gtFamily: EditGoal.Text := TGenEngine.GetFamilyStr((FTempRec as TGEDCOMFamilyRecord));
        gtSource: EditGoal.Text := (FTempRec as TGEDCOMSourceRecord).FiledByEntry;
        gtOther: EditGoal.Text := FTask.Goal;
      end;
    end;

    cbGoalType_SelectedIndexChanged(nil, nil);

    ListsRefresh();
  except
    on E: Exception do TGKUtils.LogWrite('TaskEdit.SetTask(): ' + E.Message);
  end;
end;

procedure TfmTaskEdit.btnAccept_Click(sender: System.Object; e: System.EventArgs);
var
  gt: TGoalType;
begin
  FTask.Priority := TResearchPriority(EditPriority.SelectedIndex);
  FTask.StartDate.ParseString(TGenEngine.StrToGEDCOMDate(EditStartDate.Text));
  FTask.StopDate.ParseString(TGenEngine.StrToGEDCOMDate(EditStopDate.Text));

  gt := TGoalType(cbGoalType.SelectedIndex);
  case gt of
    gtIndividual, gtFamily, gtSource: FTask.Goal := TGEDCOMObject.EncloseXRef(FTempRec.XRef);
    gtOther: FTask.Goal := EditGoal.Text;
  end;

  Base.ChangeRecord(FTask);

  Self.DialogResult := System.Windows.Forms.DialogResult.OK;
end;

procedure TfmTaskEdit.ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(Self, FTask, TGEDCOMNotes(ItemData), Action)
    then ListsRefresh();
  end;
end;

procedure TfmTaskEdit.btnGoalSelect_Click(sender: System.Object; e: System.EventArgs);
begin
  case TGoalType(cbGoalType.SelectedIndex) of
    gtIndividual: begin
      FTempRec := Base.SelectPerson(nil, tmNone, svNone);
      EditGoal.Text := TGenEngine.GetNameStr((FTempRec as TGEDCOMIndividualRecord));
    end;

    gtFamily: begin
      FTempRec := Base.SelectRecord(rtFamily, []);
      EditGoal.Text := TGenEngine.GetFamilyStr(FTempRec as TGEDCOMFamilyRecord);
    end;

    gtSource: begin
      FTempRec := Base.SelectRecord(rtSource, []);
      EditGoal.Text := (FTempRec as TGEDCOMSourceRecord).FiledByEntry;
    end;

    gtOther: begin
      //
    end;
  end;
end;

procedure TfmTaskEdit.cbGoalType_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
begin
  case TGoalType(cbGoalType.SelectedIndex) of
    gtIndividual: begin
      btnGoalSelect.Enabled := True;
      EditGoal.ForeColor := System.Drawing.SystemColors.Control;
      EditGoal.ReadOnly := True;
    end;

    gtFamily: begin
      btnGoalSelect.Enabled := True;
      EditGoal.ForeColor := System.Drawing.SystemColors.Control;
      EditGoal.ReadOnly := True;
    end;

    gtSource: begin
      btnGoalSelect.Enabled := True;
      EditGoal.ForeColor := System.Drawing.SystemColors.Control;
      EditGoal.ReadOnly := True;
    end;

    gtOther: begin
      btnGoalSelect.Enabled := False;
      EditGoal.ForeColor := System.Drawing.SystemColors.Window;
      EditGoal.ReadOnly := False;
    end;
  end;
end;

end.
