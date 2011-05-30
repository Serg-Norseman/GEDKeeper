unit GKResearchEdit; {prepare:fin; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Buttons, ComCtrls,
  ExtCtrls, GedCom551, GKBase, GKEngine, Mask, GKCtrls, GKLists, GKLangs;

type
  TfmResearchEdit = class(TForm, ILocalization)
    GroupBox1: TGroupBox;
    EditName: TEdit;
    Label1: TLabel;
    PagesGroupData: TPageControl;
    SheetNotes: TTabSheet;
    SheetTasks: TTabSheet;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    Label2: TLabel;
    EditPriority: TComboBox;
    SheetCommunications: TTabSheet;
    Label3: TLabel;
    EditStatus: TComboBox;
    Label4: TLabel;
    EditStartDate: TMaskEdit;
    Label5: TLabel;
    EditStopDate: TMaskEdit;
    Label6: TLabel;
    EditPercent: TEdit;
    UpDown1: TUpDown;
    SheetGroups: TTabSheet;
    procedure btnAcceptClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FResearch: TGEDCOMResearchRecord;

    FTasksList: TSheetList;
    FCommunicationsList: TSheetList;
    FGroupsList: TSheetList;
    FNotesList: TSheetList;

    procedure AcceptChanges();
    procedure SetResearch(const Value: TGEDCOMResearchRecord);
    procedure ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
    procedure ListsRefresh();
    function GetBase: TfmBase;
  public
    property Base: TfmBase read GetBase;
    property Research: TGEDCOMResearchRecord read FResearch write SetResearch;

    procedure SetLang();
  end;

implementation

uses
  GKUtils, GKMain, GKRecordSelect, GKPersonEdit;

{$R *.dfm}

procedure TfmResearchEdit.FormCreate(Sender: TObject);
var
  rp: TResearchPriority;
  rs: TResearchStatus;
begin
  for rp := Low(TResearchPriority) to High(TResearchPriority) do
    EditPriority.Items.Add(LSList[PriorityNames[rp]]);

  for rs := Low(TResearchStatus) to High(TResearchStatus) do
    EditStatus.Items.Add(LSList[StatusNames[rs]]);

  FTasksList := TSheetList.Create(SheetTasks);
  FTasksList.OnModify := ListModify;
  FTasksList.Buttons := [lbAdd..lbJump];
  AddListColumn(FTasksList.List, LSList[LSID_Goal], 250);
  AddListColumn(FTasksList.List, LSList[LSID_Priority], 90);
  AddListColumn(FTasksList.List, LSList[LSID_StartDate], 90);
  AddListColumn(FTasksList.List, LSList[LSID_StopDate], 90);

  FCommunicationsList := TSheetList.Create(SheetCommunications);
  FCommunicationsList.OnModify := ListModify;
  FCommunicationsList.Buttons := [lbAdd..lbJump];
  AddListColumn(FCommunicationsList.List, LSList[LSID_Theme], 150);
  AddListColumn(FCommunicationsList.List, LSList[LSID_Corresponder], 150);
  AddListColumn(FCommunicationsList.List, LSList[LSID_Type], 90);
  AddListColumn(FCommunicationsList.List, LSList[LSID_Date], 90);

  FGroupsList := TSheetList.Create(SheetGroups);
  FGroupsList.OnModify := ListModify;
  FGroupsList.Buttons := [lbAdd..lbJump];
  AddListColumn(FGroupsList.List, LSList[LSID_Group], 350);

  FNotesList := TSheetList.Create(SheetNotes, lmBox);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  SetLang();
end;

procedure TfmResearchEdit.SetLang();
begin
  btnAccept.Caption := LSList[LSID_DlgAccept];
  btnCancel.Caption := LSList[LSID_DlgCancel];

  Caption := LSList[LSID_WinResearchEdit];

  SheetTasks.Caption := LSList[LSID_RPTasks];
  SheetCommunications.Caption := LSList[LSID_RPCommunications];
  SheetGroups.Caption := LSList[LSID_RPGroups];
  SheetNotes.Caption := LSList[LSID_RPNotes];

  Label1.Caption := LSList[LSID_Title];
  Label2.Caption := LSList[LSID_Priority];
  Label3.Caption := LSList[LSID_Status];
  Label6.Caption := LSList[LSID_Percent];
  Label4.Caption := LSList[LSID_StartDate];
  Label5.Caption := LSList[LSID_StopDate];
end;

procedure TfmResearchEdit.ListsRefresh();
var
  k: Integer;
  task: TGEDCOMTaskRecord;
  corr: TGEDCOMCommunicationRecord;
  grp: TGEDCOMGroupRecord;
  item: TListItem;
begin
  Base.RecListNotesRefresh(FResearch, FNotesList.List, nil);

  with TGKListView(FTasksList.List) do begin
    Items.BeginUpdate;
    Items.Clear();
    for k := 0 to FResearch.TasksCount - 1 do begin
      task := TGEDCOMTaskRecord(FResearch.Tasks[k].Value);

      item := Items.Add();
      item.Caption := GetTaskGoalStr(Base.Tree, task);
      item.SubItems.Add(LSList[PriorityNames[task.Priority]]);
      item.SubItems.Add(GEDCOMDateToStr(task.StartDate, fmGEDKeeper.Options.DefDateFormat));
      item.SubItems.Add(GEDCOMDateToStr(task.StopDate, fmGEDKeeper.Options.DefDateFormat));
      item.Data := task;
    end;
    Items.EndUpdate;
  end;

  with TGKListView(FCommunicationsList.List) do begin
    Items.BeginUpdate;
    Items.Clear();
    for k := 0 to FResearch.CommunicationsCount - 1 do begin
      corr := TGEDCOMCommunicationRecord(FResearch.Communications[k].Value);

      item := Items.Add();
      item.Caption := corr.Name;
      item.SubItems.Add(GetCorresponderStr(Base.Tree, corr, False));
      item.SubItems.Add(LSList[CommunicationNames[corr.CommunicationType]]);
      item.SubItems.Add(GEDCOMDateToStr(corr.Date, fmGEDKeeper.Options.DefDateFormat));
      item.Data := corr;
    end;
    Items.EndUpdate;
  end;

  with TGKListView(FGroupsList.List) do begin
    Items.BeginUpdate;
    Items.Clear();
    for k := 0 to FResearch.GroupsCount - 1 do begin
      grp := TGEDCOMGroupRecord(FResearch.Groups[k].Value);

      item := Items.Add();
      item.Caption := grp.Name;
      item.Data := grp;
    end;
    Items.EndUpdate;
  end;
end;

procedure TfmResearchEdit.SetResearch(const Value: TGEDCOMResearchRecord);
begin
  FResearch := Value;

  try
    if (FResearch = nil) then begin
      EditName.Text := '';
      EditPriority.ItemIndex := -1;
      EditStatus.ItemIndex := -1;
      EditStartDate.Text := '';
      EditStopDate.Text := '';
      EditPercent.Text := '0';
    end else begin
      EditName.Text := FResearch.Name;
      EditPriority.ItemIndex := Ord(FResearch.Priority);
      EditStatus.ItemIndex := Ord(FResearch.Status);
      EditStartDate.Text := GEDCOMDateToStr(FResearch.StartDate);
      EditStopDate.Text := GEDCOMDateToStr(FResearch.StopDate);
      EditPercent.Text := IntToStr(FResearch.Percent);
    end;

    ListsRefresh();
  except
    on E: Exception do LogWrite('ResearchEdit.SetResearch(): ' + E.Message);
  end;
end;

procedure TfmResearchEdit.AcceptChanges();
begin
  FResearch.Name := EditName.Text;
  FResearch.Priority := TResearchPriority(EditPriority.ItemIndex);
  FResearch.Status := TResearchStatus(EditStatus.ItemIndex);
  FResearch.StartDate.ParseString(StrToGEDCOMDate(EditStartDate.Text));
  FResearch.StopDate.ParseString(StrToGEDCOMDate(EditStopDate.Text));
  FResearch.Percent := StrToInt(EditPercent.Text);

  Base.ChangeRecord(FResearch);
end;

procedure TfmResearchEdit.btnAcceptClick(Sender: TObject);
begin
  AcceptChanges();
end;

function TfmResearchEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmResearchEdit.ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
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

        if (task = nil) or (MessageDlg(LSList[LSID_DetachTaskQuery], mtConfirmation, [mbNo, mbYes], 0) = mrNo)
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
        if Base.Engine.AddResearchComm(FResearch, comm)
        then ListsRefresh();
      end;
      raEdit: begin
        comm := TGEDCOMCommunicationRecord(ItemData);

        if (comm <> nil) and Base.ModifyCommunication(comm)
        then ListsRefresh();
      end;
      raDelete: begin
        comm := TGEDCOMCommunicationRecord(ItemData);

        if (comm = nil) or (MessageDlg(LSList[LSID_DetachCommunicationQuery], mtConfirmation, [mbNo, mbYes], 0) = mrNo)
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
        if Base.Engine.AddResearchGroup(FResearch, group)
        then ListsRefresh();
      end;
      raEdit: ;
      raDelete: begin
        group := TGEDCOMGroupRecord(ItemData);

        if (MessageDlg(LSList[LSID_DetachGroupQuery], mtConfirmation, [mbNo, mbYes], 0) = mrNo)
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
