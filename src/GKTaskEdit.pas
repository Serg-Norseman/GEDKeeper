unit GKTaskEdit;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons,
  ComCtrls, ExtCtrls, GedCom551, GKBase, GKCommon, Mask, GKSheetList;

type
  TfmTaskEdit = class(TForm)
    GroupBox1: TGroupBox;
    PagesGroupData: TPageControl;
    SheetNotes: TTabSheet;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    Label2: TLabel;
    EditPriority: TComboBox;
    Label4: TLabel;
    EditStartDate: TMaskEdit;
    EditStopDate: TMaskEdit;
    Label5: TLabel;
    Label1: TLabel;
    cbGoalType: TComboBox;
    EditGoal: TEdit;
    btnGoalSelect: TSpeedButton;
    procedure btnAcceptClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnGoalSelectClick(Sender: TObject);
    procedure cbGoalTypeChange(Sender: TObject);
  private
    FTask: TGEDCOMTaskRecord;
    FTempRec: TGEDCOMRecord;

    FNotesList: TSheetList;

    procedure SetTask(const Value: TGEDCOMTaskRecord);
    procedure ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
    procedure ListsRefresh();
    function GetBase: TfmBase;
  public
    property Base: TfmBase read GetBase;
    property Task: TGEDCOMTaskRecord read FTask write SetTask;
  end;

implementation

uses
  bsComUtils, GKMain, GKRecordSelect, GKPersonEdit;

{$R *.dfm}

{ TfmTaskEdit }

procedure TfmTaskEdit.FormCreate(Sender: TObject);
var
  rp: TResearchPriority;
  gt: TGoalType;
begin
  for rp := Low(TResearchPriority) to High(TResearchPriority) do
    EditPriority.Items.Add(PriorityNames[rp]);

  for gt := Low(TGoalType) to High(TGoalType) do
    cbGoalType.Items.Add(GoalNames[gt]);

  FNotesList := TSheetList.Create(SheetNotes, lmBox);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  FTempRec := nil;
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
      EditPriority.ItemIndex := -1;
      EditStartDate.Text := '';
      EditStopDate.Text := '';
      cbGoalType.ItemIndex := 0;
      EditGoal.Text := '';
    end else begin
      EditPriority.ItemIndex := Ord(FTask.Priority);
      EditStartDate.Text := GEDCOMDateToStr(TGEDCOMDate(FTask.StartDate));
      EditStopDate.Text := GEDCOMDateToStr(TGEDCOMDate(FTask.StopDate));

      GetTaskGoal(Base.Tree, FTask, gt, FTempRec);
      cbGoalType.ItemIndex := Ord(gt);
      case gt of
        gtIndividual: EditGoal.Text := GetNameStr((FTempRec as TGEDCOMIndividualRecord));
        gtFamily: EditGoal.Text := GetFamilyStr((FTempRec as TGEDCOMFamilyRecord));
        gtSource: EditGoal.Text := (FTempRec as TGEDCOMSourceRecord).FiledByEntry;
        gtOther: EditGoal.Text := FTask.Goal;
      end;
    end;

    cbGoalTypeChange(nil);

    ListsRefresh();
  except
    on E: Exception do LogWrite('TaskEdit.SetTask(): ' + E.Message);
  end;
end;

procedure TfmTaskEdit.btnAcceptClick(Sender: TObject);
var
  gt: TGoalType;
begin
  FTask.Priority := TResearchPriority(EditPriority.ItemIndex);
  FTask.StartDate.ParseString(StrToGEDCOMDate(EditStartDate.Text));
  FTask.StopDate.ParseString(StrToGEDCOMDate(EditStopDate.Text));

  gt := TGoalType(cbGoalType.ItemIndex);
  case gt of
    gtIndividual, gtFamily, gtSource: FTask.Goal := EncloseXRef(FTempRec.XRef);
    gtOther: FTask.Goal := EditGoal.Text;
  end;

  Base.ChangeRecord(FTask);
end;

function TfmTaskEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmTaskEdit.ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(FTask, TGEDCOMNotes(ItemData), Action)
    then ListsRefresh();
  end;
end;

procedure TfmTaskEdit.btnGoalSelectClick(Sender: TObject);
begin
  case TGoalType(cbGoalType.ItemIndex) of
    gtIndividual: begin
      FTempRec := Base.SelectPerson(nil, tmNone, svNone);
      EditGoal.Text := GetNameStr((FTempRec as TGEDCOMIndividualRecord));
    end;

    gtFamily: begin
      FTempRec := Base.SelectRecord(smFamily);
      EditGoal.Text := GetFamilyStr(FTempRec as TGEDCOMFamilyRecord);
    end;

    gtSource: begin
      FTempRec := Base.SelectRecord(smSource);
      EditGoal.Text := (FTempRec as TGEDCOMSourceRecord).FiledByEntry;
    end;

    gtOther: begin
      //
    end;
  end;
end;

procedure TfmTaskEdit.cbGoalTypeChange(Sender: TObject);
begin
  case TGoalType(cbGoalType.ItemIndex) of
    gtIndividual: begin
      btnGoalSelect.Enabled := True;
      EditGoal.Color := clBtnFace;
      EditGoal.ReadOnly := True;
    end;

    gtFamily: begin
      btnGoalSelect.Enabled := True;
      EditGoal.Color := clBtnFace;
      EditGoal.ReadOnly := True;
    end;

    gtSource: begin
      btnGoalSelect.Enabled := True;
      EditGoal.Color := clBtnFace;
      EditGoal.ReadOnly := True;
    end;

    gtOther: begin
      btnGoalSelect.Enabled := False;
      EditGoal.Color := clWindow;
      EditGoal.ReadOnly := False;
    end;
  end;
end;

end.
