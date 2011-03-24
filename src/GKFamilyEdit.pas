unit GKFamilyEdit; {prepare:fin}

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Buttons, ComCtrls,
  ExtCtrls, GedCom551, GKBase, GKEngine, GKCtrls, GKLists;

type
  TfmFamilyEdit = class(TForm)
    PagesFamilyData: TPageControl;
    SheetEvents: TTabSheet;
    SheetNotes: TTabSheet;
    SheetMultimedia: TTabSheet;
    SheetSources: TTabSheet;
    SheetChilds: TTabSheet;                         
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    EditHusband: TEdit;
    btnHusbandAdd: TSpeedButton;
    btnHusbandDelete: TSpeedButton;
    btnHusbandSel: TSpeedButton;
    btnWifeSel: TSpeedButton;
    btnWifeDelete: TSpeedButton;
    btnWifeAdd: TSpeedButton;
    EditWife: TEdit;
    Label2: TLabel;
    Label6: TLabel;
    EditMarriageStatus: TComboBox;
    Label5: TLabel;
    cbRestriction: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure btnHusbandAddClick(Sender: TObject);
    procedure btnHusbandDeleteClick(Sender: TObject);
    procedure btnHusbandSelClick(Sender: TObject);
    procedure btnWifeAddClick(Sender: TObject);
    procedure btnWifeDeleteClick(Sender: TObject);
    procedure btnWifeSelClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure EditHusbandChange(Sender: TObject);
    procedure EditWifeChange(Sender: TObject);
    procedure cbRestrictionChange(Sender: TObject);
  private
    FFamily: TGEDCOMFamilyRecord;

    FChildsList: TSheetList;
    FEventsList: TSheetList;
    FNotesList: TSheetList;
    FMediaList: TSheetList;
    FSourcesList: TSheetList;

    procedure AcceptChanges();
    function  GetHusband(): TGEDCOMIndividualRecord;
    function  GetWife(): TGEDCOMIndividualRecord;
    procedure SetFamily(const Value: TGEDCOMFamilyRecord);
    procedure ControlsRefresh();
    function GetBase: TfmBase;
    procedure SetTitle();
    procedure ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
  public
    property Base: TfmBase read GetBase;
    property Family: TGEDCOMFamilyRecord read FFamily write SetFamily;
  end;

implementation

uses GKUtils, GKRecordSelect, GKPersonEdit, GKMain;

{$R *.dfm}

{ TfmFamilyEdit }

procedure TfmFamilyEdit.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to MarriageStatusSize - 1 do
    EditMarriageStatus.Items.Add(MarriageStatus[i].Name);

  //

  FChildsList := TSheetList.Create(SheetChilds);
  FChildsList.OnModify := ListModify;
  FChildsList.Buttons := [lbAdd..lbJump];
  AddListColumn(FChildsList.List, '№', 25);
  AddListColumn(FChildsList.List, 'Имя ребенка', 300);
  AddListColumn(FChildsList.List, 'Дата рождения', 100);

  FEventsList := TSheetList.Create(SheetEvents);
  FEventsList.OnModify := ListModify;
  Base.SetupRecEventsList(FEventsList, False);

  FNotesList := TSheetList.Create(SheetNotes, lmBox);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  FMediaList := TSheetList.Create(SheetMultimedia);
  FMediaList.OnModify := ListModify;
  Base.SetupRecMediaList(FMediaList);

  FSourcesList := TSheetList.Create(SheetSources);
  FSourcesList.OnModify := ListModify;
  Base.SetupRecSourcesList(FSourcesList);
end;

procedure TfmFamilyEdit.ControlsRefresh();
var
  k: Integer;
  spouse, child: TGEDCOMIndividualRecord;
  item: TListItem;
begin
  spouse := GetHusband();
  if (spouse <> nil)
  then EditHusband.Text := GetNameStr(spouse)
  else EditHusband.Text := UnkMale;

  btnHusbandAdd.Enabled := (spouse = nil);
  btnHusbandDelete.Enabled := (spouse <> nil);
  btnHusbandSel.Enabled := (spouse <> nil);

  spouse := GetWife();
  if (spouse <> nil)
  then EditWife.Text := GetNameStr(spouse)
  else EditWife.Text := UnkFemale;

  btnWifeAdd.Enabled := (spouse = nil);
  btnWifeDelete.Enabled := (spouse <> nil);
  btnWifeSel.Enabled := (spouse <> nil);

  Base.RecListFamilyEventsRefresh(FFamily, TGKListView(FEventsList.List), nil);
  Base.RecListNotesRefresh(FFamily, FNotesList.List, nil);
  Base.RecListMediaRefresh(FFamily, TGKListView(FMediaList.List), nil);
  Base.RecListSourcesRefresh(FFamily, TGKListView(FSourcesList.List), nil);

  with TGKListView(FChildsList.List) do begin
    Items.BeginUpdate();
    Items.Clear();
    for k := 0 to FFamily.ChildrenCount - 1 do begin
      child := TGEDCOMIndividualRecord(FFamily.Children[k].Value);

      item := Items.Add();
      item.Caption := IntToStr(k + 1);
      item.SubItems.Add(GetNameStr(child));
      item.SubItems.Add(GetBirthDate(child, fmGEDKeeper.Options.DefDateFormat));
      item.Data := child;
    end;
    Items.EndUpdate();
  end;

  btnHusbandAdd.Enabled := btnHusbandAdd.Enabled and (FFamily.Restriction <> rnLocked);
  btnHusbandDelete.Enabled := btnHusbandDelete.Enabled and (FFamily.Restriction <> rnLocked);
  btnWifeAdd.Enabled := btnWifeAdd.Enabled and (FFamily.Restriction <> rnLocked);
  btnWifeDelete.Enabled := btnWifeDelete.Enabled and (FFamily.Restriction <> rnLocked);
  EditMarriageStatus.Enabled := EditMarriageStatus.Enabled and (FFamily.Restriction <> rnLocked);

  FChildsList.ReadOnly := (FFamily.Restriction = rnLocked);
  FEventsList.ReadOnly := (FFamily.Restriction = rnLocked);
  FNotesList.ReadOnly := (FFamily.Restriction = rnLocked);
  FMediaList.ReadOnly := (FFamily.Restriction = rnLocked);
  FSourcesList.ReadOnly := (FFamily.Restriction = rnLocked);
end;

procedure TfmFamilyEdit.SetFamily(const Value: TGEDCOMFamilyRecord);
var
  stat: string;
  stat_idx: Integer;
begin
  FFamily := Value;

  try
    if (FFamily = nil) then begin
      btnHusbandSel.Enabled := False;
      btnWifeSel.Enabled := False;

      EditMarriageStatus.Enabled := False;
      EditMarriageStatus.ItemIndex := 0;

      cbRestriction.ItemIndex := 0;
    end else begin
      stat := FFamily.GetTagStringValue('_STAT');
      stat_idx := GetMarriageStatusIndex(stat);
      EditMarriageStatus.Enabled := True;
      EditMarriageStatus.ItemIndex := stat_idx;

      cbRestriction.ItemIndex := Ord(FFamily.Restriction);

      ControlsRefresh();
    end;
  except
    on E: Exception do LogWrite('FamilyEdit.SetFamily(): ' + E.Message);
  end;
end;

function TfmFamilyEdit.GetHusband(): TGEDCOMIndividualRecord;
begin
  Result := TGEDCOMIndividualRecord(FFamily.Husband.Value);
end;

function TfmFamilyEdit.GetWife(): TGEDCOMIndividualRecord;
begin
  Result := TGEDCOMIndividualRecord(FFamily.Wife.Value);
end;

procedure TfmFamilyEdit.btnHusbandAddClick(Sender: TObject);
var
  husband: TGEDCOMIndividualRecord;
begin
  husband := Base.SelectPerson(nil, tmNone, svMale);
  if (husband <> nil) and (FFamily.Husband.StringValue = '') then begin
    Base.Engine.AddFamilySpouse(FFamily, husband);
    ControlsRefresh();
  end;
end;

procedure TfmFamilyEdit.btnHusbandDeleteClick(Sender: TObject);
begin
  if (MessageDlg('Удалить ссылку на мужа?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  Base.Engine.RemoveFamilySpouse(FFamily, GetHusband());
  ControlsRefresh();
end;

procedure TfmFamilyEdit.btnHusbandSelClick(Sender: TObject);
var
  spouse: TGEDCOMIndividualRecord;
begin
  spouse := GetHusband();
  if (spouse <> nil) then begin
    AcceptChanges();
    Base.SelectRecordByXRef(spouse.XRef);
    Close;
  end;
end;

procedure TfmFamilyEdit.btnWifeAddClick(Sender: TObject);
var
  wife: TGEDCOMIndividualRecord;
begin
  wife := Base.SelectPerson(nil, tmNone, svFemale);
  if (wife <> nil) and (FFamily.Wife.StringValue = '') then begin
    Base.Engine.AddFamilySpouse(FFamily, wife);
    ControlsRefresh();
  end;
end;

procedure TfmFamilyEdit.btnWifeDeleteClick(Sender: TObject);
begin
  if (MessageDlg('Удалить ссылку на жену?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  Base.Engine.RemoveFamilySpouse(FFamily, GetWife());
  ControlsRefresh();
end;

procedure TfmFamilyEdit.btnWifeSelClick(Sender: TObject);
var
  spouse: TGEDCOMIndividualRecord;
begin
  spouse := GetWife();
  if (spouse <> nil) then begin
    AcceptChanges();
    Base.SelectRecordByXRef(spouse.XRef);
    Close;
  end;
end;

procedure TfmFamilyEdit.AcceptChanges();
var
  stat: string;
begin
  stat := MarriageStatus[EditMarriageStatus.ItemIndex].StatSign;
  FFamily.SetTagStringValue('_STAT', stat);

  FFamily.Restriction := TGEDCOMRestriction(cbRestriction.ItemIndex);
  FFamily.SortChilds();

  Base.ChangeRecord(FFamily);
end;

procedure TfmFamilyEdit.btnAcceptClick(Sender: TObject);
begin
  AcceptChanges();
end;

function TfmFamilyEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmFamilyEdit.SetTitle();
begin
  Caption := 'Семья "'+EditHusband.Text+' - '+EditWife.Text+'"';
end;

procedure TfmFamilyEdit.EditHusbandChange(Sender: TObject);
begin
  SetTitle();
end;

procedure TfmFamilyEdit.EditWifeChange(Sender: TObject);
begin
  SetTitle();
end;

procedure TfmFamilyEdit.ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
var
  child: TGEDCOMIndividualRecord;
begin
  if (Sender = FChildsList) then begin
    case Action of
      raAdd: begin
        child := Base.SelectPerson(GetHusband(), tmAncestor, svNone);
        if (child <> nil) and Base.Engine.AddFamilyChild(FFamily, child)
        then ControlsRefresh();
      end;

      raEdit: begin
        child := TGEDCOMIndividualRecord(ItemData);

        if Base.ModifyPerson(child)
        then ControlsRefresh();
      end;

      raDelete: begin
        child := TGEDCOMIndividualRecord(ItemData);

        if (child = nil) or (MessageDlg('Удалить ссылку на ребенка?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
        then Exit;

        if Base.Engine.RemoveFamilyChild(FFamily, child)
        then ControlsRefresh();
      end;

      raJump: begin
        child := TGEDCOMIndividualRecord(ItemData);
        if (child <> nil) then begin
          AcceptChanges();
          Base.SelectRecordByXRef(child.XRef);
          Close;
        end;
      end;
    end;
  end
  else
  if (Sender = FEventsList) then begin
    if Base.ModifyRecEvent(Self, FFamily, TGEDCOMCustomEvent(ItemData), Action)
    then ControlsRefresh();
  end
  else
  //
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(Self, FFamily, TGEDCOMNotes(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FMediaList) then begin
    if Base.ModifyRecMultimedia(Self, FFamily, TGEDCOMMultimediaLink(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FSourcesList) then begin
    if Base.ModifyRecSource(Self, FFamily, TGEDCOMSourceCitation(ItemData), Action)
    then ControlsRefresh();
  end;
end;

procedure TfmFamilyEdit.cbRestrictionChange(Sender: TObject);
begin
  ControlsRefresh();
end;

end.
