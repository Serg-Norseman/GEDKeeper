unit GKFamilyEdit;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, Buttons, ComCtrls,
  ExtCtrls, GedCom551, GKBase, GKCommon;

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
    SheetCommon: TTabSheet;
    Label1: TLabel;
    EditHusband: TEdit;
    btnHusbandAdd: TSpeedButton;
    btnHusbandDelete: TSpeedButton;
    btnHusbandSel: TSpeedButton;
    Label2: TLabel;
    EditWife: TEdit;
    btnWifeAdd: TSpeedButton;
    btnWifeDelete: TSpeedButton;
    btnWifeSel: TSpeedButton;
    Label6: TLabel;
    EditMarriageStatus: TComboBox;
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
  private
    FFamily: TGEDCOMFamilyRecord;
    FTree: TGEDCOMTree;

    FChildsList: TSheetList;
    FEventsList: TSheetList;

    FNotesList: TSheetList;
    FMediaList: TSheetList;
    FSourcesList: TSheetList;

    function  GetHusband(): TGEDCOMIndividualRecord;
    function  GetWife(): TGEDCOMIndividualRecord;
    procedure SetFamily(const Value: TGEDCOMFamilyRecord);
    procedure ControlsRefresh();
    function GetBase: TfmBase;
    procedure SetTitle();
    procedure ListModify(Sender: TObject; Index: Integer; Action: TRecAction);
  public
    property Base: TfmBase read GetBase;
    property Family: TGEDCOMFamilyRecord read FFamily write SetFamily;
    property Tree: TGEDCOMTree read FTree write FTree;
  end;

implementation

uses bsComUtils, GKRecordSelect, GKPersonEdit, GKMain;

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
  Base.SetupRecChildsList(FChildsList.List);

  FEventsList := TSheetList.Create(SheetEvents);
  FEventsList.OnModify := ListModify;
  Base.SetupRecEventsList(FEventsList.List, False);

  FNotesList := TSheetList.Create(SheetNotes);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList.List);

  FMediaList := TSheetList.Create(SheetMultimedia);
  FMediaList.OnModify := ListModify;
  Base.SetupRecMediaList(FMediaList.List);

  FSourcesList := TSheetList.Create(SheetSources);
  FSourcesList.OnModify := ListModify;
  Base.SetupRecSourcesList(FSourcesList.List);
end;

procedure TfmFamilyEdit.ControlsRefresh();
var
  k: Integer;
  spouse, child: TGEDCOMIndividualRecord;
  item: TListItem;
begin
  spouse := TGEDCOMIndividualRecord(FFamily.Husband.Value);
  if (spouse <> nil)
  then EditHusband.Text := GetNameStr(spouse)
  else EditHusband.Text := UnkMale;

  btnHusbandAdd.Enabled := (spouse = nil);
  btnHusbandDelete.Enabled := (spouse <> nil);
  btnHusbandSel.Enabled := (spouse <> nil);

  spouse := TGEDCOMIndividualRecord(FFamily.Wife.Value);
  if (spouse <> nil)
  then EditWife.Text := GetNameStr(spouse)
  else EditWife.Text := UnkFemale;

  btnWifeAdd.Enabled := (spouse = nil);
  btnWifeDelete.Enabled := (spouse <> nil);
  btnWifeSel.Enabled := (spouse <> nil);

  Base.RecListFamilyEventsRefresh(FFamily, FEventsList.List, nil);
  Base.RecListNotesRefresh(FFamily, FNotesList.List, nil);
  Base.RecListMediaRefresh(FFamily, FMediaList.List, nil);
  Base.RecListSourcesRefresh(FFamily, FSourcesList.List, nil);

  FChildsList.List.Items.BeginUpdate();
  FChildsList.List.Clear();
  for k := 0 to FFamily.ChildrenCount - 1 do begin
    child := TGEDCOMIndividualRecord(FFamily.Children[k].Value);

    item := FChildsList.List.Items.Add();
    item.Caption := GetNameStr(child);
    item.SubItems.Add(GetBirthDate(child, fmGEDKeeper.Options.DefDateFormat));
    item.Data := TObject(k);
  end;
  FChildsList.List.Items.EndUpdate();
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
    end else begin
      stat := FFamily.TagStringValue('_STAT');
      stat_idx := GetMarriageStatusIndex(stat);
      EditMarriageStatus.Enabled := True;
      EditMarriageStatus.ItemIndex := stat_idx;

      ControlsRefresh();
    end;
  except
    on E: Exception do LogWrite('FamilyRefresh(): ' + E.Message);
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
  if (husband <> nil) then begin
    if (FFamily.Husband.StringValue = '') then begin
      FFamily.SetTagStringValue('HUSB', '@'+husband.XRef+'@');

      husband.AddSpouseToFamilyLink(
        TGEDCOMSpouseToFamilyLink.CreateTag(
          FTree, husband, 'FAMS', '@'+FFamily.XRef+'@'));

      ControlsRefresh();
    end;
  end;
end;

procedure TfmFamilyEdit.btnHusbandDeleteClick(Sender: TObject);
var
  husband: TGEDCOMIndividualRecord;
begin
  if (MessageDlg('Удалить ссылку на мужа?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  husband := TGEDCOMIndividualRecord(FFamily.Husband.Value);
  RemoveFamilySpouse(Base.Tree, FFamily, husband);

  ControlsRefresh();
end;

procedure TfmFamilyEdit.btnHusbandSelClick(Sender: TObject);
var
  spouse: TGEDCOMIndividualRecord;
begin
  spouse := GetHusband();
  if (spouse <> nil) then begin
    if (fmPersonEdit <> nil)
    then fmPersonEdit.Person := spouse
    else Base.SelectRecordByXRef(spouse.XRef);

    Close;
  end;
end;

procedure TfmFamilyEdit.btnWifeAddClick(Sender: TObject);
var
  wife: TGEDCOMIndividualRecord;
begin
  wife := Base.SelectPerson(nil, tmNone, svFemale);
  if (wife <> nil) then begin
    if (FFamily.Wife.StringValue = '') then begin
      FFamily.SetTagStringValue('WIFE', '@'+wife.XRef+'@');

      wife.AddSpouseToFamilyLink(
        TGEDCOMSpouseToFamilyLink.CreateTag(
          FTree, wife, 'FAMS', '@'+FFamily.XRef+'@'));

      ControlsRefresh();
    end;
  end;
end;

procedure TfmFamilyEdit.btnWifeDeleteClick(Sender: TObject);
var
  wife: TGEDCOMIndividualRecord;
begin
  if (MessageDlg('Удалить ссылку на жену?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
  then Exit;

  wife := TGEDCOMIndividualRecord(FFamily.Wife.Value);
  RemoveFamilySpouse(Base.Tree, FFamily, wife);

  ControlsRefresh();
end;

procedure TfmFamilyEdit.btnWifeSelClick(Sender: TObject);
var
  spouse: TGEDCOMIndividualRecord;
begin
  spouse := GetWife();
  if (spouse <> nil) then begin
    if (fmPersonEdit <> nil)
    then fmPersonEdit.Person := spouse
    else Base.SelectRecordByXRef(spouse.XRef);

    Close;
  end;
end;

procedure TfmFamilyEdit.btnAcceptClick(Sender: TObject);
var
  stat: string;
begin
  stat := MarriageStatus[EditMarriageStatus.ItemIndex].StatSign;
  FFamily.SetTagStringValue('_STAT', stat);

  FFamily.ChangeDate.ChangeDateTime := Now();
  Base.Modified := True;
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

procedure TfmFamilyEdit.ListModify(Sender: TObject; Index: Integer; Action: TRecAction);
var
  child: TGEDCOMIndividualRecord;
  fam_link: TGEDCOMChildToFamilyLink;
begin
  if (Sender = FChildsList) then begin
    case Action of
      raAdd: begin
        child := Base.SelectPerson(GetHusband(), tmAncestor, svNone);
        if (child <> nil) then begin
          FFamily.AddChild(TGEDCOMPointer.CreateTag(FTree, FFamily, 'CHIL', '@'+child.XRef+'@'));
          fam_link := TGEDCOMChildToFamilyLink.CreateTag(FTree, child, 'FAMC', FFamily.XRef);
          fam_link.Family := FFamily;
          child.AddChildToFamilyLink(fam_link);

          ControlsRefresh();
        end;
      end;

      raEdit: ;

      raDelete: begin
        child := TGEDCOMIndividualRecord(FFamily.Children[Index].Value);
        if (child = nil) then Exit;

        if (MessageDlg('Удалить ссылку на ребенка?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
        then Exit;

        FFamily.RemoveChild(child.XRef);
        child.DeleteChildToFamilyLink(FFamily);

        ControlsRefresh();
      end;

      raJump: begin
        child := TGEDCOMIndividualRecord(FFamily.Children[Index].Value);
        if (child <> nil) then begin
          if (fmPersonEdit <> nil)
          then fmPersonEdit.Person := child
          else Base.SelectRecordByXRef(child.XRef);

          Close;
        end;
      end;
    end;
  end
  else
  if (Sender = FEventsList) then begin
    if (Action = raAdd) then Index := Integer(nil);

    if Base.ModifyRecEvent(FFamily, TGEDCOMCustomEvent(Index), Action)
    then ControlsRefresh();
  end
  else
  //
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(FFamily, Index, Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FMediaList) then begin
    if Base.ModifyRecMultimedia(FFamily, Index, Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FSourcesList) then begin
    if Base.ModifyRecSource(FFamily, Index, Action)
    then ControlsRefresh();
  end;
end;

end.
