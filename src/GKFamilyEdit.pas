unit GKFamilyEdit;

{$I GEDKeeper.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, GedCom551, ComCtrls, ExtCtrls, ActnList, bsCtrls;

type
  TfmFamilyEdit = class(TForm)
    GroupBox1: TGroupBox;
    EditHusband: TEdit;
    btnHusbandAdd: TSpeedButton;
    btnHusbandDelete: TSpeedButton;
    btnHusbandSel: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    EditWife: TEdit;
    btnWifeAdd: TSpeedButton;
    btnWifeDelete: TSpeedButton;
    btnWifeSel: TSpeedButton;
    Label6: TLabel;
    EditMarriageStatus: TComboBox;
    PanelFamilyData: TPanel;
    Panel3: TPanel;
    btnFamilyDataAdd: TSpeedButton;
    btnFamilyDataDelete: TSpeedButton;
    btnFamilyDataEdit: TSpeedButton;
    btnFamilyChildSel: TSpeedButton;
    PagesFamilyData: TPageControl;
    SheetFamilyEvents: TTabSheet;
    ListFamilyEvents: TBSListView;
    SheetFamilyNotes: TTabSheet;
    ListFamilyNotes: TListBox;
    SheetFamilyMedia: TTabSheet;
    ListFamilyMedia: TListBox;
    SheetFamilySources: TTabSheet;
    ListFamilySources: TBSListView;
    SheetFamilyChilds: TTabSheet;
    ListPersonFamilyChilds: TBSListView;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    ActionList1: TActionList;
    actRecordAdd: TAction;
    actRecordEdit: TAction;
    actRecordDelete: TAction;
    procedure FormCreate(Sender: TObject);
    procedure PagesFamilyDataChange(Sender: TObject);
    procedure btnFamilyDataAddClick(Sender: TObject);
    procedure btnFamilyDataEditClick(Sender: TObject);
    procedure btnFamilyDataDeleteClick(Sender: TObject);
    procedure btnFamilyChildSelClick(Sender: TObject);
    procedure btnHusbandAddClick(Sender: TObject);
    procedure btnHusbandDeleteClick(Sender: TObject);
    procedure btnHusbandSelClick(Sender: TObject);
    procedure btnWifeAddClick(Sender: TObject);
    procedure btnWifeDeleteClick(Sender: TObject);
    procedure btnWifeSelClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure actRecordAddExecute(Sender: TObject);
    procedure actRecordEditExecute(Sender: TObject);
    procedure actRecordDeleteExecute(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
  private
    FFamily: TGEDCOMFamilyRecord;
    FTree: TGEDCOMTree;

    function  GetHusband(): TGEDCOMIndividualRecord;
    function  GetWife(): TGEDCOMIndividualRecord;
    function  GetSelectedPersonFamilyChild(): TGEDCOMIndividualRecord;
    procedure SetFamily(const Value: TGEDCOMFamilyRecord);
    procedure ControlsRefresh();
  public
    property Family: TGEDCOMFamilyRecord read FFamily write SetFamily;
    property Tree: TGEDCOMTree read FTree write FTree;
  end;

implementation

uses bsComUtils, GKMain, GKCommon, GKRecordSelect, GKPersonEdit;

{$R *.dfm}

{ TfmFamilyEdit }

procedure TfmFamilyEdit.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to MarriageStatusSize - 1 do
    EditMarriageStatus.Items.Add(MarriageStatus[i].Name);
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

  fmGEDKeeper.RecListFamilyEventsRefresh(FFamily, ListFamilyEvents, nil);
  fmGEDKeeper.RecListNotesRefresh(FFamily, ListFamilyNotes, nil);
  fmGEDKeeper.RecListMediaRefresh(FFamily, ListFamilyMedia, nil);
  fmGEDKeeper.RecListSourcesRefresh(FFamily, ListFamilySources, nil);

  ListPersonFamilyChilds.Items.BeginUpdate();
  ListPersonFamilyChilds.Clear();
  for k := 0 to FFamily.ChildrenCount - 1 do begin
    child := TGEDCOMIndividualRecord(FFamily.Children[k].Value);

    item := ListPersonFamilyChilds.Items.Add();
    item.Caption := GetNameStr(child);
    item.SubItems.Add(GetBirthDate(child, fmGEDKeeper.Options.DefDateFormat));
    item.Data := child;
  end;
  ListPersonFamilyChilds.Items.EndUpdate();
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

function TfmFamilyEdit.GetSelectedPersonFamilyChild(): TGEDCOMIndividualRecord;
begin
  Result := nil;
  if (ListPersonFamilyChilds.Selected = nil) then Exit;
  Result := TGEDCOMIndividualRecord(ListPersonFamilyChilds.Selected.Data);
end;

procedure TfmFamilyEdit.PagesFamilyDataChange(Sender: TObject);
begin
  btnFamilyDataAdd.Enabled := True;
  btnFamilyDataEdit.Enabled := True;
  btnFamilyDataDelete.Enabled := True;
  btnFamilyChildSel.Enabled := False;

  case PagesFamilyData.TabIndex of
    0: ;
    1: ;
    2: ;
    3: ;
    4: begin
      btnFamilyDataEdit.Enabled := False;
      btnFamilyChildSel.Enabled := True;
    end;
  end;
end;

procedure TfmFamilyEdit.btnFamilyDataAddClick(Sender: TObject);
var
  child: TGEDCOMIndividualRecord;
  fam_link: TGEDCOMChildToFamilyLink;
begin
  case PagesFamilyData.TabIndex of
    0: begin // События
      if fmGEDKeeper.ModifyRecEvent(FFamily, nil, raAdd)
      then ControlsRefresh();
    end;

    1: begin // Заметки
      if fmGEDKeeper.ModifyRecNote(FFamily, -1, raAdd)
      then ControlsRefresh();
    end;

    2: begin // Мультимедиа
      if fmGEDKeeper.ModifyRecMultimedia(FFamily, -1, raAdd)
      then ControlsRefresh();
    end;

    3: begin // Источники
      if fmGEDKeeper.ModifyRecSource(FFamily, -1, raAdd)
      then ControlsRefresh();
    end;

    4: begin // Дети
      child := SelectPerson(GetHusband(), tmAncestor, svNone);
      if (child <> nil) then begin
        FFamily.AddChild(TGEDCOMPointer.CreateTag(FTree, FFamily, 'CHIL', '@'+child.XRef+'@'));
        fam_link := TGEDCOMChildToFamilyLink.CreateTag(FTree, child, 'FAMC', FFamily.XRef);
        fam_link.Family := FFamily;
        child.AddChildToFamilyLink(fam_link);

        ControlsRefresh();
      end;
    end;
  end;
end;

procedure TfmFamilyEdit.btnFamilyDataEditClick(Sender: TObject);
begin
  case PagesFamilyData.TabIndex of
    0: begin // События
      if fmGEDKeeper.ModifyRecEvent(FFamily, TGEDCOMCustomEvent(GetSelObject(ListFamilyEvents)), raEdit)
      then ControlsRefresh();
    end;

    1: begin // Заметки
      if fmGEDKeeper.ModifyRecNote(FFamily, ListFamilyNotes.ItemIndex, raEdit)
      then ControlsRefresh();
    end;

    2: begin // Мультимедиа
      if fmGEDKeeper.ModifyRecMultimedia(FFamily, ListFamilyMedia.ItemIndex, raEdit)
      then ControlsRefresh();
    end;

    3: begin // Источники
      if fmGEDKeeper.ModifyRecSource(FFamily, GetSelIndex(ListFamilySources), raEdit)
      then ControlsRefresh();
    end;

    4: begin // Дети
    end;
  end;
end;

procedure TfmFamilyEdit.btnFamilyDataDeleteClick(Sender: TObject);
var
  child: TGEDCOMIndividualRecord;
begin
  case PagesFamilyData.TabIndex of
    0: begin // События
      if fmGEDKeeper.ModifyRecEvent(FFamily, TGEDCOMCustomEvent(GetSelObject(ListFamilyEvents)), raDelete)
      then ControlsRefresh();
    end;

    1: begin // Заметки
      if fmGEDKeeper.ModifyRecNote(FFamily, ListFamilyNotes.ItemIndex, raDelete)
      then ControlsRefresh();
    end;

    2: begin // Мультимедиа
      if fmGEDKeeper.ModifyRecMultimedia(FFamily, ListFamilyMedia.ItemIndex, raDelete)
      then ControlsRefresh();
    end;

    3: begin // Источники
      if fmGEDKeeper.ModifyRecSource(FFamily, GetSelIndex(ListFamilySources), raDelete)
      then ControlsRefresh();
    end;

    4: begin // Дети
      child := GetSelectedPersonFamilyChild();
      if (child = nil) then Exit;

      if (MessageDlg('Удалить ссылку на ребенка?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
      then Exit;

      FFamily.RemoveChild(child.XRef);
      child.DeleteChildToFamilyLink(FFamily);

      ControlsRefresh();
    end;
  end;
end;

procedure TfmFamilyEdit.btnFamilyChildSelClick(Sender: TObject);
var
  child: TGEDCOMIndividualRecord;
begin
  child := GetSelectedPersonFamilyChild();
  if (child <> nil) then begin
    if (fmPersonEdit <> nil)
    then fmPersonEdit.Person := child
    else fmGEDKeeper.SelectRecordByXRef(child.XRef);

    Close;
  end;
end;

procedure TfmFamilyEdit.btnHusbandAddClick(Sender: TObject);
var
  husband: TGEDCOMIndividualRecord;
begin
  husband := SelectPerson(nil, tmNone, svMale);
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
  RemoveFamilySpouse(fmGEDKeeper.FTree, FFamily, husband);

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
    else fmGEDKeeper.SelectRecordByXRef(spouse.XRef);

    Close;
  end;
end;

procedure TfmFamilyEdit.btnWifeAddClick(Sender: TObject);
var
  wife: TGEDCOMIndividualRecord;
begin
  wife := SelectPerson(nil, tmNone, svFemale);
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
  RemoveFamilySpouse(fmGEDKeeper.FTree, FFamily, wife);

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
    else fmGEDKeeper.SelectRecordByXRef(spouse.XRef);

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
  fmGEDKeeper.Modified := True;
end;

procedure TfmFamilyEdit.actRecordAddExecute(Sender: TObject);
begin
  btnFamilyDataAddClick(nil);
end;

procedure TfmFamilyEdit.actRecordDeleteExecute(Sender: TObject);
begin
  btnFamilyDataDeleteClick(nil);
end;

procedure TfmFamilyEdit.actRecordEditExecute(Sender: TObject);
begin
  btnFamilyDataEditClick(nil);
end;

procedure TfmFamilyEdit.ListDblClick(Sender: TObject);
begin
  btnFamilyDataEditClick(nil);
end;

end.
