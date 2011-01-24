unit GKEventEdit;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, Mask, GedCom551, GKEngine, GKBase,
  GKLists, bsCtrls;

type
  TfmEventEdit = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    PageEventData: TPageControl;
    SheetNotes: TTabSheet;
    SheetMultimedia: TTabSheet;
    SheetSources: TTabSheet;
    btnAddress: TBitBtn;
    SheetCommon: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LabelAttr: TLabel;
    EditEventType: TComboBox;
    EditEventName: TEdit;
    EditEventPlace: TEdit;
    EditEventDateType: TComboBox;
    EditEventDate1: TMaskEdit;
    EditEventDate2: TMaskEdit;
    EditEventCause: TEdit;
    EditEventOrg: TEdit;
    EditAttribute: TEdit;
    btnPlaceAdd: TSpeedButton;
    btnPlaceDelete: TSpeedButton;
    btnPlaceSel: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure EditEventDateTypeChange(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure EditEventTypeChange(Sender: TObject);
    procedure btnAddressClick(Sender: TObject);
    procedure btnPlaceAddClick(Sender: TObject);
    procedure btnPlaceDeleteClick(Sender: TObject);
    procedure btnPlaceSelClick(Sender: TObject);
    procedure EditEventDate1DragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure EditEventDate1DragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure EditEventPlaceKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FEvent: TGEDCOMCustomEvent;
    FLocation: TGEDCOMLocationRecord;

    FNotesList: TSheetList;
    FMediaList: TSheetList;
    FSourcesList: TSheetList;

    procedure ControlsRefresh();
    procedure ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
    procedure SetEvent(const Value: TGEDCOMCustomEvent);
    function GetBase: TfmBase;
  public
    property Base: TfmBase read GetBase;
    property Event: TGEDCOMCustomEvent read FEvent write SetEvent;
  end;

implementation

uses GKMain, GKSourceEdit, GKAddressEdit, GKSourceCitEdit, bsComUtils;

{$R *.dfm}

{ TfmEventEdit }

procedure TfmEventEdit.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to DateKindsSize - 1 do
    EditEventDateType.Items.Add(DateKinds[i].Name);

  FLocation := nil;

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

procedure TfmEventEdit.EditEventDateTypeChange(Sender: TObject);
var
  idx: Integer;
  dates: TDateControlsRange;
begin
  idx := EditEventDateType.ItemIndex;
  dates := DateKinds[idx].Dates;

  EditEventDate1.Enabled := (1 in dates);
  if EditEventDate1.Enabled
  then EditEventDate1.Color := clWindow
  else EditEventDate1.Color := clBtnFace;

  EditEventDate2.Enabled := (2 in dates);
  if EditEventDate2.Enabled
  then EditEventDate2.Color := clWindow
  else EditEventDate2.Color := clBtnFace;
end;

procedure TfmEventEdit.EditEventPlaceKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DOWN) and (ssCtrl in Shift)
  then EditEventPlace.Text := AnsiLowerCase(EditEventPlace.Text);
end;

procedure TfmEventEdit.EditEventTypeChange(Sender: TObject);
var
  idx: Integer;
begin
  if (FEvent is TGEDCOMFamilyEvent) then begin
    EditAttribute.Enabled := False;
    EditAttribute.Color := clBtnFace;
  end else begin
    idx := EditEventType.ItemIndex;
    if (PersonEvents[idx].Kind = ekEvent) then begin
      EditAttribute.Enabled := False;
      EditAttribute.Color := clBtnFace;
      EditAttribute.Text := '';

      LabelAttr.Caption := 'Значение атрибута';
    end else begin
      EditAttribute.Enabled := True;
      EditAttribute.Color := clWindow;

      LabelAttr.Caption := 'Значение атрибута (' + EditEventType.Text + ')';
    end;
  end;
end;

procedure TfmEventEdit.ControlsRefresh();
var
  idx: Integer;
  note: TGEDCOMNotes;
  st: string;
  mmLink: TGEDCOMMultimediaLink;
  mmRec: TGEDCOMMultimediaRecord;
  cit: TGEDCOMSourceCitation;
  sourceRec: TGEDCOMSourceRecord;
  item: TListItem;
begin
  if (FLocation <> nil) then begin
    EditEventPlace.Text := FLocation.Name;
    EditEventPlace.ReadOnly := True;
    EditEventPlace.Color := clBtnFace;

    btnPlaceAdd.Enabled := False;
    btnPlaceDelete.Enabled := True;
  end else begin
    EditEventPlace.Text := FEvent.Detail.Place.StringValue;
    EditEventPlace.ReadOnly := False;
    EditEventPlace.Color := clWindow;

    btnPlaceAdd.Enabled := True;
    btnPlaceDelete.Enabled := False;
  end;

  FNotesList.List.Clear();
  for idx := 0 to FEvent.Detail.NotesCount - 1 do begin
    note := FEvent.Detail.Notes[idx];

    if (note.Notes.Count > 0) then begin
      st := Trim(note.Notes[0]);
      if (st = '') and (note.Notes.Count > 1)
      then st := Trim(note.Notes[1]);
    end else st := '';

    FNotesList.List.AddItem(st, note);
  end;

  FMediaList.List.Clear();
  for idx := 0 to FEvent.Detail.MultimediaLinksCount - 1 do begin
    mmLink := FEvent.Detail.MultimediaLinks[idx];
    mmRec := TGEDCOMMultimediaRecord(mmLink.Value);

    if (mmRec <> nil) and (mmRec.FileReferencesCount <> 0) then begin
      st := mmRec.FileReferences[0].Title;
      FMediaList.List.AddItem(st, mmLink);
    end;
  end;

  FSourcesList.List.Clear();
  for idx := 0 to FEvent.Detail.SourceCitationsCount - 1 do begin
    cit := FEvent.Detail.SourceCitations[idx];
    sourceRec := TGEDCOMSourceRecord(cit.Value);

    st := '"'+sourceRec.FiledByEntry+'"';
    if (cit.Page <> '') then st := st + ', ' + cit.Page;

    if (sourceRec <> nil) then begin
      item := TListView(FSourcesList.List).Items.Add();
      item.Caption := Trim(sourceRec.Originator.Text);
      item.SubItems.Add(st);
      item.Data := cit;
    end;
  end;
end;

procedure TfmEventEdit.SetEvent(const Value: TGEDCOMCustomEvent);
var
  i: Integer;
  date: TGEDCOMCustomDate;
  dt_range: TGEDCOMDateRange;
  dt_period: TGEDCOMDatePeriod;
begin
  FEvent := Value;

  if (FEvent is TGEDCOMFamilyEvent) then begin
    for i := 0 to FamilyEventsSize - 1 do
      EditEventType.Items.Add(FamilyEvents[i].Name);

    EditEventType.ItemIndex := GetFamilyEventIndex(FEvent.Name);
  end else begin
    for i := 0 to PersonEventsSize - 1 do
      EditEventType.Items.Add(PersonEvents[i].Name);

    i := GetPersonEventIndex(FEvent.Name);
    EditEventType.ItemIndex := i;

    if (PersonEvents[i].Kind = ekFact)
    then EditAttribute.Text := FEvent.StringValue;
  end;
  EditEventTypeChange(nil);

  //
  date := FEvent.Detail.Date.Value;
  if (date is TGEDCOMDateApproximated) then begin
    case TGEDCOMDateApproximated(date).Approximated of
      daExact: EditEventDateType.ItemIndex := 0;
      daAbout: EditEventDateType.ItemIndex := 7;
      daCalculated: EditEventDateType.ItemIndex := 8;
      daEstimated: EditEventDateType.ItemIndex := 9;
    end;
    EditEventDate1.Text := GEDCOMDateToStr(TGEDCOMDate(date));
  end
  else
  if (date is TGEDCOMDateRange) then begin
    dt_range := TGEDCOMDateRange(date);

    if (dt_range.After.StringValue = '') and (dt_range.Before.StringValue <> '')
    then EditEventDateType.ItemIndex := 1 // Ранее
    else
    if (dt_range.After.StringValue <> '') and (dt_range.Before.StringValue = '')
    then EditEventDateType.ItemIndex := 2 // Позднее
    else
    if (dt_range.After.StringValue <> '') and (dt_range.Before.StringValue <> '')
    then EditEventDateType.ItemIndex := 3; // Между

    EditEventDate1.Text := GEDCOMDateToStr(dt_range.After);
    EditEventDate2.Text := GEDCOMDateToStr(dt_range.Before);
  end
  else
  if (date is TGEDCOMDatePeriod) then begin
    dt_period := TGEDCOMDatePeriod(date);

    if (dt_period.DateFrom.StringValue <> '') and (dt_period.DateTo.StringValue = '')
    then EditEventDateType.ItemIndex := 4 // Период до
    else
    if (dt_period.DateFrom.StringValue = '') and (dt_period.DateTo.StringValue <> '')
    then EditEventDateType.ItemIndex := 5 // Период после
    else
    if (dt_period.DateFrom.StringValue <> '') and (dt_period.DateTo.StringValue <> '')
    then EditEventDateType.ItemIndex := 6; // Период между

    EditEventDate1.Text := GEDCOMDateToStr(dt_period.DateFrom);
    EditEventDate2.Text := GEDCOMDateToStr(dt_period.DateTo);
  end
  else
  if (date is TGEDCOMDate) then begin
    EditEventDateType.ItemIndex := 0;
    EditEventDate1.Text := GEDCOMDateToStr(TGEDCOMDate(date));
  end;

  EditEventDateTypeChange(nil);

  EditEventName.Text := FEvent.Detail.Classification;
  EditEventCause.Text := FEvent.Detail.Cause;
  EditEventOrg.Text := FEvent.Detail.Agency;

  FLocation := TGEDCOMLocationRecord(FEvent.Detail.Place.Location.Value);

  ControlsRefresh();
end;

procedure TfmEventEdit.btnAcceptClick(Sender: TObject);
var
  gcd1, gcd2, dt: string;
  id: Integer;
  attr: TGEDCOMIndividualAttribute;
begin
  FEvent.Detail.Place.StringValue := EditEventPlace.Text;
  FEvent.Detail.Place.Location.Value := FLocation;

  FEvent.Detail.Classification := EditEventName.Text;
  FEvent.Detail.Cause := EditEventCause.Text;
  FEvent.Detail.Agency := EditEventOrg.Text;

  gcd1 := StrToGEDCOMDate(EditEventDate1.Text);
  gcd2 := StrToGEDCOMDate(EditEventDate2.Text);

  case EditEventDateType.ItemIndex of
    0: dt := gcd1;
    1: dt := 'BEF ' + gcd2;
    2: dt := 'AFT ' + gcd1;
    3: dt := 'BET ' + gcd1 + ' AND ' + gcd2;
    4: dt := 'FROM ' + gcd1;
    5: dt := 'TO ' + gcd2;
    6: dt := 'FROM ' + gcd1 + ' TO ' + gcd2;
    7: dt := 'ABT ' + gcd1;
    8: dt := 'CAL ' + gcd1;
    9: dt := 'EST ' + gcd1;
  end;

  FEvent.Detail.Date.ParseString(dt);

  if (FEvent is TGEDCOMFamilyEvent) then begin
    FEvent.Name := FamilyEvents[EditEventType.ItemIndex].Sign;
  end else begin
    id := EditEventType.ItemIndex;

    FEvent.Name := PersonEvents[id].Sign;
    if (PersonEvents[id].Kind = ekFact)
    then FEvent.StringValue := EditAttribute.Text
    else FEvent.StringValue := '';
  end;

  if (FEvent is TGEDCOMIndividualEvent) then begin
    id := EditEventType.ItemIndex;

    if (PersonEvents[id].Kind = ekFact) then begin
      attr := TGEDCOMIndividualAttribute.Create(FEvent.Owner, FEvent.Parent);
      attr.Name := FEvent.Name;
      attr.Assign(FEvent);

      //FEvent.Destroy; // hack in caller

      FEvent := attr;
    end;
  end;
end;

procedure TfmEventEdit.btnPlaceAddClick(Sender: TObject);
begin
  FLocation := TGEDCOMLocationRecord(Base.SelectRecord(rtLocation, []));
  ControlsRefresh();
end;

procedure TfmEventEdit.btnPlaceDeleteClick(Sender: TObject);
begin
  FLocation := nil;
  ControlsRefresh();
end;

procedure TfmEventEdit.btnPlaceSelClick(Sender: TObject);
begin
  //
end;

procedure TfmEventEdit.btnAddressClick(Sender: TObject);
begin
  Base.ModifyAddress(Self, FEvent.Detail.Address);
end;

function TfmEventEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmEventEdit.ListModify(Sender: TObject; ItemData: TObject; Action: TRecAction);
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyTagNote(FEvent.Detail, TGEDCOMNotes(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FMediaList) then begin
    if Base.ModifyTagMultimedia(FEvent.Detail, TGEDCOMMultimediaLink(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FSourcesList) then begin
    if Base.ModifyTagSource(FEvent.Detail, TGEDCOMSourceCitation(ItemData), Action)
    then ControlsRefresh();
  end;
end;

procedure TfmEventEdit.EditEventDate1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source is TCustomEdit) and (TCustomEdit(Source).Name = 'edCalcResult');
end;

procedure TfmEventEdit.EditEventDate1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  txt, dt, sd, sm: string;
begin
  if (Source is TCustomEdit) and (TCustomEdit(Source).Name = 'edCalcResult') then begin
    txt := TCustomEdit(Source).Text;
    dt := TMaskEdit(Sender).Text;
    sd := GetToken(dt, '.', 1);
    sm := GetToken(dt, '.', 2);
    TMaskEdit(Sender).Text := sd + '.' + sm + '.' + txt;
  end;
end;

end.
