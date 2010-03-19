unit GKEventEdit;

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, Mask, GedCom551, GKCommon, GKBase;

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
    Label6: TLabel;
    EditEventType: TComboBox;
    EditEventName: TEdit;
    EditEventPlace: TEdit;
    EditEventDateType: TComboBox;
    EditEventDate1: TMaskEdit;
    EditEventDate2: TMaskEdit;
    EditEventCause: TEdit;
    EditEventOrg: TEdit;
    EditAttribute: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure EditEventDateTypeChange(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure EditEventTypeChange(Sender: TObject);
    procedure btnAddressClick(Sender: TObject);
  private
    FEvent: TGEDCOMCustomEvent;

    FNotesList: TSheetList;
    FMediaList: TSheetList;
    FSourcesList: TSheetList;

    procedure EventRefresh();
    procedure ListModify(Sender: TObject; Index: Integer; Action: TRecAction);
    function ModifyMultimedia(aIndex: Integer; anAction: TRecAction): Boolean;
    function ModifyNote(aIndex: Integer; anAction: TRecAction): Boolean;
    function ModifySource(aIndex: Integer; anAction: TRecAction): Boolean;
    procedure SetEvent(const Value: TGEDCOMCustomEvent);
    function GetBase: TfmBase;
  public
    property Base: TfmBase read GetBase;
    property Event: TGEDCOMCustomEvent read FEvent write SetEvent;
  end;

implementation

uses GKMain, GKSourceEdit, GKAddressEdit, GKSourceCitEdit;

{$R *.dfm}

{ TfmEventEdit }

procedure TfmEventEdit.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to DateKindsSize - 1 do
    EditEventDateType.Items.Add(DateKinds[i].Name);

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
    end else begin
      EditAttribute.Enabled := True;
      EditAttribute.Color := clWindow;
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
  EditEventPlace.Text := FEvent.Detail.Place;
  EditEventCause.Text := FEvent.Detail.Cause;
  EditEventOrg.Text := FEvent.Detail.Agency;

  EventRefresh();
end;

procedure TfmEventEdit.btnAcceptClick(Sender: TObject);
var
  gcd1, gcd2, dt: string;
  id: Integer;
  attr: TGEDCOMIndividualAttribute;
begin
  FEvent.Detail.Classification := EditEventName.Text;
  FEvent.Detail.Place := EditEventPlace.Text;
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

      FEvent.Destroy;

      FEvent := attr;
    end;
  end;
end;

function TfmEventEdit.ModifyNote(aIndex: Integer; anAction: TRecAction): Boolean;
var
  noteRec: TGEDCOMNoteRecord;
  note: TGEDCOMNotes;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ссылку на заметку?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    FEvent.Detail.DeleteNotes(aIndex);
    Base.Modified := True;

    Result := True;
    Exit;
  end;

  if (anAction = raEdit) then begin
    if (aIndex > -1) then begin
      note := FEvent.Detail.Notes[aIndex];
      noteRec := TGEDCOMNoteRecord(note.Value);
      Result := Base.ModifyNote(noteRec);
    end;
  end else begin
    noteRec := TGEDCOMNoteRecord(Base.SelectRecord(smNote));
    if (noteRec <> nil) then begin
      note := TGEDCOMNotes.Create(Base.Tree, FEvent.Detail);
      note.Value := noteRec;
      FEvent.Detail.AddNotes(note);

      Result := True;
    end;
  end;
end;

function TfmEventEdit.ModifyMultimedia(aIndex: Integer; anAction: TRecAction): Boolean;
var
  mmRec: TGEDCOMMultimediaRecord;
  mmLink: TGEDCOMMultimediaLink;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ссылку на мультимедиа?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    FEvent.Detail.DeleteMultimediaLink(aIndex);
    Base.Modified := True;

    Result := True;
    Exit;
  end;

  if (anAction = raEdit) then begin
    if (aIndex > -1) then begin
      mmLink := FEvent.Detail.MultimediaLinks[aIndex];
      mmRec := TGEDCOMMultimediaRecord(mmLink.Value);
      Result := Base.ModifyMedia(mmRec);
      Base.Modified := Base.Modified or Result;
    end;
  end else begin
    mmRec := TGEDCOMMultimediaRecord(Base.SelectRecord(smMultimedia));
    if (mmRec <> nil) then begin
      mmLink := TGEDCOMMultimediaLink.Create(Base.Tree, FEvent.Detail);
      mmLink.Value := mmRec;
      FEvent.Detail.AddMultimediaLink(mmLink);
      Base.Modified := True;
      Result := True;
    end;
  end;
end;

function TfmEventEdit.ModifySource(aIndex: Integer; anAction: TRecAction): Boolean;
var
  cit: TGEDCOMSourceCitation;
  fmSrcCitEdit: TfmSourceCitEdit;
  res: Integer;
begin
  Result := False;

  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ссылку на источник?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    FEvent.Detail.DeleteSourceCitation(aIndex);
    Base.Modified := True;
    Result := True;

    Exit;
  end;

  fmSrcCitEdit := TfmSourceCitEdit.Create(Base);
  try
    if (anAction = raEdit) and (aIndex > -1)
    then cit := FEvent.Detail.SourceCitations[aIndex]
    else cit := TGEDCOMSourceCitation.Create(Base.Tree, FEvent.Detail);

    fmSrcCitEdit.SourceCitation := cit;
    res := fmSrcCitEdit.ShowModal;

    case anAction of
      raAdd: begin
        if (res = mrOk)
        then FEvent.Detail.AddSourceCitation(cit)
        else cit.Destroy;
      end;
      raEdit: {dummy};
    end;

    Result := (res = mrOk);
  finally
    fmSrcCitEdit.Destroy;
  end;
end;

procedure TfmEventEdit.EventRefresh();
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
  FNotesList.List.Clear();
  for idx := 0 to FEvent.Detail.NotesCount - 1 do begin
    note := FEvent.Detail.Notes[idx];

    if (note.Notes.Count > 0) then begin
      st := Trim(note.Notes[0]);
      if (st = '') and (note.Notes.Count > 1)
      then st := Trim(note.Notes[1]);
    end else st := '';

    FNotesList.List.AddItem(st, TObject(idx));
  end;

  FMediaList.List.Clear();
  for idx := 0 to FEvent.Detail.MultimediaLinksCount - 1 do begin
    mmLink := FEvent.Detail.MultimediaLinks[idx];
    mmRec := TGEDCOMMultimediaRecord(mmLink.Value);

    if (mmRec <> nil) and (mmRec.FileReferencesCount <> 0) then begin
      st := mmRec.FileReferences[0].StringValue;
      FMediaList.List.AddItem(st, TObject(idx));
    end;
  end;

  FSourcesList.List.Clear();
  for idx := 0 to FEvent.Detail.SourceCitationsCount - 1 do begin
    cit := FEvent.Detail.SourceCitations[idx];
    sourceRec := TGEDCOMSourceRecord(cit.Value);

    st := '"'+sourceRec.FiledByEntry+'"';
    if (cit.Page <> '') then st := st + ', ' + cit.Page;

    if (sourceRec <> nil) then begin
      item := FSourcesList.List.Items.Add();
      item.Caption := Trim(sourceRec.Originator.Text);
      item.SubItems.Add(st);
      item.Data := TObject(idx);
    end;
  end;
end;

procedure TfmEventEdit.btnAddressClick(Sender: TObject);
var
  fmAddressEdit: TfmAddressEdit;
begin
  fmAddressEdit := TfmAddressEdit.Create(Application);
  try
    fmAddressEdit.Address := FEvent.Detail.Address;
    fmAddressEdit.ShowModal;
  finally
    fmAddressEdit.Destroy;
  end;
end;

function TfmEventEdit.GetBase: TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmEventEdit.ListModify(Sender: TObject; Index: Integer; Action: TRecAction);
begin
  if (Sender = FNotesList) then begin
    if ModifyNote(Index, Action)
    then EventRefresh();
  end
  else
  if (Sender = FMediaList) then begin
    if ModifyMultimedia(Index, Action)
    then EventRefresh();
  end
  else
  if (Sender = FSourcesList) then begin
    if ModifySource(Index, Action)
    then EventRefresh();
  end;
end;

end.
