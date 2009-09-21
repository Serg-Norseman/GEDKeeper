unit GKEventEdit;

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, Mask, GedCom551, GKCtrls, GKCommon, ActnList;

type
  TfmEventEdit = class(TForm)
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    EditEventType: TComboBox;
    EditEventName: TEdit;
    EditEventPlace: TEdit;
    EditEventDateType: TComboBox;
    EditEventDate1: TMaskEdit;
    EditEventDate2: TMaskEdit;
    EditEventCause: TEdit;
    EditEventOrg: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    btnDataAdd: TSpeedButton;
    btnDataDelete: TSpeedButton;
    btnDataEdit: TSpeedButton;
    PageEventData: TPageControl;
    SheetNotes: TTabSheet;
    ListEventNotes: TListBox;
    SheetMultimedia: TTabSheet;
    ListEventMedia: TListBox;
    SheetSources: TTabSheet;
    ListEventSources: TBSListView;
    Label6: TLabel;
    EditAttribute: TEdit;
    ActionList1: TActionList;
    actRecordAdd: TAction;
    actRecordEdit: TAction;
    actRecordDelete: TAction;
    procedure FormCreate(Sender: TObject);
    procedure EditEventDateTypeChange(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure btnDataAddClick(Sender: TObject);
    procedure btnDataEditClick(Sender: TObject);
    procedure btnDataDeleteClick(Sender: TObject);
    procedure EditEventTypeChange(Sender: TObject);
    procedure actRecordAddExecute(Sender: TObject);
    procedure actRecordEditExecute(Sender: TObject);
    procedure actRecordDeleteExecute(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
  private
    FEvent: TGEDCOMCustomEvent;

    procedure EventRefresh();
    procedure ModifyMultimedia(aIndex: Integer; anAction: TRecAction);
    procedure ModifyNote(aIndex: Integer; anAction: TRecAction);
    procedure ModifySource(aIndex: Integer; anAction: TRecAction);
    procedure SetEvent(const Value: TGEDCOMCustomEvent);
  public
    property Event: TGEDCOMCustomEvent read FEvent write SetEvent;
  end;

implementation

uses GKMain, GKNoteEdit, GKSourceEdit;

{$R *.dfm}

procedure TfmEventEdit.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to DateKindsSize - 1 do
    EditEventDateType.Items.Add(DateKinds[i].Name);
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
begin
  if (FEvent is TGEDCOMFamilyEvent) then begin
    EditAttribute.Enabled := False;
    EditAttribute.Color := clBtnFace;
  end else begin
    if (FEvent is TGEDCOMIndividualEvent) then begin
      EditAttribute.Enabled := False;
      EditAttribute.Color := clBtnFace;
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
    if (FEvent is TGEDCOMIndividualEvent) then begin
      for i := 0 to PersonEventsSize - 1 do
        EditEventType.Items.Add(PersonEvents[i].Name);

      EditEventType.ItemIndex := GetPersonEventIndex(FEvent.Name);
    end else begin
      for i := 0 to PersonAttributesSize - 1 do
        EditEventType.Items.Add(PersonAttributes[i].Name);

      EditEventType.ItemIndex := GetPersonAttributeIndex(FEvent.Name);

      EditAttribute.Text := FEvent.StringValue;
    end;
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
    then EditEventDateType.ItemIndex := 1
    else
    if (dt_range.After.StringValue <> '') and (dt_range.Before.StringValue = '')
    then EditEventDateType.ItemIndex := 2
    else
    if (dt_range.After.StringValue <> '') and (dt_range.Before.StringValue <> '')
    then EditEventDateType.ItemIndex := 3;

    EditEventDate1.Text := GEDCOMDateToStr(dt_range.After);
    EditEventDate2.Text := GEDCOMDateToStr(dt_range.Before);
  end
  else
  if (date is TGEDCOMDatePeriod) then begin
    dt_period := TGEDCOMDatePeriod(date);

    if (dt_period.DateFrom.StringValue <> '') and (dt_period.DateTo.StringValue = '')
    then EditEventDateType.ItemIndex := 4
    else
    if (dt_period.DateFrom.StringValue = '') and (dt_period.DateTo.StringValue <> '')
    then EditEventDateType.ItemIndex := 5
    else
    if (dt_period.DateFrom.StringValue <> '') and (dt_period.DateTo.StringValue <> '')
    then EditEventDateType.ItemIndex := 6;

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
    if (FEvent is TGEDCOMIndividualEvent) then begin
      FEvent.Name := PersonEvents[EditEventType.ItemIndex].Sign;
    end else begin
      FEvent.Name := PersonAttributes[EditEventType.ItemIndex].Sign;
      FEvent.StringValue := EditAttribute.Text;
    end;
  end;
end;

procedure TfmEventEdit.btnDataAddClick(Sender: TObject);
begin
  case PageEventData.TabIndex of
    0: begin // Заметки
      ModifyNote(-1, raAdd);
      EventRefresh();
    end;

    1: begin // Мультимедиа
      ModifyMultimedia(-1, raAdd);
      EventRefresh();
    end;

    2: begin // Источники
      ModifySource(-1, raAdd);
      EventRefresh();
    end;
  end;
end;

procedure TfmEventEdit.btnDataEditClick(Sender: TObject);
begin
  case PageEventData.TabIndex of
    0: begin // Заметки
      ModifyNote(ListEventNotes.ItemIndex, raEdit);
      EventRefresh();
    end;

    1: begin // Мультимедиа
      ModifyMultimedia(ListEventMedia.ItemIndex, raEdit);
      EventRefresh();
    end;

    2: begin // Источники
      ModifySource(ListEventSources.ItemIndex, raEdit);
      EventRefresh();
    end;
  end;
end;

procedure TfmEventEdit.btnDataDeleteClick(Sender: TObject);
begin
  case PageEventData.TabIndex of
    0: begin // Заметки
      ModifyNote(ListEventNotes.ItemIndex, raDelete);
      EventRefresh();
    end;

    1: begin // Мультимедиа
      ModifyMultimedia(ListEventMedia.ItemIndex, raDelete);
      EventRefresh();
    end;

    2: begin // Источники
      ModifySource(ListEventSources.ItemIndex, raDelete);
      EventRefresh();
    end;
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
  ListEventNotes.Clear();
  for idx := 0 to FEvent.Detail.NotesCount - 1 do begin
    note := FEvent.Detail.Notes[idx];

    if (note.Notes.Count > 0) then begin
      st := Trim(note.Notes[0]);
      if (st = '') and (note.Notes.Count > 1)
      then st := Trim(note.Notes[1]);
    end else st := '';

    ListEventNotes.AddItem(st, note);
  end;

  ListEventMedia.Clear();
  for idx := 0 to FEvent.Detail.MultimediaLinksCount - 1 do begin
    mmLink := FEvent.Detail.MultimediaLinks[idx];
    mmRec := TGEDCOMMultimediaRecord(mmLink.Value);

    if (mmRec <> nil) and (mmRec.FileReferencesCount <> 0) then begin
      st := mmRec.FileReferences[0].StringValue;
      ListEventMedia.AddItem(st, mmLink);
    end;
  end;

  ListEventSources.Clear();
  for idx := 0 to FEvent.Detail.SourceCitationsCount - 1 do begin
    cit := FEvent.Detail.SourceCitations[idx];
    sourceRec := TGEDCOMSourceRecord(cit.Value);

    if (sourceRec <> nil) then begin
      item := ListEventSources.Items.Add();
      item.Caption := Trim(sourceRec.Originator.Text);
      item.SubItems.Add(Trim(sourceRec.Title.Text));
    end;
  end;
end;

procedure TfmEventEdit.ModifyNote(aIndex: Integer; anAction: TRecAction);
var
  fmNoteEdit: TfmNoteEdit;
  noteRec: TGEDCOMNoteRecord;
  note: TGEDCOMNotes;
begin
  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ссылку на заметку?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    FEvent.Detail.DeleteNotes(aIndex);
    fmGEDKeeper.Modified := True;

    Exit;
  end;

  fmNoteEdit := TfmNoteEdit.Create(nil);
  try
    if (aIndex > -1) then begin
      note := FEvent.Detail.Notes[aIndex];
      noteRec := TGEDCOMNoteRecord(note.Value);
    end else begin
      noteRec := TGEDCOMNoteRecord.Create(fmGEDKeeper.FTree, fmGEDKeeper.FTree);
      noteRec.NewXRef;
    end;

    fmNoteEdit.NoteRecord := noteRec;

    if (fmNoteEdit.ShowModal = mrOk) then begin
      if (aIndex = -1) then begin
        fmGEDKeeper.FTree.AddRecord(noteRec);

        note := TGEDCOMNotes.Create(fmGEDKeeper.FTree, FEvent.Detail);
        note.Value := noteRec;
        FEvent.Detail.AddNotes(note);
      end;
    end;
  finally
    fmNoteEdit.Destroy;
  end;
end;

procedure TfmEventEdit.ModifyMultimedia(aIndex: Integer; anAction: TRecAction);
var
  fileRef: TGEDCOMFileReferenceWithTitle;
  mmRec: TGEDCOMMultimediaRecord;
begin
  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ссылку на мультимедиа?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    FEvent.Detail.DeleteMultimediaLink(aIndex);
    fmGEDKeeper.Modified := True;

    Exit;
  end;

  fmGEDKeeper.OpenDialog1.FilterIndex := 2;
  if fmGEDKeeper.OpenDialog1.Execute then begin
    if (aIndex > -1) then begin

    end else begin
      mmRec := TGEDCOMMultimediaRecord.Create(fmGEDKeeper.FTree, fmGEDKeeper.FTree);
      mmRec.NewXRef;
      fmGEDKeeper.FTree.AddRecord(mmRec);

      fileRef := TGEDCOMFileReferenceWithTitle.Create(fmGEDKeeper.FTree, mmRec);
      fileRef.LinkFile(fmGEDKeeper.OpenDialog1.FileName);
      mmRec.AddFileReference(fileRef);

      FEvent.Detail.AddMultimediaLink(
        TGEDCOMMultimediaLink.CreateTag(fmGEDKeeper.FTree, FEvent.Detail, 'OBJE', '@'+mmRec.XRef+'@'));
    end;

    fmGEDKeeper.Modified := True;
  end;
end;

procedure TfmEventEdit.ModifySource(aIndex: Integer; anAction: TRecAction);
var
  fmSrcEdit: TfmSourceEdit;
  sourceRec: TGEDCOMSourceRecord;
  cit: TGEDCOMSourceCitation;
begin
  if (anAction = raDelete) then begin
    if (MessageDlg('Удалить ссылку на источник?', mtConfirmation, [mbNo, mbYes], 0) = mrNo)
    then Exit;

    FEvent.Detail.DeleteSourceCitation(aIndex);
    fmGEDKeeper.Modified := True;

    Exit;
  end;

  fmSrcEdit := TfmSourceEdit.Create(nil);
  try
    if (aIndex > -1) then begin
      cit := FEvent.Detail.SourceCitations[aIndex];
      sourceRec := TGEDCOMSourceRecord(cit.Value);
    end else begin
      sourceRec := TGEDCOMSourceRecord.Create(fmGEDKeeper.FTree, fmGEDKeeper.FTree);
      sourceRec.NewXRef;
    end;

    fmSrcEdit.SourceRecord := sourceRec;

    if (fmSrcEdit.ShowModal = mrOk) then begin
      if (aIndex = -1) then begin
        fmGEDKeeper.FTree.AddRecord(sourceRec);

        cit := TGEDCOMSourceCitation.Create(fmGEDKeeper.FTree, FEvent.Detail);
        cit.Value := sourceRec;
        FEvent.Detail.AddSourceCitation(cit);
      end;
    end;
  finally
    fmSrcEdit.Destroy;
  end;
end;

procedure TfmEventEdit.actRecordAddExecute(Sender: TObject);
begin
  btnDataAddClick(nil);
end;

procedure TfmEventEdit.actRecordEditExecute(Sender: TObject);
begin
  btnDataEditClick(nil);
end;

procedure TfmEventEdit.actRecordDeleteExecute(Sender: TObject);
begin
  btnDataDeleteClick(nil);
end;

procedure TfmEventEdit.ListDblClick(Sender: TObject);
begin
  btnDataEditClick(nil);
end;

end.
