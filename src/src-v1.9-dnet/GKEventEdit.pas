unit GKEventEdit; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.Windows.Forms, System.Resources,
  VCLStub, GedCom551, GKEngine, GKBase, GKLists, GKUtils, GKLangs;

type
  TfmEventEdit = class(System.Windows.Forms.Form)
  strict private
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    PageEventData: System.Windows.Forms.TabControl;
    SheetNotes: System.Windows.Forms.TabPage;
    SheetMultimedia: System.Windows.Forms.TabPage;
    SheetSources: System.Windows.Forms.TabPage;
    btnAddress: System.Windows.Forms.Button;
    SheetCommon: System.Windows.Forms.TabPage;
    Label1: System.Windows.Forms.Label;
    Label2: System.Windows.Forms.Label;
    Label3: System.Windows.Forms.Label;
    Label4: System.Windows.Forms.Label;
    Label5: System.Windows.Forms.Label;
    LabelAttr: System.Windows.Forms.Label;
    EditEventType: System.Windows.Forms.ComboBox;
    EditEventName: System.Windows.Forms.TextBox;
    EditEventPlace: System.Windows.Forms.TextBox;
    EditEventDateType: System.Windows.Forms.ComboBox;
    EditEventDate1: System.Windows.Forms.TextBox;
    EditEventDate2: System.Windows.Forms.TextBox;
    EditEventCause: System.Windows.Forms.TextBox;
    EditEventOrg: System.Windows.Forms.TextBox;
    EditAttribute: System.Windows.Forms.TextBox;
    btnPlaceAdd: System.Windows.Forms.Button;
    btnPlaceDelete: System.Windows.Forms.Button;
    btnPlaceSel: System.Windows.Forms.Button;
    cbDate1Calendar: System.Windows.Forms.ComboBox;
    cbDate2Calendar: System.Windows.Forms.ComboBox;

    FNotesList: TSheetList;
    FMediaList: TSheetList;
    FSourcesList: TSheetList;

    FBase: TfmBase;
    FEvent: TGEDCOMCustomEvent;
    FLocation: TGEDCOMLocationRecord;

    procedure AcceptChanges();
    procedure ControlsRefresh();
    procedure ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
    procedure SetEvent(const Value: TGEDCOMCustomEvent);
    procedure InitializeComponent;
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
    procedure btnAddress_Click(sender: System.Object; e: System.EventArgs);
    procedure EditEventPlace_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
    procedure btnPlaceAdd_Click(sender: System.Object; e: System.EventArgs);
    procedure btnPlaceDelete_Click(sender: System.Object; e: System.EventArgs);
    procedure btnPlaceSel_Click(sender: System.Object; e: System.EventArgs);
    procedure EditEventDate1_DragOver(sender: System.Object; e: System.Windows.Forms.DragEventArgs);
    procedure EditEventDate1_DragDrop(sender: System.Object; e: System.Windows.Forms.DragEventArgs);
    procedure EditEventType_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
    procedure EditEventDateType_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property Event: TGEDCOMCustomEvent read FEvent write SetEvent;
  end;

implementation

procedure TfmEventEdit.InitializeComponent;
begin
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.PageEventData := System.Windows.Forms.TabControl.Create;
  Self.SheetCommon := System.Windows.Forms.TabPage.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.Label4 := System.Windows.Forms.Label.Create;
  Self.Label5 := System.Windows.Forms.Label.Create;
  Self.LabelAttr := System.Windows.Forms.Label.Create;
  Self.btnPlaceAdd := System.Windows.Forms.Button.Create;
  Self.btnPlaceDelete := System.Windows.Forms.Button.Create;
  Self.btnPlaceSel := System.Windows.Forms.Button.Create;
  Self.EditEventType := System.Windows.Forms.ComboBox.Create;
  Self.EditEventName := System.Windows.Forms.TextBox.Create;
  Self.EditEventPlace := System.Windows.Forms.TextBox.Create;
  Self.EditEventDateType := System.Windows.Forms.ComboBox.Create;
  Self.EditEventDate1 := System.Windows.Forms.TextBox.Create;
  Self.EditEventDate2 := System.Windows.Forms.TextBox.Create;
  Self.EditEventCause := System.Windows.Forms.TextBox.Create;
  Self.EditEventOrg := System.Windows.Forms.TextBox.Create;
  Self.EditAttribute := System.Windows.Forms.TextBox.Create;
  Self.cbDate1Calendar := System.Windows.Forms.ComboBox.Create;
  Self.cbDate2Calendar := System.Windows.Forms.ComboBox.Create;
  Self.SheetNotes := System.Windows.Forms.TabPage.Create;
  Self.SheetMultimedia := System.Windows.Forms.TabPage.Create;
  Self.SheetSources := System.Windows.Forms.TabPage.Create;
  Self.btnAddress := System.Windows.Forms.Button.Create;
  Self.PageEventData.SuspendLayout;
  Self.SheetCommon.SuspendLayout;
  Self.SuspendLayout;
  // 
  // btnAccept
  // 
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(240, 368);
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
  Self.btnCancel.Location := System.Drawing.Point.Create(328, 368);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 3;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // PageEventData
  // 
  Self.PageEventData.Controls.Add(Self.SheetCommon);
  Self.PageEventData.Controls.Add(Self.SheetNotes);
  Self.PageEventData.Controls.Add(Self.SheetMultimedia);
  Self.PageEventData.Controls.Add(Self.SheetSources);
  Self.PageEventData.Location := System.Drawing.Point.Create(0, 0);
  Self.PageEventData.Name := 'PageEventData';
  Self.PageEventData.SelectedIndex := 0;
  Self.PageEventData.Size := System.Drawing.Size.Create(418, 353);
  Self.PageEventData.TabIndex := 0;
  // 
  // SheetCommon
  // 
  Self.SheetCommon.Controls.Add(Self.Label1);
  Self.SheetCommon.Controls.Add(Self.Label2);
  Self.SheetCommon.Controls.Add(Self.Label3);
  Self.SheetCommon.Controls.Add(Self.Label4);
  Self.SheetCommon.Controls.Add(Self.Label5);
  Self.SheetCommon.Controls.Add(Self.LabelAttr);
  Self.SheetCommon.Controls.Add(Self.btnPlaceAdd);
  Self.SheetCommon.Controls.Add(Self.btnPlaceDelete);
  Self.SheetCommon.Controls.Add(Self.btnPlaceSel);
  Self.SheetCommon.Controls.Add(Self.EditEventType);
  Self.SheetCommon.Controls.Add(Self.EditEventName);
  Self.SheetCommon.Controls.Add(Self.EditEventPlace);
  Self.SheetCommon.Controls.Add(Self.EditEventDateType);
  Self.SheetCommon.Controls.Add(Self.EditEventDate1);
  Self.SheetCommon.Controls.Add(Self.EditEventDate2);
  Self.SheetCommon.Controls.Add(Self.EditEventCause);
  Self.SheetCommon.Controls.Add(Self.EditEventOrg);
  Self.SheetCommon.Controls.Add(Self.EditAttribute);
  Self.SheetCommon.Controls.Add(Self.cbDate1Calendar);
  Self.SheetCommon.Controls.Add(Self.cbDate2Calendar);
  Self.SheetCommon.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetCommon.Name := 'SheetCommon';
  Self.SheetCommon.Size := System.Drawing.Size.Create(410, 327);
  Self.SheetCommon.TabIndex := 0;
  Self.SheetCommon.Text := 'Общее';
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 8);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(50, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Событие';
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(8, 104);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(40, 13);
  Self.Label2.TabIndex := 1;
  Self.Label2.Text := 'Место';
  // 
  // Label3
  // 
  Self.Label3.Location := System.Drawing.Point.Create(8, 152);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(30, 13);
  Self.Label3.TabIndex := 2;
  Self.Label3.Text := 'Дата';
  // 
  // Label4
  // 
  Self.Label4.Location := System.Drawing.Point.Create(8, 232);
  Self.Label4.Name := 'Label4';
  Self.Label4.Size := System.Drawing.Size.Create(50, 13);
  Self.Label4.TabIndex := 3;
  Self.Label4.Text := 'Причина';
  // 
  // Label5
  // 
  Self.Label5.Location := System.Drawing.Point.Create(8, 280);
  Self.Label5.Name := 'Label5';
  Self.Label5.Size := System.Drawing.Size.Create(165, 13);
  Self.Label5.TabIndex := 4;
  Self.Label5.Text := 'Засвидетельствовавший орган';
  // 
  // LabelAttr
  // 
  Self.LabelAttr.Location := System.Drawing.Point.Create(8, 56);
  Self.LabelAttr.Name := 'LabelAttr';
  Self.LabelAttr.Size := System.Drawing.Size.Create(120, 13);
  Self.LabelAttr.TabIndex := 5;
  Self.LabelAttr.Text := 'Значение атрибута';
  // 
  // btnPlaceAdd
  // 
  Self.btnPlaceAdd.AccessibleDescription := 'Выбрать или добавить место';
  Self.btnPlaceAdd.Enabled := False;
  Self.btnPlaceAdd.Location := System.Drawing.Point.Create(312, 117);
  Self.btnPlaceAdd.Name := 'btnPlaceAdd';
  Self.btnPlaceAdd.Size := System.Drawing.Size.Create(26, 26);
  Self.btnPlaceAdd.TabIndex := 6;
  Include(Self.btnPlaceAdd.Click, Self.btnPlaceAdd_Click);
  // 
  // btnPlaceDelete
  // 
  Self.btnPlaceDelete.AccessibleDescription := 'Отсоединить место';
  Self.btnPlaceDelete.Enabled := False;
  Self.btnPlaceDelete.Location := System.Drawing.Point.Create(344, 117);
  Self.btnPlaceDelete.Name := 'btnPlaceDelete';
  Self.btnPlaceDelete.Size := System.Drawing.Size.Create(26, 26);
  Self.btnPlaceDelete.TabIndex := 7;
  Include(Self.btnPlaceDelete.Click, Self.btnPlaceDelete_Click);
  // 
  // btnPlaceSel
  // 
  Self.btnPlaceSel.AccessibleDescription := 'Перейти на запись места';
  Self.btnPlaceSel.Enabled := False;
  Self.btnPlaceSel.Location := System.Drawing.Point.Create(376, 117);
  Self.btnPlaceSel.Name := 'btnPlaceSel';
  Self.btnPlaceSel.Size := System.Drawing.Size.Create(26, 26);
  Self.btnPlaceSel.TabIndex := 8;
  Include(Self.btnPlaceSel.Click, Self.btnPlaceSel_Click);
  // 
  // EditEventType
  // 
  Self.EditEventType.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.EditEventType.Location := System.Drawing.Point.Create(8, 24);
  Self.EditEventType.Name := 'EditEventType';
  Self.EditEventType.Size := System.Drawing.Size.Create(185, 21);
  Self.EditEventType.TabIndex := 0;
  Include(Self.EditEventType.SelectedIndexChanged, Self.EditEventType_SelectedIndexChanged);
  // 
  // EditEventName
  // 
  Self.EditEventName.Location := System.Drawing.Point.Create(200, 24);
  Self.EditEventName.Name := 'EditEventName';
  Self.EditEventName.Size := System.Drawing.Size.Create(201, 21);
  Self.EditEventName.TabIndex := 1;
  Self.EditEventName.Text := '';
  // 
  // EditEventPlace
  // 
  Self.EditEventPlace.Location := System.Drawing.Point.Create(8, 120);
  Self.EditEventPlace.Name := 'EditEventPlace';
  Self.EditEventPlace.Size := System.Drawing.Size.Create(297, 21);
  Self.EditEventPlace.TabIndex := 3;
  Self.EditEventPlace.Text := '';
  Include(Self.EditEventPlace.KeyDown, Self.EditEventPlace_KeyDown);
  // 
  // EditEventDateType
  // 
  Self.EditEventDateType.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.EditEventDateType.Location := System.Drawing.Point.Create(8, 168);
  Self.EditEventDateType.Name := 'EditEventDateType';
  Self.EditEventDateType.Size := System.Drawing.Size.Create(113, 21);
  Self.EditEventDateType.TabIndex := 4;
  Include(Self.EditEventDateType.SelectedIndexChanged, Self.EditEventDateType_SelectedIndexChanged);
  // 
  // EditEventDate1
  // 
  Self.EditEventDate1.AllowDrop := True;
  Self.EditEventDate1.BackColor := System.Drawing.SystemColors.Window;
  Self.EditEventDate1.Location := System.Drawing.Point.Create(136, 168);
  Self.EditEventDate1.MaxLength := 10;
  Self.EditEventDate1.Name := 'EditEventDate1';
  Self.EditEventDate1.Size := System.Drawing.Size.Create(129, 21);
  Self.EditEventDate1.TabIndex := 5;
  Self.EditEventDate1.Text := '  .  .    ';
  Include(Self.EditEventDate1.DragOver, Self.EditEventDate1_DragOver);
  Include(Self.EditEventDate1.DragDrop, Self.EditEventDate1_DragDrop);
  // 
  // EditEventDate2
  // 
  Self.EditEventDate2.AllowDrop := True;
  Self.EditEventDate2.Location := System.Drawing.Point.Create(272, 168);
  Self.EditEventDate2.MaxLength := 10;
  Self.EditEventDate2.Name := 'EditEventDate2';
  Self.EditEventDate2.Size := System.Drawing.Size.Create(129, 21);
  Self.EditEventDate2.TabIndex := 6;
  Self.EditEventDate2.Text := '  .  .    ';
  Include(Self.EditEventDate2.DragOver, Self.EditEventDate1_DragOver);
  Include(Self.EditEventDate2.DragDrop, Self.EditEventDate1_DragDrop);
  // 
  // EditEventCause
  // 
  Self.EditEventCause.Location := System.Drawing.Point.Create(8, 248);
  Self.EditEventCause.Name := 'EditEventCause';
  Self.EditEventCause.Size := System.Drawing.Size.Create(393, 21);
  Self.EditEventCause.TabIndex := 9;
  Self.EditEventCause.Text := '';
  // 
  // EditEventOrg
  // 
  Self.EditEventOrg.Location := System.Drawing.Point.Create(8, 296);
  Self.EditEventOrg.Name := 'EditEventOrg';
  Self.EditEventOrg.Size := System.Drawing.Size.Create(393, 21);
  Self.EditEventOrg.TabIndex := 10;
  Self.EditEventOrg.Text := '';
  // 
  // EditAttribute
  // 
  Self.EditAttribute.Location := System.Drawing.Point.Create(8, 72);
  Self.EditAttribute.Name := 'EditAttribute';
  Self.EditAttribute.Size := System.Drawing.Size.Create(393, 21);
  Self.EditAttribute.TabIndex := 2;
  Self.EditAttribute.Text := '';
  // 
  // cbDate1Calendar
  // 
  Self.cbDate1Calendar.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.cbDate1Calendar.Location := System.Drawing.Point.Create(136, 200);
  Self.cbDate1Calendar.Name := 'cbDate1Calendar';
  Self.cbDate1Calendar.Size := System.Drawing.Size.Create(129, 21);
  Self.cbDate1Calendar.TabIndex := 7;
  // 
  // cbDate2Calendar
  // 
  Self.cbDate2Calendar.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.cbDate2Calendar.Location := System.Drawing.Point.Create(272, 200);
  Self.cbDate2Calendar.Name := 'cbDate2Calendar';
  Self.cbDate2Calendar.Size := System.Drawing.Size.Create(129, 21);
  Self.cbDate2Calendar.TabIndex := 8;
  // 
  // SheetNotes
  // 
  Self.SheetNotes.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetNotes.Name := 'SheetNotes';
  Self.SheetNotes.Size := System.Drawing.Size.Create(410, 327);
  Self.SheetNotes.TabIndex := 1;
  Self.SheetNotes.Text := 'Заметки';
  // 
  // SheetMultimedia
  // 
  Self.SheetMultimedia.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetMultimedia.Name := 'SheetMultimedia';
  Self.SheetMultimedia.Size := System.Drawing.Size.Create(410, 327);
  Self.SheetMultimedia.TabIndex := 2;
  Self.SheetMultimedia.Text := 'Мультимедиа';
  // 
  // SheetSources
  // 
  Self.SheetSources.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetSources.Name := 'SheetSources';
  Self.SheetSources.Size := System.Drawing.Size.Create(410, 327);
  Self.SheetSources.TabIndex := 3;
  Self.SheetSources.Text := 'Источники';
  // 
  // btnAddress
  // 
  Self.btnAddress.Location := System.Drawing.Point.Create(8, 368);
  Self.btnAddress.Name := 'btnAddress';
  Self.btnAddress.Size := System.Drawing.Size.Create(81, 25);
  Self.btnAddress.TabIndex := 1;
  Self.btnAddress.Text := 'Адрес...';
  Include(Self.btnAddress.Click, Self.btnAddress_Click);
  // 
  // TfmEventEdit
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(418, 401);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Controls.Add(Self.PageEventData);
  Self.Controls.Add(Self.btnAddress);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmEventEdit';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Событие';
  Self.PageEventData.ResumeLayout(False);
  Self.SheetCommon.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmEventEdit.Create(aBase: TfmBase);
var
  i: Integer;
  gc: TGEDCOMDate.TGEDCOMCalendar;
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  //Self.EditEventDate1.EditMask := '!99/99/9999;1;_';
  //Self.EditEventDate2.EditMask := '!99/99/9999;1;_';

  for i := 0 to TGenEngine.DateKindsSize - 1 do
    EditEventDateType.Items.Add(LSList[TGenEngine.DateKinds[i].Name]);

  for gc := Low(TGEDCOMDate.TGEDCOMCalendar) to High(TGEDCOMDate.TGEDCOMCalendar) do begin
    cbDate1Calendar.Items.Add(LSList[TGenEngine.DateCalendars[gc]]);
    cbDate2Calendar.Items.Add(LSList[TGenEngine.DateCalendars[gc]]);

    cbDate1Calendar.SelectedIndex := 0;
    cbDate2Calendar.SelectedIndex := 0;
  end;

  FLocation := nil;

  FNotesList := TSheetList.Create(SheetNotes);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  FMediaList := TSheetList.Create(SheetMultimedia);
  FMediaList.OnModify := ListModify;
  Base.SetupRecMediaList(FMediaList);

  FSourcesList := TSheetList.Create(SheetSources);
  FSourcesList.OnModify := ListModify;
  Base.SetupRecSourcesList(FSourcesList);

  /// SetLang
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];
  btnAddress.Text := LSList[LSID_Address] + '...';

  SheetCommon.Text := LSList[LSID_Common];
  SheetNotes.Text := LSList[LSID_RPNotes];
  SheetMultimedia.Text := LSList[LSID_RPMultimedia];
  SheetSources.Text := LSList[LSID_RPSources];

  Label1.Text := LSList[LSID_Event];
  LabelAttr.Text := LSList[LSID_Value];
  Label2.Text := LSList[LSID_Place];
  Label3.Text := LSList[LSID_Date];
  Label4.Text := LSList[LSID_Cause];
  Label5.Text := LSList[LSID_Agency];
end;

procedure TfmEventEdit.btnPlaceSel_Click(sender: System.Object; e: System.EventArgs);
begin
  //
end;

procedure TfmEventEdit.btnPlaceDelete_Click(sender: System.Object; e: System.EventArgs);
begin
  FLocation := nil;
  ControlsRefresh();
end;

procedure TfmEventEdit.btnPlaceAdd_Click(sender: System.Object; e: System.EventArgs);
begin
  FLocation := TGEDCOMLocationRecord(Base.SelectRecord(rtLocation, []));
  ControlsRefresh();
end;

procedure TfmEventEdit.EditEventDateType_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
var
  idx: Integer;
  dates: TGenEngine.TDateControlsRange;
begin
  idx := EditEventDateType.SelectedIndex;
  dates := TGenEngine.DateKinds[idx].Dates;

  EditEventDate1.Enabled := (1 in dates);
  if EditEventDate1.Enabled
  then EditEventDate1.ForeColor := System.Drawing.SystemColors.Window
  else EditEventDate1.ForeColor := System.Drawing.SystemColors.Control;

  EditEventDate2.Enabled := (2 in dates);
  if EditEventDate2.Enabled
  then EditEventDate2.ForeColor := System.Drawing.SystemColors.Window
  else EditEventDate2.ForeColor := System.Drawing.SystemColors.Control;

  cbDate1Calendar.Enabled := (1 in dates);
  if cbDate1Calendar.Enabled
  then cbDate1Calendar.ForeColor := System.Drawing.SystemColors.Window
  else cbDate1Calendar.ForeColor := System.Drawing.SystemColors.Control;

  cbDate2Calendar.Enabled := (2 in dates);
  if cbDate2Calendar.Enabled
  then cbDate2Calendar.ForeColor := System.Drawing.SystemColors.Window
  else cbDate2Calendar.ForeColor := System.Drawing.SystemColors.Control;
end;

procedure TfmEventEdit.EditEventPlace_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
begin
  if (e.KeyCode = Keys.Down) and (e.Control)
  then EditEventPlace.Text := EditEventPlace.Text.ToLower();
end;

procedure TfmEventEdit.EditEventType_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
var
  idx: Integer;
begin
  if (FEvent is TGEDCOMFamilyEvent) then begin
    EditAttribute.Enabled := False;
    EditAttribute.ForeColor := System.Drawing.SystemColors.Control;
  end else begin
    idx := EditEventType.SelectedIndex;
    if (TGenEngine.PersonEvents[idx].Kind = ekEvent) then begin
      EditAttribute.Enabled := False;
      EditAttribute.ForeColor := System.Drawing.SystemColors.Control;
      EditAttribute.Text := '';

      //LabelAttr.Caption := 'Значение атрибута';
    end else begin
      EditAttribute.Enabled := True;
      EditAttribute.ForeColor := System.Drawing.SystemColors.Window;

      //LabelAttr.Caption := 'Значение атрибута (' + EditEventType.Text + ')';
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
  item: ListViewItem;
begin
  if (FLocation <> nil) then begin
    EditEventPlace.Text := FLocation.Name;
    EditEventPlace.ReadOnly := True;
    EditEventPlace.ForeColor := System.Drawing.SystemColors.Control;

    btnPlaceAdd.Enabled := False;
    btnPlaceDelete.Enabled := True;
  end else begin
    EditEventPlace.Text := FEvent.Detail.Place.StringValue;
    EditEventPlace.ReadOnly := False;
    EditEventPlace.ForeColor := System.Drawing.SystemColors.Window;

    btnPlaceAdd.Enabled := True;
    btnPlaceDelete.Enabled := False;
  end;

  FNotesList.List.Items.Clear();
  for idx := 0 to FEvent.Detail.NotesCount - 1 do begin
    note := FEvent.Detail.Notes[idx];

    if (note.Notes.Count > 0) then begin
      st := note.Notes[0].Trim();
      if (st = '') and (note.Notes.Count > 1)
      then st := note.Notes[1].Trim();
    end else st := '';

    FNotesList.List.AddItem(st, note);
  end;

  FMediaList.List.Items.Clear();
  for idx := 0 to FEvent.Detail.MultimediaLinksCount - 1 do begin
    mmLink := FEvent.Detail.MultimediaLinks[idx];
    mmRec := TGEDCOMMultimediaRecord(mmLink.Value);

    if (mmRec <> nil) and (mmRec.FileReferencesCount <> 0) then begin
      st := mmRec.FileReferences[0].Title;
      FMediaList.List.AddItem(st, mmLink);
    end;
  end;

  FSourcesList.List.Items.Clear();
  for idx := 0 to FEvent.Detail.SourceCitationsCount - 1 do begin
    cit := FEvent.Detail.SourceCitations[idx];
    sourceRec := TGEDCOMSourceRecord(cit.Value);

    st := '"'+sourceRec.FiledByEntry+'"';
    if (cit.Page <> '') then st := st + ', ' + cit.Page;

    if (sourceRec <> nil) then begin
      item := FSourcesList.List.AddItem(sourceRec.Originator.Text.Trim(), cit);
      item.SubItems.Add(st);
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
    for i := 0 to TGenEngine.FamilyEventsSize - 1 do
      EditEventType.Items.Add(LSList[TGenEngine.FamilyEvents[i].Name]);

    EditEventType.SelectedIndex := TGenEngine.GetFamilyEventIndex(FEvent.Name);
  end else begin
    for i := 0 to TGenEngine.PersonEventsSize - 1 do
      EditEventType.Items.Add(LSList[TGenEngine.PersonEvents[i].Name]);

    i := TGenEngine.GetPersonEventIndex(FEvent.Name);
    EditEventType.SelectedIndex := i;

    if (TGenEngine.PersonEvents[i].Kind = ekFact)
    then EditAttribute.Text := FEvent.StringValue;
  end;
  EditEventType_SelectedIndexChanged(nil, nil);

  //
  date := FEvent.Detail.Date.Value;
  if (date is TGEDCOMDateApproximated) then begin
    case TGEDCOMDateApproximated(date).Approximated of
      daExact: EditEventDateType.SelectedIndex := 0;
      daAbout: EditEventDateType.SelectedIndex := 7;
      daCalculated: EditEventDateType.SelectedIndex := 8;
      daEstimated: EditEventDateType.SelectedIndex := 9;
    end;
    EditEventDate1.Text := TGenEngine.GEDCOMDateToStr(TGEDCOMDate(date));
    cbDate1Calendar.SelectedIndex := Ord(TGEDCOMDate(date).DateCalendar);
  end
  else
  if (date is TGEDCOMDateRange) then begin
    dt_range := TGEDCOMDateRange(date);

    if (dt_range.After.StringValue = '') and (dt_range.Before.StringValue <> '')
    then EditEventDateType.SelectedIndex := 1 // Ранее
    else
    if (dt_range.After.StringValue <> '') and (dt_range.Before.StringValue = '')
    then EditEventDateType.SelectedIndex := 2 // Позднее
    else
    if (dt_range.After.StringValue <> '') and (dt_range.Before.StringValue <> '')
    then EditEventDateType.SelectedIndex := 3; // Между

    EditEventDate1.Text := TGenEngine.GEDCOMDateToStr(dt_range.After);
    EditEventDate2.Text := TGenEngine.GEDCOMDateToStr(dt_range.Before);
    cbDate1Calendar.SelectedIndex := Ord(dt_range.After.DateCalendar);
    cbDate2Calendar.SelectedIndex := Ord(dt_range.Before.DateCalendar);
  end
  else
  if (date is TGEDCOMDatePeriod) then begin
    dt_period := TGEDCOMDatePeriod(date);

    if (dt_period.DateFrom.StringValue <> '') and (dt_period.DateTo.StringValue = '')
    then EditEventDateType.SelectedIndex := 4 // Период до
    else
    if (dt_period.DateFrom.StringValue = '') and (dt_period.DateTo.StringValue <> '')
    then EditEventDateType.SelectedIndex := 5 // Период после
    else
    if (dt_period.DateFrom.StringValue <> '') and (dt_period.DateTo.StringValue <> '')
    then EditEventDateType.SelectedIndex := 6; // Период между

    EditEventDate1.Text := TGenEngine.GEDCOMDateToStr(dt_period.DateFrom);
    EditEventDate2.Text := TGenEngine.GEDCOMDateToStr(dt_period.DateTo);
    cbDate1Calendar.SelectedIndex := Ord(dt_period.DateFrom.DateCalendar);
    cbDate2Calendar.SelectedIndex := Ord(dt_period.DateTo.DateCalendar);
  end
  else
  if (date is TGEDCOMDate) then begin
    EditEventDateType.SelectedIndex := 0;
    EditEventDate1.Text := TGenEngine.GEDCOMDateToStr(TGEDCOMDate(date));
    cbDate1Calendar.SelectedIndex := Ord(TGEDCOMDate(date).DateCalendar);
  end;

  EditEventDateType_SelectedIndexChanged(nil, nil);

  EditEventName.Text := FEvent.Detail.Classification;
  EditEventCause.Text := FEvent.Detail.Cause;
  EditEventOrg.Text := FEvent.Detail.Agency;

  FLocation := TGEDCOMLocationRecord(FEvent.Detail.Place.Location.Value);

  ControlsRefresh();
end;

procedure TfmEventEdit.AcceptChanges();
var
  gcd1, gcd2, dt: string;
  id: Integer;
  attr: TGEDCOMIndividualAttribute;
  cal1, cal2: TGEDCOMDate.TGEDCOMCalendar;
begin
  FEvent.Detail.Place.StringValue := EditEventPlace.Text;
  FEvent.Detail.Place.Location.Value := FLocation;

  FEvent.Detail.Classification := EditEventName.Text;
  FEvent.Detail.Cause := EditEventCause.Text;
  FEvent.Detail.Agency := EditEventOrg.Text;

  cal1 := TGEDCOMDate.TGEDCOMCalendar(cbDate1Calendar.SelectedIndex);
  cal2 := TGEDCOMDate.TGEDCOMCalendar(cbDate2Calendar.SelectedIndex);

  gcd1 := TGenEngine.StrToGEDCOMDate(EditEventDate1.Text);
  gcd2 := TGenEngine.StrToGEDCOMDate(EditEventDate2.Text);

  if (cal1 <> dcGregorian) then gcd1 := TGEDCOMDate.GEDCOMDateEscapeArray[cal1] + ' ' + gcd1;
  if (cal2 <> dcGregorian) then gcd2 := TGEDCOMDate.GEDCOMDateEscapeArray[cal2] + ' ' + gcd2;

  case EditEventDateType.SelectedIndex of
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
    FEvent.Name := TGenEngine.FamilyEvents[EditEventType.SelectedIndex].Sign;
  end else begin
    id := EditEventType.SelectedIndex;

    FEvent.Name := TGenEngine.PersonEvents[id].Sign;
    if (TGenEngine.PersonEvents[id].Kind = ekFact)
    then FEvent.StringValue := EditAttribute.Text
    else FEvent.StringValue := '';
  end;

  if (FEvent is TGEDCOMIndividualEvent) then begin
    id := EditEventType.SelectedIndex;

    if (TGenEngine.PersonEvents[id].Kind = ekFact) then begin
      attr := TGEDCOMIndividualAttribute.Create(FEvent.Owner, FEvent.Parent);
      attr.Name := FEvent.Name;
      attr.Assign(FEvent);

      //FEvent.Destroy; // hack in caller

      FEvent := attr;
    end;
  end;
end;

procedure TfmEventEdit.btnAccept_Click(sender: System.Object; e: System.EventArgs);
begin
  try
    AcceptChanges();

    Self.DialogResult := System.Windows.Forms.DialogResult.OK;
  except
    Self.DialogResult := System.Windows.Forms.DialogResult.None;
  end;
end;

procedure TfmEventEdit.btnAddress_Click(sender: System.Object; e: System.EventArgs);
begin
  Base.ModifyAddress(Self, FEvent.Detail.Address);
end;

procedure TfmEventEdit.ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
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

procedure TfmEventEdit.EditEventDate1_DragDrop(sender: System.Object; e: System.Windows.Forms.DragEventArgs);
var
  txt, dt, sd, sm: string;
begin
  if (e.Data.GetDataPresent(typeof(string))) then begin
    txt := string(e.Data.GetData(typeof(string)));

    dt := (sender as TextBox).Text;
    sd := TGKUtils.GetToken(dt, '.', 1);
    sm := TGKUtils.GetToken(dt, '.', 2);
    (sender as TextBox).Text := sd + '.' + sm + '.' + txt;
  end;
end;

procedure TfmEventEdit.EditEventDate1_DragOver(sender: System.Object; e: System.Windows.Forms.DragEventArgs);
begin
  if (e.Data.GetDataPresent(typeof(string)))
  then e.Effect := DragDropEffects.Move
  else e.Effect := DragDropEffects.None;
end;

end.
