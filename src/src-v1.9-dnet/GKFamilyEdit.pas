unit GKFamilyEdit; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  VCLStub, GedCom551, GKBase, GKEngine, GKCtrls, GKLists, GKUtils, GKMain, GKLangs;

type
  TfmFamilyEdit = class(System.Windows.Forms.Form)
  strict private
    PagesFamilyData: System.Windows.Forms.TabControl;
    SheetEvents: System.Windows.Forms.TabPage;
    SheetNotes: System.Windows.Forms.TabPage;
    SheetMultimedia: System.Windows.Forms.TabPage;
    SheetSources: System.Windows.Forms.TabPage;
    SheetChilds: System.Windows.Forms.TabPage;
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    GroupBox1: System.Windows.Forms.GroupBox;
    Label1: System.Windows.Forms.Label;
    EditHusband: System.Windows.Forms.TextBox;
    btnHusbandAdd: System.Windows.Forms.Button;
    btnHusbandDelete: System.Windows.Forms.Button;
    btnHusbandSel: System.Windows.Forms.Button;
    btnWifeSel: System.Windows.Forms.Button;
    btnWifeDelete: System.Windows.Forms.Button;
    btnWifeAdd: System.Windows.Forms.Button;
    EditWife: System.Windows.Forms.TextBox;
    Label2: System.Windows.Forms.Label;
    Label6: System.Windows.Forms.Label;
    EditMarriageStatus: System.Windows.Forms.ComboBox;
    Label5: System.Windows.Forms.Label;
    cbRestriction: System.Windows.Forms.ComboBox;

    FBase: TfmBase;
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
    procedure SetTitle();
    procedure ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);

    procedure InitializeComponent;
    procedure btnHusbandAddClick(sender: System.Object; e: System.EventArgs);
    procedure btnHusbandDeleteClick(sender: System.Object; e: System.EventArgs);
    procedure btnHusbandSelClick(sender: System.Object; e: System.EventArgs);
    procedure btnWifeAddClick(sender: System.Object; e: System.EventArgs);
    procedure btnWifeDeleteClick(sender: System.Object; e: System.EventArgs);
    procedure btnWifeSelClick(sender: System.Object; e: System.EventArgs);
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
    procedure EditHusband_TextChanged(sender: System.Object; e: System.EventArgs);
    procedure EditWife_TextChanged(sender: System.Object; e: System.EventArgs);
    procedure cbRestriction_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property Family: TGEDCOMFamilyRecord read FFamily write SetFamily;

    procedure SetLang();
  end;

implementation

procedure TfmFamilyEdit.InitializeComponent;
begin
  Self.PagesFamilyData := System.Windows.Forms.TabControl.Create;
  Self.SheetChilds := System.Windows.Forms.TabPage.Create;
  Self.SheetEvents := System.Windows.Forms.TabPage.Create;
  Self.SheetNotes := System.Windows.Forms.TabPage.Create;
  Self.SheetMultimedia := System.Windows.Forms.TabPage.Create;
  Self.SheetSources := System.Windows.Forms.TabPage.Create;
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.GroupBox1 := System.Windows.Forms.GroupBox.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.btnHusbandAdd := System.Windows.Forms.Button.Create;
  Self.btnHusbandDelete := System.Windows.Forms.Button.Create;
  Self.btnHusbandSel := System.Windows.Forms.Button.Create;
  Self.btnWifeSel := System.Windows.Forms.Button.Create;
  Self.btnWifeDelete := System.Windows.Forms.Button.Create;
  Self.btnWifeAdd := System.Windows.Forms.Button.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.Label6 := System.Windows.Forms.Label.Create;
  Self.EditHusband := System.Windows.Forms.TextBox.Create;
  Self.EditWife := System.Windows.Forms.TextBox.Create;
  Self.EditMarriageStatus := System.Windows.Forms.ComboBox.Create;
  Self.Label5 := System.Windows.Forms.Label.Create;
  Self.cbRestriction := System.Windows.Forms.ComboBox.Create;
  Self.PagesFamilyData.SuspendLayout;
  Self.GroupBox1.SuspendLayout;
  Self.SuspendLayout;
  // 
  // PagesFamilyData
  // 
  Self.PagesFamilyData.Controls.Add(Self.SheetChilds);
  Self.PagesFamilyData.Controls.Add(Self.SheetEvents);
  Self.PagesFamilyData.Controls.Add(Self.SheetNotes);
  Self.PagesFamilyData.Controls.Add(Self.SheetMultimedia);
  Self.PagesFamilyData.Controls.Add(Self.SheetSources);
  Self.PagesFamilyData.Location := System.Drawing.Point.Create(0, 129);
  Self.PagesFamilyData.Name := 'PagesFamilyData';
  Self.PagesFamilyData.SelectedIndex := 0;
  Self.PagesFamilyData.Size := System.Drawing.Size.Create(505, 264);
  Self.PagesFamilyData.TabIndex := 0;
  // 
  // SheetChilds
  // 
  Self.SheetChilds.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetChilds.Name := 'SheetChilds';
  Self.SheetChilds.Size := System.Drawing.Size.Create(497, 238);
  Self.SheetChilds.TabIndex := 0;
  Self.SheetChilds.Text := 'Дети';
  // 
  // SheetEvents
  // 
  Self.SheetEvents.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetEvents.Name := 'SheetEvents';
  Self.SheetEvents.Size := System.Drawing.Size.Create(497, 238);
  Self.SheetEvents.TabIndex := 1;
  Self.SheetEvents.Text := 'События';
  // 
  // SheetNotes
  // 
  Self.SheetNotes.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetNotes.Name := 'SheetNotes';
  Self.SheetNotes.Size := System.Drawing.Size.Create(497, 238);
  Self.SheetNotes.TabIndex := 2;
  Self.SheetNotes.Text := 'Заметки';
  // 
  // SheetMultimedia
  // 
  Self.SheetMultimedia.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetMultimedia.Name := 'SheetMultimedia';
  Self.SheetMultimedia.Size := System.Drawing.Size.Create(497, 238);
  Self.SheetMultimedia.TabIndex := 3;
  Self.SheetMultimedia.Text := 'Мультимедиа';
  // 
  // SheetSources
  // 
  Self.SheetSources.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetSources.Name := 'SheetSources';
  Self.SheetSources.Size := System.Drawing.Size.Create(497, 238);
  Self.SheetSources.TabIndex := 4;
  Self.SheetSources.Text := 'Источники';
  // 
  // btnAccept
  // 
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(328, 408);
  Self.btnAccept.Name := 'btnAccept';
  Self.btnAccept.Size := System.Drawing.Size.Create(81, 25);
  Self.btnAccept.TabIndex := 1;
  Self.btnAccept.Text := 'Принять';
  Self.btnAccept.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  Include(Self.btnAccept.Click, Self.btnAccept_Click);
  // 
  // btnCancel
  // 
  Self.btnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnCancel.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnCancel.Location := System.Drawing.Point.Create(416, 408);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 2;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // GroupBox1
  // 
  Self.GroupBox1.Controls.Add(Self.Label1);
  Self.GroupBox1.Controls.Add(Self.btnHusbandAdd);
  Self.GroupBox1.Controls.Add(Self.btnHusbandDelete);
  Self.GroupBox1.Controls.Add(Self.btnHusbandSel);
  Self.GroupBox1.Controls.Add(Self.btnWifeSel);
  Self.GroupBox1.Controls.Add(Self.btnWifeDelete);
  Self.GroupBox1.Controls.Add(Self.btnWifeAdd);
  Self.GroupBox1.Controls.Add(Self.Label2);
  Self.GroupBox1.Controls.Add(Self.Label6);
  Self.GroupBox1.Controls.Add(Self.EditHusband);
  Self.GroupBox1.Controls.Add(Self.EditWife);
  Self.GroupBox1.Controls.Add(Self.EditMarriageStatus);
  Self.GroupBox1.Location := System.Drawing.Point.Create(0, 0);
  Self.GroupBox1.Name := 'GroupBox1';
  Self.GroupBox1.Size := System.Drawing.Size.Create(505, 129);
  Self.GroupBox1.TabIndex := 3;
  Self.GroupBox1.TabStop := False;
  Self.GroupBox1.Text := 'Семья';
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(16, 32);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(35, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Муж';
  // 
  // btnHusbandAdd
  // 
  Self.btnHusbandAdd.AccessibleDescription := 'Выбрать или добавить мужа';
  Self.btnHusbandAdd.Enabled := False;
  Self.btnHusbandAdd.Location := System.Drawing.Point.Create(400, 21);
  Self.btnHusbandAdd.Name := 'btnHusbandAdd';
  Self.btnHusbandAdd.Size := System.Drawing.Size.Create(26, 26);
  Self.btnHusbandAdd.TabIndex := 1;
  Include(Self.btnHusbandAdd.Click, Self.btnHusbandAddClick);
  // 
  // btnHusbandDelete
  // 
  Self.btnHusbandDelete.AccessibleDescription := 'Отсоединить мужа';
  Self.btnHusbandDelete.Enabled := False;
  Self.btnHusbandDelete.Location := System.Drawing.Point.Create(429, 21);
  Self.btnHusbandDelete.Name := 'btnHusbandDelete';
  Self.btnHusbandDelete.Size := System.Drawing.Size.Create(26, 26);
  Self.btnHusbandDelete.TabIndex := 2;
  Include(Self.btnHusbandDelete.Click, Self.btnHusbandDeleteClick);
  // 
  // btnHusbandSel
  // 
  Self.btnHusbandSel.AccessibleDescription := 'Перейти на запись мужа';
  Self.btnHusbandSel.Location := System.Drawing.Point.Create(458, 21);
  Self.btnHusbandSel.Name := 'btnHusbandSel';
  Self.btnHusbandSel.Size := System.Drawing.Size.Create(26, 26);
  Self.btnHusbandSel.TabIndex := 3;
  Include(Self.btnHusbandSel.Click, Self.btnHusbandSelClick);
  // 
  // btnWifeSel
  // 
  Self.btnWifeSel.AccessibleDescription := 'Перейти на запись жены';
  Self.btnWifeSel.Location := System.Drawing.Point.Create(458, 53);
  Self.btnWifeSel.Name := 'btnWifeSel';
  Self.btnWifeSel.Size := System.Drawing.Size.Create(26, 26);
  Self.btnWifeSel.TabIndex := 4;
  Include(Self.btnWifeSel.Click, Self.btnWifeSelClick);
  // 
  // btnWifeDelete
  // 
  Self.btnWifeDelete.AccessibleDescription := 'Отсоединить жену';
  Self.btnWifeDelete.Enabled := False;
  Self.btnWifeDelete.Location := System.Drawing.Point.Create(429, 53);
  Self.btnWifeDelete.Name := 'btnWifeDelete';
  Self.btnWifeDelete.Size := System.Drawing.Size.Create(26, 26);
  Self.btnWifeDelete.TabIndex := 5;
  Include(Self.btnWifeDelete.Click, Self.btnWifeDeleteClick);
  // 
  // btnWifeAdd
  // 
  Self.btnWifeAdd.AccessibleDescription := 'Выбрать или добавить жену';
  Self.btnWifeAdd.Enabled := False;
  Self.btnWifeAdd.Location := System.Drawing.Point.Create(400, 53);
  Self.btnWifeAdd.Name := 'btnWifeAdd';
  Self.btnWifeAdd.Size := System.Drawing.Size.Create(26, 26);
  Self.btnWifeAdd.TabIndex := 6;
  Include(Self.btnWifeAdd.Click, Self.btnWifeAddClick);
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(16, 64);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(35, 13);
  Self.Label2.TabIndex := 7;
  Self.Label2.Text := 'Жена';
  // 
  // Label6
  // 
  Self.Label6.Location := System.Drawing.Point.Create(16, 96);
  Self.Label6.Name := 'Label6';
  Self.Label6.Size := System.Drawing.Size.Create(45, 13);
  Self.Label6.TabIndex := 8;
  Self.Label6.Text := 'Статус';
  // 
  // EditHusband
  // 
  Self.EditHusband.ForeColor := System.Drawing.SystemColors.Control;
  Self.EditHusband.Location := System.Drawing.Point.Create(64, 24);
  Self.EditHusband.Name := 'EditHusband';
  Self.EditHusband.ReadOnly := True;
  Self.EditHusband.Size := System.Drawing.Size.Create(329, 21);
  Self.EditHusband.TabIndex := 0;
  Self.EditHusband.Text := '';
  Include(Self.EditHusband.TextChanged, Self.EditHusband_TextChanged);
  // 
  // EditWife
  // 
  Self.EditWife.ForeColor := System.Drawing.SystemColors.Control;
  Self.EditWife.Location := System.Drawing.Point.Create(64, 56);
  Self.EditWife.Name := 'EditWife';
  Self.EditWife.ReadOnly := True;
  Self.EditWife.Size := System.Drawing.Size.Create(329, 21);
  Self.EditWife.TabIndex := 1;
  Self.EditWife.Text := '';
  Include(Self.EditWife.TextChanged, Self.EditWife_TextChanged);
  // 
  // EditMarriageStatus
  // 
  Self.EditMarriageStatus.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.EditMarriageStatus.Location := System.Drawing.Point.Create(64, 88);
  Self.EditMarriageStatus.Name := 'EditMarriageStatus';
  Self.EditMarriageStatus.Size := System.Drawing.Size.Create(145, 21);
  Self.EditMarriageStatus.TabIndex := 2;
  // 
  // Label5
  // 
  Self.Label5.Location := System.Drawing.Point.Create(8, 416);
  Self.Label5.Name := 'Label5';
  Self.Label5.Size := System.Drawing.Size.Create(150, 13);
  Self.Label5.TabIndex := 4;
  Self.Label5.Text := 'Ограничение безопасности';
  // 
  // cbRestriction
  // 
  Self.cbRestriction.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.cbRestriction.Location := System.Drawing.Point.Create(160, 408);
  Self.cbRestriction.Name := 'cbRestriction';
  Self.cbRestriction.Size := System.Drawing.Size.Create(145, 21);
  Self.cbRestriction.TabIndex := 4;
  Include(Self.cbRestriction.SelectedIndexChanged, Self.cbRestriction_SelectedIndexChanged);
  // 
  // TfmFamilyEdit
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(505, 441);
  Self.Controls.Add(Self.GroupBox1);
  Self.Controls.Add(Self.PagesFamilyData);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Controls.Add(Self.Label5);
  Self.Controls.Add(Self.cbRestriction);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmFamilyEdit';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Редактирование семьи';
  Self.PagesFamilyData.ResumeLayout(False);
  Self.GroupBox1.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmFamilyEdit.Create(aBase: TfmBase);
var
  i: Integer;
  res: TGEDCOMObject.TGEDCOMRestriction;
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  //Self.cbRestriction.OnChange := cbRestrictionChange;

  for res := Low(TGEDCOMObject.TGEDCOMRestriction) to High(TGEDCOMObject.TGEDCOMRestriction) do
    cbRestriction.Items.Add(TGenEngine.Restrictions[res]);

  for i := 0 to TGenEngine.MarriageStatusSize - 1 do
    EditMarriageStatus.Items.Add(LSList[TGenEngine.MarriageStatus[i].Name]);

  //

  FChildsList := TSheetList.Create(SheetChilds);
  FChildsList.OnModify := ListModify;
  FChildsList.Buttons := TEnumSet.Create([lbAdd, lbEdit, lbDelete, lbJump]);
  FChildsList.List.AddListColumn('№', 25);
  FChildsList.List.AddListColumn(LSList[LSID_Name], 300);
  FChildsList.List.AddListColumn(LSList[LSID_BirthDate], 100);

  FEventsList := TSheetList.Create(SheetEvents);
  FEventsList.OnModify := ListModify;
  Base.SetupRecEventsList(FEventsList, False);

  FNotesList := TSheetList.Create(SheetNotes);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  FMediaList := TSheetList.Create(SheetMultimedia);
  FMediaList.OnModify := ListModify;
  Base.SetupRecMediaList(FMediaList);

  FSourcesList := TSheetList.Create(SheetSources);
  FSourcesList.OnModify := ListModify;
  Base.SetupRecSourcesList(FSourcesList);

  SetLang();
end;

procedure TfmFamilyEdit.ControlsRefresh();
var
  k: Integer;
  spouse, child: TGEDCOMIndividualRecord;
  item: ListViewItem;
begin
  spouse := GetHusband();
  if (spouse <> nil)
  then EditHusband.Text := TGenEngine.GetNameStr(spouse)
  else EditHusband.Text := LSList[LSID_UnkMale];

  btnHusbandAdd.Enabled := (spouse = nil);
  btnHusbandDelete.Enabled := (spouse <> nil);
  btnHusbandSel.Enabled := (spouse <> nil);

  spouse := GetWife();
  if (spouse <> nil)
  then EditWife.Text := TGenEngine.GetNameStr(spouse)
  else EditWife.Text := LSList[LSID_UnkFemale];

  btnWifeAdd.Enabled := (spouse = nil);
  btnWifeDelete.Enabled := (spouse <> nil);
  btnWifeSel.Enabled := (spouse <> nil);

  Base.RecListFamilyEventsRefresh(FFamily, TGKListView(FEventsList.List), nil);
  Base.RecListNotesRefresh(FFamily, FNotesList.List, nil);
  Base.RecListMediaRefresh(FFamily, TGKListView(FMediaList.List), nil);
  Base.RecListSourcesRefresh(FFamily, TGKListView(FSourcesList.List), nil);

  with TGKListView(FChildsList.List) do begin
    BeginUpdate();
    Items.Clear();
    for k := 1 to FFamily.ChildrenCount do begin
      child := TGEDCOMIndividualRecord(FFamily.Children[k-1].Value);

      item := AddItem(k.ToString(), child);
      item.SubItems.Add(TGenEngine.GetNameStr(child));
      item.SubItems.Add(TGenEngine.GetBirthDate(child, fmGEDKeeper.Options.DefDateFormat));
    end;
    EndUpdate();
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
      EditMarriageStatus.SelectedIndex := 0;

      cbRestriction.SelectedIndex := 0;
    end else begin
      stat := FFamily.GetTagStringValue('_STAT');
      stat_idx := TGenEngine.GetMarriageStatusIndex(stat);
      EditMarriageStatus.Enabled := True;
      EditMarriageStatus.SelectedIndex := stat_idx;

      cbRestriction.SelectedIndex := Ord(FFamily.Restriction);

      ControlsRefresh();
    end;
  except
    on E: Exception do TGKUtils.LogWrite('FamilyEdit.SetFamily(): ' + E.Message);
  end;
end;

procedure TfmFamilyEdit.SetLang();
begin
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  GroupBox1.Text := LSList[LSID_Family];
  Label1.Text := LSList[LSID_Husband];
  Label2.Text := LSList[LSID_Wife];
  Label6.Text := LSList[LSID_Status];
  SheetChilds.Text := LSList[LSID_Childs];
  SheetEvents.Text := LSList[LSID_Events];
  SheetNotes.Text := LSList[LSID_RPNotes];
  SheetMultimedia.Text := LSList[LSID_RPMultimedia];
  SheetSources.Text := LSList[LSID_RPSources];
  Label5.Text := LSList[LSID_Restriction];
end;

procedure TfmFamilyEdit.SetTitle();
begin
  Text := LSList[LSID_Family] + ' "'+EditHusband.Text+' - '+EditWife.Text+'"';
end;

function TfmFamilyEdit.GetHusband(): TGEDCOMIndividualRecord;
begin
  Result := TGEDCOMIndividualRecord(FFamily.Husband.Value);
end;

function TfmFamilyEdit.GetWife(): TGEDCOMIndividualRecord;
begin
  Result := TGEDCOMIndividualRecord(FFamily.Wife.Value);
end;

procedure TfmFamilyEdit.btnHusbandAddClick(sender: System.Object; e: System.EventArgs);
var
  husband: TGEDCOMIndividualRecord;
begin
  husband := Base.SelectPerson(nil, tmNone, svMale);
  if (husband <> nil) and (FFamily.Husband.StringValue = '') then begin
    Base.Engine.AddFamilySpouse(FFamily, husband);
    ControlsRefresh();
  end;
end;

procedure TfmFamilyEdit.btnHusbandDeleteClick(sender: System.Object; e: System.EventArgs);
begin
  if (TGKUtils.ShowQuestion(LSList[LSID_DetachHusbandQuery]) = System.Windows.Forms.DialogResult.No)
  then Exit;

  Base.Engine.RemoveFamilySpouse(FFamily, GetHusband());
  ControlsRefresh();
end;

procedure TfmFamilyEdit.btnHusbandSelClick(sender: System.Object; e: System.EventArgs);
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

procedure TfmFamilyEdit.btnWifeAddClick(sender: System.Object; e: System.EventArgs);
var
  wife: TGEDCOMIndividualRecord;
begin
  wife := Base.SelectPerson(nil, tmNone, svFemale);
  if (wife <> nil) and (FFamily.Wife.StringValue = '') then begin
    Base.Engine.AddFamilySpouse(FFamily, wife);
    ControlsRefresh();
  end;
end;

procedure TfmFamilyEdit.btnWifeDeleteClick(sender: System.Object; e: System.EventArgs);
begin
  if (TGKUtils.ShowQuestion(LSList[LSID_DetachWifeQuery]) = System.Windows.Forms.DialogResult.No)
  then Exit;

  Base.Engine.RemoveFamilySpouse(FFamily, GetWife());
  ControlsRefresh();
end;

procedure TfmFamilyEdit.btnWifeSelClick(sender: System.Object; e: System.EventArgs);
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
  stat := TGenEngine.MarriageStatus[EditMarriageStatus.SelectedIndex].StatSign;
  FFamily.SetTagStringValue('_STAT', stat);

  FFamily.Restriction := TGEDCOMObject.TGEDCOMRestriction(cbRestriction.SelectedIndex);
  FFamily.SortChilds();

  Base.ChangeRecord(FFamily);
end;

procedure TfmFamilyEdit.btnAccept_Click(sender: System.Object; e: System.EventArgs);
begin
  try
    AcceptChanges();

    Self.DialogResult := System.Windows.Forms.DialogResult.OK;
  except
    Self.DialogResult := System.Windows.Forms.DialogResult.None;
  end;
end;

procedure TfmFamilyEdit.EditWife_TextChanged(sender: System.Object; e: System.EventArgs);
begin
  SetTitle();
end;

procedure TfmFamilyEdit.EditHusband_TextChanged(sender: System.Object; e: System.EventArgs);
begin
  SetTitle();
end;

procedure TfmFamilyEdit.ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
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

        if (child = nil) or (TGKUtils.ShowQuestion(LSList[LSID_DetachChildQuery]) = System.Windows.Forms.DialogResult.No)
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

procedure TfmFamilyEdit.cbRestriction_SelectedIndexChanged(sender: System.Object;
  e: System.EventArgs);
begin
  ControlsRefresh();
end;

end.
