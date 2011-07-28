unit GKMediaEdit; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms,
  GedCom551, GKBase, GKEngine, GKCtrls, GKLists, GKUtils, GKLangs;

type
  TfmMediaEdit = class(System.Windows.Forms.Form)
  strict private
    PagesData: System.Windows.Forms.TabControl;
    SheetNotes: System.Windows.Forms.TabPage;
    SheetSources: System.Windows.Forms.TabPage;
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    OpenDialog1: System.Windows.Forms.OpenFileDialog;
    btnView: System.Windows.Forms.Button;
    SheetCommon: System.Windows.Forms.TabPage;
    Label1: System.Windows.Forms.Label;
    edName: System.Windows.Forms.TextBox;
    Label2: System.Windows.Forms.Label;
    cbMediaType: System.Windows.Forms.ComboBox;
    Label4: System.Windows.Forms.Label;
    cbStoreType: System.Windows.Forms.ComboBox;
    Label3: System.Windows.Forms.Label;
    edFile: System.Windows.Forms.TextBox;
    btnFileSelect: System.Windows.Forms.Button;

    FIsNew: Boolean;
    FMediaRec: TGEDCOMMultimediaRecord;

    FBase: TfmBase;
    FNotesList: TSheetList;
    FSourcesList: TSheetList;

    function AcceptChanges(): Boolean;
    procedure ControlsRefresh();
    procedure ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
    procedure SetMediaRec(const Value: TGEDCOMMultimediaRecord);

    procedure InitializeComponent;
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
    procedure btnFileSelect_Click(sender: System.Object; e: System.EventArgs);
    procedure btnView_Click(sender: System.Object; e: System.EventArgs);
    procedure edName_TextChanged(sender: System.Object; e: System.EventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property MediaRec: TGEDCOMMultimediaRecord read FMediaRec write SetMediaRec;
  end;

implementation

procedure TfmMediaEdit.InitializeComponent;
begin
  Self.PagesData := System.Windows.Forms.TabControl.Create;
  Self.SheetCommon := System.Windows.Forms.TabPage.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.Label4 := System.Windows.Forms.Label.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.edName := System.Windows.Forms.TextBox.Create;
  Self.cbMediaType := System.Windows.Forms.ComboBox.Create;
  Self.cbStoreType := System.Windows.Forms.ComboBox.Create;
  Self.edFile := System.Windows.Forms.TextBox.Create;
  Self.btnFileSelect := System.Windows.Forms.Button.Create;
  Self.SheetNotes := System.Windows.Forms.TabPage.Create;
  Self.SheetSources := System.Windows.Forms.TabPage.Create;
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.OpenDialog1 := System.Windows.Forms.OpenFileDialog.Create;
  Self.btnView := System.Windows.Forms.Button.Create;
  Self.PagesData.SuspendLayout;
  Self.SheetCommon.SuspendLayout;
  Self.SuspendLayout;
  // 
  // PagesData
  // 
  Self.PagesData.Controls.Add(Self.SheetCommon);
  Self.PagesData.Controls.Add(Self.SheetNotes);
  Self.PagesData.Controls.Add(Self.SheetSources);
  Self.PagesData.Location := System.Drawing.Point.Create(0, 0);
  Self.PagesData.Name := 'PagesData';
  Self.PagesData.SelectedIndex := 0;
  Self.PagesData.Size := System.Drawing.Size.Create(522, 249);
  Self.PagesData.TabIndex := 0;
  // 
  // SheetCommon
  // 
  Self.SheetCommon.Controls.Add(Self.Label1);
  Self.SheetCommon.Controls.Add(Self.Label2);
  Self.SheetCommon.Controls.Add(Self.Label4);
  Self.SheetCommon.Controls.Add(Self.Label3);
  Self.SheetCommon.Controls.Add(Self.edName);
  Self.SheetCommon.Controls.Add(Self.cbMediaType);
  Self.SheetCommon.Controls.Add(Self.cbStoreType);
  Self.SheetCommon.Controls.Add(Self.edFile);
  Self.SheetCommon.Controls.Add(Self.btnFileSelect);
  Self.SheetCommon.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetCommon.Name := 'SheetCommon';
  Self.SheetCommon.Size := System.Drawing.Size.Create(514, 223);
  Self.SheetCommon.TabIndex := 0;
  Self.SheetCommon.Text := 'Общие данные';
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 8);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(55, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Название';
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(8, 56);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(25, 13);
  Self.Label2.TabIndex := 1;
  Self.Label2.Text := 'Тип';
  // 
  // Label4
  // 
  Self.Label4.Location := System.Drawing.Point.Create(192, 56);
  Self.Label4.Name := 'Label4';
  Self.Label4.Size := System.Drawing.Size.Create(95, 13);
  Self.Label4.TabIndex := 2;
  Self.Label4.Text := 'Способ хранения';
  // 
  // Label3
  // 
  Self.Label3.Location := System.Drawing.Point.Create(8, 104);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(35, 13);
  Self.Label3.TabIndex := 3;
  Self.Label3.Text := 'Файл';
  // 
  // edName
  // 
  Self.edName.Location := System.Drawing.Point.Create(8, 24);
  Self.edName.Name := 'edName';
  Self.edName.Size := System.Drawing.Size.Create(497, 21);
  Self.edName.TabIndex := 0;
  Self.edName.Text := '';
  Include(Self.edName.TextChanged, Self.edName_TextChanged);
  // 
  // cbMediaType
  // 
  Self.cbMediaType.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.cbMediaType.DropDownWidth := 15;
  Self.cbMediaType.Location := System.Drawing.Point.Create(8, 72);
  Self.cbMediaType.Name := 'cbMediaType';
  Self.cbMediaType.Size := System.Drawing.Size.Create(169, 21);
  Self.cbMediaType.TabIndex := 1;
  // 
  // cbStoreType
  // 
  Self.cbStoreType.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.cbStoreType.Location := System.Drawing.Point.Create(192, 72);
  Self.cbStoreType.Name := 'cbStoreType';
  Self.cbStoreType.Size := System.Drawing.Size.Create(201, 21);
  Self.cbStoreType.TabIndex := 2;
  // 
  // edFile
  // 
  Self.edFile.Location := System.Drawing.Point.Create(8, 120);
  Self.edFile.Name := 'edFile';
  Self.edFile.Size := System.Drawing.Size.Create(449, 21);
  Self.edFile.TabIndex := 3;
  Self.edFile.Text := '';
  // 
  // btnFileSelect
  // 
  Self.btnFileSelect.Location := System.Drawing.Point.Create(464, 120);
  Self.btnFileSelect.Name := 'btnFileSelect';
  Self.btnFileSelect.Size := System.Drawing.Size.Create(43, 21);
  Self.btnFileSelect.TabIndex := 4;
  Self.btnFileSelect.Text := '...';
  Include(Self.btnFileSelect.Click, Self.btnFileSelect_Click);
  // 
  // SheetNotes
  // 
  Self.SheetNotes.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetNotes.Name := 'SheetNotes';
  Self.SheetNotes.Size := System.Drawing.Size.Create(514, 223);
  Self.SheetNotes.TabIndex := 1;
  Self.SheetNotes.Text := 'Заметки';
  // 
  // SheetSources
  // 
  Self.SheetSources.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetSources.Name := 'SheetSources';
  Self.SheetSources.Size := System.Drawing.Size.Create(514, 223);
  Self.SheetSources.TabIndex := 2;
  Self.SheetSources.Text := 'Источники';
  // 
  // btnAccept
  // 
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(344, 264);
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
  Self.btnCancel.Location := System.Drawing.Point.Create(432, 264);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 2;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // OpenDialog1
  // 
  Self.OpenDialog1.Filter := 'Все файлы (*.*)|*.*';
  // 
  // btnView
  // 
  Self.btnView.Location := System.Drawing.Point.Create(8, 264);
  Self.btnView.Name := 'btnView';
  Self.btnView.Size := System.Drawing.Size.Create(81, 25);
  Self.btnView.TabIndex := 3;
  Self.btnView.Text := 'Просмотр...';
  Include(Self.btnView.Click, Self.btnView_Click);
  // 
  // TfmMediaEdit
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(522, 298);
  Self.Controls.Add(Self.PagesData);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Controls.Add(Self.btnView);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmMediaEdit';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Редактирование мультимедиа объекта';
  Self.PagesData.ResumeLayout(False);
  Self.SheetCommon.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmMediaEdit.Create(aBase: TfmBase);
var
  mt: TGEDCOMFileReference.TGEDCOMMediaType;
  gst: TGenEngine.TGKStoreType;
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  for mt := Low(TGEDCOMFileReference.TGEDCOMMediaType) to High(TGEDCOMFileReference.TGEDCOMMediaType) do
    cbMediaType.Items.Add(LSList[TGenEngine.MediaTypes[mt]]);

  for gst := Low(TGenEngine.TGKStoreType) to High(TGenEngine.TGKStoreType) do
    cbStoreType.Items.Add(LSList[TGenEngine.GKStoreType[gst].Name]);
  cbStoreType.SelectedIndex := 0;

  FNotesList := TSheetList.Create(SheetNotes);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  FSourcesList := TSheetList.Create(SheetSources);
  FSourcesList.OnModify := ListModify;
  Base.SetupRecSourcesList(FSourcesList);

  // SetLang
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  SheetCommon.Text := LSList[LSID_Common];
  SheetNotes.Text := LSList[LSID_RPNotes];
  SheetSources.Text := LSList[LSID_RPSources];

  Label1.Text := LSList[LSID_Title];
  Label2.Text := LSList[LSID_Type];
  Label4.Text := LSList[LSID_StoreType];
  Label3.Text := LSList[LSID_File];
  btnView.Text := LSList[LSID_View] + '...';
end;

procedure TfmMediaEdit.edName_TextChanged(sender: System.Object; e: System.EventArgs);
begin
  Text := LSList[LSID_RPMultimedia] + ' "' + edName.Text + '"';
end;

procedure TfmMediaEdit.ControlsRefresh();
var
  file_ref: TGEDCOMFileReferenceWithTitle;
  gst: TGenEngine.TGKStoreType;
  dummy: string;
begin
  file_ref := FMediaRec.FileReferences[0];

  FIsNew := (file_ref.StringValue = '');

  edName.Text := file_ref.Title;
  cbMediaType.SelectedIndex := Ord(file_ref.MediaType);
  edFile.Text := file_ref.StringValue;

  gst := Base.Engine.GetStoreType(file_ref.StringValue, dummy);
  cbStoreType.SelectedIndex := Ord(gst);

  edFile.Enabled := (FIsNew);
  btnFileSelect.Enabled := (FIsNew);
  cbStoreType.Enabled := (FIsNew);

  Base.RecListNotesRefresh(FMediaRec, FNotesList.List, nil);
  Base.RecListSourcesRefresh(FMediaRec, FSourcesList.List, nil);
end;

procedure TfmMediaEdit.SetMediaRec(const Value: TGEDCOMMultimediaRecord);
begin
  FMediaRec := Value;

  try
    ControlsRefresh();
  except
    on E: Exception do TGKUtils.LogWrite('MediaEdit.SetMediaRec(): ' + E.Message);
  end;
end;

function TfmMediaEdit.AcceptChanges(): Boolean;
var
  file_ref: TGEDCOMFileReferenceWithTitle;
  gst: TGenEngine.TGKStoreType;
  source_fn, ref_fn: string;
begin
  file_ref := FMediaRec.FileReferences[0];
  file_ref.Title := edName.Text;

  if (FIsNew) then begin
    gst := TGenEngine.TGKStoreType(cbStoreType.SelectedIndex);

    if (gst in [gstArchive, gstStorage]) then begin
      if not(Base.IsAdvanced) then begin
        TGKUtils.ShowError(LSList[LSID_AdvancedWarning]);

        if (Base.FileProperties(fpmAdvanced) = System.Windows.Forms.DialogResult.Cancel)
        or not(Base.IsAdvanced)
        then begin
          Result := False;
          Exit;
        end;
      end;

      if not(Base.Engine.CheckPath()) then begin
        Result := False;
        Exit;
      end;
    end;

    source_fn := edFile.Text;
    Base.Engine.MediaSave(source_fn, gst, ref_fn);
    file_ref.LinkFile(ref_fn, TGEDCOMFileReference.TGEDCOMMediaType(cbMediaType.SelectedIndex));
  end else begin
    file_ref.MediaType := TGEDCOMFileReference.TGEDCOMMediaType(cbMediaType.SelectedIndex);
  end;

  ControlsRefresh();
  Base.ChangeRecord(FMediaRec);
  Result := True; 
end;

procedure TfmMediaEdit.btnAccept_Click(sender: System.Object; e: System.EventArgs);
begin
  if AcceptChanges()
  then DialogResult := System.Windows.Forms.DialogResult.OK
  else DialogResult := System.Windows.Forms.DialogResult.None;
end;

procedure TfmMediaEdit.ListModify(Sender: System.Object; ItemData: System.Object;
  Action: TGenEngine.TRecAction);
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(Self, FMediaRec, TGEDCOMNotes(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FSourcesList) then begin
    if Base.ModifyRecSource(Self, FMediaRec, TGEDCOMSourceCitation(ItemData), Action)
    then ControlsRefresh();
  end;
end;

procedure TfmMediaEdit.btnFileSelect_Click(sender: System.Object; e: System.EventArgs);
begin
  if (OpenDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK)
  then edFile.Text := OpenDialog1.FileName;
end;

procedure TfmMediaEdit.btnView_Click(sender: System.Object; e: System.EventArgs);
begin
  AcceptChanges();
  Base.ShowMedia(FMediaRec);
end;

end.
