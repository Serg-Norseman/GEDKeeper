unit GKCommunicationEdit; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms,
  GedCom551, GKBase, GKEngine, GKLists, GKCtrls, GKUtils, GKLangs;

type
  TfmCommunicationEdit = class(System.Windows.Forms.Form)
  strict private
    GroupBox1: System.Windows.Forms.GroupBox;
    PagesGroupData: System.Windows.Forms.TabControl;
    SheetNotes: System.Windows.Forms.TabPage;
    SheetMultimedia: System.Windows.Forms.TabPage;
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    Label1: System.Windows.Forms.Label;
    EditName: System.Windows.Forms.TextBox;
    Label4: System.Windows.Forms.Label;
    EditDate: System.Windows.Forms.TextBox;
    Label2: System.Windows.Forms.Label;
    EditCorrType: System.Windows.Forms.ComboBox;
    EditDir: System.Windows.Forms.ComboBox;
    Label5: System.Windows.Forms.Label;
    EditCorresponder: System.Windows.Forms.TextBox;
    btnPersonAdd: System.Windows.Forms.Button;

    FBase: TfmBase;
    FCommunication: TGEDCOMCommunicationRecord;
    FTempInd: TGEDCOMIndividualRecord;

    FNotesList: TSheetList;
    FMediaList: TSheetList;

    procedure ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
    procedure ListsRefresh();
    procedure SetCommunication(const Value: TGEDCOMCommunicationRecord);

    procedure InitializeComponent;
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
    procedure btnPersonAdd_Click(sender: System.Object; e: System.EventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property Communication: TGEDCOMCommunicationRecord read FCommunication write SetCommunication;
  end;

implementation

procedure TfmCommunicationEdit.InitializeComponent;
type
  TArrayOfSystem_Object = array of System.Object;
begin
  Self.GroupBox1 := System.Windows.Forms.GroupBox.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.Label4 := System.Windows.Forms.Label.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.Label5 := System.Windows.Forms.Label.Create;
  Self.btnPersonAdd := System.Windows.Forms.Button.Create;
  Self.EditName := System.Windows.Forms.TextBox.Create;
  Self.EditDate := System.Windows.Forms.TextBox.Create;
  Self.EditCorrType := System.Windows.Forms.ComboBox.Create;
  Self.EditDir := System.Windows.Forms.ComboBox.Create;
  Self.EditCorresponder := System.Windows.Forms.TextBox.Create;
  Self.PagesGroupData := System.Windows.Forms.TabControl.Create;
  Self.SheetNotes := System.Windows.Forms.TabPage.Create;
  Self.SheetMultimedia := System.Windows.Forms.TabPage.Create;
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.GroupBox1.SuspendLayout;
  Self.PagesGroupData.SuspendLayout;
  Self.SuspendLayout;
  // 
  // GroupBox1
  // 
  Self.GroupBox1.Controls.Add(Self.Label1);
  Self.GroupBox1.Controls.Add(Self.Label4);
  Self.GroupBox1.Controls.Add(Self.Label2);
  Self.GroupBox1.Controls.Add(Self.Label5);
  Self.GroupBox1.Controls.Add(Self.btnPersonAdd);
  Self.GroupBox1.Controls.Add(Self.EditName);
  Self.GroupBox1.Controls.Add(Self.EditDate);
  Self.GroupBox1.Controls.Add(Self.EditCorrType);
  Self.GroupBox1.Controls.Add(Self.EditDir);
  Self.GroupBox1.Controls.Add(Self.EditCorresponder);
  Self.GroupBox1.Location := System.Drawing.Point.Create(0, 0);
  Self.GroupBox1.Name := 'GroupBox1';
  Self.GroupBox1.Size := System.Drawing.Size.Create(481, 97);
  Self.GroupBox1.TabIndex := 1;
  Self.GroupBox1.TabStop := False;
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 24);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(30, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Тема';
  // 
  // Label4
  // 
  Self.Label4.Location := System.Drawing.Point.Create(240, 72);
  Self.Label4.Name := 'Label4';
  Self.Label4.Size := System.Drawing.Size.Create(30, 13);
  Self.Label4.TabIndex := 1;
  Self.Label4.Text := 'Дата';
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(8, 72);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(25, 13);
  Self.Label2.TabIndex := 2;
  Self.Label2.Text := 'Тип';
  // 
  // Label5
  // 
  Self.Label5.Location := System.Drawing.Point.Create(8, 48);
  Self.Label5.Name := 'Label5';
  Self.Label5.Size := System.Drawing.Size.Create(85, 13);
  Self.Label5.TabIndex := 3;
  Self.Label5.Text := 'Корреспондент';
  // 
  // btnPersonAdd
  // 
  Self.btnPersonAdd.AccessibleDescription := 'Выбрать персональную запись';
  Self.btnPersonAdd.Location := System.Drawing.Point.Create(448, 37);
  Self.btnPersonAdd.Name := 'btnPersonAdd';
  Self.btnPersonAdd.Size := System.Drawing.Size.Create(26, 26);
  Self.btnPersonAdd.TabIndex := 4;
  Include(Self.btnPersonAdd.Click, Self.btnPersonAdd_Click);
  // 
  // EditName
  // 
  Self.EditName.Location := System.Drawing.Point.Create(96, 16);
  Self.EditName.Name := 'EditName';
  Self.EditName.Size := System.Drawing.Size.Create(377, 21);
  Self.EditName.TabIndex := 0;
  Self.EditName.Text := '';
  // 
  // EditDate
  // 
  Self.EditDate.Location := System.Drawing.Point.Create(280, 64);
  Self.EditDate.MaxLength := 10;
  Self.EditDate.Name := 'EditDate';
  Self.EditDate.Size := System.Drawing.Size.Create(161, 21);
  Self.EditDate.TabIndex := 4;
  Self.EditDate.Text := '  .  .    ';
  // 
  // EditCorrType
  // 
  Self.EditCorrType.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.EditCorrType.Location := System.Drawing.Point.Create(96, 64);
  Self.EditCorrType.Name := 'EditCorrType';
  Self.EditCorrType.Size := System.Drawing.Size.Create(105, 21);
  Self.EditCorrType.TabIndex := 3;
  // 
  // EditDir
  // 
  Self.EditDir.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.EditDir.Items.AddRange(TArrayOfSystem_Object.Create('от', 'к'));
  Self.EditDir.Location := System.Drawing.Point.Create(96, 40);
  Self.EditDir.Name := 'EditDir';
  Self.EditDir.Size := System.Drawing.Size.Create(65, 21);
  Self.EditDir.TabIndex := 1;
  // 
  // EditCorresponder
  // 
  Self.EditCorresponder.ForeColor := System.Drawing.SystemColors.Control;
  Self.EditCorresponder.Location := System.Drawing.Point.Create(168, 40);
  Self.EditCorresponder.Name := 'EditCorresponder';
  Self.EditCorresponder.ReadOnly := True;
  Self.EditCorresponder.Size := System.Drawing.Size.Create(273, 21);
  Self.EditCorresponder.TabIndex := 2;
  Self.EditCorresponder.Text := '';
  // 
  // PagesGroupData
  // 
  Self.PagesGroupData.Controls.Add(Self.SheetNotes);
  Self.PagesGroupData.Controls.Add(Self.SheetMultimedia);
  Self.PagesGroupData.Location := System.Drawing.Point.Create(0, 97);
  Self.PagesGroupData.Name := 'PagesGroupData';
  Self.PagesGroupData.SelectedIndex := 0;
  Self.PagesGroupData.Size := System.Drawing.Size.Create(481, 272);
  Self.PagesGroupData.TabIndex := 0;
  // 
  // SheetNotes
  // 
  Self.SheetNotes.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetNotes.Name := 'SheetNotes';
  Self.SheetNotes.Size := System.Drawing.Size.Create(473, 246);
  Self.SheetNotes.TabIndex := 0;
  Self.SheetNotes.Text := 'Заметки';
  // 
  // SheetMultimedia
  // 
  Self.SheetMultimedia.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetMultimedia.Name := 'SheetMultimedia';
  Self.SheetMultimedia.Size := System.Drawing.Size.Create(473, 246);
  Self.SheetMultimedia.TabIndex := 1;
  Self.SheetMultimedia.Text := 'Мультимедиа';
  // 
  // btnAccept
  // 
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(304, 384);
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
  Self.btnCancel.Location := System.Drawing.Point.Create(392, 384);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 3;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // TfmCommunicationEdit
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(481, 417);
  Self.Controls.Add(Self.GroupBox1);
  Self.Controls.Add(Self.PagesGroupData);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmCommunicationEdit';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Редактирование коммуникации';
  Self.GroupBox1.ResumeLayout(False);
  Self.PagesGroupData.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmCommunicationEdit.Create(aBase: TfmBase);
var
  ct: TGEDCOMCommunicationRecord.TCommunicationType;
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  //Self.EditDate.EditMask = '!99/99/9999;1;_'

  for ct := Low(TGEDCOMCommunicationRecord.TCommunicationType) to High(TGEDCOMCommunicationRecord.TCommunicationType) do
    EditCorrType.Items.Add(LSList[TGenEngine.CommunicationNames[ct]]);

  FNotesList := TSheetList.Create(SheetNotes);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  FMediaList := TSheetList.Create(SheetMultimedia);
  FMediaList.OnModify := ListModify;
  Base.SetupRecMediaList(FMediaList);

  FTempInd := nil;

  // SetLang
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  Text := LSList[LSID_WinCommunicationEdit];

  SheetNotes.Text := LSList[LSID_RPNotes];
  SheetMultimedia.Text := LSList[LSID_RPMultimedia];

  Label1.Text := LSList[LSID_Theme];
  Label5.Text := LSList[LSID_Corresponder];
  Label2.Text := LSList[LSID_Type];
  Label4.Text := LSList[LSID_Date];
end;

procedure TfmCommunicationEdit.ListsRefresh();
begin
  Base.RecListNotesRefresh(FCommunication, FNotesList.List, nil);
  Base.RecListMediaRefresh(FCommunication, TGKListView(FMediaList.List), nil);
end;

procedure TfmCommunicationEdit.SetCommunication(const Value: TGEDCOMCommunicationRecord);
var
  dir: TGEDCOMCommunicationRecord.TCommunicationDir;
begin
  FCommunication := Value;

  try
    if (FCommunication = nil) then begin
      EditName.Text := '';
      EditCorrType.SelectedIndex := -1;
      EditDate.Text := '';
      EditDir.SelectedIndex := 0;
      EditCorresponder.Text := '';
    end else begin
      EditName.Text := FCommunication.Name;
      EditCorrType.SelectedIndex := Ord(FCommunication.CommunicationType);
      EditDate.Text := TGenEngine.GEDCOMDateToStr(FCommunication.Date);

      FCommunication.GetCorresponder(dir, FTempInd);

      if (FTempInd <> nil) then begin
        EditDir.SelectedIndex := Ord(dir);
        EditCorresponder.Text := TGenEngine.GetNameStr(FTempInd);
      end else begin
        EditDir.SelectedIndex := 0;
        EditCorresponder.Text := '';
      end;
    end;

    ListsRefresh();
  except
    on E: Exception do TGKUtils.LogWrite('CommunicationEdit.SetCommunication(): ' + E.Message);
  end;
end;

procedure TfmCommunicationEdit.btnAccept_Click(sender: System.Object; e: System.EventArgs);
begin
  FCommunication.Name := EditName.Text;
  FCommunication.CommunicationType := TGEDCOMCommunicationRecord.TCommunicationType(EditCorrType.SelectedIndex);
  FCommunication.Date.ParseString(TGenEngine.StrToGEDCOMDate(EditDate.Text));
  FCommunication.SetCorresponder(TGEDCOMCommunicationRecord.TCommunicationDir(EditDir.SelectedIndex), FTempInd);

  Base.ChangeRecord(FCommunication);

  Self.DialogResult := System.Windows.Forms.DialogResult.OK;
end;

procedure TfmCommunicationEdit.ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(Self, FCommunication, TGEDCOMNotes(ItemData), Action)
    then ListsRefresh();
  end
  else
  if (Sender = FMediaList) then begin
    if Base.ModifyRecMultimedia(Self, FCommunication, TGEDCOMMultimediaLink(ItemData), Action)
    then ListsRefresh();
  end;
end;

procedure TfmCommunicationEdit.btnPersonAdd_Click(sender: System.Object; e: System.EventArgs);
begin
  FTempInd := Base.SelectPerson(nil, tmNone, svNone);
  EditCorresponder.Text := TGenEngine.GetNameStr(FTempInd);
end;

end.
