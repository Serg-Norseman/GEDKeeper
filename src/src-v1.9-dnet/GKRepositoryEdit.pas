unit GKRepositoryEdit; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  GedCom551, GKBase, GKEngine, GKLists, GKLangs;

type
  TfmRepositoryEdit = class(System.Windows.Forms.Form)
  strict private
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    GroupBox1: System.Windows.Forms.GroupBox;
    Label1: System.Windows.Forms.Label;
    edName: System.Windows.Forms.TextBox;
    PagesData: System.Windows.Forms.TabControl;
    SheetNotes: System.Windows.Forms.TabPage;
    btnAddress: System.Windows.Forms.Button;

    FBase: TfmBase;
    FRepository: TGEDCOMRepositoryRecord;

    FNotesList: TSheetList;

    procedure ControlsRefresh();
    procedure ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
    procedure SetRepository(const Value: TGEDCOMRepositoryRecord);
    procedure InitializeComponent;
    procedure btnAddress_Click(sender: System.Object; e: System.EventArgs);
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property Repository: TGEDCOMRepositoryRecord read FRepository write SetRepository;
  end;

implementation

procedure TfmRepositoryEdit.InitializeComponent;
begin
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.GroupBox1 := System.Windows.Forms.GroupBox.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.edName := System.Windows.Forms.TextBox.Create;
  Self.PagesData := System.Windows.Forms.TabControl.Create;
  Self.SheetNotes := System.Windows.Forms.TabPage.Create;
  Self.btnAddress := System.Windows.Forms.Button.Create;
  Self.GroupBox1.SuspendLayout;
  Self.PagesData.SuspendLayout;
  Self.SuspendLayout;
  // 
  // btnAccept
  // 
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(240, 336);
  Self.btnAccept.Name := 'btnAccept';
  Self.btnAccept.Size := System.Drawing.Size.Create(81, 25);
  Self.btnAccept.TabIndex := 3;
  Self.btnAccept.Text := 'Принять';
  Self.btnAccept.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  Include(Self.btnAccept.Click, Self.btnAccept_Click);
  // 
  // btnCancel
  // 
  Self.btnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnCancel.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnCancel.Location := System.Drawing.Point.Create(328, 336);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 4;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // GroupBox1
  // 
  Self.GroupBox1.Controls.Add(Self.Label1);
  Self.GroupBox1.Controls.Add(Self.edName);
  Self.GroupBox1.Location := System.Drawing.Point.Create(0, 0);
  Self.GroupBox1.Name := 'GroupBox1';
  Self.GroupBox1.Size := System.Drawing.Size.Create(417, 41);
  Self.GroupBox1.TabIndex := 0;
  Self.GroupBox1.TabStop := False;
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 20);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(54, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Название';
  // 
  // edName
  // 
  Self.edName.Location := System.Drawing.Point.Create(72, 12);
  Self.edName.Name := 'edName';
  Self.edName.Size := System.Drawing.Size.Create(337, 21);
  Self.edName.TabIndex := 0;
  Self.edName.Text := '';
  // 
  // PagesData
  // 
  Self.PagesData.Controls.Add(Self.SheetNotes);
  Self.PagesData.Location := System.Drawing.Point.Create(0, 41);
  Self.PagesData.Name := 'PagesData';
  Self.PagesData.SelectedIndex := 0;
  Self.PagesData.Size := System.Drawing.Size.Create(417, 280);
  Self.PagesData.TabIndex := 1;
  // 
  // SheetNotes
  // 
  Self.SheetNotes.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetNotes.Name := 'SheetNotes';
  Self.SheetNotes.Size := System.Drawing.Size.Create(409, 254);
  Self.SheetNotes.TabIndex := 0;
  Self.SheetNotes.Text := 'Заметки';
  // 
  // btnAddress
  // 
  Self.btnAddress.Location := System.Drawing.Point.Create(8, 336);
  Self.btnAddress.Name := 'btnAddress';
  Self.btnAddress.Size := System.Drawing.Size.Create(81, 25);
  Self.btnAddress.TabIndex := 2;
  Self.btnAddress.Text := 'Адрес...';
  Include(Self.btnAddress.Click, Self.btnAddress_Click);
  // 
  // TfmRepositoryEdit
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(417, 369);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Controls.Add(Self.GroupBox1);
  Self.Controls.Add(Self.PagesData);
  Self.Controls.Add(Self.btnAddress);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmRepositoryEdit';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Архив';
  Self.GroupBox1.ResumeLayout(False);
  Self.PagesData.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmRepositoryEdit.Create(aBase: TfmBase);
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  FNotesList := TSheetList.Create(SheetNotes);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  /// SetLang
  Text := LSList[LSID_Repository];

  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  Label1.Text := LSList[LSID_Title];
  SheetNotes.Text := LSList[LSID_RPNotes];
  btnAddress.Text := LSList[LSID_Address] + '...';
end;

procedure TfmRepositoryEdit.ControlsRefresh();
begin
  Base.RecListNotesRefresh(FRepository, FNotesList.List, nil);
end;

procedure TfmRepositoryEdit.SetRepository(const Value: TGEDCOMRepositoryRecord);
begin
  FRepository := Value;

  edName.Text := FRepository.RepositoryName;

  ControlsRefresh();
end;

procedure TfmRepositoryEdit.btnAddress_Click(sender: System.Object; e: System.EventArgs);
begin
  Base.ModifyAddress(Self, FRepository.Address);
end;

procedure TfmRepositoryEdit.btnAccept_Click(sender: System.Object; e: System.EventArgs);
begin
  FRepository.RepositoryName := edName.Text;
  Base.ChangeRecord(FRepository);

  Self.DialogResult := System.Windows.Forms.DialogResult.OK;
end;

procedure TfmRepositoryEdit.ListModify(Sender: System.Object; ItemData: System.Object;
  Action: TGenEngine.TRecAction);
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(Self, FRepository, TGEDCOMNotes(ItemData), Action)
    then ControlsRefresh();
  end;
end;

end.
