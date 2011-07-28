unit GKSourceEdit; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms,
  VCLStub, GedCom551, GKUtils, GKBase, GKEngine, GKCtrls, GKLists, GKLangs;

type
  TfmSourceEdit = class(System.Windows.Forms.Form)
  strict private
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    PagesData: System.Windows.Forms.TabControl;
    SheetNotes: System.Windows.Forms.TabPage;
    SheetMultimedia: System.Windows.Forms.TabPage;
    SheetRepositories: System.Windows.Forms.TabPage;
    SheetText: System.Windows.Forms.TabPage;
    EditText: System.Windows.Forms.TextBox;
    SheetCommon: System.Windows.Forms.TabPage;
    Label1: System.Windows.Forms.Label;
    EditShortTitle: System.Windows.Forms.TextBox;
    Label3: System.Windows.Forms.Label;
    EditAuthor: System.Windows.Forms.TextBox;
    Label2: System.Windows.Forms.Label;
    EditTitle: System.Windows.Forms.TextBox;
    Label4: System.Windows.Forms.Label;
    EditPublication: System.Windows.Forms.TextBox;

    FBase: TfmBase;
    FSourceRecord: TGEDCOMSourceRecord;

    FNotesList: TSheetList;
    FMediaList: TSheetList;
    FRepositoriesList: TSheetList;

    procedure AcceptChanges();
    procedure ControlsRefresh();
    procedure ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
    procedure SetSourceRecord(const Value: TGEDCOMSourceRecord);
    procedure InitializeComponent;
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
    procedure EditShortTitle_TextChanged(sender: System.Object; e: System.EventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property SourceRecord: TGEDCOMSourceRecord read FSourceRecord write SetSourceRecord;
  end;

implementation

procedure TfmSourceEdit.InitializeComponent;
begin
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.PagesData := System.Windows.Forms.TabControl.Create;
  Self.SheetCommon := System.Windows.Forms.TabPage.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.Label4 := System.Windows.Forms.Label.Create;
  Self.EditShortTitle := System.Windows.Forms.TextBox.Create;
  Self.EditAuthor := System.Windows.Forms.TextBox.Create;
  Self.EditTitle := System.Windows.Forms.TextBox.Create;
  Self.EditPublication := System.Windows.Forms.TextBox.Create;
  Self.SheetText := System.Windows.Forms.TabPage.Create;
  Self.EditText := System.Windows.Forms.TextBox.Create;
  Self.SheetRepositories := System.Windows.Forms.TabPage.Create;
  Self.SheetNotes := System.Windows.Forms.TabPage.Create;
  Self.SheetMultimedia := System.Windows.Forms.TabPage.Create;
  Self.PagesData.SuspendLayout;
  Self.SheetCommon.SuspendLayout;
  Self.SheetText.SuspendLayout;
  Self.SuspendLayout;
  // 
  // btnAccept
  // 
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(360, 416);
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
  Self.btnCancel.Location := System.Drawing.Point.Create(448, 416);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 2;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // PagesData
  // 
  Self.PagesData.Controls.Add(Self.SheetCommon);
  Self.PagesData.Controls.Add(Self.SheetText);
  Self.PagesData.Controls.Add(Self.SheetRepositories);
  Self.PagesData.Controls.Add(Self.SheetNotes);
  Self.PagesData.Controls.Add(Self.SheetMultimedia);
  Self.PagesData.Dock := System.Windows.Forms.DockStyle.Top;
  Self.PagesData.Location := System.Drawing.Point.Create(0, 0);
  Self.PagesData.Name := 'PagesData';
  Self.PagesData.SelectedIndex := 0;
  Self.PagesData.Size := System.Drawing.Size.Create(537, 401);
  Self.PagesData.TabIndex := 0;
  // 
  // SheetCommon
  // 
  Self.SheetCommon.Controls.Add(Self.Label1);
  Self.SheetCommon.Controls.Add(Self.Label3);
  Self.SheetCommon.Controls.Add(Self.Label2);
  Self.SheetCommon.Controls.Add(Self.Label4);
  Self.SheetCommon.Controls.Add(Self.EditShortTitle);
  Self.SheetCommon.Controls.Add(Self.EditAuthor);
  Self.SheetCommon.Controls.Add(Self.EditTitle);
  Self.SheetCommon.Controls.Add(Self.EditPublication);
  Self.SheetCommon.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetCommon.Name := 'SheetCommon';
  Self.SheetCommon.Size := System.Drawing.Size.Create(529, 375);
  Self.SheetCommon.TabIndex := 0;
  Self.SheetCommon.Text := 'Основное';
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 8);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(100, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Краткое название';
  // 
  // Label3
  // 
  Self.Label3.Location := System.Drawing.Point.Create(8, 32);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(60, 13);
  Self.Label3.TabIndex := 1;
  Self.Label3.Text := 'Автор';
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(8, 144);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(60, 13);
  Self.Label2.TabIndex := 2;
  Self.Label2.Text := 'Название';
  // 
  // Label4
  // 
  Self.Label4.Location := System.Drawing.Point.Create(8, 256);
  Self.Label4.Name := 'Label4';
  Self.Label4.Size := System.Drawing.Size.Create(80, 13);
  Self.Label4.TabIndex := 3;
  Self.Label4.Text := 'Опубликовано';
  // 
  // EditShortTitle
  // 
  Self.EditShortTitle.Location := System.Drawing.Point.Create(112, 8);
  Self.EditShortTitle.Name := 'EditShortTitle';
  Self.EditShortTitle.Size := System.Drawing.Size.Create(233, 21);
  Self.EditShortTitle.TabIndex := 0;
  Self.EditShortTitle.Text := '';
  Include(Self.EditShortTitle.TextChanged, Self.EditShortTitle_TextChanged);
  // 
  // EditAuthor
  // 
  Self.EditAuthor.Location := System.Drawing.Point.Create(112, 32);
  Self.EditAuthor.Multiline := True;
  Self.EditAuthor.Name := 'EditAuthor';
  Self.EditAuthor.ScrollBars := System.Windows.Forms.ScrollBars.Vertical;
  Self.EditAuthor.Size := System.Drawing.Size.Create(409, 105);
  Self.EditAuthor.TabIndex := 1;
  Self.EditAuthor.Text := '';
  // 
  // EditTitle
  // 
  Self.EditTitle.Location := System.Drawing.Point.Create(112, 144);
  Self.EditTitle.Multiline := True;
  Self.EditTitle.Name := 'EditTitle';
  Self.EditTitle.ScrollBars := System.Windows.Forms.ScrollBars.Vertical;
  Self.EditTitle.Size := System.Drawing.Size.Create(409, 105);
  Self.EditTitle.TabIndex := 2;
  Self.EditTitle.Text := '';
  //
  // EditPublication
  // 
  Self.EditPublication.Location := System.Drawing.Point.Create(112, 256);
  Self.EditPublication.Multiline := True;
  Self.EditPublication.Name := 'EditPublication';
  Self.EditPublication.ScrollBars := System.Windows.Forms.ScrollBars.Vertical;
  Self.EditPublication.Size := System.Drawing.Size.Create(409, 105);
  Self.EditPublication.TabIndex := 3;
  Self.EditPublication.Text := '';
  // 
  // SheetText
  // 
  Self.SheetText.Controls.Add(Self.EditText);
  Self.SheetText.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetText.Name := 'SheetText';
  Self.SheetText.Size := System.Drawing.Size.Create(529, 375);
  Self.SheetText.TabIndex := 1;
  Self.SheetText.Text := 'Текст';
  // 
  // EditText
  // 
  Self.EditText.Dock := System.Windows.Forms.DockStyle.Fill;
  Self.EditText.Location := System.Drawing.Point.Create(0, 0);
  Self.EditText.Multiline := True;
  Self.EditText.Name := 'EditText';
  Self.EditText.ScrollBars := System.Windows.Forms.ScrollBars.Both;
  Self.EditText.Size := System.Drawing.Size.Create(529, 375);
  Self.EditText.TabIndex := 0;
  Self.EditText.Text := '';
  // 
  // SheetRepositories
  // 
  Self.SheetRepositories.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetRepositories.Name := 'SheetRepositories';
  Self.SheetRepositories.Size := System.Drawing.Size.Create(529, 375);
  Self.SheetRepositories.TabIndex := 2;
  Self.SheetRepositories.Text := 'Архивы';
  // 
  // SheetNotes
  // 
  Self.SheetNotes.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetNotes.Name := 'SheetNotes';
  Self.SheetNotes.Size := System.Drawing.Size.Create(529, 375);
  Self.SheetNotes.TabIndex := 3;
  Self.SheetNotes.Text := 'Заметки';
  // 
  // SheetMultimedia
  //
  Self.SheetMultimedia.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetMultimedia.Name := 'SheetMultimedia';
  Self.SheetMultimedia.Size := System.Drawing.Size.Create(529, 375);
  Self.SheetMultimedia.TabIndex := 4;
  Self.SheetMultimedia.Text := 'Мультимедиа';
  // 
  // TfmSourceEdit
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(537, 449);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Controls.Add(Self.PagesData);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmSourceEdit';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Источник';
  Self.PagesData.ResumeLayout(False);
  Self.SheetCommon.ResumeLayout(False);
  Self.SheetText.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmSourceEdit.Create(aBase: TfmBase);
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  FNotesList := TSheetList.Create(SheetNotes);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  FMediaList := TSheetList.Create(SheetMultimedia);
  FMediaList.OnModify := ListModify;
  Base.SetupRecMediaList(FMediaList);

  FRepositoriesList := TSheetList.Create(SheetRepositories);
  FRepositoriesList.OnModify := ListModify;
  FRepositoriesList.Buttons := TEnumSet.Create([lbAdd, lbEdit, lbDelete, lbJump]);
  FRepositoriesList.List.AddListColumn(LSList[LSID_Repository], 300);

  // SetLang
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  Label1.Text := LSList[LSID_ShortTitle];
  Label3.Text := LSList[LSID_Author];
  Label2.Text := LSList[LSID_Title];
  Label4.Text := LSList[LSID_Publication];

  SheetCommon.Text := LSList[LSID_Common];
  SheetText.Text := LSList[LSID_Text];
  SheetRepositories.Text := LSList[LSID_RPRepositories];
  SheetNotes.Text := LSList[LSID_RPNotes];
  SheetMultimedia.Text := LSList[LSID_RPMultimedia];
end;

procedure TfmSourceEdit.EditShortTitle_TextChanged(sender: System.Object; e: System.EventArgs);
begin
  Text := LSList[LSID_Source] + ' "' + EditShortTitle.Text + '"';
end;

procedure TfmSourceEdit.ControlsRefresh();
var
  k: Integer;
  rep: TGEDCOMRepositoryRecord;
begin
  Base.RecListNotesRefresh(FSourceRecord, FNotesList.List, nil);
  Base.RecListMediaRefresh(FSourceRecord, TGKListView(FMediaList.List), nil);

  with FRepositoriesList.List do begin
    BeginUpdate();
    Items.Clear();
    for k := 0 to FSourceRecord.RepositoryCitationsCount - 1 do begin
      rep := TGEDCOMRepositoryRecord(FSourceRecord.RepositoryCitations[k].Value);
      AddItem(rep.RepositoryName, FSourceRecord.RepositoryCitations[k]);
    end;
    EndUpdate();
  end;
end;

procedure TfmSourceEdit.SetSourceRecord(const Value: TGEDCOMSourceRecord);
begin
  FSourceRecord := Value;

  EditShortTitle.Text := FSourceRecord.FiledByEntry;
  EditAuthor.Text := FSourceRecord.Originator.Text.Trim();
  EditTitle.Text := FSourceRecord.Title.Text.Trim();
  EditPublication.Text := FSourceRecord.Publication.Text.Trim();
  EditText.Text := FSourceRecord.Text.Text.Trim();

  ControlsRefresh();
end;

procedure TfmSourceEdit.AcceptChanges();
begin
  FSourceRecord.FiledByEntry := EditShortTitle.Text;

  FSourceRecord.Originator.Clear;
  FSourceRecord.SetOriginatorArray(EditAuthor.Lines);

  FSourceRecord.Title.Clear;
  FSourceRecord.SetTitleArray(EditTitle.Lines);

  FSourceRecord.Publication.Clear;
  FSourceRecord.SetPublicationArray(EditPublication.Lines);

  FSourceRecord.Text.Clear;
  FSourceRecord.SetTextArray(EditText.Lines);

  Base.ChangeRecord(FSourceRecord);
end;

procedure TfmSourceEdit.btnAccept_Click(sender: System.Object; e: System.EventArgs);
begin
  try
    AcceptChanges();

    Self.DialogResult := System.Windows.Forms.DialogResult.OK;
  except
    Self.DialogResult := System.Windows.Forms.DialogResult.None;
  end;
end;

procedure TfmSourceEdit.ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
var
  rep: TGEDCOMRepositoryRecord;
  cit: TGEDCOMRepositoryCitation;
begin
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(Self, FSourceRecord, TGEDCOMNotes(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FMediaList) then begin
    if Base.ModifyRecMultimedia(Self, FSourceRecord, TGEDCOMMultimediaLink(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FRepositoriesList) then begin
    case Action of
      raAdd: begin
        rep := TGEDCOMRepositoryRecord(Base.SelectRecord(rtRepository, []));
        if (rep <> nil) then begin
          TGenEngine.BindSourceRepository(Base.Tree, FSourceRecord, rep);
          ControlsRefresh();
        end;
      end;

      raEdit: ;

      raDelete: begin
        cit := TGEDCOMRepositoryCitation(ItemData);

        if (cit = nil) or (TGKUtils.ShowQuestion(LSList[LSID_DetachRepositoryQuery]) = System.Windows.Forms.DialogResult.No)
        then Exit;

        FSourceRecord.DeleteRepositoryCitation(cit);

        ControlsRefresh();
      end;

      raJump: begin
        cit := TGEDCOMRepositoryCitation(ItemData);
        if (cit <> nil) then begin
          AcceptChanges();
          Base.SelectRecordByXRef(TGEDCOMRepositoryRecord(cit.Value).XRef);
          Close;
        end;
      end;
    end;
  end;
end;

end.
