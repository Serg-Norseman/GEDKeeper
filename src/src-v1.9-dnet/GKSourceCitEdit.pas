unit GKSourceCitEdit; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  VCLStub, GedCom551, GKUtils, GKEngine, GKCtrls, GKBase, GKLangs;

type
  TfmSourceCitEdit = class(System.Windows.Forms.Form)
  strict private
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    Label1: System.Windows.Forms.Label;
    EditPage: System.Windows.Forms.TextBox;
    Label2: System.Windows.Forms.Label;
    btnSourceAdd: System.Windows.Forms.Button;
    Label3: System.Windows.Forms.Label;
    EditCertainty: System.Windows.Forms.ComboBox;
    cbSource: System.Windows.Forms.ComboBox;

    procedure InitializeComponent;
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
    procedure btnSourceAdd_Click(sender: System.Object; e: System.EventArgs);
    procedure cbSource_KeyUp(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
    procedure cbSource_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
  private
    FBase: TfmBase;
    FSourceCitation: TGEDCOMSourceCitation;
    FTempSrc: TGEDCOMSourceRecord;

    FSourcesList: TStringList;

    procedure RefreshSourcesList(aFilter: string);
    procedure SetSourceCitation(const Value: TGEDCOMSourceCitation);
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property SourceCitation: TGEDCOMSourceCitation read FSourceCitation write SetSourceCitation;
  end;

implementation

procedure TfmSourceCitEdit.InitializeComponent;
var
  resources: System.Resources.ResourceManager;
begin
  resources := System.Resources.ResourceManager.Create(TypeOf(TfmSourceCitEdit));
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.EditPage := System.Windows.Forms.TextBox.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.btnSourceAdd := System.Windows.Forms.Button.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.EditCertainty := System.Windows.Forms.ComboBox.Create;
  Self.cbSource := System.Windows.Forms.ComboBox.Create;
  Self.SuspendLayout;
  // 
  // btnAccept
  // 
  Self.btnAccept.Image := (System.Drawing.Image(resources.GetObject('btnAcce' +
    'pt.Image')));
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(176, 160);
  Self.btnAccept.Name := 'btnAccept';
  Self.btnAccept.Size := System.Drawing.Size.Create(81, 25);
  Self.btnAccept.TabIndex := 4;
  Self.btnAccept.Text := 'Принять';
  Self.btnAccept.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  Include(Self.btnAccept.Click, Self.btnAccept_Click);
  // 
  // btnCancel
  // 
  Self.btnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnCancel.Image := (System.Drawing.Image(resources.GetObject('btnCanc' +
    'el.Image')));
  Self.btnCancel.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnCancel.Location := System.Drawing.Point.Create(264, 160);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 5;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 56);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(90, 13);
  Self.Label1.TabIndex := 6;
  Self.Label1.Text := 'Лист/Страница';
  // 
  // EditPage
  // 
  Self.EditPage.Location := System.Drawing.Point.Create(8, 72);
  Self.EditPage.Name := 'EditPage';
  Self.EditPage.Size := System.Drawing.Size.Create(337, 21);
  Self.EditPage.TabIndex := 2;
  Self.EditPage.Text := '';
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(8, 8);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(60, 13);
  Self.Label2.TabIndex := 7;
  Self.Label2.Text := 'Источник';
  // 
  // btnSourceAdd
  // 
  Self.btnSourceAdd.AccessibleDescription := 'Выбрать персональную запись';
  Self.btnSourceAdd.Location := System.Drawing.Point.Create(320, 21);
  Self.btnSourceAdd.Name := 'btnSourceAdd';
  Self.btnSourceAdd.Size := System.Drawing.Size.Create(26, 26);
  Self.btnSourceAdd.TabIndex := 8;
  Include(Self.btnSourceAdd.Click, Self.btnSourceAdd_Click);
  // 
  // Label3
  // 
  Self.Label3.Location := System.Drawing.Point.Create(8, 104);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(100, 13);
  Self.Label3.TabIndex := 9;
  Self.Label3.Text := 'Оценка качества';
  // 
  // EditCertainty
  // 
  Self.EditCertainty.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.EditCertainty.Location := System.Drawing.Point.Create(8, 120);
  Self.EditCertainty.Name := 'EditCertainty';
  Self.EditCertainty.Size := System.Drawing.Size.Create(337, 21);
  Self.EditCertainty.TabIndex := 3;
  // 
  // cbSource
  // 
  Self.cbSource.Location := System.Drawing.Point.Create(8, 24);
  Self.cbSource.Name := 'cbSource';
  Self.cbSource.Size := System.Drawing.Size.Create(306, 21);
  Self.cbSource.Sorted := True;
  Self.cbSource.TabIndex := 1;
  Include(Self.cbSource.KeyUp, Self.cbSource_KeyUp);
  Include(Self.cbSource.SelectedIndexChanged, Self.cbSource_SelectedIndexChanged);
  // 
  // TfmSourceCitEdit
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(353, 193);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Controls.Add(Self.Label1);
  Self.Controls.Add(Self.EditPage);
  Self.Controls.Add(Self.Label2);
  Self.Controls.Add(Self.btnSourceAdd);
  Self.Controls.Add(Self.Label3);
  Self.Controls.Add(Self.EditCertainty);
  Self.Controls.Add(Self.cbSource);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmSourceCitEdit';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'SourceCitEdit';
  Self.ResumeLayout(False);
end;

constructor TfmSourceCitEdit.Create(aBase: TfmBase);
var
  i: Integer;
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  for i := 0 to 3 do EditCertainty.Items.Add(LSList[TGenEngine.CertaintyAssessments[i]]);

  FSourcesList := TStringList.Create();

  Base.Engine.GetSourcesList(FSourcesList);
  RefreshSourcesList('');

  // SetLang
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  Text := LSList[LSID_WinSourceCitEdit];

  Label2.Text := LSList[LSID_Source];
  Label1.Text := LSList[LSID_Page];
  Label3.Text := LSList[LSID_Certainty];
end;

procedure TfmSourceCitEdit.Dispose(Disposing: Boolean);
begin
  if Disposing then begin
    FSourcesList.Free;
  end;
  inherited Dispose(Disposing);
end;

procedure TfmSourceCitEdit.SetSourceCitation(const Value: TGEDCOMSourceCitation);
begin
  FSourceCitation := Value;

  FTempSrc := TGEDCOMSourceRecord(FSourceCitation.Value);
  if (FTempSrc <> nil)
  then cbSource.SelectedIndex := cbSource.Items.IndexOf(FTempSrc.FiledByEntry);

  EditPage.Text := FSourceCitation.Page;
  EditCertainty.SelectedIndex := FSourceCitation.CertaintyAssessment;
end;

procedure TfmSourceCitEdit.btnSourceAdd_Click(sender: System.Object; e: System.EventArgs);
var
  src: TGEDCOMSourceRecord;
begin
  src := TGEDCOMSourceRecord(Base.SelectRecord(rtSource, []));

  if (src <> nil) then begin
    Base.Engine.GetSourcesList(FSourcesList);
    RefreshSourcesList('');

    cbSource.SelectedIndex := cbSource.Items.IndexOf(src.FiledByEntry);
  end;
end;

procedure TfmSourceCitEdit.btnAccept_Click(sender: System.Object; e: System.EventArgs);
begin
  if (FTempSrc = nil) then begin
    TGKUtils.ShowError('Не задан источник');

    Self.DialogResult := System.Windows.Forms.DialogResult.None;
  end else begin
    FSourceCitation.Value := FTempSrc;
    FSourceCitation.Page := EditPage.Text;
    FSourceCitation.CertaintyAssessment := EditCertainty.SelectedIndex;

    Self.DialogResult := System.Windows.Forms.DialogResult.OK;
  end;
end;

procedure TfmSourceCitEdit.RefreshSourcesList(aFilter: string);
var
  i: Integer;
  st, flt: string;
begin
  flt := '*' + aFilter + '*';

  cbSource.BeginUpdate();
  try
    FTempSrc := nil;

    cbSource.Items.Clear;
    for i := 0 to FSourcesList.Count - 1 do begin
      st := FSourcesList[i];

      if (aFilter = '') or TGenEngine.IsMatchesMask(st, flt)
      then cbSource.Items.Add(TComboItem.Create(st, FSourcesList.Objects[i]));
    end;
  finally
    cbSource.EndUpdate();
  end;
end;

procedure TfmSourceCitEdit.cbSource_KeyUp(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
var
  s: Integer;
begin
  s := cbSource.SelectionStart;
  RefreshSourcesList(cbSource.Text);
  cbSource.SelectionStart := s;
end;

procedure TfmSourceCitEdit.cbSource_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
var
  idx: Integer;
begin
  idx := cbSource.SelectedIndex;

  if (idx < 0)
  then FTempSrc := nil
  else FTempSrc := TGEDCOMSourceRecord(TComboItem(cbSource.Items[idx]).Data);
end;

end.
