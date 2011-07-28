unit GKNoteEdit; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  VCLStub, GedCom551, GKBase, GKLangs;

type
  TfmNoteEdit = class(System.Windows.Forms.Form)
  strict private
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    mmNote: System.Windows.Forms.TextBox;

    FBase: TfmBase;
    FNoteRecord: TGEDCOMNoteRecord;

    procedure SetNoteRecord(const Value: TGEDCOMNoteRecord);

    procedure InitializeComponent;
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property NoteRecord: TGEDCOMNoteRecord read FNoteRecord write SetNoteRecord;
  end;

implementation

procedure TfmNoteEdit.InitializeComponent;
var
  resources: System.Resources.ResourceManager;
begin
  resources := System.Resources.ResourceManager.Create(TypeOf(TfmNoteEdit));
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.mmNote := System.Windows.Forms.TextBox.Create;
  Self.SuspendLayout;
  // 
  // btnAccept
  // 
  Self.btnAccept.Image := (System.Drawing.Image(resources.GetObject('btnAcce' +
    'pt.Image')));
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(224, 216);
  Self.btnAccept.Name := 'btnAccept';
  Self.btnAccept.Size := System.Drawing.Size.Create(81, 25);
  Self.btnAccept.TabIndex := 1;
  Self.btnAccept.Text := 'Accept';
  Self.btnAccept.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  Include(Self.btnAccept.Click, Self.btnAccept_Click);
  // 
  // btnCancel
  // 
  Self.btnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnCancel.Image := (System.Drawing.Image(resources.GetObject('btnCanc' +
    'el.Image')));
  Self.btnCancel.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnCancel.Location := System.Drawing.Point.Create(312, 216);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 2;
  Self.btnCancel.Text := 'Cancel';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // mmNote
  // 
  Self.mmNote.Location := System.Drawing.Point.Create(8, 8);
  Self.mmNote.Multiline := True;
  Self.mmNote.Name := 'mmNote';
  Self.mmNote.ScrollBars := System.Windows.Forms.ScrollBars.Both;
  Self.mmNote.Size := System.Drawing.Size.Create(385, 193);
  Self.mmNote.TabIndex := 0;
  Self.mmNote.Text := '';
  // 
  // TfmNoteEdit
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(402, 249);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Controls.Add(Self.mmNote);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmNoteEdit';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'TfmNoteEdit';
  Self.ResumeLayout(False);
end;

constructor TfmNoteEdit.Create(aBase: TfmBase);
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  // SetLang
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];
  Text := LSList[LSID_Note];
end;

procedure TfmNoteEdit.SetNoteRecord(const Value: TGEDCOMNoteRecord);
begin
  FNoteRecord := Value;
  mmNote.Text := FNoteRecord.Notes.Text.Trim();
end;

procedure TfmNoteEdit.btnAccept_Click(sender: System.Object; e: System.EventArgs);
var
  strs: TStrings;
begin
  try
    strs := StrArrayToStrings(mmNote.Lines);
    FNoteRecord.Notes := strs;
  finally
    strs.Free;
  end;

  Base.ChangeRecord(FNoteRecord);

  Self.DialogResult := System.Windows.Forms.DialogResult.OK;
end;

end.
