unit GKTipsDlg; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  VCLStub, GKMain, GKLangs;

type
  TfmTipsDialog = class(System.Windows.Forms.Form)
  strict private
    Shape1: System.Windows.Forms.Panel;
    ShowCheck: System.Windows.Forms.CheckBox;
    NextTipBtn: System.Windows.Forms.Button;
    btnClose: System.Windows.Forms.Button;
    Shape2: System.Windows.Forms.Panel;
    Shape3: System.Windows.Forms.Panel;
    TitleLabel: System.Windows.Forms.Label;
    Image1: System.Windows.Forms.PictureBox;
    TipWindow: System.Windows.Forms.TextBox;

    FTips: TStringList;

    procedure GetTips();

    procedure InitializeComponent;
    procedure NextTipBtn_Click(sender: System.Object; e: System.EventArgs);
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  public
    constructor Create;

    class function ShowTipsEx(const ACaption: string;
      ShowTipsChecked: Boolean; Tips: TStrings): Boolean;
  end;

implementation

procedure TfmTipsDialog.InitializeComponent;
var
  resources: System.Resources.ResourceManager;
begin
  resources := System.Resources.ResourceManager.Create(TypeOf(TfmTipsDialog));
  Self.Shape1 := System.Windows.Forms.Panel.Create;
  Self.ShowCheck := System.Windows.Forms.CheckBox.Create;
  Self.NextTipBtn := System.Windows.Forms.Button.Create;
  Self.btnClose := System.Windows.Forms.Button.Create;
  Self.Shape2 := System.Windows.Forms.Panel.Create;
  Self.Shape3 := System.Windows.Forms.Panel.Create;
  Self.TitleLabel := System.Windows.Forms.Label.Create;
  Self.Image1 := System.Windows.Forms.PictureBox.Create;
  Self.TipWindow := System.Windows.Forms.TextBox.Create;
  Self.SuspendLayout;
  // 
  // Shape1
  // 
  Self.Shape1.BackColor := System.Drawing.Color.White;
  Self.Shape1.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.Shape1.ForeColor := System.Drawing.Color.Black;
  Self.Shape1.Location := System.Drawing.Point.Create(88, 8);
  Self.Shape1.Name := 'Shape1';
  Self.Shape1.Size := System.Drawing.Size.Create(289, 40);
  Self.Shape1.TabIndex := 0;
  // 
  // ShowCheck
  // 
  Self.ShowCheck.Checked := True;
  Self.ShowCheck.CheckState := System.Windows.Forms.CheckState.Checked;
  Self.ShowCheck.Location := System.Drawing.Point.Create(18, 220);
  Self.ShowCheck.Name := 'ShowCheck';
  Self.ShowCheck.Size := System.Drawing.Size.Create(167, 17);
  Self.ShowCheck.TabIndex := 0;
  Self.ShowCheck.Text := 'Показывать при старте';
  // 
  // NextTipBtn
  // 
  Self.NextTipBtn.Location := System.Drawing.Point.Create(216, 216);
  Self.NextTipBtn.Name := 'NextTipBtn';
  Self.NextTipBtn.Size := System.Drawing.Size.Create(75, 25);
  Self.NextTipBtn.TabIndex := 1;
  Self.NextTipBtn.Text := 'Далее';
  Include(Self.NextTipBtn.Click, Self.NextTipBtn_Click);
  // 
  // btnClose
  // 
  Self.btnClose.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnClose.Location := System.Drawing.Point.Create(296, 216);
  Self.btnClose.Name := 'btnClose';
  Self.btnClose.Size := System.Drawing.Size.Create(75, 25);
  Self.btnClose.TabIndex := 2;
  Self.btnClose.Text := 'Закрыть';
  // 
  // Shape2
  // 
  Self.Shape2.BackColor := System.Drawing.Color.Gray;
  Self.Shape2.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.Shape2.Location := System.Drawing.Point.Create(16, 8);
  Self.Shape2.Name := 'Shape2';
  Self.Shape2.Size := System.Drawing.Size.Create(73, 185);
  Self.Shape2.TabIndex := 1;
  // 
  // Shape3
  // 
  Self.Shape3.BackColor := System.Drawing.Color.White;
  Self.Shape3.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.Shape3.ForeColor := System.Drawing.Color.Black;
  Self.Shape3.Location := System.Drawing.Point.Create(88, 47);
  Self.Shape3.Name := 'Shape3';
  Self.Shape3.Size := System.Drawing.Size.Create(289, 146);
  Self.Shape3.TabIndex := 2;
  // 
  // TitleLabel
  // 
  Self.TitleLabel.BackColor := System.Drawing.Color.White;
  Self.TitleLabel.Font := System.Drawing.Font.Create('Arial', 16, System.Drawing.FontStyle.Bold, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.TitleLabel.Location := System.Drawing.Point.Create(96, 16);
  Self.TitleLabel.Name := 'TitleLabel';
  Self.TitleLabel.Size := System.Drawing.Size.Create(270, 22);
  Self.TitleLabel.TabIndex := 3;
  Self.TitleLabel.Text := 'Вы знаете что...';
  // 
  // Image1
  // 
  Self.Image1.Image := (System.Drawing.Image(resources.GetObject('Image1.Ima' +
    'ge')));
  Self.Image1.Location := System.Drawing.Point.Create(32, 24);
  Self.Image1.Name := 'Image1';
  Self.Image1.Size := System.Drawing.Size.Create(41, 43);
  Self.Image1.TabIndex := 4;
  Self.Image1.TabStop := False;
  // 
  // TipWindow
  // 
  Self.TipWindow.BorderStyle := System.Windows.Forms.BorderStyle.None;
  Self.TipWindow.Location := System.Drawing.Point.Create(98, 57);
  Self.TipWindow.Multiline := True;
  Self.TipWindow.Name := 'TipWindow';
  Self.TipWindow.Size := System.Drawing.Size.Create(265, 127);
  Self.TipWindow.TabIndex := 3;
  Self.TipWindow.Text := '';
  // 
  // TfmTipsDialog
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnClose;
  Self.ClientSize := System.Drawing.Size.Create(389, 252);
  Self.Controls.Add(Self.TitleLabel);
  Self.Controls.Add(Self.Image1);
  Self.Controls.Add(Self.ShowCheck);
  Self.Controls.Add(Self.TipWindow);
  Self.Controls.Add(Self.NextTipBtn);
  Self.Controls.Add(Self.btnClose);
  Self.Controls.Add(Self.Shape1);
  Self.Controls.Add(Self.Shape2);
  Self.Controls.Add(Self.Shape3);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedToolWindow;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmTipsDialog';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := ' ';
  Self.ResumeLayout(False);
end;

constructor TfmTipsDialog.Create;
begin
  inherited Create;
  InitializeComponent;

  FTips := TStringList.Create;

  // SetLang
  btnClose.Text := LSList[LSID_DlgClose];
  ShowCheck.Text := LSList[LSID_StartupTips];
  NextTipBtn.Text := LSList[LSID_Next];
  TitleLabel.Text := LSList[LSID_YouKnowWhat];
end;

procedure TfmTipsDialog.Dispose(Disposing: Boolean);
begin
  if Disposing then begin
    FTips.Free;
  end;
  inherited Dispose(Disposing);
end;

procedure TfmTipsDialog.NextTipBtn_Click(sender: System.Object; e: System.EventArgs);
begin
  GetTips();
end;

procedure TfmTipsDialog.GetTips();
begin
  if (FTips.Count > 0) then begin
    TipWindow.Text := FTips[0];
    FTips.Delete(0);
  end;

  NextTipBtn.Enabled := (FTips.Count > 0);
end;

class function TfmTipsDialog.ShowTipsEx(const ACaption: string;
  ShowTipsChecked: Boolean; Tips: TStrings): Boolean;
var
  dlg: TfmTipsDialog;
begin
  dlg := TfmTipsDialog.Create();
  try
    dlg.ShowCheck.Checked := ShowTipsChecked;
    dlg.Text := ACaption;
    dlg.TitleLabel.Text := ACaption;
    dlg.FTips.Assign(Tips);
    dlg.GetTips();
    dlg.ShowDialog();
    Result := dlg.ShowCheck.Checked;
  finally
    dlg.Free;
  end;
end;

end.
