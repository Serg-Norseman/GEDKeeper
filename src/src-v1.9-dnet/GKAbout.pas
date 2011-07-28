unit GKAbout; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.Windows.Forms, System.Resources,
  GKUtils, GKLangs;

type
  TfmAbout = class(System.Windows.Forms.Form)
  strict private
    LabelProduct: System.Windows.Forms.Label;
    LabelVersion: System.Windows.Forms.Label;
    LabelCopyright: System.Windows.Forms.Label;
    LabelCite: System.Windows.Forms.Label;
    LabelMail: System.Windows.Forms.Label;
    btnClose: System.Windows.Forms.Button;

    procedure LabelMail_Click(sender: System.Object; e: System.EventArgs);
    procedure InitializeComponent;
  public
    constructor Create;
    class procedure ShowAbout(AppName, AppVersion: string);
  end;

implementation

class procedure TfmAbout.ShowAbout(AppName, AppVersion: string);
var
  dlg: TfmAbout;
begin
  dlg := TfmAbout.Create();
  try
    dlg.LabelProduct.Text := AppName;
    dlg.LabelVersion.Text := 'Version ' + AppVersion;
    dlg.ShowDialog();
  finally
    dlg.Free;
  end;
end;

procedure TfmAbout.InitializeComponent;
var
  resources: System.Resources.ResourceManager;
begin
  resources := System.Resources.ResourceManager.Create(TypeOf(TfmAbout));
  Self.LabelProduct := System.Windows.Forms.Label.Create;
  Self.LabelVersion := System.Windows.Forms.Label.Create;
  Self.btnClose := System.Windows.Forms.Button.Create;
  Self.LabelCopyright := System.Windows.Forms.Label.Create;
  Self.LabelMail := System.Windows.Forms.Label.Create;
  Self.LabelCite := System.Windows.Forms.Label.Create;
  Self.SuspendLayout;
  // 
  // LabelProduct
  // 
  Self.LabelProduct.AutoSize := True;
  Self.LabelProduct.Font := System.Drawing.Font.Create('Times New Roman', 20.25, 
      System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.LabelProduct.Location := System.Drawing.Point.Create(8, 8);
  Self.LabelProduct.Name := 'LabelProduct';
  Self.LabelProduct.Size := System.Drawing.Size.Create(23, 35);
  Self.LabelProduct.TabIndex := 0;
  Self.LabelProduct.Text := '?';
  // 
  // LabelVersion
  // 
  Self.LabelVersion.AutoSize := True;
  Self.LabelVersion.Font := System.Drawing.Font.Create('Times New Roman', 11.25, 
      System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.LabelVersion.Location := System.Drawing.Point.Create(8, 56);
  Self.LabelVersion.Name := 'LabelVersion';
  Self.LabelVersion.Size := System.Drawing.Size.Create(57, 21);
  Self.LabelVersion.TabIndex := 1;
  Self.LabelVersion.Text := 'Version';
  // 
  // btnClose
  // 
  Self.btnClose.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnClose.Image := (System.Drawing.Image(resources.GetObject('btnClose' +
    '.Image')));
  Self.btnClose.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnClose.Location := System.Drawing.Point.Create(288, 272);
  Self.btnClose.Name := 'btnClose';
  Self.btnClose.Size := System.Drawing.Size.Create(81, 25);
  Self.btnClose.TabIndex := 0;
  Self.btnClose.Text := 'Закрыть';
  Self.btnClose.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // LabelCopyright
  // 
  Self.LabelCopyright.AutoSize := True;
  Self.LabelCopyright.Font := System.Drawing.Font.Create('Times New Roman', 11.25, 
      System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.LabelCopyright.Location := System.Drawing.Point.Create(8, 88);
  Self.LabelCopyright.Name := 'LabelCopyright';
  Self.LabelCopyright.Size := System.Drawing.Size.Create(232, 21);
  Self.LabelCopyright.TabIndex := 1;
  Self.LabelCopyright.Text := 'Copyright © Serg V. Zhdanovskih';
  // 
  // LabelMail
  // 
  Self.LabelMail.AutoSize := True;
  Self.LabelMail.Font := System.Drawing.Font.Create('Times New Roman', 8.25, 
      (System.Drawing.FontStyle((System.Drawing.FontStyle.Bold or System.Drawing.FontStyle.Underline))), 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.LabelMail.ForeColor := System.Drawing.Color.Blue;
  Self.LabelMail.Location := System.Drawing.Point.Create(8, 256);
  Self.LabelMail.Name := 'LabelMail';
  Self.LabelMail.Size := System.Drawing.Size.Create(126, 16);
  Self.LabelMail.TabIndex := 2;
  Self.LabelMail.Text := 'http://gedkeeper.ucoz.ru/';
  Include(Self.LabelMail.Click, Self.LabelMail_Click);
  // 
  // LabelCite
  // 
  Self.LabelCite.Font := System.Drawing.Font.Create('Times New Roman', 12, System.Drawing.FontStyle.Bold, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.LabelCite.Location := System.Drawing.Point.Create(8, 144);
  Self.LabelCite.Name := 'LabelCite';
  Self.LabelCite.Size := System.Drawing.Size.Create(361, 97);
  Self.LabelCite.TabIndex := 3;
  Self.LabelCite.Text := '«История рода - это есть история Отечества» «Неува' +
  'жение к предкам - есть первый признак дикости и безнравственности» (Алекс' +
  'андр Сергеевич Пушкин)';
  Self.LabelCite.TextAlign := System.Drawing.ContentAlignment.TopRight;
  // 
  // TfmAbout
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnClose;
  Self.ClientSize := System.Drawing.Size.Create(378, 305);
  Self.Controls.Add(Self.LabelProduct);
  Self.Controls.Add(Self.LabelVersion);
  Self.Controls.Add(Self.LabelCopyright);
  Self.Controls.Add(Self.LabelMail);
  Self.Controls.Add(Self.btnClose);
  Self.Controls.Add(Self.LabelCite);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmAbout';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'О программе';
  Self.ResumeLayout(False);
end;

constructor TfmAbout.Create;
begin
  inherited Create;
  InitializeComponent;

  LabelCite.Text :=
    '«История рода - это есть история Отечества»'+#13#10+
    '«Неуважение к предкам - есть первый признак дикости и безнравственности»'+#13#10+
    '(Александр Сергеевич Пушкин)';

  /// SetLang
  Text := LSList[LSID_MIAbout];
  btnClose.Text := LSList[LSID_DlgClose];
end;

procedure TfmAbout.LabelMail_Click(sender: System.Object; e: System.EventArgs);
begin
  TGKUtils.LoadExtFile(LabelMail.Text);
end;

end.
