unit GKFileProperties; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  VCLStub, GedCom551, GKBase, GKLangs;

type
  TfmFileProperties = class(System.Windows.Forms.Form)
  strict private
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    SheetAuthor: System.Windows.Forms.TabPage;
    Label1: System.Windows.Forms.Label;
    Label2: System.Windows.Forms.Label;
    Label3: System.Windows.Forms.Label;
    EditName: System.Windows.Forms.TextBox;
    EditTel: System.Windows.Forms.TextBox;
    MemoAddress: System.Windows.Forms.TextBox;
    SheetAdvanced: System.Windows.Forms.TabPage;
    CheckAdvanced: System.Windows.Forms.CheckBox;
    Label4: System.Windows.Forms.Label;
    edExtName: System.Windows.Forms.TextBox;

    FBase: TfmBase;

    procedure InitializeComponent;
    procedure UpdateControls();
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
  public
    var
      PageControl1: System.Windows.Forms.TabControl;

    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
  end;

implementation

procedure TfmFileProperties.InitializeComponent;
var
  resources: System.Resources.ResourceManager;
begin
  resources := System.Resources.ResourceManager.Create(TypeOf(TfmFileProperties));
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.PageControl1 := System.Windows.Forms.TabControl.Create;
  Self.SheetAuthor := System.Windows.Forms.TabPage.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.EditName := System.Windows.Forms.TextBox.Create;
  Self.EditTel := System.Windows.Forms.TextBox.Create;
  Self.MemoAddress := System.Windows.Forms.TextBox.Create;
  Self.SheetAdvanced := System.Windows.Forms.TabPage.Create;
  Self.Label4 := System.Windows.Forms.Label.Create;
  Self.CheckAdvanced := System.Windows.Forms.CheckBox.Create;
  Self.edExtName := System.Windows.Forms.TextBox.Create;
  Self.PageControl1.SuspendLayout;
  Self.SheetAuthor.SuspendLayout;
  Self.SheetAdvanced.SuspendLayout;
  Self.SuspendLayout;
  // 
  // btnAccept
  // 
  Self.btnAccept.Image := (System.Drawing.Image(resources.GetObject('btnAcce' +
    'pt.Image')));
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(272, 296);
  Self.btnAccept.Name := 'btnAccept';
  Self.btnAccept.Size := System.Drawing.Size.Create(81, 25);
  Self.btnAccept.TabIndex := 0;
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
  Self.btnCancel.Location := System.Drawing.Point.Create(360, 296);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 1;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // PageControl1
  // 
  Self.PageControl1.Controls.Add(Self.SheetAuthor);
  Self.PageControl1.Controls.Add(Self.SheetAdvanced);
  Self.PageControl1.Location := System.Drawing.Point.Create(8, 8);
  Self.PageControl1.Name := 'PageControl1';
  Self.PageControl1.SelectedIndex := 0;
  Self.PageControl1.Size := System.Drawing.Size.Create(433, 273);
  Self.PageControl1.TabIndex := 2;
  // 
  // SheetAuthor
  // 
  Self.SheetAuthor.Controls.Add(Self.Label1);
  Self.SheetAuthor.Controls.Add(Self.Label2);
  Self.SheetAuthor.Controls.Add(Self.Label3);
  Self.SheetAuthor.Controls.Add(Self.EditName);
  Self.SheetAuthor.Controls.Add(Self.EditTel);
  Self.SheetAuthor.Controls.Add(Self.MemoAddress);
  Self.SheetAuthor.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetAuthor.Name := 'SheetAuthor';
  Self.SheetAuthor.Size := System.Drawing.Size.Create(425, 247);
  Self.SheetAuthor.TabIndex := 0;
  Self.SheetAuthor.Text := 'Автор';
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 8);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(30, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Имя';
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(8, 32);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(40, 13);
  Self.Label2.TabIndex := 1;
  Self.Label2.Text := 'Адрес';
  // 
  // Label3
  // 
  Self.Label3.Location := System.Drawing.Point.Create(8, 152);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(50, 13);
  Self.Label3.TabIndex := 2;
  Self.Label3.Text := 'Телефон';
  // 
  // EditName
  // 
  Self.EditName.Location := System.Drawing.Point.Create(64, 8);
  Self.EditName.Name := 'EditName';
  Self.EditName.Size := System.Drawing.Size.Create(353, 21);
  Self.EditName.TabIndex := 0;
  Self.EditName.Text := '';
  // 
  // EditTel
  // 
  Self.EditTel.Location := System.Drawing.Point.Create(64, 152);
  Self.EditTel.Name := 'EditTel';
  Self.EditTel.Size := System.Drawing.Size.Create(353, 21);
  Self.EditTel.TabIndex := 1;
  Self.EditTel.Text := '';
  // 
  // MemoAddress
  // 
  Self.MemoAddress.Location := System.Drawing.Point.Create(64, 32);
  Self.MemoAddress.Multiline := True;
  Self.MemoAddress.Name := 'MemoAddress';
  Self.MemoAddress.Size := System.Drawing.Size.Create(353, 113);
  Self.MemoAddress.TabIndex := 2;
  Self.MemoAddress.Text := '';
  // 
  // SheetAdvanced
  // 
  Self.SheetAdvanced.Controls.Add(Self.Label4);
  Self.SheetAdvanced.Controls.Add(Self.CheckAdvanced);
  Self.SheetAdvanced.Controls.Add(Self.edExtName);
  Self.SheetAdvanced.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetAdvanced.Name := 'SheetAdvanced';
  Self.SheetAdvanced.Size := System.Drawing.Size.Create(425, 247);
  Self.SheetAdvanced.TabIndex := 1;
  Self.SheetAdvanced.Text := 'Расширение проекта';
  // 
  // Label4
  // 
  Self.Label4.Location := System.Drawing.Point.Create(8, 40);
  Self.Label4.Name := 'Label4';
  Self.Label4.Size := System.Drawing.Size.Create(200, 13);
  Self.Label4.TabIndex := 0;
  Self.Label4.Text := 'Название архива и папки хранилища';
  // 
  // CheckAdvanced
  // 
  Self.CheckAdvanced.Location := System.Drawing.Point.Create(8, 8);
  Self.CheckAdvanced.Name := 'CheckAdvanced';
  Self.CheckAdvanced.Size := System.Drawing.Size.Create(409, 17);
  Self.CheckAdvanced.TabIndex := 0;
  Self.CheckAdvanced.Text := 'Поддержка расширения (архив, хранилище файлов)';
  // 
  // edExtName
  // 
  Self.edExtName.Location := System.Drawing.Point.Create(8, 56);
  Self.edExtName.Name := 'edExtName';
  Self.edExtName.Size := System.Drawing.Size.Create(225, 21);
  Self.edExtName.TabIndex := 1;
  Self.edExtName.Text := '';
  // 
  // TfmFileProperties
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(449, 329);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Controls.Add(Self.PageControl1);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmFileProperties';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Свойства файла';
  Self.PageControl1.ResumeLayout(False);
  Self.SheetAuthor.ResumeLayout(False);
  Self.SheetAdvanced.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmFileProperties.Create(aBase: TfmBase);
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  UpdateControls();

  /// SetLang
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  SheetAuthor.Text := LSList[LSID_Author];
  Label1.Text := LSList[LSID_Name];
  Label2.Text := LSList[LSID_Address];
  Label3.Text := LSList[LSID_Telephone];

  SheetAdvanced.Text := LSList[LSID_Advanced];
  CheckAdvanced.Text := LSList[LSID_AdvancedSupport];
  Label4.Text := LSList[LSID_ExtName];
end;

procedure TfmFileProperties.btnAccept_Click(sender: System.Object; e: System.EventArgs);
var
  submitter: TGEDCOMSubmitterRecord;
  strs: TStrings;
begin
  submitter := Base.Engine.GetSubmitter();
  submitter.Name.StringValue := EditName.Text;

  try
    strs := StrArrayToStrings(MemoAddress.Lines);
    submitter.Address.Address := strs;
  finally
    strs.Free;
  end;

  submitter.Address.PhoneNumbers[0] := EditTel.Text;
  submitter.ChangeDate.ChangeDateTime := DateTime.Now;

  Base.Engine.IsAdvanced := CheckAdvanced.Checked;
  Base.Engine.ExtName := edExtName.Text;

  Base.Modified := True;

  Self.DialogResult := System.Windows.Forms.DialogResult.OK;
end;

procedure TfmFileProperties.UpdateControls();
var
  submitter: TGEDCOMSubmitterRecord;
begin
  submitter := Base.Engine.GetSubmitter();

  EditName.Text := submitter.Name.FullName;
  MemoAddress.Text := submitter.Address.Address.Text;
  EditTel.Text := submitter.Address.PhoneNumbers[0];

  CheckAdvanced.Checked := Base.Engine.IsAdvanced;
  edExtName.Text := Base.Engine.GetSpecExtName();
end;

end.
