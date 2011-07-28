unit GKUserRefEdit; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms,
  GedCom551, GKBase, GKMain, GKEngine, GKLangs;

type
  TfmUserRefEdit = class(System.Windows.Forms.Form)
  strict private
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    Label1: System.Windows.Forms.Label;
    EditRef: System.Windows.Forms.ComboBox;
    Label2: System.Windows.Forms.Label;
    EditType: System.Windows.Forms.ComboBox;

    FBase: TfmBase;
    FUserRef: TGEDCOMUserReference;

    procedure SetUserRef(const Value: TGEDCOMUserReference);
    procedure InitializeComponent;
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property UserRef: TGEDCOMUserReference read FUserRef write SetUserRef;
  end;

implementation

procedure TfmUserRefEdit.InitializeComponent;
begin
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.EditRef := System.Windows.Forms.ComboBox.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.EditType := System.Windows.Forms.ComboBox.Create;
  Self.SuspendLayout;
  // 
  // btnAccept
  // 
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(176, 112);
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
  Self.btnCancel.Location := System.Drawing.Point.Create(264, 112);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 3;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 8);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(205, 13);
  Self.Label1.TabIndex := 4;
  Self.Label1.Text := 'Сноска/ссылка/пометка/комментарий';
  // 
  // EditRef
  // 
  Self.EditRef.Location := System.Drawing.Point.Create(8, 24);
  Self.EditRef.Name := 'EditRef';
  Self.EditRef.Size := System.Drawing.Size.Create(337, 21);
  Self.EditRef.TabIndex := 0;
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(8, 56);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(25, 13);
  Self.Label2.TabIndex := 5;
  Self.Label2.Text := 'Тип';
  // 
  // EditType
  // 
  Self.EditType.Location := System.Drawing.Point.Create(8, 72);
  Self.EditType.Name := 'EditType';
  Self.EditType.Size := System.Drawing.Size.Create(337, 21);
  Self.EditType.TabIndex := 1;
  // 
  // TfmUserRefEdit
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(353, 145);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Controls.Add(Self.Label1);
  Self.Controls.Add(Self.EditRef);
  Self.Controls.Add(Self.Label2);
  Self.Controls.Add(Self.EditType);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmUserRefEdit';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Пользовательская сноска';
  Self.ResumeLayout(False);
end;

constructor TfmUserRefEdit.Create(aBase: TfmBase);
var
  ur: TGenEngine.TUserRef;
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  for ur := Low(TGenEngine.TUserRef) to High(TGenEngine.TUserRef) do
    EditRef.Items.Add(TGenEngine.UserRefs[ur].Name);

  /// SetLang
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  Text := LSList[LSID_WinUserRefEdit];

  Label1.Text := LSList[LSID_Reference];
  Label2.Text := LSList[LSID_Type];
end;

procedure TfmUserRefEdit.SetUserRef(const Value: TGEDCOMUserReference);
begin
  FUserRef := Value;

  EditRef.Text := FUserRef.StringValue;
  EditType.Text := FUserRef.ReferenceType;
end;

procedure TfmUserRefEdit.btnAccept_Click(sender: System.Object; e: System.EventArgs);
begin
  FUserRef.StringValue := EditRef.Text;
  FUserRef.ReferenceType := EditType.Text;

  Self.DialogResult := System.Windows.Forms.DialogResult.OK;
end;

end.
