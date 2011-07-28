unit GKNameEdit; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  GedCom551, GKEngine, GKCommon, GKLangs;

type
  TfmNameEdit = class(System.Windows.Forms.Form)
  strict private
    Label2: System.Windows.Forms.Label;
    edName: System.Windows.Forms.TextBox;
    Label4: System.Windows.Forms.Label;
    edSex: System.Windows.Forms.ComboBox;
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    GroupBox1: System.Windows.Forms.GroupBox;
    Label3: System.Windows.Forms.Label;
    edFPatr: System.Windows.Forms.TextBox;
    Label1: System.Windows.Forms.Label;
    edMPatr: System.Windows.Forms.TextBox;

    FIName: TNamesTable.TName;

    procedure SetIName(const Value: TNamesTable.TName);
    procedure InitializeComponent;
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
    procedure edName_KeyPress(sender: System.Object; e: System.Windows.Forms.KeyPressEventArgs);
  public
    constructor Create;

    property IName: TNamesTable.TName read FIName write SetIName;
  end;

implementation

procedure TfmNameEdit.InitializeComponent;
var
  resources: System.Resources.ResourceManager;
begin
  resources := System.Resources.ResourceManager.Create(TypeOf(TfmNameEdit));
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.edName := System.Windows.Forms.TextBox.Create;
  Self.Label4 := System.Windows.Forms.Label.Create;
  Self.edSex := System.Windows.Forms.ComboBox.Create;
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.GroupBox1 := System.Windows.Forms.GroupBox.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.edFPatr := System.Windows.Forms.TextBox.Create;
  Self.edMPatr := System.Windows.Forms.TextBox.Create;
  Self.GroupBox1.SuspendLayout;
  Self.SuspendLayout;
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(8, 16);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(25, 13);
  Self.Label2.TabIndex := 0;
  Self.Label2.Text := 'Имя';
  // 
  // edName
  // 
  Self.edName.Location := System.Drawing.Point.Create(72, 8);
  Self.edName.Name := 'edName';
  Self.edName.Size := System.Drawing.Size.Create(193, 21);
  Self.edName.TabIndex := 0;
  Self.edName.Text := '';
  Include(Self.edName.KeyPress, Self.edName_KeyPress);
  // 
  // Label4
  // 
  Self.Label4.Location := System.Drawing.Point.Create(8, 48);
  Self.Label4.Name := 'Label4';
  Self.Label4.Size := System.Drawing.Size.Create(25, 13);
  Self.Label4.TabIndex := 1;
  Self.Label4.Text := 'Пол';
  // 
  // edSex
  // 
  Self.edSex.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.edSex.Location := System.Drawing.Point.Create(72, 40);
  Self.edSex.Name := 'edSex';
  Self.edSex.TabIndex := 1;
  // 
  // btnAccept
  // 
  Self.btnAccept.Image := (System.Drawing.Image(resources.GetObject('btnAcce' +
    'pt.Image')));
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(96, 168);
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
  Self.btnCancel.Image := (System.Drawing.Image(resources.GetObject('btnCanc' +
    'el.Image')));
  Self.btnCancel.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnCancel.Location := System.Drawing.Point.Create(184, 168);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 4;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // GroupBox1
  // 
  Self.GroupBox1.Controls.Add(Self.Label3);
  Self.GroupBox1.Controls.Add(Self.Label1);
  Self.GroupBox1.Controls.Add(Self.edFPatr);
  Self.GroupBox1.Controls.Add(Self.edMPatr);
  Self.GroupBox1.Location := System.Drawing.Point.Create(8, 72);
  Self.GroupBox1.Name := 'GroupBox1';
  Self.GroupBox1.Size := System.Drawing.Size.Create(257, 78);
  Self.GroupBox1.TabIndex := 2;
  Self.GroupBox1.TabStop := False;
  Self.GroupBox1.Text := 'Отчества';
  // 
  // Label3
  // 
  Self.Label3.Location := System.Drawing.Point.Create(8, 24);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(50, 13);
  Self.Label3.TabIndex := 0;
  Self.Label3.Text := 'Женское';
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 56);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(50, 13);
  Self.Label1.TabIndex := 1;
  Self.Label1.Text := 'Мужское';
  // 
  // edFPatr
  // 
  Self.edFPatr.Location := System.Drawing.Point.Create(64, 16);
  Self.edFPatr.Name := 'edFPatr';
  Self.edFPatr.Size := System.Drawing.Size.Create(185, 21);
  Self.edFPatr.TabIndex := 0;
  Self.edFPatr.Text := '';
  Include(Self.edFPatr.KeyPress, Self.edName_KeyPress);
  // 
  // edMPatr
  // 
  Self.edMPatr.Location := System.Drawing.Point.Create(64, 48);
  Self.edMPatr.Name := 'edMPatr';
  Self.edMPatr.Size := System.Drawing.Size.Create(185, 21);
  Self.edMPatr.TabIndex := 1;
  Self.edMPatr.Text := '';
  Include(Self.edMPatr.KeyPress, Self.edName_KeyPress);
  // 
  // TfmNameEdit
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(273, 201);
  Self.Controls.Add(Self.Label2);
  Self.Controls.Add(Self.edName);
  Self.Controls.Add(Self.Label4);
  Self.Controls.Add(Self.edSex);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Controls.Add(Self.GroupBox1);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.Name := 'TfmNameEdit';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Имя';
  Self.GroupBox1.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmNameEdit.Create;
var
  sx: TGEDCOMObject.TGEDCOMSex;
begin
  inherited Create;
  InitializeComponent;

  for sx := Low(TGEDCOMObject.TGEDCOMSex) to High(TGEDCOMObject.TGEDCOMSex) do
    edSex.Items.Add(TGenEngine.SexStr(sx));

  // SetLang
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  Text := LSList[LSID_Name];

  Label2.Text := LSList[LSID_Name];
  Label4.Text := LSList[LSID_Sex];
  GroupBox1.Text := LSList[LSID_Patronymic];
  Label3.Text := LSList[LSID_PatFemale];
  Label1.Text := LSList[LSID_PatMale];
end;

procedure TfmNameEdit.SetIName(const Value: TNamesTable.TName);
begin
  FIName := Value;

  if (FIName = nil) then begin
    edName.Text := '';
    edSex.SelectedIndex := 0;
    edFPatr.Text := '';
    edMPatr.Text := '';
  end else begin
    edName.Text := FIName.Name;
    edSex.SelectedIndex := Ord(FIName.Sex);
    edFPatr.Text := FIName.F_Patronymic;
    edMPatr.Text := FIName.M_Patronymic;
  end;
end;

procedure TfmNameEdit.btnAccept_Click(sender: System.Object; e: System.EventArgs);
begin
  FIName.Name := edName.Text;
  FIName.Sex := TGEDCOMObject.TGEDCOMSex(edSex.SelectedIndex);
  FIName.F_Patronymic := edFPatr.Text;
  FIName.M_Patronymic := edMPatr.Text;

  DialogResult := System.Windows.Forms.DialogResult.OK;
end;

procedure TfmNameEdit.edName_KeyPress(sender: System.Object; e: System.Windows.Forms.KeyPressEventArgs);
begin
  if (e.KeyChar = '/') then e.Handled := True;
end;

end.
