unit GKPersonNew; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  VCLStub, GedCom551, GKEngine, GKCommon, GKMain, GKLangs;

type
  TfmPersonNew = class(System.Windows.Forms.Form)
    edFamily: System.Windows.Forms.TextBox;
    edName: System.Windows.Forms.TextBox;
    edPatronymic: System.Windows.Forms.ComboBox;
    EditSex: System.Windows.Forms.ComboBox;
  strict private
    Label1: System.Windows.Forms.Label;
    Label2: System.Windows.Forms.Label;
    Label3: System.Windows.Forms.Label;
    Label4: System.Windows.Forms.Label;
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;

    FTarget: TGEDCOMIndividualRecord;
    FTargetMode: TGenEngine.TTargetMode;

    procedure SetTarget(const Value: TGEDCOMIndividualRecord);
    procedure InitializeComponent;
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
    procedure edFamily_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
    procedure edFamily_KeyPress(sender: System.Object; e: System.Windows.Forms.KeyPressEventArgs);
  public
    constructor Create;

    property Target: TGEDCOMIndividualRecord read FTarget write SetTarget;
    property TargetMode: TGenEngine.TTargetMode read FTargetMode write FTargetMode;
  end;

implementation

procedure TfmPersonNew.InitializeComponent;
begin
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.edFamily := System.Windows.Forms.TextBox.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.edName := System.Windows.Forms.TextBox.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.edPatronymic := System.Windows.Forms.ComboBox.Create;
  Self.Label4 := System.Windows.Forms.Label.Create;
  Self.EditSex := System.Windows.Forms.ComboBox.Create;
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.SuspendLayout;
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 16);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(55, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Фамилия';
  // 
  // edFamily
  // 
  Self.edFamily.Location := System.Drawing.Point.Create(72, 8);
  Self.edFamily.Name := 'edFamily';
  Self.edFamily.Size := System.Drawing.Size.Create(185, 21);
  Self.edFamily.TabIndex := 0;
  Self.edFamily.Text := '';
  Include(Self.edFamily.KeyDown, Self.edFamily_KeyDown);
  Include(Self.edFamily.KeyPress, Self.edFamily_KeyPress);
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(8, 40);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(55, 13);
  Self.Label2.TabIndex := 1;
  Self.Label2.Text := 'Имя';
  // 
  // edName
  // 
  Self.edName.Location := System.Drawing.Point.Create(72, 32);
  Self.edName.Name := 'edName';
  Self.edName.Size := System.Drawing.Size.Create(185, 21);
  Self.edName.TabIndex := 1;
  Self.edName.Text := '';
  Include(Self.edName.KeyDown, Self.edFamily_KeyDown);
  Include(Self.edName.KeyPress, Self.edFamily_KeyPress);
  // 
  // Label3
  // 
  Self.Label3.Location := System.Drawing.Point.Create(8, 64);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(55, 13);
  Self.Label3.TabIndex := 2;
  Self.Label3.Text := 'Отчество';
  // 
  // edPatronymic
  // 
  Self.edPatronymic.Location := System.Drawing.Point.Create(72, 56);
  Self.edPatronymic.Name := 'edPatronymic';
  Self.edPatronymic.Size := System.Drawing.Size.Create(185, 21);
  Self.edPatronymic.TabIndex := 2;
  Include(Self.edPatronymic.KeyDown, Self.edFamily_KeyDown);
  Include(Self.edPatronymic.KeyPress, Self.edFamily_KeyPress);
  // 
  // Label4
  // 
  Self.Label4.Location := System.Drawing.Point.Create(8, 88);
  Self.Label4.Name := 'Label4';
  Self.Label4.Size := System.Drawing.Size.Create(55, 13);
  Self.Label4.TabIndex := 3;
  Self.Label4.Text := 'Пол';
  // 
  // EditSex
  // 
  Self.EditSex.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.EditSex.Location := System.Drawing.Point.Create(72, 80);
  Self.EditSex.Name := 'EditSex';
  Self.EditSex.TabIndex := 3;
  // 
  // btnAccept
  // 
  Self.btnAccept.Location := System.Drawing.Point.Create(48, 120);
  Self.btnAccept.Name := 'btnAccept';
  Self.btnAccept.Size := System.Drawing.Size.Create(81, 25);
  Self.btnAccept.TabIndex := 4;
  Self.btnAccept.Text := 'Принять';
  Include(Self.btnAccept.Click, Self.btnAccept_Click);
  // 
  // btnCancel
  // 
  Self.btnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnCancel.Location := System.Drawing.Point.Create(136, 120);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 5;
  Self.btnCancel.Text := 'Отменить';
  // 
  // TfmPersonNew
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(266, 153);
  Self.Controls.Add(Self.Label1);
  Self.Controls.Add(Self.edFamily);
  Self.Controls.Add(Self.edName);
  Self.Controls.Add(Self.Label2);
  Self.Controls.Add(Self.Label3);
  Self.Controls.Add(Self.edPatronymic);
  Self.Controls.Add(Self.Label4);
  Self.Controls.Add(Self.EditSex);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmPersonNew';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Новая персональная запись';
  Self.ResumeLayout(False);
end;

constructor TfmPersonNew.Create;
var
  sx: TGEDCOMObject.TGEDCOMSex;
begin
  inherited Create;
  InitializeComponent;

  for sx := Low(TGEDCOMObject.TGEDCOMSex) to High(TGEDCOMObject.TGEDCOMSex) do
    EditSex.Items.Add(TGenEngine.SexStr(sx));

  /// SetLang
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  Text := LSList[LSID_WinPersonNew];

  Label1.Text := LSList[LSID_Surname];
  Label2.Text := LSList[LSID_Name];
  Label3.Text := LSList[LSID_Patronymic];
  Label4.Text := LSList[LSID_Sex];
end;

procedure TfmPersonNew.edFamily_KeyPress(sender: System.Object; e: System.Windows.Forms.KeyPressEventArgs);
begin
  if (e.KeyChar = '/') then e.Handled := True;
end;

procedure TfmPersonNew.edFamily_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
var
  ss, st: string;
begin
  if (e.KeyCode = Keys.Down) and (e.Control) then begin
    ss := TextBox(Sender).Text;
    st := ss.ToLower();
    st[1] := ss[1];
    TextBox(Sender).Text := st;
  end;
end;

procedure TfmPersonNew.btnAccept_Click(sender: System.Object; e: System.EventArgs);
begin
  Self.DialogResult := System.Windows.Forms.DialogResult.OK;
end;

procedure TfmPersonNew.SetTarget(const Value: TGEDCOMIndividualRecord);
var
  iFamily, iName, iPatronymic: string;
  names: TNamesTable;
  sx: TGEDCOMObject.TGEDCOMSex;
begin
  FTarget := Value;

  names := fmGEDKeeper.NamesTable;

  if (FTarget <> nil) then begin
    TGenEngine.GetNameParts(FTarget, iFamily, iName, iPatronymic);

    edFamily.Text := iFamily;

    case FTargetMode of
      tmNone: ;

      tmAncestor: begin
        edPatronymic.Items.Add(names.GetPatronymicByName(iName, svMale));
        edPatronymic.Items.Add(names.GetPatronymicByName(iName, svFemale));
      end;

      tmDescendant: begin
        sx := TGEDCOMObject.TGEDCOMSex(EditSex.SelectedIndex);

        case sx of
          svMale: edName.Text := names.GetNameByPatronymic(iPatronymic, svMale);
          svFemale: edFamily.Text := '(' + edFamily.Text + ')';
        end;
      end;
    end;
  end;
end;

end.
