unit GKSexCheck; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  GedCom551, GKEngine, GKCommon, GKMain, GKLangs;

type
  TfmSexCheck = class(System.Windows.Forms.Form)
  strict private
    edName: System.Windows.Forms.TextBox;
    GroupBox1: System.Windows.Forms.GroupBox;
    sbNone: System.Windows.Forms.RadioButton;
    sbMale: System.Windows.Forms.RadioButton;
    sbFemale: System.Windows.Forms.RadioButton;
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;

    procedure InitializeComponent;
  public
    constructor Create;

    class function DefineSex(iName, iPatr: string; aNamesTable: TNamesTable): TGEDCOMObject.TGEDCOMSex;
    class procedure CheckPersonSex(iRec: TGEDCOMIndividualRecord; aNamesTable: TNamesTable);
  end;

implementation

{ TfmSexCheck }

procedure TfmSexCheck.InitializeComponent;
var
  resources: System.Resources.ResourceManager;
begin
  resources := System.Resources.ResourceManager.Create(TypeOf(TfmSexCheck));
  Self.edName := System.Windows.Forms.TextBox.Create;
  Self.GroupBox1 := System.Windows.Forms.GroupBox.Create;
  Self.sbNone := System.Windows.Forms.RadioButton.Create;
  Self.sbMale := System.Windows.Forms.RadioButton.Create;
  Self.sbFemale := System.Windows.Forms.RadioButton.Create;
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.GroupBox1.SuspendLayout;
  Self.SuspendLayout;
  // 
  // edName
  // 
  Self.edName.Location := System.Drawing.Point.Create(8, 8);
  Self.edName.Name := 'edName';
  Self.edName.ReadOnly := True;
  Self.edName.Size := System.Drawing.Size.Create(345, 21);
  Self.edName.TabIndex := 0;
  Self.edName.Text := '';
  // 
  // GroupBox1
  // 
  Self.GroupBox1.Controls.Add(Self.sbNone);
  Self.GroupBox1.Controls.Add(Self.sbMale);
  Self.GroupBox1.Controls.Add(Self.sbFemale);
  Self.GroupBox1.Location := System.Drawing.Point.Create(8, 35);
  Self.GroupBox1.Name := 'GroupBox1';
  Self.GroupBox1.Size := System.Drawing.Size.Create(345, 49);
  Self.GroupBox1.TabIndex := 1;
  Self.GroupBox1.TabStop := False;
  Self.GroupBox1.Text := 'Пол';
  // 
  // sbNone
  // 
  Self.sbNone.Location := System.Drawing.Point.Create(8, 16);
  Self.sbNone.Name := 'sbNone';
  Self.sbNone.Size := System.Drawing.Size.Create(105, 22);
  Self.sbNone.TabIndex := 0;
  Self.sbNone.Text := '?';
  // 
  // sbMale
  // 
  Self.sbMale.Location := System.Drawing.Point.Create(119, 16);
  Self.sbMale.Name := 'sbMale';
  Self.sbMale.Size := System.Drawing.Size.Create(105, 22);
  Self.sbMale.TabIndex := 1;
  Self.sbMale.Text := 'Мужской';
  // 
  // sbFemale
  // 
  Self.sbFemale.Location := System.Drawing.Point.Create(231, 16);
  Self.sbFemale.Name := 'sbFemale';
  Self.sbFemale.Size := System.Drawing.Size.Create(105, 22);
  Self.sbFemale.TabIndex := 2;
  Self.sbFemale.Text := 'Женский';
  // 
  // btnAccept
  // 
  Self.btnAccept.DialogResult := System.Windows.Forms.DialogResult.OK;
  Self.btnAccept.Image := (System.Drawing.Image(resources.GetObject('btnAcce' +
    'pt.Image')));
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(184, 96);
  Self.btnAccept.Name := 'btnAccept';
  Self.btnAccept.Size := System.Drawing.Size.Create(81, 25);
  Self.btnAccept.TabIndex := 2;
  Self.btnAccept.Text := 'Принять';
  Self.btnAccept.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // btnCancel
  // 
  Self.btnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnCancel.Image := (System.Drawing.Image(resources.GetObject('btnCanc' +
    'el.Image')));
  Self.btnCancel.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnCancel.Location := System.Drawing.Point.Create(272, 96);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 3;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // TfmSexCheck
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(361, 130);
  Self.Controls.Add(Self.edName);
  Self.Controls.Add(Self.GroupBox1);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedSingle;
  Self.Name := 'TfmSexCheck';
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Проверка пола';
  Self.GroupBox1.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmSexCheck.Create;
begin
  inherited Create;
  InitializeComponent;

  // SetLang
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  Text := LSList[LSID_WinCheckSex];

  GroupBox1.Text := LSList[LSID_Sex];
  sbMale.Text := LSList[LSID_SexM];
  sbFemale.Text := LSList[LSID_SexF];
end;

class function TfmSexCheck.DefineSex(iName, iPatr: string; aNamesTable: TNamesTable): TGEDCOMObject.TGEDCOMSex;
var
  dlg: TfmSexCheck;
  sx: TGEDCOMObject.TGEDCOMSex;
begin
  sx := aNamesTable.GetSexByName(iName);
  Result := sx;
  if (sx <> svNone) then Exit;

  dlg := TfmSexCheck.Create();
  try
    if (dlg <> nil) then begin
      dlg.edName.Text := iName + ' ' + iPatr;

      sx := TGenEngine.GetSex(iName, iPatr, False);

      case sx of
        svNone, svUndetermined: dlg.sbNone.Checked := True;
        svMale: dlg.sbMale.Checked := True;
        svFemale: dlg.sbFemale.Checked := True;
      end;

      if (dlg.ShowDialog() = System.Windows.Forms.DialogResult.OK) then begin
        if (dlg.sbNone.Checked) then sx := svNone
        else
        if (dlg.sbMale.Checked) then sx := svMale
        else
        if (dlg.sbFemale.Checked) then sx := svFemale;

        Result := sx;

        if (sx <> svNone)
        then aNamesTable.SetNameSex(iName, sx);
      end;
    end;
  finally
    dlg.Free;
  end;
end;

class procedure TfmSexCheck.CheckPersonSex(iRec: TGEDCOMIndividualRecord; aNamesTable: TNamesTable);
var
  f_name, f_patr, f_fam: string;
begin
  if (iRec.Sex in [svNone, svUndetermined]) then begin
    TGenEngine.GetNameParts(iRec, f_fam, f_name, f_patr);
    iRec.Sex := DefineSex(f_name, f_patr, aNamesTable);
  end;
end;

end.
