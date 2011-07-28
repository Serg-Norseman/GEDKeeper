unit GKPersonScan; {trans:none}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms,
  VCLStub, GedCom551, GKBase, GKUtils, GKEngine, GKCtrls, GKSexCheck, GKLangs;

type
  TfmPersonScan = class(System.Windows.Forms.Form)
  strict private
  type
    TPersonLink = (
      plNone, plPerson, plFather, plMother, plGodparent, plSpouse, plChild);

  const
    PersonLinks: array [TPersonLink] of LSID = (
      LSID_RK_Unk, LSID_PLPerson, LSID_Father, LSID_Mother,
      LSID_PLGodparent, LSID_Spouse, LSID_Child
    );

  var
    btnParse: System.Windows.Forms.Button;
    btnClose: System.Windows.Forms.Button;
    PageControl1: System.Windows.Forms.TabControl;
    tsSimpleInput: System.Windows.Forms.TabPage;
    tsSourceInput: System.Windows.Forms.TabPage;
    Label1: System.Windows.Forms.Label;
    Label2: System.Windows.Forms.Label;
    btnMale: System.Windows.Forms.Button;
    btnFemale: System.Windows.Forms.Button;
    EditName: System.Windows.Forms.TextBox;
    MemoNote: System.Windows.Forms.TextBox;
    Panel1: System.Windows.Forms.Panel;
    Label3: System.Windows.Forms.Label;
    Label5: System.Windows.Forms.Label;
    EditBirthDate: System.Windows.Forms.TextBox;
    EditBirthPlace: System.Windows.Forms.TextBox;
    CheckBirth: System.Windows.Forms.CheckBox;
    Panel2: System.Windows.Forms.Panel;
    Label6: System.Windows.Forms.Label;
    Label7: System.Windows.Forms.Label;
    CheckDeath: System.Windows.Forms.CheckBox;
    EditDeathDate: System.Windows.Forms.TextBox;
    EditDeathPlace: System.Windows.Forms.TextBox;
    Label4: System.Windows.Forms.Label;
    Label8: System.Windows.Forms.Label;
    Label9: System.Windows.Forms.Label;
    Label10: System.Windows.Forms.Label;
    cbSource: System.Windows.Forms.ComboBox;
    edPage: System.Windows.Forms.TextBox;
    edSourceYear: System.Windows.Forms.TextBox;
    edPlace: System.Windows.Forms.TextBox;
    cbPersonLink: System.Windows.Forms.ComboBox;
    rgSourceKind: System.Windows.Forms.GroupBox;
    gbMetrics: System.Windows.Forms.GroupBox;
    Label11: System.Windows.Forms.Label;
    Label12: System.Windows.Forms.Label;
    edEventDate: System.Windows.Forms.TextBox;
    cbEventType: System.Windows.Forms.ComboBox;
    sgData: System.Windows.Forms.Panel;

    FBase: TfmBase;
    FSourcesList: TStringList;
    //sgData: System.Windows.Forms.StringGrid;

    class function GetLinkByName(const aName: string): TPersonLink; static;

    function CheckCell(ACol, ARow: Integer): Boolean;
    procedure InitGrid();
    procedure InitSimpleControls();
    procedure InitSourceControls();
    procedure ParseSimple();
    procedure ParseSource();

    procedure InitializeComponent;

    procedure EditNameKeyPress(sender: System.Object; e: System.Windows.Forms.KeyPressEventArgs);
    procedure sgDataSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    //procedure cbPersonLinkKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    //procedure sgDataKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure btnParseClick(sender: System.Object; e: System.EventArgs);
    procedure EditBirthDateChange(sender: System.Object; e: System.EventArgs);
    procedure EditDeathDateChange(sender: System.Object; e: System.EventArgs);
    procedure cbPersonLinkChange(sender: System.Object; e: System.EventArgs);
    procedure rgSourceKindClick(sender: System.Object; e: System.EventArgs);
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;

    procedure SetLang();
  end;

implementation

{ TfmPersonScan }

procedure TfmPersonScan.InitializeComponent;
begin
  Self.btnParse := System.Windows.Forms.Button.Create;
  Self.btnClose := System.Windows.Forms.Button.Create;
  Self.PageControl1 := System.Windows.Forms.TabControl.Create;
  Self.tsSimpleInput := System.Windows.Forms.TabPage.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.btnMale := System.Windows.Forms.Button.Create;
  Self.btnFemale := System.Windows.Forms.Button.Create;
  Self.EditName := System.Windows.Forms.TextBox.Create;
  Self.MemoNote := System.Windows.Forms.TextBox.Create;
  Self.Panel1 := System.Windows.Forms.Panel.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.Label5 := System.Windows.Forms.Label.Create;
  Self.EditBirthDate := System.Windows.Forms.TextBox.Create;
  Self.EditBirthPlace := System.Windows.Forms.TextBox.Create;
  Self.CheckBirth := System.Windows.Forms.CheckBox.Create;
  Self.Panel2 := System.Windows.Forms.Panel.Create;
  Self.Label6 := System.Windows.Forms.Label.Create;
  Self.Label7 := System.Windows.Forms.Label.Create;
  Self.CheckDeath := System.Windows.Forms.CheckBox.Create;
  Self.EditDeathDate := System.Windows.Forms.TextBox.Create;
  Self.EditDeathPlace := System.Windows.Forms.TextBox.Create;
  Self.tsSourceInput := System.Windows.Forms.TabPage.Create;
  Self.Label4 := System.Windows.Forms.Label.Create;
  Self.Label8 := System.Windows.Forms.Label.Create;
  Self.Label9 := System.Windows.Forms.Label.Create;
  Self.Label10 := System.Windows.Forms.Label.Create;
  Self.cbSource := System.Windows.Forms.ComboBox.Create;
  Self.edPage := System.Windows.Forms.TextBox.Create;
  Self.edSourceYear := System.Windows.Forms.TextBox.Create;
  Self.edPlace := System.Windows.Forms.TextBox.Create;
  Self.sgData := System.Windows.Forms.Panel.Create;
  Self.cbPersonLink := System.Windows.Forms.ComboBox.Create;
  Self.rgSourceKind := System.Windows.Forms.GroupBox.Create;
  Self.gbMetrics := System.Windows.Forms.GroupBox.Create;
  Self.Label11 := System.Windows.Forms.Label.Create;
  Self.Label12 := System.Windows.Forms.Label.Create;
  Self.edEventDate := System.Windows.Forms.TextBox.Create;
  Self.cbEventType := System.Windows.Forms.ComboBox.Create;
  Self.PageControl1.SuspendLayout;
  Self.tsSimpleInput.SuspendLayout;
  Self.Panel1.SuspendLayout;
  Self.Panel2.SuspendLayout;
  Self.tsSourceInput.SuspendLayout;
  Self.gbMetrics.SuspendLayout;
  Self.SuspendLayout;
  // 
  // btnParse
  // 
  Self.btnParse.Location := System.Drawing.Point.Create(464, 424);
  Self.btnParse.Name := 'btnParse';
  Self.btnParse.Size := System.Drawing.Size.Create(81, 25);
  Self.btnParse.TabIndex := 0;
  Self.btnParse.Text := 'Добавить';
  Include(Self.btnParse.Click, Self.btnParseClick);
  // 
  // btnClose
  // 
  Self.btnClose.Location := System.Drawing.Point.Create(560, 424);
  Self.btnClose.Name := 'btnClose';
  Self.btnClose.Size := System.Drawing.Size.Create(81, 25);
  Self.btnClose.TabIndex := 1;
  Self.btnClose.Text := 'Закрыть';
  // 
  // PageControl1
  // 
  Self.PageControl1.Controls.Add(Self.tsSimpleInput);
  Self.PageControl1.Controls.Add(Self.tsSourceInput);
  Self.PageControl1.Location := System.Drawing.Point.Create(8, 8);
  Self.PageControl1.Name := 'PageControl1';
  Self.PageControl1.SelectedIndex := 0;
  Self.PageControl1.Size := System.Drawing.Size.Create(633, 401);
  Self.PageControl1.TabIndex := 2;
  // 
  // tsSimpleInput
  // 
  Self.tsSimpleInput.Controls.Add(Self.Label1);
  Self.tsSimpleInput.Controls.Add(Self.Label2);
  Self.tsSimpleInput.Controls.Add(Self.btnMale);
  Self.tsSimpleInput.Controls.Add(Self.btnFemale);
  Self.tsSimpleInput.Controls.Add(Self.EditName);
  Self.tsSimpleInput.Controls.Add(Self.MemoNote);
  Self.tsSimpleInput.Controls.Add(Self.Panel1);
  Self.tsSimpleInput.Controls.Add(Self.Panel2);
  Self.tsSimpleInput.Location := System.Drawing.Point.Create(4, 22);
  Self.tsSimpleInput.Name := 'tsSimpleInput';
  Self.tsSimpleInput.Size := System.Drawing.Size.Create(625, 375);
  Self.tsSimpleInput.TabIndex := 0;
  Self.tsSimpleInput.Text := 'Простой ввод';
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 8);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(150, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Полное имя (формат ФИО)';
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(8, 232);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(50, 13);
  Self.Label2.TabIndex := 1;
  Self.Label2.Text := 'Заметка';
  // 
  // btnMale
  // 
  Self.btnMale.Location := System.Drawing.Point.Create(424, 24);
  Self.btnMale.Name := 'btnMale';
  Self.btnMale.Size := System.Drawing.Size.Create(23, 21);
  Self.btnMale.TabIndex := 2;
  Self.btnMale.Text := 'М';
  // 
  // btnFemale
  // 
  Self.btnFemale.Location := System.Drawing.Point.Create(448, 24);
  Self.btnFemale.Name := 'btnFemale';
  Self.btnFemale.Size := System.Drawing.Size.Create(23, 21);
  Self.btnFemale.TabIndex := 3;
  Self.btnFemale.Text := 'Ж';
  // 
  // EditName
  // 
  Self.EditName.Location := System.Drawing.Point.Create(8, 24);
  Self.EditName.Name := 'EditName';
  Self.EditName.Size := System.Drawing.Size.Create(409, 21);
  Self.EditName.TabIndex := 0;
  Self.EditName.Text := '';
  // 
  // MemoNote
  // 
  Self.MemoNote.Location := System.Drawing.Point.Create(8, 245);
  Self.MemoNote.Multiline := True;
  Self.MemoNote.Name := 'MemoNote';
  Self.MemoNote.Size := System.Drawing.Size.Create(465, 121);
  Self.MemoNote.TabIndex := 1;
  Self.MemoNote.Text := '';
  // 
  // Panel1
  // 
  Self.Panel1.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.Panel1.Controls.Add(Self.Label3);
  Self.Panel1.Controls.Add(Self.Label5);
  Self.Panel1.Controls.Add(Self.EditBirthDate);
  Self.Panel1.Controls.Add(Self.EditBirthPlace);
  Self.Panel1.Controls.Add(Self.CheckBirth);
  Self.Panel1.Location := System.Drawing.Point.Create(8, 56);
  Self.Panel1.Name := 'Panel1';
  Self.Panel1.Size := System.Drawing.Size.Create(465, 81);
  Self.Panel1.TabIndex := 2;
  // 
  // Label3
  // 
  Self.Label3.Location := System.Drawing.Point.Create(8, 32);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(90, 13);
  Self.Label3.TabIndex := 0;
  Self.Label3.Text := 'Дата рождения';
  // 
  // Label5
  // 
  Self.Label5.Location := System.Drawing.Point.Create(112, 32);
  Self.Label5.Name := 'Label5';
  Self.Label5.Size := System.Drawing.Size.Create(100, 13);
  Self.Label5.TabIndex := 1;
  Self.Label5.Text := 'Место рождения';
  //
  // EditBirthDate
  // 
  Self.EditBirthDate.Location := System.Drawing.Point.Create(8, 48);
  Self.EditBirthDate.MaxLength := 10;
  Self.EditBirthDate.Name := 'EditBirthDate';
  Self.EditBirthDate.Size := System.Drawing.Size.Create(97, 21);
  Self.EditBirthDate.TabIndex := 0;
  Self.EditBirthDate.Text := '  .  .    ';
  // 
  // EditBirthPlace
  // 
  Self.EditBirthPlace.Location := System.Drawing.Point.Create(112, 48);
  Self.EditBirthPlace.Name := 'EditBirthPlace';
  Self.EditBirthPlace.Size := System.Drawing.Size.Create(337, 21);
  Self.EditBirthPlace.TabIndex := 1;
  Self.EditBirthPlace.Text := '';
  // 
  // CheckBirth
  // 
  Self.CheckBirth.Location := System.Drawing.Point.Create(8, 8);
  Self.CheckBirth.Name := 'CheckBirth';
  Self.CheckBirth.Size := System.Drawing.Size.Create(96, 17);
  Self.CheckBirth.TabIndex := 2;
  Self.CheckBirth.Text := 'Родился';
  // 
  // Panel2
  // 
  Self.Panel2.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.Panel2.Controls.Add(Self.Label6);
  Self.Panel2.Controls.Add(Self.Label7);
  Self.Panel2.Controls.Add(Self.CheckDeath);
  Self.Panel2.Controls.Add(Self.EditDeathDate);
  Self.Panel2.Controls.Add(Self.EditDeathPlace);
  Self.Panel2.Location := System.Drawing.Point.Create(8, 144);
  Self.Panel2.Name := 'Panel2';
  Self.Panel2.Size := System.Drawing.Size.Create(465, 81);
  Self.Panel2.TabIndex := 3;
  // 
  // Label6
  // 
  Self.Label6.Location := System.Drawing.Point.Create(8, 32);
  Self.Label6.Name := 'Label6';
  Self.Label6.Size := System.Drawing.Size.Create(90, 13);
  Self.Label6.TabIndex := 0;
  Self.Label6.Text := 'Дата смерти';
  // 
  // Label7
  //
  Self.Label7.Location := System.Drawing.Point.Create(112, 32);
  Self.Label7.Name := 'Label7';
  Self.Label7.Size := System.Drawing.Size.Create(100, 13);
  Self.Label7.TabIndex := 1;
  Self.Label7.Text := 'Место смерти';
  // 
  // CheckDeath
  // 
  Self.CheckDeath.Location := System.Drawing.Point.Create(8, 8);
  Self.CheckDeath.Name := 'CheckDeath';
  Self.CheckDeath.Size := System.Drawing.Size.Create(95, 17);
  Self.CheckDeath.TabIndex := 0;
  Self.CheckDeath.Text := 'Умер';
  // 
  // EditDeathDate
  // 
  Self.EditDeathDate.Location := System.Drawing.Point.Create(8, 48);
  Self.EditDeathDate.MaxLength := 10;
  Self.EditDeathDate.Name := 'EditDeathDate';
  Self.EditDeathDate.Size := System.Drawing.Size.Create(97, 21);
  Self.EditDeathDate.TabIndex := 1;
  Self.EditDeathDate.Text := '  .  .    ';
  // 
  // EditDeathPlace
  // 
  Self.EditDeathPlace.Location := System.Drawing.Point.Create(112, 48);
  Self.EditDeathPlace.Name := 'EditDeathPlace';
  Self.EditDeathPlace.Size := System.Drawing.Size.Create(337, 21);
  Self.EditDeathPlace.TabIndex := 2;
  Self.EditDeathPlace.Text := '';
  // 
  // tsSourceInput
  // 
  Self.tsSourceInput.Controls.Add(Self.Label4);
  Self.tsSourceInput.Controls.Add(Self.Label8);
  Self.tsSourceInput.Controls.Add(Self.Label9);
  Self.tsSourceInput.Controls.Add(Self.Label10);
  Self.tsSourceInput.Controls.Add(Self.cbSource);
  Self.tsSourceInput.Controls.Add(Self.edPage);
  Self.tsSourceInput.Controls.Add(Self.edSourceYear);
  Self.tsSourceInput.Controls.Add(Self.edPlace);
  Self.tsSourceInput.Controls.Add(Self.sgData);
  Self.tsSourceInput.Controls.Add(Self.cbPersonLink);
  Self.tsSourceInput.Controls.Add(Self.rgSourceKind);
  Self.tsSourceInput.Controls.Add(Self.gbMetrics);
  Self.tsSourceInput.Location := System.Drawing.Point.Create(4, 22);
  Self.tsSourceInput.Name := 'tsSourceInput';
  Self.tsSourceInput.Size := System.Drawing.Size.Create(625, 375);
  Self.tsSourceInput.TabIndex := 1;
  Self.tsSourceInput.Text := 'Источник (метрики/ревизии)';
  // 
  // Label4
  // 
  Self.Label4.Location := System.Drawing.Point.Create(8, 56);
  Self.Label4.Name := 'Label4';
  Self.Label4.Size := System.Drawing.Size.Create(55, 13);
  Self.Label4.TabIndex := 0;
  Self.Label4.Text := 'Источник';
  // 
  // Label8
  // 
  Self.Label8.Location := System.Drawing.Point.Create(304, 56);
  Self.Label8.Name := 'Label8';
  Self.Label8.Size := System.Drawing.Size.Create(85, 13);
  Self.Label8.TabIndex := 1;
  Self.Label8.Text := 'Лист/страница';
  // 
  // Label9
  // 
  Self.Label9.Location := System.Drawing.Point.Create(520, 56);
  Self.Label9.Name := 'Label9';
  Self.Label9.Size := System.Drawing.Size.Create(25, 13);
  Self.Label9.TabIndex := 2;
  Self.Label9.Text := 'Год';
  // 
  // Label10
  // 
  Self.Label10.Location := System.Drawing.Point.Create(8, 88);
  Self.Label10.Name := 'Label10';
  Self.Label10.Size := System.Drawing.Size.Create(105, 13);
  Self.Label10.TabIndex := 3;
  Self.Label10.Text := 'Населенный пункт';
  // 
  // cbSource
  // 
  Self.cbSource.Location := System.Drawing.Point.Create(64, 48);
  Self.cbSource.Name := 'cbSource';
  Self.cbSource.Size := System.Drawing.Size.Create(225, 21);
  Self.cbSource.TabIndex := 0;
  // 
  // edPage
  // 
  Self.edPage.Location := System.Drawing.Point.Create(392, 48);
  Self.edPage.Name := 'edPage';
  Self.edPage.Size := System.Drawing.Size.Create(112, 21);
  Self.edPage.TabIndex := 1;
  Self.edPage.Text := '';
  // 
  // edSourceYear
  // 
  Self.edSourceYear.Location := System.Drawing.Point.Create(560, 48);
  Self.edSourceYear.MaxLength := 4;
  Self.edSourceYear.Name := 'edSourceYear';
  Self.edSourceYear.Size := System.Drawing.Size.Create(57, 21);
  Self.edSourceYear.TabIndex := 2;
  Self.edSourceYear.Text := '    ';
  // 
  // edPlace
  // 
  Self.edPlace.Location := System.Drawing.Point.Create(120, 80);
  Self.edPlace.Name := 'edPlace';
  Self.edPlace.Size := System.Drawing.Size.Create(497, 21);
  Self.edPlace.TabIndex := 3;
  Self.edPlace.Text := '';
  // 
  // sgData
  // 
  Self.sgData.Location := System.Drawing.Point.Create(8, 184);
  Self.sgData.Name := 'sgData';
  Self.sgData.Size := System.Drawing.Size.Create(609, 177);
  Self.sgData.TabIndex := 4;
  // 
  // cbPersonLink
  // 
  Self.cbPersonLink.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.cbPersonLink.Location := System.Drawing.Point.Create(336, 296);
  Self.cbPersonLink.Name := 'cbPersonLink';
  Self.cbPersonLink.Size := System.Drawing.Size.Create(145, 21);
  Self.cbPersonLink.TabIndex := 5;
  Self.cbPersonLink.Visible := False;
  // 
  // rgSourceKind
  // 
  Self.rgSourceKind.Location := System.Drawing.Point.Create(8, 0);
  Self.rgSourceKind.Name := 'rgSourceKind';
  Self.rgSourceKind.Size := System.Drawing.Size.Create(609, 38);
  Self.rgSourceKind.TabIndex := 6;
  Self.rgSourceKind.TabStop := False;
  Self.rgSourceKind.Text := 'Тип источника';
  // 
  // gbMetrics
  // 
  Self.gbMetrics.Controls.Add(Self.Label11);
  Self.gbMetrics.Controls.Add(Self.Label12);
  Self.gbMetrics.Controls.Add(Self.edEventDate);
  Self.gbMetrics.Controls.Add(Self.cbEventType);
  Self.gbMetrics.Enabled := False;
  Self.gbMetrics.Location := System.Drawing.Point.Create(8, 120);
  Self.gbMetrics.Name := 'gbMetrics';
  Self.gbMetrics.Size := System.Drawing.Size.Create(609, 50);
  Self.gbMetrics.TabIndex := 7;
  Self.gbMetrics.TabStop := False;
  Self.gbMetrics.Text := 'Метрическая книга';
  // 
  // Label11
  // 
  Self.Label11.Location := System.Drawing.Point.Create(8, 24);
  Self.Label11.Name := 'Label11';
  Self.Label11.Size := System.Drawing.Size.Create(80, 13);
  Self.Label11.TabIndex := 0;
  Self.Label11.Text := 'Дата события';
  // 
  // Label12
  // 
  Self.Label12.Location := System.Drawing.Point.Create(248, 24);
  Self.Label12.Name := 'Label12';
  Self.Label12.Size := System.Drawing.Size.Create(70, 13);
  Self.Label12.TabIndex := 1;
  Self.Label12.Text := 'Тип события';
  // 
  // edEventDate
  // 
  Self.edEventDate.Location := System.Drawing.Point.Create(96, 16);
  Self.edEventDate.MaxLength := 10;
  Self.edEventDate.Name := 'edEventDate';
  Self.edEventDate.Size := System.Drawing.Size.Create(129, 21);
  Self.edEventDate.TabIndex := 0;
  Self.edEventDate.Text := '  .  .    ';
  // 
  // cbEventType
  // 
  Self.cbEventType.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.cbEventType.Location := System.Drawing.Point.Create(328, 16);
  Self.cbEventType.Name := 'cbEventType';
  Self.cbEventType.Size := System.Drawing.Size.Create(145, 21);
  Self.cbEventType.TabIndex := 1;
  // 
  // TfmPersonScan
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.ClientSize := System.Drawing.Size.Create(649, 457);
  Self.Controls.Add(Self.btnParse);
  Self.Controls.Add(Self.btnClose);
  Self.Controls.Add(Self.PageControl1);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmPersonScan';
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Добавление персон из источника';
  Self.PageControl1.ResumeLayout(False);
  Self.tsSimpleInput.ResumeLayout(False);
  Self.Panel1.ResumeLayout(False);
  Self.Panel2.ResumeLayout(False);
  Self.tsSourceInput.ResumeLayout(False);
  Self.gbMetrics.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmPersonScan.Create;
var
  pl: TPersonLink;
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  //Self.cbPersonLink.OnChange := cbPersonLinkChange;
  //Self.cbPersonLink.OnKeyDown := cbPersonLinkKeyDown;

  //Self.rgSourceKind.Columns := 2;
  //Self.rgSourceKind.ItemIndex := 0;
  //Self.rgSourceKind.Items.Strings := (Ревизская сказка, Метрическая книга);
  //Self.rgSourceKind.OnClick := rgSourceKindClick;

  //Self.edEventDate.EditMask := '!99/99/9999;1;_';
  //Self.cbEventType.Items.Strings := (Рождение, Смерть, Брак);

  //Self.EditName.OnKeyPress := Self.EditNameKeyPress;

  //Self.EditBirthDate.EditMask := '!99/99/9999;1;_';
  //Self.EditBirthDate.OnChange := EditBirthDateChange;
  //Self.EditBirthPlace.OnChange := EditBirthDateChange;
  //Self.EditDeathDate.EditMask := '!99/99/9999;1;_';
  //Self.EditDeathDate.OnChange := EditDeathDateChange;
  //Self.EditDeathPlace.OnChange := EditDeathDateChange;

  //Self.edSourceYear.EditMask := '!0000;1;_';

  //Self.sgData.ColCount := 6;
  //Self.sgData.DefaultColWidth := 80;
  //Self.sgData.DefaultRowHeight := 20;
  //Self.sgData.FixedCols := 0;
  //Self.sgData.RowCount := 100;
  //Self.sgData.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
  //Self.sgData.OnKeyDown := sgDataKeyDown;
  //Self.sgData.OnSelectCell := sgDataSelectCell;

  FSourcesList := TStringList.Create();

  // simpleparse init
  InitSimpleControls();

  // sourceparse init
  InitGrid();
  InitSourceControls();

  for pl := plPerson to High(TPersonLink) do cbPersonLink.Items.Add(LSList[PersonLinks[pl]]);

  SetLang();
end;

procedure TfmPersonScan.Dispose(Disposing: Boolean);
begin
  if Disposing then begin
    FSourcesList.Free;
  end;
  inherited Dispose(Disposing);
end;

procedure TfmPersonScan.SetLang();
begin
  btnParse.Text := LSList[LSID_DlgAppend];
  btnClose.Text := LSList[LSID_DlgClose];

  Text := LSList[LSID_MIStreamInput];

  tsSimpleInput.Text := LSList[LSID_InputSimple];

  btnMale.Text := LSList[LSID_SexM][1];
  btnFemale.Text := LSList[LSID_SexF][1];
  Label1.Text := LSList[LSID_FullName];
  CheckBirth.Text := LSList[LSID_Birth];
  Label3.Text := LSList[LSID_BirthDate];
  Label5.Text := LSList[LSID_BirthPlace];
  CheckDeath.Text := LSList[LSID_Death];
  Label6.Text := LSList[LSID_DeathDate];
  Label7.Text := LSList[LSID_DeathPlace];
  Label2.Text := LSList[LSID_Note];

  tsSourceInput.Text := LSList[LSID_InputSource];

  rgSourceKind.Text := LSList[LSID_SourceKind];
  //rgSourceKind.Items[0] := LSList[LSID_SK_Rev];
  //rgSourceKind.Items[0] := LSList[LSID_SK_Met];
  Label4.Text := LSList[LSID_Source];
  Label8.Text := LSList[LSID_Page];
  Label9.Text := LSList[LSID_Year];
  Label10.Text := LSList[LSID_Settlement];
  gbMetrics.Text := LSList[LSID_SK_Met];
  Label11.Text := LSList[LSID_EventDate];
  Label12.Text := LSList[LSID_EventType];
end;

procedure TfmPersonScan.InitSimpleControls();
begin
  EditName.Text := '';
  EditBirthDate.Text := '';
  EditBirthPlace.Text := '';
  CheckBirth.Checked := False;
  EditDeathDate.Text := '';
  EditDeathPlace.Text := '';
  CheckDeath.Checked := False;
  MemoNote.Text := '';
  //btnMale.Down := True;//alert
end;

procedure TfmPersonScan.InitSourceControls();
var
  col, row: Integer;
begin
  Base.Engine.GetSourcesList(FSourcesList);
  {cbSource.Items.Assign(FSourcesList);
  cbSource.Text := '';
  cbSource.ItemIndex := -1;alert}

  edPage.Text := '';
  edSourceYear.Text := '';
  edPlace.Text := '';

  edEventDate.Text := '';
  cbEventType.SelectedIndex := -1;

  {for row := 1 to sgData.RowCount - 1 do
    for col := 0 to sgData.ColCount - 1 do
      sgData.Cells[col, row] := '';Alert}
end;

procedure TfmPersonScan.InitGrid();
begin
  // Alert
  {sgData.Cells[0, 0] := LSList[LSID_Join];
  sgData.Cells[1, 0] := LSList[LSID_Name];
  sgData.Cells[2, 0] := LSList[LSID_Patronymic];
  sgData.Cells[3, 0] := LSList[LSID_Surname];
  sgData.Cells[4, 0] := LSList[LSID_Age];
  sgData.Cells[5, 0] := LSList[LSID_Comment];

  sgData.DefaultRowHeight := sgData.Canvas.TextHeight('A') + 7;
  sgData.ColWidths[4] := 60;
  sgData.ColWidths[5] := 150;}
end;

procedure TfmPersonScan.ParseSimple();
var
  iRec: TGEDCOMIndividualRecord;
  tokCount: Integer;
  nam, pat, fam, tmp: string;
  sx: TGEDCOMObject.TGEDCOMSex;
begin
  tmp := EditName.Text.ToLower();
  tokCount := TGKUtils.GetTokensCount(tmp, ' ');
  if (tokCount < 3) then begin
    TGKUtils.ShowError(LSList[LSID_NameInvalid]);
    Exit;
  end;

  fam := TGKUtils.GetToken(tmp, ' ', 1);
  nam := TGKUtils.GetToken(tmp, ' ', 2);
  pat := TGKUtils.GetToken(tmp, ' ', 3);

  fam[1] := System.Char.ToUpper(fam[1]);
  nam[1] := System.Char.ToUpper(nam[1]);
  pat[1] := System.Char.ToUpper(pat[1]);

  sx := svNone;

  //alert
  {if btnMale.Down then sx := svMale
  else
  if btnFemale.Down then sx := svFemale;}

  iRec := TGenEngine.CreatePersonEx(Base.Tree, nam, pat, fam, sx, False);
  Base.ChangeRecord(iRec);

  if (CheckBirth.Checked)
  then TGenEngine.CreateEventEx(Base.Tree, iRec, 'BIRT', TGenEngine.StrToGEDCOMDate(EditBirthDate.Text), EditBirthPlace.Text);

  if (CheckDeath.Checked)
  then TGenEngine.CreateEventEx(Base.Tree, iRec, 'DEAT', TGenEngine.StrToGEDCOMDate(EditDeathDate.Text), EditDeathPlace.Text);

  //Alert
  //if (MemoNote.Text <> '')
  //then CreateNoteEx(Base.Tree, MemoNote.Lines, iRec);

  InitSimpleControls();
end;

procedure TfmPersonScan.ParseSource();

  function GetParentsFamily(iRec: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;
  begin
    if (iRec.ChildToFamilyLinksCount > 0)
    then Result := iRec.ChildToFamilyLinks[0].Family
    else begin
      Result := TGenEngine.CreateFamilyEx(Base.Tree);
      Base.Engine.AddFamilyChild(Result, iRec);
    end;
  end;

  function GetMarriageFamily(iRec: TGEDCOMIndividualRecord): TGEDCOMFamilyRecord;
  begin
    if (iRec.SpouseToFamilyLinksCount > 0)
    then Result := iRec.SpouseToFamilyLinks[0].Family
    else begin
      Result := TGenEngine.CreateFamilyEx(Base.Tree);
      Base.Engine.AddFamilySpouse(Result, iRec);
    end;
  end;

  function CheckMain(aMain: TGEDCOMIndividualRecord): Boolean;
  begin
    Result := (aMain <> nil);
    if not(Result) then raise Exception.Create(LSList[LSID_BasePersonInvalid]);
  end;

var
  row, birth_year, src_year: Integer;
  lnk, nm, pt, fm, age, comment, place, src_name, src_page, ev_name: string;
  link: TPersonLink;
  iRec, iMain: TGEDCOMIndividualRecord;
  sx: TGEDCOMObject.TGEDCOMSex;
  note: TGEDCOMNoteRecord;
  family: TGEDCOMFamilyRecord;
  evt: TGEDCOMCustomEvent;
  src_rec: TGEDCOMSourceRecord;
begin
  src_name := cbSource.Text;
  src_page := edPage.Text;
  if not(TGKUtils.IsDigits(edSourceYear.Text)) then begin
    TGKUtils.ShowError(LSList[LSID_SourceYearInvalid]);
    Exit;
  end else src_year := Int32.Parse(edSourceYear.Text);
  place := edPlace.Text;
  iMain := nil;

  try
    //alert
    (*for row := 1 to sgData.RowCount - 1 do begin
      lnk := sgData.Cells[0, row];
      nm := sgData.Cells[1, row];
      pt := sgData.Cells[2, row];
      fm := sgData.Cells[3, row];
      age := sgData.Cells[4, row];
      comment := sgData.Cells[5, row];

      if (lnk <> '') then begin
        link := GetLinkByName(lnk);

        sx := TfmSexCheck.DefineSex(nm, pt, fmGEDKeeper.NamesTable);
        iRec := CreatePersonEx(Base.Tree, nm, pt, fm, sx, False);
        Base.ChangeRecord(iRec);

        if (age <> '') and (IsDigits(age)) then begin
          birth_year := src_year - StrToInt(age);
          CreateEventEx(Base.Tree, iRec, 'BIRT', 'ABT '+IntToStr(birth_year), '');
        end;

        if (place <> '') then begin
          evt := CreateEventEx(Base.Tree, iRec, 'RESI', '', '');
          evt.Detail.Place.StringValue := place;
        end;

        if (comment <> '') then begin
          note := CreateNoteEx(Base.Tree, nil, iRec);
          AddNoteText(note, comment);
        end;

        if (src_name <> '') then begin
          src_rec := Base.Engine.FindSource(src_name);
          if (src_rec = nil) then begin
            src_rec := CreateSource(Base.Tree);
            src_rec.FiledByEntry := src_name;
          end;
          BindRecordSource(Base.Tree, iRec, src_rec, src_page, 0);
        end;

        case link of
          plNone: ;

          plPerson: begin
            iMain := iRec;

            if (rgSourceKind.ItemIndex = 1) then begin // метрика
              { Рождение, Смерть, Брак }
              case cbEventType.ItemIndex of
                -1: ev_name := '';
                 0: ev_name := 'BIRT';
                 1: ev_name := 'DEAT';
                 2: ev_name := 'MARR';
              end;
            end;

            if (ev_name = 'BIRT') or (ev_name = 'DEAT') then begin
              evt := CreateEventEx(Base.Tree, iRec, ev_name, StrToGEDCOMDate(edEventDate.Text), '');
              evt.Detail.Place.StringValue := place;
            end
            else
            if (ev_name = 'MARR') then begin
              family := GetMarriageFamily(iRec);
              evt := CreateEventEx(Base.Tree, family, ev_name, StrToGEDCOMDate(edEventDate.Text), '');
              evt.Detail.Place.StringValue := place;
            end;
          end;

          plFather, plMother: begin
            CheckMain(iMain);
            family := GetParentsFamily(iMain);
            Base.Engine.AddFamilySpouse(family, iRec);
          end;

          plGodparent: begin
            CheckMain(iMain);
            Base.Engine.AddAssociation(iMain, LSList[LSID_PLGodparent], iRec);
          end;

          plSpouse: begin
            CheckMain(iMain);
            family := GetMarriageFamily(iMain);
            Base.Engine.AddFamilySpouse(family, iRec);
          end;

          plChild: begin
            CheckMain(iMain);
            family := GetMarriageFamily(iMain);
            Base.Engine.AddFamilyChild(family, iRec);
          end;
        end;
      end;
    end;*)
  finally

  end;

  InitSourceControls();
end;

procedure TfmPersonScan.rgSourceKindClick(sender: System.Object; e: System.EventArgs);
begin
  //alert
  //gbMetrics.Enabled := (rgSourceKind.ItemIndex = 1);
end;

procedure TfmPersonScan.btnParseClick(sender: System.Object; e: System.EventArgs);
begin
  case PageControl1.SelectedIndex of
    0: ParseSimple();
    1: ParseSource();
  end;

  Base.ListsRefresh();
end;

procedure TfmPersonScan.EditBirthDateChange(sender: System.Object; e: System.EventArgs);
begin
  CheckBirth.Checked := True;
end;

procedure TfmPersonScan.EditDeathDateChange(sender: System.Object; e: System.EventArgs);
begin
  CheckDeath.Checked := True;
end;

procedure TfmPersonScan.EditNameKeyPress(sender: System.Object; e: System.Windows.Forms.KeyPressEventArgs);
begin
  if (e.KeyChar = '/') then e.Handled := True;
end;

class function TfmPersonScan.GetLinkByName(const aName: string): TPersonLink;
var
  pl: TPersonLink;
begin
  for pl := plPerson to High(TPersonLink) do
    if (LSList[PersonLinks[pl]] = aName) then begin
      Result := pl;
      Exit;
    end;

  Result := plNone;
end;

procedure TfmPersonScan.sgDataSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  R: TRect;
  idx: Integer;
begin
  // alert
  {if (ACol = 0) then begin
    idx := cbPersonLink.Items.IndexOf(sgData.Cells[0, ARow]);
    if (idx < 0) then idx := 0;
    cbPersonLink.ItemIndex := idx;

    R := sgData.CellRect(ACol, ARow);
    R.Left  := R.Left + sgData.Left;
    R.Right := R.Right + sgData.Left;
    R.Top := R.Top + sgData.Top;
    R.Bottom := R.Bottom + sgData.Top;
    cbPersonLink.Top := R.Top + 2;
    cbPersonLink.Left := R.Left + 2;
    cbPersonLink.Width := (R.Right - R.Left);
    cbPersonLink.Height := (R.Bottom - R.Top);
    cbPersonLink.Visible := True;
    cbPersonLink.SetFocus;
  end else begin
    cbPersonLink.Visible := False;
  end;}

  CanSelect := True;
end;

procedure TfmPersonScan.cbPersonLinkChange(sender: System.Object; e: System.EventArgs);
begin
  //sgData.Cells[0, sgData.Row] := cbPersonLink.Text;alert
end;

function TfmPersonScan.CheckCell(ACol, ARow: Integer): Boolean;
var
  val: string;
begin
  (*Result := True;
  val := sgData.Cells[ACol, ARow];

  if (ACol = 0) then begin // связь
    Result := (GetLinkByName(val) <> plNone);
  end;
  if (ACol = 4) then begin // возраст
    Result := (val = '') or ((val <> '') and IsDigits(val));
  end;

  if not(Result)
  then MessageDlg(LSList[LSID_ValueInvalid], mtError, [mbOk], 0);*)
end;

(*procedure TfmPersonScan.cbPersonLinkKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  sgData.Cells[0, sgData.Row] := cbPersonLink.Text;

  if (Key = VK_RETURN) and CheckCell(sgData.Col, sgData.Row) then begin
    sgData.SetFocus;
    sgData.Col := sgData.Col + 1;
  end;
end;*)

(*procedure TfmPersonScan.sgDataKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and CheckCell(sgData.Col, sgData.Row) then begin
    if (sgData.Col = 5) then begin
      sgData.Col := 0;
      sgData.Row := sgData.Row + 1;
    end else sgData.Col := sgData.Col + 1;
  end;
end;*)

end.
