unit GKOptions; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms,
  VCLStub, GedCom551, GKCtrls, GKCommon, GKLists, GKEngine, GKMain,
  GKUtils, GKLangs;

type
  TfmOptions = class(System.Windows.Forms.Form, ILocalization)
  strict private
    PageControl1: System.Windows.Forms.TabControl;
    SheetCommon: System.Windows.Forms.TabPage;
    rgCode: System.Windows.Forms.GroupBox;
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    SheetTree: System.Windows.Forms.TabPage;
    GroupBox1: System.Windows.Forms.GroupBox;
    chkFamily: System.Windows.Forms.CheckBox;
    chkName: System.Windows.Forms.CheckBox;
    chkPatronymic: System.Windows.Forms.CheckBox;
    chkDiffLines: System.Windows.Forms.CheckBox;
    chkBirthDate: System.Windows.Forms.CheckBox;
    chkDeathDate: System.Windows.Forms.CheckBox;
    chkKinship: System.Windows.Forms.CheckBox;
    GroupBox2: System.Windows.Forms.GroupBox;
    PanMaleColor: System.Windows.Forms.Panel;
    PanFemaleColor: System.Windows.Forms.Panel;
    PanUnkSexColor: System.Windows.Forms.Panel;
    PanUnHusbandColor: System.Windows.Forms.Panel;
    PanUnWifeColor: System.Windows.Forms.Panel;
    ColorDialog1: System.Windows.Forms.ColorDialog;
    GroupBox4: System.Windows.Forms.GroupBox;
    Label1: System.Windows.Forms.Label;
    Label2: System.Windows.Forms.Label;
    Label3: System.Windows.Forms.Label;
    Label4: System.Windows.Forms.Label;
    chkProxy: System.Windows.Forms.CheckBox;
    edProxyServer: System.Windows.Forms.TextBox;
    edProxyPort: System.Windows.Forms.TextBox;
    edProxyLogin: System.Windows.Forms.TextBox;
    edProxyPass: System.Windows.Forms.TextBox;
    SheetView: System.Windows.Forms.TabPage;
    PageControl2: System.Windows.Forms.TabControl;
    SheetViewCommon: System.Windows.Forms.TabPage;
    SheetViewPersons: System.Windows.Forms.TabPage;
    ListPersonColumns: System.Windows.Forms.CheckedListBox;
    btnColumnUp: System.Windows.Forms.Button;
    btnColumnDown: System.Windows.Forms.Button;
    btnDefList: System.Windows.Forms.Button;
    rgFNPFormat: System.Windows.Forms.GroupBox;
    rgDateFormat: System.Windows.Forms.GroupBox;
    chkPlacesWithAddress: System.Windows.Forms.CheckBox;
    GroupBox7: System.Windows.Forms.GroupBox;
    chkShowOnStart: System.Windows.Forms.CheckBox;
    rgEditMode: System.Windows.Forms.GroupBox;
    chkHighlightUnparented: System.Windows.Forms.CheckBox;
    chkHighlightUnmarried: System.Windows.Forms.CheckBox;
    chkOnlyYears: System.Windows.Forms.CheckBox;
    chkSignsVisible: System.Windows.Forms.CheckBox;
    chkChildlessExclude: System.Windows.Forms.CheckBox;
    Label5: System.Windows.Forms.Label;
    PanDefFont: System.Windows.Forms.Panel;
    FontDialog1: System.Windows.Forms.FontDialog;
    SheetPedigree: System.Windows.Forms.TabPage;
    GroupBox5: System.Windows.Forms.GroupBox;
    chkAttributes: System.Windows.Forms.CheckBox;
    chkNotes: System.Windows.Forms.CheckBox;
    chkSources: System.Windows.Forms.CheckBox;
    EditPedigreeFormat: System.Windows.Forms.GroupBox;
    Label6: System.Windows.Forms.Label;
    cbLanguages: System.Windows.Forms.ComboBox;
    chkTreeDecorative: System.Windows.Forms.CheckBox;
    chkPortraitsVisible: System.Windows.Forms.CheckBox;

    FOptions: TGlobalOptions;
    FPersonColumns: TGlobalOptions.TPersonColumnsList;
    RButton1: System.Windows.Forms.RadioButton;
    RButton2: System.Windows.Forms.RadioButton;
    RButton3: System.Windows.Forms.RadioButton;
    RButton4: System.Windows.Forms.RadioButton;
    RButton5: System.Windows.Forms.RadioButton;
    RButton6: System.Windows.Forms.RadioButton;
    RButton7: System.Windows.Forms.RadioButton;
    RButton8: System.Windows.Forms.RadioButton;
    RButton9: System.Windows.Forms.RadioButton;
    RButton10: System.Windows.Forms.RadioButton;
    RButton11: System.Windows.Forms.RadioButton;

    procedure UpdateColumnsList();
    procedure UpdateControls();
    procedure UpdateForm();

    procedure InitializeComponent;
    procedure PanColor_Click(sender: System.Object; e: System.EventArgs);
    procedure PanDefFont_Click(sender: System.Object; e: System.EventArgs);
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
    procedure btnColumnUp_Click(sender: System.Object; e: System.EventArgs);
    procedure btnColumnDown_Click(sender: System.Object; e: System.EventArgs);
    procedure btnDefList_Click(sender: System.Object; e: System.EventArgs);
    procedure ListPersonColumns_ItemCheck(sender: System.Object; e: System.Windows.Forms.ItemCheckEventArgs);
  public
    constructor Create;

    property Options: TGlobalOptions read FOptions write FOptions;

    procedure SetLang();
  end;

implementation

procedure TfmOptions.InitializeComponent;
begin
  Self.PageControl1 := System.Windows.Forms.TabControl.Create;
  Self.SheetCommon := System.Windows.Forms.TabPage.Create;
  Self.Label6 := System.Windows.Forms.Label.Create;
  Self.rgCode := System.Windows.Forms.GroupBox.Create;
  Self.RButton2 := System.Windows.Forms.RadioButton.Create;
  Self.RButton1 := System.Windows.Forms.RadioButton.Create;
  Self.GroupBox4 := System.Windows.Forms.GroupBox.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.Label4 := System.Windows.Forms.Label.Create;
  Self.chkProxy := System.Windows.Forms.CheckBox.Create;
  Self.edProxyServer := System.Windows.Forms.TextBox.Create;
  Self.edProxyPort := System.Windows.Forms.TextBox.Create;
  Self.edProxyLogin := System.Windows.Forms.TextBox.Create;
  Self.edProxyPass := System.Windows.Forms.TextBox.Create;
  Self.GroupBox7 := System.Windows.Forms.GroupBox.Create;
  Self.chkShowOnStart := System.Windows.Forms.CheckBox.Create;
  Self.rgEditMode := System.Windows.Forms.GroupBox.Create;
  Self.RButton4 := System.Windows.Forms.RadioButton.Create;
  Self.RButton3 := System.Windows.Forms.RadioButton.Create;
  Self.cbLanguages := System.Windows.Forms.ComboBox.Create;
  Self.SheetTree := System.Windows.Forms.TabPage.Create;
  Self.GroupBox1 := System.Windows.Forms.GroupBox.Create;
  Self.chkFamily := System.Windows.Forms.CheckBox.Create;
  Self.chkName := System.Windows.Forms.CheckBox.Create;
  Self.chkPatronymic := System.Windows.Forms.CheckBox.Create;
  Self.chkDiffLines := System.Windows.Forms.CheckBox.Create;
  Self.chkBirthDate := System.Windows.Forms.CheckBox.Create;
  Self.chkDeathDate := System.Windows.Forms.CheckBox.Create;
  Self.chkKinship := System.Windows.Forms.CheckBox.Create;
  Self.chkOnlyYears := System.Windows.Forms.CheckBox.Create;
  Self.chkSignsVisible := System.Windows.Forms.CheckBox.Create;
  Self.chkChildlessExclude := System.Windows.Forms.CheckBox.Create;
  Self.chkTreeDecorative := System.Windows.Forms.CheckBox.Create;
  Self.chkPortraitsVisible := System.Windows.Forms.CheckBox.Create;
  Self.GroupBox2 := System.Windows.Forms.GroupBox.Create;
  Self.Label5 := System.Windows.Forms.Label.Create;
  Self.PanMaleColor := System.Windows.Forms.Panel.Create;
  Self.PanFemaleColor := System.Windows.Forms.Panel.Create;
  Self.PanUnkSexColor := System.Windows.Forms.Panel.Create;
  Self.PanUnHusbandColor := System.Windows.Forms.Panel.Create;
  Self.PanUnWifeColor := System.Windows.Forms.Panel.Create;
  Self.PanDefFont := System.Windows.Forms.Panel.Create;
  Self.SheetView := System.Windows.Forms.TabPage.Create;
  Self.PageControl2 := System.Windows.Forms.TabControl.Create;
  Self.SheetViewCommon := System.Windows.Forms.TabPage.Create;
  Self.rgFNPFormat := System.Windows.Forms.GroupBox.Create;
  Self.RButton7 := System.Windows.Forms.RadioButton.Create;
  Self.RButton6 := System.Windows.Forms.RadioButton.Create;
  Self.RButton5 := System.Windows.Forms.RadioButton.Create;
  Self.rgDateFormat := System.Windows.Forms.GroupBox.Create;
  Self.RButton9 := System.Windows.Forms.RadioButton.Create;
  Self.RButton8 := System.Windows.Forms.RadioButton.Create;
  Self.chkPlacesWithAddress := System.Windows.Forms.CheckBox.Create;
  Self.chkHighlightUnparented := System.Windows.Forms.CheckBox.Create;
  Self.chkHighlightUnmarried := System.Windows.Forms.CheckBox.Create;
  Self.SheetViewPersons := System.Windows.Forms.TabPage.Create;
  Self.btnColumnUp := System.Windows.Forms.Button.Create;
  Self.btnColumnDown := System.Windows.Forms.Button.Create;
  Self.ListPersonColumns := System.Windows.Forms.CheckedListBox.Create;
  Self.btnDefList := System.Windows.Forms.Button.Create;
  Self.SheetPedigree := System.Windows.Forms.TabPage.Create;
  Self.GroupBox5 := System.Windows.Forms.GroupBox.Create;
  Self.chkAttributes := System.Windows.Forms.CheckBox.Create;
  Self.chkNotes := System.Windows.Forms.CheckBox.Create;
  Self.chkSources := System.Windows.Forms.CheckBox.Create;
  Self.EditPedigreeFormat := System.Windows.Forms.GroupBox.Create;
  Self.RButton10 := System.Windows.Forms.RadioButton.Create;
  Self.RButton11 := System.Windows.Forms.RadioButton.Create;
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.ColorDialog1 := System.Windows.Forms.ColorDialog.Create;
  Self.FontDialog1 := System.Windows.Forms.FontDialog.Create;
  Self.PageControl1.SuspendLayout;
  Self.SheetCommon.SuspendLayout;
  Self.rgCode.SuspendLayout;
  Self.GroupBox4.SuspendLayout;
  Self.GroupBox7.SuspendLayout;
  Self.rgEditMode.SuspendLayout;
  Self.SheetTree.SuspendLayout;
  Self.GroupBox1.SuspendLayout;
  Self.GroupBox2.SuspendLayout;
  Self.SheetView.SuspendLayout;
  Self.PageControl2.SuspendLayout;
  Self.SheetViewCommon.SuspendLayout;
  Self.rgFNPFormat.SuspendLayout;
  Self.rgDateFormat.SuspendLayout;
  Self.SheetViewPersons.SuspendLayout;
  Self.SheetPedigree.SuspendLayout;
  Self.GroupBox5.SuspendLayout;
  Self.EditPedigreeFormat.SuspendLayout;
  Self.SuspendLayout;
  // 
  // PageControl1
  // 
  Self.PageControl1.Controls.Add(Self.SheetCommon);
  Self.PageControl1.Controls.Add(Self.SheetTree);
  Self.PageControl1.Controls.Add(Self.SheetView);
  Self.PageControl1.Controls.Add(Self.SheetPedigree);
  Self.PageControl1.Location := System.Drawing.Point.Create(0, 0);
  Self.PageControl1.Name := 'PageControl1';
  Self.PageControl1.SelectedIndex := 0;
  Self.PageControl1.Size := System.Drawing.Size.Create(513, 377);
  Self.PageControl1.TabIndex := 0;
  // 
  // SheetCommon
  // 
  Self.SheetCommon.Controls.Add(Self.Label6);
  Self.SheetCommon.Controls.Add(Self.rgCode);
  Self.SheetCommon.Controls.Add(Self.GroupBox4);
  Self.SheetCommon.Controls.Add(Self.GroupBox7);
  Self.SheetCommon.Controls.Add(Self.rgEditMode);
  Self.SheetCommon.Controls.Add(Self.cbLanguages);
  Self.SheetCommon.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetCommon.Name := 'SheetCommon';
  Self.SheetCommon.Size := System.Drawing.Size.Create(505, 351);
  Self.SheetCommon.TabIndex := 0;
  Self.SheetCommon.Text := 'Общие';
  // 
  // Label6
  // 
  Self.Label6.Location := System.Drawing.Point.Create(248, 64);
  Self.Label6.Name := 'Label6';
  Self.Label6.Size := System.Drawing.Size.Create(35, 13);
  Self.Label6.TabIndex := 0;
  Self.Label6.Text := 'Язык';
  //
  // rgCode
  // 
  Self.rgCode.Controls.Add(Self.RButton2);
  Self.rgCode.Controls.Add(Self.RButton1);
  Self.rgCode.Location := System.Drawing.Point.Create(8, 8);
  Self.rgCode.Name := 'rgCode';
  Self.rgCode.Size := System.Drawing.Size.Create(217, 49);
  Self.rgCode.TabIndex := 0;
  Self.rgCode.TabStop := False;
  Self.rgCode.Text := 'Кодировка сохранения файлов';
  // 
  // RButton2
  // 
  Self.RButton2.Location := System.Drawing.Point.Create(88, 16);
  Self.RButton2.Name := 'RButton2';
  Self.RButton2.Size := System.Drawing.Size.Create(80, 24);
  Self.RButton2.TabIndex := 1;
  Self.RButton2.Text := 'UTF-8';
  // 
  // RButton1
  // 
  Self.RButton1.Location := System.Drawing.Point.Create(8, 16);
  Self.RButton1.Name := 'RButton1';
  Self.RButton1.Size := System.Drawing.Size.Create(80, 24);
  Self.RButton1.TabIndex := 0;
  Self.RButton1.Text := 'ASCII';
  // 
  // GroupBox4
  // 
  Self.GroupBox4.Controls.Add(Self.Label1);
  Self.GroupBox4.Controls.Add(Self.Label2);
  Self.GroupBox4.Controls.Add(Self.Label3);
  Self.GroupBox4.Controls.Add(Self.Label4);
  Self.GroupBox4.Controls.Add(Self.chkProxy);
  Self.GroupBox4.Controls.Add(Self.edProxyServer);
  Self.GroupBox4.Controls.Add(Self.edProxyPort);
  Self.GroupBox4.Controls.Add(Self.edProxyLogin);
  Self.GroupBox4.Controls.Add(Self.edProxyPass);
  Self.GroupBox4.Location := System.Drawing.Point.Create(8, 128);
  Self.GroupBox4.Name := 'GroupBox4';
  Self.GroupBox4.Size := System.Drawing.Size.Create(217, 161);
  Self.GroupBox4.TabIndex := 1;
  Self.GroupBox4.TabStop := False;
  Self.GroupBox4.Text := 'Загрузка из Интернета';
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(16, 56);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(50, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Сервер';
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(16, 80);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(50, 13);
  Self.Label2.TabIndex := 1;
  Self.Label2.Text := 'Порт';
  // 
  // Label3
  // 
  Self.Label3.Location := System.Drawing.Point.Create(16, 104);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(50, 13);
  Self.Label3.TabIndex := 2;
  Self.Label3.Text := 'Логин';
  // 
  // Label4
  // 
  Self.Label4.Location := System.Drawing.Point.Create(16, 128);
  Self.Label4.Name := 'Label4';
  Self.Label4.Size := System.Drawing.Size.Create(50, 13);
  Self.Label4.TabIndex := 3;
  Self.Label4.Text := 'Пароль';
  // 
  // chkProxy
  // 
  Self.chkProxy.Location := System.Drawing.Point.Create(16, 24);
  Self.chkProxy.Name := 'chkProxy';
  Self.chkProxy.Size := System.Drawing.Size.Create(185, 17);
  Self.chkProxy.TabIndex := 0;
  Self.chkProxy.Text := 'Использовать прокси-сервер';
  // 
  // edProxyServer
  // 
  Self.edProxyServer.Location := System.Drawing.Point.Create(80, 48);
  Self.edProxyServer.Name := 'edProxyServer';
  Self.edProxyServer.Size := System.Drawing.Size.Create(121, 21);
  Self.edProxyServer.TabIndex := 1;
  Self.edProxyServer.Text := '';
  // 
  // edProxyPort
  // 
  Self.edProxyPort.Location := System.Drawing.Point.Create(80, 72);
  Self.edProxyPort.Name := 'edProxyPort';
  Self.edProxyPort.Size := System.Drawing.Size.Create(121, 21);
  Self.edProxyPort.TabIndex := 2;
  Self.edProxyPort.Text := '';
  // 
  // edProxyLogin
  // 
  Self.edProxyLogin.Location := System.Drawing.Point.Create(80, 96);
  Self.edProxyLogin.Name := 'edProxyLogin';
  Self.edProxyLogin.Size := System.Drawing.Size.Create(121, 21);
  Self.edProxyLogin.TabIndex := 3;
  Self.edProxyLogin.Text := '';
  // 
  // edProxyPass
  //
  Self.edProxyPass.Location := System.Drawing.Point.Create(80, 120);
  Self.edProxyPass.Name := 'edProxyPass';
  Self.edProxyPass.PasswordChar := '*';
  Self.edProxyPass.Size := System.Drawing.Size.Create(121, 21);
  Self.edProxyPass.TabIndex := 4;
  Self.edProxyPass.Text := 'edProxyPass';
  // 
  // GroupBox7
  // 
  Self.GroupBox7.Controls.Add(Self.chkShowOnStart);
  Self.GroupBox7.Location := System.Drawing.Point.Create(232, 8);
  Self.GroupBox7.Name := 'GroupBox7';
  Self.GroupBox7.Size := System.Drawing.Size.Create(265, 41);
  Self.GroupBox7.TabIndex := 2;
  Self.GroupBox7.TabStop := False;
  Self.GroupBox7.Text := 'Подсказки';
  // 
  // chkShowOnStart
  // 
  Self.chkShowOnStart.Location := System.Drawing.Point.Create(16, 16);
  Self.chkShowOnStart.Name := 'chkShowOnStart';
  Self.chkShowOnStart.Size := System.Drawing.Size.Create(225, 17);
  Self.chkShowOnStart.TabIndex := 0;
  Self.chkShowOnStart.Text := 'Показывать при старте';
  // 
  // rgEditMode
  // 
  Self.rgEditMode.Controls.Add(Self.RButton4);
  Self.rgEditMode.Controls.Add(Self.RButton3);
  Self.rgEditMode.Location := System.Drawing.Point.Create(8, 64);
  Self.rgEditMode.Name := 'rgEditMode';
  Self.rgEditMode.Size := System.Drawing.Size.Create(217, 57);
  Self.rgEditMode.TabIndex := 3;
  Self.rgEditMode.TabStop := False;
  Self.rgEditMode.Text := 'Режим работы';
  // 
  // RButton4
  // 
  Self.RButton4.Location := System.Drawing.Point.Create(88, 24);
  Self.RButton4.Name := 'RButton4';
  Self.RButton4.Size := System.Drawing.Size.Create(96, 24);
  Self.RButton4.TabIndex := 1;
  Self.RButton4.Text := 'Продвинутый';
  // 
  // RButton3
  // 
  Self.RButton3.Location := System.Drawing.Point.Create(8, 24);
  Self.RButton3.Name := 'RButton3';
  Self.RButton3.Size := System.Drawing.Size.Create(80, 24);
  Self.RButton3.TabIndex := 0;
  Self.RButton3.Text := 'Простой';
  // 
  // cbLanguages
  // 
  Self.cbLanguages.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.cbLanguages.Location := System.Drawing.Point.Create(312, 56);
  Self.cbLanguages.Name := 'cbLanguages';
  Self.cbLanguages.Size := System.Drawing.Size.Create(164, 21);
  Self.cbLanguages.TabIndex := 4;
  // 
  // SheetTree
  // 
  Self.SheetTree.Controls.Add(Self.GroupBox1);
  Self.SheetTree.Controls.Add(Self.GroupBox2);
  Self.SheetTree.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetTree.Name := 'SheetTree';
  Self.SheetTree.Size := System.Drawing.Size.Create(505, 351);
  Self.SheetTree.TabIndex := 2;
  Self.SheetTree.Text := 'Родословные древа';
  // 
  // GroupBox1
  // 
  Self.GroupBox1.Controls.Add(Self.chkFamily);
  Self.GroupBox1.Controls.Add(Self.chkName);
  Self.GroupBox1.Controls.Add(Self.chkPatronymic);
  Self.GroupBox1.Controls.Add(Self.chkDiffLines);
  Self.GroupBox1.Controls.Add(Self.chkBirthDate);
  Self.GroupBox1.Controls.Add(Self.chkDeathDate);
  Self.GroupBox1.Controls.Add(Self.chkKinship);
  Self.GroupBox1.Controls.Add(Self.chkOnlyYears);
  Self.GroupBox1.Controls.Add(Self.chkSignsVisible);
  Self.GroupBox1.Controls.Add(Self.chkChildlessExclude);
  Self.GroupBox1.Controls.Add(Self.chkTreeDecorative);
  Self.GroupBox1.Controls.Add(Self.chkPortraitsVisible);
  Self.GroupBox1.Location := System.Drawing.Point.Create(8, 8);
  Self.GroupBox1.Name := 'GroupBox1';
  Self.GroupBox1.Size := System.Drawing.Size.Create(289, 225);
  Self.GroupBox1.TabIndex := 0;
  Self.GroupBox1.TabStop := False;
  Self.GroupBox1.Text := 'Отображение персон в древе';
  // 
  // chkFamily
  // 
  Self.chkFamily.Location := System.Drawing.Point.Create(16, 16);
  Self.chkFamily.Name := 'chkFamily';
  Self.chkFamily.Size := System.Drawing.Size.Create(249, 17);
  Self.chkFamily.TabIndex := 0;
  Self.chkFamily.Text := 'Фамилия';
  // 
  // chkName
  // 
  Self.chkName.Location := System.Drawing.Point.Create(16, 32);
  Self.chkName.Name := 'chkName';
  Self.chkName.Size := System.Drawing.Size.Create(249, 17);
  Self.chkName.TabIndex := 1;
  Self.chkName.Text := 'Имя';
  // 
  // chkPatronymic
  // 
  Self.chkPatronymic.Location := System.Drawing.Point.Create(16, 48);
  Self.chkPatronymic.Name := 'chkPatronymic';
  Self.chkPatronymic.Size := System.Drawing.Size.Create(249, 17);
  Self.chkPatronymic.TabIndex := 2;
  Self.chkPatronymic.Text := 'Отчество';
  // 
  // chkDiffLines
  // 
  Self.chkDiffLines.Location := System.Drawing.Point.Create(16, 64);
  Self.chkDiffLines.Name := 'chkDiffLines';
  Self.chkDiffLines.Size := System.Drawing.Size.Create(249, 17);
  Self.chkDiffLines.TabIndex := 3;
  Self.chkDiffLines.Text := 'Разные строки (имя и отчество)';
  // 
  // chkBirthDate
  // 
  Self.chkBirthDate.Location := System.Drawing.Point.Create(16, 80);
  Self.chkBirthDate.Name := 'chkBirthDate';
  Self.chkBirthDate.Size := System.Drawing.Size.Create(249, 17);
  Self.chkBirthDate.TabIndex := 4;
  Self.chkBirthDate.Text := 'Дата рождения';
  // 
  // chkDeathDate
  // 
  Self.chkDeathDate.Location := System.Drawing.Point.Create(16, 96);
  Self.chkDeathDate.Name := 'chkDeathDate';
  Self.chkDeathDate.Size := System.Drawing.Size.Create(249, 17);
  Self.chkDeathDate.TabIndex := 5;
  Self.chkDeathDate.Text := 'Дата смерти';
  // 
  // chkKinship
  // 
  Self.chkKinship.Location := System.Drawing.Point.Create(16, 128);
  Self.chkKinship.Name := 'chkKinship';
  Self.chkKinship.Size := System.Drawing.Size.Create(249, 17);
  Self.chkKinship.TabIndex := 7;
  Self.chkKinship.Text := 'Степень родства';
  // 
  // chkOnlyYears
  // 
  Self.chkOnlyYears.Location := System.Drawing.Point.Create(32, 112);
  Self.chkOnlyYears.Name := 'chkOnlyYears';
  Self.chkOnlyYears.Size := System.Drawing.Size.Create(233, 17);
  Self.chkOnlyYears.TabIndex := 6;
  Self.chkOnlyYears.Text := 'Только годы';
  // 
  // chkSignsVisible
  // 
  Self.chkSignsVisible.Location := System.Drawing.Point.Create(16, 144);
  Self.chkSignsVisible.Name := 'chkSignsVisible';
  Self.chkSignsVisible.Size := System.Drawing.Size.Create(249, 17);
  Self.chkSignsVisible.TabIndex := 8;
  Self.chkSignsVisible.Text := 'Дополнительные символы';
  // 
  // chkChildlessExclude
  // 
  Self.chkChildlessExclude.Location := System.Drawing.Point.Create(16, 200);
  Self.chkChildlessExclude.Name := 'chkChildlessExclude';
  Self.chkChildlessExclude.Size := System.Drawing.Size.Create(249, 17);
  Self.chkChildlessExclude.TabIndex := 11;
  Self.chkChildlessExclude.Text := 'Исключить умерших в детстве';
  // 
  // chkTreeDecorative
  // 
  Self.chkTreeDecorative.Location := System.Drawing.Point.Create(16, 160);
  Self.chkTreeDecorative.Name := 'chkTreeDecorative';
  Self.chkTreeDecorative.Size := System.Drawing.Size.Create(249, 17);
  Self.chkTreeDecorative.TabIndex := 9;
  Self.chkTreeDecorative.Text := 'Декоративное оформление';
  // 
  // chkPortraitsVisible
  // 
  Self.chkPortraitsVisible.Location := System.Drawing.Point.Create(16, 176);
  Self.chkPortraitsVisible.Name := 'chkPortraitsVisible';
  Self.chkPortraitsVisible.Size := System.Drawing.Size.Create(249, 17);
  Self.chkPortraitsVisible.TabIndex := 10;
  Self.chkPortraitsVisible.Text := 'Отображать портреты';
  // 
  // GroupBox2
  // 
  Self.GroupBox2.Controls.Add(Self.Label5);
  Self.GroupBox2.Controls.Add(Self.PanMaleColor);
  Self.GroupBox2.Controls.Add(Self.PanFemaleColor);
  Self.GroupBox2.Controls.Add(Self.PanUnkSexColor);
  Self.GroupBox2.Controls.Add(Self.PanUnHusbandColor);
  Self.GroupBox2.Controls.Add(Self.PanUnWifeColor);
  Self.GroupBox2.Controls.Add(Self.PanDefFont);
  Self.GroupBox2.Location := System.Drawing.Point.Create(312, 8);
  Self.GroupBox2.Name := 'GroupBox2';
  Self.GroupBox2.Size := System.Drawing.Size.Create(185, 193);
  Self.GroupBox2.TabIndex := 1;
  Self.GroupBox2.TabStop := False;
  Self.GroupBox2.Text := 'Оформление';
  // 
  // Label5
  // 
  Self.Label5.Location := System.Drawing.Point.Create(16, 144);
  Self.Label5.Name := 'Label5';
  Self.Label5.Size := System.Drawing.Size.Create(50, 13);
  Self.Label5.TabIndex := 0;
  Self.Label5.Text := 'Шрифт';
  // 
  // PanMaleColor
  // 
  Self.PanMaleColor.BackColor := System.Drawing.SystemColors.Control;
  Self.PanMaleColor.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.PanMaleColor.Cursor := System.Windows.Forms.Cursors.Hand;
  Self.PanMaleColor.Location := System.Drawing.Point.Create(16, 16);
  Self.PanMaleColor.Name := 'PanMaleColor';
  Self.PanMaleColor.Size := System.Drawing.Size.Create(73, 25);
  Self.PanMaleColor.TabIndex := 0;
  Self.PanMaleColor.Text := 'Мужчина';
  Include(Self.PanMaleColor.Click, Self.PanColor_Click);
  // 
  // PanFemaleColor
  // 
  Self.PanFemaleColor.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.PanFemaleColor.Cursor := System.Windows.Forms.Cursors.Hand;
  Self.PanFemaleColor.Location := System.Drawing.Point.Create(96, 16);
  Self.PanFemaleColor.Name := 'PanFemaleColor';
  Self.PanFemaleColor.Size := System.Drawing.Size.Create(73, 25);
  Self.PanFemaleColor.TabIndex := 1;
  Self.PanFemaleColor.Text := 'Женщина';
  Include(Self.PanFemaleColor.Click, Self.PanColor_Click);
  // 
  // PanUnkSexColor
  // 
  Self.PanUnkSexColor.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.PanUnkSexColor.Cursor := System.Windows.Forms.Cursors.Hand;
  Self.PanUnkSexColor.Location := System.Drawing.Point.Create(16, 48);
  Self.PanUnkSexColor.Name := 'PanUnkSexColor';
  Self.PanUnkSexColor.Size := System.Drawing.Size.Create(153, 25);
  Self.PanUnkSexColor.TabIndex := 2;
  Self.PanUnkSexColor.Text := 'Неизвестный пол';
  Include(Self.PanUnkSexColor.Click, Self.PanColor_Click);
  // 
  // PanUnHusbandColor
  // 
  Self.PanUnHusbandColor.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.PanUnHusbandColor.Cursor := System.Windows.Forms.Cursors.Hand;
  Self.PanUnHusbandColor.Location := System.Drawing.Point.Create(16, 80);
  Self.PanUnHusbandColor.Name := 'PanUnHusbandColor';
  Self.PanUnHusbandColor.Size := System.Drawing.Size.Create(153, 25);
  Self.PanUnHusbandColor.TabIndex := 3;
  Self.PanUnHusbandColor.Text := 'Разведенный супруг';
  Include(Self.PanUnHusbandColor.Click, Self.PanColor_Click);
  // 
  // PanUnWifeColor
  // 
  Self.PanUnWifeColor.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.PanUnWifeColor.Cursor := System.Windows.Forms.Cursors.Hand;
  Self.PanUnWifeColor.Location := System.Drawing.Point.Create(16, 112);
  Self.PanUnWifeColor.Name := 'PanUnWifeColor';
  Self.PanUnWifeColor.Size := System.Drawing.Size.Create(153, 25);
  Self.PanUnWifeColor.TabIndex := 4;
  Self.PanUnWifeColor.Text := 'Разведенная супруга';
  Include(Self.PanUnWifeColor.Click, Self.PanColor_Click);
  // 
  // PanDefFont
  // 
  Self.PanDefFont.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.PanDefFont.Cursor := System.Windows.Forms.Cursors.Hand;
  Self.PanDefFont.Location := System.Drawing.Point.Create(16, 160);
  Self.PanDefFont.Name := 'PanDefFont';
  Self.PanDefFont.Size := System.Drawing.Size.Create(153, 25);
  Self.PanDefFont.TabIndex := 5;
  Include(Self.PanDefFont.Click, Self.PanDefFont_Click);
  // 
  // SheetView
  // 
  Self.SheetView.Controls.Add(Self.PageControl2);
  Self.SheetView.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetView.Name := 'SheetView';
  Self.SheetView.Size := System.Drawing.Size.Create(505, 351);
  Self.SheetView.TabIndex := 1;
  Self.SheetView.Text := 'Интерфейс';
  // 
  // PageControl2
  // 
  Self.PageControl2.Controls.Add(Self.SheetViewCommon);
  Self.PageControl2.Controls.Add(Self.SheetViewPersons);
  Self.PageControl2.Location := System.Drawing.Point.Create(0, 0);
  Self.PageControl2.Name := 'PageControl2';
  Self.PageControl2.SelectedIndex := 0;
  Self.PageControl2.Size := System.Drawing.Size.Create(505, 349);
  Self.PageControl2.TabIndex := 0;
  //
  // SheetViewCommon
  // 
  Self.SheetViewCommon.Controls.Add(Self.rgFNPFormat);
  Self.SheetViewCommon.Controls.Add(Self.rgDateFormat);
  Self.SheetViewCommon.Controls.Add(Self.chkPlacesWithAddress);
  Self.SheetViewCommon.Controls.Add(Self.chkHighlightUnparented);
  Self.SheetViewCommon.Controls.Add(Self.chkHighlightUnmarried);
  Self.SheetViewCommon.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetViewCommon.Name := 'SheetViewCommon';
  Self.SheetViewCommon.Size := System.Drawing.Size.Create(497, 323);
  Self.SheetViewCommon.TabIndex := 0;
  Self.SheetViewCommon.Text := 'Все списки';
  // 
  // rgFNPFormat
  // 
  Self.rgFNPFormat.Controls.Add(Self.RButton7);
  Self.rgFNPFormat.Controls.Add(Self.RButton6);
  Self.rgFNPFormat.Controls.Add(Self.RButton5);
  Self.rgFNPFormat.Location := System.Drawing.Point.Create(8, 8);
  Self.rgFNPFormat.Name := 'rgFNPFormat';
  Self.rgFNPFormat.Size := System.Drawing.Size.Create(185, 97);
  Self.rgFNPFormat.TabIndex := 0;
  Self.rgFNPFormat.TabStop := False;
  Self.rgFNPFormat.Text := 'Формат имен в списках';
  // 
  // RButton7
  // 
  Self.RButton7.Location := System.Drawing.Point.Create(8, 64);
  Self.RButton7.Name := 'RButton7';
  Self.RButton7.Size := System.Drawing.Size.Create(160, 24);
  Self.RButton7.TabIndex := 2;
  Self.RButton7.Text := 'Фамилия; Имя; Отчество';
  // 
  // RButton6
  // 
  Self.RButton6.Location := System.Drawing.Point.Create(8, 40);
  Self.RButton6.Name := 'RButton6';
  Self.RButton6.Size := System.Drawing.Size.Create(160, 24);
  Self.RButton6.TabIndex := 1;
  Self.RButton6.Text := 'Фамилия; Имя_Отчество';
  // 
  // RButton5
  // 
  Self.RButton5.Location := System.Drawing.Point.Create(8, 17);
  Self.RButton5.Name := 'RButton5';
  Self.RButton5.Size := System.Drawing.Size.Create(160, 24);
  Self.RButton5.TabIndex := 0;
  Self.RButton5.Text := 'Фамилия_Имя_Отчество';
  // 
  // rgDateFormat
  // 
  Self.rgDateFormat.Controls.Add(Self.RButton9);
  Self.rgDateFormat.Controls.Add(Self.RButton8);
  Self.rgDateFormat.Location := System.Drawing.Point.Create(8, 112);
  Self.rgDateFormat.Name := 'rgDateFormat';
  Self.rgDateFormat.Size := System.Drawing.Size.Create(185, 72);
  Self.rgDateFormat.TabIndex := 1;
  Self.rgDateFormat.TabStop := False;
  Self.rgDateFormat.Text := 'Формат даты в списках';
  // 
  // RButton9
  // 
  Self.RButton9.Location := System.Drawing.Point.Create(8, 40);
  Self.RButton9.Name := 'RButton9';
  Self.RButton9.TabIndex := 1;
  Self.RButton9.Text := 'YYYY.MM.DD';
  // 
  // RButton8
  // 
  Self.RButton8.Location := System.Drawing.Point.Create(8, 16);
  Self.RButton8.Name := 'RButton8';
  Self.RButton8.TabIndex := 0;
  Self.RButton8.Text := 'DD.MM.YYYY';
  //
  // chkPlacesWithAddress
  // 
  Self.chkPlacesWithAddress.Location := System.Drawing.Point.Create(8, 200);
  Self.chkPlacesWithAddress.Name := 'chkPlacesWithAddress';
  Self.chkPlacesWithAddress.Size := System.Drawing.Size.Create(185, 17);
  Self.chkPlacesWithAddress.TabIndex := 2;
  Self.chkPlacesWithAddress.Text := 'Включать адрес в строки мест';
  // 
  // chkHighlightUnparented
  // 
  Self.chkHighlightUnparented.Location := System.Drawing.Point.Create(8, 224);
  Self.chkHighlightUnparented.Name := 'chkHighlightUnparented';
  Self.chkHighlightUnparented.Size := System.Drawing.Size.Create(241, 17);
  Self.chkHighlightUnparented.TabIndex := 3;
  Self.chkHighlightUnparented.Text := 'Подсвечивать персоны без родителей';
  // 
  // chkHighlightUnmarried
  // 
  Self.chkHighlightUnmarried.Location := System.Drawing.Point.Create(8, 248);
  Self.chkHighlightUnmarried.Name := 'chkHighlightUnmarried';
  Self.chkHighlightUnmarried.Size := System.Drawing.Size.Create(241, 17);
  Self.chkHighlightUnmarried.TabIndex := 4;
  Self.chkHighlightUnmarried.Text := 'Подсвечивать персоны без семьи';
  //
  // SheetViewPersons
  // 
  Self.SheetViewPersons.Controls.Add(Self.btnColumnUp);
  Self.SheetViewPersons.Controls.Add(Self.btnColumnDown);
  Self.SheetViewPersons.Controls.Add(Self.ListPersonColumns);
  Self.SheetViewPersons.Controls.Add(Self.btnDefList);
  Self.SheetViewPersons.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetViewPersons.Name := 'SheetViewPersons';
  Self.SheetViewPersons.Size := System.Drawing.Size.Create(497, 323);
  Self.SheetViewPersons.TabIndex := 1;
  Self.SheetViewPersons.Text := 'Список персон';
  // 
  // btnColumnUp
  // 
  Self.btnColumnUp.Location := System.Drawing.Point.Create(352, 8);
  Self.btnColumnUp.Name := 'btnColumnUp';
  Self.btnColumnUp.Size := System.Drawing.Size.Create(24, 24);
  Self.btnColumnUp.TabIndex := 0;
  Include(Self.btnColumnUp.Click, Self.btnColumnUp_Click);
  // 
  // btnColumnDown
  // 
  Self.btnColumnDown.Location := System.Drawing.Point.Create(352, 40);
  Self.btnColumnDown.Name := 'btnColumnDown';
  Self.btnColumnDown.Size := System.Drawing.Size.Create(24, 24);
  Self.btnColumnDown.TabIndex := 1;
  Include(Self.btnColumnDown.Click, Self.btnColumnDown_Click);
  // 
  // ListPersonColumns
  // 
  Self.ListPersonColumns.Location := System.Drawing.Point.Create(8, 8);
  Self.ListPersonColumns.Name := 'ListPersonColumns';
  Self.ListPersonColumns.Size := System.Drawing.Size.Create(337, 292);
  Self.ListPersonColumns.TabIndex := 0;
  Include(Self.ListPersonColumns.ItemCheck, Self.ListPersonColumns_ItemCheck);
  // 
  // btnDefList
  // 
  Self.btnDefList.Location := System.Drawing.Point.Create(352, 288);
  Self.btnDefList.Name := 'btnDefList';
  Self.btnDefList.Size := System.Drawing.Size.Create(137, 25);
  Self.btnDefList.TabIndex := 1;
  Self.btnDefList.Text := 'Значения по умолчанию';
  Include(Self.btnDefList.Click, Self.btnDefList_Click);
  // 
  // SheetPedigree
  // 
  Self.SheetPedigree.Controls.Add(Self.GroupBox5);
  Self.SheetPedigree.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetPedigree.Name := 'SheetPedigree';
  Self.SheetPedigree.Size := System.Drawing.Size.Create(505, 351);
  Self.SheetPedigree.TabIndex := 3;
  Self.SheetPedigree.Text := 'Росписи';
  // 
  // GroupBox5
  // 
  Self.GroupBox5.Controls.Add(Self.chkAttributes);
  Self.GroupBox5.Controls.Add(Self.chkNotes);
  Self.GroupBox5.Controls.Add(Self.chkSources);
  Self.GroupBox5.Controls.Add(Self.EditPedigreeFormat);
  Self.GroupBox5.Location := System.Drawing.Point.Create(8, 8);
  Self.GroupBox5.Name := 'GroupBox5';
  Self.GroupBox5.Size := System.Drawing.Size.Create(289, 160);
  Self.GroupBox5.TabIndex := 0;
  Self.GroupBox5.TabStop := False;
  Self.GroupBox5.Text := 'Генерация росписей';
  // 
  // chkAttributes
  // 
  Self.chkAttributes.Location := System.Drawing.Point.Create(16, 16);
  Self.chkAttributes.Name := 'chkAttributes';
  Self.chkAttributes.Size := System.Drawing.Size.Create(249, 17);
  Self.chkAttributes.TabIndex := 0;
  Self.chkAttributes.Text := 'Включая атрибуты персон';
  // 
  // chkNotes
  // 
  Self.chkNotes.Location := System.Drawing.Point.Create(16, 32);
  Self.chkNotes.Name := 'chkNotes';
  Self.chkNotes.Size := System.Drawing.Size.Create(249, 17);
  Self.chkNotes.TabIndex := 1;
  Self.chkNotes.Text := 'Включая заметки';
  // 
  // chkSources
  // 
  Self.chkSources.Location := System.Drawing.Point.Create(16, 48);
  Self.chkSources.Name := 'chkSources';
  Self.chkSources.Size := System.Drawing.Size.Create(249, 17);
  Self.chkSources.TabIndex := 2;
  Self.chkSources.Text := 'Включая источники';
  // 
  // EditPedigreeFormat
  // 
  Self.EditPedigreeFormat.Controls.Add(Self.RButton10);
  Self.EditPedigreeFormat.Controls.Add(Self.RButton11);
  Self.EditPedigreeFormat.Location := System.Drawing.Point.Create(16, 72);
  Self.EditPedigreeFormat.Name := 'EditPedigreeFormat';
  Self.EditPedigreeFormat.Size := System.Drawing.Size.Create(249, 72);
  Self.EditPedigreeFormat.TabIndex := 3;
  Self.EditPedigreeFormat.TabStop := False;
  Self.EditPedigreeFormat.Text := 'Формат';
  // 
  // RButton10
  // 
  Self.RButton10.Location := System.Drawing.Point.Create(16, 16);
  Self.RButton10.Name := 'RButton10';
  Self.RButton10.TabIndex := 3;
  Self.RButton10.Text := 'Избыточный';
  // 
  // RButton11
  //
  Self.RButton11.Location := System.Drawing.Point.Create(16, 40);
  Self.RButton11.Name := 'RButton11';
  Self.RButton11.TabIndex := 2;
  Self.RButton11.Text := 'Традиционный';
  // 
  // btnAccept
  // 
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(336, 392);
  Self.btnAccept.Name := 'btnAccept';
  Self.btnAccept.Size := System.Drawing.Size.Create(81, 25);
  Self.btnAccept.TabIndex := 1;
  Self.btnAccept.Text := 'Принять';
  Self.btnAccept.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  Include(Self.btnAccept.Click, Self.btnAccept_Click);
  // 
  // btnCancel
  // 
  Self.btnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnCancel.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnCancel.Location := System.Drawing.Point.Create(424, 392);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 2;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // TfmOptions
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(513, 425);
  Self.Controls.Add(Self.PageControl1);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular,
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmOptions';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Настройки';
  Self.PageControl1.ResumeLayout(False);
  Self.SheetCommon.ResumeLayout(False);
  Self.rgCode.ResumeLayout(False);
  Self.GroupBox4.ResumeLayout(False);
  Self.GroupBox7.ResumeLayout(False);
  Self.rgEditMode.ResumeLayout(False);
  Self.SheetTree.ResumeLayout(False);
  Self.GroupBox1.ResumeLayout(False);
  Self.GroupBox2.ResumeLayout(False);
  Self.SheetView.ResumeLayout(False);
  Self.PageControl2.ResumeLayout(False);
  Self.SheetViewCommon.ResumeLayout(False);
  Self.rgFNPFormat.ResumeLayout(False);
  Self.rgDateFormat.ResumeLayout(False);
  Self.SheetViewPersons.ResumeLayout(False);
  Self.SheetPedigree.ResumeLayout(False);
  Self.GroupBox5.ResumeLayout(False);
  Self.EditPedigreeFormat.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmOptions.Create;
begin
  inherited Create;
  InitializeComponent;

  FOptions := fmGEDKeeper.Options;

  SetLang();

  UpdateForm();
end;

procedure TfmOptions.SetLang();
begin
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  Text := LSList[LSID_MIOptions];

  SheetCommon.Text := LSList[LSID_Common];
  SheetView.Text := LSList[LSID_Interface];
  SheetTree.Text := LSList[LSID_Trees];
  SheetPedigree.Text := LSList[LSID_Pedigrees];

  ///

  rgCode.Text := LSList[LSID_SaveCoding];
  rgEditMode.Text := LSList[LSID_WorkMode];
  RButton3.Text := LSList[LSID_Simple];
  RButton4.Text := LSList[LSID_Expert];

  GroupBox4.Text := LSList[LSID_Internet];
  chkProxy.Text := LSList[LSID_ProxyUse];
  Label1.Text := LSList[LSID_ProxyServer];
  Label2.Text := LSList[LSID_ProxyPort];
  Label3.Text := LSList[LSID_ProxyLogin];
  Label4.Text := LSList[LSID_ProxyPassword];

  GroupBox7.Text := LSList[LSID_Tips];
  chkShowOnStart.Text := LSList[LSID_StartupTips];

  Label6.Text := LSList[LSID_Language];

  SheetViewCommon.Text := LSList[LSID_ListsAll];
  SheetViewPersons.Text := LSList[LSID_ListPersons];
    rgFNPFormat.Text := LSList[LSID_NamesFormat];
    RButton5.Text := LSList[LSID_NF1];
    RButton6.Text := LSList[LSID_NF2];
    RButton7.Text := LSList[LSID_NF3];
    rgDateFormat.Text := LSList[LSID_DateFormat];
    chkPlacesWithAddress.Text := LSList[LSID_PlacesWithAddress];
    chkHighlightUnparented.Text := LSList[LSID_HighlightUnparented];
    chkHighlightUnmarried.Text := LSList[LSID_HighlightUnmarried];
    btnDefList.Text := LSList[LSID_DefList];

  GroupBox1.Text := LSList[LSID_ViewTree];
    chkFamily.Text := LSList[LSID_Surname];
    chkName.Text := LSList[LSID_Name];
    chkPatronymic.Text := LSList[LSID_Patronymic];
    chkDiffLines.Text := LSList[LSID_DiffLines];          
    chkBirthDate.Text := LSList[LSID_BirthDate];
    chkDeathDate.Text := LSList[LSID_DeathDate];
    chkOnlyYears.Text := LSList[LSID_OnlyYears];
    chkKinship.Text := LSList[LSID_Kinship];
    chkSignsVisible.Text := LSList[LSID_SignsVisible];
    chkTreeDecorative.Text := LSList[LSID_TreeDecorative];
    chkPortraitsVisible.Text := LSList[LSID_PortraitsVisible];
    chkChildlessExclude.Text := LSList[LSID_ChildlessExclude];

  GroupBox2.Text := LSList[LSID_Decor];
    PanMaleColor.Text := LSList[LSID_Man];
    PanFemaleColor.Text := LSList[LSID_Woman];
    PanUnkSexColor.Text := LSList[LSID_UnkSex];      
    PanUnHusbandColor.Text := LSList[LSID_UnHusband];
    PanUnWifeColor.Text := LSList[LSID_UnWife];
    Label5.Text := LSList[LSID_Font];

  GroupBox5.Text := LSList[LSID_PedigreeGen];
    chkAttributes.Text := LSList[LSID_IncludeAttributes];
    chkNotes.Text := LSList[LSID_IncludeNotes];
    chkSources.Text := LSList[LSID_IncludeSources];
    EditPedigreeFormat.Text := LSList[LSID_PedigreeFormat];
    RButton10.Text := LSList[LSID_PF1];
    RButton11.Text := LSList[LSID_PF2];
end;

procedure TfmOptions.UpdateControls();
begin
  //PanDefFont.Font.Assign(FOptions.ChartOptions.DefFont);alert
  PanDefFont.Text := FOptions.ChartOptions.DefFont_Name + ', ' +
    FOptions.ChartOptions.DefFont_Size.ToString();
end;

procedure TfmOptions.UpdateForm();
var
  i, idx: Integer;
  lng_rec: TGlobalOptions.TLangRecord;
begin
  case FOptions.DefCharacterSet of
    csASCII: RButton1.Checked := True;
    csUTF8: RButton2.Checked := True;
  end;

  case FOptions.DefNameFormat of
    GKEngine.nfFNP: RButton5.Checked := True;
    GKEngine.nfF_NP: RButton6.Checked := True;
    GKEngine.nfF_N_P: RButton7.Checked := True;
  end;

  case FOptions.DefDateFormat of
    dfDD_MM_YYYY: RButton8.Checked := True;
    dfYYYY_MM_DD: RButton9.Checked := True;
  end;

  chkPlacesWithAddress.Checked := FOptions.PlacesWithAddress;
  chkHighlightUnparented.Checked := FOptions.ListPersons_HighlightUnparented;
  chkHighlightUnmarried.Checked := FOptions.ListPersons_HighlightUnmarried;

  chkFamily.Checked := FOptions.ChartOptions.FamilyVisible;
  chkName.Checked := FOptions.ChartOptions.NameVisible;
  chkPatronymic.Checked := FOptions.ChartOptions.PatronymicVisible;
  chkDiffLines.Checked := FOptions.ChartOptions.DiffLines;
  chkBirthDate.Checked := FOptions.ChartOptions.BirthDateVisible;
  chkDeathDate.Checked := FOptions.ChartOptions.DeathDateVisible;
  chkOnlyYears.Checked := FOptions.ChartOptions.OnlyYears;
  chkKinship.Checked := FOptions.ChartOptions.Kinship;
  chkSignsVisible.Checked := FOptions.ChartOptions.SignsVisible;

  chkChildlessExclude.Checked := FOptions.ChartOptions.ChildlessExclude;
  chkTreeDecorative.Checked := FOptions.ChartOptions.Decorative;
  chkPortraitsVisible.Checked := FOptions.ChartOptions.PortraitsVisible;

  PanMaleColor.BackColor := FOptions.ChartOptions.MaleColor;
  PanFemaleColor.BackColor := FOptions.ChartOptions.FemaleColor;
  PanUnkSexColor.BackColor := FOptions.ChartOptions.UnkSexColor;
  PanUnHusbandColor.BackColor := FOptions.ChartOptions.UnHusbandColor;
  PanUnWifeColor.BackColor := FOptions.ChartOptions.UnWifeColor;

  chkProxy.Checked := FOptions.Proxy.UseProxy;
  edProxyServer.Text := FOptions.Proxy.Server;
  edProxyPort.Text := FOptions.Proxy.Port;
  edProxyLogin.Text := FOptions.Proxy.Login;
  edProxyPass.Text := FOptions.Proxy.Password;

  chkAttributes.Checked := FOptions.PedigreeOptions.IncludeAttributes;
  chkNotes.Checked := FOptions.PedigreeOptions.IncludeNotes;
  chkSources.Checked := FOptions.PedigreeOptions.IncludeSources;

  case FOptions.PedigreeOptions.Format of
    GKCommon.pfExcess: RButton10.Checked := True;
    GKCommon.pfCompact: RButton11.Checked := True;
  end;

  chkShowOnStart.Checked := FOptions.ShowTips;

  case FOptions.WorkMode of
    GKCommon.wmSimple: RButton3.Checked := True;
    GKCommon.wmExpert: RButton4.Checked := True;
  end;

  FPersonColumns := FOptions.ListPersonsColumns;
  UpdateColumnsList();

  UpdateControls();

  cbLanguages.Items.Clear;
  cbLanguages.Items.Add(TTaggedComboItem.Create(LSDefName, LSDefCode));
  idx := 0;
  for i := 0 to FOptions.LangsCount - 1 do begin
    lng_rec := FOptions.Langs[i];
    if (FOptions.InterfaceLang = lng_rec.Code) then idx := i + 1;
    cbLanguages.Items.Add(TTaggedComboItem.Create(lng_rec.Name, lng_rec.Code));
  end;
  cbLanguages.SelectedIndex := idx;
end;

procedure TfmOptions.btnAccept_Click(sender: System.Object; e: System.EventArgs);
var
  code: Integer;
begin
  FOptions.ListPersonsColumns := FPersonColumns;

  if RButton1.Checked then FOptions.DefCharacterSet := csASCII
  else
  if RButton2.Checked then FOptions.DefCharacterSet := csUTF8;

  //

  if RButton5.Checked then FOptions.DefNameFormat := TGenEngine.TNameFormat.nfFNP
  else
  if RButton6.Checked then FOptions.DefNameFormat := TGenEngine.TNameFormat.nfF_NP
  else
  if RButton7.Checked then FOptions.DefNameFormat := TGenEngine.TNameFormat.nfF_N_P;

  //

  if RButton8.Checked then FOptions.DefDateFormat := TGenEngine.TDateFormat.dfDD_MM_YYYY
  else
  if RButton9.Checked then FOptions.DefDateFormat := TGenEngine.TDateFormat.dfYYYY_MM_DD;

  FOptions.PlacesWithAddress := chkPlacesWithAddress.Checked;
  FOptions.ListPersons_HighlightUnparented := chkHighlightUnparented.Checked;
  FOptions.ListPersons_HighlightUnmarried := chkHighlightUnmarried.Checked;

  FOptions.ChartOptions.FamilyVisible := chkFamily.Checked;
  FOptions.ChartOptions.NameVisible := chkName.Checked;
  FOptions.ChartOptions.PatronymicVisible := chkPatronymic.Checked;
  FOptions.ChartOptions.DiffLines := chkDiffLines.Checked;
  FOptions.ChartOptions.BirthDateVisible := chkBirthDate.Checked;
  FOptions.ChartOptions.DeathDateVisible := chkDeathDate.Checked;
  FOptions.ChartOptions.OnlyYears := chkOnlyYears.Checked;
  FOptions.ChartOptions.Kinship := chkKinship.Checked;
  FOptions.ChartOptions.SignsVisible := chkSignsVisible.Checked;

  FOptions.ChartOptions.ChildlessExclude := chkChildlessExclude.Checked;
  FOptions.ChartOptions.Decorative := chkTreeDecorative.Checked;
  FOptions.ChartOptions.PortraitsVisible := chkPortraitsVisible.Checked;

  FOptions.ChartOptions.MaleColor := PanMaleColor.BackColor;
  FOptions.ChartOptions.FemaleColor := PanFemaleColor.BackColor;
  FOptions.ChartOptions.UnkSexColor := PanUnkSexColor.BackColor;
  FOptions.ChartOptions.UnHusbandColor := PanUnHusbandColor.BackColor;
  FOptions.ChartOptions.UnWifeColor := PanUnWifeColor.BackColor;

  FOptions.Proxy.UseProxy := chkProxy.Checked;
  FOptions.Proxy.Server := edProxyServer.Text;
  FOptions.Proxy.Port := edProxyPort.Text;
  FOptions.Proxy.Login := edProxyLogin.Text;
  FOptions.Proxy.Password := edProxyPass.Text;

  FOptions.PedigreeOptions.IncludeAttributes := chkAttributes.Checked;
  FOptions.PedigreeOptions.IncludeNotes := chkNotes.Checked;
  FOptions.PedigreeOptions.IncludeSources := chkSources.Checked;

  if RButton10.Checked then FOptions.PedigreeOptions.Format := TPedigreeOptions.TPedigreeFormat.pfExcess
  else
  if RButton11.Checked then FOptions.PedigreeOptions.Format := TPedigreeOptions.TPedigreeFormat.pfCompact;

  FOptions.ShowTips := chkShowOnStart.Checked;

  if RButton3.Checked then FOptions.WorkMode := TGlobalOptions.TWorkMode.wmSimple
  else
  if RButton4.Checked then FOptions.WorkMode := TGlobalOptions.TWorkMode.wmExpert;

  code := TTaggedComboItem(cbLanguages.Items[cbLanguages.SelectedIndex]).Tag;
  fmGEDKeeper.LoadLanguage(code);

  Self.DialogResult := System.Windows.Forms.DialogResult.OK;
end;

procedure TfmOptions.PanColor_Click(sender: System.Object; e: System.EventArgs);
begin
  ColorDialog1.Color := Panel(Sender).BackColor;

  if (ColorDialog1.ShowDialog = System.Windows.Forms.DialogResult.OK)
  then Panel(Sender).BackColor := ColorDialog1.Color;
end;

procedure TfmOptions.PanDefFont_Click(sender: System.Object; e: System.EventArgs);
begin
  if (FontDialog1.ShowDialog = System.Windows.Forms.DialogResult.OK)
  then begin
    FOptions.ChartOptions.DefFont_Name := FontDialog1.Font.Name;
    FOptions.ChartOptions.DefFont_Size := Round(FontDialog1.Font.Size);
  end;

  UpdateControls();
end;

procedure TfmOptions.UpdateColumnsList();
var
  i: Integer;
  pct: TGlobalOptions.TPersonColumnType;
begin
  Exclude(Self.ListPersonColumns.ItemCheck, Self.ListPersonColumns_ItemCheck);
  ListPersonColumns.BeginUpdate;
  try
    ListPersonColumns.Items.Clear;

    for i := 0 to High(FPersonColumns) do begin
      pct := FPersonColumns[i].colType;

      ListPersonColumns.Items.Add(
        LSList[TGlobalOptions.PersonColumnsName[pct].Name], FPersonColumns[i].colActive);
    end;
  finally
    ListPersonColumns.EndUpdate;
  end;
  Include(Self.ListPersonColumns.ItemCheck, Self.ListPersonColumns_ItemCheck);
end;

procedure TfmOptions.btnColumnUp_Click(sender: System.Object; e: System.EventArgs);
var
  idx: Integer;
  props: TGlobalOptions.TPersonColumnProps;
begin
  idx := ListPersonColumns.SelectedIndex;
  if (idx <= 0) then Exit;

  props := FPersonColumns[idx - 1];
  FPersonColumns[idx - 1] := FPersonColumns[idx];
  FPersonColumns[idx] := props;

  UpdateColumnsList();
  ListPersonColumns.SelectedIndex := idx - 1;
end;

procedure TfmOptions.btnColumnDown_Click(sender: System.Object; e: System.EventArgs);
var
  idx: Integer;
  props: TGlobalOptions.TPersonColumnProps;
begin
  idx := ListPersonColumns.SelectedIndex;
  if (idx < 0) or (idx >= High(FPersonColumns)) then Exit;

  props := FPersonColumns[idx + 1];
  FPersonColumns[idx + 1] := FPersonColumns[idx];
  FPersonColumns[idx] := props;

  UpdateColumnsList();
  ListPersonColumns.SelectedIndex := idx + 1;
end;

procedure TfmOptions.ListPersonColumns_ItemCheck(sender: System.Object; e: System.Windows.Forms.ItemCheckEventArgs);
var
  cs: Boolean;
begin
  case e.NewValue of
    CheckState.Unchecked: cs := False;
    CheckState.Checked: cs := True;
  end;
  //checkit
  {if (Item <> nil) then }FPersonColumns[e.Index].colActive := cs;
end;

procedure TfmOptions.btnDefList_Click(sender: System.Object; e: System.EventArgs);
begin
  FPersonColumns := TGlobalOptions.DefPersonColumns;
  UpdateColumnsList();
end;

end.
