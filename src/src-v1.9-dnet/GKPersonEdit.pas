unit GKPersonEdit; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Resources,
  VCLStub, GedCom551, GKBase, GKEngine, GKCtrls, GKLists, GKUtils, GKMain, GKLangs;

type
  TfmPersonEdit = class(System.Windows.Forms.Form)
  strict private
    PagesPersonData: System.Windows.Forms.TabControl;
    SheetEvents: System.Windows.Forms.TabPage;
    SheetNotes: System.Windows.Forms.TabPage;
    SheetMultimedia: System.Windows.Forms.TabPage;
    SheetSources: System.Windows.Forms.TabPage;
    SheetSpouses: System.Windows.Forms.TabPage;
    SheetAssociations: System.Windows.Forms.TabPage;
    SheetGroups: System.Windows.Forms.TabPage;
    btnAccept: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    Label5: System.Windows.Forms.Label;
    cbRestriction: System.Windows.Forms.ComboBox;
    GroupBox1: System.Windows.Forms.GroupBox;
    Label1: System.Windows.Forms.Label;
    Label2: System.Windows.Forms.Label;
    Label3: System.Windows.Forms.Label;
    Label4: System.Windows.Forms.Label;
    EditFamily: System.Windows.Forms.TextBox;
    EditName: System.Windows.Forms.TextBox;
    EditPatronymic: System.Windows.Forms.TextBox;
    EditSex: System.Windows.Forms.ComboBox;
    CheckPatriarch: System.Windows.Forms.CheckBox;
    SheetUserRefs: System.Windows.Forms.TabPage;
    PageCtlParents: System.Windows.Forms.Panel;
    Label12: System.Windows.Forms.Label;
    EditFather: System.Windows.Forms.TextBox;
    btnParentsAdd: System.Windows.Forms.Button;
    btnParentsEdit: System.Windows.Forms.Button;
    btnParentsDelete: System.Windows.Forms.Button;
    chkBookmark: System.Windows.Forms.CheckBox;
    Label8: System.Windows.Forms.Label;
    edPieceSurnamePrefix: System.Windows.Forms.TextBox;
    Label6: System.Windows.Forms.Label;
    edPiecePrefix: System.Windows.Forms.TextBox;
    Label9: System.Windows.Forms.Label;
    edPieceSuffix: System.Windows.Forms.TextBox;
    Label7: System.Windows.Forms.Label;
    edPieceNickname: System.Windows.Forms.TextBox;
    panPortrait: System.Windows.Forms.Panel;
    imgPortrait: System.Windows.Forms.PictureBox;
    btnNameCopy: System.Windows.Forms.Button;
    btnPortraitAdd: System.Windows.Forms.Button;
    btnPortraitDelete: System.Windows.Forms.Button;
    btnFatherAdd: System.Windows.Forms.Button;
    btnFatherDelete: System.Windows.Forms.Button;
    btnFatherSel: System.Windows.Forms.Button;
    btnMotherAdd: System.Windows.Forms.Button;
    btnMotherDelete: System.Windows.Forms.Button;
    btnMotherSel: System.Windows.Forms.Button;

    FBase: TfmBase;
    FPerson: TGEDCOMIndividualRecord;

    FEventsList: TSheetList;
    FSpousesList: TSheetList;
    FAssociationsList: TSheetList;
    FGroupsList: TSheetList;
    FNotesList: TSheetList;
    FMediaList: TSheetList;
    FSourcesList: TSheetList;
    FUserRefList: TSheetList;
    EditMother: System.Windows.Forms.TextBox;

    procedure PortraitRefresh();
    procedure ControlsRefresh();
    procedure SetPerson(const Value: TGEDCOMIndividualRecord);
    procedure AcceptChanges();
    procedure SetTitle();
    procedure ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);

    procedure InitializeComponent;

    procedure EditFamily_TextChanged(sender: System.Object; e: System.EventArgs);
    procedure EditName_TextChanged(sender: System.Object; e: System.EventArgs);
    procedure EditPatronymic_TextChanged(sender: System.Object; e: System.EventArgs);
    procedure btnFatherAdd_Click(sender: System.Object; e: System.EventArgs);
    procedure btnFatherDelete_Click(sender: System.Object; e: System.EventArgs);
    procedure btnFatherSel_Click(sender: System.Object; e: System.EventArgs);
    procedure btnMotherAdd_Click(sender: System.Object; e: System.EventArgs);
    procedure btnMotherDelete_Click(sender: System.Object; e: System.EventArgs);
    procedure btnMotherSel_Click(sender: System.Object; e: System.EventArgs);
    procedure btnParentsAdd_Click(sender: System.Object; e: System.EventArgs);
    procedure btnParentsEdit_Click(sender: System.Object; e: System.EventArgs);
    procedure btnParentsDelete_Click(sender: System.Object; e: System.EventArgs);
    procedure btnNameCopy1_Click(sender: System.Object; e: System.EventArgs);
    procedure btnPortraitAdd_Click(sender: System.Object; e: System.EventArgs);
    procedure btnPortraitDelete_Click(sender: System.Object; e: System.EventArgs);
    procedure EditFamily_KeyPress(sender: System.Object; e: System.Windows.Forms.KeyPressEventArgs);
    procedure btnAccept_Click(sender: System.Object; e: System.EventArgs);
    procedure cbRestriction_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property Person: TGEDCOMIndividualRecord read FPerson write SetPerson;

    procedure SetLang();
  end;

implementation

procedure TfmPersonEdit.InitializeComponent;
begin
  Self.PagesPersonData := System.Windows.Forms.TabControl.Create;
  Self.SheetEvents := System.Windows.Forms.TabPage.Create;
  Self.SheetSpouses := System.Windows.Forms.TabPage.Create;
  Self.SheetAssociations := System.Windows.Forms.TabPage.Create;
  Self.SheetGroups := System.Windows.Forms.TabPage.Create;
  Self.SheetNotes := System.Windows.Forms.TabPage.Create;
  Self.SheetMultimedia := System.Windows.Forms.TabPage.Create;
  Self.SheetSources := System.Windows.Forms.TabPage.Create;
  Self.SheetUserRefs := System.Windows.Forms.TabPage.Create;
  Self.btnAccept := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.Label5 := System.Windows.Forms.Label.Create;
  Self.cbRestriction := System.Windows.Forms.ComboBox.Create;
  Self.GroupBox1 := System.Windows.Forms.GroupBox.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.Label3 := System.Windows.Forms.Label.Create;
  Self.Label4 := System.Windows.Forms.Label.Create;
  Self.Label8 := System.Windows.Forms.Label.Create;
  Self.Label6 := System.Windows.Forms.Label.Create;
  Self.Label9 := System.Windows.Forms.Label.Create;
  Self.Label7 := System.Windows.Forms.Label.Create;
  Self.btnPortraitAdd := System.Windows.Forms.Button.Create;
  Self.btnPortraitDelete := System.Windows.Forms.Button.Create;
  Self.EditFamily := System.Windows.Forms.TextBox.Create;
  Self.EditName := System.Windows.Forms.TextBox.Create;
  Self.EditPatronymic := System.Windows.Forms.TextBox.Create;
  Self.EditSex := System.Windows.Forms.ComboBox.Create;
  Self.CheckPatriarch := System.Windows.Forms.CheckBox.Create;
  Self.PageCtlParents := System.Windows.Forms.Panel.Create;
  Self.EditMother := System.Windows.Forms.TextBox.Create;
  Self.Label12 := System.Windows.Forms.Label.Create;
  Self.btnParentsAdd := System.Windows.Forms.Button.Create;
  Self.btnParentsEdit := System.Windows.Forms.Button.Create;
  Self.btnParentsDelete := System.Windows.Forms.Button.Create;
  Self.btnFatherAdd := System.Windows.Forms.Button.Create;
  Self.btnFatherDelete := System.Windows.Forms.Button.Create;
  Self.btnFatherSel := System.Windows.Forms.Button.Create;
  Self.btnMotherAdd := System.Windows.Forms.Button.Create;
  Self.btnMotherDelete := System.Windows.Forms.Button.Create;
  Self.btnMotherSel := System.Windows.Forms.Button.Create;
  Self.EditFather := System.Windows.Forms.TextBox.Create;
  Self.chkBookmark := System.Windows.Forms.CheckBox.Create;
  Self.edPieceSurnamePrefix := System.Windows.Forms.TextBox.Create;
  Self.edPiecePrefix := System.Windows.Forms.TextBox.Create;
  Self.edPieceSuffix := System.Windows.Forms.TextBox.Create;
  Self.edPieceNickname := System.Windows.Forms.TextBox.Create;
  Self.panPortrait := System.Windows.Forms.Panel.Create;
  Self.imgPortrait := System.Windows.Forms.PictureBox.Create;
  Self.btnNameCopy := System.Windows.Forms.Button.Create;
  Self.PagesPersonData.SuspendLayout;
  Self.GroupBox1.SuspendLayout;
  Self.PageCtlParents.SuspendLayout;
  Self.panPortrait.SuspendLayout;
  Self.SuspendLayout;
  // 
  // PagesPersonData
  // 
  Self.PagesPersonData.Controls.Add(Self.SheetEvents);
  Self.PagesPersonData.Controls.Add(Self.SheetSpouses);
  Self.PagesPersonData.Controls.Add(Self.SheetAssociations);
  Self.PagesPersonData.Controls.Add(Self.SheetGroups);
  Self.PagesPersonData.Controls.Add(Self.SheetNotes);
  Self.PagesPersonData.Controls.Add(Self.SheetMultimedia);
  Self.PagesPersonData.Controls.Add(Self.SheetSources);
  Self.PagesPersonData.Controls.Add(Self.SheetUserRefs);
  Self.PagesPersonData.Location := System.Drawing.Point.Create(0, 265);
  Self.PagesPersonData.Name := 'PagesPersonData';
  Self.PagesPersonData.SelectedIndex := 0;
  Self.PagesPersonData.Size := System.Drawing.Size.Create(625, 264);
  Self.PagesPersonData.TabIndex := 1;
  // 
  // SheetEvents
  // 
  Self.SheetEvents.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetEvents.Name := 'SheetEvents';
  Self.SheetEvents.Size := System.Drawing.Size.Create(617, 238);
  Self.SheetEvents.TabIndex := 0;
  Self.SheetEvents.Text := 'Факты';
  // 
  // SheetSpouses
  // 
  Self.SheetSpouses.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetSpouses.Name := 'SheetSpouses';
  Self.SheetSpouses.Size := System.Drawing.Size.Create(617, 238);
  Self.SheetSpouses.TabIndex := 1;
  Self.SheetSpouses.Text := 'Супруги';
  // 
  // SheetAssociations
  // 
  Self.SheetAssociations.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetAssociations.Name := 'SheetAssociations';
  Self.SheetAssociations.Size := System.Drawing.Size.Create(617, 238);
  Self.SheetAssociations.TabIndex := 2;
  Self.SheetAssociations.Text := 'Ассоциации';
  // 
  // SheetGroups
  // 
  Self.SheetGroups.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetGroups.Name := 'SheetGroups';
  Self.SheetGroups.Size := System.Drawing.Size.Create(617, 238);
  Self.SheetGroups.TabIndex := 3;
  Self.SheetGroups.Text := 'Группы';
  // 
  // SheetNotes
  // 
  Self.SheetNotes.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetNotes.Name := 'SheetNotes';
  Self.SheetNotes.Size := System.Drawing.Size.Create(617, 238);
  Self.SheetNotes.TabIndex := 4;
  Self.SheetNotes.Text := 'Заметки';
  // 
  // SheetMultimedia
  // 
  Self.SheetMultimedia.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetMultimedia.Name := 'SheetMultimedia';
  Self.SheetMultimedia.Size := System.Drawing.Size.Create(617, 238);
  Self.SheetMultimedia.TabIndex := 5;
  Self.SheetMultimedia.Text := 'Мультимедиа';
  // 
  // SheetSources
  // 
  Self.SheetSources.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetSources.Name := 'SheetSources';
  Self.SheetSources.Size := System.Drawing.Size.Create(617, 238);
  Self.SheetSources.TabIndex := 6;
  Self.SheetSources.Text := 'Источники';
  // 
  // SheetUserRefs
  // 
  Self.SheetUserRefs.Location := System.Drawing.Point.Create(4, 22);
  Self.SheetUserRefs.Name := 'SheetUserRefs';
  Self.SheetUserRefs.Size := System.Drawing.Size.Create(617, 238);
  Self.SheetUserRefs.TabIndex := 7;
  Self.SheetUserRefs.Text := 'Сноски/Пометки';
  // 
  // btnAccept
  // 
  Self.btnAccept.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnAccept.Location := System.Drawing.Point.Create(448, 544);
  Self.btnAccept.Name := 'btnAccept';
  Self.btnAccept.Size := System.Drawing.Size.Create(81, 25);
  Self.btnAccept.TabIndex := 4;
  Self.btnAccept.Text := 'Принять';
  Self.btnAccept.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  Include(Self.btnAccept.Click, Self.btnAccept_Click);
  // 
  // btnCancel
  // 
  Self.btnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnCancel.ImageAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.btnCancel.Location := System.Drawing.Point.Create(536, 544);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(81, 25);
  Self.btnCancel.TabIndex := 3;
  Self.btnCancel.Text := 'Отменить';
  Self.btnCancel.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  // 
  // Label5
  // 
  Self.Label5.Location := System.Drawing.Point.Create(8, 552);
  Self.Label5.Name := 'Label5';
  Self.Label5.Size := System.Drawing.Size.Create(150, 13);
  Self.Label5.TabIndex := 0;
  Self.Label5.Text := 'Ограничение безопасности';
  // 
  // cbRestriction
  // 
  Self.cbRestriction.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.cbRestriction.Location := System.Drawing.Point.Create(160, 544);
  Self.cbRestriction.Name := 'cbRestriction';
  Self.cbRestriction.Size := System.Drawing.Size.Create(145, 21);
  Self.cbRestriction.TabIndex := 2;
  Include(Self.cbRestriction.SelectedIndexChanged, Self.cbRestriction_SelectedIndexChanged);
  // 
  // GroupBox1
  // 
  Self.GroupBox1.Controls.Add(Self.Label1);
  Self.GroupBox1.Controls.Add(Self.Label2);
  Self.GroupBox1.Controls.Add(Self.Label3);
  Self.GroupBox1.Controls.Add(Self.Label4);
  Self.GroupBox1.Controls.Add(Self.Label8);
  Self.GroupBox1.Controls.Add(Self.Label6);
  Self.GroupBox1.Controls.Add(Self.Label9);
  Self.GroupBox1.Controls.Add(Self.Label7);
  Self.GroupBox1.Controls.Add(Self.btnPortraitAdd);
  Self.GroupBox1.Controls.Add(Self.btnPortraitDelete);
  Self.GroupBox1.Controls.Add(Self.EditFamily);
  Self.GroupBox1.Controls.Add(Self.EditName);
  Self.GroupBox1.Controls.Add(Self.EditPatronymic);
  Self.GroupBox1.Controls.Add(Self.EditSex);
  Self.GroupBox1.Controls.Add(Self.CheckPatriarch);
  Self.GroupBox1.Controls.Add(Self.PageCtlParents);
  Self.GroupBox1.Controls.Add(Self.chkBookmark);
  Self.GroupBox1.Controls.Add(Self.edPieceSurnamePrefix);
  Self.GroupBox1.Controls.Add(Self.edPiecePrefix);
  Self.GroupBox1.Controls.Add(Self.edPieceSuffix);
  Self.GroupBox1.Controls.Add(Self.edPieceNickname);
  Self.GroupBox1.Controls.Add(Self.panPortrait);
  Self.GroupBox1.Location := System.Drawing.Point.Create(0, 0);
  Self.GroupBox1.Name := 'GroupBox1';
  Self.GroupBox1.Size := System.Drawing.Size.Create(625, 265);
  Self.GroupBox1.TabIndex := 0;
  Self.GroupBox1.TabStop := False;
  // 
  // Label1
  // 
  Self.Label1.Location := System.Drawing.Point.Create(8, 16);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(55, 13);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'Фамилия';
  // 
  // Label2
  // 
  Self.Label2.Location := System.Drawing.Point.Create(8, 56);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(25, 13);
  Self.Label2.TabIndex := 1;
  Self.Label2.Text := 'Имя';
  // 
  // Label3
  // 
  Self.Label3.Location := System.Drawing.Point.Create(8, 96);
  Self.Label3.Name := 'Label3';
  Self.Label3.Size := System.Drawing.Size.Create(55, 13);
  Self.Label3.TabIndex := 2;
  Self.Label3.Text := 'Отчество';
  // 
  // Label4
  // 
  Self.Label4.Location := System.Drawing.Point.Create(184, 136);
  Self.Label4.Name := 'Label4';
  Self.Label4.Size := System.Drawing.Size.Create(25, 13);
  Self.Label4.TabIndex := 3;
  Self.Label4.Text := 'Пол';
  // 
  // Label8
  // 
  Self.Label8.Location := System.Drawing.Point.Create(184, 16);
  Self.Label8.Name := 'Label8';
  Self.Label8.Size := System.Drawing.Size.Create(105, 13);
  Self.Label8.TabIndex := 4;
  Self.Label8.Text := 'Префикс фамилии';
  // 
  // Label6
  // 
  Self.Label6.Location := System.Drawing.Point.Create(184, 56);
  Self.Label6.Name := 'Label6';
  Self.Label6.Size := System.Drawing.Size.Create(105, 13);
  Self.Label6.TabIndex := 5;
  Self.Label6.Text := 'Префикс имени';
  // 
  // Label9
  // 
  Self.Label9.Location := System.Drawing.Point.Create(184, 96);
  Self.Label9.Name := 'Label9';
  Self.Label9.Size := System.Drawing.Size.Create(105, 13);
  Self.Label9.TabIndex := 6;
  Self.Label9.Text := 'Суффикс имени';
  // 
  // Label7
  // 
  Self.Label7.Location := System.Drawing.Point.Create(8, 136);
  Self.Label7.Name := 'Label7';
  Self.Label7.Size := System.Drawing.Size.Create(60, 13);
  Self.Label7.TabIndex := 7;
  Self.Label7.Text := 'Прозвище';
  // 
  // btnPortraitAdd
  // 
  Self.btnPortraitAdd.AccessibleDescription := 'Присоединить портрет';
  Self.btnPortraitAdd.Location := System.Drawing.Point.Create(552, 152);
  Self.btnPortraitAdd.Name := 'btnPortraitAdd';
  Self.btnPortraitAdd.Size := System.Drawing.Size.Create(26, 26);
  Self.btnPortraitAdd.TabIndex := 8;
  Include(Self.btnPortraitAdd.Click, Self.btnPortraitAdd_Click);
  // 
  // btnPortraitDelete
  // 
  Self.btnPortraitDelete.AccessibleDescription := 'Отсоединить портрет';
  Self.btnPortraitDelete.Location := System.Drawing.Point.Create(584, 152);
  Self.btnPortraitDelete.Name := 'btnPortraitDelete';
  Self.btnPortraitDelete.Size := System.Drawing.Size.Create(26, 26);
  Self.btnPortraitDelete.TabIndex := 9;
  Include(Self.btnPortraitDelete.Click, Self.btnPortraitDelete_Click);
  // 
  // EditFamily
  // 
  Self.EditFamily.Location := System.Drawing.Point.Create(8, 32);
  Self.EditFamily.Name := 'EditFamily';
  Self.EditFamily.Size := System.Drawing.Size.Create(161, 21);
  Self.EditFamily.TabIndex := 0;
  Self.EditFamily.Text := '';
  Include(Self.EditFamily.KeyPress, Self.EditFamily_KeyPress);
  Include(Self.EditFamily.TextChanged, Self.EditFamily_TextChanged);
  // 
  // EditName
  // 
  Self.EditName.Location := System.Drawing.Point.Create(8, 72);
  Self.EditName.Name := 'EditName';
  Self.EditName.Size := System.Drawing.Size.Create(161, 21);
  Self.EditName.TabIndex := 1;
  Self.EditName.Text := '';
  Include(Self.EditName.KeyPress, Self.EditFamily_KeyPress);
  Include(Self.EditName.TextChanged, Self.EditName_TextChanged);
  // 
  // EditPatronymic
  // 
  Self.EditPatronymic.Location := System.Drawing.Point.Create(8, 112);
  Self.EditPatronymic.Name := 'EditPatronymic';
  Self.EditPatronymic.Size := System.Drawing.Size.Create(161, 21);
  Self.EditPatronymic.TabIndex := 2;
  Self.EditPatronymic.Text := '';
  Include(Self.EditPatronymic.KeyPress, Self.EditFamily_KeyPress);
  Include(Self.EditPatronymic.TextChanged, Self.EditPatronymic_TextChanged);
  // 
  // EditSex
  // 
  Self.EditSex.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.EditSex.Location := System.Drawing.Point.Create(184, 152);
  Self.EditSex.Name := 'EditSex';
  Self.EditSex.TabIndex := 7;
  // 
  // CheckPatriarch
  // 
  Self.CheckPatriarch.Location := System.Drawing.Point.Create(320, 144);
  Self.CheckPatriarch.Name := 'CheckPatriarch';
  Self.CheckPatriarch.Size := System.Drawing.Size.Create(153, 17);
  Self.CheckPatriarch.TabIndex := 8;
  Self.CheckPatriarch.Text := 'Патриарх (глава семьи)';
  // 
  // PageCtlParents
  // 
  Self.PageCtlParents.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.PageCtlParents.Controls.Add(Self.EditMother);
  Self.PageCtlParents.Controls.Add(Self.Label12);
  Self.PageCtlParents.Controls.Add(Self.btnParentsAdd);
  Self.PageCtlParents.Controls.Add(Self.btnParentsEdit);
  Self.PageCtlParents.Controls.Add(Self.btnParentsDelete);
  Self.PageCtlParents.Controls.Add(Self.btnFatherAdd);
  Self.PageCtlParents.Controls.Add(Self.btnFatherDelete);
  Self.PageCtlParents.Controls.Add(Self.btnFatherSel);
  Self.PageCtlParents.Controls.Add(Self.btnMotherAdd);
  Self.PageCtlParents.Controls.Add(Self.btnMotherDelete);
  Self.PageCtlParents.Controls.Add(Self.btnMotherSel);
  Self.PageCtlParents.Controls.Add(Self.EditFather);
  Self.PageCtlParents.Location := System.Drawing.Point.Create(2, 192);
  Self.PageCtlParents.Name := 'PageCtlParents';
  Self.PageCtlParents.Size := System.Drawing.Size.Create(621, 71);
  Self.PageCtlParents.TabIndex := 10;
  // 
  // EditMother
  // 
  Self.EditMother.ForeColor := System.Drawing.SystemColors.Control;
  Self.EditMother.Location := System.Drawing.Point.Create(296, 8);
  Self.EditMother.Name := 'EditMother';
  Self.EditMother.ReadOnly := True;
  Self.EditMother.Size := System.Drawing.Size.Create(224, 21);
  Self.EditMother.TabIndex := 10;
  Self.EditMother.Text := '';
  // 
  // Label12
  // 
  Self.Label12.Location := System.Drawing.Point.Create(8, 16);
  Self.Label12.Name := 'Label12';
  Self.Label12.Size := System.Drawing.Size.Create(55, 13);
  Self.Label12.TabIndex := 0;
  Self.Label12.Text := 'Родители';
  // 
  // btnParentsAdd
  // 
  Self.btnParentsAdd.AccessibleDescription := 'Присоединить семью родителей';
  Self.btnParentsAdd.Location := System.Drawing.Point.Create(525, 5);
  Self.btnParentsAdd.Name := 'btnParentsAdd';
  Self.btnParentsAdd.Size := System.Drawing.Size.Create(26, 26);
  Self.btnParentsAdd.TabIndex := 1;
  Include(Self.btnParentsAdd.Click, Self.btnParentsAdd_Click);
  // 
  // btnParentsEdit
  // 
  Self.btnParentsEdit.AccessibleDescription := 'Правка семьи родителей';
  Self.btnParentsEdit.Location := System.Drawing.Point.Create(554, 5);
  Self.btnParentsEdit.Name := 'btnParentsEdit';
  Self.btnParentsEdit.Size := System.Drawing.Size.Create(26, 26);
  Self.btnParentsEdit.TabIndex := 2;
  Include(Self.btnParentsEdit.Click, Self.btnParentsEdit_Click);
  // 
  // btnParentsDelete
  // 
  Self.btnParentsDelete.AccessibleDescription := 'Отсоединить семью родителе' +
  'й';
  Self.btnParentsDelete.Location := System.Drawing.Point.Create(583, 5);
  Self.btnParentsDelete.Name := 'btnParentsDelete';
  Self.btnParentsDelete.Size := System.Drawing.Size.Create(26, 26);
  Self.btnParentsDelete.TabIndex := 3;
  Include(Self.btnParentsDelete.Click, Self.btnParentsDelete_Click);
  // 
  // btnFatherAdd
  // 
  Self.btnFatherAdd.AccessibleDescription := 'Выбрать или добавить отца';
  Self.btnFatherAdd.Location := System.Drawing.Point.Create(198, 35);
  Self.btnFatherAdd.Name := 'btnFatherAdd';
  Self.btnFatherAdd.Size := System.Drawing.Size.Create(26, 26);
  Self.btnFatherAdd.TabIndex := 4;
  Include(Self.btnFatherAdd.Click, Self.btnFatherAdd_Click);
  // 
  // btnFatherDelete
  // 
  Self.btnFatherDelete.AccessibleDescription := 'Отсоединить отца';
  Self.btnFatherDelete.Location := System.Drawing.Point.Create(230, 35);
  Self.btnFatherDelete.Name := 'btnFatherDelete';
  Self.btnFatherDelete.Size := System.Drawing.Size.Create(26, 26);
  Self.btnFatherDelete.TabIndex := 5;
  Include(Self.btnFatherDelete.Click, Self.btnFatherDelete_Click);
  // 
  // btnFatherSel
  // 
  Self.btnFatherSel.AccessibleDescription := 'Перейти на запись отца';
  Self.btnFatherSel.Location := System.Drawing.Point.Create(262, 35);
  Self.btnFatherSel.Name := 'btnFatherSel';
  Self.btnFatherSel.Size := System.Drawing.Size.Create(26, 26);
  Self.btnFatherSel.TabIndex := 6;
  Include(Self.btnFatherSel.Click, Self.btnFatherSel_Click);
  // 
  // btnMotherAdd
  // 
  Self.btnMotherAdd.AccessibleDescription := 'Выбрать или добавить мать';
  Self.btnMotherAdd.Location := System.Drawing.Point.Create(430, 35);
  Self.btnMotherAdd.Name := 'btnMotherAdd';
  Self.btnMotherAdd.Size := System.Drawing.Size.Create(26, 26);
  Self.btnMotherAdd.TabIndex := 7;
  Include(Self.btnMotherAdd.Click, Self.btnMotherAdd_Click);
  // 
  // btnMotherDelete
  // 
  Self.btnMotherDelete.AccessibleDescription := 'Отсоединить мать';
  Self.btnMotherDelete.Location := System.Drawing.Point.Create(462, 35);
  Self.btnMotherDelete.Name := 'btnMotherDelete';
  Self.btnMotherDelete.Size := System.Drawing.Size.Create(26, 26);
  Self.btnMotherDelete.TabIndex := 8;
  Include(Self.btnMotherDelete.Click, Self.btnMotherDelete_Click);
  // 
  // btnMotherSel
  // 
  Self.btnMotherSel.AccessibleDescription := 'Перейти на запись матери';
  Self.btnMotherSel.Location := System.Drawing.Point.Create(494, 35);
  Self.btnMotherSel.Name := 'btnMotherSel';
  Self.btnMotherSel.Size := System.Drawing.Size.Create(26, 26);
  Self.btnMotherSel.TabIndex := 9;
  Include(Self.btnMotherSel.Click, Self.btnMotherSel_Click);
  // 
  // EditFather
  // 
  Self.EditFather.ForeColor := System.Drawing.SystemColors.Control;
  Self.EditFather.Location := System.Drawing.Point.Create(64, 8);
  Self.EditFather.Name := 'EditFather';
  Self.EditFather.ReadOnly := True;
  Self.EditFather.Size := System.Drawing.Size.Create(224, 21);
  Self.EditFather.TabIndex := 0;
  Self.EditFather.Text := '';
  // 
  // chkBookmark
  // 
  Self.chkBookmark.Location := System.Drawing.Point.Create(320, 161);
  Self.chkBookmark.Name := 'chkBookmark';
  Self.chkBookmark.Size := System.Drawing.Size.Create(153, 17);
  Self.chkBookmark.TabIndex := 9;
  Self.chkBookmark.Text := 'Закладка';
  // 
  // edPieceSurnamePrefix
  // 
  Self.edPieceSurnamePrefix.Location := System.Drawing.Point.Create(184, 32);
  Self.edPieceSurnamePrefix.Name := 'edPieceSurnamePrefix';
  Self.edPieceSurnamePrefix.Size := System.Drawing.Size.Create(121, 21);
  Self.edPieceSurnamePrefix.TabIndex := 4;
  Self.edPieceSurnamePrefix.Text := '';
  // 
  // edPiecePrefix
  // 
  Self.edPiecePrefix.Location := System.Drawing.Point.Create(184, 72);
  Self.edPiecePrefix.Name := 'edPiecePrefix';
  Self.edPiecePrefix.Size := System.Drawing.Size.Create(121, 21);
  Self.edPiecePrefix.TabIndex := 5;
  Self.edPiecePrefix.Text := '';
  // 
  // edPieceSuffix
  // 
  Self.edPieceSuffix.Location := System.Drawing.Point.Create(184, 112);
  Self.edPieceSuffix.Name := 'edPieceSuffix';
  Self.edPieceSuffix.Size := System.Drawing.Size.Create(121, 21);
  Self.edPieceSuffix.TabIndex := 6;
  Self.edPieceSuffix.Text := '';
  // 
  // edPieceNickname
  // 
  Self.edPieceNickname.Location := System.Drawing.Point.Create(8, 152);
  Self.edPieceNickname.Name := 'edPieceNickname';
  Self.edPieceNickname.Size := System.Drawing.Size.Create(161, 21);
  Self.edPieceNickname.TabIndex := 3;
  Self.edPieceNickname.Text := '';
  // 
  // panPortrait
  // 
  Self.panPortrait.Controls.Add(Self.imgPortrait);
  Self.panPortrait.Location := System.Drawing.Point.Create(480, 16);
  Self.panPortrait.Name := 'panPortrait';
  Self.panPortrait.Size := System.Drawing.Size.Create(130, 130);
  Self.panPortrait.TabIndex := 11;
  // 
  // imgPortrait
  // 
  Self.imgPortrait.Location := System.Drawing.Point.Create(1, 1);
  Self.imgPortrait.Name := 'imgPortrait';
  Self.imgPortrait.Size := System.Drawing.Size.Create(128, 128);
  Self.imgPortrait.TabIndex := 0;
  Self.imgPortrait.TabStop := False;
  // 
  // btnNameCopy
  // 
  Self.btnNameCopy.AccessibleDescription := 'Скопировать имя в буфер обмена';
  Self.btnNameCopy.Location := System.Drawing.Point.Create(384, 544);
  Self.btnNameCopy.Name := 'btnNameCopy';
  Self.btnNameCopy.Size := System.Drawing.Size.Create(33, 25);
  Self.btnNameCopy.TabIndex := 1;
  Include(Self.btnNameCopy.Click, Self.btnNameCopy1_Click);
  // 
  // TfmPersonEdit
  // 
  Self.AcceptButton := Self.btnAccept;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.CancelButton := Self.btnCancel;
  Self.ClientSize := System.Drawing.Size.Create(625, 577);
  Self.Controls.Add(Self.Label5);
  Self.Controls.Add(Self.btnNameCopy);
  Self.Controls.Add(Self.PagesPersonData);
  Self.Controls.Add(Self.btnAccept);
  Self.Controls.Add(Self.btnCancel);
  Self.Controls.Add(Self.cbRestriction);
  Self.Controls.Add(Self.GroupBox1);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular, 
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TfmPersonEdit';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Редактирование персональной информации';
  Self.PagesPersonData.ResumeLayout(False);
  Self.GroupBox1.ResumeLayout(False);
  Self.PageCtlParents.ResumeLayout(False);
  Self.panPortrait.ResumeLayout(False);
  Self.ResumeLayout(False);
end;

constructor TfmPersonEdit.Create(aBase: TfmBase);
var
  sx: TGEDCOMObject.TGEDCOMSex;
  res: TGEDCOMObject.TGEDCOMRestriction;
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;

  for res := Low(TGEDCOMObject.TGEDCOMRestriction) to High(TGEDCOMObject.TGEDCOMRestriction) do
    cbRestriction.Items.Add(TGenEngine.Restrictions[res]);

  for sx := Low(TGEDCOMObject.TGEDCOMSex) to High(TGEDCOMObject.TGEDCOMSex) do
    EditSex.Items.Add(TGenEngine.SexStr(sx));

  FEventsList := TSheetList.Create(SheetEvents);
  FEventsList.OnModify := ListModify;
  FEventsList.Buttons := TEnumSet.Create([lbAdd, lbEdit, lbDelete, lbMoveUp, lbMoveDown]);
  Base.SetupRecEventsList(FEventsList, True);

  //

  FSpousesList := TSheetList.Create(SheetSpouses);
  FSpousesList.OnModify := ListModify;
  FSpousesList.Buttons := TEnumSet.Create([lbAdd, lbEdit, lbDelete, lbJump, lbMoveUp, lbMoveDown]);
  FSpousesList.List.AddListColumn('№', 25);
  FSpousesList.List.AddListColumn(LSList[LSID_Spouse], 300);
  FSpousesList.List.AddListColumn(LSList[LSID_MarriageDate], 100);

  FAssociationsList := TSheetList.Create(SheetAssociations);
  FAssociationsList.OnModify := ListModify;
  FAssociationsList.Buttons := TEnumSet.Create([lbAdd, lbEdit, lbDelete, lbJump]);
  FAssociationsList.List.AddListColumn(LSList[LSID_Relation], 300);
  FAssociationsList.List.AddListColumn(LSList[LSID_Person], 200);

  FGroupsList := TSheetList.Create(SheetGroups);
  FGroupsList.OnModify := ListModify;
  FGroupsList.Buttons := TEnumSet.Create([lbAdd, lbDelete]);
  FGroupsList.List.AddListColumn(LSList[LSID_Group], 350);

  //

  FNotesList := TSheetList.Create(SheetNotes);
  FNotesList.OnModify := ListModify;
  Base.SetupRecNotesList(FNotesList);

  FMediaList := TSheetList.Create(SheetMultimedia);
  FMediaList.OnModify := ListModify;
  FMediaList.Buttons := TEnumSet.Create([lbAdd, lbEdit, lbDelete, lbMoveUp, lbMoveDown]);
  Base.SetupRecMediaList(FMediaList);

  FSourcesList := TSheetList.Create(SheetSources);
  FSourcesList.OnModify := ListModify;
  FSourcesList.Buttons.Include([lbMoveUp, lbMoveDown]);
  Base.SetupRecSourcesList(FSourcesList);

  FUserRefList := TSheetList.Create(SheetUserRefs);
  FUserRefList.OnModify := ListModify;
  FUserRefList.List.AddListColumn(LSList[LSID_Reference], 300);
  FUserRefList.List.AddListColumn(LSList[LSID_Type], 200);

  btnPortraitAdd.ImageList := fmGEDKeeper.ImageList_Buttons;
  btnPortraitAdd.ImageIndex := 3;
  btnPortraitDelete.ImageList := fmGEDKeeper.ImageList_Buttons;
  btnPortraitDelete.ImageIndex := 5;

  btnFatherAdd.ImageList := fmGEDKeeper.ImageList_Buttons;
  btnFatherAdd.ImageIndex := 3;
  btnFatherDelete.ImageList := fmGEDKeeper.ImageList_Buttons;
  btnFatherDelete.ImageIndex := 5;
  btnFatherSel.ImageList := fmGEDKeeper.ImageList_Buttons;
  btnFatherSel.ImageIndex := 28;

  btnMotherAdd.ImageList := fmGEDKeeper.ImageList_Buttons;
  btnMotherAdd.ImageIndex := 3;
  btnMotherDelete.ImageList := fmGEDKeeper.ImageList_Buttons;
  btnMotherDelete.ImageIndex := 5;
  btnMotherSel.ImageList := fmGEDKeeper.ImageList_Buttons;
  btnMotherSel.ImageIndex := 28;

  btnParentsAdd.ImageList := fmGEDKeeper.ImageList_Buttons;
  btnParentsAdd.ImageIndex := 3;
  btnParentsEdit.ImageList := fmGEDKeeper.ImageList_Buttons;
  btnParentsEdit.ImageIndex := 4;
  btnParentsDelete.ImageList := fmGEDKeeper.ImageList_Buttons;
  btnParentsDelete.ImageIndex := 5;

  SetLang();
end;

procedure TfmPersonEdit.SetLang();
begin
  btnAccept.Text := LSList[LSID_DlgAccept];
  btnCancel.Text := LSList[LSID_DlgCancel];

  Text := LSList[LSID_WinPersonEdit];

  Label1.Text := LSList[LSID_Surname];
  Label2.Text := LSList[LSID_Name];
  Label3.Text := LSList[LSID_Patronymic];
  Label4.Text := LSList[LSID_Sex];

  Label7.Text := LSList[LSID_Nickname];
  Label8.Text := LSList[LSID_SurnamePrefix];
  Label6.Text := LSList[LSID_NamePrefix];
  Label9.Text := LSList[LSID_NameSuffix];

  CheckPatriarch.Text := LSList[LSID_Patriarch];
  chkBookmark.Text := LSList[LSID_Bookmark];

  Label12.Text := LSList[LSID_Parents];

  SheetEvents.Text := LSList[LSID_Events];
  SheetSpouses.Text := LSList[LSID_Spouses];
  SheetAssociations.Text := LSList[LSID_Associations];
  SheetGroups.Text := LSList[LSID_RPGroups];
  SheetNotes.Text := LSList[LSID_RPNotes];
  SheetMultimedia.Text := LSList[LSID_RPMultimedia];
  SheetSources.Text := LSList[LSID_RPSources];
  SheetUserRefs.Text := LSList[LSID_UserRefs];

  Label5.Text := LSList[LSID_Restriction];
end;

procedure TfmPersonEdit.SetTitle();
begin
  Text := LSList[LSID_Person] + ' "'+EditFamily.Text+' '+EditName.Text+' '+EditPatronymic.Text+'" [' + TGenEngine.GetId(FPerson).ToString() + ']';
end;

procedure TfmPersonEdit.ControlsRefresh();

  procedure LockEditor(aLocked: Boolean);
  begin
    {if aLocked
    then EditFamily.Color := clInactiveBorder
    else EditFamily.Color := clWindow;
    EditFamily.ReadOnly := aLocked;

    if aLocked
    then EditName.Color := clInactiveBorder
    else EditName.Color := clWindow;
    EditName.ReadOnly := aLocked;

    if aLocked
    then EditPatronymic.Color := clInactiveBorder
    else EditPatronymic.Color := clWindow;
    EditPatronymic.ReadOnly := aLocked;

    if aLocked
    then EditSex.Color := clInactiveBorder
    else EditSex.Color := clWindow;
    EditSex.Enabled := not(aLocked);

    CheckPatriarch.Enabled := not(aLocked);
    chkBookmark.Enabled := not(aLocked);

    if aLocked then begin
      edPieceSurnamePrefix.Color := clInactiveBorder;
      edPiecePrefix.Color := clInactiveBorder;
      edPieceSuffix.Color := clInactiveBorder;
      edPieceNickname.Color := clInactiveBorder;
    end else begin
      edPieceSurnamePrefix.Color := clWindow;
      edPiecePrefix.Color := clWindow;
      edPieceSuffix.Color := clWindow;
      edPieceNickname.Color := clWindow;
    end;

    btnFatherAdd.Enabled := btnFatherAdd.Enabled and not(aLocked);
    btnFatherDelete.Enabled := btnFatherDelete.Enabled and not(aLocked);

    btnMotherAdd.Enabled := btnMotherAdd.Enabled and not(aLocked);
    btnMotherDelete.Enabled := btnMotherDelete.Enabled and not(aLocked);

    FEventsList.ReadOnly := (aLocked);
    FSpousesList.ReadOnly := (aLocked);
    FAssociationsList.ReadOnly := (aLocked);
    FGroupsList.ReadOnly := (aLocked);
    FNotesList.ReadOnly := (aLocked);
    FMediaList.ReadOnly := (aLocked);
    FSourcesList.ReadOnly := (aLocked);
    FUserRefList.ReadOnly := (aLocked);}
  end;

var
  rel_name: string;
  family: TGEDCOMFamilyRecord;
  rel_person: TGEDCOMIndividualRecord;
  idx: Integer;
  item: ListViewItem;
  np: TGEDCOMPersonalName;
  uref: TGEDCOMUserReference;
begin
  if (FPerson.PersonalNamesCount > 0) then begin
    np := FPerson.PersonalNames[0];
    edPiecePrefix.Text := np.Pieces.Prefix;
    edPieceNickname.Text := np.Pieces.Nickname;
    edPieceSurnamePrefix.Text := np.Pieces.SurnamePrefix;
    edPieceSuffix.Text := np.Pieces.Suffix;
  end;

  if (FPerson.ChildToFamilyLinksCount <> 0) then begin
    family := FPerson.ChildToFamilyLinks[0].Family;

    btnParentsAdd.Enabled := False;
    btnParentsEdit.Enabled := True;
    btnParentsDelete.Enabled := True;

    rel_person := TGEDCOMIndividualRecord(family.Husband.Value);
    if (rel_person <> nil) then begin
      btnFatherAdd.Enabled := False;
      btnFatherDelete.Enabled := True;
      btnFatherSel.Enabled := True;

      EditFather.Text := TGenEngine.GetNameStr(rel_person);
    end else begin
      btnFatherAdd.Enabled := True;
      btnFatherDelete.Enabled := False;
      btnFatherSel.Enabled := False;

      EditFather.Text := '';
    end;

    rel_person := TGEDCOMIndividualRecord(family.Wife.Value);
    if (rel_person <> nil) then begin
      btnMotherAdd.Enabled := False;
      btnMotherDelete.Enabled := True;
      btnMotherSel.Enabled := True;

      EditMother.Text := TGenEngine.GetNameStr(rel_person);
    end else begin
      btnMotherAdd.Enabled := True;
      btnMotherDelete.Enabled := False;
      btnMotherSel.Enabled := False;

      EditMother.Text := '';
    end;
  end else begin
    btnParentsAdd.Enabled := True;
    btnParentsEdit.Enabled := False;
    btnParentsDelete.Enabled := False;

    btnFatherAdd.Enabled := True;
    btnFatherDelete.Enabled := False;
    btnFatherSel.Enabled := False;

    btnMotherAdd.Enabled := True;
    btnMotherDelete.Enabled := False;
    btnMotherSel.Enabled := False;

    EditFather.Text := '';
    EditMother.Text := '';
  end;

  Base.RecListIndividualEventsRefresh(FPerson, FEventsList.List, nil);
  Base.RecListNotesRefresh(FPerson, FNotesList.List, nil);
  Base.RecListMediaRefresh(FPerson, FMediaList.List, nil);
  Base.RecListSourcesRefresh(FPerson, FSourcesList.List, nil);

  FSpousesList.List.Items.Clear();
  for idx := 1 to FPerson.SpouseToFamilyLinksCount do begin
    family := FPerson.SpouseToFamilyLinks[idx - 1].Family;
    if (family = nil) then Continue;

    if (FPerson.Sex = svMale) then begin
      rel_person := TGEDCOMIndividualRecord(family.Wife.Value);
      rel_name := LSList[LSID_UnkFemale];
    end else begin
      rel_person := TGEDCOMIndividualRecord(family.Husband.Value);
      rel_name := LSList[LSID_UnkMale];
    end;

    if (rel_person <> nil)
    then rel_name := TGenEngine.GetNameStr(rel_person);

    item := FSpousesList.List.AddItem(idx.ToString(), family);
    item.SubItems.Add(rel_name);
    item.SubItems.Add(TGenEngine.GetMarriageDate(family, fmGEDKeeper.Options.DefDateFormat));
  end;

  Base.RecListAssociationsRefresh(FPerson, FAssociationsList.List, nil);
  Base.RecListGroupsRefresh(FPerson, FGroupsList.List, nil);

  FUserRefList.List.Items.Clear();
  for idx := 0 to FPerson.UserReferencesCount - 1 do begin
    uref := FPerson.UserReferences[idx];

    item := FUserRefList.List.AddItem(uref.StringValue, uref);
    item.SubItems.Add(uref.ReferenceType);
  end;

  LockEditor((FPerson.Restriction = rnLocked));

  PortraitRefresh();
end;

procedure TfmPersonEdit.SetPerson(const Value: TGEDCOMIndividualRecord);
var
  fam, nam, pat: string;
begin
  FPerson := Value;

  try
    TGenEngine.GetNameParts(FPerson, fam, nam, pat);

    EditFamily.Text := fam;
    EditName.Text := nam;
    EditPatronymic.Text := pat;
    EditSex.SelectedIndex := Ord(FPerson.Sex);

    // extended begin
    CheckPatriarch.Checked := FPerson.Patriarch;
    chkBookmark.Checked := FPerson.Bookmark;
    // extended end

    cbRestriction.SelectedIndex := Ord(FPerson.Restriction);

    ControlsRefresh();
  except
    on E: Exception do TGKUtils.LogWrite('PersonEdit.SetPerson(): ' + E.Message);
  end;
end;

procedure TfmPersonEdit.btnFatherAdd_Click(sender: System.Object; e: System.EventArgs);
var
  father: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
begin
  father := Base.SelectPerson(FPerson, tmDescendant, svMale);
  if (father <> nil) then begin
    family := Base.GetChildFamily(FPerson, True, father);

    if (family.Husband.Value = nil)
    then Base.Engine.AddFamilySpouse(family, father);

    ControlsRefresh();
  end;
end;

procedure TfmPersonEdit.btnFatherDelete_Click(sender: System.Object; e: System.EventArgs);
var
  family: TGEDCOMFamilyRecord;
begin
  if (TGKUtils.ShowQuestion(LSList[LSID_DetachFatherQuery]) = System.Windows.Forms.DialogResult.No)
  then Exit;

  family := Base.GetChildFamily(FPerson, False, nil);
  if (family <> nil) then begin
    Base.Engine.RemoveFamilySpouse(family, TGEDCOMIndividualRecord(family.Husband.Value));
    ControlsRefresh();
  end;
end;

procedure TfmPersonEdit.btnFatherSel_Click(sender: System.Object; e: System.EventArgs);
var
  father: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
begin
  family := Base.GetChildFamily(FPerson, False, nil);
  if (family <> nil) then begin
    AcceptChanges();
    father := TGEDCOMIndividualRecord(family.Husband.Value);
    Base.SelectRecordByXRef(father.XRef);
    Close;
  end;
end;

procedure TfmPersonEdit.btnMotherAdd_Click(sender: System.Object; e: System.EventArgs);
var
  mother: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
begin
  mother := Base.SelectPerson(FPerson, tmDescendant, svFemale);
  if (mother <> nil) then begin
    family := Base.GetChildFamily(FPerson, True, mother);

    if (family.Wife.Value = nil)
    then Base.Engine.AddFamilySpouse(family, mother);

    ControlsRefresh();
  end;
end;

procedure TfmPersonEdit.btnMotherDelete_Click(sender: System.Object; e: System.EventArgs);
var
  mother: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
begin
  if (TGKUtils.ShowQuestion(LSList[LSID_DetachMotherQuery]) = System.Windows.Forms.DialogResult.No)
  then Exit;

  family := Base.GetChildFamily(FPerson, False, nil);
  if (family <> nil) then begin
    mother := TGEDCOMIndividualRecord(family.Wife.Value);
    Base.Engine.RemoveFamilySpouse(family, mother);

    ControlsRefresh();
  end;
end;

procedure TfmPersonEdit.btnMotherSel_Click(sender: System.Object; e: System.EventArgs);
var
  mother: TGEDCOMIndividualRecord;
  family: TGEDCOMFamilyRecord;
begin
  family := Base.GetChildFamily(FPerson, False, nil);
  if (family <> nil) then begin
    AcceptChanges();
    mother := TGEDCOMIndividualRecord(family.Wife.Value);
    Base.SelectRecordByXRef(mother.XRef);
    Close;
  end;
end;

procedure TfmPersonEdit.AcceptChanges();
var
  np: TGEDCOMPersonalName;
  pieces: TGEDCOMPersonalNamePieces;
begin
  np := FPerson.PersonalNames[0];
  np.SetNameParts(
    EditName.Text.Trim() + ' ' + EditPatronymic.Text.Trim(),
    EditFamily.Text.Trim(), np.LastPart);

  pieces := np.Pieces;
  if (pieces.Prefix <> edPiecePrefix.Text) then pieces.Prefix := edPiecePrefix.Text;
  if (pieces.Nickname <> edPieceNickname.Text) then pieces.Nickname := edPieceNickname.Text;
  if (pieces.SurnamePrefix <> edPieceSurnamePrefix.Text) then pieces.SurnamePrefix := edPieceSurnamePrefix.Text;
  if (pieces.Suffix <> edPieceSuffix.Text) then pieces.Suffix := edPieceSuffix.Text;

  Base.DoPersonChangeSex(FPerson, TGEDCOMObject.TGEDCOMSex(EditSex.SelectedIndex));
  Base.DoPersonChangePatriarch(FPerson, CheckPatriarch.Checked);

  FPerson.Bookmark := chkBookmark.Checked;

  FPerson.Restriction := TGEDCOMObject.TGEDCOMRestriction(cbRestriction.SelectedIndex);

  if (FPerson.ChildToFamilyLinksCount > 0) then begin
    FPerson.ChildToFamilyLinks[0].Family.SortChilds();
  end;

  Base.ChangeRecord(FPerson);
end;

procedure TfmPersonEdit.btnAccept_Click(sender: System.Object; e: System.EventArgs);
begin
  try
    AcceptChanges();
    Self.DialogResult := System.Windows.Forms.DialogResult.OK;
  except
    Self.DialogResult := System.Windows.Forms.DialogResult.None;
  end;
end;

procedure TfmPersonEdit.cbRestriction_SelectedIndexChanged(sender: System.Object;
  e: System.EventArgs);
begin
  ControlsRefresh();
end;

procedure TfmPersonEdit.EditFamily_TextChanged(sender: System.Object; e: System.EventArgs);
begin
  SetTitle();
end;

procedure TfmPersonEdit.EditName_TextChanged(sender: System.Object; e: System.EventArgs);
begin
  SetTitle();
end;

procedure TfmPersonEdit.EditPatronymic_TextChanged(sender: System.Object; e: System.EventArgs);
begin
  SetTitle();
end;

procedure TfmPersonEdit.ListModify(Sender: System.Object; ItemData: System.Object; Action: TGenEngine.TRecAction);
var
  family: TGEDCOMFamilyRecord;
  group: TGEDCOMGroupRecord;
  spouse: TGEDCOMIndividualRecord;
  src_cit: TGEDCOMSourceCitation;
  mmLink: TGEDCOMMultimediaLink;
  event: TGEDCOMCustomEvent;
  sp: TGEDCOMPointer;
  idx: Integer;
begin
  if (Sender = FEventsList) then begin
    if (Action in [raMoveUp, raMoveDown])
    then begin
      event := TGEDCOMCustomEvent(ItemData);
      idx := FPerson.IndexOfEvent(event);

      case Action of
        raMoveUp: FPerson.ExchangeEvents(idx - 1, idx);
        raMoveDown: FPerson.ExchangeEvents(idx, idx + 1);
      end;

      ControlsRefresh();
    end
    else
    if Base.ModifyRecEvent(Self, FPerson, TGEDCOMCustomEvent(ItemData), Action)
    then ControlsRefresh();
  end
  else
  //
  if (Sender = FSpousesList) then begin
    case Action of
      raAdd: begin
        family := nil;
        if Base.ModifyFamily(family, ftSpouse, FPerson)
        then ControlsRefresh();
      end;

      raEdit: begin
        family := TGEDCOMFamilyRecord(ItemData);
        if Base.ModifyFamily(family)
        then ControlsRefresh();
      end;

      raDelete: begin
        family := TGEDCOMFamilyRecord(ItemData);
        if (family = nil) then Exit;

        if (TGKUtils.ShowQuestion(LSList[LSID_DetachSpouseQuery]) = System.Windows.Forms.DialogResult.No)
        then Exit;

        Base.Engine.RemoveFamilySpouse(family, FPerson);
        ControlsRefresh();
      end;

      raJump: begin
        family := TGEDCOMFamilyRecord(ItemData);
        if (family = nil) then Exit;

        case FPerson.Sex of
          svNone, svUndetermined: Exit;
          svMale: sp := family.Wife;
          svFemale: sp := family.Husband;
        end;

        spouse := TGEDCOMIndividualRecord(sp.Value);

        AcceptChanges();
        Base.SelectRecordByXRef(spouse.XRef);
        Close;
      end;

      raMoveUp, raMoveDown: begin
        family := TGEDCOMFamilyRecord(ItemData);
        idx := FPerson.IndexOfSpouse(family);

        case Action of
          raMoveUp: FPerson.ExchangeSpouses(idx - 1, idx);
          raMoveDown: FPerson.ExchangeSpouses(idx, idx + 1);
        end;

        ControlsRefresh();
      end;
    end;
  end
  else
  if (Sender = FAssociationsList) then begin
    if Base.ModifyRecAssociation(Self, FPerson, TGEDCOMAssociation(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FGroupsList) then begin
    case Action of
      raAdd: begin
        group := TGEDCOMGroupRecord(Base.SelectRecord(rtGroup, []));
        if (group <> nil) and Base.Engine.AddGroupMember(group, FPerson)
        then ControlsRefresh();
      end;
      raEdit: ;
      raDelete: begin
        group := TGEDCOMGroupRecord(ItemData);

        if (TGKUtils.ShowQuestion(LSList[LSID_DetachGroupQuery]) = System.Windows.Forms.DialogResult.No)
        then Exit;

        if Base.Engine.RemoveGroupMember(group, FPerson)
        then ControlsRefresh();
      end;
      raJump: ;
    end;
  end
  else
  //
  if (Sender = FNotesList) then begin
    if Base.ModifyRecNote(Self, FPerson, TGEDCOMNotes(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FMediaList) then begin
    if (Action in [raMoveUp, raMoveDown])
    then begin
      mmLink := TGEDCOMMultimediaLink(ItemData);
      idx := FPerson.IndexOfMultimediaLink(mmLink);

      case Action of
        raMoveUp: FPerson.ExchangeMedia(idx - 1, idx);
        raMoveDown: FPerson.ExchangeMedia(idx, idx + 1);
      end;

      ControlsRefresh();
    end
    else
    if Base.ModifyRecMultimedia(Self, FPerson, TGEDCOMMultimediaLink(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FSourcesList) then begin
    if (Action in [raMoveUp, raMoveDown])
    then begin
      src_cit := TGEDCOMSourceCitation(ItemData);
      idx := FPerson.IndexOfSourceCitation(src_cit);

      case Action of
        raMoveUp: FPerson.ExchangeSources(idx - 1, idx);
        raMoveDown: FPerson.ExchangeSources(idx, idx + 1);
      end;

      ControlsRefresh();
    end
    else
    if Base.ModifyRecSource(Self, FPerson, TGEDCOMSourceCitation(ItemData), Action)
    then ControlsRefresh();
  end
  else
  if (Sender = FUserRefList) then begin
    if Base.ModifyRecUserRef(Self, FPerson, TGEDCOMUserReference(ItemData), Action)
    then ControlsRefresh();
  end;
end;

procedure TfmPersonEdit.btnParentsAdd_Click(sender: System.Object; e: System.EventArgs);
var
  family: TGEDCOMFamilyRecord;
begin
  family := TGEDCOMFamilyRecord(Base.SelectFamily(FPerson));
  if (family <> nil) then begin
    // в режиме выбора из списка добавления ребенка не происходит,
    // поэтому дополнительная проверка;
    // этого не делается в режиме вставки новой семьи
    // т.к. ребенок уже должен быть в списке
    // на момент появления диалога семьи, поэтому
    // соответствующая вставка делается в ModifyFamily()
    if (family.IndexOfChild(FPerson) < 0)
    then Base.Engine.AddFamilyChild(family, FPerson);

    ControlsRefresh();
  end;
end;

procedure TfmPersonEdit.btnParentsEdit_Click(sender: System.Object; e: System.EventArgs);
var
  family: TGEDCOMFamilyRecord;
begin
  family := Base.GetChildFamily(FPerson, False, nil);

  if (family <> nil) and Base.ModifyFamily(family)
  then ControlsRefresh();
end;

procedure TfmPersonEdit.btnParentsDelete_Click(sender: System.Object; e: System.EventArgs);
var
  family: TGEDCOMFamilyRecord;
begin
  if (TGKUtils.ShowQuestion(LSList[LSID_DetachParentsQuery]) = System.Windows.Forms.DialogResult.No)
  then Exit;

  family := Base.GetChildFamily(FPerson, False, nil);
  if (family <> nil) then begin
    Base.Engine.RemoveFamilyChild(family, FPerson);
    ControlsRefresh();
  end;
end;

procedure TfmPersonEdit.EditFamily_KeyPress(sender: System.Object; e: System.Windows.Forms.KeyPressEventArgs);
begin
  if (e.KeyChar = '/') then e.Handled := True;
end;

procedure TfmPersonEdit.btnNameCopy1_Click(sender: System.Object; e: System.EventArgs);
begin
  System.Windows.Forms.Clipboard.SetDataObject(TGenEngine.GetNameStr(FPerson));
end;

procedure TfmPersonEdit.btnPortraitAdd_Click(sender: System.Object; e: System.EventArgs);
var
  mmLink: TGEDCOMMultimediaLink;
  mmRec: TGEDCOMMultimediaRecord;
begin
  mmRec := TGEDCOMMultimediaRecord(Base.SelectRecord(rtMultimedia, []));

  if (mmRec <> nil) then begin
    mmLink := Base.Engine.GetPrimaryMultimediaLink(FPerson);
    if (mmLink <> nil) then mmLink.IsPrimary := False;
    Base.Engine.SetPrimaryMultimediaRecord(FPerson, mmRec);
    PortraitRefresh();
  end;
end;

procedure TfmPersonEdit.btnPortraitDelete_Click(sender: System.Object; e: System.EventArgs);
var
  mmLink: TGEDCOMMultimediaLink;
begin
  mmLink := Base.Engine.GetPrimaryMultimediaLink(FPerson);
  if (mmLink <> nil) then begin
    mmLink.IsPrimary := False;
    PortraitRefresh();
  end;
end;

procedure TfmPersonEdit.PortraitRefresh();
var
  bmp: System.Drawing.Bitmap;
begin
  bmp := Base.Engine.GetPrimaryBitmap(FPerson);
  if (bmp <> nil) then begin
    //imgPortrait.Center := True;
    //imgPortrait.Proportional := True;

    imgPortrait.Image := bmp;
    if (bmp.Width > imgPortrait.Width) or (bmp.Height > imgPortrait.Height)
    then imgPortrait.SizeMode := PictureBoxSizeMode.StretchImage;

    bmp.Free;
    imgPortrait.Visible := True;
  end else begin
    imgPortrait.Visible := False;
  end;
end;

end.
