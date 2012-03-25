using System;

namespace GKUI
{
	partial class TfmPersonEdit
	{
		private System.Windows.Forms.TabControl PagesPersonData;
		private System.Windows.Forms.TabPage SheetEvents;
		private System.Windows.Forms.TabPage SheetNotes;
		private System.Windows.Forms.TabPage SheetMultimedia;
		private System.Windows.Forms.TabPage SheetSources;
		private System.Windows.Forms.TabPage SheetSpouses;
		private System.Windows.Forms.TabPage SheetAssociations;
		private System.Windows.Forms.TabPage SheetGroups;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label Label5;
		private System.Windows.Forms.ComboBox cbRestriction;
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.Label Label3;
		private System.Windows.Forms.Label Label4;
		private System.Windows.Forms.TextBox EditFamily;
		private System.Windows.Forms.TextBox EditName;
		private System.Windows.Forms.TextBox EditPatronymic;
		private System.Windows.Forms.ComboBox EditSex;
		private System.Windows.Forms.CheckBox CheckPatriarch;
		private System.Windows.Forms.TabPage SheetUserRefs;
		private System.Windows.Forms.Panel PageCtlParents;
		private System.Windows.Forms.Label Label12;
		private System.Windows.Forms.TextBox EditFather;
		private System.Windows.Forms.Button btnParentsAdd;
		private System.Windows.Forms.Button btnParentsEdit;
		private System.Windows.Forms.Button btnParentsDelete;
		private System.Windows.Forms.CheckBox chkBookmark;
		private System.Windows.Forms.Label Label8;
		private System.Windows.Forms.TextBox edPieceSurnamePrefix;
		private System.Windows.Forms.Label Label6;
		private System.Windows.Forms.TextBox edPiecePrefix;
		private System.Windows.Forms.Label Label9;
		private System.Windows.Forms.TextBox edPieceSuffix;
		private System.Windows.Forms.Label Label7;
		private System.Windows.Forms.TextBox edPieceNickname;
		private System.Windows.Forms.Panel panPortrait;
		private System.Windows.Forms.PictureBox imgPortrait;
		private System.Windows.Forms.Button btnNameCopy;
		private System.Windows.Forms.Button btnPortraitAdd;
		private System.Windows.Forms.Button btnPortraitDelete;
		private System.Windows.Forms.Button btnFatherAdd;
		private System.Windows.Forms.Button btnFatherDelete;
		private System.Windows.Forms.Button btnFatherSel;
		private System.Windows.Forms.Button btnMotherAdd;
		private System.Windows.Forms.Button btnMotherDelete;
		private System.Windows.Forms.Button btnMotherSel;

		private void InitializeComponent()
		{
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.Label5 = new System.Windows.Forms.Label();
			this.cbRestriction = new System.Windows.Forms.ComboBox();
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.Label1 = new System.Windows.Forms.Label();
			this.Label2 = new System.Windows.Forms.Label();
			this.Label3 = new System.Windows.Forms.Label();
			this.Label4 = new System.Windows.Forms.Label();
			this.Label8 = new System.Windows.Forms.Label();
			this.Label6 = new System.Windows.Forms.Label();
			this.Label9 = new System.Windows.Forms.Label();
			this.Label7 = new System.Windows.Forms.Label();
			this.btnPortraitAdd = new System.Windows.Forms.Button();
			this.btnPortraitDelete = new System.Windows.Forms.Button();
			this.EditFamily = new System.Windows.Forms.TextBox();
			this.EditName = new System.Windows.Forms.TextBox();
			this.EditPatronymic = new System.Windows.Forms.TextBox();
			this.EditSex = new System.Windows.Forms.ComboBox();
			this.CheckPatriarch = new System.Windows.Forms.CheckBox();
			this.PageCtlParents = new System.Windows.Forms.Panel();
			this.EditMother = new System.Windows.Forms.TextBox();
			this.Label12 = new System.Windows.Forms.Label();
			this.btnParentsAdd = new System.Windows.Forms.Button();
			this.btnParentsEdit = new System.Windows.Forms.Button();
			this.btnParentsDelete = new System.Windows.Forms.Button();
			this.btnFatherAdd = new System.Windows.Forms.Button();
			this.btnFatherDelete = new System.Windows.Forms.Button();
			this.btnFatherSel = new System.Windows.Forms.Button();
			this.btnMotherAdd = new System.Windows.Forms.Button();
			this.btnMotherDelete = new System.Windows.Forms.Button();
			this.btnMotherSel = new System.Windows.Forms.Button();
			this.EditFather = new System.Windows.Forms.TextBox();
			this.chkBookmark = new System.Windows.Forms.CheckBox();
			this.edPieceSurnamePrefix = new System.Windows.Forms.TextBox();
			this.edPiecePrefix = new System.Windows.Forms.TextBox();
			this.edPieceSuffix = new System.Windows.Forms.TextBox();
			this.edPieceNickname = new System.Windows.Forms.TextBox();
			this.panPortrait = new System.Windows.Forms.Panel();
			this.imgPortrait = new System.Windows.Forms.PictureBox();
			this.btnNameCopy = new System.Windows.Forms.Button();
			this.PagesPersonData = new System.Windows.Forms.TabControl();
			this.SheetEvents = new System.Windows.Forms.TabPage();
			this.SheetSpouses = new System.Windows.Forms.TabPage();
			this.SheetAssociations = new System.Windows.Forms.TabPage();
			this.SheetGroups = new System.Windows.Forms.TabPage();
			this.SheetNotes = new System.Windows.Forms.TabPage();
			this.SheetMultimedia = new System.Windows.Forms.TabPage();
			this.SheetSources = new System.Windows.Forms.TabPage();
			this.SheetUserRefs = new System.Windows.Forms.TabPage();
			this.GroupBox1.SuspendLayout();
			this.PageCtlParents.SuspendLayout();
			this.panPortrait.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this.imgPortrait)).BeginInit();
			this.PagesPersonData.SuspendLayout();
			this.SuspendLayout();
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(448, 544);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(81, 25);
			this.btnAccept.TabIndex = 5;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(536, 544);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(81, 25);
			this.btnCancel.TabIndex = 6;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// Label5
			// 
			this.Label5.Location = new System.Drawing.Point(8, 552);
			this.Label5.Name = "Label5";
			this.Label5.Size = new System.Drawing.Size(150, 13);
			this.Label5.TabIndex = 2;
			this.Label5.Text = "Ограничение безопасности";
			// 
			// cbRestriction
			// 
			this.cbRestriction.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbRestriction.Location = new System.Drawing.Point(160, 544);
			this.cbRestriction.Name = "cbRestriction";
			this.cbRestriction.Size = new System.Drawing.Size(145, 21);
			this.cbRestriction.TabIndex = 3;
			this.cbRestriction.SelectedIndexChanged += new System.EventHandler(this.cbRestriction_SelectedIndexChanged);
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.Label1);
			this.GroupBox1.Controls.Add(this.Label2);
			this.GroupBox1.Controls.Add(this.Label3);
			this.GroupBox1.Controls.Add(this.Label4);
			this.GroupBox1.Controls.Add(this.Label8);
			this.GroupBox1.Controls.Add(this.Label6);
			this.GroupBox1.Controls.Add(this.Label9);
			this.GroupBox1.Controls.Add(this.Label7);
			this.GroupBox1.Controls.Add(this.btnPortraitAdd);
			this.GroupBox1.Controls.Add(this.btnPortraitDelete);
			this.GroupBox1.Controls.Add(this.EditFamily);
			this.GroupBox1.Controls.Add(this.EditName);
			this.GroupBox1.Controls.Add(this.EditPatronymic);
			this.GroupBox1.Controls.Add(this.EditSex);
			this.GroupBox1.Controls.Add(this.CheckPatriarch);
			this.GroupBox1.Controls.Add(this.PageCtlParents);
			this.GroupBox1.Controls.Add(this.chkBookmark);
			this.GroupBox1.Controls.Add(this.edPieceSurnamePrefix);
			this.GroupBox1.Controls.Add(this.edPiecePrefix);
			this.GroupBox1.Controls.Add(this.edPieceSuffix);
			this.GroupBox1.Controls.Add(this.edPieceNickname);
			this.GroupBox1.Controls.Add(this.panPortrait);
			this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
			this.GroupBox1.Location = new System.Drawing.Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(625, 265);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			// 
			// Label1
			// 
			this.Label1.Location = new System.Drawing.Point(8, 16);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(60, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Фамилия";
			// 
			// Label2
			// 
			this.Label2.Location = new System.Drawing.Point(8, 56);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(60, 13);
			this.Label2.TabIndex = 2;
			this.Label2.Text = "Имя";
			// 
			// Label3
			// 
			this.Label3.Location = new System.Drawing.Point(8, 96);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(60, 13);
			this.Label3.TabIndex = 4;
			this.Label3.Text = "Отчество";
			// 
			// Label4
			// 
			this.Label4.Location = new System.Drawing.Point(184, 136);
			this.Label4.Name = "Label4";
			this.Label4.Size = new System.Drawing.Size(35, 13);
			this.Label4.TabIndex = 14;
			this.Label4.Text = "Пол";
			// 
			// Label8
			// 
			this.Label8.Location = new System.Drawing.Point(184, 16);
			this.Label8.Name = "Label8";
			this.Label8.Size = new System.Drawing.Size(105, 13);
			this.Label8.TabIndex = 8;
			this.Label8.Text = "Префикс фамилии";
			// 
			// Label6
			// 
			this.Label6.Location = new System.Drawing.Point(184, 56);
			this.Label6.Name = "Label6";
			this.Label6.Size = new System.Drawing.Size(105, 13);
			this.Label6.TabIndex = 10;
			this.Label6.Text = "Префикс имени";
			// 
			// Label9
			// 
			this.Label9.Location = new System.Drawing.Point(184, 96);
			this.Label9.Name = "Label9";
			this.Label9.Size = new System.Drawing.Size(105, 13);
			this.Label9.TabIndex = 12;
			this.Label9.Text = "Суффикс имени";
			// 
			// Label7
			// 
			this.Label7.Location = new System.Drawing.Point(8, 136);
			this.Label7.Name = "Label7";
			this.Label7.Size = new System.Drawing.Size(60, 13);
			this.Label7.TabIndex = 6;
			this.Label7.Text = "Прозвище";
			// 
			// btnPortraitAdd
			// 
			this.btnPortraitAdd.AccessibleDescription = "Присоединить портрет";
			this.btnPortraitAdd.Image = global::GKResources.iRecNew;
			this.btnPortraitAdd.Location = new System.Drawing.Point(552, 152);
			this.btnPortraitAdd.Name = "btnPortraitAdd";
			this.btnPortraitAdd.Size = new System.Drawing.Size(26, 26);
			this.btnPortraitAdd.TabIndex = 19;
			this.btnPortraitAdd.Click += new System.EventHandler(this.btnPortraitAdd_Click);
			// 
			// btnPortraitDelete
			// 
			this.btnPortraitDelete.AccessibleDescription = "Отсоединить портрет";
			this.btnPortraitDelete.Image = global::GKResources.iRecDelete;
			this.btnPortraitDelete.Location = new System.Drawing.Point(584, 152);
			this.btnPortraitDelete.Name = "btnPortraitDelete";
			this.btnPortraitDelete.Size = new System.Drawing.Size(26, 26);
			this.btnPortraitDelete.TabIndex = 20;
			this.btnPortraitDelete.Click += new System.EventHandler(this.btnPortraitDelete_Click);
			// 
			// EditFamily
			// 
			this.EditFamily.Location = new System.Drawing.Point(8, 32);
			this.EditFamily.Name = "EditFamily";
			this.EditFamily.Size = new System.Drawing.Size(161, 21);
			this.EditFamily.TabIndex = 1;
			this.EditFamily.TextChanged += new System.EventHandler(this.EditFamily_TextChanged);
			this.EditFamily.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.EditFamily_KeyPress);
			// 
			// EditName
			// 
			this.EditName.Location = new System.Drawing.Point(8, 72);
			this.EditName.Name = "EditName";
			this.EditName.Size = new System.Drawing.Size(161, 21);
			this.EditName.TabIndex = 3;
			this.EditName.TextChanged += new System.EventHandler(this.EditName_TextChanged);
			this.EditName.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.EditFamily_KeyPress);
			// 
			// EditPatronymic
			// 
			this.EditPatronymic.Location = new System.Drawing.Point(8, 112);
			this.EditPatronymic.Name = "EditPatronymic";
			this.EditPatronymic.Size = new System.Drawing.Size(161, 21);
			this.EditPatronymic.TabIndex = 5;
			this.EditPatronymic.TextChanged += new System.EventHandler(this.EditPatronymic_TextChanged);
			this.EditPatronymic.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.EditFamily_KeyPress);
			// 
			// EditSex
			// 
			this.EditSex.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.EditSex.Location = new System.Drawing.Point(184, 152);
			this.EditSex.Name = "EditSex";
			this.EditSex.Size = new System.Drawing.Size(121, 21);
			this.EditSex.TabIndex = 15;
			// 
			// CheckPatriarch
			// 
			this.CheckPatriarch.Location = new System.Drawing.Point(320, 144);
			this.CheckPatriarch.Name = "CheckPatriarch";
			this.CheckPatriarch.Size = new System.Drawing.Size(153, 17);
			this.CheckPatriarch.TabIndex = 16;
			this.CheckPatriarch.Text = "Патриарх (глава семьи)";
			// 
			// PageCtlParents
			// 
			this.PageCtlParents.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.PageCtlParents.Controls.Add(this.EditMother);
			this.PageCtlParents.Controls.Add(this.Label12);
			this.PageCtlParents.Controls.Add(this.btnParentsAdd);
			this.PageCtlParents.Controls.Add(this.btnParentsEdit);
			this.PageCtlParents.Controls.Add(this.btnParentsDelete);
			this.PageCtlParents.Controls.Add(this.btnFatherAdd);
			this.PageCtlParents.Controls.Add(this.btnFatherDelete);
			this.PageCtlParents.Controls.Add(this.btnFatherSel);
			this.PageCtlParents.Controls.Add(this.btnMotherAdd);
			this.PageCtlParents.Controls.Add(this.btnMotherDelete);
			this.PageCtlParents.Controls.Add(this.btnMotherSel);
			this.PageCtlParents.Controls.Add(this.EditFather);
			this.PageCtlParents.Location = new System.Drawing.Point(2, 192);
			this.PageCtlParents.Name = "PageCtlParents";
			this.PageCtlParents.Size = new System.Drawing.Size(621, 71);
			this.PageCtlParents.TabIndex = 21;
			// 
			// EditMother
			// 
			this.EditMother.ForeColor = System.Drawing.SystemColors.Control;
			this.EditMother.Location = new System.Drawing.Point(296, 8);
			this.EditMother.Name = "EditMother";
			this.EditMother.ReadOnly = true;
			this.EditMother.Size = new System.Drawing.Size(224, 21);
			this.EditMother.TabIndex = 2;
			// 
			// Label12
			// 
			this.Label12.Location = new System.Drawing.Point(8, 16);
			this.Label12.Name = "Label12";
			this.Label12.Size = new System.Drawing.Size(56, 13);
			this.Label12.TabIndex = 0;
			this.Label12.Text = "Родители";
			// 
			// btnParentsAdd
			// 
			this.btnParentsAdd.AccessibleDescription = "Присоединить семью родителей";
			this.btnParentsAdd.Image = global::GKResources.iRecNew;
			this.btnParentsAdd.Location = new System.Drawing.Point(525, 5);
			this.btnParentsAdd.Name = "btnParentsAdd";
			this.btnParentsAdd.Size = new System.Drawing.Size(26, 26);
			this.btnParentsAdd.TabIndex = 3;
			this.btnParentsAdd.Click += new System.EventHandler(this.btnParentsAdd_Click);
			// 
			// btnParentsEdit
			// 
			this.btnParentsEdit.AccessibleDescription = "Правка семьи родителей";
			this.btnParentsEdit.Image = global::GKResources.iRecEdit;
			this.btnParentsEdit.Location = new System.Drawing.Point(554, 5);
			this.btnParentsEdit.Name = "btnParentsEdit";
			this.btnParentsEdit.Size = new System.Drawing.Size(26, 26);
			this.btnParentsEdit.TabIndex = 4;
			this.btnParentsEdit.Click += new System.EventHandler(this.btnParentsEdit_Click);
			// 
			// btnParentsDelete
			// 
			this.btnParentsDelete.AccessibleDescription = "Отсоединить семью родителей";
			this.btnParentsDelete.Image = global::GKResources.iRecDelete;
			this.btnParentsDelete.Location = new System.Drawing.Point(583, 5);
			this.btnParentsDelete.Name = "btnParentsDelete";
			this.btnParentsDelete.Size = new System.Drawing.Size(26, 26);
			this.btnParentsDelete.TabIndex = 5;
			this.btnParentsDelete.Click += new System.EventHandler(this.btnParentsDelete_Click);
			// 
			// btnFatherAdd
			// 
			this.btnFatherAdd.AccessibleDescription = "Выбрать или добавить отца";
			this.btnFatherAdd.Image = global::GKResources.iRecNew;
			this.btnFatherAdd.Location = new System.Drawing.Point(198, 35);
			this.btnFatherAdd.Name = "btnFatherAdd";
			this.btnFatherAdd.Size = new System.Drawing.Size(26, 26);
			this.btnFatherAdd.TabIndex = 6;
			this.btnFatherAdd.Click += new System.EventHandler(this.btnFatherAdd_Click);
			// 
			// btnFatherDelete
			// 
			this.btnFatherDelete.AccessibleDescription = "Отсоединить отца";
			this.btnFatherDelete.Image = global::GKResources.iRecEdit;
			this.btnFatherDelete.Location = new System.Drawing.Point(230, 35);
			this.btnFatherDelete.Name = "btnFatherDelete";
			this.btnFatherDelete.Size = new System.Drawing.Size(26, 26);
			this.btnFatherDelete.TabIndex = 7;
			this.btnFatherDelete.Click += new System.EventHandler(this.btnFatherDelete_Click);
			// 
			// btnFatherSel
			// 
			this.btnFatherSel.AccessibleDescription = "Перейти на запись отца";
			this.btnFatherSel.Image = global::GKResources.iRecDelete;
			this.btnFatherSel.Location = new System.Drawing.Point(262, 35);
			this.btnFatherSel.Name = "btnFatherSel";
			this.btnFatherSel.Size = new System.Drawing.Size(26, 26);
			this.btnFatherSel.TabIndex = 8;
			this.btnFatherSel.Click += new System.EventHandler(this.btnFatherSel_Click);
			// 
			// btnMotherAdd
			// 
			this.btnMotherAdd.AccessibleDescription = "Выбрать или добавить мать";
			this.btnMotherAdd.Image = global::GKResources.iRecNew;
			this.btnMotherAdd.Location = new System.Drawing.Point(430, 35);
			this.btnMotherAdd.Name = "btnMotherAdd";
			this.btnMotherAdd.Size = new System.Drawing.Size(26, 26);
			this.btnMotherAdd.TabIndex = 9;
			this.btnMotherAdd.Click += new System.EventHandler(this.btnMotherAdd_Click);
			// 
			// btnMotherDelete
			// 
			this.btnMotherDelete.AccessibleDescription = "Отсоединить мать";
			this.btnMotherDelete.Image = global::GKResources.iRecEdit;
			this.btnMotherDelete.Location = new System.Drawing.Point(462, 35);
			this.btnMotherDelete.Name = "btnMotherDelete";
			this.btnMotherDelete.Size = new System.Drawing.Size(26, 26);
			this.btnMotherDelete.TabIndex = 10;
			this.btnMotherDelete.Click += new System.EventHandler(this.btnMotherDelete_Click);
			// 
			// btnMotherSel
			// 
			this.btnMotherSel.AccessibleDescription = "Перейти на запись матери";
			this.btnMotherSel.Image = global::GKResources.iRecDelete;
			this.btnMotherSel.Location = new System.Drawing.Point(494, 35);
			this.btnMotherSel.Name = "btnMotherSel";
			this.btnMotherSel.Size = new System.Drawing.Size(26, 26);
			this.btnMotherSel.TabIndex = 11;
			this.btnMotherSel.Click += new System.EventHandler(this.btnMotherSel_Click);
			// 
			// EditFather
			// 
			this.EditFather.ForeColor = System.Drawing.SystemColors.Control;
			this.EditFather.Location = new System.Drawing.Point(64, 8);
			this.EditFather.Name = "EditFather";
			this.EditFather.ReadOnly = true;
			this.EditFather.Size = new System.Drawing.Size(224, 21);
			this.EditFather.TabIndex = 1;
			// 
			// chkBookmark
			// 
			this.chkBookmark.Location = new System.Drawing.Point(320, 161);
			this.chkBookmark.Name = "chkBookmark";
			this.chkBookmark.Size = new System.Drawing.Size(153, 17);
			this.chkBookmark.TabIndex = 17;
			this.chkBookmark.Text = "Закладка";
			// 
			// edPieceSurnamePrefix
			// 
			this.edPieceSurnamePrefix.Location = new System.Drawing.Point(184, 32);
			this.edPieceSurnamePrefix.Name = "edPieceSurnamePrefix";
			this.edPieceSurnamePrefix.Size = new System.Drawing.Size(121, 21);
			this.edPieceSurnamePrefix.TabIndex = 9;
			// 
			// edPiecePrefix
			// 
			this.edPiecePrefix.Location = new System.Drawing.Point(184, 72);
			this.edPiecePrefix.Name = "edPiecePrefix";
			this.edPiecePrefix.Size = new System.Drawing.Size(121, 21);
			this.edPiecePrefix.TabIndex = 11;
			// 
			// edPieceSuffix
			// 
			this.edPieceSuffix.Location = new System.Drawing.Point(184, 112);
			this.edPieceSuffix.Name = "edPieceSuffix";
			this.edPieceSuffix.Size = new System.Drawing.Size(121, 21);
			this.edPieceSuffix.TabIndex = 13;
			// 
			// edPieceNickname
			// 
			this.edPieceNickname.Location = new System.Drawing.Point(8, 152);
			this.edPieceNickname.Name = "edPieceNickname";
			this.edPieceNickname.Size = new System.Drawing.Size(161, 21);
			this.edPieceNickname.TabIndex = 7;
			// 
			// panPortrait
			// 
			this.panPortrait.Controls.Add(this.imgPortrait);
			this.panPortrait.Location = new System.Drawing.Point(480, 16);
			this.panPortrait.Name = "panPortrait";
			this.panPortrait.Size = new System.Drawing.Size(130, 130);
			this.panPortrait.TabIndex = 18;
			// 
			// imgPortrait
			// 
			this.imgPortrait.Location = new System.Drawing.Point(1, 1);
			this.imgPortrait.Name = "imgPortrait";
			this.imgPortrait.Size = new System.Drawing.Size(128, 128);
			this.imgPortrait.TabIndex = 0;
			this.imgPortrait.TabStop = false;
			// 
			// btnNameCopy
			// 
			this.btnNameCopy.AccessibleDescription = "Скопировать имя в буфер обмена";
			this.btnNameCopy.Image = global::GKResources.iCopy;
			this.btnNameCopy.Location = new System.Drawing.Point(384, 544);
			this.btnNameCopy.Name = "btnNameCopy";
			this.btnNameCopy.Size = new System.Drawing.Size(33, 25);
			this.btnNameCopy.TabIndex = 4;
			this.btnNameCopy.Click += new System.EventHandler(this.btnNameCopy1_Click);
			// 
			// PagesPersonData
			// 
			this.PagesPersonData.Controls.Add(this.SheetEvents);
			this.PagesPersonData.Controls.Add(this.SheetSpouses);
			this.PagesPersonData.Controls.Add(this.SheetAssociations);
			this.PagesPersonData.Controls.Add(this.SheetGroups);
			this.PagesPersonData.Controls.Add(this.SheetNotes);
			this.PagesPersonData.Controls.Add(this.SheetMultimedia);
			this.PagesPersonData.Controls.Add(this.SheetSources);
			this.PagesPersonData.Controls.Add(this.SheetUserRefs);
			this.PagesPersonData.Dock = System.Windows.Forms.DockStyle.Top;
			this.PagesPersonData.Location = new System.Drawing.Point(0, 265);
			this.PagesPersonData.Name = "PagesPersonData";
			this.PagesPersonData.SelectedIndex = 0;
			this.PagesPersonData.Size = new System.Drawing.Size(625, 264);
			this.PagesPersonData.TabIndex = 1;
			// 
			// SheetEvents
			// 
			this.SheetEvents.Location = new System.Drawing.Point(4, 22);
			this.SheetEvents.Name = "SheetEvents";
			this.SheetEvents.Size = new System.Drawing.Size(617, 238);
			this.SheetEvents.TabIndex = 0;
			this.SheetEvents.Text = "Факты";
			// 
			// SheetSpouses
			// 
			this.SheetSpouses.Location = new System.Drawing.Point(4, 22);
			this.SheetSpouses.Name = "SheetSpouses";
			this.SheetSpouses.Size = new System.Drawing.Size(617, 238);
			this.SheetSpouses.TabIndex = 1;
			this.SheetSpouses.Text = "Супруги";
			// 
			// SheetAssociations
			// 
			this.SheetAssociations.Location = new System.Drawing.Point(4, 22);
			this.SheetAssociations.Name = "SheetAssociations";
			this.SheetAssociations.Size = new System.Drawing.Size(617, 238);
			this.SheetAssociations.TabIndex = 2;
			this.SheetAssociations.Text = "Ассоциации";
			// 
			// SheetGroups
			// 
			this.SheetGroups.Location = new System.Drawing.Point(4, 22);
			this.SheetGroups.Name = "SheetGroups";
			this.SheetGroups.Size = new System.Drawing.Size(617, 238);
			this.SheetGroups.TabIndex = 3;
			this.SheetGroups.Text = "Группы";
			// 
			// SheetNotes
			// 
			this.SheetNotes.Location = new System.Drawing.Point(4, 22);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new System.Drawing.Size(617, 238);
			this.SheetNotes.TabIndex = 4;
			this.SheetNotes.Text = "Заметки";
			// 
			// SheetMultimedia
			// 
			this.SheetMultimedia.Location = new System.Drawing.Point(4, 22);
			this.SheetMultimedia.Name = "SheetMultimedia";
			this.SheetMultimedia.Size = new System.Drawing.Size(617, 238);
			this.SheetMultimedia.TabIndex = 5;
			this.SheetMultimedia.Text = "Мультимедиа";
			// 
			// SheetSources
			// 
			this.SheetSources.Location = new System.Drawing.Point(4, 22);
			this.SheetSources.Name = "SheetSources";
			this.SheetSources.Size = new System.Drawing.Size(617, 238);
			this.SheetSources.TabIndex = 6;
			this.SheetSources.Text = "Источники";
			// 
			// SheetUserRefs
			// 
			this.SheetUserRefs.Location = new System.Drawing.Point(4, 22);
			this.SheetUserRefs.Name = "SheetUserRefs";
			this.SheetUserRefs.Size = new System.Drawing.Size(617, 238);
			this.SheetUserRefs.TabIndex = 7;
			this.SheetUserRefs.Text = "Сноски/Пометки";
			// 
			// TfmPersonEdit
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(625, 577);
			this.Controls.Add(this.PagesPersonData);
			this.Controls.Add(this.Label5);
			this.Controls.Add(this.btnNameCopy);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.cbRestriction);
			this.Controls.Add(this.GroupBox1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TfmPersonEdit";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Редактирование персональной информации";
			this.GroupBox1.ResumeLayout(false);
			this.GroupBox1.PerformLayout();
			this.PageCtlParents.ResumeLayout(false);
			this.PageCtlParents.PerformLayout();
			this.panPortrait.ResumeLayout(false);
			((System.ComponentModel.ISupportInitialize)(this.imgPortrait)).EndInit();
			this.PagesPersonData.ResumeLayout(false);
			this.ResumeLayout(false);
		}
	}
}