using System;

namespace GKUI.Dialogs
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
		private System.Windows.Forms.TextBox edSurname;
		private System.Windows.Forms.TextBox edName;
		private System.Windows.Forms.TextBox edPatronymic;
		private System.Windows.Forms.ComboBox edSex;
		private System.Windows.Forms.CheckBox chkPatriarch;
		private System.Windows.Forms.TabPage SheetUserRefs;
		private System.Windows.Forms.Panel PageCtlParents;
		private System.Windows.Forms.Label Label12;
		private System.Windows.Forms.TextBox EditFather;
        private System.Windows.Forms.TextBox EditMother;
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
			this.edSurname = new System.Windows.Forms.TextBox();
			this.edName = new System.Windows.Forms.TextBox();
			this.edPatronymic = new System.Windows.Forms.TextBox();
			this.edSex = new System.Windows.Forms.ComboBox();
			this.chkPatriarch = new System.Windows.Forms.CheckBox();
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
			this.SheetNames = new System.Windows.Forms.TabPage();
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
			this.btnAccept.Location = new System.Drawing.Point(502, 529);
			this.btnAccept.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(91, 24);
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
			this.btnCancel.Location = new System.Drawing.Point(600, 529);
			this.btnCancel.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(91, 24);
			this.btnCancel.TabIndex = 6;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// Label5
			// 
			this.Label5.AutoSize = true;
			this.Label5.Location = new System.Drawing.Point(9, 531);
			this.Label5.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.Label5.Name = "Label5";
			this.Label5.Size = new System.Drawing.Size(146, 13);
			this.Label5.TabIndex = 2;
			this.Label5.Text = "Ограничение безопасности";
			// 
			// cbRestriction
			// 
			this.cbRestriction.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbRestriction.Location = new System.Drawing.Point(179, 529);
			this.cbRestriction.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.cbRestriction.Name = "cbRestriction";
			this.cbRestriction.Size = new System.Drawing.Size(163, 21);
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
			this.GroupBox1.Controls.Add(this.edSurname);
			this.GroupBox1.Controls.Add(this.edName);
			this.GroupBox1.Controls.Add(this.edPatronymic);
			this.GroupBox1.Controls.Add(this.edSex);
			this.GroupBox1.Controls.Add(this.chkPatriarch);
			this.GroupBox1.Controls.Add(this.PageCtlParents);
			this.GroupBox1.Controls.Add(this.chkBookmark);
			this.GroupBox1.Controls.Add(this.edPieceSurnamePrefix);
			this.GroupBox1.Controls.Add(this.edPiecePrefix);
			this.GroupBox1.Controls.Add(this.edPieceSuffix);
			this.GroupBox1.Controls.Add(this.edPieceNickname);
			this.GroupBox1.Controls.Add(this.panPortrait);
			this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
			this.GroupBox1.Location = new System.Drawing.Point(0, 0);
			this.GroupBox1.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Padding = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.GroupBox1.Size = new System.Drawing.Size(699, 258);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			// 
			// Label1
			// 
			this.Label1.AutoSize = true;
			this.Label1.Location = new System.Drawing.Point(9, 15);
			this.Label1.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(51, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Фамилия";
			// 
			// Label2
			// 
			this.Label2.AutoSize = true;
			this.Label2.Location = new System.Drawing.Point(9, 54);
			this.Label2.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(26, 13);
			this.Label2.TabIndex = 2;
			this.Label2.Text = "Имя";
			// 
			// Label3
			// 
			this.Label3.AutoSize = true;
			this.Label3.Location = new System.Drawing.Point(9, 94);
			this.Label3.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(56, 13);
			this.Label3.TabIndex = 4;
			this.Label3.Text = "Отчество";
			// 
			// Label4
			// 
			this.Label4.AutoSize = true;
			this.Label4.Location = new System.Drawing.Point(9, 132);
			this.Label4.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.Label4.Name = "Label4";
			this.Label4.Size = new System.Drawing.Size(26, 13);
			this.Label4.TabIndex = 14;
			this.Label4.Text = "Пол";
			// 
			// Label8
			// 
			this.Label8.AutoSize = true;
			this.Label8.Location = new System.Drawing.Point(206, 15);
			this.Label8.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.Label8.Name = "Label8";
			this.Label8.Size = new System.Drawing.Size(98, 13);
			this.Label8.TabIndex = 8;
			this.Label8.Text = "Префикс фамилии";
			// 
			// Label6
			// 
			this.Label6.AutoSize = true;
			this.Label6.Location = new System.Drawing.Point(206, 54);
			this.Label6.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.Label6.Name = "Label6";
			this.Label6.Size = new System.Drawing.Size(84, 13);
			this.Label6.TabIndex = 10;
			this.Label6.Text = "Префикс имени";
			// 
			// Label9
			// 
			this.Label9.AutoSize = true;
			this.Label9.Location = new System.Drawing.Point(206, 94);
			this.Label9.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.Label9.Name = "Label9";
			this.Label9.Size = new System.Drawing.Size(86, 13);
			this.Label9.TabIndex = 12;
			this.Label9.Text = "Суффикс имени";
			// 
			// Label7
			// 
			this.Label7.AutoSize = true;
			this.Label7.Location = new System.Drawing.Point(204, 132);
			this.Label7.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.Label7.Name = "Label7";
			this.Label7.Size = new System.Drawing.Size(58, 13);
			this.Label7.TabIndex = 6;
			this.Label7.Text = "Прозвище";
			// 
			// btnPortraitAdd
			// 
			this.btnPortraitAdd.AccessibleDescription = "Присоединить портрет";
			this.btnPortraitAdd.Image = global::GKResources.iRecNew;
			this.btnPortraitAdd.Location = new System.Drawing.Point(618, 148);
			this.btnPortraitAdd.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.btnPortraitAdd.Name = "btnPortraitAdd";
			this.btnPortraitAdd.Size = new System.Drawing.Size(29, 29);
			this.btnPortraitAdd.TabIndex = 19;
			this.btnPortraitAdd.Click += new System.EventHandler(this.btnPortraitAdd_Click);
			// 
			// btnPortraitDelete
			// 
			this.btnPortraitDelete.AccessibleDescription = "Отсоединить портрет";
			this.btnPortraitDelete.Image = global::GKResources.iRecDelete;
			this.btnPortraitDelete.Location = new System.Drawing.Point(654, 148);
			this.btnPortraitDelete.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.btnPortraitDelete.Name = "btnPortraitDelete";
			this.btnPortraitDelete.Size = new System.Drawing.Size(29, 29);
			this.btnPortraitDelete.TabIndex = 20;
			this.btnPortraitDelete.Click += new System.EventHandler(this.btnPortraitDelete_Click);
			// 
			// edSurname
			// 
			this.edSurname.Location = new System.Drawing.Point(9, 31);
			this.edSurname.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.edSurname.Name = "edSurname";
			this.edSurname.Size = new System.Drawing.Size(182, 21);
			this.edSurname.TabIndex = 1;
			this.edSurname.TextChanged += new System.EventHandler(this.edSurname_TextChanged);
			this.edSurname.KeyDown += new System.Windows.Forms.KeyEventHandler(this.edSurname_KeyDown);
			this.edSurname.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edSurname_KeyPress);
			// 
			// edName
			// 
			this.edName.Location = new System.Drawing.Point(9, 70);
			this.edName.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.edName.Name = "edName";
			this.edName.Size = new System.Drawing.Size(182, 21);
			this.edName.TabIndex = 3;
			this.edName.TextChanged += new System.EventHandler(this.EditName_TextChanged);
			this.edName.KeyDown += new System.Windows.Forms.KeyEventHandler(this.edSurname_KeyDown);
			this.edName.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edSurname_KeyPress);
			// 
			// edPatronymic
			// 
			this.edPatronymic.Location = new System.Drawing.Point(9, 109);
			this.edPatronymic.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.edPatronymic.Name = "edPatronymic";
			this.edPatronymic.Size = new System.Drawing.Size(182, 21);
			this.edPatronymic.TabIndex = 5;
			this.edPatronymic.TextChanged += new System.EventHandler(this.EditPatronymic_TextChanged);
			this.edPatronymic.KeyDown += new System.Windows.Forms.KeyEventHandler(this.edSurname_KeyDown);
			this.edPatronymic.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.edSurname_KeyPress);
			// 
			// edSex
			// 
			this.edSex.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.edSex.Location = new System.Drawing.Point(9, 148);
			this.edSex.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.edSex.Name = "edSex";
			this.edSex.Size = new System.Drawing.Size(182, 21);
			this.edSex.TabIndex = 15;
			// 
			// chkPatriarch
			// 
			this.chkPatriarch.AutoSize = true;
			this.chkPatriarch.Location = new System.Drawing.Point(358, 140);
			this.chkPatriarch.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.chkPatriarch.Name = "chkPatriarch";
			this.chkPatriarch.Size = new System.Drawing.Size(147, 17);
			this.chkPatriarch.TabIndex = 16;
			this.chkPatriarch.Text = "Патриарх (глава семьи)";
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
			this.PageCtlParents.Location = new System.Drawing.Point(2, 186);
			this.PageCtlParents.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.PageCtlParents.Name = "PageCtlParents";
			this.PageCtlParents.Size = new System.Drawing.Size(696, 69);
			this.PageCtlParents.TabIndex = 21;
			// 
			// EditMother
			// 
			this.EditMother.ForeColor = System.Drawing.SystemColors.Control;
			this.EditMother.Location = new System.Drawing.Point(331, 8);
			this.EditMother.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.EditMother.Name = "EditMother";
			this.EditMother.ReadOnly = true;
			this.EditMother.Size = new System.Drawing.Size(252, 21);
			this.EditMother.TabIndex = 2;
			// 
			// Label12
			// 
			this.Label12.AutoSize = true;
			this.Label12.Location = new System.Drawing.Point(10, 11);
			this.Label12.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.Label12.Name = "Label12";
			this.Label12.Size = new System.Drawing.Size(56, 13);
			this.Label12.TabIndex = 0;
			this.Label12.Text = "Родители";
			// 
			// btnParentsAdd
			// 
			this.btnParentsAdd.AccessibleDescription = "Присоединить семью родителей";
			this.btnParentsAdd.Image = global::GKResources.iRecNew;
			this.btnParentsAdd.Location = new System.Drawing.Point(588, 5);
			this.btnParentsAdd.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.btnParentsAdd.Name = "btnParentsAdd";
			this.btnParentsAdd.Size = new System.Drawing.Size(29, 29);
			this.btnParentsAdd.TabIndex = 3;
			this.btnParentsAdd.Click += new System.EventHandler(this.btnParentsAdd_Click);
			// 
			// btnParentsEdit
			// 
			this.btnParentsEdit.AccessibleDescription = "Правка семьи родителей";
			this.btnParentsEdit.Image = global::GKResources.iRecEdit;
			this.btnParentsEdit.Location = new System.Drawing.Point(621, 5);
			this.btnParentsEdit.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.btnParentsEdit.Name = "btnParentsEdit";
			this.btnParentsEdit.Size = new System.Drawing.Size(29, 29);
			this.btnParentsEdit.TabIndex = 4;
			this.btnParentsEdit.Click += new System.EventHandler(this.btnParentsEdit_Click);
			// 
			// btnParentsDelete
			// 
			this.btnParentsDelete.AccessibleDescription = "Отсоединить семью родителей";
			this.btnParentsDelete.Image = global::GKResources.iRecDelete;
			this.btnParentsDelete.Location = new System.Drawing.Point(653, 5);
			this.btnParentsDelete.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.btnParentsDelete.Name = "btnParentsDelete";
			this.btnParentsDelete.Size = new System.Drawing.Size(30, 29);
			this.btnParentsDelete.TabIndex = 5;
			this.btnParentsDelete.Click += new System.EventHandler(this.btnParentsDelete_Click);
			// 
			// btnFatherAdd
			// 
			this.btnFatherAdd.AccessibleDescription = "Выбрать или добавить отца";
			this.btnFatherAdd.Image = global::GKResources.iRecNew;
			this.btnFatherAdd.Location = new System.Drawing.Point(222, 34);
			this.btnFatherAdd.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.btnFatherAdd.Name = "btnFatherAdd";
			this.btnFatherAdd.Size = new System.Drawing.Size(29, 29);
			this.btnFatherAdd.TabIndex = 6;
			this.btnFatherAdd.Click += new System.EventHandler(this.btnFatherAdd_Click);
			// 
			// btnFatherDelete
			// 
			this.btnFatherDelete.AccessibleDescription = "Отсоединить отца";
			this.btnFatherDelete.Image = global::GKResources.iRecEdit;
			this.btnFatherDelete.Location = new System.Drawing.Point(258, 34);
			this.btnFatherDelete.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.btnFatherDelete.Name = "btnFatherDelete";
			this.btnFatherDelete.Size = new System.Drawing.Size(29, 29);
			this.btnFatherDelete.TabIndex = 7;
			this.btnFatherDelete.Click += new System.EventHandler(this.btnFatherDelete_Click);
			// 
			// btnFatherSel
			// 
			this.btnFatherSel.AccessibleDescription = "Перейти на запись отца";
			this.btnFatherSel.Image = global::GKResources.iRecDelete;
			this.btnFatherSel.Location = new System.Drawing.Point(294, 34);
			this.btnFatherSel.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.btnFatherSel.Name = "btnFatherSel";
			this.btnFatherSel.Size = new System.Drawing.Size(29, 29);
			this.btnFatherSel.TabIndex = 8;
			this.btnFatherSel.Click += new System.EventHandler(this.btnFatherSel_Click);
			// 
			// btnMotherAdd
			// 
			this.btnMotherAdd.AccessibleDescription = "Выбрать или добавить мать";
			this.btnMotherAdd.Image = global::GKResources.iRecNew;
			this.btnMotherAdd.Location = new System.Drawing.Point(482, 34);
			this.btnMotherAdd.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.btnMotherAdd.Name = "btnMotherAdd";
			this.btnMotherAdd.Size = new System.Drawing.Size(29, 29);
			this.btnMotherAdd.TabIndex = 9;
			this.btnMotherAdd.Click += new System.EventHandler(this.btnMotherAdd_Click);
			// 
			// btnMotherDelete
			// 
			this.btnMotherDelete.AccessibleDescription = "Отсоединить мать";
			this.btnMotherDelete.Image = global::GKResources.iRecEdit;
			this.btnMotherDelete.Location = new System.Drawing.Point(518, 34);
			this.btnMotherDelete.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.btnMotherDelete.Name = "btnMotherDelete";
			this.btnMotherDelete.Size = new System.Drawing.Size(29, 29);
			this.btnMotherDelete.TabIndex = 10;
			this.btnMotherDelete.Click += new System.EventHandler(this.btnMotherDelete_Click);
			// 
			// btnMotherSel
			// 
			this.btnMotherSel.AccessibleDescription = "Перейти на запись матери";
			this.btnMotherSel.Image = global::GKResources.iRecDelete;
			this.btnMotherSel.Location = new System.Drawing.Point(554, 34);
			this.btnMotherSel.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.btnMotherSel.Name = "btnMotherSel";
			this.btnMotherSel.Size = new System.Drawing.Size(29, 29);
			this.btnMotherSel.TabIndex = 11;
			this.btnMotherSel.Click += new System.EventHandler(this.btnMotherSel_Click);
			// 
			// EditFather
			// 
			this.EditFather.ForeColor = System.Drawing.SystemColors.Control;
			this.EditFather.Location = new System.Drawing.Point(72, 8);
			this.EditFather.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.EditFather.Name = "EditFather";
			this.EditFather.ReadOnly = true;
			this.EditFather.Size = new System.Drawing.Size(251, 21);
			this.EditFather.TabIndex = 1;
			// 
			// chkBookmark
			// 
			this.chkBookmark.AutoSize = true;
			this.chkBookmark.Location = new System.Drawing.Point(358, 157);
			this.chkBookmark.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.chkBookmark.Name = "chkBookmark";
			this.chkBookmark.Size = new System.Drawing.Size(75, 17);
			this.chkBookmark.TabIndex = 17;
			this.chkBookmark.Text = "Закладка";
			// 
			// edPieceSurnamePrefix
			// 
			this.edPieceSurnamePrefix.Location = new System.Drawing.Point(206, 31);
			this.edPieceSurnamePrefix.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.edPieceSurnamePrefix.Name = "edPieceSurnamePrefix";
			this.edPieceSurnamePrefix.Size = new System.Drawing.Size(136, 21);
			this.edPieceSurnamePrefix.TabIndex = 9;
			// 
			// edPiecePrefix
			// 
			this.edPiecePrefix.Location = new System.Drawing.Point(206, 70);
			this.edPiecePrefix.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.edPiecePrefix.Name = "edPiecePrefix";
			this.edPiecePrefix.Size = new System.Drawing.Size(136, 21);
			this.edPiecePrefix.TabIndex = 11;
			// 
			// edPieceSuffix
			// 
			this.edPieceSuffix.Location = new System.Drawing.Point(206, 109);
			this.edPieceSuffix.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.edPieceSuffix.Name = "edPieceSuffix";
			this.edPieceSuffix.Size = new System.Drawing.Size(136, 21);
			this.edPieceSuffix.TabIndex = 13;
			// 
			// edPieceNickname
			// 
			this.edPieceNickname.Location = new System.Drawing.Point(206, 148);
			this.edPieceNickname.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.edPieceNickname.Name = "edPieceNickname";
			this.edPieceNickname.Size = new System.Drawing.Size(136, 21);
			this.edPieceNickname.TabIndex = 7;
			// 
			// panPortrait
			// 
			this.panPortrait.Controls.Add(this.imgPortrait);
			this.panPortrait.Location = new System.Drawing.Point(538, 15);
			this.panPortrait.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.panPortrait.Name = "panPortrait";
			this.panPortrait.Size = new System.Drawing.Size(146, 126);
			this.panPortrait.TabIndex = 18;
			// 
			// imgPortrait
			// 
			this.imgPortrait.Location = new System.Drawing.Point(1, 1);
			this.imgPortrait.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.imgPortrait.Name = "imgPortrait";
			this.imgPortrait.Size = new System.Drawing.Size(144, 125);
			this.imgPortrait.TabIndex = 0;
			this.imgPortrait.TabStop = false;
			// 
			// btnNameCopy
			// 
			this.btnNameCopy.AccessibleDescription = "Скопировать имя в буфер обмена";
			this.btnNameCopy.Image = global::GKResources.iCopy;
			this.btnNameCopy.Location = new System.Drawing.Point(430, 529);
			this.btnNameCopy.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.btnNameCopy.Name = "btnNameCopy";
			this.btnNameCopy.Size = new System.Drawing.Size(37, 24);
			this.btnNameCopy.TabIndex = 4;
			this.btnNameCopy.Click += new System.EventHandler(this.btnNameCopy1_Click);
			// 
			// PagesPersonData
			// 
			this.PagesPersonData.Controls.Add(this.SheetEvents);
			this.PagesPersonData.Controls.Add(this.SheetSpouses);
			this.PagesPersonData.Controls.Add(this.SheetNames);
			this.PagesPersonData.Controls.Add(this.SheetAssociations);
			this.PagesPersonData.Controls.Add(this.SheetGroups);
			this.PagesPersonData.Controls.Add(this.SheetNotes);
			this.PagesPersonData.Controls.Add(this.SheetMultimedia);
			this.PagesPersonData.Controls.Add(this.SheetSources);
			this.PagesPersonData.Controls.Add(this.SheetUserRefs);
			this.PagesPersonData.Dock = System.Windows.Forms.DockStyle.Top;
			this.PagesPersonData.Location = new System.Drawing.Point(0, 258);
			this.PagesPersonData.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.PagesPersonData.Name = "PagesPersonData";
			this.PagesPersonData.SelectedIndex = 0;
			this.PagesPersonData.Size = new System.Drawing.Size(699, 256);
			this.PagesPersonData.TabIndex = 1;
			// 
			// SheetEvents
			// 
			this.SheetEvents.Location = new System.Drawing.Point(4, 22);
			this.SheetEvents.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.SheetEvents.Name = "SheetEvents";
			this.SheetEvents.Size = new System.Drawing.Size(691, 230);
			this.SheetEvents.TabIndex = 0;
			this.SheetEvents.Text = "Факты";
			// 
			// SheetSpouses
			// 
			this.SheetSpouses.Location = new System.Drawing.Point(4, 22);
			this.SheetSpouses.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.SheetSpouses.Name = "SheetSpouses";
			this.SheetSpouses.Size = new System.Drawing.Size(691, 230);
			this.SheetSpouses.TabIndex = 1;
			this.SheetSpouses.Text = "Супруги";
			// 
			// SheetAssociations
			// 
			this.SheetAssociations.Location = new System.Drawing.Point(4, 22);
			this.SheetAssociations.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.SheetAssociations.Name = "SheetAssociations";
			this.SheetAssociations.Size = new System.Drawing.Size(691, 230);
			this.SheetAssociations.TabIndex = 2;
			this.SheetAssociations.Text = "Ассоциации";
			// 
			// SheetGroups
			// 
			this.SheetGroups.Location = new System.Drawing.Point(4, 22);
			this.SheetGroups.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.SheetGroups.Name = "SheetGroups";
			this.SheetGroups.Size = new System.Drawing.Size(691, 230);
			this.SheetGroups.TabIndex = 3;
			this.SheetGroups.Text = "Группы";
			// 
			// SheetNotes
			// 
			this.SheetNotes.Location = new System.Drawing.Point(4, 22);
			this.SheetNotes.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new System.Drawing.Size(691, 230);
			this.SheetNotes.TabIndex = 4;
			this.SheetNotes.Text = "Заметки";
			// 
			// SheetMultimedia
			// 
			this.SheetMultimedia.Location = new System.Drawing.Point(4, 22);
			this.SheetMultimedia.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.SheetMultimedia.Name = "SheetMultimedia";
			this.SheetMultimedia.Size = new System.Drawing.Size(691, 230);
			this.SheetMultimedia.TabIndex = 5;
			this.SheetMultimedia.Text = "Мультимедиа";
			// 
			// SheetSources
			// 
			this.SheetSources.Location = new System.Drawing.Point(4, 22);
			this.SheetSources.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.SheetSources.Name = "SheetSources";
			this.SheetSources.Size = new System.Drawing.Size(691, 230);
			this.SheetSources.TabIndex = 6;
			this.SheetSources.Text = "Источники";
			// 
			// SheetUserRefs
			// 
			this.SheetUserRefs.Location = new System.Drawing.Point(4, 22);
			this.SheetUserRefs.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.SheetUserRefs.Name = "SheetUserRefs";
			this.SheetUserRefs.Size = new System.Drawing.Size(691, 230);
			this.SheetUserRefs.TabIndex = 7;
			this.SheetUserRefs.Text = "Сноски/Пометки";
			// 
			// SheetNames
			// 
			this.SheetNames.BackColor = System.Drawing.SystemColors.Control;
			this.SheetNames.Location = new System.Drawing.Point(4, 22);
			this.SheetNames.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.SheetNames.Name = "SheetNames";
			this.SheetNames.Padding = new System.Windows.Forms.Padding(2, 2, 2, 2);
			this.SheetNames.Size = new System.Drawing.Size(691, 230);
			this.SheetNames.TabIndex = 8;
			this.SheetNames.Text = "Names";
			// 
			// TfmPersonEdit
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(699, 563);
			this.Controls.Add(this.PagesPersonData);
			this.Controls.Add(this.Label5);
			this.Controls.Add(this.btnNameCopy);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.cbRestriction);
			this.Controls.Add(this.GroupBox1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
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
			this.PerformLayout();
		}
		private System.Windows.Forms.TabPage SheetNames;
	}
}