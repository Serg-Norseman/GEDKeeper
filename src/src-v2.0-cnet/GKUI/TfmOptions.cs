using GedCom551;
using GKCore;
using GKSys;
using GKUI.Controls;
using System;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmOptions : Form, ILocalization
	{
		private TabControl PageControl1;
		private TabPage SheetCommon;
		private GroupBox rgCode;
		private Button btnAccept;
		private Button btnCancel;
		private TabPage SheetTree;
		private GroupBox GroupBox1;
		private CheckBox chkFamily;
		private CheckBox chkName;
		private CheckBox chkPatronymic;
		private CheckBox chkDiffLines;
		private CheckBox chkBirthDate;
		private CheckBox chkDeathDate;
		private CheckBox chkKinship;
		private GroupBox GroupBox2;
		private Panel PanMaleColor;
		private Panel PanFemaleColor;
		private Panel PanUnkSexColor;
		private Panel PanUnHusbandColor;
		private Panel PanUnWifeColor;
		private ColorDialog ColorDialog1;
		private GroupBox GroupBox4;
		private Label Label1;
		private Label Label2;
		private Label Label3;
		private Label Label4;
		private CheckBox chkProxy;
		private TextBox edProxyServer;
		private TextBox edProxyPort;
		private TextBox edProxyLogin;
		private TextBox edProxyPass;
		private TabPage SheetView;
		private TabControl PageControl2;
		private TabPage SheetViewCommon;
		private TabPage SheetViewPersons;
		private CheckedListBox ListPersonColumns;
		private Button btnColumnUp;
		private Button btnColumnDown;
		private Button btnDefList;
		private GroupBox rgFNPFormat;
		private GroupBox rgDateFormat;
		private CheckBox chkPlacesWithAddress;
		private GroupBox GroupBox7;
		private CheckBox chkShowOnStart;
		private GroupBox rgEditMode;
		private CheckBox chkHighlightUnparented;
		private CheckBox chkHighlightUnmarried;
		private CheckBox chkOnlyYears;
		private CheckBox chkSignsVisible;
		private CheckBox chkChildlessExclude;
		private Label Label5;
		private Panel PanDefFont;
		private FontDialog FontDialog1;
		private TabPage SheetPedigree;
		private GroupBox GroupBox5;
		private CheckBox chkAttributes;
		private CheckBox chkNotes;
		private CheckBox chkSources;
		private GroupBox EditPedigreeFormat;
		private Label Label6;
		private ComboBox cbLanguages;
		private CheckBox chkTreeDecorative;
		private CheckBox chkPortraitsVisible;
		private TGlobalOptions FOptions;
		private TGlobalOptions.TPersonColumnProps[] FPersonColumns = new TGlobalOptions.TPersonColumnProps[24];
		private RadioButton RButton1;
		private RadioButton RButton2;
		private RadioButton RButton3;
		private RadioButton RButton4;
		private RadioButton RButton5;
		private RadioButton RButton6;
		private RadioButton RButton7;
		private RadioButton RButton8;
		private RadioButton RButton9;
		private RadioButton RButton10;
		private RadioButton RButton11;

		public TGlobalOptions Options
		{
			get { return this.FOptions; }
			set { this.FOptions = value; }
		}

		private void UpdateColumnsList()
		{
			this.ListPersonColumns.ItemCheck -= new ItemCheckEventHandler(this.ListPersonColumns_ItemCheck);
			this.ListPersonColumns.BeginUpdate();
			try
			{
				this.ListPersonColumns.Items.Clear();
				int i = 0;
				do
				{
					TGlobalOptions.TPersonColumnType pct = this.FPersonColumns[i].colType;
					this.ListPersonColumns.Items.Add(GKL.LSList[(int)TGlobalOptions.PersonColumnsName[(int)pct].Name - 1], this.FPersonColumns[i].colActive);
					i++;
				}
				while (i != 24);
			}
			finally
			{
				this.ListPersonColumns.EndUpdate();
			}
			this.ListPersonColumns.ItemCheck += new ItemCheckEventHandler(this.ListPersonColumns_ItemCheck);
		}
		private void UpdateControls()
		{
			this.PanDefFont.Text = this.FOptions.ChartOptions.DefFont_Name + ", " + this.FOptions.ChartOptions.DefFont_Size.ToString();
		}
		private void UpdateForm()
		{
			TGEDCOMObject.TGEDCOMCharacterSet defCharacterSet = this.FOptions.DefCharacterSet;
			if (defCharacterSet != TGEDCOMObject.TGEDCOMCharacterSet.csASCII)
			{
				if (defCharacterSet == TGEDCOMObject.TGEDCOMCharacterSet.csUTF8)
				{
					this.RButton2.Checked = true;
				}
			}
			else
			{
				this.RButton1.Checked = true;
			}
			TGenEngine.TNameFormat defNameFormat = this.FOptions.DefNameFormat;
			if (defNameFormat != TGenEngine.TNameFormat.nfFNP)
			{
				if (defNameFormat != TGenEngine.TNameFormat.nfF_NP)
				{
					if (defNameFormat == TGenEngine.TNameFormat.nfF_N_P)
					{
						this.RButton7.Checked = true;
					}
				}
				else
				{
					this.RButton6.Checked = true;
				}
			}
			else
			{
				this.RButton5.Checked = true;
			}
			TGenEngine.TDateFormat defDateFormat = this.FOptions.DefDateFormat;
			if (defDateFormat != TGenEngine.TDateFormat.dfDD_MM_YYYY)
			{
				if (defDateFormat == TGenEngine.TDateFormat.dfYYYY_MM_DD)
				{
					this.RButton9.Checked = true;
				}
			}
			else
			{
				this.RButton8.Checked = true;
			}
			this.chkPlacesWithAddress.Checked = this.FOptions.PlacesWithAddress;
			this.chkHighlightUnparented.Checked = this.FOptions.ListPersons_HighlightUnparented;
			this.chkHighlightUnmarried.Checked = this.FOptions.ListPersons_HighlightUnmarried;
			this.chkFamily.Checked = this.FOptions.ChartOptions.FamilyVisible;
			this.chkName.Checked = this.FOptions.ChartOptions.NameVisible;
			this.chkPatronymic.Checked = this.FOptions.ChartOptions.PatronymicVisible;
			this.chkDiffLines.Checked = this.FOptions.ChartOptions.DiffLines;
			this.chkBirthDate.Checked = this.FOptions.ChartOptions.BirthDateVisible;
			this.chkDeathDate.Checked = this.FOptions.ChartOptions.DeathDateVisible;
			this.chkOnlyYears.Checked = this.FOptions.ChartOptions.OnlyYears;
			this.chkKinship.Checked = this.FOptions.ChartOptions.Kinship;
			this.chkSignsVisible.Checked = this.FOptions.ChartOptions.SignsVisible;
			this.chkChildlessExclude.Checked = this.FOptions.ChartOptions.ChildlessExclude;
			this.chkTreeDecorative.Checked = this.FOptions.ChartOptions.Decorative;
			this.chkPortraitsVisible.Checked = this.FOptions.ChartOptions.PortraitsVisible;
			this.PanMaleColor.BackColor = this.FOptions.ChartOptions.MaleColor;
			this.PanFemaleColor.BackColor = this.FOptions.ChartOptions.FemaleColor;
			this.PanUnkSexColor.BackColor = this.FOptions.ChartOptions.UnkSexColor;
			this.PanUnHusbandColor.BackColor = this.FOptions.ChartOptions.UnHusbandColor;
			this.PanUnWifeColor.BackColor = this.FOptions.ChartOptions.UnWifeColor;
			this.chkProxy.Checked = this.FOptions.Proxy.UseProxy;
			this.edProxyServer.Text = this.FOptions.Proxy.Server;
			this.edProxyPort.Text = this.FOptions.Proxy.Port;
			this.edProxyLogin.Text = this.FOptions.Proxy.Login;
			this.edProxyPass.Text = this.FOptions.Proxy.Password;
			this.chkAttributes.Checked = this.FOptions.PedigreeOptions.IncludeAttributes;
			this.chkNotes.Checked = this.FOptions.PedigreeOptions.IncludeNotes;
			this.chkSources.Checked = this.FOptions.PedigreeOptions.IncludeSources;
			TPedigreeOptions.TPedigreeFormat format = this.FOptions.PedigreeOptions.Format;
			if (format != TPedigreeOptions.TPedigreeFormat.pfExcess)
			{
				if (format == TPedigreeOptions.TPedigreeFormat.pfCompact)
				{
					this.RButton11.Checked = true;
				}
			}
			else
			{
				this.RButton10.Checked = true;
			}
			this.chkShowOnStart.Checked = this.FOptions.ShowTips;
			TGlobalOptions.TWorkMode workMode = this.FOptions.WorkMode;
			if (workMode != TGlobalOptions.TWorkMode.wmSimple)
			{
				if (workMode == TGlobalOptions.TWorkMode.wmExpert)
				{
					this.RButton4.Checked = true;
				}
			}
			else
			{
				this.RButton3.Checked = true;
			}
			Array.Copy(this.FOptions.ListPersonsColumns, this.FPersonColumns, 24);
			this.UpdateColumnsList();
			this.UpdateControls();
			this.cbLanguages.Items.Clear();
			this.cbLanguages.Items.Add(new TTaggedComboItem("Русский", 1049));
			int idx = 0;
			int arg_472_0 = 0;
			int num = this.FOptions.LangsCount - 1;
			int i = arg_472_0;
			if (num >= i)
			{
				num++;
				do
				{
					TGlobalOptions.TLangRecord lng_rec = this.FOptions.GetLang(i);
					if (this.FOptions.InterfaceLang == lng_rec.Code)
					{
						idx = i + 1;
					}
					this.cbLanguages.Items.Add(new TTaggedComboItem(lng_rec.Name, (int)lng_rec.Code));
					i++;
				}
				while (i != num);
			}
			this.cbLanguages.SelectedIndex = idx;
		}
		private void InitializeComponent()
		{
			this.PageControl1 = new TabControl();
			this.SheetCommon = new TabPage();
			this.Label6 = new Label();
			this.rgCode = new GroupBox();
			this.RButton2 = new RadioButton();
			this.RButton1 = new RadioButton();
			this.GroupBox4 = new GroupBox();
			this.Label1 = new Label();
			this.Label2 = new Label();
			this.Label3 = new Label();
			this.Label4 = new Label();
			this.chkProxy = new CheckBox();
			this.edProxyServer = new TextBox();
			this.edProxyPort = new TextBox();
			this.edProxyLogin = new TextBox();
			this.edProxyPass = new TextBox();
			this.GroupBox7 = new GroupBox();
			this.chkShowOnStart = new CheckBox();
			this.rgEditMode = new GroupBox();
			this.RButton4 = new RadioButton();
			this.RButton3 = new RadioButton();
			this.cbLanguages = new ComboBox();
			this.SheetTree = new TabPage();
			this.GroupBox1 = new GroupBox();
			this.chkFamily = new CheckBox();
			this.chkName = new CheckBox();
			this.chkPatronymic = new CheckBox();
			this.chkDiffLines = new CheckBox();
			this.chkBirthDate = new CheckBox();
			this.chkDeathDate = new CheckBox();
			this.chkKinship = new CheckBox();
			this.chkOnlyYears = new CheckBox();
			this.chkSignsVisible = new CheckBox();
			this.chkChildlessExclude = new CheckBox();
			this.chkTreeDecorative = new CheckBox();
			this.chkPortraitsVisible = new CheckBox();
			this.GroupBox2 = new GroupBox();
			this.Label5 = new Label();
			this.PanMaleColor = new Panel();
			this.PanFemaleColor = new Panel();
			this.PanUnkSexColor = new Panel();
			this.PanUnHusbandColor = new Panel();
			this.PanUnWifeColor = new Panel();
			this.PanDefFont = new Panel();
			this.SheetView = new TabPage();
			this.PageControl2 = new TabControl();
			this.SheetViewCommon = new TabPage();
			this.rgFNPFormat = new GroupBox();
			this.RButton7 = new RadioButton();
			this.RButton6 = new RadioButton();
			this.RButton5 = new RadioButton();
			this.rgDateFormat = new GroupBox();
			this.RButton9 = new RadioButton();
			this.RButton8 = new RadioButton();
			this.chkPlacesWithAddress = new CheckBox();
			this.chkHighlightUnparented = new CheckBox();
			this.chkHighlightUnmarried = new CheckBox();
			this.SheetViewPersons = new TabPage();
			this.btnColumnUp = new Button();
			this.btnColumnDown = new Button();
			this.ListPersonColumns = new CheckedListBox();
			this.btnDefList = new Button();
			this.SheetPedigree = new TabPage();
			this.GroupBox5 = new GroupBox();
			this.chkAttributes = new CheckBox();
			this.chkNotes = new CheckBox();
			this.chkSources = new CheckBox();
			this.EditPedigreeFormat = new GroupBox();
			this.RButton10 = new RadioButton();
			this.RButton11 = new RadioButton();
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.ColorDialog1 = new ColorDialog();
			this.FontDialog1 = new FontDialog();
			this.PageControl1.SuspendLayout();
			this.SheetCommon.SuspendLayout();
			this.rgCode.SuspendLayout();
			this.GroupBox4.SuspendLayout();
			this.GroupBox7.SuspendLayout();
			this.rgEditMode.SuspendLayout();
			this.SheetTree.SuspendLayout();
			this.GroupBox1.SuspendLayout();
			this.GroupBox2.SuspendLayout();
			this.SheetView.SuspendLayout();
			this.PageControl2.SuspendLayout();
			this.SheetViewCommon.SuspendLayout();
			this.rgFNPFormat.SuspendLayout();
			this.rgDateFormat.SuspendLayout();
			this.SheetViewPersons.SuspendLayout();
			this.SheetPedigree.SuspendLayout();
			this.GroupBox5.SuspendLayout();
			this.EditPedigreeFormat.SuspendLayout();
			base.SuspendLayout();
			this.PageControl1.Controls.Add(this.SheetCommon);
			this.PageControl1.Controls.Add(this.SheetTree);
			this.PageControl1.Controls.Add(this.SheetView);
			this.PageControl1.Controls.Add(this.SheetPedigree);
			this.PageControl1.Location = new Point(0, 0);
			this.PageControl1.Name = "PageControl1";
			this.PageControl1.SelectedIndex = 0;
			this.PageControl1.Size = new Size(513, 377);
			this.PageControl1.TabIndex = 0;
			this.SheetCommon.Controls.Add(this.Label6);
			this.SheetCommon.Controls.Add(this.rgCode);
			this.SheetCommon.Controls.Add(this.GroupBox4);
			this.SheetCommon.Controls.Add(this.GroupBox7);
			this.SheetCommon.Controls.Add(this.rgEditMode);
			this.SheetCommon.Controls.Add(this.cbLanguages);
			this.SheetCommon.Location = new Point(4, 22);
			this.SheetCommon.Name = "SheetCommon";
			this.SheetCommon.Size = new Size(505, 351);
			this.SheetCommon.TabIndex = 0;
			this.SheetCommon.Text = "Общие";
			this.Label6.Location = new Point(248, 64);
			this.Label6.Name = "Label6";
			this.Label6.Size = new Size(35, 13);
			this.Label6.TabIndex = 0;
			this.Label6.Text = "Язык";
			this.rgCode.Controls.Add(this.RButton2);
			this.rgCode.Controls.Add(this.RButton1);
			this.rgCode.Location = new Point(8, 8);
			this.rgCode.Name = "rgCode";
			this.rgCode.Size = new Size(217, 49);
			this.rgCode.TabIndex = 0;
			this.rgCode.TabStop = false;
			this.rgCode.Text = "Кодировка сохранения файлов";
			this.RButton2.Location = new Point(88, 16);
			this.RButton2.Name = "RButton2";
			this.RButton2.Size = new Size(80, 24);
			this.RButton2.TabIndex = 1;
			this.RButton2.Text = "UTF-8";
			this.RButton1.Location = new Point(8, 16);
			this.RButton1.Name = "RButton1";
			this.RButton1.Size = new Size(80, 24);
			this.RButton1.TabIndex = 0;
			this.RButton1.Text = "ASCII";
			this.GroupBox4.Controls.Add(this.Label1);
			this.GroupBox4.Controls.Add(this.Label2);
			this.GroupBox4.Controls.Add(this.Label3);
			this.GroupBox4.Controls.Add(this.Label4);
			this.GroupBox4.Controls.Add(this.chkProxy);
			this.GroupBox4.Controls.Add(this.edProxyServer);
			this.GroupBox4.Controls.Add(this.edProxyPort);
			this.GroupBox4.Controls.Add(this.edProxyLogin);
			this.GroupBox4.Controls.Add(this.edProxyPass);
			this.GroupBox4.Location = new Point(8, 128);
			this.GroupBox4.Name = "GroupBox4";
			this.GroupBox4.Size = new Size(217, 161);
			this.GroupBox4.TabIndex = 1;
			this.GroupBox4.TabStop = false;
			this.GroupBox4.Text = "Загрузка из Интернета";
			this.Label1.Location = new Point(16, 56);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(50, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Сервер";
			this.Label2.Location = new Point(16, 80);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(50, 13);
			this.Label2.TabIndex = 1;
			this.Label2.Text = "Порт";
			this.Label3.Location = new Point(16, 104);
			this.Label3.Name = "Label3";
			this.Label3.Size = new Size(50, 13);
			this.Label3.TabIndex = 2;
			this.Label3.Text = "Логин";
			this.Label4.Location = new Point(16, 128);
			this.Label4.Name = "Label4";
			this.Label4.Size = new Size(50, 13);
			this.Label4.TabIndex = 3;
			this.Label4.Text = "Пароль";
			this.chkProxy.Location = new Point(16, 24);
			this.chkProxy.Name = "chkProxy";
			this.chkProxy.Size = new Size(185, 17);
			this.chkProxy.TabIndex = 0;
			this.chkProxy.Text = "Использовать прокси-сервер";
			this.edProxyServer.Location = new Point(80, 48);
			this.edProxyServer.Name = "edProxyServer";
			this.edProxyServer.Size = new Size(121, 21);
			this.edProxyServer.TabIndex = 1;
			this.edProxyServer.Text = "";
			this.edProxyPort.Location = new Point(80, 72);
			this.edProxyPort.Name = "edProxyPort";
			this.edProxyPort.Size = new Size(121, 21);
			this.edProxyPort.TabIndex = 2;
			this.edProxyPort.Text = "";
			this.edProxyLogin.Location = new Point(80, 96);
			this.edProxyLogin.Name = "edProxyLogin";
			this.edProxyLogin.Size = new Size(121, 21);
			this.edProxyLogin.TabIndex = 3;
			this.edProxyLogin.Text = "";
			this.edProxyPass.Location = new Point(80, 120);
			this.edProxyPass.Name = "edProxyPass";
			this.edProxyPass.PasswordChar = '*';
			this.edProxyPass.Size = new Size(121, 21);
			this.edProxyPass.TabIndex = 4;
			this.edProxyPass.Text = "edProxyPass";
			this.GroupBox7.Controls.Add(this.chkShowOnStart);
			this.GroupBox7.Location = new Point(232, 8);
			this.GroupBox7.Name = "GroupBox7";
			this.GroupBox7.Size = new Size(265, 41);
			this.GroupBox7.TabIndex = 2;
			this.GroupBox7.TabStop = false;
			this.GroupBox7.Text = "Подсказки";
			this.chkShowOnStart.Location = new Point(16, 16);
			this.chkShowOnStart.Name = "chkShowOnStart";
			this.chkShowOnStart.Size = new Size(225, 17);
			this.chkShowOnStart.TabIndex = 0;
			this.chkShowOnStart.Text = "Показывать при старте";
			this.rgEditMode.Controls.Add(this.RButton4);
			this.rgEditMode.Controls.Add(this.RButton3);
			this.rgEditMode.Location = new Point(8, 64);
			this.rgEditMode.Name = "rgEditMode";
			this.rgEditMode.Size = new Size(217, 57);
			this.rgEditMode.TabIndex = 3;
			this.rgEditMode.TabStop = false;
			this.rgEditMode.Text = "Режим работы";
			this.RButton4.Location = new Point(88, 24);
			this.RButton4.Name = "RButton4";
			this.RButton4.Size = new Size(96, 24);
			this.RButton4.TabIndex = 1;
			this.RButton4.Text = "Продвинутый";
			this.RButton3.Location = new Point(8, 24);
			this.RButton3.Name = "RButton3";
			this.RButton3.Size = new Size(80, 24);
			this.RButton3.TabIndex = 0;
			this.RButton3.Text = "Простой";
			this.cbLanguages.DropDownStyle = ComboBoxStyle.DropDownList;
			this.cbLanguages.Location = new Point(312, 56);
			this.cbLanguages.Name = "cbLanguages";
			this.cbLanguages.Size = new Size(164, 21);
			this.cbLanguages.TabIndex = 4;
			this.SheetTree.Controls.Add(this.GroupBox1);
			this.SheetTree.Controls.Add(this.GroupBox2);
			this.SheetTree.Location = new Point(4, 22);
			this.SheetTree.Name = "SheetTree";
			this.SheetTree.Size = new Size(505, 351);
			this.SheetTree.TabIndex = 2;
			this.SheetTree.Text = "Родословные древа";
			this.GroupBox1.Controls.Add(this.chkFamily);
			this.GroupBox1.Controls.Add(this.chkName);
			this.GroupBox1.Controls.Add(this.chkPatronymic);
			this.GroupBox1.Controls.Add(this.chkDiffLines);
			this.GroupBox1.Controls.Add(this.chkBirthDate);
			this.GroupBox1.Controls.Add(this.chkDeathDate);
			this.GroupBox1.Controls.Add(this.chkKinship);
			this.GroupBox1.Controls.Add(this.chkOnlyYears);
			this.GroupBox1.Controls.Add(this.chkSignsVisible);
			this.GroupBox1.Controls.Add(this.chkChildlessExclude);
			this.GroupBox1.Controls.Add(this.chkTreeDecorative);
			this.GroupBox1.Controls.Add(this.chkPortraitsVisible);
			this.GroupBox1.Location = new Point(8, 8);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new Size(289, 225);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			this.GroupBox1.Text = "Отображение персон в древе";
			this.chkFamily.Location = new Point(16, 16);
			this.chkFamily.Name = "chkFamily";
			this.chkFamily.Size = new Size(249, 17);
			this.chkFamily.TabIndex = 0;
			this.chkFamily.Text = "Фамилия";
			this.chkName.Location = new Point(16, 32);
			this.chkName.Name = "chkName";
			this.chkName.Size = new Size(249, 17);
			this.chkName.TabIndex = 1;
			this.chkName.Text = "Имя";
			this.chkPatronymic.Location = new Point(16, 48);
			this.chkPatronymic.Name = "chkPatronymic";
			this.chkPatronymic.Size = new Size(249, 17);
			this.chkPatronymic.TabIndex = 2;
			this.chkPatronymic.Text = "Отчество";
			this.chkDiffLines.Location = new Point(16, 64);
			this.chkDiffLines.Name = "chkDiffLines";
			this.chkDiffLines.Size = new Size(249, 17);
			this.chkDiffLines.TabIndex = 3;
			this.chkDiffLines.Text = "Разные строки (имя и отчество)";
			this.chkBirthDate.Location = new Point(16, 80);
			this.chkBirthDate.Name = "chkBirthDate";
			this.chkBirthDate.Size = new Size(249, 17);
			this.chkBirthDate.TabIndex = 4;
			this.chkBirthDate.Text = "Дата рождения";
			this.chkDeathDate.Location = new Point(16, 96);
			this.chkDeathDate.Name = "chkDeathDate";
			this.chkDeathDate.Size = new Size(249, 17);
			this.chkDeathDate.TabIndex = 5;
			this.chkDeathDate.Text = "Дата смерти";
			this.chkKinship.Location = new Point(16, 128);
			this.chkKinship.Name = "chkKinship";
			this.chkKinship.Size = new Size(249, 17);
			this.chkKinship.TabIndex = 7;
			this.chkKinship.Text = "Степень родства";
			this.chkOnlyYears.Location = new Point(32, 112);
			this.chkOnlyYears.Name = "chkOnlyYears";
			this.chkOnlyYears.Size = new Size(233, 17);
			this.chkOnlyYears.TabIndex = 6;
			this.chkOnlyYears.Text = "Только годы";
			this.chkSignsVisible.Location = new Point(16, 144);
			this.chkSignsVisible.Name = "chkSignsVisible";
			this.chkSignsVisible.Size = new Size(249, 17);
			this.chkSignsVisible.TabIndex = 8;
			this.chkSignsVisible.Text = "Дополнительные символы";
			this.chkChildlessExclude.Location = new Point(16, 200);
			this.chkChildlessExclude.Name = "chkChildlessExclude";
			this.chkChildlessExclude.Size = new Size(249, 17);
			this.chkChildlessExclude.TabIndex = 11;
			this.chkChildlessExclude.Text = "Исключить умерших в детстве";
			this.chkTreeDecorative.Location = new Point(16, 160);
			this.chkTreeDecorative.Name = "chkTreeDecorative";
			this.chkTreeDecorative.Size = new Size(249, 17);
			this.chkTreeDecorative.TabIndex = 9;
			this.chkTreeDecorative.Text = "Декоративное оформление";
			this.chkPortraitsVisible.Location = new Point(16, 176);
			this.chkPortraitsVisible.Name = "chkPortraitsVisible";
			this.chkPortraitsVisible.Size = new Size(249, 17);
			this.chkPortraitsVisible.TabIndex = 10;
			this.chkPortraitsVisible.Text = "Отображать портреты";
			this.GroupBox2.Controls.Add(this.Label5);
			this.GroupBox2.Controls.Add(this.PanMaleColor);
			this.GroupBox2.Controls.Add(this.PanFemaleColor);
			this.GroupBox2.Controls.Add(this.PanUnkSexColor);
			this.GroupBox2.Controls.Add(this.PanUnHusbandColor);
			this.GroupBox2.Controls.Add(this.PanUnWifeColor);
			this.GroupBox2.Controls.Add(this.PanDefFont);
			this.GroupBox2.Location = new Point(312, 8);
			this.GroupBox2.Name = "GroupBox2";
			this.GroupBox2.Size = new Size(185, 193);
			this.GroupBox2.TabIndex = 1;
			this.GroupBox2.TabStop = false;
			this.GroupBox2.Text = "Оформление";
			this.Label5.Location = new Point(16, 144);
			this.Label5.Name = "Label5";
			this.Label5.Size = new Size(50, 13);
			this.Label5.TabIndex = 0;
			this.Label5.Text = "Шрифт";
			this.PanMaleColor.BackColor = SystemColors.Control;
			this.PanMaleColor.BorderStyle = BorderStyle.FixedSingle;
			this.PanMaleColor.Cursor = Cursors.Hand;
			this.PanMaleColor.Location = new Point(16, 16);
			this.PanMaleColor.Name = "PanMaleColor";
			this.PanMaleColor.Size = new Size(73, 25);
			this.PanMaleColor.TabIndex = 0;
			this.PanMaleColor.Text = "Мужчина";
			this.PanMaleColor.Click += new EventHandler(this.PanColor_Click);
			this.PanFemaleColor.BorderStyle = BorderStyle.FixedSingle;
			this.PanFemaleColor.Cursor = Cursors.Hand;
			this.PanFemaleColor.Location = new Point(96, 16);
			this.PanFemaleColor.Name = "PanFemaleColor";
			this.PanFemaleColor.Size = new Size(73, 25);
			this.PanFemaleColor.TabIndex = 1;
			this.PanFemaleColor.Text = "Женщина";
			this.PanFemaleColor.Click += new EventHandler(this.PanColor_Click);
			this.PanUnkSexColor.BorderStyle = BorderStyle.FixedSingle;
			this.PanUnkSexColor.Cursor = Cursors.Hand;
			this.PanUnkSexColor.Location = new Point(16, 48);
			this.PanUnkSexColor.Name = "PanUnkSexColor";
			this.PanUnkSexColor.Size = new Size(153, 25);
			this.PanUnkSexColor.TabIndex = 2;
			this.PanUnkSexColor.Text = "Неизвестный пол";
			this.PanUnkSexColor.Click += new EventHandler(this.PanColor_Click);
			this.PanUnHusbandColor.BorderStyle = BorderStyle.FixedSingle;
			this.PanUnHusbandColor.Cursor = Cursors.Hand;
			this.PanUnHusbandColor.Location = new Point(16, 80);
			this.PanUnHusbandColor.Name = "PanUnHusbandColor";
			this.PanUnHusbandColor.Size = new Size(153, 25);
			this.PanUnHusbandColor.TabIndex = 3;
			this.PanUnHusbandColor.Text = "Разведенный супруг";
			this.PanUnHusbandColor.Click += new EventHandler(this.PanColor_Click);
			this.PanUnWifeColor.BorderStyle = BorderStyle.FixedSingle;
			this.PanUnWifeColor.Cursor = Cursors.Hand;
			this.PanUnWifeColor.Location = new Point(16, 112);
			this.PanUnWifeColor.Name = "PanUnWifeColor";
			this.PanUnWifeColor.Size = new Size(153, 25);
			this.PanUnWifeColor.TabIndex = 4;
			this.PanUnWifeColor.Text = "Разведенная супруга";
			this.PanUnWifeColor.Click += new EventHandler(this.PanColor_Click);
			this.PanDefFont.BorderStyle = BorderStyle.FixedSingle;
			this.PanDefFont.Cursor = Cursors.Hand;
			this.PanDefFont.Location = new Point(16, 160);
			this.PanDefFont.Name = "PanDefFont";
			this.PanDefFont.Size = new Size(153, 25);
			this.PanDefFont.TabIndex = 5;
			this.PanDefFont.Click += new EventHandler(this.PanDefFont_Click);
			this.SheetView.Controls.Add(this.PageControl2);
			this.SheetView.Location = new Point(4, 22);
			this.SheetView.Name = "SheetView";
			this.SheetView.Size = new Size(505, 351);
			this.SheetView.TabIndex = 1;
			this.SheetView.Text = "Интерфейс";
			this.PageControl2.Controls.Add(this.SheetViewCommon);
			this.PageControl2.Controls.Add(this.SheetViewPersons);
			this.PageControl2.Location = new Point(0, 0);
			this.PageControl2.Name = "PageControl2";
			this.PageControl2.SelectedIndex = 0;
			this.PageControl2.Size = new Size(505, 349);
			this.PageControl2.TabIndex = 0;
			this.SheetViewCommon.Controls.Add(this.rgFNPFormat);
			this.SheetViewCommon.Controls.Add(this.rgDateFormat);
			this.SheetViewCommon.Controls.Add(this.chkPlacesWithAddress);
			this.SheetViewCommon.Controls.Add(this.chkHighlightUnparented);
			this.SheetViewCommon.Controls.Add(this.chkHighlightUnmarried);
			this.SheetViewCommon.Location = new Point(4, 22);
			this.SheetViewCommon.Name = "SheetViewCommon";
			this.SheetViewCommon.Size = new Size(497, 323);
			this.SheetViewCommon.TabIndex = 0;
			this.SheetViewCommon.Text = "Все списки";
			this.rgFNPFormat.Controls.Add(this.RButton7);
			this.rgFNPFormat.Controls.Add(this.RButton6);
			this.rgFNPFormat.Controls.Add(this.RButton5);
			this.rgFNPFormat.Location = new Point(8, 8);
			this.rgFNPFormat.Name = "rgFNPFormat";
			this.rgFNPFormat.Size = new Size(185, 97);
			this.rgFNPFormat.TabIndex = 0;
			this.rgFNPFormat.TabStop = false;
			this.rgFNPFormat.Text = "Формат имен в списках";
			this.RButton7.Location = new Point(8, 64);
			this.RButton7.Name = "RButton7";
			this.RButton7.Size = new Size(160, 24);
			this.RButton7.TabIndex = 2;
			this.RButton7.Text = "Фамилия; Имя; Отчество";
			this.RButton6.Location = new Point(8, 40);
			this.RButton6.Name = "RButton6";
			this.RButton6.Size = new Size(160, 24);
			this.RButton6.TabIndex = 1;
			this.RButton6.Text = "Фамилия; Имя_Отчество";
			this.RButton5.Location = new Point(8, 17);
			this.RButton5.Name = "RButton5";
			this.RButton5.Size = new Size(160, 24);
			this.RButton5.TabIndex = 0;
			this.RButton5.Text = "Фамилия_Имя_Отчество";
			this.rgDateFormat.Controls.Add(this.RButton9);
			this.rgDateFormat.Controls.Add(this.RButton8);
			this.rgDateFormat.Location = new Point(8, 112);
			this.rgDateFormat.Name = "rgDateFormat";
			this.rgDateFormat.Size = new Size(185, 72);
			this.rgDateFormat.TabIndex = 1;
			this.rgDateFormat.TabStop = false;
			this.rgDateFormat.Text = "Формат даты в списках";
			this.RButton9.Location = new Point(8, 40);
			this.RButton9.Name = "RButton9";
			this.RButton9.TabIndex = 1;
			this.RButton9.Text = "YYYY.MM.DD";
			this.RButton8.Location = new Point(8, 16);
			this.RButton8.Name = "RButton8";
			this.RButton8.TabIndex = 0;
			this.RButton8.Text = "DD.MM.YYYY";
			this.chkPlacesWithAddress.Location = new Point(8, 200);
			this.chkPlacesWithAddress.Name = "chkPlacesWithAddress";
			this.chkPlacesWithAddress.Size = new Size(185, 17);
			this.chkPlacesWithAddress.TabIndex = 2;
			this.chkPlacesWithAddress.Text = "Включать адрес в строки мест";
			this.chkHighlightUnparented.Location = new Point(8, 224);
			this.chkHighlightUnparented.Name = "chkHighlightUnparented";
			this.chkHighlightUnparented.Size = new Size(241, 17);
			this.chkHighlightUnparented.TabIndex = 3;
			this.chkHighlightUnparented.Text = "Подсвечивать персоны без родителей";
			this.chkHighlightUnmarried.Location = new Point(8, 248);
			this.chkHighlightUnmarried.Name = "chkHighlightUnmarried";
			this.chkHighlightUnmarried.Size = new Size(241, 17);
			this.chkHighlightUnmarried.TabIndex = 4;
			this.chkHighlightUnmarried.Text = "Подсвечивать персоны без семьи";
			this.SheetViewPersons.Controls.Add(this.btnColumnUp);
			this.SheetViewPersons.Controls.Add(this.btnColumnDown);
			this.SheetViewPersons.Controls.Add(this.ListPersonColumns);
			this.SheetViewPersons.Controls.Add(this.btnDefList);
			this.SheetViewPersons.Location = new Point(4, 22);
			this.SheetViewPersons.Name = "SheetViewPersons";
			this.SheetViewPersons.Size = new Size(497, 323);
			this.SheetViewPersons.TabIndex = 1;
			this.SheetViewPersons.Text = "Список персон";
			this.btnColumnUp.Location = new Point(352, 8);
			this.btnColumnUp.Name = "btnColumnUp";
			this.btnColumnUp.Size = new Size(24, 24);
			this.btnColumnUp.TabIndex = 0;
			this.btnColumnUp.Click += new EventHandler(this.btnColumnUp_Click);
			this.btnColumnDown.Location = new Point(352, 40);
			this.btnColumnDown.Name = "btnColumnDown";
			this.btnColumnDown.Size = new Size(24, 24);
			this.btnColumnDown.TabIndex = 1;
			this.btnColumnDown.Click += new EventHandler(this.btnColumnDown_Click);
			this.ListPersonColumns.Location = new Point(8, 8);
			this.ListPersonColumns.Name = "ListPersonColumns";
			this.ListPersonColumns.Size = new Size(337, 292);
			this.ListPersonColumns.TabIndex = 0;
			this.ListPersonColumns.ItemCheck += new ItemCheckEventHandler(this.ListPersonColumns_ItemCheck);
			this.btnDefList.Location = new Point(352, 288);
			this.btnDefList.Name = "btnDefList";
			this.btnDefList.Size = new Size(137, 25);
			this.btnDefList.TabIndex = 1;
			this.btnDefList.Text = "Значения по умолчанию";
			this.btnDefList.Click += new EventHandler(this.btnDefList_Click);
			this.SheetPedigree.Controls.Add(this.GroupBox5);
			this.SheetPedigree.Location = new Point(4, 22);
			this.SheetPedigree.Name = "SheetPedigree";
			this.SheetPedigree.Size = new Size(505, 351);
			this.SheetPedigree.TabIndex = 3;
			this.SheetPedigree.Text = "Росписи";
			this.GroupBox5.Controls.Add(this.chkAttributes);
			this.GroupBox5.Controls.Add(this.chkNotes);
			this.GroupBox5.Controls.Add(this.chkSources);
			this.GroupBox5.Controls.Add(this.EditPedigreeFormat);
			this.GroupBox5.Location = new Point(8, 8);
			this.GroupBox5.Name = "GroupBox5";
			this.GroupBox5.Size = new Size(289, 160);
			this.GroupBox5.TabIndex = 0;
			this.GroupBox5.TabStop = false;
			this.GroupBox5.Text = "Генерация росписей";
			this.chkAttributes.Location = new Point(16, 16);
			this.chkAttributes.Name = "chkAttributes";
			this.chkAttributes.Size = new Size(249, 17);
			this.chkAttributes.TabIndex = 0;
			this.chkAttributes.Text = "Включая атрибуты персон";
			this.chkNotes.Location = new Point(16, 32);
			this.chkNotes.Name = "chkNotes";
			this.chkNotes.Size = new Size(249, 17);
			this.chkNotes.TabIndex = 1;
			this.chkNotes.Text = "Включая заметки";
			this.chkSources.Location = new Point(16, 48);
			this.chkSources.Name = "chkSources";
			this.chkSources.Size = new Size(249, 17);
			this.chkSources.TabIndex = 2;
			this.chkSources.Text = "Включая источники";
			this.EditPedigreeFormat.Controls.Add(this.RButton10);
			this.EditPedigreeFormat.Controls.Add(this.RButton11);
			this.EditPedigreeFormat.Location = new Point(16, 72);
			this.EditPedigreeFormat.Name = "EditPedigreeFormat";
			this.EditPedigreeFormat.Size = new Size(249, 72);
			this.EditPedigreeFormat.TabIndex = 3;
			this.EditPedigreeFormat.TabStop = false;
			this.EditPedigreeFormat.Text = "Формат";
			this.RButton10.Location = new Point(16, 16);
			this.RButton10.Name = "RButton10";
			this.RButton10.TabIndex = 3;
			this.RButton10.Text = "Избыточный";
			this.RButton11.Location = new Point(16, 40);
			this.RButton11.Name = "RButton11";
			this.RButton11.TabIndex = 2;
			this.RButton11.Text = "Традиционный";
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(336, 392);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 1;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(424, 392);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(513, 425);
			base.Controls.Add(this.PageControl1);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmOptions";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Настройки";
			this.PageControl1.ResumeLayout(false);
			this.SheetCommon.ResumeLayout(false);
			this.rgCode.ResumeLayout(false);
			this.GroupBox4.ResumeLayout(false);
			this.GroupBox7.ResumeLayout(false);
			this.rgEditMode.ResumeLayout(false);
			this.SheetTree.ResumeLayout(false);
			this.GroupBox1.ResumeLayout(false);
			this.GroupBox2.ResumeLayout(false);
			this.SheetView.ResumeLayout(false);
			this.PageControl2.ResumeLayout(false);
			this.SheetViewCommon.ResumeLayout(false);
			this.rgFNPFormat.ResumeLayout(false);
			this.rgDateFormat.ResumeLayout(false);
			this.SheetViewPersons.ResumeLayout(false);
			this.SheetPedigree.ResumeLayout(false);
			this.GroupBox5.ResumeLayout(false);
			this.EditPedigreeFormat.ResumeLayout(false);
			base.ResumeLayout(false);
		}
		private void PanColor_Click(object sender, EventArgs e)
		{
			this.ColorDialog1.Color = (sender as Panel).BackColor;
			if (this.ColorDialog1.ShowDialog() == DialogResult.OK)
			{
				(sender as Panel).BackColor = this.ColorDialog1.Color;
			}
		}
		private void PanDefFont_Click(object sender, EventArgs e)
		{
			if (this.FontDialog1.ShowDialog() == DialogResult.OK)
			{
				this.FOptions.ChartOptions.DefFont_Name = this.FontDialog1.Font.Name;
				this.FOptions.ChartOptions.DefFont_Size = (int)checked((long)Math.Round((double)this.FontDialog1.Font.Size));
			}
			this.UpdateControls();
		}
		private void btnAccept_Click(object sender, EventArgs e)
		{
			this.FOptions.ListPersonsColumns = this.FPersonColumns;
			if (this.RButton1.Checked)
			{
				this.FOptions.DefCharacterSet = TGEDCOMObject.TGEDCOMCharacterSet.csASCII;
			}
			else
			{
				if (this.RButton2.Checked)
				{
					this.FOptions.DefCharacterSet = TGEDCOMObject.TGEDCOMCharacterSet.csUTF8;
				}
			}
			if (this.RButton5.Checked)
			{
				this.FOptions.DefNameFormat = TGenEngine.TNameFormat.nfFNP;
			}
			else
			{
				if (this.RButton6.Checked)
				{
					this.FOptions.DefNameFormat = TGenEngine.TNameFormat.nfF_NP;
				}
				else
				{
					if (this.RButton7.Checked)
					{
						this.FOptions.DefNameFormat = TGenEngine.TNameFormat.nfF_N_P;
					}
				}
			}
			if (this.RButton8.Checked)
			{
				this.FOptions.DefDateFormat = TGenEngine.TDateFormat.dfDD_MM_YYYY;
			}
			else
			{
				if (this.RButton9.Checked)
				{
					this.FOptions.DefDateFormat = TGenEngine.TDateFormat.dfYYYY_MM_DD;
				}
			}
			this.FOptions.PlacesWithAddress = this.chkPlacesWithAddress.Checked;
			this.FOptions.ListPersons_HighlightUnparented = this.chkHighlightUnparented.Checked;
			this.FOptions.ListPersons_HighlightUnmarried = this.chkHighlightUnmarried.Checked;
			this.FOptions.ChartOptions.FamilyVisible = this.chkFamily.Checked;
			this.FOptions.ChartOptions.NameVisible = this.chkName.Checked;
			this.FOptions.ChartOptions.PatronymicVisible = this.chkPatronymic.Checked;
			this.FOptions.ChartOptions.DiffLines = this.chkDiffLines.Checked;
			this.FOptions.ChartOptions.BirthDateVisible = this.chkBirthDate.Checked;
			this.FOptions.ChartOptions.DeathDateVisible = this.chkDeathDate.Checked;
			this.FOptions.ChartOptions.OnlyYears = this.chkOnlyYears.Checked;
			this.FOptions.ChartOptions.Kinship = this.chkKinship.Checked;
			this.FOptions.ChartOptions.SignsVisible = this.chkSignsVisible.Checked;
			this.FOptions.ChartOptions.ChildlessExclude = this.chkChildlessExclude.Checked;
			this.FOptions.ChartOptions.Decorative = this.chkTreeDecorative.Checked;
			this.FOptions.ChartOptions.PortraitsVisible = this.chkPortraitsVisible.Checked;
			this.FOptions.ChartOptions.MaleColor = this.PanMaleColor.BackColor;
			this.FOptions.ChartOptions.FemaleColor = this.PanFemaleColor.BackColor;
			this.FOptions.ChartOptions.UnkSexColor = this.PanUnkSexColor.BackColor;
			this.FOptions.ChartOptions.UnHusbandColor = this.PanUnHusbandColor.BackColor;
			this.FOptions.ChartOptions.UnWifeColor = this.PanUnWifeColor.BackColor;
			this.FOptions.Proxy.UseProxy = this.chkProxy.Checked;
			this.FOptions.Proxy.Server = this.edProxyServer.Text;
			this.FOptions.Proxy.Port = this.edProxyPort.Text;
			this.FOptions.Proxy.Login = this.edProxyLogin.Text;
			this.FOptions.Proxy.Password = this.edProxyPass.Text;
			this.FOptions.PedigreeOptions.IncludeAttributes = this.chkAttributes.Checked;
			this.FOptions.PedigreeOptions.IncludeNotes = this.chkNotes.Checked;
			this.FOptions.PedigreeOptions.IncludeSources = this.chkSources.Checked;
			if (this.RButton10.Checked)
			{
				this.FOptions.PedigreeOptions.Format = TPedigreeOptions.TPedigreeFormat.pfExcess;
			}
			else
			{
				if (this.RButton11.Checked)
				{
					this.FOptions.PedigreeOptions.Format = TPedigreeOptions.TPedigreeFormat.pfCompact;
				}
			}
			this.FOptions.ShowTips = this.chkShowOnStart.Checked;
			if (this.RButton3.Checked)
			{
				this.FOptions.WorkMode = TGlobalOptions.TWorkMode.wmSimple;
			}
			else
			{
				if (this.RButton4.Checked)
				{
					this.FOptions.WorkMode = TGlobalOptions.TWorkMode.wmExpert;
				}
			}
			int code = (this.cbLanguages.Items[this.cbLanguages.SelectedIndex] as TTaggedComboItem).Tag;
			GKL.fmGEDKeeper.LoadLanguage(code);
			base.DialogResult = DialogResult.OK;
		}
		private void btnColumnUp_Click(object sender, EventArgs e)
		{
			int idx = this.ListPersonColumns.SelectedIndex;
			if (idx > 0)
			{
				TGlobalOptions.TPersonColumnProps props = this.FPersonColumns[idx - 1];
				this.FPersonColumns[idx - 1] = this.FPersonColumns[idx];
				this.FPersonColumns[idx] = props;
				this.UpdateColumnsList();
				this.ListPersonColumns.SelectedIndex = idx - 1;
			}
		}
		private void btnColumnDown_Click(object sender, EventArgs e)
		{
			int idx = this.ListPersonColumns.SelectedIndex;
			if (idx >= 0 && idx < 23)
			{
				TGlobalOptions.TPersonColumnProps props = this.FPersonColumns[idx + 1];
				this.FPersonColumns[idx + 1] = this.FPersonColumns[idx];
				this.FPersonColumns[idx] = props;
				this.UpdateColumnsList();
				this.ListPersonColumns.SelectedIndex = idx + 1;
			}
		}
		private void btnDefList_Click(object sender, EventArgs e)
		{
			Array.Copy(TGlobalOptions.DefPersonColumns, this.FPersonColumns, 24);
			this.UpdateColumnsList();
		}

		private void ListPersonColumns_ItemCheck(object sender, ItemCheckEventArgs e)
		{
			bool cs = (e.NewValue == CheckState.Checked);
			this.FPersonColumns[e.Index].colActive = cs;
		}

		public TfmOptions()
		{
			this.InitializeComponent();
			this.FOptions = GKL.fmGEDKeeper.Options;
			//(ILocalization)(this).SetLang(); checkit
			this.UpdateForm();
		}

		void ILocalization.SetLang()
		{
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Text = GKL.LSList[39];
			this.SheetCommon.Text = GKL.LSList[144];
			this.SheetView.Text = GKL.LSList[249];
			this.SheetTree.Text = GKL.LSList[250];
			this.SheetPedigree.Text = GKL.LSList[251];
			this.rgCode.Text = GKL.LSList[252];
			this.rgEditMode.Text = GKL.LSList[253];
			this.RButton3.Text = GKL.LSList[254];
			this.RButton4.Text = GKL.LSList[255];
			this.GroupBox4.Text = GKL.LSList[256];
			this.chkProxy.Text = GKL.LSList[257];
			this.Label1.Text = GKL.LSList[258];
			this.Label2.Text = GKL.LSList[259];
			this.Label3.Text = GKL.LSList[260];
			this.Label4.Text = GKL.LSList[261];
			this.GroupBox7.Text = GKL.LSList[262];
			this.chkShowOnStart.Text = GKL.LSList[263];
			this.Label6.Text = GKL.LSList[264];
			this.SheetViewCommon.Text = GKL.LSList[265];
			this.SheetViewPersons.Text = GKL.LSList[266];
			this.rgFNPFormat.Text = GKL.LSList[267];
			this.RButton5.Text = GKL.LSList[268];
			this.RButton6.Text = GKL.LSList[269];
			this.RButton7.Text = GKL.LSList[270];
			this.rgDateFormat.Text = GKL.LSList[271];
			this.chkPlacesWithAddress.Text = GKL.LSList[272];
			this.chkHighlightUnparented.Text = GKL.LSList[273];
			this.chkHighlightUnmarried.Text = GKL.LSList[274];
			this.btnDefList.Text = GKL.LSList[275];
			this.GroupBox1.Text = GKL.LSList[276];
			this.chkFamily.Text = GKL.LSList[84];
			this.chkName.Text = GKL.LSList[85];
			this.chkPatronymic.Text = GKL.LSList[86];
			this.chkDiffLines.Text = GKL.LSList[277];
			this.chkBirthDate.Text = GKL.LSList[122];
			this.chkDeathDate.Text = GKL.LSList[123];
			this.chkOnlyYears.Text = GKL.LSList[278];
			this.chkKinship.Text = GKL.LSList[279];
			this.chkSignsVisible.Text = GKL.LSList[280];
			this.chkTreeDecorative.Text = GKL.LSList[281];
			this.chkPortraitsVisible.Text = GKL.LSList[282];
			this.chkChildlessExclude.Text = GKL.LSList[283];
			this.GroupBox2.Text = GKL.LSList[284];
			this.PanMaleColor.Text = GKL.LSList[285];
			this.PanFemaleColor.Text = GKL.LSList[286];
			this.PanUnkSexColor.Text = GKL.LSList[287];
			this.PanUnHusbandColor.Text = GKL.LSList[288];
			this.PanUnWifeColor.Text = GKL.LSList[289];
			this.Label5.Text = GKL.LSList[290];
			this.GroupBox5.Text = GKL.LSList[291];
			this.chkAttributes.Text = GKL.LSList[292];
			this.chkNotes.Text = GKL.LSList[293];
			this.chkSources.Text = GKL.LSList[294];
			this.EditPedigreeFormat.Text = GKL.LSList[295];
			this.RButton10.Text = GKL.LSList[296];
			this.RButton11.Text = GKL.LSList[297];
		}
	}
}
