using GedCom551;
using GKCore;
using GKSys;
using System;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmPersonScan : Form
	{
		private enum TPersonLink : byte
		{
			plNone,
			plPerson,
			plFather,
			plMother,
			plGodparent,
			plSpouse,
			plChild
		}

		private static readonly LSID[] PersonLinks;
		private Button btnParse;
		private Button btnClose;
		private TabControl PageControl1;
		private TabPage tsSimpleInput;
		private TabPage tsSourceInput;
		private Label Label1;
		private Label Label2;
		private Button btnMale;
		private Button btnFemale;
		private TextBox EditName;
		private TextBox MemoNote;
		private Panel Panel1;
		private Label Label3;
		private Label Label5;
		private MaskedTextBox EditBirthDate;
		private TextBox EditBirthPlace;
		private CheckBox CheckBirth;
		private Panel Panel2;
		private Label Label6;
		private Label Label7;
		private CheckBox CheckDeath;
		private MaskedTextBox EditDeathDate;
		private TextBox EditDeathPlace;
		private Label Label4;
		private Label Label8;
		private Label Label9;
		private Label Label10;
		private ComboBox cbSource;
		private TextBox edPage;
		private TextBox edSourceYear;
		private TextBox edPlace;
		private ComboBox cbPersonLink;
		private GroupBox rgSourceKind;
		private GroupBox gbMetrics;
		private Label Label11;
		private Label Label12;
		private MaskedTextBox edEventDate;
		private ComboBox cbEventType;
		private Panel sgData;
		private TfmBase FBase;
		private TStringList FSourcesList;
		[Browsable(false)]
		public TfmBase Base
		{
			get
			{
				return this.FBase;
			}
		}
		private static TfmPersonScan.TPersonLink GetLinkByName([In] string aName)
		{
			TfmPersonScan.TPersonLink pl = TfmPersonScan.TPersonLink.plPerson;
			TfmPersonScan.TPersonLink Result;
			while (BDSSystem.WStrCmp(GKL.LSList[(int)TfmPersonScan.PersonLinks[(int)pl] - 1], aName) != 0)
			{
				pl++;
				if (pl == (TfmPersonScan.TPersonLink)7)
				{
					Result = TfmPersonScan.TPersonLink.plNone;
					return Result;
				}
			}
			Result = pl;
			return Result;
		}
		private bool CheckCell(int ACol, int ARow)
		{
			bool Result = true;
			return Result;
		}
		private void InitGrid()
		{
		}
		private void InitSimpleControls()
		{
			this.EditName.Text = "";
			this.EditBirthDate.Text = "";
			this.EditBirthPlace.Text = "";
			this.CheckBirth.Checked = false;
			this.EditDeathDate.Text = "";
			this.EditDeathPlace.Text = "";
			this.CheckDeath.Checked = false;
			this.MemoNote.Text = "";
		}
		private void InitSourceControls()
		{
			this.Base.Engine.GetSourcesList(this.FSourcesList);
			this.edPage.Text = "";
			this.edSourceYear.Text = "";
			this.edPlace.Text = "";
			this.edEventDate.Text = "";
			this.cbEventType.SelectedIndex = -1;
		}
		private void ParseSimple()
		{
			string tmp = this.EditName.Text.ToLower();
			int tokCount = TGKSys.GetTokensCount(tmp, ' ');
			if (tokCount < 3)
			{
				TGKSys.ShowError(GKL.LSList[506]);
			}
			else
			{
				string fam = TGKSys.SetAsName(TGKSys.GetToken(tmp, ' ', 1));
				string nam = TGKSys.SetAsName(TGKSys.GetToken(tmp, ' ', 2));
				string pat = TGKSys.SetAsName(TGKSys.GetToken(tmp, ' ', 3));
				TGEDCOMObject.TGEDCOMSex sx = TGEDCOMObject.TGEDCOMSex.svNone;
				TGEDCOMIndividualRecord iRec = TGenEngine.CreatePersonEx(this.Base.Tree, nam, pat, fam, sx, false);
				this.Base.ChangeRecord(iRec);
				if (this.CheckBirth.Checked)
				{
					TGenEngine.CreateEventEx(this.Base.Tree, iRec, "BIRT", TGenEngine.StrToGEDCOMDate(this.EditBirthDate.Text, true), this.EditBirthPlace.Text);
				}
				if (this.CheckDeath.Checked)
				{
					TGenEngine.CreateEventEx(this.Base.Tree, iRec, "DEAT", TGenEngine.StrToGEDCOMDate(this.EditDeathDate.Text, true), this.EditDeathPlace.Text);
				}
				this.InitSimpleControls();
			}
		}
		private void ParseSource()
		{
			string src_name = this.cbSource.Text;
			string src_page = this.edPage.Text;
			if (!TGKSys.IsDigits(this.edSourceYear.Text))
			{
				TGKSys.ShowError(GKL.LSList[508]);
			}
			else
			{
				int.Parse(this.edSourceYear.Text);
				string place = this.edPlace.Text;
				try
				{
				}
				finally
				{
				}
				this.InitSourceControls();
			}
		}
		private void InitializeComponent()
		{
			this.btnParse = new Button();
			this.btnClose = new Button();
			this.PageControl1 = new TabControl();
			this.tsSimpleInput = new TabPage();
			this.Label1 = new Label();
			this.Label2 = new Label();
			this.btnMale = new Button();
			this.btnFemale = new Button();
			this.EditName = new TextBox();
			this.MemoNote = new TextBox();
			this.Panel1 = new Panel();
			this.Label3 = new Label();
			this.Label5 = new Label();
			this.EditBirthDate = new MaskedTextBox();
			this.EditBirthPlace = new TextBox();
			this.CheckBirth = new CheckBox();
			this.Panel2 = new Panel();
			this.Label6 = new Label();
			this.Label7 = new Label();
			this.CheckDeath = new CheckBox();
			this.EditDeathDate = new MaskedTextBox();
			this.EditDeathPlace = new TextBox();
			this.tsSourceInput = new TabPage();
			this.Label4 = new Label();
			this.Label8 = new Label();
			this.Label9 = new Label();
			this.Label10 = new Label();
			this.cbSource = new ComboBox();
			this.edPage = new TextBox();
			this.edSourceYear = new TextBox();
			this.edPlace = new TextBox();
			this.sgData = new Panel();
			this.cbPersonLink = new ComboBox();
			this.rgSourceKind = new GroupBox();
			this.gbMetrics = new GroupBox();
			this.Label11 = new Label();
			this.Label12 = new Label();
			this.edEventDate = new MaskedTextBox();
			this.cbEventType = new ComboBox();
			this.PageControl1.SuspendLayout();
			this.tsSimpleInput.SuspendLayout();
			this.Panel1.SuspendLayout();
			this.Panel2.SuspendLayout();
			this.tsSourceInput.SuspendLayout();
			this.gbMetrics.SuspendLayout();
			base.SuspendLayout();
			this.btnParse.Location = new Point(464, 424);
			this.btnParse.Name = "btnParse";
			this.btnParse.Size = new Size(81, 25);
			this.btnParse.TabIndex = 0;
			this.btnParse.Text = "Добавить";
			this.btnParse.Click += new EventHandler(this.btnParseClick);
			this.btnClose.Location = new Point(560, 424);
			this.btnClose.Name = "btnClose";
			this.btnClose.Size = new Size(81, 25);
			this.btnClose.TabIndex = 1;
			this.btnClose.Text = "Закрыть";
			this.PageControl1.Controls.Add(this.tsSimpleInput);
			this.PageControl1.Controls.Add(this.tsSourceInput);
			this.PageControl1.Location = new Point(8, 8);
			this.PageControl1.Name = "PageControl1";
			this.PageControl1.SelectedIndex = 0;
			this.PageControl1.Size = new Size(633, 401);
			this.PageControl1.TabIndex = 2;
			this.tsSimpleInput.Controls.Add(this.Label1);
			this.tsSimpleInput.Controls.Add(this.Label2);
			this.tsSimpleInput.Controls.Add(this.btnMale);
			this.tsSimpleInput.Controls.Add(this.btnFemale);
			this.tsSimpleInput.Controls.Add(this.EditName);
			this.tsSimpleInput.Controls.Add(this.MemoNote);
			this.tsSimpleInput.Controls.Add(this.Panel1);
			this.tsSimpleInput.Controls.Add(this.Panel2);
			this.tsSimpleInput.Location = new Point(4, 22);
			this.tsSimpleInput.Name = "tsSimpleInput";
			this.tsSimpleInput.Size = new Size(625, 375);
			this.tsSimpleInput.TabIndex = 0;
			this.tsSimpleInput.Text = "Простой ввод";
			this.Label1.Location = new Point(8, 8);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(150, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Полное имя (формат ФИО)";
			this.Label2.Location = new Point(8, 232);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(50, 13);
			this.Label2.TabIndex = 1;
			this.Label2.Text = "Заметка";
			this.btnMale.Location = new Point(424, 24);
			this.btnMale.Name = "btnMale";
			this.btnMale.Size = new Size(23, 21);
			this.btnMale.TabIndex = 2;
			this.btnMale.Text = "М";
			this.btnFemale.Location = new Point(448, 24);
			this.btnFemale.Name = "btnFemale";
			this.btnFemale.Size = new Size(23, 21);
			this.btnFemale.TabIndex = 3;
			this.btnFemale.Text = "Ж";
			this.EditName.Location = new Point(8, 24);
			this.EditName.Name = "EditName";
			this.EditName.Size = new Size(409, 21);
			this.EditName.TabIndex = 0;
			this.EditName.Text = "";
			this.MemoNote.Location = new Point(8, 245);
			this.MemoNote.Multiline = true;
			this.MemoNote.Name = "MemoNote";
			this.MemoNote.Size = new Size(465, 121);
			this.MemoNote.TabIndex = 1;
			this.MemoNote.Text = "";
			this.Panel1.BorderStyle = BorderStyle.FixedSingle;
			this.Panel1.Controls.Add(this.Label3);
			this.Panel1.Controls.Add(this.Label5);
			this.Panel1.Controls.Add(this.EditBirthDate);
			this.Panel1.Controls.Add(this.EditBirthPlace);
			this.Panel1.Controls.Add(this.CheckBirth);
			this.Panel1.Location = new Point(8, 56);
			this.Panel1.Name = "Panel1";
			this.Panel1.Size = new Size(465, 81);
			this.Panel1.TabIndex = 2;
			this.Label3.Location = new Point(8, 32);
			this.Label3.Name = "Label3";
			this.Label3.Size = new Size(90, 13);
			this.Label3.TabIndex = 0;
			this.Label3.Text = "Дата рождения";
			this.Label5.Location = new Point(112, 32);
			this.Label5.Name = "Label5";
			this.Label5.Size = new Size(100, 13);
			this.Label5.TabIndex = 1;
			this.Label5.Text = "Место рождения";
			this.EditBirthDate.Location = new Point(8, 48);
			this.EditBirthDate.Mask = "00/00/0000";
			this.EditBirthDate.MaxLength = 10;
			this.EditBirthDate.Name = "EditBirthDate";
			this.EditBirthDate.Size = new Size(97, 21);
			this.EditBirthDate.TabIndex = 0;
			this.EditBirthDate.Text = "  .  .    ";
			this.EditBirthPlace.Location = new Point(112, 48);
			this.EditBirthPlace.Name = "EditBirthPlace";
			this.EditBirthPlace.Size = new Size(337, 21);
			this.EditBirthPlace.TabIndex = 1;
			this.EditBirthPlace.Text = "";
			this.CheckBirth.Location = new Point(8, 8);
			this.CheckBirth.Name = "CheckBirth";
			this.CheckBirth.Size = new Size(96, 17);
			this.CheckBirth.TabIndex = 2;
			this.CheckBirth.Text = "Родился";
			this.Panel2.BorderStyle = BorderStyle.FixedSingle;
			this.Panel2.Controls.Add(this.Label6);
			this.Panel2.Controls.Add(this.Label7);
			this.Panel2.Controls.Add(this.CheckDeath);
			this.Panel2.Controls.Add(this.EditDeathDate);
			this.Panel2.Controls.Add(this.EditDeathPlace);
			this.Panel2.Location = new Point(8, 144);
			this.Panel2.Name = "Panel2";
			this.Panel2.Size = new Size(465, 81);
			this.Panel2.TabIndex = 3;
			this.Label6.Location = new Point(8, 32);
			this.Label6.Name = "Label6";
			this.Label6.Size = new Size(90, 13);
			this.Label6.TabIndex = 0;
			this.Label6.Text = "Дата смерти";
			this.Label7.Location = new Point(112, 32);
			this.Label7.Name = "Label7";
			this.Label7.Size = new Size(100, 13);
			this.Label7.TabIndex = 1;
			this.Label7.Text = "Место смерти";
			this.CheckDeath.Location = new Point(8, 8);
			this.CheckDeath.Name = "CheckDeath";
			this.CheckDeath.Size = new Size(95, 17);
			this.CheckDeath.TabIndex = 0;
			this.CheckDeath.Text = "Умер";
			this.EditDeathDate.Location = new Point(8, 48);
			this.EditDeathDate.Mask = "00/00/0000";
			this.EditDeathDate.MaxLength = 10;
			this.EditDeathDate.Name = "EditDeathDate";
			this.EditDeathDate.Size = new Size(97, 21);
			this.EditDeathDate.TabIndex = 1;
			this.EditDeathDate.Text = "  .  .    ";
			this.EditDeathPlace.Location = new Point(112, 48);
			this.EditDeathPlace.Name = "EditDeathPlace";
			this.EditDeathPlace.Size = new Size(337, 21);
			this.EditDeathPlace.TabIndex = 2;
			this.EditDeathPlace.Text = "";
			this.tsSourceInput.Controls.Add(this.Label4);
			this.tsSourceInput.Controls.Add(this.Label8);
			this.tsSourceInput.Controls.Add(this.Label9);
			this.tsSourceInput.Controls.Add(this.Label10);
			this.tsSourceInput.Controls.Add(this.cbSource);
			this.tsSourceInput.Controls.Add(this.edPage);
			this.tsSourceInput.Controls.Add(this.edSourceYear);
			this.tsSourceInput.Controls.Add(this.edPlace);
			this.tsSourceInput.Controls.Add(this.sgData);
			this.tsSourceInput.Controls.Add(this.cbPersonLink);
			this.tsSourceInput.Controls.Add(this.rgSourceKind);
			this.tsSourceInput.Controls.Add(this.gbMetrics);
			this.tsSourceInput.Location = new Point(4, 22);
			this.tsSourceInput.Name = "tsSourceInput";
			this.tsSourceInput.Size = new Size(625, 375);
			this.tsSourceInput.TabIndex = 1;
			this.tsSourceInput.Text = "Источник (метрики/ревизии)";
			this.Label4.Location = new Point(8, 56);
			this.Label4.Name = "Label4";
			this.Label4.Size = new Size(55, 13);
			this.Label4.TabIndex = 0;
			this.Label4.Text = "Источник";
			this.Label8.Location = new Point(304, 56);
			this.Label8.Name = "Label8";
			this.Label8.Size = new Size(85, 13);
			this.Label8.TabIndex = 1;
			this.Label8.Text = "Лист/страница";
			this.Label9.Location = new Point(520, 56);
			this.Label9.Name = "Label9";
			this.Label9.Size = new Size(25, 13);
			this.Label9.TabIndex = 2;
			this.Label9.Text = "Год";
			this.Label10.Location = new Point(8, 88);
			this.Label10.Name = "Label10";
			this.Label10.Size = new Size(105, 13);
			this.Label10.TabIndex = 3;
			this.Label10.Text = "Населенный пункт";
			this.cbSource.Location = new Point(64, 48);
			this.cbSource.Name = "cbSource";
			this.cbSource.Size = new Size(225, 21);
			this.cbSource.TabIndex = 0;
			this.edPage.Location = new Point(392, 48);
			this.edPage.Name = "edPage";
			this.edPage.Size = new Size(112, 21);
			this.edPage.TabIndex = 1;
			this.edPage.Text = "";
			this.edSourceYear.Location = new Point(560, 48);
			this.edSourceYear.MaxLength = 4;
			this.edSourceYear.Name = "edSourceYear";
			this.edSourceYear.Size = new Size(57, 21);
			this.edSourceYear.TabIndex = 2;
			this.edSourceYear.Text = "    ";
			this.edPlace.Location = new Point(120, 80);
			this.edPlace.Name = "edPlace";
			this.edPlace.Size = new Size(497, 21);
			this.edPlace.TabIndex = 3;
			this.edPlace.Text = "";
			this.sgData.Location = new Point(8, 184);
			this.sgData.Name = "sgData";
			this.sgData.Size = new Size(609, 177);
			this.sgData.TabIndex = 4;
			this.cbPersonLink.DropDownStyle = ComboBoxStyle.DropDownList;
			this.cbPersonLink.Location = new Point(336, 296);
			this.cbPersonLink.Name = "cbPersonLink";
			this.cbPersonLink.Size = new Size(145, 21);
			this.cbPersonLink.TabIndex = 5;
			this.cbPersonLink.Visible = false;
			this.rgSourceKind.Location = new Point(8, 0);
			this.rgSourceKind.Name = "rgSourceKind";
			this.rgSourceKind.Size = new Size(609, 38);
			this.rgSourceKind.TabIndex = 6;
			this.rgSourceKind.TabStop = false;
			this.rgSourceKind.Text = "Тип источника";
			this.gbMetrics.Controls.Add(this.Label11);
			this.gbMetrics.Controls.Add(this.Label12);
			this.gbMetrics.Controls.Add(this.edEventDate);
			this.gbMetrics.Controls.Add(this.cbEventType);
			this.gbMetrics.Enabled = false;
			this.gbMetrics.Location = new Point(8, 120);
			this.gbMetrics.Name = "gbMetrics";
			this.gbMetrics.Size = new Size(609, 50);
			this.gbMetrics.TabIndex = 7;
			this.gbMetrics.TabStop = false;
			this.gbMetrics.Text = "Метрическая книга";
			this.Label11.Location = new Point(8, 24);
			this.Label11.Name = "Label11";
			this.Label11.Size = new Size(80, 13);
			this.Label11.TabIndex = 0;
			this.Label11.Text = "Дата события";
			this.Label12.Location = new Point(248, 24);
			this.Label12.Name = "Label12";
			this.Label12.Size = new Size(70, 13);
			this.Label12.TabIndex = 1;
			this.Label12.Text = "Тип события";
			this.edEventDate.Location = new Point(96, 16);
			this.edEventDate.Mask = "00/00/0000";
			this.edEventDate.MaxLength = 10;
			this.edEventDate.Name = "edEventDate";
			this.edEventDate.Size = new Size(129, 21);
			this.edEventDate.TabIndex = 0;
			this.edEventDate.Text = "  .  .    ";
			this.cbEventType.DropDownStyle = ComboBoxStyle.DropDownList;
			this.cbEventType.Location = new Point(328, 16);
			this.cbEventType.Name = "cbEventType";
			this.cbEventType.Size = new Size(145, 21);
			this.cbEventType.TabIndex = 1;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.ClientSize = new Size(649, 457);
			base.Controls.Add(this.btnParse);
			base.Controls.Add(this.btnClose);
			base.Controls.Add(this.PageControl1);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmPersonScan";
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Добавление персон из источника";
			this.PageControl1.ResumeLayout(false);
			this.tsSimpleInput.ResumeLayout(false);
			this.Panel1.ResumeLayout(false);
			this.Panel2.ResumeLayout(false);
			this.tsSourceInput.ResumeLayout(false);
			this.gbMetrics.ResumeLayout(false);
			base.ResumeLayout(false);
		}
		private void EditNameKeyPress(object sender, KeyPressEventArgs e)
		{
			if (e.KeyChar == '/')
			{
				e.Handled = true;
			}
		}
		private void sgDataSelectCell(object Sender, int ACol, int ARow, ref bool CanSelect)
		{
			CanSelect = true;
		}
		private void btnParseClick(object sender, EventArgs e)
		{
			int selectedIndex = this.PageControl1.SelectedIndex;
			if (selectedIndex != 0)
			{
				if (selectedIndex == 1)
				{
					this.ParseSource();
				}
			}
			else
			{
				this.ParseSimple();
			}
			this.Base.ListsRefresh(false);
		}
		private void EditBirthDateChange(object sender, EventArgs e)
		{
			this.CheckBirth.Checked = true;
		}
		private void EditDeathDateChange(object sender, EventArgs e)
		{
			this.CheckDeath.Checked = true;
		}
		private void cbPersonLinkChange(object sender, EventArgs e)
		{
		}
		private void rgSourceKindClick(object sender, EventArgs e)
		{
		}
		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FSourcesList.Free();
			}
			base.Dispose(Disposing);
		}
		public TfmPersonScan(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.FSourcesList = new TStringList();
			this.InitSimpleControls();
			this.InitGrid();
			this.InitSourceControls();

			for (TfmPersonScan.TPersonLink pl = TfmPersonScan.TPersonLink.plPerson; pl <= TfmPersonScan.TPersonLink.plChild; pl++)
			{
				this.cbPersonLink.Items.Add(GKL.LSList[(int)TfmPersonScan.PersonLinks[(int)pl] - 1]);
			}

			this.SetLang();
		}
		public void SetLang()
		{
			this.btnParse.Text = GKL.LSList[101];
			this.btnClose.Text = GKL.LSList[99];
			this.Text = GKL.LSList[22];
			this.tsSimpleInput.Text = GKL.LSList[485];
			this.btnMale.Text = BDSSystem.WStrFromWChar(GKL.LSList[66][0]);
			this.btnFemale.Text = BDSSystem.WStrFromWChar(GKL.LSList[67][0]);
			this.Label1.Text = GKL.LSList[301];
			this.CheckBirth.Text = GKL.LSList[321];
			this.Label3.Text = GKL.LSList[122];
			this.Label5.Text = GKL.LSList[302];
			this.CheckDeath.Text = GKL.LSList[332];
			this.Label6.Text = GKL.LSList[123];
			this.Label7.Text = GKL.LSList[303];
			this.Label2.Text = GKL.LSList[108];
			this.tsSourceInput.Text = GKL.LSList[486];
			this.rgSourceKind.Text = GKL.LSList[487];
			this.Label4.Text = GKL.LSList[109];
			this.Label8.Text = GKL.LSList[110];
			this.Label9.Text = GKL.LSList[490];
			this.Label10.Text = GKL.LSList[491];
			this.gbMetrics.Text = GKL.LSList[489];
			this.Label11.Text = GKL.LSList[492];
			this.Label12.Text = GKL.LSList[493];
		}

		static TfmPersonScan()
		{
			TfmPersonScan.PersonLinks = new LSID[]
			{
				LSID.LSID_RK_Unk, 
				LSID.LSID_PLPerson, 
				LSID.LSID_Father, 
				LSID.LSID_Mother, 
				LSID.LSID_PLGodparent, 
				LSID.LSID_Spouse, 
				LSID.LSID_Child
			};
		}

		private static TGEDCOMFamilyRecord _ParseSource_GetParentsFamily([In] TfmPersonScan Self, TGEDCOMIndividualRecord iRec)
		{
			TGEDCOMFamilyRecord Result;
			if (iRec.ChildToFamilyLinksCount > 0)
			{
				Result = iRec.GetChildToFamilyLink(0).Family;
			}
			else
			{
				Result = TGenEngine.CreateFamilyEx(Self.Base.Tree);
				Self.Base.Engine.AddFamilyChild(Result, iRec);
			}
			return Result;
		}
		private static TGEDCOMFamilyRecord _ParseSource_GetMarriageFamily([In] TfmPersonScan Self, TGEDCOMIndividualRecord iRec)
		{
			TGEDCOMFamilyRecord Result;
			if (iRec.SpouseToFamilyLinksCount > 0)
			{
				Result = iRec.GetSpouseToFamilyLink(0).Family;
			}
			else
			{
				Result = TGenEngine.CreateFamilyEx(Self.Base.Tree);
				Self.Base.Engine.AddFamilySpouse(Result, iRec);
			}
			return Result;
		}
		private static bool _ParseSource_CheckMain(TGEDCOMIndividualRecord aMain)
		{
			bool Result = aMain != null;
			if (!Result)
			{
				throw new Exception(GKL.LSList[507]);
			}
			return Result;
		}
	}
}
