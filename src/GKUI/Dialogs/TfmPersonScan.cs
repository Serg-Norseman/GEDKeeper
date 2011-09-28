using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;

namespace GKUI
{
	public partial class TfmPersonScan : Form
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
		private TfmBase FBase;
		private TStringList FSourcesList;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		private static TfmPersonScan.TPersonLink GetLinkByName([In] string aName)
		{
			TfmPersonScan.TPersonLink pl = TfmPersonScan.TPersonLink.plPerson;
			TfmPersonScan.TPersonLink Result;
			while (GKL.LSList[(int)TfmPersonScan.PersonLinks[(int)pl] - 1] != aName)
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
			int tokCount = SysUtils.GetTokensCount(tmp, ' ');
			if (tokCount < 3)
			{
				SysUtils.ShowError(GKL.LSList[506]);
			}
			else
			{
				string fam = SysUtils.SetAsName(SysUtils.GetToken(tmp, ' ', 1));
				string nam = SysUtils.SetAsName(SysUtils.GetToken(tmp, ' ', 2));
				string pat = SysUtils.SetAsName(SysUtils.GetToken(tmp, ' ', 3));
				TGEDCOMSex sx = TGEDCOMSex.svNone;
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
			if (!SysUtils.IsDigits(this.edSourceYear.Text))
			{
				SysUtils.ShowError(GKL.LSList[508]);
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
			this.btnMale.Text = new string(GKL.LSList[66][0], 1);
			this.btnFemale.Text = new string(GKL.LSList[67][0], 1);
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
			if (iRec.ChildToFamilyLinks.Count > 0)
			{
				Result = iRec.ChildToFamilyLinks[0].Family;
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
			if (iRec.SpouseToFamilyLinks.Count > 0)
			{
				Result = iRec.SpouseToFamilyLinks[0].Family;
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
