using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKSys;

/// <summary>
/// Localization: unknown
/// </summary>

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
		private StringList FSourcesList;
		private TGEDCOMSex FSimpleTempSex = TGEDCOMSex.svMale;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		private static TPersonLink GetLinkByName([In] string aName)
		{
			TPersonLink pl = TPersonLink.plPerson;
			TPersonLink Result;
			while (LangMan.LSList[(int)PersonLinks[(int)pl] - 1] != aName)
			{
				pl++;
				if (pl == (TPersonLink)7)
				{
					Result = TPersonLink.plNone;
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

			this.FSimpleTempSex = TGEDCOMSex.svMale;
			this.btnMale.Text = new string(LangMan.LSList[66][0], 1);
		}

		private void InitSourceControls()
		{
			this.Base.Engine.GetSourcesList(this.FSourcesList);
			cbSource.Items.Clear();
			for (int i = 0; i <= FSourcesList.Count - 1; i++) cbSource.Items.Add(FSourcesList[i]);

			this.edPage.Text = "";
			this.edSourceYear.Text = "";
			this.edPlace.Text = "";
			this.edEventDate.Text = "";
			this.cbEventType.SelectedIndex = -1;
			this.dataGridView1.Rows.Clear();
		}

		private void InitGrid(DataGridView dgv)
		{
			object[] linksList = new object[PersonLinks.Length];
			for (int i = 0; i <= PersonLinks.Length - 1; i++) linksList[i] = LangMan.LS(PersonLinks[i]);

			((System.ComponentModel.ISupportInitialize)(dgv)).BeginInit();
			dgv.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
			                     	AddComboColumn("FLink", LangMan.LS(LSID.LSID_Join), linksList),
			                     	AddTextColumn("FName", LangMan.LS(LSID.LSID_Name)),
			                     	AddTextColumn("FPatronymic", LangMan.LS(LSID.LSID_Patronymic)),
			                     	AddTextColumn("FSurname", LangMan.LS(LSID.LSID_Surname)),
			                     	AddTextColumn("FAge", LangMan.LS(LSID.LSID_Age)),
			                     	AddTextColumn("FComment", LangMan.LS(LSID.LSID_Comment))});
			((System.ComponentModel.ISupportInitialize)(dgv)).EndInit();
		}

		private void ParseSimple()
		{
			string tmp = this.EditName.Text.ToLower();
			string[] tokens = tmp.Split(' ');
			if (tokens.Length < 3)
			{
				TGenEngine.ShowError(LangMan.LSList[506]);
			}
			else
			{
				string fam = SysUtils.SetAsName(tokens[0]);
				string nam = SysUtils.SetAsName(tokens[1]);
				string pat = SysUtils.SetAsName(tokens[2]);

				TGEDCOMIndividualRecord iRec = TGenEngine.CreatePersonEx(this.Base.Tree, nam, pat, fam, FSimpleTempSex, false);
				if (this.CheckBirth.Checked) {
					TGenEngine.CreateEventEx(this.Base.Tree, iRec, "BIRT", TGenEngine.StrToGEDCOMDate(this.EditBirthDate.Text, true), this.EditBirthPlace.Text);
				}
				if (this.CheckDeath.Checked) {
					TGenEngine.CreateEventEx(this.Base.Tree, iRec, "DEAT", TGenEngine.StrToGEDCOMDate(this.EditDeathDate.Text, true), this.EditDeathPlace.Text);
				}
				if (this.MemoNote.Text != "") {
  					TGenEngine.CreateNoteEx(Base.Tree, MemoNote.Text, iRec);
				}
				this.Base.ChangeRecord(iRec);

				this.InitSimpleControls();
			}
		}

		private void ParseSource()
		{
			string src_name = this.cbSource.Text;
			string src_page = this.edPage.Text;
			if (!SysUtils.IsDigits(this.edSourceYear.Text))
			{
				TGenEngine.ShowError(LangMan.LSList[508]);
			}
			else
			{
				int src_year = int.Parse(this.edSourceYear.Text);
				string place = this.edPlace.Text;
				TGEDCOMIndividualRecord iMain = null;
				try
				{
					for (int r = 0; r <= dataGridView1.Rows.Count - 1; r++)
					{
						DataGridViewRow row = dataGridView1.Rows[r];

						string lnk = (string)row.Cells[0].Value;
						string nm = (string)row.Cells[1].Value;
						string pt = (string)row.Cells[2].Value;
						string fm = (string)row.Cells[3].Value;
						string age = (string)row.Cells[4].Value;
						string comment = (string)row.Cells[5].Value;

						if (!string.IsNullOrEmpty(lnk)) {
							TPersonLink link = GetLinkByName(lnk);

							TGEDCOMSex sx = TfmSexCheck.DefineSex(nm, pt, TfmGEDKeeper.Instance.NamesTable);
							TGEDCOMIndividualRecord iRec = TGenEngine.CreatePersonEx(Base.Tree, nm, pt, fm, sx, false);

							if (!string.IsNullOrEmpty(age) && SysUtils.IsDigits(age)) {
								int birth_year = src_year - int.Parse(age);
								TGenEngine.CreateEventEx(Base.Tree, iRec, "BIRT", "ABT "+birth_year.ToString(), "");
							}

							if (!string.IsNullOrEmpty(place)) {
								TGEDCOMCustomEvent evt = TGenEngine.CreateEventEx(Base.Tree, iRec, "RESI", "", "");
								evt.Detail.Place.StringValue = place;
							}

							if (!string.IsNullOrEmpty(comment)) {
								TGEDCOMNoteRecord note = TGenEngine.CreateNoteEx(Base.Tree, comment, iRec);
							}

							if (!string.IsNullOrEmpty(src_name)) {
								TGEDCOMSourceRecord src_rec = Base.Engine.FindSource(src_name);
								if (src_rec == null) {
									src_rec = TGenEngine.CreateSource(Base.Tree);
									src_rec.FiledByEntry = src_name;
								}
								TGenEngine.BindRecordSource(Base.Tree, iRec, src_rec, src_page, 0);
							}

							Base.ChangeRecord(iRec); // FIXME: call for all added records (by list)

							switch (link) {
								case TPersonLink.plNone:
									break;

								case TPersonLink.plPerson:
									{
										iMain = iRec;
										string ev_name = "";

										if (radioButton2.Checked) {
											switch (cbEventType.SelectedIndex) {
													case  0: ev_name = "BIRT"; break;
													case  1: ev_name = "DEAT"; break;
													case  2: ev_name = "MARR"; break;
											}
										}

										if (ev_name == "BIRT" || ev_name == "DEAT") {
											TGEDCOMCustomEvent evt = TGenEngine.CreateEventEx(Base.Tree, iRec, ev_name, TGenEngine.StrToGEDCOMDate(edEventDate.Text, false), "");
											evt.Detail.Place.StringValue = place;
										}
										else
											if (ev_name == "MARR") {
											TGEDCOMFamilyRecord family = _ParseSource_GetMarriageFamily(iRec);
											TGEDCOMCustomEvent evt = TGenEngine.CreateEventEx(Base.Tree, family, ev_name, TGenEngine.StrToGEDCOMDate(edEventDate.Text, false), "");
											evt.Detail.Place.StringValue = place;
										}
									}
									break;

								case TPersonLink.plFather:
								case TPersonLink.plMother:
									{
										_ParseSource_CheckMain(iMain);
										TGEDCOMFamilyRecord family = _ParseSource_GetParentsFamily(iMain);
										Base.Engine.AddFamilySpouse(family, iRec);
									}
									break;

								case TPersonLink.plGodparent:
									{
										_ParseSource_CheckMain(iMain);
										Base.Engine.AddAssociation(iMain, LangMan.LS(LSID.LSID_PLGodparent), iRec);
									}
									break;

								case TPersonLink.plSpouse:
									{
										_ParseSource_CheckMain(iMain);
										TGEDCOMFamilyRecord family = _ParseSource_GetMarriageFamily(iMain);
										Base.Engine.AddFamilySpouse(family, iRec);
									}
									break;

								case TPersonLink.plChild:
									{
										_ParseSource_CheckMain(iMain);
										TGEDCOMFamilyRecord family = _ParseSource_GetMarriageFamily(iMain);
										Base.Engine.AddFamilyChild(family, iRec);
									}
									break;
							}
						}
					}
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
			switch (this.PageControl1.SelectedIndex) {
				case 0:
					this.ParseSimple();
					break;
				case 1:
					this.ParseSource();
					break;
			}
			this.Base.ListsRefresh(false);
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

			this.cbEventType.Items.AddRange(new object[] {
									"Рождение",
									"Смерть",
									"Брак"});

			this.FBase = aBase;
			this.FSourcesList = new StringList();

			this.InitGrid(dataGridView1);
			this.InitSimpleControls();
			this.InitSourceControls();

			for (TfmPersonScan.TPersonLink pl = TfmPersonScan.TPersonLink.plPerson; pl <= TfmPersonScan.TPersonLink.plChild; pl++)
			{
				this.cbPersonLink.Items.Add(LangMan.LSList[(int)TfmPersonScan.PersonLinks[(int)pl] - 1]);
			}

			this.SetLang();
		}

		public void SetLang()
		{
			this.btnParse.Text = LangMan.LSList[101];
			this.btnClose.Text = LangMan.LSList[99];
			this.Text = LangMan.LSList[22];
			this.tsSimpleInput.Text = LangMan.LSList[485];
			this.btnMale.Text = new string(LangMan.LSList[66][0], 1);
			//this.btnFemale.Text = new string(LangMan.LSList[67][0], 1);
			this.Label1.Text = LangMan.LSList[301];
			this.CheckBirth.Text = LangMan.LSList[321];
			this.Label3.Text = LangMan.LSList[122];
			this.Label5.Text = LangMan.LSList[302];
			this.CheckDeath.Text = LangMan.LSList[332];
			this.Label6.Text = LangMan.LSList[123];
			this.Label7.Text = LangMan.LSList[303];
			this.Label2.Text = LangMan.LSList[108];
			this.tsSourceInput.Text = LangMan.LSList[486];
			this.rgSourceKind.Text = LangMan.LSList[487];
			this.Label4.Text = LangMan.LSList[109];
			this.Label8.Text = LangMan.LSList[110];
			this.Label9.Text = LangMan.LSList[490];
			this.Label10.Text = LangMan.LSList[491];
			this.gbMetrics.Text = LangMan.LSList[489];
			this.Label11.Text = LangMan.LSList[492];
			this.Label12.Text = LangMan.LSList[493];
			
			radioButton1.Text = LangMan.LS(LSID.LSID_SK_Rev);
			radioButton2.Text = LangMan.LS(LSID.LSID_SK_Met);
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

		private TGEDCOMFamilyRecord _ParseSource_GetParentsFamily(TGEDCOMIndividualRecord iRec)
		{
			TGEDCOMFamilyRecord Result;
			if (iRec.ChildToFamilyLinks.Count > 0)
			{
				Result = iRec.ChildToFamilyLinks[0].Family;
			}
			else
			{
				Result = TGenEngine.CreateFamilyEx(Base.Tree);
				Base.Engine.AddFamilyChild(Result, iRec);
			}
			return Result;
		}

		private TGEDCOMFamilyRecord _ParseSource_GetMarriageFamily(TGEDCOMIndividualRecord iRec)
		{
			TGEDCOMFamilyRecord Result;
			if (iRec.SpouseToFamilyLinks.Count > 0)
			{
				Result = iRec.SpouseToFamilyLinks[0].Family;
			}
			else
			{
				Result = TGenEngine.CreateFamilyEx(Base.Tree);
				Base.Engine.AddFamilySpouse(Result, iRec);
			}
			return Result;
		}

		private bool _ParseSource_CheckMain(TGEDCOMIndividualRecord aMain)
		{
			bool Result = aMain != null;
			if (!Result)
			{
				throw new Exception(LangMan.LSList[507]);
			}
			return Result;
		}

		void BtnMaleClick(object sender, EventArgs e)
		{
			switch (FSimpleTempSex) {
				case TGEDCOMSex.svMale:
					this.btnMale.Text = new string(LangMan.LSList[67][0], 1);
					FSimpleTempSex = TGEDCOMSex.svFemale;
					break;
				case TGEDCOMSex.svFemale:
					this.btnMale.Text = new string(LangMan.LSList[66][0], 1);
					FSimpleTempSex = TGEDCOMSex.svMale;
					break;
			}
		}

		void EditBirthDateTextChanged(object sender, EventArgs e)
		{
			this.CheckBirth.Checked = true;
		}

		void EditDeathDateTextChanged(object sender, EventArgs e)
		{
			this.CheckDeath.Checked = true;
		}

		void RadioButton1CheckedChanged(object sender, EventArgs e)
		{
			gbMetrics.Enabled = (radioButton2.Checked);
		}

		public DataGridViewColumn AddTextColumn(string colName, string headerText)
		{
			DataGridViewColumn col = new System.Windows.Forms.DataGridViewTextBoxColumn();
			col.HeaderText = headerText;
			col.Name = colName;
			return col;
		}

		public DataGridViewColumn AddComboColumn(string colName, string headerText, object[] items)
		{
			DataGridViewComboBoxColumn col = new System.Windows.Forms.DataGridViewComboBoxColumn();
			col.HeaderText = headerText;
			col.Name = colName;
			col.Items.AddRange(items);
			return col;
		}
	}
}
