using System;
using System.Collections;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Controls;
using ZedGraph;

namespace GKUI
{
	public partial class TfmStats : Form, ILocalization
	{
		private struct TTitleRec
		{
			public LSID Title;
			public LSID Cap;

			public TTitleRec(LSID title, LSID cap) {
				this.Title = title;
				this.Cap = cap;
			}
		}

		private enum TChartStyle : byte { csBar, csPoint }

		private static TTitleRec[] StTitles;
		private TGKListView ListStats;
		private ZedGraphControl zgc;
		private TfmBase FBase;
		private string ChartTitle;
		private string ChartXTitle;
		private string ChartYTitle;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		private void PrepareArray(GraphPane gPane, TChartStyle aStyle, bool aExcludeUnknowns)
		{
			gPane.Title.Text = ChartTitle;
			gPane.XAxis.Title.Text = ChartXTitle;
			gPane.YAxis.Title.Text = ChartYTitle;
			PointPairList ppList = new PointPairList();

			int num = this.ListStats.Items.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				string s = this.ListStats.Items[i].Text;
				int lab;
				if (s == "?")
				{
					lab = 0;
				}
				else
				{
					lab = int.Parse(s);
				}
				if (lab != 0 || !aExcludeUnknowns)
				{
					int val = int.Parse(this.ListStats.Items[i].SubItems[1].Text);
					ppList.Add((double)lab, (double)val);
				}
			}
			ppList.Sort();
			
			switch (aStyle) {
				case TChartStyle.csBar:
					gPane.AddBar("-", ppList, Color.Green);
					break;
				case TChartStyle.csPoint:
					gPane.AddCurve("-", ppList, Color.Green, SymbolType.Diamond).Symbol.Size = 3;
					break;
			}
		}

		private void CalcStats(TGEDCOMTree aTree, TGenEngine.TStatMode aMode)
		{
			this.ListStats.SortColumn = 0;
			this.ListStats.Columns[0].Text = GKL.LSList[(int)StTitles[(int)aMode].Cap - 1];
			this.ListStats.Columns[1].Text = GKL.LSList[202];

			this.ListStats.BeginUpdate();
			this.ListStats.Items.Clear();

			List<TGenEngine.TListVal> vals = new List<TGenEngine.TListVal>();
			try
			{
				this.Base.Engine.GetSpecStats(aMode, vals);
				foreach (TGenEngine.TListVal lv in vals)
				{
					ListViewItem item = this.ListStats.Items.Add(lv.Item);
					item.SubItems.Add(lv.Count.ToString());
				}

				/*IEnumerator Enumerator = vals.GetEnumerator();
				while (Enumerator.MoveNext())
				{
					TGenEngine.TListVal lv = (TGenEngine.TListVal)Enumerator.Current;
					ListViewItem item = this.ListStats.Items.Add(lv.Item);
					item.SubItems.Add(lv.Count.ToString());
				}*/
			}
			finally
			{
				TObjectHelper.Free(vals);
				this.ListStats.EndUpdate();
			}

			GraphPane gPane = this.zgc.GraphPane;
			try
			{
				gPane.CurveList.Clear();
				this.ChartTitle = GKL.LSList[(int)StTitles[(int)aMode].Title - 1];

				switch (aMode) {
					case TGenEngine.TStatMode.smAge: {
						this.ChartXTitle = GKL.LSList[305];
						this.ChartYTitle = GKL.LSList[533];
						this.PrepareArray(gPane, TfmStats.TChartStyle.csPoint, true);
						break;
					}

					case TGenEngine.TStatMode.smLifeExpectancy: {
						this.ChartXTitle = GKL.LSList[306];
						this.ChartYTitle = GKL.LSList[533];
						this.PrepareArray(gPane, TfmStats.TChartStyle.csPoint, true);
						break;
					}

					case TGenEngine.TStatMode.smBirthYears:
					case TGenEngine.TStatMode.smBirthTenYears:
					case TGenEngine.TStatMode.smDeathYears: 
					case TGenEngine.TStatMode.smDeathTenYears: {
						switch (aMode) {
							case TGenEngine.TStatMode.smBirthYears:
							case TGenEngine.TStatMode.smDeathYears: 
								this.ChartXTitle = GKL.LSList[(int)LSID.LSID_Years];
								break;
							case TGenEngine.TStatMode.smBirthTenYears:
							case TGenEngine.TStatMode.smDeathTenYears: 
								this.ChartXTitle = GKL.LSList[(int)LSID.LSID_Decennial];
								break;
						}

						switch (aMode) {
							case TGenEngine.TStatMode.smBirthYears:
          					case TGenEngine.TStatMode.smBirthTenYears:
          						this.ChartYTitle = GKL.LSList[(int)LSID.LSID_HowBirthes];
          						break;
          					case TGenEngine.TStatMode.smDeathYears:
          					case TGenEngine.TStatMode.smDeathTenYears:
          						this.ChartYTitle = GKL.LSList[(int)LSID.LSID_HowDeads];
          						break;
						}

        				this.PrepareArray(gPane, TfmStats.TChartStyle.csPoint, true);
						break;
					}

					case TGenEngine.TStatMode.smChildsDistribution: {
						this.ChartXTitle = GKL.LSList[118];
						this.ChartYTitle = GKL.LSList[152];
						this.PrepareArray(gPane, TfmStats.TChartStyle.csBar, true);
						break;
					}
				}
			}
			finally
			{
				zgc.AxisChange();
				zgc.Invalidate();
			}
		}

		private void TfmStats_Load(object sender, EventArgs e)
		{
			TGenEngine.TCommonStats stats;
			this.Base.Engine.GetCommonStats(out stats);
			this.ListCommon.Items.Clear();
			ListViewItem item = this.ListCommon.Items.Add(GKL.LSList[533]);
			item.SubItems.Add(stats.persons.ToString());
			item.SubItems.Add(stats.persons_m.ToString() + TfmStats._TfmStats_Load_GetPercent(stats.persons_m, stats.persons));
			item.SubItems.Add(stats.persons_f.ToString() + TfmStats._TfmStats_Load_GetPercent(stats.persons_f, stats.persons));
			item = this.ListCommon.Items.Add(GKL.LSList[538]);
			item.SubItems.Add(stats.lives.ToString());
			item.SubItems.Add(stats.lives_m.ToString());
			item.SubItems.Add(stats.lives_f.ToString());
			item = this.ListCommon.Items.Add(GKL.LSList[539]);
			item.SubItems.Add((stats.persons - stats.lives).ToString());
			item.SubItems.Add((stats.persons_m - stats.lives_m).ToString());
			item.SubItems.Add((stats.persons_f - stats.lives_f).ToString());
			checked
			{
				item = this.ListCommon.Items.Add(GKL.LSList[540]);
				item.SubItems.Add((Math.Round(SysUtils.SafeDiv(stats.age, stats.age_cnt))).ToString());
				item.SubItems.Add((Math.Round(SysUtils.SafeDiv(stats.age_m, stats.age_m_cnt))).ToString());
				item.SubItems.Add((Math.Round(SysUtils.SafeDiv(stats.age_f, stats.age_f_cnt))).ToString());

				item = this.ListCommon.Items.Add(GKL.LSList[541]);
				item.SubItems.Add((Math.Round(SysUtils.SafeDiv(stats.life, stats.life_cnt))).ToString());
				item.SubItems.Add((Math.Round(SysUtils.SafeDiv(stats.life_m, stats.life_m_cnt))).ToString());
				item.SubItems.Add((Math.Round(SysUtils.SafeDiv(stats.life_f, stats.life_f_cnt))).ToString());

				item = this.ListCommon.Items.Add(GKL.LSList[542]);
				item.SubItems.Add(string.Format("{0:0.00}", SysUtils.SafeDiv(stats.childs, stats.childs_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SysUtils.SafeDiv(stats.childs_m, stats.childs_m_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SysUtils.SafeDiv(stats.childs_f, stats.childs_f_cnt)));

				item = this.ListCommon.Items.Add(GKL.LSList[543]);
				item.SubItems.Add(string.Format("{0:0.00}", SysUtils.SafeDiv(stats.fba, stats.fba_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SysUtils.SafeDiv(stats.fba_m, stats.fba_m_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SysUtils.SafeDiv(stats.fba_f, stats.fba_f_cnt)));

				item = this.ListCommon.Items.Add(GKL.LSList[544]);
				item.SubItems.Add(string.Format("{0:0.00}", SysUtils.SafeDiv(stats.marr, stats.marr_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SysUtils.SafeDiv(stats.marr_m, stats.marr_m_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SysUtils.SafeDiv(stats.marr_f, stats.marr_f_cnt)));

				item = this.ListCommon.Items.Add(GKL.LSList[545]);
				item.SubItems.Add(string.Format("{0:0.00}", SysUtils.SafeDiv(stats.mage, stats.mage_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SysUtils.SafeDiv(stats.mage_m, stats.mage_m_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SysUtils.SafeDiv(stats.mage_f, stats.mage_f_cnt)));
			}
		}

		private void TfmStats_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Escape) base.Close();
		}

		private void cbType_SelectedIndexChanged(object sender, EventArgs e)
		{
			this.CalcStats(this.Base.Tree, (TGenEngine.TStatMode)this.cbType.SelectedIndex);
		}

		public TfmStats(TfmBase aBase)
		{
			this.InitializeComponent();
			base.MdiParent = GKUI.TfmGEDKeeper.Instance;
			this.FBase = aBase;
			this.zgc = new ZedGraphControl();
			this.zgc.IsShowPointValues = true;
			this.zgc.Dock = DockStyle.Right;
			this.zgc.Size = new Size(400, 200);
			Splitter spl = new Splitter();
			spl.Dock = DockStyle.Right;
			spl.Size = new Size(4, 290);
			spl.MinExtra = 100;
			spl.MinSize = 100;
			this.Panel1.Controls.Add(this.zgc);
			this.Panel1.Controls.Add(spl);
			this.Base.CreateListView(this.Panel1, ref this.ListStats);
			this.ListStats.AddListColumn("-", 250, false);
			this.ListStats.AddListColumn("-", 150, false);
			this.Panel1.Controls.SetChildIndex(this.ListStats, 0);
			this.Panel1.Controls.SetChildIndex(spl, 2);
			this.Panel1.Controls.SetChildIndex(this.zgc, 3);
			this.Panel1.Controls.SetChildIndex(this.ToolBar1, 4);
			this.cbType.Items.Clear();

			for (TGenEngine.TStatMode i = TGenEngine.TStatMode.smAncestors; i <= TGenEngine.TStatMode.smAAF_2; i++)
			{
				TTitleRec tr = StTitles[(int)i];
				this.cbType.Items.Add(GKL.LSList[(int)tr.Title - 1]);
			}

			(this as ILocalization).SetLang();
		}

		void ILocalization.SetLang()
		{
			this.Text = GKL.LSList[29];
		}

		static TfmStats()
		{
			StTitles = new TTitleRec[34];
			StTitles[0] = new TTitleRec(LSID.LSID_AncestorsCount, LSID.LSID_Name);
			StTitles[1] = new TTitleRec(LSID.LSID_DescendantsCount, LSID.LSID_Name);
			StTitles[2] = new TTitleRec(LSID.LSID_GenerationsCount, LSID.LSID_Name);
			StTitles[3] = new TTitleRec(LSID.LSID_Surname, LSID.LSID_Surname);
			StTitles[4] = new TTitleRec(LSID.LSID_Name, LSID.LSID_Name);
			StTitles[5] = new TTitleRec(LSID.LSID_Patronymic, LSID.LSID_Patronymic);
			StTitles[6] = new TTitleRec(LSID.LSID_Age, LSID.LSID_Age);
			StTitles[7] = new TTitleRec(LSID.LSID_LifeExpectancy, LSID.LSID_Age);
			StTitles[8] = new TTitleRec(LSID.LSID_BirthYears, LSID.LSID_BirthYears);
			StTitles[9] = new TTitleRec(LSID.LSID_BirthYearsDec, LSID.LSID_BirthYears);
			StTitles[10] = new TTitleRec(LSID.LSID_DeathYears, LSID.LSID_DeathYears);
			StTitles[11] = new TTitleRec(LSID.LSID_DeathYearsDec, LSID.LSID_DeathYears);
			StTitles[12] = new TTitleRec(LSID.LSID_ChildsCount, LSID.LSID_Name);
			StTitles[13] = new TTitleRec(LSID.LSID_DistrChilds, LSID.LSID_ChildsCount);
			StTitles[14] = new TTitleRec(LSID.LSID_BirthPlace, LSID.LSID_BirthPlace);
			StTitles[15] = new TTitleRec(LSID.LSID_DeathPlace, LSID.LSID_DeathPlace);
			StTitles[16] = new TTitleRec(LSID.LSID_Residence, LSID.LSID_Residence);
			StTitles[17] = new TTitleRec(LSID.LSID_Occupation, LSID.LSID_Occupation);
			StTitles[18] = new TTitleRec(LSID.LSID_Religion, LSID.LSID_Religion);
			StTitles[19] = new TTitleRec(LSID.LSID_Nationality, LSID.LSID_Nationality);
			StTitles[20] = new TTitleRec(LSID.LSID_Education, LSID.LSID_Education);
			StTitles[21] = new TTitleRec(LSID.LSID_Caste, LSID.LSID_Caste);
			StTitles[22] = new TTitleRec(LSID.LSID_AgeFirstborn, LSID.LSID_Name);
			StTitles[23] = new TTitleRec(LSID.LSID_MarriagesCount, LSID.LSID_Name);
			StTitles[24] = new TTitleRec(LSID.LSID_MarriagesAge, LSID.LSID_Name);
			StTitles[25] = new TTitleRec(LSID.LSID_DiffSpouses, LSID.LSID_Family);
			StTitles[26] = new TTitleRec(LSID.LSID_Hobby, LSID.LSID_Hobby);
			StTitles[27] = new TTitleRec(LSID.LSID_Award, LSID.LSID_Award);
			StTitles[28] = new TTitleRec(LSID.LSID_Mili, LSID.LSID_Mili);
			StTitles[29] = new TTitleRec(LSID.LSID_MiliInd, LSID.LSID_MiliInd);
			StTitles[30] = new TTitleRec(LSID.LSID_MiliDis, LSID.LSID_MiliDis);
			StTitles[31] = new TTitleRec(LSID.LSID_MiliRank, LSID.LSID_MiliRank);

			StTitles[32] = new TTitleRec(LSID.LSID_AAF_1, LSID.LSID_AAF_1);
			StTitles[33] = new TTitleRec(LSID.LSID_AAF_2, LSID.LSID_AAF_2);
		}

		private static string _TfmStats_Load_GetPercent(int aDividend, int aDivisor)
		{
			double value;
			if (aDivisor == 0)
			{
				value = 0.0;
			}
			else
			{
				value = ((double)aDividend / (double)aDivisor * 100.0);
			}
			return string.Format(" ({0:0.00}%)", new object[] { value });
		}
	}
}
