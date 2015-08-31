using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKUI.Controls;
using ZedGraph;

namespace GKUI
{
    /// <summary>
    /// Localization: dirty
    /// </summary>
    public sealed partial class TfmStats : Form, ILocalization
	{
		private enum TChartStyle : byte { csBar, csPoint }

        private readonly IBase fBase;
        private readonly List<GEDCOMRecord> fSelectedRecords;
		private readonly ZedGraphControl fGraph;
        private readonly GKListView fListStats;

        private string fChartTitle;
		private string fChartXTitle;
		private string fChartYTitle;
		private TreeStats fTreeStats;

		private static double SafeDiv(double dividend, double divisor)
		{
			return ((divisor == (double)0f) ? 0.0 : (dividend / divisor));
		}

		private static string GetPercent(int dividend, int divisor)
		{
			double val = ((divisor == 0) ? 0.0 : ((double)dividend / (double)divisor * 100.0));
			return string.Format(" ({0:0.00}%)", val);
		}

		private void PrepareArray(GraphPane gPane, TChartStyle style, bool excludeUnknowns)
		{
			gPane.Title.Text = fChartTitle;
			gPane.XAxis.Title.Text = fChartXTitle;
			gPane.YAxis.Title.Text = fChartYTitle;
			PointPairList ppList = new PointPairList();

			int num = this.fListStats.Items.Count;
			for (int i = 0; i < num; i++)
			{
				string s = this.fListStats.Items[i].Text;

                double lab = (s == "?") ? 0.0f : SysUtils.ParseFloat(s, 0.0f, true);

                if (lab != 0 || !excludeUnknowns)
				{
					int val = int.Parse(this.fListStats.Items[i].SubItems[1].Text);
					ppList.Add(lab, val);
				}
			}
			ppList.Sort();

			switch (style) {
				case TChartStyle.csBar:
					gPane.AddBar("-", ppList, Color.Green);
					break;
				case TChartStyle.csPoint:
					gPane.AddCurve("-", ppList, Color.Green, SymbolType.Diamond).Symbol.Size = 3;
					break;
			}
		}

		private void CalcStats(TreeStats.TStatMode mode)
		{
			this.fListStats.SortColumn = 0;
			this.fListStats.Columns[0].Text = LangMan.LS(GKData.StatsTitles[(int)mode].Cap);
			this.fListStats.Columns[1].Text = LangMan.LS(LSID.LSID_Value);

			this.fListStats.Sorting = SortOrder.None;
			this.fListStats.SortColumn = -1;
			this.fListStats.BeginUpdate();
			this.fListStats.Items.Clear();
			
			List<TreeStats.TListVal> vals = new List<TreeStats.TListVal>();
			try
			{
				fTreeStats.GetSpecStats(mode, vals);
				ListViewItem[] items = new ListViewItem[vals.Count];

				int i = 0;
				foreach (TreeStats.TListVal lv in vals)
				{
					ListViewItem item = new ListViewItem();
					item.Text = lv.Item;
					item.SubItems.Add(lv.Count.ToString());

					items[i] = item;
					i++;
				}

				this.fListStats.Items.AddRange(items);
			}
			finally
			{
				//items.Dispose();
				//vals.Dispose();
				this.fListStats.EndUpdate();
			}

			GraphPane gPane = this.fGraph.GraphPane;
			try
			{
				gPane.CurveList.Clear();
				this.fChartTitle = LangMan.LS(GKData.StatsTitles[(int)mode].Title);

				switch (mode) {
					case TreeStats.TStatMode.smAge:
						this.fChartXTitle = LangMan.LS(LSID.LSID_Age);
						this.fChartYTitle = LangMan.LS(LSID.LSID_People);
						this.PrepareArray(gPane, TChartStyle.csPoint, true);
						break;

					case TreeStats.TStatMode.smLifeExpectancy:
						this.fChartXTitle = LangMan.LS(LSID.LSID_LifeExpectancy);
						this.fChartYTitle = LangMan.LS(LSID.LSID_People);
						this.PrepareArray(gPane, TChartStyle.csPoint, true);
						break;

					case TreeStats.TStatMode.smBirthYears:
					case TreeStats.TStatMode.smBirthTenYears:
					case TreeStats.TStatMode.smDeathYears: 
					case TreeStats.TStatMode.smDeathTenYears: {
						switch (mode) {
							case TreeStats.TStatMode.smBirthYears:
							case TreeStats.TStatMode.smDeathYears: 
								this.fChartXTitle = LangMan.LS(LSID.LSID_Years);
								break;

							case TreeStats.TStatMode.smBirthTenYears:
							case TreeStats.TStatMode.smDeathTenYears: 
								this.fChartXTitle = LangMan.LS(LSID.LSID_Decennial);
								break;
						}

						switch (mode) {
							case TreeStats.TStatMode.smBirthYears:
          					case TreeStats.TStatMode.smBirthTenYears:
								this.fChartYTitle = LangMan.LS(LSID.LSID_HowBirthes);
          						break;

          					case TreeStats.TStatMode.smDeathYears:
          					case TreeStats.TStatMode.smDeathTenYears:
          						this.fChartYTitle = LangMan.LS(LSID.LSID_HowDeads);
          						break;
						}

        				this.PrepareArray(gPane, TChartStyle.csPoint, true);
						break;
					}

					case TreeStats.TStatMode.smChildsDistribution:
						this.fChartXTitle = LangMan.LS(LSID.LSID_Childs);
						this.fChartYTitle = LangMan.LS(LSID.LSID_Parents);
						this.PrepareArray(gPane, TChartStyle.csBar, true);
						break;

					case TreeStats.TStatMode.smCertaintyIndex:
						this.fChartXTitle = LangMan.LS(LSID.LSID_CertaintyIndex);
						this.fChartYTitle = LangMan.LS(LSID.LSID_People);
						this.PrepareArray(gPane, TChartStyle.csBar, true);
						break;
				}
			}
			finally
			{
				fGraph.AxisChange();
				fGraph.Invalidate();
			}
		}

		private void TfmStats_Load(object sender, EventArgs e)
		{
			this.fTreeStats = new TreeStats(this.fBase.Tree, this.fSelectedRecords);
			
			TreeStats.CommonStats stats;
			fTreeStats.GetCommonStats(out stats);

			this.ListCommon.Items.Clear();

			ListViewItem item = this.ListCommon.Items.Add(LangMan.LS(LSID.LSID_People));
			item.SubItems.Add(stats.persons.ToString());
			item.SubItems.Add(stats.persons_m.ToString() + TfmStats.GetPercent(stats.persons_m, stats.persons));
			item.SubItems.Add(stats.persons_f.ToString() + TfmStats.GetPercent(stats.persons_f, stats.persons));

			item = this.ListCommon.Items.Add(LangMan.LS(LSID.LSID_Living));
			item.SubItems.Add(stats.lives.ToString());
			item.SubItems.Add(stats.lives_m.ToString());
			item.SubItems.Add(stats.lives_f.ToString());

			item = this.ListCommon.Items.Add(LangMan.LS(LSID.LSID_Deads));
			item.SubItems.Add((stats.persons - stats.lives).ToString());
			item.SubItems.Add((stats.persons_m - stats.lives_m).ToString());
			item.SubItems.Add((stats.persons_f - stats.lives_f).ToString());

			checked
			{
				item = this.ListCommon.Items.Add(LangMan.LS(LSID.LSID_AvgAge));
				item.SubItems.Add((Math.Round(SafeDiv(stats.age, stats.age_cnt))).ToString());
				item.SubItems.Add((Math.Round(SafeDiv(stats.age_m, stats.age_m_cnt))).ToString());
				item.SubItems.Add((Math.Round(SafeDiv(stats.age_f, stats.age_f_cnt))).ToString());

				item = this.ListCommon.Items.Add(LangMan.LS(LSID.LSID_AvgLife));
				item.SubItems.Add((Math.Round(SafeDiv(stats.life, stats.life_cnt))).ToString());
				item.SubItems.Add((Math.Round(SafeDiv(stats.life_m, stats.life_m_cnt))).ToString());
				item.SubItems.Add((Math.Round(SafeDiv(stats.life_f, stats.life_f_cnt))).ToString());

				item = this.ListCommon.Items.Add(LangMan.LS(LSID.LSID_AvgChilds));
				item.SubItems.Add(string.Format("{0:0.00}", SafeDiv(stats.childs, stats.childs_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SafeDiv(stats.childs_m, stats.childs_m_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SafeDiv(stats.childs_f, stats.childs_f_cnt)));

				item = this.ListCommon.Items.Add(LangMan.LS(LSID.LSID_AvgBorn));
				item.SubItems.Add(string.Format("{0:0.00}", SafeDiv(stats.fba, stats.fba_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SafeDiv(stats.fba_m, stats.fba_m_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SafeDiv(stats.fba_f, stats.fba_f_cnt)));

				item = this.ListCommon.Items.Add(LangMan.LS(LSID.LSID_AvgMarriagesCount));
				item.SubItems.Add(string.Format("{0:0.00}", SafeDiv(stats.marr, stats.marr_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SafeDiv(stats.marr_m, stats.marr_m_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SafeDiv(stats.marr_f, stats.marr_f_cnt)));

				item = this.ListCommon.Items.Add(LangMan.LS(LSID.LSID_AvgMarriagesAge));
				item.SubItems.Add(string.Format("{0:0.00}", SafeDiv(stats.mage, stats.mage_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SafeDiv(stats.mage_m, stats.mage_m_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SafeDiv(stats.mage_f, stats.mage_f_cnt)));

				item = this.ListCommon.Items.Add(LangMan.LS(LSID.LSID_CertaintyIndex));
				item.SubItems.Add(string.Format("{0:0.00}", SafeDiv(stats.ci, stats.ci_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SafeDiv(stats.ci_m, stats.ci_m_cnt)));
				item.SubItems.Add(string.Format("{0:0.00}", SafeDiv(stats.ci_f, stats.ci_f_cnt)));
			}
		}

		private void TfmStats_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Escape) base.Close();
		}

		private void cbType_SelectedIndexChanged(object sender, EventArgs e)
		{
			this.CalcStats((TreeStats.TStatMode)this.cbType.SelectedIndex);
		}

		public TfmStats(IBase aBase, List<GEDCOMRecord> selectedRecords)
		{
			this.InitializeComponent();
			base.MdiParent = TfmGEDKeeper.Instance;
			this.fBase = aBase;
			this.fSelectedRecords = selectedRecords;

			this.fGraph = new ZedGraphControl();
			this.fGraph.IsShowPointValues = true;
			this.fGraph.Dock = DockStyle.Right;
			this.fGraph.Size = new Size(400, 200);
			Splitter spl = new Splitter();
			spl.Dock = DockStyle.Right;
			spl.Size = new Size(4, 290);
			spl.MinExtra = 100;
			spl.MinSize = 100;
			this.Panel1.Controls.Add(this.fGraph);
			this.Panel1.Controls.Add(spl);

			GKUtils.CreateListView(this.Panel1, out this.fListStats);
			this.fListStats.AddListColumn("-", 250, false);
			this.fListStats.AddListColumn("-", 150, false);

			this.Panel1.Controls.SetChildIndex(this.fListStats, 0);
			this.Panel1.Controls.SetChildIndex(spl, 2);
			this.Panel1.Controls.SetChildIndex(this.fGraph, 3);
			this.Panel1.Controls.SetChildIndex(this.ToolBar1, 4);
			this.cbType.Items.Clear();

			for (TreeStats.TStatMode i = TreeStats.TStatMode.smAncestors; i <= TreeStats.TStatMode.smLast; i++)
			{
				GKData.TStatsTitleStruct tr = GKData.StatsTitles[(int)i];
				this.cbType.Items.Add(LangMan.LS(tr.Title));
			}

			(this as ILocalization).SetLang();
		}

		void ILocalization.SetLang()
		{
			this.Text = LangMan.LS(LSID.LSID_MIStats);
		}
	}
}
