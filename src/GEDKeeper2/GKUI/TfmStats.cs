using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

using BSLib;
using ExcelLibrary.SpreadSheet;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Stats;
using GKUI.Controls;
using ZedGraph;

namespace GKUI
{
	/// <summary>
	/// 
	/// </summary>
	public sealed partial class TfmStats : Form, ILocalization
	{
		private enum ChartStyle : byte { csBar, csPoint }

		private readonly IBaseWindow fBase;
		private readonly List<GEDCOMRecord> fSelectedRecords;
		private readonly ZedGraphControl fGraph;
		private readonly GKListView fListStats;
        private readonly TreeStats fTreeStats;

		private string fChartTitle;
		private string fChartXTitle;
		private string fChartYTitle;

        public TfmStats(IBaseWindow aBase, List<GEDCOMRecord> selectedRecords)
        {
            this.InitializeComponent();
            base.MdiParent = TfmGEDKeeper.Instance;

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

            this.fListStats = GKUtils.CreateListView(this.Panel1);
            this.fListStats.AddListColumn("-", 250, false);
            this.fListStats.AddListColumn("-", 150, false);

            this.Panel1.Controls.SetChildIndex(this.fListStats, 0);
            this.Panel1.Controls.SetChildIndex(spl, 2);
            this.Panel1.Controls.SetChildIndex(this.fGraph, 3);
            this.Panel1.Controls.SetChildIndex(this.ToolBar1, 4);

            this.fBase = aBase;
            this.fSelectedRecords = selectedRecords;
            this.fTreeStats = new TreeStats(this.fBase.Tree, this.fSelectedRecords);

            this.UpdateStatsTypes();

            this.SetLang();
        }

		private static string GetPercent(int dividend, int divisor)
		{
			double val = ((divisor == 0) ? 0.0d : (dividend / (double)divisor * 100.0d));
			return string.Format(" ({0:0.00}%)", val);
		}

		private void PrepareArray(GraphPane gPane, ChartStyle style, bool excludeUnknowns)
		{
			gPane.Title.Text = fChartTitle;
			gPane.XAxis.Title.Text = fChartXTitle;
			gPane.YAxis.Title.Text = fChartYTitle;
			PointPairList ppList = new PointPairList();

			int num = this.fListStats.Items.Count;
			for (int i = 0; i < num; i++)
			{
				ListViewItem item = this.fListStats.Items[i];

				string s = item.Text;
				double lab = (s == "?") ? 0.0f : ConvHelper.ParseFloat(s, 0.0f, true);

				if (lab != 0 || !excludeUnknowns)
				{
					int val = int.Parse(item.SubItems[1].Text);
					ppList.Add(lab, val);
				}
			}
			ppList.Sort();

			switch (style) {
				case ChartStyle.csBar:
					gPane.AddBar("-", ppList, Color.Green);
					break;

				case ChartStyle.csPoint:
					gPane.AddCurve("-", ppList, Color.Green, SymbolType.Diamond).Symbol.Size = 3;
					break;
			}
		}

		private void CalcStats(StatsMode mode)
		{
			this.fListStats.SortColumn = 0;
			this.fListStats.Columns[0].Text = LangMan.LS(GKData.StatsTitles[(int)mode].Cap);
			this.fListStats.Columns[1].Text = LangMan.LS(LSID.LSID_Value);

			this.fListStats.Sorting = SortOrder.None;
			this.fListStats.SortColumn = -1;
			this.fListStats.BeginUpdate();
			this.fListStats.Items.Clear();
			
			List<StatsItem> vals = new List<StatsItem>();
			try
			{
				fTreeStats.GetSpecStats(mode, vals);
				ListViewItem[] items = new ListViewItem[vals.Count];

				int i = 0;
				foreach (StatsItem lv in vals)
				{
					ListViewItem item = new ListViewItem(lv.Caption);
					item.SubItems.Add(lv.Value.ToString());

					items[i] = item;
					i++;
				}

				this.fListStats.Items.AddRange(items);
			}
			finally
			{
				this.fListStats.EndUpdate();
			}

			GraphPane gPane = this.fGraph.GraphPane;
			try
			{
				gPane.CurveList.Clear();
				this.fChartTitle = LangMan.LS(GKData.StatsTitles[(int)mode].Title);

				switch (mode) {
					case StatsMode.smAge:
						this.fChartXTitle = LangMan.LS(LSID.LSID_Age);
						this.fChartYTitle = LangMan.LS(LSID.LSID_People);
						this.PrepareArray(gPane, ChartStyle.csPoint, true);
						break;

					case StatsMode.smLifeExpectancy:
						this.fChartXTitle = LangMan.LS(LSID.LSID_LifeExpectancy);
						this.fChartYTitle = LangMan.LS(LSID.LSID_People);
						this.PrepareArray(gPane, ChartStyle.csPoint, true);
						break;

					case StatsMode.smBirthYears:
					case StatsMode.smBirthTenYears:
					case StatsMode.smDeathYears:
						case StatsMode.smDeathTenYears: {
							switch (mode) {
								case StatsMode.smBirthYears:
								case StatsMode.smDeathYears:
									this.fChartXTitle = LangMan.LS(LSID.LSID_Years);
									break;

								case StatsMode.smBirthTenYears:
								case StatsMode.smDeathTenYears:
									this.fChartXTitle = LangMan.LS(LSID.LSID_Decennial);
									break;
							}

							switch (mode) {
								case StatsMode.smBirthYears:
								case StatsMode.smBirthTenYears:
									this.fChartYTitle = LangMan.LS(LSID.LSID_HowBirthes);
									break;

								case StatsMode.smDeathYears:
								case StatsMode.smDeathTenYears:
									this.fChartYTitle = LangMan.LS(LSID.LSID_HowDeads);
									break;
							}

							this.PrepareArray(gPane, ChartStyle.csPoint, true);
							break;
						}

					case StatsMode.smChildsDistribution:
						this.fChartXTitle = LangMan.LS(LSID.LSID_Childs);
						this.fChartYTitle = LangMan.LS(LSID.LSID_Parents);
						this.PrepareArray(gPane, ChartStyle.csBar, true);
						break;

					case StatsMode.smCertaintyIndex:
						this.fChartXTitle = LangMan.LS(LSID.LSID_CertaintyIndex);
						this.fChartYTitle = LangMan.LS(LSID.LSID_People);
						this.PrepareArray(gPane, ChartStyle.csBar, true);
						break;

					case StatsMode.smBirthByMonth:
						this.fChartXTitle = LangMan.LS(LSID.LSID_Month);
						this.fChartYTitle = LangMan.LS(LSID.LSID_People);
						this.PrepareArray(gPane, ChartStyle.csBar, true);
						break;
				}
			}
			finally
			{
				fGraph.AxisChange();
				fGraph.Invalidate();
			}
		}

		private void UpdateCommonStats()
		{
			CommonStats stats = fTreeStats.GetCommonStats();

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

			AddCompositeItem(LSID.LSID_AvgAge, stats.age);
			AddCompositeItem(LSID.LSID_AvgLife, stats.life);
			AddCompositeItem(LSID.LSID_AvgChilds, stats.childs);
			AddCompositeItem(LSID.LSID_AvgBorn, stats.fba);
			AddCompositeItem(LSID.LSID_AvgMarriagesCount, stats.marr);
			AddCompositeItem(LSID.LSID_AvgMarriagesAge, stats.mage);
			AddCompositeItem(LSID.LSID_CertaintyIndex, stats.cIndex);
		}
		
		private void AddCompositeItem(LSID name, CompositeItem item)
		{
			ListViewItem lvItem = this.ListCommon.Items.Add(LangMan.LS(name));
			lvItem.SubItems.Add(string.Format("{0:0.00}", item.CommonVal));
			lvItem.SubItems.Add(string.Format("{0:0.00}", item.MaleVal));
			lvItem.SubItems.Add(string.Format("{0:0.00}", item.FemaleVal));
		}

		private void TfmStats_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Escape) base.Close();
		}

		private void cbType_SelectedIndexChanged(object sender, EventArgs e)
		{
			this.CalcStats((StatsMode)this.cbType.SelectedIndex);
		}

		private void UpdateStatsTypes()
		{
			this.cbType.Items.Clear();
			for (StatsMode i = StatsMode.smAncestors; i <= StatsMode.smLast; i++)
			{
				GKData.StatsTitleStruct tr = GKData.StatsTitles[(int)i];
				this.cbType.Items.Add(LangMan.LS(tr.Title));
			}
		}

		private void TfmStats_Load(object sender, EventArgs e)
		{
			this.UpdateCommonStats();
		}

		public void SetLang()
		{
			this.Text = LangMan.LS(LSID.LSID_MIStats);
			this.tbExcelExport.ToolTipText = LangMan.LS(LSID.LSID_MIExportToExcelFile);
			this.UpdateCommonStats();
			
			int oldIndex = this.cbType.SelectedIndex;
			this.UpdateStatsTypes();
			this.cbType.SelectedIndex = oldIndex;
		}

		private void tbExcelExport_Click(object sender, EventArgs e)
		{
			string xlsFilename = GKUtils.RequireFilename("Excel files (*.xls)|*.xls");
			if (string.IsNullOrEmpty(xlsFilename)) return;

			try
			{
				int rowsCount = this.fListStats.Items.Count;
				this.fBase.ProgressInit(LangMan.LS(LSID.LSID_MIExport) + "...", rowsCount);

				try
				{
					Workbook workbook = new Workbook();
					Worksheet worksheet = new Worksheet(this.cbType.Text);

					worksheet.Cells[1,  1] = new Cell(this.fListStats.Columns[0].Text);
					worksheet.Cells[1,  2] = new Cell(this.fListStats.Columns[1].Text);

					int row = 1;
					for (int i = 0; i < rowsCount; i++)
					{
						ListViewItem item = this.fListStats.Items[i];

						worksheet.Cells[row, 1] = new Cell(item.Text);

						string sval = item.SubItems[1].Text;
						double dval;
						if (double.TryParse(sval, out dval)) {
							worksheet.Cells[row, 2] = new Cell(dval);
						} else {
							worksheet.Cells[row, 2] = new Cell(sval);
						}

						row++;
						this.fBase.ProgressStep();
					}

					workbook.Worksheets.Add(worksheet);
					workbook.Save(xlsFilename);

					if (File.Exists(xlsFilename)) {
						Process.Start(xlsFilename);
					}
				}
				finally
				{
					this.fBase.ProgressDone();
				}
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmStats.ExcelExport(): " + ex.Message);
				GKUtils.ShowError("Ошибка выгрузки в Excel");
			}
		}
	}
}
