/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

using ExcelLibrary.SpreadSheet;
using GKCommon;
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
    public sealed partial class StatisticsWin : Form, ILocalization
    {
        private enum ChartStyle { Bar, Point, ClusterBar }

        private readonly IBaseWindow fBase;
        private readonly List<GEDCOMRecord> fSelectedRecords;
        private readonly ZedGraphControl fGraph;
        private readonly GKListView fListStats;
        private readonly TreeStats fTreeStats;

        private string fChartTitle;
        private string fChartXTitle;
        private string fChartYTitle;

        public StatisticsWin(IBaseWindow aBase, List<GEDCOMRecord> selectedRecords)
        {
            this.InitializeComponent();
            base.MdiParent = MainWin.Instance;

            this.tbExcelExport.Image = (System.Drawing.Image)MainWin.ResourceManager.GetObjectEx("iExcel");

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

        private void PrepareArray(GraphPane gPane, ChartStyle style, bool excludeUnknowns, List<StatsItem> vals = null)
        {
            gPane.Title.Text = fChartTitle;
            gPane.XAxis.Title.Text = fChartXTitle;
            gPane.YAxis.Title.Text = fChartYTitle;

            if (style != ChartStyle.ClusterBar)
            {
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

                switch (style)
                {
                    case ChartStyle.Bar:
                        gPane.AddBar("-", ppList, Color.Green);
                        break;

                    case ChartStyle.Point:
                        gPane.AddCurve("-", ppList, Color.Green, SymbolType.Diamond).Symbol.Size = 3;
                        break;
                }
            }
            else
            {
                gPane.CurveList.Clear();
                
                int itemscount = vals.Count;
                double[] YValuesF = new double[itemscount];
                double[] YValuesM = new double[itemscount];
                double[] XValues = new double[itemscount];

                for (int i = 0; i < itemscount; i++)
                {
                    StatsItem sti = vals[i];
                    XValues[i] = ConvHelper.ParseInt(sti.Caption, 0);
                    YValuesF[i] = sti.ValF;
                    YValuesM[i] = sti.ValM;
                }

                /*BarItem bar1 = */gPane.AddBar("F", XValues, YValuesF, Color.Red);
                /*BarItem bar2 = */gPane.AddBar("M", XValues, YValuesM, Color.Blue);

                gPane.BarSettings.MinBarGap = 0.0f;
                gPane.BarSettings.MinClusterGap = 2.5f;
                
                // expand the range of the Y axis slightly to accommodate the labels
                //gPane.YAxis.Scale.Max += gPane.YAxis.Scale.MajorStep;

                // Create TextObj's to provide labels for each bar
                BarItem.CreateBarLabels(gPane, false, "f0");
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
                    
                    string stVal = (!lv.IsCombo) ? lv.Value.ToString() : lv.ValF.ToString() + " | " + lv.ValM.ToString();
                    item.SubItems.Add(stVal);

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
                        this.PrepareArray(gPane, ChartStyle.Point, true);
                        break;

                    case StatsMode.smLifeExpectancy:
                        this.fChartXTitle = LangMan.LS(LSID.LSID_LifeExpectancy);
                        this.fChartYTitle = LangMan.LS(LSID.LSID_People);
                        this.PrepareArray(gPane, ChartStyle.Point, true);
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

                            this.PrepareArray(gPane, ChartStyle.Point, true);
                            break;
                        }

                    case StatsMode.smChildsDistribution:
                        this.fChartXTitle = LangMan.LS(LSID.LSID_Childs);
                        this.fChartYTitle = LangMan.LS(LSID.LSID_Parents);
                        this.PrepareArray(gPane, ChartStyle.Bar, true);
                        break;

                    case StatsMode.smCertaintyIndex:
                        this.fChartXTitle = LangMan.LS(LSID.LSID_CertaintyIndex);
                        this.fChartYTitle = LangMan.LS(LSID.LSID_People);
                        this.PrepareArray(gPane, ChartStyle.Bar, true);
                        break;

                    case StatsMode.smBirthByMonth:
                        this.fChartXTitle = LangMan.LS(LSID.LSID_Month);
                        this.fChartYTitle = LangMan.LS(LSID.LSID_People);
                        this.PrepareArray(gPane, ChartStyle.Bar, true);
                        break;

                    case StatsMode.smDemography:
                        this.fChartXTitle = LangMan.LS(LSID.LSID_LifeExpectancy);
                        this.fChartYTitle = LangMan.LS(LSID.LSID_People);
                        this.PrepareArray(gPane, ChartStyle.ClusterBar, true, vals);
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

            this.lvSummary.Items.Clear();

            ListViewItem item = this.lvSummary.Items.Add(LangMan.LS(LSID.LSID_People));
            item.SubItems.Add(stats.persons.ToString());
            item.SubItems.Add(stats.persons_m.ToString() + GetPercent(stats.persons_m, stats.persons));
            item.SubItems.Add(stats.persons_f.ToString() + GetPercent(stats.persons_f, stats.persons));

            item = this.lvSummary.Items.Add(LangMan.LS(LSID.LSID_Living));
            item.SubItems.Add(stats.lives.ToString());
            item.SubItems.Add(stats.lives_m.ToString());
            item.SubItems.Add(stats.lives_f.ToString());

            item = this.lvSummary.Items.Add(LangMan.LS(LSID.LSID_Deads));
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
            ListViewItem lvItem = this.lvSummary.Items.Add(LangMan.LS(name));
            lvItem.SubItems.Add(string.Format("{0:0.00}", item.CommonVal));
            lvItem.SubItems.Add(string.Format("{0:0.00}", item.MaleVal));
            lvItem.SubItems.Add(string.Format("{0:0.00}", item.FemaleVal));
        }

        private void StatisticsWin_KeyDown(object sender, KeyEventArgs e)
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

        private void StatisticsWin_Load(object sender, EventArgs e)
        {
            this.UpdateCommonStats();
        }

        public void SetLang()
        {
            this.Text = LangMan.LS(LSID.LSID_MIStats);
            this.grpSummary.Text = LangMan.LS(LSID.LSID_Summary);

            this.ColumnHeader1.Text = LangMan.LS(LSID.LSID_Parameter);
            this.ColumnHeader2.Text = LangMan.LS(LSID.LSID_Total);
            this.ColumnHeader3.Text = LangMan.LS(LSID.LSID_ManSum);
            this.ColumnHeader4.Text = LangMan.LS(LSID.LSID_WomanSum);

            this.tbExcelExport.ToolTipText = LangMan.LS(LSID.LSID_MIExportToExcelFile);
            this.UpdateCommonStats();

            int oldIndex = this.cbType.SelectedIndex;
            this.UpdateStatsTypes();
            this.cbType.SelectedIndex = oldIndex;
        }

        // TODO: localize?
        private void tbExcelExport_Click(object sender, EventArgs e)
        {
            string fileName = UIHelper.GetSaveFile("", "", "Excel files (*.xls)|*.xls", 1, "xls", "");
            if (string.IsNullOrEmpty(fileName)) return;

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
                    workbook.Save(fileName);

                    if (File.Exists(fileName)) {
                        Process.Start(fileName);
                    }
                }
                finally
                {
                    this.fBase.ProgressDone();
                }
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("StatisticsWin.ExcelExport(): " + ex.Message);
                GKUtils.ShowError(LangMan.LS(LSID.LSID_UploadErrorInExcel));
            }
        }
    }
}
