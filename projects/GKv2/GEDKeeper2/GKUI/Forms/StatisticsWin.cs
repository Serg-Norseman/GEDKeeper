/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.Drawing;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Stats;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class StatisticsWin : Form, ILocalization, IWindow
    {
        private readonly IBaseWindow fBase;
        private readonly List<GEDCOMRecord> fSelectedRecords;
        private readonly ZGraphControl fGraph;
        private readonly GKListView fListStats;
        private readonly TreeStats fTreeStats;

        private string fChartTitle;
        private string fChartXTitle;
        private string fChartYTitle;
        private List<StatsItem> fCurrentValues;

        public StatisticsWin(IBaseWindow baseWin, List<GEDCOMRecord> selectedRecords)
        {
            InitializeComponent();

            tbExcelExport.Image = GKResources.iExcel;

            fGraph = new ZGraphControl();
            fGraph.Dock = DockStyle.Right;
            fGraph.Size = new Size(400, 200);

            Splitter spl = new Splitter();
            spl.Dock = DockStyle.Right;
            spl.Size = new Size(4, 290);
            spl.MinExtra = 100;
            spl.MinSize = 100;
            Panel1.Controls.Add(fGraph);
            Panel1.Controls.Add(spl);

            fListStats = UIHelper.CreateListView(Panel1);
            fListStats.AddColumn("-", 250, false);
            fListStats.AddColumn("-", 150, false);

            Panel1.Controls.SetChildIndex(fListStats, 0);
            Panel1.Controls.SetChildIndex(spl, 2);
            Panel1.Controls.SetChildIndex(fGraph, 3);
            Panel1.Controls.SetChildIndex(ToolBar1, 4);

            fBase = baseWin;
            fSelectedRecords = selectedRecords;
            fTreeStats = new TreeStats(fBase.Context, fSelectedRecords);

            UpdateStatsTypes();

            SetLang();
        }

        private static string GetPercent(int dividend, int divisor)
        {
            double val = ((divisor == 0) ? 0.0d : (dividend / (double)divisor * 100.0d));
            return string.Format(" ({0:0.00}%)", val);
        }

        private void PrepareArray(ChartStyle style, bool excludeUnknowns, List<StatsItem> vals = null)
        {
            if (style != ChartStyle.ClusterBar) {
                vals = new List<StatsItem>();

                int num = fListStats.Items.Count;
                for (int i = 0; i < num; i++)
                {
                    ListViewItem item = fListStats.Items[i];

                    string s = item.Text;
                    double lab = (s == "?") ? 0.0f : SysUtils.ParseFloat(s, 0.0f, true);

                    if (lab != 0.0d || !excludeUnknowns)
                    {
                        int val = int.Parse(item.SubItems[1].Text);
                        vals.Add(new StatsItem(item.Text, val));
                    }
                }
            }

            fGraph.PrepareArray(fChartTitle, fChartXTitle, fChartYTitle, style, excludeUnknowns, vals);
        }

        private void CalcStats(StatsMode mode)
        {
            fListStats.SortColumn = 0;
            fListStats.Columns[0].Text = LangMan.LS(GKData.StatsTitles[(int)mode].Cap);
            fListStats.Columns[1].Text = LangMan.LS(LSID.LSID_Value);

            fListStats.Sorting = SortOrder.None;
            fListStats.SortColumn = -1;
            fListStats.BeginUpdate();
            fListStats.Items.Clear();

            List<StatsItem> vals = new List<StatsItem>();
            try
            {
                fTreeStats.GetSpecStats(mode, vals);
                fCurrentValues = vals;

                ListViewItem[] items = new ListViewItem[vals.Count];

                int i = 0;
                foreach (StatsItem lv in vals)
                {
                    ListViewItem item = new ListViewItem(lv.Caption);

                    string stVal = lv.GetDisplayString();
                    item.SubItems.Add(stVal);

                    items[i] = item;
                    i++;
                }

                fListStats.Items.AddRange(items);
            }
            finally
            {
                fListStats.EndUpdate();
            }

            fChartTitle = LangMan.LS(GKData.StatsTitles[(int)mode].Title);

            switch (mode) {
                case StatsMode.smAge:
                    fChartXTitle = LangMan.LS(LSID.LSID_Age);
                    fChartYTitle = LangMan.LS(LSID.LSID_People);
                    PrepareArray(ChartStyle.Point, true);
                    break;

                case StatsMode.smLifeExpectancy:
                    fChartXTitle = LangMan.LS(LSID.LSID_LifeExpectancy);
                    fChartYTitle = LangMan.LS(LSID.LSID_People);
                    PrepareArray(ChartStyle.Point, true);
                    break;

                case StatsMode.smBirthYears:
                case StatsMode.smBirthTenYears:
                case StatsMode.smDeathYears:
                    case StatsMode.smDeathTenYears: {
                        switch (mode) {
                            case StatsMode.smBirthYears:
                            case StatsMode.smDeathYears:
                                fChartXTitle = LangMan.LS(LSID.LSID_Years);
                                break;

                            case StatsMode.smBirthTenYears:
                            case StatsMode.smDeathTenYears:
                                fChartXTitle = LangMan.LS(LSID.LSID_Decennial);
                                break;
                        }

                        switch (mode) {
                            case StatsMode.smBirthYears:
                            case StatsMode.smBirthTenYears:
                                fChartYTitle = LangMan.LS(LSID.LSID_HowBirthes);
                                break;

                            case StatsMode.smDeathYears:
                            case StatsMode.smDeathTenYears:
                                fChartYTitle = LangMan.LS(LSID.LSID_HowDeads);
                                break;
                        }

                        PrepareArray(ChartStyle.Point, true);
                        break;
                    }

                case StatsMode.smChildsDistribution:
                    fChartXTitle = LangMan.LS(LSID.LSID_Childs);
                    fChartYTitle = LangMan.LS(LSID.LSID_Parents);
                    PrepareArray(ChartStyle.Bar, true);
                    break;

                case StatsMode.smCertaintyIndex:
                    fChartXTitle = LangMan.LS(LSID.LSID_CertaintyIndex);
                    fChartYTitle = LangMan.LS(LSID.LSID_People);
                    PrepareArray(ChartStyle.Bar, true);
                    break;

                case StatsMode.smBirthByMonth:
                    fChartXTitle = LangMan.LS(LSID.LSID_Month);
                    fChartYTitle = LangMan.LS(LSID.LSID_People);
                    PrepareArray(ChartStyle.Bar, true);
                    break;

                case StatsMode.smDemography:
                    fChartXTitle = LangMan.LS(LSID.LSID_LifeExpectancy);
                    fChartYTitle = LangMan.LS(LSID.LSID_People);
                    PrepareArray(ChartStyle.ClusterBar, true, vals);
                    break;

                default:
                    fGraph.Clear();
                    break;
            }
        }

        private void UpdateCommonStats()
        {
            CommonStats stats = fTreeStats.GetCommonStats();

            lvSummary.Items.Clear();

            ListViewItem item = lvSummary.Items.Add(LangMan.LS(LSID.LSID_People));
            item.SubItems.Add(stats.persons.ToString());
            item.SubItems.Add(stats.persons_m.ToString() + GetPercent(stats.persons_m, stats.persons));
            item.SubItems.Add(stats.persons_f.ToString() + GetPercent(stats.persons_f, stats.persons));

            item = lvSummary.Items.Add(LangMan.LS(LSID.LSID_Living));
            item.SubItems.Add(stats.lives.ToString());
            item.SubItems.Add(stats.lives_m.ToString());
            item.SubItems.Add(stats.lives_f.ToString());

            item = lvSummary.Items.Add(LangMan.LS(LSID.LSID_Deads));
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
            ListViewItem lvItem = lvSummary.Items.Add(LangMan.LS(name));
            lvItem.SubItems.Add(string.Format("{0:0.00}", item.CommonVal));
            lvItem.SubItems.Add(string.Format("{0:0.00}", item.MaleVal));
            lvItem.SubItems.Add(string.Format("{0:0.00}", item.FemaleVal));
        }

        private void StatisticsWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) Close();
        }

        private void cbType_SelectedIndexChanged(object sender, EventArgs e)
        {
            CalcStats((StatsMode)cbType.SelectedIndex);
        }

        private void UpdateStatsTypes()
        {
            ICulture culture = fBase.Context.Culture;

            cbType.BeginUpdate();
            cbType.Items.Clear();
            for (StatsMode sm = StatsMode.smAncestors; sm <= StatsMode.smLast; sm++)
            {
                if (sm == StatsMode.smPatronymics && !culture.HasPatronymic()) continue;

                GKData.StatsTitleStruct tr = GKData.StatsTitles[(int)sm];
                cbType.Items.Add(LangMan.LS(tr.Title));
            }
            cbType.EndUpdate();
        }

        private void StatisticsWin_Load(object sender, EventArgs e)
        {
            UpdateCommonStats();
        }

        public void SetLang()
        {
            Text = LangMan.LS(LSID.LSID_MIStats);
            grpSummary.Text = LangMan.LS(LSID.LSID_Summary);

            ColumnHeader1.Text = LangMan.LS(LSID.LSID_Parameter);
            ColumnHeader2.Text = LangMan.LS(LSID.LSID_Total);
            ColumnHeader3.Text = LangMan.LS(LSID.LSID_ManSum);
            ColumnHeader4.Text = LangMan.LS(LSID.LSID_WomanSum);

            tbExcelExport.ToolTipText = LangMan.LS(LSID.LSID_MIExportToExcelFile);
            UpdateCommonStats();

            int oldIndex = cbType.SelectedIndex;
            UpdateStatsTypes();
            cbType.SelectedIndex = oldIndex;
        }

        private void tbExcelExport_Click(object sender, EventArgs e)
        {
            fTreeStats.WriteStatsReport(cbType.Text, fListStats.Columns[0].Text,
                                        fListStats.Columns[1].Text, fCurrentValues);
        }
    }
}
