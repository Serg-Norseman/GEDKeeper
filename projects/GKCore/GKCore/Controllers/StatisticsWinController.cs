/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Stats;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class StatisticsWinController : FormController<IStatisticsWin>
    {
        private readonly List<GDMRecord> fSelectedRecords;

        private string fChartTitle;
        private string fChartXTitle;
        private string fChartYTitle;
        private StatsMode fCurrentMode;
        private List<StatsItem> fCurrentValues;
        private TreeStats fTreeStats;

        public TreeStats TreeStats
        {
            get { return fTreeStats; }
        }

        public StatisticsWinController(IStatisticsWin view, List<GDMRecord> selectedRecords) : base(view)
        {
            fSelectedRecords = selectedRecords;
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);
            fTreeStats = new TreeStats(fBase.Context, fSelectedRecords);
        }

        public override void UpdateView()
        {
        }

        public void CalcStats()
        {
            fCurrentMode = fView.StatsType.GetSelectedTag<StatsMode>();

            fView.ListStats.SetColumnCaption(0, LangMan.LS(GKData.StatsTitles[(int)fCurrentMode].Cap));
            fView.ListStats.SetColumnCaption(1, LangMan.LS(LSID.Value));

            fView.ListStats.BeginUpdate();
            fView.ListStats.ClearItems();
            try {
                fCurrentValues = new List<StatsItem>();
                fTreeStats.GetSpecStats(fCurrentMode, fCurrentValues);

                foreach (StatsItem lv in fCurrentValues) {
                    fView.ListStats.AddItem(null, lv.Caption, lv.GetDisplayString());
                }
            } finally {
                fView.ListStats.EndUpdate();
            }

            fChartTitle = LangMan.LS(GKData.StatsTitles[(int)fCurrentMode].Title);

            switch (fCurrentMode) {
                case StatsMode.smAge:
                    fChartXTitle = LangMan.LS(LSID.Age);
                    fChartYTitle = LangMan.LS(LSID.People);
                    fView.Graph.PrepareArray(fChartTitle, fChartXTitle, fChartYTitle, ChartStyle.Point, true, fCurrentValues);
                    break;

                case StatsMode.smLifeExpectancy:
                    fChartXTitle = LangMan.LS(LSID.LifeExpectancy);
                    fChartYTitle = LangMan.LS(LSID.People);
                    fView.Graph.PrepareArray(fChartTitle, fChartXTitle, fChartYTitle, ChartStyle.Point, true, fCurrentValues);
                    break;

                case StatsMode.smBirthYears:
                case StatsMode.smBirthTenYears:
                case StatsMode.smDeathYears:
                case StatsMode.smDeathTenYears:
                    {
                        switch (fCurrentMode) {
                            case StatsMode.smBirthYears:
                            case StatsMode.smDeathYears:
                                fChartXTitle = LangMan.LS(LSID.Years);
                                break;

                            case StatsMode.smBirthTenYears:
                            case StatsMode.smDeathTenYears:
                                fChartXTitle = LangMan.LS(LSID.Decennial);
                                break;
                        }

                        switch (fCurrentMode) {
                            case StatsMode.smBirthYears:
                            case StatsMode.smBirthTenYears:
                                fChartYTitle = LangMan.LS(LSID.HowBirthes);
                                break;

                            case StatsMode.smDeathYears:
                            case StatsMode.smDeathTenYears:
                                fChartYTitle = LangMan.LS(LSID.HowDeads);
                                break;
                        }

                        fView.Graph.PrepareArray(fChartTitle, fChartXTitle, fChartYTitle, ChartStyle.Point, true, fCurrentValues);
                        break;
                    }

                case StatsMode.smChildsDistribution:
                    fChartXTitle = LangMan.LS(LSID.Childs);
                    fChartYTitle = LangMan.LS(LSID.Parents);
                    fView.Graph.PrepareArray(fChartTitle, fChartXTitle, fChartYTitle, ChartStyle.Bar, true, fCurrentValues);
                    break;

                case StatsMode.smCertaintyIndex:
                    fChartXTitle = LangMan.LS(LSID.CertaintyIndex);
                    fChartYTitle = LangMan.LS(LSID.People);
                    fView.Graph.PrepareArray(fChartTitle, fChartXTitle, fChartYTitle, ChartStyle.Bar, true, fCurrentValues);
                    break;

                case StatsMode.smBirthByMonth:
                    fChartXTitle = LangMan.LS(LSID.Month);
                    fChartYTitle = LangMan.LS(LSID.People);
                    fView.Graph.PrepareArray(fChartTitle, fChartXTitle, fChartYTitle, ChartStyle.Bar, true, fCurrentValues);
                    break;

                case StatsMode.smDemography:
                    fChartXTitle = LangMan.LS(LSID.LifeExpectancy);
                    fChartYTitle = LangMan.LS(LSID.People);
                    fView.Graph.PrepareArray(fChartTitle, fChartXTitle, fChartYTitle, ChartStyle.ClusterBar, true, fCurrentValues);
                    break;

                case StatsMode.smParentsAge:
                    fChartXTitle = LangMan.LS(LSID.ParentsAge);
                    fChartYTitle = LangMan.LS(LSID.People);
                    fView.Graph.PrepareArray(fChartTitle, fChartXTitle, fChartYTitle, ChartStyle.ClusterBar, true, fCurrentValues);
                    break;

                default:
                    fView.Graph.Clear();
                    break;
            }
        }

        public void UpdateStatsTypes()
        {
            bool hasPatronymic = fBase != null && fBase.Context.Culture.HasPatronymic;

            fView.StatsType.BeginUpdate();
            fView.StatsType.Clear();
            for (StatsMode sm = StatsMode.smAncestors; sm <= StatsMode.smLast; sm++) {
                if (sm == StatsMode.smPatronymics && !hasPatronymic) continue;

                GKData.StatsTitleStruct tr = GKData.StatsTitles[(int)sm];
                fView.StatsType.AddItem(LangMan.LS(tr.Title), sm);
            }
            fView.StatsType.EndUpdate();
        }

        private static string GetPercent(int dividend, int divisor)
        {
            double val = ((divisor == 0) ? 0.0d : (dividend / (double)divisor * 100.0d));
            return string.Format(" ({0:0.00}%)", val);
        }

        public void UpdateCommonStats()
        {
            CommonStats stats = fTreeStats.GetCommonStats();

            fView.Summary.BeginUpdate();
            try {
                fView.Summary.ClearItems();

                fView.Summary.AddItem(null, LangMan.LS(LSID.People),
                    stats.persons.ToString(),
                    stats.persons_m.ToString() + GetPercent(stats.persons_m, stats.persons),
                    stats.persons_f.ToString() + GetPercent(stats.persons_f, stats.persons));

                fView.Summary.AddItem(null, LangMan.LS(LSID.Living),
                    stats.lives.ToString(),
                    stats.lives_m.ToString(),
                    stats.lives_f.ToString());

                fView.Summary.AddItem(null, LangMan.LS(LSID.Deads),
                    (stats.persons - stats.lives).ToString(),
                    (stats.persons_m - stats.lives_m).ToString(),
                    (stats.persons_f - stats.lives_f).ToString());

                AddCompositeItem(LSID.AvgAge, stats.age);
                AddCompositeItem(LSID.AvgLife, stats.life);
                AddCompositeItem(LSID.AvgChilds, stats.childs);
                AddCompositeItem(LSID.AvgBorn, stats.fba);
                AddCompositeItem(LSID.AvgMarriagesCount, stats.marr);
                AddCompositeItem(LSID.AvgMarriagesAge, stats.mage);
                AddCompositeItem(LSID.CertaintyIndex, stats.cIndex);
            } finally {
                fView.Summary.EndUpdate();
            }
        }

        private void AddCompositeItem(LSID name, CompositeItem item)
        {
            fView.Summary.AddItem(null, LangMan.LS(name),
                string.Format("{0:0.00}", item.CommonVal),
                string.Format("{0:0.00}", item.MaleVal),
                string.Format("{0:0.00}", item.FemaleVal));
        }

        public async void ExportToExcel()
        {
            string fileName = await TableExporter.GetTableFile();
            var writer = TableExporter.GetTableWriter(fileName);
            if (writer == null) return;

            AppHost.Instance.ExecuteWork((controller) => {
                fTreeStats.WriteStatsReport(fChartTitle,
                    LangMan.LS(GKData.StatsTitles[(int)fCurrentMode].Cap),
                    LangMan.LS(LSID.Value),
                    fCurrentValues, fileName, writer, controller);
            });
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.MIStats);

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<IGroupBox>("grpSummary").Text = LangMan.LS(LSID.Summary);
            }

            var lvSummary = fView.Summary;
            lvSummary.ClearColumns();
            lvSummary.AddColumn(LangMan.LS(LSID.Parameter), 300, false);
            lvSummary.AddColumn(LangMan.LS(LSID.Total), 100, false);
            lvSummary.AddColumn(LangMan.LS(LSID.ManSum), 100, false);
            lvSummary.AddColumn(LangMan.LS(LSID.WomanSum), 100, false);

            SetToolTip("tbExcelExport", LangMan.LS(LSID.ExportTable));

            int oldIndex = fView.StatsType.SelectedIndex;
            UpdateStatsTypes();
            fView.StatsType.SelectedIndex = oldIndex;
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("tbExcelExport").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_ExportTable);
        }
    }
}
