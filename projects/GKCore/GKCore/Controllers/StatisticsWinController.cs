/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Threading.Tasks;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Export;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;
using GKCore.Stats;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class StatisticsWinController : FormController<IStatisticsWin>
    {
        private readonly List<StatsItem> fCurrentValues;
        private readonly List<GDMRecord> fSelectedRecords;
        private readonly List<SummaryItem> fSummaryValues;

        private string fChartTitle;
        private string fChartXTitle;
        private string fChartYTitle;
        private StatsMode fCurrentMode;
        private TreeStats fTreeStats;

        public TreeStats TreeStats
        {
            get { return fTreeStats; }
        }

        public StatisticsWinController(IStatisticsWin view, List<GDMRecord> selectedRecords) : base(view)
        {
            fSelectedRecords = selectedRecords;

            fCurrentValues = new List<StatsItem>();
            var listModel = new StatItemsListModel("-");
            listModel.DataSource = fCurrentValues;
            fView.ListStats.ListMan = listModel;
            fView.ListStats.UpdateContents();

            fSummaryValues = new List<SummaryItem>();
            var sumModel = new SummaryListModel();
            sumModel.DataSource = fSummaryValues;
            fView.Summary.ListMan = sumModel;
            fView.Summary.UpdateContents();
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

            fTreeStats.GetSpecStats(fCurrentMode, fCurrentValues);

            fView.ListStats.SetColumnCaption(0, LangMan.LS(GKData.StatsTitles[(int)fCurrentMode].Cap));
            fView.ListStats.UpdateContents();

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
                case StatsMode.smDeathTenYears: {
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

            fSummaryValues.Clear();
            try {
                fSummaryValues.Add(new SummaryItem(LangMan.LS(LSID.People),
                    stats.persons.ToString(),
                    stats.persons_m.ToString() + GetPercent(stats.persons_m, stats.persons),
                    stats.persons_f.ToString() + GetPercent(stats.persons_f, stats.persons)));

                fSummaryValues.Add(new SummaryItem(LangMan.LS(LSID.Living),
                    stats.lives.ToString(),
                    stats.lives_m.ToString(),
                    stats.lives_f.ToString()));

                fSummaryValues.Add(new SummaryItem(LangMan.LS(LSID.Deads),
                    (stats.persons - stats.lives).ToString(),
                    (stats.persons_m - stats.lives_m).ToString(),
                    (stats.persons_f - stats.lives_f).ToString()));

                AddSummaryItem(LSID.AvgAge, stats.age);
                AddSummaryItem(LSID.AvgLife, stats.life);
                AddSummaryItem(LSID.AvgChilds, stats.childs);
                AddSummaryItem(LSID.AvgBorn, stats.fba);
                AddSummaryItem(LSID.AvgMarriagesCount, stats.marr);
                AddSummaryItem(LSID.AvgMarriagesAge, stats.mage);
                AddSummaryItem(LSID.CertaintyIndex, stats.cIndex);
            } finally {
                fView.Summary.SortOrder = GKSortOrder.None;
                fView.Summary.UpdateContents();
            }
        }

        private void AddSummaryItem(LSID name, CompositeItem item)
        {
            fSummaryValues.Add(new SummaryItem(LangMan.LS(name),
                string.Format("{0:0.00}", item.CommonVal),
                string.Format("{0:0.00}", item.MaleVal),
                string.Format("{0:0.00}", item.FemaleVal)));
        }

        public async Task ExportToExcel()
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
            fView.SetTitle(LangMan.LS(LSID.MIStats));

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<IGroupBox>("grpSummary").Text = LangMan.LS(LSID.Summary);
            }

            SetToolTip("tbExcelExport", LangMan.LS(LSID.ExportTable));

            int oldIndex = fView.StatsType.SelectedIndex;
            UpdateStatsTypes();
            fView.StatsType.SelectedIndex = oldIndex;
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

#if NETCOREAPP
            GetControl<IButton>("tbExcelExport").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_ExportTable);
#else
            GetControl<IToolItem>("tbExcelExport").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_ExportTable);
#endif
        }


        private sealed class StatItemsListModel : SimpleListModel<StatsItem>
        {
            public StatItemsListModel(string title) :
                base(null, CreateListColumns(title))
            {
            }

            public static ListColumns CreateListColumns(string title)
            {
                var result = new ListColumns(GKListType.ltNone);
                result.AddColumn(title, DataType.dtString, 250, true);
                result.AddColumn(LangMan.LS(LSID.Value), DataType.dtString, 150, true);
                return result;
            }

            protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
            {
                object result = null;
                switch (colType) {
                    case 0:
                        result = fFetchedRec.Caption;
                        break;
                    case 1:
                        result = fFetchedRec.GetDisplayString();
                        break;
                }
                return result;
            }
        }


        public sealed class SummaryItem
        {
            public string Name;
            public string CommonVal;
            public string MaleVal;
            public string FemaleVal;

            public SummaryItem(string name, string commonVal, string maleVal, string femaleVal)
            {
                Name = name;
                CommonVal = commonVal;
                MaleVal = maleVal;
                FemaleVal = femaleVal;
            }
        }

        private sealed class SummaryListModel : SimpleListModel<SummaryItem>
        {
            public SummaryListModel() :
                base(null, CreateListColumns())
            {
            }

            public static ListColumns CreateListColumns()
            {
                var result = new ListColumns(GKListType.ltNone);
                result.AddColumn(LangMan.LS(LSID.Parameter), DataType.dtString, 300, false);
                result.AddColumn(LangMan.LS(LSID.Total), DataType.dtString, 100, false); // , false, "0.00", null
                result.AddColumn(LangMan.LS(LSID.ManSum), DataType.dtString, 100, false); // , false, "0.00", null
                result.AddColumn(LangMan.LS(LSID.WomanSum), DataType.dtString, 100, false); // , false, "0.00", null
                return result;
            }

            protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
            {
                object result = null;
                switch (colType) {
                    case 0:
                        result = fFetchedRec.Name;
                        break;
                    case 1:
                        result = fFetchedRec.CommonVal;
                        break;
                    case 2:
                        result = fFetchedRec.MaleVal;
                        break;
                    case 3:
                        result = fFetchedRec.FemaleVal;
                        break;
                }
                return result;
            }
        }
    }
}
