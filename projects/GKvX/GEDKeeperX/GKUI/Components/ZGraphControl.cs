/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using BSLib;
using GKCore;
using GKCore.Design.Controls;
using GKCore.Stats;
using OxyPlot;
using OxyPlot.Axes;
using OxyPlot.Series;
using OxyPlot.Xamarin.Forms;
using Xamarin.Forms;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ZGraphControl : ContentView, IGraphControl
    {
        private readonly PlotView fGraph;
        private PlotModel fModel;

        public bool Enabled
        {
            get { return base.IsEnabled; }
            set { base.IsEnabled = value; }
        }

        public bool Visible
        {
            get { return base.IsVisible; }
            set { base.IsVisible = value; }
        }


        public ZGraphControl()
        {
            fModel = new PlotModel();

            fGraph = new PlotView();
            fGraph.Model = fModel;
            Content = fGraph;
        }

        public void Activate()
        {
            Focus();
        }

        public void Clear()
        {
            fModel = new PlotModel();
            fGraph.Model = fModel;
        }

        public void PrepareArray(string title, string xAxis, string yAxis, ChartStyle style, bool excludeUnknowns, List<StatsItem> vals)
        {
            try {
                Clear();

                fModel.Title = title;
                fModel.Background = OxyColors.White;

                vals.Sort((a, b) => GKUtils.StrCompareEx(b.Caption, a.Caption));

                switch (style) {
                    case ChartStyle.Bar: {
                            var categoryAxis = new CategoryAxis { Position = AxisPosition.Left, Title = xAxis };
                            fModel.Axes.Add(categoryAxis);

                            var series = new BarSeries() { Title = yAxis };
                            for (int i = 0; i < vals.Count; i++) {
                                StatsItem item = vals[i];
                                if (item.Caption != "?" || !excludeUnknowns) {
                                    categoryAxis.Labels.Add(item.Caption);
                                    series.Items.Add(new BarItem(item.Value, i));
                                }
                            }
                            fModel.Series.Add(series);
                        }
                        break;

                    case ChartStyle.Point: {
                            var series = new LineSeries() { MarkerType = MarkerType.Diamond, MarkerSize = 4, Title = yAxis };
                            for (int i = 0; i < vals.Count; i++) {
                                StatsItem item = vals[i];
                                string s = item.Caption;
                                double lab = (s == "?") ? 0.0f : ConvertHelper.ParseFloat(s, 0.0f, true);
                                if (lab != 0.0d || !excludeUnknowns) {
                                    series.Points.Add(new DataPoint(lab, item.Value));
                                }
                            }
                            fModel.Series.Add(series);
                        }
                        break;

                    case ChartStyle.ClusterBar: {
                            var categoryAxis = new CategoryAxis { Position = AxisPosition.Left, Title = xAxis };
                            fModel.Axes.Add(categoryAxis);

                            var seriesF = new BarSeries() { FillColor = OxyColors.Red, Title = "F" };
                            var seriesM = new BarSeries() { FillColor = OxyColors.Blue, Title = "M" };
                            for (int i = 0; i < vals.Count; i++) {
                                StatsItem sti = vals[i];
                                categoryAxis.Labels.Add(sti.Caption);
                                seriesF.Items.Add(new BarItem(sti.ValF, i));
                                seriesM.Items.Add(new BarItem(sti.ValM, i));
                            }
                            fModel.Series.Add(seriesF);
                            fModel.Series.Add(seriesM);
                        }
                        break;
                }
            } finally {
            }
        }
    }
}
