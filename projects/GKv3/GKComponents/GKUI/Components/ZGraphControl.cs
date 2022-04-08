/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib;
using Eto.Forms;
using GKCore;
using GKCore.MVP.Controls;
using GKCore.Stats;
using OxyPlot;
using OxyPlot.Axes;
using OxyPlot.Eto;
using OxyPlot.Series;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ZGraphControl : Panel, IGraphControl
    {
        private readonly PlotView fGraph;
        private PlotModel fModel;

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
                            var categoryAxis = new CategoryAxis { Position = AxisPosition.Left };
                            fModel.Axes.Add(categoryAxis);

                            var series = new BarSeries();
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
                            var series = new LineSeries() { MarkerType = MarkerType.Diamond, MarkerSize = 4 };
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
                            var categoryAxis = new CategoryAxis { Position = AxisPosition.Left };
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
