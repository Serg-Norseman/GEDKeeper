/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using BSLib;
using GKCore;
using GKCore.Design.Controls;
using GKCore.Stats;
using Terminal.Gui;
using Terminal.Gui.Graphs;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ZGraphControl : GraphView, IGraphControl
    {

        public ZGraphControl()
        {
        }

        public void Activate()
        {
            SetFocus();
        }

        public new void Clear()
        {
            Reset();
        }

        public void PrepareArray(string title, string xAxis, string yAxis, ChartStyle style, bool excludeUnknowns, List<StatsItem> vals)
        {
            try {
                Clear();

                //fModel.Title = title;
                //fModel.Background = White;
                MarginBottom = 2;
                MarginLeft = 6;

                vals.Sort((a, b) => GKUtils.StrCompareEx(b.Caption, a.Caption));

                switch (style) {
                    case ChartStyle.Bar: {
                            AxisX.Text = xAxis;
                            AxisY.Text = yAxis;
                            var gc = new GraphCellToRender('▒');
                            CellSize = new PointF(1f, 50);

                            var series = new BarSeries();
                            series.Orientation = Orientation.Vertical;
                            for (int i = 0; i < vals.Count; i++) {
                                StatsItem item = vals[i];
                                if (item.Caption != "?" || !excludeUnknowns) {
                                    series.Bars.Add(new BarSeries.Bar(item.Caption, gc, item.Value));
                                }
                            }
                            Series.Add(series);
                        }
                        break;

                    case ChartStyle.Point: {
                            AxisX.Text = xAxis;
                            AxisY.Text = yAxis;

                            var series = new ScatterSeries();
                            for (int i = 0; i < vals.Count; i++) {
                                StatsItem item = vals[i];
                                string s = item.Caption;
                                double lab = (s == "?") ? 0.0f : ConvertHelper.ParseFloat(s, 0.0f, true);
                                if (lab != 0.0d || !excludeUnknowns) {
                                    series.Points.Add(new PointF((float)lab, item.Value));
                                }
                            }
                            Series.Add(series);
                        }
                        break;

                    case ChartStyle.ClusterBar: {
                            /*var categoryAxis = new CategoryAxis { Position = AxisPosition.Left, Title = xAxis };
                            fModel.Axes.Add(categoryAxis);

                            var seriesF = new BarSeries() { FillColor = OxyColors.Red, Title = "F" };
                            var seriesM = new BarSeries() { FillColor = OxyColors.Blue, Title = "M" };
                            for (int i = 0; i < vals.Count; i++) {
                                StatsItem sti = vals[i];
                                categoryAxis.Labels.Add(sti.Caption);
                                seriesF.Items.Add(new BarItem(sti.ValF, i));
                                seriesM.Items.Add(new BarItem(sti.ValM, i));
                            }
                            Series.Add(seriesF);
                            Series.Add(seriesM);*/
                        }
                        break;
                }
            } finally {
            }
        }
    }


    public static class ScaleHelpers
    {
        // https://stackoverflow.com/a/63158920

        //  x - x0    log(y) - log(y0)
        // ------- = -----------------
        // x1 - x0   log(y1) - log(y0)

        public static double ScaleLog(double value, double min, double max, int log = 10)
        {
            //         /  x - x0                                \
            // y = 10^|  ------- * (log(y1) - log(y0)) + log(y0) |
            //         \ x1 - x0                                /

            return Math.Pow(log, ((value - min) / (max - min)) * (Math.Log(max, log) - Math.Log(min, log)) + Math.Log(min, log));
        }

        public static double ScaleExp(double value, double min, double max, int log = 10)
        {
            //                  log(y) - log(y0)
            // x = (x1 - x0) * ----------------- + x0
            //                 log(y1) - log(y0)

            return (max - min) * ((Math.Log(value, log) - Math.Log(min, log)) / (Math.Log(max, log) - Math.Log(min, log))) + min;
        }
    }
}
