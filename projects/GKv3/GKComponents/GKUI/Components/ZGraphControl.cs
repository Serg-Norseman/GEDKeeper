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
using GKCore.MVP.Controls;
using GKCore.Stats;
using ScottPlot.Eto;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ZGraphControl : Panel, IGraphControl
    {
        private class XYPair
        {
            public double X;
            public double Y;
        }

        private readonly PlotView fGraph;

        public ZGraphControl()
        {
            fGraph = new PlotView();
            Content = fGraph;
        }

        public void Activate()
        {
            Focus();
        }

        public void Clear()
        {
            fGraph.Plot.Clear();
            fGraph.Refresh();
        }

        public void PrepareArray(string title, string xAxis, string yAxis, ChartStyle style, bool excludeUnknowns, List<StatsItem> vals)
        {
            try {
                fGraph.Plot.Clear();

                fGraph.Plot. Title(title, true);
                fGraph.Plot.XAxis.Label(xAxis);
                fGraph.Plot.YAxis.Label(yAxis);

                if (style != ChartStyle.ClusterBar) {
                    var ppList = new List<XYPair>();

                    int num = vals.Count;
                    for (int i = 0; i < num; i++) {
                        StatsItem item = vals[i];

                        string s = item.Caption;
                        double lab = (s == "?") ? 0.0f : ConvertHelper.ParseFloat(s, 0.0f, true);

                        if (lab != 0.0d || !excludeUnknowns) {
                            ppList.Add(new XYPair() { X = lab, Y = item.Value });
                        }
                    }
                    ppList.Sort((a, b) => b.X.CompareTo(a.X));

                    switch (style) {
                        case ChartStyle.Bar: {
                                var values = new double[ppList.Count];
                                var positions = new double[ppList.Count];
                                for (int i = 0; i < ppList.Count; i++) {
                                    XYPair xyPair = ppList[i];
                                    values[i] = xyPair.Y;
                                    positions[i] = xyPair.X;
                                }
                                fGraph.Plot.AddBar(values, positions);
                            }
                            break;

                        case ChartStyle.Point: {
                                var scatterList = fGraph.Plot.AddScatterList(markerSize: 7, markerShape: ScottPlot.MarkerShape.filledDiamond);
                                foreach (var xyPair in ppList) {
                                    scatterList.Add(xyPair.X, xyPair.Y);
                                }
                            }
                            break;
                    }
                } else {
                    int itemscount = vals.Count;
                    double[] yValuesF = new double[itemscount];
                    double[] yValuesM = new double[itemscount];
                    double[] xValues = new double[itemscount];

                    for (int i = 0; i < itemscount; i++) {
                        StatsItem sti = vals[i];
                        xValues[i] = ConvertHelper.ParseInt(sti.Caption, 0);
                        yValuesF[i] = sti.ValF;
                        yValuesM[i] = sti.ValM + sti.ValF;
                    }

                    fGraph.Plot.AddBar(yValuesM, xValues, System.Drawing.Color.Blue).Label = "M";
                    fGraph.Plot.AddBar(yValuesF, xValues, System.Drawing.Color.Red).Label = "F";
                }
            } finally {
                fGraph.Refresh();
            }
        }
    }
}
