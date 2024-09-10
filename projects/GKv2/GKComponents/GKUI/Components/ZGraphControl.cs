/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using System.Drawing;
using System.Windows.Forms;
using BSLib;
using GKCore.Design.Controls;
using GKCore.Stats;
using ZedGraph;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ZGraphControl : UserControl, IGraphControl
    {
        private readonly ZedGraphControl fGraph;

        public ZGraphControl()
        {
            fGraph = new ZedGraphControl();
            fGraph.IsShowPointValues = true;
            fGraph.Dock = DockStyle.Fill;
            Controls.Add(fGraph);
        }

        public void Activate()
        {
            Select();
        }

        public void Clear()
        {
            GraphPane gPane = fGraph.GraphPane;
            gPane.Title.Text = "";
            gPane.XAxis.Title.Text = "";
            gPane.YAxis.Title.Text = "";
            gPane.CurveList.Clear();

            fGraph.AxisChange();
            fGraph.Invalidate();
        }

        public void PrepareArray(string title, string xAxis, string yAxis, ChartStyle style, bool excludeUnknowns, List<StatsItem> vals)
        {
            GraphPane gPane = fGraph.GraphPane;
            try {
                gPane.CurveList.Clear();

                gPane.Title.Text = title;
                gPane.XAxis.Title.Text = xAxis;
                gPane.YAxis.Title.Text = yAxis;

                gPane.IsFontsScaled = false;

                if (style != ChartStyle.ClusterBar) {
                    PointPairList ppList = new PointPairList();

                    int num = vals.Count;
                    for (int i = 0; i < num; i++) {
                        StatsItem item = vals[i];

                        string s = item.Caption;
                        double lab = (s == "?") ? 0.0f : ConvertHelper.ParseFloat(s, 0.0f, true);

                        if (lab != 0.0d || !excludeUnknowns) {
                            ppList.Add(lab, item.Value);
                        }
                    }
                    ppList.Sort();

                    switch (style) {
                        case ChartStyle.Bar:
                            gPane.AddBar("-", ppList, Color.Green);
                            break;

                        case ChartStyle.Point:
                            gPane.AddCurve("-", ppList, Color.Green, SymbolType.Diamond).Symbol.Size = 3;
                            break;
                    }
                } else {
                    gPane.CurveList.Clear();

                    int itemscount = vals.Count;
                    double[] yValuesF = new double[itemscount];
                    double[] yValuesM = new double[itemscount];
                    double[] xValues = new double[itemscount];

                    for (int i = 0; i < itemscount; i++) {
                        StatsItem sti = vals[i];
                        xValues[i] = ConvertHelper.ParseInt(sti.Caption, 0);
                        yValuesF[i] = sti.ValF;
                        yValuesM[i] = sti.ValM;
                    }

                    gPane.AddBar("F", xValues, yValuesF, Color.Red);
                    gPane.AddBar("M", xValues, yValuesM, Color.Blue);

                    gPane.BarSettings.MinBarGap = 0.0f;
                    gPane.BarSettings.MinClusterGap = 2.5f;

                    // expand the range of the Y axis slightly to accommodate the labels
                    //gPane.YAxis.Scale.Max += gPane.YAxis.Scale.MajorStep;

                    // Create TextObj's to provide labels for each bar
                    BarItem.CreateBarLabels(gPane, false, "f0");
                }
            } finally {
                fGraph.AxisChange();
                fGraph.Invalidate();
            }
        }
    }
}
