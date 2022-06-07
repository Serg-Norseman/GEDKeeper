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

using System;
using BSLib.DataViz.ArborGVT;
using BSLib.DataViz.SmartGraph;
using Eto.Drawing;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Tools;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class PatriarchsViewerWin : CommonWindow, IPatriarchsViewer
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private ArborViewer arborViewer1;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly IBaseWindow fBase;
        private readonly int fMinGens;
        private bool fTipShow;

        public PatriarchsViewerWin(IBaseWindow baseWin, int minGens)
        {
            XamlReader.Load(this);

            fBase = baseWin;
            fMinGens = minGens;

            fTipShow = false;
        }

        private void LoadGraph()
        {
            Graph graph = null;
            AppHost.Instance.ExecuteWork((controller) => {
                graph = PatriarchsMan.GetPatriarchsGraph(fBase.Context, fMinGens, false, true, controller);
            });

            using (graph) {
                ArborSystem sys = arborViewer1.Sys;

                foreach (Vertex vtx in graph.Vertices) {
                    var arbNode = sys.AddNode(vtx.Sign) as ArborNodeEx;
                    PGNode pgNode = (PGNode)vtx.Value;

                    arbNode.Color = (pgNode.Type == PGNodeType.Intersection) ? Colors.BlueViolet : Colors.Navy;
                    arbNode.Mass = pgNode.Size;
                }

                foreach (Edge edge in graph.Edges) {
                    sys.AddEdge(edge.Source.Sign, edge.Target.Sign);
                }
            }
        }

        private void Form_Load(object sender, EventArgs e)
        {
            LoadGraph();
            arborViewer1.start();
        }

        private void ArborViewer1_MouseMove(object sender, MouseEventArgs e)
        {
            Point mpt = new Point(e.Location);
            ArborNode resNode = arborViewer1.getNodeByCoord(mpt.X, mpt.Y);

            if (resNode == null) {
                if (fTipShow) {
                    arborViewer1.ToolTip = string.Empty;
                    fTipShow = false;
                }
            } else {
                if (!fTipShow) {
                    string xref = resNode.Sign;
                    GDMFamilyRecord famRec = fBase.Context.Tree.XRefIndex_Find(xref) as GDMFamilyRecord;
                    string txt = GKUtils.GetFamilyString(fBase.Context.Tree, famRec) + " [" + xref + "] "/* + resNode.Mass.ToString()*/;

                    arborViewer1.ToolTip = txt;
                    fTipShow = true;
                }
            }
        }
    }
}
