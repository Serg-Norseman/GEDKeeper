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

using System;
using BSLib.DataViz.ArborGVT;
using BSLib.DataViz.SmartGraph;
using GKCore;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Tools;
using GKCore.Types;
using GKUI.Components;
using Xamarin.Forms;

namespace GKUI.Forms
{
    public partial class PatriarchsViewerWin : CommonWindow, IPatriarchsViewer
    {
        private readonly IBaseWindow fBase;
        private readonly int fMinGens;

        public IWindow OwnerWindow
        {
            get { return fBase; }
        }

        public PatriarchsViewerWin(IBaseWindow baseWin, int minGens)
        {
            InitializeComponent();

            fBase = baseWin;
            fMinGens = minGens;

            Appearing += Form_Load;

            arborViewer.EnergyDebug = true;
            arborViewer.NodesDragging = true;
        }

        private void LoadGraph()
        {
            if (fBase == null) {
                return;
            }

            Graph graph = null;
            AppHost.Instance.ExecuteWork((controller) => {
                graph = PatriarchsMan.GetPatriarchsGraph(fBase.Context, fMinGens, false, true, controller);
            });

            using (graph) {
                ArborSystem sys = arborViewer.Sys;

                foreach (Vertex vtx in graph.Vertices) {
                    var arbNode = sys.AddNode(vtx.Sign) as ArborNodeEx;
                    PGNode pgNode = (PGNode)vtx.Value;

                    arbNode.Color = (pgNode.Type == PGNodeType.Intersection) ? Color.BlueViolet : Color.Navy;
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
            arborViewer.Start();
        }

        private void ArborViewer1_MouseMove(object sender, EventArgs e)
        {
            /*Point mpt = new Point(e.Location);
            ArborNode resNode = fArborViewer.getNodeByCoord(mpt.X, mpt.Y);

            if (resNode != null) {
                    string xref = resNode.Sign;
                    GEDCOMFamilyRecord famRec = fBase.Context.Tree.XRefIndex_Find(xref) as GEDCOMFamilyRecord;
                    string txt = GKUtils.GetFamilyString(famRec) + " [" + xref + "] "; //  + resNode.Mass.ToString()

                    //fTip.Show(txt, arborViewer1, mpt.X + 24, mpt.Y);
                    fArborViewer.ToolTip = txt;
                    fTipShow = true;
            }*/
        }
    }
}
