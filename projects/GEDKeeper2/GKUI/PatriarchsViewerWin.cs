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

using System.Drawing;
using System.Windows.Forms;

using ArborGVT;
using GKCommon.GEDCOM;
using GKCommon.SmartGraph;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKUI
{
    public partial class PatriarchsViewerWin : Form
    {
        private readonly IBaseWindow fBase;
        private readonly ToolTip fTip;
        private bool fTipShow;

        public PatriarchsViewerWin(IBaseWindow baseWin, int minGens)
        {
            InitializeComponent();

            fBase = baseWin;
            fTip = new ToolTip();
            fTipShow = false;

            using (Graph graph = PatriarchsMan.GetPatriarchsGraph(fBase.Context, minGens, false, true)) {
                PL_ConvertGraphToArborSystem(graph, arborViewer1.Sys);
            }

            arborViewer1.Name = "arborViewer1";
            arborViewer1.NodesDragging = true;
            arborViewer1.start();
        }

        private void ArborViewer1_MouseMove(object sender, MouseEventArgs e)
        {
            ArborNode resNode = arborViewer1.getNodeByCoord(e.X, e.Y);

            if (resNode == null) {
                if (fTipShow) {
                    fTip.Hide(arborViewer1);
                    fTipShow = false;
                }
            } else {
                if (!fTipShow) {
                    string xref = resNode.Sign;
                    //GEDCOMIndividualRecord iRec = fBase.Tree.XRefIndex_Find(xref) as GEDCOMIndividualRecord;
                    //string txt = iRec.GetNameString(true, false) + " [" + xref + "]";

                    GEDCOMFamilyRecord famRec = fBase.Tree.XRefIndex_Find(xref) as GEDCOMFamilyRecord;
                    string txt = GKUtils.GetFamilyString(famRec) + " [" + xref + "] "/* + resNode.Mass.ToString()*/;

                    fTip.Show(txt, arborViewer1, e.X + 24, e.Y);
                    fTipShow = true;
                }
            }
        }

        private static void PL_ConvertGraphToArborSystem(IGraph graph, ArborSystem sys)
        {
            foreach (Vertex vtx in graph.Vertices) {
                ArborNode arbNode = sys.addNode(vtx.Sign);
                PGNode pgNode = (PGNode)vtx.Value;

                arbNode.Color = (pgNode.Type == PGNodeType.Intersection) ? Color.BlueViolet : Color.Navy;
                arbNode.Mass = pgNode.Size;
            }

            foreach (Edge edge in graph.Edges) {
                sys.addEdge(edge.Source.Sign, edge.Target.Sign);
            }
        }
    }
}
