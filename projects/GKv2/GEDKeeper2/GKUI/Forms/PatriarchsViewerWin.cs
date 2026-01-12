/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Drawing;
using System.Windows.Forms;
using BSLib.DataViz.ArborGVT;
using BSLib.DataViz.SmartGraph;
using GDModel;
using GKCore;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Tools;
using ArborNodeEx = GKUI.Components.ArborNodeEx;

namespace GKUI.Forms
{
    public partial class PatriarchsViewerWin : CommonWindow, IPatriarchsViewer
    {
        #region Design components

        private readonly ToolTip fTip;

        #endregion

        private readonly IBaseWindow fBase;
        private readonly int fMinGens;
        private bool fTipShow;

        public IWindow OwnerWindow
        {
            get { return fBase; }
        }

        public PatriarchsViewerWin(IBaseWindow baseWin, int minGens)
        {
            InitializeComponent();

            fBase = baseWin;
            fMinGens = minGens;
            fTip = new ToolTip();
            fTipShow = false;
        }

        private void LoadGraph()
        {
            if (fBase == null) return;

            Graph graph = null;
            AppHost.Instance.ExecuteWork((controller) => {
                graph = PatriarchsMan.GetPatriarchsGraph(fBase.Context, fMinGens, false, true, controller);
            });

            using (graph) {
                ArborSystem sys = arborViewer1.Sys;

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
            arborViewer1.start();
        }

        private void ArborViewer1_MouseMove(object sender, MouseEventArgs e)
        {
            Point mpt = e.Location;
            ArborNode resNode = arborViewer1.getNodeByCoord(mpt.X, mpt.Y);

            if (resNode == null) {
                if (fTipShow) {
                    fTip.Hide(arborViewer1);
                    fTipShow = false;
                }
            } else {
                if (!fTipShow) {
                    string xref = resNode.Sign;
                    GDMFamilyRecord famRec = fBase.Context.Tree.FindXRef<GDMFamilyRecord>(xref);
                    string txt = GKUtils.GetFamilyString(fBase.Context.Tree, famRec) + " [" + xref + "] "/* + resNode.Mass.ToString()*/;

                    fTip.Show(txt, arborViewer1, e.X + 24, e.Y);
                    fTipShow = true;
                }
            }
        }
    }
}
