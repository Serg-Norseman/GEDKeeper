﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Drawing;
using System.Windows.Forms;
using BSLib.DataViz.ArborGVT;
using BSLib.DataViz.SmartGraph;
using GDModel;
using GKCore;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Tools;
using GKCore.Types;
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
                    GDMFamilyRecord famRec = fBase.Context.Tree.XRefIndex_Find(xref) as GDMFamilyRecord;
                    string txt = GKUtils.GetFamilyString(fBase.Context.Tree, famRec) + " [" + xref + "] "/* + resNode.Mass.ToString()*/;

                    fTip.Show(txt, arborViewer1, e.X + 24, e.Y);
                    fTipShow = true;
                }
            }
        }
    }
}
