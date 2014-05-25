using System;
using System.Drawing;
using System.Windows.Forms;

using ExtUtils.ArborEngine;
using ExtUtils.Graph;
using GedCom551;
using GKCore;
using GKCore.Interfaces;

namespace GKUI
{
	public partial class PatriarchsViewer : Form
	{
        private readonly IBase fBase;
        private readonly ToolTip fTip;
        private bool fTipShow;

        public PatriarchsViewer(IBase aBase, int minGens)
		{
			InitializeComponent();

			this.fBase = aBase;
            this.fTip = new System.Windows.Forms.ToolTip();
            this.fTipShow = false;

        	TGraph graph = this.fBase.Context.GetPatriarchsGraph(minGens, false);
        	this.PL_ConvertGraphToArborSystem(graph, arborViewer1.Sys);

        	arborViewer1.start();
		}

		void ArborViewer1MouseMove(object sender, MouseEventArgs e)
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
					//TGEDCOMIndividualRecord iRec = this.fBase.Tree.XRefIndex_Find(xref) as TGEDCOMIndividualRecord;
					//string txt = iRec.aux_GetNameStr(true, false) + " [" + xref + "]";

					TGEDCOMFamilyRecord famRec = this.fBase.Tree.XRefIndex_Find(xref) as TGEDCOMFamilyRecord;
					string txt = GKUtils.aux_GetFamilyStr(famRec) + " [" + xref + "]";

					fTip.Show(txt, arborViewer1, new Point(e.X + 24, e.Y));
					fTipShow = true;
				}
			}
		}

		private void PL_ConvertGraphToArborSystem(IGraph graph, ArborSystem sys)
		{
			foreach (IVertex vtx in graph.Vertices) {
				ArborNode arbNode = sys.addNode(vtx.Sign);
				PatriarchsGraphNode pgNode = (PatriarchsGraphNode)vtx.Value;

				if (pgNode.Type == NodeType.ntIntersection) {
					arbNode.Color = Color.BlueViolet;
				} else {
					arbNode.Color = Color.Navy;
				}
			}

			foreach (IEdge edge in graph.Edges) {
				sys.addEdge(edge.Source.Sign, edge.Target.Sign, 1);
			}
		}

	}
}
