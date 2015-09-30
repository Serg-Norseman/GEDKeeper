using System.Drawing;
using System.Windows.Forms;

using ExtUtils.ArborEngine;
using GKCommon.GEDCOM;
using GKCommon.Graph;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

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
            this.fTip = new ToolTip();
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
					//GEDCOMIndividualRecord iRec = this.fBase.Tree.XRefIndex_Find(xref) as GEDCOMIndividualRecord;
					//string txt = iRec.GetNameString(true, false) + " [" + xref + "]";

					GEDCOMFamilyRecord famRec = this.fBase.Tree.XRefIndex_Find(xref) as GEDCOMFamilyRecord;
					string txt = GKUtils.GetFamilyString(famRec) + " [" + xref + "]";

					fTip.Show(txt, arborViewer1, new Point(e.X + 24, e.Y));
					fTipShow = true;
				}
			}
		}

		private void PL_ConvertGraphToArborSystem(IGraph graph, ArborSystem sys)
		{
			foreach (IVertex vtx in graph.Vertices) {
				ArborNode arbNode = sys.addNode(vtx.Sign);
				PGNode pgNode = (PGNode)vtx.Value;

				arbNode.Color = (pgNode.Type == PGNodeType.ntIntersection) ? Color.BlueViolet : Color.Navy;
			}

			foreach (IEdge edge in graph.Edges) {
				sys.addEdge(edge.Source.Sign, edge.Target.Sign, 1);
			}
		}

	}
}
