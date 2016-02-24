using System.Drawing;
using System.Windows.Forms;

using ArborGVT;
using BSLib.SmartGraph;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKUI
{
	public partial class PatriarchsViewer : Form
	{
        private readonly IBaseWindow fBase;
        private readonly ToolTip fTip;
        private bool fTipShow;

        public PatriarchsViewer(IBaseWindow aBase, int minGens)
		{
			InitializeComponent();

			this.fBase = aBase;
            this.fTip = new ToolTip();
            this.fTipShow = false;

        	Graph graph = this.fBase.Context.GetPatriarchsGraph(minGens, false);
        	PL_ConvertGraphToArborSystem(graph, arborViewer1.Sys);

        	arborViewer1.NodesDragging = true;
        	arborViewer1.start();
		}

		private void ArborViewer1MouseMove(object sender, MouseEventArgs e)
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
					string txt = GKUtils.GetFamilyString(famRec) + " [" + xref + "] "/* + resNode.Mass.ToString()*/;

					fTip.Show(txt, arborViewer1, e.X + 24, e.Y);
					fTipShow = true;
				}
			}
		}

		private static void PL_ConvertGraphToArborSystem(IGraph graph, ArborSystem sys)
		{
			foreach (IVertex vtx in graph.Vertices) {
				ArborNode arbNode = sys.addNode(vtx.Sign);
				PGNode pgNode = (PGNode)vtx.Value;

				arbNode.Color = (pgNode.Type == PGNodeType.Intersection) ? Color.BlueViolet : Color.Navy;
				arbNode.Mass = pgNode.Size;
			}

			foreach (IEdge edge in graph.Edges) {
				sys.addEdge(edge.Source.Sign, edge.Target.Sign);
			}
		}

	}
}
