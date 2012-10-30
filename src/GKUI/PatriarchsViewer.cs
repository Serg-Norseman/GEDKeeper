using System;
using System.Drawing;
using System.Windows.Forms;

using Ext.ArborEngine;
using Ext.Utils;
using GedCom551;
using GKCore;

namespace GKUI
{
	public partial class PatriarchsViewer : Form
	{
        private TfmBase FBase;
        private ToolTip tip = new System.Windows.Forms.ToolTip();
        private bool tipShow = false;

        public PatriarchsViewer(TfmBase aBase, int minGens)
		{
			InitializeComponent();
			this.FBase = aBase;
			CreateGraph(minGens);
		}

        private void CreateGraph(int minGens)
        {
			using (TList lst = new TList(true)) {
				TreeTools.GetPatriarchsList(this.FBase.Engine.Tree, true, true, lst, minGens, false);

				int num = lst.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TPatriarchObj p_obj = lst[i] as TPatriarchObj;

					Node node = arborViewer1.Sys.addNode(p_obj.IRec.XRef);
					node.data = p_obj.IRec;
					node.color = Color.Gray;
				}

				for (int i = 0; i <= num; i++)
				{
					TPatriarchObj pat1 = lst[i] as TPatriarchObj;

					for (int k = 0; k < pat1.ILinks.Count; k++)
					{
						int dest = pat1.ILinks[k];
						TPatriarchObj pat2 = lst[dest] as TPatriarchObj;

						Edge edge = arborViewer1.Sys.addEdge(pat1.IRec.XRef, pat2.IRec.XRef, 1);
						edge.source.color = Color.Navy;
						edge.target.color = Color.Navy;
					}
				}

				arborViewer1.start();
			}
        }

		void ArborViewer1MouseMove(object sender, MouseEventArgs e)
		{
			Graphics gfx = arborViewer1.CreateGraphics();

			ArborSystem sys = arborViewer1.Sys;

			int idx = -1;
			for (int i = 0; i < sys.c_nodes.Count; i++) {
				Node node = sys.c_nodes[i];
				RectangleF n_rect = arborViewer1.getNodeRect(gfx, node);
				if (n_rect.Contains(e.X, e.Y)) {
					idx = i;
					break;
				}
			}

			if (idx < 0) {
				if (tipShow) {
					tip.Hide(arborViewer1);
					tipShow = false;
				}
			} else {
				if (!tipShow) {
					TGEDCOMIndividualRecord iRec = (sys.c_nodes[idx].data as TGEDCOMIndividualRecord);
					string txt = iRec.aux_GetNameStr(true, false) + " [" + iRec.XRef + "]";

					tip.Show(txt, arborViewer1, new Point(e.X + 24, e.Y));
					tipShow = true;
				}
			}
		}
	}
}
