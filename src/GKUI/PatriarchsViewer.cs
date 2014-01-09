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
        private int fMinGens;
        private ToolTip tip = new System.Windows.Forms.ToolTip();
        private bool tipShow = false;

        public PatriarchsViewer(TfmBase aBase, int minGens)
		{
			InitializeComponent();

			this.FBase = aBase;
			this.fMinGens = minGens;
			this.CreateGraph(arborViewer1.Sys, minGens);
			arborViewer1.start();
		}

        private void CreateGraph(ArborSystem sys, int minGens)
        {
        	TreeTools.GPLParams gpl_params = new TreeTools.GPLParams();
        	gpl_params.aLinks = true;
        	gpl_params.aDates = false;

        	bool loneSuppress = true;

			using (TList lst = new TList(true)) {
				TreeTools.GetPatriarchsList(this.FBase.Tree, lst, null, minGens, null, gpl_params);

				int num = lst.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TPatriarchObj p_obj = lst[i] as TPatriarchObj;

					if ((!loneSuppress) || (loneSuppress && p_obj.HasLinks)) {
						Node node = sys.addNode(p_obj.IRec.XRef);
						node.data = p_obj.IRec;
						node.color = Color.Gray;
					}
				}

				for (int i = 0; i <= num; i++)
				{
					TPatriarchObj pat1 = lst[i] as TPatriarchObj;

					for (int k = 0; k < pat1.ILinks.Count; k++)
					{
						TPatriarchObj pat2 = pat1.ILinks[k];

						Edge edge = sys.addEdge(pat1.IRec.XRef, pat2.IRec.XRef, 1);
						edge.source.color = Color.Navy;
						edge.target.color = Color.Navy;
					}
				}
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
