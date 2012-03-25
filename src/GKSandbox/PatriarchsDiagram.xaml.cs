using System;
using System.Windows;

using Ext.Utils;
using GKCore;
using QuickGraph;

// Circular - не нужен
// Tree
// FR
// BoundedFR
// KK
// ISOM
// LinLog
// CompoundFDP
// EfficientSugiyama - туфта

namespace GKUI
{
    /// <summary>
    /// Interaction logic for PatriarchsDiagram.xaml
    /// </summary>
    public partial class PatriarchsDiagram : Window
    {
        private IBidirectionalGraph<object, IEdge<object>> _graphToVisualize;
        private TfmBase FBase;

        public IBidirectionalGraph<object, IEdge<object>> GraphToVisualize
        {
            get { return _graphToVisualize; }
        }

        public PatriarchsDiagram(TfmBase aBase)
        {
        	this.FBase = aBase;
            CreateGraphToVisualize();

            InitializeComponent();
        }

        private void CreateGraphToVisualize()
        {
            var g = new BidirectionalGraph<object, IEdge<object>>();

			TList lst = new TList(true);
			try
			{
				TreeTools.GetPatriarchsList(this.FBase.Engine.Tree, true, true, ref lst, 4, false);

	            string[] vertices = new string[lst.Count];

				int num = lst.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					TreeTools.TPatriarchObj p_obj = lst[i] as TreeTools.TPatriarchObj;
					string p_name = p_obj.IRec.aux_GetNameStr(true, false);
					vertices[i] = p_name; //p_obj.IRec.XRef;
					g.AddVertex(vertices[i]);
				}

				for (int i = 0; i <= num; i++)
				{
					TreeTools.TPatriarchObj p_obj = lst[i] as TreeTools.TPatriarchObj;

					for (int k = 0; k < p_obj.ILinks.Count; k++)
					{
						int dest = p_obj.ILinks[k];
						g.AddEdge(new Edge<object>(vertices[i], vertices[dest]));
					}
				}
			}
			finally
			{
				lst.Dispose();
			}

            /*string[] vertices = new string[5];
            for (int i = 0; i < 5; i++)
            {
                vertices[i] = i.ToString();
                g.AddVertex(vertices[i]);
            }

            g.AddEdge(new Edge<object>(vertices[0], vertices[1]));
            g.AddEdge(new Edge<object>(vertices[1], vertices[2]));
            g.AddEdge(new Edge<object>(vertices[2], vertices[3]));
            g.AddEdge(new Edge<object>(vertices[3], vertices[1])); 
            g.AddEdge(new Edge<object>(vertices[1], vertices[4]));*/

            _graphToVisualize = g;
        }
    }
}
