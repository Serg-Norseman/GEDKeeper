using System.Collections;
using System.Collections.Generic;
using System.Drawing;

namespace GWTree
{
    public abstract class Group
    {
        protected TreeModel fModel;
        protected List<Node> fNodes;


        public virtual bool IsEmpty
        {
            get {
                return fNodes.Count == 0;
            }
        }


        protected Group(TreeModel model)
        {
            fModel = model;
            fNodes = new List<Node>();
        }

        public virtual void Clear()
        {
            fNodes.Clear();
        }

        public virtual int AddNode(Node node)
        {
            int i = 0;
            while (i < fNodes.Count) {
                if (node == fNodes[i] || node.Id != -1 && node.Id == fNodes[i].Id) {
                    return i;
                }
                i++;
            }
            return ((IList)fNodes).Add(node);
        }

        public virtual void RemoveNode(Node node)
        {
            int idx = fNodes.IndexOf(node);
            if (idx != -1) {
                fNodes.RemoveAt(idx);
            }
        }

        public virtual Node GetLeftEdge()
        {
            return null;
        }

        public virtual Node GetRightEdge()
        {
            return null;
        }

        public virtual void ShiftPivot(int offset)
        {
        }

        public PointF GetPivot()
        {
            Node dummy1 = null, dummy2 = null;
            return GetPivot(ref dummy1, ref dummy2);
        }

        public virtual PointF GetPivot(ref Node left, ref Node right)
        {
            return PointF.Empty;
        }

        public virtual void DrawLinks(Graphics gfx)
        {
        }

        public virtual List<Node> GetNodes(List<Node> list, Node exclude = null)
        {
            if (list == null) {
                list = new List<Node>();
            }
            foreach (Node item in fNodes) {
                if (item != exclude) {
                    list.Add(item);
                }
            }
            return list;
        }
    }
}
