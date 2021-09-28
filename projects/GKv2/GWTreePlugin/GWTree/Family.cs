using System.Collections.Generic;
using System.Drawing;

namespace GWTree
{
    public sealed class Family : Group
    {
        public Pair Pair;
        public Children Children;

        public override bool IsEmpty
        {
            get {
                return Pair.IsEmpty && Children.IsEmpty;
            }
            set {
                Pair.IsEmpty = value;
                Children.IsEmpty = value;
            }
        }


        public Family(TreeModel model)
            : base(model)
        {
        }

        public override int AddNode(Node node)
        {
            return -1;
        }

        public override void RemoveNode(Node node)
        {
            Pair.RemoveNode(node);
            Children.RemoveNode(node);
        }

        public override Node GetLeftEdge()
        {
            return null;
        }

        public override Node GetRightEdge()
        {
            return null;
        }

        public override void DrawLinks(Graphics gfx)
        {
            if (Pair != null) {
                Pair.DrawLinks(gfx);
            }
            if (Children != null) {
                Children.DrawLinks(gfx);
            }
            if (Pair != null && Children != null) {
                Node dum1 = null, dum2 = null;
                PointF pairPvt = Pair.GetPivot(ref dum1, ref dum2);
                PointF childrenPvt = Children.GetPivot(ref dum1, ref dum2);
                fModel.DrawLine(gfx, 0, pairPvt.X, pairPvt.Y, childrenPvt.X, childrenPvt.Y);
            }
        }

        public override List<Node> GetNodes(List<Node> list, Node exclude = null)
        {
            if (list == null) {
                list = new List<Node>();
            }
            if (Pair != null) {
                Pair.GetNodes(list, exclude);
            }
            if (Children != null) {
                Children.GetNodes(list, exclude);
            }
            return list;
        }
    }
}
