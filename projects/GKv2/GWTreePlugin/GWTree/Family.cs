using System.Collections.Generic;
using System.Drawing;

namespace GWTree
{
    public sealed class Family : Group
    {
        public Pair Pair { get; private set; }
        public Children Children { get; private set; }

        public override bool IsEmpty
        {
            get {
                return Pair.IsEmpty && Children.IsEmpty;
            }
        }


        public Family(TreeModel model, Pair pair)
            : base(model)
        {
            Pair = pair;
            Children = new Children(model);
        }

        public override void Clear()
        {
            base.Clear();

            Pair.Clear();
            Children.Clear();
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
            if (!Pair.IsEmpty) {
                Pair.DrawLinks(gfx);
            }
            if (!Children.IsEmpty) {
                Children.DrawLinks(gfx);
            }
            if (!Pair.IsEmpty && !Children.IsEmpty) {
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
            if (!Pair.IsEmpty) {
                Pair.GetNodes(list, exclude);
            }
            if (!Children.IsEmpty) {
                Children.GetNodes(list, exclude);
            }
            return list;
        }
    }
}
