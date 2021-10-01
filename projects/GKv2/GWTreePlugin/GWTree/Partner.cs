using System.Drawing;

namespace GWTree
{
    public sealed class Partner : Pair
    {
        public int Number;

        public Node Person
        {
            get {
                return NodeA;
            }
        }

        public Node Spouse
        {
            get {
                return NodeB;
            }
        }


        public Partner(TreeModel model)
            : base(model)
        {
        }

        public override PointF GetPivot(ref Node left, ref Node right)
        {
            left = GetLeftEdge();
            right = GetRightEdge();

            PointF pivot = new PointF();
            if (Spouse == right) {
                pivot.X = right.x - TreeModel.PivotOffset;
            }
            if (Spouse == left) {
                pivot.X = left.x + TreeModel.PivotOffset;
            }
            pivot.Y = (left.y + right.y + right.height) / 2 - TreeModel.VLinkOffset * Number;

            return pivot;
        }

        public override void ShiftPivot(int offset)
        {
            var right = GetRightEdge();
            right.x = right.x + offset;
        }

        public override void DrawLinks(Graphics gfx)
        {
            if (IsEmpty) {
                return;
            }
            var nodeA = NodeA;
            var nodeB = NodeB;
            if (nodeA == null || nodeB == null) return;

            float offset = TreeModel.VLinkOffset * Number;
            fModel.DrawLine(gfx, 0,
                            nodeA.x + nodeA.width, nodeA.y + nodeA.height / 2 - offset,
                            nodeB.x, nodeB.y + nodeB.height / 2 - offset);
        }
    }
}
