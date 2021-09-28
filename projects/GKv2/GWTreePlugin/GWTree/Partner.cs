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
            GetRightEdge().x = GetRightEdge().x + offset;
        }

        public override void DrawLinks(Graphics gfx)
        {
            if (IsEmpty) {
                return;
            }
            float offset = TreeModel.VLinkOffset * Number;
            fModel.DrawLine(gfx, 0,
                NodeA.x + NodeA.width / 2, NodeA.y + NodeA.height / 2 - offset,
                NodeB.x + NodeB.width / 2, NodeB.y + NodeB.height / 2 - offset);
        }
    }
}
