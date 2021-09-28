using System.Drawing;

namespace GWTree
{
    public sealed class Children : Group
    {
        public Children(TreeModel model) : base(model)
        {
        }

        public Node GetRightSibling()
        {
            if (IsEmpty) {
                return null;
            }

            Node node = fNodes[0];
            int xOrder = node.Order;
            foreach (Node item in fNodes) {
                if (item.Order != -1) {
                    if (item.Order > xOrder) {
                        xOrder = item.Order;
                        node = item;
                    }
                }
            }
            return node;
        }

        public Node GetRightPartner()
        {
            if (IsEmpty) {
                return null;
            }

            Node right = GetRightSibling();
            Node rightRes = null;
            if (right != null && right.SelfFamily != null && right.SelfFamily.Pair != null) {
                rightRes = right.SelfFamily.Pair.GetRightEdge();
                if (right == rightRes) {
                    return null;
                }
            }
            return rightRes;
        }

        public override Node GetRightEdge()
        {
            if (IsEmpty) {
                return null;
            }

            Node right = GetRightPartner();
            if (right == null) {
                right = GetRightSibling();
            }
            return right;
        }

        public override Node GetLeftEdge()
        {
            if (IsEmpty) {
                return null;
            }

            Node left = GetLeftPartner();
            if (left == null) {
                left = GetLeftSibling();
            }
            return left;
        }

        public Node GetLeftSibling()
        {
            if (IsEmpty) {
                return null;
            }

            Node node = fNodes[0];
            int xOrder = node.Order;
            foreach (Node item in fNodes) {
                if (item.Order != -1) {
                    if (item.Order < xOrder) {
                        xOrder = item.Order;
                        node = item;
                    }
                }
            }
            return node;
        }

        public Node GetLeftPartner()
        {
            if (IsEmpty) {
                return null;
            }

            Node left = GetLeftSibling();
            Node leftRes = null;
            if (left != null && left.SelfFamily != null && left.SelfFamily.Pair != null) {
                leftRes = left.SelfFamily.Pair.GetLeftEdge();
                if (left == leftRes) {
                    return null;
                }
            }
            return leftRes;
        }

        public override void ShiftPivot(int offset)
        {
            if (IsEmpty) {
                return;
            }

            GetLeftSibling().x = GetLeftSibling().x + offset;
        }

        public override PointF GetPivot(ref Node left, ref Node right)
        {
            left = GetLeftSibling();
            right = GetRightSibling();

            PointF pivot = new PointF();
            pivot.X = (left.x + right.x + right.width) / 2;
            pivot.Y = (left.y + right.y) / 2 - TreeModel.VSpace / 2;
            return pivot;
        }

        public override void DrawLinks(Graphics gfx)
        {
            if (IsEmpty) {
                return;
            }

            Node left = null;
            Node right = null;
            PointF pivot = GetPivot(ref right, ref left);
            foreach (Node item in fNodes) {
                float ix = item.x + item.width / 2;
                fModel.DrawLine(gfx, 0, ix, item.y, ix, pivot.Y);
            }
            float bx = (right).x + (right).width / 2;
            float ex = (left).x + (left).width / 2;
            fModel.DrawLine(gfx, 0, bx, pivot.Y, ex, pivot.Y);
        }
    }
}
