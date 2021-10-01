using System;
using System.Drawing;
using System.Windows.Forms;
using GDModel;
using GKCore.Interfaces;
using GWTree;

namespace GWTreePlugin
{
    public class GWTreeView : UserControl, ITreeView
    {
        public static readonly Font DefFont = new Font("Verdana", 9.0f, FontStyle.Bold, GraphicsUnit.Pixel);
        public static readonly Pen DefPen = new Pen(Color.Black);
        public static readonly Brush DefBrush = new SolidBrush(Color.Black);

        private const float ZOOM_LOW_LIMIT = 0.0125f;
        private const float ZOOM_HIGH_LIMIT = 1000.0f;

        private readonly TreeModel fModel;
        private int fOffsetX;
        private int fOffsetY;
        private float fZoom = 1.0f;


        public IBaseContext Context
        {
            get {
                return fModel.Context;
            }
            set {
                fModel.Context = value;
            }
        }

        public TreeModel Model
        {
            get {
                return fModel;
            }
        }


        public GWTreeView()
        {
            fModel = new TreeModel(this);
        }

        public void Clear()
        {
            fModel.Clear();
        }

        public void CenterView()
        {
            var imageRect = fModel.GetImageRect();
            var imWidth = imageRect.GetWidth();
            var imHeight = imageRect.GetHeight();
            if (float.IsInfinity(imWidth) || float.IsInfinity(imHeight)) return;

            fOffsetX = (int)((-imWidth * fZoom + Width) / 2);
            fOffsetY = (int)((-imHeight * fZoom + Height) / 2);

            Invalidate();
        }

        public void CenterNode(Node node)
        {
            if (node == null) return;

            fOffsetX = (int)(-(node.x + node.width / 2) * fZoom + Width / 2);
            fOffsetY = (int)(-(node.y + node.height / 2) * fZoom + Height / 2);

            Invalidate();
        }

        void ITreeView.DrawNode(Graphics gfx, Node node)
        {
            Brush nodeBrush;
            switch (node.Sex) {
                case GDMSex.svMale:
                    nodeBrush = new SolidBrush(Color.LightSkyBlue);
                    break;
                case GDMSex.svFemale:
                    nodeBrush = new SolidBrush(Color.LightPink);
                    break;
                default:
                    nodeBrush = new SolidBrush(Color.LightGray);
                    break;
            }

            gfx.FillRectangle(nodeBrush, fOffsetX + node.x, fOffsetY + node.y, node.width, node.height);
            gfx.DrawRectangle(DefPen, fOffsetX + node.x, fOffsetY + node.y, node.width, node.height);
            gfx.DrawString(node.Id.ToString(), DefFont, DefBrush, fOffsetX + node.x, fOffsetY + node.y);
        }

        void ITreeView.DrawLine(Graphics gfx, float x1, float y1, float x2, float y2)
        {
            gfx.DrawLine(GWTreeView.DefPen, fOffsetX + x1, fOffsetY + y1, fOffsetX + x2, fOffsetY + y2);
        }

        protected override void OnClick(EventArgs e)
        {
            CenterNode(fModel.SelectedNode);

            base.OnClick(e);
        }

        protected override void OnDoubleClick(EventArgs e)
        {
            base.OnDoubleClick(e);
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            fModel.DrawLinks(e.Graphics);
            fModel.DrawNodes(e.Graphics);

            base.OnPaint(e);
        }

        protected override void OnResize(EventArgs e)
        {
            //CenterNode(fModel.SelectedNode);
            CenterView();

            base.OnResize(e);
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            base.OnKeyDown(e);
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            base.OnMouseDown(e);
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            base.OnMouseUp(e);
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            base.OnMouseMove(e);
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            if (Keys.None != (Keys.Control & ModifierKeys)) {
                if (e.Delta < 0) {
                    fZoom = Math.Max(fZoom * 0.95f, ZOOM_LOW_LIMIT);
                } else {
                    fZoom = Math.Min(fZoom * 1.05f, ZOOM_HIGH_LIMIT);
                }
            }
            fZoom += e.Delta;

            CenterNode(fModel.SelectedNode);

            base.OnMouseWheel(e);
        }
    }
}
