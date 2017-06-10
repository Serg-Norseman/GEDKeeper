/*
 *  ArborGVT - a graph vizualization toolkit
 *
 *  Physics code derived from springy.js, copyright (c) 2010 Dennis Hotson
 *  JavaScript library, copyright (c) 2011 Samizdat Drafting Co.
 *
 *  Fork and C# implementation, copyright (c) 2012,2016 by Serg V. Zhdanovskih.
 */

using System;
using System.Diagnostics;
using ArborGVT;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Components
{
    public sealed class ArborNodeEx : ArborNode
    {
        public Color Color;
        public RectangleF Box;

        public ArborNodeEx(string sign) : base(sign)
        {
            this.Color = Colors.Gray;
        }
    }

    public sealed class ArborSystemEx : ArborSystem
    {
        public ArborSystemEx(double repulsion, double stiffness, double friction, IArborRenderer renderer)
            : base(repulsion, stiffness, friction, renderer)
        {
        }

        protected override ArborNode CreateNode(string sign)
        {
            return new ArborNodeEx(sign);
        }

        protected override ArborEdge CreateEdge(ArborNode src, ArborNode tgt, double len, double stiffness, bool directed = false)
        {
            return new ArborEdge(src, tgt, len, stiffness, directed);
        }
    }

    public sealed class ArborViewer : CustomPanel, IArborRenderer
    {
        private bool fEnergyDebug;
        private ArborNode fDragged;
        private readonly Font fDrawFont;
        private bool fNodesDragging;
        //private readonly StringFormat fStrFormat;
        private readonly ArborSystemEx fSys;
        private readonly SolidBrush fBlackBrush;
        private readonly SolidBrush fWhiteBrush;

        public bool EnergyDebug
        {
            get { return fEnergyDebug; }
            set { fEnergyDebug = value; }
        }

        public bool NodesDragging
        {
            get { return fNodesDragging; }
            set { fNodesDragging = value; }
        }

        public ArborSystemEx Sys
        {
            get { return fSys; }
        }

        public ArborViewer()
        {
            //base.BorderStyle = BorderStyle.Fixed3D;
            //base.TabStop = true;
            base.BackgroundColor = Colors.White;

            //base.DoubleBuffered = true;
            //base.SetStyle(ControlStyles.AllPaintingInWmPaint, true);
            //base.SetStyle(ControlStyles.OptimizedDoubleBuffer, true);

            // repulsion - отталкивание, stiffness - тугоподвижность, friction - сила трения
            fSys = new ArborSystemEx(10000, 500/*1000*/, 0.1, this);
            fSys.setScreenSize(Width, Height);
            fSys.AutoStop = false;

            fEnergyDebug = false;
            fDrawFont = new Font("Calibri", 9);

            //fStrFormat = new StringFormat();
            //fStrFormat.Alignment = StringAlignment.Center;
            //fStrFormat.LineAlignment = StringAlignment.Center;

            fBlackBrush = new SolidBrush(Colors.Black);
            fWhiteBrush = new SolidBrush(Colors.White);
            fDragged = null;
            fNodesDragging = false;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                fSys.Dispose();
                fDrawFont.Dispose();
                fWhiteBrush.Dispose();
                fBlackBrush.Dispose();
                //fStrFormat.Dispose();
            }
            base.Dispose(disposing);
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            base.OnSizeChanged(e);

            fSys.setScreenSize(Width, Height);
            Invalidate();
        }

        protected override void OnPaint(PaintEventArgs pe)
        {
            Graphics gfx = pe.Graphics;

            try
            {
                gfx.AntiAlias = true;

                foreach (ArborNode node in fSys.Nodes)
                {
                    var xnode = node as ArborNodeEx;

                    xnode.Box = getNodeRect(gfx, node);
                    gfx.FillRectangle(new SolidBrush(xnode.Color), xnode.Box);
                    gfx.DrawText(fDrawFont, fWhiteBrush, xnode.Box.Left, xnode.Box.Top, node.Sign);
                }

                using (Pen grayPen = new Pen(Colors.Gray, 1))
                {
                    //grayPen.StartCap = PenLineCap.NoAnchor;
                    //grayPen.EndCap = PenLineCap.ArrowAnchor;

                    foreach (ArborEdge edge in fSys.Edges)
                    {
                        var srcNode = edge.Source as ArborNodeEx;
                        var tgtNode = edge.Target as ArborNodeEx;

                        ArborPoint pt1 = fSys.toScreen(srcNode.Pt);
                        ArborPoint pt2 = fSys.toScreen(tgtNode.Pt);

                        ArborPoint tail = intersect_line_box(pt1, pt2, srcNode.Box);
                        ArborPoint head = (tail.isNull()) ? ArborPoint.Null : intersect_line_box(tail, pt2, tgtNode.Box);

                        if (!head.isNull() && !tail.isNull())
                        {
                            gfx.DrawLine(grayPen, (int)tail.X, (int)tail.Y, (int)head.X, (int)head.Y);
                        }
                    }
                }

                if (fEnergyDebug)
                {
                    string energy = "max=" + fSys.EnergyMax.ToString("0.00000") + ", mean=" + fSys.EnergyMean.ToString("0.00000");
                    gfx.DrawText(fDrawFont, fBlackBrush, 10, 10, energy);
                }
            }
            catch (Exception ex)
            {
                Debug.WriteLine("ArborViewer.OnPaint(): " + ex.Message);
            }
        }

        public static ArborPoint intersect_line_line(ArborPoint p1, ArborPoint p2, ArborPoint p3, ArborPoint p4)
        {
            double denom = ((p4.Y - p3.Y) * (p2.X - p1.X) - (p4.X - p3.X) * (p2.Y - p1.Y));
            if (denom == 0) return ArborPoint.Null; // lines are parallel

            double ua = ((p4.X - p3.X) * (p1.Y - p3.Y) - (p4.Y - p3.Y) * (p1.X - p3.X)) / denom;
            double ub = ((p2.X - p1.X) * (p1.Y - p3.Y) - (p2.Y - p1.Y) * (p1.X - p3.X)) / denom;

            if (ua < 0 || ua > 1 || ub < 0 || ub > 1) return ArborPoint.Null;

            return new ArborPoint(p1.X + ua * (p2.X - p1.X), p1.Y + ua * (p2.Y - p1.Y));
        }

        public ArborPoint intersect_line_box(ArborPoint p1, ArborPoint p2, RectangleF boxTuple)
        {
            double bx = boxTuple.X;
            double by = boxTuple.Y;
            double bw = boxTuple.Width;
            double bh = boxTuple.Height;

            ArborPoint tl = new ArborPoint(bx, by);
            ArborPoint tr = new ArborPoint(bx + bw, by);
            ArborPoint bl = new ArborPoint(bx, by + bh);
            ArborPoint br = new ArborPoint(bx + bw, by + bh);

            ArborPoint pt;

            pt = intersect_line_line(p1, p2, tl, tr);
            if (!pt.isNull()) return pt;

            pt = intersect_line_line(p1, p2, tr, br);
            if (!pt.isNull()) return pt;

            pt = intersect_line_line(p1, p2, br, bl);
            if (!pt.isNull()) return pt;

            pt = intersect_line_line(p1, p2, bl, tl);
            if (!pt.isNull()) return pt;

            return ArborPoint.Null;
        }

        public void start()
        {
            fSys.start();
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            base.OnMouseDown(e);
            //if (!Focused) base.Focus();

            Point mpt = new Point(e.Location);
            if (fNodesDragging)
            {
                fDragged = fSys.nearest(mpt.X, mpt.Y);

                if (fDragged != null)
                {
                    fDragged.Fixed = true;
                }
            }
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            base.OnMouseUp(e);

            if (fNodesDragging && fDragged != null)
            {
                fDragged.Fixed = false;
                //fDragged.Mass = 1000;
                fDragged = null;
            }
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            base.OnMouseMove(e);

            Point mpt = new Point(e.Location);
            if (fNodesDragging && fDragged != null)
            {
                fDragged.Pt = fSys.fromScreen(mpt.X, mpt.Y);
            }
        }

        public RectangleF getNodeRect(Graphics gfx, ArborNode node)
        {
            SizeF tsz = gfx.MeasureString(fDrawFont, node.Sign);
            float w = tsz.Width + 10;
            float h = tsz.Height + 4;
            ArborPoint pt = fSys.toScreen(node.Pt);
            pt.X = Math.Floor(pt.X);
            pt.Y = Math.Floor(pt.Y);

            return new RectangleF((float)pt.X - w / 2, (float)pt.Y - h / 2, w, h);
        }

        public ArborNode getNodeByCoord(int x, int y)
        {
            return fSys.nearest(x, y);

            /*foreach (ArborNode node in fSys.Nodes)
            {
                if (node.Box.Contains(x, y)) {
                    return node;
                }
            }
            return null;*/
        }
    }
}
