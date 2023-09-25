/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using BSLib.DataViz.ArborGVT;
using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.Interfaces;
using GKUI.Platform;

namespace GKUI.Components
{
    public sealed class ArborNodeEx : ArborNode
    {
        public Color Color;
        public RectangleF Box;

        public ArborNodeEx(string sign) : base(sign)
        {
            Color = Colors.Gray;
        }
    }

    public sealed class ArborSystemEx : ArborSystem
    {
        private ITimer fTimer;

        public ArborSystemEx(double repulsion, double stiffness, double friction, IArborRenderer renderer)
            : base(repulsion, stiffness, friction, renderer)
        {
            this.fTimer = null;
        }

        private void TimerElapsed(object sender, EventArgs e)
        {
            TickTimer();
        }

        protected override void StartTimer()
        {
            fTimer = AppHost.Instance.CreateTimer(TimerInterval, TimerElapsed);
            fTimer.Start();
        }

        protected override void StopTimer()
        {
            if (fTimer != null) {
                fTimer.Stop();
                fTimer.Dispose();
                fTimer = null;
            }
        }

        protected override ArborNode CreateNode(string sign)
        {
            return new ArborNodeEx(sign);
        }

        protected override ArborEdge CreateEdge(ArborNode source, ArborNode target, double length, double stiffness, bool directed = false)
        {
            return new ArborEdge(source, target, length, stiffness, directed);
        }
    }

    public sealed class ArborViewer : ScrollablePanel, IArborRenderer
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
            base.BackgroundColor = Colors.White;

            // repulsion - отталкивание, stiffness - тугоподвижность, friction - сила трения
            fSys = new ArborSystemEx(10000, 500/*1000*/, 0.1, this);
            fSys.SetViewSize(Width, Height);
            fSys.AutoStop = false;

            fEnergyDebug = false;

#if !OS_LINUX
            fDrawFont = new Font("Calibri", 9);
#else
            fDrawFont = new Font(FontFamilies.SansFamilyName, 9);
#endif

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
            if (disposing) {
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

            fSys.SetViewSize(Width, Height);
            Invalidate();
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            Graphics gfx = e.Graphics;

            try {
                gfx.AntiAlias = true;

                foreach (ArborNode node in fSys.Nodes) {
                    var xnode = node as ArborNodeEx;

                    xnode.Box = getNodeRect(gfx, node);
                    gfx.FillRectangle(new SolidBrush(xnode.Color), xnode.Box);
                    gfx.DrawText(fDrawFont, fWhiteBrush, xnode.Box.Left, xnode.Box.Top, node.Sign);
                }

                using (Pen grayPen = new Pen(Colors.Gray, 1)) {
                    foreach (ArborEdge edge in fSys.Edges) {
                        var srcNode = edge.Source as ArborNodeEx;
                        var tgtNode = edge.Target as ArborNodeEx;

                        ArborPoint pt1 = fSys.GetViewCoords(srcNode.Pt);
                        ArborPoint pt2 = fSys.GetViewCoords(tgtNode.Pt);

                        ArborPoint tail = intersect_line_box(pt1, pt2, srcNode.Box);
                        ArborPoint head = (tail.IsNull()) ? ArborPoint.Null : intersect_line_box(tail, pt2, tgtNode.Box);

                        if (!head.IsNull() && !tail.IsNull()) {
                            EtoGfxRenderer.DrawArrowLine(gfx, Colors.Gray, grayPen, (float)tail.X, (float)tail.Y, (float)head.X, (float)head.Y);
                        }
                    }
                }

                if (fEnergyDebug) {
                    string energy = "max=" + fSys.EnergyMax.ToString("0.00000") + ", mean=" + fSys.EnergyMean.ToString("0.00000");
                    gfx.DrawText(fDrawFont, fBlackBrush, 10, 10, energy);
                }
            } catch (Exception ex) {
                Logger.WriteError("ArborViewer.OnPaint()", ex);
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
            if (!pt.IsNull()) return pt;

            pt = intersect_line_line(p1, p2, tr, br);
            if (!pt.IsNull()) return pt;

            pt = intersect_line_line(p1, p2, br, bl);
            if (!pt.IsNull()) return pt;

            pt = intersect_line_line(p1, p2, bl, tl);
            if (!pt.IsNull()) return pt;

            return ArborPoint.Null;
        }

        public void start()
        {
            fSys.Start();
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            if (!HasFocus) base.Focus();

            Point mpt = new Point(e.Location);
            if (fNodesDragging) {
                fDragged = fSys.GetNearestNode(mpt.X, mpt.Y);

                if (fDragged != null) {
                    fDragged.Fixed = true;
                }
            }

            e.Handled = true;
            base.OnMouseDown(e);
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            if (fNodesDragging && fDragged != null) {
                fDragged.Fixed = false;
                //fDragged.Mass = 1000;
                fDragged = null;
            }

            e.Handled = true;
            base.OnMouseUp(e);
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            Point mpt = new Point(e.Location);
            if (fNodesDragging && fDragged != null) {
                fDragged.Pt = fSys.GetModelCoords(mpt.X, mpt.Y);
            }

            e.Handled = true;
            base.OnMouseMove(e);
        }

        public RectangleF getNodeRect(Graphics gfx, ArborNode node)
        {
            SizeF tsz = gfx.MeasureString(fDrawFont, node.Sign);
            float w = tsz.Width + 10;
            float h = tsz.Height + 4;
            ArborPoint pt = fSys.GetViewCoords(node.Pt);
            pt.X = Math.Floor(pt.X);
            pt.Y = Math.Floor(pt.Y);

            return new RectangleF((float)pt.X - w / 2, (float)pt.Y - h / 2, w, h);
        }

        public ArborNode getNodeByCoord(int x, int y)
        {
            return fSys.GetNearestNode(x, y);

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
