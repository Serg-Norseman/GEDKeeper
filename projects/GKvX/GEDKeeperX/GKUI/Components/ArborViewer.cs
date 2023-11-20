/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using BSLib.DataViz.SmartGraph;
using GKCore;
using SkiaSharp;
using SkiaSharp.Views.Forms;
using Xamarin.Forms;

namespace GKUI.Components
{
    public sealed class ArborNodeEx : ArborNode
    {
        public Color Color;
        public Rectangle Box;

        public ArborNodeEx(string sign) : base(sign)
        {
            Color = Color.Gray;
        }
    }

    public sealed class ArborSystemEx : ArborSystem
    {
        private bool fTimerAlive;

        public ArborSystemEx(double repulsion, double stiffness, double friction, IArborRenderer renderer)
            : base(repulsion, stiffness, friction, renderer)
        {
            this.fTimerAlive = false;
            this.Graph = new Graph();
        }

        private bool TimerElapsed()
        {
            TickTimer();
            return fTimerAlive;
        }

        protected override void StartTimer()
        {
            fTimerAlive = true;
            Device.StartTimer(TimeSpan.FromSeconds(TimerInterval / 1000), TimerElapsed);
        }

        protected override void StopTimer()
        {
            if (fTimerAlive) {
                fTimerAlive = false;
            }
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

    public sealed class ArborViewer : ContentView, IArborRenderer
    {
        private readonly SKCanvasView fCanvas;
        private bool fEnergyDebug;
        private ArborNode fDragged;
        private bool fNodesDragging;
        private readonly ArborSystemEx fSys;

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
            fCanvas = new SKCanvasView();
            fCanvas.PaintSurface += OnPaint;
            fCanvas.EnableTouchEvents = true;
            fCanvas.Touch += OnTouch;
            fCanvas.SizeChanged += OnSizeChanged;
            Content = fCanvas;

            BackgroundColor = Color.White;

            // repulsion - отталкивание, stiffness - тугоподвижность, friction - сила трения
            fSys = new ArborSystemEx(10000, 500/*1000*/, 0.1, this);
            fSys.SetViewSize((int)Width, (int)Height);
            fSys.AutoStop = false;

            fEnergyDebug = false;
            fDragged = null;
            fNodesDragging = false;
        }

        private void OnSizeChanged(object sender, EventArgs e)
        {
            fSys.SetViewSize((int)fCanvas.Width, (int)fCanvas.Height);
            Invalidate();
        }

        private void OnPaint(object sender, SKPaintSurfaceEventArgs args)
        {
            var info = args.Info;
            var surface = args.Surface;
            var canvas = surface.Canvas;

            canvas.Clear();

            try {
                SKPaint textPaint = new SKPaint {
                    Color = SKColors.White,
                    TextSize = AppHost.GfxProvider.GetDefaultFontSize()
                };

                foreach (ArborNode node in fSys.Nodes) {
                    var xnode = node as ArborNodeEx;

                    xnode.Box = getNodeRect(textPaint, node);
                    canvas.DrawRect((float)xnode.Box.Left, (float)xnode.Box.Top, (float)xnode.Box.Width, (float)xnode.Box.Height, new SKPaint {
                        Style = SKPaintStyle.Fill,
                        Color = xnode.Color.ToSKColor()
                    });
                    canvas.DrawText(node.Sign, (float)xnode.Box.Left, (float)(xnode.Box.Top + xnode.Box.Height), textPaint);
                }

                using (var grayPen = new SKPaint()) {
                    grayPen.Style = SKPaintStyle.Stroke;
                    grayPen.Color = Color.Gray.ToSKColor();

                    foreach (ArborEdge edge in fSys.Edges) {
                        var srcNode = edge.Source as ArborNodeEx;
                        var tgtNode = edge.Target as ArborNodeEx;

                        ArborPoint pt1 = fSys.GetViewCoords(srcNode.Pt);
                        ArborPoint pt2 = fSys.GetViewCoords(tgtNode.Pt);

                        ArborPoint tail = intersect_line_box(pt1, pt2, srcNode.Box);
                        ArborPoint head = (tail.IsNull()) ? ArborPoint.Null : intersect_line_box(tail, pt2, tgtNode.Box);

                        if (!head.IsNull() && !tail.IsNull()) {
                            canvas.DrawLine((float)tail.X, (float)tail.Y, (float)head.X, (float)head.Y, grayPen);
                        }
                    }
                }

                if (fEnergyDebug) {
                    textPaint = new SKPaint {
                        Color = SKColors.Black,
                        TextSize = 16.0f
                    };

                    string energy = "max=" + fSys.EnergyMax.ToString("0.00000") + ", mean=" + fSys.EnergyMean.ToString("0.00000");
                    canvas.DrawText(energy, 10.0f, 10.0f, textPaint);
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

        public ArborPoint intersect_line_box(ArborPoint p1, ArborPoint p2, Rectangle boxTuple)
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

        public void Start()
        {
            fSys.Start();
        }

        private void OnTouch(object sender, SKTouchEventArgs e)
        {
            Point mpt = new Point(e.Location.X, e.Location.Y);

            switch (e.ActionType)
            {
                case SKTouchAction.Pressed: {
                        if (!IsFocused) base.Focus();
                        if (fNodesDragging) {
                            fDragged = fSys.GetNearestNode((int)mpt.X, (int)mpt.Y);

                            if (fDragged != null) {
                                fDragged.Fixed = true;
                            }
                        }
                    }
                    break;

                case SKTouchAction.Released: {
                        if (fNodesDragging && fDragged != null) {
                            fDragged.Fixed = false;
                            //fDragged.Mass = 1000;
                            fDragged = null;
                        }
                    }
                    break;

                case SKTouchAction.Moved: {
                        if (fNodesDragging && fDragged != null) {
                            fDragged.Pt = fSys.GetModelCoords(mpt.X, mpt.Y);
                        }
                    }
                    break;
            }
        }

        public Rectangle getNodeRect(SKPaint textPaint, ArborNode node)
        {
            SKRect textBounds = new SKRect();
            textPaint.MeasureText(node.Sign, ref textBounds);
            float w = textBounds.Width + 10;
            float h = textBounds.Height + 4;
            ArborPoint pt = fSys.GetViewCoords(node.Pt);
            pt.X = Math.Floor(pt.X);
            pt.Y = Math.Floor(pt.Y);

            return new Rectangle((float)pt.X - w / 2, (float)pt.Y - h / 2, w, h);
        }

        public ArborNode GetNodeByCoord(int x, int y)
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

        public void Invalidate()
        {
            fCanvas.InvalidateSurface();
        }
    }
}
