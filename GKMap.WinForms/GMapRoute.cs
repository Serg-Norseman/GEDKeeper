/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;

namespace GKMap.WinForms
{
    /// <summary>
    /// GKMap route
    /// </summary>
    public class GMapRoute : MapRoute, IDisposable
    {
        private bool fDisposed;
        private GraphicsPath fGraphicsPath;
        private bool fIsMouseOver;
        private GMapOverlay fOverlay;
        private bool fVisible = true;

        public GMapOverlay Overlay
        {
            get {
                return fOverlay;
            }
            internal set {
                fOverlay = value;
            }
        }

        /// <summary>
        /// is marker visible
        /// </summary>
        public bool IsVisible
        {
            get {
                return fVisible;
            }
            set {
                if (value != fVisible) {
                    fVisible = value;

                    if (Overlay != null && Overlay.Control != null) {
                        if (fVisible) {
                            Overlay.Control.UpdateRouteLocalPosition(this);
                        } else {
                            if (Overlay.Control.IsMouseOverRoute) {
                                Overlay.Control.IsMouseOverRoute = false;
                                Overlay.Control.RestoreCursorOnLeave();
                            }
                        }

                        if (!Overlay.Control.HoldInvalidation) {
                            Overlay.Control.Invalidate();
                        }
                    }
                }
            }
        }

        /// <summary>
        /// can receive input
        /// </summary>
        public bool IsHitTestVisible = false;

        /// <summary>
        /// is mouse over
        /// </summary>
        public bool IsMouseOver
        {
            get {
                return fIsMouseOver;
            }
            internal set {
                fIsMouseOver = value;
            }
        }

        public static readonly Pen DefaultStroke = new Pen(Color.FromArgb(144, Color.MidnightBlue));

        /// <summary>
        /// specifies how the outline is painted
        /// </summary>
        public Pen Stroke = DefaultStroke;

        public readonly List<GPoint> LocalPoints = new List<GPoint>();

        static GMapRoute()
        {
            DefaultStroke.LineJoin = LineJoin.Round;
            DefaultStroke.Width = 5;
        }

        public GMapRoute(string name)
            : base(name)
        {
        }

        public GMapRoute(IEnumerable<PointLatLng> points, string name)
            : base(points, name)
        {
        }

        /// <summary>
        /// Indicates whether the specified point is contained within this System.Drawing.Drawing2D.GraphicsPath
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        internal bool IsInside(int x, int y)
        {
            if (fGraphicsPath != null) {
                return fGraphicsPath.IsOutlineVisible(x, y, Stroke);
            }
            return false;
        }

        internal void UpdateGraphicsPath()
        {
            if (fGraphicsPath == null) {
                fGraphicsPath = new GraphicsPath();
            } else {
                fGraphicsPath.Reset();
            }

            for (int i = 0; i < LocalPoints.Count; i++) {
                GPoint p2 = LocalPoints[i];

                if (i == 0) {
                    fGraphicsPath.AddLine(p2.X, p2.Y, p2.X, p2.Y);
                } else {
                    PointF p = fGraphicsPath.GetLastPoint();
                    fGraphicsPath.AddLine(p.X, p.Y, p2.X, p2.Y);
                }
            }
        }

        public virtual void OnRender(Graphics g)
        {
            if (IsVisible && fGraphicsPath != null) {
                g.DrawPath(Stroke, fGraphicsPath);
            }
        }

        public virtual void Dispose()
        {
            if (!fDisposed) {
                fDisposed = true;

                LocalPoints.Clear();

                if (fGraphicsPath != null) {
                    fGraphicsPath.Dispose();
                    fGraphicsPath = null;
                }
                Clear();
            }
        }
    }

    public delegate void RouteClick(GMapRoute item, MouseEventArgs e);
    public delegate void RouteEnter(GMapRoute item);
    public delegate void RouteLeave(GMapRoute item);
    public delegate void RouteDoubleClick(GMapRoute item, MouseEventArgs e);
}
