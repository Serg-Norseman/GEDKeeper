/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;
using GKMap.MapProviders;

namespace GKMap.WinForms
{
    /// <summary>
    /// GKMap route
    /// </summary>
    public class GMapRoute : GMapFigure
    {
        public static readonly Pen DefaultStroke = new Pen(Color.FromArgb(144, Color.MidnightBlue));


        private GraphicsPath fGraphicsPath;


        /// <summary>
        /// specifies how the outline is painted
        /// </summary>
        public Pen Stroke { get; set; }


        static GMapRoute()
        {
            DefaultStroke.LineJoin = LineJoin.Round;
            DefaultStroke.Width = 5;
        }

        public GMapRoute(string name, IEnumerable<PointLatLng> points = null)
            : base(name, points)
        {
            fVisible = true;
            Stroke = DefaultStroke;
            IsHitTestVisible = false;
        }

        protected override void UpdateLocalPosition()
        {
            if (fVisible) {
                Overlay.Control.UpdateRouteLocalPosition(this);
            } else {
                if (Overlay.Control.IsMouseOverRoute) {
                    Overlay.Control.IsMouseOverRoute = false;
                    Overlay.Control.RestoreCursorOnLeave();
                }
            }
        }

        /// <summary>
        /// Indicates whether the specified point is contained within this System.Drawing.Drawing2D.GraphicsPath
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        internal bool IsInside(int x, int y)
        {
            return fGraphicsPath != null && fGraphicsPath.IsOutlineVisible(x, y, Stroke);
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

        public override void OnRender(Graphics g)
        {
            if (IsVisible && fGraphicsPath != null) {
                g.DrawPath(Stroke, fGraphicsPath);
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                LocalPoints.Clear();

                if (fGraphicsPath != null) {
                    fGraphicsPath.Dispose();
                    fGraphicsPath = null;
                }
                Clear();
            }
            base.Dispose(disposing);
        }

        /// <summary>
        /// route distance (in km)
        /// </summary>
        public double GetDistance()
        {
            double distance = 0.0;

            if (HasLines) {
                for (int i = 1; i < Points.Count; i++) {
                    distance += GMapProviders.EmptyProvider.Projection.GetDistance(Points[i - 1], Points[i]);
                }
            }

            return distance;
        }
    }

    public delegate void RouteClick(GMapRoute item, MouseEventArgs e);
    public delegate void RouteEnter(GMapRoute item);
    public delegate void RouteLeave(GMapRoute item);
    public delegate void RouteDoubleClick(GMapRoute item, MouseEventArgs e);
}
