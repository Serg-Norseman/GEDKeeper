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

namespace GKMap.WinForms
{
    /// <summary>
    /// GKMap polygon
    /// </summary>
    public class GMapPolygon : MapRoute
    {
        private bool fDisposed;
        private GraphicsPath fGraphicsPath;
        private bool fIsMouseOver;
        private GMapOverlay fOverlay;
        private bool fVisible = true;

        /// <summary>
        /// is polygon visible
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
                            Overlay.Control.UpdatePolygonLocalPosition(this);
                        } else {
                            if (Overlay.Control.IsMouseOverPolygon) {
                                Overlay.Control.IsMouseOverPolygon = false;
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

        public GMapOverlay Overlay
        {
            get {
                return fOverlay;
            }
            internal set {
                fOverlay = value;
            }
        }

        public static readonly Pen DefaultStroke = new Pen(Color.FromArgb(155, Color.MidnightBlue));

        /// <summary>
        /// specifies how the outline is painted
        /// </summary>
        public Pen Stroke = DefaultStroke;

        public static readonly Brush DefaultFill = new SolidBrush(Color.FromArgb(155, Color.AliceBlue));

        /// <summary>
        /// background color
        /// </summary>
        public Brush Fill = DefaultFill;

        public readonly List<GPoint> LocalPoints = new List<GPoint>();

        static GMapPolygon()
        {
            DefaultStroke.LineJoin = LineJoin.Round;
            DefaultStroke.Width = 5;
        }

        public GMapPolygon(List<PointLatLng> points, string name)
           : base(points, name)
        {
            LocalPoints.Capacity = Points.Count;
        }

        /// <summary>
        /// checks if point is inside the polygon,
        /// </summary>
        /// <param name="p"></param>
        /// <returns></returns>
        public bool IsInside(PointLatLng p)
        {
            int count = Points.Count;

            if (count < 3) {
                return false;
            }

            bool result = false;

            for (int i = 0, j = count - 1; i < count; i++) {
                var p1 = Points[i];
                var p2 = Points[j];

                if (p1.Lat < p.Lat && p2.Lat >= p.Lat || p2.Lat < p.Lat && p1.Lat >= p.Lat) {
                    if (p1.Lng + (p.Lat - p1.Lat) / (p2.Lat - p1.Lat) * (p2.Lng - p1.Lng) < p.Lng) {
                        result = !result;
                    }
                }
                j = i;
            }
            return result;
        }

        /// <summary>
        /// Indicates whether the specified point is contained within this System.Drawing.Drawing2D.GraphicsPath
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        internal bool IsInsideLocal(int x, int y)
        {
            if (fGraphicsPath != null) {
                return fGraphicsPath.IsVisible(x, y);
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

            Point[] pnts = new Point[LocalPoints.Count];
            for (int i = 0; i < LocalPoints.Count; i++) {
                Point p2 = new Point((int)LocalPoints[i].X, (int)LocalPoints[i].Y);
                pnts[pnts.Length - 1 - i] = p2;
            }

            if (pnts.Length > 2) {
                fGraphicsPath.AddPolygon(pnts);
            } else if (pnts.Length == 2) {
                fGraphicsPath.AddLines(pnts);
            }
        }

        public virtual void OnRender(Graphics g)
        {
            if (IsVisible && fGraphicsPath != null) {
                g.FillPath(Fill, fGraphicsPath);
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

    public delegate void PolygonClick(GMapPolygon item, MouseEventArgs e);
    public delegate void PolygonEnter(GMapPolygon item);
    public delegate void PolygonLeave(GMapPolygon item);
    public delegate void PolygonDoubleClick(GMapPolygon item, MouseEventArgs e);
}
