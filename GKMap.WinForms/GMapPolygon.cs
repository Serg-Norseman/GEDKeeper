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
using GKMap.MapObjects;

namespace GKMap.WinForms
{
    /// <summary>
    /// GKMap polygon.
    /// </summary>
    public class GMapPolygon : MapPolygon, IRenderable
    {
        public static readonly Brush DefaultFill = new SolidBrush(Color.FromArgb(155, Color.AliceBlue));
        public static readonly Pen DefaultStroke = new Pen(Color.FromArgb(155, Color.MidnightBlue));


        protected GraphicsPath fGraphicsPath;


        /// <summary>
        /// specifies how the outline is painted
        /// </summary>
        public Pen Stroke = DefaultStroke;

        /// <summary>
        /// background color
        /// </summary>
        public Brush Fill = DefaultFill;


        static GMapPolygon()
        {
            DefaultStroke.LineJoin = LineJoin.Round;
            DefaultStroke.Width = 5;
        }

        public GMapPolygon(string name, IEnumerable<PointLatLng> points)
            : base(name, points)
        {
        }

        /// <summary>
        /// Indicates whether the specified point is contained within this System.Drawing.Drawing2D.GraphicsPath
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        public override bool IsInside(int x, int y)
        {
            return fGraphicsPath != null && fGraphicsPath.IsVisible(x, y);
        }

        public override void UpdateGraphicsPath()
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

        public void OnRender(Graphics g)
        {
            if (IsVisible && fGraphicsPath != null) {
                g.FillPath(Fill, fGraphicsPath);
                g.DrawPath(Stroke, fGraphicsPath);
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing && fGraphicsPath != null) {
                fGraphicsPath.Dispose();
                fGraphicsPath = null;
            }
            base.Dispose(disposing);
        }
    }
}
