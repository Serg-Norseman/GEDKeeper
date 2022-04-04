/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Collections.Generic;
using Eto.Drawing;
using GKMap.MapObjects;

namespace GKMap.EtoForms
{
    /// <summary>
    /// GKMap route.
    /// </summary>
    public class GMapRoute : MapRoute, IRenderable
    {
        public static readonly Pen DefaultStroke = new Pen(Extensions.FromArgb(144, Colors.MidnightBlue));


        protected GraphicsPath fGraphicsPath;


        /// <summary>
        /// specifies how the outline is painted
        /// </summary>
        public Pen Stroke { get; set; }


        static GMapRoute()
        {
            DefaultStroke.LineJoin = PenLineJoin.Round;
            DefaultStroke.Thickness = 5;
        }

        public GMapRoute(string name, IEnumerable<PointLatLng> points = null)
            : base(name, points)
        {
            Stroke = DefaultStroke;
        }

        /// <summary>
        /// Indicates whether the specified point is contained within this System.Drawing.Drawing2D.GraphicsPath
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        public override bool IsInside(int x, int y)
        {
            // FIXME: -> StrokeContains, Eto >= 2.6.0
            return false;
            //return fGraphicsPath != null && fGraphicsPath.IsOutlineVisible(x, y, Stroke);
        }

        public override void UpdateGraphicsPath()
        {
            if (fGraphicsPath != null) {
                fGraphicsPath.Dispose();
            }
            fGraphicsPath = new GraphicsPath();

            for (int i = 0; i < LocalPoints.Count; i++) {
                GPoint p2 = LocalPoints[i];

                if (i == 0) {
                    fGraphicsPath.AddLine(p2.X, p2.Y, p2.X, p2.Y);
                } else {
                    PointF p = fGraphicsPath.CurrentPoint /*GetLastPoint()*/;
                    fGraphicsPath.AddLine(p.X, p.Y, p2.X, p2.Y);
                }
            }
        }

        public void OnRender(Graphics g)
        {
            if (IsVisible && fGraphicsPath != null) {
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
