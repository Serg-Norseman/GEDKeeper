/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Collections.Generic;
using GKMap.MapObjects;
using SkiaSharp;

namespace GKMap.Xamarin
{
    /// <summary>
    /// GKMap route.
    /// </summary>
    public class GMapRoute : MapRoute, IRenderable
    {
        public static readonly SKPaint DefaultStroke = new SKPaint() { Color = Extensions.FromArgb(144, SKColors.MidnightBlue), Style = SKPaintStyle.Stroke };


        protected SKPath fGraphicsPath;


        /// <summary>
        /// specifies how the outline is painted
        /// </summary>
        public SKPaint Stroke { get; set; }


        static GMapRoute()
        {
            DefaultStroke.StrokeJoin = SKStrokeJoin.Round;
            DefaultStroke.StrokeWidth = 5;
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
            return fGraphicsPath != null && fGraphicsPath.Contains(x, y);
        }

        public override void UpdateGraphicsPath()
        {
            if (fGraphicsPath != null) {
                fGraphicsPath.Dispose();
            }
            fGraphicsPath = new SKPath();

            for (int i = 0; i < LocalPoints.Count; i++) {
                GPoint p2 = LocalPoints[i];
                if (i == 0) {
                    fGraphicsPath.MoveTo(p2.X, p2.Y);
                } else {
                    fGraphicsPath.LineTo(p2.X, p2.Y);
                }
            }
        }

        public void OnRender(SKCanvas g)
        {
            if (IsVisible && fGraphicsPath != null) {
                g.DrawPath(fGraphicsPath, Stroke);
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
