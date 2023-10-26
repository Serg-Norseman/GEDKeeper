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
    /// GKMap polygon.
    /// </summary>
    public class GMapPolygon : MapPolygon, IRenderable
    {
        public static readonly SKPaint DefaultFill = new SKPaint() { Color = Extensions.FromArgb(155, SKColors.AliceBlue), Style = SKPaintStyle.Fill };
        public static readonly SKPaint DefaultStroke = new SKPaint() { Color = Extensions.FromArgb(155, SKColors.MidnightBlue), Style = SKPaintStyle.Stroke };


        protected SKPath fGraphicsPath;


        /// <summary>
        /// specifies how the outline is painted
        /// </summary>
        public SKPaint Stroke = DefaultStroke;

        /// <summary>
        /// background color
        /// </summary>
        public SKPaint Fill = DefaultFill;


        static GMapPolygon()
        {
            DefaultStroke.StrokeJoin = SKStrokeJoin.Round;
            DefaultStroke.StrokeWidth = 5;
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
            return fGraphicsPath != null && fGraphicsPath.Contains(x, y);
        }

        public override void UpdateGraphicsPath()
        {
            if (fGraphicsPath != null) {
                fGraphicsPath.Dispose();
            }
            fGraphicsPath = new SKPath();

            for (int i = 0; i < LocalPoints.Count; i++) {
                var pt = LocalPoints[i];
                if (i == 0) {
                    fGraphicsPath.MoveTo(pt.X, pt.Y);
                } else {
                    fGraphicsPath.LineTo(pt.X, pt.Y);
                }
            }
        }

        public void OnRender(SKCanvas g)
        {
            if (IsVisible && fGraphicsPath != null) {
                g.DrawPath(fGraphicsPath, Fill);
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
