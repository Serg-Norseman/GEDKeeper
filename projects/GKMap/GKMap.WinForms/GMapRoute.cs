﻿/*
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
    /// GKMap route.
    /// </summary>
    public class GMapRoute : MapRoute, IRenderable
    {
        public static readonly Pen DefaultStroke = new Pen(Color.FromArgb(144, Color.MidnightBlue));


        protected GraphicsPath fGraphicsPath;


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
            return fGraphicsPath != null && fGraphicsPath.IsOutlineVisible(x, y, Stroke);
        }

        public override void UpdateGraphicsPath()
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
