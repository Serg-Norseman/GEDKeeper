/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using GKMap.MapObjects;
using SkiaSharp;

namespace GKMap.Xamarin
{
    /// <summary>
    /// GKMap marker tooltip.
    /// </summary>
    public class GMapToolTip : MapToolTip, IRenderable
    {
        public static readonly SKPaint DefaultFill = new SKPaint() { Color = Extensions.FromArgb(222, SKColors.AliceBlue), Style = SKPaintStyle.Fill };
        public static readonly SKPaint DefaultFont = new SKPaint(new SKFont(SKTypeface.FromFamilyName("Sans"), 14) { Embolden = true });
        public static readonly SKPaint DefaultForeground = new SKPaint() { Color = SKColors.Navy, Style = SKPaintStyle.Fill };
        public static readonly SKPaint DefaultStroke = new SKPaint() { Color = Extensions.FromArgb(140, SKColors.MidnightBlue), Style = SKPaintStyle.Stroke };

        /// <summary>
        /// font
        /// </summary>
        public SKPaint Font = DefaultFont;

        /// <summary>
        /// specifies how the outline is painted
        /// </summary>
        public SKPaint Stroke = DefaultStroke;

        /// <summary>
        /// background color
        /// </summary>
        public SKPaint Fill = DefaultFill;

        /// <summary>
        /// text foreground
        /// </summary>
        public SKPaint Foreground = DefaultForeground;

        /// <summary>
        /// text padding
        /// </summary>
        public SKSize TextPadding = new SKSize(10, 10);

        static GMapToolTip()
        {
            DefaultStroke.StrokeWidth = 2;
            DefaultStroke.StrokeJoin = SKStrokeJoin.Round;
            DefaultStroke.StrokeCap = SKStrokeCap.Round;

            //DefaultFormat.LineAlignment = StringAlignment.Center;
            //DefaultFormat.Alignment = StringAlignment.Center;
        }

        public GMapToolTip(MapMarker marker) : base(marker)
        {
        }

        public virtual void OnRender(SKCanvas g)
        {
            var stWidth = Font.MeasureText(Marker.ToolTipText);
            var stHeight = Font.TextSize;
            var rect = SKRect.Create(Marker.ToolTipPosition.X, Marker.ToolTipPosition.Y - stHeight, stWidth + TextPadding.Width, stHeight + TextPadding.Height);
            rect.Offset((int)Offset.X, (int)Offset.Y);

            g.DrawLine(Marker.ToolTipPosition.X, Marker.ToolTipPosition.Y, rect.Left, rect.Top + rect.Height / 2, Stroke);

            g.DrawRect(rect, Fill);
            g.DrawRect(rect, Stroke);

            g.DrawText(Marker.ToolTipText, rect.Left, rect.Top, Font);
        }
    }
}
