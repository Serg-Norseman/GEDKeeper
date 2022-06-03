/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using Eto.Drawing;
using GKMap.MapObjects;

namespace GKMap.EtoForms
{
    /// <summary>
    /// GKMap marker tooltip.
    /// </summary>
    public class GMapToolTip : MapToolTip, IRenderable
    {
        public static readonly Brush DefaultFill = new SolidBrush(Extensions.FromArgb(222, Colors.AliceBlue));
        public static readonly Font DefaultFont = new Font(FontFamilies.SansFamilyName, 14, FontStyle.Bold);
        public static readonly Brush DefaultForeground = new SolidBrush(Colors.Navy);
        public static readonly Pen DefaultStroke = new Pen(Extensions.FromArgb(140, Colors.MidnightBlue));

        /// <summary>
        /// font
        /// </summary>
        public Font Font = DefaultFont;

        /// <summary>
        /// specifies how the outline is painted
        /// </summary>
        public Pen Stroke = DefaultStroke;

        /// <summary>
        /// background color
        /// </summary>
        public Brush Fill = DefaultFill;

        /// <summary>
        /// text foreground
        /// </summary>
        public Brush Foreground = DefaultForeground;

        /// <summary>
        /// text padding
        /// </summary>
        public Size TextPadding = new Size(10, 10);

        static GMapToolTip()
        {
            DefaultStroke.Thickness = 2;
            DefaultStroke.LineJoin = PenLineJoin.Round;
            DefaultStroke.LineCap = PenLineCap.Round;

            //DefaultFormat.LineAlignment = StringAlignment.Center;
            //DefaultFormat.Alignment = StringAlignment.Center;
        }

        public GMapToolTip(MapMarker marker) : base(marker)
        {
        }

        public virtual void OnRender(Graphics g)
        {
            var st = Font.MeasureString(Marker.ToolTipText);
            var rect = new RectangleF(Marker.ToolTipPosition.X, Marker.ToolTipPosition.Y - st.Height, st.Width + TextPadding.Width, st.Height + TextPadding.Height);
            rect.Offset((int)Offset.X, (int)Offset.Y);

            g.DrawLine(Stroke, Marker.ToolTipPosition.X, Marker.ToolTipPosition.Y, rect.X, rect.Y + rect.Height / 2);

            g.FillRectangle(Fill, rect);
            g.DrawRectangle(Stroke, rect);

            g.DrawText(Font, Foreground, rect, Marker.ToolTipText);
        }
    }
}
