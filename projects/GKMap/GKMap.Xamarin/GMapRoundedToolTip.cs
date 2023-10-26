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
    /// GKMap rounded tooltip for markers.
    /// </summary>
    public class GMapRoundedToolTip : GMapToolTip
    {
        private const float Radius = 10f;

        public GMapRoundedToolTip(MapMarker marker)
           : base(marker)
        {
            TextPadding = new SKSize((int)Radius, (int)Radius);
        }

        public void DrawRoundRectangle(SKCanvas g, SKPaint pen, float h, float v, float width, float height, float radius)
        {
            using (SKPath gp = new SKPath()) {
                /*gp.AddLine(h + radius, v, h + width - (radius * 2), v);
                gp.AddArc(h + width - (radius * 2), v, radius * 2, radius * 2, 270, 90);
                gp.AddLine(h + width, v + radius, h + width, v + height - (radius * 2));
                gp.AddArc(h + width - (radius * 2), v + height - (radius * 2), radius * 2, radius * 2, 0, 90); // Corner
                gp.AddLine(h + width - (radius * 2), v + height, h + radius, v + height);
                gp.AddArc(h, v + height - (radius * 2), radius * 2, radius * 2, 90, 90);
                gp.AddLine(h, v + height - (radius * 2), h, v + radius);
                gp.AddArc(h, v, radius * 2, radius * 2, 180, 90);

                gp.CloseFigure();

                g.FillPath(Fill, gp);
                g.DrawPath(pen, gp);*/
            }
        }

        public override void OnRender(SKCanvas g)
        {
            /*var st = Font.MeasureString(Marker.ToolTipText);
            var rect = SKRect.Create(Marker.ToolTipPosition.X, Marker.ToolTipPosition.Y - st.Height, st.Width + TextPadding.Width * 2, st.Height + TextPadding.Height);
            rect.Offset((int)Offset.X, (int)Offset.Y);

            g.DrawLine(Stroke, Marker.ToolTipPosition.X, Marker.ToolTipPosition.Y, rect.Left + Radius / 2, rect.Top + rect.Height - Radius / 2);

            DrawRoundRectangle(g, Stroke, rect.Left, rect.Top, rect.Width, rect.Height, Radius);

            /*if (Format.Alignment == StringAlignment.Near) {
                rect.Offset(TextPadding.Width, 0);
            }*/
            //g.DrawText(Font, Foreground, rect, Marker.ToolTipText);
        }
    }
}
