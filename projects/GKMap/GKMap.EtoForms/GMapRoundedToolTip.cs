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
    /// GKMap rounded tooltip for markers.
    /// </summary>
    public class GMapRoundedToolTip : GMapToolTip
    {
        private const float Radius = 10f;

        public GMapRoundedToolTip(MapMarker marker)
           : base(marker)
        {
            TextPadding = new Size((int)Radius, (int)Radius);
        }

        public void DrawRoundRectangle(Graphics g, Pen pen, float h, float v, float width, float height, float radius)
        {
            using (GraphicsPath gp = new GraphicsPath()) {
                gp.AddLine(h + radius, v, h + width - (radius * 2), v);
                gp.AddArc(h + width - (radius * 2), v, radius * 2, radius * 2, 270, 90);
                gp.AddLine(h + width, v + radius, h + width, v + height - (radius * 2));
                gp.AddArc(h + width - (radius * 2), v + height - (radius * 2), radius * 2, radius * 2, 0, 90); // Corner
                gp.AddLine(h + width - (radius * 2), v + height, h + radius, v + height);
                gp.AddArc(h, v + height - (radius * 2), radius * 2, radius * 2, 90, 90);
                gp.AddLine(h, v + height - (radius * 2), h, v + radius);
                gp.AddArc(h, v, radius * 2, radius * 2, 180, 90);

                gp.CloseFigure();

                g.FillPath(Fill, gp);
                g.DrawPath(pen, gp);
            }
        }

        public override void OnRender(Graphics g)
        {
            var st = Font.MeasureString(Marker.ToolTipText);
            var rect = new RectangleF(Marker.ToolTipPosition.X, Marker.ToolTipPosition.Y - st.Height, st.Width + TextPadding.Width * 2, st.Height + TextPadding.Height);
            rect.Offset((int)Offset.X, (int)Offset.Y);

            g.DrawLine(Stroke, Marker.ToolTipPosition.X, Marker.ToolTipPosition.Y, rect.X + Radius / 2, rect.Y + rect.Height - Radius / 2);

            DrawRoundRectangle(g, Stroke, rect.X, rect.Y, rect.Width, rect.Height, Radius);

            /*if (Format.Alignment == StringAlignment.Near) {
                rect.Offset(TextPadding.Width, 0);
            }*/
            g.DrawText(Font, Foreground, rect, Marker.ToolTipText);
        }
    }
}
