/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Drawing;
using System.Drawing.Drawing2D;

namespace GKMap.WinForms
{
    /// <summary>
    /// GKMap marker
    /// </summary>
    public class GMapBaloonToolTip : GMapToolTip
    {
        private const float Radius = 10f;

        private new static readonly Pen DefaultStroke = new Pen(Color.FromArgb(140, Color.Navy));

        static GMapBaloonToolTip()
        {
            DefaultStroke.Width = 3;
            DefaultStroke.LineJoin = LineJoin.Round;
            DefaultStroke.StartCap = LineCap.RoundAnchor;
        }

        public GMapBaloonToolTip(GMapMarker marker)
            : base(marker)
        {
            Stroke = DefaultStroke;
            Fill = Brushes.Yellow;
        }

        public override void OnRender(Graphics g)
        {
            Size st = g.MeasureString(Marker.ToolTipText, Font).ToSize();
            Rectangle rect = new Rectangle(Marker.ToolTipPosition.X, Marker.ToolTipPosition.Y - st.Height, st.Width + TextPadding.Width, st.Height + TextPadding.Height);
            rect.Offset(Offset.X, Offset.Y);

            using (var objGP = new GraphicsPath()) {
                objGP.AddLine(rect.X + 2 * Radius, rect.Y + rect.Height, rect.X + Radius, rect.Y + rect.Height + Radius);
                objGP.AddLine(rect.X + Radius, rect.Y + rect.Height + Radius, rect.X + Radius, rect.Y + rect.Height);

                objGP.AddArc(rect.X, rect.Y + rect.Height - (Radius * 2), Radius * 2, Radius * 2, 90, 90);
                objGP.AddLine(rect.X, rect.Y + rect.Height - (Radius * 2), rect.X, rect.Y + Radius);
                objGP.AddArc(rect.X, rect.Y, Radius * 2, Radius * 2, 180, 90);
                objGP.AddLine(rect.X + Radius, rect.Y, rect.X + rect.Width - (Radius * 2), rect.Y);
                objGP.AddArc(rect.X + rect.Width - (Radius * 2), rect.Y, Radius * 2, Radius * 2, 270, 90);
                objGP.AddLine(rect.X + rect.Width, rect.Y + Radius, rect.X + rect.Width, rect.Y + rect.Height - (Radius * 2));
                objGP.AddArc(rect.X + rect.Width - (Radius * 2), rect.Y + rect.Height - (Radius * 2), Radius * 2, Radius * 2, 0, 90); // Corner

                objGP.CloseFigure();

                g.FillPath(Fill, objGP);
                g.DrawPath(Stroke, objGP);
            }

            g.DrawString(Marker.ToolTipText, Font, Foreground, rect, Format);
        }
    }
}
