/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using GKCommon;

namespace GKUI.Charts
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class TreeChartGfxRenderer : TreeChartRenderer
    {
        private Graphics fCanvas;

        public TreeChartGfxRenderer(bool autoScale) : base(autoScale)
        {
        }

        public override void SetTarget(object target)
        {
            Graphics gfx = target as Graphics;
            if (gfx == null)
                throw new ArgumentException(@"Argument's type mismatch", "target");

            fCanvas = gfx;
        }

        public override void DrawImage(Image image, int x, int y)
        {
            fCanvas.DrawImage(image, x, y);
        }

        public override void DrawImage(Image image, ExtRect rect)
        {
            fCanvas.DrawImage(image, rect.Left, rect.Top, rect.GetWidth(), rect.GetHeight());
        }

        public override int GetTextHeight(string text, Font font)
        {
            return fCanvas.MeasureString(text, font).ToSize().Height;
        }

        public override int GetTextWidth(string text, Font font)
        {
            return fCanvas.MeasureString(text, font).ToSize().Width;
        }

        public override void DrawString(string text, Font font, Brush brush, int x, int y)
        {
            fCanvas.DrawString(text, font, brush, x, y);
        }

        public override void DrawLine(Pen pen, int x1, int y1, int x2, int y2)
        {
            fCanvas.DrawLine(pen, x1, y1, x2, y2);
        }

        public override void DrawRectangle(Pen pen, Color fillColor,
                                           int x, int y, int width, int height)
        {
            GraphicsPath path = SysUtils.CreateRectangle(x, y, width, height);

            if (fillColor != Color.Transparent) {
                fCanvas.FillPath(new SolidBrush(fillColor), path);
            }

            if (pen != null) {
                fCanvas.DrawPath(pen, path);
            }
        }

        public override void DrawRoundedRectangle(Pen pen, Color fillColor,
                                                  int x, int y, int width, int height, int radius)
        {
            GraphicsPath path = SysUtils.CreateRoundedRectangle(x, y, width, height, radius);

            if (fillColor != Color.Transparent) {
                fCanvas.FillPath(new SolidBrush(fillColor), path);
            }

            if (pen != null) {
                fCanvas.DrawPath(pen, path);
            }
        }
    }
}
