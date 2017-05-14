/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCore.Charts;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKUI.Charts
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class TreeChartGfxRenderer : ChartRenderer
    {
        private Graphics fCanvas;

        public TreeChartGfxRenderer() : base()
        {
        }

        public override void SetTarget(object target)
        {
            Graphics gfx = target as Graphics;
            if (gfx == null)
                throw new ArgumentException(@"Argument's type mismatch", "target");

            fCanvas = gfx;
        }

        public override void DrawImage(IImage image, float x, float y,
                                       float width, float height)
        {
            var sdImage = ((ImageHandler)image).Handle;
            fCanvas.DrawImage(sdImage, x, y, width, height);
        }

        public override void DrawImage(Image image, float x, float y,
                                       float width, float height)
        {
            fCanvas.DrawImage(image, x, y, width, height);
        }

        public override int GetTextHeight(Font font)
        {
            return fCanvas.MeasureString(STR_HEIGHT_SAMPLE, font).ToSize().Height;
        }

        public override int GetTextWidth(string text, Font font)
        {
            return fCanvas.MeasureString(text, font).ToSize().Width;
        }

        public override void DrawString(string text, Font font, Brush brush, float x, float y)
        {
            fCanvas.DrawString(text, font, brush, x, y);
        }

        public override void DrawLine(Pen pen, float x1, float y1, float x2, float y2)
        {
            fCanvas.DrawLine(pen, x1, y1, x2, y2);
        }

        private static GraphicsPath CreateRectangle(float x, float y, float width, float height)
        {
            float xw = x + width;
            float yh = y + height;

            GraphicsPath p = new GraphicsPath();
            p.StartFigure();

            p.AddLine(x, y, xw, y); // Top Edge
            p.AddLine(xw, y, xw, yh); // Right Edge
            p.AddLine(xw, yh, x, yh); // Bottom Edge
            p.AddLine(x, yh, x, y); // Left Edge

            p.CloseFigure();
            return p;
        }

        public override void DrawRectangle(Pen pen, Color fillColor,
                                           float x, float y, float width, float height)
        {
            using (GraphicsPath path = CreateRectangle(x, y, width, height)) {
                if (fillColor != Color.Transparent) {
                    fCanvas.FillPath(new SolidBrush(fillColor), path);
                }

                if (pen != null) {
                    fCanvas.DrawPath(pen, path);
                }
            }
        }

        private static GraphicsPath CreateRoundedRectangle(float x, float y, float width, float height, float radius)
        {
            float xw = x + width;
            float yh = y + height;
            float xwr = xw - radius;
            float yhr = yh - radius;
            float xr = x + radius;
            float yr = y + radius;
            float r2 = radius * 2;
            float xwr2 = xw - r2;
            float yhr2 = yh - r2;

            GraphicsPath p = new GraphicsPath();
            p.StartFigure();

            p.AddArc(x, y, r2, r2, 180, 90); // Top Left Corner
            p.AddLine(xr, y, xwr, y); // Top Edge
            p.AddArc(xwr2, y, r2, r2, 270, 90); // Top Right Corner
            p.AddLine(xw, yr, xw, yhr); // Right Edge
            p.AddArc(xwr2, yhr2, r2, r2, 0, 90); // Bottom Right Corner
            p.AddLine(xwr, yh, xr, yh); // Bottom Edge
            p.AddArc(x, yhr2, r2, r2, 90, 90); // Bottom Left Corner
            p.AddLine(x, yhr, x, yr); // Left Edge

            p.CloseFigure();
            return p;
        }

        public override void DrawRoundedRectangle(Pen pen, Color fillColor, float x, float y,
                                                  float width, float height, float radius)
        {
            using (GraphicsPath path = CreateRoundedRectangle(x, y, width, height, radius)) {
                if (fillColor != Color.Transparent) {
                    fCanvas.FillPath(new SolidBrush(fillColor), path);
                }

                if (pen != null) {
                    fCanvas.DrawPath(pen, path);
                }
            }
        }

        public override void CreateCircleSegment(GraphicsPath path,
                                                 float inRad, float extRad, float wedgeAngle,
                                                 float ang1, float ang2)
        {
            CreateCircleSegment(path, 0, 0, inRad, extRad, wedgeAngle, ang1, ang2);
        }

        public override void CreateCircleSegment(GraphicsPath path, int ctX, int ctY,
                                                 float inRad, float extRad, float wedgeAngle,
                                                 float ang1, float ang2)
        {
            UIHelper.CreateCircleSegment(path, ctX, ctY, inRad, extRad, wedgeAngle, ang1, ang2);
        }
    }
}
