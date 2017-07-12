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
using Eto.Drawing;
using GKCommon;
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

        public override void SetTarget(object target, bool antiAlias)
        {
            Graphics gfx = target as Graphics;
            if (gfx == null)
                throw new ArgumentException(@"Argument's type mismatch", "target");

            fCanvas = gfx;

            if (antiAlias) {
                //fCanvas.TextRenderingHint = TextRenderingHint.AntiAlias;
                fCanvas.AntiAlias = true;
                //fCanvas.SmoothingMode = SmoothingMode.AntiAlias;
            }
        }

        public override void DrawImage(IImage image, float x, float y,
                                       float width, float height)
        {
            var sdImage = ((ImageHandler)image).Handle;
            fCanvas.DrawImage(sdImage, x, y, width, height);
        }

        public override int GetTextHeight(IFont font)
        {
            Font sdFnt = ((FontHandler)font).Handle;

            return (int)sdFnt.MeasureString(STR_HEIGHT_SAMPLE).Height;
        }

        public override int GetTextWidth(string text, IFont font)
        {
            Font sdFnt = ((FontHandler)font).Handle;

            return (int)sdFnt.MeasureString(text).Width;
        }

        public override ExtSizeF GetTextSize(string text, IFont font)
        {
            return new ExtSizeF(GetTextWidth(text, font), GetTextHeight(font));
        }

        public override void DrawString(string text, IFont font, IBrush brush, float x, float y)
        {
            SolidBrush sdBrush = (SolidBrush)((BrushHandler)brush).Handle;
            Font sdFnt = ((FontHandler)font).Handle;

            fCanvas.DrawText(sdFnt, sdBrush, x, y, text);
        }

        public override void DrawLine(IPen pen, float x1, float y1, float x2, float y2)
        {
            Pen sdPen = ((PenHandler)pen).Handle;

            fCanvas.DrawLine(sdPen, x1, y1, x2, y2);
        }

        public override void DrawRectangle(IPen pen, IColor fillColor,
                                           float x, float y, float width, float height)
        {
            Color sdFillColor = ((ColorHandler)fillColor).Handle;

            using (GraphicsPath path = CreateRectangle(x, y, width, height)) {
                if (sdFillColor != Colors.Transparent) {
                    fCanvas.FillPath(new SolidBrush(sdFillColor), path);
                }

                if (pen != null) {
                    Pen sdPen = ((PenHandler)pen).Handle;
                    fCanvas.DrawPath(sdPen, path);
                }
            }
        }

        public override void DrawRoundedRectangle(IPen pen, IColor fillColor, float x, float y,
                                                  float width, float height, float radius)
        {
            Color sdFillColor = ((ColorHandler)fillColor).Handle;

            using (GraphicsPath path = CreateRoundedRectangle(x, y, width, height, radius)) {
                if (sdFillColor != Colors.Transparent) {
                    fCanvas.FillPath(new SolidBrush(sdFillColor), path);
                }

                if (pen != null) {
                    Pen sdPen = ((PenHandler)pen).Handle;
                    fCanvas.DrawPath(sdPen, path);
                }
            }
        }

        public override void CreateCircleSegment(IGfxPath path,
                                                 float inRad, float extRad, float wedgeAngle,
                                                 float ang1, float ang2)
        {
            CreateCircleSegment(path, 0, 0, inRad, extRad, wedgeAngle, ang1, ang2);
        }

        public override void CreateCircleSegment(IGfxPath path, int ctX, int ctY,
                                                 float inRad, float extRad, float wedgeAngle,
                                                 float ang1, float ang2)
        {
            GraphicsPath sdPath = ((GfxPathHandler)path).Handle;

            UIHelper.CreateCircleSegment(sdPath, ctX, ctY, inRad, extRad, wedgeAngle, ang1, ang2);
        }

        public override void FillPath(IBrush brush, IGfxPath path)
        {
            Brush sdBrush = ((BrushHandler)brush).Handle;
            GraphicsPath sdPath = ((GfxPathHandler)path).Handle;

            fCanvas.FillPath(sdBrush, sdPath);
        }

        public override void DrawPath(IPen pen, IGfxPath path)
        {
            Pen sdPen = ((PenHandler)pen).Handle;
            GraphicsPath sdPath = ((GfxPathHandler)path).Handle;

            fCanvas.DrawPath(sdPen, sdPath);
        }

        public override IPen CreatePen(IColor color, float width)
        {
            Color sdColor = ((ColorHandler)color).Handle;

            return new PenHandler(new Pen(sdColor, width));
        }

        public override IBrush CreateSolidBrush(IColor color)
        {
            Color sdColor = ((ColorHandler)color).Handle;

            return new BrushHandler(new SolidBrush(sdColor));
        }

        public override IGfxPath CreatePath()
        {
            return new GfxPathHandler(new GraphicsPath());
        }

        #region Private helpers

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

        #endregion

        public override void ResetTransform()
        {
            // unsupported in Eto
        }

        public override void ScaleTransform(float sx, float sy)
        {
            fCanvas.ScaleTransform(sx, sy);
        }

        public override void TranslateTransform(float dx, float dy)
        {
            fCanvas.TranslateTransform(dx, dy);
        }

        public override void RotateTransform(float angle)
        {
            fCanvas.RotateTransform(angle);
        }

        public override object SaveTransform()
        {
            fCanvas.SaveTransform();
            return fCanvas.CurrentTransform;
        }

        public override void RestoreTransform(object matrix)
        {
            fCanvas.RestoreTransform();
        }

        public override void DrawArcText(string text, float centerX, float centerY, float radius,
                                         float startAngle, float wedgeAngle,
                                         bool inside, bool clockwise, IFont font, IBrush brush)
        {
            ExtSizeF size = GetTextSize(text, font);
            radius = radius + size.Height / 2.0f;

            float textAngle = Math.Min((float)SysUtils.RadiansToDegrees((size.Width * 1.75f) / radius), wedgeAngle);
            float deltaAngle = (wedgeAngle - textAngle) / 2.0f;

            if (clockwise) {
                startAngle += deltaAngle;
            } else {
                startAngle += wedgeAngle - deltaAngle;
            }
            startAngle = -startAngle;

            for (int i = 0; i < text.Length; ++i)
            {
                float offset = (textAngle * ((float)(i) / text.Length));
                float angle = clockwise ? startAngle - offset : startAngle + offset;

                double radAngle = angle * (Math.PI / 180.0d);
                float x = (float)(centerX + Math.Cos(radAngle) * radius);
                float y = (float)(centerY - Math.Sin(radAngle) * radius);
                float charRotation = 90 - (inside ? angle : angle + 180);
                charRotation *= (float)(Math.PI / 180.0f);
                float cosine = (float)(Math.Cos(charRotation));
                float sine = (float)(Math.Sin(charRotation));

                fCanvas.SaveTransform();

                IMatrix m = Matrix.Create(cosine, sine, -sine, cosine, x, y);
                fCanvas.MultiplyTransform(m);

                string chr = new string(text[i], 1);
                DrawString(chr, font, brush, 0, 0);

                fCanvas.RestoreTransform();
            }
        }
    }
}
