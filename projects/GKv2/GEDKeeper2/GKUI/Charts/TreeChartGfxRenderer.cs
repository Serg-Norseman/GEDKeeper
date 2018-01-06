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
using System.Drawing.Text;
using System.IO;
using System.Text;

using BSLib;
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
        private Matrix fMatrix;

        private TextWriter fSVGWriter;
        private SvgGraphics fSVGGfx;

        public TreeChartGfxRenderer() : base()
        {
        }

        public override void SetSVGMode(bool active, string svgFileName, int width, int height)
        {
            if (active) {
                fSVGWriter = new StreamWriter(new FileStream(svgFileName, FileMode.Create), Encoding.UTF8);
                fSVGGfx = new SvgGraphics(fSVGWriter, ExtRectF.CreateBounds(0, 0, width, height));
                fSVGGfx.BeginDrawing();
            } else {
                if (fSVGWriter != null) {
                    fSVGGfx.EndDrawing();
                    fSVGGfx = null;

                    fSVGWriter.Flush();
                    fSVGWriter.Close();
                    fSVGWriter = null;
                }
            }
        }

        public override void SetTarget(object target, bool antiAlias)
        {
            Graphics gfx = target as Graphics;
            if (gfx == null)
                throw new ArgumentException(@"Argument's type mismatch", "target");

            fCanvas = gfx;

            if (antiAlias) {
                fCanvas.TextRenderingHint = TextRenderingHint.AntiAlias;
                fCanvas.SmoothingMode = SmoothingMode.AntiAlias;
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

            return fCanvas.MeasureString(STR_HEIGHT_SAMPLE, sdFnt).ToSize().Height;
        }

        public override int GetTextWidth(string text, IFont font)
        {
            Font sdFnt = ((FontHandler)font).Handle;

            return fCanvas.MeasureString(text, sdFnt).ToSize().Width;
        }

        public override ExtSizeF GetTextSize(string text, IFont font)
        {
            return new ExtSizeF(GetTextWidth(text, font), GetTextHeight(font));
        }

        public override void DrawString(string text, IFont font, IBrush brush, float x, float y)
        {
            Brush sdBrush = ((BrushHandler)brush).Handle;
            Font sdFnt = ((FontHandler)font).Handle;

            fCanvas.DrawString(text, sdFnt, sdBrush, x, y);

            if (fSVGGfx != null) {
                fSVGGfx.SetFont(font);
                fSVGGfx.SetColor(brush.Color);
                fSVGGfx.DrawString(text, x, y);
            }
        }

        public override void DrawLine(IPen pen, float x1, float y1, float x2, float y2)
        {
            Pen sdPen = ((PenHandler)pen).Handle;

            fCanvas.DrawLine(sdPen, x1, y1, x2, y2);

            if (fSVGGfx != null) {
                fSVGGfx.SetColor(pen.Color);
                fSVGGfx.DrawLine(x1, y1, x2, y2, pen.Width);
            }
        }

        public override void DrawRectangle(IPen pen, IColor fillColor,
                                           float x, float y, float width, float height)
        {
            Color sdFillColor = ((ColorHandler)fillColor).Handle;

            using (GraphicsPath path = CreateRectangle(x, y, width, height)) {
                if (sdFillColor != Color.Transparent) {
                    fCanvas.FillPath(new SolidBrush(sdFillColor), path);
                }

                if (pen != null) {
                    Pen sdPen = ((PenHandler)pen).Handle;
                    fCanvas.DrawPath(sdPen, path);
                }
            }

            if (fSVGGfx != null) {
                if (sdFillColor != Color.Transparent) {
                    fSVGGfx.SetColor(fillColor);
                    fSVGGfx.FillRect(x, y, width, height);
                }

                if (pen != null) {
                    fSVGGfx.SetColor(pen.Color);
                    fSVGGfx.DrawRect(x, y, width, height, pen.Width);
                }
            }
        }

        public override void DrawRoundedRectangle(IPen pen, IColor fillColor, float x, float y,
                                                  float width, float height, float radius)
        {
            Color sdFillColor = ((ColorHandler)fillColor).Handle;

            using (GraphicsPath path = CreateRoundedRectangle(x, y, width, height, radius)) {
                if (sdFillColor != Color.Transparent) {
                    fCanvas.FillPath(new SolidBrush(sdFillColor), path);
                }

                if (pen != null) {
                    Pen sdPen = ((PenHandler)pen).Handle;
                    fCanvas.DrawPath(sdPen, path);
                }
            }

            if (fSVGGfx != null) {
                if (sdFillColor != Color.Transparent) {
                    fSVGGfx.SetColor(fillColor);
                    fSVGGfx.FillRoundedRect(x, y, width, height, radius);
                }

                if (pen != null) {
                    fSVGGfx.SetColor(pen.Color);
                    fSVGGfx.DrawRoundedRect(x, y, width, height, radius, pen.Width);
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
            fCanvas.ResetTransform();
            fMatrix = fCanvas.Transform;
        }

        public override void ScaleTransform(float sx, float sy)
        {
            Matrix m = new Matrix(sx, 0, 0, sy, 0, 0);
            m.Multiply(fMatrix, MatrixOrder.Append);
            fCanvas.Transform = m;
            fMatrix = m;
        }

        public override void TranslateTransform(float dx, float dy)
        {
            Matrix m = new Matrix(1, 0, 0, 1, dx, dy);
            m.Multiply(fMatrix, MatrixOrder.Append);
            fCanvas.Transform = m;
            fMatrix = m;
        }

        public override void RotateTransform(float angle)
        {
            float rotation = (float)((angle) * Math.PI / 180.0f);
            float cosine = (float)(Math.Cos(rotation));
            float sine = (float)(Math.Sin(rotation));
            Matrix m = new Matrix(cosine, sine, -sine, cosine, 0, 0);
            m.Multiply(fMatrix, MatrixOrder.Append);
            fCanvas.Transform = m;
            fMatrix = m;
        }

        public override object SaveTransform()
        {
            return fCanvas.Transform;
        }

        public override void RestoreTransform(object matrix)
        {
            var mtx = matrix as Matrix;

            fCanvas.Transform = mtx;
            fMatrix = mtx;
        }

        public override void DrawArcText(string text, float centerX, float centerY, float radius,
                                         float startAngle, float wedgeAngle,
                                         bool inside, bool clockwise, IFont font, IBrush brush)
        {
            ExtSizeF size = GetTextSize(text, font);
            radius = radius + size.Height / 2.0f;

            float textAngle = Math.Min((float)MathHelper.RadiansToDegrees((size.Width * 1.75f) / radius), wedgeAngle);
            float deltaAngle = (wedgeAngle - textAngle) / 2.0f;

            if (clockwise) {
                startAngle += deltaAngle;
            } else {
                startAngle += wedgeAngle - deltaAngle;
            }
            startAngle = -startAngle;

            Matrix previousTransformation = fCanvas.Transform;

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

                Matrix m = new Matrix(cosine, sine, -sine, cosine, x, y);
                m.Multiply(previousTransformation, MatrixOrder.Append);
                fCanvas.Transform = m;

                string chr = new string(text[i], 1);
                DrawString(chr, font, brush, 0, 0);
            }

            fCanvas.Transform = previousTransformation;
        }
    }
}
