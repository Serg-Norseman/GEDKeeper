/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using BSLib;
using GKCore;
using GKCore.Charts;
using GKCore.Design.Graphics;
using GKUI.Components;
using GKUI.Platform.Handlers;

namespace GKUI.Platform
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class WFGfxRenderer : ChartRenderer
    {
        private Graphics fCanvas;
        private ImageAttributes fImageAttributes; // FIXME: disposable!
        private readonly Stack<Matrix> fTransforms;
        private float fTranslucent;

        public WFGfxRenderer()
        {
            fTransforms = new Stack<Matrix>();
        }

        public override void SetTarget(object target)
        {
            Graphics gfx = target as Graphics;
            if (gfx == null)
                throw new ArgumentException("target");

            fCanvas = gfx;
        }

        public override void SetSmoothing(bool value)
        {
            if (value) {
                fCanvas.InterpolationMode = InterpolationMode.HighQualityBicubic;
                fCanvas.PixelOffsetMode = PixelOffsetMode.HighQuality;
                fCanvas.CompositingQuality = CompositingQuality.HighQuality;
                fCanvas.SmoothingMode = SmoothingMode.HighQuality;
            } else {
                fCanvas.InterpolationMode = InterpolationMode.Default;
                fCanvas.PixelOffsetMode = PixelOffsetMode.Default;
                fCanvas.CompositingQuality = CompositingQuality.Default;
                fCanvas.SmoothingMode = SmoothingMode.Default;
            }
        }

        public override void DrawArc(IPen pen, float x, float y, float width, float height, float startAngle, float sweepAngle)
        {
            Pen sdPen = ((PenHandler)pen).Handle;

            fCanvas.DrawArc(sdPen, x, y, width, height, startAngle, sweepAngle);
        }

        public override void DrawImage(IImage image, float x, float y,
                                       float width, float height, string imName)
        {
            try {
                if (fCanvas != null && image != null) {
                    var sdImage = ((ImageHandler)image).Handle;
                    var destRect = new Rectangle((int)x, (int)y, (int)width, (int)height);
                    fCanvas.DrawImage(sdImage, destRect, 0, 0, sdImage.Width, sdImage.Height, GraphicsUnit.Pixel, fImageAttributes);
                }
            } catch (Exception ex) {
                Logger.WriteError(string.Format("WFGfxRenderer.DrawImage({0})", imName), ex);
            }
        }

        public override void DrawImage(IImage image, ExtRect destinationRect, ExtRect sourceRect)
        {
            var sdImage = ((ImageHandler)image).Handle;

            Rectangle destRect = UIHelper.Rt2Rt(destinationRect);
            Rectangle sourRect = UIHelper.Rt2Rt(sourceRect);
            fCanvas.DrawImage(sdImage, destRect, sourRect, GraphicsUnit.Pixel);
        }

        public override ExtSizeF GetTextSize(string text, IFont font)
        {
            if (string.IsNullOrEmpty(text) || font == null)
                return ExtSizeF.Empty;

            Font sdFnt = ((FontHandler)font).Handle;
            var size = fCanvas.MeasureString(text, sdFnt);
            return new ExtSizeF(size.Width, size.Height);
        }

        public override void DrawString(string text, IFont font, IBrush brush, float x, float y)
        {
            Brush sdBrush = ((BrushHandler)brush).Handle;
            Font sdFnt = ((FontHandler)font).Handle;

            fCanvas.DrawString(text, sdFnt, sdBrush, x, y);
        }

        private static float GetEmFontSize(Font fnt) =>
            fnt.SizeInPoints * (fnt.FontFamily.GetCellAscent(fnt.Style) +
            fnt.FontFamily.GetCellDescent(fnt.Style)) / fnt.FontFamily.GetEmHeight(fnt.Style);

        private static void DrawGlowString(Graphics gfx, string text, Font font, Brush foreBrush, float x, float y, Color glowColor, float glowSize)
        {
            //gfx.SmoothingMode = SmoothingMode.AntiAlias;
            //gfx.InterpolationMode = InterpolationMode.HighQualityBicubic;

            using (var strformat = new StringFormat())
            using (var path = new GraphicsPath()) {
                path.AddString(text, font.FontFamily, (int)font.Style, GetEmFontSize(font), new PointF(x, y), strformat);

                int r = glowColor.R;
                int g = glowColor.G;
                int b = glowColor.B;

                for (int i = 1; i < glowSize; ++i) {
                    using (var pen = new Pen(Color.FromArgb(32, r, g, b), i)) {
                        pen.LineJoin = LineJoin.Round;
                        gfx.DrawPath(pen, path);
                    }
                }

                gfx.FillPath(foreBrush, path);
            }
        }

        public override void DrawString(string text, IFont font, IBrush brush, float x, float y, TextEffect effect = TextEffect.Simple)
        {
            Brush sdBrush = ((BrushHandler)brush).Handle;
            Font sdFnt = ((FontHandler)font).Handle;

            switch (effect) {
                case TextEffect.Sunken:
                    fCanvas.DrawString(text, sdFnt, Brushes.White, x + 1, y + 1);
                    break;

                case TextEffect.Raised:
                    fCanvas.DrawString(text, sdFnt, Brushes.White, x - 1, y - 1);
                    break;

                case TextEffect.Glow:
                    DrawGlowString(fCanvas, text, sdFnt, sdBrush, x, y, Color.Gold, 5);
                    return;
            }

            fCanvas.DrawString(text, sdFnt, sdBrush, x, y);
        }

        public override void DrawLine(IPen pen, float x1, float y1, float x2, float y2)
        {
            Pen sdPen = ((PenHandler)pen).Handle;

            fCanvas.DrawLine(sdPen, x1, y1, x2, y2);
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

        public override void DrawRectangle(IPen pen, IColor fillColor,
                                           float x, float y, float width, float height)
        {
            Color sdFillColor = (fillColor == null) ? Color.Transparent : ((ColorHandler)fillColor).Handle;

            using (GraphicsPath path = CreateRectangle(x, y, width, height)) {
                if (sdFillColor != Color.Transparent) {
                    sdFillColor = PrepareColor(sdFillColor);

                    fCanvas.FillPath(new SolidBrush(sdFillColor), path);
                }

                if (pen != null) {
                    Pen sdPen = ((PenHandler)pen).Handle;
                    fCanvas.DrawPath(sdPen, path);
                }
            }
        }

        public override void FillRectangle(IBrush brush,
                                           float x, float y, float width, float height)
        {
            if (brush != null) {
                Brush sdBrush = ((BrushHandler)brush).Handle;
                fCanvas.FillRectangle(sdBrush, x, y, width, height);
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

        public override void DrawRoundedRectangle(IPen pen, IColor fillColor, float x, float y,
                                                  float width, float height, float radius)
        {
            Color sdFillColor = ((ColorHandler)fillColor).Handle;

            using (GraphicsPath path = CreateRoundedRectangle(x, y, width, height, radius)) {
                if (sdFillColor != Color.Transparent) {
                    sdFillColor = PrepareColor(sdFillColor);

                    fCanvas.FillPath(new SolidBrush(sdFillColor), path);
                }

                if (pen != null) {
                    Pen sdPen = ((PenHandler)pen).Handle;
                    fCanvas.DrawPath(sdPen, path);
                }
            }
        }

        public override void DrawPath(IPen pen, IBrush brush, IGfxPath path)
        {
            GraphicsPath sdPath = ((GfxPathHandler)path).Handle;

            if (brush != null) {
                Brush sdBrush = ((BrushHandler)brush).Handle;
                fCanvas.FillPath(sdBrush, sdPath);
            }

            if (pen != null) {
                Pen sdPen = ((PenHandler)pen).Handle;
                fCanvas.DrawPath(sdPen, sdPath);
            }
        }

        private Color PrepareColor(Color color)
        {
            int alpha = (int)((1 - fTranslucent) * 255);
            return Color.FromArgb(alpha, color);
        }

        public override IPen CreatePen(IColor color, float width, float[] dashPattern = null)
        {
            Color sdColor = ((ColorHandler)color).Handle;
            sdColor = PrepareColor(sdColor);

            var wfPen = new Pen(sdColor, width);
            if (dashPattern != null) {
                wfPen.DashPattern = dashPattern;
            }

            return new PenHandler(wfPen);
        }

        public override IBrush CreateBrush(IColor color)
        {
            Color sdColor = ((ColorHandler)color).Handle;
            sdColor = PrepareColor(sdColor);

            return new BrushHandler(new SolidBrush(sdColor));
        }

        public override IGfxPath CreateCirclePath(float x, float y, float width, float height)
        {
            var path = new GraphicsPath();
            var result = new GfxCirclePathHandler(path);

            result.X = x;
            result.Y = y;
            result.Width = width;
            result.Height = height;

            path.StartFigure();
            path.AddEllipse(x, y, width, height);
            path.CloseFigure();

            return result;
        }

        public override IGfxPath CreateCircleSegmentPath(int ctX, int ctY, float inRad, float extRad, float wedgeAngle, float ang1, float ang2)
        {
            var path = new GraphicsPath();
            var result = new GfxCircleSegmentPathHandler(path);

            result.InRad = inRad;
            result.ExtRad = extRad;
            result.WedgeAngle = wedgeAngle;
            result.Ang1 = ang1;
            result.Ang2 = ang2;

            float angCos, angSin;

            float angval1 = (float)(ang1 * Math.PI / 180.0f);
            angCos = (float)Math.Cos(angval1);
            angSin = (float)Math.Sin(angval1);
            float px1 = ctX + (inRad * angCos);
            float py1 = ctY + (inRad * angSin);
            float px2 = ctX + (extRad * angCos);
            float py2 = ctY + (extRad * angSin);

            float angval2 = (float)(ang2 * Math.PI / 180.0f);
            angCos = (float)Math.Cos(angval2);
            angSin = (float)Math.Sin(angval2);
            float nx1 = ctX + (inRad * angCos);
            float ny1 = ctY + (inRad * angSin);
            float nx2 = ctX + (extRad * angCos);
            float ny2 = ctY + (extRad * angSin);

            float ir2 = inRad * 2.0f;
            float er2 = extRad * 2.0f;

            path.StartFigure();
            path.AddLine(px2, py2, px1, py1);
            if (ir2 > 0) path.AddArc(ctX - inRad, ctY - inRad, ir2, ir2, ang1, wedgeAngle);
            path.AddLine(nx1, ny1, nx2, ny2);
            path.AddArc(ctX - extRad, ctY - extRad, er2, er2, ang2, -wedgeAngle);
            path.CloseFigure();

            return result;
        }

        public override void SetTranslucent(float value)
        {
            fTranslucent = Algorithms.CheckBounds(value, 0.0f, 1.0f);

            var colorMatrix = new ColorMatrix();
            colorMatrix.Matrix33 = 1 - fTranslucent; // opacity 0 = completely transparent, 1 = completely opaque

            if (fImageAttributes != null)
                fImageAttributes.Dispose();

            fImageAttributes = new ImageAttributes();
            fImageAttributes.SetColorMatrix(colorMatrix, ColorMatrixFlag.Default, ColorAdjustType.Bitmap);
        }

        public override void ScaleTransform(float sx, float sy)
        {
            var matrix = fCanvas.Transform;
            Matrix m = new Matrix(sx, 0, 0, sy, 0, 0);
            m.Multiply(matrix, MatrixOrder.Append);
            fCanvas.Transform = m;
        }

        public override void TranslateTransform(float dx, float dy)
        {
            var matrix = fCanvas.Transform;
            Matrix m = new Matrix(1, 0, 0, 1, dx, dy);
            m.Multiply(matrix, MatrixOrder.Append);
            fCanvas.Transform = m;
        }

        public override void RotateTransform(float angle)
        {
            var matrix = fCanvas.Transform;
            float rotation = (float)((angle) * Math.PI / 180.0f);
            float cosine = (float)(Math.Cos(rotation));
            float sine = (float)(Math.Sin(rotation));
            Matrix m = new Matrix(cosine, sine, -sine, cosine, 0, 0);
            m.Multiply(matrix, MatrixOrder.Append);
            fCanvas.Transform = m;
        }

        public override void RestoreTransform()
        {
            if (fTransforms.Count > 1) {
                fCanvas.Transform = fTransforms.Pop();
            }
        }

        public override void SaveTransform()
        {
            fTransforms.Push(fCanvas.Transform);
        }

        public static void DrawArrowLine(Graphics gfx, Color fillColor, Pen pen, float x1, float y1, float x2, float y2, int arrLength = 8)
        {
            gfx.DrawLine(pen, x1, y1, x2, y2);

            var m = x2 - x1 == 0 ? 0 : (y2 - y1) / (x2 - x1);
            var degree = Math.Atan(m);
            var toLeft = x2 > x1 ? 0 : Math.PI;

            var degree1 = degree + 5 * Math.PI / 6 + toLeft;
            var degree2 = degree + 7 * Math.PI / 6 + toLeft;

            var px1 = x2 + (float)Math.Cos(degree1) * arrLength;
            var py1 = y2 + (float)Math.Sin(degree1) * arrLength;

            var px2 = x2 + (float)Math.Cos(degree2) * arrLength;
            var py2 = y2 + (float)Math.Sin(degree2) * arrLength;

            var mp1 = new PointF(x2, y2);
            var mp2 = new PointF(px1, py1);
            var mp3 = new PointF(px2, py2);

            using (var brush = new SolidBrush(fillColor)) {
                GraphicsPath path = new GraphicsPath();
                path.AddLine(mp1, mp2);
                path.AddLine(mp2, mp3);
                path.AddLine(mp3, mp1);
                gfx.FillPath(brush, path);
            }
        }
    }
}
