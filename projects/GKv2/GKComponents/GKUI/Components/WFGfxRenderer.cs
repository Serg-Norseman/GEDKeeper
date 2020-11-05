/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using BSLib.Design.Graphics;
using BSLib.Design.Handlers;
using GKCore.Charts;
using GKUI.Components;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class WFGfxRenderer : ChartRenderer
    {
        private Graphics fCanvas;
        private readonly Stack<Matrix> fTransforms;
        private float fTranslucent;

        public WFGfxRenderer() : base()
        {
            fTransforms = new Stack<Matrix>();
        }

        public override void SetTarget(object target)
        {
            Graphics gfx = target as Graphics;
            if (gfx == null)
                throw new ArgumentNullException("target");

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
                                       float width, float height)
        {
            var sdImage = ((ImageHandler)image).Handle;
            //fCanvas.DrawImage(sdImage, x, y, width, height);

            using (var attributes = new ImageAttributes()) {
                ColorMatrix matrix = new ColorMatrix();
                matrix.Matrix33 = 1 - fTranslucent; // opacity 0 = completely transparent, 1 = completely opaque

                attributes.SetColorMatrix(matrix, ColorMatrixFlag.Default, ColorAdjustType.Bitmap);
                var destRect = new Rectangle((int)x, (int)y, (int)width, (int)height);
                fCanvas.DrawImage(sdImage, destRect, 0, 0, sdImage.Width, sdImage.Height, GraphicsUnit.Pixel, attributes);
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

        public override IBrush CreateSolidBrush(IColor color)
        {
            Color sdColor = ((ColorHandler)color).Handle;
            sdColor = PrepareColor(sdColor);

            return new BrushHandler(new SolidBrush(sdColor));
        }

        public override void SetTranslucent(float value)
        {
            fTranslucent = Algorithms.CheckBounds(value, 0.0f, 1.0f);
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

        public override void ResetTransform()
        {
            fCanvas.ResetTransform();
            fTransforms.Push(fCanvas.Transform);
        }

        public override void RestoreTransform()
        {
            if (fTransforms.Count > 1) {
                fCanvas.Transform = fTransforms.Pop();
            } else {
                ResetTransform();
            }
        }

        public override void SaveTransform()
        {
            fTransforms.Push(fCanvas.Transform);
        }
    }
}
