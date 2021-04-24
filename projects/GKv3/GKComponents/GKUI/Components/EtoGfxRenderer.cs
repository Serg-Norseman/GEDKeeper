/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using BSLib;
using BSLib.Design.Graphics;
using Eto.Drawing;
using GKCore.Charts;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class EtoGfxRenderer : ChartRenderer
    {
        private Graphics fCanvas;
        private float fTranslucent;

        public EtoGfxRenderer() : base()
        {
        }

        public override void SetSmoothing(bool value)
        {
            fCanvas.AntiAlias = value;
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

        public override void DrawImage(IImage image, ExtRect destinationRect,
            ExtRect sourceRect)
        {
            var sdImage = ((ImageHandler)image).Handle;

            Rectangle destRect = UIHelper.Rt2Rt(destinationRect);
            Rectangle sourRect = UIHelper.Rt2Rt(sourceRect);
            fCanvas.DrawImage(sdImage, sourRect, destRect);
        }

        public override ExtSizeF GetTextSize(string text, IFont font)
        {
            Font sdFnt = ((FontHandler)font).Handle;
            var size = sdFnt.MeasureString(text);
            return new ExtSizeF(size.Width, size.Height);
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
            Color sdFillColor = ((ColorHandler)fillColor).Handle;

            using (GraphicsPath path = CreateRectangle(x, y, width, height)) {
                if (sdFillColor != Colors.Transparent) {
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
                if (sdFillColor != Colors.Transparent) {
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
            float alpha = (1 - fTranslucent);
            return new Color(color, alpha);
        }

        public override IPen CreatePen(IColor color, float width, float[] dashPattern = null)
        {
            Color sdColor = ((ColorHandler)color).Handle;
            sdColor = PrepareColor(sdColor);

            return new PenHandler(new Pen(sdColor, width));
        }

        public override IBrush CreateSolidBrush(IColor color)
        {
            Color sdColor = ((ColorHandler)color).Handle;
            sdColor = PrepareColor(sdColor);

            return new BrushHandler(new SolidBrush(sdColor));
        }

        public override IGfxPath CreatePath()
        {
            return new GfxPathHandler(new GraphicsPath());
        }

        public override void SetTranslucent(float value)
        {
            fTranslucent = Algorithms.CheckBounds(value, 0.0f, 1.0f);
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

        public override void ResetTransform()
        {
            // unsupported in Eto
        }

        public override void RestoreTransform()
        {
            fCanvas.RestoreTransform();
        }

        public override void SaveTransform()
        {
            fCanvas.SaveTransform();
        }
    }
}
