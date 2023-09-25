/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017-2023 by Sergey V. Zhdanovskih.
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
using Eto.Drawing;
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
    public sealed class EtoGfxRenderer : ChartRenderer
    {
        private Graphics fCanvas;
        private float fTranslucent;

        public EtoGfxRenderer()
        {
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
            fCanvas.AntiAlias = value;
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

                    // TODO: implement output with transparency
                    fCanvas.DrawImage(sdImage, x, y, width, height);
                }
            } catch (Exception ex) {
                Logger.WriteError(string.Format("EtoGfxRenderer.DrawImage({0})", imName), ex);
            }
        }

        public override void DrawImage(IImage image, ExtRect destinationRect, ExtRect sourceRect)
        {
            var sdImage = ((ImageHandler)image).Handle;

            Rectangle destRect = UIHelper.Rt2Rt(destinationRect);
            Rectangle sourRect = UIHelper.Rt2Rt(sourceRect);
            fCanvas.DrawImage(sdImage, sourRect, destRect);
        }

        public override ExtSizeF GetTextSize(string text, IFont font)
        {
            if (string.IsNullOrEmpty(text) || font == null)
                return ExtSizeF.Empty;

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
            Color sdFillColor = (fillColor == null) ? Colors.Transparent : ((ColorHandler)fillColor).Handle;

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

            var etoPen = new Pen(sdColor, width);
            if (dashPattern != null) {
                etoPen.DashStyle = new DashStyle(0, dashPattern);
            }

            return new PenHandler(etoPen);
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

        public override void RestoreTransform()
        {
            fCanvas.RestoreTransform();
        }

        public override void SaveTransform()
        {
            fCanvas.SaveTransform();
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

            GraphicsPath path = new GraphicsPath();
            path.AddLine(mp1, mp2);
            path.AddLine(mp2, mp3);
            path.AddLine(mp3, mp1);
            gfx.FillPath(fillColor, path);
        }
    }
}
