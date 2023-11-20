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
using BSLib;
using GKCore;
using GKCore.Charts;
using GKCore.Design.Graphics;
using GKUI.Components;
using SkiaSharp;
using SkiaSharp.Views.Forms;
using Xamarin.Forms;

namespace GKUI.Platform
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class XFGfxRenderer : ChartRenderer
    {
        private bool fAntiAlias;
        private SKSurface fSurface;
        private SKCanvas fCanvas;
        private float fTranslucent;

        public XFGfxRenderer()
        {
        }

        public override void SetTarget(object target)
        {
            var gfx = target as SKSurface;
            if (gfx == null)
                throw new ArgumentException("target");

            fSurface = gfx;
            fCanvas = gfx.Canvas;
        }

        public override void SetSmoothing(bool value)
        {
            fAntiAlias = value;
        }

        public override void DrawArc(IPen pen, float x, float y, float width, float height, float startAngle, float sweepAngle)
        {
            if (pen != null) {
                var skPaint = ((PenHandler)pen).Handle;
                fCanvas.DrawArc(SKRect.Create(x, y, width, height), startAngle, sweepAngle, false, skPaint);
            }
        }

        public override IImage LoadImage(string fileName)
        {
            if (fileName == null)
                throw new ArgumentNullException("fileName");

            var img = SKImage.FromEncodedData(fileName);
            return new SKImageHandler(img);
        }

        public override IImage LoadResourceImage(Type baseType, string resName)
        {
            if (string.IsNullOrEmpty(resName))
                return null;

            using (var stream = GKUtils.LoadResourceStream(baseType, resName)) {
                var img = SKImage.FromEncodedData(stream);
                return new SKImageHandler(img);
            }
        }

        public override IImage LoadResourceImage(string resName, bool makeTransp = false)
        {
            return LoadResourceImage(typeof(GKUtils), resName);
        }

        public override void DrawImage(IImage image, float destX, float destY,
                                       float destWidth, float destHeight, string imName)
        {
            if (fCanvas == null || image == null)
                return;

            try {
                var skImage = ((SKImageHandler)image).Handle;
                var destRect = SKRect.Create(destX, destY, destWidth, destHeight);
                var sourRect = SKRect.Create(0, 0, skImage.Image.Width, skImage.Image.Height);
                fCanvas.DrawImage(skImage, sourRect, destRect);
            } catch (Exception ex) {
                Logger.WriteError(string.Format("EtoGfxRenderer.DrawImage({0})", imName), ex);
            }
        }

        public override void DrawImage(IImage image, ExtRect destinationRect, ExtRect sourceRect)
        {
            if (fCanvas == null || image == null)
                return;

            var skImage = ((SKImageHandler)image).Handle;
            var destRect = UIHelper.Rt2SkRt(destinationRect);
            var sourRect = UIHelper.Rt2SkRt(sourceRect);
            fCanvas.DrawImage(skImage, sourRect, destRect);
        }

        public override ExtSizeF GetTextSize(string text, IFont font)
        {
            if (string.IsNullOrEmpty(text) || font == null)
                return ExtSizeF.Empty;

            var skPaint = ((FontHandler)font).Handle;
            var width = skPaint.MeasureText(text);
            return new ExtSizeF(width, skPaint.TextSize);
        }

        public override void DrawString(string text, IFont font, IBrush brush, float x, float y)
        {
            var color = (brush != null) ? ((BrushHandler)brush).Handle.Color.ToFormsColor() : Color.Black;

            var skPaint = ((FontHandler)font).Handle;
            var yOffset = skPaint.FontMetrics.Ascent;
            skPaint.IsAntialias = fAntiAlias;
            skPaint.Color = color.ToSKColor();
            fCanvas.DrawText(text, x, y - yOffset, skPaint);
        }

        public override void DrawLine(IPen pen, float x1, float y1, float x2, float y2)
        {
            if (pen != null) {
                var skPaint = ((PenHandler)pen).Handle;
                fCanvas.DrawLine(x1, y1, x2, y2, skPaint);
            }
        }

        public override void DrawRectangle(IPen pen, IColor fillColor,
                                           float x, float y, float width, float height)
        {
            if (fillColor != null) {
                var skPaint = new SKPaint();
                skPaint.Color = ((ColorHandler)fillColor).Handle.ToSKColor();
                skPaint.Style = SKPaintStyle.Fill;
                fCanvas.DrawRect(x, y, width, height, skPaint);
            }

            if (pen != null) {
                var skPaint = ((PenHandler)pen).Handle;
                fCanvas.DrawRect(x, y, width, height, skPaint);
            }
        }

        public override void FillRectangle(IBrush brush,
                                           float x, float y, float width, float height)
        {
            if (brush != null) {
                var skPaint = ((BrushHandler)brush).Handle;
                skPaint.Style = SKPaintStyle.Fill;
                fCanvas.DrawRect(x, y, width, height, skPaint);
            }
        }

        public override void DrawRoundedRectangle(IPen pen, IColor fillColor, float x, float y,
                                                  float width, float height, float radius)
        {
            if (fillColor != null) {
                var skPaint = new SKPaint();
                skPaint.Color = ((ColorHandler)fillColor).Handle.ToSKColor();
                skPaint.Style = SKPaintStyle.Fill;
                fCanvas.DrawRoundRect(x, y, width, height, radius, radius, skPaint);
            }

            if (pen != null) {
                var skPaint = ((PenHandler)pen).Handle;
                fCanvas.DrawRoundRect(x, y, width, height, radius, radius, skPaint);
            }
        }

        public override void DrawPath(IPen pen, IBrush brush, IGfxPath path)
        {
            if (brush != null) {
                var skPaint = ((BrushHandler)brush).Handle;
                skPaint.Style = SKPaintStyle.Fill;
                fCanvas.DrawPath(((GfxPathHandler)path).Handle, skPaint);
            }

            if (pen != null) {
                var skPaint = ((PenHandler)pen).Handle;
                fCanvas.DrawPath(((GfxPathHandler)path).Handle, skPaint);
            }
        }

        /*private SKColor PrepareColor(Color color)
        {
            float alpha = (1 - fTranslucent);
            return new SKColor(color, alpha);
        }*/

        public override IPen CreatePen(IColor color, float width, float[] dashPattern = null)
        {
            Color xfColor = ((ColorHandler)color).Handle;
            //sdColor = PrepareColor(sdColor);
            var skPaint = new SKPaint();
            skPaint.Color = xfColor.ToSKColor();
            skPaint.StrokeWidth = width;
            skPaint.Style = SKPaintStyle.Stroke;
            skPaint.IsAntialias = fAntiAlias;

            if (dashPattern != null) {
                skPaint.PathEffect = SKPathEffect.CreateDash(dashPattern, 0);
            }

            return new PenHandler(skPaint);
        }

        public override IBrush CreateBrush(IColor color)
        {
            Color xfColor = ((ColorHandler)color).Handle;
            //sdColor = PrepareColor(sdColor);
            var skPaint = new SKPaint();
            skPaint.Color = xfColor.ToSKColor();
            skPaint.Style = SKPaintStyle.Fill;
            skPaint.IsAntialias = fAntiAlias;

            return new BrushHandler(skPaint);
        }

        public override IGfxPath CreateCirclePath(float x, float y, float width, float height)
        {
            var path = new SKPath();

            var result = new GfxCirclePathHandler(path);
            result.X = x;
            result.Y = y;
            result.Width = width;
            result.Height = height;

            path.Reset();
            path.AddOval(SKRect.Create(x, y, width, height));
            path.Close();

            return result;
        }

        public override IGfxPath CreateCircleSegmentPath(int ctX, int ctY, float inRad, float extRad, float wedgeAngle, float ang1, float ang2)
        {
            var path = new SKPath();

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

            path.Reset();
            path.MoveTo(px2, py2);
            path.LineTo(px1, py1);
            if (inRad > 0) path.ArcTo(inRad, inRad, 0, SKPathArcSize.Small, SKPathDirection.Clockwise, nx1, ny1);
            path.LineTo(nx2, ny2);
            path.ArcTo(extRad, extRad, 0, SKPathArcSize.Small, SKPathDirection.CounterClockwise, px2, py2);
            path.Close();

            return result;
        }

        public override void SetTranslucent(float value)
        {
            fTranslucent = Algorithms.CheckBounds(value, 0.0f, 1.0f);
        }

        public override void ScaleTransform(float sx, float sy)
        {
            fCanvas.Scale(sx, sy);
        }

        public override void TranslateTransform(float dx, float dy)
        {
            fCanvas.Translate(dx, dy);
        }

        public override void RotateTransform(float angle)
        {
            fCanvas.RotateDegrees(angle);
        }

        public override void RestoreTransform()
        {
            fCanvas.Restore();
        }

        public override void SaveTransform()
        {
            fCanvas.Save();
        }
    }
}
