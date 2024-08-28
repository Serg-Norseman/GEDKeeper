/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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

#if NETCORE
#define UNOFF_ITS
#endif

using System;
using BSLib;
using GKCore.Charts;
using GKCore.Design.Graphics;
using iTextSharp.text;
using iTextSharp.text.pdf;

namespace GKCore.Export
{
    using itFont = iTextSharp.text.Font;
    using itImage = iTextSharp.text.Image;

    /// <summary>
    /// 
    /// </summary>
    public sealed class PDFRenderer : ChartRenderer
    {
        private PdfContentByte fCanvas;
        private float fPageHeight;
        private float fPageWidth;

        public PDFRenderer(float pageWidth, float pageHeight)
        {
            fPageHeight = pageHeight;
            fPageWidth = pageWidth;
        }

        public void SetPageSize(ExtSize size)
        {
            fPageHeight = size.Height;
            fPageWidth = size.Width;
        }

        public override void SetTarget(object target)
        {
            PdfContentByte gfx = target as PdfContentByte;
            if (gfx == null)
                throw new ArgumentNullException("target");

            fCanvas = gfx;
        }

        public override void SetSmoothing(bool value)
        {
            // dummy
        }

        public override IPen CreatePen(IColor color, float width, float[] dashPattern = null)
        {
            return new FakePen(color, width);
        }

        public override IBrush CreateBrush(IColor color)
        {
            return new FakeBrush(color);
        }

        #region Private methods

        private static BaseFont GetBaseFont(IFont font)
        {
            if (font is PDFWriter.FontHandler) {
                return ((PDFWriter.FontHandler)font).BaseFont;
            }

            string name = Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\" + font.FontFamilyName + ".ttf");
            BaseFont baseFont = BaseFont.CreateFont(name, BaseFont.IDENTITY_H, BaseFont.EMBEDDED);
            return baseFont;
        }

        private void SetPen(IPen pen)
        {
            if (pen != null) {
                IColor color = pen.Color;

#if !UNOFF_ITS
                fCanvas.SetRGBColorStroke(color.GetR(), color.GetG(), color.GetB());
#else
                fCanvas.SetRgbColorStroke(color.GetR(), color.GetG(), color.GetB());
#endif

                fCanvas.SetLineWidth(pen.Width);
            }
        }

        private void SetFillColor(IColor color)
        {
            if (color != null) {
                fCanvas.SetColorFill(new BaseColor(color.GetR(), color.GetG(), color.GetB()));
            }
        }

        private float CheckVal(float value, bool yAxis = false, float yOffset = 0)
        {
            float newVal = value;

            // the Y-axis of this canvas starts from the bottom left corner
            if (yAxis) newVal = fPageHeight - newVal - yOffset;

            return newVal;
        }

        #endregion

        public static itImage ConvertImage(IImage image, string imName)
        {
            try {
                byte[] bytes = image.GetBytes("png");
                return itImage.GetInstance(bytes);
            } catch (Exception ex) {
                Logger.WriteError(string.Format("PDFRenderer.ConvertImage({0})", imName), ex);
                return null;
            }
        }

        public override void DrawImage(IImage image, float x, float y,
                                       float width, float height, string imName)
        {
            try {
                if (fCanvas == null || image == null) return;

                x = CheckVal(x, false);
                y = CheckVal(y, true, height);
                width = CheckVal(width);
                height = CheckVal(height);

                var img = ConvertImage(image, imName);
                if (img == null) return;

                fCanvas.AddImage(img, width, 0, 0, height, x, y);
            } catch (Exception ex) {
                Logger.WriteError(string.Format("PDFRenderer.DrawImage({0})", imName), ex);
            }
        }

        public override void DrawImage(IImage image, ExtRect destinationRect, ExtRect sourceRect)
        {
            // dont implemented yet
        }

        public override ExtSizeF GetTextSize(string text, IFont font)
        {
            BaseFont baseFont = GetBaseFont(font);
            var width = PDFWriter.FontHandler.GetTextWidth(text, baseFont, font.Size);
            var height = PDFWriter.FontHandler.GetTextHeight(baseFont, font.Size);
            return new ExtSizeF(width, height);
        }

        public override void DrawString(string text, IFont font, IBrush brush, float x, float y)
        {
            if (brush != null) {
                SetFillColor(brush.Color);
            }

            int h = GetTextHeight(font);
            x = CheckVal(x, false);
            y = CheckVal(y, true, h);

            try {
                BaseFont baseFont = GetBaseFont(font);
                fCanvas.SetFontAndSize(baseFont, font.Size);

                fCanvas.BeginText();
                fCanvas.SetTextMatrix(x, y);
                fCanvas.ShowText(text);
            } finally {
                fCanvas.EndText();
            }
        }

        public override void DrawAnchor(string text, string anchor, IFont font, IBrush brush, float x, float y)
        {
            if (brush != null) {
                SetFillColor(brush.Color);
            }

            int h = GetTextHeight(font);
            int w = GetTextWidth(text, font);

            // FIXME: temp hack
            h *= 2;
            w *= 4;

            x = CheckVal(x, false);
            y = CheckVal(y, true, h);

            var ct = new ColumnText(fCanvas);
            ct.SetSimpleColumn(x, y, x + w, y + h);
            ct.AddElement(new Chunk(text, ((PDFWriter.FontHandler)font).Handle).SetLocalDestination(anchor));
            ct.Go();
        }

        public override void DrawHyperlink(string text, string anchor, IFont font, IBrush brush, float x, float y)
        {
            if (brush != null) {
                SetFillColor(brush.Color);
            }

            int h = GetTextHeight(font);
            int w = GetTextWidth(text, font);

            // FIXME: temp hack
            h *= 2;
            w *= 4;

            x = CheckVal(x, false);
            y = CheckVal(y, true, h);

            var ct = new ColumnText(fCanvas);
            ct.SetSimpleColumn(x, y, x + w, y + h);
            ct.AddElement(new Chunk(text, ((PDFWriter.FontHandler)font).Handle).SetLocalGoto(anchor));
            ct.Go();
        }

        public override void DrawLine(IPen pen, float x1, float y1, float x2, float y2)
        {
            if (pen == null) return;

            x1 = CheckVal(x1, false);
            y1 = CheckVal(y1, true);

            x2 = CheckVal(x2, false);
            y2 = CheckVal(y2, true);

            SetPen(pen);
            fCanvas.MoveTo(x1, y1);
            fCanvas.LineTo(x2, y2);
            fCanvas.Stroke();
        }

        public override void DrawRectangle(IPen pen, IColor fillColor, float x, float y,
                                           float width, float height)
        {
            x = CheckVal(x, false);
            y = CheckVal(y, true, height);
            width = CheckVal(width);
            height = CheckVal(height);

            fCanvas.Rectangle(x, y, width, height);

            if (pen != null && (fillColor != null && !fillColor.IsTransparent())) {
                SetPen(pen);
                SetFillColor(fillColor);
                fCanvas.ClosePathFillStroke();
            } else if (pen != null) {
                SetPen(pen);
                fCanvas.ClosePathStroke();
            } else if (fillColor != null && !fillColor.IsTransparent()) {
                SetFillColor(fillColor);
                fCanvas.Fill();
            }
        }

        public override void FillRectangle(IBrush brush,
                                           float x, float y, float width, float height)
        {
            if (brush != null) {
                // TODO
            }
        }

        public override void DrawRoundedRectangle(IPen pen, IColor fillColor, float x, float y,
                                                  float width, float height, float radius)
        {
            x = CheckVal(x, false);
            y = CheckVal(y, true, height);
            width = CheckVal(width);
            height = CheckVal(height);

            fCanvas.RoundRectangle(x, y, width, height, radius);

            if (pen != null && (fillColor != null && !fillColor.IsTransparent())) {
                SetPen(pen);
                SetFillColor(fillColor);
                fCanvas.ClosePathFillStroke();
            } else if (pen != null) {
                SetPen(pen);
                fCanvas.ClosePathStroke();
            } else if (fillColor != null && !fillColor.IsTransparent()) {
                SetFillColor(fillColor);
                fCanvas.Fill();
            }
        }

        public override void DrawPath(IPen pen, IBrush brush, IGfxPath path)
        {
        }

        public override IGfxPath CreateCirclePath(float x, float y, float width, float height)
        {
            return null;
        }

        public override IGfxPath CreateCircleSegmentPath(int ctX, int ctY, float inRad, float extRad, float wedgeAngle, float ang1, float ang2)
        {
            return null;
        }

        public override void ScaleTransform(float sx, float sy)
        {
        }

        public override void TranslateTransform(float dx, float dy)
        {
        }

        public override void RotateTransform(float angle)
        {
        }

        public override void RestoreTransform()
        {
        }

        public override void SaveTransform()
        {
        }

        public override void SetTranslucent(float value)
        {
        }
    }
}
