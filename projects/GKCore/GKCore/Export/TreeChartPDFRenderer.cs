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
using GKCore.Charts;
using GKCore.Interfaces;
using iTextSharp.text;
using iTextSharp.text.pdf;

namespace GKCore.Export
{
    using itFont = iTextSharp.text.Font;
    using itImage = iTextSharp.text.Image;

    /// <summary>
    /// 
    /// </summary>
    public sealed class TreeChartPDFRenderer : ChartRenderer
    {
        private PdfContentByte fCanvas;
        private readonly float fPageHeight;
        private readonly float fPageWidth;

        public TreeChartPDFRenderer(float pageWidth, float pageHeight) : base()
        {
            fPageHeight = pageHeight;
            fPageWidth = pageWidth;
        }

        public override void SetTarget(object target, bool antiAlias)
        {
            PdfContentByte gfx = target as PdfContentByte;
            if (gfx == null)
                throw new ArgumentException(@"Argument's type mismatch", "target");

            fCanvas = gfx;
        }

        #region Private methods

        private static void GetFontInfo(IFont font, out BaseFont baseFont, out float fontSize)
        {
            string name = Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\" + font.FontFamilyName + ".ttf");

            baseFont = BaseFont.CreateFont(name, BaseFont.IDENTITY_H, BaseFont.EMBEDDED);
            fontSize = font.Size;
        }

        private void SetPen(IPen pen)
        {
            IColor color = pen.Color;
            fCanvas.SetRGBColorStroke(color.GetR(), color.GetG(), color.GetB());
            fCanvas.SetLineWidth(pen.Width);
        }

        private void SetFillColor(IColor color)
        {
            fCanvas.SetColorFill(new BaseColor(color.GetR(), color.GetG(), color.GetB()));
        }

        private float CheckVal(float value, bool yAxis = false, float yOffset = 0)
        {
            float newVal = value;

            // the Y-axis of this canvas starts from the bottom left corner
            if (yAxis) newVal = fPageHeight - newVal - yOffset;

            return newVal;
        }

        #endregion

        public static itImage ConvertImage(IImage image)
        {
            //var img = itImage.GetInstance(sdImage, ImageFormat.Bmp);

            byte[] bytes = image.GetBytes();
            var img = itImage.GetInstance(bytes);
            return img;
        }

        public override void DrawImage(IImage image, float x, float y,
                                       float width, float height)
        {
            x = CheckVal(x, false);
            y = CheckVal(y, true, height);
            width = CheckVal(width);
            height = CheckVal(height);

            var img = ConvertImage(image);
            fCanvas.AddImage(img, width, 0, 0, height, x, y);
        }

        public override int GetTextHeight(IFont font)
        {
            BaseFont baseFont;
            float fontSize;
            GetFontInfo(font, out baseFont, out fontSize);

            float ascent = baseFont.GetAscentPoint(STR_HEIGHT_SAMPLE, fontSize);
            float descent = baseFont.GetDescentPoint(STR_HEIGHT_SAMPLE, fontSize);
            float height = (ascent - descent) * 1.33f; // Line spacing
            return (int)(height);
        }

        public override int GetTextWidth(string text, IFont font)
        {
            BaseFont baseFont;
            float fontSize;
            GetFontInfo(font, out baseFont, out fontSize);

            float width = baseFont.GetWidthPoint(text, fontSize);
            return (int)(width);
        }

        public override ExtSizeF GetTextSize(string text, IFont font)
        {
            return new ExtSizeF(GetTextWidth(text, font), GetTextHeight(font));
        }

        public override void DrawString(string text, IFont font, IBrush brush, float x, float y)
        {
            SetFillColor(brush.Color);

            BaseFont baseFont;
            float fontSize;
            GetFontInfo(font, out baseFont, out fontSize);

            int h = GetTextHeight(font);
            x = CheckVal(x, false);
            y = CheckVal(y, true, h);

            try {
                fCanvas.SetFontAndSize(baseFont, fontSize);

                fCanvas.BeginText();
                fCanvas.SetTextMatrix(x, y);
                fCanvas.ShowText(text);
            } finally {
                fCanvas.EndText();
            }
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

            if (pen != null && !fillColor.IsTransparent()) {
                SetPen(pen);
                SetFillColor(fillColor);
                fCanvas.ClosePathFillStroke();
            } else if (pen != null) {
                SetPen(pen);
                fCanvas.ClosePathStroke();
            } else if (!fillColor.IsTransparent()) {
                SetFillColor(fillColor);
                fCanvas.Fill();
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

            if (pen != null && !fillColor.IsTransparent()) {
                SetPen(pen);
                SetFillColor(fillColor);
                fCanvas.ClosePathFillStroke();
            } else if (pen != null) {
                SetPen(pen);
                fCanvas.ClosePathStroke();
            } else if (!fillColor.IsTransparent()) {
                SetFillColor(fillColor);
                fCanvas.Fill();
            }
        }

        public override void CreateCircleSegment(IGfxPath path,
                                                 float inRad, float extRad, float wedgeAngle,
                                                 float ang1, float ang2)
        {
            
        }

        public override void CreateCircleSegment(IGfxPath path, int ctX, int ctY,
                                                 float inRad, float extRad, float wedgeAngle,
                                                 float ang1, float ang2)
        {
            
        }

        public override void FillPath(IBrush brush, IGfxPath path)
        {
            
        }

        public override void DrawPath(IPen pen, IGfxPath path)
        {
            
        }

        public override IPen CreatePen(IColor color, float width)
        {
            return AppHost.Utilities.CreatePen(color, width);
        }

        public override IBrush CreateSolidBrush(IColor color)
        {
            return AppHost.Utilities.CreateSolidBrush(color);
        }

        public override IGfxPath CreatePath()
        {
            return AppHost.Utilities.CreatePath();
        }

        public override void ResetTransform()
        {
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

        public override object SaveTransform()
        {
            return null;
        }

        public override void RestoreTransform(object matrix)
        {
        }

        public override void DrawArcText(string text, float centerX, float centerY, float radius,
                                         float startAngle, float wedgeAngle,
                                         bool inside, bool clockwise, IFont font, IBrush brush)
        {
        }
    }
}
