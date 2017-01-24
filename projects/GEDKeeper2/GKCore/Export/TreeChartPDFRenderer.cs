/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Drawing.Imaging;

using GKCommon;
using GKUI.Charts;
using iTextSharp.text;
using iTextSharp.text.pdf;

namespace GKCore.Export
{
    using itFont = iTextSharp.text.Font;
    using itImage = iTextSharp.text.Image;
    using sdFont = System.Drawing.Font;
    using sdImage = System.Drawing.Image;

    /// <summary>
    /// 
    /// </summary>
    public sealed class TreeChartPDFRenderer : TreeChartRenderer
    {
        private PdfContentByte fCanvas;
        private Size fPageSize;
        private Size fChartSize;
        private float fZoomFactor;

        public TreeChartPDFRenderer(bool autoScale) : base(autoScale)
        {
        }

        public override void SetTarget(object target)
        {
            PdfContentByte gfx = target as PdfContentByte;
            if (gfx == null)
                throw new ArgumentException(@"Argument's type mismatch", "target");

            fCanvas = gfx;
        }

        #region Private methods

        private BaseFont GetBaseFont(sdFont font)
        {
            string name = Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\"+font.FontFamily.Name+".ttf");
            var baseFont = BaseFont.CreateFont(name, BaseFont.IDENTITY_H, BaseFont.EMBEDDED);
            return baseFont;
        }

        private void GetFontInfo(sdFont font, out BaseFont baseFont, out float fontSize)
        {
            baseFont = GetBaseFont(font);
            fontSize = font.SizeInPoints * fZoomFactor;
        }

        private void SetPen(Pen pen)
        {
            Color color = pen.Color;
            fCanvas.SetRGBColorStroke(color.R, color.G, color.B);
            fCanvas.SetLineWidth(pen.Width);
        }

        private void SetFillColor(Color color)
        {
            fCanvas.SetColorFill(new BaseColor(color.R, color.G, color.B));
        }

        private int CheckVal(float value, bool yAxis = false, float yOffset = 0)
        {
            float newVal = value;

            // the Y-axis of this canvas starts from the bottom left corner
            if (yAxis) newVal = fPageSize.Height - newVal - yOffset;

            newVal = newVal * fZoomFactor;

            return (int)(newVal);
        }

        /*private const float PointsPerInch = 72;
        private const float PixelsPerInch = 96;

        private float pt2pix(float points)
        {
            return (points / PointsPerInch) * PixelsPerInch;
        }*/

        const float pixelToPoint = 72f / 96f;

        public static float pt2pix(float points)
        {
            return (points / pixelToPoint);
        }

        #endregion

        public void ResetFactor()
        {
            fZoomFactor = 1.0f;
        }

        public float SetSizes(Size pageSize, Size chartSize)
        {
            fPageSize = pageSize;
            fChartSize = chartSize;

            float factor = SysUtils.ZoomToFit(fChartSize.Width, fChartSize.Height, fPageSize.Width, fPageSize.Height);
            factor = (factor > 1.0f) ? 1.0f : factor;
            //fZoomFactor = factor;
            fZoomFactor = 1.0f;
            return factor;
        }

        public override void DrawImage(sdImage image, float x, float y,
                                       float width, float height)
        {
            x = CheckVal(x, false);
            y = CheckVal(y, true, height);
            width = CheckVal(width);
            height = CheckVal(height);

            var img = itImage.GetInstance(image, ImageFormat.Bmp);
            fCanvas.AddImage(img, width, 0, 0, height, x, y);
        }

        public override int GetTextHeight(string text, sdFont font)
        {
            BaseFont baseFont;
            float fontSize;
            GetFontInfo(font, out baseFont, out fontSize);

            float ascent = baseFont.GetAscentPoint(text, fontSize);
            float descent = baseFont.GetDescentPoint(text, fontSize);
            float height = (ascent - descent) * 1.66f;
            return (int)(height);
        }

        public override int GetTextWidth(string text, sdFont font)
        {
            BaseFont baseFont;
            float fontSize;
            GetFontInfo(font, out baseFont, out fontSize);

            float width = baseFont.GetWidthPoint(text, fontSize);
            return (int)(width);
        }

        public override void DrawString(string text, sdFont font, Brush brush, float x, float y)
        {
            Color color = ((SolidBrush)brush).Color;
            SetFillColor(color);

            BaseFont baseFont;
            float fontSize;
            GetFontInfo(font, out baseFont, out fontSize);

            int h = GetTextHeight(text, font);
            //int h = (int)Math.Round(baseFont.GetAscentPoint(text, fontSize));
            //int h = (int)Math.Round((baseFont.GetAscentPoint(text, fontSize) - baseFont.GetDescentPoint(text, fontSize)) * 25.4f / 72.0f);

            x = CheckVal(x, false);
            y = CheckVal(y, true, h);

            try {
                fCanvas.SetFontAndSize(baseFont, fontSize);

                fCanvas.BeginText();
                fCanvas.SetTextMatrix(x, y);
                //fCanvas.SetTextMatrix(1.0f, 0.0f, 0.0f, 1.0f, x, y);
                fCanvas.ShowText(text);
            } finally {
                fCanvas.EndText();
            }
        }

        public override void DrawLine(Pen pen, float x1, float y1, float x2, float y2)
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

        public override void DrawRectangle(Pen pen, Color fillColor, float x, float y,
                                           float width, float height)
        {
            x = CheckVal(x, false);
            y = CheckVal(y, true, height);
            width = CheckVal(width);
            height = CheckVal(height);

            fCanvas.Rectangle(x, y, width, height);

            if (pen != null && fillColor != Color.Transparent) {
                SetPen(pen);
                SetFillColor(fillColor);
                fCanvas.ClosePathFillStroke();
            } else if (pen != null) {
                SetPen(pen);
                fCanvas.ClosePathStroke();
            } else if (fillColor != Color.Transparent) {
                SetFillColor(fillColor);
                fCanvas.Fill();
            }
        }

        public override void DrawRoundedRectangle(Pen pen, Color fillColor, float x, float y,
                                                  float width, float height, float radius)
        {
            x = CheckVal(x, false);
            y = CheckVal(y, true, height);
            width = CheckVal(width);
            height = CheckVal(height);

            fCanvas.RoundRectangle(x, y, width, height, radius);

            if (pen != null && fillColor != Color.Transparent) {
                SetPen(pen);
                SetFillColor(fillColor);
                fCanvas.ClosePathFillStroke();
            } else if (pen != null) {
                SetPen(pen);
                fCanvas.ClosePathStroke();
            } else if (fillColor != Color.Transparent) {
                SetFillColor(fillColor);
                fCanvas.Fill();
            }
        }
    }
}
