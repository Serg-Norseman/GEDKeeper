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
using System.Drawing.Imaging;

using GKCore.Charts;
using GKCore.Interfaces;
using GKUI.Components;
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

        public override void SetTarget(object target)
        {
            PdfContentByte gfx = target as PdfContentByte;
            if (gfx == null)
                throw new ArgumentException(@"Argument's type mismatch", "target");

            fCanvas = gfx;
        }

        #region Private methods

        private static void GetFontInfo(sdFont font, out BaseFont baseFont, out float fontSize)
        {
            string name = Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\"+font.FontFamily.Name+".ttf");

            baseFont = BaseFont.CreateFont(name, BaseFont.IDENTITY_H, BaseFont.EMBEDDED);
            fontSize = font.SizeInPoints;
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

        private float CheckVal(float value, bool yAxis = false, float yOffset = 0)
        {
            float newVal = value;

            // the Y-axis of this canvas starts from the bottom left corner
            if (yAxis) newVal = fPageHeight - newVal - yOffset;

            return newVal;
        }

        #endregion

        public override void DrawImage(IImage image, float x, float y,
                                       float width, float height)
        {
            var sdImage = ((ImageHandler)image).Handle;
            DrawImage(sdImage, x, y, width, height);
        }

        private void DrawImage(sdImage image, float x, float y,
                               float width, float height)
        {
            x = CheckVal(x, false);
            y = CheckVal(y, true, height);
            width = CheckVal(width);
            height = CheckVal(height);

            var img = itImage.GetInstance(image, ImageFormat.Bmp);
            fCanvas.AddImage(img, width, 0, 0, height, x, y);
        }

        public override int GetTextHeight(IFont font)
        {
            sdFont sdFnt = ((FontHandler)font).Handle;

            BaseFont baseFont;
            float fontSize;
            GetFontInfo(sdFnt, out baseFont, out fontSize);

            float ascent = baseFont.GetAscentPoint(STR_HEIGHT_SAMPLE, fontSize);
            float descent = baseFont.GetDescentPoint(STR_HEIGHT_SAMPLE, fontSize);
            float height = (ascent - descent) * 1.33f; // Line spacing
            return (int)(height);
        }

        public override int GetTextWidth(string text, IFont font)
        {
            sdFont sdFnt = ((FontHandler)font).Handle;

            BaseFont baseFont;
            float fontSize;
            GetFontInfo(sdFnt, out baseFont, out fontSize);

            float width = baseFont.GetWidthPoint(text, fontSize);
            return (int)(width);
        }

        public override ExtSizeF GetTextSize(string text, IFont font)
        {
            return new ExtSizeF(GetTextWidth(text, font), GetTextHeight(font));
        }

        public override void DrawString(string text, IFont font, IBrush brush, float x, float y)
        {
            Brush sdBrush = ((BrushHandler)brush).Handle;
            sdFont sdFnt = ((FontHandler)font).Handle;

            Color color = ((SolidBrush)sdBrush).Color;
            SetFillColor(color);

            BaseFont baseFont;
            float fontSize;
            GetFontInfo(sdFnt, out baseFont, out fontSize);

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

            Pen sdPen = ((PenHandler)pen).Handle;

            x1 = CheckVal(x1, false);
            y1 = CheckVal(y1, true);

            x2 = CheckVal(x2, false);
            y2 = CheckVal(y2, true);

            SetPen(sdPen);
            fCanvas.MoveTo(x1, y1);
            fCanvas.LineTo(x2, y2);
            fCanvas.Stroke();
        }

        public override void DrawRectangle(IPen pen, IColor fillColor, float x, float y,
                                           float width, float height)
        {
            Color sdFillColor = ((ColorHandler)fillColor).Handle;

            x = CheckVal(x, false);
            y = CheckVal(y, true, height);
            width = CheckVal(width);
            height = CheckVal(height);

            fCanvas.Rectangle(x, y, width, height);

            if (pen != null && sdFillColor != Color.Transparent) {
                Pen sdPen = ((PenHandler)pen).Handle;

                SetPen(sdPen);
                SetFillColor(sdFillColor);
                fCanvas.ClosePathFillStroke();
            } else if (pen != null) {
                Pen sdPen = ((PenHandler)pen).Handle;

                SetPen(sdPen);
                fCanvas.ClosePathStroke();
            } else if (sdFillColor != Color.Transparent) {
                SetFillColor(sdFillColor);
                fCanvas.Fill();
            }
        }

        public override void DrawRoundedRectangle(IPen pen, IColor fillColor, float x, float y,
                                                  float width, float height, float radius)
        {
            Color sdFillColor = ((ColorHandler)fillColor).Handle;

            x = CheckVal(x, false);
            y = CheckVal(y, true, height);
            width = CheckVal(width);
            height = CheckVal(height);

            fCanvas.RoundRectangle(x, y, width, height, radius);

            if (pen != null && sdFillColor != Color.Transparent) {
                Pen sdPen = ((PenHandler)pen).Handle;

                SetPen(sdPen);
                SetFillColor(sdFillColor);
                fCanvas.ClosePathFillStroke();
            } else if (pen != null) {
                Pen sdPen = ((PenHandler)pen).Handle;

                SetPen(sdPen);
                fCanvas.ClosePathStroke();
            } else if (sdFillColor != Color.Transparent) {
                SetFillColor(sdFillColor);
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
    }
}
