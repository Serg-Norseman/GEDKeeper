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
using GKCommon;
using GKUI.Charts;
using iTextSharp.text;
using iTextSharp.text.pdf;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class TreeChartPDFRenderer : TreeChartRenderer
    {
        private PdfContentByte fCanvas;
        private Size fAreaSize;
        private Size fImageSize;
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

        public void SetSizes(Size areaSize, Size imageSize)
        {
            fAreaSize = areaSize;
            fImageSize = imageSize;

            float factor = SysUtils.ZoomToFit(fImageSize.Width, fImageSize.Height, fAreaSize.Width, fAreaSize.Height);
            fZoomFactor = (factor > 1.0f) ? 1.0f : factor;
        }

        private int CheckCoord(int value, bool isY, int yOffset = 0)
        {
            float newVal = value * fZoomFactor;
            if (isY) newVal = fAreaSize.Height - newVal - yOffset;
            return (int)newVal;
        }

        public override void DrawImage(System.Drawing.Image image, int x, int y)
        {
            
        }

        public override void DrawImage(System.Drawing.Image image, ExtRect rect)
        {
            
        }

        public override int GetTextHeight(string text, System.Drawing.Font font)
        {
            return 0;
        }

        public override int GetTextWidth(string text, System.Drawing.Font font)
        {
            return 0;
        }

        public override void DrawString(string text, System.Drawing.Font font, Brush brush, int x, int y)
        {
            Color color = ((SolidBrush)brush).Color;
            SetFillColor(color);

            x = CheckCoord(x, false);
            y = CheckCoord(y, true) /* - textHeight! */;

            try {
                fCanvas.BeginText();
                fCanvas.SetTextMatrix(x, y);
                fCanvas.ShowText(text);
            } finally {
                fCanvas.EndText();
            }
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

        public override void DrawLine(Pen pen, int x1, int y1, int x2, int y2)
        {
            if (pen == null) return;

            x1 = CheckCoord(x1, false);
            x2 = CheckCoord(x2, false);

            y1 = CheckCoord(y1, true);
            y2 = CheckCoord(y2, true);

            SetPen(pen);
            fCanvas.MoveTo(x1, y1);
            fCanvas.LineTo(x2, y2);
            fCanvas.Stroke();
        }

        public override void DrawRectangle(Pen pen, Color fillColor,
                                           int x, int y, int width, int height)
        {
            x = CheckCoord(x, false);
            y = CheckCoord(y, true, height);

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

        public override void DrawRoundedRectangle(Pen pen, Color fillColor,
                                                  int x, int y, int width, int height, int radius)
        {
            x = CheckCoord(x, false);
            y = CheckCoord(y, true, height);

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
