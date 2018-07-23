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
using System.IO;
using System.Text;

using BSLib;
using GKCore.Charts;
using GKCore.Interfaces;

namespace GKCore.Charts
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class SVGRenderer : ChartRenderer
    {
        private string fSVGFileName;
        private SvgGraphics fSVGGfx;
        private TextWriter fSVGWriter;
        private int fSVGWidth, fSVGHeight;
        private float fTranslucent;

        public override bool IsSVG { get { return true; } }

        public SVGRenderer(string svgFileName, int width, int height) : base()
        {
            fSVGFileName = svgFileName;
            fSVGWidth = width;
            fSVGHeight = height;
        }

        public override void BeginDrawing()
        {
            fSVGWriter = new StreamWriter(new FileStream(fSVGFileName, FileMode.Create), Encoding.UTF8);
            fSVGGfx = new SvgGraphics(fSVGWriter, ExtRectF.CreateBounds(0, 0, fSVGWidth, fSVGHeight));
            fSVGGfx.BeginDrawing();
        }

        public override void EndDrawing()
        {
            if (fSVGWriter != null) {
                fSVGGfx.EndDrawing();
                fSVGGfx = null;

                fSVGWriter.Flush();
                fSVGWriter.Close();
                fSVGWriter = null;
            }
        }

        public override void SetSVGMode(bool active, string svgFileName, int width, int height)
        {
            if (active) {
                BeginDrawing();
            } else {
                EndDrawing();
            }
        }

        public override void SetTarget(object target)
        {
            /*Graphics gfx = target as Graphics;
            if (gfx == null)
                throw new ArgumentException(@"Argument's type mismatch", "target");

            fCanvas = gfx;*/
        }

        public override void DrawImage(IImage image, float x, float y,
                                       float width, float height)
        {
            // dont implemented yet
        }

        public override void DrawImage(IImage image, ExtRect destinationRect, ExtRect sourceRect)
        {
            // dont implemented yet
        }

        public override int GetTextHeight(IFont font)
        {
            // dont implemented yet
            return 0;
        }

        public override int GetTextWidth(string text, IFont font)
        {
            // dont implemented yet
            return 0;
        }

        public override ExtSizeF GetTextSize(string text, IFont font)
        {
            return new ExtSizeF(GetTextWidth(text, font), GetTextHeight(font));
        }

        public override void DrawString(string text, IFont font, IBrush brush, float x, float y)
        {
            if (fSVGGfx != null) {
                fSVGGfx.SetFont(font);
                fSVGGfx.SetColor(brush.Color);
                fSVGGfx.DrawString(text, x, y);
            }
        }

        public override void DrawLine(IPen pen, float x1, float y1, float x2, float y2)
        {
            if (fSVGGfx != null) {
                fSVGGfx.SetColor(pen.Color);
                fSVGGfx.DrawLine(x1, y1, x2, y2, pen.Width);
            }
        }

        public override void DrawRectangle(IPen pen, IColor fillColor,
                                           float x, float y, float width, float height)
        {
            if (fSVGGfx != null) {
                if (fillColor != null && !fillColor.IsTransparent()) {
                    fSVGGfx.SetColor(fillColor);
                    fSVGGfx.FillRect(x, y, width, height);
                }

                if (pen != null) {
                    fSVGGfx.SetColor(pen.Color);
                    fSVGGfx.DrawRect(x, y, width, height, pen.Width);
                }
            }
        }

        public override void DrawRoundedRectangle(IPen pen, IColor fillColor, float x, float y,
                                                  float width, float height, float radius)
        {
            if (fSVGGfx != null) {
                if (fillColor != null && !fillColor.IsTransparent()) {
                    fSVGGfx.SetColor(fillColor);
                    fSVGGfx.FillRoundedRect(x, y, width, height, radius);
                }

                if (pen != null) {
                    fSVGGfx.SetColor(pen.Color);
                    fSVGGfx.DrawRoundedRect(x, y, width, height, radius, pen.Width);
                }
            }
        }

        public override void FillPath(IBrush brush, IGfxPath path)
        {
            // dummy
        }

        public override void DrawPath(IPen pen, IGfxPath path)
        {
            // dummy
        }

        public override void DrawPath(IPen pen, IBrush brush, IGfxPath path)
        {
            // dummy
        }

        public override void DrawCircle(IPen pen, IBrush brush, float x, float y,
                                        float width, float height)
        {
            if (fSVGGfx != null) {
                fSVGGfx.DrawEllipse(x, y, width, height, pen, brush);
            }
        }

        public override void DrawCircleSegment(IPen pen, IBrush brush, int ctX, int ctY,
                                               float inRad, float extRad,
                                               float startAngle, float wedgeAngle)
        {
            if (fSVGGfx != null) {
                fSVGGfx.DrawCircleSegment(ctX, ctY, inRad, extRad,
                                          startAngle, startAngle + wedgeAngle,
                                          pen, brush);
            }
        }

        public override IPen CreatePen(IColor color, float width)
        {
            // dont implemented yet
            return null;
        }

        public override IBrush CreateSolidBrush(IColor color)
        {
            // dont implemented yet
            return null;
        }

        public override IGfxPath CreatePath()
        {
            // dont implemented yet
            return null;
        }

        public override void SetTranslucent(float value)
        {
            fTranslucent = Algorithms.CheckBounds(value, 0.0f, 1.0f);
        }

        public override void ScaleTransform(float sx, float sy)
        {
            if (fSVGGfx != null) {
                fSVGGfx.Scale(sx, sy);
            }
        }

        public override void TranslateTransform(float dx, float dy)
        {
            if (fSVGGfx != null) {
                fSVGGfx.Translate(dx, dy);
            }
        }

        public override void RotateTransform(float angle)
        {
            if (fSVGGfx != null) {
                fSVGGfx.Rotate(angle);
            }
        }

        public override void ResetTransform()
        {
            if (fSVGGfx != null) {
                fSVGGfx.ResetState();
            }
        }

        public override void RestoreTransform()
        {
            if (fSVGGfx != null) {
                fSVGGfx.RestoreState();
            }
        }

        public override void SaveTransform()
        {
            if (fSVGGfx != null) {
                fSVGGfx.SaveState();
            }
        }
    }
}
