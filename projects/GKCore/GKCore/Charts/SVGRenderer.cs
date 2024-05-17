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
using System.IO;
using System.Text;
using BSLib;
using GKCore.Design.Graphics;
using GKCore.Options;

namespace GKCore.Charts
{
    internal class FakePen : IPen
    {
        public IColor Color { get; set; }

        public float Width { get; private set; }

        public float DashOffset { get; set; }

        public FakePen(IColor color, float width)
        {
            Color = color;
            Width = width;
        }

        public void Dispose()
        {
        }
    }

    internal class FakeBrush : IBrush
    {
        public IColor Color { get; private set; }

        public FakeBrush(IColor color)
        {
            Color = color;
        }

        public void Dispose()
        {
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class SVGRenderer : ChartRenderer
    {
        private readonly string fFileName;
        private readonly int fHeight, fWidth;

        private SvgGraphics fGfx;
        private object fTarget;
        private float fTranslucent;
        private TextWriter fWriter;

        public SVGRenderer(string svgFileName, int width, int height)
        {
            fFileName = svgFileName;
            fWidth = width;
            fHeight = height;
        }

        public override void BeginDrawing()
        {
            fWriter = new StreamWriter(new FileStream(fFileName, FileMode.Create, FileAccess.Write), Encoding.UTF8);
            fGfx = new SvgGraphics(fFileName, fWriter, ExtRectF.CreateBounds(0, 0, fWidth, fHeight), GlobalOptions.Instance.TreeChartOptions.UseInlineImagesInSvg);
            fGfx.BeginDrawing();
        }

        public override void EndDrawing()
        {
            if (fWriter != null) {
                fGfx.EndDrawing();
                fGfx = null;

                fWriter.Flush();
                fWriter.Close();
                fWriter = null;
            }
        }

        public override void SetTarget(object target)
        {
            fTarget = target;
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

        public override void DrawImage(IImage image, float x, float y, float width, float height, string imName)
        {
            try {
                if (fGfx != null && image != null) {
                    fGfx.DrawImage(image, x, y, width, height, imName);
                }
            } catch (Exception ex) {
                Logger.WriteError(string.Format("SVGRenderer.DrawImage({0})", imName), ex);
            }
        }

        public override void DrawImage(IImage image, ExtRect destinationRect, ExtRect sourceRect)
        {
            // dont implemented yet
        }

        public override ExtSizeF GetTextSize(string text, IFont font)
        {
            return AppHost.GfxProvider.GetTextSize(text, font, fTarget);
        }

        public override void DrawString(string text, IFont font, IBrush brush, float x, float y)
        {
            if (fGfx != null) {
                fGfx.SetFont(font);
                fGfx.SetColor(brush.Color);
                fGfx.DrawString(text, x, y);
            }
        }

        public override void DrawLine(IPen pen, float x1, float y1, float x2, float y2)
        {
            if (fGfx != null) {
                fGfx.SetColor(pen.Color);
                fGfx.DrawLine(x1, y1, x2, y2, pen.Width);
            }
        }

        public override void DrawRectangle(IPen pen, IColor fillColor,
                                           float x, float y, float width, float height)
        {
            if (fGfx != null) {
                if (fillColor != null && !fillColor.IsTransparent()) {
                    fGfx.SetColor(fillColor);
                    fGfx.FillRect(x, y, width, height);
                }

                if (pen != null) {
                    fGfx.SetColor(pen.Color);
                    fGfx.DrawRect(x, y, width, height, pen.Width);
                }
            }
        }

        public override void FillRectangle(IBrush brush,
                                           float x, float y, float width, float height)
        {
            if (fGfx != null && brush != null) {
                // TODO
            }
        }

        public override void DrawRoundedRectangle(IPen pen, IColor fillColor, float x, float y,
                                                  float width, float height, float radius)
        {
            if (fGfx != null) {
                if (fillColor != null && !fillColor.IsTransparent()) {
                    fGfx.SetColor(fillColor);
                    fGfx.FillRoundedRect(x, y, width, height, radius);
                }

                if (pen != null) {
                    fGfx.SetColor(pen.Color);
                    fGfx.DrawRoundedRect(x, y, width, height, radius, pen.Width);
                }
            }
        }

        public override void DrawPath(IPen pen, IBrush brush, IGfxPath path)
        {
            if (fGfx != null) {
                if (path is IGfxCirclePath) {
                    var circlePath = (IGfxCirclePath)path;
                    fGfx.DrawEllipse(circlePath.X, circlePath.Y, circlePath.Width, circlePath.Height, pen, brush);
                } else if (path is IGfxCircleSegmentPath) {
                    var segmPath = (IGfxCircleSegmentPath)path;
                    fGfx.DrawCircleSegment(0, 0, segmPath.InRad, segmPath.ExtRad, segmPath.Ang1, segmPath.Ang2, pen, brush);
                }
            }
        }

        public override IGfxPath CreateCirclePath(float x, float y, float width, float height)
        {
            return null;
        }

        public override IGfxPath CreateCircleSegmentPath(int ctX, int ctY, float inRad, float extRad, float wedgeAngle, float ang1, float ang2)
        {
            return null;
        }

        public override void SetTranslucent(float value)
        {
            fTranslucent = Algorithms.CheckBounds(value, 0.0f, 1.0f);
            if (fGfx != null) {
                fGfx.SetTranslucent(fTranslucent);
            }
        }

        public override void ScaleTransform(float sx, float sy)
        {
            if (fGfx != null) {
                fGfx.Scale(sx, sy);
            }
        }

        public override void TranslateTransform(float dx, float dy)
        {
            if (fGfx != null) {
                fGfx.Translate(dx, dy);
            }
        }

        public override void RotateTransform(float angle)
        {
            if (fGfx != null) {
                fGfx.Rotate(angle);
            }
        }

        public override void RestoreTransform()
        {
            if (fGfx != null) {
                fGfx.RestoreState();
            }
        }

        public override void SaveTransform()
        {
            if (fGfx != null) {
                fGfx.SaveState();
            }
        }
    }
}
