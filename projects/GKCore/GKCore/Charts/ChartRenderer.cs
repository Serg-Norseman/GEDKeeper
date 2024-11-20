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

using System;
using BSLib;
using GKCore.Design.Graphics;

namespace GKCore.Charts
{
    public enum ChartDrawMode
    {
        dmInteractive,
        dmStatic,
        dmStaticCentered
    }

    public enum BackgroundMode
    {
        bmNone,
        bmImage,
        bmFill,
        bmAny
    }

    public enum RenderTarget
    {
        Screen,
        Printer,
        RasterFile,
        SVG
    }

    public enum TextEffect
    {
        Simple,
        Sunken,
        Raised,
        Glow,

        First = Simple,
        Last = Glow
    }

    /// <summary>
    /// 
    /// </summary>
    public abstract class ChartRenderer
    {
        // Example of string to measurement the height, where there are chars
        // with the ascent and descent of elements.
        protected internal const string STR_HEIGHT_SAMPLE = "AZqtypdfghjl|[]";


        protected ChartRenderer()
        {
        }

        public virtual void SetViewport(int width, int height)
        {
        }

        public virtual void SetSmoothing(bool value)
        {
        }

        public virtual void SetTranslucent(float value)
        {
        }

        public virtual void BeginDrawing()
        {
        }

        public virtual void EndDrawing()
        {
        }

        public abstract void SetTarget(object target);


        public static IColor GetColor(int argb)
        {
            return AppHost.GfxProvider.CreateColor(argb);
        }

        /// <summary>
        /// A separate implementation only for those cases when different classes
        /// at the UI and diagram rendering levels are responsible for the image.
        /// </summary>
        public virtual IImage LoadImage(string fileName)
        {
            return AppHost.GfxProvider.LoadImage(fileName);
        }

        /// <summary>
        /// A separate implementation only for those cases when different classes
        /// at the UI and diagram rendering levels are responsible for the image.
        /// </summary>
        public virtual IImage LoadResourceImage(Type baseType, string resName)
        {
            return AppHost.GfxProvider.LoadResourceImage(baseType, resName, ImageTarget.Chart);
        }

        /// <summary>
        /// A separate implementation only for those cases when different classes
        /// at the UI and diagram rendering levels are responsible for the image.
        /// </summary>
        public virtual IImage LoadResourceImage(string resName, bool makeTransp = false)
        {
            return AppHost.GfxProvider.LoadResourceImage(resName, ImageTarget.Chart, makeTransp);
        }

        public void DrawImage(IImage image, float x, float y, string imName)
        {
            if (image == null) return;

            DrawImage(image, x, y, image.Width, image.Height, imName);
        }

        public abstract void DrawImage(IImage image, float destX, float destY,
                                       float destWidth, float destHeight, string imName);
        public abstract void DrawImage(IImage image, ExtRect destinationRect,
                                       ExtRect sourceRect);

        public int GetTextHeight(IFont font)
        {
            return (int)GetTextSize(STR_HEIGHT_SAMPLE, font).Height;
        }

        public int GetTextWidth(string text, IFont font)
        {
            return (int)GetTextSize(text, font).Width;
        }

        public abstract ExtSizeF GetTextSize(string text, IFont font);

        public abstract void DrawString(string text, IFont font, IBrush brush, float x, float y);

        public virtual void DrawString(string text, IFont font, IBrush brush, float x, float y, TextEffect effect = TextEffect.Simple)
        {
            DrawString(text, font, brush, x, y);
        }

        public virtual void DrawAnchor(string text, string anchor, IFont font, IBrush brush, float x, float y)
        {
            // Not applicable for most areas except exports
        }

        public virtual void DrawArc(IPen pen, float x, float y, float width, float height, float startAngle, float sweepAngle)
        {
            // Not applicable for most areas
        }

        public virtual void DrawHyperlink(string text, string anchor, IFont font, IBrush brush, float x, float y)
        {
            // Not applicable for most areas except exports
        }

        public abstract void DrawLine(IPen pen, float x1, float y1, float x2, float y2);

        public abstract void DrawRectangle(IPen pen, IColor fillColor, float x, float y,
                                           float width, float height);
        public abstract void FillRectangle(IBrush brush, float x, float y, float width, float height);
        public abstract void DrawRoundedRectangle(IPen pen, IColor fillColor, float x, float y,
                                                  float width, float height, float radius);

        public virtual void DrawCoverGlass(float x, float y, float width, float height, float radius)
        {
            // dummy
        }

        public abstract void DrawPath(IPen pen, IBrush brush, IGfxPath path);

        public virtual IFont CreateFont(string fontName, float size, bool bold)
        {
            return AppHost.GfxProvider.CreateFont(fontName, size, bold);
        }

        public IPen CreatePen(int argb, float width = 1.0f)
        {
            return CreatePen(GetColor(argb), width);
        }

        public abstract IPen CreatePen(IColor color, float width, float[] dashPattern = null);

        public abstract IBrush CreateBrush(IColor color);

        public abstract IGfxPath CreateCirclePath(float x, float y, float width, float height);
        public abstract IGfxPath CreateCircleSegmentPath(int ctX, int ctY, float inRad, float extRad, float wedgeAngle, float ang1, float ang2);

        public abstract void ScaleTransform(float sx, float sy);
        public abstract void TranslateTransform(float dx, float dy);
        public abstract void RotateTransform(float angle);

        public abstract void RestoreTransform();
        public abstract void SaveTransform();

        public virtual void DrawArcText(string text, float centerX, float centerY,
                                        float radius, float startAngle, float wedgeAngle,
                                        bool inside, bool clockwise, IFont font, IBrush brush)
        {
            ExtSizeF size = GetTextSize(text, font);
            radius += size.Height / 2.0f;

            float textAngle = Math.Min((float)MathHelper.RadiansToDegrees((size.Width * 1.75f) / radius), wedgeAngle);
            float deltaAngle = (wedgeAngle - textAngle) / 2.0f;

            if (clockwise) {
                startAngle += deltaAngle;
            } else {
                startAngle += wedgeAngle - deltaAngle;
            }
            startAngle = -startAngle;

            for (int i = 0; i < text.Length; ++i) {
                float offset = (textAngle * ((float)(i) / text.Length));
                float angle = clockwise ? startAngle - offset : startAngle + offset;
                double radAngle = angle * (Math.PI / 180.0d);
                float x = (float)(centerX + Math.Cos(radAngle) * radius);
                float y = (float)(centerY - Math.Sin(radAngle) * radius);
                float charRotation = 90 - (inside ? angle : angle + 180);

                SaveTransform();
                TranslateTransform(x, y);
                RotateTransform(charRotation);
                DrawString(text.Substring(i, 1), font, brush, 0, 0);
                RestoreTransform();
            }
        }
    }
}
