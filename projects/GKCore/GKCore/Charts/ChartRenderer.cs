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
using BSLib;
using GKCore.Interfaces;

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

    /// <summary>
    /// 
    /// </summary>
    public abstract class ChartRenderer
    {
        public const int White = 0xFFFFFF;
        public const int Black = 0x000000;
        public const int Silver = 0xC0C0C0;
        public const int Blue = 0x0000FF;
        public const int Red = 0xFF0000;

        public const int Coral = 0xFF7F50;
        public const int CadetBlue = 0x5F9EA0;
        public const int DarkGray = 0xA9A9A9;
        public const int Khaki = 0xF0E68C;
        public const int LawnGreen = 0x7CFC00;
        public const int HotPink = 0xFF69B4;
        public const int Ivory = 0xFFFFF0;
        public const int Moccasin = 0xFFE4B5;
        public const int PaleGreen = 0x98FB98;


        // Example of string to measurement the height, where there are chars
        // with the ascent and descent of elements.
        protected const string STR_HEIGHT_SAMPLE = "AZqtypdfghjl|[]";


        protected ChartRenderer()
        {
        }

        public virtual void SetSVGMode(bool active, string svgFileName, int width, int height)
        {
            // dummy
        }

        public static IColor GetColor(int argb)
        {
            return AppHost.GfxProvider.CreateColor(argb);
        }

        public static IColor GetColor(int r, int g, int b)
        {
            return AppHost.GfxProvider.CreateColor(r, g, b);
        }

        public abstract void SetTarget(object target, bool antiAlias);

        public void DrawImage(IImage image, float x, float y)
        {
            DrawImage(image, x, y, image.Width, image.Height);
        }

        public abstract void DrawImage(IImage image, float x, float y,
                                       float width, float height);

        public abstract int GetTextHeight(IFont font);
        public abstract int GetTextWidth(string text, IFont font);
        public abstract ExtSizeF GetTextSize(string text, IFont font);

        public abstract void DrawString(string text, IFont font, IBrush brush, float x, float y);

        public abstract void DrawLine(IPen pen, float x1, float y1, float x2, float y2);

        public abstract void DrawRectangle(IPen pen, IColor fillColor, float x, float y,
                                           float width, float height);
        public abstract void DrawRoundedRectangle(IPen pen, IColor fillColor, float x, float y,
                                                  float width, float height, float radius);

        public abstract void FillPath(IBrush brush, IGfxPath path);

        public abstract void DrawPath(IPen pen, IGfxPath path);

        public abstract void CreateCircleSegment(IGfxPath path,
                                                 float inRad, float extRad, float wedgeAngle,
                                                 float ang1, float ang2);
        public abstract void CreateCircleSegment(IGfxPath path, int ctX, int ctY,
                                                 float inRad, float extRad, float wedgeAngle,
                                                 float ang1, float ang2);

        public abstract IPen CreatePen(IColor color, float width);

        public abstract IBrush CreateSolidBrush(IColor color);

        public abstract IGfxPath CreatePath();

        public abstract void SetTranslucent(float value);

        public abstract void ScaleTransform(float sx, float sy);
        public abstract void TranslateTransform(float dx, float dy);
        public abstract void RotateTransform(float angle);

        public abstract void ResetTransform();
        public abstract void RestoreTransform();
        public abstract void SaveTransform();

        public void DrawArcText(string text, float centerX, float centerY, float radius,
                                float startAngle, float wedgeAngle,
                                bool inside, bool clockwise, IFont font, IBrush brush)
        {
            ExtSizeF size = GetTextSize(text, font);
            radius = radius + size.Height / 2.0f;

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
