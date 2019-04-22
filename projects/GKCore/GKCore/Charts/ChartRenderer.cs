/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
        public const int Black = 0x000000;
        public const int Blue = 0x0000FF;
        public const int CadetBlue = 0x5F9EA0;
        public const int Coral = 0xFF7F50;
        public const int DarkGray = 0xA9A9A9;
        public const int HotPink = 0xFF69B4;
        public const int Ivory = 0xFFFFF0;
        public const int Khaki = 0xF0E68C;
        public const int LawnGreen = 0x7CFC00;
        public const int LightGray = 0xDFDFDF;
        public const int Moccasin = 0xFFE4B5;
        public const int PaleGreen = 0x98FB98;
        public const int Red = 0xFF0000;
        public const int Silver = 0xC0C0C0;
        public const int Snow = 0xFFFAFA;
        public const int White = 0xFFFFFF;

        public const int ControlDark = 0xACA899;
        public const int ControlDarkDark = 0x716F64;
        public const int ControlLightLight = 0xFFFFFF;
        public const int ControlLight = 0xF1EFE2;

        // Example of string to measurement the height, where there are chars
        // with the ascent and descent of elements.
        protected internal const string STR_HEIGHT_SAMPLE = "AZqtypdfghjl|[]";


        protected ChartRenderer()
        {
        }

        public abstract void SetSmoothing(bool value);

        public virtual void BeginDrawing()
        {
        }

        public virtual void EndDrawing()
        {
        }


        public static IColor GetColor(int argb)
        {
            return AppHost.GfxProvider.CreateColor(argb);
        }

        public static IColor GetColor(int r, int g, int b)
        {
            return AppHost.GfxProvider.CreateColor(r, g, b);
        }


        public abstract void SetTarget(object target);

        public void DrawImage(IImage image, float x, float y)
        {
            DrawImage(image, x, y, image.Width, image.Height);
        }

        public abstract void DrawImage(IImage image, float x, float y,
                                       float width, float height);
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
        public abstract void DrawRoundedRectangle(IPen pen, IColor fillColor, float x, float y,
                                                  float width, float height, float radius);

        public abstract void FillPath(IBrush brush, IGfxPath path);

        public abstract void DrawPath(IPen pen, IGfxPath path);
        public abstract void DrawPath(IPen pen, IBrush brush, IGfxPath path);

        public virtual IPen CreatePen(IColor color, float width)
        {
            return AppHost.GfxProvider.CreatePen(color, width);
        }

        public IPen CreatePen(int argb, float width = 1.0f)
        {
            IColor color = GetColor(argb);
            return AppHost.GfxProvider.CreatePen(color, width);
        }

        public virtual IBrush CreateSolidBrush(IColor color)
        {
            return AppHost.GfxProvider.CreateSolidBrush(color);
        }

        public virtual IGfxPath CreatePath()
        {
            return AppHost.GfxProvider.CreatePath();
        }

        public abstract void SetTranslucent(float value);

        public abstract void ScaleTransform(float sx, float sy);
        public abstract void TranslateTransform(float dx, float dy);
        public abstract void RotateTransform(float angle);

        public abstract void ResetTransform();
        public abstract void RestoreTransform();
        public abstract void SaveTransform();

        public virtual void DrawArcText(string text, float centerX, float centerY,
                                        float radius, float startAngle, float wedgeAngle,
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
