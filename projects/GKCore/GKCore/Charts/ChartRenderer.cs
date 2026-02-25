/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
        public const string STR_HEIGHT_SAMPLE = "AZqtypdfghjl|[]";


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

        /// <summary>
        /// Not applicable for terminal implementations.
        /// </summary>
        public virtual void DrawImage(IImage image, float destX, float destY, float destWidth, float destHeight, string imName)
        {
        }

        /// <summary>
        /// Not applicable for terminal implementations.
        /// </summary>
        public virtual void DrawImage(IImage image, ExtRect destinationRect, ExtRect sourceRect)
        {
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

        /// <summary>
        /// Not applicable for most areas except exports.
        /// </summary>
        public virtual void DrawAnchor(string text, string anchor, IFont font, IBrush brush, float x, float y)
        {
        }

        /// <summary>
        /// Not applicable for most areas.
        /// </summary>
        public virtual void DrawArc(IPen pen, float x, float y, float width, float height, float startAngle, float sweepAngle)
        {
        }

        /// <summary>
        /// Not applicable for most areas except exports.
        /// </summary>
        public virtual void DrawHyperlink(string text, string anchor, IFont font, IBrush brush, float x, float y)
        {
        }

        public abstract void DrawLine(IPen pen, float x1, float y1, float x2, float y2);

        public abstract void DrawRectangle(IPen pen, IColor fillColor,
            float x, float y, float width, float height, int cornersRadius = 0);

        public abstract void FillRectangle(IBrush brush, float x, float y, float width, float height);

        public virtual void DrawCoverGlass(float x, float y, float width, float height, float radius)
        {
            // dummy
        }

        /// <summary>
        /// Not applicable for terminal implementations.
        /// </summary>
        public virtual void DrawPath(IPen pen, IBrush brush, IGfxPath path)
        {
        }

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

        /// <summary>
        /// Not applicable for terminal implementations.
        /// </summary>
        public virtual IGfxPath CreateCirclePath(float x, float y, float width, float height)
        {
            return null;
        }

        /// <summary>
        /// Not applicable for terminal implementations.
        /// </summary>
        public virtual IGfxPath CreateCircleSegmentPath(int ctX, int ctY, float inRad, float extRad, float wedgeAngle, float ang1, float ang2)
        {
            return null;
        }

        /// <summary>
        /// Not applicable for terminal implementations.
        /// </summary>
        public virtual void ScaleTransform(float sx, float sy)
        {
        }

        /// <summary>
        /// Not applicable for terminal implementations.
        /// </summary>
        public virtual void TranslateTransform(float dx, float dy)
        {
        }

        /// <summary>
        /// Not applicable for terminal implementations.
        /// </summary>
        public virtual void RotateTransform(float angle)
        {
        }

        /// <summary>
        /// Not applicable for terminal implementations.
        /// </summary>
        public virtual void RestoreTransform()
        {
        }

        /// <summary>
        /// Not applicable for terminal implementations.
        /// </summary>
        public virtual void SaveTransform()
        {
        }

        /// <summary>
        /// Not applicable for terminal implementations.
        /// </summary>
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
