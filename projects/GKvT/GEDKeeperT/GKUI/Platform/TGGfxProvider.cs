/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;
using BSLib;
using GKCore.Design.Graphics;
using GKUI.Platform.Handlers;
using Terminal.Gui;

namespace GKUI.Platform
{
    /// <summary>
    /// The main implementation of the platform-specific graphics provider for EtoForms.
    /// </summary>
    public class TGGfxProvider : IGraphicsProvider
    {
        public TGGfxProvider()
        {
        }

        public IImage LoadImage(string fileName)
        {
            return null;
        }

        public void SaveImage(IImage image, string fileName)
        {
        }

        public IImage CreateImage(Stream stream)
        {
            return null;
        }

        public IImage CreateImage(Stream stream, int thumbWidth, int thumbHeight, ExtRect cutoutArea)
        {
            return null;
        }

        public IImage LoadResourceImage(string resName, bool makeTransp)
        {
            return null;
        }

        public IGfxPath CreatePath()
        {
            return null;
        }

        public IGfxPath CreateCirclePath(float x, float y, float width, float height)
        {
            return null;
        }

        public IGfxPath CreateCircleSegmentPath(float inRad, float extRad, float wedgeAngle, float ang1, float ang2)
        {
            return null;
        }

        public IGfxPath CreateCircleSegmentPath(int ctX, int ctY, float inRad, float extRad, float wedgeAngle,
            float ang1, float ang2)
        {
            return null;
        }

        public IFont CreateFont(string fontName, float size, bool bold)
        {
            return new FontHandler(fontName);
        }

        public IColor CreateColor(int argb)
        {
            byte alpha, red, green, blue;
            GfxHelper.DecomposeARGB(argb, out alpha, out red, out green, out blue);

            var clr = new TrueColor(red, green, blue);
            return new ColorHandler(clr.ToConsoleColor());
        }

        public IColor CreateColor(int r, int g, int b)
        {
            var clr = new TrueColor(r, g, b);
            return new ColorHandler(clr.ToConsoleColor());
        }

        public IColor CreateColor(int a, int r, int g, int b)
        {
            var clr = new TrueColor(r, g, b);
            return new ColorHandler(clr.ToConsoleColor());
        }

        public IColor CreateColor(string signature)
        {
            return null;
        }

        public IBrush CreateSolidBrush(IColor color)
        {
            return null;
        }

        public IPen CreatePen(IColor color, float width)
        {
            return new PenHandler(color);
        }

        public ExtSizeF GetTextSize(string text, IFont font, object target)
        {
            return new ExtSizeF();
        }

        public string GetDefaultFontName()
        {
            return string.Empty;
        }

        public Stream CheckOrientation(Stream inputStream)
        {
            throw new NotImplementedException();
        }

        public void FreeImage(ref IImage image)
        {
            throw new NotImplementedException();
        }

        public IImage LoadImage(Stream stream, int thumbWidth, int thumbHeight, ExtRect cutoutArea, bool reduce)
        {
            throw new NotImplementedException();
        }

        public IImage LoadResourceImage(Type baseType, string resName, ImageTarget target)
        {
            throw new NotImplementedException();
        }

        public IImage LoadResourceImage(string resName, ImageTarget target, bool makeTransp = false)
        {
            throw new NotImplementedException();
        }

        public float GetDefaultFontSize()
        {
            return 6.0f;
        }
    }
}
