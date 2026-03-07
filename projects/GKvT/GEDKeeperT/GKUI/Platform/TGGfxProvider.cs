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

        public void FreeImage(ref IImage image)
        {
        }

        public Stream CheckOrientation(Stream inputStream)
        {
            return inputStream;
        }

        public IImage LoadImage(Stream stream, int thumbWidth, int thumbHeight, ExtRect cutoutArea, bool reduce)
        {
            return null;
        }

        public IImage LoadImage(string fileName)
        {
            return null;
        }

        public IImage LoadResourceImage(Type baseType, string resName, ImageTarget target)
        {
            return null;
        }

        public IImage LoadResourceImage(string resName, ImageTarget target, bool makeTransp = false)
        {
            return null;
        }

        public void SaveImage(IImage image, string fileName)
        {
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

        public IColor CreateColor(string signature)
        {
            return null;
        }

        public ExtSizeF GetTextSize(string text, IFont font, object target)
        {
            if (string.IsNullOrEmpty(text) || font == null)
                return ExtSizeF.Empty;

            return new ExtSizeF(text.Length, 1);
        }

        public string GetDefaultFontName()
        {
            return string.Empty;
        }

        public float GetDefaultFontSize()
        {
            return 1.0f;
        }
    }
}
