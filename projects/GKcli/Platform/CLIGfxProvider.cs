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

namespace GKUI.Platform;

public sealed class CLIGfxProvider : IGraphicsProvider
{
    public CLIGfxProvider()
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

    public IImage LoadResourceImage(string resName)
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
        // dummy
    }

    public IFont CreateFont(string fontName, float size, bool bold)
    {
        return null;
    }

    public IColor CreateColor(int argb)
    {
        return null;
    }

    public IColor CreateColor(string signature)
    {
        return null;
    }

    public ExtSizeF GetTextSize(string text, IFont font, object target)
    {
        return ExtSizeF.Empty;
    }

    public string GetDefaultFontName()
    {
        return null;
    }

    public float GetDefaultFontSize()
    {
        return 8.0f;
    }
}
