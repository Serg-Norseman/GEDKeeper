/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

using System.IO;
using BSLib;
using BSLib.Design.Graphics;
using GKCore.Interfaces;

namespace GKUI.Platform
{
    /// <summary>
    /// The main implementation of the platform-specific graphics provider for EtoForms.
    /// </summary>
    public class TGGfxProvider : IGraphicsProviderEx
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
            return null;
        }

        public IColor CreateColor(int argb)
        {
            return null;
        }

        public IColor CreateColor(int r, int g, int b)
        {
            return null;
        }

        public IColor CreateColor(int a, int r, int g, int b)
        {
            return null;
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
            return null;
        }

        public ExtSizeF GetTextSize(string text, IFont font, object target)
        {
            return new ExtSizeF();
        }

        public string GetDefaultFontName()
        {
            return string.Empty;
        }
    }
}
