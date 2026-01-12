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
using NSubstitute;

namespace GKTests.Stubs
{
    public class GfxProviderStub : IGraphicsProvider
    {
        public GfxProviderStub()
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
            return Substitute.For<IImage>();
        }

        public IImage LoadImage(string fileName)
        {
            return Substitute.For<IImage>();
        }

        public IImage LoadResourceImage(string resName)
        {
            return Substitute.For<IImage>();
        }

        public IImage LoadResourceImage(Type baseType, string resName, ImageTarget target)
        {
            return Substitute.For<IImage>();
        }

        public IImage LoadResourceImage(string resName, ImageTarget target, bool makeTransp)
        {
            return Substitute.For<IImage>();
        }

        public void SaveImage(IImage image, string fileName)
        {
            // dummy
        }

        public IGfxPath CreatePath()
        {
            return Substitute.For<IGfxPath>();
        }

        public IGfxPath CreateCirclePath(float x, float y, float width, float height)
        {
            return Substitute.For<IGfxPath>();
        }

        public IGfxPath CreateCircleSegmentPath(float inRad, float extRad, float wedgeAngle, float ang1, float ang2)
        {
            return Substitute.For<IGfxPath>();
        }

        public IGfxPath CreateCircleSegmentPath(int ctX, int ctY, float inRad, float extRad, float wedgeAngle,
            float ang1, float ang2)
        {
            return Substitute.For<IGfxPath>();
        }

        public IFont CreateFont(string fontName, float size, bool bold)
        {
            return Substitute.For<IFont>();
        }

        public IColor CreateColor(int argb)
        {
            return Substitute.For<IColor>();
        }

        public IColor CreateColor(int r, int g, int b)
        {
            return Substitute.For<IColor>();
        }

        public IColor CreateColor(string signature)
        {
            return Substitute.For<IColor>();
        }

        public IBrush CreateBrush(IColor color)
        {
            return Substitute.For<IBrush>();
        }

        public IPen CreatePen(IColor color, float width)
        {
            return Substitute.For<IPen>();
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
}
