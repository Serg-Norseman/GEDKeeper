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
using System.IO;
using BSLib;
using GKCore;
using GKCore.Design.Graphics;
using GKUI.Components;
using SkiaSharp;
using Xamarin.Forms;
using IImage = GKCore.Design.Graphics.IImage;

namespace GKUI.Platform
{
    /// <summary>
    /// The main implementation of the platform-specific graphics provider for Xamarin.
    /// </summary>
    public class XFGfxProvider : IGraphicsProvider
    {
        public XFGfxProvider()
        {
        }

        public void FreeImage(ref IImage image)
        {
            try {
                if (image == null) return;

                if (image is SKImageHandler) {
                    var imgHandler = image as SKImageHandler;
                    if (imgHandler == null) return;

                    var imgDisposable = imgHandler.Handle.Image as IDisposable;
                    if (imgDisposable == null) return;

                    imgDisposable.Dispose();
                }

                if (image is XFImageHandler) {
                    var imgHandler = image as XFImageHandler;
                    if (imgHandler == null) return;

                    var imgDisposable = imgHandler.Handle as IDisposable;
                    if (imgDisposable == null) return;

                    imgDisposable.Dispose();
                }

                image = null;
            } catch (Exception ex) {
                Logger.WriteError("XFGfxProvider.FreeImage()", ex);
            }
        }

        public Stream CheckOrientation(Stream inputStream)
        {
            return inputStream;
        }

        public IImage LoadImage(Stream stream, int thumbWidth, int thumbHeight, ExtRect cutoutArea, string cachedFile)
        {
            if (stream == null)
                throw new ArgumentNullException("stream");

            try {
                var img = SKImage.FromEncodedData(stream);
                return new SKImageHandler(img);
            } finally {
                stream.Close();
            }
        }

        public IImage LoadImage(string fileName)
        {
            if (fileName == null)
                throw new ArgumentNullException("fileName");

            var img = ImageSource.FromFile(fileName);
            return new XFImageHandler(img);
        }

        public IImage LoadResourceImage(Type baseType, string resName, ImageTarget target)
        {
            if (string.IsNullOrEmpty(resName))
                return null;

            if (target == ImageTarget.UI) {
                var img = ImageSource.FromResource(resName, baseType.Assembly);
                return new XFImageHandler(img);
            } else {
                using (var stream = GKUtils.LoadResourceStream(baseType, resName)) {
                    var img = SKImage.FromEncodedData(stream);
                    return new SKImageHandler(img);
                }
            }
        }

        public IImage LoadResourceImage(string resName, ImageTarget target, bool makeTransp = false)
        {
            return LoadResourceImage(typeof(GKUtils), resName, target);
        }

        public void SaveImage(IImage image, string fileName)
        {
            /*if (image == null)
                throw new ArgumentNullException("image");

            if (fileName == null)
                throw new ArgumentNullException("fileName");

            ((ImageHandler)image).Handle.Save(fileName, ImageFormat.Bmp);*/
            throw new NotImplementedException();
        }

        public IFont CreateFont(string fontName, float size, bool bold)
        {
            var skFont = new SKFont(SKTypeface.FromFamilyName(fontName), size);
            skFont.Embolden = bold;
            SKPaint pt = new SKPaint(skFont);
            return new FontHandler(pt);
        }

        public IColor CreateColor(int argb)
        {
            byte alpha, red, green, blue;
            GfxHelper.DecomposeARGB(argb, out alpha, out red, out green, out blue);

            Color color = Color.FromRgba(red, green, blue, 255);
            return new ColorHandler(color);
        }

        public IColor CreateColor(string signature)
        {
            Color color = Color.FromHex(signature);
            return new ColorHandler(color);
        }

        public ExtSizeF GetTextSize(string text, IFont font, object target)
        {
            /*Graphics gfx = target as Graphics;
            if (gfx != null && font != null) {
                Font sdFnt = ((FontHandler)font).Handle;
                var size = gfx.MeasureString(text, sdFnt);
                return new ExtSizeF(size.Width, size.Height);
            } else {
                return new ExtSizeF();
            }*/
            throw new NotImplementedException();
        }

        public string GetDefaultFontName()
        {
            string fontName;
#if __MonoCS__
            fontName = "Noto Sans";
#else
            fontName = "Verdana";
#endif
            return fontName;
        }

        public float GetDefaultFontSize()
        {
            return (float)Device.GetNamedSize(NamedSize.Default, typeof(Label));
        }
    }
}
