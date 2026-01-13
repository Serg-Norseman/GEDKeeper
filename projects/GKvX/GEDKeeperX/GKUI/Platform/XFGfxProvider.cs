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

        public IImage LoadImage(Stream stream, int thumbWidth, int thumbHeight, ExtRect cutoutArea, bool reduce)
        {
            if (stream == null)
                throw new ArgumentNullException(nameof(stream));

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
                throw new ArgumentNullException(nameof(fileName));

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
                throw new ArgumentNullException(nameof(image));

            if (fileName == null)
                throw new ArgumentNullException(nameof(fileName));

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
