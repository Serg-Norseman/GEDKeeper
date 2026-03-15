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
using GKUI.Platform.Handlers;
using SixLabors.ImageSharp;
using SixLabors.ImageSharp.PixelFormats;
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
            try {
                if (image == null) return;

                var imgHandler = image as ImageHandler;
                if (imgHandler == null) return;

                var imgDisposable = imgHandler.Handle as IDisposable;
                if (imgDisposable == null) return;

                imgDisposable.Dispose();

                image = null;
            } catch (Exception ex) {
                Logger.WriteError("EtoGfxProvider.FreeImage()", ex);
            }
        }

        public Stream CheckOrientation(Stream inputStream)
        {
            inputStream.Seek(0, SeekOrigin.Begin);
            Stream transformStream = ImageProcess.PrepareImage(inputStream);
            transformStream.Seek(0, SeekOrigin.Begin);
            return transformStream;
        }

        public IImage LoadImage(Stream stream, int thumbWidth, int thumbHeight, ExtRect cutoutArea, bool reduce)
        {
            if (stream == null)
                throw new ArgumentNullException(nameof(stream));

            try {
                var bmp = Image.Load<Rgba32>(stream);
                Image<Rgba32> result = null;

                try {
                    int imgWidth, imgHeight;

                    bool cutoutIsEmpty = cutoutArea.IsEmpty();
                    if (cutoutIsEmpty) {
                        imgWidth = bmp.Width;
                        imgHeight = bmp.Height;
                    } else {
                        imgWidth = cutoutArea.Width;
                        imgHeight = cutoutArea.Height;
                    }

                    bool thumbIsEmpty = (thumbWidth <= 0 && thumbHeight <= 0);
                    if (!thumbIsEmpty) {
                        float ratio = GfxHelper.ZoomToFit(imgWidth, imgHeight, thumbWidth, thumbHeight);
                        imgWidth = (int)(imgWidth * ratio);
                        imgHeight = (int)(imgHeight * ratio);
                    }

                    // The image will always have to be copied here so that the output cached file is at the screen DPI,
                    // but the original file remains unchanged (to avoid distortion of the coordinates of the cut-out areas
                    // that were stored in GEDCOM relative to the original full image).
                    /*Bitmap newImage = new Bitmap(imgWidth, imgHeight, PixelFormat.Format24bppRgb);
                    using (Graphics graphic = new Graphics(newImage)) {
                        // Wpf: Low and High(=Default) - very bad quality!
                        // Gtk: Medium and High - no differences.
                        graphic.ImageInterpolation = ImageInterpolation.Medium;

                        if (cutoutIsEmpty) {
                            graphic.DrawImage(bmp, 0, 0, imgWidth, imgHeight);
                        } else {
                            var sourRect = new RectangleF(cutoutArea.Left, cutoutArea.Top, cutoutArea.Width, cutoutArea.Height);
                            var destRect = new RectangleF(0, 0, imgWidth, imgHeight);

                            graphic.DrawImage(bmp, sourRect, destRect);
                        }
                    }
                    result = newImage;*/

                    result = bmp;
                } finally {
                    if (result != bmp)
                        bmp.Dispose();
                }

                return new ImageHandler(result);
            } finally {
                stream.Close();
            }
        }

        public IImage LoadImage(string fileName)
        {
            if (string.IsNullOrEmpty(fileName))
                throw new ArgumentNullException(nameof(fileName));

            if (!File.Exists(fileName))
                return null;

            try {
                using (var stream = new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)) {
                    return new ImageHandler(Image.Load<Rgba32>(stream));
                }
            } catch (Exception ex) {
                Logger.WriteError(string.Format("EtoGfxProvider.LoadImage({0})", fileName), ex);
                return null;
            }
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
            return new ColorHandler(ColorMapper.GetClosestConsoleColor(clr));
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
