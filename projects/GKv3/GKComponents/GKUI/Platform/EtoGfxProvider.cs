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
using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.Design.Graphics;
using GKUI.Components;
using GKUI.Platform.Handlers;

namespace GKUI.Platform
{
    /// <summary>
    /// The main implementation of the platform-specific graphics provider for EtoForms.
    /// </summary>
    public class EtoGfxProvider : IGraphicsProvider
    {
        public EtoGfxProvider()
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
            Stream transformStream = ImageProcess.PrepareImage(inputStream);
            transformStream.Seek(0, SeekOrigin.Begin);
            return transformStream;
        }

        public IImage LoadImage(Stream stream, int thumbWidth, int thumbHeight, ExtRect cutoutArea, string cachedFile)
        {
            if (stream == null)
                throw new ArgumentNullException("stream");

            try {
                Bitmap bmp;
                try {
                    bmp = new Bitmap(stream);
                } catch {
                    try {
                        stream = ImageProcess.LoadProblemImage(stream, cachedFile);
                        bmp = new Bitmap(stream);
                    } catch (Exception ex2) {
                        Logger.WriteError("EtoGfxProvider.LoadImage()", ex2);
                        return null;
                    }
                }

                Bitmap result = null;

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

                    if (cutoutIsEmpty && thumbIsEmpty) {
                        result = bmp;
                    } else {
                        Bitmap newImage = new Bitmap(imgWidth, imgHeight, PixelFormat.Format24bppRgb);
                        using (Graphics graphic = new Graphics(newImage)) {
                            graphic.AntiAlias = true;
                            graphic.ImageInterpolation = ImageInterpolation.High;
                            graphic.PixelOffsetMode = PixelOffsetMode.Half;

                            if (cutoutIsEmpty) {
                                graphic.DrawImage(bmp, 0, 0, imgWidth, imgHeight);
                            } else {
                                var sourRect = new RectangleF(cutoutArea.Left, cutoutArea.Top, cutoutArea.Width, cutoutArea.Height);
                                var destRect = new RectangleF(0, 0, imgWidth, imgHeight);

                                graphic.DrawImage(bmp, sourRect, destRect);
                            }
                        }
                        result = newImage;
                    }
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
                throw new ArgumentNullException("fileName");

            if (!File.Exists(fileName))
                return null;

            try {
                using (var stream = new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.Read)) {
                    return new ImageHandler(new Bitmap(stream));
                }
            } catch (Exception ex) {
                Logger.WriteError(string.Format("EtoGfxProvider.LoadImage({0})", fileName), ex);
                return null;
            }
        }

        public IImage LoadResourceImage(Type baseType, string resName, ImageTarget target)
        {
            return new ImageHandler(new Bitmap(GKUtils.LoadResourceStream(baseType, resName)));
        }

        public IImage LoadResourceImage(string resName, ImageTarget target, bool makeTransp = false)
        {
            if (string.IsNullOrEmpty(resName))
                return null;

            Bitmap img = UIHelper.LoadResourceImage(resName);

            if (makeTransp) {
                // TODO
                // only for 24, 32 bit images
                /*Color pixColor = img.GetPixel(0, 0);
                if (pixColor != Colors.Transparent) {
                    img = (Bitmap)img.Clone();

                    using (Graphics gfx = new Graphics(img)) {
                        gfx.Clear(pixColor);
                    }
                }*/
            }

            return new ImageHandler(img);
        }

        public void SaveImage(IImage image, string fileName)
        {
            if (image == null)
                throw new ArgumentNullException("image");

            if (fileName == null)
                throw new ArgumentNullException("fileName");

            ((Bitmap)((ImageHandler)image).Handle).Save(fileName, ImageFormat.Bitmap);
        }

        public IFont CreateFont(string fontName, float size, bool bold)
        {
            FontStyle style = (!bold) ? FontStyle.None : FontStyle.Bold;
            var sdFont = new Font(fontName, size, style);
            return new FontHandler(sdFont);
        }

        public IColor CreateColor(int argb)
        {
            int red = (argb >> 16) & 0xFF;
            int green = (argb >> 8) & 0xFF;
            int blue = (argb >> 0) & 0xFF;

            Color color = Color.FromArgb(red, green, blue);
            return new ColorHandler(color);
        }

        public IColor CreateColor(string signature)
        {
            Color color = Color.FromArgb(SysUtils.ParseColor(signature));
            return new ColorHandler(color);
        }

        public ExtSizeF GetTextSize(string text, IFont font, object target)
        {
            if (font != null) {
                Font sdFnt = ((FontHandler)font).Handle;
                var size = sdFnt.MeasureString(text);
                return new ExtSizeF(size.Width, size.Height);
            } else {
                return ExtSizeF.Empty;
            }
        }

        public string GetDefaultFontName()
        {
            string fontName;
            if (Application.Instance.Platform.IsGtk) {
                fontName = "Sans";
            } else {
                fontName = "Verdana";
            }
            return fontName;
        }

        public float GetDefaultFontSize()
        {
            return 8.0f;
        }
    }
}
