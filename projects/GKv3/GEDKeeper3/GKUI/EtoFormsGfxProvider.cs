/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
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
using Eto.Drawing;
using GKCommon;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    public class EtoFormsGfxProvider : IGraphicsProvider
    {
        public EtoFormsGfxProvider()
        {
        }

        public IImage LoadImage(string fileName)
        {
            if (fileName == null)
                throw new ArgumentNullException("fileName");

            using (Bitmap bmp = new Bitmap(fileName))
            {
                // cloning is necessary to release the resource
                // loaded from the image stream
                Bitmap resImage = (Bitmap)bmp.Clone();

                return new ImageHandler(resImage);
            }
        }

        public void SaveImage(IImage image, string fileName)
        {
            if (image == null)
                throw new ArgumentNullException("image");

            if (fileName == null)
                throw new ArgumentNullException("fileName");

            ((Bitmap)((ImageHandler)image).Handle).Save(fileName, ImageFormat.Bitmap);
        }

        public IImage CreateImage(Stream stream)
        {
            if (stream == null)
                throw new ArgumentNullException("stream");

            using (Bitmap bmp = new Bitmap(stream))
            {
                // cloning is necessary to release the resource
                // loaded from the image stream
                Bitmap resImage = (Bitmap)bmp.Clone();

                return new ImageHandler(resImage);
            }
        }

        public IImage CreateImage(Stream stream, int thumbWidth, int thumbHeight, ExtRect cutoutArea)
        {
            if (stream == null)
                throw new ArgumentNullException("stream");

            using (Bitmap bmp = new Bitmap(stream))
            {
                bool cutoutIsEmpty = cutoutArea.IsEmpty();
                int imgWidth = (cutoutIsEmpty) ? bmp.Width : cutoutArea.GetWidth();
                int imgHeight = (cutoutIsEmpty) ? bmp.Height : cutoutArea.GetHeight();

                if (thumbWidth > 0 && thumbHeight > 0) {
                    float ratio = SysUtils.ZoomToFit(imgWidth, imgHeight, thumbWidth, thumbHeight);
                    imgWidth = (int)(imgWidth * ratio);
                    imgHeight = (int)(imgHeight * ratio);
                }

                Bitmap newImage = new Bitmap(imgWidth, imgHeight, PixelFormat.Format24bppRgb);
                using (Graphics graphic = new Graphics(newImage)) {
                    //graphic.InterpolationMode = InterpolationMode.HighQualityBicubic;
                    graphic.ImageInterpolation = ImageInterpolation.High;
                    //graphic.SmoothingMode = SmoothingMode.HighQuality;
                    graphic.AntiAlias = true;
                    //graphic.PixelOffsetMode = PixelOffsetMode.HighQuality;
                    graphic.PixelOffsetMode = PixelOffsetMode.Half;
                    //graphic.CompositingQuality = CompositingQuality.HighQuality;

                    if (cutoutIsEmpty) {
                        graphic.DrawImage(bmp, 0, 0, imgWidth, imgHeight);
                    } else {
                        RectangleF sourRect = new RectangleF(cutoutArea.Left, cutoutArea.Top,
                                                             cutoutArea.GetWidth(), cutoutArea.GetHeight());
                        RectangleF destRect = new RectangleF(0, 0, imgWidth, imgHeight);

                        graphic.DrawImage(bmp, sourRect, destRect);
                    }
                }

                return new ImageHandler(newImage);
            }
        }

        public IImage GetResourceImage(string resName, bool makeTransp)
        {
            Bitmap img = Bitmap.FromResource("Resources." + resName);

            if (makeTransp) {
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

        public IGfxPath CreatePath()
        {
            return new GfxPathHandler(new GraphicsPath());
        }

        public IFont CreateFont(string fontName, float size, bool bold)
        {
            FontStyle style = (!bold) ? FontStyle.None : FontStyle.Bold;
            var sdFont = new Font(fontName, size, style);
            return new FontHandler(sdFont);
        }

        public IColor CreateColor(int argb)
        {
            // Dirty hack!
            //argb = (int)unchecked((long)argb & (long)((ulong)-1));
            //argb = (int)unchecked((ulong)argb & (uint)0xFF000000);
            int red = (argb >> 16) & 0xFF;
            int green = (argb >> 8) & 0xFF;
            int blue = (argb >> 0) & 0xFF;

            Color color = Color.FromArgb(red, green, blue);
            return new ColorHandler(color);
        }

        public IColor CreateColor(int r, int g, int b)
        {
            Color color = Color.FromArgb(r, g, b);
            return new ColorHandler(color);
        }

        public IColor CreateColor(string signature)
        {
            return null;
        }

        public IBrush CreateSolidBrush(IColor color)
        {
            Color sdColor = ((ColorHandler)color).Handle;

            return new BrushHandler(new SolidBrush(sdColor));
        }

        public IPen CreatePen(IColor color, float width)
        {
            Color sdColor = ((ColorHandler)color).Handle;

            return new PenHandler(new Pen(sdColor, width));
        }
    }
}
