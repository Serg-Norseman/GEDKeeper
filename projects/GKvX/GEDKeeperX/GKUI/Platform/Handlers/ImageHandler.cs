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

using System.IO;
using BSLib;
using GKCore.Design.Graphics;
using SkiaSharp.Views.Forms;
using Xamarin.Forms;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class SKImageHandler : TypeHandler<SKImageImageSource>, IImage
    {
        public int Height
        {
            get { return Handle.Image.Height; }
        }

        public int Width
        {
            get { return Handle.Image.Width; }
        }

        public SKImageHandler(SKImageImageSource handle) : base(handle)
        {
        }

        public byte[] GetBytes()
        {
            return GetBytes("bmp");
        }

        public byte[] GetBytes(string format)
        {
            using (var stream = GetStream(format)) {
                return stream.ToArray();
            }
        }

        public MemoryStream GetStream(string format)
        {
            /*ImageFormat imgFormat;
            switch (format) {
                default:
                case "bmp":
                    imgFormat = ImageFormat.Bitmap;
                    break;

                case "gif":
                    imgFormat = ImageFormat.Gif;
                    break;

                case "jpeg":
                    imgFormat = ImageFormat.Jpeg;
                    break;

                case "png":
                    imgFormat = ImageFormat.Png;
                    break;

                case "tiff":
                    imgFormat = ImageFormat.Tiff;
                    break;
            }*/

            var stream = new MemoryStream();
            //((Bitmap)Handle).Save(stream, imgFormat);
            return stream;
        }

        public IImage Resize(int newWidth, int newHeight)
        {
            return this;
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class XFImageHandler : TypeHandler<ImageSource>, IImage
    {
        public int Height
        {
            get { return 0; }
        }

        public int Width
        {
            get { return 0; }
        }

        public XFImageHandler(ImageSource handle) : base(handle)
        {
        }

        public byte[] GetBytes()
        {
            return GetBytes("bmp");
        }

        public byte[] GetBytes(string format)
        {
            using (var stream = GetStream(format)) {
                return stream.ToArray();
            }
        }

        public MemoryStream GetStream(string format)
        {
            /*ImageFormat imgFormat;
            switch (format) {
                default:
                case "bmp":
                    imgFormat = ImageFormat.Bitmap;
                    break;

                case "gif":
                    imgFormat = ImageFormat.Gif;
                    break;

                case "jpeg":
                    imgFormat = ImageFormat.Jpeg;
                    break;

                case "png":
                    imgFormat = ImageFormat.Png;
                    break;

                case "tiff":
                    imgFormat = ImageFormat.Tiff;
                    break;
            }*/

            var stream = new MemoryStream();
            //((Bitmap)Handle).Save(stream, imgFormat);
            return stream;
        }

        public IImage Resize(int newWidth, int newHeight)
        {
            return this;
        }
    }
}
