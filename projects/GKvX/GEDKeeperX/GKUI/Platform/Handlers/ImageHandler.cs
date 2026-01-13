/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
