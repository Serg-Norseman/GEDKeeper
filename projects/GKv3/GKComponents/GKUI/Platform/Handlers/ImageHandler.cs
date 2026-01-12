/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.IO;
using BSLib;
using Eto.Drawing;
using GKCore.Design.Graphics;

namespace GKUI.Platform.Handlers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ImageHandler : TypeHandler<Image>, IImage
    {
        public int Height
        {
            get { return Handle.Height; }
        }

        public int Width
        {
            get { return Handle.Width; }
        }

        public ImageHandler(Image handle) : base(handle)
        {
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (!Handle.IsDisposed) Handle.Dispose();
            }
            base.Dispose(disposing);
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
            ImageFormat imgFormat;
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
            }

            var stream = new MemoryStream();
            ((Bitmap)Handle).Save(stream, imgFormat);
            return stream;
        }

        public IImage Resize(int newWidth, int newHeight)
        {
            var original = Handle as Bitmap;

            int imgWidth = original.Width;
            int imgHeight = original.Height;

            if (newWidth != imgWidth || newHeight != imgHeight) {
                float ratio = GfxHelper.ZoomToFit(imgWidth, imgHeight, newWidth, newHeight);
                imgWidth = (int)(imgWidth * ratio);
                imgHeight = (int)(imgHeight * ratio);

                Bitmap newImage = new Bitmap(imgWidth, imgHeight, PixelFormat.Format24bppRgb);
                using (var graphic = new Graphics(newImage)) {
                    graphic.AntiAlias = true;
                    graphic.ImageInterpolation = ImageInterpolation.High;
                    graphic.PixelOffsetMode = PixelOffsetMode.Half;
                    graphic.DrawImage(original, 0, 0, imgWidth, imgHeight);
                }

                return new ImageHandler(newImage);
            } else {
                return this;
            }
        }
    }
}
