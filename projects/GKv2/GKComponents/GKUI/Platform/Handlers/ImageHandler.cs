/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.IO;
using BSLib;
using GKCore.Design.Graphics;

namespace GKUI.Platform.Handlers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ImageHandler: TypeHandler<Image>, IImage
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
                Handle.Dispose();
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
                    imgFormat = ImageFormat.Bmp;
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
            // for fix bug: "A generic error occurred in GDI+"
            using (var newBitmap = new Bitmap(Handle)) {
                newBitmap.Save(stream, imgFormat);
            }
            //Handle.Save(stream, imgFormat);
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
                using (var graphic = System.Drawing.Graphics.FromImage(newImage)) {
                    graphic.InterpolationMode = InterpolationMode.HighQualityBicubic;
                    graphic.SmoothingMode = SmoothingMode.HighQuality;
                    graphic.PixelOffsetMode = PixelOffsetMode.HighQuality;
                    graphic.CompositingQuality = CompositingQuality.HighQuality;
                    graphic.DrawImage(original, 0, 0, imgWidth, imgHeight);
                }

                return new ImageHandler(newImage);
            } else {
                return this;
            }
        }
    }
}
