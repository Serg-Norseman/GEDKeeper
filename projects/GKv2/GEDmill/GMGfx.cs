/* 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using BSLib;
using GEDmill.HTML;
using GKCore;
using GKUI.Components;
using GKUI.Platform.Handlers;
using GKL = GKCore.Logging;

namespace GEDmill
{
    internal static class GMGfx
    {
        private static readonly GKL.ILogger fLogger = GKL.LogManager.GetLogger(GMConfig.LOG_FILE, GMConfig.LOG_LEVEL, typeof(GMGfx).Name);

        public static int SelectColor(int color)
        {
            var colorHandle = AppHost.StdDialogs.SelectColor(new ColorHandler(Color.FromArgb(color)));
            var newColor = ((ColorHandler)colorHandle).Handle.ToArgb();
            return newColor;
        }

        public static Color SelectColor(Color color)
        {
            var colorHandle = AppHost.StdDialogs.SelectColor(new ColorHandler(color));
            return ((ColorHandler)colorHandle).Handle;
        }

        // Crops the specified image file to the given size. Also converts non-standard formats to standard ones.
        // Returns sFilename in case extension has changed.
        // sArea is changed to reflect new image size
        public static string ConvertAndCropImage(string folder, string fileName, ref ExtRect rectArea, int maxWidth, int maxHeight)
        {
            fLogger.WriteInfo(string.Format("ConvertAndCropImage( {0}, {1} )", folder != null ? folder : "null", fileName != null ? fileName : "null"));

            string absFilename = string.Concat(folder, fileName);

            Image image = null;
            try {
                image = Image.FromFile(absFilename);
            } catch (OutOfMemoryException) {
                // Image is not a GDI compatible format
                image = null;
            }

            if (image == null) {
                throw (new HTMLException("Unknown image format for file " + absFilename)); // Let caller sort it out.
            }

            ExtRect rectNewArea;
            if (rectArea.Width <= 0 || rectArea.Height <= 0) {
                SizeF s = image.PhysicalDimension;
                if (s.Width <= maxWidth && s.Height <= maxHeight) {
                    maxWidth = (int)s.Width;
                    maxHeight = (int)s.Height;
                    // Nothing needs to be done, bitmap already correct size.
                    // Carry on with conversion.
                }
                rectNewArea = new ExtRect(0, 0, (int)s.Width, (int)s.Height);
                rectArea = new ExtRect(0, 0, rectNewArea.Width, rectNewArea.Height);
            } else {
                rectNewArea = new ExtRect(0, 0, rectArea.Width, rectArea.Height);
            }

            if (maxWidth != 0 && maxHeight != 0) {
                // If image is too big then shrink it. (Can't always use GetThumbnailImage because that might use embedded thumbnail).
                GMHelper.ScaleAreaToFit(ref rectNewArea, maxWidth, maxHeight);
            }

            Bitmap bitmapNew = new Bitmap(rectNewArea.Width, rectNewArea.Height, PixelFormat.Format24bppRgb);
            Graphics graphicsNew = Graphics.FromImage(bitmapNew);

            graphicsNew.DrawImage(image, UIHelper.Rt2Rt(rectNewArea), UIHelper.Rt2Rt(rectArea), GraphicsUnit.Pixel);
            image.Dispose();

            // Find which format to save in. TODO: There must be a more elegant way!!
            string extn = Path.GetExtension(fileName);
            string filepart = Path.GetDirectoryName(fileName);
            filepart += "\\" + Path.GetFileNameWithoutExtension(fileName);
            ImageFormat imageFormat;
            switch (extn.ToLower()) {
                case ".jpg":
                case ".jpeg":
                    extn = ".jpg";
                    imageFormat = ImageFormat.Jpeg;
                    break;
                case ".gif":
                    imageFormat = ImageFormat.Gif;
                    break;
                case ".bmp":
                    imageFormat = ImageFormat.Bmp;
                    break;
                case ".tif":
                case ".tiff":
                    // Tif's don't display in browsers, so convert to png.
                    imageFormat = ImageFormat.Png;
                    extn = ".png";
                    break;
                case ".exif":
                    imageFormat = ImageFormat.Exif;
                    break;
                case ".png":
                    imageFormat = ImageFormat.Png;
                    break;
                default:
                    imageFormat = ImageFormat.Jpeg;
                    break;
            }

            string filenameNew = filepart + extn;
            string absFilenameNew = string.Concat(folder, filenameNew);
            try {
                if (File.Exists(absFilename)) {
                    // Delete the old file (e.g. if converting from tif to png)
                    File.Delete(absFilename);
                }
            } catch (Exception e) {
                fLogger.WriteError(string.Format("Caught exception while removing old bitmap file {0}", absFilename), e);
            }
            try {
                if (File.Exists(absFilenameNew)) {
                    // Delete any existing file
                    File.SetAttributes(absFilenameNew, FileAttributes.Normal);
                    File.Delete(absFilenameNew);
                }
                bitmapNew.Save(absFilenameNew, imageFormat);
            } catch (Exception e) {
                fLogger.WriteError(string.Format("Caught exception while writing bitmap file {0}", filenameNew), e);
                filenameNew = "";
            }
            graphicsNew.Dispose();
            bitmapNew.Dispose();

            rectArea = rectNewArea;
            return filenameNew;
        }
    }
}
