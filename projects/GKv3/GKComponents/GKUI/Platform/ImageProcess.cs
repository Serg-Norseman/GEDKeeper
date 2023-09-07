/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih, Ruslan Garipov.
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
using ExifLibrary;
using SixLabors.ImageSharp;
using SixLabors.ImageSharp.Formats.Bmp;
using SixLabors.ImageSharp.PixelFormats;
using SixLabors.ImageSharp.Processing;

namespace GKUI.Platform
{
    public static class ImageProcess
    {
        public static bool IsNeedOrient(Stream inputStream)
        {
            try {
                try {
                    var file = ImageFile.FromStream(inputStream);
                    var orientProp = file.Properties.Get<ExifEnumProperty<Orientation>>(ExifTag.Orientation);
                    return (orientProp != null && orientProp.Value != Orientation.Normal);
                } finally {
                    inputStream.Seek(0, SeekOrigin.Begin);
                }
            } catch {
                return false;
            }
        }

        public static Stream AutoOrient(Stream inputStream)
        {
            var outputStream = new MemoryStream();
            using (var image = Image.Load<Bgr565>(inputStream)) {
                /*var scrSize = Screen.PrimaryScreen.Bounds.Size;
                var resizeRatio = GfxHelper.ZoomToFit(image.Width, image.Height, scrSize.Width, scrSize.Height);
                int targetWidth = (int)Math.Round(image.Width * resizeRatio);
                int targetHeight = (int)Math.Round(image.Height * resizeRatio);*/

                /*var targetDPI = Screen.PrimaryScreen.DPI;
                double currentDPI = image.Metadata.HorizontalResolution;
                double resizeRatio = targetDPI / currentDPI;
                if (resizeRatio < 0.1) { resizeRatio *= 10.0f; }
                int targetWidth = (int)Math.Round(image.Width * resizeRatio);
                int targetHeight = (int)Math.Round(image.Height * resizeRatio);*/

                image.Mutate(x => x./*Resize(targetWidth, targetHeight).*/AutoOrient());
                var encoder = new BmpEncoder() { BitsPerPixel = BmpBitsPerPixel.Pixel16 };
                image.SaveAsBmp(outputStream, encoder);
            }
            return outputStream;
        }
    }
}
