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
using Eto.Forms;
using ExifLibrary;
using SixLabors.ImageSharp;
using SixLabors.ImageSharp.Formats.Bmp;
using SixLabors.ImageSharp.PixelFormats;
using SixLabors.ImageSharp.Processing;

namespace GKUI.Platform
{
    public static class ImageProcess
    {
        public static Stream PrepareImage(Stream inputStream)
        {
            bool isNeedOrient;
            bool changeDPI = false;

            try {
                try {
                    var file = ImageFile.FromStream(inputStream);
                    var orientProp = file.Properties.Get<ExifEnumProperty<ExifLibrary.Orientation>>(ExifTag.Orientation);
                    isNeedOrient = (orientProp != null && orientProp.Value != ExifLibrary.Orientation.Normal);

                    var resX = file.Properties.Get<ExifURational>(ExifTag.XResolution);
                    var resY = file.Properties.Get<ExifURational>(ExifTag.YResolution);
                    if (resX != null && resY != null) {
                        changeDPI = ((resX != null && (float)resX >= 100.0f) || (resY != null && (float)resY >= 100.0f));
                    } else {
                        var densX = file.Properties.Get<ExifUShort>(ExifTag.XDensity);
                        var densY = file.Properties.Get<ExifUShort>(ExifTag.YDensity);
                        if (densX != null && densY != null) {
                            changeDPI = ((densX != null && (float)densX >= 100.0f) || (densY != null && (float)densY >= 100.0f));
                        }
                    }
                } finally {
                    inputStream.Seek(0, SeekOrigin.Begin);
                }
            } catch {
                isNeedOrient = false;
                changeDPI = false;
            }

            if (!isNeedOrient && !changeDPI) {
                return inputStream;
            } else {
                var outputStream = new MemoryStream();
                using (var image = Image.Load<Bgr565>(inputStream)) {
                    if (changeDPI) {
                        var targetDPI = Screen.PrimaryScreen.DPI;

                        /*double currentDPI = image.Metadata.HorizontalResolution;
                        double resizeRatio = targetDPI / currentDPI;
                        if (resizeRatio < 0.1) { resizeRatio *= 10.0f; }
                        int targetWidth = (int)Math.Round(image.Width * resizeRatio);
                        int targetHeight = (int)Math.Round(image.Height * resizeRatio);
                        image.Mutate(x => x.Resize(targetWidth, targetHeight));*/

                        image.Metadata.HorizontalResolution = targetDPI;
                        image.Metadata.VerticalResolution = targetDPI;
                    }

                    if (isNeedOrient) {
                        image.Mutate(x => x.AutoOrient());
                    }

                    var encoder = new BmpEncoder() { BitsPerPixel = BmpBitsPerPixel.Pixel16 };
                    image.SaveAsBmp(outputStream, encoder);
                }
                return outputStream;
            }
        }

        public static Stream LoadProblemImage(Stream inputStream, string outputFileName)
        {
            var outputStream = new FileStream(outputFileName, FileMode.Create);
            using (var image = Image.Load<Bgr565>(inputStream)) {
                image.Mutate(x => x.AutoOrient());
                var encoder = new BmpEncoder() { BitsPerPixel = BmpBitsPerPixel.Pixel16 };
                image.SaveAsBmp(outputStream, encoder);

                inputStream.Close();

                outputStream.Seek(0, SeekOrigin.Begin);
            }
            return outputStream;
        }
    }
}
