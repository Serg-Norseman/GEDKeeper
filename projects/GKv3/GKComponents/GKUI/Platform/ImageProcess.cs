/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using GKCore;
using SixLabors.ImageSharp;
using SixLabors.ImageSharp.Formats.Bmp;
using SixLabors.ImageSharp.PixelFormats;
using SixLabors.ImageSharp.Processing;

namespace GKUI.Platform
{
    public static class ImageProcess
    {
        /// <summary>
        /// To avoid having to perform additional checks for bad files in GfxProvider.LoadImage(),
        /// this method should always load the stream, check the orientation, and return the prepared result.
        /// For fast loading of cached files, only BMP format is used.
        /// </summary>
        public static Stream PrepareImage(Stream inputStream)
        {
            var outputStream = new MemoryStream();
            using (var image = Image.Load<Bgr565>(inputStream)) {
                image.Mutate(x => x.AutoOrient());

                var encoder = new BmpEncoder() { BitsPerPixel = BmpBitsPerPixel.Pixel16 };
                image.SaveAsBmp(outputStream, encoder);
            }
            return outputStream;
        }

        /// <summary>
        /// Fast and lightweight extracts DPI values ​​from the BMP file header.
        /// To avoid using other classes and libraries.
        /// </summary>
        public static int GetBitmapDPI(Stream stream)
        {
            try {
                stream.Seek(0, SeekOrigin.Begin);

                var signature = new byte[2];
                if (stream.Read(signature, 0, 2) != 2 || signature[0] != 'B' || signature[1] != 'M')
                    throw new ArgumentException("The file is not a BMP file");

                stream.Seek(36, SeekOrigin.Current);

                int horizontalDpi = ReadInt32(stream);  // X pixels per meter
                int verticalDpi = ReadInt32(stream);    // Y pixels per meter

                // Convert metadata of the resolution unit to pixel per inch to match the conversion below of 1 meter = 39.3701 inches
                horizontalDpi = (int)Math.Ceiling(horizontalDpi / 39.3701);
                verticalDpi = (int)Math.Ceiling(verticalDpi / 39.3701);

                stream.Seek(0, SeekOrigin.Begin);

                return Math.Max(horizontalDpi, verticalDpi);
            } catch (Exception ex) {
                Logger.WriteError("ImageProcess.GetBitmapDPI()", ex);
                return 0;
            }
        }

        private static int ReadInt32(Stream stream)
        {
            var bytes = new byte[4];
            int readBytes = stream.Read(bytes, 0, 4);
            if (readBytes != 4)
                throw new ArgumentException("Incorrect BMP file format");
            return BitConverter.ToInt32(bytes, 0);
        }
    }
}
