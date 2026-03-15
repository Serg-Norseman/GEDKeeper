/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.IO;
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
    }
}
