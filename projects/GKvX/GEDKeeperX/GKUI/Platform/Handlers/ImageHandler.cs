/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018-2023 by Sergey V. Zhdanovskih.
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
    /*public sealed class ImageHandler : TypeHandler<SKImageImageSource>, IImage
    {
        public int Height
        {
            get { return Handle.Image.Height; }
        }

        public int Width
        {
            get { return Handle.Image.Width; }
        }

        public ImageHandler(SKImageImageSource handle) : base(handle)
        {
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                //Handle.Dispose();
            }
            base.Dispose(disposing);
        }

        public byte[] GetBytes()
        {
            return GetBytes("bmp");
        }

        public byte[] GetBytes(string format)
        {
            //Handle.get
            using (var stream = new MemoryStream()) {
                //Handle.Save(stream, System.Drawing.Imaging.ImageFormat.Bmp);
                return stream.ToArray();
            }
        }

        public IImage Resize(int newWidth, int newHeight)
        {
            return this;
        }
    }*/
    public sealed class ImageHandler : TypeHandler<ImageSource>, IImage
    {
        public int Height
        {
            get { return 0; }
        }

        public int Width
        {
            get { return 0; }
        }

        public ImageHandler(ImageSource handle) : base(handle)
        {
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                //Handle.Dispose();
            }
            base.Dispose(disposing);
        }

        public byte[] GetBytes()
        {
            return GetBytes("bmp");
        }

        public byte[] GetBytes(string format)
        {
            //Handle.get
            using (var stream = new MemoryStream()) {
                //Handle.Save(stream, System.Drawing.Imaging.ImageFormat.Bmp);
                return stream.ToArray();
            }
        }

        public IImage Resize(int newWidth, int newHeight)
        {
            return this;
        }
    }
}
