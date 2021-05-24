/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using Avalonia.Media;
using Avalonia.Media.Imaging;
using BSLib;
using BSLib.Design.Graphics;
using IBrush = BSLib.Design.Graphics.IBrush;
using IImage = BSLib.Design.Graphics.IImage;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ColorHandler: TypeHandler<Color>, IColor
    {
        public ColorHandler(Color handle) : base(handle)
        {
        }

        public IColor Darker(float fraction)
        {
            //return new ColorHandler(UIHelper.Darker(this.Handle, fraction));
            return default(IColor);
        }

        public IColor Lighter(float fraction)
        {
            //return new ColorHandler(UIHelper.Lighter(this.Handle, fraction));
            return default(IColor);
        }

        public string GetName()
        {
            Color color = this.Handle;
            return color.ToString();
        }

        public int ToArgb()
        {
            int result = 0; // FIXME this.Handle.ToArgb();
            return result;
        }

        public string GetCode()
        {
            int argb = ToArgb() & 0xFFFFFF;
            string result = argb.ToString("X6");
            return result;
        }

        public byte GetR()
        {
            return Handle.R;
        }

        public byte GetG()
        {
            return Handle.G;
        }

        public byte GetB()
        {
            return Handle.B;
        }

        public byte GetA()
        {
            return Handle.A;
        }

        public bool IsTransparent()
        {
            return false; // FIXME (Handle == Color.Transparent);
        }
    }


    /// <summary>
    /// 
    /// </summary>
    /*public class GfxPathHandler: TypeHandler<GraphicsPath>, IGfxPath
    {
        public GfxPathHandler(GraphicsPath handle) : base(handle)
        {
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                Handle.Dispose();
            }
            base.Dispose(disposing);
        }

        public void AddEllipse(float x, float y, float width, float height)
        {
            Handle.AddEllipse(x, y, width, height);
        }

        public void CloseFigure()
        {
            Handle.CloseFigure();
        }

        public void StartFigure()
        {
            Handle.StartFigure();
        }

        public ExtRectF GetBounds()
        {
            RectangleF rect = Handle.GetBounds();
            return ExtRectF.CreateBounds(rect.Left, rect.Top, rect.Width, rect.Height);
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public class GfxCirclePathHandler: GfxPathHandler, IGfxCirclePath
    {
        public float X { get; set; }
        public float Y { get; set; }
        public float Width { get; set; }
        public float Height { get; set; }

        public GfxCirclePathHandler(GraphicsPath handle) : base(handle)
        {
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public class GfxCircleSegmentPathHandler: GfxPathHandler, IGfxCircleSegmentPath
    {
        public float InRad { get; set; }
        public float ExtRad { get; set; }
        public float WedgeAngle { get; set; }
        public float Ang1 { get; set; }
        public float Ang2 { get; set; }

        public GfxCircleSegmentPathHandler(GraphicsPath handle) : base(handle)
        {
        }
    }*/


    /// <summary>
    /// 
    /// </summary>
    /*public sealed class PenHandler: TypeHandler<Pen>, IPen
    {
        public IColor Color
        {
            get { return UIHelper.ConvertColor(Handle.Color); }
        }

        public float Width
        {
            get { return Handle.Width; }
        }

        public PenHandler(Pen handle) : base(handle)
        {
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                Handle.Dispose();
            }
            base.Dispose(disposing);
        }
    }*/


    /// <summary>
    /// 
    /// </summary>
    public sealed class BrushHandler: TypeHandler<Brush>, IBrush
    {
        public IColor Color
        {
            get { return null; /*UIHelper.ConvertColor(((SolidColorBrush)Handle).Color);*/ }
        }

        public BrushHandler(Brush handle) : base(handle)
        {
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                //Handle.Dispose();
            }
            base.Dispose(disposing);
        }
    }


    /// <summary>
    /// 
    /// </summary>
    /*public sealed class FontHandler: TypeHandler<Font>, IFont
    {
        public string FontFamilyName
        {
            get { return Handle.FontFamily.Name; }
        }

        public string Name
        {
            get { return Handle.Name; }
        }

        public float Size
        {
            get { return Handle.Size; }
        }

        public FontHandler(Font handle) : base(handle)
        {
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                Handle.Dispose();
            }
            base.Dispose(disposing);
        }
    }*/


    /// <summary>
    /// 
    /// </summary>
    public sealed class ImageHandler: TypeHandler<Bitmap>, IImage
    {
        public int Height
        {
            get { return 0; /* Handle.Height; */ }
        }

        public int Width
        {
            get { return 0; /* Handle.Width; */ }
        }

        public ImageHandler(Bitmap handle) : base(handle)
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
            //Handle.get
            using (var stream = new MemoryStream())
            {
                //Handle.Save(stream, System.Drawing.Imaging.ImageFormat.Bmp);
                return stream.ToArray();
            }
        }
    }
}
