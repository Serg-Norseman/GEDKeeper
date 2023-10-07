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

using BSLib;
using GKCore.Design.Graphics;
using SkiaSharp;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GfxPathHandler: TypeHandler<SKPath>, IGfxPath
    {
        public GfxPathHandler(SKPath handle) : base(handle)
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
            Handle.AddOval(new SKRect(x, y, x + width - 1, y + height - 1));
        }

        public void CloseFigure()
        {
            Handle.Close();
        }

        public void StartFigure()
        {
            Handle.Reset();
        }

        public ExtRectF GetBounds()
        {
            var rect = Handle.Bounds;
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

        public GfxCirclePathHandler(SKPath handle) : base(handle)
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

        public GfxCircleSegmentPathHandler(SKPath handle) : base(handle)
        {
        }
    }
}
