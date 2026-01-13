/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
