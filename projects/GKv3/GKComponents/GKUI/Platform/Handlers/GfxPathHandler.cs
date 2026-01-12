/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using Eto.Drawing;
using GKCore.Design.Graphics;

namespace GKUI.Platform.Handlers
{
    /// <summary>
    /// 
    /// </summary>
    public class GfxPathHandler : TypeHandler<GraphicsPath>, IGfxPath
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
            RectangleF rect = Handle.Bounds;
            return ExtRectF.CreateBounds(rect.Left, rect.Top, rect.Width, rect.Height);
        }
    }
}
