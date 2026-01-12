/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Drawing.Drawing2D;
using GKCore.Design.Graphics;

namespace GKUI.Platform.Handlers
{
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
    }
}
