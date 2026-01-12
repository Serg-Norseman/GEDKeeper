/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Drawing;
using BSLib;
using GKCore.Design.Graphics;

namespace GKUI.Platform.Handlers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class BrushHandler: TypeHandler<Brush>, IBrush
    {
        public IColor Color
        {
            get { return new ColorHandler(((SolidBrush)Handle).Color); }
        }

        public BrushHandler(Brush handle) : base(handle)
        {
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                Handle.Dispose();
            }
            base.Dispose(disposing);
        }
    }
}
