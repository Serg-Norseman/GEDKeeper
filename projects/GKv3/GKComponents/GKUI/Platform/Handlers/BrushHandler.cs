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
using GKUI.Components;

namespace GKUI.Platform.Handlers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class BrushHandler : TypeHandler<Brush>, IBrush
    {
        public IColor Color
        {
            get { return UIHelper.ConvertColor(((SolidBrush)Handle).Color); }
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
