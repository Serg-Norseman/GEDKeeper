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
using SkiaSharp.Views.Forms;

namespace GKUI.Components
{
    public sealed class BrushHandler : TypeHandler<SKPaint>, IBrush
    {
        public IColor Color
        {
            get { return UIHelper.ConvertColor(Handle.Color.ToFormsColor()); }
        }

        public BrushHandler(SKPaint handle) : base(handle)
        {
        }
    }
}
