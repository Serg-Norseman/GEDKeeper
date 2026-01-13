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
    /// <summary>
    /// 
    /// </summary>
    public sealed class PenHandler: TypeHandler<SKPaint>, IPen
    {
        public IColor Color
        {
            get { return UIHelper.ConvertColor(Handle.Color.ToFormsColor()); }
            set { Handle.Color = ((ColorHandler)value).Handle.ToSKColor(); }
        }

        // FIXME: don't exists? SKPaint.PathEffect.Phase
        public float DashOffset
        {
            get { return 0; }
            set { }
        }

        public float Width
        {
            get { return Handle.StrokeWidth; }
        }

        public PenHandler(SKPaint handle) : base(handle)
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
