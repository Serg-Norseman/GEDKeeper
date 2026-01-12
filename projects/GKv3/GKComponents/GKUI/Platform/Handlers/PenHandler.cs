/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

#pragma warning disable CS0618

using BSLib;
using Eto.Drawing;
using GKCore.Design.Graphics;
using GKUI.Components;

namespace GKUI.Platform.Handlers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class PenHandler : TypeHandler<Pen>, IPen
    {
        public IColor Color
        {
            get { return UIHelper.ConvertColor(Handle.Color); }
            set {
                // FIXME: don't works in Eto.Forms?!
                Handle.Color = UIHelper.ConvertColor(value);
            }
        }

        public float DashOffset
        {
            get { return (Handle.DashStyle == null) ? 0 : Handle.DashStyle.Offset; }
            set {
                if (Handle.DashStyle != null) {
                    Handle.DashStyle = new DashStyle(value, Handle.DashStyle.Dashes);
                }
            }
        }

        public float Width
        {
            get { return Handle.Thickness; }
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
    }
}
