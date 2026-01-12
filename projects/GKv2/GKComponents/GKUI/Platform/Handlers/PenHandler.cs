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
    public sealed class PenHandler: TypeHandler<Pen>, IPen
    {
        public IColor Color
        {
            get { return new ColorHandler(Handle.Color); }
            set { Handle.Color = ((ColorHandler)value).Handle; }
        }

        public float DashOffset
        {
            get { return Handle.DashOffset; }
            set { Handle.DashOffset = value; }
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
    }
}
