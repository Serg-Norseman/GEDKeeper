/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
