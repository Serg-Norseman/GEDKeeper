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
