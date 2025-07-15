﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using Eto.Drawing;
using GKCore.Charts;
using GKCore.Design.Graphics;

namespace GKUI.Platform.Handlers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class FontHandler : TypeHandler<Font>, IFont
    {
        private float fHeight;

        public string FontFamilyName
        {
            get { return Handle.FamilyName; }
        }

        public float Height
        {
            get { return fHeight; }
        }

        public string Name
        {
            get { return Handle.FamilyName; }
        }

        public float Size
        {
            get { return Handle.Size; }
        }

        public FontHandler(Font handle) : base(handle)
        {
            var size = handle.MeasureString(ChartRenderer.STR_HEIGHT_SAMPLE);
            fHeight = size.Height;
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
