﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class FontHandler: TypeHandler<SKPaint>, IFont
    {
        public string FontFamilyName
        {
            get { return Handle.Typeface.FamilyName; }
        }

        public string Name
        {
            get { return Handle.Typeface.FamilyName; }
        }

        public float Size
        {
            get { return Handle.TextSize; }
        }

        public float Height
        {
            get { return Handle.TextSize /* pixels! */; }
        }

        public FontHandler(SKPaint handle) : base(handle)
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
