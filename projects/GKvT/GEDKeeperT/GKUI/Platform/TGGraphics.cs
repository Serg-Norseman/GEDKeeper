/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib.Design.Graphics;
using Terminal.Gui;

namespace GKUI.Platform
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ColorHandler: TypeHandler<Color>, IColor
    {
        public ColorHandler(Color handle) : base(handle)
        {
        }

        public IColor Darker(float fraction)
        {
            return this;

        }

        public IColor Lighter(float fraction)
        {
            return this;

        }

        public string GetName()
        {
            Color color = this.Handle;
            return color.ToString(); // Name;
        }

        public int ToArgb()
        {
            int result = 0;
            return result;
        }

        public string GetCode()
        {
            int argb = ToArgb() & 0xFFFFFF;
            string result = argb.ToString("X6");
            return result;
        }

        public byte GetR()
        {
            return 0;
        }

        public byte GetG()
        {
            return 0;
        }

        public byte GetB()
        {
            return 0;
        }

        public byte GetA()
        {
            return 0;
        }

        public bool IsTransparent()
        {
            return false;
        }
    }
}
