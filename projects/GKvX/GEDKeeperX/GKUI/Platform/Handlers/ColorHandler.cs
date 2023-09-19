/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018-2023 by Sergey V. Zhdanovskih.
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
using Xamarin.Essentials;
using Xamarin.Forms;

namespace GKUI.Components
{
    public sealed class ColorHandler : TypeHandler<Color>, IColor
    {
        public ColorHandler(Color handle) : base(handle)
        {
        }

        public IColor Darker(float fraction)
        {
            return new ColorHandler(UIHelper.Darker(Handle, fraction));
        }

        public IColor Lighter(float fraction)
        {
            return new ColorHandler(UIHelper.Lighter(Handle, fraction));
        }

        public string GetName()
        {
            Color color = Handle;
            return color.ToString();
        }

        public int ToArgb()
        {
            int result = ColorExtensions.ToInt(Handle);
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
            return (byte)(Handle.R * 255);
        }

        public byte GetG()
        {
            return (byte)(Handle.G * 255);
        }

        public byte GetB()
        {
            return (byte)(Handle.B * 255);
        }

        public byte GetA()
        {
            return (byte)(Handle.A * 255);
        }

        public bool IsTransparent()
        {
            return (Handle == Color.Transparent);
        }
    }
}
