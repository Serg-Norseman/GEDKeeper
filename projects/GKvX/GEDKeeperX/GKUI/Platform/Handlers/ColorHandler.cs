/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
