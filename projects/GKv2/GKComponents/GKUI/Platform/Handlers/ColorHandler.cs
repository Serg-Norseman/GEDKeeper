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
    public sealed class ColorHandler: TypeHandler<Color>, IColor
    {
        public ColorHandler(Color handle) : base(handle)
        {
        }

        public IColor Darker(float fraction)
        {
            int rgb = Handle.ToArgb();
            Color darkColor = Color.FromArgb(GfxHelper.Darker(rgb, fraction));
            return new ColorHandler(darkColor);
        }

        public IColor Lighter(float fraction)
        {
            int rgb = Handle.ToArgb();
            Color lightColor = Color.FromArgb(GfxHelper.Lighter(rgb, fraction));
            return new ColorHandler(lightColor);
        }

        public string GetName()
        {
            Color color = this.Handle;
            return color.Name;
        }

        public int ToArgb()
        {
            int result = this.Handle.ToArgb();
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
            return Handle.R;
        }

        public byte GetG()
        {
            return Handle.G;
        }

        public byte GetB()
        {
            return Handle.B;
        }

        public byte GetA()
        {
            return Handle.A;
        }

        public bool IsTransparent()
        {
            return (Handle == Color.Transparent);
        }
    }
}
