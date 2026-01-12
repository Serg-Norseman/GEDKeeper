/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using Eto.Drawing;
using GKCore.Design.Graphics;
using GKUI.Components;

namespace GKUI.Platform.Handlers
{
    /// <summary>
    /// 
    /// </summary>
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
            return color.ToString(); // Name;
        }

        public int ToArgb()
        {
            int result = Handle.ToArgb();
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
            return (byte)Handle.Rb;
        }

        public byte GetG()
        {
            return (byte)Handle.Gb;
        }

        public byte GetB()
        {
            return (byte)Handle.Bb;
        }

        public byte GetA()
        {
            return (byte)Handle.Ab;
        }

        public bool IsTransparent()
        {
            return Handle == Colors.Transparent;
        }
    }
}
