/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GKCore.Design.Graphics;
using Terminal.Gui;

namespace GKUI.Platform.Handlers
{
    public sealed class ColorHandler : TypeHandler<Color>, IColor
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
