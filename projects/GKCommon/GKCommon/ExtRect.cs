/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

using System.Drawing;
using System.Runtime.InteropServices;

namespace GKCommon
{
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public struct ExtRect
    {
        public static readonly ExtRect Empty = default(ExtRect);
        
        public int Left;
        public int Top;
        public int Right;
        public int Bottom;

        public static ExtRect Create(int left, int top, int right, int bottom)
        {
            ExtRect result;
            result.Left = left;
            result.Top = top;
            result.Right = right;
            result.Bottom = bottom;
            return result;
        }

        public static ExtRect CreateBounds(int left, int top, int width, int height)
        {
            return ExtRect.Create(left, top, left + width - 1, top + height - 1);
        }

        public static ExtRect CreateEmpty()
        {
            return ExtRect.Create(0, 0, 0, 0);
        }

        public int GetWidth()
        {
            return this.Right - this.Left + 1;
        }

        public int GetHeight()
        {
            return this.Bottom - this.Top + 1;
        }

        public bool IsEmpty()
        {
            return this.Right <= this.Left || this.Bottom <= this.Top;
        }

        public bool Contains(int x, int y)
        {
            return x >= this.Left && y >= this.Top && x <= this.Right && y <= this.Bottom;
        }

        public ExtRect GetOffset(int dX, int dY)
        {
            return ExtRect.Create(this.Left + dX, this.Top + dY, this.Right + dX, this.Bottom + dY);
        }

        public void Offset(int dX, int dY)
        {
            this.Left += dX;
            this.Right += dX;
            this.Top += dY;
            this.Bottom += dY;
        }

        public void Inflate(int dX, int dY)
        {
            this.Left += dX;
            this.Right -= dX;
            this.Top += dY;
            this.Bottom -= dY;
        }

        public bool IntersectsWith(ExtRect rect)
        {
            return rect.Left < this.Right && this.Left < rect.Right && rect.Top < this.Bottom && this.Top < rect.Bottom;
        }

        public override string ToString()
        {
            return string.Concat(new string[] {
                                     "{X=", this.Left.ToString(), ",Y=", this.Top.ToString(),
                                     ",Width=", this.GetWidth().ToString(), ",Height=", this.GetHeight().ToString(), "}"
                                 });
        }

        public Rectangle ToRectangle()
        {
            return new Rectangle(this.Left, this.Top, this.Right - this.Left + 1, this.Bottom - this.Top + 1);
        }
    }
}
