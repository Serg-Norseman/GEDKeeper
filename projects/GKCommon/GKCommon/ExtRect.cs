/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
            return Create(left, top, left + width - 1, top + height - 1);
        }

        public static ExtRect CreateEmpty()
        {
            return Create(0, 0, 0, 0);
        }

        public int GetWidth()
        {
            return (Right == Left) ? 0 : Right - Left + 1;
        }

        public int GetHeight()
        {
            return (Bottom == Top) ? 0 : Bottom - Top + 1;
        }

        public bool IsEmpty()
        {
            return Right <= Left || Bottom <= Top;
        }

        public bool Contains(int x, int y)
        {
            return x >= Left && y >= Top && x <= Right && y <= Bottom;
        }

        public ExtRect GetOffset(int dX, int dY)
        {
            return Create(Left + dX, Top + dY, Right + dX, Bottom + dY);
        }

        public void Offset(int dX, int dY)
        {
            Left += dX;
            Right += dX;
            Top += dY;
            Bottom += dY;
        }

        public void Inflate(int dX, int dY)
        {
            Left += dX;
            Right -= dX;
            Top += dY;
            Bottom -= dY;
        }

        public bool IntersectsWith(ExtRect rect)
        {
            return rect.Left < Right && Left < rect.Right && rect.Top < Bottom && Top < rect.Bottom;
        }

        public override string ToString()
        {
            return string.Concat("{X=", Left.ToString(), ",Y=", Top.ToString(),
                                 ",Width=", GetWidth().ToString(), ",Height=", GetHeight().ToString(), "}");
        }
    }


    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public struct ExtRectF
    {
        public static readonly ExtRect Empty = default(ExtRect);

        public float Left;
        public float Top;
        public float Right;
        public float Bottom;

        public static ExtRectF Create(float left, float top, float right, float bottom)
        {
            ExtRectF result;
            result.Left = left;
            result.Top = top;
            result.Right = right;
            result.Bottom = bottom;
            return result;
        }

        public static ExtRectF CreateBounds(float left, float top, float width, float height)
        {
            return Create(left, top, left + width - 1, top + height - 1);
        }

        public static ExtRectF CreateEmpty()
        {
            return Create(0, 0, 0, 0);
        }

        public float GetWidth()
        {
            return (Right == Left) ? 0 : Right - Left + 1;
        }

        public float GetHeight()
        {
            return (Bottom == Top) ? 0 : Bottom - Top + 1;
        }

        public bool IsEmpty()
        {
            return Right <= Left || Bottom <= Top;
        }

        public bool Contains(int x, int y)
        {
            return x >= Left && y >= Top && x <= Right && y <= Bottom;
        }

        public ExtRectF GetOffset(float dX, float dY)
        {
            return Create(Left + dX, Top + dY, Right + dX, Bottom + dY);
        }

        public void Offset(int dX, int dY)
        {
            Left += dX;
            Right += dX;
            Top += dY;
            Bottom += dY;
        }

        public void Inflate(int dX, int dY)
        {
            Left += dX;
            Right -= dX;
            Top += dY;
            Bottom -= dY;
        }

        public bool IntersectsWith(ExtRect rect)
        {
            return rect.Left < Right && Left < rect.Right && rect.Top < Bottom && Top < rect.Bottom;
        }

        public override string ToString()
        {
            return string.Concat("{X=", Left.ToString(), ",Y=", Top.ToString(),
                                 ",Width=", GetWidth().ToString(), ",Height=", GetHeight().ToString(), "}");
        }
    }
}
