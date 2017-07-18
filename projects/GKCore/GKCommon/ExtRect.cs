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

using System;
using System.Globalization;
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

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public struct ExtPoint
    {
        public static readonly ExtPoint Empty = default(ExtPoint);

        private int x;
        private int y;

        public bool IsEmpty
        {
            get {
                return this.x == 0 && this.y == 0;
            }
        }

        public int X
        {
            get {
                return this.x;
            }
            set {
                this.x = value;
            }
        }

        public int Y
        {
            get {
                return this.y;
            }
            set {
                this.y = value;
            }
        }

        public ExtPoint(int x, int y)
        {
            this.x = x;
            this.y = y;
        }

        public static implicit operator ExtPointF(ExtPoint p)
        {
            return new ExtPointF(p.X, p.Y);
        }

        public static ExtPoint Add(ExtPoint pt, ExtSize sz)
        {
            return new ExtPoint(pt.X + sz.Width, pt.Y + sz.Height);
        }

        public static ExtPoint Subtract(ExtPoint pt, ExtSize sz)
        {
            return new ExtPoint(pt.X - sz.Width, pt.Y - sz.Height);
        }

        public static ExtPoint Truncate(ExtPointF value)
        {
            return new ExtPoint((int)value.X, (int)value.Y);
        }

        public override bool Equals(object obj)
        {
            if (!(obj is ExtPoint))
            {
                return false;
            }
            ExtPoint point = (ExtPoint)obj;
            return point.X == this.X && point.Y == this.Y;
        }

        public override int GetHashCode()
        {
            return this.x ^ this.y;
        }

        public void Offset(int dx, int dy)
        {
            this.X += dx;
            this.Y += dy;
        }

        public void Offset(ExtPoint p)
        {
            this.Offset(p.X, p.Y);
        }

        public override string ToString()
        {
            return string.Concat(new string[]
                                 {
                                     "{X=",
                                     this.X.ToString(CultureInfo.CurrentCulture),
                                     ",Y=",
                                     this.Y.ToString(CultureInfo.CurrentCulture),
                                     "}"
                                 });
        }
    }


    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public struct ExtPointF
    {
        public static readonly ExtPointF Empty = default(ExtPointF);

        private float x;
        private float y;

        public bool IsEmpty
        {
            get {
                return this.x == 0f && this.y == 0f;
            }
        }

        public float X
        {
            get {
                return this.x;
            }
            set {
                this.x = value;
            }
        }

        public float Y
        {
            get {
                return this.y;
            }
            set {
                this.y = value;
            }
        }

        public ExtPointF(float x, float y)
        {
            this.x = x;
            this.y = y;
        }

        public static ExtPointF Add(ExtPointF pt, ExtSize sz)
        {
            return new ExtPointF(pt.X + (float)sz.Width, pt.Y + (float)sz.Height);
        }

        public static ExtPointF Subtract(ExtPointF pt, ExtSize sz)
        {
            return new ExtPointF(pt.X - (float)sz.Width, pt.Y - (float)sz.Height);
        }

        public static ExtPointF Add(ExtPointF pt, ExtSizeF sz)
        {
            return new ExtPointF(pt.X + sz.Width, pt.Y + sz.Height);
        }

        public static ExtPointF Subtract(ExtPointF pt, ExtSizeF sz)
        {
            return new ExtPointF(pt.X - sz.Width, pt.Y - sz.Height);
        }

        public override bool Equals(object obj)
        {
            if (!(obj is ExtPointF))
            {
                return false;
            }
            ExtPointF pointF = (ExtPointF)obj;
            return pointF.X == this.X && pointF.Y == this.Y && pointF.GetType().Equals(base.GetType());
        }

        public override int GetHashCode()
        {
            return base.GetHashCode();
        }

        public override string ToString()
        {
            return string.Format(CultureInfo.CurrentCulture, "{{X={0}, Y={1}}}", new object[]
                                 {
                                     this.x,
                                     this.y
                                 });
        }
    }


    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public struct ExtSize
    {
        public static readonly ExtSize Empty = default(ExtSize);

        private int width;
        private int height;

        public bool IsEmpty
        {
            get {
                return this.width == 0 && this.height == 0;
            }
        }

        public int Width
        {
            get {
                return this.width;
            }
            set {
                this.width = value;
            }
        }

        public int Height
        {
            get {
                return this.height;
            }
            set {
                this.height = value;
            }
        }

        public ExtSize(int width, int height)
        {
            this.width = width;
            this.height = height;
        }

        public static implicit operator ExtSizeF(ExtSize p)
        {
            return new ExtSizeF((float)p.Width, (float)p.Height);
        }

        public static explicit operator ExtPoint(ExtSize size)
        {
            return new ExtPoint(size.Width, size.Height);
        }

        public static ExtSize Add(ExtSize sz1, ExtSize sz2)
        {
            return new ExtSize(sz1.Width + sz2.Width, sz1.Height + sz2.Height);
        }

        public static ExtSize Subtract(ExtSize sz1, ExtSize sz2)
        {
            return new ExtSize(sz1.Width - sz2.Width, sz1.Height - sz2.Height);
        }

        public static ExtSize Truncate(ExtSizeF value)
        {
            return new ExtSize((int)value.Width, (int)value.Height);
        }

        public override bool Equals(object obj)
        {
            if (!(obj is ExtSize))
            {
                return false;
            }
            ExtSize size = (ExtSize)obj;
            return size.width == this.width && size.height == this.height;
        }

        public override int GetHashCode()
        {
            return this.width ^ this.height;
        }

        public override string ToString()
        {
            return string.Concat(new string[]
                                 {
                                     "{Width=",
                                     this.width.ToString(CultureInfo.CurrentCulture),
                                     ", Height=",
                                     this.height.ToString(CultureInfo.CurrentCulture),
                                     "}"
                                 });
        }
    }


    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public struct ExtSizeF
    {
        public static readonly ExtSizeF Empty = default(ExtSizeF);

        private float width;
        private float height;

        public bool IsEmpty
        {
            get {
                return this.width == 0f && this.height == 0f;
            }
        }

        public float Width
        {
            get {
                return this.width;
            }
            set {
                this.width = value;
            }
        }

        public float Height
        {
            get {
                return this.height;
            }
            set {
                this.height = value;
            }
        }

        public ExtSizeF(ExtSizeF size)
        {
            this.width = size.width;
            this.height = size.height;
        }

        public ExtSizeF(float width, float height)
        {
            this.width = width;
            this.height = height;
        }

        public static ExtSizeF Add(ExtSizeF sz1, ExtSizeF sz2)
        {
            return new ExtSizeF(sz1.Width + sz2.Width, sz1.Height + sz2.Height);
        }

        public static ExtSizeF Subtract(ExtSizeF sz1, ExtSizeF sz2)
        {
            return new ExtSizeF(sz1.Width - sz2.Width, sz1.Height - sz2.Height);
        }

        public override bool Equals(object obj)
        {
            if (!(obj is ExtSizeF))
            {
                return false;
            }
            ExtSizeF sizeF = (ExtSizeF)obj;
            return sizeF.Width == this.Width && sizeF.Height == this.Height && sizeF.GetType().Equals(base.GetType());
        }

        public override int GetHashCode()
        {
            return base.GetHashCode();
        }

        public ExtSize ToSize()
        {
            return ExtSize.Truncate(this);
        }

        public override string ToString()
        {
            return string.Concat(new string[]
                                 {
                                     "{Width=",
                                     this.width.ToString(CultureInfo.CurrentCulture),
                                     ", Height=",
                                     this.height.ToString(CultureInfo.CurrentCulture),
                                     "}"
                                 });
        }
    }
}
