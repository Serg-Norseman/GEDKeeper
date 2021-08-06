/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Globalization;

namespace GKMap
{
    /// <summary>
    /// the rect.
    /// </summary>
    public struct GRect
    {
        public static readonly GRect Empty = new GRect();

        private long fX;
        private long fY;
        private long fWidth;
        private long fHeight;

        public GRect(long x, long y, long width, long height)
        {
            fX = x;
            fY = y;
            fWidth = width;
            fHeight = height;
        }

        public GRect(GPoint location, GSize size)
        {
            fX = location.X;
            fY = location.Y;
            fWidth = size.Width;
            fHeight = size.Height;
        }

        public GPoint Location
        {
            get {
                return new GPoint(X, Y);
            }
            set {
                X = value.X;
                Y = value.Y;
            }
        }

        public GSize Size
        {
            get {
                return new GSize(Width, Height);
            }
            set {
                Width = value.Width;
                Height = value.Height;
            }
        }

        public long X
        {
            get {
                return fX;
            }
            set {
                fX = value;
            }
        }

        public long Y
        {
            get {
                return fY;
            }
            set {
                fY = value;
            }
        }

        public long Width
        {
            get {
                return fWidth;
            }
            set {
                fWidth = value;
            }
        }

        public long Height
        {
            get {
                return fHeight;
            }
            set {
                fHeight = value;
            }
        }

        public bool IsEmpty
        {
            get {
                return fHeight == 0 && fWidth == 0 && fX == 0 && fY == 0;
            }
        }

        public override bool Equals(object obj)
        {
            if (!(obj is GRect))
                return false;

            GRect comp = (GRect)obj;

            return (comp.X == X) &&
               (comp.Y == Y) &&
               (comp.Width == Width) &&
               (comp.Height == Height);
        }

        public static bool operator ==(GRect left, GRect right)
        {
            return (left.X == right.X
                       && left.Y == right.Y
                       && left.Width == right.Width
                       && left.Height == right.Height);
        }

        public static bool operator !=(GRect left, GRect right)
        {
            return !(left == right);
        }

        public override int GetHashCode()
        {
            if (IsEmpty) {
                return 0;
            }
            return (int)(((X ^ ((Y << 13) | (Y >> 0x13))) ^ ((Width << 0x1a) | (Width >> 6))) ^ ((Height << 7) | (Height >> 0x19)));
        }

        public void OffsetNegative(GPoint pos)
        {
            Offset(-pos.X, -pos.Y);
        }

        public void Offset(long x, long y)
        {
            X += x;
            Y += y;
        }

        public override string ToString()
        {
            return "{X=" + X.ToString(CultureInfo.CurrentCulture) + ",Y=" + Y.ToString(CultureInfo.CurrentCulture) +
               ",Width=" + Width.ToString(CultureInfo.CurrentCulture) + ",Height=" + Height.ToString(CultureInfo.CurrentCulture) + "}";
        }
    }
}
