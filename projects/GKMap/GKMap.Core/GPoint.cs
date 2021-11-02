/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Collections.Generic;
using System.Globalization;

namespace GKMap
{
    /// <summary>
    /// the point.
    /// </summary>
    public struct GPoint
    {
        public static readonly GPoint Empty = new GPoint();

        private long fX;
        private long fY;

        public GPoint(long x, long y)
        {
            fX = x;
            fY = y;
        }

        public bool IsEmpty
        {
            get {
                return fX == 0 && fY == 0;
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

        public static bool operator ==(GPoint left, GPoint right)
        {
            return left.X == right.X && left.Y == right.Y;
        }

        public static bool operator !=(GPoint left, GPoint right)
        {
            return !(left == right);
        }

        public override bool Equals(object obj)
        {
            if (!(obj is GPoint))
                return false;
            GPoint comp = (GPoint)obj;
            return comp.X == X && comp.Y == Y;
        }

        public override int GetHashCode()
        {
            return (int)(fX ^ fY);
        }

        public void Offset(long dx, long dy)
        {
            X += dx;
            Y += dy;
        }

        public void Offset(GPoint p)
        {
            Offset(p.X, p.Y);
        }

        public void OffsetNegative(GPoint p)
        {
            Offset(-p.X, -p.Y);
        }

        public override string ToString()
        {
            return "{X=" + X.ToString(CultureInfo.CurrentCulture) + ",Y=" + Y.ToString(CultureInfo.CurrentCulture) + "}";
        }
    }

    internal class GPointComparer : IEqualityComparer<GPoint>
    {
        public bool Equals(GPoint x, GPoint y)
        {
            return x.X == y.X && x.Y == y.Y;
        }

        public int GetHashCode(GPoint obj)
        {
            return obj.GetHashCode();
        }
    }
}
