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
    /// the size
    /// </summary>
    public struct GSize
    {
        private long fWidth;
        private long fHeight;

        public GSize(long width, long height)
        {
            fWidth = width;
            fHeight = height;
        }

        public static bool operator ==(GSize sz1, GSize sz2)
        {
            return sz1.Width == sz2.Width && sz1.Height == sz2.Height;
        }

        public static bool operator !=(GSize sz1, GSize sz2)
        {
            return !(sz1 == sz2);
        }

        public bool IsEmpty
        {
            get {
                return fWidth == 0 && fHeight == 0;
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

        public override bool Equals(object obj)
        {
            if (obj is GSize comp) {
                return (comp.fWidth == fWidth) && (comp.fHeight == fHeight);
            }
            return false;
        }

        public override int GetHashCode()
        {
            if (IsEmpty) {
                return 0;
            }
            return (Width.GetHashCode() ^ Height.GetHashCode());
        }

        public override string ToString()
        {
            return "{Width=" + fWidth.ToString(CultureInfo.CurrentCulture) + ", Height=" + fHeight.ToString(CultureInfo.CurrentCulture) + "}";
        }
    }
}
