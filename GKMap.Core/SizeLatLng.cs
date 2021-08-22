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
    /// the size of coordinates
    /// </summary>
    public struct SizeLatLng
    {
        private double fHeightLat;
        private double fWidthLng;


        public bool IsEmpty
        {
            get {
                return ((fWidthLng == 0d) && (fHeightLat == 0d));
            }
        }

        public double WidthLng
        {
            get {
                return fWidthLng;
            }
            set {
                fWidthLng = value;
            }
        }

        public double HeightLat
        {
            get {
                return fHeightLat;
            }
            set {
                fHeightLat = value;
            }
        }

        public SizeLatLng(double heightLat, double widthLng)
        {
            fHeightLat = heightLat;
            fWidthLng = widthLng;
        }

        public static bool operator ==(SizeLatLng sz1, SizeLatLng sz2)
        {
            return ((sz1.WidthLng == sz2.WidthLng) && (sz1.HeightLat == sz2.HeightLat));
        }

        public static bool operator !=(SizeLatLng sz1, SizeLatLng sz2)
        {
            return !(sz1 == sz2);
        }

        public override bool Equals(object obj)
        {
            if (!(obj is SizeLatLng)) {
                return false;
            }
            SizeLatLng ef = (SizeLatLng)obj;
            return (((ef.WidthLng == WidthLng) && (ef.HeightLat == HeightLat)) && ef.GetType() == GetType());
        }

        public override int GetHashCode()
        {
            if (IsEmpty) {
                return 0;
            }
            return (WidthLng.GetHashCode() ^ HeightLat.GetHashCode());
        }

        public override string ToString()
        {
            return ("{WidthLng=" + fWidthLng.ToString(CultureInfo.CurrentCulture) + ", HeightLng=" + fHeightLat.ToString(CultureInfo.CurrentCulture) + "}");
        }
    }
}
