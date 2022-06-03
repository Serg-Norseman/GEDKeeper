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
    /// the rect of coordinates
    /// </summary>
    public struct RectLatLng
    {
        public static readonly RectLatLng Empty = new RectLatLng();


        private double fLng;
        private double fLat;
        private double fWidthLng;
        private double fHeightLat;
        private bool fNotEmpty;


        public PointLatLng LocationTopLeft
        {
            get {
                return new PointLatLng(fLat, fLng);
            }
            set {
                fLng = value.Lng;
                fLat = value.Lat;
            }
        }

        public PointLatLng LocationRightBottom
        {
            get {
                PointLatLng ret = new PointLatLng(Lat, Lng);
                ret.Offset(HeightLat, WidthLng);
                return ret;
            }
        }

        public SizeLatLng Size
        {
            get {
                return new SizeLatLng(fHeightLat, fWidthLng);
            }
            set {
                fWidthLng = value.WidthLng;
                fHeightLat = value.HeightLat;
            }
        }

        public double Lng
        {
            get {
                return fLng;
            }
            set {
                fLng = value;
            }
        }

        public double Lat
        {
            get {
                return fLat;
            }
            set {
                fLat = value;
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

        /// <summary>
        /// returns true if coordinates wasn't assigned
        /// </summary>
        public bool IsEmpty
        {
            get {
                return !fNotEmpty;
            }
        }

        public RectLatLng(double lat, double lng, double widthLng, double heightLat)
        {
            fLng = lng;
            fLat = lat;
            fWidthLng = widthLng;
            fHeightLat = heightLat;
            fNotEmpty = true;
        }

        public static RectLatLng FromLTRB(double leftLng, double topLat, double rightLng, double bottomLat)
        {
            return new RectLatLng(topLat, leftLng, rightLng - leftLng, topLat - bottomLat);
        }

        public override bool Equals(object obj)
        {
            if (!(obj is RectLatLng)) {
                return false;
            }
            RectLatLng ef = (RectLatLng)obj;
            return ((((ef.Lng == Lng) && (ef.Lat == Lat)) && (ef.WidthLng == WidthLng)) && (ef.HeightLat == HeightLat));
        }

        public static bool operator ==(RectLatLng left, RectLatLng right)
        {
            return ((((left.Lng == right.Lng) && (left.Lat == right.Lat)) && (left.WidthLng == right.WidthLng)) && (left.HeightLat == right.HeightLat));
        }

        public static bool operator !=(RectLatLng left, RectLatLng right)
        {
            return !(left == right);
        }

        public bool Contains(double lat, double lng)
        {
            return ((((Lng <= lng) && (lng < (Lng + WidthLng))) && (Lat >= lat)) && (lat > (Lat - HeightLat)));
        }

        public bool Contains(PointLatLng pt)
        {
            return Contains(pt.Lat, pt.Lng);
        }

        public override int GetHashCode()
        {
            if (IsEmpty) {
                return 0;
            }
            return (((Lng.GetHashCode() ^ Lat.GetHashCode()) ^ WidthLng.GetHashCode()) ^ HeightLat.GetHashCode());
        }

        public override string ToString()
        {
            return ("{Lat=" + Lat.ToString(CultureInfo.CurrentCulture) + ",Lng=" + Lng.ToString(CultureInfo.CurrentCulture) + ",WidthLng=" + WidthLng.ToString(CultureInfo.CurrentCulture) + ",HeightLat=" + HeightLat.ToString(CultureInfo.CurrentCulture) + "}");
        }
    }
}
