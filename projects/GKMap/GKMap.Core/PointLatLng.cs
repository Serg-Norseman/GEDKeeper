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
    /// the point of coordinates
    /// </summary>
    public struct PointLatLng
    {
        public static readonly PointLatLng Empty = new PointLatLng();


        private double fLat;
        private double fLng;
        private bool fNotEmpty;


        public PointLatLng(double lat, double lng)
        {
            fLat = lat;
            fLng = lng;
            fNotEmpty = true;
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

        public double Lat
        {
            get {
                return fLat;
            }
            set {
                fLat = value;
                fNotEmpty = true;
            }
        }

        public double Lng
        {
            get {
                return fLng;
            }
            set {
                fLng = value;
                fNotEmpty = true;
            }
        }

        public static bool operator ==(PointLatLng left, PointLatLng right)
        {
            return ((left.Lng == right.Lng) && (left.Lat == right.Lat));
        }

        public static bool operator !=(PointLatLng left, PointLatLng right)
        {
            return !(left == right);
        }

        public override bool Equals(object obj)
        {
            if (!(obj is PointLatLng)) {
                return false;
            }
            PointLatLng tf = (PointLatLng)obj;
            return (((tf.Lng == Lng) && (tf.Lat == Lat)) && tf.GetType() == GetType());
        }

        public void Offset(double lat, double lng)
        {
            fLng += lng;
            fLat -= lat;
        }

        public override int GetHashCode()
        {
            return (Lng.GetHashCode() ^ Lat.GetHashCode());
        }

        public override string ToString()
        {
            return string.Format(CultureInfo.CurrentCulture, "{{Lat={0}, Lng={1}}}", Lat, Lng);
        }
    }
}
