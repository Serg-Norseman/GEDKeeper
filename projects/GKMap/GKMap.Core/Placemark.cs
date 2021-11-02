/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

namespace GKMap
{
    /// <summary>
    /// represents place info
    /// </summary>
    public struct Placemark
    {
        private string fAddress;

        /// <summary>
        /// the address
        /// </summary>
        public string Address
        {
            get {
                return fAddress;
            }
            internal set {
                fAddress = value;
            }
        }

        /// <summary>
        /// the accuracy of address
        /// </summary>
        public int Accuracy;

        // parsed values from address      
        public string ThoroughfareName;
        public string LocalityName;
        public string PostalCodeNumber;
        public string CountryName;
        public string AdministrativeAreaName;
        public string DistrictName;
        public string SubAdministrativeAreaName;
        public string Neighborhood;
        public string StreetNumber;

        public string CountryNameCode;
        public string HouseNo;

        internal Placemark(string address)
        {
            fAddress = address;

            Accuracy = 0;
            HouseNo = string.Empty;
            ThoroughfareName = string.Empty;
            DistrictName = string.Empty;
            LocalityName = string.Empty;
            PostalCodeNumber = string.Empty;
            CountryName = string.Empty;
            CountryNameCode = string.Empty;
            AdministrativeAreaName = string.Empty;
            SubAdministrativeAreaName = string.Empty;
            Neighborhood = string.Empty;
            StreetNumber = string.Empty;
        }
    }
}
