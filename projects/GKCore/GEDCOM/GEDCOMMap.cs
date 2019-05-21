/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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

using BSLib;

namespace GKCommon.GEDCOM
{
    public sealed class GEDCOMMap : GEDCOMTag
    {
        public double Lati
        {
            get { return GetGeoCoord(GetTagStringValue(GEDCOMTagType.LATI), GeoCoord.Lati); }
            set { SetTagFloatValue(GEDCOMTagType.LATI, value); }
        }

        public double Long
        {
            get { return GetGeoCoord(GetTagStringValue(GEDCOMTagType.LONG), GeoCoord.Long); }
            set { SetTagFloatValue(GEDCOMTagType.LONG, value); }
        }


        public GEDCOMMap(GEDCOMObject owner) : base(owner)
        {
            SetName(GEDCOMTagType.MAP);
        }

        public GEDCOMMap(GEDCOMObject owner, string tagName, string tagValue) : this(owner)
        {
            SetNameValue(tagName, tagValue);
        }

        public new static GEDCOMTag Create(GEDCOMObject owner, string tagName, string tagValue)
        {
            return new GEDCOMMap(owner, tagName, tagValue);
        }

        /// <summary>
        /// It is agreed to follow the requirements of the GEDCOM standard:
        /// PLACE_LATITUDE must be represented as [Ngg.nnnnnn (+) | Sgg.nnnnnn (-)]
        /// PLACE_LONGITUDE must be represented as [Wgg.nnnnnn (-) | Egg.nnnnnn (+)]
        /// </summary>
        private static double GetGeoCoord(string value, GeoCoord coordType)
        {
            if (string.IsNullOrEmpty(value)) {
                return 0.0;
            }

            int sign = 1;
            char firstChr = value[0];
            if ("NSWE".IndexOf(firstChr) >= 0) {
                switch (firstChr) {
                    case 'N':
                    case 'E':
                        sign = +1;
                        break;

                    case 'S':
                    case 'W':
                        sign = -1;
                        break;
                }
                value = value.Substring(1);
            }

            double result = ConvertHelper.ParseFloat(value, 0.0);
            return result * sign;
        }
    }
}
