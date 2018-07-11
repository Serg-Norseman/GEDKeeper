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

using System.Collections.Generic;
using System.Net;

namespace GKCore.Maps
{
    public abstract class IGeocoder
    {
        protected string fKey;
        protected IWebProxy fProxy;
        protected string fLang;

        protected IGeocoder()
        {
            fKey = string.Empty;
        }

        /// <summary>
        /// Yandex/Google Maps API-key (not necessarily for Yandex)
        /// </summary>
        public void SetKey(string apiKey)
        {
            fKey = apiKey;
        }

        public void SetProxy(IWebProxy proxy)
        {
            fProxy = proxy;
        }

        public void SetLang(string lang)
        {
            fLang = lang;
        }

        protected static string MakeValidString(string location)
        {
            location = location.Replace(" ", "+").Replace("&", "").Replace("?", "");
            return location;
        }

        public IList<GeoPoint> Geocode(string location, short results)
        {
            return Geocode(location, results, fLang);
        }

        /// <summary>
        /// Location determination by name, indicating the quantity of objects to return and preference language.
        /// </summary>
        /// <param name="location">Name of a geographic location.</param>
        /// <param name="results">Maximum number of objects to return.</param>
        /// <param name="lang">Preference language for describing objects.</param>
        /// <example>Geocode("Moscow", 10, "en_US");</example>
        /// <returns>Collection of found locations</returns>
        public abstract IList<GeoPoint> Geocode(string location, short results, string lang);


        public static IGeocoder Create(string type)
        {
            switch (type) {
                case "Google":
                    return new GoogleGeocoder();

                case "Yandex":
                    return new YandexGeocoder();

                case "OSM":
                    return new OSMGeocoder();

                default:
                    return new GoogleGeocoder();
            }
        }
    }
}
