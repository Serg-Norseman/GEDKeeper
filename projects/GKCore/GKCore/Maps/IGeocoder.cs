/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
        protected string fRegion;

        protected IGeocoder()
        {
            fKey = string.Empty;
        }

        /// <summary>
        /// Yandex/Google Maps API-key
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

        public void SetRegion(string region)
        {
            fRegion = region;
        }

        protected static string MakeValidString(string location)
        {
            location = location.Replace(" ", "+").Replace("&", "").Replace("?", "");
            return location;
        }

        public IList<GeoPoint> Geocode(string location, short results)
        {
            GKUtils.InitSecurityProtocol();

            return Geocode(location, results, fLang, fRegion);
        }

        /// <summary>
        /// Location determination by name, indicating the quantity of objects to return and preference language.
        /// </summary>
        /// <param name="location">Name of a geographic location.</param>
        /// <param name="results">Maximum number of objects to return.</param>
        /// <param name="lang">Preference language for describing objects.</param>
        /// <example>Geocode("Moscow", 10, "en_US");</example>
        /// <returns>Collection of found locations</returns>
        public abstract IList<GeoPoint> Geocode(string location, short results, string lang, string region);


        public static IGeocoder Create(string type, string region)
        {
            IGeocoder result;
            switch (type) {
                case "Yandex":
                    result = new YandexGeocoder();
                    result.SetKey(GKData.YAPI_KEY);
                    break;

                case "OSM":
                    result = new OSMGeocoder();
                    break;

                case "Google":
                default:
                    result = new GoogleGeocoder();
                    result.SetKey(GKData.GAPI_KEY);
                    break;
            }
            result.SetRegion(region);
            return result;
        }
    }
}
