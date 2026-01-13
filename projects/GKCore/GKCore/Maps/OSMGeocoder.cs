/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Net;
using System.Xml;
using BSLib;

namespace GKCore.Maps
{
    public sealed class OSMGeocoder : IGeocoder
    {
        private const string REQUEST_URL = "https://nominatim.openstreetmap.org/search.php?q={0}&format=xml&accept-language={1}";

        public OSMGeocoder()
        {
            CultureInfo cInfo = CultureInfo.CurrentUICulture;
            fLang = cInfo.TwoLetterISOLanguageName;
        }

        public override IList<GeoPoint> Geocode(string location, short results, string lang, string region)
        {
            string requestUrl =
                string.Format(REQUEST_URL, MakeValidString(location), lang);

            if (!string.IsNullOrEmpty(region)) {
                requestUrl += string.Format("&countrycodes={0}", region);
            }

            return ParseXml(requestUrl);
        }

        private IList<GeoPoint> ParseXml(string url)
        {
            var geoObjects = new List<GeoPoint>();

            var request = (HttpWebRequest)WebRequest.CreateDefault(new Uri(url));
            request.ContentType = "application/x-www-form-urlencoded";
            request.Credentials = CredentialCache.DefaultCredentials;
            request.Proxy = fProxy;
            request.UserAgent = "GK Geocoder";

            using (var response = (HttpWebResponse)request.GetResponse()) {
                using (var stream = response.GetResponseStream()) {
                    var xmlDocument = new XmlDocument();
                    xmlDocument.Load(stream);
                    var node = xmlDocument.DocumentElement;

                    if (node != null && node.ChildNodes.Count > 0) {
                        int num = node.ChildNodes.Count;
                        for (int i = 0; i < num; i++) {
                            var xNode = node.ChildNodes[i];
                            if (xNode.Name == "place") {
                                string placeClass = xNode.Attributes["class"].InnerText;
                                string placeType = xNode.Attributes["type"].InnerText;

                                bool valid = (placeClass == "place" || (placeClass == "boundary" && placeType == "administrative"));
                                if (!valid) continue;

                                double ptLongitude = ConvertHelper.ParseFloat(xNode.Attributes["lon"].InnerText, -1.0);
                                double ptLatitude = ConvertHelper.ParseFloat(xNode.Attributes["lat"].InnerText, -1.0);

                                if (ptLatitude != -1.0 && ptLongitude != -1.0) {
                                    string ptHint = xNode.Attributes["display_name"].InnerText;
                                    geoObjects.Add(new GeoPoint(ptLatitude, ptLongitude, ptHint));
                                }
                            }
                        }
                    }
                }
            }

            return geoObjects;
        }
    }
}
