/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Net;
using System.Xml;
using BSLib;

namespace GKCore.Maps
{
    public sealed class GoogleGeocoder : IGeocoder
    {
        private const string REQUEST_URL = "https://maps.googleapis.com/maps/api/geocode/xml?address={0}&language={1}";

        public GoogleGeocoder()
        {
            CultureInfo cInfo = CultureInfo.CurrentUICulture;
            fLang = cInfo.TwoLetterISOLanguageName;
        }

        public override IList<GeoPoint> Geocode(string location, short results, string lang, string region)
        {
            string requestUrl =
                string.Format(REQUEST_URL, MakeValidString(location), lang) +
                (string.IsNullOrEmpty(fKey) ? string.Empty : "&key=" + fKey);

            if (!string.IsNullOrEmpty(region)) {
                requestUrl += string.Format("&region={0}", region);
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
                            if (xNode.Name == "result") {
                                var addressNode = xNode["formatted_address"];
                                var geometry = xNode["geometry"];
                                var pointNode = geometry["location"];

                                if (addressNode != null && pointNode != null) {
                                    double ptLongitude = ConvertHelper.ParseFloat(pointNode["lng"].InnerText, -1.0);
                                    double ptLatitude = ConvertHelper.ParseFloat(pointNode["lat"].InnerText, -1.0);

                                    if (ptLatitude != -1.0 && ptLongitude != -1.0) {
                                        string ptHint = addressNode.InnerText;
                                        geoObjects.Add(new GeoPoint(ptLatitude, ptLongitude, ptHint));
                                    }
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
