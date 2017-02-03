/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.IO;
using System.Net;
using System.Xml;

using GKCommon;

namespace GKCore.Geocoding
{
    public sealed class GoogleGeocoder : IGeocoder
    {
        private const string REQUEST_URL = "https://maps.googleapis.com/maps/api/geocode/xml?address={0}&language={1}";

        public GoogleGeocoder()
        {
            CultureInfo cInfo = CultureInfo.CurrentUICulture;
            fLang = cInfo.TwoLetterISOLanguageName;
        }

        public override IList<GeoPoint> Geocode(string location, short results, string lang)
        {
            string requestUrl =
                string.Format(REQUEST_URL, MakeValidString(location), lang) +
                (string.IsNullOrEmpty(fKey) ? string.Empty : "&key=" + fKey);

            return ParseXml(requestUrl);
        }

        private IList<GeoPoint> ParseXml(string url)
        {
            List<GeoPoint> geoObjects = new List<GeoPoint>();

            HttpWebRequest request = (HttpWebRequest)WebRequest.CreateDefault(new Uri(url));
            request.ContentType = "application/x-www-form-urlencoded";
            request.Proxy = fProxy;

            using (HttpWebResponse response = (HttpWebResponse)request.GetResponse()) {
                using (Stream stream = response.GetResponseStream()) {

                    XmlDocument xmlDocument = new XmlDocument();
                    xmlDocument.Load(stream);
                    XmlNode node = xmlDocument.DocumentElement;

                    if (node != null && node.ChildNodes.Count > 0)
                    {
                        int num = node.ChildNodes.Count;
                        for (int i = 0; i < num; i++)
                        {
                            XmlNode xNode = node.ChildNodes[i];
                            if (xNode.Name == "result")
                            {
                                XmlNode addressNode = xNode["formatted_address"];
                                XmlNode geometry = xNode["geometry"];
                                XmlNode pointNode = geometry["location"];

                                if (addressNode != null && pointNode != null)
                                {
                                    string ptHint = addressNode.InnerText;
                                    double ptLongitude = SysUtils.ParseFloat(pointNode["lng"].InnerText, -1.0);
                                    double ptLatitude = SysUtils.ParseFloat(pointNode["lat"].InnerText, -1.0);

                                    if (ptLatitude != -1.0 && ptLongitude != -1.0)
                                    {
                                        GeoPoint gpt = new GeoPoint(ptLatitude, ptLongitude, ptHint);
                                        geoObjects.Add(gpt);
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
