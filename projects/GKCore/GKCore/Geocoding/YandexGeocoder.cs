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
using System.Globalization;
using System.IO;
using System.Net;
using System.Xml;

namespace GKCore.Geocoding
{
    public sealed class YandexGeocoder : IGeocoder
    {
        private const string REQUEST_URL = "http://geocode-maps.yandex.ru/1.x/?geocode={0}&format=xml&results={1}&lang={2}";

        public YandexGeocoder()
        {
            CultureInfo cInfo = CultureInfo.CurrentUICulture;
            fLang = cInfo.Name;
        }

        public override IList<GeoPoint> Geocode(string location, short results, string lang)
        {
            string requestUrl = string.Format(REQUEST_URL, MakeValidString(location), results, lang);

            return ParseXml(requestUrl);
        }

        private static IList<GeoPoint> ParseXml(string url)
        {
            List<GeoPoint> geoObjects = new List<GeoPoint>();

            WebRequest request = WebRequest.Create(url);
            request.Credentials = CredentialCache.DefaultCredentials;
            using (HttpWebResponse response = (HttpWebResponse)request.GetResponse()) {
                using (Stream dataStream = response.GetResponseStream()) {
                    XmlDocument doc = new XmlDocument();
                    doc.Load(dataStream);

                    XmlNamespaceManager ns = new XmlNamespaceManager(doc.NameTable);
                    ns.AddNamespace("ns", "http://maps.yandex.ru/ymaps/1.x");
                    ns.AddNamespace("opengis", "http://www.opengis.net/gml");
                    ns.AddNamespace("geocoder", "http://maps.yandex.ru/geocoder/1.x");

                    XmlNodeList nodes = doc.SelectNodes("//ns:ymaps/ns:GeoObjectCollection/opengis:featureMember/ns:GeoObject", ns);
                    foreach (XmlNode node in nodes)
                    {
                        var pointNode = node.SelectSingleNode("opengis:Point/opengis:pos", ns);
                        var metaNode = node.SelectSingleNode("opengis:metaDataProperty/geocoder:GeocoderMetaData", ns);

                        string[] splitted = pointNode.InnerText.Split(new char[] { ' ' }, 2);
                        double lng = double.Parse(splitted[0], CultureInfo.InvariantCulture);
                        double lat = double.Parse(splitted[1], CultureInfo.InvariantCulture);

                        string ptHint = (metaNode == null || metaNode["text"] == null) ? string.Empty : metaNode["text"].InnerText;

                        GeoPoint gpt = new GeoPoint(lat, lng, ptHint);
                        geoObjects.Add(gpt);
                    }
                }
            }

            return geoObjects;
        }
    }
}
