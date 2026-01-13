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

namespace GKCore.Maps
{
    public sealed class YandexGeocoder : IGeocoder
    {
        private const string REQUEST_URL = "https://geocode-maps.yandex.ru/1.x/?geocode={0}&format=xml";

        public YandexGeocoder()
        {
            CultureInfo cInfo = CultureInfo.CurrentUICulture;
            fLang = cInfo.Name;
        }

        public override IList<GeoPoint> Geocode(string location, short results, string lang, string region)
        {
            string requestUrl = 
                string.Format(REQUEST_URL, MakeValidString(location)) +
                (string.IsNullOrEmpty(fKey) ? string.Empty : "&apikey=" + fKey);

            requestUrl += string.Format("&lang={0}", lang);
            if (!string.IsNullOrEmpty(region)) {
                requestUrl += string.Format("_{0}", region);
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
                using (var dataStream = response.GetResponseStream()) {
                    var doc = new XmlDocument();
                    doc.Load(dataStream);

                    var ns = new XmlNamespaceManager(doc.NameTable);
                    ns.AddNamespace("ns", "http://maps.yandex.ru/ymaps/1.x");
                    ns.AddNamespace("opengis", "http://www.opengis.net/gml");
                    ns.AddNamespace("geocoder", "http://maps.yandex.ru/geocoder/1.x");

                    var nodes = doc.SelectNodes("//ns:ymaps/ns:GeoObjectCollection/opengis:featureMember/ns:GeoObject", ns);
                    foreach (XmlNode node in nodes) {
                        var pointNode = node.SelectSingleNode("opengis:Point/opengis:pos", ns);
                        if (pointNode == null) continue;

                        var metaNode = node.SelectSingleNode("opengis:metaDataProperty/geocoder:GeocoderMetaData", ns);
                        if (metaNode == null) continue;

                        var kindNode = metaNode["kind"];
                        if (kindNode.InnerText != "locality") continue;

                        string[] splitted = pointNode.InnerText.Split(new char[] { ' ' }, 2);
                        double lng = double.Parse(splitted[0], CultureInfo.InvariantCulture);
                        double lat = double.Parse(splitted[1], CultureInfo.InvariantCulture);

                        string ptHint = (metaNode["text"] == null) ? string.Empty : metaNode["text"].InnerText;
                        geoObjects.Add(new GeoPoint(lat, lng, ptHint));
                    }
                }
            }

            return geoObjects;
        }
    }
}
