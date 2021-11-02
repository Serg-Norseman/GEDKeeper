/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Xml;

namespace GKMap.MapProviders.Google
{
    public abstract class GoogleMapProviderBase : GMapProvider, IGeocodingProvider
    {
        public readonly string ServerAPIs /* ;}~~ */ = Stuff.GString(/*{^_^}*/"9gERyvblybF8iMuCt/LD6w=="/*d{'_'}b*/);
        public readonly string Server /* ;}~~~~ */ = Stuff.GString(/*{^_^}*/"gosr2U13BoS+bXaIxt6XWg=="/*d{'_'}b*/);

        public string SecureWord = "Galileo";

        /// <summary>
        /// Your application's API key, obtained from the Google Developers Console.
        /// This key identifies your application for purposes of quota management. 
        /// Must provide either API key or Maps for Work credentials.
        /// </summary>
        public string ApiKey = string.Empty;
        public bool TryCorrectVersion = true;

        private static bool fInit;
        private byte[] fPrivateKeyBytes;
        private string fClientId = string.Empty;
        private GMapProvider[] fOverlays;

        /// <summary>
        /// Your client ID. To access the special features of the Google Maps API for Work
        /// you must provide a client ID when accessing any of the API libraries or services.
        /// When registering for Google Google Maps API for Work you will receive this client ID
        /// from Enterprise Support. All client IDs begin with a gme- prefix.
        /// </summary>
        public string ClientId
        {
            get {
                return fClientId;
            }
        }

        public override PureProjection Projection
        {
            get {
                return MercatorProjection.Instance;
            }
        }

        public override GMapProvider[] Overlays
        {
            get {
                if (fOverlays == null) {
                    fOverlays = new GMapProvider[] { this };
                }
                return fOverlays;
            }
        }


        protected GoogleMapProviderBase()
        {
            MaxZoom = null;
            RefererUrl = string.Format("https://maps.{0}/", Server);
            Copyright = string.Format("©{0} Google - Map data ©{0} Tele Atlas, Imagery ©{0} TerraMetrics", DateTime.Today.Year);
        }

        public override void OnInitialized()
        {
            if (!fInit && TryCorrectVersion) {
                string url = string.Format("https://maps.{0}/maps/api/js?client=google-maps-lite&amp;libraries=search&amp;language=en&amp;region=", ServerAPIs);
                try {
                    string html = GMaps.Instance.GetContent(url, CacheType.UrlCache, TimeSpan.FromHours(GMapProvider.TTLCache));

                    if (string.IsNullOrEmpty(html)) {
                        html = GetContentUsingHttp(url);
                        if (!string.IsNullOrEmpty(html)) {
                            GMaps.Instance.SaveContent(url, CacheType.UrlCache, html);
                        }
                    }

                    if (!string.IsNullOrEmpty(html)) {
                        #region -- match versions --
                        Regex reg = new Regex(string.Format(@"https?://mts?\d.{0}/maps/vt\?lyrs=m@(\d*)", Server), RegexOptions.IgnoreCase);
                        Match mat = reg.Match(html);
                        if (mat.Success) {
                            GroupCollection gc = mat.Groups;
                            int count = gc.Count;
                            if (count > 0) {
                                string ver = string.Format("m@{0}", gc[1].Value);
                                string old = GMapProviders.GoogleMap.Version;

                                GMapProviders.GoogleMap.Version = ver;

                                string verh = string.Format("h@{0}", gc[1].Value);
                                string oldh = GMapProviders.GoogleHybridMap.Version;

                                GMapProviders.GoogleHybridMap.Version = verh;

#if DEBUG
                                Debug.WriteLine("GMapProviders.GoogleMap.Version: " + ver + ", " + (ver == old ? "OK" : "old: " + old + ", consider updating source"));
                                Debug.WriteLine("GMapProviders.GoogleHybridMap.Version: " + verh + ", " + (verh == oldh ? "OK" : "old: " + oldh + ", consider updating source"));

                                if (Debugger.IsAttached && ver != old) {
                                    Thread.Sleep(1111);
                                }
#endif
                            }
                        }

                        reg = new Regex(string.Format(@"https?://khms?\d.{0}/kh\?v=(\d*)", Server), RegexOptions.IgnoreCase);
                        mat = reg.Match(html);
                        if (mat.Success) {
                            GroupCollection gc = mat.Groups;
                            int count = gc.Count;
                            if (count > 0) {
                                string ver = gc[1].Value;
                                string old = GMapProviders.GoogleSatelliteMap.Version;

                                GMapProviders.GoogleSatelliteMap.Version = ver;
#if DEBUG
                                Debug.WriteLine("GMapProviders.GoogleSatelliteMap.Version: " + ver + ", " + (ver == old ? "OK" : "old: " + old + ", consider updating source"));
                                if (Debugger.IsAttached && ver != old) {
                                    Thread.Sleep(1111);
                                }
#endif
                            }
                        }

                        reg = new Regex(string.Format(@"https?://mts?\d.{0}/maps/vt\?lyrs=t@(\d*),r@(\d*)", Server), RegexOptions.IgnoreCase);
                        mat = reg.Match(html);
                        if (mat.Success) {
                            GroupCollection gc = mat.Groups;
                            int count = gc.Count;
                            if (count > 1) {
                                string ver = string.Format("t@{0},r@{1}", gc[1].Value, gc[2].Value);
                                string old = GMapProviders.GoogleTerrainMap.Version;

                                GMapProviders.GoogleTerrainMap.Version = ver;
#if DEBUG
                                Debug.WriteLine("GMapProviders.GoogleTerrainMap.Version: " + ver + ", " + (ver == old ? "OK" : "old: " + old + ", consider updating source"));
                                if (Debugger.IsAttached && ver != old) {
                                    Thread.Sleep(1111);
                                }
#endif
                            }
                        }
                        #endregion
                    }

                    fInit = true; // try it only once
                } catch (Exception ex) {
                    Debug.WriteLine("TryCorrectGoogleVersions failed: " + ex);
                }
            }
        }

        internal void GetSecureWords(GPoint pos, out string sec1, out string sec2)
        {
            sec1 = string.Empty; // after &x=...
            sec2 = string.Empty; // after &zoom=...
            int seclen = (int)((pos.X * 3) + pos.Y) % 8;
            sec2 = SecureWord.Substring(0, seclen);
            if (pos.Y >= 10000 && pos.Y < 100000) {
                sec1 = Sec1;
            }
        }

        private static readonly string Sec1 = "&s=";

        public GeocoderStatusCode GetPoints(string keywords, out List<PointLatLng> pointList)
        {
            return GetLatLngFromGeocoderUrl(MakeGeocoderUrl(keywords, LanguageStr), out pointList);
        }

        public PointLatLng? GetPoint(string keywords, out GeocoderStatusCode status)
        {
            List<PointLatLng> pointList;
            status = GetPoints(keywords, out pointList);
            return pointList != null && pointList.Count > 0 ? pointList[0] : (PointLatLng?)null;
        }

        /// <summary>
        /// NotImplemented
        /// </summary>
        /// <param name="placemark"></param>
        /// <param name="pointList"></param>
        /// <returns></returns>
        public GeocoderStatusCode GetPoints(Placemark placemark, out List<PointLatLng> pointList)
        {
            throw new NotImplementedException("use GetPoints(string keywords...");
        }

        /// <summary>
        /// NotImplemented
        /// </summary>
        /// <param name="placemark"></param>
        /// <param name="status"></param>
        /// <returns></returns>
        public PointLatLng? GetPoint(Placemark placemark, out GeocoderStatusCode status)
        {
            throw new NotImplementedException("use GetPoint(string keywords...");
        }

        public GeocoderStatusCode GetPlacemarks(PointLatLng location, out List<Placemark> placemarkList)
        {
            return GetPlacemarkFromReverseGeocoderUrl(MakeReverseGeocoderUrl(location, LanguageStr), out placemarkList);
        }

        public Placemark? GetPlacemark(PointLatLng location, out GeocoderStatusCode status)
        {
            List<Placemark> pointList;
            status = GetPlacemarks(location, out pointList);
            return pointList != null && pointList.Count > 0 ? pointList[0] : (Placemark?)null;
        }

        // The Google Geocoding API: http://tinyurl.com/cdlj889

        private string MakeGeocoderUrl(string keywords, string language)
        {
            return string.Format(CultureInfo.InvariantCulture, GeocoderUrlFormat, ServerAPIs, Uri.EscapeDataString(keywords).Replace(' ', '+'), language);
        }

        private string MakeReverseGeocoderUrl(PointLatLng pt, string language)
        {
            return string.Format(CultureInfo.InvariantCulture, ReverseGeocoderUrlFormat, ServerAPIs, pt.Lat, pt.Lng, language);
        }

        private GeocoderStatusCode GetLatLngFromGeocoderUrl(string url, out List<PointLatLng> pointList)
        {
            var status = GeocoderStatusCode.Unknown;
            pointList = null;

            try {
                string geo = GMaps.Instance.GetContent(url, CacheType.GeocoderCache);

                bool cache = false;

                if (string.IsNullOrEmpty(geo)) {
                    string urls = url;

                    // Must provide either API key or Maps for Work credentials.
                    if (!string.IsNullOrEmpty(ClientId)) {
                        urls = GetSignedUri(url);
                    } else if (!string.IsNullOrEmpty(ApiKey)) {
                        urls += "&key=" + ApiKey;
                    }

                    geo = GetContentUsingHttp(urls);

                    if (!string.IsNullOrEmpty(geo)) {
                        cache = true;
                    }
                }

                if (!string.IsNullOrEmpty(geo)) {
                    if (geo.StartsWith("<?xml")) {
                        #region -- xml response --
                        //<?xml version="1.0" encoding="UTF-8"?>
                        //<GeocodeResponse>
                        // <status>OK</status>
                        // <result>
                        //  <type>locality</type>
                        //  <type>political</type>
                        //  <formatted_address>Vilnius, Lithuania</formatted_address>
                        //  <address_component>
                        //   <long_name>Vilnius</long_name>
                        //   <short_name>Vilnius</short_name>
                        //   <type>locality</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilniaus miesto savivaldyb??</long_name>
                        //   <short_name>Vilniaus m. sav.</short_name>
                        //   <type>administrative_area_level_2</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilnius County</long_name>
                        //   <short_name>Vilnius County</short_name>
                        //   <type>administrative_area_level_1</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Lithuania</long_name>
                        //   <short_name>LT</short_name>
                        //   <type>country</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <geometry>
                        //   <location>
                        //    <lat>54.6871555</lat>
                        //    <lng>25.2796514</lng>
                        //   </location>
                        //   <location_type>APPROXIMATE</location_type>
                        //   <viewport>
                        //    <southwest>
                        //     <lat>54.5677980</lat>
                        //     <lng>25.0243760</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>54.8325440</lat>
                        //     <lng>25.4814883</lng>
                        //    </northeast>
                        //   </viewport>
                        //   <bounds>
                        //    <southwest>
                        //     <lat>54.5677980</lat>
                        //     <lng>25.0243760</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>54.8325440</lat>
                        //     <lng>25.4814883</lng>
                        //    </northeast>
                        //   </bounds>
                        //  </geometry>
                        // </result>
                        // <result>
                        //  <type>airport</type>
                        //  <type>transit_station</type>
                        //  <type>establishment</type>
                        //  <formatted_address>Vilnius International Airport (VNO), 10A, Vilnius, Lithuania</formatted_address>
                        //  <address_component>
                        //   <long_name>Vilnius International Airport</long_name>
                        //   <short_name>Vilnius International Airport</short_name>
                        //   <type>establishment</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>10A</long_name>
                        //   <short_name>10A</short_name>
                        //   <type>street_number</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilnius</long_name>
                        //   <short_name>Vilnius</short_name>
                        //   <type>locality</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilniaus miesto savivaldyb??</long_name>
                        //   <short_name>Vilniaus m. sav.</short_name>
                        //   <type>administrative_area_level_2</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilnius County</long_name>
                        //   <short_name>Vilnius County</short_name>
                        //   <type>administrative_area_level_1</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Lithuania</long_name>
                        //   <short_name>LT</short_name>
                        //   <type>country</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <geometry>
                        //   <location>
                        //    <lat>54.6369440</lat>
                        //    <lng>25.2877780</lng>
                        //   </location>
                        //   <location_type>APPROXIMATE</location_type>
                        //   <viewport>
                        //    <southwest>
                        //     <lat>54.6158331</lat>
                        //     <lng>25.2723832</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>54.6538331</lat>
                        //     <lng>25.3034219</lng>
                        //    </northeast>
                        //   </viewport>
                        //   <bounds>
                        //    <southwest>
                        //     <lat>54.6158331</lat>
                        //     <lng>25.2723832</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>54.6538331</lat>
                        //     <lng>25.3034219</lng>
                        //    </northeast>
                        //   </bounds>
                        //  </geometry>
                        // </result>
                        //</GeocodeResponse>

                        #endregion

                        XmlDocument doc = new XmlDocument();
                        doc.LoadXml(geo);

                        XmlNode nn = doc.SelectSingleNode("//status");
                        if (nn != null) {
                            if (nn.InnerText != "OK") {
                                Debug.WriteLine("GetLatLngFromGeocoderUrl: " + nn.InnerText);
                            } else {
                                status = GeocoderStatusCode.Success;

                                if (cache) {
                                    GMaps.Instance.SaveContent(url, CacheType.GeocoderCache, geo);
                                }

                                pointList = new List<PointLatLng>();

                                XmlNodeList l = doc.SelectNodes("//result");
                                if (l != null) {
                                    foreach (XmlNode n in l) {
                                        nn = n.SelectSingleNode("geometry/location/lat");
                                        if (nn != null) {
                                            double lat = double.Parse(nn.InnerText, CultureInfo.InvariantCulture);

                                            nn = n.SelectSingleNode("geometry/location/lng");
                                            if (nn != null) {
                                                double lng = double.Parse(nn.InnerText, CultureInfo.InvariantCulture);
                                                pointList.Add(new PointLatLng(lat, lng));
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            } catch (Exception ex) {
                status = GeocoderStatusCode.ExceptionInCode;
                Debug.WriteLine("GetLatLngFromGeocoderUrl: " + ex);
            }

            return status;
        }

        private GeocoderStatusCode GetPlacemarkFromReverseGeocoderUrl(string url, out List<Placemark> placemarkList)
        {
            GeocoderStatusCode status = GeocoderStatusCode.Unknown;
            placemarkList = null;

            try {
                string reverse = GMaps.Instance.GetContent(url, CacheType.PlacemarkCache);

                bool cache = false;

                if (string.IsNullOrEmpty(reverse)) {
                    string urls = url;

                    // Must provide either API key or Maps for Work credentials.
                    if (!string.IsNullOrEmpty(ClientId)) {
                        urls = GetSignedUri(url);
                    } else if (!string.IsNullOrEmpty(ApiKey)) {
                        urls += "&key=" + ApiKey;
                    }

                    reverse = GetContentUsingHttp(urls);

                    if (!string.IsNullOrEmpty(reverse)) {
                        cache = true;
                    }
                }

                if (!string.IsNullOrEmpty(reverse)) {
                    if (reverse.StartsWith("<?xml")) {
                        #region -- xml response --
                        //<?xml version="1.0" encoding="UTF-8"?>
                        //<GeocodeResponse>
                        // <status>OK</status>
                        // <result>
                        //  <type>street_address</type>
                        //  <formatted_address>Tuskul??n?? gatv?? 2, Vilnius 09213, Lithuania</formatted_address>
                        //  <address_component>
                        //   <long_name>2</long_name>
                        //   <short_name>2</short_name>
                        //   <type>street_number</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Tuskul??n?? gatv??</long_name>
                        //   <short_name>Tuskul??n?? g.</short_name>
                        //   <type>route</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilnius</long_name>
                        //   <short_name>Vilnius</short_name>
                        //   <type>locality</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilniaus miesto savivaldyb??</long_name>
                        //   <short_name>Vilniaus m. sav.</short_name>
                        //   <type>administrative_area_level_2</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilnius County</long_name>
                        //   <short_name>Vilnius County</short_name>
                        //   <type>administrative_area_level_1</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Lithuania</long_name>
                        //   <short_name>LT</short_name>
                        //   <type>country</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>09213</long_name>
                        //   <short_name>09213</short_name>
                        //   <type>postal_code</type>
                        //  </address_component>
                        //  <geometry>
                        //   <location>
                        //    <lat>54.6963339</lat>
                        //    <lng>25.2968939</lng>
                        //   </location>
                        //   <location_type>ROOFTOP</location_type>
                        //   <viewport>
                        //    <southwest>
                        //     <lat>54.6949849</lat>
                        //     <lng>25.2955449</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>54.6976829</lat>
                        //     <lng>25.2982429</lng>
                        //    </northeast>
                        //   </viewport>
                        //  </geometry>
                        // </result>
                        // <result>
                        //  <type>postal_code</type>
                        //  <formatted_address>Vilnius 09213, Lithuania</formatted_address>
                        //  <address_component>
                        //   <long_name>09213</long_name>
                        //   <short_name>09213</short_name>
                        //   <type>postal_code</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilnius</long_name>
                        //   <short_name>Vilnius</short_name>
                        //   <type>locality</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilniaus miesto savivaldyb??</long_name>
                        //   <short_name>Vilniaus m. sav.</short_name>
                        //   <type>administrative_area_level_2</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilnius County</long_name>
                        //   <short_name>Vilnius County</short_name>
                        //   <type>administrative_area_level_1</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Lithuania</long_name>
                        //   <short_name>LT</short_name>
                        //   <type>country</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <geometry>
                        //   <location>
                        //    <lat>54.6963032</lat>
                        //    <lng>25.2967390</lng>
                        //   </location>
                        //   <location_type>APPROXIMATE</location_type>
                        //   <viewport>
                        //    <southwest>
                        //     <lat>54.6950889</lat>
                        //     <lng>25.2958851</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>54.6977869</lat>
                        //     <lng>25.2985830</lng>
                        //    </northeast>
                        //   </viewport>
                        //   <bounds>
                        //    <southwest>
                        //     <lat>54.6956179</lat>
                        //     <lng>25.2958871</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>54.6972579</lat>
                        //     <lng>25.2985810</lng>
                        //    </northeast>
                        //   </bounds>
                        //  </geometry>
                        // </result>
                        // <result>
                        //  <type>neighborhood</type>
                        //  <type>political</type>
                        //  <formatted_address>??irm??nai, Vilnius, Lithuania</formatted_address>
                        //  <address_component>
                        //   <long_name>??irm??nai</long_name>
                        //   <short_name>??irm??nai</short_name>
                        //   <type>neighborhood</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilnius</long_name>
                        //   <short_name>Vilnius</short_name>
                        //   <type>locality</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilniaus miesto savivaldyb??</long_name>
                        //   <short_name>Vilniaus m. sav.</short_name>
                        //   <type>administrative_area_level_2</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilnius County</long_name>
                        //   <short_name>Vilnius County</short_name>
                        //   <type>administrative_area_level_1</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Lithuania</long_name>
                        //   <short_name>LT</short_name>
                        //   <type>country</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <geometry>
                        //   <location>
                        //    <lat>54.7117424</lat>
                        //    <lng>25.2974345</lng>
                        //   </location>
                        //   <location_type>APPROXIMATE</location_type>
                        //   <viewport>
                        //    <southwest>
                        //     <lat>54.6888939</lat>
                        //     <lng>25.2838700</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>54.7304441</lat>
                        //     <lng>25.3133630</lng>
                        //    </northeast>
                        //   </viewport>
                        //   <bounds>
                        //    <southwest>
                        //     <lat>54.6888939</lat>
                        //     <lng>25.2838700</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>54.7304441</lat>
                        //     <lng>25.3133630</lng>
                        //    </northeast>
                        //   </bounds>
                        //  </geometry>
                        // </result>
                        // <result>
                        //  <type>administrative_area_level_3</type>
                        //  <type>political</type>
                        //  <formatted_address>??irm??n?? seni??nija, Lithuania</formatted_address>
                        //  <address_component>
                        //   <long_name>??irm??n?? seni??nija</long_name>
                        //   <short_name>??irm??n?? sen.</short_name>
                        //   <type>administrative_area_level_3</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilniaus miesto savivaldyb??</long_name>
                        //   <short_name>Vilniaus m. sav.</short_name>
                        //   <type>administrative_area_level_2</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilnius County</long_name>
                        //   <short_name>Vilnius County</short_name>
                        //   <type>administrative_area_level_1</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Lithuania</long_name>
                        //   <short_name>LT</short_name>
                        //   <type>country</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <geometry>
                        //   <location>
                        //    <lat>54.7117424</lat>
                        //    <lng>25.2974345</lng>
                        //   </location>
                        //   <location_type>APPROXIMATE</location_type>
                        //   <viewport>
                        //    <southwest>
                        //     <lat>54.6892135</lat>
                        //     <lng>25.2837150</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>54.7305878</lat>
                        //     <lng>25.3135630</lng>
                        //    </northeast>
                        //   </viewport>
                        //   <bounds>
                        //    <southwest>
                        //     <lat>54.6892135</lat>
                        //     <lng>25.2837150</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>54.7305878</lat>
                        //     <lng>25.3135630</lng>
                        //    </northeast>
                        //   </bounds>
                        //  </geometry>
                        // </result>
                        // <result>
                        //  <type>locality</type>
                        //  <type>political</type>
                        //  <formatted_address>Vilnius, Lithuania</formatted_address>
                        //  <address_component>
                        //   <long_name>Vilnius</long_name>
                        //   <short_name>Vilnius</short_name>
                        //   <type>locality</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilniaus miesto savivaldyb??</long_name>
                        //   <short_name>Vilniaus m. sav.</short_name>
                        //   <type>administrative_area_level_2</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilnius County</long_name>
                        //   <short_name>Vilnius County</short_name>
                        //   <type>administrative_area_level_1</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Lithuania</long_name>
                        //   <short_name>LT</short_name>
                        //   <type>country</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <geometry>
                        //   <location>
                        //    <lat>54.6871555</lat>
                        //    <lng>25.2796514</lng>
                        //   </location>
                        //   <location_type>APPROXIMATE</location_type>
                        //   <viewport>
                        //    <southwest>
                        //     <lat>54.5677980</lat>
                        //     <lng>25.0243760</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>54.8325440</lat>
                        //     <lng>25.4814883</lng>
                        //    </northeast>
                        //   </viewport>
                        //   <bounds>
                        //    <southwest>
                        //     <lat>54.5677980</lat>
                        //     <lng>25.0243760</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>54.8325440</lat>
                        //     <lng>25.4814883</lng>
                        //    </northeast>
                        //   </bounds>
                        //  </geometry>
                        // </result>
                        // <result>
                        //  <type>administrative_area_level_2</type>
                        //  <type>political</type>
                        //  <formatted_address>Vilniaus miesto savivaldyb??, Lithuania</formatted_address>
                        //  <address_component>
                        //   <long_name>Vilniaus miesto savivaldyb??</long_name>
                        //   <short_name>Vilniaus m. sav.</short_name>
                        //   <type>administrative_area_level_2</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Vilnius County</long_name>
                        //   <short_name>Vilnius County</short_name>
                        //   <type>administrative_area_level_1</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Lithuania</long_name>
                        //   <short_name>LT</short_name>
                        //   <type>country</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <geometry>
                        //   <location>
                        //    <lat>54.6759715</lat>
                        //    <lng>25.2867413</lng>
                        //   </location>
                        //   <location_type>APPROXIMATE</location_type>
                        //   <viewport>
                        //    <southwest>
                        //     <lat>54.5677980</lat>
                        //     <lng>25.0243760</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>54.8325440</lat>
                        //     <lng>25.4814883</lng>
                        //    </northeast>
                        //   </viewport>
                        //   <bounds>
                        //    <southwest>
                        //     <lat>54.5677980</lat>
                        //     <lng>25.0243760</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>54.8325440</lat>
                        //     <lng>25.4814883</lng>
                        //    </northeast>
                        //   </bounds>
                        //  </geometry>
                        // </result>
                        // <result>
                        //  <type>administrative_area_level_1</type>
                        //  <type>political</type>
                        //  <formatted_address>Vilnius County, Lithuania</formatted_address>
                        //  <address_component>
                        //   <long_name>Vilnius County</long_name>
                        //   <short_name>Vilnius County</short_name>
                        //   <type>administrative_area_level_1</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <address_component>
                        //   <long_name>Lithuania</long_name>
                        //   <short_name>LT</short_name>
                        //   <type>country</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <geometry>
                        //   <location>
                        //    <lat>54.8086502</lat>
                        //    <lng>25.2182138</lng>
                        //   </location>
                        //   <location_type>APPROXIMATE</location_type>
                        //   <viewport>
                        //    <southwest>
                        //     <lat>54.1276599</lat>
                        //     <lng>24.3863751</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>55.5174369</lat>
                        //     <lng>26.7602130</lng>
                        //    </northeast>
                        //   </viewport>
                        //   <bounds>
                        //    <southwest>
                        //     <lat>54.1276599</lat>
                        //     <lng>24.3863751</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>55.5174369</lat>
                        //     <lng>26.7602130</lng>
                        //    </northeast>
                        //   </bounds>
                        //  </geometry>
                        // </result>
                        // <result>
                        //  <type>country</type>
                        //  <type>political</type>
                        //  <formatted_address>Lithuania</formatted_address>
                        //  <address_component>
                        //   <long_name>Lithuania</long_name>
                        //   <short_name>LT</short_name>
                        //   <type>country</type>
                        //   <type>political</type>
                        //  </address_component>
                        //  <geometry>
                        //   <location>
                        //    <lat>55.1694380</lat>
                        //    <lng>23.8812750</lng>
                        //   </location>
                        //   <location_type>APPROXIMATE</location_type>
                        //   <viewport>
                        //    <southwest>
                        //     <lat>53.8968787</lat>
                        //     <lng>20.9543679</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>56.4503209</lat>
                        //     <lng>26.8355913</lng>
                        //    </northeast>
                        //   </viewport>
                        //   <bounds>
                        //    <southwest>
                        //     <lat>53.8968787</lat>
                        //     <lng>20.9543679</lng>
                        //    </southwest>
                        //    <northeast>
                        //     <lat>56.4503209</lat>
                        //     <lng>26.8355913</lng>
                        //    </northeast>
                        //   </bounds>
                        //  </geometry>
                        // </result>
                        //</GeocodeResponse>

                        #endregion

                        XmlDocument doc = new XmlDocument();
                        doc.LoadXml(reverse);

                        XmlNode nn = doc.SelectSingleNode("//status");
                        if (nn != null) {
                            if (nn.InnerText != "OK") {
                                Debug.WriteLine("GetPlacemarkFromReverseGeocoderUrl: " + nn.InnerText);
                            } else {
                                status = GeocoderStatusCode.Success;

                                if (cache) {
                                    GMaps.Instance.SaveContent(url, CacheType.PlacemarkCache, reverse);
                                }

                                placemarkList = new List<Placemark>();

                                #region -- placemarks --
                                XmlNodeList l = doc.SelectNodes("//result");
                                if (l != null) {
                                    foreach (XmlNode n in l) {
                                        Debug.WriteLine("---------------------");

                                        nn = n.SelectSingleNode("formatted_address");
                                        if (nn != null) {
                                            var ret = new Placemark(nn.InnerText);

                                            Debug.WriteLine("formatted_address: [" + nn.InnerText + "]");

                                            nn = n.SelectSingleNode("type");
                                            if (nn != null) {
                                                Debug.WriteLine("type: " + nn.InnerText);
                                            }

                                            // TODO: fill Placemark details

                                            XmlNodeList acl = n.SelectNodes("address_component");
                                            foreach (XmlNode ac in acl) {
                                                nn = ac.SelectSingleNode("type");
                                                if (nn != null) {
                                                    var type = nn.InnerText;
                                                    Debug.Write(" - [" + type + "], ");

                                                    nn = ac.SelectSingleNode("long_name");
                                                    if (nn != null) {
                                                        Debug.WriteLine("long_name: [" + nn.InnerText + "]");

                                                        switch (type) {
                                                            case "street_address": {
                                                                    ret.StreetNumber = nn.InnerText;
                                                                }
                                                                break;

                                                            case "route": {
                                                                    ret.ThoroughfareName = nn.InnerText;
                                                                }
                                                                break;

                                                            case "postal_code": {
                                                                    ret.PostalCodeNumber = nn.InnerText;
                                                                }
                                                                break;

                                                            case "country": {
                                                                    ret.CountryName = nn.InnerText;
                                                                }
                                                                break;

                                                            case "locality": {
                                                                    ret.LocalityName = nn.InnerText;
                                                                }
                                                                break;

                                                            case "administrative_area_level_2": {
                                                                    ret.DistrictName = nn.InnerText;
                                                                }
                                                                break;

                                                            case "administrative_area_level_1": {
                                                                    ret.AdministrativeAreaName = nn.InnerText;
                                                                }
                                                                break;

                                                            case "administrative_area_level_3": {
                                                                    ret.SubAdministrativeAreaName = nn.InnerText;
                                                                }
                                                                break;

                                                            case "neighborhood": {
                                                                    ret.Neighborhood = nn.InnerText;
                                                                }
                                                                break;
                                                        }
                                                    }
                                                }
                                            }

                                            placemarkList.Add(ret);
                                        }
                                    }
                                }
                                #endregion
                            }
                        }
                    }
                }
            } catch (Exception ex) {
                status = GeocoderStatusCode.ExceptionInCode;
                placemarkList = null;
                Debug.WriteLine("GetPlacemarkReverseGeocoderUrl: " + ex);
            }

            return status;
        }

        // Note: Use of API Key requires https's queries.
        static readonly string ReverseGeocoderUrlFormat = "https://maps.{0}/maps/api/geocode/xml?latlng={1},{2}&language={3}&sensor=false";
        static readonly string GeocoderUrlFormat = "https://maps.{0}/maps/api/geocode/xml?address={1}&language={2}&sensor=false";

        /// <summary>
        /// https://developers.google.com/maps/documentation/business/webservices/auth#how_do_i_get_my_signing_key
        /// To access the special features of the Google Maps API for Work you must provide a client ID
        /// when accessing any of the API libraries or services.
        /// When registering for Google Google Maps API for Work you will receive this client ID from Enterprise Support.
        /// All client IDs begin with a gme- prefix. Your client ID is passed as the value of the client parameter.
        /// Generally, you should store your private key someplace safe and read them into your code
        /// </summary>
        /// <param name="clientId"></param>
        /// <param name="privateKey"></param>
        public void SetEnterpriseCredentials(string clientId, string privateKey)
        {
            privateKey = privateKey.Replace("-", "+").Replace("_", "/");
            fPrivateKeyBytes = Convert.FromBase64String(privateKey);
            fClientId = clientId;
        }

        private string GetSignedUri(Uri uri)
        {
            var builder = new UriBuilder(uri);
            builder.Query = builder.Query.Substring(1) + "&client=" + fClientId;
            uri = builder.Uri;
            string signature = GetSignature(uri);

            return uri.Scheme + "://" + uri.Host + uri.LocalPath + uri.Query + "&signature=" + signature;
        }

        private string GetSignedUri(string url)
        {
            return GetSignedUri(new Uri(url));
        }

        private string GetSignature(Uri uri)
        {
            byte[] encodedPathQuery = Encoding.ASCII.GetBytes(uri.LocalPath + uri.Query);
            using (var hashAlgorithm = new HMACSHA1(fPrivateKeyBytes)) {
                byte[] hashed = hashAlgorithm.ComputeHash(encodedPathQuery);
                return Convert.ToBase64String(hashed).Replace("+", "-").Replace("/", "_");
            }
        }
    }

    /// <summary>
    /// GoogleMap provider
    /// </summary>
    public class GoogleMapProvider : GoogleMapProviderBase
    {
        public static readonly GoogleMapProvider Instance = new GoogleMapProvider();

        private static readonly string UrlFormatServer = "mt";
        private static readonly string UrlFormatRequest = "vt";
        private static readonly string UrlFormat = "http://{0}{1}.{10}/maps/{2}/lyrs={3}&hl={4}&x={5}{6}&y={7}&z={8}&s={9}";

        public string Version = "m@333000000";

        private readonly Guid fId = new Guid("D7287DA0-A7FF-405F-8166-B6BAF26D066C");
        private readonly string fName = "GoogleMap";

        public override Guid Id
        {
            get {
                return fId;
            }
        }

        public override string Name
        {
            get {
                return fName;
            }
        }

        private GoogleMapProvider()
        {
        }

        public override PureImage GetTileImage(GPoint pos, int zoom)
        {
            string url = MakeTileImageUrl(pos, zoom, LanguageStr);

            return GetTileImageUsingHttp(url);
        }

        private string MakeTileImageUrl(GPoint pos, int zoom, string language)
        {
            string sec1; // after &x=...
            string sec2; // after &zoom=...
            GetSecureWords(pos, out sec1, out sec2);

            return string.Format(UrlFormat, UrlFormatServer, GetServerNum(pos, 4), UrlFormatRequest, Version, language, pos.X, sec1, pos.Y, zoom, sec2, Server);
        }
    }
}
