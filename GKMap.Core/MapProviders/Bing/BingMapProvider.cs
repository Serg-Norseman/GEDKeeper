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
using System.Net;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Xml;

namespace GKMap.MapProviders.Bing
{
    public abstract class BingMapProviderBase : GMapProvider, IGeocodingProvider
    {
        // http://dev.virtualearth.net/REST/v1/Locations/1%20Microsoft%20Way%20Redmond%20WA%2098052?o=xml&key=BingMapsKey
        private static readonly string GeocoderUrlFormat = "http://dev.virtualearth.net/REST/v1/Locations?{0}&o=xml&key={1}";

        public string Version = "4810";

        /// <summary>
        /// Bing Maps Customer Identification.
        /// |
        /// FOR LEGAL AND COMMERCIAL USAGE SET YOUR OWN REGISTERED KEY
        /// |
        /// http://msdn.microsoft.com/en-us/library/ff428642.aspx
        /// </summary>
        public string ClientKey = string.Empty;

        internal string SessionId = string.Empty;

        /// <summary>
        /// set true to append SessionId on requesting tiles
        /// </summary>
        public bool ForceSessionIdOnTileAccess = false;

        /// <summary>
        /// set true to avoid using dynamic tile url format
        /// </summary>
        public bool DisableDynamicTileUrlFormat = false;

        public bool TryCorrectVersion = true;

        /// <summary>
        /// set false to use your own key. 
        /// FOR LEGAL AND COMMERCIAL USAGE SET YOUR OWN REGISTERED KEY
        /// http://msdn.microsoft.com/en-us/library/ff428642.aspx
        /// </summary>
        public bool TryGetDefaultKey = true;

        private static bool fInit;
        private GMapProvider[] fOverlays;


        public override Guid Id
        {
            get {
                throw new NotImplementedException();
            }
        }

        public override string Name
        {
            get {
                throw new NotImplementedException();
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


        protected BingMapProviderBase()
        {
            MaxZoom = null;
            RefererUrl = "http://www.bing.com/maps/";
            Copyright = string.Format("©{0} Microsoft Corporation, ©{0} NAVTEQ, ©{0} Image courtesy of NASA", DateTime.Today.Year);
        }

        /// <summary>
        /// Converts tile XY coordinates into a QuadKey at a specified level of detail.
        /// </summary>
        /// <param name="tileX">Tile X coordinate.</param>
        /// <param name="tileY">Tile Y coordinate.</param>
        /// <param name="levelOfDetail">Level of detail, from 1 (lowest detail)
        /// to 23 (highest detail).</param>
        /// <returns>A string containing the QuadKey.</returns>       
        protected string TileXYToQuadKey(long tileX, long tileY, int levelOfDetail)
        {
            StringBuilder quadKey = new StringBuilder();
            for (int i = levelOfDetail; i > 0; i--) {
                char digit = '0';
                int mask = 1 << (i - 1);
                if ((tileX & mask) != 0) {
                    digit++;
                }
                if ((tileY & mask) != 0) {
                    digit++;
                    digit++;
                }
                quadKey.Append(digit);
            }
            return quadKey.ToString();
        }

        /// <summary>
        /// Converts a QuadKey into tile XY coordinates.
        /// </summary>
        /// <param name="quadKey">QuadKey of the tile.</param>
        /// <param name="tileX">Output parameter receiving the tile X coordinate.</param>
        /// <param name="tileY">Output parameter receiving the tile Y coordinate.</param>
        /// <param name="levelOfDetail">Output parameter receiving the level of detail.</param>
        protected void QuadKeyToTileXY(string quadKey, out int tileX, out int tileY, out int levelOfDetail)
        {
            tileX = tileY = 0;
            levelOfDetail = quadKey.Length;
            for (int i = levelOfDetail; i > 0; i--) {
                int mask = 1 << (i - 1);
                switch (quadKey[levelOfDetail - i]) {
                    case '0':
                        break;

                    case '1':
                        tileX |= mask;
                        break;

                    case '2':
                        tileY |= mask;
                        break;

                    case '3':
                        tileX |= mask;
                        tileY |= mask;
                        break;

                    default:
                        throw new ArgumentException("Invalid QuadKey digit sequence.");
                }
            }
        }

        public override PureImage GetTileImage(GPoint pos, int zoom)
        {
            throw new NotImplementedException();
        }

        public override void OnInitialized()
        {
            if (!fInit) {
                try {
                    var key = ClientKey;

                    // to avoid registration stuff, default key
                    if (TryGetDefaultKey && string.IsNullOrEmpty(ClientKey)) {
                        //old: Vx8dmDflxzT02jJUG8bEjMU07Xr9QWRpPTeRuAZTC1uZFQdDCvK/jUbHKdyHEWj4LvccTPoKofDHtzHsWu/0xuo5u2Y9rj88
                        key = Stuff.GString("Jq7FrGTyaYqcrvv9ugBKv4OVSKnmzpigqZtdvtcDdgZexmOZ2RugOexFSmVzTAhOWiHrdhFoNCoySnNF3MyyIOo5u2Y9rj88");
                    }

                    #region -- try get sesion key --
                    if (!string.IsNullOrEmpty(key)) {
                        string keyResponse = GMaps.Instance.UseUrlCache ? Cache.Instance.GetContent("BingLoggingServiceV1" + key, CacheType.UrlCache, TimeSpan.FromHours(GMapProvider.TTLCache)) : string.Empty;

                        if (string.IsNullOrEmpty(keyResponse)) {
                            // Bing Maps WPF Control
                            // http://dev.virtualearth.net/webservices/v1/LoggingService/LoggingService.svc/Log?entry=0&auth={0}&fmt=1&type=3&group=MapControl&name=WPF&version=1.0.0.0&session=00000000-0000-0000-0000-000000000000&mkt=en-US

                            keyResponse = GetContentUsingHttp(string.Format("http://dev.virtualearth.net/webservices/v1/LoggingService/LoggingService.svc/Log?entry=0&fmt=1&type=3&group=MapControl&name=AJAX&mkt=en-us&auth={0}&jsonp=microsoftMapsNetworkCallback", key));

                            if (!string.IsNullOrEmpty(keyResponse) && keyResponse.Contains("ValidCredentials")) {
                                if (GMaps.Instance.UseUrlCache) {
                                    Cache.Instance.SaveContent("BingLoggingServiceV1" + key, CacheType.UrlCache, keyResponse);
                                }
                            }
                        }

                        if (!string.IsNullOrEmpty(keyResponse) && keyResponse.Contains("sessionId") && keyResponse.Contains("ValidCredentials")) {
                            // microsoftMapsNetworkCallback({"sessionId" : "xxx", "authenticationResultCode" : "ValidCredentials"})

                            SessionId = keyResponse.Split(',')[0].Split(':')[1].Replace("\"", string.Empty).Replace(" ", string.Empty);
                            Debug.WriteLine("GMapProviders.BingMap.SessionId: " + SessionId);
                        } else {
                            Debug.WriteLine("BingLoggingServiceV1: " + keyResponse);
                        }
                    }
                    #endregion

                    // supporting old road
                    if (TryCorrectVersion && DisableDynamicTileUrlFormat) {
                        #region -- get the version --
                        string url = @"http://www.bing.com/maps";
                        string html = GMaps.Instance.UseUrlCache ? Cache.Instance.GetContent(url, CacheType.UrlCache, TimeSpan.FromDays(7)) : string.Empty;

                        if (string.IsNullOrEmpty(html)) {
                            html = GetContentUsingHttp(url);
                            if (!string.IsNullOrEmpty(html) && GMaps.Instance.UseUrlCache) {
                                Cache.Instance.SaveContent(url, CacheType.UrlCache, html);
                            }
                        }

                        if (!string.IsNullOrEmpty(html)) {
                            #region -- match versions --

                            Regex reg = new Regex("tilegeneration:(\\d*)", RegexOptions.IgnoreCase);
                            Match mat = reg.Match(html);
                            if (mat.Success) {
                                GroupCollection gc = mat.Groups;
                                int count = gc.Count;
                                if (count == 2) {
                                    string ver = gc[1].Value;
                                    string old = GMapProviders.BingMap.Version;
                                    if (ver != old) {
                                        GMapProviders.BingMap.Version = ver;
                                        GMapProviders.BingSatelliteMap.Version = ver;
                                        GMapProviders.BingHybridMap.Version = ver;
                                        GMapProviders.BingOSMap.Version = ver;
#if DEBUG
                                        Debug.WriteLine("GMapProviders.BingMap.Version: " + ver + ", old: " + old + ", consider updating source");
                                        if (Debugger.IsAttached) {
                                            Thread.Sleep(5555);
                                        }
#endif
                                    } else {
                                        Debug.WriteLine("GMapProviders.BingMap.Version: " + ver + ", OK");
                                    }
                                }
                            }
                            #endregion
                        }
                        #endregion
                    }

                    fInit = true; // try it only once
                } catch (Exception ex) {
                    Debug.WriteLine("TryCorrectBingVersions failed: " + ex);
                }
            }
        }

        protected override bool CheckTileImageHttpResponse(WebResponse response)
        {
            var pass = base.CheckTileImageHttpResponse(response);
            if (pass) {
                var tileInfo = response.Headers.Get("X-VE-Tile-Info");
                if (tileInfo != null) {
                    return !tileInfo.Equals("no-tile");
                }
            }
            return pass;
        }

        protected string GetTileUrl(string imageryType)
        {
            //Retrieve map tile URL from the Imagery Metadata service: http://msdn.microsoft.com/en-us/library/ff701716.aspx
            //This ensures that the current tile URL is always used. 
            //This will prevent the app from breaking when the map tiles change.

            string ret = string.Empty;
            if (!string.IsNullOrEmpty(SessionId)) {
                try {
                    string url = "http://dev.virtualearth.net/REST/V1/Imagery/Metadata/" + imageryType + "?output=xml&key=" + SessionId;

                    string r = GMaps.Instance.UseUrlCache ? Cache.Instance.GetContent("GetTileUrl" + imageryType, CacheType.UrlCache, TimeSpan.FromDays(7)) : string.Empty;
                    bool cache = false;

                    if (string.IsNullOrEmpty(r)) {
                        r = GetContentUsingHttp(url);
                        cache = true;
                    }

                    if (!string.IsNullOrEmpty(r)) {
                        XmlDocument doc = new XmlDocument();
                        doc.LoadXml(r);

                        XmlNode xn = doc["Response"];
                        string statuscode = xn["StatusCode"].InnerText;
                        if (string.Compare(statuscode, "200", true) == 0) {
                            xn = xn["ResourceSets"]["ResourceSet"]["Resources"];
                            XmlNodeList xnl = xn.ChildNodes;
                            foreach (XmlNode xno in xnl) {
                                XmlNode imageUrl = xno["ImageUrl"];

                                if (imageUrl != null && !string.IsNullOrEmpty(imageUrl.InnerText)) {
                                    if (cache && GMaps.Instance.UseUrlCache) {
                                        Cache.Instance.SaveContent("GetTileUrl" + imageryType, CacheType.UrlCache, r);
                                    }

                                    var baseTileUrl = imageUrl.InnerText;

                                    if (baseTileUrl.Contains("{key}") || baseTileUrl.Contains("{token}")) {
                                        baseTileUrl = baseTileUrl.Replace("{key}", SessionId).Replace("{token}", SessionId);
                                    } else if (ForceSessionIdOnTileAccess) {
                                        // haven't seen anyone doing that, yet? ;/                            
                                        baseTileUrl += "&key=" + SessionId;
                                    }

                                    Debug.WriteLine("GetTileUrl, UrlFormat[" + imageryType + "]: " + baseTileUrl);

                                    ret = baseTileUrl;
                                    break;
                                }
                            }
                        }
                    }
                } catch (Exception ex) {
                    Debug.WriteLine("GetTileUrl: Error getting Bing Maps tile URL - " + ex);
                }
            }
            return ret;
        }

        public GeocoderStatusCode GetPoints(string keywords, out List<PointLatLng> pointList)
        {
            //Escape keywords to better handle special characters.
            return GetLatLngFromGeocoderUrl(MakeGeocoderUrl("q=" + Uri.EscapeDataString(keywords)), out pointList);
        }

        public PointLatLng? GetPoint(string keywords, out GeocoderStatusCode status)
        {
            List<PointLatLng> pointList;
            status = GetPoints(keywords, out pointList);
            return pointList != null && pointList.Count > 0 ? pointList[0] : (PointLatLng?)null;
        }

        public GeocoderStatusCode GetPoints(Placemark placemark, out List<PointLatLng> pointList)
        {
            return GetLatLngFromGeocoderUrl(MakeGeocoderDetailedUrl(placemark), out pointList);
        }

        public PointLatLng? GetPoint(Placemark placemark, out GeocoderStatusCode status)
        {
            List<PointLatLng> pointList;
            status = GetLatLngFromGeocoderUrl(MakeGeocoderDetailedUrl(placemark), out pointList);
            return pointList != null && pointList.Count > 0 ? pointList[0] : (PointLatLng?)null;
        }

        private string MakeGeocoderDetailedUrl(Placemark placemark)
        {
            string parameters = string.Empty;

            if (!AddFieldIfNotEmpty(ref parameters, "countryRegion", placemark.CountryNameCode))
                AddFieldIfNotEmpty(ref parameters, "countryRegion", placemark.CountryName);

            AddFieldIfNotEmpty(ref parameters, "adminDistrict", placemark.DistrictName);
            AddFieldIfNotEmpty(ref parameters, "locality", placemark.LocalityName);
            AddFieldIfNotEmpty(ref parameters, "postalCode", placemark.PostalCodeNumber);

            if (!string.IsNullOrEmpty(placemark.HouseNo))
                AddFieldIfNotEmpty(ref parameters, "addressLine", placemark.ThoroughfareName + " " + placemark.HouseNo);
            else
                AddFieldIfNotEmpty(ref parameters, "addressLine", placemark.ThoroughfareName);

            return MakeGeocoderUrl(parameters);
        }

        private bool AddFieldIfNotEmpty(ref string Input, string FieldName, string Value)
        {
            if (!string.IsNullOrEmpty(Value)) {
                if (string.IsNullOrEmpty(Input))
                    Input = string.Empty;
                else
                    Input = Input + "&";

                Input = Input + FieldName + "=" + Value;

                return true;
            }
            return false;
        }

        public GeocoderStatusCode GetPlacemarks(PointLatLng location, out List<Placemark> placemarkList)
        {
            // http://msdn.microsoft.com/en-us/library/ff701713.aspx
            throw new NotImplementedException();
        }

        public Placemark? GetPlacemark(PointLatLng location, out GeocoderStatusCode status)
        {
            // http://msdn.microsoft.com/en-us/library/ff701713.aspx
            throw new NotImplementedException();
        }

        private string MakeGeocoderUrl(string keywords)
        {
            return string.Format(CultureInfo.InvariantCulture, GeocoderUrlFormat, keywords, SessionId);
        }

        private GeocoderStatusCode GetLatLngFromGeocoderUrl(string url, out List<PointLatLng> pointList)
        {
            GeocoderStatusCode status;
            pointList = null;

            try {
                string geo = GMaps.Instance.UseGeocoderCache ? Cache.Instance.GetContent(url, CacheType.GeocoderCache) : string.Empty;

                bool cache = false;

                if (string.IsNullOrEmpty(geo)) {
                    geo = GetContentUsingHttp(url);

                    if (!string.IsNullOrEmpty(geo)) {
                        cache = true;
                    }
                }

                status = GeocoderStatusCode.Unknown;
                if (!string.IsNullOrEmpty(geo)) {
                    if (geo.StartsWith("<?xml") && geo.Contains("<Response")) {
                        XmlDocument doc = new XmlDocument();
                        doc.LoadXml(geo);
                        XmlNode xn = doc["Response"];
                        string statuscode = xn["StatusCode"].InnerText;
                        switch (statuscode) {
                            case "200": {
                                    pointList = new List<PointLatLng>();
                                    xn = xn["ResourceSets"]["ResourceSet"]["Resources"];
                                    XmlNodeList xnl = xn.ChildNodes;
                                    foreach (XmlNode xno in xnl) {
                                        XmlNode latitude = xno["Point"]["Latitude"];
                                        XmlNode longitude = xno["Point"]["Longitude"];
                                        pointList.Add(new PointLatLng(Double.Parse(latitude.InnerText, CultureInfo.InvariantCulture),
                                                                      Double.Parse(longitude.InnerText, CultureInfo.InvariantCulture)));
                                    }

                                    if (pointList.Count > 0) {
                                        status = GeocoderStatusCode.Success;
                                        if (cache && GMaps.Instance.UseGeocoderCache) {
                                            Cache.Instance.SaveContent(url, CacheType.GeocoderCache, geo);
                                        }
                                        break;
                                    }

                                    status = GeocoderStatusCode.UnknownAddress;
                                    break;
                                }

                            case "400":
                                status = GeocoderStatusCode.BadRequest;
                                break; // bad request, The request contained an error.
                            case "401":
                                status = GeocoderStatusCode.BadKey;
                                break; // Unauthorized, Access was denied. You may have entered your credentials incorrectly, or you might not have access to the requested resource or operation.
                            case "403":
                                status = GeocoderStatusCode.BadRequest;
                                break; // Forbidden, The request is for something forbidden. Authorization will not help.
                            case "404":
                                status = GeocoderStatusCode.UnknownAddress;
                                break; // Not Found, The requested resource was not found. 
                            case "500":
                                status = GeocoderStatusCode.ServerError;
                                break; // Internal Server Error, Your request could not be completed because there was a problem with the service.
                            case "501":
                                status = GeocoderStatusCode.Unknown;
                                break; // Service Unavailable, There's a problem with the service right now. Please try again later.
                            default:
                                status = GeocoderStatusCode.Unknown;
                                break; // unknown, for possible future error codes
                        }
                    }
                }
            } catch (Exception ex) {
                status = GeocoderStatusCode.ExceptionInCode;
                Debug.WriteLine("GetLatLngFromGeocoderUrl: " + ex);
            }

            return status;
        }
    }

    /// <summary>
    /// BingMapProvider provider
    /// </summary>
    public class BingMapProvider : BingMapProviderBase
    {
        // http://ecn.t0.tiles.virtualearth.net/tiles/r120030?g=875&mkt=en-us&lbl=l1&stl=h&shading=hill&n=z
        private static readonly string UrlFormat = "http://ecn.t{0}.tiles.virtualearth.net/tiles/r{1}?g={2}&mkt={3}&lbl=l1&stl=h&shading=hill&n=z{4}";

        private readonly Guid fId = new Guid("D0CEB371-F10A-4E12-A2C1-DF617D6674A8");
        private readonly string fName = "BingMap";
        private string fUrlDynamicFormat = string.Empty;

        public static readonly BingMapProvider Instance = new BingMapProvider();


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

        private BingMapProvider()
        {
        }

        public override PureImage GetTileImage(GPoint pos, int zoom)
        {
            string url = MakeTileImageUrl(pos, zoom, LanguageStr);

            return GetTileImageUsingHttp(url);
        }

        public override void OnInitialized()
        {
            base.OnInitialized();

            if (!DisableDynamicTileUrlFormat) {
                //UrlFormat[Road]: http://ecn.{subdomain}.tiles.virtualearth.net/tiles/r{quadkey}.jpeg?g=3179&mkt={culture}&shading=hill

                fUrlDynamicFormat = GetTileUrl("Road");
                if (!string.IsNullOrEmpty(fUrlDynamicFormat)) {
                    fUrlDynamicFormat = fUrlDynamicFormat.Replace("{subdomain}", "t{0}").Replace("{quadkey}", "{1}").Replace("{culture}", "{2}");
                }
            }
        }

        private string MakeTileImageUrl(GPoint pos, int zoom, string language)
        {
            string key = TileXYToQuadKey(pos.X, pos.Y, zoom);

            if (!DisableDynamicTileUrlFormat && !string.IsNullOrEmpty(fUrlDynamicFormat)) {
                return string.Format(fUrlDynamicFormat, GetServerNum(pos, 4), key, language);
            }

            return string.Format(UrlFormat, GetServerNum(pos, 4), key, Version, language, ForceSessionIdOnTileAccess ? "&key=" + SessionId : string.Empty);
        }
    }
}
