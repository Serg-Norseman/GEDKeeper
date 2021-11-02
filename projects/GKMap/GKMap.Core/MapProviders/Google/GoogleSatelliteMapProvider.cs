/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;

namespace GKMap.MapProviders.Google
{
    /// <summary>
    /// GoogleSatelliteMap provider
    /// </summary>
    public class GoogleSatelliteMapProvider : GoogleMapProviderBase
    {
        public static readonly GoogleSatelliteMapProvider Instance = new GoogleSatelliteMapProvider();

        private static readonly string UrlFormatServer = "khm";
        private static readonly string UrlFormatRequest = "kh";
        private static readonly string UrlFormat = "http://{0}{1}.{10}/{2}/v={3}&hl={4}&x={5}{6}&y={7}&z={8}&s={9}";

        public string Version = "192";

        private readonly Guid fId = new Guid("9CB89D76-67E9-47CF-8137-B9EE9FC46388");
        private readonly string fName = "GoogleSatelliteMap";

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

        private GoogleSatelliteMapProvider()
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
