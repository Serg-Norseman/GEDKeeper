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
    /// GoogleHybridMap provider
    /// </summary>
    public class GoogleHybridMapProvider : GoogleMapProviderBase
    {
        public static readonly GoogleHybridMapProvider Instance = new GoogleHybridMapProvider();

        private static readonly string UrlFormatServer = "mt";
        private static readonly string UrlFormatRequest = "vt";
        private static readonly string UrlFormat = "http://{0}{1}.{10}/maps/{2}/lyrs={3}&hl={4}&x={5}{6}&y={7}&z={8}&s={9}";

        public string Version = "h@333000000";

        private readonly Guid fId = new Guid("B076C255-6D12-4466-AAE0-4A73D20A7E6A");
        private readonly string fName = "GoogleHybridMap";
        private GMapProvider[] fOverlays;

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

        public override GMapProvider[] Overlays
        {
            get {
                if (fOverlays == null) {
                    fOverlays = new GMapProvider[] { GoogleSatelliteMapProvider.Instance, this };
                }
                return fOverlays;
            }
        }

        private GoogleHybridMapProvider()
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
