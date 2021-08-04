/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;

namespace GKMap.MapProviders.Bing
{
    /// <summary>
    /// BingSatelliteMapProvider provider
    /// </summary>
    public class BingSatelliteMapProvider : BingMapProviderBase
    {
        // http://ecn.t1.tiles.virtualearth.net/tiles/a12030003131321231.jpeg?g=875&mkt=en-us&n=z
        private static readonly string UrlFormat = "http://ecn.t{0}.tiles.virtualearth.net/tiles/a{1}.jpeg?g={2}&mkt={3}&n=z{4}";

        private readonly Guid fId = new Guid("3AC742DD-966B-4CFB-B67D-33E7F82F2D37");
        private readonly string fName = "BingSatelliteMap";
        private string fUrlDynamicFormat = string.Empty;

        public static readonly BingSatelliteMapProvider Instance;

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

        private BingSatelliteMapProvider()
        {
        }

        static BingSatelliteMapProvider()
        {
            Instance = new BingSatelliteMapProvider();
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
                //UrlFormat[Aerial]: http://ecn.{subdomain}.tiles.virtualearth.net/tiles/a{quadkey}.jpeg?g=3179

                fUrlDynamicFormat = GetTileUrl("Aerial");
                if (!string.IsNullOrEmpty(fUrlDynamicFormat)) {
                    fUrlDynamicFormat = fUrlDynamicFormat.Replace("{subdomain}", "t{0}").Replace("{quadkey}", "{1}");
                }
            }
        }

        private string MakeTileImageUrl(GPoint pos, int zoom, string language)
        {
            string key = TileXYToQuadKey(pos.X, pos.Y, zoom);

            if (!DisableDynamicTileUrlFormat && !string.IsNullOrEmpty(fUrlDynamicFormat)) {
                return string.Format(fUrlDynamicFormat, GetServerNum(pos, 4), key);
            }

            return string.Format(UrlFormat, GetServerNum(pos, 4), key, Version, language, ForceSessionIdOnTileAccess ? "&key=" + SessionId : string.Empty);
        }
    }
}
