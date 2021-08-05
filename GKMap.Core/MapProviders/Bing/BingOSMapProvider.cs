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
    /// BingOSMapProvider provider
    /// </summary>
    public class BingOSMapProvider : BingMapProviderBase
    {
        // http://ecn.t1.tiles.virtualearth.net/tiles/r12030003131321231.jpeg?g=875&mkt=en-us&n=z&productSet=mmOS
        private static readonly string UrlFormat = "http://ecn.t{0}.tiles.virtualearth.net/tiles/r{1}.jpeg?g={2}&mkt={3}&n=z{4}&productSet=mmOS";

        private readonly Guid fId = new Guid("3C12C212-A79F-42D0-9A1B-22740E1103E8");
        private readonly string fName = "BingOSMap";
        private string fUrlDynamicFormat = string.Empty;

        public static readonly BingOSMapProvider Instance = new BingOSMapProvider();

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

        private BingOSMapProvider()
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
                //UrlFormat[OrdnanceSurvey]: http://ecn.{subdomain}.tiles.virtualearth.net/tiles/r{quadkey}.jpeg?g=3179&productSet=mmOS

                fUrlDynamicFormat = GetTileUrl("OrdnanceSurvey");
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
