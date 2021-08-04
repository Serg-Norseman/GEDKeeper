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
    /// BingHybridMap provider
    /// </summary>
    public class BingHybridMapProvider : BingMapProviderBase
    {
        // http://ecn.dynamic.t3.tiles.virtualearth.net/comp/CompositionHandler/12030012020203?mkt=en-us&it=A,G,L&n=z
        private static readonly string UrlFormat = "http://ecn.t{0}.tiles.virtualearth.net/tiles/h{1}.jpeg?g={2}&mkt={3}&n=z{4}";

        private readonly Guid fId = new Guid("94E2FCB4-CAAC-45EA-A1F9-8147C4B14970");
        private readonly string fName = "BingHybridMap";
        private string fUrlDynamicFormat = string.Empty;

        public static readonly BingHybridMapProvider Instance;

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

        private BingHybridMapProvider()
        {
        }

        static BingHybridMapProvider()
        {
            Instance = new BingHybridMapProvider();
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
                //UrlFormat[AerialWithLabels]: http://ecn.{subdomain}.tiles.virtualearth.net/tiles/h{quadkey}.jpeg?g=3179&mkt={culture}

                fUrlDynamicFormat = GetTileUrl("AerialWithLabels");
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
