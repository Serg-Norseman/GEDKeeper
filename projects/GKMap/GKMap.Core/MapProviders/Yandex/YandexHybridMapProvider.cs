/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;

namespace GKMap.MapProviders.Yandex
{
    /// <summary>
    /// YandexHybridMap provider
    /// </summary>
    public class YandexHybridMapProvider : YandexMapProviderBase
    {
        public static readonly YandexHybridMapProvider Instance = new YandexHybridMapProvider();

        private static readonly string UrlServer = "vec";
        private static readonly string UrlFormat = "https://{0}0{1}.{7}/tiles?l=skl&v={2}&x={3}&y={4}&z={5}&lang={6}";

        private readonly Guid fId = new Guid("78A3830F-5EE3-432C-A32E-91B7AF6BBCB9");
        private readonly string fName = "YandexHybridMap";
        private GMapProvider[] fOverlays;

        public override Guid Id
        {
            get { return fId; }
        }

        public override string Name
        {
            get { return fName; }
        }

        public override GMapProvider[] Overlays
        {
            get {
                if (fOverlays == null) {
                    fOverlays = new GMapProvider[] { YandexSatelliteMapProvider.Instance, this };
                }
                return fOverlays;
            }
        }

        private YandexHybridMapProvider()
        {
        }

        public override PureImage GetTileImage(GPoint pos, int zoom)
        {
            string url = MakeTileImageUrl(pos, zoom, LanguageStr);

            return GetTileImageUsingHttp(url);
        }

        private string MakeTileImageUrl(GPoint pos, int zoom, string language)
        {
            return string.Format(UrlFormat, UrlServer, GetServerNum(pos, 4) + 1, Version, pos.X, pos.Y, zoom, language, Server);
        }
    }
}
