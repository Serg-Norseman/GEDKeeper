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
    /// YandexSatelliteMap provider
    /// </summary>
    public class YandexSatelliteMapProvider : YandexMapProviderBase
    {
        public static readonly YandexSatelliteMapProvider Instance = new YandexSatelliteMapProvider();

        private static readonly string UrlServer = "sat";
        private static readonly string UrlFormat = "http://{0}0{1}.{7}/tiles?l=sat&v={2}&x={3}&y={4}&z={5}&lang={6}";

        public new string Version = "3.135.0";

        private readonly Guid fId = new Guid("2D4CE763-0F91-40B2-A511-13EF428237AD");
        private readonly string fName = "YandexSatelliteMap";

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

        private YandexSatelliteMapProvider()
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
