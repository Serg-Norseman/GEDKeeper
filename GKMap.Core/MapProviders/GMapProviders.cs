/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.Collections.Generic;
using System.Net;
using GKMap.MapProviders.Bing;
using GKMap.MapProviders.Etc;
using GKMap.MapProviders.Google;
using GKMap.MapProviders.OpenStreetMap;
using GKMap.MapProviders.Yandex;

namespace GKMap.MapProviders
{
    /// <summary>
    /// providers that are already build in
    /// </summary>
    public class GMapProviders
    {
        public static readonly EmptyProvider EmptyProvider = EmptyProvider.Instance;

        public static readonly OpenStreetMapProvider OpenStreetMap = OpenStreetMapProvider.Instance;
        public static readonly OpenStreet4UMapProvider OpenStreet4UMap = OpenStreet4UMapProvider.Instance;

        public static readonly BingMapProvider BingMap = BingMapProvider.Instance;
        public static readonly BingSatelliteMapProvider BingSatelliteMap = BingSatelliteMapProvider.Instance;
        public static readonly BingHybridMapProvider BingHybridMap = BingHybridMapProvider.Instance;
        public static readonly BingOSMapProvider BingOSMap = BingOSMapProvider.Instance;

        public static readonly GoogleMapProvider GoogleMap = GoogleMapProvider.Instance;
        public static readonly GoogleSatelliteMapProvider GoogleSatelliteMap = GoogleSatelliteMapProvider.Instance;
        public static readonly GoogleHybridMapProvider GoogleHybridMap = GoogleHybridMapProvider.Instance;
        public static readonly GoogleTerrainMapProvider GoogleTerrainMap = GoogleTerrainMapProvider.Instance;

        public static readonly YandexMapProvider YandexMap = YandexMapProvider.Instance;
        public static readonly YandexSatelliteMapProvider YandexSatelliteMap = YandexSatelliteMapProvider.Instance;
        public static readonly YandexHybridMapProvider YandexHybridMap = YandexHybridMapProvider.Instance;

        public static readonly WikiMapiaMapProvider WikiMapiaMap = WikiMapiaMapProvider.Instance;

        private static readonly List<GMapProvider> fList;

        /// <summary>
        /// get all instances of the supported providers
        /// </summary>
        public static List<GMapProvider> List
        {
            get {
                return fList;
            }
        }

        static GMapProviders()
        {
            ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls | SecurityProtocolType.Ssl3 | (SecurityProtocolType)3072;

            fList = new List<GMapProvider>();

            Type type = typeof(GMapProviders);
            foreach (var p in type.GetFields()) {
                var v = p.GetValue(null) as GMapProvider; // static classes cannot be instanced, so use null...
                if (v != null) {
                    fList.Add(v);
                }
            }
        }

        private GMapProviders()
        {
        }
    }
}
