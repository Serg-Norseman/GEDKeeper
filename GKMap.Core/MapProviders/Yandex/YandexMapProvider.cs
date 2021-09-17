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
    public abstract class YandexMapProviderBase : GMapProvider
    {
        private GMapProvider[] fOverlays;

        protected string Version = "4.6.9";

        public readonly string Server /*d{'_'}b*/ = /*{^_^}*/ Stuff.GString /*{"_"}*/ (/* ;}~ */"MECxW6okUK3Ir7a9ue/vIA=="/* ;}~ */);
        public readonly string ServerRu /*d{'_'}b*/ = /*{^_^}*/ Stuff.GString /*{"_"}*/ (/* ;}~ */"MECxW6okUK0FRlRPbF0BQg=="/* ;}~ */);
        public readonly string ServerCom /*d{'_'}b*/ = /*{^_^}*/ Stuff.GString/*{"_"}*/ (/* ;}~ */"MECxW6okUK2JNHOW5AuimA=="/* ;}~ */);

        public override PureProjection Projection
        {
            get {
                return MercatorProjectionYandex.Instance;
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
    }

    /// <summary>
    /// YandexMap provider
    /// </summary>
    public class YandexMapProvider : YandexMapProviderBase
    {
        public static readonly YandexMapProvider Instance = new YandexMapProvider();

        private static readonly string UrlServer = "vec";
        private static readonly string UrlFormat = "http://{0}0{1}.{7}/tiles?l=map&v={2}&x={3}&y={4}&z={5}&lang={6}";

        private readonly Guid fId = new Guid("82DC969D-0491-40F3-8C21-4D90B67F47EB");
        private readonly string fName = "YandexMap";

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

        private YandexMapProvider()
        {
            RefererUrl = "http://" + ServerCom + "/";
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
