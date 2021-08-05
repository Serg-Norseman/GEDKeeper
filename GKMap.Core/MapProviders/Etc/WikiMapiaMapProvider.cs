/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;

namespace GKMap.MapProviders.Etc
{
    public abstract class WikiMapiaMapProviderBase : GMapProvider
    {
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
                throw new NotImplementedException();
            }
        }


        protected WikiMapiaMapProviderBase()
        {
            MaxZoom = 22;
            RefererUrl = "http://wikimapia.org/";
            Copyright = string.Format("© WikiMapia.org - Map data ©{0} WikiMapia", DateTime.Today.Year);
        }

        public override PureImage GetTileImage(GPoint pos, int zoom)
        {
            throw new NotImplementedException();
        }

        public static int GetServerNum(GPoint pos)
        {
            return (int)(pos.X % 4 + (pos.Y % 4) * 4);
        }
    }

    /// <summary>
    /// WikiMapiaMap provider, http://wikimapia.org/
    /// </summary>
    public class WikiMapiaMapProvider : WikiMapiaMapProviderBase
    {
        private static readonly string UrlFormat = "http://i{0}.wikimapia.org/?x={1}&y={2}&zoom={3}";

        private readonly Guid fId = new Guid("7974022B-1AA6-41F1-8D01-F49940E4B48C");
        private readonly string fName = "WikiMapiaMap";
        private GMapProvider[] fOverlays;

        public static readonly WikiMapiaMapProvider Instance = new WikiMapiaMapProvider();

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
                    fOverlays = new GMapProvider[] { this };
                }
                return fOverlays;
            }
        }

        private WikiMapiaMapProvider()
        {
        }

        public override PureImage GetTileImage(GPoint pos, int zoom)
        {
            string url = MakeTileImageUrl(pos, zoom, string.Empty);
            return GetTileImageUsingHttp(url);
        }

        private string MakeTileImageUrl(GPoint pos, int zoom, string language)
        {
            return string.Format(UrlFormat, GetServerNum(pos), pos.X, pos.Y, zoom);
        }
    }
}
