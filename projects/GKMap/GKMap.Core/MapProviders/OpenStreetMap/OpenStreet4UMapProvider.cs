/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;

namespace GKMap.MapProviders.OpenStreetMap
{
    /// <summary>
    /// OpenStreet4UMap provider
    /// http://www.4umaps.eu
    /// 
    /// 4UMaps are topographic outdoor maps based on OpenStreetmap data.
    /// The map contains everything you need for any kind of back country activity like hiking,
    /// mountain biking, cycling, climbing etc. 4UMaps has elevation lines, hill shading,
    /// peak height and name, streets, ways, tracks and trails, as well as springs, supermarkets,
    /// restaurants, hotels, shelters etc.
    /// </summary>
    public class OpenStreet4UMapProvider : OpenStreetMapProviderBase
    {
        public static readonly OpenStreet4UMapProvider Instance = new OpenStreet4UMapProvider();

        private static readonly string UrlFormat = "http://4umaps.eu/{0}/{1}/{2}.png";

        private readonly Guid fId = new Guid("3E3D919E-9814-4978-B430-6AAB2C1E41B2");
        private readonly string fName = "OpenStreet4UMap";
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
                    fOverlays = new GMapProvider[] { this };
                }
                return fOverlays;
            }
        }

        private OpenStreet4UMapProvider()
        {
            RefererUrl = "http://www.4umaps.eu/map.htm";
            Copyright = string.Format("© 4UMaps.eu, © OpenStreetMap - Map data ©{0} OpenStreetMap", DateTime.Today.Year);
        }

        public override PureImage GetTileImage(GPoint pos, int zoom)
        {
            string url = MakeTileImageUrl(pos, zoom);
            return GetTileImageUsingHttp(url);
        }

        private string MakeTileImageUrl(GPoint pos, int zoom)
        {
            return string.Format(UrlFormat, zoom, pos.X, pos.Y);
        }
    }
}
