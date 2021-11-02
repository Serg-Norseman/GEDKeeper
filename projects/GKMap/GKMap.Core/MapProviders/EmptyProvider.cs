/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;

namespace GKMap.MapProviders
{
    /// <summary>
    /// represents empty provider
    /// </summary>
    public class EmptyProvider : GMapProvider
    {
        public static readonly EmptyProvider Instance = new EmptyProvider();

        public override Guid Id
        {
            get {
                return Guid.Empty;
            }
        }

        public override string Name
        {
            get {
                return "None";
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
                return null;
            }
        }

        private EmptyProvider()
        {
            MaxZoom = null;
        }

        public override PureImage GetTileImage(GPoint pos, int zoom)
        {
            return null;
        }
    }
}
