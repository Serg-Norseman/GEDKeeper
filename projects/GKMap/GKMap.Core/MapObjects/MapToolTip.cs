/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;

namespace GKMap.MapObjects
{
    /// <summary>
    /// GKMap marker tooltip.
    /// </summary>
    public abstract class MapToolTip : IDisposable
    {
        private bool fDisposed;
        private MapMarker fMarker;


        public MapMarker Marker
        {
            get {
                return fMarker;
            }
            internal set {
                fMarker = value;
            }
        }

        public GPoint Offset { get; set; }

        protected MapToolTip(MapMarker marker)
        {
            Marker = marker;
            Offset = new GPoint(14, -44);
        }

        public void Dispose()
        {
            if (!fDisposed) {
                fDisposed = true;
            }
        }
    }
}
