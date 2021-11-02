/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.Collections.Generic;
using System.Threading;

namespace GKMap
{
    /// <summary>
    /// represent tile
    /// </summary>
    public struct Tile : IDisposable
    {
        public static readonly Tile Empty = new Tile();

        private GPoint fPos;
        private PureImage[] fOverlays;
        private long fOverlaysCount;
        private int fZoom;

        public readonly bool NotEmpty;


        public IEnumerable<PureImage> Overlays
        {
            get {
                for (long i = 0, size = Interlocked.Read(ref fOverlaysCount); i < size; i++) {
                    yield return fOverlays[i];
                }
            }
        }

        internal bool HasAnyOverlays
        {
            get {
                return Interlocked.Read(ref fOverlaysCount) > 0;
            }
        }

        public int Zoom
        {
            get {
                return fZoom;
            }
        }

        public GPoint Pos
        {
            get {
                return fPos;
            }
        }


        public Tile(int zoom, GPoint pos)
        {
            NotEmpty = true;
            fZoom = zoom;
            fPos = pos;
            fOverlays = null;
            fOverlaysCount = 0;
        }

        internal void AddOverlay(PureImage i)
        {
            if (fOverlays == null) {
                fOverlays = new PureImage[4];
            }
            fOverlays[Interlocked.Increment(ref fOverlaysCount) - 1] = i;
        }

        public void Dispose()
        {
            if (fOverlays != null) {
                for (long i = Interlocked.Read(ref fOverlaysCount) - 1; i >= 0; i--) {
                    Interlocked.Decrement(ref fOverlaysCount);
                    fOverlays[i].Dispose();
                    fOverlays[i] = null;
                }
                fOverlays = null;
            }
        }

        public static bool operator ==(Tile m1, Tile m2)
        {
            return m1.fPos == m2.fPos && m1.fZoom == m2.fZoom;
        }

        public static bool operator !=(Tile m1, Tile m2)
        {
            return !(m1 == m2);
        }

        public override bool Equals(object obj)
        {
            if (!(obj is Tile))
                return false;

            Tile comp = (Tile)obj;
            return comp.Zoom == Zoom && comp.Pos == Pos;
        }

        public override int GetHashCode()
        {
            return fZoom ^ fPos.GetHashCode();
        }
    }
}
