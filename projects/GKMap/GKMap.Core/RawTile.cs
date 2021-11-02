/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Collections.Generic;

namespace GKMap
{
    /// <summary>
    /// struct for raw tile
    /// </summary>
    internal struct RawTile
    {
        public int Type;
        public GPoint Pos;
        public int Zoom;

        public RawTile(int type, GPoint pos, int zoom)
        {
            Type = type;
            Pos = pos;
            Zoom = zoom;
        }

        public override string ToString()
        {
            return Type + " at zoom " + Zoom + ", pos: " + Pos;
        }
    }

    internal class RawTileComparer : IEqualityComparer<RawTile>
    {
        public bool Equals(RawTile x, RawTile y)
        {
            return x.Type == y.Type && x.Zoom == y.Zoom && x.Pos == y.Pos;
        }

        public int GetHashCode(RawTile obj)
        {
            return obj.Type ^ obj.Zoom ^ obj.Pos.GetHashCode();
        }
    }
}

