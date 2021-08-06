/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;

namespace GKMap
{
    /// <summary>
    /// struct for drawing tile
    /// </summary>
    internal struct DrawTile : IEquatable<DrawTile>, IComparable<DrawTile>
    {
        public GPoint PosXY;
        public GPoint PosPixel;
        public double DistanceSqr;

        public override string ToString()
        {
            return PosXY + ", px: " + PosPixel;
        }

        public bool Equals(DrawTile other)
        {
            return (PosXY == other.PosXY);
        }

        public int CompareTo(DrawTile other)
        {
            return other.DistanceSqr.CompareTo(DistanceSqr);
        }
    }
}
