/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.Collections.Generic;

namespace GKMap
{
    /// <summary>
    /// tile load task
    /// </summary>
    internal struct LoadTask : IEquatable<LoadTask>
    {
        public GPoint Pos;
        public int Zoom;

        public LoadTask(GPoint pos, int zoom)
        {
            Pos = pos;
            Zoom = zoom;
        }

        public override string ToString()
        {
            return Zoom + " - " + Pos.ToString();
        }

        public bool Equals(LoadTask other)
        {
            return (Zoom == other.Zoom && Pos == other.Pos);
        }
    }

    internal class LoadTaskComparer : IEqualityComparer<LoadTask>
    {
        public bool Equals(LoadTask x, LoadTask y)
        {
            return x.Zoom == y.Zoom && x.Pos == y.Pos;
        }

        public int GetHashCode(LoadTask obj)
        {
            return obj.Zoom ^ obj.Pos.GetHashCode();
        }
    }
}
