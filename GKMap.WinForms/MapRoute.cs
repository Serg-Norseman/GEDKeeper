/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Collections.Generic;
using GKMap.MapProviders;

namespace GKMap.WinForms
{
    /// <summary>
    /// represents route of map
    /// </summary>
    public class MapRoute : GMapObject
    {
        /// <summary>
        /// points of route
        /// </summary>
        public readonly List<PointLatLng> Points = new List<PointLatLng>();

        /// <summary>
        /// route info
        /// </summary>
        public string Name;

        public bool HasLines
        {
            get {
                return (Points.Count > 1);
            }
        }

        /// <summary>
        /// route distance (in km)
        /// </summary>
        public double Distance
        {
            get {
                double distance = 0.0;

                if (HasLines) {
                    for (int i = 1; i < Points.Count; i++) {
                        distance += GMapProviders.EmptyProvider.Projection.GetDistance(Points[i - 1], Points[i]);
                    }
                }

                return distance;
            }
        }

        public MapRoute(string name)
        {
            Name = name;
        }

        public MapRoute(IEnumerable<PointLatLng> points, string name)
        {
            Points.AddRange(points);
            Name = name;
        }

        /// <summary>
        /// clears points and sets tag and name to null
        /// </summary>
        public void Clear()
        {
            Points.Clear();
            Tag = null;
            Name = null;
        }
    }
}
