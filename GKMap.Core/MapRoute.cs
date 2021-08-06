/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Collections.Generic;
using GKMap.MapProviders;

namespace GKMap
{
    /// <summary>
    /// represents route of map
    /// </summary>
    public class MapRoute
    {
        /// <summary>
        /// points of route
        /// </summary>
        public readonly List<PointLatLng> Points = new List<PointLatLng>();

        /// <summary>
        /// route info
        /// </summary>
        public string Name;

        /// <summary>
        /// custom object
        /// </summary>
        public object Tag;

        /// <summary>
        /// route start point
        /// </summary>
        public PointLatLng? From
        {
            get {
                if (Points.Count > 0) {
                    return Points[0];
                }

                return null;
            }
        }

        /// <summary>
        /// route end point
        /// </summary>
        public PointLatLng? To
        {
            get {
                if (Points.Count > 1) {
                    return Points[Points.Count - 1];
                }

                return null;
            }
        }

        /// <summary>
        /// route distance (in km)
        /// </summary>
        public double Distance
        {
            get {
                double distance = 0.0;

                if (From.HasValue && To.HasValue) {
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
