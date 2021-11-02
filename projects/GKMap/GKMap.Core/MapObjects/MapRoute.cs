/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Collections.Generic;
using GKMap.MapProviders;

namespace GKMap.MapObjects
{
    /// <summary>
    /// GKMap route.
    /// </summary>
    public abstract class MapRoute : MapFigure
    {
        protected MapRoute(string name, IEnumerable<PointLatLng> points = null)
            : base(name, points)
        {
            fVisible = true;
            IsHitTestVisible = false;
        }

        protected override void UpdateLocalPosition()
        {
            if (fVisible) {
                Overlay.Control.Core.UpdateRouteLocalPosition(this);
            } else {
                if (Overlay.Control.IsMouseOverRoute) {
                    Overlay.Control.IsMouseOverRoute = false;
                    Overlay.Control.RestoreCursorOnLeave();
                }
            }
        }

        /// <summary>
        /// route distance (in km)
        /// </summary>
        public double GetDistance()
        {
            double distance = 0.0;

            if (HasLines) {
                for (int i = 1; i < Points.Count; i++) {
                    distance += GMapProviders.EmptyProvider.Projection.GetDistance(Points[i - 1], Points[i]);
                }
            }

            return distance;
        }
    }
}
