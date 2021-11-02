/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Collections.Generic;

namespace GKMap.MapObjects
{
    /// <summary>
    /// GKMap polygon.
    /// </summary>
    public abstract class MapPolygon : MapFigure
    {
        protected MapPolygon(string name, IEnumerable<PointLatLng> points)
            : base(name, points)
        {
            fVisible = true;
            IsHitTestVisible = false;
        }

        protected override void UpdateLocalPosition()
        {
            if (fVisible) {
                Overlay.Control.Core.UpdatePolygonLocalPosition(this);
            } else {
                if (Overlay.Control.IsMouseOverPolygon) {
                    Overlay.Control.IsMouseOverPolygon = false;
                    Overlay.Control.RestoreCursorOnLeave();
                }
            }
        }

        /// <summary>
        /// checks if point is inside the polygon,
        /// </summary>
        /// <param name="p"></param>
        /// <returns></returns>
        public bool IsInsideLatLng(PointLatLng p)
        {
            int count = Points.Count;

            if (count < 3) {
                return false;
            }

            bool result = false;

            for (int i = 0, j = count - 1; i < count; i++) {
                var p1 = Points[i];
                var p2 = Points[j];

                if (p1.Lat < p.Lat && p2.Lat >= p.Lat || p2.Lat < p.Lat && p1.Lat >= p.Lat) {
                    if (p1.Lng + (p.Lat - p1.Lat) / (p2.Lat - p1.Lat) * (p2.Lng - p1.Lng) < p.Lng) {
                        result = !result;
                    }
                }
                j = i;
            }
            return result;
        }
    }
}
