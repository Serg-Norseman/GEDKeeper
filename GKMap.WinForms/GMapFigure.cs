/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Collections.Generic;
using System.Drawing.Drawing2D;

namespace GKMap.WinForms
{
    /// <summary>
    /// represents route of map
    /// </summary>
    public class GMapFigure : GMapObject
    {
        protected GraphicsPath fGraphicsPath;


        public bool HasLines
        {
            get {
                return (Points.Count > 1);
            }
        }

        public List<GPoint> LocalPoints { get; private set; }

        /// <summary>
        /// route info
        /// </summary>
        public string Name { get; set; }

        /// <summary>
        /// points of route
        /// </summary>
        public List<PointLatLng> Points { get; private set; }


        public GMapFigure(string name, IEnumerable<PointLatLng> points = null)
        {
            LocalPoints = new List<GPoint>();
            Points = new List<PointLatLng>();

            if (points != null) {
                Points.AddRange(points);
                LocalPoints.Capacity = Points.Count;
            }

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

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                LocalPoints.Clear();

                if (fGraphicsPath != null) {
                    fGraphicsPath.Dispose();
                    fGraphicsPath = null;
                }
                Clear();
            }
            base.Dispose(disposing);
        }
    }
}
