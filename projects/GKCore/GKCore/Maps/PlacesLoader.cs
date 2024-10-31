/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;
using GDModel;
using GKCore.Design.Controls;
using GKCore.Options;

namespace GKCore.Maps
{
    public class PlaceRef
    {
        public readonly GDMCustomEvent Event;
        public readonly GDMRecord Owner;

        public PlaceRef(GDMRecord owner, GDMCustomEvent evt)
        {
            Owner = owner;
            Event = evt;
        }
    }

    public struct CoordsRect
    {
        public double MinLon;
        public double MinLat;
        public double MaxLon;
        public double MaxLat;
    }

    public class MapPlace
    {
        public string Name;
        public readonly List<GeoPoint> Points;
        public readonly List<PlaceRef> PlaceRefs;

        public MapPlace(string name)
        {
            Name = name;
            Points = new List<GeoPoint>();
            PlaceRefs = new List<PlaceRef>();
        }
    }

    /// <summary>
    ///
    /// </summary>
    public static class PlacesLoader
    {
        public static void AddPoint(IList<GeoPoint> mapPoints, GeoPoint gmPt, PlaceRef placeRef)
        {
            GeoPoint pt;
            int num = mapPoints.Count;
            for (int i = 0; i < num; i++) {
                pt = mapPoints[i];
                if (pt.Hint == gmPt.Hint) {
                    return;
                }
            }

            var date = (placeRef.Event == null) ? null : placeRef.Event.Date;
            mapPoints.Add(new GeoPoint(gmPt.Latitude, gmPt.Longitude, gmPt.Hint, date));
        }

        public static void CopyPoints(IMapBrowser browser, IList<GeoPoint> gmapPoints, bool byPerson)
        {
            if (gmapPoints == null)
                throw new ArgumentNullException("gmapPoints");

            var globOpts = GlobalOptions.Instance;

            browser.BeginUpdate();
            try {
                browser.ClearPoints();

                int num = gmapPoints.Count;
                for (int i = 0; i < num; i++) {
                    GeoPoint pt = gmapPoints[i];
                    string stHint = pt.Hint;
                    if (byPerson && pt.Date != null) {
                        stHint = stHint + " [" + pt.Date.GetDisplayStringExt(globOpts.DefDateFormat, false, false) + "]";
                    }

                    browser.AddPoint(pt.Latitude, pt.Longitude, stHint);
                }

                browser.ZoomToBounds();
            } finally {
                browser.EndUpdate();
            }
        }

        public static CoordsRect GetPointsFrame(IList<GeoPoint> mapPoints)
        {
            CoordsRect result = new CoordsRect();
            if (mapPoints == null || mapPoints.Count <= 0) return result;

            GeoPoint pt = mapPoints[0];
            result.MinLon = pt.Longitude;
            result.MaxLon = pt.Longitude;
            result.MinLat = pt.Latitude;
            result.MaxLat = pt.Latitude;

            if (mapPoints.Count == 1) {
                result.MinLon = (result.MinLon - 20.0);
                result.MaxLon = (result.MaxLon + 20.0);
                result.MinLat = (result.MinLat - 20.0);
                result.MaxLat = (result.MaxLat + 20.0);
            } else {
                int num = mapPoints.Count;
                for (int i = 0; i < num; i++) {
                    pt = mapPoints[i];

                    if (result.MinLon > pt.Longitude) result.MinLon = pt.Longitude;
                    else if (result.MaxLon < pt.Longitude) result.MaxLon = pt.Longitude;

                    if (result.MinLat > pt.Latitude) result.MinLat = pt.Latitude;
                    else if (result.MaxLat < pt.Latitude) result.MaxLat = pt.Latitude;
                }
            }

            return result;
        }
    }
}
