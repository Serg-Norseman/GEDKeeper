﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using System.Globalization;
using BSLib;
using GDModel;
using GKCore.MVP.Controls;

namespace GKCore.Maps
{
    public class PlaceRef
    {
        public readonly DateTime Date;
        public readonly GDMCustomEvent Event;

        public PlaceRef(GDMCustomEvent evt)
        {
            Event = evt;
            Date = (evt == null) ? new DateTime(0) : evt.Date.GetDateTime();
        }
    }

    public struct CoordsRect
    {
        public double MinLon;
        public double MinLat;
        public double MaxLon;
        public double MaxLat;
    }

    public class MapPlace : BaseObject
    {
        public string Name;
        public readonly List<GeoPoint> Points;
        public readonly ExtList<PlaceRef> PlaceRefs;

        public MapPlace()
        {
            Points = new List<GeoPoint>();
            PlaceRefs = new ExtList<PlaceRef>(false);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                PlaceRefs.Dispose();
            }
            base.Dispose(disposing);
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public class PlacesLoader
    {
        public PlacesLoader()
        {
        }

        public static void AddPoint(ExtList<GeoPoint> mapPoints, GeoPoint gmPt, PlaceRef placeRef)
        {
            GeoPoint pt;
            int num = mapPoints.Count;
            for (int i = 0; i < num; i++) {
                pt = mapPoints[i];
                if (pt.Hint == gmPt.Hint) {
                    return;
                }
            }

            pt = new GeoPoint(gmPt.Latitude, gmPt.Longitude, gmPt.Hint);
            pt.Date = placeRef.Date;
            mapPoints.Add(pt);
        }

        public static void CopyPoints(IMapBrowser browser, ExtList<GeoPoint> gmapPoints, bool byPerson)
        {
            if (gmapPoints == null)
                throw new ArgumentNullException("gmapPoints");

            browser.BeginUpdate();
            try {
                browser.ClearPoints();

                int num = gmapPoints.Count;
                for (int i = 0; i < num; i++) {
                    GeoPoint pt = gmapPoints[i];
                    string stHint = pt.Hint;
                    if (byPerson) {
                        stHint = stHint + " [" + pt.Date.ToString() + "]";
                    }

                    browser.AddPoint(pt.Latitude, pt.Longitude, stHint);
                }

                browser.ZoomToBounds();
            } finally {
                browser.EndUpdate();
            }
        }

        public static CoordsRect GetPointsFrame(ExtList<GeoPoint> mapPoints)
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

        public static string CoordToStr(double val)
        {
            NumberFormatInfo nfi = new NumberFormatInfo();
            nfi.NumberDecimalSeparator = ".";
            return val.ToString("0.000000", nfi);
        }
    }
}
