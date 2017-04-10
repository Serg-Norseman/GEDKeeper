/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Geocoding;
using GKUI.Contracts;

namespace GKCore.Maps
{
    public class PlaceRef
    {
        public readonly DateTime Date;
        public readonly GEDCOMCustomEvent Event;

        public PlaceRef(GEDCOMCustomEvent evt)
        {
            Event = evt;
            Date = (evt == null) ? new DateTime(0) : evt.Date.GetDateTime();
        }
    }

    public class MapPlace : BaseObject
    {
        public string Name;
        public readonly IList<GeoPoint> Points;
        public readonly ExtList<PlaceRef> PlaceRefs;

        public MapPlace()
        {
            Points = new List<GeoPoint>();
            PlaceRefs = new ExtList<PlaceRef>(false);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
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
            browser.BeginUpdate();
            try
            {
                browser.ClearPoints();

                int num = gmapPoints.Count;
                for (int i = 0; i < num; i++)
                {
                    GeoPoint pt = gmapPoints[i];
                    string stHint = pt.Hint;
                    if (byPerson)
                    {
                        stHint = stHint + " [" + pt.Date.ToString() + "]";
                    }

                    browser.AddPoint(pt.Latitude, pt.Longitude, stHint);
                }

                browser.ZoomToBounds();
            }
            finally
            {
                browser.EndUpdate();
            }
        }
    }
}
