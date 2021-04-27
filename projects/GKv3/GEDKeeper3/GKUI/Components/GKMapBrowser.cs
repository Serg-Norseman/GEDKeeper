/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using BSLib;
using Eto.Forms;
using GKCore;
using GKCore.Maps;
using GKCore.MVP.Controls;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKMapBrowser : WebView, IMapBrowser
    {
        private readonly ExtList<GeoPoint> fMapPoints;
        private bool fShowPoints;
        private bool fShowLines;
        private int fUpdateCount;


        public bool ShowPoints
        {
            get {
                return fShowPoints;
            }
            set {
                fShowPoints = value;
                RefreshPoints();
            }
        }

        public bool ShowLines
        {
            get {
                return fShowLines;
            }
            set {
                fShowLines = value;
                RefreshPoints();
            }
        }

        public ExtList<GeoPoint> MapPoints
        {
            get { return fMapPoints; }
        }

        public GKMapBrowser()
        {
            BrowserContextMenuEnabled = false;

            fMapPoints = new ExtList<GeoPoint>(true);
            fUpdateCount = 0;
            fShowPoints = true;
            fShowLines = true;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                ClearPoints();
                fMapPoints.Dispose();
            }
            base.Dispose(disposing);
        }

        public void Activate()
        {
            Focus();
        }

        public int AddPoint(double latitude, double longitude, string hint)
        {
            GeoPoint pt = new GeoPoint(latitude, longitude, hint);
            return fMapPoints.Add(pt);
        }

        public void ClearPoints()
        {
            gm_ClearPoints();
            fMapPoints.Clear();
        }

        public void DeletePoint(int index)
        {
            fMapPoints.Delete(index);
            RefreshPoints();
        }

        public void BeginUpdate()
        {
            fUpdateCount++;
        }

        public void EndUpdate()
        {
            fUpdateCount--;

            if (fUpdateCount <= 0)
            {
                RefreshPoints();
                fUpdateCount = 0;
            }
        }

        public void InitMap()
        {
            try {
                const string MapContent =
                    "<html>" +
                    "<head>" +
                    "<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\"/>" +
                    "<script src=\"http://maps.googleapis.com/maps/api/js?sensor=false&language=ru&key=AIzaSyCebJC5BpniJtRaJCSEl3tXdFy3KhbV5hk\" type=\"text/javascript\"></script>" +
                    "<script type=\"text/javascript\">" +
                    "var map;" +
                    "var markersArray = [];" +
                    "function addMarker(latitude, longitude, hint) { " +
                    "	var latlng = new google.maps.LatLng(latitude,longitude); " +
                    "	var marker = new google.maps.Marker({ position: latlng, map: map, title: hint }); " +
                    "	markersArray.push(marker);" +
                    "} " +
                    "function clearOverlays() {" +
                    "	for (var i = 0; i < markersArray.length; i++ ) {" +
                    "		markersArray[i].setMap(null);" +
                    "	}" +
                    "	markersArray.length = 0;" +
                    "}" +
                    "function initialize() { " +
                    "	var mapOptions = {" +
                    "		center: new google.maps.LatLng(55.755786, 37.617633)," +
                    "		zoom: 8," +
                    "		mapTypeId: google.maps.MapTypeId.TERRAIN" +
                    "	};" +
                    "	map = new google.maps.Map(document.getElementById(\"map\"), mapOptions); " +
                    "}" +
                    "</script>" +
                    "</head>" +
                    "<body onload=\"initialize()\">" +
                    "<div id=\"map\" style=\"position:absolute; width: 100%; height: 100%; left: 0px; top: 0px;\"></div>" +
                    "<noscript>JavaScript must be switched on for use Google Maps.</noscript>" +
                    "</body></html>";

                LoadHtml(MapContent);
            } catch (Exception ex) {
                Logger.WriteError("GKMapBrowser.InitMap()", ex);
            }
        }

        public void RefreshPoints()
        {
            gm_ClearPoints();
            if (fMapPoints.Count <= 0) return;

            string pointsScript = "";
            string polylineScript = "";

            int num = fMapPoints.Count;
            for (int i = 0; i < num; i++)
            {
                GeoPoint pt = fMapPoints[i];
                pointsScript += string.Format("addMarker({0}, {1}, \"{2}\");", new object[]
                                              { PlacesLoader.CoordToStr(pt.Latitude), PlacesLoader.CoordToStr(pt.Longitude), pt.Hint });

                /*polylineScript = string.Concat(new string[]
                                                   {
                                                       polylineScript, "new google.maps.LatLng(",
                                                       CoordToStr(pt.Latitude), ",", CoordToStr(pt.Longitude), "),"
                                                   });*/

                polylineScript = string.Concat(new string[]
                                               {
                                                   polylineScript,
                                                   "{lat:", PlacesLoader.CoordToStr(pt.Latitude), ",lng:", PlacesLoader.CoordToStr(pt.Longitude), "},"
                                               });
            }

            if (ShowPoints)
            {
                gm_ExecScript(pointsScript);
            }

            if (ShowLines)
            {
                if (!string.IsNullOrEmpty(polylineScript)) {
                    polylineScript = polylineScript.Remove(polylineScript.Length - 1, 1);
                }

                polylineScript =
                    "var polyline = new google.maps.Polyline({path: [" + polylineScript + "],strokeColor: '#FF0000', strokeWeight: 3}); " +
                    "polyline.setMap(map);"/*+
                        "markersArray.push(polyline);"*/;
                gm_ExecScript(polylineScript);
            }
        }

        public void SaveSnapshot(string fileName)
        {
        }

        public void SetCenter(double latitude, double longitude, int scale)
        {
            string script;
            if (scale >= 0) {
                script = string.Concat(new string[] {
                                           "var point = new google.maps.LatLng(",
                                           PlacesLoader.CoordToStr(latitude), ",", PlacesLoader.CoordToStr(longitude), "); ",
                                           "map.setCenter(point)",
                                           "map.setZoom(", scale.ToString(), ")"
                                       });
            } else {
                script = string.Concat(new string[] {
                                           "var point = new google.maps.LatLng(",
                                           PlacesLoader.CoordToStr(latitude), ",", PlacesLoader.CoordToStr(longitude), "); ",
                                           "map.setCenter(point)"
                                       });
            }

            gm_ExecScript(script);
        }

        public void ZoomToBounds()
        {
            CoordsRect rt = PlacesLoader.GetPointsFrame(fMapPoints);
            if (rt.MinLon == rt.MaxLon || rt.MinLat == rt.MaxLat) return;

            double centerLongtude = ((rt.MaxLon + rt.MinLon) / 2.0);
            double centerLatitude = ((rt.MaxLat + rt.MinLat) / 2.0);

            string script =
                "var point1 = new google.maps.LatLng({0}, {1});" +
                "var point2 = new google.maps.LatLng({2}, {3});" +
                "var bounds = new google.maps.LatLngBounds(point1, point2);" +
                "map.fitBounds(bounds);" +
                "map.setCenter(new google.maps.LatLng({4}, {5}));";
            script = string.Format(script, new object[]
                                   { PlacesLoader.CoordToStr(rt.MinLat), PlacesLoader.CoordToStr(rt.MinLon),
                                       PlacesLoader.CoordToStr(rt.MaxLat), PlacesLoader.CoordToStr(rt.MaxLon),
                                       PlacesLoader.CoordToStr(centerLatitude), PlacesLoader.CoordToStr(centerLongtude) });

            gm_ExecScript(script);
        }

        #region Google-specific

        private void gm_ClearPoints()
        {
            gm_ExecScript("clearOverlays();");
        }

        private void gm_ExecScript(string script)
        {
            script = script.Trim();
            if (string.IsNullOrEmpty(script)) return;

            try {
                ExecuteScript(script);
            } catch (Exception ex) {
                Logger.WriteError("GKMapBrowser.gm_ExecScript()", ex);
            }
        }

        #endregion
    }
}
