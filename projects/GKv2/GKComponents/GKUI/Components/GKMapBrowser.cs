/*
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

#define USE_GMAPS

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;

using BSLib;
using GKCore;
using GKCore.Geocoding;
using GKCore.Maps;
using GKCore.UIContracts;

namespace GKUI.Components
{
    #if !USE_GMAPS

    public sealed class GKMapBrowser : WebBrowser, IMapBrowser
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

        private void SetWebBrowserDocument(string documentText)
        {
            string strContent = (documentText ?? string.Empty);

            AllowNavigation = false;
            ScriptErrorsSuppressed = true;
            AllowWebBrowserDrop = false;

            #if !__MonoCS__
            DocumentText = strContent;

            // Wait for document being loaded
            for (int i = 0; i < 50; ++i)
            {
                if (DocumentText == strContent) break;
                System.Threading.Thread.Sleep(20);
                Application.DoEvents();
            }
            //Refresh();
            #else
            DocumentText = strContent;
            Refresh();
            #endif
        }

        public void InitMap()
        {
            try
            {
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

                SetWebBrowserDocument(MapContent);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKMapBrowser.InitMap(): " + ex.Message);
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

            #if !__MonoCS__
            try
            {
                HtmlElement script1 = Document.GetElementById("gkScript");

                if (script1 == null) {
                    HtmlElement head = Document.GetElementsByTagName("head")[0];
                    script1 = Document.CreateElement("script");
                    script1.Id = "gkScript";
                    head.AppendChild(script1);
                }

                script1.SetAttribute("text", "function gkFunc() { " + script + " }");
                Document.InvokeScript("gkFunc");

                //string jCode = "alert("Hello");"
                //webBrowser1.Document.InvokeScript("eval", new object[] { jCode });
                
                //var jsCode="alert('hello world from injected code');";
                //WebBrowser.Document.InvokeScript("execScript", new Object[] { jsCode, "JavaScript" });
                
                //mshtml.IHTMLWindow2 win = (mshtml.IHTMLWindow2)Document.Window.DomWindow;
                //if (win != null) {
                //    win.execScript(script, "JavaScript");
                //}
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKMapBrowser.gm_ExecScript(): " + ex.Message);
            }
            #endif
        }

        #endregion
    }

    #else

    using System.Net.NetworkInformation;
    using System.Text;
    using GMap.NET;
    using GMap.NET.MapProviders;
    using GMap.NET.WindowsForms;
    using GMap.NET.WindowsForms.Markers;
    using GMap.NET.WindowsForms.ToolTips;

    public sealed class GKMapBrowser : UserControl, IMapBrowser
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
            InitControl();

            fMapPoints = new ExtList<GeoPoint>(true);
            fUpdateCount = 0;
            fShowPoints = true;
            fShowLines = true;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                ClearPoints();
                fMapPoints.Dispose();
            }
            base.Dispose(disposing);
        }

        public int AddPoint(double latitude, double longitude, string hint)
        {
            BeginUpdate();
            GeoPoint pt = new GeoPoint(latitude, longitude, hint);
            int res = fMapPoints.Add(pt);
            EndUpdate();

            return res;
        }

        public void ClearPoints()
        {
            ClearAll();
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
            if (fUpdateCount <= 0) {
                RefreshPoints();
                fUpdateCount = 0;
            }
        }

        public void InitMap()
        {
        }

        public void RefreshPoints()
        {
            ClearAll();
            if (fMapPoints.Count <= 0) return;

            var points = new List<PointLatLng>();

            int num = fMapPoints.Count;
            for (int i = 0; i < num; i++) {
                GeoPoint pt = fMapPoints[i];

                var point = new PointLatLng(pt.Latitude, pt.Longitude);

                if (fShowPoints) {
                    AddMarker(point);
                    if (i == num - 1) {
                        SetCenter(pt.Latitude, pt.Longitude, -1);
                    }
                }

                if (fShowLines) {
                    points.Add(point);
                }
            }

            if (fShowLines) {
                AddRoute(points);
            }

            ZoomToBounds();
        }

        public void SaveSnapshot(string fileName)
        {
            Image tmpImage = fMapControl.ToImage();
            if (tmpImage != null) {
                using (tmpImage) {
                    tmpImage.Save(fileName);
                }
            }
        }

        public void SetCenter(double latitude, double longitude, int scale)
        {
            fTargetMarker.Position = new PointLatLng(latitude, longitude);
            ZoomToBounds();
        }

        public void ZoomToBounds()
        {
            ZoomAndCenterMarkers();
        }

        #region Inner control

        private readonly GMapOverlay fObjects = new GMapOverlay("objects");
        private readonly GMapOverlay fPolygons = new GMapOverlay("polygons");
        private readonly GMapOverlay fRoutes = new GMapOverlay("routes");
        private readonly GMapOverlay fTopOverlay = new GMapOverlay();

        private bool fIsMouseDown = false;
        private GMapControl fMapControl;
        private GMapMarker fTargetMarker;

        private void InitControl()
        {
            fMapControl = new GMapControl {
                Bearing = 0F,
                CanDragMap = true,
                Dock = DockStyle.Fill,
                EmptyTileColor = Color.Navy,
                GrayScaleMode = false,
                HelperLineOption = HelperLineOptions.DontShow,
                LevelsKeepInMemmory = 5,
                Location = new Point(0, 0),
                Margin = new Padding(4),
                MarkersEnabled = true,
                MaxZoom = 17,
                MinZoom = 2,
                MouseWheelZoomType = MouseWheelZoomType.MousePositionAndCenter,
                NegativeMode = false,
                PolygonsEnabled = true,
                RetryLoadTile = 0,
                RoutesEnabled = true,
                ScaleMode = ScaleModes.Integer,
                SelectedAreaFillColor = Color.FromArgb(33, 65, 105, 225),
                ShowTileGridLines = false,
                Zoom = 0D
            };
            Controls.Add(fMapControl);

            if (!GMapControl.IsDesignerHosted) {
                // set your proxy here if need
                //GMapProvider.IsSocksProxy = true;
                //GMapProvider.WebProxy = new WebProxy("127.0.0.1", 1080);
                //GMapProvider.WebProxy.Credentials = new NetworkCredential("user", "password");
                // or
                //GMapProvider.WebProxy = WebRequest.DefaultWebProxy;

                // set cache mode only if no internet avaible
                if (!PingNetwork("pingtest.com")) {
                    fMapControl.Manager.Mode = AccessMode.CacheOnly;
                    MessageBox.Show("No internet connection available, going to CacheOnly mode.", "GMap.NET - Demo.WindowsForms", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                } else {
                    fMapControl.Manager.Mode = AccessMode.ServerAndCache;
                }

                // config map         
                fMapControl.CanDragMap = true;
                fMapControl.MapProvider = GMapProviders.OpenStreetMap;
                fMapControl.MinZoom = 0;
                fMapControl.MaxZoom = 24;
                fMapControl.ShowTileGridLines = false;
                fMapControl.Zoom = 9;

                fMapControl.Manager.UseRouteCache = true;
                fMapControl.Manager.UseGeocoderCache = true;
                fMapControl.Manager.UsePlacemarkCache = true;
                fMapControl.Manager.UseDirectionsCache = true;

                // add custom layers  
                fMapControl.Overlays.Add(fRoutes);
                fMapControl.Overlays.Add(fPolygons);
                fMapControl.Overlays.Add(fObjects);
                fMapControl.Overlays.Add(fTopOverlay);

                // map events
                fMapControl.OnPositionChanged += MainMap_OnPositionChanged;
                fMapControl.OnMapZoomChanged += MainMap_OnMapZoomChanged;
                fMapControl.OnMapTypeChanged += MainMap_OnMapTypeChanged;
                fMapControl.OnMarkerClick += MainMap_OnMarkerClick;
                fMapControl.OnMarkerEnter += MainMap_OnMarkerEnter;
                fMapControl.OnMarkerLeave += MainMap_OnMarkerLeave;
                fMapControl.OnPolygonEnter += MainMap_OnPolygonEnter;
                fMapControl.OnPolygonLeave += MainMap_OnPolygonLeave;
                fMapControl.OnRouteEnter += MainMap_OnRouteEnter;
                fMapControl.OnRouteLeave += MainMap_OnRouteLeave;
                fMapControl.MouseMove += MainMap_MouseMove;
                fMapControl.MouseDown += MainMap_MouseDown;
                fMapControl.MouseUp += MainMap_MouseUp;
                fMapControl.MouseDoubleClick += MainMap_MouseDoubleClick;
                fMapControl.KeyPress += MainForm_KeyPress;
                fMapControl.KeyUp += MainForm_KeyUp;

                // set current marker
                fTargetMarker = new GMarkerGoogle(fMapControl.Position, GMarkerGoogleType.arrow);
                fTargetMarker.IsHitTestVisible = false;
                fTargetMarker.IsVisible = true;
                fTopOverlay.Markers.Add(fTargetMarker);

                // add start location
                GeoCoderStatusCode status = GeoCoderStatusCode.Unknow;
                PointLatLng? pos = GMapProviders.GoogleMap.GetPoint("Russia, Moscow", out status);
                if (pos != null && status == GeoCoderStatusCode.G_GEO_SUCCESS) {
                    fTargetMarker.Position = pos.Value;
                    fMapControl.ZoomAndCenterMarkers(null);
                }
            }

            /*
#if !MONO   // mono doesn't handle it, so we 'lost' provider list ;]
            cmbMapType.ValueMember = "Name";
            cmbMapType.DataSource = GMapProviders.List;
            cmbMapType.SelectedItem = fMapControl.MapProvider;
#endif

            trkZoom.Minimum = fMapControl.MinZoom * 100;
            trkZoom.Maximum = fMapControl.MaxZoom * 100;
            trkZoom.TickFrequency = 100;
            trkZoom.Value = (int)fMapControl.Zoom * 100;
            */
        }

        private void ZoomAndCenterMarkers()
        {
            fMapControl.ZoomAndCenterMarkers(null);
        }

        private void AddRoute(List<PointLatLng> points)
        {
            var route = new GMapRoute(points, "route test");
            route.IsHitTestVisible = true;
            fRoutes.Routes.Add(route);
        }

        private void AddPolygon(List<PointLatLng> points)
        {
            var polygon = new GMapPolygon(points, "polygon test");
            polygon.IsHitTestVisible = true;
            fPolygons.Polygons.Add(polygon);
        }

        /// <summary>
        /// adds marker using geocoder
        /// </summary>
        /// <param name="place"></param>
        private void AddLocationMarker(string place)
        {
            GeoCoderStatusCode status = GeoCoderStatusCode.Unknow;
            PointLatLng? pos = GMapProviders.GoogleMap.GetPoint(place, out status);
            if (pos != null && status == GeoCoderStatusCode.G_GEO_SUCCESS) {
                GMarkerGoogle m = new GMarkerGoogle(pos.Value, GMarkerGoogleType.green);
                m.ToolTip = new GMapRoundedToolTip(m);
                m.ToolTipText = place;
                m.ToolTipMode = MarkerTooltipMode.Always;

                fObjects.Markers.Add(m);
            }
        }

        private void AddMarker(PointLatLng position, string toolTip = "")
        {
            GMarkerGoogle m = new GMarkerGoogle(position, GMarkerGoogleType.green_small);
            if (!string.IsNullOrEmpty(toolTip)) {
                m.ToolTipMode = MarkerTooltipMode.Always;
                m.ToolTipText = toolTip;
            }
            fObjects.Markers.Add(m);
        }

        private void AddMarkerAndSearchTooltip(PointLatLng position)
        {
            Placemark? p = null;
            GeoCoderStatusCode status;
            var ret = GMapProviders.GoogleMap.GetPlacemark(position, out status);
            if (status == GeoCoderStatusCode.G_GEO_SUCCESS && ret != null) {
                p = ret;
            }

            string toolTip = (p != null) ? p.Value.Address : fTargetMarker.Position.ToString();
            AddMarker(position, toolTip);
        }

        private void ClearAll()
        {
            fRoutes.Routes.Clear();
            fPolygons.Polygons.Clear();
            fObjects.Markers.Clear();
        }

        private static bool PingNetwork(string hostNameOrAddress)
        {
            bool pingStatus = false;

            using (Ping p = new Ping()) {
                byte[] buffer = Encoding.ASCII.GetBytes("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
                int timeout = 4444; // 4s

                try {
                    PingReply reply = p.Send(hostNameOrAddress, timeout, buffer);
                    pingStatus = (reply.Status == IPStatus.Success);
                } catch (Exception) {
                    pingStatus = false;
                }
            }

            return pingStatus;
        }

        #region Event handlers

        /*private void cmbMapType_DropDownClosed(object sender, EventArgs e)
        {
            fMapControl.MapProvider = cmbMapType.SelectedItem as GMapProvider;
        }*/

        private void MainMap_OnMarkerLeave(GMapMarker item)
        {
            // dummy
        }

        private void MainMap_OnMarkerEnter(GMapMarker item)
        {
            // dummy
        }

        private void MainMap_OnPolygonLeave(GMapPolygon item)
        {
            item.Stroke.Color = Color.MidnightBlue;
        }

        private void MainMap_OnPolygonEnter(GMapPolygon item)
        {
            item.Stroke.Color = Color.Red;
        }

        private void MainMap_OnRouteLeave(GMapRoute item)
        {
            item.Stroke.Color = Color.MidnightBlue;
        }

        private void MainMap_OnRouteEnter(GMapRoute item)
        {
            item.Stroke.Color = Color.Red;
        }

        private void MainMap_OnMapTypeChanged(GMapProvider type)
        {
            /*cmbMapType.SelectedItem = type;
            trkZoom.Minimum = fMapControl.MinZoom * 100;
            trkZoom.Maximum = fMapControl.MaxZoom * 100;*/
            fMapControl.ZoomAndCenterMarkers("objects");
        }

        private void MainMap_MouseUp(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left) {
                fIsMouseDown = false;
            }
        }

        private void MainMap_MouseDoubleClick(object sender, MouseEventArgs e)
        {
            // dummy
        }

        private void MainMap_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left) {
                fIsMouseDown = true;
                if (fTargetMarker.IsVisible) {
                    fTargetMarker.Position = fMapControl.FromLocalToLatLng(e.X, e.Y);
                }
            }
        }

        private void MainMap_MouseMove(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left && fIsMouseDown) {
                if (fTargetMarker.IsVisible) {
                    fTargetMarker.Position = fMapControl.FromLocalToLatLng(e.X, e.Y);
                }

                fMapControl.Refresh(); // force instant invalidation
            }
        }

        private void MainMap_OnMapZoomChanged()
        {
            //trkZoom.Value = (int)(fMapControl.Zoom * 100.0);
        }

        private void MainMap_OnMarkerClick(GMapMarker item, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left) {
                GeoCoderStatusCode status;
                var pos = GMapProviders.GoogleMap.GetPlacemark(item.Position, out status);
                if (status == GeoCoderStatusCode.G_GEO_SUCCESS && pos != null) {
                    item.ToolTipText = pos.Value.Address;
                    fMapControl.Invalidate(false);
                }
            }
        }

        private void MainMap_OnPositionChanged(PointLatLng point)
        {
            // dummy
        }

        private void trkZoom_ValueChanged(object sender, EventArgs e)
        {
            //fMapControl.Zoom = trkZoom.Value / 100.0;
        }

        private void btnZoomUp_Click(object sender, EventArgs e)
        {
            fMapControl.Zoom = ((int)fMapControl.Zoom) + 1;
        }

        private void btnZoomDown_Click(object sender, EventArgs e)
        {
            fMapControl.Zoom = ((int)(fMapControl.Zoom + 0.99)) - 1;
        }

        private void MainForm_KeyUp(object sender, KeyEventArgs e)
        {
            int offset = -22;

            switch (e.KeyCode) {
                case Keys.Left:
                    fMapControl.Offset(-offset, 0);
                    break;

                case Keys.Right:
                    fMapControl.Offset(offset, 0);
                    break;

                case Keys.Up:
                    fMapControl.Offset(0, -offset);
                    break;

                case Keys.Down:
                    fMapControl.Offset(0, offset);
                    break;

                case Keys.Delete:
                    break;
            }
        }

        private void MainForm_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (fMapControl.Focused) {
                switch (e.KeyChar) {
                    case '+':
                        fMapControl.Zoom = ((int)fMapControl.Zoom) + 1;
                        break;

                    case '-':
                        fMapControl.Zoom = ((int)(fMapControl.Zoom + 0.99)) - 1;
                        break;
                }
            }
        }

        #endregion

        #endregion
    }

    #endif
}
