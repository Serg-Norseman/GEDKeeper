/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using System.Drawing;
using System.Net;
using System.Net.NetworkInformation;
using System.Text;
using System.Windows.Forms;

using BSLib;
using GKCore.Maps;
using GKCore.MVP.Controls;
using GKCore.Options;
using GKMap;
using GKMap.MapProviders;
using GKMap.WinForms;

namespace GKUI.Components
{
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

        public void Activate()
        {
            Select();
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

        private bool fIsMouseDown;
        private GMapControl fMapControl;
        private GMapMarker fTargetMarker;

        private void InitControl()
        {
            fMapControl = new GMapControl();
            fMapControl.Dock = DockStyle.Fill;
            fMapControl.Location = new Point(0, 0);
            fMapControl.Margin = new Padding(4);
            fMapControl.MaxZoom = 17;
            fMapControl.MinZoom = 2;
            fMapControl.Zoom = 0D;
            Controls.Add(fMapControl);

            if (!GMapControl.IsDesignerHosted) {
                var proxy = GlobalOptions.Instance.Proxy;
                if (proxy.UseProxy) {
                    GMapProvider.IsSocksProxy = true;
                    GMapProvider.WebProxy = new WebProxy(proxy.Server, int.Parse(proxy.Port));
                    GMapProvider.WebProxy.Credentials = new NetworkCredential(proxy.Login, proxy.Password);
                } else {
                    GMapProvider.WebProxy = WebRequest.DefaultWebProxy;
                }

                // set cache mode only if no internet available
                //fMapControl.Manager.Mode = (!PingNetwork("pingtest.com")) ? AccessMode.CacheOnly : AccessMode.ServerAndCache;

                // config map         
                fMapControl.MapProvider = GMapProviders.GoogleMap;
                fMapControl.MinZoom = 0;
                fMapControl.MaxZoom = 24;
                fMapControl.Zoom = 9;

                fMapControl.Manager.UseGeocoderCache = true;
                fMapControl.Manager.UsePlacemarkCache = true;

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
                fMapControl.KeyUp += MainForm_KeyUp;

                // set current marker
                fTargetMarker = new GMarkerIcon(fMapControl.Position, GMarkerIconType.arrow);
                fTargetMarker.IsHitTestVisible = false;
                fTargetMarker.IsVisible = true;
                fTopOverlay.Markers.Add(fTargetMarker);

                // add start location
                GeocoderStatusCode status;
                PointLatLng? pos = GMapProviders.GoogleMap.GetPoint("Russia, Moscow", out status);
                if (pos != null && status == GeocoderStatusCode.Success) {
                    fTargetMarker.Position = pos.Value;
                    fMapControl.ZoomAndCenterMarkers(null);
                }
            }
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
            GeocoderStatusCode status = GeocoderStatusCode.Unknown;
            PointLatLng? pos = GMapProviders.GoogleMap.GetPoint(place, out status);
            if (pos != null && status == GeocoderStatusCode.Success) {
                GMarkerIcon m = new GMarkerIcon(pos.Value, GMarkerIconType.green);
                m.ToolTip = new GMapRoundedToolTip(m);
                m.ToolTipText = place;
                m.ToolTipMode = MarkerTooltipMode.Always;

                fObjects.Markers.Add(m);
            }
        }

        private void AddMarker(PointLatLng position, string toolTip = "")
        {
            GMarkerIcon m = new GMarkerIcon(position, GMarkerIconType.green_small);
            if (!string.IsNullOrEmpty(toolTip)) {
                m.ToolTipMode = MarkerTooltipMode.Always;
                m.ToolTipText = toolTip;
            }
            fObjects.Markers.Add(m);
        }

        private void AddMarkerAndSearchTooltip(PointLatLng position)
        {
            Placemark? p = null;
            GeocoderStatusCode status;
            var ret = GMapProviders.GoogleMap.GetPlacemark(position, out status);
            if (status == GeocoderStatusCode.Success && ret != null) {
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
        }

        private void MainMap_OnMarkerClick(GMapMarker item, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left) {
                GeocoderStatusCode status;
                var pos = GMapProviders.GoogleMap.GetPlacemark(item.Position, out status);
                if (status == GeocoderStatusCode.Success && pos != null) {
                    item.ToolTipText = pos.Value.Address;
                    fMapControl.Invalidate(false);
                }
            }
        }

        private void MainMap_OnPositionChanged(PointLatLng point)
        {
            // dummy
        }

        private void MainForm_KeyUp(object sender, KeyEventArgs e)
        {
            const int offset = -22;

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

                case Keys.Add:
                    fMapControl.Zoom = ((int)fMapControl.Zoom) + 1;
                    break;

                case Keys.Subtract:
                    fMapControl.Zoom = ((int)(fMapControl.Zoom + 0.99)) - 1;
                    break;
            }
        }

        #endregion

        #endregion
    }
}
