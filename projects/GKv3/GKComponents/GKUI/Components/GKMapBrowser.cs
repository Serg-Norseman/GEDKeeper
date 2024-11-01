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

#pragma warning disable CS0618

using System.Collections.Generic;
using System.Net;
using Eto.Drawing;
using Eto.Forms;
using GKCore.Design.Controls;
using GKCore.Maps;
using GKCore.Options;
using GKMap;
using GKMap.EtoForms;
using GKMap.MapObjects;
using GKMap.MapProviders;

namespace GKUI.Components
{
    /// <summary>
    ///
    /// </summary>
    public sealed class GKMapBrowser : Panel, IMapBrowser
    {
        private readonly List<GeoPoint> fMapPoints;
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

        public IList<GeoPoint> MapPoints
        {
            get { return fMapPoints; }
        }

        public GKMapBrowser()
        {
            InitControl();

            fMapPoints = new List<GeoPoint>();
            fUpdateCount = 0;
            fShowPoints = true;
            fShowLines = true;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                ClearPoints();
            }
            base.Dispose(disposing);
        }

        public void Activate()
        {
            Focus();
        }

        public int AddPoint(double latitude, double longitude, string hint)
        {
            BeginUpdate();
            GeoPoint pt = new GeoPoint(latitude, longitude, hint);
            int res = fMapPoints.Count;
            fMapPoints.Add(pt);
            EndUpdate();

            return res;
        }

        public int AddPoint(GeoPoint pt)
        {
            BeginUpdate();
            int res = fMapPoints.Count;
            fMapPoints.Add(pt);
            EndUpdate();

            return res;
        }

        public void ClearPoints()
        {
            fObjects.Clear();
            fMapPoints.Clear();
        }

        public void DeletePoint(int index)
        {
            fMapPoints.RemoveAt(index);
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
            fObjects.Clear();
            if (fMapPoints.Count <= 0) return;

            var points = new List<PointLatLng>();

            int num = fMapPoints.Count;
            for (int i = 0; i < num; i++) {
                GeoPoint pt = fMapPoints[i];

                var point = new PointLatLng(pt.Latitude, pt.Longitude);

                if (fShowPoints) {
                    AddMarker(point, GMarkerIconType.green, MarkerTooltipMode.OnMouseOver, pt.Hint);
                    if (i == num - 1) {
                        SetCenter(pt.Latitude, pt.Longitude, -1);
                    }
                }

                if (fShowLines) {
                    points.Add(point);
                }
            }

            if (fShowLines) {
                AddRoute("route", points);
            }

            ZoomToBounds();
        }

        public void SetCenter(double latitude, double longitude, int scale)
        {
            fTargetMarker.Position = new PointLatLng(latitude, longitude);
            ZoomToBounds();
        }

        public void ZoomToBounds()
        {
            fMapControl.ZoomAndCenterMarkers(null);
        }

        #region Inner control

        private readonly GMapOverlay fObjects = new GMapOverlay("objects");
        private readonly GMapOverlay fTopOverlay = new GMapOverlay();

        private MapObject fCurrentObj;
        private bool fIsMouseDown;
        private GMapControl fMapControl;
        private MapMarker fTargetMarker;

        public IMapControl MapControl
        {
            get { return fMapControl; }
        }

        public GMapOverlay Objects
        {
            get { return fObjects; }
        }

        public PointLatLng TargetPosition
        {
            get { return fTargetMarker.Position; }
            set { fTargetMarker.Position = value; }
        }

        private void InitControl()
        {
            Padding = new Padding(4);

            fMapControl = new GMapControl();
            fMapControl.MaxZoom = 17;
            fMapControl.MinZoom = 2;
            fMapControl.Zoom = 0;
            Content = fMapControl;

            if (!GMapControl.IsDesignerHosted) {
                var proxy = GlobalOptions.Instance.Proxy;
                if (proxy.UseProxy) {
                    GMapProvider.IsSocksProxy = true;
                    GMapProvider.WebProxy = new WebProxy(proxy.Server, int.Parse(proxy.Port));
                    GMapProvider.WebProxy.Credentials = new NetworkCredential(proxy.Login, proxy.Password);
                } else {
                    GMapProvider.WebProxy = WebRequest.DefaultWebProxy;
                }

                // config map
                fMapControl.MapProvider = GMapProviders.GoogleMap;
                fMapControl.Position = new PointLatLng(30.0447272077905, 31.2361907958984);
                fMapControl.MinZoom = 0;
                fMapControl.MaxZoom = 24;
                fMapControl.Zoom = 9;

                // add custom layers
                fMapControl.Overlays.Add(fObjects);
                fMapControl.Overlays.Add(fTopOverlay);

                // map events
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
                fMapControl.KeyUp += MainForm_KeyUp;

                // set current marker
                fTargetMarker = new GMarkerIcon(fMapControl.Position, GMarkerIconType.arrow);
                fTargetMarker.IsHitTestVisible = false;
                fTargetMarker.IsVisible = true;
                fTopOverlay.Markers.Add(fTargetMarker);
            }
        }

        public void GeneratePolygon()
        {
            var points = new List<PointLatLng>();
            foreach (MapMarker m in fObjects.Markers) {
                points.Add(m.Position);
            }
            AddPolygon("polygon test", points);
        }

        public void GenerateRoute()
        {
            var points = new List<PointLatLng>();
            foreach (MapMarker m in fObjects.Markers) {
                points.Add(m.Position);
            }
            AddRoute("route test", points);
        }

        public GMapRoute AddRoute(string name, List<PointLatLng> points)
        {
            var route = new GMapRoute(name, points);
            route.IsHitTestVisible = true;
            fObjects.Routes.Add(route);
            return route;
        }

        public GMapPolygon AddPolygon(string name, List<PointLatLng> points)
        {
            var polygon = new GMapPolygon(name, points);
            polygon.IsHitTestVisible = true;
            fObjects.Polygons.Add(polygon);
            return polygon;
        }

        /// <summary>
        /// adds marker using geocoder
        /// </summary>
        /// <param name="place"></param>
        public void AddLocationMarker(string place)
        {
            GeocoderStatusCode status;
            PointLatLng? pos = GMapProviders.GoogleMap.GetPoint(place, out status);
            if (pos != null && status == GeocoderStatusCode.Success) {
                var m = new GMarkerIcon(pos.Value, GMarkerIconType.green);
                m.ToolTipText = place;
                m.ToolTipMode = MarkerTooltipMode.Always;

                fObjects.Markers.Add(m);
            }
        }

        public void AddMarker(PointLatLng position, GMarkerIconType iconType, MarkerTooltipMode tooltipMode, string toolTip = "")
        {
            var m = new GMarkerIcon(position, iconType); // GMarkerIconType.green
            m.ToolTipMode = tooltipMode; // MarkerTooltipMode.OnMouseOver

            if (!string.IsNullOrEmpty(toolTip)) {
                m.ToolTipText = toolTip;
            } else {
                Placemark? p = null;
                GeocoderStatusCode status;
                var ret = GMapProviders.GoogleMap.GetPlacemark(position, out status);
                if (status == GeocoderStatusCode.Success && ret != null) {
                    p = ret;
                }
                m.ToolTipText = (p != null) ? p.Value.Address : position.ToString();
            }

            fObjects.Markers.Add(m);
        }

        public void SaveSnapshot(string fileName)
        {
            var tmpImage = fMapControl.ToImage();
            if (tmpImage != null) {
                using (tmpImage) {
                    tmpImage.Save(fileName, ImageFormat.Bitmap);
                }
            }
        }

        #region Event handlers

        private void MainMap_OnMarkerLeave(MapMarker item)
        {
            fCurrentObj = null;
        }

        private void MainMap_OnMarkerEnter(MapMarker item)
        {
            fCurrentObj = item;
        }

        private void MainMap_OnPolygonLeave(MapPolygon item)
        {
            fCurrentObj = null;
            ((GMapPolygon)item).Stroke.Color = Colors.MidnightBlue;
        }

        private void MainMap_OnPolygonEnter(MapPolygon item)
        {
            fCurrentObj = item;
            ((GMapPolygon)item).Stroke.Color = Colors.Red;
        }

        private void MainMap_OnRouteLeave(MapRoute item)
        {
            fCurrentObj = null;
            ((GMapRoute)item).Stroke.Color = Colors.MidnightBlue;
        }

        private void MainMap_OnRouteEnter(MapRoute item)
        {
            fCurrentObj = item;
            ((GMapRoute)item).Stroke.Color = Colors.Red;
        }

        private void MainMap_OnMapTypeChanged(GMapProvider type)
        {
            fMapControl.ZoomAndCenterMarkers("objects");
        }

        private void MainMap_MouseUp(object sender, MouseEventArgs e)
        {
            if (e.Buttons == MouseButtons.Primary) {
                fIsMouseDown = false;
            }
        }

        private void MainMap_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Buttons == MouseButtons.Primary) {
                fIsMouseDown = true;
                if (fTargetMarker.IsVisible) {
                    Point mpt = e.Location.ToPoint();
                    fTargetMarker.Position = fMapControl.FromLocalToLatLng(mpt.X, mpt.Y);
                }
            }
        }

        private void MainMap_MouseMove(object sender, MouseEventArgs e)
        {
            if (e.Buttons == MouseButtons.Primary && fIsMouseDown) {
                if (fTargetMarker.IsVisible) {
                    Point mpt = e.Location.ToPoint();
                    fTargetMarker.Position = fMapControl.FromLocalToLatLng(mpt.X, mpt.Y);
                }

                fMapControl.Refresh(); // force instant invalidation
            }
        }

        private void MainMap_OnMarkerClick(MapMarker item, MouseEventArgs e)
        {
            if (e.Buttons == MouseButtons.Primary) {
                GeocoderStatusCode status;
                var pos = GMapProviders.GoogleMap.GetPlacemark(item.Position, out status);
                if (status == GeocoderStatusCode.Success && pos != null) {
                    item.ToolTipText = pos.Value.Address;
                    fMapControl.Invalidate(false);
                }
            }
        }

        private void MainForm_KeyUp(object sender, KeyEventArgs e)
        {
            const int offset = -22;

            switch (e.Key) {
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
                    if (fCurrentObj != null) {
                        if (fCurrentObj is MapPolygon)
                            fObjects.Polygons.Remove(fCurrentObj as MapPolygon);
                        if (fCurrentObj is MapRoute)
                            fObjects.Routes.Remove(fCurrentObj as MapRoute);
                        if (fCurrentObj is MapMarker)
                            fObjects.Markers.Remove(fCurrentObj as MapMarker);
                        fCurrentObj = null;
                    }
                    break;

                case Keys.Add:
                    fMapControl.Zoom = fMapControl.Zoom + 1;
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
