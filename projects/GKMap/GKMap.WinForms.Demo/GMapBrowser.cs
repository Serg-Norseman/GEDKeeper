using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;
using GKMap.MapObjects;
using GKMap.MapProviders;

namespace GKMap.WinForms.Demo
{
    public sealed class GMapBrowser : UserControl
    {
        private readonly GMapOverlay fObjects = new GMapOverlay("objects");
        private readonly GMapOverlay fTopOverlay = new GMapOverlay();

        private GMapPolygon fCurrentPolygon;
        private GMapRoute fCurrentRoute;
        private bool fIsMouseDown;
        private GMapControl fMapControl;
        private MapMarker fTargetMarker;

        public GMapControl MapControl
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
        }

        private void InitializeComponent()
        {
            fMapControl = new GMapControl();
            fMapControl.Dock = DockStyle.Fill;
            fMapControl.Location = new Point(0, 0);
            fMapControl.Margin = new Padding(4);
            fMapControl.Size = new Size(881, 818);
            fMapControl.TabIndex = 0;

            Controls.Add(fMapControl);
        }

        public GMapBrowser()
        {
            InitializeComponent();

            if (!GMapControl.IsDesignerHosted) {
                // set your proxy here if need
                //GMapProvider.IsSocksProxy = true;
                //GMapProvider.WebProxy = new WebProxy("127.0.0.1", 1080);
                //GMapProvider.WebProxy.Credentials = new NetworkCredential("ogrenci@bilgeadam.com", "bilgeada");
                // or
                //GMapProvider.WebProxy = WebRequest.DefaultWebProxy;

                // set cache mode only if no internet available
                if (!GMaps.PingNetwork("google.com")) {
                    //fMapControl.Manager.Mode = AccessMode.CacheOnly;
                    MessageBox.Show("No internet connection available, going to CacheOnly mode.", "GKMap", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                } else {
                    //fMapControl.Manager.Mode = AccessMode.ServerAndCache;
                }

                // config map
                fMapControl.MapProvider = GMapProviders.OpenStreetMap;
                fMapControl.Position = new PointLatLng(30.0447272077905, 31.2361907958984);
                fMapControl.MinZoom = 0;
                fMapControl.MaxZoom = 24;
                fMapControl.Zoom = 9;

                // add custom layers  
                fMapControl.Overlays.Add(fObjects);
                fMapControl.Overlays.Add(fTopOverlay);

                // map events
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

                // add start location
                GeocoderStatusCode status;
                PointLatLng? pos = GMapProviders.GoogleMap.GetPoint("Egypt, Cairo", out status);
                if (pos != null && status == GeocoderStatusCode.Success) {
                    fTargetMarker.Position = pos.Value;

                    MapMarker myCity = new GMarkerIcon(pos.Value, GMarkerIconType.green_small);
                    myCity.ToolTipMode = MarkerTooltipMode.Always;
                    myCity.ToolTipText = "Welcome to Egypt! ;}";
                    fObjects.Markers.Add(myCity);
                }

                if (fObjects.Markers.Count > 0) {
                    fMapControl.ZoomAndCenterMarkers(null);
                }

                RegeneratePolygon();
            }
        }

        private void RegeneratePolygon()
        {
            List<PointLatLng> polygonPoints = new List<PointLatLng>();
            foreach (MapMarker m in fObjects.Markers) {
                polygonPoints.Add(m.Position);
            }

            AddRoute("route test", polygonPoints);
            //AddPolygon("polygon test", polygonPoints);
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

        public void AddMarker(PointLatLng position)
        {
            var m = new GMarkerIcon(position, GMarkerIconType.green);
            m.ToolTipMode = MarkerTooltipMode.OnMouseOver;

            Placemark? p = null;
            GeocoderStatusCode status;
            var ret = GMapProviders.GoogleMap.GetPlacemark(position, out status);
            if (status == GeocoderStatusCode.Success && ret != null) {
                p = ret;
            }

            m.ToolTipText = (p != null) ? p.Value.Address : position.ToString();

            fObjects.Markers.Add(m);

            RegeneratePolygon();
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

        #region Event handlers

        private void MainMap_OnMarkerLeave(MapMarker item)
        {
            // dummy
        }

        private void MainMap_OnMarkerEnter(MapMarker item)
        {
            // dummy
        }

        private void MainMap_OnPolygonLeave(MapPolygon item)
        {
            fCurrentPolygon = null;
            ((GMapPolygon)item).Stroke.Color = Color.MidnightBlue;
        }

        private void MainMap_OnPolygonEnter(MapPolygon item)
        {
            fCurrentPolygon = item as GMapPolygon;
            ((GMapPolygon)item).Stroke.Color = Color.Red;
        }

        private void MainMap_OnRouteLeave(MapRoute item)
        {
            fCurrentRoute = null;
            ((GMapRoute)item).Stroke.Color = Color.MidnightBlue;
        }

        private void MainMap_OnRouteEnter(MapRoute item)
        {
            fCurrentRoute = item as GMapRoute;
            ((GMapRoute)item).Stroke.Color = Color.Red;
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

        private void MainMap_OnMarkerClick(MapMarker item, MouseEventArgs e)
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
                    if (fCurrentPolygon != null) {
                        fObjects.Polygons.Remove(fCurrentPolygon);
                        fCurrentPolygon = null;
                    }
                    if (fCurrentRoute != null) {
                        fObjects.Routes.Remove(fCurrentRoute);
                        fCurrentRoute = null;
                    }
                    if (fTargetMarker != null) {
                        fObjects.Markers.Remove(fTargetMarker);
                        fTargetMarker = null;

                        RegeneratePolygon();
                    }
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
    }
}
