using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;
using GKMap.MapProviders;

namespace GKMap.WinForms.Demo
{
    public sealed class GMapBrowser : UserControl
    {
        private readonly GMapOverlay fObjects = new GMapOverlay("objects");
        private readonly GMapOverlay fPolygons = new GMapOverlay("polygons");
        private readonly GMapOverlay fRoutes = new GMapOverlay("routes");
        private readonly GMapOverlay fTopOverlay = new GMapOverlay();

        private GMapPolygon fCurrentPolygon;
        private GMapRoute fCurrentRoute;
        private bool fIsMouseDown;
        private PointLatLng fLastPosition;
        private int fLastZoom;
        private GMapControl fMapControl;
        private GMapMarker fTargetMarker;

        private TableLayoutPanel fToolsPanel;
        private TableLayoutPanel fPanel;
        private ComboBox cmbMapType;
        private Label lblMapType;
        private Button btnSaveView;
        private Button btnZoomCenter;
        private Button btnClearAll;
        private TrackBar trkZoom;
        private Button btnZoomUp;
        private Button btnZoomDown;
        private TableLayoutPanel panZoom;

        public IMapControl MapControl
        {
            get { return fMapControl; }
        }

        public PointLatLng TargetPosition
        {
            get { return fTargetMarker.Position; }
        }

        private void InitializeComponent()
        {
            fMapControl = new GMapControl {
                CanDragMap = true,
                Dock = DockStyle.Fill,
                EmptyTileColor = Color.Navy,
                HelperLineOption = HelperLineOptions.DontShow,
                LevelsKeepInMemmory = 5,
                Location = new Point(0, 0),
                Margin = new Padding(4),
                MarkersEnabled = true,
                MaxZoom = 17,
                MinZoom = 2,
                MouseWheelZoomType = MouseWheelZoomType.MousePositionAndCenter,
                PolygonsEnabled = true,
                RetryLoadTile = 0,
                RoutesEnabled = true,
                ScaleMode = ScaleModes.Integer,
                SelectedAreaFillColor = Color.FromArgb(33, 65, 105, 225),
                ShowTileGridLines = false,
                Size = new Size(881, 818),
                TabIndex = 0,
                Zoom = 0D
            };

            lblMapType = new Label {
                AutoSize = true,
                Location = new Point(176, 27),
                Margin = new Padding(4, 0, 4, 0),
                Size = new Size(35, 17),
                Text = "type"
            };

            cmbMapType = new ComboBox {
                DropDownStyle = ComboBoxStyle.DropDownList,
                FormattingEnabled = true,
                Location = new Point(11, 23),
                Margin = new Padding(4),
                Size = new Size(163, 24),
                TabIndex = 9
            };
            cmbMapType.DropDownClosed += cmbMapType_DropDownClosed;

            btnSaveView = new Button {
                Location = new Point(123, 144),
                Margin = new Padding(3, 2, 3, 2),
                Size = new Size(92, 30),
                TabIndex = 39,
                Text = "Save View",
                UseVisualStyleBackColor = true
            };
            btnSaveView.Click += btnSaveView_Click;

            btnZoomCenter = new Button {
                Location = new Point(8, 55),
                Margin = new Padding(4),
                Size = new Size(109, 30),
                TabIndex = 15,
                Text = "Zoom Center",
                UseVisualStyleBackColor = true
            };
            btnZoomCenter.Click += btnZoomCenter_Click;

            btnClearAll = new Button {
                Location = new Point(125, 55),
                Margin = new Padding(4),
                Size = new Size(84, 30),
                TabIndex = 13,
                Text = "Clear All",
                UseVisualStyleBackColor = true
            };
            btnClearAll.Click += btnClearAll_Click;

            fToolsPanel = new TableLayoutPanel();
            fToolsPanel.AutoSize = true;
            fToolsPanel.ColumnCount = 5;
            //fToolsPanel.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 100F));
            fToolsPanel.Controls.Add(btnClearAll, 4, 0);
            fToolsPanel.Controls.Add(btnZoomCenter, 3, 0);
            fToolsPanel.Controls.Add(btnSaveView, 2, 0);
            fToolsPanel.Controls.Add(cmbMapType, 1, 0);
            fToolsPanel.Controls.Add(lblMapType, 0, 0);
            fToolsPanel.Dock = DockStyle.Fill;
            fToolsPanel.Location = new Point(64, 0);
            fToolsPanel.Margin = new Padding(0, 0, 3, 2);
            fToolsPanel.RowCount = 5;
            fToolsPanel.RowStyles.Add(new RowStyle());
            fToolsPanel.RowStyles.Add(new RowStyle());
            fToolsPanel.RowStyles.Add(new RowStyle());
            fToolsPanel.RowStyles.Add(new RowStyle());
            fToolsPanel.RowStyles.Add(new RowStyle(SizeType.Absolute, 7F));
            fToolsPanel.Size = new Size(235, 687);
            fToolsPanel.TabIndex = 30;

            btnZoomUp = new Button {
                Dock = DockStyle.Fill,
                Font = new Font("Microsoft Sans Serif", 12F, FontStyle.Bold, GraphicsUnit.Point, 0),
                Location = new Point(0, 0),
                Margin = new Padding(0),
                Size = new Size(58, 33),
                TabIndex = 0,
                Text = "+",
                UseVisualStyleBackColor = true
            };
            btnZoomUp.Click += btnZoomUp_Click;

            trkZoom = new TrackBar {
                BackColor = Color.AliceBlue,
                Dock = DockStyle.Fill,
                LargeChange = 1,
                Location = new Point(0, 33),
                Margin = new Padding(0),
                Maximum = 1700,
                Minimum = 1,
                Orientation = Orientation.Vertical,
                Size = new Size(58, 617),
                TabIndex = 29,
                TickFrequency = 100,
                TickStyle = TickStyle.TopLeft,
                Value = 12
            };
            trkZoom.ValueChanged += trkZoom_ValueChanged;

            btnZoomDown = new Button {
                Dock = DockStyle.Fill,
                Font = new Font("Microsoft Sans Serif", 12F, FontStyle.Bold, GraphicsUnit.Point, 0),
                Location = new Point(0, 650),
                Margin = new Padding(0),
                Size = new Size(58, 33),
                TabIndex = 1,
                Text = "-",
                UseVisualStyleBackColor = true
            };
            btnZoomDown.Click += btnZoomDown_Click;

            panZoom = new TableLayoutPanel();
            panZoom.ColumnCount = 1;
            panZoom.ColumnStyles.Add(new ColumnStyle());
            panZoom.Controls.Add(btnZoomUp, 0, 0);
            panZoom.Controls.Add(trkZoom, 0, 1);
            panZoom.Controls.Add(btnZoomDown, 0, 2);
            panZoom.Dock = DockStyle.Fill;
            panZoom.Location = new Point(3, 3);
            panZoom.RowCount = 3;
            panZoom.RowStyles.Add(new RowStyle());
            panZoom.RowStyles.Add(new RowStyle(SizeType.Percent, 50F));
            panZoom.RowStyles.Add(new RowStyle());
            panZoom.Size = new Size(58, 683);
            panZoom.TabIndex = 31;

            fPanel = new TableLayoutPanel();
            fPanel.ColumnCount = 2;
            fPanel.ColumnStyles.Add(new ColumnStyle(SizeType.Percent, 100F));
            fPanel.ColumnStyles.Add(new ColumnStyle(SizeType.AutoSize));
            fPanel.Controls.Add(panZoom, 1, 1);
            fPanel.Controls.Add(fMapControl, 0, 1);
            fPanel.Controls.Add(fToolsPanel, 0, 0);
            fPanel.Dock = DockStyle.Fill;
            fPanel.Location = new Point(0, 0);
            fPanel.Margin = new Padding(0, 0, 3, 2);
            fPanel.RowCount = 2;
            fPanel.RowStyles.Add(new RowStyle(SizeType.AutoSize));
            fPanel.RowStyles.Add(new RowStyle());
            fPanel.Size = new Size(235, 687);

            Controls.Add(fPanel);
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
                if (!GMapControl.PingNetwork("pingtest.com")) {
                    fMapControl.Manager.Mode = AccessMode.CacheOnly;
                    MessageBox.Show("No internet connection available, going to CacheOnly mode.", "GKMap", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                } else {
                    fMapControl.Manager.Mode = AccessMode.ServerAndCache;
                }

                // config map         
                fMapControl.MapProvider = GMapProviders.OpenStreetMap;
                fMapControl.Position = new PointLatLng(30.0447272077905, 31.2361907958984);
                fMapControl.MinZoom = 0;
                fMapControl.MaxZoom = 24;
                fMapControl.Zoom = 9;

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

                fMapControl.CanDragMap = true;
                fMapControl.ShowTileGridLines = false;
                //fMapControl.ScaleMode = ScaleModes.Fractional;
                //fMapControl.VirtualSizeEnabled = true;

                fMapControl.Manager.UseGeocoderCache = true;
                fMapControl.Manager.UsePlacemarkCache = true;

                // get zoom  
                trkZoom.Minimum = fMapControl.MinZoom * 100;
                trkZoom.Maximum = fMapControl.MaxZoom * 100;
                trkZoom.TickFrequency = 100;

                // set current marker
                fTargetMarker = new GMarkerIcon(fMapControl.Position, GMarkerIconType.arrow);
                fTargetMarker.IsHitTestVisible = false;
                fTargetMarker.IsVisible = true;
                fTopOverlay.Markers.Add(fTargetMarker);

                // add my city location for demo
                GeoCoderStatusCode status;
                PointLatLng? pos = GMapProviders.GoogleMap.GetPoint("Egypt, Cairo", out status);
                if (pos != null && status == GeoCoderStatusCode.G_GEO_SUCCESS) {
                    fTargetMarker.Position = pos.Value;

                    GMapMarker myCity = new GMarkerIcon(pos.Value, GMarkerIconType.green_small);
                    myCity.ToolTipMode = MarkerTooltipMode.Always;
                    myCity.ToolTipText = "Welcome to Egypt! ;}";
                    fObjects.Markers.Add(myCity);
                }

                if (fObjects.Markers.Count > 0) {
                    fMapControl.ZoomAndCenterMarkers(null);
                }

                RegeneratePolygon();
            }

            // get map types
#if !MONO   // mono doesn't handle it, so we 'lost' provider list ;]
            cmbMapType.ValueMember = "Name";
            cmbMapType.DataSource = GMapProviders.List;
            cmbMapType.SelectedItem = fMapControl.MapProvider;
#endif

            if (fMapControl.Zoom >= fMapControl.MinZoom && fMapControl.Zoom <= fMapControl.MaxZoom) {
                trkZoom.Value = (int)fMapControl.Zoom * 100;
            }
        }

        #region Event handlers

        // change map type
        private void cmbMapType_DropDownClosed(object sender, EventArgs e)
        {
            fMapControl.MapProvider = cmbMapType.SelectedItem as GMapProvider;
        }

        // saves current map view
        private void btnSaveView_Click(object sender, EventArgs e)
        {
            try {
                using (SaveFileDialog sfd = new SaveFileDialog()) {
                    sfd.Filter = "PNG (*.png)|*.png";
                    sfd.FileName = "GKMap image";

                    Image tmpImage = fMapControl.ToImage();
                    if (tmpImage != null) {
                        using (tmpImage) {
                            if (sfd.ShowDialog() == DialogResult.OK) {
                                tmpImage.Save(sfd.FileName);

                                MessageBox.Show("Image saved: " + sfd.FileName, "GKMap", MessageBoxButtons.OK, MessageBoxIcon.Information);
                            }
                        }
                    }
                }
            } catch (Exception ex) {
                MessageBox.Show("Image failed to save: " + ex.Message, "GKMap", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        // clear markers and all
        private void btnClearAll_Click(object sender, EventArgs e)
        {
            fRoutes.Routes.Clear();
            fPolygons.Polygons.Clear();
            fObjects.Markers.Clear();
        }

        // zoom to max for markers
        private void btnZoomCenter_Click(object sender, EventArgs e)
        {
            fMapControl.ZoomAndCenterMarkers("objects");
        }

        private void RegeneratePolygon()
        {
            List<PointLatLng> polygonPoints = new List<PointLatLng>();
            foreach (GMapMarker m in fObjects.Markers) {
                polygonPoints.Add(m.Position);
            }

            AddRoute(polygonPoints);
        }

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
            fCurrentPolygon = null;
            item.Stroke.Color = Color.MidnightBlue;
        }

        private void MainMap_OnPolygonEnter(GMapPolygon item)
        {
            fCurrentPolygon = item;
            item.Stroke.Color = Color.Red;
        }

        private void MainMap_OnRouteLeave(GMapRoute item)
        {
            fCurrentRoute = null;
            item.Stroke.Color = Color.MidnightBlue;
        }

        private void MainMap_OnRouteEnter(GMapRoute item)
        {
            fCurrentRoute = item;
            item.Stroke.Color = Color.Red;
        }

        private void MainMap_OnMapTypeChanged(GMapProvider type)
        {
            cmbMapType.SelectedItem = type;
            trkZoom.Minimum = fMapControl.MinZoom * 100;
            trkZoom.Maximum = fMapControl.MaxZoom * 100;
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
            trkZoom.Value = (int)(fMapControl.Zoom * 100.0);
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
            lock (this) {
                fLastPosition = point;
                fLastZoom = (int)fMapControl.Zoom;
            }
        }

        private void trkZoom_ValueChanged(object sender, EventArgs e)
        {
            fMapControl.Zoom = trkZoom.Value / 100.0;
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
                    if (fCurrentPolygon != null) {
                        fPolygons.Polygons.Remove(fCurrentPolygon);
                        fCurrentPolygon = null;
                    }
                    if (fCurrentRoute != null) {
                        fRoutes.Routes.Remove(fCurrentRoute);
                        fCurrentRoute = null;
                    }
                    if (fTargetMarker != null) {
                        fObjects.Markers.Remove(fTargetMarker);
                        fTargetMarker = null;

                        RegeneratePolygon();
                    }
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

        public void AddRoute(List<PointLatLng> points)
        {
            if (fCurrentRoute == null) {
                fCurrentRoute = new GMapRoute(points, "route test");
                fCurrentRoute.IsHitTestVisible = true;
                fRoutes.Routes.Add(fCurrentRoute);
            } else {
                fCurrentRoute.Points.Clear();
                fCurrentRoute.Points.AddRange(points);

                if (fRoutes.Routes.Count == 0) {
                    fRoutes.Routes.Add(fCurrentRoute);
                } else {
                    fMapControl.UpdateRouteLocalPosition(fCurrentRoute);
                }
            }
        }

        public void AddPolygon(List<PointLatLng> points)
        {
            if (fCurrentPolygon == null) {
                fCurrentPolygon = new GMapPolygon(points, "polygon test");
                fCurrentPolygon.IsHitTestVisible = true;
                fPolygons.Polygons.Add(fCurrentPolygon);
            } else {
                fCurrentPolygon.Points.Clear();
                fCurrentPolygon.Points.AddRange(points);

                if (fPolygons.Polygons.Count == 0) {
                    fPolygons.Polygons.Add(fCurrentPolygon);
                } else {
                    fMapControl.UpdatePolygonLocalPosition(fCurrentPolygon);
                }
            }
        }

        /// <summary>
        /// adds marker using geocoder
        /// </summary>
        /// <param name="place"></param>
        public void AddLocationMarker(string place)
        {
            GeoCoderStatusCode status;
            PointLatLng? pos = GMapProviders.GoogleMap.GetPoint(place, out status);
            if (pos != null && status == GeoCoderStatusCode.G_GEO_SUCCESS) {
                var m = new GMarkerIcon(pos.Value, GMarkerIconType.green);
                m.ToolTip = new GMapRoundedToolTip(m);
                m.ToolTipText = place;
                m.ToolTipMode = MarkerTooltipMode.Always;

                fObjects.Markers.Add(m);
            }
        }

        public void AddMarker(PointLatLng targetPosition)
        {
            //var m = new GMarkerCross(targetPosition);
            var m = new GMarkerIcon(targetPosition, GMarkerIconType.green);

            //m.ToolTip = new GMapBaloonToolTip(m);
            m.ToolTipMode = MarkerTooltipMode.OnMouseOver;

            Placemark? p = null;
            GeoCoderStatusCode status;
            var ret = GMapProviders.GoogleMap.GetPlacemark(targetPosition, out status);
            if (status == GeoCoderStatusCode.G_GEO_SUCCESS && ret != null) {
                p = ret;
            }

            m.ToolTipText = (p != null) ? p.Value.Address : fTargetMarker.Position.ToString();

            fObjects.Markers.Add(m);

            RegeneratePolygon();
        }
    }
}
