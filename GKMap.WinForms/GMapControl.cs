/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

//#define DEBUG_TILE_COORDS
//#define DEBUG_CENTER
//#define DEBUG_RENDER

using System;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.Drawing.Text;
using System.Globalization;
using System.IO;
using System.Net.NetworkInformation;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows.Forms;
using GKMap.MapProviders;

namespace GKMap.WinForms
{
    /// <summary>
    /// GKMap control for Windows Forms
    /// </summary>   
    public class GMapControl : UserControl, IMapControl
    {
        private Bitmap fBackBuffer;
        private Cursor fCursorBefore = Cursors.Default;
        private Graphics fGxOff;
        private bool fIsDragging;
        private bool fIsMouseOverMarker;
        private bool fIsMouseOverPolygon;
        private bool fIsMouseOverRoute;
        private bool fLazyEvents = true;
        private RectLatLng? fLazySetZoomToFitRect;
        private bool fMouseIn;
        private int fOverObjectCount;
        private double fZoomReal;

        private readonly StringFormat BottomFormat = new StringFormat();
        private readonly StringFormat CenterFormat = new StringFormat();
        private readonly Font CopyrightFont = new Font(FontFamily.GenericSansSerif, 7, FontStyle.Regular);
        private readonly Color EmptyMapBackground = Color.WhiteSmoke;
        private readonly Pen EmptyTileBorders = new Pen(Brushes.White, 1);
        private readonly Brush EmptyTileBrush = new SolidBrush(Color.Navy);
        private readonly string EmptyTileText = "We are sorry, but we don't\nhave imagery at this zoom\nlevel for this region.";
        private readonly Font MissingDataFont = new Font(FontFamily.GenericSansSerif, 11, FontStyle.Bold);
        private readonly Font ScaleFont = new Font(FontFamily.GenericSansSerif, 5, FontStyle.Italic);
        private readonly ImageAttributes TileFlipXYAttributes = new ImageAttributes();

#if DEBUG_CENTER
        private readonly Pen CenterPen = new Pen(Brushes.Red, 1);
        private readonly Pen ScalePen = new Pen(Brushes.Blue, 1);
#endif

        #region Properties

        /// <summary>
        /// location of cache
        /// </summary>
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        [Browsable(false)]
        public string CacheLocation
        {
            get {
#if !DESIGN
                return Cache.CacheLocation;
#else
                return string.Empty;
#endif
            }
            set {
#if !DESIGN
                Cache.CacheLocation = value;
#endif
            }
        }

        /// <summary>
        /// lets you zoom by MouseWheel even when pointer is in area of marker
        /// </summary>
        public bool IgnoreMarkerOnMouseWheel = true;

        /// <summary>
        /// reverses MouseWheel zooming direction
        /// </summary>
        public bool InvertedMouseWheelZooming = false;

        /// <summary>
        /// current map center position
        /// </summary>
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        [Browsable(false)]
        public PointLatLng Position
        {
            get {
                return Core.Position;
            }
            set {
                Core.Position = value;

                if (Core.IsStarted) {
                    ForceUpdateOverlays();
                }
            }
        }

        [Category("GKMap"), DefaultValue(0)]
        public double Zoom
        {
            get {
                return fZoomReal;
            }
            set {
                if (fZoomReal != value) {
                    Debug.WriteLine("ZoomPropertyChanged: " + fZoomReal + " -> " + value);

                    if (value > MaxZoom) {
                        fZoomReal = MaxZoom;
                    } else if (value < MinZoom) {
                        fZoomReal = MinZoom;
                    } else {
                        fZoomReal = value;
                    }

                    ZoomStep = (int)Math.Floor(value);

                    if (Core.IsStarted && !fIsDragging) {
                        ForceUpdateOverlays();
                    }
                }
            }
        }

        /// <summary>
        /// map zoom level
        /// </summary>
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        [Browsable(false)]
        internal int ZoomStep
        {
            get {
                return Core.Zoom;
            }
            set {
                if (value > MaxZoom) {
                    Core.Zoom = MaxZoom;
                } else if (value < MinZoom) {
                    Core.Zoom = MinZoom;
                } else {
                    Core.Zoom = value;
                }
            }
        }

        /// <summary>
        /// is mouse over marker
        /// </summary>
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        [Browsable(false)]
        public bool IsMouseOverMarker
        {
            get {
                return fIsMouseOverMarker;
            }
            internal set {
                fIsMouseOverMarker = value;
                fOverObjectCount += value ? 1 : -1;
            }
        }

        /// <summary>
        /// is mouse over route
        /// </summary>
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        [Browsable(false)]
        public bool IsMouseOverRoute
        {
            get {
                return fIsMouseOverRoute;
            }
            internal set {
                fIsMouseOverRoute = value;
                fOverObjectCount += value ? 1 : -1;
            }
        }

        /// <summary>
        /// is mouse over polygon
        /// </summary>
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        [Browsable(false)]
        public bool IsMouseOverPolygon
        {
            get {
                return fIsMouseOverPolygon;
            }
            internal set {
                fIsMouseOverPolygon = value;
                fOverObjectCount += value ? 1 : -1;
            }
        }

        /// <summary>
        /// gets current map view top/left coordinate, width in Lng, height in Lat
        /// </summary>
        [Browsable(false)]
        public RectLatLng ViewArea
        {
            get {
                return Core.ViewArea;
            }
        }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        [Browsable(false)]
        public GMapProvider MapProvider
        {
            get {
                return Core.Provider;
            }
            set {
                if (Core.Provider == null || !Core.Provider.Equals(value)) {
                    Debug.WriteLine("MapType: " + Core.Provider.Name + " -> " + value.Name);

                    RectLatLng viewarea = ViewArea;

                    Core.Provider = value;

                    if (Core.IsStarted) {
                        if (Core.ZoomToArea) {
                            // restore zoom rect as close as possible
                            if (viewarea != RectLatLng.Empty && viewarea != ViewArea) {
                                int bestZoom = Core.GetMaxZoomToFitRect(viewarea);
                                if (bestZoom > 0 && Zoom != bestZoom) {
                                    Zoom = bestZoom;
                                }
                            }
                        } else {
                            ForceUpdateOverlays();
                        }
                    }
                }
            }
        }

        /// <summary>
        /// gets map manager
        /// </summary>
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        [Browsable(false)]
        public GMaps Manager
        {
            get {
                return GMaps.Instance;
            }
        }

        /// <summary>
        /// max zoom
        /// </summary>         
        [Category("GKMap")]
        [Description("maximum zoom level of map")]
        public int MaxZoom
        {
            get {
                return Core.MaxZoom;
            }
            set {
                Core.MaxZoom = value;
            }
        }

        /// <summary>
        /// min zoom
        /// </summary>      
        [Category("GKMap")]
        [Description("minimum zoom level of map")]
        public int MinZoom
        {
            get {
                return Core.MinZoom;
            }
            set {
                Core.MinZoom = value;
            }
        }

        /// <summary>
        /// list of overlays, should be thread safe
        /// </summary>
        public ObservableCollectionThreadSafe<GMapOverlay> Overlays { get; }

        /// <summary>
        /// enables integrated DoubleBuffer for running on windows mobile
        /// </summary>
        public bool ForceDoubleBuffer;

        /// <summary>
        /// stops immediate marker/route/polygon invalidation;
        /// call Refresh to perform single refresh and reset invalidation state
        /// </summary>
        public bool HoldInvalidation;

        // internal stuff
        internal readonly Core Core = new Core();

#endregion

#region Events

        /// <summary>
        /// occurs when current position is changed
        /// </summary>
        public event PositionChanged OnPositionChanged
        {
            add {
                Core.OnCurrentPositionChanged += value;
            }
            remove {
                Core.OnCurrentPositionChanged -= value;
            }
        }

        /// <summary>
        /// occurs when tile set load is complete
        /// </summary>
        public event TileLoadComplete OnTileLoadComplete
        {
            add {
                Core.OnTileLoadComplete += value;
            }
            remove {
                Core.OnTileLoadComplete -= value;
            }
        }

        /// <summary>
        /// occurs when tile set is starting to load
        /// </summary>
        public event TileLoadStart OnTileLoadStart
        {
            add {
                Core.OnTileLoadStart += value;
            }
            remove {
                Core.OnTileLoadStart -= value;
            }
        }

        /// <summary>
        /// occurs on map drag
        /// </summary>
        public event MapDrag OnMapDrag
        {
            add {
                Core.OnMapDrag += value;
            }
            remove {
                Core.OnMapDrag -= value;
            }
        }

        /// <summary>
        /// occurs on map zoom changed
        /// </summary>
        public event MapZoomChanged OnMapZoomChanged
        {
            add {
                Core.OnMapZoomChanged += value;
            }
            remove {
                Core.OnMapZoomChanged -= value;
            }
        }

        /// <summary>
        /// occurs on map type changed
        /// </summary>
        public event MapTypeChanged OnMapTypeChanged
        {
            add {
                Core.OnMapTypeChanged += value;
            }
            remove {
                Core.OnMapTypeChanged -= value;
            }
        }

        /// <summary>
        /// occurs on empty tile displayed
        /// </summary>
        public event EmptyTileError OnEmptyTileError
        {
            add {
                Core.OnEmptyTileError += value;
            }
            remove {
                Core.OnEmptyTileError -= value;
            }
        }

        /// <summary>
        /// occurs when clicked on marker
        /// </summary>
        public event MarkerClick OnMarkerClick;

        /// <summary>
        /// occurs when double clicked on marker
        /// </summary>
        public event MarkerDoubleClick OnMarkerDoubleClick;

        /// <summary>
        /// occurs when clicked on polygon
        /// </summary>
        public event PolygonClick OnPolygonClick;

        /// <summary>
        /// occurs when double clicked on polygon
        /// </summary>
        public event PolygonDoubleClick OnPolygonDoubleClick;

        /// <summary>
        /// occurs when clicked on route
        /// </summary>
        public event RouteClick OnRouteClick;

        /// <summary>
        /// occurs when double clicked on route
        /// </summary>
        public event RouteDoubleClick OnRouteDoubleClick;

        /// <summary>
        /// occurs on mouse enters route area
        /// </summary>
        public event RouteEnter OnRouteEnter;

        /// <summary>
        /// occurs on mouse leaves route area
        /// </summary>
        public event RouteLeave OnRouteLeave;

        /// <summary>
        /// occurs on mouse enters marker area
        /// </summary>
        public event MarkerEnter OnMarkerEnter;

        /// <summary>
        /// occurs on mouse leaves marker area
        /// </summary>
        public event MarkerLeave OnMarkerLeave;

        /// <summary>
        /// occurs on mouse enters Polygon area
        /// </summary>
        public event PolygonEnter OnPolygonEnter;

        /// <summary>
        /// occurs on mouse leaves Polygon area
        /// </summary>
        public event PolygonLeave OnPolygonLeave;

#endregion


#if !DESIGN
        /// <summary>
        /// constructor
        /// </summary>
        public GMapControl()
        {
            Overlays = new ObservableCollectionThreadSafe<GMapOverlay>();

            MaxZoom = 17;
            MinZoom = 2;
            Zoom = 0D;

            Manager.UseGeocoderCache = true;
            Manager.UsePlacemarkCache = true;
            Manager.UseUrlCache = true;

            if (!IsDesignerHosted) {
                SetStyle(ControlStyles.OptimizedDoubleBuffer, true);
                SetStyle(ControlStyles.AllPaintingInWmPaint, true);
                SetStyle(ControlStyles.UserPaint, true);
                SetStyle(ControlStyles.Opaque, true);
                ResizeRedraw = true;

                TileFlipXYAttributes.SetWrapMode(WrapMode.TileFlipXY);

                CenterFormat.Alignment = StringAlignment.Center;
                CenterFormat.LineAlignment = StringAlignment.Center;

                BottomFormat.Alignment = StringAlignment.Center;
                BottomFormat.LineAlignment = StringAlignment.Far;

                Overlays.CollectionChanged += Overlays_CollectionChanged;
            }
        }

#endif

        static GMapControl()
        {
            if (!IsDesignerHosted) {
                GMapImageProxy.Enable();

                GMaps.Instance.SQLitePing();
            }
        }

        private void Overlays_CollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (e.NewItems != null) {
                foreach (GMapOverlay obj in e.NewItems) {
                    if (obj != null) {
                        obj.Control = this;
                    }
                }

                if (Core.IsStarted && !HoldInvalidation) {
                    Invalidate();
                }
            }
        }

        /// <summary>
        /// call this to stop HoldInvalidation and perform single forced instant refresh 
        /// </summary>
        public override void Refresh()
        {
            HoldInvalidation = false;

            lock (Core.InvalidationLock) {
                Core.LastInvalidation = DateTime.Now;
            }

            base.Refresh();
        }

#if !DESIGN
        /// <summary>
        /// enqueue built-in thread safe invalidation
        /// </summary>
        public new void Invalidate()
        {
            if (Core.RefreshEvent != null) {
                Core.RefreshEvent.Set();
            }
        }
#endif

        private void invalidatorEngage(object sender, ProgressChangedEventArgs e)
        {
            base.Invalidate();
        }

        /// <summary>
        /// update objects when map is dragged/zoomed
        /// </summary>
        internal void ForceUpdateOverlays()
        {
            try {
                HoldInvalidation = true;

                foreach (GMapOverlay o in Overlays) {
                    if (o.IsVisible) {
                        o.ForceUpdate();
                    }
                }
            } finally {
                Refresh();
            }
        }

        /// <summary>
        /// updates markers local position
        /// </summary>
        /// <param name="marker"></param>
        public void UpdateMarkerLocalPosition(GMapMarker marker)
        {
            GPoint p = FromLatLngToLocal(marker.Position);
            p.OffsetNegative(Core.RenderOffset);
            marker.LocalPosition = new Point((int)(p.X + marker.Offset.X), (int)(p.Y + marker.Offset.Y));
        }

        /// <summary>
        /// updates routes local position
        /// </summary>
        /// <param name="route"></param>
        public void UpdateRouteLocalPosition(GMapRoute route)
        {
            route.LocalPoints.Clear();

            for (int i = 0; i < route.Points.Count; i++) {
                GPoint p = FromLatLngToLocal(route.Points[i]);
                p.OffsetNegative(Core.RenderOffset);
                route.LocalPoints.Add(p);
            }
            route.UpdateGraphicsPath();
        }

        /// <summary>
        /// updates polygons local position
        /// </summary>
        /// <param name="polygon"></param>
        public void UpdatePolygonLocalPosition(GMapPolygon polygon)
        {
            polygon.LocalPoints.Clear();

            for (int i = 0; i < polygon.Points.Count; i++) {
                GPoint p = FromLatLngToLocal(polygon.Points[i]);
                p.OffsetNegative(Core.RenderOffset);
                polygon.LocalPoints.Add(p);
            }
            polygon.UpdateGraphicsPath();
        }

        /// <summary>
        /// sets zoom to max to fit rect
        /// </summary>
        /// <param name="rect"></param>
        /// <returns></returns>
        public bool SetZoomToFitRect(RectLatLng rect)
        {
            if (fLazyEvents) {
                fLazySetZoomToFitRect = rect;
            } else {
                int maxZoom = Core.GetMaxZoomToFitRect(rect);
                if (maxZoom > 0) {
                    PointLatLng center = new PointLatLng(rect.Lat - (rect.HeightLat / 2), rect.Lng + (rect.WidthLng / 2));
                    Position = center;

                    if (maxZoom > MaxZoom) {
                        maxZoom = MaxZoom;
                    }

                    if ((int)Zoom != maxZoom) {
                        Zoom = maxZoom;
                    }

                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// sets to max zoom to fit all markers and centers them in map
        /// </summary>
        /// <param name="overlayId">overlay id or null to check all</param>
        /// <returns></returns>
        public bool ZoomAndCenterMarkers(string overlayId)
        {
            RectLatLng? rect = GetRectOfAllMarkers(overlayId);
            if (rect.HasValue) {
                return SetZoomToFitRect(rect.Value);
            }
            return false;
        }

        /// <summary>
        /// zooms and centers all route
        /// </summary>
        /// <param name="overlayId">overlay id or null to check all</param>
        /// <returns></returns>
        public bool ZoomAndCenterRoutes(string overlayId)
        {
            RectLatLng? rect = GetRectOfAllRoutes(overlayId);
            if (rect.HasValue) {
                return SetZoomToFitRect(rect.Value);
            }
            return false;
        }

        /// <summary>
        /// zooms and centers route 
        /// </summary>
        /// <param name="route"></param>
        /// <returns></returns>
        public bool ZoomAndCenterRoute(MapRoute route)
        {
            RectLatLng? rect = GetRectOfRoute(route);
            if (rect.HasValue) {
                return SetZoomToFitRect(rect.Value);
            }
            return false;
        }

        /// <summary>
        /// gets rectangle with all objects inside
        /// </summary>
        /// <param name="overlayId">overlay id or null to check all except zoomInsignificant</param>
        /// <returns></returns>
        public RectLatLng? GetRectOfAllMarkers(string overlayId)
        {
            RectLatLng? ret = null;

            double left = double.MaxValue;
            double top = double.MinValue;
            double right = double.MinValue;
            double bottom = double.MaxValue;

            foreach (GMapOverlay o in Overlays) {
                if (((overlayId == null && o.IsZoomSignificant) || o.Id == overlayId) && o.IsVisible && o.Markers.Count > 0) {
                    foreach (GMapMarker m in o.Markers) {
                        if (m.IsVisible) {
                            // left
                            if (m.Position.Lng < left) {
                                left = m.Position.Lng;
                            }

                            // top
                            if (m.Position.Lat > top) {
                                top = m.Position.Lat;
                            }

                            // right
                            if (m.Position.Lng > right) {
                                right = m.Position.Lng;
                            }

                            // bottom
                            if (m.Position.Lat < bottom) {
                                bottom = m.Position.Lat;
                            }
                        }
                    }
                }
            }

            if (left != double.MaxValue && right != double.MinValue && top != double.MinValue && bottom != double.MaxValue) {
                ret = RectLatLng.FromLTRB(left, top, right, bottom);
            }

            return ret;
        }

        /// <summary>
        /// gets rectangle with all objects inside
        /// </summary>
        /// <param name="overlayId">overlay id or null to check all except zoomInsignificant</param>
        /// <returns></returns>
        public RectLatLng? GetRectOfAllRoutes(string overlayId)
        {
            RectLatLng? ret = null;

            double left = double.MaxValue;
            double top = double.MinValue;
            double right = double.MinValue;
            double bottom = double.MaxValue;

            foreach (GMapOverlay o in Overlays) {
                if (((overlayId == null && o.IsZoomSignificant) || o.Id == overlayId) && o.IsVisible && o.Routes.Count > 0) {
                    foreach (GMapRoute route in o.Routes) {
                        if (route.IsVisible && route.From.HasValue && route.To.HasValue) {
                            foreach (PointLatLng p in route.Points) {
                                // left
                                if (p.Lng < left) {
                                    left = p.Lng;
                                }

                                // top
                                if (p.Lat > top) {
                                    top = p.Lat;
                                }

                                // right
                                if (p.Lng > right) {
                                    right = p.Lng;
                                }

                                // bottom
                                if (p.Lat < bottom) {
                                    bottom = p.Lat;
                                }
                            }
                        }
                    }
                }
            }

            if (left != double.MaxValue && right != double.MinValue && top != double.MinValue && bottom != double.MaxValue) {
                ret = RectLatLng.FromLTRB(left, top, right, bottom);
            }

            return ret;
        }

        /// <summary>
        /// gets rect of route
        /// </summary>
        /// <param name="route"></param>
        /// <returns></returns>
        public RectLatLng? GetRectOfRoute(MapRoute route)
        {
            RectLatLng? ret = null;

            double left = double.MaxValue;
            double top = double.MinValue;
            double right = double.MinValue;
            double bottom = double.MaxValue;

            if (route.From.HasValue && route.To.HasValue) {
                foreach (PointLatLng p in route.Points) {
                    // left
                    if (p.Lng < left) {
                        left = p.Lng;
                    }

                    // top
                    if (p.Lat > top) {
                        top = p.Lat;
                    }

                    // right
                    if (p.Lng > right) {
                        right = p.Lng;
                    }

                    // bottom
                    if (p.Lat < bottom) {
                        bottom = p.Lat;
                    }
                }
                ret = RectLatLng.FromLTRB(left, top, right, bottom);
            }
            return ret;
        }

        /// <summary>
        /// gets image of the current view
        /// </summary>
        /// <returns></returns>
        public Image ToImage()
        {
            Image ret;

            bool r = ForceDoubleBuffer;
            try {
                UpdateBackBuffer();

                if (!r) {
                    ForceDoubleBuffer = true;
                }

                Refresh();
                Application.DoEvents();

                using (MemoryStream ms = new MemoryStream()) {
                    using (var frame = (fBackBuffer.Clone() as Bitmap)) {
                        frame.Save(ms, ImageFormat.Png);
                    }
                    ret = Image.FromStream(ms);
                }
            } catch (Exception) {
                throw;
            } finally {
                if (!r) {
                    ForceDoubleBuffer = false;
                    ClearBackBuffer();
                }
            }
            return ret;
        }

        /// <summary>
        /// offset position in pixels
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        public void Offset(int x, int y)
        {
            if (IsHandleCreated) {
                Core.DragOffset(new GPoint(x, y));
                ForceUpdateOverlays();
            }
        }

#region UserControl Events

        public static readonly bool IsDesignerHosted = LicenseManager.UsageMode == LicenseUsageMode.Designtime;

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);

            if (!IsDesignerHosted) {
                if (fLazyEvents) {
                    fLazyEvents = false;

                    if (fLazySetZoomToFitRect.HasValue) {
                        SetZoomToFitRect(fLazySetZoomToFitRect.Value);
                        fLazySetZoomToFitRect = null;
                    }
                }
                Core.OnMapOpen().ProgressChanged += invalidatorEngage;
                ForceUpdateOverlays();
            }
        }

        protected override void OnCreateControl()
        {
            base.OnCreateControl();

            if (!IsDesignerHosted) {
                var f = ParentForm;
                if (f != null) {
                    while (f.ParentForm != null) {
                        f = f.ParentForm;
                    }

                    if (f != null) {
                        f.FormClosing += ParentForm_FormClosing;
                    }
                }
            }
        }

        private void ParentForm_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (e.CloseReason == CloseReason.WindowsShutDown || e.CloseReason == CloseReason.TaskManagerClosing) {
                Manager.CancelTileCaching();
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                Core.OnMapClose();

                Overlays.CollectionChanged -= Overlays_CollectionChanged;

                foreach (var o in Overlays) {
                    o.Dispose();
                }
                Overlays.Clear();

#if DEBUG_CENTER
                CenterPen.Dispose();
                ScalePen.Dispose();
#endif

                ScaleFont.Dispose();
                CenterFormat.Dispose();
                BottomFormat.Dispose();
                CopyrightFont.Dispose();
                EmptyTileBorders.Dispose();
                EmptyTileBrush.Dispose();
                ClearBackBuffer();
            }
            base.Dispose(disposing);
        }

#if !DESIGN
#if DEBUG
        private int fCounter;
        private readonly Font fDebugFont = new Font(FontFamily.GenericSansSerif, 12, FontStyle.Regular);
        private DateTime fStart;
        private DateTime fEnd;
        private int fDelta;
#endif

        protected override void OnPaint(PaintEventArgs e)
        {
#if DEBUG
            fStart = DateTime.Now;
#endif

            if (ForceDoubleBuffer) {
                if (fGxOff != null) {
                    DrawGraphics(fGxOff);
                    e.Graphics.DrawImage(fBackBuffer, 0, 0);
                }
            } else {
                DrawGraphics(e.Graphics);
            }

            base.OnPaint(e);

#if DEBUG
            fEnd = DateTime.Now;
            fDelta = (int)(fEnd - fStart).TotalMilliseconds;
#endif
        }

        private void DrawGraphics(Graphics g)
        {
            // render white background
            g.Clear(EmptyMapBackground);
            g.TranslateTransform(Core.RenderOffset.X, Core.RenderOffset.Y);
            DrawMap(g);
            OnPaintOverlays(g);
        }
#endif

        private void DrawMap(Graphics g)
        {
            if (Core.UpdatingBounds || Equals(MapProvider, EmptyProvider.Instance) || MapProvider == null) {
                Debug.WriteLine("Core.updatingBounds");
                return;
            }

            Core.TileDrawingListLock.AcquireReaderLock();
            Core.Matrix.EnterReadLock();

            try {
                foreach (var tilePoint in Core.TileDrawingList) {
                    Core.TileRect.Location = tilePoint.PosPixel;
                    Core.TileRect.OffsetNegative(Core.CompensationOffset);

                    bool found = false;

                    Tile t = Core.Matrix.GetTileWithNoLock(Core.Zoom, tilePoint.PosXY);
                    if (t.NotEmpty) {
                        // render tile
                        foreach (var pureImage in t.Overlays) {
                            var img = (GMapImage)pureImage;
                            if (img != null && img.Img != null) {
                                if (!found)
                                    found = true;

                                if (!img.IsParent) {
                                    g.DrawImage(img.Img, Core.TileRect.X, Core.TileRect.Y, Core.TileRect.Width, Core.TileRect.Height);
                                } else {
                                    float wix = (img.Img.Width / img.Ix);
                                    float hix = (img.Img.Height / img.Ix);
                                    RectangleF srcRect = new RectangleF(img.Xoff * wix, img.Yoff * hix, wix, hix);
                                    Rectangle dst = new Rectangle((int)Core.TileRect.X, (int)Core.TileRect.Y, (int)Core.TileRect.Width, (int)Core.TileRect.Height);
                                    g.DrawImage(img.Img, dst, srcRect.X, srcRect.Y, srcRect.Width, srcRect.Height, GraphicsUnit.Pixel, TileFlipXYAttributes);
                                }
                            }
                        }
                    } else if (MapProvider.Projection is MercatorProjection) {
                        // filling empty tiles using lower level images
                        int zoomOffset = 1;
                        Tile parentTile = Tile.Empty;
                        long Ix = 0;

                        while (!parentTile.NotEmpty && zoomOffset < Core.Zoom && zoomOffset <= Core.LevelsKeepInMemory) {
                            Ix = (long)Math.Pow(2, zoomOffset);
                            parentTile = Core.Matrix.GetTileWithNoLock(Core.Zoom - zoomOffset++,
                                new GPoint((int)(tilePoint.PosXY.X / Ix), (int)(tilePoint.PosXY.Y / Ix)));
                        }

                        if (parentTile.NotEmpty) {
                            long xoff = Math.Abs(tilePoint.PosXY.X - (parentTile.Pos.X * Ix));
                            long yoff = Math.Abs(tilePoint.PosXY.Y - (parentTile.Pos.Y * Ix));

                            // render tile 
                            foreach (var pureImage in parentTile.Overlays) {
                                var img = (GMapImage)pureImage;
                                if (img != null && img.Img != null && !img.IsParent) {
                                    if (!found)
                                        found = true;

                                    RectangleF srcRect = new RectangleF(xoff * (img.Img.Width / Ix),
                                        yoff * (img.Img.Height / Ix), (img.Img.Width / Ix), (img.Img.Height / Ix));
                                    Rectangle dst = new Rectangle((int)Core.TileRect.X, (int)Core.TileRect.Y,
                                        (int)Core.TileRect.Width, (int)Core.TileRect.Height);

                                    g.DrawImage(img.Img, dst, srcRect.X, srcRect.Y, srcRect.Width, srcRect.Height,
                                        GraphicsUnit.Pixel, TileFlipXYAttributes);
                                }
                            }
                        }
                    }

                    // add text if tile is missing
                    if (!found) {
                        lock (Core.FailedLoads) {
                            var lt = new LoadTask(tilePoint.PosXY, Core.Zoom);
                            if (Core.FailedLoads.ContainsKey(lt)) {
                                var ex = Core.FailedLoads[lt];
                                g.FillRectangle(EmptyTileBrush,
                                    new RectangleF(Core.TileRect.X, Core.TileRect.Y, Core.TileRect.Width,
                                        Core.TileRect.Height));

                                g.DrawString("Exception: " + ex.Message, MissingDataFont, Brushes.Red,
                                    new RectangleF(Core.TileRect.X + 11, Core.TileRect.Y + 11,
                                        Core.TileRect.Width - 11, Core.TileRect.Height - 11));

                                g.DrawString(EmptyTileText, MissingDataFont, Brushes.Blue,
                                    new RectangleF(Core.TileRect.X, Core.TileRect.Y, Core.TileRect.Width,
                                        Core.TileRect.Height), CenterFormat);

                                g.DrawRectangle(EmptyTileBorders, (int)Core.TileRect.X, (int)Core.TileRect.Y,
                                    (int)Core.TileRect.Width, (int)Core.TileRect.Height);
                            }
                        }
                    }

#if DEBUG_TILE_COORDS
                    // show tile grid lines
                    g.DrawRectangle(EmptyTileBorders, (int) Core.TileRect.X, (int) Core.TileRect.Y,
                        (int) Core.TileRect.Width, (int) Core.TileRect.Height);
                    g.DrawString(
                        (tilePoint.PosXY == Core.CenterTileXYLocation ? "CENTER: " : "TILE: ") + tilePoint,
                        MissingDataFont, Brushes.DimGray,
                        new RectangleF(Core.TileRect.X, Core.TileRect.Y, Core.TileRect.Width, Core.TileRect.Height),
                        CenterFormat);
#endif
                }
            } finally {
                Core.Matrix.LeaveReadLock();
                Core.TileDrawingListLock.ReleaseReaderLock();
            }
        }

        /// <summary>
        /// override, to render something more
        /// </summary>
        /// <param name="g"></param>
        protected virtual void OnPaintOverlays(Graphics g)
        {
            g.SmoothingMode = SmoothingMode.HighQuality;
            foreach (GMapOverlay o in Overlays) {
                if (o.IsVisible) {
                    o.OnRender(g);
                }
            }

            // center in virtual space...
#if DEBUG_CENTER
            g.DrawLine(ScalePen, -20, 0, 20, 0);
            g.DrawLine(ScalePen, 0, -20, 0, 20);
            g.DrawString("debug: virtual space center", CopyrightFont, Brushes.Blue, 2, CopyrightFont.Height);
#endif

            g.ResetTransform();

#region -- copyright --

            if (!string.IsNullOrEmpty(Core.Provider.Copyright)) {
                g.DrawString(Core.Provider.Copyright, CopyrightFont, Brushes.Navy, 3, Height - CopyrightFont.Height - 5);
            }

#endregion

#if DEBUG_RENDER
            // show center
            g.DrawLine(CenterPen, Width / 2 - 5, Height / 2, Width / 2 + 5, Height / 2);
            g.DrawLine(CenterPen, Width / 2, Height / 2 - 5, Width / 2, Height / 2 + 5);

            // debug info
            g.DrawString(string.Format(CultureInfo.InvariantCulture, "{0:0.0}", Zoom) + "z, " + MapProvider + ", refresh: " + fCounter++ + ", render: " + fDelta + "ms", fDebugFont, Brushes.Blue, fDebugFont.Height, fDebugFont.Height + 20);
#endif
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            base.OnSizeChanged(e);

            if (Width == 0 || Height == 0) {
                Debug.WriteLine("minimized");
                return;
            }

            if (Width == Core.Width && Height == Core.Height) {
                Debug.WriteLine("maximized");
                return;
            }

            if (!IsDesignerHosted) {
                if (ForceDoubleBuffer) {
                    UpdateBackBuffer();
                }

                Core.OnMapSizeChanged(Width, Height);

                if (Visible && IsHandleCreated && Core.IsStarted) {
                    ForceUpdateOverlays();
                }
            }
        }

        private void UpdateBackBuffer()
        {
            ClearBackBuffer();

            fBackBuffer = new Bitmap(Width, Height);
            fGxOff = Graphics.FromImage(fBackBuffer);
        }

        private void ClearBackBuffer()
        {
            if (fBackBuffer != null) {
                fBackBuffer.Dispose();
                fBackBuffer = null;
            }
            if (fGxOff != null) {
                fGxOff.Dispose();
                fGxOff = null;
            }
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            base.OnMouseDown(e);

            if (!IsMouseOverMarker) {
                if (e.Button == MouseButtons.Left) {
                    Core.MouseDown = new GPoint(e.X, e.Y);
                    Invalidate();
                }
            }
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            base.OnMouseUp(e);

            if (Core.IsDragging) {
                if (fIsDragging) {
                    fIsDragging = false;
                    Debug.WriteLine("IsDragging = " + fIsDragging);
                    Cursor = fCursorBefore;
                    fCursorBefore = null;
                }
                Core.EndDrag();
            } else {
                if (e.Button == MouseButtons.Left) {
                    Core.MouseDown = GPoint.Empty;
                }

                Invalidate();
            }
        }

        protected override void OnMouseClick(MouseEventArgs e)
        {
            base.OnMouseClick(e);

            if (!Core.IsDragging) {
                GPoint rp = new GPoint(e.X, e.Y);
                rp.OffsetNegative(Core.RenderOffset);

                for (int i = Overlays.Count - 1; i >= 0; i--) {
                    GMapOverlay o = Overlays[i];
                    if (o != null && o.IsVisible && o.IsHitTestVisible) {
                        foreach (GMapMarker m in o.Markers) {
                            if (m.IsVisible && m.IsHitTestVisible && m.IsInside((int)rp.X, (int)rp.Y)) {
                                if (OnMarkerClick != null) {
                                    OnMarkerClick(m, e);
                                }
                                break;
                            }
                        }

                        foreach (GMapRoute m in o.Routes) {
                            if (m.IsVisible && m.IsHitTestVisible && m.IsInside((int)rp.X, (int)rp.Y)) {
                                if (OnRouteClick != null) {
                                    OnRouteClick(m, e);
                                }
                                break;
                            }
                        }

                        foreach (GMapPolygon m in o.Polygons) {
                            if (m.IsVisible && m.IsHitTestVisible && m.IsInside(FromLocalToLatLng(e.X, e.Y))) {
                                if (OnPolygonClick != null) {
                                    OnPolygonClick(m, e);
                                }
                                break;
                            }
                        }
                    }
                }
            }
        }

        protected override void OnMouseDoubleClick(MouseEventArgs e)
        {
            base.OnMouseDoubleClick(e);

            if (!Core.IsDragging) {
                GPoint rp = new GPoint(e.X, e.Y);
                rp.OffsetNegative(Core.RenderOffset);

                for (int i = Overlays.Count - 1; i >= 0; i--) {
                    GMapOverlay o = Overlays[i];
                    if (o != null && o.IsVisible && o.IsHitTestVisible) {
                        foreach (GMapMarker m in o.Markers) {
                            if (m.IsVisible && m.IsHitTestVisible && m.IsInside((int)rp.X, (int)rp.Y)) {
                                if (OnMarkerDoubleClick != null) {
                                    OnMarkerDoubleClick(m, e);
                                }
                                break;
                            }
                        }

                        foreach (GMapRoute m in o.Routes) {
                            if (m.IsVisible && m.IsHitTestVisible && m.IsInside((int)rp.X, (int)rp.Y)) {
                                if (OnRouteDoubleClick != null) {
                                    OnRouteDoubleClick(m, e);
                                }
                                break;
                            }
                        }

                        foreach (GMapPolygon m in o.Polygons) {
                            if (m.IsVisible && m.IsHitTestVisible && m.IsInside(FromLocalToLatLng(e.X, e.Y))) {
                                if (OnPolygonDoubleClick != null) {
                                    OnPolygonDoubleClick(m, e);
                                }
                                break;
                            }
                        }
                    }
                }
            }
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            base.OnMouseMove(e);

            if (!Core.IsDragging && !Core.MouseDown.IsEmpty) {
                // Gets the width and height of a rectangle centered on the point the mouse
                // button was pressed, within which a drag operation will not begin.
                var dragSize = SystemInformation.DragSize;

                GPoint p = new GPoint(e.X, e.Y);
                if (Math.Abs(p.X - Core.MouseDown.X) * 2 >= dragSize.Width || Math.Abs(p.Y - Core.MouseDown.Y) * 2 >= dragSize.Height) {
                    Core.BeginDrag(Core.MouseDown);
                }
            }

            if (Core.IsDragging) {
                if (!fIsDragging) {
                    fIsDragging = true;
                    Debug.WriteLine("IsDragging = " + fIsDragging);

                    fCursorBefore = Cursor;
                    Cursor = Cursors.SizeAll;
                }

                Core.MouseCurrent = new GPoint(e.X, e.Y);
                Core.Drag(Core.MouseCurrent);
                base.Invalidate();
            } else {
                if (Core.MouseDown.IsEmpty) {
                    GPoint rp = new GPoint(e.X, e.Y);
                    rp.OffsetNegative(Core.RenderOffset);

                    for (int i = Overlays.Count - 1; i >= 0; i--) {
                        GMapOverlay o = Overlays[i];
                        if (o != null && o.IsVisible && o.IsHitTestVisible) {
                            foreach (GMapMarker m in o.Markers) {
                                if (m.IsVisible && m.IsHitTestVisible) {
                                    if (m.IsInside((int)rp.X, (int)rp.Y)) {
                                        if (!m.IsMouseOver) {
                                            SetCursorHandOnEnter();
                                            m.IsMouseOver = true;
                                            IsMouseOverMarker = true;

                                            if (OnMarkerEnter != null) {
                                                OnMarkerEnter(m);
                                            }

                                            Invalidate();
                                        }
                                    } else if (m.IsMouseOver) {
                                        m.IsMouseOver = false;
                                        IsMouseOverMarker = false;
                                        RestoreCursorOnLeave();
                                        if (OnMarkerLeave != null) {
                                            OnMarkerLeave(m);
                                        }

                                        Invalidate();
                                    }
                                }
                            }

                            foreach (GMapRoute m in o.Routes) {
                                if (m.IsVisible && m.IsHitTestVisible) {
                                    if (m.IsInside((int)rp.X, (int)rp.Y)) {
                                        if (!m.IsMouseOver) {
                                            SetCursorHandOnEnter();
                                            m.IsMouseOver = true;
                                            IsMouseOverRoute = true;

                                            if (OnRouteEnter != null) {
                                                OnRouteEnter(m);
                                            }

                                            Invalidate();
                                        }
                                    } else {
                                        if (m.IsMouseOver) {
                                            m.IsMouseOver = false;
                                            IsMouseOverRoute = false;
                                            RestoreCursorOnLeave();
                                            if (OnRouteLeave != null) {
                                                OnRouteLeave(m);
                                            }

                                            Invalidate();
                                        }
                                    }
                                }
                            }

                            foreach (GMapPolygon m in o.Polygons) {
                                if (m.IsVisible && m.IsHitTestVisible) {
                                    if (m.IsInsideLocal((int)rp.X, (int)rp.Y)) {
                                        if (!m.IsMouseOver) {
                                            SetCursorHandOnEnter();
                                            m.IsMouseOver = true;
                                            IsMouseOverPolygon = true;

                                            if (OnPolygonEnter != null) {
                                                OnPolygonEnter(m);
                                            }

                                            Invalidate();
                                        }
                                    } else {
                                        if (m.IsMouseOver) {
                                            m.IsMouseOver = false;
                                            IsMouseOverPolygon = false;
                                            RestoreCursorOnLeave();
                                            if (OnPolygonLeave != null) {
                                                OnPolygonLeave(m);
                                            }

                                            Invalidate();
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        internal void RestoreCursorOnLeave()
        {
            if (fOverObjectCount <= 0 && fCursorBefore != null) {
                fOverObjectCount = 0;
                Cursor = fCursorBefore;
                fCursorBefore = null;
            }
        }

        internal void SetCursorHandOnEnter()
        {
            if (fOverObjectCount <= 0 && Cursor != Cursors.Hand) {
                fOverObjectCount = 0;
                fCursorBefore = Cursor;
                Cursor = Cursors.Hand;
            }
        }

        protected override void OnMouseEnter(EventArgs e)
        {
            base.OnMouseEnter(e);

            Focus();
            fMouseIn = true;
        }

        protected override void OnMouseLeave(EventArgs e)
        {
            base.OnMouseLeave(e);
            fMouseIn = false;
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            base.OnMouseWheel(e);

            if (fMouseIn && (!IsMouseOverMarker || IgnoreMarkerOnMouseWheel) && !Core.IsDragging) {
                if (Core.MouseLastZoom.X != e.X && Core.MouseLastZoom.Y != e.Y) {
                    Core.Position = FromLocalToLatLng(e.X, e.Y);
                    Core.MouseLastZoom.X = e.X;
                    Core.MouseLastZoom.Y = e.Y;
                }

                // set mouse position to map center
                if (!GMaps.Instance.IsRunningOnMono) {
                    Point p = PointToScreen(new Point(Width / 2, Height / 2));
                    SetCursorPos(p.X, p.Y);
                }

                Core.MouseWheelZooming = true;

                if (e.Delta > 0) {
                    if (!InvertedMouseWheelZooming) {
                        Zoom = ((int)Zoom) + 1;
                    } else {
                        Zoom = ((int)(Zoom + 0.99)) - 1;
                    }
                } else if (e.Delta < 0) {
                    if (!InvertedMouseWheelZooming) {
                        Zoom = ((int)(Zoom + 0.99)) - 1;
                    } else {
                        Zoom = ((int)Zoom) + 1;
                    }
                }

                Core.MouseWheelZooming = false;
            }
        }

#endregion

        /// <summary>
        /// Call it to empty tile cache & reload tiles
        /// </summary>
        public void ReloadMap()
        {
            Core.ReloadMap();
        }

        /// <summary>
        /// set current position using keywords
        /// </summary>
        /// <param name="keys"></param>
        /// <returns>true if successful</returns>
        public GeocoderStatusCode SetPositionByKeywords(string keys)
        {
            var status = GeocoderStatusCode.Unknown;

            var gp = MapProvider as IGeocodingProvider;
            if (gp == null) {
                gp = GMapProviders.OpenStreetMap;
            }

            if (gp != null) {
                var pt = gp.GetPoint(keys, out status);
                if (status == GeocoderStatusCode.Success && pt.HasValue) {
                    Position = pt.Value;
                }
            }

            return status;
        }

        /// <summary>
        /// gets world coordinate from local control coordinate 
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        public PointLatLng FromLocalToLatLng(int x, int y)
        {
            return Core.FromLocalToLatLng(x, y);
        }

        /// <summary>
        /// gets local coordinate from world coordinate
        /// </summary>
        /// <param name="point"></param>
        /// <returns></returns>
        public GPoint FromLatLngToLocal(PointLatLng point)
        {
            return Core.FromLatLngToLocal(point);
        }

        public static bool PingNetwork(string hostNameOrAddress)
        {
            bool pingStatus;

            using (Ping p = new Ping()) {
                byte[] buffer = Encoding.ASCII.GetBytes("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
                int timeout = 4444; // 4s

                try {
                    PingReply reply = p.Send(hostNameOrAddress, timeout, buffer);
                    pingStatus = (reply != null && reply.Status == IPStatus.Success);
                } catch (Exception) {
                    pingStatus = false;
                }
            }

            return pingStatus;
        }

        [DllImport("user32.dll", EntryPoint = "SetCursorPos")]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool SetCursorPos(int x, int y);
    }
}
