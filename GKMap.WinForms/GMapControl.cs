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
using System.IO;
using System.Runtime.InteropServices;
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
        private bool fForceDoubleBuffer;
        private Graphics fGxOff;
        private bool fIsDragging;
        private bool fIsMouseOverMarker;
        private bool fIsMouseOverPolygon;
        private bool fIsMouseOverRoute;
        private bool fLazyEvents = true;
        private RectLatLng? fLazySetZoomToFitRect;
        private int fOverObjectCount;

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
                return GMaps.CacheLocation;
#else
                return string.Empty;
#endif
            }
            set {
#if !DESIGN
                GMaps.CacheLocation = value;
#endif
            }
        }

        /// <summary>
        /// reverses MouseWheel zooming direction
        /// </summary>
        public bool InvertedMouseWheelZooming
        {
            get {
                return Core.InvertedMouseWheelZooming;
            }
            set {
                Core.InvertedMouseWheelZooming = value;
            }
        }

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
        public int Zoom
        {
            get {
                return Core.Zoom;
            }
            set {
                if (Core.Zoom != value) {
                    Debug.WriteLine("ZoomPropertyChanged: " + Core.Zoom + " -> " + value);

                    if (value > MaxZoom) {
                        Core.Zoom = MaxZoom;
                    } else if (value < MinZoom) {
                        Core.Zoom = MinZoom;
                    } else {
                        Core.Zoom = value;
                    }

                    if (Core.IsStarted && !fIsDragging) {
                        ForceUpdateOverlays();
                    }
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
            set {
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
            set {
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
            set {
                fIsMouseOverPolygon = value;
                fOverObjectCount += value ? 1 : -1;
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

                    RectLatLng viewarea = Core.ViewArea;

                    Core.Provider = value;

                    if (Core.IsStarted) {
                        if (Core.ZoomToArea) {
                            // restore zoom rect as close as possible
                            if (viewarea != RectLatLng.Empty && viewarea != Core.ViewArea) {
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
        public ObservableCollection<IMapOverlay> Overlays { get; private set; }

        /// <summary>
        /// stops immediate marker/route/polygon invalidation;
        /// call Refresh to perform single refresh and reset invalidation state
        /// </summary>
        public bool HoldInvalidation { get; set; }

        internal Core Core { get; private set; }

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
        /// occurs when clicked on marker
        /// </summary>
        public event MarkerClick OnMarkerClick;

        /// <summary>
        /// occurs when double clicked on marker
        /// </summary>
        public event MarkerDoubleClick OnMarkerDoubleClick;

        /// <summary>
        /// occurs on mouse enters marker area
        /// </summary>
        public event MarkerEnter OnMarkerEnter
        {
            add {
                Core.OnMarkerEnter += value;
            }
            remove {
                Core.OnMarkerEnter -= value;
            }
        }

        /// <summary>
        /// occurs on mouse leaves marker area
        /// </summary>
        public event MarkerLeave OnMarkerLeave
        {
            add {
                Core.OnMarkerLeave += value;
            }
            remove {
                Core.OnMarkerLeave -= value;
            }
        }

        /// <summary>
        /// occurs when clicked on polygon
        /// </summary>
        public event PolygonClick OnPolygonClick;

        /// <summary>
        /// occurs when double clicked on polygon
        /// </summary>
        public event PolygonDoubleClick OnPolygonDoubleClick;

        /// <summary>
        /// occurs on mouse enters Polygon area
        /// </summary>
        public event PolygonEnter OnPolygonEnter
        {
            add {
                Core.OnPolygonEnter += value;
            }
            remove {
                Core.OnPolygonEnter -= value;
            }
        }

        /// <summary>
        /// occurs on mouse leaves Polygon area
        /// </summary>
        public event PolygonLeave OnPolygonLeave
        {
            add {
                Core.OnPolygonLeave += value;
            }
            remove {
                Core.OnPolygonLeave -= value;
            }
        }

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
        public event RouteEnter OnRouteEnter
        {
            add {
                Core.OnRouteEnter += value;
            }
            remove {
                Core.OnRouteEnter -= value;
            }
        }

        /// <summary>
        /// occurs on mouse leaves route area
        /// </summary>
        public event RouteLeave OnRouteLeave
        {
            add {
                Core.OnRouteLeave += value;
            }
            remove {
                Core.OnRouteLeave -= value;
            }
        }

        #endregion


#if !DESIGN
        /// <summary>
        /// constructor
        /// </summary>
        public GMapControl()
        {
            Core = new Core(this);

            Overlays = new ObservableCollectionThreadSafe<IMapOverlay>();

            InvertedMouseWheelZooming = false;
            MaxZoom = 17;
            MinZoom = 2;
            Zoom = 0;

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
                GMaps.Initialize(GMapImageProxy.Instance);
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

                if (Core.IsStarted) {
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
            if (Core.RefreshEvent != null && !HoldInvalidation) {
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
        public void UpdateMarkerLocalPosition(IMapMarker marker)
        {
            Core.UpdateMarkerLocalPosition(marker);
        }

        /// <summary>
        /// updates routes local position
        /// </summary>
        /// <param name="route"></param>
        public void UpdateRouteLocalPosition(IMapRoute route)
        {
            Core.UpdateRouteLocalPosition(route);
        }

        /// <summary>
        /// updates polygons local position
        /// </summary>
        /// <param name="polygon"></param>
        public void UpdatePolygonLocalPosition(IMapPolygon polygon)
        {
            Core.UpdatePolygonLocalPosition(polygon);
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

                    if (Zoom != maxZoom) {
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
            RectLatLng? rect = Core.GetRectOfAllMarkers(overlayId);
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
            RectLatLng? rect = Core.GetRectOfAllRoutes(overlayId);
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
        public bool ZoomAndCenterRoute(GMapFigure figure)
        {
            RectLatLng? rect = Core.GetRectOfRoute(figure);
            if (rect.HasValue) {
                return SetZoomToFitRect(rect.Value);
            }
            return false;
        }

        /// <summary>
        /// gets image of the current view
        /// </summary>
        /// <returns></returns>
        public Image ToImage()
        {
            Image ret;

            bool r = fForceDoubleBuffer;
            try {
                UpdateBackBuffer();

                if (!r) {
                    fForceDoubleBuffer = true;
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
                    fForceDoubleBuffer = false;
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

                    f.FormClosing += ParentForm_FormClosing;
                }
            }
        }

        private void ParentForm_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (e.CloseReason == CloseReason.WindowsShutDown || e.CloseReason == CloseReason.TaskManagerClosing) {
                GMaps.Instance.CancelTileCaching();
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

            if (fForceDoubleBuffer) {
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
                                    float wix = ((float)img.Img.Width / img.Ix);
                                    float hix = ((float)img.Img.Height / img.Ix);
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
                if (fForceDoubleBuffer) {
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
            Core.ProcessMouseClick(e.X, e.Y, e);
        }

        protected override void OnMouseDoubleClick(MouseEventArgs e)
        {
            base.OnMouseDoubleClick(e);
            Core.ProcessMouseDoubleClick(e.X, e.Y, e);
        }

        void IMapControl.DoMouseClick(IMapObject obj, EventArgs e)
        {
            if (obj is IMapMarker) {
                if (OnMarkerClick != null) {
                    OnMarkerClick(obj as IMapMarker, (MouseEventArgs)e);
                }
            } else if (obj is IMapRoute) {
                if (OnRouteClick != null) {
                    OnRouteClick(obj as IMapRoute, (MouseEventArgs)e);
                }
            } else if (obj is IMapPolygon) {
                if (OnPolygonClick != null) {
                    OnPolygonClick(obj as IMapPolygon, (MouseEventArgs)e);
                }
            }
        }

        void IMapControl.DoMouseDoubleClick(IMapObject obj, EventArgs e)
        {
            if (obj is IMapMarker) {
                if (OnMarkerDoubleClick != null) {
                    OnMarkerDoubleClick(obj as IMapMarker, (MouseEventArgs)e);
                }
            } else if (obj is IMapRoute) {
                if (OnRouteDoubleClick != null) {
                    OnRouteDoubleClick(obj as IMapRoute, (MouseEventArgs)e);
                }
            } else if (obj is IMapPolygon) {
                if (OnPolygonDoubleClick != null) {
                    OnPolygonDoubleClick(obj as IMapPolygon, (MouseEventArgs)e);
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
                    Core.ProcessOverlaysMouseMove(rp);
                }
            }
        }

        void IMapControl.RestoreCursorOnLeave()
        {
            if (fOverObjectCount <= 0 && fCursorBefore != null) {
                fOverObjectCount = 0;
                Cursor = fCursorBefore;
                fCursorBefore = null;
            }
        }

        void IMapControl.SetCursorHandOnEnter()
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
            Core.MouseIn = true;
        }

        protected override void OnMouseLeave(EventArgs e)
        {
            base.OnMouseLeave(e);
            Core.MouseIn = false;
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            base.OnMouseWheel(e);
            Core.ProcessMouseWheel(e.X, e.Y, e.Delta);
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
            return Core.SetPositionByKeywords(keys);
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

        public void SetMousePositionToMapCenter()
        {
            if (!Core.IsRunningOnMono) {
                Point p = PointToScreen(new Point(Width / 2, Height / 2));
                SetCursorPos(p.X, p.Y);
            }
        }

        [DllImport("user32.dll", EntryPoint = "SetCursorPos")]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool SetCursorPos(int x, int y);
    }
}
